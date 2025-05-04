use anyhow::{anyhow, bail, Context};
use cranelift::prelude::*;
use cranelift_module::{FuncOrDataId, Module};

use desmos_compiler::lang::codegen::ir::{
    IRModule, IRScalerType, IRSegment, IRType, InstID, Instruction, SegmentKey,
};

use crate::value::CraneliftList;

use super::{
    value::{value_count, CraneliftScaler, CraneliftValue},
    CraneliftBackend,
};

mod list;

macro_rules! match_value {
    ($value:expr, Scaler::Number) => {
        match $value {
            CraneliftValue::Scaler(CraneliftScaler::Number(v)) => Ok(v[0]),
            other => Err(anyhow::anyhow!(
                "expected Scaler::Number, found {:?}",
                other
            )),
        }
    };
    ($value:expr, Scaler::Point) => {
        match $value {
            CraneliftValue::Scaler(CraneliftScaler::Point(v)) => Ok(v),
            other => Err(anyhow::anyhow!("expected Scaler::Point, found {:?}", other)),
        }
    };
    ($value:expr, List::Number) => {
        match $value {
            CraneliftValue::List(CraneliftList::Number(v)) => Ok(v),
            other => Err(anyhow::anyhow!("expected List::Number, found {:?}", other)),
        }
    };
    ($value:expr, List::Point) => {
        match $value {
            CraneliftValue::List(CraneliftList::Point(v)) => Ok(v),
            other => Err(anyhow::anyhow!("expected List::Point, found {:?}", other)),
        }
    };
}

pub struct CraneliftBuilder<'a, 'ctx> {
    backend: &'ctx mut CraneliftBackend,
    ir_module: &'ctx IRModule,
    builder: FunctionBuilder<'a>,

    // Vector to the start of each variable, the number of values is decided by the type
    args: Vec<CraneliftValue>,
}

impl<'a, 'ctx> CraneliftBuilder<'a, 'ctx> {
    pub fn new(
        backend: &'ctx mut CraneliftBackend,
        ir_module: &'ctx IRModule,
        mut builder: FunctionBuilder<'a>,
        types: &[IRType],
    ) -> Self {
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let arg_values = builder.block_params(entry_block);

        let mut args = Vec::new();

        let mut i = 0;
        for ty in types {
            let count = value_count(*ty); // number of Values this type consumes
            let slice = &arg_values[i..i + count];
            let value = CraneliftValue::from_values(slice, *ty).unwrap(); // or handle None
            args.push(value);
            i += count;
        }

        CraneliftBuilder {
            backend,
            ir_module,
            builder,
            args,
        }
    }

    pub fn build_fn(mut self, segment: &IRSegment) -> anyhow::Result<()> {
        let entry = segment
            .entry_block()
            .with_context(|| anyhow!("entry for segment not found"))?;

        let value = self.build_block(segment, entry.insts(), &[])?;

        self.builder.ins().return_(value.as_struct());

        self.builder.finalize();

        Ok(())
    }

    fn build_block(
        &mut self,
        segment: &IRSegment,
        instructions: &[Instruction],
        block_args: &[CraneliftValue],
    ) -> anyhow::Result<CraneliftValue> {
        let mut values = Vec::with_capacity(instructions.len());

        for instr in instructions {
            let get_number = |id: &InstID| match_value!(values[id.inst()], Scaler::Number);
            let value = match instr {
                Instruction::Number(number) => {
                    CraneliftValue::number(self.builder.ins().f64const(*number))
                }

                Instruction::Point(x, y) => CraneliftValue::point([
                    match_value!(values[x.inst()], Scaler::Number)?,
                    match_value!(values[y.inst()], Scaler::Number)?,
                ]),

                Instruction::Extract(val, index) => {
                    CraneliftValue::number(match_value!(values[val.inst()], Scaler::Point)?[*index])
                }

                Instruction::Index(list_id, index_id) => {
                    let list_val = &values[list_id.inst()];
                    let (element_type, element_size, len_val, base_ptr) = match list_val {
                        CraneliftValue::List(list) => {
                            let (element_type, element_size) = list.element_type_and_size();
                            let [len, base] = list.values();
                            (element_type, element_size, len, base)
                        }
                        t => bail!("Expected list value for indexing found {t:?}"),
                    };

                    // Get index
                    let index_f64 = match &values[index_id.inst()] {
                        CraneliftValue::Scaler(CraneliftScaler::Number([val])) => *val,
                        _ => bail!("Expected numeric index"),
                    };

                    let index = self.builder.ins().fcvt_to_sint_sat(types::I64, index_f64);
                    let zero = self.builder.ins().iconst(types::I64, 0);

                    let too_small = self.builder.ins().icmp(IntCC::SignedLessThan, index, zero);
                    let too_big =
                        self.builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, index, len_val);
                    let out_of_bounds = self.builder.ins().bor(too_small, too_big);

                    // Prepare blocks
                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();

                    // Branch based on bounds check
                    self.builder
                        .ins()
                        .brif(out_of_bounds, then_block, &[], else_block, &[]);

                    // then: return NaN
                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    let nan = self.builder.ins().f64const(f64::NAN);
                    match element_type {
                        IRScalerType::Number => {
                            _ = self.builder.append_block_param(merge_block, types::F64);
                            self.builder.ins().jump(merge_block, &[nan]);
                        }
                        IRScalerType::Point => {
                            let nan2 = self.builder.ins().f64const(f64::NAN);
                            _ = self.builder.append_block_param(merge_block, types::F64);
                            _ = self.builder.append_block_param(merge_block, types::F64);
                            self.builder.ins().jump(merge_block, &[nan, nan2]);
                        }
                    }

                    // else: do the actual indexing
                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    let offset = self.builder.ins().imul_imm(index, element_size as i64);
                    let addr = self.builder.ins().iadd(base_ptr, offset);

                    match element_type {
                        IRScalerType::Number => {
                            let val = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 0);
                            self.builder.ins().jump(merge_block, &[val]);
                        }
                        IRScalerType::Point => {
                            let x = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 0);
                            let y = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 8);
                            self.builder.ins().jump(merge_block, &[x, y]);
                        }
                    }

                    // merge
                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);

                    match element_type {
                        IRScalerType::Number => {
                            let result = self.builder.block_params(merge_block)[0];
                            CraneliftValue::number(result)
                        }
                        IRScalerType::Point => {
                            let params = self.builder.block_params(merge_block);
                            CraneliftValue::point([params[0], params[1]])
                        }
                    }
                }

                Instruction::Add(lhs, rhs) => CraneliftValue::number(
                    self.builder.ins().fadd(get_number(lhs)?, get_number(rhs)?),
                ),
                Instruction::Sub(lhs, rhs) => CraneliftValue::number(
                    self.builder.ins().fsub(get_number(lhs)?, get_number(rhs)?),
                ),
                Instruction::Mul(lhs, rhs) => CraneliftValue::number(
                    self.builder.ins().fmul(get_number(lhs)?, get_number(rhs)?),
                ),
                Instruction::Div(lhs, rhs) => CraneliftValue::number(
                    self.builder.ins().fdiv(get_number(lhs)?, get_number(rhs)?),
                ),
                Instruction::Pow(lhs, rhs) => {
                    CraneliftValue::number(self.pow(get_number(lhs)?, get_number(rhs)?))
                }

                Instruction::Neg(val) => {
                    CraneliftValue::number(self.builder.ins().fneg(get_number(val)?))
                }
                Instruction::Sqrt(val) => {
                    CraneliftValue::number(self.builder.ins().sqrt(get_number(val)?))
                }
                Instruction::Sin(val) => CraneliftValue::number(self.sin(get_number(val)?)),
                Instruction::Cos(val) => CraneliftValue::number(self.cos(get_number(val)?)),
                Instruction::Tan(val) => CraneliftValue::number(self.tan(get_number(val)?)),

                Instruction::Call { func, args } => {
                    let segment_key =
                        SegmentKey::new(func, args.iter().map(|arg| arg.ty()).collect());

                    let ret = self
                        .ir_module
                        .get_segment(&segment_key)
                        .with_context(|| anyhow!("function not found in module"))?
                        .ret()
                        .unwrap()
                        .ty();

                    let func_id = match self
                        .backend
                        .module
                        .get_name(&segment_key.to_string())
                        .with_context(|| anyhow!("function not found in module"))?
                    {
                        FuncOrDataId::Func(id) => id,
                        _ => bail!("function name was a data symbol — bug"),
                    };

                    let func_ref = self
                        .backend
                        .module
                        .declare_func_in_func(func_id, self.builder.func);

                    let cranelift_args = args
                        .iter()
                        .flat_map(|arg| values[arg.inst()].as_struct().to_vec())
                        .collect::<Vec<_>>();

                    let call = self.builder.ins().call(func_ref, &cranelift_args);
                    let results = self.builder.inst_results(call);

                    CraneliftValue::from_values(results, ret)
                        .with_context(|| anyhow!("function returns wrong type"))?
                }

                Instruction::FnArg { index } => self.args[*index],

                Instruction::BlockArg { index } => *block_args.get(*index).unwrap(),

                Instruction::NumberList(insts) => {
                    CraneliftValue::List(CraneliftList::Number(self.build_new_list(
                        &insts.iter().map(|i| values[i.inst()]).collect::<Vec<_>>(),
                        IRType::NUMBER,
                    )?))
                }

                Instruction::PointList(insts) => {
                    CraneliftValue::List(CraneliftList::Point(self.build_new_list(
                        &insts.iter().map(|i| values[i.inst()]).collect::<Vec<_>>(),
                        IRType::POINT,
                    )?))
                }

                Instruction::Map {
                    args,
                    lists,
                    block_id,
                } => {
                    let inner_block = segment
                        .blocks()
                        .get(block_id.0)
                        .with_context(|| anyhow!("block does not exist"))?;
                    CraneliftValue::List(
                        self.codegen_list_map(
                            &lists
                                .iter()
                                .map(|lists| {
                                    lists
                                        .iter()
                                        .map(|inst| {
                                            Ok(match values[inst.inst()] {
                                                CraneliftValue::List(list) => list,
                                                _ => bail!("expected list"),
                                            })
                                        })
                                        .collect()
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                            if let IRType::Scaler(t) = inner_block.ret() {
                                t
                            } else {
                                bail!("expected block to return scaler")
                            },
                            |codegen, list_args| {
                                Ok(
                                    match codegen.build_block(
                                        segment,
                                        inner_block.insts(),
                                        &list_args
                                            .iter()
                                            .cloned()
                                            .map(CraneliftValue::Scaler)
                                            .chain(args.iter().map(|inst| values[inst.inst()]))
                                            .collect::<Vec<_>>(),
                                    )? {
                                        CraneliftValue::Scaler(scaler) => scaler,
                                        CraneliftValue::List(_) => {
                                            bail!("expected scaler, not list")
                                        }
                                    },
                                )
                            },
                        )?,
                    )
                }
            };

            values.push(value);
        }

        values
            .last()
            .cloned()
            .ok_or_else(|| anyhow!("block did not produce any value"))
    }

    /*fn number_list(&mut self, elements: &[Self::NumberValue]) -> Result<Self::NumberListValue> {

    }

    fn point_list(&mut self, elements: &[Self::PointValue]) -> Result<Self::PointListValue> {
        self.build_new_list(elements, GenericValue::Point(()))
    }

    fn map_list(
        &mut self,
        list: GenericList<Self::NumberListValue, Self::PointListValue>,
        output_ty: ListType,
        f: impl Fn(
            &mut Self,
            GenericList<Self::NumberValue, Self::PointValue>,
        ) -> GenericList<Self::NumberValue, Self::PointValue>,
    ) -> GenericList<Self::NumberListValue, Self::PointListValue> {
        self.codegen_list_map(&list, output_ty, f)
            .expect("Something went wrong mapping list, this should not happen")
    }*/

    fn pow(&mut self, lhs: Value, rhs: Value) -> Value {
        let func = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.pow_id, self.builder.func);

        let inst = self.builder.ins().call(func, &[lhs, rhs]);

        self.builder.inst_results(inst)[0]
    }

    fn sin(&mut self, lhs: Value) -> Value {
        let func = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.sin_id, self.builder.func);

        let inst = self.builder.ins().call(func, &[lhs]);

        self.builder.inst_results(inst)[0]
    }

    fn cos(&mut self, lhs: Value) -> Value {
        let func = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.cos_id, self.builder.func);

        let inst = self.builder.ins().call(func, &[lhs]);

        self.builder.inst_results(inst)[0]
    }

    fn tan(&mut self, lhs: Value) -> Value {
        let func = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.tan_id, self.builder.func);

        let inst = self.builder.ins().call(func, &[lhs]);

        self.builder.inst_results(inst)[0]
    }
}
