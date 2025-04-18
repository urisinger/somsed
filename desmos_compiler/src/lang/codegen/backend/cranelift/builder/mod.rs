use anyhow::{anyhow, bail, Context};
use cranelift::prelude::*;
use cranelift_module::{FuncOrDataId, Module};

use crate::lang::{
    codegen::ir::{BlockID, IRModule, IRSegment, IRType, InstID, Instruction, SegmentKey},
    expr::{BinaryOp, UnaryOp},
};

use super::{
    value::{value_count, CraneliftScaler, CraneliftValue},
    CraneliftBackend,
};

//mod list;

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
            let count = value_count(ty.clone()); // number of Values this type consumes
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

    fn build_return(mut self, value: CraneliftValue) {
        self.builder.ins().return_(value.as_struct());
    }

    fn get_arg(&mut self, index: usize) -> Option<&CraneliftValue> {
        self.args.get(index)
    }

    fn call_fn(&mut self, name: &str, values: &[CraneliftValue]) -> Option<CraneliftValue> {
        let args: Vec<_> = values
            .iter()
            .flat_map(|arg| arg.as_struct())
            .cloned()
            .collect();

        let types: Vec<_> = values.iter().map(|v| v.ty()).collect();

        let ret = self
            .ir_module
            .get_segment(&SegmentKey {
                args: types,
                name: name.to_string(),
            })
            .and_then(|segment| segment.ret())?
            .ty();

        let func_id = if let FuncOrDataId::Func(func) = self.backend.module.get_name(name)? {
            func
        } else {
            return None;
        };

        let func_ref = self
            .backend
            .module
            .declare_func_in_func(func_id, self.builder.func);

        let ins = self.builder.ins().call(func_ref, &args);
        let values = self.builder.inst_results(ins);

        CraneliftValue::from_values(values, ret)
    }

    pub fn build_fn(mut self, segment: &IRSegment) -> anyhow::Result<()> {
        let entry = segment
            .entry_block()
            .with_context(|| anyhow!("entry for segment not found"))?;

        let mut values = vec![vec![]; segment.blocks().len()];

        let get_value = |values: &Vec<Vec<_>>, id: InstID| values[id.block().0][id.inst()];

        let push_value =
            |values: &mut Vec<Vec<_>>, block_id: BlockID, value| values[block_id.0].push(value);

        let current_block = segment.entry_block.expect("entry not found");

        for (_, instr) in entry.iter() {
            let value = match instr {
                Instruction::Const(number) => {
                    CraneliftValue::number(self.builder.ins().f64const(*number))
                }
                Instruction::Point(x, y) => CraneliftValue::point([
                    match_value!(get_value(&values, *x), Scaler::Number)?,
                    match_value!(get_value(&values, *y), Scaler::Number)?,
                ]),
                Instruction::Extract(value, index) => CraneliftValue::number(
                    match_value!(get_value(&values, *value), Scaler::Point)?[*index],
                ),
                Instruction::BinaryOp { lhs, op, rhs } => {
                    let lhs = match_value!(get_value(&values, *lhs), Scaler::Number)?;
                    let rhs = match_value!(get_value(&values, *rhs), Scaler::Number)?;
                    CraneliftValue::number(match op {
                        BinaryOp::Add => self.builder.ins().fadd(lhs, rhs),
                        BinaryOp::Sub => self.builder.ins().fsub(lhs, rhs),
                        BinaryOp::Dot => self.builder.ins().fmul(lhs, rhs),
                        BinaryOp::Paran => self.builder.ins().fmul(lhs, rhs),
                        BinaryOp::Div => self.builder.ins().fdiv(lhs, rhs),
                        BinaryOp::Pow => self.pow(lhs, rhs),
                    })
                }
                Instruction::UnaryOp { op, val } => {
                    let val = match_value!(get_value(&values, *val), Scaler::Number)?;
                    CraneliftValue::number(match op {
                        UnaryOp::Neg => self.builder.ins().fneg(val),
                        UnaryOp::Sqrt => self.builder.ins().sqrt(val),
                        UnaryOp::Sin => self.sin(val),
                        UnaryOp::Cos => self.cos(val),
                        UnaryOp::Tan => self.tan(val),
                        _ => todo!(),
                    })
                }
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

                    let func_id = if let FuncOrDataId::Func(func) = self
                        .backend
                        .module
                        .get_name(&segment_key.to_string())
                        .with_context(|| anyhow!("function not found in module"))?
                    {
                        func
                    } else {
                        bail!("function is of data type, this indicates a bug")
                    };

                    let func_ref = self
                        .backend
                        .module
                        .declare_func_in_func(func_id, self.builder.func);

                    let mut cranelift_args: Vec<_> = Vec::new();

                    for inst_id in args.iter() {
                        for value in get_value(&values, *inst_id).as_struct() {
                            cranelift_args.push(*value);
                        }
                    }

                    let ins = self.builder.ins().call(func_ref, &cranelift_args);
                    let values = self.builder.inst_results(ins);

                    CraneliftValue::from_values(values, ret)
                        .with_context(|| anyhow!("function returns wrong type"))?
                }
                Instruction::FnArg { index } => self.args[*index],
                Instruction::BlockArg { index } => todo!(),
                Instruction::NumberList(inst_ids) => todo!(),
                Instruction::PointList(inst_ids) => todo!(),
                Instruction::Map {
                    lists,
                    args,
                    block_id,
                } => todo!(),
            };
            push_value(&mut values, current_block, value)
        }

        self.builder
            .ins()
            .return_(get_value(&values, segment.ret().expect("expected return value")).as_struct());

        Ok(())
    }

    /*fn number_list(&mut self, elements: &[Self::NumberValue]) -> Result<Self::NumberListValue> {
        self.build_new_list(elements, GenericValue::Number(()))
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

    fn neg(&mut self, lhs: Value) -> Value {
        self.builder.ins().fneg(lhs)
    }

    fn sqrt(&mut self, lhs: Value) -> Value {
        self.builder.ins().sqrt(lhs)
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
