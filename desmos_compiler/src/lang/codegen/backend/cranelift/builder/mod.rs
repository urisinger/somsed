use anyhow::Result;
use cranelift::{codegen::ir::Function, prelude::*};
use cranelift_module::{FuncOrDataId, Module};

use crate::lang::{
    codegen::{backend::CodeBuilder, CodeGen},
    expr::Expr,
    generic_value::{GenericList, GenericValue, ListType, ValueType},
};

use super::{
    value::{value_count, CraneliftValue},
    CraneliftBackend,
};

mod list;

pub struct CraneliftBuilder<'a, 'ctx> {
    backend: &'ctx mut CraneliftBackend,
    builder: FunctionBuilder<'a>,

    // Vector to the start of each variable, the number of values is decided by the type
    args: Vec<CraneliftValue>,
}

impl<'a, 'ctx> CraneliftBuilder<'a, 'ctx> {
    pub fn new(
        backend: &'ctx mut CraneliftBackend,
        mut builder: FunctionBuilder<'a>,
        types: &[ValueType],
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
            let value = CraneliftValue::from_values(slice, ty).unwrap(); // or handle None
            args.push(value);
            i += count;
        }

        CraneliftBuilder {
            backend,
            builder,
            args,
        }
    }
}

impl<'a, 'ctx> CodeBuilder for CraneliftBuilder<'a, 'ctx> {
    type NumberValue = [Value; 1];
    type PointValue = [Value; 2];

    type NumberListValue = [Value; 2];
    type PointListValue = [Value; 2];

    fn build_return(mut self, value: CraneliftValue) {
        self.builder.ins().return_(value.as_struct());
    }

    fn get_arg(&mut self, index: usize) -> Option<&CraneliftValue> {
        self.args.get(index)
    }

    fn call_fn<'b>(
        &mut self,
        name: &str,
        values: &[CraneliftValue],
        codegen: &mut CodeGen<'b>,
    ) -> Option<CraneliftValue> {
        let args: Vec<_> = values
            .iter()
            .flat_map(|arg| arg.as_struct())
            .cloned()
            .collect();

        let types: Vec<_> = values.iter().map(|v| v.get_type()).collect();

        let node = match codegen.exprs.get_expr(name)? {
            Expr::FnDef { rhs, .. } => rhs,
            _ => return None,
        };

        let ret = node.return_type(codegen.exprs, &types).ok()?;

        let func_id = match self.backend.module.get_name(name) {
            Some(FuncOrDataId::Func(id)) => id,
            None => {
                let mut ctx = FunctionBuilderContext::new();
                let mut function = Function::new();

                let (mut builder, function) =
                    self.backend
                        .get_builder(name, &types, &ret, &mut function, &mut ctx);

                let value = codegen.codegen_expr(&mut builder, node).ok()?;

                builder.build_return(value);
                function
            }
            _ => return None,
        };

        let func_ref = self
            .backend
            .module
            .declare_func_in_func(func_id, self.builder.func);

        let ins = self.builder.ins().call(func_ref, &args);
        let values = self.builder.inst_results(ins);

        CraneliftValue::from_values(values, &ret)
    }

    fn const_number(&mut self, number: f64) -> Self::NumberValue {
        [self.builder.ins().f64const(number)]
    }

    fn point(&mut self, x: Self::NumberValue, y: Self::NumberValue) -> Self::PointValue {
        [x[0], y[0]]
    }

    fn number_list(&mut self, elements: &[Self::NumberValue]) -> Result<Self::NumberListValue> {
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
    }

    fn get_x(&mut self, point: Self::PointValue) -> Self::NumberValue {
        [point[0]]
    }

    fn get_y(&mut self, point: Self::PointValue) -> Self::NumberValue {
        [point[1]]
    }

    fn add(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().fadd(lhs[0], rhs[0])]
    }

    fn sub(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().fsub(lhs[0], rhs[0])]
    }

    fn mul(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().fmul(lhs[0], rhs[0])]
    }

    fn div(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().fdiv(lhs[0], rhs[0])]
    }

    fn pow(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn neg(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().fneg(lhs[0])]
    }

    fn sqrt(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        [self.builder.ins().sqrt(lhs[0])]
    }

    fn sin(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn cos(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn tan(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }
}
