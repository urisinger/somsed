use anyhow::{anyhow, Context, Result};
use builder::CraneliftBuilder;
use cranelift::{
    codegen::ir::Function,
    prelude::{isa::CallConv, *},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, FuncOrDataId, Linkage, Module};
use jit::{
    convert_list, ExplicitFnImpl, ExplicitListFnImpl, ImplicitFnImpl, ImplicitListFnImpl,
    ListLayout,
};

use crate::{
    expressions::Expressions,
    lang::{
        codegen::CodeGen,
        expr::{Expr, Node},
        generic_value::{GenericList, GenericValue, ValueType},
    },
};

use super::{
    compiled_exprs::{CompiledExpr, CompiledExprs},
    jit::{ExplicitJitFn, ImplicitJitFn, JitValue, PointValue},
    ExecutionEngine,
};

pub mod builder;
pub mod jit;
pub mod value;

mod functions;

pub struct CraneliftBackend {
    module: JITModule,
    free_id: FuncId,
    malloc_id: FuncId,
}

impl CraneliftBackend {
    pub fn new() -> Result<Self> {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names())?;

        let mut module = JITModule::new(builder);

        let mut malloc_sig = module.make_signature();

        malloc_sig.params.push(AbiParam::new(types::I64));
        malloc_sig
            .returns
            .push(AbiParam::new(module.target_config().pointer_type()));

        let malloc_id = module.declare_function("malloc", Linkage::Import, &malloc_sig)?;

        let mut free_sig = module.make_signature();

        free_sig
            .params
            .push(AbiParam::new(module.target_config().pointer_type()));

        free_sig.params.push(AbiParam::new(types::I64));

        let free_id = module.declare_function("free", Linkage::Import, &free_sig)?;

        Ok(Self {
            module,
            free_id,
            malloc_id,
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_single_function<'a>(
        &mut self,
        name: &str,
        args: &[ValueType],
        ret_type: &ValueType,
        expr: &'a Node,
        codegen: &mut CodeGen<'a>,
        ctx: &mut codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let (builder, func_id) = self.get_builder(name, args, ret_type, &mut ctx.func, builder_ctx);

        codegen
            .compile_fn(builder, expr)
            .with_context(|| format!("Failed to compile function `{}`", name))?;

        self.module
            .define_function(func_id, ctx)
            .with_context(|| format!("Failed to define function `{}`", name))?;

        println!("{}", ctx.func.display());
        self.module.clear_context(ctx);

        Ok(())
    }

    pub fn compile_expressions(&mut self, exprs: &Expressions) -> CompiledExprs<Self> {
        let mut codegen = CodeGen::new(exprs);
        let mut compiled_exprs: CompiledExprs<Self> = CompiledExprs::new();

        let mut ctx = codegen::Context::new();
        let mut builder_ctx = FunctionBuilderContext::new();

        for (id, expr) in &exprs.exprs {
            match &expr.expr {
                Expr::Implicit { lhs, rhs, .. } => {
                    let args = [ValueType::Number(()), ValueType::Number(())];

                    let lhs_name = format!("implicit_{}_lhs", id.0);
                    match lhs.return_type(exprs, &args) {
                        Ok(ret_type) => {
                            if let Err(e) = self.compile_single_function(
                                &lhs_name,
                                &args,
                                &ret_type,
                                lhs,
                                &mut codegen,
                                &mut ctx,
                                &mut builder_ctx,
                            ) {
                                compiled_exprs.errors.insert(*id, format!("{:?}", e));
                            }
                        }
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        }
                    }

                    let rhs_name = format!("implicit_{}_rhs", id.0);
                    match rhs.return_type(exprs, &args) {
                        Ok(ret_type) => {
                            if let Err(e) = self.compile_single_function(
                                &rhs_name,
                                &args,
                                &ret_type,
                                rhs,
                                &mut codegen,
                                &mut ctx,
                                &mut builder_ctx,
                            ) {
                                compiled_exprs.errors.insert(*id, format!("{:?}", e));
                            }
                        }
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        }
                    }
                }

                Expr::Explicit { lhs } => {
                    let name = format!("explicit_{}", id.0);
                    let args = if expr.used_idents.contains("x") {
                        vec![ValueType::Number(())]
                    } else {
                        vec![]
                    };

                    match lhs.return_type(exprs, &args) {
                        Ok(ret_type) => {
                            if let Err(e) = self.compile_single_function(
                                &name,
                                &args,
                                &ret_type,
                                lhs,
                                &mut codegen,
                                &mut ctx,
                                &mut builder_ctx,
                            ) {
                                compiled_exprs.errors.insert(*id, format!("{:?}", e));
                            }
                        }
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        }
                    }
                }

                Expr::VarDef { rhs, ident } => {
                    if !expr.used_idents.is_empty() {
                        continue;
                    }

                    let name = ident.to_string();
                    let args = [];

                    match rhs.return_type(exprs, &args) {
                        Ok(ret_type) => {
                            if let Err(e) = self.compile_single_function(
                                &name,
                                &args,
                                &ret_type,
                                rhs,
                                &mut codegen,
                                &mut ctx,
                                &mut builder_ctx,
                            ) {
                                compiled_exprs.errors.insert(*id, format!("{:?}", e));
                            }
                        }
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        }
                    }
                }

                _ => continue,
            }
        }

        self.module.finalize_definitions().unwrap();

        // Only then we can retrive the functions
        for (id, expr) in &exprs.exprs {
            if compiled_exprs.errors.contains_key(id) {
                continue;
            }
            match &expr.expr {
                Expr::Implicit { op, lhs, rhs } => {
                    let lhs_name = format!("implicit_{}_lhs", id.0);
                    let lhs_result = self
                        .get_implicit_fn(
                            &lhs_name,
                            &lhs.return_type(
                                &exprs,
                                &[ValueType::Number(()), ValueType::Number(())],
                            )
                            .expect("type should have been checked before"),
                        )
                        .ok_or_else(|| anyhow!("fn {} does not exist", lhs_name));

                    let rhs_name = format!("implicit_{}_rhs", id.0);
                    let rhs_result = self
                        .get_implicit_fn(
                            &rhs_name,
                            &rhs.return_type(&exprs, &[ValueType::Number(())])
                                .expect("type should have been checked before"),
                        )
                        .ok_or_else(|| anyhow!("fn {} does not exist", rhs_name));

                    let result = match (lhs_result, rhs_result) {
                        (Ok(lhs), Ok(rhs)) => Ok(CompiledExpr::Implicit { lhs, op: *op, rhs }),
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                    };

                    compiled_exprs.insert(*id, result);
                }
                Expr::Explicit { lhs } => {
                    let name = format!("explicit_{}", id.0);

                    let value = if expr.used_idents.contains("x") {
                        self.get_explicit_fn(
                            &name,
                            &lhs.return_type(&exprs, &[ValueType::Number(())])
                                .expect("type should have been checked before"),
                        )
                        .ok_or_else(|| anyhow!("fn {} does not exist", name))
                        .map(|lhs| CompiledExpr::Explicit { lhs })
                    } else {
                        let return_type = &lhs
                            .return_type(&exprs, &[])
                            .expect("type should have been checked before");

                        self.eval(&name, return_type)
                            .ok_or_else(|| anyhow!("Could not find function {name}"))
                            .map(|value| CompiledExpr::Constant { value })
                    };

                    compiled_exprs.insert(*id, value);
                }
                Expr::VarDef { ident, rhs } => {
                    if !expr.used_idents.is_empty() {
                        continue;
                    }

                    let name = ident.to_string();

                    let return_type = &rhs
                        .return_type(&exprs, &[])
                        .expect("type should have been checked before");

                    let value = self.eval(&name, return_type);

                    compiled_exprs.insert(
                        *id,
                        value
                            .ok_or_else(|| anyhow!("Could not find function {name}"))
                            .map(|value| CompiledExpr::Constant { value }),
                    );
                }
                _ => {}
            };
        }

        compiled_exprs
    }

    pub fn get_builder<'a>(
        &mut self,
        name: &str,
        types: &[ValueType],
        return_type: &ValueType,
        func: &'a mut Function,
        ctx: &'a mut FunctionBuilderContext,
    ) -> (CraneliftBuilder<'a, '_>, FuncId) {
        func.signature = Signature::new(CallConv::SystemV);

        for ty in types {
            match ty {
                GenericValue::Number(_) => {
                    func.signature.params.push(AbiParam::new(types::F64));
                }
                GenericValue::Point(_) => {
                    func.signature.params.push(AbiParam::new(types::F64));
                    func.signature.params.push(AbiParam::new(types::F64));
                }
                GenericValue::List(GenericList::Number(_)) => {
                    func.signature.params.push(AbiParam::new(types::I64));
                    func.signature.params.push(AbiParam::new(types::I64));
                }
                GenericValue::List(GenericList::PointList(_)) => {
                    func.signature.params.push(AbiParam::new(types::I64));
                    func.signature.params.push(AbiParam::new(types::I64));
                }
            };
        }

        match return_type {
            GenericValue::Number(_) => {
                func.signature.returns.push(AbiParam::new(types::F64));
            }
            GenericValue::Point(_) => {
                func.signature.returns.push(AbiParam::new(types::F64));
                func.signature.returns.push(AbiParam::new(types::F64));
            }
            GenericValue::List(GenericList::Number(_)) => {
                func.signature.returns.push(AbiParam::new(types::I64));
                func.signature.returns.push(AbiParam::new(types::I64));
            }
            GenericValue::List(GenericList::PointList(_)) => {
                func.signature.returns.push(AbiParam::new(types::I64));
                func.signature.returns.push(AbiParam::new(types::I64));
            }
        }
        let func_id = self
            .module
            .declare_function(name, Linkage::Export, &func.signature)
            .expect("failed to declare function, this indicates a bug");

        (
            CraneliftBuilder::new(self, FunctionBuilder::new(func, ctx), &types),
            func_id,
        )
    }
}
