use std::rc::Rc;

use anyhow::{anyhow, Result};
use builder::LLVMBuilder;
use cranelift::codegen;
use inkwell::{
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::{BasicType, FloatType, IntType, StructType},
    values::FunctionValue,
    AddressSpace, OptimizationLevel,
};
use jit::{
    convert_list, ExplicitLLVMFn, ExplicitLLVMListFn, ImplicitLLVMFn, ImplicitLLVMListFn,
    ListLayout,
};
use value::{LLVMType, LLVMValue};

use functions::{free, malloc};

pub mod builder;
pub mod jit;
mod value;

mod functions;

use desmos_compiler::lang::{
    expressions::Expressions,
    lang::{
        codegen::CodeGen,
        expr::{Expr, Node},
        generic_value::{GenericList, GenericValue, ValueType},
    },
};

use desmos_compiler::lang::codegen::backend::{
    compiled_exprs::{CompiledExpr, CompiledSegments},
    jit::{ExplicitJitFn, ImplicitJitFn, JitValue, PointValue},
    ExecutionEngine,
};

pub struct LLVMBackend<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,

    malloc_function: FunctionValue<'ctx>,

    free_function: FunctionValue<'ctx>,

    number_type: FloatType<'ctx>,
    point_type: StructType<'ctx>,
    list_type: StructType<'ctx>,
    i64_type: IntType<'ctx>,
}

impl<'ctx> LLVMBackend<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let malloc_type = context.ptr_type(AddressSpace::default()).fn_type(
            &[context.i64_type().into(), context.i64_type().into()],
            false,
        );

        let malloc_function = module.add_function("malloc", malloc_type, None);

        let free_type = context.void_type().fn_type(
            &[
                context.ptr_type(AddressSpace::default()).into(),
                context.i64_type().into(),
                context.i64_type().into(),
            ],
            false,
        );

        let free_function = module.add_function("free", free_type, None);

        Self {
            context,
            module,

            malloc_function,
            free_function,

            number_type: context.f64_type(),
            point_type: context.struct_type(
                &[
                    context.f64_type().as_basic_type_enum(),
                    context.f64_type().as_basic_type_enum(),
                ],
                false,
            ),
            list_type: context.struct_type(
                &[
                    context.i64_type().as_basic_type_enum(),
                    context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                ],
                false,
            ),
            i64_type: context.i64_type(),
        }
    }

    pub fn compile_expressions(
        &mut self,
        exprs: &Expressions,
    ) -> CompiledSegments<LLVMExecutionEngine<'ctx>> {
        let mut codegen = CodeGen::new(exprs);

        let mut compiled_exprs: CompiledSegments<LLVMExecutionEngine> = CompiledSegments::new();

        //We first have to compile everything
        for (id, expr) in &exprs.exprs {
            match &expr.expr {
                Expr::Implicit { lhs, rhs, .. } => {
                    let args_t = [ValueType::Number(()), ValueType::Number(())];

                    let lhs_name = format!("implicit_{}_lhs", id.0);
                    let lhs_ret_t = match lhs.return_type(exprs, &args_t) {
                        Ok(ret) => ret,
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                            continue;
                        }
                    };

                    _ = codegen
                        .compile_fn(self.get_builder(&lhs_name, &args_t, &lhs_ret_t), lhs)
                        .inspect_err(|e| {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        });

                    let rhs_name = format!("implicit_{}_rhs", id.0);
                    let rhs_ret_t = match rhs.return_type(exprs, &args_t) {
                        Ok(ret) => ret,
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                            continue;
                        }
                    };
                    _ = codegen
                        .compile_fn(self.get_builder(&rhs_name, &args_t, &rhs_ret_t), rhs)
                        .inspect_err(|e| {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        });
                }
                Expr::Explicit { lhs } => {
                    let name = format!("explicit_{}", id.0);

                    let args = if expr.used_idents.contains("x") {
                        vec![ValueType::Number(())]
                    } else {
                        vec![]
                    };

                    let lhs_ret_t = match lhs.return_type(exprs, &args) {
                        Ok(ret) => ret,
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                            continue;
                        }
                    };
                    _ = codegen
                        .compile_fn(self.get_builder(&name, &args, &lhs_ret_t), lhs)
                        .inspect_err(|e| {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        });
                }
                Expr::VarDef { rhs, ident } => {
                    if !expr.used_idents.is_empty() {
                        continue;
                    }
                    let name = ident.to_string();
                    println!("{:?}", expr.used_idents);

                    let args = [];
                    let rhs_ret_t = match rhs.return_type(exprs, &args) {
                        Ok(ret) => ret,
                        Err(e) => {
                            compiled_exprs.errors.insert(*id, e.to_string());
                            continue;
                        }
                    };
                    _ = codegen
                        .compile_fn(self.get_builder(&name, &args, &rhs_ret_t), rhs)
                        .inspect_err(|e| {
                            compiled_exprs.errors.insert(*id, e.to_string());
                        });
                }
                _ => {}
            }
        }

        let execution_engine = self.get_execution_engine();

        // Only then we can retrive the functions
        for (id, expr) in &exprs.exprs {
            if compiled_exprs.errors.contains_key(id) {
                continue;
            }
            match &expr.expr {
                Expr::Implicit { op, lhs, rhs } => {
                    let lhs_name = format!("implicit_{}_lhs", id.0);
                    let lhs_result = execution_engine
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
                    let rhs_result = execution_engine
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
                        execution_engine
                            .get_explicit_fn(
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

                        execution_engine
                            .eval(&name, return_type)
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

                    let value = execution_engine.eval(&name, return_type);

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

    fn get_builder<'a>(
        &'a self,
        name: &str,
        types: &[ValueType],
        return_type: &ValueType,
    ) -> LLVMBuilder<'ctx, 'a> {
        let fn_type = LLVMType::from_type(
            return_type,
            self.number_type,
            self.point_type,
            self.list_type,
        )
        .fn_type(
            &types
                .iter()
                .map(|ty| {
                    LLVMType::from_type(ty, self.number_type, self.point_type, self.list_type)
                        .as_basic_type_enum()
                        .into()
                })
                .collect::<Vec<_>>(),
            false,
        );

        let function = self.module.add_function(name, fn_type, None);
        let builder = self.context.create_builder();

        let entry = self.context.append_basic_block(function, "entry");
        builder.position_at_end(entry);

        let args = types
            .iter()
            .enumerate()
            .map(|(index, ty)| {
                LLVMValue::from_basic_value_enum(
                    function
                        .get_nth_param(index as u32)
                        .expect("param should exist"),
                    ty,
                )
                .expect("param should be of the right type")
            })
            .collect();

        LLVMBuilder {
            backend: self,
            builder,
            function,
            args,
        }
    }

    fn get_execution_engine(&self) -> LLVMExecutionEngine<'ctx> {
        let execution_engine = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        self.module.print_to_stderr();

        execution_engine.add_global_mapping(&self.malloc_function, malloc as usize);

        execution_engine.add_global_mapping(&self.free_function, free as usize);
        LLVMExecutionEngine { execution_engine }
    }
}

pub struct LLVMExecutionEngine<'ctx> {
    execution_engine: inkwell::execution_engine::ExecutionEngine<'ctx>,
}

impl<'ctx> ExecutionEngine for LLVMExecutionEngine<'ctx> {
    type ExplicitNumberFn = ExplicitLLVMFn<'ctx, f64>;
    type ExplicitPointFn = ExplicitLLVMFn<'ctx, PointValue>;
    type ExplicitNumberListFn = ExplicitLLVMListFn<'ctx>;
    type ExplicitPointListFn = ExplicitLLVMListFn<'ctx>;

    type ImplicitNumberFn = ImplicitLLVMFn<'ctx, f64>;
    type ImplicitPointFn = ImplicitLLVMFn<'ctx, PointValue>;
    type ImplicitNumberListFn = ImplicitLLVMListFn<'ctx>;
    type ImplicitPointListFn = ImplicitLLVMListFn<'ctx>;
    fn eval(&self, name: &str, ty: &ValueType) -> Option<JitValue> {
        unsafe {
            match ty {
                GenericValue::Number(_) => self
                    .execution_engine
                    .get_function(name)
                    .map(|f: JitFunction<unsafe extern "C" fn() -> f64>| {
                        GenericValue::Number(f.call())
                    })
                    .ok(),

                GenericValue::Point(_) => self
                    .execution_engine
                    .get_function(name)
                    .map(|f: JitFunction<unsafe extern "C" fn() -> PointValue>| {
                        GenericValue::Point(f.call())
                    })
                    .ok(),
                GenericValue::List(list_t) => match list_t {
                    GenericList::Number(_) => self.execution_engine.get_function(name).ok().map(
                        |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                            GenericValue::List(GenericList::Number(convert_list(&f.call())))
                        },
                    ),

                    GenericList::Point(_) => self.execution_engine.get_function(name).ok().map(
                        |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                            GenericValue::List(GenericList::Point(convert_list(&f.call())))
                        },
                    ),
                },
            }
        }
    }

    fn get_explicit_fn(
        &self,
        name: &str,
        ty: &ValueType,
    ) -> Option<
        ExplicitJitFn<
            Self::ExplicitNumberFn,
            Self::ExplicitPointFn,
            Self::ExplicitNumberListFn,
            Self::ExplicitPointListFn,
        >,
    > {
        Some(match ty {
            GenericValue::Number(_) => {
                let func = unsafe { self.execution_engine.get_function(name).ok()? };
                GenericValue::Number(ExplicitLLVMFn { function: func })
            }
            GenericValue::Point(_) => {
                let func = unsafe { self.execution_engine.get_function(name).ok()? };
                GenericValue::Point(ExplicitLLVMFn { function: func })
            }
            GenericValue::List(generic_list) => GenericValue::List(match generic_list {
                GenericList::Number(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Number(ExplicitLLVMListFn { function: func })
                }
                GenericList::Point(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Point(ExplicitLLVMListFn { function: func })
                }
            }),
        })
    }

    fn get_implicit_fn(
        &self,
        name: &str,
        ty: &ValueType,
    ) -> Option<
        ImplicitJitFn<
            Self::ImplicitNumberFn,
            Self::ImplicitPointFn,
            Self::ImplicitNumberListFn,
            Self::ImplicitPointListFn,
        >,
    > {
        Some(match ty {
            GenericValue::Number(_) => {
                let func = unsafe { self.execution_engine.get_function(name).ok()? };
                GenericValue::Number(ImplicitLLVMFn { function: func })
            }
            GenericValue::Point(_) => {
                let func = unsafe { self.execution_engine.get_function(name).ok()? };
                GenericValue::Point(ImplicitLLVMFn { function: func })
            }
            GenericValue::List(generic_list) => GenericValue::List(match generic_list {
                GenericList::Number(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Number(ImplicitLLVMListFn { function: func })
                }
                GenericList::Point(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Point(ImplicitLLVMListFn { function: func })
                }
            }),
        })
    }
}
