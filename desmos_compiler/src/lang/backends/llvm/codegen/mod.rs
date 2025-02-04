use std::collections::HashMap;

use anyhow::{bail, Context, Result};
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;

use inkwell::types::{BasicType, FloatType, StructType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::expressions::Expressions;
use crate::lang::parser::{Expr, Node, ResolvedExpr};
use functions::IMPORTED_FUNCTIONS;

use self::functions::{free, malloc};

use super::jit::{ExplicitJitFn, ImplicitJitFn, JitListValue, JitValue, ListLayout, PointLayout};
use super::types::{CompilerListType, CompilerType};
use super::value::CompilerValue;
use super::{CompiledExpr, CompiledExprs};

mod bin_op;
mod expr;
pub mod functions;
mod list;
mod unary_op;

pub fn compile_all_exprs<'ctx>(
    context: &'ctx inkwell::context::Context,
    exprs: &Expressions,
) -> CompiledExprs<'ctx> {
    let mut codegen = CodeGen::new(context, exprs);

    let execution_engine = codegen
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let malloc_type = context.ptr_type(AddressSpace::default()).fn_type(
        &[context.i64_type().into(), context.i64_type().into()],
        false,
    );

    let malloc_function = codegen.module.add_function("malloc", malloc_type, None);
    execution_engine.add_global_mapping(&malloc_function, malloc as usize);

    let free_type = context.void_type().fn_type(
        &[
            context.ptr_type(AddressSpace::default()).into(),
            context.i64_type().into(),
            context.i64_type().into(),
        ],
        false,
    );

    let free_function = codegen.module.add_function("free", free_type, None);
    execution_engine.add_global_mapping(&free_function, free as usize);

    IMPORTED_FUNCTIONS
        .into_iter()
        .for_each(|(name, ret, args, p)| {
            let fn_type = ret.type_enum().fn_type(
                &args.iter().map(|arg| arg.metadata()).collect::<Vec<_>>(),
                false,
            );

            let function = codegen.module.add_function(name, fn_type, None);
            execution_engine.add_global_mapping(&function, p as usize);
        });

    let mut compiled_exprs = CompiledExprs::new();

    //We first have to compile everything
    for (id, expr) in &exprs.exprs {
        match &expr.expr {
            Expr::Implicit { lhs, rhs, .. } => {
                let args_t = [
                    CompilerType::Number(codegen.float_type),
                    CompilerType::Number(codegen.float_type),
                ];

                let lhs_name = format!("implicit_{}_lhs", id.0);
                _ = codegen
                    .compile_fn(&lhs_name, lhs, &args_t, Some(0), Some(1))
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });

                let rhs_name = format!("implicit_{}_rhs", id.0);
                _ = codegen
                    .compile_fn(&rhs_name, rhs, &args_t, Some(0), Some(1))
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });
            }
            Expr::Explicit { lhs } => {
                let name = format!("explicit_{}", id.0);

                let args = if expr.used_idents.contains("x") {
                    vec![CompilerType::Number(codegen.float_type)]
                } else {
                    vec![]
                };

                _ = codegen
                    .compile_fn(&name, lhs, &args, Some(0), None)
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });
            }
            Expr::VarDef { rhs, ident } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }
                let name = format!("{}_number_number", ident);

                let args = vec![];
                _ = codegen
                    .compile_fn(&name, rhs, &args, None, None)
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });
            }
            _ => {}
        }
    }

    // Only then we can retrive the functions
    for (id, expr) in &exprs.exprs {
        if compiled_exprs.errors.contains_key(id) {
            continue;
        }
        match &expr.expr {
            Expr::Implicit { op, .. } => {
                let lhs_name = format!("implicit_{}_lhs", id.0);
                let lhs_result = unsafe {
                    ImplicitJitFn::from_function(
                        &lhs_name,
                        &execution_engine,
                        codegen
                            .return_types
                            .get(&lhs_name)
                            .cloned()
                            .unwrap_or_else(|| {
                                panic!("Return type not found for lhs: {}", lhs_name);
                            }),
                    )
                };

                let rhs_name = format!("implicit_{}_rhs", id.0);
                let rhs_result = unsafe {
                    ImplicitJitFn::from_function(
                        &rhs_name,
                        &execution_engine,
                        *codegen.return_types.get(&rhs_name).unwrap_or_else(|| {
                            panic!("Return type not found for rhs: {}", rhs_name);
                        }),
                    )
                };

                let result = match (lhs_result, rhs_result) {
                    (Ok(lhs), Ok(rhs)) => Ok(CompiledExpr::Implicit { lhs, op: *op, rhs }),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
                };

                compiled_exprs.insert(*id, result);
            }
            Expr::Explicit { .. } => {
                let name = format!("explicit_{}", id.0);

                let value = if expr.used_idents.contains("x") {
                    unsafe {
                        ExplicitJitFn::from_function(
                            &name,
                            &execution_engine,
                            *codegen.return_types.get(&name).unwrap_or_else(|| {
                                panic!("Return type not found for explicit function: {}", name);
                            }),
                        )
                        .map(|lhs| CompiledExpr::Explicit { lhs })
                    }
                } else {
                    let return_type = *codegen.return_types.get(&name).unwrap_or_else(|| {
                        panic!("Return type not found for explicit function: {}", name);
                    });
                    eval_function(&execution_engine, return_type, &name)
                        .map(|value| CompiledExpr::Constant { value })
                };

                compiled_exprs.insert(*id, value);
            }
            Expr::VarDef { ident, .. } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }
                let name = format!("{}_number_number", ident);

                let return_type = *codegen.return_types.get(&name).unwrap_or_else(|| {
                    panic!("Return type not found for explicit function: {}", name);
                });

                let value = eval_function(&execution_engine, return_type, &name);

                compiled_exprs.insert(
                    *id,
                    value
                        .context("failed to lookup function")
                        .map(|value| CompiledExpr::Constant { value }),
                );
            }
            _ => {}
        }
    }

    compiled_exprs
}

fn eval_function<'ctx>(
    execution_engine: &ExecutionEngine<'ctx>,
    return_type: CompilerType<'ctx>,
    name: &str,
) -> Result<JitValue> {
    unsafe {
        match return_type {
            CompilerType::Number(_) => execution_engine
                .get_function(name)
                .map(|f: JitFunction<unsafe extern "C" fn() -> f64>| JitValue::Number(f.call())),

            CompilerType::Point(_) => execution_engine.get_function(name).map(
                |f: JitFunction<unsafe extern "C" fn() -> PointLayout>| JitValue::Point(f.call()),
            ),
            CompilerType::List(list_t) => match list_t {
                CompilerListType::Number(_) => execution_engine.get_function(name).map(
                    |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                        JitValue::List(JitListValue::Number(f.call()))
                    },
                ),

                CompilerListType::Point(_) => execution_engine.get_function(name).map(
                    |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                        JitValue::List(JitListValue::Point(f.call()))
                    },
                ),
            },
        }
        .context("Could not get function")
    }
}

pub struct FnContext<'ctx, 'a> {
    args: &'a [CompilerValue<'ctx>],
    x_index: Option<u32>,
    y_index: Option<u32>,
}

impl<'ctx> FnContext<'ctx, '_> {
    pub fn get_x(&self) -> Option<&CompilerValue<'ctx>> {
        self.x_index.and_then(|i| self.args.get(i as usize))
    }

    pub fn get_y(&self) -> Option<&CompilerValue<'ctx>> {
        self.y_index.and_then(|i| self.args.get(i as usize))
    }
}

pub struct CodeGen<'ctx, 'expr> {
    pub module: Module<'ctx>,
    pub return_types: HashMap<String, CompilerType<'ctx>>,

    context: &'ctx inkwell::context::Context,
    builder: Builder<'ctx>,
    exprs: &'expr Expressions,

    float_type: FloatType<'ctx>,
    point_type: StructType<'ctx>,
    list_type: StructType<'ctx>,
}

impl<'ctx, 'expr> CodeGen<'ctx, 'expr> {
    pub fn new(context: &'ctx inkwell::context::Context, exprs: &'expr Expressions) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            return_types: HashMap::new(),
            exprs,
            float_type: context.f64_type(),
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
        }
    }

    pub fn codegen_fn_call(
        &mut self,
        name: &str,
        fn_args: &[CompilerValue<'ctx>],
    ) -> Result<CompilerValue<'ctx>> {
        let types: Vec<_> = fn_args.iter().map(|v| v.get_type()).collect();
        let len = types.iter().map(|t| t.name().len() + 1).sum::<usize>() + name.len();

        let mut specialized_name = String::with_capacity(len);
        specialized_name.push_str(name);

        for t in fn_args {
            specialized_name.push('_');
            specialized_name.push_str(t.get_type().name());
        }

        let (function, ret_type) = match self.module.get_function(&specialized_name) {
            Some(function) => (
                function,
                *self
                    .return_types
                    .get(&specialized_name)
                    .expect("compile_fn should have inserted return type"),
            ),
            None => match self.exprs.get_expr(name) {
                Some(Expr::FnDef { rhs, args, .. }) => {
                    let block = self.builder.get_insert_block();

                    let x_index = args.iter().position(|s| *s == "x").map(|x| x as u32);

                    let y_index = args.iter().position(|s| *s == "y").map(|x| x as u32);

                    let function =
                        self.compile_fn(&specialized_name, rhs, &types, x_index, y_index);

                    if let Some(b) = block {
                        self.builder.position_at_end(b)
                    }

                    let return_type = self
                        .return_types
                        .get(&specialized_name)
                        .expect("compile_fn should have inserted return type");

                    (function?, *return_type)
                }
                None => bail!("no exprssion found for function {name}"),
                _ => unreachable!("this indicates a bug"),
            },
        };

        let fn_args = fn_args
            .iter()
            .map(|arg| Ok(BasicMetadataValueEnum::from(arg.as_basic_value_enum())))
            .collect::<Result<Vec<_>>>()?;

        CompilerValue::from_basic_value_enum(
            self.builder
                .build_call(function, &fn_args, name)?
                .try_as_basic_value()
                .expect_left("return type should not be void"),
            ret_type,
        )
        .context("ret type does not match expected ret type")
    }

    pub fn get_var(
        &mut self,
        name: &str,
        fn_context: &FnContext<'ctx, '_>,
    ) -> Result<CompilerValue<'ctx>> {
        match self.module.get_function(name) {
            Some(global) => CompilerValue::from_basic_value_enum(
                self.builder
                    .build_call(global, &[], name)?
                    .try_as_basic_value()
                    .expect_left("return type should not be void"),
                self.return_types[name],
            )
            .context("type error"),
            None => match self.exprs.get_resolved_expr(name) {
                Some(ResolvedExpr {
                    expr: Expr::VarDef { rhs, .. },
                    used_idents,
                }) => {
                    let x_var = fn_context
                        .get_x()
                        .cloned()
                        .unwrap_or_else(|| CompilerValue::Number(self.float_type.const_zero()));
                    let y_var = fn_context
                        .get_y()
                        .cloned()
                        .unwrap_or_else(|| CompilerValue::Number(self.float_type.const_zero()));
                    let block = self.builder.get_insert_block();
                    let compute_global = self.compile_fn(
                        name,
                        rhs,
                        &[x_var.get_type(), y_var.get_type()],
                        Some(0),
                        Some(1),
                    )?;

                    if let Some(b) = block {
                        self.builder.position_at_end(b)
                    }

                    if used_idents.contains("y") && fn_context.y_index.is_none() {
                        bail!("try adding variable y or making the eqution explicit");
                    }

                    CompilerValue::from_basic_value_enum(
                        self.builder
                            .build_call(
                                compute_global,
                                &[
                                    x_var.as_basic_value_enum().into(),
                                    y_var.as_basic_value_enum().into(),
                                ],
                                name,
                            )?
                            .try_as_basic_value()
                            .expect_left("return type should not be void"),
                        self.return_types[name],
                    )
                    .context("type error")
                }
                None => bail!("no exprssion found for function {name}"),
                _ => unreachable!("this indicates a bug"),
            },
        }
    }

    pub fn compile_fn(
        &mut self,
        name: &str,
        node: &Node,
        args: &[CompilerType<'ctx>],
        x_index: Option<u32>,
        y_index: Option<u32>,
    ) -> Result<FunctionValue<'ctx>> {
        let types: Vec<_> = args.iter().map(|t| t.metadata()).collect();

        let ret_type = self.return_type(node, args)?;

        self.return_types.insert(name.to_owned(), ret_type);

        let fn_type = match ret_type {
            CompilerType::List(list_type) => list_type.get_type().fn_type(&types, false),
            CompilerType::Point(p) => p.fn_type(&types, false),
            CompilerType::Number(n) => n.fn_type(&types, false),
        };

        let function = self.module.add_function(name, fn_type, None);
        let args = (0..args.len())
            .map(|i| {
                CompilerValue::from_basic_value_enum(
                    function.get_nth_param(i as u32).expect("should not happen"),
                    args[i],
                )
                .unwrap()
            })
            .collect::<Vec<_>>();

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let value = self.codegen_expr(
            node,
            &FnContext {
                args: &args,
                x_index,
                y_index,
            },
        )?;

        self.builder
            .build_return(Some(&value.as_basic_value_enum()))?;

        Ok(function)
    }
}
