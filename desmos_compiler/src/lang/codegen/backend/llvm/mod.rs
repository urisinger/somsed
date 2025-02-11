use builder::LLVMBuilder;
use inkwell::{
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::{BasicType, FloatType, StructType},
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

use crate::lang::generic_value::{GenericList, GenericValue, ValueType};

use super::{
    jit::{ExplicitJitFn, ImplicitJitFn, JitValue, PointValue},
    Backend, CompiledBackend, ExecutionEngine,
};

pub struct LLVMBackend<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,

    number_type: FloatType<'ctx>,
    point_type: StructType<'ctx>,
    list_type: StructType<'ctx>,
}

impl<'ctx> LLVMBackend<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            module: context.create_module("main"),

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
        }
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
                    GenericList::NumberList(_) => {
                        self.execution_engine.get_function(name).ok().map(
                            |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                                GenericValue::List(GenericList::NumberList(convert_list(&f.call())))
                            },
                        )
                    }

                    GenericList::PointList(_) => self.execution_engine.get_function(name).ok().map(
                        |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                            GenericValue::List(GenericList::PointList(convert_list(&f.call())))
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
                GenericList::NumberList(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::NumberList(ExplicitLLVMListFn { function: func })
                }
                GenericList::PointList(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::PointList(ExplicitLLVMListFn { function: func })
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
                GenericList::NumberList(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::NumberList(ImplicitLLVMListFn { function: func })
                }
                GenericList::PointList(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::PointList(ImplicitLLVMListFn { function: func })
                }
            }),
        })
    }
}

impl<'ctx> CompiledBackend for LLVMBackend<'ctx> {
    type Engine = LLVMExecutionEngine<'ctx>;
    fn get_execution_engine(&self) -> Self::Engine {
        let execution_engine = self
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let malloc_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );

        let malloc_function = self.module.add_function("malloc", malloc_type, None);
        execution_engine.add_global_mapping(&malloc_function, malloc as usize);

        let free_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );

        let free_function = self.module.add_function("free", free_type, None);
        execution_engine.add_global_mapping(&free_function, free as usize);
        LLVMExecutionEngine { execution_engine }
    }
}

impl<'ctx> Backend for LLVMBackend<'ctx> {
    type FnValue = FunctionValue<'ctx>;
    type Builder<'module>
        = LLVMBuilder<'module, 'ctx>
    where
        Self: 'module;

    fn get_builder<'a>(
        &'a self,
        name: &str,
        types: &[ValueType],
        return_type: &ValueType,
    ) -> Self::Builder<'a> {
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
            module: &self.module,
            builder,
            function,
            args,
            number_type: self.number_type,
            point_type: self.point_type,
            list_type: self.list_type,
            i64_type: self.context.i64_type(),
        }
    }

    fn get_fn(&self, name: &str) -> Option<Self::FnValue> {
        self.module.get_function(name)
    }
}
