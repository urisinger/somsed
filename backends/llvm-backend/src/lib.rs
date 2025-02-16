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

use desmos_compiler::lang::generic_value::{GenericList, GenericValue, ValueType};

use desmos_compiler::lang::codegen::backend::{
    compiled::{CompiledBackend, ExecutionEngine},
    jit::{ExplicitJitFn, ImplicitJitFn, JitValue, PointValue},
    Backend,
};

pub struct LLVMBackend<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,

    malloc_function: FunctionValue<'ctx>,

    free_function: FunctionValue<'ctx>,

    number_type: FloatType<'ctx>,
    point_type: StructType<'ctx>,
    list_type: StructType<'ctx>,
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
                    GenericList::Number(_) => self.execution_engine.get_function(name).ok().map(
                        |f: JitFunction<unsafe extern "C" fn() -> ListLayout>| {
                            GenericValue::List(GenericList::Number(convert_list(&f.call())))
                        },
                    ),

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
                GenericList::Number(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Number(ExplicitLLVMListFn { function: func })
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
                GenericList::Number(_) => {
                    let func = unsafe { self.execution_engine.get_function(name).ok()? };
                    GenericList::Number(ImplicitLLVMListFn { function: func })
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
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        self.module.print_to_stderr();

        execution_engine.add_global_mapping(&self.malloc_function, malloc as usize);

        execution_engine.add_global_mapping(&self.free_function, free as usize);
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
            context: self.context,
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
