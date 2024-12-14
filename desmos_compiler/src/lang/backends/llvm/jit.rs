use anyhow::Result;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};

use super::types::{ListType, ValueType};

#[repr(C)]
pub struct ListLayout {
    pub size: i64,
    pub ptr: *mut u8,
}

type ExplicitFn<'ctx, Output> = JitFunction<'ctx, unsafe extern "C" fn(f64) -> Output>;

type ImplicitFn<'ctx, Output> = JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> Output>;

pub enum ExplicitJitListFn<'ctx> {
    Number(ExplicitFn<'ctx, ListLayout>),
}

pub enum ExplicitJitFn<'ctx> {
    Number(ExplicitFn<'ctx, f64>),
    List(ExplicitJitListFn<'ctx>),
}

pub enum ImplicitJitListFn<'ctx> {
    Number(ImplicitFn<'ctx, ListLayout>),
}

pub enum ImplicitJitFn<'ctx> {
    Number(ImplicitFn<'ctx, f64>),
    List(ImplicitJitListFn<'ctx>),
}

impl<'ctx> ExplicitJitFn<'ctx> {
    pub unsafe fn from_function(
        function_name: &str,
        execution_engine: &ExecutionEngine<'ctx>,
        return_type: ValueType,
    ) -> Result<Self> {
        match return_type {
            ValueType::Number => {
                let jit_function: Result<JitFunction<'ctx, unsafe extern "C" fn(f64) -> f64>, _> =
                    execution_engine.get_function(function_name);

                Ok(jit_function.map(ExplicitJitFn::Number)?)
            }
            ValueType::List(list_type) => match list_type {
                ListType::Number => {
                    let jit_function: Result<
                        JitFunction<'ctx, unsafe extern "C" fn(f64) -> ListLayout>,
                        _,
                    > = execution_engine.get_function(function_name);

                    Ok(jit_function
                        .map(|func| ExplicitJitFn::List(ExplicitJitListFn::Number(func)))?)
                }
            },
        }
    }
}

impl<'ctx> ImplicitJitFn<'ctx> {
    pub unsafe fn from_function(
        function_name: &str,
        execution_engine: &ExecutionEngine<'ctx>,
        return_type: ValueType,
    ) -> Result<Self> {
        match return_type {
            ValueType::Number => {
                let jit_function: Result<
                    JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> f64>,
                    _,
                > = execution_engine.get_function(function_name);

                Ok(jit_function.map(ImplicitJitFn::Number)?)
            }
            ValueType::List(list_type) => match list_type {
                ListType::Number => {
                    let jit_function: Result<
                        JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> ListLayout>,
                        _,
                    > = execution_engine.get_function(function_name);

                    Ok(jit_function
                        .map(|func| ImplicitJitFn::List(ImplicitJitListFn::Number(func)))?)
                }
            },
        }
    }
}
