use anyhow::Result;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};

use super::types::{ListType, ValueType};

#[repr(C)]
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum JitListValue {
    Number(ListLayout),
}

#[derive(Debug, Clone)]
pub enum JitValue {
    Number(f64),
    List(JitListValue),
}

impl<'ctx> ExplicitJitFn<'ctx> {
    pub(crate) unsafe fn from_function(
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

impl<'ctx> ExplicitJitFn<'ctx> {
    /// Calls the JIT-compiled function and returns a `JitValue` as the result.
    ///
    /// # Safety
    /// This function is unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg: f64) -> JitValue {
        match self {
            ExplicitJitFn::Number(jit_fn) => JitValue::Number(jit_fn.call(arg)),
            ExplicitJitFn::List(list_fn) => JitValue::List(list_fn.call(arg)),
        }
    }
}

impl<'ctx> ExplicitJitListFn<'ctx> {
    /// Calls the JIT-compiled list function and returns a `JitValue::List` result.
    ///
    /// # Safety
    /// This function is unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg: f64) -> JitListValue {
        match self {
            ExplicitJitListFn::Number(jit_fn) => JitListValue::Number(jit_fn.call(arg)),
        }
    }
}

impl<'ctx> ImplicitJitFn<'ctx> {
    pub(crate) unsafe fn from_function(
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

impl<'ctx> ImplicitJitFn<'ctx> {
    /// Calls the JIT-compiled function and returns a `JitValue` as the result.
    ///
    /// # Safety
    /// This function is marked unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg1: f64, arg2: f64) -> JitValue {
        match self {
            ImplicitJitFn::Number(jit_fn) => JitValue::Number(jit_fn.call(arg1, arg2)),
            ImplicitJitFn::List(jit_fn) => JitValue::List(jit_fn.call(arg1, arg2)),
        }
    }
}

impl<'ctx> ImplicitJitListFn<'ctx> {
    /// Calls the JIT-compiled list function and returns a `JitValue::List` result.
    ///
    /// # Safety
    /// This function is unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg1: f64, arg2: f64) -> JitListValue {
        match self {
            ImplicitJitListFn::Number(jit_fn) => {
                let layout = jit_fn.call(arg1, arg2);
                JitListValue::Number(layout)
            }
        }
    }
}
