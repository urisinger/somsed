use anyhow::Result;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};

use super::types::{CompilerType, ListType};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ListLayout {
    pub size: i64,
    pub ptr: *mut u8,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct PointLayout {
    pub x: f64,
    pub y: f64,
}

pub type ExplicitFn<'ctx, Output> = JitFunction<'ctx, unsafe extern "C" fn(f64) -> Output>;

pub type ImplicitFn<'ctx, Output> = JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> Output>;

pub enum ExplicitJitListFn<'ctx> {
    Number(ExplicitFn<'ctx, ListLayout>),
    Point(ExplicitFn<'ctx, ListLayout>),
}

pub enum ExplicitJitFn<'ctx> {
    Number(ExplicitFn<'ctx, f64>),
    Point(ExplicitFn<'ctx, PointLayout>),
    List(ExplicitJitListFn<'ctx>),
}

pub enum ImplicitJitListFn<'ctx> {
    Number(ImplicitFn<'ctx, ListLayout>),
    Point(ImplicitFn<'ctx, ListLayout>),
}

pub enum ImplicitJitFn<'ctx> {
    Number(ImplicitFn<'ctx, f64>),
    Point(ImplicitFn<'ctx, PointLayout>),
    List(ImplicitJitListFn<'ctx>),
}

#[derive(Debug, Clone)]
pub enum JitListValue {
    Number(ListLayout),
    Point(ListLayout),
}

#[derive(Debug, Clone)]
pub enum JitValue {
    Number(f64),
    Point(PointLayout),
    List(JitListValue),
}

impl<'ctx> ExplicitJitFn<'ctx> {
    pub(crate) unsafe fn from_function(
        function_name: &str,
        execution_engine: &ExecutionEngine<'ctx>,
        return_type: CompilerType,
    ) -> Result<Self> {
        match return_type {
            CompilerType::Number(_) => {
                let jit_function = execution_engine.get_function(function_name)?;
                Ok(ExplicitJitFn::Number(jit_function))
            }
            CompilerType::Point(_) => {
                let jit_function = execution_engine.get_function(function_name)?;
                Ok(ExplicitJitFn::Point(jit_function))
            }
            CompilerType::List(list_type) => match list_type {
                ListType::Number(_) => {
                    let jit_function = execution_engine.get_function(function_name)?;
                    Ok(ExplicitJitFn::List(ExplicitJitListFn::Number(jit_function)))
                }
                ListType::Point(_) => {
                    let jit_function = execution_engine.get_function(function_name)?;
                    Ok(ExplicitJitFn::List(ExplicitJitListFn::Point(jit_function)))
                }
            },
        }
    }
}

impl ExplicitJitFn<'_> {
    /// Calls the JIT-compiled function and returns a `JitValue` as the result.
    ///
    /// # Safety
    /// This function is unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg: f64) -> JitValue {
        match self {
            ExplicitJitFn::Number(jit_fn) => JitValue::Number(jit_fn.call(arg)),
            ExplicitJitFn::Point(jit_fn) => JitValue::Point(jit_fn.call(arg)),
            ExplicitJitFn::List(list_fn) => JitValue::List(list_fn.call(arg)),
        }
    }
}

impl ExplicitJitListFn<'_> {
    /// Calls the JIT-compiled list function and returns a `JitValue::List` result.
    ///
    /// # Safety
    /// This function is unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg: f64) -> JitListValue {
        match self {
            ExplicitJitListFn::Number(jit_fn) => JitListValue::Number(jit_fn.call(arg)),
            ExplicitJitListFn::Point(jit_fn) => JitListValue::Point(jit_fn.call(arg)),
        }
    }
}

impl<'ctx> ImplicitJitFn<'ctx> {
    pub(crate) unsafe fn from_function(
        function_name: &str,
        execution_engine: &ExecutionEngine<'ctx>,
        return_type: CompilerType,
    ) -> Result<Self> {
        match return_type {
            CompilerType::Number(_) => {
                let jit_function = execution_engine.get_function(function_name)?;
                Ok(ImplicitJitFn::Number(jit_function))
            }
            CompilerType::Point(_) => {
                let jit_function = execution_engine.get_function(function_name)?;
                Ok(ImplicitJitFn::Point(jit_function))
            }
            CompilerType::List(list_type) => match list_type {
                ListType::Number(_) => {
                    let jit_function = execution_engine.get_function(function_name)?;
                    Ok(ImplicitJitFn::List(ImplicitJitListFn::Number(jit_function)))
                }
                ListType::Point(_) => {
                    let jit_function = execution_engine.get_function(function_name)?;
                    Ok(ImplicitJitFn::List(ImplicitJitListFn::Point(jit_function)))
                }
            },
        }
    }
}

impl ImplicitJitFn<'_> {
    /// Calls the JIT-compiled function and returns a `JitValue` as the result.
    ///
    /// # Safety
    /// This function is marked unsafe because it calls JIT-compiled code.
    pub unsafe fn call(&self, arg1: f64, arg2: f64) -> JitValue {
        match self {
            ImplicitJitFn::Number(jit_fn) => JitValue::Number(jit_fn.call(arg1, arg2)),
            ImplicitJitFn::Point(jit_fn) => JitValue::Point(jit_fn.call(arg1, arg2)),
            ImplicitJitFn::List(jit_fn) => JitValue::List(jit_fn.call(arg1, arg2)),
        }
    }
}

impl ImplicitJitListFn<'_> {
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
            ImplicitJitListFn::Point(jit_fn) => {
                let layout = jit_fn.call(arg1, arg2);
                JitListValue::Point(layout)
            }
        }
    }
}
