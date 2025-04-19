use inkwell::execution_engine::JitFunction;

use desmos_compiler::lang::codegen::backend::jit::{
    ExplicitFn, ExplicitJitFn, ImplicitFn, PointValue,
};

use crate::LLVMExecutionEngine;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ListLayout {
    pub size: i64,
    pub ptr: *mut u8,
}

pub type ExplicitLLVMJitFn<'ctx> = ExplicitJitFn<LLVMExecutionEngine<'ctx>>;

pub struct ExplicitLLVMFn<'ctx, T> {
    pub function: JitFunction<'ctx, unsafe extern "C" fn(f64) -> T>,
}

impl<T> ExplicitFn<T> for ExplicitLLVMFn<'_, T> {
    fn call(&self, x: f64) -> T {
        unsafe { self.function.call(x) }
    }
}

pub struct ImplicitLLVMFn<'ctx, T> {
    pub function: JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> T>,
}

impl<T> ImplicitFn<T> for ImplicitLLVMFn<'_, T> {
    fn call_implicit(&self, x: f64, y: f64) -> T {
        unsafe { self.function.call(x, y) }
    }
}

pub struct ExplicitLLVMListFn<'ctx> {
    pub function: JitFunction<'ctx, unsafe extern "C" fn(f64) -> ListLayout>,
}

impl<T: Clone> ExplicitFn<Vec<T>> for ExplicitLLVMListFn<'_> {
    fn call(&self, x: f64) -> Vec<T> {
        let list_layout = unsafe { self.function.call(x) };
        convert_list(&list_layout)
    }
}

pub struct ImplicitLLVMListFn<'ctx> {
    pub function: JitFunction<'ctx, unsafe extern "C" fn(f64, f64) -> ListLayout>,
}

impl<T: Clone> ImplicitFn<Vec<T>> for ImplicitLLVMListFn<'_> {
    fn call_implicit(&self, x: f64, y: f64) -> Vec<T> {
        let list_layout = unsafe { self.function.call(x, y) };
        convert_list(&list_layout)
    }
}

pub fn convert_list<T: Clone>(list_layout: &ListLayout) -> Vec<T> {
    unsafe {
        if list_layout.ptr.is_null() {
            return Vec::new();
        }

        // Compute the number of elements in the list
        let element_count = list_layout.size as usize;

        // Convert the raw pointer into a slice
        let slice = std::slice::from_raw_parts(list_layout.ptr as *const T, element_count);

        // Clone the slice into a Vec to create a safe wrapper
        slice.to_vec()
    }
}
