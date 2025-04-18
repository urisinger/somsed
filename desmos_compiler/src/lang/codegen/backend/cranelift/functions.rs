use std::alloc::{alloc, dealloc, Layout};

use anyhow::Result;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

#[macro_export]
macro_rules! cranelift_sig {
    ($module:expr, fn() -> $ret:ident) => {{
        let module = $module;
        let mut sig = module.make_signature();
        sig.returns.push(cranelift::codegen::ir::AbiParam::new(cranelift::codegen::ir::types::$ret));
        sig
    }};

    ($module:expr, fn($($param:ident),*) -> $ret:ident) => {{
        let module = $module;
        let mut sig = module.make_signature();
        $(
            sig.params.push(cranelift::codegen::ir::AbiParam::new(cranelift::codegen::ir::types::$param));
        )*
        sig.returns.push(cranelift::codegen::ir::AbiParam::new(cranelift::codegen::ir::types::$ret));
        sig
    }};
}
#[derive(Debug, Clone, Copy)]
pub struct ImportedFunctions {
    pub free_id: FuncId,
    pub malloc_id: FuncId,
    pub pow_id: FuncId,
    pub cos_id: FuncId,
    pub sin_id: FuncId,
    pub tan_id: FuncId,
}

pub fn import_symbols(builder: &mut JITBuilder) {
    builder.symbol("pow", pow as *const u8);
    builder.symbol("sin", sin as *const u8);
    builder.symbol("cos", cos as *const u8);
    builder.symbol("tan", tan as *const u8);
    builder.symbol("malloc", malloc as *const u8);
    builder.symbol("free", free as *const u8);
}

impl ImportedFunctions {
    pub fn new(module: &mut JITModule) -> Result<Self> {
        // malloc: fn(i64) -> ptr
        let mut malloc_sig = module.make_signature();
        malloc_sig.params.push(AbiParam::new(types::I64));
        malloc_sig
            .returns
            .push(AbiParam::new(module.target_config().pointer_type()));
        let malloc_id = module.declare_function("malloc", Linkage::Import, &malloc_sig)?;

        // free: fn(ptr, i64)
        let mut free_sig = module.make_signature();
        free_sig
            .params
            .push(AbiParam::new(module.target_config().pointer_type()));
        free_sig.params.push(AbiParam::new(types::I64));
        let free_id = module.declare_function("free", Linkage::Import, &free_sig)?;

        // pow: fn(f64, f64) -> f64
        let pow_sig = cranelift_sig!(&module, fn(F64, F64) -> F64);
        let pow_id = module.declare_function("pow", Linkage::Import, &pow_sig)?;

        // trig: fn(f64) -> f64
        let trig_sig = cranelift_sig!(&module, fn(F64) -> F64);
        let sin_id = module.declare_function("sin", Linkage::Import, &trig_sig)?;
        let cos_id = module.declare_function("cos", Linkage::Import, &trig_sig)?;
        let tan_id = module.declare_function("tan", Linkage::Import, &trig_sig)?;

        Ok(Self {
            malloc_id,
            free_id,
            pow_id,
            sin_id,
            cos_id,
            tan_id,
        })
    }
}

/// Allocate memory for `size` bytes and return a pointer to the allocated memory.
/// ## Safety
/// This is marked unsafe becuase it allocates a raw pointer
pub unsafe extern "C" fn malloc(size: usize) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut(); // Return null if zero bytes are requested
    }

    // Create a layout for the requested size
    let layout = Layout::from_size_align(size, 8).expect("Invalid layout");
    // Allocate memory and return the pointer
    let ptr = alloc(layout);
    if ptr.is_null() {
        panic!("Memory allocation failed");
    }
    ptr
}

/// Deallocate memory for the given pointer and size.
/// ## Safety
/// This is marked unsafe becuase it frees a raw pointer
pub unsafe extern "C" fn free(ptr: *mut u8, size: usize) {
    if !ptr.is_null() {
        // Create a layout for the size to free
        let layout = Layout::from_size_align(size, 8).expect("Invalid layout");
        dealloc(ptr, layout);
    }
}

pub extern "C" fn pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

pub extern "C" fn sin(x: f64) -> f64 {
    x.sin()
}

pub extern "C" fn cos(x: f64) -> f64 {
    x.cos()
}

pub extern "C" fn tan(x: f64) -> f64 {
    x.tan()
}
