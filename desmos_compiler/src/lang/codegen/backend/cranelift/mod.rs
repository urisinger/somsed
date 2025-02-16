use cranelift_jit::JITModule;
use cranelift_module::{FuncId, FuncOrDataId, Module};

use super::Backend;

pub mod builder;
pub mod jit;
pub mod value;

pub struct CraneliftBackend {
    module: JITModule,
}

impl Backend for CraneliftBackend {
    type FnValue = FuncId;
    fn get_fn(&self, name: &str) -> Option<Self::FnValue> {
        if let Some(FuncOrDataId::Func(id)) = self.module.get_name(name) {
            Some(id)
        } else {
            None
        }
    }
}
