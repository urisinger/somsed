use std::collections::HashMap;

use anyhow::Result;
use builder::CraneliftBuilder;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncOrDataId, Linkage, Module};
use functions::{import_symbols, ImportedFunctions};

use crate::lang::codegen::ir::{IRModule, IRScalerType, IRType, SegmentKey};

pub mod builder;
pub mod jit;
pub mod value;

mod functions;

pub struct CraneliftBackend {
    module: JITModule,
    functions: ImportedFunctions,
}

impl CraneliftBackend {
    pub fn new() -> Result<Self> {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;

        import_symbols(&mut builder);

        let mut module = JITModule::new(builder);

        let functions = ImportedFunctions::new(&mut module)?;

        Ok(Self { module, functions })
    }

    pub fn make_sig(&self, key: &SegmentKey, ret_type: IRType) -> Signature {
        let mut signature = self.module.make_signature();

        use IRScalerType::*;
        use IRType::*;
        for ty in &key.args {
            match ty {
                Scaler(Number) => {
                    signature.params.push(AbiParam::new(types::F64));
                }
                Scaler(Point) => {
                    signature.params.push(AbiParam::new(types::F64));
                    signature.params.push(AbiParam::new(types::F64));
                }
                List(_) => {
                    signature.params.push(AbiParam::new(types::I64));
                    signature.params.push(AbiParam::new(types::I64));
                }
            };
        }

        match ret_type {
            Scaler(Number) => {
                signature.returns.push(AbiParam::new(types::F64));
            }
            Scaler(Point) => {
                signature.returns.push(AbiParam::new(types::F64));
                signature.returns.push(AbiParam::new(types::F64));
            }
            List(_) => {
                signature.returns.push(AbiParam::new(types::I64));
                signature.returns.push(AbiParam::new(types::I64));
            }
        }
        signature
    }

    pub fn compile_module(&mut self, ir_module: &IRModule) -> Result<()> {
        // we must do everything in 2 passes
        for (key, segment) in ir_module.iter_segments() {
            let signature =
                self.make_sig(key, segment.ret().expect("segment cannot be empty").ty());
            let name = key.to_string();
            _ = self
                .module
                .declare_function(&name, Linkage::Export, &signature)
                .expect("failed to declare function, this indicates a bug");
        }

        let mut ctx = codegen::Context::new();
        let mut builder_ctx = FunctionBuilderContext::new();

        for (key, segment) in ir_module.iter_segments() {
            let name = key.to_string();
            let func_id = if let FuncOrDataId::Func(id) = self
                .module
                .get_name(&name)
                .expect("Function should have been generated")
            {
                id
            } else {
                panic!("Entry should not be data")
            };

            ctx.func.signature =
                self.make_sig(key, segment.ret().expect("segment cannot be empty").ty());

            let builder = CraneliftBuilder::new(
                self,
                ir_module,
                FunctionBuilder::new(&mut ctx.func, &mut builder_ctx),
                &key.args,
            );

            builder.build_fn(segment)?;
            println!("{}", &ctx.func.display());
            self.module.define_function(func_id, &mut ctx)?;
            self.module.clear_context(&mut ctx);
        }

        self.module
            .finalize_definitions()
            .expect("failed to finilize module");

        Ok(())
    }
}
