use anyhow::{anyhow, bail, Context, Result};
use cranelift::prelude::*;
use cranelift_module::Module;
use desmos_compiler::lang::codegen::ir::{IRScalerType, IRType};

use crate::value::{CraneliftList, CraneliftScaler, CraneliftValue};

use super::CraneliftBuilder;

impl CraneliftBuilder<'_, '_> {
    pub fn build_new_list(
        &mut self,
        elements: &[CraneliftValue],
        ty: IRType,
    ) -> Result<[Value; 2]> {
        let element_size = match ty {
            IRType::Scaler(IRScalerType::Number) => 8,
            IRType::Scaler(IRScalerType::Point) => 16,
            IRType::List(_) => bail!("list cannot be made of lists"),
        };

        let total_size_bytes = (elements.len() * element_size) as i64;

        // Allocate space
        let size_val = self.builder.ins().iconst(types::I64, total_size_bytes);
        let base_ptr = self.codegen_allocate(size_val); // assume returns a `Value` of pointer type

        // Store each element at the correct offset
        for (i, field_values) in elements.iter().enumerate() {
            let offset = (i * element_size) as i32;

            for (j, field) in field_values.as_struct().iter().enumerate() {
                let field_offset = offset + (j * 8) as i32;

                self.builder
                    .ins()
                    .store(MemFlags::new(), *field, base_ptr, field_offset);
            }
        }

        Ok([
            self.builder.ins().iconst(types::I64, elements.len() as i64), // list size (element count)
            base_ptr, // pointer to allocated memory
        ])
    }

    pub fn codegen_free(&mut self, list: CraneliftList) -> Result<()> {
        match list {
            CraneliftList::Number(struct_value) => self.codegen_free_list(&struct_value, 8),
            CraneliftList::Point(struct_value) => self.codegen_free_list(&struct_value, 16),
        }
    }

    /// In-place transformation of each element in a "list struct".
    ///
    /// - `list_struct`: A struct containing [ size (i64 or i32), pointer (T*) ].
    /// - `element_type`: The LLVM type of each element (e.g. `context.f64_type().into()`).
    /// - `transform`: A closure that takes a loaded element and returns a new element.
    ///
    /// Returns the same `StructValue<'ctx>`, after in-place modification.
    pub fn codegen_list_map(
        &mut self,
        lists: &[CraneliftList],
        output_ty: IRScalerType,
        transform: impl Fn(&mut Self, &[CraneliftScaler]) -> Result<CraneliftScaler>,
    ) -> Result<CraneliftList> {
        let index_type = types::I64;

        let size = lists
            .iter()
            .map(|list| match list {
                CraneliftList::Number(s) => s[0],
                CraneliftList::Point(s) => s[0],
            })
            .fold(None, |last_size, s| {
                if let Some(last_size) = last_size {
                    let lt = self.builder.ins().icmp(IntCC::SignedLessThan, last_size, s);
                    Some(self.builder.ins().select(lt, last_size, s))
                } else {
                    Some(s)
                }
            })
            .with_context(|| anyhow!("list cannot be empty"))?;

        let output_size = self.builder.ins().iconst(
            types::I64,
            match output_ty {
                IRScalerType::Number => 8,
                IRScalerType::Point => 16,
            },
        );

        let output_byte_size = self.builder.ins().imul(size, output_size);

        let output_pointer = self.codegen_allocate(output_byte_size);
        let output_struct = [size, output_pointer];

        let index_var = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
            3,
        ));
        let zero = self.builder.ins().iconst(index_type, 0);
        self.builder.ins().stack_store(zero, index_var, 0);

        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let current_index = self.builder.ins().stack_load(index_type, index_var, 0);

        let condition = self
            .builder
            .ins()
            .icmp(IntCC::UnsignedLessThan, current_index, size);
        self.builder
            .ins()
            .brif(condition, body_block, &[], exit_block, &[]);

        self.builder.switch_to_block(body_block);

        let args: Vec<_> = lists
            .iter()
            .map(|list| {
                let ([_, input_ptr], element_size) = match list {
                    CraneliftList::Number(s) => (s, 8),
                    CraneliftList::Point(s) => (s, 16),
                };

                let elem_size_val = self.builder.ins().iconst(index_type, element_size);

                let offset = self.builder.ins().imul(current_index, elem_size_val);

                // input_ptr + offset
                let element_ptr = self.builder.ins().iadd(*input_ptr, offset);

                // load element fields (based on type)
                match list {
                    CraneliftList::Number(_) => {
                        let value =
                            self.builder
                                .ins()
                                .load(types::F64, MemFlags::new(), element_ptr, 0);
                        CraneliftScaler::Number([value])
                    }
                    CraneliftList::Point(_) => {
                        let x =
                            self.builder
                                .ins()
                                .load(types::F64, MemFlags::new(), element_ptr, 0);
                        let y =
                            self.builder
                                .ins()
                                .load(types::F64, MemFlags::new(), element_ptr, 8);
                        CraneliftScaler::Point([x, y])
                    }
                }
            })
            .collect();

        // apply transformation (returns CraneliftValue)
        let transformed = transform(self, &args)?;

        let transformed_values: &[Value] = match &transformed {
            CraneliftScaler::Number(x) => x,
            CraneliftScaler::Point(x) => x,
        };

        let offset = self.builder.ins().imul(current_index, output_size);
        // compute output offset: output_ptr + offset
        let output_ptr = self.builder.ins().iadd(output_pointer, offset);

        // store fields
        for (i, field) in transformed_values.iter().enumerate() {
            let field_offset = (i * 8) as i32;
            self.builder
                .ins()
                .store(MemFlags::new(), *field, output_ptr, field_offset);
        }

        // Increment index
        let one = self.builder.ins().iconst(index_type, 1);
        let next_index = self.builder.ins().iadd(current_index, one);
        self.builder.ins().stack_store(next_index, index_var, 0);

        // Jump back to loop header
        self.builder.ins().jump(header_block, &[]); // Branch back to loop header

        self.builder.switch_to_block(exit_block);

        self.builder.seal_block(header_block);
        self.builder.seal_block(body_block);
        self.builder.seal_block(exit_block);

        let output = match output_ty {
            IRScalerType::Number => CraneliftList::Number(output_struct),
            IRScalerType::Point => CraneliftList::Point(output_struct),
        };

        Ok(output)
    }

    fn codegen_allocate(&mut self, size: Value) -> Value {
        // Call malloc to allocate memory
        let malloc_fn = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.malloc_id, self.builder.func);

        let raw_ptr = self.builder.ins().call(malloc_fn, &[size]);

        self.builder.inst_results(raw_ptr)[0]
    }

    fn codegen_free_list(&mut self, struct_value: &[Value; 2], size: usize) -> Result<()> {
        // Compute total size: `total_size = size * size_of::<T>()`
        let element_size = self.builder.ins().iconst(types::I64, size as i64);
        let total_size = self.builder.ins().imul(struct_value[0], element_size);

        // Call the `free` function
        let free_fn = self
            .backend
            .module
            .declare_func_in_func(self.backend.functions.free_id, self.builder.func);

        self.builder
            .ins()
            .call(free_fn, &[struct_value[1], total_size]);

        Ok(())
    }
}
