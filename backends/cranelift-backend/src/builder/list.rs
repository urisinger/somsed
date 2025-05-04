use anyhow::{anyhow, bail, Context, Result};
use cranelift::{codegen::ir::StackSlot, prelude::*};
use cranelift_module::Module;
use desmos_compiler::lang::codegen::ir::{IRScalerType, IRType};

use crate::value::{CraneliftList, CraneliftScaler, CraneliftValue};

use super::CraneliftBuilder;

impl CraneliftBuilder<'_, '_> {
    pub fn get_element_size(ty: IRType) -> Result<usize> {
        match ty {
            IRType::Scaler(IRScalerType::Number) => Ok(8),
            IRType::Scaler(IRScalerType::Point) => Ok(16),
            IRType::List(_) => bail!("Nested lists are not supported"),
        }
    }

    pub fn build_new_list(
        &mut self,
        elements: &[CraneliftValue],
        ty: IRType,
    ) -> Result<[Value; 2]> {
        let element_size = Self::get_element_size(ty)?;

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

    pub fn codegen_list_map(
        &mut self,
        lists: &[Vec<CraneliftList>],
        output_ty: IRScalerType,
        transform: impl Fn(&mut Self, &[CraneliftScaler]) -> Result<CraneliftScaler>,
    ) -> Result<CraneliftList> {
        let index_type = types::I64;

        let output_size = match output_ty {
            IRScalerType::Number => 8,
            IRScalerType::Point => 16,
        };
        let output_size_val = self.builder.ins().iconst(index_type, output_size);

        let mut total_size: Option<Value> = None;

        for group in lists {
            let mut group_size: Option<Value> = None;

            for list in group {
                let size = match list {
                    CraneliftList::Number([size, _]) => *size,
                    CraneliftList::Point([size, _]) => *size,
                };

                group_size = Some(match group_size {
                    None => size,
                    Some(acc) => {
                        let lt = self.builder.ins().icmp(IntCC::SignedLessThan, acc, size);
                        self.builder.ins().select(lt, acc, size)
                    }
                });
            }

            let group_size = group_size.context("empty list group")?;

            total_size = Some(match total_size {
                None => group_size,
                Some(acc) => self.builder.ins().imul(acc, group_size),
            });
        }

        let total_size = total_size.context("no list groups")?;
        let total_bytes = self.builder.ins().imul(total_size, output_size_val);
        let output_ptr = self.codegen_allocate(total_bytes);
        let output_struct = [total_size, output_ptr];

        self.codegen_list_map_recursive(
            lists,
            0,
            &[],
            &[],
            output_ty,
            output_size_val,
            output_ptr,
            &transform,
        )?;

        Ok(match output_ty {
            IRScalerType::Number => CraneliftList::Number(output_struct),
            IRScalerType::Point => CraneliftList::Point(output_struct),
        })
    }

    fn codegen_list_map_recursive(
        &mut self,
        lists: &[Vec<CraneliftList>],
        depth: usize,
        index_slots: &[StackSlot],
        index_vals: &[Value],
        output_ty: IRScalerType,
        output_size_val: Value,
        output_ptr: Value,
        transform: &impl Fn(&mut Self, &[CraneliftScaler]) -> Result<CraneliftScaler>,
    ) -> Result<()> {
        let index_type = types::I64;

        let index_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
            3,
        ));
        let zero = self.builder.ins().iconst(index_type, 0);
        self.builder.ins().stack_store(zero, index_slot, 0);

        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let exit = self.builder.create_block();

        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);

        let index = self.builder.ins().stack_load(index_type, index_slot, 0);

        let group = &lists[depth];
        let group_size = group
            .iter()
            .map(|list| match list {
                CraneliftList::Number([s, _]) => *s,
                CraneliftList::Point([s, _]) => *s,
            })
            .reduce(|a, b| {
                let lt = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
                self.builder.ins().select(lt, a, b)
            })
            .unwrap();

        let cond = self
            .builder
            .ins()
            .icmp(IntCC::UnsignedLessThan, index, group_size);
        self.builder.ins().brif(cond, body, &[], exit, &[]);

        self.builder.switch_to_block(body);

        let mut index_slots = index_slots.to_vec();
        let mut index_vals = index_vals.to_vec();
        index_slots.push(index_slot);
        index_vals.push(index);

        if depth + 1 == lists.len() {
            let mut all_scalars = Vec::new();
            for (g, &idx) in lists.iter().zip(&index_vals) {
                for list in g {
                    let (ptr, ty) = match list {
                        CraneliftList::Number([_, ptr]) => (*ptr, IRScalerType::Number),
                        CraneliftList::Point([_, ptr]) => (*ptr, IRScalerType::Point),
                    };
                    let field_size = match ty {
                        IRScalerType::Number => 8,
                        IRScalerType::Point => 16,
                    };
                    let field_size_val = self.builder.ins().iconst(index_type, field_size);
                    let offset = self.builder.ins().imul(idx, field_size_val);
                    let addr = self.builder.ins().iadd(ptr, offset);

                    let scalar = match ty {
                        IRScalerType::Number => {
                            let val = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 0);
                            CraneliftScaler::Number([val])
                        }
                        IRScalerType::Point => {
                            let x = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 0);
                            let y = self
                                .builder
                                .ins()
                                .load(types::F64, MemFlags::new(), addr, 8);
                            CraneliftScaler::Point([x, y])
                        }
                    };
                    all_scalars.push(scalar);
                }
            }

            let result = transform(self, &all_scalars)?;
            let last_index = *index_vals.last().unwrap();
            let offset = self.builder.ins().imul(last_index, output_size_val);
            let ptr = self.builder.ins().iadd(output_ptr, offset);

            let values = match result {
                CraneliftScaler::Number([x]) => vec![x],
                CraneliftScaler::Point([x, y]) => vec![x, y],
            };

            for (i, val) in values.into_iter().enumerate() {
                self.builder
                    .ins()
                    .store(MemFlags::new(), val, ptr, (i * 8) as i32);
            }
        } else {
            self.codegen_list_map_recursive(
                lists,
                depth + 1,
                &index_slots,
                &index_vals,
                output_ty,
                output_size_val,
                output_ptr,
                transform,
            )?;
        }

        let one = self.builder.ins().iconst(index_type, 1);
        let next = self.builder.ins().iadd(index, one);
        self.builder.ins().stack_store(next, index_slot, 0);
        self.builder.ins().jump(header, &[]);

        self.builder.switch_to_block(exit);
        self.builder.seal_block(header);
        self.builder.seal_block(body);
        self.builder.seal_block(exit);

        Ok(())
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
