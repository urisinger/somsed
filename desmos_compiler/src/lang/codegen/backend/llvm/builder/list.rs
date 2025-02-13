use anyhow::{bail, Context, Result};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue},
    IntPredicate,
};

use crate::lang::generic_value::{GenericList, GenericValue, ListType, ValueType};

use super::LLVMBuilder;

impl<'ctx> LLVMBuilder<'_, 'ctx> {
    pub fn build_new_list<T: BasicValue<'ctx>>(
        &self,
        elements: &[T],
        ty: ValueType,
    ) -> Result<StructValue<'ctx>> {
        let size = self.i64_type.const_int(elements.len() as u64, false);

        let pointer = match ty {
            GenericValue::Number(_) => {
                self.codegen_allocate(size, self.number_type.as_basic_type_enum())?
            }
            GenericValue::Point(_) => {
                self.codegen_allocate(size, self.point_type.as_basic_type_enum())?
            }
            GenericValue::List(_) => bail!("Cant craete lists of lists"),
        };

        // Initialize the struct with size and pointer
        let list_value = self
            .list_type
            .const_named_struct(&[size.as_basic_value_enum(), pointer.as_basic_value_enum()]); // Start with a zeroed struct

        for (i, value) in elements.iter().enumerate() {
            let value = value.as_basic_value_enum();
            let element_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    value.get_type(),
                    pointer,
                    &[self.i64_type.const_int(i as u64, false)],
                    &format!("element_ptr_{}", i),
                )
            };
            self.builder.build_store(element_ptr?, value)?;
        }

        Ok(list_value)
    }

    pub fn codegen_free(
        &self,
        list: GenericList<StructValue<'ctx>, StructValue<'ctx>>,
    ) -> Result<()> {
        match list {
            GenericList::Number(struct_value) => {
                self.codegen_free_list(struct_value, self.number_type.as_basic_type_enum())
            }
            GenericList::PointList(struct_value) => {
                self.codegen_free_list(struct_value, self.point_type.as_basic_type_enum())
            }
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
        &self,
        list: &GenericList<StructValue<'ctx>, StructValue<'ctx>>,
        output_ty: ListType,
        transform: impl Fn(
            GenericList<FloatValue<'ctx>, StructValue<'ctx>>,
        ) -> GenericList<FloatValue<'ctx>, StructValue<'ctx>>,
    ) -> Result<GenericList<StructValue<'ctx>, StructValue<'ctx>>> {
        let size_field_index = 0;
        let pointer_field_index = 1;

        let (input_struct, input_llvm_type) = match list {
            GenericList::Number(s) => (s, self.number_type.as_basic_type_enum()),
            GenericList::PointList(s) => (s, self.point_type.as_basic_type_enum()),
        };

        let size = input_struct
            .get_field_at_index(size_field_index)
            .context("Failed to extract size as int")?
            .into_int_value(); // Now extract as into_pointer_value

        let input_pointer = input_struct
            .get_field_at_index(pointer_field_index)
            .context("Failed to get size field")?
            .into_pointer_value();

        let output_llvm_type = match output_ty {
            GenericList::Number(_) => self.number_type.as_basic_type_enum(),
            GenericList::PointList(_) => self.point_type.as_basic_type_enum(),
        };

        let output_pointer = self.codegen_allocate(size, input_llvm_type)?;
        let output_struct = self.list_type.const_named_struct(&[
            size.as_basic_value_enum(),
            output_pointer.as_basic_value_enum(),
        ]); // Start with a zeroed struct

        // 2) Prepare blocks for the loop
        let current_fn = self.function;

        // Create separate blocks for loop header and loop body
        let loop_header = self.context.append_basic_block(current_fn, "loop_header");
        let loop_body = self.context.append_basic_block(current_fn, "loop_body");
        let end_block = self.context.append_basic_block(current_fn, "end");

        let index_alloca = self.builder.build_alloca(self.i64_type, "index")?;
        self.builder
            .build_store(index_alloca, self.i64_type.const_int(0, false))?;

        // Initial branch to the loop header
        self.builder.build_unconditional_branch(loop_header)?;

        // Loop Header: Check the loop condition
        self.builder.position_at_end(loop_header);
        let current_index_val = self
            .builder
            .build_load(self.i64_type, index_alloca, "current_index")?
            .into_int_value();
        let cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            current_index_val,
            size,
            "loop_cond",
        )?;
        self.builder
            .build_conditional_branch(cond, loop_body, end_block)?;

        // Loop Body: Execute loop operations
        self.builder.position_at_end(loop_body);

        // GEP to find the pointer of the current element
        let element_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                input_llvm_type,
                input_pointer,
                &[current_index_val],
                "element_ptr",
            )?
        };

        // Load the element and transform it
        let loaded_value = match (
            self.builder
                .build_load(input_llvm_type, element_ptr, "loaded_value")?,
            &list,
        ) {
            (BasicValueEnum::FloatValue(n), GenericList::Number(_)) => GenericList::Number(n),
            (BasicValueEnum::StructValue(s), GenericList::PointList(_)) => {
                GenericList::PointList(s)
            }
            _ => unreachable!("Something went wrong with the types, should not happen"),
        };

        let transformed_value = transform(loaded_value);

        // Compute pointer for storing transformed value
        let store_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                output_llvm_type,
                output_pointer,
                &[current_index_val],
                "store_ptr",
            )?
        };
        self.builder.build_store(store_ptr, transformed_value)?;

        // Increment index
        let next_index = self.builder.build_int_add(
            current_index_val,
            self.i64_type.const_int(1, false),
            "next_index",
        )?;
        self.builder.build_store(index_alloca, next_index)?;

        // Branch back to loop header
        self.builder.build_unconditional_branch(loop_header)?;

        // End block: continue here when loop is finished
        self.builder.position_at_end(end_block);
        let output = match list {
            GenericList::Number(_) => GenericList::Number(output_struct),
            GenericList::PointList(_) => GenericList::PointList(output_struct),
        };
        Ok(output)
    }

    fn codegen_allocate(
        &self,
        size: IntValue<'ctx>,
        t: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let element_size = t.size_of().context("List value must be sized")?; // Assuming 8 bytes per element (for f64)
        let total_size = self
            .builder
            .build_int_mul(size, element_size, "total_size")?;

        // Call malloc to allocate memory
        let malloc_fn = self
            .module
            .get_function("malloc")
            .expect("malloc should be defined"); // Assuming `malloc` is defined

        let element_align = if let BasicTypeEnum::StructType(t) = t {
            t.get_alignment()
        } else {
            element_size
        };

        let raw_ptr = self
            .builder
            .build_call(
                malloc_fn,
                &[total_size.into(), element_align.into()],
                "malloc_call",
            )?
            .try_as_basic_value()
            .left()
            .expect("return type should not be void")
            .into_pointer_value();

        Ok(raw_ptr)
    }

    fn codegen_free_list(
        &self,
        struct_value: StructValue<'ctx>,
        t: BasicTypeEnum<'ctx>,
    ) -> Result<()> {
        let pointer_field_index = 1; // Adjust if needed
        let size_field_index = 0; // Adjust if needed

        // Extract pointer field
        let pointer: PointerValue<'ctx> = struct_value
            .get_field_at_index(pointer_field_index)
            .expect("Failed to get pointer field")
            .into_pointer_value();

        // Extract size field
        let size_value = struct_value
            .get_field_at_index(size_field_index)
            .expect("Failed to get size field")
            .into_int_value();

        // Compute total size: `total_size = size * size_of::<T>()`
        let element_size = t.size_of().context("List value must be sized")?;
        let total_size = self
            .builder
            .build_int_mul(size_value, element_size, "total_size")?;

        let element_align = if let BasicTypeEnum::StructType(t) = t {
            t.get_alignment()
        } else {
            element_size
        };

        // Call the `free` function
        let free_fn = self
            .module
            .get_function("free")
            .expect("Free function not found");
        self.builder.build_call(
            free_fn,
            &[pointer.into(), total_size.into(), element_align.into()],
            "free_call",
        )?;

        Ok(())
    }
}
