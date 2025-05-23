use anyhow::{bail, Context, Result};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue},
    IntPredicate,
};

use desmos_compiler::lang::generic_value::{
    GenericList, GenericScalerValue, GenericValue, ListType, ValueType,
};

use crate::value::LLVMValue;

use super::LLVMBuilder;

impl<'ctx> LLVMBuilder<'ctx, '_> {
    pub fn build_new_list<T: BasicValue<'ctx>>(
        &self,
        elements: &[T],
        ty: ValueType,
    ) -> Result<StructValue<'ctx>> {
        let size = self
            .backend
            .i64_type
            .const_int(elements.len() as u64, false);

        let pointer = match ty {
            GenericValue::Number(_) => {
                self.codegen_allocate(size, self.backend.number_type.as_basic_type_enum())?
            }
            GenericValue::Point(_) => {
                self.codegen_allocate(size, self.backend.point_type.as_basic_type_enum())?
            }
            GenericValue::List(_) => bail!("Cant craete lists of lists"),
        };

        // Initialize the struct with size and pointer
        let list_value = self
            .backend
            .list_type
            .const_named_struct(&[size.as_basic_value_enum(), pointer.as_basic_value_enum()]); // Start with a zeroed struct

        for (i, value) in elements.iter().enumerate() {
            let value = value.as_basic_value_enum();
            let element_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    value.get_type(),
                    pointer,
                    &[self.backend.i64_type.const_int(i as u64, false)],
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
                self.codegen_free_list(struct_value, self.backend.number_type.as_basic_type_enum())
            }
            GenericList::Point(struct_value) => {
                self.codegen_free_list(struct_value, self.backend.point_type.as_basic_type_enum())
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
        &mut self,
        list: &GenericList<StructValue<'ctx>, StructValue<'ctx>>,
        output_ty: ListType,
        transform: impl Fn(
            &mut Self,
            GenericList<FloatValue<'ctx>, StructValue<'ctx>>,
        ) -> GenericList<FloatValue<'ctx>, StructValue<'ctx>>,
    ) -> Result<GenericList<StructValue<'ctx>, StructValue<'ctx>>> {
        let size_field_index = 0;
        let pointer_field_index = 1;

        let (input_struct, input_llvm_type) = match list {
            GenericList::Number(s) => (s, self.backend.number_type.as_basic_type_enum()),
            GenericList::Point(s) => (s, self.backend.point_type.as_basic_type_enum()),
        };

        let size = self
            .builder
            .build_extract_value(*input_struct, size_field_index, "size")
            .context("Failed to extract size as int")?
            .into_int_value(); // Now extract as into_pointer_value

        let input_pointer = self
            .builder
            .build_extract_value(*input_struct, pointer_field_index, "pointer")
            .context("Failed to get pointer field")?
            .into_pointer_value();

        let output_llvm_type = match output_ty {
            GenericList::Number(_) => self.backend.number_type.as_basic_type_enum(),
            GenericList::Point(_) => self.backend.point_type.as_basic_type_enum(),
        };

        let output_pointer = self.codegen_allocate(size, input_llvm_type)?;
        let output_struct = self.create_list(size, output_pointer);

        // 2) Prepare blocks for the loop
        let current_fn = self.function;

        // Create separate blocks for loop header and loop body
        let loop_header = self
            .backend
            .context
            .append_basic_block(current_fn, "loop_header");
        let loop_body = self
            .backend
            .context
            .append_basic_block(current_fn, "loop_body");
        let end_block = self.backend.context.append_basic_block(current_fn, "end");

        let index_alloca = self.builder.build_alloca(self.backend.i64_type, "index")?;
        self.builder
            .build_store(index_alloca, self.backend.i64_type.const_int(0, false))?;

        // Initial branch to the loop header
        self.builder.build_unconditional_branch(loop_header)?;

        // Loop Header: Check the loop condition
        self.builder.position_at_end(loop_header);
        let current_index_val = self
            .builder
            .build_load(self.backend.i64_type, index_alloca, "current_index")?
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
            (BasicValueEnum::StructValue(s), GenericList::Point(_)) => GenericList::Point(s),
            _ => unreachable!("Something went wrong with the types, should not happen"),
        };

        let transformed_value = transform(self, loaded_value);

        // Compute pointer for storing transformed value
        let store_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                output_llvm_type,
                output_pointer,
                &[current_index_val],
                "store_ptr",
            )?
        };

        self.builder.build_store(
            store_ptr,
            match transformed_value {
                GenericList::Number(n) => n.as_basic_value_enum(),
                GenericList::Point(p) => p.as_basic_value_enum(),
            },
        )?;

        // Increment index
        let next_index = self.builder.build_int_add(
            current_index_val,
            self.backend.i64_type.const_int(1, false),
            "next_index",
        )?;
        self.builder.build_store(index_alloca, next_index)?;

        // Branch back to loop header
        self.builder.build_unconditional_branch(loop_header)?;

        // End block: continue here when loop is finished
        self.builder.position_at_end(end_block);
        let output = match list {
            GenericList::Number(_) => GenericList::Number(output_struct),
            GenericList::Point(_) => GenericList::Point(output_struct),
        };
        Ok(output)
    }

    fn create_list(&self, size: IntValue<'ctx>, pointer: PointerValue<'ctx>) -> StructValue<'ctx> {
        let list_value = self.backend.list_type.get_undef();

        let list_value = self
            .builder
            .build_insert_value(list_value, size, 0, "list_value")
            .expect("build in invalid state")
            .into_struct_value();

        let list_value = self
            .builder
            .build_insert_value(list_value, pointer, 1, "list_value")
            .expect("build in invalid state")
            .into_struct_value();

        list_value
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
            .backend
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
        let pointer: PointerValue<'ctx> = self
            .builder
            .build_extract_value(struct_value, pointer_field_index, "pointer")
            .expect("Failed to get pointer field")
            .into_pointer_value();

        // Extract size field
        let size_value = self
            .builder
            .build_extract_value(struct_value, size_field_index, "size")
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
            .backend
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
