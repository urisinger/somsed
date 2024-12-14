use anyhow::{Context, Result};
use inkwell::values::{BasicValue, FloatValue, IntValue, PointerValue};

use crate::lang::backends::llvm::value::{List, Value};

use super::CodeGen;

impl<'ctx, 'expr> CodeGen<'ctx, 'expr> {
    pub fn codegen_allocate(&self, size: IntValue<'ctx>) -> Result<PointerValue<'ctx>> {
        // Assuming you're using LLVM's `malloc` to allocate memory
        let i64_type = self.context.i64_type();
        let element_size = i64_type.const_int(8, false); // Assuming 8 bytes per element (for f64)
        let total_size = self
            .builder
            .build_int_mul(size, element_size, "total_size")?;

        // Call malloc to allocate memory
        let malloc_fn = self
            .module
            .get_function("malloc")
            .expect("malloc should be defined"); // Assuming `malloc` is defined
                                                 //
                                                 //
        let raw_ptr = self
            .builder
            .build_call(malloc_fn, &[total_size.into()], "malloc_call")?
            .try_as_basic_value()
            .left()
            .expect("return type should not be void")
            .into_pointer_value();

        Ok(raw_ptr)
    }

    pub fn codegen_free(&self, list: List<'ctx>) -> Result<()> {
        match list {
            List::Number(struct_value) => {
                // Assuming the pointer is stored as the first field of the struct
                let pointer_field_index = 1; // Change this if the pointer is at a different index
                let pointer: PointerValue<'ctx> = struct_value
                    .get_field_at_index(pointer_field_index)
                    .expect("Failed to get pointer field")
                    .into_pointer_value();

                // Get the size of the array (assuming the size is stored as the first field)
                let size_field_index = 0; // Change this if the size is at a different index
                let size_value = struct_value
                    .get_field_at_index(size_field_index)
                    .expect("Failed to get size field");

                // Create an integer type for the size
                let int_type = self.context.i64_type(); // Use i32 or i64 as needed

                // Convert size_value to IntValue
                let size_int = size_value.into_int_value();

                // Calculate the total size dynamically: total_size = size * size_of::<f32>()
                let float_size = int_type.const_int(size_of::<f64>() as u64, false); // Use f64 if necessary
                let total_size = self
                    .builder
                    .build_int_mul(size_int, float_size, "total_size")?;

                // Call the free function
                let free_fn = self
                    .module
                    .get_function("free")
                    .expect("Free function not found");
                self.builder.build_call(
                    free_fn,
                    &[pointer.into(), total_size.into()],
                    "free_call",
                )?;

                Ok(())
            }
        }
    }

    pub fn codegen_list_new(&self, size: IntValue<'ctx>) -> Result<Value<'ctx>> {
        // Allocate memory for the list
        let pointer = self.codegen_allocate(size)?;

        // Create a struct representing the list
        // Assuming the struct contains the size (i32) and the pointer (f64*)
        let list_type = self.context.struct_type(
            &[self.context.i32_type().into(), pointer.get_type().into()],
            false,
        );

        // Initialize the struct with size and pointer
        let mut list_value = list_type.const_zero(); // Start with a zeroed struct
        list_value = self
            .builder
            .build_insert_value(list_value, size, 0, "list_size")
            .expect("failed to inizlize struct")
            .into_struct_value();
        list_value = self
            .builder
            .build_insert_value(list_value, pointer, 1, "list_ptr")
            .expect("failed to initlize struct")
            .into_struct_value();

        // Return the new list as Value::List
        Ok(Value::List(List::Number(list_value)))
    }

    pub fn codegen_list_loop<F>(&self, lhs: List<'ctx>, func: F) -> Result<Value<'ctx>>
    where
        F: FnOnce(FloatValue<'ctx>) -> Result<FloatValue<'ctx>>,
    {
        let size_field_index = 0;
        let pointer_field_index = 1;
        match lhs {
            List::Number(lhs) => {
                // Get the size of the list
                let size_value = lhs
                    .get_field_at_index(size_field_index)
                    .context("Failed to get size field")?;
                let size: IntValue<'ctx> = size_value.into_int_value();

                // Get the pointer to the list elements
                let pointer_value: PointerValue<'ctx> = lhs
                    .get_field_at_index(pointer_field_index)
                    .context("Failed to get pointer field")?
                    .into_pointer_value();

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                // Create a loop that iterates through the elements
                let entry_block = self.context.append_basic_block(current_fn, "entry");
                let loop_block = self.context.append_basic_block(current_fn, "loop");
                let end_block = self.context.append_basic_block(current_fn, "end");

                self.builder.position_at_end(entry_block);
                let index = self
                    .builder
                    .build_alloca(self.context.i64_type(), "index")?;
                self.builder
                    .build_store(index, self.context.i64_type().const_int(0, false))?;

                // Start the loop
                self.builder.build_unconditional_branch(loop_block)?;

                self.builder.position_at_end(loop_block);
                // Load index value
                let current_index =
                    self.builder
                        .build_load(self.context.i64_type(), index, "current_index")?;
                let current_index_value = current_index.into_int_value();

                // Check if we are still within bounds
                let condition = self.builder.build_int_compare(
                    inkwell::IntPredicate::ULT,
                    current_index_value,
                    size,
                    "condition",
                )?;

                // If condition fails, jump to end
                self.builder
                    .build_conditional_branch(condition, loop_block, end_block)?;

                // Calculate the pointer for the current element
                let element_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        self.context.f64_type(),
                        pointer_value,
                        &[current_index_value],
                        "element_ptr",
                    )?
                };

                // Load the current element value
                let current_value = self.builder.build_load(
                    self.context.f64_type(),
                    element_ptr,
                    "current_value",
                )?;

                // Create a `Number` from the current value
                let lhs_value = current_value.into_float_value();

                // Call the provided function on the current value
                let new_value = func(lhs_value); // Invoke the function

                // Store the new value back in the list
                self.builder
                    .build_store(element_ptr, new_value?.as_basic_value_enum())?;

                // Increment the index
                let incremented_index = self.builder.build_int_add(
                    current_index_value,
                    self.context.i64_type().const_int(1, false),
                    "incremented_index",
                )?;
                self.builder.build_store(index, incremented_index)?;

                // Repeat the loop
                self.builder.build_unconditional_branch(loop_block)?;

                // End block
                self.builder.position_at_end(end_block);
                // Return the modified list
                Ok(Value::List(List::Number(lhs)))
            }
        }
    }
}
