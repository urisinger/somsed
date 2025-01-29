use anyhow::{bail, Context, Result};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue},
    IntPredicate,
};

use crate::lang::backends::llvm::{
    jit::PointLayout,
    types::{CompilerType, ListType},
    value::{CompilerList, CompilerValue},
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn codegen_list_new(&self, elements: Vec<CompilerValue>) -> Result<CompilerValue<'ctx>> {
        let i64_type = self.context.i64_type();
        let size = i64_type.const_int(elements.len() as u64, false);

        let t = if let Some(first) = elements.first() {
            match first.get_type() {
                CompilerType::Number(_) => ListType::Number(self.list_type),

                CompilerType::Point(_) => ListType::Point(self.list_type),
                t => bail!("Cannot construct list filled with: {t:?}"),
            }
        } else {
            ListType::Number(self.list_type)
        };
        // Allocate memory for the list
        let pointer = match t {
            ListType::Number(_) => {
                self.codegen_allocate(size, self.float_type.as_basic_type_enum())?
            }
            ListType::Point(_) => {
                self.codegen_allocate(size, self.point_type.as_basic_type_enum())?
            }
        };

        // Create a struct representing the list
        // Assuming the struct contains the size (i32) and the pointer (f64*)
        let list_type = t.get_type();

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

        for (i, value) in elements.iter().enumerate() {
            let value = match value {
                CompilerValue::Number(v) => v.as_basic_value_enum(),
                CompilerValue::Point(p) => p.as_basic_value_enum(),
                _ => bail!("List elements must be numbers"),
            };

            let element_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    value.get_type(),
                    pointer,
                    &[i64_type.const_int(i as u64, false)],
                    &format!("element_ptr_{}", i),
                )
            };
            self.builder.build_store(element_ptr?, value)?;
        }
        match t {
            ListType::Number(_) => Ok(CompilerValue::List(CompilerList::Number(list_value))),

            ListType::Point(_) => Ok(CompilerValue::List(CompilerList::Point(list_value))),
        }
    }

    pub fn codegen_allocate(
        &self,
        size: IntValue<'ctx>,
        t: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        println!("{t}, {:?}", t.size_of());
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

    pub fn codegen_free(&self, list: CompilerList<'ctx>) -> Result<()> {
        match list {
            CompilerList::Number(struct_value) => {
                self.codegen_free_list(struct_value, self.float_type.as_basic_type_enum())
            }
            CompilerList::Point(struct_value) => {
                self.codegen_free_list(struct_value, self.point_type.as_basic_type_enum())
            }
        }
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

    /// In-place transformation of each element in a "list struct".
    ///
    /// - `list_struct`: A struct containing [ size (i64 or i32), pointer (T*) ].
    /// - `element_type`: The LLVM type of each element (e.g. `context.f64_type().into()`).
    /// - `transform`: A closure that takes a loaded element and returns a new element.
    ///
    /// Returns the same `StructValue<'ctx>`, after in-place modification.
    pub fn codegen_list_map(
        &self,
        list_struct: StructValue<'ctx>,
        element_type: BasicTypeEnum<'ctx>,
        transform: impl Fn(BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>>,
    ) -> Result<StructValue<'ctx>> {
        let size_field_index = 0;
        let pointer_field_index = 1;

        // 1) Extract `size` (assume i64) and the pointer field
        let size_val = list_struct
            .get_field_at_index(size_field_index)
            .context("Failed to get size field")?
            .into_int_value();

        let pointer_val = list_struct
            .get_field_at_index(pointer_field_index)
            .context("Failed to get pointer field")?
            .into_pointer_value();

        // 2) Prepare blocks for the loop
        let current_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let entry_block = self.context.append_basic_block(current_fn, "entry");
        let loop_block = self.context.append_basic_block(current_fn, "loop");
        let end_block = self.context.append_basic_block(current_fn, "end");

        // 3) In the `entry` block, create and zero-initialize an `index`.
        self.builder.position_at_end(entry_block);

        let index_alloca = self
            .builder
            .build_alloca(self.context.i64_type(), "index")?;
        self.builder
            .build_store(index_alloca, self.context.i64_type().const_int(0, false))?;
        self.builder.build_unconditional_branch(loop_block)?;

        // 4) In the `loop` block, load the current index and check bounds.
        self.builder.position_at_end(loop_block);

        let current_index_val = self
            .builder
            .build_load(self.context.i64_type(), index_alloca, "current_index")?
            .into_int_value();

        let cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            current_index_val,
            size_val,
            "loop_cond",
        )?;
        self.builder
            .build_conditional_branch(cond, loop_block, end_block)?;

        // GEP to find the pointer of the current element: element_ptr = pointer_val + current_index
        let element_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                element_type, // T
                pointer_val,  // base pointer
                &[current_index_val],
                "element_ptr",
            )?
        };

        // 5) Load the element, call the `transform` closure, store result
        let loaded_value = self
            .builder
            .build_load(element_type, element_ptr, "loaded_value")?;
        let transformed_value = transform(loaded_value)?;
        self.builder.build_store(element_ptr, transformed_value)?;

        // 6) Increment the index and repeat
        let next_index = self.builder.build_int_add(
            current_index_val,
            self.context.i64_type().const_int(1, false),
            "next_index",
        )?;
        self.builder.build_store(index_alloca, next_index)?;
        self.builder.build_unconditional_branch(loop_block)?;

        // 7) At the `end` block, return the list_struct
        self.builder.position_at_end(end_block);
        Ok(list_struct)
    }
}
