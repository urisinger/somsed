use inkwell::{
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    AddressSpace,
};

#[derive(Debug, Clone, Copy)]
pub enum ListType {
    Number,
}

impl ListType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Number => "number_list",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Number,
    List(ListType),
}

impl ValueType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Number => "number",
            Self::List(list_type) => list_type.name(),
        }
    }

    pub fn metadata<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
    ) -> BasicMetadataTypeEnum<'ctx> {
        match self {
            Self::Number => BasicMetadataTypeEnum::FloatType(context.f64_type()),
            Self::List(_) => BasicMetadataTypeEnum::StructType(
                context.struct_type(
                    &[
                        context
                            .ptr_type(AddressSpace::default())
                            .as_basic_type_enum(),
                        context.i64_type().as_basic_type_enum(),
                    ],
                    false,
                ),
            ),
        }
    }

    pub fn type_enum<'ctx>(&self, context: &'ctx inkwell::context::Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Number => BasicTypeEnum::FloatType(context.f64_type()),
            Self::List(_) => BasicTypeEnum::StructType(
                context.struct_type(
                    &[
                        context
                            .ptr_type(AddressSpace::default())
                            .as_basic_type_enum(),
                        context.i64_type().as_basic_type_enum(),
                    ],
                    false,
                ),
            ),
        }
    }
}
