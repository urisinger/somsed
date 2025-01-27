use inkwell::{
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, StructType},
    AddressSpace,
};

#[derive(Debug, Clone, Copy)]
pub enum ListType<'ctx> {
    Number(StructType<'ctx>),
}

impl<'ctx> ListType<'ctx> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number_list",
        }
    }

    pub fn get_type(&self) -> StructType<'ctx> {
        match self {
            ListType::Number(list) => *list,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType<'ctx> {
    Number(FloatType<'ctx>),
    Point(StructType<'ctx>),
    List(ListType<'ctx>),
}

impl<'ctx> ValueType<'ctx> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Point(_) => "point",
            Self::List(list_type) => list_type.name(),
        }
    }

    pub fn metadata(&self) -> BasicMetadataTypeEnum<'ctx> {
        match self {
            Self::Number(t) => BasicMetadataTypeEnum::FloatType(*t),
            Self::Point(p) => BasicMetadataTypeEnum::StructType(*p),
            Self::List(list) => BasicMetadataTypeEnum::StructType(list.get_type()),
        }
    }

    pub fn type_enum(&self) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Number(n) => BasicTypeEnum::FloatType(*n),
            Self::Point(p) => BasicTypeEnum::StructType(*p),
            Self::List(list) => BasicTypeEnum::StructType(list.get_type()),
        }
    }
}
