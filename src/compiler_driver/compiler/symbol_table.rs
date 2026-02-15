#[derive(Debug, PartialEq, Clone, Default)]
pub enum Type {
    #[default]
    Undefined,
    Int,
    Long,
    FuncType {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

pub enum IdentifierAttributes {
    FuncAttribute {
        is_defined: bool,
        is_globally_visible: bool,
    },
    StaticStorageAttribute {
        init: InitialValue,
        is_globally_visible: bool,
    },
    LocalAttribute,
}
#[derive(Clone)]
pub enum Constant {
    ConstInt(i32),
    ConstLong(i64),
}

impl Default for Constant {
    fn default() -> Self {
        Constant::ConstInt(0)
    }
}

#[derive(Clone)]
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64),
}

#[derive(Clone)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

pub struct Symbol {
    pub symbol_type: Type,
    pub identifier_attributes: IdentifierAttributes,
}
