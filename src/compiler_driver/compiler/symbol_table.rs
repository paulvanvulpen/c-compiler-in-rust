#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    Int,
    Long,
    FuncType {
        parameter_types: Vec<Symbol>,
        return_type: Box<Symbol>,
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
    ConstInt(u32),
    ConstLong(u64),
}

impl Default for Constant {
    fn default() -> Self {
        Constant::ConstInt(0)
    }
}

#[derive(Clone)]
pub enum InitialValue {
    Tentative,
    Initial(Constant),
    NoInitializer,
}

pub struct SymbolState {
    pub symbol_type: Symbol,
    pub identifier_attributes: IdentifierAttributes,
}
