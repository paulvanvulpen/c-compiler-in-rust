#[derive(Debug, PartialEq)]
pub enum Symbol {
    Int,
    FuncType { param_count: usize },
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

pub enum InitialValue {
    Tentative,
    Initial(usize),
    NoInitializer,
}

pub struct SymbolState {
    pub symbol_type: Symbol,
    pub identifier_attributes: IdentifierAttributes,
}
