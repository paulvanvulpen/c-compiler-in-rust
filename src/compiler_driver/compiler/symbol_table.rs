#[derive(Debug, PartialEq)]
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
pub enum InitialValue {
    Tentative,
    Initial(usize),
    NoInitializer,
}

pub struct SymbolState {
    pub symbol_type: Symbol,
    pub identifier_attributes: IdentifierAttributes,
}
