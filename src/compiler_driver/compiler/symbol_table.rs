#[derive(Debug, PartialEq)]
pub enum Symbol {
    Int,
    FuncType { param_count: usize },
}

pub enum IdentifierAttributes {
    FuncAttribute { is_defined: bool, is_global: bool },
    StaticAttribute { init: InitialValue, is_global: bool },
    LocalAttribute,
}

pub enum InitialValue {
    Tentative,
    Initial(isize),
    NoInitializer,
}

pub struct SymbolState {
    pub symbol_type: Symbol,
    pub identifier_attributes: IdentifierAttributes,
}
