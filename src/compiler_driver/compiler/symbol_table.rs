#[derive(Debug, PartialEq)]
pub enum Symbol {
    Int,
    FuncType { param_count: usize },
}

pub struct SymbolState {
    pub symbol_type: Symbol,
    pub is_defined: bool,
}
