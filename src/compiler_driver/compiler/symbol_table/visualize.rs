use crate::compiler_driver::compiler::symbol_table;
use crate::compiler_driver::compiler::visualize;

impl visualize::Visualizer for symbol_table::Constant {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            symbol_table::Constant::ConstInt(x) => {
                format!("{}", x)
            }
            symbol_table::Constant::ConstLong(x) => {
                format!("{}L", x)
            }
        }
    }
}
