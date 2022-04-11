// TODO: Maybe refractor to global context of all cli runtime and local contexts
// of each query run?
/// Main context of program evaluation/compilation
pub struct Ctx {
    symbol_table: (),
    proto_symbol_table: (),
    strategy: StrategyKind,
}

pub enum StrategyKind {
    Interpretation,
    Compilation,
    Jit,
}
