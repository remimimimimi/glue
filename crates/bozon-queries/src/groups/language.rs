use crate::groups;

/// Main database trait which describes high-level language compiler functions.
#[salsa::query_group(LanguageStorage)]
pub trait Language: salsa::Database + groups::Vfs + groups::Parser {
    // fn check();
    // fn build();
}
