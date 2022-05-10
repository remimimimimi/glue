//! All kind of compiler language queries upon which it is built.
//!
//! # Why queries?
//!
//! # How to read this documentation?
//!
//! See [groups] module for possible queries.
//!
//! # Examples
//!
//! # Feature plans

/// Definition of all queries traits.
pub mod groups;
/// Database interner structure
pub(crate) mod interner;
/// Internable values
pub(crate) mod values;

/// Repository of All cache.
#[salsa::database(groups::VfsStorage, groups::LanguageStorage, groups::ParserStorage)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl<'a> salsa::Database for Database {}
