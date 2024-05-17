//! A simple, JITable, ultra low-level language, designed as a compiler target.
//!
//! You want [compiler::process_program].

mod parser;
mod stdlib;
mod inkwell_helpers;
mod intrinsics_lists;
pub mod compiler;

