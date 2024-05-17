//! A simple, JITable, ultra low-level language, designed as a compiler target.
//!
//! You want [compiler::process_program].
//!
//! For information about the Konoran language, see <https://github.com/wareya/konoran/>.
//!
//! Here's a small example Konoran program:
//! ```rust
//! f32 func_gravity()
//! {
//!     u64 i = 0u64;
//!     f64 yvel = 0.0f64;
//!     f64 y = 0.0f64;
//!     f64 gravity = 9.8f64;
//!     f64 delta = 0.001f64;
//!     
//! head:
//!     yvel = yvel + delta*gravity*0.5f64;
//!     y = y + yvel*delta;
//!     yvel = yvel + delta*gravity*0.5f64;
//!     
//!     i = i + 1u64;
//!     
//!     if (i < 500000000u64)
//!         goto head;
//!     
//!     return (y) as f32;
//! }
//! ```


mod parser;
mod stdlib;
mod inkwell_helpers;
mod intrinsics_lists;
pub mod compiler;

