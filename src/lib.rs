//! Konoran is a simple, JITable, ultra low-level language, designed as a compiler target.
//!
//! You want [compiler::process_program].
//!
//! For information about the Konoran language, see <https://github.com/wareya/konoran/>.
//!
//! See <https://github.com/wareya/konoran/tree/main/examples> for more examples.
//!
//! Here's a small example Konoran program:
//! ```typescript
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
pub mod filelines;
pub mod compiler;

/// Re-exported from inkwell for documentation browsing convenience, but please see the upstream docs!
///
/// <https://thedan64.github.io/inkwell/inkwell/>
///
pub mod inkwell
{
    /// <https://thedan64.github.io/inkwell/inkwell/context/struct.Context.html?search=Context>
    ///
    /// (Re-exported for convenience. Some links here are broken because inkwell's docs are external. Please use the upstream docs!)
    ///
    pub use inkwell::context::Context;
    /// <https://thedan64.github.io/inkwell/inkwell/module/struct.Module.html?search=Module>
    ///
    /// (Re-exported for convenience. Some links here are broken because inkwell's docs are external. Please use the upstream docs!)
    ///
    pub use inkwell::module::Module;
    /// <https://thedan64.github.io/inkwell/inkwell/targets/struct.TargetMachine.html?search=TargetMachine>
    ///
    /// (Re-exported for convenience. Some links here are broken because inkwell's docs are external. Please use the upstream docs!)
    ///
    pub use inkwell::targets::TargetMachine;
    /// <https://thedan64.github.io/inkwell/inkwell/execution_engine/struct.ExecutionEngine.html?search=ExecutionEngine>
    ///
    /// (Re-exported for convenience. Some links here are broken because inkwell's docs are external. Please use the upstream docs!)
    ///
    pub use inkwell::execution_engine::ExecutionEngine;
}
