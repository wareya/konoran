#![no_std]

#[repr(C)]
pub struct Vec2
{
    x : f32,
    y : f32,
}
#[no_mangle]
pub fn add (a : Vec2, b : Vec2) -> Vec2
{
    let mut ret = Vec2{x : 0.0, y : 0.0};
    ret.x = a.x + b.x;
    ret.y = a.y + b.y;
    ret
}
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}