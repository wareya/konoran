	.text
	.file	"rust_out.7f80e3a4-cgu.0"
	.globaltype	__stack_pointer, i32
	.functype	_ZN8rust_out3add17h4230cb203812714aE (i32, f32, f32, f32, f32) -> ()
	.section	.text._ZN8rust_out3add17h4230cb203812714aE,"",@
	.hidden	_ZN8rust_out3add17h4230cb203812714aE
	.globl	_ZN8rust_out3add17h4230cb203812714aE
	.type	_ZN8rust_out3add17h4230cb203812714aE,@function
_ZN8rust_out3add17h4230cb203812714aE:
	.functype	_ZN8rust_out3add17h4230cb203812714aE (i32, f32, f32, f32, f32) -> ()
	.local  	i32, i32, i32, i32, f32, f32, f32, f32
	global.get	__stack_pointer
	local.set	5
	i32.const	16
	local.set	6
	local.get	5
	local.get	6
	i32.sub 
	local.set	7
	i32.const	0
	local.set	8
	local.get	7
	local.get	8
	i32.store	8
	local.get	7
	local.get	8
	i32.store	12
	local.get	1
	local.get	3
	f32.add 
	local.set	9
	local.get	7
	local.get	9
	f32.store	8
	local.get	2
	local.get	4
	f32.add 
	local.set	10
	local.get	7
	local.get	10
	f32.store	12
	local.get	7
	f32.load	8
	local.set	11
	local.get	7
	f32.load	12
	local.set	12
	local.get	0
	local.get	12
	f32.store	4
	local.get	0
	local.get	11
	f32.store	0
	return
	end_function
.Lfunc_end0:
	.size	_ZN8rust_out3add17h4230cb203812714aE, .Lfunc_end0-_ZN8rust_out3add17h4230cb203812714aE

