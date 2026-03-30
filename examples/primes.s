
	.text

	.globl	GeneralApply
GeneralApply:
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	call	_apply

	.globl	GC
GC:
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	call	_gcMinor
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
	ret	

	.globl	_GetStartClosure
_GetStartClosure:
	leaq	ho143+1(%rip), %rax
	ret	

	.globl	_EnterMiranda
_EnterMiranda:
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
	movq	%rdi, %r11
	jmp	*7(%r11)

	.globl	_CallMiranda
_CallMiranda:
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
	jmp	*%rax

	.globl	_RetMiranda
_RetMiranda:
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
	ret	

	.globl	_PackArgs0
_PackArgs0:
0:
	movq	%rbp, %r11
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$65537, (%r11)
	leaq	UnpackArgs0(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	ret	

	.globl	_PackArgs1
_PackArgs1:
0:
	movq	%rbp, %r11
	addq	$32, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$4, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$131073, (%r11)
	leaq	UnpackArgs1(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	ret	

	.globl	_PackArgs2
_PackArgs2:
0:
	movq	%rbp, %r11
	addq	$40, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$5, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$196609, (%r11)
	leaq	UnpackArgs2(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	ret	

	.globl	_PackArgs3
_PackArgs3:
0:
	movq	%rbp, %r11
	addq	$48, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$6, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$262145, (%r11)
	leaq	UnpackArgs3(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	ret	

	.globl	_PackArgs4
_PackArgs4:
0:
	movq	%rbp, %r11
	addq	$56, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$7, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$327681, (%r11)
	leaq	UnpackArgs4(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	ret	

	.globl	_PackArgs5
_PackArgs5:
0:
	movq	%rbp, %r11
	addq	$64, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$8, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$393217, (%r11)
	leaq	UnpackArgs5(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	ret	

	.globl	_PackArgs6
_PackArgs6:
0:
	movq	%rbp, %r11
	addq	$72, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$9, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$458753, (%r11)
	leaq	UnpackArgs6(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	ret	

	.globl	_PackArgs7
_PackArgs7:
0:
	movq	%rbp, %r11
	addq	$80, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$10, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$524289, (%r11)
	leaq	UnpackArgs7(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	ret	

	.globl	_PackArgs8
_PackArgs8:
0:
	movq	%rbp, %r11
	addq	$88, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$11, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$589825, (%r11)
	leaq	UnpackArgs8(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	ret	

	.globl	_PackArgs9
_PackArgs9:
0:
	movq	%rbp, %r11
	addq	$96, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$12, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$655361, (%r11)
	leaq	UnpackArgs9(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	ret	

	.globl	_PackArgs10
_PackArgs10:
0:
	movq	%rbp, %r11
	addq	$104, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$13, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$720897, (%r11)
	leaq	UnpackArgs10(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	ret	

	.globl	_PackArgs11
_PackArgs11:
0:
	movq	%rbp, %r11
	addq	$112, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$14, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$786433, (%r11)
	leaq	UnpackArgs11(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	ret	

	.globl	_PackArgs12
_PackArgs12:
0:
	movq	%rbp, %r11
	addq	$120, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$15, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$851969, (%r11)
	leaq	UnpackArgs12(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 112(%r11)
	ret	

	.globl	_PackArgs13
_PackArgs13:
0:
	movq	%rbp, %r11
	addq	$128, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$16, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$917505, (%r11)
	leaq	UnpackArgs13(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 112(%r11)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 120(%r11)
	ret	

	.globl	_PackArgs14
_PackArgs14:
0:
	movq	%rbp, %r11
	addq	$136, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$17, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$983041, (%r11)
	leaq	UnpackArgs14(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 112(%r11)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 120(%r11)
	movq	_Arg+104(%rip), %rbx
	movq	%rbx, 128(%r11)
	ret	

	.globl	_PackArgs15
_PackArgs15:
0:
	movq	%rbp, %r11
	addq	$144, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$18, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$1048577, (%r11)
	leaq	UnpackArgs15(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 112(%r11)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 120(%r11)
	movq	_Arg+104(%rip), %rbx
	movq	%rbx, 128(%r11)
	movq	_Arg+112(%rip), %rbx
	movq	%rbx, 136(%r11)
	ret	

	.globl	_PackArgs16
_PackArgs16:
0:
	movq	%rbp, %r11
	addq	$152, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$19, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$1114113, (%r11)
	leaq	UnpackArgs16(%rip), %rbx
	movq	%rbx, 8(%r11)
	movq	%rax, 16(%r11)
	movq	%rdi, 24(%r11)
	movq	%rsi, 32(%r11)
	movq	%rdx, 40(%r11)
	movq	%rcx, 48(%r11)
	movq	%r8, 56(%r11)
	movq	%r9, 64(%r11)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 72(%r11)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 80(%r11)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 88(%r11)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 96(%r11)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 104(%r11)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 112(%r11)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 120(%r11)
	movq	_Arg+104(%rip), %rbx
	movq	%rbx, 128(%r11)
	movq	_Arg+112(%rip), %rbx
	movq	%rbx, 136(%r11)
	movq	_Arg+120(%rip), %rbx
	movq	%rbx, 144(%r11)
	ret	

UnpackArgs0:
	movq	15(%r11), %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

UnpackArgs1:
	movq	15(%r11), %rax
	leaq	_PackArgs1(%rip), %r10
	movq	23(%r11), %rdi
	ret	

UnpackArgs2:
	movq	15(%r11), %rax
	leaq	_PackArgs2(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	ret	

UnpackArgs3:
	movq	15(%r11), %rax
	leaq	_PackArgs3(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	ret	

UnpackArgs4:
	movq	15(%r11), %rax
	leaq	_PackArgs4(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	ret	

UnpackArgs5:
	movq	15(%r11), %rax
	leaq	_PackArgs5(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	ret	

UnpackArgs6:
	movq	15(%r11), %rax
	leaq	_PackArgs6(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	ret	

UnpackArgs7:
	movq	15(%r11), %rax
	leaq	_PackArgs7(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	ret	

UnpackArgs8:
	movq	15(%r11), %rax
	leaq	_PackArgs8(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	ret	

UnpackArgs9:
	movq	15(%r11), %rax
	leaq	_PackArgs9(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	ret	

UnpackArgs10:
	movq	15(%r11), %rax
	leaq	_PackArgs10(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	ret	

UnpackArgs11:
	movq	15(%r11), %rax
	leaq	_PackArgs11(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	ret	

UnpackArgs12:
	movq	15(%r11), %rax
	leaq	_PackArgs12(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	movq	111(%r11), %rbx
	movq	%rbx, _Arg+88(%rip)
	ret	

UnpackArgs13:
	movq	15(%r11), %rax
	leaq	_PackArgs13(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	movq	111(%r11), %rbx
	movq	%rbx, _Arg+88(%rip)
	movq	119(%r11), %rbx
	movq	%rbx, _Arg+96(%rip)
	ret	

UnpackArgs14:
	movq	15(%r11), %rax
	leaq	_PackArgs14(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	movq	111(%r11), %rbx
	movq	%rbx, _Arg+88(%rip)
	movq	119(%r11), %rbx
	movq	%rbx, _Arg+96(%rip)
	movq	127(%r11), %rbx
	movq	%rbx, _Arg+104(%rip)
	ret	

UnpackArgs15:
	movq	15(%r11), %rax
	leaq	_PackArgs15(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	movq	111(%r11), %rbx
	movq	%rbx, _Arg+88(%rip)
	movq	119(%r11), %rbx
	movq	%rbx, _Arg+96(%rip)
	movq	127(%r11), %rbx
	movq	%rbx, _Arg+104(%rip)
	movq	135(%r11), %rbx
	movq	%rbx, _Arg+112(%rip)
	ret	

UnpackArgs16:
	movq	15(%r11), %rax
	leaq	_PackArgs16(%rip), %r10
	movq	23(%r11), %rdi
	movq	31(%r11), %rsi
	movq	39(%r11), %rdx
	movq	47(%r11), %rcx
	movq	55(%r11), %r8
	movq	63(%r11), %r9
	movq	71(%r11), %rbx
	movq	%rbx, _Arg+48(%rip)
	movq	79(%r11), %rbx
	movq	%rbx, _Arg+56(%rip)
	movq	87(%r11), %rbx
	movq	%rbx, _Arg+64(%rip)
	movq	95(%r11), %rbx
	movq	%rbx, _Arg+72(%rip)
	movq	103(%r11), %rbx
	movq	%rbx, _Arg+80(%rip)
	movq	111(%r11), %rbx
	movq	%rbx, _Arg+88(%rip)
	movq	119(%r11), %rbx
	movq	%rbx, _Arg+96(%rip)
	movq	127(%r11), %rbx
	movq	%rbx, _Arg+104(%rip)
	movq	135(%r11), %rbx
	movq	%rbx, _Arg+112(%rip)
	movq	143(%r11), %rbx
	movq	%rbx, _Arg+120(%rip)
	ret	

ApplyToEnv0:
	movq	%rax, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

ApplyToEnv1:
	popq	%rdi
	movq	%rax, %r11
	movq	$1, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$1, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk1
	jmp	GeneralApply

ApplyToEnv2:
	popq	%rdi
	popq	%rsi
	movq	%rax, %r11
	movq	$2, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$2, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk2
	jmp	GeneralApply

ApplyToEnv3:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	movq	%rax, %r11
	movq	$3, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$3, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk3
	jmp	GeneralApply

ApplyToEnv4:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	movq	%rax, %r11
	movq	$4, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$4, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk4
	jmp	GeneralApply

ApplyToEnv5:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	movq	%rax, %r11
	movq	$5, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$5, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk5
	jmp	GeneralApply

ApplyToEnv6:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	movq	%rax, %r11
	movq	$6, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$6, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk6
	jmp	GeneralApply

ApplyToEnv7:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	movq	%rax, %r11
	movq	$7, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$7, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk7
	jmp	GeneralApply

ApplyToEnv8:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	movq	%rax, %r11
	movq	$8, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$8, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk8
	jmp	GeneralApply

ApplyToEnv9:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	movq	%rax, %r11
	movq	$9, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$9, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk9
	jmp	GeneralApply

ApplyToEnv10:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	movq	%rax, %r11
	movq	$10, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$10, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk10
	jmp	GeneralApply

ApplyToEnv11:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	popq	_Arg+80(%rip)
	movq	%rax, %r11
	movq	$11, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$11, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk11
	jmp	GeneralApply

ApplyToEnv12:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	popq	_Arg+80(%rip)
	popq	_Arg+88(%rip)
	movq	%rax, %r11
	movq	$12, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$12, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk12
	jmp	GeneralApply

ApplyToEnv13:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	popq	_Arg+80(%rip)
	popq	_Arg+88(%rip)
	popq	_Arg+96(%rip)
	movq	%rax, %r11
	movq	$13, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$13, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk13
	jmp	GeneralApply

ApplyToEnv14:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	popq	_Arg+80(%rip)
	popq	_Arg+88(%rip)
	popq	_Arg+96(%rip)
	popq	_Arg+104(%rip)
	movq	%rax, %r11
	movq	$14, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$14, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk14
	jmp	GeneralApply

ApplyToEnv15:
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%r8
	popq	%r9
	popq	_Arg+48(%rip)
	popq	_Arg+56(%rip)
	popq	_Arg+64(%rip)
	popq	_Arg+72(%rip)
	popq	_Arg+80(%rip)
	popq	_Arg+88(%rip)
	popq	_Arg+96(%rip)
	popq	_Arg+104(%rip)
	popq	_Arg+112(%rip)
	movq	%rax, %r11
	movq	$15, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$15, %ah
	jne	0f
	jmp	*7(%r11)
0:
	cmpb	$0, %ah
	je	OverApplyThunk15
	jmp	GeneralApply

	.data

	.globl	_ApplyToEnvFns
_ApplyToEnvFns:
	.quad	ApplyToEnv0
	.quad	ApplyToEnv1
	.quad	ApplyToEnv2
	.quad	ApplyToEnv3
	.quad	ApplyToEnv4
	.quad	ApplyToEnv5
	.quad	ApplyToEnv6
	.quad	ApplyToEnv7
	.quad	ApplyToEnv8
	.quad	ApplyToEnv9
	.quad	ApplyToEnv10
	.quad	ApplyToEnv11
	.quad	ApplyToEnv12
	.quad	ApplyToEnv13
	.quad	ApplyToEnv14
	.quad	ApplyToEnv15

	.text

OverApplyThunk1:
	subq	$8, %rsp
	movq	%rdi, (%rsp)
	leaq	ApplyToEnv1(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk2:
	subq	$16, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	leaq	ApplyToEnv2(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk3:
	subq	$24, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	ApplyToEnv3(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk4:
	subq	$32, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	ApplyToEnv4(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk5:
	subq	$40, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	leaq	ApplyToEnv5(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk6:
	subq	$48, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	leaq	ApplyToEnv6(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk7:
	subq	$56, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	leaq	ApplyToEnv7(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk8:
	subq	$64, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	leaq	ApplyToEnv8(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk9:
	subq	$72, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	leaq	ApplyToEnv9(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk10:
	subq	$80, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	leaq	ApplyToEnv10(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk11:
	subq	$88, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 80(%rsp)
	leaq	ApplyToEnv11(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk12:
	subq	$96, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 80(%rsp)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 88(%rsp)
	leaq	ApplyToEnv12(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk13:
	subq	$104, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 80(%rsp)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 88(%rsp)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 96(%rsp)
	leaq	ApplyToEnv13(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk14:
	subq	$112, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 80(%rsp)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 88(%rsp)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 96(%rsp)
	movq	_Arg+104(%rip), %rbx
	movq	%rbx, 104(%rsp)
	leaq	ApplyToEnv14(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

OverApplyThunk15:
	subq	$120, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	_Arg+48(%rip), %rbx
	movq	%rbx, 48(%rsp)
	movq	_Arg+56(%rip), %rbx
	movq	%rbx, 56(%rsp)
	movq	_Arg+64(%rip), %rbx
	movq	%rbx, 64(%rsp)
	movq	_Arg+72(%rip), %rbx
	movq	%rbx, 72(%rsp)
	movq	_Arg+80(%rip), %rbx
	movq	%rbx, 80(%rsp)
	movq	_Arg+88(%rip), %rbx
	movq	%rbx, 88(%rsp)
	movq	_Arg+96(%rip), %rbx
	movq	%rbx, 96(%rsp)
	movq	_Arg+104(%rip), %rbx
	movq	%rbx, 104(%rsp)
	movq	_Arg+112(%rip), %rbx
	movq	%rbx, 112(%rsp)
	leaq	ApplyToEnv15(%rip), %rbx
	pushq	%rbx
	jmp	*7(%r11)

	.text
# builtin.<unpack>

	.globl	co0
co0:
# RetEnv

# builtin.<update>

	.globl	co1
co1:
# Update (Ref (Env,0)) (Imm (Caddr 0))
	popq	%r13
	call	*%r10
	movb	$2, -1(%r13)
	leaq	co3(%rip), %rbx
	movq	%rbx, 7(%r13)
	leaq	1(%r11), %rbx
	movq	%rbx, 15(%r13)
	cmpq	_gcNewBase(%rip), %r13
	jae	0f
	movq	_gcRootsAlloc(%rip), %rbx
	subq	$8, %rbx
	movq	%r13, (%rbx)
	movq	%rbx, _gcRootsAlloc(%rip)
	cmpq	%rbx, _gcHeapTop(%rip)
	jl	0f
	movq	$0, _gcRequestSize(%rip)
	call	GC
0:
# Ret (Ref (Reg,0)) (Ref (Reg,1))
	ret	

# builtin.<applytoenv>

	.globl	co2
co2:
# ApplyToEnv

# builtin.<enterenv0>

	.globl	co3
co3:
# EnterInd (Ref (Env,0)) []
	movq	15(%r11), %r11
	jmp	*7(%r11)

# builtin.<selret>

	.globl	co4
co4:
# Sel (Reg,0) (Ref (Env,0))
# Ret (Ref (Reg,0)) (Imm (Word 0))
	leaq	_PackArgs0(%rip), %r10
	ret	

# builtin.<selapply>

	.globl	co5
co5:
# Sel (Reg,0) (Ref (Env,0))
# Apply (Ref (Reg,0)) []
	movq	%rax, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# builtin.<blackhole>

	.globl	co6
co6:
# Enter 152 []
	leaq	ho152+1(%rip), %r11
	jmp	*7(%r11)

# builtin.:

	.globl	co7
co7:
# Ret (Imm (Word 1)) (Imm (Word 2))
	movq	$2, %rax
	leaq	_PackArgs2(%rip), %r10
	ret	

# builtin.Nil

	.globl	co8
co8:
# Ret (Imm (Word 0)) (Imm (Word 0))
	movq	$0, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# builtin.Tuple2

	.globl	co9
co9:
# Ret (Imm (Word 0)) (Imm (Word 2))
	movq	$0, %rax
	leaq	_PackArgs2(%rip), %r10
	ret	

# builtin.Tuple3

	.globl	co10
co10:
# Ret (Imm (Word 0)) (Imm (Word 3))
	movq	$0, %rax
	leaq	_PackArgs3(%rip), %r10
	ret	

# builtin.Unit

	.globl	co11
co11:
# Ret (Imm (Word 0)) (Imm (Word 0))
	movq	$0, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# io.Handle

	.globl	co12
co12:
# Ret (Imm (Word 0)) (Imm (Word 1))
	movq	$0, %rax
	leaq	_PackArgs1(%rip), %r10
	ret	

# io.stdout

	.globl	co13
co13:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocStream (Reg,3) (Imm (Word 1)) (Imm (Word 32))
0:
	movq	%rbp, %r13
	addq	$264, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$33, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$2097413, (%r13)
	incq	%r13
# Call 12 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co12

# stdlib.C%hash

	.globl	co14
co14:
# Ret (Imm (Word 0)) (Imm (Word 1))
	movq	$0, %rax
	leaq	_PackArgs1(%rip), %r10
	ret	

# stdlib.writeByteStream

	.globl	co15
co15:
# PushCont 16 [Ref (Arg,0)]
	subq	$8, %rsp
	movq	%rdi, (%rsp)
	leaq	co16(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,1)) []
	movq	%rsi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.writeByteStream

	.globl	co16
co16:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [20,21]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co20
	.quad	co21

# stdlib.writeByteStream

	.globl	co17
co17:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# WriteStream (Reg,0) (Ref (Env,0)) (Ref (Arg,0))
	movq	$-2, %rax
	movq	15(%r11), %r10
	movzwq	3(%r10), %rbx
	movzwq	1(%r10), %rax
	shlq	$3, %rax
	cmpq	%rax, %rbx
	jae	1f
	movq	%rdi, %rax
	sarq	%rax
	movb	%al, 7(%r10, %rbx, 1)
	incw	%bx
	movw	%bx, 3(%r10)
	movq	$0, %rax
1:
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 19
	cmpq	$0, %rax
	je	co19
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 18 [Ref (Arg,0)]
	movq	$65537, -1(%r12)
	leaq	co18(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
# Call 7 [Ref (Frm,0),Ref (Env,1)]
	leaq	(%r12), %rdi
	movq	23(%r11), %rsi
	jmp	co7

# stdlib.writeByteStream

	.globl	co18
co18:
# Call 14 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co14

# stdlib.writeByteStream

	.globl	co19
co19:
# Call 15 [Ref (Env,0),Ref (Env,1)]
	movq	15(%r11), %rdi
	movq	23(%r11), %rsi
	jmp	co15

# stdlib.writeByteStream

	.globl	co20
co20:
# Call 8 []
	jmp	co8

# stdlib.writeByteStream

	.globl	co21
co21:
# PushCont 17 [Ref (Env,0),Ref (Arg,1)]
	subq	$16, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	%rsi, 8(%rsp)
	leaq	co17(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# io.writeFileStream

	.globl	co22
co22:
# PushCont 23 [Ref (Arg,1)]
	subq	$8, %rsp
	movq	%rsi, (%rsp)
	leaq	co23(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# io.writeFileStream

	.globl	co23
co23:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# AllocFrame 8
0:
	movq	%rbp, %r12
	addq	$64, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$8, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$8, _FrmSize(%rip)
# Fclosure (Frm,0) 24 2 [Ref (Frm,0),Ref (Arg,0)]
	movq	$131584, -1(%r12)
	leaq	co24(%rip), %rbx
	movq	%rbx, 7(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 15(%r12)
	movq	%rdi, 23(%r12)
# Fclosure (Frm,4) 27 1 [Ref (Frm,0),Ref (Env,0)]
	movq	$131328, 31(%r12)
	leaq	co27(%rip), %rbx
	movq	%rbx, 39(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 47(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 55(%r12)
# Ret (Ref (Frm,4)) (Imm (Word 0))
	leaq	32(%r12), %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# io.writeFileStream`restart

	.globl	co24
co24:
# PushCont 25 [Ref (Env,0),Ref (Env,1),Ref (Arg,1)]
	subq	$24, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	23(%r11), %rbx
	movq	%rbx, 8(%rsp)
	movq	%rsi, 16(%rsp)
	leaq	co25(%rip), %rbx
	pushq	%rbx
# Call 15 [Ref (Env,1),Ref (Arg,0)]
	movq	%rdi, _Arg(%rip)
	movq	23(%r11), %rdi
	movq	_Arg(%rip), %rsi
	jmp	co15

# io.writeFileStream`restart

	.globl	co25
co25:
# PopEnv 3
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	popq	_Env+32(%rip)
	leaq	_Env+1(%rip), %r11
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 26
	cmpq	$0, %rax
	je	co26
# Vclosure (Reg,3) 0
	pushq	%r11
	call	*%r10
	leaq	1(%r11), %r13
	popq	%r11
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# EnterInd (Ref (Env,0)) [Ref (Reg,3),Ref (Env,2)]
	movq	%r13, %rdi
	movq	31(%r11), %rsi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# io.writeFileStream`restart

	.globl	co26
co26:
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# SystemOp 4 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$8, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# Call 9 [Imm (Haddr 8),Ref (Env,2)]
	leaq	ho8+1(%rip), %rdi
	movq	31(%r11), %rsi
	jmp	co9

# io.writeFileStream

	.globl	co27
co27:
# EnterInd (Ref (Env,0)) [Ref (Env,1),Ref (Arg,0)]
	movq	%rdi, _Arg(%rip)
	movq	23(%r11), %rdi
	movq	_Arg(%rip), %rsi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# io.putStr

	.globl	co28
co28:
# Call 22 [Imm (Haddr 12),Ref (Arg,0)]
	movq	%rdi, _Arg(%rip)
	leaq	ho12+1(%rip), %rdi
	movq	_Arg(%rip), %rsi
	jmp	co22

# stdlib.++

	.globl	co29
co29:
# PushCont 30 [Ref (Arg,1)]
	subq	$8, %rsp
	movq	%rsi, (%rsp)
	leaq	co30(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.++

	.globl	co30
co30:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [32,33]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co32
	.quad	co33

# stdlib.++

	.globl	co31
co31:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,1),Ref (Env,0)]
	movq	23(%r11), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# stdlib.++

	.globl	co32
co32:
# Apply (Ref (Env,0)) []
	movq	15(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.++

	.globl	co33
co33:
# AllocFrame 4
0:
	movq	%rbp, %r12
	addq	$32, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$4, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$4, _FrmSize(%rip)
# Tclosure (Frm,0) 31 [Ref (Env,0),Ref (Arg,1)]
	movq	$131073, -1(%r12)
	leaq	co31(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 15(%r12)
	movq	%rsi, 23(%r12)
# Call 7 [Ref (Arg,0),Ref (Frm,0)]
	leaq	(%r12), %rsi
	jmp	co7

# stdlib.readByteStream

	.globl	co34
co34:
# ReadStream (Reg,3) (Ref (Arg,0))
	movq	$-2, %r13
	movzwq	5(%rdi), %rbx
	cmpw	3(%rdi), %bx
	jae	1f
	movzbq	7(%rdi, %rbx, 1), %r13
	shlq	%r13
	incw	%bx
	movw	%bx, 5(%rdi)
1:
# Cmp (Reg,0) (Ref (Reg,3)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 1)) 37
	cmpq	$0, %r13
	jl	co37
# AllocFrame 6
0:
	movq	%rbp, %r12
	addq	$48, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$6, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$6, _FrmSize(%rip)
# Tclosure (Frm,0) 35 [Ref (Reg,3)]
	movq	$65537, -1(%r12)
	leaq	co35(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%r13, 15(%r12)
# Tclosure (Frm,3) 36 [Ref (Arg,0)]
	movq	$65537, 23(%r12)
	leaq	co36(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	%rdi, 39(%r12)
# Call 7 [Ref (Frm,0),Ref (Frm,3)]
	leaq	(%r12), %rdi
	leaq	24(%r12), %rsi
	jmp	co7

# stdlib.readByteStream

	.globl	co35
co35:
# Call 14 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co14

# stdlib.readByteStream

	.globl	co36
co36:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co34

# stdlib.readByteStream

	.globl	co37
co37:
# Call 8 []
	jmp	co8

# io.lit_35

	.globl	co38
co38:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 30)]
	leaq	ho30+1(%rip), %rdi
	jmp	co34

# io.putStrLn

	.globl	co39
co39:
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 40 [Ref (Arg,0)]
	movq	$65537, -1(%r12)
	leaq	co40(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
# Call 28 [Ref (Frm,0)]
	leaq	(%r12), %rdi
	jmp	co28

# io.putStrLn

	.globl	co40
co40:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,0),Imm (Haddr 27)]
	movq	15(%r11), %rdi
	leaq	ho27+1(%rip), %rsi
	jmp	co29

# stdlib.I%hash

	.globl	co41
co41:
# Ret (Imm (Word 0)) (Imm (Word 1))
	movq	$0, %rax
	leaq	_PackArgs1(%rip), %r10
	ret	

# stdlib.lit_199

	.globl	co42
co42:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 39)]
	leaq	ho39+1(%rip), %rdi
	jmp	co34

# stdlib.lit_247

	.globl	co43
co43:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 45)]
	leaq	ho45+1(%rip), %rdi
	jmp	co34

# stdlib.error

	.globl	co44
co44:
# AllocStream (Reg,3) (Imm (Word 2)) (Imm (Word 32))
0:
	movq	%rbp, %r13
	addq	$264, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$33, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$2097669, (%r13)
	incq	%r13
# AllocFrame 9
0:
	movq	%rbp, %r12
	addq	$72, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$9, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$9, _FrmSize(%rip)
# Tclosure (Frm,0) 45 [Ref (Arg,0)]
	movq	$65537, -1(%r12)
	leaq	co45(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
# Tclosure (Frm,3) 46 [Ref (Frm,0)]
	movq	$65537, 23(%r12)
	leaq	co46(%rip), %rbx
	movq	%rbx, 31(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 39(%r12)
# Fclosure (Frm,6) 48 2 [Ref (Frm,6)]
	movq	$66048, 47(%r12)
	leaq	co48(%rip), %rbx
	movq	%rbx, 55(%r12)
	leaq	48(%r12), %rbx
	movq	%rbx, 63(%r12)
# PushCont 51 []
	leaq	co51(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Frm,6)) [Ref (Reg,3),Ref (Frm,3)]
	movq	%r13, %rdi
	leaq	24(%r12), %rsi
	leaq	48(%r12), %r11
	jmp	*7(%r11)

# stdlib.error

	.globl	co45
co45:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Imm (Haddr 36),Ref (Env,0)]
	leaq	ho36+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# stdlib.error

	.globl	co46
co46:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 47 [Ref (Env,0)]
	movq	$65537, -1(%r12)
	leaq	co47(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 15(%r12)
# Call 29 [Imm (Haddr 42),Ref (Frm,0)]
	leaq	ho42+1(%rip), %rdi
	leaq	(%r12), %rsi
	jmp	co29

# stdlib.error

	.globl	co47
co47:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,0),Imm (Haddr 42)]
	movq	15(%r11), %rdi
	leaq	ho42+1(%rip), %rsi
	jmp	co29

# stdlib.error

	.globl	co48
co48:
# PushCont 49 [Ref (Env,0),Ref (Arg,0)]
	subq	$16, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	%rdi, 8(%rsp)
	leaq	co49(%rip), %rbx
	pushq	%rbx
# Call 15 [Ref (Arg,0),Ref (Arg,1)]
	jmp	co15

# stdlib.error

	.globl	co49
co49:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 50
	cmpq	$0, %rax
	je	co50
# Vclosure (Reg,3) 0
	pushq	%r11
	call	*%r10
	leaq	1(%r11), %r13
	popq	%r11
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# EnterInd (Ref (Env,0)) [Ref (Env,1),Ref (Reg,3)]
	movq	23(%r11), %rdi
	movq	%r13, %rsi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# stdlib.error

	.globl	co50
co50:
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# Call 11 []
	jmp	co11

# stdlib.error

	.globl	co51
co51:
# PopEnv 0
# SystemOp 0 (Reg,0) (Imm (Word 1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$0, %rdi
	movq	$0, %rsi
	movq	$2, %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp

# stdlib.lit_227

	.globl	co52
co52:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 52)]
	leaq	ho52+1(%rip), %rdi
	jmp	co34

# stdlib.lit_234

	.globl	co53
co53:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 60)]
	leaq	ho60+1(%rip), %rdi
	jmp	co34

# stdlib.lit_292

	.globl	co54
co54:
# Call 14 [Imm (Word 45)]
	movq	$90, %rdi
	jmp	co14

# stdlib.lit_367

	.globl	co55
co55:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 71)]
	leaq	ho71+1(%rip), %rdi
	jmp	co34

# stdlib.lit_444

	.globl	co56
co56:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 78)]
	leaq	ho78+1(%rip), %rdi
	jmp	co34

# stdlib.showintBase

	.globl	co57
co57:
# PushCont 58 [Ref (Arg,0),Ref (Arg,1)]
	subq	$16, %rsp
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	leaq	co58(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,2)) []
	movq	%rdx, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.showintBase

	.globl	co58
co58:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp / CaseVec
	cmpq	$0, %rdi
	je	co80
	jl	co81
	jg	co82

# stdlib.showintBase

	.globl	co59
co59:
# Call 7 [Imm (Haddr 65),Ref (Env,0)]
	leaq	ho65+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co7

# stdlib.showintBase

	.globl	co60
co60:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Sub (Reg,3) (Imm (Word 0)) (Ref (Env,0))
	movq	$0, %r13
	subq	15(%r11), %r13
# Call 41 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co41

# stdlib.showintBase`codeA

	.globl	co61
co61:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Sub (Reg,3) (Imm (Word 97)) (Imm (Word 10))
	movq	$194, %r13
	subq	$20, %r13
# Call 41 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co41

# stdlib.showintBase`go

	.globl	co62
co62:
# PushCont 63 [Ref (Env,0),Ref (Env,1),Ref (Env,2),Ref (Arg,0)]
	subq	$32, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	23(%r11), %rbx
	movq	%rbx, 8(%rsp)
	movq	31(%r11), %rbx
	movq	%rbx, 16(%rsp)
	movq	%rdi, 24(%rsp)
	leaq	co63(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,1)) []
	movq	%rsi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.showintBase`go

	.globl	co63
co63:
# PopEnv 4
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	popq	_Env+32(%rip)
	popq	_Env+40(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp (Reg,0) (Ref (Arg,0)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 77
	cmpq	$0, %rdi
	je	co77
# AllocFrame 13
0:
	movq	%rbp, %r12
	addq	$104, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$13, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$13, _FrmSize(%rip)
# Tclosure (Frm,0) 64 [Ref (Arg,0),Ref (Env,0),Ref (Env,2)]
	movq	$196609, -1(%r12)
	leaq	co64(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 23(%r12)
	movq	31(%r11), %rbx
	movq	%rbx, 31(%r12)
# Tclosure (Frm,5) 73 [Ref (Frm,0),Ref (Env,3)]
	movq	$131073, 39(%r12)
	leaq	co73(%rip), %rbx
	movq	%rbx, 47(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 55(%r12)
	movq	39(%r11), %rbx
	movq	%rbx, 63(%r12)
# Tclosure (Frm,9) 74 [Ref (Arg,0),Ref (Env,2)]
	movq	$131073, 71(%r12)
	leaq	co74(%rip), %rbx
	movq	%rbx, 79(%r12)
	movq	%rdi, 87(%r12)
	movq	31(%r11), %rbx
	movq	%rbx, 95(%r12)
# EnterInd (Ref (Env,1)) [Ref (Frm,5),Ref (Frm,9)]
	leaq	40(%r12), %rdi
	leaq	72(%r12), %rsi
	movq	23(%r11), %r11
	jmp	*7(%r11)

# stdlib.showintBase`go

	.globl	co64
co64:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# PushCont 65 [Ref (Env,0),Ref (Env,1)]
	subq	$16, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	23(%r11), %rbx
	movq	%rbx, 8(%rsp)
	leaq	co65(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Env,2)) []
	movq	31(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.showintBase`go

	.globl	co65
co65:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp (Reg,0) (Ref (Arg,0)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 72
	cmpq	$0, %rdi
	je	co72
# Mod (Reg,3) (Ref (Env,0)) (Ref (Arg,0))
	push	%rdx
	movq	%rdi, %r13
	sarq	%r13
	movq	15(%r11), %rax
	sarq	%rax
	cqto	
	idivq	%r13
	cmpq	$0, %rdx
	jz	0f
	movq	%rdi, %rbx
	sarq	$63, %rbx
	movq	%rdx, %rax
	sarq	$63, %rax
	cmpq	%rax, %rbx
	je	0f
	movq	%rdi, %rbx
	sarq	%rbx
	addq	%rbx, %rdx
0:
	shlq	%rdx
	movq	%rdx, %r13
	pop	%rdx
# Cmp (Reg,0) (Ref (Reg,3)) (Imm (Word 10))
# Jeq (Ref (Reg,0)) (Imm (Word 1)) 71
	cmpq	$20, %r13
	jl	co71
# PushCont 68 [Ref (Reg,3)]
	subq	$8, %rsp
	movq	%r13, (%rsp)
	leaq	co68(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,1)) []
	movq	23(%r11), %r11
	jmp	*7(%r11)

# stdlib.showintBase`go

	.globl	co66
co66:
# Call 44 [Imm (Haddr 57)]
	leaq	ho57+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co67
co67:
# Call 44 [Imm (Haddr 57)]
	leaq	ho57+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co68
co68:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# Add (Reg,3) (Ref (Arg,0)) (Ref (Env,0))
	movq	%rdi, %r13
	addq	15(%r11), %r13
# Cmp (Reg,0) (Ref (Reg,3)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 1)) 70
	cmpq	$0, %r13
	jl	co70
# Cmp (Reg,0) (Ref (Reg,3)) (Imm (Word 255))
# Jeq (Ref (Reg,0)) (Imm (Word 2)) 69
	cmpq	$510, %r13
	jg	co69
# Call 14 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co14

# stdlib.showintBase`go

	.globl	co69
co69:
# Call 44 [Imm (Haddr 57)]
	leaq	ho57+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co70
co70:
# Call 44 [Imm (Haddr 57)]
	leaq	ho57+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co71
co71:
# Add (Reg,4) (Imm (Word 48)) (Ref (Reg,3))
	movq	$96, %r14
	addq	%r13, %r14
# Cmp (Reg,0) (Ref (Reg,4)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 1)) 67
	cmpq	$0, %r14
	jl	co67
# Cmp (Reg,0) (Ref (Reg,4)) (Imm (Word 255))
# Jeq (Ref (Reg,0)) (Imm (Word 2)) 66
	cmpq	$510, %r14
	jg	co66
# Call 14 [Ref (Reg,4)]
	movq	%r14, %rdi
	jmp	co14

# stdlib.showintBase`go

	.globl	co72
co72:
# Call 44 [Imm (Haddr 68)]
	leaq	ho68+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co73
co73:
# Call 7 [Ref (Env,0),Ref (Env,1)]
	movq	15(%r11), %rdi
	movq	23(%r11), %rsi
	jmp	co7

# stdlib.showintBase`go

	.globl	co74
co74:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# PushCont 75 [Ref (Env,0)]
	subq	$8, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	leaq	co75(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Env,1)) []
	movq	23(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.showintBase`go

	.globl	co75
co75:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp (Reg,0) (Ref (Arg,0)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 76
	cmpq	$0, %rdi
	je	co76
# Div (Reg,3) (Ref (Env,0)) (Ref (Arg,0))
	push	%rdx
	movq	%rdi, %r13
	sarq	%r13
	movq	15(%r11), %rax
	sarq	%rax
	cqto	
	idivq	%r13
	cmpq	$0, %rdx
	jz	0f
	movq	%rdi, %rbx
	sarq	$63, %rbx
	sarq	$63, %rdx
	xorq	%rdx, %rbx
	addq	%rbx, %rax
0:
	shlq	%rax
	movq	%rax, %r13
	pop	%rdx
# Call 41 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co41

# stdlib.showintBase`go

	.globl	co76
co76:
# Call 44 [Imm (Haddr 49)]
	leaq	ho49+1(%rip), %rdi
	jmp	co44

# stdlib.showintBase`go

	.globl	co77
co77:
# Apply (Ref (Env,3)) []
	movq	39(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.showintBase

	.globl	co78
co78:
# Call 41 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co41

# stdlib.showintBase

	.globl	co79
co79:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,1)) [Imm (Haddr 2),Ref (Env,0)]
	leaq	ho2+1(%rip), %rdi
	movq	15(%r11), %rsi
	movq	23(%r11), %r11
	jmp	*7(%r11)

# stdlib.showintBase

	.globl	co80
co80:
# Call 29 [Ref (Env,1),Imm (Haddr 75)]
	movq	23(%r11), %rdi
	leaq	ho75+1(%rip), %rsi
	jmp	co29

# stdlib.showintBase

	.globl	co81
co81:
# AllocFrame 6
0:
	movq	%rbp, %r12
	addq	$48, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$6, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$6, _FrmSize(%rip)
# Tclosure (Frm,0) 59 [Ref (Env,1)]
	movq	$65537, -1(%r12)
	leaq	co59(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	23(%r11), %rbx
	movq	%rbx, 15(%r12)
# Tclosure (Frm,3) 60 [Ref (Arg,0)]
	movq	$65537, 23(%r12)
	leaq	co60(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	%rdi, 39(%r12)
# Call 57 [Ref (Env,0),Ref (Frm,0),Ref (Frm,3)]
	movq	15(%r11), %rdi
	leaq	(%r12), %rsi
	leaq	24(%r12), %rdx
	jmp	co57

# stdlib.showintBase

	.globl	co82
co82:
# AllocFrame 15
0:
	movq	%rbp, %r12
	addq	$120, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$15, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$15, _FrmSize(%rip)
# Tclosure (Frm,0) 61 []
	movq	$65537, -1(%r12)
	leaq	co61(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	$0, 15(%r12)
# Fclosure (Frm,3) 62 2 [Ref (Frm,0),Ref (Frm,3),Ref (Env,0)]
	movq	$197120, 23(%r12)
	leaq	co62(%rip), %rbx
	movq	%rbx, 31(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 39(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 47(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 55(%r12)
# Tclosure (Frm,8) 78 [Ref (Arg,0)]
	movq	$65537, 63(%r12)
	leaq	co78(%rip), %rbx
	movq	%rbx, 71(%r12)
	movq	%rdi, 79(%r12)
# Tclosure (Frm,11) 79 [Ref (Frm,8),Ref (Frm,3)]
	movq	$131073, 87(%r12)
	leaq	co79(%rip), %rbx
	movq	%rbx, 95(%r12)
	leaq	64(%r12), %rbx
	movq	%rbx, 103(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 111(%r12)
# Call 29 [Ref (Env,1),Ref (Frm,11)]
	movq	23(%r11), %rdi
	leaq	88(%r12), %rsi
	jmp	co29

# stdlib.lit_161

	.globl	co83
co83:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 85)]
	leaq	ho85+1(%rip), %rdi
	jmp	co34

# stdlib.lit_162

	.globl	co84
co84:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 92)]
	leaq	ho92+1(%rip), %rdi
	jmp	co34

# stdlib.lit_163

	.globl	co85
co85:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 98)]
	leaq	ho98+1(%rip), %rdi
	jmp	co34

# stdlib.lit_297

	.globl	co86
co86:
# Call 41 [Imm (Word 10)]
	movq	$20, %rdi
	jmp	co41

# stdlib.lit_405

	.globl	co87
co87:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 106)]
	leaq	ho106+1(%rip), %rdi
	jmp	co34

# stdlib.caseFail

	.globl	co88
co88:
# PushCont 89 []
	leaq	co89(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.caseFail

	.globl	co89
co89:
# PopEnv 0
# AllocFrame 23
0:
	movq	%rbp, %r12
	addq	$184, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$23, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$23, _FrmSize(%rip)
# Tclosure (Frm,0) 90 [Ref (Arg,1)]
	movq	$65537, -1(%r12)
	leaq	co90(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rsi, 15(%r12)
# Tclosure (Frm,3) 92 [Ref (Arg,2)]
	movq	$65537, 23(%r12)
	leaq	co92(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	%rdx, 39(%r12)
# Tclosure (Frm,6) 94 [Ref (Frm,3)]
	movq	$65537, 47(%r12)
	leaq	co94(%rip), %rbx
	movq	%rbx, 55(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 63(%r12)
# Tclosure (Frm,9) 95 [Ref (Frm,0),Ref (Frm,6)]
	movq	$131073, 71(%r12)
	leaq	co95(%rip), %rbx
	movq	%rbx, 79(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 87(%r12)
	leaq	48(%r12), %rbx
	movq	%rbx, 95(%r12)
# Tclosure (Frm,13) 96 [Ref (Frm,9)]
	movq	$65537, 103(%r12)
	leaq	co96(%rip), %rbx
	movq	%rbx, 111(%r12)
	leaq	72(%r12), %rbx
	movq	%rbx, 119(%r12)
# Tclosure (Frm,16) 97 [Ref (Frm,13),Ref (Arg,0)]
	movq	$131073, 127(%r12)
	leaq	co97(%rip), %rbx
	movq	%rbx, 135(%r12)
	leaq	104(%r12), %rbx
	movq	%rbx, 143(%r12)
	movq	%rdi, 151(%r12)
# Tclosure (Frm,20) 98 [Ref (Frm,16)]
	movq	$65537, 159(%r12)
	leaq	co98(%rip), %rbx
	movq	%rbx, 167(%r12)
	leaq	128(%r12), %rbx
	movq	%rbx, 175(%r12)
# Call 44 [Ref (Frm,20)]
	leaq	160(%r12), %rdi
	jmp	co44

# stdlib.caseFail

	.globl	co90
co90:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 91 [Ref (Env,0)]
	movq	$65537, -1(%r12)
	leaq	co91(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 15(%r12)
# Call 57 [Imm (Haddr 100),Imm (Haddr 103),Ref (Frm,0)]
	leaq	ho100+1(%rip), %rdi
	leaq	ho103+1(%rip), %rsi
	leaq	(%r12), %rdx
	jmp	co57

# stdlib.caseFail

	.globl	co91
co91:
# Call 41 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co41

# stdlib.caseFail

	.globl	co92
co92:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 93 [Ref (Env,0)]
	movq	$65537, -1(%r12)
	leaq	co93(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 15(%r12)
# Call 57 [Imm (Haddr 100),Imm (Haddr 103),Ref (Frm,0)]
	leaq	ho100+1(%rip), %rdi
	leaq	ho103+1(%rip), %rsi
	leaq	(%r12), %rdx
	jmp	co57

# stdlib.caseFail

	.globl	co93
co93:
# Call 41 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co41

# stdlib.caseFail

	.globl	co94
co94:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Imm (Haddr 95),Ref (Env,0)]
	leaq	ho95+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# stdlib.caseFail

	.globl	co95
co95:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,0),Ref (Env,1)]
	movq	15(%r11), %rdi
	movq	23(%r11), %rsi
	jmp	co29

# stdlib.caseFail

	.globl	co96
co96:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Imm (Haddr 89),Ref (Env,0)]
	leaq	ho89+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# stdlib.caseFail

	.globl	co97
co97:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,1),Ref (Env,0)]
	movq	23(%r11), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# stdlib.caseFail

	.globl	co98
co98:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Imm (Haddr 82),Ref (Env,0)]
	leaq	ho82+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co29

# primes.lit_30

	.globl	co99
co99:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 112)]
	leaq	ho112+1(%rip), %rdi
	jmp	co34

# primes.primes

	.globl	co100
co100:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 6
0:
	movq	%rbp, %r12
	addq	$48, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$6, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$6, _FrmSize(%rip)
# Fclosure (Frm,0) 101 1 [Ref (Frm,0)]
	movq	$65792, -1(%r12)
	leaq	co101(%rip), %rbx
	movq	%rbx, 7(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 15(%r12)
# Tclosure (Frm,3) 118 []
	movq	$65537, 23(%r12)
	leaq	co118(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	$0, 39(%r12)
# EnterInd (Ref (Frm,0)) [Ref (Frm,3)]
	leaq	24(%r12), %rdi
	leaq	(%r12), %r11
	jmp	*7(%r11)

# primes.primes`sieve

	.globl	co101
co101:
# PushCont 102 [Ref (Env,0)]
	subq	$8, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	leaq	co102(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.primes`sieve

	.globl	co102
co102:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [116,117]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co116
	.quad	co117

# primes.primes`sieve

	.globl	co103
co103:
# Call 10 [Imm (Haddr 109),Imm (Word 6),Imm (Word 12)]
	leaq	ho109+1(%rip), %rdi
	movq	$12, %rsi
	movq	$24, %rdx
	jmp	co10

# primes.primes`sieve

	.globl	co104
co104:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 4
0:
	movq	%rbp, %r12
	addq	$32, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$4, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$4, _FrmSize(%rip)
# Fclosure (Frm,0) 105 1 [Ref (Frm,0),Ref (Env,0)]
	movq	$131328, -1(%r12)
	leaq	co105(%rip), %rbx
	movq	%rbx, 7(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 15(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 23(%r12)
# EnterInd (Ref (Frm,0)) [Ref (Env,1)]
	movq	23(%r11), %rdi
	leaq	(%r12), %r11
	jmp	*7(%r11)

# primes.primes`sieve

	.globl	co105
co105:
# PushCont 106 [Ref (Env,0),Ref (Env,1)]
	subq	$16, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	23(%r11), %rbx
	movq	%rbx, 8(%rsp)
	leaq	co106(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.primes`sieve

	.globl	co106
co106:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [113,114]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co113
	.quad	co114

# primes.primes`sieve

	.globl	co107
co107:
# PopEnv 3
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	popq	_Env+32(%rip)
	leaq	_Env+1(%rip), %r11
# PushCont 108 [Ref (Env,0),Ref (Arg,0),Ref (Env,2)]
	subq	$24, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	%rdi, 8(%rsp)
	movq	31(%r11), %rbx
	movq	%rbx, 16(%rsp)
	leaq	co108(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Env,1)) []
	movq	23(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.primes`sieve

	.globl	co108
co108:
# PopEnv 3
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	popq	_Env+32(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp (Reg,0) (Ref (Arg,0)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 112
	cmpq	$0, %rdi
	je	co112
# Mod (Reg,3) (Ref (Env,1)) (Ref (Arg,0))
	push	%rdx
	movq	%rdi, %r13
	sarq	%r13
	movq	23(%r11), %rax
	sarq	%rax
	cqto	
	idivq	%r13
	cmpq	$0, %rdx
	jz	0f
	movq	%rdi, %rbx
	sarq	$63, %rbx
	movq	%rdx, %rax
	sarq	$63, %rax
	cmpq	%rax, %rbx
	je	0f
	movq	%rdi, %rbx
	sarq	%rbx
	addq	%rbx, %rdx
0:
	shlq	%rdx
	movq	%rdx, %r13
	pop	%rdx
# Cmp (Reg,0) (Ref (Reg,3)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 111
	cmpq	$0, %r13
	je	co111
# AllocFrame 7
0:
	movq	%rbp, %r12
	addq	$56, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$7, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$7, _FrmSize(%rip)
# Tclosure (Frm,0) 109 [Ref (Env,1)]
	movq	$65537, -1(%r12)
	leaq	co109(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	23(%r11), %rbx
	movq	%rbx, 15(%r12)
# Tclosure (Frm,3) 110 [Ref (Env,0),Ref (Env,2)]
	movq	$131073, 23(%r12)
	leaq	co110(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 39(%r12)
	movq	31(%r11), %rbx
	movq	%rbx, 47(%r12)
# Call 7 [Ref (Frm,0),Ref (Frm,3)]
	leaq	(%r12), %rdi
	leaq	24(%r12), %rsi
	jmp	co7

# primes.primes`sieve

	.globl	co109
co109:
# Call 41 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co41

# primes.primes`sieve

	.globl	co110
co110:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,0)) [Ref (Env,1)]
	movq	23(%r11), %rdi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# primes.primes`sieve

	.globl	co111
co111:
# EnterInd (Ref (Env,0)) [Ref (Env,2)]
	movq	31(%r11), %rdi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# primes.primes`sieve

	.globl	co112
co112:
# Call 44 [Imm (Haddr 68)]
	leaq	ho68+1(%rip), %rdi
	jmp	co44

# primes.primes`sieve

	.globl	co113
co113:
# Call 8 []
	jmp	co8

# primes.primes`sieve

	.globl	co114
co114:
# PushCont 107 [Ref (Env,0),Ref (Env,1),Ref (Arg,1)]
	subq	$24, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	23(%r11), %rbx
	movq	%rbx, 8(%rsp)
	movq	%rsi, 16(%rsp)
	leaq	co107(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.primes`sieve

	.globl	co115
co115:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,1)) [Ref (Env,0)]
	movq	15(%r11), %rdi
	movq	23(%r11), %r11
	jmp	*7(%r11)

# primes.primes`sieve

	.globl	co116
co116:
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 103 []
	movq	$65537, -1(%r12)
	leaq	co103(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	$0, 15(%r12)
# Call 88 [Ref (Frm,0)]
	leaq	(%r12), %rdi
	jmp	co88

# primes.primes`sieve

	.globl	co117
co117:
# AllocFrame 8
0:
	movq	%rbp, %r12
	addq	$64, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$8, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$8, _FrmSize(%rip)
# Tclosure (Frm,0) 104 [Ref (Arg,0),Ref (Arg,1)]
	movq	$131073, -1(%r12)
	leaq	co104(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
	movq	%rsi, 23(%r12)
# Tclosure (Frm,4) 115 [Ref (Frm,0),Ref (Env,0)]
	movq	$131073, 31(%r12)
	leaq	co115(%rip), %rbx
	movq	%rbx, 39(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 47(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 55(%r12)
# Call 7 [Ref (Arg,0),Ref (Frm,4)]
	leaq	32(%r12), %rsi
	jmp	co7

# primes.primes

	.globl	co118
co118:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Fclosure (Frm,0) 119 1 [Ref (Frm,0)]
	movq	$65792, -1(%r12)
	leaq	co119(%rip), %rbx
	movq	%rbx, 7(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 15(%r12)
# EnterInd (Ref (Frm,0)) [Imm (Word 2)]
	movq	$4, %rdi
	leaq	(%r12), %r11
	jmp	*7(%r11)

# primes.primes

	.globl	co119
co119:
# AllocFrame 7
0:
	movq	%rbp, %r12
	addq	$56, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$7, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$7, _FrmSize(%rip)
# Add (Reg,3) (Ref (Arg,0)) (Imm (Word 1))
	movq	%rdi, %r13
	addq	$2, %r13
# Tclosure (Frm,0) 120 [Ref (Arg,0)]
	movq	$65537, -1(%r12)
	leaq	co120(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
# Tclosure (Frm,3) 121 [Ref (Env,0),Ref (Reg,3)]
	movq	$131073, 23(%r12)
	leaq	co121(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 39(%r12)
	movq	%r13, 47(%r12)
# Call 7 [Ref (Frm,0),Ref (Frm,3)]
	leaq	(%r12), %rdi
	leaq	24(%r12), %rsi
	jmp	co7

# primes.primes

	.globl	co120
co120:
# Call 41 [Ref (Env,0)]
	movq	15(%r11), %rdi
	jmp	co41

# primes.primes

	.globl	co121
co121:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,0)) [Ref (Env,1)]
	movq	23(%r11), %rdi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# stdlib.take

	.globl	co122
co122:
# PushCont 123 [Ref (Arg,1)]
	subq	$8, %rsp
	movq	%rsi, (%rsp)
	leaq	co123(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# stdlib.take

	.globl	co123
co123:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# Cmp (Reg,0) (Ref (Arg,0)) (Imm (Word 0))
# Jeq (Ref (Reg,0)) (Imm (Word 2)) 129
	cmpq	$0, %rdi
	jg	co129
# Call 8 []
	jmp	co8

# stdlib.take

	.globl	co124
co124:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [127,128]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co127
	.quad	co128

# stdlib.take

	.globl	co125
co125:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Sub (Reg,3) (Ref (Env,0)) (Imm (Word 1))
	movq	15(%r11), %r13
	subq	$2, %r13
# Call 41 [Ref (Reg,3)]
	movq	%r13, %rdi
	jmp	co41

# stdlib.take

	.globl	co126
co126:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 122 [Ref (Env,0),Ref (Env,1)]
	movq	15(%r11), %rdi
	movq	23(%r11), %rsi
	jmp	co122

# stdlib.take

	.globl	co127
co127:
# Call 8 []
	jmp	co8

# stdlib.take

	.globl	co128
co128:
# AllocFrame 7
0:
	movq	%rbp, %r12
	addq	$56, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$7, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$7, _FrmSize(%rip)
# Tclosure (Frm,0) 125 [Ref (Env,0)]
	movq	$65537, -1(%r12)
	leaq	co125(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 15(%r12)
# Tclosure (Frm,3) 126 [Ref (Frm,0),Ref (Arg,1)]
	movq	$131073, 23(%r12)
	leaq	co126(%rip), %rbx
	movq	%rbx, 31(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 39(%r12)
	movq	%rsi, 47(%r12)
# Call 7 [Ref (Arg,0),Ref (Frm,3)]
	leaq	24(%r12), %rsi
	jmp	co7

# stdlib.take

	.globl	co129
co129:
# PushCont 124 [Ref (Arg,0)]
	subq	$8, %rsp
	movq	%rdi, (%rsp)
	leaq	co124(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Env,0)) []
	movq	15(%r11), %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.lit_20

	.globl	co130
co130:
# Call 41 [Imm (Word 100)]
	movq	$200, %rdi
	jmp	co41

# stdlib.lit_449

	.globl	co131
co131:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 125)]
	leaq	ho125+1(%rip), %rdi
	jmp	co34

# stdlib.lit_453

	.globl	co132
co132:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 130)]
	leaq	ho130+1(%rip), %rdi
	jmp	co34

# stdlib.lit_454

	.globl	co133
co133:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 135)]
	leaq	ho135+1(%rip), %rdi
	jmp	co34

# stdlib.lit_455

	.globl	co134
co134:
# Call 14 [Imm (Word 91)]
	movq	$182, %rdi
	jmp	co14

# primes.main

	.globl	co135
co135:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 136 []
	movq	$65537, -1(%r12)
	leaq	co136(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	$0, 15(%r12)
# Call 39 [Ref (Frm,0)]
	leaq	(%r12), %rdi
	jmp	co39

# primes.main

	.globl	co136
co136:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# PushCont 137 []
	leaq	co137(%rip), %rbx
	pushq	%rbx
# Call 122 [Imm (Haddr 119),Imm (Haddr 114)]
	leaq	ho119+1(%rip), %rdi
	leaq	ho114+1(%rip), %rsi
	jmp	co122

# primes.main

	.globl	co137
co137:
# PopEnv 0
# CaseVec (Ref (Reg,0)) [148,149]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co148
	.quad	co149

# primes.main

	.globl	co138
co138:
# PushCont 139 [Ref (Env,0)]
	subq	$8, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	leaq	co139(%rip), %rbx
	pushq	%rbx
# Apply (Ref (Arg,0)) []
	movq	%rdi, %r11
	movq	$0, %r13
	movq	-1(%r11), %rax
	cmpb	$2, %al
	ja	GeneralApply
	cmpb	$0, %ah
	jne	0f
	jmp	*7(%r11)
0:
	movq	%r11, %rax
	leaq	_PackArgs0(%rip), %r10
	ret	

# primes.main

	.globl	co139
co139:
# PopEnv 1
	popq	_Env+16(%rip)
	leaq	_Env+1(%rip), %r11
# CaseVec (Ref (Reg,0)) [143,144]
	leaq	1f(%rip), %rbx
	jmp	*(%rbx, %rax, 4)
1:
	.quad	co143
	.quad	co144

# primes.main

	.globl	co140
co140:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 57 [Imm (Haddr 100),Imm (Haddr 103),Ref (Env,0)]
	leaq	ho100+1(%rip), %rdi
	leaq	ho103+1(%rip), %rsi
	movq	15(%r11), %rdx
	jmp	co57

# primes.main

	.globl	co141
co141:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,0)) [Ref (Env,1)]
	movq	23(%r11), %rdi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# primes.main

	.globl	co142
co142:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Ref (Env,0),Ref (Env,1)]
	movq	15(%r11), %rdi
	movq	23(%r11), %rsi
	jmp	co29

# primes.main

	.globl	co143
co143:
# Enter 127 []
	leaq	ho127+1(%rip), %r11
	jmp	*7(%r11)

# primes.main

	.globl	co144
co144:
# AllocFrame 11
0:
	movq	%rbp, %r12
	addq	$88, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$11, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$11, _FrmSize(%rip)
# Tclosure (Frm,0) 140 [Ref (Arg,0)]
	movq	$65537, -1(%r12)
	leaq	co140(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	%rdi, 15(%r12)
# Tclosure (Frm,3) 141 [Ref (Env,0),Ref (Arg,1)]
	movq	$131073, 23(%r12)
	leaq	co141(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	15(%r11), %rbx
	movq	%rbx, 39(%r12)
	movq	%rsi, 47(%r12)
# Tclosure (Frm,7) 142 [Ref (Frm,0),Ref (Frm,3)]
	movq	$131073, 55(%r12)
	leaq	co142(%rip), %rbx
	movq	%rbx, 63(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 71(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 79(%r12)
# Call 29 [Imm (Haddr 132),Ref (Frm,7)]
	leaq	ho132+1(%rip), %rdi
	leaq	56(%r12), %rsi
	jmp	co29

# primes.main

	.globl	co145
co145:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 57 [Imm (Haddr 100),Imm (Haddr 103),Ref (Env,0)]
	leaq	ho100+1(%rip), %rdi
	leaq	ho103+1(%rip), %rsi
	movq	15(%r11), %rdx
	jmp	co57

# primes.main

	.globl	co146
co146:
# Call 7 [Imm (Haddr 137),Ref (Env,0)]
	leaq	ho137+1(%rip), %rdi
	movq	15(%r11), %rsi
	jmp	co7

# primes.main

	.globl	co147
co147:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Env,0)) [Ref (Env,1)]
	movq	23(%r11), %rdi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# primes.main

	.globl	co148
co148:
# Enter 122 []
	leaq	ho122+1(%rip), %r11
	jmp	*7(%r11)

# primes.main

	.globl	co149
co149:
# AllocFrame 13
0:
	movq	%rbp, %r12
	addq	$104, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$13, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$13, _FrmSize(%rip)
# Fclosure (Frm,0) 138 1 [Ref (Frm,0)]
	movq	$65792, -1(%r12)
	leaq	co138(%rip), %rbx
	movq	%rbx, 7(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 15(%r12)
# Tclosure (Frm,3) 145 [Ref (Arg,0)]
	movq	$65537, 23(%r12)
	leaq	co145(%rip), %rbx
	movq	%rbx, 31(%r12)
	movq	%rdi, 39(%r12)
# Tclosure (Frm,6) 146 [Ref (Frm,3)]
	movq	$65537, 47(%r12)
	leaq	co146(%rip), %rbx
	movq	%rbx, 55(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 63(%r12)
# Tclosure (Frm,9) 147 [Ref (Frm,0),Ref (Arg,1)]
	movq	$131073, 71(%r12)
	leaq	co147(%rip), %rbx
	movq	%rbx, 79(%r12)
	leaq	(%r12), %rbx
	movq	%rbx, 87(%r12)
	movq	%rsi, 95(%r12)
# Call 29 [Ref (Frm,6),Ref (Frm,9)]
	leaq	48(%r12), %rdi
	leaq	72(%r12), %rsi
	jmp	co29

# primes.!start

	.globl	co150
co150:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# PushCont 151 []
	leaq	co151(%rip), %rbx
	pushq	%rbx
# PushCont 2 [Imm (Haddr 8)]
	subq	$8, %rsp
	leaq	ho8+1(%rip), %rbx
	movq	%rbx, (%rsp)
	leaq	ApplyToEnv1(%rip), %rbx
	pushq	%rbx
# Enter 140 []
	leaq	ho140+1(%rip), %r11
	jmp	*7(%r11)

# primes.!start

	.globl	co151
co151:
# PopEnv 0
# SystemOp 0 (Reg,0) (Imm (Word 0))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$0, %rdi
	movq	$0, %rsi
	movq	$0, %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp

# stdlib.lit_553

	.globl	co152
co152:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 34 [Imm (Haddr 149)]
	leaq	ho149+1(%rip), %rdi
	jmp	co34

# stdlib.blackHole

	.globl	co153
co153:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocStream (Reg,3) (Imm (Word 2)) (Imm (Word 32))
0:
	movq	%rbp, %r13
	addq	$264, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$33, _gcRequestSize(%rip)
	jmp	GC
0:
	movq	$2097669, (%r13)
	incq	%r13
# AllocFrame 6
0:
	movq	%rbp, %r12
	addq	$48, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$6, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$6, _FrmSize(%rip)
# Tclosure (Frm,0) 154 []
	movq	$65537, -1(%r12)
	leaq	co154(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	$0, 15(%r12)
# Fclosure (Frm,3) 156 2 [Ref (Frm,3)]
	movq	$66048, 23(%r12)
	leaq	co156(%rip), %rbx
	movq	%rbx, 31(%r12)
	leaq	24(%r12), %rbx
	movq	%rbx, 39(%r12)
# PushCont 159 []
	leaq	co159(%rip), %rbx
	pushq	%rbx
# EnterInd (Ref (Frm,3)) [Ref (Reg,3),Ref (Frm,0)]
	movq	%r13, %rdi
	leaq	(%r12), %rsi
	leaq	24(%r12), %r11
	jmp	*7(%r11)

# stdlib.blackHole

	.globl	co154
co154:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# AllocFrame 3
0:
	movq	%rbp, %r12
	addq	$24, %rbp
	cmpq	%rbp, _gcHeapTop(%rip)
	jg	0f
	leaq	0b(%rip), %rbx
	pushq	%rbx
	movq	$3, _gcRequestSize(%rip)
	jmp	GC
0:
	incq	%r12
	movq	$3, _FrmSize(%rip)
# Tclosure (Frm,0) 155 []
	movq	$65537, -1(%r12)
	leaq	co155(%rip), %rbx
	movq	%rbx, 7(%r12)
	movq	$0, 15(%r12)
# Call 29 [Imm (Haddr 42),Ref (Frm,0)]
	leaq	ho42+1(%rip), %rdi
	leaq	(%r12), %rsi
	jmp	co29

# stdlib.blackHole

	.globl	co155
co155:
# Update (Ref (Reg,2)) (Imm (Caddr 6))
	leaq	co6(%rip), %rbx
	movq	%rbx, 7(%r11)
# PushCont 1 [Ref (Reg,2)]
	subq	$8, %rsp
	movq	%r11, (%rsp)
	leaq	co1(%rip), %rbx
	pushq	%rbx
# Call 29 [Imm (Haddr 146),Imm (Haddr 42)]
	leaq	ho146+1(%rip), %rdi
	leaq	ho42+1(%rip), %rsi
	jmp	co29

# stdlib.blackHole

	.globl	co156
co156:
# PushCont 157 [Ref (Env,0),Ref (Arg,0)]
	subq	$16, %rsp
	movq	15(%r11), %rbx
	movq	%rbx, (%rsp)
	movq	%rdi, 8(%rsp)
	leaq	co157(%rip), %rbx
	pushq	%rbx
# Call 15 [Ref (Arg,0),Ref (Arg,1)]
	jmp	co15

# stdlib.blackHole

	.globl	co157
co157:
# PopEnv 2
	popq	_Env+16(%rip)
	popq	_Env+24(%rip)
	leaq	_Env+1(%rip), %r11
# Jeq (Ref (Reg,0)) (Imm (Word 0)) 158
	cmpq	$0, %rax
	je	co158
# Vclosure (Reg,3) 0
	pushq	%r11
	call	*%r10
	leaq	1(%r11), %r13
	popq	%r11
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# EnterInd (Ref (Env,0)) [Ref (Env,1),Ref (Reg,3)]
	movq	23(%r11), %rdi
	movq	%r13, %rsi
	movq	15(%r11), %r11
	jmp	*7(%r11)

# stdlib.blackHole

	.globl	co158
co158:
# SystemOp 6 (Reg,0) (Ref (Env,1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$12, %rdi
	movq	$0, %rsi
	movq	23(%r11), %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp
# Call 11 []
	jmp	co11

# stdlib.blackHole

	.globl	co159
co159:
# PopEnv 0
# SystemOp 0 (Reg,0) (Imm (Word 1))
	movq	%rdi, _Arg(%rip)
	movq	%rsi, _Arg+8(%rip)
	movq	%rdx, _Arg+16(%rip)
	movq	%rcx, _Arg+24(%rip)
	movq	%r8, _Arg+32(%rip)
	movq	%r9, _Arg+40(%rip)
	movq	%rax, _Reg(%rip)
	movq	%r10, _Reg+8(%rip)
	movq	%r11, _Reg+16(%rip)
	movq	%r13, _Reg+24(%rip)
	movq	%r14, _Reg+32(%rip)
	movq	%r15, _Reg+40(%rip)
	movq	%r12, _Frm(%rip)
	movq	%rbp, _gcNewAlloc(%rip)
	movq	%rsp, _Stk(%rip)
	subq	$160, %rsp
	andq	$-16, %rsp
	movq	$0, %rdi
	movq	$0, %rsi
	movq	$2, %rdx
	call	_systemOp
	movq	_Arg(%rip), %rdi
	movq	_Arg+8(%rip), %rsi
	movq	_Arg+16(%rip), %rdx
	movq	_Arg+24(%rip), %rcx
	movq	_Arg+32(%rip), %r8
	movq	_Arg+40(%rip), %r9
	movq	_Reg(%rip), %rax
	movq	_Reg+8(%rip), %r10
	movq	_Reg+16(%rip), %r11
	movq	_Reg+24(%rip), %r13
	movq	_Reg+32(%rip), %r14
	movq	_Reg+40(%rip), %r15
	movq	_Frm(%rip), %r12
	movq	_Stk(%rip), %rsp
	movq	_gcNewAlloc(%rip), %rbp


	.data

	.globl	_GlobalBase
_GlobalBase:
ho0:
	.quad	512
	.quad	co7

ho2:
	.quad	0
	.quad	co8

ho4:
	.quad	512
	.quad	co9

ho6:
	.quad	768
	.quad	co10

ho8:
	.quad	0
	.quad	co11

ho10:
	.quad	256
	.quad	co12

ho12:
	.quad	65537
	.quad	co13
	.quad	0

ho15:
	.quad	256
	.quad	co14

ho17:
	.quad	512
	.quad	co15

ho19:
	.quad	512
	.quad	co22

ho21:
	.quad	256
	.quad	co28

ho23:
	.quad	512
	.quad	co29

ho25:
	.quad	256
	.quad	co34

ho27:
	.quad	65537
	.quad	co38
	.quad	0

ho30:
	.quad	4295032837
	.ascii	"\n"
	.byte	0, 0, 0, 0, 0, 0, 0

ho32:
	.quad	256
	.quad	co39

ho34:
	.quad	256
	.quad	co41

ho36:
	.quad	65537
	.quad	co42
	.quad	0

ho39:
	.quad	64424640517
	.ascii	"program error: "
	.byte	0

ho42:
	.quad	65537
	.quad	co43
	.quad	0

ho45:
	.quad	4295032837
	.ascii	"\n"
	.byte	0, 0, 0, 0, 0, 0, 0

ho47:
	.quad	256
	.quad	co44

ho49:
	.quad	65537
	.quad	co52
	.quad	0

ho52:
	.quad	107374444549
	.ascii	"attempt to divide by zero"
	.byte	0, 0, 0, 0, 0, 0, 0

ho57:
	.quad	65537
	.quad	co53
	.quad	0

ho60:
	.quad	128849281029
	.ascii	"decode: character out of range"
	.byte	0, 0

ho65:
	.quad	65537
	.quad	co54
	.quad	0

ho68:
	.quad	65537
	.quad	co55
	.quad	0

ho71:
	.quad	103079411717
	.ascii	"attempt to mod with zero"

ho75:
	.quad	65537
	.quad	co56
	.quad	0

ho78:
	.quad	4295032837
	.ascii	"0"
	.byte	0, 0, 0, 0, 0, 0, 0

ho80:
	.quad	768
	.quad	co57

ho82:
	.quad	65537
	.quad	co83
	.quad	0

ho85:
	.quad	98784444421
	.ascii	"missing case in module "
	.byte	0

ho89:
	.quad	65537
	.quad	co84
	.quad	0

ho92:
	.quad	47244771333
	.ascii	" near line "
	.byte	0, 0, 0, 0, 0

ho95:
	.quad	65537
	.quad	co85
	.quad	0

ho98:
	.quad	21474902021
	.ascii	" col "
	.byte	0, 0, 0

ho100:
	.quad	65537
	.quad	co86
	.quad	0

ho103:
	.quad	65537
	.quad	co87
	.quad	0

ho106:
	.quad	65541
	.byte	0, 0, 0, 0, 0, 0, 0, 0

ho107:
	.quad	256
	.quad	co88

ho109:
	.quad	65537
	.quad	co99
	.quad	0

ho112:
	.quad	25769869317
	.ascii	"primes"
	.byte	0, 0

ho114:
	.quad	65537
	.quad	co100
	.quad	0

ho117:
	.quad	512
	.quad	co122

ho119:
	.quad	65537
	.quad	co130
	.quad	0

ho122:
	.quad	65537
	.quad	co131
	.quad	0

ho125:
	.quad	8590000133
	.ascii	"[]"
	.byte	0, 0, 0, 0, 0, 0

ho127:
	.quad	65537
	.quad	co132
	.quad	0

ho130:
	.quad	4295032837
	.ascii	"]"
	.byte	0, 0, 0, 0, 0, 0, 0

ho132:
	.quad	65537
	.quad	co133
	.quad	0

ho135:
	.quad	8590000133
	.ascii	", "
	.byte	0, 0, 0, 0, 0, 0

ho137:
	.quad	65537
	.quad	co134
	.quad	0

ho140:
	.quad	65537
	.quad	co135
	.quad	0

ho143:
	.quad	65537
	.quad	co150
	.quad	0

ho146:
	.quad	65537
	.quad	co152
	.quad	0

ho149:
	.quad	42949804037
	.ascii	"BLACK HOLE"
	.byte	0, 0, 0, 0, 0, 0

ho152:
	.quad	65537
	.quad	co153
	.quad	0


	.globl	_GlobalTop
_GlobalTop:
