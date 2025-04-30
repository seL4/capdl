/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */


/**
 * Define entry points for all supported architectures.
 * 
 * _start is used as the entry point symbol. A stack is set
 * from an object declared in .bss and then control is jumped to
 * main().
 *
 */

#ifdef CONFIG_ARCH_AARCH64
__asm__ (
    ".section .text ; "
    ".global _start ; "
    "_start: ; "
    "    ldr x19, =__stack_top ; "
    "    add sp, x19, #0 ; "
    "    bl main ; "
    /* should not return */
    "1: ; "
    "    b 1b ; "
);

#elif CONFIG_ARCH_AARCH32

__asm__ (
    ".section .text ; "
    ".global _start ; "
	".type _start, %function ; "
    "_start: ; "
    "    adr r1, LC2 ; "
    "    ldr sp, [r1] ;"
    "    bl main ; "
    /* should not return */
    "1: ; "
    "    b 1b ; "
    ".size _start, .-_start ;"
	".type LC2, #object ; "
	"LC2: ; "
	".word __stack_top ; "
	".size LC2, . - LC2 ; "

);

#elif CONFIG_ARCH_IA32
__asm__ (
    ".section .text ; "
    ".global _start ; "
    "_start: ; "
    "    leal __stack_top, %esp ; "
    "    mov  %esp, %ebp ; "
    /*
     * GCC expects that a C function is always entered via a call
     * instruction and that the stack is 16-byte aligned before such an
     * instruction (leaving it 16-byte aligned + 1 word from the
     * implicit push when the function is entered).
     *
     * If additional items are pushed onto the stack, the stack must be
     * manually re-aligned before before pushing the arguments for the
     * call instruction to main.
     */
    "    sub  $0x8, %esp ; "
    "    push %ebp ; "
    "    push %ebx ; "
    "    call main ; "

    /* should not return */
    "1: ; "
    "    jmp  1b ; "
);


#elif CONFIG_ARCH_X86_64
__asm__ (

    ".section .text ; "
    ".global _start ; "
    "_start: ; "
    "    leaq __stack_top, %rsp ; "
    "    movq %rsp, %rbp ; "
    /*
     * GCC expects that a C function is always entered via a call
     * instruction and that the stack is 16-byte aligned before such an
     * instruction (leaving it 16-byte aligned + 1 word from the
     * implicit push when the function is entered).
     *
     * If additional items are pushed onto the stack, the stack must be
     * manually re-aligned before the call instruction to
     * main.
     */
    "    subq $0x8, %rsp ; "
    "    push %rbp ; "
    "    call main ; "

    /* should not return */
    "1: ; "
    "    jmp  1b ; "
);

#elif CONFIG_ARCH_RISCV
__asm (

    ".section .text ; "
    ".global _start ; "
    "_start: ; "

    /* Set gp for relaxation. See
     * https://www.sifive.com/blog/2017/08/28/all-aboard-part-3-linker-relaxation-in-riscv-toolchain/
     */
    ".option push ; "
    ".option norelax ; "
    "1:auipc gp, %pcrel_hi(__global_pointer$) ; "
    "  addi  gp, gp, %pcrel_lo(1b) ; "
    ".option pop ; "

    "    la sp, __stack_top ; "
    "    jal main ; "
    /* should not return */
    "1: ; "
    "    j 1b ; "

    );

#else

#error "Unsupported architecture"
#endif
__asm__ (
    ".section .bss ; "
    "__stack_base: ; "
    ".align 16 ; "
    ".space " CONFIG_CAPDL_LOADER_ROOT_STACK ";"
    "__stack_top: ; "
    );
