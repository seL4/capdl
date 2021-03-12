"
" Copyright 2017, Data61, CSIRO (ABN 41 687 119 230)
"
" SPDX-License-Identifier: BSD-2-Clause
"
"
" CapDL syntax highlighting for vim.
"
" To use this, copy to ~/.vim/syntax/ and put the following in
" ~/.vim/filetype.vim:
"
"  augroup filetypedetect
"    au BufRead,BufNewFile *.cdl setfiletype capdl
"  augroup END
"
" Note that this supports CPP commands in your CapDL as well.
"

syn keyword CapDLKeyword arch caps objects arm11 ia32
syn match CapDLIRQMap "\<irq maps\>"
syn match CapDLIRQ "\<irq\>\( maps\)\@!"
syn keyword CapDLObject notification asid_pool cnode ep frame io_device io_ports io_pt pd pt tcb ut
syn keyword CapDLAttribute addr affinity badge dom fault_ep G guard guard_size init ip max_prio prio sp R RG RX RW RWG RWX W WG WX paddr cached uncached
syn match CapDLCPP "[ \t]*#.*$"
syn match CapDLLiteral "\<\(0x\)\?[0-9]\+\(k\|M\)\?\( bits\)\?\>"
syn match CapDLLiteral "\<0x[0-f]\+\>"
syn keyword CapDLSymbolicSlot cspace vspace reply_slot caller_slot ipc_buffer_slot fault_ep_slot sc_slot temp_fault_ep_slot bound_notification bound_vcpu

syn region Foldable start="{" end="}" fold transparent

syn match CapDLMultiLineComment "\/\*\_.\{-}\*\/"
syn match CapDLSingleLineComment "\(\/\/\|--\).*$"

hi def link CapDLMultiLineComment Comment
hi def link CapDLSingleLineComment Comment
hi def link CapDLIRQMap Statement
hi def link CapDLKeyword Statement
hi def link CapDLObject Type
hi def link CapDLIRQ Type
hi def link CapDLAttribute Type
hi def link CapDLCPP PreProc
hi def link CapDLLiteral Constant
hi def link CapDLSymbolicSlot Constant
