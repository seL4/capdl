#!/usr/bin/env python
#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from capdl import ELF

elf = ELF('unstripped.bin')
assert elf.get_arch() == 'x86'

# Confirm that the address concurs with the one we get from objdump.
assert elf.get_symbol_vaddr('_start') == 0x08048d48

elf = ELF('stripped.bin')
assert elf.get_arch() == 'x86'

# We shouldn't be able to get the symbol from the stripped binary.
try:
    vaddr = elf.get_symbol_vaddr('_start')
    assert not ('Symbol lookup on a stripped binary returned _start == 0x%0.8x' % vaddr)
except:
    # Expected
    pass
