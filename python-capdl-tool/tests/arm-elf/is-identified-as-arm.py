#!/usr/bin/env python
#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

from capdl import ELF

elf = ELF('hello.bin')
assert elf.get_arch() in [40, 'EM_ARM', 'ARM']

elf.get_spec()
