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

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from capdl import ELF

elf = ELF('hello.bin')
assert elf.get_arch() == 'x86'

elf.get_spec()

