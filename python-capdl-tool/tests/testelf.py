#!/usr/bin/env python
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import unittest

from capdl import ELF
from tests import CapdlTestCase


class TestElf(CapdlTestCase):

    def test_elf(self):
        elf = ELF('resources/arm-hello.bin')
        assert elf.get_arch() in [40, 'EM_ARM', 'ARM']
        elf.get_spec()

    def test_ia32_elf(self):
        elf = ELF('resources/ia32-hello.bin')
        assert elf.get_arch() == 'x86'

        elf.get_spec()

    def test_symbol_lookup(self):
        elf = ELF('resources/unstripped.bin')
        assert elf.get_arch() == 'x86'

        # Confirm that the address concurs with the one we get from objdump.
        assert elf.get_symbol_vaddr('_start') == 0x08048d48

        elf = ELF('resources/stripped.bin')
        assert elf.get_arch() == 'x86'

        # We shouldn't be able to get the symbol from the stripped binary.
        try:
            vaddr = elf.get_symbol_vaddr('_start')
            assert not ('Symbol lookup on a stripped binary returned _start == 0x%0.8x' % vaddr)
        except:
            # Expected
            pass


if __name__ == '__main__':
    unittest.main()
