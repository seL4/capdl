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

import unittest

from capdl import ELF, register_object_sizes

test_object_sizes = {
        'seL4_TCBObject': 9,
        'seL4_EndpointObject': 4,
        'seL4_NotificationObject': 4,
        'seL4_SmallPageObject': 12,
        'seL4_LargePageObject': 16,
        'seL4_ASID_Pool': 12,
        'seL4_Slot': 4,
        'seL4_PageTableObject': 10,
        'seL4_PageDirectoryObject': 14,
        'seL4_ARM_SectionObject': 20,
        'seL4_ARM_SuperSectionObject': 24,
        'seL4_IOPageTableObject': 12
    }

class TestElf(unittest.TestCase):

    def setUp(self):
        register_object_sizes(test_object_sizes)

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
