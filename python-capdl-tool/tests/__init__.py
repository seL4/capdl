#!/usr/bin/env python
#
# Copyright 2019, Data61
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

from capdl import register_object_sizes

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

class CapdlTestCase(unittest.TestCase):

    def setUp(self):
        register_object_sizes(test_object_sizes)

