#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Create an address space with two regions, one R and one RW.
pc = capdl.create_address_space([
    {'start': 0x00010000, 'end': 0x00015000, 'read': True},
    {'start': 0x00017000, 'end': 0x00020000, 'read': True, 'write': True},
])

# Print a CapDL spec for all the virtual address space objects.
print(pc.get_spec())
