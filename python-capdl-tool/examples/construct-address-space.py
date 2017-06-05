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

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Create an address space with two regions, one R and one RW.
pc = capdl.create_address_space([
    {'start':0x00010000, 'end':0x00015000, 'read':True},
    {'start':0x00017000, 'end':0x00020000, 'read':True, 'write':True},
    ])

# Print a CapDL spec for all the virtual address space objects.
print(pc.get_spec())
