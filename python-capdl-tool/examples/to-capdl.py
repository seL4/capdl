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

# Load an ELF file.
elf = capdl.ELF('../tests/arm-elf/hello.bin')

# Generate CapDL.
print(elf.get_spec())
