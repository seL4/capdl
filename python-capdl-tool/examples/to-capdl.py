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

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Load an ELF file.
elf = capdl.ELF('../tests/arm-elf/hello.bin')

# Generate CapDL.
print(elf.get_spec())
