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

# The module Allocator.py contains an object and CSpace allocator. This example
# shows how these can be used for convenience.

from __future__ import absolute_import, division, print_function, \
    unicode_literals

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Create an allocator for kernel objects.
obj_allocator = capdl.ObjectAllocator()

# Let's create a CNode. Note that the allocation type constants are named to
# match the enums in the kernel.
my_cnode = obj_allocator.alloc(capdl.seL4_CapTableObject, size_bits=28)

# Now let's allocate some objects and caps to them in this CNode using a CSpace
# allocator.
cap_allocator = capdl.CSpaceAllocator(my_cnode)
my_tcb = obj_allocator.alloc(capdl.seL4_TCBObject)
tcb_slot = cap_allocator.alloc(my_tcb)
cnode_slot = cap_allocator.alloc(my_cnode)

# Now create a spec and print it out to show what we did.
spec = capdl.Spec()
spec.add_object(my_cnode)
spec.add_object(my_tcb)
print(str(spec))
