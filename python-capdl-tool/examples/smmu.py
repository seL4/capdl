#
# Copyright 2026, DornerWorks
#
# SPDX-License-Identifier: BSD-2-Clause
#

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Create an allocator for kernel objects.
obj_allocator = capdl.ObjectAllocator()

# Let's create a CNode for the SMMU driver.
smmu_cnode = obj_allocator.alloc(capdl.ObjectType.seL4_CapTableObject,
                                 name='smmu_cnode', size_bits=4)

# Let's create some SMMU stream IDs. These are fixed by the hardware, so
# each one must be given its stream ID when it is allocated.
sids = [obj_allocator.alloc(capdl.ObjectType.seL4_ARMSID, name, streamid=sid)
        for name, sid in [('sid_a', 4), ('sid_b', 9), ('sid_c', 12)]]

# Let's give the CNode a cap to each stream ID.
cap_allocator = capdl.CSpaceAllocator(smmu_cnode)
for sid in sids:
    cap_allocator.alloc(sid)

# Now create a spec and print it out to show what we did.
spec = capdl.Spec(arch='aarch64')
spec.add_object(smmu_cnode)
for sid in sids:
    spec.add_object(sid)
print(str(spec))
