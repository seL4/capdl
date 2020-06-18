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

# Let's make a TCB:
tcb = capdl.TCB('my_tcb')

# Set some relevant properties of it:
tcb.ip = 0x8000
tcb.sp = 0xdeadbeef
tcb.init += [0xcafe, 0xdeaf]

# Create an endpoint:
ep = capdl.Endpoint('my_ep')

# Let's set that endpoint as the TCB's fault EP:
ep_cap = capdl.Cap(ep)
tcb['fault_ep'] = ep_cap

# Let's setup a CSpace and VSpace for the TCB:
cspace = capdl.CNode('my_cnode', 28)  # <-- size in bits
vspace = capdl.PageDirectory('my_pd')
tcb['cspace'] = capdl.Cap(cspace)
tcb['vspace'] = capdl.Cap(vspace)

# Throw in an untyped and give the thread a cap to it:
ut = capdl.Untyped('my_ut', 10)  # <-- size in bits
cspace[1] = capdl.Cap(ut)

# Let's create a spec from all this and output it:
spec = capdl.Spec()
for obj in [tcb, ep, cspace, vspace, ut]:
    spec.add_object(obj)
print(spec)
