#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

# Add the root directory of this repository to your PYTHONPATH environment
# variable to enable the following import.
import capdl

# Let's make a TCB:
tcb_a = capdl.TCB('tcb_a', domain=0)
tcb_b = capdl.TCB('tcb_b', domain=1)

# Let's create a spec from all this and output it:
spec = capdl.Spec(arch="aarch64")
for obj in [tcb_a, tcb_b]:
    spec.add_object(obj)


# domain, duration
spec.add_schedule_item(0, 1000)
spec.add_schedule_item(1, 1000, capdl.DomainDurationUnit.Us)

print(spec)
