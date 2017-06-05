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

import capdl

a = capdl.TCB('foo')
b = capdl.TCB('bar')

spec1 = capdl.Spec()
spec2 = capdl.Spec()
spec1.merge(spec2)
assert len(spec1.objs) == 0

spec1 = capdl.Spec()
spec1.add_object(a)
spec2 = capdl.Spec()
spec1.merge(spec2)
assert spec1.objs == set([a])

spec1 = capdl.Spec()
spec1.add_object(a)
spec2 = capdl.Spec()
spec2.add_object(b)
spec1.merge(spec2)
assert spec1.objs == set([a, b])
