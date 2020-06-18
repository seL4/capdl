#!/usr/bin/env python
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import capdl
from tests import CapdlTestCase


class TestSpec(CapdlTestCase):

    def test_merge(self):
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
