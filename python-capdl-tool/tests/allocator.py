#!/usr/bin/env python
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import unittest

import hypothesis.strategies as st
from hypothesis import given

from capdl import Spec, Untyped, Endpoint, IRQControl, Frame
from capdl.Allocator import BestFitAllocator, AllocatorException
from capdl.util import round_up
from tests import CapdlTestCase


class TestAllocator(CapdlTestCase):

    def assertValidSpec(self, allocator, spec, ut_size, child_size, children, uts):
        if ut_size >= child_size:
            allocator.allocate(spec)
            # now check if all the children got allocated
            for c in children:
                if not any(c in ut.children for ut in uts):
                    self.fail("Child {0} not allocated!".format(c))
        else:
            with self.assertRaises(AllocatorException):
                allocator.allocate(spec)

    @given(st.integers(min_value=4, max_value=64), st.integers(min_value=4, max_value=64))
    def test_alloc_single_fun_obj(self, child_size_bits, ut_size_bits):
        """
        Test allocating a single object from a single untyped. Vary the untyped size and object size,
        and make sure it either succeeds (should always succeed if the untyped size is big enough) or fails
        by throwing an AllocatorException
        """
        allocator = BestFitAllocator()
        spec = Spec()

        ut = Untyped(name="test_ut", size_bits=ut_size_bits, paddr=1 << ut_size_bits)
        allocator.add_untyped(ut)

        child = Untyped(name="child_ut", size_bits=child_size_bits)
        spec.add_object(child)

        self.assertValidSpec(allocator, spec, ut_size_bits, child_size_bits, [child], [ut])

    @staticmethod
    def alloc_children(spec, object_sizes):
        sum = 0
        children = []
        for i, size_bits in enumerate(object_sizes):
            sum += (1 << size_bits)
            child = Untyped(name="child ut {0}".format(i), size_bits=size_bits)
            spec.add_object(child)
            children.append(child)
        return sum, children

    @given(st.lists(st.integers(min_value=4, max_value=64), max_size=100, min_size=10), st.integers(min_value=4, max_value=64))
    def test_alloc_multiple_fun_obj(self, object_sizes, ut_size_bits):
        """Test allocating multiple child objects from a single untyped"""
        allocator = BestFitAllocator()
        spec = Spec()
        (total_child_size, children) = TestAllocator.alloc_children(spec, object_sizes)

        ut = Untyped(name="test_ut", size_bits=ut_size_bits, paddr=1 << ut_size_bits)
        allocator.add_untyped(ut)

        self.assertValidSpec(allocator, spec, 1 << ut_size_bits, total_child_size, children, [ut])

    @given(st.lists(st.integers(min_value=4, max_value=32), max_size=100, min_size=10),
           st.lists(st.integers(min_value=32, max_value=64), max_size=20, min_size=2))
    def test_alloc_multiple_fun_multiple_untyped(self, object_sizes, ut_sizes):
        """Test allocating multiple children from multiple untyped"""
        allocator = BestFitAllocator()
        spec = Spec()
        (total_child_size, children) = TestAllocator.alloc_children(spec, object_sizes)
        untyped = []
        paddr = 0x1
        ut_size = 0

        for i in range(0, len(ut_sizes)):
            size_bits = ut_sizes[i]
            paddr = round_up(paddr, 1 << size_bits)
            ut = Untyped("untyped_{0}".format(i), size_bits=size_bits, paddr=paddr)
            untyped.append(ut)
            allocator.add_untyped(ut)
            paddr += 1 << size_bits
            ut_size += 1 << size_bits

        self.assertValidSpec(allocator, spec, ut_size, total_child_size, children, untyped)

    def test_alloc_no_spec_no_untyped(self):
        """
        Test allocating nothing from nothing works.
        """
        BestFitAllocator().allocate(Spec())

    def test_alloc_no_spec(self):
        """
        Test allocating nothing from something works
        """
        allocator = BestFitAllocator()
        allocator.add_untyped(Untyped(name="test_ut", size_bits=16, paddr=0))
        allocator.allocate(Spec())

    def test_alloc_no_untyped(self):
        """
        Test allocating something from nothing fails elegantly
        """
        ep = Endpoint(name="test_ep")
        spec = Spec()
        spec.add_object(ep)

        with self.assertRaises(AllocatorException):
            BestFitAllocator().allocate(spec)

    def test_alloc_unsized(self):
        """Test allocating an object with no size"""
        irq_ctrl = IRQControl("irq_control")
        spec = Spec()
        spec.add_object(irq_ctrl)
        allocator = BestFitAllocator()
        allocator.add_untyped(Untyped(name="test_ut", size_bits=16, paddr=0x10000))
        allocator.allocate(spec)
        self.assertTrue(irq_ctrl in spec.objs)

    @given(st.integers(min_value=0xA0, max_value=0xD0), st.integers(min_value=0xB0, max_value=0xC0))
    def test_alloc_paddr(self, unfun_paddr, ut_paddr):
        """
        Test allocating a single unfun untyped in and out of bounds of an untyped
        """

        allocator = BestFitAllocator()
        size_bits = 12

        unfun_paddr = unfun_paddr << size_bits
        ut_paddr = ut_paddr << size_bits
        unfun_end = unfun_paddr + (1 << size_bits)
        ut_end = ut_paddr + (1 << size_bits)

        parent = Untyped("parent_ut", size_bits=size_bits, paddr=ut_paddr)
        allocator.add_untyped(parent)
        spec = Spec()
        child = Untyped("child_ut", size_bits=size_bits, paddr=unfun_paddr)
        spec.add_object(child)

        if unfun_paddr >= ut_paddr and unfun_end <= ut_end:
            self.assertValidSpec(allocator, spec, size_bits, size_bits, [child], [parent])
        else:
            with self.assertRaises(AllocatorException):
                allocator.allocate(spec)

    @given(st.lists(st.integers(min_value=4, max_value=64), min_size=1), st.lists(st.integers(min_value=4, max_value=64), min_size=1))
    def test_device_ut_only(self, ut_sizes, obj_sizes):
        """
        Test allocating fun objects from only device untypeds
        """
        allocator = BestFitAllocator()
        paddr = 0x1
        for i in range(0, len(ut_sizes)):
            paddr = round_up(paddr, 1 << ut_sizes[i])
            allocator.add_device_untyped(
                Untyped("device_untyped_{0}".format(i), size_bits=ut_sizes[i], paddr=paddr))
            paddr += 1 << ut_sizes[i]

        spec = Spec()
        for i in range(0, len(obj_sizes)):
            spec.add_object(Untyped("obj_untyped_{0}".format(i), size_bits=obj_sizes[i]))

        with self.assertRaises(AllocatorException):
            allocator.allocate(spec)

    @given(st.integers(min_value=0, max_value=3))
    def test_overlapping_paddr_smaller(self, offset):
        """Test allocating unfun objects with overlapping paddrs, where the overlapping paddr is from a smaller
        object """

        paddr = 0xAAAA0000
        size_bits = 16
        overlap_paddr = paddr + offset * (1 << (size_bits-2))

        allocator = BestFitAllocator()
        allocator.add_untyped(Untyped("parent", paddr=paddr, size_bits=size_bits))

        spec = Spec()
        spec.add_object(Untyped("child", paddr=paddr, size_bits=size_bits))
        spec.add_object(Untyped("overlap_child", paddr=overlap_paddr, size_bits=size_bits-2))

        with self.assertRaises(AllocatorException):
            allocator.allocate(spec)

    def test_overlapping_paddr_larger(self):
        """Test allocating unfun objects with overlapping paddrs, where the overlapping paddr is from a larger object"""
        allocator = BestFitAllocator()

        paddr = 0xAAAA0000
        allocator.add_untyped(Untyped("deadbeef", size_bits=16, paddr=paddr))

        spec = Spec()
        spec.add_object(Untyped("obj_untyped_1", size_bits=14, paddr=paddr + 2 * (1 << 15)))
        spec.add_object(Untyped("obj_untyped_2", size_bits=15, paddr=paddr))

        with self.assertRaises(AllocatorException):
            allocator.allocate(spec)

    @given(st.lists(st.integers(min_value=4, max_value=16), min_size=1, max_size=1000))
    def test_placeholder_uts(self, sizes):
        """
        Test allocating a collection of unfun objects that do not align and have placeholder uts between them
        """
        allocator = BestFitAllocator()
        start_paddr = 1 << (max(sizes) + len(sizes).bit_length())
        paddr = start_paddr
        ut_size = 0

        children = []
        spec = Spec()
        for i in range(0, len(sizes)):
            paddr = round_up(paddr, 1 << sizes[i])
            ut = Untyped("ut_{0}".format(i), size_bits=sizes[i], paddr=paddr)
            spec.add_object(ut)
            paddr += 1 << sizes[i]
            ut_size += 1 << sizes[i]
            children.append(ut)

        ut_size_bits = (paddr - start_paddr).bit_length()
        ut = Untyped("ut_parent", size_bits=ut_size_bits, paddr=start_paddr)
        allocator.add_untyped(ut)
        self.assertValidSpec(allocator, spec, 1 << ut_size_bits, ut_size, children, [ut])

    def test_regression_unfun_at_end(self):
        """
        Ensure that if an unfun object is the last to get allocated,
        its root ut is included in the spec.
        """
        allocator = BestFitAllocator()
        root_ut_A = Untyped("root_ut_A", size_bits=16, paddr=0x10000)
        root_ut_B = Untyped("root_ut_B", size_bits=16, paddr=0x20000)
        allocator.add_untyped(root_ut_A)
        allocator.add_untyped(root_ut_B)

        spec = Spec()
        my_frame_A0 = Frame("my_frame_A0")
        my_frame_A1 = Frame("my_frame_A1")
        my_pinned_frame_B = Frame("my_pinned_frame_B", paddr=0x20000)
        spec.add_object(my_frame_A0)
        spec.add_object(my_frame_A1)
        spec.add_object(my_pinned_frame_B)

        allocator.allocate(spec)
        self.assertIn(root_ut_B, spec.objs)  # main test
        # other tests:
        for obj in (root_ut_A, root_ut_B, my_frame_A0, my_frame_A1, my_pinned_frame_B):
            self.assertIn(obj, spec.objs)
        for obj in (my_frame_A0, my_frame_A1):
            self.assertIn(obj, root_ut_A.children)
        for obj in (my_pinned_frame_B,):
            self.assertIn(obj, root_ut_B.children)


if __name__ == '__main__':
    unittest.main()
