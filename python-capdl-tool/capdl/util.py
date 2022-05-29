#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Various internal utility functions. Pay no mind to this file.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import abc

import six
from six.moves import range

from .Object import ObjectType, PageTable, PageDirectory, PML4, PDPT, PGD, PUD, get_object_size

# Size of a frame and page (applies to all architectures)
FRAME_SIZE = 4096  # bytes
PAGE_SIZE = 4096  # bytes

SIZE_64K = 64 * 1024
SIZE_1M = 1024 * 1024
SIZE_2M = 2 * SIZE_1M
SIZE_4M = 4 * SIZE_1M
SIZE_16M = 16 * SIZE_1M
SIZE_32M = 32 * SIZE_1M
SIZE_1GB = 1 * 1024 * 1024 * 1024
SIZE_4GB = 4 * SIZE_1GB


class Level:
    """
    Abstraction of a 'level' in a virtual address space hierarchy. A level
    is parameterized by things such as; the virtual address range covered
    by this object, the virtual address range covered by each entry in
    the level and what kinds of frames (if any) can be placed directly
    at this level.
    """

    def __init__(self, coverage, pages, object, make_object, type_name, parent=None, child=None):
        self.coverage = coverage
        self.pages = pages
        self.parent = parent
        self.child = child
        self.object = object
        self.make_object = make_object
        self.type_name = type_name

    def base_vaddr(self, vaddr):
        """
        Base address of the range covered by this level, determined
        by using a virtual address from somewhere inside this object
        """
        return round_down(vaddr, self.coverage)

    def parent_index(self, vaddr):
        """
        Index of this object in the parent level. Determine by a
        using a virtual address from inside this object
        """
        return self.parent.child_index(vaddr)

    def child_index(self, vaddr):
        """
        Index of a child object that is contained in this object.
        Determine the index by using a combination of our coverage
        and coverage of child objects. If we have no child assume
        our 'child' is a standard page
        """
        if self.child is None:
            return vaddr % self.coverage // PAGE_SIZE
        else:
            return vaddr % self.coverage // self.child.coverage


def make_levels(levels):
    assert levels is not None
    assert len(levels) > 0
    for i in range(0, len(levels), 1):
        if i > 0:
            levels[i].parent = levels[i - 1]
        if i < len(levels) - 1:
            levels[i].child = levels[i + 1]
    return levels[0]


class Arch(six.with_metaclass(abc.ABCMeta, object)):
    def get_pages(self):
        level = self.vspace()
        pages = []
        while level is not None:
            pages.extend(level.pages)
            level = level.child
        return pages

    def vspace(self):
        return make_levels(self.levels())

    @abc.abstractmethod
    def levels(self):
        pass


class IA32Arch(Arch):
    def capdl_name(self):
        return "ia32"

    def levels(self):
        return [
            Level(SIZE_4GB, [ObjectType.seL4_LargePageObject],
                  ObjectType.seL4_PageDirectoryObject, PageDirectory, "pd"),
            Level(SIZE_4M, [ObjectType.seL4_SmallPageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
        ]

    def word_size_bits(self):
        return 32

    def ipc_buffer_size(self):
        return 512


class X64Arch(Arch):
    def capdl_name(self):
        return "x86_64"

    def levels(self):
        return [
            Level(2 ** 48, [], ObjectType.seL4_X64_PML4, PML4, "pml4"),
            Level(2 ** 39, [ObjectType.seL4_HugePageObject],
                  ObjectType.seL4_X64_PDPT, PDPT, "pdpt"),
            Level(2 ** 30, [ObjectType.seL4_LargePageObject],
                  ObjectType.seL4_PageDirectoryObject, PageDirectory, "pd"),
            Level(2 ** 21, [ObjectType.seL4_SmallPageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
        ]

    def word_size_bits(self):
        return 64

    def ipc_buffer_size(self):
        return 1024


class ARM32Arch(Arch):
    def __init__(self, hyp=False):
        self.hyp = hyp

    def capdl_name(self):
        return "arm11"

    def levels(self):
        return [
            Level(SIZE_4GB, [ObjectType.seL4_ARM_SectionObject, ObjectType.seL4_ARM_SuperSectionObject],
                  ObjectType.seL4_PageDirectoryObject, PageDirectory, "pd"),
            Level(SIZE_2M if self.hyp else SIZE_1M, [
                  ObjectType.seL4_SmallPageObject, ObjectType.seL4_LargePageObject], ObjectType.seL4_PageTableObject, PageTable, "pt"),
        ]

    def word_size_bits(self):
        return 32

    def ipc_buffer_size(self):
        return 512


class AARCH64Arch(Arch):
    def capdl_name(self):
        return "aarch64"

    def levels(self):
        # When on 40-bit PA size on aarch64-hyp the PGD level doesn't exist.
        if get_object_size(ObjectType.seL4_AARCH64_PGD):
            return [
                Level(2 ** 48, [], ObjectType.seL4_AARCH64_PGD, PGD, "pgd"),
                Level(2 ** 39, [ObjectType.seL4_HugePageObject],
                      ObjectType.seL4_AARCH64_PUD, PUD, "pud"),
                Level(2 ** 30, [ObjectType.seL4_LargePageObject],
                      ObjectType.seL4_PageDirectoryObject, PageDirectory, "pd"),
                Level(2 ** 21, [ObjectType.seL4_SmallPageObject],
                      ObjectType.seL4_PageTableObject, PageTable, "pt"),
            ]
        else:
            return [Level(2 ** 39, [ObjectType.seL4_HugePageObject], ObjectType.seL4_AARCH64_PUD, PUD, "pud"),
                    Level(2 ** 30, [ObjectType.seL4_LargePageObject],
                          ObjectType.seL4_PageDirectoryObject, PageDirectory, "pd"),
                    Level(2 ** 21, [ObjectType.seL4_SmallPageObject],
                          ObjectType.seL4_PageTableObject, PageTable, "pt"),
                    ]

    def word_size_bits(self):
        return 64

    def ipc_buffer_size(self):
        return 1024


class RISCV64Arch(Arch):
    def capdl_name(self):
        return "riscv"

    def levels(self):
        return [
            Level(2 ** 39, [ObjectType.seL4_HugePageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
            Level(2 ** 30, [ObjectType.seL4_LargePageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
            Level(2 ** 21, [ObjectType.seL4_SmallPageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
        ]

    def word_size_bits(self):
        return 64

    def ipc_buffer_size(self):
        return 1024


class RISCV32Arch(Arch):
    def capdl_name(self):
        return "riscv"

    def levels(self):
        return [
            Level(2 ** 32, [ObjectType.seL4_LargePageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
            Level(2 ** 22, [ObjectType.seL4_SmallPageObject],
                  ObjectType.seL4_PageTableObject, PageTable, "pt"),
        ]

    def word_size_bits(self):
        return 32

    def ipc_buffer_size(self):
        return 512


# Support for ARMv6 has been removed from seL4 in early 2022. However, support
# for "arm11" is kept here, because this name is used in the CapDL specification
# for AARCH32 configurations. Updating this is a low priority task, because it
# is a lot of work with not much gain (except cleaning up legacy), Also, keeping
# the name there isn't causing any issues.
CAPDL_SUPPORTED_ARCHITECTURES = {
    # <name>:  [arch_obj_ctor, <alias_list>]
    'aarch32': [lambda: ARM32Arch(),         ['arm', 'arm11']],
    'arm_hyp': [lambda: ARM32Arch(hyp=True), []],
    'aarch64': [lambda: AARCH64Arch(),       []],
    'ia32':    [lambda: IA32Arch(),          ['x86']],
    'x86_64':  [lambda: X64Arch(),           []],
    'riscv32': [lambda: RISCV32Arch(),       []],
    'riscv64': [lambda: RISCV64Arch(),       []],
}


def valid_architectures():
    return set(CAPDL_SUPPORTED_ARCHITECTURES.keys())


def lookup_architecture(arch):
    arch_normalised = arch.lower()
    for name, ctx in CAPDL_SUPPORTED_ARCHITECTURES.items():
        arch_alias_list = ctx[1]
        if (arch_normalised == name) or (arch_normalised in arch_alias_list):
            arch_obj_ctor = ctx[0]
            return arch_obj_ctor()
    raise Exception('invalid architecture: %s' % arch)


def round_down(n, alignment=FRAME_SIZE):
    """
    Round a number down to 'alignment'.
    """
    return n // alignment * alignment


def round_up(n, alignment):
    """
    Round a number up to 'alignment'
    """
    return round_down(n + alignment-1, alignment)


def last_level(level):
    while level.child is not None:
        level = level.child
    return level


def page_sizes(arch):
    if isinstance(arch, six.string_types):
        arch = lookup_architecture(arch)
    list = [get_object_size(page) for page in arch.get_pages()]
    list.sort()
    return list


def page_table_coverage(arch):
    """
    The number of bytes a page table covers.
    """
    return last_level(lookup_architecture(arch).vspace()).coverage


def page_table_vaddr(arch, vaddr):
    """
    The base virtual address of a page table, derived from the virtual address
    of a location within that table's coverage.
    """
    return last_level(lookup_architecture(arch).vspace()).base_vaddr(vaddr)


def page_table_index(arch, vaddr):
    """
    The index of a page table within a containing page directory, derived from
    the virtual address of a location within that table's coverage.
    """
    return last_level(lookup_architecture(arch).vspace()).parent_index(vaddr)


def page_index(arch, vaddr):
    """
    The index of a page within a containing page table, derived from the
    virtual address of a location within that page.
    """
    return last_level(lookup_architecture(arch).vspace()).child_index(vaddr)


def page_vaddr(vaddr):
    """
    The base virtual address of a page, derived from the virtual address of a
    location within that page.
    """
    return vaddr // PAGE_SIZE * PAGE_SIZE


def ctz(size_bytes):
    """
    Count trailing zeros in a python integer.
    The value must be greater than 0.
    """
    assert(size_bytes > 0)
    assert(isinstance(size_bytes, six.integer_types))
    low = size_bytes & -size_bytes
    low_bit = -1
    while low:
        low = low >> 1
        low_bit += 1
    return low_bit
