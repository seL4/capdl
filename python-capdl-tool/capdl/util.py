#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

"""
Various internal utility functions. Pay no mind to this file.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals

# Size of a frame and page (applies to all architectures)
FRAME_SIZE = 4096 # bytes
PAGE_SIZE = 4096 # bytes

def normalise_architecture(arch):
    # Allow the user to be a bit hazy with their architecture description, and
    # tweak it to match the values accepted by the CapDL translator.
    NORMALISATION = {
        'aarch32':'arm11',
        'arm':'arm11',
        'arm11':'arm11',
        'arm_hyp':'arm11',
        'ia32':'ia32',
        'x86':'ia32',
    }
    try:
        return NORMALISATION[arch.lower()]
    except KeyError:
        raise Exception('invalid architecture: %s' % arch)

def round_down(n, alignment=FRAME_SIZE):
    """
    Round a number down to 'alignment'.
    """
    return n // alignment * alignment

def page_sizes(arch):
    if normalise_architecture(arch) == 'arm11':
        if arch == 'arm_hyp':
            return [PAGE_SIZE,        # 4K
                    64 * 1024,        # 64K
                    2 * 1024 * 1024,  # 2M
                    32 * 1024 * 1024] # 32M

        else:
            return [PAGE_SIZE,        # 4K
                    64 * 1024,        # 64K
                    1024 * 1024,      # 1M
                    16 * 1024 * 1024] # 16M

    elif normalise_architecture(arch) == 'ia32':
        return [PAGE_SIZE,       # 4K
                4 * 1024 * 1024] # 4M

    raise NotImplementedError

def page_table_coverage(arch):
    """
    The number of bytes a page table covers.
    """
    if normalise_architecture(arch) == 'ia32':
        # On IA32 a page table covers 4M
        return 4 * 1024 * 1024
    elif normalise_architecture(arch) == 'arm11':
        if arch == 'arm_hyp':
            # In HYP mode a page table covers 2M
            return 2 * 1024 * 1024
        # On ARM a page table covers 1M
        return 1 * 1024 * 1024
    else:
        # NB: If you end up in this branch while dealing with an ELF that you
        # are reasonably sure is ARM, chances are you don't have a recent
        # enough version of pyelftools.
        raise NotImplementedError

def page_table_vaddr(arch, vaddr):
    """
    The base virtual address of a page table, derived from the virtual address
    of a location within that table's coverage.
    """
    return round_down(vaddr, page_table_coverage(arch))

def page_table_index(arch, vaddr):
    """
    The index of a page table within a containing page directory, derived from
    the virtual address of a location within that table's coverage.
    """
    return vaddr // page_table_coverage(arch)

def page_index(arch, vaddr):
    """
    The index of a page within a containing page table, derived from the
    virtual address of a location within that page.
    """
    return vaddr % page_table_coverage(arch) // PAGE_SIZE

def page_vaddr(vaddr):
    """
    The base virtual address of a page, derived from the virtual address of a
    location within that page.
    """
    return vaddr // PAGE_SIZE * PAGE_SIZE
