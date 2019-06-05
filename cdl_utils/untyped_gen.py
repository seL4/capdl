#!/usr/bin/env python
#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#
"""
Generate an initial list of untyped objects based on the available physical
memory regions, the kernel image size, user image size, and devices on a platform
"""

import argparse
import logging
from collections import namedtuple, deque, defaultdict

from elftools.elf.elffile import ELFFile
from pyaml import yaml
from sortedcontainers import SortedList

from capdl import register_object_sizes
from capdl.ELF import ELF
from capdl.Object import get_object_size_bits, ObjectType
from capdl.util import ctz, valid_architectures, lookup_architecture, round_down, round_up, PAGE_SIZE

Region = namedtuple('Region', ['start', 'end'])


def create_untypeds_for_region(object_sizes, region, arch, device):
    untyped = []
    start = region.start
    end = region.end
    while start != end:
        assert (start <= end)
        size_bits = (end - start).bit_length() - 1 if start < end else arch.word_size_bits()
        align_bits = ctz(start) if start > 0 else size_bits
        size_bits = min(size_bits,
                        align_bits,
                        object_sizes['seL4_Value_MaxUntypedBits'])
        if size_bits > object_sizes['seL4_Value_MinUntypedBits']:
            untyped.append({'device': device, 'size_bits': size_bits, 'paddr': start})
        start += 1 << size_bits
    return untyped


def init_freemem(available, reserved):
    """Remove any reserved regions from available and return a new list of
    available regions that does not contain any reserved regions

    This method mirrors init_freemem in the kernel."""

    freemem = []
    available = deque(available)
    reserved = deque(reserved)
    while len(available) and len(reserved):
        if reserved[0].start == reserved[0].end:
            # reserved region is empty - skip it
            reserved.popleft()
        if available[0].start >= available[0].end:
            # skip the entire region - it's empty now after trimming
            available.popleft()
        elif reserved[0].end <= available[0].start:
            # the reserved region is below the available region - skip it
            reserved.popleft()
        elif reserved[0].start >= available[0].end:
            # the reserved region is above the available region - take the whole thing
            freemem.append(available[0])
            available.popleft()
        else:
            # the reserved region overlaps with the available region
            if reserved[0].start <= available[0].start:
                # the region overlaps with the start of the available region.
                # trim start of the available region
                available[0] = Region(min(available[0].end, reserved[0].end),
                                      available[0].end)
                reserved.popleft()
            else:
                assert reserved[0].start < available[0].end
                # take the first chunk of the available region and move
                # the start to the end of the reserved region
                freemem.append(Region(available[0].start, reserved[0].start))
                if available[0].end > reserved[0].end:
                    available[0] = Region(reserved[0].end, available[0].end)
                    reserved.popleft()
                else:
                    available.popleft()

    # no more reserved regions - add the rest
    freemem += list(available)
    return freemem


def rootserver_memory_size(num_nodes, root_cnode_size_bits, extra_bi_size_bits, arch, user_virt):
    """Compute the total size of the root server objects, and the largest object size"""
    size_bits_histogram = defaultdict(int)

    def add_size_bits(size_bits, n):
        size_bits_histogram[size_bits] += n

    def add_objects(object_type, n):
        add_size_bits(get_object_size_bits(object_type), n)

    add_objects(ObjectType.seL4_TCBObject, 1 + num_nodes)  # rootserver tcb + idle thread(s)
    add_size_bits(root_cnode_size_bits + get_object_size_bits(ObjectType.seL4_Slot), 1)  # root cnode
    add_objects(ObjectType.seL4_ASID_Pool, 1)  # asid pool
    add_objects(ObjectType.seL4_SmallPageObject, 2)  # ipc buf + boot info
    if extra_bi_size_bits:
        add_size_bits(extra_bi_size_bits, 1)  # extra boot info
    for level in arch.levels(): # paging
        add_size_bits(get_object_size_bits(level.object),
                      level.objects_for_range(user_virt.start, user_virt.end))

    num_bytes = sum(count << size_bits for size_bits, count in size_bits_histogram.items())
    return (num_bytes, max(size_bits_histogram.keys()))


def get_load_bounds(elf):
    end = 0
    start = 0xFFFFFFFFFFFFFFFF
    for s in elf.iter_segments():
        if s['p_type'] == 'PT_LOAD':
            paddr = s['p_paddr']
            memsz = s['p_memsz']
            start = min(paddr, start)
            end = max(paddr + memsz, end)
    print("ELF image: {0}<-->{1}".format(hex(start), hex(end)))
    return Region(start, end)


def get_symbol_size(elf, name):
    symbol_table = elf.get_section_by_name('.symtab')
    symbol = symbol_table.get_symbol_by_name(name)
    if not symbol:
        logging.fatal("No symbol {0}".format(name))
    return symbol['st_size']


def main(args):
    arch = lookup_architecture(args.architecture)
    addresses = yaml.load(args.input)
    object_sizes = yaml.load(args.object_sizes)
    register_object_sizes(object_sizes)

    # create the list of reserved regions. This duplicates the load_images part of the elf loader. Ultimately
    # we should feed this info to the elf loader rather than doing it dynamically
    reserved = SortedList()
    # first the kernel image
    kernel_elf = ELFFile(args.kernel_elf)
    kernel_region = get_load_bounds(kernel_elf)
    reserved.add(kernel_region)

    # now the dtb
    next_paddr = round_up(kernel_region.end, PAGE_SIZE)
    if args.dtb_size:
        reserved.add(Region(next_paddr, next_paddr + args.dtb_size))
        print("DTB: {0}<-->{1}".format(hex(next_paddr), hex(next_paddr + args.dtb_size)))

    # now we need to work out the user image size
    if args.loader:
        user_elf = ELFFile(args.loader)
        user_region = get_load_bounds(user_elf)
        capdl_object_size = get_symbol_size(user_elf, 'capdl_object')
    else:
        capdl_object_size = 0

    spec_size = args.max_spec_size
    spec_object_size = capdl_object_size

    # work out how much memory we need for the user image
    user_size, user_max_size_bits = rootserver_memory_size(args.num_nodes, args.cnode_size, args.extra_bi_size_bits,
                                                           arch, Region(args.user_virt_start, args.user_virt_end))
    user_start = round_up(args.alloc_start, 1 << user_max_size_bits)
    # create a new reserved region at the end of the user image
    reserved.add(Region(user_start, user_start + user_size))

    available = SortedList()
    for a in addresses['memory']:
        # trim to paddr-top
        start = min(a['start'], args.paddr_top)
        end = min(a['end'], args.paddr_top)
        if start != end:
            available.add(Region(start, end))

    # calculate free regions based on available + reserved regions
    freemem = init_freemem(available, reserved)

    # create untyped for each region
    untypeds = []
    for f in freemem:
        untypeds += create_untypeds_for_region(object_sizes, f, arch, False)

    # create untyped for each device untyped
    for d in addresses['devices']:
        untypeds += create_untypeds_for_region(object_sizes, Region(d['start'], d['end']), arch, True)

    # finally output the file
    yaml.dump(untypeds, args.output)


if __name__ == '__main__':
    def int_or_hex(s):
        if s.strip().startswith('0x'):
            return int(s, 0)
        else:
            return int(s)

    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=argparse.FileType('r'),
                        help='input yaml describing physical and device memory')
    parser.add_argument('--output', required=True, type=argparse.FileType('w'),
                        help='output file for generated untyped yaml')
    parser.add_argument('--linker', type=argparse.FileType('w'), help="Output file for linker")
    parser.add_argument('--object-sizes', required=True, type=argparse.FileType('r'),
                        help='Yaml file with kernel object sizes')
    parser.add_argument('--extra-bi-size-bits', default=0, type=int,
                        help='Size_bits of extra bootinfo frame (0 if none)')
    parser.add_argument('--kernel-elf', type=argparse.FileType('r'),
                        help='Kernel elf file', required=False)
    parser.add_argument('--paddr-top', required=True, type=int_or_hex,
                        help='Kernel\'s PADDR_TOP (highest usable physical memory addr)')
    parser.add_argument('--loader', type=argparse.FileType('r'),
                        help='Base loader image with empty spec.')
    parser.add_argument('--user-virt-start', required=True, type=int_or_hex,
                        help='Start virtual address of loader image')
    parser.add_argument('--user-virt-end', required=True, type=int_or_hex,
                        help='End virtual address of loader image')
    parser.add_argument('--alloc-start', type=int_or_hex,
                        help='Where the kernel starts allocating rootserver objects '
                             '(default: after last loaded image)')
    parser.add_argument('--architecture', choices=valid_architectures())
    parser.add_argument('--num-nodes', type=int, default=1,
                        help='Number of processors the kernel is configured to run')
    parser.add_argument('--cnode-size', type=int, default=12,
                        help='Size_bits slots in the root cnode')
    parser.add_argument('--dtb-size', type=int_or_hex, default=0,
                        help='DTB (device tree binary) blob size')
    parser.add_argument('--max-spec-size', type=int_or_hex, help="Max number of objects in the spec")
    parser.add_argument('--elffile', nargs='+', action='append')

    main(parser.parse_args())
