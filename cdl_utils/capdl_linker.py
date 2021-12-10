#!/usr/bin/env python
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from capdl.Object import register_object_sizes, Untyped
from capdl.Allocator import ASIDTableAllocator, BestFitAllocator
from capdl import ELF, lookup_architecture, TCB, valid_architectures
from jinja2 import Environment, BaseLoader, FileSystemLoader
import sys
import argparse
import pickle
import logging
import os
import tempfile
import pkg_resources
import yaml
import six
pkg_resources.require("jinja2>=2.10")


CSPACE_TEMPLATE_FILE = os.path.join(os.path.dirname(__file__), "templates/cspace.template.c")


def manifest(cap_symbols, region_symbols, architecture, targets):
    """
    Generates a c file from CSPACE_TEMPLATE_FILE with some runtime information
    about CSpace slots and special address ranges
    """
    temp_file = open(CSPACE_TEMPLATE_FILE, 'r').read()
    template = Environment(loader=BaseLoader).from_string(temp_file)

    for (e, ccspace) in targets:
        name = os.path.basename(e)
        if ccspace:
            data = template.render(
                {'slots': cap_symbols[name], 'symbols': region_symbols[name], 'progname': name, 'ipc_buffer_symbol': "mainIpcBuffer"})
            ccspace.write(data)


def final_spec(args, obj_space, cspaces, addr_spaces, targets, architecture):
    """
    Generates a final CapDL spec file that can be given to a capdl loader application
    """
    arch = lookup_architecture(architecture)

    for e, key in targets:
        name = os.path.basename(e)
        elf = ELF(e, name, architecture)
        cspace = cspaces[key]

        # Avoid inferring a TCB as we've already created our own.
        elf_spec = elf.get_spec(infer_tcb=False, infer_asid=False,
                                pd=addr_spaces[key].vspace_root, addr_space=addr_spaces[key])
        obj_space.merge(elf_spec, label=key)
        for slot, v in cspace.cnode.slots.items():
            if v is not None and isinstance(v.referent, TCB):
                tcb = v.referent
                if 'cspace' in tcb and tcb['cspace'] and tcb['cspace'].referent is not cspace.cnode:
                    # We exclude TCBs that refer to a different CSpace
                    continue
                funcs = {"get_vaddr": lambda x: elf.get_symbol_vaddr(x)}
                tcb.ip = eval(str(tcb.ip), {"__builtins__": None}, funcs)
                tcb.sp = eval(str(tcb.sp), {"__builtins__": None}, funcs)
                tcb.addr = eval(str(tcb.addr), {"__builtins__": None}, funcs)
                tcb.init = eval(str(tcb.init), {"__builtins__": None}, funcs)
                if not args.fprovide_tcb_caps:
                    del cspace.cnode[slot]
        cspace.cnode.finalise_size(arch=arch)
    return obj_space


def main():
    parser = argparse.ArgumentParser(
        description="")
    parser.add_argument('--architecture', '--arch', default='aarch32',
                        type=lambda x: type('')(x).lower(), choices=valid_architectures(),
                        help='Target architecture.')
    parser.add_argument('--object-sizes', required=True, type=argparse.FileType('r'))
    subparsers = parser.add_subparsers()
    parser_a = subparsers.add_parser('build_cnode')
    parser_a.add_argument('--ccspace', nargs='+', type=argparse.FileType('w'), action='append')
    parser_a.set_defaults(which="build_cnode")
    parser_a.add_argument('--manifest-in', type=argparse.FileType('rb'))
    parser_a.add_argument('--elffile', nargs='+', action='append')
    parser_b = subparsers.add_parser('gen_cdl')
    parser_b.add_argument('--outfile', type=argparse.FileType('w'))
    parser_b.set_defaults(which="gen_cdl")
    parser_b.add_argument('--manifest-in', type=argparse.FileType('rb'))
    parser_b.add_argument('--elffile', nargs='+', action='append')
    parser_b.add_argument('--keys', nargs='+', action='append')
    parser_b.add_argument('--fprovide-tcb-caps', action='store_true',
                          default=True, help='Hand out TCB caps to components, allowing them to '
                          'exit cleanly.')
    parser_b.add_argument('--fno-provide-tcb-caps', action='store_false',
                          dest='fprovide_tcb_caps', help='Do not hand out TCB caps, causing '
                          'components to fault on exiting.')
    parser_b.add_argument('--save-object-state', type=argparse.FileType('wb'))
    parser_b.add_argument('--static-alloc', action='store_true',
                          help='Perform static object allocation (requires --untyped)')
    parser_b.add_argument('--dynamic-alloc', action='store_false', dest='static_alloc',
                          help='Cancel --static-alloc')
    parser_b.add_argument('--untyped', type=argparse.FileType('r'),
                          help="YAML file with available seL4 bootinfo untypeds")

    args = parser.parse_args()
    register_object_sizes(yaml.load(args.object_sizes, Loader=yaml.FullLoader))

    if args.which == "build_cnode":
        data = yaml.load(args.manifest_in, Loader=yaml.FullLoader)
        assert 'cap_symbols' in data and 'region_symbols' in data, "Invalid file format"
        elfs = [item for sublist in args.elffile for item in sublist]
        cspaces = [item for sublist in args.ccspace for item in sublist]
        targets = zip(elfs, cspaces)
        manifest(data['cap_symbols'], data['region_symbols'], args.architecture, targets)
        return 0

    if args.which == "gen_cdl":
        if args.static_alloc and not args.untyped:
            parser.error('--static-alloc requires --untyped')

        allocator_state = pickle.load(args.manifest_in)
        elfs = [item for sublist in args.elffile for item in sublist]
        keys = [item for sublist in args.keys for item in sublist]
        targets = zip(elfs, keys)
        obj_space = final_spec(args, allocator_state.obj_space, allocator_state.cspaces,
                               allocator_state.addr_spaces, targets, args.architecture)

        # Calculate final layout for objects and ASID slots...
        ASIDTableAllocator().allocate(obj_space.spec)
        if args.static_alloc:
            alloc = BestFitAllocator()
            for ut in yaml.load(args.untyped, Loader=yaml.FullLoader):
                if len(ut):
                    is_device, paddr, size_bits = ut['device'], ut['paddr'], ut['size_bits']
                    alloc.add_untyped(Untyped("root_untyped_0x%x" % paddr,
                                              size_bits=size_bits, paddr=paddr), is_device)
            alloc.allocate(obj_space.spec)

        args.outfile.write(repr(obj_space.spec))
        if args.save_object_state:
            pickle.dump(allocator_state, args.save_object_state)

    return 0


if __name__ == '__main__':
    sys.exit(main())
