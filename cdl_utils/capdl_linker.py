#!/usr/bin/env python
#
# Copyright 201*, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

from simpleeval import EvalWithCompoundTypes
from capdl.Object import register_object_sizes
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

    for (e, key) in targets:
        name = os.path.basename(e)
        elf = ELF(e, name, architecture)
        cspace = cspaces[key]

        # Avoid inferring a TCB as we've already created our own.
        elf_spec = elf.get_spec(infer_tcb=False, infer_asid=False,
                                pd=addr_spaces[key].vspace_root, addr_space=addr_spaces[key])
        obj_space.merge(elf_spec, label=key)
        for (slot, tcb) in [(k, v.referent) for (k, v) in cspace.cnode.slots.items()
                            if v is not None and isinstance(v.referent, TCB)]:
            if tcb['cspace'] and tcb['cspace'].referent is not cspace.cnode:
                # We exclude TCBs that refer to a different CSpace
                continue
            funcs = {"get_vaddr": lambda x: elf.get_symbol_vaddr(x)}
            s = EvalWithCompoundTypes(functions=funcs)
            tcb.ip = s.eval(str(tcb.ip))
            tcb.sp = s.eval(str(tcb.sp))
            tcb.addr = s.eval(str(tcb.addr))
            tcb.init = s.eval(str(tcb.init))
            tcb.elf = name
            if not args.fprovide_tcb_caps:
                del space.cnode[slot]
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
    parser_b.add_argument('--save-object-state', type=argparse.FileType('w'))

    args = parser.parse_args()
    register_object_sizes(yaml.load(args.object_sizes, Loader=yaml.FullLoader))

    if args.which is "build_cnode":
        data = yaml.load(args.manifest_in, Loader=yaml.FullLoader)
        assert 'cap_symbols' in data and 'region_symbols' in data, "Invalid file format"
        elfs = [item for sublist in args.elffile for item in sublist]
        cspaces = [item for sublist in args.ccspace for item in sublist]
        targets = zip(elfs, cspaces)
        manifest(data['cap_symbols'], data['region_symbols'], args.architecture, targets)
        return 0

    if args.which is "gen_cdl":
        allocator_state = pickle.load(args.manifest_in)
        elfs = [item for sublist in args.elffile for item in sublist]
        keys = [item for sublist in args.keys for item in sublist]
        targets = zip(elfs, keys)
        obj_space = final_spec(args, allocator_state.obj_space, allocator_state.cspaces,
                               allocator_state.addr_spaces, targets, args.architecture)

        args.outfile.write(repr(obj_space.spec))
        if args.save_object_state:
            pickle.dump(allocator_state, args.save_object_state)

    return 0


if __name__ == '__main__':
    sys.exit(main())
