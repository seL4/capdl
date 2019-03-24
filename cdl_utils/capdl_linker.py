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
from jinja2 import Environment, BaseLoader, FileSystemLoader

from capdl import ELF, lookup_architecture, TCB
from capdl.Object import register_object_sizes
from simpleeval import EvalWithCompoundTypes

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
            data = template.render({'slots': cap_symbols[name], 'symbols': region_symbols[name], 'progname': name, 'ipc_buffer_symbol': "mainIpcBuffer"})
            ccspace.write(data)

def final_spec(cspaces, obj_space, addr_spaces, elf_files, architecture):
    """
    Generates a final CapDL spec file that can be given to a capdl loader application
    """
    arch = lookup_architecture(architecture)

    for e in [item for sublist in elf_files for item in sublist]:
        name = os.path.basename(e)
        elf = ELF(e, name, architecture)
        cspace = cspaces[name]

        # Avoid inferring a TCB as we've already created our own.
        elf_spec = elf.get_spec(infer_tcb=False, infer_asid=False,pd=addr_spaces[name].vspace_root, addr_space=addr_spaces[name])
        obj_space.merge(elf_spec)
        for (slot, tcb) in [(k, v.referent) for (k, v) in cspace.cnode.slots.items()
                if v is not None and isinstance(v.referent, TCB)]:
            funcs = {"get_vaddr": lambda x: elf.get_symbol_vaddr(x)}
            s = EvalWithCompoundTypes(functions=funcs)
            tcb.ip = s.eval(str(tcb.ip))
            tcb.sp = s.eval(str(tcb.sp))
            tcb.addr = s.eval(str(tcb.addr))
            tcb.init = s.eval(str(tcb.init))
            tcb.elf = name
        cspace.cnode.finalise_size(arch=arch)

    return obj_space

def main():
    parser = argparse.ArgumentParser(
                description="")
    parser.add_argument('--architecture', '--arch', default='aarch32',
        type=lambda x: type('')(x).lower(), choices=('aarch32', 'arm_hyp', 'ia32', 'x86_64', 'aarch64'),
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
    args = parser.parse_args()
    register_object_sizes(yaml.load(args.object_sizes))

    if args.which is "build_cnode":
        data = yaml.load(args.manifest_in)
        assert 'cap_symbols' in data and 'region_symbols' in data, "Invalid file format"
        elfs = [item for sublist in args.elffile for item in sublist]
        cspaces = [item for sublist in args.ccspace for item in sublist]
        targets = zip(elfs, cspaces)
        manifest(data['cap_symbols'], data['region_symbols'], args.architecture, targets)
        return 0

    if args.which is "gen_cdl":
        allocator_state = pickle.load(args.manifest_in)
        obj_space = final_spec(allocator_state.cspaces, allocator_state.obj_space, allocator_state.addr_spaces, args.elffile, args.architecture)
        args.outfile.write(repr(obj_space.spec))

    return 0

if __name__ == '__main__':
    sys.exit(main())
