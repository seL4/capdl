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

from .Object import IRQ, Object, PageTable
from .util import lookup_architecture

class Spec(object):
    """
    A CapDL specification.
    """
    def __init__(self, arch='arm11'):
        self.arch = arch
        self.objs = set()

    @property
    def arch(self):
        return self._arch

    @arch.setter
    def arch(self, value):
        if value is None:
            self._arch = None
        else:
            self._arch = lookup_architecture(value)

    def add_object(self, obj):
        assert isinstance(obj, Object)
        self.objs.add(obj)

    def merge(self, other):
        assert isinstance(other, Spec)
        self.objs.update(other)

    def __iter__(self):
        return self.objs.__iter__()

    def __repr__(self):
        return 'arch %(arch)s\n\n' \
               'objects {\n%(objs)s\n}\n\n' \
               'caps {\n%(caps)s\n}\n\n' \
               'irq maps {\n%(irqs)s\n}' % {

            # Architecture; arm11 or ia32
            'arch':self.arch.capdl_name(),

            # Kernel objects
            'objs':'\n'.join(sorted(str(x) for x in self.objs)),

            # Capabilities to kernel objects
            'caps':'\n'.join(sorted(
                x.print_contents() for x in self.objs if x.is_container())),

            # Mapping from interrupt numbers to IRQ objects
            'irqs':'\n'.join(sorted(
                '%d: %s' % (x.number, x.name) for x in self.objs
                    if isinstance(x, IRQ) and x.number is not None)),
        }
