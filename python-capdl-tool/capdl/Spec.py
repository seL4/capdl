#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from .Object import IRQ, Object
from .util import lookup_architecture


class Spec(object):
    """
    A CapDL specification.
    """

    def __init__(self, arch='arm11'):
        self.arch = arch
        self.objs = set()
        # empty list means no schedule will be set
        self.schedule = []
        # None means default (= 0), -1 means no domain_set_start will be performed
        self.domain_set_start = None
        # None means default (= 0)
        self.index_shift = None

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

    def add_schedule_item(self, domain, duration):
        self.schedule.append((domain, duration))

    def add_schedule(self, schedule):
        self.schedule.extend(schedule)

    def merge(self, other):
        assert isinstance(other, Spec)
        self.objs.update(other)
        assert (not self.schedule or not other.schedule or self.schedule == other.schedule)
        assert (self.domain_set_start is None or other.domain_set_start is None or
                self.domain_set_start == other.domain_set_start)
        assert (self.index_shift is None or other.index_shift is None or
                self.index_shift == other.index_shift)
        self.schedule = self.schedule or other.schedule
        self.domain_set_start = self.domain_set_start or other.domain_set_start
        self.index_shift = self.index_shift or other.index_shift

    def __iter__(self):
        return self.objs.__iter__()

    def show_schedule(self):
        if not self.schedule:
            return ''
        items = ', '.join('(%d, %d)' % (d, t) for d, t in self.schedule)
        set_start = self.domain_set_start or 0
        return '\ndomains {\n' \
               '  schedule: [%s]\n' \
               '  domain_set_start: %s\n' \
               '  index_shift: %d\n' \
               '}\n' % (items,
                        'no_start' if set_start == -1 else str(set_start),
                        self.index_shift or 0)

    def __repr__(self):
        return 'arch %(arch)s\n\n' \
               'objects {\n%(objs)s\n}\n\n' \
               'caps {\n%(caps)s\n}\n\n' \
               'irq maps {\n%(irqs)s\n}\n' \
               '%(domains)s' % {

                   # Architecture; arm11 or ia32
                   'arch': self.arch.capdl_name(),

                   # Kernel objects
                   'objs': '\n'.join(sorted(str(x) for x in self.objs)),

                   # Capabilities to kernel objects
                   'caps': '\n'.join(sorted(
                       x.print_contents() for x in self.objs if x.is_container())),

                   # Mapping from interrupt numbers to IRQ objects
                   'irqs': '\n'.join(sorted(
                       '%d: %s' % (x.number, x.name) for x in self.objs
                       if isinstance(x, IRQ) and x.number is not None)),

                   'domains': self.show_schedule()
               }
