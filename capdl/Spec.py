#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

from Object import IRQ, Object, PageTable

class Spec(object):
    """
    A CapDL specification.
    """
    def __init__(self, arch='arm11'):
        self.arch = arch
        self.objs = set()

    def add_object(self, obj):
        assert isinstance(obj, Object)
        self.objs.add(obj)

    def merge(self, other):
        assert isinstance(other, Spec)
        self.objs.update(other)

    def __getitem__(self, key):
        return self.objs[key]

    def __iter__(self):
        return self.objs.__iter__()

    def __repr__(self):
        return 'arch %(arch)s\n\n' \
               'objects {\n%(objs)s\n}\n\n' \
               'caps {\n%(caps)s\n}\n\n' \
               'irq maps {\n%(irqs)s\n}' % {

            # Architecture; arm11 or ia32
            'arch':self.arch,

            # Kernel objects
            'objs':'\n'.join(map(str, self.objs)),

            # Capabilities to kernel objects
            'caps':'\n'.join(map(lambda x: x.print_contents(),
                        filter(lambda x: x.is_container(),
                         self.objs))),

            # Mapping from interrupt numbers to IRQ objects
            'irqs':'\n'.join(map(lambda x: '%d: %s' % (x.number, x.name),
                        filter(lambda x: isinstance(x, IRQ) and x.number is not None,
                        self.objs))),
        }
