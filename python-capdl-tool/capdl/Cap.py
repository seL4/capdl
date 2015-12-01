#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

import Object

class Cap(object):
    """
    A capability to a kernel object.
    """
    def __init__(self, referent, read=False, write=False, grant=False):
        assert isinstance(referent, Object.Object)
        self.referent = referent
        self.read = read
        self.write = write
        self.grant = grant
        self.guard = 0
        self.guard_size = 0
        self.badge = None
        self.cached = True
        self.ports = None
        self.mapping_container = None
        self.mapping_slot = None

    def set_guard(self, guard):
        assert isinstance(self.referent, Object.CNode)
        assert isinstance(guard, int)
        assert guard & 0x3ffff == guard, 'guards can be a maximum of 18 bits'
        self.guard = guard

    def set_guard_size(self, guard_size):
        assert isinstance(self.referent, Object.CNode)
        assert isinstance(guard_size, int)
        assert guard_size & 0x1f == guard_size, \
            'guard sizes can be maximum of 5 bits'
        self.guard_size = guard_size

    def set_badge(self, badge):
        # Only endpoint caps can be badged.
        assert isinstance(self.referent, Object.Endpoint) or \
            isinstance(self.referent, Object.Notification)
        assert badge & 0xfffffff == badge, 'badges can be a maximum of 28 bits'
        self.badge = badge

    def set_cached(self, cached):
        assert isinstance(self.referent, Object.Frame)
        self.cached = cached

    def set_ports(self, ports):
        assert isinstance(self.referent, Object.IOPorts)
        self.ports = ports

    def set_mapping(self, container, slot):
        assert isinstance(self.referent, Object.Frame)
        self.mapping_container = container
        self.mapping_slot = slot

    def __repr__(self):
        extra = []

        if isinstance(self.referent, Object.Frame) or \
           isinstance(self.referent, Object.Endpoint) or \
           isinstance(self.referent, Object.Notification):
            extra.append('%s%s%s' %
                ('R' if self.read else '',
                 'W' if self.write else '',
                 'X' if self.grant else ''))
        if isinstance(self.referent, Object.Frame) and not self.cached:
            extra.append('uncached')
        if isinstance(self.referent, Object.Frame) and \
            self.mapping_container is not None:
            extra.append('mapping: (%s, %d)' % (self.mapping_container.name, self.mapping_slot))
        if (isinstance(self.referent, Object.Endpoint) or
            isinstance(self.referent, Object.Notification)) and \
           self.badge is not None:
            extra.append('badge: %d' % self.badge)
        if isinstance(self.referent, Object.CNode):
            extra.append('guard: %s' % self.guard)
            extra.append('guard_size: %s' % self.guard_size)
        if isinstance(self.referent, Object.IOPorts):
            #TODO: Partially support ranges, see CapDL spec for complete ranges semantic.
            assert self.ports
            extra.append('ports: [%s..%s]' % (self.ports[0], self.ports[-1]))

        extra = [x for x in extra if x != '']

        return '%s%s' % (self.referent.name,
                         (' (%s)' % ', '.join(extra)) if extra else '')
