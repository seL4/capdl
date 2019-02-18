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

from . import Object

class Cap(object):
    """
    A capability to a kernel object.
    """
    def __init__(self, referent, **kwargs):
        assert isinstance(referent, Object.Object)
        self.referent = referent
        fields = {
            'read': False,
            'write': False,
            'grant': False,
            'grantreply': False,
            'guard': 0,
            'guard_size': 0,
            'badge': None,
            'cached': True,
            'ports': None,
            'mapping_container': None,
            'mapping_slot': None,
        }
        for k, default in fields.items():
            setattr(self, k, kwargs.get(k, default))

    def set_guard(self, guard):
        assert isinstance(self.referent, Object.CNode)
        assert isinstance(guard, int)
        assert guard % 2**18 == guard, 'guards can be a maximum of 18 bits'
        self.guard = guard

    def set_guard_size(self, guard_size):
        assert isinstance(self.referent, Object.CNode)
        assert isinstance(guard_size, int)
        self.guard_size = guard_size

    def set_badge(self, badge):
        # Only endpoint caps can be badged.
        assert isinstance(self.referent, Object.Endpoint) or \
            isinstance(self.referent, Object.Notification)
        assert badge % 2**28 == badge, 'badges can be a maximum of 28 bits'
        self.badge = badge

    def set_cached(self, cached):
        assert isinstance(self.referent, Object.Frame)
        self.cached = cached

    def set_mapping(self, container, slot):
        assert isinstance(self.referent, Object.Frame)
        self.mapping_container = container
        self.mapping_slot = slot

    def __repr__(self):
        extra = []

        if isinstance(self.referent, Object.SchedControl):
            return "sched_control (core: %d)" % self.referent.core

        if isinstance(self.referent,
                      (Object.Frame, Object.Endpoint, Object.Notification)):
            is_frame = isinstance(self.referent, Object.Frame)
            rights = [ sym for sym, flag in
                       [ ('R', self.read),
                         ('W', self.write),
                         ('X', is_frame and self.grant),
                         ('G', not is_frame and self.grant),
                         ('P', self.grantreply) ]
                       if flag ]
            extra.append(''.join(rights))
        else:
            assert not any((self.read, self.write, self.grant, self.grantreply))

        if isinstance(self.referent, Object.Frame) and not self.cached:
            extra.append('uncached')
        if isinstance(self.referent, Object.Frame) and \
            self.mapping_container is not None:
            extra.append('mapping: (%s, %d)' % (self.mapping_container.name, self.mapping_slot))
        if isinstance(self.referent, (Object.Endpoint, Object.Notification)) and \
           self.badge is not None:
            extra.append('badge: %d' % self.badge)
        if isinstance(self.referent, Object.CNode):
            extra.append('guard: %s' % self.guard)
            extra.append('guard_size: %s' % self.guard_size)

        extra = [x for x in extra if x != '']

        return '%s%s' % (self.referent.name,
                         (' (%s)' % ', '.join(extra)) if extra else '')
