#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from capdl.Object import Object, Frame, CNode, Endpoint, Notification, \
    SchedControl, SMC


class Cap(object):
    """
    A capability to a kernel object.
    """

    def __init__(self, referent, **kwargs):
        assert isinstance(referent, Object)
        self.referent = referent
        self.read = False
        self.write = False
        self.grant = False
        self.grantreply = False
        self.guard = 0
        self.guard_size = 0
        self.badge = None
        self.cached = True
        self.ports = None
        self.mapping_container = None
        self.mapping_slot = None
        self.mapping_deferred = False
        for (k, v) in kwargs.items():
            if hasattr(self, k):
                setattr(self, k, v)

    def set_guard(self, guard):
        assert isinstance(self.referent, CNode)
        assert isinstance(guard, int)
        assert guard % 2**18 == guard, 'guards can be a maximum of 18 bits'
        self.guard = guard

    def set_guard_size(self, guard_size):
        assert isinstance(self.referent, CNode)
        assert isinstance(guard_size, int)
        self.guard_size = guard_size

    def set_badge(self, badge):
        # Only endpoint caps can be badged.
        assert isinstance(self.referent, Endpoint) or \
            isinstance(self.referent, Notification) or \
            isinstance(self.referent, SMC)
        assert badge % 2**28 == badge, 'badges can be a maximum of 28 bits'
        self.badge = badge

    def set_cached(self, cached):
        assert isinstance(self.referent, Frame)
        self.cached = cached

    def set_mapping(self, container, slot):
        """
        @brief      For a frame cap in a cspace, ensure that this is the
                    cap used for a mapping into a mapping object specified by
                    container and slot.

        @param      self       A frame cap that is inside a CNode.
        @param      container  The mapping object the frame is mapped into.
        @param      slot       The index of the mapping object
        """
        assert isinstance(self.referent, Frame)
        self.mapping_container = container
        self.mapping_slot = slot

    def set_mapping_deferred(self):
        """
        @brief      Indicate that this cap is a mapping cap, but the container
                    and slot values are to be set later.
        """
        assert isinstance(self.referent, Frame)
        self.mapping_deferred = True

    def __repr__(self):
        extra = []

        if isinstance(self.referent, SchedControl):
            return "sched_control (core: %d)" % self.referent.core

        if isinstance(self.referent,
                      (Frame, Endpoint, Notification, SMC)):
            is_frame = isinstance(self.referent, Frame)
            rights = [sym for sym, flag in
                      [('R', self.read),
                       ('W', self.write),
                       ('X', is_frame and self.grant),
                       ('G', not is_frame and self.grant),
                       ('P', self.grantreply)]
                      if flag]
            extra.append(''.join(rights))

        if isinstance(self.referent, Frame) and not self.cached:
            extra.append('uncached')
        if isinstance(self.referent, Frame) and \
                self.mapping_container is not None:
            extra.append('mapping: (%s, %d)' % (self.mapping_container.name, self.mapping_slot))
        if isinstance(self.referent, (Endpoint, Notification, SMC)) and \
           self.badge is not None:
            extra.append('badge: %d' % self.badge)
        if isinstance(self.referent, CNode):
            extra.append('guard: %s' % self.guard)
            extra.append('guard_size: %s' % self.guard_size)

        extra = [x for x in extra if x != '']

        return '%s%s' % (self.referent.name,
                         (' (%s)' % ', '.join(extra)) if extra else '')
