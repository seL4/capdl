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

from .Object import Frame, PageTable, PageDirectory, CNode, Endpoint, \
    Notification, TCB, Untyped, IOPageTable, Object, IRQ, IOPorts, IODevice, \
    VCPU, ASIDPool, SC, SchedControl, RTReply, \
    seL4_UntypedObject, seL4_TCBObject, seL4_EndpointObject, \
    seL4_NotificationObject, seL4_CapTableObject, seL4_ARM_SmallPageObject, seL4_ARM_LargePageObject, \
    seL4_ARM_SectionObject, seL4_ARM_SuperSectionObject, seL4_ARM_PageTableObject, \
    seL4_ARM_PageDirectoryObject, seL4_IA32_4K, seL4_IA32_4M, seL4_IA32_PageTableObject, \
    seL4_IA32_PageDirectoryObject, seL4_IA32_IOPageTableObject, seL4_IA32_IOPort, \
    seL4_IA32_IOSpace, seL4_IA32_VCPU, seL4_FrameObject, seL4_IRQControl, \
    seL4_ARM_IOSpace, ARMIODevice, \
    seL4_PageDirectoryObject, seL4_ASID_Pool, seL4_SchedContextObject, seL4_SchedControl, \
    seL4_RTReplyObject, \
    seL4_CanRead, seL4_CanWrite, seL4_CanGrant, seL4_AllRights, IOAPICIRQ, MSIIRQ, PML4
from .Spec import Spec
from .Cap import Cap
import collections, os

class ObjectAllocator(object):
    '''
    An offline object allocator. This can be useful for incrementally
    generating a CapDL spec.
    '''

    def __init__(self, prefix='obj'):
        self.prefix = prefix
        self.counter = 0
        self.spec = Spec()
        self.labels = collections.defaultdict(set)
        self.name_to_object = {}

    def _assign_label(self, label, obj):
        self.labels[label].add(obj)

    def _remove_label(self, obj):
        for l, objs in self.labels.items():
            if obj in objs:
                self.labels[l].remove(obj)
                break

    def relabel(self, label, obj):
        self._remove_label(obj)
        self._assign_label(label, obj)

    def alloc(self, type, name=None, label=None, **kwargs):
        if name is None:
            name = '%s%d' % (self.prefix, self.counter)

        o = self.name_to_object.get(name)
        if o is not None:
            assert o in self.labels.get(label, set()), \
                'attempt to allocate object %s under a new, differing label' % o.name
            return o

        self.counter += 1
        frame_type = [page for page in self.spec.arch.get_pages() if page.object == type]
        if type == seL4_UntypedObject:
            size_bits = kwargs.get('size_bits', 12)
            paddr = kwargs.get('paddr', None)
            assert(paddr != 0)
            o = Untyped(name, size_bits, paddr)
        elif type == seL4_TCBObject:
            o = TCB(name)
        elif type == seL4_EndpointObject:
            o = Endpoint(name)
        elif type == seL4_NotificationObject:
            o = Notification(name)
        elif type == seL4_CapTableObject:
            o = CNode(name, **kwargs)
        elif type == seL4_FrameObject:
            if 'size' not in kwargs:
                kwargs['size'] = 4096
            o = Frame(name, **kwargs)
        elif type == seL4_IA32_PageTableObject or type == seL4_ARM_PageTableObject:
            o = PageTable(name)
        elif type == self.spec.arch.vspace().object:
            o = self.spec.arch.vspace().make_object(name)
        elif type == seL4_PageDirectoryObject:
            o = PageDirectory(name)
        elif type == seL4_IA32_IOPageTableObject:
            o = IOPageTable(name)
        elif type == seL4_IA32_IOPort:
            # There is only one IOPort object in the system, which describes the entire
            # port region.
            if 'start_port' in kwargs and 'end_port' in kwargs:
                o = IOPorts(name, start_port=kwargs['start_port'], end_port=kwargs['end_port'])
            else:
                raise ValueError
        elif type == seL4_IA32_IOSpace:
            o = IODevice(name, **kwargs)
        elif type == seL4_ARM_IOSpace:
            o = ARMIODevice(name, **kwargs)
        elif type == seL4_IA32_VCPU:
            o = VCPU(name)
        elif type == seL4_IRQControl:
            if 'number' in kwargs and 'notification' in kwargs:
                o = IRQ(name, kwargs['number'])
                o.set_notification(kwargs['notification'])
            elif 'vector' in kwargs and 'notification' in kwargs and 'ioapic' in kwargs \
                    and 'ioapic_pin' in kwargs and 'level' in kwargs and 'polarity' in kwargs:
                o = IOAPICIRQ(name, kwargs['vector'], kwargs['ioapic'], kwargs['ioapic_pin'],
                    kwargs['level'], kwargs['polarity'])
                o.set_notification(kwargs['notification'])
            elif 'vector' in kwargs and 'notification' in kwargs and 'handle' in kwargs \
                    and 'pci_bus' in kwargs and 'pci_dev' in kwargs and 'pci_fun' in kwargs:
                o = MSIIRQ(name, kwargs['vector'], kwargs['handle'], kwargs['pci_bus'],
                    kwargs['pci_dev'], kwargs['pci_fun'])
                o.set_notification(kwargs['notification'])
            else:
                raise ValueError
        elif type == seL4_ASID_Pool:
            o = ASIDPool(name)
        elif len(frame_type) == 1:
            o = Frame(name, frame_type[0].size, **kwargs)
        elif type == seL4_SchedContextObject:
            o = SC(name)
        elif type == seL4_SchedControl:
            o = SchedControl(name)
        elif type == seL4_RTReplyObject:
            o = RTReply(name)
        else:
            raise Exception('Invalid object type %s' % type)
        self.spec.add_object(o)
        self.name_to_object[name] = o
        self._assign_label(label, o)
        return o

    def merge(self, spec, label=None):
        assert isinstance(spec, Spec)
        self.spec.merge(spec)
        [self._assign_label(label, x) for x in spec.objs]
        self.name_to_object.update({x.name:x for x in spec})

    def __contains__(self, item):
        return item in self.name_to_object

    def __getitem__(self, name):
        return self.name_to_object[name]

    def __iter__(self):
        return self.spec.__iter__()

    def remove(self, o):
        self._remove_label(o)
        self.spec.objs.remove(o)
        del self.name_to_object[o.name]

class CSpaceAllocator(object):
    '''
    An offline CSpace allocator. Note that this is only capable of allocating
    from a single level CSpace.
    '''

    def __init__(self, cnode):
        assert isinstance(cnode, CNode)
        self.cnode = cnode
        self.slot = 1 # Skip the null slot

    def alloc(self, obj, **kwargs):
        '''
        Allocate a cap in the next available slot, referencing the given
        object. The caller is expected to pass either (a) no extra parameters
        indicating a cap with no rights, (b) the extra parameter 'rights' set
        to one of 0/seL4_CanRead/seL4_CanWrite/seL4_CanGrant/seL4_AllRights,
        or (c) some combination of the boolean parameters 'read', 'write' and
        'grant' indicating the rights of the cap.
        '''
        assert isinstance(obj, Object) or obj is None

        while self.slot in self.cnode.slots:
            # Skip slots the caller may have manually populated.
            self.slot += 1
        if self.cnode.size_bits != 'auto' and self.slot > (1 << self.cnode.size_bits) - 1:
            # Ran out of space in the CNode.
            return -1
        slot = self.slot
        self.slot += 1
        if obj is None:
            # The caller requested just a free slot.
            cap = None
        else:
            if 'rights' in kwargs:
                assert 'read' not in kwargs
                assert 'write' not in kwargs
                assert 'grant' not in kwargs
                read = kwargs['rights'] & seL4_CanRead > 0
                write = kwargs['rights'] & seL4_CanWrite > 0
                grant = kwargs['rights'] & seL4_CanGrant > 0
            else:
                read = kwargs.get('read', False)
                write = kwargs.get('write', False)
                grant = kwargs.get('grant', False)
            cap = Cap(obj, read=read, write=write, grant=grant)
        self.cnode[slot] = cap
        return slot
