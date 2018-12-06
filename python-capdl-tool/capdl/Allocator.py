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
    ARMIODevice, VCPU, ASIDPool, SC, SchedControl, RTReply, ObjectType, \
    ObjectRights, IOAPICIRQ, MSIIRQ, PML4, IRQControl
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
        if type == ObjectType.seL4_UntypedObject:
            size_bits = kwargs.get('size_bits', 12)
            paddr = kwargs.get('paddr', None)
            assert(paddr != 0)
            o = Untyped(name, size_bits, paddr)
        elif type == ObjectType.seL4_TCBObject:
            o = TCB(name)
        elif type == ObjectType.seL4_EndpointObject:
            o = Endpoint(name)
        elif type == ObjectType.seL4_NotificationObject:
            o = Notification(name)
        elif type == ObjectType.seL4_CapTableObject:
            o = CNode(name, **kwargs)
        elif type == ObjectType.seL4_FrameObject:
            if 'size' not in kwargs:
                kwargs['size'] = 4096
            o = Frame(name, **kwargs)
        elif type == ObjectType.seL4_IA32_PageTableObject or type == ObjectType.seL4_ARM_PageTableObject:
            o = PageTable(name)
        elif type == self.spec.arch.vspace().object:
            o = self.spec.arch.vspace().make_object(name)
        elif type == ObjectType.seL4_PageDirectoryObject:
            o = PageDirectory(name)
        elif type == ObjectType.seL4_IA32_IOPageTableObject:
            o = IOPageTable(name)
        elif type == ObjectType.seL4_IA32_IOPort:
            # There is only one IOPort object in the system, which describes the entire
            # port region.
            if 'start_port' in kwargs and 'end_port' in kwargs:
                o = IOPorts(name, start_port=kwargs['start_port'], end_port=kwargs['end_port'])
            else:
                raise ValueError
        elif type == ObjectType.seL4_IA32_IOSpace:
            o = IODevice(name, **kwargs)
        elif type == ObjectType.seL4_ARM_IOSpace:
            o = ARMIODevice(name, **kwargs)
        elif type == ObjectType.seL4_IA32_VCPU:
            o = VCPU(name)
        elif type == ObjectType.seL4_IRQHandler:
            if 'number' in kwargs:
                o = IRQ(name, kwargs['number'])
            elif 'vector' in kwargs and 'ioapic' in kwargs \
                    and 'ioapic_pin' in kwargs and 'level' in kwargs and 'polarity' in kwargs:
                o = IOAPICIRQ(name, kwargs['vector'], kwargs['ioapic'], kwargs['ioapic_pin'],
                    kwargs['level'], kwargs['polarity'])
            elif 'vector' in kwargs and 'handle' in kwargs \
                    and 'pci_bus' in kwargs and 'pci_dev' in kwargs and 'pci_fun' in kwargs:
                o = MSIIRQ(name, kwargs['vector'], kwargs['handle'], kwargs['pci_bus'],
                    kwargs['pci_dev'], kwargs['pci_fun'])
            else:
                raise ValueError("IRQHandler objects must define (number|vector,ioapic,ioapic_pin,level,"
                                 "polarity|vector,handle,pci_bus,pci_dev,pci_fun)")

            if 'notification' in kwargs:
                o.set_notification(kwargs['notification'])
        elif type == ObjectType.seL4_IRQControl:
            o = IRQControl(name)
        elif type == ObjectType.seL4_ASID_Pool:
            o = ASIDPool(name)
        elif len(frame_type) == 1:
            o = Frame(name, frame_type[0].size, **kwargs)
        elif type == ObjectType.seL4_SchedContextObject:
            o = SC(name)
        elif type == ObjectType.seL4_SchedControl:
            o = SchedControl(name)
        elif type == ObjectType.seL4_RTReplyObject:
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
        or (c) some combination of the boolean parameters 'read', 'write', 'grant'
        and 'grantreply' indicating the rights of the cap.
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
                assert 'grantreply' not in kwargs
                kwargs['read'] = kwargs['rights'] & ObjectRights.seL4_CanRead > 0
                kwargs['write'] = kwargs['rights'] & ObjectRights.seL4_CanWrite > 0
                kwargs['grant'] = kwargs['rights'] & ObjectRights.seL4_CanGrant > 0
                kwargs['grantreply'] = kwargs['rights'] & ObjectRights.seL4_CanGrantReply > 0
            cap = Cap(obj, **kwargs)
        if isinstance(obj, CNode):
            obj.update_guard_size_caps.append(cap)

        self.cnode[slot] = cap
        return slot

class AddressSpaceAllocator(object):
    '''
    Structure for describing backing frame policy for an address space loaded
    by capDL.

    Currently there is a default policy that will create frame mappings
    for all elf loadable sections that will use the largest frames that are less
    or equal to the section sizes. In the future it may be possible to specify
    different policies, such as ones that minimise object count or translate
    ELF mapping attributes differently.

    For now, this structure is used to describe regions with different attributes
    than the default policy for describing things such as IPC buffers, shared memory,
    guarded symbols, unmapped symbols, DMA pools, Physical memory mappings, etc.

    A likely lifecycle of this object is to record special symbols that will appear
    in the ELF file with the frame objects to use for mapping. Then when the ELF file is
    constructed, these symbols will be translated into virtual addresses that can
    be given to the PageCollection object as it creates a spec with the provided frames.
    '''
    def __init__(self, name, vspace_root):
        self.name = name
        self.vspace_root = vspace_root
        self._symbols = {}
        self._regions = {}

    def add_symbol_with_caps(self, symbol, sizes, caps):
        '''
        Specify the caps and sizes to use for a given symbol.  Objects that the
        caps refer to should have been allocated by a ObjectAllocator otherwise
        they might not end up in the final CDL spec file.

        It should be possible for the frames to be mapped into the address space
        of the symbol in a sensible way. Therefore alignment and size of the symbol
        should be compatible with the array of frames provided.

        Examples:
        - A symbol of size 4k with a 4k Frame object should be 4K aligned in the
          elf file.
        - A Symbol of size 12k, with 3 4K Frames that have different rights, should
          be 4K aligned in the ELF file and the frames correspond to the mappings
          in order from lowest to highest.
        - A Symbol using a 1M sized frame object should be either 1M sized with
          compatible alignment, or guarantee that the ELF file will have a 1M hole
          with compatible alignment for the 1M frame to be mapped that overlaps the
          symbol.
        '''
        self._symbols[symbol] = (sizes, caps)

    def get_symbols_and_clear(self):
        '''
        This function is used for converting symbols into virtual addresses with
        an elf file. Resulting regions should be added back via add_region_with_caps
        This sets the internal symbols structure to None to prevent usages after.
        '''
        symbols = self._symbols
        self._symbols = None
        return symbols

    def add_region_with_caps(self, vaddr, sizes, caps):
        '''
        This is the same as add_symbol_with_caps but uses a specific address in
        the virtual address space instead of a symbol. At some point before the
        address space objects are created, all of the symbols should be converted
        to regions using get_symbols_and_clear and add_region_with_caps to transform
        the symbols into vaddrs with the help of an ELF file.
        '''
        assert len(sizes) == len(caps)
        for (size, cap) in zip(sizes, caps):
            self._regions[vaddr] = (size, cap)
            vaddr+=size

    def get_regions_and_clear(self):
        '''
        This is for consuming this allocator to create a PageCollection structure
        that gets merged with the main object allocator.
        This sets the internal regions structure to None to prevent usages after.
        '''
        regions = self._regions
        self._regions = None
        return regions
