#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import abc
import collections
import logging

import six
from sortedcontainers import SortedList, SortedSet, SortedDict

from .Cap import Cap
from .Object import Frame, PageTable, PageDirectory, CNode, Endpoint, \
    Notification, TCB, Untyped, IOPageTable, Object, IRQ, IOPorts, IODevice, \
    ARMIODevice, VCPU, ASIDPool, SC, SchedControl, RTReply, ObjectType, \
    ObjectRights, IOAPICIRQ, MSIIRQ, IRQControl, get_object_size, ASIDControl, \
    DomainControl, is_aligned, ARMIRQMode, ARMIRQ, ContextBank, StreamID, SMC, ARMSGISignal
from capdl.util import ctz
from .Spec import Spec


class AllocatorState(object):
    def __init__(self, obj_space, cspaces={}, pds={}, addr_spaces={}):
        self.obj_space = obj_space
        self.cspaces = cspaces
        self.pds = pds
        self.addr_spaces = addr_spaces


class RenderState(AllocatorState):
    '''
    This bolts extra CAmkES state onto the capDL allocator state.
    For now, this just tracks extra adjustments to the integrity
    policy used in cdl-refine.thy.

    FIXME: this ought to go into camkes-tool, but Python pickle
    doesn't seem to like deserialising locally defined classes.
    '''

    def __init__(self, *args, **kwargs):
        super(RenderState, self).__init__(*args, **kwargs)
        self.policy_extra = set()


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
        frame_type = [page for page in self.spec.arch.get_pages() if page == type]
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
        elif type == ObjectType.seL4_PageTableObject:
            o = PageTable(name)
        elif type == self.spec.arch.vspace().object:
            o = self.spec.arch.vspace().make_object(name)
        elif type == ObjectType.seL4_PageDirectoryObject:
            o = PageDirectory(name)
        elif type == ObjectType.seL4_IOPageTableObject:
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
        elif type == ObjectType.seL4_VCPU:
            o = VCPU(name)
        elif type == ObjectType.seL4_IRQHandler:
            notification = None
            if 'notification' in kwargs:
                notification = kwargs['notification']
                del kwargs['notification']

            if 'trigger' in kwargs or 'target' in kwargs:
                o = ARMIRQ(name, **kwargs)
            elif 'number' in kwargs:
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

            if notification is not None:
                o.set_notification(notification)
        elif type == ObjectType.seL4_ARM_SGI_Signal:
            o = ARMSGISignal(name, **kwargs)
        elif type == ObjectType.seL4_IRQControl:
            o = IRQControl(name)
        elif type == ObjectType.seL4_ASID_Control:
            o = ASIDControl(name)
        elif type == ObjectType.seL4_DomainControl:
            o = DomainControl(name)
        elif type == ObjectType.seL4_ASID_Pool:
            o = ASIDPool(name)
        elif len(frame_type) == 1:
            o = Frame(name, get_object_size(frame_type[0]), **kwargs)
        elif type == ObjectType.seL4_SchedContextObject:
            o = SC(name)
        elif type == ObjectType.seL4_SchedControl:
            core = kwargs.get('core', 0)
            o = SchedControl(name, core)
        elif type == ObjectType.seL4_RTReplyObject:
            o = RTReply(name)
        elif type == ObjectType.seL4_ARMSID:
            o = StreamID(name)
        elif type == ObjectType.seL4_ARMCB:
            o = ContextBank(name)
        elif type == ObjectType.seL4_ARMSMC:
            o = SMC(name)
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
        self.name_to_object.update({x.name: x for x in spec})

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
        self.slot = 1  # Skip the null slot

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
            assert vaddr not in self._regions, "duplicate definition: vaddr 0x%x already exists" % vaddr
            self._regions[vaddr] = (size, cap)
            vaddr += size

    def get_regions_and_clear(self):
        '''
        This is for consuming this allocator to create a PageCollection structure
        that gets merged with the main object allocator.
        This sets the internal regions structure to None to prevent usages after.
        '''
        regions = self._regions
        self._regions = None
        return regions


class AllocatorException(Exception):
    pass


class ASIDTableAllocator(object):
    """
    Assign ASID table slot numbers for ASID pools.

    While slot numbers are not visible to userland, they are visible in the
    DSpec verification model, so the capDL loader supports explicit policies
    for ASID allocation.
    """

    def allocate(self, spec):
        """
        For each ASID pool in the spec, assign it to an unused ASID table slot.
        This modifies the spec's ASID pool objects in-place.

        Slot 0 is always skipped, because it is used for the init thread's ASID pool.
        We assume that the C loader also skips slot 0.

        This allocator allows ASID pools that already have assigned asid_high numbers.
        However, seL4 only allows allocating table slots in sequential order.
        Therefore, we raise AllocatorException if the spec's asid_high numbers cannot
        be obtained by the C loader.
        """
        assert(isinstance(spec, Spec))

        num_asid_high = get_object_size(ObjectType.seL4_ASID_Table)
        free_asid_highs = SortedSet(range(num_asid_high))
        free_asid_highs.remove(0)  # Init thread's

        asid_pools = []

        # Get all ASIDPools
        for obj in spec.objs:
            if isinstance(obj, ASIDPool):
                asid_pools.append(obj)
        # Make deterministic
        asid_pools = sorted(asid_pools, key=lambda obj: obj.name)

        # Check availability of asid_highs; check existing claims
        for asid_pool in asid_pools:
            if asid_pool.asid_high is not None:
                if asid_pool.asid_high < 0 or asid_pool.asid_high >= num_asid_high:
                    raise AllocatorException("invalid asid_high of 0x%x, ASID pool %s" %
                                             (asid_pool.asid_high, asid_pool.name))
                elif asid_pool.asid_high in free_asid_highs:
                    raise AllocatorException("asid_high 0x%x already in use, can't give to ASID pool %s" %
                                             (asid_pool.asid_high, asid_pool.name))
                else:
                    free_asid_highs.remove(asid_pool.asid_high)

        # Allocate free_asid_highs
        for asid_pool in asid_pools:
            if asid_pool.asid_high is None:
                if not free_asid_highs:
                    raise AllocatorException("ran out of asid_highs to allocate (next ASID pool: %s)" %
                                             asid_pool.name)
                else:
                    asid_pool.asid_high = free_asid_highs.pop(0)

        # Check that asid_highs are contiguous
        for asid_pool in asid_pools:
            if asid_pool.asid_high > 0 and asid_pool.asid_high - 1 in free_asid_highs:
                raise AllocatorException("asid_high not contiguous: %s wants 0x%x but 0x%x not assigned" %
                                         (asid_pool.name, asid_pool.asid_high, asid_pool.asid_high - 1))


class UntypedAllocator(six.with_metaclass(abc.ABCMeta, object)):
    """
    An allocation interface for assigning objects to specific untypeds.
    Each untyped allocator implements its own policy.
    """

    @abc.abstractmethod
    def add_untyped(self, u, device=False):
        pass

    def add_device_untyped(self, u):
        self.add_untyped(u, True)

    @abc.abstractmethod
    def allocate(self, spec):
        """
        Using the untypeds added to this instance of this class via add_untyped and add_device_untyped, calculate
        untyped derivations of all objects from spec from those untyped. Throw an AllocationException if the untypeds do
        not cover the required paddrs, or if the untyped are insufficient to allocate all objects in the spec.

        This method will mutate the spec by adding the used untypeds from the UntypedAllocator as objects, and adding any
        intermediate untypeds created to maintain alignment or achieve a specific paddr in a seL4_Untyped_Retype call.
        :param spec:
        :return:
        """
        pass


class AllocQueue:

    def __init__(self, s):
        assert(isinstance(s, Spec))
        # dict of unfungible objects, sorted by paddr.
        self.unfun_objects = SortedDict(lambda x: -x)
        # dict of lists of fungible objects, indexed by size_bits
        self.objects = {}
        self.sizes = SortedList()
        # Sort objects secondarily by name, for two reasons:
        # 1. Determinism (Python sets are not ordered)
        # 2. Makes it more likely that objects of the same size will be
        #    allocated contiguously by name. For example, the CAmkES DMA
        #    allocator relies on this for large contiguous allocations.
        # Reversed because we are pushing onto a queue.
        for o in sorted(s.objs, key=lambda obj: obj.name, reverse=True):
            if hasattr(o, 'paddr') and o.paddr:
                self._push_unfun_obj(o)
            elif o.get_size_bits():
                self._push_fun(o)

    def _push_fun(self, o):
        size_bits = o.get_size_bits()
        if size_bits in self.objects:
            self.objects[size_bits].append(o)
        else:
            self.objects[size_bits] = collections.deque([o])
            self.sizes.add(size_bits)

    def pop_fun(self, size_bits):
        if size_bits in self.objects:
            popped = self.objects[size_bits].pop()
            if not len(self.objects[size_bits]):
                self.sizes.remove(size_bits)
                del self.objects[size_bits]
        return popped

    def _push_unfun_obj(self, o):
        if o.paddr in self.unfun_objects:
            old = self.unfun_objects[o.paddr]
            raise AllocatorException("Duplicate paddr 0x%x (%s, %s)" % (o.paddr, old.name, o.name))
        self.unfun_objects[o.paddr] = o

    def pop_unfun(self):
        if self.unfun_objects:
            (paddr, obj) = self.unfun_objects.popitem()
            return obj
        return None

    def max_size(self):
        return self.sizes[len(self.sizes) - 1]

    def min_size(self):
        return self.sizes[0]

    def more_unfun(self):
        return len(self.unfun_objects) > 0

    def more_fun(self, size_bits=0):
        if size_bits:
            return size_bits in self.objects and len(self.objects[size_bits])
        return len(self.objects) > 0


class BestFitAllocator(UntypedAllocator):
    """
    This allocator works using the following algorithm:

    - sort untyped objects by paddr
    - sort unfun objects by paddr
    - collate fun objects by size

    Starting from the first untyped and unfun, allocate appropriately sized fun objects
    from the untyped until the first unfun paddr is reached. Then take the next unfun object, and take
    the next untyped when the current one is full. If there are no fun objects to fill gaps with, create
    placeholder untypeds to acheive the desired retype alignment.
    """

    def __init__(self):
        self.untyped = SortedList()
        self.ut_iter = None

    @staticmethod
    def _overlap(before, after):
        assert isinstance(before, Untyped)
        assert isinstance(after, Untyped)
        assert(before.paddr <= after.paddr)
        return not (before.paddr < after.paddr and ((before.paddr + before.get_size()) < (after.paddr + after.get_size())))

    @staticmethod
    def _overlap_exception(u, overlap):
        assert isinstance(u, Untyped)
        assert isinstance(overlap, Untyped)
        raise AllocatorException("New untyped (%x <--> %x) would overlap with existing (%x <--> %x)",
                                 u.paddr, u.paddr + u.get_size(),
                                 overlap.paddr, overlap.paddr + overlap.get_size())

    def add_untyped(self, u, device=False):
        """
        Add an untyped object to the allocator. Throw an error if the untyped overlaps with existing untypeds, is
        missing a paddr, or does not have a paddr aligned to its size.
        """
        assert isinstance(u, Untyped)
        if u.paddr is None:
            raise AllocatorException("Untyped has no defined paddr")
        if not is_aligned(u.paddr, u.get_size_bits()):
            raise AllocatorException("Untyped at %x is not aligned", u.paddr)

        # check overlap
        index = self.untyped.bisect_right((u, device))
        if index > 0 and self._overlap(self.untyped[index - 1][0], u):
            self._overlap_exception(u, self.untyped[index - 1][0])
        if index < len(self.untyped) and self._overlap(u, self.untyped[index][0]):
            self._overlap_exception(u, self.untyped[index][0])

        self.untyped.add((u, device))

    @staticmethod
    def _add_placeholder(untyped, size_bits, spec):
        name = "place_holder_0x%x" % untyped.watermark_paddr()
        placeholder = Untyped(name, size_bits, untyped.watermark_paddr())
        untyped.add_child(placeholder, untyped.watermark_paddr())
        spec.add_object(placeholder)

    def _fill_from_objects(self, objects, untyped, size_bits, spec):
        if not objects.more_fun() or size_bits < objects.min_size():
            return size_bits
        if objects.more_fun(size_bits):
            # we found an object of size_bits that needs allocating, allocate it!
            untyped.add_child(objects.pop_fun(size_bits))
            return 0
        else:
            # no objects of the size we were looking for -- try for two of a smaller size
            first = self._fill_from_objects(objects, untyped, size_bits - 1, spec)
            second = self._fill_from_objects(objects, untyped, size_bits - 1, spec)
            assert(first == 0 or first == size_bits - 1)
            assert(second == 0 or second == size_bits - 1)
            if first == second:
                if first == 0:
                    # we successfully allocated all of the space
                    return 0
                elif first == size_bits - 1:
                    # we didn't allocate either successfully, return that amount for potential
                    # munging into a larger untyped
                    return size_bits
            else:
                # we only managed to allocate one of the smaller objects. Create a
                # place holder untyped to retain alignment
                self._add_placeholder(untyped, size_bits - 1, spec)
                return 0

    def _use_untyped(self, objects, untyped, size_bytes, is_device, spec):
        assert(untyped.remaining() >= size_bytes)

        while size_bytes:
            # size_bits is calculated from the minimum alignment between the goal
            # size_bytes and the watermark. If the watermark is 0 (which is valid)
            # then just take the alignment of the size_bytes as it's guaranteed to
            # be smaller.
            size_bits = min(ctz(size_bytes), ctz(untyped.watermark_paddr())
                            ) if untyped.watermark_paddr() else ctz(size_bytes)
            if is_device:
                self._add_placeholder(untyped, size_bits, spec)
            else:
                res = self._fill_from_objects(objects, untyped, size_bits, spec)
                if res == size_bits:
                    # we failed to fill from objects at all
                    self._add_placeholder(untyped, size_bits, spec)
            size_bytes -= (1 << size_bits)
            assert(size_bytes >= 0)

    def _next_ut(self):
        if not self.ut_iter:
            self.ut_iter = iter(self.untyped)
        try:
            return next(self.ut_iter)
        except StopIteration:
            raise AllocatorException("Out of untyped memory to allocate from")

    def allocate(self, s):
        assert(isinstance(s, Spec))

        if not len(s.objs):
            # nothing to do
            logging.warn("No objects to allocate")
            return

        # put the objects from spec into the order we need to complete allocation
        objects = AllocQueue(s)
        (ut, is_device) = self._next_ut()

        # allocate all of the unfun objects
        while objects.more_unfun():
            paddr, unfun = objects.unfun_objects.peekitem()

            if ut.remaining() == 0:
                # HACK: Only add untypeds where we actually allocated objects.
                # This is mainly to skip special device regions which might
                # appear in the untyped list but are not available at boot.
                if all(obj.name.startswith('place_holder_') for obj in ut.children):
                    for obj in ut.children:
                        s.objs.remove(obj)
                else:
                    s.add_object(ut)
                (ut, is_device) = self._next_ut()

            if paddr < ut.watermark_paddr():
                # we've gone past the paddr and didn't find a ut!
                raise AllocatorException(
                    "No untyped for object: {0} paddr: {1}".format(unfun.name, paddr))
            elif paddr >= ut.watermark_paddr() and \
                    (paddr + unfun.get_size()) <= (ut.paddr + ut.get_size()):
                # the object we want is in this untyped!
                self._use_untyped(objects, ut, paddr - ut.watermark_paddr(), is_device, s)
                ut.add_child(unfun, paddr)
                objects.unfun_objects.popitem()
            elif objects.more_fun():
                # no objects we want in this untyped, fill it up
                self._use_untyped(objects, ut, ut.remaining(), is_device, s)
            else:
                (ut, is_device) = self._next_ut()

        # we don't allocate device untyped from this point, so
        # if the last untyped we used was a device and has some children, add it to the new objects,
        if is_device:
            if ut.children:
                s.add_object(ut)
        elif ut.remaining() > 0:
            self._use_untyped(objects, ut, ut.remaining(), is_device, s)
            if not objects.more_fun():
                # the allocator below will not run; add ut before we finish
                s.add_object(ut)

        # now allocate the rest of the objects
        while objects.more_fun():
            (ut, is_device) = self._next_ut()
            if not is_device:
                self._use_untyped(objects, ut, ut.remaining(), is_device, s)
                s.add_object(ut)
