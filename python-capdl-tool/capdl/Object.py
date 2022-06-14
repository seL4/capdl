#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Definitions of kernel objects.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import abc
import math
import six
from functools import total_ordering

from aenum import Enum, Flag, unique, auto, IntEnum
import logging

# dict of all object sizes, must be registered before using any Object in this file.
# this dict is indexed by strings matching ObjectType.name
object_sizes = {}


def register_object_sizes(sizes):
    """Register the object sizes to be used when creating objects in this class"""
    global object_sizes
    object_sizes = sizes


def get_libsel4_constant(name):
    global object_sizes
    try:
        return object_sizes[name]
    except KeyError:
        if not object_sizes:
            logging.fatal("No object sizes registered!")
        logging.fatal("No value for key {}".format(name))


def get_object_size_bits(object_type):
    global object_sizes
    try:
        return object_sizes[object_type.name]
    except KeyError:
        if not object_sizes:
            logging.fatal("No object sizes registered!")
        logging.fatal("No size for object {}".format(object_type.name))


def get_object_size(object_type):
    size = get_object_size_bits(object_type)
    if not size:
        return 0
    return 1 << size


@unique
class ObjectType(Enum):
    seL4_UntypedObject = auto()
    seL4_TCBObject = auto()
    seL4_EndpointObject = auto()
    seL4_NotificationObject = auto()
    seL4_CapTableObject = auto()

    seL4_SmallPageObject = auto()
    seL4_LargePageObject = auto()
    seL4_HugePageObject = auto()
    seL4_ARM_SectionObject = auto()
    seL4_ARM_SuperSectionObject = auto()
    seL4_PageTableObject = auto()
    seL4_PageDirectoryObject = auto()

    seL4_X64_PDPT = auto()
    seL4_X64_PML4 = auto()
    seL4_IOPageTableObject = auto()
    seL4_IA32_IOPort = auto()
    seL4_IA32_IOSpace = auto()
    seL4_VCPU = auto()

    seL4_FrameObject = auto()
    seL4_IRQControl = auto()
    seL4_IRQHandler = auto()
    seL4_DomainControl = auto()

    seL4_ASID_Pool = auto()
    seL4_ASID_Control = auto()

    seL4_SchedContextObject = auto()
    seL4_SchedControl = auto()
    seL4_RTReplyObject = auto()
    seL4_ARM_IOSpace = auto()

    seL4_ARMSID = auto()
    seL4_ARMCB = auto()

    seL4_AARCH64_PGD = auto()
    seL4_AARCH64_PUD = auto()

    seL4_Slot = auto()

    # Only used by ASIDTableAllocator. Note: this counts slots, not bytes.
    seL4_ASID_Table = auto()


class ObjectRights(Flag):
    _order_ = 'seL4_NoRights seL4_CanRead seL4_CanWrite seL4_CanGrant seL4_CanGrantReply seL4_AllRights'
    seL4_NoRights = 0
    seL4_CanRead = auto()
    seL4_CanWrite = auto()
    seL4_CanGrant = auto()
    seL4_CanGrantReply = auto()
    seL4_AllRights = seL4_CanRead | seL4_CanWrite | seL4_CanGrant | seL4_CanGrantReply


class ARMIRQMode(IntEnum):
    seL4_ARM_IRQ_LEVEL = 0
    seL4_ARM_IRQ_EDGE = 1


class Object(six.with_metaclass(abc.ABCMeta, object)):
    """
    Parent of all kernel objects.
    """

    def __init__(self, name):
        self.name = name

    @abc.abstractmethod
    def get_size_bits(self):
        pass

    def get_size(self):
        return 1 << self.get_size_bits()

    def is_container(self):
        return False


class ContainerObject(six.with_metaclass(abc.ABCMeta, Object)):
    """
    Common functionality for all objects that are cap containers, in the sense
    that they may have child caps.
    """

    def __init__(self, name):
        super(ContainerObject, self).__init__(name)
        self.slots = {}

    def is_container(self):
        return True

    def print_contents(self):
        keys = self.slots.keys()
        if all(isinstance(k, six.integer_types) for k in keys):
            def print_slot_index(index): return '0x%x' % index
        elif all(isinstance(k, six.string_types) for k in keys):
            def print_slot_index(index): return index
        else:
            raise RuntimeError(
                "Object %s: slot indexes must be either all strings or all integers" % self.name)

        return '%s {\n%s\n}' % (self.name,
                                '\n'.join('%s: %s' % (print_slot_index(index), val)
                                          for index, val in sorted(self.slots.items())
                                          if val is not None))

    def __contains__(self, key):
        return key in self.slots

    def __delitem__(self, key):
        del self.slots[key]

    def __getitem__(self, key):
        return self.slots[key]

    def __setitem__(self, slot, cap):
        self.slots[slot] = cap

    def __iter__(self):
        return self.slots.__iter__()


class Frame(Object):
    def __init__(self, name, size=4096, paddr=None, fill=[], **_):
        super(Frame, self).__init__(name)
        self.size = size
        self.paddr = paddr
        self.fill = fill
        # check the size is aligned to a power of 2
        assert(self.size == (1 << self.get_size_bits()))

    def set_fill(self, fill):
        self.fill = fill

    def get_size_bits(self):
        return self.size.bit_length() - 1

    def __repr__(self):
        if self.size % (1024 * 1024) == 0:
            size = '%dM' % (self.size // 1024 // 1024)
        elif self.size % 1024 == 0:
            size = '%dk' % (self.size // 1024)
        else:
            size = str(self.size)
        return '%(name)s = frame (%(size)s%(maybepaddr)s%(maybefill)s)' % {
            'name': self.name,
            'size': size,
            'maybepaddr': (', paddr: 0x%x' % self.paddr) if self.paddr is not None else '',
            'maybefill': (', fill: [%s]' % ",".join(["{%s}" % f for f in self.fill])),
        }


class PageTable(ContainerObject):
    def __repr__(self):
        return '%s = pt' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_PageTableObject)


class PageDirectory(ContainerObject):
    def __repr__(self):
        return '%s = pd' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_PageDirectoryObject)


class PDPT(ContainerObject):
    def __repr__(self):
        return '%s = pdpt' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_X64_PDPT)


class PML4(ContainerObject):
    def __repr__(self):
        return '%s = pml4' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_X64_PML4)


class PUD(ContainerObject):
    def __repr__(self):
        return '%s = pud' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_AARCH64_PUD)


class PGD(ContainerObject):
    def __repr__(self):
        return '%s = pgd' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_AARCH64_PGD)


class ASIDPool(ContainerObject):
    def __init__(self, name, asid_high=None):
        super(ASIDPool, self).__init__(name)
        self.asid_high = asid_high

    def __repr__(self):
        s = '%s = asid_pool' % self.name
        if self.asid_high is not None:
            s += ' (asid_high: 0x%x)' % self.asid_high
        return s

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_ASID_Pool)


def calculate_cnode_size(max_slot):
    return int(math.floor(math.log(max(max_slot, 2), 2)) + 1)


def calculate_size(cnode):
    max_slot = None
    try:
        max_slot = max(cnode.slots.keys())
    except ValueError as e:
        max_slot = 0
    return calculate_cnode_size(max_slot)


class CNode(ContainerObject):
    def __init__(self, name, size_bits='auto'):
        super(CNode, self).__init__(name)
        self.size_bits = size_bits
        self.update_guard_size_caps = []

    def finalise_size(self, arch=None):
        if self.size_bits == 'auto':
            # Minimum CNode size is 1 bit. Maximum size (28 bits) is not
            # checked.
            self.size_bits = calculate_size(self)
        if arch:
            for x in self.update_guard_size_caps:
                x.set_guard_size(arch.word_size_bits() - self.size_bits)

    def get_slot_bits(self):
        if self.size_bits == 'auto':
            size_bits = calculate_size(self)
        else:
            size_bits = self.size_bits
        return size_bits

    def __repr__(self):
        return '%s = cnode (%s bits)' % (self.name, self.get_slot_bits())

    def get_size_bits(self):
        return self.get_slot_bits() + get_object_size_bits(ObjectType.seL4_Slot)


class Endpoint(Object):
    def __repr__(self):
        return '%s = ep' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_EndpointObject)


class Notification(Object):
    def __repr__(self):
        return '%s = notification' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_NotificationObject)


class TCB(ContainerObject):
    def __init__(self, name, ipc_buffer_vaddr=0x0, ip=0x0, sp=0x0,
                 prio=254, max_prio=254, affinity=0, init=None, domain=None, fault_ep_slot=None, resume=True):
        super(TCB, self).__init__(name)
        self.addr = ipc_buffer_vaddr
        self.ip = ip
        self.sp = sp
        self.prio = prio
        self.max_prio = max_prio
        self.affinity = affinity
        self.init = init or []
        self.domain = domain
        self.fault_ep_slot = fault_ep_slot
        self.resume = resume

    def __repr__(self):
        fields = [
            'addr: 0x%(addr)x',
            'ip: 0x%(ip)x',
            'sp: 0x%(sp)x',
            'prio: %(prio)s',
            'max_prio: %(max_prio)s',
            'affinity: %(affinity)s',
            'init: %(init)s'
        ]
        if self.fault_ep_slot is not None:
            fields += ['fault_ep: 0x%(fault_ep_slot)0.8x']
        if self.domain is not None:
            fields += ['dom: %(domain)d']
        if self.resume is False:
            fields += ['resume: %(resume)s']
        return ('%(name)s = tcb (' + ','.join(fields) + ')') % self.__dict__

    def set_affinity(self, affinity):
        self.affinity = affinity

    def set_fault_ep_slot(self, fault_ep_slot=0, fault_ep=None, badge=0, rights=ObjectRights.seL4_AllRights):
        if fault_ep_slot != 0:
            self.fault_ep_slot = fault_ep_slot
        if fault_ep:
            fields = []
            if badge != 0:
                fields += ['badge: %d' % badge]
            rights_list = [
                sym for sym, right in [
                    ('R', ObjectRights.seL4_CanRead),
                    ('W', ObjectRights.seL4_CanWrite),
                    ('G', ObjectRights.seL4_CanGrant),
                    ('P', ObjectRights.seL4_CanGrantReply)
                ] if right in rights
            ]
            if rights_list:
                fields += [''.join(rights_list)]
            if fields:
                fault_ep += ' (' + ','.join(fields) + ')'
            self.__setitem__("fault_ep_slot", fault_ep)

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_TCBObject)

# untypeds are ordered by their paddr, then size, then name, which makes allocation of objects from an
# untyped at specific addresses easier.


@total_ordering
class Untyped(Object):
    def __init__(self, name, size_bits=12, paddr=None):
        super(Untyped, self).__init__(name)
        self.size_bits = size_bits
        self.paddr = paddr
        self.watermark = 0
        self.children = []

    def add_child(self, child, paddr=None):
        """Add a child to this untyped. The child *must* align with the current
        watermark, and must fit into the untyped"""
        assert isinstance(child, Object)
        assert(self.remaining() >= child.get_size())
        assert(paddr is None or is_aligned(paddr, child.get_size_bits()))
        if paddr:
            assert(self.watermark_paddr() == paddr)
        self.children.append(child)
        self.watermark += child.get_size()
        return self.remaining()

    def remaining(self):
        """Return the amount of space left that can be retyped in this untyped"""
        return self.get_size() - self.watermark

    def watermark_paddr(self):
        """Return the current paddr watermark"""
        return self.watermark + self.paddr

    def __repr__(self):
        return '%(name)s = ut (%(size_bits)s bits%(maybepaddr)s) { %(kids)s }' % {
            'name': self.name,
            'size_bits': self.size_bits,
            'maybepaddr': (', paddr: 0x%x' % self.paddr) if self.paddr is not None else '',
            'kids':  ('\n'.join([k.name for k in self.children])) if self.children else ''
        }

    def get_size_bits(self):
        return self.size_bits

    def __eq__(self, other):
        return self.get_size_bits() == other.get_size_bits() and self.paddr == other.paddr and self.name == other.name

    def __lt__(self, other):
        return (self.paddr, self.size_bits, self.name) < (other.paddr, other.size_bits, other.name)

    def __hash__(self):
        return hash((self.paddr, self.size_bits, self.name))


class IOPorts(Object):
    # In the implementation there is no such thing as an IO port object, but it is
    # simpler to model it here as an actual object.
    def __init__(self, name, start_port=None, end_port=None):
        super(IOPorts, self).__init__(name)
        self.start_port = start_port
        self.end_port = end_port

    def __repr__(self):
        return '%(name)s = io_ports (ports:[%(start)s..%(end)s])' % \
            {'name': self.name,
             'start': self.start_port,
             'end': self.end_port - 1}

    def get_size_bits(self):
        return None


class IODevice(Object):
    def __init__(self, name, domainID, bus, dev, fun):
        super(IODevice, self).__init__(name)
        self.domainID = domainID
        self.bus = bus
        self.dev = dev
        self.fun = fun

    def __repr__(self):
        return '%s = io_device (domainID: %d, 0x%x:%d.%d)' % (self.name, self.domainID, self.bus, self.dev, self.fun)

    def get_size_bits(self):
        return None


class ARMIODevice(Object):
    def __init__(self, name, iospace):
        super(ARMIODevice, self).__init__(name)
        self.iospace = iospace

    def __repr__(self):
        return '%s = arm_io_device (iospace: %d)' % (self.name, self.iospace)

    def get_size_bits(self):
        return None


class IOPageTable(ContainerObject):
    def __init__(self, name, level=1):
        super(IOPageTable, self).__init__(name)
        assert level in [1, 2, 3]  # Complies with CapDL spec
        self.level = level

    def __repr__(self):
        return '%(name)s = io_pt (level: %(level)s)' % self.__dict__

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_IOPageTableObject)


class IRQ(ContainerObject):
    # In the implementation there is no such thing as an IRQ object, but it is
    # simpler to model it here as an actual object.
    def __init__(self, name, number=None):
        super(IRQ, self).__init__(name)
        self.number = number

    def set_notification(self, notification_cap):
        assert isinstance(notification_cap.referent, Notification)
        self[0] = notification_cap

    def get_size_bits(self):
        return None

    def __repr__(self):
        # Note, in CapDL this is actually represented as a 0-sized CNode.
        return '%s = irq' % self.name


class IOAPICIRQ(IRQ):
    def __init__(self, name, vector=None, ioapic=None, pin=None, level=None, polarity=None):
        super(IOAPICIRQ, self).__init__(name, number=vector)
        self.ioapic = ioapic
        self.pin = pin
        self.level = level
        self.polarity = polarity

    def __repr__(self):
        return '%s = ioapic_irq (ioapic_num:%d, ioapic_pin:%d, ioapic_level:%d, ioapic_polarity:%d)' % (self.name,
                                                                                                        self.ioapic, self.pin, self.level, self.polarity)


class MSIIRQ(IRQ):
    def __init__(self, name, vector=None, handle=None, bus=None, dev=None, fun=None):
        super(MSIIRQ, self).__init__(name, number=vector)
        self.handle = handle
        self.bus = bus
        self.dev = dev
        self.fun = fun

    def __repr__(self):
        return '%s = msi_irq (msi_handle:%d, msi_pci_bus:%d, msi_pci_dev:%d, msi_pci_fun:%d)' % (self.name,
                                                                                                 self.handle, self.bus, self.dev, self.fun)


class ARMIRQ(IRQ):
    def __init__(self, name, number, trigger=ARMIRQMode.seL4_ARM_IRQ_LEVEL, target=0):
        super(ARMIRQ, self).__init__(name, number=number)
        self.trigger = trigger
        self.target = target

    def __repr__(self):
        return '%s = arm_irq (trigger:%s, target:%d)' % (self.name, "level" if self.trigger == ARMIRQMode.seL4_ARM_IRQ_LEVEL else "edge", self.target)


class VCPU(Object):
    def __repr__(self):
        return '%s = vcpu' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_VCPU)


class SC(Object):
    def __init__(self, name, period=10000, budget=10000, data=0x0, size_bits='auto'):
        super(SC, self).__init__(name)
        self.period = period
        self.budget = budget
        self.data = data
        if size_bits == 'auto':
            size_bits = get_object_size_bits(ObjectType.seL4_SchedContextObject)
        self.size_bits = size_bits

    def __repr__(self):
        s = '%(name)s = sc (period: %(period)s, budget: %(budget)s, data: %(data)s, %(size_bits)s bits)' % self.__dict__
        return s

    def get_size_bits(self):
        return self.size_bits


class IRQControl(Object):
    def __init__(self, name):
        super(IRQControl, self).__init__(name)
        self.name = 'irq_control'

    def __repr__(self):
        # no object representation for an IRQControl
        s = ""
        return s

    def get_size_bits(self):
        return None


class ASIDControl(Object):
    def __init__(self, name):
        super(ASIDControl, self).__init__(name)
        self.name = 'asid_control'

    def __repr__(self):
        # no object representation for an ASID Control
        s = ""
        return s

    def get_size_bits(self):
        return None


class DomainControl(Object):
    def __init__(self, name):
        super(DomainControl, self).__init__(name)
        self.name = 'domain'

    def __repr__(self):
        # no object representation for an DomainControl
        s = ""
        return s

    def get_size_bits(self):
        return None


class SchedControl(Object):
    def __init__(self, name, core=0):
        super(SchedControl, self).__init__(name)
        self.core = core

    def __repr__(self):
        # no object representation for a sched control
        s = ""
        return s

    def get_size_bits(self):
        return None


class RTReply(Object):
    def __init__(self, name):
        super(RTReply, self).__init__(name)

    def __repr__(self):
        return '%s = rtreply' % self.name

    def get_size_bits(self):
        return get_object_size_bits(ObjectType.seL4_RTReplyObject)


class StreamID(Object):
    def __init__(self, name):
        super().__init__(name)

    def __repr__(self):
        return '%s = streamid' % self.name

    def get_size_bits(self):
        return None


class ContextBank(Object):
    def __init__(self, name):
        super().__init__(name)

        # Assignment of context bank numbers will evolve with use case
        self.bank = 0

    def __repr__(self):
        s = '%s = contextbank (bank: %d)' % (self.name, self.bank)
        self.bank += 1
        return s

    def get_size_bits(self):
        return None


def is_aligned(value, size_bits):
    """
    Return true if value is aligned to the provided alignment
    """
    return (value % (1 << size_bits)) == 0
