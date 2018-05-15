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

"""
Definitions of kernel objects.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import math, six

seL4_UntypedObject = 0
seL4_TCBObject = 1
seL4_EndpointObject = 2
seL4_NotificationObject = 3
seL4_CapTableObject = 4

seL4_ARM_SmallPageObject = 5
seL4_ARM_LargePageObject = 6
seL4_ARM_SectionObject = 7
seL4_ARM_SuperSectionObject = 8
seL4_ARM_PageTableObject = 9
seL4_ARM_PageDirectoryObject = 10

seL4_IA32_4K = 15
seL4_IA32_2M = 16
seL4_IA32_4M = 17
seL4_X64_1G = 18
seL4_IA32_PageTableObject = 19
seL4_IA32_PageDirectoryObject = 20
seL4_X64_PDPT = 21
seL4_X64_PML4 = 22
seL4_IA32_IOPageTableObject = 23
seL4_IA32_IOPort = 24
seL4_IA32_IOSpace = 25
seL4_IA32_VCPU = 26

seL4_FrameObject = 27
seL4_IRQControl = 28

seL4_PageDirectoryObject = 30
seL4_ASID_Pool = 31

seL4_SchedContextObject = 32
seL4_SchedControl = 33
seL4_RTReplyObject = 34

seL4_ARM_IOSpace = 35

seL4_CanRead = 1
seL4_CanWrite = 2
seL4_CanGrant = 4
seL4_AllRights = seL4_CanRead|seL4_CanWrite|seL4_CanGrant

class Object(object):
    """
    Parent of all kernel objects. This class is not expected to be instantiated.
    """
    def __init__(self, name):
        self.name = name

    def is_container(self):
        return False

class ContainerObject(Object):
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
        def slot_index(index):
            """
            Print a slot index in a sensible way.
            """
            if index is None:
                return ''
            elif isinstance(index, six.integer_types):
                return '%s: ' % reliable_hex(index)
            else:
                assert isinstance(index, six.string_types), \
                        "Slot index is of type %s. Slots indices must be either strings or integers."
                return '%s: ' % index

        return '%s {\n%s\n}' % (self.name,
            '\n'.join(sorted('%s%s' % (slot_index(x[0]), x[1])
                for x in self.slots.items() if x[1] is not None)))

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

# Hex does not produce porcelain output across architectures due to
# difference between Int and Long types and tacking an L on the end in
# such cases. We do not want a distinguishing letter at the end since
# we have asked for a hex value
def reliable_hex(val) :
    return hex(val).rstrip('L')

class Frame(Object):
    def __init__(self, name, size=4096, paddr=None, fill='', **_):
        super(Frame, self).__init__(name)
        self.size = size
        self.paddr = paddr
        self.fill = fill

    def set_fill(self, fill):
        self.fill = fill

    def __repr__(self):
        if self.size % (1024 * 1024) == 0:
            size = '%dM' % (self.size // 1024 // 1024)
        elif self.size % 1024 == 0:
            size = '%dk' % (self.size // 1024)
        else:
            size = str(self.size)
        return '%(name)s = frame (%(size)s%(maybepaddr)s%(maybefill)s)' % {
            'name':self.name,
            'size':size,
            'maybepaddr':(', paddr: %s' % reliable_hex(self.paddr)) if self.paddr is not None else '',
            'maybefill':(', fill: {%s}' % self.fill) if self.fill != '' else '',
        }

class PageTable(ContainerObject):
    def __repr__(self):
        return '%s = pt' % self.name

class PageDirectory(ContainerObject):
    def __repr__(self):
        return '%s = pd' % self.name

class PDPT(ContainerObject):
    def __repr__(self):
        return '%s = pdpt' % self.name

class PML4(ContainerObject):
    def __repr__(self):
        return '%s = pml4' % self.name

class ASIDPool(ContainerObject):
    def __repr__(self):
        return '%s = asid_pool' % self.name

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

    def finalise_size(self):
        if self.size_bits == 'auto':
            # Minimum CNode size is 1 bit. Maximum size (28 bits) is not
            # checked.
            self.size_bits = calculate_size(self)

    def __repr__(self):
        if self.size_bits == 'auto':
            size_bits = calculate_size(self)
        else:
            size_bits = self.size_bits
        return '%s = cnode (%s bits)' % (self.name, size_bits)

class Endpoint(Object):
    def __repr__(self):
        return '%s = ep' % self.name

class Notification(Object):
    def __repr__(self):
        return '%s = notification' % self.name

class TCB(ContainerObject):
    def __init__(self, name, ipc_buffer_vaddr=0x0, ip=0x0, sp=0x0, elf=None,
            prio=254, max_prio=254, affinity=0, init=None, domain=None, fault_ep_slot=None):
        super(TCB, self).__init__(name)
        self.addr = ipc_buffer_vaddr
        self.ip = ip
        self.sp = sp
        self.elf = elf or ''
        self.prio = prio
        self.max_prio = max_prio
        self.affinity = affinity
        self.init = init or []
        self.domain = domain
        self.fault_ep_slot = fault_ep_slot

    def __repr__(self):
        fields = [
            'addr: 0x%(addr)x',
            'ip: 0x%(ip)x',
            'sp: 0x%(sp)x',
            'elf: %(elf)s',
            'prio: %(prio)s',
            'max_prio: %(max_prio)s',
            'affinity: %(affinity)s',
            'init: %(init)s'
        ]
        if self.fault_ep_slot is not None:
            fields += ['fault_ep: 0x%(fault_ep_slot)0.8x']
        if self.domain is not None:
            fields += ['dom: %(domain)d']
        return ('%(name)s = tcb (' + ','.join(fields) + ')') % self.__dict__

    def set_affinity(self, affinity):
        self.affinity = affinity

    def set_fault_ep_slot(self, fault_ep_slot=0, fault_ep=None, badge=0):
        if fault_ep_slot != 0:
            self.fault_ep_slot = fault_ep_slot
        if fault_ep:
            if badge != 0:
                fault_ep += " (badge: %d)" % badge
            self.__setitem__("fault_ep_slot", fault_ep)

class Untyped(Object):
    def __init__(self, name, size_bits=12, paddr=None):
        super(Untyped, self).__init__(name)
        self.size_bits = size_bits
        self.paddr = paddr

    def __repr__(self):
        return '%(name)s = ut (%(size_bits)s bits%(maybepaddr)s)' % {
            'name': self.name,
            'size_bits': self.size_bits,
            'maybepaddr':(', paddr: %s' % reliable_hex(self.paddr)) if self.paddr is not None else '',
        }

class IOPorts(Object):
    # In the implementation there is no such thing as an IO port object, but it is
    # simpler to model it here as an actual object.
    def __init__(self, name, start_port=None, end_port=None):
        super(IOPorts, self).__init__(name)
        self.start_port = start_port
        self.end_port = end_port

    def __repr__(self):
        return '%(name)s = io_ports (ports:[%(start)s..%(end)s])' % \
            {'name':self.name,
             'start':self.start_port,
             'end':self.end_port - 1}

class IODevice(Object):
    def __init__(self, name, domainID, bus, dev, fun):
        super(IODevice, self).__init__(name)
        self.domainID = domainID
        self.bus = bus
        self.dev = dev
        self.fun = fun

    def __repr__(self):
        return '%s = io_device (domainID: %d, 0x%x:%d.%d)' % (self.name, self.domainID, self.bus, self.dev, self.fun)

class  ARMIODevice(Object):
    def __init__(self, name, iospace):
        super(ARMIODevice, self).__init__(name)
        self.iospace = iospace

    def __repr__(self):
        return '%s = arm_io_device (iospace: %d)' % (self.name, self.iospace)

class IOPageTable(ContainerObject):
    def __init__(self, name, level=1):
        super(IOPageTable, self).__init__(name)
        assert level in [1, 2, 3] # Complies with CapDL spec
        self.level = level

    def __repr__(self):
        return '%(name)s = io_pt (level: %(level)s)' % self.__dict__

class IRQ(ContainerObject):
    # In the implementation there is no such thing as an IRQ object, but it is
    # simpler to model it here as an actual object.
    def __init__(self, name, number=None):
        super(IRQ, self).__init__(name)
        self.number = number

    def set_notification(self, notification_cap):
        assert isinstance(notification_cap.referent, Notification)
        self[0] = notification_cap

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
        return '%s = ioapic_irq (ioapic_num:%d, ioapic_pin:%d, ioapic_level:%d, ioapic_polarity:%d)' % (self.name, \
            self.ioapic, self.pin, self.level, self.polarity)

class MSIIRQ(IRQ):
    def __init__(self, name, vector=None, handle=None, bus=None, dev=None, fun=None):
        super(MSIIRQ, self).__init__(name, number=vector)
        self.handle = handle
        self.bus = bus
        self.dev = dev
        self.fun = fun

    def __repr__(self):
        return '%s = msi_irq (msi_handle:%d, msi_pci_bus:%d, msi_pci_dev:%d, msi_pci_fun:%d)' % (self.name, \
            self.handle, self.bus, self.dev, self.fun)

class VCPU(Object):
    def __repr__(self):
        return '%s = vcpu' % self.name

class SC(Object):
    def __init__(self, name, period=10000, budget=10000, data=0x0, size_bits='auto'):
        super(SC, self).__init__(name)
        self.period = period
        self.budget = budget
        self.data = data
        if size_bits == 'auto':
            size_bits = 8
        self.size_bits = size_bits

    def __repr__(self):
        s = '%(name)s = sc (period: %(period)s, budget: %(budget)s, data: %(data)s, %(size_bits)s bits)' % self.__dict__
        return s

class SchedControl(Object):
    def __init__(self, name, core=0):
        super(SchedControl, self).__init__(name)
        self.core = core

    def __repr__(self):
        # no object representation for a sched control
        s = ""
        return s

class RTReply(Object):
    def __init__(self, name):
        super(RTReply, self).__init__(name)

    def __repr__(self):
        return '%s = rtreply' % self.name
        return s
