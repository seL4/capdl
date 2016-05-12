#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
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
seL4_IA32_4M = 16
seL4_IA32_PageTableObject = 17
seL4_IA32_PageDirectoryObject = 18
seL4_IA32_IOPageTableObject = 19
seL4_IA32_IOPort = 20
seL4_IA32_IOSpace = 21
seL4_IA32_VCPU = 22

seL4_FrameObject = 25
seL4_IRQControl = 26

seL4_PageDirectoryObject = 30
seL4_ASID_Pool = 31

seL4_SchedContextObject = 32
seL4_SchedControlObject = 33

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
            elif isinstance(index, int):
                return '%s: ' % hex(index)
            else:
                assert isinstance(index, six.string_types)
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
    def __init__(self, name, size=4096, paddr=0):
        super(Frame, self).__init__(name)
        self.size = size
        self.paddr = paddr

    def __repr__(self):
        if self.size % (1024 * 1024) == 0:
            size = '%dM' % (self.size // 1024 // 1024)
        elif self.size % 1024 == 0:
            size = '%dk' % (self.size // 1024)
        else:
            size = str(self.size)
        return '%(name)s = frame (%(size)s%(maybepaddr)s)' % {
            'name':self.name,
            'size':size,
            'maybepaddr':(', paddr: %s' % reliable_hex(self.paddr)) if self.paddr != 0 else '',
        }

class PageTable(ContainerObject):
    def __repr__(self):
        return '%s = pt' % self.name

class PageDirectory(ContainerObject):
    def __repr__(self):
        return '%s = pd' % self.name

class ASIDPool(ContainerObject):
    def __repr__(self):
        return '%s = asid_pool' % self.name

def calculate_cnode_size(max_slot):
    return int(math.floor(math.log(max(max_slot, 2), 2)) + 1)

def calculate_size(cnode):
    return calculate_cnode_size(max(cnode.slots.keys()))

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
            prio=254, max_prio=254, crit=3, max_crit=3, init=None, domain=None, fault_ep_slot=None):
        super(TCB, self).__init__(name)
        self.addr = ipc_buffer_vaddr
        self.ip = ip
        self.sp = sp
        self.elf = elf or ''
        self.prio = prio
        self.max_prio = max_prio
        self.crit = crit
        self.max_crit = max_crit
        self.init = init or []
        self.domain = domain
        self.fault_ep_slot = fault_ep_slot

    def __repr__(self):
        s = '%(name)s = tcb (addr: 0x%(addr)x, ip: 0x%(ip)x, sp: 0x%(sp)x, elf: %(elf)s, prio: %(prio)s, \
               max_prio: %(max_prio)s, crit: %(crit)s, max_crit: %(max_crit)s, init: %(init)s' % self.__dict__
        if self.fault_ep_slot is not None:
            s += ', fault_ep: 0x%0.8x' % self.fault_ep_slot
        if self.domain is not None:
            s += ', dom: %d' % self.domain
        s += ')'
        return s

    def set_fault_ep_slot(self, fault_ep_slot):
        self.fault_ep_slot = fault_ep_slot

class Untyped(Object):
    def __init__(self, name, size_bits=12, paddr=None):
        super(Untyped, self).__init__(name)
        self.size_bits = size_bits
        self.paddr = paddr

    def __repr__(self):
        return '%(name)s = ut (%(size_bits)s bits%(maybepaddr)s)' % {
            'name': self.name,
            'size_bits': self.size_bits,
            'maybepaddr':(', paddr: %s' % reliable_hex(self.paddr)) if self.paddr else '',
        }

class IOPorts(Object):
    def __init__(self, name, size=65536): # 64k size is the default in CapDL spec.
        super(IOPorts, self).__init__(name)
        self.size = size

    def __repr__(self):
        return '%(name)s = io_ports (%(size)sk ports)' % \
            {'name':self.name,
             'size':self.size // 1024}

class IODevice(Object):
    def __init__(self, name, domainID, bus, dev, fun):
        super(IODevice, self).__init__(name)
        self.domainID = domainID
        self.bus = bus
        self.dev = dev
        self.fun = fun

    def __repr__(self):
        return '%s = io_device (domainID: %d, 0x%x:%d.%d)' % (self.name, self.domainID, self.bus, self.dev, self.fun)

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
    def __init__(self, name, period=0x0, budget=0x0, data=0x0):
        super(SC, self).__init__(name)
        self.period = period
        self.budget = budget
        self.data = data

    def __repr__(self):
        s = '%(name)s = sc (period: %(period)s, budget: %(budget)s, data: %(data)s)' % self.__dict__
        return s
