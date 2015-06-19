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

import Cap
import math

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
                assert isinstance(index, str)
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
            size = '%dM' % (self.size / 1024 / 1024)
        elif self.size % 1024 == 0:
            size = '%dk' % (self.size / 1024)
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

class AsyncEndpoint(Object):
    def __repr__(self):
        return '%s = aep' % self.name

class TCB(ContainerObject):
    def __init__(self, name, ipc_buffer_vaddr=0x0, ip=0x0, sp=0x0, elf=None, \
            prio=254, init=None, domain=None):
        super(TCB, self).__init__(name)
        self.addr = ipc_buffer_vaddr
        self.ip = ip
        self.sp = sp
        self.elf = elf or ''
        self.prio = prio
        self.init = init or []
        self.domain = domain

    def __repr__(self):
        # XXX: Assumes 32-bit pointers
        s = '%(name)s = tcb (addr: 0x%(addr)0.8x, ip: 0x%(ip)0.8x, sp: 0x%(sp)0.8x, elf: %(elf)s, prio: %(prio)s, init: %(init)s' % self.__dict__
        if self.domain is not None:
            s += ', dom: %d' % self.domain
        s += ')'
        return s

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
            {'name':self.name, \
             'size':self.size / 1024}

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

    def set_endpoint(self, aep):
        # Allow the user to pass an object or cap.
        if isinstance(aep, Object):
            assert isinstance(aep, AsyncEndpoint)
            c = Cap.Cap(aep)
        else:
            assert isinstance(aep, Cap.Cap)
            assert isinstance(aep.referent, AsyncEndpoint)
            c = aep
        self[0] = c

    def __repr__(self):
        # Note, in CapDL this is actually represented as a 0-sized CNode.
        return '%s = irq' % self.name

class VCPU(Object):
    def __repr__(self):
        return '%s = vcpu' % self.name
