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

'''
Wrapper around a dict of pages for some extra functionality. Only intended to
be used internally.
'''

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from .Cap import Cap
from .Object import ASIDPool, Frame
from .Spec import Spec
from .util import round_down, PAGE_SIZE, lookup_architecture
import collections

def consume(iterator):
    '''Take a generator and exhaust it. Useful for discarding the unused result
    of something that would otherwise accumulate in memory. Clagged from
    https://docs.python.org/2/library/itertools.html'''
    # feed the entire iterator into a zero-length deque
    collections.deque(iterator, maxlen=0)

class PageCollection(object):
    def __init__(self, name='', arch='arm11', infer_asid=True, vspace_root=None):
        self.name = name
        self.arch = arch
        self._pages = {}
        self._vspace_root = vspace_root
        self._asid = None
        self.infer_asid = infer_asid
        self._spec = None

    def add_page(self, vaddr, read=False, write=False, execute=False, size=PAGE_SIZE):
        if vaddr not in self._pages:
            # Only create this page if we don't already have it.
            self._pages[vaddr] = {
                'read':False,
                'write':False,
                'execute':False,
                'size':PAGE_SIZE,
            }
        # Now upgrade this page's permissions to meet our current requirements.
        self._pages[vaddr]['read'] |= read
        self._pages[vaddr]['write'] |= write
        self._pages[vaddr]['execute'] |= execute
        self._pages[vaddr]['size'] = size

    def __getitem__(self, key):
        return self._pages[key]

    def __iter__(self):
        return self._pages.__iter__()

    def get_vspace_root(self):
        if not self._vspace_root:
            vspace = lookup_architecture(self.arch).vspace()
            self._vspace_root = vspace.get_make_object()('%s_%s' % (vspace.get_type_name(), self.name))
        return self._vspace_root, Cap(self._vspace_root)

    def get_asid(self):
        if not self._asid and self.infer_asid:
            self._asid = ASIDPool('asid_%s' % self.name)
            self._asid[0] = self.get_vspace_root()[1]
        return self._asid

    def get_spec(self):
        if self._spec is not None:
            return self._spec

        spec = Spec(self.arch)

        # Page directory and ASID.
        vspace_root, vspace_root_cap = self.get_vspace_root()
        spec.add_object(vspace_root)
        asid = self.get_asid()
        if asid is not None:
            spec.add_object(asid)

        # Construct frames and infer page objects from the pages.
        vspace = spec.arch.vspace()
        object_counter = 0
        objects = {}
        for page_counter, (page_vaddr, page) in enumerate(self._pages.items()):
            frame = Frame('frame_%s_%04d' % (self.name, page_counter),
                page['size'])
            spec.add_object(frame)
            page_cap = Cap(frame, read=page['read'], write=page['write'],
                grant=page['execute'])

            # Walk the hierarchy, creating missing objects until we can
            # insert the frame
            level = vspace
            parent = vspace_root
            while level.child is not None and page['size'] < level.child.coverage:
                level = level.child
                object_vaddr = level.base_vaddr(page_vaddr)
                object_index = level.parent_index(object_vaddr)
                if (level, object_vaddr) not in objects:
                    object = level.make_object('%s_%s_%04d' % (level.type_name, self.name, object_counter))
                    object_counter += 1
                    spec.add_object(object)
                    object_cap = Cap(object)
                    parent[object_index] = object_cap
                    objects[(level, object_vaddr)] = object
                parent = parent[object_index].referent
            object_counter += 1
            parent[level.child_index(page_vaddr)] = page_cap

        # Cache the result for next time.
        assert self._spec is None
        self._spec = spec

        return spec

def create_address_space(regions, name='', arch='arm11'):
    assert isinstance(regions, list)

    pages = PageCollection(name, arch)
    for r in regions:
        assert 'start' in r
        assert 'end' in r
        v = round_down(r['start'])
        while round_down(v) < r['end']:
            pages.add_page(v, r.get('read', False), r.get('write', False),
                r.get('execute', False))
            v += PAGE_SIZE

    return pages
