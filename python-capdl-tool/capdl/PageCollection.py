#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

'''
Wrapper around a dict of pages for some extra functionality. Only intended to
be used internally.
'''

from __future__ import absolute_import, division, print_function, \
    unicode_literals

from .Cap import Cap
from .Object import ASIDPool, PageDirectory, Frame, PageTable
from .Spec import Spec
from .util import page_table_vaddr, page_table_index, page_index, round_down, \
    PAGE_SIZE, page_table_coverage
import collections

def consume(iterator):
    '''Take a generator and exhaust it. Useful for discarding the unused result
    of something that would otherwise accumulate in memory. Clagged from
    https://docs.python.org/2/library/itertools.html'''
    # feed the entire iterator into a zero-length deque
    collections.deque(iterator, maxlen=0)

class PageCollection(object):
    def __init__(self, name='', arch='arm11', infer_asid=True, pd=None, hyp=None):
        self.name = name
        self.arch = arch
        self._pages = {}
        self._pd = pd
        self._asid = None
        self.infer_asid = infer_asid
        self._spec = None
        self.hyp = hyp

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

    def get_page_directory(self):
        if not self._pd:
            self._pd = PageDirectory('pd_%s' % self.name)
        return self._pd, Cap(self._pd)

    def get_asid(self):
        if not self._asid and self.infer_asid:
            self._asid = ASIDPool('asid_%s' % self.name)
            self._asid[0] = self.get_page_directory()[1]
        return self._asid

    def get_spec(self):
        if self._spec is not None:
            return self._spec

        spec = Spec(self.arch)

        # Page directory and ASID.
        pd, pd_cap = self.get_page_directory()
        spec.add_object(pd)
        asid = self.get_asid()
        if asid is not None:
            spec.add_object(asid)

        # Construct frames and infer page tables from the pages.
        pts = {}
        pt_counter = 0
        for page_counter, (page_vaddr, page) in enumerate(self._pages.items()):
            frame = Frame('frame_%s_%04d' % (self.name, page_counter),
                page['size'])
            spec.add_object(frame)
            page_cap = Cap(frame, read=page['read'], write=page['write'],
                grant=page['execute'])

            pt_vaddr = page_table_vaddr(self.arch, page_vaddr)
            pt_index = page_table_index(self.arch, pt_vaddr)
            if page['size'] >= page_table_coverage(self.arch):
                pt_counter += 1
                pd[pt_index] = page_cap
            else:
                if pt_vaddr not in pts:
                    pt = PageTable('pt_%s_%04d' % (self.name, pt_counter))
                    pt_counter += 1
                    spec.add_object(pt)
                    pt_cap = Cap(pt)
                    pd[pt_index] = pt_cap
                    pts[pt_vaddr] = pt
                pt = pd[pt_index].referent
                p_index = page_index(self.arch, page_vaddr)
                pt[p_index] = page_cap

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
