#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2019, Data61
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
This script is a quick way to execute the tests for all capdl-python modules.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals
from concurrencytest import ConcurrentTestSuite, fork_for_tests

import argparse, multiprocessing, os, sys, unittest

ME = os.path.abspath(__file__)


def main(argv):
    parser = argparse.ArgumentParser(prog=argv[0],
        description='Run capdl tests')
    parser.add_argument('--verbosity', '-v', default=1, type=int,
                        help="Verbosity to run tests. 0 = quiet. 1 = default. 2 = verbose")
    options = parser.parse_args(argv[1:])

    # load the tests we want to run
    loader = unittest.TestLoader()
    test_suite = unittest.TestSuite()
    print("Looking for tests in {0}".format(os.path.dirname(ME)))
    test_suite.addTests(loader.discover(os.path.dirname(ME), pattern="*.py"))

    concurrent_suite = ConcurrentTestSuite(test_suite, fork_for_tests(multiprocessing.cpu_count()))
    runner = unittest.TextTestRunner(verbosity=options.verbosity)
    result = runner.run(concurrent_suite)
    if result.wasSuccessful():
        return 0
    return 1


if __name__ == '__main__':
    sys.exit(main(sys.argv))