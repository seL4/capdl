#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
This script is a quick way to execute the tests for all capdl-python modules.
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals
from concurrencytest import ConcurrentTestSuite, fork_for_tests

import argparse
import multiprocessing
import os
import sys
import unittest

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
