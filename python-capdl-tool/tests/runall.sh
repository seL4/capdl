#!/bin/bash
#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

# Run all the tests.

set -e

function exec_in() {
    pushd $1 &>/dev/null
    $2
    popd &>/dev/null
}

ROOT=`exec_in .. pwd`

for t in `find . -mindepth 2 -executable`; do
    dir=`dirname $t`
    f=`basename $t`
    echo " [EXEC] $f"
    PYTHONPATH=${ROOT}:${PYTHONPATH} exec_in ${dir} ./$f
done
