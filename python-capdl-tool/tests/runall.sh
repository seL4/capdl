#!/bin/bash
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
