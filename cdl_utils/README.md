<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# cdl_utils

This directory provides a collection of utility scripts relating to capDL.

## capdl_linker.py

The capdl_linker.py generates a final capDL spec from a input list of ELF files,
and a record of which resources via objects and capabilities that the ELF file
applications require. This then produces a capDL file (.cdl) that can be given
to the translator tool for translating into different formats.

## untyped_gen.py

This takes a description of the memory layout in a system from a seL4 build system
artifact and generates the list of untyped objects that the kernel is expected to
create. This relies on knowledge of which policies that the kernel will use for
generating the initial untyped objects. The output of this script is typically
used for performing more exact allocation of objects in a capDL specification.
