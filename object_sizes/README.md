<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Object Sizes

This directory provides a cmake target runs which runs the preprocessor over a
yaml file to extract the preprocessed values from of specific object size
constants from libsel4 to be passed to other tools which require knowledge of
object sizes. This allows sizes to be evaluated with respect to the kernel
configuration.

The location of the preprocessed file in the build directory is then set in the
FILE_PATH property of the object_sizes target.

