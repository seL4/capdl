<!--
   Copyright 2018, Data61
   Commonwealth Scientific and Industrial Research Organisation (CSIRO)
    ABN 41 687 119 230.

    This software may be distributed and modified according to the terms of
    the BSD 2-Clause license. Note that NO WARRANTY is provided.
    See "LICENSE_BSD2.txt" for details.

    @TAG(DATA61_BSD)
-->

# Object Sizes

This directory provides a cmake target runs which runs the preprocessor over a
yaml file to extract the preprocessed values from of specific object size
constants from libsel4 to be passed to other tools which require knowledge of
object sizes. This allows sizes to be evaluated with respect to the kernel
configuration.

The location of the preprocessed file in the build directory is then set in the
FILE_PATH property of the object_sizes target.

