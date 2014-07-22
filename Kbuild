#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

apps-$(CONFIG_MODULE_CAPDL_LOADER) += capdl-loader-experimental

capdl-loader-experimental-y = kernel_elf common libsel4 $(libc) libcpio libelf libsel4muslcsys libsel4platsupport libsel4simple libutils libsel4utils

ifdef CONFIG_KERNEL_STABLE
capdl-loader-experimental-y += libsel4simple-stable
else
capdl-loader-experimental-y += libsel4simple-default
endif

capdl-loader-experimental: $(capdl-loader-experimental-y)
