/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef DEBUG_H__
#define DEBUG_H__

#include <autoconf.h>

#include <sel4/types.h>
#include <sel4utils/sel4_debug.h>

#include "capdl.h"

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define die(fmt, args...) do { \
    printf("\ncapDL-loader :: \033[1;31m<< Error: " fmt " >>\033[0m\n", ##args); \
    abort(); \
} while (0)

/* print and abort if a seL4 function returned an error */
#define seL4_AssertSuccess(x) do { \
    int __error = x; \
    if (__error) { \
        die("%s:%d: %s", __FUNCTION__, __LINE__, sel4_errlist[__error]); \
    } \
} while (0)

#else  /* CONFIG_CAPDL_LOADER_VERIFIED */

#define die(fmt, args...) do {} while (0)
#define seL4_AssertSuccess(x) (void) (x)

#endif

#endif
