/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <autoconf.h>

#include <sel4/types.h>

#include "capdl.h"

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define die(fmt, args...) do { \
    printf("\ncapDL-loader :: \033[1;31m<< Error: " fmt " >>\033[0m\n", ##args); \
    abort(); \
} while (0)

static
char *__seL4_Errors[] = {
    "seL4_NoError",
    "seL4_InvalidArgument",
    "seL4_InvalidCapability",
    "seL4_IllegalOperation",
    "seL4_RangeError",
    "seL4_AlignmentError",
    "seL4_FailedLookup",
    "seL4_TruncatedMessage",
    "seL4_DeleteFirst",
    "seL4_RevokeFirst",
    "seL4_NotEnoughMemory",
};

/* print and abort if a seL4 function returned an error */
#define seL4_AssertSuccess(x) do { \
    int __error = x; \
    switch (__error) { \
        case INT_MIN ... -1: \
        case (sizeof(__seL4_Errors) / sizeof(__seL4_Errors[0])) ... INT_MAX: \
            die("%s:%d: <error code out of range> (%d)", __FUNCTION__, __LINE__, __error); \
        case 0: \
            break; \
        default: \
            die("%s:%d: %s", __FUNCTION__, __LINE__, __seL4_Errors[__error]); \
    } \
} while (0)

#else  /* CONFIG_CAPDL_LOADER_VERIFIED */

#define die(fmt, args...) do {} while (0)
#define seL4_AssertSuccess(x) (void) (x)

#endif

#endif
