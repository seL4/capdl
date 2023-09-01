/*
 * Copyright 2023, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <stdarg.h>
#include "stdio_impl.h"

#include <sel4/sel4.h>

static size_t sn_write(FILE *f, const unsigned char *s, size_t l)
{
    size_t k = f->wpos - f->wbase;
    for (size_t i = 0; i < k; i++) {
        seL4_DebugPutChar(f->wbase[i]);
    }
    size_t t = k;
    k = l;

    for (size_t i = 0; i < k; i++) {
        seL4_DebugPutChar(s[i]);
    }
	f->wpos = f->wbase = f->buf;
    return t + k;
}


int _printf(const char *restrict fmt, ...)
{
    int ret;
    va_list ap;
    va_start(ap, fmt);
    unsigned char buf[1];
    FILE f = {
        .lbf = EOF,
        .write = sn_write,
        .lock = -1,
        .buf = buf,
        .cookie = NULL,
    };

    ret = vfprintf(&f, fmt, ap);

    va_end(ap);
    return ret;
}

