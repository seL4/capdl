/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#include <sel4/sel4.h>
#include <utils/util.h>

{% raw %}
#define DECLARE_IPCBUFFER_SYMBOL(symbol) \
extern char symbol[]; \
void CONSTRUCTOR(199) setIPCBuffer(void) { \
    __sel4_ipc_buffer = (seL4_IPCBuffer *) symbol;\
}
{% endraw %}

DECLARE_IPCBUFFER_SYMBOL({{ipc_buffer_symbol}})

#define SIZED_SYMBOL(symbol, size, section) \
	char symbol[size] VISIBLE ALIGN(4096) SECTION(section);

{% for (symbol, slot) in slots -%}
seL4_CPtr {{symbol}} = {{slot}};
{% endfor %}

{% for (symbol, size, section) in symbols -%}
SIZED_SYMBOL({{symbol}}, {{size}}, "{{section}}")
{% endfor %}


char progname[] = "{{progname}}";
