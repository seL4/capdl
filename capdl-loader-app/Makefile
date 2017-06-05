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

# Targets
TARGETS := $(notdir ${SOURCE_DIR}).bin

CFILES   := $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/*.c))
CFILES   += $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/plat/${PLAT}/*.c))
CFILES   += $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/arch/${ARCH}/*.c))

HDRFILES := $(wildcard $(SOURCE_DIR)/include/*)
OFILES   := archive.o

CFILES += ${BUILD_DIR}/src/capdl_spec.c

ENTRY_POINT := _sel4_start

LIBS := c sel4 elf cpio sel4muslcsys sel4platsupport platsupport sel4simple sel4utils utils sel4vspace sel4debug
ifdef CONFIG_KERNEL_STABLE
LIBS += sel4simple-stable
else
LIBS += sel4simple-default
endif

include $(SEL4_COMMON)/common.mk

# Possible names of the capDL loader
loaders = $(SEL4_BINDIR)/capDL-loader $(SEL4_BINDIR)/capdl-loader-experimental
components = $(filter-out $(loaders),$(wildcard $(SEL4_BINDIR)/*))
ifeq (${components},)
$(error You must select at least one application to boot with the CapDL loader (Nothing relevant found in ${SEL4_BINDIR}))
endif

.PHONY: archive.o
archive.o:
	$(Q)mkdir -p $(dir $@)
	@echo " [CPIO] $@"
	@for i in $(foreach v,${components},$(notdir ${v})); do echo "  [CPIO] $$i"; done
	${Q}V=${V} ${COMMON_PATH}/files_to_obj.sh $@ _capdl_archive ${components}

# Expect the caller to provide the spec source to translate in CAPDL_SPEC.
${BUILD_DIR}/src/capdl_spec.c: ${CAPDL_SPEC}
	@echo " [GEN] $(notdir $@)"
	${Q}mkdir -p "$(dir $@)"
	${Q}$(if $^,,echo "No CapDL spec provided" >&2 ; exit 1)
	${Q}$(if $(filter 1,$(words $^)),,echo "Error: multiple CapDL specs provided: $^" >&2 ; exit 1)
	${Q}${STAGE_DIR}/parse-capDL/parse-capDL --code "$@" "$^"
