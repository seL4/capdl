#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

cmake_minimum_required(VERSION 3.7.2)

set(configure_string "")

config_choice(
    CapDLLoaderCallingConvention
    CAPDL_LOADER_CALLING_CONVENTION
    "Select the calling convention that loaded processes are expecting. If
    you select the register convention you will be limited to passing four
    arguments to processes and their stack frames will not need to be
    mapped in during thread configuration."
    "standard;CapDLLoaderCallingConventionStandard;CAPDL_LOADER_CC_STANDARD"
    "registers;CapDLLoaderCallingConventionRegisters;CAPDL_LOADER_CC_REGISTERS"
)

config_string(
    CapDLLoaderMaxObjects
    CAPDL_LOADER_MAX_OBJECTS
    "Maximum number of objects that can be handled by the loader. Note that
    there is an upper limit on this based on the size of the initialiser's
    CNode."
    DEFAULT
    4096
    UNQUOTE
)

config_string(
    CapDLLoaderRootStack
    CAPDL_LOADER_ROOT_STACK
    "Size of the initial stack for the root task"
    DEFAULT
    16384
)

config_string(
    CapDLLoaderFillsPerFrame
    CAPDL_LOADER_FILLS_PER_FRAME
    "Maximum number of fill commands per frame."
    DEFAULT
    1
    UNQUOTE
)

config_option(
    CapDLLoaderWriteablePages
    CAPDL_LOADER_WRITEABLE_PAGES
    "CapDL Loader will map all pages as writeable if the cap has grant permissions.
    This is currently used to enable instruction and data rewriting by an external debugger such as GDB."
    DEFAULT
    OFF
)

config_option(
    CapDLLoaderPrintDeviceInfo
    CAPDL_LOADER_PRINT_DEVICE_INFO
    "Dump device frame information on startup for the purposes of debugging."
    DEFAULT
    OFF
    DEPENDS
    "NOT CapDLLoaderVerified"
)

config_option(
    CapDLLoaderPrintUntypeds
    CAPDL_LOADER_PRINT_UNTYPEDS
    "Dump untyped object information on startup for the purposes of debugging."
    DEFAULT
    OFF
    DEPENDS
    "NOT CapDLLoaderVerified"
)

config_option(
    CapDLLoaderPrintCapDLObjects
    CAPDL_LOAD_PRINT_CAPDL_OBJECTS
    "Display verbose capDL objects as they are created. Could help with debugging,
    but for large specs with lots of objects, this could slow things down significantly."
    DEFAULT
    OFF
    DEPENDS
    "NOT CapDLLoaderVerified"
)

config_option(
    CapDLLoaderStaticAlloc
    CAPDL_LOADER_STATIC_ALLOC
    "Build the loader to expect a statically allocated capDL spec."
    DEFAULT
    OFF
)

add_config_library(capdl_loader_app "${configure_string}")


if(DEFINED platform_yaml)

    find_file(PLATFORM_SIFT platform_sift.py PATHS ${CMAKE_MODULE_PATH} NO_CMAKE_FIND_ROOT_PATH)
    mark_as_advanced(FORCE PLATFORM_SIFT)
    if("${PLATFORM_SIFT}" STREQUAL "PLATFORM_SIFT-NOTFOUND")
        message(
            FATAL_ERROR
                "Failed to find platform_sift.py. Consider using -DPLATFORM_SIFT=/path/to/file"
        )
    endif()

    set(
        MEMORY_REGIONS
        "${CMAKE_BINARY_DIR}/capdl/capdl-loader-app/gen_config/capdl_loader_app/platform_info.h"
    )
    add_custom_command(
        COMMAND ${PLATFORM_SIFT} --emit-c-syntax ${platform_yaml} > ${MEMORY_REGIONS}
        OUTPUT ${MEMORY_REGIONS}
    )
    add_custom_target(mem_regions DEPENDS ${platform_yaml} ${PLATFORM_SIFT} ${MEMORY_REGIONS})
    set_property(
        SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/capdl/capdl-loader-app/src/main.c
        PROPERTY OBJECT_DEPENDS mem_regions
    )
endif()

get_target_property(MUSL_SOURCE_DIR muslc_gen SOURCE_DIR)

if(KernelDebugBuild)
    set(print_sources
        # Debug sources for printing
        src/check.c
        ${MUSL_SOURCE_DIR}/src/stdio/vfprintf.c
        ${MUSL_SOURCE_DIR}/src/stdio/fwrite.c
        ${MUSL_SOURCE_DIR}/src/stdio/__towrite.c
        ${MUSL_SOURCE_DIR}/src/multibyte/wctomb.c
        ${MUSL_SOURCE_DIR}/src/multibyte/wcrtomb.c
        ${MUSL_SOURCE_DIR}/src/math/__signbitl.c
        ${MUSL_SOURCE_DIR}/src/math/__fpclassifyl.c
        ${MUSL_SOURCE_DIR}/src/math/frexpl.c
        ${MUSL_SOURCE_DIR}/src/string/strnlen.c
        ${MUSL_SOURCE_DIR}/src/string/memchr.c
    )
endif()


# Build the application
add_library(capdl_base OBJECT EXCLUDE_FROM_ALL
    src/main.c
    # Add implementations of memset and memcpy from lib musl
    ${MUSL_SOURCE_DIR}/src/string/memset.c
    ${MUSL_SOURCE_DIR}/src/string/memcpy.c

    ${print_sources}
    )

target_include_directories(
    capdl_base
    PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/include"
        ${MUSL_SOURCE_DIR}/src/internal
        ${MUSL_SOURCE_DIR}/arch/aarch64
)
target_link_libraries(
    capdl_base
    PRIVATE
    sel4
    cpio
    capdl_loader_app_Config
    sel4_autoconf
    muslc
)

if(DEFINED platform_yaml)
    add_dependencies("capdl_base" mem_regions)
endif()

separate_arguments(
    cmake_c_flags_sep NATIVE_COMMAND "${CMAKE_C_FLAGS}"
)
add_custom_command(
    OUTPUT capdl-loader.o
    COMMAND
        ${CMAKE_C_COMPILER} ${cmake_c_flags_sep} -static -nostdlib -z max-page-size=0x1000 -Wl,-r
            $<TARGET_OBJECTS:capdl_base>
            $<TARGET_OBJECTS:cpio>
            -o capdl-loader.o
    DEPENDS $<TARGET_OBJECTS:capdl_base> $<TARGET_OBJECTS:cpio> COMMAND_EXPAND_LISTS
)

add_custom_target(
    capdl_loader_precompile
    DEPENDS capdl-loader.o
)

add_library(capdl_loader_base STATIC IMPORTED GLOBAL)
set_property(TARGET capdl_loader_base PROPERTY IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/capdl-loader.o")
set_property(
    TARGET capdl_loader_base
    PROPERTY INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include"
    "${MUSL_SOURCE_DIR}/src/internal"
    "${MUSL_SOURCE_DIR}/arch/aarch64"
)
add_library(capdl_loader INTERFACE)
add_dependencies(capdl_loader capdl_loader_base)
set_property(TARGET capdl_loader PROPERTY INTERFACE_LINK_LIBRARIES capdl_loader_base)
target_include_directories(capdl_loader INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/include")
