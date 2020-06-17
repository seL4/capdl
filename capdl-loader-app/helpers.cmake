#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

cmake_minimum_required(VERSION 3.7.2)

function(BuildCapDLApplication)
    cmake_parse_arguments(PARSE_ARGV 0 CAPDL_BUILD_APP "" "C_SPEC;OUTPUT" "ELF;DEPENDS")
    if(NOT "${CAPDL_BUILD_APP_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to BuildCapDLApplication")
    endif()
    # Require a cspec and an output
    if("${CAPDL_BUILD_APP_C_SPEC}" STREQUAL "")
        message(FATAL_ERROR "C_SPEC is required argument to BuildCapDLApplication")
    endif()
    if("${CAPDL_BUILD_APP_OUTPUT}" STREQUAL "")
        message(FATAL_ERROR "OUTPUT is required argument to BuildCapDLApplication")
    endif()
    # Build a CPIO archive out of the provided ELF files
    include(cpio)
    MakeCPIO(
        ${CAPDL_BUILD_APP_OUTPUT}_archive.o
        "${CAPDL_BUILD_APP_ELF}"
        CPIO_SYMBOL
        _capdl_archive
    )
    # Build the application
    add_executable(
        "${CAPDL_BUILD_APP_OUTPUT}"
        EXCLUDE_FROM_ALL
        $<TARGET_PROPERTY:capdl_app_properties,C_FILES>
        ${CAPDL_LOADER_APP_C_FILES}
        ${CAPDL_BUILD_APP_OUTPUT}_archive.o
        ${CAPDL_BUILD_APP_C_SPEC}
    )
    add_dependencies("${CAPDL_BUILD_APP_OUTPUT}" ${CAPDL_BUILD_APP_DEPENDS})
    target_include_directories(
        "${CAPDL_BUILD_APP_OUTPUT}"
        PRIVATE $<TARGET_PROPERTY:capdl_app_properties,INCLUDE_DIRS>
    )
    target_link_libraries(
        "${CAPDL_BUILD_APP_OUTPUT}"
        sel4runtime
        muslc
        sel4
        cpio
        sel4platsupport
        sel4utils
        sel4muslcsys
        capdl_loader_app_Config
        sel4_autoconf
    )
endfunction(BuildCapDLApplication)

# Hook for CAmkES build system. This allows CAmkES projects to
# propagate the capDL allocation setting into the loader.
function(SetCapDLLoaderStaticAlloc)
    set(CapDLLoaderStaticAlloc ON CACHE BOOL "" FORCE)
endfunction()
