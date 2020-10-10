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

    # Build the application
    add_executable(
        "${CAPDL_BUILD_APP_OUTPUT}"
        EXCLUDE_FROM_ALL
        $<TARGET_PROPERTY:capdl_app_properties,C_FILES>
        ${CAPDL_LOADER_APP_C_FILES}
        ${CAPDL_BUILD_APP_OUTPUT}_archive.o
        ${CAPDL_BUILD_APP_C_SPEC}
    )

    if(DEFINED platform_yaml)
        add_dependencies("${CAPDL_BUILD_APP_OUTPUT}" mem_regions)
    endif()

    add_dependencies("${CAPDL_BUILD_APP_OUTPUT}" ${CAPDL_BUILD_APP_DEPENDS})
    target_include_directories(
        "${CAPDL_BUILD_APP_OUTPUT}"
        PRIVATE $<TARGET_PROPERTY:capdl_app_properties,INCLUDE_DIRS>
    )
    target_link_libraries(
        "${CAPDL_BUILD_APP_OUTPUT}"
        sel4runtime
        sel4
        cpio
        sel4platsupport
        sel4utils
        capdl_loader_app_Config
        sel4_autoconf
    )
    if(KernelDebugBuild)
        target_link_libraries("${CAPDL_BUILD_APP_OUTPUT}" sel4muslcsys)
    endif()
endfunction(BuildCapDLApplication)

# Hook for CAmkES build system. This allows CAmkES projects to
# propagate the capDL allocation setting into the loader.
function(SetCapDLLoaderStaticAlloc)
    set(CapDLLoaderStaticAlloc ON CACHE BOOL "" FORCE)
endfunction()
