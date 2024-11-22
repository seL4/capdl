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
    separate_arguments(
        cmake_c_flags_sep NATIVE_COMMAND "${CMAKE_C_FLAGS}"
    )

    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}.bin
        BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}
        COMMAND_EXPAND_LISTS
        COMMAND ${CMAKE_C_COMPILER} ${cmake_c_flags_sep}  -static -nostdlib -z max-page-size=0x1000

            "-I$<JOIN:$<TARGET_PROPERTY:capdl_loader,INTERFACE_INCLUDE_DIRECTORIES>,;-I>"
            "-I$<JOIN:$<TARGET_PROPERTY:sel4,INTERFACE_INCLUDE_DIRECTORIES>,;-I>"
            "-I$<JOIN:$<TARGET_PROPERTY:sel4_autoconf,INTERFACE_INCLUDE_DIRECTORIES>,;-I>"
            -I$<TARGET_PROPERTY:capdl_loader_app_Config,INTERFACE_INCLUDE_DIRECTORIES>
            $<TARGET_OBJECTS:capdl_loader_base>
            ${CAPDL_BUILD_APP_OUTPUT}_archive.o
            ${CAPDL_BUILD_APP_C_SPEC}
            -lgcc
            -o ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}
        COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT} ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}.bin
        DEPENDS  ${CAPDL_BUILD_APP_OUTPUT}_archive.o capdl_loader $<TARGET_OBJECTS:capdl_loader_base>
        "${CAPDL_BUILD_APP_ELF}"
         ${CAPDL_BUILD_APP_DEPENDS}
    )

    add_custom_target("${CAPDL_BUILD_APP_OUTPUT}-custom" DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}.bin)

    add_library("${CAPDL_BUILD_APP_OUTPUT}" STATIC IMPORTED GLOBAL)
    add_dependencies("${CAPDL_BUILD_APP_OUTPUT}" "${CAPDL_BUILD_APP_OUTPUT}-custom")
    set_property(
        TARGET "${CAPDL_BUILD_APP_OUTPUT}"
        PROPERTY IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${CAPDL_BUILD_APP_OUTPUT}"
    )

endfunction(BuildCapDLApplication)

# Hook for CAmkES build system. This allows CAmkES projects to
# propagate the capDL allocation setting into the loader.
function(SetCapDLLoaderStaticAlloc)
    set(CapDLLoaderStaticAlloc ON CACHE BOOL "" FORCE)
endfunction()
