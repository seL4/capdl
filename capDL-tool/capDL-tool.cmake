#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

set(CapDLToolDirectory ${CMAKE_CURRENT_LIST_DIR})

include(make)
# Build and install capDL-tool
# Path of installed tool will be returned in program_path
# This assumes that there are no other dependencies.
function(CapDLToolInstall target program_path)
    # Require the parse-capDL tool
    create_depfile_by_find(
        depfile_commands
        "${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/parse-capDL"
        "${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/parse-capDL.d"
        "${CapDLToolDirectory}/"
    )
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/parse-capDL"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/
        ${depfile_commands}
        COMMAND
            cp -a ${CapDLToolDirectory}/* .
        COMMAND
            ${CMAKE_COMMAND} -E env make
    )
    add_custom_target(${target} DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/parse-capDL")
    set(${program_path} "${CMAKE_CURRENT_BINARY_DIR}/capDL-tool/parse-capDL" PARENT_SCOPE)
endfunction()

# Create target for creating capdl C file from cdl spec
# target: Name of target to Create
# output: Output name of C file
# static_alloc: whether the spec has been statically allocated
# object_sizes: object_sizes file
# input: Input name of capdl spec
# capdl_tool: Path to capdl parse tool
# MAX_IRQS: Named argument for size of IRQ array (Deprecated)
# DEPENDS: Any target or file dependencies that the parse command depends on
function(CapDLToolCFileGen target output static_alloc object_sizes input capdl_tool)
    cmake_parse_arguments(PARSE_ARGV 6 CAPDL "" "MAX_IRQS" "DEPENDS")
    if(NOT "${CAPDL_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to CapDLToolCFileGen")
    endif()
    if(static_alloc)
        set(alloc_type_opt "--code-static-alloc")
        set(object_sizes_opt "")
    else()
        set(alloc_type_opt "--code-dynamic-alloc")
        set(object_sizes_opt "--object-sizes=${object_sizes}")
    endif()
    if(NOT "${CAPDL_MAX_IRQS}" STREQUAL "")
        message(WARNING "MAX_IRQS option to CapDLToolCFileGen is no longer used by the tool")
    endif()
    # Invoke the parse-capDL tool to turn the CDL spec into a C spec
    add_custom_command(
        OUTPUT ${output}
        COMMAND ${capdl_tool} --code ${output} ${alloc_type_opt} ${object_sizes_opt} "${input}"
        DEPENDS "${input}" "${object_sizes}" ${CAPDL_DEPENDS}
    )
    add_custom_target(${target} DEPENDS ${output} ${object_sizes})
endfunction()
