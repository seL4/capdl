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

# Build and install capDL-tool
# Path of installed tool will be returned in program_path
# This assumes that there are no other dependencies.
function(CapDLToolInstall target program_path)
    # Require the parse-capDL tool
    ExternalProject_Add(parse_capdl_tool
        BUILD_ALWAYS ON # for tracking changes to the capDL source dir
        CONFIGURE_COMMAND "" # capDL-tool has its own build system
        SOURCE_DIR "${CapDLToolDirectory}"
        BUILD_COMMAND bash -c "cp -a ${CapDLToolDirectory}/* ."
        COMMAND       ${CMAKE_COMMAND} -E env make
        INSTALL_COMMAND ${CMAKE_COMMAND} -E env "PATH=$ENV{PATH}:${CMAKE_CURRENT_BINARY_DIR}/parse_capdl_tool-prefix/src/parse_capdl_tool-build" make install
    )
    ExternalProject_Get_property(parse_capdl_tool BINARY_DIR)
    DeclareExternalProjObjectFiles(parse_capdl_tool ${BINARY_DIR} FILES parse-capDL)
    add_custom_target(${target} DEPENDS "${BINARY_DIR}/parse-capDL")
    set(${program_path} "${BINARY_DIR}/parse-capDL" PARENT_SCOPE)
endfunction()

# Create target for creating capdl C file from cdl spec
# target: Name of target to Create
# output: Output name of C file
# input: Input name of capdl spec
# capdl_tool: Path to capdl parse tool
# MAX_IRQS: Named argument for size of IRQ array
# DEPENDS: Any target or file dependencies that the parse command depends on
function(CapDLToolCFileGen target output input capdl_tool)
    cmake_parse_arguments(PARSE_ARGV 4 CAPDL "" "MAX_IRQS" "DEPENDS")
    if (NOT "${CAPDL_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to CapDLToolCFileGen")
    endif()
    if (NOT "${CAPDL_MAX_IRQS}" STREQUAL "")
        set(max_irqs "--code-max-irqs=${CAPDL_MAX_IRQS}")
    endif()
    # Invoke the parse-capDL tool to turn the CDL spec into a C spec
    add_custom_command(
        OUTPUT ${output}
        COMMAND
            ${capdl_tool} ${max_irqs} --code ${output} "${input}"
        DEPENDS
            "${input}"
            ${CAPDL_DEPENDS}
    )
    add_custom_target(${target} DEPENDS ${output})
endfunction()
