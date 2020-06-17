#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

cmake_minimum_required(VERSION 3.7.2)

project(object_sizes C)

set(objsz_file object_sizes.yaml)
add_custom_command(
    OUTPUT ${objsz_file}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${objsz_file}
    COMMAND
        ${CMAKE_C_COMPILER} -E -P "-I$<JOIN:$<TARGET_PROPERTY:sel4,INCLUDE_DIRECTORIES>,;-I>" - >
        ${objsz_file} < ${CMAKE_CURRENT_SOURCE_DIR}/${objsz_file} COMMAND_EXPAND_LISTS
    DEPENDS sel4
)

add_custom_target(object_sizes DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${objsz_file} sel4)
set_property(TARGET object_sizes PROPERTY FILE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${objsz_file}")
