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

cmake_minimum_required(VERSION 3.7.2)

project(object_sizes C)

set (objsz_file object_sizes.yaml)
add_custom_command(OUTPUT ${objsz_file} DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${objsz_file}
    COMMAND ${CMAKE_C_COMPILER} -E -P "-I$<JOIN:$<TARGET_PROPERTY:sel4,INCLUDE_DIRECTORIES>,;-I>" - > ${objsz_file} < ${CMAKE_CURRENT_SOURCE_DIR}/${objsz_file}
    COMMAND_EXPAND_LISTS DEPENDS sel4)

add_custom_target(object_sizes DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${objsz_file} sel4)
set_property(TARGET object_sizes PROPERTY FILE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${objsz_file}")

