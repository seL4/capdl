#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

set(CAPDL_DIR "${CMAKE_CURRENT_LIST_DIR}" CACHE STRING "")
set(PYTHON_CAPDL_PATH "${CAPDL_DIR}/python-capdl-tool" CACHE STRING "")
set(CAPDL_TOOL_HELPERS "${CAPDL_DIR}/capDL-tool/capDL-tool.cmake" CACHE STRING "")
set(CAPDL_LINKER_TOOL "${CAPDL_DIR}/cdl_utils/capdl_linker.py" CACHE STRING "")
mark_as_advanced(CAPDL_DIR PYTHON_CAPDL_PATH CAPDL_TOOL_HELPERS CAPDL_LINKER_TOOL)

macro(capdl_import_project)
    add_subdirectory(${CAPDL_DIR} capdl)
endmacro()

include(${CAPDL_DIR}/capdl-loader-app/helpers.cmake)
include(${CAPDL_DIR}/capDL-tool/capDL-tool.cmake)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(
    capdl
    DEFAULT_MSG
    CAPDL_DIR
    PYTHON_CAPDL_PATH
    CAPDL_TOOL_HELPERS
    CAPDL_LINKER_TOOL
)
