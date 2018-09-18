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

cmake_minimum_required(VERSION 3.7.2)

set(configure_string "")

config_choice(CapDLLoaderCallingConvention CAPDL_LOADER_CALLING_CONVENTION
    "Select the calling convention that loaded processes are expecting. If
    you select the register convention you will be limited to passing four
    arguments to processes and their stack frames will not need to be
    mapped in during thread configuration."
    "standard;CapDLLoaderCallingConventionStandard;CAPDL_LOADER_CC_STANDARD"
    "registers;CapDLLoaderCallingConventionRegisters;CAPDL_LOADER_CC_REGISTERS"
)

config_string(CapDLLoaderMaxObjects CAPDL_LOADER_MAX_OBJECTS
    "Maximum number of objects that can be handled by the loader. Note that
    there is an upper limit on this based on the size of the initialiser's
    CNode."
    DEFAULT 4096
    UNQUOTE
)

config_string(CapDLLoaderMaxIRQs CAPDL_LOADER_MAX_IRQS
    "Maximum number of interrupts supported. The initialiser will only
    create interrupt handlers for interrupts up to this maximum."
    DEFAULT 256
    UNQUOTE
)

config_option(CapDLLoaderAllowNoCSpace CAPDL_LOADER_ALLOW_NO_CSPACE
    "Allow threads to be specified in CapDL with no CSpace and have them
    configured as such by the initialiser. Note that this requires a kernel
    that supports threads without CSpaces."
    DEFAULT OFF
)

config_option(CapDLLoaderWriteablePages CAPDL_LOADER_WRITEABLE_PAGES
    "CapDL Loader will map all pages as writeable if the cap has grant permissions.
    This is currently used to enable instruction and data rewriting by an external debugger such as GDB."
    DEFAULT OFF
)

config_option(CapDLLoaderPrintDeviceInfo CAPDL_LOADER_PRINT_DEVICE_INFO
    "Dump device frame information on startup for the purposes of debugging."
    DEFAULT OFF
    DEPENDS "NOT CapDLLoaderVerified"
)

config_option(CapDLLoaderPrintUntypeds CAPDL_LOADER_PRINT_UNTYPEDS
    "Dump untyped object information on startup for the purposes of debugging."
    DEFAULT OFF
    DEPENDS "NOT CapDLLoaderVerified"
)

config_option(CapDLLoaderPrintCapDLObjects CAPDL_LOAD_PRINT_CAPDL_OBJECTS
    "Display verbose capDL objects as they are created. Could help with debugging,
    but for large specs with lots of objects, this could slow things down significantly."
    DEFAULT OFF
    DEPENDS "NOT CapDLLoaderVerified"
)

add_config_library(capdl_loader_app "${configure_string}")

# The capdl-loader-app requires outside configuration in order to build. To achieve this
# we just declare a target here with custom properties for describing the source and headers.
# This is done here as at this point we know the source directory and can construct these things.
# Later on the user will construct a rule for actually generating the capdl executable from a
# different project directory
add_custom_target(capdl_app_properties)
set_property(TARGET capdl_app_properties PROPERTY C_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/main.c")
set_property(TARGET capdl_app_properties PROPERTY INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

RequireFile(CAPDL_LOADER_BUILD_HELPERS helpers.cmake)
