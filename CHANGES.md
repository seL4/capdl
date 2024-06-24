<--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: BSD-2-Clause
-->

Revision history for capDL

This file should be word wrapped to 120 characters

---
Upcoming release

## Changes


## Upgrade Notes
---
0.2.1 2021-06-10
Using seL4 version 12.1.0

## Changes

* Added page-upper-directory caps to the valid TCB check for platforms like QEMU arm-virt as they use different paging
  structures.
* Added const qualifiers to the capdl-loader-app to avoid compiler warnings against other libraries.
* Improved the README for the capdl-loader-app.

## Upgrade Notes

* None to be aware of. This is not a source-breaking or binary-breaking release.

---
capdl-0.2.0 2020-10-27
Using seL4 version 12.0.0

## Changes

* Convert to SPDX license tags. This includes marking all documentation files CC-BY-SA-4.0.

* Build system:
  - Support `CMakeForegroundComplexCommands`. This enables long-running build steps like Haskell installation to
    directly print to the console.
  - Make PLATFORM_SIFT agnostic of the build system directory layout
  - Save the binary artifacts for the capDL-tool in an out-of-build-tree directory, this will not rebuild in future if
    it can find a previously built artifact.
  - Migrate scripts to python3
* Add `seL4_BadgeBits` constant and update python-capdl-tool to directly query the object_sizes dictionary. This allows
  for templates to use badge sizes.
* Add support for Arm smmu v2.
* Add support for Arm GetTrigger and GetTriggerCore seL4 invocations. This enables specs to correctly specify interrupt
  trigger mode and core affinities on Arm.
* Add TCB Resume field to capDL object and support raw TCB object creation.
* Add GitHub actions scripts. These scripts replicate internal CI checks directly on GitHub

### capDL-tool

* rework validObjCap and check TCB slots, which allows vcpus for all architectures.
* convert CapDL language specification to Markdown.

### Capdl-loader-app

* Improve log output.
* Initialise libc in debug builds.
* Add check to only flush and invalidate kernel memory regions in capdl loader on Arm. Add platform_info header with
  memory window.
* Add vcpu support for aarch64.
* Handle IRQ binding to badged notifications properly: If an irq is bound to a notification with a non-zero badge, a
  badged capability is minted and used.  Previously, the IRQ was bound to the unbadged notification.
* Track number of used untypeds during object allocation and fail more gracefully if they run out.
* Improve debugging printouts.
* Fix issue where large DTB images inside BootInfo would overlap reserved memory address used to initialise frames.
* Remove CONFIG_CAPDL_LOADER_ALLOW_NO_CSPACE config option as it has been unused for a while.

### Capdl-linker:

* Optimize spec generation performance:
  - Sort elf symbols by their vaddr.
  - Replace linear search with binary when looking for virtual addresses.
* Fix Python syntax warnings when `capdl_linker.py` is invoked.

---
0.1.0 2019-11-19
Using seL4 version 11.0.0

## Changes

* Add GrantReply access right for endpoint capabilities.
  - This is a new right available on seL4 Endpoint object capabilities.
  - A capDL spec can now describe an endpoint capability with the 'P' GrantReply access right.
  - The capdl-loader-app will create capabilities with these rights based on the translated spec.
  - python-capdl-tool supports creating endpoint caps with GrantReply rights.
* Add object_sizes target.
  - This target generates a YAML file describing the object size of each seL4 object.
  - It is based on the current build configuration of the kernel.
  - This file can be used as an input to tools performing allocation of seL4 objects from untypeds.
* Add untyped_gen.py in cdl_utils.
  - This is only supported on Arm architectures currently.
  - This tool generates a predicted list of untypeds that the kernel will provide to userlevel
  - This list is calculated from an input of memory regions for the platform and sizes of the kernel image and device
    tree binary.
  - The list is intended to be used as an input for an allocator to perform allocation of initial system resources.
* capDL untyped allocation
  - Add support for generating a capDL spec that specifies which untyped object each object and capability is allocated
    from. This is intended to be used when implementing trustworthy system initialisers as it removes online allocation
    decisions and results in a simpler init program.
  - This is only supported on Arm.
* Static allocator
  - Updates capdl-loader-app to load static specs with all objects allocated from a predicted list of untypeds.
  - This simpler version of the loader app will act as a reference implementation for a more trustworthy implementation
    that is in development.
  - This is only supported on Arm.
* RISC-V support added
  - Support for generating, translating and loading RISC-V applications.
* seL4runtime updates
  - Port capdl-loader-app to sel4runtime
* cdl-refine
  - This support is closely tied to CAmkES and L4V.
  - The capDL-tool can translate specs into Isabelle .thy file formats. These are then used to calculate which access
    rights different parts of the system have to each other.
  - It is used to check that the capDL system spec implements certain access policies as specified by an external
    source.
  - See cdl-refine in the context of CAmkES for more information.
* 40-bit PA for aarch64 hyp
  - When seL4 is running in EL2 on aarch64, VSpace objects are an abstraction of stage 2 translations.
    These translations have different input address restrictions based on the physical address range the CPU supports.
    40-bit PA support supports platforms that have 40-bit stage 2 input address ranges when in hyp-mode.
* Remove --code-max-irqs from capDL tool
  - The capDL-tool that translates capDL formats no longer requires --code-max-irqs as an option for generating C specs.
    It now infers the total IRQs from the input spec and specifies this value in the generated C spec.
* Add FrameFill mechanism for files and use this to implement ELF loading
  - The FrameFill mechanism allows Frame objects to be annotated with an attribute describing a way to initialise
    their contents by a loader.
  - The new file FrameFill mechanism allows frames to be initialised from the contents of a file provided to the loader
    in a cpio archive.
  - This mechanism is used to remove ELF loader support from the initialiser. Instead a spec describes how frames are
    initialised via copies from offsets into a provided ELF file.
  - This allows a loader to load program data from different file formats also.

---
