<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# CapDL initialiser for seL4

This repository contains the capDL initialiser for seL4.

The capDL initialiser is the root task for a seL4 system that takes a
description of the state to be initialised using [capDL][Kuz_KLW_10],
and starts the system in conformance with the specification.

The code is an implementation of the formal algorithm specified
in Isabelle/HOL.

  [Kuz_KLW_10]: https://ts.data61.csiro.au/publications/nicta_full_text/3679.pdf "capDL: A language for describing capability-based systems"

## Repository overview

  * [`src/main.c`](src/main.c): The implementation of the initialiser
  * [`include/capdl.h`](include/capdl.h): The data structure for capDL.

## Dependencies

The capDL loader uses `capDL-tool` to generate a data structure
containing the capDL specification to be initialised.

## Related papers

The formal model for the capDL initialiser is documented in
[ICFEM '13 paper][Boyton_13] and Andrew Boyton's PhD thesis.

  [Boyton_13]: https://ts.data61.csiro.au/publications/nictaabstracts/Boyton:phd.abstract.pml "Formally Verified System Initialisation"

## License

The files in this repository are release under standard open source licenses.
Please see individual file headers and the `LICENSE_BSD2`.txt file for details.
