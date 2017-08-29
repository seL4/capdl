<!--
  Copyright 2017, Data61
  Commonwealth Scientific and Industrial Research Organisation (CSIRO)
  ABN 41 687 119 230.

  This software may be distributed and modified according to the terms of
  the BSD 2-Clause license. Note that NO WARRANTY is provided.
  See "LICENSE_BSD2.txt" for details.

  @TAG(DATA61_BSD)
-->
CapDL initialiser for seL4
==========================

This repository contains the capDL initialiser for seL4.

The capDL initialiser is the root task for a seL4 system that takes a
description of the state to be initialised using [capDL][capDL paper],
and starts the system in conformance with the specification.

The code is an implementation of the formal algorithm specified
in Isabelle/HOL.

  [capDL paper]: http://www.ssrg.nicta.com.au/publications/papers/Kuz_KLW_10.pdf "capDL: A language for describing capability-based systems"

Repository overview
-------------------

  * [`src/main.c`](src/main.c): The implementation of the initialiser
  * [`include/capdl.h`](include/capdl.h): The data structure for capDL.

Dependencies
-------------

The capDL loader uses `capDL-tool` to generate a data structure
containing the capDL specification to be initialised.

Related papers
--------------

The formal model for the capDL initialiser is documented in
[ICFEM '13 paper][Boyton_13] and Andrew Boyton's PhD thesis.

  [Boyton_13]: http://www.nicta.com.au/pub?id=7047 "Formally Verified System Initialisation"

License
-------

The files in this repository are release under standard open source licenses.
Please see individual file headers and the `LICENSE_BSD2`.txt file for details.
