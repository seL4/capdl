<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# capDL-tool

This tool is intended to be used with capDL textual specifications. In
particular, it can parse input files into a variety of different output
formats, including Isabelle theory files and C source files that can be given
to the capDL initialiser.

For usage instructions run 'make' and then './parse-capDL'.

For details of the Capability Distribution Language, see the specification in
the 'doc/' directory. Run 'make' in that directory to generate 'capDL.pdf'.

## Repository overview

  * 'CapDL': Haskell source code of the tool
  * '*.cdl': Toy examples used as test files
  * '*.right': Canonical representations of the examples used in testing
  * 'doc': Latex source of the capDL Specification

## Dependencies

[Stack][1] should automatically retrieve all necessary Haskell dependencies to
build this tool, but if you need exact dependency information it is available
in [`capDL-tool.cabal`](capDL-tool.cabal) and [`stack.yaml`](stack.yaml).

  [1]: https://haskellstack.org

To install all dependencies of this tool (including the haskell compiler if an
appropriate version of GHC isn't found), run

    make sandbox

This will install GHC in "~/.stack" (if it isn't already installed there), and
install all libraries in the local directory ".stack-work".

To build this tool, run

    make

To place the capdl binary in the top level directory of this project, run

    make install

## Related papers

The Capability Distribution Language is also documented in

  Ihor Kuz, Gerwin Klein, Corey Lewis and Adam Christopher Walker
  [_"capDL: A language for describing capability-based systems"_][CapDL]
  Proceedings of the 1st Asia-Pacific Workshop on Systems (APSys), pp. 31–36,
  New Delhi, India, August, 2010

  [CapDL]: https://trustworthy.systems/publications/papers/Kuz_KLW_10.pdf

## License

The files in this repository are released under standard open source licenses.
Please see individual file headers and the `LICENSE_BSD2`.txt file for details.
