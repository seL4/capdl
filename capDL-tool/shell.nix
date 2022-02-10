#
# Copyright 2022, UNSW
#
# SPDX-License-Identifier: BSD-2-Clause
#

with import
  (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-18.09";
    url = "https://github.com/nixos/nixpkgs/";
    # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/heads/nixos-18.09";
    rev = "a7e559a5504572008567383c3dc8e142fa7a8633";
  })
{ };
mkShell {
  buildInputs = [ haskell.compiler.ghc802 ];
}
