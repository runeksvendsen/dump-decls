#!/usr/bin/env bash

set -e

NIX_PATH="nixpkgs=https://github.com/NixOS/nixpkgs/archive/release-23.05.tar.gz" nix-shell -p haskell.compiler.ghc96 -p cabal-install --run 'cd /home/runeks/code/ghc/utils/dump-decls; cabal build all'

