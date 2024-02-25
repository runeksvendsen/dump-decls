#!/usr/bin/env bash

# Run CI steps locally

set -euxo pipefail

# shellcheck disable=SC2016
SHELL_NIX_STR='${{ matrix.nixShell }}'
SHELLS="shell.nix shell-902.nix"

for shell in $SHELLS; do
  sed 's/'"$SHELL_NIX_STR"'/'"$shell"'/' .github/workflows/cabal-in-nix-shell.yml| \
  grep -v -e '^$'| \
  grep 'run: nix-shell'| \
  sed 's/[ ]*run: //' | \
  while read line; do eval "$line"; done
done
