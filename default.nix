{ pkgs ? (import nix/pkgs.nix).pkgs
}:
pkgs.haskell.packages.ghc98.callCabal2nix "dump-decls" ./. { }
