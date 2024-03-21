{ pkgs ? (import nix/pkgs.nix).pkgs
}:
pkgs.haskell.packages.ghc96.callCabal2nix "dump-decls" ./. { }
