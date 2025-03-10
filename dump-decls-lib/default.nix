{ nixpkgs ? (import ../nix/pkgs.nix).pkgs
, compiler ? "ghc96"
}:
nixpkgs.haskell.packages.${compiler}.callCabal2nix "dump-decls-lib" ./. { }
