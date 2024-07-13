{ nixpkgs ? (import ../nix/pkgs.nix).pkgs
, compiler ? "ghc96"
}:
let
  args =
    { dump-decls-lib = import ../dump-decls-lib { inherit nixpkgs compiler; };
    };
in nixpkgs.haskell.packages.${compiler}.callCabal2nix "dump-decls-exe" ./. args
