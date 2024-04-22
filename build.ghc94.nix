(import ./nix/pkgs.nix).pkgs.haskell.packages.ghc94.callCabal2nix "dump-decls" ./. { }
