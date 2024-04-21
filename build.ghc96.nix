(import ./nix/pkgs.nix).pkgs.haskell.packages.ghc96.callCabal2nix "dump-decls" ./. { }
