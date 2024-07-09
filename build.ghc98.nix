import ./default.nix
  { nixpkgs = (import ./nix/pkgs.nix).pkgs98;
    compiler = "ghc98";
  }
