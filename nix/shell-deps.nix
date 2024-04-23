with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgs.haskell.packages.ghc96.haskell-language-server
    pkgsUnstable.cabal-install
    pkgs.git
    pkgs.zlib # needed for building haskell-language-server
  ];
}