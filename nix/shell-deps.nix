with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgsUnstable.cabal-install
    pkgs.git
    pkgs.zlib # needed for building haskell-language-server
  ];
}