with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs98.haskell.compiler.ghc98
    pkgsUnstable.cabal-install
    pkgs.git
    pkgs.zlib # needed for building haskell-language-server
  ];
}
