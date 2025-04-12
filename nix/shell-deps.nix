with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs.haskell.compiler.native-bignum.ghc965
    pkgsUnstable.cabal-install
    pkgs.git
    pkgs.zlib # needed for building haskell-language-server
    pkgs.hlint
  ];
}
