with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgsUnstable.cabal-install
    pkgs.git
  ];
}
