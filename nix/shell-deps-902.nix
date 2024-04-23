with (import ./pkgs.nix);
{ nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgs.haskell.packages.ghc90.haskell-language-server
    pkgsUnstable.cabal-install
    pkgs.git
  ];
}
