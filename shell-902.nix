with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgsUnstable.cabal-install
    pkgs.git
  ];
}
