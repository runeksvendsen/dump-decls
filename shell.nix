with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgsUnstable.cabal-install
    pkgs.git
  ];
}
