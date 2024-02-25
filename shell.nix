with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    release-21-05.cabal-install
    pkgs.git
  ];
}
