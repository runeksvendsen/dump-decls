with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgsUnstable.cabal-install
    pkgs.git
  ];

  shellHook = ''
    export CABAL_PROJECT_FILE="cabal.project"
  '';
}
