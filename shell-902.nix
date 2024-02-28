with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgsUnstable.cabal-install
    pkgs.git
  ];

  shellHook = ''
    export CABAL_PROJECT_FILE="cabal-902.project"
  '';
}
