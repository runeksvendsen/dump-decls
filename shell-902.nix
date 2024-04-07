with (import ./nix/pkgs.nix);
pkgs.mkShell {
  inherit (import ./nix/shell-deps-902.nix) nativeBuildInputs;

  shellHook = ''
    export CABAL_PROJECT_FILE="cabal-902.project"
  '';
}
