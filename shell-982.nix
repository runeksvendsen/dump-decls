with (import ./nix/pkgs.nix);
pkgs.mkShell {
  inherit (import ./nix/shell-deps-982.nix) nativeBuildInputs;

  shellHook = ''
    export CABAL_PROJECT_FILE="cabal.project"
    export PATH="$(pwd)/hls":$PATH
  '';
}
