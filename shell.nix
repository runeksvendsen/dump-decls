with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgsUnstable.cabal-install
    pkgs.git
  ];

  shellHook = ''
    shopt -s expand_aliases # Expand aliases in non-interactive shells. Makes sure aliases work in CI.
    alias cabal='cabal --project-file=cabal.project'
    export PATH="$(pwd)/hls":$PATH
  '';
}
