with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgsUnstable.cabal-install
    pkgs.git
  ];

  shellHook = ''
    alias cabal='cabal --project-file=cabal.project'
    export PATH="$(pwd)/hls":$PATH
  '';
}
