with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgsUnstable.cabal-install
    pkgs.git
  ];

  shellHook = ''
    alias cabal='cabal --project-file=cabal-902.project'
    export PATH="$(pwd)/hls":$PATH
  '';
}
