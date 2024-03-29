{ pkgs ? (import ./nix/pkgs.nix).pkgs
, hls-version ? "2.6.0.0"
, exe-dir-expression ? "$(pwd)/hls"
, shellNixFile ? ./shell.nix
}:
let nix = pkgs.nixVersions.nix_2_14;
    coreutils = pkgs.coreutils;
    realpath = "${coreutils}/bin/realpath";
in
pkgs.writeScriptBin "build-hls.sh" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i bash ${shellNixFile}

    set -e

    EXE_DIR="${exe-dir-expression}"

    cd "$(mktemp -d)"
    git clone https://github.com/haskell/haskell-language-server.git
    cd haskell-language-server
    git checkout ${hls-version}
    cabal build exe:haskell-language-server
    cabal install --installdir "$EXE_DIR" --overwrite-policy=always exe:haskell-language-server
    echo "Built haskell-language-server v${hls-version}"
    echo "Executables installed in $(realpath $EXE_DIR)"
''