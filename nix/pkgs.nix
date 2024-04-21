{ pkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/91a91b9ae468de8500dccb74c7896b1218e9c1de.tar.gz";
      sha256 = "1knklrnvd9syyc48vmbjj7q5il75i4k928qq2c2d7g3jm03k9aya";
    }) {};

  pkgsUnstable =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/6652baf493bb1092f9d7db1c0e29d493f10c9f16.tar.gz";
      sha256 = "13ijm1qjjn4walv4nyjcqwd3z9rhh2zadinywya0y7f4vnkp059k";
    }) {};
}
