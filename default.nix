{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz") {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = (drv: pkgs.haskell.lib.addBuildTools drv (with pkgs;
    [ cabal-install haskell-language-server ]));
}
