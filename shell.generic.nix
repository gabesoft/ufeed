{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [ cabal-install ghci-pretty stack ]);
in
  pkgs.stdenv.mkDerivation rec {
    name = "ufeed-${version}";
    version = "1.0.0";
    buildInputs = [ ghc pkgs.fish pkgs.vim ];
  }
