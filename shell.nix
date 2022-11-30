{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc8107
    pkgs.zlib
  ];
}
