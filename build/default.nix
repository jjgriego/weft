{ pkgs ? import <nixpkgs> {} , hsCompiler ? "ghc8104"}:
pkgs.pkgs.haskell.packages.${hsCompiler}.callPackage ./builder.nix {}
