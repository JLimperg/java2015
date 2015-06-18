{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv }:
      mkDerivation {
        pname = "bool";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ base ];
        homepage = "https://github.com/JLimperg/java2015";
        description = "Haskell interpretation of ex07_2";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
