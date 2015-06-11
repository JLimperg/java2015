with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, stdenv, text, vector }:
             mkDerivation {
               pname = "undo";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base text vector ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex06_1";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
