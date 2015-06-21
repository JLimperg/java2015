with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, primitive, stdenv, text, vector
             }:
             mkDerivation {
               pname = "undo";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base containers primitive text vector ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex06_1";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
