with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, stdenv }:
             mkDerivation {
               pname = "rivers";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex05_1";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
