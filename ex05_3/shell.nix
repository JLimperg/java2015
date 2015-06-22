with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, stdenv, tasty, tasty-quickcheck }:
             mkDerivation {
               pname = "geometry";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base ];
               testDepends = [ base tasty tasty-quickcheck ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex02_1";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
