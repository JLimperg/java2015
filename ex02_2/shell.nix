with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, lens-family, lens-family-th
             , stdenv
             }:
             mkDerivation {
               pname = "menu";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base containers lens-family lens-family-th ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex02_2";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
