with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, auto, base, containers, mtl, stdenv }:
             mkDerivation {
               pname = "bridges";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ auto base containers mtl ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex04_2 (contains ex04_1)";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
