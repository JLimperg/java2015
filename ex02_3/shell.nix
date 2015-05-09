with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, friendly-time, lens-family
             , lens-family-th, pretty, stdenv, time
             }:
             mkDerivation {
               pname = "newsfeed";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 base containers friendly-time lens-family lens-family-th pretty
                 time
               ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex02_3";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
