with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, stdenv }:
             mkDerivation {
               pname = "prime-printer";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base ];
               homepage = "https://github.com/JLimperg/java2015";
               description = "Haskell interpretation of ex01_1";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
