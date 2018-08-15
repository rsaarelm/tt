{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  f = { mkDerivation, base, hspec, intervals, optparse-applicative, parsec, stdenv }:
  mkDerivation {
    pname = "tt";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = [
      base hspec intervals optparse-applicative parsec ];

    homepage = "https://github.com/rsaarelm/tt/";
    description = "Command line time tracking and todo tool";
    license = stdenv.lib.licenses.bsd3;
  };

  haskellPackages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};
in
  if nixpkgs.lib.inNixShell then drv.env else drv
