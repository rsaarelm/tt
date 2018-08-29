{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, filepath, hspec
      , intervals, optparse-applicative, parsec, stdenv, time
      }:
      mkDerivation {
        pname = "tt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers directory filepath intervals parsec time
        ];
        executableHaskellDepends = [
          base containers directory filepath intervals optparse-applicative
          parsec time
        ];
        testHaskellDepends = [ base hspec intervals time ];
        homepage = "https://github.com/rsaarelm/tt";
        description = "Command line time tracking and todo tool";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
