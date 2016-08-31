{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, mtl, optparse-applicative
      , parsec, stdenv
      }:
      mkDerivation {
        pname = "cogs";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base bytestring parsec ];
        executableHaskellDepends = [ base mtl optparse-applicative ];
        homepage = "zachsully.com/cogs";
        description = "A language for numeric and symbolic computing";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
