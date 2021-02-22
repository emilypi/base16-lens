{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16, bytestring, Cabal
      , cabal-doctest, lens, lib, text, text-short
      }:
      mkDerivation {
        pname = "base16-lens";
        version = "0.1.3.1";
        src = ./.;
        setupHaskellDepends = [ base Cabal cabal-doctest ];
        libraryHaskellDepends = [
          base base16 bytestring lens text text-short
        ];
        homepage = "https://github.com/emilypi/base16-lens";
        description = "Optics for the Base16 library";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
