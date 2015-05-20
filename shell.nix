{ pkgs ? import <nixpkgs> {} }:

let ghc = pkgs.haskell-ng.packages.ghc784;
    hepNixOverlay = pkgs.hepNixOverlay;
    stdenv = pkgs.stdenv;
in { ttbar = ghc.callPackage ({mkDerivation}:
  mkDerivation {
    pname = "ttbar";
    version = "0.1.999";
    src = ./lib;
    license = stdenv.lib.licenses.gpl3;
    buildDepends = with (ghc // hepNixOverlay); [
      binary conduit conduit-util HEPUtil hmatrix HROOT hepNixOverlay.LHCOAnalysis
      LHCOAnalysis-type LHEParser mersenne-random mtl text
      transformers
    ];
    buildTools = with ghc; [ cabal-install ];
  }) {};
}

