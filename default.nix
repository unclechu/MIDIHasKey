let
  nixpkgsSnapshot = {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.09";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };

  ambientNixpkgs = import <nixpkgs> {};
in
{ nixpkgs ? ambientNixpkgs.pkgs.fetchFromGitHub nixpkgsSnapshot
}:
let
  pkgs = import nixpkgs {};

  haskell = pkgs.haskell.packages.ghc865.override {
    overrides = self: super: {
      base-unicode-symbols = self.callPackage
        ({ mkDerivation, base }:
          mkDerivation {
            pname = "base-unicode-symbols";
            version = "0.2.4.2";
            sha256 = "0qkhp4ybmx4nbqqkrmw3hkm47bv61i2wpi20qb09wvk10g2dcr23";
            libraryHaskellDepends = [ base ];
            description = "Unicode alternatives for common functions and operators";
            license = pkgs.stdenv.lib.licenses.bsd3;
          }) {};
    };
  };

  midiHasKeyUtils =
    let
      dir = ./midihaskey-utils;
      name = baseNameOf (toString dir);
    in
      {
        inherit name;
        pkg = haskell.callCabal2nix name (pkgs.lib.cleanSource dir) {};
      };

  withUtils = haskell.override {
    overrides = self: super: with midiHasKeyUtils; { "${name}" = pkg; };
  };

  midiHasKey =
    let
      dir = ./midihaskey;
      name = baseNameOf (toString dir);
    in
      withUtils.callCabal2nix name (pkgs.lib.cleanSource dir) {};

  midiHasKeyJackHs =
    let
      dir = ./midiplayer-jack-hs;
      name = baseNameOf (toString dir);
    in
      withUtils.callCabal2nix name (pkgs.lib.cleanSource dir) {};
in
pkgs.stdenv.mkDerivation {
  name = "MIDIHasKey";

  buildInputs = [
    midiHasKey
    midiHasKeyJackHs
  ];
}
