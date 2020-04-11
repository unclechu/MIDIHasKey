let
  nixpkgsSnapshot = {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.09"; # Git commit hash: d5291756487d70bc336e33512a9baf9fa1788faf
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };

  ambientNixpkgs = import <nixpkgs> {};
in
{ nixpkgs ? ambientNixpkgs.pkgs.fetchFromGitHub nixpkgsSnapshot
, dev ? false
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

  package = haskellContext: dir:
    let
      name = baseNameOf (toString dir);
      pkg  = haskellContext.callCabal2nix name (pkgs.lib.cleanSource dir) {};
    in
      { inherit name pkg; };

  packageSingleton = { name, pkg }: { "${name}" = pkg; };

  midiHasKeyUtils = package haskell ./midihaskey-utils;
  midiHasKey = package haskellWithUtils ./midihaskey;
  midiHasKeyJackHs = package haskellWithUtils ./midiplayer-jack-hs;

  haskellWithUtils = haskell.override {
    overrides = self: super: packageSingleton midiHasKeyUtils;
  };

  haskellWithAll = haskellWithUtils.override {
    overrides = self: super:
      packageSingleton midiHasKeyUtils // # Fork from "haskellWithUtils" doesn't help to inherit it
      packageSingleton midiHasKey //
      packageSingleton midiHasKeyJackHs;
  };

  onlyExecutables =
    if dev then (x: x) else pkgs.haskell.lib.justStaticExecutables;

  devDependencies =
    if dev then [
      (haskellWithAll.ghcWithPackages (p: [
        p.midihaskey-utils
        p.midihaskey
        p.midiplayer-jack-hs

        p.jack
      ]))

      pkgs.cabal-install
    ] else [];
in
pkgs.stdenv.mkDerivation {
  name = "MIDIHasKey";

  buildInputs = [
    (onlyExecutables midiHasKey.pkg)
    (onlyExecutables midiHasKeyJackHs.pkg)
  ] ++ devDependencies;
}
