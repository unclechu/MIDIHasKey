let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, dev  ? false
}:
let
  hpWithUtils = pkgs.haskellPackages.extend (self: super:
    packageSingleton midihaskey-utils
  );

  package = hp: dir: rec {
    name = baseNameOf (toString dir);
    pkg  = hp.callCabal2nix name (pkgs.lib.cleanSource dir) {};
  };

  packageSingleton = { name, pkg }: { "${name}" = pkg; };

  midihaskey-utils   = package pkgs.haskellPackages ./midihaskey-utils;
  midihaskey         = package hpWithUtils          ./midihaskey;
  midiplayer-jack-hs = package hpWithUtils          ./midiplayer-jack-hs;

  haskellWithAll = hpWithUtils.extend (self: super:
    packageSingleton midihaskey-utils // # Fork from "haskellWithUtils" doesn’t help to inherit it
    packageSingleton midihaskey //
    packageSingleton midiplayer-jack-hs
  );

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

      pkgs.gnumake
      pkgs.pkg-config
      pkgs.jack2
    ] else [];

  midiplayer-jack-cpp = pkgs.stdenv.mkDerivation {
    name = "MIDIHaskKey-midiplayer-jack-cpp";
    src = ./midiplayer-jack-cpp;
    buildInputs = [ pkgs.jack2 pkgs.gnumake pkgs.pkg-config ];

    buildPhase = ''
      set -eu
      make
    '';

    installPhase = ''
      set -eu
      mkdir -p -- "$out"/bin
      cp -- build/midiplayer-jack-cpp "$out"/bin
    '';

    meta.license = pkgs.stdenv.lib.licenses.gpl3;
    meta.platforms = pkgs.stdenv.lib.platforms.linux;
  };
in
pkgs.stdenv.mkDerivation {
  name = "MIDIHasKey";

  buildInputs = [
    (onlyExecutables midihaskey.pkg)
    (onlyExecutables midiplayer-jack-hs.pkg)
    midiplayer-jack-cpp
  ] ++ devDependencies;

  meta.license = pkgs.stdenv.lib.licenses.gpl3;
  meta.platforms = pkgs.stdenv.lib.platforms.linux;
}
