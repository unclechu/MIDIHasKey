let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, dev  ? false
}:
let
  hpWithUtils = pkgs.haskellPackages.extend (self: super:
    packageSingleton midiHasKeyUtils
  );

  package = hp: dir: rec {
    name = baseNameOf (toString dir);
    pkg  = hp.callCabal2nix name (pkgs.lib.cleanSource dir) {};
  };

  packageSingleton = { name, pkg }: { "${name}" = pkg; };

  midiHasKeyUtils  = package pkgs.haskellPackages ./midihaskey-utils;
  midiHasKey       = package hpWithUtils          ./midihaskey;
  midiHasKeyJackHs = package hpWithUtils          ./midiplayer-jack-hs;

  haskellWithAll = hpWithUtils.extend (self: super:
    packageSingleton midiHasKeyUtils // # Fork from "haskellWithUtils" doesn’t help to inherit it
    packageSingleton midiHasKey //
    packageSingleton midiHasKeyJackHs
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
    ] else [];
in
pkgs.stdenv.mkDerivation {
  name = "MIDIHasKey";

  buildInputs = [
    (onlyExecutables midiHasKey.pkg)
    (onlyExecutables midiHasKeyJackHs.pkg)
  ] ++ devDependencies;
}
