let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, run  ? true
, dev  ? false

, with-midihaskey          ? true
, with-midiplayer-jack-hs  ? true
, with-midiplayer-jack-cpp ? false
}:
let
  haskellPackagesWithUtils =
    pkgs.haskellPackages.extend (self: super: packageSingleton midihaskey-utils);

  package = haskellPackages: dir: rec {
    name = baseNameOf (toString dir);
    pkg  = haskellPackages.callCabal2nix name (pkgs.lib.cleanSource dir) {};
  };

  packageSingleton = { name, pkg }: { "${name}" = pkg; };

  midihaskey-utils   = package pkgs.haskellPackages     ./midihaskey-utils;
  midihaskey         = package haskellPackagesWithUtils ./midihaskey;
  midiplayer-jack-hs = package haskellPackagesWithUtils ./midiplayer-jack-hs;

  haskellPacakgesWithAll = haskellPackagesWithUtils.extend (self: super:
    (if with-midihaskey         then packageSingleton midihaskey         else {}) //
    (if with-midiplayer-jack-hs then packageSingleton midiplayer-jack-hs else {})
  );

  onlyExecutables =
    if dev then (x: x) else pkgs.haskell.lib.justStaticExecutables;

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

  runDependencies =
    (if with-midihaskey          then [(onlyExecutables midihaskey.pkg)]         else []) ++
    (if with-midiplayer-jack-hs  then [(onlyExecutables midiplayer-jack-hs.pkg)] else []) ++
    (if with-midiplayer-jack-cpp then [midiplayer-jack-cpp]                      else []);

  devDependencies =
    (if with-midihaskey || with-midiplayer-jack-hs then [
      (haskellPacakgesWithAll.ghcWithPackages (p:
        [ p.midihaskey-utils ] ++
        (if with-midihaskey         then [ p.midihaskey ]                else []) ++
        (if with-midiplayer-jack-hs then [ p.midiplayer-jack-hs p.jack ] else [])
      ))

      pkgs.cabal-install
    ] else []) ++

    (if with-midiplayer-jack-cpp then [
      pkgs.gnumake
      pkgs.pkg-config
      pkgs.jack2
    ] else []);
in
pkgs.stdenv.mkDerivation {
  name = "MIDIHasKey";

  buildInputs =
    (if run then runDependencies else []) ++
    (if dev then devDependencies else []);

  meta.license = pkgs.stdenv.lib.licenses.gpl3;
  meta.platforms = pkgs.stdenv.lib.platforms.linux;
}
