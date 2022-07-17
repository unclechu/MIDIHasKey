let
  sources = import nix/sources.nix;
  availableBuildTools = [ "cabal" "stack" ];
in

{ pkgs ? import sources.nixpkgs {}

# When this file is called by nix-shell it’s set to `true` automatically.
, inNixShell ? false

# These flags are only for nix-shell (when `inNixShell` is `true`).
# `with-APPNAME` means APPNAME executable will be built and added to the shell.
, with-midihaskey ? false
, with-midiplayer-jack-hs ? false
, with-midiplayer-jack-cpp ? false
, buildTools ? [ "cabal" ] # See “availableBuildTools”
, withHoogle ? true
}:

assert builtins.all (x: builtins.elem x availableBuildTools) buildTools;

let
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) justStaticExecutables;

  hsPkgs = pkgs.haskellPackages.extend (self: super:
    (
      let
        dir = ./midihaskey-utils;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (lib.cleanSource dir) {};
      in
        { ${name} = pkg; }
    ) // (
      let
        dir = ./midihaskey;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (lib.cleanSource dir) {};
      in
        { ${name} = pkg // { exe = justStaticExecutables pkg; }; }
    ) // (
      let
        dir = ./midiplayer-jack-hs;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (lib.cleanSource dir) {};
      in
        { ${name} = pkg // { exe = justStaticExecutables pkg; }; }
    )
  );

  midiplayer-jack-cpp = pkgs.stdenv.mkDerivation {
    name = "MIDIHaskKey-midiplayer-jack-cpp";
    src = ./midiplayer-jack-cpp;
    nativeBuildInputs = [ pkgs.gnumake pkgs.pkg-config ];
    buildInputs = [ pkgs.jack2 ];

    buildPhase = ''
      make
    '';

    installPhase = ''(
      set -o nounset
      mkdir -p -- "$out"/bin
      cp -- build/midiplayer-jack-cpp "$out"/bin
    )'';

    meta = with lib; {
      homepage = "https://github.com/metachronica/audio-midihaskey";
      description = "MIDIHasKey JACK MIDI player written in C++";
      maintainers = with maintainers; [ unclechu ];
      license = licenses.gpl3;
      platforms = platforms.linux;
    };
  };

  shell = hsPkgs.shellFor {
    name = "MIDIHasKey-shell";

    packages = p: [
      p.midihaskey-utils
      p.midiplayer-jack-hs
      p.midihaskey
    ];

    inherit withHoogle;

    buildInputs =
      lib.optional (builtins.elem "cabal" buildTools) hsPkgs.cabal-install
      ++ lib.optional (builtins.elem "stack" buildTools) hsPkgs.stack
      ++ lib.optional with-midiplayer-jack-cpp midiplayer-jack-cpp
      ++ lib.optional with-midihaskey hsPkgs.midihaskey.exe
      ++ lib.optional with-midiplayer-jack-hs hsPkgs.midiplayer-jack-hs.exe;
  };
in

(if inNixShell then shell else {}) // {
  inherit shell midiplayer-jack-cpp;

  inherit (hsPkgs)
    midihaskey-utils
    midihaskey
    midiplayer-jack-hs
    ;
}
