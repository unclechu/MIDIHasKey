let
  sources = import nix/sources.nix;
  availableBuildTools = [ "cabal" "stack" ];
in

{ pkgs ? import sources.nixpkgs {}

, ghcVersion ? null # e.g. “--argstr ghcVersion ghc923”

, haskellPackages ?
    if isNull ghcVersion
    then pkgs.haskellPackages # The default one from the nixpkgs pin
    else
      assert builtins.isString ghcVersion;
      pkgs.haskell.packages.${ghcVersion}

# When this file is called by nix-shell it’s set to `true` automatically.
, inNixShell ? false

# These flags are only for nix-shell (when `inNixShell` is `true`).
# `with-APPNAME` means APPNAME executable will be built and added to the shell.
, with-midihaskey ? false
, with-midiplayer-jack-hs ? false
, with-midiplayer-jack-cpp ? false
, buildTools ? [ "cabal" ] # See “availableBuildTools”
, withHoogle ? true # Generate Hoogle index
, withHLS ? true # Haskell Language Server (LSP) https://github.com/haskell/haskell-language-server
}:

assert builtins.all (x: builtins.elem x availableBuildTools) buildTools;

let
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) justStaticExecutables doJailbreak overrideCabal overrideSrc;
  cleanSource = pkgs.callPackage nix/clean-source.nix {};

  # Some fixes to make it build successfully with GHC 9.2.3
  overridesForGHC923 = self: super: {
    linux-evdev = overrideCabal (overrideSrc super.linux-evdev {
      src = fetchTarball {
        url = "https://github.com/bgamari/linux-evdev/archive/89869658c421b70e29fadd3e99ed75139048aced.tar.gz";
        sha256 = "1i37fibalrqw86kkp02nzgahdv4ja6ckh6yfrb3165hi8jkq8r58";
      };
    }) {
      patches = [ nix/linux-evdev-cabal-fix.patch ];
    };

    qm-interpolated-string = overrideCabal super.qm-interpolated-string {
      version = "0.3.1.0";
      sha256 = "sha256-U1x8iSZvuaT7G7RotNKR/C24CTVfECFnlrnNetecXYo=";
    };

    midi = overrideCabal super.midi {
      version = "0.2.2.3";
      sha256 = "sha256-dRLrW0JbJtSWhb269BpTLSe9AtfyevpasZ/Otg9Mcos=";
    };

    jack = doJailbreak super.jack;

    singletons-th = overrideCabal super.singletons-th {
      version = "3.1";
      sha256 = "sha256-6tRWxCHrKOV1gJNexeTYyfnoSITZZ4iPU/0f3pQ+HdY=";
    };
  };

  hsPkgs = (haskellPackages.override {
    overrides =
      if builtins.elem ghcVersion ["ghc923"]
      then overridesForGHC923
      else _: _: {};
  }).extend (self: super:
    (
      let
        dir = ./midihaskey-utils;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (cleanSource dir) {};
      in
        { ${name} = pkg; }
    ) // (
      let
        dir = ./midihaskey;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (cleanSource dir) {};
      in
        { ${name} = pkg // { exe = justStaticExecutables pkg; }; }
    ) // (
      let
        dir = ./midiplayer-jack-hs;
        name = baseNameOf (toString dir);
        pkg = self.callCabal2nix name (cleanSource dir) {};
      in
        { ${name} = pkg // { exe = justStaticExecutables pkg; }; }
    )
  );

  midiplayer-jack-cpp = pkgs.stdenv.mkDerivation {
    name = "MIDIHaskKey-midiplayer-jack-cpp";
    src = cleanSource ./midiplayer-jack-cpp;
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
      ++ lib.optional withHLS hsPkgs.haskell-language-server
      ++ lib.optional with-midihaskey hsPkgs.midihaskey.exe
      ++ lib.optional with-midiplayer-jack-hs hsPkgs.midiplayer-jack-hs.exe
      ++ lib.optional with-midiplayer-jack-cpp midiplayer-jack-cpp;
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
