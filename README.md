# MIDIHasKey

Virtual MIDI keyboard for microtonal music.

It works with [JACK Audio Connection Kit](http://jackaudio.org/) but technically it can work with
anything you name. You’d just have to create an application or a script that handles the events that
are coming from MIDIHasKey’s stdout.

One of the main reasons to create MIDIHasKey is to use all keyboard keys efficiently in context of
microtonal music.  For example [Jack Keyboard](http://jack-keyboard.sourceforge.net/) is trying to
simulate the feel of a real piano keyboard by imitating its layout of white and black keys. It
doesn’t make any sense for microtonal scales such as 17tet, 19tet, 22tet and many other. Those
scales won’t be devided in equal octaves on the piano keyboard, and we have some unused keys,‥ so
wasteful when we have just about 2 and half octaves in 12tet scale and even less in other micro
scales.

![Screenshot](artwork/readme-screenshot.png)

## WARNING!

**Work in progress! It’s playable but some stuff can be changed or is only partially implemented.**

See https://github.com/metachronica/audio-midihaskey/projects/1 page that about progress of first
release.

For now you can:
* Play notes by pressing keys on keyboard you set to handle
* Play notes by pressing GUI buttons
* Trigger note-offs for everything by pressing `Panic` button
* Change base pitch, octave, MIDI channel to play to
* Store settings between application restarts

## Supported OS

* GNU/Linux

## Requirements

* [GTK3](https://www.gtk.org/)
* [JACK Audio Connection Kit](http://jackaudio.org/)  
  For included C++ JACK MIDI Player application. It’s optional (if you have your own app for that)
  because MIDIHasKey just writes events to stdout (in its own format) and you can redirect it
  wherever you want.  
  **TODO** For me: Document the MIDI Player API. For now you can reverse-engeneer it by looking
  inside [this file](midiplayer-jack-cpp/src/main.c++) for instance. It also will change in the
  future.
* [GCC](https://gcc.gnu.org/) __>=6.4.*__ (maybe lower, but must support C++17)  
  To build C++ JACK MIDI Player (it’s optional too if you use your own one).

## Usage

### Using [Nix](https://nixos.org/nix/)

This is the recommended way to run it. It’s also the most simple one.

``` sh
nix-shell --pure --arg with-midiplayer-jack-cpp true --run 'midihaskey /dev/input/by-id/usb-xxxx_yyyy-event-kbd | midiplayer-jack-cpp'
```

Where `/dev/input/by-id/usb-xxxx_yyyy-event-kbd` is your keyboard device path
(you must have access to read from that file for your current user).

*N.B. `--pure` is optional. It just proves that MIDIHasKey has everything it needs provided by Nix.*

#### Tuning `nix-shell`

Just look at [default.nix](default.nix)’s arguments to see available options, it’s pretty flexible.

#### Development mode

You can enter `nix-shell` in development mode to have `ghci` repl with all the dependencies inside
as well as some other development-related stuff:

``` sh
nix-shell --arg run false --arg dev true
```

*N.B. `--arg run false` is optional, it just prevents final executables from building and populating
your `PATH`. If you’re developing you may not need them.*

##### Development mode for C++ JACK MIDI Player

``` sh
nix-shell --arg dev true --arg with-midiplayer-jack-cpp true
```

*N.B. There is no `--arg run false` here because you might want to test it with `midihaskey`.*

### Using [Stack](https://haskellstack.org)

``` sh
stack build --install-ghc
(cd midiplayer-jack-cpp && make)
stack exec -- midihaskey /dev/input/by-id/usb-xxxx_yyyy-event-kbd | midiplayer-jack-cpp/build/midiplayer-jack-cpp
```

# Author

Viacheslav Lotsmanov

# License

[GNU/GPLv3](LICENSE)
