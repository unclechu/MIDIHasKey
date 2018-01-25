# MIDIHasKey

Virtual MIDI keyboard for microtonal music.
It works with [JACK Audio Connection Kit](http://jackaudio.org/).

One of the reason to create it is to use all keys efficiently in sense of microtonal music.
For example [Jack Keyboard](http://jack-keyboard.sourceforge.net/) trying to simulate feel of real
piano keyboard by immitating layout of white and black keys that doesn't makes any sense in
microtonal scales such as 17tet, 19tet, 22tet and any other, because these won't be devided in
equal octaves, and we have some unused keys,‥ so wasteful when we have just about 2 and half octaves
in 12tet scale and even less in other micro scales.

## WARNING!

**Work in progress! Do not expect anything for now.**

See https://github.com/metachronica/audio-midihaskey/projects/1 page that about progress of first
release.

## Supported OS

* GNU/Linux

## Requirements

* [wxWidgets](http://wxwidgets.org/)
* [JACK Audio Connection Kit](http://jackaudio.org/)
* [GCC](https://gcc.gnu.org/) __>=6.4.*__ (maybe lower, but must support C++17)

## Usage (from build to run)

```bash
$ ./env.sh stack build --install-ghc
$ (cd midiplayer && make)
$ env PATH="midiplayer/build:$PATH" stack exec midihaskey -- /dev/input/by-id/usb-04b4_6018-event-kbd
```

Where `/dev/input/by-id/usb-04b4_6018-event-kbd` is your keyboard device path.

## More info

Real-time critical part (see [midiplayer](./midiplayer)) written in C++ because Haskell runtime with
[jack](http://hackage.haskell.org/package/jack) package produces a lot of XRUNs, especially when
"Panic" is triggered (it triggers 2048 MIDI events). I've tried to optimize that by avoiding lists,
using `IOArray`s (only for "Panic") and `IORef`s but it haven't helped. MIDI events still generated
and constructed in Haskell code, this C++ part just absorbs events as byte-code from stdin, puts
them to queue and then triggers them in process callback, this works perfect.

# Known issues

* https://github.com/commercialhaskell/stack/issues/2299

  `libwxc.so` could not be found during build process.  
  Build **MIDIHasKey** by this command to solve it (you need to run this twice first time):

  ```bash
  $ ./env.sh stack build --install-ghc
  ```

# Author

Viacheslav Lotsmanov

# License

[GNU/GPLv3](./LICENSE)
