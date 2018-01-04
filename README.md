# MIDIHasKey

Virtual MIDI keyboard for microtonal music.
It works with [JACK Audio Connection Kit](http://jackaudio.org/).

One of the reason to create it is to use all keys efficiently in sense of microtonal music.
For example [Jack Keyboard](http://jack-keyboard.sourceforge.net/) trying to simulate feel of real
piano keyboard by immitating layout of white and black keys that doesn't makes any sense in
microtonal scales such as 17tet, 19tet, 22tet and any other, because these won't be devided in
equal octaves, and we have some unused keys,‥ so wasteful when we have just about 2 and half octaves
in 12tet scale and even less in other micro scales.

## Supported OS

* GNU/Linux

## Requirements

* [wxWidgets](http://wxwidgets.org/)
* [JACK Audio Connection Kit](http://jackaudio.org/)

## WARNING!

**Work in progress! Do not expect anything for now.**

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
