# MIDIHasKey

Virtual MIDI keyboard for microtonal music.
It works with [JACK Audio Connection Kit](http://jackaudio.org/).

## Requirements

* [wxWidgets](http://wxwidgets.org/)
* [JACK Audio Connection Kit](http://jackaudio.org/)

## WARNING!

**Work in progress! Do not expect anything for now.**

# Known issues

* https://github.com/commercialhaskell/stack/issues/2299

  Build **MIDIHasKey** by this command to solve it (need to run this twice first time):

  ```bash
  $ ./env.sh stack build --install-ghc
  ```

# Author

Viacheslav Lotsmanov

# License

[GNU/GPLv3](./LICENSE)
