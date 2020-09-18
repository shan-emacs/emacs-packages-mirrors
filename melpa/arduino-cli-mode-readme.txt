This package contains a wrapper for the official Arduino command line utility.
It aims to be as simple as possible, while still providing useful conveniences.

The structure of this package was inspired by ZachMassia's PlatformIO-Mode:
https://github.com/ZachMassia/PlatformIO-Mode/

Most of the fns consist of hairy imperative let*s that check/get
something from the cli.  Mostly parsing json or getting things from
maps that are structured in semi-coherent ways.

For more information on the wrapper, see the readme at https://github.com/motform/arduino-cli-mode
For more information on arduino-cli itself, see https://github.com/arduino/arduino-cli

Tested against arduino-cli >= 0.10.0
