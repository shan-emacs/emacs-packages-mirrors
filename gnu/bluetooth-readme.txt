This package implements basic Bluetooth management functionality, such as
connecting and disconnecting devices, setting properties and aliases,
putting the adapter in discovery mode and controlling its power supply.
It also includes a pairing agent.
It uses the Bluez Bluetooth stack on GNU/Linux via the D-Bus interface.
Therefore, it requires an Emacs with D-Bus support compiled-in.

To use the package, invoke `bluetooth-list-devices'.