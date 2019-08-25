ovpn-mode expects configurations to be named with the name.ovpn convention

M-x ovpn will pop you into the default configuration directory and list any
existing .ovpn files from there.  You can then interact with that listing with
the following single key commands:

s: start the selected ovpn
n: start the selected ovpn in a dedicated namespace
q: stop the selected ovpn
r: restart the selected ovpn

There's a simple color coding scheme that tells you which state a given ovpn
is in:

red: ovpn has stopped/dropped/broken, use q to reset/purge, or b to debug
pink: VPN is in the process of initializing
blue: namespaced VPN is ready for use
green: system wide VPN is ready for use

Additionally you have available:

i: remote link info for the selected ovpn
b: switch to the output buffer for the selected ovpn
e: edit the selected ovpn
d: set the active vpn conf directory
~: apply a keyword filter to the current conf listing
6: toggle ipv6 support on/off (automatically called on start of ovpn)
x: execute an asynchronous shell command in the context of any associated namespace
T: spawn a terminal in the context of any associated namespace
B: spawn a browser in the context of any associated namespace
a: show all active vpn configurations across all conf directories
h: describe mode

ovpn-mode will maintain state for any running configurations, so you can
switch between multiple directories and keep state accordingly.
