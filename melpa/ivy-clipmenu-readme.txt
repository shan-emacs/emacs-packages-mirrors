Ivy integration with the clipboard manager, clipmenu.  Essentially, clipmenu
turns your system clipboard into a list.

To use this module, you must first install clipmenu and ensure that the
clipmenud daemon is running.  Refer to the installation instructions at
https://github.com/cdown/clipmenu for those details.

This module intentionally does not define any keybindings since I'd prefer
not to presume my users' preferences.  Personally, I use EXWM as my window
manager, so I call `exwm-input-set-key' and map it to `ivy-clipmenu-copy'.

Usually clipmenu integrates with rofi or dmenu.  This Emacs module integrates
with ivy.  Launch this when you want to select a clip.

Clipmenu itself supports a variety of environment variables that allow you to
customize its behavior.  These variables are respected herein.  If you'd
prefer to customize clipmenu's behavior from within Emacs, refer to the
variables defined in this module.

For more information:
- See `clipmenu --help`.
- Visit github.com/cdown/clipmenu.
