This package is provides a hexchat-like status bar for joined
channels in ERC.  It relies on the `erc-track' module, and displays
all of the same information that `erc-track' does in the mode line,
but in an alternative format in form of a sidebar.

Shout out to sidebar.el <https://github.com/sebastiencs/sidebar.el>
and outline-toc.el <https://github.com/abingham/outline-toc.el> for
the sidebar window management ideas.

Usage:

Use M-x erc-status-sidebar-open RET to open the ERC status sidebar
in the current frame.  Make sure that the `erc-track' module is
active (this is the default).

Use M-x erc-status-sidebar-close RET to close the sidebar on the
current frame.  With a prefix argument, it closes the sidebar on
all frames.

Use M-x erc-status-sidebar-kill RET to kill the sidebar buffer and
close the sidebar on all frames.
