`eshell-outline-mode' defines a few commands to integrate
`outline-minor-mode' into `eshell'.  Some eshell-specific keys have
been rebound so that they have multiple uses.

Namely, "C-c C-c" and "C-c C-k" will either kill/interrupt the
running process, or show/hide the prompt+output at point.

"C-c M-o" (`eshell-mark-output'/`eshell-outline-narrow') now uses
narrowing to clear the buffer as an analogue to
`comint-clear-buffer' and replacement for `eshell/clear'.  It is
also able to narrow to a previous prompt+output.

"C-c M-m" (undefined/`eshell-outline-mark') marks the prompt+output
at point, replacing "C-c M-o" which has been rebound.  If point is
at an empty prompt at the end of the buffer, this will mark the
previous prompt+output instead.

Because this mode doesn't actually enable `outline-minor-mode', I
also bind "C-c @" to `outline-mode-prefix-map'.

See the docstring of `eshell-outline-mode' for a full list of
keybindings.
