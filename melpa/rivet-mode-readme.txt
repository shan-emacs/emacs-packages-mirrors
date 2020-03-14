Rivet mode is a minor mode for editing Apache Rivet files. It automatically
detects whether TCL or HTML is currently being edited and uses the major
modes tcl-mode and web-mode, respectively.

By default, `rivet-mode' requires `tcl' (built-in) and `web-mode'. To use another mode,
customize `rivet-mode-host-mode' and `rivet-mode-inner-mode' to suit.

The variable =rivet-mode-host-mode= determines the "host" major mode, which
is web-mode by default.

The variable =rivet-mode-inner-mode= determines the "inner" major mode, which
is the built-in tcl-mode by default.

The variable =rivet-mode-delimiters= defines the left and right delimiters
which demark the bounds of the "inner" major mode (TCL). These are "<?" and
"?>" by default. Note that the "<?=" delimiter, which marks the start of an
expression, still begins with "<?" and so will be caught.
