`cl' is a deprecated library, and elisp authors should use `cl-lib'
instead.  In most cases, this is a matter of requiring "cl-lib" and
adding a "cl-" prefix to symbols that came from "cl".

This library provides an interactive command, `cl-libify', which
replaces usages of "cl" symbols with their "cl-lib" equivalent,
optionally prompting for each

Note that some cl functions do not have exact replacements,
e.g. `flet', so further code changes might still be necessary.

You can also use `cl-libify-mark-cl-symbols-obsolete' to mark old
`cl' names as obsolete, so that the byte compiler will help flag
their use.
