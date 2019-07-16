This package defines additional commands to kill and delete text.
Most notably it defines "smarter" variants of some built-in
commands which delete text.  Where the built-in command always
deletes text the variant defined here instead kills the text if
(and only if) the previous command was a kill command.

Note that this package is not namespace-safe.
