This package mainly defines `flymake-quickdef-backend', a macro
which helps remove some of the boilerplate code from defining new
Flymake backend functions.  Consult the function's documentation
for full information on use.  The macro defines a function which is
suitable for use with `flymake-diagnostic-functions' and handles
running the external process, creating and removing any necessary
files and buffers, and regex matches against diagnostic output.

Users defining a new check function with the macro provide Lisp
forms giving the command line arguments of the external process, a
regular expression to search its output, and Lisp forms to
processes the regex matches and produce arguments for
`flymake-make-diagnostic'.

See the documentation of `flymake-quickdef-backend' for more
information on how to use the macro.
