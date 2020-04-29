Convert from a plain Elisp to an expression using a leaf.

This is accomplished by not just converting use-package's
keywords, but by converting them once they have been expanded
to a plain Elisp.

This may result in a simpler leaf expression, but if there is
no corresponding keyword, most of it will be converted to a
:config section.

Since the source is Elisp, it is possible to convert the leaf
to Elisp once and then convert it to leaf again.

This can be used to convert miscellaneous settings in the
:config section into leaf expressions with appropriate
keywords.  It is also possible to optimize settings that are
not needed.

Currently, the many ~leaf~ keywords are supported for
automatic conversion.

Please see https://github.com/conao3/leaf-convert.el to get
more information.
