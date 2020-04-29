Provides macros that allow you to declaratively configure
settings typical of an Elisp package with various keywords.

By separating the settings of a package and combining many
'leaves' of a package's settings, you could make a 'Yggdrasill'
on your Emacs.

A leaf can consist of multiple packages, in which case you can
disable all dependent child packages by disabling one parent's
package.

It also has a key management system and package management
uses the package.el.  With minimal external dependencies and
careful implementation, this package is guaranteed to be fully
functional from Emacs-24.4, now, and in future Emacs.

More information is [[https://github.com/conao3/leaf.el][here]]
