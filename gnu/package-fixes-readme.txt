This package fixes some critical bugs in package.el 1.0.1 which
cause bad .elc files to be created during package upgrades when a
macro changes.  It is designed to be required as a dependency by
packages whose installation is affected by these bugs.

This package can be safely installed on Emacs >= 25, in which
case it does nothing.