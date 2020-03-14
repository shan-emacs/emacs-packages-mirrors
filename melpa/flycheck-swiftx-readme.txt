Add Swift support to Flycheck using Swift compiler frontend.

Features:

- Apple Swift 5 support.
- Integration with Xcode projects.
- The `xcrun` command support (only on macOS)

Usage:

See README.md

Debug:

In flycheck.el:flycheck-start-command-checker, add:
(when (equal checker 'swiftx) (message "%s %s" checker command))
