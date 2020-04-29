This Emacs package provides bindings for working with Gomacro, a
read eval print loop for Go.

Function for interfacing with the gomacro REPL are provided through
the "M-x" interface.  Most important functions to check out are:
- `gomacro-run'
- `gomacro-verbose-toggle'
- `gomacro-eval'
- `gomacro-eval-region'
- `gomacro-eval-line'
- `gomacro-eval-defun'
- `gomacro-eval-buffer'

When `gomacro-mode' is activated the following keybindings are
defined:
- C-M-x `gomacro-eval-defun'
- C-c C-r `gomacro-eval-region'
- C-c C-l `gomacro-eval-line'
- C-c C-t `gomacro-verbose-toggle'

For more information, see the readme at https://github.com/storvik/gomacro-mode
