This package provides GNU Emacs major modes for editing Pest
grammar files.  Currently, it supports syntax highlighting,
indentation, imenu integration.

Syntax checking is available from flymake-pest or flycheck-pest.

Also, you can use `pest-test-grammar' to open a new buffer, in
which you can experiment with your language defined by the
grammar.  In this new buffer, you can use `pest-analyze-input'
(default keybinding: C-c C-c) to analyze the input, which will
give you an analysis report of the structure.  Also, if
`eldoc-mode' is enabled, put the point anywhere under an
grammatical element, a path on the parse tree will be shown in
the minibuffer.
