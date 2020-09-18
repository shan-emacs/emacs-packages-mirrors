lsp-focus provides support for focus.el using language server
protocol's "textDocument/foldingRange" functionality.  It can be enabled
with
(require 'lsp-focus)
(add-hook 'focus-mode-hook #'lsp-focus-mode)
