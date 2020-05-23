lsp-origami provides support for origami.el using language server
protocol's "textDocument/foldingRange" functionality.  It can be enabled
with
(require 'lsp-origami)
(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)
