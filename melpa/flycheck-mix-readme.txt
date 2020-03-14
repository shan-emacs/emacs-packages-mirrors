The package is deprecated and will be removed from melpa
Please use elixir-lsp instead https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/
This package adds support for Elixir mix to flycheck.
To use it, add to your init.el:

(require 'flycheck-mix)
(flycheck-mix-setup)

Elixir compilation uses macros and it can run arbitrary code.
You should use =elixir-mix= checker only with trusted projects.
