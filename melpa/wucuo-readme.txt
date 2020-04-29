1. Setup
Please install either aspell or hunspell and its corresponding dictionaries.

2. Usage
Run `wucuo-start' to setup and start `flyspell-mode'.
It spell check camel case words in code.

To enable wucuo for all languages, insert below code into ".emacs",

  (setq wucuo-flyspell-start-mode "lite") ; optional
  (defun prog-mode-hook-setup ()
    (wucuo-start t))
  (add-hook 'prog-mode-hook 'prog-mode-hook-setup)

If `wucuo-flyspell-start-mode' is "lite", `wucuo-start' calls
`flyspell-buffer' periodically.
The interval of buffer checking is controlled by `wucuo-update-interval'.
It's more light weight than running `flyspell-mode'.

The `flyspell-mode' is turned on by `wucuo-start' by default.
See `wucuo-flyspell-start-mode' for other options.

Please note `flyspell-prog-mode' should not be enabled when using "wucuo".
`flyspell-prog-mode' could be replaced by "wucuo".

Or add one line setup if you prefer running `flyspell-buffer' manually:
  (setq flyspell-generic-check-word-predicate #'wucuo-generic-check-word-predicate)

Or setup for only one major mode when major mode has its own flyspell setup:
  (wucuo-setup-major-mode "js2-mode")
