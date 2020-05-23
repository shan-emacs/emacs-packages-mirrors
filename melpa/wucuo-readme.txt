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

The `flyspell-mode' is turned on by `wucuo-start' by default.
See `wucuo-flyspell-start-mode' for other options.

If `wucuo-flyspell-start-mode' is "lite", `wucuo-start' calls
`flyspell-buffer' periodically.
If it's "lite", `wucuo-start' calls `flyspell-region' to check visible
region in current window periodically.

The interval of buffer checking or region checking is controlled
by `wucuo-update-interval'.
Checking bufffer or region only is more efficient than `flyspell-mode'.

Please note `flyspell-prog-mode' should not be enabled when using "wucuo".
`flyspell-prog-mode' could be replaced by "wucuo".

Or add one line setup if you prefer running `flyspell-buffer' manually:
  (setq flyspell-generic-check-word-predicate #'wucuo-generic-check-word-predicate)

Or setup for only one major mode when major mode has its own flyspell setup:
  (wucuo-setup-major-mode "js2-mode")

Instead of enabling `flyspell-mode' to check the word when inputting, you can use
`wucuo-spell-check-buffer' to spell check current buffer.

`wucuo-spell-check-buffer' uses `wucuo-update-interval',`wucuo-spell-check-buffer-max',
and `wucuo-spell-check-buffer-predicate' to ensure checking happen less frequently.
