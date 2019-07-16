1. Setup
Please install either aspell or hunspell and its corresponding dictionaries.

2. Usage
Run `wucuo-start' to setup and start `flyspell-mode'.
It spell check camel case words in code.
Or just add "(wucuo-start)" into "~/.emacs".

Please note `flyspell-prog-mode' should not be enabled when using "wucuo".
`flyspell-prog-mode' could be replaced by "wucuo".

OR add one line setup if you prefer running `flyspell-buffer' manually:
 (setq flyspell-generic-check-word-predicate #'wucuo-generic-check-word-predicate)

OR setup for only one major mode:
 (put 'js2-mode 'flyspell-mode-predicate 'wucuo-generic-check-word-predicate)
