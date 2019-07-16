 Emacs integration for nodenv.


Installation

Copy file `nodenv.el` to directory `~/.emacs.d/site-lisp/nodenv.el/`, for example, and add this to your .emacs to load the mode

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nodenv.el"))
(add-to-list 'exec-path (expand-file-name "~/.nodenv/shims"))
(require 'nodenv)
(add-hook 'js-mode-hook #'nodenv-mode)
