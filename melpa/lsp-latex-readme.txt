lsp-mode client for LaTeX.
How to Use?
  - First, you have to install ~texlab~.
    Please install this from https://github.com/latex-lsp/texlab/releases .
  - Next, you should make ~lsp-mode~ available.
    See https://github.com/emacs-lsp/lsp-mode.
  - Now, you can use Language Server Protocol (LSP) on (la)tex-mode or
    yatex-mode just to evaluate this:

  (add-to-list 'load-path "/path/to/lsp-latex")
  (require 'lsp-latex)
  ;; "texlab" must be located at a directory contained in `exec-path'.
  ;; If you want to put "texlab" somewhere else,
  ;; you can specify the path to "texlab" as follows:
  ;; (setq lsp-latex-texlab-executable "/path/to/texlab")

  (with-eval-after-load "tex-mode"
   (add-hook 'tex-mode-hook 'lsp)
   (add-hook 'latex-mode-hook 'lsp))

  ;; For YaTeX
  (with-eval-after-load "yatex"
   (add-hook 'yatex-mode-hook 'lsp))

Note
  In this package, you can use even texlab v0.4.2 or older, written with Java,
  though it is not recommended.

License
  This package is licensed by GPLv3. See the file "LICENSE".
