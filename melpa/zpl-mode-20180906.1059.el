;;; zpl-mode.el --- ZIMPL major mode

;; URL: https://github.com/ax487/zpl-mode.git
;; Package-Version: 20180906.1059
;; Package-Commit: 35e7e23c6baf31b5e65dd7405c8ab9b13c70637e
;; Version: 20180712
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a major mode for
;; ZIMPL (Zuse Institute Mathematical Programming Language).
;; It supports basic font-lock highlights.

;;; Code:

(defvar zpl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `zpl-mode'.")

(defvar zpl-keywords '("set" "param" "var"
                         "defset" "defnumb" "defstrg" "defbool"
                         "minimize" "maximize"
                         "subto"
                         "include"
                         "and" "or" "xor" "not"
                         "if" "then" "else" "end"
                         "forall" "in" "do"
                         "without" "with"
                         "binary" "integer" "implicit binary" "real"
                         "scale" "separate" "checkonly" "indicator"
                         "read" "as" "skip" "use" "comment"))

(defvar zpl-builtins '(
                          "union" "cross" "inter" "in" "symdiff" "proj"
                          "argmin" "argmax"
                          "print" "check"
                          "min" "max" "mod" "abs" "sgn" "floor" "ceil" "round"
                          "sum" "prod" "card" "random" "ord"
                          "sqrt" "log" "ln" "exp"
                          "substr" "lenth"
                          "powerset" "indexset" "subsets"))

;; (regexp-opt zpl-keywords)
;; (regexp-opt zpl-constants)

(defvar zpl-constants '("infinity"))

;; (defvar zpl-font-lock-keywords
;;   `(,(regexp-opt zpl-keywords) . font-lock-constant-face)
;;   "Keyword highlighting specification for `zpl-mode'.")

(defvar zpl-font-lock-keywords
  (list `(,(regexp-opt zpl-keywords 'words) . font-lock-keyword-face)
        `(,(regexp-opt zpl-constants 'words) . font-lock-constant-face)
        `(,(regexp-opt zpl-builtins 'words) . font-lock-builtin-face))
  "Keyword highlighting specification for `zpl-mode'.")

;; (defvar zpl-imenu-generic-expression
;;   ...)

;; (defvar zpl-outline-regexp
;;   ...)

 ;;;###autoload
(define-derived-mode zpl-mode fundamental-mode "ZIMPL"
  "A major mode for editing ZIMPL files."
  :syntax-table zpl-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(zpl-font-lock-keywords))
                                        ;(setq-local indent-line-function 'zpl-indent-line)
                                        ;(setq-local imenu-generic-expression
                                        ;            zpl-imenu-generic-expression)
                                        ;(setq-local outline-regexp zpl-outline-regexp)
  )

(add-to-list 'auto-mode-alist '("\\.zpl\\'" . zpl-mode))

(provide 'zpl-mode)

;;; zpl-mode.el ends here
