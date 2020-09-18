;;; ron-mode.el --- Rusty Object Notation mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daniel Hutzley
;; This work is licensed under the terms of the BSD 2-Clause License ( https://opensource.org/licenses/BSD-2-Clause )
;; Some inspiration was drawn from Devin Schwab's RON major mode, most predominantly in the indentation function.
;; SPDX-License-Identifier: BSD-2-Clause

;; Author: Daniel Hutzley <endergeryt@gmail.com>
;; URL: https://chiselapp.com/user/Hutzdog/repository/ron-mode/home
;; Package-Version: 20200830.1554
;; Package-Commit: c5e0454b9916d6b73adc15dab8abbb0b0a68ea22
;; Version: 1
;; Package-Requires: ((emacs "24.5.1"))
;; Keywords: languages


;;; Commentary:
;; Syntax highlights Rusty Object Notation, see https://github.com/ron-rs/ron

;;; Code:

(defvar ron-highlights nil "Highlights for Rusty Object Notation.")
(defvar ron-indent-offset 4)
(defvar ron-mode-syntax-table nil "Ron Mode Syntax Table")

(setq ron-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))

(setq ron-highlights
      '(; Comments
        ("//.*\\(TODO\\|FIXME\\|XXX\\|BUG\\).*" . (1 font-lock-warning-face))

       ; Constant face
        ("true\\|false" . font-lock-constant-face)
        ("[0-9]+"       . font-lock-constant-face)

        ; Function name face
        ("[A-Z]\\([a-zA-Z\\-]*\\)" . font-lock-function-name-face)

        ; Keyword face
        ("[a-z]\\([a-zA-Z\\-]*\\)" . font-lock-keyword-face)))

(defun ron-indent-line ()
  "Handles line indentation."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{\\(]")
              (setq indent-col (+ indent-col ron-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}\\)]") (>= indent-col ron-indent-offset))
        (setq indent-col (- indent-col ron-indent-offset))))
    (indent-line-to indent-col)))


(define-derived-mode ron-mode prog-mode "ron"
  "Major mode for Rusty Object Notation"
  (setq font-lock-defaults '(ron-highlights))
  (setq tab-width ron-indent-offset)
  (setq indent-line-function #'ron-indent-line)
  (setq indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.ron" . ron-mode))
(provide 'ron-mode)

;;; ron-mode.el ends here
