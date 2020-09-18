;;; etc-sudoers-mode.el --- Edit Sudo security policies -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Peter Oliver.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Peter Oliver <git@mavit.org.uk>
;; Version: 1.0.0
;; Package-Version: 20200608.1555
;; Package-Commit: 52d5be9214185cfbba56e0b39bc4af474fc95f45
;; Package-Requires: ()
;; Keywords: languages
;; URL: https://gitlab.com/mavit/etc-sudoers-mode/

;;; Commentary:

;; This package provides syntax highlighting for the Sudo security
;; policy file, /etc/sudoers.

;;; Code:

;;;###autoload
(define-generic-mode 'etc-sudoers-mode
  '(?#)
  nil
  '(
    ("\\(?:^\\|\\W\\)\\(#include\\(?:dir\\)?\\)\\>"
     1 font-lock-preprocessor-face)
    ("\\(#\\|#[^[:digit:]].*\\)$" 1 font-lock-comment-face)
    ("\\(\".*\"\\)" 1 font-lock-string-face)
    ("^\\s *\\(Defaults\\)\\_>" 1 font-lock-keyword-face)
    ("^\\s *\\(User_Alias\\|Runas_Alias\\|Host_Alias\\|Cmnd_Alias\\|Defaults\\)\\(?:\\s +\\([A-Z][A-Z0-9_]+\\)\\_>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\_<\\(root\\|su\\)\\_>" 1 font-lock-warning-face)
    ("\\(\\*\\)" 1 font-lock-warning-face)
    ("\\(!\\)" 1 font-lock-keyword-face)
    ("\\(?:^\\|\\W\\)\\([%+][A-Za-z0-9_]+\\)\\>"
     1 font-lock-variable-name-face)
    ("\\(\\(?:NO\\)?\\(?:EXEC\\|FOLLOW\\|LOG_INPUT\\|LOG_OUTPUT\\|MAIL\\|PASSWD\\|SETENV\\)\\):"
     1 'font-lock-builtin-face)
    ("\\_<\\(ALL\\)\\_>" 1 'font-lock-constant-face)
    ("\\(\\\\\\)$" 1 font-lock-string-face))
  '("/sudoers\\>")
  '((lambda ()
      (setq font-lock-defaults '(generic-font-lock-keywords t))))
  "Generic mode for sudoers configuration files.")

;;;###autoload
(add-to-list 'auto-mode-alist '("/sudoers\\>" . etc-sudoers-mode))

(provide 'etc-sudoers-mode)

;;; etc-sudoers-mode.el ends here
