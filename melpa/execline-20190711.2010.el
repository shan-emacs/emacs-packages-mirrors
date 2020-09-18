;;; execline.el --- Major mode for editing execline scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dmitry Bogatov

;; Author: Dmitry Bogatov <KAction@debian.org>
;; Keywords: tools, unix, languages
;; Package-Version: 20190711.2010
;; Package-Commit: c75dd9b2c54d8e59fc35fd4bd98d8e213948a3f5
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (s "1.6.0"))
;; License: GPL v3
;; Homepage: https://gitlab.com/KAction/emacs-execline

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing execline scripts.  Features:
;;
;;   * syntax highlighting of commends, builtin commands and variable substitution.
;;   * completion of builtin commands.
;;   * working "comment-region" command.
;;   * indentation of blocks
;;   * automatic enable of mode in *.exec files.
;;   * automatic enable of mode in files with "execlineb" interpreter
;;
;; Missing features:
;;
;;   * adaptive indentation.  Current indentation algorithm assumes that
;;     previous line indented by same algorithm.  Trying to use indentation,
;;     provided by this mode in buffer with script, indented with another style
;;     (say, with two spaces per indentation level), causes wrong results.

;;; Code:
(eval-when-compile (require 'subr-x)) ; for when-let*
(eval-when-compile (require 'cl-lib)) ; for incf, decf
(require 's)

(defvar execline-builtin-commands
  '(
    "background"
    "backtick"
    "cd"
    "define"
    "dollarat"
    "elgetopt"
    "elgetpositionals"
    "elglob"
    "emptyenv"
    "exec"
    "exitcodes"
    "exit"
    "export"
    "fdblock"
    "fdclose"
    "fdmove"
    "fdreserve"
    "fdswap"
    "forbacktickx"
    "foreground"
    "forstdin"
    "forx"
    "getcwd"
    "getpid"
    "heredoc"
    "homeof"
    "ifelse"
    "if"
    "ifte"
    "ifthenelse"
    "importas"
    "loopwhilex"
    "multidefine"
    "multisubstitute"
    "pipeline"
    "piperw"
    "redirfd"
    "runblock"
    "shift"
    "trap"
    "tryexec"
    "umask"
    "unexport"
    "upgrade"
    "wait"
    "withstdinas"
    )
  "List of commands, provided by execline distribution.")

(defun execline-completion-at-point ()
  "Function used for `completion-at-point-functions' in `execline-mode'.

Complete name of command, provided by execline distribution."
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (start (car bounds))
              (end   (cdr bounds)))
      (list start end execline-builtin-commands)))

(defun execline--current-line-empty? ()
  "Return t if current line is empty."
  (eq (line-beginning-position) (line-end-position)))

(defun execline--previous-line ()
  "Return line above the point as string."
  (let* ((begin (line-beginning-position 0))
         (end   (line-end-position 0)))
    (buffer-substring-no-properties begin end)))

(defun execline--strip-leading-whitespaces ()
  "Remove leading whitespace in current line."
  (while (and (execline--whitespace? (char-after (line-beginning-position)))
              (not (execline--current-line-empty?)))
    (save-excursion
      (beginning-of-line)
      (delete-char 1))))

(defun execline--whitespace? (char)
  "Return t if CHAR has whitespace syntax category."
  (eq #x20 (char-syntax char)))

(defun execline--current-line ()
  "Return line containing the point."
  (let* ((begin (line-beginning-position))
         (end   (line-end-position)))
    (buffer-substring-no-properties begin end)))

(defun execline--count-leading-whitespace (string)
  "Return count of leading whitespace characters in STRING."
  (let ((without-indent (s-trim-left string)))
    (- (length string) (length without-indent))))

;; Indenting empty line
;; ~~~~~~~~~~~~~~~~~~~~
;;
;; Code in `execline-indent-line-function' saves current point while indenting,
;; so when user calls indention function in middle of line (e.g presses TAB),
;; point stays in same position, relative to other text in that line.
;;
;; Described behaviour is desirable in most cases, but when indenting empty
;; line, it results in indentation inserted after the point: point stays at
;; beginning of line.
;;
;; Point can also stay behind indentation if indentation function is called
;; when point is in indentation of non-empty line.
;;
;; These cases are handled specially: point is moved past indentation.

;; Assumption about previous line
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; It is assumed, that previous line is indented with tabs. This is
;; how `execline-mode' indents, but call to `indent-line' in script,
;; pre-indented with spaces lead to broken re-indentation.

(defun execline-indent-line-function ()
  "Indent line of execline script.

If previous line ends with opening brace, indent current line one
level deeper then previous line; if current line starts with
closing brace, indent current line one level shallower then
previous line; indent same as previous line otherwise."
  ; See [Assumption about previous line]
  (let* ((prev (execline--previous-line))
         (this (execline--current-line))
         (prev-indent (execline--count-leading-whitespace prev)))
    (execline--strip-leading-whitespaces)
    (let ((new-indent prev-indent))
      (when (s-ends-with?   "{" (s-trim-right prev))
        (cl-incf new-indent))
      (when (s-starts-with? "}" (s-trim-left  this))
        (cl-decf new-indent))
      (save-excursion
        (beginning-of-line)
        (indent-to-column (* tab-width new-indent)))
      ;; See note [Indenting empty line]
      (when (and (bolp) (> new-indent 0))
        (forward-whitespace 1)))))

(defvar execline-font-lock-keywords
  (let ((keywords-rx         (regexp-opt execline-builtin-commands 'words))
        (comment-rx          (rx "#"  (0+ nonl)))
        (variable-rx         (rx "$"  (1+ (or (syntax word) (syntax symbol)))))
        (variable-{-rx       (rx "${" (1+ (or (syntax word) (syntax symbol))) "}"))
        (variable-special-rx (rx "$"  (any digit ?\@))))
    `(
      (,comment-rx          . 'font-lock-comment-face)
      (,variable-rx         . 'font-lock-variable-name-face)
      (,variable-{-rx       . 'font-lock-variable-name-face)
      (,variable-special-rx . 'font-lock-preprocessor-face)
      (,keywords-rx         . 'font-lock-builtin-face)
      )))

;;;###autoload
(define-derived-mode execline-mode prog-mode "execline"
  "Major mode for editing execline scripts.

For more information about execline, visit https://skarnet.org/software/execline.
\\{execline-mode-map}"
  (setq-local comment-start "#")
  (setq-local font-lock-defaults '((execline-font-lock-keywords)))
  (setq-local indent-line-function #'execline-indent-line-function)
  (setq-local completion-at-point-functions '(execline-completion-at-point)))

; Name of variable can contain anything, except $, { and }.
;
; Author of execline recommends to use only alphanumeric characters, but I also
; support both dash ("-") and dot (".") being part of variable name.
(modify-syntax-entry ?\. "_" execline-mode-syntax-table)
(modify-syntax-entry ?\- "_" execline-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.exec\\'" . execline-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("execlineb" . execline-mode))

(provide 'execline)
;;; execline.el ends here
