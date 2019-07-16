;;; auto-correct.el --- Remembers and automatically fixes past corrections -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: editing
;; Version: 1.0

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable, use:

;; M-x `auto-correct-mode'

;; After that, any future corrections made with flyspell or Ispell (or any other
;; supported package) will be automatically corrected for you as you type.

;; For example, if you type "befroe" and fixed it with `ispell-word',
;; `auto-correct-mode' will change "befroe" to "before" every time you type it
;; from then on.

;; Corrections are only made when `auto-correct-mode' is enabled.  Expansion is
;; case-insensitive, so trying to fix alice as Alice won't work.

;; For more fine-grained control over when corrections are made, set the
;; buffer-local variable `auto-correct-predicate'.

;; Behind the scenes, auto-correct uses an abbrev table, so in order to clean
;; out or modify any fixes auto-correct has learned, use `list-abbrevs'.  This
;; also means that fixes are saved between Emacs sessions along with the abbrev
;; tables.

;; Ispell and flyspell are the only two packages that auto-correct supports out
;; of the box, but it's possible to add support for any package that corrects
;; text:

;; 1. Create a function that calls `auto-correct--add-or-update-correction' with
;; the old text and the corrected text from your package.

;; 2. Write two functions: an activation function and a deactivation function.
;; These will be called by `auto-correct-mode' to activate and deactivate
;; support.

;; 3. Call `auto-correct--add-support' with your activation and deactivation
;; functions.

;; 4. You're done.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'thingatpt)

(defgroup auto-correct nil
  "Auto correction support."
  :prefix "auto-correct-"
  :group 'editing)

;; Core Functionality

(defvar-local auto-correct-predicate nil
  "Predicate to check whether automatic corrections should be made.

This should be a function of no arguments that returns non-nil if
auto-correct should operate on the current text.")

(defun auto-correct-expand-p ()
  "Return non-nil if auto-correct should operate on the current point.

To customize this behavior, set `auto-correct-predicate'."
  (when auto-correct-predicate (funcall auto-correct-predicate)))

(define-abbrev-table 'auto-correct-abbrev-table nil
  "Abbrev table where automatic corrections are stored."
  :enable-function #'auto-correct-expand-p)

(defun auto-correct--get-abbrev-table (local)
  "Get the abbrev table to use with auto-correct.

If LOCAL is non-nil, use the local table if it exists.
Otherwise, use auto-correct's abbrev table."
  (if local
      (or local-abbrev-table auto-correct-abbrev-table)
    auto-correct-abbrev-table))

(defun auto-correct--add-or-update-correction (before after &optional local)
  "Add or update a correction into auto-correct's table.

BEFORE is the misspelled word, and AFTER is the correct spelling.

Optional argument LOCAL determines whether to make the correction
locally.  If nil, the correction will be made whenever
`auto-correct-mode' is enabled."
  (let ((table (auto-correct--get-abbrev-table local))
        (bef (downcase before))
        (aft (downcase after)))
    (define-abbrev table bef aft nil :count 1)
    ;; Save the abbrevs.
    (write-abbrev-file)
    (message "\"%s\" now expands to \"%s\"" bef aft)))

;; Extension Support

(defvar auto-correct-activate-functions nil
  "Functions to run to activate auto-correct support in various packages.

These are called by `auto-correct-mode' when it is enabled.")

(defvar auto-correct-deactivate-functions nil
  "Functions to run to deactivate auto-correct support in various packages.

These are called by `auto-correct-mode' when it is disabled.")

;; The mode

;;;###autoload
(define-minor-mode auto-correct-mode
  "Activate automatic corrections.

Auto correct expansions will only work when this mode is enabled,
but auto-correct can be trained with `auto-correct-fix-and-add'
even if this mode is disabled.

When this mode is enabled, corrections made with flyspell and
Ispell will be made automatically after fixing them once.

In order to add corrections to the auto-correct abbrev table in
flyspell (and thus have them corrected later), set
`flyspell-use-global-abbrev-table-p' to non-nil.

In order to set corrections as local using Ispell, use
the command `auto-correct-toggle-ispell-local'.

\\{auto-correct-mode-map}"
  :group 'auto-correct
  :global t
  :init-value nil
  :lighter " Auto-Correct"
  (if auto-correct-mode
      (run-hooks 'auto-correct-activate-functions)
    (run-hooks 'auto-correct-deactivate-functions)))

;; Only enable the abbrev list when auto-correct-mode is active.
(add-to-list 'abbrev-minor-mode-table-alist
             `(auto-correct-mode ,auto-correct-abbrev-table)
             'append
             #'equal)

(defun auto-correct--add-support (activate-fun deactivate-fun)
  "Add auto-correct support for a spelling package.

Support will be activated by ACTIVATE-FUN and deactivated by DEACTIVATE-FUN."
  (add-hook 'auto-correct-activate-functions activate-fun)
  (add-hook 'auto-correct-deactivate-functions deactivate-fun)
  ;; If `auto-correct-mode' is enabled, activate this package's support.
  (when auto-correct-mode
    (funcall activate-fun)))

(defun auto-correct--remove-support (activate-fun deactivate-fun)
  "Remove support for a spelling package.

Support will be activated by ACTIVATE-FUN and deactivated by DEACTIVATE-FUN."
  (remove-hook 'auto-correct-activate-functions activate-fun)
  (remove-hook 'auto-correct-deactivate-functions deactivate-fun)
  ;; If `auto-correct-mode' is enabled, deactivate this package's support.
  (when auto-correct-mode
    (funcall deactivate-fun)))

(defun auto-correct--handle-support (add activate-fun deactivate-fun)
  "Helper function to add or remove auto-correct support for a package.

If ADD is non-nil, add support, otherwise remove it.
ACTIVATE-FUN and DEACTIVATE-FUN are exactly as they are in
`auto-correct--add-support'."
  (if add
      (auto-correct--add-support activate-fun deactivate-fun)
    (auto-correct--remove-support activate-fun deactivate-fun)))

;; Flyspell Support

(defvar flyspell-auto-correct-word)
(defvar flyspell-use-global-abbrev-table-p)
(defvar flyspell-insert-function)

(defun auto-correct-flyspell-insert (word)
  "Insert WORD and add it as a correction.

The original (misspelled) word is drawn from the variable
`flyspell-auto-correct-word'.

When `auto-correct-mode' is enabled, this function is set as
`flyspell-insert-function'."
  (let ((old-word flyspell-auto-correct-word)
        (new-word word)
        (local (not flyspell-use-global-abbrev-table-p)))
    (auto-correct--add-or-update-correction old-word new-word local)
    (insert word)))

(defun auto-correct--flyspell-activate ()
  "Activate flyspell auto-correct support.

Sets `flyspell-insert-function' to `auto-correct-flyspell-insert'."
  ;; Add flyspell corrections as auto-corrections
  (setq flyspell-insert-function 'auto-correct-flyspell-insert))

(defun auto-correct--flyspell-deactivate ()
  "Deactivate flyspell auto-correct support."
  (setq flyspell-insert-function 'insert))

(defcustom auto-correct-enable-flyspell-support t
  "Whether to automatically correct corrections made in flyspell."
  :group 'auto-correct
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (auto-correct--handle-support
          val
          'auto-correct--flyspell-activate
          'auto-correct--flyspell-deactivate)))

;; Ispell support

(defvar ispell-following-word)

(defvar auto-correct--ispell-use-local-table nil
  "Whether to use the local table with Ispell.

Toggle this interactively with `auto-correct-toggle-ispell-local'.")

(defun auto-correct-toggle-ispell-local ()
  "Toggle whether to use the local or auto-correct table for Ispell."
  (interactive)
  (setq auto-correct--ispell-use-local-table
        (not auto-correct--ispell-use-local-table))
  (message "Auto-Correct is now using the %s table"
           (if auto-correct--ispell-use-local-table "local" "global")))

(defun auto-correct--ispell-handler (ispell-result)
  "Add ISPELL-RESULT as a correction.

The original (misspelled) word is drawn from the function
`word-at-point'.

This is intended to be added as advice to `ispell-command-loop'."
  (when-let ((word-before (word-at-point))
             (correction ispell-result))
    (when (and correction (consp correction))
      ;; The correction was entered by hand.
      (setq correction (car correction)))
    (if (and (not (or (eq correction 0)  ;; Word was corrected from list
                    (eq correction 'quit))) ;; Session was exited
             (not (equal word-before correction))) ;; Word was corrected
        (auto-correct--add-or-update-correction word-before correction
                                          auto-correct--ispell-use-local-table)))
  ispell-result)

(defun auto-correct--ispell-activate ()
  "Activate Ispell auto-correct support.

Adds advice to `ispell-command-loop' that adds the result as a
correction."
  ;; Add corrections from ispell as auto-corrections
  (advice-add 'ispell-command-loop :filter-return
              #'auto-correct--ispell-handler))

(defun auto-correct--ispell-deactivate ()
  "Deactivate Ispell auto-correct support."
  (advice-remove 'ispell-command-loop #'auto-correct--ispell-handler))

(defcustom auto-correct-enable-ispell-support t
  "Whether to automatically correct corrections made in Ispell."
  :group 'auto-correct
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (auto-correct--handle-support
          val
          'auto-correct--ispell-activate
          'auto-correct--ispell-deactivate)))

;; Standalone (piggybacks on Ispell)

;;;###autoload
(defun auto-correct-fix-and-add (local)
  "Use `ispell-word' to fix a misspelled word at point.

Once the misspelled word is fixed, auto-correct will remember the
fix and auto-correct it from then on, so long as
`auto-correct-mode' is enabled.

With a non-nil argument LOCAL (interactively, the prefix argument),
create a fix for the typo that will be auto-corrected for buffers
using the current local mode.

This is pointless to use when `auto-correct-mode' is enabled;
instead, use `ispell-word' and `auto-correct-toggle-ispell-local'
to use the local abbrev table."
  (interactive "P")
  (let ((auto-correct--ispell-use-local-table local))
    (auto-correct--ispell-handler (ispell-word ispell-following-word 'quietly))))

;;;###autoload
(defun auto-correct-scan-buffer ()
  "Scan current buffer for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Stop from being prompted to save the personal dictionary after every
    ;; change.
    (cl-letf (((symbol-function 'ispell-pdict-save) #'ignore))
      (while (forward-word)
        (auto-correct-fix-and-add nil)))
    (ispell-pdict-save)))

;;;###autoload
(defun auto-correct-scan-region (start end)
  "Scan the region between START and END for misspelled words.

Interactively, START and END are the current region.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (auto-correct-scan-buffer)))

;;;###autoload
(defun auto-correct-scan ()
  "Scan the buffer or region for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive)
  (if (region-active-p)
      (auto-correct-scan-region (region-beginning) (region-end))
    (auto-correct-scan-buffer)))

;;;; ChangeLog:

;; 2017-09-04  Ian Dunn  <dunni@gnu.org>
;; 
;; 	Added auto-correct package
;; 
;; 	Single-file package to automatically make corrections found by packages
;; 	such as Ispell and flyspell.
;; 
;; 	* packages/auto-correct/auto-correct.el: Added.
;; 


(provide 'auto-correct)

;;; auto-correct.el ends here
