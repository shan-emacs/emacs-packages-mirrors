;;; persist.el --- Persist Variables -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.1

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

;; This package provides variables which persist across sessions.

;; The main entry point is `persist-defvar' which behaves like
;; `defvar' but which persists the variables between session.  Variables
;; are automatically saved when Emacs exits.

;; Other useful functions are `persist-save' which saves the variable
;; immediately, `persist-load' which loads the saved value,
;; `persist-reset' which resets to the default value.

;; Values are stored in a directory in `user-emacs-directory', using
;; one file per value.  This makes it easy to delete or remove unused
;; variables.

;;; Code:
(defvar persist--directory-location
  (concat user-emacs-directory "persist/")
  "The location of persist directory.")

(defvar persist--symbols nil
  "List of symbols to persist.")

(defvar persist-load-hook nil
  "Special hook run on loading a variable.

Hook functions are called with two values: the symbol and the
value it will be set to.  If any function returns nil, the
variable is not set to the value.")

(defun persist--file-location (symbol)
  "Return the file name at which SYMBOL does or will persist."
  (concat persist--directory-location (symbol-name symbol)))

(defmacro persist-defvar (symbol initvalue docstring)
  "Define SYMBOL as a persistant variable and return SYMBOL.

This form is nearly equivalent to `defvar', except that the
variable persists between Emacs sessions.

It does not support the optional parameters.  Both INITVALUE and
DOCSTRING need to be given."
  ;; We cannot distinguish between calls with initvalue of nil and a
  ;; single parameter call. Unfortunately, these two calls have
  ;; different semantics -- the single arity shuts up the byte
  ;; compiler, but does not define the symbol. So, don't support a
  ;; single arity persist-defvar.

  ;; Don't support 2-arity calls either because we are lazy and
  ;; because if you want to persist it, you want to doc it.
  (declare (debug defvar) (doc-string 3))
  (let ((form
         `(defvar ,symbol ,initvalue ,docstring)))
    (persist-symbol symbol initvalue)
    (persist-load symbol)
    form))

(defun persist-symbol (symbol &optional initvalue)
  "Make SYMBOL a persistant variable.

If non-nil, INITVALUE is the value to which SYMBOL will be set if
`persist-reset' is called.  Otherwise, the INITVALUE will be the
current `symbol-value' of SYMBOL.

INITVALUE is set for the session and will itself not persist
across sessions."
  (let ((initvalue (or initvalue (symbol-value symbol))))
    (add-to-list 'persist--symbols symbol)
    (put symbol 'persist t)
    (put symbol 'persist-default initvalue)))

(defun persist--persistant-p (symbol)
  "Return non-nil if SYMBOL is a persistant variable."
  (get symbol 'persist))

(defun persist-save (symbol)
  "Save SYMBOL now.

Normally, it should not be necessary to call this explicitly, as
variables persist automatically when Emacs exits."
  (unless (persist--persistant-p symbol)
    (error (format
            "Symbol %s is not persistant" symbol)))
  (unless (file-exists-p persist--directory-location)
    (mkdir persist--directory-location))
  (with-temp-buffer
    (print (symbol-value symbol) (current-buffer))
    (write-region (point-min) (point-max)
                  (persist--file-location symbol)
                  nil 'quiet)))

(defun persist-default (symbol)
  "Return the default value for SYMBOL."
  (get symbol 'persist-default))

(defun persist-reset (symbol)
  "Reset the value of SYMBOL to the default."
  (set symbol (persist-default symbol)))

(defun persist-load (symbol)
  "Load the saved value of SYMBOL."
  (when (file-exists-p (persist--file-location symbol))
    (with-temp-buffer
      (insert-file-contents (persist--file-location symbol))
      (let ((val (read (current-buffer))))
        (when (run-hook-with-args-until-failure 'persist-load-hook
                                                symbol val)
          (set symbol val))))))

(defun persist-unpersist (symbol)
  "Stop the value in SYMBOL from persisting.

This does not remove any saved value of SYMBOL."
  (put symbol 'persist nil)
  (setq persist--symbols
        (remove symbol persist--symbols)))

(defun persist--save-all ()
  "Save all persistant symbols."
  (mapc 'persist-save persist--symbols))

;; Save on kill-emacs-hook anyway
(add-hook 'kill-emacs-hook
          'persist--save-all)

;;;; ChangeLog:



(provide 'persist)
;;; persist.el ends here
