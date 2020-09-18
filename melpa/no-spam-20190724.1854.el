;;; no-spam.el --- Add repeat delays to commands -*- lexical-binding: t -*-

;; Copyright (C) 2019 Daniel Phan

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20190724.1854
;; Package-Commit: 860860e4a0d59bd15c8e092dc42f5f7f769a428e
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/mamapanda/no-spam
;; Keywords: keyboard, tools

;; This file is NOT part of GNU Emacs.

;;; License:
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

;;; Commentary:
;;
;; `no-spam-mode' allows adding repeat delays to commands.
;; To add a repeat delay, use the macro `no-spam-add-repeat-delay',
;; then turn on `no-spam-mode' to enable the delays.

;;; Code:
(eval-when-compile
  (require 'subr-x))

(defgroup no-spam nil
  "Add repeat delays to commands."
  :group 'keyboard)

(defcustom no-spam-lighter " NS"
  "The mode-line lighter for no-spam."
  :type 'string)

(defcustom no-spam-default-repeat-delay 1
  "The default repeat delay in seconds."
  :type 'float)

(defcustom no-spam-default-exception #'ignore
  "The default exception function."
  :type 'function)

(defvar no-spam--advice-alist nil
  "An alist of commands to their advices.")

;;;###autoload
(define-minor-mode no-spam-mode
  "A minor mode to add repeat delays to commands."
  :global t
  :lighter no-spam-lighter
  (if no-spam-mode
      (dolist (advice-desc no-spam--advice-alist)
        (advice-add (car advice-desc) :around (cdr advice-desc)))
    (dolist (advice-desc no-spam--advice-alist)
      (advice-remove (car advice-desc) (cdr advice-desc)))))

(defun no-spam--register-advice (command advice)
  "Register ADVICE for COMMAND with no-spam."
  (when no-spam-mode
    (when-let ((old-advice (alist-get command no-spam--advice-alist)))
      (advice-remove command old-advice))
    (advice-add command :around advice))
  (setq no-spam--advice-alist (assq-delete-all command no-spam--advice-alist))
  (add-to-list 'no-spam--advice-alist (cons command advice)))

(defvar no-spam--last-time nil
  "The last time checked by no-spam.")

(defmacro no-spam-add-repeat-delay (command &optional delay except)
  "Set a repeat delay for COMMAND of DELAY seconds.

COMMAND may be a single command or a list of commands.  In the latter
case, the commands are all treated as one command and share the same
repeat delay.

DELAY defaults to `no-spam-default-repeat-delay'.

EXCEPT is a zero-argument function that returns non-nil if COMMAND's
repeat delay should be bypassed.  EXCEPT defaults to
`no-spam-default-exception-fn'."
  (let ((commands (if (listp command) command (list command)))
        (delay (or delay no-spam-default-repeat-delay))
        (except (or except no-spam-default-exception))
        (advice-fn (gensym "no-spam--advice"))
        (orig-fn-var (gensym "orig-fn"))
        (args-var (gensym "args"))
        (time-var (gensym "time"))
        (cmd-var (gensym "cmd")))
    `(progn
       (defun ,advice-fn (,orig-fn-var &rest ,args-var)
         ,(format "Advice for repeat delay of %s." commands)
         (cond
          ((not (called-interactively-p))
           (apply ,orig-fn-var ,args-var))
          ((,except)
           (apply ,orig-fn-var ,args-var)
           ;; Reset the delays, or a wrong last time will be used.
           (setq no-spam--last-time nil))
          (t
           (let ((,time-var (float-time)))
             (unless (and no-spam--last-time
                          (< (- ,time-var no-spam--last-time) ,delay)
                          (memq real-last-command ',commands))
               (apply ,orig-fn-var ,args-var)
               (setq no-spam--last-time ,time-var))))))
       (dolist (,cmd-var ',commands)
         (no-spam--register-advice ,cmd-var #',advice-fn)))))

(provide 'no-spam)
;;; no-spam.el ends here
