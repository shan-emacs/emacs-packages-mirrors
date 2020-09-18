;;; retrie.el --- Refactoring Haskell code with retrie

;; Copyright (C) 2020 Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 2020/05/16
;; Version: 0.0.1
;; Package-Version: 20200519.551
;; Package-Commit: 976d6f01a3e214917f16b82e750d825cb9bfcc59
;; Keywords: files, languages, tools
;; URL: https://github.com/Ailrun/emacs-retrie
;; Package-Requires: ((emacs "24.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is an Emacs wrapper of retrie (URL `https://github.com/facebookincubator/retrie').
;;
;; Currently this provides the following 21 functions
;;
;; - retrie-fold-in-current-buffer
;; - retrie-fold-in-buffer
;; - retrie-fold-in-file
;; - retrie-unfold-in-current-buffer
;; - retrie-unfold-in-buffer
;; - retrie-unfold-in-file
;; - retrie-rule-forward-in-current-buffer
;; - retrie-rule-forward-in-buffer
;; - retrie-rule-forward-in-file
;; - retrie-rule-backward-in-current-buffer
;; - retrie-rule-backward-in-buffer
;; - retrie-rule-backward-in-file
;; - retrie-type-forward-in-current-buffer
;; - retrie-type-forward-in-buffer
;; - retrie-type-forward-in-file
;; - retrie-type-backward-in-current-buffer
;; - retrie-type-backward-in-buffer
;; - retrie-type-backward-in-file
;; - retrie-adhoc-in-current-buffer
;; - retrie-adhoc-in-buffer
;; - retrie-adhoc-in-file

;;; Change Log:

;;; Code:
(require 'subr-x)

;; Customize variables

(defgroup retrie
  nil
  "Settings for Haskell tool retrie"
  :tag "Retrie"
  :group 'convenience
  :group 'haskell
  :group 'tools
  :package-version '(retrie . "0.0.1")
  :prefix "retrie-")

(defcustom retrie-executable "retrie"
  "The name of the retrie executable to find."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type 'string
  :risky t)

(defcustom retrie-use-single-thread nil
  "A boolean to disable parallel executions of retrie (usually for debugging).
Disabling works only when `retrie-log-level' is not loud."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type 'boolean)

(defcustom retrie-log-level 0
  "The level of verbosity used to print retrie log.
Setting loud makes parallel executions of retrie disabled"
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type '(choice
          (const :tag "silent" 0)
          (const :tag "normal" 1)
          (const :tag "loud" 0)))

(defcustom retrie-other-arguments nil
  "Other arguments that are not specified as a custom variable in this plugin.
They will be passed to retrie along with the options created from the retrie- custom variables."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type '(list string)
  :risky t)

;; Constants

(defconst retrie--fold-option "--fold")
(defconst retrie--unfold-option "--unfold")
(defconst retrie--rule-forward-option "--rule-forward")
(defconst retrie--rule-backward-option "--rule-backward")
(defconst retrie--type-forward-option "--type-forward")
(defconst retrie--type-backward-option "--type-backward")
(defconst retrie--adhoc-option "--adhoc")

;; Interactive commands

(defun retrie-fold-in-current-buffer (name)
  "Fold the function definition of the fully-qualified name NAME in the current buffer."
  (interactive "sname of function to fold in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--fold-option name))

(defun retrie-fold-in-buffer (buffername name)
  "Fold the function definition of the fully-qualified name NAME in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of function to fold in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--fold-option name))

(defun retrie-fold-in-file (filepath name)
  "Fold the function definition of the fully-qualified name NAME in the file FILEPATH."
  (interactive "ffile path:\nsname of function to fold in %s:")
  (retrie--call-retrie-for-file filepath retrie--fold-option name))

(defun retrie-unfold-in-current-buffer (name)
  "Unfold the function definition of the fully-qualified name NAME in the current buffer."
  (interactive "sname of function to unfold in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--unfold-option name))

(defun retrie-unfold-in-buffer (buffername name)
  "Unfold the function definition of the fully-qualified name NAME in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of function to unfold in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--unfold-option name))

(defun retrie-unfold-in-file (filepath name)
  "Unfold the function definition of the fully-qualified name NAME in the file FILEPATH."
  (interactive "ffile path:\nsname of function to unfold in %s:")
  (retrie--call-retrie-for-file filepath retrie--unfold-option name))

(defun retrie-rule-forward-in-current-buffer (name)
  "Apply the rule of the fully-qualified name NAME in left-to-right manner in the current buffer."
  (interactive "sname of rule to apply (left-to-right) in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--rule-forward-option name))

(defun retrie-rule-forward-in-buffer (buffername name)
  "Apply the rule of the fully-qualified name NAME in left-to-right manner in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of rule to apply (left-to-right) in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--rule-forward-option name))

(defun retrie-rule-forward-in-file (filepath name)
  "Apply the rule of the fully-qualified name NAME in left-to-right manner in the file FILEPATH."
  (interactive "ffile path:\nsname of rule to apply (left-to-right) in %s:")
  (retrie--call-retrie-for-file filepath retrie--rule-forward-option name))

(defun retrie-rule-backward-in-current-buffer (name)
  "Apply the rule of the fully-qualified name NAME in right-to-left manner in the current buffer."
  (interactive "sname of rule to apply (right-to-left) in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--rule-backward-option name))

(defun retrie-rule-backward-in-buffer (buffername name)
  "Apply the rule of the fully-qualified name NAME in right-to-left manner in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of rule to apply (right-to-left) in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--rule-backward-option name))

(defun retrie-rule-backward-in-file (filepath name)
  "Apply the rule of the fully-qualified name NAME in right-to-left manner in the file FILEPATH."
  (interactive "ffile path:\nsname of rule to apply (right-to-left) in %s:")
  (retrie--call-retrie-for-file filepath retrie--rule-backward-option name))

(defun retrie-type-forward-in-current-buffer (name)
  "Apply the type of the fully-qualified name NAME in left-to-right manner in the current buffer."
  (interactive "sname of type to apply (left-to-right) in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--type-forward-option name))

(defun retrie-type-forward-in-buffer (buffername name)
  "Apply the type of the fully-qualified name NAME in left-to-right manner in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of type to apply (left-to-right) in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--type-forward-option name))

(defun retrie-type-forward-in-file (filepath name)
  "Apply the type of the fully-qualified name NAME in left-to-right manner in the file FILEPATH."
  (interactive "ffile path:\nsname of type to apply (left-to-right) in %s:")
  (retrie--call-retrie-for-file filepath retrie--type-forward-option name))

(defun retrie-type-backward-in-current-buffer (name)
  "Apply the type of the fully-qualified name NAME in right-to-left manner in the current buffer."
  (interactive "sname of type to apply (right-to-left) in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--type-backward-option name))

(defun retrie-type-backward-in-buffer (buffername name)
  "Apply the type of the fully-qualified name NAME in right-to-left manner in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of type to apply (right-to-left) in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--type-backward-option name))

(defun retrie-type-backward-in-file (filepath name)
  "Apply the type of the fully-qualified name NAME in right-to-left manner in the file FILEPATH."
  (interactive "ffile path:\nsname of type to apply (right-to-left) in %s:")
  (retrie--call-retrie-for-file filepath retrie--type-backward-option name))

(defun retrie-adhoc-in-current-buffer (adhoc)
  "Apply the rule ADHOC in the current buffer."
  (interactive "srewrite rule to apply in the current buffer:")
  (retrie--call-retrie-for-buffer (current-buffer) retrie--adhoc-option adhoc))

(defun retrie-adhoc-in-buffer (buffername adhoc)
  "Apply the rule ADHOC in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsrewrite rule to apply in %s:")
  (retrie--call-retrie-for-buffer (get-buffer buffername) retrie--adhoc-option adhoc))

(defun retrie-adhoc-in-file (filepath adhoc)
  "Apply the rule ADHOC in the file FILEPATH."
  (interactive "ffile path:\nsrewrite rule to apply in %s:")
  (retrie--call-retrie-for-file filepath retrie--adhoc-option adhoc))

;; Internal functions

(defun retrie--call-retrie-for-buffer (buffer &rest args)
  "Internal function to call retrie in BUFFER with ARGS."
  (let ((filepath (buffer-file-name buffer)))
    (unless filepath
      (error "%s is not visiting any file.  Abort" (buffer-name buffer)))
    (apply #'retrie--call-retrie-for-file filepath args)))

(defun retrie--call-retrie-for-file (filepath &rest args)
  "Internal function to call retrie in FILEPATH with ARGS."
  (apply #'retrie--call-retrie "--target-file" filepath args))

(defun retrie--call-retrie (&rest args)
  "Internal function to call retrie with ARGS."
  (let ((logbuf (generate-new-buffer "*retrie*"))
        (args
         (append
          args
          (if retrie-use-single-thread '("--single-threaded") nil)
          (list "--verbosity" (number-to-string retrie-log-level))
          retrie-other-arguments))
        (retrie (executable-find retrie-executable)))
    (unless retrie
      (error "Cannot find %s in exec-path.  Please check your exec-path" retrie-executable))
    (let ((process (apply #'start-process retrie logbuf retrie-executable args)))
      (set-process-sentinel process 'retrie--sentinel))))

(defun retrie--sentinel (process event)
  "Internal function taking PROCESS and EVENT."
  (if (and (= 0 (process-exit-status process))
           (process-live-p process))
      (retrie--print-log "retrie is %s" (string-trim-right event))
    (with-current-buffer (process-buffer process)
      (retrie--print-log "retrie is %s\n%s" (string-trim-right event) (string-trim-right (buffer-string))))
    (kill-buffer (process-buffer process))))

(defun retrie--print-log (string &rest args)
  "Internal function taking STRING and ARGS."
  (apply #'message string args))

(provide 'retrie)
;;; retrie.el ends here
