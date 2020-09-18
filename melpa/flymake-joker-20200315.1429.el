;;; flymake-joker.el --- Add Clojure syntax checker (via Joker) to flymake -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Mateusz Probachta <mateusz.probachta@gmail.com>
;;
;; Author: Mateusz Probachta <mateusz.probachta@gmail.com>
;; Created: 12 February 2019
;; Version: 0.0.2
;; Package-Version: 20200315.1429
;; Package-Commit: fc132beedac9e6f415b72e578e77318fd13af9ee
;; Package-Requires: ((emacs "26.1") (flymake-quickdef "0.1.1"))
;; URL: https://github.com/beetleman/flymake-joker

;;; Commentary:

;; This package adds Clojure syntax checker (via Joker) to flymake.
;; Make sure Joker binary is on your path.
;; Joker installation instructions are here: https://github.com/candid82/joker#installation
;; Its based on https://github.com/candid82/flycheck-joker but use flymake instead flycheck

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'flymake-quickdef)

(eval-when-compile
  (defvar flymake-joker-search-regexp))
(setf flymake-joker-search-regexp
      "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]\\ ]+\\): \\(.+\\)$")

(defun flymake-joker--severity-to-type (severity)
  "Convert error type reported by joker to keyboard accepted by flymake.
SEVERITY - error type reported by joker"
  (cond
   ((string= severity "Read error") :error)

   ((string= severity "Parse warning") :warning)
   ((string= severity "Parse error") :error)

   ((string= severity "Exception") :error)

   (t :note)))

(defmacro flymake-joker--defn-checker (name dialect)
  "Define flymake checker for joker using provided dialect.
NAME - name of function
DIALECT - dialect accepted by `--dialect' option"
  `(flymake-quickdef-backend ,name
     :pre-let ((joker-exec (executable-find "joker")))
     :pre-check (unless joker-exec (error "Cannot find joker executable"))
     :write-type 'pipe
     :proc-form (list joker-exec "--lint" "--dialect" ,dialect "-")
     :search-regexp flymake-joker-search-regexp
     :prep-diagnostic
     (let* ((lnum (string-to-number (match-string 1)))
            (lcol (string-to-number (match-string 2)))
            (severity (match-string 3))
            (msg (match-string 4))
            (pos (flymake-diag-region fmqd-source lnum lcol))
            (beg (car pos))
            (end (cdr pos))
            (type (flymake-joker--severity-to-type severity)))
       (list fmqd-source beg end type msg))))

(flymake-joker--defn-checker flymake-joker-clj-checker "clj")
(flymake-joker--defn-checker flymake-joker-edn-checker "edn")
(flymake-joker--defn-checker flymake-joker-cljs-checker "cljs")

;;;###autoload
(defun flymake-joker-clj-enable ()
  "Enable joker checker in clj or edn, ignore `clojurescript-mode'."
  (let ((ext (file-name-extension (or (buffer-file-name)
                                      (buffer-name)))))
    (unless (string= "cljs" ext)
      (if (string= "edn" ext)
          (add-hook 'flymake-diagnostic-functions #'flymake-joker-edn-checker nil t)
        (add-hook 'flymake-diagnostic-functions #'flymake-joker-clj-checker nil t)))))

;;;###autoload
(defun flymake-joker-cljs-enable ()
  "Enable joker checker in cljs mode."
  (add-hook 'flymake-diagnostic-functions #'flymake-joker-cljs-checker nil t))

(provide 'flymake-joker)

;;; flymake-joker.el ends here
