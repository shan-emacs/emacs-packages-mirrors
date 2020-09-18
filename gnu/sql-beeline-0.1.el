;;; sql-beeline.el --- Beeline support for sql.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2020  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.1
;; Keywords: sql, hive, beeline, hiveserver2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO
;;
;; - Recognize prompt for a user when !connect is given only url
;; - Turn off echo
;;

;;; Code:

(require 'sql)


(defcustom sql-beeline-program "beeline"
  "Command to start the Beeline (HiveServer2 client)."
  :type 'file
  :group 'SQL)

(defcustom sql-beeline-options
  '("--silent" "--incremental=false" "--headerInterval=10000")
  "List of additional options for `sql-beeline-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-beeline-login-params
  `((user :default ,(user-login-name))
    (database :default "default")
    (server :default "localhost")
    (port :default 10000))
  "List of login parameters needed to connect to HiveServer2."
  :type 'sql-login-params
  :group 'SQL)


(defun sql-comint-beeline (product options &optional buf-name)
  "Create comint buffer and connect to HiveServer2."
  (let ((params (append
                 (list "-u" (format "jdbc:hive2://%s:%d/%s"
                                    sql-server sql-port sql-database))
                 (unless (string-empty-p sql-user)
                   (list "-n" sql-user))
                 (unless (string-empty-p sql-password)
                   (list "-p" sql-password))
                 options))
        ;; TERM=dumb makes jline library (version 2.12 used in Hive
        ;; 1.1.0, for example) to fallback to "unsupported" terminal,
        ;; and in that mode its ConsoleReader emulates password char
        ;; hiding by emitting prompt together with carriage returns
        ;; every few milliseconds - we don't want it because it
        ;; just makes garbage.
        (comint-terminfo-terminal ""))
    (sql-comint product params buf-name)))

;;;###autoload
(defun sql-beeline (&optional buffer)
  "Run beeline as an inferior process."
  (interactive "P")
  (sql-product-interactive 'beeline buffer))

;; FIXME: We required `sql' above, so why use eval-after-load?
(eval-after-load "sql"
  '(sql-add-product
    'beeline "Beeline"
    :font-lock 'sql-mode-ansi-font-lock-keywords
    :sqli-program 'sql-beeline-program
    :sqli-options 'sql-beeline-options
    :sqli-login 'sql-beeline-login-params
    :sqli-comint-func #'sql-comint-beeline
    :list-all '("show tables;" . "!tables")
    :list-table '("describe %s;" . "!describe %s")
    :prompt-regexp "^[^ .][^>\n]*> "
    :prompt-cont-regexp "^[ .]*> "))

;;;; ChangeLog:

;; 2020-09-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/sql-beeline/sql-beeline.el: Add missing final line
;; 
;; 2020-09-02  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/sql-beeline/sql-beeline.el: Initial commit
;; 


(provide 'sql-beeline)
;;; sql-beeline.el ends here
