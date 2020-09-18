;;; evil-ex-fasd.el --- using fasd right from evil-ex  -*- lexical-binding: t; -*-

;; Copyright © 2018, Rashawn Zhang, all rights reserved.

;; Version: 0.1.0
;; Package-Version: 20180903.612
;; Package-Commit: ed8fbbe23a8a268d9dcbf1a6132e928ba2c655c5
;; URL: https://github.com/yqrashawn/evil-ex-fasd
;; Package-Requires: ((emacs "24.4") (evil "1.1.0") (fasd "0"))
;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created:  2 August 2018
;; Keywords: tools, fasd, evil, navigation

;; This program is free software; you can redistribute it and/or modify
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

;;  Invoke fasd functionality right from `evil-ex'

;;; Code:

(require 'evil)
(require 'fasd)

(defvar evil-ex-fasd-prefix ":"
  "Prefix for ‘evil-ex’ command that will invoke fasd.")

(defun evil-ex-fasd-eval (orig-fun str)
  "Advice for ‘evil-ex-execute’. ORIG-FUN is `evil-ex-execute', STR is the command input."
  (if (not (cond
            ((string-prefix-p evil-ex-fasd-prefix str)
             (funcall #'fasd-find-file t (string-remove-prefix evil-ex-fasd-prefix str)))))
      (funcall orig-fun str)))

(advice-add #'evil-ex-execute :around 'evil-ex-fasd-eval)

;;;###autoload
(defun evil-ex-fasd ()
  "Invoke `evil-ex' with `evil-ex-fasd-prefix'."
  (interactive)
  (evil-ex evil-ex-fasd-prefix))

(provide 'evil-ex-fasd)
;;; evil-ex-fasd.el ends here
