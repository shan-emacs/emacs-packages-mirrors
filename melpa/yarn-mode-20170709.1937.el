;;; yarn-mode.el --- Major mode for yarn.lock files.

;; Copyright (c) 2017 Nicolás Salas V.
;;
;; Author: Nicolás Salas V. <nikosalas@gmail.com>
;; URL: https://github.com/anachronic/yarn-mode
;; Package-Version: 20170709.1937
;; Keywords: convenience
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

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

;; yarn-mode is a major mode designed to be used to look at yarn.lock
;; files generated by Facebook's yarn package manager

;; To install, just (require 'yarn-mode)

;; yarn.lock files will be automatically be opened with yarn-mode and
;; will be in read-only mode

;; Visit the home page at https://github.com/anachronic/yarn-mode

;;; Code:

(defgroup yarn-mode nil
  "Major mode for yarn.lock files"
  :group 'convenience)

(defvar yarn-mode-syntax-table
  nil
  "Syntax table for `yarn-mode'.")

(defvar yarn-mode-attributes-re
  nil
  "Keywords of `yarn-mode'.")

(defvar yarn-mode-font-lock-defaults
  nil
  "List for font lock in `yarn-mode'.")

(defvar yarn-mode-package-re
  nil
  "Regular expression that defines a package.")

(defvar yarn-mode-dependencies-re
  nil
  "Regular expression that defines a package dependency.")

(setq yarn-mode-package-re "\\(^\\|,\\s-\\)\\([a-zA-Z-_0-9]+\\)@")
(setq yarn-mode-dependencies-re "\\s-\\{4,\\}\\([a-zA-Z-_0-9]+\\)\\s-")
(setq yarn-mode-attributes-re (regexp-opt '("version" "resolved" "dependencies")))

(setq yarn-mode-font-lock-defaults
      `((,yarn-mode-attributes-re . 'yarn-mode-keywords-face)
        (,yarn-mode-package-re . (2 'yarn-mode-package-face t)) ;; Direct deps
        (,yarn-mode-dependencies-re . (1 'yarn-mode-dependency-face t)) ;; Dep of another dep (nested)
        ))

(setq yarn-mode-syntax-table
      (let ((syntable (make-syntax-table)))
        (modify-syntax-entry ?# "<" syntable)
        (modify-syntax-entry ?\n ">" syntable)
        (modify-syntax-entry ?\" "\"" syntable)
        syntable))

;; Custom faces
(defface yarn-mode-package-face
  '((t :inherit bold))
  "Font lock face for package names in yarn mode."
  :group 'yarn-mode)

(defface yarn-mode-dependency-face
  '((t :inherit bold))
  "Font lock face for package nested dependencies in yarn mode."
  :group 'yarn-mode)

(defface yarn-mode-keywords-face
  '((t :inherit font-lock-builtin-face))
  "Font lock face for yarn keywords."
  :group 'yarn-mode)

;;;###autoload
(define-derived-mode yarn-mode text-mode "Yarn"
  "Simple mode to highlight yarn.lock files."
  :syntax-table yarn-mode-syntax-table
  (setq font-lock-defaults '(yarn-mode-font-lock-defaults))
  (setq buffer-read-only t))

;;;###autoload
(add-to-list 'auto-mode-alist '("yarn\\.lock\\'" . yarn-mode))

(provide 'yarn-mode)
;;; yarn-mode.el ends here