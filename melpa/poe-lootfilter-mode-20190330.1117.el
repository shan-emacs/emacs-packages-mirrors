;;; poe-lootfilter-mode.el --- Major mode for editing Path of Exile lootfilters -*- lexical-binding: t; -*-

;; Copyright 2019 Jeremiah Dodds <jeremiah.dodds@gmail.com>

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20190330.1117
;; Package-Commit: 5ef06684cb2b17b090ee1f303c2b789fa71bc106
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jdodds/poe-lootfilter-mode
;; Keywords: languages, games

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides basic support for editing Path of Exile lootfilters.

;; Setting `poe-lootfilter-section-regexp' to a regexp of one group that matches
;; your section marker syntax will enable imenu for sections in your filters.

;;; Code:

(defgroup poe-lootfilter nil
  "Support for Path of Exile lootfilters <http://www.pathofexile.com>"
  :group 'languages
  :prefix "poe-lootfilter-")

(defcustom poe-lootfilter-section-regexp nil
  "Regexp matching the comment marker used to describe sections.
If non-nil, must be a regexp of one group that matches the section
name.  Enables imenu for sections when set."
  :type '(choice (const :tag "None" nil)
                 (regexp :tag "Regexp"))
  :group 'poe-lootfilter)

(defface poe-lootfilter-condition-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for conditions (ItemLevel, DropLevel, etc)"
  :group 'poe-lootfilter)

(defface poe-lootfilter-command-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for commands (SetFontSize, PlayEffect, etc)"
  :group 'poe-lootfilter)

(defconst poe-lootfilter-condition-regexp
  (concat
   "^\\s-*"
   (regexp-opt
    '("ItemLevel"
      "DropLevel"
      "Quality"
      "Rarity"
      "Class"
      "BaseType"
      "Sockets"
      "LinkedSockets"
      "SocketGroup"
      "Height"
      "Width"
      "HasExplicitMod"
      "AnyEnchantment"
      "HasEnchantment"
      "StackSize"
      "GemLevel"
      "Identified"
      "Corrupted"
      "ElderItem"
      "ShaperItem"
      "FracturedItem"
      "SynthesisedItem"
      "ShapedMap"
      "MapTier"))))

(defconst poe-lootfilter-command-regexp
  (concat
   "^\\s-*"
   (regexp-opt
    '("SetBorderColor"
      "SetTextColor"
      "SetBackgroundColor"
      "SetFontSize"
      "PlayAlertSound"
      "PlayAlertSoundPositional"
      "DisableDropSound"
      "CustomAlertSound"
      "MinimapIcon"
      "Size"
      "Color"
      "Shape"
      "Displays"
      "PlayEffect"))))

(defconst poe-lootfilter-keyword-regexp
  (regexp-opt '("Show" "Hide")))

(defconst poe-lootfilter-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst poe-lootfilter-mode-fontlock-keywords
  `(,poe-lootfilter-keyword-regexp
    (,poe-lootfilter-condition-regexp (0 'poe-lootfilter-condition-face))
    (,poe-lootfilter-command-regexp (0 'poe-lootfilter-command-face))))

(defun poe-lootfilter-indent-line ()
  "Indent the current line if we're inside a block."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (or (looking-at-p poe-lootfilter-condition-regexp)
              (looking-at-p poe-lootfilter-command-regexp))
      (delete-horizontal-space)
      (insert-tab))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.filter$" . poe-lootfilter-mode))

;;;###autoload
(define-derived-mode poe-lootfilter-mode prog-mode "lootfilter"
  "Major mode for editing Path of Exile loot filters."
  :group 'poe-lootfiter
  :syntax-table poe-lootfilter-mode-syntax-table
  (setq font-lock-defaults '(poe-lootfilter-mode-fontlock-keywords))
  (setq-local indent-line-function #'poe-lootfilter-indent-line)
  (when poe-lootfilter-section-regexp
    (setq imenu-generic-expression
          `((nil ,poe-lootfilter-section-regexp 1)))))

(provide 'poe-lootfilter-mode)

;;; poe-lootfilter-mode.el ends here
