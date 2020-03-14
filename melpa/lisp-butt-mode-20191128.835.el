;;; lisp-butt-mode.el --- Slim Lisp Butts -*- lexical-binding: t -*-


;; THIS FILE HAS BEEN GENERATED.  For sustainable program-development
;; edit the literate source file "lisp-butt-mode.org".  Find also
;; additional information there.


;; Copyright 2019 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: [2019-07-11]
;; Version: 1.0.3
;; Package-Version: 20191128.835
;; Package-Requires: ((emacs "25"))
;; Keywords: lisp
;; URL: https://gitlab.com/marcowahl/lisp-butt-mode
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

;; lisp-butt-mode brings fat lisp butts in shape.
;; 
;; With lisp-butt-mode e.g.
;; 
;; 	))))))))))))))))))))))))))))))))
;; 
;; gets displayed nicely as (pun intended):
;; 
;; 	).)

;; There is a global `lisp-butt-global-mode' and a local `lisp-butt-mode'
;; variant.
;; 
;; Global:
;; 
;; - Toggle the mode with {M-x lisp-butt-global-mode RET}.
;; - Activate the mode with {C-u M-x lisp-butt-global-mode RET}.
;; - Deactivate the mode with {C-u -1 M-x lisp-butt-global-mode RET}.
;; 
;; Local:
;; 
;; - Toggle the mode with {M-x lisp-butt-mode RET}.
;; - Activate the mode with {C-u M-x lisp-butt-mode RET}.
;; - Deactivate the mode with {C-u -1 M-x lisp-butt-mode RET}.
;; 
;; Unveil the full butt at the cursor temporarily with
;; 
;;     {M-x lisp-butt-unfontify RET}
;; 
;; Customize lisp-butt-auto-unfontify
;; 
;;     {M-x customize-variable RET lisp-butt-auto-unfontify RET }
;; 
;; for automatic unfontification when point hits a butt.
;; 
;; Some configuration is possible.  See
;; 
;;     {M-x customize-group RET lisp-butt RET}
;; 
;; To turn on lisp-butt-mode automatically see
;; 
;;     {M-x customize-variable RET lisp-butt-global-mode RET}
;; 
;; See also the literate source file for modifying the whole thing.  E.g. see
;; https://gitlab.com/marcowahl/lisp-butt-mode.


;;; Code:


;; customizable

(defcustom lisp-butt-hole
  ?.
  "The character replacing the hole."
  :type 'character
  :group 'lisp-butt)

(defcustom lisp-butt-mode-lighter
  " (.)"
  "Text in the mode line to indicate that the mode is on."
  :type 'string
  :group 'lisp-butt)

(defcustom lisp-butt-modes
  '(lisp-mode emacs-lisp-mode clojure-mode)
  "Modes considered by `lisp-butt-global-mode'."
  :type '(repeat symbol)
  :group 'lisp-butt)

(defvar lisp-butt-regexp ")\\())+\\))")

(defvar lisp-butt-pattern
  `((,lisp-butt-regexp
     (1 (compose-region
         (match-beginning 1) (match-end 1)
         lisp-butt-hole)
        nil))))

(defcustom lisp-butt-auto-unfontify
  t
  "Automatically unfontify when cursor hits a butt."
  :type '(boolean)
  :group 'lisp-butt)


;; core

(defun lisp-butt-set-slim-display ()
  "Function to produce nicer Lisp butts."
  (font-lock-add-keywords nil lisp-butt-pattern))

(defun lisp-butt-unset-slim-display ()
  "Function to undo the nicer Lisp butts."
  (font-lock-remove-keywords nil lisp-butt-pattern)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward lisp-butt-regexp nil t)
        (decompose-region (match-beginning 0) (match-end 0))))))

;;;###autoload
(defun lisp-butt-unfontify ()
  "Unfontify Lisp butt at point."
  (interactive)
  (let ((point (point)))
    (while (and (< (point-min) (point))
		(string= ")" (buffer-substring-no-properties (1- (point)) (point))))
      (backward-char))
    (save-match-data
      (re-search-forward ")*")
      (font-lock-unfontify-region
       (match-beginning 0) (match-end 0))
      (let ((composi (find-composition (- (match-end 0) 2))))
	(when composi
	  (decompose-region (nth 0 composi) (nth 1 composi)))))
    (goto-char point)))

(defun lisp-butt-unfontify-on-paren ()
  "Unfontify Lisp butt at point when before a paren."
  (when (and lisp-butt-mode (= ?\) (following-char)))
    (lisp-butt-unfontify)))


;; mode definition

;;;###autoload
(define-minor-mode lisp-butt-mode
  "Display slim lisp butts."
  :lighter lisp-butt-mode-lighter
  (cond
   (lisp-butt-mode (lisp-butt-set-slim-display)
                   (when lisp-butt-auto-unfontify
                     (add-hook 'post-command-hook 'lisp-butt-unfontify-on-paren 0 t)))
   (t (when lisp-butt-auto-unfontify
        (remove-hook 'post-command-hook 'lisp-butt-unfontify-on-paren t))
      (lisp-butt-unset-slim-display)))
  (font-lock-flush))

;;;###autoload
(define-global-minor-mode lisp-butt-global-mode
  lisp-butt-mode
  (lambda ()
    (when (apply #'derived-mode-p lisp-butt-modes)
        (lisp-butt-mode))))


(provide 'lisp-butt-mode)

;;; lisp-butt-mode.el ends here
