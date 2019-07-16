;;; visual-filename-abbrev.el --- Visually abbreviate filenames  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc

;; Author: Tassilo Horn <tsdh@gnu.org>
;; Maintainer: Tassilo Horn <tsdh@gnu.org>
;; Keywords: tools
;; Package-Requires: ((emacs "26.1"))
;; Version: 1.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This minor mode abbreviates the directory part of filenames by using
;; overlays.  For example, a longish filename like
;;
;;    /home/myuser/Documents/Letters/Personal-Family/Letter-to-John.tex
;;
;; will be displayed like this:
;;
;;   /h…/m…/D…/L…/P…-F…/Letter-to-John.tex
;;
;; By default, the abbreviate display is disabled when point enters the overlay
;; so that you can edit the file name normally.  Also, abbreviated file names
;; are only shown if the abbreviation as actually shorter as the original one
;; (which depends on what you add as replacement).
;;
;; There's stuff to customize, just check `M-x customize-group RET
;; visual-filename-abbrev RET'.

;;; Code:

(require 'subr-x)
(require 'seq)

(defgroup visual-filename-abbrev nil
  "Visually abbreviate the directory part of filenames."
  :group 'tools)

(defcustom visual-filename-abbrev-regex
  (concat "\\(?:file://\\)?/?"
	  "\\(?:[[:alnum:]@_.-]+/\\)+[[:alnum:]@_.-]*\\.\\w+")
  "Regexp matching filenames."
  :group 'visual-filename-abbrev
  :type 'regexp)

(defcustom visual-filename-abbrev-replace-regex
  "[.@]?[[:alnum:]]\\([[:alnum:]]\\{2,\\}\\)[-_/.@]"
  "Regexp which will be visually replaced in filenames.
All matches of this regexp's group number 1 in the filenames
matching `visual-filename-abbrev-regex' will be replaced by
`visual-filename-abbrev-ellipsis'."
  :group 'visual-filename-abbrev
  :type 'regexp)

(defcustom visual-filename-abbrev-ellipsis "…"
  "String displayed instead of group 1 of `visual-filename-abbrev-regex'."
  :group 'visual-filename-abbrev
  :type 'string)

(defcustom visual-filename-abbrev-unabbreviate-under-point t
  "If non-nil, filenames under point are displayed unabbreviated."
  :group 'visual-filename-abbrev
  :type 'boolean)

(defun visual-filename-abbrev--get-abbrev (filename)
  (let ((file (file-name-nondirectory filename))
	(dir (file-name-directory filename)))
    (concat
     (file-name-as-directory
      (replace-regexp-in-string
       visual-filename-abbrev-replace-regex
       visual-filename-abbrev-ellipsis dir nil nil 1))
     file)))

(defsubst visual-filename-abbrev--get-overlay (pos)
  (car (seq-filter
	(lambda (o) (overlay-get o 'visual-filename-abbrev))
	(overlays-at pos))))

(defun visual-filename-abbrev--abbrev-shorter-p (_buffer _pos filename abbrev)
  "Return non-nil if ABBREV is shorter than FILENAME.
Shorter means less characters here."
  (< (string-width abbrev)
     (string-width filename)))

(defsubst visual-filename-abbrev--get-visual-width (str font)
  (seq-reduce (lambda (acc g) (+ acc (aref g 4)))
	      (font-get-glyphs font 0 (length str) str)
	      0))

(defun visual-filename-abbrev--abbrev-visually-shorter-p (buffer pos filename abbrev)
  "Return non-nil if ABBREV's display representation is shorter than FILENAME.
This takes the font into account."
  ;; When activated from a hook, this function may run before the current
  ;; buffer is shown in a window.  In that case, `font-at' would error with
  ;; "Specified window is not displaying the current buffer".
  (let ((window (selected-window)))
    (when (and window
	       (eq (window-buffer window) (current-buffer)))
      (let ((font (font-at pos window)))
	(< (visual-filename-abbrev--get-visual-width abbrev font)
	   (visual-filename-abbrev--get-visual-width filename font))))))

(defcustom visual-filename-abbrev-predicates
  (list #'visual-filename-abbrev--abbrev-visually-shorter-p)
  "A list of predicates inhibiting abbreviation of a filename.
A filename is only abbreviate if all predicates in this list
return true.

Each predicate is called with the following four arguments:

  - BUFFER: The buffer holding the abbreviation overlay.
  - POS: The position in BUFFER of the overlay.
  - FILE: The filename to be abbreviated.
  - ABBREV: The abbreviated version of the filename.

These predicates are available:

  - `visual-filename-abbrev--abbrev-shorter-p' ensures that an
    abbreviation is only shown if it is shorter (in the number of
    characters) than the original filename.  This is fast but
    doesn't work too good if `visual-filename-abbrev-ellipsis' is
    displayed wider than what's abbreviater (which depends on the
    font).

  - `visual-filename-abbrev--abbrev-visually-shorter-p' ensures
    that an abbreviation is only shown if it is visually shorter
    than the original filename, i.e., it takes the current font
    and, e.g., double-width unicode characters into account.
    This predicate is a bit more expensive to compute."
  :group 'visual-filename-abbrev
  :type '(repeat function))

(defun visual-filename-abbrev--abbreviate-p (buffer pos filename abbrev)
  (seq-every-p (lambda (pred)
		 (funcall pred buffer pos filename abbrev))
	       visual-filename-abbrev-predicates))

(defun visual-filename-abbrev--delete-overlays (beg end)
  (dolist (ol (overlays-in beg end))
    (when (overlay-get ol 'visual-filename-abbrev)
      (delete-overlay ol))))

(defun visual-filename-abbrev--cursor-sensor (window old-pos dir)
  ;;(message "cs: %S %S %S" window old-pos dir)
  (when-let ((ol (if (eq dir 'entered)
		     (visual-filename-abbrev--get-overlay (point))
		   (or (visual-filename-abbrev--get-overlay old-pos)
		       (visual-filename-abbrev--get-overlay (if (> (point) old-pos)
								(1- old-pos)
							      (1+ old-pos)))))))
    ;;(message "  => %S" ol)
    (if (eq dir 'entered)
	(when-let ((d (overlay-get ol 'display)))
	  (overlay-put ol 'visual-filename-abbrev--display-backup d)
	  (overlay-put ol 'display nil))
      (when-let ((d (overlay-get ol 'visual-filename-abbrev--display-backup)))
	(overlay-put ol 'display d)
	(overlay-put ol 'visual-filename-abbrev--display-backup nil)))))

(defun visual-filename-abbrev--place-overlays (start end)
  (goto-char start)
  (while (re-search-forward visual-filename-abbrev-regex end t)
    (let* ((m-beg (match-beginning 0))
	   (m-end (match-end 0))
	   (filename (match-string 0))
	   (abbrev (visual-filename-abbrev--get-abbrev filename)))
      (when (visual-filename-abbrev--abbreviate-p
	     (current-buffer) (point) filename abbrev)
	(let ((ol (or (when-let ((o (visual-filename-abbrev--get-overlay m-beg)))
			(move-overlay o m-beg m-end)
			o)
		      (make-overlay m-beg m-end nil t))))
	  (when visual-filename-abbrev-unabbreviate-under-point
	    (overlay-put ol 'cursor-sensor-functions
			 (list #'visual-filename-abbrev--cursor-sensor)))
	  (overlay-put ol 'visual-filename-abbrev t)
	  (overlay-put ol 'evaporate t)
	  (overlay-put ol 'help-echo filename)
	  (overlay-put ol 'display abbrev))))))

(defun visual-filename-abbrev--jit-lock (beg end &optional _old-len)
  "Function registered for jit-lock."
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
	(end-line (save-excursion (goto-char end) (line-end-position))))
    (visual-filename-abbrev--place-overlays beg-line end-line)))

(defvar visual-filename-abbrev--csm-before-activation nil)
(make-variable-buffer-local 'visual-filename-abbrev--csm-before-activation)

;;###autoload
(define-minor-mode visual-filename-abbrev-mode
  "Visually abbreviate the directory part of filenames."
  nil " VFAbbr" nil
  (if visual-filename-abbrev-mode
      (progn
	(jit-lock-register #'visual-filename-abbrev--jit-lock)
	(require 'cursor-sensor)
	;; Remember if c-s-m has been enabled before we enable it.
	(setq visual-filename-abbrev--csm-before-activation cursor-sensor-mode)
	(cursor-sensor-mode)
	(visual-filename-abbrev--jit-lock (window-start)
					  (window-end)))
    (jit-lock-unregister #'visual-filename-abbrev--jit-lock)
    ;; Deactivate it only if it has been disabled before we started it.
    (when visual-filename-abbrev--csm-before-activation
      (cursor-sensor-mode -1))
    (visual-filename-abbrev--delete-overlays 1 (1+ (buffer-size)))))

;;;; ChangeLog:

;; 2019-03-09  Tassilo Horn  <tsdh@gnu.org>
;; 
;; 	Improve previous fix (which didn't fix all cases)
;; 
;; 2019-03-09  Tassilo Horn  <tsdh@gnu.org>
;; 
;; 	Don't error if fontification starts before buffer is shown in window
;; 
;; 2019-03-09  Tassilo Horn  <tsdh@gnu.org>
;; 
;; 	visual-filename-abbrev: Release version 1.0
;; 
;; 2019-03-08  Tassilo Horn  <tsdh@gnu.org>
;; 
;; 	New package: visual-filename-abbrev.el
;; 


(provide 'visual-filename-abbrev)

;; Local Variables:
;; bug-reference-url-format: "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
;; End:

;;; visual-filename-abbrev.el ends here
