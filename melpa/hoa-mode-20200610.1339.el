;;; hoa-mode.el --- Major mode for the HOA format -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2017, 2019-2020  Alexandre Duret-Lutz

;; Author: Alexandre Duret-Lutz <adl@lrde.epita.fr>
;; Maintainer: Alexandre Duret-Lutz <adl@lrde.epita.fr>
;; URL: https://gitlab.lrde.epita.fr/spot/emacs-modes
;; Package-Version: 20200610.1339
;; Package-Commit: 18f5c981e256f867f29a93376ccdc04717b159cd
;; Keywords: major-mode, automata, convenience
;; Created: 2015-11-13

;;; License:

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing files in the Hanoi Omega Automata format.
;; (See URL `http://adl.github.io/hoaf/' for that format.)  This
;; provides rules for syntax highlighting, some navigation functions,
;; and a convenient way to display the automata in Emacs.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hoa\\'" . hoa-mode))

;;;###autoload
(add-to-list 'magic-mode-alist '("\\<HOA:\\s-*v" . hoa-mode))

(defgroup hoa-mode nil
  "Major mode for editing Hanoi Omega Automata files."
  :group 'data)

(defgroup hoa-mode-faces nil
  "Faces used by `hoa-mode'."
  :group 'hoa-mode
  :group 'faces)

(defface hoa-header-uppercase-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for headers with an uppercase initial."
  :group 'hoa-mode-faces)

(defface hoa-header-lowercase-face
  '((t :inherit font-lock-type-face :weight normal))
  "Face for headers with a lowercase initial."
  :group 'hoa-mode-faces)

(defface hoa-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used for --BODY--, --END--, and --ABORT--."
  :group 'hoa-mode-faces)

(defface hoa-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face used for Inf, Fin, t, and f."
  :group 'hoa-mode-faces)

(defface hoa-acceptance-set-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used for acceptance sets."
  :group 'hoa-mode-faces)

(defface hoa-alias-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for aliases."
  :group 'hoa-mode-faces)

(defface hoa-ap-number-face
  '((t :inherit font-lock-constant-face))
  "Face used for numbers that denote atomic propositions."
  :group 'hoa-mode-faces)

(defconst hoa-alias-regex
  "@[a-zA-Z0-9_.-]*\\_>"
  "Regex for matching aliases.")

(defvar hoa-font-lock-keywords-1
  `(("\\_<--\\(:?BODY\\|END\\|ABORT\\)--" . 'hoa-keyword-face)
    ("\\_<\\(:?Inf\\|Fin\\|t\\|f\\)\\_>" . 'hoa-builtin-face)
    ("\\_<[A-Z][a-zA-Z0-9_.-]*:" . 'hoa-header-uppercase-face)
    ("\\_<[a-z][a-zA-Z0-9_.-]*:" . 'hoa-header-lowercase-face)
    (,hoa-alias-regex . 'hoa-alias-face))
  "Fontification rules for keywords, builtins, headers and aliases.")

(defvar hoa-font-lock-keywords-2
  (append hoa-font-lock-keywords-1
	  `(("(\\s-*\\([0-9]+\\)\\s-*)" 1 'hoa-acceptance-set-face)
	    ("{\\(\\([0-9]\\|\\s-\\)+\\)}" 1 'hoa-acceptance-set-face)
	    ;; numbers between brackets denote atomic propositions.
	    ("\\["
	     ("\\_<[0-9]+\\_>"
	      (save-excursion (search-forward "]" nil t))
	      nil
	      (0 'hoa-ap-number-face)))
	    ;; likewise for numbers following an Alias: definition
	    (,(concat "Alias:\\s-*" hoa-alias-regex)
	     ("\\_<[0-9]+\\_>"
	      (save-excursion
		(re-search-forward
		 (concat "\\(" hoa-alias-regex
			 "\\|[0-9|&!]\\|\\_<[ft]\\_>\\|\\s-\\)+") nil t))
	      nil
	      (0 'hoa-ap-number-face)))))
  "Complete fontification rules, including acceptance sets and AP numbers.")

(defvar hoa-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?/ ". 14bn" st)
    (modify-syntax-entry ?* ". 23bn" st)
    st)
  "Syntax table for `hoa-mode'.")

(defun hoa-at-start-of-automaton ()
  "Is the point at the start of an automaton?"
  (looking-at "HOA:"))

(defun hoa-start-of-automaton ()
  "Move to the start of the automaton at point."
  (interactive)
  (search-backward "HOA:"))

(defun hoa-end-of-automaton ()
  "Move to the end of the automaton at point."
  (interactive)
  ; if we are pointing inside something that looks like --END-- or
  ; --ABORT--, back out a bit.
  (if (looking-at "[ENDABORT-]*-")
      (backward-word))
  (re-search-forward "--\\(END\\|ABORT\\)--\n?"))

(defun hoa-mark-automaton-at-point ()
  "Mark the automaton at point."
  (interactive)
  (hoa-end-of-automaton)
  (set-mark (point))
  (hoa-start-of-automaton))

(defun hoa-display-buffer-active ()
  "Check whether an HOA automaton is actually displayed.

Returns the window of the display buffer, or nil."
  (get-buffer-window hoa-display-buffer))

(defun hoa-display-buffer-refresh ()
  "Update any displayed automaton with the automaton at point."
  (if (hoa-display-buffer-active)
      (hoa-display-automaton-at-point)))

(defun hoa-next-automaton ()
  "Move to the next automaton, and optionally update display.

This works as `hoa-end-of-automaton', but if an automaton is displayed,
the display is updated with the new automaton."
  (interactive)
  (hoa-end-of-automaton)
  (hoa-display-buffer-refresh))

(defun hoa-previous-automaton ()
  "Move to the previous automaton, and optionally update display.

This works as `hoa-start-of-automaton', but if an automaton is displayed,
the display is updated with the new automaton."
  (interactive)
  (unless (hoa-at-start-of-automaton)
    (hoa-start-of-automaton))
  (hoa-start-of-automaton)
  (hoa-display-buffer-refresh))

(defcustom hoa-display-error-buffer "*hoa-dot-error*"
  "The name of the buffer to display errors from `hoa-display-command'."
  :group 'hoa-mode
  :type 'string)

(defcustom hoa-display-buffer "*hoa-display*"
  "The name of the buffer to display automata."
  :group 'hoa-mode
  :type 'string)

(defcustom hoa-display-command "autfilt --dot='barf(Lato)' | dot -Tpng"
  "Command used to display HOA files.

The command is expected to take the automaton in HOA format on
its standard input, and output an image in PNG format on its
standard output.

The default value uses the tools autfilt (part of the Spot
package, see URL `https://spot.lrde.epita.fr/') and dot (part of
the GraphViz package, see URL `http://www.graphviz.org/').
It also assumes that the Lato font is installed."
  :group 'hoa-mode
  :type 'string)

(defun hoa-display-automaton-at-point (&optional arg)
  "Display the automaton-at-point.

This uses the command in `hoa-display-command' to convert HOA
into PNG, and then display the result in `hoa-display-buffer'.
If the command terminates with an error, its standard error is
put in `hoa-display-error-buffer' and shown.

With a numeric prefix, the value of `hoa-display-command' can
be edited before it is executed."
  (interactive "P")
  (when arg
    (setq hoa-display-command (read-string "command: " hoa-display-command)))
  (let ((b (save-excursion (if (not (looking-at "HOA:"))
			       (hoa-start-of-automaton)
			     (point))))
	(e (save-excursion (hoa-end-of-automaton) (point)))
	(dotbuf (generate-new-buffer "*hoa-dot-output*"))
	(errfile (make-temp-file
		  (expand-file-name "hoadot" temporary-file-directory)))
	(coding-system-for-read 'no-conversion))
    (with-current-buffer dotbuf
      (set-buffer-multibyte nil))
    (let ((exit-status
	   (call-process-region b e shell-file-name nil (list dotbuf errfile)
				nil shell-command-switch hoa-display-command)))
      (when (equal 0 exit-status)
        (let ((img-string (with-current-buffer dotbuf (buffer-string)))
              (img-type (if (image-type-available-p 'imagemagick)
                             'imagemagick
                          'png)))
          ;; Display the buffer before we load the image, so that we
          ;; can specify a max size.  These max-width/max-height are
          ;; ignored if 'imagemagick is not installed.
          (with-current-buffer (get-buffer-create hoa-display-buffer)
            (display-buffer (current-buffer))
            (setq buffer-read-only nil)
            (erase-buffer)
            (pcase-let ((`(,ax ,ay ,bx ,by)
                         (window-body-pixel-edges (hoa-display-buffer-active))))
              (let ((win-width (- bx ax))
                    (win-height (- by ay)))
                (insert-image (create-image img-string img-type t
                                            :max-width win-width
                                            :max-height win-height))))
	    (setq buffer-read-only t)))))
      (when (file-exists-p errfile)
	(when (< 0 (nth 7 (file-attributes errfile)))
	  (with-current-buffer (get-buffer-create hoa-display-error-buffer)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (format-insert-file errfile nil)
	    (display-buffer (current-buffer))))
	(delete-file errfile))
      (kill-buffer dotbuf)))

(defvar hoa-mode-map
  (let ((map (make-keymap)))
    (define-key map "\M-e" 'hoa-end-of-automaton)
    (define-key map "\M-a" 'hoa-start-of-automaton)
    (define-key map (kbd "<M-up>") 'hoa-previous-automaton)
    (define-key map (kbd "<M-down>") 'hoa-next-automaton)
    (define-key map "\C-\M-h" 'hoa-mark-automaton-at-point)
    (define-key map "\C-c\C-c" 'hoa-display-automaton-at-point)
    map)
  "Keymap for `hoa-mode'.")

;;;### autoload
(define-derived-mode hoa-mode fundamental-mode "HOA"
  "Major mode for editing files in the Hanoi Omega Automata format.

See URL `http://adl.github.io/hoaf/' for a definition of that format.

The following key bindings are installed in hoa-mode:

\\{hoa-mode-map}

By default the `hoa-display-automaton-at-point' function requires
extra software (Spot and GraphViz), but can be configured to use
other tools.  See that function for details."
  :group 'hoa-mode
  (setq font-lock-defaults '((hoa-font-lock-keywords-1
			      hoa-font-lock-keywords-1
			      hoa-font-lock-keywords-2))))

(provide 'hoa-mode)
;;; hoa-mode.el ends here
