;;; temp-buffer-browse.el --- temp buffer browse mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: convenience

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

;;; Commentary:

;; Allow keys `SPC', `DEL' and `RET' following a temp buffer popup to
;; scroll up, scroll down and close the temp buffer window,
;; respectively.

;;; Code:

(eval-and-compile
  (or (fboundp 'set-temporary-overlay-map) ; new in 24.4.
      (defun set-temporary-overlay-map (map &optional keep-pred)
        "Set MAP as a temporary keymap taking precedence over most other keymaps.
Note that this does NOT take precedence over the \"overriding\" maps
`overriding-terminal-local-map' and `overriding-local-map' (or the
`keymap' text property).  Unlike those maps, if no match for a key is
found in MAP, the normal key lookup sequence then continues.

Normally, MAP is used only once.  If the optional argument
KEEP-PRED is t, MAP stays active if a key from MAP is used.
KEEP-PRED can also be a function of no arguments: if it returns
non-nil then MAP stays active."
        (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
               (overlaysym (make-symbol "t"))
               (alist (list (cons overlaysym map)))
               (clearfun
                `(lambda ()
                   (unless ,(cond ((null keep-pred) nil)
                                  ((eq t keep-pred)
                                   `(eq this-command
                                        (lookup-key ',map
                                                    (this-command-keys-vector))))
                                  (t `(funcall ',keep-pred)))
                     (set ',overlaysym nil) ;Just in case.
                     (remove-hook 'pre-command-hook ',clearfunsym)
                     (setq emulation-mode-map-alists
                           (delq ',alist emulation-mode-map-alists))))))
          (set overlaysym overlaysym)
          (fset clearfunsym clearfun)
          (add-hook 'pre-command-hook clearfunsym)
          (push alist emulation-mode-map-alists)))))

(defvar temp-buffer-browse--window nil)

(defvar temp-buffer-browse-map
  (let ((map (make-sparse-keymap))
        (quit (lambda ()
                (interactive)
                (when (window-live-p temp-buffer-browse--window)
                  (quit-window nil temp-buffer-browse--window))))
        (up (lambda ()
              (interactive)
              (when (window-live-p temp-buffer-browse--window)
                (with-selected-window temp-buffer-browse--window
                  (condition-case nil
                      (scroll-up)
                    (end-of-buffer (quit-window)))))))
        (down (lambda ()
                (interactive)
                (when (window-live-p temp-buffer-browse--window)
                  (with-selected-window temp-buffer-browse--window
                    (scroll-up '-))))))
    (define-key map "\C-m" quit)
    (define-key map [return] quit)
    (define-key map " " up)
    (define-key map (kbd "DEL") down)
    (define-key map [delete] down)
    (define-key map [backspace] down)
    map))

(defun temp-buffer-browse-setup ()
  "Browse temp buffers easily.
Set up `SPC', `DEL' and `RET' to scroll up, scroll down and close
the temp buffer window, respectively."
  (unless (derived-mode-p 'completion-list-mode)
    (setq temp-buffer-browse--window (selected-window))
    ;; When re-using existing window don't call
    ;; `fit-window-to-buffer'. See also (info "(elisp)Window
    ;; Parameters").
    (when (and (window-full-width-p)
               (memq (cadr (window-parameter nil 'quit-restore))
                     '(window frame)))
      (fit-window-to-buffer nil (floor (frame-height) 2))
      ;; In case buffer contents are inserted asynchronously such as
      ;; in `slime-inspector-mode'.
      (add-hook 'after-change-functions
                (lambda (&rest _)
                  (fit-window-to-buffer nil (floor (frame-height) 2)))
                nil 'local))
    (let ((o (make-overlay (point-min) (point-max))))
      (overlay-put o 'evaporate t)
      (overlay-put o 'window t)
      (overlay-put o 'line-prefix
                   (propertize
                    "|" 'display
                    (unless (zerop (or (frame-parameter nil 'left-fringe) 0))
                      '(left-fringe centered-vertical-bar warning))
                    'face 'warning))
      (set-temporary-overlay-map
       temp-buffer-browse-map
       (lambda ()
         ;; When any error happens the keymap is active forever.
         (with-demoted-errors
           (or (and (window-live-p temp-buffer-browse--window)
                    (not (member (this-command-keys) '("\C-m" [return])))
                    (eq this-command (lookup-key temp-buffer-browse-map
                                                 (this-command-keys))))
               (overlay-put o 'line-prefix nil))))))))

;;;###autoload
(define-minor-mode temp-buffer-browse-mode nil
  :lighter ""
  :global t
  (if temp-buffer-browse-mode
      (add-hook 'temp-buffer-show-hook 'temp-buffer-browse-setup t)
    (remove-hook 'temp-buffer-show-hook 'temp-buffer-browse-setup)))

;;;; ChangeLog:

;; 2013-08-18  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge temp-buffer-browse as our subdirectory
;; 


(provide 'temp-buffer-browse)
;;; temp-buffer-browse.el ends here
