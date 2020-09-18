;;; archive-region.el --- Move region to archive file instead of killing

;; Copyright (C) 2010,2013,2014  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/archive-region.el
;; Package-Version: 20200316.1425
;; Package-Commit: 53cd2d96ea7c33f320353982b36854f25c900c2e
;; Keywords: languages
;; Version: 0.1
;; URL: https://github.com/rubikitch/archive-region
;; Package-Requires: ((emacs "24.4"))

;;; This file is NOT part of GNU Emacs

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

;; Extend C-w to have archive feature.
;; C-u C-w moves region to the archive file.
;; C-u C-u C-w opens the archive file.
;; The archive files have suffix "_archive" after original filename.


;;; Commands:

;; Below are complete command list:

;;  `archive-region'
;;    Move the region to archive file.
;;  `archive-region-open-archive-file-other-window'
;;    Open archive file.
;;  `kill-region-or-archive-region'
;;    Extend `kill-region' (C-w) to have archive feature.


;;; Customizable Options:

;; Below are customizable option list:


;;; Installation:

;; Put archive-region.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))

;; And the following to your ~/.emacs startup file.

;;   (require 'archive-region)

;; No need more.


;;; Customize:

;; All of the above can customize by:
;;   M-x customize-group RET archive-region RET


;;; Code:

(require 'cl-lib)
(require 'newcomment)

(defgroup archive-region nil
  "archive-region"
  :group 'languages)

(defvar archive-region-filename-suffix "_archive")
(defvar archive-region-date-format "[%Y/%m/%d]")

(defun archive-region (s e)
  "Move the region to archive file."
  (interactive "r")
  (or buffer-file-name (error "Need filename"))
  (save-restriction
    (narrow-to-region s e)
    (uncomment-region (point-min) (point-max))
    (archive-region-add-header)
    (goto-char (point-max))
    (insert "\n")
    (append-to-file (point-min) (point-max) (archive-region-current-archive-file))
    (delete-region (point-min) (point-max))))

(defun archive-region-add-header ()
  (goto-char (point-min))
  (insert (format-time-string archive-region-date-format) "\n"
          (format "%S\n" (archive-region-link-to-original)))
  (let ((comment-start (or comment-start "#")))
    (comment-region (point-min) (point))))

(defun archive-region-link-to-original ()
  (list 'archive-region-pos
        (save-excursion
          (save-restriction
            (widen)
            (if (= (line-number-at-pos) 1)
                nil
              ;; find previous nonempty line
              (while (progn (forward-line -1)
                            (eq (point-at-bol) (point-at-eol))))
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))

(defun archive-region-pos (line)
  (find-file-other-window (archive-region-current-original-file))
  (save-restriction
    (widen)
    (goto-char (point-min))
    (and line
         (search-forward (concat "\n" line "\n") nil t)
         (forward-line -1))))

(defun archive-region-current-archive-file ()
  (or buffer-file-name (error "Need filename"))
  (concat buffer-file-name archive-region-filename-suffix))

(defun archive-region-current-original-file ()
  (or buffer-file-name (error "Need filename"))
  (replace-regexp-in-string
   (concat (regexp-quote archive-region-filename-suffix) "$")
   "" buffer-file-name))

(defun archive-region-open-archive-file (&optional func)
  "Open archive file."
  (interactive)
  (unless (file-exists-p (archive-region-current-archive-file))
    (error "Archive file does not exist."))
  (funcall (or func 'find-file) (archive-region-current-archive-file)))

(defun archive-region-open-archive-file-other-window ()
  "Open archive file."
  (interactive)
  (archive-region-open-archive-file 'find-file-other-window))

(defun kill-region-or-archive-region (arg s e)
  "Extend `kill-region' (C-w) to have archive feature.
C-w: `kill-region' (normal C-w)
C-u C-w: `archive-region' (move text to archive file) / also in kill-ring
C-u C-u C-w: `archive-region-open-archive-file-other-window' (open archive file)"
  (interactive "p\nr")
  (cl-case arg
    (1  (if (and (boundp 'cua--rectangle) cua--rectangle)
            (cua-cut-rectangle nil)
          (kill-region s e)))
    (4  (kill-new (buffer-substring s e)) (archive-region s e))
    (16 (archive-region-open-archive-file-other-window))))

(substitute-key-definition 'kill-region 'kill-region-or-archive-region global-map)

(provide 'archive-region)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; archive-region.el ends here
