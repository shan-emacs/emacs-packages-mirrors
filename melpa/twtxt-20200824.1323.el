;;; twtxt.el --- Client twtxt

;; Author: DEADBLACKCLOVER <deadblackclover@protonmail.com>
;; Version: 0.1
;; Package-Version: 20200824.1323
;; Package-Commit: e7bafaf92124bb4f2a0be5c1a635b80f9b3a8c87
;; URL: https://github.com/deadblackclover/twtxt-el
;; Package-Requires: ((emacs "25.1") (request "0.2.0"))

;; Copyright (c) 2020, DEADBLACKCLOVER. This file is
;; licensed under the GNU General Public License version 3 or later. See
;; the LICENSE file.

;;; Commentary:

;; twtxt is a decentralised, minimalist microblogging service for hackers.

;;; Code:
(require 'cl-lib)
(require 'request)

(defgroup twtxt nil
  "Client twtxt"
  :group 'twtxt)

(defcustom twtxt-file "~/twtxt"
  "Path to twtxt file."
  :type 'file
  :group 'twtxt)

(defcustom twtxt-following nil
  "Following list."
  :type '(repeat (list (string :tag "Name")
		       (string :tag "URL")))
  :group 'twtxt)

(defvar twtxt-timeline-list nil
  "Timeline list.")

(defvar twtxt-username ""
  "Temporary storage of username.")

(defun twtxt-get-datetime ()
  "Getting date and time according to RFC 3339 standard."
  (concat (format-time-string "%Y-%m-%dT%T")
	  ((lambda (x)
	     (concat (substring x 0 3) ":" (substring x 3 5)))
	   (format-time-string "%z"))))

(defun twtxt-parse-text (text)
  "Convert TEXT to list post."
  (split-string text "\n"))

(defun twtxt-replace-tab (str)
  "Replacing tabs with line breaks in STR."
  (replace-regexp-in-string "\t" "\n" str))

(defun twtxt-sort-post (list)
  "Sorting posts in LIST."
  (sort list #'string>))

(defun twtxt-append-username (text)
  "Append username in TEXT."
  (mapcar (lambda (item)
	    (concat twtxt-username "\t" item))
	  (twtxt-parse-text text)))

(defun twtxt-push-text (text)
  "Concatenate the resulting TEXT with the current list."
  (setq twtxt-timeline-list (append twtxt-timeline-list (twtxt-append-username text))))

(defun twtxt-fetch (url)
  "Getting text by URL."
  (progn (request url
	   :parser 'buffer-string
	   :success (cl-function (lambda
				   (&key
				    data
				    &allow-other-keys)
				   (twtxt-push-text data))))))

(defun twtxt-fetch-list ()
  "Getting a list of texts."
  (mapc (lambda (item)
	  (setq twtxt-username (concat "[[" (car (cdr item)) "][" (car item) "]]"))
	  (twtxt-fetch (car (cdr item)))) twtxt-following))

(defun twtxt-timeline-buffer (data)
  "Create buffer and DATA recording."
  (switch-to-buffer (get-buffer-create "*twtxt-timeline*"))
  (mapc (lambda (item)
	  (insert (twtxt-replace-tab item))
	  (insert "\n\n")) data)
  (org-mode)
  (goto-char (point-min)))

(defun twtxt-timeline ()
  "View your timeline."
  (interactive)
  (twtxt-fetch-list)
  (twtxt-timeline-buffer (twtxt-sort-post twtxt-timeline-list))
  (setq twtxt-timeline-list nil))

(defun twtxt-post (post)
  "POST a status update."
  (interactive "sPost:")
  (append-to-file (concat (twtxt-get-datetime) "\t" post "\n") nil twtxt-file))

(provide 'twtxt)
;;; twtxt.el ends here
