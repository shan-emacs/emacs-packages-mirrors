;;; org-lookup-dnd.el --- Reference the index of a D&D handbook pdf -*- lexical-binding: t; -*-

;; This program was meant a) To help me run my Dungeons and Dragons sessions
;; better, and b) as a exercise for me to learn a bit of lisp.

;; Copyright (C) 2019 Malte Lau Petersen

;; Author: Malte Lau Petersen <maltelau@protonmail.com>
;; Created: May 2019
;; Version: 0.1
;; Package-Version: 20190622.2224
;; URL: https://gitlab.com/maltelau/org-lookup-dnd
;; Package-Requires: ((emacs "24.4") (org-pdfview "0.1"))

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

;; ## INSTALLATION
;; 0. Install the dependencies
;; 1. org-lookup-dnd is available on MELPA. You can install and load it
;;    by putting this in your .emacs

;; (use-package org-lookup-dnd
;;     :ensure t
;;     :bind ("C-c d" . org-lookup-dnd-at-point))

;; 2. Customize the variable org-lookup-dnd-sources to point to
;; one or more pdf files you'd like to run this on.  For example
;; to index the table of contents on page 2 of the basic rules
;; (https://media.wizards.com/2018/dnd/downloads/DnD_BasicRules_2018.pdf)
;; and add 0 from all the page numbers in that index:

;; Path to pdf        : ~/Dowloads/DnD_BasicRules_2018.pdf
;; Page offset        : 0
;; First page of index: 2
;; Last page of index : 2

;; ## HOW TO USE IT
;; Run `org-lookup-dnd-at-point`.  If there is a word under the pointer, it will
;; search for that term.  Otherwise, write a search term in the minibuffer.
;; If there are more than one matches, you get to pick which one to link to.

;; ## DEPENDENCIES
;; - pdftotext (from poppler-utils on ubuntu)
;; - org-pdfview (from melpa)

;; ## RECOMMENDED
;; Your completion framework of choice.  See `M-x customize-variable RET org-lookup-dnd-chose RET`
;; Tested with ivy, ido, and helm.

;;; Code:

(require 'seq)
(require 'org-table)


;; Customization

(defgroup org-lookup-dnd nil
  "This package indexes some pdfs and lets you insert links
from the table of contents into your org-mode document.

You need to tell it which pdfs to index, and which pages to look at."
  :group 'org)


(defcustom org-lookup-dnd-db-file "~/.local/share/org-lookup-dnd-db.el"
  "Location to store the index on disk."
  :type '(file)
  :set #'org-lookup-dnd-new-config
  :initialize #'custom-initialize-default
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-extra-index "~/.local/share/org-lookup-dnd-extra.org"
  "Location of (org)file with extra search references.  Optional.
The format is an org table with the columns: | searchterm | path/to/pdffile | page |"
  :type '(file)
  :set #'org-lookup-dnd-new-config
  :initialize #'custom-initialize-default
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-sources nil
  "A list of source(book)s. Each entry should be a list of four elements:
1. The pdf's filename, 2. How many pages in the pdf to add to the page nr,
3. The first page of the index in the pdf, 4. the last page of the index.

Needs to be customized before org-lookup-dnd will work at all."
  :type '(repeat (list :tag ""
		       (file    :tag "Path to pdf        ")
		       (integer :tag "Page offset        ")
		       (integer :tag "First page of index")
		       (integer :tag "Last page of index ")))
  :set #'org-lookup-dnd-new-config
  :initialize #'custom-initialize-default
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-link-format "[[pdfview:%s::%d][%s]]"
  "Format string to be inserted at point with ‘org-lookup-dnd-at-point’.
The first replacement is the path to the pdf.
The second is the page number in the pdf,
and the third is the link title."
  :type '(string)
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-chose #'org-lookup-dnd-chose-vanilla
  "Completion framework to use for searching the index.

If you chose 'Custom', the value should be a function
that takes two arguments: PROMPT and COLLECTION"
  :type '(radio (const :tag "Completing-read" org-lookup-dnd-chose-vanilla)
		(const :tag "Ivy" org-lookup-dnd-chose-ivy)
		(const :tag "Ido" org-lookup-dnd-chose-ido)
		(const :tag "Helm" org-lookup-dnd-chose-helm)
		(sexp  :tag "Custom"))
  :group 'org-lookup-dnd
  :tag "Completion framework")


(declare-function ivy-read "ext:ivy")
(defun org-lookup-dnd-chose-ivy (prompt collection)
  "Use ivy.  Given PROMPT, select from COLLECTION."
  (require 'ivy)
  (ivy-read prompt collection :require-match t))

(defun org-lookup-dnd-chose-vanilla (prompt collection)
  "Use ‘completing-read’.  Given PROMPT, select from COLLECTION."
  ;; setting completion-styles to substring is needed
  ;; to match an entry without typing the name of the source
  (let ((completion-styles (list 'substring)))
    (completing-read prompt collection nil t)))

(defun org-lookup-dnd-chose-ido (prompt collection)
  "Use ido.  Given PROMPT, select from COLLECTION."
  (require 'ido)
  (ido-completing-read prompt collection nil t))

(declare-function helm-comp-read "ext:helm")
(defun org-lookup-dnd-chose-helm (prompt collection)
  "Use helm.  Given PROMPT, select from COLLECTION."
  (require 'helm)
  (helm-comp-read prompt collection :must-match t))


;; Variables

(defvar org-lookup-dnd-db nil
  "The db, loaded into memory.
A hash table, where each hash is 'file: searchterm' and each value is
a list: ('searchterm' '/path/to/file.pdf' pagenr)")


(defvar org-lookup-dnd-choice nil
  "Last dnd entry inserted.")


;; Utility functions

(defun org-lookup-dnd-dump-vars-to-file (varlist filename)
  "Simplistic dumping of variables in VARLIST to a file FILENAME."
  (unless (file-exists-p filename) (make-directory (file-name-directory filename) t))
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (org-lookup-dnd-dump varlist buf)
      (save-buffer)
      (kill-buffer))))


(defun org-lookup-dnd-dump (varlist buffer)
  "For all the variables in VARLIST, insert into BUFFER the setq statement to recreate them."
  (dolist (var varlist)
    (print (list 'setq var (list 'quote (symbol-value var)))
	   buffer)))


(defun org-lookup-dnd-delete-region-curried (pos)
  "Delete a region, accepting POS as a (START . END) list."
  (when pos (delete-region (car pos) (cdr pos))))


(defun org-lookup-dnd-extract-from-index (regexp string &optional start)
  "Match a REGEXP in STRING with exactly 2 capture groups.
Returns a list of the matches (group1 group2).
Optional: Specify the START position.
Adapted from ‘replace-regexp-in-string’."
  ;; (setq matches nil)
  (let ((len (length string))
	(start (or start 0))
	matches mb me)
    (save-match-data
      (while (and (< start len) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min len (1+ mb))))
	(setq start me)
	(setq matches
	      (cons (list (match-string 1 string) (match-string 2 string))
		    matches))))
    (reverse matches)))


;; Here comes the meat of this little library

(defun org-lookup-dnd-new-config (symbol value)
  "Set the custom varable (assign to SYMBOL the VALUE) and index the pdfs to be ready to search."
  (set symbol value)
  (when (and (bound-and-true-p org-lookup-dnd-sources)
	     (bound-and-true-p org-lookup-dnd-db-file))
    (org-lookup-dnd-parse)))


(defun org-lookup-dnd-setup ()
  "Check if the custom variables are setup, and load the db index from file."
  (unless (and (bound-and-true-p org-lookup-dnd-sources)
	       (bound-and-true-p org-lookup-dnd-db-file))
    (error "Please ensure you've customized org-lookup-dnd to point to your pdf"))
  (unless (bound-and-true-p org-lookup-dnd-db) (load-file org-lookup-dnd-db-file)))


(defun org-lookup-dnd-parse ()
  "Parse pdfs, (opt) the extra org table, and store the db on disk."
  (setq org-lookup-dnd-db
	(make-hash-table :test #'equal :size 256 :rehash-size 2.0 :rehash-threshold .97))
  (org-lookup-dnd-parse-pdfs)
  (org-lookup-dnd-parse-extras)
  (org-lookup-dnd-dump-vars-to-file '(org-lookup-dnd-db) org-lookup-dnd-db-file))


(defun org-lookup-dnd-parse-pdfs ()
  "Read in all the pdfs, and extract and index the table of contents.
Stores what it finds in ‘org-lookup-dnd-db’."
  (dolist (source org-lookup-dnd-sources)
    (let (txt lst)
      ;; For each pdf source, run pdftottext on the index,
      (setq txt (shell-command-to-string (format "pdftotext -layout -f %d -l %d %s -"
						 (nth 2 source)
						 (nth 3 source)
						 (shell-quote-argument (expand-file-name (car source))))))
      ;; remove most punctuation,
      (setq txt (replace-regexp-in-string "[-':�;~^\"\`\'•|,·./() ]" "" txt))
      ;; And extract the (term)......(page).
      (setq lst (org-lookup-dnd-extract-from-index
		 "\\([^\n[:digit:]]+\\)\n*\\([[:digit:]]+\\)" txt))
      (dolist (entry lst)
	(puthash (format "%s: %s" (file-name-base (car source)) (car entry))
		 (list (car entry)
		       (car source)
		       (+ (string-to-number (nth 1 entry)) (nth 1 source)))
		 org-lookup-dnd-db)))))


(defun org-lookup-dnd-parse-extras ()
  "Read in the extra index from ‘org-lookup-dnd-extra-index’."
  (when (file-exists-p org-lookup-dnd-extra-index)
    (let ((buf (find-file-noselect org-lookup-dnd-extra-index)))
      (with-current-buffer buf
	(goto-char (point-min))
	;; Read in the table, interpreting the page nr (3rd col) as numeric
	(dolist (entry (cdr (cdr (org-table-to-lisp))))
	  (puthash (format "%s: %s" (file-name-base (nth 1 entry)) (car entry))
		   (list (car entry)
			 (nth 1 entry)
			 (string-to-number (nth 2 entry)))
		   org-lookup-dnd-db)))
      (kill-buffer buf))))


;;;###autoload
(defun org-lookup-dnd-at-point ()
  "Search for a (dnd) term from the index, clarify which one is meant, and then output an ‘org-mode’ link to the pdf at the right page."
  (interactive)
  (org-lookup-dnd-setup)
  ;; pre-filter the completion list according to the word at point
  (let ((collection (seq-filter (lambda (x) (string-match (or (thing-at-point 'word) "") x))
				 (hash-table-keys org-lookup-dnd-db))))
    (setq org-lookup-dnd-choice
	  (funcall org-lookup-dnd-chose "Link to which entry: " collection))
    (let ((entry (gethash org-lookup-dnd-choice org-lookup-dnd-db)))
      (org-lookup-dnd-delete-region-curried (bounds-of-thing-at-point 'word))
      (insert (format org-lookup-dnd-link-format
		      (nth 1 entry)
		      (nth 2 entry)
		      (or (thing-at-point 'word) (car entry)))))))


(provide 'org-lookup-dnd)

;;; org-lookup-dnd.el ends here
