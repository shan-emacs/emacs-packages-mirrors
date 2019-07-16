;;; company-ebdb.el --- company-mode completion backend for EBDB in message-mode

;; Copyright (C) 2013-2014, 2016  Free Software Foundation, Inc.

;; Author: Jan Tatarik <jan.tatarik@gmail.com>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Version: 1.1
;; Package-Requires: ((company "0.9.4") (ebdb "0.2"))

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

;; Company integration for EBDB.  Copied more or less intact from
;; company-bbdb, originally by Jan Tatarik.

;;; Code:

(require 'company)
(require 'cl-lib)

(declare-function ebdb-record-mail "ebdb")
(declare-function ebdb-records "ebdb")
(declare-function ebdb-dwim-mail "ebdb-com")
(declare-function ebdb-search "ebdb-com")

(defgroup company-ebdb nil
  "Completion backend for EBDB."
  :group 'company)

(defcustom company-ebdb-modes '(message-mode mail-mode)
  "Major modes in which `company-ebdb' may complete."
  :type '(repeat (symbol :tag "Major mode"))
  :package-version '(company . "0.8.8"))

(defcustom company-ebdb-pop-up t
  "When non-nil, pop up an *EBDB* buffer after completion."
  :type 'boolean)

(defun company-ebdb--candidates (arg)
  (cl-mapcan (lambda (record)
	       (delq nil
		     (mapcar (lambda (mail)
			       (let ((dwim (ebdb-dwim-mail record mail)))
				 (when (string-match-p arg dwim)
				   dwim)))
			     (ebdb-record-mail record))))
             (eval '(ebdb-search (ebdb-records) `((ebdb-field-name ,arg)
						  (ebdb-field-mail ,arg))))))

(defun company-ebdb--post-complete (arg)
  (when (and company-ebdb-pop-up
	     (apply #'derived-mode-p company-ebdb-modes))
   (let* ((bits (ebdb-decompose-ebdb-address arg))
	  (recs (ebdb-message-search (car bits) (nth 1 bits))))
     (when recs
       (ebdb-display-records recs nil nil nil (ebdb-popup-window))))))

;;;###autoload
(defun company-ebdb (command &optional arg &rest ignore)
  "`company-mode' completion backend for EBDB."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ebdb))
    (prefix (and (apply #'derived-mode-p company-ebdb-modes)
                 (featurep 'ebdb-com)
                 (looking-back "^\\(To\\|Cc\\|Bcc\\): *.*? *\\([^,;]*\\)"
                               (line-beginning-position))
                 (match-string-no-properties 2)))
    (candidates (company-ebdb--candidates arg))
    (post-completion (company-ebdb--post-complete arg))
    (sorted t)
    (no-cache t)))

(add-to-list 'company-backends 'company-ebdb)

;;;; ChangeLog:

;; 2018-12-23  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Bump to 1.1
;; 
;; 2018-12-23  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Only return record addresses that actually match
;; 
;; 	* packages/company-ebdb/company-ebdb.el (company-ebdb--candidates):
;; 	 Previously was returning all the mail addresses of matching records.
;; 	 Now only return actually matching mail addresses.
;; 
;; 2018-12-23  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Provide new company-ebdb-pop-up option
;; 
;; 	* packages/company-ebdb/company-ebdb.el (company-ebdb-pop-up): In case
;; 	 users only want completion, no pop up.
;; 
;; 2018-12-23  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Add to company-backends by default
;; 
;; 	* packages/company-ebdb/company-ebdb.el: Otherwise, why would they
;; 	 hvae installed it?
;; 
;; 2018-12-22  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Use derived-mode-p with company-ebdb-modes
;; 
;; 	* packages/company-ebdb/company-ebdb.el (company-ebdb-modes): And
;; 	 remove notmuch-message-mode from the list, as that derives from
;; 	 message-mode.
;; 
;; 2018-12-22  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	[company-ebdb] Offer record role field mail addresses for completion
;; 
;; 	* packages/company-ebdb/company-ebdb.el (company-ebdb--candidates):
;; 	 Why did I have it that way?
;; 
;; 2017-08-12  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Tweaks for package format correctness
;; 
;; 2017-08-11  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Add external/contrib packages for EBDB
;; 


(provide 'company-ebdb)
;;; company-ebdb.el ends here
