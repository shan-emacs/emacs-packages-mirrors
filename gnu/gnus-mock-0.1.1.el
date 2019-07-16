;;; gnus-mock.el --- Mock Gnus installation for testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Version: 0.1.1

;; This program is free software; you can redistribute it and/or modify
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

;; This module provides a reproducible Gnus installation, including
;; dummy data, that can be used for Gnus development and testing.
;; Call `gnus-mock-start' from your currently-running Emacs to start a
;; new Emacs instance, skipping all user init (ie startup as -Q), but
;; preloading a mock Gnus installation.  All normal Gnus startup
;; commands will begin a session within this mock installation.

;; The developer can also specify a different Emacs executable to
;; start (for instance, when working on a Git branch checked out in a
;; worktree).  This is controlled by the `gnus-mock-emacs-program'
;; option.

;; The mock session starts with some predefined servers, as well as
;; some dummy mail data.  At startup, all dummy data is copied into a
;; temporary directory, which is deleted at shutdown.  The environment
;; can thus be loaded, tweaked, trashed, and re-loaded with impunity.
;; To fully restore a clean testing environment, simply quit the Emacs
;; process and restart it from the parent process by running
;; `gnus-mock-start' again.  Alternately it's possible to restart "in
;; place" by calling `gnus-mock-reload', though, depending on what the
;; developer has gotten up to, this isn't guaranteed to completely
;; restore the environment.

;; Users have two options for adding custom configuration to the mock
;; session:

;; - `gnus-mock-gnus-settings' can be set to a filename, the contents
;;    of which will be appended to the .gnus.el startup file in the
;;    mock session.  This code will be executed at Gnus startup.

;; - `gnus-mock-init-setting' should also be a filename, the contents
;;   of which will be appended to the init.el file that is loaded when
;;   the child Emacs process starts.

;; It's possible to compose and send mail in a mock Gnus session; the
;; mail will be sent using the value of `gnus-mock-sendmail-program'.
;; If Python is available on the user's system, this option will be
;; set to a Python program that simply accepts the outgoing mail and
;; shunts it to the "incoming" mailbox of the pre-defined nnmaildir
;; server.

;;; Code:

(require 'gnus)
(require 'message)

(defgroup gnus-mock nil
  "Options for the mock Gnus installation."
  :group 'gnus)

(defcustom gnus-mock-gnus-file nil
  "Path to an additional Gnus config file for mock Gnus.
The contents of this file will be appended to gnus-mock's Gnus
init file, which will be loaded when Gnus is started."
  :group 'gnus-mock
  :type 'file)

(defcustom gnus-mock-init-file nil
  "Path to an additional init config file for mock Gnus.
The contents of this file will be appended to gnus-mock's init
file, which will be loaded when the child Emacs process is
started."
  :group 'gnus-mock
  :type 'file)

(defcustom gnus-mock-emacs-program "emacs"
  "Name of the Emacs executable to use for the mock session."
  :group 'gnus-mock
  :type 'string)

(defcustom gnus-mock-cleanup-p t
  "When non-nil, delete temporary files after shutdown.
Each Gnus mock session will create a unique temporary directory,
so multiple sessions will not conflict if this option is nil."
  :group 'gnus-mock
  :type 'boolean)

(defcustom gnus-mock-use-images t
  "When non-nil, use some cute Gnus-mock-specific images."
  :group 'gnus-mock
  :type 'boolean)

(defcustom gnus-mock-sendmail-program
  (when (executable-find "python")
    (if (memq system-type '(cygwin ms-dos windows-nt))
	"windows-sendmail-wrapper.cmd"
      "fakesendmail.py"))
  "Program used as the value of `sendmail-program'."
  :group 'gnus-mock
  :type 'string)

(defconst gnus-mock-data-dir
  (file-name-as-directory (expand-file-name
			   "data"
			   (file-name-directory load-file-name)))
  "Source directory for Gnus mock data.")

;;;###autoload
(defun gnus-mock-start ()
  "Start a new Emacs process, with the Gnus mock setup.
The new Emacs process will be started as \"-Q\", with the mock
Gnus settings pre-loaded.  Any of the normal Gnus entry points
will start a mock Gnus session."
  (interactive)
  (let* ((mock-tmp-dir (make-temp-file "emacs-gnus-mock-" t))
	 (init-file (expand-file-name "init.el" mock-tmp-dir)))
    (with-temp-buffer
      (insert "(setq "
	      (format
	       "gnus-home-directory \"%s\"
init-file-user \"%s\"
sendmail-program \"%s\"
message-directory \"%s\"
gnus-startup-file \"%s\"
gnus-init-file \"%s\"
nndraft-directory \"%s\"
gnus-agent-directory \"%s\"
gnus-directory \"%s\"
"
	       mock-tmp-dir
	       "mockturtle"
	       (expand-file-name gnus-mock-sendmail-program mock-tmp-dir)
	       mock-tmp-dir
	       (expand-file-name ".newsrc" mock-tmp-dir)
	       (expand-file-name ".gnus" mock-tmp-dir)
	       (expand-file-name "drafts/" mock-tmp-dir)
	       (expand-file-name "agent/" mock-tmp-dir)
	       (expand-file-name "News/" mock-tmp-dir))
	      ")\n\n")
      ;; Constant that can be checked if we need to know it's a mock
      ;; session.
      (insert "(defconst gnus-mock-p t)\n")
      ;; Constant for use in `gnus-mock-reload', which is defined in
      ;; the .gnus.el startup file.
      (insert (format "(defconst gnus-mock-data-dir \"%s\")\n"
		      gnus-mock-data-dir))
      (when gnus-mock-cleanup-p
	(insert
	 (format
	  "(add-hook 'kill-emacs-hook (lambda () (delete-directory \"%s\" t)))\n"
	  mock-tmp-dir)))
      (when gnus-mock-use-images
	(insert
	 (format "(add-to-list 'load-path \"%s/data\")\n"
		 mock-tmp-dir)))
      (write-file init-file))
    ;; Put our data and config in place.
    (copy-directory
     gnus-mock-data-dir
     (file-name-as-directory mock-tmp-dir) nil nil t)
    ;; Git doesn't let us commit empty directories, so create our
    ;; necessary empty maildir bits.
    (mapc (lambda (path) (make-directory path t))
	  (mapcar (lambda (dir)
		    (format "%s/test/%s" mock-tmp-dir dir))
		  '("Welcome/new" "Welcome/tmp" "Welcome/.nnmaildir/marks"
		    "incoming/tmp" "incoming/new" "incoming/cur"
		    "incoming/.nnmaildir/marks" "incoming/.nnmaildir/nov"
		    "mails/tmp" "mails/new" "mails/.nnmaildir/marks")))
    ;; Possibly insert additional config.
    (when gnus-mock-init-file
      (with-temp-buffer
	(insert-file-contents gnus-mock-init-file)
	(append-to-file
	 (point-min) (point-max) init-file)))
    (when gnus-mock-gnus-file
      (with-temp-buffer
	(insert-file-contents gnus-mock-gnus-file)
	(append-to-file
	 (point-min) (point-max)
	 (expand-file-name ".gnus.el" mock-tmp-dir))))
    ;; There are absolute paths in the .newsrc.eld file, so doctor
    ;; that file.
    (with-current-buffer (find-file-noselect
			  (expand-file-name ".newsrc.eld" mock-tmp-dir))
      (while (re-search-forward "REPLACE_ME" (point-max) t)
	(replace-match mock-tmp-dir t))
      (basic-save-buffer))
    (condition-case nil
     (make-process :name "gnus-mock" :buffer nil
		   :command (list gnus-mock-emacs-program
				  "-Q" "--load" init-file)
		   :stderr "*gnus mock errors*")
     (error (when gnus-mock-cleanup-p
	      (delete-directory mock-tmp-dir t))))))

;;;; ChangeLog:

;; 2018-10-12  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	gnus-mock/data/.gnus.el: Fix comment
;; 
;; 	* packages/gnus-mock/data/.gnus.el: Customization option name has
;; 	 changed, note the new name.
;; 
;; 2018-10-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	packages/gnus-mock: Adjust copyright
;; 
;; 2018-10-12  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Provide support for fakesendmail script in windows, bump to 0.1.1
;; 
;; 	* packages/gnus-mock/data/windows-sendmail-wrapper.cmd: Executable
;; 	 wrapper for the fakesendmail.py Python script.
;; 	* packages/gnus-mock/gnus-mock.el (gnus-mock-sendmail-program): Detect
;; 	 if we're using Windows, and set accordingly.
;; 
;; 2018-10-11  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Maybe clean up dirs if the start process fails
;; 
;; 	* packages/gnus-mock/gnus-mock.el (gnus-mock-start): Check value of
;; 	 gnus-mock-cleanup-p.
;; 
;; 2018-10-11  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Warn user if sendmail-program is unavailable
;; 
;; 	* packages/gnus-mock/data/.gnus.el (gnus-started-hook): Eventually
;; 	 come up with a more robust solution for sending messages.
;; 
;; 2018-10-11  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Create necessary empty maildir on startup
;; 
;; 	* packages/gnus-mock/gnus-mock.el (gnus-mock-start): Git won't commit
;; 	 empty directories to the repository.
;; 
;; 2018-10-10  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Split gnus-mock-settings-file into two separate options
;; 
;; 	* packages/gnus-mock/gnus-mock.el (gnus-mock-gnus-file,
;; 	 gnus-mock-init-file): Remove `gnus-mock-settings-file', replace with
;; 	 these two options. The former gets appended to the Gnus startup
;; 	 file, the latter gets appended to the Emacs init file.
;; 
;; 2018-10-10  Eric Abrahamsen  <eric@ericabrahamsen.net>
;; 
;; 	Gnus mock initial commit
;; 
;; 	* packages/gnus-mock/gnus-mock.el: Basic package, including
;; 	 `gnus-mock-start' command.
;; 	* packages/gnus-mock/data/: Data directory.
;; 


(provide 'gnus-mock)
;;; gnus-mock.el ends here
