;;; vcl-mode.el --- Major mode for Varnish Configuration Language

;; Author: Sergey Poznyakoff <gray@gnu.org.ua>
;; Version: 1.0
;; Keywords: Varnish, VCL

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs support for Varnish's configuration language:
;; https://varnish-cache.org/docs/trunk/users-guide/vcl.html
;; This version of vcl-mode supports VCL-4.0.

;; The features provided are auto-indentation (based on CC-mode's
;; engine), keyword highlighting, and matching of {"..."} multi-line
;; string delimiters.

;; If you need support for VCL-2.0, you might have more luck with the older
;; package: https://github.com/ssm/elisp/blob/master/vcl-mode.el

;; Installation:
;; You may wish to use precompiled version of the mode. To create it
;; run:
;;    emacs -batch -f batch-byte-compile vcl-mode.el
;; Install the file vcl-mode.elc (and, optionally, vcl-mode.el) to
;; a directory in your Emacs load-path.

;; Customization:
;;  To your .emacs or site-start.el add:
;;  (autoload 'vcl-mode "vcl-mode" "Major mode for Varnish VCL sources" t)
;;  (add-to-list 'auto-mode-alist (cons (purecopy "\\.vcl\\'")  'vcl-mode))

;;; Code:

(require 'cl)
(require 'cc-langs)

(defvar vcl-mode-map ()
  "Keymap used in vcl-mode buffers.")
(if vcl-mode-map
    nil
  (setq vcl-mode-map (c-make-inherited-keymap))
  (define-key vcl-mode-map "\C-c%" 'vcl-match-paren))

(defvar vcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\r ">   " st)
    (modify-syntax-entry ?\f ">   " st)
;    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?~ "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?\" "." st)
    st)
  "Syntax table in use in VCL Mode buffers.")

(defvar vcl-mode-abbrev-table nil
  "Abbreviation table used in vcl-mode buffers.")
(c-define-abbrev-table 'vcl-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)))

;; Font locking
(defconst vcl-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Version declaration
     '("^[ \t]*\\(vcl\\)\\>[ \t]*\\([[:digit:]]+\\.[[:digit:]]+\\)"
       (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
     ;; Built-ins
     (cons
      (concat "\\<"
	      (regexp-opt
	       '("vcl_init"
		 "vcl_recv"
		 "vcl_pipe"
		 "vcl_pass"
		 "vcl_hash"
		 "vcl_hit"
		 "vcl_miss"
		 "vcl_fetch"
		 "vcl_deliver"
		 "vcl_error"
		 "vcl_fini"
		 "vcl_synth"
		 "vcl_backend_fetch"
                 "vcl_backend_response"
                 "vcl_backend_error") t)
	      "\\>")
	'font-lock-builtin-face)
     ;; Keywords
     (cons
      (concat "\\<"
	      (regexp-opt
	       '("sub"
		 "import"
		 "include"
		 "backend"))
	      "\\>")
      'font-lock-keyword-face)
     ))
  "Subdued level highlighting for VCL buffers.")

(defconst vcl-font-lock-keywords-2
  (append vcl-font-lock-keywords-1
	  (eval-when-compile
	    (list
	     ;; Keywords
	     (cons
	      (concat "\\<"
		      (regexp-opt
		       '("acl"
			 "if"
			 "else"
			 "return"
			 "call"
			 "set"
		         "remove"
			 "unset"
			 "director"
			 "probe") t)
		      "\\>")
	      'font-lock-keyword-face)
	     ;; Return values
	     (cons
	      (concat "\\<"
		      (regexp-opt
		       '("error"
			 "fetch"
		         "hash"
		         "hit_for_pass"
		         "lookup"
		         "ok"
		         "pass"
		         "pipe"
			 "deliver"
		         "restart"
	                 "true"
                         "false") t)
		      "\\>")
	      'font-lock-constant-face)
	     ;; Functions
	     (cons
	      (concat "\\<"
		      (regexp-opt
		       '("ban"
			 "call"
			 "hash_data"
			 "new"
		         "synth"
			 "synthetic"
			 "regsub"
			 "regsuball") t)
		      "\\>")
	      'font-lock-function-name-face)

	     ;; Objects and variables
	     ;; See https://www.varnish-cache.org/docs/4.0/reference/vcl.html#variables
	     (list (concat "\\<"
	     	      (regexp-opt
	     	       '("req"
	     		 "resp"
	     		 "bereq"
                         "beresp"
                         "obj") t)
		      "\\.\\(http\\)\\(\\.\\([a-zA-Z_-][a-zA-Z_0-9-]*\\)\\)?")
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face)
	       '(4 font-lock-string-face nil t))
	     (list (concat "\\<\\(bereq\\)\\."
			   (regexp-opt
			    '("backend"
			      "between_bytes_timeout"
			      "connect_timeout"
			      "first_byte_timeout"
			      "method"
			      "proto"
			      "retries"
			      "uncacheable"
			      "url"
			      "xid") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(beresp\\)\\.\\(backend\\)\\."
			   (regexp-opt
			    '("name"
			      "ip") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face)
	       '(3 font-lock-builtin-face))
	     (list (concat "\\<\\(beresp\\)\\."
			   (regexp-opt
			    '("do_esi"
			      "do_gunzip"
			      "do_gzip"
			      "do_stream"
			      "grace"
			      "keep"
			      "proto"
			      "reason"
			      "status"
			      "storage_hint"
			      "ttl"
			      "uncacheable") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(client\\)\\."
			   (regexp-opt
			    '("identity"
			      "ip") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(obj\\)\\."
			   (regexp-opt
			    '("grace"
			      "hits"
			      "keep"
			      "proto"
			      "reason"
			      "status"
			      "ttl"
			      "uncacheable") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(req\\)\\."
			   (regexp-opt
			    '("backend_hint"
			      "can_gzip"
			      "esi"
			      "esi_level"
			      "hash_always_miss"
			      "hash_ignore_busy"
			      "method"
			      "proto"
			      "restarts"
			      "ttl"
			      "url"
			      "xid") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(resp\\)\\."
			   (regexp-opt
			    '("proto"
			      "reason"
			      "status") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(server\\)\\."
			   (regexp-opt
			    '("hostname"
			      "identity"
			      "ip") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-builtin-face))
	     (list (concat "\\<\\(storage\\)\\.\\(\\sw+\\)\\."
			   (regexp-opt
			    '("free_space"
			      "used_space"
			      "happy") t))
	       '(1 font-lock-builtin-face)
	       '(2 font-lock-variahle-name-face)
	       '(3 font-lock-builtin-face))

	     (cons
	      (concat "\\<"
	     	      (regexp-opt
	     	       '("req"
	     		 "resp"
	     		 "bereq"
                         "beresp"
	     	         "client"
                         "server"
                         "obj"
			 "now") t)
	     	      "\\>")
	      'font-lock-builtin-face)

	     ;; Function calls
	     '("\\<\\(\\(\\sw+\\)\\.\\)*\\(\\sw+\\)[ \t]*("
	       (2 font-lock-variable-name-face nil t)
	       (3 font-lock-function-name-face))
	     ;; '("\\<\\(\\sw+\\)\\(\\.\\(\\sw+\\)\\)*[ \t]*("
	     ;;   (1 font-lock-function-name-face)
	     ;;   (3 font-lock-function-name-face nil t))

	     ;; Constants
	     '("\\<\\([[:digit:]]+\\(\\.[[:digit:]]+\\)?\\)[ \t]*\\(ms\\|[smhdwy]\\)?\\>"
	       (1 font-lock-constant-face) (3 font-lock-builtin-face nil t)))))
  "Medium level highlighting for VCL buffers.")

(defconst vcl-font-lock-keywords-3
  (append vcl-font-lock-keywords-2
	  (eval-when-compile
	    (list
	     ;; User function names.
	     '("^[ \t]*\\(sub\\)\\>[ \t]*\\(\\sw+\\)?"
	       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t)))))
  "Gaudy level highlighting for VCL buffers.")

(defvar vcl-font-lock-keywords vcl-font-lock-keywords-3)

(put 'vcl-mode 'c-mode-prefix "vcl-")

(defun vcl-sharp-comment-syntax ()
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((syntax (save-match-data (syntax-ppss))))
      (cond
       ((not (or (nth 3 syntax) (nth 4 syntax)))
	(put-text-property (match-beginning 1) (match-end 1)
			   'syntax-table (string-to-syntax "<"))
	(end-of-line)
	(put-text-property (point) (+ (point) 1)
			   'syntax-table (string-to-syntax ">")))))))

(defconst vcl-syntax-propertize-function
  (syntax-propertize-rules
   ("\\({\\)\""
    (1 "|"))
   ("\\({\\)[^\"]"
    (1 "(}"))
   ("\"\\(}\\)"
    (1 "|"))
   ("\\(^\\|[^\"]\\)\\(}\\)"
    (2 "){"))
   ("\\(\"\\)[^}]"
    (1 (let ((syntax (save-match-data (syntax-ppss))))
	 (string-to-syntax
	  (cond ((nth 4 syntax)
		 ".")
		((eq (nth 3 syntax) t)
		 (backward-char)
		 ".")
		(t
		 (backward-char)
		 "\""))))))
   ("\\(#\\)"
    (1 (ignore (vcl-sharp-comment-syntax))))
   ))

(defun vcl-match-paren (&optional arg)
  "If point is on a parenthesis (including VCL multi-line string delimiter),
find the matching one and move point to it.
With ARG, do it that many times.
"
 (interactive "p")
 (let ((n (or arg 1))
       (matcher (cond
		 ((looking-at "\\s\(")
		  (cons
		   (lexical-let ((s (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))
		     (lambda ()
		       (search-forward s)
		       (backward-char)))
		   (lambda ()
		     (forward-list)
		     (backward-char))))
		 ((looking-at "\\s\)")
		  (cons
		   (lexical-let ((s (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))
		     (lambda ()
		       (search-backward s)))
		   (lambda ()
		     (forward-char)
		     (backward-list))))
		 ((or (looking-at "{\"")
		      (save-excursion
			(backward-char)
			(looking-at "{\"")))
		  (cons
		   (lambda ()
		     (search-forward "{\""))
		   (lambda ()
		     (search-forward-regexp "\"}")
		     (backward-char))))
		 ((or (looking-at "\"}")
		      (save-excursion
			(backward-char)
			(looking-at "\"}")))
		  (cons
		   (lambda ()
		     (search-backward "\"}"))
		   (lambda ()
		     (search-backward-regexp "{\"")))))))
   (if (not matcher)
       (message "Point not at parenthesis")
     (condition-case err
	 (let ((fx (car matcher))
	       (fn (cdr matcher)))
	   (catch 'stop
	     (while t
	       (funcall fn)
	       (setq n (1- n))
	       (if (= n 0)
		   (throw 'stop t)
		 (condition-case e
		     (funcall fx)
		   (search-failed
		    (message "Not enough groups to satisfy the request")
		    (throw 'stop t)))))))

       (scan-error (goto-char (nth 2 err))
		   (message "%s" (nth 1 err)))
       (search-failed (message "Unbalanced %s" (cdr err)))))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.vcl\\'")  'vcl-mode))

;;;###autoload
(define-derived-mode vcl-mode prog-mode "VCL"
  "Major mode for editing VCL code.

Key bindings:
\\{vcl-mode-map}"

  (set (make-local-variable 'syntax-propertize-function)
       vcl-syntax-propertize-function)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (c-initialize-cc-mode t)
  (c-lang-setvar comment-start "# ")
  (setq c-opt-cpp-prefix nil)
  (set-syntax-table vcl-mode-syntax-table)
  (setq local-abbrev-table vcl-mode-abbrev-table
	abbrev-mode t)
  (use-local-map vcl-mode-map)
  (c-init-language-vars vcl-mode)
  (c-common-init 'vcl-mode)

  (c-run-mode-hooks 'c-mode-common-hook 'vcl-mode-hook)
  (c-update-modeline))

;;;; ChangeLog:

;; 2018-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* vcl-mode.el: Update header and fix last line; improve commentary
;; 
;; 2018-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add 'packages/vcl-mode/' from commit
;; 	'd6bba7c13e0d72936001f5adea155256151339ac'
;; 
;; 	git-subtree-dir: packages/vcl-mode git-subtree-mainline:
;; 	c0c44c3c0ded215e5bc60da74e2aaa090a35617b git-subtree-split:
;; 	d6bba7c13e0d72936001f5adea155256151339ac
;; 


(provide 'vcl-mode)
;;; vcl-mode.el ends here
