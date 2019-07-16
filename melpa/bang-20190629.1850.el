;;; bang.el --- A more intelligent shell-command -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.1.0
;; Package-Version: 20190629.1850
;; Keywords: unix, processes, convenience
;; Package-Requires: ((emacs "24.1") (seq "2.20"))
;; URL: https://git.sr.ht/~zge/bang

;; This file is NOT part of Emacs.
;; 
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; Bang is a interactive `shell-command' substitute, that extends the
;; regular Emacs function by considering the first character as special.
;; Read `bang's docstring for more details.
;;
;; This version of Bang has been based on Leah Neukirchen's version, and
;; has been (messily) extended to handle history commands (!32 for the
;; 32'nd last command, !xterm for the last command starting with xterm).
;; Most of the internal code of `bang' has been rewritten for this sake.
;;
;; The original version can be found here:
;; https://leahneukirchen.org/dotfiles/.emacs

(require 'rx)
(require 'seq)

;;; Code:

(defconst bang--command-regexp
  (rx bos (* space)
      (or (seq (group "!" )
               (or (group (+ digit))
                   (group (+ alnum))))
          (group "<") (group ">") (group "|") "")
      (? (* space)
         (group (* not-newline)
                (not space)
                (*? not-newline)))
      eos)
  "Regular expression to parse input to `bang'.")

(defvar bang--last-commands '()
  "List of non-history commands last executed by `bang'.")

(defvar bang-history-size 80
  "Number of commands to save in `bang--last-commands'.")

(defun bang--remember-command (command)
  "Helper function to save COMMAND in bang's history."
  (push command bang--last-commands)
  (let ((overflow (- (length bang--last-commands)
                     bang-history-size)))
    (when (> overflow 0)
      (setq bang--last-commands
            (nbutlast bang--last-commands overflow)))))

(defun bang--find-last-command (prefix)
  "Helper function to find last command that started with PREFIX."
  (or (seq-find (apply-partially #'string-prefix-p prefix)
                bang--last-commands)
      (error "No such command in history")))

(defun bang--get-command-number (arg rest)
  "Helper function to find ARG'th last command.

Second argument REST will be concatenated to what was found."
  (let* ((num (string-to-number arg))
         (pos (- (length bang--last-commands)
                 (1- num)))
         (cmd (nth pos bang--last-commands)))
    (concat cmd " " rest)))

(defun bang-history ()
  "Display a buffer with overview of previous bang commands."
  (interactive)
  (let ((buf (get-buffer-create "*bang-history*"))
        (i (length bang--last-commands)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (delete-region (goto-char (point-min))
                     (point-max))
      (dolist (cmd bang--last-commands)
        (insert (format "%d\t%s\n" i cmd))
        (setq i (1- i)))
      (special-mode))
    (pop-to-buffer buf)))

(defun bang (command beg end)
  "Intelligently execute string COMMAND in inferior shell.

When COMMAND starts with
  <  the output of COMMAND replaces the current selection
  >  COMMAND is run with the current selection as input
  |  the current selection is filtered through COMMAND
  !  executes the last command that started with COMMAND,
     or if a number, re-execute nth last command

Without any argument, `bang' will behave like `shell-command'.

Inside COMMAND, % is replaced with the current file name. To
insert a literal % quote it using a backslash.

In case a region is active, bang will only work with the region
between BEG and END. Otherwise the whole buffer is processed."
  (interactive (list (read-shell-command "Bang command: ")
                     (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))
  (save-match-data
    (unless (string-match bang--command-regexp command)
      (error "Invalid command"))
    (let ((has-! (match-string-no-properties 1 command))
          (num-! (match-string-no-properties 2 command))
          (arg-! (match-string-no-properties 3 command))
          (has-< (match-string-no-properties 4 command))
          (has-> (match-string-no-properties 5 command))
          (has-| (match-string-no-properties 6 command))
          (rest (condition-case nil
                    (replace-regexp-in-string
                     (rx (* ?\\ ?\\) (or ?\\ (group "%")))
                     buffer-file-name
                     (match-string-no-properties 7 command)
                     nil nil 1)
                  (error (match-string-no-properties 7 command)))))
      (cond (arg-! (bang (bang--find-last-command arg-!)
                         beg end))
            (num-! (bang (bang--get-command-number num-! rest)
                         beg end))
            (has-< (delete-region beg end)
                   (shell-command rest t shell-command-default-error-buffer)
                   (exchange-point-and-mark))
            (has-> (shell-command-on-region
                    beg end rest nil nil
                    shell-command-default-error-buffer t))
            (has-| (shell-command-on-region
                    beg end rest t t
                    shell-command-default-error-buffer t))
            (t (shell-command command nil shell-command-default-error-buffer)))
      (when (or has-! has->)
        (with-current-buffer "*Shell Command Output*"
          (delete-region (point-min) (point-max))))
      (unless (or num-! arg-!)
        (bang--remember-command command)))))

(provide 'bang)

;;; bang.el ends here
