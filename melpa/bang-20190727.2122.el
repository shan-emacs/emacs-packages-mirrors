;;; bang.el --- A more intelligent shell-command -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.2.0
;; Package-Version: 20190727.2122
;; Keywords: unix, processes, convenience
;; Package-Requires: ((emacs "24.1"))
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

;;; Code:

(defun bang (command beg end)
  "Intelligently execute string COMMAND in inferior shell.

When COMMAND starts with
  <  the output of COMMAND replaces the current selection
  >  COMMAND is run with the current selection as input
  |  the current selection is filtered through COMMAND

Without any argument, `bang' will behave like `shell-command'.

Inside COMMAND, % is replaced with the current file name. To
insert a literal % quote it using a backslash.

In case a region is active, bang will only work with the region
between BEG and END. Otherwise the whole buffer is processed."
  (interactive (list (read-shell-command "Bang command: ")
                     (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))
  (save-match-data
    (unless (string-match (rx bos (* space)
                              (or (group "<") (group ">") (group "|") "")
                              (group (* not-newline))
                              eos)
                          command)
      (error "Invalid command"))
    (let ((has-< (match-string-no-properties 1 command))
          (has-> (match-string-no-properties 2 command))
          (has-| (match-string-no-properties 3 command))
          (rest (condition-case nil
                    (replace-regexp-in-string
                     (rx (* ?\\ ?\\) (or ?\\ (group "%")))
                     buffer-file-name
                     (match-string-no-properties 4 command)
                     nil nil 1)
                  (error (match-string-no-properties 4 command)))))
      (cond (has-< (delete-region beg end)
                   (shell-command rest t shell-command-default-error-buffer)
                   (exchange-point-and-mark))
            (has-> (shell-command-on-region
                    beg end rest nil nil
                    shell-command-default-error-buffer t))
            (has-| (shell-command-on-region
                    beg end rest t t
                    shell-command-default-error-buffer t))
            (t (shell-command command (if current-prefix-arg t nil)
                              shell-command-default-error-buffer)))
      (when has->
        (with-current-buffer "*Shell Command Output*"
          (delete-region (point-min) (point-max)))))))

(provide 'bang)

;;; bang.el ends here
