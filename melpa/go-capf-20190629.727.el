;;; go-capf.el --- Completion-at-point backend for go -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 1.0.0
;; Package-Version: 20190629.727
;; Keywords: languages, abbrev, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://git.sr.ht/~zge/go-capf

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; Emacs built-in `completion-at-point' completion mechanism has no
;; support for go by default. This package helps solve the problem by
;; with a custom `completion-at-point' function, that should be added to
;; `completion-at-point-functions' as so:
;;
;;   (add-to-list 'completion-at-point-functions #'go-completion-at-point-function)
;;
;; Note that this requires gocode (https://github.com/mdempsky/gocode)
;; to be installed on your system, that's compatible with the version of
;; go you are using.

;;; Code:

(defgroup go-capf nil
  "Completion back-end for Go."
  :group 'completion
  :prefix "go-capf-")

(defcustom go-capf-gocode "gocode"
  "Path to gocode binary."
  :type 'file)

(defcustom go-capf-special-chars
  '(?\. ?, ?\t ?\n ?\ ?\; ?\( ?\) ?\[ ?\] ?\{ ?\} ?\n ?\t ? ?\" ?\')
  "List of characters that wrap a symbol"
  :type '(list character))

(defcustom go-capf-gocode-flags '("-builtin")
  "Additional flags to pass to gocode."
  :type '(list string))

(defcustom go-capf-ignore-case nil
  "Should completion ignore case."
  :type 'boolean)

(defcustom go-capf-show-type t
  "Should completion show types."
  :type 'boolean)

(defun go-capf--clean-up-gocode ()
  "Hook to clean up gocode daemon when Emacs closes."
  (when (file-exists-p (expand-file-name
                        (concat "gocode-daemon." (or (getenv "USER") "all"))
                        temporary-file-directory))
    (ignore-errors (call-process "gocode" nil nil nil "close"))))

(defun go-capf--parse-csv ()
  "Collect function names from gocode -f=csv output."
  (let (completions)
    (while (not (eobp))
      (beginning-of-line)
      (let* ((type (buffer-substring
                    (point)
                    (- (search-forward ",,") 2)))
             (name (buffer-substring
                    (point)
                    (- (search-forward ",,") 2)))
             (sig (buffer-substring
                   (point)
                   (- (search-forward ",,") 2))))
        (put-text-property 0 (length name) 'go-capf-sig
                           sig name)
        (put-text-property 0 (length name) 'go-capf-type
                           type name)
        (push name completions))
      (forward-line))
    completions))

(defun go-capf--completions (&rest _ignore)
  "Collect list of completions at point."
  (let* ((temp (generate-new-buffer " *gocode*")))
    (prog2
        (apply #'call-process-region
               (append (list (point-min) (point-max)
                             go-capf-gocode
                             nil temp nil)
                       go-capf-gocode-flags
                       (list "-f=csv" "autocomplete"
                             (or (buffer-file-name) "")
                             (format "c%d" (- (point) 1)))))
        (with-current-buffer temp
          (goto-char (point-min))
          (go-capf--parse-csv))
      (kill-buffer temp))))

(defun go-capf--annotate (str)
  "Extract type of completed symbol from STR as annotation."
  (let ((sig (get-text-property 0 'go-capf-sig str)))
    (when sig (concat "\t : " sig))))

;;;###autoload
(defun go-capf ()
  "Return possible completions for go code at point."
  (unless go-capf-gocode
    (error "Binary \"gocode\" either not installed or not in path"))
  (unless (memq #'go-capf--clean-up-gocode kill-emacs-hook)
    (add-hook 'kill-emacs-hook #'go-capf--clean-up-gocode))
  (list (save-excursion
          (unless (memq (char-before) go-capf-special-chars)
            (backward-sexp))
          (point))
        (save-excursion
          (unless (memq (char-after) go-capf-special-chars)
            (forward-sexp))
          (point))
        (completion-table-with-cache #'go-capf--completions
                                     go-capf-ignore-case)
        :annotation-function (and go-capf-show-type
                                  #'go-capf--annotate)
        :exclusive 'no))

(provide 'go-capf)

;;; go-capf.el ends here
