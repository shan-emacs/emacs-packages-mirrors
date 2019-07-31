;;; cpp-capf.el --- Completion-at-point backend for c/c++ using clang -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 1.0.0
;; Package-Version: 20190723.1158
;; Keywords: c, abbrev, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://git.sr.ht/~zge/cpp-capf

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; Emacs built-in `completion-at-point' completion mechanism doesn't
;; support C in any meaningful by default, which this package tries to
;; remedy, by using clang's completion mechanism. Hence this package
;; requires clang to be installed (as specified in `cpp-capf-clang'.
;;
;; If a header file is not automatically found or in the default path,
;; extending `cpp-capf-include-paths' or `cpp-capf-extra-flags' might
;; help.
;;
;; `cpp-capf' is based on/inspired by:
;; - https://opensource.apple.com/source/lldb/lldb-167.2/llvm/tools/clang/utils/clang-completion-mode.el.auto.html
;; - https://github.com/company-mode/company-mode/blob/master/company-clang.el
;; - https://github.com/brianjcj/auto-complete-clang/blob/master/auto-complete-clang.el
;; - https://www.reddit.com/r/vim/comments/2wf3cn/basic_clang_autocompletion_query/
;; - https://foicica.com/wiki/cpp-clang-completion

;;; Code:

(defgroup cpp-capf nil
  "Completion back-end for C using clang."
  :group 'completion
  :prefix "cpp-capf-")

(defcustom cpp-capf-include-paths
  '("/usr/local/include"
    "/usr/lib/llvm-7/lib/clang/7.0.1/include"
    "/usr/include/x86_64-linux-gnu"
    "/usr/include" "." ".." "../..")
  "Paths to directories with header files."
  :type 'list)

(defcustom cpp-capf-special-chars
  '(?\. ?, ?\t ?\n ?\ ?\; ?\( ?\) ?\[ ?\] ?\{ ?\} ?\n ?\t ? ?\" ?\')
  "List of characters that wrap a symbol"
  :type '(list character))

(defcustom cpp-capf-extra-flags nil
  "Additional flags to call clang with."
  :type '(list string))

(defcustom cpp-capf-clang "clang++"
  "Path to clang binary."
  :type 'file)

(defcustom cpp-capf-ignore-case nil
  "Should completion ignore case."
  :type 'boolean)

(defcustom cpp-capf-show-type t
  "Should completion show types."
  :type 'boolean)


(defun cpp-capf--parse-output ()
  (when cpp-capf-show-type
    (save-excursion
      (let ((re "<#\\|#>\\|\\[#\\|#]\\(?:[[:word:]]\\|_\\)+"))
        (while (re-search-forward re nil t)
          (replace-match ""))))
    (save-excursion
      (save-match-data
        (let ((re "[[:space:]]?\\(?:[[:word:]]\\|_\\)*\\([),]\\)"))
          (while (re-search-forward re nil t)
            (replace-match (match-string 1)))))))
  (let (result)
    (while (search-forward-regexp
            "^COMPLETION: \\(.+\\) : \\(.+\\)$"
            nil t)
      (let ((symb (match-string 1)))
        (put-text-property 0 1
                           'sig
                           (match-string 2)
                           symb)
        (push symb result)))
    result))

(defun cpp-capf--completions (&rest _ignore)
  "Call clang to collect suggestions at point."
  (let* ((temp (generate-new-buffer " *clang*")))
    (prog2
        (apply
         #'call-process-region
         (append (list (point-min) (point-max)
                       cpp-capf-clang nil temp nil
                       "-cc1" "-fsyntax-only"
                       "-code-completion-macros")
                 (mapcar (apply-partially #'concat "-I")
                         cpp-capf-include-paths)
                 cpp-capf-extra-flags
                 (list (format
                        "-code-completion-at=-:%d:%d"
                        (line-number-at-pos)
                        (1+ (length (encode-coding-region
                                     (line-beginning-position)
                                     (point) 'utf-8 t))))
                       "-")))
        (with-current-buffer temp
          (goto-char (point-min))
          (cpp-capf--parse-output))
      (kill-buffer temp))))

(defun cpp-capf--annotate (str)
  "Extract type of completed symbol from STR as annotation."
  (let ((sig (get-text-property 0 'sig str)))
    (when sig (concat " : " sig))))

;;;###autoload
(defun cpp-capf ()
  "Function used for `completion-at-point-functions' using clang."
  (unless cpp-capf-clang
    (error "Company either not installed or not in path"))
  (list (save-excursion
          (unless (memq (char-before) cpp-capf-special-chars)
            (backward-sexp))
          (point))
        (save-excursion
          (unless (memq (char-after) cpp-capf-special-chars)
            (forward-sexp))
          (point))
        (completion-table-with-cache #'cpp-capf--completions
                                     cpp-capf-ignore-case)
        :annotation-function (and cpp-capf-show-type
                                  #'cpp-capf--annotate)
        :exclusive 'no))

(provide 'cpp-capf)

;;; cpp-capf.el ends here
