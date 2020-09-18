;;; verona-mode.el --- A major mode for the Verona programming language  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Damon Kwok

;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; Package-Version: 20200823.536
;; Package-Commit: 72dd31ef847344d79409503f3c42169041eb3da4
;; URL: https://github.com/damon-kwok/verona-mode
;; Keywords: languages programming
;; Package-Requires: ((emacs "25.1") (dash "2.17.0") (hydra "0.15.0"))

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Description:
;;
;; This is a major mode for the Verona programming language
;;
;; For more details, see the project page at
;; https://github.com/damon-kwok/verona-mode
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install verona-mode
;;
;; Or, copy verona-mode.el to some location in your Emacs load
;; path.  Then add "(require 'verona-mode)" to your Emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'verona-mode)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'js)
(require 'dash)
(require 'xref)
(require 'hydra)
(require 'imenu)
(require 'easymenu)

(defvar verona-mode-hook nil)

(defconst verona-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using verona-keywords

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table) table))

(defvar verona-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent) map)
  "Keymap for Verona major mode.")

(defconst verona-keywords
  '("module" "type" "class"                        ;
     "if" "else" "while" "for" "in" "match" "when" ;
     "where" "static" "private"                    ;
     "break" "continue" "return" "yield"           ;
     "new"                                         ;
     "var" "let")
  "Verona language keywords.")

(defconst verona-declaration-keywords   ;
  '("module" "type" "class")
  "Verona declaration keywords.")

(defconst verona-preprocessor-keywords '("imports")
  "Verona preprocessor keywords.")

(defconst verona-careful-keywords
  '("builtin" "create" "new" "where" "static" "private" ;
     "break" "continue" "return" "yield"                ;
     "cown")
  "Verona language careful keywords.")

(defconst verona-builtin-keywords '("imm" "mut" "iso")
  "Verona language keywords.")

(defconst verona-constants              ;
  '("false" "true")
  "Common constants.")

(defconst verona-operator-functions
  '("add" "sub" "mul" "div" "mod" "shl" "shr" ;
     "lt" "gt" "le" "ge" "eq" "ne" "and" "or")
  "Verona language operators functions.")

;; create the regex string for each class of keywords

(defconst verona-keywords-regexp (regexp-opt verona-keywords 'words)
  "Regular expression for matching keywords.")

(defconst verona-declaration-keywords-regexp
                                        ;
  (regexp-opt verona-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst verona-preprocessor-keywords-regexp
                                        ;
  (regexp-opt verona-preprocessor-keywords 'words)
  "Regular expression for matching preprocessor keywords.")

(defconst verona-careful-keywords-regexp
                                        ;
  (regexp-opt verona-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst verona-builtin-keywords-regexp
  (regexp-opt verona-builtin-keywords 'words)
  "Regular expression for matching builtin type.")

(defconst verona-constant-regexp        ;
  (regexp-opt verona-constants 'words)
  "Regular expression for matching constants.")

(defconst verona-operator-functions-regexp
                                        ;
  (regexp-opt verona-operator-functions 'words)
  "Regular expression for matching operator functions.")

(defconst verona-font-lock-keywords
  `(
     ;; builtin
     (,verona-builtin-keywords-regexp . font-lock-builtin-face)

     ;; careful
     (,verona-careful-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,verona-declaration-keywords-regexp . font-lock-keyword-face)

     ;; preprocessor
     (,verona-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|:>\\|:=\\|\\.\\.\\||\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\($?[.,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\($?[+-/*//%~=<>]+\\)$?,?" 1 'font-lock-negation-char-face)
     ("\\($?[?^!&]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: = : separate
     ("[^+-/*//%~^!=<>]\\([=:]\\)[^+-/*//%~^!=<>]" 1
       'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face)

     ;; operator methods
     (,verona-operator-functions-regexp . font-lock-builtin-face)

     ;; macro
     ("#\\(?:include\\|flag\\)" . 'font-lock-builtin-face)

     ;; type
     ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; method
     ("\\(?:builtin\s+\\)*\\([a-z_]$?[a-z0-9_]?+\\)[ \t]*\\(.*\\)(" 1
       'font-lock-function-name-face)

     ;; constants references
     (,verona-constant-regexp . font-lock-constant-face)

     ;; @
     ("@[A-Za-z_]*[A-Z-a-z0-9_]*" . 'font-lock-builtin-face)

     ;; parameter
     ("\\(?:(\\|,\\)\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1
       'font-lock-variable-name-face)
     ("\\(?:(\\|,\\)[ \t]+\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1
       'font-lock-variable-name-face)

     ;; tuple references
     ("[.]$?[ \t]?\\($?_[1-9]$?[0-9]?*\\)" 1 'font-lock-variable-name-face)

     ;; keywords
     (,verona-keywords-regexp . font-lock-keyword-face) ;;font-lock-keyword-face

     ;; character literals
     ("\\('[\\].'\\)" 1 'font-lock-constant-face)

     ;; numeric literals
     ("[ \t/+-/*//=><([,;]\\([0-9]+[0-9a-zA-Z_]*\\)+" 1
       'font-lock-constant-face)

     ;; variable references
     ("\\([a-z_]+[a-z0-9_']*\\)+" 1 'font-lock-variable-name-face))
  "An alist mapping regexes to font-lock faces.")

(defun verona-project-root-p (path)
  "Return t if directory `PATH' is the root of the Verona project."
  (let* ((files '("CMakeLists.txt" "make.bat" "Makefile"     ;
                   "Dockerfile" ".editorconfig" ".gitignore" ;
                   ".git" ".svn" ".hg" ".bzr"))
          (foundp nil))
    (while (and (> (length files) 0)
             (not foundp))
      (let* ((filename (car files))
              (filepath (concat (file-name-as-directory path) filename)))
        (setq files (cdr files))
        (setq foundp (file-exists-p filepath)))) ;
    foundp))

(defun verona-project-root
  (&optional
    path)
  "Return the root of the Verona project.
Optional argument PATH: project path."
  (let* ((bufdir (if buffer-file-name   ;
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if path (file-name-as-directory path) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent (file-name-as-directory (getenv "HOME")))
          (string= parent "/")
          (verona-project-root-p curdir)) ;
      curdir                              ;
      (verona-project-root parent))))

(defun verona-project-name ()
  "Return Verona project name."
  (file-name-base (directory-file-name (verona-project-root))))

(defun verona-project-file-exists-p (filename)
  "Return t if file `FILENAME' exists."
  (file-exists-p (concat (verona-project-root) filename)))

(defun verona-run-command (command &optional path)
  "Return `COMMAND' in the root of the Verona project.
Optional argument PATH: project path."
  (let ((oldir default-directory))
    (setq default-directory (if path path (verona-project-root path)))
    (compile command)
    (setq default-directory oldir)))

(defun verona-project-build ()
  "Build project with veronac."
  (interactive)
  (if (verona-project-file-exists-p "Makefile")
    (verona-run-command "make")
    (verona-run-command "veronac .")))

(defun verona-project-open ()
  "Open `Makefile' file."
  (interactive)
  (if (verona-project-file-exists-p "Makefile")
    (find-file (concat (verona-project-root) "Makefile"))))

(defun verona-buffer-dirname ()
  "Return current buffer directory file name."
  (directory-file-name (if buffer-file-name (file-name-directory
                                              buffer-file-name)
                         default-directory)))

(defun verona-project-run ()
  "Run project."
  (interactive)
  (let* ((bin1 (concat (verona-project-root) "bin/" (verona-project-name)))
          (bin2 (concat (verona-project-root) "/" (verona-project-name)))
          (bin3 (concat (verona-buffer-dirname) "/" (verona-project-name))))
    (cond ((file-exists-p bin1)
            (verona-run-command bin1))
      ((file-exists-p bin2)
        (verona-run-command bin2))
      ((file-exists-p bin2)
        (verona-run-command bin3)))))

(easy-menu-define verona-mode-menu verona-mode-map ;
  "Menu for Verona mode."                          ;
  '("Verona"                                       ;
     ["Build" verona-project-build t]              ;
     ["Run" verona-project-run t]                  ;
     "---"                                         ;
     ("Community"                                  ;
       ["Home" (verona-run-command
                 "xdg-open https://github.com/microsoft/verona") t]
       ["Contribute" ;;
         (verona-run-command
           "xdg-open https://github.com/microsoft/verona/blob/master/CONTRIBUTING.md") t])))

(defun verona-banner-default ()
  "Verona banner."
  "
  __   _____ _ __ ___  _ __   __ _
  \\ \\ / / _ \\ '__/ _ \\| '_ \\ / _` |
   \\ V /  __/ | | (_) | | | | (_| |
    \\_/ \\___|_|  \\___/|_| |_|\\__,_|
")

(defhydra verona-hydra-menu
  (:color blue
    :hint none)
  "
%s(verona-banner-default)
  Project     |  _b_: Build     _r_: Run
  _q_: Quit"                            ;
  ("b" verona-project-build "Build")
  ("r" verona-project-run "Run")
  ("q" nil "Quit"))

(defun verona-menu ()
  "Open Verona hydra menu."
  (interactive)
  (verona-hydra-menu/body))

(defun verona-build-tags ()
  "Build tags for current project."
  (interactive)
  (let ((tags-buffer (get-buffer "TAGS"))
         (tags-buffer2 (get-buffer (format "TAGS<%s>" (verona-project-name)))))
    (if tags-buffer (kill-buffer tags-buffer))
    (if tags-buffer2 (kill-buffer tags-buffer2)))
  (let* ((verona-path (string-trim (shell-command-to-string "which veronac")))
          (verona-executable (string-trim (shell-command-to-string (concat
                                                                     "readlink -f "
                                                                     verona-path))))
          (packages-path (concat (file-name-directory verona-executable)
                           "../stdlib"))
          (ctags-params                 ;
            (concat
              "ctags --languages=-verona --langdef=verona --langmap=verona:.verona "
              "--regex-verona=/[ \\t]*builtin[ \\t]+([a-zA-Z0-9_]+)/\\1/m,method/ "
              "--regex-verona=/[ \\t]*create[ \\t]+([a-zA-Z0-9_]+)/\\1/n,constructor/ "
              "--regex-verona=/^[ \\t]*class[ \\t]+([a-zA-Z0-9_]+)/\\1/c,class/ " ;
              "-e -R . " packages-path)))
    (when (file-exists-p packages-path)
      (let ((oldir default-directory))
        (setq default-directory (verona-project-root))
        (message "ctags:%s" (shell-command-to-string ctags-params))
        (verona-load-tags)
        (setq default-directory oldir)))))

(defun verona-load-tags
  (&optional
    build)
  "Visit tags table.
Optional argument BUILD If the tags file does not exist, execute the build."
  (interactive)
  (let* ((tags-file (concat (verona-project-root) "TAGS")))
    (if (file-exists-p tags-file)
      (progn (visit-tags-table (concat (verona-project-root) "TAGS")))
      (if build (verona-build-tags)))))

(defun verona-after-save-hook ()
  "After save hook."
  (when (eq major-mode 'verona-mode)
    (shell-command (concat  "veronac fmt " (buffer-file-name)))
    (revert-buffer
      :ignore-auto
      :noconfirm)
    (if (not (executable-find "ctags"))
      (message "Could not locate executable '%s'" "ctags")
      (verona-build-tags))))

;;;###autoload
(define-derived-mode verona-mode prog-mode
  "Verona"
  "Major mode for editing Verona files."
  :syntax-table verona-mode-syntax-table
  ;; (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  ;;
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  ;;
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local indent-line-function #'js-indent-line)
  (setq-local js-indent-level tab-width)
  ;;
  ;; (setq-local font-lock-defaults        ;
  ;; '(verona-font-lock-keywords ;
  ;; nil nil nil nil         ;
  ;; (font-lock-syntactic-face-function . verona-mode-syntactic-face-function)))
  (setq-local font-lock-defaults '(verona-font-lock-keywords))
  (font-lock-ensure)
  ;;
  ;; (setq-local syntax-propertize-function verona-syntax-propertize-function)
  ;;
  (setq-local imenu-generic-expression ;;
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("method" "[ \t]*builtin[ \t]+\\([a-z0-9_]+\\)[ \t]*" 1)
       ("constructor" "[ \t]+\\(create\\)[ \t]*" 1)
       ("class" "^[ \t]*class[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("module" "[ \t]*module[ \t]+\\([a-zA-Z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Index")
  ;;
  (add-hook 'after-save-hook #'verona-after-save-hook nil t)
  (verona-load-tags))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verona\\'" . verona-mode))

;;
(provide 'verona-mode)

;;; verona-mode.el ends here
