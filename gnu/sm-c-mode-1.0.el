;;; sm-c-mode.el --- C major mode based on SMIE  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.0

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

;; This started as an experiment to see concretely where&how SMIE falls down
;; when trying to handle a language like C, to get an idea of maybe what it
;; would take to change SMIE to better support C-style syntax.
;;
;; So, this does provide "SMIE-based indentation for C" and might even do it OK
;; in practice, but it really doesn't benefit much from SMIE:
;; - it does a lot of its own parsing by hand.
;; - its smie-rules-function also does a lot of indentation by hand.
;; Hopefully at some point, someone will find a way to extend SMIE such that
;; we can handle C without having to constantly work around SMIE, e.g.
;; it'd be nice to hook sm-c--while-to-do, sm-c--else-to-if, sm-c--boi,
;; sm-c--boe, ... into SMIE at some level.

;; This is not designed to supplant Emacs's built-in c-mode, which does a more
;; thorough job.  It was not even meant to be used by anyone, really, but
;; I finally decided to release this because some users pointed out that on
;; slow machines it can be a worthy lightweigth alternative.

;;;; Known limitations:

;; - This mode makes no attempt to try and handle sanely K&R style function
;;   definitions (i.e. where the type of arguments is given between the list of
;;   arguments and the body).  There are 2 good reasons for that: this old
;;   syntax sucks and should be laid to rest, and it'd be a lot of extra work
;;   to try and handle it.

;;;; Todo:

;; - This mode mostly limits itself to the C99 syntax, so it would be nice
;;   to make it handle the syntactic constructs introduced since then.
;; - We "use but don't use" SMIE.
;; - CPP directives are treated as comments.  To some extent this is OK, but in
;;   many other cases it isn't.  See for instance the comment-only-p advice.
;; - M-q in a comment doesn't do the right thing.


;;;; Benchmarks

;; This code can't be compared to CC-mode since its scope is much more limited
;; (only tries to handle the kind of code found in Emacs's source code, for
;; example; does not intend to be extensible to handle C++ or ObjC; does not
;; offer the same kind of customizability of indentation style, ...).
;; But in order to make sure it's doing a good enough job on the code for which
;; it was tuned, I did run some quick benchmarks against CC-mode:
;;
;; Benchmarks: reindent emacs/src/*.[ch] (skipping macuvs.h and globals.h
;; because CC-mode gets pathologically slow on them).
;;    (cd .../emacs/; git reset --hard; mv src/macuvs.h src/globals.h ./);
;;    files=($(echo .../emacs/src/*.[ch]));
;;    (cd .../emacs/; mv macuvs.h globals.h src/);
;;    time make -j4 ${^${files}}.reindent EMACS="emacs24 -Q";
;;    (cd .../emacs/; git diff|wc)
;; - Default settings:
;;   diff|wc =>  86800  379362 2879534
;;   make -j4  191.57s user 1.77s system 334% cpu   57.78 total
;; - With (setq sm-c-indent-cpp-basic 0)
;;   diff|wc =>  59909  275415 2034045
;;   make -j4  177.88s user 1.70s system 340% cpu   52.80 total
;; - For reference, CC-mode gets:
;;   diff|wc =>  79164  490894 3428542
;;   make -j4  804.83s user 2.79s system 277% cpu 4:51.08 total
;;
;; IOW, in this "best case" scenario, `sm-c-mode' indented almost as well
;; as `c-mode' does (as measured by diff|wc), and it did it about
;; 4 times faster.
;; BEWARE: take this with a large grain of salt, since this is testing
;; `sm-c-mode' in the most favorable light (IOW it's a very strongly biased
;; benchmark).
;; All this says, is that sm-c-mode's indentation might actually be usable if
;; you use it on C code that is sufficiently similar to Emacs's.

;;; Code:

(require 'cl-lib)
(require 'smie)

(defgroup sm-c-mode nil
  "Major mode to edit C code, based on SMIE."
  :group 'programming)

(defcustom sm-c-indent-basic 2
  "Basic step of indentation.
Typically 2 for GNU style and `tab-width' for Linux style."
  :type 'integer)

(defcustom sm-c-indent-braces t
  "If nil, braces in if/while/... are aligned with the if/while/...
Else, they're indented by `sm-c-indent-basic' columns.
For braces placed at the end of lines (which SMIE calls \"hanging\"), it makes
no difference."
  :type 'boolean)

;;; Handling CPP directives.

(defsubst sm-c--cpp-inside-p (ppss)
  (eq 2 (nth 7 ppss)))

(eval-and-compile
  (defconst sm-c--cpp-regexp "^[ \t]*\\(\\(#\\)[ \t]*\\([a-z]+\\)\\)"))

(defconst sm-c--cpp-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defun sm-c--cpp-goto-end (ppss &optional limit)
  (cl-assert (sm-c--cpp-inside-p ppss))
  (let (found)
    (while
        (and (setq found (re-search-forward "\\(?:\\\\\\\\\\)*\n" limit 'move))
             ;; We could also check (nth 5 ppss) to figure out if we're
             ;; after a backslash, but this is a very common case, so it's good
             ;; to avoid calling parse-partial-sexp for that.
             (or (eq ?\\ (char-before (match-beginning 0)))
                 (with-syntax-table sm-c--cpp-syntax-table
                   (nth 4 (parse-partial-sexp (1+ (nth 8 ppss)) (point)))))))
    found))

(defun sm-c--cpp-fontify-syntactically (ppss)
  ;; FIXME: ¡¡BIG UGLY HACK!!
  ;; Copied from font-lock.el's font-lock-fontify-syntactically-region.
  (cl-assert (> (point) (nth 8 ppss)))
  (save-excursion
    (save-restriction
      (sm-c--cpp-goto-end ppss)
      (narrow-to-region (1+ (nth 8 ppss)) (point))
      ;; FIXME: We should add some "with-local-syntax-ppss" macro to
      ;; encapsulate this.
      (let ((syntax-propertize-function nil)
            (syntax-ppss-cache nil)
            (syntax-ppss-last nil))
        (font-lock-fontify-syntactically-region (point-min) (point-max))))))

(defun sm-c--cpp-syntax-propertize (end)
  (let ((ppss (syntax-ppss))
        found)
    (when (sm-c--cpp-inside-p ppss)
      (while
          (and (setq found (re-search-forward "\\(\\\\\\\\\\)*\n" end 'move))
               (or (eq ?\\ (char-before (match-beginning 0)))
                   (with-syntax-table sm-c--cpp-syntax-table
                     (nth 4 (parse-partial-sexp (1+ (nth 8 ppss)) (point)))))))
      (when found
        (let* ((ppss-in
                (save-excursion
                  (parse-partial-sexp (1+ (nth 8 ppss)) (1- (point)))))
               ;; Put the end before a closing //...\n comment so as to avoid
               ;; a bug in back_comment.  The problem is that back_comment
               ;; otherwise will see "// <...> <...> \n" and will consider the
               ;; CPP pseudo-comments as nested within the //...\n comment.
               (end (if (and (nth 4 ppss-in)        ;Inside a comment.
                             (null (nth 7 ppss-in)) ;A style `a' comment.
                             (memq (char-before (nth 8 ppss-in)) '(?\s ?\t)))
                        (nth 8 ppss-in)
                      (point))))
          (put-text-property (1- end) end
                             'syntax-table (string-to-syntax "> c")))))))

;;;; Indenting CPP directives.

(defcustom sm-c-indent-cpp-basic 1
  "Indent step for CPP directives.
If non-zero, CPP directives are indented according to CPP depth.
E.g. a #define nested within 2 #ifs will be turned into \"#  define\"."
  :type 'integer)

(defun sm-c--cpp-prev (tok)
  (let ((offset nil))
    (while
        (when (re-search-backward sm-c--cpp-regexp nil t)
          (pcase (cons tok (match-string 3))
            (`(,_ . "endif") (sm-c--cpp-prev "endif"))
            ((or `(,(or "endif" "else" "elif") . ,(or "if" "ifdef" "ifndef"))
                 `(,(or "else" "elif") . "elif"))
             (setq offset 0))
            (`(,(or "endif" "else" "elif") . ,_) nil)
            (`(,_ . ,(or "if" "ifdef" "ifndef" "elif" "else"))
             (setq offset sm-c-indent-cpp-basic))
            (_ (setq offset 0)))
          (not offset)))
    (when offset
      (goto-char (match-beginning 3))
      (+ offset (current-column)))))


(defun sm-c--cpp-indent-line (&optional _arg)
  ;; FIXME: Also align the terminating \, if any.
  (when (> sm-c-indent-cpp-basic 0)
    (let* ((pos (point-marker))
           (beg)
           (indent
            (save-excursion
              (forward-line 0)
              (when (looking-at sm-c--cpp-regexp)
                (setq beg (match-beginning 3))
                (or (sm-c--cpp-prev (match-string 3)) 0)))))
      (when indent
        (let ((before (<= pos beg)))
          (goto-char beg)
          (unless (= (current-column) indent)
            (skip-chars-backward " \t")
            (delete-region (point)
                           (progn (skip-chars-forward " \t") (point)))
            (indent-to indent))
          (unless before (goto-char pos)))))))

;;;; Indenting inside CPP #define.

(defconst sm-c--cpp-smie-indent-functions
  ;; FIXME: Don't just align line after #define with the "d"!
  (mapcar
   (lambda (f)
     (cond
      ((eq f #'smie-indent-comment-inside) #'sm-c--cpp-indent-comment-inside)
      ;; ((eq f #'smie-indent-exps) #'sm-c--cpp-indent-exps)
      (t f)))
   (default-value 'smie-indent-functions)))

(defun sm-c--cpp-indent-comment-inside ()
  (let ((ppss (syntax-ppss)))
    (when (nth 4 ppss)
      ;; Indicate where's the comment start.
      `(noindent . ,(nth 8 ppss)))))

(defun sm-c--cpp-smie-indent ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((sm-c--cpp-inside-p ppss)
      (save-restriction
        (narrow-to-region (nth 8 ppss) (point-max))
        (let ((indent
               (let ((smie-indent-functions sm-c--cpp-smie-indent-functions)
                     (syntax-ppss-cache nil)
                     (syntax-ppss-last nil)
                     (parse-sexp-lookup-properties nil))
                 (smie-indent-calculate))))
          (if (not (eq 'noindent (car-safe indent)))
              (if (integerp indent)
                  (max (funcall smie-rules-function :elem 'basic) indent)
                indent)
            ;; We can't just return `noindent' if we're inside a comment,
            ;; because the indent.el code would then be similarly confused,
            ;; thinking the `noindent' is because we're inside the cpp
            ;; pseudo-comment, and would hence align the code with the content
            ;; of the psuedo-comment rather than the nested real comment!
            ;;
            ;; FIXME: Copy&paste from indent--default-inside-comment.
            ;; FIXME: This will always re-indent inside these comments, even
            ;; during indent-region.
            (save-excursion
              (forward-line -1)
              (skip-chars-forward " \t")
              (when (< (1- (point)) (cdr indent) (line-end-position))
                (goto-char (cdr indent))
                (when (looking-at comment-start-skip)
                  (goto-char (match-end 0))))
              (current-column))))))

     ((equal (syntax-after (point)) (string-to-syntax "< c")) 0)
     )))

;;; Syntax table

(defvar sm-c-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    st))

(defun sm-c-syntax-propertize (start end)
  (goto-char start)
  (sm-c--cpp-syntax-propertize end)
  (funcall
   (syntax-propertize-rules
    (sm-c--cpp-regexp
     (2 (prog1 "< c"
          (when (and (equal (match-string 3) "include")
                     (looking-at "[ \t]*\\(<\\)[^>\n]*\\(>\\)"))
            (put-text-property (match-beginning 1) (match-end 1)
                               'syntax-table (string-to-syntax "|"))
            (put-text-property (match-beginning 2) (match-end 2)
                               'syntax-table (string-to-syntax "|")))
          (sm-c--cpp-syntax-propertize end)))))
   (point) end))

(defun sm-c-syntactic-face-function (ppss)
  (if (sm-c--cpp-inside-p ppss)
      (prog1 nil (sm-c--cpp-fontify-syntactically ppss))
    (funcall (default-value 'font-lock-syntactic-face-function) ppss)))

;;; SMIE support

(defconst sm-c-paren-block-keywords '("if" "while" "for" "switch"))

(defconst sm-c-smie-precedence-table
  '((assoc ";")
    ;; Compiled from https://en.wikipedia.org/wiki/Operators_in_C_and_C++.
    (assoc ",")                         ;1
    ;; (nonassoc "throw")
    (nonassoc "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|=") ;2
    ;; (nonassoc "?" ":") ;; Better handle it in the BNF.
    (assoc "||")                        ;3
    (assoc "&&")                        ;4
    (assoc "|")                         ;5
    (assoc "^")                         ;6
    ;; (assoc "&") ;; Binary and.  Confused with address-of.
    (nonassoc "==" "!=")                ;7
    (nonassoc "<" "<=" ">" ">=")        ;8
    (nonassoc "<<" ">>")                ;9
    (assoc "+" "-")                     ;10
    (assoc "/" "* mult" "%")            ;11
    ;; (nonassoc ".*" "->*")            ;12   ;; Only C++
    ;; (nonassoc "++" "--" "+" "-" "!" "~" "(type)" "*" "&"
    ;;          "sizeof" "new" "delete");13  ;; All prefix.
    (left "." "->") ;; "++" "--" suffixes, "()", "[]", "typeid", "*_cast". ;14
    ;; (noassoc "::") ;; Only C++
    ))

(defconst sm-c-smie-grammar
  ;; `((:smie-closer-alist ("{" . "}")) ("{" (39) 0) ("}" 0 (40)) ("else" 27 26) ("," 38 38) ("do" (41) 22) ("while" (42) 23) ("for" (43) 24) (";" 11 11) ("if" (44) 25))
  (let ((grm
         (smie-prec2->grammar
          (smie-merge-prec2s
           (smie-bnf->prec2
            '((decls ("typedef" decl) ("extern" decl)
                     (decls ";" decls))
              (decl)
              (id)
              (insts ("{" insts "}")
                     (insts ";" insts)
                     ("return" exp)
                     ("goto" exp)
                     (":label")
                     ("case" subexp ": case")
                     ("else" exp-if))
              (exp-if ("if" exp) ("do" exp) ("while" exp) ("switch" exp) ("for" exp)
                      (exp))
              (exp ("(" exp ")") (exp "," exp) (subexp "?" exp ":" exp))
              (subexp (subexp "||" subexp))
              ;; Some of the precedence table deals with pre/postfixes, which
              ;; smie-precs->prec2 can't handle, so handle it here instead.
              (exp11 (exp12) (exp11 "/" exp11))
              (exp12 (exp13))           ;C++ only.
              (exp13 (exp14) ("++ prefix" exp13) ("-- prefix" exp13)
                     ("!" exp13) ("~" exp13) ("&" exp13) ("* deref" exp13))
              (exp14 (id) (exp14 "++ postfix") (exp14 "-- postfix")
                     (exp14 "->" id) (exp14 "." id)))
            '((assoc ";") (assoc ",") (nonassoc "?" ":"))
            sm-c-smie-precedence-table)
           (smie-precs->prec2 sm-c-smie-precedence-table)
           (smie-precs->prec2 '((nonassoc ";") (nonassoc ":")))))))
    ;; SMIE gives (":label" 261 262), but really this could just as well be
    ;; (":label" nil nil) because labels don't have any argument to their left
    ;; or right.  They're like both openers and closers at the same time.
    (mapcar (lambda (x)
              (if (equal (car-safe x) ":label")
                  ;; Rather than (":label" (n1) (n2)) we use
                  ;; (":label" (n1) n2) because SMIE otherwise complains:
                  ;; cl--assertion-failed((numberp (funcall op-forw toklevels)))
                  ;; in smie-next-sexp.
                  `(,(nth 0 x) (,(nth 1 x)) ,(nth 2 x)) x))
            grm)))

;; (defun sm-c--:-discriminate ()
;;   (save-excursion
;;     (and (null (smie-backward-sexp))
;;          (let ((prev (smie-indent-backward-token)))
;;            (cond
;;             ((equal prev "case" ) ": case")
;;             ((member prev '(";" "{" "}")) ":-label")
;;             (t ":"))))))

(defconst sm-c-smie-operator-regexp
  (let ((ops '()))
    (pcase-dolist (`(,token . ,_) sm-c-smie-grammar)
      (when (and (stringp token) (string-match "\\`[^ [:alnum:](){}]+" token))
        (push (match-string 0 token) ops)))
    (regexp-opt ops)))

(defun sm-c-smie-forward-token ()
  (forward-comment (point-max))
  (let ((tok (if (looking-at sm-c-smie-operator-regexp)
                 (progn (goto-char (match-end 0)) (match-string 0))
               (smie-default-forward-token))))
    (cond
     ((and (equal tok "") (looking-at "\\\\\n"))
      (goto-char (match-end 0))
      (sm-c-smie-forward-token))
     ((member tok '(":" "*"))
      (save-excursion (sm-c-smie-backward-token)))
     ((looking-at "[ \t]*:")
      (if (not (equal (save-excursion (sm-c-smie-forward-token)) ":label"))
          tok
        (looking-at "[ \t]*:")
        (goto-char (match-end 0)) ":label"))
     (t tok))))


(defun sm-c-smie-backward-token ()
  (forward-comment (- (point)))
  (let ((tok (if (looking-back sm-c-smie-operator-regexp (- (point) 3) t)
                 (progn (goto-char (match-beginning 0)) (match-string 0))
               (smie-default-backward-token))))
    (cond
     ((and (equal tok "") (looking-at "\n"))
      (let ((pos (point)))
        (if (not (= 0 (mod (skip-chars-backward "\\\\") 2)))
            (sm-c-smie-backward-token)
          (goto-char pos)
          tok)))
     ((equal tok "*") (sm-c-smie--*-token))
     ((equal tok ":")
      (let ((pos1 (point))
            (prev (sm-c-smie-backward-token)))
        (if (zerop (length prev))
            (progn (goto-char pos1) tok)
          (let ((pos2 (point)))
            (pcase (car (smie-indent-backward-token))
              ("case" (goto-char pos1) ": case")
              ((or ";" "{" "}") (goto-char pos2) ":label")
              (_ (goto-char pos1) tok))))))
     (t tok))))

(defun sm-c--prev-token ()
  (car (smie-indent-backward-token)))

(defun sm-c--else-to-if ()
  (let ((pos (point)))
    (unless (equal (sm-c--prev-token) ";")
      (goto-char pos))
    (while
        (pcase (smie-backward-sexp)
          (`(,_ ,pos "if") (goto-char pos) nil) ;Found it!
          (`(,_ ,_ ";") nil)                    ;Can't find it!
          (`(,_ ,pos "else") (goto-char pos) (sm-c--else-to-if) t)
          (`(,_ ,pos "while")
           (goto-char pos) (unless (sm-c--while-to-do) (goto-char pos)) t)
          (`(t . ,_) nil)               ;Can't find it!
          (`(,_ ,pos . ,_) (goto-char pos) t)
          (`nil t)))))

(defun sm-c--while-to-do ()
  "Jump to the matching `do' and return non-nil, if any.  Return nil otherwise."
  (pcase (sm-c--prev-token)
    ("}"
     ;; The easy case!
     (forward-char 1) (backward-sexp 1)
     (equal (sm-c--prev-token) "do"))
    (";"
     (let ((found-do nil))
       (while
           (pcase (smie-backward-sexp)
             (`(,_ ,pos "do") (goto-char pos) (setq found-do t) nil)
             (`(,_ ,_ ";") nil)         ;Can't find it!
             (`(,_ ,pos "else") (goto-char pos) (sm-c--else-to-if) t)
             (`(,_ ,pos "while")
              (goto-char pos) (unless (sm-c--while-to-do) (goto-char pos)) t)
             (`(t . ,_) nil)            ;Can't find it!
             (`(,_ ,pos . ,_) (goto-char pos) t)
             (`nil (or (not (looking-at "{"))
                       (smie-rule-prev-p "=")))))
       found-do))))

(defun sm-c--skip-labels (max)
  (while
      (let ((start (point)))
        (pcase (sm-c-smie-forward-token)
          ("case"
           (smie-forward-sexp "case")
           (forward-comment (point-max))
           (if (>= (point) max) (progn (goto-char start) nil)
             t))
          (":label"
           (forward-comment (point-max))
           (if (>= (point) max) (progn (goto-char start) nil)
             t))
          (_ (goto-char start) nil)))))

(defun sm-c--boi (&optional inner)
  "Jump to the beginning-of-instruction.
By default for things like nested ifs, it jumps to the outer if, but
if INNER is non-nil, it stops at the innermost one."
  (while
      (let ((pos (point)))
        (pcase (smie-backward-sexp)
          (`(,_ ,_ ";") nil)            ;Found it!
          (`(,_ ,pos "else") (goto-char pos) (sm-c--else-to-if) t)
          (`(,_ ,pos "while")
           (goto-char pos) (unless (sm-c--while-to-do) (goto-char pos)) t)
          (`(,(pred numberp) ,pos . ,_) (goto-char pos) t)
          ((or `nil `(nil . ,_))
           (if (and (or (not (looking-at "{"))
                        (smie-rule-prev-p "="))
                    (not (bobp)))
               t
             (goto-char pos) nil))
          (`(,_ ,_ ,(or "(" "{" "[")) nil) ;Found it!
          (`(,_ ,pos ,(and tok
                           (guard (when inner
                                    (or (member tok sm-c-paren-block-keywords)
                                        (equal tok "do"))))))
           (goto-char pos) nil) ;Found it!
          (`(t ,(pred (eq (point-min))) . ,_) nil)
          (`(,_ ,pos . ,_) (goto-char pos) t)))))

;; (defun sm-c--if-tail-to-head ()
;;   (pcase (sm-c--prev-token)
;;     (")"
;;      (forward-char 1) (backward-sexp 1)
;;      (pcase (sm-c--prev-token)
;;        ("if" nil)
;;        ((or "while" "for") (sm-c--if-tail-to-head))))
;;     ("do" (sm-c--if-tail-to-head))))

(defun sm-c--boe (tok)
  (let ((start (point))
        (res (smie-backward-sexp tok)))
    (when (member (nth 2 res) '("if" "while" "do" "for" "else"))
      (when (member (nth 2 res) '("if" "for"))
        (let ((forward-sexp-function nil))
          (forward-sexp 1))
        (forward-comment (point-max)))
      (when (looking-at "{")
        (let ((forward-sexp-function nil))
          (forward-sexp 1))
        (forward-comment (point-max)))
      (if (> (point) start) (goto-char start)))))

(defun sm-c-smie--*-token ()
  (save-excursion
    (let ((pos (point)))
      (pcase (car (smie-indent-backward-token))
        (")"
         ;; Can be a multiplication (as in "(a+b)*c"), or a deref
         ;; (as in "if (stop) *a = 0;")
         (if (and (goto-char (nth 1 (syntax-ppss)))
                  (eq ?\( (char-after))
                  (member (smie-default-backward-token) '("if" "for")))
             "* deref"
           "* mult"))
        ("]" "* mult")                         ;Multiplication.
        ((or "(" "[" "{" "}") "* deref")
        (`nil
         (goto-char pos)
         (let ((res nil))
           (while (not (or res (bobp)))
             (pcase (smie-backward-sexp)
               (`(,_ ,_ ,(or ";" "{")) (setq res "* deref"))
               ((and `nil (guard (looking-at "{"))) (setq res "* deref"))
               (`(,left ,_ ,op)
                (if (and (numberp left)
                         (numberp (nth 2 (assoc op smie-grammar)))
                         (< (nth 2 (assoc op smie-grammar))
                            (nth 1 (assoc "* mult" smie-grammar))))
                    (smie-backward-sexp 'halfsexp)
                  (setq res "* mult")))))
           (or res "* mult")))
        (_ "* mult")))))

(defun sm-c-smie-hanging-eolp ()
  (let ((start (point))
        (prev (smie-indent-backward-token)))
    (if (and (not (numberp (nth 1 prev)))
             (save-excursion (equal (sm-c-smie-backward-token) ";")))
        ;; Treat instructions that start after ";" as always "hanging".
        (end-of-line)
      (goto-char start)))
  (skip-chars-forward " \t")
  (or (eolp)
      (forward-comment (point-max))
      (and (looking-at "\\\\\n")
           (goto-char (match-end 0)))))

(defvar sm-c-smie--inhibit-case/label-rule nil)

(defun sm-c--smie-virtual ()
  (if (and (smie-indent--bolp)
           (not (save-excursion
                  (member (sm-c-smie-forward-token)
                          '("case" ":label")))))
      (current-column)
    (let ((sm-c-smie--inhibit-case/label-rule t))
      (smie-indent-calculate))))

(defun sm-c-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sm-c-indent-basic)
    (`(:list-intro . ";")
     (save-excursion
       (forward-char 1)
       (if (and (null (smie-forward-sexp))
                ;; FIXME: Handle \\\n as well!
                (progn (forward-comment (point-max))
                       (looking-at "(")))
           nil
         t)))
    (`(:before . "else")
     (save-excursion
       (sm-c--else-to-if)
       `(column . ,(smie-indent-virtual))))
    (`(:before . "while")
     (save-excursion
       (when (sm-c--while-to-do)
         `(column . ,(smie-indent-virtual)))))
    (`(:before . ,(or "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|="))
     (save-excursion
       (sm-c--boe token)
       `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                      (smie-indent-virtual)))))
    (`(:before . "if")
     (when (and (not (smie-rule-bolp)) (smie-rule-prev-p "else"))
       (save-excursion
         (smie-indent-backward-token)
         `(column . ,(sm-c--smie-virtual)))))
    ;; (`(:after . ,(or "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|="))
    ;;  (funcall smie-rules-function :elem 'basic))
    (`(:before . "{")
     (cond
      ((smie-rule-prev-p "=") nil)      ;Not a block of instructions!
      ((save-excursion
         (let ((pos (point)))
           (sm-c--boi 'inner) (sm-c--skip-labels (point-max))
           (let ((tok (save-excursion (sm-c-smie-forward-token))))
             (cond
              ((or (equal tok "typedef")
                   (and (member tok '("enum" "struct"))
                        ;; Make sure that the {...} is about this struct/enum,
                        ;; as opposed to "struct foo *get_foo () {...}"!
                        (save-excursion
                          (smie-indent-forward-token)
                          (smie-indent-forward-token)
                          (forward-comment (point-max))
                          (>= (point) pos))))
               `(column . ,(+ (if (save-excursion
                                    (goto-char pos)
                                    (smie-rule-hanging-p))
                                  0
                                (funcall smie-rules-function :elem 'basic))
                              (smie-indent-virtual))))
              ((and (member tok '("enum" "struct"))
                    ;; Make sure that the {...} is about this struct/enum, as
                    ;; opposed to "struct foo *get_foo () {...}"!
                    (save-excursion
                      (smie-indent-forward-token)
                      (smie-indent-forward-token)
                      (forward-comment (point-max))
                      (>= (point) pos)))
               `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                              (smie-indent-virtual))))
              ((or (member tok sm-c-paren-block-keywords)
                   (equal tok "do"))
               nil)
              ((save-excursion
                 (goto-char pos)
                 (when (and (> (car (syntax-ppss)) 0)
                            (equal ")" (car (smie-indent-backward-token))))
                   (up-list -1)
                   `(column . ,(sm-c--smie-virtual)))))
              ((>= (point) pos) nil)
              (t `(column . ,(smie-indent-virtual))))))))
      ((smie-rule-hanging-p)
       (cond
        ((smie-rule-prev-p "do" "else")
         (smie-indent-backward-token))
        ((smie-rule-prev-p ")")
         (smie-backward-sexp)
         (smie-indent-backward-token))
        (t (sm-c--boi 'inner)))
       `(column . ,(sm-c--smie-virtual)))
      (t
       (let ((pos (point)))
         (pcase (sm-c--prev-token)
           ((or "do" "else")
            (cond
             (sm-c-indent-braces
              `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                             (smie-indent-virtual))))))
           (")" nil)
           (_ (goto-char pos) (sm-c--boi)
              (if (< (point) pos)
                  `(column . ,(sm-c--smie-virtual)))))))))
    (`(:before . "(")
     (save-excursion
       (let ((res (smie-backward-sexp)))
         (pcase res
           (`nil
            (if (looking-at "(")
                ;; (unless (save-excursion
                ;;           (member (sm-c-smie-backward-token)
                ;;                   sm-c-paren-block-keywords))
                ;;   `(column . ,(sm-c--smie-virtual)))
                nil
              `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                             (sm-c--smie-virtual)))))))))
    (`(:after . "else")
     (save-excursion
       (funcall smie-rules-function :elem 'basic)))
    (`(:after . ")")
     (save-excursion
       (let ((_ (progn (forward-char 1) (backward-sexp 1)))
             (pos (point))
             (prev (sm-c-smie-backward-token)))
         (cond
          ((member prev sm-c-paren-block-keywords)
           `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                          (smie-indent-virtual))))
          ((and (looking-at "[[:alnum:]_]+(")
                (save-excursion
                  (forward-line 0)
                  (and (bobp) (looking-at sm-c--cpp-regexp))))
           ;; Will be bumped up presumably by the "max" in
           ;; sm-c--cpp-smie-indent.
           `(column . 0))
          (t (goto-char pos) `(column . ,(sm-c--smie-virtual)))))))
    (`(:after . "}")
     (save-excursion
       (forward-char 1) (backward-sexp 1)
       (sm-c--boi)
       `(column . ,(sm-c--smie-virtual))))
    (`(:after . ";")
     (save-excursion
       (sm-c--boi)
       `(column . ,(sm-c--smie-virtual))))
    (`(:after . ":label")
     ;; Yuck!
     `(column . ,(sm-c--smie-virtual)))
    (`(:after . ": case")
     ;; Yuck!
     (save-excursion
       (smie-backward-sexp ": case")
       `(column . ,(sm-c--smie-virtual))))
    (`(:after . "* deref") `(column . ,(sm-c--smie-virtual)))
    ((and `(:before . ":label") (guard (not sm-c-smie--inhibit-case/label-rule)))
     (let ((ppss (syntax-ppss)))
       (when (nth 1 ppss)
         (save-excursion
           (goto-char (nth 1 ppss))
           `(column . ,(smie-indent-virtual))))))
    ((and `(:before . "case") (guard (not sm-c-smie--inhibit-case/label-rule)))
     (catch 'found
       (dolist (pos (reverse (nth 9 (syntax-ppss))))
         (save-excursion
           (goto-char pos)
           (and (looking-at "{")
                (null (car-safe (smie-backward-sexp)))
                (equal "switch" (sm-c-smie-backward-token))
                (goto-char pos)
                (throw 'found `(column . ,(smie-indent-virtual))))))))))

;;; Backslash alignment

(defvar-local sm-c--bs-changed nil)

(defun sm-c--bs-after-change (beg end _len)
  (unless undo-in-progress
    (if (null sm-c--bs-changed)
        (setq sm-c--bs-changed (cons beg end))
      (cl-callf (lambda (x) (min x beg)) (car sm-c--bs-changed))
      (cl-callf (lambda (x) (max x end)) (cdr sm-c--bs-changed)))))

(defun sm-c--bs-realign ()
  (when sm-c--bs-changed
    (sm-c--bs-realign-1 (car sm-c--bs-changed) (cdr sm-c--bs-changed))
    (setq sm-c--bs-changed nil)))

(defun sm-c--bs-realign-1 (from to)
  (save-excursion
    (goto-char from)
    (end-of-line)
    (unless (zerop (mod (skip-chars-backward "\\\\") 2))
      (skip-chars-backward " \t")
      (setq from (point))
      (let ((col (current-column))
            start end)
        (while
            (progn (setq start (point))
                   (end-of-line 0)
                   (and (< (point) start)
                        (not (zerop (mod (skip-chars-backward "\\\\") 2)))))
          (skip-chars-backward " \t")
          (setq col (max (current-column) col)))
        (goto-char from)
        (while
            (progn (setq end (point))
                   (end-of-line 2)
                   (and (> (line-beginning-position) end)
                        (not (zerop (mod (skip-chars-backward "\\\\") 2)))))
          (skip-chars-backward " \t")
          (setq col (max (current-column) col)))
        (goto-char to)
        (beginning-of-line)
        (unless (or (> (point) end)       ;Don't realign if we changed outside!
                    (<= end start))        ;A lone \
          
          (setq col (1+ col))         ;Add a space before the backslashes.
          (goto-char end)
          (end-of-line)
          (while (>= (point) start)
            (cl-assert (eq (char-before) ?\\))
            (forward-char -1)
            (let ((curcol (current-column)))
              (cond
               ((> col curcol) (indent-to col))
               ((< col curcol)
                (move-to-column col t)
                (delete-region (point)
                               (progn (skip-chars-forward " \t") (point))))))
            (end-of-line 0)))))))
            
;;; Font-lock support

(defconst sm-c--comment-regexp
  "/\\(?:/.*\n\\|\\*[^*]*\\(?:\\*+[^/*][^*]*\\)*\\*+/\\)")

(defconst sm-c--defun-regexp
  (let* ((spc0 (concat "\\(?:\n?[ \t]\\|" sm-c--comment-regexp "\\)*"))
         (spc1 (concat "\n?[ \t]" spc0))
         (id "\\(?:\\sw\\|\\s_\\)+"))
    (cl-flet ((repeat (repetition &rest res)
                      (concat "\\(?:" (apply #'concat res) "\\)"
                              (pcase repetition
                                ((pred symbolp) (symbol-name repetition))
                                (1 "")))))
      (concat
       "^\\(?:"
       (repeat '* "\\*" spc0)                               ;Pointer symbols.
       (repeat '* id (repeat 1 spc1 "\\|" spc0 "\\*" spc0)) ;Type(s).
       "\\(" id "\\)[ \t\n]*("                              ;Function name.
       "\\|"
       "[ \t]*#[ \t]*define[ \t]+\\(?1:" id "\\)("
       "\\)"))))

(defconst sm-c-font-lock-keywords
  `((,sm-c--cpp-regexp (1 font-lock-preprocessor-face))
    ("\\_<\\(?:true\\|false\\)\\_>" (0 font-lock-constant-face))
    ("\\_<\\(case\\)\\_>[ \t]*\\([^: \t]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
    ("\\(?:[{};]\\(\\)\\|^\\)[ \t]*\\([[:alpha:]_][[:alnum:]_]*\\)[ \t]*:"
     (2 (if (or (match-beginning 1)
                (save-excursion (equal ":label" (sm-c-smie-backward-token))))
            font-lock-constant-face)))
    (,(let ((kws (delq nil (mapcar (lambda (x)
                                     (setq x (car x))
                                     (and (stringp x)
                                          (string-match "\\`[a-z]" x)
                                          x))
                                   sm-c-smie-grammar))))
        (concat "\\_<" (regexp-opt
                        (append
                         ;; Elements not in SMIE's grammar.  Either because
                         ;; they're uninteresting from a parsing point of view,
                         ;; or because SMIE's parsing engine can't handle them
                         ;; even poorly.
                         '("break" "continue" "struct" "enum" "union" "static")
                         ;; "case" already handled above.
                         (delete "case" kws)))
                "\\_>"))
     (0 font-lock-keyword-face))
    (,sm-c--defun-regexp
     (1
      (prog1 font-lock-function-name-face
        (if (< (match-beginning 0) (line-beginning-position))
            (put-text-property (match-beginning 0) (match-end 0)
                               'font-lock-multiline t)))))))

(defconst sm-c--def-regexp
  (let ((spc0 (concat "\\(?:[ \t\n]\\|" sm-c--comment-regexp "\\)*"))
        (id "\\(?:\\sw\\|\\s_\\)+"))
    (concat sm-c--defun-regexp
            "\\|"
            "\\_<\\(?1:\\(?:struct\\|enum\\)[ \t]+" id "\\)" spc0 "{")))

;;;###autoload
(define-derived-mode sm-c-mode prog-mode "smC"
  "C editing mode based on SMIE."
  ;; (setq-local font-lock-support-mode nil) ;; To help debugging.
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local syntax-propertize-function #'sm-c-syntax-propertize)
  (setq-local font-lock-defaults '(sm-c-font-lock-keywords))
  (setq-local font-lock-syntactic-face-function #'sm-c-syntactic-face-function)
  (smie-setup sm-c-smie-grammar #'sm-c-smie-rules
              :backward-token #'sm-c-smie-backward-token
              :forward-token #'sm-c-smie-forward-token)
  ;; FIXME: The stock SMIE forward-sexp-function is not good enough here, since
  ;; our grammar is much too poor.  We should setup another function instead
  ;; (or ideally teach SMIE to use it).
  (kill-local-variable 'forward-sexp-function)
  (add-hook 'smie-indent-functions #'sm-c--cpp-smie-indent nil t)
  (add-function :after (local 'indent-line-function)
                #'sm-c--cpp-indent-line)
  (setq-local smie--hanging-eolp-function #'sm-c-smie-hanging-eolp)
  ;; Backslash auto-realign.
  (add-hook 'after-change-functions #'sm-c--bs-after-change nil t)
  (add-hook 'post-command-hook #'sm-c--bs-realign nil t)
  (setq-local add-log-current-defun-header-regexp sm-c--def-regexp)
  (setq-local imenu-generic-expression `((nil ,sm-c--def-regexp 1))))

(defun sm-c--cpp-is-not-really-a-comment (&rest args)
  ;; Without this, placing the region around a CPP directive and hitting
  ;; M-; would just strip the leading "#" instead of commenting things out.
  (if (not (derived-mode-p 'sm-c-mode))
      (apply args)
    (let ((parse-sexp-lookup-properties nil))
      (apply args))))

;; FIXME: Maybe we should change newcomment.el instead; or maybe CPP directives
;; should not be defined as comments, or at least "not always"!
(advice-add 'comment-only-p :around #'sm-c--cpp-is-not-really-a-comment)

;;;; ChangeLog:

;; 2020-04-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/sm-c-mode/sm-c-mode.el: Release as version 1.0
;; 
;; 2017-04-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode.el: Fix some problems reported by Andrés Ramírez
;; 
;; 	(sm-c-smie-rules): Avoid inf-looping on "case foo: {".
;; 	(sm-c--cpp-syntax-propertize): Treat trailing //...\n in CPP directive 
;; 	as being outside the directive.
;; 
;; 2016-07-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode.el (sm-c--comment-regexp): Fix excessive backtracking
;; 
;; 	(sm-c-mode-syntax-table): Add + and -.
;; 
;; 2016-01-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode/sm-c-mode.el (sm-c-smie-rules): Improve indent of enum's
;; 	{...}
;; 
;; 2015-11-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/sm-c-mode: Add imenu/which-func/add-log support
;; 
;; 	(sm-c--comment-regexp): New var.
;; 	(sm-c--defun-regexp): New var, extracted from sm-c-font-lock-keywords. 
;; 	Also recognize macro definitions.
;; 	(sm-c-font-lock-keywords): Use it.
;; 	(sm-c--def-regexp): New var.
;; 	(sm-c-mode): Use it for add-log and imenu.
;; 
;; 2015-11-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode.el: Improve "Commentary:" and docstrings
;; 
;; 	* sm-c-mode.el: Improve "Commentary:" and docstrings.
;; 	(sm-c-smie--*-token): Fix inf-loop at BOB.
;; 
;; 	* GNUmakefile (%.reindent): New rule.
;; 	(HIJACK): New var.
;; 	(%.test): Use $<.
;; 
;; 2015-11-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode: Improve indentation of struct; plus bug fixes
;; 
;; 	* packages/sm-c-mode/GNUmakefile: New file.
;; 
;; 	* packages/sm-c-mode/sm-c-mode-test.c: Add some "struct" tests. Plus a
;; 	#include.
;; 
;; 	* packages/sm-c-mode/sm-c-mode.el (sm-c-syntax-propertize): Mark the
;; 	<...> of #include as a string.
;; 	(sm-c-smie--*-token): Try not to look too far back.
;; 	(sm-c-smie-rules): Indent the {...} of struct and enum definitions.
;; 	(sm-c--bs-realign-1): Fix behavior at EOB.
;; 	(sm-c--cpp-is-not-really-a-comment): New function.
;; 	(comment-only-p): Use it.
;; 
;; 2015-11-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode.el: Auto-align backslashes.	 Improve indent rules
;; 
;; 	* sm-c-mode/sm-c-mode.el: Auto-align backslashes.  Improve indent rules.
;; 	(sm-c--cpp-indent-comment-inside): New function.
;; 	(sm-c--cpp-smie-indent-functions): Use it.
;; 	(sm-c--cpp-smie-indent): Handle indentation within comments-within-cpp.
;; 	(sm-c-smie-grammar): Manually tweak precedence of ":label".
;; 	(sm-c-smie-operator-regexp): Parentheses aren't operators.
;; 	(sm-c--boi): Add `inner' arg.
;; 	(sm-c--boe): Complete the code.
;; 	(sm-c-smie--*-token): Refine.
;; 	(sm-c-smie-rules): Various tweaks based on tests on src/eval.c and 
;; 	src/xdisp.c.
;; 	(sm-c--bs-changed): New var.
;; 	(sm-c--bs-after-change, sm-c--bs-realign, sm-c--bs-realign-1): New
;; 	functions.
;; 	(sm-c-mode): Use them.
;; 
;; 	* sm-c-mode/sm-c-mode-test.c: Add a few more cases.
;; 
;; 2015-11-14  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode.el (sm-c-font-lock-keywords): Match function defs.
;; 
;; 	* sm-c-mode-test.c: New file.
;; 
;; 2015-11-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sm-c-mode: New experimental package
;; 


(provide 'sm-c-mode)
;;; sm-c-mode.el ends here
