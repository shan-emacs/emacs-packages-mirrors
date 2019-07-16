;;; advice-patch.el --- Use patches to advise the inside of functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Package-requires: ((emacs "24.4"))
;; Version: 0.1
;; Keywords:

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

;; This package builds on `advice-add' but instead of letting you add code
;; before/after/around the body of the advised function, it lets you directly
;; patch the inside of that function.

;; This is inspired from [el-patch](https://github.com/raxod502/el-patch),
;; but stripped down to its barest essentials.  `el-patch' provides many more
;; features, especially to be notified when the advised function is modified
;; and to help you update your patches accordingly.

;; Beware: this can eat your lunch and can misbehave unexpectedly in many
;; legitimate cases.

;;;; TODO:

;; - Lots of cases to fix and features to add.  See FIXMEs in the code.

;;; Code:

;; Changing the *internals* of a function.
;; Inspired by https://github.com/raxod502/el-patch

(defun advice--patch (form newcode oldcodes)
  "Return FORM where one occurrence of one of OLDCODES is replaced with NEWCODE."
  ;; FIXME: Maybe provide fancier "patch specifications" than just
  ;; "newcode <-> oldcodes" (e.g. like el-search-query-replace).
  ;; E.g. maybe it would be good to allow specifying
  ;; how many occurrences to expect/replace.  Or to specify a *sequence* of
  ;; oldcodes to replace.  But this is at least "good enough" in the sense that
  ;; in the worst case, you can just provide the full original function body
  ;; along with its replacement.
  (let ((counter 0))
    (letrec ((patch (lambda (x)
                      (cond
                       ((member x oldcodes)
                        (setq counter (1+ counter))
                        newcode)
                       ((consp x)
                        (cons (funcall patch (car x))
                              (funcall patch (cdr x))))
                       (t x)))))
      (let ((new-form (funcall patch form)))
        (cond
         ((= counter 0)
          (error "Old code not found!"))
         ((> counter 1)
          (error "Not sure which of %d copies of oldcode to patch" counter))
         (t new-form))))))

;;;###autoload
(defun advice-patch (name newcode oldcode)
  "Replace OLDCODE with NEWCODE in the definition of NAME.
OLDCODE is an S-expression to replace in the source code.
It can also be a vector of S-expressions, so that you can specify the various original source codes found in various Emacs versions."
  ;; FIXME: We probably want to *name* the override, so as to be able to
  ;; remove/update it.
  ;; FIXME: Provide a docstring that describes the effect of the patch!
  ;; FIXME: Make it work on functions that aren't defined yet!
  ;; FIXME: Make it possible to combine several "overrides".
  ;; FIXME: Should this be a macro, so that `newcode' can refer to lexical
  ;; variables defined in the context?
  (pcase-let* ((`(,buf . ,pos)
                (let ((enable-local-variables :safe))
                  (or (find-function-noselect name 'lisp-only)
                      (error "Can't find source file of %S" name))))
               (pos (or pos
                        (error "Can't find original definition of %S" name)))
               (form (with-current-buffer buf
                       (save-excursion
                         (goto-char pos)
                         (read (current-buffer)))))
               (orig-body
                (if (and (equal name (nth 1 form))
                         (listp (nth 2 form)))
                    (nthcdr 3 form)
                  (error "Don't know how to extract the original body of %S"
                         name)))
               (new-body
                (advice--patch orig-body newcode
                               (if (vectorp oldcode)
                                   (mapcar #'identity oldcode)
                                 (list oldcode))))
               ;; FIXME: This new function is incorrect if `form' is something
               ;; like a `define-derived-mode' or `define-minor-mode', ...
               ;; Basically, this works for `defun' (and with luck maybe a few
               ;; other cases as well).
               (fundef `(lambda ,(nth 2 form) . ,new-body))
               (funval (let ((lexical-binding (with-current-buffer buf
                                                lexical-binding)))
                         (byte-compile fundef))))
    ;; FIXME: `C-h o' on the function will show the docstring twice!
    ;; FIXME: This doesn't keep track of the source of the override, we should
    ;; indirect through a symbol and add it to current-load-list.
    ;; FIXME: There's no easy way to un-override the function!
    (advice-add name :override funval
                ;; Only override the original definition, not the various
                ;; pieces of advice that might have been applied to it.
                '((depth . 100)))))

;;;; ChangeLog:

;; 2019-05-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/advice-patch/advice-patch.el: New package.
;; 


(provide 'advice-patch)
;;; advice-patch.el ends here
