;;; cangjie.el --- retrieve cangjie code for han characters  -*- lexical-binding: t; -*-

;; Authors: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/kisaragi-hiu/cangjie.el
;; Package-Version: 20190322.1134
;; Version: 0.7.2
;; Package-Requires: ((emacs "24.4") (s "1.12.0") (dash "2.14.1") (f "0.2.0"))
;; Keywords: convenience, writing

;; This file is NOT part of GNU Emacs.

;; cangjie.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cangjie.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cangjie.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lookup the Cangjie encoding for a given Han character.

;;; Code:

(require 's)
(require 'f)
(require 'dash)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

(defgroup cangjie nil
  "Lookup Cangjie code from a RIME dictionary or Wiktionary."
  :group 'convenience
  :prefix "cangjie-")

(defcustom cangjie-source 'rime
  "RIME dictionary to lookup the character's code in.

Its value can be:

- a path,
  which makes `cangjie' read from that path if it's a valid RIME dictionary.
- `rime',
  to download the dictionary from URL `https://github.com/rime/rime-cangjie',
  and save it for future use.
- `wiktionary-raw',
  to output the line with the Cangjie code on the character's Wiktionary page,
- `wiktionary', or anything else,
  to grep the Wiktionary page like `wiktionary-raw', then try to remove the
  markup in the result, leaving just the Cangjie code.

Set to `rime' by default, so the dictionary will be downloaded on first use."
  :group 'cangjie
  :type '(choice file
                 (const :tag "'rime" rime)
                 (const :tag "'wiktionary-raw" wiktionary-raw)
                 (const :tag "'wiktionary" wiktionary)))

(defcustom cangjie-display-format 'han
  "Format to display Cangjie code in.

- `han' means the Han character representation \(弓木)
- `abc' means the alphabetical representation \(nd)
- `combined' means a combined format that looks like \"弓木 \(nd)\""
  :group 'cangjie
  :type '(choice (const :tag "Han character" han)
                 (const :tag "Alphabet" 'abc)
                 (const :tag "Combined" 'combined)))


(defun cangjie--grep-buffer (buffer s)
  "Return lines in BUFFER matching S as a list."
  (let (results)
    (with-current-buffer buffer
      (goto-char 0)
      (while (search-forward s nil t)
        (setq results
              (cons (buffer-substring-no-properties
                     (progn (beginning-of-line) (point))
                     (progn (end-of-line) (point)))
                    results))))
    results))

(defun cangjie--grep-file (file s)
  "Grab lines from FILE containing S, and return them as a list."
  (with-temp-buffer
    (insert-file-contents file)
    (cangjie--grep-buffer (current-buffer) s)))

(defun cangjie--file-contains? (file s)
  "Does FILE contain S?"
  (not (not (cangjie--grep-file file s))))

(defun cangjie--valid-rime-dict? (val)
  "Check if VAL is a path to a valid RIME dictionary."
  (and (stringp val)
       (f-exists? val)
       (s-suffix? ".yaml" val)
       (cangjie--file-contains? val "name:")
       (cangjie--file-contains? val "use_preset_vocabulary:")))

(defvar cangjie--abc-han-alist
  '(("a" . "日") ("b" . "月") ("c" . "金") ("d" . "木") ("e" . "水") ("f" . "火")
    ("g" . "土") ("h" . "竹") ("i" . "戈") ("j" . "十") ("k" . "大") ("l" . "中")
    ("m" . "一") ("n" . "弓") ("o" . "人") ("p" . "心") ("q" . "手") ("r" . "口")
    ("s" . "尸") ("t" . "廿") ("u" . "山") ("v" . "女") ("w" . "田") ("x" . "難")
    ("y" . "卜") ("z" . "重"))
  "Alist mapping alphabetical Cangjie code to Han characters.")

(defun cangjie--abc-to-han (abc)
  "Convert alphabetical Cangjie code representation ABC into Han characters."
  (->> (downcase abc)
       (s-split "")
       (--map (cdr (assoc it cangjie--abc-han-alist)))
       (s-join "")))

(defun cangjie--han-to-abc (han)
  "Convert Han character Cangjie code representation HAN into alphabets."
  (->> (s-split "" han)
       (--map (car (rassoc it cangjie--abc-han-alist)))
       (s-join "")))

(cl-defun cangjie--echo (code)
  "Echo CODE into the echo area, applying `cangjie-display-format'.

Assumes that CODE is in HAN character format."
  (unless code
    (cl-return-from cangjie--echo nil))
  (case cangjie-display-format
    (han (message "%s" code))
    (combined (message "%s (%s)" code (cangjie--han-to-abc code)))
    (t (message "%s" (cangjie--han-to-abc code)))))

;;;###autoload
(cl-defun cangjie (character)
  "Retrieve Cangjie code for the han CHARACTER."
  (interactive "M漢字：")
  (when (characterp character)
    (setq character (char-to-string character)))
  (unless (stringp character)
    (error "%s not a string or character" character))
  ;; not a han character
  (unless (eq (aref char-script-table (string-to-char character))
              'han)
    (cl-return-from cangjie nil))
  (let ((result
         (cond ((eq cangjie-source 'rime)
                (let ((cangjie-source (f-join user-emacs-directory "cangjie5.dict.yaml")))
                  (if (cangjie--valid-rime-dict? cangjie-source)
                      (cangjie character)
                    ;; download the dictionary when it's not there
                    (url-copy-file
                     "https://raw.githubusercontent.com/rime/rime-cangjie/master/cangjie5.dict.yaml"
                     cangjie-source)
                    (cangjie character))))

               ((cangjie--valid-rime-dict? cangjie-source)
                ;; take cangjie encoding from RIME dictionary
                (->> (cangjie--grep-file cangjie-source character)
                     (--filter (not (s-prefix? "#" it)))
                     ;; if there are multiple, grab the shortest
                     (--sort (< (length it) (length other)))
                     first
                     (s-split "\t")
                     second
                     cangjie--abc-to-han))

               ((eq cangjie-source 'wiktionary-raw)
                (cangjie--grep-buffer (url-retrieve-synchronously
                                       (concat "https://zh.wiktionary.org/wiki/"
                                               character))
                                      "仓颉"))

               (t
                ;; Try to extract encoding from grep'd wiktionary text
                (->>
                 (cangjie--grep-buffer (url-retrieve-synchronously
                                        (concat "https://zh.wiktionary.org/wiki/"
                                                character))
                                       "仓颉")
                 (s-replace-regexp "^.*：" "")
                 s-trim
                 (s-replace-regexp "<.*>$" "")
                 cangjie--abc-to-han)))))

    (when (called-interactively-p 'interactive)
      (cangjie--echo result))
    result))

;;;###autoload
(defun cangjie-at-point ()
  "Run `cangjie' at point."
  (interactive)
  (when-let* ((char (char-after))
              (result (cangjie (string char))))
    (when (called-interactively-p 'interactive)
      (cangjie--echo result))
    result))

(defun cangjie--at-point-in-buf (buf)
  "Return Cangjie code for character at current point in BUF."
  (interactive)
  (let ((current-point (point)))
    (with-current-buffer buf
      (setf (point) current-point)
      (cangjie--echo (cangjie-at-point)))))

;;;###autoload
(defun cangjie-practice ()
  "Start a Cangjie practicing session.

Should be run in a buffer containing the text to practice.

This will discard the current window layout; launch in a new
frame if you don't want that to happen.

The Cangjie code for the next character to type will be displayed
in the echo area while you type. Use `cangjie-format' to control
the way it's displayed."
  (interactive)
  (delete-other-windows)
  (let ((text-buf (current-buffer))
        ;; this will change current-buffer
        (practice-buf (pop-to-buffer (get-buffer-create "*Cangjie Practice*"))))
    (with-current-buffer practice-buf
      (delete-region (point-min) (point-max))
      (add-hook 'post-command-hook
                ;; Since this is local, I reckon it's fine to not name this.
                (lambda () (cangjie--at-point-in-buf text-buf))
                nil
                :local))))

(provide 'cangjie)
;;; cangjie.el ends here
