;;; jack-connect.el --- Manage jack connections within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2019 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.2
;; Package-Version: 20200519.1027
;; Package-Commit: fae8c5f9b383f7606f3883badfd1294e8affb0db

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

;; `jack-connect' and `jack-disconnect' allow to manage connections of
;; jackd audio server from Emacs minibuffer.
;; `jack-snapshot-to-register' stores a snapshot of current
;; connections in a register.  That can be later restored using
;; `jump-to-register'.
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)


(defun make--jtrie (strings)
  "Construct a prefix tree from a list of STRINGS."
  (let ((trie))
    (cl-loop for str in strings
             do
             (setf trie (jtrie--push trie str str)))
    (jtrie--compress trie)))


(defun jtrie--push (trie keystr elem)
  "Add  ELEM to TRIE, indexed by KEYSTR."
  (if (string-empty-p keystr)
      ;; terminal node. elem must be atomic
      (cons elem trie)
    (let* ((first-char    (substring keystr 0 1))
           (rest-string   (substring keystr 1))
           (matching-node (assoc first-char trie #'string=)))
      (pcase matching-node
        ;; found a node
        (`(,_ . ,children)
         (setf (cdr matching-node)
               (jtrie--push children
                             rest-string
                             elem))
         
         trie)
        ;; no matching node found
        (_
         (let ((new-elt
                `(,first-char
                  ,@(jtrie--push (list)
                                  rest-string
                                  elem))))
           (cons new-elt trie)))))))

(defun jtrie--prepend-string-to-node (node str)
  "Prepend STR to prefix of a NODE."
  (pcase node
    ((pred atom) node)

    (`(,suffix . ,nodes)
     `(,(concat str suffix) ,@nodes))))

(defun jtrie--compress-node (node)
  "Unify NODE with child if node has only a child."
  ;; E.g. (("a" ("b" ("c" "d")))) -> (("ab" ("c" "d")))
  (pcase node
    ((pred atom) node)

    ;; single child
    (`(,prefix (,suf . ,nodes))
     (jtrie--compress-node
      `(,(concat prefix suf) ,@nodes)))

    ;; multiple children
    (`(,prefix . ,nodes)
     `(,prefix ,@(mapcar #'jtrie--compress-node nodes)))
                            
    (err (error "Invalid node %s" err))))


(defun jtrie--compress (trie)
  "Compress TRIE by unifying nodes having one child with their children."
  (mapcar #'jtrie--compress-node trie))


(defun jtrie--disband (trie key)
  "Disband nodes in TRIE when applying KEY on children gives different results."
  ;; E.g.: (("a" ("b") ("c")) => (("ab") ("ac"))
  (letrec ((extract-properties
            (lambda (node)
              (pcase node
                ((pred atom)
                 ;; (props node)
                 `(,(funcall key node) ,node))
                
                (`(,prefix . ,trie)
                 (let* ((prop+nodes   (mapcar extract-properties trie))
                        (prop         (mapcar #'car prop+nodes))
                        (trie         (mapcan #'cdr prop+nodes))
                        (grouped-prop (cl-reduce (lambda (a b)
                                                   (and (equal a b) a))
                                                 prop)))
                   (if grouped-prop
                       ;; (props node)
                       `(,grouped-prop (,prefix ,@trie))
                     ;; (props node*)
                     `(nil ,@(mapcar (lambda (node)
                                     (jtrie--prepend-string-to-node node prefix))
                                   trie)))))))))
    (let* ((prop+nodes (mapcar extract-properties trie))
           (trie       (mapcan #'cdr prop+nodes)))
      (jtrie--compress trie))))

(defun jtrie--filter (trie predicate)
  "Keep only nodes in TRIE where PREDICATE is t."
  (cl-flet ((filter-node
             (node)
             (pcase node
               ((pred atom)
                (and (funcall predicate node) node))
               (`(,prefix . ,nodes)
                (let ((nodes (jtrie--filter nodes
                                              predicate)))
                  (and nodes (cons prefix
                                 nodes)))))))
    (seq-filter #'identity
                (mapcar #'filter-node trie))))

(defun jtrie--flatten (trie)
  "Transform TRIE into an alist.
Recursively accumulate atoms descendent from node into each node."
  ;; (("ab" ("c" "d"))) -> (("ab" "abc" "abd") ("abc" "abc") ("abd" "abd"))
  (let ((alst))
    (letrec ((collect-node
              (lambda (node prefix)
                (pcase node
                  ((pred atom)
                   (list node))
                  
                  (`(,suffix . ,trie)
                   (let* ((prefix (concat prefix suffix))
                          (atoms (seq-mapcat
                                  (lambda (node)
                                    (funcall collect-node
                                             node
                                             prefix))
                                  trie))
                          (sorted-atoms
                           (sort atoms #'string-lessp)))
                     
                     (push `(,prefix ,@sorted-atoms) alst)
                     sorted-atoms))))))
      (dolist (node trie)
        (funcall collect-node node ""))
      alst)))

(defun jtrie--mutate (trie mutator)
  "Mutate TRIE by applying the MUTATOR to each terminal node."
  (let ((mutate-node
         (lambda (node)
           (pcase node
             ((pred atom) (funcall mutator node))
             (`(,ch . ,trie) (jtrie--mutate trie mutator))))))
    (mapcar mutate-node trie)))

(defun jtrie--decorate-alist (alst)
  "Append `*' to keys with more than one value in ALST."
  ;; (("ab" "abc" "abd")) -> (("ab*" "abc" "abd"))
  (mapcar (lambda (kv)
            (pcase kv
              ((pred atom) kv)
              (`(,k ,v) kv)
              (`(,k . ,v) `(,(concat k "*") ,@v))))
          alst))

(defun jtrie-->alst (trie)
  "Transform TRIE into an alist."
  (-> trie
     (jtrie--compress)
     (jtrie--flatten)
     (jtrie--decorate-alist)))



(defvar jack--port-table (make-hash-table :test #'equal))

(defun jack--make-port (name &optional client port-name)
  "Create the port record NAME with optional CLIENT and PORT-NAME."
  (puthash name
           (copy-tree `((:client ,@client)
                        (:name   ,@port-name)
                        (:connections)
                        (:type)
                        (:properties)))
           jack--port-table))

(defun jack--list-ports ()
  "Return a list of port names."
  (let ((lst))
    (maphash (lambda (k v) (push k lst)) jack--port-table)
    lst))

(defmacro jack-get-port (port)
  "Retrieve port properties by PORT name."
  `(gethash ,port jack--port-table))


(defmacro define-jack-port-accessor (name kw)
  "Define NAME as both a getter and setter of a jack port property KW."
  (let ((setter (intern (format "%s-set" name))))
    `(progn
       (defun ,name (port)
         (alist-get ,kw (jack-get-port port)))
       (defun ,setter (port value)
         (setf (alist-get ,kw (jack-get-port port)) value))
       (gv-define-simple-setter ,name ,setter))))

(define-jack-port-accessor jack-port-properties  :properties)
(define-jack-port-accessor jack-port-type        :type)
(define-jack-port-accessor jack-port-name        :name)
(define-jack-port-accessor jack-port-connections :connections)
(define-jack-port-accessor jack-port-client      :client)


(defun jack-port-input-p (port)
  "Return t if PORT is an input port."
  (and (memq 'input (jack-port-properties port)) t))

(defun jack-port-output-p (port)
  "Return t if PORT is an output port."
  (and (memq 'output (jack-port-properties port)) t))

(defun jack-port-audio-p (port)
  "Return t if PORT is an audio port."
  (string-match-p "audio" (jack-port-type port)))

(defun jack-port-midi-p (port)
  "Return t if PORT is a midi port."
  (string-match-p "midi" (jack-port-type port)))

(defun jack-port-connected-p (port)
  "Return t if PORT is connected."
  (jack-port-connections port))

(defun jack-running-p ()
  "Return t if jackd is started."
  (pcase (process-lines "jack_wait" "-c" "-s" "default")
    (`("running") t)
    (`("not running") nil)))

(defun jack-lsp ()
  "Update the port table parsing the output of jack_lsp."
  (if (not (jack-running-p))
      (error "Jack default server is not active")
    (let ((current-port nil))
      (clrhash jack--port-table)
      (dolist (line (process-lines "jack_lsp" "-ctp"))
        (cond
         ;; port properties
         ((string-match "^[ \t]+properties: \\(.*\\)" line)
          (setf (jack-port-properties current-port)
                (mapcar #'intern
                        (split-string (replace-match "\\1" nil nil line) "," t))))

         ;; port connection
         ((string-match "^ \\{3\\}\\(.*\\)" line)
          (push (replace-match "\\1" nil nil line)
                (jack-port-connections current-port)))

         ;; port type
         ((string-match "^[ \t]+\\(.*\\)" line)
          (setf (jack-port-type current-port)
                (replace-match "\\1" nil nil line)))

         ;; port name (sets current-port)
         (t
          (cl-destructuring-bind (client &rest port)
              (split-string line ":")
            (setf current-port line)
            (jack--make-port current-port client (string-join port)))))))))


;;;###autoload
(defun jack-connect (p1s p2s)
  "Connect port selection P1S with port selection P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((from (if current-prefix-arg 'input 'output))
            (tree   (-> (jack--list-ports)
                       (make--jtrie)))
            (node1 (-> tree
                      (jtrie--filter
                       (case from
                         (input  #'jack-port-input-p)
                         (output #'jack-port-output-p)))
                      (jtrie--disband (lambda (p)
                                          (list
                                           (jack-port-client p)
                                           (jack-port-type p))))
                      (jtrie-->alst)))
            (sel1
             (if node1
                 (completing-read "connect: " node1)
               (error (format "There are no %s ports registered" from))))
            (p1s   (cdr (assoc sel1 node1)))
            (type  (jack-port-type (car p1s)))
            (node2 (-> tree
                      (jtrie--filter
                       (case from
                         (input  #'jack-port-output-p)
                         (output #'jack-port-input-p)))
                      (jtrie--filter
                       (lambda (p)
                         (string= (jack-port-type p) type)))
                      (jtrie-->alst)))
            (sel2  (completing-read (format "connect %s to: "
                                            sel1)
                                    node2)))
       (list p1s (cdr (assoc sel2 node2))))))
  (when p1s
    (cl-mapc (lambda (p1 p2)
               (call-process "jack_connect" nil nil nil
                             p1
                             p2))
             p1s
             p2s)))


(defun jack--merge-connections (ports)
  "Return the union of the connections of PORTS."
  (cl-reduce #'cl-union
             (mapcar #'jack-port-connections ports)))

;;;###autoload
(defun jack-disconnect (p1s p2s)
  "Disconnect port set P1S from port set P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((node1 (-> (jack--list-ports)
                      (make--jtrie)
                      (jtrie--filter #'jack-port-connected-p)
                      (jtrie--disband #'jack-port-client)
                      (jtrie-->alst)))
            (sel1
             (if node1
                 (completing-read "disconnect jack port(s): " node1)
               (error "There are no jack connections")))
            (p1s   (cdr (assoc sel1 node1)))
            ;; make an alst with p1s
            (node2 (-> (jack--merge-connections p1s)
                      (make--jtrie)
                      (jtrie-->alst)))
            (sel2  (completing-read (format "disconnect %s from: "
                                            sel1)
                                    node2))
            (p2s   (cdr (assoc sel2 node2))))
       (list p1s p2s))))
  (when p1s
    (dolist (p1 p1s)
      (dolist (p2 p2s)
        (when (member p2 p2s)
          (call-process "jack_disconnect" nil nil nil
                        p1 p2))))))



(defun jack--snapshot-restore (snapshot)
  "Restore all the connections in SNAPSHOT."
  (jack-lsp)
  (cl-loop
   for cell in snapshot
   for (k . v) = cell
   when (and (gethash k jack--port-table )
           (gethash v jack--port-table))
   do
   (call-process "jack_connect" nil nil nil k v)))

(defun jack--snapshot-princ (snapshot)
  "Display SNAPSHOT."
  (princ (format "jack-snapshot:\n%s" snapshot)))

(defun jack--snapshot ()
  "Return an alist with all the current connections in jack."
  (jack-lsp)
  (cl-loop
   for p in (jack--list-ports)
   when (jack-port-output-p p)
   append
   (cl-loop
    for c in (jack-port-connections p)
    collect   (cons p c))))

;;;###autoload
(defun jack-snapshot-to-register (snapshot reg)
  "Store a SNAPSHOT of jack connections into register REG.
Restore connections using `jump-to-register'."
  (interactive
   (let ((snapshot (jack--snapshot)))
     (if snapshot
         (list snapshot
               (register-read-with-preview "Jack snapshot to register: "))
       (error "There are no connections"))))
  (set-register reg
                (registerv-make
                 snapshot
                 :print-func #'jack--snapshot-princ
                 :jump-func #'jack--snapshot-restore)))

(provide 'jack-connect)
;;; jack-connect.el ends here
