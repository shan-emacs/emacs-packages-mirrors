;;; ejson-mode.el --- Major mode for editing ejson files. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Dante Catalfamo

;; Author: Dante Catalfamo
;; Version: 0.5.2
;; Package-Version: 20190720.2138
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/dantecatalfamo/ejson-mode
;; Keywords: convenience, languages, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode designed for editing ejson files.  Will automatocally
;; generate encryption keys if none are present in the file and allows
;; for manual in-buffer encryption and decryption, and optional
;; automatic ejson encryption on save.  Location of ejson keystore and
;; binary can be set manually.  See https://github.com/Shopify/ejson
;; for more details.

;;; Default keybindings

;; C-x C-s Save and encrypt a file, generate a key if necessary
;; C-c C-e Encrypt the saved file (run on save by default)
;; C-c C-d Decrypt the file into the current buffer

;;; Variables

;; All can be set through customization group ejson

;; `ejson-binary-location' Manually specify the location of the ejson binary
;; `ejson-keystore-location' Specify an alternate location for the ejson keystore
;; `ejson-encrypt-on-save' Disable automatic encryption on save

;;; Code:

(require 'json)


(defgroup ejson nil
  "Customize variables for ejson-mode."
  :group 'js)


(defcustom ejson-binary-location nil
  "The location of the ejson binary.
If nil, binary location is determined with PATH environment variable."
  :type '(choice (const :tag "Get location from $PATH" nil)
                 (file :tag "Specify location"))
  :group 'ejson)


(defcustom ejson-keystore-location nil
  "The location of the ejson keystore.
Used to set the environment variable EJSON_KEYDIR when
calling ejson.  If nil use the ejson default directory."
  :type '(choice (const :tag "Use default location" nil)
                 (directory :tag "Specify location"))
  :group 'ejson)


(defcustom ejson-encrypt-on-save t
  "If non-nil, automatically encrypt ejson on save."
  :type 'boolean
  :group 'ejson)


(defconst ejson-output-buffer "*ejson output*"
  "Output buffer of the ejson command.")


(defun ejson--json-read-buffer ()
  "Read the read the buffer as JSON and return it formatted as an alist."
  (save-excursion
    (goto-char (point-min))
    (json-read-object)))


(defun ejson--replace-buffer (string)
  "Helper function, replace the contents of the current buffer with STRING."
  (erase-buffer)
  (insert string))


(defun ejson-run-command (args)
  "Run the ejson command with the ARGS arguments."
  (unless (if ejson-binary-location
            (file-executable-p ejson-binary-location)
          (executable-find "ejson"))
    (error "Ejson executable not found"))
  (when (get-buffer ejson-output-buffer)
    (with-current-buffer ejson-output-buffer
      (erase-buffer)))
  (when ejson-keystore-location
    (setenv "EJSON_KEYDIR" ejson-keystore-location))
  (let ((ejson-binary (or ejson-binary-location "ejson")))
    (if (eq 0 (call-process-shell-command (concat ejson-binary " " args) nil ejson-output-buffer nil))
        (with-current-buffer ejson-output-buffer
          (replace-regexp-in-string "\n$" "" (buffer-string)))
      (view-buffer-other-window ejson-output-buffer))))


(defun ejson-generate-key ()
  "Generate a new key for ejson, return the piblic key and store the private key in the ejson-keystore."
  (ejson-run-command "keygen -w"))


(defun ejson-encrypt-file (path)
  "Use ejson to encrypt a file at PATH."
  (ejson-run-command (concat "encrypt " path)))


(defun ejson-decrypt-file (path)
  "Use ejson to decrypt a file at PATH and return it as a string."
  (ejson-run-command (concat "decrypt " path)))


(defun ejson-get-file-key (path)
  "Get ejson key from file at PATH."
  (alist-get '_public_key (json-read-file path)))


(defun ejson-get-buffer-key ()
  "Get ejson key from current buffer."
  (alist-get '_public_key (ejson--json-read-buffer)))


(defun ejson-insert-key (ejson-key)
  "Insert EJSON-KEY into the current buffer."
  (ejson--replace-buffer
   (json-encode-alist (cons (cons '_public_key ejson-key)
                                           (ejson--json-read-buffer))))
    (json-pretty-print-buffer))


(defun ejson-encrypt-and-reload ()
  "Use ejson to encrypt file used by current buffer, then reload.
Does not automatically save the buffer before encryption."
  (interactive)
  (ejson-encrypt-file (buffer-file-name))
  (revert-buffer t t)
  (message "%s Encrypted" (buffer-name)))


(defun ejson-prompt-generate-key ()
  "Check if the current buffer has a public key, and prompt the user to generate one if it doesn't."
  (interactive)
  (unless (ejson-get-buffer-key)
    (if (y-or-n-p (concat (buffer-name) " has no encryption key, generate one?"))
        (ejson-insert-key (ejson-generate-key))
      (message "Cannot encrypt %s without a key" (buffer-name)))))


(defun ejson-decrypt-in-buffer ()
  "Decrypt the contents of the open ejson file, replacing the buffer's contents."
  (interactive)
  (ejson--replace-buffer (ejson-decrypt-file (buffer-file-name)))
  (message "%s Decrypted" (buffer-name)))


(defun ejson-generate-on-save()
  "Run on ejson save, choose whether to prompt for key generation or not."
  (when ejson-encrypt-on-save
    (ejson-prompt-generate-key)))

(defun ejson-encrypt-on-save ()
  "Run on ejson save, chooses whether to automatically encrypt or not."
  (when ejson-encrypt-on-save
    (ejson-encrypt-and-reload)))

(defvar ejson-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'ejson-decrypt-in-buffer)
    (define-key map (kbd "C-c C-e") #'ejson-encrypt-and-reload)
    map))

;;;###autoload
(define-derived-mode ejson-mode js-mode "Encrypted-JSON"
  "Major mode for editing ejson files"
  (add-hook 'before-save-hook #'ejson-generate-on-save nil t)
  (add-hook 'after-save-hook #'ejson-encrypt-on-save nil t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ejson\\'" . ejson-mode))

(provide 'ejson-mode)
;;; ejson-mode.el ends here
