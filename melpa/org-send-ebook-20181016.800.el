;;; org-send-ebook.el --- Send org link file to ebook reader.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (seq "2.20"))
;; Package-Version: 20181016.800
;; Package-X-Original-Version: 0.1
;; Keywords: org link ebook kindle epub mobi
;; homepage: https://github.com/stardiviner/org-send-ebook

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; [M-x org-send-ebook]

;;; Code:

(require 'cl-lib) ; for `cl-case'
(require 'seq) ; for `seq-filter'
(require 'org)
(require 'dash) ; for `->>'

(defgroup org-send-ebook nil
  "Send org-mode ebook file: link to external devices with corresponding formats.."
  :prefix "org-send-ebook-"
  :group 'org)

(defvar org-send-ebook-target-format nil)

(defcustom org-send-ebook-default-format ".epub"
  "The default target device format used to send."
  :type 'string
  :group 'org-send-ebook)

;;;###autoload
(defun org-send-ebook--read-device-info ()
  "Detect plugged in device."
  ;; TODO: improve this function.
  (if (seq-filter
       (lambda (usb)
         ;; if USB contains "Amazon Kindle" string.
         (string-match (rx "Amazon Kindle") usb)
         )
       ;; read USB devices info
       (split-string (shell-command-to-string "lsusb") "\n"))
      "kindle"
    (progn
      (warn "unknown device, can't detect device correctly, please report to https://github.com/stardiviner/org-send-ebook/issues")
      "unknown")))

;;;###autoload
(defun org-send-ebook--detect-format ()
  "Detect plugged in device's ebook format."
  (cl-case (intern (org-send-ebook--read-device-info))
    ('kindle ".mobi")
    (t org-send-ebook-default-format)))

;;;###autoload
(defun org-send-ebook--mount-path ()
  "Get Linux general mount path."
  (directory-file-name
   (concat "/run/media/" (getenv "USER"))))

;;;###autoload
(defun org-send-ebook--detect-directory ()
  "Detect plugged in device directory of saving ebook."
  (cl-case (intern (org-send-ebook--read-device-info))
    ('kindle
     (expand-file-name
      (concat (org-send-ebook--mount-path) "/Kindle/documents/")))
    (t
     (read-directory-name "Send to device directory: "))))

(defun org-send-ebook--strim-special-chars (filename)
  "strim some special characters in filename which does not
    supported by Kindle filesystem."
  (->> filename
       (replace-regexp-in-string ":" "-")))

;;;###autoload
(defun org-send-ebook ()
  "Send `org-mode' ebook file: link to external devices with corresponding formats."
  (interactive)
  ;; get the file path under org-mode link.
  (when (string= (org-element-property :type (org-element-context)) "file")
    (let* ((source-file (expand-file-name (org-link-unescape (org-element-property :path (org-element-context)))))
           (target-file-name (org-send-ebook--strim-special-chars
                              (file-name-nondirectory
                               (concat (file-name-sans-extension source-file) (org-send-ebook--detect-format)))))
           (default-directory (temporary-file-directory))
           (target-file (concat (temporary-file-directory) target-file-name))
           (device-directory (org-send-ebook--detect-directory)))
      ;; device already has this file.
      (unless (or (file-exists-p (concat device-directory target-file-name))
                  (file-exists-p
                   (concat
                    device-directory
                    (file-name-sans-extension target-file-name) ".azw3")))
        ;; converted temp file exist, when previous convert failed.
        (if (file-exists-p target-file)
            (progn
              (message "org-send-ebook: converted temp target file exist.")
              (copy-file target-file device-directory)
              (message (format "org-send-ebook: %s finished." target-file-name)))
          ;; if source file format is matched for device, copy directly.
          (if (or (string= (file-name-extension source-file)
                           (file-name-extension target-file-name))
                  ;; if source file is .azw3, also suitable for Kindle.
                  (if (equal (org-send-ebook--read-device-info) "kindle")
                      (string= (file-name-extension source-file) "azw3")))
              (progn
                (copy-file source-file device-directory)
                (message (format "org-send-ebook: %s finished." target-file-name)))
            ;; convert ebook to device compatible format.
            (message (format "org-send-ebook: %s started..." target-file-name))

            (async-shell-command
             (concat "ebook-convert"
                     " " (shell-quote-argument source-file)
                     " " (shell-quote-argument target-file) " ; "
                     "cp"
                     " " (shell-quote-argument target-file)
                     " " device-directory)
             (format "*org-send-ebook: %s*" target-file-name)
             (format "*Error org-send-ebook: %s*" target-file-name))

            ;; FIXME:
            ;; (make-process
            ;;  :name (format "org-send-ebook: %s" target-file-name)
            ;;  :command (list
            ;;            "ebook-convert" " "
            ;;            (shell-quote-argument source-file) " "
            ;;            (shell-quote-argument target-file))
            ;;  :sentinel (lambda (proc event)
            ;;              ;; send converted file to device
            ;;              (if (string= event "finished\n")
            ;;                  (progn
            ;;                    (copy-file target-file device-directory)
            ;;                    (message "org-send-ebook: %s finished." target-file-name))
            ;;                (user-error "Error on process: org-send-ebook.\n%S" event)))
            ;;  :buffer (format "*org-send-ebook: %s*" target-file-name))
            
            ))))))



(provide 'org-send-ebook)

;;; org-send-ebook.el ends here
