;;; scalariform.el --- Format Scala code with scalariform.

;; Copyright (C) 2018-2019 Wei Zhao
;; Author: zwild <judezhao@outlook.com>
;; Created: 2018-12-26T22:41:19+08:00
;; URL: https://github.com/zwild/scalariform
;; Package-Version: 20190114.215
;; Package-Commit: 478a15ccb4f825aba73262bccd3e61ce7017f64b
;; Package-Requires: ((s "1.12.0") (f "0.20.0"))
;; Version: 0.1
;; Keywords: processes scala scalariform

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; M-x: with command scalariform-format-region or scalariform-format-region-or-buffer.
;; If you want to use a property file for scalariform, you can
;; (setq scalariform-use-properties-file-p t)
;; (setq scalariform-properties-file "~/.scalariform.conf")
;; You can also customize the variable `scalariform-program-args' to past extra arguments to scalariform.

;;; Code:
(require 'f)

(defgroup scalariform nil
  "Group for scalariform."
  :group 'convenience)

(defcustom scalariform-program "scalariform"
  "Program name for scalariform."
  :type 'string
  :group 'scalariform)

(defcustom scalariform-buffer-name "*scalariform*"
  "Buffer name for scalariform."
  :type 'string
  :group 'scalariform)

(defcustom scalariform-program-args '()
  "Program name for scalariform."
  :type '(repeat string)
  :group 'scalariform)

(defcustom scalariform-use-properties-file-p nil
  "If use properties file for scalariform or not."
  :type 'boolean
  :group 'scalariform)

(defcustom scalariform-properties-file "~/.scalariform.conf"
  "Position for properties file."
  :type 'string
  :group 'scalariform)

(defun scalariform-call-process (start end buffer)
  "A process to call scalariform.
Argument START: region to start.
Argument END: region to end.
Argument BUFFER: the buffer to call this process."
  (unless (executable-find scalariform-program)
    (error (format "%s is not found." scalariform-program)))
  (let ((args '("--stdin")))
    (when scalariform-use-properties-file-p
      (push (format "-p=%s" (f-long scalariform-properties-file)) args))

    (with-current-buffer (current-buffer)
      (apply 'call-process-region start end scalariform-program nil buffer nil args))))

;;;###autoload
(defun scalariform-format-region (start end)
  "Format the region.
Argument START: region to start.
Argument END: region to end."
  (interactive "r")
  (let* ((original-point (point))
         (buffer scalariform-buffer-name)
         (process-res (scalariform-call-process start end buffer))
         (output (with-current-buffer buffer (buffer-string))))
    (kill-buffer buffer)
    (if (eq process-res 0)
        (progn
          (delete-region start end)
          (insert output)
          (goto-char original-point)
          (message "Region formatted."))
      (error (s-chomp output)))))

;;;###autoload
(defun scalariform-format-region-or-buffer ()
  "Format the region or buffer."
  (interactive)
  (if (region-active-p)
      (scalariform-format-region (region-beginning) (region-end))
    (scalariform-format-region (point-min) (point-max))
    (message "Buffer formatted.")))

(provide 'scalariform)

;;; scalariform.el ends here
