;;; hack-time-mode.el --- Forge time   -*- lexical-binding: t ; eval: (read-only-mode 1) -*-


;; THIS FILE HAS BEEN GENERATED.

;; [[id:bdf129d9-29f3-477c-9fab-a7879bdb7e5a][inner-program]]
;; [[[[id:e83c08f0-f37a-44c3-b9e9-bf6bb7a58402][Prologue]]][prologue]]
;; Copyright 2017-2019 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: 2017
;; Version: <<hack-time-mode-version>>
;; Package-Version: 20190529.855
;; Package-Requires: ((emacs "24.4"))
;; Keywords: time, convenience
;; URL: https://gitlab.com/marcowahl/hack-time-mode
;; 
;; This file is not part of Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; 
;; Hack time by typing
;; 
;;     {M-x hack-time-mode RET}
;; 
;; - Choose a date.  E.g. enter
;;   - "+" for tomorrow 11:55AM.  11:55AM has been chosen as default.
;;   - "-1" for yesterday.  Time gets set to 11:55AM.
;;   - "-10 12:05" for 10 days ago at 12:05 PM"
;; 
;; After this action the current time is frozen to the chosen time.
;; 
;; Watch out for 'HACK-TIME-MODE' in the modeline which indicates that
;; hack-time-mode is on.
;; Turn the mode off typing
;; 
;;     {M-x hack-time-mode RET}
;; 
;; again.  The time is back to normal flow.
;; 
;; * Use cases
;; 
;; - Mark Org-todo-items done at another date.
;; - View the Org agenda as if it was tomorrow.
;; - Bulk scatter (keys BS in Org agenda) items starting at a certain
;;   date.
;; 
;; * Warning
;; 
;; Possibly some functionalities behave weird when hack-time-mode is on.
;; 
;; Watch out!
;; 
;; 'hack-time-mode' has limitted control over time.  There are time
;; sources in Emacs which can _not_ be controlled by 'hack-time-mode'.
;; 
;; * Dependencies
;; 
;; 'hack-time-mode' depends on function 'org-read-date' of
;; Orgmode.
;; 
;; * Vision
;; 
;; `hack-time-mode` gives the user full control about every aspect of
;; time in this universe and all parallel universes.


;;; Code:
;; prologue ends here
;; [[[[id:e0a33b2d-e274-4dd4-bb43-a7e324383984][To the mode]]][ht-minor-mode-config]]


;;;###autoload
(define-minor-mode hack-time-mode
  "Toggle hack-time-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `hack-time-mode'."
  :group 'hack-time
  :global t
  :lighter " HACK-TIME-MODE"
  (if hack-time-mode
      (call-interactively #'hack-time-mode-set-current-time)
    (hack-time-mode--reset)))
;; ht-minor-mode-config ends here
;; [[[[id:e62ab536-0322-4583-9994-0150a330445c][Core]]][forge-time-symbol-functions]]
; credits to Paul Eggert who introduced this to org-test.el.

(defvar hack-time-mode-at)

(defconst hack-time-mode-current-time (symbol-function 'current-time))
(defconst hack-time-mode-current-time-string (symbol-function 'current-time-string))
(defconst hack-time-mode-current-time-zone (symbol-function 'current-time-zone))
(defconst hack-time-mode-decode-time (symbol-function 'decode-time))
(defconst hack-time-mode-encode-time (symbol-function 'encode-time))
(defconst hack-time-mode-float-time (symbol-function 'float-time))
(defconst hack-time-mode-format-time-string (symbol-function 'format-time-string))
(defconst hack-time-mode-set-file-times (symbol-function 'set-file-times))
(defconst hack-time-mode-time-add (symbol-function 'time-add))
(defconst hack-time-mode-time-equal-p (symbol-function 'time-equal-p))
(defconst hack-time-mode-time-less-p (symbol-function 'time-less-p))
(defconst hack-time-mode-time-subtract (symbol-function 'time-subtract))

(defun hack-time-mode--reset ()
  (setf (symbol-function 'current-time) hack-time-mode-current-time)
  (setf (symbol-function 'current-time-string) hack-time-mode-current-time-string)
  (setf (symbol-function 'current-time-zone) hack-time-mode-current-time-zone)
  (setf (symbol-function 'decode-time) hack-time-mode-decode-time)
  (setf (symbol-function 'encode-time) hack-time-mode-encode-time)
  (setf (symbol-function 'float-time) hack-time-mode-float-time)
  (setf (symbol-function 'format-time-string) hack-time-mode-format-time-string)
  (setf (symbol-function 'set-file-times) hack-time-mode-set-file-times)
  (setf (symbol-function 'time-add) hack-time-mode-time-add)
  (setf (symbol-function 'time-equal-p) hack-time-mode-time-equal-p)
  (setf (symbol-function 'time-less-p) hack-time-mode-time-less-p)
  (setf (symbol-function 'time-subtract) hack-time-mode-time-subtract))

(defun hack-time-mode--set-time (hack-time)
  (setf hack-time-mode-at (if (stringp hack-time)
               (apply #'encode-time (org-parse-time-string hack-time))
             hack-time))
  (setf (symbol-function 'current-time) (lambda () hack-time-mode-at))
  (setf (symbol-function 'current-time-string)
        (lambda (&optional time &rest args)
          (apply hack-time-mode-current-time-string
                 (or time hack-time-mode-at) args)))
  (setf (symbol-function 'current-time-zone)
        (lambda (&optional time &rest args)
          (apply hack-time-mode-current-time-zone
                 (or time hack-time-mode-at) args)))
  (setf (symbol-function 'decode-time)
        (lambda (&optional time) (funcall hack-time-mode-decode-time
                                     (or time hack-time-mode-at))))
  (setf (symbol-function 'encode-time)
        (lambda (time &rest args)
          (apply hack-time-mode-encode-time (or time hack-time-mode-at) args)))
  (setf (symbol-function 'float-time)
        (lambda (&optional time)
          (funcall hack-time-mode-float-time (or time hack-time-mode-at))))
  (setf (symbol-function 'format-time-string)
        (lambda (format &optional time &rest args)
          (apply hack-time-mode-format-time-string
                 format (or time hack-time-mode-at) args)))
  (setf (symbol-function 'set-file-times)
        (lambda (file &optional time)
          (funcall hack-time-mode-set-file-times file (or time hack-time-mode-at))))
  (setf (symbol-function 'time-add)
        (lambda (a b) (funcall hack-time-mode-time-add
                          (or a hack-time-mode-at) (or b hack-time-mode-at))))
  (setf (symbol-function 'time-equal-p)
        (lambda (a b) (funcall hack-time-mode-time-equal-p
                          (or a hack-time-mode-at) (or b hack-time-mode-at))))
  (setf (symbol-function 'time-less-p)
        (lambda (a b) (funcall hack-time-mode-time-less-p
                          (or a hack-time-mode-at) (or b hack-time-mode-at))))
  (setf (symbol-function 'time-subtract)
        (lambda (a b) (funcall hack-time-mode-time-subtract
                               (or a hack-time-mode-at) (or b hack-time-mode-at)))))
;; forge-time-symbol-functions ends here
;; [[[[id:5febcc2d-8798-4b1b-98ae-eb0f478db53d][Commands]]][commands]]


(declare-function org-read-date "org")


;; Commands
(defun hack-time-mode-set-current-time (target-date)
  "Ask user for a date and set it as current time.
The current time does not move until call of
`hack-time-mode-current-time-back-to-normal'.

Examples for specifying the current time.

- \"-1\" to set current time to yesterday at 11:55 am.
- \"-1 12:05\" to set current time to yesterday at 12:05 pm.

See `org-read-date' for more about how to specify the current
time."
  (interactive (list (org-read-date)))
  (hack-time-mode--set-time (concat target-date " 11:55")) ; little hack to get HH:MM in case user did not specify.
  (message "%s" (format-time-string "current-time hacked to: %Y-%m-%d %H:%M"
                                    (current-time))))
;; commands ends here
;; inner-program ends here


(provide 'hack-time-mode)


;;; hack-time-mode.el ends here
