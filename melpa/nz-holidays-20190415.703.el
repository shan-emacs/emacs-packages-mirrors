;;; nz-holidays.el --- New Zealand public holidays for calendar.
;; Copyright (C) 2019 Sod Oscarfono

;; Author: Sod Oscarfono <sod@oscarfono.com>
;; URL: https://github.com/techquila/nz-holidays
;; Package-Version: 20190415.703
;; Package-Commit: afc875cf40789fa45a4a811685b0a7c4f239392f
;; Version: 0.0.4
;; Keywords: calendar

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; ========================================
;; Remove default holidays, then append NZ calendar:

;; (setq holiday-general-holidays nil)
;; (setq holiday-christian-holidays nil)
;; (setq holiday-hebrew-holidays nil)
;; (setq holiday-islamic-holidays nil)
;; (setq holiday-bahai-holidays nil)
;; (setq holiday-oriental-holidays nil)

;; (setq calendar-holidays (append holiday-local-holidays holiday-nz-holidays))

;;; Code:
;; ========================================
(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar nz-holidays--statutory
  '((holiday-fixed 1 1 "New Years Day")
    (holiday-fixed 1 2 "Day after New Years")
    (holiday-fixed 2 6 "Waitangi Day")
    (holiday-easter-etc -2 "Good Friday")
    (holiday-easter-etc +1 "Easter Monday")
    (holiday-fixed 4 25 "Anzac Day")
    (holiday-float 6 1 1 "Queens Birthday")
    (holiday-float 10 1 4 "Labour Day")
    (holiday-fixed 12 25 "Christmas Day")
    (holiday-fixed 12 26 "Boxing Day")))

(defvar nz-holidays--town-anniversary
  '((holiday-fixed 1 29 "Auckland Anniversary Day")
    (holiday-fixed 1 23 "Wellington Anniversary Day")
    (holiday-fixed 1 30 "Nelson Anniversary Day")
    (holiday-fixed 3 13 "Taranaki Anniversary Day")
    (holiday-fixed 3 20 "Otago Anniversary Day")
    (holiday-fixed 4 18 "Southland Anniversary")
    (holiday-fixed 9 25 "South Canterbury Anniversary Day")
    (holiday-fixed 10 20 "Hawkes Bay Anniversary Day")
    (holiday-fixed 10 30 "Marlborough Anniversary Day")
    (holiday-fixed 11 17 "Canterbury Anniversary Day")
    (holiday-fixed 12 04 "Westland Anniversary Day")))

(defvar holiday-nz-holidays
  (append nz-holidays--statutory  nz-holidays--town-anniversary))

(provide 'nz-holidays)
;;; nz-holidays.el ends here
