;;; swiss-holidays.el --- Swiss holidays for the calendar  -*- lexical-binding: t; -*-

;; Author: Christian Egli <christian.egli@alumni.ethz.ch>
;; Version: 0.1
;; Package-Version: 20200526.822
;; Package-Commit: 0995c9685033a09466f5b2dceb7316362bde997a
;; Keywords: calendar
;; URL: https://github.com/egli/swiss-holidays

;; This file is not part of GNU Emacs

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
;;
;; The list of official Swiss holidays. With regional holidays included.
;;
;; Installation:
;;
;;  M-x package-install RET swiss-holidays RET
;;
;; Configuration:
;;
;; To use `swiss-holidays' in your calendar
;;
;;  (setq holiday-other-holidays swiss-holidays)
;;
;; If you'd like to add regional holidays, pick additional holidays
;; from `swiss-holidays-catholic', `swiss-holidays-epiphany',
;; `swiss-holidays-st-joseph-day' or `swiss-holidays-labour-day'.
;;
;; For the canton of Zürich for example you'd use the following:
;;
;;  (setq holiday-other-holidays
;;        (append swiss-holidays swiss-holidays-labour-day))
;;
;; For the canton of Schwyz you could use the following:
;;
;;  (setq holiday-other-holidays
;;        (append swiss-holidays swiss-holidays-catholic
;;                swiss-holidays-epiphany swiss-holidays-st-joseph-day))
;;
;; The code was inspired by russian-holidays.el and
;; https://www.emacswiki.org/emacs/CalendarLocalization

;;; Code:
(eval-when-compile
  (require 'calendar)
  (require 'holidays))

; Holidays data based on
; https://de.wikipedia.org/wiki/Feiertage_in_der_Schweiz
(defvar swiss-holidays
  '((holiday-fixed 1 1 "Neujahr")
    (holiday-fixed 1 2 "Berchtoldstag")
    (holiday-easter-etc -2 "Karfreitag")
    (holiday-easter-etc 1 "Ostermontag")
    (holiday-easter-etc 39 "Auffahrt")
    (holiday-easter-etc 50 "Pfingstmontag")
    (holiday-fixed 8 1  "Bundesfeier")
    (holiday-fixed 12 25 "Weihnachtstag")
    (holiday-fixed 12 26 "Stephanstag"))
  "Official holidays in Switzerland.")

; Additional catholic holidays
(defvar swiss-holidays-catholic
  '((holiday-easter-etc 60 "Fronleichnam")
    (holiday-fixed 8 15 "Mariä Himmelfahrt")
    (holiday-fixed 11 1 "Allerheiligen")
    (holiday-fixed 12 8 "Mariä Empfängnis"))
  "Catholic holidays in Switzerland.")

; Some holidays are only valid in a few select cantons. Pick the ones
; appropriate for your location
(defvar swiss-holidays-epiphany
  '((holiday-fixed 1 6   "Heilige Drei Könige"))
  "Epiphany is only a holiday in some cantons.")

(defvar swiss-holidays-st-joseph-day
  '((holiday-fixed 3 19   "Josefstag"))
  "St Joseph's Day is only a holiday in some cantons.")

(defvar swiss-holidays-labour-day
  '((holiday-fixed 5 1   "Tag der Arbeit"))
  "Labour day is only a holiday in some cantons.")

(provide 'swiss-holidays)

;;; swiss-holidays.el ends here
