;;; metronome.el --- A simple metronome -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jonathan Gregory

;; Version: 0.1
;; Package-Version: 20200502.1748
;; Package-Commit: 18257ecdd7b3d816104e83a5f0f96e676cc9fbfc
;; Package-Requires: ((emacs "25.1"))
;; URL: https://gitlab.com/jagrg/metronome
;; Author: Jonathan Gregory <jgrg at autistici dot org>

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

;; This is a simple metronome for GNU Emacs. To install it from
;; source, add metronome.el to your load path and require it. Then M-x
;; metronome to play/pause, and C-u M-x metronome to set/play a new
;; tempo. When prompted, enter the BPM and optional beats per bar
;; preceded by space.

;; You can also set a new tempo by tapping two or more times
;; successively with the metronome-tap-tempo command, or with the
;; metronome-(in/de)crement-tempo commands.

;; For a visual reference of the tempo, beat and (optional) bar count,
;; use the metronome-display command. Press SPC to play/pause, n/p to
;; change tempo, h/s to tap/set a new tempo, and q to quit. See
;; metronome-mode for a list of commands.

;;; Code:

(require 'timer)
(require 'cl-lib)
(require 'subr-x)

(defgroup metronome nil
  "Metronome utilities."
  :group 'metronome)

(defcustom metronome-click
  (when load-file-name
    (concat (file-name-directory load-file-name) "sounds/low.wav"))
  "The filename of the low click sound."
  :type 'file)

(defcustom metronome-accent
  (when load-file-name
    (concat (file-name-directory load-file-name) "sounds/high.wav"))
  "The filename of the high click sound."
  :type 'file)

(defcustom metronome-window-size nil
  "Resize metronome window vertically by N lines.
If N is a positive integer, enlarge window. If N is a negative
 integer, shrink window. With a non-nil value, make window fill
 its frame."
  :type '(choice integer boolean))

(defvar metronome-timer nil
  "The timer of the metronome.")

(defvar metronome-tempo nil
  "The last tempo defined.")

(defvar metronome-paused-p nil
  "Whether the metronome is paused.")


;;; Metronome buffer

(defvar metronome-buffer-name "*metronome*")
(defvar metronome-display-timer nil)
(defvar metronome-oscillator 0)
(defvar metronome-beat-counter 0)
(defvar metronome-bar-counter 0)
(defvar metronome-bar-counter-timer nil)

(defvar metronome-bar-count 8)
(defvar metronome-bar-count-p nil)
(defvar metronome-bar-count-voice-p nil)

(defface metronome-tempo-face '((t :height 4.5))
  "Face for the metronome tempo.")
(defface metronome-beat-face '((t (:height 3.5)))
  "Face for the beat count.")
(defface metronome-oscillator-face '((t :height 3.5))
  "Face for the oscillator.")
(defface metronome-bar-face '((t :height 2.5))
  "Face for the bar count.")

(defvar metronome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'metronome-display)
    (define-key map "n" 'metronome-increment-tempo)
    (define-key map "p" 'metronome-decrement-tempo)
    (define-key map "h" 'metronome-tap-tempo)
    (define-key map "s" 'metronome-set-tempo)
    (define-key map "v" 'metronome-toggle-voice-count)
    (define-key map "q" 'metronome-exit)
    map)
  "Keymap for `metronome-mode'.")

(define-derived-mode metronome-mode special-mode "Metronome"
  "Major mode for displaying and controlling a metronome.
\\{metronome-mode-map}"
  (setq-local cursor-type nil))

(defun metronome-count-bars ()
  "Increment and return the bar count for each bar cycle played.
Reset the count after every `metronome-bar-count' bars. When
`metronome-bar-count-voice-p' is non-nil, also send the count
output to spd-say (text-to-speech command)."
  (cl-incf metronome-bar-counter)
  (when (and metronome-bar-count
             (> metronome-bar-counter metronome-bar-count))
    (setq metronome-bar-counter 1))
  (when metronome-bar-count-p
    (when (and metronome-bar-count-voice-p
               (executable-find "spd-say"))
      (call-process-shell-command
       (format "spd-say -r 50 '%d'" metronome-bar-counter))))
  metronome-bar-counter)

(defun metronome-get-tempo (&optional bpb)
  "Return the current BPM or optional BPB when set."
  (if bpb (or (cadr metronome-tempo) 1)
    (car metronome-tempo)))

(defun metronome-start-bar-counter ()
  "Start bar counter timer.
Do nothing if the BPB is 1."
  (let* ((bpm (metronome-get-tempo))
         (bpb (metronome-get-tempo t))
         (secs (metronome-duration bpm bpb)))
    (when (> bpb 1)
      (setq metronome-bar-counter-timer
            (run-at-time nil secs #'metronome-count-bars)))))

(defun metronome-count-beats ()
  "Count the number of beats in one bar."
  (cl-incf metronome-beat-counter)
  (when (> metronome-beat-counter
	   (metronome-get-tempo 'beats))
    (setq metronome-beat-counter 1))
  metronome-beat-counter)

(defun metronome-redisplay ()
  "Create and update the metronome buffer."
  (with-current-buffer metronome-buffer-name
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((bpm (format "%d" (metronome-get-tempo)))
           (bpb (metronome-get-tempo t))
           (count (format " %d" (metronome-count-beats))))
      (insert (propertize bpm 'face 'metronome-tempo-face))
      ;; This adds a visual emphasis on beat 1
      (set-face-attribute 'metronome-beat-face nil
                          :overline (or (= metronome-beat-counter 1)
                                        (= metronome-beat-counter 0)))
      (if (null (= bpb 1))
          (insert (propertize count 'face 'metronome-beat-face))
        (cl-incf metronome-oscillator)
        (let ((osc (if (cl-oddp metronome-oscillator)
                       " 1 " " 2 ")))
          (insert (propertize osc 'face 'metronome-oscillator-face))))
      (when-let (bar-count (and metronome-bar-count-p
                                (null (= bpb 1))
                                (format " %d" metronome-bar-counter)))
        (insert (propertize bar-count 'face 'metronome-bar-face)))
      (setq buffer-read-only t))))


;;; Core functions

(defun metronome-play-click ()
  "Play low click sound."
  (play-sound `(sound :file ,metronome-click)))

(defun metronome-play-accent ()
  "Play high click sound."
  (play-sound `(sound :file ,metronome-accent)))

(defun metronome-delays (bpm &optional bpb)
  "Return a list of seconds per beat relative to beat one.
For example, 4 BPB at 120 BPM yields (0.0 0.5 1.0 1.5)."
  (let* ((secs)
	 (delay (/ 60 (float bpm)))
	 (wait delay)
	 (now (string-to-number
	       (format-time-string "%S" (current-time)))))
    (dotimes (_i (or bpb 1))
      (push (+ now (cl-incf wait delay)) secs))
    (setq secs (mapcar (lambda (sec)
			 (- (car secs) sec))
		       secs))))

(defun metronome-duration (bpm &optional bpb)
  "Calculate the duration in seconds between cycles.
The duration is the time span of one full cycle of BPB beats per
bar at BPM beats per minute."
  (let ((secs (metronome-delays bpm (or bpb 1)))
	(delay (/ 60 (float bpm))))
    (* delay (length secs))))

(defun metronome-play-pattern (bpm &optional bpb)
  "Play metronome pattern once at BPM beats per minute.
With optional argument BPB, play a different sound on the first
of BPB beats per bar."
  (let ((secs (metronome-delays bpm (or bpb 1))))
    (dolist (i secs)
      (if (and (= i (car secs))
	       (> bpb 1))
	  (run-with-timer i nil #'metronome-play-accent)
	(run-with-timer i nil #'metronome-play-click)))))

(defvar metronome-timer-regexp
  "^metronome-\\(\\(play-\\(click\\|pattern\\)\\)\\|redisplay\\|count-bars\\)")

(defun metronome-cancel-timers ()
  "Cancel all metronome timers and reset variables."
  (dolist (timer timer-list)
    (when-let (fn (aref timer 5))
      (when-let (fn (and (symbolp fn)
                         (symbol-name fn)))
	;; Only cancel timers running metronome functions
	(when (string-match metronome-timer-regexp fn)
	  (cancel-timer timer)))))
  (setq metronome-beat-counter 0
        metronome-oscillator 0
        metronome-bar-counter 0
        metronome-bar-counter-timer nil
        metronome-display-timer nil))

(defun metronome-stop ()
  "Stop the metronome."
  (when metronome-timer
    (metronome-pause)
    (setq metronome-tempo nil)))

(defun metronome-maybe-round (bpm)
  "Round BPM up or down if outside 30-250 range."
  (cond ((< bpm 30) 30)
	((> bpm 250) 250)
	(t bpm)))

(defun metronome-start (bpm)
  "Start metronome at BPM beats per minute.
BPM can be an integer or a list of integers where the first
element is the BPM and the second element is the BPB. It can also
be a symbol, in which case prompt for a new input."
  (let ((bpb (or (car-safe (cdr-safe bpm)) 1)))
    (metronome-stop)
    (setq bpm (if (symbolp bpm)
		  (let* ((it (read-from-minibuffer "Tempo: "))
			 (it (split-string it "\s"))
			 (it (mapcar #'string-to-number it)))
		    (setq bpb (or (car-safe (cdr-safe it)) 1))
		    (car-safe it))
		(or (car-safe bpm) bpm)))
    (setq bpm (if (= bpm 0) 120
		(metronome-maybe-round bpm)))
    (setq metronome-timer
	  (let ((wait (metronome-duration bpm bpb)))
	    (run-at-time nil wait #'metronome-play-pattern bpm bpb)))
    (setq metronome-tempo (list bpm bpb)
	  metronome-paused-p nil)))

(defun metronome-pause ()
  "Pause the metronome."
  (when metronome-timer
    (metronome-cancel-timers)
    (setq metronome-paused-p t)))

(defun metronome-resume ()
  "Resume the metronome."
  (if-let (last-bpm metronome-tempo)
      (metronome-start last-bpm)
    (metronome-start 'prompt))
  (setq metronome-paused-p nil))


;;; Tap Tempo

(defvar metronome-elapsed-time nil)
(defconst metronome-cached-time (current-time))

(defun metronome-find-difference (seq)
  "Find the difference between consecutive numbers in SEQ."
  (let* ((l1 (remove (butlast seq) seq))
	 (l2 (remove (car seq) seq)))
    (cl-mapcar (lambda (x y)
		 (- (- x y)))
	       l2 l1)))

;;;###autoload
(defun metronome-tap-tempo ()
  "Tap to set new tempo when called two or more times successively."
  (interactive)
  ;; Clear tempo cache
  (unless (eq last-command 'metronome-tap-tempo)
    (setq metronome-elapsed-time nil))
  (unless metronome-paused-p
    (metronome-pause))
  (let ((message-log-max nil)
	(bpb (metronome-get-tempo 'beats))
	(last-time (car metronome-elapsed-time))
	;; Collect elapsed time since cached time
	(time (string-to-number
	       (format "%.02f" (float-time (time-since metronome-cached-time))))))
    (setq metronome-elapsed-time
	  (list time (or last-time (truncate time))))
    ;; Find the difference between TIME and LAST-TIME
    (let* ((secs (metronome-find-difference metronome-elapsed-time))
    	   (bpm (metronome-maybe-round
		 (/ 60 (float (car secs))))))
      (metronome-play-click)
      (setq metronome-tempo (list bpm bpb))
      (if (get-buffer metronome-buffer-name)
          (with-current-buffer metronome-buffer-name
            (setq buffer-read-only nil)
            (erase-buffer)
            (let ((tempo (format "%d" (metronome-get-tempo))))
              (insert (propertize tempo 'face 'metronome-tempo-face))
              (setq buffer-read-only t)))
        (message "%d" bpm)))))


;;;###autoload
(defun metronome-increment-tempo (&optional dec)
  "Increment tempo by 2.
With optional DEC argument, decrement tempo by 2."
  (interactive)
  (let ((message-log-max nil)
 	(tempo (metronome-get-tempo)))
    (setf (car metronome-tempo) (+ tempo (if dec -2 2)))
    (metronome-start metronome-tempo)
    (if (equal (current-buffer)
               (get-buffer metronome-buffer-name))
        (metronome-display nil)
      (metronome-start metronome-tempo)
      (message "%d" (metronome-get-tempo)))))

;;;###autoload
(defun metronome-decrement-tempo ()
  "Decrement tempo by 2."
  (interactive)
  (metronome-increment-tempo 'decrement))

;;;###autoload
(defun metronome-set-tempo ()
  "Set a new tempo with optional beats per bar."
  (interactive)
  (when metronome-display-timer
    (metronome-pause))
  (metronome-start 'prompt)
  (metronome nil)
  (metronome-display nil))

;;;###autoload
(defun metronome-toggle-voice-count ()
  "Toggle the voice count."
  (interactive)
  (if (executable-find "spd-say")
      (setq metronome-bar-count-p t
            metronome-bar-count-voice-p
            (if metronome-bar-count-voice-p
                nil t))
    (user-error "The spd-say program is not installed")))

;;;###autoload
(defun metronome-exit ()
  "Exit metronome buffer."
  (interactive)
  (let ((window (get-buffer-window metronome-buffer-name)))
    (quit-window)
    (when (and window
	       (null (one-window-p t)))
      (delete-window))
    (when metronome-display-timer
      (metronome-pause))))

;;;###autoload
(defun metronome-display (arg)
  "Start/pause/resume metronome and display its buffer.
With a prefix ARG, prompt for a new tempo."
  (interactive "P")
  (if metronome-display-timer
      (metronome-pause)
    (unless metronome-paused-p
      (metronome-pause))
    (call-interactively #'metronome arg)
    (let ((buffer metronome-buffer-name)
	  (window-size metronome-window-size))
      (if (and window-size
	       (symbolp window-size))
	  (pop-to-buffer-same-window buffer)
	(set-window-buffer (if (window-live-p (get-buffer-window buffer))
			       (get-buffer-window buffer)
			     (split-window-vertically window-size))
			   (get-buffer-create buffer))
	(pop-to-buffer buffer)))
    (metronome-start-bar-counter)
    (let ((secs (metronome-duration (metronome-get-tempo))))
      (setq metronome-display-timer
	    (run-at-time nil secs #'metronome-redisplay))
      (metronome-mode))))

;;;###autoload
(defun metronome (arg)
  "Start/pause/resume metronome.
With a prefix ARG, prompt for a new tempo.

When prompted, enter an integer (the BPM) or two integers
separated by space, where the second integer is the number of
beats per bar."
  (interactive "P")
  (if (or arg (null metronome-timer))
      (metronome-start 'prompt)
    (if metronome-paused-p
	(metronome-resume)
      (metronome-pause))))

(provide 'metronome)
;;; metronome.el ends here
