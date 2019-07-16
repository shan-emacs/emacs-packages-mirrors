;;; transcribe.el --- Package for audio transcriptions

;; Copyright 2014-2015  Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 1.0.2

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

;; REQUIRES:
;; -----------------------------
;; This module works without any requires, but in order to use the audio 
;; functions, you need to install the emacs package "emms", by Joe Drew, 
;; and the external program "mpg321", by Jorgen Schafer and Ulrik Jensen,
;; both under GPL licenses.
;;
;; USAGE:
;; -------------------------
;; Transcribe is a tool to make audio transcriptions. It allows the 
;; transcriber to control the audio easily while typing, as well as 
;; automate the insertion of xml tags, in case the transcription protocol 
;; include them.
;; The analyse function will search for a specific structure 
;; of episodes that can be automatically added with the macro NewEpisode. 
;; The function expects the speech acts to be transcribed inside a turn xml 
;; tag with the identifier of the speaker with optional move attribute.
;; Each speech act is spected inside a <l1> or <l2> tag, depending 
;; on the language used by the person. The attributes expected are the 
;; number of clauses that form the utterance, the number of errors the 
;; transcriber observes, and the function of the speech act. The parser will
;; work even if some attributes are missing.
;; 
;; 
;; AUDIO COMMANDS
;; ------------------------------
;;     C-x C-p ------> Play audio file. You will be prompted for the name 
;;                     of the file. The recommended format is mp2.
;;     <f5> ---------> Pause or play audio.
;;     C-x <right> --> seek audio 10 seconds forward.
;;     C-x <left> --->seek audio 10 seconds backward.
;;     <f8> ---------> seek interactively: positive seconds go forward and 
;;                       negative seconds go backward
;;
;; XML TAGGING COMMANDS
;; --------------------------------------------------
;;     C-x C-n ------> Create new episode structure. This is useful in case your 
;;                 xml file structure requires it.
;;     <f2> ---------> Interactively insert a function attribute in a speech act 
;;                 (l1 or l2) tag.
;;     <f3> ---------> Interactively insert a move attribute in a turn (person) tag
;;     <f4> ---------> Interactively insert an attribute (any kind)
;;     <f9> ---------> Insert turn (person) tag. Inserts a move attribute.
;;     <f10> --------> Insert a custom tag. Edit the function to adapt to your needs.
;;     <f11> --------> Insert speech act tag in L1, with clauses, errors and function
;;                     attributes.
;;     <f12> --------> Insert speech act tag in L2, with clauses, errors and function
;;                     attributes.
;;
;; AUTOMATIC PARSING
;; -----------------------------------------------------
;;     C-x C-a ------> Analyses the text for measurments of performance.

;;; Code:

(if t (require 'emms-setup))
;(require 'emms-player-mpd)
;(setq emms-player-mpd-server-name "localhost")
;(setq emms-player-mpd-server-port "6600")

(emms-standard)
(emms-default-players)
(if t (require 'emms-player-mpg321-remote))
(defvar emms-player-list)
(push 'emms-player-mpg321-remote emms-player-list)

(if t (require 'emms-mode-line))
(emms-mode-line 1)
(if t (require 'emms-playing-time))
(emms-playing-time 1)

(defvar transcribe-function-list '("initiating" "responding" "control" "expresive" "interpersonal"))
(defvar transcribe-move-list '("initiation" "response" "follow-up"))
(defvar transcribe-attribute-list '("clauses" "errors" "function" "move"))
;(append transcribe-attribute-list transcribe-function-list transcribe-move-list)

(defun transcribe-analyze-episode (episode person)
  "This calls the external python package analyze_episodes2.py. The new 
   function transcribe-analyze implements its role now."
  (interactive "sepisode: \nsperson:")
  (shell-command (concat (expand-file-name  "analyze_episodes2.py") 
                  " -e " episode " -p " person " -i " buffer-file-name )))

(defun transcribe-analyze (episodenumber personid)
  "Extract from a given episode and person the number of asunits per 
   second produced, and the number of clauses per asunits, for L2 and L1."
  (interactive "sepisodenumber: \nspersonid:")
  (let* ((interventionsl2 '())
     (interventionsl1 '())
     (xml (xml-parse-region (point-min) (point-max)))
     (results (car xml))
     (episodes (xml-get-children results 'episode))
     (asunitsl2 0.0000)
     (asunitsl1 0.0000)
     (shifts nil)
     (clausesl1 0.0000)
     (errorsl1 0.0000)
     (clausesl2 0.0000)
     (errorsl2 0.0000)
     (duration nil)
     (number nil))
         
     (dolist (episode episodes)
       (let*((numbernode (xml-get-children episode 'number)))
                 
         (setq number (nth 2 (car numbernode)))
         (when (equal episodenumber number)
           (let* ((durationnode (xml-get-children episode 'duration))
             (transcription (xml-get-children episode 'transcription)))
                       
             (setq duration (nth 2 (car durationnode)))
             (dolist (turn transcription)
               (let* ((interventionnode (xml-get-children turn 
                 (intern personid))))
                 
                 (dolist (intervention interventionnode)
                   (let* ((l2node (xml-get-children intervention 'l2))
                     (l1node (xml-get-children intervention 'l1)))
                       
                     (dolist (l2turn l2node)
                       (let* ((l2 (nth 2 l2turn))
                          (clausesl2node (nth 1 l2turn))
                          (clausesl2nodeinc (cdr (car clausesl2node))))
                          
                          (when (not (equal clausesl2node nil))
                            (setq clausesl2 (+ clausesl2 (string-to-number 
                             clausesl2nodeinc))))
                          (when (not (equal l2 nil)) 
                            (add-to-list 'interventionsl2 l2) 
                            (setq asunitsl2 (1+ asunitsl2)))))
                     (dolist (l1turn l1node)
                       (let*((l1 (nth 2 l1turn))
                         (clausesl1node (nth 1 l1turn))
                         (clausesl1nodeinc (cdr (car clausesl1node))))
                         
                         (when (not (equal clausesl1node nil))
                           (setq clausesl1 (+ clausesl1 (string-to-number 
                              clausesl1nodeinc))))
                         (when (not (equal l1 nil)) 
                           (add-to-list 'interventionsl1 l1) 
                           (setq asunitsl1 (1+ asunitsl1)))))))))))))
  (reverse interventionsl2)
  (reverse interventionsl1)
  ;(print interventions) ;uncomment to display all the interventions on screen
  (let((asunitspersecondl2 (/ asunitsl2 (string-to-number duration)))
    (clausesperasunitl2 (/ clausesl2 asunitsl2))
    (asunitspersecondl1 (/ asunitsl1 (string-to-number duration)))
    (clausesperasunitl1 (/ clausesl1 asunitsl1)))
  
    (princ (format "episode: %s, duration: %s, person: %s\n" episodenumber duration personid))
    (princ (format "L2(Asunits/second): %s, L2(clauses/Asunit): %s, L1(Asunits/second): %s" 
          asunitspersecondl2 clausesperasunitl2 asunitspersecondl1)))))

(defun transcribe-xml-tag-person (xmltag)
  "This function allows the automatic insetion of a speaker xml tag and places the cursor."
  (interactive "stag:")
  (insert (format "<%s move=\"\"></%s>" xmltag xmltag))
  (backward-char 3)
  (backward-char (string-width xmltag)))

(defun transcribe-xml-tag (xmltag)
  "This function allows the automatic insetion of a custom xml tag and places the cursor."
  (interactive "stag:")
  (insert (format "<%s></%s>" xmltag xmltag))
  (backward-char 3)
  (backward-char (string-width xmltag)))

(defun transcribe-region-xml-tag (xmltag)
  "This function encapsulates the marked region in the given tag."
  (interactive "stag:")
  (let ((beginning (region-beginning))
       (end (region-end)))
  (goto-char beginning)
  (insert (format "<%s>" xmltag))
  (goto-char end)
  (insert (format "</%s>" xmltag))))

(defun transcribe-add-attribute (att val)
  "Adds a xml attribute at cursor with the name and value specified (autocompletion possible)"
  (interactive (list(completing-read "attibute name:" transcribe-attribute-list)(read-string "value:"))) 
  (insert (format "%s=\"%s\"" att val)))

(defun transcribe-add-attribute-function (val)
  "Adds the xml attribute 'function' at cursor with the name specified (autocompletion possible)"
  (interactive (list(completing-read "function name:" transcribe-function-list))) 
  (insert (format "function=\"%s\"" val)))

(defun transcribe-add-attribute-move (val)
  "Adds the xml attribute 'move' at cursor with the name specified (autocompletion possible"
  (interactive (list(completing-read "move name:" transcribe-move-list))) 
  (insert (format "move=\"%s\"" val)))

(defun transcribe-xml-tag-l1 ()
  "Inserts a l1 tag and places the cursor"
  (interactive)
  (insert "<l1 clauses=\"1\" errors=\"0\" function=\"\"></l1>")
  (backward-char 5))

(defun transcribe-xml-tag-l2 ()
  "Inserts a l2 tag and places the cursor"
  (interactive)
  (insert "<l2 clauses=\"1\" errors=\"0\" function=\"\"></l2>")
  (backward-char 5))

(defun transcribe-xml-tag-break (xmltag)
  "This function breaks an unit into two. That is, insert a closing and an opening equal tags"
  (interactive "stag:")
  (insert (format "</%s><%s clauses=\"1\" errors=\"0\" function=\"\">" xmltag xmltag)))

(defun transcribe-display-audio-info ()
  (interactive)
  (emms-player-mpg321-remote-proc)
  (shell-command "/usr/bin/mpg321 -R - &"))


(fset 'NewEpisode
      "<episode>\n<number>DATE-NUMBER</number>\n<duration></duration>\n<comment></comment>\n<subject>Subject (level)</subject>\n<task>\n\t<role>low or high</role>\n<context>low or high</context>\n<demand>low or high</demand>\r</task>\n<auxiliar>Yes/no</auxiliar>\n<transcription>\n</transcription>\n</episode>");Inserts a new episode structure

;;;###autoload
(define-minor-mode transcribe-mode
 "Toggle transcribe-mode"
  nil
  " Trans"
  '(([?\C-x ?\C-p] . emms-play-file)
    ([?\C-x ?\C-a] . transcribe-analyze)
    ([?\C-x ?\C-n] . NewEpisode)
    ([?\C-x down] . emms-stop)
    ([?\C-x right] . emms-seek-forward)
    ([?\C-x left] . emms-seek-backward)
    
    ([f2] . transcribe-add-attribute-function)
    ([f3] . transcribe-add-attribute-move)
    ([f4] . transcribe-add-attribute)
    
    ([f5] . emms-pause)
    ([f8] . emms-seek)
   
    ([f9] . transcribe-xml-tag)
    ([f10] . transcribe-xml-tag-person)
    ([f11] . transcribe-xml-tag-l1)
    ([f12] . transcribe-xml-tag-l2))
)

;;;; ChangeLog:

;; 2015-12-13  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	/transcribe/transcribe.el: Add some functions, fix bugs
;; 
;; 2015-12-13  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	/transcribe/transcribe.el: Fix bug
;; 
;; 2015-12-13  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	/transcribe/transcribe.el: edit comments, add new functions
;; 
;; 2015-12-09  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	/packages/transcribe.el: Fix global variables
;; 
;; 2015-12-08  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	/packages/transcribe: Fix variables
;; 
;; 2015-12-08  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/transcribe.el: Add native discourse analysis in elisp
;; 
;; 2015-12-02  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Merge branch 'master' of git.sv.gnu.org:/srv/git/emacs/elpa:
;; 	transcribe.el update
;; 
;; 2015-12-02  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* /transcribe/transcribe.el: Fix unworking keys
;; 
;; 2015-12-01  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe/transcribe.el: Remove manual change log
;; 
;; 2015-12-01  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/transcribe.el: Manually changed Changelog
;; 
;; 2015-11-30  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Added minor-mode function as suggested
;; 
;; 2015-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe/transcribe.el: Allow compilation without emms
;; 
;; 2015-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe.el: Add `provide' statement
;; 
;; 2015-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe.el: Fix up formatting and copyright
;; 
;; 2015-11-29  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Added some usage information
;; 
;; 2015-11-29  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Package transcribe added
;; 


(provide 'transcribe)

;;; transcribe.el ends here
