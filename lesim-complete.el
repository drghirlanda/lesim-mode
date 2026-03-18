;;; lesim-complete.el --- Completion and eldoc for lesim-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

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

;; Completion-at-point and eldoc support for lesim-mode.

;;; Code:

(require 'lesim-type)
(require 'lesim-locate)

(defvar lesim--stimuli)
(defvar lesim--behaviors)
(defvar lesim-parameters)
(defvar lesim-parameter-names)
(defvar lesim-keywords)
(defvar lesim-commands)
(defvar lesim-mechanisms)

;;; Context detection

(defun lesim--in-comment-p ()
  "Return non-nil if point is inside a comment."
  (save-excursion
    (let ((pos (point))
          (bol (line-beginning-position)))
      ;; single-line comment:
      (goto-char bol)
      (if (looking-at "\\s-*#")
          t
        ;; multiline ### ... ### comment:
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward "###" pos t)
            (setq count (1+ count)))
          (not (zerop (% count 2))))))))

(defun lesim--completion-context ()
  "Determine the completion context at point.
Returns a plist (:context SYMBOL :region REGION :param STRING)."
  (if (lesim--in-comment-p)
      '(:context comment)
    (let* ((bol (line-beginning-position))
           (before (buffer-substring-no-properties bol (point)))
           (region (lesim--phase-region-at-point)))
    (cond
     ;; @run line — complete phase names after the command:
     ((string-match "^\\s-*@run\\b" before)
      (list :context 'after-run))
     ;; parameter assignment — complete values after =:
     ((string-match (concat "^\\s-*\\(" lesim--name-re "\\)\\s-*=\\s-*") before)
      (let ((param (match-string 1 before)))
        (if (string= param "mechanism")
            (list :context 'after-equals-mechanism :param param)
          (list :context 'after-equals :param param))))
     ;; inside a phase block:
     (region
      (save-excursion
        (goto-char bol)
        (cond
         ;; on the @phase declaration line itself:
         ((looking-at "\\s-*@phase")
          (list :context 'command))
         ;; phase line content:
         ((string-match "|" before)
          ;; find the last | and check for : after it
          (let ((last-pipe (save-match-data
                             (string-match ".*|" before)
                             (match-end 0))))
            (if (string-match ":" (substring before last-pipe))
                ;; after : means action position (e.g., right:S1x2)
                (list :context 'phase-action :region region)
              ;; before : or no : means condition position
              (list :context 'phase-condition :region region))))
         ;; before first | — stimulus or line name position:
         ((string-match (concat "^\\s-*" lesim--name-re "\\s-+") before)
          (list :context 'phase-stimulus :region region))
         (t
          (list :context 'phase-line-start :region region)))))
     ;; at line start outside phase — parameter or command:
     ((string-match "^\\s-*@?" before)
      (list :context 'line-start))
     (t nil)))))

;;; Completion at point

(defun lesim--completion-bounds ()
  "Return (BEG . END) for the word at point."
  (let ((beg (point))
        (end (point)))
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_@")
      (setq beg (point)))
    (save-excursion
      (skip-chars-forward "a-zA-Z0-9_@")
      (setq end (point)))
    (cons beg end)))

(defun lesim-completion-at-point ()
  "Completion-at-point function for `lesim-mode'."
  (let* ((ctx (lesim--completion-context))
         (context (plist-get ctx :context))
         (region (plist-get ctx :region))
         (bounds (lesim--completion-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (candidates
          (pcase context
            ('line-start
             (append lesim-parameter-names
                     lesim-commands
                     (mapcar #'car lesim-keywords)))
            ('after-run
             (lesim--phase-names))
            ('after-equals-mechanism
             lesim-mechanisms)
            ('phase-line-start
             (lesim--phase-lines region))
            ('phase-stimulus
             lesim--stimuli)
            ('phase-action
             (append lesim--behaviors
                     (lesim--phase-lines region)
                     (lesim--phase-local-variables region)
                     '("@omit_learn")))
            ('phase-condition
             (append lesim--behaviors
                     (lesim--phase-lines region)
                     (lesim--phase-local-variables region)))
            ('command
             lesim-commands)
            (_ nil))))
    (when candidates
      (list beg end candidates :exclusive 'no))))

;;; Eldoc

(defvar lesim--command-descriptions
  '(("@phase"      . "Define a phase: @phase NAME [stop_condition]")
    ("@run"         . "Run phases: @run PHASE1, PHASE2, ...")
    ("@vplot"       . "Plot associative strengths")
    ("@vexport"     . "Export associative strengths to CSV")
    ("@wplot"       . "Plot conditional stimulus strengths")
    ("@wexport"     . "Export conditional stimulus strengths to CSV")
    ("@hexport"     . "Export history to CSV")
    ("@pplot"       . "Plot response probabilities")
    ("@pexport"     . "Export response probabilities to CSV")
    ("@nplot"       . "Plot network outputs")
    ("@nexport"     . "Export network outputs to CSV")
    ("@figure"      . "Start a new figure")
    ("@subplot"     . "Add a subplot to the current figure")
    ("@legend"      . "Add a legend to the current plot")
    ("@omit_learn"  . "Skip learning on this step"))
  "Brief descriptions of Learning Simulator @ commands.")

(defun lesim--phase-name-at (region)
  "Return the phase name declared in REGION."
  (when region
    (save-excursion
      (save-match-data
        (goto-char (nth 0 region))
        (when (looking-at
               (concat "\\s-*@phase\\s-+\\(" lesim--name-re "\\)"))
          (match-string 1))))))

(defun lesim--type-name (type-sym)
  "Return a readable name for TYPE-SYM."
  (pcase type-sym
    ('scalar "scalar")
    ('natnum "positive integer")
    ('list   "comma-separated list")
    ('dict   "dictionary")
    ('mechanism "mechanism name")
    (_ (if type-sym (symbol-name type-sym) "any"))))

(defun lesim-eldoc-function ()
  "Eldoc documentation function for `lesim-mode'."
  (let ((word (thing-at-point 'symbol t)))
    (when word
      (let ((entry nil))
        (cond
         ;; parameter:
         ((setq entry (assoc word lesim-parameters))
          (format "%s (%s, default: %s)"
                  word (lesim--type-name (nth 2 entry)) (nth 1 entry)))
         ;; keyword (non-parameter):
         ((setq entry (assoc word lesim-keywords))
          (format "%s (keyword, default: %s)" word (nth 1 entry)))
         ;; @ command:
         ((setq entry (assoc word lesim--command-descriptions))
          (cdr entry))
         ;; stimulus element:
         ((member word lesim--stimuli)
          (format "%s (stimulus element)" word))
         ;; behavior:
         ((member word lesim--behaviors)
          (format "%s (behavior)" word))
         ;; phase line:
         ((let ((region (lesim--phase-region-at-point)))
            (when (and region (member word (lesim--phase-lines region)))
              (let ((own (member word (lesim--phase-own-lines region)))
                    (phase-name (lesim--phase-name-at region)))
                (if own
                    (format "%s (phase line in %s)" word phase-name)
                  (format "%s (inherited phase line in %s)" word phase-name))))))
         ;; phase name:
         ((member word (lesim--phase-names))
          (format "%s (phase)" word))
         ;; mechanism:
         ((member word lesim-mechanisms)
          (format "%s (mechanism)" word)))))))

(provide 'lesim-complete)
;;; lesim-complete.el ends here
