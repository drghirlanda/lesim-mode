;;; lesim-highlight.el --- Font-lock functions for lesim-mode -*- lexical-binding: t; -*-

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

;; Font-lock functions for lesim-mode

;;; Code:

(require 'lesim-online)

(defvar font-lock-beg) ; avoid byte-compile warnings
(defvar font-lock-end)

(defvar lesim--behaviors)
(defvar lesim--stimuli)

(defun lesim--mark (beg end msg)
  "Mark the region between BEG and END as invalid.
Add MSG as tooltip."
  (put-text-property beg end 'face font-lock-warning-face)
  (put-text-property beg end 'help-echo msg))

(defun lesim--extend-region ()
  "Mark multiline comments."
  ;; count ### before start to see if we are in a comment:
  (save-excursion
    (save-match-data
      (let ((start (point))
	    (count 0)
	    (region nil)
	    (beg nil)
	    (end nil))
	(goto-char start)
	(setq region (lesim--phase-region-at-point))
	(if region
	    (progn
	      (setq beg (nth 0 region))
	      (setq end (nth 1 region)))
	  (goto-char (point-min))
	  (while (search-forward "###" start t)
	    (setq count (1+ count)))
	  (when (/= 0 (/ count 2))
	    (goto-char start)
	    (when (search-backward "###" (point-min) t)
	      (setq beg (match-beginning 0))
	    (when (search-forward "###")
	      (setq end (match-end 0))))))
	(when (and beg (< beg font-lock-beg))
	  (setq font-lock-beg beg))
	(when (and end (> end font-lock-end))
	  (setq font-lock-end end))
	(or beg end)))))

(defun lesim--match-multiline-comment (limit)
  "Highlight multiline comment between point and LIMIT."
  (let (beg end)
    (save-excursion
      (save-match-data
        (when (search-forward "###" limit t)
          (setq beg (match-beginning 0))
          (when (search-forward "###" limit t)
            (setq end (match-end 0))))))
    (when (and beg end)
      (goto-char end)
      (set-match-data (list beg end))
      end)))

(defun lesim--match-parameter (limit)
  "Highlight parameters between point and LIMIT."
  (let (mat)
    (save-excursion
      (save-match-data
        (when (length> lesim-parameter-names 0)
          (let* ((rex1 (regexp-opt lesim-parameter-names "\\(?1:"))
                 (rex2 (concat rex1 "[ \t]*=[ \t]*\\(.+?\\)\\s-*[#\n]")))
            (when (re-search-forward rex2 limit t)
              (let* ((para (match-string 1))
                     (valu (match-string 2))
                     (type (nth 2 (assoc para lesim-parameters)))
                     (beg (match-beginning 2))
                     (end (match-end 2)))
                (if (lesim-valid-p valu type)
                    (progn
                      (cond
                       ((string= para "behaviors")
                        (setq lesim--behaviors (split-string valu "," t "\\s-*")))
                       ((string= para "stimulus_elements")
                        (setq lesim--stimuli (split-string valu "," t "\\s-*"))))
                      (setq mat (list beg end beg end 0 0)))
                  (setq mat (list beg end 0 0 beg end)))))))))
    (when mat
      (set-match-data mat)
      (goto-char (nth 1 mat)))))

(defun lesim--valid-run-wholeline ()
  "Match Learning Simulator one-line @run commands."
  (let* ((other-list (split-string (thing-at-point 'line) "," t "\\s-+"))
	 (first-list (split-string (pop other-list)))
         (phases (lesim--phase-names)))
    (when (string= "@run" (pop first-list))
      (if (length= first-list 1)
          (member (nth 0 first-list) phases)
	;; 1st elt (phase label) must be a valid name:
	(when (lesim-name-p (pop first-list))
          (seq-every-p (lambda (x) (member x phases))
		       (append first-list other-list)))))))

(defun lesim--match-command (limit)
  "Match and validate lesim @ commands until LIMIT."
  (let (mat)
    (save-excursion
      (save-match-data
        (when (re-search-forward "@[[:alpha:]_]+" limit t)
          (let* ((com (match-string 0))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
            (when (member com lesim-commands)
              (cond
               ((string= com "@phase")
                ;; we check that @phase is followed by a valid
                ;; name. this can include an inherited phase name: now
                ;; we just lazily match (), but later we should check
                ;; that what is inside () is a different phase name.
                (if (re-search-forward "\\=\\s-+[[:alpha:]_][[:alnum:]_()]*"
                                       limit
                                       t)
                    (setq mat (list beg end beg end 0 0))
                  (forward-word)
                  (setq mat (list beg (point) 0 0 beg (point)))))
               ((string= com "@run")
                (if (lesim--valid-run-wholeline)
                    (setq mat (list beg end beg end 0 0))
                  (setq mat (list beg (line-end-position) 0 0 beg (line-end-position)))))
               (t
                (setq mat (list beg end beg end)))))))))
    (when mat
      (set-match-data mat)
      (goto-char (nth 1 mat)))))

(defun lesim--match-invalid-phase-stimuli (limit)
  "Highlight undeclared stimuli within LIMIT."
  (let ((line-re (concat "^\\s-*" lesim--name-re "\\s-+\\(.*?\\)\\s-*|"))
        (elem-re (concat "\\(" lesim--name-re "\\)\\[?\\([0-9a-z._]*\\)\\]?"))
        (line-beg (point))
        (case-fold-search t)
        (variables (lesim--phase-local-variables (lesim--phase-region-at-point))))
    (when (re-search-forward line-re limit t)
      (goto-char (match-beginning 1))
      (let ((line-end (line-end-position))
            (stim-end (match-end 1)))
        (while (re-search-forward elem-re stim-end t)
          (let ((elem (match-string 1))
                (elem-beg (match-beginning 1))
                (elem-end (match-end 1))
                (inte (match-string 2))
                (inte-beg (match-beginning 2))
                (inte-end (match-end 2)))
            (unless (and (length elem) (member elem lesim--stimuli))
              (lesim--mark elem-beg elem-end "Unknown stimulus element"))
            (unless (or (member inte variables)
                        (lesim-scalar-p inte))
              (lesim--mark inte-beg inte-end "Intensity must be a scalar or a local variable"))))
      (set-match-data (list line-beg line-end))
      (goto-char line-end)))))

(defun lesim--match-invalid-condition (condition)
  ""
  (let* ((operators '("and" "or" "+" "-" "/" "*" "**" "==" ">" ">=" "<" "<="))
	 (behaviors (lesim--value-of "behaviors"))
	 (phase-region (lesim--phase-region-at-point))
	 (lines (lesim--phase-lines phase-region))
	 (locals (lesim--phase-local-variables phase-region))
	 (good-stuff (append behaviors lines locals)))
    (dolist (bit (split-string (regexp-opt operators) condition))
      (cond
       ((string-match "\\`\\(choice\\|rand\\)" bit)
	nil)
       ;; behaviors, phase lines, local variables
       ((string-match (concat "\\`" lesim--name-re "\\'") bit)
	(unless (member bit good-stuff)
	  (concat bit " is not a behavior, phase line, or variable")))
       (t
	nil)))))

(defun lesim--match-invalid-action (bit)
  ""
  (cond
   ;; assignments to local phase variables
   ((string-match "\\(.+\\)=" bit)
    (if (not (lesim-name-p (match-string 1 bit)))
	(concat (match-string 1 bit) " is not a valid variable name")))
   ;; count_reset (must come before phase line)
   ((string-match (concat "count_reset(\\(" lesim--name-re "\\))") bit)
    (let ((event (match-string 1 bit)))
      (when (not (member event (append (lesim--value-of "stimulus_elements")
				      (lesim--value-of "behaviors")
				      (lesim--phase-lines (lesim--phase-region-at-point)))))
	(concat event " is not a stimulus element, behavior, or phase line")))) 
   ;; @omit_learn (must come before phase line)
   ((string= bit "@omit_learn")
    nil)
   ;; deterministic or probabilistic phase line
   ((string-match (concat "\\(" lesim--name-re "\\)\\s-*(?\\([^)]*\\))?") bit)
    (let ((phase-line (match-string 1 bit))
	  (probability (match-string 2 bit))
	  (msg nil))
      (unless (member phase-line (lesim--phase-lines (lesim--phase-region-at-point)))
	(setq msg (concat phase-line " is not a phase line name")))
      (when (and (length> probability 0)
		 (not (lesim-scalar-p probability nil nil '(0 1))))
	(setq msg (concat msg (when msg ". ") probability " is not a probability")))
      msg))
   (t
    nil)))

(defun lesim--match-invalid-behaviors-and-lines (limit)
  "Highlight undeclared behaviors and line names.
Checks between (point) and LIMIT."
    (when (re-search-forward "|\\s-*\\(.+?\\)\\s-*\\([|#\n]\\)" limit t)
      (let ((field-beg (match-beginning 1))
            (field-end (match-end 1)))
        (goto-char field-beg) ; also ensures we are in a phase
	(while (re-search-forward "\\s-*\\([^,:]+\\)\\s-*\\([,:]?\\)" field-end t)
          (let ((bit (match-string 1))
		(bit-beg (match-beginning 1))
		(bit-end (match-end 1))
                (del (match-string 2))
                (msg nil))
            (cond
	     ((string-match-p "\\(rand\\|choice\\)(" bit)
	      (goto-char bit-beg)
	      (unless (re-search-forward ")" bit-end t)
		(goto-char bit-end)
		(setq msg "No closing parenthesis")))
	     ((string= del ":")
		(setq msg (lesim--match-invalid-condition bit)))
	     (t
	      (setq msg (lesim--match-invalid-action bit))))
            (when msg
              (lesim--mark bit-beg bit-end msg))))
        (set-match-data (list field-beg field-end))
        (goto-char field-end))))

(defun lesim-highlight ()
  "Highlight a `lesim-mode' buffer."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (point-min) end (point-max)))
    (font-lock-flush beg end)
    (font-lock-ensure beg end)))

(provide 'lesim-highlight)
;;; lesim-highlight.el ends here
