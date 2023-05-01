;;; lesim-mode.el --- Major mode for Learning Simulator scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano Ghirlanda
;; Keywords: languages, faces
;; Version: 0.1
;; Package-Requires: 
;; URL: https://github.com/drghirlanda/lesim-mode

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

;; Major mode for Learning Simulator scripts. See
;; https://learningsimulator.org and
;; https://learningsimulator.readthedocs.io/en/latest/index.html for
;; detailed information about the Learning simulator.
;;
;; Credits: I started with Anders Lindgren's ini-mode as a template.

;;; Code:

(defun lesim--value-of (this)
  "Scan current buffer for a definition of THIS.
If found, split on \",\" and return as list."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((this-re (concat "^\s*" this "\s*[:=]\s*\\(.+\\)")))
      (when (re-search-forward this-re nil t)
	(split-string (match-string 1) "," t "\s*")))))

(defun lesim--phase-region-at-point ()
  "Return beginning and end char of @phase block at point.
Return nil if point is not in a @phase block."
  (save-excursion
    (let ((search-beg (point))
	  (case-fold-search t))
      ;; can we find @phase backwards?
      (when (re-search-backward "^[[:space:]]*@phase" nil t)
	  (let ((phase-beg (match-beginning 0)))
	    ;; not in @phase if empty or @.+ line until search-beg:
	    (unless (re-search-forward "^\\([[:space:]]*\\|@.+\\)$" search-beg t)
	      (next-line)
	      ;; advance while there are phase lines:
	      (while (re-search-forward "^\\([[:alpha:]][[:alnum:]_]*\\)[ \t].+|"
					nil
					t))
	      (goto-char (match-beginning 1))
	      (end-of-line)
	      (list phase-beg (point))))))))


(defun lesim--phase-lines (region)
  "Return list of phase line names within REGION."
  (save-excursion
    (goto-char (nth 0 region))
    (next-line)
    (let ((end (nth 1 region))
	  (lines (list))
	  (lines-re "^\s*\\([[:alnum:]_]+\\)\s"))
      (while (re-search-forward lines-re end t)
	(add-to-list 'lines (match-string 1)))
      lines)))

(defun lesim--phase-stimuli (region)
  "Return list of stimuli within REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (save-excursion
    (goto-char (nth 0 region))
    (next-line)
    (let ((end (nth 1 region))
	  (stim (list))
	  (stim-re "^\s*[[:alnum:]_]+\s+\\([][[:alnum:],.]+\\)"))
      (while (re-search-forward stim-re end t)
	(add-to-list 'stim (match-string 1)))
      stim)))

(defun lesim--highlight-undeclared-stimuli (region)
  "Highlight undeclared stimuli in REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (save-excursion
    (let ((beg (nth 0 region))
	  (end (nth 1 region))
	  (good-stimuli (lesim--value-of "stimulus_elements"))
	  (stim-re "^\s*[[:alnum:]_]+\s+\\([][[:alnum:],.]+\\)"))
      (remove-overlays beg end 'id 'lesim--errors)
      (goto-char beg)
      (while (re-search-forward stim-re end t)
	(let ((mbeg (match-beginning 1))
	      (mend (match-end 1))
	      (mstr (match-string 1)))
	  (unless (member mstr good-stimuli)
	    (let ((ov (make-overlay mbeg mend)))
	      (overlay-put ov 'face '(:foreground "red"))
	      (overlay-put ov 'id 'lesim--errors))))))))

(defun lesim--align-phase (region)
  "Align phase lines in REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (save-excursion
    (goto-char (nth 0 region))
    (next-line) ; skip @phase line
    (let ((beg (point))
	  (end (nth 1 region))
	  (indent-tabs-mode nil))
      ;; standardize spaces after |:
      (replace-regexp "|[ \t]+" "| " nil beg end) 
      ;; align line id and stimulus:
      (align-regexp beg end "\\(\\s-*\\)\\s-" 1 1 nil)
      ;; align | clauses:
      (align-regexp beg end "\\(\\s-*\\)|" 1 1 t))))

(defun lesim--align-parameters ()
  "Align parameter block at point."
  (save-excursion
    (let ((assign-re  "^[[:alpha:]_][[:alnum:]_]*?\\s-*=.+?$"))
      (while (re-search-backward assign-re (point-min) t))
      (let ((beg (match-beginning 0))
	    (indent-tabs-mode nil))
	(while (re-search-forward assign-re (point-max) t))
	(let ((end (match-end 0)))
	  ;; standardize spaces after = and ,
	  (replace-regexp "\\([=,]\\)[ \t]+" "\\1 " nil beg end) 
	  ;; hide replace-regexp message:
	  (message "")
	  (align-regexp beg
			end
			"\\(\\s-*\\)="
			1
			1
			t))))))

(defun lesim-validate ()
  "Check that behaviors and stimulus elements are correct."
  (interactive)
  (let* ((region (lesim--phase-region-at-point)))
    (cond (region
	   (lesim--highlight-undeclared-stimuli region)
	   (lesim--align-phase region))
	  (t
	   (lesim--align-parameters)))))

;;;###autoload
(define-derived-mode lesim-mode prog-mode "Les"
  "Major mode for editing Learning Simulator scripts."
  :group 'lesim
  (kill-all-local-variables)
  (setq-local mode-name "Les")
  ;; end-of-line comment syntax:
  (setq-local comment-start "# " comment-end "")
  ;; set up TAB magic: 
  (setq-local indent-line-function #'lesim-validate)
  ;; next 3 variables list lesim keywords whose values are strings,
  ;; numbers, or either. used for highlighting below:
  (setq-local lesim--strings
	      '("behaviors" "mechanism" "response_requirements"
		"stimulus_elements" "cumulative" "phases" "match"
		"runlabel" "stop" "subject" "xscale" "xscale_match")
	      lesim--numbers
	      '("discount" "n_subjects" "trace")
	      lesim--whatevs
	      '("alpha_v" "alpha_w" "alpha_vss" "behavior_cost"
		"beta" "filename" "lambda" "mu" "subject"
		"start_v" "start_vss" "start_w" "u"))
  ;; search-based highlighting:
  (setq-local lesim--keywords
	      `(
		;; @ keywords:
		("\\(@[[:alpha:]_]+\\)\\>" . (1 font-lock-keyword-face))
		;; functions:
		("count\\(_line\\|_reset\\)?" . font-lock-function-name-face)
		("choice\\|rand\\|and\\|or" . font-lock-function-name-face)
		;; phase line names:
		("^\\([[:alpha:]_][[:alnum:]_]+\\)\s+.*?|" . (1 font-lock-constant-face))
		;; warn about unassigned parameters:
		(,(regexp-opt (append lesim--strings lesim--numbers lesim--whatevs) 'words) . (1 font-lock-warning-face t))
		;; assigned parameters are cool: 
		(,(concat (regexp-opt lesim--numbers 'words) "\s*[:=]\s*[[:digit:]-.]") . (1 font-lock-type-face t))
		(,(concat (regexp-opt lesim--strings 'words) "\s*[:=]\s*[[:alpha:]_]") . (1 font-lock-type-face t))
		(,(concat (regexp-opt lesim--whatevs 'words) "\s*[:=]\s*[^\s]") . (1 font-lock-type-face t))
		))
  (setq-local font-lock-defaults '(lesim--keywords nil t)))

(provide 'lesim-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.les\\'" . lesim-mode))

;;; lesim-mode.el ends here
