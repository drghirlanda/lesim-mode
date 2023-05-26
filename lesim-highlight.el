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

(defun lesim--extend-region ()
  "Mark multiline comments."
  ;; count ### before start to see if we are in a comment:
  (let (beg end)
    (when (search-backward "###" nil t)
      (setq beg (match-beginning 0)))
    (when (search-forward "###" nil t)
      (setq end (match-end 0)))
    (if (< beg font-lock-beg)
	(setq font-lock-beg beg)
      (setq beg nil))
    (if (> end font-lock-end)
	(setq font-lock-end end)
      (setq end nil))
    (or beg end)))

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
  "Font-lock matcher for Learning Simulator parameters."
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
  (let ((fields (split-string (thing-at-point 'line) "," t "[ \t]+"))
        (phases (lesim--phase-names)))
    ;; 1st field needs to be split again on whitespace:
    (setq fields (append (split-string (pop fields)) fields))
    (when (string= "@run" (pop fields))
      (if (length= fields 1)
          (member (nth 0 fields) phases)
        (pop fields) ; label: can be anything
        (seq-every-p (lambda (x) (member x phases)) fields)))))

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
                ;; we check that @phase is followed by a valid name,
                ;; which can include an inherited phase name
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
  (let ((line-re (concat "^\\s-*" lesim--name-re "\\s-+\\(.+?\\)\\s-*|"))
	(elem-re (concat "\\(" lesim--name-re "\\)\\[?[0-9.]*\\]?"))
	(line-beg (point)))
    (when (re-search-forward line-re limit t)
      (goto-char (match-beginning 1))
      (let ((line-end (line-end-position))
	    (stim-end (match-end 1)))
	(while (re-search-forward elem-re stim-end t)
          (let ((elem (match-string 1))
                (elem-beg (match-beginning 1))
                (elem-end (match-end 1)))
            (unless (member elem lesim--stimuli)
	      (put-text-property elem-beg elem-end 'face font-lock-warning-face))))
	(set-match-data (list line-beg line-end))
	(goto-char line-end)))))

(defun lesim--match-invalid-behaviors-and-lines (limit)
  "Highlight undeclared behaviors and line names."
    (when (re-search-forward "|\\s-*\\(.+?\\)\\s-*\\([|#\n]\\)" limit t)
      (let ((field-beg (match-beginning 1))
	    (field-end (match-end 1))
	    (lines nil)) ; set later, we might be too far now
	(goto-char field-beg) ; also ensures we are in a phase
	(setq lines (lesim--phase-lines (lesim--phase-region-at-point)))
	(while (re-search-forward "\\s-*\\([^(,:]+\\)\\s-*\\([().:,0-9]*\\)" field-end t)
	  (let ((invalid nil)
		(bit (match-string 1))
		(del (match-string 2)))
	    (if (string= del ":")
		(unless (member bit lesim--behaviors)
		  (setq invalid t))
	      (if (not (string-match-p "=" bit))
		  (unless (member bit lines)
		    (setq invalid t))))
	    (when invalid
	      (put-text-property (match-beginning 1) (match-end 1) 'face font-lock-warning-face))))
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
