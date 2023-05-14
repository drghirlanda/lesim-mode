;;; lesim-mode.el --- Major mode for Learning Simulator scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano Ghirlanda
;; Keywords: languages, faces
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
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

;; Major mode for Learning Simulator scripts.  See
;; https://learningsimulator.org and
;; https://learningsimulator.readthedocs.io/en/latest/index.html for
;; detailed information about the Learning simulator.
;;

;;; Code:

;; This part defines variables and functions to scan for script
;; elements like stimuli, behaviors, and phases.

(defvar lesim--name
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regexp to match Learning Simulator valid names.
A valid name starts with a letter or underscore and continues
with letters, digits, and underscores.")

(defun lesim--value-of (this)
  "Scan buffer for a definition of THIS.
If found, split on \",\" and return as list."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((this-re (concat "^\s*" this "\s*[:=]\s*\\(.+\\)")))
      (when (re-search-forward this-re nil t)
        (lesim-debug "Found %s = %s" this (match-string 1))
        (split-string (match-string 1) "," t "\s*")))))

(defun lesim--phase-region-at-point ()
  "Return beginning and end char of @phase block at point.
Return nil if point is not in a @phase block."
  (save-excursion
    (let ((search-beg (point))
          (case-fold-search t))
      ;; can we find @phase backwards?
      (when (re-search-backward "^\\s-*@phase" nil t)
        (let ((phase-beg (match-beginning 0)))
          (lesim-debug "Found @phase beginning at %s" phase-beg)
          (forward-line)
	  ;; move forward while we get phase lines:
          (while (string-match "^\\(#\\|\\s-+\\|\\s-*[[:alpha:]_].+?|\\)"
			       (thing-at-point 'line))
	    (forward-line))
	  (forward-line -1)
	  (end-of-line)
	  (when (> (point) search-beg)
            (lesim-debug "Found @phase end at %s" (point))
            (list phase-beg (point))))))))

(defun lesim--phase-lines (region)
  "Return list of phase line names within REGION."
  (save-excursion
    (let ((reg-beg (nth 0 region))
          (reg-end (nth 1 region))
          (lines (list))
          (line-re (concat "^\\s-*\\(" lesim--name "\\)\\s-")))
      (lesim-debug "Looking for phase lines in %s-%s" reg-beg reg-end)
      (lesim-debug "Line regexp is %s" line-re)
      (goto-char reg-beg)
      (while (re-search-forward line-re reg-end t)
        (let ((line (match-string 1))
              (line-beg (match-beginning 1)))
          (lesim-debug "Found %s at %s" line line-beg)
          (push line lines)))
      lines)))

;; This part defines validation functions that highlight misspelled
;; stimuli, behaviors, and phase lines, and alignment functions for
;; parameter and phase blocks.

(defun lesim--validate-stimuli (region)
  "Highlight undeclared stimuli in REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (save-excursion
    (let ((reg-beg (nth 0 region))
          (reg-end (nth 1 region))
          (declared-stimuli (lesim--value-of "stimulus_elements"))
          (stim-re (concat "^\\s-*[^#]\\s-*" lesim--name "\\s-+\\([][[:alnum:],.]+\\)\\s-*|"))
          (elem-re (concat "\\(" lesim--name "\\)\\[?[0-9.]*\\]?,?")))
      (lesim-debug "Validating stimuli in %s-%s" reg-beg reg-end)
      (goto-char reg-beg)
      (while (re-search-forward stim-re reg-end t)
          (let ((stim-beg (match-beginning 1))
		(stim-end (match-end 1))
		(stim (match-string 1)))
            (lesim-debug "Found stimulus %s at %s" stim stim-beg)
            (goto-char stim-beg)
            (while (re-search-forward elem-re stim-end t)
              (let ((elem (match-string 1))
                    (elem-beg (match-beginning 1))
                  (elem-end (match-end 1)))
		(lesim-debug "Found element %s at %s" elem elem-beg)
		(unless (member elem declared-stimuli)
                  (let ((ov (make-overlay elem-beg elem-end)))
                    (overlay-put ov 'face lesim-invalid-face)
                    (overlay-put ov 'id 'lesim--invalid))))))))))

(defun lesim--validate-behaviors-and-lines (region)
  "Highlight undeclared behaviors and line names in REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (let ((reg-beg (nth 0 region))
        (reg-end (nth 1 region)))
    (save-excursion
      (goto-char reg-beg)
      (forward-line)
      (while (re-search-forward (concat "\\(" lesim--name "\\)") reg-end t)
        (let* ((word (match-string 1))
               (word-beg (match-beginning 1))
               (word-end (match-end 1))
               (stimuli (lesim--value-of "stimulus_elements"))
               (behaviors (lesim--value-of "behaviors"))
               (lines (lesim--phase-lines region)))
          (lesim-debug "Validating %s" word)
	  (when (and (not (string-match "^\\s-*#" (thing-at-point 'line)))
		     (not (member word (append behaviors lines stimuli))))
            (lesim-debug "%s is invalid" word)
            (let ((ov (make-overlay word-beg word-end)))
              (overlay-put ov 'face lesim-invalid-face)
              (overlay-put ov 'id 'lesim--invalid))))))))

(defun lesim--align-phase (region)
  "Align phase lines in REGION.
REGION must be a non-nil return value of
'lesim--phase-region-at-point'."
  (save-excursion
    (goto-char (nth 0 region))
    (forward-line) ; skip @phase line
    (let ((beg (point))
          (end (nth 1 region))
          (indent-tabs-mode nil))
      (lesim-debug "Aligning phase lines at %s-%s" beg end)
      ;; standardize spaces after | and hide resulting messages:
      (while (re-search-forward "|\\s-*" end t)
        (replace-match "| "))
      (message "")
      ;; align line id and stimulus:
      (align-regexp beg end "\\(\\s-*\\)\\s-" 1 1 nil)
      ;; align | clauses:
      (align-regexp beg end "\\(\\s-*\\)|" 1 1 t)
      ;; align comments
      (goto-char (nth 0 region))
      (while (re-search-forward "^\\s-*\\(#+\\s-*\\)" end t)
	(replace-match "# ")))))

(defun lesim--align-parameters ()
  "Align parameter block at point."
  (save-excursion
    (let ((search-start (point))
	  (assign-re  (concat "^\\s-*" lesim--name "?\\s-*=.+?$")))
      (while (re-search-backward assign-re (point-min) t))
      (let ((beg (match-beginning 0))
            (indent-tabs-mode nil))
        (while (re-search-forward assign-re (point-max) t))
        (let ((end (match-end 0)))
	  (when (and (<= search-start end) (>= search-start beg))
            ;; standardize spaces after = and ,
            (goto-char beg)
            (while (re-search-forward "\\([=,]\\)[ \t]+" end t)
              (replace-match "\\1 "))
            ;; hide replace-regexp message:
            (message "")
            (align-regexp beg
                          end
                          "\\(\\s-*\\)="
                          1
                          1
                          t)))))))

(defun lesim-validate ()
  "Check phase and parameter blocks.
If point is in a phase block, align it at | signs and highlight
undeclared stimuli, behaviors, and line names.  If point is in a
parameter block, align it at = signs."
  (interactive)
  (let* ((region (lesim--phase-region-at-point))
	 (reg-beg (nth 0 region))
         (reg-end (nth 1 region)))
    (cond (region
	   ;; validation:
           (remove-overlays reg-beg reg-end 'id 'lesim--invalid)
           (lesim--validate-stimuli region)
           (lesim--validate-behaviors-and-lines region)
	   (lesim--align-phase region))
	  (t
	   (lesim--align-parameters)))))

(defun lesim-forward-word ()
  "Move forward by field or word.

This function is bound to \\[lesim-forward-word]"
  (interactive)
  (let* ((region (lesim--phase-region-at-point))
	 (reg-beg (nth 0 region))
         (reg-end (nth 1 region)))
    (re-search-forward lesim--name (point-max) t 2)
    (goto-char (match-beginning 0))
    (when (and region (> (point) reg-end))
      (goto-char reg-beg)
      (forward-line))))

(defun lesim-backward-word ()
  "Move backward by field or word.

This function is bound to \\[lesim-backward-word]"
  (interactive)
  (let* ((region (lesim--phase-region-at-point))
	 (reg-beg (nth 0 region))
         (reg-end (nth 1 region)))
    (re-search-backward (concat "\\b" lesim--name) (point-min) t 1)
    (goto-char (match-beginning 0)))
  (when (eq ?_ (char-before))
    (backward-char)
    (lesim-backward-word)))

;; This part defines lesim keywords whose values are strings, numbers,
;; or either. Used for highlighting and in lesim-template.

(defvar lesim--strings
  '("mechanism" "behaviors" "stimulus_elements"
    "response_requirements")
  "List of Learning Simulator parameters whose values must be strings.")

(defvar lesim--numbers
  '("n_subjects" "discount" "trace")
    "List of Learning Simulator parameters whose values must be numbers.")

(defvar lesim--whatevs
  '("alpha_v" "alpha_w" "alpha_vss" "beta" "behavior_cost"
    "start_v" "start_vss" "start_w" "u" "lambda" "mu")
      "Learning Simulator parameters that can be numbers or strings.")

(defvar lesim--postprocessing
  '("cumulative" "phases" "match" "subject" "filename"
    "runlabel" "stop" "subject" "xscale" "xscale_match")
  "List of Learning Simulator postprocessing parameters.")

(defun lesim-template ()
  "Insert a bare bones Learning Simulator script template."
  (interactive)
  (goto-char (point-min))
  (insert "###\ntitle:   \nauthor:  \ndate:    "
          (format-time-string "%F")
          "\nsummary: \n###\n\n")
  (let ((here (point)))
    (dolist (par lesim--strings)
      (insert (format "%s = \n" par)))
    (dolist (par lesim--numbers)
      (insert (format "%s = \n" par)))
    (dolist (par lesim--whatevs)
      (insert (format "%s = \n" par)))
    (goto-char here)
    (lesim--align-parameters)
    (goto-char (point-min))
    (forward-line)
    (end-of-line)))

(defun lesim-find-error (error-list)
  "Locate the error string at the cdr of ERROR-LIST.
Return a list with beginning and end points."
  (save-excursion
    (if (eq major-mode "org-mode")
	(re-search-backward "#\\+begin_src\\s-+lesim")
      (goto-char (point-min)))
    (search-forward (nth 2 error-list))
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (list beg (point)))))
    
(defun lesim-error (error-list)
  "Highlight lesim error in current buffer.
ERROR-LIST is a list returned by `lesim-run'.  When ERROR-LIST is
nil (no error running the script), remove error highlights."
  (remove-overlays (point-min) (point-max) 'id 'lesim--invalid)
  (when error-list
    (let* ((regn (lesim-find-error error-list))
           (mess (nth 1 error-list))
           (ovrl (make-overlay (nth 0 regn) (nth 1 regn))))
      (overlay-put ovrl 'face lesim-invalid-face)
      (overlay-put ovrl 'id 'lesim--invalid)
      (overlay-put ovrl 'help-echo mess)
      (goto-char (nth 0 regn))
      (message "lesim: %s" mess)
      mess)))
  
(defun lesim-run (script-file)
  "Run Learning Simulator on file SCRIPT-FILE."
  (interactive)
  (let* ((script-command (concat lesim-command " " script-file))
         (script-output (shell-command-to-string script-command)))
    (when (string-match "Error on line \\([0-9]+\\): \\(.+\\)"
                        script-output)
      (let ((line (string-to-number (match-string 1 script-output)))
            (mess (match-string 0 script-output)))
	(with-temp-buffer
	  (insert-file script-file)
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (let ((beg (point)))
	    (end-of-line)
            (list line
		  mess
		  (buffer-substring beg (point)))))))))

(defun lesim-run-and-error ()
  "Run lesim on the current buffer's file.
Prompt to save unsaved changes if any."
  (interactive)
  (save-some-buffers nil `(lambda () (eq (current-buffer)
                                         ,(current-buffer))))
  (lesim-error (lesim-run (buffer-file-name))))

(defun lesim-debug (fmt &rest args)
  "Log message if `lesim-debug-flag' is not nil.
FMT and ARGS are treated like in `message'."
  (when lesim-debug-flag
    (apply #'message (concat "lesim: " fmt) args)))

;;; Customization

(defgroup lesim nil
  "Customization options for Lesim-Mode."
  :group 'emacs)

(defcustom lesim-command "lesim.py run"
  "Command to run a script in the Learning Simulator."
  :type 'string
  :group 'lesim)

(defcustom lesim-run-key (kbd "C-c C-c")
  "Keybinding to run a script in the Learning Simulator."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-validate-key (kbd "C-c C-v")
  "Keybinding to validate Learning Simulator code."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-template-key (kbd "C-c C-t")
  "Keybinding to insert a Learning Simulator script template."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-template-auto t
  "Insert a script template when opening an empty .les file."
  :type 'boolean
  :group 'lesim)

(defcustom lesim-invalid-face '(:foreground "red")
  "Face to display invalid construcs."
  :type 'face
  :group 'lesim)

(defcustom lesim-parameter-face font-lock-type-face
  "Face to display parameters."
  :type 'face
  :group 'lesim)

(defvar lesim-debug-flag nil
  "Non-nil means send debug information to *Messages*.")

;;; Lesim-Mode definition

;;;###autoload
(define-derived-mode lesim-mode prog-mode "Les"
  "Major mode to edit Learning Simulator scripts.

\\{keymap}"
  :group 'lesim
  ;; keymap:
  (define-key lesim-mode-map lesim-run-key #'lesim-run-and-error)
  (define-key lesim-mode-map lesim-template-key #'lesim-template)
  (define-key lesim-mode-map [backtab] #'lesim-backward-word)
  (define-key lesim-mode-map lesim-validate-key #'lesim-validate)
  (lesim-debug "Keymap is %s" (current-local-map))
  (setq-local indent-line-function #'lesim-forward-word)
  ;; insert template if buffer is empty:
  (when (and lesim-template-auto (not (buffer-size)))
    (lesim-template))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; search-based highlighting:
  (setq-local lesim--keywords
              `(
	        ;; end-of-line comments:
                ("\\(#.*\\)$" . (1 font-lock-comment-face))
                ;; @ keywords:
                ("\\(@[[:alpha:]_]+\\)\\>" . (1 font-lock-keyword-face))
                ;; functions:
                ("count\\(_line\\|_reset\\)?" . font-lock-function-name-face)
                ("choice\\|rand" . font-lock-function-name-face)
                ;; phase line names:
                ("^\\s-*\\([[:alpha:]_][[:alnum:]_]+\\)\s+.*?|" . (1 font-lock-constant-face))
                ;; warn about unassigned parameters:
                (,(regexp-opt (append lesim--strings lesim--numbers lesim--whatevs lesim--postprocessing) 'words) . (1 lesim-invalid-face t))
                ;; assigned parameters are cool:
                (,(concat (regexp-opt lesim--numbers 'words) "\s*[:=]\s*[[:digit:].-]") . (1 lesim-parameter-face t))
                (,(concat (regexp-opt lesim--strings 'words) "\s*[:=]\s*[[:alnum:]_]") . (1 lesim-parameter-face t))
                (,(concat (regexp-opt lesim--whatevs 'words) "\s*[:=]\s*[[:alnum:]_]") . (1 lesim-parameter-face t))
                (,(concat (regexp-opt lesim--postprocessing 'words) "\s*[:=]\s*[[:alpha:]_]") . (1 lesim-parameter-face t))))
  (setq-local font-lock-defaults '(lesim--keywords nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.les\\'" . lesim-mode))

(provide 'lesim-mode)
;;; lesim-mode.el ends here
