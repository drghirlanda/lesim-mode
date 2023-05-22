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

(defvar lesim--name-re
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regexp to match Learning Simulator valid names.
A valid name starts with a letter or underscore and continues
with letters, digits, and underscores.")

(defvar lesim--scalar-re
  "[+-]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)"
  "Regexp to match Learning Simulator scalars.")

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
          (line-re (concat "^\\s-*\\(" lesim--name-re "\\)\\s-")))
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
          (stim-re (concat "^\\s-*[^#]\\s-*" lesim--name-re "\\s-+\\([][[:alnum:],.]+\\)\\s-*|"))
          (elem-re (concat "\\(" lesim--name-re "\\)\\[?[0-9.]*\\]?,?")))
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
;      (forward-line)
      (while (re-search-forward (concat "\\(" lesim--name-re "\\)") reg-end t)
        (let* ((word (match-string 1))
               (word-beg (match-beginning 1))
               (word-end (match-end 1))
               (stimuli (lesim--value-of "stimulus_elements"))
               (behaviors (lesim--value-of "behaviors"))
               (lines (lesim--phase-lines region)))
          (lesim-debug "Validating %s" word)
          (when (and (not (string-match "^\\s-*#" (thing-at-point 'line)))
                     (not (member word (append behaviors lines stimuli lesim-commands))))
            (lesim-debug "%s is invalid" word)
            (let ((ov (make-overlay word-beg word-end)))
              (overlay-put ov 'face lesim-invalid-face)
              (overlay-put ov 'id 'lesim--invalid))))))))

(defun lesim--align-phase ()
  "Align phase block at point.
If point is not in a phase block, do nothing."
  (let ((region (lesim--phase-region-at-point)))
    (when region
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
          (align-regexp beg end "\\(\\s-*\\)|" 1 1 t))))))
          ;; align comments
          ;; (goto-char (nth 0 region))
          ;; (while (re-search-forward "^\\s-*\\(#+\\s-+\\)" end t)
          ;;   (replace-match "# ")))))))

(defun lesim--align-parameters ()
  "Align parameter block at point.
If point is not in a parameter block, do nothing."
  (save-excursion
    (let ((search-start (point))
          (assign-re  (concat "^\\s-*" lesim--name-re "?\\s-*=.+?$")))
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

(defun lesim-align ()
  "Align phase and parameters blocks at point."
  (interactive)
  (or (lesim--align-phase)
      (lesim--align-parameters)))

(defun lesim-validate (region)
  "Check phase blocks (non-nil REGION) or parameter blocks (nil).
If point is in a phase block, align it at | signs and highlight
undeclared stimuli, behaviors, and line names.  If point is in a
parameter block, align it at = signs."
  (interactive)
  ;; this cond preparaes for future validation outside phase blocks
  (cond (region
         (remove-overlays (nth 0 region) (nth 1 region) 'id 'lesim--invalid)
         (lesim--validate-stimuli region)
         (lesim--validate-behaviors-and-lines region))))

(defun lesim-forward-word ()
  "Move forward by field or word.

This function is bound to \\[lesim-forward-word]"
  (interactive)
  (unless (looking-at-p "^$")
    (lesim-align)
    (let* ((region (lesim--phase-region-at-point))
           (reg-beg (nth 0 region))
           (reg-end (nth 1 region)))
      (lesim-validate region)
      (re-search-forward (concat lesim--name-re
                                 "\\|"
                                 lesim--scalar-re)
                         (point-max) t 2)
      (goto-char (match-beginning 0))
      (when (and region (> (point) reg-end))
        (goto-char reg-beg)
        (forward-line)))))

(defun lesim-backward-word ()
  "Move backward by field or word.

This function is bound to \\[lesim-backward-word]"
  (interactive)
  (lesim-validate (lesim--phase-region-at-point))
  (re-search-backward (concat "\\b\\("
                              lesim--name-re
                              "\\|"
                              lesim--scalar-re
                              "\\)")
                      (point-min) t 1)
  (goto-char (match-beginning 0))
  ;; take into account that _ is a word boundary char in the syntax
  ;; table, but a word char in a lesim name:
  (when (eq ?_ (char-before))
    (backward-char)
    (lesim-backward-word)))

;; Now we def vars and funs to retrieve lesim parametrs and keywords
;; from github to keep syntax highlighting and abbrev-table up to
;; date. The only information that may need updating is what entry
;; separates parameters from other keywords in parameters.py. As of
;; 2023-05-16, this is kw.TITLE. This matters for lesim-template,
;; which uses the list of paramaters.

(defvar lesim-url "https://raw.githubusercontent.com/learningsimulator/learningsimulator/master/"
  "Learning Simulator code base.")

(defvar lesim-parameters nil
  "Learning Simulator parameters.")

(defvar lesim-parameter-names nil
  "Learning Simulator parameter names.")

(defvar lesim-keywords nil
  "Learning Simulator keywords (not parameters or commands).")

(defvar lesim-mechanisms nil
  "Learning Simulator mechanism names.")

(defvar lesim-commands '()
  "Learning Simulator @ commands.")

;; Lesim type predicates

(defun lesim-name-p (string &optional bopen eopen)
  "Return non-nil if STRING is a valid Learning Simulator name.
If BOPEN is nil, the name must be at the start og STRING,
otherwise is can start later.  EOPEN works similarly for the end
of STRING."
  (when string
    (let ((case-fold-search t)
          (regexp lesim--name-re))
      (unless bopen (setq regexp (concat "\\`" regexp)))
      (unless eopen (setq regexp (concat regexp "\\'")))
      (string-match-p regexp string))))

(defun lesim-interval-p (string interval)
  "Return non-nil if STRING is a number within INTERVAL."
  (let ((x (string-to-number string)))
    (and (>= x (nth 0 interval)) (<= x (nth 1 interval)))))

(defvar lesim--scalar-re "[+-]?\\([0-9]+\\.?[0-9]*\\|[0-9]*\\.?[0-9]+\\)"
  "Regexp to match Learning Simulator scalars.")

(defun lesim-scalar-p (string &optional bopen eopen interval)
  "Return non-nil if STRING is a real number.
For INTERVAL, see `lesim-interval-p'.  For BOPEN and EOPEN, see
`lesim-name-p'."
  (when string
    (let ((regexp lesim--scalar-re))
      (unless bopen (setq regexp (concat "\\`" regexp)))
      (unless eopen (setq regexp (concat regexp "\\'")))
      (if (string-match-p regexp string)
          (if interval
              (lesim-interval-p string interval)
            t)))))

(defun lesim-natnum-p (string &optional bopen eopen interval)
  "Return non-nil if STRING is a positive integer.
For INTERVAL, see `lesim-interval-p'.  For BOPEN and EOPEN, see
`lesim-name-p'."
  (let ((regexp "+?[1-9][0-9]*"))
    (unless bopen (setq regexp (concat "\\`" regexp)))
    (unless eopen (setq regexp (concat regexp "\\'")))
    (when (string-match-p regexp string)
      (if interval
          (lesim-interval-p string interval)
        t))))

(defun lesim-mechanism-p (string)
  "Return non-nil if STRING is a valid mechanism name."
  (member string lesim-mechanisms))

(defun lesim-default-p (string &optional bopen eopen)
  "Return non-nil if STRING matches default:x, with x a real number.
For BOPEN and EOPEN, see `lesim-name-p'."
  (let ((regexp "default:"))
    (unless bopen (setq regexp (concat "\\`" regexp)))
    (save-match-data
      (when (string-match "default:\\s-*\\(.+?\\)" string)
        (lesim-scalar-p (match-string 1 string) t eopen)))))

(defun lesim-list-p (string)
  "Return non-nil if STRING is a comma-separated list of strings."
  (let* ((errors 0)
         (items (split-string string ",\s*" t "(?)?")))
    (while (and (equal errors 0) (> (length items) 0))
      (unless (lesim-name-p (pop items))
        (setq errors (1+ errors))))
    (equal errors 0)))

(defun lesim-type (string &optional nodict)
  "Return the Learning Simulator type of STRING.
The return value (a symbol) is natnum, scalar, name, name-scalar,
name-name-scalar, list, dict, or if STRING does not match any of
these types.  If NODICT is non-nil, the dict type is not checked
for.  (This is used in `lesim-dict-p' avoid infinite loops.)"
  (cond
   ;; mechanism:
   ((lesim-mechanism-p string)
    'mechanism)
   ;; natnum:
   ((lesim-natnum-p string)
    'natnum)
   ;; scalar:
   ((lesim-scalar-p string)
    'scalar)
   ;; name:
   ((lesim-name-p string)
    'name)
   ;; default: (must come before name:scalar)
   ((lesim-default-p string)
    'default)
   ;; name:scalar:
   ((let ((ns (split-string string "\s*:\s*")))
      (and (lesim-name-p (nth 0 ns)) (lesim-scalar-p (nth 1 ns))))
    'name-scalar)
   ;; name:list:
   ((let ((ns (split-string string "\s*:\s*")))
      (and (lesim-name-p (nth 0 ns)) (lesim-list-p (nth 1 ns))))
    'name-list)
   ;; name->name:scalar:
   ((let ((nss (split-string string "\s*->\s*")))
      (and (lesim-name-p (nth 0 nss))
           (equal 'name-scalar (lesim-type (nth 1 nss)))))
    'name-name-scalar)
   ;; list:
   ((lesim-list-p string)
    'list)
   ;; dict:
   ((and (not nodict) (lesim-dict-p string))
    'dict)))
   
(defun lesim-dict-p (string)
  "Return non-nil if STRING is a Learning Simulator dict."
  ;; A dict value can be of these forms:
  ;; scalar
  ;; name:scalar,name:scalar,...
  ;; name->name:scalar,name->name:scalar,...
  ;; name:(name,name,...)|name,name:(name,name,...)|name
  ;; The trick here is that they must be homogeneous, apart from a possible default:
  (let ((typs '())
        (elts (split-string string "," t "\\s-*")))
    (while (> (length elts) 0)
      (let ((elt (pop elts)))
        (when (string-match-p "(" elt)
          (while (and (not (string-match-p ")" elt))
                      (> (length elts) 0))
            (setq elt (concat elt (pop elts)))))
        (push (lesim-type elt t) typs)))
    (if (length= typs 1) ; must be scalar/natnum/default
        (let ((typ (nth 0 typs)))
          (or (equal 'default typ)
              (equal 'natnum typ)
              (equal 'scalar typ)))
      (setq typs (seq-remove (lambda (x) (equal x 'default)) typs))
      (let ((typ (pop typs)))
        (if (and (length= typs 0) (equal typ 'name-list))
            nil ; name-list does not have default
          (seq-every-p (lambda (x) (equal x typ)) typs))))))

(defun lesim-valid-p (value type)
  "Return non-nil if VALUE has type TYPE."
  (if type
      (funcall (intern (format "lesim-%s-p" type)) value)
    t)) ; everything is valid if type is nil

;; Here we define functions to retrieve and parse parameter specs from
;; the Learning Simulator repository

(defun lesim--retrieve (what)
  "Retrieve a list of WHAT from Learning Simulator code.
The code is looked for at `lesim-url'.  WHAT can be \"parameters\"
or \"keywords\"."
  (if (member what '("parameters" "keywords" "mechanism_names"))
      (url-retrieve (concat lesim-url what ".py")
                    (intern (concat "lesim--parse-" what))
                    nil
                    t)
    (user-error "Cannot retrieve %s" what)))

(defun lesim--parse-spec (string)
  "Return the data type described by STRING.
See `lesim-type' for a list of types."
  (let* ((case-fold-search t)
         (specs '(("\\`list of" . list)
                  ("\\`scalar or" . dict)
                  ("\\`scalar" . scalar)
                  ("\\`positive" . natnum)
                  ("\\`one of" . mechanism))))
    (cdr (assoc string
                specs
                #'string-match-p))))

(defun lesim--parse-parameters (status)
  "Parse parameters and keywords in Learning Simulator code.
This function is called by `lesim--retrieve' in a buffer
containing parameters.py.  It checks that STATUS does not contain
errors."
  (setq lesim-parameters '())
  (setq lesim-keywords '())
  (unless (plist-get status :error)
    (goto-char (point-min))
    (let ((parameter t)
          (lst nil))
      (while (re-search-forward "kw\\.\\([[:upper:]_]+\\):\\s-+\\(.+\\),\\s-+#\\s-+\\(.+\\)" nil t)
        (let ((kwd (downcase (match-string 1)))
              (def (match-string 2))
              (val (lesim--parse-spec (match-string 3))))
          (if val (setq lst val) (setq val lst))
          (dolist (str '("'" "list()" "dict()"))
            (setq def (string-replace str "" def)))
          (when (string= kwd "title") (setq parameter nil)) ;; hardcoded :(
          (if parameter
              (push (list kwd def val) lesim-parameters)
            (push (list kwd def nil) lesim-keywords)))))
    (setq lesim-parameters (reverse lesim-parameters))
    (setq lesim-parameter-names (mapcar (lambda (x) (car x)) lesim-parameters))
    (setq lesim-keywords (reverse lesim-keywords))))

(defun lesim--parse-keywords (status)
  "Parse @ keywords in Learning Simulator code.
This function is called by `lesim--retrieve' in a buffer
containing keywords.py.  It checks that STATUS does not contain
errors."
  (unless (plist-get status :error)
    (setq lesim-commands '())
    (while (re-search-forward "'\\(@[[:alpha:]]+\\)'" nil t)
      (push (match-string 1) lesim-commands))))

(defun lesim--parse-mechanism_names (status)
  "Parse mechanism names in Learning Simulator code.
This function is called by `lesim--retrieve' in a buffer
containing mechanism_names.py.  It checks that STATUS does not
contain errors."
  (unless (plist-get status :error)
    (setq lesim-mechanisms '())
    (while (re-search-forward "'\\([[:alpha:]]+\\)'" nil t)
      (push (match-string 1) lesim-mechanisms))))

(defun lesim-template ()
  "Insert a bare bones Learning Simulator script template."
  (interactive)
  (goto-char (point-min))
  (insert "# title:   \n# author:  \n# date:    "
          (format-time-string "%F")
          "\n# summary: \n\n")
  (let ((here (point)))
    (dolist (par lesim-parameters)
      (insert (format "%s = %s\n" (nth 0 par) (nth 1 par))))
    (goto-char here)
    (lesim--align-parameters)
    (goto-char (point-min))
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
      (message mess)
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
          (insert-file-contents script-file)
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

(defcustom lesim-template-key (kbd "C-c C-t")
  "Keybinding to insert a Learning Simulator script template."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-mode-key (kbd "C-c C-h")
  "Keybinding to re-highlight a Learning Simulator buffer."
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

;; Functions for syntax highlighting

(defvar font-lock-beg) ; avoid byte-compile warnings
(defvar font-lock-end)

(defun lesim--extend-region ()
  "Mark multiline comments."
  ;; count ### before start to see if we are in a comment:
  (let ((count 0)
        beg
        end)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "###" font-lock-beg t)
        (setq count (1+ count)))
      (goto-char font-lock-beg)
      ;; If in a comment, try to find the end. If found, adjust
      ;; font-lock-beg and font-lock-end:
      (unless (= 0 (/ count 2))
        (when (search-forward "###" nil t)
          (setq end (match-end 0)))
        (when (search-backward "###" nil t)
          (setq beg (match-beginning 0)))))
    (when (and beg end)
      (setq font-lock-beg beg)
      (setq font-lock-end end)
      t)))

(defun lesim--match-multiline-comment (limit)
  "Highlight multiline comment between point and LIMIT."
  (let (beg end)
    (save-excursion
      (save-match-data
        (when (search-forward "###" limit t)
          (setq beg (match-beginning 0)))
        (when (search-forward "###" limit t)
          (setq end (match-end 0)))))
    (when (and beg end)
      (goto-char end)
      (set-match-data (list beg end))
      end)))

(defun lesim--match-parameter (limit &optional invalid)
  "Font-lock matcher for Learning Simulator parameters.
Match parameter assigments between (point) and LIMIT.  If INVALID
is nil, matching syntactically invalid assignments, otherwise
match valid ones."
  (when (length> lesim-parameter-names 0)
    (let* ((rex1 (regexp-opt lesim-parameter-names "\\(?1:"))
           (rex2 (concat rex1 "[ \t]*=[ \t]*\\(.+?\\)\\s-*[#\n]")))
      (when (re-search-forward rex2 limit t)
        (let* ((para (match-string 1))
               (valu (match-string 2))
               (type (nth 2 (assoc para lesim-parameters))))
          (let ((result (save-match-data (lesim-valid-p valu type))))
            (if invalid
                (not result)
              result)))))))

(defun lesim--phase-names ()
  "Return list of phase names in a Learning Simulator script."
  (save-excursion
    (save-match-data
      (let (phases)
        (goto-char (point-min))
        (while (re-search-forward "@phase\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)" nil t)
          (push (match-string 1) phases))
        phases))))
                   

(defun lesim--valid-run-wholeline ()
  "Match Learning Simulator one-line @run commands."
  (let ((fields (split-string (thing-at-point 'line) "," t "[ \t]+"))
        (phases (lesim--phase-names)))
    ;; 1st field needs to be split again on whitespace:
    (setq fields (append (split-string (pop fields)) fields))
    (when (string= "@run" (pop fields))
      (if (length= fields 1)
          (member (nth 0 fields) phases)
        (pop fields)
        (seq-every-p (lambda (x) (member x phases)) fields)))))


(defun lesim--match-command (limit &optional invalid)
  "Match and validate lesim @ commands until LIMIT.
Return non-nil and set `match-data' when matching a valid command
or, if INVALID is non-nil, when matching an invalid command."
  (let (mat val)
    (save-excursion
      (save-match-data
        (when (re-search-forward "@[[:alpha:]_]+" limit t)
          (let* ((com (match-string 0))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
            (when (member com lesim-commands)
              (setq mat (list beg end beg end))
              (setq val t)
              (cond
               ((string= com "@phase")
                ;; we check that @phase is followed by a valid name
                (if (re-search-forward "\\=\\s-+[[:alpha:]_][[:alnum:]_()]*"
                                       (min (line-end-position) limit)
                                       t)
                    (setq mat (list beg (match-end 0) beg end))
                  (setq val nil)))
               ((string= com "@run")
                (setq mat (list beg (line-end-position) beg end))
                (unless (lesim--valid-run-wholeline)
                  ;;  (setq mat (list beg end beg end))
                  (setq val nil)))))))))
      (when (or (and val (not invalid)) (and (not val) invalid))
        (set-match-data mat)
        (goto-char (nth 1 mat)))))
  
;;; Lesim-Mode definition

;;;###autoload
(define-derived-mode lesim-mode prog-mode "Les"
  "Major mode to edit Learning Simulator scripts.
Features in brief: syntax highlighting and validation as you
type; run script with \\[lesim-run-and-error]; insert script
template with \\[lesim-template]].  More details at
`https://github.com/drghirlanda/lesim'.

\\{lesim-mode-map}"
  :group 'lesim
  ;; insert template if configured and buffer is empty:
  (when (and lesim-template-auto (not (buffer-size)))
    (lesim-template))
  (lesim--retrieve "parameters")
  (lesim--retrieve "keywords")
  (lesim--retrieve "mechanism_names")
  ;; keymap:
  (define-key lesim-mode-map lesim-run-key #'lesim-run-and-error)
  (define-key lesim-mode-map lesim-template-key #'lesim-template)
  (define-key lesim-mode-map [backtab] #'lesim-backward-word)
  (define-key lesim-mode-map lesim-mode-key #'lesim-mode)
  (lesim-debug "Keymap is %s" (current-local-map))
  (setq-local indent-line-function #'lesim-forward-word)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; search-based highlighting:
;;  (add-to-list 'font-lock-extend-region-functions #'lesim--extend-region)
  (setq-local lesim--keywords
              `(
                ;; eol comments: (2 patterns to avoid ## and ### at bol)
                ("^\\(#[ \t]+.*\\)$" . (1 font-lock-comment-face))
                ("[^#]\\(#[ \t]+.*\\)$" . (1 font-lock-comment-face))
                ;; invalid @ commands:
                ((lambda (limit) (lesim--match-command limit t)) . (0 lesim-invalid-face))
                ;; valid @ commands:
                (lesim--match-command . (1 font-lock-keyword-face))
                ;; functions:
                (,(regexp-opt (list "count" "count_line" "count_reset" "choice" "rand") 'words) . font-lock-function-name-face)
                ;; other keywords:
                (,(regexp-opt (mapcar (lambda (x) (car x)) lesim-keywords) 'words) . font-lock-function-name-face)
                ;; phase line names:
                ("^\\s-*\\([[:alpha:]_][[:alnum:]_]+\\)\s+.*?|" . (1 font-lock-constant-face))
                ;; valid parameters:
                (lesim--match-parameter . (2 lesim-parameter-face))
                ;; invalid parameters:
                ((lambda (limit) (lesim--match-parameter limit t)) . (2 lesim-invalid-face))
                ;; multiline comments:
                (lesim--match-multiline-comment (0 font-lock-comment-face t))))
  (setq-local font-lock-defaults '(lesim--keywords nil t))
  ;; menu
  (easy-menu-define lesim-menu lesim-mode-map
    "Menu for Learning Simulator scripts."
    '("Lesim"
      ["Run" lesim-run-and-error]
      ["Template" lesim-template]
      ["Highlight" lesim-mode]
      ["Help" describe-mode]
      ["Customize" (lambda () (interactive) (customize-group 'lesim))]
      ["Bugs" (lambda () (interactive) (browse-url "https://github.com/drghirlanda/lesim/issues"))]
      ("Learning Simulator"
       ["Docs" (lambda () (interactive) (browse-url "https://learningsimulator.readthedocs.io/en/latest/index.html"))]
       ["Issues" (lambda () (interactive) (browse-url "https://github.com/learningsimulator/issues"))]))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.les\\'" . lesim-mode))

(provide 'lesim-mode)
;;; lesim-mode.el ends here
