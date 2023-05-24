;;; lesim-online.el --- Retrieve Learning Simulator info from github. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano Ghirlanda
;; Keywords: languages, faces
;; Version: 0.2
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

;; Functions to retrieve and parse parameter specs from the Learning
;; Simulator repository.

;; Now we def vars and funs to retrieve lesim parametrs and keywords
;; from github to keep syntax highlighting and abbrev-table up to
;; date. The only information that may need updating is what entry
;; separates parameters from other keywords in parameters.py. As of
;; 2023-05-16, this is kw.TITLE. This matters for lesim-template,
;; which uses the list of paramaters.



;;; Code:

(require 'lesim-type)

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
  ;; what until parameter names are known:
  (sleep-for 0.5)
  (let ((count 0))
    (while (and (length= lesim-parameter-names 0) (< count 10))
      (sleep-for 1)
      (1+ count)))
  (unless (plist-get status :error)
    ;; remap parameter names if necessary:
    (dolist (p lesim-parameter-names)
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (upcase p) " = '\\(.+\\)'"))
	(let ((newp (match-string-no-properties 1)))
	  (when (not (string= p newp))
	    (delete p lesim-parameter-names)
	    (push newp lesim-parameter-names)
	    (let ((a (assoc p lesim-parameters)))
	      (delete a lesim-parameters)
	      (push (list newp (nth 1 a) (nth 2 a)) lesim-parameters))))))
    ;; get command names:
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

(provide 'lesim-online)
;;; lesim-online.el ends here
