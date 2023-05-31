;;; lesim-mode.el --- Major mode for Learning Simulator scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stefano Ghirlanda

;; Author: Stefano Ghirlanda <drghirlanda@gmail.com>
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

;; Major mode for Learning Simulator scripts.  See
;; https://learningsimulator.org and
;; https://learningsimulator.readthedocs.io/en/latest/index.html for
;; detailed information about the Learning simulator.

;;; Code:

(require 'lesim-type)      ; type predicates
(require 'lesim-online)    ; retrieve info from github
(require 'lesim-locate)    ; locate script elements
(require 'lesim-align)     ; alignment
(require 'lesim-run)       ; running scripts
(require 'lesim-highlight) ; font-lock functions
(require 'lesim-custom)    ; customization

(defun lesim-forward-word ()
  "Move forward by field or word.

This function is bound to \\[lesim-forward-word]"
  (interactive)
  (unless (looking-at-p "^$")
    (lesim-align)
    (let* ((region (lesim--phase-region-at-point))
           (reg-beg (nth 0 region))
           (reg-end (nth 1 region)))
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

(defun lesim-next-error ()
  "Move to next validation error."
  (interactive)
  (let ((start (point))
        (struc (text-property-search-forward 'help-echo)))
    (if struc
        (progn
          (goto-char (prop-match-beginning struc))
          (message (prop-match-value struc)))
      (message "No more errors")
      (goto-char start))))

(defvar lesim--stimuli)
(defvar lesim--behaviors)

;;; Lesim-Mode definition

;;;###autoload
(define-derived-mode lesim-mode prog-mode "Les"
  "Major mode to edit Learning Simulator scripts.
Features in brief: syntax highlighting and validation as you
type; run script with \\[lesim-run-and-error]; insert script
template with \\[lesim-template]].  More details at
URL `https://github.com/drghirlanda/lesim'.

\\{lesim-mode-map}"
  :group 'lesim
  ;; insert template if configured and buffer is empty:
  (when (and lesim-template-auto (not (buffer-size)))
    (lesim-template))
  (setq-local lesim--stimuli (lesim--value-of "stimulus_elements"))
  (setq-local lesim--behaviors (lesim--value-of "behaviors"))
  (lesim--retrieve "parameters")
  (lesim--retrieve "keywords")
  (lesim--retrieve "mechanism_names")
  ;; keymap:
  (define-key lesim-mode-map lesim-run-key #'lesim-run-and-error)
  (define-key lesim-mode-map lesim-template-key #'lesim-template)
  (define-key lesim-mode-map [backtab] #'lesim-backward-word)
  (define-key lesim-mode-map lesim-highlight-key #'lesim-highlight)
  (define-key lesim-mode-map lesim-next-error-key #'lesim-next-error)
  (setq-local indent-line-function #'lesim-forward-word)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; search-based highlighting:
  (setq-local font-lock-support-mode nil) ; helps with multiline
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions #'lesim--extend-region)
  (setq-local lesim--font-lock-keywords
              `(
                ;; eol comments: (2 patterns to avoid ## and ### at bol)
                ("^\\(#[ \t]+.*\\)$" . (1 font-lock-comment-face t))
                ("[^#]\\(#[ \t]+.*\\)$" . (1 font-lock-comment-face t))
                ;; phase elements: (do their own highlighting)
                (lesim--match-invalid-behaviors-and-lines)
                (lesim--match-invalid-phase-stimuli)
                ;; @ commands:
                (lesim--match-command (1 font-lock-keyword-face nil t) (2 font-lock-warning-face nil t))
                ;; functions:
                (,(regexp-opt (list "count" "count_line" "count_reset" "choice" "rand") 'words) . font-lock-function-name-face)
                ;; other keywords:
                (,(regexp-opt (mapcar (lambda (x) (car x)) lesim-keywords) 'words) . font-lock-function-name-face)
                ;; phase line names:
                ("^\\s-*\\([[:alpha:]_][[:alnum:]_]+\\)\s+.*?|" . (1 font-lock-constant-face t))
                ;; parameter assignments:
                (lesim--match-parameter  (1 font-lock-type-face nil t) (2 font-lock-warning-face nil t))
                ;; multiline comments:
                (lesim--match-multiline-comment (0 font-lock-comment-face t))))
  (setq-local font-lock-defaults '(lesim--font-lock-keywords nil t))
  ;; menu
  (easy-menu-define lesim-menu lesim-mode-map
    "Menu for Learning Simulator scripts."
    '("Lesim"
      ["Run" lesim-run-and-error]
      ["Template" lesim-template]
      ["Highlight" lesim-highlight]
      ["Help" describe-mode]
      ["Customize" (lambda () (interactive) (customize-group 'lesim))]
      ["Bugs and issues" (lambda () (interactive) (browse-url "https://github.com/drghirlanda/lesim-mode/issues"))]
      ("Learning Simulator"
       ["Docs" (lambda () (interactive) (browse-url "https://learningsimulator.readthedocs.io/en/latest/index.html"))]
       ["Bugs and issues" (lambda () (interactive) (browse-url "https://github.com/learningsimulator/issues"))]))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.les\\'" . lesim-mode))

(provide 'lesim-mode)
;;; lesim-mode.el ends here
