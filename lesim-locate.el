;;; lesim-mode.el --- Major mode for Learning Simulator scripts -*- lexical-binding: t; -*-

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

(defun lesim--value-of (this)
  "Scan buffer for a definition of THIS.
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
      (when (re-search-backward "^\\s-*@phase" nil t)
        (let ((phase-beg (match-beginning 0))
	      (phase-end nil))
          (forward-line) ; skip @phase line
          ;; move forward while we get phase lines:
          (while (looking-at "^#\\|\\s-+\\|\\(\\s-*[[:alpha:]_].+?|\\)")
	    (when (match-string 1)
	      (setq phase-end (line-end-position)))
            (forward-line))
          (when (>= phase-end search-beg)
            (list phase-beg phase-end)))))))

(defun lesim--phase-lines (region)
  "Return list of phase line names within REGION."
  (save-excursion
    (let ((reg-beg (nth 0 region))
          (reg-end (nth 1 region))
          (lines (list))
          (line-re (concat "^\\s-*\\(" lesim--name-re "\\)\\s-")))
      (goto-char reg-beg)
      (while (re-search-forward line-re reg-end t)
        (let ((line (match-string 1))
              (line-beg (match-beginning 1)))
          (push line lines)))
      lines)))

(defun lesim--phase-names ()
  "Return list of phase names in a Learning Simulator script."
  (save-excursion
    (save-match-data
      (let (phases)
        (goto-char (point-min))
        (while (re-search-forward "@phase\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)" nil t)
          (push (match-string 1) phases))
        phases))))
                   
(provide 'lesim-locate)
;;; lesim-locate.el ends here
