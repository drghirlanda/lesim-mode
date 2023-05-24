;;; lesim-validate.el --- Check Learning Simulator syntax -*- lexical-binding: t; -*-

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

;; Function to validate Learning Simulator syntax

(defun lesim--validate-stimuli (region)
  "Highlight undeclared stimuli in REGION."
  (save-excursion
    (let ((reg-beg (nth 0 region))
          (reg-end (nth 1 region))
          (declared-stimuli (lesim--value-of "stimulus_elements"))
          (stim-re (concat "^\\s-*[^#]\\s-*" lesim--name-re "\\s-+\\([][[:alnum:],.]+\\)\\s-*|"))
          (elem-re (concat "\\(" lesim--name-re "\\)\\[?[0-9.]*\\]?,?")))
      (goto-char reg-beg)
      (while (re-search-forward stim-re reg-end t)
          (let ((stim-beg (match-beginning 1))
                (stim-end (match-end 1))
                (stim (match-string 1)))
            (goto-char stim-beg)
            (while (re-search-forward elem-re stim-end t)
              (let ((elem (match-string 1))
                    (elem-beg (match-beginning 1))
                  (elem-end (match-end 1)))
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
      (while (re-search-forward (concat "\\(" lesim--name-re "\\)") reg-end t)
        (let* ((word (match-string 1))
               (word-beg (match-beginning 1))
               (word-end (match-end 1))
               (stimuli (lesim--value-of "stimulus_elements"))
               (behaviors (lesim--value-of "behaviors"))
               (lines (lesim--phase-lines region)))
          (when (and (not (string-match "^\\s-*#" (thing-at-point 'line)))
                     (not (member word (append behaviors lines stimuli lesim-commands))))
            (let ((ov (make-overlay word-beg word-end)))
              (overlay-put ov 'face lesim-invalid-face)
              (overlay-put ov 'id 'lesim--invalid))))))))


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

(provide 'lesim-validate)
;;; lesim-validate.el ends here
