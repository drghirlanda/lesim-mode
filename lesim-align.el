;;; lesim-align.el --- Functions to align lesim buffers -*- lexical-binding: t; -*-

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

;; Functions to align lesim buffers.

;;; Code:

(require 'lesim-type)
(require 'lesim-locate)

(defvar-local lesim--phase-col-cache nil
  "Cached phase alignment columns: (BEG TICK STIM-COL . PIPE-COLS).")

(defun lesim--compute-phase-columns (region)
  "Compute alignment columns for phase block REGION.
Returns (STIM-COL . PIPE-COLS) where STIM-COL is the column
where stimulus content starts and PIPE-COLS is a list of columns
for each | separator."
  (save-excursion
    (save-match-data
      (let ((beg (nth 0 region))
            (end (nth 1 region))
            (name-max 0)
            (field-maxes nil))
        (goto-char beg)
        (forward-line)
        (while (and (<= (point) end) (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match
                   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(.*\\)" line)
              (let* ((name (match-string 1 line))
                     (rest (match-string 2 line))
                     (fields (split-string rest "|"))
                     (i 0))
                (setq name-max (max name-max (length name)))
                (dolist (f fields)
                  (let ((w (length (string-trim f))))
                    (when (>= i (length field-maxes))
                      (setq field-maxes (append field-maxes (list 0))))
                    (when (> w (nth i field-maxes))
                      (setf (nth i field-maxes) w)))
                  (setq i (1+ i))))))
          (forward-line))
        (let* ((stim-col (1+ name-max))
               (col stim-col)
               (pipe-cols nil))
          (when field-maxes
            (setq col (+ col (pop field-maxes) 1))
            (push col pipe-cols)
            (dolist (fw field-maxes)
              (setq col (+ col 2 fw 1))
              (push col pipe-cols)))
          (cons stim-col (nreverse pipe-cols)))))))

(defun lesim--phase-columns (region)
  "Return alignment columns for phase REGION, using cache if valid."
  (let ((tick (buffer-chars-modified-tick))
        (beg (nth 0 region)))
    (if (and lesim--phase-col-cache
             (= beg (nth 0 lesim--phase-col-cache))
             (eql tick (nth 1 lesim--phase-col-cache)))
        (cddr lesim--phase-col-cache)
      (let ((cols (lesim--compute-phase-columns region)))
        (setq lesim--phase-col-cache
              (cons beg (cons tick cols)))
        cols))))

(defun lesim--align-phase-line (columns)
  "Align the current line to COLUMNS.
COLUMNS is (STIM-COL . PIPE-COLS) as from `lesim--compute-phase-columns'."
  (save-match-data
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (when (string-match
             "^\\(\\s-*\\)\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(.*\\)" line)
        (let* ((indent (match-string 1 line))
               (name (match-string 2 line))
               (rest (match-string 3 line))
               (fields (split-string rest "|"))
               (stim-col (car columns))
               (pipe-cols (cdr columns))
               (result (concat indent name))
               (remaining fields)
               (pipes pipe-cols))
          ;; pad name to stim-col (stim-col is relative to name only)
          (setq result
                (concat result
                        (make-string (max 1 (- stim-col (length name))) ? )))
          ;; add first field (stimulus content)
          (when remaining
            (setq result (concat result (string-trim (pop remaining)))))
          ;; add | + field pairs
          (while (and pipes remaining)
            (let* ((pcol (pop pipes))
                   (target (+ (length indent) pcol))
                   (padding (max 1 (- target (length result)))))
              (setq result
                    (concat result
                            (make-string padding ? )
                            "| "
                            (string-trim (pop remaining))))))
          ;; only modify buffer if line actually changed
          (unless (string= line result)
            (let ((beg (line-beginning-position))
                  (col (current-column))
                  (inhibit-modification-hooks t))
              (delete-region beg (line-end-position))
              (insert result)
              (move-to-column col)
              ;; re-fontify just this line (hooks were inhibited):
              (font-lock-flush beg (line-end-position)))))))))

(defun lesim--align-phase ()
  "Align phase block at point.
If point is not in a phase block, do nothing."
  (let ((region (lesim--phase-region-at-point)))
    (when region
      (let ((columns (lesim--phase-columns region))
            (end-marker (copy-marker (nth 1 region))))
        (save-excursion
          (goto-char (nth 0 region))
          (forward-line) ; skip @phase line
          (while (and (<= (point) end-marker) (not (eobp)))
            (lesim--align-phase-line columns)
            (forward-line)))
        (set-marker end-marker nil))
      t)))

(defun lesim--align-all-phases ()
  "Align all phase blocks in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*@phase\\b" nil t)
      (forward-line)
      (lesim--align-phase))))

(defun lesim--align-parameters ()
  "Align parameter block at point.
If point is not in a parameter block, do nothing."
  (save-excursion
    (let ((search-start (point))
          (assign-re  (concat "^\\s-*" lesim--name-re "\\s-*=.+"))
	  (indent-tabs-mode nil)
          (block-beg nil)
	  (block-end nil))
      ;; search backward, stop when we find non-comment non-empty
      ;; line that is not a parameter assignment:
      (while (re-search-backward assign-re (point-min) t)
	(setq block-beg (match-beginning 0)))
      (while (re-search-forward assign-re (point-max) t)
	(setq block-end (match-end 0)))
      (when (and (<= search-start block-end) (>= search-start block-beg))
        ;; standardize spaces after = and ,
        (goto-char block-beg)
        (while (re-search-forward "\\([=,]\\)[ \t]+" block-end t)
          (replace-match "\\1 "))
        ;; hide replace-regexp message:
        (message "")
        (align-regexp block-beg
                      block-end
                      "\\(\\s-*\\)="
                      1
                      1
                      t)))))

(defun lesim-align ()
  "Align phase and parameters blocks at point."
  (interactive)
  (or (lesim--align-phase)
      (lesim--align-parameters)))

(provide 'lesim-align)
;;; lesim-align.el ends here
