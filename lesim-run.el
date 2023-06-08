;;; lesim-run.el --- Functions to run Learning Simulator scripts -*- lexical-binding: t; -*-

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

;; Functions to run Learning Simulator scripts

;;; Code:

(defun lesim-find-error (error-list)
  "Locate the error string at the cdr of ERROR-LIST.
Return a list with beginning and end points."
  (save-excursion
    (if (eq major-mode "org-mode")
        (re-search-backward "#\\+begin_src\\s-+lesim")
      (goto-char (point-min)))
    (search-forward (nth 2 error-list))
    (list (line-beginning-position) (line-end-position))))
    
(defun lesim-error (error-list)
  "Highlight lesim error in current buffer.
ERROR-LIST is a list returned by `lesim-run'.  When ERROR-LIST is
nil (no error running the script), remove error highlights."
  (remove-overlays (point-min) (point-max) 'id 'lesim-error)
  (when error-list
    (let* ((regn (lesim-find-error error-list))
           (mess (nth 1 error-list))
           (ovrl (make-overlay (nth 0 regn) (nth 1 regn))))
      (overlay-put ovrl 'face font-lock-warning-face)
      (overlay-put ovrl 'id 'lesim-error)
      (overlay-put ovrl 'help-echo mess)
      (goto-char (nth 0 regn))
      (message mess)
      mess)))
  
(defun lesim-run (script-file)
  "Run Learning Simulator on file SCRIPT-FILE."
  (interactive)
  (message "Running script...")
  (let* ((proc-buf (generate-new-buffer (concat " *lesim run: " (buffer-file-name))))
	 (proc-cmd (concat lesim-command " " script-file))
	 (proc-obj (start-process-shell-command "lesim" proc-buf proc-cmd)))
    (setq proc-buf (process-buffer proc-obj)) ; could be different...
    (with-current-buffer proc-buf
      (while (process-live-p proc-obj)
	(accept-process-output proc-obj 0.25)
	(let (prog-msg prog-val)
	  (goto-char (point-max))
	  (when (re-search-backward "Running .+" nil t)
	    (setq prog-msg (match-string 0))
	    (goto-char (point-max))
	    (when (re-search-backward "[^0-9]\\([0-9]+%\\)" nil t)
	      (setq prog-val (match-string 1))
	      (message "%s %s" prog-msg prog-val)))))
      (message "Done")
      (goto-char (point-max))
      (when (re-search-backward "Error on line \\([0-9]+\\): \\(.+\\)" nil t)
	(let ((line (string-to-number (match-string 1)))
	      (mess (match-string 0)))
          (with-temp-buffer
            (insert-file-contents script-file)
            (goto-char (point-min))
            (forward-line (1- line))
            (let ((beg (point)))
	      (end-of-line)
	      (list line
                    mess
                    (buffer-substring beg (point))))))))))
	

(defun lesim-run-and-error ()
  "Run lesim on the current buffer's file.
Prompt to save unsaved changes if any."
  (interactive)
  (save-some-buffers nil `(lambda () (eq (current-buffer)
                                         ,(current-buffer))))
  (lesim-error (lesim-run (buffer-file-name))))

(provide 'lesim-run)
;;; lesim-run.el ends here
