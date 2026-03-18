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
    (if (eq major-mode 'org-mode)
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

(defun lesim-run (script-file buffer)
  "Run Learning Simulator on SCRIPT-FILE.
BUFFER is the source buffer where errors should be highlighted.
Uses a process sentinel so Emacs is not blocked during execution."
  (message "Running")
  (let* ((proc-buf (generate-new-buffer (concat " *lesim run: " script-file)))
         (proc-obj (start-process-shell-command
                    "lesim" proc-buf
                    (concat lesim-command " "
                            (shell-quote-argument script-file)))))
    (set-process-filter
     proc-obj
     (lambda (proc output)
       (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert output)))
       (when (string-match "Running [^\r\n]+" output)
         (message "%s" (match-string 0 output)))))
    (set-process-sentinel
     proc-obj
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let (err-line err-mess)
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (when (re-search-backward
                      "Error on line \\([0-9]+\\): \\(.+\\)" nil t)
                 (setq err-line (string-to-number (match-string 1))
                       err-mess (match-string 0))))
             (kill-buffer (process-buffer proc)))
           (if (not err-line)
               (message "Done")
             (let (error-list)
               (with-temp-buffer
                 (insert-file-contents script-file)
                 (goto-char (point-min))
                 (forward-line (1- err-line))
                 (setq error-list
                       (list err-line err-mess
                             (buffer-substring
                              (point) (line-end-position)))))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (lesim-error error-list)))))))))))

(defun lesim-run-and-error ()
  "Run lesim on the current buffer's file.
Prompt to save unsaved changes if any."
  (interactive)
  (lesim-error nil) ; remove errors
  (lesim-highlight)
  (save-some-buffers nil `(lambda () (eq (current-buffer)
                                         ,(current-buffer))))
  (lesim-run (buffer-file-name) (current-buffer)))

(provide 'lesim-run)
;;; lesim-run.el ends here
