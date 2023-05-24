;;; lesim-type.el --- Type validation functions for lesim-mode -*- lexical-binding: t; -*-

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

;; Functions to validate Learning Simulator variables.

;;; Code:

(defvar lesim--name-re
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regexp to match Learning Simulator valid names.
A valid name starts with a letter or underscore and continues
with letters, digits, and underscores.")

(defvar lesim--scalar-re
  "[+-]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)"
  "Regexp to match Learning Simulator scalars.")

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

(provide 'lesim-type)
;;; lesim-type.el ends here
