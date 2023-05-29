;;; lesim-custom.el --- Customizations for lesim-mode -*- lexical-binding: t; -*-

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

;; Customizations for lesim-mode

;;; Code:

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

(defcustom lesim-highlight-key (kbd "C-c C-h")
  "Keybinding to re-highlight a Learning Simulator buffer."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-next-error-key (kbd "C-c C-e")
  "Keybinding to move to the next validation error."
  :type 'key-sequence
  :group 'lesim)

(defcustom lesim-template-auto t
  "Insert a script template when opening an empty .les file."
  :type 'boolean
  :group 'lesim)

(provide 'lesim-custom)
;;; lesim-custom.el ends here
