;;; systemctl.el --- Perform systemctl operations from within Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'systemd)
(require 'tabulated-list)

(defcustom systemctl-list-units-format
  (vector (list "Unit" 30 t)
	  (list "Description" 20 nil)
	  (list "Active" 8 t)
	  (list "Loaded" 8 t)
	  (list "State" 8 t))
  "See `tabulated-list-format'."
  :type '(vector (list :tag "Unit"
		       (string :tag "Title")
		       (number :tag "Width")
		       (boolean :tag "Sortable"))
		 (list :tag "Description"
		       (string :tag "Title")
		       (number :tag "Width")
		       (boolean :tag "Sortable"))
		 (list :tag "Active"
		       (string :tag "Title")
		       (number :tag "Width")
		       (boolean :tag "Sortable"))
		 (list :tag "Loaded"
		       (string :tag "Title")
		       (number :tag "Width")
		       (boolean :tag "Sortable"))
		 (list :tag "State"
		       (string :tag "Title")
		       (number :tag "Width")
		       (boolean :tag "Sortable"))))

(define-derived-mode systemctl-list-units-mode tabulated-list-mode "Units"
  "Major mode for displaying a list of Systemd Units."
  (setq tabulated-list-format systemctl-list-units-format)
  (tabulated-list-init-header))
  
(defun systemctl-list-units ()
  "Display a list of all Systemd Units."
  (interactive)
  (with-current-buffer (get-buffer-create "*Systemd Units*")
    (systemctl-list-units-mode)
    (setq tabulated-list-entries
	  (mapcar (lambda (desc)
		    (list (nth 6 desc)
			  (vector (nth 0 desc)
				  (nth 1 desc)
				  (nth 2 desc)
				  (nth 3 desc)
				  (nth 4 desc))))
		  (systemd-ListUnits)))
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun systemctl-start (unit)
  (interactive (list (or (tabulated-list-get-id)
			 (systemd-GetUnit (read-string "Unit: ")))))
  (systemd-unit-Start unit))

(provide 'systemctl)
;;; systemctl.el ends here
