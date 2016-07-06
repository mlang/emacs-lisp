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
  (vector (list "Unit" 22 t)
          (list "Active" 9 t)
          (list "Loaded" 8 t)
          (list "State" 8 t)
          (list "Description" 50 nil))
  "Column format specification for `systemctl-list-units'."
  :type '(vector (list :tag "Unit"
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
                       (boolean :tag "Sortable"))
		 (list :tag "Description"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))))

(defun systemctl-list-units-entries ()
  (mapcar (lambda (desc)
            (list (nth 6 desc)
                  (vector (nth 0 desc)
                          (nth 2 desc)
                          (nth 3 desc)
                          (nth 4 desc)
                          (nth 1 desc))))
          (systemd-ListUnits)))

(defun systemctl-unescape-unit-name (string)
  (while (string-match "\\\\x\\([0-9a-f]\\{2\\}\\)" string)
    (setq string
	  (replace-match (string (string-to-int (match-string 1 string) 16))
			 t t string)))
  string)

(defun systemctl-list-units-print-entry (id cols)
  "Insert a Systemd Units List entry at point."
  (let ((beg   (point))
	(x     (max tabulated-list-padding 0))
	(inhibit-read-only t))
    (if (> tabulated-list-padding 0)
	(insert (make-string x ?\s)))
    (dotimes (n (length tabulated-list-format))
      (let ((desc (aref cols n)))
	(when (= n 0)
	  (setq desc (systemctl-unescape-unit-name desc)))
	(setq x (tabulated-list-print-col n desc x))))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(defvar systemctl-list-units-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'systemctl-find-file)
    (define-key map "start" #'systemctl-start)
    (define-key map "stop"  #'systemctl-stop)
    map)
  "Keymap for `systemctl-list-units-mode'.")

(define-derived-mode systemctl-list-units-mode tabulated-list-mode "Systemd-Units"
  "Major mode for displaying a list of Systemd Units."
  (setq tabulated-list-entries #'systemctl-list-units-entries
        tabulated-list-format    systemctl-list-units-format
	tabulated-list-printer #'systemctl-list-units-print-entry)
  (tabulated-list-init-header))
  
(defun systemctl-list-units ()
  "Display a list of all Systemd Units."
  (interactive)
  (with-current-buffer (get-buffer-create "*Systemd Units*")
    (systemctl-list-units-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun systemctl-start (unit)
  (interactive (list (or (and (tabulated-list-get-entry)
                              (aref (tabulated-list-get-entry) 0))
                         (read-string "Unit: "))))
  (systemd-StartUnit unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-stop (unit)
  (interactive (list (or (and (tabulated-list-get-entry)
                              (aref (tabulated-list-get-entry) 0))
                         (read-string "Unit: "))))
  (systemd-StopUnit unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-find-file (unit)
  (interactive
   (list (or (tabulated-list-get-id) (systemd-GetUnit (read-string "Unit: ")))))
  (find-file (systemd-unit-FragmentPath unit)))

(provide 'systemctl)
;;; systemctl.el ends here
