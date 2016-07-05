;;; systemd.el --- D-Bus bindings for Systemd       -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'systemd-introspect))
(require 'dbus)
(require 'gv)

(systemd-define "systemd1")
(systemd-define "login1")
(systemd-define "network1")
(systemd-define "hostname1")
(systemd-define "resolve1")
(systemd-define "locale1")
(systemd-define "timedate1")
(systemd-define "machine1")

(define-derived-mode systemctl-list-units-mode tabulated-list-mode "Units"
  "Major mode for displaying a list of Systemd Units."
  (setq tabulated-list-format (vector (list "Unit" 30 t)
				      (list "Descritpion" 20 nil)
				      (list "Active" 8 t)
				      (list "Loaded" 8 t)
				      (list "State" 8 t)))
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

(provide 'systemd)
;;; systemd.el ends here