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
(require 'tramp)

(defgroup systemctl nil
  "Interface to Systemd.")

(defcustom systemctl-list-units-format
  (vector (list "Unit" 22 t)
          (list "Active" 9 t)
          (list "Loaded" 8 t)
          (list "State" 8 t)
          (list "Description" 50 nil))
  "Column format specification for `systemctl-list-units'."
  :group 'systemctl
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

(defcustom systemctl-tramp-method "scpx"
  "The TRAMP method to use when remotely accessing Systemd Unit files."
  :group 'systemctl
  :type (cons 'choice
	      (mapcar (lambda (method)
			(list 'const (car method)))
		      tramp-methods)))

(defvar-local systemctl-bus :system
  "Default D-Bus bus to use when accessing Systemd.")

(defvar systemctl-list-units-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'systemctl-edit-unit-files)
    (define-key map "f" #'systemctl-find-fragment)
    (define-key map "start" #'systemctl-start)
    (define-key map "stop"  #'systemctl-stop)
    map)
  "Keymap for `systemctl-list-units-mode'.")

(defun systemctl-bus ()
  (when (stringp systemctl-bus)
    (dbus-init-bus systemctl-bus))
  systemctl-bus)
      
(defun systemctl-list-units-entries ()
  (mapcar (lambda (desc)
            (list (nth 6 desc)
                  (vector (nth 0 desc)
                          (nth 2 desc)
                          (nth 3 desc)
                          (nth 4 desc)
                          (nth 1 desc))))
          (systemd-ListUnits (systemctl-bus))))

(defun systemctl-unescape-unit-name (string)
  (while (string-match "\\\\x\\([0-9a-f]\\{2\\}\\)" string)
    (setq string
          (replace-match (string (string-to-number (match-string 1 string) 16))
                         t t string)))
  string)

(defun systemctl-list-units-print-entry (id cols)
  "Insert a Systemd Units List entry at point."
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (inhibit-read-only t))
    (when (> x 0) (insert (make-string x ?\s)))
    (dotimes (n (length tabulated-list-format))
      (let ((desc (aref cols n)))
        (when (= n 0)
          (setq desc (systemctl-unescape-unit-name desc)))
        (setq x (tabulated-list-print-col n desc x))))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(define-derived-mode systemctl-list-units-mode tabulated-list-mode
  "Systemd-Units"
  "Major mode for displaying a list of Systemd Units."
  (setq tabulated-list-entries #'systemctl-list-units-entries
        tabulated-list-format    systemctl-list-units-format
        tabulated-list-printer #'systemctl-list-units-print-entry)
  (tabulated-list-init-header))
  
(defun systemctl-list-units (&optional host)
  "Display a list of all Systemd Units."
  (interactive
   (list (when (equal current-prefix-arg '(4))
           (read-string "Remote host: "))))
  
  (with-current-buffer (let ((buffer-name (if host
					      (format "*Systemd Units (%s)*"
						      host)
					    "*Systemd Units*")))
			 (get-buffer-create buffer-name))
    (systemctl-list-units-mode)
    (when host
      (setq systemctl-bus (systemd-remote-bus host)))
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun systemctl-start (unit)
  (interactive (list (or (and (tabulated-list-get-entry)
                              (aref (tabulated-list-get-entry) 0))
                         (read-string "Unit: "))))
  (systemd-StartUnit (systemctl-bus) unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-stop (unit)
  (interactive (list (or (and (tabulated-list-get-entry)
                              (aref (tabulated-list-get-entry) 0))
                         (read-string "Unit: "))))
  (systemd-StopUnit (systemctl-bus) unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-find-fragment (unit)
  (interactive
   (list (or (tabulated-list-get-id)
	     (systemd-GetUnit (systemctl-bus) (read-string "Unit: ")))))
  (let ((fragment-path (systemd-unit-FragmentPath (systemctl-bus) unit)))
    (when fragment-path
      (if (and (stringp systemctl-bus)
	       (string-match "unixexec:path=ssh,.*argv2=\\([^,]*\\),"
			     systemctl-bus))
	  (let ((host (match-string 1 systemctl-bus)))
	    (find-file (concat "/" systemctl-tramp-method ":" host ":"
			       fragment-path)))
	(find-file fragment-path)))))

(defun systemctl-edit-unit-files (unit &optional override-file)
  "Visit all configuration files related to UNIT simultaneously.
If optional OVERRIDE-FILE is specified, or if a prefix argument has been
given interactively, open a (new) override file."
  (interactive
   (let* ((unit (if (tabulated-list-get-entry)
		    (systemctl-unescape-unit-name (aref (tabulated-list-get-entry) 0))
		  (read-string "Unit: ")))
	  (unit-path (or (tabulated-list-get-id)
			 (systemd-GetUnit (systemctl-bus) unit)))
	  (override-file
	   (when (equal current-prefix-arg '(4))
	     (read-file-name "Override file: "
			     (concat "/etc/systemd/system/" unit ".d/") nil nil
			     "override.conf"))))
     (list unit-path override-file)))
  (let ((files (systemd-unit-DropInPaths (systemctl-bus) unit)))
    (when override-file
      (let ((directory (file-name-directory override-file)))
	(setq files (append (list override-file) files))
	(unless (file-directory-p directory)
	  (make-directory directory))))
    (let ((path (systemd-unit-FragmentPath (systemctl-bus) unit)))
      (when (not (string= path ""))
	(setq files (append files (list path)))))
    (let ((path (systemd-unit-SourcePath (systemctl-bus) unit)))
      (when (not (string= path ""))
	(setq files (append files (list path)))))
    (when files
      (let ((buffers (mapcar #'find-file-noselect files)))
	(pop-to-buffer (car buffers))
	(when (cdr buffers)
	  (delete-other-windows)
	  (dolist (buffer (cdr buffers))
	    (let ((window (split-window (car (last (window-list))))))
	      (shrink-window-if-larger-than-buffer)
	      (set-window-buffer window buffer)))
	  (mapc #'shrink-window-if-larger-than-buffer (window-list)))))))

(defvar systemd-unit-font-lock-keywords
  '(;; [section]
    ("^[ \t]*\\[\\(Unit\\|Service\\)\\]"
     1 'font-lock-type-face)
    ;; var=val or var[index]=val
    ("^[ \t]*\\(.+?\\)[ \t]*="
     1 'font-lock-variable-name-face))
  "Keywords to highlight in Conf mode.")

(define-derived-mode systemd-unit-mode conf-unix-mode "Systemd-Unit"
  (conf-mode-initialize "#" systemd-unit-font-lock-keywords))

(mapc (apply-partially #'add-to-list 'auto-mode-alist)
      '(("\\.service\\'" . systemd-unit-mode)))

(provide 'systemctl)
;;; systemctl.el ends here
