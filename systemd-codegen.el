;;; systemd-codegen.el --- D-Bus Introspection      -*- lexical-binding: t; -*-

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

;;; Code:

(require 'dbus)

(defconst systemd-codegen-prefixes
  '(("org.freedesktop.systemd1.Manager" . "systemd-")
    ("org.freedesktop.systemd1.Automount" . "systemd-automount-")
    ("org.freedesktop.systemd1.BusName" . "systemd-bus-name-")
    ("org.freedesktop.systemd1.Device" . "systemd-device-")
    ("org.freedesktop.systemd1.Mount" . "systemd-mount-")
    ("org.freedesktop.systemd1.Path" . "systemd-path-")
    ("org.freedesktop.systemd1.Service" . "systemd-service-")
    ("org.freedesktop.systemd1.Scope" . "systemd-scope-")
    ("org.freedesktop.systemd1.Slice" . "systemd-slice-")
    ("org.freedesktop.systemd1.Socket" . "systemd-socket-")
    ("org.freedesktop.systemd1.Swap" . "systemd-swap-")
    ("org.freedesktop.systemd1.Target" . "systemd-target-")
    ("org.freedesktop.systemd1.Timer" . "systemd-timer-")
    ("org.freedesktop.systemd1.Unit" . "systemd-unit-")
    ("org.freedesktop.login1.Manager" . "systemd-logind-")
    ("org.freedesktop.login1.Seat" . "systemd-logind-seat-")
    ("org.freedesktop.login1.Session" . "systemd-logind-session-")
    ("org.freedesktop.login1.User" . "systemd-logind-user-")
    ("org.freedesktop.network1.Manager" . "systemd-networkd-")
    ("org.freedesktop.network1.Link" . "systemd-networkd-link-")
    ("org.freedesktop.network1.Network" . "systemd-networkd-network-")
    ("org.freedesktop.resolve1.Manager" . "systemd-resolved-")
    ("org.freedesktop.resolve1.Link" . "systemd-resolved-link-")
    ("org.freedesktop.hostname1" . "systemd-hostnamed-")
    ("org.freedesktop.locale1" . "systemd-localed-")
    ("org.freedesktop.timedate1" . "systemd-timedated-")
    ("org.freedesktop.machine1.Manager" . "systemd-machined-")
    ("org.freedesktop.machine1.Image" . "systemd-machined-image-")
    ("org.freedesktop.machine1.Machine" . "systemd-machined-machine-")))

(defun systemd-codegen-introspect (service path &optional interfaces)
  (let ((xml (dbus-introspect-xml :system service path)))
    (dolist (item (and (eq (car-safe xml) 'node)
		       (xml-node-children xml)) interfaces)
      (cond
       ((and (listp item) (eq 'interface (car-safe item)))
	(let* ((interface (xml-get-attribute-or-nil item 'name))
	       (prefix (cdr (assoc interface systemd-codegen-prefixes)))
	       (object-interface (not (string-match "\\(\\.Manager\\|1\\)$" interface)))
	       (service (pcase service
			  ("org.freedesktop.systemd1" 'systemd-dbus-service)
			  (_ service)))
	       (path (pcase path
		       ("/org/freedesktop/systemd1" 'systemd-dbus-path)
		       (_ path)))
	       forms)
	  (when (and prefix (not (assoc interface interfaces)))
	    (setq
	     interfaces
	     (append
	      interfaces
	      (list
	       (cons
		interface
		(dolist (interface-item (cddr item) (nreverse forms))
		  (cond
		   ((eq 'property (car-safe interface-item))
		    (let* ((property (cdr (assq 'name (cadr interface-item))))
			   (name (intern (concat prefix property)))
			   (readwrite (string-equal "readwrite"
						    (cdr (assq 'access (cadr interface-item)))))
			   (arglist `(bus
				      ,@(when object-interface
					  '(path)))))
		      (push `(defun ,name ,arglist
			       ,(if readwrite
				    "Use `setf' to set the value of this property."
				  "Read only property.")
			       (dbus-get-property
				bus ,service
				,(if object-interface 'path path)
				,interface ,property))
			    forms)
		      (when readwrite
			(push (list 'gv-define-setter name (cons 'value arglist)
				    (list '\`
					  (list 'dbus-set-property
						(list '\, 'bus)
						service
						(if object-interface
						    (list '\, 'path)
						  path)
						interface property
						(list '\, 'value))))
			      forms))))

		   ((eq 'method (car-safe interface-item))
		    (let* ((method (xml-get-attribute-or-nil interface-item 'name))
			   (name (intern (concat prefix method))))
		      (push `(defun ,name (bus ,@(when object-interface
						   '(path))
					   &rest args)
			       (apply #'dbus-call-method
				      bus ,service
				      ,(if object-interface 'path path)
				      ,interface ,method args))
			    forms))))))))))))
       ((and (listp item) (eq 'node (xml-node-name item)))
	(let ((name (xml-get-attribute-or-nil item 'name)))
	  (setq interfaces (systemd-codegen-introspect
			    service (concat path "/" name) interfaces))))))))

(defmacro systemd-codegen-define (suffix)
  `(progn
     ,@(apply
	#'append
	(mapcar #'cdr
		(systemd-codegen-introspect
		 (concat "org.freedesktop." suffix)
		 (concat "/org/freedesktop/" suffix))))))

(defun systemd-codegen-to-string (suffix)
  (with-temp-buffer
    (pcase-dolist (`(,interface . ,forms)
		   (systemd-codegen-introspect
		    (concat "org.freedesktop." suffix)
		    (concat "/org/freedesktop/" suffix)))
      (insert ";;; " interface "\n\n")
      (dolist (form forms)
	(pp form (current-buffer))
	(insert "\n"))
      (delete-backward-char 1))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (while (re-search-forward "^(\\(defun\\|gv-define-setter\\)" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 2) (delete-forward-char 4) (up-list 1))
    (goto-char (point-min))
    (while (re-search-forward "(dbus-\\(get\\|set\\)-property" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 4) (insert "\n") (up-list -1) (indent-sexp)
      (up-list 1))
    (goto-char (point-min))
    (while (re-search-forward "(apply #'dbus-call-method" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 5) (insert "\n") (up-list -1) (indent-sexp)
      (up-list 1))
    (buffer-string)))

(provide 'systemd-codegen)
;;; systemd.el ends here
