;;; bluez.el --- Bluetooth                          -*- lexical-binding: t; -*-

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

(require 'cl)
(require 'dbus)

(defconst bluez-service "org.bluez")
(defconst bluez-adapter-interface "org.bluez.Adapter1")
(defconst bluez-device-interface "org.bluez.Device1")
(defconst bluez-network-interface "org.bluez.Network1")

(defun bluez-property (interface path property)
  (dbus-get-property :system bluez-service path interface property))

(gv-define-setter
 bluez-property
 (value interface path property)
 `(dbus-set-property :system bluez-service ,path ,interface ,property ,value)))

(defun bluez-objects ()
  (dbus-get-all-managed-objects :system bluez-service "/"))

(defun bluez-handler (path interfaces-and-properties)
  (dolist (iface-entry interfaces-and-properties)
    (if (cadr iface-entry)
	(setcdr iface-entry
		(dolist (property-entry (cadr iface-entry) (cadr iface-entry))
		  (setcdr property-entry (caadr property-entry))))
      (setcdr iface-entry nil)))
  (message "%s %S" path interfaces-and-properties))

(setq bluez-interfaces-added
      (dbus-register-signal :system bluez-service nil
			    dbus-interface-objectmanager "InterfacesAdded"
			    #'bluez-handler))

;;; Adapter

(defun bluez-adapter-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-adapter-interface)
			       (bluez-objects)
			       :key #'cadr)))

(defun bluez-adapter-call-method (adapter-path method &rest args)
  (apply #'dbus-call-method :system bluez-service adapter-path
	 bluez-adapter-interface method args))

(defun bluez-adapter-address (adapter-path)
  (bluez-get-property bluez-adapter-interface "Address"))

(defun bluez-adapter-powered-p (adapter-path)
  (bluez-get-property bluez-adapter-interface "Powered"))

(gv-define-setter
 bluez-adapter-powered-p
 (value adapter-path)
 `(setf (bluez-property bluez-adapter-interface ,adapter-path "Powered")
	,value))

(defun bluez-adapter-remove-device (adapter-path device-path)
  "Removes the remote device object at the given DEVICE-PATH.
It will remove also the pairing information."
  (interactive
   (let* ((device-paths (bluez-device-paths))
	  (name (completing-read "Device to remove: "
				 (mapcar #'bluez-device-name device-paths)
				 nil t)))
     (cdr (assoc name
		 (mapcar (lambda (path)
			   (list (bluez-device-name path)
				 (bluez-device-adapter path) path))
			 device-paths)))))
  (bluez-adapter-call-method adapter-path
			     "RemoveDevice" :object-path device-path))

(defun bluez-adapter-discovering-p (adapter-path)
  (bluez-get-property bluez-adapter-interface adapter-path "Discovering"))

(defun bluez-adapter-start-discovery (adapter-path)
  (bluez-adapter-call-method adapter-path "StartDiscovery"))

(defun bluez-adapter-stop-discovery (adapter-path)
  (bluez-adapter-call-method adapter-path "StopDiscovery"))

;;; Device

(defun bluez-device-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-device-interface)
			       (bluez-objects)
			       :key #'cadr)))

(defun bluez-device-name (device-path)
  (bluez-get-property bluez-device-interface device-path "Name"))

(defun bluez-device-paired-p (device-path)
  (bluez-get-property bluez-device-interface device-path "Paired"))

(defun bluez-device-adapter (device-path)
  (bluez-get-property bluez-device-interface device-path "Adapter"))

;;; Network

(defun bluez-network-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-network-interface)
			       (bluez-objects)
			       :key #'cadr)))

(provide 'bluez)
;;; bluez.el ends here
