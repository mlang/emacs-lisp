;;; bluez.el --- D-Bus bindings for BlueZ           -*- lexical-binding: t; -*-

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

;; D-Bus bindings for BlueZ, the Linux Bluetooth stack.

;;; Code:

(require 'cl)
(require 'dbus)

(defconst bluez-service "org.bluez")
(defconst bluez-adapter-interface "org.bluez.Adapter1")
(defconst bluez-device-interface "org.bluez.Device1")
(defconst bluez-network-interface "org.bluez.Network1")

(defun bluez-objects ()
  (dbus-get-all-managed-objects :system bluez-service "/"))

(defun bluez-property (interface path property)
  (dbus-get-property :system bluez-service path interface property))

(gv-define-setter bluez-property (value interface path property)
  `(dbus-set-property :system bluez-service
		      ,path ,interface ,property ,value))

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
  (mapcar #'car
	  (remove-if-not (apply-partially #'assoc bluez-adapter-interface)
			 (bluez-objects)
			 :key #'cadr)))

(defun bluez-adapter-call-method (adapter-path method &rest args)
  (apply #'dbus-call-method :system bluez-service adapter-path
         bluez-adapter-interface method args))

;; org.bluez.Adapter1.Address
(defun bluez-adapter-address (adapter-path)
  (bluez-get-property bluez-adapter-interface "Address"))

;; org.bluez.Adapter1.Powered
(defun bluez-adapter-powered-p (adapter-path)
  "Indicates if the adapter at ADAPTER-PATH is powered on.
Use `setf' to set the value of this property."
  (bluez-get-property bluez-adapter-interface "Powered"))

(gv-define-setter bluez-adapter-powered-p (value adapter-path)
  `(setf (bluez-property bluez-adapter-interface ,adapter-path "Powered")
	 ,value))

;; org.bluez.Adapter1.Alias
(defun bluez-adapter-alias (adapter-path)
  "The Bluetooth friendly name of the adapter at ADAPTER-PATH.
Use `setf' to set the value of this property."
  (bluez-get-property bluez-adapter-interface adapter-path "Alias"))

(gv-define-setter bluez-adapter-alias (value adapter-path)
  `(setf (bluez-property bluez-adapter-interface ,adapter-path "Alias")
	 ,value))

;; org.bluez.Adapter1.Name
(defun bluez-adapter-name (adapter-path)
  "The Bluetooth system name (pretty hostname) of the adapter at ADAPTER-PATH."
  (bluez-get-property bluez-adapter-interface adapter-path "Name"))

(defun bluez-adapter-discoverable-p (adapter-path)
  (bluez-get-property bluez-adapter-interface adapter-path "Discoverable"))

(gv-define-setter bluez-adapter-discoverable-p (value adapter-path)
  `(setf (bluez-property bluez-adapter-interface ,adapter-path "Discoverable")
         ,value))

(defun bluez-adapter-pairable-p (adapter-path)
  (bluez-get-property bluez-adapter-interface adapter-path "Pairable"))

(gv-define-setter bluez-adapter-pairable-p (value adapter-path)
  `(setf (bluez-property bluez-adapter-interface ,adapter-path "Pairable")
         ,value))

(defun bluez-adapter-remove-device (adapter-path device-path)
  "Removes the remote device object at the given DEVICE-PATH.
It will remove also the pairing information.

If called interactively, lets the user choose from a list
of all known devices."
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

(defun bluez-device-call-method (device-path method &rest args)
  (apply #'dbus-call-method :system bluez-service device-path
         bluez-device-interface method args))

(defun bluez-device-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-device-interface)
                               (bluez-objects)
                               :key #'cadr)))

(defun bluez-device-connect (device-path)
  (bluez-device-call-method device-path "Connect"))

(defun bluez-device-disconnect (device-path)
  (bluez-device-call-method device-path "Disconnect"))

(defun bluez-device-connect-profile (device-path uuid)
  (bluez-device-call-method device-path "ConnectProfile" uuid))

(defun bluez-device-disconnect-profile (device-path uuid)
  (bluez-device-call-method device-path "DisconnectProfile" uuid))

(defun bluez-device-pair (device-path)
  (bluez-device-call-method device-path "Pair"))

(defun bluez-device-cancel-pairing (device-path)
  (bluez-device-call-method device-path "CancelPairing"))

(defun bluez-device-address (device-path)
  (bluez-get-property bluez-device-interface device-path "Address"))

(defun bluez-device-name (device-path)
  (bluez-get-property bluez-device-interface device-path "Name"))

(defun bluez-device-alias (device-path)
  (bluez-get-property bluez-device-interface device-path "Alias"))

(gv-define-setter bluez-device-alias (value device-path)
  `(setf (bluez-property bluez-device-interface ,device-path "Alias")
	 ,value))

(defun bluez-device-paired-p (device-path)
  (bluez-get-property bluez-device-interface device-path "Paired"))

(defun bluez-device-trusted-p (device-path)
  (bluez-get-property bluez-device-interface device-path "Trusted"))

(gv-define-setter bluez-device-trusted-p (value device-path)
  `(setf (bluez-property bluez-device-interface ,device-path "Trusted")
	 ,value))

(defun bluez-device-blocked-p (device-path)
  (bluez-get-property bluez-device-interface device-path "Blocked"))

(gv-define-setter bluez-device-blocked-p (value device-path)
  `(setf (bluez-property bluez-device-interface ,device-path "Blocked")
	 ,value))

(defun bluez-device-connected-p (device-path)
  (bluez-get-property bluez-device-interface device-path "Connected"))

(defun bluez-device-adapter (device-path)
  (bluez-get-property bluez-device-interface device-path "Adapter"))

;;; Network

(defun bluez-network-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-network-interface)
			       (bluez-objects)
			       :key #'cadr)))

(provide 'bluez)
;;; bluez.el ends here
