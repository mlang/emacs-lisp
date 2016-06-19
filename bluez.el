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
(defconst bluez-interface-adapter "org.bluez.Adapter1")
(defconst bluez-interface-device "org.bluez.Device1")
(defconst bluez-interface-network "org.bluez.Network1")

(defun bluez-objects ()
  (dbus-get-all-managed-objects :system bluez-service "/"))

(defun bluez-property (interface path property)
  (dbus-get-property :system bluez-service path interface property))

(gv-define-setter bluez-property (value interface path property)
  `(dbus-set-property :system bluez-service
		      ,path ,interface ,property ,value))

;;; org.bluez.Adapter1

(defun bluez-adapter-path (adapter)
  (if (string-prefix-p "/org/bluez" adapter)
      adapter
    (concat "/org/bluez/" adapter)))

(defun bluez-adapter-paths ()
  (mapcar #'car
	  (remove-if-not (apply-partially #'assoc bluez-interface-adapter)
			 (bluez-objects)
			 :key #'cadr)))

(defun bluez-adapter-call-method (adapter method &rest args)
  (apply #'dbus-call-method :system bluez-service (bluez-adapter-path adapter)
         bluez-interface-adapter method args))

(defun bluez-adapter-property (adapter property)
  (bluez-property bluez-interface-adapter (bluez-adapter-path adapter) property))

(gv-define-setter bluez-adapter-property (value adapter property)
  `(setf (bluez-property bluez-interface-adapter (bluez-adapter-path ,adapter) ,property) ,value))

;; org.bluez.Adapter1.Address
(defun bluez-adapter-address (adapter-path)
  (bluez-adapter-property adapter-path "Address"))

;; org.bluez.Adapter1.Powered
(defun bluez-adapter-powered-p (adapter)
  "Indicates if the adapter at ADAPTER is powered on.
Use `setf' to set the value of this property."
  (bluez-adapter-property adapter "Powered"))

(gv-define-setter bluez-adapter-powered-p (value adapter)
  `(setf (bluez-adapter-property ,adapter "Powered") ,value))

;; org.bluez.Adapter1.Alias
(defun bluez-adapter-alias (adapter)
  "The Bluetooth friendly name of the adapter at ADAPTER.
Use `setf' to set the value of this property."
  (bluez-adapter-property adapter "Alias"))

(gv-define-setter bluez-adapter-alias (value adapter)
  `(setf (bluez-adapter-property ,adapter "Alias") ,value))

;; org.bluez.Adapter1.Name
(defun bluez-adapter-name (adapter)
  "The Bluetooth system name (pretty hostname) of the adapter at ADAPTER."
  (bluez-adapter-property adapter "Name"))

;; org.bluez.Adapter1.Discoverable
(defun bluez-adapter-discoverable-p (adapter)
  (bluez-adapter-property adapter "Discoverable"))

(gv-define-setter bluez-adapter-discoverable-p (value adapter)
  `(setf (bluez-adapter-property ,adapter "Discoverable") ,value))

;; org.bluez.Adapter1.DiscoverableTimeout
(defun bluez-adapter-discoverable-timeout (adapter)
  (bluez-adapter-property adapter "DiscoverableTimeout"))

(gv-define-setter bluez-adapter-discoverable-timeout (value adapter)
  `(setf (bluez-adapter-property ,adapter "DiscoverableTimeout") ,value))

;; org.bluez.Adapter1.Pairable
(defun bluez-adapter-pairable-p (adapter)
  (bluez-adapter-property adapter "Pairable"))

(gv-define-setter bluez-adapter-pairable-p (value adapter)
  `(setf (bluez-adapter-property ,adapter "Pairable") ,value))

;; org.bluez.Adapter1.PairableTimeout
(defun bluez-adapter-pairable-timeout (adapter)
  (bluez-adapter-property adapter "PairableTimeout"))

(gv-define-setter bluez-adapter-pairable-timeout (value adapter)
  `(setf (bluez-adapter-property ,adapter "PairableTimeout") ,value))

(defun bluez-adapter-remove-device (adapter device-path)
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
  (bluez-adapter-call-method adapter "RemoveDevice" :object-path device-path))

;; org.bluez.Adapter1.Discovering
(defun bluez-adapter-discovering-p (adapter)
  (bluez-adapter-property adapter "Discovering"))

(defun bluez-adapter-start-discovery (adapter)
  (bluez-adapter-call-method adapter "StartDiscovery"))

(defun bluez-adapter-stop-discovery (adapter)
  (bluez-adapter-call-method adapter "StopDiscovery"))

;;; org.bluez.Device1

(defun bluez-device-call-method (device-path method &rest args)
  (apply #'dbus-call-method :system bluez-service device-path
         bluez-interface-device method args))

(defun bluez-device-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-interface-device)
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

;; org.bluez.Device1.Address
(defun bluez-device-address (device-path)
  (bluez-property bluez-interface-device device-path "Address"))

;; org.bluez.Device1.Name
(defun bluez-device-name (device-path)
  (bluez-property bluez-interface-device device-path "Name"))

;; org.bluez.Device1.Alias
(defun bluez-device-alias (device-path)
  (bluez-property bluez-interface-device device-path "Alias"))

(gv-define-setter bluez-device-alias (value device-path)
  `(setf (bluez-property bluez-interface-device ,device-path "Alias") ,value))

;; org.bluez.Device1.Paired
(defun bluez-device-paired-p (device-path)
  (bluez-property bluez-interface-device device-path "Paired"))

;; org.bluez.Device1.Trusted
(defun bluez-device-trusted-p (device-path)
  (bluez-property bluez-interface-device device-path "Trusted"))

(gv-define-setter bluez-device-trusted-p (value device-path)
  `(setf (bluez-property bluez-interface-device ,device-path "Trusted") ,value))

;; org.bluez.Device1.Blocked
(defun bluez-device-blocked-p (device-path)
  (bluez-property bluez-interface-device device-path "Blocked"))

(gv-define-setter bluez-device-blocked-p (value device-path)
  `(setf (bluez-property bluez-interface-device ,device-path "Blocked") ,value))

;; org.bluez.Device1.Connected
(defun bluez-device-connected-p (device-path)
  (bluez-property bluez-interface-device device-path "Connected"))

;; org.bluez.Device1.Adapter
(defun bluez-device-adapter (device-path)
  (bluez-property bluez-interface-device device-path "Adapter"))

;;; org.bluez.Network1

(defun bluez-network-paths ()
  (mapcar #'car (remove-if-not (apply-partially #'assoc bluez-interface-network)
			       (bluez-objects)
			       :key #'cadr)))


;;; Agent

(defconst bluez-interface-agent "org.bluez.Agent1")

(defconst bluez-path-agent (concat dbus-path-emacs "/bluez/agent")
  "Path of the agent object used to receive agent requests from bluetoothd.")

(defun bluez-agent-request-pin-code-handler (device-path)
  (read-string (format "PIN code for %s: " (bluez-device-name device-path))))

(defvar bluez-agent-request-pin-code-method
  (dbus-register-method
   :system nil bluez-path-agent bluez-interface-agent "RequestPinCode"
   #'bluez-agent-request-pin-code-handler t))

;;; AgentManager

(defconst bluez-interface-agent-manager "org.bluez.AgentManager1")

(defun bluez-agent-manager-call-method (method &rest args)
  (apply #'dbus-call-method :system bluez-service "/org/bluez"
         bluez-interface-agent-manager method args))

(defun bluez-agent-manager-register-agent (&optional object-path capabilities)
  (interactive)
  (bluez-agent-manager-call-method
   "RegisterAgent"
   :object-path (or object-path bluez-path-agent)
   (or capabilities "")))

(defun bluez-agent-manager-unregister-agent (&optional object-path)
  (interactive)
  (bluez-agent-manager-call-method
   "UnregisterAgent" :object-path (or object-path bluez-path-agent)))

(defun bluez-agent-manager-request-default-agent (&optional object-path)
  (interactive)
  (bluez-agent-manager-call-method
   "RequestDefaultAgent"
   :object-path (or object-path bluez-path-agent)))

(define-derived-mode bluez-device-list-mode tabulated-list-mode "BlueZ"
  "Major mode for displaying Bluetooth remote devices."
  (setq tabulated-list-format [("Name" 24 t) ("Alias" 24 t) ("Address" 20 t)])
  (setq tabulated-list-entries
	(mapcar
	 (lambda (device-path)
	   (list device-path
		 (vector (or (bluez-device-name device-path) "None")
			 (or (bluez-device-alias device-path) "None")
			 (bluez-device-address device-path))))
	 (bluez-device-paths)))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun list-bluetooth-devices ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Bluetooth Devices*")
    (bluez-device-list-mode)
    (pop-to-buffer (current-buffer))))

(defun bluez-interfaces-added-handler (path interfaces-and-properties)
  ;; Make the structure more Lispy.
  (dolist (iface-entry interfaces-and-properties)
    (if (cadr iface-entry)
        (setcdr iface-entry
                (dolist (property-entry (cadr iface-entry) (cadr iface-entry))
                  (setcdr property-entry (caadr property-entry))))
      (setcdr iface-entry nil)))

  (when (buffer-live-p (get-buffer "*Bluetooth Devices*"))
    (with-current-buffer (get-buffer "*Bluetooth Devices*")
      (let ((device-properties (cdr (assoc bluez-interface-device interfaces-and-properties))))
	(when device-properties
	  (let ((address (cdr (assoc "Address" device-properties)))
		(name (cdr (assoc "Name" device-properties)))
		(alias (cdr (assoc "Alias" device-properties))))
	    (when address
	      (let ((entry (assoc path tabulated-list-entries))
		    (data (vector (or name "None") (or alias "None") address)))
		(if entry
		    (setf (cadr entry) data)
		(push (list path data) tabulated-list-entries)))
	      (tabulated-list-revert)))))))
  (message "%s %S" path interfaces-and-properties))

(defvar bluez-interfaces-added-signal nil)

(defun bluetooth-start-discovery ()
  (interactive)
  (dolist (adapter-path (bluez-adapter-paths))
    (bluez-adapter-start-discovery adapter-path))
  (setq bluez-interfaces-added-signal
	(dbus-register-signal
	 :system bluez-service nil
	 dbus-interface-objectmanager "InterfacesAdded"
	 #'bluez-interfaces-added-handler)))

(defun bluetooth-stop-discovery ()
  (interactive)
  (dolist (adapter-path (bluez-adapter-paths))
    (when (bluez-adapter-discovering-p adapter-path)
      (bluez-adapter-stop-discovery adapter-path)))
  (when bluez-interfaces-added-signal
    (dbus-unregister-object bluez-interfaces-added-signal)
    (setq bluez-interfaces-added-signal nil)))

(provide 'bluez)
;;; bluez.el ends here
