;;; bluez.el --- D-Bus bindings for BlueZ           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, hardware

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

(require 'cl-lib)
(require 'dbus)
(require 'gv)
(require 'pcase)
(require 'tabulated-list)

(defconst bluez-service "org.bluez")

(defconst bluez-path "/org/bluez")

(defconst bluez-path-emacs-agent (concat dbus-path-emacs "/bluez/agent")
  "D-Bus path of the Emacs agent object used to receive PIN code requests.")

(defun bluez-objects ()
  "Return a list of all objects (adaptors, devices) known to BlueZ."
  (let ((objects (dbus-get-all-managed-objects :system bluez-service "/")))
    ;; For some reason, `dbus-interface-objectmanager' returns extra levels
    ;; of nesting in some places.  Lets correct this so that we keep adjustments
    ;; in only one place.
    (dolist (object objects objects)
      (if (and (cdr object) (cadr object))
	  (setcdr object
		  (dolist (iface-entry (cadr object) (cadr object))
		    (if (cadr iface-entry)
			(setcdr iface-entry (cadr iface-entry))
		      (setcdr iface-entry nil))))
	(setcdr object nil)))))

(defun bluez-property (path interface property)
  (dbus-get-property :system bluez-service path interface property))

(gv-define-setter bluez-property (value path interface property)
  `(dbus-set-property :system bluez-service
		      ,path ,interface ,property ,value))

;;; org.bluez.Adapter1

(defconst bluez-interface-adapter "org.bluez.Adapter1")

(defun bluez-adapter-path (adapter)
  (if (string-prefix-p bluez-path adapter)
      adapter
    (concat bluez-path "/" adapter)))

(defun bluez-call-adapter-method (adapter method &rest args)
  (apply #'dbus-call-method :system bluez-service (bluez-adapter-path adapter)
         bluez-interface-adapter method args))

(defun bluez-adapter-property (adapter property)
  (bluez-property (bluez-adapter-path adapter) bluez-interface-adapter property))

(gv-define-setter bluez-adapter-property (value adapter property)
  `(setf (bluez-property (bluez-adapter-path ,adapter) bluez-interface-adapter ,property) ,value))

(defun bluez-adapter-paths ()
  (mapcar #'car
	  (cl-remove-if-not (apply-partially #'assoc bluez-interface-adapter)
			    (bluez-objects) :key #'cdr)))

;; org.bluez.Adapter1.Address
(defun bluez-adapter-address (adapter)
  "The Bluetooth address of ADAPTER."
  (bluez-adapter-property adapter "Address"))

;; org.bluez.Adapter1.Powered
(defun bluez-adapter-powered-p (adapter)
  "Indicates if ADAPTER is powered on.
Use `setf' to set the value of this property."
  (bluez-adapter-property adapter "Powered"))

(gv-define-setter bluez-adapter-powered-p (value adapter)
  `(setf (bluez-adapter-property ,adapter "Powered") ,value))

;; org.bluez.Adapter1.Alias
(defun bluez-adapter-alias (adapter)
  "The Bluetooth friendly name of ADAPTER.
Use `setf' to set the value of this property."
  (bluez-adapter-property adapter "Alias"))

(gv-define-setter bluez-adapter-alias (value adapter)
  `(setf (bluez-adapter-property ,adapter "Alias") ,value))

;; org.bluez.Adapter1.Name
(defun bluez-adapter-name (adapter)
  "The Bluetooth system name (pretty hostname) of ADAPTER."
  (bluez-adapter-property adapter "Name"))

;; org.bluez.Adapter1.Discoverable
(defun bluez-adapter-discoverable-p (adapter)
  "Indicates that ADAPTER is currently discoverable.
You can use `setf' to set the value of this property.
See also `bluez-adapter-discoverable-timeout'."
  (bluez-adapter-property adapter "Discoverable"))

(gv-define-setter bluez-adapter-discoverable-p (value adapter)
  `(setf (bluez-adapter-property ,adapter "Discoverable") ,value))

;; org.bluez.Adapter1.DiscoverableTimeout
(defun bluez-adapter-discoverable-timeout (adapter)
  "The time (in seconds) after which discoverable mode is automatically disabled.
Use can use `setf' to set the value of this adapter property."
  (bluez-adapter-property adapter "DiscoverableTimeout"))

(gv-define-setter bluez-adapter-discoverable-timeout (value adapter)
  `(setf (bluez-adapter-property ,adapter "DiscoverableTimeout") ,value))

;; org.bluez.Adapter1.Pairable
(defun bluez-adapter-pairable (adapter)
  "Indicates that ADAPTER is currently pairable.
You can use `setf' to set the value of this property.
See also `bluez-adapter-pairable-timeout'."
  (bluez-adapter-property adapter "Pairable"))

(gv-define-setter bluez-adapter-pairable (value adapter)
  `(setf (bluez-adapter-property ,adapter "Pairable") ,value))

;; org.bluez.Adapter1.PairableTimeout
(defun bluez-adapter-pairable-timeout (adapter)
  "The time (in seconds) after whiich ADAPTER stops to be pairable again.
Use can use `setf' to set the value of this property."
  (bluez-adapter-property adapter "PairableTimeout"))

(gv-define-setter bluez-adapter-pairable-timeout (value adapter)
  `(setf (bluez-adapter-property ,adapter "PairableTimeout") ,value))

;; org.bluez.Adapter1.RemoveDevice
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
  (bluez-call-adapter-method adapter "RemoveDevice" :object-path device-path))

;; org.bluez.Adapter1.Discovering
(defun bluez-adapter-discovering-p (adapter)
  (bluez-adapter-property adapter "Discovering"))

;; org.bluez.Adapter1.StartDiscovery
(defun bluez-adapter-start-discovery (adapter)
  (bluez-call-adapter-method adapter "StartDiscovery"))

;; org.bluez.Adapter1.StopDiscovery
(defun bluez-adapter-stop-discovery (adapter)
  (bluez-call-adapter-method adapter "StopDiscovery"))

;;; org.bluez.Device1
(defconst bluez-interface-device "org.bluez.Device1")

(defun bluez-call-device-method (device-path method &rest args)
  (apply #'dbus-call-method :system bluez-service device-path
         bluez-interface-device method args))

(defun bluez-call-device-method-asynchronously (device-path method handler &rest args)
  (apply #'dbus-call-method-asynchronously :system bluez-service device-path
         bluez-interface-device method handler args))

(defun bluez-device-paths ()
  (mapcar #'car (cl-remove-if-not
		 (apply-partially #'assoc bluez-interface-device)
		 (bluez-objects)
		 :key #'cdr)))

(defun bluez-read-unpaired-device-path (prompt)
  (let* ((unpaired-devices
	  (cl-remove-if
	   (lambda (interfaces)
	     (let ((device-interface
		    (cdr (assoc
			  bluez-interface-device
			  interfaces))))
	       (or (not device-interface)
		   (cdr (assoc
			 "Paired"
			 device-interface)))))
	   (bluez-objects) :key #'cdr))
         (table (mapcar (lambda (dev)
                          (cons (let ((device-properties
				       (cdr (assoc bluez-interface-device
						   (cdr dev)))))
                                  (format "%s (%s)"
                                          (or (cdr (assoc "Name" device-properties))
                                              (cdr (assoc "Alias" device-properties)))
                                          (cdr (assoc "Address" device-properties))))
                                dev))
			unpaired-devices)))
    (when table
      (let ((selection (completing-read prompt table nil t)))
        (when (and selection (not (string-equal selection "")))
          (cdr (assoc selection table)))))))

(defun bluez-read-untrusted-device-path (prompt)
  (let* ((untrusted-devices (cl-remove-if (lambda (interfaces)
					    (let ((device-interface
						   (cdr (assoc
							 bluez-interface-device
							 interfaces))))
					      (or (not device-interface)
						  (not
						   (cdr (assoc
							 "Paired"
							 (car device-interface))))
						  (cdr
						   (assoc
						    "Trusted"
						    (car device-interface))))))
					  (bluez-objects) :key #'cadr))
         (table (mapcar (lambda (dev)
                          (cons (let ((device-properties
				       (cadr (assoc bluez-interface-device
						    (cadr dev)))))
                                  (format "%s (%s)"
                                          (or (cdr (assoc "Name"
							  device-properties))
                                              (cdr (assoc "Alias"
							  device-properties)))
                                          (cdr (assoc "Address"
						      device-properties))))
                                dev))
			untrusted-devices)))
    (when table
      (let ((selection (completing-read prompt table nil t)))
        (when (and selection (not (string-equal selection "")))
          (cadr (assoc selection table)))))))

(defun bluez-device-connect (device-path)
  (bluez-call-device-method device-path "Connect"))

(defun bluez-device-disconnect (device-path)
  (bluez-call-device-method device-path "Disconnect"))

(defun bluez-device-connect-profile (device-path uuid)
  (bluez-call-device-method device-path "ConnectProfile" uuid))

(defun bluez-device-disconnect-profile (device-path uuid)
  (bluez-call-device-method device-path "DisconnectProfile" uuid))

(defun bluez-device-pair (device-path)
  (bluez-call-device-method-asynchronously device-path "Pair" nil))

(defun bluez-device-cancel-pairing (device-path)
  (bluez-call-device-method device-path "CancelPairing"))

;; org.bluez.Device1.Address
(defun bluez-device-address (device-path)
  (bluez-property device-path bluez-interface-device "Address"))

;; org.bluez.Device1.Name
(defun bluez-device-name (device-path)
  (bluez-property device-path bluez-interface-device "Name"))

;; org.bluez.Device1.Alias
(defun bluez-device-alias (device-path)
  (bluez-property device-path bluez-interface-device "Alias"))

(gv-define-setter bluez-device-alias (value device-path)
  `(setf (bluez-property ,device-path bluez-interface-device "Alias") ,value))

;; org.bluez.Device1.Paired
(defun bluez-device-paired-p (device-path)
  (bluez-property device-path bluez-interface-device "Paired"))

;; org.bluez.Device1.Trusted
(defun bluez-device-trusted (device-path)
  (bluez-property device-path bluez-interface-device "Trusted"))

(gv-define-setter bluez-device-trusted (value device-path)
  `(setf (bluez-property ,device-path bluez-interface-device "Trusted") ,value))

;; org.bluez.Device1.Blocked
(defun bluez-device-blocked-p (device-path)
  (bluez-property device-path bluez-interface-device "Blocked"))

(gv-define-setter bluez-device-blocked-p (value device-path)
  `(setf (bluez-property ,device-path bluez-interface-device "Blocked") ,value))

;; org.bluez.Device1.Connected
(defun bluez-device-connected-p (device-path)
  (bluez-property device-path bluez-interface-device "Connected"))

;; org.bluez.Device1.Adapter
(defun bluez-device-adapter (device-path)
  (bluez-property device-path bluez-interface-device "Adapter"))

;;; org.bluez.Network1
(defconst bluez-interface-network "org.bluez.Network1")

(defun bluez-network-paths ()
  (mapcar #'car (cl-remove-if-not
		 (apply-partially #'assoc bluez-interface-network)
		 (bluez-objects) :key #'cdr)))


;;; AgentManager

(defconst bluez-interface-agent-manager "org.bluez.AgentManager1")

(defun bluez-call-agent-manager-method (method &rest args)
  (apply #'dbus-call-method :system bluez-service bluez-path
         bluez-interface-agent-manager method args))

(defun bluez-agent-manager-register-agent (&optional object-path capabilities)
  (interactive)
  (bluez-call-agent-manager-method
   "RegisterAgent"
   :object-path (or object-path bluez-path-emacs-agent)
   (or capabilities "")))

(defun bluez-agent-manager-unregister-agent (&optional object-path)
  (interactive)
  (bluez-call-agent-manager-method
   "UnregisterAgent" :object-path (or object-path bluez-path-emacs-agent)))

(defun bluez-agent-manager-request-default-agent (&optional object-path)
  (interactive)
  (bluez-call-agent-manager-method
   "RequestDefaultAgent"
   :object-path (or object-path bluez-path-emacs-agent)))

;;; Agent

(defconst bluez-interface-agent "org.bluez.Agent1")

(defun bluez-agent-request-pin-code-handler (device-path)
  (read-string (format "PIN code for %s: " (bluez-device-name device-path))))

(defun bluez-agent-display-pin-code-handler (device-path pin-code)
  (message "Enter %s on %s." pin-code (bluez-device-name device-path)))

(defun bluez-agent-request-confirmation-handler (device-path passkey)
  (if (yes-or-no-p (format "Device %s is displaying %d? "
			   (bluez-device-name device-path) passkey))
      :ignore
    (signal 'dbus-error "org.bluez.Error.Rejected")))

(defun bluez-agent-authorize-service-handler (device-path uuid)
  (if (yes-or-no-p (format "Device %s requesting a %s connection, authorize? "
			   (bluez-device-name device-path) uuid))
      :ignore
    (signal 'dbus-error 'dbus-error "org.bluez.Error.Rejected")))

(defcustom bluez-agent-capabilities ""
  "Capabilities of the Emacs Bluetooth Agent."
  :type 'string)

(defun bluez-register-agent-method (method handler)
  (dbus-register-method
   :system (dbus-get-unique-name :system) bluez-path-emacs-agent
   bluez-interface-agent method handler))

(defvar bluez-agent-methods
  (prog1 (list (bluez-register-agent-method
		"RequestPinCode" #'bluez-agent-request-pin-code-handler)
	       (bluez-register-agent-method
		"DisplayPinCode" #'bluez-agent-display-pin-code-handler)
	       (bluez-register-agent-method
		"RequestConfirmation" #'bluez-agent-request-confirmation-handler)
	       (bluez-register-agent-method
		"AuthorizeService" #'bluez-agent-authorize-service-handler))
    (bluez-agent-manager-register-agent
     bluez-path-emacs-agent bluez-agent-capabilities)))

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
  (add-hook 'bluez-interface-added-hook
	    #'bluez-update-device-properties
	    nil 'local))

;;; Incoming signals

(defun bluez-register-signal (interface method handler)
  (dbus-register-signal :system bluez-service nil interface method handler))

(defun bluez-update-device-properties (interface properties)
  (when (string-equal interface bluez-interface-device)
    (let ((address (cdr (assoc "Address" properties)))
	  (name (cdr (assoc "Name" properties)))
	  (alias (cdr (assoc "Alias" properties))))
      (when address
	(let ((entry (assoc path tabulated-list-entries))
	      (data (vector (or name "None") (or alias "None") address)))
	  (if entry
	      (setf (cadr entry) data)
	    (push (list path data) tabulated-list-entries)))
	(tabulated-list-revert)))))

(defcustom bluez-interface-added-hook nil
  "Hook called when a D-Bus interface is added to a BlueZ object.
Functions on this hook receive arguments (interface properties) where
properties is an alist of all known properties of that interface."
  :type 'hook)

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
      (pcase-dolist (`(,interface . ,properties) interfaces-and-properties)
	(run-hook-with-args
	 'bluez-interface-added-hook interface properties))))
  (message "%s %S" path interfaces-and-properties))

(defvar bluez-interfaces-added-signal
  (bluez-register-signal dbus-interface-objectmanager
			 "InterfacesAdded" #'bluez-interfaces-added-handler))

(defun bluez-report-adapter-property-changes (path interface property value)
  (when (string-equal interface bluez-interface-adapter)
    (pcase property
      ("Discoverable"
       (message (if value
		    "Adapter %s (%s) is now discoverable..."
		  "Adapter %s (%s) is no longer discoverable.")
		(bluez-adapter-name path)
		(bluez-adapter-address path)))
      ("Discovering"
       (message (if value "Adapter %s (%s) is discovering new devices..."
		  "Adapter %s (%s) has stopped discovering new devices.")
		(bluez-adapter-name path)
		(bluez-adapter-address path)))
      ("Pairable"
       (message (if value
		    "Adapter %s (%s) is now pairable..."
		  "Adapter %s (%s) is no longer pairable.")
		(bluez-adapter-name path)
		(bluez-adapter-address path))))))

(defvar bluez-property-changed-hook '(bluez-report-adapter-property-changes)
  "A list of functions to call when a BlueZ D-Bus property has changed.
Arguments to functions: (PATH INTERFACE PROPERTY VALUE).")

(defvar bluez-property-invalidated-hook nil
  "A list of functions to call when a BlueZ D-Bus property was invalidated.
Arguments to functions: (PATH INTERFACE PROPERTY).")

(defun bluez-properties-changed-handler (interface
					 changed-properties invalidated)
  (dolist (property changed-properties)
    (when (cdr property)
      (setf (cdr property) (caadr property))))
  (let ((path (dbus-event-path-name last-input-event)))
    (dolist (property changed-properties)
      (message "PropertyChanged: %s %s.%s=%S"
	       path interface (car property) (cdr property))
      (run-hook-with-args 'bluez-property-changed-hook
			  path interface (car property) (cdr property)))
    (dolist (property invalidated)
      (message "PropertyInvalidated: %s %s.%s" path interface property)
      (run-hook-with-args 'bluez-property-invalidated-hook
			  path interface property))))

(defvar bluez-properties-changed-signal
  (bluez-register-signal dbus-interface-properties
			 "PropertiesChanged"
			 #'bluez-properties-changed-handler))

;;; User Interface

(defun bluetooth-list-devices ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Bluetooth Devices*")
    (bluez-device-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun bluetooth-start-discovery ()
  (interactive)
  (dolist (adapter-path (bluez-adapter-paths))
    (unless (bluez-adapter-discovering-p adapter-path)
      (bluez-adapter-start-discovery adapter-path))))

(defun bluetooth-stop-discovery ()
  (interactive)
  (dolist (adapter-path (bluez-adapter-paths))
    (when (bluez-adapter-discovering-p adapter-path)
      (bluez-adapter-stop-discovery adapter-path))))

(defun bluetooth-make-discoverable (&optional timeout)
  "Make all Bluetooth adapters discoverable.
An automatic timeout (in seconds) can be set by providing a numeric prefix argument."
  (interactive "P")
  (dolist (adapter (bluez-adapter-paths))
    (unless (bluez-adapter-discoverable-p adapter)
      (when (numberp timeout)
        (setf (bluez-adapter-discoverable-timeout adapter) timeout))
      (setf (bluez-adapter-discoverable-p adapter) t))))

(defun bluetooth-make-pairable (&optional timeout)
  "Make all Bluetooth adapters pairable.
If called interactively, an automatic timeout (in seconds) can be set by
providing a numeric prefix argument."
  (interactive "P")
  (bluez-agent-manager-request-default-agent bluez-path-emacs-agent)
  (dolist (adapter (bluez-adapter-paths))
    (unless (bluez-adapter-pairable adapter)
      (when (numberp timeout)
        (setf (bluez-adapter-pairable-timeout adapter) timeout))
      (setf (bluez-adapter-pairable adapter) t))))

(defun bluetooth-initiate-pairing (device)
  (interactive (list (bluez-read-unpaired-device-path "Initiate pairing to: ")))
  (if (not device)
      (when (called-interactively-p 'interactive)
        (message "No unpaired devices found."))
    (bluez-agent-manager-request-default-agent bluez-path-emacs-agent)
    (bluez-device-pair device)))

(defun bluetooth-trust-device (device)
  (interactive (list (bluez-read-unpaired-device-path "Device to trust: ")))
  (if (not device)
      (when (called-interactively-p 'interactive)
	(message "No paired but untrusted devices found."))
    (setf (bluez-device-trusted device) t)))

(provide 'bluez)
;;; bluez.el ends here
