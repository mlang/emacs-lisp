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

(require 'dbus)
(require 'gv)

(defun systemd-escape-dbus-address (string)
  (apply #'concat (mapcar (lambda (c)
                            (if (or (and (>= c ?a) (<= c ?z))
                                    (and (>= c ?A) (<= c ?Z))
                                    (and (>= c ?0) (<= c ?9))
                                    (= c ?-) (= c ?_)
                                    (= c ?/) (= c ?\\)
                                    (= c ?.))
                                (string c)
                              (format "%%%02x" c)))
                          string)))

(defun systemd-unescape-dbus-address (string)
  (while (string-match "%\\([0-9a-f]\\{2\\}\\)" string)
    (setq string
	  (replace-match
	   (string (string-to-number (match-string 1 string) 16)) t t string)))
  string)

(defun systemd-remote-bus (host &optional address)
  (unless address
    (setq address "unix:path=/run/dbus/system_bus_socket"))
  (concat "unixexec:"
          "path=ssh"
          ",argv1=-xT"
          ",argv2=" (systemd-escape-dbus-address host)
          ",argv3=systemd-stdio-bridge"
          ",argv4=" (systemd-escape-dbus-address (concat "--bus-path=" address))))

(defconst systemd-dbus-service "org.freedesktop.systemd1")
(defconst systemd-dbus-path "/org/freedesktop/systemd1")

;;; org.freedesktop.systemd1.Automount

(defconst systemd-dbus-interface-automount "org.freedesktop.systemd1.Automount")

(defun systemd-automount-Where (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-automount "Where"))

(defun systemd-automount-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-automount "DirectoryMode"))

(defun systemd-automount-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-automount "Result"))

(defun systemd-automount-TimeoutIdleUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-automount "TimeoutIdleUSec"))
;;; org.freedesktop.systemd1.BusName

(defconst systemd-dbus-interface-bus-name "org.freedesktop.systemd1.BusName")

(defun systemd-bus-name-Name (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "Name"))

(defun systemd-bus-name-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "TimeoutUSec"))

(defun systemd-bus-name-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "ControlPID"))

(defun systemd-bus-name-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "Result"))

(defun systemd-bus-name-Activating (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "Activating"))

(defun systemd-bus-name-AcceptFileDescriptors (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-bus-name "AcceptFileDescriptors"))
;;; org.freedesktop.systemd1.Device

(defconst systemd-dbus-interface-device "org.freedesktop.systemd1.Device")

(defun systemd-device-SysFSPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-device "SysFSPath"))
;;; org.freedesktop.systemd1.Manager

(defconst systemd-dbus-interface-manager "org.freedesktop.systemd1.Manager")

(defun systemd-Version (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Version"))

(defun systemd-Features (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Features"))

(defun systemd-Virtualization (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Virtualization"))

(defun systemd-Architecture (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Architecture"))

(defun systemd-Tainted (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Tainted"))

(defun systemd-FirmwareTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "FirmwareTimestamp"))

(defun systemd-FirmwareTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "FirmwareTimestampMonotonic"))

(defun systemd-LoaderTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "LoaderTimestamp"))

(defun systemd-LoaderTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "LoaderTimestampMonotonic"))

(defun systemd-KernelTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "KernelTimestamp"))

(defun systemd-KernelTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "KernelTimestampMonotonic"))

(defun systemd-InitRDTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "InitRDTimestamp"))

(defun systemd-InitRDTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "InitRDTimestampMonotonic"))

(defun systemd-UserspaceTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UserspaceTimestamp"))

(defun systemd-UserspaceTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UserspaceTimestampMonotonic"))

(defun systemd-FinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "FinishTimestamp"))

(defun systemd-FinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "FinishTimestampMonotonic"))

(defun systemd-SecurityStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "SecurityStartTimestamp"))

(defun systemd-SecurityStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "SecurityStartTimestampMonotonic"))

(defun systemd-SecurityFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "SecurityFinishTimestamp"))

(defun systemd-SecurityFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "SecurityFinishTimestampMonotonic"))

(defun systemd-GeneratorsStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "GeneratorsStartTimestamp"))

(defun systemd-GeneratorsStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "GeneratorsStartTimestampMonotonic"))

(defun systemd-GeneratorsFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "GeneratorsFinishTimestamp"))

(defun systemd-GeneratorsFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "GeneratorsFinishTimestampMonotonic"))

(defun systemd-UnitsLoadStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UnitsLoadStartTimestamp"))

(defun systemd-UnitsLoadStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UnitsLoadStartTimestampMonotonic"))

(defun systemd-UnitsLoadFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UnitsLoadFinishTimestamp"))

(defun systemd-UnitsLoadFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UnitsLoadFinishTimestampMonotonic"))

(defun systemd-LogLevel (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "LogLevel"))

(gv-define-setter systemd-LogLevel (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      systemd-dbus-interface-manager "LogLevel" ,value))

(defun systemd-LogTarget (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "LogTarget"))

(gv-define-setter systemd-LogTarget (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      systemd-dbus-interface-manager "LogTarget" ,value))

(defun systemd-NNames (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "NNames"))

(defun systemd-NFailedUnits (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "NFailedUnits"))

(defun systemd-NJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "NJobs"))

(defun systemd-NInstalledJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "NInstalledJobs"))

(defun systemd-NFailedJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "NFailedJobs"))

(defun systemd-Progress (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Progress"))

(defun systemd-Environment (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "Environment"))

(defun systemd-ConfirmSpawn (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "ConfirmSpawn"))

(defun systemd-ShowStatus (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "ShowStatus"))

(defun systemd-UnitPath (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "UnitPath"))

(defun systemd-DefaultStandardOutput (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultStandardOutput"))

(defun systemd-DefaultStandardError (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultStandardError"))

(defun systemd-RuntimeWatchdogUSec (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "RuntimeWatchdogUSec"))

(gv-define-setter systemd-RuntimeWatchdogUSec (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      systemd-dbus-interface-manager "RuntimeWatchdogUSec" ,value))

(defun systemd-ShutdownWatchdogUSec (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "ShutdownWatchdogUSec"))

(gv-define-setter systemd-ShutdownWatchdogUSec (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      systemd-dbus-interface-manager "ShutdownWatchdogUSec" ,value))

(defun systemd-ControlGroup (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "ControlGroup"))

(defun systemd-SystemState (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "SystemState"))

(defun systemd-ExitCode (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "ExitCode"))

(defun systemd-DefaultTimerAccuracyUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultTimerAccuracyUSec"))

(defun systemd-DefaultTimeoutStartUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultTimeoutStartUSec"))

(defun systemd-DefaultTimeoutStopUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultTimeoutStopUSec"))

(defun systemd-DefaultRestartUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultRestartUSec"))

(defun systemd-DefaultStartLimitIntervalSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultStartLimitIntervalSec"))

(defun systemd-DefaultStartLimitBurst (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultStartLimitBurst"))

(defun systemd-DefaultCPUAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultCPUAccounting"))

(defun systemd-DefaultBlockIOAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultBlockIOAccounting"))

(defun systemd-DefaultMemoryAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultMemoryAccounting"))

(defun systemd-DefaultTasksAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultTasksAccounting"))

(defun systemd-DefaultLimitCPU (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitCPU"))

(defun systemd-DefaultLimitCPUSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitCPUSoft"))

(defun systemd-DefaultLimitFSIZE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitFSIZE"))

(defun systemd-DefaultLimitFSIZESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitFSIZESoft"))

(defun systemd-DefaultLimitDATA (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitDATA"))

(defun systemd-DefaultLimitDATASoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitDATASoft"))

(defun systemd-DefaultLimitSTACK (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitSTACK"))

(defun systemd-DefaultLimitSTACKSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitSTACKSoft"))

(defun systemd-DefaultLimitCORE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitCORE"))

(defun systemd-DefaultLimitCORESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitCORESoft"))

(defun systemd-DefaultLimitRSS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRSS"))

(defun systemd-DefaultLimitRSSSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRSSSoft"))

(defun systemd-DefaultLimitNOFILE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNOFILE"))

(defun systemd-DefaultLimitNOFILESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNOFILESoft"))

(defun systemd-DefaultLimitAS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitAS"))

(defun systemd-DefaultLimitASSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitASSoft"))

(defun systemd-DefaultLimitNPROC (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNPROC"))

(defun systemd-DefaultLimitNPROCSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNPROCSoft"))

(defun systemd-DefaultLimitMEMLOCK (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitMEMLOCK"))

(defun systemd-DefaultLimitMEMLOCKSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitMEMLOCKSoft"))

(defun systemd-DefaultLimitLOCKS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitLOCKS"))

(defun systemd-DefaultLimitLOCKSSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitLOCKSSoft"))

(defun systemd-DefaultLimitSIGPENDING (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitSIGPENDING"))

(defun systemd-DefaultLimitSIGPENDINGSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitSIGPENDINGSoft"))

(defun systemd-DefaultLimitMSGQUEUE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitMSGQUEUE"))

(defun systemd-DefaultLimitMSGQUEUESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitMSGQUEUESoft"))

(defun systemd-DefaultLimitNICE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNICE"))

(defun systemd-DefaultLimitNICESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitNICESoft"))

(defun systemd-DefaultLimitRTPRIO (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRTPRIO"))

(defun systemd-DefaultLimitRTPRIOSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRTPRIOSoft"))

(defun systemd-DefaultLimitRTTIME (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRTTIME"))

(defun systemd-DefaultLimitRTTIMESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultLimitRTTIMESoft"))

(defun systemd-DefaultTasksMax (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "DefaultTasksMax"))

(defun systemd-TimerSlackNSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     systemd-dbus-interface-manager "TimerSlackNSec"))

(defun systemd-GetUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetUnit" args))

(defun systemd-GetUnitByPID (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetUnitByPID" args))

(defun systemd-LoadUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "LoadUnit" args))

(defun systemd-StartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "StartUnit" args))

(defun systemd-StartUnitReplace (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "StartUnitReplace" args))

(defun systemd-StopUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "StopUnit" args))

(defun systemd-ReloadUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ReloadUnit" args))

(defun systemd-RestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "RestartUnit" args))

(defun systemd-TryRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "TryRestartUnit" args))

(defun systemd-ReloadOrRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ReloadOrRestartUnit" args))

(defun systemd-ReloadOrTryRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ReloadOrTryRestartUnit" args))

(defun systemd-KillUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "KillUnit" args))

(defun systemd-ResetFailedUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ResetFailedUnit" args))

(defun systemd-SetUnitProperties (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "SetUnitProperties" args))

(defun systemd-StartTransientUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "StartTransientUnit" args))

(defun systemd-GetUnitProcesses (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetUnitProcesses" args))

(defun systemd-GetJob (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetJob" args))

(defun systemd-CancelJob (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "CancelJob" args))

(defun systemd-ClearJobs (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ClearJobs" args))

(defun systemd-ResetFailed (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ResetFailed" args))

(defun systemd-ListUnits (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnits" args))

(defun systemd-ListUnitsFiltered (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnitsFiltered" args))

(defun systemd-ListUnitsByPatterns (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnitsByPatterns" args))

(defun systemd-ListUnitsByNames (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnitsByNames" args))

(defun systemd-ListJobs (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListJobs" args))

(defun systemd-Subscribe (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Subscribe" args))

(defun systemd-Unsubscribe (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Unsubscribe" args))

(defun systemd-Dump (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Dump" args))

(defun systemd-CreateSnapshot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "CreateSnapshot" args))

(defun systemd-RemoveSnapshot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "RemoveSnapshot" args))

(defun systemd-Reload (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Reload" args))

(defun systemd-Reexecute (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Reexecute" args))

(defun systemd-Exit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Exit" args))

(defun systemd-Reboot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Reboot" args))

(defun systemd-PowerOff (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "PowerOff" args))

(defun systemd-Halt (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "Halt" args))

(defun systemd-KExec (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "KExec" args))

(defun systemd-SwitchRoot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "SwitchRoot" args))

(defun systemd-SetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "SetEnvironment" args))

(defun systemd-UnsetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "UnsetEnvironment" args))

(defun systemd-UnsetAndSetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "UnsetAndSetEnvironment" args))

(defun systemd-ListUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnitFiles" args))

(defun systemd-ListUnitFilesByPatterns (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ListUnitFilesByPatterns" args))

(defun systemd-GetUnitFileState (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetUnitFileState" args))

(defun systemd-EnableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "EnableUnitFiles" args))

(defun systemd-DisableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "DisableUnitFiles" args))

(defun systemd-ReenableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "ReenableUnitFiles" args))

(defun systemd-LinkUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "LinkUnitFiles" args))

(defun systemd-PresetUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "PresetUnitFiles" args))

(defun systemd-PresetUnitFilesWithMode (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "PresetUnitFilesWithMode" args))

(defun systemd-MaskUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "MaskUnitFiles" args))

(defun systemd-UnmaskUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "UnmaskUnitFiles" args))

(defun systemd-RevertUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "RevertUnitFiles" args))

(defun systemd-SetDefaultTarget (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "SetDefaultTarget" args))

(defun systemd-GetDefaultTarget (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "GetDefaultTarget" args))

(defun systemd-PresetAllUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "PresetAllUnitFiles" args))

(defun systemd-AddDependencyUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "AddDependencyUnitFiles" args))

(defun systemd-SetExitCode (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 systemd-dbus-interface-manager "SetExitCode" args))
;;; org.freedesktop.systemd1.Mount

(defconst systemd-dbus-interface-mount "org.freedesktop.systemd1.Mount")

(defun systemd-mount-Where (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Where"))

(defun systemd-mount-What (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "What"))

(defun systemd-mount-Options (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Options"))

(defun systemd-mount-Type (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Type"))

(defun systemd-mount-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TimeoutUSec"))

(defun systemd-mount-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ControlPID"))

(defun systemd-mount-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "DirectoryMode"))

(defun systemd-mount-SloppyOptions (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SloppyOptions"))

(defun systemd-mount-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Result"))

(defun systemd-mount-ExecMount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ExecMount"))

(defun systemd-mount-ExecUnmount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ExecUnmount"))

(defun systemd-mount-ExecRemount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ExecRemount"))

(defun systemd-mount-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Slice"))

(defun systemd-mount-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ControlGroup"))

(defun systemd-mount-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "MemoryCurrent"))

(defun systemd-mount-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUUsageNSec"))

(defun systemd-mount-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TasksCurrent"))

(defun systemd-mount-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-mount "GetProcesses" args))

(defun systemd-mount-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Delegate"))

(defun systemd-mount-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUAccounting"))

(defun systemd-mount-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUShares"))

(defun systemd-mount-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StartupCPUShares"))

(defun systemd-mount-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUQuotaPerSecUSec"))

(defun systemd-mount-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOAccounting"))

(defun systemd-mount-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOWeight"))

(defun systemd-mount-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StartupIOWeight"))

(defun systemd-mount-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IODeviceWeight"))

(defun systemd-mount-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOReadBandwidthMax"))

(defun systemd-mount-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOWriteBandwidthMax"))

(defun systemd-mount-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOReadIOPSMax"))

(defun systemd-mount-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOWriteIOPSMax"))

(defun systemd-mount-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "BlockIOAccounting"))

(defun systemd-mount-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "BlockIOWeight"))

(defun systemd-mount-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StartupBlockIOWeight"))

(defun systemd-mount-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "BlockIODeviceWeight"))

(defun systemd-mount-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "BlockIOReadBandwidth"))

(defun systemd-mount-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "BlockIOWriteBandwidth"))

(defun systemd-mount-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "MemoryAccounting"))

(defun systemd-mount-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "MemoryLimit"))

(defun systemd-mount-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "DevicePolicy"))

(defun systemd-mount-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "DeviceAllow"))

(defun systemd-mount-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TasksAccounting"))

(defun systemd-mount-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TasksMax"))

(defun systemd-mount-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Environment"))

(defun systemd-mount-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "EnvironmentFiles"))

(defun systemd-mount-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "PassEnvironment"))

(defun systemd-mount-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "UMask"))

(defun systemd-mount-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitCPU"))

(defun systemd-mount-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitCPUSoft"))

(defun systemd-mount-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitFSIZE"))

(defun systemd-mount-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitFSIZESoft"))

(defun systemd-mount-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitDATA"))

(defun systemd-mount-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitDATASoft"))

(defun systemd-mount-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitSTACK"))

(defun systemd-mount-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitSTACKSoft"))

(defun systemd-mount-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitCORE"))

(defun systemd-mount-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitCORESoft"))

(defun systemd-mount-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRSS"))

(defun systemd-mount-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRSSSoft"))

(defun systemd-mount-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNOFILE"))

(defun systemd-mount-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNOFILESoft"))

(defun systemd-mount-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitAS"))

(defun systemd-mount-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitASSoft"))

(defun systemd-mount-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNPROC"))

(defun systemd-mount-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNPROCSoft"))

(defun systemd-mount-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitMEMLOCK"))

(defun systemd-mount-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitMEMLOCKSoft"))

(defun systemd-mount-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitLOCKS"))

(defun systemd-mount-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitLOCKSSoft"))

(defun systemd-mount-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitSIGPENDING"))

(defun systemd-mount-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitSIGPENDINGSoft"))

(defun systemd-mount-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitMSGQUEUE"))

(defun systemd-mount-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitMSGQUEUESoft"))

(defun systemd-mount-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNICE"))

(defun systemd-mount-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitNICESoft"))

(defun systemd-mount-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRTPRIO"))

(defun systemd-mount-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRTPRIOSoft"))

(defun systemd-mount-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRTTIME"))

(defun systemd-mount-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "LimitRTTIMESoft"))

(defun systemd-mount-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "WorkingDirectory"))

(defun systemd-mount-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "RootDirectory"))

(defun systemd-mount-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "OOMScoreAdjust"))

(defun systemd-mount-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Nice"))

(defun systemd-mount-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IOScheduling"))

(defun systemd-mount-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUSchedulingPolicy"))

(defun systemd-mount-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUSchedulingPriority"))

(defun systemd-mount-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUAffinity"))

(defun systemd-mount-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TimerSlackNSec"))

(defun systemd-mount-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CPUSchedulingResetOnFork"))

(defun systemd-mount-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "NonBlocking"))

(defun systemd-mount-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StandardInput"))

(defun systemd-mount-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StandardOutput"))

(defun systemd-mount-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "StandardError"))

(defun systemd-mount-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TTYPath"))

(defun systemd-mount-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TTYReset"))

(defun systemd-mount-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TTYVHangup"))

(defun systemd-mount-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "TTYVTDisallocate"))

(defun systemd-mount-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SyslogPriority"))

(defun systemd-mount-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SyslogIdentifier"))

(defun systemd-mount-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SyslogLevelPrefix"))

(defun systemd-mount-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SyslogLevel"))

(defun systemd-mount-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SyslogFacility"))

(defun systemd-mount-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SecureBits"))

(defun systemd-mount-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "CapabilityBoundingSet"))

(defun systemd-mount-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "AmbientCapabilities"))

(defun systemd-mount-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "User"))

(defun systemd-mount-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Group"))

(defun systemd-mount-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SupplementaryGroups"))

(defun systemd-mount-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "PAMName"))

(defun systemd-mount-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ReadWriteDirectories"))

(defun systemd-mount-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ReadOnlyDirectories"))

(defun systemd-mount-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "InaccessibleDirectories"))

(defun systemd-mount-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "MountFlags"))

(defun systemd-mount-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "PrivateTmp"))

(defun systemd-mount-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "PrivateNetwork"))

(defun systemd-mount-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "PrivateDevices"))

(defun systemd-mount-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ProtectHome"))

(defun systemd-mount-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "ProtectSystem"))

(defun systemd-mount-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SameProcessGroup"))

(defun systemd-mount-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "UtmpIdentifier"))

(defun systemd-mount-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "UtmpMode"))

(defun systemd-mount-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SELinuxContext"))

(defun systemd-mount-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "AppArmorProfile"))

(defun systemd-mount-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SmackProcessLabel"))

(defun systemd-mount-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "IgnoreSIGPIPE"))

(defun systemd-mount-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "NoNewPrivileges"))

(defun systemd-mount-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SystemCallFilter"))

(defun systemd-mount-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SystemCallArchitectures"))

(defun systemd-mount-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SystemCallErrorNumber"))

(defun systemd-mount-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "Personality"))

(defun systemd-mount-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "RestrictAddressFamilies"))

(defun systemd-mount-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "RuntimeDirectoryMode"))

(defun systemd-mount-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "RuntimeDirectory"))

(defun systemd-mount-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "KillMode"))

(defun systemd-mount-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "KillSignal"))

(defun systemd-mount-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SendSIGKILL"))

(defun systemd-mount-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-mount "SendSIGHUP"))
;;; org.freedesktop.systemd1.Path

(defconst systemd-dbus-interface-path "org.freedesktop.systemd1.Path")

(defun systemd-path-Unit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-path "Unit"))

(defun systemd-path-Paths (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-path "Paths"))

(defun systemd-path-MakeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-path "MakeDirectory"))

(defun systemd-path-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-path "DirectoryMode"))

(defun systemd-path-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-path "Result"))
;;; org.freedesktop.systemd1.Scope

(defconst systemd-dbus-interface-scope "org.freedesktop.systemd1.Scope")

(defun systemd-scope-Controller (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "Controller"))

(defun systemd-scope-TimeoutStopUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "TimeoutStopUSec"))

(defun systemd-scope-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "Result"))

(defun systemd-scope-Abandon (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-scope "Abandon" args))

(defun systemd-scope-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "Slice"))

(defun systemd-scope-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "ControlGroup"))

(defun systemd-scope-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "MemoryCurrent"))

(defun systemd-scope-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "CPUUsageNSec"))

(defun systemd-scope-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "TasksCurrent"))

(defun systemd-scope-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-scope "GetProcesses" args))

(defun systemd-scope-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "Delegate"))

(defun systemd-scope-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "CPUAccounting"))

(defun systemd-scope-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "CPUShares"))

(defun systemd-scope-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "StartupCPUShares"))

(defun systemd-scope-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "CPUQuotaPerSecUSec"))

(defun systemd-scope-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOAccounting"))

(defun systemd-scope-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOWeight"))

(defun systemd-scope-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "StartupIOWeight"))

(defun systemd-scope-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IODeviceWeight"))

(defun systemd-scope-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOReadBandwidthMax"))

(defun systemd-scope-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOWriteBandwidthMax"))

(defun systemd-scope-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOReadIOPSMax"))

(defun systemd-scope-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "IOWriteIOPSMax"))

(defun systemd-scope-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "BlockIOAccounting"))

(defun systemd-scope-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "BlockIOWeight"))

(defun systemd-scope-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "StartupBlockIOWeight"))

(defun systemd-scope-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "BlockIODeviceWeight"))

(defun systemd-scope-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "BlockIOReadBandwidth"))

(defun systemd-scope-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "BlockIOWriteBandwidth"))

(defun systemd-scope-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "MemoryAccounting"))

(defun systemd-scope-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "MemoryLimit"))

(defun systemd-scope-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "DevicePolicy"))

(defun systemd-scope-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "DeviceAllow"))

(defun systemd-scope-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "TasksAccounting"))

(defun systemd-scope-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "TasksMax"))

(defun systemd-scope-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "KillMode"))

(defun systemd-scope-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "KillSignal"))

(defun systemd-scope-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "SendSIGKILL"))

(defun systemd-scope-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-scope "SendSIGHUP"))
;;; org.freedesktop.systemd1.Service

(defconst systemd-dbus-interface-service "org.freedesktop.systemd1.Service")

(defun systemd-service-Type (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Type"))

(defun systemd-service-Restart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Restart"))

(defun systemd-service-PIDFile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PIDFile"))

(defun systemd-service-NotifyAccess (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "NotifyAccess"))

(defun systemd-service-RestartUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RestartUSec"))

(defun systemd-service-TimeoutStartUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TimeoutStartUSec"))

(defun systemd-service-TimeoutStopUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TimeoutStopUSec"))

(defun systemd-service-RuntimeMaxUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RuntimeMaxUSec"))

(defun systemd-service-WatchdogUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "WatchdogUSec"))

(defun systemd-service-WatchdogTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "WatchdogTimestamp"))

(defun systemd-service-WatchdogTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "WatchdogTimestampMonotonic"))

(defun systemd-service-FailureAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "FailureAction"))

(defun systemd-service-PermissionsStartOnly (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PermissionsStartOnly"))

(defun systemd-service-RootDirectoryStartOnly (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RootDirectoryStartOnly"))

(defun systemd-service-RemainAfterExit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RemainAfterExit"))

(defun systemd-service-GuessMainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "GuessMainPID"))

(defun systemd-service-MainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "MainPID"))

(defun systemd-service-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ControlPID"))

(defun systemd-service-BusName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BusName"))

(defun systemd-service-FileDescriptorStoreMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "FileDescriptorStoreMax"))

(defun systemd-service-NFileDescriptorStore (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "NFileDescriptorStore"))

(defun systemd-service-StatusText (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StatusText"))

(defun systemd-service-StatusErrno (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StatusErrno"))

(defun systemd-service-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Result"))

(defun systemd-service-USBFunctionDescriptors (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "USBFunctionDescriptors"))

(defun systemd-service-USBFunctionStrings (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "USBFunctionStrings"))

(defun systemd-service-ExecMainStartTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainStartTimestamp"))

(defun systemd-service-ExecMainStartTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainStartTimestampMonotonic"))

(defun systemd-service-ExecMainExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainExitTimestamp"))

(defun systemd-service-ExecMainExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainExitTimestampMonotonic"))

(defun systemd-service-ExecMainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainPID"))

(defun systemd-service-ExecMainCode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainCode"))

(defun systemd-service-ExecMainStatus (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecMainStatus"))

(defun systemd-service-ExecStartPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecStartPre"))

(defun systemd-service-ExecStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecStart"))

(defun systemd-service-ExecStartPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecStartPost"))

(defun systemd-service-ExecReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecReload"))

(defun systemd-service-ExecStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecStop"))

(defun systemd-service-ExecStopPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ExecStopPost"))

(defun systemd-service-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Slice"))

(defun systemd-service-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ControlGroup"))

(defun systemd-service-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "MemoryCurrent"))

(defun systemd-service-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUUsageNSec"))

(defun systemd-service-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TasksCurrent"))

(defun systemd-service-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-service "GetProcesses" args))

(defun systemd-service-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Delegate"))

(defun systemd-service-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUAccounting"))

(defun systemd-service-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUShares"))

(defun systemd-service-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StartupCPUShares"))

(defun systemd-service-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUQuotaPerSecUSec"))

(defun systemd-service-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOAccounting"))

(defun systemd-service-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOWeight"))

(defun systemd-service-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StartupIOWeight"))

(defun systemd-service-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IODeviceWeight"))

(defun systemd-service-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOReadBandwidthMax"))

(defun systemd-service-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOWriteBandwidthMax"))

(defun systemd-service-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOReadIOPSMax"))

(defun systemd-service-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOWriteIOPSMax"))

(defun systemd-service-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BlockIOAccounting"))

(defun systemd-service-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BlockIOWeight"))

(defun systemd-service-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StartupBlockIOWeight"))

(defun systemd-service-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BlockIODeviceWeight"))

(defun systemd-service-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BlockIOReadBandwidth"))

(defun systemd-service-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "BlockIOWriteBandwidth"))

(defun systemd-service-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "MemoryAccounting"))

(defun systemd-service-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "MemoryLimit"))

(defun systemd-service-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "DevicePolicy"))

(defun systemd-service-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "DeviceAllow"))

(defun systemd-service-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TasksAccounting"))

(defun systemd-service-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TasksMax"))

(defun systemd-service-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Environment"))

(defun systemd-service-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "EnvironmentFiles"))

(defun systemd-service-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PassEnvironment"))

(defun systemd-service-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "UMask"))

(defun systemd-service-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitCPU"))

(defun systemd-service-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitCPUSoft"))

(defun systemd-service-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitFSIZE"))

(defun systemd-service-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitFSIZESoft"))

(defun systemd-service-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitDATA"))

(defun systemd-service-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitDATASoft"))

(defun systemd-service-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitSTACK"))

(defun systemd-service-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitSTACKSoft"))

(defun systemd-service-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitCORE"))

(defun systemd-service-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitCORESoft"))

(defun systemd-service-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRSS"))

(defun systemd-service-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRSSSoft"))

(defun systemd-service-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNOFILE"))

(defun systemd-service-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNOFILESoft"))

(defun systemd-service-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitAS"))

(defun systemd-service-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitASSoft"))

(defun systemd-service-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNPROC"))

(defun systemd-service-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNPROCSoft"))

(defun systemd-service-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitMEMLOCK"))

(defun systemd-service-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitMEMLOCKSoft"))

(defun systemd-service-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitLOCKS"))

(defun systemd-service-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitLOCKSSoft"))

(defun systemd-service-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitSIGPENDING"))

(defun systemd-service-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitSIGPENDINGSoft"))

(defun systemd-service-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitMSGQUEUE"))

(defun systemd-service-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitMSGQUEUESoft"))

(defun systemd-service-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNICE"))

(defun systemd-service-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitNICESoft"))

(defun systemd-service-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRTPRIO"))

(defun systemd-service-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRTPRIOSoft"))

(defun systemd-service-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRTTIME"))

(defun systemd-service-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "LimitRTTIMESoft"))

(defun systemd-service-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "WorkingDirectory"))

(defun systemd-service-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RootDirectory"))

(defun systemd-service-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "OOMScoreAdjust"))

(defun systemd-service-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Nice"))

(defun systemd-service-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IOScheduling"))

(defun systemd-service-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUSchedulingPolicy"))

(defun systemd-service-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUSchedulingPriority"))

(defun systemd-service-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUAffinity"))

(defun systemd-service-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TimerSlackNSec"))

(defun systemd-service-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CPUSchedulingResetOnFork"))

(defun systemd-service-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "NonBlocking"))

(defun systemd-service-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StandardInput"))

(defun systemd-service-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StandardOutput"))

(defun systemd-service-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "StandardError"))

(defun systemd-service-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TTYPath"))

(defun systemd-service-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TTYReset"))

(defun systemd-service-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TTYVHangup"))

(defun systemd-service-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "TTYVTDisallocate"))

(defun systemd-service-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SyslogPriority"))

(defun systemd-service-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SyslogIdentifier"))

(defun systemd-service-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SyslogLevelPrefix"))

(defun systemd-service-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SyslogLevel"))

(defun systemd-service-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SyslogFacility"))

(defun systemd-service-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SecureBits"))

(defun systemd-service-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "CapabilityBoundingSet"))

(defun systemd-service-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "AmbientCapabilities"))

(defun systemd-service-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "User"))

(defun systemd-service-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Group"))

(defun systemd-service-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SupplementaryGroups"))

(defun systemd-service-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PAMName"))

(defun systemd-service-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ReadWriteDirectories"))

(defun systemd-service-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ReadOnlyDirectories"))

(defun systemd-service-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "InaccessibleDirectories"))

(defun systemd-service-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "MountFlags"))

(defun systemd-service-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PrivateTmp"))

(defun systemd-service-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PrivateNetwork"))

(defun systemd-service-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "PrivateDevices"))

(defun systemd-service-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ProtectHome"))

(defun systemd-service-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "ProtectSystem"))

(defun systemd-service-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SameProcessGroup"))

(defun systemd-service-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "UtmpIdentifier"))

(defun systemd-service-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "UtmpMode"))

(defun systemd-service-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SELinuxContext"))

(defun systemd-service-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "AppArmorProfile"))

(defun systemd-service-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SmackProcessLabel"))

(defun systemd-service-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "IgnoreSIGPIPE"))

(defun systemd-service-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "NoNewPrivileges"))

(defun systemd-service-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SystemCallFilter"))

(defun systemd-service-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SystemCallArchitectures"))

(defun systemd-service-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SystemCallErrorNumber"))

(defun systemd-service-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "Personality"))

(defun systemd-service-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RestrictAddressFamilies"))

(defun systemd-service-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RuntimeDirectoryMode"))

(defun systemd-service-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "RuntimeDirectory"))

(defun systemd-service-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "KillMode"))

(defun systemd-service-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "KillSignal"))

(defun systemd-service-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SendSIGKILL"))

(defun systemd-service-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-service "SendSIGHUP"))
;;; org.freedesktop.systemd1.Slice

(defconst systemd-dbus-interface-slice "org.freedesktop.systemd1.Slice")

(defun systemd-slice-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "Slice"))

(defun systemd-slice-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "ControlGroup"))

(defun systemd-slice-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "MemoryCurrent"))

(defun systemd-slice-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "CPUUsageNSec"))

(defun systemd-slice-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "TasksCurrent"))

(defun systemd-slice-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-slice "GetProcesses" args))

(defun systemd-slice-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "Delegate"))

(defun systemd-slice-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "CPUAccounting"))

(defun systemd-slice-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "CPUShares"))

(defun systemd-slice-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "StartupCPUShares"))

(defun systemd-slice-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "CPUQuotaPerSecUSec"))

(defun systemd-slice-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOAccounting"))

(defun systemd-slice-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOWeight"))

(defun systemd-slice-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "StartupIOWeight"))

(defun systemd-slice-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IODeviceWeight"))

(defun systemd-slice-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOReadBandwidthMax"))

(defun systemd-slice-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOWriteBandwidthMax"))

(defun systemd-slice-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOReadIOPSMax"))

(defun systemd-slice-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "IOWriteIOPSMax"))

(defun systemd-slice-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "BlockIOAccounting"))

(defun systemd-slice-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "BlockIOWeight"))

(defun systemd-slice-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "StartupBlockIOWeight"))

(defun systemd-slice-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "BlockIODeviceWeight"))

(defun systemd-slice-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "BlockIOReadBandwidth"))

(defun systemd-slice-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "BlockIOWriteBandwidth"))

(defun systemd-slice-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "MemoryAccounting"))

(defun systemd-slice-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "MemoryLimit"))

(defun systemd-slice-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "DevicePolicy"))

(defun systemd-slice-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "DeviceAllow"))

(defun systemd-slice-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "TasksAccounting"))

(defun systemd-slice-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-slice "TasksMax"))
;;; org.freedesktop.systemd1.Socket

(defconst systemd-dbus-interface-socket "org.freedesktop.systemd1.Socket")

(defun systemd-socket-BindIPv6Only (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BindIPv6Only"))

(defun systemd-socket-Backlog (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Backlog"))

(defun systemd-socket-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TimeoutUSec"))

(defun systemd-socket-BindToDevice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BindToDevice"))

(defun systemd-socket-SocketUser (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SocketUser"))

(defun systemd-socket-SocketGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SocketGroup"))

(defun systemd-socket-SocketMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SocketMode"))

(defun systemd-socket-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "DirectoryMode"))

(defun systemd-socket-Accept (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Accept"))

(defun systemd-socket-Writable (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Writable"))

(defun systemd-socket-KeepAlive (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KeepAlive"))

(defun systemd-socket-KeepAliveTimeUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KeepAliveTimeUSec"))

(defun systemd-socket-KeepAliveIntervalUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KeepAliveIntervalUSec"))

(defun systemd-socket-KeepAliveProbes (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KeepAliveProbes"))

(defun systemd-socket-DeferAcceptUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "DeferAcceptUSec"))

(defun systemd-socket-NoDelay (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "NoDelay"))

(defun systemd-socket-Priority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Priority"))

(defun systemd-socket-ReceiveBuffer (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ReceiveBuffer"))

(defun systemd-socket-SendBuffer (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SendBuffer"))

(defun systemd-socket-IPTOS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IPTOS"))

(defun systemd-socket-IPTTL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IPTTL"))

(defun systemd-socket-PipeSize (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PipeSize"))

(defun systemd-socket-FreeBind (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "FreeBind"))

(defun systemd-socket-Transparent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Transparent"))

(defun systemd-socket-Broadcast (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Broadcast"))

(defun systemd-socket-PassCredentials (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PassCredentials"))

(defun systemd-socket-PassSecurity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PassSecurity"))

(defun systemd-socket-RemoveOnStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "RemoveOnStop"))

(defun systemd-socket-Listen (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Listen"))

(defun systemd-socket-Symlinks (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Symlinks"))

(defun systemd-socket-Mark (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Mark"))

(defun systemd-socket-MaxConnections (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MaxConnections"))

(defun systemd-socket-MessageQueueMaxMessages (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MessageQueueMaxMessages"))

(defun systemd-socket-MessageQueueMessageSize (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MessageQueueMessageSize"))

(defun systemd-socket-ReusePort (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ReusePort"))

(defun systemd-socket-SmackLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SmackLabel"))

(defun systemd-socket-SmackLabelIPIn (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SmackLabelIPIn"))

(defun systemd-socket-SmackLabelIPOut (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SmackLabelIPOut"))

(defun systemd-socket-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ControlPID"))

(defun systemd-socket-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Result"))

(defun systemd-socket-NConnections (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "NConnections"))

(defun systemd-socket-NAccepted (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "NAccepted"))

(defun systemd-socket-FileDescriptorName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "FileDescriptorName"))

(defun systemd-socket-SocketProtocol (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SocketProtocol"))

(defun systemd-socket-TriggerLimitIntervalUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TriggerLimitIntervalUSec"))

(defun systemd-socket-TriggerLimitBurst (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TriggerLimitBurst"))

(defun systemd-socket-ExecStartPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ExecStartPre"))

(defun systemd-socket-ExecStartPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ExecStartPost"))

(defun systemd-socket-ExecStopPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ExecStopPre"))

(defun systemd-socket-ExecStopPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ExecStopPost"))

(defun systemd-socket-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Slice"))

(defun systemd-socket-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ControlGroup"))

(defun systemd-socket-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MemoryCurrent"))

(defun systemd-socket-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUUsageNSec"))

(defun systemd-socket-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TasksCurrent"))

(defun systemd-socket-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-socket "GetProcesses" args))

(defun systemd-socket-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Delegate"))

(defun systemd-socket-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUAccounting"))

(defun systemd-socket-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUShares"))

(defun systemd-socket-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StartupCPUShares"))

(defun systemd-socket-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUQuotaPerSecUSec"))

(defun systemd-socket-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOAccounting"))

(defun systemd-socket-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOWeight"))

(defun systemd-socket-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StartupIOWeight"))

(defun systemd-socket-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IODeviceWeight"))

(defun systemd-socket-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOReadBandwidthMax"))

(defun systemd-socket-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOWriteBandwidthMax"))

(defun systemd-socket-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOReadIOPSMax"))

(defun systemd-socket-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOWriteIOPSMax"))

(defun systemd-socket-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BlockIOAccounting"))

(defun systemd-socket-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BlockIOWeight"))

(defun systemd-socket-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StartupBlockIOWeight"))

(defun systemd-socket-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BlockIODeviceWeight"))

(defun systemd-socket-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BlockIOReadBandwidth"))

(defun systemd-socket-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "BlockIOWriteBandwidth"))

(defun systemd-socket-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MemoryAccounting"))

(defun systemd-socket-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MemoryLimit"))

(defun systemd-socket-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "DevicePolicy"))

(defun systemd-socket-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "DeviceAllow"))

(defun systemd-socket-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TasksAccounting"))

(defun systemd-socket-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TasksMax"))

(defun systemd-socket-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Environment"))

(defun systemd-socket-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "EnvironmentFiles"))

(defun systemd-socket-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PassEnvironment"))

(defun systemd-socket-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "UMask"))

(defun systemd-socket-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitCPU"))

(defun systemd-socket-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitCPUSoft"))

(defun systemd-socket-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitFSIZE"))

(defun systemd-socket-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitFSIZESoft"))

(defun systemd-socket-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitDATA"))

(defun systemd-socket-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitDATASoft"))

(defun systemd-socket-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitSTACK"))

(defun systemd-socket-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitSTACKSoft"))

(defun systemd-socket-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitCORE"))

(defun systemd-socket-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitCORESoft"))

(defun systemd-socket-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRSS"))

(defun systemd-socket-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRSSSoft"))

(defun systemd-socket-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNOFILE"))

(defun systemd-socket-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNOFILESoft"))

(defun systemd-socket-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitAS"))

(defun systemd-socket-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitASSoft"))

(defun systemd-socket-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNPROC"))

(defun systemd-socket-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNPROCSoft"))

(defun systemd-socket-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitMEMLOCK"))

(defun systemd-socket-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitMEMLOCKSoft"))

(defun systemd-socket-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitLOCKS"))

(defun systemd-socket-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitLOCKSSoft"))

(defun systemd-socket-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitSIGPENDING"))

(defun systemd-socket-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitSIGPENDINGSoft"))

(defun systemd-socket-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitMSGQUEUE"))

(defun systemd-socket-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitMSGQUEUESoft"))

(defun systemd-socket-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNICE"))

(defun systemd-socket-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitNICESoft"))

(defun systemd-socket-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRTPRIO"))

(defun systemd-socket-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRTPRIOSoft"))

(defun systemd-socket-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRTTIME"))

(defun systemd-socket-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "LimitRTTIMESoft"))

(defun systemd-socket-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "WorkingDirectory"))

(defun systemd-socket-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "RootDirectory"))

(defun systemd-socket-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "OOMScoreAdjust"))

(defun systemd-socket-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Nice"))

(defun systemd-socket-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IOScheduling"))

(defun systemd-socket-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUSchedulingPolicy"))

(defun systemd-socket-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUSchedulingPriority"))

(defun systemd-socket-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUAffinity"))

(defun systemd-socket-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TimerSlackNSec"))

(defun systemd-socket-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CPUSchedulingResetOnFork"))

(defun systemd-socket-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "NonBlocking"))

(defun systemd-socket-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StandardInput"))

(defun systemd-socket-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StandardOutput"))

(defun systemd-socket-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "StandardError"))

(defun systemd-socket-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TTYPath"))

(defun systemd-socket-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TTYReset"))

(defun systemd-socket-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TTYVHangup"))

(defun systemd-socket-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "TTYVTDisallocate"))

(defun systemd-socket-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SyslogPriority"))

(defun systemd-socket-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SyslogIdentifier"))

(defun systemd-socket-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SyslogLevelPrefix"))

(defun systemd-socket-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SyslogLevel"))

(defun systemd-socket-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SyslogFacility"))

(defun systemd-socket-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SecureBits"))

(defun systemd-socket-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "CapabilityBoundingSet"))

(defun systemd-socket-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "AmbientCapabilities"))

(defun systemd-socket-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "User"))

(defun systemd-socket-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Group"))

(defun systemd-socket-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SupplementaryGroups"))

(defun systemd-socket-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PAMName"))

(defun systemd-socket-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ReadWriteDirectories"))

(defun systemd-socket-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ReadOnlyDirectories"))

(defun systemd-socket-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "InaccessibleDirectories"))

(defun systemd-socket-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "MountFlags"))

(defun systemd-socket-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PrivateTmp"))

(defun systemd-socket-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PrivateNetwork"))

(defun systemd-socket-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "PrivateDevices"))

(defun systemd-socket-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ProtectHome"))

(defun systemd-socket-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "ProtectSystem"))

(defun systemd-socket-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SameProcessGroup"))

(defun systemd-socket-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "UtmpIdentifier"))

(defun systemd-socket-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "UtmpMode"))

(defun systemd-socket-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SELinuxContext"))

(defun systemd-socket-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "AppArmorProfile"))

(defun systemd-socket-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SmackProcessLabel"))

(defun systemd-socket-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "IgnoreSIGPIPE"))

(defun systemd-socket-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "NoNewPrivileges"))

(defun systemd-socket-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SystemCallFilter"))

(defun systemd-socket-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SystemCallArchitectures"))

(defun systemd-socket-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SystemCallErrorNumber"))

(defun systemd-socket-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "Personality"))

(defun systemd-socket-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "RestrictAddressFamilies"))

(defun systemd-socket-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "RuntimeDirectoryMode"))

(defun systemd-socket-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "RuntimeDirectory"))

(defun systemd-socket-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KillMode"))

(defun systemd-socket-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "KillSignal"))

(defun systemd-socket-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SendSIGKILL"))

(defun systemd-socket-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-socket "SendSIGHUP"))
;;; org.freedesktop.systemd1.Swap

(defconst systemd-dbus-interface-swap "org.freedesktop.systemd1.Swap")

(defun systemd-swap-What (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "What"))

(defun systemd-swap-Priority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Priority"))

(defun systemd-swap-Options (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Options"))

(defun systemd-swap-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TimeoutUSec"))

(defun systemd-swap-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ControlPID"))

(defun systemd-swap-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Result"))

(defun systemd-swap-ExecActivate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ExecActivate"))

(defun systemd-swap-ExecDeactivate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ExecDeactivate"))

(defun systemd-swap-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Slice"))

(defun systemd-swap-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ControlGroup"))

(defun systemd-swap-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "MemoryCurrent"))

(defun systemd-swap-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUUsageNSec"))

(defun systemd-swap-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TasksCurrent"))

(defun systemd-swap-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-swap "GetProcesses" args))

(defun systemd-swap-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Delegate"))

(defun systemd-swap-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUAccounting"))

(defun systemd-swap-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUShares"))

(defun systemd-swap-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StartupCPUShares"))

(defun systemd-swap-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUQuotaPerSecUSec"))

(defun systemd-swap-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOAccounting"))

(defun systemd-swap-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOWeight"))

(defun systemd-swap-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StartupIOWeight"))

(defun systemd-swap-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IODeviceWeight"))

(defun systemd-swap-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOReadBandwidthMax"))

(defun systemd-swap-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOWriteBandwidthMax"))

(defun systemd-swap-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOReadIOPSMax"))

(defun systemd-swap-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOWriteIOPSMax"))

(defun systemd-swap-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "BlockIOAccounting"))

(defun systemd-swap-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "BlockIOWeight"))

(defun systemd-swap-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StartupBlockIOWeight"))

(defun systemd-swap-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "BlockIODeviceWeight"))

(defun systemd-swap-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "BlockIOReadBandwidth"))

(defun systemd-swap-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "BlockIOWriteBandwidth"))

(defun systemd-swap-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "MemoryAccounting"))

(defun systemd-swap-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "MemoryLimit"))

(defun systemd-swap-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "DevicePolicy"))

(defun systemd-swap-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "DeviceAllow"))

(defun systemd-swap-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TasksAccounting"))

(defun systemd-swap-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TasksMax"))

(defun systemd-swap-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Environment"))

(defun systemd-swap-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "EnvironmentFiles"))

(defun systemd-swap-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "PassEnvironment"))

(defun systemd-swap-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "UMask"))

(defun systemd-swap-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitCPU"))

(defun systemd-swap-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitCPUSoft"))

(defun systemd-swap-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitFSIZE"))

(defun systemd-swap-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitFSIZESoft"))

(defun systemd-swap-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitDATA"))

(defun systemd-swap-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitDATASoft"))

(defun systemd-swap-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitSTACK"))

(defun systemd-swap-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitSTACKSoft"))

(defun systemd-swap-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitCORE"))

(defun systemd-swap-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitCORESoft"))

(defun systemd-swap-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRSS"))

(defun systemd-swap-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRSSSoft"))

(defun systemd-swap-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNOFILE"))

(defun systemd-swap-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNOFILESoft"))

(defun systemd-swap-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitAS"))

(defun systemd-swap-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitASSoft"))

(defun systemd-swap-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNPROC"))

(defun systemd-swap-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNPROCSoft"))

(defun systemd-swap-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitMEMLOCK"))

(defun systemd-swap-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitMEMLOCKSoft"))

(defun systemd-swap-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitLOCKS"))

(defun systemd-swap-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitLOCKSSoft"))

(defun systemd-swap-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitSIGPENDING"))

(defun systemd-swap-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitSIGPENDINGSoft"))

(defun systemd-swap-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitMSGQUEUE"))

(defun systemd-swap-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitMSGQUEUESoft"))

(defun systemd-swap-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNICE"))

(defun systemd-swap-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitNICESoft"))

(defun systemd-swap-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRTPRIO"))

(defun systemd-swap-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRTPRIOSoft"))

(defun systemd-swap-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRTTIME"))

(defun systemd-swap-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "LimitRTTIMESoft"))

(defun systemd-swap-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "WorkingDirectory"))

(defun systemd-swap-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "RootDirectory"))

(defun systemd-swap-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "OOMScoreAdjust"))

(defun systemd-swap-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Nice"))

(defun systemd-swap-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IOScheduling"))

(defun systemd-swap-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUSchedulingPolicy"))

(defun systemd-swap-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUSchedulingPriority"))

(defun systemd-swap-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUAffinity"))

(defun systemd-swap-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TimerSlackNSec"))

(defun systemd-swap-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CPUSchedulingResetOnFork"))

(defun systemd-swap-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "NonBlocking"))

(defun systemd-swap-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StandardInput"))

(defun systemd-swap-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StandardOutput"))

(defun systemd-swap-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "StandardError"))

(defun systemd-swap-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TTYPath"))

(defun systemd-swap-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TTYReset"))

(defun systemd-swap-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TTYVHangup"))

(defun systemd-swap-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "TTYVTDisallocate"))

(defun systemd-swap-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SyslogPriority"))

(defun systemd-swap-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SyslogIdentifier"))

(defun systemd-swap-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SyslogLevelPrefix"))

(defun systemd-swap-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SyslogLevel"))

(defun systemd-swap-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SyslogFacility"))

(defun systemd-swap-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SecureBits"))

(defun systemd-swap-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "CapabilityBoundingSet"))

(defun systemd-swap-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "AmbientCapabilities"))

(defun systemd-swap-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "User"))

(defun systemd-swap-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Group"))

(defun systemd-swap-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SupplementaryGroups"))

(defun systemd-swap-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "PAMName"))

(defun systemd-swap-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ReadWriteDirectories"))

(defun systemd-swap-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ReadOnlyDirectories"))

(defun systemd-swap-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "InaccessibleDirectories"))

(defun systemd-swap-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "MountFlags"))

(defun systemd-swap-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "PrivateTmp"))

(defun systemd-swap-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "PrivateNetwork"))

(defun systemd-swap-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "PrivateDevices"))

(defun systemd-swap-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ProtectHome"))

(defun systemd-swap-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "ProtectSystem"))

(defun systemd-swap-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SameProcessGroup"))

(defun systemd-swap-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "UtmpIdentifier"))

(defun systemd-swap-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "UtmpMode"))

(defun systemd-swap-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SELinuxContext"))

(defun systemd-swap-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "AppArmorProfile"))

(defun systemd-swap-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SmackProcessLabel"))

(defun systemd-swap-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "IgnoreSIGPIPE"))

(defun systemd-swap-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "NoNewPrivileges"))

(defun systemd-swap-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SystemCallFilter"))

(defun systemd-swap-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SystemCallArchitectures"))

(defun systemd-swap-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SystemCallErrorNumber"))

(defun systemd-swap-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "Personality"))

(defun systemd-swap-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "RestrictAddressFamilies"))

(defun systemd-swap-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "RuntimeDirectoryMode"))

(defun systemd-swap-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "RuntimeDirectory"))

(defun systemd-swap-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "KillMode"))

(defun systemd-swap-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "KillSignal"))

(defun systemd-swap-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SendSIGKILL"))

(defun systemd-swap-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-swap "SendSIGHUP"))
;;; org.freedesktop.systemd1.Target

(defconst systemd-dbus-interface-target "org.freedesktop.systemd1.Target")
;;; org.freedesktop.systemd1.Timer

(defconst systemd-dbus-interface-timer "org.freedesktop.systemd1.Timer")

(defun systemd-timer-Unit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "Unit"))

(defun systemd-timer-TimersMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "TimersMonotonic"))

(defun systemd-timer-TimersCalendar (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "TimersCalendar"))

(defun systemd-timer-NextElapseUSecRealtime (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "NextElapseUSecRealtime"))

(defun systemd-timer-NextElapseUSecMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "NextElapseUSecMonotonic"))

(defun systemd-timer-LastTriggerUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "LastTriggerUSec"))

(defun systemd-timer-LastTriggerUSecMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "LastTriggerUSecMonotonic"))

(defun systemd-timer-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "Result"))

(defun systemd-timer-AccuracyUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "AccuracyUSec"))

(defun systemd-timer-RandomizedDelayUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "RandomizedDelayUSec"))

(defun systemd-timer-Persistent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "Persistent"))

(defun systemd-timer-WakeSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "WakeSystem"))

(defun systemd-timer-RemainAfterElapse (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-timer "RemainAfterElapse"))
;;; org.freedesktop.systemd1.Unit

(defconst systemd-dbus-interface-unit "org.freedesktop.systemd1.Unit")

(defun systemd-unit-Id (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Id"))

(defun systemd-unit-Names (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Names"))

(defun systemd-unit-Following (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Following"))

(defun systemd-unit-Requires (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Requires"))

(defun systemd-unit-Requisite (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Requisite"))

(defun systemd-unit-Wants (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Wants"))

(defun systemd-unit-BindsTo (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "BindsTo"))

(defun systemd-unit-PartOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "PartOf"))

(defun systemd-unit-RequiredBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RequiredBy"))

(defun systemd-unit-RequisiteOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RequisiteOf"))

(defun systemd-unit-WantedBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "WantedBy"))

(defun systemd-unit-BoundBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "BoundBy"))

(defun systemd-unit-ConsistsOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ConsistsOf"))

(defun systemd-unit-Conflicts (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Conflicts"))

(defun systemd-unit-ConflictedBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ConflictedBy"))

(defun systemd-unit-Before (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Before"))

(defun systemd-unit-After (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "After"))

(defun systemd-unit-OnFailure (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "OnFailure"))

(defun systemd-unit-Triggers (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Triggers"))

(defun systemd-unit-TriggeredBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "TriggeredBy"))

(defun systemd-unit-PropagatesReloadTo (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "PropagatesReloadTo"))

(defun systemd-unit-ReloadPropagatedFrom (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ReloadPropagatedFrom"))

(defun systemd-unit-JoinsNamespaceOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "JoinsNamespaceOf"))

(defun systemd-unit-RequiresMountsFor (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RequiresMountsFor"))

(defun systemd-unit-Documentation (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Documentation"))

(defun systemd-unit-Description (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Description"))

(defun systemd-unit-LoadState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "LoadState"))

(defun systemd-unit-ActiveState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ActiveState"))

(defun systemd-unit-SubState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "SubState"))

(defun systemd-unit-FragmentPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "FragmentPath"))

(defun systemd-unit-SourcePath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "SourcePath"))

(defun systemd-unit-DropInPaths (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "DropInPaths"))

(defun systemd-unit-UnitFileState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "UnitFileState"))

(defun systemd-unit-UnitFilePreset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "UnitFilePreset"))

(defun systemd-unit-StateChangeTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StateChangeTimestamp"))

(defun systemd-unit-StateChangeTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StateChangeTimestampMonotonic"))

(defun systemd-unit-InactiveExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "InactiveExitTimestamp"))

(defun systemd-unit-InactiveExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "InactiveExitTimestampMonotonic"))

(defun systemd-unit-ActiveEnterTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ActiveEnterTimestamp"))

(defun systemd-unit-ActiveEnterTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ActiveEnterTimestampMonotonic"))

(defun systemd-unit-ActiveExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ActiveExitTimestamp"))

(defun systemd-unit-ActiveExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ActiveExitTimestampMonotonic"))

(defun systemd-unit-InactiveEnterTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "InactiveEnterTimestamp"))

(defun systemd-unit-InactiveEnterTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "InactiveEnterTimestampMonotonic"))

(defun systemd-unit-CanStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "CanStart"))

(defun systemd-unit-CanStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "CanStop"))

(defun systemd-unit-CanReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "CanReload"))

(defun systemd-unit-CanIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "CanIsolate"))

(defun systemd-unit-Job (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Job"))

(defun systemd-unit-StopWhenUnneeded (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StopWhenUnneeded"))

(defun systemd-unit-RefuseManualStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RefuseManualStart"))

(defun systemd-unit-RefuseManualStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RefuseManualStop"))

(defun systemd-unit-AllowIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "AllowIsolate"))

(defun systemd-unit-DefaultDependencies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "DefaultDependencies"))

(defun systemd-unit-OnFailureJobMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "OnFailureJobMode"))

(defun systemd-unit-IgnoreOnIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "IgnoreOnIsolate"))

(defun systemd-unit-NeedDaemonReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "NeedDaemonReload"))

(defun systemd-unit-JobTimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "JobTimeoutUSec"))

(defun systemd-unit-JobTimeoutAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "JobTimeoutAction"))

(defun systemd-unit-JobTimeoutRebootArgument (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "JobTimeoutRebootArgument"))

(defun systemd-unit-ConditionResult (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ConditionResult"))

(defun systemd-unit-AssertResult (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "AssertResult"))

(defun systemd-unit-ConditionTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ConditionTimestamp"))

(defun systemd-unit-ConditionTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "ConditionTimestampMonotonic"))

(defun systemd-unit-AssertTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "AssertTimestamp"))

(defun systemd-unit-AssertTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "AssertTimestampMonotonic"))

(defun systemd-unit-Conditions (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Conditions"))

(defun systemd-unit-Asserts (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Asserts"))

(defun systemd-unit-LoadError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "LoadError"))

(defun systemd-unit-Transient (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "Transient"))

(defun systemd-unit-StartLimitIntervalSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StartLimitIntervalSec"))

(defun systemd-unit-StartLimitBurst (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StartLimitBurst"))

(defun systemd-unit-StartLimitAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "StartLimitAction"))

(defun systemd-unit-RebootArgument (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     systemd-dbus-interface-unit "RebootArgument"))

(defun systemd-unit-Start (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "Start" args))

(defun systemd-unit-Stop (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "Stop" args))

(defun systemd-unit-Reload (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "Reload" args))

(defun systemd-unit-Restart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "Restart" args))

(defun systemd-unit-TryRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "TryRestart" args))

(defun systemd-unit-ReloadOrRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "ReloadOrRestart" args))

(defun systemd-unit-ReloadOrTryRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "ReloadOrTryRestart" args))

(defun systemd-unit-Kill (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "Kill" args))

(defun systemd-unit-ResetFailed (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "ResetFailed" args))

(defun systemd-unit-SetProperties (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 systemd-dbus-interface-unit "SetProperties" args))

(provide 'systemd)
;;; systemd.el ends here
