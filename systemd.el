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
(defconst systemd-dbus-interface-manager "org.freedesktop.systemd.Manager")

;;; org.freedesktop.systemd1.Manager

(defun systemd-Version (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Version"))

(defun systemd-Features (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Features"))

(defun systemd-Virtualization (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Virtualization"))

(defun systemd-Architecture (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Architecture"))

(defun systemd-Tainted (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Tainted"))

(defun systemd-FirmwareTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "FirmwareTimestamp"))

(defun systemd-FirmwareTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "FirmwareTimestampMonotonic"))

(defun systemd-LoaderTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "LoaderTimestamp"))

(defun systemd-LoaderTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "LoaderTimestampMonotonic"))

(defun systemd-KernelTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "KernelTimestamp"))

(defun systemd-KernelTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "KernelTimestampMonotonic"))

(defun systemd-InitRDTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "InitRDTimestamp"))

(defun systemd-InitRDTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "InitRDTimestampMonotonic"))

(defun systemd-UserspaceTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UserspaceTimestamp"))

(defun systemd-UserspaceTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UserspaceTimestampMonotonic"))

(defun systemd-FinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "FinishTimestamp"))

(defun systemd-FinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "FinishTimestampMonotonic"))

(defun systemd-SecurityStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "SecurityStartTimestamp"))

(defun systemd-SecurityStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "SecurityStartTimestampMonotonic"))

(defun systemd-SecurityFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "SecurityFinishTimestamp"))

(defun systemd-SecurityFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "SecurityFinishTimestampMonotonic"))

(defun systemd-GeneratorsStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "GeneratorsStartTimestamp"))

(defun systemd-GeneratorsStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "GeneratorsStartTimestampMonotonic"))

(defun systemd-GeneratorsFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "GeneratorsFinishTimestamp"))

(defun systemd-GeneratorsFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "GeneratorsFinishTimestampMonotonic"))

(defun systemd-UnitsLoadStartTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UnitsLoadStartTimestamp"))

(defun systemd-UnitsLoadStartTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UnitsLoadStartTimestampMonotonic"))

(defun systemd-UnitsLoadFinishTimestamp (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UnitsLoadFinishTimestamp"))

(defun systemd-UnitsLoadFinishTimestampMonotonic (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UnitsLoadFinishTimestampMonotonic"))

(defun systemd-LogLevel (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "LogLevel"))

(gv-define-setter systemd-LogLevel (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      "org.freedesktop.systemd1.Manager" "LogLevel" ,value))

(defun systemd-LogTarget (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "LogTarget"))

(gv-define-setter systemd-LogTarget (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      "org.freedesktop.systemd1.Manager" "LogTarget" ,value))

(defun systemd-NNames (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "NNames"))

(defun systemd-NFailedUnits (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "NFailedUnits"))

(defun systemd-NJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "NJobs"))

(defun systemd-NInstalledJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "NInstalledJobs"))

(defun systemd-NFailedJobs (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "NFailedJobs"))

(defun systemd-Progress (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Progress"))

(defun systemd-Environment (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "Environment"))

(defun systemd-ConfirmSpawn (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "ConfirmSpawn"))

(defun systemd-ShowStatus (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "ShowStatus"))

(defun systemd-UnitPath (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "UnitPath"))

(defun systemd-DefaultStandardOutput (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultStandardOutput"))

(defun systemd-DefaultStandardError (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultStandardError"))

(defun systemd-RuntimeWatchdogUSec (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "RuntimeWatchdogUSec"))

(gv-define-setter systemd-RuntimeWatchdogUSec (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      "org.freedesktop.systemd1.Manager" "RuntimeWatchdogUSec" ,value))

(defun systemd-ShutdownWatchdogUSec (bus)
  "Use `setf' to set the value of this property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "ShutdownWatchdogUSec"))

(gv-define-setter systemd-ShutdownWatchdogUSec (value bus)
  `(dbus-set-property ,bus systemd-dbus-service systemd-dbus-path
		      "org.freedesktop.systemd1.Manager" "ShutdownWatchdogUSec" ,value))

(defun systemd-ControlGroup (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "ControlGroup"))

(defun systemd-SystemState (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "SystemState"))

(defun systemd-ExitCode (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "ExitCode"))

(defun systemd-DefaultTimerAccuracyUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultTimerAccuracyUSec"))

(defun systemd-DefaultTimeoutStartUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultTimeoutStartUSec"))

(defun systemd-DefaultTimeoutStopUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultTimeoutStopUSec"))

(defun systemd-DefaultRestartUSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultRestartUSec"))

(defun systemd-DefaultStartLimitIntervalSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultStartLimitIntervalSec"))

(defun systemd-DefaultStartLimitBurst (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultStartLimitBurst"))

(defun systemd-DefaultCPUAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultCPUAccounting"))

(defun systemd-DefaultBlockIOAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultBlockIOAccounting"))

(defun systemd-DefaultMemoryAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultMemoryAccounting"))

(defun systemd-DefaultTasksAccounting (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultTasksAccounting"))

(defun systemd-DefaultLimitCPU (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitCPU"))

(defun systemd-DefaultLimitCPUSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitCPUSoft"))

(defun systemd-DefaultLimitFSIZE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitFSIZE"))

(defun systemd-DefaultLimitFSIZESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitFSIZESoft"))

(defun systemd-DefaultLimitDATA (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitDATA"))

(defun systemd-DefaultLimitDATASoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitDATASoft"))

(defun systemd-DefaultLimitSTACK (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitSTACK"))

(defun systemd-DefaultLimitSTACKSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitSTACKSoft"))

(defun systemd-DefaultLimitCORE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitCORE"))

(defun systemd-DefaultLimitCORESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitCORESoft"))

(defun systemd-DefaultLimitRSS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRSS"))

(defun systemd-DefaultLimitRSSSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRSSSoft"))

(defun systemd-DefaultLimitNOFILE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNOFILE"))

(defun systemd-DefaultLimitNOFILESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNOFILESoft"))

(defun systemd-DefaultLimitAS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitAS"))

(defun systemd-DefaultLimitASSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitASSoft"))

(defun systemd-DefaultLimitNPROC (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNPROC"))

(defun systemd-DefaultLimitNPROCSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNPROCSoft"))

(defun systemd-DefaultLimitMEMLOCK (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitMEMLOCK"))

(defun systemd-DefaultLimitMEMLOCKSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitMEMLOCKSoft"))

(defun systemd-DefaultLimitLOCKS (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitLOCKS"))

(defun systemd-DefaultLimitLOCKSSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitLOCKSSoft"))

(defun systemd-DefaultLimitSIGPENDING (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitSIGPENDING"))

(defun systemd-DefaultLimitSIGPENDINGSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitSIGPENDINGSoft"))

(defun systemd-DefaultLimitMSGQUEUE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitMSGQUEUE"))

(defun systemd-DefaultLimitMSGQUEUESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitMSGQUEUESoft"))

(defun systemd-DefaultLimitNICE (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNICE"))

(defun systemd-DefaultLimitNICESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitNICESoft"))

(defun systemd-DefaultLimitRTPRIO (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRTPRIO"))

(defun systemd-DefaultLimitRTPRIOSoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRTPRIOSoft"))

(defun systemd-DefaultLimitRTTIME (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRTTIME"))

(defun systemd-DefaultLimitRTTIMESoft (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultLimitRTTIMESoft"))

(defun systemd-DefaultTasksMax (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "DefaultTasksMax"))

(defun systemd-TimerSlackNSec (bus)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service systemd-dbus-path
		     "org.freedesktop.systemd1.Manager" "TimerSlackNSec"))

(defun systemd-GetUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetUnit" args))

(defun systemd-GetUnitByPID (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetUnitByPID" args))

(defun systemd-LoadUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "LoadUnit" args))

(defun systemd-StartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "StartUnit" args))

(defun systemd-StartUnitReplace (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "StartUnitReplace" args))

(defun systemd-StopUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "StopUnit" args))

(defun systemd-ReloadUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ReloadUnit" args))

(defun systemd-RestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "RestartUnit" args))

(defun systemd-TryRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "TryRestartUnit" args))

(defun systemd-ReloadOrRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ReloadOrRestartUnit" args))

(defun systemd-ReloadOrTryRestartUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ReloadOrTryRestartUnit" args))

(defun systemd-KillUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "KillUnit" args))

(defun systemd-ResetFailedUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ResetFailedUnit" args))

(defun systemd-SetUnitProperties (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "SetUnitProperties" args))

(defun systemd-StartTransientUnit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "StartTransientUnit" args))

(defun systemd-GetUnitProcesses (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetUnitProcesses" args))

(defun systemd-GetJob (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetJob" args))

(defun systemd-CancelJob (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "CancelJob" args))

(defun systemd-ClearJobs (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ClearJobs" args))

(defun systemd-ResetFailed (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ResetFailed" args))

(defun systemd-ListUnits (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnits" args))

(defun systemd-ListUnitsFiltered (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnitsFiltered" args))

(defun systemd-ListUnitsByPatterns (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnitsByPatterns" args))

(defun systemd-ListUnitsByNames (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnitsByNames" args))

(defun systemd-ListJobs (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListJobs" args))

(defun systemd-Subscribe (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Subscribe" args))

(defun systemd-Unsubscribe (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Unsubscribe" args))

(defun systemd-Dump (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Dump" args))

(defun systemd-CreateSnapshot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "CreateSnapshot" args))

(defun systemd-RemoveSnapshot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "RemoveSnapshot" args))

(defun systemd-Reload (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Reload" args))

(defun systemd-Reexecute (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Reexecute" args))

(defun systemd-Exit (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Exit" args))

(defun systemd-Reboot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Reboot" args))

(defun systemd-PowerOff (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "PowerOff" args))

(defun systemd-Halt (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "Halt" args))

(defun systemd-KExec (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "KExec" args))

(defun systemd-SwitchRoot (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "SwitchRoot" args))

(defun systemd-SetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "SetEnvironment" args))

(defun systemd-UnsetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "UnsetEnvironment" args))

(defun systemd-UnsetAndSetEnvironment (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "UnsetAndSetEnvironment" args))

(defun systemd-ListUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnitFiles" args))

(defun systemd-ListUnitFilesByPatterns (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ListUnitFilesByPatterns" args))

(defun systemd-GetUnitFileState (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetUnitFileState" args))

(defun systemd-EnableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "EnableUnitFiles" args))

(defun systemd-DisableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "DisableUnitFiles" args))

(defun systemd-ReenableUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "ReenableUnitFiles" args))

(defun systemd-LinkUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "LinkUnitFiles" args))

(defun systemd-PresetUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "PresetUnitFiles" args))

(defun systemd-PresetUnitFilesWithMode (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "PresetUnitFilesWithMode" args))

(defun systemd-MaskUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "MaskUnitFiles" args))

(defun systemd-UnmaskUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "UnmaskUnitFiles" args))

(defun systemd-RevertUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "RevertUnitFiles" args))

(defun systemd-SetDefaultTarget (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "SetDefaultTarget" args))

(defun systemd-GetDefaultTarget (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "GetDefaultTarget" args))

(defun systemd-PresetAllUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "PresetAllUnitFiles" args))

(defun systemd-AddDependencyUnitFiles (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "AddDependencyUnitFiles" args))

(defun systemd-SetExitCode (bus &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service systemd-dbus-path
	 "org.freedesktop.systemd1.Manager" "SetExitCode" args))
;;; org.freedesktop.systemd1.Service

(defun systemd-service-Type (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Type"))

(defun systemd-service-Restart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Restart"))

(defun systemd-service-PIDFile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PIDFile"))

(defun systemd-service-NotifyAccess (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "NotifyAccess"))

(defun systemd-service-RestartUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RestartUSec"))

(defun systemd-service-TimeoutStartUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TimeoutStartUSec"))

(defun systemd-service-TimeoutStopUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TimeoutStopUSec"))

(defun systemd-service-RuntimeMaxUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RuntimeMaxUSec"))

(defun systemd-service-WatchdogUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "WatchdogUSec"))

(defun systemd-service-WatchdogTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "WatchdogTimestamp"))

(defun systemd-service-WatchdogTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "WatchdogTimestampMonotonic"))

(defun systemd-service-FailureAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "FailureAction"))

(defun systemd-service-PermissionsStartOnly (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PermissionsStartOnly"))

(defun systemd-service-RootDirectoryStartOnly (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RootDirectoryStartOnly"))

(defun systemd-service-RemainAfterExit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RemainAfterExit"))

(defun systemd-service-GuessMainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "GuessMainPID"))

(defun systemd-service-MainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "MainPID"))

(defun systemd-service-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ControlPID"))

(defun systemd-service-BusName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BusName"))

(defun systemd-service-FileDescriptorStoreMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "FileDescriptorStoreMax"))

(defun systemd-service-NFileDescriptorStore (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "NFileDescriptorStore"))

(defun systemd-service-StatusText (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StatusText"))

(defun systemd-service-StatusErrno (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StatusErrno"))

(defun systemd-service-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Result"))

(defun systemd-service-USBFunctionDescriptors (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "USBFunctionDescriptors"))

(defun systemd-service-USBFunctionStrings (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "USBFunctionStrings"))

(defun systemd-service-ExecMainStartTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainStartTimestamp"))

(defun systemd-service-ExecMainStartTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainStartTimestampMonotonic"))

(defun systemd-service-ExecMainExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainExitTimestamp"))

(defun systemd-service-ExecMainExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainExitTimestampMonotonic"))

(defun systemd-service-ExecMainPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainPID"))

(defun systemd-service-ExecMainCode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainCode"))

(defun systemd-service-ExecMainStatus (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecMainStatus"))

(defun systemd-service-ExecStartPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecStartPre"))

(defun systemd-service-ExecStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecStart"))

(defun systemd-service-ExecStartPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecStartPost"))

(defun systemd-service-ExecReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecReload"))

(defun systemd-service-ExecStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecStop"))

(defun systemd-service-ExecStopPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ExecStopPost"))

(defun systemd-service-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Slice"))

(defun systemd-service-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ControlGroup"))

(defun systemd-service-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "MemoryCurrent"))

(defun systemd-service-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUUsageNSec"))

(defun systemd-service-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TasksCurrent"))

(defun systemd-service-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Service" "GetProcesses" args))

(defun systemd-service-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Delegate"))

(defun systemd-service-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUAccounting"))

(defun systemd-service-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUShares"))

(defun systemd-service-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StartupCPUShares"))

(defun systemd-service-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUQuotaPerSecUSec"))

(defun systemd-service-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOAccounting"))

(defun systemd-service-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOWeight"))

(defun systemd-service-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StartupIOWeight"))

(defun systemd-service-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IODeviceWeight"))

(defun systemd-service-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOReadBandwidthMax"))

(defun systemd-service-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOWriteBandwidthMax"))

(defun systemd-service-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOReadIOPSMax"))

(defun systemd-service-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOWriteIOPSMax"))

(defun systemd-service-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BlockIOAccounting"))

(defun systemd-service-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BlockIOWeight"))

(defun systemd-service-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StartupBlockIOWeight"))

(defun systemd-service-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BlockIODeviceWeight"))

(defun systemd-service-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BlockIOReadBandwidth"))

(defun systemd-service-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "BlockIOWriteBandwidth"))

(defun systemd-service-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "MemoryAccounting"))

(defun systemd-service-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "MemoryLimit"))

(defun systemd-service-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "DevicePolicy"))

(defun systemd-service-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "DeviceAllow"))

(defun systemd-service-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TasksAccounting"))

(defun systemd-service-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TasksMax"))

(defun systemd-service-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Environment"))

(defun systemd-service-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "EnvironmentFiles"))

(defun systemd-service-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PassEnvironment"))

(defun systemd-service-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "UMask"))

(defun systemd-service-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitCPU"))

(defun systemd-service-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitCPUSoft"))

(defun systemd-service-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitFSIZE"))

(defun systemd-service-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitFSIZESoft"))

(defun systemd-service-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitDATA"))

(defun systemd-service-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitDATASoft"))

(defun systemd-service-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitSTACK"))

(defun systemd-service-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitSTACKSoft"))

(defun systemd-service-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitCORE"))

(defun systemd-service-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitCORESoft"))

(defun systemd-service-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRSS"))

(defun systemd-service-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRSSSoft"))

(defun systemd-service-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNOFILE"))

(defun systemd-service-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNOFILESoft"))

(defun systemd-service-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitAS"))

(defun systemd-service-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitASSoft"))

(defun systemd-service-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNPROC"))

(defun systemd-service-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNPROCSoft"))

(defun systemd-service-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitMEMLOCK"))

(defun systemd-service-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitMEMLOCKSoft"))

(defun systemd-service-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitLOCKS"))

(defun systemd-service-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitLOCKSSoft"))

(defun systemd-service-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitSIGPENDING"))

(defun systemd-service-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitSIGPENDINGSoft"))

(defun systemd-service-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitMSGQUEUE"))

(defun systemd-service-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitMSGQUEUESoft"))

(defun systemd-service-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNICE"))

(defun systemd-service-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitNICESoft"))

(defun systemd-service-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRTPRIO"))

(defun systemd-service-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRTPRIOSoft"))

(defun systemd-service-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRTTIME"))

(defun systemd-service-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "LimitRTTIMESoft"))

(defun systemd-service-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "WorkingDirectory"))

(defun systemd-service-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RootDirectory"))

(defun systemd-service-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "OOMScoreAdjust"))

(defun systemd-service-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Nice"))

(defun systemd-service-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IOScheduling"))

(defun systemd-service-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUSchedulingPolicy"))

(defun systemd-service-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUSchedulingPriority"))

(defun systemd-service-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUAffinity"))

(defun systemd-service-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TimerSlackNSec"))

(defun systemd-service-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CPUSchedulingResetOnFork"))

(defun systemd-service-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "NonBlocking"))

(defun systemd-service-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StandardInput"))

(defun systemd-service-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StandardOutput"))

(defun systemd-service-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "StandardError"))

(defun systemd-service-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TTYPath"))

(defun systemd-service-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TTYReset"))

(defun systemd-service-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TTYVHangup"))

(defun systemd-service-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "TTYVTDisallocate"))

(defun systemd-service-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SyslogPriority"))

(defun systemd-service-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SyslogIdentifier"))

(defun systemd-service-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SyslogLevelPrefix"))

(defun systemd-service-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SyslogLevel"))

(defun systemd-service-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SyslogFacility"))

(defun systemd-service-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SecureBits"))

(defun systemd-service-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "CapabilityBoundingSet"))

(defun systemd-service-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "AmbientCapabilities"))

(defun systemd-service-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "User"))

(defun systemd-service-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Group"))

(defun systemd-service-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SupplementaryGroups"))

(defun systemd-service-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PAMName"))

(defun systemd-service-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ReadWriteDirectories"))

(defun systemd-service-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ReadOnlyDirectories"))

(defun systemd-service-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "InaccessibleDirectories"))

(defun systemd-service-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "MountFlags"))

(defun systemd-service-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PrivateTmp"))

(defun systemd-service-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PrivateNetwork"))

(defun systemd-service-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "PrivateDevices"))

(defun systemd-service-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ProtectHome"))

(defun systemd-service-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "ProtectSystem"))

(defun systemd-service-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SameProcessGroup"))

(defun systemd-service-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "UtmpIdentifier"))

(defun systemd-service-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "UtmpMode"))

(defun systemd-service-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SELinuxContext"))

(defun systemd-service-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "AppArmorProfile"))

(defun systemd-service-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SmackProcessLabel"))

(defun systemd-service-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "IgnoreSIGPIPE"))

(defun systemd-service-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "NoNewPrivileges"))

(defun systemd-service-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SystemCallFilter"))

(defun systemd-service-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SystemCallArchitectures"))

(defun systemd-service-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SystemCallErrorNumber"))

(defun systemd-service-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "Personality"))

(defun systemd-service-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RestrictAddressFamilies"))

(defun systemd-service-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RuntimeDirectoryMode"))

(defun systemd-service-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "RuntimeDirectory"))

(defun systemd-service-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "KillMode"))

(defun systemd-service-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "KillSignal"))

(defun systemd-service-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SendSIGKILL"))

(defun systemd-service-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Service" "SendSIGHUP"))
;;; org.freedesktop.systemd1.Unit

(defun systemd-unit-Id (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Id"))

(defun systemd-unit-Names (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Names"))

(defun systemd-unit-Following (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Following"))

(defun systemd-unit-Requires (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Requires"))

(defun systemd-unit-Requisite (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Requisite"))

(defun systemd-unit-Wants (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Wants"))

(defun systemd-unit-BindsTo (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "BindsTo"))

(defun systemd-unit-PartOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "PartOf"))

(defun systemd-unit-RequiredBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RequiredBy"))

(defun systemd-unit-RequisiteOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RequisiteOf"))

(defun systemd-unit-WantedBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "WantedBy"))

(defun systemd-unit-BoundBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "BoundBy"))

(defun systemd-unit-ConsistsOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ConsistsOf"))

(defun systemd-unit-Conflicts (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Conflicts"))

(defun systemd-unit-ConflictedBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ConflictedBy"))

(defun systemd-unit-Before (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Before"))

(defun systemd-unit-After (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "After"))

(defun systemd-unit-OnFailure (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "OnFailure"))

(defun systemd-unit-Triggers (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Triggers"))

(defun systemd-unit-TriggeredBy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "TriggeredBy"))

(defun systemd-unit-PropagatesReloadTo (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "PropagatesReloadTo"))

(defun systemd-unit-ReloadPropagatedFrom (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ReloadPropagatedFrom"))

(defun systemd-unit-JoinsNamespaceOf (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "JoinsNamespaceOf"))

(defun systemd-unit-RequiresMountsFor (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RequiresMountsFor"))

(defun systemd-unit-Documentation (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Documentation"))

(defun systemd-unit-Description (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Description"))

(defun systemd-unit-LoadState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "LoadState"))

(defun systemd-unit-ActiveState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ActiveState"))

(defun systemd-unit-SubState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "SubState"))

(defun systemd-unit-FragmentPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "FragmentPath"))

(defun systemd-unit-SourcePath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "SourcePath"))

(defun systemd-unit-DropInPaths (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "DropInPaths"))

(defun systemd-unit-UnitFileState (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "UnitFileState"))

(defun systemd-unit-UnitFilePreset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "UnitFilePreset"))

(defun systemd-unit-StateChangeTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StateChangeTimestamp"))

(defun systemd-unit-StateChangeTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StateChangeTimestampMonotonic"))

(defun systemd-unit-InactiveExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "InactiveExitTimestamp"))

(defun systemd-unit-InactiveExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "InactiveExitTimestampMonotonic"))

(defun systemd-unit-ActiveEnterTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ActiveEnterTimestamp"))

(defun systemd-unit-ActiveEnterTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ActiveEnterTimestampMonotonic"))

(defun systemd-unit-ActiveExitTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ActiveExitTimestamp"))

(defun systemd-unit-ActiveExitTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ActiveExitTimestampMonotonic"))

(defun systemd-unit-InactiveEnterTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "InactiveEnterTimestamp"))

(defun systemd-unit-InactiveEnterTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "InactiveEnterTimestampMonotonic"))

(defun systemd-unit-CanStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "CanStart"))

(defun systemd-unit-CanStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "CanStop"))

(defun systemd-unit-CanReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "CanReload"))

(defun systemd-unit-CanIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "CanIsolate"))

(defun systemd-unit-Job (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Job"))

(defun systemd-unit-StopWhenUnneeded (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StopWhenUnneeded"))

(defun systemd-unit-RefuseManualStart (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RefuseManualStart"))

(defun systemd-unit-RefuseManualStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RefuseManualStop"))

(defun systemd-unit-AllowIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "AllowIsolate"))

(defun systemd-unit-DefaultDependencies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "DefaultDependencies"))

(defun systemd-unit-OnFailureJobMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "OnFailureJobMode"))

(defun systemd-unit-IgnoreOnIsolate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "IgnoreOnIsolate"))

(defun systemd-unit-NeedDaemonReload (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "NeedDaemonReload"))

(defun systemd-unit-JobTimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "JobTimeoutUSec"))

(defun systemd-unit-JobTimeoutAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "JobTimeoutAction"))

(defun systemd-unit-JobTimeoutRebootArgument (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "JobTimeoutRebootArgument"))

(defun systemd-unit-ConditionResult (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ConditionResult"))

(defun systemd-unit-AssertResult (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "AssertResult"))

(defun systemd-unit-ConditionTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ConditionTimestamp"))

(defun systemd-unit-ConditionTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "ConditionTimestampMonotonic"))

(defun systemd-unit-AssertTimestamp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "AssertTimestamp"))

(defun systemd-unit-AssertTimestampMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "AssertTimestampMonotonic"))

(defun systemd-unit-Conditions (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Conditions"))

(defun systemd-unit-Asserts (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Asserts"))

(defun systemd-unit-LoadError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "LoadError"))

(defun systemd-unit-Transient (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "Transient"))

(defun systemd-unit-StartLimitIntervalSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StartLimitIntervalSec"))

(defun systemd-unit-StartLimitBurst (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StartLimitBurst"))

(defun systemd-unit-StartLimitAction (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "StartLimitAction"))

(defun systemd-unit-RebootArgument (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Unit" "RebootArgument"))

(defun systemd-unit-Start (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "Start" args))

(defun systemd-unit-Stop (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "Stop" args))

(defun systemd-unit-Reload (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "Reload" args))

(defun systemd-unit-Restart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "Restart" args))

(defun systemd-unit-TryRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "TryRestart" args))

(defun systemd-unit-ReloadOrRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "ReloadOrRestart" args))

(defun systemd-unit-ReloadOrTryRestart (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "ReloadOrTryRestart" args))

(defun systemd-unit-Kill (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "Kill" args))

(defun systemd-unit-ResetFailed (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "ResetFailed" args))

(defun systemd-unit-SetProperties (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Unit" "SetProperties" args))
;;; org.freedesktop.systemd1.Slice

(defun systemd-slice-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "Slice"))

(defun systemd-slice-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "ControlGroup"))

(defun systemd-slice-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "MemoryCurrent"))

(defun systemd-slice-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "CPUUsageNSec"))

(defun systemd-slice-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "TasksCurrent"))

(defun systemd-slice-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Slice" "GetProcesses" args))

(defun systemd-slice-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "Delegate"))

(defun systemd-slice-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "CPUAccounting"))

(defun systemd-slice-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "CPUShares"))

(defun systemd-slice-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "StartupCPUShares"))

(defun systemd-slice-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "CPUQuotaPerSecUSec"))

(defun systemd-slice-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOAccounting"))

(defun systemd-slice-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOWeight"))

(defun systemd-slice-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "StartupIOWeight"))

(defun systemd-slice-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IODeviceWeight"))

(defun systemd-slice-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOReadBandwidthMax"))

(defun systemd-slice-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOWriteBandwidthMax"))

(defun systemd-slice-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOReadIOPSMax"))

(defun systemd-slice-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "IOWriteIOPSMax"))

(defun systemd-slice-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "BlockIOAccounting"))

(defun systemd-slice-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "BlockIOWeight"))

(defun systemd-slice-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "StartupBlockIOWeight"))

(defun systemd-slice-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "BlockIODeviceWeight"))

(defun systemd-slice-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "BlockIOReadBandwidth"))

(defun systemd-slice-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "BlockIOWriteBandwidth"))

(defun systemd-slice-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "MemoryAccounting"))

(defun systemd-slice-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "MemoryLimit"))

(defun systemd-slice-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "DevicePolicy"))

(defun systemd-slice-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "DeviceAllow"))

(defun systemd-slice-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "TasksAccounting"))

(defun systemd-slice-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Slice" "TasksMax"))
;;; org.freedesktop.systemd1.Socket

(defun systemd-socket-BindIPv6Only (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BindIPv6Only"))

(defun systemd-socket-Backlog (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Backlog"))

(defun systemd-socket-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TimeoutUSec"))

(defun systemd-socket-BindToDevice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BindToDevice"))

(defun systemd-socket-SocketUser (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SocketUser"))

(defun systemd-socket-SocketGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SocketGroup"))

(defun systemd-socket-SocketMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SocketMode"))

(defun systemd-socket-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "DirectoryMode"))

(defun systemd-socket-Accept (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Accept"))

(defun systemd-socket-Writable (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Writable"))

(defun systemd-socket-KeepAlive (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KeepAlive"))

(defun systemd-socket-KeepAliveTimeUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KeepAliveTimeUSec"))

(defun systemd-socket-KeepAliveIntervalUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KeepAliveIntervalUSec"))

(defun systemd-socket-KeepAliveProbes (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KeepAliveProbes"))

(defun systemd-socket-DeferAcceptUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "DeferAcceptUSec"))

(defun systemd-socket-NoDelay (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "NoDelay"))

(defun systemd-socket-Priority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Priority"))

(defun systemd-socket-ReceiveBuffer (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ReceiveBuffer"))

(defun systemd-socket-SendBuffer (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SendBuffer"))

(defun systemd-socket-IPTOS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IPTOS"))

(defun systemd-socket-IPTTL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IPTTL"))

(defun systemd-socket-PipeSize (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PipeSize"))

(defun systemd-socket-FreeBind (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "FreeBind"))

(defun systemd-socket-Transparent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Transparent"))

(defun systemd-socket-Broadcast (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Broadcast"))

(defun systemd-socket-PassCredentials (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PassCredentials"))

(defun systemd-socket-PassSecurity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PassSecurity"))

(defun systemd-socket-RemoveOnStop (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "RemoveOnStop"))

(defun systemd-socket-Listen (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Listen"))

(defun systemd-socket-Symlinks (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Symlinks"))

(defun systemd-socket-Mark (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Mark"))

(defun systemd-socket-MaxConnections (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MaxConnections"))

(defun systemd-socket-MessageQueueMaxMessages (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MessageQueueMaxMessages"))

(defun systemd-socket-MessageQueueMessageSize (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MessageQueueMessageSize"))

(defun systemd-socket-ReusePort (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ReusePort"))

(defun systemd-socket-SmackLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SmackLabel"))

(defun systemd-socket-SmackLabelIPIn (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SmackLabelIPIn"))

(defun systemd-socket-SmackLabelIPOut (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SmackLabelIPOut"))

(defun systemd-socket-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ControlPID"))

(defun systemd-socket-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Result"))

(defun systemd-socket-NConnections (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "NConnections"))

(defun systemd-socket-NAccepted (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "NAccepted"))

(defun systemd-socket-FileDescriptorName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "FileDescriptorName"))

(defun systemd-socket-SocketProtocol (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SocketProtocol"))

(defun systemd-socket-TriggerLimitIntervalUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TriggerLimitIntervalUSec"))

(defun systemd-socket-TriggerLimitBurst (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TriggerLimitBurst"))

(defun systemd-socket-ExecStartPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ExecStartPre"))

(defun systemd-socket-ExecStartPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ExecStartPost"))

(defun systemd-socket-ExecStopPre (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ExecStopPre"))

(defun systemd-socket-ExecStopPost (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ExecStopPost"))

(defun systemd-socket-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Slice"))

(defun systemd-socket-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ControlGroup"))

(defun systemd-socket-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MemoryCurrent"))

(defun systemd-socket-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUUsageNSec"))

(defun systemd-socket-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TasksCurrent"))

(defun systemd-socket-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Socket" "GetProcesses" args))

(defun systemd-socket-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Delegate"))

(defun systemd-socket-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUAccounting"))

(defun systemd-socket-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUShares"))

(defun systemd-socket-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StartupCPUShares"))

(defun systemd-socket-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUQuotaPerSecUSec"))

(defun systemd-socket-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOAccounting"))

(defun systemd-socket-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOWeight"))

(defun systemd-socket-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StartupIOWeight"))

(defun systemd-socket-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IODeviceWeight"))

(defun systemd-socket-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOReadBandwidthMax"))

(defun systemd-socket-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOWriteBandwidthMax"))

(defun systemd-socket-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOReadIOPSMax"))

(defun systemd-socket-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOWriteIOPSMax"))

(defun systemd-socket-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BlockIOAccounting"))

(defun systemd-socket-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BlockIOWeight"))

(defun systemd-socket-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StartupBlockIOWeight"))

(defun systemd-socket-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BlockIODeviceWeight"))

(defun systemd-socket-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BlockIOReadBandwidth"))

(defun systemd-socket-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "BlockIOWriteBandwidth"))

(defun systemd-socket-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MemoryAccounting"))

(defun systemd-socket-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MemoryLimit"))

(defun systemd-socket-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "DevicePolicy"))

(defun systemd-socket-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "DeviceAllow"))

(defun systemd-socket-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TasksAccounting"))

(defun systemd-socket-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TasksMax"))

(defun systemd-socket-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Environment"))

(defun systemd-socket-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "EnvironmentFiles"))

(defun systemd-socket-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PassEnvironment"))

(defun systemd-socket-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "UMask"))

(defun systemd-socket-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitCPU"))

(defun systemd-socket-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitCPUSoft"))

(defun systemd-socket-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitFSIZE"))

(defun systemd-socket-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitFSIZESoft"))

(defun systemd-socket-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitDATA"))

(defun systemd-socket-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitDATASoft"))

(defun systemd-socket-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitSTACK"))

(defun systemd-socket-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitSTACKSoft"))

(defun systemd-socket-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitCORE"))

(defun systemd-socket-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitCORESoft"))

(defun systemd-socket-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRSS"))

(defun systemd-socket-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRSSSoft"))

(defun systemd-socket-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNOFILE"))

(defun systemd-socket-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNOFILESoft"))

(defun systemd-socket-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitAS"))

(defun systemd-socket-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitASSoft"))

(defun systemd-socket-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNPROC"))

(defun systemd-socket-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNPROCSoft"))

(defun systemd-socket-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitMEMLOCK"))

(defun systemd-socket-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitMEMLOCKSoft"))

(defun systemd-socket-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitLOCKS"))

(defun systemd-socket-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitLOCKSSoft"))

(defun systemd-socket-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitSIGPENDING"))

(defun systemd-socket-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitSIGPENDINGSoft"))

(defun systemd-socket-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitMSGQUEUE"))

(defun systemd-socket-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitMSGQUEUESoft"))

(defun systemd-socket-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNICE"))

(defun systemd-socket-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitNICESoft"))

(defun systemd-socket-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRTPRIO"))

(defun systemd-socket-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRTPRIOSoft"))

(defun systemd-socket-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRTTIME"))

(defun systemd-socket-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "LimitRTTIMESoft"))

(defun systemd-socket-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "WorkingDirectory"))

(defun systemd-socket-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "RootDirectory"))

(defun systemd-socket-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "OOMScoreAdjust"))

(defun systemd-socket-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Nice"))

(defun systemd-socket-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IOScheduling"))

(defun systemd-socket-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUSchedulingPolicy"))

(defun systemd-socket-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUSchedulingPriority"))

(defun systemd-socket-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUAffinity"))

(defun systemd-socket-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TimerSlackNSec"))

(defun systemd-socket-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CPUSchedulingResetOnFork"))

(defun systemd-socket-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "NonBlocking"))

(defun systemd-socket-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StandardInput"))

(defun systemd-socket-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StandardOutput"))

(defun systemd-socket-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "StandardError"))

(defun systemd-socket-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TTYPath"))

(defun systemd-socket-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TTYReset"))

(defun systemd-socket-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TTYVHangup"))

(defun systemd-socket-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "TTYVTDisallocate"))

(defun systemd-socket-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SyslogPriority"))

(defun systemd-socket-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SyslogIdentifier"))

(defun systemd-socket-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SyslogLevelPrefix"))

(defun systemd-socket-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SyslogLevel"))

(defun systemd-socket-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SyslogFacility"))

(defun systemd-socket-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SecureBits"))

(defun systemd-socket-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "CapabilityBoundingSet"))

(defun systemd-socket-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "AmbientCapabilities"))

(defun systemd-socket-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "User"))

(defun systemd-socket-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Group"))

(defun systemd-socket-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SupplementaryGroups"))

(defun systemd-socket-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PAMName"))

(defun systemd-socket-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ReadWriteDirectories"))

(defun systemd-socket-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ReadOnlyDirectories"))

(defun systemd-socket-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "InaccessibleDirectories"))

(defun systemd-socket-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "MountFlags"))

(defun systemd-socket-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PrivateTmp"))

(defun systemd-socket-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PrivateNetwork"))

(defun systemd-socket-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "PrivateDevices"))

(defun systemd-socket-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ProtectHome"))

(defun systemd-socket-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "ProtectSystem"))

(defun systemd-socket-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SameProcessGroup"))

(defun systemd-socket-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "UtmpIdentifier"))

(defun systemd-socket-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "UtmpMode"))

(defun systemd-socket-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SELinuxContext"))

(defun systemd-socket-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "AppArmorProfile"))

(defun systemd-socket-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SmackProcessLabel"))

(defun systemd-socket-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "IgnoreSIGPIPE"))

(defun systemd-socket-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "NoNewPrivileges"))

(defun systemd-socket-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SystemCallFilter"))

(defun systemd-socket-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SystemCallArchitectures"))

(defun systemd-socket-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SystemCallErrorNumber"))

(defun systemd-socket-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "Personality"))

(defun systemd-socket-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "RestrictAddressFamilies"))

(defun systemd-socket-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "RuntimeDirectoryMode"))

(defun systemd-socket-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "RuntimeDirectory"))

(defun systemd-socket-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KillMode"))

(defun systemd-socket-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "KillSignal"))

(defun systemd-socket-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SendSIGKILL"))

(defun systemd-socket-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Socket" "SendSIGHUP"))
;;; org.freedesktop.systemd1.Device

(defun systemd-device-SysFSPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Device" "SysFSPath"))
;;; org.freedesktop.systemd1.Mount

(defun systemd-mount-Where (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Where"))

(defun systemd-mount-What (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "What"))

(defun systemd-mount-Options (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Options"))

(defun systemd-mount-Type (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Type"))

(defun systemd-mount-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TimeoutUSec"))

(defun systemd-mount-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ControlPID"))

(defun systemd-mount-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "DirectoryMode"))

(defun systemd-mount-SloppyOptions (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SloppyOptions"))

(defun systemd-mount-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Result"))

(defun systemd-mount-ExecMount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ExecMount"))

(defun systemd-mount-ExecUnmount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ExecUnmount"))

(defun systemd-mount-ExecRemount (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ExecRemount"))

(defun systemd-mount-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Slice"))

(defun systemd-mount-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ControlGroup"))

(defun systemd-mount-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "MemoryCurrent"))

(defun systemd-mount-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUUsageNSec"))

(defun systemd-mount-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TasksCurrent"))

(defun systemd-mount-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Mount" "GetProcesses" args))

(defun systemd-mount-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Delegate"))

(defun systemd-mount-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUAccounting"))

(defun systemd-mount-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUShares"))

(defun systemd-mount-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StartupCPUShares"))

(defun systemd-mount-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUQuotaPerSecUSec"))

(defun systemd-mount-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOAccounting"))

(defun systemd-mount-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOWeight"))

(defun systemd-mount-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StartupIOWeight"))

(defun systemd-mount-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IODeviceWeight"))

(defun systemd-mount-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOReadBandwidthMax"))

(defun systemd-mount-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOWriteBandwidthMax"))

(defun systemd-mount-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOReadIOPSMax"))

(defun systemd-mount-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOWriteIOPSMax"))

(defun systemd-mount-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "BlockIOAccounting"))

(defun systemd-mount-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "BlockIOWeight"))

(defun systemd-mount-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StartupBlockIOWeight"))

(defun systemd-mount-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "BlockIODeviceWeight"))

(defun systemd-mount-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "BlockIOReadBandwidth"))

(defun systemd-mount-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "BlockIOWriteBandwidth"))

(defun systemd-mount-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "MemoryAccounting"))

(defun systemd-mount-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "MemoryLimit"))

(defun systemd-mount-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "DevicePolicy"))

(defun systemd-mount-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "DeviceAllow"))

(defun systemd-mount-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TasksAccounting"))

(defun systemd-mount-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TasksMax"))

(defun systemd-mount-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Environment"))

(defun systemd-mount-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "EnvironmentFiles"))

(defun systemd-mount-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "PassEnvironment"))

(defun systemd-mount-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "UMask"))

(defun systemd-mount-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitCPU"))

(defun systemd-mount-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitCPUSoft"))

(defun systemd-mount-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitFSIZE"))

(defun systemd-mount-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitFSIZESoft"))

(defun systemd-mount-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitDATA"))

(defun systemd-mount-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitDATASoft"))

(defun systemd-mount-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitSTACK"))

(defun systemd-mount-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitSTACKSoft"))

(defun systemd-mount-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitCORE"))

(defun systemd-mount-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitCORESoft"))

(defun systemd-mount-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRSS"))

(defun systemd-mount-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRSSSoft"))

(defun systemd-mount-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNOFILE"))

(defun systemd-mount-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNOFILESoft"))

(defun systemd-mount-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitAS"))

(defun systemd-mount-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitASSoft"))

(defun systemd-mount-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNPROC"))

(defun systemd-mount-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNPROCSoft"))

(defun systemd-mount-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitMEMLOCK"))

(defun systemd-mount-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitMEMLOCKSoft"))

(defun systemd-mount-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitLOCKS"))

(defun systemd-mount-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitLOCKSSoft"))

(defun systemd-mount-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitSIGPENDING"))

(defun systemd-mount-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitSIGPENDINGSoft"))

(defun systemd-mount-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitMSGQUEUE"))

(defun systemd-mount-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitMSGQUEUESoft"))

(defun systemd-mount-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNICE"))

(defun systemd-mount-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitNICESoft"))

(defun systemd-mount-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRTPRIO"))

(defun systemd-mount-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRTPRIOSoft"))

(defun systemd-mount-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRTTIME"))

(defun systemd-mount-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "LimitRTTIMESoft"))

(defun systemd-mount-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "WorkingDirectory"))

(defun systemd-mount-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "RootDirectory"))

(defun systemd-mount-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "OOMScoreAdjust"))

(defun systemd-mount-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Nice"))

(defun systemd-mount-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IOScheduling"))

(defun systemd-mount-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUSchedulingPolicy"))

(defun systemd-mount-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUSchedulingPriority"))

(defun systemd-mount-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUAffinity"))

(defun systemd-mount-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TimerSlackNSec"))

(defun systemd-mount-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CPUSchedulingResetOnFork"))

(defun systemd-mount-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "NonBlocking"))

(defun systemd-mount-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StandardInput"))

(defun systemd-mount-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StandardOutput"))

(defun systemd-mount-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "StandardError"))

(defun systemd-mount-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TTYPath"))

(defun systemd-mount-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TTYReset"))

(defun systemd-mount-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TTYVHangup"))

(defun systemd-mount-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "TTYVTDisallocate"))

(defun systemd-mount-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SyslogPriority"))

(defun systemd-mount-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SyslogIdentifier"))

(defun systemd-mount-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SyslogLevelPrefix"))

(defun systemd-mount-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SyslogLevel"))

(defun systemd-mount-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SyslogFacility"))

(defun systemd-mount-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SecureBits"))

(defun systemd-mount-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "CapabilityBoundingSet"))

(defun systemd-mount-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "AmbientCapabilities"))

(defun systemd-mount-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "User"))

(defun systemd-mount-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Group"))

(defun systemd-mount-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SupplementaryGroups"))

(defun systemd-mount-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "PAMName"))

(defun systemd-mount-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ReadWriteDirectories"))

(defun systemd-mount-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ReadOnlyDirectories"))

(defun systemd-mount-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "InaccessibleDirectories"))

(defun systemd-mount-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "MountFlags"))

(defun systemd-mount-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "PrivateTmp"))

(defun systemd-mount-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "PrivateNetwork"))

(defun systemd-mount-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "PrivateDevices"))

(defun systemd-mount-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ProtectHome"))

(defun systemd-mount-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "ProtectSystem"))

(defun systemd-mount-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SameProcessGroup"))

(defun systemd-mount-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "UtmpIdentifier"))

(defun systemd-mount-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "UtmpMode"))

(defun systemd-mount-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SELinuxContext"))

(defun systemd-mount-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "AppArmorProfile"))

(defun systemd-mount-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SmackProcessLabel"))

(defun systemd-mount-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "IgnoreSIGPIPE"))

(defun systemd-mount-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "NoNewPrivileges"))

(defun systemd-mount-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SystemCallFilter"))

(defun systemd-mount-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SystemCallArchitectures"))

(defun systemd-mount-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SystemCallErrorNumber"))

(defun systemd-mount-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "Personality"))

(defun systemd-mount-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "RestrictAddressFamilies"))

(defun systemd-mount-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "RuntimeDirectoryMode"))

(defun systemd-mount-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "RuntimeDirectory"))

(defun systemd-mount-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "KillMode"))

(defun systemd-mount-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "KillSignal"))

(defun systemd-mount-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SendSIGKILL"))

(defun systemd-mount-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Mount" "SendSIGHUP"))
;;; org.freedesktop.systemd1.Scope

(defun systemd-scope-Controller (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "Controller"))

(defun systemd-scope-TimeoutStopUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "TimeoutStopUSec"))

(defun systemd-scope-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "Result"))

(defun systemd-scope-Abandon (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Scope" "Abandon" args))

(defun systemd-scope-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "Slice"))

(defun systemd-scope-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "ControlGroup"))

(defun systemd-scope-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "MemoryCurrent"))

(defun systemd-scope-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "CPUUsageNSec"))

(defun systemd-scope-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "TasksCurrent"))

(defun systemd-scope-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Scope" "GetProcesses" args))

(defun systemd-scope-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "Delegate"))

(defun systemd-scope-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "CPUAccounting"))

(defun systemd-scope-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "CPUShares"))

(defun systemd-scope-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "StartupCPUShares"))

(defun systemd-scope-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "CPUQuotaPerSecUSec"))

(defun systemd-scope-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOAccounting"))

(defun systemd-scope-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOWeight"))

(defun systemd-scope-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "StartupIOWeight"))

(defun systemd-scope-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IODeviceWeight"))

(defun systemd-scope-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOReadBandwidthMax"))

(defun systemd-scope-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOWriteBandwidthMax"))

(defun systemd-scope-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOReadIOPSMax"))

(defun systemd-scope-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "IOWriteIOPSMax"))

(defun systemd-scope-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "BlockIOAccounting"))

(defun systemd-scope-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "BlockIOWeight"))

(defun systemd-scope-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "StartupBlockIOWeight"))

(defun systemd-scope-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "BlockIODeviceWeight"))

(defun systemd-scope-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "BlockIOReadBandwidth"))

(defun systemd-scope-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "BlockIOWriteBandwidth"))

(defun systemd-scope-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "MemoryAccounting"))

(defun systemd-scope-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "MemoryLimit"))

(defun systemd-scope-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "DevicePolicy"))

(defun systemd-scope-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "DeviceAllow"))

(defun systemd-scope-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "TasksAccounting"))

(defun systemd-scope-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "TasksMax"))

(defun systemd-scope-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "KillMode"))

(defun systemd-scope-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "KillSignal"))

(defun systemd-scope-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "SendSIGKILL"))

(defun systemd-scope-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Scope" "SendSIGHUP"))
;;; org.freedesktop.systemd1.Target
;;; org.freedesktop.systemd1.Swap

(defun systemd-swap-What (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "What"))

(defun systemd-swap-Priority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Priority"))

(defun systemd-swap-Options (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Options"))

(defun systemd-swap-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TimeoutUSec"))

(defun systemd-swap-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ControlPID"))

(defun systemd-swap-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Result"))

(defun systemd-swap-ExecActivate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ExecActivate"))

(defun systemd-swap-ExecDeactivate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ExecDeactivate"))

(defun systemd-swap-Slice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Slice"))

(defun systemd-swap-ControlGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ControlGroup"))

(defun systemd-swap-MemoryCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "MemoryCurrent"))

(defun systemd-swap-CPUUsageNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUUsageNSec"))

(defun systemd-swap-TasksCurrent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TasksCurrent"))

(defun systemd-swap-GetProcesses (bus path &rest args)
  (apply #'dbus-call-method bus systemd-dbus-service path
	 "org.freedesktop.systemd1.Swap" "GetProcesses" args))

(defun systemd-swap-Delegate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Delegate"))

(defun systemd-swap-CPUAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUAccounting"))

(defun systemd-swap-CPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUShares"))

(defun systemd-swap-StartupCPUShares (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StartupCPUShares"))

(defun systemd-swap-CPUQuotaPerSecUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUQuotaPerSecUSec"))

(defun systemd-swap-IOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOAccounting"))

(defun systemd-swap-IOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOWeight"))

(defun systemd-swap-StartupIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StartupIOWeight"))

(defun systemd-swap-IODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IODeviceWeight"))

(defun systemd-swap-IOReadBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOReadBandwidthMax"))

(defun systemd-swap-IOWriteBandwidthMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOWriteBandwidthMax"))

(defun systemd-swap-IOReadIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOReadIOPSMax"))

(defun systemd-swap-IOWriteIOPSMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOWriteIOPSMax"))

(defun systemd-swap-BlockIOAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "BlockIOAccounting"))

(defun systemd-swap-BlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "BlockIOWeight"))

(defun systemd-swap-StartupBlockIOWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StartupBlockIOWeight"))

(defun systemd-swap-BlockIODeviceWeight (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "BlockIODeviceWeight"))

(defun systemd-swap-BlockIOReadBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "BlockIOReadBandwidth"))

(defun systemd-swap-BlockIOWriteBandwidth (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "BlockIOWriteBandwidth"))

(defun systemd-swap-MemoryAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "MemoryAccounting"))

(defun systemd-swap-MemoryLimit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "MemoryLimit"))

(defun systemd-swap-DevicePolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "DevicePolicy"))

(defun systemd-swap-DeviceAllow (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "DeviceAllow"))

(defun systemd-swap-TasksAccounting (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TasksAccounting"))

(defun systemd-swap-TasksMax (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TasksMax"))

(defun systemd-swap-Environment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Environment"))

(defun systemd-swap-EnvironmentFiles (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "EnvironmentFiles"))

(defun systemd-swap-PassEnvironment (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "PassEnvironment"))

(defun systemd-swap-UMask (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "UMask"))

(defun systemd-swap-LimitCPU (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitCPU"))

(defun systemd-swap-LimitCPUSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitCPUSoft"))

(defun systemd-swap-LimitFSIZE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitFSIZE"))

(defun systemd-swap-LimitFSIZESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitFSIZESoft"))

(defun systemd-swap-LimitDATA (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitDATA"))

(defun systemd-swap-LimitDATASoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitDATASoft"))

(defun systemd-swap-LimitSTACK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitSTACK"))

(defun systemd-swap-LimitSTACKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitSTACKSoft"))

(defun systemd-swap-LimitCORE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitCORE"))

(defun systemd-swap-LimitCORESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitCORESoft"))

(defun systemd-swap-LimitRSS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRSS"))

(defun systemd-swap-LimitRSSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRSSSoft"))

(defun systemd-swap-LimitNOFILE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNOFILE"))

(defun systemd-swap-LimitNOFILESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNOFILESoft"))

(defun systemd-swap-LimitAS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitAS"))

(defun systemd-swap-LimitASSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitASSoft"))

(defun systemd-swap-LimitNPROC (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNPROC"))

(defun systemd-swap-LimitNPROCSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNPROCSoft"))

(defun systemd-swap-LimitMEMLOCK (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitMEMLOCK"))

(defun systemd-swap-LimitMEMLOCKSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitMEMLOCKSoft"))

(defun systemd-swap-LimitLOCKS (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitLOCKS"))

(defun systemd-swap-LimitLOCKSSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitLOCKSSoft"))

(defun systemd-swap-LimitSIGPENDING (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitSIGPENDING"))

(defun systemd-swap-LimitSIGPENDINGSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitSIGPENDINGSoft"))

(defun systemd-swap-LimitMSGQUEUE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitMSGQUEUE"))

(defun systemd-swap-LimitMSGQUEUESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitMSGQUEUESoft"))

(defun systemd-swap-LimitNICE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNICE"))

(defun systemd-swap-LimitNICESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitNICESoft"))

(defun systemd-swap-LimitRTPRIO (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRTPRIO"))

(defun systemd-swap-LimitRTPRIOSoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRTPRIOSoft"))

(defun systemd-swap-LimitRTTIME (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRTTIME"))

(defun systemd-swap-LimitRTTIMESoft (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "LimitRTTIMESoft"))

(defun systemd-swap-WorkingDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "WorkingDirectory"))

(defun systemd-swap-RootDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "RootDirectory"))

(defun systemd-swap-OOMScoreAdjust (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "OOMScoreAdjust"))

(defun systemd-swap-Nice (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Nice"))

(defun systemd-swap-IOScheduling (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IOScheduling"))

(defun systemd-swap-CPUSchedulingPolicy (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUSchedulingPolicy"))

(defun systemd-swap-CPUSchedulingPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUSchedulingPriority"))

(defun systemd-swap-CPUAffinity (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUAffinity"))

(defun systemd-swap-TimerSlackNSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TimerSlackNSec"))

(defun systemd-swap-CPUSchedulingResetOnFork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CPUSchedulingResetOnFork"))

(defun systemd-swap-NonBlocking (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "NonBlocking"))

(defun systemd-swap-StandardInput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StandardInput"))

(defun systemd-swap-StandardOutput (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StandardOutput"))

(defun systemd-swap-StandardError (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "StandardError"))

(defun systemd-swap-TTYPath (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TTYPath"))

(defun systemd-swap-TTYReset (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TTYReset"))

(defun systemd-swap-TTYVHangup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TTYVHangup"))

(defun systemd-swap-TTYVTDisallocate (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "TTYVTDisallocate"))

(defun systemd-swap-SyslogPriority (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SyslogPriority"))

(defun systemd-swap-SyslogIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SyslogIdentifier"))

(defun systemd-swap-SyslogLevelPrefix (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SyslogLevelPrefix"))

(defun systemd-swap-SyslogLevel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SyslogLevel"))

(defun systemd-swap-SyslogFacility (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SyslogFacility"))

(defun systemd-swap-SecureBits (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SecureBits"))

(defun systemd-swap-CapabilityBoundingSet (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "CapabilityBoundingSet"))

(defun systemd-swap-AmbientCapabilities (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "AmbientCapabilities"))

(defun systemd-swap-User (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "User"))

(defun systemd-swap-Group (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Group"))

(defun systemd-swap-SupplementaryGroups (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SupplementaryGroups"))

(defun systemd-swap-PAMName (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "PAMName"))

(defun systemd-swap-ReadWriteDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ReadWriteDirectories"))

(defun systemd-swap-ReadOnlyDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ReadOnlyDirectories"))

(defun systemd-swap-InaccessibleDirectories (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "InaccessibleDirectories"))

(defun systemd-swap-MountFlags (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "MountFlags"))

(defun systemd-swap-PrivateTmp (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "PrivateTmp"))

(defun systemd-swap-PrivateNetwork (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "PrivateNetwork"))

(defun systemd-swap-PrivateDevices (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "PrivateDevices"))

(defun systemd-swap-ProtectHome (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ProtectHome"))

(defun systemd-swap-ProtectSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "ProtectSystem"))

(defun systemd-swap-SameProcessGroup (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SameProcessGroup"))

(defun systemd-swap-UtmpIdentifier (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "UtmpIdentifier"))

(defun systemd-swap-UtmpMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "UtmpMode"))

(defun systemd-swap-SELinuxContext (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SELinuxContext"))

(defun systemd-swap-AppArmorProfile (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "AppArmorProfile"))

(defun systemd-swap-SmackProcessLabel (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SmackProcessLabel"))

(defun systemd-swap-IgnoreSIGPIPE (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "IgnoreSIGPIPE"))

(defun systemd-swap-NoNewPrivileges (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "NoNewPrivileges"))

(defun systemd-swap-SystemCallFilter (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SystemCallFilter"))

(defun systemd-swap-SystemCallArchitectures (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SystemCallArchitectures"))

(defun systemd-swap-SystemCallErrorNumber (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SystemCallErrorNumber"))

(defun systemd-swap-Personality (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "Personality"))

(defun systemd-swap-RestrictAddressFamilies (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "RestrictAddressFamilies"))

(defun systemd-swap-RuntimeDirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "RuntimeDirectoryMode"))

(defun systemd-swap-RuntimeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "RuntimeDirectory"))

(defun systemd-swap-KillMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "KillMode"))

(defun systemd-swap-KillSignal (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "KillSignal"))

(defun systemd-swap-SendSIGKILL (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SendSIGKILL"))

(defun systemd-swap-SendSIGHUP (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Swap" "SendSIGHUP"))
;;; org.freedesktop.systemd1.BusName

(defun systemd-bus-name-Name (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "Name"))

(defun systemd-bus-name-TimeoutUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "TimeoutUSec"))

(defun systemd-bus-name-ControlPID (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "ControlPID"))

(defun systemd-bus-name-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "Result"))

(defun systemd-bus-name-Activating (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "Activating"))

(defun systemd-bus-name-AcceptFileDescriptors (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.BusName" "AcceptFileDescriptors"))
;;; org.freedesktop.systemd1.Path

(defun systemd-path-Unit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Path" "Unit"))

(defun systemd-path-Paths (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Path" "Paths"))

(defun systemd-path-MakeDirectory (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Path" "MakeDirectory"))

(defun systemd-path-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Path" "DirectoryMode"))

(defun systemd-path-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Path" "Result"))
;;; org.freedesktop.systemd1.Automount

(defun systemd-automount-Where (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Automount" "Where"))

(defun systemd-automount-DirectoryMode (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Automount" "DirectoryMode"))

(defun systemd-automount-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Automount" "Result"))

(defun systemd-automount-TimeoutIdleUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Automount" "TimeoutIdleUSec"))
;;; org.freedesktop.systemd1.Timer

(defun systemd-timer-Unit (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "Unit"))

(defun systemd-timer-TimersMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "TimersMonotonic"))

(defun systemd-timer-TimersCalendar (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "TimersCalendar"))

(defun systemd-timer-NextElapseUSecRealtime (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "NextElapseUSecRealtime"))

(defun systemd-timer-NextElapseUSecMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "NextElapseUSecMonotonic"))

(defun systemd-timer-LastTriggerUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "LastTriggerUSec"))

(defun systemd-timer-LastTriggerUSecMonotonic (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "LastTriggerUSecMonotonic"))

(defun systemd-timer-Result (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "Result"))

(defun systemd-timer-AccuracyUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "AccuracyUSec"))

(defun systemd-timer-RandomizedDelayUSec (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "RandomizedDelayUSec"))

(defun systemd-timer-Persistent (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "Persistent"))

(defun systemd-timer-WakeSystem (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "WakeSystem"))

(defun systemd-timer-RemainAfterElapse (bus path)
  "Read only property."
  (dbus-get-property bus systemd-dbus-service path
		     "org.freedesktop.systemd1.Timer" "RemainAfterElapse"))

(provide 'systemd)
;;; systemd.el ends here
