;;; systemd-mode.el --- Major mode for systemd unit files  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: files

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

(require 'conf-mode)

(defvar systemd-unit-font-lock-keywords
  '(;; [section]
    ("^[ \t]*\\[\\(Unit\\|Service\\)\\]"
     1 'font-lock-type-face)
    ;; var=val
    ("^[ \t]*\\(.+?\\)[ \t]*="
     1 'font-lock-variable-name-face))
  "Keywords to highlight in Conf mode.")

(defvar systemd-unit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" #'pcomplete)
    map))

(defvar-local systemd-unit-mode-sections '("Unit" "Install"))

;;;###autoload
(define-derived-mode systemd-unit-mode conf-unix-mode "Systemd-Unit"
  (conf-mode-initialize "#" systemd-unit-font-lock-keywords))

;;;###autoload
(define-derived-mode systemd-automount-mode systemd-unit-mode "Systemd-AutoMount"
  (add-to-list 'systemd-unit-mode-sections "AutoMount"))

;;;###autoload
(define-derived-mode systemd-mount-mode systemd-unit-mode "Systemd-Mount"
  (add-to-list 'systemd-unit-mode-sections "Mount"))

;;;###autoload
(define-derived-mode systemd-service-mode systemd-unit-mode "Systemd-Service"
  (add-to-list 'systemd-unit-mode-sections "Service"))

;;;###autoload
(define-derived-mode systemd-socket-mode systemd-unit-mode "Systemd-Socket"
  (add-to-list 'systemd-unit-mode-sections "Socket"))

;;;###autoload
(define-derived-mode systemd-timer-mode systemd-unit-mode "Systemd-Timer"
  (add-to-list 'systemd-unit-mode-sections "Timer"))

(defvar systemd-mode-section-keywords-alist
  ;; Currently unused
  '(("AutoMount"
     "Where" "DirectoryMode" "TimeoutIdleSec")
    ("Device")
    ("Mount"
     "What" "Where" "Type" "Options" "SloppyOptions" "DirectoryMode"
     "TimeoutSec")
    ("Path"
     "PathExists" "PathExistsGlob" "PathChanged" "PathModified"
     "DirectoryNotEmpty" "Unit" "MakeDirectory" "DirectoryMode")
    ("Service"
     "Type" "RemainAfterExit" "GuessMainPID" "PIDFile" "BusName"
     "ExecStart" "ExecStartPre" "ExecStartPost" "ExecReload"
     "ExecStop" "ExecStopPost" "RestartSec" "TimeoutStartSec"
     "TimeoutStopSec" "TimeoutSec" "RuntimeMaxSec" "WatchdogSec"
     "Restart" "SuccessExitStatus" "RestartPreventExitStatus"
     "RestartForceExitStatus" "PermissionsStartOnly"
     "RootDirectoryStartOnly" "NonBlocking" "NotifyAccess" "Sockets"
     "FailureAction" "FileDescriptorStoreMax" "USBFunctionDescriptors"
     "USBFunctionStrings")
    ("Slice")
    ("Socket"
     "ListenStream" "ListenDatagram" "ListenSequentialPacket"
     "ListenFIFO" "ListenSpecial" "ListenNetlink" "ListenMessageQueue"
     "ListenUSBFunction" "SocketProtocol" "BindIPv6Only"
     "Backlog" "BindToDevice" "SocketUser" "SocketGroup"
     "SocketMode" "DirectoryMode" "Accept" "Writable" "MaxConnections"
     "KeepAlive" "KeepAliveTimeSec" "KeepAliveIntervalSec"
     "KeepAliveProbes" "NoDelay" "Priority" "DeferAcceptSec"
     "ReceiveBuffer" "SendBuffer" "IPTOS" "IPTTL" "Mark" "ReusePort"
     "SmackLabel" "SmackLabelIPIn" "SmackLabelIPOut"
     "SELinuxContextFromNet" "PipeSize" "MessageQueueMaxMessages"
     "MessageQueueMessageSize" "FreeBind" "Transparent" "Broadcast"
     "PassCredentials" "PassSecurity" "TCPCongestion" "ExecStartPre"
     "ExecStartPost" "ExecStopPre" "ExecStopPost" "TimeoutSec"
     "Service" "RemoveOnStop" "Symlinks" "FileDescriptorName"
     "TriggerLimitIntervalSec" "TriggerLimitBurst")
    ("Swap"
     "What" "Priority" "Options" "TimeoutSec")
    ("Target")
    ("Timer"
     "OnActiveSec" "OnBootSec" "OnStartupSec" "OnUnitActiveSec"
     "OnUnitInactiveSec" "OnCalendar" "AccuracySec" "RandomizedDelaySec"
     "Unit" "Persistent" "WakeSystem" "RemainAfterElapse")
    ("Unit"
     "Description" "Documentation" "Requires" "Requisite" "Wants" "BindsTo"
     "PartOf" "Conflicts" "Before" "After" "OnFailure" "PropagatesReloadTo"
     "ReloadPropagatedFrom" "JoinsNamespaceOf" "RequiresMountsFor"
     "OnFailureJobMode" "IgnoreOnIsolate" "StopWhenUnneeded" "RefuseManualStart"
     "RefuseManualStop" "AllowIsolate" "DefaultDependencies"
     "JobTimeoutSec" "JobTimeoutAction" "JobTimeoutRebootArgument"
     "StartLimitIntervalSec" "StartLimitBurst"
     "StartLimitAction" "RebootArgument"
     "ConditionArchitecture" "ConditionVirtualization" "ConditionHost"
     "ConditionKernelCommandLine" "ConditionSecurity" "ConditionCapability"
     "ConditionACPower" "ConditionNeedsUpdate" "ConditionFirstBoot"
     "ConditionPathExists" "ConditionPathExistsGlob" "ConditionPathIsDirectory"
     "ConditionPathIsSymbolicLink" "ConditionPathIsMountPoint"
     "ConditionPathIsReadWrite" "ConditionDirectoryNotEmpty"
     "ConditionFileNotEmpty" "ConditionFileIsExecutable"
     "AssertArchitecture" "AssertVirtualization" "AssertHost"
     "AssertKernelCommandLine" "AssertSecurity" "AssertCapability"
     "AssertACPower" "AssertNeedsUpdate" "AssertFirstBoot" "AssertPathExists"
     "AssertPathExistsGlob" "AssertPathIsDirectory" "AssertPathIsSymbolicLink"
     "AssertPathIsMountPoint" "AssertPathIsReadWrite" "AssertDirectoryNotEmpty"
     "AssertFileNotEmpty" "AssertFileIsExecutable"
     "SourcePath")
    ("Install"
     "Alias" "WantedBy" "RequiredBy" "Also" "DefaultInstance")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.automount\\'" . systemd-automount-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mount\\'" . systemd-mount-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-service-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.socket\\'" . systemd-socket-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-timer-mode))

(provide 'systemd-mode)
;;; systemd-mode.el ends here
