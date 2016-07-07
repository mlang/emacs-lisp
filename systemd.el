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

(defun systemd-remote-bus (host &optional address)
  (unless address
    (setq address "unix:path=/run/dbus/system_bus_socket"))
  (concat "unixexec:"
          "path=ssh"
          ",argv1=-xT"
          ",argv2=" (systemd-escape-dbus-address host)
          ",argv3=systemd-stdio-bridge"
          ",argv4=" (systemd-escape-dbus-address (concat "--bus-path=" address))))

(provide 'systemd)
;;; systemd.el ends here
