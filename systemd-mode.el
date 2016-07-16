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

;;;###autoload
(define-derived-mode systemd-unit-mode conf-unix-mode "Systemd-Unit"
  (conf-mode-initialize "#" systemd-unit-font-lock-keywords)
  (setq-local pcomplete-command-completion-function
	      (lambda ()
		(pcomplete-here '("Description" "Documentation"
				  "Requires" "Requisite" "Wants" "BindsTo"
				  "PartOf" "Conflicts" "Before" "After"
				  "OnFailure" "PropagatesReloadTo" "ReloadPropagatedFrom"
				  "JoinsNamespaceOf" "RequiresMountsFor"
				  "OnFailureJobMode" "IgnoreOnIsolate"
				  "StopWhenUnneeded" "RefuseManualStart" "RefuseManualStop"))))
  (setq-local pcomplete-termination-string "="))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-unit-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-unit-mode))

(provide 'systemd-mode)
;;; systemd-mode.el ends here
