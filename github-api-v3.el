;;; github-api-v3.el --- GitHub API v3 for Emacs

;; Copyright (C) 2014  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>

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

;; Work in progress.

;;; Code:

(require 'url)

(defcustom github-api-base-url "https://api.github.com"
  "*Base URL for GitHUB API v3."
  :type 'string)

(defun github-get-json (path)
  "Send a GET request and return parsed JSON output."
  (let ((url-package-name "GitHub-API-v3")
	(url-package-version "0.1")
	(url-privacy-level nil))
    (with-current-buffer
	(url-retrieve-synchronously (concat github-api-base-url path))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (json-read))))

(defun github-profile (user)
  "Get the profile of a particular USER."
  (github-get-json (concat "/users/" user)))

(defun github-received-events (user)
  (github-get-json (concat "/users/" user "/received_events")))

(defun github-repos (user)
  (github-get-json (concat "/users/" user "/repos")))

(provide 'github-api-v3)
;;; github-api-v3.el ends here
