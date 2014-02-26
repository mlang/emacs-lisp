;;; github-api-v3.el --- Lightweight GitHub API v3 client for Emacs

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

;; This is a lightweight playground for the GitHub API in Emacs.
;;
;; For a much more complete implementation of the GitHub API for Emacs, see
;; https://github.com/sigma/gh.el and https://github.com/defunkt/gist.el
;;
;; We create an OAuth token which we cache in `github-oauth-token'.
;; If you restart Emacs without persisting that variable you will have to
;; re-enter your username/password once and the token will be retrieved
;; from GitHub again.

;;; Code:

(require 'json)
(require 'url)

(defgroup github-api ()
  "GitHub API support.")

(defcustom github-api-base-url "https://api.github.com"
  "*Base URL for GitHUB API v3."
  :group 'github-api
  :type 'string)

(defcustom github-oauth-token nil
  "Your GitHub OAuth token.

If you decide to save this setting, you will not be asked for your
GitHub password once you restart Emacs.  However, make sure you do not
leak your token accidentally when sharing Emacs customizations with others."
  :group 'github-api
  :type 'string)

(defun github--request-headers (url)
  (list (cons "Authorization"
	      (if (string-match "/authorizations$" url)
		  (url-get-authentication url nil "basic" t)
		(concat "token " (github-oauth-token))))))

(defun github-get-json (path &optional extra-headers)
  "Send a GET request and return parsed JSON output."
  (let* ((url (concat github-api-base-url path))
	 (url-package-name "GitHub-API-v3")
	 (url-package-version "0.1")
	 (url-privacy-level nil)
	 (url-request-extra-headers (or extra-headers
					(github--request-headers url))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (json-read))))

(defun github-post-json (path data &optional extra-headers)
  "Send a POST request and return parsed JSON output."
  (unless extra-headers (setq extra-headers (github--request-headers)))
  (setq extra-headers
	(append '(("Content-Type" . "application/json"))
		  extra-headers))
  (let* ((url (concat github-api-base-url path))
	 (url-package-name "GitHub-API-v3")
	 (url-package-version "0.1")
	 (url-privacy-level nil)
	 (url-request-data (json-encode data))
	 (url-request-extra-headers extra-headers)
	 (url-request-method "POST"))
    (with-current-buffer
	(url-retrieve-synchronously (concat github-api-base-url path))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (json-read))))

(defun github-delete-json (path)
  "Send a DELETE request."
  (let* ((url (concat github-api-base-url path))
	 (url-package-name "GitHub-API-v3")
	 (url-package-version "0.1")
	 (url-privacy-level nil)
	 (url-request-extra-headers (github--request-headers))
	 (url-request-method "DELETE"))
    (with-current-buffer (url-retrieve-synchronously url)
      ;; A successful request returns no JSON, so fake some.
      (goto-char (point-max))
      (insert "true")
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (json-read))))

(defun github-profile (user)
  "Get the profile of a particular USER."
  (github-get-json (concat "/users/" user)))

(defun github-following (user)
  (github-get-json (concat "/users/" user "/following")))

(defun github-followers (user)
  (github-get-json (concat "/users/" user "/followers")))

(defun github-received-events (user)
  (github-get-json (concat "/users/" user "/received_events")))

(defun github-repos (user)
  (github-get-json (concat "/users/" user "/repos")))

(defun github-authorizations ()
  (github-get-json "/authorizations"))

(defcustom github-token-scopes '("repo" "user")
  "A list of scopes to request."
  :group 'github-api)

(defconst github-app-url
  "https://github.com/mlang/emacs-lisp/blob/master/github-api-v3.el")

(defun github-oauth-token ()
  (setq github-oauth-token
	(or
	 ;; Check if we already know the token
	 (and (stringp github-oauth-token) github-oauth-token)

	 (let* ((url (concat github-api-base-url "/authorizations"))
		(extra-headers (github--request-headers url)))
	   (or
	    ;; Check if GitHub already has a token
	    (cdr-safe (assq 'token
			    (find github-app-url
				  (github-get-json "/authorizations"
						   extra-headers)
				  :test (lambda (x y)
					  (string= x
						   (cdr-safe
						    (assq 'url
							  (cdr-safe
							   (assq 'app y)))))))))
	    ;; Generate a new token remotely
	    (cdr-safe
	     (assq 'token
		   (github-post-json
		    "/authorizations"
		    (list (cons "scopes" github-token-scopes)
			  (cons "note" "github-api-v3.el")
			  (cons "note_url" github-app-url))
		    extra-headers))))))))

(provide 'github-api-v3)
;;; github-api-v3.el ends here
