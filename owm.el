;;; owm.el --- OpenWeatherMap client library for Emacs

;; Copyright (C) 2013  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, data

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

;; JSON client library for OpenWeatherMap.Org

;;; Code:

(require 'json)
(require 'url)

(defvar owm-weather-city-name-url "http://api.openweathermap.org/data/2.6/weather?q=%s&units=metric")
(defvar owm-weather-city-id-url "http://api.openweathermap.org/data/2.6/weather?id=%d&units=metric")
(defvar owm-weather-location-url "http://api.openweathermap.org/data/2.6/weather?lat=%f&lon=&f&units=metric")

(defun owm-weather (item)
  (with-temp-buffer
    (url-insert-file-contents
     (cond
      ((stringp item)
       (format owm-weather-city-name-url item))
      ((integerp item)
       (format owm-weather-city-id-url item))
      ((and (consp item) (numberp (car item)) (numberp (cdr item)))
       (format owm-weather-location-url (car item) (cdr item)))
      (t (error "Unknown search item"))))
    (json-read)))

(provide 'owm)
;;; owm.el ends here
