;;; weather.el --- Weather information for Emacs

;; Copyright (C) 2007  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run M-x weather to get a simple weather report for your area.

;;; Code:

(require 'cl)
(require 'solar)
(require 'url)

(defvar metar-stations-info-url "http://weather.noaa.gov/data/nsd_bbsss.txt"
  "*URL to use for retrieving station meta information.")

(defvar metar-stations nil
  "Variable containing (cached) METAR station information.
Use the function `metar-stations' to get the actual station list.")

(defun metar-stations ()
  "Retrieve a list of METAR stations.
Results are cached in variable `metar-stations'.
If this variable is nil, the information is retrieved from the Internet."
  (or metar-stations
      (let ((data (with-temp-buffer
		    (url-insert-file-contents metar-stations-info-url)
		    (mapcar (lambda (entry)
			      (split-string entry ";"))
			    (split-string (buffer-string) "\n")))))
	(setq metar-stations nil)
	(while data
	  (when (and (nth 7 (car data)) (nth 8 (car data))
		     (not (string= (nth 2 (car data)) "----")))
	    (setq metar-stations
		  (append
		   (let ((item (car data)))
		     (list
		      (list (cons 'code (nth 2 item))
			    (cons 'name (nth 3 item))
			    (cons 'country (nth 5 item))
			    (cons 'latitude
				  (when (string-match "^\\([0-9]+\\)-\\([0-9]+\\)\\(-[0-9]+\\)?\\([NS]\\)" (nth 7 item))
				      (funcall (if (string= (match-string 4 (nth 7 item)) "N") #'+ #'-)
					       (+ (string-to-number (match-string 1 (nth 7 item))) (/ (string-to-number (match-string 2 (nth 7 item))) 60.0)))))
			    (cons 'longitude
				  (when (string-match "^\\([0-9]+\\)-\\([0-9]+\\)\\(-[0-9]+\\)?\\([WE]\\)" (nth 8 item))
				    (funcall (if (string= (match-string 4 (nth 8 item)) "E") #'+ #'-)
					     (+ (string-to-number (match-string 1 (nth 8 item))) (/ (string-to-number (match-string 2 (nth 8 item))) 60.0)))))
			    (cons 'altitude (string-to-number (nth 12 item))))))
		   metar-stations)))
	  (setq data (cdr data)))
	;; (unless metar-timer
	;;   (setq metar-timer
	;; 	(run-with-timer 600 nil (lambda () (setq metar-stations nil)))))
	metar-stations)))

(defun metar-stations-get (station-code key)
  "Get meta information for station with STATION-CODE and KEY.
KEY can be one of the symbols `code', `name', `country', `latitude',
`longitude' or `altitude'."
  (let ((stations (metar-stations)) result)
    (while stations
      (when (string= (cdr (assoc 'code (car stations))) station-code)
	(setq result (cdr (assoc key (car stations)))
	      stations nil))
      (setq stations (cdr stations)))
    result))

(defun latitude-longitude-bearing (latitude1 longitude1 latitude2 longitude2)
  "Calculate bearing from start point LATITUDE1/LONGITUDE1 to end point
LATITUDE2/LONGITUDE2."
  (% (+ 360
	(truncate
	 (radians-to-degrees
	  (atan (* (sin (degrees-to-radians (- longitude2 longitude1)))
		   (cos (degrees-to-radians latitude2))) 
		(- (* (cos (degrees-to-radians latitude1))
		      (sin (degrees-to-radians latitude2)))
		   (* (sin (degrees-to-radians latitude1))
		      (cos (degrees-to-radians latitude2))
		      (cos (degrees-to-radians (- longitude2 longitude1)))))))))
     360))

(defun latitude-longitude-distance-haversine (latitude1 longitude1
					      latitude2 longitude2)
  "Caluclate the distance (in kilometers) between two points on the
surface of the earth given as LATITUDE1, LONGITUDE1, LATITUDE2 and LONGITUDE2."
  (macrolet ((distance (d1 d2)
	       `(expt (sin (/ (degrees-to-radians (- ,d2 ,d1)) 2)) 2)))
    (let ((a (+ (distance latitude1 latitude2)
		(* (cos (degrees-to-radians latitude1)) (cos (degrees-to-radians latitude2))
		   (distance longitude1 longitude2)))))
      (* 6371 (* 2 (atan (sqrt a) (sqrt (- 1 a))))))))

(defun metar-find-station-by-latitude/longitude (latitude longitude &optional
							  radius)
  "Find a station near the coordinates given by LATITUDE and LONGITUDE.
Returns a cons where car is the station code and cdr is the distance in
kilometers.
If RADIUS is non-nil, only stations within this range (in kilometers) are
considered.
If no match if found, nil is returned."
  (interactive
   (list
    (solar-get-number "Enter latitude (decimal fraction; + north, - south): ")
    (solar-get-number "Enter longitude (decimal fraction; + east, - west): ")))
  (let ((stations (metar-stations))
	(best-distance (or radius 10000))
	(station-code nil))
    (while stations
      (let ((station-latitude (cdr (assoc 'latitude (car stations))))
	    (station-longitude (cdr (assoc 'longitude (car stations)))))
	(when (and station-latitude station-longitude)
	  (let ((distance (latitude-longitude-distance-haversine
			   latitude longitude
			   station-latitude station-longitude)))
	    (when (< distance best-distance)
	      (setq best-distance distance
		    station-code (cdr (assoc 'code (car stations))))))))
      (setq stations (cdr stations)))
    (if (interactive-p)
	(if station-code
	    (message "%s, %s (%s) at %s is %d km away from %s."
		     (metar-stations-get station-code 'name)
		     (metar-stations-get station-code 'country)
		     station-code
		     (let ((float-output-format "%.1f"))
		       (format "%s%s, %s%s"
			       (abs (metar-stations-get station-code 'latitude))
			       (if (> (metar-stations-get station-code 'latitude) 0) "N" "S")
			       (abs (metar-stations-get station-code 'longitude))
			       (if (> (metar-stations-get station-code 'longitude) 0) "E" "W")))
		     best-distance
		     (let ((float-output-format "%.1f"))
		       (format "%s%s, %s%s"
			       (if (numberp latitude)
				   (abs latitude)
				 (+ (aref latitude 0)
				    (/ (aref latitude 1) 60.0)))
			       (if (numberp latitude)
				   (if (> latitude 0) "N" "S")
				 (if (equal (aref latitude 2) 'north) "N" "S"))
			       (if (numberp longitude)
				   (abs longitude)
				 (+ (aref longitude 0)
				    (/ (aref longitude 1) 60.0)))
			       (if (numberp longitude)
				   (if (> longitude 0) "E" "W")
				 (if (equal (aref longitude 2) 'east)
				     "E" "W")))))
	  (message "No appropriate station found."))
      (when station-code
	(cons station-code (round best-distance))))))

(defun metar-temp-to-number (string)
  (if (= (aref string 0) ?M)
      (- (string-to-number (substring string 1)))
    (string-to-number string)))

(defun metar-find-pressure (items)
  (let ((item (find-if (lambda (item) (string-match "^[QA][0-9]+$" item)) items)))
    (when item
      (cond
       ((= (aref item 0) ?Q)
	(cons (string-to-number (substring item 1)) 'hPa))
       ((= (aref item 0) ?A)
	(cons (string-to-number (substring item 1)) '\"Hg))))))

(defvar metar-wind-regexp
  "^\\(VRB\\|[0-9][0-9][0-9]\\)\\([0-9][0-9][0-9]?\\)\\(G\\([0-9][0-9][0-9]?\\)\\)?KT$"
  "Regular expression matching wind information in a METAR/SPECI record.")

(defun metar-find-wind (items)
  (let ((item (member-if (lambda (item) (string-match metar-wind-regexp item)) items)))
    (if item
	(let ((gust (when (match-string 3 (car item))
		      (string-to-number (match-string 4 (car item))))))
	  (append
	   (list :direction 
		 (if (string= (match-string 1 (car item)) "VRB")
		     (save-match-data
		       (let ((range (find-if (lambda (str) (string-match "^\\([0-9][0-9][0-9]\\)V\\([0-9][0-9][0-9]\\)$" str)) item)))
			 (if range
			     (cons (string-to-number (match-string 1 range)) (string-to-number (match-string 2 range)))
			   'variable)))
		   (string-to-number (match-string 1 (car item))))
		 :speed (string-to-number (match-string 2 (car item))))
	   (when gust
	     (list :gust gust)))))))

(defvar metar-url "http://weather.noaa.gov/pub/data/observations/metar/stations/%s.TXT"
  "*URL used to fetch station specific information.
%s is replaced with the 4 letter station code.")

(defvar metar-record-regexp "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\)\n\\(%s .*\\)"
  "*Regular expression used to extract METAR information from `metar-url'.
%s is replaced with the station code which always has to be present in a METAR
record.")

(defun metar-get-record (station)
  "Retrieve a METAR/SPECI record for STATION from the Internet.
REturns a cons where `car' is the time of the measurement (as an emacs-lsip
time value) and `cdr' is a string containing the actual METAR code.
If no record was found for STATION, nil is returned."
  (unless (string-match "^[A-Z][A-Z0-9][A-Z0-9][A-Z0-9]$" station)
    (signal 'error "Invalid station code"))
  (with-temp-buffer
    (url-insert-file-contents (format metar-url (upcase station)))
    (when (re-search-forward (format metar-record-regexp station) nil t)
      (cons (encode-time
	     0
	     (string-to-number (match-string 5))
	     (string-to-number (match-string 4))
	     (string-to-number (match-string 3))
	     (string-to-number (match-string 2))
	     (string-to-number (match-string 1))
	     0)
	    (match-string 6)))))

(defun metar-decode (record)
  "Return a lisp structure describing the weather information in RECORD."
  (when record
    (let* ((items (split-string (cdr record) " "))
	   (tempitem (find-if (lambda (item) (string-match "^M?[0-9]+/M?[0-9]+$" item)) items))
	   (temperature (when tempitem (metar-temp-to-number (nth 0 (split-string tempitem "/")))))
	   (dewpoint (when tempitem (metar-temp-to-number (nth 1 (split-string tempitem "/")))))
	   (pressure (metar-find-pressure items))
	   (wind (metar-find-wind items)))
      (list (cons 'station (nth 0 items))
	    (cons 'time-since (time-since (car record)))
	    (cons 'wind wind)
	    (cons 'temperature temperature)
	    (cons 'dewpoint dewpoint)
	    (cons 'humidity (when tempitem (round (magnus-formula-humidity-from-dewpoint temperature dewpoint))))
	    (cons 'pressure pressure)))))

(defun magnus-formula-humidity-from-dewpoint (temperature dewpoint)
  "Calculate relative humidity (in %) from TEMPERATURE and DEWPOINT (in
degrees celsius)."
  (* 10000
     (expt 10
	   (- (/ (- (* 0.4343
		       (+ 243.12 temperature)
		       (/ (* dewpoint 17.62)
			  (+ 243.12 dewpoint)))
		    (* 0.4343 17.62 temperature))
		 (+ 243.12 temperature))
	      2))))

(defun weather (&optional arg)
  "Display recent weather information.
If a prefix argument is given, prompt for the exact station code.
Otherwise, determine the best station via latitude/longitude."
  (interactive "p")
  (unless arg (setq arg 1))
  (let (station)
    (cond
     ((= arg 1)
      (unless calendar-longitude
	(setq calendar-longitude
	      (solar-get-number
	       "Enter longitude (decimal fraction; + east, - west): ")))
      (unless calendar-latitude
	(setq calendar-latitude
	      (solar-get-number
	     "Enter latitude (decimal fraction; + north, - south): ")))
      (when (and calendar-latitude calendar-longitude
		 (setq station (metar-find-station-by-latitude/longitude (calendar-latitude) (calendar-longitude))))
	(message "Found %s %d kilometers away." (car station) (cdr station))
	(setq station (car station))))
     ((= arg 4)
      (setq station (read-string "Enter station code: "))))
    (let ((info (metar-decode (metar-get-record station))))
      (if info
	  (message "%d minutes ago at %s: %d°C, %d%% relative humidity"
		   (/ (truncate (float-time (cdr (assoc 'time-since info)))) 60)
		   (or (metar-stations-get (cdr (assoc 'station info)) 'name) (cdr (assoc 'station info)))
		   (cdr (assoc 'temperature info))
		   (cdr (assoc 'humidity info)))
	(message "No weather information found, sorry.")))))
  
(defun metar-station-countries ()
  (let (countries (stations (metar-stations)))
    (while stations
      (let ((country (cdr (assoc 'country (car stations)))))
	(unless (member country countries)
	  (setq countries (append (list country) countries))))
      (setq stations (cdr stations)))
    countries))

(defun metar-average-temperature (country)
  "Display average temperature from all stations in COUNTRY."
  (interactive
   (list (completing-read "Country: " (metar-station-countries) nil t)))
  (let ((count 0) (temp-sum 0)
	(stations (metar-stations))
	(url-show-status nil)
	(progress (make-progress-reporter
		   "Downloading METAR records..."
		   0
		   (count-if (lambda (station)
			       (string= (cdr (assoc 'country station)) country))
			     (metar-stations)))))
    (while stations
      (when (string= (cdr (assoc 'country (car stations))) country)
	(let ((temp (cdr (assoc 'temperature
				(metar-decode
				 (metar-get-record
				  (cdr (assoc 'code (car stations)))))))))
	  (when temp
	    (setq temp-sum (+ temp-sum temp)
		  count (+ count 1))
	    (progress-reporter-update progress count))))
      (setq stations (cdr stations)))
    (progress-reporter-done progress)
    (if (interactive-p)
	(message "Average temperature in %s is %s"
		 country
		 (if (> count 0)
		     (format "%.1f°C (%d stations)"
			     (/ (float temp-sum) count)
			     count)
		   "unknown"))
      (when (> count 0)
	(/ (float temp-sum) count)))))

(provide 'weather)
;;; weather.el ends here
