;;; -*- lexical-binding: t; -*-
(require 'cl)
	 
(defstruct duration hours minutes)

(defun seq-chop (list count)
  (let (
	(head (seq-take list count))
	(rest (seq-drop list count)))
    (if (null rest)
	(list head)
      (cons head (chop rest count)))))

(defun string-to-duration (string-duration)
  (let ((parts (mapcar 'string-to-number
		       (split-string string-duration ":"))))
    (normalize-duration
     (make-duration :hours (car parts) :minutes (cadr parts)))))

(defun duration-to-string (duration)
  (format "%02d:%02d" (duration-hours duration) (duration-minutes duration)))

(defun normalize-duration (duration)
  (let (
	(hh (duration-hours duration))
	(mm (duration-minutes duration)))
    (make-duration :hours (+ hh (/ mm 60))
		   :minutes (mod mm 60))))

(defun add-durations (duration1 duration2)
  (make-duration :hours (+ (duration-hours duration1) (duration-hours duration2))
		 :minutes (+ (duration-minutes duration1) (duration-minutes duration2))))
  
(defun sum-durations (durations)
  (normalize-duration
   (seq-reduce 'add-durations durations (make-duration :hours 0 :minutes 0))))

(defun count-launches (rows launch-type)
  (length
   (seq-filter (lambda (elt) (equal (caddr elt) launch-type)) rows)))

(defun string-to-date (string-date)
  ;; yyyy/mm/dd
  (string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" string-date)
  (mapcar 'string-to-number
	  (list (match-string 1 string-date) (match-string 2 string-date) (match-string 3 string-date))))

(defun date-to-string (date)
  (format "%02d/%02d/%d" (car date) (cadr date) (caddr date)))

(defun date< (date1 date2)
  (let (
	(year1  (caddr date1))
	(month1 (cadr  date1))
	(day1   (car   date1))
	(year2  (caddr date2))
	(month2 (cadr  date2))
	(day2   (car   date2)))
    (cond ((< year1 year2) t)
	  ((and (= year1 year2) (< month1 month2)) t)
	  ((and (= year1 year2) (= month1 month2) (< day1 day2)) t)
	  (t nil))))

(defun report-from-buffer (buffer)
  (let* ((lines
	  (with-current-buffer buffer
	    (split-string
	     (string-trim
	      (buffer-substring (point-min) (point-max) )) "\n")))
	 (raw-data
	  (mapcar
	   (lambda (line)
	     (mapcar 'string-trim
		     (split-string line ",")))
	   (cdr lines)))
	 (data-by-date
	  (make-hash-table :test #'equal))
	 (result ()))   
    (dolist (row raw-data)
      (let* ((date (car row))
	     (bucket (gethash date data-by-date ())))
	(puthash date (cons row bucket) data-by-date)))
    (maphash
     (lambda (date rows)
       (let (
	     (durations (mapcar 'string-to-duration (mapcar 'cadr rows) ))
	     (winch-count (count-launches rows "Winch"))
	     (aerotow-count (count-launches rows "Aerotow")))
	 (add-to-list 'result
		      (list
		       (string-to-date date)
		       (sum-durations durations)
		       winch-count
		       aerotow-count))))
     data-by-date)
    (seq-sort (lambda (record1 record2)
		(date< (car record1) (car record2)))
	      result)))

(defun create-logbook-page (report brought-forward-duration brought-forward-winch-count brought-forward-aerotow-count)
  (let (
	(glider-logbook-buffer (generate-new-buffer "glider-logbook.org"))
	(durations (mapcar 'cadr report))
	(winch-launches (mapcar 'caddr report))
	(aerotow-launches (mapcar 'cadddr report)))

    (insert "| Date | Time | Winch | Aerotow | Total\n")
    (insert "|------+------+-------+---------|\n")
    (insert (format "| Brought Forward  |  %s | %d  | %d  | %d |\n"
		    (duration-to-string brought-forward-duration)
		    brought-forward-winch-count
		    brought-forward-aerotow-count
		    (+ brought-forward-winch-count brought-forward-aerotow-count)))
    (insert "|------+------+-------+---------|\n")
    (dolist (report-line report)
      (let (
	    (date (date-to-string (car report-line)))
	    (duration (duration-to-string (cadr report-line)))
	    (winch-count (caddr report-line))
	    (aerotow-count (cadddr report-line)))
	(insert (format "|  %s    |   %s   |   %d    |    %d     |\n"
			date
			duration
			winch-count
			aerotow-count))))
    (insert "|------+------+-------+---------|\n")
    (let (
	  (total-duration (sum-durations (cons brought-forward-duration durations)))
	  (total-winch-launches (seq-reduce '+ winch-launches brought-forward-winch-count))
	  (total-aerotow-launches (seq-reduce '+ aerotow-launches brought-forward-aerotow-count)))
      (insert (format "| Carry forward  | %s  | %d   | %d  | %d |\n"
		      (duration-to-string total-duration)
		      total-winch-launches
		      total-aerotow-launches
		      (+ total-winch-launches total-aerotow-launches)))
      (org-table-align)
      (insert "\n")
      (list total-duration total-winch-launches total-aerotow-launches))))

;; Creates a logbook out of a CSV-BUFFER that contains a
;; CSV file exported from gops' Engineers Report.
;;
;; BROUGHT-FORWARD-DURATION needs to be an instance of DURATION
;; BROUGHT-FORWARD-WINCH-COUNT and BROUGHT-FORWARD-AEROTOW-COUNT are self explanatory.
;; ROWS-PER-PAGE is the number of rows that should be generated for each page of the log book.
(defun create-logbook (
			  csv-buffer
			  brought-forward-duration
			  brought-forward-winch-count
			  brought-forward-aerotow-count
			  rows-per-page)
  (let (
	(glider-logbook-buffer (generate-new-buffer "glider-logbook.org"))
	(report (report-from-buffer csv-buffer)))
    (with-current-buffer glider-logbook-buffer
      (seq-reduce (lambda (brought-forward-list report-section)
		    ;;(debug)
		    (create-logbook-page
		     report-section
		     (car brought-forward-list)
		     (cadr brought-forward-list)
		     (caddr brought-forward-list)))
		  (seq-chop report rows-per-page)
		  (list brought-forward-duration
			brought-forward-winch-count
			brought-forward-aerotow-count))
      (switch-to-buffer-other-window glider-logbook-buffer))))

(create-logbook
 "Engineering.csv"
 (make-duration :hours 10 :minutes 10)
 100
200
12)
