(in-package #:md)

(defun flag-p (str)
  "Checks if a given string contains a flag"
  (not (null (search "FLAG" str))))

(defun format-p (str)
  "Checks if a given string contains a format"
  (not (null (search "FORMAT" str))))

(defun parse-flag (str)
  "Takes a string known to contain a flag name and parses out the flag"
  (subseq str (+ 1 (mismatch "%FLAG" str))))

(defun string-to-list (str)
  "Given a string of numbers, the return is a list of those numbers"
  (with-input-from-string (s str)
	(loop for num = (read s nil nil)
		  while num collect num)))

(defun trim-string (str)
  "Remove all whitespace from around words in a string"
  (if (not (null str))
	(string-trim " " str)))

(defun load-prmtop-file (filename)
  "Parses the data from a prmtop file and returns an alist:
          key = %FLAG values
          value = data that follows"
  (with-open-file (f filename
		     :direction :input)
      (read-line f nil nil)		; trim off the first header line and skip the header flag
      (let ((alist ())			; This holds the parsed flag/data from the prmtop file
	    (data ()))			; While this is used to build up individual entries
	(loop for line = (trim-string (read-line f nil nil))
	   while (not (null line)) do
	     (cond			; 3 possible tasks - parse a flag, a format, or data
	       ;; Check which task to perform, and build the data
	       ((flag-p line)		; Parsing flags
		(acons (car data) (cdr data) alist) ; Push any previous data into the alist
		(setf data (list (parse-flag line)))) ;... and then begin adding data to the new flag
	       ((format-p line) nil)	; Skipping formatting strings
	       ((not (eql data nil))	; Parsing data to be paired with the flag
		(nconc data (string-to-list line)))))
	(acons (car data) (cdr data) alist)
	alist)))