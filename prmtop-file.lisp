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
      (read-line f nil nil)		; trim off the first header line
      (let ((alist ())
	    (data ()))
	(loop for line = (trim-string (read-line f nil nil)) while (not (null line)) do
	     (cond
	       ;; for each flag we find we parse the flag followed by the data it represents
	       ((flag-p line)
		(acons (car data) (cdr data) alist)
		(setf data (list (parse-flag line))))
	       ((format-p line) nil)
	       ((not (eql data nil))
		(nconc data (string-to-list line)))))
	(acons (car data) (cdr data) alist)
	alist)))