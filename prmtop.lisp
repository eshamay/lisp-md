(in-package #:md)

(defstruct topology
  (atom-names nil)
  (molecule-names nil)
  (atoms nil)
  (molecules nil))


(defun flag-p (str)
  "Checks if a given string contains a flag"
  (not (null (search "FLAG" str))))

(defun format-p (str)
  "Checks if a given string contains a format"
  (not (null (search "FORMAT" str))))

(defun parse-flag (str)
  "Takes a string known to contain a flag name and parses out the flag"
  (subseq str (+ 1 (mismatch "%FLAG" str))))

(defun string-to-numbers (str)
  "Given a string of numbers, the return is a list of those numbers"
  (with-input-from-string (s str)
	(loop for num = (read s nil nil)
		  while num collect num)))

(defun trim-string (str)
  "Remove all whitespace from around words in a string"
  (if (not (null str))
	(string-trim " " str)))

(defmacro add-flag (str hsh)
  "Adds a new flag to the prmtop-data"
  `(progn
	 (setf flag (intern (parse-flag ,str)))
	 (if (not (gethash flag ,hsh))
	   (setf (gethash flag ,hsh) '()))))

(defmacro add-data (str hsh)
  "Pushes incoming data into the proper location in the prmtop-data hash"
  `(let ((data (string-to-numbers ,str)))
	 (setf (gethash flag ,hsh) (push data (gethash flag ,hsh)))))

(defmacro arrange-hash-data ()
  "takes the hash data that was input in reverse and as a list of lists, and makes it one properly-ordered list"
  `(setf (gethash flag prmtop-data) 
	 (flatten-lists (reverse (gethash flag prmtop-data)))))

(defun load-prmtop (filename)
  (let ((prmtop-data (make-hash-table))
	(flag nil))
    (with-open-file (f filename
		       :direction :input)
      (read-line f nil nil)
      (loop for line = (trim-string (read-line f nil nil)) while (not (null line)) do
      (cond
	;; for each flag we find we parse the flag and make a new space for its data
	((flag-p line) 
	 (arrange-hash-data)
	 (add-flag line prmtop-data))
	((format-p line) nil)
	;; for incoming data, just grab it and put it into the data holder
	((not (eql flag nil)) (add-data line prmtop-data)))))
    prmtop-data))
	  
(defun flatten-lists (l)
  (cond
	((null l) nil)
	((atom l) (list l))
	(t (append (flatten-lists (car l))
			 (flatten-lists (cdr l))))))