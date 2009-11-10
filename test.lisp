(defun parse-numbers-from-string (str)
  (with-input-from-string (s str)
   (loop for num = (read s nil nil) while num collect num)))

(defun parse-rows-from-file (f)
  (loop for line = (read-line f nil nil) while line collect	  
		(parse-numbers-from-string line)))

(defun parse-column-data-file (filename)
  (with-open-file (file filename)
	(parse-rows-from-file file)))

(defun parse-files (files)
 (mapcar #'parse-column-data-file files))

(format t "~{~{~a~%~}~%~}" (parse-files '("test.dat" "test1.dat")))
