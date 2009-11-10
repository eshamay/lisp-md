(in-package #:md)

(defclass xyz-file (coordinate-file) ())

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defmethod read-header ((xyz xyz-file))
  "The header of an XYZ file is comprised of one line containing the number of atoms in each frame, and a next line with some type of information about the frame (frame number, useful text, etc.). This method sets the number of atoms and then eats up the informational header info."
  (with-slots (file num-atoms) xyz
    (setf num-atoms (read-from-string (clean-string (read-line file nil nil))))
    (read-line file nil nil)
    num-atoms))

(defmethod parse-xyz-row ((xyz xyz-file))
  "XYZ files are written with a single atom on each row. The format is \(Atom-name  x  y  z\). This parses a single atom and returns the name & coordinates in a list"
  (with-slots (file) xyz
    (let ((line (read-line file nil nil)))
      (with-input-from-string (s (clean-string line))
	(loop for i = (read s nil nil) while i collect i)))))

(defmethod load-next-frame ((xyz xyz-file))
  (with-slots (num-atoms frame) xyz
    (setf num-atoms (read-header xyz))
    (if (not (null num-atoms))
	(setf frame
	      (loop repeat num-atoms collect (parse-xyz-row xyz)))
	nil)))

(defun xyz-frame-row->%atom (row)
  "Creates a _new_ atom from a row parsed from an xyz-file"
  (make-instance '%atom
		 :name (car row)
		 :pos (apply #'vecr (cdr row))))

(defmethod atoms ((xyz xyz-file))
  "This takes a single frame from an xyz file and translates it into a list of the atoms in the frame"
  (let ((frame (frame xyz))) ; grab the frame from the coordinate-file
    (map 'list #'xyz-frame-row->%atom frame)))
