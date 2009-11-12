(in-package #:md)

(defun mdcrd-frame-to-vecr (frame)
  "Returns a list of vecr's from the list of coordinates in a frame"
  (mapcar #'(lambda (coordinate) (apply #'vecr coordinate)) frame))

(defun mdcrd-grab-next-frame (mdcrd atom-count)
  "Returns the list of vecr's that comprise the coordinates of the next frame"
  (grab-lists-from-stream atom-count 3 mdcrd))

(defun mdcrd-grab-next-coordinate (mdcrd)
  (grab-n-from-stream 3 mdcrd))

#|
(defmethod load-next-frame ((prmtop prmtop-data) mdcrd-stream)
  "Update the atom positions from the frame in the mdcrd file"
  (let ((new-positions
	 (mdcrd-frame-to-vecr (mdcrd-grab-next-frame
			       mdcrd-stream (prmtop-data-atom-count prmtop)))))
    (mapcar #'(lambda (a p) (move a p)) (prmtop-data-atoms prmtop) new-positions)))
|#  

