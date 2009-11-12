(in-package #:md)

(defclass amber-system ()
  ((prmtop :initarg :prmtop :initform nil :accessor prmtop)
   (mdcrd  :initarg :mdcrd  :initform nil :accessor mdcrd)))

(defmethod atoms ((sys amber-system))
  (prmtop-data-atoms (slot-value sys 'prmtop)))

(defmethod molecules ((sys amber-system))
  (prmtop-data-molecules (slot-value sys 'prmtop)))

(defmethod atom-count ((sys amber-system))
  (prmtop-data-atom-count (slot-value sys 'prmtop)))

(defmethod molecule-count ((sys amber-system))
  (prmtop-data-molecule-count (slot-value sys 'prmtop)))

(defmethod load-next-frame ((sys amber-system))
  "Parses the coordinates for each atom in the next frame, and moves each atom to the new position. Then the system size is parsed"
  (with-slots (prmtop mdcrd) sys
    (loop for a in (prmtop-data-atoms prmtop) do
	 (move a (apply #'vecr (mdcrd-grab-next-coordinate mdcrd))))
    (apply #'set-system-size (mdcrd-grab-next-coordinate mdcrd))))	; pull out the system-size after each frame

(defmacro with-amber-system ((prmtop-file-name mdcrd-file-name &key (system-var 'sys)) &rest body)
  `(with-prmtop-file (prmtop ,prmtop-file-name)
     (with-open-file (mdcrd ,mdcrd-file-name)
       (let ((,system-var (make-instance 'amber-system :prmtop prmtop :mdcrd mdcrd)))
	 ,@body))))