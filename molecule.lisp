(in-package #:md)

(defclass %molecule (particle)
  ((atoms :accessor atoms
	  :initform (list))
   (com :accessor com :initform (vecr))))
   

(specialized-method-getter %molecule com com)
(specialized-method-getter %molecule atoms atoms)

(defun make-molecule (name &key (id nil))
  (make-instance '%molecule :name name :id id))

(defmethod add-atom ((m %molecule) (a %atom))
  (with-slots (atoms) m
    (pushnew a atoms) ; takes care of adding the atom into the molecule
    (setf (molecule a) m))) ; while this fixes the atom up

(defmethod num-atoms ((m %molecule))
  (length (atoms m)))

(defmethod calc-mass ((m %molecule))
  "Calculates the total molecular mass of the molecule"
  (with-slots (atoms mass) m
    (setf mass
	  (reduce #'+ (mapcar #'mass (atoms m))))
    mass))

(defmethod calc-com ((m %molecule))
  "Calculates the center of mass of the given molecule"
  (calc-mass m)
  (scale
   (reduce #'add
	  (mapcar #'(lambda (x) (scale (pos x) (mass x))) (atoms m)))
   (/ 1.0 (mass m))))

(defmethod print-object ((m %molecule) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (name id) m
      (format stream "~a[~d]" name id))))
