(in-package #:md)

(defclass particle ()
  ((pos :accessor pos :initarg :pos :initform (vecr))
   (name :accessor name :initarg :name)
   (id :accessor id :initarg :id :initform nil)
   (mass :accessor mass :initarg :mass :initform 0.0)
   (charge :accessor charge :initarg :charge :initform 0.0)))

(defvar *system-size* nil)
(defun set-system-size (x y z)
  "Allows us to set the system size to enable calculations involving periodic boundaries (on orthorhombic systems)"
  (setf *system-size* (vecr x y z)))

(defclass %atom (particle)
   ((name :initarg :name
	  :initform (error "(%atom) :: Every atom in the system must have a name!"))
    (molecule :accessor molecule :initarg :molecule :initform nil)))

(defvar *atomic-masses*
  '((H 1.008) (D 2.014) (T 3.016)
    (C 12.0108)
    (N 14.0067)
    (O 15.9994)
    (Na 22.9898)
    (Si 28.0855)
    (P 30.9738)
    (Cl 35.453)
    (K 39.0983)
    (Ca 40.078)
    (Br 79.904)
    (I 126.9045)))
    
(defvar *atomic-charges*
  '((H 1.0) (D 1.0) (T 1.0)
    (C 4.0)
    (N 5.0)
    (O -2.0)
    (Na 1.0)
    (Si 4.0)
    (Cl -1.0)
    (K 1.0)
    (Ca 2.0)
    (Br -1.0)
    (I -1.0)))

;; generate all the generic functions for setting/getting slots
(define-generic-slot-setter id id)
(define-generic-slot-setter molecule molecule)

(define-generic-slot-getter name)
(define-generic-slot-getter id)
(define-generic-slot-getter charge)
(define-generic-slot-getter nass)
(define-generic-slot-getter pos)
(define-generic-slot-getter molecule)
(define-generic-slot-getter com)

(defgeneric move (obj location)
  (:documentation "Move an object to a new location"))
(defgeneric shift (obj offset)
  (:documentation "Shifts an object by an offset to a new location"))
(defgeneric rename (obj name)
  (:documentation "Rename an object"))

(specialized-method-getter particle name name)
(specialized-method-getter particle id id)
(specialized-method-getter particle mass mass)
(specialized-method-getter particle charge charge)
(specialized-method-getter particle pos pos)
(specialized-method-getter %atom molecule molecule)

(specialized-method-setter particle mass mass)
(specialized-method-setter particle id id)
(specialized-method-setter %atom molecule molecule)

(defmethod move ((p particle) position)
  (if (vecr-p position)
      (setf (slot-value p 'pos) position)
      (error "[particle:move] :: Using a non-vector to specify a new location")))
(defmethod shift ((p particle) offset)
  (if (vecr-p offset)
      (let ((new-position (add (pos p) offset)))
	(move p new-position))
      (error "[particle:shift] :: Trying to shift a particle by a non-vector object")))

(defun make-atom (name &key (position (vecr)) (id nil) (molecule nil))
  (make-instance '%atom :name name :pos position :id id :molecule molecule))
		 
(defmethod print-object ((a %atom) stream)
  (print-unreadable-object (a stream :type t)
    (with-slots (pos name id) a
      (format stream "~a[~d] ~a" name id pos))))

(defmethod initialize-instance :after ((a %atom) &key)
  (with-slots (mass charge name) a
    (setf (slot-value a 'charge) (cadr (assoc name *atomic-charges*)))
    (setf (slot-value a 'mass) (cadr (assoc name *atomic-masses*)))))

(defmethod distance-vector ((a %atom) (b %atom))
  (minimum-distance-vector (pos a) (pos b) *system-size*))

(defmethod distance ((a %atom) (b %atom))
  (minimum-distance (pos a) (pos b) *system-size*))