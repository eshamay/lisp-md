(in-package #:md)

(defclass particle ()
  ((pos :accessor pos :initarg :pos :initform (vecr))
   (name :accessor name :initarg :name)
   (id :accessor id :initarg :id :initform nil)
   (mass :accessor mass :initarg :mass :initform 0.0)
   (charge :accessor charge :initarg :charge :initform 0.0)))

(defclass %atom (particle)
   ((name :initarg :name
	  :initform (error "(%atom) :: Every atom in the system must have a name!"))
    (molecule :accessor molecule :initarg :molecule :initform nil)))

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

(defmethod move ((p particle) (position vector))
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

(defmethod atom-list-lookup ((a %atom) (b %atom) alist)
  "Takes a specialized alist - each car is a list of two atoms, each cdr is the key (i.e. bond-length) and returns the cdr - and returns the value for the pair-list given"
  (cdr (assoc '((name a) (name b)) alist :test #'equal)))
	       
(defmethod h-bonding-lookup ((a %atom) (b %atom))
  (atom-list-lookup a b *h-bonding-atoms*))

(defmethod bond-length-lookup ((a %atom) (b %atom))
  (atom-list-lookup a b *bond-lengths*))


(defmethod bonded-p ((a %atom) (b %atom))
  "Returns or false if the two atoms are bound based on the above bond-length criteria"
  (let ((distance (distance a b))
	(max-length (try-both-ways 'bond-length-lookup a b)))
    (if (and
	 max-length
	 (< distance max-length))
	distance
	nil)))

(defun atom-graph (atoms)
  "Creates a graph to represent interconnections between atoms in the system given a list of the %atoms"
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-class 'cl-graph:weighted-edge)))
    (loop-on-pairs a b atoms		; check the distance between each pair of atoms in the system
	 (let ((bond-length (bonded-p a b)))
	   (if (not (null bond-length))	; only add a graph edge if the two atoms are bound based on the bond-length criteria
	       (cl-graph:add-edge-between-vertexes graph a b :value bond-length :edge-type :undirected))))
    graph))
