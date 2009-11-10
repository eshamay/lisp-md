(in-package #:md)

;(defpackage #:vectors 			
;  (:use #:cl)
;  (:export #:vecr
;	   #:x #:y #:z
;	   #:add #:sub #:minimum-distance-vector #:minimum-distance
;	   #:dot #:cross #:angle
;	   #:mag #:unit #:scale #:vecr-p))



(defun make-vector (size &key (contents nil))
  (cond
	((null contents) (make-array size))
	((atom contents) (make-array size :initial-element contents))
	((listp contents) 
	 (if (eq (length contents) size)
	   (make-array size :initial-contents contents)
	   (format t "MAKE-VECTOR~%Size of vector specified is ~a~%Initial contents, however, is ~a, which has a size of ~a~%" size contents (length contents))))))

(defun vecr (&optional (x 0.0) (y x) (z y))
  (make-vector 3 :contents (list x y z)))

(defmacro vector-element-getter (name index)
  "creates readers for each element of the vector by coordinate-name (i.e. x, y, z corresponds to element 0,1,2)"
  `(defmethod ,name ((v vector))
     (aref v ,index)))
(vector-element-getter x 0)
(vector-element-getter y 1)
(vector-element-getter z 2)
		      
(defmacro vector-element-setter (name index)
  "Creates writers for the vectors elements"
  `(defmethod (setf ,name) ((v vector) input)
     (setf (aref v ,index) input)))
(vector-element-setter x 0)
(vector-element-setter y 1)
(vector-element-setter z 2)
  
(defun vecr-p (v)
  "Test if a vector is a valid vecr"
  (and
   (vectorp v)
   (eq (array-rank v) 1)
   (eq (array-dimension v 0) 3)))

(defmacro if-vecr (vecs &body body)
  `(if (not (and ,@(loop for v in vecs collect `(vecr-p ,v))))
       (format t "if-vecr::Input vectors are not proper vecr form~%")
       ,@body))

(defmethod add ((v1 vector) (v2 vector))
  "vector addition"
  (if-vecr (v1 v2)
    (map 'vector (lambda (x y) (+ x y)) v1 v2)))

(defmethod sub ((v1 vector) (v2 vector))
  "vector subtraction"
  (if-vecr (v1 v2)
    (map 'vector (lambda (x y) (- x y)) v1 v2)))

(defmethod scale ((v vector) (scale float))
  "Scale a vector by a given value"
  (if-vecr (v)
    (map 'vector (lambda (x) (* x scale)) v)))

(defmethod dot ((v1 vector) (v2 vector))
  "Inner product of two vectors"
  (if-vecr (v1 v2)
  (reduce #'+ (map 'vector (lambda (x y) (* x y)) v1 v2))))

(defmethod mag ((v vector))
  "Vector magnitude (length)"
  (if-vecr (v)
  (sqrt (reduce #'+ (map 'vector (lambda (x) (* x x)) v)))))

(defmethod unit ((v vector))
  "A unit vector in the same direction as the input"
  (if-vecr (v)
  (let ((mag (mag v)))
	(scale v (/ 1.0 mag)))))

(defmethod cross ((v1 vector) (v2 vector))
  "Outter/Cross-product of two vecrs"
  (if-vecr (v1 v2)
	   (vecr
		  (- (* (y v1) (z v2)) (* (z v1) (y v2)))
		  (- (* (z v1) (x v2)) (* (x v1) (z v2)))
		  (- (* (x v1) (y v2)) (* (y v1) (x v2))))))
  
(defmethod angle ((v1 vector) (v2 vector))
  "Angle cosine between two vectors"
  (if-vecr (v1 v2)
	   (/ (dot v1 v2) (mag v1) (mag v2))))

(defun check-vectors (&rest body)
  (dolist (i body)
    (if (null i)
	(error "Set the system-size before trying to call various vector routines on the system!"))))

(defmacro minimum-vector-element (element v system-size)
  "Returns a single element of the minimum-image vector v, if the system-size is supplied (as a vector)"
  `(- (,element ,v)
      (* (fround (/ (,element ,v) (,element ,system-size)))
	 (,element ,system-size))))

(defmethod minimum-distance-vector ((v1 vector) (v2 vector) (system-size vector))
  (check-vectors v1 v2 system-size)
  "Calculates the minimum-image distance between two vectors in a periodic system. The result is a vector pointing from v1 to v2"
  (let* ((dv (sub v2 v1))
	 (x (minimum-vector-element x dv system-size))
	 (y (minimum-vector-element y dv system-size))
	 (z (minimum-vector-element z dv system-size)))
    (vecr x y z)))

(defmethod minimum-distance ((v1 vector) (v2 vector) (system-size vector))
  (check-vectors v1 v2 system-size)
  (mag (minimum-distance-vector v1 v2 system-size)))