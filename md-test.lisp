(in-package #:md)

(defvar a (make-atom 'o :position (vecr 1.0 1.0 1.0) :id 1))
(defvar b (make-atom 'h :position (vecr 1.5 2.0 1.5 ) :id 2))
(defvar c (make-atom 'h :position (vecr 2.0 1.5 1.6 ) :id 2))

(defmacro loop-on-pairs (id1 id2 lst &body body)
  "loops over all pairs of items in a list using the names specified by the two ids"
  `(loop for (,id1 . rest) on ,lst do
	(loop for ,id2 in rest
	   do ,@body)))

(defmacro print-if (fn-p a b)
  "a stupid test function"
  `(if (funcall ,fn-p ,a ,b)
       (format t "it worked! ~a is ~a ~a~%" ,a ',fn-p ,b)

       (format t "it didn't work... ~a is no ~a ~a~%" ,a ',fn-p ,b)))

(defmacro add-edge-between-vertexes-if (fn-p thing vertex1 vertex2 &body body)
  "Uses the cl-graph:add-edge-between-vertexes, but takes a predicate to test on the vertexes before doing so"
  `(if (funcall ,fn-p ,vertex1 ,vertex2)
       (cl-graph:add-edge-between-vertexes ,thing ,vertex1 ,vertex2)))

(defun atom-graph (atoms)
  "Creates a graph to represent interconnections between atoms in the system given a list of the atoms"
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-class 'cl-graph:weighted-edge)))
    (loop-on-pairs a b atoms		; check the distance between each pair of atoms in the system
	 (let ((bond-length (bonded-p a b)))
	   (if (not (null bond-length))	; only add a graph edge if the two atoms are bound based on the bond-length criteria
	       (cl-graph:add-edge-between-vertexes graph a b :value bond-length :edge-type :undirected))))
    graph))

(defmacro with-xyz-file (filename &rest body)
  `(with-coordinate-file ,filename 'xyz-file xyz
			 ,@body))

(defvar *h-bond-angle* 0.866025)

(defvar *bond-lengths*
  '(
    ((o h) . 1.2)
    ((n o) . 2.0)
    ((n h) . 1.3)
    ((si o) . 1.55)))

(defvar *h-bonding-atoms*
  '(
    ((o h) . 2.46)))

(defmacro atom-list-lookup (a b alist)
  "Takes a specialized alist - each car is a list of two atoms, each cdr is the key (i.e. bond-length) and returns the cdr - and returns the value for the pair-list given"
  `(cdr (assoc `(,(name a) ,(name b)) ,alist :test #'equal)))
	       
(defmethod h-bonding-lookup ((a %atom) (b %atom))
  (atom-list-lookup a b *h-bonding-atoms*))

(defmethod bond-length-lookup ((a %atom) (b %atom))
  (atom-list-lookup a b *bond-lengths*))

(defun try-both-ways (fn arg1 arg2)
  "Takes a function and two arguments. If running the function with the arguments in the order as given doesn't work (results nil) then it tries them reversed."
  (let ((result (funcall fn arg1 arg2)))
    (if (not (null result))
	result
	(funcall fn arg2 arg1))))

(defmethod bonded-p ((a %atom) (b %atom) &key bond-type)
  "Returns or false if the two atoms are bound based on the above bond-length criteria"
  (let ((distance (distance a b))
	(max-length (cond
		      ((eq bond-type 'covalent)
		       (try-both-ways 'h-bond-length-lookup a b))
		      ((eq bond-type 'hbond)
		       (try-both-ways 'bond-length-lookup a b))
		      (t nil))))
    (if (and
	 max-length
	 (< distance max-length))
	distance
	nil)))

(with-xyz-file "beta-cristobalite-unitcell.xyz"
  (set-system-size 30.0 30.0 30.0)
  (print (cl-graph:edge-count (atom-graph (atoms xyz)))))