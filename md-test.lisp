(in-package #:md)

(set-system-size 30.0 30.0 30.0)
(defvar *atoms*)
(defvar edges)
(with-xyz-file "beta-cristobalite-unitcell.xyz"
  (setf *atoms* (atoms xyz))
  (setf edges (cl-graph:edge-count (atom-graph *atoms*))))
; some test atoms to work with
(defvar a (nth 0 *atoms*))		   
(defvar b (nth 1 *atoms*))
(defvar c (nth 2 *atoms*))

