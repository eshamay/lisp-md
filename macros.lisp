(in-package #:md)

(defmacro define-generic-slot-setter (fn-name input)
  `(defgeneric (setf ,fn-name) (obj ,input)
     (:documentation "A function to set the slot with the input value")))

(defmacro define-generic-slot-getter (name)
  `(defgeneric ,name (obj)
     (:documentation "Returns the value of the slot")))

(defmacro specialized-method-getter (obj-type fn-name slot-name)
  `(defmethod ,fn-name ((o ,obj-type))
     (slot-value o ',slot-name)))

(defmacro specialized-method-setter (obj-type fn-name slot-name)
  `(defmethod (setf ,fn-name) ((o ,obj-type) input)
     (setf (slot-value o ',slot-name) input)))

(defun string-to-list (str)
  "Given a string of numbers, the return is a list of those numbers"
  (with-input-from-string (s str)
	(loop for num = (read s nil nil)
		  while num collect num)))

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun trim-string (str)
  "Remove all whitespace from around words in a string"
  (if str (string-trim " " str)))

(defun grab-n-from-stream (n stream)
  "returns a list of the first n elements from the stream"
  (loop repeat n collect (read stream nil nil)))

(defun grab-lists-from-stream (n m stream)
  "returns n lists, each with m elements parsed from the stream"
  (loop repeat n collect
       (grab-n-from-stream m stream)))

(defmacro loop-on-pairs (id1 id2 lst &body body)
  "loops over all pairs of items in a list using the names specified by the two ids"
  `(loop for (,id1 . rest) on ,lst do
	(loop for ,id2 in rest
	   do ,@body)))

(defun try-both-ways (fn arg1 arg2)
  "Takes a function and two arguments. If running the function with the arguments in the order as given doesn't work (results nil) then it tries them reversed."
  (let ((result (funcall fn arg1 arg2)))
    (if (not (null result))
	result
	(funcall fn arg2 arg1))))

(defun list-deep-length (lst)
  (if (null lst)
      0
      (cond
	((listp (car lst))
	 (+ (list-deep-length (car lst)) (list-deep-length (cdr lst))))
	((atom (car lst))
	 (+ 1 (list-deep-length (cdr lst)))))))