(in-package #:md)

(defclass coordinate-file ()
  ((file :accessor file :initarg :file) ; The stream to the contents of the coordinate file
   (frame :accessor frame)
   (num-atoms :initform 0 :accessor num-atoms)))

(defgeneric load-coordinate-file (crd-file)
  (:documentation "Initializes the coordinate file by opening it up and loading the first frame"))

(defgeneric read-header (crd-file)
  (:documentation "Reads any header information at the top of the file, and between frames if necessary"))

(defgeneric parse-coordinate (crd-file)
  (:documentation "Parses a single coordinate from the coordinate file"))

(defgeneric load-next-frame (crd-file)
  (:documentation "Parses an entire frame and returns all the coordinates of that frame"))



(defmethod num-atoms ((crd coordinate-file))
  "returns the number of atoms parsed by the coordinate file in each frame"
  (slot-value crd 'num-atoms))

(defmethod get-frame ((crd coordinate-file))
  "Returns the coordinate set from the current frame"
  (slot-value crd 'frame))

(defmethod load-coordinate-file ((crd coordinate-file))
  "First the file is opened using the filename provided at the make-instance, then the first frame is parsed"
  (with-slots (file num-atoms) crd
    (load-next-frame crd)))

(defmacro with-coordinate-file (filename file-type accessor &rest body)
  "A useful way to load up a coordinate file and have an accessor to it"
  `(with-open-file (f-stream ,filename)
     (let ((,accessor (make-instance ,file-type :file f-stream)))
       (load-coordinate-file ,accessor)
       ,@body)))