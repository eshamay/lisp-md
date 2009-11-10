;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem #:md
  :depends-on (#:cl-graph)
  :serial t
  :components ((:file "package")
	       (:file "vectors")
  	       (:file "particle")
	       (:file "molecule")
	       (:file "coordinate-file")
	       (:file "xyz-file")))

