(defpackage :mdcrd
  (:use common-lisp prmtop vectors))

(in-package :mdcrd)
(require 'asdf)
(push "./" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :prmtop)

(defun load-mdcrd (mdcrd-path prmtop-path)
  (let ((prmtop-data ('prmtop::load-prmtop prmtop-path))
		(mdcrd-data '())
		(n-atoms ('prmtop::n-atoms prmtop-data)))
	(print n-atoms)))

