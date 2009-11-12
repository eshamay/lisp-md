(in-package #:md)

(defvar *h-bond-angle* 0.866025)

(defvar *bond-lengths*
  '(
    ((o h) . 1.2)
    ((n o) . 2.0)
    ((n h) . 1.3)
    ((si o) . 1.56)))

(defvar *h-bonding-atoms*
  '(
    ((o h) . 2.46)))

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

(defun set-system-size (x y z)
  "Allows us to set the system size to enable calculations involving periodic boundaries (on orthorhombic systems)"
  (setf *system-size* (vecr x y z)))

(defvar *system-size* nil)
