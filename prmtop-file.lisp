(in-package #:md)

#|
Standard prmtop information

**POINTERS**
FORMAT(20a4)  (ITITL(i), i=1,20)
  ITITL  : title

FORMAT(12i6)  NATOM,  NTYPES, NBONH,  MBONA,  NTHETH, MTHETA,
              NPHIH,  MPHIA,  NHPARM, NPARM,  NEXT,   NRES,
              NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,
              NATYP,  NPHB,   IFPERT, NBPER,  NGPER,  NDPER,
              MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
00  NATOM  : total number of atoms 
01  NTYPES : total number of distinct atom types
02  NBONH  : number of bonds containing hydrogen
03  MBONA  : number of bonds not containing hydrogen
04  NTHETH : number of angles containing hydrogen
05  MTHETA : number of angles not containing hydrogen
06  NPHIH  : number of dihedrals containing hydrogen
07  MPHIA  : number of dihedrals not containing hydrogen
08  NHPARM : currently not used
09  NPARM  : currently not used
10  NEXT   : number of excluded atoms
11  NRES   : number of residues
12  NBONA  : MBONA + number of constraint bonds
13  NTHETA : MTHETA + number of constraint angles
14  NPHIA  : MPHIA + number of constraint dihedrals
15  NUMBND : number of unique bond types
16  NUMANG : number of unique angle types
17  NPTRA  : number of unique dihedral types
18  NATYP  : number of atom types in parameter file, see SOLTY below
19  NPHB   : number of distinct 10-12 hydrogen bond pair types
20  IFPERT : set to 1 if perturbation info is to be read in
21  NBPER  : number of bonds to be perturbed
22  NGPER  : number of angles to be perturbed
23  NDPER  : number of dihedrals to be perturbed
24  MBPER  : number of bonds with atoms completely in perturbed group
25  MGPER  : number of angles with atoms completely in perturbed group
26  MDPER  : number of dihedrals with atoms completely in perturbed groups
27  IFBOX  : set to 1 if standard periodic box, 2 when truncated octahedral
28  NMXRS  : number of atoms in the largest residue
29  IFCAP  : set to 1 if the CAP option from edit was specified
|#

(defstruct prmtop-data
  atom-count molecule-count
  atoms molecules)

(defun prmtop-flagp (str)
  "Checks if a given string contains a flag"
  (search "FLAG" str))

(defun prmtop-formatp (str)
  "Checks if a given string contains a format"
  (search "FORMAT" str))

(defun parse-prmtop-flag (str)
  "Takes a string known to contain a flag name and parses out the flag"
  (subseq str (+ 1 (mismatch "%FLAG" str))))

(defun load-prmtop-file (filename)
  "Parses the data from a prmtop file and returns an alist:
          key = %FLAG values
          value = data that follows"
  (with-open-file (f filename
		     :direction :input)
      (read-line f nil nil)		; trim off the first header line and skip the header flag
      (let ((alist ())			; This holds the parsed flag/data from the prmtop file
	    (data ()))			; While this is used to build up individual entries
	(loop for line = (trim-string (read-line f nil nil))
	   while line do
	     (cond			; 3 possible tasks - parse a flag, a format, or data
	       ;; Check which task to perform, and build the data
	       ((prmtop-flagp line)		; Parsing flags
		(setf alist (acons (car data) (cdr data) alist)) ; Push any previous data into the alist
		(setf data (list (parse-prmtop-flag line)))) ;... and then begin adding data to the new flag
	       ((prmtop-formatp line) nil)	; Skipping formatting strings
	       ((not (eql data nil))	; Parsing data to be paired with the flag
		(nconc data (string-to-list line)))))
	(setf alist (acons (car data) (cdr data) alist)) ; one last push to grab the last bit of data
	alist)))

(defun get-prmtop-data (flag prmtop-file-data)
  "Returns the data matching the given prmtop flag"
  (if (stringp flag)
      (cdr (assoc flag prmtop-file-data :test #'string=))))

(defun prmtop-make-obj-list (name-lst id fn)
  (if (null name-lst)
      nil
      (cons (funcall fn (car name-lst) :id id)
	    (make-obj-list (cdr name-lst) (+ 1 id) fn))))

(defun create-prmtop-list (prmtop-file-data prmtop-flag make-obj-fn)
  "Creates a list of obj instances that have their names and id's set from the prmtop-file"
  (let ((names (get-prmtop-data prmtop-flag prmtop-file-data))) ; The list of names for creating the atoms
    (prmtop-make-obj-list names 0 make-obj-fn)))

(defmacro with-prmtop-file ((var filename) &rest body)
  "A wrapper for letting us work easily with a prmtop file"
  `(let* ((prmtop-file-data (load-prmtop-file ,filename))
	 (,var (make-prmtop-data	; This creates the handy struct that will from here on out hold all the needed data for the system
		:atom-count (nth 0 (get-prmtop-data "POINTERS" prmtop-file-data))
		:molecule-count (nth 11 (get-prmtop-data "POINTERS" prmtop-file-data))
		:atoms (create-prmtop-list prmtop-file-data "ATOM_NAME" #'make-atom)
		:molecules (create-prmtop-list prmtop-file-data "RESIDUE_LABEL" #'make-molecule))))
     ,@body))
