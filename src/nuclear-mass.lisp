;;;; nuclear-mass.lisp

(in-package #:nuclear-mass)


(defparameter *mass-table-path*
  (package-path "./mass-eval/" "mass20.csv")
  "Path to the mass csv file.")

(defparameter *mass-table* (mapcar (lambda (line)
				     (cl-ppcre:split "," line))
				   (cl-ppcre:split
				    "\\n"
				    (alexandria:read-file-into-string *mass-table-path*)))
  "List of lists of the mass csv file.")

(defvar +electron-mass+ 510.99895000
  "Mass of electron in keV")

(defvar +atomic-units->mev+ 931.49410242
  "Number of MeV in 1u.")

(defun load-mass-table (mass-table-file-path)
  (arrows:->> (alexandria:read-file-into-string mass-table-file-path)
	      (cl-ppcre:split "\\n")
	      (mapcar (lambda (line)
			(cl-ppcre:split "," line)))))

(defmacro define-mass-table (name mass-table-file-path)
  `(defparameter ,name
     (load-mass-table
      (package-path "./mass-eval/" ,mass-table-file-path))))


(defmacro with-mass-table (mass-table &body body)
  (if (consp mass-table)
      `(list ,@(loop :for mt :in mass-table
		     :collect `(let ((*mass-table* ,mt))
				 ,@body)))
      `(let ((*mass-table* ,mass-table))
	 ,@body)))

(define-mass-table mass20 "mass20.csv")
(define-mass-table mass16 "mass16.csv")
(define-mass-table mass12 "mass12.csv")
(define-mass-table mass03 "mass03.csv")
(define-mass-table mass95 "mass95.csv")
(define-mass-table mass93 "mass93.csv")


(declaim (inline atomic-units->mev))
(defun atomic-units->mev (u)
  (* u +atomic-units->mev+))

(declaim (inline atomic-units->kev))
(defun atomic-units->kev (u)
  (* 1000.0 (* u +atomic-units->mev+)))

(declaim (inline mev->atomic-units))
(defun mev->atomic-units (mev)
  (/ mev +atomic-units->mev+))

(defun get-nuclei-info (element number)
  (let ((element-index
	  (etypecase element
	    (number 2)
	    (string 4))))
    (loop with element-str = (if (numberp element)
				 (write-to-string element)
				 element)
	  for line in *mass-table*
	  do
	     (when (and (string= (nth 3 line) number)
			(string= (nth element-index line) element-str))
	       (return line)))))

(defun electron-binding-energy (charge-number)
  "Binding energy in keV. Adapted from Eq.A4 of Lunney et. al. 2003."
  (/ (+ (* 14.4381 (expt charge-number 2.39))
	(* 1.55468e-6 (expt charge-number 5.35)))
     1000.0))

(defun atomic-mass->nuclear-mass (atomic-mass charge-number)
  (let ((binding-energy (electron-binding-energy charge-number))
	(total-electron-mass (* +electron-mass+ charge-number))
	(mass-kev (* (atomic-units->mev (value atomic-mass)) 1000.0)))
    (make-instance 'measurement
		   :value (mev->atomic-units(/ (+ (- mass-kev total-electron-mass)
						  binding-energy)
					       1000.0))
		   :uncertainty (uncertainty atomic-mass))))

(defclass nucleus ()
  ((mass-number :accessor mass-number :initarg :mass-number)
   (charge-number :accessor charge-number :initarg :charge-number)
   (name :accessor name :initarg :name)
   (atomic-mass :accessor atomic-mass :initarg :atomic-mass)
   (nuclear-mass :accessor nuclear-mass :initarg :nuclear-mass)))

(defclass measurement () 
  ((value :initarg :value :accessor value)
   (uncertainty :initarg :uncertainty :accessor uncertainty)))

(defmethod initialize-instance :after ((nuc nucleus) &rest initargs)
  (declare (ignorable initargs))
  (setf (slot-value nuc 'nuclear-mass)
	(atomic-mass->nuclear-mass (slot-value nuc 'atomic-mass)
				   (slot-value nuc 'charge-number))))

(defmethod print-object ((nuc nucleus) out)
  (format out "Name: ~A~%A = ~A~%Z = ~A~%Atomic Mass = ~A~%Nuclear Mass = ~A~% "
	  (name nuc)
	  (mass-number nuc)
	  (charge-number nuc)
	  (value (atomic-mass nuc))
	  (value (nuclear-mass nuc))))

(defun translate-nucleus-name (string)
  "Everyone will want to write a nucleus differently,
this function normalizes them all for the rest of the package.
na 23, 23Na, 23na, 23NA, 23  na, should all give you 23Na"
  (when (member string (list "1n" "n" "N") :test 'string=)
    (return-from translate-nucleus-name (list "n" "1")))
  (when (string= string "p")
    (return-from translate-nucleus-name (list "H" "1")))
  (when (string= string "a")
    (return-from translate-nucleus-name (list "He" "4")))
  (list (string-capitalize ;;name
	 (first
	  (cl-ppcre:all-matches-as-strings "[a-zA-Z]+" string)))
	(first ;; number
	 (cl-ppcre:all-matches-as-strings "\\d+" string))))

(defun make-photon ()
  (make-instance 'nucleus
		 :charge-number 0
		 :mass-number 0
		 :name "g"
		 :atomic-mass (make-instance 'measurement :value 0d0 :uncertainty 0d0)))

(defun make-nucleus (nucleus-name)
  "Creates a nucleus object from a string containing the atomic mass number and element name."
  (declare (optimize (debug 3)))
  (when (or (string= nucleus-name "g")
	    (string= nucleus-name "G"))
    (return-from make-nucleus (make-photon)))
  (destructuring-bind (name number)
      (translate-nucleus-name nucleus-name)
    (let ((info (get-nuclei-info name number)))
      (make-instance 'nucleus
		     :charge-number (parse-integer (nth 2 info))
		     :mass-number (parse-integer (nth 3 info))
		     :name (nth 4 info)
		     :atomic-mass
		     (make-instance 'measurement
				    :value (* 1e-6
					      (parse-number:parse-number
					       (nth 12 info)))
				    :uncertainty (* 1e-6
						    (parse-number:parse-number
						     (nth 13 info))))))))

(defun make-nucleus-from-atomic-number (atomic-mass-number atomic-number)
  "Creates a nucleus object from a mass and atomic number."
  (let ((info (get-nuclei-info atomic-number (write-to-string atomic-mass-number))))
    (make-instance 'nucleus
		   :charge-number (parse-integer (nth 2 info))
		   :mass-number (parse-integer (nth 3 info))
		   :name (nth 4 info)
		   :atomic-mass
		   (make-instance 'measurement
				  :value (* 1e-6
					    (parse-number:parse-number
					     (nth 12 info)))
				  :uncertainty (* 1e-6
						  (parse-number:parse-number
						   (nth 13 info)))))))


(defun get-atomic-mass (nucleus-name)
  "If you just want an atomic mass and uncertainty."
  (let ((mass (atomic-mass (make-nucleus nucleus-name))))
    (values (value mass)
	    (uncertainty mass))))

(defun get-nuclear-mass (nucleus-name)
  "If you just want a nuclear mass and uncertainty."
  (let ((mass (nuclear-mass (make-nucleus nucleus-name))))
    (values (value mass)
	    (uncertainty mass))))

(defun q-value (nuclei-in nuclei-out &key (mass-function #'get-nuclear-mass))
  "Calculate the q-value in keV according to a :mass-function. Default is nuclear mass."
  (let ((initial 0d0)
	(final 0d0)
	(uncertainty 0d0))
    ;; first do the initial masses
    (loop for (mass unc) in (mapcar (lambda (nuc)
				      (multiple-value-list
				       (funcall mass-function nuc)))
				    nuclei-in)
	  do
	     (incf initial mass)
	     (incf uncertainty (expt unc 2)))
    ;; next the final
    (loop for (mass unc) in (mapcar (lambda (nuc)
				      (multiple-value-list
				       (funcall mass-function nuc)))
				    nuclei-out)
	  do
	     (incf final mass)
	     (incf uncertainty (expt unc 2)))
    ;; convert to keV
    (values (* 1000.0 (atomic-units->mev (- initial final)))
	    (* 1000.0 (atomic-units->mev (sqrt uncertainty))))))

(defun de-broglie-wavelength (projectile target energy &key (mass-function #'get-nuclear-mass))
  "Calculate the center of mass de-Broglie wavelength from a laboratory resonance energy (keV).
See Iliadis Eq. 4.107"
  (let ((m-0 (funcall mass-function projectile))
	(m-1 (funcall mass-function target)))
    (* (/ (+ m-0 m-1) m-1)
       (sqrt
	(/ (* 4.125d-18 2.0)
	   (* m-0 (* 1000.0 energy)))))))

(defun reduced-mass (projectile target &key (mass-function #'get-nuclear-mass))
  (let ((m-0 (funcall mass-function projectile))
	(m-1 (funcall mass-function target)))
    (/ (* m-0 m-1) (+ m-0 m-1))))


(defun nuclear-radius (r0 projectile target &key (mass-function #'get-nuclear-mass))
  (let ((m-0 (funcall mass-function projectile))
	(m-1 (funcall mass-function target)))
    (* r0 (+ (expt m-0 1/3)
	     (expt m-1 1/3)))))


(defmacro with-nuclear-masses (mass-list &body body)
  `(prog1
       (let ,(loop for mass in mass-list
		   collect `(,mass (get-nuclear-mass ,(string mass))))
	 ,@body)
     ;; If the symbol did not have a value, unintern it
     (mapc #'unintern
	   ',(remove-if #'boundp mass-list))))

(defmacro with-atomic-masses (mass-list &body body)
  `(prog1
       (let ,(loop for mass in mass-list
		   collect `(,mass (get-atomic-mass ,(string mass))))
	 ,@body)
     ;; If the symbol did not have a value, unintern it
     (mapc #'unintern
	   ',(remove-if #'boundp mass-list))))
