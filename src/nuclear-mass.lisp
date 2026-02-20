;;;; nuclear-mass.lisp
(in-package #:nuclear-mass)

(defparameter *default-mass-function* #'get-atomic-mass)

(declaim (inline atomic-units->mev))
(defun atomic-units->mev (u)
  (* u +atomic-units->mev+))

(declaim (inline atomic-units->kev))
(defun atomic-units->kev (u)
  (* 1000.0 (* u +atomic-units->mev+)))

(declaim (inline mev->atomic-units))
(defun mev->atomic-units (mev)
  (/ mev +atomic-units->mev+))

(defun get-nuclei-info (string)
  "Pull the info from the mass table hash."
  (arrows:->> string
	      translate-nucleus-name
	      (apply #'str:concat)
	      (sera:@ *mass-table*)))

(defun electron-binding-energy (charge-number)
  "Binding energy in keV. Adapted from Eq.A4 of Lunney et. al. 2003."
  (/ (+ (* 14.4381 (expt charge-number 2.39))
	(* 1.55468e-6 (expt charge-number 5.35)))
     1000.0))

(defun atomic-mass->nuclear-mass (atomic-mass charge-number)
  "Converts an atomic mass to nuclear mass. ATOMIC-MASS is
an instance of the MEASUREMENT class."
  (let ((binding-energy (electron-binding-energy charge-number))
	(total-electron-mass (* +electron-mass+ charge-number))
	(mass-kev (atomic-units->kev (value atomic-mass))))
    (make-instance 'measurement
		   :value (mev->atomic-units (/ (+ (- mass-kev total-electron-mass)
						   binding-energy)
						1000.0))
		   :uncertainty (uncertainty atomic-mass))))

(defclass nucleus ()
  ((mass-number :accessor mass-number :initarg :mass-number)
   (charge-number :accessor charge-number :initarg :charge-number)
   (name :accessor name :initarg :name)
   (atomic-mass :accessor atomic-mass :initarg :atomic-mass)
   (nuclear-mass :accessor nuclear-mass :initarg :nuclear-mass)
   (mass-excess :accessor mass-excess :initarg :mass-excess)
   (nuclear-mass-excess :accessor nuclear-mass-excess :initarg :nuclear-mass-excess)))

(defclass measurement () 
  ((value :initarg :value :accessor value)
   (uncertainty :initarg :uncertainty :accessor uncertainty)))

(defmethod initialize-instance :after ((nuc nucleus) &rest initargs)
  (declare (ignorable initargs))
  (when (string= (slot-value nuc 'name) "g")
    (setf (slot-value nuc 'nuclear-mass) 0.0)
    (return-from initialize-instance
      (setf (slot-value nuc 'nuclear-mass-excess)
	    (make-instance 'measurement :value 0.0 :uncertainty 0.0))))
  (setf (slot-value nuc 'nuclear-mass)
	(atomic-mass->nuclear-mass (slot-value nuc 'atomic-mass)
				   (slot-value nuc 'charge-number))
	;; nuclear mass excess calculation
	(slot-value nuc 'nuclear-mass-excess)
	(make-instance 'measurement
		       :value (+ (- (value (slot-value nuc 'mass-excess))
				    (* +electron-mass+ (slot-value nuc 'charge-number)))
				 (electron-binding-energy (slot-value nuc 'charge-number)))
		       :uncertainty (uncertainty (slot-value nuc 'mass-excess)))))

(defmethod print-object ((nuc nucleus) out)
  (print-unreadable-object (nuc out :type t)
    (format out "{Name: ~A | A = ~A | Z = ~A | Atomic Mass = ~,3F | Nuclear Mass = ~,3F}"
	    (name nuc)
	    (mass-number nuc)
	    (charge-number nuc)
	    (value (atomic-mass nuc))
	    (value (nuclear-mass nuc)))))

(defmethod print-object ((obj measurement) out)
  (print-unreadable-object (obj out :type t)
    (format out "{VALUE: ~,3F | UNC: ~,3E}" (value obj) (uncertainty obj))))

(defun make-photon ()
  (make-instance 'nucleus
		 :charge-number 0
		 :mass-number 0
		 :name "g"
		 :atomic-mass (make-instance 'measurement :value 0d0 :uncertainty 0d0)))

(defun make-nucleus (nucleus-name)
  "Creates a nucleus object from a string containing the atomic mass number and element name."
  (when (or (string= nucleus-name "g")
	    (string= nucleus-name "G"))
    (return-from make-nucleus (make-photon)))
  (let ((info (get-nuclei-info nucleus-name)))
    (make-instance 'nucleus
		   :charge-number (aref info 0)
		   :mass-number (aref info 1)
		   :name (aref info 2)
		   :atomic-mass
		   (make-instance 'measurement
				  :value (aref info 3)
				  :uncertainty (aref info 4))
		   :mass-excess
		   (make-instance 'measurement
				  :value (aref info 5)
				  :uncertainty (aref info 6)))))

(defun make-nucleus-from-atomic-number (atomic-mass-number atomic-number)
  "Creates a nucleus object from a mass and atomic number."
  (arrows:->> atomic-number
	      (aref *atomic-number-to-mass-name*)
	      (format nil "~A~A" atomic-mass-number)
	      (make-nucleus)))


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

(defun q-value (nuclei-in nuclei-out &key (mass-function *default-mass-function*))
  "Calculate the q-value in keV according to a :mass-function. Default is nuclear mass."
  (let ((initial 0d0)
	(final 0d0)
	(uncertainty 0d0))
    ;; first do the initial masses
    (loop :for (mass unc) :in (mapcar (lambda (nuc)
					(multiple-value-list
					 (funcall mass-function nuc)))
				      nuclei-in)
	  :do
	     (incf initial mass)
	     (incf uncertainty (expt unc 2)))
    ;; next the final
    (loop :for (mass unc) :in (mapcar (lambda (nuc)
					(multiple-value-list
					 (funcall mass-function nuc)))
				      nuclei-out)
	  :do
	     (incf final mass)
	     (incf uncertainty (expt unc 2)))
    ;; convert to keV
    (values (atomic-units->kev (- initial final))
	    (atomic-units->kev (sqrt uncertainty)))))

(defun de-broglie-wavelength (projectile target energy &key (mass-function *default-mass-function*))
  "Calculate the center of mass de-Broglie wavelength from a laboratory resonance energy (keV).
See Iliadis Eq. 4.107"
  (let ((m-0 (funcall mass-function projectile))
	(m-1 (funcall mass-function target)))
    (* (/ (+ m-0 m-1) m-1)
       (sqrt
	(/ (* 4.125d-18 2.0)
	   (* m-0 (* 1000.0 energy)))))))

(defun reduced-mass (projectile target &key (mass-function *default-mass-function*))
  (let ((m-0 (funcall mass-function projectile))
	(m-1 (funcall mass-function target)))
    (/ (* m-0 m-1) (+ m-0 m-1))))


  (defun nuclear-radius (r0 projectile target &key (mass-function *default-mass-function*))
    (let ((m-0 (funcall mass-function projectile))
	  (m-1 (funcall mass-function target)))
      (* r0 (+ (expt m-0 1/3)
	       (expt m-1 1/3)))))

(defun gamow-peak (projectile target temperature)
  "Calculate the gamow peak energy for a given TEMPERATURE in GK."
  (with-slots ((m0 atomic-mass) (z0 charge-number)) (make-nucleus projectile)
    (with-slots ((m1 atomic-mass) (z1 charge-number)) (make-nucleus target)
      (let* ((m0 (value m0))
	     (m1 (value m1))
	     (const (* (sqr z0)
		       (sqr z1)
		       (/ (* m0 m1)
			  (+ m0 m1))))
	     (peak 
	       (* 0.1220 1000.0
		  (expt
		   (* const
		      (sqr temperature))
		   1/3)))
	     (width (* 0.2368 1000.0
		       (expt
			(* const (expt temperature 5))
			1/6))))
	(values peak width)))))


(deftype lifetime-or-half-life ()
  '(member :lifetime :half-life))

(declaim (ftype (function (number &optional lifetime-or-half-life) number) line-width))
(defun line-width (value &optional (lifetime-or-half-life :lifetime))
  "Returns the line width in eV."
  (case lifetime-or-half-life
    (:lifetime (/ +h-bar+ value))
    (:half-life (/ +h-bar+
		   (/ value (log 2))))))

(defun gamow-factor (projectile target energy)
  "Eq. 3.71 and Eq. 3.75 in Iliadis. Energy in MeV."
  (with-slots ((m0 atomic-mass) (z0 charge-number)) (make-nucleus projectile)
    (with-slots ((m1 atomic-mass) (z1 charge-number)) (make-nucleus target)
      (let ((2-pi-eta  (* 0.98951013 z0 z1 (sqrt (* (/ (* (value m0) (value m1))
						       (+ (value m0) (value m1)))
						    (/ 1.0 energy))))))
	(exp (* -1.0 2-pi-eta))))))

(defun s-factor->cross-section (projectile target s-factor energy)
  "Calculate the reaction cross section based on the astrophysical s-factor.
Energy must be MeV in the center-of-mass frame."
  (* (/ 1.0 energy)
     (gamow-factor projectile target energy)
     s-factor))

(defun cross-section->s-factor (projectile target cross-section energy)
  "Calculate the astrophysical s-factor from the cross section.
Energy must be MeV in the center-of-mass frame."
  (* cross-section energy (/ 1.0 (gamow-factor projectile target energy))))

(defun add-nuclei (&rest nuc)
  "Find the compound system from two nuclei."
  (let* ((nuc-list (mapcar #'make-nucleus nuc))
	 (a (reduce #'+ nuc-list :key #'mass-number))
	 (z (reduce #'+ nuc-list :key #'charge-number)))
    (make-nucleus-from-atomic-number a z)))


(defmacro with-nuclear-masses (mass-list &body body)
  `(prog1
       (let ,(append `((*default-mass-function* #'get-nuclear-mass))
		     (loop for mass in mass-list
			   collect `(,mass (get-nuclear-mass ,(string mass)))))
	 ,@body)
     ;; If the symbol did not have a value, unintern it
     (mapc #'unintern
	   ',(remove-if #'boundp mass-list))))

(defmacro with-atomic-masses (mass-list &body body)
  `(prog1
       (let ,(append `((*default-mass-function* #'get-atomic-mass))
		     (loop for mass in mass-list
			   collect `(,mass (get-atomic-mass ,(string mass)))))
	 ,@body)
     ;; If the symbol did not have a value, unintern it
     (mapc #'unintern
	   ',(remove-if #'boundp mass-list))))
