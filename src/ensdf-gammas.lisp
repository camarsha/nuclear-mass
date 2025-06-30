;; this is a side project to pull every gamma ray transition know in ensdf and insert
;; it into a database

(in-package #:nuclear-mass)

(defclass gamma-record ()
  ((energy :accessor energy :initarg :energy)
   (energy-unc :accessor energy-unc :initarg :energy-unc)
   (intensity :accessor intensity :initarg :intensity)
   (intensity-unc :accessor intensity-unc :initarg :intensity-unc)
   (multi :accessor multi :initarg :multi)))

(defmacro destructure-ensdf-line (ensdf-line elements &body body)
  "Destructure an ensdf record according to the definition in the manual."
  `(let* ,(loop :for ele :in elements
		:collect `(,(car ele)
			   (str:trim
			    (str:substring
			     ,(- (nth 1 ele) 1)
			     ,(nth 2 ele)
			     ,ensdf-line))))
     ,@body))


(defun make-gamma (ensdf-line)
  (destructure-ensdf-line ensdf-line
      ((energy 10 19)
       (unc 20 21)
       (intensity 22 29)
       (d-intensity 30 31)
       (multi 32 41))
    (let ((e (try-parse-energy energy))
	  (i (try-parse-energy intensity)))
      (make-instance 'gamma-record
		     :energy e
		     :energy-unc (when e
				   (try-parse-unc energy unc))
		     :intensity i
		     :intensity-unc (when i
				      (try-parse-unc intensity d-intensity))
		     :multi multi))))

(defun get-levels-and-gammas (ensdf-entry)
  (let ((result (make-hash-table :test 'equal))
	(current nil))
    (flet ((add-level (key line)
	     (push (cons '() (make-level key line))
		   (sera:@ result key)))
	   (add-gamma (key line)
	     (push (make-gamma line)
		   (car (car (sera:@ result key))))))
      (loop-over-data-sets ensdf-entry
	(when (search "ADOPTED LEVELS, GAMMAS" (first data-set))
	  (setf current (arrows:->> (first data-set)
				    (str:substring 0 5)
				    (str:trim)
				    (translate-nucleus-name)
				    (apply #'str:concat))
		(sera:@ result current) '())
	  ;; Now we associate a level with all of the gammas
	  (loop :for line :in (cdr data-set)
		:do (cond ((and (char= (aref line 7) #\L)
				(char= (aref line 6) #\Space)
				(char= (aref line 5) #\Space))
			   (add-level current line))
			  ((and (char= (aref line 7) #\G)
				(char= (aref line 6) #\Space)
				(char= (aref line 5) #\Space))
			   (add-gamma current line))))
	  (setf (sera:@ result current)
		(nreverse (sera:@ result current))))))
    result))

(defun records-to-row (hash-table)
  (let ((result '()))
    (sera:do-hash-table (key list hash-table)
      (declare (ignorable key))
      (dolist (ele list)
	(when (car ele)
	  (dolist (gamma (car ele))
	    (when (energy gamma) 
	      (push (list (energy gamma) (energy-unc gamma)
			  (intensity gamma)
			  (intensity-unc gamma)
			  (multi gamma)
			  (name (cdr ele))
			  (parsed-energy (cdr ele))
			  (spin (cdr ele)))
		    result))))))
    (reverse result)))

(defun write-row (stream line)
  (format stream "~{~A~^|~}~%"
	  (mapcar (lambda (ele)
		    (if ele
			ele
			""))
		  line)))

(defun get-all-gammas (file-name)
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
    (format stream "energy|denergy|intensity|dintensity|multi|nucleus|Ex|Jpi~%")
    (zippy:with-zip-file (zip *ensdf-archive*)
      (loop :for i from 1 :to 300 :do
	(print i)
	(arrows:->> (get-ensdf-mass-file zip i)
		    (get-levels-and-gammas)
		    (records-to-row)
		    (mapcar (lambda (ele)
			      (write-row stream ele))))))))
