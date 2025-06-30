(in-package #:nuclear-mass)

(defun make-mass-table-hash (list)
  "Make a hash table from the list of lists"
  (declare (optimize (debug 3)))
  (let ((result (make-hash-table :test 'equal)))
    ;; loop and add info to hash
    (loop :for line :in list
	  :for element-str = (nth 4 line)
	  :for mass-number = (nth 3 line)
	  :for key = (arrows:->> (str:concat mass-number element-str)
				 (translate-nucleus-name)
				 (apply #'str:concat))
	  :for value = (make-array 7 :initial-contents
				   (list (parse-integer (nth 2 line))
					 (parse-integer (nth 3 line))
					 (nth 4 line)
					 (* 1e-6
					    (parse-number:parse-number
					     (nth 12 line)))
					 (* 1e-6
					    (parse-number:parse-number
					     (nth 13 line)))
					 (parse-number:parse-number
					  (nth 6 line))
					 (parse-number:parse-number
					  (nth 7 line))))
	  :do
	     (setf (sera:@ result key) value))
    result))

(defun load-mass-table (mass-table-file-path)
  (arrows:->> (alexandria:read-file-into-string mass-table-file-path)
	      (cl-ppcre:split "\\n")
	      (mapcar (lambda (line)
			(cl-ppcre:split "," line)))
	      (cdr)
	      (make-mass-table-hash)))


(defparameter *mass-table-path*
  (package-path "./mass-eval/" "mass20.csv")
  "Path to the mass csv file.")

(defparameter *mass-table* (load-mass-table *mass-table-path*)
  "Hash table of the mass csv file.")



(defmacro define-mass-table (name mass-table-file-path)
  (format t "~& Defining mass table ~A~%" name)
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
;; These is broken at the moment
;; (define-mass-table mass95 "mass95.csv")
;; (define-mass-table mass93 "mass93.csv")

(defun atomic-number-to-mass-name ()
  (let ((result (make-array 120)))
    (loop :for val :in (alexandria:hash-table-values *mass-table*)
	  :for number = (aref val 0)
	  :for name = (aref val 2)
	  :do
	     (setf (aref result number) name))
    result))

(defparameter *atomic-number-to-mass-name*
  (atomic-number-to-mass-name)
  "Hash table that stores integer value relationship to element names")

