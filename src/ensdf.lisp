(in-package #:nuclear-mass)

;;; All of these functions handle getting the data from the compressed
;;; ensdf file.

(defvar *ensdf-archive* (handler-case (update-ensdf)
			  (error (c)
			    (format t "~&Failed to check for ensdf update, falling back to default.")
			    (package-path "./ensdf/" "ensdf.zip"))))


(defun slice (array start end)
  "Generate a displaced array for the array from start to end (exclusive)"
  (make-array (- end start) :element-type `(unsigned-byte 8)
			    :displaced-to array :displaced-index-offset start))

(defun mass-number->ensdf-filename (mass-number)
  (format nil "~A.~3,'0d"
	  "ensdf"
	  mass-number))

(defun get-ensdf-mass-file (zip mass-number)
  "Given a mass number, find the ensdf entry in the zip archive."
  (loop with mass-string = (mass-number->ensdf-filename mass-number)
	for entry across (zippy:entries zip)
	when (string= (zippy:file-name entry) mass-string)
	  do (return-from get-ensdf-mass-file entry)))

(defun ensdf-nucid (nucleus-name)
  "Convert a name and number into a properly formatted ensdf string."
  (destructuring-bind (name number)
      (translate-nucleus-name nucleus-name)
    (format nil "~3@a~:@(~2a~)" number name)))

;;; The compressed file will be read into a byte vector. Use the ensdf format to make
;;; further parsiing simple.

(defclass data-set ()
  ((start :accessor start
	  :initarg :start
	  :type fixnum
	  :initform 0)
   (end :accessor end
	:initarg :end
	:type fixnum
	:initform 0)
   (data :accessor data
	 :initarg :data)))

(defun make-data-set (array start end)
  (let* ((data (slice array start end)))
    (make-instance 'data-set :start start
			     :end end
			     :data (loop with start = 0
					 for i from 0 below (length data)
					 if (= (aref data i) 10)
					   collect (map 'string #'code-char (slice data start i))
					   and do (setf start (1+ i))))))

(defmethod print-object ((obj data-set) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A" (data obj))))


(defun %generate-data-sets (ensdf-vector)
  (loop with start = 0
	for i below (length ensdf-vector)
	;; two new lines in a row mean a blank line and the end of one data set
	if (and (= (aref ensdf-vector i) 10)
		(< (1+ i) (length ensdf-vector))
		(= (aref ensdf-vector (1+ i)) 10))
	  collect (make-data-set ensdf-vector start i)
	  and 
	    do 
	       (setf start (+ i 2))))

(defmacro loop-over-data-sets (ensdf-entry &body body)
  "Loops over the whole file and then chunks it into a list that can be looped over.
Captures the variables data-set and data-sets."
  (let ((g-loop (gensym)))
    `(loop with data-sets = (%generate-data-sets (zippy:entry-to-vector ,ensdf-entry))
	   for ,g-loop in data-sets
	   for data-set = (data ,g-loop)
	   do
	   ,@body)))


(defun get-ensdf-level-records (ensdf-entry nucleus-name)
  (loop-over-data-sets ensdf-entry
    (when (and (search "ADOPTED LEVELS" (first data-set))
	       (search (ensdf-nucid nucleus-name) (first data-set)))
      (return-from get-ensdf-level-records
	(remove-if-not (lambda (ele)
			 (and (char= (aref ele 7) #\L)
			      (char= (aref ele 6) #\Space)
			      (char= (aref ele 5) #\Space)))
		       data-set)))))


;;; Now these the api functions

(defclass level ()
  ((excitation-energy :accessor excitation-energy
		      :initarg :excitation-energy)
   (spin :accessor spin
	 :initarg :spin
	 :initform ""
	 :type (simple-array character (*)))
   (parsed-energy :accessor parsed-energy
		  :initarg :parsed-energy
		  :type double-float)))

(defmethod print-object ((obj level) out)
  (print-unreadable-object (obj out :type t)
    (format out "{Ex=~A | Jpi=~A}" (excitation-energy obj) (spin obj))))


(defun try-parse-level (string)
  "Attempts to parse the level as a floating point number, if that
fails parse the string character by character collecting only the numerical characters and
decimals then try and parse again."
  (handler-case (parse-number:parse-number string)
    (sb-int:simple-parse-error (c)
      (declare (ignorable c))
      ;; This loop removes all of the junk and then parses the number again.
      (loop for char across string
	    if (or (digit-char-p char) (char= #\. char))
	      collect char into new-string
	    finally (return (parse-number:parse-number (concatenate 'string new-string)))))))

(defun make-level (ensdf-line)
  (let ((excitation-string (string-trim '(#\Space) (subseq ensdf-line 9 19))))
    (make-instance 'level
		   :excitation-energy excitation-string
		   :parsed-energy (try-parse-level excitation-string)
		   :spin (string-trim '(#\Space) (subseq ensdf-line 21 39)))))

(defun make-nucleus-levels (nucleus-name)
  "Retrieve all of the adopted levels for a given nucleus."
  (let ((proper-name (format nil "~{~A~}"
			     (reverse (translate-nucleus-name nucleus-name)))))
    (mapcar #'make-level
	    (zippy:with-zip-file (zip *ensdf-archive*)
	      (get-ensdf-level-records
	       (get-ensdf-mass-file zip (parse-integer proper-name :junk-allowed t))
	       proper-name)))))

(defun get-ground-state-spin (nucleus-name &optional return-integer)
  "Get a string for the ground state spin."
  (let ((result (if (string= nucleus-name "g")
		    "1-"
		    (spin (first (make-nucleus-levels nucleus-name))))))
    (if return-integer
	(read-from-string (subseq result 0 (- (length result) 1)))
	result)))

(defun find-levels (levels-or-nucleus level &key (energy-range nil) (delta 1.0))
  "Returns a list of all levels within an energy range. Two options can be used:
1) Specify a level and look for all levels within some delta
2) Specify an energy range as a list of (lower upper)"
  (let ((levels nil)
	(lower (if energy-range
		   (first energy-range)
		   (- level delta)))
	(upper (if energy-range
		   (second energy-range)
		   (+ level delta))))
    (etypecase levels-or-nucleus
      (string (setf levels (make-nucleus-levels levels-or-nucleus)))
      (list (setf levels levels-or-nucleus)))
    (loop for level in levels
	  for energy = (parsed-energy level)
	  if (<= lower energy upper)
	    collect level)))

(defun find-states-near-resonance (resonance-energy projectile target &key (delta 5.0))
  "Given an resonance energy observed in the lab between PROJECTILE and TARGET find
all excited states that are with DELTA. In other words quickly see if there are any
known states close to an observed resonance." 
  (let* ((projectile-nucleus (make-nucleus projectile))
	 (target-nucleus (make-nucleus target))
	 (compound-nucleus (make-nucleus-from-atomic-number
			    (+ (mass-number target-nucleus)
			       (mass-number projectile-nucleus))
			    (+ (charge-number target-nucleus)
			       (charge-number projectile-nucleus))))
	 (compound-name (str:concat (write-to-string (mass-number compound-nucleus))
				    (name compound-nucleus)))
	 (separation-energy (q-value (list projectile target)
				     (list compound-name)
				     :mass-function #'get-atomic-mass))
	 (com-conversion (/ (value (atomic-mass target-nucleus))
			    (+ (value (atomic-mass target-nucleus))
			       (value (atomic-mass projectile-nucleus)))))
	 (level (+ separation-energy (* com-conversion resonance-energy))))
    (values (find-levels compound-name level :delta delta)
	    level (* com-conversion resonance-energy))))




