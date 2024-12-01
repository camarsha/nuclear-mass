;; Convert the AME file into a csv file

(in-package #:nuclear-mass)



(defparameter *ame-format* '(1 3 5 5 5 1 3 4 1 13 11 11 9 1 2 11 9 1 3 1 12 11))
(defparameter *ame-csv-header* "N-Z,N,Z,A,El,O,ME,ME-unc,BE,BE-unc,BD,BD-unc,Mass,Mass-unc")


(defun make-ame-line (vals)
  (assert (= (length vals) 20))
  (loop :for val in vals
	:for key :in '(:junk
		       :n-z
		       :n
		       :z
		       :a
		       :junk
		       :el
		       :o
		       :junk
		       :me :me-unc
		       :be :be-unc
		       :junk
		       :junk
		       :bd :bd-unc
		       :junk
		       :mass :mass-unc)
	:unless (eql key :junk)
	  :collect (cons key (if (string= val "*") "" val))))

(defun split-ame-line (line)
  (loop :with current-char = 0
	:for len :in *ame-format*
	:for string = (make-string len :initial-element #\space)
	:do
	   (dotimes (i len)
	     (when (< current-char (length line)) 
	       (setf (aref string i) (aref line current-char))
	       (incf current-char)))
	:collect string))


(defun combine-mass-values (line)
  (reverse
   (let* ((l (reverse line))
	  (unc (pop l))
	  (mass-low (pop l))
	  (junk (pop l))
	  (mass-high (pop l))
	  (mass (str:concat mass-high mass-low)))
     (declare (ignorable junk))
     (cons unc (cons mass l)))))



(defun parse-ame-line (line)
  (arrows:->> (split-ame-line line)
	      (mapcar #'str:trim)
	      (mapcar (lambda (ele)
			(str:replace-all "#" "" ele)))
              (combine-mass-values)
	      (make-ame-line)))


(defun read-mass-file (file-name)
  (with-open-file (stream file-name :direction :input)
    (loop :for line = (read-line stream nil)
	  :for line-num from 0
	  :while line
	  :when (>= line-num 39)
	    :collect (parse-ame-line line))))


(defun ame->csv (file-name output-file)
  (with-open-file (stream output-file :direction :output
				      :if-does-not-exist :create
				      :if-exists :supersede)
    (format stream "~&~A~%" *ame-csv-header*)
    (dolist (line (read-mass-file file-name))
      (format stream "~&~{~A~^,~}~%" (mapcar #'cdr line)))))
