(in-package #:nuclear-mass)

(defun package-path (&optional (file/dir "") file)
  (let ((system-path (asdf:system-relative-pathname 'nuclear-mass file/dir)))
    (if file
	(merge-pathnames file system-path)
	system-path)))

(declaim (inline sqr))
(defun sqr (x)
  (* x x))
