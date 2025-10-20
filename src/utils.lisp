(in-package #:nuclear-mass)

(defun package-path (&optional (file/dir "") file)
  (let ((system-path (asdf:system-relative-pathname 'nuclear-mass file/dir)))
    (if file
	(merge-pathnames file system-path)
	system-path)))

(declaim (inline sqr))
(defun sqr (x)
  (* x x))

(defvar +electron-mass+ 510.998950691753
  "Mass of electron in keV, CODATA 2022")

(defvar +electron-charge+ 1.602176634e-19
  "Electron charge in coulombs, CODATA 2022")

(defvar +atomic-units->mev+ 931.49410242
  "Number of MeV in 1u.")

(defvar +h-bar+ 6.582119569e-16
  "Reduced Planck's constant in eV s.")

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
  (list (first ;; number
	 (cl-ppcre:all-matches-as-strings "\\d+" string))
	(string-capitalize ;;name
	 (first
	  (cl-ppcre:all-matches-as-strings "[a-zA-Z]+" string)))))
