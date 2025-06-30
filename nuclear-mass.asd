;;;; nuclear-mass.asd

(asdf:defsystem #:nuclear-mass
  :description "Nuclear mass information"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre"
               "alexandria"
               "parse-number"
	       "zippy"
	       "flexi-streams"
	       "str"
	       "arrows"
	       "dexador"
	       "lquery"
	       "com.inuoe.jzon"
	       "serapeum")
  :components ((:module "mass-eval"
		:components ((:static-file "mass20.csv")
			     (:static-file "mass16.csv")
			     (:static-file "mass12.csv")
			     (:static-file "mass03.csv")
			     (:static-file "mass95.csv")
			     (:static-file "mass93.csv")))
	       (:module "ensdf"
		:components ((:static-file "ensdf.zip")))
	       (:module "src"
		:components ((:file "package")
			     (:file "utils")
			     (:file "mass-table")
			     (:file "nuclear-mass")
			     (:file "ensdf-scrap")
			     (:file "ensdf")
			     (:file "ensdf-gammas")))))
