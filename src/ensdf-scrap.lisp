(in-package #:nuclear-mass)

(defparameter *ensdf-url* "https://www.nndc.bnl.gov/ensdfarchivals/")
(defparameter *ensdf-json-url* "https://www.nndc.bnl.gov/ensdfarchivals/distributions/files.json")

(defun file-name-to-download-url (ensdf-file-name)
  "Take the file name found from the json file and then
construct the correct url."
  (str:concat
   *ensdf-url*
   "distributions/dist"
   (arrows:->> ensdf-file-name
	       (ppcre:split "(\\.|\_)")
	       (second)
	       (str:substring 0 2))
   "/"
   ensdf-file-name))


(defun download-file (url &optional file-name)
  (let ((file-name (if file-name
		       file-name
		       (file-namestring url))))
    (with-open-file (stream file-name
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (let ((file (dex:get url :force-binary t)))
	(loop :for byte :across file
	      :do
		 (write-byte byte stream))))
    file-name))

(defun make-ensdf-hash (json-path)
  "The json file format is moronic, so I clean it up a bit before passing it around."
  (let* ((raw-data (with-open-file (stream json-path)
		     (jzon:parse stream)))
	 (new-hash (make-hash-table :test 'eql)))
    ;; latest file will have key 0
    (setf (gethash 0 new-hash)
	  (aref (sera:@ raw-data "latest" "files") 0))
    ;; now get the distributions
    (loop :for h :across (sera:@ raw-data "distributions")
	  :for year = (parse-integer (sera:@ h "year"))
	  :for files = (remove-if-not (lambda (f)
					(and (string= "zip" (pathname-type f))
					     (or (string= "ensdf" (str:substring 0 5 f))
						 (string= "ENSDF" (str:substring 0 5 f)))))
				      (sera:@ h "files"))
	  :do
	     (setf (sera:@ new-hash year) files))
    new-hash))


(defun lookup-ensdf-file (ensdf-hash year)
  (flet ((translate (year)
	   (if (eql year 'latest)
	       0
	       year)))
    (sera:@ ensdf-hash (translate year))))


(defun update-ensdf (&key force)
  ;; update the json
  (download-file *ensdf-json-url* (package-path "./ensdf/" "files.json"))
  (let* ((ht (arrows:->> *ensdf-json-url*
			 (file-namestring)
			 (package-path "./ensdf/")
			 (make-ensdf-hash)))
	 (latest-file (lookup-ensdf-file ht 'latest)))
    (when (or force
	      (arrows:->> latest-file
			  (package-path "./ensdf/")
			  (uiop:file-exists-p)
			  (not)))
      (format t "~&Downloading ensdf update: ~A~%" latest-file)
      (download-file (arrows:->> latest-file
				 (file-name-to-download-url))
		     (package-path "./ensdf/" latest-file))
      (format t "Done!"))
    ;; update default for when we don't have internet access.
    (uiop:copy-file (package-path "./ensdf/" latest-file)
		    (package-path "./ensdf/" "ensdf.zip")))
  (package-path "./ensdf/" "ensdf.zip"))

