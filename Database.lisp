(ql:quickload :cl-ppcre)
(ql:quickload :cl-utilities)
;;; Fonction nécéssaire pour load/save db et lire fichier
(defvar *db* nil)
(defvar *current-dir* nil)
(defvar *total* 0)
(defvar *db-keys* '(:type :object :category :money :date))

;; Update the total amount
; transform it to macro!
(defun update-total (operation amount)
  (if (string= "addition" operation) (setf *total* (+ *total* (parse-integer amount)))
      (setf *total* (- *total* (parse-integer amount)))))

(defun make-operation (plist-csv)
  (setf (getf plist-csv :total) (update-total (getf plist-csv :type) (getf plist-csv :money)))
  (return-from make-operation plist-csv))

;;Transforme a list to plist
(defun list-to-plist (list-data keys)
  (let ((return-list nil))
    (do ((tmp-keys keys (cdr tmp-keys))
	 (tmp-list-data list-data (cdr tmp-list-data))
	 (index 0 (1+ index)))
	 (())
      (when (= index (list-length list-data))
	(return (reverse return-list)))
      (setf (getf return-list (car tmp-list-data)) (car tmp-keys)) )))

(defun add-operation (cd)
  (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* (read out)))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Read CSV in the file
(defun read-csv (file)
  (loop for line = (read-line file nil :eof) ; stream, no error, :eof value
     until (eq line :eof)
     do (add-operation (make-operation (list-to-plist (cl-ppcre:split "(;)" line) *db-keys*)))))

;; Read the file
(defun read-entry (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read-csv in))))

;; Save directory for further operation
(defun set-root-directory (user-specified-dir)
  (setf *current-dir* (make-pathname :directory (pathname-directory (pathname user-specified-dir)) :name :wild :type "txt")))

;; Parse directory files.
(defun parse-directory (dir)
  (set-root-directory dir)
  (loop for x in (directory *current-dir*)
     do (read-entry x)))


;;; Opérateur de DB
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@ (make-comparisons-list clauses))))

