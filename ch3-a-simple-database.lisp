(defun make-cd (title artist rating ripped)
  (list :title title
	:artist artist
	:rating rating
	:ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title ")
   (prompt-read "Artist ")
   (or (parse-integer (prompt-read "Rating ") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]:")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))
	 (return))))

(defun save-db (filename)
  (with-open-file (out filename
		   :direction :output
		   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Querying the database
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd)
       (equal (getf cd :artist)
	      artist))
   *db*))

;;; Instead of having a select-by-artist, by-title, rating and ripped we can have a
;;; generic select function

;;; Example usage of select
;;; (select (where :ARTIST "Arun"))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd)
      (equal (getf cd :artist)
	     artist)))

;;; To avoid having to write boilerplate code for title-selector, ripped-selector and
;;; rating-selector we are going to have a 'where'

;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (cd)
;;       (and
;;        (if title    (equal (getf cd :title)  title)  t)
;;        (if artist   (equal (getf cd :artist) artist) t)
;;        (if rating   (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped)  ripped) t))))

;;; Example usage of update
;; (update (where :Artist "Me")
;; 	:Artist "Arun" :ripped t :rating 10)
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title    (setf (getf row :title)  title))
	       (if artist   (setf (getf row :artist) artist))
	       (if rating   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row)
	 *db*)))

;;; delete function
;;; Example: (delete-rows (where :title "hello"))
(defun delete-rows (selector-fn)
  (setf *db*
	(remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))


;;; Eg: (select (where :Artist "arun" :ripped nil))
(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))
