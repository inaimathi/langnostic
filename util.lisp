(in-package :langnostic)

(defmethod ->title ((fname pathname))
  (->title (pathname-name fname)))

(defmethod ->title ((fname string))
  (string-capitalize (substitute #\space #\- fname)))

(defmethod truncat ((thing string) (len number))
  (if (> (length thing) len)
      (let* ((short (subseq thing 0 len))
	     (last-space (position #\space short :from-end t)))
	(concatenate 'string (subseq short 0 last-space) "..."))
      thing))

(defun link-list (&rest uri/name-list)
  (with-html-output (s *standard-output*)
    (:ul (loop for (uri name) on uri/name-list by #'cddr
	    do (htm (:li (:a :href uri (str name))))))))
