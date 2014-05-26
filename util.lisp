(in-package :langnostic)

(defmethod ->title ((fname pathname))
  (->title (pathname-name fname)))

(defmethod ->title ((fname string))
  (string-capitalize (substitute #\space #\- fname)))

(defun link-list (&rest uri/name-list)
  (with-html-output (s *standard-output*)
    (:ul (loop for (uri name) on uri/name-list by #'cddr
	    do (htm (:li (:a :href uri (str name))))))))
