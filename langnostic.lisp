;;;; langnostic.lisp
(in-package #:langnostic)

(defvar *article-dir* ".")

(defmethod ->title ((fname pathname))
  (->title (pathname-name fname)))

(defmethod ->title ((fname string))
  (string-capitalize (substitute #\space #\- fname)))

(defmacro page (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html (:head
	     (:style 
	      :type "text/css"
	      (str (cl-css:css 
		    '((body :width 75% :margin auto)
		      (pre :padding "10px 5px" :width 90% :background-color "#eee" :margin auto :overflow auto))))))
	    (:body ,@body))))

(define-closing-handler (root) ()
  (page 
    (:h1 (:code "/tmp/langnostic"))
    (:p "This is a stopgap blog archive for my own sanity, until either"
	(:ul (:li "Google/blogger gets off its ass and fixes " (:a :href "http://langnostic.blogspot.com" "this"))
	     (:li "I have enough time to put something nicer together.")))
    (:p "The second should happen within a week or so, when I've finished writing "
	(:a :href "#" "this")
	" and possibly putting something together for " 
	(:a :href "http://www.future-programming.org/call.html" "this") ". In the meanwhile, enjoy the below archived offerings. I haven't had time to put up a new CC widget, but rest assured that all content here is still CC-BY-SA licensed, and you may still do what you like with it.")
    (:hr)
    (:ul (loop for fname in (sort (cl-fad:list-directory ".") #'>= :key #'file-write-date)
	    do (htm (:li (:a :href (format nil "/article?name=~a" (pathname-name fname))
			     (str (->title fname)))))))))

(define-closing-handler (article) ((name :string))
  (page (let ((path (cl-fad:file-exists-p (merge-pathnames (concatenate 'string (pathname-name name) ".html") *article-dir*))))
	      (if path
		  (with-open-file (s path)
		    (let ((buf (make-string (file-length s))))
		      (read-sequence buf s)
		      (htm 
		       (:div :class "article"
			     (:h1 (:code (str (format nil "/tmp/langnostic/~a" (pathname-name path)))))
			     (str (format nil "~a" buf))))))
		  (str "Nonexistent...")))))

(defvar *server* (bt:make-thread (lambda () (start 4444))))
