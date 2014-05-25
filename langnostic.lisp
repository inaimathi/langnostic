;;;; langnostic.lisp
(in-package #:langnostic)

(define-closing-handler (meta) ()
  (page (:code "/tmp/langnostic/about-me")
    (:p "I'm a Graphic Designer specializing in illustration and web development. Also, I like programming, and learning languages. Which is why I'm here.")))

(define-closing-handler (root) ()
  (page nil
    (:p "This is a stopgap blog archive for my own sanity, until either"
	(:ul (:li "Google/blogger gets off its ass and fixes " (:a :href "http://langnostic.blogspot.com" "this"))
	     (:li "I have enough time to put something nicer together.")))
    (:p "The second should happen within a week or so, when I've finished writing "
	(:a :href "https://github.com/Inaimathi/500lines/blob/master/async-web-server/writeup.md" "this")
	" and possibly putting something together for " (:a :href "http://www.future-programming.org/call.html" "this") 
        ". In the meanwhile, enjoy the below archived offerings.")
    (:hr)
    (:ul (loop for (fname title) in (articles)
	    do (htm (:li (:a :href (format nil "/article?name=~a" fname) (str title))))))))

(define-closing-handler (article) ((name :string))
  (let ((article (first (for-all (and (?id :file name) (?id :body ?body)) :in *base* :collecting ?body))))
    (page (:code (str (format nil "/tmp/langnostic/~a" name)))
      (if article
	  (str article)
	  (str "Nope...")))))

(define-file-handler "static")

(defvar *server* (bt:make-thread (lambda () (start 4444))))
