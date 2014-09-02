(in-package :langnostic)

(defparameter *base* (fact-base:load! #p"langnostic.base"))
(defparameter *latest* (latest-n 5))

(defmethod read-chronology ()
  (with-open-file (s "blog-chronology.lisp")
    (reverse (read s))))

(defmethod write-chronology (cron)
  (with-open-file (s "blog-chronology.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~s" cron)))

(defmethod load-all! ((dirname pathname))
  (let ((base (make-fact-base :file-name "langnostic.base")))
    (loop for (title file tags date) in (read-chronology)
       for path = (merge-pathnames file dirname)
       do (with-open-file (s path)
	    (let ((buf (make-string (file-length s))))
	      (read-sequence buf s)
	      (multi-insert! base `((:title ,title) (:file ,file) (:edited ,(file-write-date path)) (:body ,buf) (:posted ,date)
				    ,@(mapcar (lambda (tag) `(:tag ,tag)) tags))))))
    base))

(defmethod insert-article! ((base fact-base) (file pathname) (tags list) (body string) &optional title)
  (for-all (?id :current t) :in base :do (delete! base (list ?id :current t)))
  (multi-insert! base `((:title ,(or title (->title file))) 
			(:file ,(file-namestring file)) 
			(:edited ,(file-write-date file))
			(:body ,body) (:posted ,(get-universal-time))
			(:current t) ,@(mapcar (lambda (tag) `(:tag ,tag)) tags))))

(defun reload! ()
  (setf *base* (load! #p"langnostic.base")
	*latest* (latest-n 5))
  nil)

(defmethod update-article! ((file pathname))
  (for-all `(and (?id :file ,(file-namestring file))
		 (?id :edited ?edited)
		 (?id :body ?body))
	   :in *base* :do (progn (delete! *base* (list ?id :edited ?edited))
				 (delete! *base* (list ?id :body ?body))
				 (insert! *base* (list ?id :edited (file-write-date file)))
				 (with-open-file (s file)
				   (let ((buf (make-string (file-length s))))
				     (read-sequence buf s)
				     (insert! *base* (list ?id :body buf)))))))

(defmethod add-tags! ((article-id number) (new-tags list))
  (loop for tag in new-tags
     do (insert! *base* (list article-id :tag tag))))


(defmethod remove-tag! ((article-id number) (tag symbol))
  (delete! *base* (list article-id :tag tag)))

(defmethod replace-tags! ((article-id number) (new-tags list))
  (for-all `(,article-id :tag ?tag) :in *base* :do (delete! *base* (list article-id :tag ?tag)))
  (add-tags! article-id new-tags))

(defmethod new-article! ((file string) tags &optional title)
  (new-article! (pathname file) tags title))
(defmethod new-article! ((file pathname) (tags list) &optional title)
  (with-open-file (s file)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      (insert-article! *base* file tags buf title))))

(defun all-tags ()
  (let ((hash (make-hash-table)))
    (for-all (?id :tag ?tag) :in *base* 
	     :do (incf (gethash ?tag hash 0)))
    (sort (alexandria:hash-table-alist hash) #'string<=
	  :key (lambda (pair) (symbol-name (car pair))))))
