(in-package :langnostic)

(defun top-menu (current-section name/uri-pairs)
  `(:div :class "top-menu fade-in second"
	 ,@(loop for ((name uri) . rest) on name/uri-pairs
	      if (and current-section (string= name current-section))
	      collect name
	      else collect `(:a :href ,uri ,name)
	      when rest collect " | ")))

(defmacro page ((&optional title &key section) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html (:head (:link :rel "stylesheet" :href "/static/langnostic.css")
		   (:script :type "text/javascript" :src "/static/langnostic.js")
		   (:noscript (:style :type "text/css" (str (cl-css:css '((.fade-in :opacity 0)))))))
	    (:body 
	     (:a :href "/" (:img :class "logo-image fade-in first" :src "/static/img/langnostic.png"))
	     (:hr :class "fade-in second")
	     ,(top-menu 
	       section
	       '(("blog" "/")
		 ("archive" "/archive")
		 ("links" "/links")
		 ("meta" "/meta")))
	     (:hr :class "fade-in second")
	     ,@(when title `((:h1 :class "page-title fade-in third" ,title)))
	     (:div :class "content fade-in fourth" 
		   ,@body)
	     (:hr :class "license fade-in fourth")
	     (:div :class "license fade-in fourth"
		   (:a :rel "license" :href "http://creativecommons.org/licenses/by-sa/3.0/"
		       (:img :alt "Creative Commons License" :style "border-width:0;float: left; margin: 0px 15px 15px 0px;"
			     :src "http://i.creativecommons.org/l/by-sa/3.0/88x31.png"))
		   (:p (:span :xmlns\:dct "http://purl.org/dc/terms/"
			      :property "dct:title"
			      "all articles at langnostic")
		       " are licensed under a "
		       (:a :rel "license" :href "http://creativecommons.org/licenses/by-sa/3.0/"
			   "Creative Commons Attribution-ShareAlike 3.0 Unported License"))
		   (:p "Reprint, rehost and distribute freely (even for profit), but attribute the work and allow your readers the same freedoms. " 
		       (:a :href "http://creativecommons.org/choose/results-one?q_1=2&q_1=1&field_commercial=y&field_derivatives=sa&field_jurisdiction=&field_format=&field_worktitle=this+langnostic+article&field_attribute_to_name=Inaimathi&field_attribute_to_url=http%3A%2F%2Flangnostic.inaimathi.com&field_sourceurl=http%3A%2F%2Flangnostic.inaimathi.com&field_morepermissionsurl=&lang=en_US&n_questions=3" "Here's")
		       " a license widget you can use."))))))

(define-closing-handler (static/langnostic.css :content-type "text/css") ()
  (cl-css:css 
   `((body :width 80% :margin auto :font-family sans-serif)
     
     (.logo-image :width 100% :margin-top 15px :padding-bottom 3px)
     (.top-menu :font-weight bold)
     (".top-menu a" :color "#CC0606")
     (".top-menu a:hover" :text-decoration none :color "#999")
     (.tags :clear both)
     (".tags li" :list-style-type none :display inline-block :margin 3px :font-weight bold)

     (".next, .prev" :font-weight bold :color "#CC0606")
     (".next:hover, .prev:hover" :text-decoration none :color "#999")
     (.next :float right)
     
     (.license :font-size x-small)

     (hr :border-color red)
     (.page-title :color "#CC0606")
     
     (pre :padding 10px :background-color "#eee")

     (.fade-in :opacity 0)
     (.first ,@(cl-css:transition :opacity :duration 2))
     (.second ,@(cl-css:transition :opacity :duration 1 :delay .3))
     (.third ,@(cl-css:transition :opacity :duration 1 :delay .5))
     (.fourth ,@(cl-css:transition :opacity :duration 1 :delay .7))
     (.revealed :opacity 1))))

(define-closing-handler (static/langnostic.js :content-type "application/javascript") ()
  (ps (defun dom-ready (callback)
	(chain document (add-event-listener "DOMContentLoaded" callback)))
      (defun by-selector (selector)
	(chain document (query-selector selector)))
      (defun by-selector-all (selector)
	(chain document (query-selector-all selector)))
      (defun map (fn thing)
	(if (object? thing)
	    (let ((res (-array)))
	      (for-in (k thing) (chain res (push (fn (aref thing k) k))))
	      res)
	    (loop for elem in thing collect (fn elem))))
      
      (dom-ready
       (lambda ()
	 (let ((fades (by-selector-all ".fade-in")))
	   (when fades (loop for elem in fades do (chain elem class-list (add "revealed")))))))))
