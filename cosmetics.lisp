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
     (:html (:head (:link :rel "stylesheet" :href "/static/langnostic.css"))
	    (:body 
	     (:a :href "/" (:img :class "logo-image" :src "/static/img/langnostic.png"))
	     (:hr)
	     ,(top-menu 
	       section
	       '(("blog" "/")
		 ("archive" "/archive")
		 ("links" "/links")
		 ("meta" "/meta")))
	     (:hr)
	     ,@(when title `((:h1 :class "page-title" ,title)))
	     (:div :class "content" 
		   ,@body)
	     (:hr)
	     (:div :class "license"
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
     
     (pre :padding 10px :background-color "#eee"))))
