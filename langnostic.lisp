;;;; langnostic.lisp
(in-package #:langnostic)

(define-closing-handler (meta) ()
  (page ("Meta" :section "meta")
    (:h3 "About Me")
    (:p "I'm a Graphic Designer " 
	(:i "(graduated from OCAD with a bachelor's)")
	"specializing in illustration and web development. Also, I like programming, and learning languages. Which is why I'm here.")
    (:p "Currently, I'm working as a Common Lisp developer at the Moneris point of sale R&D team.")

    (:h3 "About This Site")
    (:p "This site was built out of parentheses and adrenaline. The site itself is about 300 lines of fairly readable Common Lisp. The \"database\" is a " 
	(:a :href "https://github.com/Inaimathi/fact-base" "simple triple-store")
	" implemented in another ~400 lines of the same, and the web server is a "
	(:a :href "https://github.com/Inaimathi/house" "minimal, asynchronous system")
	" put together in yet another ~800 lines of CL.")))

(defun prev+next-links (article-id)
  (with-html-output (s *standard-output*)
    (let ((prev (- article-id 1))
	  (next (+ article-id 1)))
      (for-all (and (prev :file ?prev) (prev :title ?title)) :in *base*
	       :do (htm (:a :class "prev" :href (format nil "/article?name=~a" ?prev) (fmt "<- ~a" (truncat ?title 30)))))
      (for-all (and (next :file ?next) (next :title ?title)) :in *base*
	       :do (htm (:a :class "next" :href (format nil "/article?name=~a" ?next) (fmt "~a ->" (truncat ?title 30)))))
      (htm (:br :style "clear:both;")))))

(define-closing-handler (root) ()
  (page (nil :section "blog")
    (:p "Welcome to Language Agnostic, the blog of " (:a :href "https://github.com/Inaimathi" "Inaimathi") "! And, thanks to the " (:a :href "https://github.com/Inaimathi/fact-base" "storage approach") ", possibly the least performant blog on the internet!")
    (:p "Enjoy the various programming-themed writings available on offer. The Latest post is available below, and the archive link is directly above this text.")
    (:hr)
    (for-all (and (?id :current t) (?id :title ?title) (?id :body ?body))
	     :in *base*
	     :do (htm (:h1 :class "page-title" (str ?title))
		      (:div :class "content" (str ?body))
		      (:hr)
		      (prev+next-links ?id)))))

(define-closing-handler (feed/atom :content-type "application/rss+xml") ()
  (with-html-output-to-string (*standard-output* nil :prologue "<?xml version=\"1.0\" encoding=\"utf-8\"?>" :indent t)
    (:feed :xmlns "http://www.w3.org/2005/Atom"
	   (:title "Language Agnostic")
	   (:subtitle "Site-wide Langnostic Atom feed")
	   (:link :href "http://langnostic.inaimathi.ca/feed/atom" :rel "self")
	   (:link :href "http://langnostic.inaimathi.ca")

	   (for-all (and (?id :current t) (?id :title ?title) (?id :file ?file) (?id :posted ?timestamp) (?id :body ?body)) :in *base*
		    :do (htm (:id (fmt "tag:langnostic.inaimathi.ca,~a" ?id))
			     (:updated (rss-timestamp ?timestamp))
			     (:entry
			      (:title (str ?title))
			      (:link :href (format nil "http://langnostic.inaimathi.ca/article?name=~a" ?file))
			      (:id (fmt "tag:langnostic.inaimathi.ca,~a" ?id))
			      (:updated (rss-timestamp ?timestamp))
			      (:content 
			       :type "xhtml" :xml\:lang "en" 
			       (:div :xmlns "http://www.w3.org/1999/xhtml" 
				     (str ?body)))
			      (:author (:name "Inaimathi"))))))))

(define-closing-handler (article) ((name :string))
  (aif (for-all (and (?id :file name) (?id :title ?title) (?id :body ?body)) 
		:in *base* 
		:collect (page ((str ?title) :section "blog")
			   (str ?body)
			   (:hr)
			   (prev+next-links ?id)))
       (first it)
       (page ((fmt "Not found: ~s" name) :section "blog"))))

(define-closing-handler (archive) ()
  (page ("Archive" :section "archive")
    (:h3 "Tags")
    (:ul :class "tags" 
	 (loop for (tg . ct) in (all-tags)
	    do (htm (:li (:a :href (format nil "/archive/by-tag?tag=~a" tg) (str tg)) (fmt "(~a)" ct)))))
    (:hr)
    (:h3 "Chronological")
    (:ul (for-all (and (?id :title ?title) (?id :file ?fname)) :in *base*
		  :do (htm (:li (:a :href (format nil "/article?name=~a" ?fname) (str ?title))))))))

(define-closing-handler (archive/by-tag) ((tag :keyword))
  (page ((fmt "By Tag: ~a" tag) :section "archive")
    (:ul (for-all (and (?id :title ?title) (?id :file ?fname) (?id :tag tag)) :in *base*
		  :do (htm (:li (:a :href (format nil "/article?name=~a" ?fname) (str ?title))))))
    (:hr)
    (:ul :class "tags" 
	 (loop for (tg . ct) in (all-tags)
	    do (htm (:li (:a :href (format nil "/archive/by-tag?tag=~a" tg) (str tg)) (fmt "(~a)" ct)))))))


(define-closing-handler (links) ()
  (page ("Links of Interest to Programming Polyglots" :section "links")
    (:h3 "Generally useful things")
    (link-list
     "http://git-scm.com/" "GIT - Fast Version Control"
     "http://conkeror.org/" "Conkeror - Firefox for Emacs users"
     "http://xmonad.org/" "XMonad"
     "http://www.nongnu.org/stumpwm/" "StumpWM"
     "http://rosettacode.org/wiki/Rosetta_Code" "Rosetta Code"
     "http://hyperpolyglot.org/" "Hyperpolyglot"
     "https://github.com/adambard/learnxinyminutes-docs" "Learn X in Y Minutes")
    (:h3 "Language Resources (in no particular order)")
    (:h4 "JavaScript")
    (link-list
     "http://nodejs.org/" "node.js"
     "http://code.google.com/p/v8/" "V8"
     "http://jquery.com/" "jQuery"
     "http://underscorejs.org/" "underscore.js"
     "http://mbostock.github.com/d3/" "d3.js"
     "http://handlebarsjs.com/" "Handlebars.js"
     "http://angularjs.org/" "Angular.js")
    (:h4 "REBOL")
    (link-list
     "https://github.com/rebol/r3" "REBOL3 Repository"
     "http://www.rebol.com/rebolsteps.html" "REBOL in Ten Steps"
     "http://www.rebol.com/r3/docs/guide.html" "REBOL3 Guide"
     "http://www.rebol.com/r3/docs/functions.html" "REBOL3 function reference"
     "http://chat.stackoverflow.com/rooms/291/rebol-and-red" "REBOL/RED Chatroom"
     "http://www.red-lang.org/" "RED: an in-development variant of REBOL with a JIT compiler")
    (:h4 "Ruby")
    (link-list
     "http://www.ruby-lang.org/en/" "The Ruby Language"
     "http://mislav.uniqpath.com/poignant-guide/book/" "Why's Guide"
     "http://watir.com/" "Watir Testing Framework")
    (:h4 "Erlang")
    (link-list
     "http://armstrongonsoftware.blogspot.com/" "Armstrong on Software"
     "http://ftp.sunet.se/pub/lang/erlang/" "Erlang + OTP"
     "http://yaws.hyber.org/" "YAWS"
     "http://nitrogenproject.com/" "Nitrogen"
     "http://learnyousomeerlang.com/" "Learn You Erlang"
     "https://github.com/rvirding/lfe" "Lisp Flavored Erlang"
     "https://github.com/basho/rebar/" "Rebar")
    (:h6 "Erlang Language Interfaces")
    (link-list
     "http://www.erlang.org/doc/tutorial/c_port.html" "C"
     "http://erlport.org/" "Python"
     "https://github.com/mojombo/erlectricity" "Ruby"
     "http://www.erlang.org/doc/apps/jinterface/java/index.html" "Java")
    (:i :style "font-size: x-small;" "The Common Lisp and Haskell interfaces aren't listed because \"I haven't gotten it working\" and \"it sucks donkey dong\" respectively.")
    (:h4 "Haskell")
    (link-list
     "http://learnyouahaskell.com/" "Learn You Haskell"
     "http://book.realworldhaskell.org/" "Real World Haskell"
     "http://www.xent.com/pipermail/fork/Week-of-Mon-20070219/044101.html" "An Accurate Assessment"
     "http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours" "Write Yourself a Scheme"
     "http://happstack.com/docs/crashcourse/index.html" "Happstack Crash Course"
     "http://snapframework.com/" "snap"
     "http://www.haskell.org/haskellwiki/Web/Servers#Warp" "Warp"
     "http://hackage.haskell.org/packages/archive/pkg-list.html" "hackage"
     "http://www.haskell.org/hoogle/" "hoogle"
     "http://holumbus.fh-wedel.de/hayoo/hayoo.html" "hayoo"
     "http://leksah.org/index.html" "leksah")
    (:h4 "Smalltalk")
    (link-list
     "http://www.pharo-project.org/home" "Pharo"
     "http://rmod.lille.inria.fr/coral/index.html" "Coral - Scripting with Pharo"
     "http://pharobyexample.org/" "Pharo By Example"
     "http://book.pharo-project.org/book/table-of-contents/?_s=GaoutvL8fOIi-wBt&_k=dffRK647Ld0GiAwR&_n&6" "Pharo CollaborActive Book"
     "http://www.seaside.st/" "Seaside Web Framework"
     "http://seaside.gemstone.com/tutorial.html" "Seaside 3.0 In Pharo"
     "http://onsmalltalk.com/terse-guide-to-seaside" "Terse Guide to Seaside")
    (:h4 "PHP")
    (link-list
     "http://php.net/manual/en/index.php" "PHP Manual"
     "http://codeigniter.com/" "CodeIgniter"
     "http://wordpress.org/" "WordPress"
     "http://drupal.org/" "Drupal")
    (:h4 "Python")
    (link-list
     "http://www.python.org/download/" "IDLE Download"
     "http://webpython.codepoint.net/mod_python_tutorial" "mod_python tutorial"
     "http://www.modpython.org/live/current/doc-html/" "mod_python docs"
     "http://webpy.org/" "web.py"
     "http://www.tornadoweb.org/" "tornado")
    (:h4 "Common Lisp")
    (link-list
     "http://dept-info.labri.u-bordeaux.fr/~idurand/enseignement/PFS/Common/Strandh-Tutorial/indentation.html" "Indenting Lisp"
     "http://www.cliki.net/CL-WIKI" "The CLiki"
     "http://www.lispworks.com/documentation/HyperSpec/Front/X_AllSym.htm" "Hyperspec Symbol List"
     "http://community.schemewiki.org/?scheme-vs-common-lisp" "Scheme vs. Common Lisp comparison"
     "http://www.lisperati.com/" "Lisperati"
     "http://gigamonkeys.com/book/" "Practical Common Lisp"
     "http://cl-cookbook.sourceforge.net/index.html" "Common Lisp Cookbook"
     "http://weitz.de/hunchentoot/" "Hunchentoot (the CL-web server, not the rock opera)")
    (:h4 "Scheme")
    (link-list
     "http://racket-lang.org/" "PLT Racket"
     "http://schemecookbook.org/" "Scheme Cookbook"
     "http://community.schemewiki.org/?scheme-vs-common-lisp" "Scheme vs. Common Lisp comparison"
     "http://www.call-cc.org/" "Chicken Scheme"
     "http://www-sop.inria.fr/mimosa/fp/Bigloo/" "Bigloo Scheme"
     "http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page" "Gambit"
     "http://code.google.com/p/termite/" "Termite")
    (:h4 "Clojure")
    (link-list
     "http://clojuredocs.org/" "Clojure Docs"
     "http://leiningen.org/" "Leiningen"
     "http://webnoir.org/" "Noir"
     "http://search.maven.org/" "maven"
     "https://clojars.org/" "clojars"
     "http://www.heroku.com/" "Heroku")
    (:h4 "Emacs")
    (link-list
     "http://www.gnu.org/software/emacs/" "Emacs"
     "http://www.emacswiki.org/emacs/RedoMode" "Redo Mode"
     "http://www.emacswiki.org/emacs/auto-complete.el" "auto-complete mode"
     "http://www.emacswiki.org/emacs/AutoPairs" "autopair mode"
     "http://www.emacswiki.org/emacs/ParEdit" "paredit"
     "http://www.emacswiki.org/emacs/multi-shell.el" "multi-eshell mode"
     "http://www.emacs.uniyar.ac.ru/doc/O'reilly_emacs/writing%20gnu%20emacs%20extensions.pdf" "Emacs Extension Guide(pdf)"
     "http://xahlee.org/emacs/emacs.html" "Xah's Emacs Guide"
     "http://xahlee.org/emacs/elisp.html" "Xah's Elisp Guide")
    (:h6 "Emacs Language-specific Modes")
    (link-list
     "http://common-lisp.net/project/slime/" "SLIME - Superior Lisp Interaction Mode for Emacs"
     "http://www.neilvandyke.org/quack/" "Quack - enhanced Scheme mode for Emacs"
     "https://github.com/technomancy/clojure-mode/" "Clojure Mode and Clojure Test Mode"
     "https://github.com/technomancy/swank-clojure" "swank-clojure"
     "http://www.emacswiki.org/emacs/Js2Mode" "js2 mode"
     "http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/" "Setting up node environment in Emacs"
     "http://projects.haskell.org/haskellmode-emacs/" "Haskell Mode")
    (:i :style "font-size: x-small;" "Ruby and Erlang each come with their own modes, and recent Emacs versions ship with a built-in Python mode and shell. Smalltalk uses its own environment (though GNU Smalltalk does have " (:a :href "http://www.gnu.org/software/smalltalk/manual/html_node/Emacs.html" "its own mode") "), and I'd really rather not talk about PHP. If you're writing in it, chances are you're using Eclipse or an IDE anyway.")
    (:h3 "More from Inaimathi")
    (link-list
     "https://github.com/Inaimathi" "github"
     "http://inaimathi.deviantart.com/" "deviantArt"
     "http://stackoverflow.com/users/190887/inaimathi" "SO profile"
     "http://www.cliki.net/Inaimathi" "Cliki entry")))

(define-file-handler "static")

(defvar *server* (bt:make-thread (lambda () (start 4444))))
