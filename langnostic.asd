;;;; langnostic.asd

(asdf:defsystem #:langnostic
  :serial t
  :description "Describe langnostic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:anaphora #:house #:cl-who #:cl-fad #:cl-css #:parenscript #:fact-base)
  :components ((:file "package")
	       (:file "util")
	       (:file "model")
	       (:file "cosmetics")
               (:file "langnostic")))

