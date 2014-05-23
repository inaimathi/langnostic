;;;; langnostic.asd

(asdf:defsystem #:langnostic
  :serial t
  :description "Describe langnostic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:house #:cl-who #:cl-fad #:cl-css)
  :components ((:file "package")
               (:file "langnostic")))

