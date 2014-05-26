(in-package :langnostic)

(defparameter *base* (fact-base:load! #p"langnostic.base"))

(defun articles ()
  (for-all (and (?id :file ?fname) (?id :title ?title))
	   :in *base* :collect (list ?fname ?title)))
