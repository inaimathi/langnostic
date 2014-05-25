(in-package :langnostic)

(defparameter *base* (fact-base:load! "langnostic.base"))

(defun articles ()
  (for-all (and (?id :file ?fname) (?id :title ?title))
	   :in *base* :collecting (list ?fname ?title)))
