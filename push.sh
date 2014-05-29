rsync *.lisp *.asd langnostic.base --progress --exclude=blog-chronology.lisp inaimathi.ca:quicklisp/local-projects/langnostic/
rsync static/img/* --progress inaimathi.ca:quicklisp/local-projects/langnostic/static/img/
