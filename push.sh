rsync *.lisp *.asd --progress --exclude=blog-chronology.lisp inaimathi.ca:quicklisp/local-projects/langnostic/
rsync langnostic.base --progress --append-verify inaimathi.ca:quicklisp/local-projects/langnostic/
rsync static/img/* --progress inaimathi.ca:quicklisp/local-projects/langnostic/static/img/
