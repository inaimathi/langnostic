rsync -rc resources/posts --exclude=*~ --progress inaimathi.ca:langnostic/resources
rsync resources/posts.json --progress inaimathi.ca:langnostic/resources
rsync -r --exclude=*~ --progress resources/public/ inaimathi.ca:langnostic/resources/public/
