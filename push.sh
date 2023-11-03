rsync -r project.clj src doc LICENSE --exclude=*~ --exclude=*clj --progress langnostic.inaimathi.ca:langnostic
rsync -rc resources/posts --exclude=*~ --progress langnostic.inaimathi.ca:langnostic/resources
rsync resources/posts.json --progress langnostic.inaimathi.ca:langnostic/resources
rsync -r --progress resources/public/ langnostic.inaimathi.ca:langnostic/resources/public/
