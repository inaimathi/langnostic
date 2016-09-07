rsync -ric resources project.clj src test doc LICENSE --exclude=*~ --exclude=resources/posts/* --progress inaimathi.ca:langnostic
rsync -ric resources/posts/ --exclude=*~ --dry-run --progress inaimathi.ca:langnostic/resources/posts/ | cut -d ' ' -f2 | tail -n +2 | xargs -I+ scp "resources/posts/+" inaimathi.ca:langnostic/resources/posts/
