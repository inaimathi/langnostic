rsync -r project.clj src test doc LICENSE --exclude=*~ --progress inaimathi.ca:langnostic
rsync -r resources/public --exclude=*~ --progress inaimathi.ca:langnostic/resources
rsync -ric resources/posts/ --exclude=*~ --dry-run --progress inaimathi.ca:langnostic/resources/posts/ | cut -d ' ' -f2 | tail -n +2 | xargs -I+ scp "resources/posts/+" inaimathi.ca:langnostic/resources/posts/
rsync -ric resources/posts.json --dry-run --progress inaimathi.ca:langnostic/resources/posts.json | cut -d ' ' -f2 | tail -n +2 | xargs -I+ scp resources/posts.json inaimathi.ca:langnostic/resources/
