rsync -rc ${INCLUDE_PUBLIC:+resources/public/}resources/posts --exclude=*~ --progress inaimathi.ca:langnostic/resources
echo $?
rsync resources/posts.json --progress inaimathi.ca:langnostic/resources
if [ $? -eq 0 ]; then rsync -r --exclude=*~ --progress ${INCLUDE_PUBLIC:+resources/public/}resources/public/ inaimathi.ca:langnostic/resources/public/; fi
