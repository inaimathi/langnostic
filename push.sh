#!/bin/bash

# Sync posts and posts.json by default
rsync -rc resources/posts --exclude=*~ --progress inaimathi.ca:langnostic/resources
rsync resources/posts.json --progress inaimathi.ca:langnostic/resources

# Optionally sync public directory if argument is passed
if [ "$1" = "--include-public" ]; then
  rsync -r --exclude=*~ --progress resources/public/ inaimathi.ca:langnostic/resources/public/
fi
