# Sync posts and posts.json to the server. Pass --public (or --all) to also
# sync the generated public/ directory.
sync_public=0
for arg in "$@"; do
  case "$arg" in
    --public|--all) sync_public=1 ;;
  esac
done

rsync -rc resources/posts --exclude=*~ --progress inaimathi.ca:langnostic/resources
rsync resources/posts.json --progress inaimathi.ca:langnostic/resources

if [ "$sync_public" -eq 1 ]; then
  rsync -r --exclude=*~ --progress resources/public/ inaimathi.ca:langnostic/resources/public/
fi
