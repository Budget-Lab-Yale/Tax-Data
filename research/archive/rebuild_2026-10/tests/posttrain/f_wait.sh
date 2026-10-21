#!/bin/bash
# Wait until every file named on the command line exists (a glob is allowed),
# polling every 2 minutes. Used to chain F's jobs on other pieces' markers.
for f in "$@"; do
  until compgen -G "$f" > /dev/null; do sleep 120; done
  echo "$(date '+%H:%M:%S') found $f"
done
