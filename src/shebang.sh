/*usr/bin/env true

set -eu

type scryer-prolog > /dev/null 2> /dev/null \
    && exec scryer-prolog -f -g "bakage:run" "$0" -- "$@"

echo "No known supported Prolog implementation available in PATH."
echo "Try to install Scryer Prolog."
exit 1
#*/

