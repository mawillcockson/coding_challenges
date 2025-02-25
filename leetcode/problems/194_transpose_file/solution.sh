#! bash
# Read from the file file.txt and output the tenth line to stdout.
set -o posix
set -eu
FIELDS="$(< file.txt head -n 1 | tr -Cd ' ' | wc -m)"
FIELDS="$((FIELDS+1))"
export FIELDS
INDEX='1'

while test "$((FIELDS-INDEX))" -ge 0; do
    < file.txt cut -d ' ' -f "${INDEX}" | tr '\n' ' '
    printf '\n'
    INDEX="$((INDEX+1))"
done
