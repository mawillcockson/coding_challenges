#!/bin/bash
# Read from the file file.txt and output all valid phone numbers to stdout.
set -o posix
set -eu
grep -xEe '(\([[:digit:]]{3}\) |[[:digit:]]{3}-)[[:digit:]]{3}-[[:digit:]]{4}' file.txt
