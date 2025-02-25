#!/bin/bash
# Read from the file words.txt and output the word frequency list to stdout.
set -o posix
set -eu
< words.txt tr -s '\n\t ' '\n' | sort | uniq -c | sort -rnk 1,1 | tr -s ' \t' ' ' | awk '{print $2 " " $1}'
