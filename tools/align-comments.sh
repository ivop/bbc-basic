#! /bin/sh

if [ "$#" != "1" ]; then
    echo "usage: $i filename" >&2
    exit 1
fi

perl -pe 's/^(.+?)(?= ;)/$1 . " "x(41-length($1))/e' "$1"
