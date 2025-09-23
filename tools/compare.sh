#! /bin/sh

if [ "$#" != 2 ] ; then
    echo "usage: $0 refrom newrom" >&2
    exit 1
fi

ORG="$1"
NEW="$2"
size=`stat -c '%s' "$NEW"`

echo "Compare $size bytes from $ORG with $NEW"

dd if="$ORG" of=temp.rom bs=1 count=$size status=none

od -t x1 -A x temp.rom > org.txt
od -t x1 -A x "$NEW" > new.txt

diff -us org.txt new.txt

rm -f temp.rom org.txt new.txt
