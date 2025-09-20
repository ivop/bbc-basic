#! /bin/sh

ORG=original/Basic2
size=`stat -c '%s' basic.rom`

dd if=$ORG of=temp.rom bs=1 count=$size

od -t x1 -A x temp.rom > org.txt
od -t x1 -A x basic.rom > new.txt

diff -us org.txt new.txt

rm -f temp.rom org.txt new.txt
