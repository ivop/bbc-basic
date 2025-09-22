#! /bin/sh

read A

while [ "$A" != "" ] ; do
    B=`echo "$A" | sed 's/^L/=$/g'`
    echo "$A$B"
    read A
done
