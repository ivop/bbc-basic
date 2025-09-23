#! /bin/sh

read A

while [ "$A" != "" ] ; do
    B=`echo "$A" | sed 's/^L/=$/g'`
    if [ "$B" = "$A" ] ; then
        B=`echo "$A" | sed 's/^X/=$/g'`
    fi
    echo "$A$B"
    read A
done
