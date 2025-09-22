#! /bin/sh

if [ "$#" != "1" ]; then
    echo "usage: $0 file" > /dev/stderr
    return 1
fi

sed -i 's/:\\/;/g' "$1"
sed -i 's/:/\n/g' "$1"
sed -i 's/&/$/g' "$1"
sed -i 's/^/    /g' "$1"
sed -i 's/^    \.L\(.*\)/L\1:/g' "$1"
sed -i 's/ASL A/ASL/g' "$1"
sed -i 's/ROL A/ROL/g' "$1"
sed -i 's/LSR A/LSR/g' "$1"
sed -i 's/ROR A/ROR/g' "$1"
sed -i 's/^    \\/;/g' "$1"
sed -i 's/^    $//g' "$1"
sed -i 's/ASC"\(.\)\"/'\''\1'\''/g' "$1"
