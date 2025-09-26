#! /bin/sh

if [ "$#" != "1" ]; then
    echo "usage: $0 filename" >&2
    exit 1
fi

for i in ADC AND ASL BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS CLC CLD CLI CLV CMP CPX CPY DEC DEX DEY EOR INC INX INY JMP JSR LDA LDX LDY LSR NOP ORA PHA PHP PLA PLP ROL ROR RTI RTS SBC SEC SED SEI STA STX STY TAX TAY TSX TXA TXS TYA ; do
    j=`echo "$i" | tr '[A-Z]' '[a-z]'`
    sed -i 's/^\( \+\)'"$i"'\(.*$\)/\1'"$j"'\2/g' "$1"
done
sed -i 's/brkV/BRKV/g' "$1"
