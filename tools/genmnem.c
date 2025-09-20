#include <stdio.h>

static char *mnemonics[] = {
    "BRK", "CLC", "CLD", "CLI", "CLV", "DEX", "DEY", "INX", "INY", "NOP",
    "PHA", "PHP", "PLA", "PLP", "RTI", "RTS", "SEC", "SED", "SEI", "TAX",
    "TAY", "TSX", "TXA", "TXS", "TYA", "BCC", "BCS", "BEQ", "BMI", "BNE",
    "BPL", "BVC", "BVS", "AND", "EOR", "ORA", "ADC", "CMP", "LDA", "SBC",
    "ASL", "LSR", "ROL", "ROR", "DEC", "INC", "CPX", "CPY", "BIT", "JMP",
    "JSR", "LDX", "LDY", "STA", "STX", "STY", "OPT", "EQU", NULL
};

static inline unsigned char mnemL(char *m) {
    return (m[1]<<5) + (m[2]&0x1f);
}

static inline unsigned char mnemH(char *m) {
    return ((m[0]&0x1f)<<2) + ((m[1]&0x1f)>>3);
}

int main(int argc, char **argv) {
    if (argc != 2 || (argv[1][0] != 'L' && argv[1][0] != 'H')) {
        printf("usage: %s L|H\n", argv[0]);
        return 1;
    }
    if (argv[1][0] == 'L') {
        for (int i=0; mnemonics[i]; i++)
            printf("    dta $%02x ; %s\n", mnemL(mnemonics[i]), mnemonics[i]);
    }
    if (argv[1][0] == 'H') {
        for (int i=0; mnemonics[i]; i++)
            printf("    dta $%02x ; %s\n", mnemH(mnemonics[i]), mnemonics[i]);
    }
}
