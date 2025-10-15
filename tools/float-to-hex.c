#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

static void print_hex(double x) {
    double y = x;
    bool neg = x < 0;
    if (neg) x = -x;

    int exp = 0;

    while (x > 1.0) x /= 2.0, exp++;
    while (x > 0.0 && x < 0.5) x *= 2.0, exp--;
    exp += 0x80;

    uint32_t m = (uint64_t)(round(x * 0x100000000LL)) & 0x7fffffff;
    if (neg) m |= 0x80000000;

    printf("    dta $%02x, $%02x, $%02x, $%02x, $%02x         ; %.16f\n", exp, m>>24, (m>>16)&0xff, (m>>8)&0xff, m&0xff, y);
}

#if 1
// FLOGTC
static double table[] = {
    0.0089246379611723,
    249.0571281313896179,
    0.0416681755596073,
    45.0015963613986969,
    0.4444444156251848,
    2.9999999413266778,
    -0.4999999998835847
};
#endif

int main(int argc, char **argv) {
    int len = sizeof(table) / sizeof(double);
    printf("    dta $%02x                             ; length -1\n", len-1);
    for (int i=0; i<len; i++)
        print_hex(table[i]);
}
