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

    while (x >= 1.0) x /= 2.0, exp++;
    while (x > 0.0 && x < 0.5) x *= 2.0, exp--;
    exp += 0x80;

    uint32_t m = (uint64_t)(round(x * 0x100000000LL)) & 0x7fffffff;
    if (neg) m |= 0x80000000;

    printf("    dta $%02x, $%02x, $%02x, $%02x, $%02x         ; %.16f\n", exp, m>>24, (m>>16)&0xff, (m>>8)&0xff, m&0xff, y);
}

#if 0
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

#if 0
// FATANC
static double table[] = {
    -20.4189003035426140,
    0.6117710596881807,
    0.8427043480332941,
    -0.7914132184814662,
    -8.7958473488688469,
    -1.1686409553512931,
    1.0842109108343720,
    0.4954648205311969,
    -3.9278772315010428,
    0.9272952182218432
};
#endif

#if 0
// FSINC
static double table[] = {
    -8.6821404509246349,
    9.6715652160346508,
    11.4544274024665356,
    -3.3333338461816311,
    -6.0000000093132257,
    1.0000000000000000
};
#endif

#if 1
// HPIHI, HPILO, HALFPI, FPIs18, F180sP, RPLN10
static double table[] = {
    -1.5708007812500000,
    0.0000044544551105,
    1.5707963267341256,
    0.0174532925157109,
    57.2957795113325119,
    0.4342944819945842
};
#endif

int main(int argc, char **argv) {
    int len = sizeof(table) / sizeof(double);
    printf("    dta $%02x                             ; length -1\n", len-1);
    for (int i=0; i<len; i++)
        print_hex(table[i]);
}
