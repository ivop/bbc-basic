#include <stdio.h>
#include <math.h>
#include "fcf.h"

static double c[] = {
    -8.6821404509246349,
    9.6715652160346508,
    11.4544274024665356,
    -3.3333338461816311,
    -6.0000000093132257,
    1.0000000000000000
};

// check only one quadrant

int main(int argc, char **argv) {
    double x = 0.5;
    double fwsd, fwsa;
    int cnt = sizeof(c)/sizeof(double);

    fwsd = x * x;
    fwsa = fcf(sizeof(c)/sizeof(double), c, fwsd);

    fwsa *= x;

    printf("fcf: sin(%.8f) = %.8f\n", x, fwsa);

    printf("mathlib: sin(%.8f) = %.8f\n", x, sin(x));
}
