#include <stdio.h>
#include <math.h>

static double c[7] = {
    0.00892464,
    249.05712813,
    0.04166818,
    45.00159636,
    0.44444442,
    2.99999994,
    -0.50000000
};

#include "fcf.c"

int main(int argc, char **argv) {
    double x = 2.42;
    double fwsd, fwsa;
    int cnt = sizeof(c)/sizeof(double);

    fwsd = x - 1;

    fwsa = fcf(sizeof(c)/sizeof(double), c, fwsd);

    fwsa *= fwsd;
    fwsa *= fwsd;
    fwsa += fwsd;

    printf("fcf: ln(%.8f) = %.8f\n", x, fwsa);

    printf("mathlib: ln(%.8f) = %.8f\n", x, log(x));
}
