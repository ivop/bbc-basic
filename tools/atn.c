#include <stdio.h>
#include <math.h>
#include "fcf.h"

static double c[10] = {
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

// only the FCF path for 0.0001 < x <0 1.0

int main(int argc, char **argv) {
    double x = 0.5;
    double fwsd, fwsa;
    int cnt = sizeof(c)/sizeof(double);

    fwsd = x - 0.5;
    fwsa = fcf(sizeof(c)/sizeof(double), c, fwsd);

    fwsa *= x;

    printf("fcf: atn(%.8f) = %.8f\n", x, fwsa);

    printf("mathlib: atan(%.8f) = %.8f\n", x, atan(x));
}
