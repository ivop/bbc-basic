#include <stdio.h>
#include <math.h>

int main(int argc, char **argv) {
    double x;
    double N = 42;

    x = N;
    for (int i=0; i<6; i++)
        x = 0.5 * (x + N/x);

    printf("basic: sqrt(%.7f) = %0.7f\n", N, x);
    printf("math.h: sqrt(%.7f) = %0.7f\n", N, sqrt(N));
}
