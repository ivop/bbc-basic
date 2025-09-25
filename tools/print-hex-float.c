#include <stdio.h>
#include <stdint.h>
#include <math.h>

static void print_float(uint8_t *hex) {
    int mantissa = (hex[4] + (hex[3]<<8) + (hex[2]<<16) + (hex[1]<<24))
                                                            & 0x7fffffff;
    int sign = hex[1] >> 7;
    int exponent = hex[0];

    double value = (1.0 - sign*2) * (1.0 + ((double)mantissa / 0x80000000)) *
                                                        pow(2,exponent-129);

    printf("%.8f\n", value);
}

int main(int argc, char **argv) {
    uint8_t table[][5] = {
        { 0x81, 0, 0, 0, 0 }
    };

    for (int i=0; i<sizeof(table)/5; i++)
        print_float(table[i]);
}
