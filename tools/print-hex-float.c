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

    printf("%.16f\n", value);
}

#if 1
// FLOGTC
static uint8_t table[][5] = {
    { 0x7a, 0x12, 0x38, 0xa5, 0x0B },
    { 0x88, 0x79, 0x0E, 0x9f, 0xf3 },
    { 0x7c, 0x2a, 0xac, 0x3f, 0xb5 },
    { 0x86, 0x34, 0x01, 0xa2, 0x7a },
    { 0x7f, 0x63, 0x8e, 0x37, 0xec },
    { 0x82, 0x3f, 0xff, 0xff, 0xc1 },
    { 0x7f ,0xff, 0xff, 0xff, 0xff }
};
#endif

int main(int argc, char **argv) {
    for (int i=0; i<sizeof(table)/5; i++)
        print_float(table[i]);
}
