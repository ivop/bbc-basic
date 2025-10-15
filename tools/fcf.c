static double fcf(int cnt, double *c, double fwsd) {
    double fwsa = 0.0;
    for (int i=0; i<cnt; i++) {
        fwsa = c[i] + fwsa;
        if (i != cnt-1)
            fwsa = fwsd / fwsa;
    }
    return fwsa;
}

