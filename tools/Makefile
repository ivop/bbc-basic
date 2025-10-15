CC=gcc

all: genmnem print-hex-float ln sqrt float-to-hex atn sin

genmnem: genmnem.c
	$(CC) -o $@ $<

print-hex-float: print-hex-float.c
	$(CC) -o $@ $< -lm

float-to-hex: float-to-hex.c
	$(CC) -o $@ $< -lm

sqrt: sqrt.c
	$(CC) -o $@ $< -lm

ln: ln.c fcf.c
	$(CC) -o $@ $^ -lm

atn: atn.c fcf.c
	$(CC) -o $@ $^ -lm

sin: sin.c fcf.c
	$(CC) -o $@ $^ -lm

clean:
	rm -f genmnem print-hex-float ln sqrt float-to-hex atn sin

cleaner: clean
	rm -f *~
