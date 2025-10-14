CC=gcc

all: genmnem print-hex-float ln sqrt

genmnem: genmnem.c
	$(CC) -o $@ $<

print-hex-float: print-hex-float.c
	$(CC) -o $@ $< -lm

sqrt: sqrt.c
	$(CC) -o $@ $< -lm

ln: ln.c
	$(CC) -o $@ $< -lm

clean:
	rm -f genmnem print-hex-float ln sqrt

cleaner: clean
	rm -f *~
