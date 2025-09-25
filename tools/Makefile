CC=gcc

all: genmnem print-hex-float

genmnem: genmnem.c
	$(CC) -o $@ $<

print-hex-float: print-hex-float.c
	$(CC) -o $@ $< -lm

clean:
	rm -f genmnem print-hex-float

cleaner: clean
	rm -f *~
