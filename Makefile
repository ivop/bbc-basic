
MADS=mads

basic.rom: basic.s
	$(MADS) -o:$@ $<

compare: basic.rom
	./compare.sh

clean:
	rm -f basic.rom

cleaner: clean
	rm -f *~
