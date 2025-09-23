
MADS=mads

basic2.rom: basic.s
	$(MADS) -o:$@ $<

compare2: basic2.rom
	@tools/compare.sh original/Basic2 $<

compare: compare2

clean:
	rm -f basic2.rom

cleaner: clean
	rm -f *~ */*~
