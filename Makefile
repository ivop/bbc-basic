
MADS=mads

basic2.rom: basic.s
	$(MADS) -l:basic2.lst -d:BUILD_BBC_BASIC2=1 -o:$@ $<

basic3.rom: basic.s
	$(MADS) -l:basic3.lst -d:BUILD_BBC_BASIC3=1 -o:$@ $<

compare2: basic2.rom
	@tools/compare.sh original/Basic2 $<

compare3: basic3.rom
	@tools/compare.sh original/Basic3 $<

compare: compare2 compare3

clean:
	rm -f basic2.rom basic3.rom *.lst

cleaner: clean
	rm -f *~ */*~
