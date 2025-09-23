
MADS=mads

basic2.rom: basic.s
	$(MADS) -l:basic2.lst -d:BUILD_BBC_BASIC2=1 -o:$@ $<

basic3.rom: basic.s
	$(MADS) -l:basic3.lst -d:BUILD_BBC_BASIC3=1 -o:$@ $<

basic310hi.rom: basic.s
	$(MADS) -l:basic310hi.lst -d:BUILD_BBC_BASIC310HI=1 -o:$@ $<

compare2: basic2.rom
	@tools/compare.sh original/Basic2 $<

compare3: basic3.rom
	@tools/compare.sh original/Basic3 $<

compare310hi: basic310hi.rom
	@tools/compare.sh original/HiBasic310 $<

compare: compare2 compare3 compare310hi

clean:
	rm -f *.rom *.lst

cleaner: clean
	rm -f *~ */*~
