MADS=mads

# ----------------------------------------------------------------------------
# Acorn System
#
sbasic2.rom: basic.s
	$(MADS) -l:sbasic2.lst -d:BUILD_SYSTEM_BASIC2=1 -o:$@ $<

sbasic310.rom: basic.s
	$(MADS) -l:sbasic3hi.lst -d:BUILD_SYSTEM_BASIC310=1 -o:$@ $<

# ----------------------------------------------------------------------------
# Acorn BBC Micro
#
basic2.rom: basic.s
	$(MADS) -l:basic2.lst -d:BUILD_BBC_BASIC2=1 -o:$@ $<

basic3.rom: basic.s
	$(MADS) -l:basic3.lst -d:BUILD_BBC_BASIC3=1 -o:$@ $<

basic310hi.rom: basic.s
	$(MADS) -l:basic310hi.lst -d:BUILD_BBC_BASIC310HI=1 -o:$@ $<

# ----------------------------------------------------------------------------
#  Compare with reference ROMs
#
compares2: sbasic2.rom
	@tools/compare.sh ref/SBasic2 $<

compares310: sbasic310.rom
	@tools/compare.sh ref/SBasic3 $<

compare2: basic2.rom
	@tools/compare.sh ref/Basic2 $<

compare3: basic3.rom
	@tools/compare.sh ref/Basic3 $<

compare310hi: basic310hi.rom
	@tools/compare.sh ref/HiBasic310 $<

compare: compare2 compare3 compare310hi compares2 compares310

# ----------------------------------------------------------------------------
#  Clean up
#
clean:
	rm -f *.rom *.lst

cleaner: clean
	rm -f *~ */*~
