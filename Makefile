MADS=mads

all: sbasic2.rom sbasic310.rom atbasic2.rom atbasic310.rom basic2.rom basic3.rom basic310hi.rom c64basic2.rom

basic.s: vars.s
	touch basic.s

# ----------------------------------------------------------------------------
# Acorn System
#
sbasic2.rom: basic.s
	$(MADS) -d:BUILD_SYSTEM_BASIC2=1 -o:$@ $<

sbasic310.rom: basic.s
	$(MADS) -d:BUILD_SYSTEM_BASIC310=1 -o:$@ $<

# ----------------------------------------------------------------------------
# Acorn Atom
#
atbasic2.rom: basic.s
	$(MADS) -d:BUILD_ATOM_BASIC2=1 -o:$@ $<

atbasic310.rom: basic.s
	$(MADS) -d:BUILD_ATOM_BASIC310=1 -o:$@ $<

# ----------------------------------------------------------------------------
# Acorn BBC Micro
#
basic2.rom: basic.s
	$(MADS) -d:BUILD_BBC_BASIC2=1 -o:$@ $<

basic3.rom: basic.s
	$(MADS) -d:BUILD_BBC_BASIC3=1 -o:$@ $<

basic310hi.rom: basic.s
	$(MADS) -d:BUILD_BBC_BASIC310HI=1 -o:$@ $<

# ----------------------------------------------------------------------------
# Commodore 64
#
c64basic2.rom: basic.s
	$(MADS) -d:BUILD_C64_BASIC2=1 -o:$@ $<

# ----------------------------------------------------------------------------
#  Compare with reference ROMs
#
compares2: sbasic2.rom
	@tools/compare.sh ref/SBasic2.fixed2 $<

compares310: sbasic310.rom
	@tools/compare.sh ref/SBasic310.fixed $<

compareat2: atbasic2.rom
	@tools/compare.sh ref/AtBasic2.fixed2 $<

compareat310: atbasic310.rom
	@tools/compare.sh ref/AtBasic310.fixed $<

compare2: basic2.rom
	@tools/compare.sh ref/Basic2 $<

compare3: basic3.rom
	@tools/compare.sh ref/Basic3 $<

compare310hi: basic310hi.rom
	@tools/compare.sh ref/HiBasic310 $<

comparec642: c64basic2.rom
	@tools/compare.sh ref/C64Basic2.fixed $<

compare: compare2 compare3 compare310hi compares2 compares310 compareat2 compareat310 comparec642

# ----------------------------------------------------------------------------
#  Clean up
#
clean:
	rm -f *.rom *.lst

cleaner: clean
	rm -f *~ */*~
