# BBC BASIC II and III

This is a fully annotated disassembly of BBC BASIC II and III.

Build with the [Mad-Assembler](https://github.com/tebe6502/Mad-Assembler).

[Run BBC BASIC](https://github.com/ivop/run-bbc-basic/) on Linux!

Or on an [Atari XL/XE](https://github.com/ivop/atari-bbc-basic)!

Currently, the following versions are supported:

| | ROM | ref | Version | Load | MD5|
| --- | --- | --- | --- | --- | --- |
| System | sbasic2.rom    | SBasic2.fixed2    | 2.00 | $A000 | 74c803820eb39ff8b693ea3a90f05b33 |
| System | sbasic310.rom  | SBasic310.fixed  | 3.10 | $A000 | 477d2c05b550ecb2730814ba1068b0d1 |
| Atom   | atbasic2.rom   | AtBasic2.fixed2   | 2.00 | $4000 | 07936440e266d780d4082f2844aa78e4 |
| Atom   | atbasic310.rom | AtBasic310.fixed | 3.10 | $4000 | 5ca195f247cd1dabb311f2207220df2b |
| BBC    | basic2.rom     | Basic2     | 2.00 | $8000 | 2cc67be4624df4dc66617742571a8e3d |
| BBC    | basic3.rom     | Basic 3    | 3.00 | $8000 | 361148f2ae1cb2c87885bcb463d9e74c |
| BBC    | basic310hi.rom | HiBasic310 | 3.10 | $B800 | 68e79c8b6f46aa4f07a6dd687897229c |
| C64    | c64basic2.rom  | C64Basic2.fixed  | 2.00 | $B800 | 89f5b82721cb351f22145ee0c07530c2 |

Notes:
* The fixed reference ROMs contain the proper floating point value for 5.00000 in the FEXPCO table.
See [this commit](https://github.com/ivop/bbc-basic/commit/5a7d6ff7deaeb792381d46ab6004b1abc9a0d855) at line 7481 for details.
* The fixed2 reference ROMs contain previous fix, and a fix in the INSTR instruction that branched to the wrong
memory location on error. See [this commit](https://github.com/ivop/bbc-basic/commit/d0676f8ba5c34023562e5f5bba92a48514324571)
for details.

### Credits

Conversion to mads, improvements, lots of comments, and bug fixes by Ivo van Poorten, 2025  
[ARM Basic 65 source reconstruction](https://mdfs.net/Software/BBCBasic/6502/) and commentary, Copyright © 2018 J.G.Harston  
BBC BASIC Copyright © 1982,1983 Acorn Computer and Sophie Wilson  
[The BBC Micro Compendium](https://archive.org/search?query=bbc+micro+compendium) by Jeremy Ruston, © 1983  
Thanks to Paul Fellows for releasing the original [CmosBasic](https://github.com/stardot/AcornCmosBasic),
[DmosBasic](https://github.com/stardot/AcornDmosBasic), and [Basic128]( https://github.com/stardot/AcornBasic128) sources.  
