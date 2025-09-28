# BBC BASIC II and III

Build with the [Mad-Assembler](https://github.com/tebe6502/Mad-Assembler).

Currently, the following versions are supported:

| | ROM | ref | Version | Load | MD5|
| --- | --- | --- | --- | --- | --- |
| System | sbasic2.rom    | SBasic2.fixed    | 2.00 | $A000 | c591662ad0f179807e33d41fdf23293a |
| System | sbasic310.rom  | SBasic310.fixed  | 3.10 | $A000 | 477d2c05b550ecb2730814ba1068b0d1 |
| Atom   | atbasic2.rom   | AtBasic2.fixed   | 2.00 | $4000 | baf7820d6507dacc066da793d43c97a7 |
| Atom   | atbasic310.rom | AtBasic310.fixed | 3.10 | $4000 | 5ca195f247cd1dabb311f2207220df2b |
| BBC    | basic2.rom     | Basic2     | 2.00 | $8000 | 2cc67be4624df4dc66617742571a8e3d |
| BBC    | basic3.rom     | Basic 3    | 3.00 | $8000 | 361148f2ae1cb2c87885bcb463d9e74c |
| BBC    | basic310hi.rom | HiBasic310 | 3.10 | $B800 | 68e79c8b6f46aa4f07a6dd687897229c |
| C64    | c64basic2.rom  | C64Basic2.fixed  | 2.00 | $B800 | 89f5b82721cb351f22145ee0c07530c2 |

Note: the fixed reference ROMs contain the proper floating point value for 5.00000 in the FEXPCO table.
See [this commit](https://github.com/ivop/bbc-basic/commit/5a7d6ff7deaeb792381d46ab6004b1abc9a0d855) for details.

### Credits

BBC BASIC Copyright © 1982,1983 Acorn Computer and Roger Wilson  
[ARM Basic 65 source reconstruction](https://mdfs.net/Software/BBCBasic/6502/) and commentary, Copyright © 2018 J.G.Harston  
Conversion to mads, improvements and bug fixes by Ivo van Poorten, 2025
