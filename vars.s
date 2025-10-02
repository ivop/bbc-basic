; Zero Page

.ifndef zpLOMEM
    zpLOMEM = zp + $00  ; ptr
.endif
zpFSA     = zp + $02    ; ptr, VARTOP, end of BASIC variables
zpAESTKP  = zp + $04    ; ptr, BASIC Stack Pointer
zpHIMEM   = zp + $06    ; ptr
zpERL     = zp + $08    ; ptr, address of instruction that errored
zpCURSOR  = zp + $0a    ;      BASIC Text Pointer Offset
zpLINE    = zp + $0b    ; ptr, BASIC Text Pointer
zpSEED    = zp + $0d    ; RND work area (5 bytes)
zpTOP     = zp + $12    ; ptr, end of BASIC program excluding variables
zpPRINTS  = zp + $14    ; number of bytes in a print output field
zpPRINTF  = zp + $15    ; PRINT Flag
zpERRORLH = zp + $16    ; ptr, error routine vector
zpTXTP    = zp + $18    ; _page_ number where BASIC program starts
zpAELINE  = zp + $19    ; ptr, secondary BASIC Text Pointer (sim. zpLINE)
zpAECUR   = zp + $1b    ; secondary BASIC Text Pointer Offset (sim. zpCURSOR)
zpDATAP   = zp + $1c    ; ptr, BASIC program start
zpTALLY   = zp + $1e    ; counter, number of bytes printed since last newline
zpLISTOP  = zp + $1f    ; LISTO Flag
zpTRFLAG  = zp + $20    ; TRACE Flag
zpTRNUM   = zp + $21    ; word, maximum trace line number
zpWIDTHV  = zp + $23    ; WIDTH, as set by WIDTH command
zpDOSTKP  = zp + $24    ; DO Stack Pointer
zpSUBSTP  = zp + $25    ; SUB Stack Pointer
zpFORSTP  = zp + $26    ; FOR Stack Pointer
zpTYPE    = zp + $27    ; Variable type
zpBYTESM  = zp + $28    ; OPT Flag
zpOPCODE  = zp + $29    ; Index into opcode table
zpIACC    = zp + $2a    ; Integer Accumulator (32-bits)
zpFACCS   = zp + $2e    ; Floating Point Accumulator, SIGN
zpFACCXH  = zp + $2f    ; Floating Point Accumulator, OVER/UNDERFLOW
zpFACCX   = zp + $30    ; Floating Point Accumulator, EXPONENT
zpFACCMA  = zp + $31    ; Floating Point Accumulator, MANTISSA
zpFACCMB  = zp + $32    ; Floating Point Accumulator, MANTISSA
zpFACCMC  = zp + $33    ; Floating Point Accumulator, MANTISSA
zpFACCMD  = zp + $34    ; Floating Point Accumulator, MANTISSA
zpFACCMG  = zp + $35    ; Floating Point Accumulator, ROUNDING
zpCLEN    = zp + $36    ; Length of string buffer
zpWORK    = zp + $37    ; General work area

; WORK+x seem to match Cmos, not Basic128 (is one off sometimes)
; remove this list once all zp38-zp4a are replaced with WORK or FWRK refs.
zp38 = zp + $38 ; WORK+1    ok.
zp39 = zp + $39 ; WORK+2    ok.
zp3A = zp + $3a ; WORK+3    ok.
zp3B = zp + $3b ; WORK+4    ok.
zp3C = zp + $3c ; WORK+5    ok.
zp3D = zp + $3d ; WORK+6    ok.
zp3E = zp + $3e ; WORK+7    ok.
zp3F = zp + $3f ; WORK+8    ok.
zp40 = zp + $40 ; WORK+9    ok
zp41 = zp + $41 ; WORK+10   ok
zp42 = zp + $42 ; WORK+11   ok
zp43 = zp + $43 ; WORK+12
zp44 = zp + $44 ; WORK+13   ok.
zp45 = zp + $45 ; WORK+14
zp46 = zp + $46 ; WORK+15
zp47 = zp + $47 ; WORK+16
zp48 = zp + $48 ; WORK+17
zp49 = zp + $49 ; WORK+18
zp4A = zp + $4a ; WORK+19

; zp3B onwards are also used as zpWORK+4 etc..., need manual replace!

zpFWRKS   = zp + $3b    ; Work Floating Point Accumulator, SIGN
zpFWRKXH  = zp + $3c    ; Work Floating Point Accumulator, OVER/UNDERFLOW
zpFWRKX   = zp + $3d    ; Work Floating Point Accumulator, EXPONENT
zpFWRKMA  = zp + $3e    ; Work Floating Point Accumulator, MANTISSA
zpFWRKMB  = zp + $3f    ; Work Floating Point Accumulator, MANTISSA
zpFWRKMC  = zp + $40    ; Work Floating Point Accumulator, MANTISSA
zpFWRKMD  = zp + $41    ; Work Floating Point Accumulator, MANTISSA
zpFWRKMG  = zp + $42    ; Work Floating Point Accumulator, ROUNDING

zpFTMPMA  = zp + $43    ; Temp Floating Point Accumulator, MANTISSA
zpFTMPMB  = zp + $44    ; Temp Floating Point Accumulator, MANTISSA
zpFTMPMC  = zp + $45    ; Temp Floating Point Accumulator, MANTISSA
zpFTMPMD  = zp + $46    ; Temp Floating Point Accumulator, MANTISSA
zpFTMPMG  = zp + $47    ; Temp Floating Point Accumulator, ROUNDING

zpFRDDDP  = zp + $48    ; Decimal Point flag
zpFRDDDX  = zp + $49    ; Exponent
zpFPRTDX  = zp + $49    ; Alt.
zpFRDDW   = zp + $4a
zpFQUAD   = zp + $4a    ; Alt. Quadrant

zpARGP    = zp + $4b
zpCOEFP   = zp + $4d    ; ptr
zpFDIGS   = zp + $4e    ; alternative usage (overlaps with COEFP+1
zpFPRTWN  = zp + $4e    ; another alternative usage (idem)
zpNEWVAR  = zp + $4f
zp4F      = zp + $4f

; ----------------------------------------------------------------------------

; Workspace (for now, relative to ws)

WORKSPACE = ws

VARL    = ws + $0400    ; VARiable List of resident integer variables
                        ; 4 bytes each [$0400-$046b] [@A-Z]

VARL_AT = VARL          ; @%
VARL_A  = VARL + $04    ; A%
VARL_C  = VARL + $0c    ; C%
VARL_O  = VARL + $3c    ; O%
VARL_P  = VARL + $40    ; P%
VARL_X  = VARL + $60    ; X%
VARL_Y  = VARL + $64    ; Y%

PC      = VARL_P        ; Program Counter

FWSA    = VARL + $6c    ; FP WorkSpace temporary A, 5 bytes
FWSB    = VARL + $71    ; FP WorkSpace temporary B, 5 bytes
FWSC    = VARL + $76    ; FP WorkSpace temporary C, 5 bytes
FWSD    = VARL + $7b    ; FP WorkSpace temporary D, 5 bytes

VARPTR  = ws + $0480    ; Variable Pointer Table

; Stacks

; FOR Stack, 15 entries per frame

FORINL  = ws + $0500
FORINH  = FORINL + 1
FORINT  = FORINH + 1
FORSPL  = FORINT + 1
FORSPM  = FORSPL + 1
FORSPN  = FORSPM + 1
FORSPH  = FORSPN + 1
FORSPE  = FORSPH + 1
FORLML  = FORSPE + 1
FORLMM  = FORLML + 1
FORLMN  = FORLMM + 1
FORLMH  = FORLMN + 1
FORLME  = FORLMH + 1
FORADL  = FORLME + 1
FORADH  = FORADL + 1

; (there's a hole between offset cFORTOP and DOADL)

DOADL   = ws + $05a4        ; $14 bytes
DOADH   = DOADL + $14       ; $14 bytes

SUBADL  = ws + $05cc        ; $1a bytes
SUBADH  = SUBADL + $1a      ; $1a bytes

; String Work Area

STRACC  = ws + $0600

; BASIC Line Input Buffer

BUFFER  = ws + $0700

; ----------------------------------------------------------------------------

; Constants

cFORTOP = $96       ; 10 frames
cSUBTOP = $1a       ; 26 entries
cDOTOP  = $14       ; 20 entries

