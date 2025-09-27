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
zpLISTPO  = zp + $1f    ; LISTO Flag
zpTRFLAG  = zp + $20    ; TRACE Flag
zpTRNUM   = zp + $21    ; word, maximum trace line number
zpWIDTHV  = zp + $23    ; WIDTH, as set by WIDTH command
zpDOSTKP  = zp + $24    ; DO Stack Pointer
zpSUBSTP  = zp + $25    ; SUB Stack Pointer
zpFORSTP  = zp + $26    ; FOR Stack Pointer
zpTYPE    = zp + $27    ; Variable type
zpBYTESM  = zp + $28    ; OPT Flag
zpOPCODE  = zp + $29    ; Index into opcode table
zpIACC = zp + $2a       ; Integer Accumulator (32-bits)
zp2E = zp + $2e
zp2F = zp + $2f
zp30 = zp + $30
zp31 = zp + $31
zp32 = zp + $32
zp33 = zp + $33
zp34 = zp + $34
zp35 = zp + $35
zp36 = zp + $36
zp37 = zp + $37
zp38 = zp + $38
zp39 = zp + $39
zp3A = zp + $3a
zp3B = zp + $3b
zp3C = zp + $3c
zp3D = zp + $3d
zp3E = zp + $3e
zp3F = zp + $3f
zp40 = zp + $40
zp41 = zp + $41
zp42 = zp + $42
zp43 = zp + $43
zp44 = zp + $44
zp45 = zp + $45
zp46 = zp + $46
zp47 = zp + $47
zp48 = zp + $48
zp49 = zp + $49
zp4A = zp + $4a
zp4B = zp + $4b
zp4C = zp + $4c
zpCOEFP = zp + $4d      ; ptr
zpFDIGS = zpCOEFP+1     ; alternative usage
zpFPRTWN = zpCOEFP+1    ; another alternative usage
zp4F = zp + $4f

; Workspace (for now, relative to ws)

VARL    = ws + $0400    ; VARiable List of resident integer variables
                        ; 4 bytes each [$0400-$046b] [@A-Z]

PC      = VARL + $40    ; P%, program counter

FWSA    = VARL + $69    ; FP WorkSpace temporary A, 5 bytes
FWSB    = VARL + $71    ; FP WorkSpace temporary B, 5 bytes
FWSC    = VARL + $76    ; FP WorkSpace temporary C, 5 bytes
FWSD    = VARL + $7b    ; FP WorkSpace temporary D, 5 bytes

; Stacks

;CmosBasic, might be different for BASIC II/III
;DOADL   = ws + $0500
;DOADH   = DOADL + $14
;
;FORINL  = ws + $0528
;FORINH  = FORINL + 1
;FORINT  = FORINH + 1
;FORSPL  = FORINT + 1
;FORSPM  = FORSPL + 1
;FORSPN  = FORSPM + 1
;FORSPH  = FORSPN + 1
;FORSPE  = FORSPH + 1
;FORLML  = FORSPE + 1
;FORLMM  = FORLML + 1
;FORLMN  = FORLMM + 1
;FORLMH  = FORLMN + 1
;FORLME  = FORLMH + 1
;FORADL  = FORLME + 1
;FORADH  = FORADL + 1
;
;SUBADL  = ws + $05cc
;SUBADH  = SUBADL + $1a

; String Work Area

STRACC  = ws + $0600

; BASIC Line Input Buffer

BUFFER  = $0700
