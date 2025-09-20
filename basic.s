; Basic2/src
; Source for 6502 BASIC II/III
;
; BBC BASIC Copyright (C) 1982/1983 Acorn Computer and Roger Wilson
; Source reconstruction and commentary Copyright (C) J.G.Harston
;
; Conversion to MADS by Ivo van Poorten, September 2025

    opt h-

TARGET_BBC = 1
MOS_BBC    = 1
VERSION    = 2

load    = $8000         ; Code start address
ws      = $0400-$0400   ; Offset from &400 to workspace
membot  = 0             ; Use OSBYTE to find memory limits
memtop  = 0             ; ...
zp      = $00           ; Start of ZP addresses

    icl 'zp.s'          ; All zp aliases 00-5f relative to 'zp'

; zp00 and zp11         ; LOMEM in normal location

zpfd = $fd
zpfe = $fe
zpff = $ff

FAULT  = zpfd           ; Pointer to error block
ESCFLG = zpff           ; Escape pending flag

F_LOAD  = zp39          ; LOAD/SAVE control block
F_EXEC  = F_LOAD+4
F_START = F_LOAD+8
F_END   = F_LOAD+12

; MOS Entry Points

OS_CLI = $FFF7
OSBYTE = $FFF4
OSWORD = $FFF1
OSWRCH = $FFEE
OSWRCR = $FFEC
OSNEWL = $FFE7
OSASCI = $FFE3
OSRDCH = $FFE0
OSFILE = $FFDD
OSARGS = $FFDA
OSBGET = $FFD7
OSBPUT = $FFD4
OSGBPB = $FFD1
OSFIND = $FFCE
BRKV   = $202
WRCHV  = $020E

; Dummy variables for non-Atom code

OSECHO = 0
OSLOAD = 0
OSSAVE = 0
OSRDAR = 0
OSSTAR = 0
OSSHUT = 0

; BASIC Token Values

tknAND      = $80
tknDIV      = $81
tknEOR      = $82
tknMOD      = $83
tknOR       = $84
tknERROR    = $85
tknLINE     = $86
tknOFF      = $87
tknSTEP     = $88
tknSPC      = $89
tknTAB      = $8A
tknELSE     = $8B
tknTHEN     = $8C
tknERL      = $9E
tknEXP      = $A1
tknEXT      = $A2
tknFN       = $A4
tknLOG      = $AB
tknTO       = $B8
tknAUTO     = $C6
tknPTRc     = $CF
tknDATA     = $DC
tknDEF      = $DD
tknRENUMBER = $CC
tknDIM      = $DE
tknEND      = $E0
tknFOR      = $E3
tknGOSUB    = $E4
tknGOTO     = $E5
tknIF       = $E7
tknLOCAL    = $EA
tknMODE     = $EB
tknON       = $EE
tknPRINT    = $F1
tknPROC     = $F2
tknREPEAT   = $F5
tknSTOP     = $FA
tknLOMEM    = $92
tknHIMEM    = $93
tknREPORT   = $F6

; ----------------------------------------------------------------------------

    org load

; ----------------------------------------------------------------------------

; Atom/System Code Header
; =======================

.ifdef MOS_ATOM
    .error
.endif

; BBC Code Header
; ===============

.ifdef MOS_BBC
    cmp #$01            ; Language Entry
    beq L8023
    rts
    nop
    dta $60                         ; ROM type = Lang+Tube+6502 BASIC
    dta copyright_string - load     ; Offset to copyright string
    dta [version*2]-3               ; Version 2 = $01, Version 3 = $03
    dta 'BASIC'
copyright_string:
    dta 0
    dta '(C)198', [$30+version], ' Acorn', 10, 13, 0
    dta a(load), a(0)
.endif

; LANGUAGE STARTUP
; ================

L8023:
    .if memtop = 0
        lda #$84            ; Read top of memory
        jsr OSBYTE
    .elseif memtop > 0
        .error
    .elseif memtop < 0
        .error
    .endif
    stx zp06
    sty zp07

    .if memtop = 0
        lda #$83            ; Read bottom of memory
        jsr OSBYTE
    .elseif memtop > 0
        .error
    .elseif memtop < 0
        .error
    .endif
    sty zp18

