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
    .if memtop == 0
        lda #$84            ; Read top of memory
        jsr OSBYTE
    .elseif memtop > 0
        .error
    .elseif memtop < 0
        .error
    .endif
    stx zp06
    sty zp07

    .if memtop == 0
        lda #$83            ; Read bottom of memory
        jsr OSBYTE
    .elseif memtop > 0
        .error
    .elseif memtop < 0
        .error
    .endif
    sty zp18

    LDX #$00
    STX zp1f                ; Set LISTO to 0
    STX ws+$0402
    STX ws+$0403            ; Set @% to $0000xxxx
    DEX
    STX zp23                ; Set WIDTH to $FF

    LDX #$0A
    STX ws+$0400
    DEX
    STX ws+$0401            ; Set @% to $0000090A

    LDA #$01
    AND zp11
    ORA zp0d                ; Check RND seed
    ORA zp0e
    ORA zp0f
    ORA zp10
    BNE L8063               ; If nonzero, skip past

    LDA #$41                ; Set RND seed to $575241
    STA zp0d
    LDA #$52
    STA zp0e
    LDA #$57
    STA zp0f                ; "ARW" - Acorn Roger Wilson?

L8063:
    LDA #<LB402
    STA BRKV+0              ; Set up error handler
    LDA #>LB402
    STA BRKV+1
    CLI
    JMP L8ADD               ; Enable IRQs, jump to immediate loop

; ----------------------------------------------------------------------------

; TOKEN TABLE
; ===========
; string, token, flags
;
; Token flags:
; Bit 0 - Conditional tokenisation (don't tokenise if followed by an
;                                                   alphabetic character).
; Bit 1 - Go into "middle of Statement" mode.
; Bit 2 - Go into "Start of Statement" mode.
; Bit 3 - FN/PROC keyword - don't tokenise the name of the subroutine.
; Bit 4 - Start tokenising a line number now (after a GOTO, etc...).
; Bit 5 - Don't tokenise rest of line (REM, DATA, etc...)
; Bit 6 - Pseudo variable flag - add &40 to token if at the start of a
;                                                       statement/hex number
; Bit 7 - Unused - used externally for quote toggle.

L8071:
    dta 'AND'     , $80, $00  ; 00000000
    dta 'ABS'     , $94, $00  ; 00000000
    dta 'ACS'     , $95, $00  ; 00000000
    dta 'ADVAL'   , $96, $00  ; 00000000
    dta 'ASC'     , $97, $00  ; 00000000
    dta 'ASN'     , $98, $00  ; 00000000
    dta 'ATN'     , $99, $00  ; 00000000
    dta 'AUTO'    , $C6, $10  ; 00010000
    dta 'BGET'    , $9A, $01  ; 00000001
    dta 'BPUT'    , $D5, $03  ; 00000011
    .if version != 3
        dta 'COLOUR', $FB, $02 ; 00000010
    .elseif version == 3
        dta 'COLOR', $FB, $02 ; 00000010
    .endif
    dta 'CALL'    , $D6, $02  ; 00000010
    dta 'CHAIN'   , $D7, $02  ; 00000010
    dta 'CHR$'    , $BD, $00  ; 00000000
    dta 'CLEAR'   , $D8, $01  ; 00000001
    dta 'CLOSE'   , $D9, $03  ; 00000011
    dta 'CLG'     , $DA, $01  ; 00000001
    dta 'CLS'     , $DB, $01  ; 00000001
    dta 'COS'     , $9B, $00  ; 00000000
    dta 'COUNT'   , $9C, $01  ; 00000001
    .if version == 3
        dta 'COLOUR', $FB, $02 ; 00000010
    .elseif version > 3
        dta 'COLOR', $FB, $02 ; 00000010
    .endif
    dta 'DATA'    , $DC, $20  ; 00100000
    dta 'DEG'     , $9D, $00  ; 00000000
    dta 'DEF'     , $DD, $00  ; 00000000
    dta 'DELETE'  , $C7, $10  ; 00010000
    dta 'DIV'     , $81, $00  ; 00000000
    dta 'DIM'     , $DE, $02  ; 00000010
    dta 'DRAW'    , $DF, $02  ; 00000010
    dta 'ENDPROC' , $E1, $01  ; 00000001
    dta 'END'     , $E0, $01  ; 00000001
    dta 'ENVELOPE', $E2, $02  ; 00000010
    dta 'ELSE'    , $8B, $14  ; 00010100
    dta 'EVAL'    , $A0, $00  ; 00000000
    dta 'ERL'     , $9E, $01  ; 00000001
    dta 'ERROR'   , $85, $04  ; 00000100
    dta 'EOF'     , $C5, $01  ; 00000001
    dta 'EOR'     , $82, $00  ; 00000000
    dta 'ERR'     , $9F, $01  ; 00000001
    dta 'EXP'     , $A1, $00  ; 00000000
    dta 'EXT'     , $A2, $01  ; 00000001
    dta 'FOR'     , $E3, $02  ; 00000010
    dta 'FALSE'   , $A3, $01  ; 00000001
    dta 'FN'      , $A4, $08  ; 00001000
    dta 'GOTO'    , $E5, $12  ; 00010010
    dta 'GET$'    , $BE, $00  ; 00000000
    dta 'GET'     , $A5, $00  ; 00000000
    dta 'GOSUB'   , $E4, $12  ; 00010010
    dta 'GCOL'    , $E6, $02  ; 00000010
    dta 'HIMEM'   , $93, $43  ; 00100011
    dta 'INPUT'   , $E8, $02  ; 00000010
    dta 'IF'      , $E7, $02  ; 00000010
    dta 'INKEY$'  , $BF, $00  ; 00000000
    dta 'INKEY'   , $A6, $00  ; 00000000
    dta 'INT'     , $A8, $00  ; 00000000
    dta 'INSTR('  , $A7, $00  ; 00000000
    dta 'LIST'    , $C9, $10  ; 00010000
    dta 'LINE'    , $86, $00  ; 00000000
    dta 'LOAD'    , $C8, $02  ; 00000010
    dta 'LOMEM'   , $92, $43  ; 01000011
    dta 'LOCAL'   , $EA, $02  ; 00000010
    dta 'LEFT$('  , $C0, $00  ; 00000000
    dta 'LEN'     , $A9, $00  ; 00000000
    dta 'LET'     , $E9, $04  ; 00000100
    dta 'LOG'     , $AB, $00  ; 00000000
    dta 'LN'      , $AA, $00  ; 00000000
    dta 'MID$('   , $C1, $00  ; 00000000
    dta 'MODE'    , $EB, $02  ; 00000010
    dta 'MOD'     , $83, $00  ; 00000000
    dta 'MOVE'    , $EC, $02  ; 00000010
    dta 'NEXT'    , $ED, $02  ; 00000010
    dta 'NEW'     , $CA, $01  ; 00000001
    dta 'NOT'     , $AC, $00  ; 00000000
    dta 'OLD'     , $CB, $01  ; 00000001
    dta 'ON'      , $EE, $02  ; 00000010
    dta 'OFF'     , $87, $00  ; 00000000
    dta 'OR'      , $84, $00  ; 00000000
    dta 'OPENIN'  , $8E, $00  ; 00000000
    dta 'OPENOUT' , $AE, $00  ; 00000000
    dta 'OPENUP'  , $AD, $00  ; 00000000
    dta 'OSCLI'   , $FF, $02  ; 00000010
    dta 'PRINT'   , $F1, $02  ; 00000010
    dta 'PAGE'    , $90, $43  ; 01000011
    dta 'PTR'     , $8F, $43  ; 01000011
    dta 'PI'      , $AF, $01  ; 00000001
    dta 'PLOT'    , $F0, $02  ; 00000010
    dta 'POINT('  , $B0, $00  ; 00000000
    dta 'PROC'    , $F2, $0A  ; 00001010
    dta 'POS'     , $B1, $01  ; 00000001
    dta 'RETURN'  , $F8, $01  ; 00000001
    dta 'REPEAT'  , $F5, $00  ; 00000000
    dta 'REPORT'  , $F6, $01  ; 00000001
    dta 'READ'    , $F3, $02  ; 00000010
    dta 'REM'     , $F4, $20  ; 00100000
    dta 'RUN'     , $F9, $01  ; 00000001
    dta 'RAD'     , $B2, $00  ; 00000000
    dta 'RESTORE' , $F7, $12  ; 00010010
    dta 'RIGHT$(' , $C2, $00  ; 00000000
    dta 'RND'     , $B3, $01  ; 00000001
    dta 'RENUMBER', $CC, $10  ; 00010000
    dta 'STEP'    , $88, $00  ; 00000000
    dta 'SAVE'    , $CD, $02  ; 00000010
    dta 'SGN'     , $B4, $00  ; 00000000
    dta 'SIN'     , $B5, $00  ; 00000000
    dta 'SQR'     , $B6, $00  ; 00000000
    dta 'SPC'     , $89, $00  ; 00000000
    dta 'STR$'    , $C3, $00  ; 00000000
    dta 'STRING$(', $C4, $00  ; 00000000
    dta 'SOUND'   , $D4, $02  ; 00000010
    dta 'STOP'    , $FA, $01  ; 00000001
    dta 'TAN'     , $B7, $00  ; 00000000
    dta 'THEN'    , $8C, $14  ; 00010100
    dta 'TO'      , $B8, $00  ; 00000000
    dta 'TAB('    , $8A, $00  ; 00000000
    dta 'TRACE'   , $FC, $12  ; 00010010
    dta 'TIME'    , $91, $43  ; 01000011
    dta 'TRUE'    , $B9, $01  ; 00000001
    dta 'UNTIL'   , $FD, $02  ; 00000010
    dta 'USR'     , $BA, $00  ; 00000000
    dta 'VDU'     , $EF, $02  ; 00000010
    dta 'VAL'     , $BB, $00  ; 00000000
    dta 'VPOS'    , $BC, $01  ; 00000001
    dta 'WIDTH'   , $FE, $02  ; 00000010
    dta 'PAGE'    , $D0, $00  ; 00000000
    dta 'PTR'     , $CF, $00  ; 00000000
    dta 'TIME'    , $D1, $00  ; 00000000
    dta 'LOMEM'   , $D2, $00  ; 00000000
    dta 'HIMEM'   , $D3, $00  ; 00000000

; ----------------------------------------------------------------------------

; FUNCTION/COMMAND DISPATCH TABLE, ADDRESS LOW BYTES
; ==================================================

; ----------------------------------------------------------------------------

; Temporary labels to make assembler happy

L8ADD=$8add
LB402=$b402
