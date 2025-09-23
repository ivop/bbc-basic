;
; Source for 6502 BASIC II/III
;
; BBC BASIC Copyright (C) 1982/1983 Acorn Computer and Roger Wilson
; Source reconstruction and commentary Copyright (C) J.G.Harston
;
; Conversion to Mad-Assember (mads) by Ivo van Poorten, September 2025
;

    opt h-              ; No Atari header

TARGET_BBC   = 1
MOS_BBC      = 1
VERSION      = 2
MINORVERSION = 0

version_string = '2'

load    = $8000         ; Code start address
split   = 0
foldup  = 0
title   = 0
ws      = $0400-$0400   ; Offset from &400 to workspace
membot  = 0             ; Use OSBYTE to find memory limits
memtop  = 0             ; ...

    .macro FNfold str
        dta :1
    .endm

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

; FUNCTION/COMMAND DISPATCH TABLE, MACRO
; ======================================

func_table .macro operator
    dta :1LBF78             ; $8E - OPENIN
    dta :1LBF47             ; $8F - PTR
    .if version < 3
        dta :1LAEC0         ; $90 - PAGE
        dta :1LAEB4         ; $91 - TIME
        dta :1LAEFC         ; $92 - LOMEM
        dta :1LAF03         ; $93 - HIMEM
    .elseif version >= 3
        dta :1XAEA7         ; $90 - PAGE
        dta :1LAEB4         ; $91 - TIME
        dta :1XAEFC         ; $92 - LOMEM
        dta :1XAF03         ; $93 - HIMEM
    .endif
    dta :1LAD6A             ; $94 - ABS
    dta :1LA8D4             ; $95 - ACS
    dta :1LAB33             ; $96 - ADVAL
    dta :1LAC9E             ; $97 - ASC
    dta :1LA8DA             ; $98 - ASN
    dta :1LA907             ; $99 - ATN
    dta :1LBF6F             ; $9A - BGET
    dta :1LA98D             ; $9B - COS
    .if version < 3
        dta :1LAEF7         ; $9C - COUNT
    .elseif version >= 3
        dta :1XAEF7         ; $9C - COUNT
    .endif
    dta :1LABC2             ; $9D - DEG
    .if version < 3
        dta :1LAF9F         ; $9E - ERL
        dta :1LAFA6         ; $9F - ERR
    .elseif version >= 3
        dta :1XAF9F         ; $9E - ERL
        dta :1XAFA6         ; $9F - ERR
    .endif
    dta :1LABE9             ; $A0 - EVAL
    dta :1LAA91             ; $A1 - EXP
    dta :1LBF46             ; $A2 - EXT
    .if version < 3
        dta :1LAECA         ; $A3 - FALSE
    .elseif version >= 3
        dta :1LACCD         ; $A3 - FALSE
    .endif
    dta :1LB195             ; $A4 - FN
    dta :1LAFB9             ; $A5 - GET
    dta :1LACAD             ; $A6 - INKEY
    dta :1LACE2             ; $A7 - INSTR(
    dta :1LAC78             ; $A8 - INT
    .if version < 3
        dta :1LAED1         ; $A9 - LEN
    .elseif version >= 3
        dta :1XAECC         ; $A9 - LEN
    .endif
    dta :1LA7FE             ; $AA - LN
    dta :1LABA8             ; $AB - LOG
    .if version < 3
        dta :1LACD1         ; $AC - NOT
    .elseif version >= 3
        dta :1XAB5B         ; $AC - NOT
    .endif
    dta :1LBF80             ; $AD - OPENUP
    dta :1LBF7C             ; $AE - OPENOUT
    dta :1LABCB             ; $AF - PI
    .if version < 3
        dta :1LAB41         ; $B0 - POINT(
    .elseif version >= 3
        dta :1XAB41         ; $B0 - POINT(
    .endif
    dta :1LAB6D             ; $B1 - POS
    dta :1LABB1             ; $B2 - RAD
    dta :1LAF49             ; $B3 - RND
    .if version < 3
        dta :1LAB88         ; $B4 - SGN
    .elseif version >= 3
        dta :1XACAA         ; $B4 - SGN
    .endif
    dta :1LA998             ; $B5 - SIN
    dta :1LA7B4             ; $B6 - SQR
    dta :1LA6BE             ; $B7 - TAN
    .if version < 3
        dta :1LAEDC         ; $B8 - TO
    .elseif version >= 3
        dta :1XAEA6         ; $B8 - TO
    .endif
    dta :1LACC4             ; $B9 - TRUE
    dta :1LABD2             ; $BA - USR
    dta :1LAC2F             ; $BB - VAL
    dta :1LAB76             ; $BC - VPOS
    dta :1LB3BD             ; $BD - CHR$
    dta :1LAFBF             ; $BE - GET$
    dta :1LB026             ; $BF - INKEY$
    dta :1LAFCC             ; $C0 - LEFT$(
    dta :1LB039             ; $C1 - MID$(
    dta :1LAFEE             ; $C2 - RIGHT$(
    dta :1LB094             ; $C3 - STR$(
    dta :1LB0C2             ; $C4 - STRING$(
    dta :1LACB8             ; $C5 - EOF
    dta :1L90AC             ; $C6 - AUTO
    dta :1L8F31             ; $C7 - DELETE
    dta :1LBF24             ; $C8 - LOAD
    dta :1LB59C             ; $C9 - LIST
    dta :1L8ADA             ; $CA - NEW
    dta :1L8AB6             ; $CB - OLD
    dta :1L8FA3             ; $CC - RENUMBER
    dta :1LBEF3             ; $CD - SAVE
    dta :1L982A             ; $CE - unused
    dta :1LBF30             ; $CF - PTR
    dta :1L9283             ; $D0 - PAGE
    dta :1L92C9             ; $D1 - TIME
    dta :1L926F             ; $D2 - LOMEM
    dta :1L925D             ; $D3 - HIMEM
    dta :1LB44C             ; $D4 - SOUND
    dta :1LBF58             ; $D5 - BPUT
    dta :1L8ED2             ; $D6 - CALL
    dta :1LBF2A             ; $D7 - CHAIN
    dta :1L928D             ; $D8 - CLEAR
    dta :1LBF99             ; $D9 - CLOSE
    dta :1L8EBD             ; $DA - CLG
    dta :1L8EC4             ; $DB - CLS
    dta :1L8B7D             ; $DC - DATA
    dta :1L8B7D             ; $DD - DEF
    dta :1L912F             ; $DE - DIM
    dta :1L93E8             ; $DF - DRAW
    dta :1L8AC8             ; $E0 - END
    dta :1L9356             ; $E1 - ENDPROC
    dta :1LB472             ; $E2 - ENVELOPE
    dta :1LB7C4             ; $E3 - FOR
    dta :1LB888             ; $E4 - GOSUB
    dta :1LB8CC             ; $E5 - GOTO
    dta :1L937A             ; $E6 - GCOL
    dta :1L98C2             ; $E7 - IF
    dta :1LBA44             ; $E8 - INPUT
    dta :1L8BE4             ; $E9 - LET
    dta :1L9323             ; $EA - LOCAL
    dta :1L939A             ; $EB - MODE
    dta :1L93E4             ; $EC - MOVE
    dta :1LB695             ; $ED - NEXT
    dta :1LB915             ; $EE - ON
    dta :1L942F             ; $EF - VDU
    dta :1L93F1             ; $F0 - PLOT
    dta :1L8D9A             ; $F1 - PRINT
    .if split == 0
        dta :1L9304         ; $F2 - PROC
    .elseif split != 0
        dta :1X9304         ; $F2 - PROC
    .endif 
    dta :1LBB1F             ; $F3 - READ
    dta :1L8B7D             ; $F4 - REM
    dta :1LBBE4             ; $F5 - REPEAT
    dta :1LBFE4             ; $F6 - REPORT
    dta :1LBAE6             ; $F7 - RESTORE
    dta :1LB8B6             ; $F8 - RETURN
    dta :1LBD11             ; $F9 - RUN
    dta :1L8AD0             ; $FA - STOP
    dta :1L938E             ; $FB - COLOUR
    dta :1L9295             ; $FC - TRACE
    dta :1LBBB1             ; $FD - UNTIL
    dta :1LB4A0             ; $FE - WIDTH
    dta :1LBEC2             ; $FF - OSCLI
.endm

; FUNCTION/COMMAND DISPATCH TABLE, ADDRESS LOW BYTES
; ==================================================

L836D:
    func_table <

; FUNCTION/COMMAND DISPATCH TABLE, ADDRESS HIGH BYTES
; ===================================================

L83DF:
    func_table >

; ----------------------------------------------------------------------------

; ASSEMBLER
; =========

; Packed mnemonic table, low bytes
; --------------------------------

L8451:
    icl 'mneml.s'

; Packed mnemonic table, high bytes
; ---------------------------------

L848B:
    icl 'mnemh.s'

; Opcode base table
; -----------------

L84C5:

; No arguments
; ------------
    BRK:CLC:CLD:CLI:CLV:DEX:DEY:INX
    INY:NOP:PHA:PHP:PLA:PLP:RTI:RTS
    SEC:SED:SEI:TAX:TAY:TSX:TXA:TXS:TYA

; Branches
; --------
    dta $90, $B0, $F0, $30  ; BMI, BCC, BCS, BEQ
    dta $D0, $10, $50, $70  ; BNE, BPL, BVC, BVS

; Arithmetic
; ----------
    dta $21, $41, $01, $61  ; AND, EOR, ORA, ADC
    dta $C1, $A1, $E1, $06  ; CMP, LDA, SBC, ASL
    dta $46, $26, $66, $C6  ; LSR, ROL, ROR, DEC
    dta $E6, $E0, $C0, $20  ; INC, CPX, CPY, BIT

; Others
; ------
    dta $4C, $20, $A2, $A0  ; JMP, JSR, LDX, LDY
    dta $81, $86, $84       ; STA, STX, STY

; ----------------------------------------------------------------------------

; Exit Assembler
; --------------

L84FD:
    LDA #$FF                 ; Set OPT to 'BASIC'
L84FF:
    STA zp28
    JMP L8BA3                ; Set OPT, return to execution loop

L8504:
    LDA #$03
    STA zp28                 ; Set OPT 3, default on entry to '['
L8508
    JSR L8A97                ; Skip spaces
    CMP #']'
    BEQ L84FD                ; ']' - exit assembler
    JSR L986D

L8512:
    DEC zp0A
    JSR L85BA

    DEC zp0A
    LDA zp28
    LSR
    BCC L857E

    LDA zp1E
    ADC #$04
    STA zp3F
    LDA zp38
    JSR LB545

    LDA zp37
    JSR LB562

    LDX #$FC
    LDY zp39
    BPL L8536

    LDY zp36
L8536:
    STY zp38
    BEQ L8556

    LDY #$00
L853C:
    INX
    BNE L854C

    JSR LBC25      ; Print newline

    LDX zp3F

L8544:
    .if version < 3
        JSR LB565       ; Print a space
        DEX
        BNE L8544       ; Loop to print spaces
    .elseif version >= 3
        JSR LB580       ; Print multiple spaces
    .endif

    LDX #$FD
L854C:
    LDA (zp3A),Y
    JSR LB562

    INY
    DEC zp38
    BNE L853C

L8556:
    .if version < 3
        INX
        BPL L8565
        JSR LB565
        JSR LB558
        JSR LB558
        JMP L8556
L8565:
        LDY #0
    .elseif version >= 3
        TXA
        TAY
X855C:
        INY
X855D:
        BEQ X8566
        LDX #3
        JSR LB580
        BEQ X855C
X8566:
        LDX #$0A
        LDA (zp0B),Y
        CMP #$2E
        BNE X857D
X856E:
        JSR LB50E           ; Print char or token
        DEX
        BNE X8576
        LDX #1
X8576:
        INY
        LDA (zp0B),Y
        CPY zp4F
        BNE X856E
X857D:
        JSR LB580
        DEY
X8581:
        INY
        CMP (zp0B),Y
        BEQ X8581
    .endif

L8567:
    LDA (zp0B),Y
    CMP #$3A
    BEQ L8577
    CMP #$0D
    BEQ L857B
L8571:
    JSR LB50E  ; Print character or token
    INY:BNE L8567
L8577
    CPY zp0A
    BCC L8571
L857B:
    JSR LBC25  ; Print newline
L857E
    LDY zp0A
    DEY
L8581:
    INY
    LDA (zp0B),Y
    CMP #$3A
    BEQ L858C
    CMP #$0D
    BNE L8581
L858C:
    JSR L9859
    DEY
    LDA (zp0B),Y
    CMP #$3A
    BEQ L85A2
    LDA zp0C
    CMP #$07+(ws/256)
    BNE L859F
    JMP L8AF6

L859F:
    JSR L9890
L85A2:
    JMP L8508

L85A5:
    JSR L9582
    BEQ L8604
    BCS L8604
    JSR LBD94
    JSR LAE3A       ; Find P%
    STA zp27
    JSR LB4B4
    JSR L8827

L85BA:
    LDX #$03              ; Prepare to fetch three characters
    JSR L8A97             ; Skip spaces
    LDY #$00
    STY zp3D
    CMP #':'
    BEQ L862B           ; End of statement
    CMP #$0D
    BEQ L862B           ; End of line
    CMP #'\'
    BEQ L862B           ; Comment
    CMP #'.'
    BEQ L85A5 ; Label
    DEC zp0A
L85D5:
    LDY zp0A
    INC zp0A       ; Get current character, inc. index
    LDA (zp0B),Y
    BMI L8607 ; Token, check for tokenised AND, EOR, OR
    CMP #$20
    BEQ L85F1    ; Space, step past
    LDY #$05
    ASL
    ASL
    ASL     ; Compact first character
L85E6:
    ASL
    ROL zp3D
    ROL zp3E
    DEY
    BNE L85E6
    DEX
    BNE L85D5         ; Loop to fetch three characters

; The current opcode has now been compressed into two bytes
; ---------------------------------------------------------

L85F1:
    LDX #$3A                ; Point to end of opcode lookup table
    LDA zp3D                ; Get low byte of compacted mnemonic
L85F5:
    CMP L8451-1,X
    BNE L8601 ; Low half doesn't match
    LDY L848B-1,X           ; Check high half
    CPY zp3E
    BEQ L8620       ; Mnemonic matches
L8601:
    DEX
    BNE L85F5           ; Loop through opcode lookup table
L8604:
    JMP L982A               ; Mnemonic not matched, Mistake

L8607:
    LDX #$22                ; opcode number for 'AND'
    CMP #tknAND
    BEQ L8620   ; Tokenised 'AND'
    INX                     ; opcode number for 'EOR'
    CMP #tknEOR
    BEQ L8620   ; Tokenised 'EOR'
    INX                     ; opcode number for 'ORA'
    CMP #tknOR
    BNE L8604    ; Not tokenised 'OR'
    INC zp0A
    INY
    LDA (zp0B),Y ; Get next character
    CMP #'A'
    BNE L8604   ; Ensure 'OR' followed by 'A'

; Opcode found
; ------------

L8620:
    LDA L84C5-1,X
    STA zp29   ; Get base opcode
    LDY #$01                ; Y=1 for one byte
    CPX #$1A
    BCS L8673      ; Opcode $1A+ have arguments
L862B:
    LDA ws+$0440
    STA zp37    ; Get P% low byte
    STY zp39
    LDX zp28
    CPX #$04        ; Offset assembly (opt>3)
    LDX ws+$0441
    STX zp38    ; Get P% high byte
    BCC L8643               ; No offset assembly
    LDA ws+$043C
    LDX ws+$043D ; Get O%
L8643:
    STA zp3A
    STX zp3B         ; Store destination pointer
    TYA
    BEQ L8672
    BPL L8650
    LDY zp36
    BEQ L8672
L8650:
    DEY
    LDA zp29,Y         ; Get opcode byte   (lda abs,y (!))
    BIT zp39
    BPL L865B       ; Opcode - jump to store it
    LDA ws+$0600,Y          ; Get EQU byte
L865B:
    STA (zp3A),Y             ; Store byte
    INC ws+$0440
    BNE L8665  ; Increment P%
    INC ws+$0441
L8665:
    BCC L866F
    INC ws+$043C
    BNE L866F  ; Increment O%
    INC ws+$043D
L866F:
    TYA
    BNE L8650
L8672:
    RTS

L8673:
    CPX #$22
    BCS L86B7
    JSR L8821
    CLC
    LDA zp2A
    SBC ws+$0440
    TAY
    LDA zp2B
    SBC ws+$0441
    CPY #$01
    DEY
    SBC #$00
    BEQ L86B2
    CMP #$FF
    BEQ L86AD

L8691:
    LDA zp28
    .if version < 3
        LSR
    .elseif version >= 3
        AND #$02
    .endif
    BEQ L86A5

    BRK
    dta 1, 'Out of range'
    BRK

L86A5:
    TAY
L86A6:
    STY zp2A
L86A8:
    LDY #$02
    JMP L862B

L86AD:
    TYA
    BMI L86A6
    BPL L8691

L86B2:
    TYA
    BPL L86A6
    BMI L8691

L86B7:
    CPX #$29
    BCS L86D3
    JSR L8A97               ; Skip spaces
    CMP #'#'
    BNE L86DA
    JSR L882F
L86C5:
    JSR L8821
L86C8:
    LDA zp2B
    BEQ L86A8
L86CC:
    BRK
    dta $02
    FNfold 'Byte'
    BRK

; Parse (zp),Y addressing mode
; ----------------------------
L86D3:
    CPX #$36
    BNE L873F
    JSR L8A97               ; Skip spaces
L86DA:
    CMP #'('
    BNE L8715
    JSR L8821
    JSR L8A97               ; Skip spaces
    CMP #')'
    BNE L86FB
    JSR L8A97               ; Skip spaces
    CMP #','
    BNE L870D   ; No comma, jump to Index error
    JSR L882C
    JSR L8A97               ; Skip spaces
    CMP #'Y'
    BNE L870D   ; (zp),Y missing Y, jump to Index error
    BEQ L86C8

; Parse (zp,X) addressing mode
; ----------------------------
L86FB:
    CMP #','
    BNE L870D   ; No comma, jump to Index error
    JSR L8A97               ; Skip spaces
    CMP #'X'
    BNE L870D   ; zp,X missing X, jump to Index error
    JSR L8A97               ; Skip spaces
    CMP #')'
    BEQ L86C8   ; zp,X) - jump to process
L870D:
    BRK
    dta $03
    FNfold 'Index'
    BRK

L8715:
    DEC zp0A
    JSR L8821
    JSR L8A97               ; Skip spaces
    CMP #','
    BNE L8735   ; No comma - jump to process as abs,X
    JSR L882C
    JSR L8A97               ; Skip spaces
    CMP #'X'
    BEQ L8735   ; abs,X - jump to process
    CMP #'Y'
    BNE L870D   ; Not abs,Y - jump to Index error
L872F:
    JSR L882F
    JMP L879A

; abs and abs,X
; -------------
L8735:
    JSR L8832
L8738:
    LDA zp2B
    BNE L872F
    JMP L86A8

L873F:
    CPX #$2F
    BCS L876E
    CPX #$2D
    BCS L8750
    JSR L8A97               ; Skip spaces
    CMP #'A'
    BEQ L8767   ; ins A -
    DEC zp0A
L8750:
    JSR L8821
    JSR L8A97               ; Skip spaces
    CMP #','
    BNE L8738   ; No comma, jump to ...
    JSR L882C
    JSR L8A97               ; Skip spaces
    CMP #'X'
    BEQ L8738   ; Jump with address,X
    JMP L870D               ; Otherwise, jump to Index error

L8767:
    JSR L8832
    LDY #$01
    BNE L879C

L876E:
    CPX #$32
    BCS L8788
    CPX #$31
    BEQ L8782
    JSR L8A97               ; Skip spaces
    CMP #'#'
    BNE L8780   ; Not #, jump with address
    JMP L86C5               ; Jump with immediate

L8780:
    DEC zp0A
L8782:
    JSR L8821
    JMP L8735

L8788:
    CPX #$33
    BEQ L8797
    BCS L87B2
    JSR L8A97               ; Skip spaces
    CMP #'('
    BEQ L879F   ; Jump with (... addressing mode
    DEC zp0A
L8797:
    JSR L8821
L879A:
    LDY #$03
L879C:
    JMP L862B

L879F:
    JSR L882C
    JSR L882C
    JSR L8821
    JSR L8A97               ; Skip spaces
    CMP #')'
    BEQ L879A
    JMP L870D               ; No ) - jump to Index error

L87B2:
    CPX #$39
    BCS L8813
    LDA zp3D
    EOR #$01
    AND #$1F
    PHA
    CPX #$37
    BCS L87F0
    JSR L8A97               ; Skip spaces
    CMP #'#'
    BNE L87CC
    PLA
    JMP L86C5

L87CC:
    DEC zp0A
    JSR L8821
    PLA
    STA zp37
    JSR L8A97               ; Skip spaces
    CMP #','
    BEQ L87DE
    JMP L8735

L87DE:
    JSR L8A97               ; Skip spaces
    AND #$1F
    CMP zp37
    BNE L87ED
    JSR L882C
    JMP L8735

L87ED:
    JMP L870D               ; Jump to Index error

L87F0:
    JSR L8821
    PLA
    STA zp37
    JSR L8A97               ; Skip spaces
    CMP #','
    BNE L8810
    JSR L8A97               ; Skip spaces
    AND #$1F
    CMP zp37
    BNE L87ED
    JSR L882C
    LDA zp2B
    BEQ L8810       ; High byte=0, continue
    JMP L86CC               ; value>255, jump to Byte error

L8810:
    JMP L8738

L8813:
    BNE L883A
    JSR L8821
    LDA zp2A
    STA zp28
    LDY #$00
    JMP L862B

L8821:
    JSR L9B1D
    JSR L92F0
L8827:
    LDY zp1B
    STY zp0A
    RTS

L882C:
    JSR L882F
L882F:
    JSR L8832
L8832:
    LDA zp29
    CLC
    ADC #$04
    STA zp29
    RTS

L883A:
    LDX #$01              ; Prepare for one byte
    LDY zp0A
    INC zp0A       ; Increment index
    LDA (zp0B),Y           ; Get next character
    CMP #'B'
    BEQ L8858 ; EQUB
    INX                   ; Prepare for two bytes
    CMP #'W'
    BEQ L8858 ; EQUW
    LDX #$04              ; Prepare for four bytes
    CMP #'D'
    BEQ L8858 ; EQUD
    CMP #'S'
    BEQ L886A ; EQUS
    JMP L982A             ; Syntax error

L8858:
    TXA
    PHA
    JSR L8821
    LDX #$29
    JSR LBE44
    PLA
    TAY
L8864:
    JMP L862B

L8867:
    JMP L8C0E

L886A:
    LDA zp28
    PHA
    JSR L9B1D
    BNE L8867
    PLA
    STA zp28
    JSR L8827
    LDY #$FF
    BNE L8864

L887C:
    PHA
    CLC
    TYA
    ADC zp37
    STA zp39
    LDY #$00
    TYA
    ADC zp38
    STA zp3A
    PLA
    STA (zp37),Y
L888D:
    INY
    LDA (zp39),Y
    STA (zp37),Y
    CMP #$0D
    BNE L888D
    RTS

L8897:
    AND #$0F
    STA zp3D
    STY zp3E
L889D:
    INY
    LDA (zp37),Y
    .if version < 3
        CMP #'9'+1
        BCS L88DA
        CMP #'0'
    .elseif version >= 3
        JSR L8936
    .endif
    BCC L88DA
    AND #$0F
    PHA
    LDX zp3E
    LDA zp3D
    ASL
    ROL zp3E
    BMI L88D5
    ASL
    ROL zp3E
    BMI L88D5
    ADC zp3D
    STA zp3D
    TXA
    ADC zp3E
    ASL zp3D
    ROL
    BMI L88D5
    BCS L88D5
    STA zp3E
    PLA
    ADC zp3D
    STA zp3D
    BCC L889D
    INC zp3E
    BPL L889D
    PHA
L88D5:
    PLA
    LDY #$00
    SEC
    RTS

L88DA:
    DEY
    LDA #$8D
    JSR L887C
    LDA zp37
    ADC #$02
    STA zp39
    LDA zp38
    ADC #$00
    STA zp3A
L88EC:
    LDA (zp37),Y
    STA (zp39),Y
    DEY
    BNE L88EC
    LDY #$03
L88F5:
    LDA zp3E
    ORA #$40
    STA (zp37),Y
    DEY
    LDA zp3D
    AND #$3F
    ORA #$40
    STA (zp37),Y
    DEY
    LDA zp3D
    AND #$C0
    STA zp3D
    LDA zp3E
    AND #$C0
    LSR
    LSR
    ORA zp3D
    LSR
    LSR
    EOR #$54
    STA (zp37),Y
    JSR L8944               ; Increment $37/8
    JSR L8944               ; Increment $37/8
    JSR L8944               ; Increment $37/8
    LDY #$00
L8924:
    CLC
    RTS

L8926:
    CMP #$7B
    BCS L8924
    CMP #$5F
    BCS L893C
    CMP #$5B
    BCS L8924
    CMP #$41
    BCS L893C
L8936:
    CMP #$3A
    BCS L8924
    CMP #$30
L893C:
    RTS

L893D:
    CMP #$2E
    BNE L8936
    RTS

L8942:
    LDA (zp37),Y
L8944:
    INC zp37
    BNE L894A
    INC zp38
L894A:
    RTS

L894B:
    JSR L8944                ; Increment $37/8
    LDA (zp37),Y
    RTS

; Tokenise line at $37/8
; ======================
L8951:
    LDY #$00
    STY zp3B      ; Set tokeniser to left-hand-side
L8955:
    STY zp3C
L8957:
    LDA (zp37),Y           ; Get current character
    CMP #$0D
    BEQ L894A    ; Exit with <cr>
    CMP #$20
    BNE L8966    ; Skip <spc>
L8961:
    JSR L8944
    BNE L8957   ; Increment $37/8 and check next character

L8966:
    CMP #'&'
    BNE L897C ; Jump if not '&'
L896A:
    JSR L894B                ; Increment $37/8 and get next character
    JSR L8936
    BCS L896A      ; Jump if numeric character
    CMP #'A'
    BCC L8957    ; Loop back if <'A'
    CMP #'F'+1
    BCC L896A  ; Step to next if 'A'..'F'
    BCS L8957                ; Loop back for next character
L897C:
    CMP #$22
    BNE L898C
L8980:
    JSR L894B                ; Increment $37/8 and get next character
    CMP #$22
    BEQ L8961       ; Not quote, jump to process next character
    CMP #$0D
    BNE L8980
    RTS

L898C:
    CMP #':'
    BNE L8996
    STY zp3B
    STY zp3C
    BEQ L8961
L8996:
    CMP #','
    BEQ L8961
    CMP #'*'
    BNE L89A3
    LDA zp3B
    BNE L89E3
    RTS

L89A3:
    CMP #'.'
    BEQ L89B5
    JSR L8936
    BCC L89DF
    LDX zp3C
    BEQ L89B5
    JSR L8897
    BCC L89E9
L89B5:
    LDA (zp37),Y
    JSR L893D
    BCC L89C2
    JSR L8944
    JMP L89B5

L89C2:
    LDX #$FF
    STX zp3B
    STY zp3C
    JMP L8957

L89CB:
    JSR L8926
    BCC L89E3
L89D0:
    LDY #$00
L89D2:
    LDA (zp37),Y
    JSR L8926
    BCC L89C2
    JSR L8944
    JMP L89D2

L89DF:
    CMP #'A'
    BCS L89EC    ; Jump if letter
L89E3:
    LDX #$FF
    STX zp3B
    STY zp3C
L89E9:
    JMP L8961

L89EC:
    CMP #'X'
    BCS L89CB      ; Jump if >='X', nothing starts with X,Y,Z
    LDX #<L8071
    STX zp39 ; Point to token table
    LDX #>L8071
    STX zp3A
L89F8:
    CMP (zp39),Y
    BCC L89D2
    BNE L8A0D
L89FE:
    INY
    LDA (zp39),Y
    BMI L8A37
    CMP (zp37),Y
    BEQ L89FE
    LDA (zp37),Y
    CMP #'.'
    BEQ L8A18
L8A0D:
    INY
    LDA (zp39),Y
    BPL L8A0D
    CMP #$FE
    BNE L8A25
    BCS L89D0
L8A18:
    INY
L8A19:
    LDA (zp39),Y
    BMI L8A37
    INC zp39
    BNE L8A19
    INC zp3A
    BNE L8A19
L8A25:
    SEC
    INY
    TYA
    ADC zp39
    STA zp39
    BCC L8A30
    INC zp3A
L8A30:
    LDY #$00
    LDA (zp37),Y
    JMP L89F8

L8A37:
    TAX
    INY
    LDA (zp39),Y
    STA zp3D  ; Get token flag
    DEY
    LSR
    BCC L8A48
    LDA (zp37),Y
    JSR L8926
    BCS L89D0
L8A48:
    TXA
    BIT zp3D
    BVC L8A54
    LDX zp3B
    BNE L8A54
    .if split == 0
        CLC        ; Superflous as all paths to here have CLC
    .endif
    ADC #$40
L8A54:
    DEY
    JSR L887C
    LDY #$00
    LDX #$FF
    LDA zp3D
    LSR
    LSR
    BCC L8A66
    STX zp3B
    STY zp3C
L8A66:
    LSR
    BCC L8A6D
    STY zp3B
    STY zp3C
L8A6D:
    LSR
    BCC L8A81
    PHA
    INY
L8A72:
    LDA (zp37),Y
    JSR L8926
    BCC L8A7F
    JSR L8944
    JMP L8A72

L8A7F:
    DEY
    PLA
L8A81:
    LSR
    BCC L8A86
    STX zp3C
L8A86:
    LSR
    BCS L8A96
    JMP L8961

; Skip Spaces
; ===========
L8A8C:
    LDY zp1B
    INC zp1B           ; Get offset, increment it
    LDA (zp19),Y               ; Get current character
    CMP #' '
    BEQ L8A8C     ; Loop until not space
L8A96:
    RTS

; Skip spaces at PtrA
; -------------------
L8A97:
    LDY zp0A
    INC zp0A
    LDA (zp0B),Y
    CMP #$20
    BEQ L8A97
L8AA1:
    RTS

    .if version < 3
L8AA2:
        BRK
        dta 5
        FNfold 'Missing ,'
        BRK
    .endif

L8AAE:
    JSR L8A8C
    CMP #','
    .if version < 3
        BNE L8AA2
        RTS
    .elseif version >= 3
        BEQ L8AA1
X8AC8:
        BRK
        dta 5
        FNfold 'Missing ,'
        BRK
    .endif


; OLD - Attempt to restore program
; ================================
L8AB6:
    JSR L9857                 ; Check end of statement
    LDA zp18
    STA zp38           ; Point $37/8 to PAGE
    LDA #$00
    STA zp37
    STA (zp37),Y               ; Remove end marker
    JSR LBE6F                 ; Check program and set TOP
    BNE L8AF3                 ; Jump to clear heap and go to immediate mode

; END - Return to immediate mode
; ==============================
L8AC8:
    JSR L9857                 ; Check end of statement
    JSR LBE6F                 ; Check program and set TOP
    BNE L8AF6                 ; Jump to immediate mode, keeping variables, etc

; STOP - Abort program with an error
; ==================================
L8AD0:
    JSR L9857                 ; Check end of statement
    .if version < 3 && foldup == 0
        BRK
        dta 0
        dta 'STOP'
        BRK
    .endif
    .if version >= 3 || foldup != 0
        BRK
        dta 0
        dta tknSTOP
        BRK
    .endif

; NEW - Clear program, enter immediate mode
; =========================================
L8ADA:
    JSR L9857                 ; Check end of statement
    .if title != 0
        JSR X8ADD                 ; NEW program
    .endif

; Start up with NEW program
; -------------------------
L8ADD:
    .if title == 0
        LDA #$0D
        LDY zp18
        STY zp13  ; TOP hi=PAGE hi
        LDY #$00
        STY zp12
        STY zp20  ; TOP=PAGE, TRACE OFF
        STA (zp12),Y               ; ?(PAGE+0)=<cr>
        LDA #$FF
        INY
        STA (zp12),Y  ; ?(PAGE+1)=$FF
        INY
        STY zp12               ; TOP=PAGE+2
    .endif

L8AF3:
    JSR LBD20                 ; Clear variables, heap, stack

; IMMEDIATE LOOP
; ==============
L8AF6:
    LDY #$07+(ws/256)
    STY zp0C ; PtrA=$0700 - input buffer
    LDY #$00
    STY zp0B
    LDA #<LB433
    STA zp16; ON ERROR OFF
    LDA #>LB433
    STA zp17
    LDA #'>'
    JSR LBC02     ; Print '>' prompt, read input to buffer at PtrA

; Execute line at program pointer in $0B/C
; ----------------------------------------
L8B0B:
    LDA #<LB433
    STA zp16; ON ERROR OFF again
    LDA #>LB433
    STA zp17
    LDX #$FF
    STX zp28          ; OPT=$FF - not within assembler
    STX zp3C
    TXS               ; Clear machine stack
    JSR LBD3A
    TAY             ; Clear DATA and stacks
    LDA zp0B
    STA zp37           ; Point $37/8 to program line
    LDA zp0C
    STA zp38
    STY zp3B
    STY zp0A
    JSR L8957
    JSR L97DF
    BCC L8B38       ; Tokenise, jump forward if no line number
    JSR LBC8D
    JMP L8AF3       ; Insert into program, jump back to immediate loop

; Command entered at immediate prompt
; -----------------------------------
L8B38:
    JSR L8A97                 ; Skip spaces at PtrA
    CMP #$C6
    BCS L8BB1        ; If command token, jump to execute command
    BCC L8BBF                 ; Not command token, try variable assignment

L8B41:
    JMP L8AF6                 ; Jump back to immediate mode

; [ - enter assembler
; ===================
L8B44:
    JMP L8504                 ; Jump to assembler

; =<value> - return from FN
; =========================
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknFN
L8B47:
    TSX
    CPX #$FC
    BCS L8B59        ; If stack is empty, jump to give error
    LDA $01FF
    CMP #tknFN
    BNE L8B59; If pushed token<>'FN', give error
    JSR L9B1D                     ; Evaluate expression
    JMP L984C                     ; Check for end of statement and return to pop from function
L8B59:
    BRK
    dta 7
    FNfold 'No '
    dta tknFN       ; XXX
    BRK

; Check for =, *, [ commands
; ==========================
L8B60:
    LDY zp0A
    DEY
    LDA (zp0B),Y   ; Step program pointer back and fetch char
    CMP #'='
    BEQ L8B47     ; Jump for '=', return from FN
    CMP #'*'
    BEQ L8B73     ; Jump for '*', embedded *command
    CMP #'['
    BEQ L8B44     ; Jump for '[', start assembler
    BNE L8B96                 ; Otherwise, see if end of statement

; Embedded *command
; =================
L8B73:
    JSR L986D                 ; Update PtrA to current address
    LDX zp0B
    LDY zp0C           ; XY=>command string


    .ifdef MOS_ATOM
        JSR cmdStar              ; Pass command at ptrA to Atom OSCLI
    .endif

    .ifdef MOS_BBC
        JSR OS_CLI               ; Pass command at ptrA to OSCLI
    .endif

; DATA, DEF, REM, ELSE
; ====================
; Skip to end of line
; -------------------
L8B7D:
    LDA #$0D
    LDY zp0A
    DEY      ; Get program pointer
L8B82:
    INY
    CMP (zp0B),Y
    BNE L8B82 ; Loop until <cr> found
L8B87:
    CMP #tknELSE
    BEQ L8B7D    ; If 'ELSE', jump to skip to end of line
    LDA zp0C
    CMP #(ws+$0700)/256
    BEQ L8B41; Program in command buffer, jump back to immediate loop
    JSR L9890
    BNE L8BA3       ; Check for end of program, step past <cr>

L8B96:
    DEC zp0A
L8B98:
    JSR L9857

; Main execution loop
; -------------------
L8B9B:
    LDY #$00
    LDA (zp0B),Y      ; Get current character
    CMP #':'
    BNE L8B87     ; Not <colon>, check for ELSE

L8BA3:
    LDY zp0A
    INC zp0A           ; Get program pointer, increment for next time
    LDA (zp0B),Y               ; Get current character
    CMP #$20
    BEQ L8BA3        ; Skip spaces
    CMP #$CF
    BCC L8BBF        ; Not program command, jump to try variable assignment

; Dispatch function/command
; -------------------------
L8BB1:
    TAX                       ; Index into dispatch table
    LDA L836D-$8E,X
    STA zp37   ; Get routine address from table
    LDA L83DF-$8E,X
    STA zp38
    JMP (zp37)               ; Jump to routine

; Not a command byte, try variable assignment, or =, *, [
; -------------------------------------------------------
L8BBF:
    LDX zp0B
    STX zp19           ; Copy PtrA to PtrB
    LDX zp0C
    STX zp1A
    STY zp1B
    JSR L95DD         ; Check if variable or indirection
    BNE L8BE9                 ; NE - jump for existing variable or indirection assignment
    BCS L8B60                 ; CS - not variable assignment, try =, *, [ commands

; Variable not found, create a new one
; ------------------------------------
    STX zp1B
    JSR L9841         ; Check for and step past '='
    JSR L94FC                 ; Create new variable
    LDX #$05                  ; X=$05 = float
    CPX zp2C
    BNE L8BDF         ; Jump if dest. not a float
    INX                       ; X=$06
L8BDF:
    JSR L9531
    DEC zp0A

; LET variable = expression
; =========================
L8BE4:
    JSR L9582
    BEQ L8C0B
L8BE9:
    BCC L8BFB
    JSR LBD94 ; Stack integer (address of data)
    JSR L9813 ; Check for end of statement
    LDA zp27   ; Get evaluation type
    BNE L8C0E ; If not string, error
    JSR L8C1E ; Assign the string
    JMP L8B9B ; Return to execution loop

L8BFB:
    JSR LBD94 ; Stack integer (address of data)
    JSR L9813 ; Check for end of statement
    LDA zp27   ; Get evaluation type
    BEQ L8C0E ; If not number, error
    JSR LB4B4 ; Assign the number
    JMP L8B9B ; Return to execution loop

L8C0B:
    JMP L982A

L8C0E:
    BRK
    dta 6
    FNfold 'Type mismatch'
    BRK

L8C1E:
    JSR LBDEA ; Unstack integer (address of data)
L8C21:
    LDA zp2C
    CMP #$80
    BEQ L8CA2 ; Jump if absolute string $addr
    LDY #$02
    LDA (zp2A),Y
    CMP zp36
    BCS L8C84
    LDA zp02
    STA zp2C
    LDA zp03
    STA zp2D
    LDA zp36
    CMP #$08
    BCC L8C43
    ADC #$07
    BCC L8C43
    LDA #$FF
L8C43:
    CLC
    PHA
    TAX
    LDA (zp2A),Y
    LDY #$00
    ADC (zp2A),Y
    EOR zp02
    BNE L8C5F
    INY
    ADC (zp2A),Y
    EOR zp03
    BNE L8C5F
    STA zp2D
    TXA
    INY
    SEC
    SBC (zp2A),Y
    TAX
L8C5F:
    TXA
    CLC
    ADC zp02
    TAY
    LDA zp03
    ADC #$00
    CPY zp04
    TAX
    SBC zp05
    BCS L8CB7
    STY zp02
    STX zp03
    PLA
    LDY #$02
    STA (zp2A),Y
    DEY
    LDA zp2D
    BEQ L8C84
    STA (zp2A),Y
    DEY
    LDA zp2C
    STA (zp2A),Y
L8C84:
    LDY #$03
    LDA zp36
    STA (zp2A),Y
    BEQ L8CA1
    DEY
    DEY
    LDA (zp2A),Y
    STA zp2D
    DEY
    LDA (zp2A),Y
    STA zp2C
L8C97:
    LDA ws+$0600,Y
    STA (zp2C),Y
    INY
    CPY zp36
    BNE L8C97
L8CA1:
    RTS

L8CA2:
    JSR LBEBA
    CPY #$00
    BEQ L8CB4
L8CA9:
    LDA ws+$0600,Y
    STA (zp2A),Y
    DEY
    BNE L8CA9
    LDA ws+$0600
L8CB4:
    STA (zp2A),Y
    RTS

L8CB7:
    BRK
    dta 0
    FNfold 'No room'
    BRK

L8CC1:
    LDA zp39
    CMP #$80
    BEQ L8CEE
    BCC L8D03
    LDY #$00
    LDA (zp04),Y
    TAX
    BEQ L8CE5
    LDA (zp37),Y
    SBC #$01
    STA zp39
    INY
    LDA (zp37),Y
    SBC #$00
    STA zp3A
L8CDD:
    LDA (zp04),Y
    STA (zp39),Y
    INY
    DEX
    BNE L8CDD
L8CE5:
    LDA (zp04,X)
    LDY #$03
L8CE9:
    STA (zp37),Y
    JMP LBDDC

L8CEE:
    LDY #$00
    LDA (zp04),Y
    TAX
    BEQ L8CFF
L8CF5:
    INY
    LDA (zp04),Y
    DEY
    STA (zp37),Y
    INY
    DEX
    BNE L8CF5
L8CFF:
    LDA #$0D
    BNE L8CE9
L8D03:
    LDY #$00
    LDA (zp04),Y
    STA (zp37),Y
    .if version < 3
        INY
        CPY zp39
        BCS L8D26
    .elseif version >= 3
        LDY #4
        LDA zp39
        BEQ L8D26
        LDY #$01
    .endif
    LDA (zp04),Y
    STA (zp37),Y
    INY
    LDA (zp04),Y
    STA (zp37),Y
    INY
    LDA (zp04),Y
    STA (zp37),Y
    INY
    CPY zp39
    BCS L8D26
    LDA (zp04),Y
    STA (zp37),Y
    INY
L8D26:
    TYA
    CLC
    JMP LBDE1

; PRINT#
L8D2B:
    DEC zp0A
    JSR LBFA9
L8D30:
    TYA
    PHA
    JSR L8A8C
    CMP #$2C
    BNE L8D77
    JSR L9B29
    JSR LA385
    PLA
    TAY
    LDA zp27
    JSR OSBPUT
    TAX
    BEQ L8D64
    BMI L8D57
    LDX #$03
L8D4D:
    LDA zp2A,X
    JSR OSBPUT
    DEX
    BPL L8D4D
    BMI L8D30
L8D57:
    LDX #$04
L8D59:
    LDA ws+$046C,X
    JSR OSBPUT
    DEX
    BPL L8D59
    BMI L8D30
L8D64:
    LDA zp36
    JSR OSBPUT
    TAX
    BEQ L8D30
L8D6C:
    LDA ws+$05FF,X
    JSR OSBPUT
    DEX
    BNE L8D6C
    BEQ L8D30
L8D77:
    PLA
    STY zp0A
    JMP L8B98

; End of PRINT statement
; ----------------------
L8D7D:
    JSR LBC25                 ; Output new line and set COUNT to zero
L8D80:
    JMP L8B96                 ; Check end of statement, return to execution loop

L8D83:
    LDA #$00
    STA zp14
    STA zp15  ; Set current field to zero, hex/dec flag to decimal
    JSR L8A97                 ; Get next non-space character
    CMP #':'
    BEQ L8D80     ; <colon> found, finish printing
    CMP #$0D
    BEQ L8D80        ; <cr> found, finish printing
    CMP #tknELSE
    BEQ L8D80    ; 'ELSE' found, finish printing
    BNE L8DD2                 ; Otherwise, continue into main loop

; PRINT [~][print items]['][,][;]
; ===============================
L8D9A:
    JSR L8A97                 ; Get next non-space char
    CMP #'#'
    BEQ L8D2B     ; If '#' jump to do PRINT#
    DEC zp0A
    JMP L8DBB         ; Jump into PRINT loop

; Print a comma
; -------------
L8DA6:
    LDA ws+$0400
    BEQ L8DBB    ; If field width zero, no padding needed, jump back into main loop
    LDA zp1E                   ; Get COUNT
L8DAD:
    BEQ L8DBB                 ; Zero, just started a new line, no padding, jump back into main loop
    SBC ws+$0400              ; Get COUNT-field width
    BCS L8DAD                 ; Loop to reduce until (COUNT MOD fieldwidth)<0
    TAY                       ; Y=number of spaces to get back to (COUNT MOD width)=zero
L8DB5:
    JSR LB565
    INY
    BNE L8DB5   ; Loop to print required spaces

L8DBB:
    CLC                       ; Prepare to print decimal
    LDA ws+$0400
    STA zp14      ; Set current field width from @%
L8DC1:
    ROR zp15                   ; Set hex/dec flag from Carry
L8DC3:
    JSR L8A97                 ; Get next non-space character
    CMP #':'
    BEQ L8D7D     ; End of statement if <colon> found
    CMP #$0D
    BEQ L8D7D        ; End if statement if <cr> found
    CMP #tknELSE
    BEQ L8D7D    ; End of statement if 'ELSE' found

L8DD2:
    CMP #'~'
    BEQ L8DC1     ; Jump back to set hex/dec flag from Carry
    CMP #','
    BEQ L8DA6     ; Jump to pad to next print field
    CMP #';'
    BEQ L8D83     ; Jump to check for end of print statement
    JSR L8E70
    BCC L8DC3       ; Check for ' TAB SPC, if print token found return to outer main loop

; All print formatting have been checked, so it now must be an expression
; -----------------------------------------------------------------------
    LDA zp14
    PHA
    LDA zp15
    PHA   ; Save field width and flags, as evaluator
                              ;  may call PRINT (eg FN, STR$, etc.)
    DEC zp1B
    JSR L9B29         ; Evaluate expression
    PLA
    STA zp15
    PLA
    STA zp14   ; Restore field width and flags
    LDA zp1B
    STA zp0A           ; Update program pointer
    TYA
    BEQ L8E0E             ; If type=0, jump to print string
    JSR L9EDF                 ; Convert numeric value to string
    LDA zp14                   ; Get current field width
    SEC
    SBC zp36               ; A=width-stringlength
    BCC L8E0E                 ; length>width - print it
    BEQ L8E0E                 ; length=width - print it
    TAY                       ; Otherwise, Y=number of spaces to pad with
L8E08:
    JSR LB565
    DEY
    BNE L8E08   ; Loop to print required spaces to pad the number

; Print string in string buffer
; -----------------------------
L8E0E:
    LDA zp36
    BEQ L8DC3         ; Null string, jump back to main loop
    LDY #$00                  ; Point to start of string
L8E14:
    LDA ws+$0600,Y
    JSR LB558  ; Print character from string buffer
    INY
    CPY zp36
    BNE L8E14     ; Increment pointer, loop for full string
    BEQ L8DC3                 ; Jump back for next print item

L8E21:
    .if version < 3
        JMP L8AA2
    .elseif version >= 3
        JMP X8AC8
    .endif

L8E24:
    CMP #','
    BNE L8E21     ; No comma, jump to TAB(x)
    LDA zp2A
    PHA               ; Save X
    JSR LAE56
    JSR L92F0

; Atom - manually position cursor
; -------------------------------
    .ifdef MOS_ATOM
        LDA #$1E
        JSR OSWRCH   ; Home cursor
        LDY zp2A
        BEQ XADDC     ; Y=0, no movement needed
        LDA #$0A
XADD6:
        JSR OSWRCH            ; Move cursor down
        DEY
        BNE XADD6         ; Loop until Y position reached
XADDC:
        PLA
        BEQ XADE8         ; X=0, no movement needed
        TAY
        LDA #$09
XADE2:
        JSR OSWRCH            ; Move cursor right
        DEY
        BNE XADE2         ; Loop until X position reached
XADE8:
    .endif

; BBC - send VDU 31,x,y sequence
; ------------------------------
    .ifdef MOS_BBC
        LDA #$1F
        JSR OSWRCH      ; TAB()
        PLA
        JSR OSWRCH           ; X coord
        JSR L9456                ; Y coord
    .endif

    JMP L8E6A                 ; Continue to next PRINT item

L8E40:
    JSR L92DD
    JSR L8A8C
    CMP #')'
    BNE L8E24
    LDA zp2A
    SBC zp1E
    BEQ L8E6A
    .if version < 3
        TAY
    .elseif version >= 3
        TAX
    .endif
    BCS L8E5F
    JSR LBC25
    BEQ L8E5B
L8E58:
    JSR L92E3
L8E5B:
    .if version < 3
        LDY zp2A
    .elseif version >= 3
        LDX zp2A
    .endif
    BEQ L8E6A
L8E5F:
    .if version < 3
        JSR LB565
        DEY
        BNE L8E5F
    .elseif version >= 3
        JSR LB580
    .endif
    BEQ L8E6A
L8E67:
    JSR LBC25
L8E6A:
    CLC
    LDY zp1B
    STY zp0A
    RTS

L8E70:
    LDX zp0B
    STX zp19
    LDX zp0C
    STX zp1A
    LDX zp0A
    STX zp1B
    CMP #$27
    BEQ L8E67
    CMP #$8A
    BEQ L8E40
    CMP #$89
    BEQ L8E58
    SEC
L8E89:
    RTS

L8E8A:
    JSR L8A97 ; Skip spaces
    JSR L8E70
    BCC L8E89
    CMP #$22
    BEQ L8EA7
    SEC
    RTS

L8E98:
    BRK
    dta 9
    FNfold 'Missing '
    dta 0x22                ; "
    BRK

L8EA4:
    JSR LB558
L8EA7:
    INY
    LDA (zp19),Y
    CMP #$0D
    BEQ L8E98
    CMP #$22
    BNE L8EA4
    INY
    STY zp1B
    LDA (zp19),Y
    CMP #$22
    BNE L8E6A
    BEQ L8EA4

; CLG
; ===
L8EBD:
    JSR L9857                 ; Check end of statement
    LDA #$10
    BNE L8ECC        ; Jump to do VDU 16

; CLS
; ===
L8EC4:
    JSR L9857                 ; Check end of statement
    JSR LBC28                 ; Set COUNT to zero
    LDA #$0C                  ; Do VDU 12
L8ECC:
    JSR OSWRCH
    JMP L8B9B      ; Send A to OSWRCH, jump to execution loop

; CALL numeric [,items ... ]
; ==========================
L8ED2:
    JSR L9B1D
    JSR L92EE
    JSR LBD94
    LDY #$00
    STY ws+$0600
L8EE0:
    STY ws+$06FF
    JSR L8A8C
    CMP #$2C
    BNE L8F0C
    LDY zp1B
    JSR L95D5
    BEQ L8F1B
    LDY ws+$06FF
    INY
    LDA zp2A
    STA ws+$0600,Y
    INY
    LDA zp2B
    STA ws+$0600,Y
    INY
    LDA zp2C
    STA ws+$0600,Y
    INC ws+$0600
    JMP L8EE0

L8F0C:
    DEC zp1B
    JSR L9852 ; Check for end of statement
    JSR LBDEA         ; Pop integer to IntA
    JSR L8F1E         ; Set up registers and call code at IntA
    CLD               ; Ensure Binary mode on return
    JMP L8B9B         ; Jump back to program loop

L8F1B:
    JMP LAE43

; Call code
; ---------
L8F1E:
    LDA ws+$040C
    LSR
    LDA ws+$0404  ; Get Carry from C%, A from A%
    LDX ws+$0460
    LDY ws+$0464        ; Get X from X%, Y from Y%
    JMP (zp2A)                      ; Jump to address in IntA


L8F2E:
    JMP L982A

; DELETE linenum, linenum
; =======================
L8F31:
    JSR L97DF
    BCC L8F2E
    JSR LBD94
    JSR L8A97
    CMP #$2C
    BNE L8F2E
    JSR L97DF
    BCC L8F2E
    JSR L9857
    LDA zp2A
    STA zp39
    LDA zp2B
    STA zp3A
    JSR LBDEA
L8F53:
    JSR LBC2D
    JSR L987B
    JSR L9222
    LDA zp39
    CMP zp2A
    LDA zp3A
    SBC zp2B
    BCS L8F53
    JMP L8AF3

; Called by RENUMBER and AUTO
L8F69:
    LDA #$0A
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    JSR L97DF
    JSR LBD94
    LDA #$0A
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    JSR L8A97
    CMP #$2C
    BNE L8F8D
    JSR L97DF
    LDA zp2B
    BNE L8FDF
    LDA zp2A
    BEQ L8FDF
    INC zp0A
L8F8D:
    DEC zp0A
    JMP L9857

; called by renumber
L8F92:
    LDA zp12
    STA zp3B
    LDA zp13
    STA zp3C
L8F9A:
    LDA zp18
    STA zp38
    LDA #$01
    STA zp37
    RTS

; RENUMBER [linenume [,linenum]]
; ==============================
L8FA3:
    JSR L8F69
    LDX #$39
    JSR LBE0D
    JSR LBE6F
    JSR L8F92
; Build up table of line numbers
L8FB1:
    LDY #$00
    LDA (zp37),Y
    BMI L8FE7 ; Line.hi>$7F, end of program
    STA (zp3B),Y
    INY
    LDA (zp37),Y
    STA (zp3B),Y
    SEC
    TYA
    ADC zp3B
    STA zp3B
    TAX
    LDA zp3C
    ADC #$00
    STA zp3C
    CPX zp06
    SBC zp07
    BCS L8FD6
    JSR L909F
    BCC L8FB1
L8FD6:
    BRK
    dta 0
    dta tknRENUMBER
    FNfold ' space'  ; Terminated by following BRK
L8FDF:
    BRK
    dta 0
    FNfold 'Silly'
    BRK

; Do 4K+12K split here
; --------------------
L8FE7:
    .if split == 1
        JMP X8FE7

        ; PROCname [(parameters)]
        ; =======================
X9304:
        LDA zp0B
        STA zp19           ; PtrB=PtrA=>after 'PROC' token
        LDA zp0C
        STA zp1A
        LDA zp0A
        STA zp1B
        LDA #$F2
        JSR LB197        ; Call PROC/FN dispatcher
                               ; Will return here after ENDPROC
        JSR L9852                 ; Check for end of statement
        JMP L8B9B                 ; Return to execution loop
    .endif

; Look for renumber references
X8FE7:
    JSR L8F9A
L8FEA:
    LDY #$00
    LDA (zp37),Y
    BMI L900D
    LDA zp3A
    STA (zp37),Y
    LDA zp39
    INY
    STA (zp37),Y
    CLC
    LDA zp2A
    ADC zp39
    STA zp39
    LDA #$00
    ADC zp3A
    AND #$7F
    STA zp3A
    JSR L909F
    BCC L8FEA
L900D:
    LDA zp18
    STA zp0C
    LDY #$00
    STY zp0B
    INY
    LDA (zp0B),Y
    .if version < 3
        BMI L903A
    .elseif version >= 3
        BMI L9080
    .endif
L901A:
    LDY #$04
L901C:
    LDA (zp0B),Y
    CMP #$8D
    BEQ L903D
    INY
    CMP #$0D
    BNE L901C
    LDA (zp0B),Y
    .if version < 3
        BMI L903A
    .elseif version >= 3
        BMI L9080
    .endif
    LDY #$03
    LDA (zp0B),Y
    CLC
    ADC zp0B
    STA zp0B
    BCC L901A
    INC zp0C
    BCS L901A
L903A:
    .if version < 3
        JMP L8AF3
    .endif

L903D:
    JSR L97EB
    JSR L8F92
L9043:
    LDY #$00
    LDA (zp37),Y
    BMI L9082
    LDA (zp3B),Y
    INY
    CMP zp2B
    BNE L9071
    LDA (zp3B),Y
    CMP zp2A
    BNE L9071
    LDA (zp37),Y
    STA zp3D
    DEY
    LDA (zp37),Y
    STA zp3E
    LDY zp0A
    DEY
    LDA zp0B
    STA zp37
    LDA zp0C
    STA zp38
    JSR L88F5
L906D:
    LDY zp0A
    BNE L901C
L9071:
    .if version >= 3
        CLC
    .endif
    JSR L909F
    LDA zp3B
    ADC #$02
    STA zp3B
    BCC L9043
    INC zp3C
    BCS L9043
L9080:
    .if version >= 3
        BMI L90D9
    .endif
L9082:
    JSR LBFCF        ; Print inline text
    FNfold 'Failed at '
    INY
    LDA (zp0B),Y
    STA zp2B
    INY
    LDA (zp0B),Y
    STA zp2A
    JSR L991F        ; Print in decimal
    JSR LBC25        ; Print newline
    BEQ L906D
L909F:
    INY
    LDA (zp37),Y
    ADC zp37
    STA zp37
    BCC L90AB
    INC zp38
    CLC
L90AB:
    RTS

; AUTO [numeric [, numeric ]]
; ===========================
L90AC:
    JSR L8F69
    LDA zp2A
    PHA
    JSR LBDEA
L90B5:
    JSR LBD94
    JSR L9923
    LDA #$20
    JSR LBC02
    JSR LBDEA
    JSR L8951
    JSR LBC8D
    JSR LBD20
    PLA
    PHA
    CLC
    ADC zp2A
    STA zp2A
    BCC L90B5
    INC zp2B
    BPL L90B5
L90D9:
    JMP L8AF3

L90DC:
    JMP L9218

L90DF:
    DEC zp0A
    JSR L9582
    BEQ L9127
    BCS L9127
    JSR LBD94
    JSR L92DD
    JSR L9222
    LDA zp2D
    ORA zp2C
    BNE L9127
    CLC
    LDA zp2A
    ADC zp02
    TAY
    LDA zp2B
    ADC zp03
    TAX
    CPY zp04
    SBC zp05
    BCS L90DC
    LDA zp02
    STA zp2A
    LDA zp03
    STA zp2B
    STY zp02
    STX zp03
    LDA #$00
    STA zp2C
    STA zp2D
    LDA #$40
    STA zp27
    JSR LB4B4
    JSR L8827
    JMP L920B

L9127:
    BRK
    dta 10
    FNfold 'Bad '
    dta tknDIM          ; XXX
    BRK

; DIM numvar [numeric] [(arraydef)]
; =================================
L912F:
    JSR L8A97
    TYA
    CLC
    ADC zp0B
    LDX zp0C
    BCC L913C
    INX
    CLC
L913C:
    SBC #$00
    STA zp37
    TXA
    SBC #$00
    STA zp38
    LDX #$05
    STX zp3F
    LDX zp0A
    JSR L9559
    CPY #$01
    BEQ L9127
    CMP #'('
    BEQ L916B
    CMP #$24
    BEQ L915E
    CMP #$25
    BNE L9168
L915E:
    DEC zp3F
    INY
    INX
    LDA (zp37),Y
    CMP #'('
    BEQ L916B
L9168:
    JMP L90DF

L916B:
    STY zp39
    STX zp0A
    JSR L9469
    BNE L9127
    JSR L94FC
    LDX #$01
    JSR L9531
    LDA zp3F
    PHA
    LDA #$01
    PHA
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
L9185:
    JSR LBD94
    JSR L8821
    LDA zp2B
    AND #$C0
    ORA zp2C
    ORA zp2D
    BNE L9127
    JSR L9222
    PLA
    TAY
    LDA zp2A
    STA (zp02),Y
    INY
    LDA zp2B
    STA (zp02),Y
    INY
    TYA
    PHA
    JSR L9231
    JSR L8A97
    CMP #$2C
    BEQ L9185
    CMP #')'
    BEQ L91B7
    JMP L9127

L91B7:
    PLA
    STA zp15
    PLA
    STA zp3F
    LDA #$00
    STA zp40
    JSR L9236
    LDY #$00
    LDA zp15
    STA (zp02),Y
    ADC zp2A
    STA zp2A
    BCC L91D2
    INC zp2B
L91D2:
    LDA zp03
    STA zp38
    LDA zp02
    STA zp37
    CLC
    ADC zp2A
    TAY
    LDA zp2B
    ADC zp03
    BCS L9218
    TAX
    CPY zp04
    SBC zp05
    BCS L9218
    STY zp02
    STX zp03
    LDA zp37
    ADC zp15
    TAY
    LDA #$00
    STA zp37
    BCC L91FC
    INC zp38
L91FC:
    STA (zp37),Y
    INY
    BNE L9203
    INC zp38
L9203:
    CPY zp02
    BNE L91FC
    CPX zp38
    BNE L91FC
L920B:
    JSR L8A97
    CMP #$2C
    BEQ L9215
    JMP L8B96

L9215:
    JMP L912F

L9218:
    BRK
    dta 11
    dta tknDIM
    FNfold ' space'
    BRK

L9222:
    INC zp2A
    BNE L9230
    INC zp2B
    BNE L9230
    INC zp2C
    BNE L9230
    INC zp2D
L9230:
    RTS

L9231:
    LDX #$3F
    JSR LBE0D
L9236:
    LDX #$00
    LDY #$00
L923A:
    LSR zp40
    ROR zp3F
    BCC L924B
    CLC
    TYA
    ADC zp2A
    TAY
    TXA
    ADC zp2B
    TAX
    BCS L925A
L924B:
    ASL zp2A
    ROL zp2B
    LDA zp3F
    ORA zp40
    BNE L923A
    STY zp2A
    STX zp2B
    RTS

L925A:
    JMP L9127

; HIMEM=numeric
; =============
L925D:
    JSR L92EB                 ; Set past '=', evaluate integer
    LDA zp2A
    STA zp06
    STA zp04   ; Set HIMEM and STACK
    LDA zp2B
    STA zp07
    STA zp05
    JMP L8B9B                 ; Jump back to execution loop

; LOMEM=numeric
; =============
L926F:
    JSR L92EB                 ; Step past '=', evaluate integer
    LDA zp2A
    STA ZP00
    STA zp02  ; Set LOMEM and VAREND
    LDA zp2B
    STA ZP01
    STA zp03
    JSR LBD2F
    BEQ L928A       ; Clear dynamic variables, jump to execution loop

; PAGE=numeric
; ============
L9283:
    JSR L92EB                 ; Step past '=', evaluate integer
    LDA zp2B
    STA zp18           ; Set PAGE
L928A:
    JMP L8B9B                 ; Jump to execution loop

; CLEAR
; =====
L928D:
    JSR L9857                 ; Check end of statement
    JSR LBD20                 ; Clear heap, stack, data, variables
    BEQ L928A                 ; Jump to execution loop

; TRACE ON | OFF | numeric
; ========================
L9295:
    JSR L97DF
    BCS L92A5       ; If line number, jump for TRACE linenum
    CMP #$EE
    BEQ L92B7        ; Jump for TRACE ON
    CMP #$87
    BEQ L92C0        ; Jump for TRACE OFF
    JSR L8821                 ; Evaluate integer

; TRACE numeric
; -------------
L92A5:
    JSR L9857                 ; Check end of statement
    LDA zp2A
    STA zp21           ; Set trace limit low byte
    LDA zp2B
L92AE:
    STA zp22
    LDA #$FF          ; Set trace limit high byte, set TRACE ON
L92B2:
    STA zp20
    JMP L8B9B         ; Set TRACE flag, return to execution loop

; TRACE ON
; --------
L92B7:
    INC zp0A
    JSR L9857         ; Step past, check end of statement
    LDA #$FF
    BNE L92AE        ; Jump to set TRACE $FFxx

; TRACE OFF
; ---------
L92C0:
    INC zp0A
    JSR L9857         ; Step past, check end of statement
    LDA #$00
    BEQ L92B2        ; Jump to set TRACE OFF

; TIME=numeric
; ============
L92C9:
    JSR L92EB                   ; Step past '=', evaluate integer
    .ifdef MOS_BBC
        LDX #$2A
        LDY #$00
        STY zp2E ; Point to integer, set 5th byte to 0
        LDA #$02
        JSR OSWORD       ; Call OSWORD $02 to do TIME=
    .endif
    JMP L8B9B                   ; Jump to execution loop

; Evaluate <comma><numeric>
; =========================
L92DA:
    JSR L8AAE                 ; Check for and step past comma
L92DD:
    JSR L9B29
    JMP L92F0

L92E3:
    JSR LADEC
    BEQ L92F7
    BMI L92F4
L92EA:
    RTS

; Evaluate <equals><integer>
; ==========================
L92EB:
    JSR L9807                 ; Check for equals, evaluate numeric
L92EE:
    LDA zp27                   ; Get result type
L92F0:
    BEQ L92F7                 ; String, jump to 'Type mismatch'
    BPL L92EA                 ; Integer, return
L92F4:
    JMP LA3E4                 ; Real, jump to convert to integer

L92F7:
    JMP L8C0E                 ; Jump to 'Type mismatch' error

; Evaluate <real>
; ===============
L92FA:
    JSR LADEC                 ; Evaluate expression

; Ensure value is real
; --------------------
L92FD:
    BEQ L92F7                 ; String, jump to 'Type mismatch'
    BMI L92EA                 ; Real, return
    JMP LA2BE                 ; Integer, jump to convert to real

    .if split == 0
; PROCname [(parameters)]
; =======================
L9304:
        LDA zp0B
        STA zp19           ; PtrB=PtrA=>after 'PROC' token
        LDA zp0C
        STA zp1A
        LDA zp0A
        STA zp1B
        LDA #$F2
        JSR LB197        ; Call PROC/FN dispatcher
                               ; Will return here after ENDPROC
        JSR L9852                 ; Check for end of statement
        JMP L8B9B                 ; Return to execution loop
    .endif

; Make string zero length
; -----------------------
L931B:
    LDY #$03
    LDA #$00        ; Set length to zero
    STA (zp2A),Y
    BEQ L9341    ; Jump to look for next LOCAL item

; LOCAL variable [,variable ...]
; ==============================
L9323:
    TSX
    CPX #$FC
    BCS L936B   ; Not inside subroutine, error
    JSR L9582
    BEQ L9353      ; Find variable, jump if bad variable name
    JSR LB30D                ; Push value on stack, push variable info on stack
    LDY zp2C
    BMI L931B        ; If a string, jump to make zero length
    JSR LBD94                ; 
    LDA #$00                 ; Set IntA to zero
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    STA zp27
    JSR LB4B4        ; Set current variable to IntA (zero)

; Next LOCAL item
; ---------------
L9341:
    TSX
    INC $0106,X           ; Increment number of LOCAL items
    LDY zp1B
    STY zp0A           ; Update line pointer
    JSR L8A97                 ; Get next character
    CMP #$2C
    BEQ L9323        ; Comma, loop back to do another item
    JMP L8B96                 ; Jump to main execution loop

L9353:
    JMP L8B98

; ENDPROC
; =======
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknPROC
L9356:
    TSX
    CPX #$FC
    BCS L9365       ; If stack empty, jump to give error
    LDA $01FF
    CMP #$F2
    BNE L9365 ; If pushed token<>'PROC', give error
    JMP L9857                    ; Check for end of statement and return to pop from subroutine
L9365:
    BRK
    dta 13
    FNfold 'No '
    dta tknPROC   ; Terminated by following BRK
L936B:
    BRK
    dta 12
    FNfold 'Not '
    dta tknLOCAL ; Terminated by following BRK
L9372:
    BRK
    dta $19
    FNfold 'Bad '
    dta tknMODE
    BRK

; GCOL numeric, numeric
; =====================
L937A:
    JSR L8821
    LDA zp2A
    PHA     ; Evaluate integer
    JSR L92DA                 ; Step past comma, evaluate integer
    JSR L9852                 ; Update program pointer, check for end of statement
    LDA #$12
    JSR OSWRCH       ; Send VDU 18 for GCOL
    JMP L93DA                 ; Jump to send two bytes to OSWRCH

; COLOUR numeric
; ==============
L938E:
    LDA #$11
    PHA              ; Stack VDU 17 for COLOUR
    JSR L8821
    JSR L9857       ; Evaluate integer, check end of statement
    JMP L93DA                 ; Jump to send two bytes to OSWRCH

; MODE numeric
; ============
L939A:
    LDA #$16
    PHA              ; Stack VDU 22 for MODE
    JSR L8821
    JSR L9857       ; Evaluate integer, check end of statement

; BBC - Check if changing MODE will move screen into stack
; --------------------------------------------------------
    .ifdef MOS_BBC
        JSR LBEE7                ; Get machine address high word
        CPX #$FF
        BNE L93D7       ; Not $xxFFxxxx, skip memory test
        CPY #$FF
        BNE L93D7       ; Not $FFFFxxxx, skip memory test

        ; MODE change in I/O processor, must check memory limits
        LDA zp04
        CMP zp06
        BNE L9372      ; STACK<>HIMEM, stack not empty, give 'Bad MODE' error
        LDA zp05
        CMP zp07
        BNE L9372
        LDX zp2A
        LDA #$85
        JSR OSBYTE    ; Get top of memory if we used this MODE
        CPX zp02
        TYA
        SBC zp03
        BCC L9372  ; Would be below VAREND, give error
        CPX zp12
        TYA
        SBC zp13
        BCC L9372  ; Would be below TOP, give error

        ; BASIC stack is empty, screen would not hit heap or program
        STX zp06
        STX zp04           ; Set STACK and HIMEM to new address
        STY zp07
        STY zp05
    .endif

; Change MODE
L93D7:
    JSR LBC28                 ; Set COUNT to zero

; Send two bytes to OSWRCH, stacked byte, then IntA
; -------------------------------------------------
L93DA:
    PLA
    JSR OSWRCH            ; Send stacked byte to OSWRCH
    JSR L9456
    JMP L8B9B       ; Send IntA to OSWRCH, jump to execution loop

; MOVE numeric, numeric
; =====================
L93E4:
    LDA #$04
    BNE L93EA        ; Jump forward to do PLOT 4 for MOVE

; DRAW numeric, numeric
; =====================
L93E8:
    LDA #$05                  ; Do PLOT 5 for DRAW
L93EA:
    PHA
    JSR L9B1D             ; Evaluate first expression
    JMP L93FD                 ; Jump to evaluate second expression and send to OSWRCH

; PLOT numeric, numeric, numeric
; ==============================
L93F1:
    JSR L8821
    LDA zp2A
    PHA     ; Evaluate integer
    JSR L8AAE
    JSR L9B29       ; Step past comma, evaluate expression
L93FD:
    JSR L92EE                 ; Confirm numeric and ensure is integer
    JSR LBD94                 ; Stack integer
    JSR L92DA                 ; Step past command and evaluate integer
    JSR L9852                 ; Update program pointer, check for end of statement
    LDA #$19
    JSR OSWRCH       ; Send VDU 25 for PLOT
    PLA
    JSR OSWRCH            ; Send PLOT action
    JSR LBE0B                 ; Pop integer to temporary store at $37/8
    LDA zp37
    JSR OSWRCH        ; Send first coordinate to OSWRCH
    LDA zp38
    JSR OSWRCH
    JSR L9456                 ; Send IntA to OSWRCH, second coordinate
    LDA zp2B
    JSR OSWRCH        ; Send IntA high byte to OSWRCH
    JMP L8B9B                 ; Jump to execution loop


L942A:
    LDA zp2B
    JSR OSWRCH        ; Send IntA byte 2 to OSWRCH

; VDU num[,][;][...]
; ==================
L942F:
    JSR L8A97                 ; Get next character
L9432:
    CMP #$3A
    BEQ L9453        ; If end of statement, jump to exit
    CMP #$0D
    BEQ L9453
    CMP #$8B
    BEQ L9453
    DEC zp0A                   ; Step back to current character
    JSR L8821
    JSR L9456       ; Evaluate integer and output low byte
    JSR L8A97                 ; Get next character
    CMP #','
    BEQ L942F     ; Comma, loop to read another number
    CMP #';'
    BNE L9432     ; Not semicolon, loop to check for end of statement
    BEQ L942A                 ; Loop to output high byte and read another
L9453:
    JMP L8B96                 ; Jump to execution loop


; Send IntA to OSWRCH via WRCHV
; =============================
L9456:
    LDA zp2A
    .if WRCHV != 0
        JMP (WRCHV)
    .elseif WRCHV == 0
        JMP OSWRCH 
    .endif


; VARIABLE PROCESSING
; ===================
; Look for a FN/PROC in heap
; --------------------------
; On entry, ($37)+1=>FN/PROC token (ie, first character of name)
;
L945B:
    LDY #$01
    LDA (zp37),Y      ; Get PROC/FN character
    LDY #$F6                  ; Point to PROC list start
    CMP #tknPROC
    BEQ L946F    ; If PROC, jump to scan list
    LDY #$F8
    BNE L946F        ; Point to FN list start and scan list

; Look for a variable in the heap
; -------------------------------
; On entry, ($37)+1=>first character of name
;
L9469:
    LDY #$01
    LDA (zp37),Y      ; Get first character of variable
    ASL
    TAY                 ; Double it to index into index list

; Scan though linked lists in heap
; --------------------------------
L946F:
    LDA ws+$0400,Y
    STA zp3A    ; Get start of linked list
    LDA ws+$0401,Y
    STA zp3B
L9479:
    LDA zp3B
    BEQ L94B2         ; End of list
    LDY #$00
    LDA (zp3A),Y
    STA zp3C
    INY
    LDA (zp3A),Y
    STA zp3D
    INY
    LDA (zp3A),Y
    BNE L949A ; Jump if not null name
    DEY
    CPY zp39
    BNE L94B3
    INY
    BCS L94A7
L9495:
    INY
    LDA (zp3A),Y
    BEQ L94B3
L949A:
    CMP (zp37),Y
    BNE L94B3
    CPY zp39
    BNE L9495
    INY
    LDA (zp3A),Y
    BNE L94B3
L94A7:
    TYA
    ADC zp3A
    STA zp2A
    LDA zp3B
    ADC #$00
    STA zp2B
L94B2:
    RTS

L94B3:
    LDA zp3D
    BEQ L94B2
    LDY #$00
    LDA (zp3C),Y
    STA zp3A
    INY
    LDA (zp3C),Y
    STA zp3B
    INY
    LDA (zp3C),Y
    BNE L94D4
    DEY
    CPY zp39
    BNE L9479
    INY
    BCS L94E1
L94CF:
    INY
    LDA (zp3C),Y
    BEQ L9479
L94D4:
    CMP (zp37),Y
    BNE L9479
    CPY zp39
    BNE L94CF
    INY
    LDA (zp3C),Y
    BNE L9479
L94E1:
    TYA
    ADC zp3C
    STA zp2A
    LDA zp3D
    ADC #$00
    STA zp2B
    RTS

L94ED:
    LDY #$01
    LDA (zp37),Y
    TAX
    LDA #$F6
    CPX #$F2
    BEQ L9501
    LDA #$F8
    BNE L9501
L94FC:
    LDY #$01
    LDA (zp37),Y
    ASL
L9501:
    STA zp3A
    LDA #$04+(ws/256)
    STA zp3B
L9507:
    LDA (zp3A),Y
    BEQ L9516
    TAX
    DEY
    LDA (zp3A),Y
    STA zp3A
    STX zp3B
    INY
    BPL L9507
L9516:
    LDA zp03
    STA (zp3A),Y
    LDA zp02
    DEY
    STA (zp3A),Y
    TYA
    INY
    STA (zp02),Y
    CPY zp39
    BEQ L9558
L9527:
    INY
    LDA (zp37),Y
    STA (zp02),Y
    CPY zp39
    BNE L9527
    RTS

L9531:
    LDA #$00
L9533:
    INY
    STA (zp02),Y
    DEX
    BNE L9533
L9539:
    SEC
    TYA
    ADC zp02
    BCC L9541
    INC zp03
L9541:
    LDY zp03
    CPY zp05
    BCC L9556
    BNE L954D
    CMP zp04
    BCC L9556
L954D:
    LDA #$00
    LDY #$01
    STA (zp3A),Y
    JMP L8CB7

L9556:
    STA zp02
L9558:
    RTS


; Check if variable name is valid
; ===============================
L9559:
    LDY #$01
L955B:
    LDA (zp37),Y
    CMP #$30
    BCC L9579
    CMP #$40
    BCS L9571
    CMP #$3A
    BCS L9579
    CPY #$01
    BEQ L9579
L956D:
    INX
    INY
    BNE L955B
L9571:
    CMP #$5F
    BCS L957A
    CMP #$5B
    BCC L956D
L9579:
    RTS

L957A:
    CMP #$7B
    BCC L956D
    RTS

L957F:
    JSR L9531
L9582:
    JSR L95C9
    BNE L95A4
    BCS L95A4
    JSR L94FC
    LDX #$05
    CPX zp2C
    BNE L957F
    INX
    BNE L957F
L9595:
    CMP #$21
    BEQ L95A5
    CMP #$24
    BEQ L95B0
    EOR #$3F
    BEQ L95A7
    LDA #$00
    SEC
L95A4:
    RTS

L95A5:
    LDA #$04
L95A7:
    PHA
    INC zp1B
    JSR L92E3
    JMP L969F

L95B0:
    INC zp1B
    JSR L92E3
    LDA zp2B
    BEQ L95BF
    LDA #$80
    STA zp2C
    SEC
    RTS

L95BF:
    BRK
    dta 8
    FNfold '$ range'
    BRK
L95C9:
    LDA zp0B
    STA zp19
    LDA zp0C
    STA zp1A
    LDY zp0A
    DEY
L95D4:
    INY
L95D5:
    STY zp1B
    LDA (zp19),Y
    CMP #$20
    BEQ L95D4
L95DD:
    CMP #$40
    BCC L9595
    CMP #$5B
    BCS L95FF
    ASL
    ASL
    STA zp2A
    LDA #$04+(ws/256)
    STA zp2B
    INY
    LDA (zp19),Y
    INY
    CMP #$25
    BNE L95FF
    LDX #$04
    STX zp2C
    LDA (zp19),Y
    CMP #'('
    BNE L9665
L95FF:
    LDX #$05
    STX zp2C
    LDA zp1B
    CLC
    ADC zp19
    LDX zp1A
    BCC L960E
    INX
    CLC
L960E:
    SBC #$00
    STA zp37
    BCS L9615
    DEX
L9615:
    STX zp38
    LDX zp1B
    LDY #$01
L961B:
    LDA (zp37),Y
    CMP #$41
    BCS L962D
    CMP #$30
    BCC L9641
    CMP #$3A
    BCS L9641
    INX
    INY
    BNE L961B
L962D:
    CMP #$5B
    BCS L9635
    INX
    INY
    BNE L961B
L9635:
    CMP #$5F
    BCC L9641
    CMP #$7B
    BCS L9641
    INX
    INY
    BNE L961B
L9641:
    DEY
    BEQ L9673
    CMP #$24
    BEQ L96AF
    CMP #$25
    BNE L9654
    DEC zp2C
    INY
    INX
    INY
    LDA (zp37),Y
    DEY
L9654:
    STY zp39
    CMP #'('
    BEQ L96A6
    JSR L9469
    BEQ L9677
    STX zp1B
L9661:
    LDY zp1B
    LDA (zp19),Y
L9665:
    CMP #$21
    BEQ L967F
    CMP #$3F
    BEQ L967B
    CLC
    STY zp1B
    LDA #$FF
    RTS

L9673:
    LDA #$00
    SEC
    RTS

L9677:
    LDA #$00
    CLC
    RTS

L967B:
    LDA #$00
    BEQ L9681
L967F:
    LDA #$04
L9681:
    PHA
    INY
    STY zp1B
    JSR LB32C
    JSR L92F0
    LDA zp2B
    PHA
    LDA zp2A
    PHA
    JSR L92E3
    CLC
    PLA
    ADC zp2A
    STA zp2A
    PLA
    ADC zp2B
    STA zp2B
L969F:
    PLA
    STA zp2C
    CLC
    LDA #$FF
    RTS

L96A6:
    INX
    INC zp39
    JSR L96DF
    JMP L9661

L96AF:
    INX
    INY
    STY zp39
    INY
    DEC zp2C
    LDA (zp37),Y
    CMP #'('
    BEQ L96C9
    JSR L9469
    BEQ L9677
    STX zp1B
    LDA #$81
    STA zp2C
    SEC
    RTS

L96C9:
    INX
    STY zp39
    DEC zp2C
    JSR L96DF
    LDA #$81
    STA zp2C
    SEC
    RTS

L96D7:
    BRK
    dta 14
    FNfold 'Array'
    BRK

L96DF:
    JSR L9469
    BEQ L96D7
    STX zp1B
    LDA zp2C
    PHA
    LDA zp2A
    PHA
    LDA zp2B
    PHA
    LDY #$00
    LDA (zp2A),Y
    CMP #$04
    BCC L976C
    TYA
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    LDA #$01
    STA zp2D
L96FF:
    JSR LBD94
    JSR L92DD
    INC zp1B
    CPX #$2C
    BNE L96D7
    LDX #$39
    JSR LBE0D
    LDY zp3C
    PLA
    STA zp38
    PLA
    STA zp37
    PHA
    LDA zp38
    PHA
    JSR L97BA
    STY zp2D
    LDA (zp37),Y
    STA zp3F
    INY
    LDA (zp37),Y
    STA zp40
    LDA zp2A
    ADC zp39
    STA zp2A
    LDA zp2B
    ADC zp3A
    STA zp2B
    JSR L9236
    LDY #$00
    SEC
    LDA (zp37),Y
    SBC zp2D
    CMP #$03
    BCS L96FF
    JSR LBD94
    JSR LAE56
    JSR L92F0
    PLA
    STA zp38
    PLA
    STA zp37
    LDX #$39
    JSR LBE0D
    LDY zp3C
    JSR L97BA
    CLC
    LDA zp39
    ADC zp2A
    STA zp2A
    LDA zp3A
    ADC zp2B
    STA zp2B
    BCC L977D
L976C:
    JSR LAE56
    JSR L92F0
    PLA
    STA zp38
    PLA
    STA zp37
    LDY #$01
    JSR L97BA
L977D:
    PLA
    STA zp2C
    CMP #$05
    BNE L979B
    LDX zp2B
    LDA zp2A
    ASL zp2A
    ROL zp2B
    ASL zp2A
    ROL zp2B
    ADC zp2A
    STA zp2A
    TXA
    ADC zp2B
    STA zp2B
    BCC L97A3
L979B:
    ASL zp2A
    ROL zp2B
    ASL zp2A
    ROL zp2B
L97A3:
    TYA
    ADC zp2A
    STA zp2A
    BCC L97AD
    INC zp2B
    CLC
L97AD:
    LDA zp37
    ADC zp2A
    STA zp2A
    LDA zp38
    ADC zp2B
    STA zp2B
    RTS

L97BA:
    LDA zp2B
    AND #$C0
    ORA zp2C
    ORA zp2D
    BNE L97D1
    LDA zp2A
    CMP (zp37),Y
    INY
    LDA zp2B
    SBC (zp37),Y
    BCS L97D1
    INY
    RTS

L97D1:
    BRK
    dta 15
    FNfold 'Subscript'
    BRK
L97DD:
    INC zp0A
L97DF:
    LDY zp0A
    LDA (zp0B),Y
    CMP #$20
    BEQ L97DD
    CMP #$8D
    BNE L9805
L97EB:
    INY
    LDA (zp0B),Y
    ASL
    ASL
    TAX
    AND #$C0
    INY
    EOR (zp0B),Y
    STA zp2A
    TXA
    ASL
    ASL
    INY
    EOR (zp0B),Y
    STA zp2B
    INY
    STY zp0A
    SEC
    RTS

L9805:
    CLC
    RTS

L9807:
    LDA zp0B
    STA zp19
    LDA zp0C
    STA zp1A
    LDA zp0A
    STA zp1B
L9813:
    LDY zp1B
    INC zp1B
    LDA (zp19),Y
    CMP #$20
    BEQ L9813
    CMP #$3D
    BEQ L9849
L9821:
    BRK
    dta 4
    FNfold 'Mistake'
L982A:
    BRK
    dta 16
    FNfold 'Syntax error' ; Terminated by following BRK
    .ifdef MOS_ATOM
        BRK
    .endif

; Escape error
; ------------
L9838:
    .ifdef TARGET_ATOM
        LDA LB001
        AND #$20
        BEQ L9838   ; Loop until Escape not pressed
    .endif

    .ifdef TARGET_SYSTEM
        CMP L0E21
        BEQ L9838            ; Loop until key no longer pressed
    .endif

    BRK
    dta 17
    FNfold 'Escape'
    BRK

L9841:
    JSR L8A8C
    CMP #'='
    BNE L9821
    RTS

L9849:
    JSR L9B29
L984C:
    TXA
    LDY zp1B
    JMP L9861

L9852:
    LDY zp1B
    JMP L9859

; Check for end of statement, check for Escape
; ============================================
L9857:
    LDY zp0A                   ; Get program pointer offset
L9859:
    DEY                       ; Step back to previous character
L985A:
    INY
    LDA (zp0B),Y           ; Get next character
    CMP #$20
    BEQ L985A        ; Skip spaces
L9861:
    CMP #':'
    BEQ L986D     ; Colon, jump to update program pointer
    CMP #$0D
    BEQ L986D        ; <cr>, jump to update program pointer
    CMP #tknELSE
    BNE L982A    ; Not 'ELSE', jump to 'Syntax error'

; Update program pointer
; ----------------------
L986D:
    CLC
    TYA
    ADC zp0B
    STA zp0B   ; Update program pointer in PtrA
    BCC L9877
    INC zp0C
L9877:
    LDY #$01
    STY zp0A

; Check background Escape state
; -----------------------------
L987B:

; Atom - check keyboard matrix
; ----------------------------
    .ifdef TARGET_ATOM
        PHA                       ; Save A
        LDA LB001
        AND #$20        ; Check keyboard matrix
        BEQ L9838                 ; Escape key pressed, jump to error
        PLA                       ; Restore A
    .endif

; System - check current keypress
; -------------------------------
    .ifdef TARGET_SYSTEM
        BIT L0E21
        BMI L987F       ; Nothing pressed
        PHA
        LDA L0E21             ; Save A, get keypress
        CMP #$1B
        BEQ L9838        ; If Escape, jump to error
        PLA                       ; Restore A
    .endif

; BBC - check background Escape state
; -----------------------------------
    .ifdef MOS_BBC
        BIT ESCFLG
        BMI L9838      ; If Escape set, jump to give error
    .endif

L987F:
    RTS

L9880:
    JSR L9857
    DEY
    LDA (zp0B),Y
    CMP #$3A
    BEQ L987F
    LDA zp0C
    CMP #$07+(ws/256)
    BEQ L98BC
L9890:
    INY
    LDA (zp0B),Y
    BMI L98BC
    LDA zp20
    BEQ L98AC
    TYA
    PHA
    INY
    LDA (zp0B),Y
    PHA
    DEY
    LDA (zp0B),Y
    TAY
    PLA
    .if version < 3
        JSR LAEEA
    .elseif version >= 3
        JSR XAED5
    .endif
    JSR L9905
    PLA
    TAY
L98AC:
    INY
    SEC
    TYA
    ADC zp0B
    STA zp0B
    BCC L98B7
    INC zp0C
L98B7:
    LDY #$01
    STY zp0A
L98BB:
    RTS

L98BC:
    JMP L8AF6

L98BF:
    JMP L8C0E

; IF numeric
; ==========
L98C2:
    JSR L9B1D
    BEQ L98BF
    BPL L98CC
    JSR LA3E4
L98CC:
    LDY zp1B
    STY zp0A
    LDA zp2A
    ORA zp2B
    ORA zp2C
    ORA zp2D
    BEQ L98F1
    CPX #$8C
    BEQ L98E1
L98DE:
    JMP L8BA3

L98E1:
    INC zp0A
L98E3:
    JSR L97DF
    BCC L98DE
    JSR LB9AF
    JSR L9877
    JMP LB8D2

L98F1:
    LDY zp0A
L98F3:
    LDA (zp0B),Y
    CMP #$0D
    BEQ L9902
    INY
    CMP #$8B
    BNE L98F3
    STY zp0A
    BEQ L98E3
L9902:
    JMP L8B87

L9905:
    LDA zp2A
    CMP zp21
    LDA zp2B
    SBC zp22
    BCS L98BB
    LDA #$5B
L9911:
    JSR LB558
    JSR L991F
    LDA #$5D
    JSR LB558
    JMP LB565

; Print 16-bit decimal number
; ===========================
L991F:
    LDA #$00 ; No padding
    BEQ L9925
L9923:
    LDA #$05 ; Pad to five characters
L9925:
    STA zp14
    LDX #$04
L9929:
    LDA #$00
    STA zp3F,X
    SEC
L992E:
    LDA zp2A
    SBC L996B,X     ; Subtract 10s low byte
    TAY
    LDA zp2B
    SBC L99B9,X     ; Subtract 10s high byte
    BCC L9943       ; Result<0, no more for this digit
    STA zp2B
    STY zp2A ; Update number
    INC zp3F,X
    BNE L992E

L9943:
    DEX
    BPL L9929
    LDX #$05
L9948:
    DEX
    BEQ L994F
    LDA zp3F,X
    BEQ L9948
L994F:
    STX zp37
    LDA zp14
    BEQ L9960
    SBC zp37
    BEQ L9960
    .if version < 3
        TAY
L995A:
        JSR LB565
        DEY
        BNE L995A
    .elseif version > 3
        TAX
        JSR LB580
        LDX zp37
    .endif
L9960:
    LDA zp3F,X
    ORA #$30
    JSR LB558
    DEX
    BPL L9960
    RTS

; Low bytes of powers of ten
L996B:
    dta 1, 10, 100, <1000, <10000

; Line Search
L9970:
    LDY #$00
    STY zp3D
    LDA zp18
    STA zp3E
L9978:
    LDY #$01
    LDA (zp3D),Y
    CMP zp2B
    BCS L998E
L9980:
    LDY #$03
    LDA (zp3D),Y
    ADC zp3D
    STA zp3D
    BCC L9978
    INC zp3E
    BCS L9978
L998E:
    BNE L99A4
    LDY #$02
    LDA (zp3D),Y
    CMP zp2A
    BCC L9980
    BNE L99A4
    TYA
    ADC zp3D
    STA zp3D
    BCC L99A4
    INC zp3E
    CLC
L99A4:
    LDY #$02
    RTS

L99A7:
    BRK
    dta $12
    FNfold 'Division by zero'

; High byte of powers of ten
L99B9:
    dta 0, 0, 0, >1000, >10000

L99BE:
    TAY              ; 99BE= A8           (
    JSR L92F0        ; 99BF= 20 F0 92     p.
    LDA zp2D          ; 99C2= A5 2D       %-
    PHA              ; 99C4= 48          H
    JSR LAD71        ; 99C5= 20 71 AD     q-
    JSR L9E1D        ; 99C8= 20 1D 9E     ..
    STX zp27          ; 99CB= 86 27       .'
    TAY              ; 99CD= A8          (
    JSR L92F0        ; 99CE= 20 F0 92     p.
    PLA              ; 99D1= 68          h
    STA zp38          ; 99D2= 85 38       .8
    EOR zp2D          ; 99D4= 45 2D       E-
    STA zp37          ; 99D6= 85 37       .7
    JSR LAD71        ; 99D8= 20 71 AD     q-
    LDX #$39         ; 99DB= A2 39       "9
    JSR LBE0D        ; 99DD= 20 0D BE     .>
    STY zp3D          ; 99E0= 84 3D       .=
    STY zp3E          ; 99E2= 84 3E       .>
    STY zp3F          ; 99E4= 84 3F       .?
    STY zp40          ; 99E6= 84 40       .@
    LDA zp2D          ; 99E8= A5 2D       %-
    ORA zp2A          ; 99EA= 05 2A       .*
    ORA zp2B          ; 99EC= 05 2B       .+
    ORA zp2C          ; 99EE= 05 2C       .,
    BEQ L99A7        ; 99F0= F0 B5       p5
    LDY #$20         ; 99F2= A0 20
L99F4:
    DEY              ; 99F4= 88          .
    BEQ L9A38        ; 99F5= F0 41       pA
    ASL zp39          ; 99F7= 06 39       .9
    ROL zp3A          ; 99F9= 26 3A       $

    ROL zp3B          ; 99FB= 26 3B       $;
    ROL zp3C          ; 99FD= 26 3C       $<
    BPL L99F4        ; 99FF= 10 F3       .s
L9A01:
    ROL zp39          ; 9A01= 26 39       $9
    ROL zp3A          ; 9A03= 26 3A       $

    ROL zp3B          ; 9A05= 26 3B       $;
    ROL zp3C          ; 9A07= 26 3C       $<
    ROL zp3D          ; 9A09= 26 3D       $=
    ROL zp3E          ; 9A0B= 26 3E       $>
    ROL zp3F          ; 9A0D= 26 3F       $?
    ROL zp40          ; 9A0F= 26 40       $@
    SEC              ; 9A11= 38          8
    LDA zp3D          ; 9A12= A5 3D       %=
    SBC zp2A          ; 9A14= E5 2A       e*
    PHA              ; 9A16= 48          H
    LDA zp3E          ; 9A17= A5 3E       %>
    SBC zp2B          ; 9A19= E5 2B       e+
    PHA              ; 9A1B= 48          H
    LDA zp3F          ; 9A1C= A5 3F       %?
    SBC zp2C          ; 9A1E= E5 2C       e,
    TAX              ; 9A20= AA          *
    LDA zp40          ; 9A21= A5 40       %@
    SBC zp2D          ; 9A23= E5 2D       e-
    BCC L9A33        ; 9A25= 90 0C       ..
    STA zp40          ; 9A27= 85 40       .@
    STX zp3F          ; 9A29= 86 3F       .?
    PLA              ; 9A2B= 68          h
    STA zp3E          ; 9A2C= 85 3E       .>
    PLA              ; 9A2E= 68          h
    STA zp3D          ; 9A2F= 85 3D       .=
    BCS L9A35        ; 9A31= B0 02       0.
L9A33:
    PLA              ; 9A33= 68          h
    PLA              ; 9A34= 68          h
L9A35:
    DEY              ; 9A35= 88          .
    BNE L9A01        ; 9A36= D0 C9       PI
L9A38:
    RTS              ; 9A38= 60          `

L9A39:
    STX zp27          ; 9A39= 86 27       .'
    JSR LBDEA        ; 9A3B= 20 EA BD     j=
    JSR LBD51        ; 9A3E= 20 51 BD     Q=
    JSR LA2BE        ; 9A41= 20 BE A2     >"
    JSR LA21E        ; 9A44= 20 1E A2     ."
    JSR LBD7E        ; 9A47= 20 7E BD     ~=
    JSR LA3B5        ; 9A4A= 20 B5 A3     5#
    JMP L9A62        ; 9A4D= 4C 62 9A    Lb.

L9A50:
    JSR LBD51        ; 9A50= 20 51 BD     Q=
    JSR L9C42        ; 9A53= 20 42 9C     B.
    STX zp27          ; 9A56= 86 27       .'
    TAY              ; 9A58= A8          (
    JSR L92FD        ; 9A59= 20 FD 92     }.
    JSR LBD7E        ; 9A5C= 20 7E BD     ~=
L9A5F:
    JSR LA34E        ; 9A5F= 20 4E A3     N#

; Compare FPA = FPB
; -----------------
L9A62:
    LDX zp27
    LDY #$00
    LDA zp3B
    AND #$80
    STA zp3B
    LDA zp2E
    AND #$80
    CMP zp3B
    BNE L9A92
    LDA zp3D
    CMP zp30
    BNE L9A93
    LDA zp3E
    CMP zp31
    BNE L9A93
    LDA zp3F
    CMP zp32
    BNE L9A93
    LDA zp40
    CMP zp33
    BNE L9A93
    LDA zp41
    CMP zp34
    BNE L9A93
L9A92:
    RTS

L9A93:
    ROR
    EOR zp3B
    ROL
    LDA #$01
    RTS

L9A9A:
    JMP L8C0E        ; Jump to 'Type mismatch' error


; Evaluate next expression and compare with previous
; --------------------------------------------------
L9A9D:
    TXA
L9A9E:
    BEQ L9AE7                 ; Jump if current is string
    BMI L9A50                 ; Jump if current is float
    JSR LBD94                 ; Stack integer
    JSR L9C42
    TAY             ; Evaluate next expression
    BEQ L9A9A                 ; Error if string
    BMI L9A39                 ; Float, jump to compare floats

; Compare IntA with top of stack
; ------------------------------
    LDA zp2D
    EOR #$80
    STA zp2D
    SEC
    LDY #$00
    LDA (zp04),Y
    SBC zp2A
    STA zp2A
    INY
    LDA (zp04),Y
    SBC zp2B
    STA zp2B
    INY
    LDA (zp04),Y
    SBC zp2C
    STA zp2C
    INY
    LDA (zp04),Y
    LDY #$00
    EOR #$80
    SBC zp2D
    ORA zp2A
    ORA zp2B
    ORA zp2C
    PHP
    CLC
    LDA #$04
    ADC zp04  ; Drop integer from stack
    STA zp04
    BCC L9AE5
    INC zp05
L9AE5:
    PLP
    RTS

; Compare string with next expression
; -----------------------------------
L9AE7:
    JSR LBDB2        ; 9AE7= 20 B2 BD     2=
    JSR L9C42        ; 9AEA= 20 42 9C     B.
    TAY              ; 9AED= A8          (
    BNE L9A9A        ; 9AEE= D0 AA       P*
    STX zp37          ; 9AF0= 86 37       .7
    LDX zp36          ; 9AF2= A6 36       $6
    .if version < 3 || (version == 3 && minorversion < 10)
        LDY #$00
    .endif
    LDA (zp04),Y      ; 9AF6= B1 04       1.
    STA zp39          ; 9AF8= 85 39       .9
    CMP zp36          ; 9AFA= C5 36       E6
    BCS L9AFF        ; 9AFC= B0 01       0.
    TAX              ; 9AFE= AA          *
L9AFF:
    STX zp3A          ; 9AFF= 86 3A       .

    .if version < 3 || (version == 3 && minorversion < 10)
        LDY #$00
    .endif
L9B03:
    CPY zp3A          ; 9B03= C4 3A       D

    BEQ L9B11        ; 9B05= F0 0A       p.
    INY              ; 9B07= C8          H
    LDA (zp04),Y      ; 9B08= B1 04       1.
    CMP ws+$05FF,Y   ; 9B0A= D9 FF 05    Y..
    BEQ L9B03        ; 9B0D= F0 F4       pt
    BNE L9B15        ; 9B0F= D0 04       P.
L9B11:
    LDA zp39          ; 9B11= A5 39       %9
    CMP zp36          ; 9B13= C5 36       E6
L9B15:
    PHP              ; 9B15= 08          .
    JSR LBDDC        ; 9B16= 20 DC BD     \=
    LDX zp37          ; 9B19= A6 37       $7
    PLP              ; 9B1B= 28          (
    RTS              ; 9B1C= 60          `


; EXPRESSION EVALUATOR
; ====================

; Evaluate expression at PtrA
; ---------------------------
L9B1D:
    LDA zp0B
    STA zp19          ; Copy PtrA to PtrB
    LDA zp0C
    STA zp1A
    LDA zp0A
    STA zp1B

; Evaluate expression at PtrB
; ---------------------------
; TOP LEVEL EVALUATOR
;
; Evaluator Level 7 - OR, EOR
; ---------------------------
L9B29:
    JSR L9B72                 ; Call Evaluator Level 6 - AND
                              ; Returns A=type, value in IntA/FPA/StrA, X=next char
L9B2C:
    CPX #tknOR
    BEQ L9B3A      ; Jump if next char is OR
    CPX #tknEOR
    BEQ L9B55     ; Jump if next char is EOR
    DEC zp1B                   ; Step PtrB back to last char
    TAY
    STA zp27
    RTS           ; Set flags from type, store type in $27 and return

; OR numeric
; ----------
L9B3A:
    JSR L9B6B
    TAY             ; Stack as integer, call Evaluator Level 6
    JSR L92F0
    LDY #$03        ; If float, convert to integer
L9B43:
    LDA (zp04),Y
    ORA zp2A,Y   ; OR IntA with top of stack    ; abs,y (!)
    STA zp2A,Y                                  ; abs,y (!)
    DEY
    BPL L9B43 ; Store result in IntA
L9B4E:
    JSR LBDFF                 ; Drop integer from stack
    LDA #$40
    BNE L9B2C        ; Return type=Int, jump to check for more OR/EOR

; EOR numeric
; -----------
L9B55:
    JSR L9B6B
    TAY
    JSR L92F0
    LDY #$03        ; If float, convert to integer
L9B5E:
    LDA (zp04),Y
    EOR zp2A,Y   ; EOR IntA with top of stack       ; abs,y (!)
    STA zp2A,Y                                      ; abs,y (!)
    DEY
    BPL L9B5E ; Store result in IntA
    BMI L9B4E                 ; Jump to drop from stack and continue

; Stack current as integer, evaluate another Level 6
; --------------------------------------------------
L9B6B:
    TAY
    JSR L92F0
    JSR LBD94   ; If float, convert to integer, push into stack

; Evaluator Level 6 - AND
; -----------------------
L9B72:
    JSR L9B9C                 ; Call Evaluator Level 5, < <= = >= > <>
L9B75:
    CPX #tknAND
    BEQ L9B7A
    RTS ; Return if next char not AND

; AND numeric
; -----------
L9B7A:
    TAY
    JSR L92F0
    JSR LBD94   ; If float, convert to integer, push onto stack
    JSR L9B9C                 ; Call Evaluator Level 5, < <= = >= > <>
    TAY
    JSR L92F0
    LDY #$03    ; If float, convert to integer
L9B8A:
    LDA (zp04),Y
    AND zp2A,Y   ; AND IntA with top of stack   ; abs,y (!)
    STA zp2A,Y                                  ; abs,y (!)
    DEY
    BPL L9B8A ; Store result in IntA
    JSR LBDFF                 ; Drop integer from stack
    LDA #$40
    BNE L9B75        ; Return type=Int, jump to check for another AND

; Evaluator Level 5 - >... =... or <...
; -------------------------------------
L9B9C:
    JSR L9C42                 ; Call Evaluator Level 4, + -
    CPX #'>'+1
    BCS L9BA7   ; Larger than '>', return
    CPX #'<'
    BCS L9BA8     ; Smaller than '<', return
L9BA7:
    RTS

; >... =... or <...
; -----------------
L9BA8:
    BEQ L9BC0                 ; Jump with '<'
    CPX #'>'
    BEQ L9BE8     ; Jump with '>'
                              ; Must be '='
; = numeric
; ---------
    TAX
    JSR L9A9E
    BNE L9BB5   ; Jump with result=0 for not equal
L9BB4:
    DEY                       ; Decrement to $FF for equal
L9BB5:
    STY zp2A
    STY zp2B
    STY zp2C   ; Store 0/-1 in IntA
    STY zp2D
    LDA #$40
    RTS      ; Return type=Int

; < <= <>
; -------
L9BC0:
    TAX
    LDY zp1B
    LDA (zp19),Y   ; Get next char from PtrB
    CMP #'='
    BEQ L9BD4     ; Jump for <=
    CMP #'>'
    BEQ L9BDF     ; Jump for <>

; Must be < numeric
; -----------------
    JSR L9A9D                 ; Evaluate next and compare
    BCC L9BB4
    BCS L9BB5       ; Jump to return TRUE if <, FALSE if not <

; <= numeric
; ----------
L9BD4:
    INC zp1B
    JSR L9A9D         ; Step past '=', evaluate next and compare
    BEQ L9BB4
    BCC L9BB4       ; Jump to return TRUE if =, TRUE if <
    BCS L9BB5                 ; Jump to return FALSE otherwise

; <> numeric
; ----------
L9BDF:
    INC zp1B
    JSR L9A9D         ; Step past '>', evaluate next and compare
    BNE L9BB4
    BEQ L9BB5       ; Jump to return TRUE if <>, FALSE if =

; > >=
; ----
L9BE8:
    TAX
    LDY zp1B
    LDA (zp19),Y   ; Get next char from PtrB
    CMP #'='
    BEQ L9BFA     ; Jump for >=

; > numeric
; ---------
    JSR L9A9D                 ; Evaluate next and compare
    BEQ L9BB5
    BCS L9BB4       ; Jump to return FALSE if =, TRUE if >
    BCC L9BB5                 ; Jump to return FALSE if <

; >= numeric
; ----------
L9BFA:
    INC zp1B
    JSR L9A9D         ; Step past '=', evaluate next and compare
    BCS L9BB4
    BCC L9BB5       ; Jump to return TRUE if >=, FALSE if <

L9C03:
    BRK
    dta $13
    FNfold 'String too long'
    BRK

; String addition
; ---------------
L9C15:
    JSR LBDB2
    JSR L9E20       ; Stack string, call Evaluator Level 2
    TAY
    BNE L9C88             ; string + number, jump to 'Type mismatch' error
    CLC
    STX zp37
    LDY #$00
    LDA (zp04),Y      ; Get stacked string length
    ADC zp36
    BCS L9C03         ; If added string length >255, jump to error
    TAX
    PHA
    LDY zp36           ; Save new string length
L9C2D:
    LDA ws+$05FF,Y
    STA ws+$05FF,X ; Move current string up in string buffer
    DEX
    DEY
    BNE L9C2D
    JSR LBDCB                 ; Unstack string to start of string buffer
    PLA
    STA zp36
    LDX zp37       ; Set new string length
    TYA
    BEQ L9C45             ; Set type=string, jump to check for more + or -

; Evaluator Level 4, + -
; ----------------------
L9C42:
    JSR L9DD1                 ; Call Evaluator Level 3, * / DIV MOD
L9C45:
    CPX #'+'
    BEQ L9C4E     ; Jump with addition
    CPX #'-'
    BEQ L9CB5     ; Jump with subtraction
    RTS                       ; Return otherwise

; + <value>
; ---------
L9C4E:
    TAY
    BEQ L9C15             ; Jump if current value is a string
    BMI L9C8B                 ; Jump if current value is a float

; Integer addition
; ----------------
    JSR L9DCE                 ; Stack current and call Evaluator Level 3
    TAY
    BEQ L9C88             ; If int + string, jump to 'Type mismatch' error
    BMI L9CA7                 ; If int + float, jump ...
    LDY #$00
    CLC
    LDA (zp04),Y
    ADC zp2A
    STA zp2A  ; Add top of stack to IntA
    INY
    LDA (zp04),Y
    ADC zp2B
    STA zp2B  ; Store result in IntA
    INY
    LDA (zp04),Y
    ADC zp2C
    STA zp2C
    INY
    LDA (zp04),Y
    ADC zp2D
L9C77:
    STA zp2D
    CLC
    LDA zp04
    ADC #$04
    STA zp04  ; Drop integer from stack
    LDA #$40
    BCC L9C45        ; Set result=integer, jump to check for more + or -
    INC zp05
    BCS L9C45         ; Jump to check for more + or -

L9C88:
    JMP L8C0E                 ; Jump to 'Type mismatch' error

; Real addition
; -------------
L9C8B:
    JSR LBD51
    JSR L9DD1       ; Stack float, call Evaluator Level 3
    TAY
    BEQ L9C88             ; float + string, jump to 'Type mismatch' error
    STX zp27
    BMI L9C9B         ; float + float, skip conversion
    JSR LA2BE                 ; float + int, convert int to float
L9C9B:
    JSR LBD7E                 ; Pop float from stack, point FPTR to it
    JSR LA500                 ; Unstack float to FPA2 and add to FPA1
L9CA1:
    LDX zp27                   ; Get nextchar back
    LDA #$FF
    BNE L9C45        ; Set result=float, loop to check for more + or -

; int + float
; -----------
L9CA7:
    STX zp27
    JSR LBDEA         ; Unstack integer to IntA
    JSR LBD51
    JSR LA2BE       ; Stack float, convert integer in IntA to float in FPA1
    JMP L9C9B                 ; Jump to do float + <stacked float>

; - numeric
; ---------
L9CB5:
    TAY
    BEQ L9C88             ; If current value is a string, jump to error
    BMI L9CE1                 ; Jump if current value is a float

; Integer subtraction
; -------------------
    JSR L9DCE                 ; Stack current and call Evaluator Level 3
    TAY
    BEQ L9C88             ; int + string, jump to error
    BMI L9CFA                 ; int + float, jump to convert and do real subtraction
    SEC
    LDY #$00
    LDA (zp04),Y
    SBC zp2A
    STA zp2A
    INY
    LDA (zp04),Y
    SBC zp2B
    STA zp2B ; Subtract IntA from top of stack
    INY
    LDA (zp04),Y
    SBC zp2C
    STA zp2C ; Store in IntA
    INY
    LDA (zp04),Y
    SBC zp2D
    JMP L9C77                 ; Jump to pop stack and loop for more + or -

; Real subtraction
; ----------------
L9CE1:
    JSR LBD51
    JSR L9DD1       ; Stack float, call Evaluator Level 3
    TAY
    BEQ L9C88             ; float - string, jump to 'Type mismatch' error
    STX zp27
    BMI L9CF1         ; float - float, skip conversion
    JSR LA2BE                 ; float - int, convert int to float
L9CF1:
    JSR LBD7E                 ; Pop float from stack and point FPTR to it
    JSR LA4FD                 ; Unstack float to FPA2 and subtract it from FPA1
    JMP L9CA1                 ; Jump to set result and loop for more + or -

; int - float
; -----------
L9CFA:
    STX zp27
    JSR LBDEA         ; Unstack integer to IntA
    JSR LBD51
    JSR LA2BE       ; Stack float, convert integer in IntA to float in FPA1
    JSR LBD7E                 ; Pop float from stack, point FPTR to it
    JSR LA4D0                 ; Subtract FPTR float from FPA1 float
    JMP L9CA1                 ; Jump to set result and loop for more + or -

L9D0E:
    JSR LA2BE        ; 9D0E= 20 BE A2     >"
L9D11:
    JSR LBDEA        ; 9D11= 20 EA BD     j=
    JSR LBD51        ; 9D14= 20 51 BD     Q=
    JSR LA2BE        ; 9D17= 20 BE A2     >"
    JMP L9D2C        ; 9D1A= 4C 2C 9D    L,.

L9D1D:
    JSR LA2BE        ; 9D1D= 20 BE A2     >"
L9D20:
    JSR LBD51        ; 9D20= 20 51 BD     Q=
    JSR L9E20        ; 9D23= 20 20 9E      .
    STX zp27          ; 9D26= 86 27       .'
    TAY              ; 9D28= A8          (
    JSR L92FD        ; 9D29= 20 FD 92     }.
L9D2C:
    JSR LBD7E        ; 9D2C= 20 7E BD     ~=
    JSR LA656        ; 9D2F= 20 56 A6     V$
    LDA #$FF         ; 9D32= A9 FF       ).
    LDX zp27          ; 9D34= A6 27       $'
    JMP L9DD4        ; 9D36= 4C D4 9D    LT.

L9D39:
    JMP L8C0E        ; 9D39= 4C 0E 8C    L..

; * <value>
; ---------
L9D3C:
    TAY
    BEQ L9D39             ; If current value is string, jump to error
    BMI L9D20                 ; Jump if current valus ia a float
    LDA zp2D
    CMP zp2C
    BNE L9D1D
    TAY
    BEQ L9D4E
    CMP #$FF
    BNE L9D1D

L9D4E:
    EOR zp2B          ; 9D4E= 45 2B       E+
    BMI L9D1D        ; 9D50= 30 CB       0K
    JSR L9E1D        ; 9D52= 20 1D 9E     ..
    STX zp27          ; 9D55= 86 27       .'
    TAY              ; 9D57= A8          (
    BEQ L9D39        ; 9D58= F0 DF       p_
    BMI L9D11        ; 9D5A= 30 B5       05
    LDA zp2D          ; 9D5C= A5 2D       %-
    CMP zp2C          ; 9D5E= C5 2C       E,
    BNE L9D0E        ; 9D60= D0 AC       P,
    TAY              ; 9D62= A8          (
    BEQ L9D69        ; 9D63= F0 04       p.
    CMP #$FF         ; 9D65= C9 FF       I.
    BNE L9D0E        ; 9D67= D0 A5       P%
L9D69:
    EOR zp2B          ; 9D69= 45 2B       E+
    BMI L9D0E        ; 9D6B= 30 A1       0!
    LDA zp2D          ; 9D6D= A5 2D       %-
    PHA              ; 9D6F= 48          H
    JSR LAD71        ; 9D70= 20 71 AD     q-
    LDX #$39         ; 9D73= A2 39       "9
    JSR LBE44        ; 9D75= 20 44 BE     D>
    JSR LBDEA        ; 9D78= 20 EA BD     j=
    PLA              ; 9D7B= 68          h
    EOR zp2D          ; 9D7C= 45 2D       E-
    STA zp37          ; 9D7E= 85 37       .7
    JSR LAD71        ; 9D80= 20 71 AD     q-
    LDY #$00         ; 9D83= A0 00        .
    LDX #$00         ; 9D85= A2 00       ".
    STY zp3F          ; 9D87= 84 3F       .?
    STY zp40          ; 9D89= 84 40       .@
L9D8B:
    LSR zp3A          ; 9D8B= 46 3A       F

    ROR zp39          ; 9D8D= 66 39       f9
    BCC L9DA6        ; 9D8F= 90 15       ..
    CLC              ; 9D91= 18          .
    TYA              ; 9D92= 98          .
    ADC zp2A          ; 9D93= 65 2A       e*
    TAY              ; 9D95= A8          (
    TXA              ; 9D96= 8A          .
    ADC zp2B          ; 9D97= 65 2B       e+
    TAX              ; 9D99= AA          *
    LDA zp3F          ; 9D9A= A5 3F       %?
    ADC zp2C          ; 9D9C= 65 2C       e,
    STA zp3F          ; 9D9E= 85 3F       .?
    LDA zp40          ; 9DA0= A5 40       %@
    ADC zp2D          ; 9DA2= 65 2D       e-
    STA zp40          ; 9DA4= 85 40       .@
L9DA6:
    ASL zp2A          ; 9DA6= 06 2A       .*
    ROL zp2B          ; 9DA8= 26 2B       $+
    ROL zp2C          ; 9DAA= 26 2C       $,
    ROL zp2D          ; 9DAC= 26 2D       $-
    LDA zp39          ; 9DAE= A5 39       %9
    ORA zp3A          ; 9DB0= 05 3A       .

    BNE L9D8B        ; 9DB2= D0 D7       PW
    STY zp3D          ; 9DB4= 84 3D       .=
    STX zp3E          ; 9DB6= 86 3E       .>
    LDA zp37          ; 9DB8= A5 37       %7
    PHP              ; 9DBA= 08          .

L9DBB:
    LDX #$3D         ; 9DBB= A2 3D       "=
L9DBD:
    JSR LAF56        ; 9DBD= 20 56 AF     V/
    PLP              ; 9DC0= 28          (
    BPL L9DC6        ; 9DC1= 10 03       ..
    JSR LAD93        ; 9DC3= 20 93 AD     .-
L9DC6:
    LDX zp27          ; 9DC6= A6 27       $'
    JMP L9DD4        ; 9DC8= 4C D4 9D    LT.

; * <value>
; ---------
L9DCB:
    JMP L9D3C        ; Bounce back to multiply code


; Stack current value and continue in Evaluator Level 3
; ------------------------------------------------------- 
L9DCE:
    JSR LBD94

; Evaluator Level 3, * / DIV MOD
; ------------------------------
L9DD1:
    JSR L9E20                 ; Call Evaluator Level 2, ^
L9DD4:
    CPX #'*'
    BEQ L9DCB     ; Jump with multiply
    CPX #'/'
    BEQ L9DE5     ; Jump with divide
    CPX #tknMOD
    BEQ L9E01     ; Jump with MOD
    CPX #tknDIV
    BEQ L9E0A
    RTS ; Jump with DIV

; / <value>
; ---------
L9DE5:
    TAY
    JSR L92FD             ; Ensure current value is real
    JSR LBD51
    JSR L9E20       ; Stack float, call Evaluator Level 2
    STX zp27
    TAY
    JSR L92FD     ; Ensure current value is real
    JSR LBD7E
    JSR LA6AD       ; Unstack to FPTR, call divide routine
    LDX zp27
    LDA #$FF
    BNE L9DD4; Set result, loop for more * / MOD DIV

; MOD <value>
; -----------
L9E01:
    JSR L99BE                 ; Ensure current value is integer
    LDA zp38
    PHP
    JMP L9DBB                 ; Jump to MOD routine

; DIV <value>
; -----------
L9E0A:
    JSR L99BE                 ; Ensure current value is integer
    ROL zp39
    ROL zp3A
    ROL zp3B   ; Multiply IntA by 2
    ROL zp3C
    BIT zp37
    PHP
    LDX #$39
    JMP L9DBD        ; Jump to DIV routine


; Stack current integer and evaluate another Level 2
; --------------------------------------------------
L9E1D:
    JSR LBD94                 ; Stack integer

; Evaluator Level 2, ^
; --------------------
L9E20:
    JSR LADEC                 ; Call Evaluator Level 1, - + NOT function ( ) ? ! $ | "
L9E23:
    PHA
L9E24:
    LDY zp1B
    INC zp1B
    LDA (zp19),Y ; Get character
    CMP #$20
    BEQ L9E24          ; Skip spaces
    TAX
    PLA
    CPX #'^'
    BEQ L9E35
    RTS ; Return if not ^

; ^ <value>
; ---------
L9E35:
    TAY
    JSR L92FD             ; Ensure current value is a float
    JSR LBD51
    JSR L92FA       ; Stack float, evaluate a real
    LDA zp30
    CMP #$87
    BCS L9E88
    JSR LA486
    BNE L9E59
    JSR LBD7E
    JSR LA3B5
    LDA zp4A
    JSR LAB12
    LDA #$FF
    BNE L9E23        ; Set result=real, loop to check for more ^

L9E59:
    JSR LA381
    LDA zp04
    STA zp4B
    LDA zp05
    STA zp4C
    JSR LA3B5
    LDA zp4A
    JSR LAB12
L9E6C:
    JSR LA37D
    JSR LBD7E
    JSR LA3B5
    JSR LA801
    JSR LAAD1
    JSR LAA94
    JSR LA7ED
    JSR LA656
    LDA #$FF
    BNE L9E23        ; Set result=real, loop to check for more ^

L9E88:
    JSR LA381
    JSR LA699
    BNE L9E6C


; Convert number to hex string
; ----------------------------
L9E90:
    TYA
    BPL L9E96
    JSR LA3E4  ; Convert real to integer
L9E96:
    LDX #$00
    LDY #$00
L9E9A:
    LDA zp2A,Y          ; abs,y (!)
    PHA          ; Expand four bytes into eight digits
    AND #$0F
    STA zp3F,X
    PLA
    LSR
    LSR
    LSR
    LSR
    INX
    STA zp3F,X
    INX
    INY
    CPY #$04
    BNE L9E9A       ; Loop for four bytes
L9EB0:
    DEX
    BEQ L9EB7            ; No digits left, output a single zero
    LDA zp3F,X
    BEQ L9EB0      ; Skip leading zeros
L9EB7:
    LDA zp3F,X
    CMP #$0A       ; Get byte from workspace
    BCC L9EBF
    ADC #$06       ; Convert byte to hex
L9EBF:
    ADC #'0'
    JSR LA066    ; Convert to digit and store in buffer
    DEX
    BPL L9EB7
    RTS        ; Loop for all digits

; Output nonzero real number
; --------------------------
L9EC8:
    BPL L9ED1                ; Jump forward if positive
    LDA #'-'
    STA zp2E      ; A='-', clear sign flag
    JSR LA066                ; Add '-' to string buffer
L9ED1:
    LDA zp30                  ; Get exponent
    CMP #$81
    BCS L9F25       ; If m*2^1 or larger, number>=1, jump to output it
    JSR LA1F4                ; FloatA=FloatA*10
    DEC zp49
    JMP L9ED1        ; Loop until number is >=1

; Convert numeric value to string
; ===============================
; On entry, FloatA ($2E-$35)  = number
;           or IntA ($2A-$2D) = number
;                           Y = type
;                          @% = print format
;                     $15.b7 set if hex
; Uses,     $37=format type 0/1/2=G/E/F
;           $38=max digits
;           $49
; On exit,  StrA contains string version of number
;           $36=string length
;
L9EDF:
    LDX ws+$0402             ; Get format byte
    CPX #$03
    BCC L9EE8       ; If <3, ok - use it
    LDX #$00                 ; If invalid, $00 for General format
L9EE8:
    STX zp37                  ; Store format type
    LDA ws+$0401
    BEQ L9EF5   ; If digits=0, jump to check format
    CMP #$0A
    BCS L9EF9       ; If 10+ digits, jump to use 10 digits
    BCC L9EFB                ; If <10 digits, use specified number
L9EF5:
    CPX #$02
    BEQ L9EFB       ; If fixed format, use zero digits

; STR$ enters here to use general format
; --------------------------------------
L9EF9:
    LDA #$0A                 ; Otherwise, default to ten digits
L9EFB:
    STA zp38
    STA zp4E          ; Store digit length
    LDA #$00
    STA zp36
    STA zp49 ; Set initial output length to 0, initial exponent to 0
    BIT zp15
    BMI L9E90        ; Jump for hex conversion if $15.b7 set
    TYA
    BMI L9F0F
    JSR LA2BE  ; Convert integer to real
L9F0F:
    JSR LA1DA
    BNE L9EC8      ; Get -1/0/+1 sign, jump if not zero to output nonzero number
    LDA zp37
    BNE L9F1D        ; If not General format, output fixed or exponential zero
    LDA #'0'
    JMP LA066    ; Store single '0' into string buffer and return
L9F1D:
    JMP L9F9C                ; Jump to output zero in fixed or exponential format

L9F20:
    JSR LA699
    BNE L9F34      ; FloatA=1.0

; FloatA now is >=1, check that it is <10
; ---------------------------------------
L9F25:
    CMP #$84
    BCC L9F39       ; Exponent<4, FloatA<10, jump to convert it
    BNE L9F31                ; Exponent<>4, need to divide it
    LDA zp31                  ; Get mantissa top byte
    CMP #$A0
    BCC L9F39       ; Less than $A0, less than ten, jump to convert it
L9F31:
    JSR LA24D                ; FloatA=FloatA / 10
L9F34:
    INC zp49
    JMP L9ED1        ; Jump back to get the number >=1 again

; FloatA is now between 1 and 9.999999999
; ---------------------------------------
L9F39:
    LDA zp35
    STA zp27
    JSR LA385; Copy FloatA to FloatTemp at $27/$046C
    LDA zp4E
    STA zp38          ; Get number of digits
    LDX zp37                  ; Get print format
    CPX #$02
    BNE L9F5C       ; Not fixed format, jump to do exponent/general
    ADC zp49
    BMI L9FA0
    STA zp38
    CMP #$0B
    BCC L9F5C
    LDA #$0A         ; 9F54= A9 0A       ).
    STA zp38          ; 9F56= 85 38       .8
    LDA #$00
    STA zp37 ; 9F5A= 85 37       .7
L9F5C:
    JSR LA686; Clear FloatA
    LDA #$A0         ; 9F5F= A9 A0       )
    STA zp31          ; 9F61= 85 31       .1
    LDA #$83         ; 9F63= A9 83       ).
    STA zp30          ; 9F65= 85 30       .0
    LDX zp38          ; 9F67= A6 38       $8
    BEQ L9F71        ; 9F69= F0 06       p.
L9F6B:
    JSR LA24D; FloatA=FloatA/10
    DEX
    BNE L9F6B
L9F71:
    JSR LA7F5; Point to $46C
    JSR LA34E; Unpack to FloatB
    LDA zp27
    STA zp42
    JSR LA50B; Add
L9F7E:
    LDA zp30
    CMP #$84
    BCS L9F92
    ROR zp31            ; 9F84= 66 31       f1
    ROR zp32            ; 9F86= 66 32       f2
    ROR zp33            ; 9F88= 66 33       f3
    ROR zp34            ; 9F8A= 66 34       f4
    ROR zp35            ; 9F8C= 66 35       f5
    INC zp30            ; 9F8E= E6 30       f0
    BNE L9F7E          ; 9F90= D0 EC       Pl
L9F92:
    LDA zp31            ; 9F92= A5 31       %1
    CMP #$A0
    BCS L9F20 ; 9F96= B0 88       0.
    LDA zp38            ; 9F98= A5 38       %8
    BNE L9FAD          ; 9F9A= D0 11       P.

; Output zero in Exponent or Fixed format
; ---------------------------------------
L9F9C:
    CMP #$01
    BEQ L9FE6        ; 9F9E= F0 46       pF
L9FA0:
    JSR LA686; Clear FloatA
    LDA #$00
    STA zp49   ; 9FA5= 85 49       .I
    LDA zp4E            ; 9FA7= A5 4E       %N
    STA zp38            ; 9FA9= 85 38       .8
    INC zp38            ; 9FAB= E6 38       f8
L9FAD:
    LDA #$01           ; 9FAD= A9 01       ).
    CMP zp37
    BEQ L9FE6  ; 9FB1= F0 33       p3
    LDY zp49
    BMI L9FC3  ; 9FB5= 30 0C       0.
    CPY zp38
    BCS L9FE6  ; 9FB9= B0 2B       0+
    LDA #$00
    STA zp49   ; 9FBD= 85 49       .I
    INY                ; 9FBF= C8          H
    TYA                ; 9FC0= 98          .
    BNE L9FE6          ; 9FC1= D0 23       P#
L9FC3:
    LDA zp37            ; 9FC3= A5 37       %7
    CMP #$02
    BEQ L9FCF ; 9FC7= F0 06       p.
    LDA #$01           ; 9FC9= A9 01       ).
    CPY #$FF
    BNE L9FE6 ; 9FCD= D0 17       P.
L9FCF:
    LDA #'0'
    JSR LA066; Output '0'
    LDA #'.'
    JSR LA066; Output '.'
    LDA #'0'          ; Prepare '0'
L9FDB:
    INC zp49
    BEQ L9FE4
    JSR LA066; Output
    BNE L9FDB

L9FE4:
    LDA #$80         ; 9FE4= A9 80       ).
L9FE6:
    STA zp4E          ; 9FE6= 85 4E       .N
L9FE8:
    JSR LA040        ; 9FE8= 20 40 A0     @
    DEC zp4E          ; 9FEB= C6 4E       FN
    BNE L9FF4        ; 9FED= D0 05       P.
    LDA #$2E         ; 9FEF= A9 2E       ).
    JSR LA066        ; 9FF1= 20 66 A0     f
L9FF4:
    DEC zp38          ; 9FF4= C6 38       F8
    BNE L9FE8        ; 9FF6= D0 F0       Pp
    LDY zp37          ; 9FF8= A4 37       $7
    DEY              ; 9FFA= 88          .
    BEQ LA015        ; 9FFB= F0 18       p.
    DEY              ; 9FFD= 88          .
    BEQ LA011        ; 9FFE= F0 11       p.
    LDY zp36          ; A000= A4 36       $6
LA002:
    DEY              ; A002= 88          .
    LDA ws+$0600,Y   ; A003= B9 00 06    9..
    CMP #'0'
    BEQ LA002        ; A008= F0 F8       px
    CMP #'.'
    BEQ LA00F        ; A00C= F0 01       p.
    INY              ; A00E= C8          H
LA00F:
    STY zp36          ; A00F= 84 36       .6
LA011:
    LDA zp49          ; A011= A5 49       %I
    BEQ LA03F        ; A013= F0 2A       p*
LA015:
    LDA #'E'
    JSR LA066        ; Output 'E'
    LDA zp49
    BPL LA028
    LDA #'-'
    JSR LA066        ; Output '-'
    SEC
    LDA #$00
    SBC zp49         ; Negate
LA028:
    JSR LA052        ; A028= 20 52 A0     R
    LDA zp37          ; A02B= A5 37       %7
    BEQ LA03F        ; A02D= F0 10       p.
    LDA #$20         ; A02F= A9 20       )
    LDY zp49          ; A031= A4 49       $I
    BMI LA038        ; A033= 30 03       0.
    JSR LA066        ; A035= 20 66 A0     f
LA038:
    CPX #$00         ; A038= E0 00       `.
    BNE LA03F        ; A03A= D0 03       P.
    JMP LA066        ; A03C= 4C 66 A0    Lf

LA03F:
    RTS              ; A03F= 60          `

LA040:
    LDA zp31          ; A040= A5 31       %1
    LSR            ; A042= 4A          J
    LSR            ; A043= 4A          J
    LSR            ; A044= 4A          J
    LSR            ; A045= 4A          J
    JSR LA064        ; A046= 20 64 A0     d
    LDA zp31          ; A049= A5 31       %1
    AND #$0F         ; A04B= 29 0F       ).
    STA zp31          ; A04D= 85 31       .1
    JMP LA197        ; A04F= 4C 97 A1    L.!

LA052:
    LDX #$FF         ; A052= A2 FF       ".
    SEC              ; A054= 38          8
LA055:
    INX              ; A055= E8          h
    SBC #$0A         ; A056= E9 0A       i.
    BCS LA055        ; A058= B0 FB       0{
    ADC #$0A         ; A05A= 69 0A       i.
    PHA              ; A05C= 48          H
    TXA              ; A05D= 8A          .
    BEQ LA063        ; A05E= F0 03       p.
    JSR LA064        ; A060= 20 64 A0     d
LA063:
    PLA              ; A063= 68          h
LA064:
    ORA #'0'      ; A064= 09 30       .0

; Store character in string buffer
; --------------------------------
LA066:
    STX zp3B
    LDX zp36
    STA ws+$0600,X ; Store character
    LDX zp3B
    INC zp36
    RTS            ; Increment string length

LA072:
    CLC
    STX zp35
    JSR LA1DA
    LDA #$FF
    RTS

; Scan decimal number
; -------------------
LA07B:
    LDX #$00
    STX zp31
    STX zp32; Clear FloatA
    STX zp33
    STX zp34
    STX zp35
    STX zp48                 ; Clear 'Decimal point' flag
    STX zp49                 ; Set exponent to zero
    CMP #'.'
    BEQ LA0A0   ; Leading decimal point
    CMP #'9'+1
    BCS LA072 ; Not a decimal digit, finish
    SBC #'0'-1
    BMI LA072 ; Convert to binary, if not digit finish
    STA zp35                 ; Store digit
LA099:
    INY
    LDA (zp19),Y         ; Get next character
    CMP #'.'
    BNE LA0A8   ; Not decimal point
LA0A0:
    LDA zp48
    BNE LA0E8       ; Already got decimal point, 
    INC zp48
    BNE LA099       ; Set Decimal Point flag, loop for next
LA0A8:
    CMP #'E'
    BEQ LA0E1   ; Jump to scan exponent
    CMP #'9'+1
    BCS LA0E8 ; Not a digit, jump to finish
    SBC #'0'-1
    BCC LA0E8 ; Not a digit, jump to finish
    LDX zp31                 ; Get mantissa top byte
    CPX #$18
    BCC LA0C2      ; If <25, still small enough to add to
    LDX zp48
    BNE LA099       ; Decimal point found, skip digits to end of number
    INC zp49
    BCS LA099       ; No decumal point, increment exponent and skip digits

LA0C2:
    LDX zp48
    BEQ LA0C8 
    DEC zp49                 ; Decimal point found, decrement exponent
LA0C8:
    JSR LA197               ; Multiply FloatA by 10
    ADC zp35
    STA zp35         ; Add digit to mantissa low byte
    BCC LA099               ; No overflow
    INC zp34
    BNE LA099       ; Add carry through mantissa
    INC zp33
    BNE LA099
    INC zp32
    BNE LA099
    INC zp31
    BNE LA099       ; Loop to check next digit

; Deal with Exponent in scanned number
; ------------------------------------
LA0E1:
    JSR LA140        ; Scan following number
    ADC zp49
    STA zp49  ; Add to current exponent

; End of number found
; -------------------
LA0E8:
    STY zp1B          ; Store PtrB offset
    LDA zp49
    ORA zp48  ; Check exponent and 'decimal point' flag
    BEQ LA11F        ; No exponent, no decimal point, return integer
    JSR LA1DA        ; A0F0= 20 DA A1     Z!
    BEQ LA11B        ; A0F3= F0 26       p$
LA0F5:
    LDA #$A8         ; A0F5= A9 A8       )(
    STA zp30          ; A0F7= 85 30       .0
    LDA #$00         ; A0F9= A9 00       ).
    STA zp2F          ; A0FB= 85 2F       ./
    STA zp2E          ; A0FD= 85 2E       ..
    JSR LA303        ; A0FF= 20 03 A3     .#
    LDA zp49          ; A102= A5 49       %I
    BMI LA111        ; A104= 30 0B       0.
    BEQ LA118        ; A106= F0 10       p.
LA108:
    JSR LA1F4        ; A108= 20 F4 A1     t!
    DEC zp49          ; A10B= C6 49       FI
    BNE LA108        ; A10D= D0 F9       Py
    BEQ LA118        ; A10F= F0 07       p.
LA111:
    JSR LA24D        ; A111= 20 4D A2     M"
    INC zp49          ; A114= E6 49       fI
    BNE LA111        ; A116= D0 F9       Py
LA118:
    JSR LA65C        ; A118= 20 5C A6     \$
LA11B:
    SEC              ; A11B= 38          8
    LDA #$FF         ; A11C= A9 FF       ).
    RTS              ; A11E= 60          `

LA11F:
    LDA zp32
    STA zp2D
    AND #$80
    ORA zp31
    BNE LA0F5
    LDA zp35
    STA zp2A
    LDA zp34
    STA zp2B
    LDA zp33
    STA zp2C
    LDA #$40
    SEC
    RTS

LA139:
    JSR LA14B                ; Scan following number
    EOR #$FF
    SEC
    RTS         ; Negate it, return CS=Ok

; Scan exponent, allows E E+ E- followed by one or two digits
; -----------------------------------------------------------
LA140:
    INY
    LDA (zp19),Y          ; Get next character
    CMP #'-'
    BEQ LA139    ; If '-', jump to scan and negate
    CMP #'+'
    BNE LA14E    ; If '+', just step past
LA14B:
    INY
    LDA (zp19),Y          ; Get next character
LA14E:
    CMP #'9'+1
    BCS LA174  ; Not a digit, exit with CC and A=0
    SBC #'0'-1
    BCC LA174  ; Not a digit, exit with CC and A=0
    STA zp4A                  ; Store exponent digit
    INY
    LDA (zp19),Y          ; Get next character
    CMP #'9'+1
    BCS LA170  ; Not a digit, exit with CC and A=exp
    SBC #'0'-1
    BCC LA170  ; Not a digit, exit with CC and A=exp
    INY
    STA zp43              ; Step past digit, store current digit
    LDA zp4A                  ; Get current exponent
    ASL
    ASL
    ADC zp4A      ; exp=exp*10
    ASL
    ADC zp43            ; exp=exp*10+digit
    RTS

LA170:
    LDA zp4A
    CLC
    RTS          ; Get exp and return CC=Ok

LA174:
    LDA #$00
    CLC
    RTS         ; Return exp=0 and CC=Ok

LA178:
    LDA zp35
    ADC zp42
    STA zp35
    LDA zp34
    ADC zp41
    STA zp34
    LDA zp33
    ADC zp40
    STA zp33
    LDA zp32
    ADC zp3F
    STA zp32
    LDA zp31
    ADC zp3E
    STA zp31
    RTS

LA197:
    PHA
    LDX zp34
    LDA zp31
    PHA
    LDA zp32
    PHA
    LDA zp33
    PHA
    LDA zp35
    ASL
    ROL zp34          ; A1A6= 26 34       $4
    ROL zp33          ; A1A8= 26 33       $3
    ROL zp32          ; A1AA= 26 32       $2
    ROL zp31          ; A1AC= 26 31       $1
    ASL            ; A1AE= 0A          .
    ROL zp34          ; A1AF= 26 34       $4
    ROL zp33          ; A1B1= 26 33       $3
    ROL zp32          ; A1B3= 26 32       $2
    ROL zp31          ; A1B5= 26 31       $1
    ADC zp35          ; A1B7= 65 35       e5
    STA zp35          ; A1B9= 85 35       .5
    TXA              ; A1BB= 8A          .
    ADC zp34          ; A1BC= 65 34       e4
    STA zp34          ; A1BE= 85 34       .4
    PLA              ; A1C0= 68          h
    ADC zp33          ; A1C1= 65 33       e3
    STA zp33          ; A1C3= 85 33       .3
    PLA              ; A1C5= 68          h
    ADC zp32          ; A1C6= 65 32       e2
    STA zp32          ; A1C8= 85 32       .2
    PLA              ; A1CA= 68          h
    ADC zp31          ; A1CB= 65 31       e1
    ASL zp35          ; A1CD= 06 35       .5
    ROL zp34          ; A1CF= 26 34       $4
    ROL zp33          ; A1D1= 26 33       $3
    ROL zp32          ; A1D3= 26 32       $2
    ROL            ; A1D5= 2A          *
    STA zp31          ; A1D6= 85 31       .1
    PLA              ; A1D8= 68          h
    RTS              ; A1D9= 60          `

LA1DA:
    LDA zp31          ; A1DA= A5 31       %1
    ORA zp32          ; A1DC= 05 32       .2
    ORA zp33          ; A1DE= 05 33       .3
    ORA zp34          ; A1E0= 05 34       .4
    ORA zp35          ; A1E2= 05 35       .5
    BEQ LA1ED        ; A1E4= F0 07       p.
    LDA zp2E          ; A1E6= A5 2E       %.
    BNE LA1F3        ; A1E8= D0 09       P.
    LDA #$01         ; A1EA= A9 01       ).
    RTS              ; A1EC= 60          `

LA1ED:
    STA zp2E          ; A1ED= 85 2E       ..
    STA zp30          ; A1EF= 85 30       .0
    STA zp2F          ; A1F1= 85 2F       ./
LA1F3:
    RTS              ; A1F3= 60          `

LA1F4:
    CLC              ; A1F4= 18          .
    LDA zp30          ; A1F5= A5 30       %0
    ADC #$03         ; A1F7= 69 03       i.
    STA zp30          ; A1F9= 85 30       .0
    BCC LA1FF        ; A1FB= 90 02       ..
    INC zp2F          ; A1FD= E6 2F       f/
LA1FF:
    JSR LA21E        ; A1FF= 20 1E A2     ."
    JSR LA242        ; A202= 20 42 A2     B"
    JSR LA242        ; A205= 20 42 A2     B"
LA208:
    JSR LA178        ; A208= 20 78 A1     x!
LA20B:
    BCC LA21D        ; A20B= 90 10       ..
    ROR zp31          ; A20D= 66 31       f1
    ROR zp32          ; A20F= 66 32       f2
    ROR zp33          ; A211= 66 33       f3
    ROR zp34          ; A213= 66 34       f4
    ROR zp35          ; A215= 66 35       f5
    INC zp30          ; A217= E6 30       f0
    BNE LA21D        ; A219= D0 02       P.
    INC zp2F          ; A21B= E6 2F       f/
LA21D:
    RTS              ; A21D= 60          `

LA21E:
    LDA zp2E
LA220:
    STA zp3B
    LDA zp2F
    STA zp3C
    LDA zp30
    STA zp3D
    LDA zp31
    STA zp3E
    LDA zp32
    STA zp3F
    LDA zp33
    STA zp40
    LDA zp34
    STA zp41
    LDA zp35
    STA zp42
    RTS

LA23F:
    JSR LA21E        ; A23F= 20 1E A2     ."
LA242:
    LSR zp3E          ; A242= 46 3E       F>
    ROR zp3F          ; A244= 66 3F       f?
    ROR zp40          ; A246= 66 40       f@
    ROR zp41          ; A248= 66 41       fA
    ROR zp42          ; A24A= 66 42       fB
    RTS              ; A24C= 60          `

LA24D:
    SEC              ; A24D= 38          8
    LDA zp30          ; A24E= A5 30       %0
    SBC #$04         ; A250= E9 04       i.
    STA zp30          ; A252= 85 30       .0
    BCS LA258        ; A254= B0 02       0.
    DEC zp2F          ; A256= C6 2F       F/
LA258:
    JSR LA23F        ; A258= 20 3F A2     ?"
    JSR LA208        ; A25B= 20 08 A2     ."
    JSR LA23F        ; A25E= 20 3F A2     ?"
    JSR LA242        ; A261= 20 42 A2     B"
    JSR LA242        ; A264= 20 42 A2     B"
    JSR LA242        ; A267= 20 42 A2     B"
    JSR LA208        ; A26A= 20 08 A2     ."
    LDA #$00
    STA zp3E
    LDA zp31
    STA zp3F
    LDA zp32
    STA zp40
    LDA zp33
    STA zp41
    LDA zp34
    STA zp42
    LDA zp35
    ROL
    JSR LA208
    LDA #$00
    STA zp3E
    STA zp3F
    LDA zp31
    STA zp40
    LDA zp32
    STA zp41
    LDA zp33
    STA zp42
    LDA zp34
    ROL
    JSR LA208
    LDA zp32
    ROL
    LDA zp31
LA2A4:
    ADC zp35
    STA zp35
    BCC LA2BD
    INC zp34
    BNE LA2BD
    INC zp33
    BNE LA2BD
    INC zp32
    BNE LA2BD
    INC zp31
    BNE LA2BD
    JMP LA20B

LA2BD:
    RTS

LA2BE:
    LDX #$00         ; A2BE= A2 00       ".
    STX zp35          ; A2C0= 86 35       .5
    STX zp2F          ; A2C2= 86 2F       ./
    LDA zp2D          ; A2C4= A5 2D       %-
    BPL LA2CD        ; A2C6= 10 05       ..
    JSR LAD93        ; A2C8= 20 93 AD     .-
    LDX #$FF         ; A2CB= A2 FF       ".
LA2CD:
    STX zp2E          ; A2CD= 86 2E       ..
    LDA zp2A          ; A2CF= A5 2A       %*
    STA zp34          ; A2D1= 85 34       .4
    LDA zp2B          ; A2D3= A5 2B       %+
    STA zp33          ; A2D5= 85 33       .3
    LDA zp2C          ; A2D7= A5 2C       %,
    STA zp32          ; A2D9= 85 32       .2
    LDA zp2D          ; A2DB= A5 2D       %-
    STA zp31          ; A2DD= 85 31       .1
    LDA #$A0         ; A2DF= A9 A0       )
    STA zp30          ; A2E1= 85 30       .0
    JMP LA303        ; A2E3= 4C 03 A3    L.#

LA2E6:
    STA zp2E          ; A2E6= 85 2E       ..
    STA zp30          ; A2E8= 85 30       .0
    STA zp2F          ; A2EA= 85 2F       ./
LA2EC:
    RTS              ; A2EC= 60          `

LA2ED:
    PHA              ; A2ED= 48          H
    JSR LA686        ; A2EE= 20 86 A6     .$
    PLA              ; A2F1= 68          h
    BEQ LA2EC        ; A2F2= F0 F8       px
    BPL LA2FD        ; A2F4= 10 07       ..
    STA zp2E          ; A2F6= 85 2E       ..
    LDA #$00         ; A2F8= A9 00       ).
    SEC              ; A2FA= 38          8
    SBC zp2E          ; A2FB= E5 2E       e.
LA2FD:
    STA zp31          ; A2FD= 85 31       .1
    LDA #$88         ; A2FF= A9 88       ).
    STA zp30          ; A301= 85 30       .0
LA303:
    LDA zp31          ; A303= A5 31       %1
    BMI LA2EC        ; A305= 30 E5       0e
    ORA zp32          ; A307= 05 32       .2
    ORA zp33          ; A309= 05 33       .3
    ORA zp34          ; A30B= 05 34       .4
    ORA zp35          ; A30D= 05 35       .5
    BEQ LA2E6        ; A30F= F0 D5       pU
    LDA zp30          ; A311= A5 30       %0
LA313:
    LDY zp31          ; A313= A4 31       $1
    BMI LA2EC        ; A315= 30 D5       0U
    BNE LA33A        ; A317= D0 21       P!
    LDX zp32          ; A319= A6 32       $2
    STX zp31          ; A31B= 86 31       .1
    LDX zp33          ; A31D= A6 33       $3
    STX zp32          ; A31F= 86 32       .2
    LDX zp34          ; A321= A6 34       $4
    STX zp33          ; A323= 86 33       .3
    LDX zp35          ; A325= A6 35       $5
    STX zp34          ; A327= 86 34       .4
    STY zp35          ; A329= 84 35       .5
    SEC              ; A32B= 38          8
    SBC #$08         ; A32C= E9 08       i.
    STA zp30          ; A32E= 85 30       .0
    BCS LA313        ; A330= B0 E1       0a
    DEC zp2F          ; A332= C6 2F       F/
    BCC LA313        ; A334= 90 DD       .]
LA336:
    LDY zp31          ; A336= A4 31       $1
    BMI LA2EC        ; A338= 30 B2       02
LA33A:
    ASL zp35          ; A33A= 06 35       .5
    ROL zp34          ; A33C= 26 34       $4
    ROL zp33          ; A33E= 26 33       $3
    ROL zp32          ; A340= 26 32       $2
    ROL zp31          ; A342= 26 31       $1
    SBC #$00         ; A344= E9 00       i.
    STA zp30          ; A346= 85 30       .0
    BCS LA336        ; A348= B0 EC       0l
    DEC zp2F          ; A34A= C6 2F       F/
    BCC LA336        ; A34C= 90 E8       .h
LA34E:
    LDY #$04         ; A34E= A0 04        .
    LDA (zp4B),Y      ; A350= B1 4B       1K
    STA zp41          ; A352= 85 41       .A
    DEY              ; A354= 88          .
    LDA (zp4B),Y      ; A355= B1 4B       1K
    STA zp40          ; A357= 85 40       .@
    DEY              ; A359= 88          .
    LDA (zp4B),Y      ; A35A= B1 4B       1K
    STA zp3F          ; A35C= 85 3F       .?
    DEY              ; A35E= 88          .
    LDA (zp4B),Y      ; A35F= B1 4B       1K
    STA zp3B          ; A361= 85 3B       .;
    DEY              ; A363= 88          .
    STY zp42          ; A364= 84 42       .B
    STY zp3C          ; A366= 84 3C       .<
    LDA (zp4B),Y      ; A368= B1 4B       1K
    STA zp3D          ; A36A= 85 3D       .=
    ORA zp3B          ; A36C= 05 3B       .;
    ORA zp3F          ; A36E= 05 3F       .?
    ORA zp40          ; A370= 05 40       .@
    ORA zp41          ; A372= 05 41       .A
    BEQ LA37A        ; A374= F0 04       p.
    LDA zp3B          ; A376= A5 3B       %;
    ORA #$80         ; A378= 09 80       ..
LA37A:
    STA zp3E          ; A37A= 85 3E       .>
    RTS              ; A37C= 60          `

LA37D:
    LDA #$71         ; A37D= A9 71       )q
    BNE LA387        ; A37F= D0 06       P.
LA381:
    LDA #$76         ; A381= A9 76       )v
    BNE LA387        ; A383= D0 02       P.
LA385:
    LDA #$6C         ; A385= A9 6C       )l
LA387:
    STA zp4B          ; A387= 85 4B       .K
    LDA #$04+(ws/256); A389= A9 04       ).
    STA zp4C          ; A38B= 85 4C       .L
LA38D:
    LDY #$00         ; A38D= A0 00        .
    LDA zp30          ; A38F= A5 30       %0
    STA (zp4B),Y      ; A391= 91 4B       .K
    INY              ; A393= C8          H
    LDA zp2E          ; A394= A5 2E       %.
    AND #$80         ; A396= 29 80       ).
    STA zp2E          ; A398= 85 2E       ..
    LDA zp31          ; A39A= A5 31       %1
    AND #$7F         ; A39C= 29 7F       ).
    ORA zp2E          ; A39E= 05 2E       ..
    STA (zp4B),Y      ; A3A0= 91 4B       .K
    LDA zp32          ; A3A2= A5 32       %2
    INY              ; A3A4= C8          H
    STA (zp4B),Y      ; A3A5= 91 4B       .K
    LDA zp33          ; A3A7= A5 33       %3
    INY              ; A3A9= C8          H
    STA (zp4B),Y      ; A3AA= 91 4B       .K
    LDA zp34          ; A3AC= A5 34       %4
    INY              ; A3AE= C8          H
    STA (zp4B),Y      ; A3AF= 91 4B       .K
    RTS              ; A3B1= 60          `

LA3B2:
    JSR LA7F5        ; A3B2= 20 F5 A7     u'
LA3B5:
    LDY #$04         ; A3B5= A0 04        .
    LDA (zp4B),Y      ; A3B7= B1 4B       1K
    STA zp34          ; A3B9= 85 34       .4
    DEY              ; A3BB= 88          .
    LDA (zp4B),Y      ; A3BC= B1 4B       1K
    STA zp33          ; A3BE= 85 33       .3
    DEY              ; A3C0= 88          .
    LDA (zp4B),Y      ; A3C1= B1 4B       1K
    STA zp32          ; A3C3= 85 32       .2
    DEY              ; A3C5= 88          .
    LDA (zp4B),Y      ; A3C6= B1 4B       1K
    STA zp2E          ; A3C8= 85 2E       ..
    DEY              ; A3CA= 88          .
    LDA (zp4B),Y      ; A3CB= B1 4B       1K
    STA zp30          ; A3CD= 85 30       .0
    STY zp35          ; A3CF= 84 35       .5
    STY zp2F          ; A3D1= 84 2F       ./
    ORA zp2E          ; A3D3= 05 2E       ..
    ORA zp32          ; A3D5= 05 32       .2
    ORA zp33          ; A3D7= 05 33       .3
    ORA zp34          ; A3D9= 05 34       .4
    BEQ LA3E1        ; A3DB= F0 04       p.
    LDA zp2E          ; A3DD= A5 2E       %.
    ORA #$80         ; A3DF= 09 80       ..
LA3E1:
    STA zp31          ; A3E1= 85 31       .1
    RTS              ; A3E3= 60          `

; Convert real to integer
; =======================
LA3E4:
    JSR LA3FE                 ; Convert real to integer
LA3E7:
    LDA zp31
    STA zp2D           ; Copy to Integer Accumulator
    LDA zp32
    STA zp2C
    LDA zp33
    STA zp2B
    LDA zp34
    STA zp2A
    RTS

LA3F8:
    JSR LA21E                ; Copy FloatA to FloatB
    JMP LA686                ; Set FloatA to zero and return

; Convert float to integer
; ========================
; On entry, FloatA ($30-$34) holds a float
; On exit,  FloatA ($30-$34) holds integer part
; ---------------------------------------------
; The real value is partially denormalised by repeatedly dividing the mantissa
; by 2 and incrementing the exponent to multiply the number by 2, until the
; exponent is $80, indicating that we have got to mantissa * 2^0.
;
LA3FE:
    LDA zp30
    BPL LA3F8        ; Exponent<$80, number<1, jump to return 0
    JSR LA453                ; Set $3B-$42 to zero
    JSR LA1DA
    BNE LA43C
    BEQ LA468

LA40C:
    LDA zp30                  ; Get exponent
    CMP #$A0
    BCS LA466       ; Exponent is +32, float has been denormalised to an integer
    CMP #$99
    BCS LA43C       ; Loop to keep dividing
    ADC #$08
    STA zp30         ; Increment exponent by 8
    LDA zp40
    STA zp41
    LDA zp3F
    STA zp40
    LDA zp3E
    STA zp3F
    LDA zp34
    STA zp3E
    LDA zp33
    STA zp34          ; Divide mantissa by 2^8
    LDA zp32
    STA zp33
    LDA zp31
    STA zp32
    LDA #$00
    STA zp31
    BEQ LA40C                ; Loop to keep dividing

LA43C:
    LSR zp31
    ROR zp32
    ROR zp33
    ROR zp34
    ROR zp3E
    ROR zp3F
    ROR zp40
    ROR zp41
    INC zp30
    BNE LA40C
LA450:
    JMP LA66C

LA453:
    LDA #$00
    STA zp3B
    STA zp3C
    STA zp3D
    STA zp3E
    STA zp3F
    STA zp40
    STA zp41
    STA zp42
    RTS

LA466:
    BNE LA450                ; Exponent>32, jump to 'Too big' error
LA468:
    LDA zp2E
    BPL LA485        ; If positive, jump to return
LA46C:
    SEC                      ; Negate the mantissa to get integer
    LDA #$00
    SBC zp34
    STA zp34
    LDA #$00
    SBC zp33
    STA zp33
    LDA #$00
    SBC zp32
    STA zp32
    LDA #$00
    SBC zp31
    STA zp31
LA485:
    RTS

LA486:
    LDA zp30
    BMI LA491
    LDA #$00
    STA zp4A
    JMP LA1DA

LA491:
    JSR LA3FE        ; A491= 20 FE A3     ~#
    LDA zp34
    STA zp4A  ; A496= 85 4A       .J
    JSR LA4E8        ; A498= 20 E8 A4     h$
    LDA #$80
    STA zp30 ; A49D= 85 30       .0
    LDX zp31
    BPL LA4B3; A4A1= 10 10       ..
    EOR zp2E
    STA zp2E  ; A4A5= 85 2E       ..
    BPL LA4AE        ; A4A7= 10 05       ..
    INC zp4A
    JMP LA4B0; A4AB= 4C B0 A4    L0$

LA4AE:
    DEC zp4A          ; A4AE= C6 4A       FJ
LA4B0:
    JSR LA46C        ; A4B0= 20 6C A4     l$
LA4B3:
    JMP LA303        ; A4B3= 4C 03 A3    L.#

LA4B6:
    INC zp34
    BNE LA4C6        ; A4B8= D0 0C       P.
    INC zp33
    BNE LA4C6        ; A4BC= D0 08       P.
    INC zp32
    BNE LA4C6        ; A4C0= D0 04       P.
    INC zp31
    BEQ LA450        ; A4C4= F0 8A       p.
LA4C6:
    RTS              ; A4C6= 60          `

LA4C7:
    JSR LA46C        ; A4C7= 20 6C A4     l$
    JSR LA4B6        ; A4CA= 20 B6 A4     6$
    JMP LA46C        ; A4CD= 4C 6C A4    Ll$

LA4D0:
    JSR LA4FD        ; A4D0= 20 FD A4     }$
    JMP LAD7E        ; A4D3= 4C 7E AD    L~-

LA4D6:
    JSR LA34E        ; A4D6= 20 4E A3     N#
    JSR LA38D        ; A4D9= 20 8D A3     .#
LA4DC:
    LDA zp3B
    STA zp2E          ; A4DE= 85 2E       ..
    LDA zp3C
    STA zp2F          ; A4E2= 85 2F       ./
    LDA zp3D
    STA zp30          ; A4E6= 85 30       .0
LA4E8:
    LDA zp3E
    STA zp31          ; A4EA= 85 31       .1
    LDA zp3F
    STA zp32          ; A4EE= 85 32       .2
    LDA zp40
    STA zp33          ; A4F2= 85 33       .3
    LDA zp41
    STA zp34          ; A4F6= 85 34       .4
    LDA zp42
    STA zp35          ; A4FA= 85 35       .5
LA4FC:
    RTS              ; A4FC= 60          `

LA4FD:
    JSR LAD7E        ; A4FD= 20 7E AD     ~-
LA500:
    JSR LA34E        ; A500= 20 4E A3     N#
    BEQ LA4FC        ; A503= F0 F7       pw
LA505:
    JSR LA50B        ; A505= 20 0B A5     .%
    JMP LA65C        ; A508= 4C 5C A6    L\$

LA50B:
    JSR LA1DA        ; A50B= 20 DA A1     Z!
    BEQ LA4DC        ; A50E= F0 CC       pL
    LDY #$00         ; A510= A0 00        .
    SEC              ; A512= 38          8
    LDA zp30          ; A513= A5 30       %0
    SBC zp3D          ; A515= E5 3D       e=
    BEQ LA590        ; A517= F0 77       pw
    BCC LA552        ; A519= 90 37       .7
    CMP #$25         ; A51B= C9 25       I%
    BCS LA4FC        ; A51D= B0 DD       0]
    PHA              ; A51F= 48          H
    AND #$38         ; A520= 29 38       )8
    BEQ LA53D        ; A522= F0 19       p.
    LSR            ; A524= 4A          J
    LSR            ; A525= 4A          J
    LSR            ; A526= 4A          J
    TAX              ; A527= AA          *
LA528:
    LDA zp41
    STA zp42      ; A52A= 85 42       .B
    LDA zp40
    STA zp41      ; A52E= 85 41       .A
    LDA zp3F
    STA zp40      ; A532= 85 40       .@
    LDA zp3E
    STA zp3F      ; A536= 85 3F       .?
    STY zp3E              ; A538= 84 3E       .>
    DEX
    BNE LA528        ; A53B= D0 EB       Pk
LA53D:
    PLA              ; A53D= 68          h
    AND #$07         ; A53E= 29 07       ).
    BEQ LA590        ; A540= F0 4E       pN
    TAX              ; A542= AA          *
LA543:
    LSR zp3E          ; A543= 46 3E       F>
    ROR zp3F          ; A545= 66 3F       f?
    ROR zp40          ; A547= 66 40       f@
    ROR zp41          ; A549= 66 41       fA
    ROR zp42          ; A54B= 66 42       fB
    DEX              ; A54D= CA          J
    BNE LA543        ; A54E= D0 F3       Ps
    BEQ LA590        ; A550= F0 3E       p>
LA552:
    SEC              ; A552= 38          8
    LDA zp3D          ; A553= A5 3D       %=
    SBC zp30          ; A555= E5 30       e0
    CMP #$25         ; A557= C9 25       I%
    BCS LA4DC        ; A559= B0 81       0.
    PHA              ; A55B= 48          H
    AND #$38         ; A55C= 29 38       )8
    BEQ LA579        ; A55E= F0 19       p.
    LSR            ; A560= 4A          J
    LSR            ; A561= 4A          J
    LSR            ; A562= 4A          J
    TAX              ; A563= AA          *
LA564:
    LDA zp34          ; A564= A5 34       %4
    STA zp35          ; A566= 85 35       .5
    LDA zp33          ; A568= A5 33       %3
    STA zp34          ; A56A= 85 34       .4
    LDA zp32          ; A56C= A5 32       %2
    STA zp33          ; A56E= 85 33       .3
    LDA zp31          ; A570= A5 31       %1
    STA zp32          ; A572= 85 32       .2
    STY zp31          ; A574= 84 31       .1
    DEX              ; A576= CA          J
    BNE LA564        ; A577= D0 EB       Pk
LA579:
    PLA              ; A579= 68          h
    AND #$07         ; A57A= 29 07       ).
    BEQ LA58C        ; A57C= F0 0E       p.
    TAX              ; A57E= AA          *
LA57F:
    LSR zp31          ; A57F= 46 31       F1
    ROR zp32          ; A581= 66 32       f2
    ROR zp33          ; A583= 66 33       f3
    ROR zp34          ; A585= 66 34       f4
    ROR zp35          ; A587= 66 35       f5
    DEX              ; A589= CA          J
    BNE LA57F        ; A58A= D0 F3       Ps
LA58C:
    LDA zp3D          ; A58C= A5 3D       %=
    STA zp30          ; A58E= 85 30       .0
LA590:
    LDA zp2E          ; A590= A5 2E       %.
    EOR zp3B          ; A592= 45 3B       E;
    BPL LA5DF        ; A594= 10 49       .I
    LDA zp31          ; A596= A5 31       %1
    CMP zp3E          ; A598= C5 3E       E>
    BNE LA5B7        ; A59A= D0 1B       P.
    LDA zp32          ; A59C= A5 32       %2
    CMP zp3F          ; A59E= C5 3F       E?
    BNE LA5B7        ; A5A0= D0 15       P.
    LDA zp33          ; A5A2= A5 33       %3
    CMP zp40          ; A5A4= C5 40       E@
    BNE LA5B7        ; A5A6= D0 0F       P.
    LDA zp34          ; A5A8= A5 34       %4
    CMP zp41          ; A5AA= C5 41       EA
    BNE LA5B7        ; A5AC= D0 09       P.
    LDA zp35          ; A5AE= A5 35       %5
    CMP zp42          ; A5B0= C5 42       EB
    BNE LA5B7        ; A5B2= D0 03       P.
    JMP LA686        ; A5B4= 4C 86 A6    L.$

LA5B7:
    BCS LA5E3        ; A5B7= B0 2A       0*
    SEC              ; A5B9= 38          8
    LDA zp42          ; A5BA= A5 42       %B
    SBC zp35          ; A5BC= E5 35       e5
    STA zp35          ; A5BE= 85 35       .5
    LDA zp41          ; A5C0= A5 41       %A
    SBC zp34          ; A5C2= E5 34       e4
    STA zp34          ; A5C4= 85 34       .4
    LDA zp40          ; A5C6= A5 40       %@
    SBC zp33          ; A5C8= E5 33       e3
    STA zp33          ; A5CA= 85 33       .3
    LDA zp3F          ; A5CC= A5 3F       %?
    SBC zp32          ; A5CE= E5 32       e2
    STA zp32          ; A5D0= 85 32       .2
    LDA zp3E          ; A5D2= A5 3E       %>
    SBC zp31          ; A5D4= E5 31       e1
    STA zp31          ; A5D6= 85 31       .1
    LDA zp3B          ; A5D8= A5 3B       %;
    STA zp2E          ; A5DA= 85 2E       ..
    JMP LA303        ; A5DC= 4C 03 A3    L.#

LA5DF:
    CLC              ; A5DF= 18          .
    JMP LA208        ; A5E0= 4C 08 A2    L."

LA5E3:
    SEC              ; A5E3= 38          8
    LDA zp35          ; A5E4= A5 35       %5
    SBC zp42          ; A5E6= E5 42       eB
    STA zp35          ; A5E8= 85 35       .5
    LDA zp34          ; A5EA= A5 34       %4
    SBC zp41          ; A5EC= E5 41       eA
    STA zp34          ; A5EE= 85 34       .4
    LDA zp33          ; A5F0= A5 33       %3
    SBC zp40          ; A5F2= E5 40       e@
    STA zp33          ; A5F4= 85 33       .3
    LDA zp32          ; A5F6= A5 32       %2
    SBC zp3F          ; A5F8= E5 3F       e?
    STA zp32          ; A5FA= 85 32       .2
    LDA zp31          ; A5FC= A5 31       %1
    SBC zp3E          ; A5FE= E5 3E       e>
    STA zp31          ; A600= 85 31       .1
    JMP LA303        ; A602= 4C 03 A3    L.#

LA605:
    RTS              ; A605= 60          `

LA606:
    JSR LA1DA        ; A606= 20 DA A1     Z!
    BEQ LA605        ; A609= F0 FA       pz
    JSR LA34E        ; A60B= 20 4E A3     N#
    BNE LA613        ; A60E= D0 03       P.
    JMP LA686        ; A610= 4C 86 A6    L.$

LA613:
    CLC              ; A613= 18          .
    LDA zp30          ; A614= A5 30       %0
    ADC zp3D          ; A616= 65 3D       e=
    BCC LA61D        ; A618= 90 03       ..
    INC zp2F          ; A61A= E6 2F       f/
    CLC              ; A61C= 18          .
LA61D:
    SBC #$7F         ; A61D= E9 7F       i.
    STA zp30          ; A61F= 85 30       .0
    BCS LA625        ; A621= B0 02       0.
    DEC zp2F          ; A623= C6 2F       F/
LA625:
    LDX #$05         ; A625= A2 05       ".
    LDY #$00         ; A627= A0 00        .
LA629:
    LDA zp30,X        ; A629= B5 30       50
    STA zp42,X        ; A62B= 95 42       .B
    STY zp30,X        ; A62D= 94 30       .0
    DEX              ; A62F= CA          J
    BNE LA629        ; A630= D0 F7       Pw
    LDA zp2E          ; A632= A5 2E       %.
    EOR zp3B          ; A634= 45 3B       E;
    STA zp2E          ; A636= 85 2E       ..
    LDY #$20         ; A638= A0 20
LA63A:
    LSR zp3E          ; A63A= 46 3E       F>
    ROR zp3F          ; A63C= 66 3F       f?
    ROR zp40          ; A63E= 66 40       f@
    ROR zp41          ; A640= 66 41       fA
    ROR zp42          ; A642= 66 42       fB
    ASL zp46          ; A644= 06 46       .F
    ROL zp45          ; A646= 26 45       $E
    ROL zp44          ; A648= 26 44       $D
    ROL zp43          ; A64A= 26 43       $C
    BCC LA652        ; A64C= 90 04       ..
    CLC              ; A64E= 18          .
    JSR LA178        ; A64F= 20 78 A1     x!
LA652:
    DEY              ; A652= 88          .
    BNE LA63A        ; A653= D0 E5       Pe
    RTS              ; A655= 60          `

LA656:
    JSR LA606        ; A656= 20 06 A6     .$
LA659:
    JSR LA303        ; A659= 20 03 A3     .#
LA65C:
    LDA zp35          ; A65C= A5 35       %5
    CMP #$80         ; A65E= C9 80       I.
    BCC LA67C        ; A660= 90 1A       ..
    BEQ LA676        ; A662= F0 12       p.
    LDA #$FF         ; A664= A9 FF       ).
    JSR LA2A4        ; A666= 20 A4 A2     $"
    JMP LA67C        ; A669= 4C 7C A6    L|$

LA66C:
    BRK
    dta $14
    FNfold 'Too big'
    BRK
LA676:
    LDA zp34          ; A676= A5 34       %4
    ORA #$01         ; A678= 09 01       ..
    STA zp34          ; A67A= 85 34       .4
LA67C:
    LDA #$00         ; A67C= A9 00       ).
    STA zp35          ; A67E= 85 35       .5
    LDA zp2F          ; A680= A5 2F       %/
    BEQ LA698        ; A682= F0 14       p.
    BPL LA66C        ; A684= 10 E6       .f
LA686:
    LDA #$00         ; A686= A9 00       ).
    STA zp2E          ; A688= 85 2E       ..
    STA zp2F          ; A68A= 85 2F       ./
    STA zp30          ; A68C= 85 30       .0
    STA zp31          ; A68E= 85 31       .1
    STA zp32          ; A690= 85 32       .2
    STA zp33          ; A692= 85 33       .3
    STA zp34          ; A694= 85 34       .4
    STA zp35          ; A696= 85 35       .5
LA698:
    RTS              ; A698= 60          `

LA699:
    JSR LA686        ; A699= 20 86 A6     .$
    LDY #$80         ; A69C= A0 80        .
    STY zp31          ; A69E= 84 31       .1
    INY              ; A6A0= C8          H
    STY zp30          ; A6A1= 84 30       .0
    TYA              ; A6A3= 98          .
    RTS              ; A6A4= 60          `

LA6A5:
    JSR LA385        ; A6A5= 20 85 A3     .#
    JSR LA699        ; A6A8= 20 99 A6     .$
    BNE LA6E7        ; A6AB= D0 3A       P

LA6AD:
    JSR LA1DA        ; A6AD= 20 DA A1     Z!
    BEQ LA6BB        ; A6B0= F0 09       p.
    JSR LA21E        ; A6B2= 20 1E A2     ."
    JSR LA3B5        ; A6B5= 20 B5 A3     5#
    BNE LA6F1        ; A6B8= D0 37       P7
    RTS              ; A6BA= 60          `

LA6BB:
    JMP L99A7        ; A6BB= 4C A7 99    L'.

; =TAN numeric
; ============
LA6BE:
    JSR L92FA        ; A6BE= 20 FA 92     z.
    JSR LA9D3        ; A6C1= 20 D3 A9     S)
    LDA zp4A          ; A6C4= A5 4A       %J
    PHA              ; A6C6= 48          H
    JSR LA7E9        ; A6C7= 20 E9 A7     i'
    JSR LA38D        ; A6CA= 20 8D A3     .#
    INC zp4A          ; A6CD= E6 4A       fJ
    JSR LA99E        ; A6CF= 20 9E A9     .)
    JSR LA7E9        ; A6D2= 20 E9 A7     i'
    JSR LA4D6        ; A6D5= 20 D6 A4     V$
    PLA              ; A6D8= 68          h
    STA zp4A          ; A6D9= 85 4A       .J
    JSR LA99E        ; A6DB= 20 9E A9     .)
    JSR LA7E9        ; A6DE= 20 E9 A7     i'
    JSR LA6E7        ; A6E1= 20 E7 A6     g$
    LDA #$FF         ; A6E4= A9 FF       ).
    RTS              ; A6E6= 60          `

LA6E7:
    JSR LA1DA        ; A6E7= 20 DA A1     Z!
    BEQ LA698        ; A6EA= F0 AC       p,
    JSR LA34E        ; A6EC= 20 4E A3     N#
    BEQ LA6BB        ; A6EF= F0 CA       pJ
LA6F1:
    LDA zp2E          ; A6F1= A5 2E       %.
    EOR zp3B          ; A6F3= 45 3B       E;
    STA zp2E          ; A6F5= 85 2E       ..
    SEC              ; A6F7= 38          8
    LDA zp30          ; A6F8= A5 30       %0
    SBC zp3D          ; A6FA= E5 3D       e=
    BCS LA701        ; A6FC= B0 03       0.
    DEC zp2F          ; A6FE= C6 2F       F/
    SEC              ; A700= 38          8
LA701:
    ADC #$80         ; A701= 69 80       i.
    STA zp30          ; A703= 85 30       .0
    BCC LA70A        ; A705= 90 03       ..
    INC zp2F          ; A707= E6 2F       f/
    CLC              ; A709= 18          .
LA70A:
    LDX #$20         ; A70A= A2 20       "
LA70C:
    BCS LA726
    LDA zp31
    CMP zp3E
    BNE LA724
    LDA zp32
    CMP zp3F
    BNE LA724
    LDA zp33
    CMP zp40
    BNE LA724
    LDA zp34
    CMP zp41
LA724:
    BCC LA73F
LA726:
    LDA zp34
    SBC zp41
    STA zp34
    LDA zp33
    SBC zp40
    STA zp33
    LDA zp32
    SBC zp3F
    STA zp32
    LDA zp31
    SBC zp3E
    STA zp31
    SEC
LA73F:
    ROL zp46
    ROL zp45
    ROL zp44
    ROL zp43
    ASL zp34
    ROL zp33
    ROL zp32
    ROL zp31
    DEX
    BNE LA70C
    LDX #$07
LA754:
    BCS LA76E
    LDA zp31
    CMP zp3E
    BNE LA76C
    LDA zp32
    CMP zp3F
    BNE LA76C
    LDA zp33
    CMP zp40
    BNE LA76C
    LDA zp34
    CMP zp41
LA76C:
    BCC LA787
LA76E:
    LDA zp34
    SBC zp41
    STA zp34
    LDA zp33
    SBC zp40
    STA zp33
    LDA zp32
    SBC zp3F
    STA zp32
    LDA zp31
    SBC zp3E
    STA zp31
    SEC
LA787:
    ROL zp35          ; A787= 26 35       $5
    ASL zp34          ; A789= 06 34       .4
    ROL zp33          ; A78B= 26 33       $3
    ROL zp32          ; A78D= 26 32       $2
    ROL zp31          ; A78F= 26 31       $1
    DEX              ; A791= CA          J
    BNE LA754        ; A792= D0 C0       P@
    ASL zp35          ; A794= 06 35       .5
    LDA zp46
    STA zp34
    LDA zp45
    STA zp33
    LDA zp44
    STA zp32
    LDA zp43
    STA zp31
    JMP LA659

LA7A9:
    BRK
    dta $15
    FNfold '-ve root'
    BRK

; =SQR numeric
; ============
LA7B4:
    JSR L92FA        ; A7B4= 20 FA 92     z.
LA7B7:
    JSR LA1DA        ; A7B7= 20 DA A1     Z!
    BEQ LA7E6        ; A7BA= F0 2A       p*
    BMI LA7A9        ; A7BC= 30 EB       0k
    JSR LA385        ; A7BE= 20 85 A3     .#
    LDA zp30          ; A7C1= A5 30       %0
    LSR            ; A7C3= 4A          J
    ADC #$40         ; A7C4= 69 40       i@
    STA zp30          ; A7C6= 85 30       .0
    LDA #$05         ; A7C8= A9 05       ).
    STA zp4A          ; A7CA= 85 4A       .J
    JSR LA7ED        ; A7CC= 20 ED A7     m'
LA7CF:
    JSR LA38D        ; A7CF= 20 8D A3     .#
    LDA #$6C         ; A7D2= A9 6C       )l
    STA zp4B          ; A7D4= 85 4B       .K
    JSR LA6AD        ; A7D6= 20 AD A6     -$
    LDA #$71         ; A7D9= A9 71       )q
    STA zp4B          ; A7DB= 85 4B       .K
    JSR LA500        ; A7DD= 20 00 A5     .%
    DEC zp30          ; A7E0= C6 30       F0
    DEC zp4A          ; A7E2= C6 4A       FJ
    BNE LA7CF        ; A7E4= D0 E9       Pi
LA7E6:
    LDA #$FF         ; A7E6= A9 FF       ).
    RTS              ; A7E8= 60          `

; Point $4B/C to a floating point temp
; ------------------------------------
LA7E9:
    LDA #$7B
    BNE LA7F7  ; ws+$047B-7F FPTEMP4
LA7ED:
    LDA #$71
    BNE LA7F7  ; ws+$0471-75 FPTEMP2
LA7F1:
    LDA #$76
    BNE LA7F7  ; ws+$0476-7A FPTEMP3
LA7F5:
    LDA #$6C            ; ws+$046C-70 FPTEMP1


LA7F7:
    STA zp4B                   ; $4B/C=>FPTEMP
    LDA #$04+(ws/256)
    STA zp4C
    RTS

; =LN numeric
; ===========
LA7FE:
    JSR L92FA        ; A7FE= 20 FA 92     z.
LA801:
    JSR LA1DA        ; A801= 20 DA A1     Z!
    BEQ LA808        ; A804= F0 02       p.
    BPL LA814        ; A806= 10 0C       ..
LA808:
    .if foldup == 0
        BRK
        dta $16
        dta 'Log range'
        BRK
    .elseif foldup != 0
        BRK
        dta $16
        dta tknLOG
        FNfold ' range'
        BRK
    .endif
LA814:
    JSR LA453        ; A814= 20 53 A4     S$
    LDY #$80         ; A817= A0 80        .
    STY zp3B          ; A819= 84 3B       .;
    STY zp3E          ; A81B= 84 3E       .>
    INY              ; A81D= C8          H
    STY zp3D          ; A81E= 84 3D       .=
    LDX zp30          ; A820= A6 30       $0
    BEQ LA82A        ; A822= F0 06       p.
    LDA zp31          ; A824= A5 31       %1
    CMP #$B5         ; A826= C9 B5       I5
    BCC LA82C        ; A828= 90 02       ..
LA82A:
    INX              ; A82A= E8          h
    DEY              ; A82B= 88          .
LA82C:
    TXA              ; A82C= 8A          .
    PHA              ; A82D= 48          H
    STY zp30          ; A82E= 84 30       .0
    JSR LA505        ; A830= 20 05 A5     .%
    LDA #$7B         ; A833= A9 7B       ){
    JSR LA387        ; A835= 20 87 A3     .#
    LDA #<LA873
    LDY #>LA873
    JSR LA897        ; A83C= 20 97 A8     .(
    JSR LA7E9        ; A83F= 20 E9 A7     i'
    JSR LA656        ; A842= 20 56 A6     V$
    JSR LA656        ; A845= 20 56 A6     V$
    JSR LA500        ; A848= 20 00 A5     .%
    JSR LA385        ; A84B= 20 85 A3     .#
    PLA              ; A84E= 68          h
    SEC              ; A84F= 38          8
    SBC #$81         ; A850= E9 81       i.
    JSR LA2ED        ; A852= 20 ED A2     m"
    LDA #<LA86E
    STA zp4B             ; A857= 85 4B       .K
    LDA #>LA86E
    STA zp4C          ; A85B= 85 4C       .L
    JSR LA656        ; A85D= 20 56 A6     V$
    JSR LA7F5        ; A860= 20 F5 A7     u'
    JSR LA500        ; A863= 20 00 A5     .%
    LDA #$FF         ; A866= A9 FF       ).
    RTS              ; A868= 60          `

LA869:
    .if version < 3
        dta $7F
        dta $5E
        dta $5B
        dta $D8
        dta $AA
    .endif
LA86E:
    dta $80
    dta $31
    dta $72
    dta $17
    dta $F8
LA873:
    dta $06
    dta $7A
    dta $12

LA876:                                      ; never referenced?
    dta $38, $A5, $0B, $88, $79, $0E, $9F
    dta $F3         ; A87D= F3          s
    dta $7C
    dta $2A
    dta $AC
    dta $3F
    dta $B5
    dta $86
    dta $34
    dta $01; A884= 34 01       4.
    dta $a2, $7a
    dta $7F
    dta $63
    dta $8E
    dta $37
    dta $EC; A88B= 37 EC       7l
    dta $82         ; A88D= 82          .
    dta $3F
    dta $ff, $ff
    dta $c1, $7f
    dta $ff, $ff, $ff, $ff
LA897:
    sta zp4D         ; ERROR: EQUW $4D85
    STY zp4E          ; A899= 84 4E       .N
    JSR LA385        ; A89B= 20 85 A3     .#
    LDY #$00         ; A89E= A0 00        .
    LDA (zp4D),Y      ; A8A0= B1 4D       1M
    STA zp48          ; A8A2= 85 48       .H
    INC zp4D          ; A8A4= E6 4D       fM
    BNE LA8AA        ; A8A6= D0 02       P.
    INC zp4E          ; A8A8= E6 4E       fN
LA8AA:
    LDA zp4D          ; A8AA= A5 4D       %M
    STA zp4B          ; A8AC= 85 4B       .K
    LDA zp4E          ; A8AE= A5 4E       %N
    STA zp4C          ; A8B0= 85 4C       .L
    JSR LA3B5        ; A8B2= 20 B5 A3     5#
LA8B5:
    JSR LA7F5        ; A8B5= 20 F5 A7     u'
    JSR LA6AD        ; A8B8= 20 AD A6     -$
    CLC              ; A8BB= 18          .
    LDA zp4D          ; A8BC= A5 4D       %M
    ADC #$05         ; A8BE= 69 05       i.
    STA zp4D          ; A8C0= 85 4D       .M
    STA zp4B          ; A8C2= 85 4B       .K
    LDA zp4E          ; A8C4= A5 4E       %N
    ADC #$00         ; A8C6= 69 00       i.
    STA zp4E          ; A8C8= 85 4E       .N
    STA zp4C          ; A8CA= 85 4C       .L
    JSR LA500        ; A8CC= 20 00 A5     .%
    DEC zp48          ; A8CF= C6 48       FH
    BNE LA8B5        ; A8D1= D0 E2       Pb
    RTS              ; A8D3= 60          `

; =ACS numeric
; ============
LA8D4:
    JSR LA8DA        ; A8D4= 20 DA A8     Z(
    JMP LA927        ; A8D7= 4C 27 A9    L')

; =ASN numeric
; ============
LA8DA:
    JSR L92FA        ; A8DA= 20 FA 92     z.
    JSR LA1DA        ; A8DD= 20 DA A1     Z!
    BPL LA8EA        ; A8E0= 10 08       ..
    LSR zp2E          ; A8E2= 46 2E       F.
    JSR LA8EA        ; A8E4= 20 EA A8     j(
    JMP LA916        ; A8E7= 4C 16 A9    L.)

LA8EA:
    JSR LA381        ; A8EA= 20 81 A3     .#
    JSR LA9B1        ; A8ED= 20 B1 A9     1)
    JSR LA1DA        ; A8F0= 20 DA A1     Z!
    BEQ LA8FE        ; A8F3= F0 09       p.
    JSR LA7F1        ; A8F5= 20 F1 A7     q'
    JSR LA6AD        ; A8F8= 20 AD A6     -$
    JMP LA90A        ; A8FB= 4C 0A A9    L.)

LA8FE:
    JSR LAA55        ; A8FE= 20 55 AA     U*
    JSR LA3B5        ; A901= 20 B5 A3     5#
LA904:
    LDA #$FF         ; A904= A9 FF       ).
    RTS              ; A906= 60          `

; =ATN numeric
; ============
LA907:
    JSR L92FA        ; A907= 20 FA 92     z.
LA90A:
    JSR LA1DA        ; A90A= 20 DA A1     Z!
    BEQ LA904        ; A90D= F0 F5       pu
    BPL LA91B        ; A90F= 10 0A       ..
    LSR zp2E          ; A911= 46 2E       F.
    JSR LA91B        ; A913= 20 1B A9     .)
LA916:
    LDA #$80         ; A916= A9 80       ).
    STA zp2E          ; A918= 85 2E       ..
    RTS              ; A91A= 60          `

LA91B:
    LDA zp30          ; A91B= A5 30       %0
    CMP #$81         ; A91D= C9 81       I.
    BCC LA936        ; A91F= 90 15       ..
    JSR LA6A5        ; A921= 20 A5 A6     %$
    JSR LA936        ; A924= 20 36 A9     6)
LA927:
    JSR LAA48        ; A927= 20 48 AA     H*
    JSR LA500        ; A92A= 20 00 A5     .%
    JSR LAA4C        ; A92D= 20 4C AA     L*
    JSR LA500        ; A930= 20 00 A5     .%
    JMP LAD7E        ; A933= 4C 7E AD    L~-

LA936:
    LDA zp30          ; A936= A5 30       %0
    CMP #$73         ; A938= C9 73       Is
    BCC LA904        ; A93A= 90 C8       .H
    JSR LA381        ; A93C= 20 81 A3     .#
    JSR LA453        ; A93F= 20 53 A4     S$
    LDA #$80         ; A942= A9 80       ).
    STA zp3D          ; A944= 85 3D       .=
    STA zp3E          ; A946= 85 3E       .>
    STA zp3B          ; A948= 85 3B       .;
    JSR LA505        ; A94A= 20 05 A5     .%
    LDA #<LA95A
    LDY #>LA95A
    JSR LA897        ; A951= 20 97 A8     .(
    JSR LAAD1        ; A954= 20 D1 AA     Q*
    LDA #$FF         ; A957= A9 FF       ).
    RTS              ; A959= 60          `

LA95A:
    dta $09, $85
    dta $A3         ; A95C= A3          #
    dta $59, $e8, $67
    dta $80
    dta $1C; A960= 80 1C       ..

    dta $9d, $07, $36
    dta $80
    dta $57; A965= 80 57       .W

    dta $BB         ; A967= BB          ;
    dta $78
    dta $DF
    dta $80
    dta $CA
    dta $9a
    dta $0E
    dta $83, $84
    dta $8c, $bb, $ca
    dta $6E
    dta $81, $95
    dta $96, $06
    dta $de, $81, $0a
    dta $C7
    dta $6C; A97B= C7 6C       Gl
    dta $52
    dta $7F; A97D= 52 7F       R.
    dta $7D
    dta $ad, $90
    dta $a1, $82
    dta $FB         ; A984= FB          {
    dta $62         ; A985= 62          b
    dta $57         ; A986= 57          W
    dta $2F
    dta $80
    dta $6D
    dta $63         ; A98A= 63          c
    dta $38
    dta $2C         ; A98C= 2C 20 FA    , z

; =COS numeric
; ============
LA98D:
    JSR L92FA        ; Evaluate float
    JSR LA9D3
    INC zp4A
    JMP LA99E

; =SIN numeric
; ============
LA998:
    JSR L92FA        ; Evaluate float
    JSR LA9D3        ; A99B= 20 D3 A9     S)
LA99E:
    LDA zp4A          ; A99E= A5 4A       %J
    AND #$02         ; A9A0= 29 02       ).
    BEQ LA9AA        ; A9A2= F0 06       p.
    JSR LA9AA        ; A9A4= 20 AA A9     *)
    JMP LAD7E        ; A9A7= 4C 7E AD    L~-

LA9AA:
    LSR zp4A          ; A9AA= 46 4A       FJ
    BCC LA9C3        ; A9AC= 90 15       ..
    JSR LA9C3        ; A9AE= 20 C3 A9     C)
LA9B1:
    JSR LA385        ; A9B1= 20 85 A3     .#
    JSR LA656        ; A9B4= 20 56 A6     V$
    JSR LA38D        ; A9B7= 20 8D A3     .#
    JSR LA699        ; A9BA= 20 99 A6     .$
    JSR LA4D0        ; A9BD= 20 D0 A4     P$
    JMP LA7B7        ; A9C0= 4C B7 A7    L7'

LA9C3:
    JSR LA381        ; A9C3= 20 81 A3     .#
    JSR LA656        ; A9C6= 20 56 A6     V$
    .if version < 3
        LDA #<LAA72
        LDY #>LAA72
    .elseif version >= 3
        LDA #>XAA72
        LDY #<XAA72
    .endif
    JSR LA897        ; A9CD= 20 97 A8     .(
    JMP LAAD1        ; A9D0= 4C D1 AA    LQ*

LA9D3:
    LDA zp30          ; A9D3= A5 30       %0
    CMP #$98         ; A9D5= C9 98       I.
    BCS LAA38        ; A9D7= B0 5F       0_
    JSR LA385        ; A9D9= 20 85 A3     .#
    JSR LAA55        ; A9DC= 20 55 AA     U*
    JSR LA34E        ; A9DF= 20 4E A3     N#
    LDA zp2E          ; A9E2= A5 2E       %.
    STA zp3B          ; A9E4= 85 3B       .;
    DEC zp3D          ; A9E6= C6 3D       F=
    JSR LA505        ; A9E8= 20 05 A5     .%
    JSR LA6E7        ; A9EB= 20 E7 A6     g$
    JSR LA3FE        ; A9EE= 20 FE A3     ~#
    LDA zp34          ; A9F1= A5 34       %4
    STA zp4A          ; A9F3= 85 4A       .J
    ORA zp33          ; A9F5= 05 33       .3
    ORA zp32          ; A9F7= 05 32       .2
    ORA zp31          ; A9F9= 05 31       .1
    BEQ LAA35        ; A9FB= F0 38       p8
    LDA #$A0         ; A9FD= A9 A0       )
    STA zp30          ; A9FF= 85 30       .0
    LDY #$00         ; AA01= A0 00        .
    STY zp35          ; AA03= 84 35       .5
    LDA zp31          ; AA05= A5 31       %1
    STA zp2E          ; AA07= 85 2E       ..
    BPL LAA0E        ; AA09= 10 03       ..
    JSR LA46C        ; AA0B= 20 6C A4     l$
LAA0E:
    JSR LA303        ; AA0E= 20 03 A3     .#
    JSR LA37D        ; AA11= 20 7D A3     }#
    JSR LAA48        ; AA14= 20 48 AA     H*
    JSR LA656        ; AA17= 20 56 A6     V$
    JSR LA7F5        ; AA1A= 20 F5 A7     u'
    JSR LA500        ; AA1D= 20 00 A5     .%
    JSR LA38D        ; AA20= 20 8D A3     .#
    JSR LA7ED        ; AA23= 20 ED A7     m'
    JSR LA3B5        ; AA26= 20 B5 A3     5#
    JSR LAA4C        ; AA29= 20 4C AA     L*
    JSR LA656        ; AA2C= 20 56 A6     V$
    JSR LA7F5        ; AA2F= 20 F5 A7     u'
    JMP LA500        ; AA32= 4C 00 A5    L.%

LAA35:
    JMP LA3B2        ; AA35= 4C B2 A3    L2#

LAA38:
    BRK
    dta $17
    FNfold 'Accuracy lost'
    BRK
LAA48:
    LDA #<LAA59
    .if (LAA59 & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    BNE LAA4E                  ; AA4A= D0 02       P.
LAA4C:
    LDA #<LAA5E
LAA4E:
    STA zp4B                    ; AA4E= 85 4B       .K
    LDA #>LAA59
    STA zp4C          ; AA52= 85 4C       .L
    RTS              ; AA54= 60          `

LAA55:
    LDA #<LAA63
    .if (LAA63 & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    BNE LAA4E          ; AA57= D0 F5       Pu

LAA59:
    dta $81, $c9
    dta $10, $00
LAA5D:
    BRK              ; AA5D= 00          .
LAA5E:
    dta $6F
    dta $15
    dta $77
    dta $7A         ; AA61= 7A          z
    dta $61         ; AA62= 61 81       a.
LAA63:
    dta $81
    dta $49, $0f
    dta $DA         ; AA66= DA          Z
    dta $A2
LAA68:
    dta $7B
    dta $0e, $fa, $35
    dta $12
LAA6D:
    dta $86
    dta $65, $2e, $e0, $d3
LAA72:
    .if version >= 3
        dta $7F
        dta $5E
        dta $5B
        dta $D8
        dta $AA
    .endif
XAA72:
    dta $05, $84, $8a, $ea
    dta $0C
    dta $1B
    dta $84; AA76= 0C 1B 84    ...
    dta $1A         ; AA79= 1A          .
    dta $be, $bb, $2b, $84, $37, $45, $55
    dta $AB         ; AA81= AB          +
    dta $82         ; AA82= 82          .
    dta $d5, $55
    dta $57         ; AA85= 57          W
    dta $7C
    dta $83
    dta $C0; AA86= 7C 83 C0    |.@

    BRK              ; AA89= 00          .
    BRK              ; AA8A= 00          .
    dta $05, $81
    BRK              ; AA8D= 00          .
    BRK              ; AA8E= 00          .
    BRK              ; AA8F= 00          .
    BRK              ; AA90= 00          .

; = EXP numeric
; =============
LAA91:
    JSR L92FA        ; AA91= 20 FA 92     z.
LAA94:
    LDA zp30          ; AA94= A5 30       %0
    CMP #$87         ; AA96= C9 87       I.
    BCC LAAB8        ; AA98= 90 1E       ..
    BNE LAAA2        ; AA9A= D0 06       P.
LAA9C:
    LDY zp31          ; AA9C= A4 31       $1
    CPY #$B3         ; AA9E= C0 B3       @3
    BCC LAAB8        ; AAA0= 90 16       ..
LAAA2:
    LDA zp2E          ; AAA2= A5 2E       %.
    BPL LAAAC        ; AAA4= 10 06       ..
    JSR LA686        ; AAA6= 20 86 A6     .$
    LDA #$FF         ; AAA9= A9 FF       ).
    RTS              ; AAAB= 60          `

LAAAC:
    .if foldup == 0
        BRK
        dta $18
        dta 'Exp range'
        BRK
    .elseif foldup != 0
        BRK
        dta $18
        dta tknEXP
        FNfold ' range'
        BRK
    .endif
LAAB8:
    JSR LA486        ; AAB8= 20 86 A4     .$
    JSR LAADA        ; AABB= 20 DA AA     Z*
    JSR LA381        ; AABE= 20 81 A3     .#
    LDA #<LAAE4
    STA zp4B          ; AAC3= 85 4B       .K
    LDA #>LAAE4
    STA zp4C          ; AAC7= 85 4C       .L
    JSR LA3B5        ; AAC9= 20 B5 A3     5#
    LDA zp4A          ; AACC= A5 4A       %J
    JSR LAB12        ; AACE= 20 12 AB     .+
LAAD1:
    JSR LA7F1        ; AAD1= 20 F1 A7     q'
    JSR LA656        ; AAD4= 20 56 A6     V$
    LDA #$FF         ; AAD7= A9 FF       ).
    RTS              ; AAD9= 60          `

LAADA:
    LDA #<LAAE9
    LDY #>LAAE9
    JSR LA897        ; AADE= 20 97 A8     .(
    LDA #$FF         ; AAE1= A9 FF       ).
    RTS              ; AAE3= 60          `

LAAE4:
    dta $82         ; AAE4= 82          .
    dta $2d, $f8, $54
    dta $58
LAAE9:
    dta $07
    dta $83; AAE9= 07 83       ..
    dta $e0, $20
    dta $86, $5b
    dta $82         ; AAEF= 82          .
    dta $80
    dta $53; AAF0= 80 53       .S

    dta $93         ; AAF2= 93          .
    dta $b8
    dta $83         ; AAF4= 83          .
    JSR ws+$0600        ; AAF5= 20 00 06     ..
    dta $a1, $82
    BRK              ; AAFA= 00          .
    BRK              ; AAFB= 00          .
    dta $21, $63
    dta $82         ; AAFE= 82          .
    dta $c0, $00
    BRK              ; AB01= 00          .
    dta $02         ; AB02= 02          .
    dta $82         ; AB03= 82          .
    dta $80
    dta $00; AB04= 80 00       ..

LAB06:
    BRK              ; AB06= 00          .
    dta $0C
    dta $81
    dta $00; AB07= 0C 81 00    ...
    BRK              ; AB0A= 00          .
    BRK              ; AB0B= 00          .
    BRK              ; AB0C= 00          .
    dta $81, $00
    BRK              ; AB0F= 00          .
    BRK              ; AB10= 00          .
    BRK              ; AB11= 00          .
LAB12:
    TAX              ; AB12= AA          *
    BPL LAB1E        ; AB13= 10 09       ..
    DEX              ; AB15= CA          J
    TXA              ; AB16= 8A          .
    EOR #$FF         ; AB17= 49 FF       I.
    PHA              ; AB19= 48          H
    JSR LA6A5        ; AB1A= 20 A5 A6     %$
    PLA              ; AB1D= 68          h
LAB1E:
    PHA              ; AB1E= 48          H
    JSR LA385        ; AB1F= 20 85 A3     .#
    JSR LA699        ; AB22= 20 99 A6     .$
LAB25:
    PLA              ; AB25= 68          h
    BEQ LAB32        ; AB26= F0 0A       p.
    SEC              ; AB28= 38          8
    SBC #$01         ; AB29= E9 01       i.
    PHA              ; AB2B= 48          H
    JSR LA656        ; AB2C= 20 56 A6     V$
    JMP LAB25        ; AB2F= 4C 25 AB    L%+

LAB32:
    RTS              ; AB32= 60          `

; =ADVAL numeric - Call OSBYTE to read buffer/device
; ==================================================
LAB33:
    JSR L92E3        ; Evaluate integer
    LDX zp2A
    LDA #$80 ; X=low byte, A=$80 for ADVAL
; 
; WRONG in original disassembly
;    .if 0            ; FALSE
;        JSR OSBYTE
;    .endif
;    .ifdef MOS_BBC
;        JSR LAFB2
;    .endif
    .ifdef MOS_BBC
        jsr OSBYTE
    .endif
    TXA
    .if version < 3
        JMP LAEEA
    .elseif version >= 3
        JMP XAED5
    .endif

    .if version < 3
        ; =POINT(numeric, numeric)
        ; ========================
LAB41:
        JSR L92DD
        JSR LBD94
        JSR L8AAE
        JSR LAE56
        JSR L92F0
        LDA zp2A
        PHA
        LDA zp2B
        PHA
        JSR LBDEA
        PLA
        STA zp2D
        PLA
        STA zp2C
        LDX #$2A
        LDA #$09
        JSR OSWORD
        LDA zp2E
        BMI LAB9D
        JMP LAED8
    .elseif version >= 3
        ; =NOT
        ; ====
XAB5B:
        JSR L92E3
XAB5E:
        LDX #$03
XAB60:
        LDA zp2A,X
        EOR #$FF
        STA zp2A,X
        DEX
        BPL XAB60
        LDA #$40
        RTS
    .endif

; =POS
; ====
LAB6D:
    .if version < 3
        LDA #$86
        JSR OSBYTE
        TXA
        JMP LAED8
    .elseif version >= 3
        JSR LAB76
        STX zp2A
        RTS
    .endif

; =VPOS
; =====
LAB76:
    LDA #$86
    JSR OSBYTE
    TYA
    .if version < 3
        JMP LAED8
    .elseif version >= 3
        JMP XAED3
    .endif

    .if version < 3
LAB7F:
        JSR LA1DA
        BEQ LABA2
        BPL LABA0
        BMI LAB9D
     
        ; =SGN numeric
        ; ============
LAB88:
        JSR LADEC
        BEQ LABE6
        BMI LAB7F
        LDA zp2D
        ORA zp2C
        ORA zp2B
        ORA zp2A
        BEQ LABA5
        LDA zp2D
        BPL LABA0
LAB9D:
        JMP LACC4

LABA0:
        LDA #$01
LABA2:
        JMP LAED8

LABA5:
        LDA #$40
        RTS
    .endif

; =LOG numeric
; ============
LABA8:
    JSR LA7FE
    .if version < 3
        LDY #<LA869
        LDA #>LA869
    .elseif version >= 3
        LDY #<LAA72
    .endif
    BNE LABB8

; =RAD numeric
; ============
LABB1:
    JSR L92FA
    LDY #<LAA68
    .if version < 3
        LDA #>LAA68
    .endif
LABB8:
    .if version >= 3
        LDA #>LAA68         ; identical to version , 3
    .endif
    STY zp4B
    STA zp4C
    JSR LA656
    LDA #$FF
    RTS

; =DEG numeric
; ============
LABC2:
    JSR L92FA
    LDY #<LAA6D
    .if version < 3
        LDA #>LAA6D
    .endif
    BNE LABB8

; =PI
; ===
LABCB:
    JSR LA8FE
    INC zp30
    TAY
    RTS

; =USR numeric
; ============
LABD2:
    JSR L92E3       ; Evaluate integer
    JSR L8F1E       ; Set up registers and call code at IntA
    STA zp2A
    STX zp2B ; Store returned A,X in IntA
    STY zp2C         ; Store returned Y
    PHP
    PLA
    STA zp2D ; Store returned flags in IntA
    CLD             ; Ensure in binary mode on return
    LDA #$40
    RTS    ; Return INTEGER

    .if version < 3
LABE6:
        JMP L8C0E
    .endif

; =EVAL string$ - Tokenise and evaluate expression
; ================================================
LABE9:
    JSR LADEC            ; Evaluate value
    .if version < 3
        BNE LABE6
    .elseif version >= 3
        BNE LAC2C
    .endif
    INC zp36
    LDY zp36      ; Increment string length to add a <cr>
    LDA #$0D
    STA ws+$05FF,Y ; Put in terminating <cr>
    JSR LBDB2            ; Stack the string
                         ; String has to be stacked as otherwise would
                         ;  be overwritten by any string operations
                         ;  called by Evaluator
    LDA zp19
    PHA          ; Save PTRB
    LDA zp1A
    PHA
    LDA zp1B
    PHA
    LDY zp04
    LDX zp05      ; YX=>stackbottom (wrong way around)
    INY                  ; Step over length byte
    STY zp19              ; PTRB=>stacked string
    STY zp37              ; GPTR=>stacked string
    BNE LAC0F
    INX        ; Inc high byte if next page
LAC0F:
    STX zp1A
    STX zp38      ; PTRB and GPTR high bytes
    LDY #$FF
    STY zp3B
    INY
    STY zp1B          ; Point PTRB offset back to start
    JSR L8955            ; Tokenise string on stack at GPTR
    JSR L9B29            ; Call expression evaluator
    JSR LBDDC            ; Drop string from stack
LAC23:
    PLA
    STA zp1B          ; Restore PTRB
    PLA
    STA zp1A
    PLA
    STA zp19
    LDA zp27              ; Get expression return value
    RTS                  ; And return

    .if version >= 3
LAC2C:
        JMP L8C0E
    .endif

; =VAL numeric
; ============
LAC2F:
    JSR LADEC
    .if version < 3
        BNE LAC9B
    .elseif version >= 3
        BNE LAC2C
    .endif
LAC34:
    LDY zp36
    LDA #$00
    STA ws+$0600,Y
    LDA zp19
    PHA
    LDA zp1A
    PHA
    LDA zp1B
    PHA
    LDA #$00
    STA zp1B
    .if version < 3
        LDA #$00
    .endif
    STA zp19
    LDA #$06+(ws/256)
    STA zp1A
    JSR L8A8C
    CMP #$2D
    BEQ LAC66
    CMP #$2B
    BNE LAC5E
    JSR L8A8C
LAC5E:
    DEC zp1B
    JSR LA07B
    JMP LAC73

LAC66:
    JSR L8A8C
    DEC zp1B
    JSR LA07B
    BCC LAC73
    JSR LAD8F
LAC73:
    STA zp27
    JMP LAC23

; =INT numeric
; ============
LAC78:
    JSR LADEC
    .if version < 3
        BEQ LAC9B
    .elseif version >= 3
        BEQ XAC81
    .endif
    BPL LAC9A
    LDA zp2E
    PHP
    JSR LA3FE
    PLP
    BPL LAC95
    LDA zp3E
    ORA zp3F
    ORA zp40
    ORA zp41
    BEQ LAC95
    JSR LA4C7
LAC95:
    JSR LA3E7
    LDA #$40
LAC9A:
    RTS

    .if version < 3
LAC9B:
        JMP L8C0E
    .endif

; =ASC string$
; ============
LAC9E:
    JSR LADEC
    .if version < 3
        BNE LAC9B
    .elseif version >= 3
        BNE XAC81
    .endif
    LDA zp36
    BEQ LACC4
    LDA ws+$0600
LACAA:
    .if version < 3
        JMP LAED8
    .elseif version >= 3
        JMP XAED3
    .endif

; =INKEY numeric
; ==============
LACAD:
    JSR LAFAD
    .if version < 3
        CPY #$00
    .elseif version >= 3
        TYA     
    .endif
    BNE LACC4
    TXA
    .if version < 3
        JMP LAEEA
    .elseif version >= 3
        JMP XAED5
    .endif

    .if version >= 3
XAC81:
        JMP L8C0E
    .endif

; =EOF#numeric
; ============
LACB8:
    JSR LBFB5
    TAX
    LDA #$7F
    .ifdef MOS_BBC
        JSR OSBYTE
    .endif
    TXA
    .if version < 3
        BEQ LACAA
    .elseif version >= 3
        BEQ LACC6
    .endif

; =TRUE
; =====
LACC4:
    .if version < 3
        LDA #$FF
    .elseif version >= 3
        LDX #$FF
    .endif
LACC6:
    .if version < 3
        STA zp2A
        STA zp2B
        STA zp2C
        STA zp2D
    .elseif version >= 3
        STX zp2A
        STX zp2B
        STX zp2C
        STX zp2D
    .endif
LACC8:
    LDA #$40
    RTS

    .if version >= 3
    ; =FALSE
    ; ======
LACCD:
        LDX #$00
        BEQ LACC6

XACA1:
        JSR LA1DA
        BEQ LACCD
        BPL XACBF
        BMI LACC4
     
    ; =SGN numeric
    ; ============
XACAA:
        JSR LADEC
        BEQ XAC81
        BMI XACA1
        LDA zp2D
        ORA zp2C
        ORA zp2B
        ORA zp2A
        BEQ LACC8
        LDA zp2D
        BMI LACC4
XACBF:
        LDA #$01
XACC1:
        JMP XAED3

    ; =POINT(numeric, numeric)
    ; ========================
XAB41:
        JSR L92DD
        JSR LBD94
        JSR L8AAE
        JSR LAE56
        JSR L92F0
        LDA zp2A
        PHA
        LDX zp2B
        JSR LBDEA
        STX zp2D
        PLA
        STA zp2C
        LDX #$2A
        LDA #$09
        JSR OSWORD
        LDA zp2E
        BMI LACC4
        BPL XACC1
    .endif

    .if version < 3
    ; =NOT numeric
    ; ============
LACD1:
        JSR L92E3
        LDX #$03
LACD6:
        LDA zp2A,X
        EOR #$FF
        STA zp2A,X
        DEX
        BPL LACD6
        LDA #$40
        RTS
    .endif

; =INSTR(string$, string$ [, numeric])
; ====================================
LACE2:
    JSR L9B29
    .if version < 3
        BNE LACE2-$47           ; dest=LAC9B
    .elseif version >= 3
        BNE XAC81
    .endif
    CPX #$2C
    BNE LAD03
    INC zp1B
    JSR LBDB2
    JSR L9B29
    .if version < 3
        BNE LACE2-$47           ; dest=LAC9B
    .elseif version >= 3
        BNE XAC81
    .endif
    LDA #$01
    STA zp2A
    INC zp1B
    CPX #')'
    BEQ LAD12
    CPX #$2C
    BEQ LAD06
LAD03:
    .if version < 3
        JMP L8AA2
    .elseif version >= 3
        JMP X8AC8
    .endif

LAD06:
    JSR LBDB2
    JSR LAE56
    JSR L92F0
    JSR LBDCB
LAD12:
    LDY #$00
    LDX zp2A
    BNE LAD1A
    LDX #$01
LAD1A:
    STX zp2A
    TXA
    DEX
    STX zp2D
    CLC
    ADC zp04
    STA zp37
    TYA
    ADC zp05
    STA zp38
    LDA (zp04),Y
    SEC
    SBC zp2D
    BCC LAD52
    SBC zp36
    BCC LAD52
    ADC #$00
    STA zp2B
    JSR LBDDC
LAD3C:
    LDY #$00
    LDX zp36
    BEQ LAD4D
LAD42:
    LDA (zp37),Y
    CMP ws+$0600,Y
    BNE LAD59
    INY
    DEX
    BNE LAD42
LAD4D:
    LDA zp2A
LAD4F:
    .if version < 3
        JMP LAED8
    .elseif version >= 3
        JMP XAED3
    .endif

LAD52:
    JSR LBDDC
LAD55:
    LDA #$00
    BEQ LAD4F

LAD59:
    INC zp2A
    DEC zp2B
    BEQ LAD55
    INC zp37
    BNE LAD3C
    INC zp38
    BNE LAD3C
LAD67:
    JMP L8C0E

; =ABS numeric
; ============
LAD6A:
    JSR LADEC
    BEQ LAD67
    BMI LAD77
LAD71:
    BIT zp2D
    BMI LAD93
    BPL LADAA
LAD77:
    JSR LA1DA
    BPL LAD89
    BMI LAD83
LAD7E:
    JSR LA1DA
    BEQ LAD89
LAD83:
    LDA zp2E
    EOR #$80
    STA zp2E
LAD89:
    LDA #$FF
    RTS

LAD8C:
    JSR LAE02
LAD8F:
    BEQ LAD67
    BMI LAD7E
LAD93:
    SEC
    LDA #$00
    TAY
    SBC zp2A
    STA zp2A
    TYA
    SBC zp2B
    STA zp2B
    TYA
    SBC zp2C
    STA zp2C
    TYA
    SBC zp2D
    STA zp2D
LADAA:
    LDA #$40
    RTS

LADAD:
    JSR L8A8C
    CMP #$22
    BEQ LADC9
    LDX #$00
LADB6:
    LDA (zp19),Y
    STA ws+$0600,X
    INY
    INX
    CMP #$0D
    BEQ LADC5
    CMP #$2C
    BNE LADB6
LADC5:
    DEY
    .if version < 3
        JMP LADE1
    .elseif version >= 3
LADC8:
        DEX
        STX zp36
        STY zp1B
        LDA #$00
        RTS
    .endif

LADC9:
    LDX #$00
LADCB:
    INY
LADCC:
    LDA (zp19),Y
    CMP #$0D
    BEQ LADE9
    .if version < 3
        INY
        STA ws+$0600,X
    .elseif version >= 3
        STA ws+$0600,X
        INY
    .endif
    INX
    CMP #$22
    BNE LADCC
    LDA (zp19),Y
    CMP #$22
    BEQ LADCB
    .if version < 3
LADE1:
        DEX
        STX zp36
        STY zp1B
        LDA #$00
        RTS
    .elseif version >= 3
        BNE LADC8
    .endif

LADE9:
    JMP L8E98

; Evaluator Level 1, - + NOT function ( ) ? ! $ | "
; -------------------------------------------------
LADEC:
    LDY zp1B
    INC zp1B
    LDA (zp19),Y    ; Get next character
    CMP #$20
    BEQ LADEC             ; Loop to skip spaces
    CMP #'-'
    BEQ LAD8C          ; Jump with unary minus
    CMP #'"'
    BEQ LADC9         ; Jump with string
    CMP #'+'
    BNE LAE05          ; Jump with unary plus
LAE02:
    JSR L8A8C                      ; Get current character
LAE05:
    CMP #$8E
    BCC LAE10             ; Lowest function token, test for indirections
    CMP #$C6
    BCS LAE43             ; Highest function token, jump to error
    JMP L8BB1                      ; Jump via function dispatch table

; Indirection, hex, brackets
; --------------------------
LAE10:
    CMP #'?'
    BCS LAE20 ; Jump with ?numeric or higher
    CMP #'.'
    BCS LAE2A ; Jump with .numeric or higher
    CMP #'&'
    BEQ LAE6D ; Jump with hex number
    CMP #'('
    BEQ LAE56 ; Jump with brackets
LAE20:
    DEC zp1B
    JSR L95DD
    BEQ LAE30       ; Jump with undefined variable or bad name
    JMP LB32C

LAE2A:
    JSR LA07B
    BCC LAE43
    RTS

LAE30:
    LDA zp28         ; Check assembler option
    AND #$02        ; Is 'ignore undefiened variables' set?
    BNE LAE43       ; b1=1, jump to give No such variable
    BCS LAE43       ; Jump with bad variable name
    STX zp1B
LAE3A:
    LDA ws+$0440    ; Use P% for undefined variable
    LDY ws+$0441
    .if version < 3
        JMP LAEEA   ; Jump to return 16-bit integer
    .elseif version >= 3
        JMP XAED5   ; Jump to return 16-bit integer
    .endif

LAE43:
    BRK
    dta $1A
    FNfold 'No such variable'
LAE54:
    BRK
    .if version >= 3
        dta $1B
        FNfold 'Missing )'
LAE55:
        BRK
        dta $1C
        FNfold 'Bad HEX'
        BRK
    .endif

LAE56:
    JSR L9B29
    INC zp1B
    CPX #')'
    .if version < 3
        BNE LAE61
    .elseif version >= 3
        BNE LAE54
    .endif
    TAY
    RTS

    .if version < 3
LAE61:
        BRK
        dta $1B
        FNfold 'Missing )'
        BRK
    .endif
LAE6D:
    .if version < 3
        LDX #$00
        STX zp2A
        STX zp2B
        STX zp2C
        STX zp2D
        LDY zp1B
    .elseif version >= 3
        JSR LACCD
        INY
    .endif
LAE79:
    LDA (zp19),Y
    CMP #$30
    BCC LAEA2
    CMP #$3A
    BCC LAE8D
    SBC #$37
    CMP #$0A
    BCC LAEA2
    CMP #$10
    BCS LAEA2
LAE8D:
    ASL
    ASL
    ASL
    ASL
    LDX #$03
LAE93:
    ASL
    ROL zp2A
    ROL zp2B
    ROL zp2C
    ROL zp2D
    DEX
    BPL LAE93
    INY
    BNE LAE79
LAEA2:
    TXA
    .if version < 3
        BPL LAEAA
    .elseif version >= 3
        BPL LAE55
    .endif
    STY zp1B
    LDA #$40
    RTS

    .if version >= 3
    ; =TOP - Return top of program
    ; ============================
XAEA6:
        INY
        LDA (zp19),Y
        CMP #'P'
        BNE LAE43
        INC zp1B
        LDA zp12
        LDY zp13
        BCS XAED5

    ; =PAGE - Read PAGE
    ; =================
XAEA7:
        LDY zp18
        LDA #$00
        BEQ XAED5

XAEC9:
        JMP L8C0E

    ; =LEN string$
    ; ============
XAECC:
        JSR LADEC
        BNE XAEC9
        LDA zp36
    
    ; Return 8-bit integer
    ; --------------------
XAED3:
        LDY #$00                  ; Clear b8-b15, jump to return 16-bit int

    ; Return 16-bit integer in AY
    ; ---------------------------
XAED5:
        STA zp2A
        STY zp2B           ; Store AY in integer accumulator
        LDA #$00
        STA zp2C
        STA zp2D  ; Set b16-b31 to 0
        LDA #$40
        RTS              ; Return 'integer'

    ; =COUNT - Return COUNT
    ; =====================
XAEF7:
        LDA zp1E
        BCC XAED3         ; Get COUNT, jump to return 8-bit integer
     
    ; =LOMEM - Start of BASIC heap
    ; ============================
XAEFC:
        LDA ZP00
        LDY ZP01
        BCC XAED5 ; Get LOMEM to AY, jump to return as integer
     
    ; =HIMEM - Top of BASIC memory
    ; ============================
XAF03:
        LDA zp06
        LDY zp07
        BCC XAED5  ; Get HIMEM to AY, jump to return as integer

    ; =ERL - Return error line number
    ; ===============================
XAF9F:
        LDY zp09
        LDA zp08
        BCC XAED5  ; Get ERL to AY, jump to return 16-bit integer

    ; =ERR - Return current error number
    ; ==================================
XAFA6:
        LDY #$00
        LDA (FAULT),Y
        BCC XAED5  ; Get error number, jump to return 16-bit integer
    .endif

    .if version < 3
LAEAA:
        BRK
        dta $1C
        FNfold 'Bad HEX'
        BRK
    .endif

; =TIME - Read system TIME
; ========================
LAEB4:
    LDX #$2A
    LDY #$00         ; Point to integer accumulator
    LDA #$01                  ; Read TIME to IntA via OSWORD $01
    .ifdef MOS_BBC
        JSR OSWORD
    .endif
    LDA #$40
    RTS              ; Return 'integer'

    .if version < 3
    ; =PAGE - Read PAGE
    ; =================
LAEC0:
        LDA #$00
        LDY zp18
        JMP LAEEA
     
LAEC7:
        JMP LAE43
     
    ; =FALSE
    ; ======
LAECA:
        LDA #$00
        BEQ LAED8        ; Jump to return $00 as 16-bit integer

LAECE:
        JMP L8C0E
     
    ; =LEN string$
    ; ============
LAED1:
        JSR LADEC
        BNE LAECE
        LDA zp36

    ; Return 8-bit integer
    ; --------------------
LAED8:
        LDY #$00
        BEQ LAEEA        ; Clear b8-b15, jump to return 16-bit int

    ; =TOP - Return top of program
    ; ============================
LAEDC:
        LDY zp1B
        LDA (zp19),Y
        CMP #$50
        BNE LAEC7
        INC zp1B
        LDA zp12
        LDY zp13

    ; Return 16-bit integer in AY
    ; ---------------------------
LAEEA:
        STA zp2A
        STY zp2B           ; Store AY in integer accumulator
        LDA #$00
        STA zp2C
        STA zp2D  ; Set b16-b31 to 0
        LDA #$40
        RTS              ; Return 'integer'

    ; =COUNT - Return COUNT
    ; =====================
LAEF7:
        LDA zp1E
        JMP LAED8         ; Get COUNT, jump to return 8-bit integer
     
    ; =LOMEM - Start of BASIC heap
    ; ============================
LAEFC:
        LDA ZP00
        LDY ZP01
        JMP LAEEA ; Get LOMEM to AY, jump to return as integer
     
    ; =HIMEM - Top of BASIC memory
    ; ============================
LAF03:
        LDA zp06
        LDY zp07
    JMP LAEEA ; Get HIMEM to AY, jump to return as integer
    .endif

; =RND(numeric)
; -------------
LAF0A:
    INC zp1B
    JSR LAE56
    JSR L92F0
    LDA zp2D
    BMI LAF3F
    ORA zp2C
    ORA zp2B
    BNE LAF24
    LDA zp2A
    BEQ LAF6C
    CMP #$01
    BEQ LAF69
LAF24:
    JSR LA2BE
    JSR LBD51
    JSR LAF69
    JSR LBD7E
    JSR LA606
    JSR LA303
    JSR LA3E4
    JSR L9222
    LDA #$40
    RTS

LAF3F:
    LDX #$0D
    JSR LBE44
    LDA #$40
    STA zp11
    RTS

; RND [(numeric)]
; ===============
LAF49:
    LDY zp1B
    LDA (zp19),Y     ; Get current character
    CMP #'('
    BEQ LAF0A   ; Jump with RND(numeric)
    JSR LAF87               ; Get random number
    LDX #$0D
LAF56:
    LDA zp00,X
    STA zp2A       ; Copy random number to IntA
    LDA zp01,X
    STA zp2B
    LDA zp02,X
    STA zp2C
    LDA zp03,X
    STA zp2D
    LDA #$40
    RTS            ; Return Integer

LAF69:
    JSR LAF87
LAF6C:
    LDX #$00
    STX zp2E
    STX zp2F
    STX zp35
    LDA #$80
    STA zp30
LAF78:
    LDA zp0D,X
    STA zp31,X
    INX
    CPX #$04
    BNE LAF78
    JSR LA659
    LDA #$FF
    RTS

LAF87:
    .if version >= 3
        LDY #$04 ; Rotate through four bytes, faster but bigger
LAF89:
        ROR zp11
        LDA zp10
        PHA
        ROR
        STA zp11
        LDA zp0F
        TAX
        ASL
        ASL
        ASL
        ASL
        STA zp10
        LDA zp0E
        STA zp0F
        LSR
        LSR
        LSR
        LSR
        ORA zp10
        EOR zp11
        STX zp10
        LDX zp0D
        STX zp0E
        STA zp0D
        PLA
        STA zp11
LAFB1:
        DEY
        BNE LAF89
        RTS
    .elseif version < 3
        LDY #$20 ; Rotate through 32 bits, shorter but slower
LAF89:
        LDA zp0F
        LSR
        LSR
        LSR
        EOR zp11
        ROR
        ROL zp0D
        ROL zp0E
        ROL zp0F
        ROL zp10
        ROL zp11
        DEY
        BNE LAF89
        RTS
 
    ; =ERL - Return error line number
    ; ===============================
LAF9F:
        LDY zp09
        LDA zp08
        JMP LAEEA ; Get ERL to AY, jump to return 16-bit integer

    ; =ERR - Return current error number
    ; ==================================
LAFA6:
        LDY #$00
        LDA (FAULT),Y
        JMP LAEEA  ; Get error number, jump to return 16-bit integer
    .endif

; INKEY
; =====
LAFAD:
    JSR L92E3       ; Evaluate <numeric>

; Atom/System - Manually implement INKEY(num)
; -------------------------------------------
LCF8D:
    .ifdef TARGET_ATOM
        JSR $FE71
        BCC LCFAB         ; Key pressed
    .endif
    .ifdef TARGET_SYSTEM
        LDA $0E21
        BPL LCFAB         ; Key pressed
    .endif
    .ifdef TARGET_ATOM
        LDA zp2A
        ORA zp2D
        BEQ LCFB4   ; Timeout=0
        LDY #$08                    ; $0800 gives 1cs delay
LCF9A:
        DEX
        BNE LCF9A
        DEY
        BNE LCF9A ; Wait 1cs
        LDA zp2A
        BNE LCFA6
        DEC zp2D   ; Decrement timeout
LCFA6:
        DEC zp2A
        JMP LCF8D           ; Loop to keep waiting
LCFAB:
    .endif
    .ifdef TARGET_ATOM
        JSR LCFB7                   ; Convert keypress
    .endif
    .ifdef TARGET_SYSTEM
        LDY $0E21
        BPL LCFAB         ; Loop until key released
    .endif
    .ifdef MOS_ATOM
        LDY #$00
        TAX
        RTS            ; Y=0, X=key, return
LCFB4:
        LDY #$FF
        RTS                ; Y=$FF for no keypress
    .endif
    .ifdef TARGET_ATOM
LCFB7:
        PHP
        JMP $FEA4               ; Convert Atom keypress
    .endif

; BBC - Call MOS to wait for keypress
; -----------------------------------
    .ifdef MOS_BBC
        LDA #$81
LAFB2:
        LDX zp2A
        LDY zp2B
        JMP OSBYTE
    .endif

; =GET
; ====
LAFB9:
    JSR OSRDCH
    .if version < 3
        JMP LAED8
    .elseif version > 3
        JMP XAED3
    .endif


; =GET$
; =====
LAFBF:
    JSR OSRDCH
LAFC2:
    STA ws+$0600
    LDA #$01
    STA zp36
    LDA #$00
    RTS

; =LEFT$(string$, numeric)
; ========================
LAFCC:
    JSR L9B29
    BNE LB033
    CPX #$2C
    BNE LB036
    INC zp1B
    JSR LBDB2
    JSR LAE56
    JSR L92F0
    JSR LBDCB
    LDA zp2A
    CMP zp36
    BCS LAFEB
    STA zp36
LAFEB:
    LDA #$00
    RTS

; =RIGHT$(string$, numeric)
; =========================
LAFEE:
    JSR L9B29
    BNE LB033
    CPX #$2C
    BNE LB036
    INC zp1B
    JSR LBDB2
    JSR LAE56
    JSR L92F0
    JSR LBDCB
    LDA zp36
    SEC
    SBC zp2A
    BCC LB023
    BEQ LB025
    TAX
    LDA zp2A
    STA zp36
    BEQ LB025
    LDY #$00
LB017:
    LDA ws+$0600,X
    STA ws+$0600,Y
    INX
    INY
    DEC zp2A
    BNE LB017
LB023:
    LDA #$00
LB025:
    RTS

; =INKEY$ numeric
; ===============
LB026:
    JSR LAFAD
    TXA
    CPY #$00
    BEQ LAFC2
LB02E:
    LDA #$00
    STA zp36
    RTS

LB033:
    JMP L8C0E

LB036:
    .if version < 3
        JMP L8AA2
    .elseif version >= 3
        JMP X8AC8
    .endif


; =MID$(string$, numeric [, numeric] )
; ====================================
LB039:
    JSR L9B29
    BNE LB033
    CPX #$2C
    BNE LB036
    JSR LBDB2
    INC zp1B
    JSR L92DD
    LDA zp2A
    PHA
    LDA #$FF
    STA zp2A
    INC zp1B
    CPX #')'
    BEQ LB061
    CPX #$2C
    BNE LB036
    JSR LAE56
    JSR L92F0
LB061:
    JSR LBDCB
    PLA
    TAY
    CLC
    BEQ LB06F
    SBC zp36
    BCS LB02E
    DEY
    TYA
LB06F:
    STA zp2C
    TAX
    LDY #$00
    LDA zp36
    SEC
    SBC zp2C
    CMP zp2A
    BCS LB07F
    STA zp2A
LB07F:
    LDA zp2A
    BEQ LB02E
LB083:
    LDA ws+$0600,X
    STA ws+$0600,Y
    INY
    INX
    CPY zp2A
    BNE LB083
    STY zp36
    LDA #$00
    RTS

; =STR$ [~] numeric
; =================
LB094:
    JSR L8A8C                 ; Skip spaces
    LDY #$FF                  ; Y=$FF for decimal
    CMP #'~'
    BEQ LB0A1
    LDY #$00
    DEC zp1B          ; Y=$00 for hex, step past ~
LB0A1:
    TYA
    PHA                   ; Save format
    JSR LADEC
    BEQ LB0BF       ; Evaluate, error if not number
    TAY
    PLA
    STA zp15               ; Get format back
    LDA ws+$0403
    BNE LB0B9    ; Top byte of @%, STR$ uses @%
    STA zp37                   ; Store 'General format'
    JSR L9EF9                 ; Convert using general format
    LDA #$00
    RTS              ; Return string

LB0B9:
    JSR L9EDF                 ; Convert using @% format
    LDA #$00
    RTS              ; Return string

LB0BF:
    JMP L8C0E                 ; Jump to Type mismatch error

; =STRING$(numeric, string$)
; ==========================
LB0C2:
    JSR L92DD
    JSR LBD94
    JSR L8AAE
    JSR LAE56
    BNE LB0BF
    JSR LBDEA
    LDY zp36
    BEQ LB0F5
    LDA zp2A
    BEQ LB0F8
    DEC zp2A
    BEQ LB0F5
LB0DF:
    LDX #$00
LB0E1:
    LDA ws+$0600,X
    STA ws+$0600,Y
    INX
    INY
    BEQ LB0FB
    CPX zp36
    BCC LB0E1
    DEC zp2A
    BNE LB0DF
    STY zp36
LB0F5:
    LDA #$00
    RTS

LB0F8:
    STA zp36
    RTS

LB0FB:
    JMP L9C03

LB0FE:
    PLA
    STA zp0C
    PLA
    STA zp0B
    BRK
    dta $1D
    FNfold 'No such '
    dta tknFN, '/', tknPROC
    BRK

; Look through program for FN/PROC
; --------------------------------
LB112:
    LDA zp18
    STA zp0C         ; Start at PAGE
    LDA #$00
    STA zp0B
LB11A:
    LDY #$01
    LDA (zp0B),Y    ; Get line number high byte
    BMI LB0FE               ; End of program, jump to 'No such FN/PROC' error
    LDY #$03
LB122:
    INY
    LDA (zp0B),Y
    CMP #$20
    BEQ LB122      ; Skip past spaces
    CMP #tknDEF
    BEQ LB13C   ; Found DEF at start of line
LB12D:
    LDY #$03
    LDA (zp0B),Y    ; Get line length
    CLC
    ADC zp0B
    STA zp0B     ; Point to next line
    BCC LB11A
    INC zp0C
    BCS LB11A               ; Loop back to check next line

LB13C:
    INY
    STY zp0A
    JSR L8A97
    TYA
    TAX
    CLC
    ADC zp0B
    LDY zp0C
    BCC LB14D
    INY
    CLC
LB14D:
    SBC #$00
    STA zp3C
    TYA
    SBC #$00
    STA zp3D
    LDY #$00
LB158:
    INY
    INX
    LDA (zp3C),Y
    CMP (zp37),Y
    BNE LB12D
    CPY zp39
    BNE LB158
    INY
    LDA (zp3C),Y
    JSR L8926
    BCS LB12D
    TXA
    TAY
    JSR L986D
    JSR L94ED
    LDX #$01
    JSR L9531
    LDY #$00
    LDA zp0B
    STA (zp02),Y
    INY
    LDA zp0C
    STA (zp02),Y
    JSR L9539
    JMP LB1F4

LB18A:
    BRK
    dta $1E
    FNfold 'Bad call'
    BRK

; =FNname [parameters]
; ====================
LB195:
    LDA #$A4                 ; 'FN' token

; Call subroutine
; ---------------
; A=FN or PROC
; PtrA=>start of FN/PROC name
;
LB197:
    STA zp27                  ; Save PROC/FN token
    TSX
    TXA
    CLC
    ADC zp04      ; Drop BASIC stack by size of 6502 stack
    JSR LBE2E                ; Store new BASIC stack pointer, check for No Room
    LDY #$00
    TXA
    STA (zp04),Y ; Store 6502 Stack Pointer on BASIC stack
LB1A6:
    INX
    INY
    LDA $0100,X
    STA (zp04),Y  ; Copy 6502 stack onto BASIC stack
    CPX #$FF
    BNE LB1A6
    TXS                      ; Clear 6502 stack
    LDA zp27
    PHA              ; Push PROC/FN token
    LDA zp0A
    PHA
    LDA zp0B
    PHA  ; Push PtrA line pointer
    LDA zp0C
    PHA              ; Push PtrA line pointer offset
    LDA zp1B
    TAX
    CLC
    ADC zp19
    LDY zp1A
    BCC LB1CA
LB1C8:
    INY
    CLC
LB1CA:
    SBC #$01
    STA zp37
    TYA
    SBC #$00
    STA zp38     ; $37/8=>PROC token
    LDY #$02
    JSR L955B       ; Check name is valid
    CPY #$02
    BEQ LB18A       ; No valid characters, jump to 'Bad call' error
    STX zp1B                  ; Line pointer offset => after valid FN/PROC name
    DEY
    STY zp39
    JSR L945B
    BNE LB1E9      ; Look for FN/PROC name in heap, if found, jump to it
    JMP LB112                ; Not in heap, jump to look through program

; FN/PROC destination found
; -------------------------
LB1E9:
    LDY #$00
    LDA (zp2A),Y
    STA zp0B ; Set PtrA to address from FN/PROC infoblock
    INY
    LDA (zp2A),Y
    STA zp0C
LB1F4:
    LDA #$00
    PHA
    STA zp0A         ; Push 'no parameters' (?)
    JSR L8A97
    CMP #'('
    BEQ LB24D
    DEC zp0A
LB202:
    LDA zp1B
    PHA
    LDA zp19
    PHA
    LDA zp1A
    PHA
    JSR L8BA3
    PLA
    STA zp1A
    PLA
    STA zp19
    PLA
    STA zp1B
    PLA
    BEQ LB226
    STA zp3F
LB21C:
    JSR LBE0B
    JSR L8CC1
    DEC zp3F
    BNE LB21C
LB226:
    PLA
    STA zp0C
    PLA
    STA zp0B
    PLA
    STA zp0A
    PLA
    LDY #$00
    LDA (zp04),Y
    TAX
    TXS
LB236:
    INY
    INX
    LDA (zp04),Y
    STA $0100,X  ; Copy stacked 6502 stack back onto 6502 stack
    CPX #$FF
    BNE LB236
    TYA
    ADC zp04
    STA zp04      ; Adjust BASIC stack pointer
    BCC LB24A
    INC zp05
LB24A:
    LDA zp27
    RTS

LB24D:
    LDA zp1B
    PHA
    LDA zp19
    PHA
    LDA zp1A
    PHA
    JSR L9582
    BEQ LB2B5
    LDA zp1B
    STA zp0A
    PLA
    STA zp1A
    PLA
    STA zp19
    PLA
    STA zp1B
    PLA
    TAX
    LDA zp2C
    PHA
    LDA zp2B
    PHA
    LDA zp2A
    PHA
    INX
    TXA
    PHA
    JSR LB30D
    JSR L8A97
    CMP #','
    BEQ LB24D
    CMP #')'
    BNE LB2B5
    LDA #$00
    PHA
    JSR L8A8C
    CMP #'('
    BNE LB2B5
LB28E:
    JSR L9B29
    JSR LBD90
    LDA zp27
    STA zp2D
    JSR LBD94
    PLA
    TAX
    INX
    TXA
    PHA
    JSR L8A8C
    CMP #','
    BEQ LB28E
    CMP #')'
    BNE LB2B5
    PLA
    PLA
    STA zp4D
    STA zp4E
    CPX zp4D
    BEQ LB2CA
LB2B5:
    LDX #$FB
    TXS
    PLA
    STA zp0C
    PLA
    STA zp0B
    BRK
    dta $1F
    FNfold 'Arguments'
    BRK

LB2CA:
    JSR LBDEA
    PLA
    STA zp2A
    PLA
    STA zp2B
    PLA
    STA zp2C
    BMI LB2F9
    LDA zp2D
    BEQ LB2B5
    STA zp27
    LDX #$37
    JSR LBE44
    LDA zp27
    BPL LB2F0
    JSR LBD7E
    JSR LA3B5
    JMP LB2F3

LB2F0:
    JSR LBDEA
LB2F3:
    JSR LB4B7
    JMP LB303

LB2F9:
    LDA zp2D
    BNE LB2B5
    JSR LBDCB
    JSR L8C21
LB303:
    DEC zp4D
    BNE LB2CA
    LDA zp4E
    PHA
    JMP LB202

; Push a value onto the stack
; ---------------------------
LB30D:
    LDY zp2C
    .if version < 3
        CPY #$04
        BNE LB318
    .elseif version >= 3
        CPY #$05
        BCS LB318
    .endif
    LDX #$37
    JSR LBE44
LB318:
    JSR LB32C
    PHP
    JSR LBD90
    PLP
    BEQ LB329
    BMI LB329
    LDX #$37
    JSR LAF56
LB329:
    JMP LBD94

LB32C:
    LDY zp2C
    BMI LB384
    BEQ LB34F
    CPY #$05
    BEQ LB354
    LDY #$03
    LDA (zp2A),Y
    STA zp2D
    DEY
    LDA (zp2A),Y
    STA zp2C
    DEY
    LDA (zp2A),Y
    TAX
    DEY
    LDA (zp2A),Y
    STA zp2A
    STX zp2B
    LDA #$40
    RTS

LB34F:
    LDA (zp2A),Y
    .if version < 3
        JMP LAEEA
    .elseif version >= 3
        JMP XAED5
    .endif

LB354:
    DEY
    LDA (zp2A),Y
    STA zp34
    DEY
    LDA (zp2A),Y
    STA zp33
    DEY
    LDA (zp2A),Y
    STA zp32
    DEY
    LDA (zp2A),Y
    STA zp2E
    DEY
    LDA (zp2A),Y
    STA zp30
    STY zp35
    STY zp2F
    ORA zp2E
    ORA zp32
    ORA zp33
    ORA zp34
    BEQ LB37F
    LDA zp2E
    ORA #$80
LB37F:
    STA zp31
    LDA #$FF
    RTS

LB384:
    CPY #$80
    BEQ LB3A7
    LDY #$03
    LDA (zp2A),Y
    STA zp36
    BEQ LB3A6
    LDY #$01
    LDA (zp2A),Y
    STA zp38
    DEY
    LDA (zp2A),Y
    STA zp37
    LDY zp36
LB39D:
    DEY
    LDA (zp37),Y
    STA ws+$0600,Y
    TYA
    BNE LB39D
LB3A6:
    RTS

LB3A7:
    LDA zp2B
    BEQ LB3C0
LB3AB:
    LDY #$00
LB3AD:
    LDA (zp2A),Y
    STA ws+$0600,Y
    EOR #$0D
    BEQ LB3BA
    INY
    BNE LB3AD
    TYA
LB3BA:
    STY zp36
    RTS

; =CHR$ numeric
; =============
LB3BD:
    JSR L92E3
LB3C0:
    LDA zp2A
    JMP LAFC2

LB3C5:
    LDY #$00
    STY zp08
    STY zp09
    LDX zp18
    STX zp38
    STY zp37
    LDX zp0C
    CPX #$07+(ws/256)
    BEQ LB401
    LDX zp0B
LB3D9:
    JSR L8942
    CMP #$0D
    BNE LB3F9
    CPX zp37
    LDA zp0C
    SBC zp38
    BCC LB401
    JSR L8942
    ORA #$00
    BMI LB401
    STA zp09
    JSR L8942
    STA zp08
    JSR L8942
LB3F9:
    CPX zp37
    LDA zp0C
    SBC zp38
    BCS LB3D9
LB401:
    RTS


; ERROR HANDLER
; =============
LB402:


; Atom/System - Process raw BRK to get FAULT pointer
; --------------------------------------------------
    .ifdef MOS_ATOM
        PLA
        CLD
        CLI
        PLA           ; Drop flags, pop return low byte
        SEC
        SBC #$01
        STA FAULT+0  ; Point to error block
        PLA
        SBC #$00
        STA FAULT+1
        CMP #>L8000
        BCC LD428   ; If outside BASIC, not a full error block
        CMP #[>LC000]-1       ; syntax?
        BCS LD428 ; So generate default error
    .endif


; FAULT set up, now process BRK error
; -----------------------------------
    JSR LB3C5
    STY zp20
    LDA (FAULT),Y
    BNE LB413   ; If ERR<>0, skip past ON ERROR OFF
    LDA #<LB433
    STA zp16     ; ON ERROR OFF
    LDA #>LB433
    STA zp17
LB413:
    LDA zp16
    STA zp0B           ; Point program point to ERROR program
    LDA zp17
    STA zp0C
    JSR LBD3A                 ; Clear DATA and stack
    TAX
    STX zp0A
    .ifdef MOS_BBC
        LDA #$DA
        JSR OSBYTE      ; Clear VDU queue
        LDA #$7E
        JSR OSBYTE      ; Acknowlege any Escape state
    .endif
    LDX #$FF
    STX zp28
    TXS      ; Clear system stack
    JMP L8BA3                 ; Jump to execution loop

LD428:
    .ifdef MOS_ATOM
        .if foldup == 0
            BRK
            dta $FF
            dta 'External Error'
            BRK
        .endif
        .if foldup != 0
            BRK
            dta $FF
            dta tknEXT, 'ERNAL ', tknERROR
            BRK
        .endif
    .endif

; Default ERROR program
; ---------------------
; REPORT IF ERL PRINT " at line ";ERL END ELSE PRINT END
LB433:
    dta tknREPORT
    dta ':'
    dta tknIF
    dta tknERL
    dta tknPRINT
    dta '"', ' at line ', '"', ';'      ;; was FNfold
    dta tknERL
    dta ':'
    dta tknEND
    dta tknELSE
    dta tknPRINT
    dta ':'
    dta tknEND
    dta 13

; SOUND numeric, numeric, numeric, numeric
; ========================================
LB44C:
    JSR L8821                ; Evaluate integer
    LDX #$03                 ; Three more to evaluate
LB451:
    .ifdef MOS_BBC
        LDA zp2A
        PHA
        LDA zp2B
        PHA ; Stack current 16-bit integer
    .endif
    TXA
    PHA
    JSR L92DA        ; Step past comma, evaluate next integer
    PLA
    TAX
    DEX
    BNE LB451    ; Loop to stack this one
    .ifdef MOS_BBC
        JSR L9852               ; Check end of statement
        LDA zp2A
        STA zp3D         ; Copy current 16-bit integer to end of control block
        LDA zp2B
        STA zp3E
        LDY #$07
        LDX #$05       ; Prepare for OSWORD 7 and 6 more bytes
        BNE LB48F               ; Jump to pop to control block and call OSWORD
    .endif
    .ifdef MOS_ATOM
        BEQ LB484               ; Check end of statement and return
    .endif

; ENVELOPE a,b,c,d,e,f,g,h,i,j,k,l,m,n
; ====================================
LB472:
    JSR L8821                ; Evaluate integer
    LDX #$0D                 ; 13 more to evaluate
LB477:
    .ifdef MOS_BBC
        LDA zp2A
        PHA             ; Stack current 8-bit integer
    .endif
    TXA
    PHA
    JSR L92DA        ; Step past comma, evaluate next integer
    PLA
    TAX
    DEX
    BNE LB477    ; Loop to stack this one
LB484:
    JSR L9852                ; Check end of statement
    .ifdef MOS_BBC
        LDA zp2A
        STA zp44         ; Copy current 8-bit integer to end of control block
        LDX #$0C
        LDY #$08       ; Prepare for 12 more bytes and OSWORD 8
    .endif


LB48F:
    .ifdef MOS_BBC
        PLA
        STA zp37,X           ; Pop bytes into control block
        DEX
        BPL LB48F
        TYA                     ; Y=OSWORD number
        LDX #$37
        LDY #$00       ; XY=>control block
        JSR OSWORD
    .endif
    JMP L8B9B                ; Return to execution loop

; WIDTH numeric
; =============
LB4A0:
    JSR L8821
    JSR L9852
    LDY zp2A
    DEY
    STY zp23
    JMP L8B9B

LB4AE:
    JMP L8C0E

; Store byte or word integer
; ==========================
LB4B1:
    JSR L9B29               ; Evaluate expression
LB4B4:
    JSR LBE0B               ; Unstack integer (address of data)
LB4B7:
    LDA zp39
    CMP #$05
    BEQ LB4E0      ; Size=5, jump to store float
    LDA zp27
    BEQ LB4AE       ; Type<>num, jump to error
    BPL LB4C6               ; Type=int, jump to store it
    JSR LA3E4               ; Convert float to integer
LB4C6:
    LDY #$00
    LDA zp2A
    STA (zp37),Y     ; Store byte 1
    LDA zp39
    BEQ LB4DF       ; Exit if size=0, byte
    LDA zp2B
    INY
    STA (zp37),Y ; Store byte 2
    LDA zp2C
    INY
    STA (zp37),Y ; Store byte 3
    LDA zp2D
    INY
    STA (zp37),Y ; Store byte 4
LB4DF:
    RTS

; Store float
; ===========
LB4E0:
    LDA zp27
    BEQ LB4AE       ; Type<>num, jump to error
    BMI LB4E9               ; Type=float, jump to store it
    JSR LA2BE               ; Convert integer to float
LB4E9:
    LDY #$00                 ; Store 5-byte float
    LDA zp30
    STA (zp37),Y
    INY  ; exponent
    LDA zp2E
    AND #$80
    STA zp2E ; Unpack sign
    LDA zp31
    AND #$7F         ; Unpack mantissa 1
    ORA zp2E
    STA (zp37),Y      ; sign + mantissa 1
    INY
    LDA zp32
    STA (zp37),Y  ; mantissa 2
    INY
    LDA zp33
    STA (zp37),Y  ; mantissa 3
    INY
    LDA zp34
    STA (zp37),Y  ; mantissa 4
    RTS

LB500:
LB50E:
    STA zp37
    CMP #$80
    BCC LB558
    LDA #<L8071
    STA zp38 ; Point to token table
    LDA #>L8071
    STA zp39
    STY zp3A
LB51E:
    LDY #$00
LB520:
    INY
    LDA (zp38),Y
    BPL LB520
    CMP zp37
    BEQ LB536
    INY
    TYA
    SEC
    ADC zp38
    STA zp38
    BCC LB51E
    INC zp39
    BCS LB51E

LB536:
    LDY #$00
LB538:
    LDA (zp38),Y
    BMI LB542
    JSR LB558
    INY
    BNE LB538
LB542:
    LDY zp3A
    RTS

LB545:
    PHA
    LSR
    LSR
    LSR
    LSR
    JSR LB550
    PLA
    AND #$0F
LB550:
    CMP #$0A
    BCC LB556
    ADC #$06
LB556:
    ADC #$30
LB558:
    CMP #$0D
    BNE LB567
    JSR OSWRCH
    JMP LBC28 ; Set COUNT to zero

LB562:
    JSR LB545
LB565:
    LDA #$20
LB567:
    PHA
    LDA zp23
    CMP zp1E
    BCS LB571
    JSR LBC25
LB571:
    PLA
    INC zp1E
    .if WRCHV != 0
        JMP (WRCHV)
    .endif
    .if WRCHV == 0
        JMP OSWRCH 
    .endif

LB577:
    AND zp1F
    BEQ LB589
    TXA
    BEQ LB589
    BMI LB565
    .if version >= 3
        ASL
        TAX
    .endif
LB580:
    JSR LB565
    .if version < 3
        JSR LB558
    .endif
    DEX
    BNE LB580
LB589:
    RTS

LB58A:
    INC zp0A
    JSR L9B1D
    JSR L984C
    JSR L92EE
    LDA zp2A
    STA zp1F
    JMP L8AF6

; LIST [linenum [,linenum]]
; =========================
LB59C:
    INY
    LDA (zp0B),Y
    CMP #'O'
    BEQ LB58A
    LDA #$00
    STA zp3B
    STA zp3C
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    JSR L97DF
    PHP
    JSR LBD94
    LDA #$FF
    STA zp2A
    LDA #$7F
    STA zp2B
    PLP
    BCC LB5CF
    JSR L8A97
    CMP #','
    BEQ LB5D8
    JSR LBDEA
    JSR LBD94
    DEC zp0A
    BPL LB5DB
LB5CF:
    JSR L8A97
    CMP #','
    BEQ LB5D8
    DEC zp0A
LB5D8:
    JSR L97DF
LB5DB:
    LDA zp2A
    STA zp31
    LDA zp2B
    STA zp32
    JSR L9857
    JSR LBE6F
    JSR LBDEA
    JSR L9970
    LDA zp3D
    STA zp0B
    LDA zp3E
    STA zp0C
    BCC LB60F
    DEY
    BCS LB602

LB5FC:
    JSR LBC25
    JSR L986D
LB602:
    LDA (zp0B),Y
    STA zp2B
    INY
    LDA (zp0B),Y
    STA zp2A
    INY
    INY
    STY zp0A
LB60F:
    LDA zp2A
    CLC
    SBC zp31
    LDA zp2B
    SBC zp32
    BCC LB61D
    JMP L8AF6

LB61D:
    JSR L9923
    LDX #$FF
    STX zp4D
    LDA #$01
    JSR LB577
    LDX zp3B
    LDA #$02
    JSR LB577
    LDX zp3C
    LDA #$04
    JSR LB577
LB637:
    LDY zp0A
LB639:
    LDA (zp0B),Y
    CMP #$0D
    BEQ LB5FC
    CMP #$22
    BNE LB651
    LDA #$FF
    EOR zp4D
    STA zp4D
    LDA #$22
LB64B:
    JSR LB558
    INY
    BNE LB639
LB651:
    BIT zp4D
    BPL LB64B
    CMP #$8D
    BNE LB668
    JSR L97EB
    STY zp0A
    LDA #$00
    STA zp14
    JSR L991F
    JMP LB637

LB668:
    CMP #$E3
    BNE LB66E
    INC zp3B
LB66E:
    CMP #$ED
    BNE LB678
    LDX zp3B
    BEQ LB678
    DEC zp3B
LB678:
    CMP #$F5
    BNE LB67E
    INC zp3C
LB67E:
    CMP #$FD
    BNE LB688
    LDX zp3C
    BEQ LB688
    DEC zp3C
LB688:
    JSR LB50E
    INY
    BNE LB639
LB68E:
    BRK
    dta $20
    FNfold 'No '
    dta tknFOR
    BRK

; NEXT [variable [,...]]
; ======================
LB695:
    JSR L95C9
    BNE LB6A3
    LDX zp26
    BEQ LB68E
    BCS LB6D7
LB6A0:
    JMP L982A

LB6A3:
    BCS LB6A0
    LDX zp26
    BEQ LB68E
LB6A9:
    LDA zp2A
    CMP ws+$04F1,X
    BNE LB6BE
    LDA zp2B
    CMP ws+$04F2,X
    BNE LB6BE
    LDA zp2C
    CMP ws+$04F3,X
    BEQ LB6D7
LB6BE:
    TXA
    SEC
    SBC #$0F
    TAX
    STX zp26
    BNE LB6A9
    BRK
    dta zp21
    FNfold 'Can'
    dta 0x27, 't Match ', tknFOR
    BRK

LB6D7:
    LDA ws+$04F1,X
    STA zp2A
    LDA ws+$04F2,X
    STA zp2B
    LDY ws+$04F3,X
    CPY #$05
    BEQ LB766
    LDY #$00
    LDA (zp2A),Y
    ADC ws+$04F4,X
    STA (zp2A),Y
    STA zp37
    INY
    LDA (zp2A),Y
    ADC ws+$04F5,X
    STA (zp2A),Y
    STA zp38
    INY
    LDA (zp2A),Y
    ADC ws+$04F6,X
    STA (zp2A),Y
    STA zp39
    INY
    LDA (zp2A),Y
    ADC ws+$04F7,X
    STA (zp2A),Y
    TAY
    LDA zp37
    SEC
    SBC ws+$04F9,X
    STA zp37
    LDA zp38
    SBC ws+$04FA,X
    STA zp38
    LDA zp39
    SBC ws+$04FB,X
    STA zp39
    TYA
    SBC ws+$04FC,X
    ORA zp37
    ORA zp38
    ORA zp39
    BEQ LB741
    TYA
    EOR ws+$04F7,X
    EOR ws+$04FC,X
    BPL LB73F
    BCS LB741
    BCC LB751
LB73F:
    BCS LB751
LB741:
    LDY ws+$04FE,X
    LDA ws+$04FF,X
    STY zp0B
    STA zp0C
    JSR L9877
    JMP L8BA3

LB751:
    LDA zp26
    SEC
    SBC #$0F
    STA zp26
    LDY zp1B
    STY zp0A
    JSR L8A97
    CMP #','
    BNE LB7A1
    JMP LB695

LB766:
    JSR LB354
    LDA zp26
    CLC
    ADC #$F4
    STA zp4B
    LDA #$05+(ws/256)
    STA zp4C
    JSR LA500
    LDA zp2A
    STA zp37
    LDA zp2B
    STA zp38
    JSR LB4E9
    LDA zp26
    STA zp27
    CLC
    ADC #$F9
    STA zp4B
    LDA #$05+(ws/256)
    STA zp4C
    JSR L9A5F
    BEQ LB741
    LDA ws+$04F5,X
    BMI LB79D
    BCS LB741
    BCC LB751
LB79D:
    BCC LB741
    BCS LB751
LB7A1:
    JMP L8B96

LB7A4:
    BRK
    dta $22
    dta tknFOR
    FNfold ' variable'
LB7B0:
    BRK
    dta $23
    FNfold 'Too many '
    dta tknFOR, 's'
LB7BD:
    BRK
    dta $24
    FNfold 'No '
    dta tknTO
    BRK

; FOR numvar = numeric TO numeric [STEP numeric]
; ==============================================
LB7C4:
    JSR L9582
    BEQ LB7A4
    BCS LB7A4
    JSR LBD94
    JSR L9841
    JSR LB4B1
    LDY zp26
    CPY #$96
    BCS LB7B0
    LDA zp37
    STA ws+$0500,Y
    LDA zp38
    STA ws+$0501,Y
    LDA zp39
    STA ws+$0502,Y
    TAX
    JSR L8A8C
    CMP #$B8
    BNE LB7BD
    CPX #$05
    BEQ LB84F
    JSR L92DD
    LDY zp26
    LDA zp2A
    STA ws+$0508,Y
    LDA zp2B
    STA ws+$0509,Y
    LDA zp2C
    STA ws+$050A,Y
    LDA zp2D
    STA ws+$050B,Y
    LDA #$01
    .if version < 3
        JSR LAED8
    .elseif version >= 3
        JSR XAED3
    .endif
    JSR L8A8C
    CMP #tknSTEP
    BNE LB81F
    JSR L92DD
    LDY zp1B
LB81F:
    STY zp0A
    LDY zp26
    LDA zp2A
    STA ws+$0503,Y
    LDA zp2B
    STA ws+$0504,Y
    LDA zp2C
    STA ws+$0505,Y
    LDA zp2D
    STA ws+$0506,Y
LB837:
    JSR L9880
    LDY zp26
    LDA zp0B
    STA ws+$050D,Y
    LDA zp0C
    STA ws+$050E,Y
    CLC
    TYA
    ADC #$0F
    STA zp26
    JMP L8BA3

LB84F:
    JSR L9B29
    JSR L92FD
    LDA zp26
    CLC
    ADC #$08
    STA zp4B
    LDA #$05+(ws/256)
    STA zp4C
    JSR LA38D
    JSR LA699
    JSR L8A8C
    CMP #$88
    BNE LB875
    JSR L9B29
    JSR L92FD
    LDY zp1B
LB875:
    STY zp0A
    LDA zp26
    CLC
    ADC #$03
    STA zp4B
    LDA #$05+(ws/256)
    STA zp4C
    JSR LA38D
    JMP LB837

; GOSUB numeric
; =============
LB888:
    JSR LB99A
LB88B:
    JSR L9857
    LDY zp25
    CPY #$1A
    BCS LB8A2
    LDA zp0B
    STA ws+$05CC,Y
    LDA zp0C
    STA ws+$05E6,Y
    INC zp25
    BCC LB8D2

LB8A2:
    BRK
    dta $25
    FNfold 'Too many '
    dta tknGOSUB, 's'
LB8AF:
    BRK
    dta $26
    FNfold 'No '
    dta tknGOSUB
    BRK

; RETURN
; ======
LB8B6:
    JSR L9857                  ; Check for end of statement
    LDX zp25
    BEQ LB8AF          ; If GOSUB stack empty, error
    DEC zp25                    ; Decrement GOSUB stack
    LDY ws+$05CB,X             ; Get stacked line pointer
    LDA ws+$05E5,X
    STY zp0B
    STA zp0C            ; Set line pointer
    JMP L8B9B                  ; Jump back to execution loop

; GOTO numeric
; ============
LB8CC:
    JSR LB99A
    JSR L9857        ; Find destination line, check for end of statement
LB8D2:
    LDA zp20
    BEQ LB8D9
    JSR L9905; If TRACE ON, print current line number
LB8D9:
    LDY zp3D
    LDA zp3E            ; Get destination line address
LB8DD:
    STY zp0B
    STA zp0C            ; Set line pointer
    JMP L8BA3                  ; Jump back to execution loop

; ON ERROR OFF
; ------------
LB8E4:
    JSR L9857                  ; Check end of statement
    LDA #<LB433
    STA zp16                    ; ON ERROR OFF
    LDA #>LB433
    STA zp17
    JMP L8B9B                  ; Jump to execution loop

; ON ERROR [OFF | program ]
; -------------------------
LB8F2:
    JSR L8A97
    CMP #tknOFF
    BEQ LB8E4      ; ON ERROR OFF
    LDY zp0A
    DEY
    JSR L986D
    LDA zp0B
    STA zp16            ; Point ON ERROR pointer to here
    LDA zp0C
    STA zp17
    JMP L8B7D                  ; Skip past end of line

LB90A:
    BRK
    dta $27
    dta tknON
    FNfold ' syntax'
    BRK

; ON [ERROR] [numeric]
; ====================
LB915:
    JSR L8A97                  ; Skip spaces and get next character
    CMP #tknERROR
    BEQ LB8F2    ; Jump with ON ERROR
    DEC zp0A
    JSR L9B1D
    JSR L92F0
    LDY zp1B
    INY
    STY zp0A
    CPX #tknGOTO
    BEQ LB931
    CPX #tknGOSUB
    BNE LB90A
LB931:
    TXA
    PHA                    ; Save GOTO/GOSUB token
    LDA zp2B
    ORA zp2C            ; Get IntA
    ORA zp2D
    BNE LB97D          ; ON >255 - out of range, look for an ELSE
    LDX zp2A
    BEQ LB97D          ; ON zero - out of range, look for an ELSE
    DEX
    BEQ LB95C              ; Dec. counter, if zero use first destination
    LDY zp0A                    ; Get line index
LB944:
    LDA (zp0B),Y
    INY
    CMP #$0D
    BEQ LB97D         ; End of line - error
    CMP #':'
    BEQ LB97D      ; End of statement - error
    CMP #tknELSE
    BEQ LB97D     ; ELSE - drop everything else to here
    CMP #','
    BNE LB944      ; No comma, keep looking
    DEX
    BNE LB944              ; Comma found, loop until count decremented to zero
    STY zp0A                    ; Store line index
LB95C:
    JSR LB99A                  ; Read line number
    PLA                        ; Get stacked token back
    CMP #tknGOSUB
    BEQ LB96A    ; Jump to do GOSUB
    JSR L9877                  ; Update line index and check Escape
    JMP LB8D2                  ; Jump to do GOTO

; Update line pointer so RETURN comes back to next statement
; ----------------------------------------------------------
LB96A:
    LDY zp0A                    ; Get line pointer
LB96C:
    LDA (zp0B),Y
    INY            ; Get character from line
    CMP #$0D
    BEQ LB977         ; End of line, RETURN to here
    CMP #':'
    BNE LB96C      ; <colon>, return to here
LB977:
    DEY
    STY zp0A                ; Update line index to RETURN point
    JMP LB88B                  ; Jump to do the GOSUB

; ON num out of range - check for an ELSE clause
; ----------------------------------------------
LB97D:
    LDY zp0A                    ; Get line index
    PLA                        ; Drop GOTO/GOSUB token
LB980:
    LDA (zp0B),Y
    INY            ; Get character from line
    CMP #tknELSE
    BEQ LB995     ; Found ELSE, jump to use it
    CMP #$0D
    BNE LB980         ; Loop until end of line
    BRK
    dta $28
    dta tknON
    FNfold ' range'
    BRK

LB995:
    STY zp0A
    JMP L98E3          ; Store line index and jump to GOSUB

LB99A:
    JSR L97DF
    BCS LB9AF        ; Embedded line number found
    JSR L9B1D
    JSR L92F0        ; Evaluate expression, ensure integer
    LDA zp1B
    STA zp0A            ; Line number low byte
    LDA zp2B
    AND #$7F
    STA zp2B   ; Line number high byte
                               ; Note - this makes goto $8000+10 the same as goto 10
LB9AF:
    JSR L9970
    BCS LB9B5
    RTS    ; Look for line, error if not found

LB9B5:
    BRK
    dta $29
    FNfold 'No such line'
    BRK

LB9C4:
    JMP L8C0E

LB9C7:
    JMP L982A

LB9CA:
    STY zp0A
    JMP L8B98

; INPUT#channel, ...
; ------------------
LB9CF:
    DEC zp0A
    JSR LBFA9
    LDA zp1B
    STA zp0A
    STY zp4D
LB9DA:
    JSR L8A97
    CMP #','
    BNE LB9CA
    LDA zp4D
    PHA
    JSR L9582
    BEQ LB9C7
    LDA zp1B
    STA zp0A
    PLA
    STA zp4D
    PHP
    JSR LBD94
    LDY zp4D
    JSR OSBGET
    STA zp27
    PLP
    BCC LBA19
    LDA zp27
    BNE LB9C4
    JSR OSBGET
    STA zp36
    TAX
    BEQ LBA13
LBA0A:
    JSR OSBGET
    STA ws+$05FF,X
    DEX
    BNE LBA0A
LBA13:
    JSR L8C1E
    JMP LB9DA

LBA19:
    LDA zp27
    BEQ LB9C4
    BMI LBA2B
    LDX #$03
LBA21:
    JSR OSBGET
    STA zp2A,X
    DEX
    BPL LBA21
    BMI LBA39
LBA2B:
    LDX #$04
LBA2D:
    JSR OSBGET
    STA ws+$046C,X
    DEX
    BPL LBA2D
    JSR LA3B2
LBA39:
    JSR LB4B4
    JMP LB9DA

LBA3F:
    PLA
    PLA
    JMP L8B98

; INPUT [LINE] [print items][variables]
; =====================================
LBA44:
    JSR L8A97                 ; Get next non-space char
    CMP #'#'
    BEQ LB9CF     ; If '#' jump to do INPUT#
    CMP #tknLINE
    BEQ LBA52    ; If 'LINE', skip next with CS
    DEC zp0A
    CLC               ; Step back to non-LINE char, set CC
LBA52:
    ROR zp4D
    LSR zp4D           ; bit7=0, bit6=notLINE/LINE
    LDA #$FF
    STA zp4E
LBA5A:
    JSR L8E8A
    BCS LBA69       ; Process ' " TAB SPC, jump if none found
LBA5F:
    JSR L8E8A
    BCC LBA5F       ; Keep processing any print items
    LDX #$FF
    STX zp4E
    CLC
LBA69:
    PHP
    ASL zp4D
    PLP
    ROR zp4D
    CMP #','
    BEQ LBA5A     ; ',' - jump to do next item
    CMP #';'
    BEQ LBA5A     ; ';' - jump to do next item
    DEC zp0A
    LDA zp4D
    PHA
    LDA zp4E
    PHA
    JSR L9582
    BEQ LBA3F
    PLA
    STA zp4E
    PLA
    STA zp4D
    LDA zp1B
    STA zp0A
    PHP
    BIT zp4D
    BVS LBA99
    LDA zp4E
    CMP #$FF
    BNE LBAB0
LBA99:
    BIT zp4D
    BPL LBAA2
    LDA #'?'
    JSR LB558
LBAA2:
    JSR LBBFC ; Call MOS to input line, set COUNT=0
    STY zp36
    ASL zp4D
    CLC
    ROR zp4D
    BIT zp4D
    BVS LBACD
LBAB0:
    STA zp1B
    LDA #$00
    STA zp19
    LDA #$06+(ws/256)
    STA zp1A
    JSR LADAD
LBABD:
    JSR L8A8C
    CMP #','
    BEQ LBACA
    CMP #$0D
    BNE LBABD
    LDY #$FE
LBACA:
    INY
    STY zp4E
LBACD:
    PLP
    BCS LBADC
    JSR LBD94
    JSR LAC34
    JSR LB4B4
    JMP LBA5A

LBADC:
    LDA #$00
    STA zp27
    JSR L8C21
    JMP LBA5A

; RESTORE [linenum]
; =================
LBAE6:
    LDY #$00
    STY zp3D     ; Set DATA pointer to PAGE
    LDY zp18
    STY zp3E
    JSR L8A97
    DEC zp0A
    CMP #':'
    BEQ LBB07
    CMP #$0D
    BEQ LBB07
    CMP #tknELSE
    BEQ LBB07
    JSR LB99A
    LDY #$01
    JSR LBE55
LBB07:
    JSR L9857
    LDA zp3D
    STA zp1C
    LDA zp3E
    STA zp1D
    JMP L8B9B

LBB15:
    JSR L8A97
    CMP #','
    BEQ LBB1F
    JMP L8B96

; READ varname [,...]
; ===================
LBB1F:
    JSR L9582
    BEQ LBB15
    BCS LBB32
    JSR LBB50
    JSR LBD94
    JSR LB4B1
    JMP LBB40

LBB32:
    JSR LBB50
    JSR LBD94
    JSR LADAD
    STA zp27
    JSR L8C1E
LBB40:
    CLC
    LDA zp1B
    ADC zp19
    STA zp1C
    LDA zp1A
    ADC #$00
    STA zp1D
    JMP LBB15

LBB50:
    LDA zp1B
    STA zp0A
    LDA zp1C
    STA zp19
    LDA zp1D
    STA zp1A
    LDY #$00
    STY zp1B
    JSR L8A8C
    CMP #','
    BEQ LBBB0
    CMP #tknDATA
    BEQ LBBB0
    CMP #$0D
    BEQ LBB7A
LBB6F:
    JSR L8A8C
    CMP #','
    BEQ LBBB0
    CMP #$0D
    BNE LBB6F
LBB7A:
    LDY zp1B
    LDA (zp19),Y
    BMI LBB9C
    INY
    INY
    LDA (zp19),Y
    TAX
LBB85:
    INY
    LDA (zp19),Y
    CMP #$20
    BEQ LBB85
    CMP #tknDATA
    BEQ LBBAD
    TXA
    CLC
    ADC zp19
    STA zp19
    BCC LBB7A
    INC zp1A
    BCS LBB7A
LBB9C:
    BRK
    dta $2A
    FNfold 'Out of '
    dta tknDATA
LBBA6:
    BRK
    dta $2B
    FNfold 'No '
    dta tknREPEAT
    BRK

LBBAD:
    INY
    STY zp1B
LBBB0:
    RTS

; UNTIL numeric
; =============
LBBB1:
    JSR L9B1D
    JSR L984C
    JSR L92EE
    LDX zp24
    BEQ LBBA6
    LDA zp2A
    ORA zp2B
    ORA zp2C
    ORA zp2D
    BEQ LBBCD
    DEC zp24
    JMP L8B9B

LBBCD:
    LDY ws+$05A3,X
    LDA ws+$05B7,X
    JMP LB8DD
;xxx

LBBD6:
    BRK
    dta $2C
    FNfold 'Too many '
    dta tknREPEAT, 's'
    BRK

; REPEAT
; ======
LBBE4:
    LDX zp24
    CPX #$14
    BCS LBBD6
    JSR L986D
    LDA zp0B
    STA ws+$05A4,X
    LDA zp0C
    STA ws+$05B8,X
    INC zp24
    JMP L8BA3

; Input string to string buffer
; -----------------------------
LBBFC:
    LDY #$00
    LDA #$06+(ws/256) ; String buffer at $0600
    BNE LBC09

; Print character, read input line
; --------------------------------
LBC02:
    JSR LB558                  ; Print character
    LDY #$00
    LDA #$07+(ws/256) ; $AAYY=input buffer at $0700

LBC09:
    STY zp37
    STA zp38            ; $37/8=>input buffer

; Manually implement RDLINE (OSWORD 0)
; ------------------------------------
    .ifdef MOS_ATOM
LDBE4:
        JSR OSRDCH          ; Wait for character
        CMP #$1B
        BEQ LDC21  ; Escape
        CMP #$7F
        BNE LDBFA  ; Not Delete
        CPY #$00
        BEQ LDBE4  ; Nothing to delete
        JSR OSWRCH          ; VDU 127
        DEY
        JMP LDBE4       ; Dec. counter, loop back
     
LDBFA:
        CMP #$15
        BNE LDC0B  ; Not Ctrl-U
        TYA
        BEQ LDBE4
        LDA #$7F
LDC03:
        JSR OSWRCH
        DEY
        BNE LDC03
        BEQ LDBE4
     
LDC0B:
        STA (zp37),Y         ; Store character
        CMP #$0D
        BEQ LBC25  ; Return - finish
        CPY #$EE
        BCS LDC1E  ; Maximum length
        CMP #$20
        BCS LDC1A  ; Control character
        DEY
LDC1A:
        INY
        JSR OSWRCH      ; Inc. counter, print character
LDC1E:
        JMP LDBE4           ; Loop for more
LDC21:
    .endif

; BBC - Call MOS to read a line
; -----------------------------
    .ifdef MOS_BBC
        LDA #$EE
        STA zp39   ; Maximum length
        LDA #$20
        STA zp3A   ; Lowest acceptable character
        LDY #$FF
        STY zp3B   ; Highest acceptable character
        INY
        LDX #$37       ; XY=>control block at $0037
        TYA
        JSR OSWORD     ; Call OSWORD 0 to read line of text
        BCC LBC28          ; CC, Escape not pressed, exit and set COUNT=0
    .endif
    JMP L9838           ; Escape

LBC25:
    JSR OSNEWL
LBC28:
    LDA #$00
    STA zp1E    ; Set COUNT to zero
    RTS

LBC2D:
    JSR L9970
    BCS LBC80
    LDA zp3D
    SBC #$02
    STA zp37
    STA zp3D
    STA zp12
    LDA zp3E
    SBC #$00
    STA zp38
    STA zp13
    STA zp3E
    LDY #$03
    LDA (zp37),Y
    CLC
    ADC zp37
    STA zp37
    BCC LBC53
    INC zp38
LBC53:
    LDY #$00
LBC55:
    LDA (zp37),Y
    STA (zp12),Y
    CMP #$0D
    BEQ LBC66
LBC5D:
    INY
    BNE LBC55
    INC zp38
    INC zp13
    BNE LBC55
LBC66:
    INY
    BNE LBC6D
    INC zp38
    INC zp13
LBC6D:
    LDA (zp37),Y
    STA (zp12),Y
    BMI LBC7C
    JSR LBC81
    JSR LBC81
    JMP LBC5D

LBC7C:
    JSR LBE92
    CLC
LBC80:
    RTS

LBC81:
    INY
    BNE LBC88
    INC zp13
    INC zp38
LBC88:
    LDA (zp37),Y
    STA (zp12),Y
    RTS

LBC8D:
    STY zp3B
    JSR LBC2D
    LDY #$07+(ws/256)
    STY zp3C
    LDY #$00
    LDA #$0D
    CMP (zp3B),Y
    BEQ LBD10
LBC9E:
    INY
    CMP (zp3B),Y
    BNE LBC9E
    INY
    INY
    INY
    STY zp3F
    INC zp3F
    LDA zp12
    STA zp39
    LDA zp13
    STA zp3A
    JSR LBE92
    STA zp37
    LDA zp13
    STA zp38
    DEY
    LDA zp06
    CMP zp12
    LDA zp07
    SBC zp13
    BCS LBCD6
    JSR LBE6F
    JSR LBD20
    BRK
    dta 0
    dta tknLINE
    FNfold ' space'
    BRK

LBCD6:
    LDA (zp39),Y
    STA (zp37),Y
    TYA
    BNE LBCE1
    DEC zp3A
    DEC zp38
LBCE1:
    DEY
    TYA
    ADC zp39
    LDX zp3A
    BCC LBCEA
    INX
LBCEA:
    CMP zp3D
    TXA
    SBC zp3E
    BCS LBCD6
    SEC
    LDY #$01
    LDA zp2B
    STA (zp3D),Y
    INY
    LDA zp2A
    STA (zp3D),Y
    INY
    LDA zp3F
    STA (zp3D),Y
    JSR LBE56
    LDY #$FF
LBD07:
    INY
    LDA (zp3B),Y
    STA (zp3D),Y
    CMP #$0D
    BNE LBD07
LBD10:
    RTS

; RUN
; ===
LBD11:
    JSR L9857
LBD14:
    JSR LBD20
    LDA zp18
    STA zp0C           ; Point PtrA to PAGE
    STX zp0B
    JMP L8B0B

; Clear BASIC heap, stack and DATA pointer
; ========================================
LBD20:
    LDA zp12
    STA ZP00
    STA zp02      ; LOMEM=TOP, VAREND=TOP
    LDA zp13
    STA ZP01
    STA zp03
    JSR LBD3A                     ; Clear DATA and stack
LBD2F:
    LDX #$80
    LDA #$00
LBD33:
    STA ws+$047F,X
    DEX
    BNE LBD33
    RTS ; Clear dynamic variables list

; Clear DATA pointer and BASIC stack
; ==================================
LBD3A:
    LDA zp18
    STA zp1D                 ; DATA pointer hi=PAGE hi
    LDA zp06
    STA zp04
    LDA zp07
    STA zp05 ; STACK=HIMEM
    LDA #$00
    STA zp24
    STA zp26
    STA zp25; Clear REPEAT, FOR, GOSUB stacks
    STA zp1C
    RTS                     ; DATA pointer=PAGE

LBD51:
    LDA zp04
    SEC
    SBC #$05
    JSR LBE2E
    LDY #$00
    LDA zp30
    STA (zp04),Y
    INY
    LDA zp2E
    AND #$80
    STA zp2E
    LDA zp31
    AND #$7F
    ORA zp2E
    STA (zp04),Y
    INY
    LDA zp32
    STA (zp04),Y
    INY
    LDA zp33
    STA (zp04),Y
    INY
    LDA zp34
    STA (zp04),Y
    RTS

LBD7E:
    LDA zp04
    CLC
    STA zp4B
    ADC #$05
    STA zp04
    LDA zp05
    STA zp4C
    ADC #$00
    STA zp05
    RTS

LBD90:
    BEQ LBDB2
    BMI LBD51
LBD94:
    LDA zp04
    SEC
    SBC #$04
LBD99:
    JSR LBE2E
    LDY #$03
    LDA zp2D
    STA (zp04),Y
    DEY
    LDA zp2C
    STA (zp04),Y
    DEY
    LDA zp2B
    STA (zp04),Y
    DEY
    LDA zp2A
    STA (zp04),Y
    RTS

; Stack the current string
; ========================
LBDB2:
    CLC
    LDA zp04
    SBC zp36        ; stackbot=stackbot-length-1
    JSR LBE2E                  ; Check enough space
    LDY zp36
    BEQ LBDC6          ; Zero length, just stack length
LBDBE:
    LDA ws+$05FF,Y
    STA (zp04),Y ; Copy string to stack
    DEY
    BNE LBDBE              ; Loop for all characters
LBDC6:
    LDA zp36
    STA (zp04),Y        ; Copy string length
    RTS

; Unstack a string
; ================
LBDCB:
    LDY #$00
    LDA (zp04),Y       ; Get stacked string length
    STA zp36
    BEQ LBDDC
    TAY      ; If zero length, just unstack length
LBDD4:
    LDA (zp04),Y
    STA ws+$05FF,Y ; Copy string to string buffer
    DEY
    BNE LBDD4              ; Loop for all characters
LBDDC:
    LDY #$00
    LDA (zp04),Y       ; Get string length again
    SEC
LBDE1:
    ADC zp04
    STA zp04            ; Update stack pointer
    BCC LBE0A
    INC zp05
    RTS

; Unstack an integer to IntA
; --------------------------
LBDEA:
    LDY #$03
    LDA (zp04),Y
    STA zp2D
    DEY    ; Copy to IntA
    LDA (zp04),Y
    STA zp2C
    DEY
    LDA (zp04),Y
    STA zp2B
    DEY
    LDA (zp04),Y
    STA zp2A
LBDFF:
    CLC
    LDA zp04
    ADC #$04
    STA zp04           ; Drop 4 bytes from stack
    BCC LBE0A
    INC zp05
LBE0A:
    RTS

; Unstack an integer to zero page
; -------------------------------
LBE0B:
    LDX #$37
LBE0D:
    LDY #$03
    LDA (zp04),Y
    STA zp03,X
    DEY
    LDA (zp04),Y
    STA zp02,X
    DEY
    LDA (zp04),Y
    STA zp01,X
    DEY
    LDA (zp04),Y
    STA zp00,X
    CLC
    LDA zp04
    ADC #$04
    STA zp04   ; Drop 4 bytes from stack
    BCC LBE0A
    INC zp05
    RTS

LBE2E:
    STA zp04
    BCS LBE34
    DEC zp05
LBE34:
    LDY zp05
    CPY zp03
    BCC LBE41
    BNE LBE40
    CMP zp02
    BCC LBE41
LBE40:
    RTS

LBE41:
    JMP L8CB7

LBE44:
    LDA zp2A
    STA zp00,X
    LDA zp2B
    STA zp01,X
    LDA zp2C
    STA zp02,X
    LDA zp2D
    STA zp03,X
    RTS

LBE55:
    CLC
LBE56:
    TYA
    ADC zp3D
    STA zp3D
    BCC LBE5F
    INC zp3E
LBE5F:
    LDY #$01
    RTS

LBE62:
    JSR LBEDD
    TAY
    LDA #$FF    ; FILE.LOAD=PAGE

    .ifdef MOS_ATOM
        STA F_EXEC+0
        LDX #$37    ; FILE.EXEC=$FF, load to specified address
        SEC
        JSR OSLOAD
    .endif

    .ifdef MOS_BBC
        STY F_EXEC+0
        LDX #$37    ; FILE.EXEC=0, load to specified address
        JSR OSFILE
    .endif

; Scan program to check consistancy and find TOP
; ----------------------------------------------
LBE6F:
    LDA zp18
    STA zp13
    LDY #$00
    STY zp12
    INY      ; Point TOP to PAGE
LBE78:
    DEY
    LDA (zp12),Y           ; Get byte preceding line
    CMP #$0D
    BNE LBE9E        ; Not <cr>, jump to 'Bad program'
    INY                       ; Step to line number/terminator
    LDA (zp12),Y
    BMI LBE90     ; b7 set, end of program
    LDY #$03                  ; Point to line length
    LDA (zp12),Y
    BEQ LBE9E     ; Zero length, jump to 'Bad program'
    CLC
    JSR LBE93             ; Update TOP to point to next line
    BNE LBE78                 ; Loop to check next line

; End of program found, set TOP
; -----------------------------
LBE90:
    INY
    CLC
LBE92:
    TYA
LBE93:
    ADC zp12
    STA zp12           ; TOP=TOP+A
    BCC LBE9B
    INC zp13
LBE9B:
    LDY #$01
    RTS              ; Return Y=1, NE

; Report 'Bad program' and jump to immediate mode
; -----------------------------------------------
LBE9E:
    JSR LBFCF                 ; Print inline text
    dta 13
    FNfold 'Bad program'
    dta 13
    NOP
    JMP L8AF6                 ; Jump to immediate mode

; Point $37/8 to <cr>-terminated string in string buffer
; ------------------------------------------------------
LBEB2:
    LDA #$00
    STA zp37
    LDA #$06+(ws/256)
    STA zp38
LBEBA:
    LDY zp36
    LDA #$0D
    STA ws+$0600,Y
    RTS

; OSCLI string$ - Pass string to OSCLI to execute
; ===============================================
LBEC2:
    JSR LBED2                   ; $37/8=>cr-string


    .ifdef MOS_ATOM
        JSR cmdStar1
        JMP L8B9B     ; Call Atom OSCLI and return to execution loop
     

    ; Embedded star command
    ; ---------------------
cmdStar:
        STX zp37
        STY zp38            ; $37/8=>cr-string
cmdStar1:
        LDY #$FF
cmdStarLp1:
        INY
        LDA (zp37),Y
        CMP #'*'
        BEQ cmdStarLp1 ; Skip leading stars
        LDX #0
cmdStarLp2:
        LDA (zp37),Y
        STA $0100,X         ; Copy string onto stack
        INY
        INX
        CMP #$0D
        BNE cmdStarLp2 ; Atom OSCLI passed string at $100
        JMP OS_CLI
    .endif

    .ifdef MOS_BBC
        LDX #$00
        LDY #>(ws+$0600)
        JSR OS_CLI
        JMP L8B9B       ; Call OSCLI and return to execution loop
    .endif

LBECF:
    JMP L8C0E

LBED2:
    JSR L9B1D
    BNE LBECF         ; Evaluate expression, error if not string
    JSR LBEB2
    JMP L984C         ; Convert to <cr>-string, check end of statement

; Set FILE.LOAD to MEMHI.PAGE
; ---------------------------
LBEDD:
    JSR LBED2
    DEY
    STY F_LOAD+0  ; LOAD.lo=$00
    LDA zp18
    STA F_LOAD+1        ; LOAD.hi=PAGEhi
LBEE7:
    .ifdef MOS_BBC
        LDA #$82
        JSR OSBYTE        ; Get memory base high word
        STX F_LOAD+2
        STY F_LOAD+3  ; Set LOAD high word
        LDA #$00
    .endif
    RTS

; BBC At/Sy
; 37   37   FNAME
; 38
; 39   39   LOAD
; 3A
; 3B
; 3C
; 3D   3B   EXEC
; 3E
; 3F
; 40
; 41   3D   START
; 42
; 43
; 44
; 45   3F   END
; 46
; 47
; 48

;  SAVE string$
; =============
LBEF3:
    JSR LBE6F                         ; Check program, set TOP

    .if version < 3
        LDA zp12
        STA F_END+0              ; Set FILE.END to TOP
        LDA zp13
        STA F_END+1
        LDA #<L8023
        STA F_EXEC+0  ; Set FILE.EXEC to STARTUP
        LDA #>L8023
        STA F_EXEC+1
        LDA zp18
        STA F_START+1            ; Set FILE.START to PAGE
        JSR LBEDD                        ; Set FILE.LOAD to PAGE
    .endif
    .if version < 3
        .ifdef MOS_BBC
            STX F_EXEC+2 
            STY F_EXEC+3       ; Set address high words
            STX F_START+2
            STY F_START+3
            STX F_END+2  
            STY F_END+3
        .endif
        .ifdef MOS_ATOM
            STY F_START+0                    ; Low byte of FILE.START
        .endif
        .ifdef MOS_BBC
            STA F_START+0                    ; Low byte of FILE.START
        .endif
    .endif

    .if version >= 3
        JSR LBEDD                        ; Set FILE.LOAD to PAGE
    .endif
    .if version >= 3
        .ifdef MOS_BBC
            STX F_EXEC+2 
            STY F_EXEC+3       ; Set address high words
            STX F_START+2
            STY F_START+3
            STX F_END+2  
            STY F_END+3
        .endif
        .ifdef MOS_ATOM
            STY F_START+0                    ; Low byte of FILE.START
        .endif
        .ifdef MOS_BBC
            STA F_START+0                    ; Low byte of FILE.START
        .endif
    .endif
    .if version >= 3
        LDX zp12
        STX F_END+0              ; Set FILE.END to TOP
        LDX zp13
        STX F_END+1
        LDX #<L8023
        STX F_EXEC+0  ; Set FILE.EXEC to STARTUP
        LDX #>L8023
        STX F_EXEC+1
        LDX zp18
        STX F_START+1            ; High byte of FILE.START=PAGE
    .endif
    TAY
    LDX #$37
    .ifdef MOS_ATOM
        SEC
        JSR OSSAVE
    .endif
    .ifdef MOS_BBC
        JSR OSFILE    
    .endif
    JMP L8B9B

; LOAD string$
; ============
LBF24:
    JSR LBE62
    JMP L8AF3               ; Do LOAD, jump to immediate mode

; CHAIN string$
; =============
LBF2A:
    JSR LBE62
    JMP LBD14               ; Do LOAD, jump to execution loop

; PTR#numeric=numeric
; ===================
LBF30:
    JSR LBFA9
    PHA             ; Evaluate #handle
    JSR L9813
    JSR L92EE       ; Step past '=', evaluate integer
    PLA
    TAY
    LDX #$2A          ; Get handle, point to IntA
    .ifdef MOS_ATOM
        JSR OSSTAR         
    .endif
    .ifdef MOS_BBC
        LDA #$01
        JSR OSARGS
    .endif
    JMP L8B9B                 ; Jump to execution loop

; =EXT#numeric - Read file pointer via OSARGS
; ===========================================
LBF46:
    SEC                       ; Flag to do =EXT

; =PTR#numeric - Read file pointer via OSARGS
; ===========================================
LBF47:
    LDA #$00
    ROL            ; A=0 or 1 for =PTR or =EXT
    .ifdef MOS_BBC
        ROL
    .endif
    PHA                       ; Atom - A=0/1, BBC - A=0/2
    JSR LBFB5
    LDX #$2A
    PLA    ; Evaluate #handle, point to IntA
    .ifdef MOS_ATOM
        JSR OSRDAR
    .endif
    .ifdef MOS_BBC
        JSR OSARGS
    .endif
    LDA #$40
    RTS              ; Return integer

; BPUT#numeric, numeric
; =====================
LBF58:
    JSR LBFA9
    PHA             ; Evaluate #handle
    JSR L8AAE
    JSR L9849
    JSR L92EE
    PLA
    TAY
    LDA zp2A
    JSR OSBPUT
    JMP L8B9B      ; Call OSBPUT, jump to execution loop

; =BGET#numeric
; =============
LBF6F:
    JSR LBFB5
    JSR OSBGET      ; Evaluate #handle
    .if version < 3
        JMP LAED8           ; Jump to return 8-bit integer
    .elseif version >= 3
        JMP XAED3           ; Jump to return 8-bit integer
    .endif

; OPENIN f$ - Call OSFIND to open file for input
; ==============================================
LBF78:
    .ifdef MOS_ATOM
        SEC             ; SEC=OPENUP
        BCS LBF82     
    .endif
    .ifdef MOS_BBC
        LDA #$40        ; $40=OPENUP
        BNE LBF82
    .endif

; OPENOUT f$ - Call OSFIND to open file for output
; ================================================
LBF7C:
    .ifdef MOS_ATOM
        CLC             ; CLC=OPENOUT
        BCC LBF82     
    .endif
    .ifdef MOS_BBC
        LDA #$80        ; 80=OPENOUT
        BNE LBF82
    .endif

; OPENUP f$ - Call OSFIND to open file for update
; ===============================================
LBF80:
    .ifdef MOS_ATOM
        SEC             ; SEC=OPENUP
    .endif
    .ifdef MOS_BBC
        LDA #$C0        ; C0=OPENUP
    .endif
LBF82:
    .ifdef MOS_ATOM
        PHP       
    .endif
    .ifdef MOS_BBC
        PHA       
    .endif
    JSR LADEC
    BNE LBF96       ; Evaluate, if not string, jump to error

    .ifdef MOS_ATOM
        JSR LBEB2        ; Terminate string with <cr>, point $37/8=>string
        LDX #$37
        PLP             ; Point to string pointer, get action back
    .endif

    .ifdef MOS_BBC
        JSR LBEBA                ; Terminate string with <cr>
        LDX #$00
        LDY #$06
        PLA    ; Point to string buffer, get action back
    .endif

    JSR OSFIND                ; Pass to OSFIND, jump to return integer from A
    .if version < 3
        JMP LAED8
    .elseif version >= 3
        JMP XAED3
    .endif

LBF96:
    JMP L8C0E                 ; Jump to 'Type mismatch' error

; CLOSE#numeric
; =============
LBF99:
    JSR LBFA9
    JSR L9852       ; Evaluate #handle, check end of statement
    LDY zp2A                   ; Get handle from IntA
    .ifdef MOS_ATOM
        JSR OSSHUT         
    .endif
    .ifdef MOS_BBC
        LDA #$00
        JSR OSFIND
    .endif
    JMP L8B9B                 ; Jump back to execution loop

; Copy PtrA to PtrB, then get handle
; ==================================
LBFA9:
    LDA zp0A
    STA zp1B           ; Set PtrB to program pointer in PtrA
    LDA zp0B
    STA zp19
    LDA zp0C
    STA zp1A

; Check for '#', evaluate channel
; ===============================
LBFB5:
    JSR L8A8C                 ; Skip spaces
    CMP #'#'               ; If not '#', jump to give error
    .if version < 3
        BNE LBFC3
    .elseif version >= 3
        BNE LBFF4
    .endif
    JSR L92E3                 ; Evaluate as integer
LBFBF:
    LDY zp2A
    TYA               ; Get low byte and return
NULLRET:
    RTS

    .if version < 3
LBFC3:
        BRK
        dta $2D
        FNfold 'Missing #'
        BRK
    .endif

; Print inline text
; =================
LBFCF:
    PLA
    STA zp37
    PLA
    STA zp38   ; Pop return address to pointer
    LDY #$00
    BEQ LBFDC        ; Jump into loop
LBFD9:
    JSR OSASCI                ; Print character
LBFDC:
    JSR L894B
    BPL LBFD9       ; Update pointer, get character, loop if b7=0
    JMP (zp37)               ; Jump back to program

; REPORT
; ======
LBFE4:
    JSR L9857
    JSR LBC25       ; Check end of statement, print newline, clear COUNT
    LDY #$01
LBFEC:
    LDA (FAULT),Y
    BEQ LBFF6   ; Get byte, exit if $00 terminator
    JSR LB500
    INY
    BNE LBFEC   ; Print character or token, loop for next
LBFF6:
    JMP L8B9B                 ; Jump to main execution loop

    .if version >= 3
LBFF4:
        BRK
        dta $2D
        FNfold 'Missing #'
        BRK
    .endif

    .if * > [load + $4000]
        .error "***WARNING: Code overrun"
    .endif

    .if version < 3 && [[*+6]&$ff] > 6
        BRK
        dta 'Roger'
        brk
    .endif

    .if [[*+3]&$ff] > 3
        dta version_string
    .endif

    .align load + $4000, 0
LC000:
