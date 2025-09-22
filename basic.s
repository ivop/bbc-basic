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

;xxx fixup zp from here

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


; ----------------------------------------------------------------------------

; Temporary labels to make assembler happy

    icl 'undecl.s'
