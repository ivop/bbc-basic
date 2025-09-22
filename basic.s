;
; Source for 6502 BASIC II/III
;
; BBC BASIC Copyright (C) 1982/1983 Acorn Computer and Roger Wilson
; Source reconstruction and commentary Copyright (C) J.G.Harston
;
; Conversion to Mad-Assember (mads) by Ivo van Poorten, September 2025
;

    opt h-              ; No Atari header

TARGET_BBC = 1
MOS_BBC    = 1
VERSION    = 2

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

;xxx
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

; ----------------------------------------------------------------------------

; Temporary labels to make assembler happy

L909F=$909F
L9222=$9222
L92DD=$92DD
L92E3=$92E3
L92EE=$92EE
L9456=$9456
L95D5=$95D5
L9852=$9852
L987B=$987B
L9EDF=$9EDF
LAE43=$AE43
LAE56=$AE56
LAED8=$AED8
LBC28=$BC28
LBC2D=$BC2D
LBE0D=$BE0D
L90AC=$90ac
L912F=$912f
L925D=$925d
L926F=$926f
L9283=$9283
L928D=$928d
L9295=$9295
L92C9=$92c9
L92F0=$92f0
L9304=$9304
L9323=$9323
L9356=$9356
L937A=$937a
L938E=$938e
L939A=$939a
L93E4=$93e4
L93E8=$93e8
L93F1=$93f1
L942F=$942f
L94FC=$94FC
L9531=$9531
L9582=$9582
L95DD=$95DD
L97DF=$97DF
L9813=$9813
L982A=$982a
L9841=$9841
L984C=$984C
L9857=$9857
L9859=$9859
L986D=$986d
L9890=$9890
L98C2=$98c2
L9B1D=$9b1d
L9B29=$9B29
LA385=$A385
LA6BE=$a6be
LA7B4=$a7b4
LA7FE=$a7fe
LA8D4=$a8d4
LA8DA=$a8da
LA907=$a907
LA98D=$a98d
LA998=$a998
LAA91=$aa91
LAB33=$ab33
LAB41=$ab41
LAB6D=$ab6d
LAB76=$ab76
LAB88=$ab88
LABA8=$aba8
LABB1=$abb1
LABC2=$abc2
LABCB=$abcb
LABD2=$abd2
LABE9=$abe9
LAC2F=$ac2f
LAC78=$ac78
LAC9E=$ac9e
LACAD=$acad
LACB8=$acb8
LACC4=$acc4
LACD1=$acd1
LACE2=$ace2
LAD6A=$ad6a
LAE3A=$ae3a
LAEB4=$aeb4
LAEC0=$aec0
LAECA=$aeca
LAED1=$aed1
LAEDC=$aedc
LAEF7=$aef7
LAEFC=$aefc
LAF03=$af03
LAF49=$af49
LAF9F=$af9f
LAFA6=$afa6
LAFB9=$afb9
LAFBF=$afbf
LAFCC=$afcc
LAFEE=$afee
LB026=$b026
LB039=$b039
LB094=$b094
LB0C2=$b0c2
LB195=$b195
LB3BD=$b3bd
LB402=$b402
LB433=$B433
LB44C=$b44c
LB472=$b472
LB4A0=$b4a0
LB4B4=$b4b4
LB50E=$b50e
LB545=$b545
LB558=$b558
LB562=$b562
LB565=$b565
LB59C=$b59c
LB695=$b695
LB7C4=$b7c4
LB888=$b888
LB8B6=$b8b6
LB8CC=$b8cc
LB915=$b915
LBA44=$ba44
LBAE6=$bae6
LBB1F=$bb1f
LBBB1=$bbb1
LBBE4=$bbe4
LBC02=$BC02
LBC25=$bc25
LBC8D=$BC8D
LBD11=$bd11
LBD20=$BD20
LBD3A=$BD3A
LBD94=$bd94
LBDDC=$BDDC
LBDE1=$BDE1
LBDEA=$BDEA
LBE44=$be44
LBE6F=$be6f
LBEBA=$BEBA
LBEC2=$bec2
LBEF3=$bef3
LBF24=$bf24
LBF2A=$bf2a
LBF30=$bf30
LBF46=$bf46
LBF47=$bf47
LBF58=$bf58
LBF6F=$bf6f
LBF78=$bf78
LBF7C=$bf7c
LBF80=$bf80
LBF99=$bf99
LBFA9=$BFA9
LBFE4=$bfe4
