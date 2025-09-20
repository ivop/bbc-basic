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

; ----------------------------------------------------------------------------

; Temporary labels to make assembler happy

L8AB6=$8ab6
L8AC8=$8ac8
L8AD0=$8ad0
L8ADA=$8ada
L8ADD=$8add
L8B7D=$8b7d
L8BE4=$8be4
L8D9A=$8d9a
L8EBD=$8ebd
L8EC4=$8ec4
L8ED2=$8ed2
L8F31=$8f31
L8FA3=$8fa3
L90AC=$90ac
L912F=$912f
L925D=$925d
L926F=$926f
L9283=$9283
L928D=$928d
L9295=$9295
L92C9=$92c9
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
L982A=$982a
L98C2=$98c2
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
LB44C=$b44c
LB472=$b472
LB4A0=$b4a0
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
LBD11=$bd11
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
LBFE4=$bfe4
L8BA3=$8ba3
L8A97=$8a97
L986D=$986d
LB545=$b545
LB562=$b562
LBC25=$bc25
LB565=$b565
LB558=$b558
LB50E=$b50e
L9859=$9859
L8AF6=$8af6
L9890=$9890
L9582=$9582
LBD94=$bd94
LAE3A=$ae3a
LB4B4=$b4b4
L8827=$8827
L8821=$8821
L8813=$8813
L882C=$882c
L882F=$882f
L8832=$8832

