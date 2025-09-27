;
; BBC BASIC II/III -- NMOS 6502
;
; Conversion to mads, labelling, bug fixes, and more comments by
; Ivo van Poorten, September 2025
;
; Based on source reconstruction and commentary © 2018 J.G.Harston
; https://mdfs.net/Software/BBCBasic/BBC/
;
; BBC BASIC Copyright © 1982/1983 Acorn Computer and Roger Wilson
;
; References used:
;   Advanced BASIC ROM USer Guide
;   BBC Micro Compendium
;   AcornCmosBasic (https://github.com/stardot/AcornCmosBasic)
;   AcornDmosBasic (https://github.com/stardot/AcornDmosBasic)
;   AcornBasic128 (github.com/stardot/AcornBasic128)
;

    opt h-            ; No Atari header

; ----------------------------------------------------------------------------

    .if .def BUILD_BBC_BASIC2 || .def BUILD_BBC_BASIC3 || .def BUILD_BBC_BASIC310HI
        TARGET_BBC = 1
        MOS_BBC    = 1

        .if .def BUILD_BBC_BASIC2
            load          = $8000     ; Code start address
            VERSION       = 2
            MINORVERSION  = 0
        .elseif .def BUILD_BBC_BASIC3
            load          = $8000     ; Code start address
            VERSION       = 3 
            MINORVERSION  = 0
        .elseif .def BUILD_BBC_BASIC310HI
            load          = $b800     ; Code start address
            VERSION       = 3 
            MINORVERSION  = 10
        .endif

        split   = 0
        foldup  = 0
        title   = 0
        ws      = $0400-$0400     ; Offset from &400 to workspace
        membot  = 0       ; Use OSBYTE to find memory limits
        memtop  = 0       ; ...

        zp      = $00     ; Start of ZP addresses

        FAULT  = $fd      ; Pointer to error block
        ESCFLG = $ff      ; Escape pending flag

        F_LOAD  = zp39    ; LOAD/SAVE control block
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
        BRKV   = $0202
        WRCHV  = $020E

        ; Dummy variables for non-Atom code

        OSECHO = 0
        OSLOAD = 0
        OSSAVE = 0
        OSRDAR = 0
        OSSTAR = 0
        OSSHUT = 0

    .elseif .def BUILD_SYSTEM_BASIC2 || .def BUILD_SYSTEM_BASIC310 || .def BUILD_ATOM_BASIC2 || .def BUILD_ATOM_BASIC310

        MOS_ATOM = 1

        .if .def BUILD_SYSTEM_BASIC2
            TARGET_SYSTEM = 1
            VERSION       = 2
            MINORVERSION  = 0
            foldup        = 0
        .elseif .def BUILD_SYSTEM_BASIC310
            TARGET_SYSTEM = 1
            VERSION       = 3
            MINORVERSION  = 10
            foldup = 0
        .elseif .def BUILD_ATOM_BASIC2
            TARGET_ATOM   = 1
            VERSION       = 2
            MINORVERSION  = 0
            foldup        = 1
        .elseif .def BUILD_ATOM_BASIC310
            TARGET_ATOM   = 1
            VERSION       = 3
            MINORVERSION  = 10
            foldup        = 1
        .endif

        split  = 0
        title  = 0

        .if .def TARGET_SYSTEM
            load   = $a000    ; Code start address
            ws     = $2800-$0400      ; Offset from &400 to workspace
            membot = $3000
            ESCFLG = $0e21    ; Escape pending flag
        .elseif .def TARGET_ATOM
            load   = $4000    ; Code start address
            ws     = $9c00-$0400      ; Offset from &400 to workspace
            membot = $2800
            ESCFLG = $b001    ; Escape pending flag
        .endif

        memtop  = load    ; Top of memory is start of code
        zp      = $00     ; Start of ZP addresses

        FAULT  = zp4F     ; Pointer to error block

        F_LOAD  = zp39    ; LOAD/SAVE control block
        F_EXEC  = F_LOAD+2
        F_START = F_LOAD+4
        F_END   = F_LOAD+6

        ; MOS Entry Points

        OS_CLI=$FFF7
        OSWRCH=$FFF4
        OSWRCR=$FFF2
        OSNEWL=$FFED
        OSASCI=$FFE9
        OSECHO=$FFE6
        OSRDCH=$FFE3
        OSLOAD=$FFE0
        OSSAVE=$FFDD
        OSRDAR=$FFDA
        OSSTAR=$FFD7
        OSBGET=$FFD4
        OSBPUT=$FFD1
        OSFIND=$FFCE
        OSSHUT=$FFCB
        BRKV=$0202
        WRCHV=$0208
      
        ; Dummy variables for non-BBC code

        OSBYTE=NULLRET
        OSWORD=NULLRET
        OSFILE=00000
        OSARGS=00000

    .elseif .def BUILD_C64_BASIC2

        TARGET_C64 = 1
        MOS_BBC    = 1

        load          = $b800     ; Code start address
        VERSION       = 2
        MINORVERSION  = 0

        split   = 0
        foldup  = 0
        title   = 0
        ws      = $0400-$0400     ; Offset from &400 to workspace
        membot  = 0       ; Use OSBYTE to find memory limits
        memtop  = 0       ; ...

        zp      = $00     ; Start of ZP addresses

        zpLOMEM = $50    ; Avoid 6510 registers

        FAULT  = $fd      ; Pointer to error block
        ESCFLG = $ff      ; Escape pending flag

        F_LOAD  = zp39    ; LOAD/SAVE control block
        F_EXEC  = F_LOAD+4
        F_START = F_LOAD+8
        F_END   = F_LOAD+12

        ; MOS Entry Points

        OS_CLI=$FFF7
        OSBYTE=$FFF4
        OSWORD=$FFF1
        OSWRCH=$FFEE
        OSWRCR=$FFEC
        OSNEWL=$FFE7
        OSASCI=$FFE3
        OSRDCH=$FFE0
        OSFILE=$FFDD
        OSARGS=$FFDA
        OSBGET=$FFD7
        OSBPUT=$FFD4
        OSGBPB=$FFD1
        OSFIND=$FFCE
        BRKV=$0316    ; Fixed
        WRCHV=0       ; Fixed
      
        ; Dummy variables for non-Atom code

        OSECHO = 0
        OSLOAD = 0
        OSSAVE = 0
        OSRDAR = 0
        OSSTAR = 0
        OSSHUT = 0
    .else
        .error "Please specify your build (i.e. -d:BUILD_BBC_BASIC2=1)"
    .endif

; ----------------------------------------------------------------------------

; Include ZP definitions of 00-4f, relative to 'zp', and Workspace Variables

    icl 'vars.s'

; ----------------------------------------------------------------------------

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

L8000:

; ----------------------------------------------------------------------------

; Atom/System Code Header
; =======================

.ifdef MOS_ATOM
    jsr LBFCF         ; Print inline text
    dta 'BBC BASIC II'
    .if version == 3
        dta 'I'
    .endif
    dta 13, '(C)198', [$30+version]
    .if foldup == 1
        dta ' ACORN'
    .else
        dta ' Acorn'
    .endif
    dta 13, 13
.endif

; BBC Code Header
; ===============

.ifdef MOS_BBC
    cmp #$01          ; Language Entry
    beq ENTRY
    rts
    nop
    dta $60           ; ROM type = Lang+Tube+6502 BASIC
    dta copyright_string - load       ; Offset to copyright string
    dta [version*2]-3     ; Version 2 = $01, Version 3 = $03
    dta 'BASIC'
copyright_string:
    dta 0
    dta '(C)198', [$30+version], ' Acorn', 10, 13, 0
    dta a(load), a(0)
.endif

; LANGUAGE STARTUP
; ================

.proc ENTRY
    .if memtop == 0
        lda #$84      ; Read top of memory
        jsr OSBYTE
    .elseif memtop > 0
        ldx #0
        ldy #>memtop
    .elseif memtop < 0
        ldx memtop+0
        ldy memtop+1
    .endif
    stx zpHIMEM
    sty zpHIMEM+1

    .if membot == 0
        lda #$83      ; Read bottom of memory
        jsr OSBYTE
    .elseif membot > 0
        ldy #>membot
    .elseif membot < 0
        ldy membot+1
    .endif
    sty zpTXTP

    ldx #$00
    stx zpLISTPO          ; Set LISTO to 0
    stx VARL_AT+2
    stx VARL_AT+3      ; Set @% to $0000xxxx
    dex
    stx zpWIDTHV          ; Set WIDTH to $FF

    ldx #$0A
    stx VARL_AT
    dex
    stx VARL_AT+1      ; Set @% to $0000090A

    lda #$01
    and zpSEED+4
    ora zpSEED          ; Check RND seed
    ora zpSEED+1
    ora zpSEED+2
    ora zpSEED+3
    bne RNDOK         ; If nonzero, skip past

    lda #'A'          ; Set RND seed to $575241
    sta zpSEED
    lda #'R'
    sta zpSEED+1
    lda #'W'
    sta zpSEED+2          ; "ARW" - Acorn Roger Wilson?

RNDOK:
    lda #<LB402
    sta BRKV+0        ; Set up error handler
    lda #>LB402
    sta BRKV+1
    cli
    jmp L8ADD         ; Enable IRQs, jump to immediate loop
.endp

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
    dta 'AND'     , $80, $00      ; 00000000
    dta 'ABS'     , $94, $00      ; 00000000
    dta 'ACS'     , $95, $00      ; 00000000
    dta 'ADVAL'   , $96, $00      ; 00000000
    dta 'ASC'     , $97, $00      ; 00000000
    dta 'ASN'     , $98, $00      ; 00000000
    dta 'ATN'     , $99, $00      ; 00000000
    dta 'AUTO'    , $C6, $10      ; 00010000
    dta 'BGET'    , $9A, $01      ; 00000001
    dta 'BPUT'    , $D5, $03      ; 00000011
    .if version == 2 || (version == 3 && minorversion == 10)
        dta 'COLOUR', $FB, $02    ; 00000010
    .elseif version == 3
        dta 'COLOR', $FB, $02     ; 00000010
    .endif
    dta 'CALL'    , $D6, $02      ; 00000010
    dta 'CHAIN'   , $D7, $02      ; 00000010
    dta 'CHR$'    , $BD, $00      ; 00000000
    dta 'CLEAR'   , $D8, $01      ; 00000001
    dta 'CLOSE'   , $D9, $03      ; 00000011
    dta 'CLG'     , $DA, $01      ; 00000001
    dta 'CLS'     , $DB, $01      ; 00000001
    dta 'COS'     , $9B, $00      ; 00000000
    dta 'COUNT'   , $9C, $01      ; 00000001
    .if version == 3 && minorversion != 10
        dta 'COLOUR', $FB, $02    ; 00000010
    .elseif version == 3 && minorversion == 10
        dta 'COLOR', $FB, $02     ; 00000010
    .endif
    dta 'DATA'    , $DC, $20      ; 00100000
    dta 'DEG'     , $9D, $00      ; 00000000
    dta 'DEF'     , $DD, $00      ; 00000000
    dta 'DELETE'  , $C7, $10      ; 00010000
    dta 'DIV'     , $81, $00      ; 00000000
    dta 'DIM'     , $DE, $02      ; 00000010
    dta 'DRAW'    , $DF, $02      ; 00000010
    dta 'ENDPROC' , $E1, $01      ; 00000001
    dta 'END'     , $E0, $01      ; 00000001
    dta 'ENVELOPE', $E2, $02      ; 00000010
    dta 'ELSE'    , $8B, $14      ; 00010100
    dta 'EVAL'    , $A0, $00      ; 00000000
    dta 'ERL'     , $9E, $01      ; 00000001
    dta 'ERROR'   , $85, $04      ; 00000100
    dta 'EOF'     , $C5, $01      ; 00000001
    dta 'EOR'     , $82, $00      ; 00000000
    dta 'ERR'     , $9F, $01      ; 00000001
    dta 'EXP'     , $A1, $00      ; 00000000
    dta 'EXT'     , $A2, $01      ; 00000001
    dta 'FOR'     , $E3, $02      ; 00000010
    dta 'FALSE'   , $A3, $01      ; 00000001
    dta 'FN'      , $A4, $08      ; 00001000
    dta 'GOTO'    , $E5, $12      ; 00010010
    dta 'GET$'    , $BE, $00      ; 00000000
    dta 'GET'     , $A5, $00      ; 00000000
    dta 'GOSUB'   , $E4, $12      ; 00010010
    dta 'GCOL'    , $E6, $02      ; 00000010
    dta 'HIMEM'   , $93, $43      ; 00100011
    dta 'INPUT'   , $E8, $02      ; 00000010
    dta 'IF'      , $E7, $02      ; 00000010
    dta 'INKEY$'  , $BF, $00      ; 00000000
    dta 'INKEY'   , $A6, $00      ; 00000000
    dta 'INT'     , $A8, $00      ; 00000000
    dta 'INSTR('  , $A7, $00      ; 00000000
    dta 'LIST'    , $C9, $10      ; 00010000
    dta 'LINE'    , $86, $00      ; 00000000
    dta 'LOAD'    , $C8, $02      ; 00000010
    dta 'LOMEM'   , $92, $43      ; 01000011
    dta 'LOCAL'   , $EA, $02      ; 00000010
    dta 'LEFT$('  , $C0, $00      ; 00000000
    dta 'LEN'     , $A9, $00      ; 00000000
    dta 'LET'     , $E9, $04      ; 00000100
    dta 'LOG'     , $AB, $00      ; 00000000
    dta 'LN'      , $AA, $00      ; 00000000
    dta 'MID$('   , $C1, $00      ; 00000000
    dta 'MODE'    , $EB, $02      ; 00000010
    dta 'MOD'     , $83, $00      ; 00000000
    dta 'MOVE'    , $EC, $02      ; 00000010
    dta 'NEXT'    , $ED, $02      ; 00000010
    dta 'NEW'     , $CA, $01      ; 00000001
    dta 'NOT'     , $AC, $00      ; 00000000
    dta 'OLD'     , $CB, $01      ; 00000001
    dta 'ON'      , $EE, $02      ; 00000010
    dta 'OFF'     , $87, $00      ; 00000000
    dta 'OR'      , $84, $00      ; 00000000
    dta 'OPENIN'  , $8E, $00      ; 00000000
    dta 'OPENOUT' , $AE, $00      ; 00000000
    dta 'OPENUP'  , $AD, $00      ; 00000000
    dta 'OSCLI'   , $FF, $02      ; 00000010
    dta 'PRINT'   , $F1, $02      ; 00000010
    dta 'PAGE'    , $90, $43      ; 01000011
    dta 'PTR'     , $8F, $43      ; 01000011
    dta 'PI'      , $AF, $01      ; 00000001
    dta 'PLOT'    , $F0, $02      ; 00000010
    dta 'POINT('  , $B0, $00      ; 00000000
    dta 'PROC'    , $F2, $0A      ; 00001010
    dta 'POS'     , $B1, $01      ; 00000001
    dta 'RETURN'  , $F8, $01      ; 00000001
    dta 'REPEAT'  , $F5, $00      ; 00000000
    dta 'REPORT'  , $F6, $01      ; 00000001
    dta 'READ'    , $F3, $02      ; 00000010
    dta 'REM'     , $F4, $20      ; 00100000
    dta 'RUN'     , $F9, $01      ; 00000001
    dta 'RAD'     , $B2, $00      ; 00000000
    dta 'RESTORE' , $F7, $12      ; 00010010
    dta 'RIGHT$(' , $C2, $00      ; 00000000
    dta 'RND'     , $B3, $01      ; 00000001
    dta 'RENUMBER', $CC, $10      ; 00010000
    dta 'STEP'    , $88, $00      ; 00000000
    dta 'SAVE'    , $CD, $02      ; 00000010
    dta 'SGN'     , $B4, $00      ; 00000000
    dta 'SIN'     , $B5, $00      ; 00000000
    dta 'SQR'     , $B6, $00      ; 00000000
    dta 'SPC'     , $89, $00      ; 00000000
    dta 'STR$'    , $C3, $00      ; 00000000
    dta 'STRING$(', $C4, $00      ; 00000000
    dta 'SOUND'   , $D4, $02      ; 00000010
    dta 'STOP'    , $FA, $01      ; 00000001
    dta 'TAN'     , $B7, $00      ; 00000000
    dta 'THEN'    , $8C, $14      ; 00010100
    dta 'TO'      , $B8, $00      ; 00000000
    dta 'TAB('    , $8A, $00      ; 00000000
    dta 'TRACE'   , $FC, $12      ; 00010010
    dta 'TIME'    , $91, $43      ; 01000011
    dta 'TRUE'    , $B9, $01      ; 00000001
    dta 'UNTIL'   , $FD, $02      ; 00000010
    dta 'USR'     , $BA, $00      ; 00000000
    dta 'VDU'     , $EF, $02      ; 00000010
    dta 'VAL'     , $BB, $00      ; 00000000
    dta 'VPOS'    , $BC, $01      ; 00000001
    dta 'WIDTH'   , $FE, $02      ; 00000010
    dta 'PAGE'    , $D0, $00      ; 00000000
    dta 'PTR'     , $CF, $00      ; 00000000
    dta 'TIME'    , $D1, $00      ; 00000000
    dta 'LOMEM'   , $D2, $00      ; 00000000
    dta 'HIMEM'   , $D3, $00      ; 00000000

; ----------------------------------------------------------------------------

; FUNCTION/COMMAND DISPATCH TABLE, MACRO
; ======================================

func_table .macro operator
    dta :1LBF78       ; $8E - OPENIN
    dta :1LBF47       ; $8F - PTR
    .if version < 3
        dta :1LAEC0       ; $90 - PAGE
        dta :1LAEB4       ; $91 - TIME
        dta :1LAEFC       ; $92 - LOMEM
        dta :1LAF03       ; $93 - HIMEM
    .elseif version >= 3
        dta :1XAEA7       ; $90 - PAGE
        dta :1LAEB4       ; $91 - TIME
        dta :1XAEFC       ; $92 - LOMEM
        dta :1XAF03       ; $93 - HIMEM
    .endif
    dta :1LAD6A       ; $94 - ABS
    dta :1LA8D4       ; $95 - ACS
    dta :1LAB33       ; $96 - ADVAL
    dta :1LAC9E       ; $97 - ASC
    dta :1LA8DA       ; $98 - ASN
    dta :1LA907       ; $99 - ATN
    dta :1LBF6F       ; $9A - BGET
    dta :1LA98D       ; $9B - COS
    .if version < 3
        dta :1LAEF7       ; $9C - COUNT
    .elseif version >= 3
        dta :1XAEF7       ; $9C - COUNT
    .endif
    dta :1LABC2       ; $9D - DEG
    .if version < 3
        dta :1LAF9F       ; $9E - ERL
        dta :1LAFA6       ; $9F - ERR
    .elseif version >= 3
        dta :1XAF9F       ; $9E - ERL
        dta :1XAFA6       ; $9F - ERR
    .endif
    dta :1LABE9       ; $A0 - EVAL
    dta :1LAA91       ; $A1 - EXP
    dta :1LBF46       ; $A2 - EXT
    .if version < 3
        dta :1LAECA       ; $A3 - FALSE
    .elseif version >= 3
        dta :1LACCD       ; $A3 - FALSE
    .endif
    dta :1LB195       ; $A4 - FN
    dta :1LAFB9       ; $A5 - GET
    dta :1LACAD       ; $A6 - INKEY
    dta :1LACE2       ; $A7 - INSTR(
    dta :1LAC78       ; $A8 - INT
    .if version < 3
        dta :1LAED1       ; $A9 - LEN
    .elseif version >= 3
        dta :1XAECC       ; $A9 - LEN
    .endif
    dta :1LA7FE       ; $AA - LN
    dta :1LABA8       ; $AB - LOG
    .if version < 3
        dta :1LACD1       ; $AC - NOT
    .elseif version >= 3
        dta :1XAB5B       ; $AC - NOT
    .endif
    dta :1LBF80       ; $AD - OPENUP
    dta :1LBF7C       ; $AE - OPENOUT
    dta :1LABCB       ; $AF - PI
    .if version < 3
        dta :1LAB41       ; $B0 - POINT(
    .elseif version >= 3
        dta :1XAB41       ; $B0 - POINT(
    .endif
    dta :1LAB6D       ; $B1 - POS
    dta :1LABB1       ; $B2 - RAD
    dta :1LAF49       ; $B3 - RND
    .if version < 3
        dta :1LAB88       ; $B4 - SGN
    .elseif version >= 3
        dta :1XACAA       ; $B4 - SGN
    .endif
    dta :1LA998       ; $B5 - SIN
    dta :1LA7B4       ; $B6 - SQR
    dta :1LA6BE       ; $B7 - TAN
    .if version < 3
        dta :1LAEDC       ; $B8 - TO
    .elseif version >= 3
        dta :1XAEA6       ; $B8 - TO
    .endif
    dta :1LACC4       ; $B9 - TRUE
    dta :1LABD2       ; $BA - USR
    dta :1LAC2F       ; $BB - VAL
    dta :1LAB76       ; $BC - VPOS
    dta :1LB3BD       ; $BD - CHR$
    dta :1LAFBF       ; $BE - GET$
    dta :1LB026       ; $BF - INKEY$
    dta :1LAFCC       ; $C0 - LEFT$(
    dta :1LB039       ; $C1 - MID$(
    dta :1LAFEE       ; $C2 - RIGHT$(
    dta :1LB094       ; $C3 - STR$(
    dta :1LB0C2       ; $C4 - STRING$(
    dta :1LACB8       ; $C5 - EOF
    dta :1L90AC       ; $C6 - AUTO
    dta :1L8F31       ; $C7 - DELETE
    dta :1LBF24       ; $C8 - LOAD
    dta :1LB59C       ; $C9 - LIST
    dta :1L8ADA       ; $CA - NEW
    dta :1L8AB6       ; $CB - OLD
    dta :1L8FA3       ; $CC - RENUMBER
    dta :1LBEF3       ; $CD - SAVE
    dta :1L982A       ; $CE - unused
    dta :1LBF30       ; $CF - PTR
    dta :1L9283       ; $D0 - PAGE
    dta :1L92C9       ; $D1 - TIME
    dta :1L926F       ; $D2 - LOMEM
    dta :1L925D       ; $D3 - HIMEM
    dta :1LB44C       ; $D4 - SOUND
    dta :1LBF58       ; $D5 - BPUT
    dta :1L8ED2       ; $D6 - CALL
    dta :1LBF2A       ; $D7 - CHAIN
    dta :1L928D       ; $D8 - CLEAR
    dta :1LBF99       ; $D9 - CLOSE
    dta :1L8EBD       ; $DA - CLG
    dta :1L8EC4       ; $DB - CLS
    dta :1L8B7D       ; $DC - DATA
    dta :1L8B7D       ; $DD - DEF
    dta :1L912F       ; $DE - DIM
    dta :1L93E8       ; $DF - DRAW
    dta :1L8AC8       ; $E0 - END
    dta :1L9356       ; $E1 - ENDPROC
    dta :1LB472       ; $E2 - ENVELOPE
    dta :1LB7C4       ; $E3 - FOR
    dta :1LB888       ; $E4 - GOSUB
    dta :1LB8CC       ; $E5 - GOTO
    dta :1L937A       ; $E6 - GCOL
    dta :1L98C2       ; $E7 - IF
    dta :1LBA44       ; $E8 - INPUT
    dta :1L8BE4       ; $E9 - LET
    dta :1L9323       ; $EA - LOCAL
    dta :1L939A       ; $EB - MODE
    dta :1L93E4       ; $EC - MOVE
    dta :1LB695       ; $ED - NEXT
    dta :1LB915       ; $EE - ON
    dta :1L942F       ; $EF - VDU
    dta :1L93F1       ; $F0 - PLOT
    dta :1L8D9A       ; $F1 - PRINT
    .if split == 0
        dta :1L9304       ; $F2 - PROC
    .elseif split != 0
        dta :1X9304       ; $F2 - PROC
    .endif 
    dta :1LBB1F       ; $F3 - READ
    dta :1L8B7D       ; $F4 - REM
    dta :1LBBE4       ; $F5 - REPEAT
    dta :1LBFE4       ; $F6 - REPORT
    dta :1LBAE6       ; $F7 - RESTORE
    dta :1LB8B6       ; $F8 - RETURN
    dta :1LBD11       ; $F9 - RUN
    dta :1L8AD0       ; $FA - STOP
    dta :1L938E       ; $FB - COLOUR
    dta :1L9295       ; $FC - TRACE
    dta :1LBBB1       ; $FD - UNTIL
    dta :1LB4A0       ; $FE - WIDTH
    dta :1LBEC2       ; $FF - OSCLI
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
    brk:clc:cld:cli:clv:dex:dey:inx
    iny:nop:pha:php:pla:plp:rti:rts
    sec:sed:sei:tax:tay:tsx:txa:txs:tya

; Branches
; --------
    dta $90, $B0, $F0, $30    ; BMI, BCC, BCS, BEQ
    dta $D0, $10, $50, $70    ; BNE, BPL, BVC, BVS

; Arithmetic
; ----------
    dta $21, $41, $01, $61    ; AND, EOR, ORA, ADC
    dta $C1, $A1, $E1, $06    ; CMP, LDA, SBC, ASL
    dta $46, $26, $66, $C6    ; LSR, ROL, ROR, DEC
    dta $E6, $E0, $C0, $20    ; INC, CPX, CPY, BIT

; Others
; ------
    dta $4C, $20, $A2, $A0    ; JMP, JSR, LDX, LDY
    dta $81, $86, $84         ; STA, STX, STY

; ----------------------------------------------------------------------------

; Exit Assembler
; --------------

L84FD:
    lda #$FF          ; Set OPT to 'BASIC'
L84FF:
    sta zpBYTESM
    jmp L8BA3         ; Set OPT, return to execution loop

L8504:
    lda #$03
    sta zpBYTESM          ; Set OPT 3, default on entry to '['
L8508
    jsr L8A97         ; Skip spaces
    cmp #']'
    beq L84FD         ; ']' - exit assembler
    jsr CLYADP

L8512:
    dec zpCURSOR
    jsr L85BA

    dec zpCURSOR
    lda zpBYTESM
    lsr
    bcc L857E

    lda zpTALLY
    adc #$04
    sta zp3F
    lda zp38
    jsr LB545

    lda zp37
    jsr LB562

    ldx #$FC
    ldy zp39
    bpl L8536

    ldy zp36
L8536:
    sty zp38
    beq L8556

    ldy #$00
L853C:
    inx
    bne L854C

    jsr LBC25         ; Print newline

    ldx zp3F

    .if version < 3
L8544:
        jsr LB565     ; Print a space
        dex
        bne L8544     ; Loop to print spaces
    .elseif version >= 3
        jsr LB580     ; Print multiple spaces
    .endif

    ldx #$FD
L854C:
    lda (zp3A),Y
    jsr LB562

    iny
    dec zp38
    bne L853C

L8556:
    .if version < 3
        inx
        bpl L8565
        jsr LB565
        jsr LB558
        jsr LB558
        jmp L8556
L8565:
        ldy #0
    .elseif version >= 3
        txa
        tay
X855C:
        iny
X855D:
        beq X8566
        ldx #3
        jsr LB580
        beq X855C
X8566:
        ldx #$0A
        lda (zpLINE),Y
        cmp #$2E
        bne X857D
X856E:
        jsr LB50E     ; Print char or token
        dex
        bne X8576
        ldx #1
X8576:
        iny
        lda (zpLINE),Y
        cpy zp4F
        bne X856E
X857D:
        jsr LB580
        dey
X8581:
        iny
        cmp (zpLINE),Y
        beq X8581
    .endif

L8567:
    lda (zpLINE),Y
    cmp #$3A
    beq L8577
    cmp #$0D
    beq L857B
L8571:
    jsr LB50E         ; Print character or token
    iny:BNE L8567
L8577
    cpy zpCURSOR
    bcc L8571
L857B:
    jsr LBC25         ; Print newline
L857E
    ldy zpCURSOR
    dey
L8581:
    iny
    lda (zpLINE),Y
    cmp #$3A
    beq L858C
    cmp #$0D
    bne L8581
L858C:
    jsr DONE_WITH_Y
    dey
    lda (zpLINE),Y
    cmp #$3A
    beq L85A2
    lda zpLINE+1
    cmp #$07+(ws/256)
    bne L859F
    jmp L8AF6

L859F:
    jsr LINO
L85A2:
    jmp L8508

L85A5:
    jsr L9582
    beq L8604
    bcs L8604
    jsr LBD94
    jsr LAE3A         ; Find P%
    sta zpTYPE
    jsr LB4B4
    jsr L8827

    .if version >= 3
        sty zp4F
    .endif

L85BA:
    ldx #$03          ; Prepare to fetch three characters
    jsr L8A97         ; Skip spaces
    ldy #$00
    sty zp3D
    cmp #':'
    beq L862B         ; End of statement
    cmp #$0D
    beq L862B         ; End of line
    cmp #'\'
    beq L862B         ; Comment
    cmp #'.'
    beq L85A5         ; Label
    dec zpCURSOR
L85D5:
    ldy zpCURSOR
    inc zpCURSOR          ; Get current character, inc. index
    lda (zpLINE),Y
    bmi L8607         ; Token, check for tokenised AND, EOR, OR
    cmp #$20
    beq L85F1         ; Space, step past
    ldy #$05
    asl
    asl
    asl               ; Compact first character
L85E6:
    asl
    rol zp3D
    rol zp3E
    dey
    bne L85E6
    dex
    bne L85D5         ; Loop to fetch three characters

; The current opcode has now been compressed into two bytes
; ---------------------------------------------------------

L85F1:
    ldx #$3A          ; Point to end of opcode lookup table
    lda zp3D          ; Get low byte of compacted mnemonic
L85F5:
    cmp L8451-1,X
    bne L8601         ; Low half doesn't match
    ldy L848B-1,X     ; Check high half
    cpy zp3E
    beq L8620         ; Mnemonic matches
L8601:
    dex
    bne L85F5         ; Loop through opcode lookup table
L8604:
    jmp L982A         ; Mnemonic not matched, Mistake

L8607:
    ldx #$22          ; opcode number for 'AND'
    cmp #tknAND
    beq L8620         ; Tokenised 'AND'
    inx               ; opcode number for 'EOR'
    cmp #tknEOR
    beq L8620         ; Tokenised 'EOR'
    inx               ; opcode number for 'ORA'
    cmp #tknOR
    bne L8604         ; Not tokenised 'OR'
    inc zpCURSOR
    iny
    lda (zpLINE),Y      ; Get next character
    cmp #'A'
    bne L8604         ; Ensure 'OR' followed by 'A'

; Opcode found
; ------------

L8620:
    lda L84C5-1,X
    sta zpOPCODE          ; Get base opcode
    ldy #$01          ; Y=1 for one byte
    cpx #$1A
    bcs L8673         ; Opcode $1A+ have arguments
L862B:
    lda ws+$0440
    sta zp37          ; Get P% low byte
    sty zp39
    ldx zpBYTESM
    cpx #$04          ; Offset assembly (opt>3)
    ldx ws+$0441
    stx zp38          ; Get P% high byte
    bcc L8643         ; No offset assembly
    lda ws+$043C
    ldx ws+$043D      ; Get O%
L8643:
    sta zp3A
    stx zp3B          ; Store destination pointer
    tya
    beq L8672
    bpl L8650
    ldy zp36
    beq L8672
L8650:
    dey
    lda zpOPCODE,Y        ; Get opcode byte   (lda abs,y (!))
    bit zp39
    bpl L865B         ; Opcode - jump to store it
    lda ws+$0600,Y    ; Get EQU byte
L865B:
    sta (zp3A),Y      ; Store byte
    inc ws+$0440
    bne L8665         ; Increment P%
    inc ws+$0441
L8665:
    bcc L866F
    inc ws+$043C
    bne L866F         ; Increment O%
    inc ws+$043D
L866F:
    tya
    bne L8650
L8672:
    rts

L8673:
    cpx #$22
    bcs L86B7
    jsr L8821
    clc
    lda zpIACC
    sbc ws+$0440
    tay
    lda zpIACC+1
    sbc ws+$0441
    cpy #$01
    dey
    sbc #$00
    beq L86B2
    cmp #$FF
    beq L86AD

L8691:
    lda zpBYTESM
    .if version < 3
        lsr
    .elseif version >= 3
        and #$02
    .endif
    beq L86A5

    brk
    .if foldup == 1
        dta 1, 'OUT OF RANGE'
    .else
        dta 1, 'Out of range'
    .endif
    brk

L86A5:
    tay
L86A6:
    sty zpIACC
L86A8:
    ldy #$02
    jmp L862B

L86AD:
    tya
    bmi L86A6
    bpl L8691

L86B2:
    tya
    bpl L86A6
    bmi L8691

L86B7:
    cpx #$29
    bcs L86D3
    jsr L8A97         ; Skip spaces
    cmp #'#'
    bne L86DA
    jsr L882F
L86C5:
    jsr L8821
L86C8:
    lda zpIACC+1
    beq L86A8
L86CC:
    brk
    dta $02
    .if foldup == 1
        dta 'BYTE'
    .else
        dta 'Byte'
    .endif
    brk

; Parse (zp),Y addressing mode
; ----------------------------
L86D3:
    cpx #$36
    bne L873F
    jsr L8A97         ; Skip spaces
L86DA:
    cmp #'('
    bne L8715
    jsr L8821
    jsr L8A97         ; Skip spaces
    cmp #')'
    bne L86FB
    jsr L8A97         ; Skip spaces
    cmp #','
    bne L870D         ; No comma, jump to Index error
    jsr L882C
    jsr L8A97         ; Skip spaces
    cmp #'Y'
    bne L870D         ; (zp),Y missing Y, jump to Index error
    beq L86C8

; Parse (zp,X) addressing mode
; ----------------------------
L86FB:
    cmp #','
    bne L870D         ; No comma, jump to Index error
    jsr L8A97         ; Skip spaces
    cmp #'X'
    bne L870D         ; zp,X missing X, jump to Index error
    jsr L8A97         ; Skip spaces
    cmp #')'
    beq L86C8         ; zp,X) - jump to process
L870D:
    brk
    dta $03
    .if foldup == 1
        dta 'INDEX'
    .else
        dta 'Index'
    .endif
    brk

L8715:
    dec zpCURSOR
    jsr L8821
    jsr L8A97         ; Skip spaces
    cmp #','
    bne L8735         ; No comma - jump to process as abs,X
    jsr L882C
    jsr L8A97         ; Skip spaces
    cmp #'X'
    beq L8735         ; abs,X - jump to process
    cmp #'Y'
    bne L870D         ; Not abs,Y - jump to Index error
L872F:
    jsr L882F
    jmp L879A

; abs and abs,X
; -------------
L8735:
    jsr L8832
L8738:
    lda zpIACC+1
    bne L872F
    jmp L86A8

L873F:
    cpx #$2F
    bcs L876E
    cpx #$2D
    bcs L8750
    jsr L8A97         ; Skip spaces
    cmp #'A'
    beq L8767         ; ins A -
    dec zpCURSOR
L8750:
    jsr L8821
    jsr L8A97         ; Skip spaces
    cmp #','
    bne L8738         ; No comma, jump to ...
    jsr L882C
    jsr L8A97         ; Skip spaces
    cmp #'X'
    beq L8738         ; Jump with address,X
    jmp L870D         ; Otherwise, jump to Index error

L8767:
    jsr L8832
    ldy #$01
    bne L879C

L876E:
    cpx #$32
    bcs L8788
    cpx #$31
    beq L8782
    jsr L8A97         ; Skip spaces
    cmp #'#'
    bne L8780         ; Not #, jump with address
    jmp L86C5         ; Jump with immediate

L8780:
    dec zpCURSOR
L8782:
    jsr L8821
    jmp L8735

L8788:
    cpx #$33
    beq L8797
    bcs L87B2
    jsr L8A97         ; Skip spaces
    cmp #'('
    beq L879F         ; Jump with (... addressing mode
    dec zpCURSOR
L8797:
    jsr L8821
L879A:
    ldy #$03
L879C:
    jmp L862B

L879F:
    jsr L882C
    jsr L882C
    jsr L8821
    jsr L8A97         ; Skip spaces
    cmp #')'
    beq L879A
    jmp L870D         ; No ) - jump to Index error

L87B2:
    cpx #$39
    bcs L8813
    lda zp3D
    eor #$01
    and #$1F
    pha
    cpx #$37
    bcs L87F0
    jsr L8A97         ; Skip spaces
    cmp #'#'
    bne L87CC
    pla
    jmp L86C5

L87CC:
    dec zpCURSOR
    jsr L8821
    pla
    sta zp37
    jsr L8A97         ; Skip spaces
    cmp #','
    beq L87DE
    jmp L8735

L87DE:
    jsr L8A97         ; Skip spaces
    and #$1F
    cmp zp37
    bne L87ED
    jsr L882C
    jmp L8735

L87ED:
    jmp L870D         ; Jump to Index error

L87F0:
    jsr L8821
    pla
    sta zp37
    jsr L8A97         ; Skip spaces
    cmp #','
    bne L8810
    jsr L8A97         ; Skip spaces
    and #$1F
    cmp zp37
    bne L87ED
    jsr L882C
    lda zpIACC+1
    beq L8810         ; High byte=0, continue
    jmp L86CC         ; value>255, jump to Byte error

L8810:
    jmp L8738

L8813:
    bne L883A
    jsr L8821
    lda zpIACC
    sta zpBYTESM
    ldy #$00
    jmp L862B

L8821:
    jsr L9B1D
    jsr L92F0
L8827:
    ldy zpAECUR
    sty zpCURSOR
    rts

L882C:
    jsr L882F
L882F:
    jsr L8832
L8832:
    lda zpOPCODE
    clc
    adc #$04
    sta zpOPCODE
    rts

L883A:
    ldx #$01          ; Prepare for one byte
    ldy zpCURSOR
    inc zpCURSOR          ; Increment index
    lda (zpLINE),Y      ; Get next character
    cmp #'B'
    beq L8858         ; EQUB
    inx               ; Prepare for two bytes
    cmp #'W'
    beq L8858         ; EQUW
    ldx #$04          ; Prepare for four bytes
    cmp #'D'
    beq L8858         ; EQUD
    cmp #'S'
    beq L886A         ; EQUS
    jmp L982A         ; Syntax error

L8858:
    txa
    pha
    jsr L8821
    ldx #$29
    jsr LBE44
    pla
    tay
L8864:
    jmp L862B

L8867:
    jmp L8C0E

L886A:
    lda zpBYTESM
    pha
    jsr L9B1D
    bne L8867
    pla
    sta zpBYTESM
    jsr L8827
    ldy #$FF
    bne L8864

L887C:
    pha
    clc
    tya
    adc zp37
    sta zp39
    ldy #$00
    tya
    adc zp38
    sta zp3A
    pla
    sta (zp37),Y
L888D:
    iny
    lda (zp39),Y
    sta (zp37),Y
    cmp #$0D
    bne L888D
    rts

L8897:
    and #$0F
    sta zp3D
    sty zp3E
L889D:
    iny
    lda (zp37),Y
    .if version < 3
        cmp #'9'+1
        bcs L88DA
        cmp #'0'
    .elseif version >= 3
        jsr L8936
    .endif
    bcc L88DA
    and #$0F
    pha
    ldx zp3E
    lda zp3D
    asl
    rol zp3E
    bmi L88D5
    asl
    rol zp3E
    bmi L88D5
    adc zp3D
    sta zp3D
    txa
    adc zp3E
    asl zp3D
    rol
    bmi L88D5
    bcs L88D5
    sta zp3E
    pla
    adc zp3D
    sta zp3D
    bcc L889D
    inc zp3E
    bpl L889D
    pha
L88D5:
    pla
    ldy #$00
    sec
    rts

L88DA:
    dey
    lda #$8D
    jsr L887C
    lda zp37
    adc #$02
    sta zp39
    lda zp38
    adc #$00
    sta zp3A
L88EC:
    lda (zp37),Y
    sta (zp39),Y
    dey
    bne L88EC
    ldy #$03
L88F5:
    lda zp3E
    ora #$40
    sta (zp37),Y
    dey
    lda zp3D
    and #$3F
    ora #$40
    sta (zp37),Y
    dey
    lda zp3D
    and #$C0
    sta zp3D
    lda zp3E
    and #$C0
    lsr
    lsr
    ora zp3D
    lsr
    lsr
    eor #$54
    sta (zp37),Y
    jsr L8944         ; Increment $37/8
    jsr L8944         ; Increment $37/8
    jsr L8944         ; Increment $37/8
    ldy #$00
L8924:
    clc
    rts

L8926:
    cmp #$7B
    bcs L8924
    cmp #$5F
    bcs L893C
    cmp #$5B
    bcs L8924
    cmp #$41
    bcs L893C
L8936:
    cmp #$3A
    bcs L8924
    cmp #$30
L893C:
    rts

L893D:
    cmp #$2E
    bne L8936
    rts

L8942:
    lda (zp37),Y
L8944:
    inc zp37
    bne L894A
    inc zp38
L894A:
    rts

L894B:
    jsr L8944         ; Increment $37/8
    lda (zp37),Y
    rts

; Tokenise line at $37/8
; ======================
L8951:
    ldy #$00
    sty zp3B          ; Set tokeniser to left-hand-side
L8955:
    sty zp3C
L8957:
    lda (zp37),Y      ; Get current character
    cmp #$0D
    beq L894A         ; Exit with <cr>
    cmp #$20
    bne L8966         ; Skip <spc>
L8961:
    jsr L8944
    bne L8957         ; Increment $37/8 and check next character

L8966:
    cmp #'&'
    bne L897C         ; Jump if not '&'
L896A:
    jsr L894B         ; Increment $37/8 and get next character
    jsr L8936
    bcs L896A         ; Jump if numeric character
    cmp #'A'
    bcc L8957         ; Loop back if <'A'
    cmp #'F'+1
    bcc L896A         ; Step to next if 'A'..'F'
    bcs L8957         ; Loop back for next character
L897C:
    cmp #$22
    bne L898C
L8980:
    jsr L894B         ; Increment $37/8 and get next character
    cmp #$22
    beq L8961         ; Not quote, jump to process next character
    cmp #$0D
    bne L8980
    rts

L898C:
    cmp #':'
    bne L8996
    sty zp3B
    sty zp3C
    beq L8961
L8996:
    cmp #','
    beq L8961
    cmp #'*'
    bne L89A3
    lda zp3B
    bne L89E3
    rts

L89A3:
    cmp #'.'
    beq L89B5
    jsr L8936
    bcc L89DF
    ldx zp3C
    beq L89B5
    jsr L8897
    bcc L89E9
L89B5:
    lda (zp37),Y
    jsr L893D
    bcc L89C2
    jsr L8944
    jmp L89B5

L89C2:
    ldx #$FF
    stx zp3B
    sty zp3C
    jmp L8957

L89CB:
    jsr L8926
    bcc L89E3
L89D0:
    ldy #$00
L89D2:
    lda (zp37),Y
    jsr L8926
    bcc L89C2
    jsr L8944
    jmp L89D2

L89DF:
    cmp #'A'
    bcs L89EC         ; Jump if letter
L89E3:
    ldx #$FF
    stx zp3B
    sty zp3C
L89E9:
    jmp L8961

L89EC:
    cmp #'X'
    bcs L89CB         ; Jump if >='X', nothing starts with X,Y,Z
    ldx #<L8071
    stx zp39          ; Point to token table
    ldx #>L8071
    stx zp3A
L89F8:
    cmp (zp39),Y
    bcc L89D2
    bne L8A0D
L89FE:
    iny
    lda (zp39),Y
    bmi L8A37
    cmp (zp37),Y
    beq L89FE
    lda (zp37),Y
    cmp #'.'
    beq L8A18
L8A0D:
    iny
    lda (zp39),Y
    bpl L8A0D
    cmp #$FE
    bne L8A25
    bcs L89D0
L8A18:
    iny
L8A19:
    lda (zp39),Y
    bmi L8A37
    inc zp39
    bne L8A19
    inc zp3A
    bne L8A19
L8A25:
    sec
    iny
    tya
    adc zp39
    sta zp39
    bcc L8A30
    inc zp3A
L8A30:
    ldy #$00
    lda (zp37),Y
    jmp L89F8

L8A37:
    tax
    iny
    lda (zp39),Y
    sta zp3D          ; Get token flag
    dey
    lsr
    bcc L8A48
    lda (zp37),Y
    jsr L8926
    bcs L89D0
L8A48:
    txa
    bit zp3D
    bvc L8A54
    ldx zp3B
    bne L8A54
    .if split == 0
        clc           ; Superflous as all paths to here have CLC
    .endif
    adc #$40
L8A54:
    dey
    jsr L887C
    ldy #$00
    ldx #$FF
    lda zp3D
    lsr
    lsr
    bcc L8A66
    stx zp3B
    sty zp3C
L8A66:
    lsr
    bcc L8A6D
    sty zp3B
    sty zp3C
L8A6D:
    lsr
    bcc L8A81
    pha
    iny
L8A72:
    lda (zp37),Y
    jsr L8926
    bcc L8A7F
    jsr L8944
    jmp L8A72

L8A7F:
    dey
    pla
L8A81:
    lsr
    bcc L8A86
    stx zp3C
L8A86:
    lsr
    bcs L8A96
    jmp L8961

; Skip Spaces
; ===========
L8A8C:
    ldy zpAECUR
    inc zpAECUR          ; Get offset, increment it
    lda (zpAELINE),Y      ; Get current character
    cmp #' '
    beq L8A8C         ; Loop until not space
L8A96:
    rts

; Skip spaces at PtrA
; -------------------
L8A97:
    ldy zpCURSOR
    inc zpCURSOR
    lda (zpLINE),Y
    cmp #$20
    beq L8A97
L8AA1:
    rts

    .if version < 3
L8AA2:
        brk
        dta 5
        .if foldup == 1
            dta 'MISSING ,'
        .else
            dta 'Missing ,'
        .endif
        brk
    .endif

L8AAE:
    jsr L8A8C
    cmp #','
    .if version < 3
        bne L8AA2
        rts
    .elseif version >= 3
        beq L8AA1
X8AC8:
        brk
        dta 5
        .if foldup == 1
            dta 'MISSING ,'
        .else
            dta 'Missing ,'
        .endif
        brk
    .endif


; OLD - Attempt to restore program
; ================================
L8AB6:
    jsr DONE         ; Check end of statement
    lda zpTXTP
    sta zp38          ; Point $37/8 to PAGE
    lda #$00
    sta zp37
    sta (zp37),Y      ; Remove end marker
    jsr LBE6F         ; Check program and set TOP
    bne L8AF3         ; Jump to clear heap and go to immediate mode

; END - Return to immediate mode
; ==============================
L8AC8:
    jsr DONE         ; Check end of statement
    jsr LBE6F         ; Check program and set TOP
    bne L8AF6         ; Jump to immediate mode, keeping variables, etc

; STOP - Abort program with an error
; ==================================
L8AD0:
    jsr DONE         ; Check end of statement
    .if version < 3 && foldup == 0
        brk
        dta 0
        dta 'STOP'
        brk
    .endif
    .if version >= 3 || foldup != 0
        brk
        dta 0
        dta tknSTOP
        brk
    .endif

; NEW - Clear program, enter immediate mode
; =========================================
L8ADA:
    jsr DONE         ; Check end of statement
    .if title != 0
        jsr X8ADD     ; NEW program
    .endif

; Start up with NEW program
; -------------------------
L8ADD:  ; FORMAT?
    .if title == 0
        lda #$0D
        ldy zpTXTP
        sty zpTOP+1      ; TOP hi=PAGE hi
        ldy #$00
        sty zpTOP
        sty zpTRFLAG      ; TOP=PAGE, TRACE OFF
        sta (zpTOP),Y  ; ?(PAGE+0)=<cr>
        lda #$FF
        iny
        sta (zpTOP),Y  ; ?(PAGE+1)=$FF
        iny
        sty zpTOP      ; TOP=PAGE+2
    .endif

L8AF3:
    jsr LBD20         ; Clear variables, heap, stack

; IMMEDIATE LOOP
; ==============
L8AF6:
    ldy #$07+(ws/256)
    sty zpLINE+1          ; PtrA=$0700 - input buffer
    ldy #$00
    sty zpLINE
    lda #<LB433
    sta zpERRORLH          ; ON ERROR OFF
    lda #>LB433
    sta zpERRORLH+1
    lda #'>'
    jsr LBC02         ; Print '>' prompt, read input to buffer at PtrA

; Execute line at program pointer in $0B/C
; ----------------------------------------
L8B0B:
    lda #<LB433
    sta zpERRORLH          ; ON ERROR OFF again
    lda #>LB433
    sta zpERRORLH+1
    ldx #$FF
    stx zpBYTESM          ; OPT=$FF - not within assembler
    stx zp3C
    txs               ; Clear machine stack
    jsr LBD3A
    tay               ; Clear DATA and stacks
    lda zpLINE
    sta zp37          ; Point $37/8 to program line
    lda zpLINE+1
    sta zp38
    sty zp3B
    sty zpCURSOR
    jsr L8957
    jsr L97DF
    bcc L8B38         ; Tokenise, jump forward if no line number
    jsr LBC8D
    jmp L8AF3         ; Insert into program, jump back to immediate loop

; Command entered at immediate prompt
; -----------------------------------
L8B38:
    jsr L8A97         ; Skip spaces at PtrA
    cmp #$C6
    bcs L8BB1         ; If command token, jump to execute command
    bcc L8BBF         ; Not command token, try variable assignment

L8B41:
    jmp L8AF6         ; Jump back to immediate mode

; [ - enter assembler
; ===================
L8B44:
    jmp L8504         ; Jump to assembler

; =<value> - return from FN
; =========================
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknFN
L8B47:
    tsx
    cpx #$FC
    bcs L8B59         ; If stack is empty, jump to give error
    lda $01FF
    cmp #tknFN
    bne L8B59; If pushed token<>'FN', give error
    jsr L9B1D         ; Evaluate expression
    jmp L984C         ; Check for end of statement and return to pop from function
L8B59:
    brk
    dta 7
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknFN
    brk

; Check for =, *, [ commands
; ==========================
L8B60:
    ldy zpCURSOR
    dey
    lda (zpLINE),Y      ; Step program pointer back and fetch char
    cmp #'='
    beq L8B47         ; Jump for '=', return from FN
    cmp #'*'
    beq L8B73         ; Jump for '*', embedded *command
    cmp #'['
    beq L8B44         ; Jump for '[', start assembler
    bne L8B96         ; Otherwise, see if end of statement

; Embedded *command
; =================
L8B73:
    jsr CLYADP         ; Update PtrA to current address
    ldx zpLINE
    ldy zpLINE+1          ; XY=>command string


    .ifdef MOS_ATOM
        jsr cmdStar       ; Pass command at ptrA to Atom OSCLI
    .endif

    .ifdef MOS_BBC
        jsr OS_CLI    ; Pass command at ptrA to OSCLI
    .endif

; DATA, DEF, REM, ELSE
; ====================
; Skip to end of line
; -------------------
L8B7D:
    lda #$0D
    ldy zpCURSOR
    dey               ; Get program pointer
L8B82:
    iny
    cmp (zpLINE),Y
    bne L8B82         ; Loop until <cr> found
L8B87:
    cmp #tknELSE
    beq L8B7D         ; If 'ELSE', jump to skip to end of line
    lda zpLINE+1
    cmp #(ws+$0700)/256
    beq L8B41; Program in command buffer, jump back to immediate loop
    jsr LINO
    bne L8BA3         ; Check for end of program, step past <cr>

L8B96:
    dec zpCURSOR
L8B98:
    jsr DONE

; Main execution loop
; -------------------
L8B9B:
    ldy #$00
    lda (zpLINE),Y      ; Get current character
    cmp #':'
    bne L8B87         ; Not <colon>, check for ELSE

L8BA3:
    ldy zpCURSOR
    inc zpCURSOR          ; Get program pointer, increment for next time
    lda (zpLINE),Y      ; Get current character
    cmp #$20
    beq L8BA3         ; Skip spaces
    cmp #$CF
    bcc L8BBF         ; Not program command, jump to try variable assignment

; Dispatch function/command
; -------------------------
L8BB1:
    tax               ; Index into dispatch table
    lda L836D-$8E,X
    sta zp37          ; Get routine address from table
    lda L83DF-$8E,X
    sta zp38
    jmp (zp37)        ; Jump to routine

; Not a command byte, try variable assignment, or =, *, [
; -------------------------------------------------------
L8BBF:
    ldx zpLINE
    stx zpAELINE          ; Copy PtrA to PtrB
    ldx zpLINE+1
    stx zpAELINE+1
    sty zpAECUR
    jsr L95DD         ; Check if variable or indirection
    bne L8BE9         ; NE - jump for existing variable or indirection assignment
    bcs L8B60         ; CS - not variable assignment, try =, *, [ commands

; Variable not found, create a new one
; ------------------------------------
    stx zpAECUR
    jsr L9841         ; Check for and step past '='
    jsr L94FC         ; Create new variable
    ldx #$05          ; X=$05 = float
    cpx zpIACC+2
    bne L8BDF         ; Jump if dest. not a float
    inx               ; X=$06
L8BDF:
    jsr L9531
    dec zpCURSOR

; LET variable = expression
; =========================
L8BE4:
    jsr L9582
    beq L8C0B
L8BE9:
    bcc L8BFB
    jsr LBD94         ; Stack integer (address of data)
    jsr L9813         ; Check for end of statement
    lda zpTYPE          ; Get evaluation type
    bne L8C0E         ; If not string, error
    jsr L8C1E         ; Assign the string
    jmp L8B9B         ; Return to execution loop

L8BFB:
    jsr LBD94         ; Stack integer (address of data)
    jsr L9813         ; Check for end of statement
    lda zpTYPE          ; Get evaluation type
    beq L8C0E         ; If not number, error
    jsr LB4B4         ; Assign the number
    jmp L8B9B         ; Return to execution loop

L8C0B:
    jmp L982A

L8C0E:
    brk
    dta 6
    .if foldup == 1
        dta 'TYPE MISMATCH'
    .else
        dta 'Type mismatch'
    .endif
    brk

L8C1E:
    jsr LBDEA         ; Unstack integer (address of data)
L8C21:
    lda zpIACC+2
    cmp #$80
    beq L8CA2         ; Jump if absolute string $addr
    ldy #$02
    lda (zpIACC),Y
    cmp zp36
    bcs L8C84
    lda zpFSA
    sta zpIACC+2
    lda zpFSA+1
    sta zpIACC+3
    lda zp36
    cmp #$08
    bcc L8C43
    adc #$07
    bcc L8C43
    lda #$FF
L8C43:
    clc
    pha
    tax
    lda (zpIACC),Y
    ldy #$00
    adc (zpIACC),Y
    eor zpFSA
    bne L8C5F
    iny
    adc (zpIACC),Y
    eor zpFSA+1
    bne L8C5F
    sta zpIACC+3
    txa
    iny
    sec
    sbc (zpIACC),Y
    tax
L8C5F:
    txa
    clc
    adc zpFSA
    tay
    lda zpFSA+1
    adc #$00
    cpy zpAESTKP
    tax
    sbc zpAESTKP+1
    bcs L8CB7
    sty zpFSA
    stx zpFSA+1
    pla
    ldy #$02
    sta (zpIACC),Y
    dey
    lda zpIACC+3
    beq L8C84
    sta (zpIACC),Y
    dey
    lda zpIACC+2
    sta (zpIACC),Y
L8C84:
    ldy #$03
    lda zp36
    sta (zpIACC),Y
    beq L8CA1
    dey
    dey
    lda (zpIACC),Y
    sta zpIACC+3
    dey
    lda (zpIACC),Y
    sta zpIACC+2
L8C97:
    lda ws+$0600,Y
    sta (zpIACC+2),Y
    iny
    cpy zp36
    bne L8C97
L8CA1:
    rts

L8CA2:
    jsr LBEBA
    cpy #$00
    beq L8CB4
L8CA9:
    lda ws+$0600,Y
    sta (zpIACC),Y
    dey
    bne L8CA9
    lda ws+$0600
L8CB4:
    sta (zpIACC),Y
    rts

L8CB7:
    brk
    dta 0
    .if foldup == 1
        dta 'NO ROOM'
    .else
        dta 'No room'
    .endif
    brk

L8CC1:
    lda zp39
    cmp #$80
    beq L8CEE
    bcc L8D03
    ldy #$00
    lda (zpAESTKP),Y
    tax
    beq L8CE5
    lda (zp37),Y
    sbc #$01
    sta zp39
    iny
    lda (zp37),Y
    sbc #$00
    sta zp3A
L8CDD:
    lda (zpAESTKP),Y
    sta (zp39),Y
    iny
    dex
    bne L8CDD
L8CE5:
    lda (zpAESTKP,X)
    ldy #$03
L8CE9:
    sta (zp37),Y
    jmp LBDDC

L8CEE:
    ldy #$00
    lda (zpAESTKP),Y
    tax
    beq L8CFF
L8CF5:
    iny
    lda (zpAESTKP),Y
    dey
    sta (zp37),Y
    iny
    dex
    bne L8CF5
L8CFF:
    lda #$0D
    bne L8CE9
L8D03:
    ldy #$00
    lda (zpAESTKP),Y
    sta (zp37),Y
    .if version < 3
        iny
        cpy zp39
        bcs L8D26
    .elseif version >= 3
        ldy #4
        lda zp39
        beq L8D26
        ldy #$01
    .endif
    lda (zpAESTKP),Y
    sta (zp37),Y
    iny
    lda (zpAESTKP),Y
    sta (zp37),Y
    iny
    lda (zpAESTKP),Y
    sta (zp37),Y
    iny
    cpy zp39
    bcs L8D26
    lda (zpAESTKP),Y
    sta (zp37),Y
    iny
L8D26:
    tya
    clc
    jmp LBDE1

; PRINT#
L8D2B:
    dec zpCURSOR
    jsr LBFA9
L8D30:
    tya
    pha
    jsr L8A8C
    cmp #$2C
    bne L8D77
    jsr L9B29
    jsr LA385
    pla
    tay
    lda zpTYPE
    jsr OSBPUT
    tax
    beq L8D64
    bmi L8D57
    ldx #$03
L8D4D:
    lda zpIACC,X
    jsr OSBPUT
    dex
    bpl L8D4D
    bmi L8D30
L8D57:
    ldx #$04
L8D59:
    lda ws+$046C,X
    jsr OSBPUT
    dex
    bpl L8D59
    bmi L8D30
L8D64:
    lda zp36
    jsr OSBPUT
    tax
    beq L8D30
L8D6C:
    lda ws+$05FF,X
    jsr OSBPUT
    dex
    bne L8D6C
    beq L8D30
L8D77:
    pla
    sty zpCURSOR
    jmp L8B98

; End of PRINT statement
; ----------------------
L8D7D:
    jsr LBC25         ; Output new line and set COUNT to zero
L8D80:
    jmp L8B96         ; Check end of statement, return to execution loop

L8D83:
    lda #$00
    sta zpPRINTS
    sta zpPRINTF          ; Set current field to zero, hex/dec flag to decimal
    jsr L8A97         ; Get next non-space character
    cmp #':'
    beq L8D80         ; <colon> found, finish printing
    cmp #$0D
    beq L8D80         ; <cr> found, finish printing
    cmp #tknELSE
    beq L8D80         ; 'ELSE' found, finish printing
    bne L8DD2         ; Otherwise, continue into main loop

; PRINT [~][print items]['][,][;]
; ===============================
L8D9A:
    jsr L8A97         ; Get next non-space char
    cmp #'#'
    beq L8D2B         ; If '#' jump to do PRINT#
    dec zpCURSOR
    jmp L8DBB         ; Jump into PRINT loop

; Print a comma
; -------------
L8DA6:
    lda VARL_AT
    beq L8DBB         ; If field width zero, no padding needed, jump back into main loop
    lda zpTALLY          ; Get COUNT
L8DAD:
    beq L8DBB         ; Zero, just started a new line, no padding, jump back into main loop
    sbc VARL_AT      ; Get COUNT-field width
    bcs L8DAD         ; Loop to reduce until (COUNT MOD fieldwidth)<0
    tay               ; Y=number of spaces to get back to (COUNT MOD width)=zero
L8DB5:
    jsr LB565
    iny
    bne L8DB5         ; Loop to print required spaces

L8DBB:
    clc               ; Prepare to print decimal
    lda VARL_AT
    sta zpPRINTS          ; Set current field width from @%
L8DC1:
    ror zpPRINTF          ; Set hex/dec flag from Carry
L8DC3:
    jsr L8A97         ; Get next non-space character
    cmp #':'
    beq L8D7D         ; End of statement if <colon> found
    cmp #$0D
    beq L8D7D         ; End if statement if <cr> found
    cmp #tknELSE
    beq L8D7D         ; End of statement if 'ELSE' found

L8DD2:
    cmp #'~'
    beq L8DC1         ; Jump back to set hex/dec flag from Carry
    cmp #','
    beq L8DA6         ; Jump to pad to next print field
    cmp #';'
    beq L8D83         ; Jump to check for end of print statement
    jsr L8E70
    bcc L8DC3         ; Check for ' TAB SPC, if print token found return to outer main loop

; All print formatting have been checked, so it now must be an expression
; -----------------------------------------------------------------------
    lda zpPRINTS
    pha
    lda zpPRINTF
    pha               ; Save field width and flags, as evaluator
                      ;  may call PRINT (eg FN, STR$, etc.)
    dec zpAECUR
    jsr L9B29         ; Evaluate expression
    pla
    sta zpPRINTF
    pla
    sta zpPRINTS          ; Restore field width and flags
    lda zpAECUR
    sta zpCURSOR          ; Update program pointer
    tya
    beq L8E0E         ; If type=0, jump to print string
    jsr FCON         ; Convert numeric value to string
    lda zpPRINTS          ; Get current field width
    sec
    sbc zp36          ; A=width-stringlength
    bcc L8E0E         ; length>width - print it
    beq L8E0E         ; length=width - print it
    tay               ; Otherwise, Y=number of spaces to pad with
L8E08:
    jsr LB565
    dey
    bne L8E08         ; Loop to print required spaces to pad the number

; Print string in string buffer
; -----------------------------
L8E0E:
    lda zp36
    beq L8DC3         ; Null string, jump back to main loop
    ldy #$00          ; Point to start of string
L8E14:
    lda ws+$0600,Y
    jsr LB558         ; Print character from string buffer
    iny
    cpy zp36
    bne L8E14         ; Increment pointer, loop for full string
    beq L8DC3         ; Jump back for next print item

L8E21:
    .if version < 3
        jmp L8AA2
    .elseif version >= 3
        jmp X8AC8
    .endif

L8E24:
    cmp #','
    bne L8E21         ; No comma, jump to TAB(x)
    lda zpIACC
    pha               ; Save X
    jsr LAE56
    jsr L92F0

; Atom - manually position cursor
; -------------------------------
    .ifdef MOS_ATOM
        lda #$1E
        jsr OSWRCH    ; Home cursor
        ldy zpIACC
        beq XADDC     ; Y=0, no movement needed
        lda #$0A
XADD6:
        jsr OSWRCH    ; Move cursor down
        dey
        bne XADD6     ; Loop until Y position reached
XADDC:
        pla
        beq XADE8     ; X=0, no movement needed
        tay
        lda #$09
XADE2:
        jsr OSWRCH    ; Move cursor right
        dey
        bne XADE2     ; Loop until X position reached
XADE8:
    .endif

; BBC - send VDU 31,x,y sequence
; ------------------------------
    .ifdef MOS_BBC
        lda #$1F
        jsr OSWRCH    ; TAB()
        pla
        jsr OSWRCH    ; X coord
        jsr L9456     ; Y coord
    .endif

    jmp L8E6A         ; Continue to next PRINT item

L8E40:
    jsr L92DD
    jsr L8A8C
    cmp #')'
    bne L8E24
    lda zpIACC
    sbc zpTALLY
    beq L8E6A
    .if version < 3
        tay
    .elseif version >= 3
        tax
    .endif
    bcs L8E5F
    jsr LBC25
    beq L8E5B
L8E58:
    jsr L92E3
L8E5B:
    .if version < 3
        ldy zpIACC
    .elseif version >= 3
        ldx zpIACC
    .endif
    beq L8E6A
L8E5F:
    .if version < 3
        jsr LB565
        dey
        bne L8E5F
    .elseif version >= 3
        jsr LB580
    .endif
    beq L8E6A
L8E67:
    jsr LBC25
L8E6A:
    clc
    ldy zpAECUR
    sty zpCURSOR
    rts

L8E70:
    ldx zpLINE
    stx zpAELINE
    ldx zpLINE+1
    stx zpAELINE+1
    ldx zpCURSOR
    stx zpAECUR
    cmp #$27
    beq L8E67
    cmp #$8A
    beq L8E40
    cmp #$89
    beq L8E58
    sec
L8E89:
    rts

L8E8A:
    jsr L8A97         ; Skip spaces
    jsr L8E70
    bcc L8E89
    cmp #$22
    beq L8EA7
    sec
    rts

L8E98:
    brk
    dta 9
    .if foldup == 1
        dta 'MISSING '
    .else
        dta 'Missing '
    .endif
    dta 0x22          ; "
    brk

L8EA4:
    jsr LB558
L8EA7:
    iny
    lda (zpAELINE),Y
    cmp #$0D
    beq L8E98
    cmp #$22
    bne L8EA4
    iny
    sty zpAECUR
    lda (zpAELINE),Y
    cmp #$22
    bne L8E6A
    beq L8EA4

; CLG
; ===
L8EBD:
    jsr DONE         ; Check end of statement
    lda #$10
    bne L8ECC         ; Jump to do VDU 16

; CLS
; ===
L8EC4:
    jsr DONE         ; Check end of statement
    jsr LBC28         ; Set COUNT to zero
    lda #$0C          ; Do VDU 12
L8ECC:
    jsr OSWRCH
    jmp L8B9B         ; Send A to OSWRCH, jump to execution loop

; CALL numeric [,items ... ]
; ==========================
L8ED2:
    jsr L9B1D
    jsr L92EE
    jsr LBD94
    ldy #$00
    sty ws+$0600
L8EE0:
    sty ws+$06FF
    jsr L8A8C
    cmp #$2C
    bne L8F0C
    ldy zpAECUR
    jsr L95D5
    beq L8F1B
    ldy ws+$06FF
    iny
    lda zpIACC
    sta ws+$0600,Y
    iny
    lda zpIACC+1
    sta ws+$0600,Y
    iny
    lda zpIACC+2
    sta ws+$0600,Y
    inc ws+$0600
    jmp L8EE0

L8F0C:
    dec zpAECUR
    jsr L9852         ; Check for end of statement
    jsr LBDEA         ; Pop integer to IntA
    jsr L8F1E         ; Set up registers and call code at IntA
    cld               ; Ensure Binary mode on return
    jmp L8B9B         ; Jump back to program loop

L8F1B:
    jmp LAE43

; Call code
; ---------
L8F1E:
    lda VARL_C
    lsr
    lda VARL_A      ; Get Carry from C%, A from A%
    ldx ws+$0460
    ldy ws+$0464      ; Get X from X%, Y from Y%
    .if .def TARGET_C64
        jmp $ff9b
    .else
        jmp (zpIACC)    ; Jump to address in IntA
    .endif


L8F2E:
    jmp L982A

; DELETE linenum, linenum
; =======================
L8F31:
    jsr L97DF
    bcc L8F2E
    jsr LBD94
    jsr L8A97
    cmp #$2C
    bne L8F2E
    jsr L97DF
    bcc L8F2E
    jsr DONE
    lda zpIACC
    sta zp39
    lda zpIACC+1
    sta zp3A
    jsr LBDEA
L8F53:
    jsr LBC2D
    jsr TSTBRK
    jsr L9222
    lda zp39
    cmp zpIACC
    lda zp3A
    sbc zpIACC+1
    bcs L8F53
    jmp L8AF3

; Called by RENUMBER and AUTO
L8F69:
    lda #$0A
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    jsr L97DF
    jsr LBD94
    lda #$0A
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    jsr L8A97
    cmp #$2C
    bne L8F8D
    jsr L97DF
    lda zpIACC+1
    bne L8FDF
    lda zpIACC
    beq L8FDF
    inc zpCURSOR
L8F8D:
    dec zpCURSOR
    jmp DONE

; called by renumber
L8F92:
    lda zpTOP
    sta zp3B
    lda zpTOP+1
    sta zp3C
L8F9A:
    lda zpTXTP
    sta zp38
    lda #$01
    sta zp37
    rts

; RENUMBER [linenume [,linenum]]
; ==============================
L8FA3:
    jsr L8F69
    ldx #$39
    jsr LBE0D
    jsr LBE6F
    jsr L8F92
; Build up table of line numbers
L8FB1:
    ldy #$00
    lda (zp37),Y
    bmi L8FE7         ; Line.hi>$7F, end of program
    sta (zp3B),Y
    iny
    lda (zp37),Y
    sta (zp3B),Y
    sec
    tya
    adc zp3B
    sta zp3B
    tax
    lda zp3C
    adc #$00
    sta zp3C
    cpx zpHIMEM
    sbc zpHIMEM+1
    bcs L8FD6
    jsr L909F
    bcc L8FB1
L8FD6:
    brk
    dta 0
    dta tknRENUMBER
    .if foldup == 1
        dta ' SPACE'      ; Terminated by following BRK
    .else
        dta ' space'      ; Terminated by following BRK
    .endif
L8FDF:
    brk
    dta 0
    .if foldup == 1
       dta 'SILLY'
    .else
       dta 'Silly'
    .endif
    brk

; Do 4K+12K split here
; --------------------
L8FE7:
    .if split == 1
        jmp X8FE7

; PROCname [(parameters)]
; =======================
X9304:
        lda zpLINE
        sta zpAELINE      ; PtrB=PtrA=>after 'PROC' token
        lda zpLINE+1
        sta zpAELINE+1
        lda zpCURSOR
        sta zpAECUR
        lda #$F2
        jsr LB197     ; Call PROC/FN dispatcher
                      ; Will return here after ENDPROC
        jsr L9852     ; Check for end of statement
        jmp L8B9B     ; Return to execution loop
    .endif

; Look for renumber references
X8FE7:
    jsr L8F9A
L8FEA:
    ldy #$00
    lda (zp37),Y
    bmi L900D
    lda zp3A
    sta (zp37),Y
    lda zp39
    iny
    sta (zp37),Y
    clc
    lda zpIACC
    adc zp39
    sta zp39
    lda #$00
    adc zp3A
    and #$7F
    sta zp3A
    jsr L909F
    bcc L8FEA
L900D:
    lda zpTXTP
    sta zpLINE+1
    ldy #$00
    sty zpLINE
    iny
    lda (zpLINE),Y
    .if version < 3
        bmi L903A
    .elseif version >= 3
        bmi L9080
    .endif
L901A:
    ldy #$04
L901C:
    lda (zpLINE),Y
    cmp #$8D
    beq L903D
    iny
    cmp #$0D
    bne L901C
    lda (zpLINE),Y
    .if version < 3
        bmi L903A
    .elseif version >= 3
        bmi L9080
    .endif
    ldy #$03
    lda (zpLINE),Y
    clc
    adc zpLINE
    sta zpLINE
    bcc L901A
    inc zpLINE+1
    bcs L901A
L903A:
    .if version < 3
        jmp L8AF3
    .endif

L903D:
    jsr L97EB
    jsr L8F92
L9043:
    ldy #$00
    lda (zp37),Y
    bmi L9082
    lda (zp3B),Y
    iny
    cmp zpIACC+1
    bne L9071
    lda (zp3B),Y
    cmp zpIACC
    bne L9071
    lda (zp37),Y
    sta zp3D
    dey
    lda (zp37),Y
    sta zp3E
    ldy zpCURSOR
    dey
    lda zpLINE
    sta zp37
    lda zpLINE+1
    sta zp38
    jsr L88F5
L906D:
    ldy zpCURSOR
    bne L901C
L9071:
    .if version >= 3
        clc
    .endif
    jsr L909F
    lda zp3B
    adc #$02
    sta zp3B
    bcc L9043
    inc zp3C
    bcs L9043
L9080:
    .if version >= 3
        bmi L90D9
    .endif
L9082:
    jsr LBFCF         ; Print inline text
    .if foldup == 1
        dta 'FAILED AT '
    .else
        dta 'Failed at '
    .endif
    iny
    lda (zpLINE),Y
    sta zpIACC+1
    iny
    lda (zpLINE),Y
    sta zpIACC
    jsr L991F         ; Print in decimal
    jsr LBC25         ; Print newline
    beq L906D
L909F:
    iny
    lda (zp37),Y
    adc zp37
    sta zp37
    bcc L90AB
    inc zp38
    clc
L90AB:
    rts

; AUTO [numeric [, numeric ]]
; ===========================
L90AC:
    jsr L8F69
    lda zpIACC
    pha
    jsr LBDEA
L90B5:
    jsr LBD94
    jsr L9923
    lda #$20
    jsr LBC02
    jsr LBDEA
    jsr L8951
    jsr LBC8D
    jsr LBD20
    pla
    pha
    clc
    adc zpIACC
    sta zpIACC
    bcc L90B5
    inc zpIACC+1
    bpl L90B5
L90D9:
    jmp L8AF3

L90DC:
    jmp L9218

L90DF:
    dec zpCURSOR
    jsr L9582
    beq L9127
    bcs L9127
    jsr LBD94
    jsr L92DD
    jsr L9222
    lda zpIACC+3
    ora zpIACC+2
    bne L9127
    clc
    lda zpIACC
    adc zpFSA
    tay
    lda zpIACC+1
    adc zpFSA+1
    tax
    cpy zpAESTKP
    sbc zpAESTKP+1
    bcs L90DC
    lda zpFSA
    sta zpIACC
    lda zpFSA+1
    sta zpIACC+1
    sty zpFSA
    stx zpFSA+1
    lda #$00
    sta zpIACC+2
    sta zpIACC+3
    lda #$40
    sta zpTYPE
    jsr LB4B4
    jsr L8827
    jmp L920B

L9127:
    brk
    dta 10
    .if foldup == 1
        dta 'BAD '
    .else
        dta 'Bad '
    .endif
    dta tknDIM
    brk

; DIM numvar [numeric] [(arraydef)]
; =================================
L912F:
    jsr L8A97
    tya
    clc
    adc zpLINE
    ldx zpLINE+1
    bcc L913C
    inx
    clc
L913C:
    sbc #$00
    sta zp37
    txa
    sbc #$00
    sta zp38
    ldx #$05
    stx zp3F
    ldx zpCURSOR
    jsr L9559
    cpy #$01
    beq L9127
    cmp #'('
    beq L916B
    cmp #$24
    beq L915E
    cmp #$25
    bne L9168
L915E:
    dec zp3F
    iny
    inx
    lda (zp37),Y
    cmp #'('
    beq L916B
L9168:
    jmp L90DF

L916B:
    sty zp39
    stx zpCURSOR
    jsr L9469
    bne L9127
    jsr L94FC
    ldx #$01
    jsr L9531
    lda zp3F
    pha
    lda #$01
    pha
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
L9185:
    jsr LBD94
    jsr L8821
    lda zpIACC+1
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne L9127
    jsr L9222
    pla
    tay
    lda zpIACC
    sta (zpFSA),Y
    iny
    lda zpIACC+1
    sta (zpFSA),Y
    iny
    tya
    pha
    jsr L9231
    jsr L8A97
    cmp #$2C
    beq L9185
    cmp #')'
    beq L91B7
    jmp L9127

L91B7:
    pla
    sta zpPRINTF
    pla
    sta zp3F
    lda #$00
    sta zp40
    jsr L9236
    ldy #$00
    lda zpPRINTF
    sta (zpFSA),Y
    adc zpIACC
    sta zpIACC
    bcc L91D2
    inc zpIACC+1
L91D2:
    lda zpFSA+1
    sta zp38
    lda zpFSA
    sta zp37
    clc
    adc zpIACC
    tay
    lda zpIACC+1
    adc zpFSA+1
    bcs L9218
    tax
    cpy zpAESTKP
    sbc zpAESTKP+1
    bcs L9218
    sty zpFSA
    stx zpFSA+1
    lda zp37
    adc zpPRINTF
    tay
    lda #$00
    sta zp37
    bcc L91FC
    inc zp38
L91FC:
    sta (zp37),Y
    iny
    bne L9203
    inc zp38
L9203:
    cpy zpFSA
    bne L91FC
    cpx zp38
    bne L91FC
L920B:
    jsr L8A97
    cmp #$2C
    beq L9215
    jmp L8B96

L9215:
    jmp L912F

L9218:
    brk
    dta 11
    dta tknDIM
    .if foldup == 1
        dta ' SPACE'
    .else
        dta ' space'
    .endif
    brk

L9222:
    inc zpIACC
    bne L9230
    inc zpIACC+1
    bne L9230
    inc zpIACC+2
    bne L9230
    inc zpIACC+3
L9230:
    rts

L9231:
    ldx #$3F
    jsr LBE0D
L9236:
    ldx #$00
    ldy #$00
L923A:
    lsr zp40
    ror zp3F
    bcc L924B
    clc
    tya
    adc zpIACC
    tay
    txa
    adc zpIACC+1
    tax
    bcs L925A
L924B:
    asl zpIACC
    rol zpIACC+1
    lda zp3F
    ora zp40
    bne L923A
    sty zpIACC
    stx zpIACC+1
    rts

L925A:
    jmp L9127

; HIMEM=numeric
; =============
L925D:
    jsr L92EB         ; Set past '=', evaluate integer
    lda zpIACC
    sta zpHIMEM
    sta zpAESTKP          ; Set HIMEM and STACK
    lda zpIACC+1
    sta zpHIMEM+1
    sta zpAESTKP+1
    jmp L8B9B         ; Jump back to execution loop

; LOMEM=numeric
; =============
L926F:
    jsr L92EB         ; Step past '=', evaluate integer
    lda zpIACC
    sta zpLOMEM
    sta zpFSA          ; Set LOMEM and VAREND
    lda zpIACC+1
    sta zpLOMEM+1
    sta zpFSA+1
    jsr LBD2F
    beq L928A         ; Clear dynamic variables, jump to execution loop

; PAGE=numeric
; ============
L9283:
    jsr L92EB         ; Step past '=', evaluate integer
    lda zpIACC+1
    sta zpTXTP          ; Set PAGE
L928A:
    jmp L8B9B         ; Jump to execution loop

; CLEAR
; =====
L928D:
    jsr DONE         ; Check end of statement
    jsr LBD20         ; Clear heap, stack, data, variables
    beq L928A         ; Jump to execution loop

; TRACE ON | OFF | numeric
; ========================
L9295:
    jsr L97DF
    bcs L92A5         ; If line number, jump for TRACE linenum
    cmp #$EE
    beq L92B7         ; Jump for TRACE ON
    cmp #$87
    beq L92C0         ; Jump for TRACE OFF
    jsr L8821         ; Evaluate integer

; TRACE numeric
; -------------
L92A5:
    jsr DONE         ; Check end of statement
    lda zpIACC
    sta zpTRNUM          ; Set trace limit low byte
    lda zpIACC+1
L92AE:
    sta zpTRNUM+1
    lda #$FF          ; Set trace limit high byte, set TRACE ON
L92B2:
    sta zpTRFLAG
    jmp L8B9B         ; Set TRACE flag, return to execution loop

; TRACE ON
; --------
L92B7:
    inc zpCURSOR
    jsr DONE         ; Step past, check end of statement
    lda #$FF
    bne L92AE         ; Jump to set TRACE $FFxx

; TRACE OFF
; ---------
L92C0:
    inc zpCURSOR
    jsr DONE         ; Step past, check end of statement
    lda #$00
    beq L92B2         ; Jump to set TRACE OFF

; TIME=numeric
; ============
L92C9:
    jsr L92EB         ; Step past '=', evaluate integer
    .ifdef MOS_BBC
        ldx #$2A
        ldy #$00
        sty zp2E      ; Point to integer, set 5th byte to 0
        lda #$02
        jsr OSWORD    ; Call OSWORD $02 to do TIME=
    .endif
    jmp L8B9B         ; Jump to execution loop

; Evaluate <comma><numeric>
; =========================
L92DA:
    jsr L8AAE         ; Check for and step past comma
L92DD:
    jsr L9B29
    jmp L92F0

L92E3:
    jsr LADEC
    beq L92F7
    bmi L92F4
L92EA:
    rts

; Evaluate <equals><integer>
; ==========================
L92EB:
    jsr L9807         ; Check for equals, evaluate numeric
L92EE:
    lda zpTYPE          ; Get result type
L92F0:
    beq L92F7         ; String, jump to 'Type mismatch'
    bpl L92EA         ; Integer, return
L92F4:
    jmp LA3E4         ; Real, jump to convert to integer

L92F7:
    jmp L8C0E         ; Jump to 'Type mismatch' error

; Evaluate <real>
; ===============
L92FA:
    jsr LADEC         ; Evaluate expression

; Ensure value is real
; --------------------
L92FD:
    beq L92F7         ; String, jump to 'Type mismatch'
    bmi L92EA         ; Real, return
    jmp LA2BE         ; Integer, jump to convert to real

    .if split == 0
; PROCname [(parameters)]
; =======================
L9304:
        lda zpLINE
        sta zpAELINE      ; PtrB=PtrA=>after 'PROC' token
        lda zpLINE+1
        sta zpAELINE+1
        lda zpCURSOR
        sta zpAECUR
        lda #$F2
        jsr LB197     ; Call PROC/FN dispatcher
                      ; Will return here after ENDPROC
        jsr L9852     ; Check for end of statement
        jmp L8B9B     ; Return to execution loop
    .endif

; Make string zero length
; -----------------------
L931B:
    ldy #$03
    lda #$00          ; Set length to zero
    sta (zpIACC),Y
    beq L9341         ; Jump to look for next LOCAL item

; LOCAL variable [,variable ...]
; ==============================
L9323:
    tsx
    cpx #$FC
    bcs L936B         ; Not inside subroutine, error
    jsr L9582
    beq L9353         ; Find variable, jump if bad variable name
    jsr LB30D         ; Push value on stack, push variable info on stack
    ldy zpIACC+2
    bmi L931B         ; If a string, jump to make zero length
    jsr LBD94         ; 
    lda #$00          ; Set IntA to zero
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    sta zpTYPE
    jsr LB4B4         ; Set current variable to IntA (zero)

; Next LOCAL item
; ---------------
L9341:
    tsx
    inc $0106,X       ; Increment number of LOCAL items
    ldy zpAECUR
    sty zpCURSOR          ; Update line pointer
    jsr L8A97         ; Get next character
    cmp #$2C
    beq L9323         ; Comma, loop back to do another item
    jmp L8B96         ; Jump to main execution loop

L9353:
    jmp L8B98

; ENDPROC
; =======
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknPROC
L9356:
    tsx
    cpx #$FC
    bcs L9365         ; If stack empty, jump to give error
    lda $01FF
    cmp #$F2
    bne L9365         ; If pushed token<>'PROC', give error
    jmp DONE         ; Check for end of statement and return to pop from subroutine
L9365:
    brk
    dta 13
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknPROC       ; Terminated by following BRK
L936B:
    brk
    dta 12
    .if foldup == 1
        dta 'NOT '
    .else
        dta 'Not '
    .endif
    dta tknLOCAL      ; Terminated by following BRK
L9372:
    brk
    dta $19
    .if foldup == 1
        dta 'BAD '
    .else
        dta 'Bad '
    .endif
    dta tknMODE
    brk

; GCOL numeric, numeric
; =====================
L937A:
    jsr L8821
    lda zpIACC
    pha               ; Evaluate integer
    jsr L92DA         ; Step past comma, evaluate integer
    jsr L9852         ; Update program pointer, check for end of statement
    lda #$12
    jsr OSWRCH        ; Send VDU 18 for GCOL
    jmp L93DA         ; Jump to send two bytes to OSWRCH

; COLOUR numeric
; ==============
L938E:
    lda #$11
    pha               ; Stack VDU 17 for COLOUR
    jsr L8821
    jsr DONE         ; Evaluate integer, check end of statement
    jmp L93DA         ; Jump to send two bytes to OSWRCH

; MODE numeric
; ============
L939A:
    lda #$16
    pha               ; Stack VDU 22 for MODE
    jsr L8821
    jsr DONE         ; Evaluate integer, check end of statement

; BBC - Check if changing MODE will move screen into stack
; --------------------------------------------------------
    .ifdef MOS_BBC
        jsr LBEE7     ; Get machine address high word
        cpx #$FF
        bne L93D7     ; Not $xxFFxxxx, skip memory test
        cpy #$FF
        bne L93D7     ; Not $FFFFxxxx, skip memory test

                      ; MODE change in I/O processor, must check memory limits
        lda zpAESTKP
        cmp zpHIMEM
        bne L9372     ; STACK<>HIMEM, stack not empty, give 'Bad MODE' error
        lda zpAESTKP+1
        cmp zpHIMEM+1
        bne L9372
        ldx zpIACC
        lda #$85
        jsr OSBYTE    ; Get top of memory if we used this MODE
        cpx zpFSA
        tya
        sbc zpFSA+1
        bcc L9372     ; Would be below VAREND, give error
        cpx zpTOP
        tya
        sbc zpTOP+1
        bcc L9372     ; Would be below TOP, give error

                      ; BASIC stack is empty, screen would not hit heap or program
        stx zpHIMEM
        stx zpAESTKP      ; Set STACK and HIMEM to new address
        sty zpHIMEM+1
        sty zpAESTKP+1
    .endif

; Change MODE
L93D7:
    jsr LBC28         ; Set COUNT to zero

; Send two bytes to OSWRCH, stacked byte, then IntA
; -------------------------------------------------
L93DA:
    pla
    jsr OSWRCH        ; Send stacked byte to OSWRCH
    jsr L9456
    jmp L8B9B         ; Send IntA to OSWRCH, jump to execution loop

; MOVE numeric, numeric
; =====================
L93E4:
    lda #$04
    bne L93EA         ; Jump forward to do PLOT 4 for MOVE

; DRAW numeric, numeric
; =====================
L93E8:
    lda #$05          ; Do PLOT 5 for DRAW
L93EA:
    pha
    jsr L9B1D         ; Evaluate first expression
    jmp L93FD         ; Jump to evaluate second expression and send to OSWRCH

; PLOT numeric, numeric, numeric
; ==============================
L93F1:
    jsr L8821
    lda zpIACC
    pha               ; Evaluate integer
    jsr L8AAE
    jsr L9B29         ; Step past comma, evaluate expression
L93FD:
    jsr L92EE         ; Confirm numeric and ensure is integer
    jsr LBD94         ; Stack integer
    jsr L92DA         ; Step past command and evaluate integer
    jsr L9852         ; Update program pointer, check for end of statement
    lda #$19
    jsr OSWRCH        ; Send VDU 25 for PLOT
    pla
    jsr OSWRCH        ; Send PLOT action
    jsr LBE0B         ; Pop integer to temporary store at $37/8
    lda zp37
    jsr OSWRCH        ; Send first coordinate to OSWRCH
    lda zp38
    jsr OSWRCH
    jsr L9456         ; Send IntA to OSWRCH, second coordinate
    lda zpIACC+1
    jsr OSWRCH        ; Send IntA high byte to OSWRCH
    jmp L8B9B         ; Jump to execution loop


L942A:
    lda zpIACC+1
    jsr OSWRCH        ; Send IntA byte 2 to OSWRCH

; VDU num[,][;][...]
; ==================
L942F:
    jsr L8A97         ; Get next character
L9432:
    cmp #$3A
    beq L9453         ; If end of statement, jump to exit
    cmp #$0D
    beq L9453
    cmp #$8B
    beq L9453
    dec zpCURSOR          ; Step back to current character
    jsr L8821
    jsr L9456         ; Evaluate integer and output low byte
    jsr L8A97         ; Get next character
    cmp #','
    beq L942F         ; Comma, loop to read another number
    cmp #';'
    bne L9432         ; Not semicolon, loop to check for end of statement
    beq L942A         ; Loop to output high byte and read another
L9453:
    jmp L8B96         ; Jump to execution loop


; Send IntA to OSWRCH via WRCHV
; =============================
L9456:
    lda zpIACC
    .if WRCHV != 0
        jmp (WRCHV)
    .elseif WRCHV == 0
        jmp OSWRCH 
    .endif


; VARIABLE PROCESSING
; ===================
; Look for a FN/PROC in heap
; --------------------------
; On entry, ($37)+1=>FN/PROC token (ie, first character of name)
;
L945B:
    ldy #$01
    lda (zp37),Y      ; Get PROC/FN character
    ldy #$F6          ; Point to PROC list start
    cmp #tknPROC
    beq L946F         ; If PROC, jump to scan list
    ldy #$F8
    bne L946F         ; Point to FN list start and scan list

; Look for a variable in the heap
; -------------------------------
; On entry, ($37)+1=>first character of name
;
L9469:
    ldy #$01
    lda (zp37),Y      ; Get first character of variable
    asl
    tay               ; Double it to index into index list

; Scan though linked lists in heap
; --------------------------------
L946F:
    lda VARL,Y
    sta zp3A          ; Get start of linked list
    lda VARL+1,Y
    sta zp3B
L9479:
    lda zp3B
    beq L94B2         ; End of list
    ldy #$00
    lda (zp3A),Y
    sta zp3C
    iny
    lda (zp3A),Y
    sta zp3D
    iny
    lda (zp3A),Y
    bne L949A         ; Jump if not null name
    dey
    cpy zp39
    bne L94B3
    iny
    bcs L94A7
L9495:
    iny
    lda (zp3A),Y
    beq L94B3
L949A:
    cmp (zp37),Y
    bne L94B3
    cpy zp39
    bne L9495
    iny
    lda (zp3A),Y
    bne L94B3
L94A7:
    tya
    adc zp3A
    sta zpIACC
    lda zp3B
    adc #$00
    sta zpIACC+1
L94B2:
    rts

L94B3:
    lda zp3D
    beq L94B2
    ldy #$00
    lda (zp3C),Y
    sta zp3A
    iny
    lda (zp3C),Y
    sta zp3B
    iny
    lda (zp3C),Y
    bne L94D4
    dey
    cpy zp39
    bne L9479
    iny
    bcs L94E1
L94CF:
    iny
    lda (zp3C),Y
    beq L9479
L94D4:
    cmp (zp37),Y
    bne L9479
    cpy zp39
    bne L94CF
    iny
    lda (zp3C),Y
    bne L9479
L94E1:
    tya
    adc zp3C
    sta zpIACC
    lda zp3D
    adc #$00
    sta zpIACC+1
    rts

L94ED:
    ldy #$01
    lda (zp37),Y
    tax
    lda #$F6
    cpx #$F2
    beq L9501
    lda #$F8
    bne L9501
L94FC:
    ldy #$01
    lda (zp37),Y
    asl
L9501:
    sta zp3A
    lda #$04+(ws/256)
    sta zp3B
L9507:
    lda (zp3A),Y
    beq L9516
    tax
    dey
    lda (zp3A),Y
    sta zp3A
    stx zp3B
    iny
    bpl L9507
L9516:
    lda zpFSA+1
    sta (zp3A),Y
    lda zpFSA
    dey
    sta (zp3A),Y
    tya
    iny
    sta (zpFSA),Y
    cpy zp39
    beq L9558
L9527:
    iny
    lda (zp37),Y
    sta (zpFSA),Y
    cpy zp39
    bne L9527
    rts

L9531:
    lda #$00
L9533:
    iny
    sta (zpFSA),Y
    dex
    bne L9533
L9539:
    sec
    tya
    adc zpFSA
    bcc L9541
    inc zpFSA+1
L9541:
    ldy zpFSA+1
    cpy zpAESTKP+1
    bcc L9556
    bne L954D
    cmp zpAESTKP
    bcc L9556
L954D:
    lda #$00
    ldy #$01
    sta (zp3A),Y
    jmp L8CB7

L9556:
    sta zpFSA
L9558:
    rts


; Check if variable name is valid
; ===============================
L9559:
    ldy #$01
L955B:
    lda (zp37),Y
    cmp #$30
    bcc L9579
    cmp #$40
    bcs L9571
    cmp #$3A
    bcs L9579
    cpy #$01
    beq L9579
L956D:
    inx
    iny
    bne L955B
L9571:
    cmp #$5F
    bcs L957A
    cmp #$5B
    bcc L956D
L9579:
    rts

L957A:
    cmp #$7B
    bcc L956D
    rts

L957F:
    jsr L9531
L9582:
    jsr L95C9
    bne L95A4
    bcs L95A4
    jsr L94FC
    ldx #$05
    cpx zpIACC+2
    bne L957F
    inx
    bne L957F
L9595:
    cmp #$21
    beq L95A5
    cmp #$24
    beq L95B0
    eor #$3F
    beq L95A7
    lda #$00
    sec
L95A4:
    rts

L95A5:
    lda #$04
L95A7:
    pha
    inc zpAECUR
    jsr L92E3
    jmp L969F

L95B0:
    inc zpAECUR
    jsr L92E3
    lda zpIACC+1
    beq L95BF
    lda #$80
    sta zpIACC+2
    sec
    rts

L95BF:
    brk
    dta 8
    .if foldup == 1
        dta '$ RANGE'
    .else
        dta '$ range'
    .endif
    brk
L95C9:
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    ldy zpCURSOR
    dey
L95D4:
    iny
L95D5:
    sty zpAECUR
    lda (zpAELINE),Y
    cmp #$20
    beq L95D4
L95DD:
    cmp #$40
    bcc L9595
    cmp #$5B
    bcs L95FF
    asl
    asl
    sta zpIACC
    lda #$04+(ws/256)
    sta zpIACC+1
    iny
    lda (zpAELINE),Y
    iny
    cmp #$25
    bne L95FF
    ldx #$04
    stx zpIACC+2
    lda (zpAELINE),Y
    cmp #'('
    bne L9665
L95FF:
    ldx #$05
    stx zpIACC+2
    lda zpAECUR
    clc
    adc zpAELINE
    ldx zpAELINE+1
    bcc L960E
    inx
    clc
L960E:
    sbc #$00
    sta zp37
    bcs L9615
    dex
L9615:
    stx zp38
    ldx zpAECUR
    ldy #$01
L961B:
    lda (zp37),Y
    cmp #$41
    bcs L962D
    cmp #$30
    bcc L9641
    cmp #$3A
    bcs L9641
    inx
    iny
    bne L961B
L962D:
    cmp #$5B
    bcs L9635
    inx
    iny
    bne L961B
L9635:
    cmp #$5F
    bcc L9641
    cmp #$7B
    bcs L9641
    inx
    iny
    bne L961B
L9641:
    dey
    beq L9673
    cmp #$24
    beq L96AF
    cmp #$25
    bne L9654
    dec zpIACC+2
    iny
    inx
    iny
    lda (zp37),Y
    dey
L9654:
    sty zp39
    cmp #'('
    beq L96A6
    jsr L9469
    beq L9677
    stx zpAECUR
L9661:
    ldy zpAECUR
    lda (zpAELINE),Y
L9665:
    cmp #$21
    beq L967F
    cmp #$3F
    beq L967B
    clc
    sty zpAECUR
    lda #$FF
    rts

L9673:
    lda #$00
    sec
    rts

L9677:
    lda #$00
    clc
    rts

L967B:
    lda #$00
    beq L9681
L967F:
    lda #$04
L9681:
    pha
    iny
    sty zpAECUR
    jsr LB32C
    jsr L92F0
    lda zpIACC+1
    pha
    lda zpIACC
    pha
    jsr L92E3
    clc
    pla
    adc zpIACC
    sta zpIACC
    pla
    adc zpIACC+1
    sta zpIACC+1
L969F:
    pla
    sta zpIACC+2
    clc
    lda #$FF
    rts

L96A6:
    inx
    inc zp39
    jsr L96DF
    jmp L9661

L96AF:
    inx
    iny
    sty zp39
    iny
    dec zpIACC+2
    lda (zp37),Y
    cmp #'('
    beq L96C9
    jsr L9469
    beq L9677
    stx zpAECUR
    lda #$81
    sta zpIACC+2
    sec
    rts

L96C9:
    inx
    sty zp39
    dec zpIACC+2
    jsr L96DF
    lda #$81
    sta zpIACC+2
    sec
    rts

L96D7:
    brk
    dta 14
    .if foldup == 1
        dta 'ARRAY'
    .else
        dta 'Array'
    .endif
    brk

L96DF:
    jsr L9469
    beq L96D7
    stx zpAECUR
    lda zpIACC+2
    pha
    lda zpIACC
    pha
    lda zpIACC+1
    pha
    ldy #$00
    lda (zpIACC),Y
    cmp #$04
    bcc L976C
    tya
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    lda #$01
    sta zpIACC+3
L96FF:
    jsr LBD94
    jsr L92DD
    inc zpAECUR
    cpx #$2C
    bne L96D7
    ldx #$39
    jsr LBE0D
    ldy zp3C
    pla
    sta zp38
    pla
    sta zp37
    pha
    lda zp38
    pha
    jsr L97BA
    sty zpIACC+3
    lda (zp37),Y
    sta zp3F
    iny
    lda (zp37),Y
    sta zp40
    lda zpIACC
    adc zp39
    sta zpIACC
    lda zpIACC+1
    adc zp3A
    sta zpIACC+1
    jsr L9236
    ldy #$00
    sec
    lda (zp37),Y
    sbc zpIACC+3
    cmp #$03
    bcs L96FF
    jsr LBD94
    jsr LAE56
    jsr L92F0
    pla
    sta zp38
    pla
    sta zp37
    ldx #$39
    jsr LBE0D
    ldy zp3C
    jsr L97BA
    clc
    lda zp39
    adc zpIACC
    sta zpIACC
    lda zp3A
    adc zpIACC+1
    sta zpIACC+1
    bcc L977D
L976C:
    jsr LAE56
    jsr L92F0
    pla
    sta zp38
    pla
    sta zp37
    ldy #$01
    jsr L97BA
L977D:
    pla
    sta zpIACC+2
    cmp #$05
    bne L979B
    ldx zpIACC+1
    lda zpIACC
    asl zpIACC
    rol zpIACC+1
    asl zpIACC
    rol zpIACC+1
    adc zpIACC
    sta zpIACC
    txa
    adc zpIACC+1
    sta zpIACC+1
    bcc L97A3
L979B:
    asl zpIACC
    rol zpIACC+1
    asl zpIACC
    rol zpIACC+1
L97A3:
    tya
    adc zpIACC
    sta zpIACC
    bcc L97AD
    inc zpIACC+1
    clc
L97AD:
    lda zp37
    adc zpIACC
    sta zpIACC
    lda zp38
    adc zpIACC+1
    sta zpIACC+1
    rts

L97BA:
    lda zpIACC+1
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne L97D1
    lda zpIACC
    cmp (zp37),Y
    iny
    lda zpIACC+1
    sbc (zp37),Y
    bcs L97D1
    iny
    rts

L97D1:
    brk
    dta 15
    .if foldup == 1
        dta 'SUBSCRIPT'
    .else
        dta 'Subscript'
    .endif
    brk
L97DD:
    inc zpCURSOR
L97DF:
    ldy zpCURSOR
    lda (zpLINE),Y
    cmp #$20
    beq L97DD
    cmp #$8D
    bne L9805
L97EB:
    iny
    lda (zpLINE),Y
    asl
    asl
    tax
    and #$C0
    iny
    eor (zpLINE),Y
    sta zpIACC
    txa
    asl
    asl
    iny
    eor (zpLINE),Y
    sta zpIACC+1
    iny
    sty zpCURSOR
    sec
    rts

L9805:
    clc
    rts

L9807:
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    lda zpCURSOR
    sta zpAECUR
L9813:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y
    cmp #$20
    beq L9813
    cmp #$3D
    beq L9849
L9821:
    brk
    dta 4
    .if foldup == 1
        dta 'MISTAKE'
    .else
        dta 'Mistake'
    .endif
L982A:
    brk
    dta 16
    .if foldup == 1
        dta 'SYNTAX ERROR'    ; Terminated by following BRK
    .else
        dta 'Syntax error'    ; Terminated by following BRK
    .endif
    .ifdef MOS_ATOM
        brk
    .endif

; Escape error
; ------------
L9838:
    .ifdef TARGET_ATOM
        lda ESCFLG
        and #$20
        beq L9838     ; Loop until Escape not pressed
    .endif

    .ifdef TARGET_SYSTEM
        cmp ESCFLG
        beq L9838     ; Loop until key no longer pressed
    .endif

    brk
    dta 17
    .if foldup == 1
        dta 'ESCAPE'
    .else
        dta 'Escape'
    .endif
    brk

L9841:
    jsr L8A8C
    cmp #'='
    bne L9821
    rts

L9849:
    jsr L9B29
L984C:
    txa
    ldy zpAECUR
    jmp DONET

L9852:
    ldy zpAECUR
    jmp DONE_WITH_Y

; Check for end of statement, check for Escape
; ============================================
DONE:
    ldy zpCURSOR          ; Get program pointer offset
DONE_WITH_Y:
    dey               ; Step back to previous character
BLINK:
    iny
    lda (zpLINE),Y      ; Get next character
    cmp #' '
    beq BLINK         ; Skip spaces
DONET:
    cmp #':'
    beq CLYADP         ; Colon, jump to update program pointer
    cmp #$0D
    beq CLYADP         ; <cr>, jump to update program pointer
    cmp #tknELSE
    bne L982A         ; Not 'ELSE', jump to 'Syntax error'

; Update program pointer
; ----------------------
CLYADP:
    clc
    tya
    adc zpLINE
    sta zpLINE          ; Update program pointer in PtrA
    bcc SECUR
    inc zpLINE+1
SECUR:
    ldy #$01
    sty zpCURSOR

; Check background Escape state
; -----------------------------
TSTBRK:

; Atom - check keyboard matrix
; ----------------------------
    .ifdef TARGET_ATOM
        pha           ; Save A
        lda ESCFLG
        and #$20      ; Check keyboard matrix
        beq L9838     ; Escape key pressed, jump to error
        pla           ; Restore A
    .endif

; System - check current keypress
; -------------------------------
    .ifdef TARGET_SYSTEM
        bit ESCFLG
        bmi SECEND     ; Nothing pressed
        pha
        lda ESCFLG    ; Save A, get keypress
        cmp #$1B
        beq L9838     ; If Escape, jump to error
        pla           ; Restore A
    .endif

; BBC - check background Escape state
; -----------------------------------
    .ifdef MOS_BBC
        bit ESCFLG
        bmi L9838     ; If Escape set, jump to give error
    .endif

SECEND:
    rts

FORR:
    jsr DONE
    dey
    lda (zpLINE),Y
    cmp #$3A
    beq SECEND

    lda zpLINE+1
    cmp #$07+(ws/256)
    beq LEAVER

LINO:
    iny
    lda (zpLINE),Y
    bmi LEAVER
    lda zpTRFLAG
    beq NOTR
    tya
    pha
    iny
    lda (zpLINE),Y
    pha
    dey
    lda (zpLINE),Y
    tay
    pla
    .if version < 3
        jsr LAEEA
    .elseif version >= 3
        jsr XAED5
    .endif
    jsr L9905
    pla
    tay
NOTR:
    iny
    sec
    tya
    adc zpLINE
    sta zpLINE
    bcc LINOIN
    inc zpLINE+1
LINOIN:
    ldy #$01
    sty zpCURSOR
NOTRDE:
    rts

LEAVER:
    jmp L8AF6

; IF numeric
; ==========
L98BF:
    jmp L8C0E

L98C2:
    jsr L9B1D
    beq L98BF
    bpl L98CC
    jsr LA3E4
L98CC:
    ldy zpAECUR
    sty zpCURSOR
    lda zpIACC
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq L98F1
    cpx #$8C
    beq L98E1
L98DE:
    jmp L8BA3

L98E1:
    inc zpCURSOR
L98E3:
    jsr L97DF
    bcc L98DE
    jsr LB9AF
    jsr SECUR
    jmp LB8D2

L98F1:
    ldy zpCURSOR
L98F3:
    lda (zpLINE),Y
    cmp #$0D
    beq L9902
    iny
    cmp #$8B
    bne L98F3
    sty zpCURSOR
    beq L98E3
L9902:
    jmp L8B87

L9905:
    lda zpIACC
    cmp zpTRNUM
    lda zpIACC+1
    sbc zpTRNUM+1
    bcs NOTRDE
    lda #$5B
L9911:
    jsr LB558
    jsr L991F
    lda #$5D
    jsr LB558
    jmp LB565

; Print 16-bit decimal number
; ===========================
L991F:
    lda #$00          ; No padding
    beq L9925
L9923:
    lda #$05          ; Pad to five characters
L9925:
    sta zpPRINTS
    ldx #$04
L9929:
    lda #$00
    sta zp3F,X
    sec
L992E:
    lda zpIACC
    sbc L996B,X       ; Subtract 10s low byte
    tay
    lda zpIACC+1
    sbc L99B9,X       ; Subtract 10s high byte
    bcc L9943         ; Result<0, no more for this digit
    sta zpIACC+1
    sty zpIACC          ; Update number
    inc zp3F,X
    bne L992E

L9943:
    dex
    bpl L9929
    ldx #$05
L9948:
    dex
    beq L994F
    lda zp3F,X
    beq L9948
L994F:
    stx zp37
    lda zpPRINTS
    beq L9960
    sbc zp37
    beq L9960
    .if version < 3
        tay
L995A:
        jsr LB565
        dey
        bne L995A
    .elseif version >= 3
        tax
        jsr LB580
        ldx zp37
    .endif
L9960:
    lda zp3F,X
    ora #$30
    jsr LB558
    dex
    bpl L9960
    rts

; Low bytes of powers of ten
L996B:
    dta 1, 10, 100, <1000, <10000

; Line Search
L9970:
    ldy #$00
    sty zp3D
    lda zpTXTP
    sta zp3E
L9978:
    ldy #$01
    lda (zp3D),Y
    cmp zpIACC+1
    bcs L998E
L9980:
    ldy #$03
    lda (zp3D),Y
    adc zp3D
    sta zp3D
    bcc L9978
    inc zp3E
    bcs L9978
L998E:
    bne L99A4
    ldy #$02
    lda (zp3D),Y
    cmp zpIACC
    bcc L9980
    bne L99A4
    tya
    adc zp3D
    sta zp3D
    bcc L99A4
    inc zp3E
    clc
L99A4:
    ldy #$02
    rts

ZDIVOR:
    brk
    dta $12
    .if foldup == 1
        dta 'DIVISION BY ZERO'
    .else
        dta 'Division by zero'
    .endif
    ; ending zero overlaps with VALM

; High byte of powers of ten
L99B9:
    dta 0, 0, 0, >1000, >10000

L99BE:
    tay
    jsr L92F0
    lda zpIACC+3
    pha
    jsr LAD71
    jsr L9E1D
    stx zpTYPE
    tay
    jsr L92F0
    pla
    sta zp38
    eor zpIACC+3
    sta zp37
    jsr LAD71
    ldx #$39
    jsr LBE0D
    sty zp3D
    sty zp3E
    sty zp3F
    sty zp40
    lda zpIACC+3
    ora zpIACC
    ora zpIACC+1
    ora zpIACC+2
    beq ZDIVOR      ; Divide by 0 error
    ldy #$20
L99F4:
    dey
    beq L9A38
    asl zp39
    rol zp3A

    rol zp3B
    rol zp3C
    bpl L99F4
L9A01:
    rol zp39
    rol zp3A

    rol zp3B
    rol zp3C
    rol zp3D
    rol zp3E
    rol zp3F
    rol zp40
    sec
    lda zp3D
    sbc zpIACC
    pha
    lda zp3E
    sbc zpIACC+1
    pha
    lda zp3F
    sbc zpIACC+2
    tax
    lda zp40
    sbc zpIACC+3
    bcc L9A33
    sta zp40
    stx zp3F
    pla
    sta zp3E
    pla
    sta zp3D
    bcs L9A35
L9A33:
    pla
    pla
L9A35:
    dey
    bne L9A01
L9A38:
    rts

L9A39:
    stx zpTYPE
    jsr LBDEA
    jsr LBD51
    jsr LA2BE
    jsr LA21E
    jsr LBD7E
    jsr LA3B5
    jmp L9A62

L9A50:
    jsr LBD51
    jsr L9C42
    stx zpTYPE
    tay
    jsr L92FD
    jsr LBD7E
L9A5F:
    jsr LA34E

; Compare FPA = FPB
; -----------------
L9A62:
    ldx zpTYPE
    ldy #$00
    lda zp3B
    and #$80
    sta zp3B
    lda zp2E
    and #$80
    cmp zp3B
    bne L9A92
    lda zp3D
    cmp zp30
    bne L9A93
    lda zp3E
    cmp zp31
    bne L9A93
    lda zp3F
    cmp zp32
    bne L9A93
    lda zp40
    cmp zp33
    bne L9A93
    lda zp41
    cmp zp34
    bne L9A93
L9A92:
    rts

L9A93:
    ror
    eor zp3B
    rol
    lda #$01
    rts

L9A9A:
    jmp L8C0E         ; Jump to 'Type mismatch' error


; Evaluate next expression and compare with previous
; --------------------------------------------------
L9A9D:
    txa
L9A9E:
    beq L9AE7         ; Jump if current is string
    bmi L9A50         ; Jump if current is float
    jsr LBD94         ; Stack integer
    jsr L9C42
    tay               ; Evaluate next expression
    beq L9A9A         ; Error if string
    bmi L9A39         ; Float, jump to compare floats

; Compare IntA with top of stack
; ------------------------------
    lda zpIACC+3
    eor #$80
    sta zpIACC+3
    sec
    ldy #$00
    lda (zpAESTKP),Y
    sbc zpIACC
    sta zpIACC
    iny
    lda (zpAESTKP),Y
    sbc zpIACC+1
    sta zpIACC+1
    iny
    lda (zpAESTKP),Y
    sbc zpIACC+2
    sta zpIACC+2
    iny
    lda (zpAESTKP),Y
    ldy #$00
    eor #$80
    sbc zpIACC+3
    ora zpIACC
    ora zpIACC+1
    ora zpIACC+2
    php
    clc
    lda #$04
    adc zpAESTKP          ; Drop integer from stack
    sta zpAESTKP
    bcc L9AE5
    inc zpAESTKP+1
L9AE5:
    plp
    rts

; Compare string with next expression
; -----------------------------------
L9AE7:
    jsr LBDB2
    jsr L9C42
    tay
    bne L9A9A
    stx zp37
    ldx zp36
    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00
    .endif
    lda (zpAESTKP),Y
    sta zp39
    cmp zp36
    bcs L9AFF
    tax
L9AFF:
    stx zp3A

    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00
    .endif
L9B03:
    cpy zp3A
    beq L9B11
    iny
    lda (zpAESTKP),Y
    cmp ws+$05FF,Y
    beq L9B03
    bne L9B15
L9B11:
    lda zp39
    cmp zp36
L9B15:
    php
    jsr LBDDC
    ldx zp37
    plp
    rts


; EXPRESSION EVALUATOR
; ====================

; Evaluate expression at PtrA
; ---------------------------
L9B1D:
    lda zpLINE
    sta zpAELINE          ; Copy PtrA to PtrB
    lda zpLINE+1
    sta zpAELINE+1
    lda zpCURSOR
    sta zpAECUR

; Evaluate expression at PtrB
; ---------------------------
; TOP LEVEL EVALUATOR
;
; Evaluator Level 7 - OR, EOR
; ---------------------------
L9B29:
    jsr L9B72         ; Call Evaluator Level 6 - AND
                      ; Returns A=type, value in IntA/FPA/StrA, X=next char
L9B2C:
    cpx #tknOR
    beq L9B3A         ; Jump if next char is OR
    cpx #tknEOR
    beq L9B55         ; Jump if next char is EOR
    dec zpAECUR          ; Step PtrB back to last char
    tay
    sta zpTYPE
    rts               ; Set flags from type, store type in $27 and return

; OR numeric
; ----------
L9B3A:
    jsr L9B6B
    tay               ; Stack as integer, call Evaluator Level 6
    jsr L92F0
    ldy #$03          ; If float, convert to integer
L9B43:
    lda (zpAESTKP),Y
    ora zpIACC,Y        ; OR IntA with top of stack    ; abs,y (!)
    sta zpIACC,Y              ; abs,y (!)
    dey
    bpl L9B43         ; Store result in IntA
L9B4E:
    jsr LBDFF         ; Drop integer from stack
    lda #$40
    bne L9B2C         ; Return type=Int, jump to check for more OR/EOR

; EOR numeric
; -----------
L9B55:
    jsr L9B6B
    tay
    jsr L92F0
    ldy #$03          ; If float, convert to integer
L9B5E:
    lda (zpAESTKP),Y
    eor zpIACC,Y        ; EOR IntA with top of stack       ; abs,y (!)
    sta zpIACC,Y                  ; abs,y (!)
    dey
    bpl L9B5E         ; Store result in IntA
    bmi L9B4E         ; Jump to drop from stack and continue

; Stack current as integer, evaluate another Level 6
; --------------------------------------------------
L9B6B:
    tay
    jsr L92F0
    jsr LBD94         ; If float, convert to integer, push into stack

; Evaluator Level 6 - AND
; -----------------------
L9B72:
    jsr L9B9C         ; Call Evaluator Level 5, < <= = >= > <>
L9B75:
    cpx #tknAND
    beq L9B7A
    rts               ; Return if next char not AND

; AND numeric
; -----------
L9B7A:
    tay
    jsr L92F0
    jsr LBD94         ; If float, convert to integer, push onto stack
    jsr L9B9C         ; Call Evaluator Level 5, < <= = >= > <>
    tay
    jsr L92F0
    ldy #$03          ; If float, convert to integer
L9B8A:
    lda (zpAESTKP),Y
    and zpIACC,Y        ; AND IntA with top of stack   ; abs,y (!)
    sta zpIACC,Y              ; abs,y (!)
    dey
    bpl L9B8A         ; Store result in IntA
    jsr LBDFF         ; Drop integer from stack
    lda #$40
    bne L9B75         ; Return type=Int, jump to check for another AND

; Evaluator Level 5 - >... =... or <...
; -------------------------------------
L9B9C:
    jsr L9C42         ; Call Evaluator Level 4, + -
    cpx #'>'+1
    bcs L9BA7         ; Larger than '>', return
    cpx #'<'
    bcs L9BA8         ; Smaller than '<', return
L9BA7:
    rts

; >... =... or <...
; -----------------
L9BA8:
    beq L9BC0         ; Jump with '<'
    cpx #'>'
    beq L9BE8         ; Jump with '>'
                      ; Must be '='
; = numeric
; ---------
    tax
    jsr L9A9E
    bne L9BB5         ; Jump with result=0 for not equal
L9BB4:
    dey               ; Decrement to $FF for equal
L9BB5:
    sty zpIACC
    sty zpIACC+1
    sty zpIACC+2          ; Store 0/-1 in IntA
    sty zpIACC+3
    lda #$40
    rts               ; Return type=Int

; < <= <>
; -------
L9BC0:
    tax
    ldy zpAECUR
    lda (zpAELINE),Y      ; Get next char from PtrB
    cmp #'='
    beq L9BD4         ; Jump for <=
    cmp #'>'
    beq L9BDF         ; Jump for <>

; Must be < numeric
; -----------------
    jsr L9A9D         ; Evaluate next and compare
    bcc L9BB4
    bcs L9BB5         ; Jump to return TRUE if <, FALSE if not <

; <= numeric
; ----------
L9BD4:
    inc zpAECUR
    jsr L9A9D         ; Step past '=', evaluate next and compare
    beq L9BB4
    bcc L9BB4         ; Jump to return TRUE if =, TRUE if <
    bcs L9BB5         ; Jump to return FALSE otherwise

; <> numeric
; ----------
L9BDF:
    inc zpAECUR
    jsr L9A9D         ; Step past '>', evaluate next and compare
    bne L9BB4
    beq L9BB5         ; Jump to return TRUE if <>, FALSE if =

; > >=
; ----
L9BE8:
    tax
    ldy zpAECUR
    lda (zpAELINE),Y      ; Get next char from PtrB
    cmp #'='
    beq L9BFA         ; Jump for >=

; > numeric
; ---------
    jsr L9A9D         ; Evaluate next and compare
    beq L9BB5
    bcs L9BB4         ; Jump to return FALSE if =, TRUE if >
    bcc L9BB5         ; Jump to return FALSE if <

; >= numeric
; ----------
L9BFA:
    inc zpAECUR
    jsr L9A9D         ; Step past '=', evaluate next and compare
    bcs L9BB4
    bcc L9BB5         ; Jump to return TRUE if >=, FALSE if <

L9C03:
    brk
    dta $13
    .if foldup == 1
        dta 'STRING TOO LONG'
    .else
        dta 'String too long'
    .endif
    brk

; String addition
; ---------------
L9C15:
    jsr LBDB2
    jsr L9E20         ; Stack string, call Evaluator Level 2
    tay
    bne L9C88         ; string + number, jump to 'Type mismatch' error
    clc
    stx zp37
    ldy #$00
    lda (zpAESTKP),Y      ; Get stacked string length
    adc zp36
    bcs L9C03         ; If added string length >255, jump to error
    tax
    pha
    ldy zp36          ; Save new string length
L9C2D:
    lda ws+$05FF,Y
    sta ws+$05FF,X    ; Move current string up in string buffer
    dex
    dey
    bne L9C2D
    jsr LBDCB         ; Unstack string to start of string buffer
    pla
    sta zp36
    ldx zp37          ; Set new string length
    tya
    beq L9C45         ; Set type=string, jump to check for more + or -

; Evaluator Level 4, + -
; ----------------------
L9C42:
    jsr L9DD1         ; Call Evaluator Level 3, * / DIV MOD
L9C45:
    cpx #'+'
    beq L9C4E         ; Jump with addition
    cpx #'-'
    beq L9CB5         ; Jump with subtraction
    rts               ; Return otherwise

; + <value>
; ---------
L9C4E:
    tay
    beq L9C15         ; Jump if current value is a string
    bmi L9C8B         ; Jump if current value is a float

; Integer addition
; ----------------
    jsr L9DCE         ; Stack current and call Evaluator Level 3
    tay
    beq L9C88         ; If int + string, jump to 'Type mismatch' error
    bmi L9CA7         ; If int + float, jump ...
    ldy #$00
    clc
    lda (zpAESTKP),Y
    adc zpIACC
    sta zpIACC          ; Add top of stack to IntA
    iny
    lda (zpAESTKP),Y
    adc zpIACC+1
    sta zpIACC+1          ; Store result in IntA
    iny
    lda (zpAESTKP),Y
    adc zpIACC+2
    sta zpIACC+2
    iny
    lda (zpAESTKP),Y
    adc zpIACC+3
L9C77:
    sta zpIACC+3
    clc
    lda zpAESTKP
    adc #$04
    sta zpAESTKP          ; Drop integer from stack
    lda #$40
    bcc L9C45         ; Set result=integer, jump to check for more + or -
    inc zpAESTKP+1
    bcs L9C45         ; Jump to check for more + or -

L9C88:
    jmp L8C0E         ; Jump to 'Type mismatch' error

; Real addition
; -------------
L9C8B:
    jsr LBD51
    jsr L9DD1         ; Stack float, call Evaluator Level 3
    tay
    beq L9C88         ; float + string, jump to 'Type mismatch' error
    stx zpTYPE
    bmi L9C9B         ; float + float, skip conversion
    jsr LA2BE         ; float + int, convert int to float
L9C9B:
    jsr LBD7E         ; Pop float from stack, point FPTR to it
    jsr LA500         ; Unstack float to FPA2 and add to FPA1
L9CA1:
    ldx zpTYPE          ; Get nextchar back
    lda #$FF
    bne L9C45         ; Set result=float, loop to check for more + or -

; int + float
; -----------
L9CA7:
    stx zpTYPE
    jsr LBDEA         ; Unstack integer to IntA
    jsr LBD51
    jsr LA2BE         ; Stack float, convert integer in IntA to float in FPA1
    jmp L9C9B         ; Jump to do float + <stacked float>

; - numeric
; ---------
L9CB5:
    tay
    beq L9C88         ; If current value is a string, jump to error
    bmi L9CE1         ; Jump if current value is a float

; Integer subtraction
; -------------------
    jsr L9DCE         ; Stack current and call Evaluator Level 3
    tay
    beq L9C88         ; int + string, jump to error
    bmi L9CFA         ; int + float, jump to convert and do real subtraction
    sec
    ldy #$00
    lda (zpAESTKP),Y
    sbc zpIACC
    sta zpIACC
    iny
    lda (zpAESTKP),Y
    sbc zpIACC+1
    sta zpIACC+1          ; Subtract IntA from top of stack
    iny
    lda (zpAESTKP),Y
    sbc zpIACC+2
    sta zpIACC+2          ; Store in IntA
    iny
    lda (zpAESTKP),Y
    sbc zpIACC+3
    jmp L9C77         ; Jump to pop stack and loop for more + or -

; Real subtraction
; ----------------
L9CE1:
    jsr LBD51
    jsr L9DD1         ; Stack float, call Evaluator Level 3
    tay
    beq L9C88         ; float - string, jump to 'Type mismatch' error
    stx zpTYPE
    bmi L9CF1         ; float - float, skip conversion
    jsr LA2BE         ; float - int, convert int to float
L9CF1:
    jsr LBD7E         ; Pop float from stack and point FPTR to it
    jsr LA4FD         ; Unstack float to FPA2 and subtract it from FPA1
    jmp L9CA1         ; Jump to set result and loop for more + or -

; int - float
; -----------
L9CFA:
    stx zpTYPE
    jsr LBDEA         ; Unstack integer to IntA
    jsr LBD51
    jsr LA2BE         ; Stack float, convert integer in IntA to float in FPA1
    jsr LBD7E         ; Pop float from stack, point FPTR to it
    jsr LA4D0         ; Subtract FPTR float from FPA1 float
    jmp L9CA1         ; Jump to set result and loop for more + or -

L9D0E:
    jsr LA2BE
L9D11:
    jsr LBDEA
    jsr LBD51
    jsr LA2BE
    jmp L9D2C

L9D1D:
    jsr LA2BE
L9D20:
    jsr LBD51
    jsr L9E20
    stx zpTYPE
    tay
    jsr L92FD
L9D2C:
    jsr LBD7E
    jsr FMUL
    lda #$FF
    ldx zpTYPE
    jmp L9DD4

L9D39:
    jmp L8C0E

; * <value>
; ---------
L9D3C:
    tay
    beq L9D39         ; If current value is string, jump to error
    bmi L9D20         ; Jump if current valus ia a float
    lda zpIACC+3
    cmp zpIACC+2
    bne L9D1D
    tay
    beq L9D4E
    cmp #$FF
    bne L9D1D

L9D4E:
    eor zpIACC+1
    bmi L9D1D
    jsr L9E1D
    stx zpTYPE
    tay
    beq L9D39
    bmi L9D11
    lda zpIACC+3
    cmp zpIACC+2
    bne L9D0E
    tay
    beq L9D69
    cmp #$FF
    bne L9D0E
L9D69:
    eor zpIACC+1
    bmi L9D0E
    lda zpIACC+3
    pha
    jsr LAD71
    ldx #$39
    jsr LBE44
    jsr LBDEA
    pla
    eor zpIACC+3
    sta zp37
    jsr LAD71
    ldy #$00
    ldx #$00
    sty zp3F
    sty zp40
L9D8B:
    lsr zp3A

    ror zp39
    bcc L9DA6
    clc
    tya
    adc zpIACC
    tay
    txa
    adc zpIACC+1
    tax
    lda zp3F
    adc zpIACC+2
    sta zp3F
    lda zp40
    adc zpIACC+3
    sta zp40
L9DA6:
    asl zpIACC
    rol zpIACC+1
    rol zpIACC+2
    rol zpIACC+3
    lda zp39
    ora zp3A

    bne L9D8B
    sty zp3D
    stx zp3E
    lda zp37
    php

L9DBB:
    ldx #$3D
L9DBD:
    jsr LAF56
    plp
    bpl L9DC6
    jsr LAD93
L9DC6:
    ldx zpTYPE
    jmp L9DD4

; * <value>
; ---------
L9DCB:
    jmp L9D3C         ; Bounce back to multiply code


; Stack current value and continue in Evaluator Level 3
; ------------------------------------------------------- 
L9DCE:
    jsr LBD94

; Evaluator Level 3, * / DIV MOD
; ------------------------------
L9DD1:
    jsr L9E20         ; Call Evaluator Level 2, ^
L9DD4:
    cpx #'*'
    beq L9DCB         ; Jump with multiply
    cpx #'/'
    beq L9DE5         ; Jump with divide
    cpx #tknMOD
    beq L9E01         ; Jump with MOD
    cpx #tknDIV
    beq L9E0A
    rts               ; Jump with DIV

; / <value>
; ---------
L9DE5:
    tay
    jsr L92FD         ; Ensure current value is real
    jsr LBD51
    jsr L9E20         ; Stack float, call Evaluator Level 2
    stx zpTYPE
    tay
    jsr L92FD         ; Ensure current value is real
    jsr LBD7E
    jsr FXDIV         ; Unstack to FPTR, call divide routine
    ldx zpTYPE
    lda #$FF
    bne L9DD4; Set result, loop for more * / MOD DIV

; MOD <value>
; -----------
L9E01:
    jsr L99BE         ; Ensure current value is integer
    lda zp38
    php
    jmp L9DBB         ; Jump to MOD routine

; DIV <value>
; -----------
L9E0A:
    jsr L99BE         ; Ensure current value is integer
    rol zp39
    rol zp3A
    rol zp3B          ; Multiply IntA by 2
    rol zp3C
    bit zp37
    php
    ldx #$39
    jmp L9DBD         ; Jump to DIV routine


; Stack current integer and evaluate another Level 2
; --------------------------------------------------
L9E1D:
    jsr LBD94         ; Stack integer

; Evaluator Level 2, ^
; --------------------
L9E20:
    jsr LADEC         ; Call Evaluator Level 1, - + NOT function ( ) ? ! $ | "
L9E23:
    pha
L9E24:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y      ; Get character
    cmp #$20
    beq L9E24         ; Skip spaces
    tax
    pla
    cpx #'^'
    beq L9E35
    rts               ; Return if not ^

; ^ <value>
; ---------
L9E35:
    tay
    jsr L92FD         ; Ensure current value is a float
    jsr LBD51
    jsr L92FA         ; Stack float, evaluate a real
    lda zp30
    cmp #$87
    bcs L9E88
    jsr LA486
    bne L9E59
    jsr LBD7E
    jsr LA3B5
    lda zp4A
    jsr FIPOW
    lda #$FF
    bne L9E23         ; Set result=real, loop to check for more ^

L9E59:
    jsr LA381
    lda zpAESTKP
    sta zp4B
    lda zpAESTKP+1
    sta zp4C
    jsr LA3B5
    lda zp4A
    jsr FIPOW
L9E6C:
    jsr LA37D
    jsr LBD7E
    jsr LA3B5
    jsr LA801
    jsr LAAD1
    jsr LAA94
    jsr LA7ED
    jsr FMUL
    lda #$FF
    bne L9E23         ; Set result=real, loop to check for more ^

L9E88:
    jsr LA381
    jsr FONE
    bne L9E6C


; Convert number to hex string
; ----------------------------
FCONHX:
    tya
    bpl L9E96
    jsr LA3E4         ; Convert real to integer
L9E96:
    ldx #$00
    ldy #$00
L9E9A:
    lda zpIACC,Y        ; abs,y (!)
    pha               ; Expand four bytes into eight digits
    and #$0F
    sta zp3F,X
    pla
    lsr
    lsr
    lsr
    lsr
    inx
    sta zp3F,X
    inx
    iny
    cpy #$04
    bne L9E9A         ; Loop for four bytes
L9EB0:
    dex
    beq L9EB7         ; No digits left, output a single zero
    lda zp3F,X
    beq L9EB0         ; Skip leading zeros
L9EB7:
    lda zp3F,X
    cmp #$0A          ; Get byte from workspace
    bcc L9EBF
    adc #$06          ; Convert byte to hex
L9EBF:
    adc #'0'
    jsr LA066         ; Convert to digit and store in buffer
    dex
    bpl L9EB7
    rts               ; Loop for all digits

; Output nonzero real number
; --------------------------
L9EC8:
    bpl L9ED1         ; Jump forward if positive
    lda #'-'
    sta zp2E          ; A='-', clear sign flag
    jsr LA066         ; Add '-' to string buffer
L9ED1:
    lda zp30          ; Get exponent
    cmp #$81
    bcs L9F25         ; If m*2^1 or larger, number>=1, jump to output it
    jsr LA1F4         ; FloatA=FloatA*10
    dec zp49
    jmp L9ED1         ; Loop until number is >=1

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
FCON:
    ldx VARL_AT+2      ; Get format byte
    cpx #$03
    bcc L9EE8         ; If <3, ok - use it
    ldx #$00          ; If invalid, $00 for General format
L9EE8:
    stx zp37          ; Store format type
    lda VARL_AT+1
    beq L9EF5         ; If digits=0, jump to check format
    cmp #$0A
    bcs L9EF9         ; If 10+ digits, jump to use 10 digits
    bcc L9EFB         ; If <10 digits, use specified number
L9EF5:
    cpx #$02
    beq L9EFB         ; If fixed format, use zero digits

; STR$ enters here to use general format
; --------------------------------------
L9EF9:
    lda #$0A          ; Otherwise, default to ten digits
L9EFB:
    sta zp38
    sta zpFDIGS          ; Store digit length
    lda #$00
    sta zp36
    sta zp49          ; Set initial output length to 0, initial exponent to 0
    bit zpPRINTF
    bmi FCONHX         ; Jump for hex conversion if $15.b7 set
    tya
    bmi L9F0F
    jsr LA2BE         ; Convert integer to real
L9F0F:
    jsr LA1DA
    bne L9EC8         ; Get -1/0/+1 sign, jump if not zero to output nonzero number
    lda zp37
    bne L9F1D         ; If not General format, output fixed or exponential zero
    lda #'0'
    jmp LA066         ; Store single '0' into string buffer and return
L9F1D:
    jmp L9F9C         ; Jump to output zero in fixed or exponential format

L9F20:
    jsr FONE
    bne L9F34         ; FloatA=1.0

; FloatA now is >=1, check that it is <10
; ---------------------------------------
L9F25:
    cmp #$84
    bcc L9F39         ; Exponent<4, FloatA<10, jump to convert it
    bne L9F31         ; Exponent<>4, need to divide it
    lda zp31          ; Get mantissa top byte
    cmp #$A0
    bcc L9F39         ; Less than $A0, less than ten, jump to convert it
L9F31:
    jsr LA24D         ; FloatA=FloatA / 10
L9F34:
    inc zp49
    jmp L9ED1         ; Jump back to get the number >=1 again

; FloatA is now between 1 and 9.999999999
; ---------------------------------------
L9F39:
    lda zp35
    sta zpTYPE
    jsr LA385         ; Copy FloatA to FloatTemp at $27/$046C
    lda zpFDIGS
    sta zp38          ; Get number of digits
    ldx zp37          ; Get print format
    cpx #$02
    bne L9F5C         ; Not fixed format, jump to do exponent/general
    adc zp49
    bmi L9FA0
    sta zp38
    cmp #$0B
    bcc L9F5C
    lda #$0A
    sta zp38
    lda #$00
    sta zp37
L9F5C:
    jsr LA686         ; Clear FloatA
    lda #$A0
    sta zp31
    lda #$83
    sta zp30
    ldx zp38
    beq L9F71
L9F6B:
    jsr LA24D         ; FloatA=FloatA/10
    dex
    bne L9F6B
L9F71:
    jsr LA7F5         ; Point to $46C
    jsr LA34E         ; Unpack to FloatB
    lda zpTYPE
    sta zp42
    jsr LA50B; Add
L9F7E:
    lda zp30
    cmp #$84
    bcs L9F92
    ror zp31
    ror zp32
    ror zp33
    ror zp34
    ror zp35
    inc zp30
    bne L9F7E
L9F92:
    lda zp31
    cmp #$A0
    bcs L9F20
    lda zp38
    bne L9FAD

; Output zero in Exponent or Fixed format
; ---------------------------------------
L9F9C:
    cmp #$01
    beq L9FE6
L9FA0:
    jsr LA686         ; Clear FloatA
    lda #$00
    sta zp49
    lda zpFDIGS
    sta zp38
    inc zp38
L9FAD:
    lda #$01
    cmp zp37
    beq L9FE6
    ldy zp49
    bmi L9FC3
    cpy zp38
    bcs L9FE6
    lda #$00
    sta zp49
    iny
    tya
    bne L9FE6
L9FC3:
    lda zp37
    cmp #$02
    beq L9FCF
    lda #$01
    cpy #$FF
    bne L9FE6
L9FCF:
    lda #'0'
    jsr LA066         ; Output '0'
    lda #'.'
    jsr LA066         ; Output '.'
    lda #'0'          ; Prepare '0'
L9FDB:
    inc zp49
    beq L9FE4
    jsr LA066         ; Output
    bne L9FDB

L9FE4:
    lda #$80
L9FE6:
    sta zpFPRTWN
L9FE8:
    jsr LA040
    dec zpFPRTWN
    bne L9FF4
    lda #$2E
    jsr LA066
L9FF4:
    dec zp38
    bne L9FE8
    ldy zp37
    dey
    beq LA015
    dey
    beq LA011
    ldy zp36
LA002:
    dey
    lda ws+$0600,Y
    cmp #'0'
    beq LA002
    cmp #'.'
    beq LA00F
    iny
LA00F:
    sty zp36
LA011:
    lda zp49
    beq LA03F
LA015:
    lda #'E'
    jsr LA066         ; Output 'E'
    lda zp49
    bpl LA028
    lda #'-'
    jsr LA066         ; Output '-'
    sec
    lda #$00
    sbc zp49          ; Negate
LA028:
    jsr LA052
    lda zp37
    beq LA03F
    lda #$20
    ldy zp49
    bmi LA038
    jsr LA066
LA038:
    cpx #$00
    bne LA03F
    jmp LA066

LA03F:
    rts

LA040:
    lda zp31
    lsr
    lsr
    lsr
    lsr
    jsr LA064
    lda zp31
    and #$0F
    sta zp31
    jmp LA197

LA052:
    ldx #$FF
    sec
LA055:
    inx
    sbc #$0A
    bcs LA055
    adc #$0A
    pha
    txa
    beq LA063
    jsr LA064
LA063:
    pla
LA064:
    ora #'0'

; Store character in string buffer
; --------------------------------
LA066:
    stx zp3B
    ldx zp36
    sta ws+$0600,X    ; Store character
    ldx zp3B
    inc zp36
    rts               ; Increment string length

LA072:
    clc
    stx zp35
    jsr LA1DA
    lda #$FF
    rts

; Scan decimal number
; -------------------
LA07B:
    ldx #$00
    stx zp31
    stx zp32          ; Clear FloatA
    stx zp33
    stx zp34
    stx zp35
    stx zp48          ; Clear 'Decimal point' flag
    stx zp49          ; Set exponent to zero
    cmp #'.'
    beq LA0A0         ; Leading decimal point
    cmp #'9'+1
    bcs LA072         ; Not a decimal digit, finish
    sbc #'0'-1
    bmi LA072         ; Convert to binary, if not digit finish
    sta zp35          ; Store digit
LA099:
    iny
    lda (zpAELINE),Y      ; Get next character
    cmp #'.'
    bne LA0A8         ; Not decimal point
LA0A0:
    lda zp48
    bne LA0E8         ; Already got decimal point, 
    inc zp48
    bne LA099         ; Set Decimal Point flag, loop for next
LA0A8:
    cmp #'E'
    beq LA0E1         ; Jump to scan exponent
    cmp #'9'+1
    bcs LA0E8         ; Not a digit, jump to finish
    sbc #'0'-1
    bcc LA0E8         ; Not a digit, jump to finish
    ldx zp31          ; Get mantissa top byte
    cpx #$18
    bcc LA0C2         ; If <25, still small enough to add to
    ldx zp48
    bne LA099         ; Decimal point found, skip digits to end of number
    inc zp49
    bcs LA099         ; No decumal point, increment exponent and skip digits

LA0C2:
    ldx zp48
    beq LA0C8 
    dec zp49          ; Decimal point found, decrement exponent
LA0C8:
    jsr LA197         ; Multiply FloatA by 10
    adc zp35
    sta zp35          ; Add digit to mantissa low byte
    bcc LA099         ; No overflow
    inc zp34
    bne LA099         ; Add carry through mantissa
    inc zp33
    bne LA099
    inc zp32
    bne LA099
    inc zp31
    bne LA099         ; Loop to check next digit

; Deal with Exponent in scanned number
; ------------------------------------
LA0E1:
    jsr LA140         ; Scan following number
    adc zp49
    sta zp49          ; Add to current exponent

; End of number found
; -------------------
LA0E8:
    sty zpAECUR          ; Store PtrB offset
    lda zp49
    ora zp48          ; Check exponent and 'decimal point' flag
    beq LA11F         ; No exponent, no decimal point, return integer
    jsr LA1DA
    beq LA11B
LA0F5:
    lda #$A8
    sta zp30
    lda #$00
    sta zp2F
    sta zp2E
    jsr LA303
    lda zp49
    bmi LA111
    beq LA118
LA108:
    jsr LA1F4
    dec zp49
    bne LA108
    beq LA118
LA111:
    jsr LA24D
    inc zp49
    bne LA111
LA118:
    jsr LA65C
LA11B:
    sec
    lda #$FF
    rts

LA11F:
    lda zp32
    sta zpIACC+3
    and #$80
    ora zp31
    bne LA0F5
    lda zp35
    sta zpIACC
    lda zp34
    sta zpIACC+1
    lda zp33
    sta zpIACC+2
    lda #$40
    sec
    rts

LA139:
    jsr LA14B         ; Scan following number
    eor #$FF
    sec
    rts               ; Negate it, return CS=Ok

; Scan exponent, allows E E+ E- followed by one or two digits
; -----------------------------------------------------------
LA140:
    iny
    lda (zpAELINE),Y      ; Get next character
    cmp #'-'
    beq LA139         ; If '-', jump to scan and negate
    cmp #'+'
    bne LA14E         ; If '+', just step past
LA14B:
    iny
    lda (zpAELINE),Y      ; Get next character
LA14E:
    cmp #'9'+1
    bcs LA174         ; Not a digit, exit with CC and A=0
    sbc #'0'-1
    bcc LA174         ; Not a digit, exit with CC and A=0
    sta zp4A          ; Store exponent digit
    iny
    lda (zpAELINE),Y      ; Get next character
    cmp #'9'+1
    bcs LA170         ; Not a digit, exit with CC and A=exp
    sbc #'0'-1
    bcc LA170         ; Not a digit, exit with CC and A=exp
    iny
    sta zp43          ; Step past digit, store current digit
    lda zp4A          ; Get current exponent
    asl
    asl
    adc zp4A          ; exp=exp*10
    asl
    adc zp43          ; exp=exp*10+digit
    rts

LA170:
    lda zp4A
    clc
    rts               ; Get exp and return CC=Ok

LA174:
    lda #$00
    clc
    rts               ; Return exp=0 and CC=Ok

LA178:
    lda zp35
    adc zp42
    sta zp35
    lda zp34
    adc zp41
    sta zp34
    lda zp33
    adc zp40
    sta zp33
    lda zp32
    adc zp3F
    sta zp32
    lda zp31
    adc zp3E
    sta zp31
    rts

LA197:
    pha
    ldx zp34
    lda zp31
    pha
    lda zp32
    pha
    lda zp33
    pha
    lda zp35
    asl
    rol zp34
    rol zp33
    rol zp32
    rol zp31
    asl
    rol zp34
    rol zp33
    rol zp32
    rol zp31
    adc zp35
    sta zp35
    txa
    adc zp34
    sta zp34
    pla
    adc zp33
    sta zp33
    pla
    adc zp32
    sta zp32
    pla
    adc zp31
    asl zp35
    rol zp34
    rol zp33
    rol zp32
    rol
    sta zp31
    pla
    rts

LA1DA:
    lda zp31
    ora zp32
    ora zp33
    ora zp34
    ora zp35
    beq LA1ED
    lda zp2E
    bne LA1F3
    lda #$01
    rts

LA1ED:
    sta zp2E
    sta zp30
    sta zp2F
LA1F3:
    rts

LA1F4:
    clc
    lda zp30
    adc #$03
    sta zp30
    bcc LA1FF
    inc zp2F
LA1FF:
    jsr LA21E
    jsr LA242
    jsr LA242
LA208:
    jsr LA178
LA20B:
    bcc LA21D
    ror zp31
    ror zp32
    ror zp33
    ror zp34
    ror zp35
    inc zp30
    bne LA21D
    inc zp2F
LA21D:
    rts

LA21E:
    lda zp2E
LA220:
    sta zp3B
    lda zp2F
    sta zp3C
    lda zp30
    sta zp3D
    lda zp31
    sta zp3E
    lda zp32
    sta zp3F
    lda zp33
    sta zp40
    lda zp34
    sta zp41
    lda zp35
    sta zp42
    rts

LA23F:
    jsr LA21E
LA242:
    lsr zp3E
    ror zp3F
    ror zp40
    ror zp41
    ror zp42
    rts

LA24D:
    sec
    lda zp30
    sbc #$04
    sta zp30
    bcs LA258
    dec zp2F
LA258:
    jsr LA23F
    jsr LA208
    jsr LA23F
    jsr LA242
    jsr LA242
    jsr LA242
    jsr LA208
    lda #$00
    sta zp3E
    lda zp31
    sta zp3F
    lda zp32
    sta zp40
    lda zp33
    sta zp41
    lda zp34
    sta zp42
    lda zp35
    rol
    jsr LA208
    lda #$00
    sta zp3E
    sta zp3F
    lda zp31
    sta zp40
    lda zp32
    sta zp41
    lda zp33
    sta zp42
    lda zp34
    rol
    jsr LA208
    lda zp32
    rol
    lda zp31
LA2A4:
    adc zp35
    sta zp35
    bcc LA2BD
    inc zp34
    bne LA2BD
    inc zp33
    bne LA2BD
    inc zp32
    bne LA2BD
    inc zp31
    bne LA2BD
    jmp LA20B

LA2BD:
    rts

LA2BE:
    ldx #$00
    stx zp35
    stx zp2F
    lda zpIACC+3
    bpl LA2CD
    jsr LAD93
    ldx #$FF
LA2CD:
    stx zp2E
    lda zpIACC
    sta zp34
    lda zpIACC+1
    sta zp33
    lda zpIACC+2
    sta zp32
    lda zpIACC+3
    sta zp31
    lda #$A0
    sta zp30
    jmp LA303

LA2E6:
    sta zp2E
    sta zp30
    sta zp2F
LA2EC:
    rts

LA2ED:
    pha
    jsr LA686
    pla
    beq LA2EC
    bpl LA2FD
    sta zp2E
    lda #$00
    sec
    sbc zp2E
LA2FD:
    sta zp31
    lda #$88
    sta zp30
LA303:
    lda zp31
    bmi LA2EC
    ora zp32
    ora zp33
    ora zp34
    ora zp35
    beq LA2E6
    lda zp30
LA313:
    ldy zp31
    bmi LA2EC
    bne LA33A
    ldx zp32
    stx zp31
    ldx zp33
    stx zp32
    ldx zp34
    stx zp33
    ldx zp35
    stx zp34
    sty zp35
    sec
    sbc #$08
    sta zp30
    bcs LA313
    dec zp2F
    bcc LA313
LA336:
    ldy zp31
    bmi LA2EC
LA33A:
    asl zp35
    rol zp34
    rol zp33
    rol zp32
    rol zp31
    sbc #$00
    sta zp30
    bcs LA336
    dec zp2F
    bcc LA336
LA34E:
    ldy #$04
    lda (zp4B),Y
    sta zp41
    dey
    lda (zp4B),Y
    sta zp40
    dey
    lda (zp4B),Y
    sta zp3F
    dey
    lda (zp4B),Y
    sta zp3B
    dey
    sty zp42
    sty zp3C
    lda (zp4B),Y
    sta zp3D
    ora zp3B
    ora zp3F
    ora zp40
    ora zp41
    beq LA37A
    lda zp3B
    ora #$80
LA37A:
    sta zp3E
    rts

; ----------------------------------------------------------------------------

LA37D:
    lda #$71            ; LSB of FWSB = VARL ($0400) + $71, FP TEMP2
    bne LA387

LA381:
    lda #$76            ; LSB of FWSC = VARL ($0400) + $76, FP TEMP3
    bne LA387

LA385:
    lda #$6C            ; LSB of FWSA = VARL ($0400) + $6c, FP TEMP1

LA387:
    sta zp4B
    lda #$04+(ws/256)   ; MSB of all FWS / FP TEMP variables
    sta zp4C

LA38D:
    ldy #$00
    lda zp30
    sta (zp4B),Y
    iny
    lda zp2E            ; tidy up sign bit
    and #$80
    sta zp2E
    lda zp31
    and #$7F
    ora zp2E
    sta (zp4B),Y
    lda zp32
    iny
    sta (zp4B),Y
    lda zp33
    iny
    sta (zp4B),Y
    lda zp34
    iny
    sta (zp4B),Y
    rts

LA3B2:
    jsr LA7F5
LA3B5:
    ldy #$04
    lda (zp4B),Y
    sta zp34
    dey
    lda (zp4B),Y
    sta zp33
    dey
    lda (zp4B),Y
    sta zp32
    dey
    lda (zp4B),Y
    sta zp2E
    dey
    lda (zp4B),Y
    sta zp30
    sty zp35
    sty zp2F
    ora zp2E
    ora zp32
    ora zp33
    ora zp34
    beq LA3E1
    lda zp2E
    ora #$80
LA3E1:
    sta zp31
    rts

; Convert real to integer
; =======================
LA3E4:
    jsr LA3FE         ; Convert real to integer
LA3E7:
    lda zp31
    sta zpIACC+3          ; Copy to Integer Accumulator
    lda zp32
    sta zpIACC+2
    lda zp33
    sta zpIACC+1
    lda zp34
    sta zpIACC
    rts

LA3F8:
    jsr LA21E         ; Copy FloatA to FloatB
    jmp LA686         ; Set FloatA to zero and return

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
    lda zp30
    bpl LA3F8         ; Exponent<$80, number<1, jump to return 0
    jsr LA453         ; Set $3B-$42 to zero
    jsr LA1DA
    bne LA43C
    beq LA468

LA40C:
    lda zp30          ; Get exponent
    cmp #$A0
    bcs LA466         ; Exponent is +32, float has been denormalised to an integer
    cmp #$99
    bcs LA43C         ; Loop to keep dividing
    adc #$08
    sta zp30          ; Increment exponent by 8
    lda zp40
    sta zp41
    lda zp3F
    sta zp40
    lda zp3E
    sta zp3F
    lda zp34
    sta zp3E
    lda zp33
    sta zp34          ; Divide mantissa by 2^8
    lda zp32
    sta zp33
    lda zp31
    sta zp32
    lda #$00
    sta zp31
    beq LA40C         ; Loop to keep dividing

LA43C:
    lsr zp31
    ror zp32
    ror zp33
    ror zp34
    ror zp3E
    ror zp3F
    ror zp40
    ror zp41
    inc zp30
    bne LA40C
LA450:
    jmp LA66C

LA453:
    lda #$00
    sta zp3B
    sta zp3C
    sta zp3D
    sta zp3E
    sta zp3F
    sta zp40
    sta zp41
    sta zp42
    rts

LA466:
    bne LA450         ; Exponent>32, jump to 'Too big' error
LA468:
    lda zp2E
    bpl LA485         ; If positive, jump to return
LA46C:
    sec               ; Negate the mantissa to get integer
    lda #$00
    sbc zp34
    sta zp34
    lda #$00
    sbc zp33
    sta zp33
    lda #$00
    sbc zp32
    sta zp32
    lda #$00
    sbc zp31
    sta zp31
LA485:
    rts

LA486:
    lda zp30
    bmi LA491
    lda #$00
    sta zp4A
    jmp LA1DA

LA491:
    jsr LA3FE
    lda zp34
    sta zp4A
    jsr LA4E8
    lda #$80
    sta zp30
    ldx zp31
    bpl LA4B3
    eor zp2E
    sta zp2E
    bpl LA4AE
    inc zp4A
    jmp LA4B0

LA4AE:
    dec zp4A
LA4B0:
    jsr LA46C
LA4B3:
    jmp LA303

LA4B6:
    inc zp34
    bne LA4C6
    inc zp33
    bne LA4C6
    inc zp32
    bne LA4C6
    inc zp31
    beq LA450
LA4C6:
    rts

LA4C7:
    jsr LA46C
    jsr LA4B6
    jmp LA46C

LA4D0:
    jsr LA4FD
    jmp LAD7E

LA4D6:
    jsr LA34E
    jsr LA38D
LA4DC:
    lda zp3B
    sta zp2E
    lda zp3C
    sta zp2F
    lda zp3D
    sta zp30
LA4E8:
    lda zp3E
    sta zp31
    lda zp3F
    sta zp32
    lda zp40
    sta zp33
    lda zp41
    sta zp34
    lda zp42
    sta zp35
LA4FC:
    rts

LA4FD:
    jsr LAD7E
LA500:
    jsr LA34E
    beq LA4FC
LA505:
    jsr LA50B
    jmp LA65C

LA50B:
    jsr LA1DA
    beq LA4DC
    ldy #$00
    sec
    lda zp30
    sbc zp3D
    beq LA590
    bcc LA552
    cmp #$25
    bcs LA4FC
    pha
    and #$38
    beq LA53D
    lsr
    lsr
    lsr
    tax
LA528:
    lda zp41
    sta zp42
    lda zp40
    sta zp41
    lda zp3F
    sta zp40
    lda zp3E
    sta zp3F
    sty zp3E
    dex
    bne LA528
LA53D:
    pla
    and #$07
    beq LA590
    tax
LA543:
    lsr zp3E
    ror zp3F
    ror zp40
    ror zp41
    ror zp42
    dex
    bne LA543
    beq LA590
LA552:
    sec
    lda zp3D
    sbc zp30
    cmp #$25
    bcs LA4DC
    pha
    and #$38
    beq LA579
    lsr
    lsr
    lsr
    tax
LA564:
    lda zp34
    sta zp35
    lda zp33
    sta zp34
    lda zp32
    sta zp33
    lda zp31
    sta zp32
    sty zp31
    dex
    bne LA564
LA579:
    pla
    and #$07
    beq LA58C
    tax
LA57F:
    lsr zp31
    ror zp32
    ror zp33
    ror zp34
    ror zp35
    dex
    bne LA57F
LA58C:
    lda zp3D
    sta zp30
LA590:
    lda zp2E
    eor zp3B
    bpl LA5DF
    lda zp31
    cmp zp3E
    bne LA5B7
    lda zp32
    cmp zp3F
    bne LA5B7
    lda zp33
    cmp zp40
    bne LA5B7
    lda zp34
    cmp zp41
    bne LA5B7
    lda zp35
    cmp zp42
    bne LA5B7
    jmp LA686

LA5B7:
    bcs LA5E3
    sec
    lda zp42
    sbc zp35
    sta zp35
    lda zp41
    sbc zp34
    sta zp34
    lda zp40
    sbc zp33
    sta zp33
    lda zp3F
    sbc zp32
    sta zp32
    lda zp3E
    sbc zp31
    sta zp31
    lda zp3B
    sta zp2E
    jmp LA303

LA5DF:
    clc
    jmp LA208

LA5E3:
    sec
    lda zp35
    sbc zp42
    sta zp35
    lda zp34
    sbc zp41
    sta zp34
    lda zp33
    sbc zp40
    sta zp33
    lda zp32
    sbc zp3F
    sta zp32
    lda zp31
    sbc zp3E
    sta zp31
    jmp LA303

LA605:
    rts

LA606:
    jsr LA1DA
    beq LA605
    jsr LA34E
    bne LA613
    jmp LA686

LA613:
    clc
    lda zp30
    adc zp3D
    bcc LA61D
    inc zp2F
    clc
LA61D:
    sbc #$7F
    sta zp30
    bcs LA625
    dec zp2F
LA625:
    ldx #$05
    ldy #$00
LA629:
    lda zp30,X
    sta zp42,X
    sty zp30,X
    dex
    bne LA629
    lda zp2E
    eor zp3B
    sta zp2E
    ldy #$20
LA63A:
    lsr zp3E
    ror zp3F
    ror zp40
    ror zp41
    ror zp42
    asl zp46
    rol zp45
    rol zp44
    rol zp43
    bcc LA652
    clc
    jsr LA178
LA652:
    dey
    bne LA63A
    rts

FMUL:
    jsr LA606       ; IFMUL
LA659:
    jsr LA303       ; FNRM
LA65C:
    lda zp35
    cmp #$80
    bcc LA67C
    beq LA676
    lda #$FF
    jsr LA2A4       ; FPLNF
    jmp LA67C       ; FTRNDZ

LA66C:
    brk
    dta $14
    .if foldup == 1
        dta 'TOO BIG'
    .else
        dta 'Too big'
    .endif
    brk
LA676:
    lda zp34
    ora #$01
    sta zp34
LA67C:
    lda #$00
    sta zp35
    lda zp2F
    beq LA698
    bpl LA66C
LA686:
    lda #$00
    sta zp2E
    sta zp2F
    sta zp30
    sta zp31
    sta zp32
    sta zp33
    sta zp34
    sta zp35
LA698:
    rts

; ----------------------------------------------------------------------------

.proc FONE
    jsr LA686           ; FCLR
    ldy #$80
    sty zp31
    iny
    sty zp30
    tya
    rts                 ; always return with !Z
.endp

; ----------------------------------------------------------------------------

.proc FRECIP
    jsr LA385
    jsr FONE
    bne LA6E7           ; branch always, FONE returns with !Z
.endp

; ----------------------------------------------------------------------------

.proc FXDIV
    jsr LA1DA
    beq FDIVZ
    jsr LA21E
    jsr LA3B5
    bne LA6F1
    rts
.endp

.proc FDIVZ
    jmp ZDIVOR      ; Divide by zero error
.endp

; ----------------------------------------------------------------------------

; =TAN numeric
; ============
LA6BE:
    jsr L92FA
    jsr LA9D3
    lda zp4A
    pha
    jsr LA7E9
    jsr LA38D
    inc zp4A
    jsr LA99E
    jsr LA7E9
    jsr LA4D6
    pla
    sta zp4A
    jsr LA99E
    jsr LA7E9
    jsr LA6E7
    lda #$FF
    rts

LA6E7:
    jsr LA1DA
    beq LA698
    jsr LA34E
    beq FDIVZ
LA6F1:
    lda zp2E
    eor zp3B
    sta zp2E
    sec
    lda zp30
    sbc zp3D
    bcs LA701
    dec zp2F
    sec
LA701:
    adc #$80
    sta zp30
    bcc LA70A
    inc zp2F
    clc
LA70A:
    ldx #$20
LA70C:
    bcs LA726
    lda zp31
    cmp zp3E
    bne LA724
    lda zp32
    cmp zp3F
    bne LA724
    lda zp33
    cmp zp40
    bne LA724
    lda zp34
    cmp zp41
LA724:
    bcc LA73F
LA726:
    lda zp34
    sbc zp41
    sta zp34
    lda zp33
    sbc zp40
    sta zp33
    lda zp32
    sbc zp3F
    sta zp32
    lda zp31
    sbc zp3E
    sta zp31
    sec
LA73F:
    rol zp46
    rol zp45
    rol zp44
    rol zp43
    asl zp34
    rol zp33
    rol zp32
    rol zp31
    dex
    bne LA70C
    ldx #$07
LA754:
    bcs LA76E
    lda zp31
    cmp zp3E
    bne LA76C
    lda zp32
    cmp zp3F
    bne LA76C
    lda zp33
    cmp zp40
    bne LA76C
    lda zp34
    cmp zp41
LA76C:
    bcc LA787
LA76E:
    lda zp34
    sbc zp41
    sta zp34
    lda zp33
    sbc zp40
    sta zp33
    lda zp32
    sbc zp3F
    sta zp32
    lda zp31
    sbc zp3E
    sta zp31
    sec
LA787:
    rol zp35
    asl zp34
    rol zp33
    rol zp32
    rol zp31
    dex
    bne LA754
    asl zp35
    lda zp46
    sta zp34
    lda zp45
    sta zp33
    lda zp44
    sta zp32
    lda zp43
    sta zp31
    jmp LA659

LA7A9:
    brk
    dta $15
    .if foldup == 1
        dta '-VE ROOT'
    .else
        dta '-ve root'
    .endif
    brk

; =SQR numeric
; ============
LA7B4:
    jsr L92FA
LA7B7:
    jsr LA1DA
    beq LA7E6
    bmi LA7A9
    jsr LA385
    lda zp30
    lsr
    adc #$40
    sta zp30
    lda #$05
    sta zp4A
    jsr LA7ED
LA7CF:
    jsr LA38D
    lda #$6C
    sta zp4B
    jsr FXDIV
    lda #$71
    sta zp4B
    jsr LA500
    dec zp30
    dec zp4A
    bne LA7CF
LA7E6:
    lda #$FF
    rts

; Point $4B/C to a floating point temp
; ------------------------------------
LA7E9:
    lda #$7B
    bne LA7F7         ; ws+$047B-7F FPTEMP4
LA7ED:
    lda #$71
    bne LA7F7         ; ws+$0471-75 FPTEMP2
LA7F1:
    lda #$76
    bne LA7F7         ; ws+$0476-7A FPTEMP3
LA7F5:
    lda #$6C          ; ws+$046C-70 FPTEMP1


LA7F7:
    sta zp4B          ; $4B/C=>FPTEMP
    lda #$04+(ws/256)
    sta zp4C
    rts

; =LN numeric
; ===========
LA7FE:
    jsr L92FA
LA801:
    jsr LA1DA
    beq LA808
    bpl LA814
LA808:
    .if foldup == 0
        brk
        dta $16
        dta 'Log range'
        brk
    .elseif foldup != 0
        brk
        dta $16
        dta tknLOG
        dta ' RANGE'
        brk
    .endif
LA814:
    jsr LA453
    ldy #$80
    sty zp3B
    sty zp3E
    iny
    sty zp3D
    ldx zp30
    beq LA82A
    lda zp31
    cmp #$B5
    bcc LA82C
LA82A:
    inx
    dey
LA82C:
    txa
    pha
    sty zp30
    jsr LA505
    lda #$7B
    jsr LA387
    lda #<FLOGTC
    ldy #>FLOGTC
    jsr FCF
    jsr LA7E9
    jsr FMUL
    jsr FMUL
    jsr LA500
    jsr LA385
    pla
    sec
    sbc #$81
    jsr LA2ED
    lda #<LOGTWO
    sta zp4B
    lda #>LOGTWO
    sta zp4C
    jsr FMUL
    jsr LA7F5
    jsr LA500
    lda #$FF
    rts

    .if version < 3
RPLN10:
        dta $7f, $5e, $5b, $d8, $aa     ; 0.43429448 = LOG10(e)
    .endif

LOGTWO:
    dta $80, $31, $72, $17, $f8         ; LN(2.0)

FLOGTC:
    dta $06                             ; Length - 1
    dta $7a, $12, $38, $a5, $0B         ; 0.00892464
    dta $88, $79, $0E, $9f, $f3         ; 249.05712813
    dta $7c, $2a, $ac, $3f, $b5         ; 0.04166818
    dta $86, $34, $01, $a2, $7a         ; 45.00159636
    dta $7f, $63, $8e, $37, $ec         ; 0.44444442
    dta $82, $3f, $ff, $ff, $c1         ; 2.99999994
    dta $7f ,$ff, $ff, $ff, $ff         ; -0.50000000

    .if .hi(*) != .hi(FLOGTC)
        .error "Table FLOGTC crosses page!"
    .endif

; FCF - Evaluates a rational function of the form
;       A0 + X/(A1+X/(A2+X/ ...
;       i.e. a continued fraction.
;       It takes a table of the form:
;       <BYTE N> <AN> ... <A0>
;       where AN through A0 are floating point values.
;       Sam demands that no table cross a page!

FCF:
    sta zpCOEFP
    sty zpCOEFP+1
    jsr LA385
    ldy #$00
    lda (zpCOEFP),Y
    sta zp48
    inc zpCOEFP
    bne LA8AA
    inc zpCOEFP+1
LA8AA:
    lda zpCOEFP
    sta zp4B
    lda zpCOEFP+1
    sta zp4C
    jsr LA3B5
LA8B5:
    jsr LA7F5
    jsr FXDIV
    clc
    lda zpCOEFP
    adc #$05
    sta zpCOEFP
    sta zp4B
    lda zpCOEFP+1
    adc #$00
    sta zpCOEFP+1
    sta zp4C
    jsr LA500
    dec zp48
    bne LA8B5
    rts

; =ACS numeric
; ============
LA8D4:
    jsr LA8DA
    jmp LA927

; =ASN numeric
; ============
LA8DA:
    jsr L92FA
    jsr LA1DA
    bpl LA8EA
    lsr zp2E
    jsr LA8EA
    jmp LA916

LA8EA:
    jsr LA381
    jsr LA9B1
    jsr LA1DA
    beq LA8FE
    jsr LA7F1
    jsr FXDIV
    jmp LA90A

LA8FE:
    jsr LAA55
    jsr LA3B5
LA904:
    lda #$FF
    rts

; =ATN numeric
; ============
LA907:
    jsr L92FA
LA90A:
    jsr LA1DA
    beq LA904
    bpl LA91B
    lsr zp2E
    jsr LA91B
LA916:
    lda #$80
    sta zp2E
    rts

LA91B:
    lda zp30
    cmp #$81
    bcc LA936
    jsr FRECIP
    jsr LA936
LA927:
    jsr LAA48
    jsr LA500
    jsr LAA4C
    jsr LA500
    jmp LAD7E

LA936:
    lda zp30
    cmp #$73
    bcc LA904
    jsr LA381
    jsr LA453
    lda #$80
    sta zp3D
    sta zp3E
    sta zp3B
    jsr LA505
    lda #<FATANC
    ldy #>FATANC
    jsr FCF
    jsr LAAD1
    lda #$FF
    rts

FATANC:
    dta $09                         ; Length - 1
    dta $85, $a3, $59, $e8, $67     ; -20.41890030
    dta $80, $1c, $9d, $07, $36     ; 0.61177106
    dta $80, $57, $bb, $78, $df     ; 0.84270435
    dta $80, $ca, $9a, $0e, $83     ; -0.79141322
    dta $84, $8c, $bb, $ca, $6e     ; -8.79584735
    dta $81, $95, $96, $06, $de     ; -1.16864096
    dta $81, $0a, $c7, $6c, $52     ; 1.08421091
    dta $7f, $7d, $ad, $90, $a1     ; 0.49546482
    dta $82, $fb, $62, $57, $2f     ; -3.92787723
    dta $80, $6d, $63, $38, $2C     ; 0.92729522

    .if .hi(*) != .hi(FATANC)
        .error "Table FATANC crosses page!"
    .endif

; ----------------------------------------------------------------------------

; =COS numeric
; ============
LA98D:
    jsr L92FA         ; Evaluate float
    jsr LA9D3
    inc zp4A
    jmp LA99E

; =SIN numeric
; ============
LA998:
    jsr L92FA
    jsr LA9D3
LA99E:
    lda zp4A
    and #$02
    beq LA9AA
    jsr LA9AA
    jmp LAD7E

LA9AA:
    lsr zp4A
    bcc LA9C3
    jsr LA9C3
LA9B1:
    jsr LA385
    jsr FMUL
    jsr LA38D
    jsr FONE
    jsr LA4D0
    jmp LA7B7

LA9C3:
    jsr LA381
    jsr FMUL
    lda #<FSINC
    ldy #>FSINC
    jsr FCF
    jmp LAAD1

LA9D3:
    lda zp30
    cmp #$98
    bcs LAA38
    jsr LA385
    jsr LAA55
    jsr LA34E
    lda zp2E
    sta zp3B
    dec zp3D
    jsr LA505
    jsr LA6E7
    jsr LA3FE
    lda zp34
    sta zp4A
    ora zp33
    ora zp32
    ora zp31
    beq LAA35
    lda #$A0
    sta zp30
    ldy #$00
    sty zp35
    lda zp31
    sta zp2E
    bpl LAA0E
    jsr LA46C
LAA0E:
    jsr LA303
    jsr LA37D
    jsr LAA48
    jsr FMUL
    jsr LA7F5
    jsr LA500
    jsr LA38D
    jsr LA7ED
    jsr LA3B5
    jsr LAA4C
    jsr FMUL
    jsr LA7F5
    jmp LA500

LAA35:
    jmp LA3B2

LAA38:
    brk
    dta $17
    .if foldup == 1
        dta 'ACCURACY LOST'
    .else
        dta 'Accuracy lost'
    .endif
    brk
LAA48:
    lda #<HPIHI
    .if (HPIHI & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    bne LAA4E
LAA4C:
    lda #<HPILO
LAA4E:
    sta zp4B
    lda #>HPIHI
    sta zp4C
    rts

LAA55:
    lda #<HALFPI
    .if (HALFPI & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    bne LAA4E         ; AA57= D0 F5       Pu

; HPIHI + HPILO = -PI/2

HPIHI:
    dta $81, $c9, $10, $00, $00         ; -1.57080078
HPILO:
    dta $6f, $15, $77, $7a, $61         ; 0.00000445
HALFPI:
    dta $81, $49, $0f, $da, $a2         ; 1.57079633 = PI/2
FPIs18:
    dta $7b, $0e, $fa, $35, $12         ; 0.01745329 = PI/180 (1 deg in rads)
F180sP:
    dta $86, $65, $2e, $e0, $d3         ; 57.29577951 = 180/PI (1 rad in degs)
    .if version >= 3
RPLN10:
        dta $7f, $5e, $5b, $d8, $aa     ; 0.43429448 = LOG10(e)
    .endif

    .if .hi(*) != .hi(HPIHI)
        .error "PI table crosses page!"
    .endif

FSINC:
    dta $05                         ; Length -1
    dta $84, $8a, $ea, $0c, $1b     ; -8.68214045
    dta $84, $1a, $be, $bb, $2b     ; 9.67156522
    dta $84, $37, $45, $55, $ab     ; 11.45442740
    dta $82, $d5, $55, $57, $7c     ; -3.33333385
    dta $83, $c0, $00, $00, $05     ; -6.00000001
    dta $81, $00, $00, $00, $00     ; 1.00000000

    .if .hi(*) != .hi(FSINC)
        .error "Table FSINC crosses page!"
    .endif

; ----------------------------------------------------------------------------

; = EXP numeric
; =============
LAA91:
    jsr L92FA
LAA94:
    lda zp30
    cmp #$87
    bcc LAAB8
    bne LAAA2
LAA9C:
    ldy zp31
    cpy #$B3
    bcc LAAB8
LAAA2:
    lda zp2E
    bpl LAAAC
    jsr LA686
    lda #$FF
    rts

LAAAC:
    .if foldup == 0
        brk
        dta $18
        dta 'Exp range'
        brk
    .elseif foldup != 0
        brk
        dta $18
        dta tknEXP
        dta ' RANGE'
        brk
    .endif
LAAB8:
    jsr LA486
    jsr LAADA
    jsr LA381
    lda #<FNUME
    sta zp4B
    lda #>FNUME
    sta zp4C
    jsr LA3B5
    lda zp4A
    jsr FIPOW
LAAD1:
    jsr LA7F1
    jsr FMUL
    lda #$FF
    rts

LAADA:
    lda #<FEXPCO
    ldy #>FEXPCO
    jsr FCF
    lda #$FF
    rts

FNUME:
    dta $82, $2d, $f8, $54, $58     ; 2.71828183 = e

FEXPCO:
    dta $07                         ; Length - 1
    dta $83, $e0, $20, $86, $5b     ; -7.00397032
    dta $82, $80, $53, $93, $b8     ; -2.00510114
    dta $83, $20, $00, $06, $a1     ; 5.00000316
    dta $82, $00, $00, $21, $63     ; 2.00000796
    dta $82, $c0, $00, $00, $02     ; -3.00000000
    dta $82, $80, $00, $00, $0c     ; -2.00000001
    dta $81, $00, $00, $00, $00     ; 1.00000000
    dta $81, $00, $00, $00, $00     ; 1.00000000

; Later versions of BBC BASIC enforce this, but it fails for BASIC II/III
;    .if .hi(*) != .hi(FEXPCO)
;        .error "Table FEXPCO crosses page!"
;    .endif

; ----------------------------------------------------------------------------

; FIPOW - Computes X**N where X is passed in FP Accu, N is a one byte
;         signed integer passed in A

.proc FIPOW
    tax
    bpl FIPOWA
    dex
    txa
    eor #$FF        ; complement
    pha
    jsr FRECIP
    pla             ; recover exponent
FIPOWA:
    pha
    jsr LA385       ; STARGA
    jsr FONE
FIPOWB:
    pla
    beq FIPOWZ      ; exit condition
    sec
    sbc #$01
    pha
    jsr FMUL
    jmp FIPOWB

FIPOWZ:
    rts
.endp

; ----------------------------------------------------------------------------

; =ADVAL numeric - Call OSBYTE to read buffer/device
; ==================================================
LAB33:
    jsr L92E3         ; Evaluate integer
    ldx zpIACC
    lda #$80          ; X=low byte, A=$80 for ADVAL
; 
; WRONG in original disassembly
;    .if 0            ; FALSE
;        JSR OSBYTE
;    .endif
;    .ifdef MOS_BBC
;        JSR LAFB2
;    .endif
    .ifdef MOS_BBC
        .if .def TARGET_C64
            jsr LAFB2
        .else
            jsr OSBYTE
        .endif
    .endif
    txa
    .if version < 3
        jmp LAEEA
    .elseif version >= 3
        jmp XAED5
    .endif

    .if version < 3
; =POINT(numeric, numeric)
; ========================
LAB41:
        jsr L92DD
        jsr LBD94
        jsr L8AAE
        jsr LAE56
        jsr L92F0
        lda zpIACC
        pha
        lda zpIACC+1
        pha
        jsr LBDEA
        pla
        sta zpIACC+3
        pla
        sta zpIACC+2
        ldx #$2A
        lda #$09
        jsr OSWORD
        lda zp2E
        bmi LAB9D
        jmp LAED8
    .elseif version >= 3
; =NOT
; ====
XAB5B:
        jsr L92E3
XAB5E:
        ldx #$03
XAB60:
        lda zpIACC,X
        eor #$FF
        sta zpIACC,X
        dex
        bpl XAB60
        lda #$40
        rts
    .endif

; =POS
; ====
LAB6D:
    .if version < 3
        lda #$86
        jsr OSBYTE
        txa
        jmp LAED8
    .elseif version >= 3
        jsr LAB76
        stx zpIACC
        rts
    .endif

; =VPOS
; =====
LAB76:
    lda #$86
    jsr OSBYTE
    tya
    .if version < 3
        jmp LAED8
    .elseif version >= 3
        jmp XAED3
    .endif

    .if version < 3
LAB7F:
        jsr LA1DA
        beq LABA2
        bpl LABA0
        bmi LAB9D
     
; =SGN numeric
; ============
LAB88:
        jsr LADEC
        beq LABE6
        bmi LAB7F
        lda zpIACC+3
        ora zpIACC+2
        ora zpIACC+1
        ora zpIACC
        beq LABA5
        lda zpIACC+3
        bpl LABA0
LAB9D:
        jmp LACC4

LABA0:
        lda #$01
LABA2:
        jmp LAED8

LABA5:
        lda #$40
        rts
    .endif

; =LOG numeric
; ============
LABA8:
    jsr LA7FE
    ldy #<RPLN10
    .if version < 3
        lda #>RPLN10
    .endif
    bne LABB8

; =RAD numeric
; ============
LABB1:
    jsr L92FA
    ldy #<FPIs18
    .if version < 3
        lda #>FPIs18
    .endif
LABB8:
    .if version >= 3
        lda #>FPIs18       ; identical to version < 3
    .endif
    sty zp4B
    sta zp4C
    jsr FMUL
    lda #$FF
    rts

; =DEG numeric
; ============
LABC2:
    jsr L92FA
    ldy #<F180sP
    .if version < 3
        lda #>F180sP
    .endif
    bne LABB8

; =PI
; ===
LABCB:
    jsr LA8FE
    inc zp30
    tay
    rts

; =USR numeric
; ============
LABD2:
    jsr L92E3         ; Evaluate integer
    jsr L8F1E         ; Set up registers and call code at IntA
    sta zpIACC
    stx zpIACC+1          ; Store returned A,X in IntA
    sty zpIACC+2          ; Store returned Y
    php
    pla
    sta zpIACC+3          ; Store returned flags in IntA
    cld               ; Ensure in binary mode on return
    lda #$40
    rts               ; Return INTEGER

    .if version < 3
LABE6:
        jmp L8C0E
    .endif

; =EVAL string$ - Tokenise and evaluate expression
; ================================================
LABE9:
    jsr LADEC         ; Evaluate value
    .if version < 3
        bne LABE6
    .elseif version >= 3
        bne LAC2C
    .endif
    inc zp36
    ldy zp36          ; Increment string length to add a <cr>
    lda #$0D
    sta ws+$05FF,Y    ; Put in terminating <cr>
    jsr LBDB2         ; Stack the string
                      ; String has to be stacked as otherwise would
                      ;  be overwritten by any string operations
                      ;  called by Evaluator
    lda zpAELINE
    pha               ; Save PTRB
    lda zpAELINE+1
    pha
    lda zpAECUR
    pha
    ldy zpAESTKP
    ldx zpAESTKP+1          ; YX=>stackbottom (wrong way around)
    iny               ; Step over length byte
    sty zpAELINE          ; PTRB=>stacked string
    sty zp37          ; GPTR=>stacked string
    bne LAC0F
    inx               ; Inc high byte if next page
LAC0F:
    stx zpAELINE+1
    stx zp38          ; PTRB and GPTR high bytes
    ldy #$FF
    sty zp3B
    iny
    sty zpAECUR          ; Point PTRB offset back to start
    jsr L8955         ; Tokenise string on stack at GPTR
    jsr L9B29         ; Call expression evaluator
    jsr LBDDC         ; Drop string from stack
LAC23:
    pla
    sta zpAECUR          ; Restore PTRB
    pla
    sta zpAELINE+1
    pla
    sta zpAELINE
    lda zpTYPE          ; Get expression return value type
    rts               ; And return

    .if version >= 3
LAC2C:
        jmp L8C0E
    .endif

; =VAL numeric
; ============
LAC2F:
    jsr LADEC
    .if version < 3
        bne LAC9B
    .elseif version >= 3
        bne LAC2C
    .endif
LAC34:
    ldy zp36
    lda #$00
    sta ws+$0600,Y
    lda zpAELINE
    pha
    lda zpAELINE+1
    pha
    lda zpAECUR
    pha
    lda #$00
    sta zpAECUR
    .if version < 3
        lda #$00
    .endif
    sta zpAELINE
    lda #$06+(ws/256)
    sta zpAELINE+1
    jsr L8A8C
    cmp #$2D
    beq LAC66
    cmp #$2B
    bne LAC5E
    jsr L8A8C
LAC5E:
    dec zpAECUR
    jsr LA07B
    jmp LAC73

LAC66:
    jsr L8A8C
    dec zpAECUR
    jsr LA07B
    bcc LAC73
    jsr LAD8F
LAC73:
    sta zpTYPE
    jmp LAC23

; =INT numeric
; ============
LAC78:
    jsr LADEC
    .if version < 3
        beq LAC9B
    .elseif version >= 3
        beq XAC81
    .endif
    bpl LAC9A
    lda zp2E
    php
    jsr LA3FE
    plp
    bpl LAC95
    lda zp3E
    ora zp3F
    ora zp40
    ora zp41
    beq LAC95
    jsr LA4C7
LAC95:
    jsr LA3E7
    lda #$40
LAC9A:
    rts

    .if version < 3
LAC9B:
        jmp L8C0E
    .endif

; =ASC string$
; ============
LAC9E:
    jsr LADEC
    .if version < 3
        bne LAC9B
    .elseif version >= 3
        bne XAC81
    .endif
    lda zp36
    beq LACC4
    lda ws+$0600
LACAA:
    .if version < 3
        jmp LAED8
    .elseif version >= 3
        jmp XAED3
    .endif

; =INKEY numeric
; ==============
LACAD:
    jsr LAFAD
    .if version < 3
        cpy #$00
    .elseif version >= 3
        tya     
    .endif
    bne LACC4
    txa
    .if version < 3
        jmp LAEEA
    .elseif version >= 3
        jmp XAED5
    .endif

    .if version >= 3
XAC81:
        jmp L8C0E
    .endif

; =EOF#numeric
; ============
LACB8:
    jsr LBFB5
    tax
    lda #$7F
    .ifdef MOS_BBC
        jsr OSBYTE
    .endif
    txa
    .if version < 3
        beq LACAA
    .elseif version >= 3
        beq LACC6
    .endif

; =TRUE
; =====
LACC4:
    .if version < 3
        lda #$FF
    .elseif version >= 3
        ldx #$FF
    .endif
LACC6:
    .if version < 3
        sta zpIACC
        sta zpIACC+1
        sta zpIACC+2
        sta zpIACC+3
    .elseif version >= 3
        stx zpIACC
        stx zpIACC+1
        stx zpIACC+2
        stx zpIACC+3
    .endif
LACC8:
    lda #$40
    rts

    .if version >= 3
; =FALSE
; ======
LACCD:
        ldx #$00
        beq LACC6

XACA1:
        jsr LA1DA
        beq LACCD
        bpl XACBF
        bmi LACC4
     
; =SGN numeric
; ============
XACAA:
        jsr LADEC
        beq XAC81
        bmi XACA1
        lda zpIACC+3
        ora zpIACC+2
        ora zpIACC+1
        ora zpIACC
        beq LACC8
        lda zpIACC+3
        bmi LACC4
XACBF:
        lda #$01
XACC1:
        jmp XAED3

; =POINT(numeric, numeric)
; ========================
XAB41:
        jsr L92DD
        jsr LBD94
        jsr L8AAE
        jsr LAE56
        jsr L92F0
        lda zpIACC
        pha
        ldx zpIACC+1
        jsr LBDEA
        stx zpIACC+3
        pla
        sta zpIACC+2
        ldx #$2A
        lda #$09
        jsr OSWORD
        lda zp2E
        bmi LACC4
        bpl XACC1
    .endif

    .if version < 3
; =NOT numeric
; ============
LACD1:
        jsr L92E3
        ldx #$03
LACD6:
        lda zpIACC,X
        eor #$FF
        sta zpIACC,X
        dex
        bpl LACD6
        lda #$40
        rts
    .endif

; =INSTR(string$, string$ [, numeric])
; ====================================
LACE2:
    jsr L9B29
    .if version < 3
        bne LACE2-$47     ; dest=LAC9B
    .elseif version >= 3
        bne XAC81
    .endif
    cpx #$2C
    bne LAD03
    inc zpAECUR
    jsr LBDB2
    jsr L9B29
    .if version < 3
        bne LACE2-$47     ; dest=LAC9B
    .elseif version >= 3
        bne XAC81
    .endif
    lda #$01
    sta zpIACC
    inc zpAECUR
    cpx #')'
    beq LAD12
    cpx #$2C
    beq LAD06
LAD03:
    .if version < 3
        jmp L8AA2
    .elseif version >= 3
        jmp X8AC8
    .endif

LAD06:
    jsr LBDB2
    jsr LAE56
    jsr L92F0
    jsr LBDCB
LAD12:
    ldy #$00
    ldx zpIACC
    bne LAD1A
    ldx #$01
LAD1A:
    stx zpIACC
    txa
    dex
    stx zpIACC+3
    clc
    adc zpAESTKP
    sta zp37
    tya
    adc zpAESTKP+1
    sta zp38
    lda (zpAESTKP),Y
    sec
    sbc zpIACC+3
    bcc LAD52
    sbc zp36
    bcc LAD52
    adc #$00
    sta zpIACC+1
    jsr LBDDC
LAD3C:
    ldy #$00
    ldx zp36
    beq LAD4D
LAD42:
    lda (zp37),Y
    cmp ws+$0600,Y
    bne LAD59
    iny
    dex
    bne LAD42
LAD4D:
    lda zpIACC
LAD4F:
    .if version < 3
        jmp LAED8
    .elseif version >= 3
        jmp XAED3
    .endif

LAD52:
    jsr LBDDC
LAD55:
    lda #$00
    beq LAD4F

LAD59:
    inc zpIACC
    dec zpIACC+1
    beq LAD55
    inc zp37
    bne LAD3C
    inc zp38
    bne LAD3C
LAD67:
    jmp L8C0E

; =ABS numeric
; ============
LAD6A:
    jsr LADEC
    beq LAD67
    bmi LAD77
LAD71:
    bit zpIACC+3
    bmi LAD93
    bpl LADAA
LAD77:
    jsr LA1DA
    bpl LAD89
    bmi LAD83
LAD7E:
    jsr LA1DA
    beq LAD89
LAD83:
    lda zp2E
    eor #$80
    sta zp2E
LAD89:
    lda #$FF
    rts

LAD8C:
    jsr LAE02
LAD8F:
    beq LAD67
    bmi LAD7E
LAD93:
    sec
    lda #$00
    tay
    sbc zpIACC
    sta zpIACC
    tya
    sbc zpIACC+1
    sta zpIACC+1
    tya
    sbc zpIACC+2
    sta zpIACC+2
    tya
    sbc zpIACC+3
    sta zpIACC+3
LADAA:
    lda #$40
    rts

LADAD:
    jsr L8A8C
    cmp #$22
    beq LADC9
    ldx #$00
LADB6:
    lda (zpAELINE),Y
    sta ws+$0600,X
    iny
    inx
    cmp #$0D
    beq LADC5
    cmp #$2C
    bne LADB6
LADC5:
    dey
    .if version < 3
        jmp LADE1
    .elseif version >= 3
LADC8:
        dex
        stx zp36
        sty zpAECUR
        lda #$00
        rts
    .endif

LADC9:
    ldx #$00
LADCB:
    iny
LADCC:
    lda (zpAELINE),Y
    cmp #$0D
    beq LADE9
    .if version < 3
        iny
        sta ws+$0600,X
    .elseif version >= 3
        sta ws+$0600,X
        iny
    .endif
    inx
    cmp #$22
    bne LADCC
    lda (zpAELINE),Y
    cmp #$22
    beq LADCB
    .if version < 3
LADE1:
        dex
        stx zp36
        sty zpAECUR
        lda #$00
        rts
    .elseif version >= 3
        bne LADC8
    .endif

LADE9:
    jmp L8E98

; Evaluator Level 1, - + NOT function ( ) ? ! $ | "
; -------------------------------------------------
LADEC:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y      ; Get next character
    cmp #$20
    beq LADEC         ; Loop to skip spaces
    cmp #'-'
    beq LAD8C         ; Jump with unary minus
    cmp #'"'
    beq LADC9         ; Jump with string
    cmp #'+'
    bne LAE05         ; Jump with unary plus
LAE02:
    jsr L8A8C         ; Get current character
LAE05:
    cmp #$8E
    bcc LAE10         ; Lowest function token, test for indirections
    cmp #$C6
    bcs LAE43         ; Highest function token, jump to error
    jmp L8BB1         ; Jump via function dispatch table

; Indirection, hex, brackets
; --------------------------
LAE10:
    cmp #'?'
    bcs LAE20         ; Jump with ?numeric or higher
    cmp #'.'
    bcs LAE2A         ; Jump with .numeric or higher
    cmp #'&'
    beq LAE6D         ; Jump with hex number
    cmp #'('
    beq LAE56         ; Jump with brackets
LAE20:
    dec zpAECUR
    jsr L95DD
    beq LAE30         ; Jump with undefined variable or bad name
    jmp LB32C

LAE2A:
    jsr LA07B
    bcc LAE43
    rts

LAE30:
    lda zpBYTESM          ; Check assembler option
    and #$02          ; Is 'ignore undefiened variables' set?
    bne LAE43         ; b1=1, jump to give No such variable
    bcs LAE43         ; Jump with bad variable name
    stx zpAECUR
LAE3A:
    lda ws+$0440      ; Use P% for undefined variable
    ldy ws+$0441
    .if version < 3
        jmp LAEEA     ; Jump to return 16-bit integer
    .elseif version >= 3
        jmp XAED5     ; Jump to return 16-bit integer
    .endif

LAE43:
    brk
    dta $1A
    .if foldup == 1
        dta 'NO SUCH VARIABLE'
    .else
        dta 'No such variable'
    .endif
LAE54:
    brk
    .if version >= 3
        dta $1B
        .if foldup == 1
            dta 'MISSING )'
        .else
            dta 'Missing )'
        .endif
LAE55:
        brk
        dta $1C
        .if foldup == 1
            dta 'BAD HEX'
        .else
            dta 'Bad HEX'
        .endif
        brk
    .endif

LAE56:
    jsr L9B29
    inc zpAECUR
    cpx #')'
    .if version < 3
        bne LAE61
    .elseif version >= 3
        bne LAE54
    .endif
    tay
    rts

    .if version < 3
LAE61:
        brk
        dta $1B
        .if foldup == 1
            dta 'MISSING )'
        .else
            dta 'Missing )'
        .endif
        brk
    .endif
LAE6D:
    .if version < 3
        ldx #$00
        stx zpIACC
        stx zpIACC+1
        stx zpIACC+2
        stx zpIACC+3
        ldy zpAECUR
    .elseif version >= 3
        jsr LACCD
        iny
    .endif
LAE79:
    lda (zpAELINE),Y
    cmp #$30
    bcc LAEA2
    cmp #$3A
    bcc LAE8D
    sbc #$37
    cmp #$0A
    bcc LAEA2
    cmp #$10
    bcs LAEA2
LAE8D:
    asl
    asl
    asl
    asl
    ldx #$03
LAE93:
    asl
    rol zpIACC
    rol zpIACC+1
    rol zpIACC+2
    rol zpIACC+3
    dex
    bpl LAE93
    iny
    bne LAE79
LAEA2:
    txa
    .if version < 3
        bpl LAEAA
    .elseif version >= 3
        bpl LAE55
    .endif
    sty zpAECUR
    lda #$40
    rts

    .if version >= 3
; =TOP - Return top of program
; ============================
XAEA6:
        iny
        lda (zpAELINE),Y
        cmp #'P'
        bne LAE43
        inc zpAECUR
        lda zpTOP
        ldy zpTOP+1
        bcs XAED5

; =PAGE - Read PAGE
; =================
XAEA7:
        ldy zpTXTP
        lda #$00
        beq XAED5

XAEC9:
        jmp L8C0E

; =LEN string$
; ============
XAECC:
        jsr LADEC
        bne XAEC9
        lda zp36
    
; Return 8-bit integer
; --------------------
XAED3:
        ldy #$00      ; Clear b8-b15, jump to return 16-bit int

; Return 16-bit integer in AY
; ---------------------------
XAED5:
        sta zpIACC
        sty zpIACC+1      ; Store AY in integer accumulator
        lda #$00
        sta zpIACC+2
        sta zpIACC+3      ; Set b16-b31 to 0
        lda #$40
        rts           ; Return 'integer'

; =COUNT - Return COUNT
; =====================
XAEF7:
        lda zpTALLY
        bcc XAED3     ; Get COUNT, jump to return 8-bit integer
     
; =LOMEM - Start of BASIC heap
; ============================
XAEFC:
        lda zpLOMEM
        ldy zpLOMEM+1
        bcc XAED5     ; Get LOMEM to AY, jump to return as integer
     
; =HIMEM - Top of BASIC memory
; ============================
XAF03:
        lda zpHIMEM
        ldy zpHIMEM+1
        bcc XAED5     ; Get HIMEM to AY, jump to return as integer

; =ERL - Return error line number
; ===============================
XAF9F:
        ldy zpERL+1
        lda zpERL
        bcc XAED5     ; Get ERL to AY, jump to return 16-bit integer

; =ERR - Return current error number
; ==================================
XAFA6:
        ldy #$00
        lda (FAULT),Y
        bcc XAED5     ; Get error number, jump to return 16-bit integer
    .endif

    .if version < 3
LAEAA:
        brk
        dta $1C
        .if foldup == 1
            dta 'BAD HEX'
        .else
            dta 'Bad HEX'
        .endif
        brk
    .endif

; =TIME - Read system TIME
; ========================
LAEB4:
    ldx #$2A
    ldy #$00          ; Point to integer accumulator
    lda #$01          ; Read TIME to IntA via OSWORD $01
    .ifdef MOS_BBC
        jsr OSWORD
    .endif
    lda #$40
    rts               ; Return 'integer'

    .if version < 3
; =PAGE - Read PAGE
; =================
LAEC0:
        lda #$00
        ldy zpTXTP
        jmp LAEEA
     
LAEC7:
        jmp LAE43
     
; =FALSE
; ======
LAECA:
        lda #$00
        beq LAED8     ; Jump to return $00 as 16-bit integer

LAECE:
        jmp L8C0E
     
; =LEN string$
; ============
LAED1:
        jsr LADEC
        bne LAECE
        lda zp36

; Return 8-bit integer
; --------------------
LAED8:
        ldy #$00
        beq LAEEA     ; Clear b8-b15, jump to return 16-bit int

; =TOP - Return top of program
; ============================
LAEDC:
        ldy zpAECUR
        lda (zpAELINE),Y
        cmp #$50
        bne LAEC7
        inc zpAECUR
        lda zpTOP
        ldy zpTOP+1

; Return 16-bit integer in AY
; ---------------------------
LAEEA:
        sta zpIACC
        sty zpIACC+1      ; Store AY in integer accumulator
        lda #$00
        sta zpIACC+2
        sta zpIACC+3      ; Set b16-b31 to 0
        lda #$40
        rts           ; Return 'integer'

; =COUNT - Return COUNT
; =====================
LAEF7:
        lda zpTALLY
        jmp LAED8     ; Get COUNT, jump to return 8-bit integer
     
; =LOMEM - Start of BASIC heap
; ============================
LAEFC:
        lda zpLOMEM
        ldy zpLOMEM+1
        jmp LAEEA     ; Get LOMEM to AY, jump to return as integer
     
; =HIMEM - Top of BASIC memory
; ============================
LAF03:
        lda zpHIMEM
        ldy zpHIMEM+1
    jmp LAEEA         ; Get HIMEM to AY, jump to return as integer
    .endif

; =RND(numeric)
; -------------
LAF0A:
    inc zpAECUR
    jsr LAE56
    jsr L92F0
    lda zpIACC+3
    bmi LAF3F
    ora zpIACC+2
    ora zpIACC+1
    bne LAF24
    lda zpIACC
    beq LAF6C
    cmp #$01
    beq LAF69
LAF24:
    jsr LA2BE
    jsr LBD51
    jsr LAF69
    jsr LBD7E
    jsr LA606
    jsr LA303
    jsr LA3E4
    jsr L9222
    lda #$40
    rts

LAF3F:
    ldx #$0D
    jsr LBE44
    lda #$40
    sta zpSEED+4
    rts

; RND [(numeric)]
; ===============
LAF49:
    ldy zpAECUR
    lda (zpAELINE),Y      ; Get current character
    cmp #'('
    beq LAF0A         ; Jump with RND(numeric)
    jsr LAF87         ; Get random number
    ldx #$0D
LAF56:
    lda zp+0,X
    sta zpIACC          ; Copy random number to IntA
    lda zp+1,X
    sta zpIACC+1
    lda zp+2,X
    sta zpIACC+2
    lda zp+3,X
    sta zpIACC+3
    lda #$40
    rts               ; Return Integer

LAF69:
    jsr LAF87
LAF6C:
    ldx #$00
    stx zp2E
    stx zp2F
    stx zp35
    lda #$80
    sta zp30
LAF78:
    lda zpSEED,X
    sta zp31,X
    inx
    cpx #$04
    bne LAF78
    jsr LA659
    lda #$FF
    rts

LAF87:
    .if version >= 3
        ldy #$04      ; Rotate through four bytes, faster but bigger
LAF89:
        ror zpSEED+4
        lda zpSEED+3
        pha
        ror
        sta zpSEED+4
        lda zpSEED+2
        tax
        asl
        asl
        asl
        asl
        sta zpSEED+3
        lda zpSEED+1
        sta zpSEED+2
        lsr
        lsr
        lsr
        lsr
        ora zpSEED+3
        eor zpSEED+4
        stx zpSEED+3
        ldx zpSEED
        stx zpSEED+1
        sta zpSEED
        pla
        sta zpSEED+4
LAFB1:
        dey
        bne LAF89
        rts
    .elseif version < 3
        ldy #$20      ; Rotate through 32 bits, shorter but slower
LAF89:
        lda zpSEED+2
        lsr
        lsr
        lsr
        eor zpSEED+4
        ror
        rol zpSEED
        rol zpSEED+1
        rol zpSEED+2
        rol zpSEED+3
        rol zpSEED+4
        dey
        bne LAF89
        rts
 
; =ERL - Return error line number
; ===============================
LAF9F:
        ldy zpERL+1
        lda zpERL
        jmp LAEEA     ; Get ERL to AY, jump to return 16-bit integer

; =ERR - Return current error number
; ==================================
LAFA6:
        ldy #$00
        lda (FAULT),Y
        jmp LAEEA     ; Get error number, jump to return 16-bit integer
    .endif

; INKEY
; =====
LAFAD:
    jsr L92E3         ; Evaluate <numeric>

; Atom/System - Manually implement INKEY(num)
; -------------------------------------------
LCF8D:
    .ifdef TARGET_ATOM
        jsr $FE71
        bcc LCFAB     ; Key pressed
    .endif
    .ifdef TARGET_SYSTEM
        lda ESCFLG
        bpl LCFAB     ; Key pressed
    .endif
    .ifdef MOS_ATOM
        lda zpIACC
        ora zpIACC+3
        beq LCFB4     ; Timeout=0
        ldy #$08      ; $0800 gives 1cs delay
LCF9A:
        dex
        bne LCF9A
        dey
        bne LCF9A     ; Wait 1cs
        lda zpIACC
        bne LCFA6
        dec zpIACC+3      ; Decrement timeout
LCFA6:
        dec zpIACC
        jmp LCF8D     ; Loop to keep waiting
LCFAB:
    .endif
    .ifdef TARGET_ATOM
        jsr LCFB7     ; Convert keypress
    .endif
    .ifdef TARGET_SYSTEM
        ldy ESCFLG
        bpl LCFAB     ; Loop until key released
    .endif
    .ifdef MOS_ATOM
        ldy #$00
        tax
        rts           ; Y=0, X=key, return
LCFB4:
        ldy #$FF
        rts           ; Y=$FF for no keypress
    .endif
    .ifdef TARGET_ATOM
LCFB7:
        php
        jmp $FEA4     ; Convert Atom keypress
    .endif

; BBC - Call MOS to wait for keypress
; -----------------------------------
    .ifdef MOS_BBC
        lda #$81
LAFB2:
        ldx zpIACC
        ldy zpIACC+1
        jmp OSBYTE
    .endif

; =GET
; ====
LAFB9:
    jsr OSRDCH
    .if version < 3
        jmp LAED8
    .elseif version >= 3
        jmp XAED3
    .endif


; =GET$
; =====
LAFBF:
    jsr OSRDCH
LAFC2:
    sta ws+$0600
    lda #$01
    sta zp36
    lda #$00
    rts

; =LEFT$(string$, numeric)
; ========================
LAFCC:
    jsr L9B29
    bne LB033
    cpx #$2C
    bne LB036
    inc zpAECUR
    jsr LBDB2
    jsr LAE56
    jsr L92F0
    jsr LBDCB
    lda zpIACC
    cmp zp36
    bcs LAFEB
    sta zp36
LAFEB:
    lda #$00
    rts

; =RIGHT$(string$, numeric)
; =========================
LAFEE:
    jsr L9B29
    bne LB033
    cpx #$2C
    bne LB036
    inc zpAECUR
    jsr LBDB2
    jsr LAE56
    jsr L92F0
    jsr LBDCB
    lda zp36
    sec
    sbc zpIACC
    bcc LB023
    beq LB025
    tax
    lda zpIACC
    sta zp36
    beq LB025
    ldy #$00
LB017:
    lda ws+$0600,X
    sta ws+$0600,Y
    inx
    iny
    dec zpIACC
    bne LB017
LB023:
    lda #$00
LB025:
    rts

; =INKEY$ numeric
; ===============
LB026:
    jsr LAFAD
    txa
    cpy #$00
    beq LAFC2
LB02E:
    lda #$00
    sta zp36
    rts

LB033:
    jmp L8C0E

LB036:
    .if version < 3
        jmp L8AA2
    .elseif version >= 3
        jmp X8AC8
    .endif


; =MID$(string$, numeric [, numeric] )
; ====================================
LB039:
    jsr L9B29
    bne LB033
    cpx #$2C
    bne LB036
    jsr LBDB2
    inc zpAECUR
    jsr L92DD
    lda zpIACC
    pha
    lda #$FF
    sta zpIACC
    inc zpAECUR
    cpx #')'
    beq LB061
    cpx #$2C
    bne LB036
    jsr LAE56
    jsr L92F0
LB061:
    jsr LBDCB
    pla
    tay
    clc
    beq LB06F
    sbc zp36
    bcs LB02E
    dey
    tya
LB06F:
    sta zpIACC+2
    tax
    ldy #$00
    lda zp36
    sec
    sbc zpIACC+2
    cmp zpIACC
    bcs LB07F
    sta zpIACC
LB07F:
    lda zpIACC
    beq LB02E
LB083:
    lda ws+$0600,X
    sta ws+$0600,Y
    iny
    inx
    cpy zpIACC
    bne LB083
    sty zp36
    lda #$00
    rts

; =STR$ [~] numeric
; =================
LB094:
    jsr L8A8C         ; Skip spaces
    ldy #$FF          ; Y=$FF for decimal
    cmp #'~'
    beq LB0A1
    ldy #$00
    dec zpAECUR          ; Y=$00 for hex, step past ~
LB0A1:
    tya
    pha               ; Save format
    jsr LADEC
    beq LB0BF         ; Evaluate, error if not number
    tay
    pla
    sta zpPRINTF          ; Get format back
    lda VARL_AT+3
    bne LB0B9         ; Top byte of @%, STR$ uses @%
    sta zp37          ; Store 'General format'
    jsr L9EF9         ; Convert using general format
    lda #$00
    rts               ; Return string

LB0B9:
    jsr FCON         ; Convert using @% format
    lda #$00
    rts               ; Return string

LB0BF:
    jmp L8C0E         ; Jump to Type mismatch error

; =STRING$(numeric, string$)
; ==========================
LB0C2:
    jsr L92DD
    jsr LBD94
    jsr L8AAE
    jsr LAE56
    bne LB0BF
    jsr LBDEA
    ldy zp36
    beq LB0F5
    lda zpIACC
    beq LB0F8
    dec zpIACC
    beq LB0F5
LB0DF:
    ldx #$00
LB0E1:
    lda ws+$0600,X
    sta ws+$0600,Y
    inx
    iny
    beq LB0FB
    cpx zp36
    bcc LB0E1
    dec zpIACC
    bne LB0DF
    sty zp36
LB0F5:
    lda #$00
    rts

LB0F8:
    sta zp36
    rts

LB0FB:
    jmp L9C03

LB0FE:
    pla
    sta zpLINE+1
    pla
    sta zpLINE
    brk
    dta $1D
    .if foldup == 1
        dta 'NO SUCH '
    .else
        dta 'No such '
    .endif
    dta tknFN, '/', tknPROC
    brk

; Look through program for FN/PROC
; --------------------------------
LB112:
    lda zpTXTP
    sta zpLINE+1          ; Start at PAGE
    lda #$00
    sta zpLINE
LB11A:
    ldy #$01
    lda (zpLINE),Y      ; Get line number high byte
    bmi LB0FE         ; End of program, jump to 'No such FN/PROC' error
    ldy #$03
LB122:
    iny
    lda (zpLINE),Y
    cmp #$20
    beq LB122         ; Skip past spaces
    cmp #tknDEF
    beq LB13C         ; Found DEF at start of line
LB12D:
    ldy #$03
    lda (zpLINE),Y      ; Get line length
    clc
    adc zpLINE
    sta zpLINE          ; Point to next line
    bcc LB11A
    inc zpLINE+1
    bcs LB11A         ; Loop back to check next line

LB13C:
    iny
    sty zpCURSOR
    jsr L8A97
    tya
    tax
    clc
    adc zpLINE
    ldy zpLINE+1
    bcc LB14D
    iny
    clc
LB14D:
    sbc #$00
    sta zp3C
    tya
    sbc #$00
    sta zp3D
    ldy #$00
LB158:
    iny
    inx
    lda (zp3C),Y
    cmp (zp37),Y
    bne LB12D
    cpy zp39
    bne LB158
    iny
    lda (zp3C),Y
    jsr L8926
    bcs LB12D
    txa
    tay
    jsr CLYADP
    jsr L94ED
    ldx #$01
    jsr L9531
    ldy #$00
    lda zpLINE
    sta (zpFSA),Y
    iny
    lda zpLINE+1
    sta (zpFSA),Y
    jsr L9539
    jmp LB1F4

LB18A:
    brk
    dta $1E
    .if foldup == 1
        dta 'BAD CALL'
    .else
        dta 'Bad call'
    .endif
    brk

; =FNname [parameters]
; ====================
LB195:
    lda #$A4          ; 'FN' token

; Call subroutine
; ---------------
; A=FN or PROC
; PtrA=>start of FN/PROC name
;
LB197:
    sta zpTYPE          ; Save PROC/FN token
    tsx
    txa
    clc
    adc zpAESTKP          ; Drop BASIC stack by size of 6502 stack
    jsr LBE2E         ; Store new BASIC stack pointer, check for No Room
    ldy #$00
    txa
    sta (zpAESTKP),Y      ; Store 6502 Stack Pointer on BASIC stack
LB1A6:
    inx
    iny
    lda $0100,X
    sta (zpAESTKP),Y      ; Copy 6502 stack onto BASIC stack
    cpx #$FF
    bne LB1A6
    txs               ; Clear 6502 stack
    lda zpTYPE
    pha               ; Push PROC/FN token
    lda zpCURSOR
    pha
    lda zpLINE
    pha               ; Push PtrA line pointer
    lda zpLINE+1
    pha               ; Push PtrA line pointer offset
    lda zpAECUR
    tax
    clc
    adc zpAELINE
    ldy zpAELINE+1
    bcc LB1CA
LB1C8:
    iny
    clc
LB1CA:
    sbc #$01
    sta zp37
    tya
    sbc #$00
    sta zp38          ; $37/8=>PROC token
    ldy #$02
    jsr L955B         ; Check name is valid
    cpy #$02
    beq LB18A         ; No valid characters, jump to 'Bad call' error
    stx zpAECUR          ; Line pointer offset => after valid FN/PROC name
    dey
    sty zp39
    jsr L945B
    bne LB1E9         ; Look for FN/PROC name in heap, if found, jump to it
    jmp LB112         ; Not in heap, jump to look through program

; FN/PROC destination found
; -------------------------
LB1E9:
    ldy #$00
    lda (zpIACC),Y
    sta zpLINE          ; Set PtrA to address from FN/PROC infoblock
    iny
    lda (zpIACC),Y
    sta zpLINE+1
LB1F4:
    lda #$00
    pha
    sta zpCURSOR          ; Push 'no parameters' (?)
    jsr L8A97
    cmp #'('
    beq LB24D
    dec zpCURSOR
LB202:
    lda zpAECUR
    pha
    lda zpAELINE
    pha
    lda zpAELINE+1
    pha
    jsr L8BA3
    pla
    sta zpAELINE+1
    pla
    sta zpAELINE
    pla
    sta zpAECUR
    pla
    beq LB226
    sta zp3F
LB21C:
    jsr LBE0B
    jsr L8CC1
    dec zp3F
    bne LB21C
LB226:
    pla
    sta zpLINE+1
    pla
    sta zpLINE
    pla
    sta zpCURSOR
    pla
    ldy #$00
    lda (zpAESTKP),Y
    tax
    txs
LB236:
    iny
    inx
    lda (zpAESTKP),Y
    sta $0100,X       ; Copy stacked 6502 stack back onto 6502 stack
    cpx #$FF
    bne LB236
    tya
    adc zpAESTKP
    sta zpAESTKP          ; Adjust BASIC stack pointer
    bcc LB24A
    inc zpAESTKP+1
LB24A:
    lda zpTYPE
    rts

LB24D:
    lda zpAECUR
    pha
    lda zpAELINE
    pha
    lda zpAELINE+1
    pha
    jsr L9582
    beq LB2B5
    lda zpAECUR
    sta zpCURSOR
    pla
    sta zpAELINE+1
    pla
    sta zpAELINE
    pla
    sta zpAECUR
    pla
    tax
    lda zpIACC+2
    pha
    lda zpIACC+1
    pha
    lda zpIACC
    pha
    inx
    txa
    pha
    jsr LB30D
    jsr L8A97
    cmp #','
    beq LB24D
    cmp #')'
    bne LB2B5
    lda #$00
    pha
    jsr L8A8C
    cmp #'('
    bne LB2B5
LB28E:
    jsr L9B29
    jsr LBD90
    lda zpTYPE
    sta zpIACC+3
    jsr LBD94
    pla
    tax
    inx
    txa
    pha
    jsr L8A8C
    cmp #','
    beq LB28E
    cmp #')'
    bne LB2B5
    pla
    pla
    sta zpCOEFP
    sta zpCOEFP+1
    cpx zpCOEFP
    beq LB2CA
LB2B5:
    ldx #$FB
    txs
    pla
    sta zpLINE+1
    pla
    sta zpLINE
    brk
    dta $1F
    .if foldup == 1
        dta 'ARGUMENTS'
    .else
        dta 'Arguments'
    .endif
    brk

LB2CA:
    jsr LBDEA
    pla
    sta zpIACC
    pla
    sta zpIACC+1
    pla
    sta zpIACC+2
    bmi LB2F9
    lda zpIACC+3
    beq LB2B5
    sta zpTYPE
    ldx #$37
    jsr LBE44
    lda zpTYPE
    bpl LB2F0
    jsr LBD7E
    jsr LA3B5
    jmp LB2F3

LB2F0:
    jsr LBDEA
LB2F3:
    jsr LB4B7
    jmp LB303

LB2F9:
    lda zpIACC+3
    bne LB2B5
    jsr LBDCB
    jsr L8C21
LB303:
    dec zpCOEFP
    bne LB2CA
    lda zpCOEFP+1
    pha
    jmp LB202

; Push a value onto the stack
; ---------------------------
LB30D:
    ldy zpIACC+2
    .if version < 3
        cpy #$04
        bne LB318
    .elseif version >= 3
        cpy #$05
        bcs LB318
    .endif
    ldx #$37
    jsr LBE44
LB318:
    jsr LB32C
    php
    jsr LBD90
    plp
    beq LB329
    bmi LB329
    ldx #$37
    jsr LAF56
LB329:
    jmp LBD94

LB32C:
    ldy zpIACC+2
    bmi LB384
    beq LB34F
    cpy #$05
    beq LB354
    ldy #$03
    lda (zpIACC),Y
    sta zpIACC+3
    dey
    lda (zpIACC),Y
    sta zpIACC+2
    dey
    lda (zpIACC),Y
    tax
    dey
    lda (zpIACC),Y
    sta zpIACC
    stx zpIACC+1
    lda #$40
    rts

LB34F:
    lda (zpIACC),Y
    .if version < 3
        jmp LAEEA
    .elseif version >= 3
        jmp XAED5
    .endif

LB354:
    dey
    lda (zpIACC),Y
    sta zp34
    dey
    lda (zpIACC),Y
    sta zp33
    dey
    lda (zpIACC),Y
    sta zp32
    dey
    lda (zpIACC),Y
    sta zp2E
    dey
    lda (zpIACC),Y
    sta zp30
    sty zp35
    sty zp2F
    ora zp2E
    ora zp32
    ora zp33
    ora zp34
    beq LB37F
    lda zp2E
    ora #$80
LB37F:
    sta zp31
    lda #$FF
    rts

LB384:
    cpy #$80
    beq LB3A7
    ldy #$03
    lda (zpIACC),Y
    sta zp36
    beq LB3A6
    ldy #$01
    lda (zpIACC),Y
    sta zp38
    dey
    lda (zpIACC),Y
    sta zp37
    ldy zp36
LB39D:
    dey
    lda (zp37),Y
    sta ws+$0600,Y
    tya
    bne LB39D
LB3A6:
    rts

LB3A7:
    lda zpIACC+1
    beq LB3C0
LB3AB:
    ldy #$00
LB3AD:
    lda (zpIACC),Y
    sta ws+$0600,Y
    eor #$0D
    beq LB3BA
    iny
    bne LB3AD
    tya
LB3BA:
    sty zp36
    rts

; =CHR$ numeric
; =============
LB3BD:
    jsr L92E3
LB3C0:
    lda zpIACC
    jmp LAFC2

LB3C5:
    ldy #$00
    sty zpERL
    sty zpERL+1
    ldx zpTXTP
    stx zp38
    sty zp37
    ldx zpLINE+1
    cpx #$07+(ws/256)
    beq LB401
    ldx zpLINE
LB3D9:
    jsr L8942
    cmp #$0D
    bne LB3F9
    cpx zp37
    lda zpLINE+1
    sbc zp38
    bcc LB401
    jsr L8942
    ora #$00
    bmi LB401
    sta zpERL+1
    jsr L8942
    sta zpERL
    jsr L8942
LB3F9:
    cpx zp37
    lda zpLINE+1
    sbc zp38
    bcs LB3D9
LB401:
    rts


; ERROR HANDLER
; =============
LB402:


; Atom/System - Process raw BRK to get FAULT pointer
; --------------------------------------------------
    .ifdef MOS_ATOM
        pla
        cld
        cli
        pla           ; Drop flags, pop return low byte
        sec
        sbc #$01
        sta FAULT+0       ; Point to error block
        pla
        sbc #$00
        sta FAULT+1
        cmp #>L8000
        bcc LD428     ; If outside BASIC, not a full error block
        cmp #[>LC000]-1       ; syntax?
        bcs LD428     ; So generate default error
    .endif


; FAULT set up, now process BRK error
; -----------------------------------
    jsr LB3C5
    sty zpTRFLAG
    lda (FAULT),Y
    bne LB413         ; If ERR<>0, skip past ON ERROR OFF
    lda #<LB433
    sta zpERRORLH          ; ON ERROR OFF
    lda #>LB433
    sta zpERRORLH+1
LB413:          ; BREKA
    lda zpERRORLH
    sta zpLINE          ; Point program point to ERROR program
    lda zpERRORLH+1
    sta zpLINE+1
    jsr LBD3A         ; Clear DATA and stack
    tax
    stx zpCURSOR
    .ifdef MOS_BBC
        lda #$DA
        jsr OSBYTE    ; Clear VDU queue
        lda #$7E
        jsr OSBYTE    ; Acknowlege any Escape state
    .endif
    ldx #$FF
    stx zpBYTESM
    txs               ; Clear system stack
    jmp L8BA3         ; Jump to execution loop

LD428:
    .ifdef MOS_ATOM
        .if foldup == 0
            brk
            dta $FF
            dta 'External Error'
            brk
        .endif
        .if foldup != 0
            brk
            dta $FF
            dta tknEXT, 'ERNAL ', tknERROR
            brk
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
    dta '"'
    .if foldup == 1
        dta ' AT LINE '
    .else
        dta ' at line '
    .endif
    dta '"', ';'
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
    jsr L8821         ; Evaluate integer
    ldx #$03          ; Three more to evaluate
LB451:
    .ifdef MOS_BBC
        lda zpIACC
        pha
        lda zpIACC+1
        pha           ; Stack current 16-bit integer
    .endif
    txa
    pha
    jsr L92DA         ; Step past comma, evaluate next integer
    pla
    tax
    dex
    bne LB451         ; Loop to stack this one
    .ifdef MOS_BBC
        jsr L9852     ; Check end of statement
        lda zpIACC
        sta zp3D      ; Copy current 16-bit integer to end of control block
        lda zpIACC+1
        sta zp3E
        ldy #$07
        ldx #$05      ; Prepare for OSWORD 7 and 6 more bytes
        bne LB48F     ; Jump to pop to control block and call OSWORD
    .endif
    .ifdef MOS_ATOM
        beq LB484     ; Check end of statement and return
    .endif

; ENVELOPE a,b,c,d,e,f,g,h,i,j,k,l,m,n
; ====================================
LB472:
    jsr L8821         ; Evaluate integer
    ldx #$0D          ; 13 more to evaluate
LB477:
    .ifdef MOS_BBC
        lda zpIACC
        pha           ; Stack current 8-bit integer
    .endif
    txa
    pha
    jsr L92DA         ; Step past comma, evaluate next integer
    pla
    tax
    dex
    bne LB477         ; Loop to stack this one
LB484:
    jsr L9852         ; Check end of statement
    .ifdef MOS_BBC
        lda zpIACC
        sta zp44      ; Copy current 8-bit integer to end of control block
        ldx #$0C
        ldy #$08      ; Prepare for 12 more bytes and OSWORD 8
    .endif


LB48F:
    .ifdef MOS_BBC
        pla
        sta zp37,X    ; Pop bytes into control block
        dex
        bpl LB48F
        tya           ; Y=OSWORD number
        ldx #$37
        ldy #$00      ; XY=>control block
        jsr OSWORD
    .endif
    jmp L8B9B         ; Return to execution loop

; WIDTH numeric
; =============
LB4A0:
    jsr L8821
    jsr L9852
    ldy zpIACC
    dey
    sty zpWIDTHV
    jmp L8B9B

LB4AE:
    jmp L8C0E

; Store byte or word integer
; ==========================
LB4B1:
    jsr L9B29         ; Evaluate expression
LB4B4:
    jsr LBE0B         ; Unstack integer (address of data)
LB4B7:
    lda zp39
    cmp #$05
    beq LB4E0         ; Size=5, jump to store float
    lda zpTYPE
    beq LB4AE         ; Type<>num, jump to error
    bpl LB4C6         ; Type=int, jump to store it
    jsr LA3E4         ; Convert float to integer
LB4C6:
    ldy #$00
    lda zpIACC
    sta (zp37),Y      ; Store byte 1
    lda zp39
    beq LB4DF         ; Exit if size=0, byte
    lda zpIACC+1
    iny
    sta (zp37),Y      ; Store byte 2
    lda zpIACC+2
    iny
    sta (zp37),Y      ; Store byte 3
    lda zpIACC+3
    iny
    sta (zp37),Y      ; Store byte 4
LB4DF:
    rts

; Store float
; ===========
LB4E0:
    lda zpTYPE
    beq LB4AE         ; Type<>num, jump to error
    bmi LB4E9         ; Type=float, jump to store it
    jsr LA2BE         ; Convert integer to float
LB4E9:
    ldy #$00          ; Store 5-byte float
    lda zp30
    sta (zp37),Y
    iny               ; exponent
    lda zp2E
    and #$80
    sta zp2E          ; Unpack sign
    lda zp31
    and #$7F          ; Unpack mantissa 1
    ora zp2E
    sta (zp37),Y      ; sign + mantissa 1
    iny
    lda zp32
    sta (zp37),Y      ; mantissa 2
    iny
    lda zp33
    sta (zp37),Y      ; mantissa 3
    iny
    lda zp34
    sta (zp37),Y      ; mantissa 4
    rts

LB500:
LB50E:
    sta zp37
    cmp #$80
    bcc LB558
    lda #<L8071
    sta zp38          ; Point to token table
    lda #>L8071
    sta zp39
    sty zp3A
LB51E:
    ldy #$00
LB520:
    iny
    lda (zp38),Y
    bpl LB520
    cmp zp37
    beq LB536
    iny
    tya
    sec
    adc zp38
    sta zp38
    bcc LB51E
    inc zp39
    bcs LB51E

LB536:
    ldy #$00
LB538:
    lda (zp38),Y
    bmi LB542
    jsr LB558
    iny
    bne LB538
LB542:
    ldy zp3A
    rts

LB545:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr LB550
    pla
    and #$0F
LB550:
    cmp #$0A
    bcc LB556
    adc #$06
LB556:
    adc #$30
LB558:
    cmp #$0D
    bne LB567
    jsr OSWRCH
    jmp LBC28         ; Set COUNT to zero

LB562:
    jsr LB545
LB565:
    lda #$20
LB567:
    pha
    lda zpWIDTHV
    cmp zpTALLY
    bcs LB571
    jsr LBC25
LB571:
    pla
    inc zpTALLY
    .if WRCHV != 0
        jmp (WRCHV)
    .endif
    .if WRCHV == 0
        jmp OSWRCH 
    .endif

LB577:
    and zpLISTPO
    beq LB589
    txa
    beq LB589
    bmi LB565
    .if version >= 3
        asl
        tax
    .endif
LB580:
    jsr LB565
    .if version < 3
        jsr LB558
    .endif
    dex
    bne LB580
LB589:
    rts

LB58A:
    inc zpCURSOR
    jsr L9B1D
    jsr L984C
    jsr L92EE
    lda zpIACC
    sta zpLISTPO
    jmp L8AF6

; LIST [linenum [,linenum]]
; =========================
LB59C:
    iny
    lda (zpLINE),Y
    cmp #'O'
    beq LB58A
    lda #$00
    sta zp3B
    sta zp3C
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    jsr L97DF
    php
    jsr LBD94
    lda #$FF
    sta zpIACC
    lda #$7F
    sta zpIACC+1
    plp
    bcc LB5CF
    jsr L8A97
    cmp #','
    beq LB5D8
    jsr LBDEA
    jsr LBD94
    dec zpCURSOR
    bpl LB5DB
LB5CF:
    jsr L8A97
    cmp #','
    beq LB5D8
    dec zpCURSOR
LB5D8:
    jsr L97DF
LB5DB:
    lda zpIACC
    sta zp31
    lda zpIACC+1
    sta zp32
    jsr DONE
    jsr LBE6F
    jsr LBDEA
    jsr L9970
    lda zp3D
    sta zpLINE
    lda zp3E
    sta zpLINE+1
    bcc LB60F
    dey
    bcs LB602

LB5FC:
    jsr LBC25
    jsr CLYADP
LB602:
    lda (zpLINE),Y
    sta zpIACC+1
    iny
    lda (zpLINE),Y
    sta zpIACC
    iny
    iny
    sty zpCURSOR
LB60F:
    lda zpIACC
    clc
    sbc zp31
    lda zpIACC+1
    sbc zp32
    bcc LB61D
    jmp L8AF6

LB61D:
    jsr L9923
    ldx #$FF
    stx zpCOEFP
    lda #$01
    jsr LB577
    ldx zp3B
    lda #$02
    jsr LB577
    ldx zp3C
    lda #$04
    jsr LB577
LB637:
    ldy zpCURSOR
LB639:
    lda (zpLINE),Y
    cmp #$0D
    beq LB5FC
    cmp #$22
    bne LB651
    lda #$FF
    eor zpCOEFP
    sta zpCOEFP
    lda #$22
LB64B:
    jsr LB558
    iny
    bne LB639
LB651:
    bit zpCOEFP
    bpl LB64B
    cmp #$8D
    bne LB668
    jsr L97EB
    sty zpCURSOR
    lda #$00
    sta zpPRINTS
    jsr L991F
    jmp LB637

LB668:
    cmp #$E3
    bne LB66E
    inc zp3B
LB66E:
    cmp #$ED
    bne LB678
    ldx zp3B
    beq LB678
    dec zp3B
LB678:
    cmp #$F5
    bne LB67E
    inc zp3C
LB67E:
    cmp #$FD
    bne LB688
    ldx zp3C
    beq LB688
    dec zp3C
LB688:
    jsr LB50E
    iny
    bne LB639
LB68E:
    brk
    dta $20
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknFOR
    brk

; NEXT [variable [,...]]
; ======================
LB695:
    jsr L95C9
    bne LB6A3
    ldx zpFORSTP
    beq LB68E
    bcs LB6D7
LB6A0:
    jmp L982A

LB6A3:
    bcs LB6A0
    ldx zpFORSTP
    beq LB68E
LB6A9:
    lda zpIACC
    cmp ws+$04F1,X
    bne LB6BE
    lda zpIACC+1
    cmp ws+$04F2,X
    bne LB6BE
    lda zpIACC+2
    cmp ws+$04F3,X
    beq LB6D7
LB6BE:
    txa
    sec
    sbc #$0F
    tax
    stx zpFORSTP
    bne LB6A9
    brk
    dta $21
    .if .def TARGET_BBC
        dta 'Can', 0x27, 't Match ', tknFOR
    .elseif .def TARGET_SYSTEM || .def TARGET_C64
        dta 'Can', 0x27, 't match ', tknFOR
    .elseif .def TARGET_ATOM
        dta 'CAN', 0x27, 'T MATCH ', tknFOR
    .endif
    brk

LB6D7:
    lda ws+$04F1,X
    sta zpIACC
    lda ws+$04F2,X
    sta zpIACC+1
    ldy ws+$04F3,X
    cpy #$05
    beq LB766
    ldy #$00
    lda (zpIACC),Y
    adc ws+$04F4,X
    sta (zpIACC),Y
    sta zp37
    iny
    lda (zpIACC),Y
    adc ws+$04F5,X
    sta (zpIACC),Y
    sta zp38
    iny
    lda (zpIACC),Y
    adc ws+$04F6,X
    sta (zpIACC),Y
    sta zp39
    iny
    lda (zpIACC),Y
    adc ws+$04F7,X
    sta (zpIACC),Y
    tay
    lda zp37
    sec
    sbc ws+$04F9,X
    sta zp37
    lda zp38
    sbc ws+$04FA,X
    sta zp38
    lda zp39
    sbc ws+$04FB,X
    sta zp39
    tya
    sbc ws+$04FC,X
    ora zp37
    ora zp38
    ora zp39
    beq LB741
    tya
    eor ws+$04F7,X
    eor ws+$04FC,X
    bpl LB73F
    bcs LB741
    bcc LB751
LB73F:
    bcs LB751
LB741:
    ldy ws+$04FE,X
    lda ws+$04FF,X
    sty zpLINE
    sta zpLINE+1
    jsr SECUR
    jmp L8BA3

LB751:
    lda zpFORSTP
    sec
    sbc #$0F
    sta zpFORSTP
    ldy zpAECUR
    sty zpCURSOR
    jsr L8A97
    cmp #','
    bne LB7A1
    jmp LB695

LB766:
    jsr LB354
    lda zpFORSTP
    clc
    adc #$F4
    sta zp4B
    lda #$05+(ws/256)
    sta zp4C
    jsr LA500
    lda zpIACC
    sta zp37
    lda zpIACC+1
    sta zp38
    jsr LB4E9
    lda zpFORSTP
    sta zpTYPE
    clc
    adc #$F9
    sta zp4B
    lda #$05+(ws/256)
    sta zp4C
    jsr L9A5F
    beq LB741
    lda ws+$04F5,X
    bmi LB79D
    bcs LB741
    bcc LB751
LB79D:
    bcc LB741
    bcs LB751
LB7A1:
    jmp L8B96

LB7A4:
    brk
    dta $22
    dta tknFOR
    .if foldup == 1
        dta ' VARIABLE'
    .else
        dta ' variable'
    .endif
LB7B0:
    brk
    dta $23
    .if foldup == 1
        dta 'TOO MANY ', tknFOR, 'S'
    .else
        dta 'Too many ', tknFOR, 's'
    .endif
LB7BD:
    brk
    dta $24
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknTO
    brk

; FOR numvar = numeric TO numeric [STEP numeric]
; ==============================================
LB7C4:
    jsr L9582
    beq LB7A4
    bcs LB7A4
    jsr LBD94
    jsr L9841
    jsr LB4B1
    ldy zpFORSTP
    cpy #$96
    bcs LB7B0
    lda zp37
    sta ws+$0500,Y
    lda zp38
    sta ws+$0501,Y
    lda zp39
    sta ws+$0502,Y
    tax
    jsr L8A8C
    cmp #$B8
    bne LB7BD
    cpx #$05
    beq LB84F
    jsr L92DD
    ldy zpFORSTP
    lda zpIACC
    sta ws+$0508,Y
    lda zpIACC+1
    sta ws+$0509,Y
    lda zpIACC+2
    sta ws+$050A,Y
    lda zpIACC+3
    sta ws+$050B,Y
    lda #$01
    .if version < 3
        jsr LAED8
    .elseif version >= 3
        jsr XAED3
    .endif
    jsr L8A8C
    cmp #tknSTEP
    bne LB81F
    jsr L92DD
    ldy zpAECUR
LB81F:
    sty zpCURSOR
    ldy zpFORSTP
    lda zpIACC
    sta ws+$0503,Y
    lda zpIACC+1
    sta ws+$0504,Y
    lda zpIACC+2
    sta ws+$0505,Y
    lda zpIACC+3
    sta ws+$0506,Y
LB837:
    jsr FORR
    ldy zpFORSTP
    lda zpLINE
    sta ws+$050D,Y
    lda zpLINE+1
    sta ws+$050E,Y
    clc
    tya
    adc #$0F
    sta zpFORSTP
    jmp L8BA3

LB84F:
    jsr L9B29
    jsr L92FD
    lda zpFORSTP
    clc
    adc #$08
    sta zp4B
    lda #$05+(ws/256)
    sta zp4C
    jsr LA38D
    jsr FONE
    jsr L8A8C
    cmp #$88
    bne LB875
    jsr L9B29
    jsr L92FD
    ldy zpAECUR
LB875:
    sty zpCURSOR
    lda zpFORSTP
    clc
    adc #$03
    sta zp4B
    lda #$05+(ws/256)
    sta zp4C
    jsr LA38D
    jmp LB837

; GOSUB numeric
; =============
LB888:
    jsr LB99A
LB88B:
    jsr DONE
    ldy zpSUBSTP
    cpy #$1A
    bcs LB8A2
    lda zpLINE
    sta ws+$05CC,Y
    lda zpLINE+1
    sta ws+$05E6,Y
    inc zpSUBSTP
    bcc LB8D2

LB8A2:
    brk
    dta $25
    .if foldup == 1
        dta 'TOO MANY ', tknGOSUB, 'S'
    .else
        dta 'Too many ', tknGOSUB, 's'
    .endif
LB8AF:
    brk
    dta $26
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknGOSUB
    brk

; RETURN
; ======
LB8B6:
    jsr DONE         ; Check for end of statement
    ldx zpSUBSTP
    beq LB8AF         ; If GOSUB stack empty, error
    dec zpSUBSTP          ; Decrement GOSUB stack
    ldy ws+$05CB,X    ; Get stacked line pointer
    lda ws+$05E5,X
    sty zpLINE
    sta zpLINE+1          ; Set line pointer
    jmp L8B9B         ; Jump back to execution loop

; GOTO numeric
; ============
LB8CC:
    jsr LB99A
    jsr DONE         ; Find destination line, check for end of statement
LB8D2:
    lda zpTRFLAG
    beq LB8D9
    jsr L9905; If TRACE ON, print current line number
LB8D9:
    ldy zp3D
    lda zp3E          ; Get destination line address
LB8DD:
    sty zpLINE
    sta zpLINE+1          ; Set line pointer
    jmp L8BA3         ; Jump back to execution loop

; ON ERROR OFF
; ------------
LB8E4:
    jsr DONE         ; Check end of statement
    lda #<LB433
    sta zpERRORLH          ; ON ERROR OFF
    lda #>LB433
    sta zpERRORLH+1
    jmp L8B9B         ; Jump to execution loop

; ON ERROR [OFF | program ]
; -------------------------
LB8F2:
    jsr L8A97
    cmp #tknOFF
    beq LB8E4         ; ON ERROR OFF
    ldy zpCURSOR
    dey
    jsr CLYADP
    lda zpLINE
    sta zpERRORLH          ; Point ON ERROR pointer to here
    lda zpLINE+1
    sta zpERRORLH+1
    jmp L8B7D         ; Skip past end of line

LB90A:
    brk
    dta $27
    dta tknON
    .if foldup == 1
        dta ' SYNTAX'
    .else
        dta ' syntax'
    .endif
    brk

; ON [ERROR] [numeric]
; ====================
LB915:
    jsr L8A97         ; Skip spaces and get next character
    cmp #tknERROR
    beq LB8F2         ; Jump with ON ERROR
    dec zpCURSOR
    jsr L9B1D
    jsr L92F0
    ldy zpAECUR
    iny
    sty zpCURSOR
    cpx #tknGOTO
    beq LB931
    cpx #tknGOSUB
    bne LB90A
LB931:
    txa
    pha               ; Save GOTO/GOSUB token
    lda zpIACC+1
    ora zpIACC+2          ; Get IntA
    ora zpIACC+3
    bne LB97D         ; ON >255 - out of range, look for an ELSE
    ldx zpIACC
    beq LB97D         ; ON zero - out of range, look for an ELSE
    dex
    beq LB95C         ; Dec. counter, if zero use first destination
    ldy zpCURSOR          ; Get line index
LB944:
    lda (zpLINE),Y
    iny
    cmp #$0D
    beq LB97D         ; End of line - error
    cmp #':'
    beq LB97D         ; End of statement - error
    cmp #tknELSE
    beq LB97D         ; ELSE - drop everything else to here
    cmp #','
    bne LB944         ; No comma, keep looking
    dex
    bne LB944         ; Comma found, loop until count decremented to zero
    sty zpCURSOR          ; Store line index
LB95C:
    jsr LB99A         ; Read line number
    pla               ; Get stacked token back
    cmp #tknGOSUB
    beq LB96A         ; Jump to do GOSUB
    jsr SECUR         ; Update line index and check Escape
    jmp LB8D2         ; Jump to do GOTO

; Update line pointer so RETURN comes back to next statement
; ----------------------------------------------------------
LB96A:
    ldy zpCURSOR          ; Get line pointer
LB96C:
    lda (zpLINE),Y
    iny               ; Get character from line
    cmp #$0D
    beq LB977         ; End of line, RETURN to here
    cmp #':'
    bne LB96C         ; <colon>, return to here
LB977:
    dey
    sty zpCURSOR          ; Update line index to RETURN point
    jmp LB88B         ; Jump to do the GOSUB

; ON num out of range - check for an ELSE clause
; ----------------------------------------------
LB97D:
    ldy zpCURSOR          ; Get line index
    pla               ; Drop GOTO/GOSUB token
LB980:
    lda (zpLINE),Y
    iny               ; Get character from line
    cmp #tknELSE
    beq LB995         ; Found ELSE, jump to use it
    cmp #$0D
    bne LB980         ; Loop until end of line
    brk
    dta $28
    dta tknON
    .if foldup == 1
        dta ' RANGE'
    .else
        dta ' range'
    .endif
    brk

LB995:
    sty zpCURSOR
    jmp L98E3         ; Store line index and jump to GOSUB

LB99A:
    jsr L97DF
    bcs LB9AF         ; Embedded line number found
    jsr L9B1D
    jsr L92F0         ; Evaluate expression, ensure integer
    lda zpAECUR
    sta zpCURSOR          ; Line number low byte
    lda zpIACC+1
    and #$7F
    sta zpIACC+1          ; Line number high byte
                      ; Note - this makes goto $8000+10 the same as goto 10
LB9AF:
    jsr L9970
    bcs LB9B5
    rts               ; Look for line, error if not found

LB9B5:
    brk
    dta $29
    .if foldup == 1
        dta 'NO SUCH LINE'
    .else
        dta 'No such line'
    .endif
    brk

LB9C4:
    jmp L8C0E

LB9C7:
    jmp L982A

LB9CA:
    sty zpCURSOR
    jmp L8B98

; INPUT#channel, ...
; ------------------
LB9CF:
    dec zpCURSOR
    jsr LBFA9
    lda zpAECUR
    sta zpCURSOR
    sty zpCOEFP
LB9DA:
    jsr L8A97
    cmp #','
    bne LB9CA
    lda zpCOEFP
    pha
    jsr L9582
    beq LB9C7
    lda zpAECUR
    sta zpCURSOR
    pla
    sta zpCOEFP
    php
    jsr LBD94
    ldy zpCOEFP
    jsr OSBGET
    sta zpTYPE
    plp
    bcc LBA19
    lda zpTYPE
    bne LB9C4
    jsr OSBGET
    sta zp36
    tax
    beq LBA13
LBA0A:
    jsr OSBGET
    sta ws+$05FF,X
    dex
    bne LBA0A
LBA13:
    jsr L8C1E
    jmp LB9DA

LBA19:
    lda zpTYPE
    beq LB9C4
    bmi LBA2B
    ldx #$03
LBA21:
    jsr OSBGET
    sta zpIACC,X
    dex
    bpl LBA21
    bmi LBA39
LBA2B:
    ldx #$04
LBA2D:
    jsr OSBGET
    sta ws+$046C,X
    dex
    bpl LBA2D
    jsr LA3B2
LBA39:
    jsr LB4B4
    jmp LB9DA

LBA3F:
    pla
    pla
    jmp L8B98

; INPUT [LINE] [print items][variables]
; =====================================
LBA44:
    jsr L8A97         ; Get next non-space char
    cmp #'#'
    beq LB9CF         ; If '#' jump to do INPUT#
    cmp #tknLINE
    beq LBA52         ; If 'LINE', skip next with CS
    dec zpCURSOR
    clc               ; Step back to non-LINE char, set CC
LBA52:
    ror zpCOEFP
    lsr zpCOEFP          ; bit7=0, bit6=notLINE/LINE
    lda #$FF
    sta zpCOEFP+1
LBA5A:
    jsr L8E8A
    bcs LBA69         ; Process ' " TAB SPC, jump if none found
LBA5F:
    jsr L8E8A
    bcc LBA5F         ; Keep processing any print items
    ldx #$FF
    stx zpCOEFP+1
    clc
LBA69:
    php
    asl zpCOEFP
    plp
    ror zpCOEFP
    cmp #','
    beq LBA5A         ; ',' - jump to do next item
    cmp #';'
    beq LBA5A         ; ';' - jump to do next item
    dec zpCURSOR
    lda zpCOEFP
    pha
    lda zpCOEFP+1
    pha
    jsr L9582
    beq LBA3F
    pla
    sta zpCOEFP+1
    pla
    sta zpCOEFP
    lda zpAECUR
    sta zpCURSOR
    php
    bit zpCOEFP
    bvs LBA99
    lda zpCOEFP+1
    cmp #$FF
    bne LBAB0
LBA99:
    bit zpCOEFP
    bpl LBAA2
    lda #'?'
    jsr LB558
LBAA2:
    jsr LBBFC         ; Call MOS to input line, set COUNT=0
    sty zp36
    asl zpCOEFP
    clc
    ror zpCOEFP
    bit zpCOEFP
    bvs LBACD
LBAB0:
    sta zpAECUR
    lda #$00
    sta zpAELINE
    lda #$06+(ws/256)
    sta zpAELINE+1
    jsr LADAD
LBABD:
    jsr L8A8C
    cmp #','
    beq LBACA
    cmp #$0D
    bne LBABD
    ldy #$FE
LBACA:
    iny
    sty zpCOEFP+1
LBACD:
    plp
    bcs LBADC
    jsr LBD94
    jsr LAC34
    jsr LB4B4
    jmp LBA5A

LBADC:
    lda #$00
    sta zpTYPE
    jsr L8C21
    jmp LBA5A

; RESTORE [linenum]
; =================
LBAE6:
    ldy #$00
    sty zp3D          ; Set DATA pointer to PAGE
    ldy zpTXTP
    sty zp3E
    jsr L8A97
    dec zpCURSOR
    cmp #':'
    beq LBB07
    cmp #$0D
    beq LBB07
    cmp #tknELSE
    beq LBB07
    jsr LB99A
    ldy #$01
    jsr LBE55
LBB07:
    jsr DONE
    lda zp3D
    sta zpDATAP
    lda zp3E
    sta zpDATAP+1
    jmp L8B9B

LBB15:
    jsr L8A97
    cmp #','
    beq LBB1F
    jmp L8B96

; READ varname [,...]
; ===================
LBB1F:
    jsr L9582
    beq LBB15
    bcs LBB32
    jsr LBB50
    jsr LBD94
    jsr LB4B1
    jmp LBB40

LBB32:
    jsr LBB50
    jsr LBD94
    jsr LADAD
    sta zpTYPE
    jsr L8C1E
LBB40:
    clc
    lda zpAECUR
    adc zpAELINE
    sta zpDATAP
    lda zpAELINE+1
    adc #$00
    sta zpDATAP+1
    jmp LBB15

LBB50:
    lda zpAECUR
    sta zpCURSOR
    lda zpDATAP
    sta zpAELINE
    lda zpDATAP+1
    sta zpAELINE+1
    ldy #$00
    sty zpAECUR
    jsr L8A8C
    cmp #','
    beq LBBB0
    cmp #tknDATA
    beq LBBB0
    cmp #$0D
    beq LBB7A
LBB6F:
    jsr L8A8C
    cmp #','
    beq LBBB0
    cmp #$0D
    bne LBB6F
LBB7A:
    ldy zpAECUR
    lda (zpAELINE),Y
    bmi LBB9C
    iny
    iny
    lda (zpAELINE),Y
    tax
LBB85:
    iny
    lda (zpAELINE),Y
    cmp #$20
    beq LBB85
    cmp #tknDATA
    beq LBBAD
    txa
    clc
    adc zpAELINE
    sta zpAELINE
    bcc LBB7A
    inc zpAELINE+1
    bcs LBB7A
LBB9C:
    brk
    dta $2A
    .if foldup == 1
        dta 'OUT OF '
    .else
        dta 'Out of '
    .endif
    dta tknDATA
LBBA6:
    brk
    dta $2B
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknREPEAT
    brk

LBBAD:
    iny
    sty zpAECUR
LBBB0:
    rts

; UNTIL numeric
; =============
LBBB1:
    jsr L9B1D
    jsr L984C
    jsr L92EE
    ldx zpDOSTKP
    beq LBBA6
    lda zpIACC
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq LBBCD
    dec zpDOSTKP
    jmp L8B9B

LBBCD:
    ldy ws+$05A3,X
    lda ws+$05B7,X
    jmp LB8DD

LBBD6:
    brk
    dta $2C
    .if foldup == 1
        dta 'TOO MANY '
        dta tknREPEAT, 'S'
    .else
        dta 'Too many '
        dta tknREPEAT, 's'
    .endif
    brk

; REPEAT
; ======
LBBE4:
    ldx zpDOSTKP
    cpx #$14
    bcs LBBD6
    jsr CLYADP
    lda zpLINE
    sta ws+$05A4,X
    lda zpLINE+1
    sta ws+$05B8,X
    inc zpDOSTKP
    jmp L8BA3

; Input string to string buffer
; -----------------------------
LBBFC:
    ldy #$00
    lda #$06+(ws/256)     ; String buffer at $0600
    bne LBC09

; Print character, read input line
; --------------------------------
LBC02:
    jsr LB558         ; Print character
    ldy #$00
    lda #$07+(ws/256)     ; $AAYY=input buffer at $0700

LBC09:
    sty zp37
    sta zp38          ; $37/8=>input buffer

; Manually implement RDLINE (OSWORD 0)
; ------------------------------------
    .ifdef MOS_ATOM
LDBE4:
        jsr OSRDCH    ; Wait for character
        cmp #$1B
        beq LDC21     ; Escape
        cmp #$7F
        bne LDBFA     ; Not Delete
        cpy #$00
        beq LDBE4     ; Nothing to delete
        jsr OSWRCH    ; VDU 127
        dey
        jmp LDBE4     ; Dec. counter, loop back
     
LDBFA:
        cmp #$15
        bne LDC0B     ; Not Ctrl-U
        tya
        beq LDBE4
        lda #$7F
LDC03:
        jsr OSWRCH
        dey
        bne LDC03
        beq LDBE4
     
LDC0B:
        sta (zp37),Y      ; Store character
        cmp #$0D
        beq LBC25     ; Return - finish
        cpy #$EE
        bcs LDC1E     ; Maximum length
        cmp #$20
        bcs LDC1A     ; Control character
        dey
LDC1A:
        iny
        jsr OSWRCH    ; Inc. counter, print character
LDC1E:
        jmp LDBE4     ; Loop for more
LDC21:
    .endif

; BBC - Call MOS to read a line
; -----------------------------
    .ifdef MOS_BBC
        lda #$EE
        sta zp39      ; Maximum length
        lda #$20
        sta zp3A      ; Lowest acceptable character
        ldy #$FF
        sty zp3B      ; Highest acceptable character
        iny
        ldx #$37      ; XY=>control block at $0037
        tya
        jsr OSWORD    ; Call OSWORD 0 to read line of text
        bcc LBC28     ; CC, Escape not pressed, exit and set COUNT=0
    .endif
    jmp L9838         ; Escape

LBC25:
    jsr OSNEWL
LBC28:
    lda #$00
    sta zpTALLY          ; Set COUNT to zero
    rts

LBC2D:
    jsr L9970
    bcs LBC80
    lda zp3D
    sbc #$02
    sta zp37
    sta zp3D
    sta zpTOP
    lda zp3E
    sbc #$00
    sta zp38
    sta zpTOP+1
    sta zp3E
    ldy #$03
    lda (zp37),Y
    clc
    adc zp37
    sta zp37
    bcc LBC53
    inc zp38
LBC53:
    ldy #$00
LBC55:
    lda (zp37),Y
    sta (zpTOP),Y
    cmp #$0D
    beq LBC66
LBC5D:
    iny
    bne LBC55
    inc zp38
    inc zpTOP+1
    bne LBC55
LBC66:
    iny
    bne LBC6D
    inc zp38
    inc zpTOP+1
LBC6D:
    lda (zp37),Y
    sta (zpTOP),Y
    bmi LBC7C
    jsr LBC81
    jsr LBC81
    jmp LBC5D

LBC7C:
    jsr LBE92
    clc
LBC80:
    rts

LBC81:
    iny
    bne LBC88
    inc zpTOP+1
    inc zp38
LBC88:
    lda (zp37),Y
    sta (zpTOP),Y
    rts

LBC8D:
    sty zp3B
    jsr LBC2D
    ldy #$07+(ws/256)
    sty zp3C
    ldy #$00
    lda #$0D
    cmp (zp3B),Y
    beq LBD10
LBC9E:
    iny
    cmp (zp3B),Y
    bne LBC9E
    iny
    iny
    iny
    sty zp3F
    inc zp3F
    lda zpTOP
    sta zp39
    lda zpTOP+1
    sta zp3A
    jsr LBE92
    sta zp37
    lda zpTOP+1
    sta zp38
    dey
    lda zpHIMEM
    cmp zpTOP
    lda zpHIMEM+1
    sbc zpTOP+1
    bcs LBCD6
    jsr LBE6F
    jsr LBD20
    brk
    dta 0
    dta tknLINE
    .if foldup == 1
        dta ' SPACE'
    .else
        dta ' space'
    .endif
    brk

LBCD6:
    lda (zp39),Y
    sta (zp37),Y
    tya
    bne LBCE1
    dec zp3A
    dec zp38
LBCE1:
    dey
    tya
    adc zp39
    ldx zp3A
    bcc LBCEA
    inx
LBCEA:
    cmp zp3D
    txa
    sbc zp3E
    bcs LBCD6
    sec
    ldy #$01
    lda zpIACC+1
    sta (zp3D),Y
    iny
    lda zpIACC
    sta (zp3D),Y
    iny
    lda zp3F
    sta (zp3D),Y
    jsr LBE56
    ldy #$FF
LBD07:
    iny
    lda (zp3B),Y
    sta (zp3D),Y
    cmp #$0D
    bne LBD07
LBD10:
    rts

; RUN
; ===
LBD11:
    jsr DONE
LBD14:
    jsr LBD20
    lda zpTXTP
    sta zpLINE+1          ; Point PtrA to PAGE
    stx zpLINE
    jmp L8B0B

; Clear BASIC heap, stack and DATA pointer
; ========================================
LBD20:
    lda zpTOP
    sta zpLOMEM
    sta zpFSA          ; LOMEM=TOP, VAREND=TOP
    lda zpTOP+1
    sta zpLOMEM+1
    sta zpFSA+1
    jsr LBD3A         ; Clear DATA and stack
LBD2F:
    ldx #$80
    lda #$00
LBD33:
    sta ws+$047F,X
    dex
    bne LBD33
    rts               ; Clear dynamic variables list

; Clear DATA pointer and BASIC stack
; ==================================
LBD3A:
    lda zpTXTP
    sta zpDATAP+1          ; DATA pointer hi=PAGE hi
    lda zpHIMEM
    sta zpAESTKP
    lda zpHIMEM+1
    sta zpAESTKP+1          ; STACK=HIMEM
    lda #$00
    sta zpDOSTKP
    sta zpFORSTP
    sta zpSUBSTP; Clear REPEAT, FOR, GOSUB stacks
    sta zpDATAP
    rts               ; DATA pointer=PAGE

LBD51:
    lda zpAESTKP
    sec
    sbc #$05
    jsr LBE2E
    ldy #$00
    lda zp30
    sta (zpAESTKP),Y
    iny
    lda zp2E
    and #$80
    sta zp2E
    lda zp31
    and #$7F
    ora zp2E
    sta (zpAESTKP),Y
    iny
    lda zp32
    sta (zpAESTKP),Y
    iny
    lda zp33
    sta (zpAESTKP),Y
    iny
    lda zp34
    sta (zpAESTKP),Y
    rts

LBD7E:
    lda zpAESTKP
    clc
    sta zp4B
    adc #$05
    sta zpAESTKP
    lda zpAESTKP+1
    sta zp4C
    adc #$00
    sta zpAESTKP+1
    rts

LBD90:
    beq LBDB2
    bmi LBD51
LBD94:
    lda zpAESTKP
    sec
    sbc #$04
LBD99:
    jsr LBE2E
    ldy #$03
    lda zpIACC+3
    sta (zpAESTKP),Y
    dey
    lda zpIACC+2
    sta (zpAESTKP),Y
    dey
    lda zpIACC+1
    sta (zpAESTKP),Y
    dey
    lda zpIACC
    sta (zpAESTKP),Y
    rts

; Stack the current string
; ========================
LBDB2:
    clc
    lda zpAESTKP
    sbc zp36          ; stackbot=stackbot-length-1
    jsr LBE2E         ; Check enough space
    ldy zp36
    beq LBDC6         ; Zero length, just stack length
LBDBE:
    lda ws+$05FF,Y
    sta (zpAESTKP),Y      ; Copy string to stack
    dey
    bne LBDBE         ; Loop for all characters
LBDC6:
    lda zp36
    sta (zpAESTKP),Y      ; Copy string length
    rts

; Unstack a string
; ================
LBDCB:
    ldy #$00
    lda (zpAESTKP),Y      ; Get stacked string length
    sta zp36
    beq LBDDC
    tay               ; If zero length, just unstack length
LBDD4:
    lda (zpAESTKP),Y
    sta ws+$05FF,Y    ; Copy string to string buffer
    dey
    bne LBDD4         ; Loop for all characters
LBDDC:
    ldy #$00
    lda (zpAESTKP),Y      ; Get string length again
    sec
LBDE1:
    adc zpAESTKP
    sta zpAESTKP          ; Update stack pointer
    bcc LBE0A
    inc zpAESTKP+1
    rts

; Unstack an integer to IntA
; --------------------------
LBDEA:
    ldy #$03
    lda (zpAESTKP),Y
    sta zpIACC+3
    dey               ; Copy to IntA
    lda (zpAESTKP),Y
    sta zpIACC+2
    dey
    lda (zpAESTKP),Y
    sta zpIACC+1
    dey
    lda (zpAESTKP),Y
    sta zpIACC
LBDFF:
    clc
    lda zpAESTKP
    adc #$04
    sta zpAESTKP          ; Drop 4 bytes from stack
    bcc LBE0A
    inc zpAESTKP+1
LBE0A:
    rts

; Unstack an integer to zero page
; -------------------------------
LBE0B:
    ldx #$37
LBE0D:
    ldy #$03
    lda (zpAESTKP),Y
    sta zp+3,X
    dey
    lda (zpAESTKP),Y
    sta zp+2,X
    dey
    lda (zpAESTKP),Y
    sta zp+1,X
    dey
    lda (zpAESTKP),Y
    sta zp+0,X
    clc
    lda zpAESTKP
    adc #$04
    sta zpAESTKP          ; Drop 4 bytes from stack
    bcc LBE0A
    inc zpAESTKP+1
    rts

LBE2E:
    sta zpAESTKP
    bcs LBE34
    dec zpAESTKP+1
LBE34:
    ldy zpAESTKP+1
    cpy zpFSA+1
    bcc LBE41
    bne LBE40
    cmp zpFSA
    bcc LBE41
LBE40:
    rts

LBE41:
    jmp L8CB7

LBE44:
    lda zpIACC
    sta zp+0,X
    lda zpIACC+1
    sta zp+1,X
    lda zpIACC+2
    sta zp+2,X
    lda zpIACC+3
    sta zp+3,X
    rts

LBE55:
    clc
LBE56:
    tya
    adc zp3D
    sta zp3D
    bcc LBE5F
    inc zp3E
LBE5F:
    ldy #$01
    rts

LBE62:
    jsr LBEDD
    tay
    lda #$FF          ; FILE.LOAD=PAGE

    .ifdef MOS_ATOM
        sta F_EXEC+0
        ldx #$37      ; FILE.EXEC=$FF, load to specified address
        sec
        jsr OSLOAD
    .endif

    .ifdef MOS_BBC
        sty F_EXEC+0
        ldx #$37      ; FILE.EXEC=0, load to specified address
        jsr OSFILE
    .endif

; Scan program to check consistancy and find TOP
; ----------------------------------------------
LBE6F:
    lda zpTXTP
    sta zpTOP+1
    ldy #$00
    sty zpTOP
    iny               ; Point TOP to PAGE
LBE78:
    dey
    lda (zpTOP),Y      ; Get byte preceding line
    cmp #$0D
    bne LBE9E         ; Not <cr>, jump to 'Bad program'
    iny               ; Step to line number/terminator
    lda (zpTOP),Y
    bmi LBE90         ; b7 set, end of program
    ldy #$03          ; Point to line length
    lda (zpTOP),Y
    beq LBE9E         ; Zero length, jump to 'Bad program'
    clc
    jsr LBE93         ; Update TOP to point to next line
    bne LBE78         ; Loop to check next line

; End of program found, set TOP
; -----------------------------
LBE90:
    iny
    clc
LBE92:
    tya
LBE93:
    adc zpTOP
    sta zpTOP          ; TOP=TOP+A
    bcc LBE9B
    inc zpTOP+1
LBE9B:
    ldy #$01
    rts               ; Return Y=1, NE

; Report 'Bad program' and jump to immediate mode
; -----------------------------------------------
LBE9E:
    jsr LBFCF         ; Print inline text
    dta 13
    .if foldup == 1
        dta 'BAD PROGRAM'
    .else
        dta 'Bad program'
    .endif
    dta 13
    nop
    jmp L8AF6         ; Jump to immediate mode

; Point $37/8 to <cr>-terminated string in string buffer
; ------------------------------------------------------
LBEB2:
    lda #$00
    sta zp37
    lda #$06+(ws/256)
    sta zp38
LBEBA:
    ldy zp36
    lda #$0D
    sta ws+$0600,Y
    rts

; OSCLI string$ - Pass string to OSCLI to execute
; ===============================================
LBEC2:
    jsr LBED2         ; $37/8=>cr-string


    .ifdef MOS_ATOM
        jsr cmdStar1
        jmp L8B9B     ; Call Atom OSCLI and return to execution loop
     

; Embedded star command
; ---------------------
cmdStar:
        stx zp37
        sty zp38      ; $37/8=>cr-string
cmdStar1:
        ldy #$FF
cmdStarLp1:
        iny
        lda (zp37),Y
        cmp #'*'
        beq cmdStarLp1    ; Skip leading stars
        ldx #0
cmdStarLp2:
        lda (zp37),Y
        sta $0100,X       ; Copy string onto stack
        iny
        inx
        cmp #$0D
        bne cmdStarLp2    ; Atom OSCLI passed string at $100
        jmp OS_CLI
    .endif

    .ifdef MOS_BBC
        ldx #$00
        ldy #>(ws+$0600)
        jsr OS_CLI
        jmp L8B9B     ; Call OSCLI and return to execution loop
    .endif

LBECF:
    jmp L8C0E

LBED2:
    jsr L9B1D
    bne LBECF         ; Evaluate expression, error if not string
    jsr LBEB2
    jmp L984C         ; Convert to <cr>-string, check end of statement

; Set FILE.LOAD to MEMHI.PAGE
; ---------------------------
LBEDD:
    jsr LBED2
    dey
    sty F_LOAD+0      ; LOAD.lo=$00
    lda zpTXTP
    sta F_LOAD+1      ; LOAD.hi=PAGEhi
LBEE7:
    .ifdef MOS_BBC
        lda #$82
        jsr OSBYTE    ; Get memory base high word
        stx F_LOAD+2
        sty F_LOAD+3      ; Set LOAD high word
        lda #$00
    .endif
    rts

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
    jsr LBE6F         ; Check program, set TOP

    .if version < 3
        lda zpTOP
        sta F_END+0       ; Set FILE.END to TOP
        lda zpTOP+1
        sta F_END+1
        lda #<ENTRY
        sta F_EXEC+0      ; Set FILE.EXEC to STARTUP
        lda #>ENTRY
        sta F_EXEC+1
        lda zpTXTP
        sta F_START+1     ; Set FILE.START to PAGE
        jsr LBEDD     ; Set FILE.LOAD to PAGE
    .endif
    .if version < 3
        .ifdef MOS_BBC
            stx F_EXEC+2 
            sty F_EXEC+3      ; Set address high words
            stx F_START+2
            sty F_START+3
            stx F_END+2  
            sty F_END+3
        .endif
        .ifdef MOS_ATOM
            sty F_START+0    ; Low byte of FILE.START
        .endif
        .ifdef MOS_BBC
            sta F_START+0    ; Low byte of FILE.START
        .endif
    .endif

    .if version >= 3
        jsr LBEDD     ; Set FILE.LOAD to PAGE
    .endif
    .if version >= 3
        .ifdef MOS_BBC
            stx F_EXEC+2 
            sty F_EXEC+3      ; Set address high words
            stx F_START+2
            sty F_START+3
            stx F_END+2  
            sty F_END+3
        .endif
        .ifdef MOS_ATOM
            sty F_START+0    ; Low byte of FILE.START
        .endif
        .ifdef MOS_BBC
            sta F_START+0    ; Low byte of FILE.START
        .endif
    .endif
    .if version >= 3
        ldx zpTOP
        stx F_END+0       ; Set FILE.END to TOP
        ldx zpTOP+1
        stx F_END+1
        ldx #<ENTRY
        stx F_EXEC+0      ; Set FILE.EXEC to STARTUP
        ldx #>ENTRY
        stx F_EXEC+1
        ldx zpTXTP
        stx F_START+1     ; High byte of FILE.START=PAGE
    .endif
    tay
    ldx #$37
    .ifdef MOS_ATOM
        sec
        jsr OSSAVE
    .endif
    .ifdef MOS_BBC
        jsr OSFILE    
    .endif
    jmp L8B9B

; LOAD string$
; ============
LBF24:
    jsr LBE62
    jmp L8AF3         ; Do LOAD, jump to immediate mode

; CHAIN string$
; =============
LBF2A:
    jsr LBE62
    jmp LBD14         ; Do LOAD, jump to execution loop

; PTR#numeric=numeric
; ===================
LBF30:
    jsr LBFA9
    pha               ; Evaluate #handle
    jsr L9813
    jsr L92EE         ; Step past '=', evaluate integer
    pla
    tay
    ldx #$2A          ; Get handle, point to IntA
    .ifdef MOS_ATOM
        jsr OSSTAR         
    .endif
    .ifdef MOS_BBC
        lda #$01
        jsr OSARGS
    .endif
    jmp L8B9B         ; Jump to execution loop

; =EXT#numeric - Read file pointer via OSARGS
; ===========================================
LBF46:
    sec               ; Flag to do =EXT

; =PTR#numeric - Read file pointer via OSARGS
; ===========================================
LBF47:
    lda #$00
    rol               ; A=0 or 1 for =PTR or =EXT
    .ifdef MOS_BBC
        rol
    .endif
    pha               ; Atom - A=0/1, BBC - A=0/2
    jsr LBFB5
    ldx #$2A
    pla               ; Evaluate #handle, point to IntA
    .ifdef MOS_ATOM
        jsr OSRDAR
    .endif
    .ifdef MOS_BBC
        jsr OSARGS
    .endif
    lda #$40
    rts               ; Return integer

; BPUT#numeric, numeric
; =====================
LBF58:
    jsr LBFA9
    pha               ; Evaluate #handle
    jsr L8AAE
    jsr L9849
    jsr L92EE
    pla
    tay
    lda zpIACC
    jsr OSBPUT
    jmp L8B9B         ; Call OSBPUT, jump to execution loop

; =BGET#numeric
; =============
LBF6F:
    jsr LBFB5
    jsr OSBGET        ; Evaluate #handle
    .if version < 3
        jmp LAED8     ; Jump to return 8-bit integer
    .elseif version >= 3
        jmp XAED3     ; Jump to return 8-bit integer
    .endif

; OPENIN f$ - Call OSFIND to open file for input
; ==============================================
LBF78:
    .ifdef MOS_ATOM
        sec           ; SEC=OPENUP
        bcs LBF82     
    .endif
    .ifdef MOS_BBC
        lda #$40      ; $40=OPENUP
        bne LBF82
    .endif

; OPENOUT f$ - Call OSFIND to open file for output
; ================================================
LBF7C:
    .ifdef MOS_ATOM
        clc           ; CLC=OPENOUT
        bcc LBF82     
    .endif
    .ifdef MOS_BBC
        lda #$80      ; 80=OPENOUT
        bne LBF82
    .endif

; OPENUP f$ - Call OSFIND to open file for update
; ===============================================
LBF80:
    .ifdef MOS_ATOM
        sec           ; SEC=OPENUP
    .endif
    .ifdef MOS_BBC
        lda #$C0      ; C0=OPENUP
    .endif
LBF82:
    .ifdef MOS_ATOM
        php       
    .endif
    .ifdef MOS_BBC
        pha       
    .endif
    jsr LADEC
    bne LBF96         ; Evaluate, if not string, jump to error

    .ifdef MOS_ATOM
        jsr LBEB2     ; Terminate string with <cr>, point $37/8=>string
        ldx #$37
        plp           ; Point to string pointer, get action back
    .endif

    .ifdef MOS_BBC
        jsr LBEBA     ; Terminate string with <cr>
        ldx #$00
        ldy #$06
        pla           ; Point to string buffer, get action back
    .endif

    jsr OSFIND        ; Pass to OSFIND, jump to return integer from A
    .if version < 3
        jmp LAED8
    .elseif version >= 3
        jmp XAED3
    .endif

LBF96:
    jmp L8C0E         ; Jump to 'Type mismatch' error

; CLOSE#numeric
; =============
LBF99:
    jsr LBFA9
    jsr L9852         ; Evaluate #handle, check end of statement
    ldy zpIACC          ; Get handle from IntA
    .ifdef MOS_ATOM
        jsr OSSHUT         
    .endif
    .ifdef MOS_BBC
        lda #$00
        jsr OSFIND
    .endif
    jmp L8B9B         ; Jump back to execution loop

; Copy PtrA to PtrB, then get handle
; ==================================
LBFA9:
    lda zpCURSOR
    sta zpAECUR          ; Set PtrB to program pointer in PtrA
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1

; Check for '#', evaluate channel
; ===============================
LBFB5:
    jsr L8A8C         ; Skip spaces
    cmp #'#'          ; If not '#', jump to give error
    .if version < 3
        bne LBFC3
    .elseif version >= 3
        bne LBFF4
    .endif
    jsr L92E3         ; Evaluate as integer
LBFBF:
    ldy zpIACC
    tya               ; Get low byte and return
NULLRET:
    rts

    .if version < 3
LBFC3:
        brk
        dta $2D
        .if foldup == 1
            dta 'MISSING #'
        .else
            dta 'Missing #'
        .endif
        brk
    .endif

; Print inline text
; =================
LBFCF:
    pla
    sta zp37
    pla
    sta zp38          ; Pop return address to pointer
    ldy #$00
    beq LBFDC         ; Jump into loop
LBFD9:
    jsr OSASCI        ; Print character
LBFDC:
    jsr L894B
    bpl LBFD9         ; Update pointer, get character, loop if b7=0
    jmp (zp37)        ; Jump back to program

; REPORT
; ======
LBFE4:
    jsr DONE
    jsr LBC25         ; Check end of statement, print newline, clear COUNT
    ldy #$01
LBFEC:
    lda (FAULT),Y
    beq LBFF6         ; Get byte, exit if $00 terminator
    jsr LB500
    iny
    bne LBFEC         ; Print character or token, loop for next
LBFF6:
    jmp L8B9B         ; Jump to main execution loop

    .if version >= 3
LBFF4:
        brk
        dta $2D
        .if foldup == 1
            dta 'MISSING #'
        .else
            dta 'Missing #'
        .endif
        brk
    .endif

    .if * > [load + $4000]
        .error "***WARNING: Code overrun"
    .endif

    .if version < 3 && [[*+6]&$ff] > 6
        brk
        dta 'Roger'
        brk
    .endif

    .if [[*+3]&$ff] > 3
        .if version == 2
            dta '2', '.', '0', '0'
        .elseif version == 3
            .if minorversion != 10
                dta '3'
            .else
                dta '3', '.', '1'
                .if .def BUILD_ATOM_BASIC310
                    dta '0'
                .endif
            .endif
        .endif
    .endif

    .if * > [load + $4000]
        .error "***WARNING: Code overrun"
    .endif

    .align load + $4000, 0
LC000:
