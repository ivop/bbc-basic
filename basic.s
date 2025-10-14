; ----------------------------------------------------------------------------
;
; BBC BASIC II/III -- NMOS 6502
;
; Conversion to mads, labelling, bug fixes, and more comments by
; Ivo van Poorten, September 2025
;
; Based on source reconstruction and commentary © 2018 J.G. Harston
; https://mdfs.net/Software/BBCBasic/BBC/
;
; BBC BASIC Copyright © 1982/1983 Acorn Computer and Sophie Wilson
;
; References used:
;   Advanced BASIC ROM USer Guide
;   AcornCmosBasic (https://github.com/stardot/AcornCmosBasic)
;   AcornDmosBasic (https://github.com/stardot/AcornDmosBasic)
;   AcornBasic128 (github.com/stardot/AcornBasic128)
;   BBC Micro Compendium
;
; ----------------------------------------------------------------------------


    opt h-            ; No Atari header

; ----------------------------------------------------------------------------

; Important for future ports, ws MUST be page aligned, or a lot of things
; will break (i.e. testing MSB for end of STRACC)

; ----------------------------------------------------------------------------

    .if .def BUILD_BBC_BASIC2 || .def BUILD_BBC_BASIC3 || .def BUILD_BBC_BASIC310HI
        TARGET_BBC = 1
        MOS_BBC    = 1

        .if .def BUILD_BBC_BASIC2
            romstart      = $8000     ; Code start address
            VERSION       = 2
            MINORVERSION  = 0
        .elseif .def BUILD_BBC_BASIC3
            romstart      = $8000     ; Code start address
            VERSION       = 3
            MINORVERSION  = 0
        .elseif .def BUILD_BBC_BASIC310HI
            romstart      = $b800     ; Code start address
            VERSION       = 3
            MINORVERSION  = 10
        .endif

        split     = 0
        foldup    = 0
        title     = 0
        workspace = $0400
        membot    = 0           ; Use OSBYTE to find memory limits
        memtop    = 0           ; ...

        zp      = $00           ; Start of ZP addresses

        FAULT  = $fd            ; Pointer to error block
        ESCFLG = $ff            ; Escape pending flag

        BRKV   = $0202
        WRCHV  = $020E

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
            romstart  = $a000            ; Code start address
            workspace = $2800
            membot    = $3000
            ESCFLG    = $0e21            ; Escape pending flag
        .elseif .def TARGET_ATOM
            romstart  = $4000            ; Code start address
            workspace = $9c00
            membot    = $2800
            ESCFLG    = $b001            ; Escape pending flag
        .endif

        memtop  = romstart    ; Top of memory is start of code
        zp      = $00         ; Start of ZP addresses

        FAULT  = zp4F         ; Pointer to error block

        BRKV=$0202
        WRCHV=$0208

        OSBYTE=NULLRET
        OSWORD=NULLRET

    .elseif .def BUILD_C64_BASIC2

        TARGET_C64 = 1
        MOS_BBC    = 1

        romstart      = $b800     ; Code start address
        VERSION       = 2
        MINORVERSION  = 0

        split     = 0
        foldup    = 0
        title     = 0
        workspace = $0400
        membot    = 0           ; Use OSBYTE to find memory limits
        memtop    = 0           ; ...

        zp      = $00           ; Start of ZP addresses

        zpLOMEM = $50           ; Avoid 6510 registers

        FAULT  = $fd            ; Pointer to error block
        ESCFLG = $ff            ; Escape pending flag

        BRKV=$0316    ; Fixed
        WRCHV=0       ; Fixed

    .else
        .error "Please specify your build (i.e. -d:BUILD_BBC_BASIC2=1)"
    .endif

    .if .def MOS_BBC

        OS_CLI = $FFF7
        OSBYTE = $FFF4
        OSWORD = $FFF1
        OSWRCH = $FFEE
        OSNEWL = $FFE7
        OSASCI = $FFE3
        OSRDCH = $FFE0
        OSFILE = $FFDD
        OSARGS = $FFDA
        OSBGET = $FFD7
        OSBPUT = $FFD4
        OSFIND = $FFCE

        F_LOAD  = zpWORK+2      ; LOAD/SAVE control block
        F_EXEC  = F_LOAD+4
        F_START = F_LOAD+8
        F_END   = F_LOAD+12

    .elseif .def MOS_ATOM

        OS_CLI=$FFF7
        OSWRCH=$FFF4
        OSNEWL=$FFED
        OSASCI=$FFE9
        OSRDCH=$FFE3
        OSLOAD=$FFE0
        OSSAVE=$FFDD
        OSRDAR=$FFDA
        OSSTAR=$FFD7
        OSBGET=$FFD4
        OSBPUT=$FFD1
        OSFIND=$FFCE
        OSSHUT=$FFCB

        F_LOAD  = zpWORK+2    ; LOAD/SAVE control block
        F_EXEC  = F_LOAD+2
        F_START = F_LOAD+4
        F_END   = F_LOAD+6

    .else
        .error "No MOS API specified"
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
tknCONST    = $8D
tknOPENIN   = $8E
tknPTR      = $8F
tknPAGE     = $90
tknTIME     = $91
tknLOMEM    = $92
tknHIMEM    = $93
tknABS      = $94
tknACS      = $95
tknADVAL    = $96
tknASC      = $97
tknASN      = $98
tknATN      = $99
tknBGET     = $9A
tknCOS      = $9B
tknCOUNT    = $9C
tknDEG      = $9D
tknERL      = $9E
tknERR      = $9F
tknEVAL     = $A0
tknEXP      = $A1
tknEXT      = $A2
tknFALSE    = $A3
tknFN       = $A4
tknGET      = $A5
tknINKEY    = $A6
tknINSTR    = $A7
tknINT      = $A8
tknLEN      = $A9
tknLN       = $AA
tknLOG      = $AB
tknNOT      = $AC
tknOPENUP   = $AD
tknOPENOUT  = $AE
tknPI       = $AF
tknPOINT    = $B0
tknPOS      = $B1
tknRAD      = $B2
tknRND      = $B3
tknSGN      = $B4
tknSIN      = $B5
tknSQR      = $B6
tknTAN      = $B7
tknTO       = $B8
tknTRUE     = $B9
tknUSR      = $BA
tknVAL      = $BB
tknVPOS     = $BC
tknCHRD     = $BD
tknGETD     = $BE
tknINKEYD   = $BF
tknLEFTD    = $C0
tknMIDD     = $C1
tknRIGHTD   = $C2
tknSTRD     = $C3
tknSTRINGD  = $C4
tknEOF      = $C5
tknAUTO     = $C6   ; first "command" token
tknDELETE   = $C7
tknLOAD     = $C8
tknLIST     = $C9
tknNEW      = $CA
tknOLD      = $CB
tknRENUMBER = $CC
tknSAVE     = $CD
tknPTR2     = $CF   ; tknPTR   + $40, see bit 6 of token table flags
tknPAGE2    = $D0   ; tknPAGE  + $40
tknTIME2    = $D1   ; tknTIME  + $40
tknLOMEM2   = $D2   ; tknLOMEM + $40
tknHIMEM2   = $D3   ; tknHIMEM + $40
tknSOUND    = $D4
tknBPUT     = $D5
tknCALL     = $D6
tknCHAIN    = $D7
tknCLEAR    = $D8
tknCLOSE    = $D9
tknCLG      = $DA
tknCLS      = $DB
tknDATA     = $DC
tknDEF      = $DD
tknDIM      = $DE
tknDRAW     = $DF
tknEND      = $E0
tknENDPROC  = $E1
tknENVELOPE = $E2
tknFOR      = $E3
tknGOSUB    = $E4
tknGOTO     = $E5
tknGCOL     = $E6
tknIF       = $E7
tknINPUT    = $E8
tknLET      = $E9
tknLOCAL    = $EA
tknMODE     = $EB
tknMOVE     = $EC
tknNEXT     = $ED
tknON       = $EE
tknVDU      = $EF
tknPLOT     = $F0
tknPRINT    = $F1
tknPROC     = $F2
tknREAD     = $F3
tknREM      = $F4
tknREPEAT   = $F5
tknREPORT   = $F6
tknRESTORE  = $F7
tknRETURN   = $F8
tknRUN      = $F9
tknSTOP     = $FA
tknCOLOR    = $FB
tknTRACE    = $FC
tknUNTIL    = $FD
tknWIDTH    = $FE
tknOSCLI    = $FF

; ----------------------------------------------------------------------------

    org romstart

START_OF_ROM:

; ----------------------------------------------------------------------------

; Atom/System Code Header
; =======================

.ifdef MOS_ATOM
    jsr VSTRNG         ; Print inline text
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
    dta $60                             ; ROM type = Lang+Tube+6502 BASIC
    dta copyright_string - romstart     ; Offset to copyright string
    dta [version*2]-3                   ; Version 2 = $01, Version 3 = $03
    dta 'BASIC'
copyright_string:
    dta 0
    dta '(C)198', [$30+version], ' Acorn', 10, 13, 0
    dta a(romstart), a(0)
.endif

; LANGUAGE STARTUP
; ================

ENTRY:
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
    sty zpTXTP        ; PAGE

    ldx #$00
    stx zpLISTOP          ; Set LISTO to 0
    stx VARL_AT+2
    stx VARL_AT+3         ; Set @% to $0000xxxx
    dex
    stx zpWIDTHV          ; Set WIDTH to $FF

    ldx #$0A
    stx VARL_AT
    dex
    stx VARL_AT+1      ; Set @% to $0000090A        9.10

    lda #$01
    and zpSEED+4
    ora zpSEED         ; Check RND seed
    ora zpSEED+1
    ora zpSEED+2
    ora zpSEED+3
    bne RNDOK          ; If nonzero, skip past

    lda #'A'           ; Set RND seed to $575241
    sta zpSEED
    lda #'R'
    sta zpSEED+1
    lda #'W'
    sta zpSEED+2       ; "ARW" - Acorn Roger Wilson?

RNDOK:
    lda #<BREK
    sta BRKV+0         ; Set up error handler
    lda #>BREK
    sta BRKV+1
    cli
    jmp FORMAT         ; Enable IRQs, jump to immediate loop

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

TOKENS:
    dta 'AND'     , tknAND,         $00      ; 00000000
    dta 'ABS'     , tknABS,         $00      ; 00000000
    dta 'ACS'     , tknACS,         $00      ; 00000000
    dta 'ADVAL'   , tknADVAL,       $00      ; 00000000
    dta 'ASC'     , tknASC,         $00      ; 00000000
    dta 'ASN'     , tknASN,         $00      ; 00000000
    dta 'ATN'     , tknATN,         $00      ; 00000000
    dta 'AUTO'    , tknAUTO,        $10      ; 00010000
    dta 'BGET'    , tknBGET,        $01      ; 00000001
    dta 'BPUT'    , tknBPUT,        $03      ; 00000011
    .if version == 2 || (version == 3 && minorversion >= 10)
        dta 'COLOUR', tknCOLOR,     $02      ; 00000010
    .elseif version == 3
        dta 'COLOR', tknCOLOR,      $02      ; 00000010
    .endif
    dta 'CALL'    , tknCALL,        $02      ; 00000010
    dta 'CHAIN'   , tknCHAIN,       $02      ; 00000010
    dta 'CHR$'    , tknCHRD,        $00      ; 00000000
    dta 'CLEAR'   , tknCLEAR,       $01      ; 00000001
    dta 'CLOSE'   , tknCLOSE,       $03      ; 00000011
    dta 'CLG'     , tknCLG,         $01      ; 00000001
    dta 'CLS'     , tknCLS,         $01      ; 00000001
    dta 'COS'     , tknCOS,         $00      ; 00000000
    dta 'COUNT'   , tknCOUNT,       $01      ; 00000001
    .if version == 3 && minorversion < 10
        dta 'COLOUR', tknCOLOR,     $02      ; 00000010
    .elseif version == 3 && minorversion >= 10
        dta 'COLOR', tknCOLOR,      $02      ; 00000010
    .endif
    dta 'DATA'    , tknDATA,        $20      ; 00100000
    dta 'DEG'     , tknDEG,         $00      ; 00000000
    dta 'DEF'     , tknDEF,         $00      ; 00000000
    dta 'DELETE'  , tknDELETE,      $10      ; 00010000
    dta 'DIV'     , tknDIV,         $00      ; 00000000
    dta 'DIM'     , tknDIM,         $02      ; 00000010
    dta 'DRAW'    , tknDRAW,        $02      ; 00000010
    dta 'ENDPROC' , tknENDPROC,     $01      ; 00000001
    dta 'END'     , tknEND,         $01      ; 00000001
    dta 'ENVELOPE', tknENVELOPE,    $02      ; 00000010
    dta 'ELSE'    , tknELSE,        $14      ; 00010100
    dta 'EVAL'    , tknEVAL,        $00      ; 00000000
    dta 'ERL'     , tknERL,         $01      ; 00000001
    dta 'ERROR'   , tknERROR,       $04      ; 00000100
    dta 'EOF'     , tknEOF,         $01      ; 00000001
    dta 'EOR'     , tknEOR,         $00      ; 00000000
    dta 'ERR'     , tknERR,         $01      ; 00000001
    dta 'EXP'     , tknEXP,         $00      ; 00000000
    dta 'EXT'     , tknEXT,         $01      ; 00000001
    dta 'FOR'     , tknFOR,         $02      ; 00000010
    dta 'FALSE'   , tknFALSE,       $01      ; 00000001
    dta 'FN'      , tknFN,          $08      ; 00001000
    dta 'GOTO'    , tknGOTO,        $12      ; 00010010
    dta 'GET$'    , tknGETD,        $00      ; 00000000
    dta 'GET'     , tknGET,         $00      ; 00000000
    dta 'GOSUB'   , tknGOSUB,       $12      ; 00010010
    dta 'GCOL'    , tknGCOL,        $02      ; 00000010
    dta 'HIMEM'   , tknHIMEM,       $43      ; 01000011
    dta 'INPUT'   , tknINPUT,       $02      ; 00000010
    dta 'IF'      , tknIF,          $02      ; 00000010
    dta 'INKEY$'  , tknINKEYD,      $00      ; 00000000
    dta 'INKEY'   , tknINKEY,       $00      ; 00000000
    dta 'INT'     , tknINT,         $00      ; 00000000
    dta 'INSTR('  , tknINSTR,       $00      ; 00000000
    dta 'LIST'    , tknLIST,        $10      ; 00010000
    dta 'LINE'    , tknLINE,        $00      ; 00000000
    dta 'LOAD'    , tknLOAD,        $02      ; 00000010
    dta 'LOMEM'   , tknLOMEM,       $43      ; 01000011
    dta 'LOCAL'   , tknLOCAL,       $02      ; 00000010
    dta 'LEFT$('  , tknLEFTD,       $00      ; 00000000
    dta 'LEN'     , tknLEN,         $00      ; 00000000
    dta 'LET'     , tknLET,         $04      ; 00000100
    dta 'LOG'     , tknLOG,         $00      ; 00000000
    dta 'LN'      , tknLN,          $00      ; 00000000
    dta 'MID$('   , tknMIDD,        $00      ; 00000000
    dta 'MODE'    , tknMODE,        $02      ; 00000010
    dta 'MOD'     , tknMOD,         $00      ; 00000000
    dta 'MOVE'    , tknMOVE,        $02      ; 00000010
    dta 'NEXT'    , tknNEXT,        $02      ; 00000010
    dta 'NEW'     , tknNEW,         $01      ; 00000001
    dta 'NOT'     , tknNOT,         $00      ; 00000000
    dta 'OLD'     , tknOLD,         $01      ; 00000001
    dta 'ON'      , tknON,          $02      ; 00000010
    dta 'OFF'     , tknOFF,         $00      ; 00000000
    dta 'OR'      , tknOR,          $00      ; 00000000
    dta 'OPENIN'  , tknOPENIN,      $00      ; 00000000
    dta 'OPENOUT' , tknOPENOUT,     $00      ; 00000000
    dta 'OPENUP'  , tknOPENUP,      $00      ; 00000000
    dta 'OSCLI'   , tknOSCLI,       $02      ; 00000010
    dta 'PRINT'   , tknPRINT,       $02      ; 00000010
    dta 'PAGE'    , tknPAGE,        $43      ; 01000011
    dta 'PTR'     , tknPTR,         $43      ; 01000011
    dta 'PI'      , tknPI,          $01      ; 00000001
    dta 'PLOT'    , tknPLOT,        $02      ; 00000010
    dta 'POINT('  , tknPOINT,       $00      ; 00000000
    dta 'PROC'    , tknPROC,        $0A      ; 00001010
    dta 'POS'     , tknPOS,         $01      ; 00000001
    dta 'RETURN'  , tknRETURN,      $01      ; 00000001
    dta 'REPEAT'  , tknREPEAT,      $00      ; 00000000
    dta 'REPORT'  , tknREPORT,      $01      ; 00000001
    dta 'READ'    , tknREAD,        $02      ; 00000010
    dta 'REM'     , tknREM,         $20      ; 00100000
    dta 'RUN'     , tknRUN,         $01      ; 00000001
    dta 'RAD'     , tknRAD,         $00      ; 00000000
    dta 'RESTORE' , tknRESTORE,     $12      ; 00010010
    dta 'RIGHT$(' , tknRIGHTD,      $00      ; 00000000
    dta 'RND'     , tknRND,         $01      ; 00000001
    dta 'RENUMBER', tknRENUMBER,    $10      ; 00010000
    dta 'STEP'    , tknSTEP,        $00      ; 00000000
    dta 'SAVE'    , tknSAVE,        $02      ; 00000010
    dta 'SGN'     , tknSGN,         $00      ; 00000000
    dta 'SIN'     , tknSIN,         $00      ; 00000000
    dta 'SQR'     , tknSQR,         $00      ; 00000000
    dta 'SPC'     , tknSPC,         $00      ; 00000000
    dta 'STR$'    , tknSTRD,        $00      ; 00000000
    dta 'STRING$(', tknSTRINGD,     $00      ; 00000000
    dta 'SOUND'   , tknSOUND,       $02      ; 00000010
    dta 'STOP'    , tknSTOP,        $01      ; 00000001
    dta 'TAN'     , tknTAN,         $00      ; 00000000
    dta 'THEN'    , tknTHEN,        $14      ; 00010100
    dta 'TO'      , tknTO,          $00      ; 00000000
    dta 'TAB('    , tknTAB,         $00      ; 00000000
    dta 'TRACE'   , tknTRACE,       $12      ; 00010010
    dta 'TIME'    , tknTIME,        $43      ; 01000011
    dta 'TRUE'    , tknTRUE,        $01      ; 00000001
    dta 'UNTIL'   , tknUNTIL,       $02      ; 00000010
    dta 'USR'     , tknUSR,         $00      ; 00000000
    dta 'VDU'     , tknVDU,         $02      ; 00000010
    dta 'VAL'     , tknVAL,         $00      ; 00000000
    dta 'VPOS'    , tknVPOS,        $01      ; 00000001
    dta 'WIDTH'   , tknWIDTH,       $02      ; 00000010
    dta 'PAGE'    , tknPAGE2,       $00      ; 00000000
    dta 'PTR'     , tknPTR2,        $00      ; 00000000
    dta 'TIME'    , tknTIME2,       $00      ; 00000000
    dta 'LOMEM'   , tknLOMEM2,      $00      ; 00000000
    dta 'HIMEM'   , tknHIMEM2,      $00      ; 00000000

; ----------------------------------------------------------------------------

; FUNCTION/COMMAND DISPATCH TABLE, MACRO
; ======================================

FIRST_TOKEN = tknOPENIN

func_table .macro operator
    dta :1OPENIN      ; $8E - OPENIN
    dta :1RPTR        ; $8F - PTR
    dta :1RPAGE       ; $90 - PAGE
    dta :1RTIME       ; $91 - TIME
    dta :1RLOMEM      ; $92 - LOMEM
    dta :1RHIMEM      ; $93 - HIMEM
    dta :1ABS         ; $94 - ABS
    dta :1ACS         ; $95 - ACS
    dta :1ADVAL       ; $96 - ADVAL
    dta :1ASC         ; $97 - ASC
    dta :1ASN         ; $98 - ASN
    dta :1ATN         ; $99 - ATN
    dta :1BGET        ; $9A - BGET
    dta :1COS         ; $9B - COS
    dta :1COUNT       ; $9C - COUNT
    dta :1DEG         ; $9D - DEG
    dta :1ERL         ; $9E - ERL
    dta :1ERR         ; $9F - ERR
    dta :1EVAL        ; $A0 - EVAL
    dta :1EXP         ; $A1 - EXP
    dta :1EXT         ; $A2 - EXT
    dta :1FALSE       ; $A3 - FALSE
    dta :1FN          ; $A4 - FN
    dta :1GET         ; $A5 - GET
    dta :1INKEY       ; $A6 - INKEY
    dta :1INSTR       ; $A7 - INSTR(
    dta :1INT         ; $A8 - INT
    dta :1LEN         ; $A9 - LEN
    dta :1LN          ; $AA - LN
    dta :1LOG         ; $AB - LOG
    dta :1NOT         ; $AC - NOT
    dta :1OPENI       ; $AD - OPENUP
    dta :1OPENO       ; $AE - OPENOUT
    dta :1PI          ; $AF - PI
    dta :1POINT       ; $B0 - POINT(
    dta :1POS         ; $B1 - POS
    dta :1RAD         ; $B2 - RAD
    dta :1RND         ; $B3 - RND
    dta :1SGN         ; $B4 - SGN
    dta :1SIN         ; $B5 - SIN
    dta :1SQR         ; $B6 - SQR
    dta :1TAN         ; $B7 - TAN
    dta :1TO          ; $B8 - TO
    dta :1TRUE        ; $B9 - TRUE
    dta :1USR         ; $BA - USR
    dta :1VAL         ; $BB - VAL
    dta :1VPOS        ; $BC - VPOS
    dta :1CHRD        ; $BD - CHR$
    dta :1GETD        ; $BE - GET$
    dta :1INKED       ; $BF - INKEY$
    dta :1LEFTD       ; $C0 - LEFT$(
    dta :1MIDD        ; $C1 - MID$(
    dta :1RIGHTD      ; $C2 - RIGHT$(
    dta :1STRD        ; $C3 - STR$(
    dta :1STRND       ; $C4 - STRING$(
    dta :1EOF         ; $C5 - EOF
    dta :1AUTO        ; $C6 - AUTO
    dta :1DELETE      ; $C7 - DELETE
    dta :1LOAD        ; $C8 - LOAD
    dta :1LIST        ; $C9 - LIST
    dta :1NEW         ; $CA - NEW
    dta :1OLD         ; $CB - OLD
    dta :1RENUM       ; $CC - RENUMBER
    dta :1SAVE        ; $CD - SAVE
    dta :1STDED       ; $CE - unused
    dta :1LPTR        ; $CF - PTR
    dta :1LPAGE       ; $D0 - PAGE
    dta :1LTIME       ; $D1 - TIME
    dta :1LLOMM       ; $D2 - LOMEM
    dta :1LHIMM       ; $D3 - HIMEM
    dta :1BEEP        ; $D4 - SOUND
    dta :1BPUT        ; $D5 - BPUT
    dta :1CALL        ; $D6 - CALL
    dta :1CHAIN       ; $D7 - CHAIN
    dta :1CLEAR       ; $D8 - CLEAR
    dta :1CLOSE       ; $D9 - CLOSE
    dta :1CLG         ; $DA - CLG
    dta :1CLS         ; $DB - CLS
    dta :1DATA        ; $DC - DATA
    dta :1DEF         ; $DD - DEF
    dta :1DIM         ; $DE - DIM
    dta :1DRAW        ; $DF - DRAW
    dta :1END         ; $E0 - END
    dta :1ENDPR       ; $E1 - ENDPROC
    dta :1ENVEL       ; $E2 - ENVELOPE
    dta :1FOR         ; $E3 - FOR
    dta :1GOSUB       ; $E4 - GOSUB
    dta :1GOTO        ; $E5 - GOTO
    dta :1GRAPH       ; $E6 - GCOL
    dta :1IF          ; $E7 - IF
    dta :1INPUT       ; $E8 - INPUT
    dta :1LET         ; $E9 - LET
    dta :1LOCAL       ; $EA - LOCAL
    dta :1MODES       ; $EB - MODE
    dta :1MOVE        ; $EC - MOVE
    dta :1NEXT        ; $ED - NEXT
    dta :1ON          ; $EE - ON
    dta :1VDU         ; $EF - VDU
    dta :1PLOT        ; $F0 - PLOT
    dta :1PRINT       ; $F1 - PRINT
    dta :1PROC        ; $F2 - PROC
    dta :1READ        ; $F3 - READ
    dta :1REM         ; $F4 - REM
    dta :1REPEAT      ; $F5 - REPEAT
    dta :1REPORT      ; $F6 - REPORT
    dta :1RESTORE     ; $F7 - RESTORE
    dta :1RETURN      ; $F8 - RETURN
    dta :1RUN         ; $F9 - RUN
    dta :1STOP        ; $FA - STOP
    dta :1COLOUR      ; $FB - COLOUR
    dta :1TRACE       ; $FC - TRACE
    dta :1UNTIL       ; $FD - UNTIL
    dta :1WIDTH       ; $FE - WIDTH
    dta :1OSCL        ; $FF - OSCLI
.endm

; FUNCTION/COMMAND DISPATCH TABLE, ADDRESS LOW AND HIGH BYTES
; ===========================================================

ADTABL:
    func_table <
ADTABH:
    func_table >

; ----------------------------------------------------------------------------

; ASSEMBLER
; =========

    .macro packmnemL a b c
        dta [[:b<<5] + [:c&0x1f]] & 0xff
    .endm
    .macro packmnemH a b c
        dta [[:a&0x1f]<<2] + [[:b&0x1f]>>3]
    .endm
    .macro mnemonics
        :1 'B' 'R' 'K'
        :1 'C' 'L' 'C'
        :1 'C' 'L' 'D'
        :1 'C' 'L' 'I'
        :1 'C' 'L' 'V'
        :1 'D' 'E' 'X'
        :1 'D' 'E' 'Y'
        :1 'I' 'N' 'X'
        :1 'I' 'N' 'Y'
        :1 'N' 'O' 'P'
        :1 'P' 'H' 'A'
        :1 'P' 'H' 'P'
        :1 'P' 'L' 'A'
        :1 'P' 'L' 'P'
        :1 'R' 'T' 'I'
        :1 'R' 'T' 'S'
        :1 'S' 'E' 'C'
        :1 'S' 'E' 'D'
        :1 'S' 'E' 'I'
        :1 'T' 'A' 'X'
        :1 'T' 'A' 'Y'
        :1 'T' 'S' 'X'
        :1 'T' 'X' 'A'
        :1 'T' 'X' 'S'
        :1 'T' 'Y' 'A'
        :1 'B' 'C' 'C'
        :1 'B' 'C' 'S'
        :1 'B' 'E' 'Q'
        :1 'B' 'M' 'I'
        :1 'B' 'N' 'E'
        :1 'B' 'P' 'L'
        :1 'B' 'V' 'C'
        :1 'B' 'V' 'S'
        :1 'A' 'N' 'D'
        :1 'E' 'O' 'R'
        :1 'O' 'R' 'A'
        :1 'A' 'D' 'C'
        :1 'C' 'M' 'P'
        :1 'L' 'D' 'A'
        :1 'S' 'B' 'C'
        :1 'A' 'S' 'L'
        :1 'L' 'S' 'R'
        :1 'R' 'O' 'L'
        :1 'R' 'O' 'R'
        :1 'D' 'E' 'C'
        :1 'I' 'N' 'C'
        :1 'C' 'P' 'X'
        :1 'C' 'P' 'Y'
        :1 'B' 'I' 'T'
        :1 'J' 'M' 'P'
        :1 'J' 'S' 'R'
        :1 'L' 'D' 'X'
        :1 'L' 'D' 'Y'
        :1 'S' 'T' 'A'
        :1 'S' 'T' 'X'
        :1 'S' 'T' 'Y'
        :1 'O' 'P' 'T'
        :1 'E' 'Q' 'U'
    .endm

; Packed mnemonic table, low/high bytes
; -------------------------------------

MNEML:
    mnemonics packmnemL
MNEMH:
    mnemonics packmnemH

ALLOPS = * -MNEMH

; Opcode base table
; -----------------
STCODE:

    brk:clc:cld:cli:clv:dex:dey:inx
    iny:nop:pha:php:pla:plp:rti:rts
    sec:sed:sei:tax:tay:tsx:txa:txs:tya

IMPLIED = * - STCODE

    dta $90, $B0, $F0, $30    ; BMI, BCC, BCS, BEQ
    dta $D0, $10, $50, $70    ; BNE, BPL, BVC, BVS

BRANCH = * - STCODE

    dta $21, $41, $01, $61    ; AND, EOR, ORA, ADC
    dta $C1, $A1, $E1         ; CMP, LDA, SBC

GROUP1 = * - STCODE

    dta $06, $46, $26, $66    ; ASL, LSR, ROL, ROR

ASLROR = * - STCODE

    dta $C6, $E6              ; DEC, INC

DECINC = * - STCODE

    dta $E0, $C0              ; CPX, CPY

CPXCPY = * - STCODE

    dta $20, $4C, $20         ; BIT, JMP, JSR

JSRJMP = * - STCODE

    dta $A2, $A0              ; LDX, LDY

COPSTA = * - STCODE

    dta $81, $86, $84         ; STA, STX, STY

PSEUDO = * - STCODE

; ----------------------------------------------------------------------------

; Assembler
; =========

STOPASM:              ; Exit Assembler
    lda #$FF          ; Set OPT to 'BASIC'
    sta zpBYTESM
    jmp STMT          ; return to execution loop

ASS:
    lda #$03
    sta zpBYTESM      ; Set OPT 3, default on entry to '['

CASM:
    jsr SPACES        ; Skip spaces, and get next character
    cmp #']'
    beq STOPASM       ; ']' - exit assembler

    jsr CLYADP        ; add Y to zpLINE, set Y and zpCURSOR to 1

    dec zpCURSOR
    jsr MNEENT        ; assemble a single instruction

    dec zpCURSOR      ; point to last character that could not assemble
    lda zpBYTESM      ; top bit determines whether to list or not
    lsr
    bcc NOLIST

    ; generate listing

    lda zpTALLY
    adc #$04          ; carry is set, add 5 for reasonable output
    sta zpWORK+8      ; save as indentation
    lda zpWORK+1      ; P% as it was before assembling instruction
    jsr HEXOUT        ; print MSB

    lda zpWORK
    jsr HEXSP         ; print LSB and space

    ldx #-4           ; counter, max. 3 bytes per line, X=0 after 4th increment
    ldy zpWORK+2
    bpl WRTLOP

    ldy zpCLEN
WRTLOP:
    sty zpWORK+1
    beq RMOVE         ; hex of instructions done

    ldy #$00
WRTLPY:
    inx
    bne WRTLPA        ; fourth increment, print newline first

    jsr NLINE         ; Print newline

    ldx zpWORK+8      ; retrieve indentation

    .if version < 3
WRTLPZ:
        jsr LISTPT     ; Print a space
        dex
        bne WRTLPZ     ; Loop to print spaces
    .elseif version >= 3
        jsr LISTPL     ; Print multiple spaces
    .endif

    ldx #-3    ; one more than -4 because first loop through inx is not done

WRTLPA:
    lda (zpWORK+3),Y
    jsr HEXSP

    iny
    dec zpWORK+1
    bne WRTLPY

RMOVE:
    .if version < 3
        inx
        bpl RMOVEB
        jsr LISTPT          ; print a space
        jsr CHOUT
        jsr CHOUT
        jmp RMOVE
RMOVEB:
        ldy #0              ; used to index into source text just assembled
    .elseif version >= 3
        txa
        tay
RMOVEL:
        iny
        beq LLLLL5
        ldx #3
        jsr LISTPL
        beq RMOVEL

LLLLL5:
        ldx #$0A
        lda (zpLINE),Y
        cmp #'.'
        bne NOLABL

LTLABL:
        jsr TOKOUT     ; Print char or token
        dex
        bne MALABL
        ldx #1

MALABL:
        iny             ; Y was 0
        lda (zpLINE),Y
        cpy zp4F
        bne LTLABL

NOLABL:
        jsr LISTPL
        dey

LABLSP:
        iny
        cmp (zpLINE),Y
        beq LABLSP
    .endif

LLLL4:
    lda (zpLINE),Y
    cmp #':'           ; possible end of statement?
    beq NOCODA

    cmp #$0D
    beq NOCOD

LLLLLL6:
    jsr TOKOUT         ; Print character or token
    iny
    bne LLLL4          ; branch always, as line length of 256+ is impossible

NOCODA:
    cpy zpCURSOR
    bcc LLLLLL6        ; not end of statement yet, continue processing

NOCOD:
    jsr NLINE         ; Print newline

NOLIST:
    ldy zpCURSOR
    dey

NOLA:
    iny
    lda (zpLINE),Y
    cmp #':'
    beq NOLB

    cmp #$0D           ; CR, EOL
    bne NOLA

NOLB:
    jsr DONE_WITH_Y    ; check statement has ended, add Y to zpLINE, Y=1

    dey
    lda (zpLINE),Y      ; get previous character
    cmp #':'            ; check if statement ended with :
    beq CASMJ           ; assemble next instruction

    lda zpLINE+1        ; check for immediate mode if LINE points to BUFFER
    cmp #>BUFFER
    bne INTXT           ; jump if not immediate mode

    jmp CLRSTK          ; done

INTXT:
    jsr LINO

CASMJ:
    jmp CASM            ; assemble next instruction

; ----------------------------------------------------------------------------

SETL:                 ; set label
    jsr CRAELV        ; get variable name, check if it exists, or create
    beq ASSDED        ; badly formed
    bcs ASSDED        ; or string

    jsr PHACC         ; push to AE stack
    jsr GETPC         ; put P% in IACC

    sta zpTYPE        ; A contains $40, type is integer

    jsr STORE         ; assign value to variable
    jsr ASCUR         ; update cursor offset

    .if version >= 3
        sty zpNEWVAR
    .endif

; Assemble a single instruction

MNEENT:
    ldx #$03          ; mnemonics are three characters
    jsr SPACES        ; skip spaces, and get next character

    ldy #$00         ; number of bytes
    sty zpWORK+6     ; make sure temp location is zero for packing mnemonic

    cmp #':'
    beq MMMM         ; End of statement

    cmp #$0D
    beq MMMM         ; End of line

    cmp #'\'
    beq MMMM         ; Comment

    cmp #'.'
    beq SETL         ; Label

    dec zpCURSOR

RDLUP:
    ldy zpCURSOR      ; current character position
    inc zpCURSOR      ; increment index
    lda (zpLINE),Y
    bmi RDSLPT        ; Token, check for tokenised AND, EOR, OR
                      ; (they are tokenised because they match BASIC keywords)

    cmp #' '
    beq RDOUT         ; premature space, step past

    ; pack mnemonic

    ldy #$05          ; 5 bottom bits, fun fact: you can use lower case, too
                      ;                          or even $ for D etc...
    asl               ; skip top 3 bits
    asl
    asl

INLUP:
    asl
    rol zpWORK+6      ; was cleared before, so there are not random top bits
    rol zpWORK+7
    dey
    bne INLUP         ; shift 5 times

    dex
    bne RDLUP         ; Loop to fetch three characters

; The current opcode has now been compressed into two bytes
; ---------------------------------------------------------

RDOUT:
    ldx #ALLOPS       ; Point to end of opcode lookup table
    lda zpWORK+6      ; Get low byte of compacted mnemonic

SRCHM:
    cmp MNEML-1,X
    bne NOTGOT        ; Low half doesn't match

    ldy MNEMH-1,X     ; Check high half
    cpy zpWORK+7
    beq RDOPGT        ; Mnemonic matches

NOTGOT:
    dex
    bne SRCHM         ; Loop through opcode lookup table

ASSDED:
    jmp STDED         ; Mnemonic not matched, Mistake

; The check-for-tokens routine called earlier

RDSLPT:
    ldx #BRANCH+1     ; opcode number for 'AND'
    cmp #tknAND
    beq RDOPGT        ; Tokenised 'AND'

    inx               ; opcode number for 'EOR'
    cmp #tknEOR
    beq RDOPGT        ; Tokenised 'EOR'

    inx               ; opcode number for 'ORA'
    cmp #tknOR
    bne ASSDED        ; Not tokenised 'OR'

    inc zpCURSOR
    iny
    lda (zpLINE),Y    ; Get next character
    cmp #'A'
    bne ASSDED        ; Ensure 'OR' followed by 'A'

; Continue here when mnemonic is found in table

RDOPGT:
    lda STCODE-1,X
    sta zpOPCODE      ; Get base opcode
    ldy #$01          ; code length is 1
    cpx #IMPLIED+1    ; compare with implied group border
    bcs NGPONE        ; Opcode in next group(s)

; Implied instruction group

MMMM:
    lda PC
    sta zpWORK        ; Get P% low byte

    sty zpWORK+2
    ldx zpBYTESM
    cpx #$04          ; Offset assembly (opt>3), set flags for next bcc

    ldx PC+1
    stx zpWORK+1      ; Get P% high byte
    bcc MMMMLR        ; No offset assembly

    lda VARL_O        ; Get O%, origin
    ldx VARL_O+1

MMMMLR:
    sta zpWORK+3
    stx zpWORK+4      ; Store destination pointer
    tya               ; code length to A
    beq MMMMRT        ; exit, no more bytes
    bpl MMMMLP        ; it's an opcode

    ldy zpCLEN        ; it's a string, length of string in STRACC buffer
    beq MMMMRT        ; exit if string has zero length

MMMMLP:
    dey
    lda zpOPCODE,Y    ; Get opcode byte   (lda abs,y (!))
    bit zpWORK+2
    bpl MMMMCL        ; Opcode - jump to store it

    lda STRACC,Y      ; Get EQUS byte

MMMMCL:
    sta (zpWORK+3),Y  ; Store byte
    inc PC            ; Increment P%
    bne MMMMLQ

    inc PC+1

MMMMLQ:
    bcc MMMMPP

    inc VARL_O        ; increment O%
    bne MMMMPP

    inc VARL_O+1

MMMMPP:
    tya
    bne MMMMLP        ; continue until counter reaches zero

MMMMRT:
    rts

; Branch instruction group

NGPONE:
    cpx #BRANCH+1      ; index for 'AND' opcode
    bcs NGPTWO         ; opcode in next group(s)

    ; it's a branch

    jsr ASEXPR         ; evaluate integer expression to IACC

    ; calculate branching distance

    clc
    lda zpIACC
    sbc PC
    tay
    lda zpIACC+1
    sbc PC+1
    cpy #$01
    dey
    sbc #$00
    beq FWD         ; branch forwards, MSB is zero, probably not out of range

    cmp #$FF        ; MSB of branching distance is $ff, so negative
    beq BACKWARDS   ; branch backwards

BOR:
    lda zpBYTESM            ; check OPT
    .if version < 3
        lsr                 ; bug: only works if O% is not used
    .elseif version >= 3
        and #$02            ; bug fixed
    .endif
    beq BRSTOR              ; error not trapped

    brk
    .if foldup == 1
        dta 1, 'OUT OF RANGE'
    .else
        dta 1, 'Out of range'
    .endif
    brk

BRSTOR:
    tay             ; Y=A=0

BRSTO:
    sty zpIACC

BRST:
    ldy #$02        ; set code length to 2 bytes
    jmp MMMM        ; join the original code

BACKWARDS:
    tya             ; set flags by copying displacement to A
    bmi BRSTO       ; use displacement, not out of range
    bpl BOR         ; positive for backwards is out of range

FWD:
    tya             ; set flags
    bpl BRSTO       ; positive is good for forward branch
    bmi BOR         ; negative is out of range

; Group 1 instruction group

NGPTWO:
    cpx #GROUP1+1   ; compare with group1 border
    bcs NGPTHR

    jsr SPACES      ; Skip spaces, and get next character

    cmp #'#'
    bne NOTHSH      ; not immediate, skip to next addressing mode

    jsr PLUS8       ; add 8 to the base opcode to correct for immediate

IMMED:
    jsr ASEXPR      ; evaluate integer expression

INDINX:
    lda zpIACC+1    ; check that MSB is zero, i.e. we got an 8-bit value
    beq BRST        ; ok, jump to set code length to 2, and store

    ; immediate argument > 255

BYTE:
    brk
    dta $02
    .if foldup == 1
        dta 'BYTE'
    .else
        dta 'Byte'
    .endif
    brk

; ----------------------------------------------------------------------------

; Parse (zp),Y addressing mode
; ----------------------------
NGPTHR:
    cpx #COPSTA+1      ; compare with next instruction group border
    bne NOPSTA

    jsr SPACES         ; Skip spaces, and get next character

NOTHSH:
    cmp #'('
    bne NOTIND         ; not indirect

    jsr ASEXPR         ; evaluate integer expression

    jsr SPACES         ; Skip spaces, and get next character
    cmp #')'
    bne ININX          ; branch if possible (zp,x)

    jsr SPACES         ; Skip spaces, and get next character
    cmp #','
    bne BADIND         ; No comma, (zp),y error

    jsr PLUS10         ; add $10 to base opcode

    jsr SPACES         ; Skip spaces, get next character

    cmp #'Y'
    bne BADIND         ; (zp),Y missing Y, jump to Index error
    beq INDINX         ; jump to check operand is a byte, and store/error

; Parse (zp,X) addressing mode
; ----------------------------
ININX:
    cmp #','
    bne BADIND         ; No comma, jump to Index error

    jsr SPACES         ; Skip spaces, and get next character
    cmp #'X'
    bne BADIND         ; zp,X missing X, jump to Index error

    jsr SPACES         ; Skip spaces, and get next character
    cmp #')'
    beq INDINX         ; zp,X) jump to check operand is abyte, and store/error

    ; the error message

BADIND:
    brk
    dta $03
    .if foldup == 1
        dta 'INDEX'
    .else
        dta 'Index'
    .endif
    brk

    ; check abs,x and abs,y

NOTIND:
    dec zpCURSOR       ; one position back
    jsr ASEXPR         ; evaluate integer expression
    jsr SPACES         ; Skip spaces, and get next character

    cmp #','
    bne OPTIM          ; No comma - jump to process as abs,X

    jsr PLUS10         ; add $10 (16) to opcode
    jsr SPACES         ; Skip spaces, and get next character
    cmp #'X'
    beq OPTIM          ; abs,X - jump to process

    cmp #'Y'
    bne BADIND         ; Not abs,Y - jump to Index error

UNOPT:
    jsr PLUS8          ; add 8 to the opcode
    jmp JSRB           ; go on to indicate instruction length of 3 and continue

    ; abs and abs,X get here

OPTIM:
    jsr PLUS4          ; add 4 to the opcode

OPTIMA:
    lda zpIACC+1
    bne UNOPT          ; check if MSB !=0 (16-bit operand)
    jmp BRST           ; jump to instruction length 2, and store

NOPSTA:
    cpx #DECINC+1      ; check for DECINC group
    bcs NGPFR

    cpx #ASLROR+1      ; check for ASLROR group
    bcs NOTACC         ; skip check for 'A' if INC or DEC

    jsr SPACES         ; Skip spaces, and get next character
    cmp #'A'
    beq ACCUMS         ; brnach if ins A, e.g. ASL A

    dec zpCURSOR       ; one character back

NOTACC:
    jsr ASEXPR         ; evaluate integer expression
    jsr SPACES         ; Skip spaces, and get next character
    cmp #','
    bne OPTIMA         ; No comma, jump to ...

    jsr PLUS10         ; add $10 to opcode
    jsr SPACES         ; Skip spaces, and get next character
    cmp #'X'
    beq OPTIMA         ; handle as address,X

    jmp BADIND         ; Otherwise, jump to Index error

    ; ins A, e.g. ROR A

ACCUMS:
    jsr PLUS4          ; add $04 to opcode

    ldy #$01           ; set instruction length to 1
    bne JSRC           ; and exit

NGPFR:
    cpx #JSRJMP-1      ; check JSRJMP group
    bcs NGPFV

    cpx #CPXCPY+1      ; compare next group border
    beq BIT_           ; BIT does not support immediate addressing mode

    jsr SPACES         ; Skip spaces, and get next character
    cmp #'#'
    bne NHASH          ; Not #, jump to handle address

    jmp IMMED          ; handle immediate mode

NHASH:
    dec zpCURSOR       ; one character back

BIT_:
    jsr ASEXPR         ; evaluate integer expression
    jmp OPTIM          ; handle as abs before

NGPFV:
    cpx #JSRJMP      ; next instruction group
    beq JSR_         ; it's JSR

    bcs NGPSX        ; it's not JMP

    ; it's JMP

    jsr SPACES       ; Skip spaces, and get next character

    cmp #'('
    beq JSRA         ; Jump with (... addressing mode

    dec zpCURSOR

JSR_:
    jsr ASEXPR       ; evaluate integer expression

JSRB:
    ldy #$03         ; set instruction length to 3

JSRC:
    jmp MMMM         ; continue with store

    ; jmp (abs)

JSRA:
    jsr PLUS10
    jsr PLUS10       ; opcode plus $20

    jsr ASEXPR       ; evaluate integer expression
    jsr SPACES       ; Skip spaces, and get next character

    cmp #')'
    beq JSRB         ; jump to instruction length is 3, and continue

    jmp BADIND       ; No ) - jump to Index error

NGPSX:
    cpx #PSEUDO+1    ; check next instruction group
    bcs OPTION

    lda zpWORK+6     ; still contains encoded mnemonic
    eor #$01         ; invert bit 0
    and #$1F         ; ignore all but the bottom 5 bits
    pha              ; save last "letter" for later, source or dest. register

    cpx #PSEUDO-1
    bcs STXY         ; if STX/STY, jump, as they don't allow immediate addr.

    jsr SPACES       ; Skip spaces, and get next character
    cmp #'#'
    bne LDXY         ; jump if not immediate

    pla              ; we don't need it, but we need to fix the stack
    jmp IMMED

LDXY:
    dec zpCURSOR     ; one step back
    jsr ASEXPR       ; evaluate integer expression
    pla              ; destination register back from stack
    sta zpWORK       ; and save

    jsr SPACES       ; Skip spaces, and get next character

    cmp #','
    beq LDIND        ; comma means indexed

    jmp OPTIM        ; jump back to zp or abs code

LDIND:
    jsr SPACES       ; Skip spaces, and get next character
    and #$1F         ; ignore top 3 bits
    cmp zpWORK
    bne LDINDB       ; index error

    jsr PLUS10       ; add $10
    jmp OPTIM        ; same code as before for

LDINDB:
    jmp BADIND       ; Jump to Index error

STXY:
    jsr ASEXPR       ; evaluate integer expression following STX or STY

    pla              ; source register back from stack
    sta zpWORK       ; save

    jsr SPACES       ; Skip spaces, and get next character
    cmp #','
    bne GOOP         ; not indexed

    jsr SPACES       ; Skip spaces, and get next character

    and #$1F         ; bottom 5 bits
    cmp zpWORK
    bne LDINDB       ; index error

    jsr PLUS10       ; add $10 to opcode

    lda zpIACC+1
    beq GOOP         ; High byte=0, continue

    jmp BYTE         ; value>255, jump to Byte error

GOOP:
    jmp OPTIMA       ; 8- or 16-bit operand, and continue

OPTION:
    bne EQUBWS

    ; this is OPT

    jsr ASEXPR       ; evaluate integer expression

    lda zpIACC
    sta zpBYTESM     ; set OPT

    ldy #$00         ; instruction length is 0
    jmp MMMM         ; continue

; ----------------------------------------------------------------------------

; Evaluate integer expression

ASEXPR:
    jsr AEEXPR       ; evaluate expression
    jsr INTEGB       ; ensure it is integer

ASCUR:
    ldy zpAECUR
    sty zpCURSOR     ; move cursor offset
    rts

; ----------------------------------------------------------------------------

; Add constants to zpOPCODE

PLUS10:             ; + $10 (+16)
    jsr PLUS8
PLUS8:
    jsr PLUS4
PLUS4:
    lda zpOPCODE
    clc
    adc #$04
    sta zpOPCODE
    rts

; ----------------------------------------------------------------------------

EQUBWS:
    ldx #$01          ; Prepare for one byte
    ldy zpCURSOR
    inc zpCURSOR      ; Increment index
    lda (zpLINE),Y    ; Get next character
    cmp #'B'
    beq EQUB

    inx               ; Prepare for two bytes
    cmp #'W'
    beq EQUB

    ldx #$04          ; Prepare for four bytes
    cmp #'D'
    beq EQUB

    cmp #'S'
    beq EQUS
    jmp STDED         ; Syntax error

; enter with X= 1 (bytes), 2 (words), or 4 (dwords),

EQUB:
    txa
    pha                ; save number of bytes per expression on stack

    jsr ASEXPR         ; evaluate the expression

    ldx #zpOPCODE      ; copy IACC to OPCODE and next three bytes
    jsr ACCTOM         ; basically shift one byte up (see vars.s)

    pla
    tay                ; set code length

EQUSX:
    jmp MMMM           ; and continue

EQUSE:
    jmp LETM           ; type mismatch error

EQUS:
    lda zpBYTESM
    pha                ; save current OPT value

    jsr AEEXPR         ; evaluate expression

    bne EQUSE          ; error if not a string

    pla
    sta zpBYTESM       ; restore OPT

    jsr ASCUR          ; move CURSOR to AECUR

    ldy #$FF           ; signal string with code length set to $ff
    bne EQUSX          ; exit

; ----------------------------------------------------------------------------

; Replace of string by a single byte token.
; zpWORK points to the string of characters that are replaced by this token.
; On etry, A contains the token, and Y the string length
; The whole string can be longer than Y, and is terminated by CR ($0d)

INTOK:
    pha                 ; save token
    clc
    tya                 ; number of bytes to remove in A
    adc zpWORK          ; add to pointer
    sta zpWORK+2        ; save in another pointer
    ldy #$00            ; handle MSB, set Y=0 in the process
    tya
    adc zpWORK+1
    sta zpWORK+3
    pla                 ; retrieve token
    sta (zpWORK),Y      ; store

INTOKA:
    iny
    lda (zpWORK+2),Y    ; copy rest of string directly after token
    sta (zpWORK),Y
    cmp #$0D
    bne INTOKA          ; copy until EOL/CR

    ; carry is always set

    rts

; ----------------------------------------------------------------------------

; Convert ASCII number to 16-bit binary and insert as tknCONST
; Y is set to the length of the number. This routine is used to encode
; line numbers. On entry, A contains the first digit, and Y is 0.

CONSTQ:
    and #$0F            ; bottom nibble (top was $3x)
    sta zpWORK+6        ; save as LSB of result
    sty zpWORK+7        ; MSB is 0

CONSTR:
    iny
    lda (zpWORK),Y
    .if version < 3
        cmp #'9'+1
        bcs CONSTX          ; not a digit
        cmp #'0'
    .elseif version >= 3
        jsr NUMBCP
    .endif
    bcc CONSTX              ; not a digit

    and #$0F                ; convert to binary digit again (0-9)
    pha                     ; save

    ; multiply 16-bit int by 10
    ;
    ldx zpWORK+7            ; remember original MSB in X
    lda zpWORK+6
    asl                     ; multiply by 2
    rol zpWORK+7
    bmi CONSTY              ; result >= 32768, error

    asl                     ; multiply by 2 (total is *4)
    rol zpWORK+7
    bmi CONSTY              ; result >= 32768, error

    adc zpWORK+6            ; LSB*4 was kept in A, add original
    sta zpWORK+6
    txa                     ; retrieve remembered original MSB
    adc zpWORK+7            ; add new MSB, we now have 4*n+n = 5n

    asl zpWORK+6            ; multiple by 2
    rol
    bmi CONSTY              ; result >= 32768, error
    bcs CONSTY              ; result >= 32768, error

    sta zpWORK+7            ; MSB was in A, save, we now have (4*n+n)*2 = 10n

    pla                     ; finally add our new digit
    adc zpWORK+6
    sta zpWORK+6            ; store
    bcc CONSTR              ; next digit

    inc zpWORK+7            ; adjust MSB
    bpl CONSTR              ; next digit

    pha  ; dummy push because we fallthrough to error condition with pla

CONSTY:
    pla                     ; drop top of stack
    ldy #$00                ; length 0
    sec                     ; C=1 means error/overflow
    rts

; Insert line number into buffer
; zpWORK+0/1 points to ASCII string
; binary representation is in zpWORK+6/7

CONSTX:
    dey                 ; make Y reflect length of line number (string)
    lda #tknCONST
    jsr INTOK           ; insert token, return with carry set(!)
                        ; Y points to CR ($0d)

    lda zpWORK          ; reserve space for binary number
    adc #$02            ; add 3 (!) and save as source pointer
    sta zpWORK+2
    lda zpWORK+1
    adc #$00
    sta zpWORK+3

CONSTL:
    lda (zpWORK),Y      ; copy backwards from $0d location to zero
    sta (zpWORK+2),Y    ; hence creating a gap for the binary number
    dey
    bne CONSTL

    ldy #$03            ; start at 3rd position

; Encode line number constant, see SPGETN for decoder and format
; number is in zpWORK+6/7

CONSTI:                 ; insert constant
    lda zpWORK+7
    ora #$40            ; always 1 bit at bit6
    sta (zpWORK),Y
    dey                 ; 2nd position
    lda zpWORK+6
    and #~$C0
    ora #$40            ; always 1 bit at bit6
    sta (zpWORK),Y
    dey                 ; 1st position
    lda zpWORK+6
    and #$C0
    sta zpWORK+6
    lda zpWORK+7
    and #$C0
    lsr
    lsr
    ora zpWORK+6
    lsr
    lsr
    eor #$54            ; inverse of original bit6 and bit14, and always 1 bit
    sta (zpWORK),Y

    jsr NEXTCH          ; add 3 to the pointer to point to the last byte
    jsr NEXTCH          ; of the integer constant
    jsr NEXTCH

    ldy #$00            ; offset zero

WORDCN:
    clc                 ; success and return
    rts

; Check alphabet and _

WORDCQ:
    cmp #'z'+1
    bcs WORDCN      ; fail > 'z'
    cmp #'_'
    bcs WORDCY      ; succeed >= '_' (0x60 pound sign, ASCII backtick allowed)
    cmp #'Z'+1
    bcs WORDCN      ; fail > 'Z'
    cmp #'A'
    bcs WORDCY      ; succeed >= 'A'

; Check numeric

NUMBCP:
    cmp #'9'+1
    bcs WORDCN          ; fail > '9'
    cmp #'0'
WORDCY:                 ; succeed
    rts

; Check dot

NUMBCQ:
    cmp #'.'
    bne NUMBCP
    rts

; Get character and increment WORK pointer

GETWRK:
    lda (zpWORK),Y

; Only increment pointer

NEXTCH:
    inc zpWORK
    bne RELRTS
    inc zpWORK+1

RELRTS:
    rts

; Increment first, then get character

GETWK2:
    jsr NEXTCH
    lda (zpWORK),Y
    rts

; ----------------------------------------------------------------------------

; Tokenise line at (zpWORK)
; =========================

; Set default values to zero

MATCH:
    ldy #$00
    sty zpWORK+4        ; flags if at the start of a statement or not
                        ; zero, start of statement, otherwise, not

MATCEV:
    sty zpWORK+5        ; flag if numbers are line numbers
                        ; zero, don't tokenise number, otherwise, do

; Usual entry point with flags in zpWORK+4/5 already set

MATCHA:
    lda (zpWORK),Y     ; Get current character
    cmp #$0D
    beq RELRTS         ; Exit with <cr>

    cmp #' '
    bne BMATCH         ; jump if not space

MATCHB:
    jsr NEXTCH         ; skip character, increment WORK pointer
    bne MATCHA         ; and loop

BMATCH:
    cmp #'&'
    bne CMATCH         ; Jump if not '&'

MATCHC:
    jsr GETWK2         ; increment WORK pointer, and get next character
    jsr NUMBCP         ; check if it's a digit
    bcs MATCHC         ; Jump if numeric character

    cmp #'A'
    bcc MATCHA         ; Loop back if <'A'

    cmp #'F'+1
    bcc MATCHC         ; Step to next if 'A'..'F'
    bcs MATCHA         ; Loop back for next character

CMATCH:
    cmp #'"'
    bne DMATCH         ; skip if not '"'

MATCHD:
    jsr GETWK2         ; Increment WORK pointer and get next character
    cmp #'"'
    beq MATCHB         ; Not quote, jump to process next character

    cmp #$0D
    bne MATCHD         ; continue until EOL/CR

    rts

DMATCH:
    cmp #':'
    bne MATCHE         ; skip if not a colon

    sty zpWORK+4       ; mode := left of statement
    sty zpWORK+5       ; constant := tokenise numbers
    beq MATCHB

MATCHE:
    cmp #','
    beq MATCHB          ; return to start if ','

    cmp #'*'
    bne FMATCH          ; skip if not '*'

    lda zpWORK+4
    bne YMATCH          ; test '*' and mode=left

    rts                 ; abort, rest is OSCLI command

FMATCH:
    cmp #'.'
    beq MATCHZ          ; skip if '.'

    jsr NUMBCP          ; check if number
    bcc GMATCH          ; if not, skip

    ldx zpWORK+5        ; tokenize constant?
    beq MATCHZ          ; zero, don't tokenize

    jsr CONSTQ          ; tokenize constant
    bcc MATCHF          ; return to start of tokenisation was successful

MATCHZ:
    lda (zpWORK),Y      ; get current character
    jsr NUMBCQ          ; check for number or period
    bcc MATCHY          ; end found

    jsr NEXTCH          ; increment pointer
    jmp MATCHZ          ; continue scanning to the end of the number

MATCHY:
    ldx #$FF            ; set both flags to $ff
    stx zpWORK+4
    sty zpWORK+5
    jmp MATCHA          ; and jump to start

MATCHW:
    jsr WORDCQ          ; check alphanum
    bcc YMATCH          ; jump if not

MATCHV:
    ldy #$00

MATCHG:
    lda (zpWORK),Y      ; get current character
    jsr WORDCQ          ; check alphanum
    bcc MATCHY          ; jump if not

    jsr NEXTCH
    jmp MATCHG

GMATCH:                ; lookup word (optimised for none present words)
    cmp #'A'
    bcs HMATCH         ; Jump if letter

YMATCH:
    ldx #$FF            ; set both flags
    stx zpWORK+4
    sty zpWORK+5

MATCHF:
    jmp MATCHB          ; loop

    ; lookup keywords, table is in alphabetical order

HMATCH:
    cmp #'X'
    bcs MATCHW         ; Jump if >='X', nothing starts with X,Y,Z

    ldx #<TOKENS
    stx zpWORK+2       ; Point to token table
    ldx #>TOKENS
    stx zpWORK+3

IMATCH:
    cmp (zpWORK+2),Y   ; Special check on first character
    bcc MATCHG         ; lower than 1st character, jump back, variable name
    bne JMATCH         ; no match, skip to next table entry

KMATCH:
    iny
    lda (zpWORK+2),Y
    bmi LMATCH         ; compating with token, end of keyword reached

    cmp (zpWORK),Y
    beq KMATCH          ; match, continue matching

    lda (zpWORK),Y
    cmp #'.'
    beq ABBREV          ; matched '.', deal with abbreviation

    ; move to next table entry

JMATCH:
    iny
    lda (zpWORK+2),Y
    bpl JMATCH          ; keep skipping bytes, until byte >= 0x80, token

    cmp #tknWIDTH       ; last token in list
    bne MMATCH          ; not end of list yet, continue
    bcs MATCHV          ; end of list, skip rest of variable name

ABBREV:
    iny

ABBREA:
    lda (zpWORK+2),Y    ; next character from ROM
    bmi LMATCH          ; jump if token

    inc zpWORK+2        ; add 1 to pointer
    bne ABBREA
    inc zpWORK+3
    bne ABBREA

MMATCH:
    sec                 ; add Y+1 to pointer
    iny
    tya
    adc zpWORK+2
    sta zpWORK+2
    bcc NMATCH

    inc zpWORK+3

NMATCH:                 ; points to next keyword in table
    ldy #$00
    lda (zpWORK),Y
    jmp IMATCH          ; try again

LMATCH:
    tax                 ; token held in x for now
    iny
    lda (zpWORK+2),Y    ; get token flags
    sta zpWORK+6        ; store for later
    dey
    lsr
    bcc OMATCH          ; skip if bit 0 of flags is 0

    lda (zpWORK),Y      ; check last character
    jsr WORDCQ          ; alphanum
    bcs MATCHV          ; stop tokenising

OMATCH:
    txa                 ; token back to A
    bit zpWORK+6
    bvc WMATCH          ; skip if bit 6 of flags is set

    ldx zpWORK+4        ; mode = left of statement?
    bne WMATCH          ; skip if not start of statement

    .if split == 0
        clc             ; Superflous as all paths to here have CLC
    .endif

    adc #tknPTR2-tknPTR ; add $40 to the token

WMATCH:
    dey
    jsr INTOK           ; insert token

    ldy #$00            ; future new flags
    ldx #$FF

    lda zpWORK+6
    lsr
    lsr
    bcc QMATCH          ; skip to QMATCH if bit 1 of token flags not set

    stx zpWORK+4        ; mode=right, not start of statement
    sty zpWORK+5        ; constant=false, do not tokenise numbers

QMATCH:
    lsr
    bcc RMATCH          ; skip if bit 2 of token flags is not set

    sty zpWORK+4        ; mode=left, start of statement
    sty zpWORK+5        ; constant=false, do not tokenise

RMATCH:
    lsr
    bcc TMATCH          ; skip following section if bit 3 of flags is not set

    pha                 ; save shifted flags
    iny                 ; make Y=1

SMATCH:
    lda (zpWORK),Y      ; get character
    jsr WORDCQ          ; check alphanum
    bcc XMATCH          ; stop if not

    jsr NEXTCH
    jmp SMATCH          ; loop

XMATCH:
    dey
    pla                 ; restore tokenise flags

TMATCH:
    lsr
    bcc UMATCH          ; skip if bit 4 of tokenise flags is set

    stx zpWORK+5        ; constant=true, tokenise line numbers

UMATCH:
    lsr
    bcs AESPAR          ; return immediately if bit of tokenise flags is set
    jmp MATCHB          ; otherwise, go back to the start

; ----------------------------------------------------------------------------

; Skip Spaces, get next character from AELINE at AECUR
; ----------------------------------------------------
AESPAC:
    ldy zpAECUR        ; Get offset
    inc zpAECUR        ; increment it
    lda (zpAELINE),Y   ; Get current character
    cmp #' '
    beq AESPAC         ; Loop until not space
AESPAR:
    rts

; ----------------------------------------------------------------------------

; Skip spaces, get next character from the LINE at CURSOR
; -------------------------------------------------------
SPACES:
    ldy zpCURSOR
    inc zpCURSOR
    lda (zpLINE),Y
    cmp #' '
    beq SPACES

    ; return with zpCURSOR position in Y

COMRTS:
    rts

; ----------------------------------------------------------------------------

; Check for comma at AELINE

    .if version < 3
COMERR:
        brk
        dta 5
        .if foldup == 1
            dta 'MISSING ,'
        .else
            dta 'Missing ,'
        .endif
        brk
    .endif

COMEAT:
    jsr AESPAC              ; get character
    cmp #','
    .if version < 3
        bne COMERR          ; not equal, error
        rts
    .elseif version >= 3
        beq COMRTS          ; equal, ok

COMERR:
        brk
        dta 5
        .if foldup == 1
            dta 'MISSING ,'
        .else
            dta 'Missing ,'
        .endif
        brk
    .endif

; ----------------------------------------------------------------------------

; OLD - Attempt to restore program
; ================================
; OLD command does ?&3001=0, finds new end of text and frees

OLD:
    jsr DONE          ; Check end of statement
    lda zpTXTP
    sta zpWORK+1      ; Point zpWORK to PAGE
    lda #$00
    sta zpWORK
    sta (zpWORK),Y    ; Remove end marker
    jsr ENDER         ; Check program and set TOP
    bne FSASET        ; Jump to clear heap and go to immediate mode

; ----------------------------------------------------------------------------

; END - Return to immediate mode
; ==============================
; END statement finds end of text and stops

END:
    jsr DONE          ; Check end of statement
    jsr ENDER         ; Check program and set TOP
    bne CLRSTK        ; Jump to immediate mode, keeping variables, etc

; ----------------------------------------------------------------------------

; STOP - Abort program with an error
; ==================================
; STOP statement prints that it has stopped

STOP:
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

; ----------------------------------------------------------------------------

    .if title != 0
NEWTITLE:
        lda #$0D        ; EOL/CR
        ldy zpTXTP
        sty zpTOP+1     ; TOP hi=PAGE hi
        ldy #0
        sty zpTOP
        sty zpTRFLAG    ; TOP=PAGE, TRACE OFF
        sta (zpTOP),Y   ; ?(PAGE+0)=<cr>
        lda $ff#
        iny
        sta (zpTOP),Y     ; ?(PAGE+1)=&FF
        iny
        sty zpTOP
        rts             ; TOP=PAGE+2
    .endif

; ----------------------------------------------------------------------------

; NEW - Clear program, enter immediate mode
; =========================================
; NEW comand clears text and frees

; Cold start

NEW:
    jsr DONE          ; Check end of statement
    .if title != 0
        jsr NEWTITLE  ; NEW program
    .endif

; Start up with NEW program
; -------------------------
FORMAT:
    .if title == 0
        lda #$0D       ; EOL/CR
        ldy zpTXTP
        sty zpTOP+1    ; TOP hi=PAGE hi
        ldy #$00
        sty zpTOP      ; TOP lo=0
        sty zpTRFLAG   ; TRACE OFF
        sta (zpTOP),Y  ; place CR at PAGE/TOP
        lda #$FF
        iny
        sta (zpTOP),Y  ; place $ff after that
        iny
        sty zpTOP      ; TOP=PAGE+2
    .endif

; Warm start

FSASET:
    jsr SETFSA         ; Clear variables, heap, stack

; IMMEDIATE LOOP
; ==============

CLRSTK:
    ldy #>BUFFER       ; point zpLINE to keyboard buffer
    sty zpLINE+1
    ldy #<BUFFER
    sty zpLINE
    lda #<BASERR       ; default error message
    sta zpERRORLH
    lda #>BASERR
    sta zpERRORLH+1
    lda #'>'
    jsr BUFF           ; Print '>' prompt, read input to buffer at (zpWORK)

; Execute line at program pointer in zpLINE
; -----------------------------------------

RUNTHG:
    lda #<BASERR       ; default error message
    sta zpERRORLH
    lda #>BASERR
    sta zpERRORLH+1

    ldx #$FF
    stx zpBYTESM       ; OPT=$FF - not within assembler
    stx zpWORK+5       ; constant, enable tokenising line number
    txs                ; Clear machine stack

    jsr SETVAR         ; Clear DATA and stacks, returns with A=0

    tay                ; Y=0
    lda zpLINE         ; Point zpWORK to program line
    sta zpWORK
    lda zpLINE+1
    sta zpWORK+1
    sty zpWORK+4       ; mode is start/left of statement
    sty zpCURSOR       ; reset CURSOR position

    jsr MATCHA         ; tokenise keyboard buffer
    jsr SPTSTN         ; see if it started with a line number
    bcc DC             ; jump forward if no line number

    jsr INSRT          ; Insert into program
    jmp FSASET         ; Jump back to immediate loop

; Command entered at immediate prompt
; -----------------------------------

DC:                    ; Direct Command
    jsr SPACES         ; Skip spaces at (zpLINE)
    cmp #tknAUTO       ; first command token
    bcs DISPATCH       ; if command token, jump to execute command
    bcc LETST          ; not command token, try variable assignment

LEAVE:                 ; trampoline for branches below
    jmp CLRSTK         ; Jump back to immediate mode

; [ - enter assembler
; ===================

JUMPASS:
    jmp ASS         ; Jump to assembler

; =<value> - return from FN
; =========================
; Stack needs to contain these items:
;
; (top) ($01ff)
;   tknFN
;   zpCURSOR
;   zpLINE
;   zpLINE+1
;   number of parameters
;   zpAECUR
;   zpAELINE
;   zpAELINE+1
;   return address MSB
;   return address LSB
; (bottom)

FNRET:
    tsx
    cpx #$FC
    bcs FNERR     ; If stack is empty, jump to give error

    lda $01FF
    cmp #tknFN
    bne FNERR     ; If pushed token != 'FN', give error

    jsr AEEXPR    ; Evaluate expression
    jmp FDONE     ; Check for end of statement and return to pop from function

FNERR:
    brk
    dta 7
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknFN
    brk

; ----------------------------------------------------------------------------

; Check for =, *, [ commands
; ==========================

OTSTMT:
    ldy zpCURSOR
    dey               ; step program pointer back
    lda (zpLINE),Y    ; get the character
    cmp #'='
    beq FNRET         ; Jump for '=', return from FN

    cmp #'*'
    beq DOS           ; Jump for '*', embedded *command

    cmp #'['
    beq JUMPASS       ; Jump for '[', start assembler
    bne SUNK          ; Otherwise, see if end of statement

; ----------------------------------------------------------------------------

; Embedded *command
; =================

DOS:
    jsr CLYADP         ; Update zpLINE to current address

    ldx zpLINE
    ldy zpLINE+1       ; XY => command string

    .ifdef MOS_ATOM
        jsr cmdStar    ; Pass command at (zpLINE) to Atom OSCLI
    .endif

    .ifdef MOS_BBC
        jsr OS_CLI     ; Pass command at (zpLINE) to OSCLI
    .endif

; DATA, DEF, REM, ELSE
; ====================
; Skip to end of line
; -------------------

DATA:
DEF:
REM:
    lda #$0D         ; EOL/CR to match to
    ldy zpCURSOR     ; get program pointer
    dey              ; pre-decrement because loops start with iny

ILP:
    iny
    cmp (zpLINE),Y
    bne ILP          ; Loop until <cr> found

ENDEDL:
    cmp #tknELSE
    beq REM          ; If 'ELSE', jump to skip to end of line

    lda zpLINE+1
    cmp #>BUFFER     ; Program in command buffer?
    beq LEAVE        ; if so, jump back to immediate loop

    jsr LINO

    bne STMT         ; Check for end of program, step past <cr>

; Main execution loop
; -------------------

SUNK:
    dec zpCURSOR

DONEXT:
    jsr DONE          ; check for end of statement

NXT:
    ldy #$00
    lda (zpLINE),Y    ; Get current character
    cmp #':'
    bne ENDEDL        ; Not <colon>, check for ELSE

STMT:
    ldy zpCURSOR      ; Get program pointer
    inc zpCURSOR      ; increment for next time
    lda (zpLINE),Y    ; Get current character
    cmp #' '
    beq STMT          ; Skip spaces, and get next character

    cmp #tknPTR2
    bcc LETST         ; Not program command, jump to try variable assignment

; Dispatch function/command
; -------------------------

DISPATCH:
    tax                         ; Index into dispatch table
    lda ADTABL-FIRST_TOKEN,X    ; Get routine address from table
    sta zpWORK
    lda ADTABH-FIRST_TOKEN,X
    sta zpWORK+1
    jmp (zpWORK)        ; Jump to routine

; ----------------------------------------------------------------------------

; Not a command byte, try variable assignment, or =, *, [
; -------------------------------------------------------

LETST:
    ldx zpLINE
    stx zpAELINE    ; Copy LINE to AELINE
    ldx zpLINE+1
    stx zpAELINE+1
    sty zpAECUR

    jsr LVCONT      ; Check if variable or indirection
    bne GOTLT       ; NE - jump for existing variable or indirection assignment
    bcs OTSTMT      ; CS - not variable assignment, try =, *, [ commands

; Variable not found, create a new one
; ------------------------------------
    stx zpAECUR
    jsr EQEAT         ; Check (zpAELINE) for '=' and step past
    jsr CREATE        ; Create new variable

    ldx #$05          ; X=$05 = float
    cpx zpIACC+2
    bne LETSZ         ; Jump if dest. not a float

    inx               ; X=$06

LETSZ:
    jsr CREAX
    dec zpCURSOR

; LET variable = expression
; =========================

LET:
    jsr CRAELV
    beq NOLET

GOTLT:
    bcc LETED
    jsr PHACC         ; Stack integer (address of data)
    jsr EQEXPR        ; Check for end of statement

    lda zpTYPE        ; Get evaluation type
    bne LETM          ; If not string, error

    jsr STSTOR        ; Assign the string
    jmp NXT           ; Return to execution loop

LETED:
    jsr PHACC         ; Stack integer (address of data)
    jsr EQEXPR        ; Check for end of statement
    lda zpTYPE        ; Get evaluation type
    beq LETM          ; If not number, error

    jsr STORE         ; Assign the number
    jmp NXT           ; Return to execution loop

NOLET:
    jmp STDED         ; syntax error

LETM:
    brk
    dta 6
    .if foldup == 1
        dta 'TYPE MISMATCH'
    .else
        dta 'Type mismatch'
    .endif
    brk

; String Assignments

STSTOR:
    jsr POPACC       ; Unstack integer (address of data)

STSTRE:
    lda zpIACC+2
    cmp #$80
    beq NSTR         ; Jump if absolute string $addr

    ldy #$02
    lda (zpIACC),Y
    cmp zpCLEN
    bcs ALLOCX       ; old mlen >= to new len, enough room

    lda zpFSA        ; save current setting of VARTOP/FSA
    sta zpIACC+2
    lda zpFSA+1
    sta zpIACC+3

    lda zpCLEN
    cmp #$08         ; if <8 characters then use this as mlen
    bcc ALLOCU

    adc #$07         ; add 8 because carry is set, some room for growth
    bcc ALLOCU       ; jump if no (unsigned) overflow

    lda #$FF         ; give the string a length of $ff

ALLOCU:
    clc
    pha              ; save (new) length on stack
    tax              ; and in X
    lda (zpIACC),Y
    ldy #$00
    adc (zpIACC),Y
    eor zpFSA
    bne ALLJIM       ; is new space contiguous to old?

    iny
    adc (zpIACC),Y
    eor zpFSA+1
    bne ALLJIM      ; is new space contiguous to old?

    sta zpIACC+3    ; new space is, so reduce amount needed
    txa
    iny
    sec
    sbc (zpIACC),Y
    tax

ALLJIM:
    txa             ; get length to be allocated from X
    clc
    adc zpFSA       ; add VARTOP
    tay             ; store in Y

    lda zpFSA+1     ; MSB of VARTOP
    adc #$00        ; add possible carry

    cpy zpAESTKP    ; are we hitting our heads on the roof?
    tax             ; resulting MSB in X
    sbc zpAESTKP+1
    bcs ALLOCR      ; jump to No room error

    sty zpFSA       ; store LSB
    stx zpFSA+1     ; store MSB

    pla             ; get back the allocated length of the new string
    ldy #$02
    sta (zpIACC),Y  ; save new mlen
    dey
    lda zpIACC+3
    beq ALLOCX      ; we have expanded the same starting location

    sta (zpIACC),Y  ; change the start address of the string, save MSB
    dey
    lda zpIACC+2
    sta (zpIACC),Y  ; save LSB

ALLOCX:
    ldy #$03
    lda zpCLEN      ; get the length of the string
    sta (zpIACC),Y  ; store it in the block
    beq STDONE      ; return if length is zero

    dey
    dey
    lda (zpIACC),Y  ; copy MSB and LSB of start address and store it
    sta zpIACC+3
    dey
    lda (zpIACC),Y
    sta zpIACC+2

LTCVRM:
    lda STRACC,Y        ; get character from string buffer
    sta (zpIACC+2),Y    ; save it in the variable area
    iny
    cpy zpCLEN
    bne LTCVRM          ; continue until the end of the string is met

STDONE:
    rts

; ----------------------------------------------------------------------------

; Assignments for defined address strings

NSTR:
    jsr OSSTRT      ; place CR at end of string, returns w/ A=$0d and Y=length
    cpy #$00
    beq NSTRX       ; zero length string, jump to end

NSLOOP:
    lda STRACC,Y    ; copy from string buffer
    sta (zpIACC),Y  ; to variable
    dey
    bne NSLOOP      ; untile Y becomes zero

    lda STRACC      ; last byte

NSTRX:
    sta (zpIACC),Y  ; store and exit
    rts

; ----------------------------------------------------------------------------

ALLOCR:
    brk
    dta 0
    .if foldup == 1
        dta 'NO ROOM'
    .else
        dta 'No room'
    .endif
    brk

; ----------------------------------------------------------------------------

; Unstack a parameter

STORST:
    lda zpWORK+2        ; get type of item
    cmp #$80
    beq STORSX          ; jump if it's a string at a defined address
    bcc STORIT          ; jump if it's numeric

    ldy #$00            ; it's a dynamic string
    lda (zpAESTKP),Y    ; get length from top of stack
    tax                 ; save in X
    beq STORSY          ; skip copy if length is 0

    lda (zpWORK),Y      ; subtract 1 because of length byte
    sbc #$01
    sta zpWORK+2        ; store as new pointer
    iny
    lda (zpWORK),Y
    sbc #$00
    sta zpWORK+3

STORSL:
    lda (zpAESTKP),Y    ; get character from stacked string
    sta (zpWORK+2),Y    ; save in variable area
    iny
    dex
    bne STORSL          ; continue until length is zero

STORSY:
    lda (zpAESTKP,X)    ; get length of string again (X=0)
    ldy #$03
STORSW:
    sta (zpWORK),Y      ; save in string information block
    jmp POPSTX          ; discard string on top of stack

    ; strings at fixed address

STORSX:
    ldy #$00
    lda (zpAESTKP),Y    ; get length of string
    tax                 ; save in X
    beq STORSZ          ; jump if length is 0

STORSV:
    iny
    lda (zpAESTKP),Y    ; get character
    dey                 ; adjust for different indeces, double iny in loop
    sta (zpWORK),Y      ; store in variable
    iny
    dex
    bne STORSV          ; loop until length is zero

STORSZ:
    lda #$0D            ; place CR at end of string
    bne STORSW

    ; numeric entries

STORIT:
    ldy #$00
    lda (zpAESTKP),Y    ; get first byte
    sta (zpWORK),Y      ; and save it

    .if version < 3
        iny             ; Y=1
        cpy zpWORK+2    ; compare with type
        bcs STORIY      ; exit, it's an 8-bit value
    .elseif version >= 3
        ldy #4
        lda zpWORK+2
        beq STORIY      ; exit, it's an 8-bit value, but leave with Y=4 (why?)
        ldy #$01        ; continue with Y=1 again
    .endif
    lda (zpAESTKP),Y    ; second byte
    sta (zpWORK),Y

    iny
    lda (zpAESTKP),Y    ; third byte
    sta (zpWORK),Y

    iny
    lda (zpAESTKP),Y    ; fourth byte
    sta (zpWORK),Y

    iny
    cpy zpWORK+2        ; check if it's float
    bcs STORIY

    lda (zpAESTKP),Y    ; fifth byte
    sta (zpWORK),Y
    iny

STORIY:
    tya                 ; transfer number of bytes to A
    clc
    jmp POPN            ; pop A number of bytes

; ----------------------------------------------------------------------------

; PRINT#, called indirect via PRINT routine when # is detected

PRINTH:
    dec zpCURSOR        ; correct cursor position
    jsr AECHAN          ; find out file handle number

PRINHL:
    tya                 ; file handle to A
    pha                 ; and save
    jsr AESPAC          ; get next character
    cmp #','
    bne PRINHX          ; exit if not a comma

    jsr EXPR            ; evaluate expression
    jsr STARGA          ; store FACC in workspace temporary FWSA

    pla                 ; get file handle
    tay                 ; transfer back to Y
    lda zpTYPE
    jsr OSBPUT          ; write type of item to file

    tax                 ; type of item to X
    beq PRINHS          ; jump if item is a string
    bmi PRINHF          ; jump if item is floating point

    ; item is integer

    ldx #$03            ; 4 bytes (3-0), MSB first
PRINHQ:
    lda zpIACC,X
    jsr OSBPUT          ; write integer to file
    dex
    bpl PRINHQ
    bmi PRINHL          ; loop back to check for comma

PRINHF:
    ldx #$04            ; float is 5 bytes (4-0)

PRINHP:
    lda FWSA,X
    jsr OSBPUT          ; write float from workspace temporary FWSA
    dex
    bpl PRINHP
    bmi PRINHL          ; loop back to check for comma

PRINHS:
    lda zpCLEN
    jsr OSBPUT          ; write string length
    tax
    beq PRINHL          ; loop back to check for comma if length is 0

PRINHO:
    lda STRACC-1,X
    jsr OSBPUT          ; write string backwards
    dex
    bne PRINHO
    beq PRINHL          ; loop back to check for comma

PRINHX:
    pla                 ; get handle from stack
    sty zpCURSOR        ; save current offset
    jmp DONEXT

; end of PRINT statement

DEDPRC:
    jsr NLINE         ; Output new line and set COUNT to zero
DEDPR:
    jmp SUNK          ; Check end of statement, return to execution loop

; semi-colon encountered

PRFUNY:
    lda #$00
    sta zpPRINTS      ; Set current field to zero
    sta zpPRINTF      ; hex/dec flag to decimal

    jsr SPACES        ; Get next non-space character
    cmp #':'
    beq DEDPR         ; <colon> found, finish printing

    cmp #$0D
    beq DEDPR         ; <cr> found, finish printing

    cmp #tknELSE
    beq DEDPR         ; 'ELSE' found, finish printing
    bne CONTPR        ; Otherwise, continue into main loop

; PRINT [~][print items]['][,][;]
; ===============================

PRINT:
    jsr SPACES         ; Get next non-space char
    cmp #'#'
    beq PRINTH         ; If '#' jump to do PRINT#

    dec zpCURSOR
    jmp STRTPR         ; Jump into PRINT loop

; Print a comma

PRCOMM:
    lda VARL_AT
    beq STRTPR       ; If field width zero, no padding needed
                     ; jump back into main loop

    ; padding

    lda zpTALLY      ; Get COUNT

PRCOML:
    beq STRTPR       ; Zero, just started a new line, no padding
                     ; jump back into main print loop

    sbc VARL_AT      ; Get COUNT-field width
    bcs PRCOML       ; Loop to reduce until (COUNT MOD fieldwidth)<0

    tay              ; Y=number of spaces to get back to (COUNT MOD width)=zero
                     ; Y is a negative number

PRCOMO:
    jsr LISTPT
    iny
    bne PRCOMO       ; Loop to print required spaces until Y=0

STRTPR:
    clc              ; Prepare to print decimal
    lda VARL_AT
    sta zpPRINTS     ; Set current field width from @%

AMPER:
    ror zpPRINTF     ; Set hex/dec flag from Carry

ENDPRI:
    jsr SPACES       ; Get next non-space character

    cmp #':'
    beq DEDPRC       ; End of statement if <colon> found

    cmp #$0D
    beq DEDPRC       ; End if statement if <cr> found

    cmp #tknELSE
    beq DEDPRC       ; End of statement if 'ELSE' found

CONTPR:
    cmp #'~'
    beq AMPER        ; Jump back to set hex/dec flag from Carry, C=1

    cmp #','
    beq PRCOMM       ; Jump to pad to next print field

    cmp #';'
    beq PRFUNY       ; Jump to check for end of print statement

    jsr PRSPEC       ; Check for ' TAB SPC
    bcc ENDPRI       ; if print token found return to outer main loop

; All print formatting have been checked, so it now must be an expression

    lda zpPRINTS
    pha               ; save field width
    lda zpPRINTF
    pha               ; save hex flags, as evaluator might call PRINT
                      ; for example if a FN call uses PRINT
    dec zpAECUR       ; account for not finding anything so far
    jsr EXPR          ; Evaluate expression

    pla
    sta zpPRINTF      ; restore width
    pla
    sta zpPRINTS      ; restore hex flag

    lda zpAECUR
    sta zpCURSOR      ; Update program pointer
    tya
    beq PSTR          ; If type=0, jump to print string

    jsr FCON          ; Convert numeric value to string

    lda zpPRINTS      ; Get current field width
    sec
    sbc zpCLEN        ; A=width-stringlength
    bcc PSTR          ; length>width - print it
    beq PSTR          ; length=width - print it

    tay               ; Otherwise, Y=number of spaces to pad with
FPRNL:
    jsr LISTPT        ; print a space
    dey
    bne FPRNL         ; Loop to print required spaces to pad the number

; Print string in string buffer

PSTR:
    lda zpCLEN
    beq ENDPRI        ; Null string, jump back to main loop

    ldy #$00          ; Point to start of string
LOOP:
    lda STRACC,Y
    jsr CHOUT         ; Print character from string buffer
    iny
    cpy zpCLEN
    bne LOOP          ; Increment pointer, loop for full string

    beq ENDPRI        ; Jump back for next print item, branch always

TABCOM:
    jmp COMERR        ; 'Missing,' error

; TAB(X,Y) routine

TAB2:
    cmp #','
    bne TABCOM        ; No comma, jump to COMERR

    lda zpIACC
    pha               ; Save X coordinate of destination

    jsr BRA           ; evaluate expression and check for closing bracket
    jsr INTEGB        ; ensure result was an integer

; Atom - manually position cursor
; -------------------------------
    .ifdef MOS_ATOM
        lda #$1E
        jsr OSWRCH                ; Home cursor
        ldy zpIACC
        beq NO_YMOVEMENT          ; Y=0, no movement needed

        lda #$0A
MOVEY_CURSOR_LOOP:
        jsr OSWRCH                ; Move cursor down
        dey
        bne MOVEY_CURSOR_LOOP     ; Loop until Y position reached

NO_YMOVEMENT:
        pla
        beq NO_XMOVEMENT          ; X=0, no movement needed

        tay
        lda #$09
MOVEX_CURSOR_LOOP:
        jsr OSWRCH                ; Move cursor right
        dey
        bne MOVEX_CURSOR_LOOP     ; Loop until X position reached
NO_XMOVEMENT:
    .endif

; BBC - send VDU 31,x,y sequence
; ------------------------------
    .ifdef MOS_BBC
        lda #$1F
        jsr OSWRCH    ; TAB()
        pla
        jsr OSWRCH    ; X coord
        jsr WRIACC    ; Y coord
    .endif

    jmp PRTSTM        ; Continue to next PRINT item

; TAB(X) routine

TAB:
    jsr INEXPR         ; get integer expression
    jsr AESPAC         ; get next character
    cmp #')'
    bne TAB2           ; jump if not ')', try TAB(X,Y)

    ; carry is set

    lda zpIACC         ; destination column
    sbc zpTALLY        ; minus current column
    beq PRTSTM         ; jump if equal

    .if version < 3
        tay
    .elseif version >= 3
        tax
    .endif
    bcs SPCLOP         ; jump if difference is positive

    jsr NLINE          ; new line
    beq SPCT           ; jump to SPC routine, w/o getting integer expression

; SPC routine

SPC:
    jsr INTFAC         ; evaluate argument

SPCT:
    .if version < 3
        ldy zpIACC
    .elseif version >= 3
        ldx zpIACC
    .endif
    beq PRTSTM          ; jump if number of space is 0

SPCLOP:
    .if version < 3
        jsr LISTPT      ; print a space
        dey
        bne SPCLOP
    .elseif version >= 3
        jsr LISTPL      ; print spaces, counter in X
    .endif
    beq PRTSTM          ; skip jsr NLINE

PRCR:
    jsr NLINE

PRTSTM:
    clc                 ; clear carry to indicate item was dealt with
    ldy zpAECUR
    sty zpCURSOR        ; update cursor
    rts

PRSPEC:
    ldx zpLINE          ; set zpAELINE to zpLINE
    stx zpAELINE
    ldx zpLINE+1
    stx zpAELINE+1
    ldx zpCURSOR        ; also copy cursor offset
    stx zpAECUR

    cmp #$27            ; '
    beq PRCR

    cmp #tknTAB
    beq TAB

    cmp #tknSPC
    beq SPC

    sec                 ; indicate nothing was found

PRTSTO:
    rts

PRTSTN:
    jsr SPACES         ; Skip spaces, and get next character
    jsr PRSPEC
    bcc PRTSTO

    cmp #'"'
    beq PRSTRN

    sec
    rts

NSTNG:
    brk
    dta 9
    .if foldup == 1
        dta 'MISSING '
    .else
        dta 'Missing '
    .endif
    dta '"'
    brk

; print quoted string

PCH:
    jsr CHOUT           ; print character

PRSTRN:
    iny
    lda (zpAELINE),Y
    cmp #$0D
    beq NSTNG           ; print 'Missing "' if next character is CR

    cmp #'"'
    bne PCH             ; not '"', so print character

    iny
    sty zpAECUR
    lda (zpAELINE),Y
    cmp #'"'
    bne PRTSTM          ; check for another " to handle "", jump if not
    beq PCH             ; branch always, print "

; ----------------------------------------------------------------------------

; CLG
; ===
CLG:
    jsr DONE         ; Check end of statement
    lda #$10
    bne DOCL         ; Jump to do VDU 16, branch always

; CLS
; ===
CLS:
    jsr DONE         ; Check end of statement
    jsr BUFEND       ; Set COUNT to zero
    lda #$0C         ; Do VDU 12

DOCL:
    jsr OSWRCH
    jmp NXT          ; jump to execution loop

; ----------------------------------------------------------------------------

; CALL numeric [,items ... ]
; ==========================
CALL:
    jsr AEEXPR          ; evaluate expression, address of routine
    jsr INTEG           ; ensure it's an integer
    jsr PHACC           ; save on stack

    ldy #$00
    sty STRACC          ; zero number of parameters

CALLLP:
    sty ws+$06FF        ; pointer into parameter area
    jsr AESPAC          ; get next character

    cmp #','
    bne CALLDN          ; jump out of loop if no comma

    ldy zpAECUR
    jsr LVBLNKplus1     ; get variable name
    beq CALLFL          ; No such variable error

    ldy ws+$06FF
    iny
    lda zpIACC
    sta STRACC,Y        ; LSB of parameter address

    iny
    lda zpIACC+1
    sta STRACC,Y        ; MSB of parameter address

    iny
    lda zpIACC+2
    sta STRACC,Y        ; type of parameter

    inc STRACC          ; increment number of parameters

    jmp CALLLP

CALLDN:
    dec zpAECUR
    jsr AEDONE      ; check for end of statement
    jsr POPACC      ; pull address of the stack
    jsr USER        ; Set up registers and call code at IACC

    cld             ; ensure Binary mode on return
    jmp NXT         ; jump back to program loop

CALLFL:
    jmp FACERR

; Call code
; ---------
USER:
    lda VARL_C          ; get carry from C%
    lsr
    lda VARL_A          ; get A from A%
    ldx VARL_X          ; get X from X%
    ldy VARL_Y          ; get Y from Y%
    .if .def TARGET_C64
        jmp $ff9b
    .else
        jmp (zpIACC)    ; Jump to address in IACC
    .endif

; ----------------------------------------------------------------------------

DELDED:
    jmp STDED           ; Syntax error

; DELETE linenum, linenum
; =======================

DELETE:
    jsr SPTSTN          ; decode line number after the word DELETE
    bcc DELDED          ; syntax error

    jsr PHACC           ; push line number to the stack
    jsr SPACES
    cmp #','
    bne DELDED          ; no comma, syntax error

    jsr SPTSTN          ; decode next line number
    bcc DELDED          ; syntax error

    jsr DONE            ; check end of statement

    lda zpIACC          ; second line number to work pointer (zpWORK+2)
    sta zpWORK+2
    lda zpIACC+1
    sta zpWORK+3

    jsr POPACC          ; pop first line number into IACC

DODELS:
    jsr REMOVE          ; delete line number in IACC
    jsr TSTBRK          ; check escape key
    jsr INCACC          ; increment line number in IACC

    lda zpWORK+2        ; compare with second line number in zpWORK+2/3
    cmp zpIACC
    lda zpWORK+3
    sbc zpIACC+1
    bcs DODELS          ; continue removing if <

    jmp FSASET          ; jump to warm start

;  ----------------------------------------------------------------------------

; Decode parameters for RENUMBER and AUTO

GETTWO:
    lda #10             ; default value
    jsr SINSTK          ; store in IACC

    jsr SPTSTN          ; decode first line number
    jsr PHACC           ; push to stack

    lda #10             ; second default value
    jsr SINSTK          ; store in IACC

    jsr SPACES
    cmp #','
    bne NO              ; no comma, no second parameter

    jsr SPTSTN          ; get second line number
    lda zpIACC+1
    bne GETYUK          ; 'Silly' error if interval > 255

    lda zpIACC
    beq GETYUK          ; 'Silly' if interval = 0

    inc zpCURSOR        ; pre-increment for next decrement

NO:
    dec zpCURSOR
    jmp DONE            ; check for end of statement, and return (tail call)

; called by renumber

RENSET:
    lda zpTOP           ; copy TOP ptr to zpWORK+4/5
    sta zpWORK+4
    lda zpTOP+1
    sta zpWORK+5

RENSTR:
    lda zpTXTP          ; copy PAGE ptr to zpWORK+0/1
    sta zpWORK+1
    lda #$01            ; plus 1
    sta zpWORK
    rts

; RENUMBER [linenume [,linenum]]
; ==============================

RENUM:
    jsr GETTWO          ; decode the parameters
    ldx #zpWORK+2
    jsr POPX            ; pull starting line number to zpWORK+2..5

    jsr ENDER           ; check for 'Bad program'
    jsr RENSET          ; set zpWORK+4/5 to TOP ptr

; Build up table of line numbers

NUMBA:
    ldy #$00
    lda (zpWORK),Y      ; get line number of current line in the program
    bmi NUMBB           ; MSB > $7F, end of program

    sta (zpWORK+4),Y    ; save the MSB of the number in the pile at TOP
    iny
    lda (zpWORK),Y
    sta (zpWORK+4),Y    ; similar for LSB

    sec                 ; adjust pointer
    tya
    adc zpWORK+4
    sta zpWORK+4

    tax
    lda zpWORK+5
    adc #$00
    sta zpWORK+5

    cpx zpHIMEM         ; check if pointer colides with HIMEM
    sbc zpHIMEM+1
    bcs NUMBFL          ; if so, 'RENUMBER space' error

    jsr STEPON          ; Y=1, move pointer to the next line, returns with C=0
    bcc NUMBA           ; always loop back to the next line

NUMBFL:
    brk
    dta 0
    dta tknRENUMBER
    .if foldup == 1
        dta ' SPACE'      ; Terminated by following BRK
    .else
        dta ' space'      ; Terminated by following BRK
    .endif
GETYUK:
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
NUMBB:
    .if split == 1
        jmp NUMBB

; PROCname [(parameters)]
; =======================
PROC:
        lda zpLINE
        sta zpAELINE        ; AELINE=LINE --> after 'PROC' token
        lda zpLINE+1
        sta zpAELINE+1
        lda zpCURSOR
        sta zpAECUR
        lda #tknPROC
        jsr FNBODY          ; Call PROC/FN dispatcher
                            ; Will return here after ENDPROC
        jsr AEDONE          ; Check for end of statement
        jmp NXT             ; Return to execution loop
NUMBB:
    .endif

; Look for renumber references

    jsr RENSTR              ; set zpWORK+0/1 to PAGE+1

NUMBC:
    ldy #$00
    lda (zpWORK),Y          ; get MSB of line number
    bmi NUMBD               ; exit if the end of the program has been reached

    lda zpWORK+3            ; update the line number
    sta (zpWORK),Y
    lda zpWORK+2
    iny
    sta (zpWORK),Y

    clc                     ; add interval to line number
    lda zpIACC
    adc zpWORK+2
    sta zpWORK+2

    lda #$00
    adc zpWORK+3
    and #$7F                ; assure it doesn't exceed 32767
    sta zpWORK+3

    jsr STEPON              ; move to the next line, returns with C=0
    bcc NUMBC               ; always loop for handling next line

NUMBD:
    lda zpTXTP              ; copy PAGE to zpLINE
    sta zpLINE+1
    ldy #$00
    sty zpLINE

    iny
    lda (zpLINE),Y          ; get MSB of line number of current line
    .if version < 3
        bmi NUMBXX          ; exit if the end of the program has been reached
    .elseif version >= 3
        bmi NUMBX           ; exit if the end of the program has been reached
    .endif

NUMBE:
    ldy #$04                ; point to the first byte of the text of the line

NUMBF:
    lda (zpLINE),Y          ; get byte of text of the line
    cmp #tknCONST
    beq NUMBG               ; jump if tknCONST was found

    iny                     ; point to next byte (in case bne is taken)
    cmp #$0D
    bne NUMBF               ; loop as long as CR is not found

    lda (zpLINE),Y
    .if version < 3
        bmi NUMBXX          ; exit if the end of the program has been reached
    .elseif version >= 3
        bmi NUMBX           ; exit if the end of the program has been reached
    .endif

    ldy #$03
    lda (zpLINE),Y          ; add length of the line to zpLINE pointer
    clc
    adc zpLINE
    sta zpLINE
    bcc NUMBE               ; branch always, loop

    inc zpLINE+1
    bcs NUMBE               ; branch always, loop

NUMBXX:
    .if version < 3
        jmp FSASET          ; jump to warm start
    .endif

NUMBG:
    jsr SPGETN              ; decode the line number
    jsr RENSET              ; set zpWORK+4/5 to TOP, and zpWORK+0/1 to PAGE+1

NUMBH:
    ldy #$00
    lda (zpWORK),Y          ; get MSB of line number of current line
    bmi NUMBJ               ; exit if the end of the program has been reached

    lda (zpWORK+4),Y        ; get MSB of next line number in the pile
    iny                     ; Y=1
    cmp zpIACC+1            ; check against line number being sought
    bne NUMBI               ; skip forwards if they do not match

    lda (zpWORK+4),Y        ; same for the LSB
    cmp zpIACC
    bne NUMBI               ; jump if no match

    lda (zpWORK),Y          ; get line number
    sta zpWORK+6            ; and store it in zpWORK+6/7
    dey                     ; Y=0
    lda (zpWORK),Y
    sta zpWORK+7

    ldy zpCURSOR
    dey
    lda zpLINE              ; copy LINE pointer to WORK
    sta zpWORK
    lda zpLINE+1
    sta zpWORK+1

    jsr CONSTI              ; encode the line number and store it

NUMBFA:
    ldy zpCURSOR
    bne NUMBF               ; go back and continue the search

NUMBI:
    .if version >= 3
        clc
    .endif

    jsr STEPON              ; move to the next line

    lda zpWORK+4            ; add two to zpWORK+4/5
    adc #$02
    sta zpWORK+4
    bcc NUMBH               ; go back and look for the line number

    inc zpWORK+5
    bcs NUMBH               ; go back and look for the line number

NUMBX:
    .if version >= 3
        bmi ENDAUT          ; jump to warm start
    .endif

NUMBJ:
    jsr VSTRNG              ; Print inline text after this JSR
                            ; up to the first character with bit 7 set
    .if foldup == 1
        dta 'FAILED AT '
    .else
        dta 'Failed at '
    .endif

    iny                     ; opcode 0xc8 has bit 7 set
    lda (zpLINE),Y          ; get line number that couldn't be found
    sta zpIACC+1            ; into IACC
    iny
    lda (zpLINE),Y
    sta zpIACC

    jsr POSITE        ; Print in decimal
    jsr NLINE         ; Print newline
    beq NUMBFA        ; join the original code

STEPON:
    iny               ; make Y point to the byte giving the length of the line
    lda (zpWORK),Y    ; get the length
    adc zpWORK        ; add it to the pointer
    sta zpWORK        ; and store
    bcc STEPX

    inc zpWORK+1      ; adjust MSB if necessary
    clc

    ; always return with C=0

STEPX:
    rts

; ----------------------------------------------------------------------------

; AUTO [numeric [, numeric ]]
; ===========================
AUTO:
    jsr GETTWO      ; decode parameters

    lda zpIACC      ; save interval on machine stack
    pha
    jsr POPACC      ; get starting line number back

AUTOLP:
    jsr PHACC       ; save on AE stack
    jsr NPRN        ; print IACC using field width of five characters

    lda #' '
    jsr BUFF        ; get line of text with ' ' as the prompt

    jsr POPACC      ; pull lline number back
    jsr MATCH       ; tokenise the line
    jsr INSRT       ; insert the line into program
    jsr SETFSA      ; clear variable area and stacks

    pla             ; get interval back
    pha             ; and save for next loop
    clc
    adc zpIACC      ; add interval to current line number
    sta zpIACC
    bcc AUTOLP      ; loop to next line number

    inc zpIACC+1
    bpl AUTOLP      ; loop to next line number

    ; fallthrough if line number becomes >= 32768

ENDAUT:
    jmp FSASET      ; jump to warm start

; ----------------------------------------------------------------------------

; Code related to DIM, reserve space, and if necessary, create the
; variable indicated and adjust VARTOP

DIMSPR:
    jmp DIMRAM      ; jump to 'DIM space' error message

DIMSP:
    dec zpCURSOR
    jsr CRAELV      ; get the name of the variable
    beq NOTGO       ; give 'Bad DIM' error
    bcs NOTGO       ; if name is a string or invalid

    jsr PHACC       ; save the address of the variable
    jsr INEXPR      ; evaluate integer expression (number of bytes to reserve)
    jsr INCACC      ; increment by 1 for "zeroth" elementh

    lda zpIACC+3
    ora zpIACC+2
    bne NOTGO       ; 'Bad DIM' if attempting to reserve too much

    clc
    lda zpIACC      ; add LSB of reserved size to
    adc zpFSA       ; VARTOP's LSB
    tay             ; save in Y

    lda zpIACC+1    ; add MSB of reserved size to
    adc zpFSA+1     ; VARTOP's MSB
    tax             ; save in X

    cpy zpAESTKP    ; compare
    sbc zpAESTKP+1  ; with top of AE stack
    bcs DIMSPR      ; if there's not enough space, error with 'DIM space'

    lda zpFSA       ; save current VARTOP in IACC
    sta zpIACC
    lda zpFSA+1
    sta zpIACC+1

    sty zpFSA       ; store new VARTOP values
    stx zpFSA+1

    lda #$00        ; clear top 16-bits of 32-bit IACC
    sta zpIACC+2
    sta zpIACC+3

    lda #$40        ; set type to integer
    sta zpTYPE

    jsr STORE       ; assigne the variable
    jsr ASCUR       ; move CURSOR to AECUR
    jmp DIMNXT      ; join main DIM code

NOTGO:
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

DIM:
    jsr SPACES      ; skip any spaces after DIM keyword

    tya             ; get zpLINE offset (zpCURSOR) in A

    clc
    adc zpLINE      ; add offset to LINE pointer
    ldx zpLINE+1    ; MSB in X
    bcc DIMNNO

    inx             ; handle carry overflow, final MSB in X
    clc

DIMNNO:
    sbc #$00        ; we arrive here with C=0, so subtract 1
    sta zpWORK      ; store LSB in zpWORK
    txa             ; MSB to A
    sbc #$00        ; adjust for underflow
    sta zpWORK+1    ; store MSB in zpWORK+1

    ldx #$05        ; we start of with sizeof each element = 5 = floating point
    stx zpWORK+8

    ldx zpCURSOR
    jsr WORD        ; get the name of the array being defined

    cpy #$01        ; if Y still points to the first position
    beq NOTGO       ; jump to 'Bad DIM' error

    cmp #'('        ; first character after variable name
    beq DIMVAR      ; jump to dim floats

    cmp #'$'
    beq DIMINT      ; jump to dim int or string

    cmp #'%'
    bne DIMSPA      ; not ints, no '(', jump to dim space

DIMINT:
    dec zpWORK+8    ; decrement size of element to correspond to int and string
    iny             ; point to character after '%' or '$'
    inx
    lda (zpWORK),Y
    cmp #'('
    beq DIMVAR      ; jump to dim variable

DIMSPA:
    jmp DIMSP

DIMVAR:
    sty zpWORK+2    ; save length of the name
    stx zpCURSOR    ; set CURSOR to character after the name
    jsr LOOKUP      ; find the address of the variable
    bne NOTGO       ; error if variable is found (redimensioning not supported)

    jsr CREATE      ; create catalogue entry fo the array name

    ldx #$01        ; clear the single byte after the name, and make VARTOP
    jsr CREAX       ; to point after the zero byte terminating the name

    lda zpWORK+8    ; get number of bytes in each element
    pha             ; save on machine stack
    lda #$01        ; save the current offset into the subscript area
    pha             ; on the machine stack

    jsr SINSTK      ; set IACC to 1

RDLOOP:
    jsr PHACC       ; save IACC on BASIC stack
    jsr ASEXPR      ; evaluate the expression giving the (next) dimension

    lda zpIACC+1    ; check if subscript is greater than 16383
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne NOTGO       ; if so, jump to error

    jsr INCACC      ; increment IACC to allow for element zero

    pla             ; retrieve offset into subscript area
    tay
    lda zpIACC      ; save size in subscript area, LSB
    sta (zpFSA),Y
    iny
    lda zpIACC+1    ; idem, MSB
    sta (zpFSA),Y
    iny

    tya             ; save the pointer on the machine stack
    pha

    jsr SMUL        ; 16-bit multiply top of AE stack with IACC

    jsr SPACES      ; get the next character

    cmp #','
    beq RDLOOP      ; go back for the next subscript if it's a comma

    cmp #')'
    beq DIMGON      ; continue on closing bracket

    jmp NOTGO       ; 'Bad DIM' error

DIMGON:
    pla             ; offset into subscript storage area
    sta zpPRINTF
    pla             ; number of bytes per element
    sta zpWORK+8
    lda #$00
    sta zpWORK+9

    jsr WMUL        ; IACC = zpWORK+8/9 * IACC, total number of bytes required

    ldy #$00
    lda zpPRINTF    ; offset into subscript storage area
    sta (zpFSA),Y   ; store immediately after the zero following the name
    adc zpIACC      ; add this to the total number of bytes required
    sta zpIACC
    bcc NOHINC

    inc zpIACC+1

NOHINC:
    lda zpFSA+1     ; VARTOP to WORK
    sta zpWORK+1
    lda zpFSA
    sta zpWORK

    clc             ; add length of array to VARTOP
    adc zpIACC
    tay
    lda zpIACC+1
    adc zpFSA+1
    bcs DIMRAM      ; jump to 'DIM space' error

    tax             ; compare with AE stack pointer
    cpy zpAESTKP
    sbc zpAESTKP+1
    bcs DIMRAM      ; jump to 'DIM space' error

    sty zpFSA       ; store new VARTOP value
    stx zpFSA+1

    ; clear the just allocated area

    lda zpWORK
    adc zpPRINTF    ; add offset to first element
    tay
    lda #$00        ; A=0
    sta zpWORK
    bcc ZELOOP

    inc zpWORK+1

ZELOOP:
    sta (zpWORK),Y  ; A=0, clear
    iny
    bne ZELPA
    inc zpWORK+1

ZELPA:
    cpy zpFSA
    bne ZELOOP      ; clear loop
    cpx zpWORK+1
    bne ZELOOP      ; clear until VARTOP is reached

DIMNXT:
    jsr SPACES
    cmp #','
    beq DIMJ        ; skip exit

    jmp SUNK        ; exit

DIMJ:
    jmp DIM         ; go back and get the next array

DIMRAM:
    brk
    dta 11
    dta tknDIM
    .if foldup == 1
        dta ' SPACE'
    .else
        dta ' space'
    .endif
    brk

; ----------------------------------------------------------------------------

; Increment IACC by 1

INCACC:
    inc zpIACC
    bne INCDON
    inc zpIACC+1
    bne INCDON
    inc zpIACC+2
    bne INCDON
    inc zpIACC+3
INCDON:
    rts

; ----------------------------------------------------------------------------

; Integer Multiply, 16-bit, top of stack * IACC, result in IACC

SMUL:
    ldx #zpWORK+8
    jsr POPX            ; pop 32-bit int from AE stack

; multiply as 16-bit int with IACC as 16-bit int

WMUL:
    ldx #$00        ; X and Y are used as accumulators where the answer
    ldy #$00        ; is built up. Clear on entry

SMULA:
    lsr zpWORK+9    ; get least significant bit of multiplicand
    ror zpWORK+8
    bcc SMULB       ; do not add the multiplier to the YX accu if clear

    clc             ; add multiplier (zpIACC) to YX accu
    tya
    adc zpIACC
    tay
    txa
    adc zpIACC+1
    tax
    bcs SMULXE      ; 'Bad DIM' error on overflow

SMULB:
    asl zpIACC      ; multiply multiplier by 2 (one left shift)
    rol zpIACC+1

    lda zpWORK+8    ; check multiplicand
    ora zpWORK+9
    bne SMULA       ; continue until it's zero

    .if .hi(SMULA) != .hi(*)
        .error "ASSERT: SMUL loop crosses page"
    .endif

    sty zpIACC      ; store final answer
    stx zpIACC+1
    rts

SMULXE:
    jmp NOTGO

; ----------------------------------------------------------------------------

; HIMEM=numeric
; =============

LHIMM:
    jsr INEQEX          ; Set past '=', evaluate integer

    lda zpIACC
    sta zpHIMEM
    sta zpAESTKP        ; Set HIMEM and AE STACK pointer
    lda zpIACC+1
    sta zpHIMEM+1
    sta zpAESTKP+1

    jmp NXT             ; Jump back to execution loop

; ----------------------------------------------------------------------------

; LOMEM=numeric
; =============

LLOMM:
    jsr INEQEX         ; Step past '=', evaluate integer

    lda zpIACC
    sta zpLOMEM
    sta zpFSA          ; Set LOMEM and VARTOP
    lda zpIACC+1
    sta zpLOMEM+1
    sta zpFSA+1
    jsr SETVAL         ; Clear the dynamic variables catalogue

    beq LPAGEX         ; branch always, jump to execution loop

; ----------------------------------------------------------------------------

; PAGE=numeric
; ============

LPAGE:
    jsr INEQEX         ; Step past '=', evaluate integer
    lda zpIACC+1
    sta zpTXTP         ; Set PAGE
LPAGEX:
    jmp NXT            ; Jump to execution loop

; ----------------------------------------------------------------------------

; CLEAR
; =====

CLEAR:
    jsr DONE           ; Check end of statement
    jsr SETFSA         ; Clear heap, stack, data, variables
    beq LPAGEX         ; Jump to execution loop

; ----------------------------------------------------------------------------

; TRACE ON | OFF | numeric
; ========================

TRACE:
    jsr SPTSTN         ; check for line number
    bcs TRACNM         ; If line number, jump for TRACE linenum

    cmp #tknON
    beq TRACON         ; Jump for TRACE ON

    cmp #tknOFF
    beq TOFF           ; Jump for TRACE OFF

    jsr ASEXPR         ; Evaluate integer

; TRACE numeric
; -------------

TRACNM:
    jsr DONE           ; Check end of statement
    lda zpIACC
    sta zpTRNUM        ; Set trace limit low byte
    lda zpIACC+1
TRACNO:
    sta zpTRNUM+1      ; Set trace limit high byte
    lda #$FF           ; set TRACE ON
TRACNN:
    sta zpTRFLAG       ; Set TRACE flag
    jmp NXT            ; return to execution loop

; ----------------------------------------------------------------------------

; TRACE ON
; --------

TRACON:
    inc zpCURSOR       ; Step past
    jsr DONE           ; check end of statement
    lda #$FF
    bne TRACNO         ; Jump to set TRACE $FFxx

; ----------------------------------------------------------------------------

; TRACE OFF
; ---------

TOFF:
    inc zpCURSOR       ; Step past
    jsr DONE           ; check end of statement
    lda #$00
    beq TRACNN         ; Jump to set TRACE OFF

; ----------------------------------------------------------------------------

; TIME=numeric
; ============

LTIME:
    jsr INEQEX         ; Step past '=', evaluate integer
    .ifdef MOS_BBC
        ldx #zpIACC    ; YX = address of value, IACC
        ldy #$00       ; MSB always zero
        sty zpFACCS    ; set 5th byte to 0 (zpFACCS == zpIACC+4)
        lda #$02
        jsr OSWORD     ; Call OSWORD $02 to do TIME=
    .endif
    jmp NXT            ; Jump to execution loop

; ----------------------------------------------------------------------------

; Evaluate <comma><numeric>
; =========================

INCMEX:
    jsr COMEAT         ; Check for and step past comma

INEXPR:
    jsr EXPR           ; evaluate expression
    jmp INTEGB         ; check it is integer, tail call

; ----------------------------------------------------------------------------

; Evaluate <equals><integer>
; ==========================
INTFAC:
    jsr FACTOR         ; evaluate expression
    beq INTEGE         ; jump to 'Type mismatch' error
    bmi INTEGF         ; convert to int if it's a float

INTEGX:
    rts

INEQEX:
    jsr AEEQEX         ; Check for equals, evaluate numeric

INTEG:
    lda zpTYPE         ; Get result type

INTEGB:
    beq INTEGE         ; String, jump to 'Type mismatch'
    bpl INTEGX         ; Integer, return

INTEGF:
    jmp IFIX           ; Float, jump to convert to integer, tail call

INTEGE:
    jmp LETM           ; Jump to 'Type mismatch' error

; Evaluate <real>
; ===============

FLTFAC:
    jsr FACTOR         ; Evaluate expression

; Ensure value is float
; ---------------------

FLOATI:
    beq INTEGE         ; String, jump to 'Type mismatch'
    bmi INTEGX         ; float, return
    jmp IFLT           ; Integer, jump to convert to real

; ----------------------------------------------------------------------------

    .if split == 0
; PROCname [(parameters)]
; =======================
PROC:
        lda zpLINE
        sta zpAELINE      ; AELINE=LINE=>after 'PROC' token
        lda zpLINE+1
        sta zpAELINE+1
        lda zpCURSOR
        sta zpAECUR
        lda #tknPROC
        jsr FNBODY     ; Call PROC/FN dispatcher
                       ; Will return here after ENDPROC
        jsr AEDONE     ; Check for end of statement
        jmp NXT        ; Return to execution loop
    .endif

; ----------------------------------------------------------------------------

; LOCAL routine(s)

LOCSTR:
    ldy #$03
    lda #$00           ; Set length to zero
    sta (zpIACC),Y
    beq LOCVAR         ; Jump to look for next LOCAL item

; LOCAL variable [,variable ...]
; ==============================
LOCAL:
    tsx
    cpx #$FC          ; at least four bytes should be on the machine stack
    bcs NLOCAL        ; Not inside subroutine, error

    jsr CRAELV        ; get next variable name
    beq LOCEND        ; jump if bad variable name

    jsr RETINF        ; Push value on stack, push variable info on stack

    ldy zpIACC+2      ; get type of variable
    bmi LOCSTR        ; If a string, jump to make zero length

    jsr PHACC         ; save descriptor on stack

    lda #$00          ; Set IACC to zero
    jsr SINSTK        ; returns with A=$40

    sta zpTYPE        ; set type to integer

    jsr STORE         ; Set current variable to IACC (zero)

; Next LOCAL item
; ---------------
LOCVAR:
    tsx
    inc $0106,X       ; Increment number of LOCAL items
                      ; machine will crash with too many
                      ; local variables and/or parameters

    ldy zpAECUR
    sty zpCURSOR      ; Update line pointer offset
    jsr SPACES        ; Get next character
    cmp #','
    beq LOCAL         ; Comma, loop back to do another item

    jmp SUNK          ; Jump to main execution loop

LOCEND:
    jmp DONEXT        ; jump to main loop

; ----------------------------------------------------------------------------

; ENDPROC
; =======
; Stack needs to contain these items:
;
; (top) ($01ff)
;   tknFN
;   zpCURSOR
;   zpLINE
;   zpLINE+1
;   number of parameters
;   zpAECUR
;   zpAELINE
;   zpAELINE+1
;   return address MSB
;   return address LSB
; (bottom)

ENDPR:
    tsx
    cpx #$FC        ; should be at least four bytes
    bcs NOPROC      ; If stack empty, jump to print error

    lda $01FF
    cmp #tknPROC
    bne NOPROC      ; If pushed token != 'PROC', print error

    jmp DONE        ; Check end of statement and return to pop from subroutine

NOPROC:
    brk
    dta 13
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknPROC       ; Terminated by following BRK

NLOCAL:
    brk
    dta 12
    .if foldup == 1
        dta 'NOT '
    .else
        dta 'Not '
    .endif
    dta tknLOCAL      ; Terminated by following BRK

MODESX:
    brk
    dta $19
    .if foldup == 1
        dta 'BAD '
    .else
        dta 'Bad '
    .endif
    dta tknMODE
    brk

; ----------------------------------------------------------------------------

; GCOL numeric, numeric
; =====================

GRAPH:
    jsr ASEXPR         ; evaluate modifier

    lda zpIACC
    pha                ; save result on stack

    jsr INCMEX         ; Step past comma, evaluate integer
    jsr AEDONE         ; Update program pointer, check for end of statement

    lda #$12
    jsr OSWRCH         ; Send VDU 18 for GCOL
    jmp SENTWO         ; Jump to send two bytes to OSWRCH

; ----------------------------------------------------------------------------

; COLOUR numeric
; ==============

COLOUR:
    lda #$11
    pha                ; Stack VDU 17 for COLOUR
    jsr ASEXPR         ; evaluate integer expression
    jsr DONE           ; check end of statement
    jmp SENTWO         ; Jump to send two bytes to OSWRCH

; ----------------------------------------------------------------------------

; MODE numeric
; ============

MODES:
    lda #$16
    pha               ; Stack VDU 22 for MODE
    jsr ASEXPR        ; evaluate integer expression
    jsr DONE          ; check end of statement

; BBC - Check if changing MODE will move screen into stack
; --------------------------------------------------------
    .ifdef MOS_BBC
        jsr GETMAC    ; Get machine address high word
        cpx #$FF      ; later BASICs use inx
        bne MODEGO    ; Not $xxFFxxxx, skip memory test
        cpy #$FF      ; later BASICs use iny
        bne MODEGO    ; Not $FFFFxxxx, skip memory test

                      ; MODE change in I/O processor, must check memory limits
        lda zpAESTKP
        cmp zpHIMEM
        bne MODESX    ; STACK<>HIMEM, stack not empty, give 'Bad MODE' error

        lda zpAESTKP+1
        cmp zpHIMEM+1
        bne MODESX

        ldx zpIACC
        lda #$85
        jsr OSBYTE    ; Get top of memory if we used this MODE

        cpx zpFSA
        tya
        sbc zpFSA+1
        bcc MODESX    ; Would be below VAREND, give error

        cpx zpTOP
        tya
        sbc zpTOP+1
        bcc MODESX    ; Would be below TOP, give error

        ; BASIC stack is empty, screen would not hit heap or program

        stx zpHIMEM
        stx zpAESTKP   ; Set STACK and HIMEM to new address
        sty zpHIMEM+1
        sty zpAESTKP+1
    .endif

; Change MODE
MODEGO:
    jsr BUFEND         ; Set COUNT to zero

; Send two bytes to OSWRCH, stacked byte, then IACC
; -------------------------------------------------
SENTWO:
    pla
    jsr OSWRCH        ; Send stacked byte to OSWRCH
    jsr WRIACC        ; Print byte in IACC
    jmp NXT           ; jump to execution loop

; ----------------------------------------------------------------------------

; MOVE numeric, numeric
; =====================
MOVE:
    lda #$04
    bne DRAWER         ; Jump forward to do PLOT 4 for MOVE

; ----------------------------------------------------------------------------

; DRAW numeric, numeric
; =====================
DRAW:
    lda #$05          ; Do PLOT 5 for DRAW
DRAWER:
    pha
    jsr AEEXPR        ; Evaluate first expression
    jmp PLOTER        ; Jump to evaluate second expression and send to OSWRCH

; ----------------------------------------------------------------------------

; PLOT numeric, numeric, numeric
; ==============================
PLOT:
    jsr ASEXPR        ; evaluate integer expression

    lda zpIACC
    pha               ; save on stack

    jsr COMEAT        ; eat comma
    jsr EXPR          ; evaluate expression

PLOTER:
    jsr INTEG         ; Confirm numeric and ensure is integer
    jsr PHACC         ; Push IACC
    jsr INCMEX        ; Step past command and evaluate integer
    jsr AEDONE        ; Update program pointer, check for end of statement

    lda #$19
    jsr OSWRCH        ; Send VDU 25 for PLOT

    pla
    jsr OSWRCH        ; Send PLOT action
    jsr POPWRK        ; Pop integer to temporary store at $37/8

    lda zpWORK
    jsr OSWRCH        ; Send first coordinate to OSWRCH LSB

    lda zpWORK+1
    jsr OSWRCH        ; MSB

    jsr WRIACC        ; Send IACC to OSWRCH, second coordinate

    lda zpIACC+1
    jsr OSWRCH        ; Send IACC high byte to OSWRCH

    jmp NXT           ; Jump to execution loop

; ----------------------------------------------------------------------------

VDUP:
    lda zpIACC+1
    jsr OSWRCH        ; Send IACC byte 2 to OSWRCH

; ----------------------------------------

; VDU num[,][;][...]
; ==================

VDU:
    jsr SPACES         ; Get next character

VDUL:
    cmp #':'
    beq VDUX           ; If end of statement, jump to exit

    cmp #$0D
    beq VDUX           ; exit if CR

    cmp #tknELSE
    beq VDUX           ; exit if ELSE token

    dec zpCURSOR       ; Step back to current character
    jsr ASEXPR         ; evaluate integer expression
    jsr WRIACC         ; and output low byte
    jsr SPACES         ; Get next character

    cmp #','
    beq VDU            ; Comma, loop to read another number

    cmp #';'
    bne VDUL           ; Not semicolon, loop to check for end of statement
    beq VDUP           ; Loop to output high byte and read another

VDUX:
    jmp SUNK           ; Jump to execution loop

; ----------------------------------------------------------------------------

; Send IACC to OSWRCH via WRCHV
; =============================
WRIACC:
    lda zpIACC
    .if WRCHV != 0
        jmp (WRCHV)
    .elseif WRCHV == 0
        jmp OSWRCH
    .endif

; ----------------------------------------------------------------------------

; VARIABLE PROCESSING
; ===================
; Look for a FN/PROC in heap
; --------------------------
; On entry, (zpWORK)+1=>FN/PROC token (ie, first character of name)
;

CREAFN:
    ldy #$01
    lda (zpWORK),Y      ; Get PROC/FN character
    ldy #$F6            ; Point to PROC list start
    cmp #tknPROC
    beq CREATF          ; If PROC, jump to scan list

    ldy #$F8            ; cataloge offset for functions
    bne CREATF          ; Point to FN list start and scan list

; ----------------------------------------------------------------------------

; Look for a variable in the heap
; -------------------------------
; LOOKUP is given a base address-1 in WORK,WORK+1, length+1 in WORK+2
; It returns with Z flag set if it can't find the thing, else with
; IACC,IACC+1 pointing to the data item and Z cleared
;
; The routine uses two slightly different search routines, which are used
; alternately. This is done to reduce the amount of swapping of pointers

LOOKUP:
    ldy #$01
    lda (zpWORK),Y      ; Get first character of variable
    asl                 ; Double it to index into index list
    tay

; Scan though linked lists in heap
; --------------------------------
CREATF:
    lda VARL,Y
    sta zpWORK+3          ; Get start of linked list
    lda VARL+1,Y
    sta zpWORK+4

CREATLP:
    lda zpWORK+4
    beq CREAEOLST         ; End of list reached

    ldy #$00              ; store link address of the next entry in zpWORK+5/6
    lda (zpWORK+3),Y
    sta zpWORK+5

    iny
    lda (zpWORK+3),Y
    sta zpWORK+6

    iny
    lda (zpWORK+3),Y       ; get first character of name
    bne CREATG             ; Jump if not null name

    dey
    cpy zpWORK+2           ; compare length
    bne CREATH             ; no match, continue with next entry

    iny
    bcs CREATI             ; branch always, carry is set by cpy before

CREATL:
    iny
    lda (zpWORK+3),Y       ; next character from the catalogue entry
    beq CREATH             ; jump if zero marking end is found

CREATG:
    cmp (zpWORK),Y         ; compare with name being sought
    bne CREATH             ; jump if no match

    cpy zpWORK+2
    bne CREATL             ; end of sought variable name has not been reached

    iny
    lda (zpWORK+3),Y       ; next character from the catalogue entry
    bne CREATH             ; jump if not end yet

CREATI:
    tya                    ; make IACC point to the zero byte after the name
    adc zpWORK+3
    sta zpIACC
    lda zpWORK+4
    adc #$00
    sta zpIACC+1

CREAEOLST:
    rts

CREATH:
    lda zpWORK+6
    beq CREAEOLST           ; no more entries for the same initial letter

    ldy #$00                ; get new link bytes from the new catalogue entry
    lda (zpWORK+5),Y
    sta zpWORK+3

    iny
    lda (zpWORK+5),Y
    sta zpWORK+4

    iny
    lda (zpWORK+5),Y        ; next character from catalogue entry
    bne CREATK              ; jump if not zero marking the end of the name

    dey
    cpy zpWORK+2            ; check against length of sought variable
    bne CREATLP             ; jump if not the same

    iny                     ; point to zero after the name
    bcs CREATM              ; carry always set, jump to exit

CREATJ:
    iny
    lda (zpWORK+5),Y        ; get next information block character
    beq CREATLP             ; move to the next block if the character is zero

CREATK:
    cmp (zpWORK),Y          ; check against character of sought variable
    bne CREATLP             ; jump if no match

    cpy zpWORK+2
    bne CREATJ              ; end of sought variable name has not been reached

    iny
    lda (zpWORK+5),Y
    bne CREATLP             ; jump to next info block if not zero marking

CREATM:
    tya                     ; make IACC point to the zero byte after the name
    adc zpWORK+5
    sta zpIACC
    lda zpWORK+6
    adc #$00
    sta zpIACC+1
    rts

LOOKFN:
    ldy #$01
    lda (zpWORK),Y
    tax
    lda #$F6                ; catalogue offset for procedures
    cpx #tknPROC
    beq LOOKMN
    lda #$F8                ; catalogue offset for functions
    bne LOOKMN

; ----------------------------------------------------------------------------

;  CREATE takes the same parameters as LOOKUP and adds the
;  data item to the linked list. It is not optimised.

CREATE:
    ldy #$01
    lda (zpWORK),Y      ; get the first letter of the variable
    asl                 ; multiply by 2

LOOKMN:
    sta zpWORK+3        ; use it to create pointer into variable catalogue
    lda #>VARL
    sta zpWORK+4

LOOPLB:
    lda (zpWORK+3),Y    ; get MSB of linked list
    beq LOOKK           ; jump if at the end of list sharing the same inital
                        ; letter as the one being created

    tax                 ; save MSB in X
    dey
    lda (zpWORK+3),Y    ; get LSB of linked list item
    sta zpWORK+3        ; store as new pointer
    stx zpWORK+4
    iny
    bpl LOOPLB          ; loop until we reach the end of the linked list

LOOKK:
    lda zpFSA+1         ; use VARTOP as location for new entry
    sta (zpWORK+3),Y
    lda zpFSA
    dey
    sta (zpWORK+3),Y

    tya                 ; A=Y=0
    iny
    sta (zpFSA),Y       ; save zero as the MSB, indicating end of list
    cpy zpWORK+2        ; compare length
    beq CREATX          ; exit if the name has only one letter

LOOPRA:
    iny
    lda (zpWORK),Y      ; copy next character
    sta (zpFSA),Y
    cpy zpWORK+2
    bne LOOPRA          ; loop until end of the name has been reached

    rts

; ----------------------------------------------------------------------------
;  CREAX updates FSA on the assumption that x-1 bytes are to be used
;  Y contains next address offset, four bytes are zeroed
;  If we run out of ram it resets the list end with the old
;  pointer in WORK+3/4

CREAX:
    lda #$00
CREZER:
    iny
    sta (zpFSA),Y       ; clear
    dex
    bne CREZER

FSAPY:
    sec                 ; set carry to add +1
    tya
    adc zpFSA           ; add Y+1 to VARTOP (LSB), keep in A
    bcc CREATY

    inc zpFSA+1         ; adjust MSB, never restored on 'No room' error?

CREATY:
    ldy zpFSA+1
    cpy zpAESTKP+1      ; compare against AE stack pointer
    bcc CREATZ          ; VARTOP MSB is lower, ok, exit
    bne CREATD          ; skip LSB check if MSB is different

    cmp zpAESTKP        ; check VARTOP LSB against AE stack pointer
    bcc CREATZ          ; jump to exit if lower

CREATD:
    lda #$00            ; zero MSB of link byte to stop block from existing
    ldy #$01
    sta (zpWORK+3),Y

                        ; BUG? MSB of VARTOP is not decremented if it was
                        ; increased before the test.
                        ; Possible fix: pair inc zpFSA+1 with inx,
                        ; here: dex:bne .skip:dec zpFSA+1:.skip

    jmp ALLOCR          ; 'No room' error

CREATZ:
    sta zpFSA           ; store LSB of VARTOP

CREATX:
    rts

; ----------------------------------------------------------------------------

; Get an array name
; Search for an array name starting at (zpWORK) + 1
; On exit, (zpWORK),Y point to the first character that could not be
; interpreted as part of the array name, and A contains this character.
; X is incremented for each character that is scanned.

WORD:
    ldy #$01        ; point to first character of name

WORDLP:
    lda (zpWORK),Y
    cmp #'0'
    bcc WORDDN      ; exit if < '0'

    cmp #'@'
    bcs WORDNA      ; continue if  >= '@'

    cmp #'9'+1
    bcs WORDDN      ; exit if > '9'

    ; it's a number

    cpy #$01
    beq WORDDN      ; exit if it's the first character

WORDNC:
    inx
    iny             ; point to next character
    bne WORDLP      ; and loop

WORDNA:
    cmp #'_'
    bcs WORDNB      ; jump if >= '_' (0x60, pound sign, ASCII backtick allowed)

    cmp #'Z'+1
    bcc WORDNC      ; continue if <= 'Z'

WORDDN:
    ; return value in C, C=0 is OK, C=1 is FAIL
    rts

WORDNB:
    cmp #'z'+1
    bcc WORDNC      ; continue if <= 'z'
    rts

; ----------------------------------------------------------------------------

CRAELT:
    jsr CREAX       ; clear the variable

; Get a variable, creating it if needed

CRAELV:
    jsr AELV        ; search for the variable name
    bne LVRTS       ; exit if variable exists
    bcs LVRTS       ; exit if variable is invalid

    jsr CREATE      ; create the variable

    ldx #$05        ; for ints and strings
    cpx zpIACC+2
    bne CRAELT      ; clear
    inx             ; for floats
    bne CRAELT      ; jump always, clear

LVFD:
    cmp #'!'
    beq UNPLIN      ; unary '!'

    cmp #'$'
    beq DOLL        ; $<address>

    eor #'?'
    beq UNIND       ; unary '?'

    lda #$00        ; variable not found, invalid
    sec

LVRTS:
    rts

; Unary '!'

UNPLIN:
    lda #$04        ; four bytes

; Unary '?'

UNIND:
    pha             ; save number of bytes
    inc zpAECUR     ; move cursor past '!' or '?'
    jsr INTFAC      ; get integer expression
    jmp INSET       ; join the code that deals with binary operators

; $<address>

DOLL:
    inc zpAECUR     ; move cursor past '$'
    jsr INTFAC      ; get integer expression

    lda zpIACC+1
    beq DOLLER      ; error if < 256

    lda #$80        ; string
    sta zpIACC+2
    sec
    rts

DOLLER:
    brk
    dta 8
    .if foldup == 1
        dta '$ RANGE'
    .else
        dta '$ range'
    .endif
    brk

; Copy LINE pointer to AE pointer, and skip spaces before searching for
; a variable name

AELV:
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    ldy zpCURSOR
    dey             ; one position back to allow for loop to start with iny

LVBLNK:
    iny

LVBLNKplus1:
    sty zpAECUR         ; save AE offset
    lda (zpAELINE),Y
    cmp #' '
    beq LVBLNK          ; loop to skip spaces

; enter here to search for a variable. A contains the first character
; of the alleged variable. This routine is optimized towards identifying
; resident integer variables as fast as it can.
; On exit, the address of the value of the variable is stored in IACC.
; If Z is set, the variable does not exist.
; If C is set, the variable is invalid
; If Z is clear, a valid, defined variable was found, and the carry flag
; will be set if the variable is a string variable
;
; Z=0, C=0 --> a defined numeric variable was found
; Z=0, C=1 --> a defined string variable was found
; Z=1, C=0 --> a valid but undefined variable was found
; Z=1, C=1 --> an invalid variable was found

LVCONT:
    cmp #'@'
    bcc LVFD        ; probably not an lv but check for unary things
                    ; this test also removes numeric first characters

    cmp #'['
    bcs MULTI       ; jump if >=

    ; upper case letter

    asl             ; multiply by four, makes it an index into VARL page
    asl
    sta zpIACC      ; store as LSB
    lda #>VARL
    sta zpIACC+1    ; store MSB

    iny
    lda (zpAELINE),Y
    iny
    cmp #'%'
    bne MULTI       ; jump if next character is not '%'

    ldx #$04        ; integer type/size
    stx zpIACC+2
    lda (zpAELINE),Y
    cmp #'('
    bne CHKQUE      ; jump if not '('

MULTI:
    ldx #$05        ; set type/size to float
    stx zpIACC+2

    lda zpAECUR     ; add offset to AELINE, result in XA
    clc
    adc zpAELINE
    ldx zpAELINE+1
    bcc BKTVNO

    inx
    clc

BKTVNO:
    sbc #$00        ; subtract 1 and save as WORK pointer
    sta zpWORK      ; pointing one position before the first character
    bcs BKTVNP

    dex

BKTVNP:
    stx zpWORK+1    ; MSB of pointer

    ldx zpAECUR     ; keep track of offset
    ldy #$01        ; offset to WORK pointer, start at 1

BKTVD:
    lda (zpWORK),Y
    cmp #'A'
    bcs BKTVA       ; jump if >= 'A'

    cmp #'0'
    bcc BKTVE       ; jump if < '0'

    cmp #'9'+1
    bcs BKTVE       ; jump if > '9'

    inx
    iny
    bne BKTVD       ; loop and get the next character

BKTVA:
    cmp #'Z'+1
    bcs BKTVDD      ; jump if > 'Z'

    inx
    iny
    bne BKTVD       ; loop and get the next character

BKTVDD:
    cmp #'_'
    bcc BKTVE       ; jump if < '_'

    cmp #'z'+1
    bcs BKTVE       ; jump if > 'z'

    inx
    iny
    bne BKTVD       ; loop and get the next character

BKTVE:
    dey
    beq BKTVFL      ; if Y is still 1, no character was valid, error

    cmp #'$'
    beq LVSTR       ; jump if it's a string

    cmp #'%'
    bne BKTVF       ; jump if it's not an integer but a float

    ; it's an integer

    dec zpIACC+2    ; decrement 5 to 4
    iny             ; compensate for dey at BKTVE
    inx             ; next character
    iny             ; next character
    lda (zpWORK),Y
    dey             ; Y must be maintained at 1 position to the left

BKTVF:
    sty zpWORK+2    ; save the length of the name
    cmp #'('
    beq BKTVAR      ; jump if we have an array

    jsr LOOKUP      ; find address of the variable
    beq BKTVFC      ; exit with Z=1,C=0 if the variable is undefined

    stx zpAECUR     ; update AELINE offset

CHKPLI:
    ldy zpAECUR
    lda (zpAELINE),Y

CHKQUE:
    cmp #'!'
    beq BIPLIN      ; jump to binary '!'

    cmp #'?'
    beq BIQUER      ; jump to binary '?'

    clc             ; indicate numeric, C=0
    sty zpAECUR     ; update cursor position
    lda #$FF        ; indicate valid, Z=0
    rts

BKTVFL:
    lda #$00    ; Z=1
    sec         ; C=1
    rts

BKTVFC:
    lda #$00    ; Z=1
    clc         ; C=0
    rts

; Binary '?'

BIQUER:
    lda #$00
    beq BIPLIN+2    ; skip lda #4 (could save byte with dta 0x2c (BIT abs))

; Binary '!'

BIPLIN:
    lda #$04        ; set type to 4
    pha             ; save on stack
    iny
    sty zpAECUR     ; increment past '?' or '!' and store cursor

    jsr VARIND      ; get the value of the variable scanned so far
    jsr INTEGB      ; make sure it's an integer

    lda zpIACC+1    ; save address on stack
    pha
    lda zpIACC
    pha

    jsr INTFAC      ; get the integer operand following '?' or '!'

    clc             ; add the address on the stack to the value of the operand
    pla
    adc zpIACC
    sta zpIACC
    pla
    adc zpIACC+1
    sta zpIACC+1

INSET:
    pla             ; restore type from stack
    sta zpIACC+2
    clc             ; C=0
    lda #$FF        ; Z=0
    rts

    ; Prepare for array

BKTVAR:
    inx             ; increment past the '('
    inc zpWORK+2

    jsr ARRAY       ; deal with the array

    jmp CHKPLI      ; check for indirection operators, tail call

    ; String

LVSTR:
    inx             ; increment past the '$' at the end of the name
    iny
    sty zpWORK+2    ; save the length of the name
    iny             ; point to next character
    dec zpIACC+2    ; decrement type to 4
    lda (zpWORK),Y
    cmp #'('
    beq LVSTRA      ; jump if string array

    jsr LOOKUP      ; find the address of the variable
    beq BKTVFC      ; error if undefined

    stx zpAECUR     ; update AE offset
    lda #$81        ; set type to $81, and Z=0
    sta zpIACC+2
    sec             ; C=1
    rts

    ; String array

LVSTRA:
    inx             ; increment past '('
    sty zpWORK+2
    dec zpIACC+2    ; decrement type to 3

    jsr ARRAY       ; handle array

    lda #$81        ; set type to $81, and Z=0
    sta zpIACC+2
    sec             ; C=1
    rts

UNARRY:
    brk
    dta 14
    .if foldup == 1
        dta 'ARRAY'
    .else
        dta 'Array'
    .endif
    brk

    ; Array

ARRAY:
    jsr LOOKUP      ; find address of the variable
    beq UNARRY      ; error if not found

    stx zpAECUR     ; update AE offset

    lda zpIACC+2    ; save type and address of the base of the array
    pha
    lda zpIACC
    pha
    lda zpIACC+1
    pha

    ldy #$00
    lda (zpIACC),Y  ; get number of elements of the array (stored as 2n+1)
    cmp #$04
    bcc AQUICK      ; jump if array is one dimensional

    tya
    jsr SINSTK      ; zero IACC

    lda #$01        ; save the pointer to the current subscript index
    sta zpIACC+3

ARLOP:
    jsr PHACC       ; save IACC on BASIC stack
    jsr INEXPR      ; evaluate next array index
    inc zpAECUR
    cpx #','
    bne UNARRY      ; error if no ',' is present

    ldx #zpWORK+2
    jsr POPX        ; pop IACC to zpWORK+2 onwards

    ldy zpWORK+5    ; get offset of current index

    pla             ; retrieve base address of the array to zpWORK
    sta zpWORK+1
    pla
    sta zpWORK

    pha             ; and stack it again
    lda zpWORK+1
    pha

    jsr TSTRNG      ; check current index against its limit

    sty zpIACC+3    ; save pointer to current element

    lda (zpWORK),Y  ; save next subscript limit
    sta zpWORK+8
    iny
    lda (zpWORK),Y
    sta zpWORK+9

    lda zpIACC      ; add preset subscript to total subscript
    adc zpWORK+2    ; (subscript into array as if it was one long array)
    sta zpIACC
    lda zpIACC+1
    adc zpWORK+3
    sta zpIACC+1

    jsr WMUL        ; multiply subscript bye the next subscript limit

    ldy #$00
    sec
    lda (zpWORK),Y  ; get offset of the last subscript
    sbc zpIACC+3    ; subtract offset of the current subscript
    cmp #$03
    bcs ARLOP       ; continue if more than one subscript is left

    jsr PHACC       ; save IACC
    jsr BRA         ; get expression and check for right hand bracket
    jsr INTEGB      ; ensure expression is integer

    pla             ; get base address of array
    sta zpWORK+1
    pla
    sta zpWORK

    ldx #zpWORK+2
    jsr POPX        ; POP to zpWORK+2 onwards

    ldy zpWORK+5    ; get the offset of the current subscript
    jsr TSTRNG      ; check against limit

    clc             ; add new subscript to toal subscript
    lda zpWORK+2
    adc zpIACC
    sta zpIACC
    lda zpWORK+3
    adc zpIACC+1
    sta zpIACC+1
    bcc ARFOUR

    ; single dimension arrays

AQUICK:
    jsr BRA         ; get expression and check for ')'
    jsr INTEGB      ; ensure it's integer

    pla             ; retrieve base address of the array
    sta zpWORK+1
    pla
    sta zpWORK

    ldy #$01
    jsr TSTRNG      ; check subscript limit

ARFOUR:
    pla             ; get type of array
    sta zpIACC+2    ; and save it

    cmp #$05
    bne ARFO        ; jump if it's not a floating point array

    ; multiply total subscript by 5

    ldx zpIACC+1    ; save MSB in X so we can add it later

    lda zpIACC
    asl zpIACC      ; *2
    rol zpIACC+1
    asl zpIACC      ; *2 (total is *4)
    rol zpIACC+1

    adc zpIACC      ; add original
    sta zpIACC
    txa
    adc zpIACC+1
    sta zpIACC+1    ; total is 4n+n = 5n
    bcc ARFI        ; skip code for integer and string arrays

    ; multiply total subscript by 4 for integer and string arrays
ARFO:
    asl zpIACC
    rol zpIACC+1
    asl zpIACC
    rol zpIACC+1

ARFI:
    tya             ; add length of the preamble
    adc zpIACC
    sta zpIACC
    bcc NNINC

    inc zpIACC+1
    clc

NNINC:
    lda zpWORK      ; add base addres to get the actual address of the element
    adc zpIACC
    sta zpIACC
    lda zpWORK+1
    adc zpIACC+1
    sta zpIACC+1    ; result in IACC
    rts

; check the array subscript

TSTRNG:
    lda zpIACC+1    ; ensure subscript is less than 16384
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne SUBSCP      ; error if it's not

    lda zpIACC      ; check against the limit
    cmp (zpWORK),Y
    iny
    lda zpIACC+1
    sbc (zpWORK),Y
    bcs SUBSCP      ; error if it's too big

    iny             ; return with Y past limit
    rts

SUBSCP:
    brk
    dta 15
    .if foldup == 1
        dta 'SUBSCRIPT'
    .else
        dta 'Subscript'
    .endif
    brk

; ----------------------------------------------------------------------------

; Line number routines

SPTSTM:
    inc zpCURSOR

SPTSTN:
    ldy zpCURSOR
    lda (zpLINE),Y
    cmp #' '
    beq SPTSTM          ; loop to skip spaces

    cmp #tknCONST
    bne FDA             ; exit if not CONST token


; Decode line number constant. bit 0-14 are encoded as:
;
;    Byte 1: | 0 | 1 |  bit7 |  bit6 |     0 | bit14 |    0 |   0
;    Byte 2: | 0 | 1 |  bit5 |  bit4 |  bit3 |  bit2 | bit1 | bit0
;    Byte 3: | 0 | 1 | bit13 | bit12 | bit11 | bit10 | bit9 | bit8
;
; bit6 and bit14 are stored inverted

SPGETN:
    iny
    lda (zpLINE),Y  ; get byte 1
    asl             ; shift bit7 and bit6 to the top
    asl
    tax             ; save in X for bit14
    and #$C0        ; mask top two bits
    iny
    eor (zpLINE),Y  ; XOR byte 2, the bottom 6 bits and invert bit6
    sta zpIACC      ; and store in IACC
    txa             ; retrieve shifted first byte
    asl             ; shift bit14 (already at bit4 position) to bit6 position
    asl
    iny
    eor (zpLINE),Y  ; XOR byte3, bottom 6 bits (bit8-bit13), invert bit6
    sta zpIACC+1    ; and store as high byte of IACC
    iny
    sty zpCURSOR    ; update cursor position
    sec             ; indicate line number was found
    rts

FDA:
    clc             ; indicate line number was not found
    rts

; ----------------------------------------------------------------------------

; Note: copying LINE to AELINE is replicated several times, and could
;       be made a subroutine

; Check for '=', evaluate Arithmetic Expression, check for end of statement

AEEQEX:
    lda zpLINE      ; copy LINE pointer to AELINE pointer
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    lda zpCURSOR    ; and its offset
    sta zpAECUR

EQEXPR:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y
    cmp #' '
    beq EQEXPR      ; loop to skip spaces

    cmp #'='
    beq EXPRDN      ; jump to evaluate expression, else fallthrough to error

EQERRO:
    brk
    dta 4
    .if foldup == 1
        dta 'MISTAKE'
    .else
        dta 'Mistake'
    .endif

STDED:
    brk                       ; also end of Mistake
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
DOBRK:
    .ifdef TARGET_ATOM
        lda ESCFLG
        and #$20
        beq DOBRK     ; Loop until Escape not pressed
    .endif

    .ifdef TARGET_SYSTEM
        cmp ESCFLG
        beq DOBRK     ; Loop until key no longer pressed
    .endif

    BRK               ; doubles as end of Syntax error on TARGET_BBC
    dta 17
    .if foldup == 1
        dta 'ESCAPE'
    .else
        dta 'Escape'
    .endif
    brk

EQEAT:
    jsr AESPAC      ; skip spaces and get next character
    cmp #'='
    bne EQERRO      ; error if not '='
    rts

EXPRDN:
    jsr EXPR        ; evaluate the expression

FDONE:
    txa
    ldy zpAECUR
    jmp DONET       ; check for end of statement, tail call

AEDONE:
    ldy zpAECUR
    jmp DONE_WITH_Y

; ----------------------------------------------------------------------------

; Check for end of statement, check for Escape
; ============================================
DONE:
    ldy zpCURSOR       ; Get program pointer offset

DONE_WITH_Y:
    dey                ; Step back to previous character

BLINK:
    iny
    lda (zpLINE),Y     ; Get next character
    cmp #' '
    beq BLINK          ; Skip spaces, and get next character

DONET:
    cmp #':'
    beq CLYADP         ; Colon, jump to update program pointer

    cmp #$0D
    beq CLYADP         ; <cr>, jump to update program pointer

    cmp #tknELSE
    bne STDED          ; Not 'ELSE', jump to 'Syntax error'

; Update program pointer

CLYADP:
    clc
    tya
    adc zpLINE
    sta zpLINE         ; Update program pointer in zpLINE
    bcc SECUR

    inc zpLINE+1

SECUR:
    ldy #$01
    sty zpCURSOR

; Check background Escape state

TSTBRK:

; Atom - check keyboard matrix
; ----------------------------
    .ifdef TARGET_ATOM
        pha           ; Save A
        lda ESCFLG
        and #$20      ; Check keyboard matrix
        beq DOBRK     ; Escape key pressed, jump to error
        pla           ; Restore A
    .endif

; System - check current keypress
; -------------------------------
    .ifdef TARGET_SYSTEM
        bit ESCFLG
        bmi SECEND    ; Nothing pressed
        pha
        lda ESCFLG    ; Save A, get keypress
        cmp #$1B
        beq DOBRK     ; If Escape, jump to error
        pla           ; Restore A
    .endif

; BBC - check background Escape state
; -----------------------------------
    .ifdef MOS_BBC
        bit ESCFLG
        bmi DOBRK     ; If Escape set, jump to give error
    .endif

SECEND:
    rts

; ----------------------------------------------------------------------------

; Move to the next statement
;
; This routine advances zpLINE to point to the start of the next statement,
; moving to the next line if necessary.

FORR:
    jsr DONE        ; check for end of statement
    dey
    lda (zpLINE),Y  ; get character that caused end of statement
    cmp #':'
    beq SECEND      ; exit if it was a colon

    lda zpLINE+1
    cmp #>BUFFER
    beq LEAVER      ; leave if statement came from BUFFER (immediate mode)

LINO:
    iny
    lda (zpLINE),Y
    bmi LEAVER      ; leave if end of program is reached

    lda zpTRFLAG
    beq NOTR        ; jump if TRACE is turned off

    tya             ; save current offset on the stack
    pha

    iny
    lda (zpLINE),Y  ; get LSB of line number and save on stack
    pha

    dey
    lda (zpLINE),Y  ; get MSB of line number
    tay             ; into Y

    pla             ; retrieve LSB in A again

    jsr AYACC       ; put YA in IACC
    jsr TRJOBA      ; print TRACE info if required

    pla             ; restore offset from the stack
    tay

NOTR:
    iny
    sec             ; C=1, add one more
    tya
    adc zpLINE
    sta zpLINE      ; save addjusted LINE
    bcc LINOIN

    inc zpLINE+1

LINOIN:
    ldy #$01        ; set offset to 1
    sty zpCURSOR

NOTRDE:
    rts

LEAVER:
    jmp CLRSTK      ; jump to immediate mode

; ----------------------------------------------------------------------------

; IF numeric
; ==========
;
; First evaluate expression. If the result is zero, the remainder of the line
; is scanned for ELSE. If it is found, the statements following are executed.
; If noy, control passes to the next line.
; If the expression is non-zero, the statements after the word THEN are
; executed.
; The complicating factors are the possible ommission of the word THEN and the
; possible presence of a line number after either THEN or ELSE, as in:
;   IF A=2 THEN 320

IFE:
    jmp LETM        ; 'Type mismatch' error

IF:
    jsr AEEXPR      ; evaluate the expression
    beq IFE         ; Type mismatch if expression is a string
    bpl IFX         ; skip conversion to int if it's already an int

    jsr IFIX        ; convert FACC to IACC

IFX:
    ldy zpAECUR
    sty zpCURSOR    ; update cursor past Arithmetic Expression

    lda zpIACC      ; check if IACC is zero
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq ELSE        ; if so, jump to scan for ELSE

    cpx #tknTHEN
    beq THEN        ; handle THEN token

THENST:
    jmp STMT        ; start executing statement(s)

THEN:
    inc zpCURSOR    ; increment past THEN token

THENLN:
    jsr SPTSTN      ; check for line number token
    bcc THENST      ; jump to start executing statements if not a line number

    jsr GOTGO       ; check that line number exists
    jsr SECUR       ; make zpLINE point directly after the line number
    jmp GODONE      ; join GOTO code

ELSE:
    ldy zpCURSOR    ; try to find else clause

ELSELP:
    lda (zpLINE),Y
    cmp #$0D
    beq ENDED       ; exit if end of line

    iny             ; increment to next character
    cmp #tknELSE
    bne ELSELP      ; loop until ELSE or EOL is found

    sty zpCURSOR    ; save cursor position
    beq THENLN      ; branch always (Z by cmp #tknELSE), join THEN code

ENDED:
    jmp ENDEDL      ; jump to skip everything after ELSE

; TRACE output if required

TRJOBA:
    lda zpIACC
    cmp zpTRNUM
    lda zpIACC+1
    sbc zpTRNUM+1
    bcs NOTRDE      ; return if current line number is greater than limit

    lda #'['
    jsr CHOUT
    jsr POSITE      ; print the line number
    lda #']'
    jsr CHOUT
    jmp LISTPT

; ----------------------------------------------------------------------------

; Print IACC as a 16-bit decimal number
; =====================================

POSITE:
    lda #$00          ; No padding
    beq NPRN+2        ; skip lda #5

NPRN:
    lda #$05          ; Pad to five characters
    sta zpPRINTS
    ldx #$04

NUMLOP:
    lda #$00
    sta zpWORK+8,X   ; zero current digit
    sec

NUMLP:
    lda zpIACC
    sbc VALL,X       ; Subtract 10s low byte
    tay
    lda zpIACC+1
    sbc VALM,X       ; Subtract 10s high byte
    bcc OUTNUM       ; Result<0, no more for this digit

    sta zpIACC+1
    sty zpIACC       ; Update number
    inc zpWORK+8,X
    bne NUMLP        ; branch always

OUTNUM:
    dex
    bpl NUMLOP        ; loop until all digits are done

    ldx #$05

LZB:
    dex
    beq LASTZ         ; reached end of number

    lda zpWORK+8,X    ; get current digit
    beq LZB           ; continue if it's zero

LASTZ:
    stx zpWORK      ; index of first non-zero digit, or last digit if IACC=0

    lda zpPRINTS
    beq PLUME       ; skip leading spaces if field width is 0

    sbc zpWORK      ; carry clear
    beq PLUME

    ; print required number of spaces

    .if version < 3
        tay
LISTPLLP:
        jsr LISTPT
        dey
        bne LISTPLLP
    .elseif version >= 3
        tax
        jsr LISTPL
        ldx zpWORK
    .endif

    ; print digits

PLUME:
    lda zpWORK+8,X
    ora #'0'        ; add ASCII offset
    jsr CHOUT
    dex
    bpl PLUME       ; loop until all digits are printed

    rts

; ----------------------------------------------------------------------------

; Low bytes of powers of ten
VALL:
    dta 1, 10, 100, <1000, <10000

; ----------------------------------------------------------------------------

; Line Search, find line number in IACC
; On exit, C=1 means line does not exist, C=0 if it does, and zpWORK+6/7
; points to one less than the address of the text of the line

FNDLNO:
    ldy #$00            ; LSB always zero
    sty zpWORK+6
    lda zpTXTP
    sta zpWORK+7        ; set WORK+6/7 to PAGE

SIGHT:
    ldy #$01
    lda (zpWORK+6),Y    ; get MSB of line number
    cmp zpIACC+1        ; compare against MSB being sought
    bcs LOOK            ; jump of >=

LOOKR:
    ldy #$03
    lda (zpWORK+6),Y    ; get length of current line
    adc zpWORK+6        ; add to pointer
    sta zpWORK+6
    bcc SIGHT           ; and continue search

    inc zpWORK+7        ; adjust MSB of pointer
    bcs SIGHT           ; and continue search

LOOK:
    bne PAST            ; not equal, exit with C=1

    ldy #$02
    lda (zpWORK+6),Y    ; get LSB of line number
    cmp zpIACC          ; compare against what's being sought
    bcc LOOKR           ; continue search if it being too small
    bne PAST            ; not equal, exit with C=1

    tya                 ; Y=A=2
    adc zpWORK+6        ; add 3 because C is always set here
    sta zpWORK+6        ; store
    bcc PAST

    inc zpWORK+7        ; possibly increment MSB of pointer
    clc                 ; indicate line number exists

PAST:
    ldy #$02            ; return with Y=2
    rts

; ----------------------------------------------------------------------------

ZDIVOR:
    brk
    dta $12
    .if foldup == 1
        dta 'DIVISION BY ZERO'
    .else
        dta 'Division by zero'
    .endif
    ; ending zero overlaps with VALM

; ----------------------------------------------------------------------------

; High byte of powers of ten
VALM:
    dta 0, 0, 0, >1000, >10000

; ----------------------------------------------------------------------------

; Divide top of stack by IACC
;
; This routine does integer division. It's used by both MOD and DIV.

DIVOP:              ; divide with remainder
    tay
    jsr INTEGB      ; ensure dividend is an integer

    lda zpIACC+3
    pha             ; save byte with sign on stack

    jsr ABSCOM      ; take absolute value of IACC
    jsr PHPOW       ; push dividend and get another operand

    stx zpTYPE
    tay
    jsr INTEGB      ; ensure divisor is an integer

    pla             ; get sign byte back
    sta zpWORK+1

    eor zpIACC+3    ; eor with sign of divisor
    sta zpWORK      ; and store

    jsr ABSCOM      ; take the absolute value of the divisor

    ldx #zpWORK+2
    jsr POPX        ; pop from stack into zpWORK+2 ... zpWORK+5

    sty zpWORK+6    ; clear next dword in zpWORK space
    sty zpWORK+7
    sty zpWORK+8
    sty zpWORK+9

    lda zpIACC+3    ; check divisor
    ora zpIACC
    ora zpIACC+1
    ora zpIACC+2
    beq ZDIVOR      ; Divide by 0 error

    ldy #32         ; number of iterations

DIVJUS:
    dey
    beq DIVRET      ; exit if enough iterations have taken place

    asl zpWORK+2    ; shift the dividend left
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    bpl DIVJUS      ; repeat until most significant bit is 1

DIVER:
    rol zpWORK+2    ; shift dividend left...
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    rol zpWORK+6    ; and shift bit into the accumulator
    rol zpWORK+7
    rol zpWORK+8
    rol zpWORK+9

    sec             ; subtract the divisor from IACC and stack result (LSB)
    lda zpWORK+6
    sbc zpIACC
    pha

    lda zpWORK+7    ; same for the next byte
    sbc zpIACC+1
    pha

    lda zpWORK+8    ; same for the third byte, but keep in X
    sbc zpIACC+2
    tax

    lda zpWORK+9    ; and finally the MSB
    sbc zpIACC+3
    bcc NOSUB       ; if the subtraction did not 'go', discard the result

    sta zpWORK+9    ; store it in out work Accu
    stx zpWORK+8
    pla             ; pull saved bytes from stack
    sta zpWORK+7
    pla
    sta zpWORK+6

    bcs NOSUB+2     ; skip pla, pla

NOSUB:
    pla             ; discard our two bytes saved
    pla

    dey
    bne DIVER       ; continue the loop

; Later BASICs have this check, disabled for now because it fails for
; Atom and System targets...
;    .if .hi(DIVER) != .hi(*)
;        .error "ASSERT: page crossing in DIVOP loop"
;    .endif

DIVRET:
    rts

; ----------------------------------------------------------------------------

; FCOMPS is called when a comparison is attempted where the first operand
; is an integer and the second is a floating point. On entry, the integer
; is on the stack and the floating point value is in FACC.

FCOMPS:
    stx zpTYPE

    jsr POPACC      ; pop integer from stack into IACC
    jsr PHFACC      ; push FACC

    jsr IFLT        ; convert integer to float
    jsr FTOW        ; copy to FWRK
    jsr POPSET      ; discard FP on stack, but have zpARGP point to it
    jsr FLDA        ; then unpack from (zpARGP) to FACC
    jmp FCMPA       ; jump to main floating point comparison routine

; Floating point comparison

FCOMPR:
    jsr PHFACC      ; save first operand on stack
    jsr ADDER       ; get second operand via 'level 4' parser
    stx zpTYPE
    tay
    jsr FLOATI      ; ensure it's a float
    jsr POPSET      ; discard FP on stack, but have zpARGP point to it

FCMP:
    jsr FLDW        ; load to FWRK via ARGP

; Compare FACC with FWRK

FCMPA:
    ldx zpTYPE
    ldy #$00

    lda zpFWRKS     ; isolate sign of FWRK
    and #$80
    sta zpFWRKS

    lda zpFACCS     ; isolate sign of FACC
    and #$80
    cmp zpFWRKS     ; compare signs
    bne FCMPZ       ; not equal, exit

    lda zpFWRKX     ; compare exponents
    cmp zpFACCX
    bne FCMPZZ      ; not equal, exit with proper flags

    lda zpFWRKMA    ; compare first mantissa
    cmp zpFACCMA
    bne FCMPZZ      ; not equal, exit with proper flags

    lda zpFWRKMB    ; etc...
    cmp zpFACCMB
    bne FCMPZZ

    lda zpFWRKMC
    cmp zpFACCMC
    bne FCMPZZ

    lda zpFWRKMD
    cmp zpFACCMD
    bne FCMPZZ

FCMPZ:
    rts

FCMPZZ:
    ror             ; clear top bit of A
    eor zpFWRKS     ; eor with sign
    rol             ; shift sign bit to C
    lda #$01        ; make suer Z=0
    rts

COMPRE:
    jmp LETM         ; Jump to 'Type mismatch' error

; Evaluate next expression and compare with previous
; --------------------------------------------------
; On exit:
;   A=B     Z=1
;   A<>B    Z=0
;   A>B     C=1 and Z=0
;   A<B     C=0

COMPR:
    txa

COMPRP1:
    beq STNCMP        ; Jump if current is string
    bmi FCOMPR        ; Jump if current is float

; Integer comparison

    jsr PHACC         ; Stack integer
    jsr ADDER         ; evaluate second operand expression
    tay               ; examine type
    beq COMPRE        ; Error if string
    bmi FCOMPS        ; Float, jump to compare int to float

; Compare IACC with top of stack

    lda zpIACC+3      ; flip sign bit of IACC ; this is to avoid the problem
    eor #$80          ; where a negative number will be interpreted as being
    sta zpIACC+3      ; bigger than any positive number by SBC and CMP below

    sec               ; prepare for subtraction

    ldy #$00          ; subtract byte for byte with top of AE stack
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
    eor #$80          ; flip sign of top of stack int, too
    sbc zpIACC+3
    ora zpIACC        ; check rest of result for zero
    ora zpIACC+1
    ora zpIACC+2

    php                   ; save flags
    clc
    lda #$04
    adc zpAESTKP          ; Drop integer from stack
    sta zpAESTKP
    bcc COMPRX

    inc zpAESTKP+1

COMPRX:
    plp                   ; restore flags
    rts

; Compare string with next expression
; -----------------------------------

STNCMP:
    jsr PHSTR       ; push first string to the stack
    jsr ADDER       ; evaluate next expression

    tay             ; set flags on A
    bne COMPRE      ; 'Type mismatch' if not a string

                    ; Y=A=0

    stx zpWORK      ; save the next character

    ldx zpCLEN

    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00        ; see above, Y is already zero
    .endif

    lda (zpAESTKP),Y    ; get length of first string from stack
    sta zpWORK+2        ; save in zpWORK+2
    cmp zpCLEN          ; compare both lengths
    bcs COMPRF          ; skip next instruction if second string is shorter

    tax

COMPRF:
    stx zpWORK+3        ; save length of the (shortest) string

    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00        ; Y is still 0
    .endif

COMPRG:
    cpy zpWORK+3
    beq COMPRH          ; exit if shorter string has zero length

    ; compare string in STRACC with string on top of stack

    iny
    lda (zpAESTKP),Y
    cmp STRACC-1,Y
    beq COMPRG          ; loop to next character if equal
    bne COMPRI          ; leave when not equal

COMPRH:
    lda zpWORK+2        ; compare the lengths of the strings
    cmp zpCLEN

COMPRI:
    php                 ; save the flags
    jsr POPSTX          ; discard string on top of stack
    ldx zpWORK          ; get the character back
    plp                 ; restore the flags
    rts


; EXPRESSION EVALUATOR
; ====================

; Evaluate expression at (zpLINE)
; -------------------------------

AEEXPR:
    lda zpLINE
    sta zpAELINE          ; Copy zpLINE to zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    lda zpCURSOR
    sta zpAECUR

; Evaluate expression at zpAELINE
; ---------------------------
; TOP LEVEL EVALUATOR
;
; Evaluator Level 7 - OR, EOR
; ---------------------------

; EXPR reads a rhs. If status is eq then it returned a string.
; If status is neq and plus then it returned a word.
; If status is neq and minus then it returned an fp.

EXPR:
    jsr ANDER         ; Call Evaluator Level 6 - AND
                      ; Returns A=type, value in IACC/FACC/STRACC, X=next char
EXPRQ:
    cpx #tknOR
    beq OR            ; Jump if next char is OR

    cpx #tknEOR
    beq EOR_          ; Jump if next char is EOR

    dec zpAECUR       ; Step zpAECUR back to last char
    tay
    sta zpTYPE
    rts               ; Set flags from type, store type in $27 and return

; OR numeric
; ----------

OR:
    jsr PHANDR        ; push as integer, evaluate next expression
    tay
    jsr INTEGB        ; ensure it's also an integer, if not, convert

    ldy #$03

ORLP:
    lda (zpAESTKP),Y
    ora zpIACC,Y      ; OR IACC with top of stack    ; abs,y (!)
    sta zpIACC,Y      ; abs,y (!)
    dey
    bpl ORLP          ; Store result in IACC

EXPRP:
    jsr POPINC        ; Drop integer from stack
    lda #$40          ; return type is integer
    bne EXPRQ         ; check for more OR/EOR, branch always

; EOR numeric
; -----------

EOR_:
    jsr PHANDR        ; push as integer, evaluate next expression
    tay
    jsr INTEGB        ; ensure it's also an integer, if not, convert

    ldy #$03

EORLP:
    lda (zpAESTKP),Y
    eor zpIACC,Y      ; EOR IACC with top of stack       ; abs,y (!)
    sta zpIACC,Y      ; abs,y (!)
    dey
    bpl EORLP         ; Store result in IACC
    bmi EXPRP         ; Jump to drop from stack and continue

; Stack current as integer, evaluate another Level 6
; --------------------------------------------------

PHANDR:
    tay
    jsr INTEGB        ; ensure it's an integer, else, convert to integer
    jsr PHACC         ; push to stack

; Evaluator Level 6 - AND
; -----------------------

ANDER:
    jsr RELATE         ; Call Evaluator Level 5, < <= = >= > <>

ANDERQ:
    cpx #tknAND
    beq AND
    rts               ; Return if next char not AND

; AND numeric
; -----------

AND:
    tay
    jsr INTEGB        ; ensure it's an integer, convert otherwise
    jsr PHACC         ; push onto stack
    jsr RELATE        ; Call Evaluator Level 5, < <= = >= > <>

    tay
    jsr INTEGB        ; ensure it's an integer

    ldy #$03

ANDLP:
    lda (zpAESTKP),Y
    and zpIACC,Y      ; AND IACC with top of stack   ; abs,y (!)
    sta zpIACC,Y      ; abs,y (!)
    dey
    bpl ANDLP         ; Store result in IACC

    jsr POPINC        ; Drop integer from stack
    lda #$40          ; return type is integer
    bne ANDERQ        ; jump to check for another AND

; Evaluator Level 5 - >... =... or <...
; -------------------------------------

RELATE:
    jsr ADDER         ; Call Evaluator Level 4, + -
    cpx #'>'+1
    bcs RELATX        ; Larger than '>', return

    cpx #'<'
    bcs RELTS         ; Smaller than '<', return

RELATX:
    rts

; >... =... or <...
; -----------------

RELTS:
    beq RELTLT        ; Jump with '<'
    cpx #'>'
    beq RELTGT        ; Jump with '>'
                      ; Must be '='
; = numeric
; ---------

    tax
    jsr COMPRP1
    bne FAIL          ; Jump with result=0 for not equal

PASS:
    dey               ; Decrement to $FF for equal

FAIL:
    sty zpIACC        ; Store 0/-1 in IACC
    sty zpIACC+1
    sty zpIACC+2
    sty zpIACC+3
    lda #$40          ; return type is integer
    rts

; < <= <>
; -------
RELTLT:               ; RELate Less Than
    tax
    ldy zpAECUR
    lda (zpAELINE),Y  ; Get next char from zpAELINE
    cmp #'='
    beq LTOREQ        ; Jump for <=

    cmp #'>'
    beq NEQUAL        ; Jump for <>

; Must be < numeric
; -----------------
    jsr COMPR         ; test less than, evaluate next and compare
    bcc PASS          ; Jump to return TRUE if <
    bcs FAIL          ; return FALSE if not <

; <= numeric
; ----------
LTOREQ:
    inc zpAECUR       ; step past '='
    jsr COMPR         ; evaluate next and compare
    beq PASS          ; jump if equal
    bcc PASS          ; jump if less than
    bcs FAIL          ; Jump to return FALSE otherwise

; <> numeric
; ----------
NEQUAL:
    inc zpAECUR       ; step past '>'
    jsr COMPR         ; evaluate next and compare
    bne PASS          ; jump if not equal
    beq FAIL          ; or fail

; > >=
; ----
RELTGT:               ; RELate Greater Than
    tax
    ldy zpAECUR
    lda (zpAELINE),Y  ; Get next char from zpAELINE
    cmp #'='
    beq GTOREQ        ; Jump for >=

; > numeric
; ---------
    jsr COMPR         ; test greater than, evaluate next and compare
    beq FAIL          ; fail if equal
    bcs PASS          ; pass if greater than (and not equal)
    bcc FAIL          ; fail if less than

; >= numeric
; ----------
GTOREQ:
    inc zpAECUR       ; step past '='
    jsr COMPR         ; evaluate next and compare
    bcs PASS          ; pass if greater than or equal
    bcc FAIL          ; fail if less than, branch always

; ----------------------------------------------------------------------------

STROVR:
    brk
    dta $13
    .if foldup == 1
        dta 'STRING TOO LONG'
    .else
        dta 'String too long'
    .endif
    brk

; String addition / concatenation
; -------------------------------

STNCON:
    jsr PHSTR         ; Stack string
    jsr POWER         ; call Evaluator Level 2
    tay
    bne ADDERE        ; string + number, jump to 'Type mismatch' error

    clc
    stx zpWORK
    ldy #$00
    lda (zpAESTKP),Y  ; Get stacked string length
    adc zpCLEN
    bcs STROVR        ; If added string length >255, jump to error

    tax
    pha
    ldy zpCLEN        ; Save new string length
CONLOP:
    lda STRACC-1,Y
    sta STRACC-1,X    ; Move current string up in string buffer
    dex
    dey
    bne CONLOP

    jsr POPSTR        ; Unstack string to start of string buffer

    pla
    sta zpCLEN
    ldx zpWORK        ; Set new string length
    tya               ; set type is string
    beq ADDERQ        ; jump to check for more + or -, branch always

; Evaluator Level 4, + -
; ----------------------

ADDER:
    jsr TERM          ; Call Evaluator Level 3, * / DIV MOD

ADDERQ:
    cpx #'+'
    beq PLUS          ; Jump for addition

    cpx #'-'
    beq MINUS         ; Jump for subtraction

    rts               ; Return otherwise

; + <value>
; ---------

PLUS:
    tay
    beq STNCON        ; Jump if current value is a string (concatenation)
    bmi FPLUS         ; Jump if current value is a float

; Integer addition
; ----------------

    jsr PHTERM        ; Stack current and call Evaluator Level 3
    tay
    beq ADDERE        ; If int + string, jump to 'Type mismatch' error
    bmi FPLUST        ; If int + float, jump ...

    ldy #$00
    clc
    lda (zpAESTKP),Y
    adc zpIACC         ; Add top of stack to IACC
    sta zpIACC         ; Store result in IACC
    iny
    lda (zpAESTKP),Y
    adc zpIACC+1
    sta zpIACC+1
    iny
    lda (zpAESTKP),Y
    adc zpIACC+2
    sta zpIACC+2
    iny
    lda (zpAESTKP),Y
    adc zpIACC+3

ADDERP:
    sta zpIACC+3
    clc
    lda zpAESTKP
    adc #$04
    sta zpAESTKP       ; Drop integer from stack
    lda #$40
    bcc ADDERQ         ; Set result=integer, jump to check for more + or -

    inc zpAESTKP+1
    bcs ADDERQ         ; Jump to check for more + or -

ADDERE:
    jmp LETM           ; Jump to 'Type mismatch' error

; Floating point addition
; -----------------------

FPLUS:
    jsr PHFACC         ; push FACC
    jsr TERM           ; Stack float, call Evaluator Level 3
    tay
    beq ADDERE         ; float + string, jump to 'Type mismatch' error

    stx zpTYPE
    bmi FPLUSS         ; float + float, skip conversion

    jsr IFLT           ; float + int, convert int to float

FPLUSS:
    jsr POPSET         ; Pop float from stack, point ARGP to it
    jsr FADD           ; load FWRK via ARGP, and add to FACC

FFFFA:
    ldx zpTYPE         ; Get nextchar back
    lda #$FF
    bne ADDERQ         ; Set result=float, loop to check for more + or -

; int + float
; -----------

FPLUST:
    stx zpTYPE
    jsr POPACC         ; Unstack integer to IACC
    jsr PHFACC         ; push FACC
    jsr IFLT           ; convert integer in IACC to float in FACC
    jmp FPLUSS         ; Jump to do float + <stacked float>

; - numeric
; ---------

MINUS:
    tay
    beq ADDERE         ; If current value is a string, jump to error
    bmi FMINUS         ; Jump if current value is a float

; Integer subtraction
; -------------------

    jsr PHTERM         ; Stack current and call Evaluator Level 3
    tay
    beq ADDERE         ; int + string, jump to error
    bmi FMINUT         ; int + float, jump to convert and do real subtraction

    sec
    ldy #$00
    lda (zpAESTKP),Y
    sbc zpIACC         ; Subtract IACC from top of stack
    sta zpIACC         ; Store in IACC
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
    sbc zpIACC+3
    jmp ADDERP         ; Jump to pop stack and loop for more + or -

; Floating point subtraction
; --------------------------

FMINUS:
    jsr PHFACC         ; push FACC
    jsr TERM           ; Stack float, call Evaluator Level 3

    tay
    beq ADDERE         ; float - string, jump to 'Type mismatch' error
    stx zpTYPE
    bmi FMINUR         ; float - float, skip conversion

    jsr IFLT           ; float - int, convert int to float

FMINUR:
    jsr POPSET         ; Pop float from stack and point ARGP to it
    jsr FXSUB          ; load FWRK via ARGP, and subtract it from FACC
    jmp FFFFA          ; Jump to set result and loop for more + or -

; int - float
; -----------

FMINUT:
    stx zpTYPE
    jsr POPACC         ; Unstack integer to IACC
    jsr PHFACC         ; push FACC
    jsr IFLT           ; convert integer in IACC to float in FACC
    jsr POPSET         ; Pop float from stack, point ARGP to it
    jsr FSUB           ; Subtract ARGP float from FACC float
    jmp FFFFA          ; Jump to set result and loop for more + or -

; Floating point multiplication
; -----------------------------

FTIMLF:
    jsr IFLT        ; convert IACC to FACC

FTIML:
    jsr POPACC      ; pull integer from stack
    jsr PHFACC      ; push FACC to stack
    jsr IFLT        ; convert IACC to FACC
    jmp FTIMR       ; jump to floating point multiplication

FTIMFL:
    jsr IFLT        ; convert IACC to FACC

FTIM:
    jsr PHFACC      ; push FACC to stack
    jsr POWER       ; evaluate next expression
    stx zpTYPE      ; save the next character
    tay
    jsr FLOATI      ; ensure operand is floating point number

FTIMR:
    jsr POPSET      ; pop float, leave ARGP pointing to it
    jsr FMUL        ; multiply!
    lda #$FF
    ldx zpTYPE
    jmp TERMQ       ; rejoin parser

FTIME:
    jmp LETM        ; jump to 'Type mismatch' error

; * <value>
; ---------

TIMES:
    tay
    beq FTIME         ; If current value is string, jump to error
    bmi FTIM          ; Jump if current valus ia a float

; Integer multiplication
;
; This algorithm is complicated by checks that are made for overflow before
; it starts (e.g. &100000 * &100000 would overflow in integer arithmetic)
; Thus the routine tries to sense whether multiplication would be better
; carried out using floating point arithmetic. This is done bye checking
; if either operand is outside the -&8000 to &7FFF range. If either is,
; floating point multiplication is used.

    lda zpIACC+3
    cmp zpIACC+2
    bne FTIMFL      ; do FP mul if top two bytes of operand are not equal

    tay
    beq TIMESA      ; skip if (both) byte(s) are zero

    cmp #$FF
    bne FTIMFL      ; if not $ff, do floating point multiplication

    ; we can be sure the top bytes are either $0000 or $ffff

TIMESA:
    eor zpIACC+1
    bmi FTIMFL      ; top bit differs from previous two bytes --> fp mul

    jsr PHPOW       ; save current operand and evaluate next

    stx zpTYPE
    tay
    beq FTIME       ; 'Type mismatch' error if 2nd expression is a string
    bmi FTIML       ; floating point multiplication if it's a real

    lda zpIACC+3    ; same range checks as done on first operand
    cmp zpIACC+2
    bne FTIMLF

    tay
    beq TIMESB

    cmp #$FF
    bne FTIMLF

TIMESB:
    eor zpIACC+1
    bmi FTIMLF

    lda zpIACC+3    ; save byte containing sign bit on the stack
    pha
    jsr ABSCOM      ; take absolute value of IACC

    ldx #zpWORK+2
    jsr ACCTOM      ; stash IACC in zpWORK+2 and onwards

    jsr POPACC      ; pop first operand to IACC

    pla             ; retrieve the other sign
    eor zpIACC+3    ; eor to get sign of answer
    sta zpWORK      ; store in zpWORK

    jsr ABSCOM      ; absolute value of IACC

; Mostly the same multiplication as we have seen before
; multiply IACC by zpWORK+2/3, result in zpWORK+6..9
; for speed, zpWORK+6/7 are kept in XY, and stored at the end

    ldy #$00
    ldx #$00
    sty zpWORK+8
    sty zpWORK+9

NUL:
    lsr zpWORK+3    ; shift right
    ror zpWORK+2
    bcc NAD         ; no addition if there's no carry

    clc             ; add IACC to answer, keep LSB and LSB+1 in XY
    tya
    adc zpIACC
    tay
    txa
    adc zpIACC+1
    tax

    lda zpWORK+8
    adc zpIACC+2
    sta zpWORK+8
    lda zpWORK+9
    adc zpIACC+3
    sta zpWORK+9

NAD:
    asl zpIACC      ; shift IACC left
    rol zpIACC+1
    rol zpIACC+2
    rol zpIACC+3
    lda zpWORK+2
    ora zpWORK+3
    bne NUL         ; continue until zpWORK value is zero

    sty zpWORK+6    ; finally store XY in result
    stx zpWORK+7
    lda zpWORK
    php             ; save flags / sign of result

REMIN:
    ldx #zpWORK+6   ; 'M' offset

DIVIN:
    jsr MTOACC      ; move 'M' to IACC

    plp             ; pull sign of result
    bpl TERMA       ; exit if ok.

    jsr COMPNO      ; negate the answer

TERMA:
    ldx zpTYPE      ; get back the next letter
    jmp TERMQ

; * <value>
; ---------
TIMESJ:
    jmp TIMES         ; Bounce back to multiply code

; ----------------------------------------------------------------------------

; Stack current value and continue in Evaluator Level 3
; -------------------------------------------------------
PHTERM:
    jsr PHACC

; Evaluator Level 3, * / DIV MOD
; ------------------------------
TERM:
    jsr POWER          ; Call Evaluator Level 2, ^

TERMQ:
    cpx #'*'
    beq TIMESJ         ; Jump with multiply

    cpx #'/'
    beq DIVIDE         ; Jump with divide

    cpx #tknMOD
    beq REMAIN         ; Jump with MOD

    cpx #tknDIV
    beq INTDIV         ; Jump with DIV

    rts

; / <value>
; ---------
DIVIDE:
    tay
    jsr FLOATI         ; Ensure current value is real
    jsr PHFACC         ; push FACC to stack
    jsr POWER          ; call Evaluator Level 2

    stx zpTYPE         ; save next character
    tay
    jsr FLOATI         ; Ensure current value is real
    jsr POPSET         ; pop, and have ARGP point to popped float
    jsr FXDIV          ; call divide routine

    ldx zpTYPE         ; get back character
    lda #$FF           ; indicate result is floating point
    bne TERMQ          ; loop for more * / MOD DIV

; MOD <value>
; -----------

REMAIN:
    jsr DIVOP         ; call the integer division routine

    lda zpWORK+1      ; save sign of the dividend
    php               ; on the stack

    jmp REMIN         ; join int mul code to retrieve 'M' to IACC

; DIV <value>
; -----------

INTDIV:
    jsr DIVOP         ; call the integer division routine

    rol zpWORK+2      ; multiply dividend by 2 (final operation)
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    bit zpWORK        ; save the sign
    php               ; on the stack

    ldx #zpWORK+2     ; for MTOACC
    jmp DIVIN         ; join int mul code to retrieve 'M' to IACC

; ----------------------------------------------------------------------------

; Stack current integer and evaluate another Level 2
; --------------------------------------------------
PHPOW:
    jsr PHACC         ; Stack IACC

; Evaluator Level 2, ^
; --------------------
POWER:
    jsr FACTOR         ; Call Evaluator Level 1, - + NOT function ( ) ? ! $ | "

POWERB:
    pha                 ; save the type of operand

POWERA:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y    ; Get character
    cmp #' '
    beq POWERA          ; Skip spaces, and get next character

    tax                 ; transfer next character to X
    pla                 ; retrieve type back in A
    cpx #'^'
    beq POW             ; jump if exponentiation

    rts                 ; Return if not ^

; ^ <value>
; ---------
POW:
    tay
    jsr FLOATI      ; ensure current value is a float
    jsr PHFACC      ; push FACC
    jsr FLTFAC      ; evaluate expression, make sure it's a real number

    lda zpFACCX
    cmp #$87
    bcs FPOWA       ; jump if abs(n) >= 64

    jsr FFRAC       ; set FQUAD to the integer part of FACC, and FACCC to
                    ; its fractional part
    bne FPOWE       ; jummp if fractional part != 0

                    ; exponent n is integer between -64 and +64

    jsr POPSET      ; pop float from stack, leave ARGP pointing to it
    jsr FLDA        ; unpack to FACC

    lda zpFQUAD     ; get (integer) exponent n in A
    jsr FIPOW       ; calculate FACC ^ n

    lda #$FF        ; result is floating point
    bne POWERB      ; loop to check for more ^

    ; here when exponent is not an integer

FPOWE:
    jsr STARGC      ; copy fractional part to FWSC temporary workspace

    lda zpAESTKP    ; make ARGP point to top of stack
    sta zpARGP
    lda zpAESTKP+1
    sta zpARGP+1

    jsr FLDA        ; unpack top of stack to FACC

    lda zpFQUAD
    jsr FIPOW       ; calculate FACC = FACC ^ INT(n)

FPOWC:
    jsr STARGB      ; save intermediate answer in FWSB temporary workspace

    jsr POPSET      ; pop float from stack, leave ARGP pointing to it
    jsr FLDA        ; unpack to FACC
    jsr FLOG        ; calculate FACC = LN(FACC)
    jsr ACMUL       ; calculate FACC = FACC * FWSC (FWSC = fractional part of n)
    jsr FEXP        ; calculate FACC = e ^ FACC
    jsr ARGB        ; make ARGP point to FWSB (partial answer)
    jsr FMUL        ; multiply, FACC = FACC * FWSB

    lda #$FF        ; result is floating point
    bne POWERB      ; loop to check for more ^

FPOWA:
    jsr STARGC      ; store FACC at FWSC temporary workspace
    jsr FONE        ; set FACC to 1
    bne FPOWC       ; branch always

; ----------------------------------------------------------------------------

; Convert number to hex string in STRACC
; --------------------------------------

FCONHX:
    tya
    bpl FCONHF

    jsr IFIX          ; convert floating point to to integer

FCONHF:
    ldx #$00          ; pointer into zpWORK+8
    ldy #$00          ; pointer into IACC

HEXPLP:
    lda zpIACC,Y      ; get first byte (abs,y (!))
    pha               ; save
    and #$0F          ; lower nibble
    sta zpWORK+8,X    ; save in workspace

    pla               ; retrieve original byte
    lsr               ; shift right 4 times
    lsr
    lsr
    lsr

    inx
    sta zpWORK+8,X    ; save upper nibble as digit
    inx
    iny
    cpy #$04
    bne HEXPLP        ; Loop for four bytes

HEXLZB:
    dex
    beq HEXP          ; No digits left, output a single zero

    lda zpWORK+8,X
    beq HEXLZB        ; Skip leading zeros

HEXP:
    lda zpWORK+8,X    ; traverse storage backwards
    cmp #$0A
    bcc NOTHX         ; less than 10

                      ; carry is set, so it's +7
    adc #$06          ; >= 10, convert byte to hex (A-F after '0' is added)

NOTHX:
    adc #'0'          ; to ASCII
    jsr CHTOBF        ; store in STRACC buffer
    dex
    bpl HEXP          ; loop for remaining digits

    rts

; ----------------------------------------------------------------------------

; Output nonzero real number
; --------------------------
FPRTA:
    bpl FPRTC          ; Jump forward if positive

    lda #'-'
    sta zpFACCS        ; A='-', clear sign flag
    jsr CHTOBF         ; Add '-' to string buffer

FPRTC:
    lda zpFACCX       ; Get exponent
    cmp #$81          ; get into range 1.0000 to 9.9999
    bcs FPRTD         ; If m*2^1 or larger, number>=1, jump to output it

    jsr FTENFX        ; FACC=FACC*10

    dec zpFPRTDX
    jmp FPRTC         ; Loop until number is >=1

; Convert numeric value to string
; ===============================
; On entry, FACC ($2E-$35)    = number
;           or IACC ($2A-$2D) = number
;                           Y = type
;                          @% = print format
;                     $15.b7 set if hex
; Uses,     zpWORK=format type 0/1/2=G/E/F
;           zpWORK+1=max digits
;           zpFPRTDX
; On exit,  STRACC contains string version of number
;           zpCLEN=string length
;
FCON:
    ldx VARL_AT+2     ; Get format byte, flag forcing E
    cpx #$03
    bcc FCONOK        ; If <3, ok - use it

    ldx #$00          ; If invalid, $00 for General format
FCONOK:
    stx zpWORK        ; Store format type
    lda VARL_AT+1
    beq FCONC         ; If digits=0, jump to check format

    cmp #$0A
    bcs FCONA         ; If 10+ digits, jump to use 10 digits

;  In G,E formats VARL is no. of significant figs > 0
;  In F format it is no. of decimals and can e >= 0

    bcc FCONB         ; If <10 digits, use specified number

FCONC:
    cpx #$02
    beq FCONB         ; If fixed format, use zero digits

; STR$ enters here to use general format, having set zpWORK to 0
; --------------------------------------------------------------
FCONA:
    lda #$0A          ; Otherwise, default to ten digits

FCONB:
    sta zpWORK+1
    sta zpFDIGS       ; Store digit length
    lda #$00
    sta zpCLEN        ; set initial STRACC length to 0
    sta zpFPRTDX      ; set initial exponent to 0
    bit zpPRINTF      ; check bit 7
    bmi FCONHX        ; Jump for hex conversion if bit 7 set

    tya
    bmi FCONFX        ; ensure we have float

    jsr IFLT          ; convert integer to floating point

FCONFX:
    jsr FTST      ; Get -1/0/+1 sign
    bne FPRTA     ; jump if not zero to output nonzero number

    lda zpWORK
    bne FPRTHJ    ; If not General format, output fixed or exponential zero

    lda #'0'
    jmp CHTOBF    ; Store single '0' into string buffer and return

FPRTHJ:
    jmp FPRTHH     ; Jump to output zero in fixed or exponential format

FPRTEE:
    jsr FONE        ; FACC = 1.0
    bne FPRTEP3     ; branch always

; FACC now is >=1, check that it is <10
; -------------------------------------

FPRTD:
    cmp #$84        ; exponent of 9.99
    bcc FPRTF       ; 1.0 to 7.999999 all OK
    bne FPRTE       ; exponent 85 or more
    lda zpFACCMA    ; fine check when exponent=84
    cmp #$A0
    bcc FPRTF       ; 8.0000 to 9.9999

FPRTE:
    jsr FTENFQ      ; divide FACC by 10.0

FPRTEP3:
    inc zpFPRTDX    ; indicate decimal point moved 1 position to the left
    jmp FPRTC       ; jump back to get the number >=1 again

; FACC is now between 1 and 9.999999999
; --------------------------------------

FPRTF:
    lda zpFACCMG      ; save rounding byte separately
    sta zpTYPE
    jsr STARGA        ; Copy FACC to FWSA, workspace temporary float

    lda zpFDIGS
    sta zpWORK+1      ; Get number of digits
    ldx zpWORK        ; Get print format
    cpx #$02
    bne FPRTFH        ; Not fixed format, jump to do exponent/general

    adc zpFPRTDX      ; fix up the precision
    bmi FPRTZR

    sta zpWORK+1
    cmp #$0B
    bcc FPRTFH        ; precision still reasonable

    lda #$0A          ; ten digits
    sta zpWORK+1
    lda #$00
    sta zpWORK        ; treat as G format

FPRTFH:
    jsr FCLR          ; Clear FACC

    lda #$A0
    sta zpFACCMA
    lda #$83
    sta zpFACCX       ;  5.0 --> FACC

    ldx zpWORK+1
    beq FPRTGJ        ; loop if remaining digits

FPRTGG:
    jsr FTENFQ        ; divide FACC by 10.0
    dex
    bne FPRTGG        ; continue until the digit count is zero

FPRTGJ:
    jsr ARGA          ; Point ARGP to workspace FP temp A (FWSA)
    jsr FLDW          ; Unpack (ARGP) to FWRK

    lda zpTYPE
    sta zpFWRKMG      ; restore rounding byte

    jsr FADDW1        ; Add FWRK to FACC

FPRTFF:
    lda zpFACCX
    cmp #$84
    bcs FPRTG         ; exit if exponent >= $84

    ror zpFACCMA      ; shift mantissa right (share code with end of FTENFX?)
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG
    inc zpFACCX       ; compensate by increasing exponent
    bne FPRTFF        ; continue

FPRTG:
    lda zpFACCMA
    cmp #$A0          ; see if unnormalized
    bcs FPRTEE        ; fix up if so

    lda zpWORK+1
    bne FPRTH         ; skip next section if number of digits != 0, always here

; Output zero in Exponent or Fixed format
; ---------------------------------------

FPRTHH:
    cmp #$01
    beq FPRTK       ; goto 'E' format

FPRTZR:
    jsr FCLR        ; Clear FACC

    lda #$00
    sta zpFPRTDX    ; set decimal exponent to zero
    lda zpFDIGS
    sta zpWORK+1    ; set number of digits to be printed to value in @%
    inc zpWORK+1    ; increment by one to allow for the leading zero

;  The exponent is $84, so the top digit of FACC is the first digit to print

FPRTH:
    lda #$01
    cmp zpWORK
    beq FPRTK       ; goto 'E' format

    ldy zpFPRTDX
    bmi FPRTKK      ; print leading zeroes

    cpy zpWORK+1
    bcs FPRTK       ; use scientific is <1.0 or > 10^digits

    lda #$00
    sta zpFPRTDX    ; set exponent to zero
    iny
    tya             ; location of decimal point in A
    bne FPRTK       ; use F type format

FPRTKK:
    lda zpWORK
    cmp #$02
    beq FPRTKL      ; F format case

    lda #$01
    cpy #$FF
    bne FPRTK       ; set E format

FPRTKL:
    lda #'0'
    jsr CHTOBF      ; Output '0'

    lda #'.'
    jsr CHTOBF      ; Output '.'

    lda #'0'        ; Prepare '0'
FPRTKM:
    inc zpFPRTDX    ; increment exponent
    beq FPRTKN      ; exit when it becomes zero

    jsr CHTOBF      ; Output '0'
    bne FPRTKM      ; repeat the process

FPRTKN:
    lda #$80        ; indicate decimal point is at $80, will not be printed

FPRTK:
    sta zpFPRTWN    ; save decimal point location

FPRTI:
    jsr FPRTNN      ; print next digit to buffer

    dec zpFPRTWN    ; decrement decimal point location
    bne FPRTL

    lda #'.'        ; add decimal point to buffer
    jsr CHTOBF

FPRTL:
    dec zpWORK+1
    bne FPRTI       ; loop for the required number of digits

    ldy zpWORK
    dey
    beq FPRTTX      ; print exponent part if 'E' mode is in use

    dey
    beq FPRTTYp2    ; print exponent part in 'F' mode

    ; 'G' mode, remove trailing zeros

    ldy zpCLEN

FPRTTZ:
    dey
    lda STRACC,Y
    cmp #'0'
    beq FPRTTZ      ; remove 0s

    cmp #'.'
    beq FPRTTY      ; and decimal point if needed

    iny

FPRTTY:
    sty zpCLEN      ; store string length

FPRTTYp2:
    lda zpFPRTDX
    beq FPRTX       ; return if exponent = 0

FPRTTX:
    lda #'E'
    jsr CHTOBF      ; Output 'E'

    lda zpFPRTDX
    bpl FPRTJ       ; skip next part if exponent is positive

    lda #'-'
    jsr CHTOBF      ; Output '-'

    sec
    lda #$00
    sbc zpFPRTDX    ; Negate exponent

FPRTJ:
    jsr IPRT        ; print exponent to the buffer

    lda zpWORK
    beq FPRTX       ; exit if 'G' format is in use

    lda #' '
    ldy zpFPRTDX
    bmi FPRTTW      ; skip if exponent was minus

    jsr CHTOBF      ; when positive, add space to make up for the minus sign

FPRTTW:
    cpx #$00
    bne FPRTX

    jmp CHTOBF      ; add another ' ' if exponent is a single digit, tail call

FPRTX:
    rts

; print next mantissa digit

FPRTNN:
    lda zpFACCMA    ; get top nibble
    lsr
    lsr
    lsr
    lsr
    jsr FPRTDG      ; print digit

    lda zpFACCMA
    and #$0F        ; mask out top nibble
    sta zpFACCMA    ; and store

    jmp FTENX       ; multiply by 10, tail call

; ----------------------------------------------------------------------------

; Print Accu in decimal unsigned, output of exponent as a number (0-99)

IPRT:
    ldx #$FF        ; start with most significant digit as -1
    sec

IPRTA:
    inx             ; increment digit
    sbc #$0A        ; subtract 10
    bcs IPRTA       ; if still >= 0, keep subtracting 10 and increasing X

    adc #$0A        ; add last 10 back
    pha             ; and save on stack
    txa
    beq IPRTB       ; skip printing if it's 0

    jsr FPRTDG      ; print digit

IPRTB:
    pla             ; get remainder back in A

FPRTDG:
    ora #'0'        ; add '0' to make it ASCII

; Store character in string buffer
; --------------------------------
CHTOBF:
    stx zpWORK+4    ; save X register
    ldx zpCLEN
    sta STRACC,X    ; Store character
    ldx zpWORK+4    ; restore X register
    inc zpCLEN      ; Increment string length
    rts

; ----------------------------------------------------------------------------

; READ ROUTINES

FRDDXX:
    clc             ; indicate failure
    stx zpFACCMG    ; set rounding byte to zero (always entered with X=0)
    jsr FTST        ; set sign and under/overflow to zero
    lda #$FF        ; indidcate answer is floating point
    rts

; Get decimal number from AELINE
; ------------------------------

FRDD:
    ldx #$00
    stx zpFACCMA        ; Clear FACC
    stx zpFACCMB
    stx zpFACCMC
    stx zpFACCMD
    stx zpFACCMG
    stx zpFRDDDP        ; Clear 'Decimal point' flag
    stx zpFRDDDX        ; Set exponent to zero
    cmp #'.'
    beq FRDDDD          ; Leading decimal point

    cmp #'9'+1
    bcs FRDDXX          ; Not a decimal digit, finish

    sbc #'0'-1          ; carry is clear
    bmi FRDDXX          ; Convert to binary, if not digit finish

    sta zpFACCMG        ; Store digit

FRDDC:
    iny
    lda (zpAELINE),Y  ; Get next character
    cmp #'.'
    bne FRDDD         ; Not decimal point

FRDDDD:
    lda zpFRDDDP      ; seen before?
    bne FRDDQ         ; Already got decimal point,

    inc zpFRDDDP      ; set decimal point flag, indicate period has been seen
    bne FRDDC         ; loop for next

FRDDD:
    cmp #'E'
    beq FRDDEX        ; Jump to scan exponent

    cmp #'9'+1
    bcs FRDDQ         ; Not a digit, jump to finish

    sbc #'0'-1
    bcc FRDDQ         ; Not a digit, jump to finish, end of number

    ldx zpFACCMA      ; Get mantissa top byte
    cpx #$18
    bcc FRDDE         ; If <25, still small enough to add another digit

    ldx zpFRDDDP
    bne FRDDC         ; Decimal point found, go back for next character

    inc zpFRDDDX      ; Otherwise, increment tens
    bcs FRDDC         ; go back and examine the next character

FRDDE:
    ldx zpFRDDDP
    beq FRDDF         ; No period yet

    dec zpFRDDDX      ; Decimal point found, decrement exponent

FRDDF:
    jsr FTENX         ; Multiply mantissa by 10

    adc zpFACCMG
    sta zpFACCMG      ; Add digit to mantissa low byte
    bcc FRDDC         ; No overflow

    inc zpFACCMD      ; Add carry through mantissa
    bne FRDDC

    inc zpFACCMC
    bne FRDDC

    inc zpFACCMB
    bne FRDDC

    inc zpFACCMA
    bne FRDDC         ; Loop to check next digit

; Deal with Exponent in scanned number

FRDDEX:
    jsr IRDD          ; Scan following number
    adc zpFRDDDX      ; Add to current exponent
    sta zpFRDDDX

; End of number found

FRDDQ:
    sty zpAECUR       ; Store AELINE offset
    lda zpFRDDDX
    ora zpFRDDDP      ; Check exponent and 'decimal point' flag
    beq FRINT         ; No exponent, no decimal point, return integer

    jsr FTST          ; was it zero?
    beq FRDDZZ        ; if so, exit at once

FRFP:
    lda #$A8          ; set exponent to indicate "bicemal" point
    sta zpFACCX       ; is to the right of the rounding byte
    lda #$00          ; clear sign and under/overflow bytes
    sta zpFACCXH
    sta zpFACCS
    jsr FNRM          ; normalise the number

; Now I have to MUL or DIV by power of ten given in zpFRDDDX

    lda zpFRDDDX
    bmi FRDDM         ; jump if it's negative
    beq FRDDZ         ; exit if it's zero

FRDDP:
    jsr FTENFX        ; times 10.0
    dec zpFRDDDX      ; decrement exponent
    bne FRDDP         ; keep adjusting until exponent is zero
    beq FRDDZ         ; exit, branch always

FRDDM:
    jsr FTENFQ        ; divide by 10.0
    inc zpFRDDDX      ; increment exponent
    bne FRDDM         ; until it's zero

FRDDZ:
    jsr FTIDY         ; round, check overflow

FRDDZZ:
    sec               ; indicate success
    lda #$FF          ; result is floating point
    rts

FRINT:
    lda zpFACCMB      ; make sure numbe is not too big to be an integer
    sta zpIACC+3
    and #$80
    ora zpFACCMA
    bne FRFP          ; jump if too big

    lda zpFACCMG      ; copy relevant parts of mantissa to IACC
    sta zpIACC
    lda zpFACCMD
    sta zpIACC+1
    lda zpFACCMC
    sta zpIACC+2

    lda #$40          ; indicate it's an integer
    sec               ; and success
    rts

IRDDB:
    jsr IRDDC         ; Scan following number
    eor #$FF          ; Negate it
    sec               ; return ok
    rts

; Scan exponent, allows E E+ E- followed by one or two digits
; -----------------------------------------------------------

IRDD:
    iny
    lda (zpAELINE),Y  ; Get next character
    cmp #'-'
    beq IRDDB         ; If '-', jump to scan and negate

    cmp #'+'
    bne IRDDA         ; If not '+', don't get next character

IRDDC:
    iny
    lda (zpAELINE),Y  ; Get next character

IRDDA:
    cmp #'9'+1
    bcs IRDDOW        ; Not a digit, exit with C=0 and A=0

    sbc #'0'-1
    bcc IRDDOW        ; Not a digit, exit with C=0 and A=0

    sta zpFRDDW       ; Store exponent digit
    iny
    lda (zpAELINE),Y  ; Get next character
    cmp #'9'+1
    bcs IRDDQ         ; Not a digit, exit with C=0 and A=exp

    sbc #'0'-1
    bcc IRDDQ         ; Not a digit, exit with C=0 and A=exp

    iny
    sta zpFTMPMA      ; Step past digit, store current digit
    lda zpFRDDW       ; Get current exponent
    asl
    asl               ; exp *= 4
    adc zpFRDDW       ; exp += exp (total *5)
    asl               ; exp *= 2   (total *10)
    adc zpFTMPMA      ; exp=exp*10+digit
    rts

IRDDQ:
    lda zpFRDDW
    clc
    rts               ; Get exp and return CC=Ok

IRDDOW:
    lda #$00
    clc
    rts               ; Return exp=0 and CC=Ok

; ----------------------------------------------------------------------------

; Add FWRK mantissa to FACC mantisa, result in FACC

FPLW:
    lda zpFACCMG
    adc zpFWRKMG
    sta zpFACCMG
    lda zpFACCMD
    adc zpFWRKMD
    sta zpFACCMD
    lda zpFACCMC
    adc zpFWRKMC
    sta zpFACCMC
    lda zpFACCMB
    adc zpFWRKMB
    sta zpFACCMB
    lda zpFACCMA
    adc zpFWRKMA
    sta zpFACCMA
    rts

; ----------------------------------------------------------------------------

; Multiply FACC mantissa by 10

FTENX:
    pha             ; preserve A

    ldx zpFACCMD    ; save mantissa in X and on the stack in the order we
    lda zpFACCMA    ; need to add it later
    pha
    lda zpFACCMB
    pha
    lda zpFACCMC
    pha

    lda zpFACCMG    ; shift left (m*2)
    asl
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA

    asl             ; shift left (m*2, total m*4)
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA

    adc zpFACCMG    ; add original version (m*4+m, total m*5)
    sta zpFACCMG
    txa
    adc zpFACCMD
    sta zpFACCMD
    pla
    adc zpFACCMC
    sta zpFACCMC
    pla
    adc zpFACCMB
    sta zpFACCMB
    pla
    adc zpFACCMA

    asl zpFACCMG    ; shift left (m*2, total m*10)
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol
    sta zpFACCMA

    pla             ; restore A
    rts

; ----------------------------------------------------------------------------

; FTST sets N,Z flags on FACC, tidies up if zero

FTST:
    lda zpFACCMA
    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    ora zpFACCMG
    beq FTSTZ       ; it is zero

    lda zpFACCS     ; get sign byte
    bne FTSTR       ; exit if it's not zero

    lda #$01        ; non-zero and positive
    rts

FTSTZ:
    sta zpFACCS     ; zero sign, exponent, and under/overflow
    sta zpFACCX
    sta zpFACCXH

FTSTR:
    rts

; ----------------------------------------------------------------------------

; FACC times 10.0
;
;   FX:=FX+3            \ exponent + 3 --> *8
;   FW:=FACC            \ FWRK = FACC
;   FW:=FW>>2           \ divide by 4, i.e. two times the original value
;   FACC:=FACC+FW       \ add it to FACC, result is original *10
;   IF CARRY THEN {     \ in case of overflow
;     FACC:=FACC>>1;    \ divide by 2
;     FX:=FX+1 }        \ adjust exponent to compensate

FTENFX:
    clc
    lda zpFACCX     ; add 3 to exponent, *8
    adc #$03
    sta zpFACCX
    bcc FTENFA

    inc zpFACCXH    ; increment overflow if needed

FTENFA:
    jsr FTOW        ; copy FACC to FWRK
    jsr FASRW       ; divide by 2
    jsr FASRW       ; divide by 2 (FWRK contains original *2)

FPLWF:
    jsr FPLW        ; FACC = FACC + FWRK, total is original *10

;   CY is set on carry out of FACCMA and FWRKMA

FRENRM:
    bcc FRENX       ; exit if no overflow occurred

    ror zpFACCMA    ; shift mantissa right, divide by 2
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG

    inc zpFACCX     ; increment exponent to compensate for divide
    bne FRENX

    inc zpFACCXH    ; possible exponent overflow

FRENX:
    rts

; ----------------------------------------------------------------------------

; FTOW -- Copy FACC to FWRK

FTOW:
    lda zpFACCS
    sta zpFWRKS
    lda zpFACCXH
    sta zpFWRKXH
    lda zpFACCX
    sta zpFWRKX
    lda zpFACCMA
    sta zpFWRKMA
    lda zpFACCMB
    sta zpFWRKMB
    lda zpFACCMC
    sta zpFWRKMC
    lda zpFACCMD
    sta zpFWRKMD
    lda zpFACCMG
    sta zpFWRKMG
    rts

; FTOWAS -- Copy FACC to FWRK and Arithmetic Shift Right

FTOWAS:
    jsr FTOW

; FASRW -- Mantissa Arithmetic Shift Right FWRK

FASRW:
    lsr zpFWRKMA
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD
    ror zpFWRKMG
    rts

; ----------------------------------------------------------------------------

; FTENFQ -- Divide FACC by 10.0
;
; original author notes:
;
;   FX:=FX-4; WRK:=ACC; WRK:=WRK>>1; ACC:=ACC+WRK
;   ADJUST IF CARRY
;   WRK:=ACC; WRK:=WRK>>4; ACC:=ACC+WRK
;   ADJUST IF CARRY
;   WRK:=ACC>>8; ACC:=ACC>>8
;   ADJUST IF CARRY
;   WRK:=ACC>>16; ACC:=ACC+WRK
;   ADJUST IF CARRY
;   ACC:=ACC+(ACC>>32)
;   ADJUST IF CARRY

FTENFQ:
    sec
    lda zpFACCX     ; subtract 4 from the exponent, divide by 16
    sbc #$04
    sta zpFACCX
    bcs FTENB

    dec zpFACCXH    ; adjust underflow if needed

FTENB:
    jsr FTOWAS      ; copy FACC to FWRK and divide by 2 (>>1)
    jsr FPLWF       ; FACC += FWRK, (* 0.00011)

    jsr FTOWAS      ; copy FACC to FWRK and divide by 2
    jsr FASRW       ; divide FWRK by 2
    jsr FASRW       ; divide FWRK by 2
    jsr FASRW       ; divide FWRK by 2 (total FWRK divided by 16, >>4)
    jsr FPLWF       ; FACC += FWRK, (* 0.000110011)

    lda #$00        ; FWRK = FACC DIV 256 (shifted right one byte, >>8)
    sta zpFWRKMA
    lda zpFACCMA
    sta zpFWRKMB
    lda zpFACCMB
    sta zpFWRKMC
    lda zpFACCMC
    sta zpFWRKMD
    lda zpFACCMD
    sta zpFWRKMG

    lda zpFACCMG    ; include rounding bit
    rol             ; set carry bit properly
    jsr FPLWF       ; FACC += FWRK
    lda #$00
    sta zpFWRKMA    ; later BASICs skip this, because FWRKMA is already 0
    sta zpFWRKMB

    lda zpFACCMA    ; FWRK = FACC DIV 65536 (shift right two bytes, >>16)
    sta zpFWRKMC
    lda zpFACCMB
    sta zpFWRKMD
    lda zpFACCMC
    sta zpFWRKMG

    lda zpFACCMD    ; include rounding bit
    rol
    jsr FPLWF       ; FACC += FWRK

    lda zpFACCMB    ; get rounding bit of byte 2 in carry
    rol
    lda zpFACCMA    ; get byte 1

FPLNF:
    adc zpFACCMG    ; add to mantissa
    sta zpFACCMG
    bcc FPLNY       ; jump if no carry

    inc zpFACCMD    ; ripple carry through whole mantissa
    bne FPLNY

    inc zpFACCMC
    bne FPLNY

    inc zpFACCMB
    bne FPLNY

    inc zpFACCMA
    bne FPLNY

    jmp FRENRM      ; right shift and increment exponent if still carry

FPLNY:
    rts

; ----------------------------------------------------------------------------

; Convert IACC to FACC

IFLT:
    ldx #$00        ; zero under/overflow and rounding bytes
    stx zpFACCMG
    stx zpFACCXH

    lda zpIACC+3
    bpl IFLTA       ; skip negating if it is already positive

    jsr COMPNO      ; negate it
    ldx #$FF        ; indicate floating point sign

IFLTA:
    stx zpFACCS     ; save sign

    lda zpIACC      ; copy IACC to mantissa
    sta zpFACCMD
    lda zpIACC+1
    sta zpFACCMC
    lda zpIACC+2
    sta zpFACCMB
    lda zpIACC+3
    sta zpFACCMA

    lda #$A0        ; set exponent, indicate "bicemal" point is at 32 bits
    sta zpFACCX

    jmp FNRM        ; normalise the number

; ----------------------------------------------------------------------------

; Copy A to sign, exponent, and exponent under/overflow bytes

FNRMZ:
    sta zpFACCS
    sta zpFACCX
    sta zpFACCXH

FNRMX:
    rts

; Convert A to FACC

FLTACC:
    pha             ; save A
    jsr FCLR        ; zero FACC
    pla             ; restore A

    beq FNRMX       ; done if 0.0
    bpl FLTAA       ; >0.0

    sta zpFACCS     ; indicate negative sign

    lda #$00        ; negate
    sec
    sbc zpFACCS

FLTAA:
    sta zpFACCMA    ; store in most significant byte of mantissa
    lda #$88        ; set "bicemal" point at 8 bits
    sta zpFACCX

; FNRM normalizes the FACC using 16 bit exponent, so no worry about
; exponent overflow

FNRM:
    lda zpFACCMA
    bmi FNRMX       ; exit if already normalised

    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    ora zpFACCMG
    beq FNRMZ       ; if mantissa is zero, set rest to zero and exit

    lda zpFACCX     ; rescue the exponent, A is always the current exponent

FNRMA:
    ldy zpFACCMA
    bmi FNRMX       ; exit if number is normalised
    bne FNRMC       ; if top byte is not zero, no (more) byte shifts

    ldx zpFACCMB    ; byte shift on mantissa (<<8)
    stx zpFACCMA
    ldx zpFACCMC
    stx zpFACCMB
    ldx zpFACCMD
    stx zpFACCMC
    ldx zpFACCMG
    stx zpFACCMD
    sty zpFACCMG    ; zero rounding byte

    sec             ; subtract 8 of exponent
    sbc #$08
    sta zpFACCX     ; and save
    bcs FNRMA       ; restart loop

    dec zpFACCXH    ; update underflow
    bcc FNRMA       ; restart loop, branch always

FNRMB:
    ldy zpFACCMA
    bmi FNRMX       ; exit if normalised

FNRMC:
    asl zpFACCMG    ; shift mantissa left (<<1)
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    sbc #$00        ; subtract 1 of exponent
    sta zpFACCX     ; and save
    bcs FNRMB       ; restart the loop

    dec zpFACCXH
    bcc FNRMB       ; restart the loop, branch always

; ----------------------------------------------------------------------------

; FLDW -- Load FWRK via zpARGP

FLDW:
    ldy #5-1        ; five bytes

    lda (zpARGP),Y  ; get four mantissa bytes
    sta zpFWRKMD

    dey
    lda (zpARGP),Y
    sta zpFWRKMC

    dey
    lda (zpARGP),Y
    sta zpFWRKMB

    dey
    lda (zpARGP),Y
    sta zpFWRKS

    dey
    sty zpFWRKMG    ; zero over/underflow bytes
    sty zpFWRKXH

    lda (zpARGP),Y  ; get exponent
    sta zpFWRKX

    ora zpFWRKS
    ora zpFWRKMB
    ora zpFWRKMC
    ora zpFWRKMD
    beq FLDWX       ; if zero, set top byte of mantissa to zero

    lda zpFWRKS     ; set top byte from the sign byte
    ora #$80

FLDWX:
    sta zpFWRKMA    ; store
    rts

; ----------------------------------------------------------------------------

; Store FACC to one of the workspace FP temps via ARGP

STARGB:
    lda #<FWSB
    bne FSTAP

STARGC:
    lda #<FWSC
    bne FSTAP

STARGA:
    lda #<FWSA

FSTAP:
    sta zpARGP
    lda #>FWSA          ; MSB of all FWS / FP TEMP variables
    sta zpARGP+1

FSTA:
    ldy #$00
    lda zpFACCX
    sta (zpARGP),Y

    iny
    lda zpFACCS         ; tidy up sign bit
    and #$80
    sta zpFACCS

    lda zpFACCMA
    and #$7F
    ora zpFACCS         ; set true sign bit
    sta (zpARGP),Y

    lda zpFACCMB
    iny
    sta (zpARGP),Y

    lda zpFACCMC
    iny
    sta (zpARGP),Y

    lda zpFACCMD
    iny
    sta (zpARGP),Y
    rts

; ----------------------------------------------------------------------------

LDARGA:
    jsr ARGA        ; set ARGP to point to FWSA

; Load FACC via ARGP

FLDA:
    ldy #$04
    lda (zpARGP),Y
    sta zpFACCMD

    dey
    lda (zpARGP),Y
    sta zpFACCMC

    dey
    lda (zpARGP),Y
    sta zpFACCMB

    dey
    lda (zpARGP),Y
    sta zpFACCS

    dey
    lda (zpARGP),Y
    sta zpFACCX

    sty zpFACCMG    ; zero under/overflow bytes
    sty zpFACCXH

    ora zpFACCS
    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    beq FLDAX       ; zero top byte of the mantissa if rest is zero

    lda zpFACCS     ; set top byte from sign
    ora #$80

FLDAX:
    sta zpFACCMA    ; store
    rts

; ----------------------------------------------------------------------------

; Convert floating point to integer
; =================================

IFIX:
    jsr FFIX         ; Convert floating point to integer

COPY_FACC_TO_IACC:
    lda zpFACCMA
    sta zpIACC+3     ; Copy to Integer Accumulator
    lda zpFACCMB
    sta zpIACC+2
    lda zpFACCMC
    sta zpIACC+1
    lda zpFACCMD
    sta zpIACC
    rts

; FFIX converts float to integer
; ==============================
;
; On entry, FACCX-FACCMD ($30-$34) holds a float
; On exit,  FACCX-FACCMD ($30-$34) holds integer part
; ---------------------------------------------------
; The real value is partially denormalised by repeatedly dividing the mantissa
; by 2 and incrementing the exponent to multiply the number by 2, until the
; exponent is $80, indicating that we have got to mantissa * 2^0.
; Truncates towards zero.
; The fractional part is left in FWRK.

FFIXQ:
    jsr FTOW       ; Copy FACC to FWRK
    jmp FCLR       ; Set FACC to zero and return

FFIX:
    lda zpFACCX
    bpl FFIXQ      ; Exponent<$80, number<1, jump to return 0

    jsr FCLRW      ; Set FWRK to zero
    jsr FTST       ; test FACC
    bne FFIXG      ; Always shift at least once
    beq FFIXY      ; except for 0

FFIXB:
    lda zpFACCX    ; Get exponent
    cmp #$A0
    bcs FFIXC      ; Exponent is +32, float has been denormalised to an integer

    cmp #$99
    bcs FFIXG      ; Loop to keep dividing

    adc #$08
    sta zpFACCX    ; Increment exponent by 8, allow for byte shift

    lda zpFWRKMC   ; byte shift, divide by 256 (>>8)
    sta zpFWRKMD
    lda zpFWRKMB
    sta zpFWRKMC
    lda zpFWRKMA
    sta zpFWRKMB
    lda zpFACCMD
    sta zpFWRKMA
    lda zpFACCMC
    sta zpFACCMD
    lda zpFACCMB
    sta zpFACCMC
    lda zpFACCMA
    sta zpFACCMB
    lda #$00        ; shift in zeros at the top
    sta zpFACCMA
    beq FFIXB       ; Loop to keep dividing, branch always

FFIXG:
    lsr zpFACCMA    ; bitwise right shift, divide by 2 (>>1)
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFWRKMA    ; rotate into FWRK
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD

    inc zpFACCX     ; increment exponent by 1
    bne FFIXB       ; continue and repeat the test

; Here I have overflow

FFIXV:
    jmp FOVR        ; 'Too big' error message

; ----------------------------------------------------------------------------

; Clear FWRK

FCLRW:
    lda #$00
    sta zpFWRKS
    sta zpFWRKXH
    sta zpFWRKX
    sta zpFWRKMA
    sta zpFWRKMB
    sta zpFWRKMC
    sta zpFWRKMD
    sta zpFWRKMG
    rts

; ----------------------------------------------------------------------------

FFIXC:
    bne FFIXV         ; Exponent > 32, jump to 'Too big' error

FFIXY:
    lda zpFACCS
    bpl FFIXZ         ; If positive, jump to return

FINEG:
    sec               ; Negate the mantissa to get integer
    lda #$00
    sbc zpFACCMD
    sta zpFACCMD
    lda #$00
    sbc zpFACCMC
    sta zpFACCMC
    lda #$00
    sbc zpFACCMB
    sta zpFACCMB
    lda #$00
    sbc zpFACCMA
    sta zpFACCMA

FFIXZ:
    rts

; ----------------------------------------------------------------------------

; FFRAC sets FQUAD to the integer part of FACC, and FACCC to its fractional
; part. Returns with condition code set zero if fraction is zero.
; Assumes that on input FIX(FACC) < 128.

FFRAC:
    lda zpFACCX
    bmi FFRACA      ; normal case

    lda #$00
    sta zpFQUAD
    jmp FTST        ; exit, set flags, if FACC's integer part is zero

FFRACA:
    jsr FFIX        ; fix FACC

    lda zpFACCMD    ; get least significant integer part
    sta zpFQUAD

    jsr FMWTOA      ; copy FWRK to FACC

    lda #$80        ; set exponent to $80, "bicemal" at the start of the number
    sta zpFACCX
    ldx zpFACCMA
    bpl FNEARN      ; exit if fractional part < 0.5

    eor zpFACCS
    sta zpFACCS     ; change sign of fractional part
    bpl FNEARQ      ; if it's now positive, round down

    inc zpFQUAD     ; round upwards
    jmp FNEARR      ; skip next instruction (could be BIT to save 2 bytes)

FNEARQ:
    dec zpFQUAD     ; round integral part downwards

FNEARR:
    jsr FINEG       ; Negate FACC

FNEARN:
    jmp FNRM        ; normalise and exit, tail call

; ----------------------------------------------------------------------------

; Increment FACC mantissa

FINC:
    inc zpFACCMD
    bne FNEARZ

    inc zpFACCMC
    bne FNEARZ

    inc zpFACCMB
    bne FNEARZ

    inc zpFACCMA
    beq FFIXV       ; overflow, 'Too big' error message

FNEARZ:
    rts

; ----------------------------------------------------------------------------

; Decrement FACC mantissa

FNEARP:
    jsr FINEG   ; negate
    jsr FINC    ; increment
    jmp FINEG   ; negate, tail call

; ----------------------------------------------------------------------------

; Subtraction, FACC = FACC - (ARGP)

FSUB:
    jsr FXSUB   ; FACC = (ARGP) - FACC
    jmp FNEG    ; negate, and exit, tail call

; Exchange FACC and (ARGP)

FSWOP:
    jsr FLDW        ; load FWRK from (ARGP)
    jsr FSTA        ; save FACC to (ARGP)

    ; copy FWRK sign and exponent to FACC

FWTOA:
    lda zpFWRKS
    sta zpFACCS
    lda zpFWRKXH
    sta zpFACCXH
    lda zpFWRKX
    sta zpFACCX

    ; copy FWRK mantissa to FACC

FMWTOA:
    lda zpFWRKMA
    sta zpFACCMA
    lda zpFWRKMB
    sta zpFACCMB
    lda zpFWRKMC
    sta zpFACCMC
    lda zpFWRKMD
    sta zpFACCMD
    lda zpFWRKMG
    sta zpFACCMG

FADDZ:
    rts

; ----------------------------------------------------------------------------

; FACC = (ARGP) - FACC

FXSUB:
    jsr FNEG    ; negate FACC

    ; fallthrough

; FACC = (ARGP) + FACC

FADD:
    jsr FLDW        ; load FWRK from (ARGP)
    beq FADDZ       ; A + 0.0 = A, answer is already in FACC

FADDW:
    jsr FADDW1      ; do the addition
    jmp FTIDY       ; tidy up and exit, tail call

FADDW1:
    jsr FTST        ; see if FACC is 0
    beq FWTOA       ; if so, load FACC with FWRK

    ; Here I have non-trivial add

    ldy #$00        ; Y=0 so we can zero memory locations with sty

    sec
    lda zpFACCX
    sbc zpFWRKX     ; subtract FWRK exponent from FACC exponent
    beq FADDA       ; if zero, no shifts are needed
    bcc FADDB       ; jump if X(FACC) < X(FWRK) (FACC needs shifting)

                    ; FWRK needs shifting

    cmp #$25
    bcs FADDZ       ; jump if shift too large for significance
                    ; basically, FACC + FWRK = FACC

    pha             ; save the difference
    and #$38        ; find out number of bytes to shift (bottom 3 bits ignored)
    beq FADDCA      ; jump if no bytes need to be moved

    lsr             ; divide by 8
    lsr
    lsr
    tax             ; transfer to X as loop counter

FADDCB:
    lda zpFWRKMD    ; shift whole FWRK mantissa one byte to the right (>>8)
    sta zpFWRKMG
    lda zpFWRKMC
    sta zpFWRKMD
    lda zpFWRKMB
    sta zpFWRKMC
    lda zpFWRKMA
    sta zpFWRKMB
    sty zpFWRKMA    ; zero to top byte
    dex
    bne FADDCB      ; loop X number of times

FADDCA:
    pla             ; retreive the difference
    and #$07        ; keep bottom 3 bits (0-7 bits to shift)
    beq FADDA       ; jump if zero shifts are needed

    tax             ; transer to X as loop counter again

FADDC:
    lsr zpFWRKMA    ; shift mantissa of FWRK right (>>1)
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD
    ror zpFWRKMG
    dex
    bne FADDC       ; loop for indicated number of bits
    beq FADDA       ; alligned, skip shifting FACC, branch always

; --------------------------------------

FADDB:
    sec
    lda zpFWRKX
    sbc zpFACCX     ; amount to shift FACC
    cmp #$25
    bcs FWTOA       ; jump if FACC not significant
                    ; basically FACC + FWRK = FWRK

; Now shift FACC right

    pha             ; save difference on the stack
    and #$38        ; same as for FWRK, ignore bottom 3 bits
    beq FADDDA      ; jump if zero byte shifts are to be done

    lsr             ; divide by 8
    lsr
    lsr
    tax             ; into X for loop counter

FADDDB:
    lda zpFACCMD    ; shift mantissa of FACC one byte to the right (>>8)
    sta zpFACCMG
    lda zpFACCMC
    sta zpFACCMD
    lda zpFACCMB
    sta zpFACCMC
    lda zpFACCMA
    sta zpFACCMB
    sty zpFACCMA    ; zero in top byte
    dex
    bne FADDDB      ; loop for X number of bytes

FADDDA:
    pla             ; retrieve difference from stack
    and #$07        ; check bottom 3 bits
    beq FADDAL      ; jump if no more shifts are needed

    tax             ; loop counter again

FADDD:
    lsr zpFACCMA    ; shift FACC mantissa to the right (>>1)
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG
    dex
    bne FADDD       ; loop for required number of shifts

FADDAL:
    lda zpFWRKX     ; copy FWRK exponent
    sta zpFACCX     ; to FACC exponent

FADDA:
    lda zpFACCS     ; eor the signs of the two number
    eor zpFWRKS
    bpl FADDE       ; jump if both same sign

    lda zpFACCMA    ; compare mantissas
    cmp zpFWRKMA
    bne FADDF

    lda zpFACCMB
    cmp zpFWRKMB
    bne FADDF

    lda zpFACCMC
    cmp zpFWRKMC
    bne FADDF

    lda zpFACCMD
    cmp zpFWRKMD
    bne FADDF

    lda zpFACCMG
    cmp zpFWRKMG
    bne FADDF

    jmp FCLR        ; mantissas are the same mgnitude, the result is zero, tail

FADDF:
    bcs FADDG       ; jump if abs(FACC) > abs(FWRK)

    ; calculate FAACCM = FWRKM - FACCM

    sec
    lda zpFWRKMG
    sbc zpFACCMG
    sta zpFACCMG
    lda zpFWRKMD
    sbc zpFACCMD
    sta zpFACCMD
    lda zpFWRKMC
    sbc zpFACCMC
    sta zpFACCMC
    lda zpFWRKMB
    sbc zpFACCMB
    sta zpFACCMB
    lda zpFWRKMA
    sbc zpFACCMA
    sta zpFACCMA
    lda zpFWRKS     ; use the sign of FWRK as the sign of the result
    sta zpFACCS

    jmp FNRM        ; normalise, and exit, tail call

FADDE:
    clc             ; numbers are of the same sign,
    jmp FPLWF       ; so add FWRK to FACC, and exit, tail call

    ; calculate FACCM = FACCM - FWRKM

FADDG:
    sec
    lda zpFACCMG
    sbc zpFWRKMG
    sta zpFACCMG
    lda zpFACCMD
    sbc zpFWRKMD
    sta zpFACCMD
    lda zpFACCMC
    sbc zpFWRKMC
    sta zpFACCMC
    lda zpFACCMB
    sbc zpFWRKMB
    sta zpFACCMB
    lda zpFACCMA
    sbc zpFWRKMA
    sta zpFACCMA

    jmp FNRM        ; normalise and exit, tail call

; ----------------------------------------------------------------------------

FMULZ:
    rts

; Multiply, FACC = FACC * (ARGP)

IFMUL:
    jsr FTST        ; check if FACC is zero
    beq FMULZ       ; exit if it is

    jsr FLDW        ; unpack (ARGP) to FWRK
    bne FMULA       ; jump if non-zero, so real work

    jmp FCLR        ; FWRK is zero, so clear FACC, and exit, tail call

; Multiply, FACC = FACC * FWRK, both non-zero

FMULA:
    clc
    lda zpFACCX
    adc zpFWRKX     ; add exponents
    bcc FMULB       ; skip next if no overflow occurred

    inc zpFACCXH    ; increment overflow flag

    ; Subtract $80 bias from exponent, do not check over/underflow yet
    ; in case renormalisation fixes things

    clc

FMULB:
    sbc #$7F        ; carry subtracts extra 1
    sta zpFACCX     ; save as exponent of the result
    bcs FMULC       ; skip next if no underflow occurred

    dec zpFACCXH    ; decrement over/underflow flag

; Copy FACC to FTMP, clear FACC then I can do FACC:=FWRK*FTMP
; as a fixed point operation. FTMP is mantissa only.

FMULC:
    ldx #$05
    ldy #$00        ; to preset FACC to 0.0

FMULD:
    lda zpFACCMA-1,X    ; copy mantissa of FACC
    sta zpFTMPMA-1,X    ; to FTMP
    sty zpFACCMA-1,X    ; and clear FACC
    dex
    bne FMULD

    lda zpFACCS
    eor zpFWRKS
    sta zpFACCS     ; get sign right

; Now for 1:32 do {
;   IF MSB(FTMP)=1 FACC:=FACC+FWRK
;   FTMP:=FTMP<<1
;   FWRK:=FWRK>>1 }

    ldy #$20

FMULE:
    lsr zpFWRKMA    ; shift FWRK mantissa one bit to the right
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD
    ror zpFWRKMG

                    ; FTMPG cannot affect answer

    asl zpFTMPMD    ; shift FTMP mantissa one bit to the left
    rol zpFTMPMC
    rol zpFTMPMB
    rol zpFTMPMA
    bcc FMULF       ; skip addition if carry did not 'fall out'

    clc
    jsr FPLW        ; add FWRK to FACC

FMULF:
    dey
    bne FMULE       ; loop until Y=0

    rts

; --------------------------------------

; FACC = FACC * (ARGP), and check overflow

FMUL:
    jsr IFMUL       ; do the multiplication

NRMTDY:
    jsr FNRM        ; normalise the result

FTIDY:
    lda zpFACCMG
    cmp #$80
    bcc FTRNDZ      ; rounding byte < $80, jump to round to zero
    beq FTRNDA

    lda #$FF
    jsr FPLNF       ; add 0.$ff to mantissa of FACC

    jmp FTRNDZ      ; exit via round to zero (sets rounding byte to zero)

FOVR:
    brk
    dta $14
    .if foldup == 1
        dta 'TOO BIG'
    .else
        dta 'Too big'
    .endif
    brk

FTRNDA:
    lda zpFACCMD    ; set least significant bit of the mantissa
    ora #$01
    sta zpFACCMD    ; as a partial rounding operation

FTRNDZ:
    lda #$00        ; set mantissa rounding byte to zero
    sta zpFACCMG

    lda zpFACCXH
    beq FTIDYZ      ; exit if over/underflow byte is zero
    bpl FOVR        ; 'Too big' error message if overflow

    ; otherwise, fallthrough to zero FACC

; ----------------------------------------------------------------------------

; Clear FACC

FCLR:
    lda #$00
    sta zpFACCS
    sta zpFACCXH
    sta zpFACCX
    sta zpFACCMA
    sta zpFACCMB
    sta zpFACCMC
    sta zpFACCMD
    sta zpFACCMG

FTIDYZ:
    rts

; ----------------------------------------------------------------------------

; Set FACC to 1.000

FONE:
    jsr FCLR        ; clear FACC
    ldy #$80
    sty zpFACCMA    ; set mantissa to $80000000
    iny
    sty zpFACCX     ; and exponent to $81
    tya
    rts             ; always return with !Z

; ----------------------------------------------------------------------------

; FACC = 1.0 / FACC

FRECIP:
    jsr STARGA      ; save FACC at workspace temporary FWSA, also sets ARGP
    jsr FONE        ; set FACC to 1.0
    bne FDIV        ; divide, branch always, FONE returns !Z

; ----------------------------------------------------------------------------

; FACC = (ARGP) / FACC

FXDIV:
    jsr FTST        ; test FACC
    beq FDIVZ       ; if zero, divide by zero error

    jsr FTOW        ; copy FACC to FWRK
    jsr FLDA        ; load FACC from (ARGP)
    bne FDIVA       ; if not zero, jump to divide

    rts             ; 0/x is always zero

FDIVZ:
    jmp ZDIVOR      ; Divide by zero error

; ----------------------------------------------------------------------------

; =TAN numeric
; ============

; FTAN works as FSIN(X)/FCOS(X)

TAN:
    jsr FLTFAC      ; get operand, ensure it's a floating point value
    jsr FRANGE      ; compute the quadrant of the angle

    lda zpFQUAD
    pha             ; save the quadrant on the stack

    jsr ARGD        ; set ARGP to point to FWSD
    jsr FSTA        ; store FACC at (ARGP)

    ; the FSC function is common to sine and cosine, just a quadrant apart

    inc zpFQUAD     ; increase quadrant for cosine

    jsr FSC         ; calculate cosine
    jsr ARGD        ; set ARGP to point to FWSD
    jsr FSWOP       ; swap FACC with (ARGP)

    pla             ; retrieve quadrant
    sta zpFQUAD     ; and store

    jsr FSC         ; calculate sine
    jsr ARGD        ; set ARGP to point to FWSD
    jsr FDIV        ; FACC = FACC / (ARGP)  \ sin(x)/cos(x)

    lda #$FF        ; indicate it's a floating point number
    rts

; ----------------------------------------------------------------------------

FDIV:
    jsr FTST        ; test FACC
    beq FTIDYZ      ; 0.0/anything = 0.0 (including 0.0/0.0)

    jsr FLDW        ; load FWRK from (ARGP)
    beq FDIVZ       ; if it's zero, error 'Divide by zero'

FDIVA:
    lda zpFACCS     ; eor the signs to get the sign of the result
    eor zpFWRKS
    sta zpFACCS

    sec
    lda zpFACCX
    sbc zpFWRKX     ; difference of exponents
    bcs FDIVB       ; jump if no underflow

    dec zpFACCXH    ; adjust underflow
    sec

FDIVB:
    adc #$80        ; $81 because carry is set
    sta zpFACCX     ; store as exponent of the answer
    bcc FDIVC       ; jump if no overflow

    inc zpFACCXH    ; adjust overflow flag
    clc

FDIVC:
    ldx #$20        ; loop counter

FDIVE:
    bcs FDIVH       ; skip test if previous shift set the carry flag

    lda zpFACCMA
    cmp zpFWRKMA
    bne FDIVF       ; not equal, test <

    lda zpFACCMB
    cmp zpFWRKMB
    bne FDIVF       ; not equal, test <

    lda zpFACCMC
    cmp zpFWRKMC
    bne FDIVF       ; not equal, test <

    lda zpFACCMD
    cmp zpFWRKMD

FDIVF:
    bcc FDIVG       ; skip subtraction if FACCM < FWRKM

; Calculate FACCM = FACCM - FWRKM, carry already set for sbc

FDIVH:
    lda zpFACCMD
    sbc zpFWRKMD
    sta zpFACCMD
    lda zpFACCMC
    sbc zpFWRKMC
    sta zpFACCMC
    lda zpFACCMB
    sbc zpFWRKMB
    sta zpFACCMB
    lda zpFACCMA
    sbc zpFWRKMA
    sta zpFACCMA

    sec             ; C=1, subtraction tookplace

FDIVG:
    rol zpFTMPMD    ; shift carry into FTMPM
    rol zpFTMPMC
    rol zpFTMPMB
    rol zpFTMPMA

    asl zpFACCMD    ; multiply FACM by two, shift left
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA

    dex
    bne FDIVE       ; loop as often as necessary

; Here, the same process is repeated, except the rounding byte is used

    ldx #$07        ; 7 iterations required (7 guard bits)

FDIVJ:
    bcs FDIVL       ; skip the comparison if the previous shift generated carry

    lda zpFACCMA    ; compare FACCM to FWRKM
    cmp zpFWRKMA
    bne FDIVK       ; not equal, test <

    lda zpFACCMB
    cmp zpFWRKMB
    bne FDIVK       ; not equal, test <

    lda zpFACCMC
    cmp zpFWRKMC
    bne FDIVK       ; not equal, test <

    lda zpFACCMD
    cmp zpFWRKMD
FDIVK:
    bcc FDIVM       ; skip subtraction if FACCM < FWRKM

    ; carry always set (fallthrough, or came via bcs FDIVL)

    ; calculate FACCM = FACCM - FWRKM

FDIVL:
    lda zpFACCMD
    sbc zpFWRKMD
    sta zpFACCMD
    lda zpFACCMC
    sbc zpFWRKMC
    sta zpFACCMC
    lda zpFACCMB
    sbc zpFWRKMB
    sta zpFACCMB
    lda zpFACCMA
    sbc zpFWRKMA
    sta zpFACCMA

    sec             ; subtraction took place

FDIVM:
    rol zpFACCMG    ; shift carry flag into mantissa via rounding byte
    asl zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    dex
    bne FDIVJ       ; loop for as many times as necessary

    ; since only seven iterations were performed, this instruction is
    ; needed to line the rounding byte up

    asl zpFACCMG    ; shift left once

    lda zpFTMPMD    ; transfer result from FTMPM to FACCM
    sta zpFACCMD
    lda zpFTMPMC
    sta zpFACCMC
    lda zpFTMPMB
    sta zpFACCMB
    lda zpFTMPMA
    sta zpFACCMA

    jmp NRMTDY      ; normalize, and tidy up, tail call

; ----------------------------------------------------------------------------

; Read -ve as negative

FSQRTE:
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

; Newton method: SQR(n): x[i+1] = 0.5 * (x[i] + N / x[i])

SQR:
    jsr FLTFAC      ; evaluate operand, ensure it's floating point

FSQRT:
    jsr FTST        ; set flags
    beq FSQRTZ      ; exit if zero, sqr(0) is easy
    bmi FSQRTE      ; argument is negative, jump to '-ve root'

    jsr STARGA      ; store FACC at temporary workspace FWSA

    lda zpFACCX
    lsr             ; halve the exponent of the number
    adc #$40        ; and add $40
    sta zpFACCX     ; store, new number is first approximation

    lda #$05        ; loop 5 times
    sta zpFRDDW

    jsr ARGB        ; set ARGP to point to FWSB

FSQRTA:
    jsr FSTA        ; store FACC at (ARGP) --> FWSB

    lda #<FWSA
    sta zpARGP      ; set ARGP to point to FWSA (value of n)
    jsr FXDIV       ; FACC = (ARGP) / FACC --> n / x[i]

    lda #<FWSB
    sta zpARGP      ; set ARGP to point to FWSB (x[i])
    jsr FADD        ; FACC = FACC + (ARGP) --> x[i] + n / x[i]

    dec zpFACCX     ; divide by 2 by decreasing exponent (*0.5)
    dec zpFRDDW     ; decrement counter
    bne FSQRTA      ; loop required times

FSQRTZ:
    lda #$FF        ; return type is floating point
    rts

; ----------------------------------------------------------------------------

; Point zpARGP to a workspace floating point temps
; ------------------------------------------------

ARGD:
    lda #<FWSD
    bne ARGCOM

ARGB:
    lda #<FWSB
    bne ARGCOM

ARGC:
    lda #<FWSC
    bne ARGCOM

ARGA:
    lda #<FWSA


ARGCOM:
    sta zpARGP
    lda #>FWSD          ; MSB the same for all four temp registers
    sta zpARGP+1
    rts

; ----------------------------------------------------------------------------

; FLOG sets FACC= LOG(FACC) (base e)
; works by
; (A) Check for acc <= 0.0
; (B) Strip exponent to put FACC in range 1.0 - 2.0
;     and renormalize to .707 TO 1.414
; (B2) Extra care with smallest possible exponent
; (C) Approximate log using (x-1)+(x-1)^2*cf(x-1)
;     where cf is a minimax continued fraction
; (D) Add result to exponent * LOG(2.0)
; N.B. Result can not overflow so no worry there.
; The series approximation used for LOGs is a continued
; fraction: F(X)=C(0)+X/(C(1)+X/(...

; =LN numeric
; ===========

LN:
    jsr FLTFAC  ; evaluate argument, ensure it's floating point

FLOG:
    jsr FTST    ; test and set flags
    beq FLOGA   ; LOG(0) is illegal
    bpl FLOGB   ; LOG(>0.0) is OK

    ; error when argument is negative or zero

FLOGA:
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

FLOGB:
    jsr FCLRW       ; clear FWRK

    ldy #$80        ; set FWRK to -1.0
    sty zpFWRKS
    sty zpFWRKMA
    iny
    sty zpFWRKX

    ldx zpFACCX     ; skip mantissa test if exponent of argument is zero
    beq FLOGC

    lda zpFACCMA    ; check if mantissa < $b5000000
    cmp #$B5
    bcc FLOGD

FLOGC:
    inx             ; increment exponent in X
    dey             ; decrement Y (Y=$80)

FLOGD:
    txa
    pha             ; save exponent on stack

    sty zpFACCX     ; set exponent of result
    jsr FADDW       ; calculate FACC = FACC + FWRK

    lda #<FWSD
    jsr FSTAP       ; store FACC in FWSD (workspace temporary)

    lda #<FLOGTC    ; set YA to point to FLOGTC table for continued fraction
    ldy #>FLOGTC
    jsr FCF         ; evaluate continued fraction
    jsr ARGD        ; make ARGP point to FWSD
    jsr FMUL        ; FACC = FACC * (ARGP)
    jsr FMUL        ; FACC = FACC * (ARGP)
    jsr FADD        ; FACC = FACC + (ARGP)
    jsr STARGA      ; save partial result in FWSA

    pla             ; recover exponent byte

    sec
    sbc #$81        ; subtract bias
    jsr FLTACC      ; turn A into float in FACC

    lda #<LOGTWO
    sta zpARGP
    lda #>LOGTWO
    sta zpARGP+1

    jsr FMUL        ; calculate FACC = FACC * LOGTWO
    jsr ARGA        ; make ARGP point to FWSA
    jsr FADD        ; FACC = FACC + (ARGP)

    lda #$FF        ; result is a floating point value
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

; ----------------------------------------------------------------------------

; FCF - Evaluates a rational function of the form
;       A0 + X/(A1+X/(A2+X/ ...
;       i.e. a continued fraction.
;       It takes a table of the form:
;       <BYTE N> <AN> ... <A0>
;       where AN through A0 are floating point values.
;       Sam demands that no table cross a page!
;
; Note that the tables are in reverse order. The formula is evaluated
; from right to left.

FCF:
    sta zpCOEFP         ; store pointer to coefficients
    sty zpCOEFP+1
    jsr STARGA          ; save FACC (X value in formula) to FWSA

    ldy #$00
    lda (zpCOEFP),Y     ; length of series - always downstairs
    sta zpFRDDDP        ; store as counter

    inc zpCOEFP         ; point to next byte, start of first constant
    bne FCFA

    inc zpCOEFP+1

FCFA:
    lda zpCOEFP         ; copy COEFP to ARGP
    sta zpARGP
    lda zpCOEFP+1
    sta zpARGP+1

    jsr FLDA            ; load FACC with constant at (ARGP)

FCFLP:
    jsr ARGA            ; make ARGP point to FWSA (value of X in formula)
    jsr FXDIV           ; calculate FACC = (ARGP) / FACC    \ X / (....)

    clc
    lda zpCOEFP         ; move COEFP pointer to next floating point value
    adc #$05            ; size of float is 5 bytes
    sta zpCOEFP
    sta zpARGP
    lda zpCOEFP+1
    adc #$00
    sta zpCOEFP+1
    sta zpARGP+1

    jsr FADD            ; FACC = FACC = (ARGP)  \ add X

    dec zpFRDDDP        ; decrement counter
    bne FCFLP           ; and loop if necessary

    rts

; ----------------------------------------------------------------------------

; =ACS numeric
; ============
ACS:
    jsr ASN
    jmp PISUB

; ----------------------------------------------------------------------------

; =ASN numeric
; ============
ASN:
    jsr FLTFAC
    jsr FTST
    bpl ASNA

    lsr zpFACCS
    jsr ASNA
    jmp SETNEG

ASNA:
    jsr STARGC
    jsr SQRONE
    jsr FTST
    beq ASINAA

    jsr ARGC
    jsr FXDIV
    jmp FATAN

ASINAA:
    jsr ARGHPI
    jsr FLDA

FATANZ:
    lda #$FF
    rts

; ----------------------------------------------------------------------------

;   FATAN computes arctangent
;   Method:
;   (A) ATAN(-X) = - ATAN(X)
;   (B) IF X>1.0 use
;       ATAN(X)=PI/2 - ATAN(1/X)
;   (C0) IF X<0.0001 result is X
;     ELSE ...
;   (C1) LET Y=(X-0.5), so Y is in range -0.5 TO 0.5
;   (D) Compute series in Y so that it gives ATAN(X)/X
;   (E) Multiply by X to get result
;   (F) (Put back PI/2 and '-')

; =ATN numeric
; ============
ATN:
    jsr FLTFAC

FATAN:
    jsr FTST
    beq FATANZ
    bpl FATANA

    lsr zpFACCS     ; force +ve
    jsr FATANA      ; ATAN(-X)

SETNEG:
    lda #$80
    sta zpFACCS     ; negate at end
    rts

FATANA:
    lda zpFACCX
    cmp #$81        ; is FACC >= 1.0 ?
    bcc FATANB      ; no it isn't

    jsr FRECIP
    jsr FATANB      ; ATAN(1/X)

PISUB:
    jsr AHPIHI      ; PI/2-A
    jsr FADD
    jsr LAA4C
    jsr FADD
    jmp FNEG

FATANB:
    lda zpFACCX
    cmp #$73
    bcc FATANZ      ; very small number

    jsr STARGC      ; save arg away
    jsr FCLRW

    lda #$80
    sta zpFWRKX
    sta zpFWRKMA
    sta zpFWRKS

    jsr FADDW       ; W = -0.5

; Now FACC is in (-0.5,0.5)

    lda #<FATANC
    ldy #>FATANC
    jsr FCF         ; sum magic series
    jsr ACMUL       ; multiply by arg, exit

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
COS:
    jsr FLTFAC      ; evaluate float
    jsr FRANGE      ; reduce to <PI/2
    inc zpFQUAD     ; quadrant counter
    jmp FSC         ; common code SIN/COS

; =SIN numeric
; ============
SIN:
    jsr FLTFAC
    jsr FRANGE

FSC:
    lda zpFQUAD
    and #$02
    beq FSCA
    jsr FSCA
    jmp FNEG

FSCA:
    lsr zpFQUAD
    bcc FSCB        ; 1st or 2nd (+ve)

    jsr FSCB

SQRONE:
    jsr STARGA      ; SQR(1-FACC^2)
    jsr FMUL
    jsr FSTA
    jsr FONE
    jsr FSUB
    jmp FSQRT

FSCB:
    jsr STARGC
    jsr FMUL
    lda #<FSINC
    ldy #>FSINC
    jsr FCF         ; evaluate approximation
    jmp ACMUL       ; X*(SIN(X)/X)

; ----------------------------------------------------------------------------

; FRANGE subtracts an integral multiple of PI/2 from FACC,
; and sets FQUAD to indicate (MOD 4, at least) what the
; integer was. Note that the subtraction is done with a
; certain degree of care so that large arguments still give
; decent accuracy.

FRANGE:
    lda zpFACCX
    cmp #$98
    bcs FRNGQQ      ; arg too big

    jsr STARGA      ; save arg away
    jsr ARGHPI      ; PI/2
    jsr FLDW

    lda zpFACCS
    sta zpFWRKS
    dec zpFWRKX     ; PI/4*SGN(INPUT)

    jsr FADDW
    jsr FDIV

; Note that the above division only has to get its result about right to
; the nearest integer.

    jsr FFIX

    lda zpFACCMD
    sta zpFQUAD
    ora zpFACCMC
    ora zpFACCMB
    ora zpFACCMA
    beq FRNGD       ; FIX(A/(PI/2))=0

    lda #$A0
    sta zpFACCX
    ldy #$00
    sty zpFACCMG
    lda zpFACCMA
    sta zpFACCS
    bpl FFLOTA

    jsr FINEG

FFLOTA:
    jsr FNRM
    jsr STARGB
    jsr AHPIHI
    jsr FMUL
    jsr ARGA
    jsr FADD
    jsr FSTA
    jsr ARGB
    jsr FLDA
    jsr LAA4C
    jsr FMUL
    jsr ARGA
    jmp FADD

FRNGD:
    jmp LDARGA

; ----------------------------------------------------------------------------

FRNGQQ:
    brk
    dta $17
    .if foldup == 1
        dta 'ACCURACY LOST'
    .else
        dta 'Accuracy lost'
    .endif
    brk

; ----------------------------------------------------------------------------

AHPIHI:
    lda #<HPIHI
    .if (HPIHI & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    bne LAA4E
LAA4C:
    lda #<HPILO
LAA4E:
    sta zpARGP
    lda #>HPIHI
    sta zpARGP+1
    rts

ARGHPI:
    lda #<HALFPI
    .if (HALFPI & 0xff) == 0
        .error "BNE as BRA will not be taken!"
    .endif
    bne LAA4E         ; AA57= D0 F5       Pu

; ----------------------------------------------------------------------------

; HPIHI + HPILO = -PI/2
; Done this way for accuracy to 1.5 precision approx.

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

; ----------------------------------------------------------------------------

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

; FEXP algorithm:
; (A) If ABS(ARG) > 89.5 (Approx.) THEN GIVE UNDER/OVERFLOW.
; (B) LET P=nearest integer to ARG, and F be residue.
;   (If ABS(X)<0.5 to start with compute this quickly)
; (C) Compute EXP(P) as power of e=2.71828...
; (D) Note ABS(F)<=0.5, compute EXP(F) by continued fraction
; (E) Combine partial results

; = EXP numeric
; =============
EXP:
    jsr FLTFAC

FEXP:
    lda zpFACCX
    cmp #$87
    bcc FEXPA       ; certainly in range
    bne FEXPB       ; certainly not

    ldy zpFACCMA
    cpy #$B3
    bcc FEXPA       ; in range, at least nearly

FEXPB:
    lda zpFACCS
    bpl FEXPC       ; overflow case

    jsr FCLR

    lda #$FF
    rts

FEXPC:
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

FEXPA:
    jsr FFRAC       ; get fractional part
    jsr FEXPS       ; EXP(fraction)
    jsr STARGC      ; save it away

    lda #<FNUME
    sta zpARGP
    lda #>FNUME
    sta zpARGP+1
    jsr FLDA

    lda zpFQUAD
    jsr FIPOW       ; X**N

ACMUL:
    jsr ARGC
    jsr FMUL

    lda #$FF
    rts

FEXPS:
    lda #<FEXPCO
    ldy #>FEXPCO
    jsr FCF         ; sum continued fraction

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

FIPOW:
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
    jsr STARGA       ; STARGA
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

; ----------------------------------------------------------------------------

; =ADVAL numeric - Call OSBYTE to read buffer/device
; ==================================================
ADVAL:
    jsr INTFAC        ; Evaluate integer
    ldx zpIACC
    lda #$80          ; X=low byte, A=$80 for ADVAL

    .if .def MOS_BBC
        .if .def TARGET_C64
            jsr C64_XY_OSBYTE
        .else
            jsr OSBYTE
        .endif
    .endif
    txa
    jmp AYACC

; ----------------------------------------------------------------------------

    .if version < 3
; =POINT(numeric, numeric)
; ========================
POINT:
        jsr INEXPR
        jsr PHACC
        jsr COMEAT
        jsr BRA
        jsr INTEGB

        lda zpIACC
        pha
        lda zpIACC+1
        pha
        jsr POPACC

        pla
        sta zpIACC+3
        pla
        sta zpIACC+2

        ldx #zpIACC
        lda #$09
        jsr OSWORD

        lda zpFACCS
        bmi SGNJMPTRUE

        jmp SINSTK
    .elseif version >= 3
; =NOT
; ====
NOT:
        jsr INTFAC

        ldx #$03

NOTLOP:
        lda zpIACC,X
        eor #$FF
        sta zpIACC,X
        dex
        bpl NOTLOP

        lda #$40
        rts
    .endif

; ----------------------------------------------------------------------------

; =POS
; ====
POS:
    .if version < 3
        lda #$86
        jsr OSBYTE
        txa
        jmp SINSTK
    .elseif version >= 3
        jsr VPOS
        stx zpIACC
        rts
    .endif

; ----------------------------------------------------------------------------

; =VPOS
; =====
VPOS:
    lda #$86
    jsr OSBYTE
    tya
    jmp SINSTK          ; tail call

; ----------------------------------------------------------------------------

; =SGN numeric
; ============
    .if version < 3
SGNFLT:
        jsr FTST
        beq SGNSIN
        bpl SGNPOS
        bmi SGNJMPTRUE

SGN:
        jsr FACTOR
        beq FACTE
        bmi SGNFLT

        lda zpIACC+3
        ora zpIACC+2
        ora zpIACC+1
        ora zpIACC
        beq SGNINT

        lda zpIACC+3
        bpl SGNPOS

SGNJMPTRUE:
        jmp TRUE

SGNPOS:
        lda #$01
SGNSIN:
        jmp SINSTK

SGNINT:
        lda #$40
        rts
    .endif          ;  version < 3

; ----------------------------------------------------------------------------

; =LOG numeric
; ============
LOG:
    jsr LN
    ldy #<RPLN10
    .if version < 3
        lda #>RPLN10
    .endif
    bne CX

; ----------------------------------------------------------------------------

; =RAD numeric
; ============
RAD:
    jsr FLTFAC
    ldy #<FPIs18
    .if version < 3
        lda #>FPIs18
    .endif

CX:
    .if version >= 3
        lda #>FPIs18       ; identical to version < 3
    .endif
    sty zpARGP
    sta zpARGP+1
    jsr FMUL

    lda #$FF
    rts

; ----------------------------------------------------------------------------

; =DEG numeric
; ============
DEG:
    jsr FLTFAC
    ldy #<F180sP
    .if version >= 3
        .error  <F180sP == 0
    .endif
    .if version < 3
        lda #>F180sP
        .error >F180sP == 0
    .endif
    bne CX               ; branch always

; ----------------------------------------------------------------------------

; =PI
; ===
PI:
    jsr ASINAA
    inc zpFACCX
    tay
    rts

; ----------------------------------------------------------------------------

; =USR numeric
; ============
USR:
    jsr INTFAC          ; Evaluate integer
    jsr USER            ; Set up registers and call code at IACC
    sta zpIACC
    stx zpIACC+1        ; Store returned A,X in IACC
    sty zpIACC+2        ; Store returned Y

    php
    pla
    sta zpIACC+3        ; Store returned flags in IACC
    cld                 ; Ensure in binary mode on return

    lda #$40
    rts               ; Return INTEGER

    .if version < 3
FACTE:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =EVAL string$ - Tokenise and evaluate expression
; ================================================
EVAL:
    jsr FACTOR         ; Evaluate value
    .if version < 3
        bne FACTE
    .elseif version >= 3
        bne EVALE
    .endif

    inc zpCLEN
    ldy zpCLEN        ; Increment string length to add a <cr>
    lda #$0D
    sta STRACC-1,Y    ; Put in terminating <cr>
    jsr PHSTR         ; Stack the string
                      ; String has to be stacked as otherwise would
                      ; be overwritten by any string operations
                      ; called by Evaluator
    lda zpAELINE
    pha               ; Save PTRB
    lda zpAELINE+1
    pha
    lda zpAECUR
    pha

    ldy zpAESTKP
    ldx zpAESTKP+1    ; YX=>stackbottom (wrong way around)
    iny               ; Step over length byte
    sty zpAELINE      ; AELINE=>stacked string
    sty zpWORK        ; WORK=>stacked string
    bne EVALX

    inx               ; Inc high byte if next page

EVALX:
    stx zpAELINE+1
    stx zpWORK+1      ; AELINE and WORK high bytes
    ldy #$FF
    sty zpWORK+4
    iny
    sty zpAECUR        ; Point AELINE offset back to start

    jsr MATCEV         ; Tokenise string on stack
    jsr EXPR           ; Call expression evaluator
    jsr POPSTX         ; Drop string from stack

VALOUT:
    pla
    sta zpAECUR          ; Restore AELINE
    pla
    sta zpAELINE+1
    pla
    sta zpAELINE
    lda zpTYPE          ; Get expression return value type
    rts               ; And return

    .if version >= 3
EVALE:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =VAL numeric
; ============
VAL:
    jsr FACTOR
    .if version < 3
        bne EVALE
    .elseif version >= 3
        bne EVALE
    .endif

VALSTR:
    ldy zpCLEN
    lda #$00
    sta STRACC,Y

    lda zpAELINE
    pha
    lda zpAELINE+1
    pha
    lda zpAECUR
    pha

    lda #$00
    sta zpAECUR

    .if version < 3
        lda #<STRACC        ; of course <STRACC = 0
    .endif
    sta zpAELINE
    lda #>STRACC
    sta zpAELINE+1

    jsr AESPAC

    cmp #'-'
    beq VALMIN

    cmp #'+'
    bne VALNUB

    jsr AESPAC

VALNUB:
    dec zpAECUR
    jsr FRDD

    jmp VALTOG

VALMIN:
    jsr AESPAC

    dec zpAECUR
    jsr FRDD

    bcc VALTOG

    jsr VALCMP

VALTOG:
    sta zpTYPE
    jmp VALOUT

; =INT numeric
; ============
INT:
    jsr FACTOR

    .if version < 3
        beq EVALE
    .elseif version >= 3
        beq FACTE
    .endif

    bpl INTX

    lda zpFACCS
    php

    jsr FFIX

    plp
    bpl INTF

    lda zpFWRKMA
    ora zpFWRKMB
    ora zpFWRKMC
    ora zpFWRKMD
    beq INTF

    jsr FNEARP

INTF:
    jsr COPY_FACC_TO_IACC
    lda #$40

INTX:
    rts

    .if version < 3
EVALE:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =ASC string$
; ============
ASC:
    jsr FACTOR
    .if version < 3
        bne EVALE
    .elseif version >= 3
        bne FACTE
    .endif
    lda zpCLEN
    beq TRUE

    lda STRACC

ASCX:
    jmp SINSTK          ; tail call

; ----------------------------------------------------------------------------

; =INKEY numeric
; ==============
INKEY:
    jsr INKEA
    .if version < 3
        cpy #$00
    .elseif version >= 3
        tya
    .endif
    bne TRUE

    txa
    jmp AYACC

    .if version >= 3
FACTE:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =EOF#numeric
; ============
EOF:
    jsr CHANN
    tax
    lda #$7F
    .ifdef MOS_BBC
        jsr OSBYTE
    .endif
    txa
    .if version < 3
        beq ASCX
    .elseif version >= 3
        beq TRUTWO
    .endif

; ----------------------------------------------------------------------------

; =TRUE
; =====
TRUE:
    .if version < 3
        lda #$FF
    .elseif version >= 3
        ldx #$FF
    .endif
TRUTWO:
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
SGNINT:
    .endif
    lda #$40
    rts

; ----------------------------------------------------------------------------

    .if version >= 3
; =FALSE
; ======
FALSE:
        ldx #$00
        beq TRUTWO       ; branch always

SGNFLT:
        jsr FTST
        beq FALSE
        bpl SGNPOS
        bmi TRUE

; ----------------------------------------------------------------------------

; =SGN numeric
; ============
SGN:
        jsr FACTOR
        beq FACTE
        bmi SGNFLT

        lda zpIACC+3
        ora zpIACC+2
        ora zpIACC+1
        ora zpIACC
        beq SGNINT

        lda zpIACC+3
        bmi TRUE

SGNPOS:
        lda #$01
SGNSIN:
        jmp SINSTK

; ----------------------------------------------------------------------------

; =POINT(numeric, numeric)
; ========================
POINT:
        jsr INEXPR
        jsr PHACC
        jsr COMEAT
        jsr BRA
        jsr INTEGB

        lda zpIACC
        pha
        ldx zpIACC+1
        jsr POPACC

        stx zpIACC+3
        pla
        sta zpIACC+2
        ldx #zpIACC
        lda #$09
        jsr OSWORD

        lda zpFACCS
        bmi TRUE
        bpl SGNSIN

    .endif              ; version >= 3

; ----------------------------------------------------------------------------

    .if version < 3
; =NOT numeric
; ============
NOT:
        jsr INTFAC
        ldx #$03
NOTLOP:
        lda zpIACC,X
        eor #$FF
        sta zpIACC,X
        dex
        bpl NOTLOP

        lda #$40
        rts
    .endif

; ----------------------------------------------------------------------------

; =INSTR(string$, string$ [, numeric])
; ====================================
INSTR:
    jsr EXPR
    .if version < 3
        bne EVALE
    .elseif version >= 3
        bne FACTE
    .endif
    cpx #','
    bne INSTRE

    inc zpAECUR
    jsr PHSTR
    jsr EXPR

    .if version < 3
        bne EVALE
    .elseif version >= 3
        bne FACTE
    .endif

    lda #$01
    sta zpIACC
    inc zpAECUR
    cpx #')'
    beq INSTRG

    cpx #','
    beq INSTRH

INSTRE:
    jmp COMERR

INSTRH:
    jsr PHSTR
    jsr BRA
    jsr INTEGB
    jsr POPSTR

INSTRG:
    ldy #$00
    ldx zpIACC
    bne INSTRF

    ldx #$01
INSTRF:
    stx zpIACC
    txa
    dex
    stx zpIACC+3

    clc
    adc zpAESTKP
    sta zpWORK

    tya
    adc zpAESTKP+1
    sta zpWORK+1

    lda (zpAESTKP),Y
    sec
    sbc zpIACC+3
    bcc INSTRY

    sbc zpCLEN
    bcc INSTRY

    adc #$00
    sta zpIACC+1
    jsr POPSTX

INSTRL:
    ldy #$00
    ldx zpCLEN
    beq INSTRO

INSTRM:
    lda (zpWORK),Y
    cmp STRACC,Y
    bne INSTRN

    iny
    dex
    bne INSTRM

INSTRO:
    lda zpIACC

INSTRP:
    jmp SINSTK      ; tail call

INSTRY:
    jsr POPSTX

INSTRZ:
    lda #$00
    beq INSTRP

INSTRN:
    inc zpIACC
    dec zpIACC+1
    beq INSTRZ

    inc zpWORK
    bne INSTRL

    inc zpWORK+1
    bne INSTRL

ABSE:
    jmp LETM

; ----------------------------------------------------------------------------

; =ABS numeric
; ============
ABS:
    jsr FACTOR       ; FACTOR
    beq ABSE
    bmi FABS

ABSCOM:
    bit zpIACC+3
    bmi COMPNO
    bpl COMPDN

FABS:
    jsr FTST
    bpl FNEGX
    bmi NEGACC

FNEG:
    jsr FTST
    beq FNEGX

NEGACC:
    lda zpFACCS
    eor #$80
    sta zpFACCS

FNEGX:
    lda #$FF
    rts

; unary minus

UNMINS:
    jsr UNPLUS

VALCMP:
    beq ABSE
    bmi FNEG

COMPNO:
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

COMPDN:
    lda #$40
    rts

; ----------------------------------------------------------------------------

DATAST:
    jsr AESPAC
    cmp #'"'
    beq QSTR

    ldx #$00
DATASL:
    lda (zpAELINE),Y
    sta STRACC,X
    iny
    inx
    cmp #$0D
    beq QSTRF

    cmp #','
    bne DATASL

QSTRF:
    dey
    .if version < 3
        jmp QSTRE
    .elseif version >= 3
QSTRE:
        dex
        stx zpCLEN
        sty zpAECUR
        lda #$00
        rts
    .endif

QSTR:
    ldx #$00

QSTRP:
    iny

QSTRL:
    lda (zpAELINE),Y
    cmp #$0D
    beq QSTREG

    .if version < 3
        iny
        sta STRACC,X
    .elseif version >= 3
        sta STRACC,X
        iny
    .endif

    inx
    cmp #'"'
    bne QSTRL

    lda (zpAELINE),Y
    cmp #'"'
    beq QSTRP

    .if version < 3
QSTRE:
        dex
        stx zpCLEN
        sty zpAECUR
        lda #$00
        rts
    .elseif version >= 3
        bne QSTRE
    .endif

QSTREG:
    jmp NSTNG

; ----------------------------------------------------------------------------

; Evaluator Level 1, - + NOT function ( ) ? ! $ | "
; -------------------------------------------------
FACTOR:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y   ; Get next character
    cmp #' '
    beq FACTOR         ; Loop to skip spaces

    cmp #'-'
    beq UNMINS         ; Jump with unary minus

    cmp #'"'
    beq QSTR           ; Jump with string

    cmp #'+'
    bne DOPLUS         ; Jump with unary plus

UNPLUS:
    jsr AESPAC         ; Get current character

DOPLUS:
    cmp #tknOPENIN
    bcc TSTVAR          ; Lowest function token, test for indirections

    cmp #tknEOF+1
    bcs FACERR         ; Highest function token, jump to error

    jmp DISPATCH       ; Jump via function dispatch table

; Indirection, hex, brackets
; --------------------------
TSTVAR:
    cmp #'?'
    bcs TSTVB         ; Jump with ?numeric or higher

    cmp #'.'
    bcs TSTN          ; Jump with .numeric or higher

    cmp #'&'
    beq HEXIN         ; Jump with hex number

    cmp #'('
    beq BRA           ; Jump with brackets

TSTVB:
    dec zpAECUR
    jsr LVCONT
    beq ERRFAC        ; Jump with undefined variable or bad name

    jmp VARIND

TSTN:
    jsr FRDD
    bcc FACERR
    rts

ERRFAC:
    lda zpBYTESM      ; Check assembler option
    and #$02          ; Is 'ignore undefiened variables' set?
    bne FACERR        ; b1=1, jump to give No such variable
    bcs FACERR        ; Jump with bad variable name

    stx zpAECUR

GETPC:
    lda PC        ; Use P% for undefined variable
    ldy PC+1
    jmp AYACC     ; Jump to return 16-bit integer, tail call

; ----------------------------------------------------------------------------

FACERR:
    brk
    dta $1A
    .if foldup == 1
        dta 'NO SUCH VARIABLE'
    .else
        dta 'No such variable'
    .endif

    brk
    .if version >= 3
BKTERR = * -1               ; include previous BRK
        dta $1B
        .if foldup == 1
            dta 'MISSING )'
        .else
            dta 'Missing )'
        .endif

HEXDED:
        brk
        dta $1C
        .if foldup == 1
            dta 'BAD HEX'
        .else
            dta 'Bad HEX'
        .endif
        brk
    .endif

; ----------------------------------------------------------------------------

BRA:
    jsr EXPR
    inc zpAECUR
    cpx #')'
    bne BKTERR
    tay
    rts

    .if version < 3
BKTERR:
        brk
        dta $1B
        .if foldup == 1
            dta 'MISSING )'
        .else
            dta 'Missing )'
        .endif
        brk
    .endif

HEXIN:
    .if version < 3
        ldx #$00
        stx zpIACC
        stx zpIACC+1
        stx zpIACC+2
        stx zpIACC+3
        ldy zpAECUR
    .elseif version >= 3
        jsr FALSE
        iny
    .endif

HEXIP:
    lda (zpAELINE),Y
    cmp #'0'
    bcc HEXEND

    cmp #'9'+1
    bcc OKHEX       ; '0'-'9' ok

    sbc #$37        ; carry is set
    cmp #$0A
    bcc HEXEND

    cmp #$10
    bcs HEXEND      ; branch if > 'F'

OKHEX:
    asl
    asl
    asl
    asl
    ldx #$03

INLOOP:
    asl
    rol zpIACC
    rol zpIACC+1
    rol zpIACC+2
    rol zpIACC+3
    dex
    bpl INLOOP

    iny
    bne HEXIP

HEXEND:
    txa
    bpl HEXDED

    sty zpAECUR
    lda #$40
    rts

; ----------------------------------------------------------------------------

    .if version >= 3

; =TOP - Return top of program
; ============================
TO:
        iny
        lda (zpAELINE),Y
        cmp #'P'
        bne FACERR

        inc zpAECUR
        lda zpTOP
        ldy zpTOP+1
        bcs AYACC

; ----------------------------------------------------------------------------

; =PAGE - Read PAGE
; =================
RPAGE:
        ldy zpTXTP
        lda #$00
        beq AYACC       ; branch always

LENB:
        jmp LETM

; ----------------------------------------------------------------------------

; =LEN string$
; ============
LEN:
        jsr FACTOR
        bne LENB
        lda zpCLEN

; Return 8-bit integer
; --------------------
SINSTK:
        ldy #$00      ; Clear b8-b15, jump to return 16-bit int

; Return 16-bit integer in YA
; ---------------------------
AYACC:
        sta zpIACC
        sty zpIACC+1      ; Store YA in integer accumulator
        lda #$00
        sta zpIACC+2
        sta zpIACC+3      ; Set b16-b31 to 0
        lda #$40          ; type integer
        rts

; ----------------------------------------------------------------------------

; =COUNT - Return COUNT
; =====================
COUNT:
        lda zpTALLY
        bcc SINSTK     ; Get COUNT, jump to return 8-bit integer

; ----------------------------------------------------------------------------

; =LOMEM - Start of BASIC heap
; ============================
RLOMEM:
        lda zpLOMEM
        ldy zpLOMEM+1
        bcc AYACC     ; Get LOMEM to AY, jump to return as integer

; ----------------------------------------------------------------------------

; =HIMEM - Top of BASIC memory
; ============================
RHIMEM:
        lda zpHIMEM
        ldy zpHIMEM+1
        bcc AYACC     ; Get HIMEM to AY, jump to return as integer

; ----------------------------------------------------------------------------

; =ERL - Return error line number
; ===============================
ERL:
        ldy zpERL+1
        lda zpERL
        bcc AYACC     ; Get ERL to AY, jump to return 16-bit integer

; ----------------------------------------------------------------------------

; =ERR - Return current error number
; ==================================
ERR:
        ldy #$00
        lda (FAULT),Y
        bcc AYACC     ; Get error number, jump to return 16-bit integer

    .endif      ; version > 3

; ----------------------------------------------------------------------------

    .if version < 3
HEXDED:
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
RTIME:
    ldx #zpIACC
    ldy #$00          ; Point to integer accumulator
    lda #$01          ; Read TIME to IACC via OSWORD $01
    .ifdef MOS_BBC
        jsr OSWORD
    .endif
    lda #$40
    rts               ; Return 'integer'

; ----------------------------------------------------------------------------

    .if version < 3

; =PAGE - Read PAGE
; =================
RPAGE:
        lda #$00
        ldy zpTXTP
        jmp AYACC

JMPFACERR:
        jmp FACERR

; ----------------------------------------------------------------------------

; =FALSE
; ======
FALSE:
        lda #$00
        beq SINSTK     ; Jump to return $00 as 16-bit integer

LENB:
        jmp LETM

; ----------------------------------------------------------------------------

; =LEN string$
; ============
LEN:
        jsr FACTOR
        bne LENB
        lda zpCLEN

; Return 8-bit integer
; --------------------
SINSTK:
        ldy #$00
        beq AYACC     ; Clear b8-b15, jump to return 16-bit int

; =TOP - Return top of program
; ============================
TO:
        ldy zpAECUR
        lda (zpAELINE),Y
        cmp #'P'
        bne JMPFACERR

        inc zpAECUR
        lda zpTOP
        ldy zpTOP+1

; Return 16-bit integer in AY
; ---------------------------
AYACC:
        sta zpIACC
        sty zpIACC+1      ; Store AY in integer accumulator
        lda #$00
        sta zpIACC+2
        sta zpIACC+3      ; Set b16-b31 to 0
        lda #$40
        rts               ; Return 'integer'

; ----------------------------------------------------------------------------

; =COUNT - Return COUNT
; =====================
COUNT:
        lda zpTALLY
        jmp SINSTK     ; Get COUNT, jump to return 8-bit integer

; ----------------------------------------------------------------------------

; =LOMEM - Start of BASIC heap
; ============================
RLOMEM:
        lda zpLOMEM
        ldy zpLOMEM+1
        jmp AYACC     ; Get LOMEM to AY, jump to return as integer

; ----------------------------------------------------------------------------

; =HIMEM - Top of BASIC memory
; ============================
RHIMEM:
        lda zpHIMEM
        ldy zpHIMEM+1
        jmp AYACC     ; Get HIMEM to AY, jump to return as integer

    .endif      ; version < 3

; ----------------------------------------------------------------------------

; =RND(numeric)
; -------------
RNDB:
    inc zpAECUR
    jsr BRA
    jsr INTEGB

    lda zpIACC+3
    bmi RNDSET

    ora zpIACC+2
    ora zpIACC+1
    bne RNDBB

    lda zpIACC
    beq FRND

    cmp #$01
    beq FRNDAA

RNDBB:
    jsr IFLT
    jsr PHFACC
    jsr FRNDAA
    jsr POPSET
    jsr IFMUL
    jsr FNRM
    jsr IFIX
    jsr INCACC

    lda #$40
    rts

RNDSET:
    ldx #zpSEED
    jsr ACCTOM
    lda #$40
    sta zpSEED+4

    rts

; RND [(numeric)]
; ===============
RND:
    ldy zpAECUR
    lda (zpAELINE),Y  ; Get current character
    cmp #'('
    beq RNDB          ; Jump with RND(numeric)

    jsr FRNDAB        ; Get random number

    ldx #zpSEED

MTOACC:
    lda zp+0,X
    sta zpIACC        ; Copy number pointed to by X to IACC
    lda zp+1,X
    sta zpIACC+1
    lda zp+2,X
    sta zpIACC+2
    lda zp+3,X
    sta zpIACC+3

    lda #$40
    rts               ; Return Integer

FRNDAA:
    jsr FRNDAB

FRND:
    ldx #$00
    stx zpFACCS
    stx zpFACCXH
    stx zpFACCMG
    lda #$80
    sta zpFACCX

MTOFACC:
    lda zpSEED,X
    sta zpFACCMA,X
    inx
    cpx #$04
    bne MTOFACC

    jsr NRMTDY

    lda #$FF
    rts

; ----------------------------------------------------------------------------

FRNDAB:

    .if version >= 3
        ldy #$04      ; Rotate through four bytes, faster but bigger
RTOP:
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
        dey
        bne RTOP
        rts
    .elseif version < 3
        ldy #$20      ; Rotate through 32 bits, shorter but slower
RTOP:
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
        bne RTOP
        rts

; ----------------------------------------------------------------------------

; =ERL - Return error line number
; ===============================
ERL:
        ldy zpERL+1
        lda zpERL
        jmp AYACC     ; Get ERL to AY, jump to return 16-bit integer

; ----------------------------------------------------------------------------

; =ERR - Return current error number
; ==================================
ERR:
        ldy #$00
        lda (FAULT),Y
        jmp AYACC     ; Get error number, jump to return 16-bit integer

    .endif          ; version < 3

; ----------------------------------------------------------------------------

; INKEY
; =====
INKEA:
    jsr INTFAC         ; Evaluate <numeric>

; Atom/System - Manually implement INKEY(num)
; -------------------------------------------
INKEALP:
    .ifdef TARGET_ATOM
        jsr $FE71
        bcc INKEB     ; Key pressed
    .endif
    .ifdef TARGET_SYSTEM
        lda ESCFLG
        bpl INKEB     ; Key pressed
    .endif
    .ifdef MOS_ATOM
        lda zpIACC
        ora zpIACC+3
        beq INKEAX    ; Timeout=0
        ldy #$08      ; $0800 gives 1cs delay
WAITLP:
        dex
        bne WAITLP
        dey
        bne WAITLP     ; Wait 1cs
        lda zpIACC
        bne INKEATIMEOUT
        dec zpIACC+3   ; Decrement timeout
INKEATIMEOUT:
        dec zpIACC
        jmp INKEALP    ; Loop to keep waiting
INKEB:
    .endif
    .ifdef TARGET_ATOM
        jsr CONVKEY     ; Convert keypress
    .endif
    .ifdef TARGET_SYSTEM
        ldy ESCFLG
        bpl INKEB       ; Loop until key released
    .endif
    .ifdef MOS_ATOM
        ldy #$00
        tax
        rts             ; Y=0, X=key, return
INKEAX:
        ldy #$FF
        rts             ; Y=$FF for no keypress
    .endif
    .ifdef TARGET_ATOM
CONVKEY:
        php
        jmp $FEA4       ; Convert Atom keypress
    .endif

; BBC - Call MOS to wait for keypress
; -----------------------------------
    .ifdef MOS_BBC
        lda #$81
C64_XY_OSBYTE:
        ldx zpIACC
        ldy zpIACC+1
        jmp OSBYTE
    .endif

; ----------------------------------------------------------------------------

; =GET
; ====
GET:
    jsr OSRDCH
    jmp SINSTK

; ----------------------------------------------------------------------------

; =GET$
; =====
GETD:
    jsr OSRDCH
SINSTR:
    sta STRACC
    lda #$01
    sta zpCLEN
    lda #$00
    rts

; ----------------------------------------------------------------------------

; Note how LEFTD and RIGHTD are partly the same

; =LEFT$(string$, numeric)
; ========================
LEFTD:
    jsr EXPR
    bne LEFTE

    cpx #','
    bne MIDC

    inc zpAECUR

    jsr PHSTR
    jsr BRA
    jsr INTEGB
    jsr POPSTR

    lda zpIACC
    cmp zpCLEN
    bcs LEFTX

    sta zpCLEN

LEFTX:
    lda #$00
    rts

; ----------------------------------------------------------------------------

; =RIGHT$(string$, numeric)
; =========================
RIGHTD:
    jsr EXPR
    bne LEFTE

    cpx #','
    bne MIDC

    inc zpAECUR

    jsr PHSTR
    jsr BRA
    jsr INTEGB
    jsr POPSTR

    lda zpCLEN
    sec
    sbc zpIACC
    bcc RALL
    beq RIGHTX

    tax
    lda zpIACC
    sta zpCLEN
    beq RIGHTX

    ldy #$00

RGHLOP:
    lda STRACC,X
    sta STRACC,Y
    inx
    iny
    dec zpIACC
    bne RGHLOP

RALL:
    lda #$00

RIGHTX:
    rts

; ----------------------------------------------------------------------------

; =INKEY$ numeric
; ===============
INKED:
    jsr INKEA
    txa
    cpy #$00
    beq SINSTR

RNUL:
    lda #$00
    sta zpCLEN
    rts

LEFTE:
    jmp LETM

MIDC:
    jmp COMERR

; ----------------------------------------------------------------------------

; =MID$(string$, numeric [, numeric] )
; ====================================
MIDD:
    jsr EXPR
    bne LEFTE

    cpx #','
    bne MIDC

    jsr PHSTR

    inc zpAECUR
    jsr INEXPR

    lda zpIACC
    pha
    lda #$FF
    sta zpIACC
    inc zpAECUR
    cpx #')'
    beq MIDTWO

    cpx #','
    bne MIDC

    jsr BRA
    jsr INTEGB

MIDTWO:
    jsr POPSTR
    pla
    tay
    clc
    beq MIDLB

    sbc zpCLEN
    bcs RNUL

    dey
    tya

MIDLB:
    sta zpIACC+2
    tax
    ldy #$00
    lda zpCLEN
    sec
    sbc zpIACC+2
    cmp zpIACC
    bcs MIDLA

    sta zpIACC

MIDLA:
    lda zpIACC
    beq RNUL

MIDLP:
    lda STRACC,X
    sta STRACC,Y
    iny
    inx
    cpy zpIACC
    bne MIDLP

    sty zpCLEN
    lda #$00
    rts

; ----------------------------------------------------------------------------

; =STR$ [~] numeric
; =================
STRD:
    jsr AESPAC        ; Skip spaces, and get next character
    ldy #$FF          ; Y=$FF for decimal
    cmp #'~'
    beq STRDT

    ldy #$00
    dec zpAECUR       ; Y=$00 for hex, step past ~

STRDT:
    tya
    pha               ; Save format
    jsr FACTOR
    beq STRE          ; Evaluate, error if not number

    tay
    pla
    sta zpPRINTF      ; Get format back
    lda VARL_AT+3
    bne STRDM         ; Top byte of @%, STR$ uses @%

    sta zpWORK        ; Store 'General format'
    jsr FCONA         ; Convert using general format

    lda #$00
    rts               ; Return string

STRDM:
    jsr FCON          ; Convert using @% format
    lda #$00
    rts               ; Return string

STRE:
    jmp LETM          ; Jump to Type mismatch error

; ----------------------------------------------------------------------------

; =STRING$(numeric, string$)
; ==========================
STRND:
    jsr INEXPR
    jsr PHACC
    jsr COMEAT
    jsr BRA
    bne STRE

    jsr POPACC
    ldy zpCLEN
    beq STRNX

    lda zpIACC
    beq STRNY

    dec zpIACC
    beq STRNX

STRNL:
    ldx #$00

STRNLP:
    lda STRACC,X
    sta STRACC,Y
    inx
    iny
    beq STRNOV

    cpx zpCLEN
    bcc STRNLP

    dec zpIACC
    bne STRNL

    sty zpCLEN

STRNX:
    lda #$00
    rts

STRNY:
    sta zpCLEN
    rts

STRNOV:
    jmp STROVR

; ----------------------------------------------------------------------------

FNMISS:
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
FNDEF:
    lda zpTXTP
    sta zpLINE+1       ; Start at PAGE
    lda #$00
    sta zpLINE

FNFIND:
    ldy #$01
    lda (zpLINE),Y     ; Get line number high byte
    bmi FNMISS         ; End of program, jump to 'No such FN/PROC' error

    ldy #$03

FNFINS:
    iny
    lda (zpLINE),Y
    cmp #' '
    beq FNFINS         ; Skip past spaces

    cmp #tknDEF
    beq DEFFND         ; Found DEF at start of line

NEXLIN:
    ldy #$03
    lda (zpLINE),Y     ; Get line length
    clc
    adc zpLINE
    sta zpLINE         ; Point to next line
    bcc FNFIND

    inc zpLINE+1
    bcs FNFIND         ; Loop back to check next line

DEFFND:
    iny
    sty zpCURSOR
    jsr SPACES

    tya
    tax
    clc
    adc zpLINE
    ldy zpLINE+1
    bcc FNFDIN

    iny
    clc

FNFDIN:
    sbc #$00
    sta zpWORK+5
    tya
    sbc #$00
    sta zpWORK+6

    ldy #$00

FNFDLK:
    iny
    inx
    lda (zpWORK+5),Y
    cmp (zpWORK),Y
    bne NEXLIN

    cpy zpWORK+2
    bne FNFDLK

    iny
    lda (zpWORK+5),Y
    jsr WORDCQ
    bcs NEXLIN

    txa
    tay
    jsr CLYADP
    jsr LOOKFN

    ldx #$01
    jsr CREAX

    ldy #$00
    lda zpLINE
    sta (zpFSA),Y
    iny
    lda zpLINE+1
    sta (zpFSA),Y
    jsr FSAPY

    jmp FNGO

FNCALL:
    brk
    dta $1E
    .if foldup == 1
        dta 'BAD CALL'
    .else
        dta 'Bad call'
    .endif
    brk

; ----------------------------------------------------------------------------

; =FNname [parameters]
; ====================
FN:
    lda #tknFN

; Call subroutine
; ---------------
; A=tknFN or tknPROC
; zpLINE=>start of FN/PROC name
;
FNBODY:
    sta zpTYPE        ; Save PROC/FN token
    tsx
    txa
    clc
    adc zpAESTKP      ; Drop BASIC stack by size of 6502 stack
    jsr HIDEC         ; Store new BASIC stack pointer, check for No Room

    ldy #$00
    txa
    sta (zpAESTKP),Y  ; Store 6502 Stack Pointer on BASIC stack

FNPHLP:
    inx
    iny
    lda $0100,X
    sta (zpAESTKP),Y  ; Copy 6502 stack onto BASIC stack
    cpx #$FF
    bne FNPHLP

    txs               ; Clear 6502 stack
    lda zpTYPE
    pha               ; Push PROC/FN token
    lda zpCURSOR
    pha               ; Push line pointer offset
    lda zpLINE
    pha               ; Push zpLINE pointer
    lda zpLINE+1
    pha
    lda zpAECUR
    tax
    clc
    adc zpAELINE
    ldy zpAELINE+1
    bcc FNNOIC

    iny
    clc

FNNOIC:
    sbc #$01
    sta zpWORK
    tya
    sbc #$00
    sta zpWORK+1       ; (zpWORK)=>PROC token

    ldy #$02
    jsr WORDLP         ; Check name is valid

    cpy #$02
    beq FNCALL         ; No valid characters, jump to 'Bad call' error

    stx zpAECUR        ; Line pointer offset => after valid FN/PROC name
    dey
    sty zpWORK+2

    jsr CREAFN
    bne FNGOA         ; Look for FN/PROC name in heap, if found, jump to it

    jmp FNDEF         ; Not in heap, jump to look through program

; FN/PROC destination found
; -------------------------
FNGOA:
    ldy #$00
    lda (zpIACC),Y
    sta zpLINE          ; Set zpLINE to address from FN/PROC infoblock
    iny
    lda (zpIACC),Y
    sta zpLINE+1

FNGO:
    lda #$00
    pha
    sta zpCURSOR        ; Push 'no parameters' (?)
    jsr SPACES
    cmp #'('
    beq FNARGS

    dec zpCURSOR

DOFN:
    lda zpAECUR
    pha
    lda zpAELINE
    pha
    lda zpAELINE+1
    pha

    jsr STMT            ; type now contains type

    pla
    sta zpAELINE+1
    pla
    sta zpAELINE
    pla
    sta zpAECUR
    pla
    beq DNARGS

    sta zpWORK+8

GTARGS:
    jsr POPWRK
    jsr STORST

    dec zpWORK+8
    bne GTARGS

DNARGS:
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

FNPLLP:
    iny
    inx
    lda (zpAESTKP),Y
    sta $0100,X       ; Copy stacked 6502 stack back onto 6502 stack
    cpx #$FF
    bne FNPLLP

    tya
    adc zpAESTKP
    sta zpAESTKP      ; Adjust BASIC stack pointer
    bcc FNPL

    inc zpAESTKP+1

FNPL:
    lda zpTYPE
    rts

FNARGS:
    lda zpAECUR       ; parse arglist - first hpush AELINE
    pha
    lda zpAELINE
    pha
    lda zpAELINE+1
    pha

    jsr CRAELV
    beq ARGMAT

    lda zpAECUR
    sta zpCURSOR

    pla                 ; hpull AELINE
    sta zpAELINE+1
    pla
    sta zpAELINE
    pla
    sta zpAECUR
    pla
    tax                 ; hpull args

    lda zpIACC+2        ; hpush lvalue
    pha
    lda zpIACC+1
    pha
    lda zpIACC
    pha

    inx                 ; args=args+1
    txa
    pha                 ; hpush args

    jsr RETINF
    jsr SPACES

    cmp #','
    beq FNARGS

    cmp #')'
    bne ARGMAT

    lda #$00
    pha                 ; hpush 0
    jsr AESPAC

    cmp #'('
    bne ARGMAT

FNARGP:
    jsr EXPR
    jsr PHTYPE          ; bpush val

    lda zpTYPE
    sta zpIACC+3

    jsr PHACC           ; bpush type

    pla                 ; hpull args, inc args, hpush args
    tax
    inx
    txa
    pha

    jsr AESPAC

    cmp #','
    beq FNARGP

    cmp #')'
    bne ARGMAT

    pla
    pla
    sta zpCOEFP
    sta zpCOEFP+1
    cpx zpCOEFP
    beq FNARGZ

ARGMAT:
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

FNARGZ:
    jsr POPACC

    pla
    sta zpIACC
    pla
    sta zpIACC+1
    pla
    sta zpIACC+2

    bmi FNARGY

    lda zpIACC+3
    beq ARGMAT

    sta zpTYPE
    ldx #zpWORK
    jsr ACCTOM

    lda zpTYPE
    bpl FNARPO

    jsr POPSET
    jsr FLDA

    jmp FNAROP

FNARPO:
    jsr POPACC

FNAROP:
    jsr STORF
    jmp FNARGW

FNARGY:
    lda zpIACC+3
    bne ARGMAT

    jsr POPSTR
    jsr STSTRE

FNARGW:
    dec zpCOEFP
    bne FNARGZ
    lda zpCOEFP+1
    pha
    jmp DOFN

; ----------------------------------------------------------------------------

; Push a value onto the stack
; ---------------------------
RETINF:
    ldy zpIACC+2
    .if version < 3
        cpy #$04
        bne FNINFO
    .elseif version >= 3
        cpy #$05
        bcs FNINFO
    .endif
    ldx #zpWORK
    jsr ACCTOM

FNINFO:
    jsr VARIND

    php
    jsr PHTYPE
    plp
    beq FNSTRD
    bmi FNSTRD

    ldx #zpWORK
    jsr MTOACC

FNSTRD:
    jmp PHACC

VARIND:
    ldy zpIACC+2
    bmi FACSTR
    beq VARONE

    cpy #$05
    beq VARFP

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

VARONE:
    lda (zpIACC),Y
    jmp AYACC

VARFP:
    dey
    lda (zpIACC),Y
    sta zpFACCMD

    dey
    lda (zpIACC),Y
    sta zpFACCMC

    dey
    lda (zpIACC),Y
    sta zpFACCMB

    dey
    lda (zpIACC),Y
    sta zpFACCS

    dey
    lda (zpIACC),Y
    sta zpFACCX

    sty zpFACCMG
    sty zpFACCXH
    ora zpFACCS
    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    beq VARFPX

    lda zpFACCS
    ora #$80

VARFPX:
    sta zpFACCMA
    lda #$FF
    rts

FACSTR:
    cpy #$80
    beq FACSTT

    ldy #$03
    lda (zpIACC),Y
    sta zpCLEN
    beq STRRTS

    ldy #$01
    lda (zpIACC),Y
    sta zpWORK+1
    dey
    lda (zpIACC),Y
    sta zpWORK
    ldy zpCLEN

MOVTOS:
    dey
    lda (zpWORK),Y
    sta STRACC,Y
    tya
    bne MOVTOS

STRRTS:
    rts

FACSTT:
    lda zpIACC+1
    beq CHRF

    ldy #$00

FACSTU:
    lda (zpIACC),Y
    sta STRACC,Y
    eor #$0D
    beq FACSTV

    iny
    bne FACSTU

    tya

FACSTV:
    sty zpCLEN
    rts

; ----------------------------------------------------------------------------

; =CHR$ numeric
; =============
CHRD:
    jsr INTFAC

CHRF:
    lda zpIACC
    jmp SINSTR

; ----------------------------------------------------------------------------

FNLINO:
    ldy #$00
    sty zpERL
    sty zpERL+1
    ldx zpTXTP
    stx zpWORK+1
    sty zpWORK

    ldx zpLINE+1
    cpx #>BUFFER
    beq FNLINX

    ldx zpLINE

FIND:
    jsr GETWRK

    cmp #$0D
    bne CHKA

    cpx zpWORK
    lda zpLINE+1
    sbc zpWORK+1
    bcc FNLINX

    jsr GETWRK

    ora #$00
    bmi FNLINX

    sta zpERL+1
    jsr GETWRK

    sta zpERL
    jsr GETWRK

CHKA:
    cpx zpWORK
    lda zpLINE+1
    sbc zpWORK+1
    bcs FIND

FNLINX:
    rts

; ----------------------------------------------------------------------------

; ERROR HANDLER
; =============

BREK:

; Atom/System - Process raw BRK to get FAULT pointer
; --------------------------------------------------
    .ifdef MOS_ATOM
        pla
        cld
        cli
        pla             ; Drop flags, pop return low byte
        sec
        sbc #$01
        sta FAULT+0     ; Point to error block
        pla
        sbc #$00
        sta FAULT+1
        cmp #>START_OF_ROM
        bcc EXTERR              ; If outside BASIC, not a full error block
        cmp #[>END_OF_ROM]-1
        bcs EXTERR              ; So generate default error
    .endif


; FAULT set up, now process BRK error
; -----------------------------------
    jsr FNLINO

    sty zpTRFLAG
    lda (FAULT),Y
    bne BREKA           ; If ERR<>0, skip past ON ERROR OFF

    lda #<BASERR
    sta zpERRORLH       ; ON ERROR OFF
    lda #>BASERR
    sta zpERRORLH+1

BREKA:
    lda zpERRORLH
    sta zpLINE          ; Point program point to ERROR program
    lda zpERRORLH+1
    sta zpLINE+1
    jsr SETVAR          ; Clear DATA and stack
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
    jmp STMT          ; Jump to execution loop

    .ifdef MOS_ATOM
EXTERR:
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

; ----------------------------------------------------------------------------

; Default ERROR program
; ---------------------
; REPORT IF ERL PRINT " at line ";ERL END ELSE PRINT END
BASERR:
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

; ----------------------------------------------------------------------------

; SOUND numeric, numeric, numeric, numeric
; ========================================
BEEP:
    jsr ASEXPR        ; Evaluate integer

    ldx #$03          ; Three more to evaluate

BEEPLP:
    .ifdef MOS_BBC
        lda zpIACC
        pha
        lda zpIACC+1
        pha           ; Stack current 16-bit integer
    .endif
    txa
    pha
    jsr INCMEX        ; Step past comma, evaluate next integer

    pla
    tax
    dex
    bne BEEPLP        ; Loop to stack this one

    .ifdef MOS_BBC
        jsr AEDONE    ; Check end of statement
        lda zpIACC
        sta zpWORK+6  ; Copy current 16-bit integer to end of control block
        lda zpIACC+1
        sta zpWORK+7
        ldy #$07
        ldx #$05      ; Prepare for OSWORD 7 and 6 more bytes
        bne ENVELL    ; Jump to pop to control block and call OSWORD
    .endif

    .ifdef MOS_ATOM
        beq EVELX     ; Check end of statement and return
    .endif

; ENVELOPE a,b,c,d,e,f,g,h,i,j,k,l,m,n
; ====================================
ENVEL:
    jsr ASEXPR        ; Evaluate integer

    ldx #$0D          ; 13 more to evaluate

ENVELP:
    .ifdef MOS_BBC
        lda zpIACC
        pha            ; Stack current 8-bit integer
    .endif

    txa
    pha
    jsr INCMEX         ; Step past comma, evaluate next integer

    pla
    tax
    dex
    bne ENVELP         ; Loop to stack this one

    .ifdef MOS_ATOM
EVELX:
    .endif

    jsr AEDONE         ; Check end of statement
    .ifdef MOS_BBC
        lda zpIACC
        sta zpWORK+13  ; Copy current 8-bit integer to end of control block
        ldx #$0C
        ldy #$08       ; Prepare for 12 more bytes and OSWORD 8
    .endif

ENVELL:
    .ifdef MOS_BBC
        pla
        sta zpWORK,X   ; Pop bytes into control block
        dex
        bpl ENVELL
        tya            ; Y=OSWORD number
        ldx #zpWORK
        ldy #$00       ; YX=>control block
        jsr OSWORD
    .endif

    jmp NXT         ; Return to execution loop

; ----------------------------------------------------------------------------

; WIDTH numeric
; =============
WIDTH:
    jsr ASEXPR
    jsr AEDONE

    ldy zpIACC
    dey
    sty zpWIDTHV
    jmp NXT

; ----------------------------------------------------------------------------

STORER:
    jmp LETM

; Store byte or word integer
; ==========================
STEXPR:
    jsr EXPR          ; Evaluate expression

STORE:
    jsr POPWRK        ; Unstack integer (address of data)

STORF:
    lda zpWORK+2
    cmp #$05
    beq STORFP        ; Size=5, jump to store float

    lda zpTYPE
    beq STORER        ; Type<>num, jump to error
    bpl STORIN        ; Type=int, jump to store it

    jsr IFIX          ; Convert float to integer

STORIN:
    ldy #$00
    lda zpIACC
    sta (zpWORK),Y    ; Store byte 1
    lda zpWORK+2
    beq STORDN        ; Exit if size=0, byte

    lda zpIACC+1
    iny
    sta (zpWORK),Y    ; Store byte 2

    lda zpIACC+2
    iny
    sta (zpWORK),Y    ; Store byte 3

    lda zpIACC+3
    iny
    sta (zpWORK),Y    ; Store byte 4

STORDN:
    rts

; ----------------------------------------------------------------------------

; Store float
; ===========
STORFP:
    lda zpTYPE
    beq STORER        ; Type<>num, jump to error
    bmi STORPF        ; Type=float, jump to store it

    jsr IFLT          ; Convert integer to float

STORPF:
    ldy #$00          ; Store 5-byte float
    lda zpFACCX
    sta (zpWORK),Y

    iny               ; exponent
    lda zpFACCS
    and #$80
    sta zpFACCS       ; Unpack sign

    lda zpFACCMA
    and #$7F          ; Unpack mantissa 1
    ora zpFACCS
    sta (zpWORK),Y    ; sign + mantissa 1

    iny
    lda zpFACCMB
    sta (zpWORK),Y    ; mantissa 2

    iny
    lda zpFACCMC
    sta (zpWORK),Y    ; mantissa 3

    iny
    lda zpFACCMD
    sta (zpWORK),Y    ; mantissa 4
    rts

; ----------------------------------------------------------------------------

TOKOUT:
    sta zpWORK
    cmp #$80
    bcc CHOUT

    lda #<TOKENS
    sta zpWORK+1        ; Point to token table
    lda #>TOKENS
    sta zpWORK+2
    sty zpWORK+3        ; save Y

FINTOK:
    ldy #$00

LOOTOK:
    iny
    lda (zpWORK+1),Y
    bpl LOOTOK

    cmp zpWORK
    beq GOTTOK

    iny
    tya
    sec
    adc zpWORK+1
    sta zpWORK+1
    bcc FINTOK

    inc zpWORK+2
    bcs FINTOK

GOTTOK:
    ldy #$00

PRTTOK:
    lda (zpWORK+1),Y
    bmi ENDTOK

    jsr CHOUT

    iny
    bne PRTTOK

ENDTOK:
    ldy zpWORK+3        ; restore Y
    rts

; ----------------------------------------------------------------------------

; Print byte in A as %02x hexadecimal

HEXOUT:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr DIG
    pla
    and #$0F
DIG:
    cmp #$0A
    bcc DIGR
    adc #$06
DIGR:
    adc #$30
CHOUT:
    cmp #$0D
    bne NCH

    jsr OSWRCH
    jmp BUFEND         ; Set COUNT to zero

HEXSP:
    jsr HEXOUT

LISTPT:
    lda #' '            ; print a space

NCH:
    pha
    lda zpWIDTHV
    cmp zpTALLY
    bcs NOCRLF

    jsr NLINE

NOCRLF:
    pla
    inc zpTALLY
    .if WRCHV != 0
        jmp (WRCHV)     ; tail call
    .endif
    .if WRCHV == 0
        jmp OSWRCH      ; tail call
    .endif

; ----------------------------------------------------------------------------

LISTPS:
    and zpLISTOP
    beq LISTPX

    txa
    beq LISTPX
    bmi LISTPT

    .if version >= 3
        asl
        tax
    .endif

LISTPL:
    jsr LISTPT

    .if version < 3
        jsr CHOUT
    .endif

    dex
    bne LISTPL

LISTPX:
    rts

LISTO:
    inc zpCURSOR

    jsr AEEXPR
    jsr FDONE
    jsr INTEG

    lda zpIACC
    sta zpLISTOP
    jmp CLRSTK

; LIST [linenum [,linenum]]
; =========================
LIST:
    iny
    lda (zpLINE),Y
    cmp #'O'                ; listo command ?
    beq LISTO

    lda #$00
    sta zpWORK+4            ; FOR count
    sta zpWORK+5            ; REPEAT count

    jsr SINSTK
    jsr SPTSTN

    php
    jsr PHACC

    lda #$FF
    sta zpIACC
    lda #$7F
    sta zpIACC+1
    plp

    bcc NONUML

    jsr SPACES
    cmp #','
    beq GOTCX

    jsr POPACC
    jsr PHACC
    dec zpCURSOR
    bpl GOTCFF

NONUML:
    jsr SPACES
    cmp #','
    beq GOTCX

    dec zpCURSOR

GOTCX:
    jsr SPTSTN

GOTCFF:
    lda zpIACC
    sta zpFACCMA
    lda zpIACC+1
    sta zpFACCMB

    jsr DONE
    jsr ENDER           ; finished command line (?)
    jsr POPACC
    jsr FNDLNO

    lda zpWORK+6
    sta zpLINE
    lda zpWORK+7
    sta zpLINE+1
    bcc LIMTST

    dey
    bcs GETNUM

ENDLN:
    jsr NLINE
    jsr CLYADP

GETNUM:
    lda (zpLINE),Y
    sta zpIACC+1
    iny
    lda (zpLINE),Y
    sta zpIACC
    iny
    iny
    sty zpCURSOR

LIMTST:
    lda zpIACC
    clc
    sbc zpFACCMA
    lda zpIACC+1
    sbc zpFACCMB
    bcc LISTLN

    jmp CLRSTK

LISTLN:
    jsr NPRN

    ldx #$FF
    stx zpCOEFP
    lda #$01
    jsr LISTPS

    ldx zpWORK+4
    lda #$02
    jsr LISTPS

    ldx zpWORK+5
    lda #$04
    jsr LISTPS

LPX:
    ldy zpCURSOR

LP:
    lda (zpLINE),Y
    cmp #$0D
    beq ENDLN

    cmp #'"'
    bne LPTOKS

    lda #$FF
    eor zpCOEFP
    sta zpCOEFP

    lda #'"'
LPQUOT:
    jsr CHOUT

    iny
    bne LP

LPTOKS:
    bit zpCOEFP
    bpl LPQUOT

    cmp #tknCONST
    bne LPSIMP

    jsr SPGETN

    sty zpCURSOR
    lda #$00
    sta zpPRINTS
    jsr POSITE

    jmp LPX

LPSIMP:
    cmp #tknFOR
    bne LPSIMQ

    inc zpWORK+4

LPSIMQ:
    cmp #tknNEXT
    bne LPSIMR

    ldx zpWORK+4
    beq LPSIMR

    dec zpWORK+4

LPSIMR:
    cmp #tknREPEAT
    bne LPSIMS

    inc zpWORK+5

LPSIMS:
    cmp #tknUNTIL
    bne LPSIMT

    ldx zpWORK+5
    beq LPSIMT

    dec zpWORK+5

LPSIMT:
    jsr TOKOUT

    iny
    bne LP

; ----------------------------------------------------------------------------

NEXER:
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
NEXT:
    jsr AELV

    bne STRIPA

    ldx zpFORSTP
    beq NEXER
    bcs NOCHK

NEXHOW:
    jmp STDED

STRIPA:
    bcs NEXHOW

    ldx zpFORSTP
    beq NEXER

STRIP:
    lda zpIACC
    cmp FORINL-$f,X
    bne NOTIT

    lda zpIACC+1
    cmp FORINH-$f,X
    bne NOTIT

    lda zpIACC+2
    cmp FORINT-$f,X
    beq NOCHK

NOTIT:
    txa
    sec
    sbc #$0F
    tax
    stx zpFORSTP
    bne STRIP

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

NOCHK:
    lda FORINL-$f,X
    sta zpIACC
    lda FORINH-$f,X
    sta zpIACC+1
    ldy FORINT-$f,X
    cpy #$05
    beq FNEXT

    ldy #$00
    lda (zpIACC),Y
    adc FORSPL-$f,X
    sta (zpIACC),Y
    sta zpWORK

    iny
    lda (zpIACC),Y
    adc FORSPM-$f,X
    sta (zpIACC),Y
    sta zpWORK+1

    iny
    lda (zpIACC),Y
    adc FORSPN-$f,X
    sta (zpIACC),Y
    sta zpWORK+2

    iny
    lda (zpIACC),Y
    adc FORSPH-$f,X
    sta (zpIACC),Y

    tay
    lda zpWORK
    sec
    sbc FORLML-$f,X
    sta zpWORK
    lda zpWORK+1
    sbc FORLMM-$f,X
    sta zpWORK+1

    lda zpWORK+2
    sbc FORLMN-$f,X
    sta zpWORK+2
    tya
    sbc FORLMH-$f,X
    ora zpWORK
    ora zpWORK+1
    ora zpWORK+2
    beq NOTFIN

    tya
    eor FORSPH-$f,X
    eor FORLMH-$f,X
    bpl FORDRN
    bcs NOTFIN
    bcc FINFOR

FORDRN:
    bcs FINFOR

NOTFIN:
    ldy FORADL-$f,X
    lda FORADH-$f,X
    sty zpLINE
    sta zpLINE+1
    jsr SECUR

    jmp STMT

FINFOR:
    lda zpFORSTP
    sec
    sbc #$0F
    sta zpFORSTP
    ldy zpAECUR
    sty zpCURSOR

    jsr SPACES

    cmp #','
    bne NXTFIN

    jmp NEXT

FNEXT:
    jsr VARFP
    lda zpFORSTP
    clc
    adc #<(FORSPL-$f)
    sta zpARGP
    lda #>FORSPL
    sta zpARGP+1

    jsr FADD

    lda zpIACC
    sta zpWORK
    lda zpIACC+1
    sta zpWORK+1
    jsr STORPF

    lda zpFORSTP
    sta zpTYPE
    clc
    adc #<(FORLML-$f)
    sta zpARGP
    lda #>FORLML
    sta zpARGP+1
    jsr FCMP

    beq NOTFIN

    lda FORSPM-$f,X
    bmi FFORDR
    bcs NOTFIN
    bcc FINFOR

FFORDR:
    bcc NOTFIN
    bcs FINFOR

NXTFIN:
    jmp SUNK

; ----------------------------------------------------------------------------

FORCV:
    brk
    dta $22
    dta tknFOR
    .if foldup == 1
        dta ' VARIABLE'
    .else
        dta ' variable'
    .endif
    ; brk overlap
FORDP:
    brk
    dta $23
    .if foldup == 1
        dta 'TOO MANY ', tknFOR, 'S'
    .else
        dta 'Too many ', tknFOR, 's'
    .endif
    ; brk overlap
FORTO:
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
FOR:
    jsr CRAELV
    beq FORCV
    bcs FORCV

    jsr PHACC
    jsr EQEAT
    jsr STEXPR

    ldy zpFORSTP
    cpy #cFORTOP
    bcs FORDP

    lda zpWORK
    sta FORINL,Y
    lda zpWORK+1
    sta FORINH,Y
    lda zpWORK+2
    sta FORINT,Y
    tax
    jsr AESPAC

    cmp #tknTO
    bne FORTO

    cpx #$05
    beq FFOR

    jsr INEXPR

    ldy zpFORSTP
    lda zpIACC
    sta FORLML,Y
    lda zpIACC+1
    sta FORLMM,Y
    lda zpIACC+2
    sta FORLMN,Y
    lda zpIACC+3
    sta FORLMH,Y

    lda #$01
    jsr SINSTK
    jsr AESPAC

    cmp #tknSTEP
    bne FORSTW

    jsr INEXPR

    ldy zpAECUR

FORSTW:
    sty zpCURSOR
    ldy zpFORSTP
    lda zpIACC
    sta FORSPL,Y
    lda zpIACC+1
    sta FORSPM,Y
    lda zpIACC+2
    sta FORSPN,Y
    lda zpIACC+3
    sta FORSPH,Y

FORN:
    jsr FORR
    ldy zpFORSTP
    lda zpLINE
    sta FORADL,Y
    lda zpLINE+1
    sta FORADH,Y
    clc
    tya
    adc #$0F
    sta zpFORSTP

    jmp STMT

FFOR:
    jsr EXPR
    jsr FLOATI

    lda zpFORSTP
    clc
    adc #<FORLML        ; -$f in later BASICs?
    sta zpARGP
    lda #>FORLML
    sta zpARGP+1

    jsr FSTA
    jsr FONE
    jsr AESPAC

    cmp #tknSTEP
    bne FFORST

    jsr EXPR
    jsr FLOATI

    ldy zpAECUR

FFORST:
    sty zpCURSOR
    lda zpFORSTP
    clc
    adc #<FORSPL        ; -$f in later BASICs?
    sta zpARGP
    lda #>FORSPL
    sta zpARGP+1

    jsr FSTA

    jmp FORN

; ----------------------------------------------------------------------------

; GOSUB numeric
; =============
GOSUB:
    jsr GOFACT

ONGOSB:
    jsr DONE

    ldy zpSUBSTP
    cpy #cSUBTOP
    bcs GOSDP

    lda zpLINE
    sta SUBADL,Y
    lda zpLINE+1
    sta SUBADH,Y
    inc zpSUBSTP
    bcc GODONE

GOSDP:
    brk
    dta $25
    .if foldup == 1
        dta 'TOO MANY ', tknGOSUB, 'S'
    .else
        dta 'Too many ', tknGOSUB, 's'
    .endif

; ----------------------------------------------------------------------------

RETNUN:
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
RETURN:
    jsr DONE          ; Check for end of statement
    ldx zpSUBSTP
    beq RETNUN        ; If GOSUB stack empty, error

    dec zpSUBSTP      ; Decrement GOSUB stack
    ldy SUBADL-1,X    ; Get stacked line pointer
    lda SUBADH-1,X
    sty zpLINE
    sta zpLINE+1      ; Set line pointer

    jmp NXT           ; Jump back to execution loop

; ----------------------------------------------------------------------------

; GOTO numeric
; ============
GOTO:
    jsr GOFACT
    jsr DONE         ; Find destination line, check for end of statement

GODONE:
    lda zpTRFLAG
    beq GONO

    jsr TRJOBA       ; If TRACE ON, print current line number

GONO:
    ldy zpWORK+6
    lda zpWORK+7     ; Get destination line address

JUMPAY:
    sty zpLINE
    sta zpLINE+1     ; Set line pointer
    jmp STMT         ; Jump back to execution loop

; ----------------------------------------------------------------------------

; ON ERROR OFF
; ------------
ONERGF:
    jsr DONE         ; Check end of statement

    lda #<BASERR
    sta zpERRORLH    ; ON ERROR OFF
    lda #>BASERR
    sta zpERRORLH+1

    jmp NXT          ; Jump to execution loop

; ----------------------------------------------------------------------------

; ON ERROR [OFF | program ]
; -------------------------
ONERRG:
    jsr SPACES

    cmp #tknOFF
    beq ONERGF         ; ON ERROR OFF

    ldy zpCURSOR
    dey
    jsr CLYADP

    lda zpLINE
    sta zpERRORLH          ; Point ON ERROR pointer to here
    lda zpLINE+1
    sta zpERRORLH+1

    jmp REM         ; Skip past end of line

ONER:
    brk
    dta $27
    dta tknON
    .if foldup == 1
        dta ' SYNTAX'
    .else
        dta ' syntax'
    .endif
    brk

; ----------------------------------------------------------------------------

; ON [ERROR] [numeric]
; ====================
ON:
    jsr SPACES         ; Skip spaces and get next character
    cmp #tknERROR
    beq ONERRG         ; Jump with ON ERROR

    dec zpCURSOR
    jsr AEEXPR
    jsr INTEGB

    ldy zpAECUR
    iny
    sty zpCURSOR
    cpx #tknGOTO
    beq ONOK

    cpx #tknGOSUB
    bne ONER

ONOK:
    txa
    pha               ; Save GOTO/GOSUB token
    lda zpIACC+1
    ora zpIACC+2      ; Get IACC
    ora zpIACC+3
    bne ONRG          ; ON >255 - out of range, look for an ELSE

    ldx zpIACC
    beq ONRG          ; ON zero - out of range, look for an ELSE

    dex
    beq ONGOT         ; Dec. counter, if zero use first destination

; IACC+1 and IACC+2 are 0

    ldy zpCURSOR      ; Get line index
ONSRCH:
    lda (zpLINE),Y
    iny
    cmp #$0D
    beq ONRG         ; End of line - error

    cmp #':'
    beq ONRG         ; End of statement - error

    cmp #tknELSE
    beq ONRG         ; ELSE - drop everything else to here

    cmp #','
    bne ONSRCH       ; No comma, keep looking

    dex
    bne ONSRCH       ; Comma found, loop until count decremented to zero
    sty zpCURSOR     ; Store line index

ONGOT:
    jsr GOFACT         ; Read line number

    pla                ; Get stacked token back
    cmp #tknGOSUB
    beq ONGOS          ; Jump to do GOSUB

    jsr SECUR          ; Update line index and check Escape

    jmp GODONE         ; Jump to do GOTO

; Update line pointer so RETURN comes back to next statement
; ----------------------------------------------------------
ONGOS:
    ldy zpCURSOR       ; Get line pointer

ONSKIP:
    lda (zpLINE),Y
    iny                ; Get character from line
    cmp #$0D
    beq SKIPED         ; End of line, RETURN to here

    cmp #':'
    bne ONSKIP         ; <colon>, return to here

SKIPED:
    dey
    sty zpCURSOR       ; Update line index to RETURN point

    jmp ONGOSB         ; Jump to do the GOSUB

; ON num out of range - check for an ELSE clause
; ----------------------------------------------
ONRG:
    ldy zpCURSOR      ; Get line index
    pla               ; Drop GOTO/GOSUB token

ONELSE:
    lda (zpLINE),Y
    iny               ; Get character from line
    cmp #tknELSE
    beq ONELS         ; Found ELSE, jump to use it

    cmp #$0D
    bne ONELSE         ; Loop until end of line

    brk
    dta $28
    dta tknON
    .if foldup == 1
        dta ' RANGE'
    .else
        dta ' range'
    .endif
    brk

ONELS:
    sty zpCURSOR
    jmp THENLN         ; Store line index and jump to GOSUB

GOFACT:
    jsr SPTSTN
    bcs GOTGO          ; Embedded line number found

    jsr AEEXPR
    jsr INTEGB         ; Evaluate expression, ensure integer

    lda zpAECUR
    sta zpCURSOR       ; Line number low byte
    lda zpIACC+1
    and #$7F
    sta zpIACC+1       ; Line number high byte
                       ; Note - this makes goto $8000+10 the same as goto 10
                       ; try 10 PRINT "BBC"   20 GOTO 32768+10

GOTGO:
    jsr FNDLNO
    bcs NOLINE

    rts               ; Look for line, error if not found

NOLINE:
    brk
    dta $29
    .if foldup == 1
        dta 'NO SUCH LINE'
    .else
        dta 'No such line'
    .endif
    brk

; ----------------------------------------------------------------------------

INPUHD:
    jmp LETM

INPUHE:
    jmp STDED

INPUHX:
    sty zpCURSOR
    jmp DONEXT

; INPUT#channel, ...
; ------------------
INPUTH:
    dec zpCURSOR
    jsr AECHAN

    lda zpAECUR
    sta zpCURSOR
    sty zpCOEFP

INPUHL:
    jsr SPACES
    cmp #','
    bne INPUHX

    lda zpCOEFP
    pha
    jsr CRAELV

    beq INPUHE

    lda zpAECUR
    sta zpCURSOR
    pla
    sta zpCOEFP
    php
    jsr PHACC

    ldy zpCOEFP
    jsr OSBGET

    sta zpTYPE
    plp
    bcc INPUHN

    lda zpTYPE
    bne INPUHD

    jsr OSBGET

    sta zpCLEN
    tax
    beq INPUHS

INPUHT:
    jsr OSBGET

    sta STRACC-1,X
    dex
    bne INPUHT

INPUHS:
    jsr STSTOR
    jmp INPUHL

INPUHN:
    lda zpTYPE
    beq INPUHD
    bmi INPUHF

    ldx #$03

INPUHI:
    jsr OSBGET
    sta zpIACC,X
    dex
    bpl INPUHI

    bmi INPUHJ

INPUHF:
    ldx #$04

INPUHR:
    jsr OSBGET

    sta FWSA,X
    dex
    bpl INPUHR

    jsr LDARGA

INPUHJ:
    jsr STORE
    jmp INPUHL

INOUT:
    pla
    pla
    jmp DONEXT

; ----------------------------------------------------------------------------

; INPUT [LINE] [print items][variables]
; =====================================
INPUT:
    jsr SPACES         ; Get next non-space char
    cmp #'#'
    beq INPUTH         ; If '#' jump to do INPUT#

    cmp #tknLINE
    beq INLIN          ; If 'LINE', skip next with CS

    dec zpCURSOR
    clc                ; Step back to non-LINE char, set CC

INLIN:
    ror zpCOEFP
    lsr zpCOEFP        ; bit7=0, bit6=notLINE/LINE
    lda #$FF
    sta zpCOEFP+1

INPLP:
    jsr PRTSTN
    bcs INPHP         ; Process ' " TAB SPC, jump if none found

INPLO:
    jsr PRTSTN
    bcc INPLO         ; Keep processing any print items

    ldx #$FF
    stx zpCOEFP+1
    clc

INPHP:
    php
    asl zpCOEFP
    plp
    ror zpCOEFP
    cmp #','
    beq INPLP         ; ',' - jump to do next item

    cmp #';'
    beq INPLP         ; ';' - jump to do next item

    dec zpCURSOR
    lda zpCOEFP
    pha
    lda zpCOEFP+1
    pha
    jsr CRAELV
    beq INOUT

    pla
    sta zpCOEFP+1
    pla
    sta zpCOEFP
    lda zpAECUR
    sta zpCURSOR
    php
    bit zpCOEFP
    bvs INGET

    lda zpCOEFP+1
    cmp #$FF
    bne INGOT

INGET:
    bit zpCOEFP
    bpl INGETA

    lda #'?'
    jsr CHOUT

INGETA:
    jsr INLINE         ; Call MOS to input line, set COUNT=0

    sty zpCLEN
    asl zpCOEFP
    clc
    ror zpCOEFP
    bit zpCOEFP
    bvs INGETB

INGOT:
    sta zpAECUR
    lda #<STRACC
    sta zpAELINE
    lda #>STRACC
    sta zpAELINE+1
    jsr DATAST

INTERM:
    jsr AESPAC
    cmp #','
    beq INGETC

    cmp #$0D
    bne INTERM

    ldy #$FE
INGETC:
    iny
    sty zpCOEFP+1

INGETB:
    plp
    bcs INPSTR

    jsr PHACC
    jsr VALSTR
    jsr STORE

    jmp INPLP

INPSTR:
    lda #$00
    sta zpTYPE
    jsr STSTRE
    jmp INPLP

; ----------------------------------------------------------------------------

; RESTORE [linenum]
; =================
RESTORE:
    ldy #$00
    sty zpWORK+6          ; Set DATA pointer to PAGE
    ldy zpTXTP
    sty zpWORK+7
    jsr SPACES

    dec zpCURSOR
    cmp #':'
    beq RESDON

    cmp #$0D
    beq RESDON

    cmp #tknELSE
    beq RESDON

    jsr GOFACT

    ldy #$01
    jsr CLYADW          ; 0 points to the hopeful DATA token

RESDON:
    jsr DONE

    lda zpWORK+6
    sta zpDATAP
    lda zpWORK+7
    sta zpDATAP+1

    jmp NXT

; ----------------------------------------------------------------------------

READS:
    jsr SPACES
    cmp #','
    beq READ
    jmp SUNK

; READ varname [,...]
; ===================
READ:
    jsr CRAELV

    beq READS
    bcs READST

    jsr DATAIT
    jsr PHACC
    jsr STEXPR

    jmp READEN

READST:
    jsr DATAIT
    jsr PHACC
    jsr DATAST

    sta zpTYPE

    jsr STSTOR

READEN:
    clc
    lda zpAECUR
    adc zpAELINE
    sta zpDATAP
    lda zpAELINE+1
    adc #$00
    sta zpDATAP+1

    jmp READS

DATAIT:
    lda zpAECUR
    sta zpCURSOR
    lda zpDATAP
    sta zpAELINE
    lda zpDATAP+1
    sta zpAELINE+1

    ldy #$00
    sty zpAECUR
    jsr AESPAC

    cmp #','
    beq DATAOK

    cmp #tknDATA
    beq DATAOK

    cmp #$0D
    beq DATANX

DATALN:
    jsr AESPAC

    cmp #','
    beq DATAOK

    cmp #$0D
    bne DATALN

DATANX:
    ldy zpAECUR
    lda (zpAELINE),Y
    bmi DATAOT

    iny
    iny
    lda (zpAELINE),Y
    tax

DATANS:
    iny
    lda (zpAELINE),Y
    cmp #' '
    beq DATANS

    cmp #tknDATA
    beq DATAOL

    txa
    clc
    adc zpAELINE
    sta zpAELINE
    bcc DATANX

    inc zpAELINE+1
    bcs DATANX

DATAOT:
    brk
    dta $2A
    .if foldup == 1
        dta 'OUT OF '
    .else
        dta 'Out of '
    .endif
    dta tknDATA
    ; brk overlap
NODOS:
    brk
    dta $2B
    .if foldup == 1
        dta 'NO '
    .else
        dta 'No '
    .endif
    dta tknREPEAT
    brk

DATAOL:
    iny
    sty zpAECUR
DATAOK:
    rts

; ----------------------------------------------------------------------------

; UNTIL numeric
; =============
UNTIL:
    jsr AEEXPR
    jsr FDONE
    jsr INTEG

    ldx zpDOSTKP
    beq NODOS

    lda zpIACC
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq REDO

    dec zpDOSTKP
    jmp NXT

REDO:
    ldy DOADL-1,X
    lda DOADH-1,X

    jmp JUMPAY

; ----------------------------------------------------------------------------

DODP:
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

; ----------------------------------------------------------------------------

; REPEAT
; ======
REPEAT:
    ldx zpDOSTKP
    cpx #cDOTOP
    bcs DODP

    jsr CLYADP

    lda zpLINE
    sta DOADL,X
    lda zpLINE+1
    sta DOADH,X
    inc zpDOSTKP

    jmp STMT

; ----------------------------------------------------------------------------

; Input string to string buffer
; -----------------------------
INLINE:
    ldy #<STRACC
    lda #>STRACC
    bne BUFFA

; Print character, read input line
; --------------------------------
BUFF:
    jsr CHOUT         ; Print character
    ldy #<BUFFER
    lda #>BUFFER

BUFFA:
    sty zpWORK        ; 0 for both STRACC and BUFFER, could save a load here
    sta zpWORK+1      ; zpWORK=>input buffer

; Manually implement RDLINE (OSWORD 0)
; ------------------------------------
    .ifdef MOS_ATOM
RDLINELP:
        jsr OSRDCH       ; Wait for character
        cmp #$1B
        beq RDLINEX      ; Escape

        cmp #$7F
        bne RDLINED      ; Not Delete

        cpy #$00
        beq RDLINELP     ; Nothing to delete

        jsr OSWRCH       ; VDU 127
        dey
        jmp RDLINELP     ; Dec. counter, loop back

RDLINED:
        cmp #$15
        bne RDLINEC      ; Not Ctrl-U
        tya
        beq RDLINELP

        lda #$7F
RDLINLP2:
        jsr OSWRCH
        dey
        bne RDLINLP2
        beq RDLINELP

RDLINEC:
        sta (zpWORK),Y   ; Store character
        cmp #$0D
        beq NLINE        ; Return - finish

        cpy #$EE
        bcs RDLINEL      ; Maximum length

        cmp #$20
        bcs RDLINEG      ; Control character
        dey

RDLINEG:
        iny
        jsr OSWRCH       ; Inc. counter, print character
RDLINEL:
        jmp RDLINELP     ; Loop for more
RDLINEX:
    .endif

; BBC - Call MOS to read a line
; -----------------------------
    .ifdef MOS_BBC
        lda #$EE
        sta zpWORK+2      ; Maximum length
        lda #$20
        sta zpWORK+3      ; Lowest acceptable character
        ldy #$FF
        sty zpWORK+4      ; Highest acceptable character
        iny
        ldx #zpWORK       ; XY=>control block at $0037
        tya
        jsr OSWORD        ; Call OSWORD 0 to read line of text
        bcc BUFEND        ; CC, Escape not pressed, exit and set COUNT=0
    .endif
    jmp DOBRK         ; Escape

; ----------------------------------------------------------------------------

NLINE:
    jsr OSNEWL
BUFEND:
    lda #$00
    sta zpTALLY          ; Set COUNT to zero
    rts

; Removes line whose number is in IACC

REMOVE:
    jsr FNDLNO
    bcs REMOVX

    lda zpWORK+6    ; step WORK+6 back to line start
    sbc #$02
    sta zpWORK
    sta zpWORK+6
    sta zpTOP
    lda zpWORK+7
    sbc #$00
    sta zpWORK+1
    sta zpTOP+1
    sta zpWORK+7

    ldy #$03        ; get length
    lda (zpWORK),Y
    clc
    adc zpWORK
    sta zpWORK
    bcc MOVEA

    inc zpWORK+1

MOVEA:
    ldy #$00

MOVEB:
    lda (zpWORK),Y
    sta (zpTOP),Y
    cmp #$0D
    beq MOVED

MOVEC:
    iny
    bne MOVEB

    inc zpWORK+1
    inc zpTOP+1
    bne MOVEB

MOVED:
    iny
    bne MOVEE

    inc zpWORK+1
    inc zpTOP+1

MOVEE:
    lda (zpWORK),Y
    sta (zpTOP),Y
    bmi MOVEF

    jsr MOVEG
    jsr MOVEG
    jmp MOVEC

MOVEF:
    jsr CLYADT
    clc

REMOVX:
    rts

MOVEG:
    iny
    bne MOVEH

    inc zpTOP+1
    inc zpWORK+1

MOVEH:
    lda (zpWORK),Y
    sta (zpTOP),Y
    rts

; ----------------------------------------------------------------------------

INSRT:
    sty zpWORK+4
    jsr REMOVE          ; find old line and remove it

    ldy #>BUFFER
    sty zpWORK+5
    ldy #$00
    lda #$0D
    cmp (zpWORK+4),Y
    beq INSRTX

LENGTH:
    iny
    cmp (zpWORK+4),Y
    bne LENGTH

    iny
    iny
    iny
    sty zpWORK+8
    inc zpWORK+8
    lda zpTOP
    sta zpWORK+2
    lda zpTOP+1
    sta zpWORK+3
    jsr CLYADT

    sta zpWORK
    lda zpTOP+1
    sta zpWORK+1
    dey
    lda zpHIMEM
    cmp zpTOP
    lda zpHIMEM+1
    sbc zpTOP+1
    bcs MOVEUP

    jsr ENDER
    jsr SETFSA

    brk
    dta 0
    dta tknLINE
    .if foldup == 1
        dta ' SPACE'
    .else
        dta ' space'
    .endif
    brk

; ----------------------------------------------------------------------------

MOVEUP:
    lda (zpWORK+2),Y
    sta (zpWORK),Y
    tya
    bne LOW

    dec zpWORK+3
    dec zpWORK+1

LOW:
    dey
    tya
    adc zpWORK+2
    ldx zpWORK+3
    bcc LOWW

    inx

LOWW:
    cmp zpWORK+6
    txa
    sbc zpWORK+7
    bcs MOVEUP

    sec
    ldy #$01
    lda zpIACC+1
    sta (zpWORK+6),Y

    iny
    lda zpIACC
    sta (zpWORK+6),Y

    iny
    lda zpWORK+8
    sta (zpWORK+6),Y

    jsr CLYADWP1

    ldy #$FF

INSLOP:
    iny
    lda (zpWORK+4),Y
    sta (zpWORK+6),Y
    cmp #$0D
    bne INSLOP

INSRTX:
    rts

; ----------------------------------------------------------------------------

; RUN
; ===
RUN:
    jsr DONE

RUNNER:
    jsr SETFSA

    lda zpTXTP
    sta zpLINE+1          ; Point zpLINE to PAGE
    stx zpLINE

    jmp RUNTHG

; ----------------------------------------------------------------------------

; Clear BASIC heap, stack and DATA pointer
; ========================================
SETFSA:
    lda zpTOP
    sta zpLOMEM
    sta zpFSA          ; LOMEM=TOP, VAREND=TOP
    lda zpTOP+1
    sta zpLOMEM+1
    sta zpFSA+1

    jsr SETVAR         ; Clear DATA and stack

SETVAL:
    ldx #$80
    lda #$00

SETVRL:
    sta VARPTR-1,X
    dex
    bne SETVRL

    rts               ; Clear dynamic variables list

; ----------------------------------------------------------------------------

; Clear DATA pointer and BASIC stack
; ==================================
SETVAR:
    lda zpTXTP
    sta zpDATAP+1          ; DATA pointer hi=PAGE hi

    lda zpHIMEM
    sta zpAESTKP
    lda zpHIMEM+1
    sta zpAESTKP+1         ; STACK=HIMEM

    lda #$00
    sta zpDOSTKP
    sta zpFORSTP
    sta zpSUBSTP           ; Clear REPEAT, FOR, GOSUB stacks
    sta zpDATAP

    rts                    ; always return with A=0

; ----------------------------------------------------------------------------

PHFACC:
    lda zpAESTKP
    sec
    sbc #$05
    jsr HIDEC

    ldy #$00
    lda zpFACCX
    sta (zpAESTKP),Y

    iny
    lda zpFACCS
    and #$80
    sta zpFACCS

    lda zpFACCMA
    and #$7F
    ora zpFACCS
    sta (zpAESTKP),Y

    iny
    lda zpFACCMB
    sta (zpAESTKP),Y

    iny
    lda zpFACCMC
    sta (zpAESTKP),Y

    iny
    lda zpFACCMD
    sta (zpAESTKP),Y
    rts

; ----------------------------------------------------------------------------

; Pop the stack and set ARGP to point to the entry

POPSET:
    lda zpAESTKP
    clc
    sta zpARGP
    adc #$05
    sta zpAESTKP

    lda zpAESTKP+1
    sta zpARGP+1
    adc #$00
    sta zpAESTKP+1
    rts

; ----------------------------------------------------------------------------

PHTYPE:
    beq PHSTR
    bmi PHFACC

; Push Integer ACC to stack

PHACC:
    lda zpAESTKP
    sec
    sbc #$04

    jsr HIDEC

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

; ----------------------------------------------------------------------------

; Stack the current string
; ========================
PHSTR:
    clc               ; extra -1
    lda zpAESTKP
    sbc zpCLEN        ; stackbot=stackbot-length-1
    jsr HIDEC         ; Check enough space

    ldy zpCLEN
    beq PHSTRX        ; Zero length, just stack length

PHSTRL:
    lda STRACC-1,Y
    sta (zpAESTKP),Y  ; Copy string to stack
    dey
    bne PHSTRL        ; Loop for all characters

PHSTRX:
    lda zpCLEN
    sta (zpAESTKP),Y  ; Copy string length
    rts

; ----------------------------------------------------------------------------

; Unstack a string
; ================
POPSTR:
    ldy #$00
    lda (zpAESTKP),Y    ; Get stacked string length
    sta zpCLEN
    beq POPSTX          ; If zero length, just unstack length

    tay

POPSTL:
    lda (zpAESTKP),Y
    sta STRACC-1,Y      ; Copy string to string buffer
    dey
    bne POPSTL          ; Loop for all characters

POPSTX:
    ldy #$00
    lda (zpAESTKP),Y    ; Get string length again
    sec

POPN:
    adc zpAESTKP
    sta zpAESTKP        ; Update stack pointer
    bcc POPACI

    inc zpAESTKP+1
    rts

; ----------------------------------------------------------------------------

; Pop Integer ACC (zpIACC)

POPACC:
    ldy #$03
    lda (zpAESTKP),Y
    sta zpIACC+3

    dey
    lda (zpAESTKP),Y
    sta zpIACC+2

    dey
    lda (zpAESTKP),Y
    sta zpIACC+1

    dey
    lda (zpAESTKP),Y
    sta zpIACC

POPINC:
    clc
    lda zpAESTKP          ; Adjust stack pointer, drop 4 bytes
    adc #$04
    sta zpAESTKP
    bcc POPACI
    inc zpAESTKP+1

POPACI:
    rts

; Unstack an integer to zpWORK
; -----------------------------
POPWRK:
    ldx #zpWORK

; Use X as Index, jsr here to override X (i.e. ldx #zpWORK+8, jsr POPX)

POPX:
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
    bcc POPACI

    inc zpAESTKP+1
    rts

; ----------------------------------------------------------------------------

HIDEC:
    sta zpAESTKP
    bcs HIDECA

    dec zpAESTKP+1

HIDECA:
    ldy zpAESTKP+1
    cpy zpFSA+1
    bcc HIDECE
    bne HIDECX

    cmp zpFSA
    bcc HIDECE

HIDECX:
    rts

HIDECE:
    jmp ALLOCR

; ----------------------------------------------------------------------------

ACCTOM:
    lda zpIACC
    sta zp+0,X
    lda zpIACC+1
    sta zp+1,X
    lda zpIACC+2
    sta zp+2,X
    lda zpIACC+3
    sta zp+3,X
    rts

; ----------------------------------------------------------------------------

CLYADW:
    clc

CLYADWP1:
    tya
    adc zpWORK+6
    sta zpWORK+6
    bcc CLYIDW

    inc zpWORK+7

CLYIDW:
    ldy #$01
    rts

; ----------------------------------------------------------------------------

LOADER:
    jsr OSTHIG
    tay
    lda #$FF          ; FILE.LOAD=PAGE

    .ifdef MOS_ATOM
        sta F_EXEC+0
        ldx #zpWORK   ; FILE.EXEC=$FF, load to specified address
        sec
        jsr OSLOAD
    .endif

    .ifdef MOS_BBC
        sty F_EXEC+0
        ldx #zpWORK   ; FILE.EXEC=0, load to specified address
        jsr OSFILE
    .endif

; Scan program to check consistancy and find TOP
; ----------------------------------------------
ENDER:
    lda zpTXTP        ; TOP = PAGE
    sta zpTOP+1
    ldy #$00
    sty zpTOP
    iny

; find new TOP

FNDTOP:
    dey
    lda (zpTOP),Y     ; Get byte preceding line
    cmp #$0D
    bne BADPRO        ; Not <cr>, jump to 'Bad program'

    iny               ; Step to line number/terminator
    lda (zpTOP),Y
    bmi SETTOP        ; b7 set, end of program

    ldy #$03          ; Point to line length
    lda (zpTOP),Y
    beq BADPRO        ; Zero length, jump to 'Bad program'

    clc
    jsr CLYADTP1      ; Update TOP to point to next line
    bne FNDTOP        ; Loop to check next line

; End of program found, set TOP
; -----------------------------
SETTOP:
    iny
    clc

CLYADT:
    tya

CLYADTP1:
    adc zpTOP
    sta zpTOP          ; TOP=TOP+A
    bcc ENDADT

    inc zpTOP+1

ENDADT:
    ldy #$01
    rts               ; Return Y=1, NE

; Report 'Bad program' and jump to immediate mode
; -----------------------------------------------
BADPRO:
    jsr VSTRNG         ; Print inline text
    dta 13
    .if foldup == 1
        dta 'BAD PROGRAM'
    .else
        dta 'Bad program'
    .endif
    dta 13
    nop
    jmp CLRSTK         ; Jump to immediate mode

; ----------------------------------------------------------------------------

; Point zpWORK to <cr>-terminated string in string buffer
; ------------------------------------------------------
OSSTRG:
    lda #<STRACC
    sta zpWORK
    lda #>STRACC
    sta zpWORK+1

; Place <CR> at end of string

OSSTRT:
    ldy zpCLEN
    lda #$0D
    sta STRACC,Y
    rts                 ; returns with A=$0d and Y=string length

; ----------------------------------------------------------------------------

; OSCLI string$ - Pass string to OSCLI to execute
; ===============================================
OSCL:
    jsr OSTHIF         ; $37/8=>cr-string


    .ifdef MOS_ATOM
        jsr cmdStar1
        jmp NXT     ; Call Atom OSCLI and return to execution loop


    ; Embedded star command
    ; ---------------------
cmdStar:
        stx zpWORK
        sty zpWORK+1      ; $37/8=>cr-string
cmdStar1:
        ldy #$FF
cmdStarLp1:
        iny
        lda (zpWORK),Y
        cmp #'*'
        beq cmdStarLp1    ; Skip leading stars
        ldx #0
cmdStarLp2:
        lda (zpWORK),Y
        sta $0100,X       ; Copy string onto stack
        iny
        inx
        cmp #$0D
        bne cmdStarLp2    ; Atom OSCLI passed string at $100
        jmp OS_CLI
    .endif

    .ifdef MOS_BBC
        ldx #$00
        ldy #>(STRACC)
        jsr OS_CLI
        jmp NXT           ; Call OSCLI and return to execution loop
    .endif

OSTHIE:
    jmp LETM

OSTHIF:
    jsr AEEXPR
    bne OSTHIE            ; Evaluate expression, error if not string
    jsr OSSTRG
    jmp FDONE             ; Convert to <cr>-string, check end of statement

; ----------------------------------------------------------------------------

; Set FILE.LOAD to MEMHI.PAGE
; ---------------------------
OSTHIG:
    jsr OSTHIF
    dey
    sty F_LOAD+0        ; LOAD.lo=$00
    lda zpTXTP
    sta F_LOAD+1        ; LOAD.hi=PAGEhi

GETMAC:
    .ifdef MOS_BBC
        lda #$82
        jsr OSBYTE      ; Get memory base high word
        stx F_LOAD+2
        sty F_LOAD+3    ; Set LOAD high word
        lda #$00
    .endif
    rts

; BBC At/Sy
; 37   37   FNAME   zpWORK
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

; ----------------------------------------------------------------------------

;  SAVE string$
; =============
SAVE:
    jsr ENDER               ; Check program, set TOP

    .if version < 3
        lda zpTOP
        sta F_END+0         ; Set FILE.END to TOP
        lda zpTOP+1
        sta F_END+1
        lda #<ENTRY
        sta F_EXEC+0        ; Set FILE.EXEC to STARTUP
        lda #>ENTRY
        sta F_EXEC+1
        lda zpTXTP
        sta F_START+1       ; Set FILE.START to PAGE
        jsr OSTHIG          ; Set FILE.LOAD to PAGE
    .endif
    .if version < 3
        .ifdef MOS_BBC
            stx F_EXEC+2
            sty F_EXEC+3    ; Set address high words
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
        jsr OSTHIG           ; Set FILE.LOAD to PAGE
    .endif
    .if version >= 3
        .ifdef MOS_BBC
            stx F_EXEC+2
            sty F_EXEC+3     ; Set address high words
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
    ldx #zpWORK
    .ifdef MOS_ATOM
        sec
        jsr OSSAVE
    .endif
    .ifdef MOS_BBC
        jsr OSFILE
    .endif
    jmp NXT

; ----------------------------------------------------------------------------

; LOAD string$
; ============
LOAD:
    jsr LOADER
    jmp FSASET         ; Do LOAD, jump to immediate mode

; ----------------------------------------------------------------------------

; CHAIN string$
; =============
CHAIN:
    jsr LOADER         ; Do LOAD
    jmp RUNNER         ; jump to execution loop

; ----------------------------------------------------------------------------

; PTR#numeric=numeric
; ===================
LPTR:
    jsr AECHAN        ; Evaluate #handle

    pha
    jsr EQEXPR
    jsr INTEG         ; Step past '=', evaluate integer
    pla

    tay
    ldx #zpIACC
    .ifdef MOS_ATOM
        jsr OSSTAR
    .endif
    .ifdef MOS_BBC
        lda #$01
        jsr OSARGS
    .endif
    jmp NXT         ; Jump to execution loop

; ----------------------------------------------------------------------------

; =EXT#numeric - Read file pointer via OSARGS
; ===========================================
EXT:
    sec               ; Flag to do =EXT

; =PTR#numeric - Read file pointer via OSARGS
; ===========================================
RPTR:
    lda #$00
    rol               ; A=0 or 1 for =PTR or =EXT
    .ifdef MOS_BBC
        rol
    .endif

    pha               ; Atom - A=0/1, BBC - A=0/2
    jsr CHANN         ; Evaluate #handle
    ldx #zpIACC       ; point to IACC
    pla

    .ifdef MOS_ATOM
        jsr OSRDAR
    .endif
    .ifdef MOS_BBC
        jsr OSARGS
    .endif
    lda #$40
    rts               ; Return integer

; ----------------------------------------------------------------------------

; BPUT#numeric, numeric
; =====================
BPUT:
    jsr AECHAN      ; Evaluate #handle

    pha
    jsr COMEAT
    jsr EXPRDN
    jsr INTEG
    pla

    tay
    lda zpIACC
    jsr OSBPUT

    jmp NXT         ; Call OSBPUT, jump to execution loop

; ----------------------------------------------------------------------------

; =BGET#numeric
; =============
BGET:
    jsr CHANN         ; Evaluate #handle
    jsr OSBGET
    jmp SINSTK        ; Jump to return 8-bit integer

; ----------------------------------------------------------------------------

; OPENIN f$ - Call OSFIND to open file for input
; ==============================================
OPENIN:
    .ifdef MOS_ATOM
        sec           ; SEC=OPENUP
        bcs F
    .endif
    .ifdef MOS_BBC
        lda #$40      ; $40=OPENUP
        bne F
    .endif

; ----------------------------------------------------------------------------

; OPENOUT f$ - Call OSFIND to open file for output
; ================================================
OPENO:
    .ifdef MOS_ATOM
        clc           ; CLC=OPENOUT
        bcc F
    .endif
    .ifdef MOS_BBC
        lda #$80      ; 80=OPENOUT
        bne F
    .endif

; ----------------------------------------------------------------------------

; OPENUP f$ - Call OSFIND to open file for update
; ===============================================
OPENI:
    .ifdef MOS_ATOM
        sec           ; SEC=OPENUP
    .endif
    .ifdef MOS_BBC
        lda #$C0      ; C0=OPENUP
    .endif
F:
    .ifdef MOS_ATOM
        php
    .endif
    .ifdef MOS_BBC
        pha
    .endif
    jsr FACTOR
    bne OPENE          ; Evaluate, if not string, jump to error

    .ifdef MOS_ATOM
        jsr OSSTRG     ; Terminate string with <cr>, point $37/8=>string
        ldx #zpWORK
        plp            ; Point to string pointer, get action back
    .endif

    .ifdef MOS_BBC
        jsr OSSTRT     ; Terminate string with <cr>
        ldx #<STRACC
        ldy #>STRACC
        pla            ; Point to string buffer, get action back
    .endif

    jsr OSFIND         ; Pass to OSFIND, jump to return integer from A
    jmp SINSTK

OPENE:
    jmp LETM           ; Jump to 'Type mismatch' error

; ----------------------------------------------------------------------------

; CLOSE#numeric
; =============
CLOSE:
    jsr AECHAN
    jsr AEDONE         ; Evaluate #handle, check end of statement
    ldy zpIACC         ; Get handle from IACC
    .ifdef MOS_ATOM
        jsr OSSHUT
    .endif
    .ifdef MOS_BBC
        lda #$00
        jsr OSFIND
    .endif
    jmp NXT            ; Jump back to execution loop

; ----------------------------------------------------------------------------

; Copy LINE to AELINE, then get handle
; ====================================
AECHAN:
    lda zpCURSOR
    sta zpAECUR        ; copy cursor/offset
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1

; Check for '#', evaluate channel
; ===============================
CHANN:
    jsr AESPAC        ; Skip spaces, and get next character
    cmp #'#'          ; If not '#', jump to give error
    bne CHANNE

    jsr INTFAC        ; Evaluate as integer

    ldy zpIACC
    tya               ; Get low byte and return

NULLRET:
    rts

    .if version < 3
CHANNE:
        brk
        dta $2D
        .if foldup == 1
            dta 'MISSING #'
        .else
            dta 'Missing #'
        .endif
        brk
    .endif

; ----------------------------------------------------------------------------

; Print inline text
; =================
VSTRNG:
    pla
    sta zpWORK
    pla
    sta zpWORK+1      ; Pop return address to zpWORK

    ldy #$00
    beq VSTRLP        ; Jump into loop

VSTRLM:
    jsr OSASCI        ; Print character

VSTRLP:
    jsr GETWK2
    bpl VSTRLM        ; Update pointer, get character, loop if b7=0
    jmp (zpWORK)      ; Jump back to program

; ----------------------------------------------------------------------------

; REPORT
; ======
REPORT:
    jsr DONE
    jsr NLINE         ; Check end of statement, print newline, clear COUNT

    ldy #$01

REPLOP:
    lda (FAULT),Y
    beq REPORX        ; Get byte, exit if $00 terminator

    jsr TOKOUT

    iny
    bne REPLOP        ; Print character or token, loop for next

REPORX:
    jmp NXT           ; Jump to main execution loop

    .if version >= 3
CHANNE:
        brk
        dta $2D
        .if foldup == 1
            dta 'MISSING #'
        .else
            dta 'Missing #'
        .endif
        brk
    .endif

    .if * > [romstart + $4000]
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
            .if minorversion < 10
                dta '3'
            .else
                dta '3', '.', '1'
                .if .def BUILD_ATOM_BASIC310
                    dta '0'
                .endif
            .endif
        .endif
    .endif

    .if * > [romstart + $4000]
        .error "***WARNING: Code overrun"
    .endif

    .align romstart + $4000, 0
END_OF_ROM:

; vi:syntax=mads
