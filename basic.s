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
; BBC BASIC Copyright © 1982/1983 Acorn Computer and Roger Wilson
;
; References used:
;   Advanced BASIC ROM USer Guide
;   AcornCmosBasic (https://github.com/stardot/AcornCmosBasic)
;   AcornDmosBasic (https://github.com/stardot/AcornDmosBasic)
;   AcornBasic128 (github.com/stardot/AcornBasic128)
;   BBC Micro Compendium (not really used yet)
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

        split   = 0
        foldup  = 0
        title   = 0
        ws      = $0400-$0400     ; Offset from &400 to workspace
        membot  = 0       ; Use OSBYTE to find memory limits
        memtop  = 0       ; ...

        zp      = $00     ; Start of ZP addresses

        FAULT  = $fd      ; Pointer to error block
        ESCFLG = $ff      ; Escape pending flag

        F_LOAD  = zpWORK+2    ; LOAD/SAVE control block
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
            romstart = $a000    ; Code start address
            ws       = $2800-$0400      ; Offset from &400 to workspace
            membot   = $3000
            ESCFLG   = $0e21    ; Escape pending flag
        .elseif .def TARGET_ATOM
            romstart = $4000    ; Code start address
            ws       = $9c00-$0400      ; Offset from &400 to workspace
            membot   = $2800
            ESCFLG   = $b001    ; Escape pending flag
        .endif

        memtop  = romstart    ; Top of memory is start of code
        zp      = $00     ; Start of ZP addresses

        FAULT  = zp4F     ; Pointer to error block

        F_LOAD  = zpWORK+2    ; LOAD/SAVE control block
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

        romstart      = $b800     ; Code start address
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

        F_LOAD  = zpWORK+2    ; LOAD/SAVE control block
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
tknAUTO     = $C6
tknDELETE   = $C7
tknLOAD     = $C8
tknLIST     = $C9
tknNEW      = $CA
tknOLD      = $CB
tknRENUMBER = $CC
tknSAVE     = $CD
tknPTR2     = $CF
tknPAGE2    = $D0
tknTIME2    = $D1
tknLOMEM2   = $D2
tknHIMEM2   = $D3
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

L8000:

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
    dta $60           ; ROM type = Lang+Tube+6502 BASIC
    dta copyright_string - romstart       ; Offset to copyright string
    dta [version*2]-3     ; Version 2 = $01, Version 3 = $03
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
    sty zpTXTP

    ldx #$00
    stx zpLISTOP          ; Set LISTO to 0
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
    .if version == 2 || (version == 3 && minorversion == 10)
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
    .if version == 3 && minorversion != 10
        dta 'COLOUR', tknCOLOR,     $02      ; 00000010
    .elseif version == 3 && minorversion == 10
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
    dta 'HIMEM'   , tknHIMEM,       $43      ; 00100011
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
    jmp STMT          ; Set OPT, return to execution loop

ASS:
    lda #$03
    sta zpBYTESM      ; Set OPT 3, default on entry to '['

CASM:
    jsr SPACES        ; Skip spaces
    cmp #']'
    beq STOPASM       ; ']' - exit assembler

    jsr CLYADP

    dec zpCURSOR
    jsr MNEENT

    dec zpCURSOR
    lda zpBYTESM
    lsr
    bcc NOLIST

    lda zpTALLY
    adc #$04
    sta zpWORK+8
    lda zpWORK+1
    jsr HEXOUT

    lda zpWORK
    jsr HEXSP

    ldx #$FC
    ldy zpWORK+2
    bpl WRTLOP

    ldy zpCLEN
WRTLOP:
    sty zpWORK+1
    beq RMOVE

    ldy #$00
WRTLPY:
    inx
    bne WRTLPA

    jsr NLINE         ; Print newline

    ldx zpWORK+8

    .if version < 3
L8544:
        jsr LISTPT     ; Print a space
        dex
        bne L8544     ; Loop to print spaces
    .elseif version >= 3
        jsr LISTPL     ; Print multiple spaces
    .endif

    ldx #$fd
WRTLPA:
    lda (zpWORK+3),Y
    jsr HEXSP

    iny
    dec zpWORK+1
    bne WRTLPY

RMOVE:
    .if version < 3
        inx
        bpl L8565
        jsr LISTPT
        jsr CHOUT
        jsr CHOUT
        jmp RMOVE
L8565:
        ldy #0
    .elseif version >= 3
; RMOVE:
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
    cmp #':'
    beq NOCODA

    cmp #$0D
    beq NOCOD

LLLLLL6:
    jsr TOKOUT         ; Print character or token
    iny
    bne LLLL4

NOCODA:
    cpy zpCURSOR
    bcc LLLLLL6

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
    cmp #$0D
    bne NOLA

NOLB:
    jsr DONE_WITH_Y
    dey
    lda (zpLINE),Y
    cmp #':'
    beq CASMJ
    lda zpLINE+1
    cmp #>BUFFER
    bne INTXT
    jmp CLRSTK

INTXT:
    jsr LINO
CASMJ:
    jmp CASM

; ----------------------------------------------------------------------------

SETL:                 ; set label
    jsr CRAELV
    beq ASSDED
    bcs ASSDED
    jsr PHACC         ; PHADDR??
    jsr GETPC         ; Find P%
    sta zpTYPE
    jsr STORE
    jsr ASCUR

    .if version >= 3
        sty zpNEWVAR
    .endif

MNEENT:
    ldx #$03          ; Prepare to fetch three characters
    jsr SPACES        ; Skip spaces

    ldy #$00         ; number of bytes
    sty zpWORK+6
    cmp #':'
    beq MMMM         ; End of statement
    cmp #$0D
    beq MMMM         ; End of line
    cmp #'\'
    beq MMMM         ; Comment
    cmp #'.'
    beq SETL          ; Label
    dec zpCURSOR

RDLUP:
    ldy zpCURSOR
    inc zpCURSOR          ; Get current character, inc. index
    lda (zpLINE),Y
    bmi RDSLPT         ; Token, check for tokenised AND, EOR, OR

    cmp #' '
    beq RDOUT         ; Space, step past

    ldy #$05
    asl
    asl
    asl               ; Compact first character
INLUP:
    asl
    rol zpWORK+6
    rol zpWORK+7
    dey
    bne INLUP
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
    cpy zp3E
    beq RDOPGT        ; Mnemonic matches
NOTGOT:
    dex
    bne SRCHM         ; Loop through opcode lookup table
ASSDED:
    jmp STDED         ; Mnemonic not matched, Mistake

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

; Opcode found
; ------------

RDOPGT:
    lda STCODE-1,X
    sta zpOPCODE      ; Get base opcode
    ldy #$01          ; Y=1 for one byte
    cpx #IMPLIED+1
    bcs NGPONE        ; Opcode $1A+ have arguments

MMMM:
    lda PC
    sta zpWORK        ; Get P% low byte
    sty zpWORK+2
    ldx zpBYTESM
    cpx #$04          ; Offset assembly (opt>3)
    ldx PC+1
    stx zpWORK+1      ; Get P% high byte
    bcc MMMMLR        ; No offset assembly

    lda VARL_O        ; Get O%
    ldx VARL_O+1
MMMMLR:
    sta zpWORK+3
    stx zpWORK+4      ; Store destination pointer
    tya
    beq MMMMRT
    bpl MMMMLP
    ldy zpCLEN
    beq MMMMRT

MMMMLP:
    dey
    lda zpOPCODE,Y    ; Get opcode byte   (lda abs,y (!))
    bit zpWORK+2
    bpl MMMMCL        ; Opcode - jump to store it
    lda STRACC,Y      ; Get EQU byte
MMMMCL:
    sta (zp3A),Y      ; Store byte
    inc PC            ; Increment P%
    bne MMMMLQ
    inc PC+1
MMMMLQ:
    bcc MMMMPP        ; Increment O%
    inc VARL_O
    bne MMMMPP
    inc VARL_O+1
MMMMPP:
    tya
    bne MMMMLP
MMMMRT:
    rts

NGPONE:
    cpx #BRANCH+1      ; index for 'AND' opcode
    bcs NGPTWO
    jsr ASEXPR

    clc
    lda zpIACC
    sbc PC
    tay
    lda zpIACC+1
    sbc PC+1
    cpy #$01
    dey
    sbc #$00
    beq FWD

    cmp #$FF
    beq BACKWARDS

BOR:
    lda zpBYTESM
    .if version < 3
        lsr
    .elseif version >= 3
        and #$02
    .endif
    beq BRSTOR

    brk
    .if foldup == 1
        dta 1, 'OUT OF RANGE'
    .else
        dta 1, 'Out of range'
    .endif
    brk

BRSTOR:
    tay
BRSTO:
    sty zpIACC      ; zpOPCODE+1
BRST:
    ldy #$02
    jmp MMMM

BACKWARDS:
    tya
    bmi BRSTO
    bpl BOR

FWD:
    tya
    bpl BRSTO
    bmi BOR

NGPTWO:
    cpx #GROUP1+1      ; index for 'ASL' opcode
    bcs NGPTHR
    jsr SPACES         ; Skip spaces
    cmp #'#'
    bne NOTHSH
    jsr PLUS8

IMMED:
    jsr ASEXPR

INDINX:
    lda zpIACC+1
    beq BRST

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
    cpx #COPSTA+1
    bne NOPSTA
    jsr SPACES         ; Skip spaces, sta as others in group3 but for #

NOTHSH:
    cmp #'('
    bne NOTIND
    jsr ASEXPR
    jsr SPACES         ; Skip spaces
    cmp #')'
    bne ININX
    jsr SPACES         ; Skip spaces
    cmp #','
    bne BADIND         ; No comma, jump to Index error
    jsr PLUS10
    jsr SPACES         ; Skip spaces
    cmp #'Y'
    bne BADIND         ; (zp),Y missing Y, jump to Index error
    beq INDINX

; Parse (zp,X) addressing mode
; ----------------------------
ININX:
    cmp #','
    bne BADIND         ; No comma, jump to Index error

    jsr SPACES         ; Skip spaces
    cmp #'X'
    bne BADIND         ; zp,X missing X, jump to Index error

    jsr SPACES         ; Skip spaces
    cmp #')'
    beq INDINX         ; zp,X) - jump to process

BADIND:
    brk
    dta $03
    .if foldup == 1
        dta 'INDEX'
    .else
        dta 'Index'
    .endif
    brk

NOTIND:
    dec zpCURSOR
    jsr ASEXPR
    jsr SPACES         ; Skip spaces
    cmp #','
    bne OPTIM         ; No comma - jump to process as abs,X

    jsr PLUS10
    jsr SPACES         ; Skip spaces
    cmp #'X'
    beq OPTIM         ; abs,X - jump to process

    cmp #'Y'
    bne BADIND         ; Not abs,Y - jump to Index error

UNOPT:
    jsr PLUS8
    jmp JSRB

; abs and abs,X
; -------------
OPTIM:
    jsr PLUS4
OPTIMA:
    lda zpIACC+1
    bne UNOPT
    jmp BRST

NOPSTA:
    cpx #DECINC+1
    bcs NGPFR
    cpx #ASLROR+1
    bcs L8750

    jsr SPACES         ; Skip spaces
    cmp #'A'
    beq ACCUMS         ; ins A -

    dec zpCURSOR
L8750:
    jsr ASEXPR
    jsr SPACES         ; Skip spaces
    cmp #','
    bne OPTIMA         ; No comma, jump to ...
    jsr PLUS10
    jsr SPACES         ; Skip spaces
    cmp #'X'
    beq OPTIMA         ; Jump with address,X
    jmp BADIND         ; Otherwise, jump to Index error

ACCUMS:
    jsr PLUS4
    ldy #$01
    bne JSRC

NGPFR:
    cpx #JSRJMP-1
    bcs NGPFV
    cpx #CPXCPY+1
    beq BIT_
    jsr SPACES         ; Skip spaces
    cmp #'#'
    bne NHASH         ; Not #, jump with address
    jmp IMMED         ; Jump with immediate

NHASH:
    dec zpCURSOR

BIT_:
    jsr ASEXPR
    jmp OPTIM

NGPFV:
    cpx #JSRJMP
    beq JSR_
    bcs NGPSX
    jsr SPACES       ; Skip spaces
    cmp #'('
    beq JSRA         ; Jump with (... addressing mode
    dec zpCURSOR

JSR_:
    jsr ASEXPR

JSRB:
    ldy #$03

JSRC:
    jmp MMMM

JSRA:
    jsr PLUS10
    jsr PLUS10
    jsr ASEXPR
    jsr SPACES         ; Skip spaces
    cmp #')'
    beq JSRB
    jmp BADIND         ; No ) - jump to Index error

NGPSX:
    cpx #PSEUDO + 1
    bcs OPTION
    lda zp3D
    eor #$01
    and #$1F
    pha
    cpx #PSEUDO - 1
    bcs STXY
    jsr SPACES         ; Skip spaces
    cmp #'#'
    bne LDXY
    pla
    jmp IMMED

LDXY:
    dec zpCURSOR
    jsr ASEXPR
    pla
    sta zpWORK
    jsr SPACES         ; Skip spaces
    cmp #','
    beq LDIND
    jmp OPTIM

LDIND:
    jsr SPACES         ; Skip spaces
    and #$1F
    cmp zpWORK
    bne LDINDB
    jsr PLUS10
    jmp OPTIM

LDINDB:
    jmp BADIND         ; Jump to Index error

STXY:
    jsr ASEXPR
    pla
    sta zpWORK
    jsr SPACES         ; Skip spaces
    cmp #','
    bne GOOP
    jsr SPACES         ; Skip spaces
    and #$1F
    cmp zpWORK
    bne LDINDB
    jsr PLUS10
    lda zpIACC+1
    beq GOOP         ; High byte=0, continue
    jmp BYTE         ; value>255, jump to Byte error

GOOP:
    jmp OPTIMA

OPTION:
    bne EQUBWS
    jsr ASEXPR
    lda zpIACC
    sta zpBYTESM
    ldy #$00
    jmp MMMM

ASEXPR:
    jsr AEEXPR
    jsr INTEGB

ASCUR:
    ldy zpAECUR
    sty zpCURSOR
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
    inc zpCURSOR          ; Increment index
    lda (zpLINE),Y      ; Get next character
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

EQUB:
    txa
    pha
    jsr ASEXPR
    ldx #zpOPCODE
    jsr ACCTOM

    pla
    tay
EQUSX:
    jmp MMMM

EQUSE:
    jmp LETM

EQUS:
    lda zpBYTESM
    pha
    jsr AEEXPR
    bne EQUSE
    pla
    sta zpBYTESM
    jsr ASCUR
    ldy #$FF
    bne EQUSX

; ----------------------------------------------------------------------------

; Insert token in A into line

INTOK:
    pha
    clc
    tya
    adc zpWORK
    sta zpWORK+2
    ldy #$00
    tya
    adc zpWORK+1
    sta zpWORK+3
    pla
    sta (zpWORK),Y
INTOKA:
    iny             ; Replace
    lda (zp39),Y
    sta (zpWORK),Y
    cmp #$0D
    bne INTOKA
    rts

; ----------------------------------------------------------------------------

CONSTQ:
    and #$0F
    sta zpWORK+6
    sty zpWORK+7

CONSTR:
    iny
    lda (zpWORK),Y
    .if version < 3
        cmp #'9'+1
        bcs CONSTX
        cmp #'0'
    .elseif version >= 3
        jsr NUMBCP
    .endif
    bcc CONSTX
    and #$0F
    pha
    ldx zpWORK+7
    lda zpWORK+6
    asl
    rol zpWORK+7
    bmi CONSTY
    asl
    rol zpWORK+7
    bmi CONSTY
    adc zpWORK+6
    sta zpWORK+6
    txa
    adc zpWORK+7
    asl zpWORK+6
    rol
    bmi CONSTY
    bcs CONSTY
    sta zpWORK+7
    pla
    adc zpWORK+6
    sta zpWORK+6
    bcc CONSTR
    inc zpWORK+7
    bpl CONSTR
    pha

CONSTY:
    pla
    ldy #$00
    sec
    rts

CONSTX:
    dey
    lda #tknCONST
    jsr INTOK
    lda zpWORK
    adc #$02
    sta zp39
    lda zpWORK+1
    adc #$00
    sta zp3A

CONSTL:
    lda (zpWORK),Y
    sta (zpWORK+2),Y
    dey
    bne CONSTL
    ldy #$03

CONSTI:                 ; insert constant
    lda zpWORK+7
    ora #$40
    sta (zpWORK),Y
    dey
    lda zpWORK+6
    and #~$C0
    ora #$40
    sta (zpWORK),Y
    dey
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
    eor #$54
    sta (zpWORK),Y
    jsr NEXTCH
    jsr NEXTCH
    jsr NEXTCH
    ldy #$00
WORDCN:
    clc
    rts

WORDCQ:
    cmp #$7B
    bcs WORDCN
    cmp #'_'
    bcs WORDCY
    cmp #$5B
    bcs WORDCN
    cmp #'A'
    bcs WORDCY

NUMBCP:
    cmp #'9'+1
    bcs WORDCN
    cmp #'0'
WORDCY:
    rts

NUMBCQ:
    cmp #'.'
    bne NUMBCP
    rts

GETWRK:
    lda (zpWORK),Y
NEXTCH:
    inc zpWORK
    bne RELRTS
    inc zpWORK+1
RELRTS:
    rts

GETWK2:
    jsr NEXTCH         ; Increment $37/8
    lda (zpWORK),Y
    rts

; ----------------------------------------------------------------------------

; Tokenise line at $37/8
; ======================
MATCH:
    ldy #$00
    sty zpWORK+4        ; Set tokeniser to left-hand-side

MATCEV:
    sty zpWORK+5

MATCHA:
    lda (zpWORK),Y      ; Get current character
    cmp #$0D
    beq RELRTS         ; Exit with <cr>
    cmp #' '
    bne BMATCH         ; Skip <spc>

MATCHB:
    jsr NEXTCH
    bne MATCHA         ; Increment $37/8 and check next character

BMATCH:
    cmp #'&'
    bne CMATCH         ; Jump if not '&'

MATCHC:
    jsr GETWK2         ; Increment $37/8 and get next character
    jsr NUMBCP
    bcs MATCHC         ; Jump if numeric character

    cmp #'A'
    bcc MATCHA         ; Loop back if <'A'

    cmp #'F'+1
    bcc MATCHC         ; Step to next if 'A'..'F'
    bcs MATCHA         ; Loop back for next character

CMATCH:
    cmp #'"'
    bne DMATCH

MATCHD:
    jsr GETWK2         ; Increment $37/8 and get next character
    cmp #'"'
    beq MATCHB         ; Not quote, jump to process next character

    cmp #$0D
    bne MATCHD
    rts

DMATCH:
    cmp #':'
    bne MATCHE
    sty zpWORK+4
    sty zpWORK+5    ; mode:=left;constant:=false
    beq MATCHB

MATCHE:
    cmp #','
    beq MATCHB

    cmp #'*'
    bne FMATCH

    lda zpWORK+4
    bne YMATCH      ; test '*' and mode=left
    rts

FMATCH:
    cmp #'.'
    beq MATCHZ

    jsr NUMBCP
    bcc GMATCH

    ldx zpWORK+5    ; constant?
    beq MATCHZ

    jsr CONSTQ
    bcc MATCHF

MATCHZ:
    lda (zpWORK),Y
    jsr NUMBCQ
    bcc MATCHY

    jsr NEXTCH
    jmp MATCHZ

MATCHY:
    ldx #$FF
    stx zpWORK+4
    sty zpWORK+5
    jmp MATCHA

MATCHW:
    jsr WORDCQ
    bcc YMATCH

MATCHV:
    ldy #$00

MATCHG:
    lda (zpWORK),Y
    jsr WORDCQ
    bcc MATCHY

    jsr NEXTCH
    jmp MATCHG

GMATCH:               ; lookup word (optimised for none present words)
    cmp #'A'
    bcs HMATCH         ; Jump if letter

YMATCH:
    ldx #$FF
    stx zpWORK+4
    sty zpWORK+5

MATCHF:
    jmp MATCHB

HMATCH:
    cmp #'X'
    bcs MATCHW         ; Jump if >='X', nothing starts with X,Y,Z

    ldx #<TOKENS
    stx zpWORK+2       ; Point to token table
    ldx #>TOKENS
    stx zpWORK+3

IMATCH:
    cmp (zpWORK+2),Y   ; Special check on first character
    bcc MATCHG
    bne JMATCH

KMATCH:
    iny
    lda (zpWORK+2),Y
    bmi LMATCH

    cmp (zpWORK),Y
    beq KMATCH

    lda (zpWORK),Y
    cmp #'.'
    beq ABBREV

JMATCH:
    iny
    lda (zpWORK+2),Y
    bpl JMATCH

    cmp #tknWIDTH       ; last token in list
    bne MMATCH
    bcs MATCHV

ABBREV:
    iny

ABBREA:
    lda (zpWORK+2),Y
    bmi LMATCH

    inc zpWORK+2
    bne ABBREA
    inc zpWORK+3
    bne ABBREA

MMATCH:
    sec
    iny
    tya
    adc zpWORK+2
    sta zpWORK+2
    bcc NMATCH

    inc zp3A
NMATCH:
    ldy #$00
    lda (zpWORK),Y
    jmp IMATCH

LMATCH:
    tax               ; token held in x for now
    iny
    lda (zpWORK+2),Y
    sta zpWORK+6      ; Get token flag
    dey
    lsr
    bcc OMATCH

    lda (zpWORK),Y
    jsr WORDCQ
    bcs MATCHV

OMATCH:
    txa
    bit zpWORK+6
    bvc WMATCH

    ldx zpWORK+4      ; mode=left?
    bne WMATCH

    .if split == 0
        clc           ; Superflous as all paths to here have CLC
    .endif
    adc #tknPTR2-$8f

WMATCH:
    dey
    jsr INTOK

    ldy #$00
    ldx #$FF
    lda zpWORK+6
    lsr
    lsr
    bcc QMATCH

    stx zpWORK+4        ; mode=right
    sty zpWORK+5        ; constant=false

QMATCH:
    lsr
    bcc RMATCH

    sty zpWORK+4
    sty zpWORK+5

RMATCH:
    lsr
    bcc TMATCH

    pha
    iny

SMATCH:
    lda (zpWORK),Y
    jsr WORDCQ
    bcc XMATCH

    jsr NEXTCH
    jmp SMATCH

XMATCH:
    dey
    pla

TMATCH:
    lsr
    bcc UMATCH

    stx zpWORK+5        ; constant

UMATCH:
    lsr
    bcs AESPAR
    jmp MATCHB

; ----------------------------------------------------------------------------

; Skip Spaces, get next character from the aeline, aecur
; ------------------------------------------------------
AESPAC:
    ldy zpAECUR
    inc zpAECUR        ; Get offset, increment it
    lda (zpAELINE),Y   ; Get current character
    cmp #' '
    beq AESPAC         ; Loop until not space
AESPAR:
    rts

; Skip spaces, get next character from the line, cursor
; -----------------------------------------------------
SPACES:
    ldy zpCURSOR
    inc zpCURSOR
    lda (zpLINE),Y
    cmp #' '
    beq SPACES
COMRTS:
    rts

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
    jsr AESPAC
    cmp #','
    .if version < 3
        bne COMERR
        rts
    .elseif version >= 3
        beq COMRTS
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
    sta zpWORK+1      ; Point $37/8 to PAGE
    lda #$00
    sta zpWORK
    sta (zpWORK),Y    ; Remove end marker
    jsr ENDER         ; Check program and set TOP
    bne FSASET         ; Jump to clear heap and go to immediate mode

; ----------------------------------------------------------------------------

; END - Return to immediate mode
; ==============================
; END statement finds end of text and stops

END:
    jsr DONE         ; Check end of statement
    jsr ENDER         ; Check program and set TOP
    bne CLRSTK         ; Jump to immediate mode, keeping variables, etc

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

; NEW - Clear program, enter immediate mode
; =========================================
; NEW comand clears text and frees

NEW:
    jsr DONE         ; Check end of statement
    .if title != 0
        jsr X8ADD     ; NEW program
    .endif

; Start up with NEW program
; -------------------------
FORMAT:
    .if title == 0
        lda #$0D
        ldy zpTXTP
        sty zpTOP+1    ; TOP hi=PAGE hi
        ldy #$00
        sty zpTOP
        sty zpTRFLAG   ; TOP=PAGE, TRACE OFF
        sta (zpTOP),Y  ; ?(PAGE+0)=<cr>
        lda #$FF
        iny
        sta (zpTOP),Y  ; ?(PAGE+1)=$FF
        iny
        sty zpTOP      ; TOP=PAGE+2
    .endif

FSASET:
    jsr SETFSA         ; Clear variables, heap, stack

; IMMEDIATE LOOP
; ==============
CLRSTK:
    ldy #>BUFFER
    sty zpLINE+1
    ldy #<BUFFER
    sty zpLINE
    lda #<BASERR       ; default error message
    sta zpERRORLH
    lda #>BASERR
    sta zpERRORLH+1
    lda #'>'
    jsr BUFF           ; Print '>' prompt, read input to buffer at PtrA

; Execute line at program pointer in $0B/C
; ----------------------------------------
RUNTHG:
    lda #<BASERR       ; default error message
    sta zpERRORLH
    lda #>BASERR
    sta zpERRORLH+1
    ldx #$FF
    stx zpBYTESM       ; OPT=$FF - not within assembler
    stx zpWORK+5       ; constant
    txs                ; Clear machine stack
    jsr SETVAR         ; Clear DATA and stacks

    tay
    lda zpLINE
    sta zpWORK         ; Point zpWORK to program line
    lda zpLINE+1
    sta zpWORK+1
    sty zpWORK+4
    sty zpCURSOR

    jsr MATCHA
    jsr SPTSTN
    bcc DC             ; Tokenise, jump forward if no line number

    jsr INSRT          ; Insert into program
    jmp FSASET         ; Jump back to immediate loop

; Command entered at immediate prompt
; -----------------------------------
DC:                    ; Direct Command
    jsr SPACES         ; Skip spaces at PtrA
    cmp #tknAUTO
    bcs DISPATCH       ; If command token, jump to execute command
    bcc LETST          ; Not command token, try variable assignment

LEAVE:
    jmp CLRSTK         ; Jump back to immediate mode

; [ - enter assembler
; ===================
JUMPASS:
    jmp ASS         ; Jump to assembler

; =<value> - return from FN
; =========================
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknFN
FNRET:
    tsx
    cpx #$FC
    bcs FNERR     ; If stack is empty, jump to give error
    lda $01FF
    cmp #tknFN
    bne FNERR     ; If pushed token<>'FN', give error
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
    dey
    lda (zpLINE),Y    ; Step program pointer back and fetch char
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
DATA:
DEF:
REM:
    lda #$0D
    ldy zpCURSOR
    dey               ; Get program pointer
ILP:
    iny
    cmp (zpLINE),Y
    bne ILP          ; Loop until <cr> found
ENDEDL:
    cmp #tknELSE
    beq REM          ; If 'ELSE', jump to skip to end of line
    lda zpLINE+1
    cmp #>BUFFER
    beq LEAVE        ; Program in command buffer, jump back to immediate loop
    jsr LINO
    bne STMT         ; Check for end of program, step past <cr>

; Main execution loop
; -------------------
SUNK:
    dec zpCURSOR

DONEXT:
    jsr DONE

NXT:
    ldy #$00
    lda (zpLINE),Y    ; Get current character
    cmp #':'
    bne ENDEDL         ; Not <colon>, check for ELSE

STMT:
    ldy zpCURSOR
    inc zpCURSOR      ; Get program pointer, increment for next time
    lda (zpLINE),Y    ; Get current character
    cmp #' '
    beq STMT          ; Skip spaces
    cmp #tknPTR2
    bcc LETST         ; Not program command, jump to try variable assignment

; Dispatch function/command
; -------------------------
DISPATCH:
    tax                 ; Index into dispatch table
    lda ADTABL-$8E,X
    sta zpWORK          ; Get routine address from table
    lda ADTABH-$8E,X
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
    jsr EQEAT         ; Check for and step past '='
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
    jmp STDED

LETM:
    brk
    dta 6
    .if foldup == 1
        dta 'TYPE MISMATCH'
    .else
        dta 'Type mismatch'
    .endif
    brk

STSTOR:
    jsr POPACC         ; Unstack integer (address of data)

STSTRE:
    lda zpIACC+2
    cmp #$80
    beq NSTR         ; Jump if absolute string $addr

    ldy #$02
    lda (zpIACC),Y
    cmp zpCLEN
    bcs ALLOCX       ; old mlen >= to new len

    lda zpFSA
    sta zpIACC+2
    lda zpFSA+1
    sta zpIACC+3

    lda zpCLEN
    cmp #$08         ; if <8 characters then use this as mlen
    bcc ALLOCU

    adc #$07         ; add 8 because carry is set
    bcc ALLOCU

    lda #$FF
ALLOCU:
    clc
    pha
    tax
    lda (zpIACC),Y   ; is new space contiguous to old?
    ldy #$00
    adc (zpIACC),Y
    eor zpFSA
    bne ALLJIM

    iny
    adc (zpIACC),Y
    eor zpFSA+1
    bne ALLJIM

    sta zpIACC+3    ; new space is, so reduce amount needed
    txa
    iny
    sec
    sbc (zpIACC),Y
    tax

ALLJIM:
    txa
    clc
    adc zpFSA
    tay
    lda zpFSA+1
    adc #$00
    cpy zpAESTKP    ; are we hitting our heads on the roof?
    tax
    sbc zpAESTKP+1
    bcs ALLOCR

    sty zpFSA
    stx zpFSA+1
    pla
    ldy #$02
    sta (zpIACC),Y  ; new mlen
    dey
    lda zpIACC+3
    beq ALLOCX      ; we were contiguous

    sta (zpIACC),Y
    dey
    lda zpIACC+2
    sta (zpIACC),Y

ALLOCX:
    ldy #$03
    lda zpCLEN
    sta (zpIACC),Y
    beq STDONE

    dey
    dey
    lda (zpIACC),Y
    sta zpIACC+3
    dey
    lda (zpIACC),Y
    sta zpIACC+2

LTCVRM:
    lda STRACC,Y
    sta (zpIACC+2),Y
    iny
    cpy zpCLEN
    bne LTCVRM

STDONE:
    rts

; ----------------------------------------------------------------------------

NSTR:
    jsr LBEBA
    cpy #$00
    beq NSTRX

NSLOOP:
    lda STRACC,Y
    sta (zpIACC),Y
    dey
    bne NSLOOP

    lda STRACC
NSTRX:
    sta (zpIACC),Y
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

STORST:
    lda zpWORK+2
    cmp #$80
    beq STORSX
    bcc STORIT

    ldy #$00
    lda (zpAESTKP),Y
    tax
    beq STORSY

    lda (zpWORK),Y
    sbc #$01
    sta zpWORK+2
    iny
    lda (zpWORK),Y
    sbc #$00
    sta zpWORK+3

STORSL:
    lda (zpAESTKP),Y
    sta (zpWORK+2),Y
    iny
    dex
    bne STORSL

STORSY:
    lda (zpAESTKP,X)
    ldy #$03
STORSW:
    sta (zpWORK),Y
    jmp POPSTX

STORSX:
    ldy #$00
    lda (zpAESTKP),Y
    tax
    beq STORSZ

STORSV:
    iny
    lda (zpAESTKP),Y
    dey
    sta (zpWORK),Y
    iny
    dex
    bne STORSV

STORSZ:
    lda #$0D
    bne STORSW

STORIT:
    ldy #$00
    lda (zpAESTKP),Y
    sta (zpWORK),Y
    .if version < 3
        iny
        cpy zpWORK+2
        bcs STORIY
    .elseif version >= 3
        ldy #4
        lda zpWORK+2
        beq STORIY
        ldy #$01
    .endif
    lda (zpAESTKP),Y
    sta (zpWORK),Y
    iny
    lda (zpAESTKP),Y
    sta (zpWORK),Y
    iny
    lda (zpAESTKP),Y
    sta (zpWORK),Y
    iny
    cpy zpWORK+2
    bcs STORIY

    lda (zpAESTKP),Y
    sta (zpWORK),Y
    iny
STORIY:
    tya
    clc
    jmp POPN

; ----------------------------------------------------------------------------

; PRINT#

PRINTH:
    dec zpCURSOR
    jsr LBFA9

PRINHL:
    tya
    pha
    jsr AESPAC
    cmp #','
    bne PRINHX

    jsr EXPR
    jsr STARGA

    pla
    tay
    lda zpTYPE
    jsr OSBPUT

    tax
    beq PRINHS
    bmi PRINHF

    ldx #$03
PRINHQ:
    lda zpIACC,X
    jsr OSBPUT
    dex
    bpl PRINHQ
    bmi PRINHL

PRINHF:
    ldx #$04

PRINHP:
    lda FWSA,X
    jsr OSBPUT
    dex
    bpl PRINHP
    bmi PRINHL      ; branch always

PRINHS:
    lda zpCLEN
    jsr OSBPUT
    tax
    beq PRINHL

PRINHO:
    lda STRACC-1,X
    jsr OSBPUT
    dex
    bne PRINHO
    beq PRINHL

PRINHX:
    pla
    sty zpCURSOR
    jmp DONEXT

; End of PRINT statement
; ----------------------
DEDPRC:
    jsr NLINE         ; Output new line and set COUNT to zero
DEDPR:
    jmp SUNK          ; Check end of statement, return to execution loop

PRFUNY:
    lda #$00
    sta zpPRINTS
    sta zpPRINTF      ; Set current field to zero, hex/dec flag to decimal
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
; -------------
PRCOMM:
    lda VARL_AT
    beq STRTPR       ; If field width zero, no padding needed
                     ; jump back into main loop

    lda zpTALLY      ; Get COUNT

PRCOML:
    beq STRTPR       ; Zero, just started a new line, no padding
                     ; jump back into main loop

    sbc VARL_AT      ; Get COUNT-field width
    bcs PRCOML       ; Loop to reduce until (COUNT MOD fieldwidth)<0

    tay              ; Y=number of spaces to get back to (COUNT MOD width)=zero
PRCOMO:
    jsr LISTPT
    iny
    bne PRCOMO         ; Loop to print required spaces

STRTPR:
    clc               ; Prepare to print decimal
    lda VARL_AT
    sta zpPRINTS          ; Set current field width from @%
AMPER:
    ror zpPRINTF          ; Set hex/dec flag from Carry
ENDPRI:
    jsr SPACES         ; Get next non-space character
    cmp #':'
    beq DEDPRC         ; End of statement if <colon> found
    cmp #$0D
    beq DEDPRC         ; End if statement if <cr> found
    cmp #tknELSE
    beq DEDPRC         ; End of statement if 'ELSE' found

CONTPR:
    cmp #'~'
    beq AMPER         ; Jump back to set hex/dec flag from Carry
    cmp #','
    beq PRCOMM         ; Jump to pad to next print field
    cmp #';'
    beq PRFUNY         ; Jump to check for end of print statement
    jsr PRSPEC
    bcc ENDPRI         ; Check for ' TAB SPC, if print token found return to outer main loop

; All print formatting have been checked, so it now must be an expression
; -----------------------------------------------------------------------
    lda zpPRINTS
    pha
    lda zpPRINTF
    pha               ; Save field width and flags, as evaluator
                      ;  may call PRINT (eg FN, STR$, etc.)
    dec zpAECUR
    jsr EXPR          ; Evaluate expression

    pla
    sta zpPRINTF
    pla
    sta zpPRINTS      ; Restore field width and flags

    lda zpAECUR
    sta zpCURSOR      ; Update program pointer
    tya
    beq PSTR         ; If type=0, jump to print string

    jsr FCON          ; Convert numeric value to string

    lda zpPRINTS      ; Get current field width
    sec
    sbc zpCLEN        ; A=width-stringlength
    bcc PSTR          ; length>width - print it
    beq PSTR          ; length=width - print it

    tay               ; Otherwise, Y=number of spaces to pad with
FPRNL:
    jsr LISTPT
    dey
    bne FPRNL         ; Loop to print required spaces to pad the number

; Print string in string buffer
; -----------------------------
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
    jmp COMERR

TAB2:
    cmp #','
    bne TABCOM         ; No comma, jump to TAB(x)
    lda zpIACC
    pha               ; Save X
    jsr BRA
    jsr INTEGB

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
        jsr WRIACC     ; Y coord
    .endif

    jmp PRTSTM         ; Continue to next PRINT item

TAB:
    jsr INEXPR
    jsr AESPAC
    cmp #')'
    bne TAB2

    lda zpIACC
    sbc zpTALLY
    beq PRTSTM
    .if version < 3
        tay
    .elseif version >= 3
        tax
    .endif
    bcs SPCLOP
    jsr NLINE
    beq SPCT

SPC:
    jsr INTFAC

SPCT:
    .if version < 3
        ldy zpIACC
    .elseif version >= 3
        ldx zpIACC
    .endif
    beq PRTSTM

SPCLOP:
    .if version < 3
        jsr LISTPT
        dey
        bne SPCLOP
    .elseif version >= 3
        jsr LISTPL
    .endif
    beq PRTSTM

PRCR:
    jsr NLINE

PRTSTM:
    clc
    ldy zpAECUR
    sty zpCURSOR
    rts

PRSPEC:
    ldx zpLINE
    stx zpAELINE
    ldx zpLINE+1
    stx zpAELINE+1
    ldx zpCURSOR
    stx zpAECUR

    cmp #$27            ; '
    beq PRCR

    cmp #tknTAB
    beq TAB

    cmp #tknSPC
    beq SPC

    sec
PRTSTO:
    rts

PRTSTN:
    jsr SPACES         ; Skip spaces
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

PCH:
    jsr CHOUT
PRSTRN:
    iny
    lda (zpAELINE),Y
    cmp #$0D
    beq NSTNG

    cmp #'"'
    bne PCH

    iny
    sty zpAECUR
    lda (zpAELINE),Y
    cmp #'"'
    bne PRTSTM
    beq PCH                 ; branch always

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
    jsr AEEXPR
    jsr INTEG
    jsr PHACC

    ldy #$00
    sty STRACC

CALLLP:
    sty ws+$06FF
    jsr AESPAC

    cmp #','
    bne CALLDN

    ldy zpAECUR
    jsr LVBLNKplus1
    beq CALLFL

    ldy ws+$06FF
    iny
    lda zpIACC
    sta STRACC,Y
    iny
    lda zpIACC+1
    sta STRACC,Y
    iny
    lda zpIACC+2
    sta STRACC,Y
    inc STRACC

    jmp CALLLP

CALLDN:
    dec zpAECUR
    jsr AEDONE      ; Check for end of statement
    jsr POPACC      ; Pop integer to IACC
    jsr USER        ; Set up registers and call code at IACC
    cld             ; Ensure Binary mode on return
    jmp NXT         ; Jump back to program loop

CALLFL:
    jmp FACERR

; Call code
; ---------
USER:
    lda VARL_C      ; get carry from C%
    lsr
    lda VARL_A      ; get A from A%
    ldx VARL_X      ; get X from X%
    ldy VARL_Y      ; get Y from Y%
    .if .def TARGET_C64
        jmp $ff9b
    .else
        jmp (zpIACC)    ; Jump to address in IACC
    .endif

; ----------------------------------------------------------------------------

DELDED:
    jmp STDED

; DELETE linenum, linenum
; =======================
DELETE:
    jsr SPTSTN
    bcc DELDED

    jsr PHACC
    jsr SPACES
    cmp #','
    bne DELDED

    jsr SPTSTN
    bcc DELDED

    jsr DONE
    lda zpIACC
    sta zpWORK+2
    lda zpIACC+1
    sta zpWORK+3
    jsr POPACC

DODELS:
    jsr REMOVE
    jsr TSTBRK
    jsr INCACC

    lda zpWORK+2
    cmp zpIACC
    lda zpWORK+3
    sbc zpIACC+1
    bcs DODELS

    jmp FSASET

;  ----------------------------------------------------------------------------

; Called by RENUMBER and AUTO
GETTWO:
    lda #$0A
    jsr SINSTK
    jsr SPTSTN
    jsr PHACC

    lda #$0A
    jsr SINSTK
    jsr SPACES
    cmp #','
    bne NO

    jsr SPTSTN
    lda zpIACC+1
    bne GETYUK

    lda zpIACC
    beq GETYUK

    inc zpCURSOR
NO:
    dec zpCURSOR
    jmp DONE

; called by renumber
RENSET:
    lda zpTOP
    sta zpWORK+4
    lda zpTOP+1
    sta zpWORK+5

RENSTR:
    lda zpTXTP
    sta zpWORK+1
    lda #$01
    sta zpWORK
    rts

; RENUMBER [linenume [,linenum]]
; ==============================
RENUM:
    jsr GETTWO
    ldx #zpWORK+2
    jsr POPX
    jsr ENDER
    jsr RENSET

; Build up table of line numbers
NUMBA:
    ldy #$00
    lda (zpWORK),Y
    bmi NUMBB         ; Line.hi>$7F, end of program

    sta (zpWORK+4),Y
    iny
    lda (zpWORK),Y
    sta (zpWORK+4),Y

    sec
    tya
    adc zpWORK+4
    sta zpWORK+4

    tax
    lda zpWORK+5
    adc #$00
    sta zpWORK+5
    cpx zpHIMEM
    sbc zpHIMEM+1
    bcs NUMBFL

    jsr STEPON      ; Y=1
    bcc NUMBA

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
        sta zpAELINE      ; PtrB=PtrA=>after 'PROC' token
        lda zpLINE+1
        sta zpAELINE+1
        lda zpCURSOR
        sta zpAECUR
        lda #$F2
        jsr FNBODY     ; Call PROC/FN dispatcher
                       ; Will return here after ENDPROC
        jsr AEDONE     ; Check for end of statement
        jmp NXT        ; Return to execution loop
NUMBB:
    .endif

; Look for renumber references
    jsr RENSTR

NUMBC:
    ldy #$00
    lda (zpWORK),Y
    bmi NUMBD

    lda zpWORK+3
    sta (zpWORK),Y
    lda zpWORK+2
    iny
    sta (zpWORK),Y

    clc
    lda zpIACC
    adc zpWORK+2
    sta zpWORK+2

    lda #$00
    adc zpWORK+3
    and #$7F
    sta zpWORK+3

    jsr STEPON
    bcc NUMBC

NUMBD:
    lda zpTXTP
    sta zpLINE+1
    ldy #$00
    sty zpLINE
    iny
    lda (zpLINE),Y
    .if version < 3
        bmi NUMBXX
    .elseif version >= 3
        bmi NUMBX
    .endif

NUMBE:
    ldy #$04
NUMBF:
    lda (zpLINE),Y
    cmp #tknCONST
    beq NUMBG
    iny
    cmp #$0D
    bne NUMBF
    lda (zpLINE),Y
    .if version < 3
        bmi NUMBXX
    .elseif version >= 3
        bmi NUMBX
    .endif
    ldy #$03
    lda (zpLINE),Y
    clc
    adc zpLINE
    sta zpLINE
    bcc NUMBE

    inc zpLINE+1
    bcs NUMBE

NUMBXX:
    .if version < 3
        jmp FSASET
    .endif

NUMBG:
    jsr SPGETN
    jsr RENSET

NUMBH:
    ldy #$00
    lda (zpWORK),Y
    bmi NUMBJ
    lda (zpWORK+4),Y
    iny
    cmp zpIACC+1
    bne NUMBI
    lda (zpWORK+4),Y
    cmp zpIACC
    bne NUMBI
    lda (zpWORK),Y
    sta zpWORK+6
    dey
    lda (zpWORK),Y
    sta zpWORK+7
    ldy zpCURSOR
    dey
    lda zpLINE
    sta zpWORK
    lda zpLINE+1
    sta zpWORK+1
    jsr CONSTI

NUMBFA:
    ldy zpCURSOR
    bne NUMBF

NUMBI:
    .if version >= 3
        clc
    .endif
    jsr STEPON
    lda zpWORK+4
    adc #$02
    sta zp3B
    bcc NUMBH

    inc zpWORK+5
    bcs NUMBH

NUMBX:
    .if version >= 3
        bmi ENDAUT
    .endif

NUMBJ:
    jsr VSTRNG         ; Print inline text
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

    jsr POSITE         ; Print in decimal
    jsr NLINE         ; Print newline
    beq NUMBFA

STEPON:
    iny
    lda (zpWORK),Y
    adc zpWORK
    sta zpWORK
    bcc STEPX

    inc zpWORK+1
    clc
STEPX:
    rts

; ----------------------------------------------------------------------------

; AUTO [numeric [, numeric ]]
; ===========================
AUTO:
    jsr GETTWO
    lda zpIACC
    pha
    jsr POPACC

AUTOLP:
    jsr PHACC
    jsr NPRN

    lda #' '
    jsr BUFF
    jsr POPACC
    jsr MATCH
    jsr INSRT
    jsr SETFSA

    pla
    pha
    clc
    adc zpIACC
    sta zpIACC
    bcc AUTOLP
    inc zpIACC+1
    bpl AUTOLP

ENDAUT:
    jmp FSASET

; ----------------------------------------------------------------------------

; Code related to DIM

DIMSPR:
    jmp DIMRAM

DIMSP:
    dec zpCURSOR
    jsr CRAELV
    beq NOTGO
    bcs NOTGO

    jsr PHACC
    jsr INEXPR
    jsr INCACC

    lda zpIACC+3
    ora zpIACC+2
    bne NOTGO

    clc
    lda zpIACC
    adc zpFSA
    tay
    lda zpIACC+1
    adc zpFSA+1
    tax
    cpy zpAESTKP
    sbc zpAESTKP+1
    bcs DIMSPR

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

    jsr STORE
    jsr ASCUR
    jmp DIMNXT

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
    jsr SPACES
    tya
    clc
    adc zpLINE
    ldx zpLINE+1
    bcc DIMNNO

    inx
    clc

DIMNNO:
    sbc #$00
    sta zpWORK
    txa
    sbc #$00
    sta zpWORK+1
    ldx #$05
    stx zpWORK+8
    ldx zpCURSOR
    jsr WORD

    cpy #$01
    beq NOTGO
    cmp #'('
    beq DIMVAR
    cmp #'$'
    beq DIMINT
    cmp #'%'
    bne DIMSPA

DIMINT:
    dec zpWORK+8
    iny
    inx
    lda (zpWORK),Y
    cmp #'('
    beq DIMVAR

DIMSPA:
    jmp DIMSP

DIMVAR:
    sty zpWORK+2
    stx zpCURSOR
    jsr LOOKUP
    bne NOTGO       ; NE chain branch to NOTGO

    jsr CREATE
    ldx #$01
    jsr CREAX

    lda zpWORK+8
    pha
    lda #$01
    pha
    jsr SINSTK

RDLOOP:
    jsr PHACC
    jsr ASEXPR

    lda zpIACC+1
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne NOTGO

    jsr INCACC

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
    jsr SMUL
    jsr SPACES

    cmp #','
    beq RDLOOP
    cmp #')'
    beq DIMGON

    jmp NOTGO

DIMGON:
    pla
    sta zpPRINTF
    pla
    sta zpWORK+8

    lda #$00
    sta zpWORK+9
    jsr WMUL

    ldy #$00
    lda zpPRINTF
    sta (zpFSA),Y
    adc zpIACC
    sta zpIACC
    bcc NOHINC

    inc zpIACC+1
NOHINC:
    lda zpFSA+1
    sta zpWORK+1
    lda zpFSA
    sta zpWORK
    clc
    adc zpIACC
    tay
    lda zpIACC+1
    adc zpFSA+1
    bcs DIMRAM

    tax
    cpy zpAESTKP
    sbc zpAESTKP+1
    bcs DIMRAM

    sty zpFSA
    stx zpFSA+1
    lda zpWORK
    adc zpPRINTF
    tay
    lda #$00
    sta zpWORK
    bcc ZELOOP

    inc zpWORK+1

ZELOOP:
    sta (zpWORK),Y
    iny
    bne ZELPA
    inc zpWORK+1

ZELPA:
    cpy zpFSA
    bne ZELOOP
    cpx zpWORK+1
    bne ZELOOP

DIMNXT:
    jsr SPACES
    cmp #','
    beq DIMJ
    jmp SUNK

DIMJ:
    jmp DIM

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
    jsr POPX

WMUL:
    ldx #$00
    ldy #$00

SMULA:
    lsr zpWORK+9
    ror zpWORK+8
    bcc SMULB

    clc
    tya
    adc zpIACC
    tay
    txa
    adc zpIACC+1
    tax
    bcs SMULXE

SMULB:
    asl zpIACC
    rol zpIACC+1
    lda zpWORK+8
    ora zpWORK+9
    bne SMULA

    .if .hi(SMULA) != .hi(*)
        .error "ASSERT: SMUL loop crosses page"
    .endif

    sty zpIACC
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
    sta zpAESTKP        ; Set HIMEM and STACK
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
    sta zpFSA          ; Set LOMEM and VAREND
    lda zpIACC+1
    sta zpLOMEM+1
    sta zpFSA+1
    jsr SETVAL         ; Clear dynamic variables

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
    jsr SPTSTN
    bcs TRACNM         ; If line number, jump for TRACE linenum
    cmp #tknON
    beq TRACON         ; Jump for TRACE ON
    cmp #tknOFF
    beq TOFF         ; Jump for TRACE OFF
    jsr ASEXPR         ; Evaluate integer

; TRACE numeric
; -------------
TRACNM:
    jsr DONE         ; Check end of statement
    lda zpIACC
    sta zpTRNUM          ; Set trace limit low byte
    lda zpIACC+1
TRACNO:
    sta zpTRNUM+1
    lda #$FF          ; Set trace limit high byte, set TRACE ON
TRACNN:
    sta zpTRFLAG
    jmp NXT         ; Set TRACE flag, return to execution loop

; ----------------------------------------------------------------------------

; TRACE ON
; --------
TRACON:
    inc zpCURSOR
    jsr DONE         ; Step past, check end of statement
    lda #$FF
    bne TRACNO         ; Jump to set TRACE $FFxx

; ----------------------------------------------------------------------------

; TRACE OFF
; ---------
TOFF:
    inc zpCURSOR
    jsr DONE         ; Step past, check end of statement
    lda #$00
    beq TRACNN         ; Jump to set TRACE OFF

; ----------------------------------------------------------------------------

; TIME=numeric
; ============
LTIME:
    jsr INEQEX         ; Step past '=', evaluate integer
    .ifdef MOS_BBC
        ldx #$2A
        ldy #$00
        sty zpFACCS      ; Point to integer, set 5th byte to 0
        lda #$02
        jsr OSWORD    ; Call OSWORD $02 to do TIME=
    .endif
    jmp NXT         ; Jump to execution loop

; ----------------------------------------------------------------------------

; Evaluate <comma><numeric>
; =========================
INCMEX:
    jsr COMEAT         ; Check for and step past comma

INEXPR:
    jsr EXPR
    jmp INTEGB

; ----------------------------------------------------------------------------

; Evaluate <equals><integer>
; ==========================
INTFAC:
    jsr FACTOR
    beq INTEGE
    bmi INTEGF

INTEGX:
    rts

INEQEX:
    jsr AEEQEX          ; Check for equals, evaluate numeric

INTEG:
    lda zpTYPE         ; Get result type

INTEGB:
    beq INTEGE         ; String, jump to 'Type mismatch'
    bpl INTEGX         ; Integer, return

INTEGF:
    jmp IFIX           ; Real, jump to convert to integer

INTEGE:
    jmp LETM           ; Jump to 'Type mismatch' error

; Evaluate <real>
; ===============
FLTFAC:
    jsr FACTOR         ; Evaluate expression

; Ensure value is real
; --------------------
FLOATI:
    beq INTEGE         ; String, jump to 'Type mismatch'
    bmi INTEGX         ; Real, return
    jmp IFLT           ; Integer, jump to convert to real

; ----------------------------------------------------------------------------

    .if split == 0
; PROCname [(parameters)]
; =======================
PROC:
        lda zpLINE
        sta zpAELINE      ; PtrB=PtrA=>after 'PROC' token
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

LOCSTR:
    ldy #$03
    lda #$00           ; Set length to zero
    sta (zpIACC),Y
    beq LOCVAR         ; Jump to look for next LOCAL item

; LOCAL variable [,variable ...]
; ==============================
LOCAL:
    tsx
    cpx #$FC
    bcs NLOCAL        ; Not inside subroutine, error

    jsr CRAELV
    beq LOCEND        ; Find variable, jump if bad variable name

    jsr RETINF        ; Push value on stack, push variable info on stack

    ldy zpIACC+2
    bmi LOCSTR        ; If a string, jump to make zero length

    jsr PHACC
    lda #$00          ; Set IACC to zero
    jsr SINSTK

    sta zpTYPE
    jsr STORE         ; Set current variable to IACC (zero)

; Next LOCAL item
; ---------------
LOCVAR:
    tsx
    inc $0106,X       ; Increment number of LOCAL items
    ldy zpAECUR
    sty zpCURSOR      ; Update line pointer
    jsr SPACES        ; Get next character
    cmp #','
    beq LOCAL         ; Comma, loop back to do another item
    jmp SUNK          ; Jump to main execution loop

LOCEND:
    jmp DONEXT

; ----------------------------------------------------------------------------

; ENDPROC
; =======
; Stack needs to contain these items,
;  ret_lo, ret_hi, PtrB_hi, PtrB_lo, PtrB_off, numparams, PtrA_hi, PtrA_lo, PtrA_off, tknPROC
ENDPR:
    tsx
    cpx #$FC
    bcs NOPROC         ; If stack empty, jump to give error
    lda $01FF
    cmp #tknPROC
    bne NOPROC         ; If pushed token<>'PROC', give error
    jmp DONE     ; Check for end of statement and return to pop from subroutine

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
    jsr ASEXPR
    lda zpIACC
    pha               ; Evaluate integer
    jsr INCMEX         ; Step past comma, evaluate integer
    jsr AEDONE         ; Update program pointer, check for end of statement
    lda #$12
    jsr OSWRCH        ; Send VDU 18 for GCOL
    jmp SENTWO         ; Jump to send two bytes to OSWRCH

; ----------------------------------------------------------------------------

; COLOUR numeric
; ==============
COLOUR:
    lda #$11
    pha               ; Stack VDU 17 for COLOUR
    jsr ASEXPR
    jsr DONE         ; Evaluate integer, check end of statement
    jmp SENTWO         ; Jump to send two bytes to OSWRCH

; ----------------------------------------------------------------------------

; MODE numeric
; ============
MODES:
    lda #$16
    pha               ; Stack VDU 22 for MODE
    jsr ASEXPR
    jsr DONE         ; Evaluate integer, check end of statement

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
        bne MODESX     ; STACK<>HIMEM, stack not empty, give 'Bad MODE' error

        lda zpAESTKP+1
        cmp zpHIMEM+1
        bne MODESX

        ldx zpIACC
        lda #$85
        jsr OSBYTE    ; Get top of memory if we used this MODE

        cpx zpFSA
        tya
        sbc zpFSA+1
        bcc MODESX     ; Would be below VAREND, give error

        cpx zpTOP
        tya
        sbc zpTOP+1
        bcc MODESX     ; Would be below TOP, give error

        ; BASIC stack is empty, screen would not hit heap or program

        stx zpHIMEM
        stx zpAESTKP      ; Set STACK and HIMEM to new address
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
    jsr WRIACC
    jmp NXT           ; Send IACC to OSWRCH, jump to execution loop

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
    jmp PLOTER         ; Jump to evaluate second expression and send to OSWRCH

; ----------------------------------------------------------------------------

; PLOT numeric, numeric, numeric
; ==============================
PLOT:
    jsr ASEXPR

    lda zpIACC
    pha               ; Evaluate integer
    jsr COMEAT
    jsr EXPR          ; Step past comma, evaluate expression

PLOTER:
    jsr INTEG         ; Confirm numeric and ensure is integer
    jsr PHACC         ; Stack integer
    jsr INCMEX        ; Step past command and evaluate integer
    jsr AEDONE        ; Update program pointer, check for end of statement

    lda #$19
    jsr OSWRCH        ; Send VDU 25 for PLOT

    pla
    jsr OSWRCH        ; Send PLOT action
    jsr POPWRK        ; Pop integer to temporary store at $37/8

    lda zpWORK
    jsr OSWRCH        ; Send first coordinate to OSWRCH

    lda zpWORK+1
    jsr OSWRCH

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
    beq VDUX

    cmp #tknELSE
    beq VDUX

    dec zpCURSOR       ; Step back to current character
    jsr ASEXPR
    jsr WRIACC         ; Evaluate integer and output low byte
    jsr SPACES         ; Get next character

    cmp #','
    beq VDU            ; Comma, loop to read another number

    cmp #';'
    bne VDUL           ; Not semicolon, loop to check for end of statement
    beq VDUP           ; Loop to output high byte and read another

VDUX:
    jmp SUNK         ; Jump to execution loop

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
    ldy #$F8
    bne CREATF          ; Point to FN list start and scan list

; ----------------------------------------------------------------------------

; Look for a variable in the heap
; -------------------------------
; LOOKUP is given a base address-1 in WORK,WORK+1, length+1 in WORK+2
; It returns with EQ set if it can't find the thing, else with
; IACC,IACC+1 pointing to the data item and NEQ.
;
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

L9479:
    lda zpWORK+4
    beq L94B2         ; End of list

    ldy #$00
    lda (zpWORK+3),Y
    sta zpWORK+5
    iny
    lda (zpWORK+3),Y
    sta zpWORK+6
    iny
    lda (zpWORK+3),Y
    bne L949A         ; Jump if not null name

    dey
    cpy zpWORK+2
    bne L94B3

    iny
    bcs L94A7

L9495:
    iny
    lda (zpWORK+3),Y
    beq L94B3

L949A:
    cmp (zpWORK),Y
    bne L94B3

    cpy zpWORK+2
    bne L9495

    iny
    lda (zpWORK+3),Y
    bne L94B3

L94A7:
    tya
    adc zpWORK+3
    sta zpIACC
    lda zpWORK+4
    adc #$00
    sta zpIACC+1

L94B2:
    rts

L94B3:
    lda zpWORK+6
    beq L94B2

    ldy #$00
    lda (zpWORK+5),Y
    sta zpWORK+3
    iny
    lda (zpWORK+5),Y
    sta zpWORK+4
    iny
    lda (zpWORK+5),Y
    bne L94D4

    dey
    cpy zpWORK+2
    bne L9479

    iny
    bcs L94E1

L94CF:
    iny
    lda (zpWORK+5),Y
    beq L9479

L94D4:
    cmp (zpWORK),Y
    bne L9479

    cpy zpWORK+2
    bne L94CF

    iny
    lda (zpWORK+5),Y
    bne L9479

L94E1:
    tya
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
    lda #$F6
    cpx #tknPROC
    beq LOOKMN
    lda #$F8
    bne LOOKMN

; ----------------------------------------------------------------------------

;  CREATE takes the same parameters as LOOKUP and adds the
;  data item to the linked list. It is not optimised.

CREATE:
    ldy #$01
    lda (zpWORK),Y
    asl

LOOKMN:
    sta zpWORK+3
    lda #>VARL
    sta zpWORK+4

LOOPLB:
    lda (zpWORK+3),Y
    beq LOOKK

    tax
    dey
    lda (zpWORK+3),Y
    sta zpWORK+3
    stx zpWORK+4
    iny
    bpl LOOPLB

LOOKK:
    lda zpFSA+1
    sta (zpWORK+3),Y
    lda zpFSA
    dey
    sta (zpWORK+3),Y
    tya
    iny
    sta (zpFSA),Y
    cpy zpWORK+2
    beq CREATX

LOOPRA:
    iny
    lda (zpWORK),Y
    sta (zpFSA),Y
    cpy zpWORK+2
    bne LOOPRA
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
    sta (zpFSA),Y
    dex
    bne CREZER

FSAPY:
    sec
    tya
    adc zpFSA
    bcc CREATY

    inc zpFSA+1

CREATY:
    ldy zpFSA+1
    cpy zpAESTKP+1
    bcc CREATZ
    bne CREATD

    cmp zpAESTKP
    bcc CREATZ

CREATD:
    lda #$00
    ldy #$01
    sta (zpWORK+3),Y
    jmp ALLOCR

CREATZ:
    sta zpFSA
CREATX:
    rts

; ----------------------------------------------------------------------------

; Check if variable name is valid
; ===============================
WORD:
    ldy #$01

WORDLP:
    lda (zpWORK),Y
    cmp #'0'
    bcc WORDDN

    cmp #'@'
    bcs WORDNA

    cmp #':'
    bcs WORDDN

    cpy #$01
    beq WORDDN

WORDNC:
    inx
    iny
    bne WORDLP

WORDNA:
    cmp #'_'
    bcs WORDNB
    cmp #'['
    bcc WORDNC
WORDDN:
    rts

WORDNB:
    cmp #$7B
    bcc WORDNC
    rts

; ----------------------------------------------------------------------------

CRAELT:
    jsr CREAX

CRAELV:
    jsr AELV
    bne LVRTS
    bcs LVRTS

    jsr CREATE

    ldx #$05
    cpx zpIACC+2
    bne CRAELT
    inx
    bne CRAELT

LVFD:
    cmp #'!'
    beq UNPLIN
    cmp #'$'
    beq DOLL
    eor #'?'
    beq UNIND

    lda #$00
    sec

LVRTS:
    rts

UNPLIN:
    lda #$04
UNIND:
    pha
    inc zpAECUR
    jsr INTFAC
    jmp INSET

DOLL:
    inc zpAECUR
    jsr INTFAC

    lda zpIACC+1
    beq DOLLER

    lda #$80
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

AELV:
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    ldy zpCURSOR
    dey

LVBLNK:
    iny

LVBLNKplus1:
    sty zpAECUR
    lda (zpAELINE),Y
    cmp #' '
    beq LVBLNK

LVCONT:
    cmp #'@'
    bcc LVFD        ; probably not an lv but check for unary things
                    ; this test also removes numeric first characters

    cmp #'['
    bcs MULTI

    asl
    asl
    sta zpIACC
    lda #>VARL
    sta zpIACC+1
    iny
    lda (zpAELINE),Y
    iny
    cmp #'%'
    bne MULTI

    ldx #$04
    stx zpIACC+2
    lda (zpAELINE),Y
    cmp #'('
    bne CHKQUE

MULTI:
    ldx #$05
    stx zpIACC+2
    lda zpAECUR
    clc
    adc zpAELINE
    ldx zpAELINE+1
    bcc BKTVNO

    inx
    clc
BKTVNO:
    sbc #$00
    sta zpWORK
    bcs BKTVNP

    dex
BKTVNP:
    stx zpWORK+1
    ldx zpAECUR
    ldy #$01

BKTVD:
    lda (zpWORK),Y
    cmp #'A'
    bcs BKTVA

    cmp #'0'
    bcc BKTVE

    cmp #'9'+1
    bcs BKTVE

    inx
    iny
    bne BKTVD

BKTVA:
    cmp #'Z'+1
    bcs BKTVDD
    inx
    iny
    bne BKTVD

BKTVDD:
    cmp #'_'
    bcc BKTVE

    cmp #'z'+1
    bcs BKTVE

    inx
    iny
    bne BKTVD

BKTVE:
    dey
    beq BKTVFL

    cmp #'$'
    beq LVSTR

    cmp #'%'
    bne BKTVF

    dec zpIACC+2
    iny
    inx
    iny
    lda (zpWORK),Y
    dey

BKTVF:
    sty zpWORK+2
    cmp #'('
    beq BKTVAR

    jsr LOOKUP
    beq BKTVFC

    stx zpAECUR

CHKPLI:
    ldy zpAECUR
    lda (zpAELINE),Y

CHKQUE:
    cmp #'!'
    beq BIPLIN

    cmp #'?'
    beq BIQUER

    clc
    sty zpAECUR
    lda #$FF
    rts

BKTVFL:
    lda #$00
    sec
    rts

BKTVFC:
    lda #$00
    clc
    rts

BIQUER:
    lda #$00
    beq BIPLIN+2    ; skip lda #4

BIPLIN:
    lda #$04
    pha
    iny
    sty zpAECUR

    jsr VARIND
    jsr INTEGB

    lda zpIACC+1
    pha
    lda zpIACC
    pha
    jsr INTFAC

    clc
    pla
    adc zpIACC
    sta zpIACC
    pla
    adc zpIACC+1
    sta zpIACC+1

INSET:
    pla
    sta zpIACC+2
    clc
    lda #$FF
    rts

BKTVAR:
    inx
    inc zpWORK+2
    jsr ARRAY
    jmp CHKPLI

LVSTR:
    inx
    iny
    sty zpWORK+2
    iny
    dec zpIACC+2
    lda (zpWORK),Y
    cmp #'('
    beq LVSTRA

    jsr LOOKUP
    beq BKTVFC

    stx zpAECUR
    lda #$81
    sta zpIACC+2
    sec
    rts

LVSTRA:
    inx
    sty zp39
    dec zpIACC+2
    jsr ARRAY
    lda #$81
    sta zpIACC+2
    sec
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

ARRAY:
    jsr LOOKUP
    beq UNARRY

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
    bcc AQUICK

    tya
    jsr SINSTK
    lda #$01
    sta zpIACC+3

ARLOP:
    jsr PHACC
    jsr INEXPR
    inc zpAECUR
    cpx #','
    bne UNARRY

    ldx #zpWORK+2
    jsr POPX

    ldy zpWORK+5
    pla
    sta zpWORK+1
    pla
    sta zpWORK
    pha
    lda zpWORK+1
    pha

    jsr TSTRNG

    sty zpIACC+3
    lda (zpWORK),Y
    sta zpWORK+8
    iny
    lda (zpWORK),Y
    sta zpWORK+9
    lda zpIACC
    adc zpWORK+2
    sta zpIACC
    lda zpIACC+1
    adc zpWORK+3
    sta zpIACC+1

    jsr WMUL

    ldy #$00
    sec
    lda (zpWORK),Y
    sbc zpIACC+3
    cmp #$03
    bcs ARLOP

    jsr PHACC
    jsr BRA
    jsr INTEGB

    pla
    sta zpWORK+1
    pla
    sta zpWORK

    ldx #zpWORK+2
    jsr POPX

    ldy zpWORK+5
    jsr TSTRNG

    clc
    lda zpWORK+2
    adc zpIACC
    sta zpIACC
    lda zpWORK+3
    adc zpIACC+1
    sta zpIACC+1
    bcc ARFOUR

AQUICK:
    jsr BRA
    jsr INTEGB

    pla
    sta zpWORK+1
    pla
    sta zpWORK
    ldy #$01
    jsr TSTRNG

ARFOUR:
    pla
    sta zpIACC+2
    cmp #$05
    bne ARFO

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
    bcc ARFI

ARFO:
    asl zpIACC
    rol zpIACC+1
    asl zpIACC
    rol zpIACC+1

ARFI:
    tya
    adc zpIACC
    sta zpIACC
    bcc NNINC

    inc zpIACC+1
    clc
NNINC:
    lda zpWORK
    adc zpIACC
    sta zpIACC
    lda zpWORK+1
    adc zpIACC+1
    sta zpIACC+1
    rts

TSTRNG:
    lda zpIACC+1
    and #$C0
    ora zpIACC+2
    ora zpIACC+3
    bne SUBSCP

    lda zpIACC
    cmp (zpWORK),Y
    iny
    lda zpIACC+1
    sbc (zpWORK),Y
    bcs SUBSCP

    iny
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

SPTSTM:
    inc zpCURSOR

SPTSTN:
    ldy zpCURSOR
    lda (zpLINE),Y
    cmp #' '
    beq SPTSTM

    cmp #tknCONST
    bne FDA

SPGETN:
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

FDA:
    clc
    rts

AEEQEX:
    lda zpLINE
    sta zpAELINE
    lda zpLINE+1
    sta zpAELINE+1
    lda zpCURSOR
    sta zpAECUR

EQEXPR:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y
    cmp #' '
    beq EQEXPR

    cmp #'='
    beq EXPRDN

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
    jsr AESPAC
    cmp #'='
    bne EQERRO
    rts

EXPRDN:
    jsr EXPR

FDONE:
    txa
    ldy zpAECUR
    jmp DONET

AEDONE:
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
    bne STDED         ; Not 'ELSE', jump to 'Syntax error'

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
        beq DOBRK     ; Escape key pressed, jump to error XXX: A is not popped?
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
        beq DOBRK     ; If Escape, jump to error, XXX: A is not popped?
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

FORR:
    jsr DONE
    dey
    lda (zpLINE),Y
    cmp #':'
    beq SECEND

    lda zpLINE+1
    cmp #>BUFFER
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

    jsr AYACC
    jsr TRJOBA

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
    jmp CLRSTK

; IF numeric
; ==========
IFE:
    jmp LETM

IF:
    jsr AEEXPR
    beq IFE
    bpl IFX

    jsr IFIX
IFX:
    ldy zpAECUR
    sty zpCURSOR
    lda zpIACC
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq ELSE

    cpx #tknTHEN
    beq THEN

THENST:
    jmp STMT

THEN:
    inc zpCURSOR

THENLN:
    jsr SPTSTN
    bcc THENST
    jsr GOTGO
    jsr SECUR
    jmp GODONE

ELSE:
    ldy zpCURSOR        ; try to find else clause

ELSELP:
    lda (zpLINE),Y
    cmp #$0D
    beq ENDED

    iny
    cmp #tknELSE
    bne ELSELP

    sty zpCURSOR
    beq THENLN

ENDED:
    jmp ENDEDL

TRJOBA:
    lda zpIACC
    cmp zpTRNUM
    lda zpIACC+1
    sbc zpTRNUM+1
    bcs NOTRDE

    lda #'['
    jsr CHOUT
    jsr POSITE
    lda #']'
    jsr CHOUT
    jmp LISTPT

; ----------------------------------------------------------------------------

; Print 16-bit decimal number
; ===========================
POSITE:
    lda #$00          ; No padding
    beq NPRN+2        ; skip lda #5

NPRN:
    lda #$05          ; Pad to five characters
    sta zpPRINTS
    ldx #$04

NUMLOP:
    lda #$00
    sta zpWORK+8,X
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
    bpl NUMLOP

    ldx #$05
LZB:
    dex
    beq LASTZ
    lda zpWORK+8,X
    beq LZB

LASTZ:
    stx zpWORK
    lda zpPRINTS
    beq PLUME

    sbc zpWORK      ; carry clear
    beq PLUME

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

PLUME:
    lda zpWORK+8,X
    ora #$30
    jsr CHOUT
    dex
    bpl PLUME

    rts

; ----------------------------------------------------------------------------

; Low bytes of powers of ten
VALL:
    dta 1, 10, 100, <1000, <10000

; ----------------------------------------------------------------------------

; Line Search, find line number in IACC
FNDLNO:
    ldy #$00
    sty zpWORK+6
    lda zpTXTP
    sta zpWORK+7

SIGHT:
    ldy #$01
    lda (zpWORK+6),Y
    cmp zpIACC+1
    bcs LOOK

LOOKR:
    ldy #$03
    lda (zpWORK+6),Y
    adc zpWORK+6
    sta zpWORK+6
    bcc SIGHT

    inc zpWORK+7
    bcs SIGHT

LOOK:
    bne PAST

    ldy #$02
    lda (zpWORK+6),Y
    cmp zpIACC
    bcc LOOKR
    bne PAST

    tya
    adc zpWORK+6
    sta zpWORK+6
    bcc PAST

    inc zpWORK+7
    clc
PAST:
    ldy #$02
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

DIVOP:              ; divide with remainder
    tay
    jsr INTEGB
    lda zpIACC+3
    pha
    jsr ABSCOM
    jsr PHPOW

    stx zpTYPE
    tay
    jsr INTEGB

    pla
    sta zpWORK+1
    eor zpIACC+3
    sta zpWORK
    jsr ABSCOM

    ldx #zpWORK+2
    jsr POPX        ; pop from stack into zpWORK+2 ... zpWORK+5

    sty zpWORK+6    ; clear dword
    sty zpWORK+7
    sty zpWORK+8
    sty zpWORK+9

    lda zpIACC+3    ; check divisor
    ora zpIACC
    ora zpIACC+1
    ora zpIACC+2
    beq ZDIVOR      ; Divide by 0 error

    ldy #$20

DIVJUS:
    dey
    beq DIVRET
    asl zpWORK+2
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    bpl DIVJUS

DIVER:
    rol zpWORK+2
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    rol zpWORK+6
    rol zpWORK+7
    rol zpWORK+8
    rol zpWORK+9

    sec
    lda zpWORK+6
    sbc zpIACC
    pha

    lda zpWORK+7
    sbc zpIACC+1
    pha

    lda zpWORK+8
    sbc zpIACC+2
    tax

    lda zpWORK+9
    sbc zpIACC+3
    bcc NOSUB

    sta zpWORK+9
    stx zpWORK+8
    pla
    sta zpWORK+7
    pla
    sta zpWORK+6
    bcs NOSUB+2         ; skip pla, pla

NOSUB:
    pla
    pla
    dey
    bne DIVER

; Later BASICs have this check, disabled for now because it fails for
; Atom and System targets...
;    .if .hi(DIVER) != .hi(*)
;        .error "ASSERT: page crossing in DIVOP loop"
;    .endif

DIVRET:
    rts

; ----------------------------------------------------------------------------

FCOMPS:
    stx zpTYPE
    jsr POPACC
    jsr PHFACC
    jsr IFLT
    jsr FTOW
    jsr POPSET
    jsr FLDA
    jmp FCMPA

FCOMPR:
    jsr PHFACC
    jsr ADDER
    stx zpTYPE
    tay
    jsr FLOATI
    jsr POPSET

FCMP:
    jsr FLDW

; Compare FACC with FWRK
; ----------------------
FCMPA:
    ldx zpTYPE
    ldy #$00
    lda zpFWRKS
    and #$80
    sta zpFWRKS
    lda zpFACCS
    and #$80
    cmp zpFWRKS
    bne FCMPZ

    lda zpFWRKX
    cmp zpFACCX
    bne FCMPZZ

    lda zpFWRKMA
    cmp zpFACCMA
    bne FCMPZZ

    lda zpFWRKMB
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
    ror
    eor zpFWRKS
    rol
    lda #$01
    rts

COMPRE:
    jmp LETM         ; Jump to 'Type mismatch' error

; Evaluate next expression and compare with previous
; --------------------------------------------------
COMPR:
    txa

COMPRP1:
    beq STNCMP        ; Jump if current is string
    bmi FCOMPR        ; Jump if current is float

    jsr PHACC         ; Stack integer
    jsr ADDER
    tay               ; Evaluate next expression
    beq COMPRE        ; Error if string
    bmi FCOMPS        ; Float, jump to compare floats

; Compare IACC with top of stack
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
    jsr PHSTR
    jsr ADDER

    tay
    bne COMPRE

    stx zpWORK
    ldx zpCLEN
    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00
    .endif
    lda (zpAESTKP),Y
    sta zpWORK+2
    cmp zpCLEN
    bcs COMPRF

    tax
COMPRF:
    stx zpWORK+3

    .if version < 3 || (version == 3 && minorversion < 10)
        ldy #$00
    .endif

COMPRG:
    cpy zpWORK+3
    beq COMPRH

    iny
    lda (zpAESTKP),Y
    cmp STRACC-1,Y
    beq COMPRG
    bne COMPRI

COMPRH:
    lda zpWORK+2
    cmp zpCLEN

COMPRI:
    php
    jsr POPSTX
    ldx zpWORK
    plp
    rts


; EXPRESSION EVALUATOR
; ====================

; Evaluate expression at PtrA
; ---------------------------
AEEXPR:
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

; EXPR reads a rhs. If status is eq then it returned a string.
; If status is neq and plus then it returned a word.
; If status is neq and minus then it returned an fp.

EXPR:
    jsr ANDER         ; Call Evaluator Level 6 - AND
                      ; Returns A=type, value in IACC/FPA/StrA, X=next char
EXPRQ:
    cpx #tknOR
    beq OR            ; Jump if next char is OR

    cpx #tknEOR
    beq EOR_          ; Jump if next char is EOR

    dec zpAECUR       ; Step PtrB back to last char
    tay
    sta zpTYPE
    rts               ; Set flags from type, store type in $27 and return

; OR numeric
; ----------
OR:
    jsr PHANDR
    tay               ; Stack as integer, call Evaluator Level 6
    jsr INTEGB

    ldy #$03          ; If float, convert to integer
ORLP:
    lda (zpAESTKP),Y
    ora zpIACC,Y      ; OR IACC with top of stack    ; abs,y (!)
    sta zpIACC,Y      ; abs,y (!)
    dey
    bpl ORLP          ; Store result in IACC

EXPRP:
    jsr POPINC        ; Drop integer from stack
    lda #$40
    bne EXPRQ         ; Return type=Int, check for more OR/EOR, branch always

; EOR numeric
; -----------
EOR_:
    jsr PHANDR
    tay
    jsr INTEGB

    ldy #$03          ; If float, convert to integer
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
    jsr INTEGB        ; convert to integer
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
    jsr INTEGB
    jsr PHACC         ; If float, convert to integer, push onto stack
    jsr RELATE        ; Call Evaluator Level 5, < <= = >= > <>

    tay
    jsr INTEGB

    ldy #$03          ; If float, convert to integer
ANDLP:
    lda (zpAESTKP),Y
    and zpIACC,Y      ; AND IACC with top of stack   ; abs,y (!)
    sta zpIACC,Y      ; abs,y (!)
    dey
    bpl ANDLP         ; Store result in IACC

    jsr POPINC        ; Drop integer from stack
    lda #$40
    bne ANDERQ        ; Return type=Int, jump to check for another AND

; Evaluator Level 5 - >... =... or <...
; -------------------------------------
RELATE:
    jsr ADDER         ; Call Evaluator Level 4, + -
    cpx #'>'+1
    bcs RELATX         ; Larger than '>', return

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
    lda #$40
    rts               ; Return type=Int

; < <= <>
; -------
RELTLT:               ; RELate Less Than
    tax
    ldy zpAECUR
    lda (zpAELINE),Y  ; Get next char from zpAELINE
    cmp #'='
    beq LTOREQ         ; Jump for <=

    cmp #'>'
    beq NEQUAL         ; Jump for <>

; Must be < numeric
; -----------------
    jsr COMPR         ; test less than, evaluate next and compare
    bcc PASS          ; Jump to return TRUE if <
    bcs FAIL          ; return FALSE if not <

; <= numeric
; ----------
LTOREQ:
    inc zpAECUR
    jsr COMPR         ; Step past '=', evaluate next and compare
    beq PASS
    bcc PASS          ; Jump to return TRUE if =, TRUE if <
    bcs FAIL          ; Jump to return FALSE otherwise

; <> numeric
; ----------
NEQUAL:
    inc zpAECUR
    jsr COMPR         ; Step past '>', evaluate next and compare
    bne PASS
    beq FAIL          ; Jump to return TRUE if <>, FALSE if =

; > >=
; ----
RELTGT:               ; RELate Greater Than
    tax
    ldy zpAECUR
    lda (zpAELINE),Y  ; Get next char from zpAELINE
    cmp #'='
    beq GTOREQ         ; Jump for >=

; > numeric
; ---------
    jsr COMPR         ; test greater than, evaluate next and compare
    beq FAIL
    bcs PASS          ; Jump to return FALSE if =, TRUE if >
    bcc FAIL          ; Jump to return FALSE if <

; >= numeric
; ----------
GTOREQ:
    inc zpAECUR
    jsr COMPR         ; Step past '=', evaluate next and compare
    bcs PASS
    bcc FAIL          ; Jump to return TRUE if >=, FALSE if <
                      ; branch always

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

    jsr LBDCB         ; Unstack string to start of string buffer

    pla
    sta zpCLEN
    ldx zpWORK        ; Set new string length
    tya
    beq ADDERQ        ; Set type=string, jump to check for more + or -

; Evaluator Level 4, + -
; ----------------------
ADDER:
    jsr TERM         ; Call Evaluator Level 3, * / DIV MOD

ADDERQ:
    cpx #'+'
    beq PLUS         ; Jump with addition

    cpx #'-'
    beq MINUS         ; Jump with subtraction

    rts               ; Return otherwise

; + <value>
; ---------
PLUS:
    tay
    beq STNCON        ; Jump if current value is a string
    bmi FPLUS         ; Jump if current value is a float

; Integer addition
; ----------------
    jsr PHTERM         ; Stack current and call Evaluator Level 3
    tay
    beq ADDERE         ; If int + string, jump to 'Type mismatch' error
    bmi FPLUST         ; If int + float, jump ...

    ldy #$00
    clc
    lda (zpAESTKP),Y
    adc zpIACC          ; Add top of stack to IACC
    sta zpIACC          ; Store result in IACC
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

; Real addition
; -------------
FPLUS:
    jsr PHFACC
    jsr TERM           ; Stack float, call Evaluator Level 3
    tay
    beq ADDERE         ; float + string, jump to 'Type mismatch' error
    stx zpTYPE
    bmi FPLUSS          ; float + float, skip conversion

    jsr IFLT           ; float + int, convert int to float

FPLUSS:
    jsr POPSET         ; Pop float from stack, point FPTR to it
    jsr FADD           ; Unstack float to FPA2 and add to FPA1

FFFFA:
    ldx zpTYPE         ; Get nextchar back
    lda #$FF
    bne ADDERQ         ; Set result=float, loop to check for more + or -

; int + float
; -----------
FPLUST:
    stx zpTYPE
    jsr POPACC         ; Unstack integer to IACC
    jsr PHFACC
    jsr IFLT           ; Stack float, convert integer in IACC to float in FPA1
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

; Real subtraction
; ----------------
FMINUS:
    jsr PHFACC
    jsr TERM           ; Stack float, call Evaluator Level 3

    tay
    beq ADDERE         ; float - string, jump to 'Type mismatch' error
    stx zpTYPE
    bmi FMINUR         ; float - float, skip conversion

    jsr IFLT           ; float - int, convert int to float

FMINUR:
    jsr POPSET         ; Pop float from stack and point FPTR to it
    jsr FXSUB          ; Unstack float to FWRK and subtract it from FACC
    jmp FFFFA          ; Jump to set result and loop for more + or -

; int - float
; -----------
FMINUT:
    stx zpTYPE
    jsr POPACC         ; Unstack integer to IACC
    jsr PHFACC
    jsr IFLT           ; Stack float, convert integer in IACC to float in FACC
    jsr POPSET         ; Pop float from stack, point FPTR to it
    jsr FSUB           ; Subtract FPTR float from FPA1 float
    jmp FFFFA          ; Jump to set result and loop for more + or -

FTIMLF:
    jsr IFLT

FTIML:
    jsr POPACC
    jsr PHFACC
    jsr IFLT
    jmp FTIMR

FTIMFL:
    jsr IFLT

FTIM:
    jsr PHFACC
    jsr POWER
    stx zpTYPE
    tay
    jsr FLOATI

FTIMR:
    jsr POPSET
    jsr FMUL
    lda #$FF
    ldx zpTYPE
    jmp TERMQ

FTIME:
    jmp LETM

; * <value>
; ---------
TIMES:
    tay
    beq FTIME         ; If current value is string, jump to error
    bmi FTIM          ; Jump if current valus ia a float

    lda zpIACC+3
    cmp zpIACC+2
    bne FTIMFL

    tay
    beq TIMESA
    cmp #$FF
    bne FTIMFL

TIMESA:
    eor zpIACC+1
    bmi FTIMFL

    jsr PHPOW

    stx zpTYPE
    tay
    beq FTIME
    bmi FTIML

    lda zpIACC+3
    cmp zpIACC+2
    bne FTIMLF

    tay
    beq TIMESB

    cmp #$FF
    bne FTIMLF

TIMESB:
    eor zpIACC+1
    bmi FTIMLF

    lda zpIACC+3
    pha
    jsr ABSCOM

    ldx #zpWORK+2
    jsr ACCTOM
    jsr POPACC

    pla
    eor zpIACC+3
    sta zpWORK
    jsr ABSCOM

    ldy #$00
    ldx #$00
    sty zpWORK+8
    sty zpWORK+9

NUL:
    lsr zpWORK+3
    ror zpWORK+2
    bcc NAD

    clc
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
    asl zpIACC
    rol zpIACC+1
    rol zpIACC+2
    rol zpIACC+3
    lda zpWORK+2
    ora zpWORK+3
    bne NUL

    sty zpWORK+6
    stx zpWORK+7
    lda zpWORK
    php

REMIN:
    ldx #zpWORK+6

DIVIN:
    jsr MTOACC

    plp
    bpl TERMA

    jsr COMPNO

TERMA:
    ldx zpTYPE
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
    jsr POWER         ; Call Evaluator Level 2, ^

TERMQ:
    cpx #'*'
    beq TIMESJ        ; Jump with multiply

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
    jsr PHFACC
    jsr POWER          ; Stack float, call Evaluator Level 2

    stx zpTYPE
    tay
    jsr FLOATI         ; Ensure current value is real
    jsr POPSET
    jsr FXDIV          ; Unstack to FPTR, call divide routine

    ldx zpTYPE
    lda #$FF
    bne TERMQ; Set result, loop for more * / MOD DIV

; MOD <value>
; -----------
REMAIN:
    jsr DIVOP         ; Ensure current value is integer
    lda zpWORK+1
    php
    jmp REMIN         ; Jump to MOD routine

; DIV <value>
; -----------
INTDIV:
    jsr DIVOP         ; Ensure current value is integer
    rol zpWORK+2      ; Multiply by 2
    rol zpWORK+3
    rol zpWORK+4
    rol zpWORK+5
    bit zpWORK
    php
    ldx #zpWORK+2
    jmp DIVIN         ; Jump to DIV routine

; ----------------------------------------------------------------------------

; Stack current integer and evaluate another Level 2
; --------------------------------------------------
PHPOW:
    jsr PHACC         ; Stack integer

; Evaluator Level 2, ^
; --------------------
POWER:
    jsr FACTOR         ; Call Evaluator Level 1, - + NOT function ( ) ? ! $ | "

POWERB:
    pha

POWERA:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y      ; Get character
    cmp #' '
    beq POWERA            ; Skip spaces

    tax
    pla
    cpx #'^'
    beq POW

    rts               ; Return if not ^

; ^ <value>
; ---------
POW:
    tay
    jsr FLOATI         ; Ensure current value is a float
    jsr PHFACC
    jsr FLTFAC         ; Stack float, evaluate a real

    lda zpFACCX
    cmp #$87
    bcs FPOWA          ; abs(n) >= 64

    jsr FFRAC
    bne FPOWE

    jsr POPSET
    jsr FLDA

    lda zpFQUAD
    jsr FIPOW

    lda #$FF
    bne POWERB         ; Set result=real, loop to check for more ^

FPOWE:
    jsr STARGC

    lda zpAESTKP
    sta zpARGP
    lda zpAESTKP+1
    sta zpARGP+1
    jsr FLDA

    lda zpFQUAD
    jsr FIPOW

FPOWC:
    jsr STARGB
    jsr POPSET
    jsr FLDA
    jsr FLOG
    jsr ACMUL
    jsr FEXP
    jsr ARGB
    jsr FMUL
    lda #$FF
    bne POWERB         ; Set result=real, loop to check for more ^

FPOWA:
    jsr STARGC
    jsr FONE
    bne FPOWC          ; branch always

; ----------------------------------------------------------------------------

; Convert number to hex string in STRACC
; --------------------------------------
FCONHX:
    tya
    bpl FCONHF
    jsr IFIX         ; Convert real to integer

FCONHF:
    ldx #$00
    ldy #$00

HEXPLP:
    lda zpIACC,Y      ; abs,y (!)
    pha               ; Expand four bytes into eight digits
    and #$0F
    sta zpWORK+8,X
    pla
    lsr
    lsr
    lsr
    lsr
    inx
    sta zpWORK+8,X
    inx
    iny
    cpy #$04
    bne HEXPLP         ; Loop for four bytes

HEXLZB:
    dex
    beq HEXP          ; No digits left, output a single zero

    lda zpWORK+8,X
    beq HEXLZB        ; Skip leading zeros

HEXP:
    lda zpWORK+8,X
    cmp #$0A
    bcc NOTHX         ; less than 10

    adc #$06          ; >= 10, convert byte to hex (A-F after '0' is added)

NOTHX:
    adc #'0'          ; to ASCII
    jsr CHTOBF         ; store in buffer
    dex
    bpl HEXP

    rts               ; Loop for all digits

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
; On exit,  StrA contains string version of number
;           zpCLEN=string length
;
FCON:
    ldx VARL_AT+2      ; Get format byte, flag forcing E
    cpx #$03
    bcc FCONOK         ; If <3, ok - use it

    ldx #$00          ; If invalid, $00 for General format
FCONOK:
    stx zpWORK          ; Store format type
    lda VARL_AT+1
    beq FCONC         ; If digits=0, jump to check format

    cmp #$0A
    bcs FCONA         ; If 10+ digits, jump to use 10 digits

;  In G,E formats varl is no. of sig figs >0
;  In F format it is no. of decimals and can e >=0

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
    sta zpFDIGS        ; Store digit length
    lda #$00
    sta zpCLEN
    sta zpFPRTDX       ; Set initial output length to 0, initial exponent to 0
    bit zpPRINTF
    bmi FCONHX         ; Jump for hex conversion if $15.b7 set

    tya
    bmi FCONFX

    jsr IFLT           ; Convert integer to real

FCONFX:
    jsr FTST
    bne FPRTA     ; Get -1/0/+1 sign, jump if not zero to output nonzero number

    lda zpWORK
    bne FPRTHJ    ; If not General format, output fixed or exponential zero

    lda #'0'
    jmp CHTOBF    ; Store single '0' into string buffer and return

FPRTHJ:
    jmp FPRTHH     ; Jump to output zero in fixed or exponential format

FPRTEE:
    jsr FONE
    bne FPRTEP3    ; FACC=1.0

; FACC now is >=1, check that it is <10
; -------------------------------------
FPRTD:
    cmp #$84        ; exponent of 9.99
    bcc FPRTF       ; 1.0 to 7.999999 all OK
    bne FPRTE       ; exponent 85 or more
    lda zpFACCMA    ; fine check if exponent=84
    cmp #$A0
    bcc FPRTF       ; 8.0000 to 9.9999

FPRTE:
    jsr FTENFQ      ; divide FACC by 10.0

FPRTEP3:
    inc zpFPRTDX
    jmp FPRTC         ; Jump back to get the number >=1 again

; FloatA is now between 1 and 9.999999999
; ---------------------------------------
FPRTF:
    lda zpFACCMG
    sta zpTYPE
    jsr STARGA         ; Copy FloatA to FloatTemp at $27/$046C

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

    lda #$0A
    sta zpWORK+1
    lda #$00
    sta zpWORK        ; treat as G format

FPRTFH:
    jsr FCLR         ; Clear FloatA

    lda #$A0
    sta zpFACCMA
    lda #$83
    sta zpFACCX       ;  5.0 --> FACC
    ldx zpWORK+1
    beq FPRTGJ

FPRTGG:
    jsr FTENFQ        ; divide FACC by 10.0
    dex
    bne FPRTGG        ; create .00,,005 const

FPRTGJ:
    jsr ARGA         ; Point to workspace FP temp A
    jsr FLDW         ; Unpack to FWRK

    lda zpTYPE
    sta zpFWRKMG
    jsr FADDW1       ; Add

FPRTFF:
    lda zpFACCX
    cmp #$84
    bcs FPRTG

    ror zpFACCMA     ; could call end of FTENFX
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG
    inc zpFACCX
    bne FPRTFF

FPRTG:
    lda zpFACCMA
    cmp #$A0         ; see if unnormalized
    bcs FPRTEE       ; fix up if so
    lda zpWORK+1
    bne FPRTH

; Output zero in Exponent or Fixed format
; ---------------------------------------
FPRTHH:
    cmp #$01
    beq FPRTK

FPRTZR:
    jsr FCLR         ; Clear FACC
    lda #$00
    sta zpFPRTDX
    lda zpFDIGS
    sta zpWORK+1
    inc zpWORK+1

;  The exponent is $84, so the top digit of FACC is the first digit to print

FPRTH:
    lda #$01
    cmp zpWORK
    beq FPRTK

    ldy zpFPRTDX
    bmi FPRTKK

    cpy zpWORK+1
    bcs FPRTK       ; use scientific is <1.0 or > 10^digits

    lda #$00
    sta zpFPRTDX    ; use F type format
    iny
    tya
    bne FPRTK

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
    inc zpFPRTDX
    beq FPRTKN

    jsr CHTOBF      ; Output
    bne FPRTKM

FPRTKN:
    lda #$80

FPRTK:
    sta zpFPRTWN

FPRTI:
    jsr FPRTNN
    dec zpFPRTWN
    bne FPRTL

    lda #'.'
    jsr CHTOBF

FPRTL:
    dec zpWORK+1
    bne FPRTI

    ldy zpWORK
    dey
    beq FPRTTX

    dey
    beq FPRTTYp2

    ldy zpCLEN
FPRTTZ:
    dey
    lda STRACC,Y
    cmp #'0'
    beq FPRTTZ

    cmp #'.'
    beq FPRTTY

    iny
FPRTTY:
    sty zpCLEN
FPRTTYp2:
    lda zpFPRTDX
    beq FPRTX          ; exponent=0

FPRTTX:
    lda #'E'
    jsr CHTOBF         ; Output 'E'

    lda zpFPRTDX
    bpl FPRTJ

    lda #'-'
    jsr CHTOBF         ; Output '-'

    sec
    lda #$00
    sbc zpFPRTDX       ; Negate

FPRTJ:
    jsr IPRT
    lda zpWORK
    beq FPRTX

    lda #' '
    ldy zpFPRTDX
    bmi FPRTTW

    jsr CHTOBF
FPRTTW:
    cpx #$00
    bne FPRTX
    jmp CHTOBF

FPRTX:
    rts

FPRTNN:
    lda zpFACCMA
    lsr
    lsr
    lsr
    lsr
    jsr FPRTDG

    lda zpFACCMA
    and #$0F
    sta zpFACCMA
    jmp FTENX

; ----------------------------------------------------------------------------

; Print AC in decimal unsigned

IPRT:
    ldx #$FF
    sec

IPRTA:
    inx
    sbc #$0A
    bcs IPRTA

    adc #$0A
    pha
    txa
    beq IPRTB

    jsr FPRTDG

IPRTB:
    pla

FPRTDG:
    ora #'0'

; Store character in string buffer
; --------------------------------
CHTOBF:
    stx zpWORK+4
    ldx zpCLEN
    sta STRACC,X    ; Store character
    ldx zpWORK+4
    inc zpCLEN
    rts               ; Increment string length

; ----------------------------------------------------------------------------

; READ ROUTINES

FRDDXX:
    clc
    stx zpFACCMG
    jsr FTST
    lda #$FF
    rts

; Scan decimal number
; -------------------
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

    inc zpFRDDDP      ; Set decimal point flag
    bne FRDDC         ; loop for next

FRDDD:
    cmp #'E'
    beq FRDDEX         ; Jump to scan exponent

    cmp #'9'+1
    bcs FRDDQ         ; Not a digit, jump to finish

    sbc #'0'-1
    bcc FRDDQ         ; Not a digit, jump to finish, end of number

    ldx zpFACCMA      ; Get mantissa top byte
    cpx #$18
    bcc FRDDE         ; If <25, still small enough to add to

    ldx zpFRDDDP
    bne FRDDC         ; Decimal point found, skip digits to end of number

    inc zpFRDDDX      ; Otherwise, increment tens
    bcs FRDDC         ; and skip digits

FRDDE:
    ldx zpFRDDDP
    beq FRDDF         ; No . yet

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
; ------------------------------------
FRDDEX:
    jsr IRDD          ; Scan following number
    adc zpFRDDDX      ; Add to current exponent
    sta zpFRDDDX

; End of number found
; -------------------
FRDDQ:
    sty zpAECUR       ; Store AELINE offset
    lda zpFRDDDX
    ora zpFRDDDP      ; Check exponent and 'decimal point' flag
    beq FRINT         ; No exponent, no decimal point, return integer

    jsr FTST          ; was it zero?
    beq FRDDZZ        ; if so, exit at once

FRFP:
    lda #$A8
    sta zpFACCX
    lda #$00
    sta zpFACCXH
    sta zpFACCS
    jsr FNRM

; Now I have to MUL or DIV by power of ten given in zpFRDDDX

    lda zpFRDDDX
    bmi FRDDM
    beq FRDDZ

FRDDP:
    jsr FTENFX          ; times 10.0
    dec zpFRDDDX
    bne FRDDP
    beq FRDDZ

FRDDM:
    jsr FTENFQ          ; divider 10.0
    inc zpFRDDDX
    bne FRDDM

FRDDZ:
    jsr FTIDY           ; round, check overflow

FRDDZZ:
    sec
    lda #$FF
    rts

FRINT:
    lda zpFACCMB
    sta zpIACC+3
    and #$80
    ora zpFACCMA
    bne FRFP

    lda zpFACCMG
    sta zpIACC
    lda zpFACCMD
    sta zpIACC+1
    lda zpFACCMC
    sta zpIACC+2

    lda #$40
    sec
    rts

IRDDB:
    jsr IRDDC         ; Scan following number
    eor #$FF
    sec
    rts               ; Negate it, return CS=Ok

; Scan exponent, allows E E+ E- followed by one or two digits
; -----------------------------------------------------------
IRDD:
    iny
    lda (zpAELINE),Y  ; Get next character
    cmp #'-'
    beq IRDDB         ; If '-', jump to scan and negate

    cmp #'+'
    bne IRDDA         ; If '+', just step past

IRDDC:
    iny
    lda (zpAELINE),Y  ; Get next character

IRDDA:
    cmp #'9'+1
    bcs IRDDOW        ; Not a digit, exit with CC and A=0

    sbc #'0'-1
    bcc IRDDOW        ; Not a digit, exit with CC and A=0

    sta zpFRDDW       ; Store exponent digit
    iny
    lda (zpAELINE),Y  ; Get next character
    cmp #'9'+1
    bcs IRDDQ         ; Not a digit, exit with CC and A=exp

    sbc #'0'-1
    bcc IRDDQ         ; Not a digit, exit with CC and A=exp

    iny
    sta zpFTMPMA      ; Step past digit, store current digit
    lda zpFRDDW       ; Get current exponent
    asl
    asl               ; exp *= 4
    adc zpFRDDW       ; exp += exp (total *5
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

; Multiply FACC MANTISSA by 10

FTENX:
    pha
    ldx zpFACCMD
    lda zpFACCMA
    pha
    lda zpFACCMB
    pha
    lda zpFACCMC
    pha
    lda zpFACCMG
    asl
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    asl
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    adc zpFACCMG
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
    asl zpFACCMG
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol
    sta zpFACCMA
    pla
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

    lda zpFACCS
    bne FTSTR

    lda #$01        ; non-zero if sign byte
    rts

FTSTZ:
    sta zpFACCS
    sta zpFACCX
    sta zpFACCXH
FTSTR:
    rts

; ----------------------------------------------------------------------------

; FACC times 10.0
;
;   FX:=FX+3
;   FW:=FACC
;   FW:=FW>>2
;   FACC:=FACC+FW
;   IF CARRY THEN {
;     FACC:=FACC>>1;
;     FX:=FX+1 }

FTENFX:
    clc
    lda zpFACCX
    adc #$03
    sta zpFACCX
    bcc FTENFA

    inc zpFACCXH

FTENFA:
    jsr FTOW        ; copy to FWRK
    jsr FASRW
    jsr FASRW

FPLWF:
    jsr FPLW

;   CY is set on carry out of FACCMA and FWRKMA

FRENRM:
    bcc FRENX
    ror zpFACCMA
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG
    inc zpFACCX
    bne FRENX

    inc zpFACCXH

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
    lda zpFACCX
    sbc #$04
    sta zpFACCX
    bcs FTENB

    dec zpFACCXH
FTENB:
    jsr FTOWAS
    jsr FPLWF           ; * 0.00011
    jsr FTOWAS
    jsr FASRW
    jsr FASRW
    jsr FASRW
    jsr FPLWF           ; * 0.000110011

    lda #$00
    sta zpFWRKMA
    lda zpFACCMA
    sta zpFWRKMB
    lda zpFACCMB
    sta zpFWRKMC
    lda zpFACCMC
    sta zpFWRKMD
    lda zpFACCMD
    sta zpFWRKMG

    lda zpFACCMG
    rol                 ; set carry bit properly
    jsr FPLWF           ; OK to 16 bits
    lda #$00
    sta zpFWRKMA        ; later BASICs skip this, because FWRKMA is already 0
    sta zpFWRKMB

    lda zpFACCMA
    sta zpFWRKMC
    lda zpFACCMB
    sta zpFWRKMD
    lda zpFACCMC
    sta zpFWRKMG

    lda zpFACCMD
    rol
    jsr FPLWF
    lda zpFACCMB
    rol
    lda zpFACCMA

FPLNF:
    adc zpFACCMG
    sta zpFACCMG
    bcc FPLNY

    inc zpFACCMD
    bne FPLNY

    inc zpFACCMC
    bne FPLNY

    inc zpFACCMB
    bne FPLNY

    inc zpFACCMA
    bne FPLNY

    jmp FRENRM

FPLNY:
    rts

; ----------------------------------------------------------------------------

IFLT:
    ldx #$00
    stx zpFACCMG
    stx zpFACCXH
    lda zpIACC+3
    bpl IFLTA

    jsr COMPNO
    ldx #$FF
IFLTA:
    stx zpFACCS
    lda zpIACC
    sta zpFACCMD
    lda zpIACC+1
    sta zpFACCMC
    lda zpIACC+2
    sta zpFACCMB
    lda zpIACC+3
    sta zpFACCMA
    lda #$A0
    sta zpFACCX
    jmp FNRM

; ----------------------------------------------------------------------------

FNRMZ:
    sta zpFACCS
    sta zpFACCX
    sta zpFACCXH

FNRMX:
    rts

FLTACC:
    pha
    jsr FCLR
    pla
    beq FNRMX       ; done if 0.0
    bpl FLTAA       ; >0.0

    sta zpFACCS
    lda #$00
    sec
    sbc zpFACCS

FLTAA:
    sta zpFACCMA
    lda #$88
    sta zpFACCX

; FNRM normalizes the FACC using 16 bit exponent, so no worry about
; exponent overflow

FNRM:
    lda zpFACCMA
    bmi FNRMX

    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    ora zpFACCMG
    beq FNRMZ

    lda zpFACCX
FNRMA:
    ldy zpFACCMA
    bmi FNRMX
    bne FNRMC

    ldx zpFACCMB
    stx zpFACCMA
    ldx zpFACCMC
    stx zpFACCMB
    ldx zpFACCMD
    stx zpFACCMC
    ldx zpFACCMG
    stx zpFACCMD
    sty zpFACCMG
    sec
    sbc #$08
    sta zpFACCX
    bcs FNRMA

    dec zpFACCXH
    bcc FNRMA

FNRMB:
    ldy zpFACCMA
    bmi FNRMX           ; fully normalized

FNRMC:
    asl zpFACCMG
    rol zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    sbc #$00
    sta zpFACCX
    bcs FNRMB

    dec zpFACCXH
    bcc FNRMB           ; branch always

; ----------------------------------------------------------------------------

; FLDW -- Load FWRK via zpARGP

FLDW:
    ldy #$04
    lda (zpARGP),Y
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
    sty zpFWRKMG
    sty zpFWRKXH
    lda (zpARGP),Y
    sta zpFWRKX
    ora zpFWRKS
    ora zpFWRKMB
    ora zpFWRKMC
    ora zpFWRKMD
    beq FLDWX

    lda zpFWRKS
    ora #$80
FLDWX:
    sta zpFWRKMA
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
    lda #>FWSA   ; MSB of all FWS / FP TEMP variables
    sta zpARGP+1

FSTA:
    ldy #$00
    lda zpFACCX
    sta (zpARGP),Y

    iny
    lda zpFACCS            ; tidy up sign bit
    and #$80
    sta zpFACCS
    lda zpFACCMA
    and #$7F
    ora zpFACCS
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
    jsr ARGA

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

    sty zpFACCMG
    sty zpFACCXH
    ora zpFACCS
    ora zpFACCMB
    ora zpFACCMC
    ora zpFACCMD
    beq FLDAX

    lda zpFACCS
    ora #$80
FLDAX:
    sta zpFACCMA
    rts

; ----------------------------------------------------------------------------

; Convert real to integer
; =======================
IFIX:
    jsr FFIX         ; Convert real to integer

COPY_FACC_TO_IACC:
    lda zpFACCMA
    sta zpIACC+3          ; Copy to Integer Accumulator
    lda zpFACCMB
    sta zpIACC+2
    lda zpFACCMC
    sta zpIACC+1
    lda zpFACCMD
    sta zpIACC
    rts

; FFIX converts float to integer
; ==============================
; On entry, FACCX-FACCMD ($30-$34) holds a float
; On exit,  FACCX-FACCMD ($30-$34) holds integer part
; ---------------------------------------------------
; The real value is partially denormalised by repeatedly dividing the mantissa
; by 2 and incrementing the exponent to multiply the number by 2, until the
; exponent is $80, indicating that we have got to mantissa * 2^0.
; Truncates towards zero.

FFIXQ:
    jsr FTOW         ; Copy FloatA to FloatB
    jmp FCLR         ; Set FloatA to zero and return

FFIX:
    lda zpFACCX
    bpl FFIXQ         ; Exponent<$80, number<1, jump to return 0

    jsr FCLRW         ; Set $3B-$42 to zero
    jsr FTST
    bne FFIXG         ; Always shift at least once
    beq FFIXY         ; except for 0

FFIXB:
    lda zpFACCX    ; Get exponent
    cmp #$A0
    bcs FFIXC      ; Exponent is +32, float has been denormalised to an integer

    cmp #$99
    bcs FFIXG      ; Loop to keep dividing

    adc #$08
    sta zpFACCX    ; Increment exponent by 8
    lda zpFWRKMC
    sta zpFWRKMD
    lda zpFWRKMB
    sta zpFWRKMC
    lda zpFWRKMA
    sta zpFWRKMB
    lda zpFACCMD
    sta zpFWRKMA
    lda zpFACCMC
    sta zpFACCMD          ; Divide mantissa by 2^8
    lda zpFACCMB
    sta zpFACCMC
    lda zpFACCMA
    sta zpFACCMB
    lda #$00
    sta zpFACCMA
    beq FFIXB         ; Loop to keep dividing, branch always

FFIXG:
    lsr zpFACCMA
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFWRKMA
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD      ; rotate into FWRK
    inc zpFACCX
    bne FFIXB

; Here I have overflow

FFIXV:
    jmp FOVR

; ----------------------------------------------------------------------------

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
    bne FFIXV         ; Exponent>32, jump to 'Too big' error

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

; FFRAC sets FQUAD to the integer part of
; FACC, and FACCC to its fractional part. Returns with
; condition code set zero if fraction is zero.
; Assumes that on input FIX(FACC) < 128.

FFRAC:
    lda zpFACCX
    bmi FFRACA      ; normal case

    lda #$00
    sta zpFQUAD
    jmp FTST

FFRACA:
    jsr FFIX
    lda zpFACCMD
    sta zpFQUAD
    jsr FMWTOA

    lda #$80
    sta zpFACCX
    ldx zpFACCMA
    bpl FNEARN      ; fraction part < 0.5

    eor zpFACCS
    sta zpFACCS     ; change sign of fraction part
    bpl FNEARQ

    inc zpFQUAD
    jmp FNEARR

FNEARQ:
    dec zp4A

FNEARR:
    jsr FINEG       ; achieves fract = 1 - fract

FNEARN:
    jmp FNRM

FINC:
    inc zpFACCMD
    bne FNEARZ

    inc zpFACCMC
    bne FNEARZ

    inc zpFACCMB
    bne FNEARZ

    inc zpFACCMA
    beq FFIXV       ; overflow

FNEARZ:
    rts

FNEARP:
    jsr FINEG
    jsr FINC
    jmp FINEG

; ----------------------------------------------------------------------------

FSUB:
    jsr FXSUB
    jmp FNEG

FSWOP:
    jsr FLDW
    jsr FSTA

FWTOA:
    lda zpFWRKS
    sta zpFACCS
    lda zpFWRKXH
    sta zpFACCXH
    lda zpFWRKX
    sta zpFACCX

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

FXSUB:
    jsr FNEG

FADD:
    jsr FLDW
    beq FADDZ       ; A+0.0=A

FADDW:
    jsr FADDW1
    jmp FTIDY

FADDW1:
    jsr FTST        ; see if adding to 0
    beq FWTOA       ; load with FWRK

    ; Here I have non-trivial add

    ldy #$00
    sec
    lda zpFACCX
    sbc zpFWRKX
    beq FADDA
    bcc FADDB       ; X(FACC) < X(FWRK)

    cmp #$25
    bcs FADDZ       ; shift too large for significance

    pha
    and #$38
    beq FADDCA

    lsr
    lsr
    lsr
    tax

FADDCB:
    lda zpFWRKMD
    sta zpFWRKMG
    lda zpFWRKMC
    sta zpFWRKMD
    lda zpFWRKMB
    sta zpFWRKMC
    lda zpFWRKMA
    sta zpFWRKMB
    sty zpFWRKMA
    dex
    bne FADDCB

FADDCA:
    pla
    and #$07
    beq FADDA

    tax

FADDC:
    lsr zpFWRKMA
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD
    ror zpFWRKMG
    dex
    bne FADDC
    beq FADDA       ; alligned

FADDB:
    sec
    lda zpFWRKX
    sbc zpFACCX     ; amounto to shift FACC
    cmp #$25
    bcs FWTOA       ; FACC not significant

; Now shift FACC right

    pha
    and #$38
    beq FADDDA

    lsr
    lsr
    lsr
    tax

FADDDB:
    lda zpFACCMD
    sta zpFACCMG
    lda zpFACCMC
    sta zpFACCMD
    lda zpFACCMB
    sta zpFACCMC
    lda zpFACCMA
    sta zpFACCMB
    sty zpFACCMA
    dex
    bne FADDDB

FADDDA:
    pla
    and #$07
    beq FADDAL

    tax

FADDD:
    lsr zpFACCMA
    ror zpFACCMB
    ror zpFACCMC
    ror zpFACCMD
    ror zpFACCMG
    dex
    bne FADDD

FADDAL:
    lda zpFWRKX
    sta zpFACCX

FADDA:
    lda zpFACCS
    eor zpFWRKS
    bpl FADDE       ; both same sign

    lda zpFACCMA
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

    jmp FCLR        ; FACC=FWRK in difference case

FADDF:
    bcs FADDG       ; abs(FACC) > abs(FWRK)

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
    lda zpFWRKS
    sta zpFACCS
    jmp FNRM

FADDE:
    clc
    jmp FPLWF       ; add FWRK to FACC

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
    jmp FNRM

; ----------------------------------------------------------------------------

FMULZ:
    rts

IFMUL:
    jsr FTST
    beq FMULZ       ; 0.0 * something
    jsr FLDW        ; get other arg
    bne FMULA       ; non-zero, so real work
    jmp FCLR

FMULA:
    clc
    lda zpFACCX
    adc zpFWRKX     ; add exponents
    bcc FMULB

    inc zpFACCXH

    ; Subtract $80 bias from exponent, do not check over/underflow yet
    ; in case renormalisation fixes things

    clc
FMULB:
    sbc #$7F        ; carry subtracts extra 1
    sta zpFACCX
    bcs FMULC

    dec zpFACCXH

; Copy FACC to FTMP, clear FACC then I can do FACC:=FWRK*FTMP
; as a fixed point operation.

FMULC:
    ldx #$05
    ldy #$00        ; to preset FACC to 0.0

FMULD:
    lda zpFACCMA-1,X
    sta zpFTMPMA-1,X
    sty zpFACCMA-1,X
    dex
    bne FMULD

    lda zpFACCS
    eor zpFWRKS
    sta zpFACCS      ; get sign right

; Now for 1:32 do {
;   IF MSB(FTMP)=1 FACC:=FACC+FWRK
;   FTMP:=FTMP<<1
;   FWRK:=FWRK>>1 }

    ldy #$20
FMULE:
    lsr zpFWRKMA
    ror zpFWRKMB
    ror zpFWRKMC
    ror zpFWRKMD
    ror zpFWRKMG
    asl zpFTMPMD    ; FTMPG cannot affect answer
    rol zpFTMPMC
    rol zpFTMPMB
    rol zpFTMPMA
    bcc FMULF

    clc
    jsr FPLW

FMULF:
    dey
    bne FMULE
    rts

FMUL:
    jsr IFMUL

NRMTDY:
    jsr FNRM

FTIDY:
    lda zpFACCMG
    cmp #$80
    bcc FTRNDZ
    beq FTRNDA

    lda #$FF
    jsr FPLNF
    jmp FTRNDZ

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
    lda zpFACCMD
    ora #$01
    sta zpFACCMD

FTRNDZ:
    lda #$00
    sta zpFACCMG
    lda zpFACCXH
    beq FTIDYZ
    bpl FOVR

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

FONE:
    jsr FCLR
    ldy #$80
    sty zpFACCMA
    iny
    sty zpFACCX
    tya
    rts                 ; always return with !Z

; ----------------------------------------------------------------------------

; 1/X

FRECIP:
    jsr STARGA
    jsr FONE
    bne FDIV           ; branch always, FONE returns with !Z

; ----------------------------------------------------------------------------

FXDIV:
    jsr FTST
    beq FDIVZ

    jsr FTOW
    jsr FLDA
    bne FDIVA

    rts             ; result is zero

FDIVZ:
    jmp ZDIVOR      ; Divide by zero error

; ----------------------------------------------------------------------------

; =TAN numeric
; ============

; FTAN works as FSIN(X)/FCOS(X)

TAN:
    jsr FLTFAC
    jsr FRANGE

    lda zpFQUAD
    pha
    jsr ARGD
    jsr FSTA

    inc zpFQUAD

    jsr FSC
    jsr ARGD
    jsr FSWOP

    pla
    sta zpFQUAD

    jsr FSC
    jsr ARGD
    jsr FDIV

    lda #$FF
    rts

; ----------------------------------------------------------------------------

FDIV:
    jsr FTST
    beq FTIDYZ      ; 0.0/anything = 0.0 (including 0.0/0.0)

    jsr FLDW
    beq FDIVZ       ; divide by zero

FDIVA:
    lda zpFACCS
    eor zpFWRKS
    sta zpFACCS     ; sign correct
    sec
    lda zpFACCX
    sbc zpFWRKX     ; difference of exponents
    bcs FDIVB

    dec zpFACCXH
    sec
FDIVB:
    adc #$80        ; $81 because carry is set
    sta zpFACCX
    bcc FDIVC

    inc zpFACCXH
    clc
FDIVC:
    ldx #$20

FDIVE:
    bcs FDIVH

    lda zpFACCMA
    cmp zpFWRKMA
    bne FDIVF

    lda zpFACCMB
    cmp zpFWRKMB
    bne FDIVF

    lda zpFACCMC
    cmp zpFWRKMC
    bne FDIVF

    lda zpFACCMD
    cmp zpFWRKMD
FDIVF:
    bcc FDIVG       ; won't go

; FACC guard on basis of the comparison, carry already set for sbc

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
    sec

FDIVG:
    rol zpFTMPMD
    rol zpFTMPMC
    rol zpFTMPMB
    rol zpFTMPMA
    asl zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    dex
    bne FDIVE

    ldx #$07
FDIVJ:
    bcs FDIVL       ; now generate seven guard bits
    lda zpFACCMA
    cmp zpFWRKMA
    bne FDIVK

    lda zpFACCMB
    cmp zpFWRKMB
    bne FDIVK

    lda zpFACCMC
    cmp zpFWRKMC
    bne FDIVK

    lda zpFACCMD
    cmp zpFWRKMD
FDIVK:
    bcc FDIVM

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
    sec

FDIVM:
    rol zpFACCMG
    asl zpFACCMD
    rol zpFACCMC
    rol zpFACCMB
    rol zpFACCMA
    dex
    bne FDIVJ

    asl zpFACCMG
    lda zpFTMPMD
    sta zpFACCMD
    lda zpFTMPMC
    sta zpFACCMC
    lda zpFTMPMB
    sta zpFACCMB
    lda zpFTMPMA
    sta zpFACCMA

    jmp NRMTDY

; ----------------------------------------------------------------------------

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
SQR:
    jsr FLTFAC

FSQRT:
    jsr FTST
    beq FSQRTZ      ; SQRT(0.0) easy
    bmi FSQRTE      ; bad -ve

    jsr STARGA

    lda zpFACCX
    lsr
    adc #$40
    sta zpFACCX
    lda #$05
    sta zpFRDDW
    jsr ARGB

FSQRTA:
    jsr FSTA

    lda #<FWSA
    sta zpARGP
    jsr FXDIV

    lda #<FWSB
    sta zpARGP
    jsr FADD

    dec zpFACCX
    dec zpFRDDW
    bne FSQRTA

FSQRTZ:
    lda #$FF
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

;   FLOG sets FACC= LOG(FACC)
;   (base e). works by
;   (A) Check for acc <= 0.0
;   (B) Strip exponent to put FACC in range 1.0 - 2.0
;       and renormalize to .707 TO 1.414
;   (B2) Extra care with smallest possible exponent
;   (C) Approximate log using (x-1)+(x-1)^2*cf(x-1)
;       where cf is a minimax continued fraction
;   (D) Add result to exponent * LOG(2.0)
;   N.B. Result can not overflow so no worry there.
;   The series approximation used for LOGs is a continued
;   fraction: F(X)=C(0)+X/(C(1)+X/(...

; =LN numeric
; ===========
LN:
    jsr FLTFAC

FLOG:
    jsr FTST
    beq FLOGA   ; LOG(0) is illegal
    bpl FLOGB   ; LOG(>0.0) is OK

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
    jsr FCLRW

    ldy #$80
    sty zpFWRKS
    sty zpFWRKMA
    iny
    sty zpFWRKX         ; FWRK = -1

    ldx zpFACCX
    beq FLOGC

    lda zpFACCMA
    cmp #$B5
    bcc FLOGD

FLOGC:
    inx
    dey

FLOGD:
    txa
    pha
    sty zpFACCX
    jsr FADDW

    lda #<FWSD
    jsr FSTAP

    lda #<FLOGTC
    ldy #>FLOGTC
    jsr FCF         ; evaluate continued fraction
    jsr ARGD
    jsr FMUL
    jsr FMUL
    jsr FADD
    jsr STARGA      ; save partial result

    pla             ; recover exponent byte
    sec
    sbc #$81
    jsr FLTACC

    lda #<LOGTWO
    sta zpARGP
    lda #>LOGTWO
    sta zpARGP+1

    jsr FMUL
    jsr ARGA
    jsr FADD

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

; ----------------------------------------------------------------------------

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
    jsr STARGA          ; save x

    ldy #$00
    lda (zpCOEFP),Y     ; length of series - always downstairs
    sta zpFRDDDP
    inc zpCOEFP
    bne FCFA

    inc zpCOEFP+1
FCFA:
    lda zpCOEFP
    sta zpARGP
    lda zpCOEFP+1
    sta zpARGP+1
    jsr FLDA

FCFLP:
    jsr ARGA
    jsr FXDIV

    clc
    lda zpCOEFP
    adc #$05
    sta zpCOEFP
    sta zpARGP
    lda zpCOEFP+1
    adc #$00
    sta zpCOEFP+1
    sta zpARGP+1

    jsr FADD

    dec zpFRDDDP
    bne FCFLP

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

; = EXP numeric
; =============
EXP:
    jsr FLTFAC
FEXP:
    lda zpFACCX
    cmp #$87
    bcc LAAB8
    bne LAAA2
LAA9C:
    ldy zpFACCMA
    cpy #$B3
    bcc LAAB8
LAAA2:
    lda zpFACCS
    bpl LAAAC
    jsr FCLR
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
    jsr FFRAC
    jsr LAADA
    jsr STARGC
    lda #<FNUME
    sta zpARGP
    lda #>FNUME
    sta zpARGP+1
    jsr FLDA
    lda zp4A
    jsr FIPOW

ACMUL:
    jsr ARGC
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
    jsr INTFAC         ; Evaluate integer
    ldx zpIACC
    lda #$80          ; X=low byte, A=$80 for ADVAL

    .if .def MOS_BBC
        .if .def TARGET_C64
            jsr LAFB2
        .else
            jsr OSBYTE
        .endif
    .endif
    txa
    jmp AYACC

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
        ldx #$2A
        lda #$09
        jsr OSWORD
        lda zpFACCS
        bmi LAB9D
        jmp SINSTK
    .elseif version >= 3
; =NOT
; ====
NOT:
        jsr INTFAC
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

; =SGN numeric
; ============
    .if version < 3
LAB7F:
        jsr FTST
        beq LABA2
        bpl LABA0
        bmi LAB9D
     
SGN:
        jsr FACTOR
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
        jmp TRUE

LABA0:
        lda #$01
LABA2:
        jmp SINSTK

LABA5:
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
    bne LABB8

; =RAD numeric
; ============
RAD:
    jsr FLTFAC
    ldy #<FPIs18
    .if version < 3
        lda #>FPIs18
    .endif
LABB8:
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
    bne LABB8               ; branch always

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
    jsr INTFAC         ; Evaluate integer
    jsr USER         ; Set up registers and call code at IACC
    sta zpIACC
    stx zpIACC+1          ; Store returned A,X in IACC
    sty zpIACC+2          ; Store returned Y
    php
    pla
    sta zpIACC+3          ; Store returned flags in IACC
    cld               ; Ensure in binary mode on return
    lda #$40
    rts               ; Return INTEGER

    .if version < 3
LABE6:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =EVAL string$ - Tokenise and evaluate expression
; ================================================
EVAL:
    jsr FACTOR         ; Evaluate value
    .if version < 3
        bne LABE6
    .elseif version >= 3
        bne LAC2C
    .endif
    inc zpCLEN
    ldy zpCLEN          ; Increment string length to add a <cr>
    lda #$0D
    sta STRACC-1,Y    ; Put in terminating <cr>
    jsr PHSTR         ; Stack the string
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
    sty zpWORK          ; GPTR=>stacked string
    bne LAC0F
    inx               ; Inc high byte if next page
LAC0F:
    stx zpAELINE+1
    stx zpWORK+1          ; PTRB and GPTR high bytes
    ldy #$FF
    sty zp3B
    iny
    sty zpAECUR          ; Point PTRB offset back to start
    jsr MATCEV         ; Tokenise string on stack at GPTR
    jsr EXPR         ; Call expression evaluator
    jsr POPSTX         ; Drop string from stack
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
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =VAL numeric
; ============
VAL:
    jsr FACTOR
    .if version < 3
        bne LAC9B
    .elseif version >= 3
        bne LAC2C
    .endif
LAC34:
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
        lda #<STRACC
    .endif
    sta zpAELINE
    lda #>STRACC
    sta zpAELINE+1
    jsr AESPAC
    cmp #$2D
    beq LAC66
    cmp #$2B
    bne LAC5E
    jsr AESPAC
LAC5E:
    dec zpAECUR
    jsr FRDD
    jmp LAC73

LAC66:
    jsr AESPAC
    dec zpAECUR
    jsr FRDD
    bcc LAC73
    jsr LAD8F
LAC73:
    sta zpTYPE
    jmp LAC23

; =INT numeric
; ============
INT:
    jsr FACTOR
    .if version < 3
        beq LAC9B
    .elseif version >= 3
        beq XAC81
    .endif
    bpl LAC9A
    lda zpFACCS
    php
    jsr FFIX
    plp
    bpl LAC95
    lda zp3E
    ora zp3F
    ora zp40
    ora zp41
    beq LAC95
    jsr FNEARP
LAC95:
    jsr COPY_FACC_TO_IACC
    lda #$40
LAC9A:
    rts

    .if version < 3
LAC9B:
        jmp LETM
    .endif

; ----------------------------------------------------------------------------

; =ASC string$
; ============
ASC:
    jsr FACTOR
    .if version < 3
        bne LAC9B
    .elseif version >= 3
        bne XAC81
    .endif
    lda zpCLEN
    beq TRUE
    lda STRACC
LACAA:
    jmp SINSTK          ; tail call

; ----------------------------------------------------------------------------

; =INKEY numeric
; ==============
INKEY:
    jsr LAFAD
    .if version < 3
        cpy #$00
    .elseif version >= 3
        tya     
    .endif
    bne TRUE
    txa
    jmp AYACC

    .if version >= 3
XAC81:
        jmp LETM
    .endif

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
        beq LACAA
    .elseif version >= 3
        beq LACC6
    .endif

; =TRUE
; =====
TRUE:
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

; ----------------------------------------------------------------------------

    .if version >= 3
; =FALSE
; ======
FALSE:
        ldx #$00
        beq LACC6       ; branch always

XACA1:
        jsr FTST
        beq FALSE
        bpl XACBF
        bmi TRUE

; ----------------------------------------------------------------------------

; =SGN numeric
; ============
SGN:
        jsr FACTOR
        beq XAC81
        bmi XACA1
        lda zpIACC+3
        ora zpIACC+2
        ora zpIACC+1
        ora zpIACC
        beq LACC8
        lda zpIACC+3
        bmi TRUE
XACBF:
        lda #$01
XACC1:
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
        ldx #$2A
        lda #$09
        jsr OSWORD
        lda zpFACCS
        bmi TRUE
        bpl XACC1

    .endif              ; version >= 3

; ----------------------------------------------------------------------------

    .if version < 3
; =NOT numeric
; ============
NOT:
        jsr INTFAC
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

; ----------------------------------------------------------------------------

; =INSTR(string$, string$ [, numeric])
; ====================================
INSTR:
    jsr EXPR
    .if version < 3
        ; BASIC II for Atom and System do not branch to LAC9B with this code
        ; this seems broken... XXX: fix
        ; .print "INSTR-$47 = ", INSTR-$47
        ; .print "LAC9B = ", LAC9B
        bne INSTR-$47     ; dest=LAC9B
    .elseif version >= 3
        bne XAC81
    .endif
    cpx #$2C
    bne LAD03
    inc zpAECUR
    jsr PHSTR
    jsr EXPR
    .if version < 3
        bne INSTR-$47     ; dest=LAC9B
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
    jmp COMERR

LAD06:
    jsr PHSTR
    jsr BRA
    jsr INTEGB
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
    sta zpWORK
    tya
    adc zpAESTKP+1
    sta zpWORK+1
    lda (zpAESTKP),Y
    sec
    sbc zpIACC+3
    bcc LAD52
    sbc zpCLEN
    bcc LAD52
    adc #$00
    sta zpIACC+1
    jsr POPSTX
LAD3C:
    ldy #$00
    ldx zpCLEN
    beq LAD4D
LAD42:
    lda (zpWORK),Y
    cmp STRACC,Y
    bne LAD59
    iny
    dex
    bne LAD42
LAD4D:
    lda zpIACC
LAD4F:
    jmp SINSTK      ; tail call

LAD52:
    jsr POPSTX
LAD55:
    lda #$00
    beq LAD4F

LAD59:
    inc zpIACC
    dec zpIACC+1
    beq LAD55
    inc zpWORK
    bne LAD3C
    inc zpWORK+1
    bne LAD3C
LAD67:
    jmp LETM

; ----------------------------------------------------------------------------

; =ABS numeric
; ============
ABS:
    jsr FACTOR       ; FACTOR
    beq LAD67
    bmi LAD77
ABSCOM:
    bit zpIACC+3
    bmi COMPNO
    bpl LADAA
LAD77:
    jsr FTST
    bpl LAD89
    bmi LAD83
FNEG:
    jsr FTST
    beq LAD89
LAD83:
    lda zpFACCS
    eor #$80
    sta zpFACCS
LAD89:
    lda #$FF
    rts

LAD8C:
    jsr LAE02
LAD8F:
    beq LAD67
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
LADAA:
    lda #$40
    rts

LADAD:
    jsr AESPAC
    cmp #$22
    beq LADC9
    ldx #$00
LADB6:
    lda (zpAELINE),Y
    sta STRACC,X
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
        stx zpCLEN
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
        sta STRACC,X
    .elseif version >= 3
        sta STRACC,X
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
        stx zpCLEN
        sty zpAECUR
        lda #$00
        rts
    .elseif version >= 3
        bne LADC8
    .endif

LADE9:
    jmp NSTNG

; Evaluator Level 1, - + NOT function ( ) ? ! $ | "
; -------------------------------------------------
FACTOR:
    ldy zpAECUR
    inc zpAECUR
    lda (zpAELINE),Y      ; Get next character
    cmp #$20
    beq FACTOR         ; Loop to skip spaces
    cmp #'-'
    beq LAD8C         ; Jump with unary minus
    cmp #'"'
    beq LADC9         ; Jump with string
    cmp #'+'
    bne LAE05         ; Jump with unary plus
LAE02:
    jsr AESPAC         ; Get current character
LAE05:
    cmp #$8E
    bcc LAE10         ; Lowest function token, test for indirections
    cmp #$C6
    bcs FACERR         ; Highest function token, jump to error
    jmp DISPATCH         ; Jump via function dispatch table

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
    beq BRA         ; Jump with brackets
LAE20:
    dec zpAECUR
    jsr LVCONT
    beq ERRFAC         ; Jump with undefined variable or bad name
    jmp VARIND

LAE2A:
    jsr FRDD
    bcc FACERR
    rts

ERRFAC:
    lda zpBYTESM      ; Check assembler option
    and #$02          ; Is 'ignore undefiened variables' set?
    bne FACERR         ; b1=1, jump to give No such variable
    bcs FACERR         ; Jump with bad variable name
    stx zpAECUR

GETPC:
    lda PC      ; Use P% for undefined variable
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

LAE6D:
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

; Return 16-bit integer in AY
; ---------------------------
AYACC:
        sta zpIACC
        sty zpIACC+1      ; Store AY in integer accumulator
        lda #$00
        sta zpIACC+2
        sta zpIACC+3      ; Set b16-b31 to 0
        lda #$40
        rts           ; Return 'integer'

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
RTIME:
    ldx #$2A
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
     
LAEC7:
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
        bne LAEC7
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
        rts           ; Return 'integer'

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
LAF0A:
    inc zpAECUR
    jsr BRA
    jsr INTEGB
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
    jsr IFLT
    jsr PHFACC
    jsr LAF69
    jsr POPSET
    jsr IFMUL
    jsr FNRM
    jsr IFIX
    jsr INCACC
    lda #$40
    rts

LAF3F:
    ldx #$0D
    jsr ACCTOM
    lda #$40
    sta zpSEED+4
    rts

; RND [(numeric)]
; ===============
RND:
    ldy zpAECUR
    lda (zpAELINE),Y      ; Get current character
    cmp #'('
    beq LAF0A         ; Jump with RND(numeric)
    jsr LAF87         ; Get random number
    ldx #$0D

; XXX check if all calls have X set with proper ZP offset
MTOACC:
    lda zp+0,X
    sta zpIACC          ; Copy number pointed to by X to IACC
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
    stx zpFACCS
    stx zpFACCXH
    stx zpFACCMG
    lda #$80
    sta zpFACCX
LAF78:
    lda zpSEED,X
    sta zpFACCMA,X
    inx
    cpx #$04
    bne LAF78
    jsr NRMTDY
    lda #$FF
    rts

; ----------------------------------------------------------------------------

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
LAFAD:
    jsr INTFAC         ; Evaluate <numeric>

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

; ----------------------------------------------------------------------------

; =GET
; ====
GET:
    jsr OSRDCH
    .if version < 3
        jmp SINSTK
    .elseif version >= 3
        jmp SINSTK
    .endif

; ----------------------------------------------------------------------------

; =GET$
; =====
GETD:
    jsr OSRDCH
LAFC2:
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
    bne LB033
    cpx #$2C
    bne LB036
    inc zpAECUR
    jsr PHSTR
    jsr BRA
    jsr INTEGB
    jsr LBDCB
    lda zpIACC
    cmp zpCLEN
    bcs LAFEB
    sta zpCLEN
LAFEB:
    lda #$00
    rts

; ----------------------------------------------------------------------------

; =RIGHT$(string$, numeric)
; =========================
RIGHTD:
    jsr EXPR
    bne LB033
    cpx #$2C
    bne LB036
    inc zpAECUR
    jsr PHSTR
    jsr BRA
    jsr INTEGB
    jsr LBDCB
    lda zpCLEN
    sec
    sbc zpIACC
    bcc LB023
    beq LB025
    tax
    lda zpIACC
    sta zpCLEN
    beq LB025
    ldy #$00
LB017:
    lda STRACC,X
    sta STRACC,Y
    inx
    iny
    dec zpIACC
    bne LB017
LB023:
    lda #$00
LB025:
    rts

; ----------------------------------------------------------------------------

; =INKEY$ numeric
; ===============
INKED:
    jsr LAFAD
    txa
    cpy #$00
    beq LAFC2
LB02E:
    lda #$00
    sta zpCLEN
    rts

LB033:
    jmp LETM

LB036:
    jmp COMERR

; ----------------------------------------------------------------------------

; =MID$(string$, numeric [, numeric] )
; ====================================
MIDD:
    jsr EXPR
    bne LB033
    cpx #$2C
    bne LB036
    jsr PHSTR
    inc zpAECUR
    jsr INEXPR
    lda zpIACC
    pha
    lda #$FF
    sta zpIACC
    inc zpAECUR
    cpx #')'
    beq LB061
    cpx #$2C
    bne LB036
    jsr BRA
    jsr INTEGB
LB061:
    jsr LBDCB
    pla
    tay
    clc
    beq LB06F
    sbc zpCLEN
    bcs LB02E
    dey
    tya
LB06F:
    sta zpIACC+2
    tax
    ldy #$00
    lda zpCLEN
    sec
    sbc zpIACC+2
    cmp zpIACC
    bcs LB07F
    sta zpIACC
LB07F:
    lda zpIACC
    beq LB02E
LB083:
    lda STRACC,X
    sta STRACC,Y
    iny
    inx
    cpy zpIACC
    bne LB083
    sty zpCLEN
    lda #$00
    rts

; ----------------------------------------------------------------------------

; =STR$ [~] numeric
; =================
STRD:
    jsr AESPAC         ; Skip spaces
    ldy #$FF          ; Y=$FF for decimal
    cmp #'~'
    beq LB0A1
    ldy #$00
    dec zpAECUR          ; Y=$00 for hex, step past ~
LB0A1:
    tya
    pha               ; Save format
    jsr FACTOR
    beq LB0BF         ; Evaluate, error if not number
    tay
    pla
    sta zpPRINTF          ; Get format back
    lda VARL_AT+3
    bne LB0B9         ; Top byte of @%, STR$ uses @%
    sta zpWORK          ; Store 'General format'
    jsr FCONA         ; Convert using general format
    lda #$00
    rts               ; Return string

LB0B9:
    jsr FCON         ; Convert using @% format
    lda #$00
    rts               ; Return string

LB0BF:
    jmp LETM         ; Jump to Type mismatch error

; ----------------------------------------------------------------------------

; =STRING$(numeric, string$)
; ==========================
STRND:
    jsr INEXPR
    jsr PHACC
    jsr COMEAT
    jsr BRA
    bne LB0BF
    jsr POPACC
    ldy zpCLEN
    beq LB0F5
    lda zpIACC
    beq LB0F8
    dec zpIACC
    beq LB0F5
LB0DF:
    ldx #$00
LB0E1:
    lda STRACC,X
    sta STRACC,Y
    inx
    iny
    beq LB0FB
    cpx zpCLEN
    bcc LB0E1
    dec zpIACC
    bne LB0DF
    sty zpCLEN
LB0F5:
    lda #$00
    rts

LB0F8:
    sta zpCLEN
    rts

LB0FB:
    jmp STROVR

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
    jsr SPACES
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
    cmp (zpWORK),Y
    bne LB12D
    cpy zp39
    bne LB158
    iny
    lda (zp3C),Y
    jsr WORDCQ
    bcs LB12D
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

; ----------------------------------------------------------------------------

; =FNname [parameters]
; ====================
FN:
    lda #tknFN

; Call subroutine
; ---------------
; A=FN or PROC
; PtrA=>start of FN/PROC name
;
FNBODY:
    sta zpTYPE          ; Save PROC/FN token
    tsx
    txa
    clc
    adc zpAESTKP          ; Drop BASIC stack by size of 6502 stack
    jsr HIDEC         ; Store new BASIC stack pointer, check for No Room
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
    sta zpWORK
    tya
    sbc #$00
    sta zpWORK+1          ; $37/8=>PROC token
    ldy #$02
    jsr WORDLP         ; Check name is valid
    cpy #$02
    beq LB18A         ; No valid characters, jump to 'Bad call' error
    stx zpAECUR          ; Line pointer offset => after valid FN/PROC name
    dey
    sty zp39
    jsr CREAFN
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
    jsr SPACES
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
    jsr STMT
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
    jsr POPWRK
    jsr STORST
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
    jsr CRAELV
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
    jsr RETINF
    jsr SPACES
    cmp #','
    beq LB24D
    cmp #')'
    bne LB2B5
    lda #$00
    pha
    jsr AESPAC
    cmp #'('
    bne LB2B5
LB28E:
    jsr EXPR
    jsr LBD90
    lda zpTYPE
    sta zpIACC+3
    jsr PHACC
    pla
    tax
    inx
    txa
    pha
    jsr AESPAC
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
    jsr POPACC
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
    jsr ACCTOM
    lda zpTYPE
    bpl LB2F0
    jsr POPSET
    jsr FLDA
    jmp LB2F3

LB2F0:
    jsr POPACC
LB2F3:
    jsr STORF
    jmp LB303

LB2F9:
    lda zpIACC+3
    bne LB2B5
    jsr LBDCB
    jsr STSTRE
LB303:
    dec zpCOEFP
    bne LB2CA
    lda zpCOEFP+1
    pha
    jmp LB202

; ----------------------------------------------------------------------------

; Push a value onto the stack
; ---------------------------
RETINF:
    ldy zpIACC+2
    .if version < 3
        cpy #$04
        bne LB318
    .elseif version >= 3
        cpy #$05
        bcs LB318
    .endif
    ldx #zpWORK
    jsr ACCTOM

LB318:
    jsr VARIND
    php
    jsr LBD90
    plp
    beq LB329
    bmi LB329
    ldx #$37
    jsr MTOACC
LB329:
    jmp PHACC

VARIND:
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
    jmp AYACC

LB354:
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
    beq LB37F
    lda zpFACCS
    ora #$80
LB37F:
    sta zpFACCMA
    lda #$FF
    rts

LB384:
    cpy #$80
    beq LB3A7
    ldy #$03
    lda (zpIACC),Y
    sta zpCLEN
    beq LB3A6
    ldy #$01
    lda (zpIACC),Y
    sta zpWORK+1
    dey
    lda (zpIACC),Y
    sta zpWORK
    ldy zpCLEN
LB39D:
    dey
    lda (zpWORK),Y
    sta STRACC,Y
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
    sta STRACC,Y
    eor #$0D
    beq LB3BA
    iny
    bne LB3AD
    tya
LB3BA:
    sty zpCLEN
    rts

; =CHR$ numeric
; =============
CHRD:
    jsr INTFAC
LB3C0:
    lda zpIACC
    jmp LAFC2

LB3C5:
    ldy #$00
    sty zpERL
    sty zpERL+1
    ldx zpTXTP
    stx zpWORK+1
    sty zpWORK
    ldx zpLINE+1
    cpx #>BUFFER
    beq LB401
    ldx zpLINE
LB3D9:
    jsr GETWRK
    cmp #$0D
    bne LB3F9
    cpx zpWORK
    lda zpLINE+1
    sbc zpWORK+1
    bcc LB401
    jsr GETWRK
    ora #$00
    bmi LB401
    sta zpERL+1
    jsr GETWRK
    sta zpERL
    jsr GETWRK
LB3F9:
    cpx zpWORK
    lda zpLINE+1
    sbc zpWORK+1
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
    lda #<BASERR
    sta zpERRORLH          ; ON ERROR OFF
    lda #>BASERR
    sta zpERRORLH+1
LB413:          ; BREKA
    lda zpERRORLH
    sta zpLINE          ; Point program point to ERROR program
    lda zpERRORLH+1
    sta zpLINE+1
    jsr SETVAR         ; Clear DATA and stack
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
    jmp STMT         ; Jump to execution loop

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

; SOUND numeric, numeric, numeric, numeric
; ========================================
BEEP:
    jsr ASEXPR         ; Evaluate integer
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
    jsr INCMEX         ; Step past comma, evaluate next integer
    pla
    tax
    dex
    bne LB451         ; Loop to stack this one
    .ifdef MOS_BBC
        jsr AEDONE     ; Check end of statement
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
ENVEL:
    jsr ASEXPR         ; Evaluate integer
    ldx #$0D          ; 13 more to evaluate
LB477:
    .ifdef MOS_BBC
        lda zpIACC
        pha           ; Stack current 8-bit integer
    .endif
    txa
    pha
    jsr INCMEX         ; Step past comma, evaluate next integer
    pla
    tax
    dex
    bne LB477         ; Loop to stack this one
LB484:
    jsr AEDONE         ; Check end of statement
    .ifdef MOS_BBC
        lda zpIACC
        sta zp44      ; Copy current 8-bit integer to end of control block
        ldx #$0C
        ldy #$08      ; Prepare for 12 more bytes and OSWORD 8
    .endif


LB48F:
    .ifdef MOS_BBC
        pla
        sta zpWORK,X    ; Pop bytes into control block
        dex
        bpl LB48F
        tya           ; Y=OSWORD number
        ldx #$37
        ldy #$00      ; XY=>control block
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
LB4B1:
    jsr EXPR         ; Evaluate expression
STORE:
    jsr POPWRK         ; Unstack integer (address of data)
STORF:
    lda zpWORK+2
    cmp #$05
    beq LB4E0         ; Size=5, jump to store float
    lda zpTYPE
    beq STORER        ; Type<>num, jump to error
    bpl STORIN        ; Type=int, jump to store it
    jsr IFIX          ; Convert float to integer

STORIN:
    ldy #$00
    lda zpIACC
    sta (zpWORK),Y      ; Store byte 1
    lda zp39
    beq LB4DF         ; Exit if size=0, byte
    lda zpIACC+1
    iny
    sta (zpWORK),Y      ; Store byte 2
    lda zpIACC+2
    iny
    sta (zpWORK),Y      ; Store byte 3
    lda zpIACC+3
    iny
    sta (zpWORK),Y      ; Store byte 4
LB4DF:
    rts

; ----------------------------------------------------------------------------

; Store float
; ===========
LB4E0:
    lda zpTYPE
    beq STORER         ; Type<>num, jump to error
    bmi LB4E9         ; Type=float, jump to store it
    jsr IFLT         ; Convert integer to float
LB4E9:
    ldy #$00          ; Store 5-byte float
    lda zpFACCX
    sta (zpWORK),Y
    iny               ; exponent
    lda zpFACCS
    and #$80
    sta zpFACCS          ; Unpack sign
    lda zpFACCMA
    and #$7F          ; Unpack mantissa 1
    ora zpFACCS
    sta (zpWORK),Y      ; sign + mantissa 1
    iny
    lda zpFACCMB
    sta (zpWORK),Y      ; mantissa 2
    iny
    lda zpFACCMC
    sta (zpWORK),Y      ; mantissa 3
    iny
    lda zpFACCMD
    sta (zpWORK),Y      ; mantissa 4
    rts

; ----------------------------------------------------------------------------

TOKOUT:
    sta zpWORK
    cmp #$80
    bcc CHOUT
    lda #<TOKENS
    sta zpWORK+1          ; Point to token table
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
    lda #' '
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

LB58A:
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
    cmp #'O'
    beq LB58A
    lda #$00
    sta zp3B
    sta zp3C
    .if version < 3
        jsr SINSTK
    .elseif version >= 3
        jsr SINSTK
    .endif
    jsr SPTSTN
    php
    jsr PHACC
    lda #$FF
    sta zpIACC
    lda #$7F
    sta zpIACC+1
    plp
    bcc LB5CF
    jsr SPACES
    cmp #','
    beq LB5D8
    jsr POPACC
    jsr PHACC
    dec zpCURSOR
    bpl LB5DB
LB5CF:
    jsr SPACES
    cmp #','
    beq LB5D8
    dec zpCURSOR
LB5D8:
    jsr SPTSTN
LB5DB:
    lda zpIACC
    sta zpFACCMA
    lda zpIACC+1
    sta zpFACCMB
    jsr DONE
    jsr ENDER
    jsr POPACC
    jsr FNDLNO
    lda zp3D
    sta zpLINE
    lda zp3E
    sta zpLINE+1
    bcc LB60F
    dey
    bcs LB602

LB5FC:
    jsr NLINE
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
    sbc zpFACCMA
    lda zpIACC+1
    sbc zpFACCMB
    bcc LB61D
    jmp CLRSTK

LB61D:
    jsr NPRN
    ldx #$FF
    stx zpCOEFP
    lda #$01
    jsr LISTPS
    ldx zp3B
    lda #$02
    jsr LISTPS
    ldx zp3C
    lda #$04
    jsr LISTPS
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
    jsr CHOUT
    iny
    bne LB639
LB651:
    bit zpCOEFP
    bpl LB64B
    cmp #$8D
    bne LB668
    jsr SPGETN
    sty zpCURSOR
    lda #$00
    sta zpPRINTS
    jsr POSITE
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
    jsr TOKOUT
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
NEXT:
    jsr AELV
    bne LB6A3
    ldx zpFORSTP
    beq LB68E
    bcs LB6D7
LB6A0:
    jmp STDED

LB6A3:
    bcs LB6A0
    ldx zpFORSTP
    beq LB68E
LB6A9:
    lda zpIACC
    cmp FORINL-$f,X
    bne LB6BE
    lda zpIACC+1
    cmp FORINH-$f,X
    bne LB6BE
    lda zpIACC+2
    cmp FORINT-$f,X
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
    lda FORINL-$f,X
    sta zpIACC
    lda FORINH-$f,X
    sta zpIACC+1
    ldy FORINT-$f,X
    cpy #$05
    beq LB766
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
    sta zp39
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
    lda zp39
    sbc FORLMN-$f,X
    sta zp39
    tya
    sbc FORLMH-$f,X
    ora zpWORK
    ora zpWORK+1
    ora zp39
    beq LB741
    tya
    eor FORSPH-$f,X
    eor FORLMH-$f,X
    bpl LB73F
    bcs LB741
    bcc LB751
LB73F:
    bcs LB751
LB741:
    ldy FORADL-$f,X
    lda FORADH-$f,X
    sty zpLINE
    sta zpLINE+1
    jsr SECUR
    jmp STMT

LB751:
    lda zpFORSTP
    sec
    sbc #$0F
    sta zpFORSTP
    ldy zpAECUR
    sty zpCURSOR
    jsr SPACES
    cmp #','
    bne LB7A1
    jmp NEXT

LB766:
    jsr LB354
    lda zpFORSTP
    clc
    adc #$F4
    sta zpARGP
    lda #$05+(ws/256)
    sta zpARGP+1
    jsr FADD
    lda zpIACC
    sta zpWORK
    lda zpIACC+1
    sta zpWORK+1
    jsr LB4E9
    lda zpFORSTP
    sta zpTYPE
    clc
    adc #$F9
    sta zpARGP
    lda #$05+(ws/256)
    sta zpARGP+1
    jsr FCMP
    beq LB741
    lda FORSPM-$f,X
    bmi LB79D
    bcs LB741
    bcc LB751
LB79D:
    bcc LB741
    bcs LB751
LB7A1:
    jmp SUNK

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
FOR:
    jsr CRAELV
    beq LB7A4
    bcs LB7A4
    jsr PHACC
    jsr EQEAT
    jsr LB4B1
    ldy zpFORSTP
    cpy #$96
    bcs LB7B0
    lda zpWORK
    sta FORINL,Y
    lda zpWORK+1
    sta FORINH,Y
    lda zp39
    sta FORINT,Y
    tax
    jsr AESPAC
    cmp #$B8
    bne LB7BD
    cpx #$05
    beq LB84F
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
    .if version < 3
        jsr SINSTK
    .elseif version >= 3
        jsr SINSTK
    .endif
    jsr AESPAC
    cmp #tknSTEP
    bne LB81F
    jsr INEXPR
    ldy zpAECUR
LB81F:
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
LB837:
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

LB84F:
    jsr EXPR
    jsr FLOATI
    lda zpFORSTP
    clc
    adc #$08
    sta zpARGP
    lda #$05+(ws/256)
    sta zpARGP+1
    jsr FSTA
    jsr FONE
    jsr AESPAC
    cmp #$88
    bne LB875
    jsr EXPR
    jsr FLOATI
    ldy zpAECUR
LB875:
    sty zpCURSOR
    lda zpFORSTP
    clc
    adc #$03
    sta zpARGP
    lda #$05+(ws/256)
    sta zpARGP+1
    jsr FSTA
    jmp LB837

; GOSUB numeric
; =============
GOSUB:
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
    bcc GODONE

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

; ----------------------------------------------------------------------------

; RETURN
; ======
RETURN:
    jsr DONE         ; Check for end of statement
    ldx zpSUBSTP
    beq LB8AF         ; If GOSUB stack empty, error
    dec zpSUBSTP          ; Decrement GOSUB stack
    ldy ws+$05CB,X    ; Get stacked line pointer
    lda ws+$05E5,X
    sty zpLINE
    sta zpLINE+1          ; Set line pointer
    jmp NXT         ; Jump back to execution loop

; ----------------------------------------------------------------------------

; GOTO numeric
; ============
GOTO:
    jsr LB99A
    jsr DONE         ; Find destination line, check for end of statement
GODONE:
    lda zpTRFLAG
    beq LB8D9
    jsr TRJOBA; If TRACE ON, print current line number
LB8D9:
    ldy zp3D
    lda zp3E          ; Get destination line address
LB8DD:
    sty zpLINE
    sta zpLINE+1          ; Set line pointer
    jmp STMT         ; Jump back to execution loop

; ----------------------------------------------------------------------------

; ON ERROR OFF
; ------------
LB8E4:
    jsr DONE         ; Check end of statement
    lda #<BASERR
    sta zpERRORLH          ; ON ERROR OFF
    lda #>BASERR
    sta zpERRORLH+1
    jmp NXT         ; Jump to execution loop

; ----------------------------------------------------------------------------

; ON ERROR [OFF | program ]
; -------------------------
LB8F2:
    jsr SPACES
    cmp #tknOFF
    beq LB8E4         ; ON ERROR OFF
    ldy zpCURSOR
    dey
    jsr CLYADP
    lda zpLINE
    sta zpERRORLH          ; Point ON ERROR pointer to here
    lda zpLINE+1
    sta zpERRORLH+1
    jmp REM         ; Skip past end of line

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

; ----------------------------------------------------------------------------

; ON [ERROR] [numeric]
; ====================
ON:
    jsr SPACES         ; Skip spaces and get next character
    cmp #tknERROR
    beq LB8F2         ; Jump with ON ERROR
    dec zpCURSOR
    jsr AEEXPR
    jsr INTEGB
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
    ora zpIACC+2          ; Get IACC
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
    jmp GODONE         ; Jump to do GOTO

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
    jmp THENLN         ; Store line index and jump to GOSUB

LB99A:
    jsr SPTSTN
    bcs GOTGO         ; Embedded line number found
    jsr AEEXPR
    jsr INTEGB         ; Evaluate expression, ensure integer
    lda zpAECUR
    sta zpCURSOR          ; Line number low byte
    lda zpIACC+1
    and #$7F
    sta zpIACC+1          ; Line number high byte
                      ; Note - this makes goto $8000+10 the same as goto 10
GOTGO:
    jsr FNDLNO
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
    jmp LETM

LB9C7:
    jmp STDED

LB9CA:
    sty zpCURSOR
    jmp DONEXT

; INPUT#channel, ...
; ------------------
LB9CF:
    dec zpCURSOR
    jsr LBFA9
    lda zpAECUR
    sta zpCURSOR
    sty zpCOEFP
LB9DA:
    jsr SPACES
    cmp #','
    bne LB9CA
    lda zpCOEFP
    pha
    jsr CRAELV
    beq LB9C7
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
    bcc LBA19
    lda zpTYPE
    bne LB9C4
    jsr OSBGET
    sta zpCLEN
    tax
    beq LBA13
LBA0A:
    jsr OSBGET
    sta STRACC-1,X
    dex
    bne LBA0A
LBA13:
    jsr STSTOR
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
    sta FWSA,X
    dex
    bpl LBA2D
    jsr LDARGA
LBA39:
    jsr STORE
    jmp LB9DA

LBA3F:
    pla
    pla
    jmp DONEXT

; ----------------------------------------------------------------------------

; INPUT [LINE] [print items][variables]
; =====================================
INPUT:
    jsr SPACES         ; Get next non-space char
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
    jsr PRTSTN
    bcs LBA69         ; Process ' " TAB SPC, jump if none found
LBA5F:
    jsr PRTSTN
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
    jsr CRAELV
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
    jsr CHOUT
LBAA2:
    jsr LBBFC         ; Call MOS to input line, set COUNT=0
    sty zpCLEN
    asl zpCOEFP
    clc
    ror zpCOEFP
    bit zpCOEFP
    bvs LBACD
LBAB0:
    sta zpAECUR
    lda #<STRACC
    sta zpAELINE
    lda #>STRACC
    sta zpAELINE+1
    jsr LADAD
LBABD:
    jsr AESPAC
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
    jsr PHACC
    jsr LAC34
    jsr STORE
    jmp LBA5A

LBADC:
    lda #$00
    sta zpTYPE
    jsr STSTRE
    jmp LBA5A

; RESTORE [linenum]
; =================
RESTORE:
    ldy #$00
    sty zpWORK+6          ; Set DATA pointer to PAGE
    ldy zpTXTP
    sty zp3E
    jsr SPACES
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
    jmp NXT

LBB15:
    jsr SPACES
    cmp #','
    beq READ
    jmp SUNK

; READ varname [,...]
; ===================
READ:
    jsr CRAELV
    beq LBB15
    bcs LBB32
    jsr LBB50
    jsr PHACC
    jsr LB4B1
    jmp LBB40

LBB32:
    jsr LBB50
    jsr PHACC
    jsr LADAD
    sta zpTYPE
    jsr STSTOR
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
    jsr AESPAC
    cmp #','
    beq LBBB0
    cmp #tknDATA
    beq LBBB0
    cmp #$0D
    beq LBB7A
LBB6F:
    jsr AESPAC
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

; ----------------------------------------------------------------------------

; UNTIL numeric
; =============
UNTIL:
    jsr AEEXPR
    jsr FDONE
    jsr INTEG
    ldx zpDOSTKP
    beq LBBA6
    lda zpIACC
    ora zpIACC+1
    ora zpIACC+2
    ora zpIACC+3
    beq LBBCD
    dec zpDOSTKP
    jmp NXT

LBBCD:
    ldy ws+$05A3,X
    lda ws+$05B7,X
    jmp LB8DD

; ----------------------------------------------------------------------------

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

; ----------------------------------------------------------------------------

; REPEAT
; ======
REPEAT:
    ldx zpDOSTKP
    cpx #$14
    bcs LBBD6
    jsr CLYADP
    lda zpLINE
    sta ws+$05A4,X
    lda zpLINE+1
    sta ws+$05B8,X
    inc zpDOSTKP
    jmp STMT

; ----------------------------------------------------------------------------

; Input string to string buffer
; -----------------------------
LBBFC:
    ldy #<STRACC
    lda #>STRACC
    bne LBC09

; Print character, read input line
; --------------------------------
BUFF:
    jsr CHOUT         ; Print character
    ldy #<BUFFER
    lda #>BUFFER

LBC09:
    sty zpWORK
    sta zpWORK+1      ; zpWORK=>input buffer

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
        sta (zpWORK),Y      ; Store character
        cmp #$0D
        beq NLINE     ; Return - finish
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
        bcc BUFEND     ; CC, Escape not pressed, exit and set COUNT=0
    .endif
    jmp DOBRK         ; Escape

; ----------------------------------------------------------------------------

NLINE:
    jsr OSNEWL
BUFEND:
    lda #$00
    sta zpTALLY          ; Set COUNT to zero
    rts

REMOVE:
    jsr FNDLNO
    bcs LBC80
    lda zpWORK+6
    sbc #$02
    sta zpWORK
    sta zpWORK+6
    sta zpTOP
    lda zpWORK+7
    sbc #$00
    sta zpWORK+1
    sta zpTOP+1
    sta zpWORK+7
    ldy #$03
    lda (zpWORK),Y
    clc
    adc zpWORK
    sta zpWORK
    bcc LBC53

    inc zpWORK+1
LBC53:
    ldy #$00
LBC55:
    lda (zpWORK),Y
    sta (zpTOP),Y
    cmp #$0D
    beq LBC66
LBC5D:
    iny
    bne LBC55
    inc zpWORK+1
    inc zpTOP+1
    bne LBC55
LBC66:
    iny
    bne LBC6D
    inc zpWORK+1
    inc zpTOP+1
LBC6D:
    lda (zpWORK),Y
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
    inc zpWORK+1
LBC88:
    lda (zpWORK),Y
    sta (zpTOP),Y
    rts

; ----------------------------------------------------------------------------

INSRT:
    sty zpWORK+4
    jsr REMOVE
    ldy #>BUFFER
    sty zpWORK+5
    ldy #$00
    lda #$0D
    cmp (zpWORK+4),Y
    beq LBD10

LBC9E:
    iny
    cmp (zpWORK+4),Y
    bne LBC9E
    iny
    iny
    iny
    sty zpWORK+8
    inc zpWORK+8
    lda zpTOP
    sta zpWORK+2
    lda zpTOP+1
    sta zpWORK+3
    jsr LBE92       ; CLYADT
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
    jsr LBE56
    ldy #$FF
INSLOP:
    iny
    lda (zpWORK+4),Y
    sta (zpWORK+6),Y
    cmp #$0D
    bne INSLOP
LBD10:
    rts

; ----------------------------------------------------------------------------

; RUN
; ===
RUN:
    jsr DONE
RUNNER:
    jsr SETFSA
    lda zpTXTP
    sta zpLINE+1          ; Point PtrA to PAGE
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
    sta zpAESTKP+1          ; STACK=HIMEM
    lda #$00
    sta zpDOSTKP
    sta zpFORSTP
    sta zpSUBSTP; Clear REPEAT, FOR, GOSUB stacks
    sta zpDATAP
    rts               ; DATA pointer=PAGE

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

LBD90:
    beq PHSTR
    bmi PHFACC

; Push Integer ACC to stack

PHACC:
    lda zpAESTKP
    sec
    sbc #$04
LBD99:
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

; Stack the current string
; ========================
PHSTR:
    clc
    lda zpAESTKP
    sbc zpCLEN          ; stackbot=stackbot-length-1
    jsr HIDEC         ; Check enough space
    ldy zpCLEN
    beq LBDC6         ; Zero length, just stack length
LBDBE:
    lda STRACC-1,Y
    sta (zpAESTKP),Y      ; Copy string to stack
    dey
    bne LBDBE         ; Loop for all characters
LBDC6:
    lda zpCLEN
    sta (zpAESTKP),Y      ; Copy string length
    rts

; ----------------------------------------------------------------------------

; Unstack a string
; ================
LBDCB:
    ldy #$00
    lda (zpAESTKP),Y      ; Get stacked string length
    sta zpCLEN
    beq POPSTX
    tay               ; If zero length, just unstack length

LBDD4:
    lda (zpAESTKP),Y
    sta STRACC-1,Y    ; Copy string to string buffer
    dey
    bne LBDD4         ; Loop for all characters

POPSTX:
    ldy #$00
    lda (zpAESTKP),Y      ; Get string length again
    sec
POPN:
    adc zpAESTKP
    sta zpAESTKP          ; Update stack pointer
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
; XXX: check all calls have X set with ZP offset

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

; XXX: check all calls have X set with ZP offset
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
    jsr OSTHIG
    tay
    lda #$FF          ; FILE.LOAD=PAGE

    .ifdef MOS_ATOM
        sta F_EXEC+0
        ldx #$37      ; FILE.EXEC=$FF, romstart to specified address
        sec
        jsr OSLOAD
    .endif

    .ifdef MOS_BBC
        sty F_EXEC+0
        ldx #$37      ; FILE.EXEC=0, romstart to specified address
        jsr OSFILE
    .endif

; Scan program to check consistancy and find TOP
; ----------------------------------------------
ENDER:
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

; Point $37/8 to <cr>-terminated string in string buffer
; ------------------------------------------------------
LBEB2:
    lda #<STRACC
    sta zpWORK
    lda #>STRACC
    sta zpWORK+1
LBEBA:
    ldy zpCLEN
    lda #$0D
    sta STRACC,Y
    rts

; ----------------------------------------------------------------------------

; OSCLI string$ - Pass string to OSCLI to execute
; ===============================================
OSCL:
    jsr LBED2         ; $37/8=>cr-string


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
        jmp NXT     ; Call OSCLI and return to execution loop
    .endif

LBECF:
    jmp LETM

LBED2:
    jsr AEEXPR
    bne LBECF         ; Evaluate expression, error if not string
    jsr LBEB2
    jmp FDONE         ; Convert to <cr>-string, check end of statement

; ----------------------------------------------------------------------------

; Set FILE.LOAD to MEMHI.PAGE
; ---------------------------
OSTHIG:
    jsr LBED2
    dey
    sty F_LOAD+0      ; LOAD.lo=$00
    lda zpTXTP
    sta F_LOAD+1      ; LOAD.hi=PAGEhi

GETMAC:
    .ifdef MOS_BBC
        lda #$82
        jsr OSBYTE    ; Get memory base high word
        stx F_LOAD+2
        sty F_LOAD+3      ; Set LOAD high word
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
    jsr ENDER         ; Check program, set TOP

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
        jsr OSTHIG     ; Set FILE.LOAD to PAGE
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
        jsr OSTHIG     ; Set FILE.LOAD to PAGE
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
    jmp NXT

; ----------------------------------------------------------------------------

; LOAD string$
; ============
LOAD:
    jsr LBE62
    jmp FSASET         ; Do LOAD, jump to immediate mode

; ----------------------------------------------------------------------------

; CHAIN string$
; =============
CHAIN:
    jsr LBE62
    jmp RUNNER         ; Do LOAD, jump to execution loop

; ----------------------------------------------------------------------------

; PTR#numeric=numeric
; ===================
LPTR:
    jsr LBFA9
    pha               ; Evaluate #handle
    jsr EQEXPR
    jsr INTEG         ; Step past '=', evaluate integer
    pla
    tay
    ldx #$2A          ; Get handle, point to IACC
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
    jsr CHANN
    ldx #$2A
    pla               ; Evaluate #handle, point to IACC
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
    jsr LBFA9
    pha               ; Evaluate #handle
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
    jsr CHANN
    jsr OSBGET        ; Evaluate #handle
    .if version < 3
        jmp SINSTK     ; Jump to return 8-bit integer
    .elseif version >= 3
        jmp SINSTK     ; Jump to return 8-bit integer
    .endif

; ----------------------------------------------------------------------------

; OPENIN f$ - Call OSFIND to open file for input
; ==============================================
OPENIN:
    .ifdef MOS_ATOM
        sec           ; SEC=OPENUP
        bcs LBF82     
    .endif
    .ifdef MOS_BBC
        lda #$40      ; $40=OPENUP
        bne LBF82
    .endif

; ----------------------------------------------------------------------------

; OPENOUT f$ - Call OSFIND to open file for output
; ================================================
OPENO:
    .ifdef MOS_ATOM
        clc           ; CLC=OPENOUT
        bcc LBF82     
    .endif
    .ifdef MOS_BBC
        lda #$80      ; 80=OPENOUT
        bne LBF82
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
LBF82:
    .ifdef MOS_ATOM
        php       
    .endif
    .ifdef MOS_BBC
        pha       
    .endif
    jsr FACTOR
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
        jmp SINSTK
    .elseif version >= 3
        jmp SINSTK
    .endif

LBF96:
    jmp LETM         ; Jump to 'Type mismatch' error

; CLOSE#numeric
; =============
CLOSE:
    jsr LBFA9
    jsr AEDONE         ; Evaluate #handle, check end of statement
    ldy zpIACC         ; Get handle from IACC
    .ifdef MOS_ATOM
        jsr OSSHUT         
    .endif
    .ifdef MOS_BBC
        lda #$00
        jsr OSFIND
    .endif
    jmp NXT         ; Jump back to execution loop

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
CHANN:
    jsr AESPAC         ; Skip spaces
    cmp #'#'          ; If not '#', jump to give error
    .if version < 3
        bne LBFC3
    .elseif version >= 3
        bne LBFF4
    .endif
    jsr INTFAC         ; Evaluate as integer
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
VSTRNG:
    pla
    sta zpWORK
    pla
    sta zpWORK+1          ; Pop return address to pointer
    ldy #$00
    beq LBFDC         ; Jump into loop
LBFD9:
    jsr OSASCI        ; Print character
LBFDC:
    jsr GETWK2
    bpl LBFD9         ; Update pointer, get character, loop if b7=0
    jmp (zpWORK)        ; Jump back to program

; REPORT
; ======
REPORT:
    jsr DONE
    jsr NLINE         ; Check end of statement, print newline, clear COUNT
    ldy #$01
LBFEC:
    lda (FAULT),Y
    beq LBFF6         ; Get byte, exit if $00 terminator
    jsr TOKOUT
    iny
    bne LBFEC         ; Print character or token, loop for next
LBFF6:
    jmp NXT         ; Jump to main execution loop

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

    .if * > [romstart + $4000]
        .error "***WARNING: Code overrun"
    .endif

    .align romstart + $4000, 0
LC000:
