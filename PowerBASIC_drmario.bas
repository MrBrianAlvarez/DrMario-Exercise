
'Generated with PluriBASIC 6.0.237326.0

#COMPILE EXE

GLOBAL SYSTEM_UDT_OFFSETS() AS STRING

%GAME_W                                                                             = 256
%GAME_H                                                                             = 224
%TIMER_REFRESHSCREEN                                                                = 1001
%TIMER_MUSICLOOP                                                                    = 1002
%TIMER_MUSICCHECK                                                                   = 1003
%AREA_VIRUS                                                                         = 1
%AREA_BLOCK                                                                         = 2
%AREA_PILL                                                                          = 3
%AREA_HALFPILL                                                                      = 4
%BOTTLE_X                                                                           = 96
%BOTTLE_Y                                                                           = 72
%VIRUS_NORMAL                                                                       = 0
%VIRUS_FLOOR                                                                        = 1
%VIRUS_DEAD                                                                         = 3
%SCREEN_TITLE                                                                       = 1
%SCREEN_OPTIONS                                                                     = 2
%SCREEN_GAME                                                                        = 3
%SCREEN_ENDING                                                                      = 4
%DIR_UP                                                                             = 1
%DIR_RT                                                                             = 2
%DIR_DN                                                                             = 3
%DIR_LF                                                                             = 4
%COLOR_RED                                                                          = 1
%COLOR_YELLOW                                                                       = 2
%COLOR_BLUE                                                                         = 3
%MUSIC_FEVER                                                                        = 1
%NULL                                                                               = 0
%SRCCOPY                                                                            = 13369376
%VK_RETURN                                                                          = 13
%VK_ESCAPE                                                                          = 27
%VK_LEFT                                                                            = 37
%VK_UP                                                                              = 38
%VK_RIGHT                                                                           = 39
%VK_DOWN                                                                            = 40
%VK_X                                                                               = 88
%VK_Z                                                                               = 90
%WM_DESTROY                                                                         = 2
%WM_SETFOCUS                                                                        = 7
%WM_KILLFOCUS                                                                       = 8
%WM_PAINT                                                                           = 15
%WM_INITDIALOG                                                                      = 272
%WM_TIMER                                                                           = 275
%WS_OVERLAPPEDWINDOW                                                                = 13565952
%MB_ICONERROR                                                                       = 16
%FD_SETSIZE                                                                         = 64
%SND_ASYNC                                                                          = 1
%SND_RESOURCE                                                                       = 262148

TYPE PILL
    active AS LONG
    lfcolor AS LONG
    rtcolor AS LONG
    xa AS LONG
    ya AS LONG
    x AS LONG
    y AS LONG
    d AS LONG
    step AS LONG
    mome AS SINGLE
    dir AS LONG
END TYPE

TYPE GAMEDATA
    screen AS BYTE
    switchframe AS BYTE
    music AS BYTE
    musiclen AS DWORD
    musicloop AS DWORD
    musictimes AS LONG
    musicstart AS SINGLE
    focused AS LONG
    gameover AS LONG
    loseframe AS LONG
    stageclear AS LONG
    fanfare AS LONG
    setup AS LONG
    adjust AS LONG
    mtimer AS DWORD
    hmain AS DWORD
    lstart AS BYTE
    level AS BYTE
    speed AS BYTE
    nextpill AS PILL
    curpill AS PILL
    hitvirus AS LONG
    hitblock AS LONG
    pilldir AS LONG
    viruses AS BYTE
    hiscore AS DWORD
    score AS DWORD
    curframe AS LONG
    pillnum AS LONG
    pilldelay AS LONG
    rotated AS LONG
    moved AS LONG
    movestep AS LONG
    curbond AS LONG
    redhit AS LONG
    bluehit AS LONG
    yellowhit AS LONG
    paused AS LONG
    pauser AS LONG
    combo AS LONG
    menurow AS LONG
    mariostep AS LONG
    marioframe AS LONG
    throwpill AS LONG
    falling AS LONG
END TYPE

TYPE AREATYPE
    color AS LONG
    block AS LONG
    dir AS LONG
    step AS LONG
    bond AS LONG
END TYPE

TYPE BIGVIRUS
    x AS LONG
    y AS LONG
    yo AS LONG
    state AS LONG
    steps AS LONG
    frame AS BYTE
    color AS LONG
END TYPE

TYPE POINT
    x AS LONG
    y AS LONG
END TYPE

TYPE RECT_OLD
    nleft AS LONG
    ntop AS LONG
    nright AS LONG
    nbottom AS LONG
END TYPE

TYPE RECT_NEW
    left AS LONG
    top AS LONG
    right AS LONG
    bottom AS LONG
END TYPE

UNION RECT
    RECT_OLD
    RECT_NEW
END UNION

TYPE LVITEMA
    mask AS DWORD
    iitem AS LONG
    isubitem AS LONG
    state AS DWORD
    statemask AS DWORD
    psztext AS ASCIIZ PTR * 255
    cchtextmax AS LONG
    iimage AS LONG
    lparam AS LONG
    iindent AS LONG
END TYPE

TYPE LVITEM
    LVITEMA
END TYPE

TYPE LV_ITEM
    LVITEM
END TYPE

TYPE TVITEMA
    mask AS DWORD
    hitem AS DWORD
    state AS DWORD
    statemask AS DWORD
    psztext AS ASCIIZ PTR * 255
    cchtextmax AS LONG
    iimage AS LONG
    iselectedimage AS LONG
    cchildren AS LONG
    lparam AS LONG
END TYPE

TYPE TVITEM
    TVITEMA
END TYPE

TYPE PAINTSTRUCT
    hdc AS DWORD
    ferase AS LONG
    rcpaint AS RECT
    frestore AS LONG
    fincupdate AS LONG
    rgbreserved(31) AS BYTE
END TYPE


DECLARE FUNCTION BITBLT LIB "GDI32.DLL" ALIAS "BitBlt" (BYVAL P1 AS DWORD, BYVAL P2 AS LONG, BYVAL P3 AS LONG, BYVAL P4 AS LONG, BYVAL P5 AS LONG, BYVAL P6 AS DWORD,  _
                         BYVAL P7 AS LONG, BYVAL P8 AS LONG, BYVAL P9 AS DWORD) AS LONG
DECLARE FUNCTION CREATECOMPATIBLEBITMAP LIB "GDI32.DLL" ALIAS "CreateCompatibleBitmap" (BYVAL P1 AS DWORD, BYVAL P2 AS LONG, BYVAL P3 AS LONG) AS DWORD
DECLARE FUNCTION CREATECOMPATIBLEDC LIB "GDI32.DLL" ALIAS "CreateCompatibleDC" (BYVAL P1 AS DWORD) AS DWORD
DECLARE FUNCTION CREATESOLIDBRUSH LIB "GDI32.DLL" ALIAS "CreateSolidBrush" (BYVAL P1 AS DWORD) AS DWORD
DECLARE FUNCTION DELETEDC LIB "GDI32.DLL" ALIAS "DeleteDC" (BYVAL P1 AS DWORD) AS LONG
DECLARE FUNCTION DELETEOBJECT LIB "GDI32.DLL" ALIAS "DeleteObject" (BYVAL P1 AS DWORD) AS LONG
DECLARE FUNCTION SELECTOBJECT LIB "GDI32.DLL" ALIAS "SelectObject" (BYVAL P1 AS DWORD, BYVAL P2 AS DWORD) AS DWORD
DECLARE FUNCTION STRETCHBLT LIB "GDI32.DLL" ALIAS "StretchBlt" (BYVAL P1 AS DWORD, BYVAL P2 AS LONG, BYVAL P3 AS LONG, BYVAL P4 AS LONG, BYVAL P5 AS LONG, BYVAL P6 AS DWORD,  _
                         BYVAL P7 AS LONG, BYVAL P8 AS LONG, BYVAL P9 AS LONG, BYVAL P10 AS LONG, BYVAL P11 AS DWORD) AS LONG
DECLARE FUNCTION GETASYNCKEYSTATE LIB "User32.dll" ALIAS "GetAsyncKeyState" (BYVAL P1 AS LONG) AS INTEGER
DECLARE FUNCTION SETTIMER LIB "User32.dll" ALIAS "SetTimer" (BYVAL P1 AS DWORD, BYVAL P2 AS DWORD, BYVAL P3 AS DWORD, OPT BYVAL P4 AS DWORD) AS DWORD
DECLARE FUNCTION KILLTIMER LIB "User32.dll" ALIAS "KillTimer" (BYVAL P1 AS DWORD, BYVAL P2 AS DWORD) AS LONG
DECLARE FUNCTION BEGINPAINT LIB "User32.dll" ALIAS "BeginPaint" (BYVAL P1 AS DWORD, P2 AS PAINTSTRUCT) AS DWORD
DECLARE FUNCTION ENDPAINT LIB "User32.dll" ALIAS "EndPaint" (BYVAL P1 AS DWORD, P2 AS PAINTSTRUCT) AS LONG
DECLARE FUNCTION INVALIDATERECT LIB "User32.dll" ALIAS "InvalidateRect" (BYVAL P1 AS DWORD, P2 AS RECT, BYVAL P3 AS LONG) AS LONG
DECLARE FUNCTION GETCLIENTRECT LIB "User32.dll" ALIAS "GetClientRect" (BYVAL P1 AS DWORD, P2 AS RECT) AS LONG
DECLARE FUNCTION FILLRECT LIB "User32.dll" ALIAS "FillRect" (BYVAL P1 AS DWORD, P2 AS RECT, BYVAL P3 AS DWORD) AS LONG
DECLARE FUNCTION LOADBITMAP LIB "User32.dll" ALIAS "LoadBitmapA" (BYVAL P1 AS DWORD, P2 AS ASCIIZ) AS DWORD
DECLARE FUNCTION SNDPLAYSOUND LIB "WINMM.DLL" ALIAS "sndPlaySoundA" (P1 AS ASCIIZ, BYVAL P2 AS DWORD) AS LONG
DECLARE FUNCTION MCISENDSTRING LIB "WINMM.DLL" ALIAS "mciSendStringA" (P1 AS ASCIIZ, P2 AS ASCIIZ, BYVAL P3 AS DWORD, BYVAL P4 AS DWORD) AS DWORD
DECLARE FUNCTION TESTBGM(BYVAL P1 AS STRING) AS LONG
DECLARE FUNCTION PLAYBGM(BYVAL P1 AS STRING, BYVAL P2 AS LONG, BYVAL P3 AS LONG, BYVAL P4 AS LONG) AS LONG
DECLARE FUNCTION CHANGEGAMESCREEN(BYVAL P1 AS LONG) AS LONG
DECLARE FUNCTION SETAREAFORLEVEL(BYVAL P1 AS LONG) AS LONG
DECLARE SUB INITGAME() 
DECLARE FUNCTION PILLCANGO(BYVAL P1 AS LONG) AS LONG
DECLARE FUNCTION LEAVEPILL() AS LONG
DECLARE FUNCTION MOVEPILL(BYVAL P1 AS LONG) AS LONG
DECLARE FUNCTION CANROTATE() AS LONG
DECLARE FUNCTION CHECKVERT(BYVAL P1 AS LONG, BYVAL P2 AS LONG) AS LONG
DECLARE FUNCTION REMAININGVIRUS(BYVAL P1 AS LONG) AS LONG
DECLARE FUNCTION CHECKHORZ(BYVAL P1 AS LONG, BYVAL P2 AS LONG) AS LONG
DECLARE FUNCTION INDIVIDUALIZEBLOCKS() AS LONG
DECLARE FUNCTION DESTROYED() AS LONG
DECLARE FUNCTION BLOCKFELL() AS LONG
DECLARE FUNCTION ROTATEPILL(BYVAL P1 AS LONG) AS LONG
DECLARE FUNCTION EXECGAMEMECHANICS() AS LONG
DECLARE FUNCTION PBMAIN() AS LONG
DECLARE CALLBACK FUNCTION DLGPROC() AS LONG
GLOBAL hvirbg  AS DWORD
GLOBAL hvirsm  AS DWORD
GLOBAL hmario  AS DWORD
GLOBAL hnumbr  AS DWORD
GLOBAL hbackg  AS DWORD
GLOBAL htitle  AS DWORD
GLOBAL hselec  AS DWORD
GLOBAL hhighl  AS DWORD
GLOBAL hendin  AS DWORD
GLOBAL config  AS GAMEDATA
GLOBAL area()  AS AREATYPE
GLOBAL bigv()  AS BIGVIRUS
GLOBAL area()  AS AREATYPE
GLOBAL bigv()  AS BIGVIRUS
GLOBAL default_form  AS STRING

' STARTS PLURIBASIC_PREPARE.BIN

FUNCTION OPE_SHIFTLEFT(BYVAL V1 AS QUAD, BYVAL V2 AS QUAD) AS QUAD
    SHIFT LEFT V1, V2
    FUNCTION = V1
END FUNCTION

FUNCTION OPE_SHIFTRIGHT(BYVAL V1 AS QUAD, BYVAL V2 AS QUAD) AS QUAD
    SHIFT RIGHT V1, V2
    FUNCTION = V1
END FUNCTION

' END OF PLURIBASIC_PREPARE.BIN
' STARTS PLURIBASIC_INIT.BIN
' 
FUNCTION PLURIBASIC_INIT( ) AS LONG

END FUNCTION

' END OF PLURIBASIC_INIT.BIN

' Initializes various things in the script.
FUNCTION PluriBASIC_Initialize() AS LONG
   DIM SYSTEM_UDT_OFFSETS(0) AS GLOBAL STRING
END FUNCTION

FUNCTION TESTBGM(BYVAL resname AS STRING) AS LONG
   LOCAL ff AS LONG
   LOCAL cont AS STRING
   ERRCLEAR 
   ff = FREEFILE 
   OPEN resname & ".mp3" FOR BINARY AS #ff 
   IF ISFALSE(ERR) THEN
      cont = RESOURCE$(RCDATA, resname) 
      ERRCLEAR 
      PUT #ff,, cont
      SETEOF #ff 
      CLOSE #ff 
   END IF
   FUNCTION = (ERR=0) 
END FUNCTION

FUNCTION PLAYBGM(BYVAL resname AS STRING,  _ 
                 BYVAL musiclen AS LONG,  _ 
                 BYVAL musicloop AS LONG,  _ 
                 BYVAL adjust AS LONG) AS LONG
   LOCAL temp AS ASCIIZ * 255
   LOCAL r AS LONG
   LOCAL ff AS LONG
   LOCAL cont AS STRING
   STATIC playing AS LONG
   STATIC resplay AS STRING
   STATIC pmusiclen AS LONG
   STATIC pmusicloop AS LONG
   STATIC padjust AS LONG
   IF ISFALSE(LEN(DIR$(resname & ".mp3"))) THEN
      ff = FREEFILE 
      ERRCLEAR 
      OPEN resname & ".mp3" FOR BINARY AS #ff 
      cont = RESOURCE$(RCDATA, resname) 
      ERRCLEAR 
      PUT #ff,, cont
      SETEOF #ff 
      CLOSE #ff 
   END IF
   IF playing THEN
      IF config.mtimer THEN
         KILLTIMER config.hmain, config.mtimer 
      END IF
      MCISENDSTRING("stop mp3file", BYVAL 0, 0, 0) 
      MCISENDSTRING("close mp3file", BYVAL 0, 0, 0) 
   END IF
   IF resname="pause" THEN
      playing = 0 
      EXIT FUNCTION
   ELSEIF resname="resume" THEN
      resname = resplay 
      musiclen = pmusiclen 
      musicloop = pmusicloop 
      adjust = padjust 
   ELSE
      resplay = resname 
      pmusiclen = musiclen 
      pmusicloop = musicloop 
      padjust = adjust 
   END IF
   r = MCISENDSTRING("open " + resname + ".mp3 type MPEGVideo alias mp3file", BYVAL %NULL, 0, BYVAL %NULL) 
   IF r=0 THEN
      playing = -1 
      MCISENDSTRING("status mp3file length", temp, 255, BYVAL %NULL) 
      config.musiclen = musiclen 
      MCISENDSTRING("play mp3file", BYVAL 0, 0, 0) 
      config.mtimer = SETTIMER(config.hmain, %TIMER_MUSICLOOP, config.musiclen + 3000, BYVAL %NULL) 
      config.musicloop = musicloop 
      config.musictimes = 0 
      config.adjust = adjust 
      config.musicstart = TIMER 
      FUNCTION = VAL(temp) 
   ELSE
      playing = 0 
   END IF
END FUNCTION

FUNCTION CHANGEGAMESCREEN(BYVAL scrn AS LONG) AS LONG
   SELECT CASE scrn
      CASE %SCREEN_TITLE
         PLAYBGM("SFX21", 23060, 11839, -900) 
         config.gameover = 0 
         bigv(4).state = %VIRUS_NORMAL 
         bigv(4).x = 192 
         bigv(4).y = 163 
         bigv(4).color = %COLOR_BLUE 
         config.screen = scrn 
      CASE %SCREEN_OPTIONS
         config.menurow = 1 
         PLAYBGM("SFX23", 16350, 270, -100) 
         config.screen = scrn 
      CASE %SCREEN_GAME
         config.gameover = 0 
         config.stageclear = 0 
         config.paused = 0 
         SELECT CASE config.music
            CASE 1
               PLAYBGM("SFX24", 64700, 2600, 1000) 
            CASE 2
               PLAYBGM("SFX22", 116000, 7523, -4000) 
            CASE 3
               PLAYBGM("none", 0, 0, 0) 
         END SELECT
         bigv(1).x = 41 
         bigv(1).y = 133 
         bigv(1).color = %COLOR_RED 
         bigv(2).x = 15 
         bigv(2).y = 146 
         bigv(2).color = %COLOR_YELLOW 
         bigv(3).x = 40 
         bigv(3).y = 163 
         bigv(3).color = %COLOR_BLUE 
         config.lstart = config.level 
         SETAREAFORLEVEL(config.lstart) 
         config.screen = scrn 
      CASE %SCREEN_ENDING
         config.curframe = 0 
         PLAYBGM("SFX28", 99999, 0, 0) 
         config.screen = scrn 
      CASE ELSE
   END SELECT
END FUNCTION

FUNCTION SETAREAFORLEVEL(BYVAL lvl AS LONG) AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   LOCAL thecolor AS LONG
   LOCAL numvirus AS LONG
   LOCAL numblock AS LONG
   LOCAL highest AS LONG
   RANDOMIZE TIMER 
   IF lvl < 0 THEN
      lvl = 0 
   ELSEIF lvl > 20 THEN
      lvl = 20 
   END IF
   config.setup = -1 
   DO
      FOR x = 0 TO 9
         FOR y = 0 TO 17
            RESET area(x,y)
         NEXT 
      NEXT 
      thecolor = 0 
      numvirus = ((lvl + 1) * 4) 
      config.viruses = numvirus 
      highest = (12 - (lvl / 3)) 
      IF y < 3 THEN
         y = 3 
      END IF
      DO WHILE numvirus
         x = RND(1, 8) 
         y = RND(highest, 16) 
         IF area(x,y).block=0 THEN
            numvirus = ((numvirus) - 1) 
            thecolor = ((thecolor) + 1) 
            area(x,y).dir = 0 
            area(x,y).block = %AREA_VIRUS 
            IF thecolor > 3 THEN
               area(x,y).color = RND(1, 3) 
            ELSE
               area(x,y).color = thecolor 
            END IF
         END IF
      IterLabel0379:
      LOOP
      IF ISFALSE(DESTROYED()) THEN
         EXIT DO
      END IF
   IterLabel0376:
   LOOP
   config.setup = 0 
   config.curframe = 0 
   config.pillnum = 0 
   config.marioframe = 3 
   config.mariostep = 0 
   config.combo = 0 
   config.pilldelay = (30 - ((config.speed - 1) * 5)) 
   IF config.pilldelay < 3 THEN
      config.pilldelay = 3 
   END IF
   config.nextpill.step = 0 
   config.nextpill.x = 188 
   config.nextpill.y = 64 
   config.nextpill.dir = %DIR_RT 
   config.nextpill.lfcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
   config.nextpill.rtcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
   bigv(1).state = %VIRUS_NORMAL 
   bigv(1).steps = 0 
   bigv(2).state = %VIRUS_NORMAL 
   bigv(2).steps = 0 
   bigv(3).state = %VIRUS_NORMAL 
   bigv(3).steps = 0 
   RESET config.curpill
END FUNCTION

SUB INITGAME()
   DIM area(9, 17) AS GLOBAL areatype
   DIM bigv(4) AS GLOBAL BigVirus
END SUB

FUNCTION PILLCANGO(BYVAL dir AS LONG) AS LONG
   SELECT CASE dir
      CASE %DIR_DN
         IF config.curpill.ya > 15 THEN
            EXIT FUNCTION
         END IF
         IF area(config.curpill.xa,config.curpill.ya + 1).color THEN
            EXIT FUNCTION
         END IF
         SELECT CASE config.curpill.dir
            CASE %DIR_LF, %DIR_RT
               IF area(config.curpill.xa + 1,config.curpill.ya + 1).color THEN
                  EXIT FUNCTION
               END IF
         END SELECT
      CASE %DIR_LF
         IF config.curpill.xa=1 THEN
            EXIT FUNCTION
         END IF
         SELECT CASE config.curpill.dir
            CASE %DIR_UP, %DIR_DN
               IF area(config.curpill.xa - 1,config.curpill.ya - 1).color THEN
                  EXIT FUNCTION
               END IF
               IF area(config.curpill.xa - 1,config.curpill.ya).color THEN
                  EXIT FUNCTION
               END IF
            CASE %DIR_LF, %DIR_RT
               IF area(config.curpill.xa - 1,config.curpill.ya).color THEN
                  EXIT FUNCTION
               END IF
         END SELECT
      CASE %DIR_RT
         SELECT CASE config.curpill.dir
            CASE %DIR_UP, %DIR_DN
               IF config.curpill.xa > 7 THEN
                  EXIT FUNCTION
               END IF
               IF area(config.curpill.xa + 1,config.curpill.ya - 1).color THEN
                  EXIT FUNCTION
               END IF
               IF area(config.curpill.xa + 1,config.curpill.ya).color THEN
                  EXIT FUNCTION
               END IF
            CASE %DIR_LF, %DIR_RT
               IF config.curpill.xa > 6 THEN
                  EXIT FUNCTION
               END IF
               IF area(config.curpill.xa + 2,config.curpill.ya).color THEN
                  EXIT FUNCTION
               END IF
         END SELECT
   END SELECT
   FUNCTION = -1 
END FUNCTION

FUNCTION LEAVEPILL() AS LONG
   LOCAL x AS LONG
   config.curbond = ((config.curbond) + 1) 
   SELECT CASE config.curpill.dir
      CASE %DIR_UP
         area(config.curpill.xa,config.curpill.ya + 0).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya + 0).color = config.curpill.lfcolor 
         area(config.curpill.xa,config.curpill.ya + 0).dir = %DIR_DN 
         area(config.curpill.xa,config.curpill.ya + 0).bond = config.curbond 
         area(config.curpill.xa,config.curpill.ya - 1).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya - 1).color = config.curpill.rtcolor 
         area(config.curpill.xa,config.curpill.ya - 1).dir = %DIR_UP 
         area(config.curpill.xa,config.curpill.ya - 1).bond = config.curbond 
      CASE %DIR_DN
         area(config.curpill.xa,config.curpill.ya + 0).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya + 0).color = config.curpill.rtcolor 
         area(config.curpill.xa,config.curpill.ya + 0).dir = %DIR_DN 
         area(config.curpill.xa,config.curpill.ya + 0).bond = config.curbond 
         area(config.curpill.xa,config.curpill.ya - 1).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya - 1).color = config.curpill.lfcolor 
         area(config.curpill.xa,config.curpill.ya - 1).dir = %DIR_UP 
         area(config.curpill.xa,config.curpill.ya - 1).bond = config.curbond 
      CASE %DIR_RT
         area(config.curpill.xa,config.curpill.ya).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya).color = config.curpill.lfcolor 
         area(config.curpill.xa,config.curpill.ya).dir = %DIR_LF 
         area(config.curpill.xa,config.curpill.ya).bond = config.curbond 
         area(config.curpill.xa + 1,config.curpill.ya).block = %AREA_PILL 
         area(config.curpill.xa + 1,config.curpill.ya).color = config.curpill.rtcolor 
         area(config.curpill.xa + 1,config.curpill.ya).dir = %DIR_RT 
         area(config.curpill.xa + 1,config.curpill.ya).bond = config.curbond 
      CASE %DIR_LF
         area(config.curpill.xa,config.curpill.ya).block = %AREA_PILL 
         area(config.curpill.xa,config.curpill.ya).color = config.curpill.rtcolor 
         area(config.curpill.xa,config.curpill.ya).dir = %DIR_LF 
         area(config.curpill.xa,config.curpill.ya).bond = config.curbond 
         area(config.curpill.xa + 1,config.curpill.ya).block = %AREA_PILL 
         area(config.curpill.xa + 1,config.curpill.ya).color = config.curpill.lfcolor 
         area(config.curpill.xa + 1,config.curpill.ya).dir = %DIR_RT 
         area(config.curpill.xa + 1,config.curpill.ya).bond = config.curbond 
   END SELECT
   FOR x = 1 TO 8
      RESET area(x,0)
   NEXT 
END FUNCTION

FUNCTION MOVEPILL(BYVAL dir AS LONG) AS LONG
   SELECT CASE dir
      CASE %DIR_DN
         config.curpill.y = ((config.curpill.y) + 8) 
         config.curpill.ya = ((config.curpill.ya) + 1) 
      CASE %DIR_LF
         config.curpill.x = ((config.curpill.x) - 8) 
         config.curpill.xa = ((config.curpill.xa) - 1) 
         IF ISFALSE(config.fanfare) THEN
            SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
         END IF
      CASE %DIR_RT
         config.curpill.x = ((config.curpill.x) + 8) 
         config.curpill.xa = ((config.curpill.xa) + 1) 
         IF ISFALSE(config.fanfare) THEN
            SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
         END IF
   END SELECT
END FUNCTION

FUNCTION CANROTATE() AS LONG
   SELECT CASE config.curpill.dir
      CASE %DIR_LF, %DIR_RT
         IF area(config.curpill.xa,config.curpill.ya - 1).color THEN
            IF area(config.curpill.xa,config.curpill.ya + 1).color THEN
               EXIT FUNCTION
            ELSE
               MOVEPILL(%DIR_DN) 
            END IF
         END IF
      CASE %DIR_UP, %DIR_DN
         IF ISTRUE(config.curpill.xa > 7) OR ISTRUE(area(config.curpill.xa + 1,config.curpill.ya).color) THEN
            IF area(config.curpill.xa - 1,config.curpill.ya).color THEN
               EXIT FUNCTION
            ELSE
               MOVEPILL(%DIR_LF) 
            END IF
         END IF
   END SELECT
   FUNCTION = -1 
END FUNCTION

FUNCTION CHECKVERT(BYVAL x AS LONG,  _ 
                   BYVAL y AS LONG) AS LONG
   LOCAL i AS LONG
   LOCAL v AS LONG
   LOCAL e AS LONG
   LOCAL hv AS LONG
   LOCAL hb AS LONG
   IF ISFALSE(area(x,y).color) THEN
      EXIT FUNCTION
   END IF
   FOR i = y TO (y + 16)
      IF i > 16 THEN
         EXIT FOR
      END IF
      IF area(x,i).color=area(x,y).color THEN
         IF area(x,i).block=%AREA_VIRUS THEN
            hv = ((hv) + 1) 
         ELSE
            hb = ((hb) + 1) 
         END IF
         e = ((e) + 1) 
      ELSE
         EXIT FOR
      END IF
   NEXT 
   IF e >= 4 THEN
      IF ISFALSE(config.setup) THEN
         config.hitvirus = ((config.hitvirus) + hv) 
         config.hitblock = ((config.hitblock) + hb) 
      END IF
      FOR i = y TO (y + 16)
         IF i > 16 THEN
            EXIT FOR
         END IF
         IF area(x,i).color=area(x,y).color THEN
            IF area(x,i).block=%AREA_VIRUS THEN
               IF area(x,i).color=%COLOR_RED THEN
                  config.redhit = 1 
               END IF
               IF area(x,i).color=%COLOR_BLUE THEN
                  config.bluehit = 1 
               END IF
               IF area(x,i).color=%COLOR_YELLOW THEN
                  config.yellowhit = 1 
               END IF
               v = -1 
            END IF
            area(x,i).block = %AREA_BLOCK 
            area(x,i).bond = 0 
            area(x,i).step = 1 
         ELSE
            EXIT FOR
         END IF
      NEXT 
      IF v THEN
         FUNCTION = 2 
      ELSE
         FUNCTION = 1 
      END IF
   END IF
END FUNCTION

FUNCTION REMAININGVIRUS(BYVAL clr AS LONG) AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   LOCAL nvirus AS LONG
   IF clr=0 THEN
      FOR x = 1 TO 8
         FOR y = 1 TO 16
            IF area(x,y).block=%AREA_VIRUS THEN
               nvirus = ((nvirus) + 1) 
            END IF
         NEXT 
      NEXT 
   ELSE
      FOR x = 1 TO 8
         FOR y = 1 TO 16
            IF area(x,y).block=%AREA_VIRUS THEN
               IF area(x,y).color=clr THEN
                  nvirus = ((nvirus) + 1) 
               END IF
            END IF
         NEXT 
      NEXT 
   END IF
   FUNCTION = nvirus 
END FUNCTION

FUNCTION CHECKHORZ(BYVAL x AS LONG,  _ 
                   BYVAL y AS LONG) AS LONG
   LOCAL i AS LONG
   LOCAL v AS LONG
   LOCAL e AS LONG
   LOCAL hv AS LONG
   LOCAL hb AS LONG
   IF ISFALSE(area(x,y).color) THEN
      EXIT FUNCTION
   END IF
   FOR i = x TO (x + 8)
      IF i > 8 THEN
         EXIT FOR
      END IF
      IF area(i,y).color=area(x,y).color THEN
         IF area(x,i).block=%AREA_VIRUS THEN
            hv = ((hv) + 1) 
         ELSE
            hb = ((hb) + 1) 
         END IF
         e = ((e) + 1) 
      ELSE
         EXIT FOR
      END IF
   NEXT 
   IF e >= 4 THEN
      IF ISFALSE(config.setup) THEN
         config.hitvirus = ((config.hitvirus) + hv) 
         config.hitblock = ((config.hitblock) + hb) 
      END IF
      FOR i = x TO (x + 8)
         IF i > 8 THEN
            EXIT FOR
         END IF
         IF area(i,y).color=area(x,y).color THEN
            IF area(i,y).block=%AREA_VIRUS THEN
               IF area(i,y).color=%COLOR_RED THEN
                  config.redhit = 1 
               END IF
               IF area(i,y).color=%COLOR_BLUE THEN
                  config.bluehit = 1 
               END IF
               IF area(i,y).color=%COLOR_YELLOW THEN
                  config.yellowhit = 1 
               END IF
               v = -1 
            END IF
            area(i,y).block = %AREA_BLOCK 
            area(i,y).bond = 0 
            area(i,y).step = 1 
         ELSE
            EXIT FOR
         END IF
      NEXT 
      IF v THEN
         FUNCTION = 2 
      ELSE
         FUNCTION = 1 
      END IF
   END IF
END FUNCTION

FUNCTION INDIVIDUALIZEBLOCKS() AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   FOR x = 1 TO 8
      FOR y = 1 TO 16
         IF area(x,y).block=%AREA_PILL THEN
            IF area(x,y).bond=area(x - 1,y).bond THEN
               ITERATE FOR
            END IF
            IF area(x,y).bond=area(x + 1,y).bond THEN
               ITERATE FOR
            END IF
            IF area(x,y).bond=area(x,y - 1).bond THEN
               ITERATE FOR
            END IF
            IF area(x,y).bond=area(x,y + 1).bond THEN
               ITERATE FOR
            END IF
            area(x,y).bond = 0 
            area(x,y).block = %AREA_HALFPILL 
         END IF
      NEXT 
   NEXT 
END FUNCTION

FUNCTION DESTROYED() AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   LOCAL r AS LONG
   LOCAL v AS LONG
   LOCAL boom AS LONG
   config.redhit = 0 
   config.bluehit = 0 
   config.yellowhit = 0 
   config.hitvirus = 0 
   config.hitblock = 0 
   FOR x = 1 TO 6
      FOR y = 1 TO 16
         r = CHECKHORZ(x, y) 
         IF r THEN
            IF r=2 THEN
               v = -1 
            END IF
            boom = -1 
         END IF
      NEXT 
   NEXT 
   FOR x = 1 TO 8
      FOR y = 1 TO 13
         r = CHECKVERT(x, y) 
         IF r THEN
            IF r=2 THEN
               v = -1 
            END IF
            boom = -1 
         END IF
      NEXT 
   NEXT 
   IF boom THEN
      IF config.setup THEN
         FUNCTION = 1 
         EXIT FUNCTION
      END IF
      config.combo = ((config.combo) + 1) 
      config.score = ((config.score) + ((config.hitvirus * 100) * config.combo)) 
      config.score = ((config.score) + ((config.hitblock * 10) * config.combo)) 
      IF v THEN
         IF config.redhit THEN
            bigv(1).state = %VIRUS_FLOOR 
            bigv(1).steps = 0 
         END IF
         IF config.yellowhit THEN
            bigv(2).state = %VIRUS_FLOOR 
            bigv(2).steps = 0 
         END IF
         IF config.bluehit THEN
            bigv(3).state = %VIRUS_FLOOR 
            bigv(3).steps = 0 
         END IF
         INDIVIDUALIZEBLOCKS() 
         SNDPLAYSOUND(bycopy "sfx" & FORMAT$(9), %SND_RESOURCE OR %SND_ASYNC) 
         FUNCTION = 2 
      ELSE
         INDIVIDUALIZEBLOCKS() 
         SNDPLAYSOUND(bycopy "sfx" & FORMAT$(8), %SND_RESOURCE OR %SND_ASYNC) 
         FUNCTION = 1 
      END IF
      SELECT CASE config.combo
         CASE 3
            config.fanfare = 1 
            SNDPLAYSOUND(bycopy "sfx" & FORMAT$(12), %SND_RESOURCE OR %SND_ASYNC) 
         CASE 4
            config.fanfare = 1 
            SNDPLAYSOUND(bycopy "sfx" & FORMAT$(15), %SND_RESOURCE OR %SND_ASYNC) 
      END SELECT
   END IF
END FUNCTION

FUNCTION BLOCKFELL() AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   LOCAL f AS LONG
   FOR y = 15 TO 1 STEP -1
      FOR x = 8 TO 1 STEP -1
         SELECT CASE area(x,y).block
            CASE %AREA_HALFPILL
               IF area(x,y + 1).block=0 THEN
                  area(x,y + 1) = area(x,y) 
                  RESET area(x,y)
                  f = 1 
               END IF
            CASE %AREA_PILL
               IF area(x,y).bond=area(x - 1,y).bond THEN
                  IF ISFALSE(area(x - 1,y + 1).block) AND ISFALSE(area(x,y + 1).block) THEN
                     area(x,y + 1) = area(x,y) 
                     RESET area(x,y)
                     area(x - 1,y + 1) = area(x - 1,y) 
                     RESET area(x - 1,y)
                     f = 1 
                  END IF
                  x = ((x) - 1) 
               ELSE
                  IF ISFALSE(area(x,y + 1).block) THEN
                     area(x,y + 1) = area(x,y) 
                     RESET area(x,y)
                     f = 1 
                  END IF
               END IF
         END SELECT
      NEXT 
   NEXT 
   FUNCTION = f 
END FUNCTION

FUNCTION ROTATEPILL(BYVAL dir AS LONG) AS LONG
   IF ISFALSE(config.fanfare) THEN
      SNDPLAYSOUND(bycopy "sfx" & FORMAT$(4), %SND_RESOURCE OR %SND_ASYNC) 
   END IF
   SELECT CASE dir
      CASE %DIR_LF
         config.rotated = 1 
         config.curpill.dir = ((config.curpill.dir) - 1) 
         IF config.curpill.dir < 1 THEN
            config.curpill.dir = 4 
         END IF
      CASE %DIR_RT
         config.rotated = 1 
         config.curpill.dir = ((config.curpill.dir) + 1) 
         IF config.curpill.dir > 4 THEN
            config.curpill.dir = 1 
         END IF
   END SELECT
END FUNCTION

FUNCTION EXECGAMEMECHANICS() AS LONG
   LOCAL r AS LONG
   IF config.focused THEN
      SELECT CASE config.screen
         CASE %SCREEN_TITLE
            config.curframe = ((config.curframe) + 1) 
            IF GETASYNCKEYSTATE(%VK_RETURN) THEN
               DO WHILE GETASYNCKEYSTATE(%VK_RETURN)
                  SLEEP 100 
               IterLabel0491:
               LOOP
               CHANGEGAMESCREEN(%SCREEN_OPTIONS) 
            END IF
         CASE %SCREEN_OPTIONS
            config.curframe = ((config.curframe) + 1) 
            IF config.moved THEN
               config.movestep = ((config.movestep) + 1) 
            END IF
            IF GETASYNCKEYSTATE(%VK_ESCAPE) THEN
               IF config.moved=0 THEN
                  CHANGEGAMESCREEN(%SCREEN_TITLE) 
               END IF
            ELSEIF GETASYNCKEYSTATE(%VK_UP) THEN
               IF config.moved=0 THEN
                  IF config.menurow > 1 THEN
                     SNDPLAYSOUND(bycopy "sfx" & FORMAT$(2), %SND_RESOURCE OR %SND_ASYNC) 
                     config.menurow = ((config.menurow) - 1) 
                     config.moved = 1 
                  END IF
               END IF
            ELSEIF GETASYNCKEYSTATE(%VK_DOWN) THEN
               IF config.moved=0 THEN
                  IF config.menurow < 3 THEN
                     SNDPLAYSOUND(bycopy "sfx" & FORMAT$(2), %SND_RESOURCE OR %SND_ASYNC) 
                     config.menurow = ((config.menurow) + 1) 
                     config.moved = 1 
                  END IF
               END IF
            ELSEIF GETASYNCKEYSTATE(%VK_LEFT) THEN
               SELECT CASE config.menurow
                  CASE 1
                     IF (config.moved=0) OR (config.movestep > 3) THEN
                        IF config.level > 0 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.level = ((config.level) - 1) 
                           IF config.moved=0 THEN
                              config.movestep = -15 
                           ELSE
                              config.movestep = 0 
                           END IF
                           config.moved = 1 
                        END IF
                     END IF
                  CASE 2
                     IF (config.moved=0) THEN
                        IF config.speed > 1 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.speed = ((config.speed) - 1) 
                           config.moved = 1 
                        END IF
                     END IF
                  CASE 3
                     IF (config.moved=0) THEN
                        IF config.music > 1 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.music = ((config.music) - 1) 
                           config.moved = 1 
                        END IF
                     END IF
               END SELECT
            ELSEIF GETASYNCKEYSTATE(%VK_RIGHT) THEN
               SELECT CASE config.menurow
                  CASE 1
                     IF (config.moved=0) OR (config.movestep > 3) THEN
                        IF config.level < 20 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.level = ((config.level) + 1) 
                           IF config.moved=0 THEN
                              config.movestep = -10 
                           ELSE
                              config.movestep = 0 
                           END IF
                           config.moved = 1 
                        END IF
                     END IF
                  CASE 2
                     IF (config.moved=0) THEN
                        IF config.speed < 3 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.speed = ((config.speed) + 1) 
                           config.moved = 1 
                        END IF
                     END IF
                  CASE 3
                     IF (config.moved=0) THEN
                        IF config.music < 3 THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(0), %SND_RESOURCE OR %SND_ASYNC) 
                           config.music = ((config.music) + 1) 
                           config.moved = 1 
                        END IF
                     END IF
               END SELECT
            ELSEIF GETASYNCKEYSTATE(%VK_RETURN) THEN
               IF config.moved=0 THEN
                  DO WHILE GETASYNCKEYSTATE(%VK_RETURN)
                     SLEEP 100 
                  IterLabel0516:
                  LOOP
                  CHANGEGAMESCREEN(%SCREEN_GAME) 
               END IF
            ELSE
               config.moved = 0 
            END IF
         CASE %SCREEN_GAME
            config.curframe = ((config.curframe) + 1) 
            IF ISFALSE(config.stageclear) THEN
               IF ISFALSE(config.paused) THEN
                  IF config.fanfare THEN
                     config.fanfare = ((config.fanfare) + 1) 
                     IF config.fanfare > 40 THEN
                        config.fanfare = 0 
                     END IF
                  END IF
               END IF
            END IF
            IF GETASYNCKEYSTATE(%VK_RETURN) THEN
               IF ISFALSE(config.stageclear) THEN
                  IF ISFALSE(config.pauser) THEN
                     config.pauser = 1 
                     config.paused = (config.paused=0) 
                     IF config.paused THEN
                        SNDPLAYSOUND(bycopy "sfx" & FORMAT$(5), %SND_RESOURCE OR %SND_ASYNC) 
                        PLAYBGM("pause", 0, 0, 0) 
                     ELSE
                        PLAYBGM("resume", 0, 0, 0) 
                     END IF
                  END IF
               END IF
            ELSE
               config.pauser = 0 
            END IF
            IF config.paused THEN
            ELSEIF config.curframe > 80 THEN
               IF config.gameover THEN
                  config.curpill.active = 0 
                  config.loseframe = ((config.loseframe) + 1) 
                  IF config.loseframe > 60 THEN
                     IF GETASYNCKEYSTATE(%VK_RETURN) THEN
                        DO WHILE GETASYNCKEYSTATE(%VK_RETURN)
                           SLEEP 100 
                        IterLabel0529:
                        LOOP
                        CHANGEGAMESCREEN(%SCREEN_TITLE) 
                     END IF
                  END IF
               ELSEIF config.stageclear THEN
                  config.curpill.active = 0 
                  config.loseframe = ((config.loseframe) + 1) 
                  IF config.loseframe > 60 THEN
                     IF GETASYNCKEYSTATE(%VK_RETURN) THEN
                        IF config.lstart=20 THEN
                           CHANGEGAMESCREEN(%SCREEN_ENDING) 
                        ELSE
                           config.pauser = -1 
                           config.gameover = 0 
                           config.stageclear = 0 
                           config.curframe = 0 
                           SELECT CASE config.music
                              CASE 1
                                 PLAYBGM("SFX24", 64700, 2600, 1000) 
                              CASE 2
                                 PLAYBGM("SFX22", 116000, 7523, -4000) 
                              CASE 3
                                 PLAYBGM("none", 0, 0, 0) 
                           END SELECT
                           bigv(1).state = %VIRUS_NORMAL 
                           bigv(2).state = %VIRUS_NORMAL 
                           bigv(3).state = %VIRUS_NORMAL 
                           config.pillnum = 0 
                           config.throwpill = 0 
                           config.curpill.step = 0 
                           config.falling = 0 
                           config.lstart = ((config.lstart) + 1) 
                           SETAREAFORLEVEL(config.lstart) 
                           config.nextpill.lfcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
                           config.nextpill.rtcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
                        END IF
                     END IF
                  END IF
               ELSEIF config.pillnum=0 THEN
                  config.pillnum = ((config.pillnum) + 1) 
                  config.throwpill = 1 
                  config.mariostep = 0 
                  config.nextpill.step = 1 
                  config.nextpill.x = 188 
                  config.nextpill.y = 64 
                  config.nextpill.mome = -8 
               ELSEIF config.throwpill THEN
                  config.mariostep = ((config.mariostep) + 1) 
                  config.nextpill.mome = ((config.nextpill.mome) + 0.69) 
                  config.nextpill.dir = ((config.nextpill.dir) + 1) 
                  IF config.nextpill.dir > 4 THEN
                     config.nextpill.dir = 1 
                  END IF
                  config.nextpill.x = ((config.nextpill.x) - 3) 
                  config.nextpill.y = ((config.nextpill.y) + config.nextpill.mome) 
                  GETASYNCKEYSTATE(%VK_DOWN) 
                  GETASYNCKEYSTATE(%VK_LEFT) 
                  GETASYNCKEYSTATE(%VK_RIGHT) 
                  SELECT CASE config.mariostep
                     CASE 4
                        config.marioframe = 4 
                     CASE 8
                        config.marioframe = 5 
                     CASE 23
                        config.marioframe = 3 
                        config.throwpill = 0 
                        config.curpill.step = 0 
                        config.curpill = config.nextpill 
                        config.curpill.x = 120 
                        config.curpill.y = 72 
                        config.curpill.xa = 4 
                        config.curpill.ya = 1 
                        config.curpill.dir = %DIR_RT 
                        IF ISTRUE(area(4,1).block) OR ISTRUE(area(5,1).block) THEN
                           LEAVEPILL() 
                           PLAYBGM("SFX20", 9999099, 9909999, 0) 
                           config.gameover = -1 
                           config.curpill.active = 0 
                           config.loseframe = 0 
                        ELSE
                           config.curpill.active = 1 
                           config.nextpill.x = 188 
                           config.nextpill.y = 64 
                           config.nextpill.lfcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
                           config.nextpill.rtcolor = CHOOSE&(RND(1, 3), %COLOR_RED, %COLOR_YELLOW, %COLOR_BLUE) 
                           config.nextpill.dir = %DIR_RT 
                           config.nextpill.step = 0 
                           config.nextpill.mome = -8 
                           config.mariostep = 0 
                        END IF
                  END SELECT
               END IF
            END IF
            IF config.paused THEN
            ELSEIF config.curpill.active THEN
               IF (config.curframe MOD 2000)=0 THEN
                  IF ISFALSE(config.stageclear) THEN
                     config.pilldelay = ((config.pilldelay) - config.speed) 
                     IF ISFALSE(config.fanfare) THEN
                        config.fanfare = 1 
                        SNDPLAYSOUND(bycopy "sfx" & FORMAT$(7), %SND_RESOURCE OR %SND_ASYNC) 
                     END IF
                     IF config.pilldelay < 1 THEN
                        config.pilldelay = 1 
                     END IF
                  END IF
               END IF
               IF config.falling THEN
                  config.falling = ((config.falling) + 1) 
                  IF config.falling > 11 THEN
                     config.falling = 1 
                     IF config.stageclear THEN
                     ELSE
                        config.viruses = REMAININGVIRUS(0) 
                        IF config.viruses=0 THEN
                           GOTO stageisclear
                        ELSE
                           IF BLOCKFELL() THEN
                              IF ISFALSE(config.fanfare) THEN
                                 SNDPLAYSOUND(bycopy "sfx" & FORMAT$(3), %SND_RESOURCE OR %SND_ASYNC) 
                              END IF
                           ELSE
                              r = DESTROYED() 
                              IF r THEN
                                 config.viruses = REMAININGVIRUS(0) 
                                 IF config.viruses=0 THEN
                                 stageisclear:
                                    config.stageclear = 1 
                                    config.loseframe = 0 
                                    SELECT CASE config.music
                                       CASE 1
                                          PLAYBGM("SFX26", 999999, 0, 0) 
                                       CASE 2
                                          PLAYBGM("SFX25", 999999, 0, 0) 
                                       CASE 3
                                          PLAYBGM("SFX27", 999999, 0, 0) 
                                    END SELECT
                                 ELSE
                                    config.falling = 1 
                                 END IF
                              ELSE
                                 config.falling = 0 
                                 config.throwpill = 1 
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               ELSEIF config.throwpill THEN
               ELSE
                  IF config.rotated THEN
                     IF ISFALSE(GETASYNCKEYSTATE(%VK_Z)) AND ISFALSE(GETASYNCKEYSTATE(%VK_X)) THEN
                        config.rotated = 0 
                     END IF
                  ELSEIF GETASYNCKEYSTATE(%VK_Z) THEN
                     IF CANROTATE() THEN
                        ROTATEPILL(%DIR_LF) 
                     END IF
                  ELSEIF GETASYNCKEYSTATE(%VK_X) THEN
                     IF CANROTATE() THEN
                        ROTATEPILL(%DIR_RT) 
                     END IF
                  END IF
                  IF GETASYNCKEYSTATE(%VK_DOWN) THEN
                     config.moved = 0 
                     config.movestep = 0 
                     IF PILLCANGO(%DIR_DN) THEN
                        MOVEPILL(%DIR_DN) 
                     ELSE
                        GOTO pillfell
                     END IF
                  ELSEIF config.moved THEN
                     IF GETASYNCKEYSTATE(config.moved) THEN
                        config.movestep = ((config.movestep) + 1) 
                        IF config.movestep > 15 THEN
                           IF config.moved=%VK_RIGHT THEN
                              IF PILLCANGO(%DIR_RT) THEN
                                 config.moved = %VK_RIGHT 
                                 MOVEPILL(%DIR_RT) 
                              END IF
                           ELSEIF config.moved=%VK_LEFT THEN
                              IF PILLCANGO(%DIR_LF) THEN
                                 config.moved = %VK_LEFT 
                                 MOVEPILL(%DIR_LF) 
                              END IF
                           END IF
                        END IF
                     ELSE
                        config.moved = 0 
                        config.movestep = 0 
                     END IF
                  ELSE
                     IF GETASYNCKEYSTATE(%VK_RIGHT) THEN
                        IF PILLCANGO(%DIR_RT) THEN
                           config.moved = %VK_RIGHT 
                           MOVEPILL(%DIR_RT) 
                        END IF
                     ELSEIF GETASYNCKEYSTATE(%VK_LEFT) THEN
                        IF PILLCANGO(%DIR_LF) THEN
                           config.moved = %VK_LEFT 
                           MOVEPILL(%DIR_LF) 
                        END IF
                     END IF
                  END IF
                  config.curpill.step = ((config.curpill.step) + 1) 
                  IF (config.curpill.step MOD config.pilldelay)=0 THEN
                     IF PILLCANGO(%DIR_DN) THEN
                        MOVEPILL(%DIR_DN) 
                     ELSE
                     pillfell:
                        IF ISFALSE(config.fanfare) THEN
                           SNDPLAYSOUND(bycopy "sfx" & FORMAT$(3), %SND_RESOURCE OR %SND_ASYNC) 
                        END IF
                        LEAVEPILL() 
                        config.combo = 0 
                        r = DESTROYED() 
                        IF r THEN
                           config.falling = 1 
                        ELSE
                           config.throwpill = 1 
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         CASE %SCREEN_ENDING
            config.curframe = ((config.curframe) + 1) 
            IF config.curframe > 50 THEN
               IF GETASYNCKEYSTATE(%VK_RETURN) THEN
                  DO WHILE GETASYNCKEYSTATE(%VK_RETURN)
                     SLEEP 100 
                  IterLabel0571:
                  LOOP
                  CHANGEGAMESCREEN(%SCREEN_TITLE) 
               END IF
            END IF
      END SELECT
   END IF
   IF (config.curframe MOD 4)=0 THEN
      IF config.switchframe THEN
         config.switchframe = 0 
      ELSE
         config.switchframe = 1 
      END IF
   END IF
END FUNCTION

FUNCTION PBMAIN() AS LONG
   CALL PluriBASIC_Initialize()
   LOCAL hdlg AS DWORD
   INITGAME() 
   IF ISFALSE(TESTBGM("sfx21")) THEN
      MSGBOX "Music resources could not be extracted.", %MB_ICONERROR, "The game cannot start" 
      EXIT FUNCTION
   END IF
   DIALOG NEW PIXELS, 0, "DrMario - " & "PowerBASIC for Windows", , , 512, 448, %WS_OVERLAPPEDWINDOW TO hdlg 
   config.hmain = hdlg 
   DIALOG SET ICON  hdlg, "DLGICON" 
   DIALOG SHOW MODAL hdlg CALL DLGPROC() 
END FUNCTION

CALLBACK FUNCTION DLGPROC() AS LONG
   STATIC htimer AS DWORD
   STATIC htimem AS DWORD
   STATIC hframe AS LONG
   LOCAL rc AS RECT
   LOCAL hmemdc AS DWORD
   LOCAL hmemdb AS DWORD
   LOCAL hmembm AS DWORD
   LOCAL holdbm AS DWORD
   LOCAL holdbb AS DWORD
   LOCAL hfont AS DWORD
   LOCAL holdf AS DWORD
   LOCAL hbrush AS DWORD
   LOCAL hdc AS DWORD
   LOCAL offframe AS LONG
   LOCAL sprframe AS LONG
   LOCAL index AS LONG
   LOCAL xa AS LONG
   LOCAL ya AS LONG
   LOCAL x AS LONG
   LOCAL y AS LONG
   LOCAL ps AS PAINTSTRUCT
   LOCAL xn1 AS LONG
   LOCAL wn2 AS LONG
   LOCAL yn7 AS LONG
   LOCAL sn3 AS STRING
   LOCAL in4 AS LONG
   LOCAL tn5 AS LONG
   LOCAL nxn6 AS LONG
   LOCAL sn8 AS STRING
   LOCAL in9 AS LONG
   LOCAL tna AS LONG
   LOCAL nxnb AS LONG
   LOCAL xne AS LONG
   LOCAL wnf AS LONG
   LOCAL p1nc AS LONG
   LOCAL p2nd AS LONG
   LOCAL p1n10 AS LONG
   LOCAL p2n11 AS LONG
   LOCAL rcn12 AS RECT
   LOCAL sn13 AS STRING
   LOCAL in14 AS LONG
   LOCAL tn15 AS LONG
   LOCAL nxn16 AS LONG
   LOCAL sn17 AS STRING
   LOCAL in18 AS LONG
   LOCAL tn19 AS LONG
   LOCAL nxn1a AS LONG
   LOCAL sn1b AS STRING
   LOCAL in1c AS LONG
   LOCAL tn1d AS LONG
   LOCAL nxn1e AS LONG
   LOCAL sn1f AS STRING
   LOCAL in20 AS LONG
   LOCAL tn21 AS LONG
   LOCAL nxn22 AS LONG
   SELECT CASE CB.msg
      CASE %WM_INITDIALOG
         config.hiscore = 10000 
         config.speed = 1 
         config.level = 0 
         config.music = %MUSIC_FEVER 
         config.focused = -1 
         hvirbg = LOADBITMAP(EXE.Inst, "VBIG") 
         hvirsm = LOADBITMAP(EXE.Inst, "VSML") 
         hmario = LOADBITMAP(EXE.Inst, "DRMA") 
         hnumbr = LOADBITMAP(EXE.Inst, "NUMR") 
         hbackg = LOADBITMAP(EXE.Inst, "BACK") 
         htitle = LOADBITMAP(EXE.Inst, "TITL") 
         hselec = LOADBITMAP(EXE.Inst, "SELE") 
         hhighl = LOADBITMAP(EXE.Inst, "HLTH") 
         hendin = LOADBITMAP(EXE.Inst, "ENDS") 
         htimer = SETTIMER(CB.hndl, %TIMER_REFRESHSCREEN, 20, BYVAL %NULL) 
         htimem = SETTIMER(CB.hndl, %TIMER_MUSICCHECK, 500, BYVAL %NULL) 
         CHANGEGAMESCREEN(%SCREEN_TITLE) 
      CASE %WM_DESTROY
         IF htimer THEN
            KILLTIMER CB.hndl, htimer 
         END IF
         IF htimem THEN
            KILLTIMER CB.hndl, htimem 
         END IF
         SLEEP 100 
         DELETEOBJECT hvirbg 
         DELETEOBJECT hvirsm 
         DELETEOBJECT hmario 
         DELETEOBJECT hnumbr 
         DELETEOBJECT hbackg 
         DELETEOBJECT htitle 
         DELETEOBJECT hselec 
         DELETEOBJECT hhighl 
         DELETEOBJECT hendin 
      CASE %WM_TIMER
         SELECT CASE CB.wparam
            CASE %TIMER_MUSICCHECK
            CASE %TIMER_REFRESHSCREEN
               EXECGAMEMECHANICS() 
               GETCLIENTRECT(CB.hndl, rc) 
               hframe = 1 
               INVALIDATERECT CB.hndl, rc, 0 
            CASE %TIMER_MUSICLOOP
               MCISENDSTRING("seek mp3file to " & FORMAT$(config.musicloop), BYVAL 0, 0, 0) 
               MCISENDSTRING("play mp3file", BYVAL 0, 0, 0) 
               IF config.musictimes=0 THEN
                  IF config.mtimer THEN
                     KILLTIMER CB.hndl, config.mtimer 
                     config.musiclen = ((config.musiclen - config.musicloop) + config.adjust) 
                     config.mtimer = SETTIMER(CB.hndl, %TIMER_MUSICLOOP, config.musiclen, BYVAL %NULL) 
                  END IF
               END IF
               config.musictimes = ((config.musictimes) + 1) 
         END SELECT
      CASE %WM_KILLFOCUS
         config.focused = 0 
      CASE %WM_SETFOCUS
         config.focused = -1 
      CASE %WM_PAINT
         IF hframe THEN
            hframe = 0 
            hdc = BEGINPAINT(CB.hndl, ps) 
            hmemdc = CREATECOMPATIBLEDC(hdc) 
            hmemdb = CREATECOMPATIBLEDC(hdc) 
            hmembm = CREATECOMPATIBLEBITMAP(hdc, %GAME_W, %GAME_H) 
            holdbm = SELECTOBJECT(hmemdc, hmembm) 
            SELECT CASE config.screen
               CASE %SCREEN_TITLE
                  holdbb = SELECTOBJECT(hmemdb, htitle) 
                  BITBLT hmemdc, 0, 0, %GAME_W, %GAME_H, hmemdb, 0, 0, %SRCCOPY 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  IF (config.curframe MOD 10)=0 THEN
                     IF config.marioframe THEN
                        config.marioframe = 0 
                     ELSE
                        config.marioframe = 1 
                     END IF
                  END IF
                  holdbb = SELECTOBJECT(hmemdb, hmario) 
                  xn1 = (config.marioframe * 34) 
                  wn2 = 30 
                  BITBLT hmemdc, 50, 152, wn2, 40, hmemdb, xn1, 0, %SRCCOPY 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  holdbb = SELECTOBJECT(hmemdb, hvirbg) 
                  DO
                     IF config.gameover THEN
                        IF config.switchframe THEN
                           x = (24 * 1) 
                        ELSE
                           x = (24 * 3) 
                        END IF
                        y = (24 * (4 - 1)) 
                        BITBLT hmemdc, bigv(4).x, bigv(4).y + bigv(4).yo, 24, 24, hmemdb, x, y, %SRCCOPY 
                     ELSE
                        SELECT CASE bigv(4).state
                           CASE %VIRUS_NORMAL
                              IF (config.curframe MOD 8)=0 THEN
                                 bigv(4).frame = ((bigv(4).frame) + 1) 
                              END IF
                              offframe = bigv(4).frame MOD 4 
                              sprframe = CHOOSE&(offframe + 1, 1, 2, 3, 2) 
                              x = (24 * (sprframe - 1)) 
                              y = (24 * (4 - 1)) 
                           CASE %VIRUS_DEAD
                              bigv(4).steps = ((bigv(4).steps) + 1) 
                              IF bigv(4).steps < 20 THEN
                                 x = (24 * (7 - 1)) 
                                 y = (24 * (4 - 1)) 
                              ELSE
                                 EXIT DO
                              END IF
                           CASE %VIRUS_FLOOR
                              IF (config.curframe MOD 3)=0 THEN
                                 bigv(4).frame = ((bigv(4).frame) + 1) 
                              END IF
                              bigv(4).steps = ((bigv(4).steps) + 1) 
                              offframe = bigv(4).frame MOD 2 
                              sprframe = CHOOSE&((offframe + 1), 5, 6) 
                              x = (24 * (sprframe - 1)) 
                              y = (24 * (4 - 1)) 
                              IF bigv(4).steps < 7 THEN
                                 SELECT CASE bigv(4).steps
                                    CASE 1
                                       bigv(4).yo = -5 
                                    CASE 2
                                       bigv(4).yo = -9 
                                    CASE 3
                                       bigv(4).yo = -11 
                                    CASE 4
                                       bigv(4).yo = -9 
                                    CASE 5
                                       bigv(4).yo = -5 
                                    CASE 6
                                       bigv(4).yo = 0 
                                 END SELECT
                              END IF
                              IF bigv(4).steps > 80 THEN
                                 IF REMAININGVIRUS(bigv(4).color) THEN
                                    bigv(4).state = %VIRUS_NORMAL 
                                 ELSE
                                    config.fanfare = 1 
                                    SNDPLAYSOUND(bycopy "sfx" & FORMAT$(11), %SND_RESOURCE OR %SND_ASYNC) 
                                    bigv(4).state = %VIRUS_DEAD 
                                    bigv(4).steps = 0 
                                 END IF
                              END IF
                        END SELECT
                        BITBLT hmemdc, bigv(4).x, bigv(4).y + bigv(4).yo, 24, 24, hmemdb, x, y, %SRCCOPY 
                     END IF
                     EXIT DO
                  IterLabel0587:
                  LOOP
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
               CASE %SCREEN_OPTIONS
                  holdbb = SELECTOBJECT(hmemdb, hselec) 
                  BITBLT hmemdc, 0, 0, %GAME_W, %GAME_H, hmemdb, 0, 0, %SRCCOPY 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  holdbb = SELECTOBJECT(hmemdb, hhighl) 
                  SELECT CASE config.menurow
                     CASE 1
                        yn7 = 47 
                     CASE 2
                        yn7 = 104 
                     CASE 3
                        yn7 = 153 
                  END SELECT
                  BITBLT hmemdc, 39, yn7, 108, 23, hmemdb, 0, (config.menurow - 1) * 24, %SRCCOPY 
                  tn5 = config.level 
                  sn3 = FORMAT$(tn5, REPEAT$(2, "0")) 
                  x = 185 
                  yn7 = 72 
                  holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                  FOR in4 = 1 TO 2
                     nxn6 = VAL(MID$(sn3, in4, 1)) * 7 
                     BITBLT hmemdc, x, yn7, 7, 7, hmemdb, nxn6, ((2 - 1) * 7), %SRCCOPY 
                     x = ((x) + 8) 
                  NEXT 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  BITBLT hmemdc, 87 + (config.level * 4), 74, 7, 7, hmemdb, 111, 0, %SRCCOPY 
                  BITBLT hmemdc, 50 + (config.speed * 39), 128, 21, 7, hmemdb, 111, 8, %SRCCOPY 
                  BITBLT hmemdc, 4 + (config.music * 55), 179, 51, 18, hmemdb, 0 + ((config.music - 1) * 51), 71, %SRCCOPY 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
               CASE %SCREEN_ENDING
                  holdbb = SELECTOBJECT(hmemdb, hendin) 
                  BITBLT hmemdc, 0, 0, %GAME_W, %GAME_H, hmemdb, 0, 0, %SRCCOPY 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  tna = config.level 
                  sn8 = FORMAT$(tna, REPEAT$(2, "0")) 
                  x = 169 
                  y = 88 
                  holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                  FOR in9 = 1 TO 2
                     nxnb = VAL(MID$(sn8, in9, 1)) * 7 
                     BITBLT hmemdc, x, y, 7, 7, hmemdb, nxnb, ((3 - 1) * 7), %SRCCOPY 
                     x = ((x) + 8) 
                  NEXT 
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  holdbb = SELECTOBJECT(hmemdb, hhighl) 
                  SELECT CASE config.speed
                     CASE 1
                        BITBLT hmemdc, 169, 104, 25, 9, hmemdb, 139, 23, %SRCCOPY 
                     CASE 2
                        BITBLT hmemdc, 169, 104, 25, 9, hmemdb, 139, 32, %SRCCOPY 
                     CASE 3
                        BITBLT hmemdc, 169, 104, 25, 9, hmemdb, 139, 41, %SRCCOPY 
                  END SELECT
                  holdbb = SELECTOBJECT(hmemdb, holdbb) 
               CASE %SCREEN_GAME
                  IF config.paused THEN
                     GETCLIENTRECT(CB.hndl, rc) 
                     hbrush = CREATESOLIDBRUSH(RGB(0, 0, 0)) 
                     FILLRECT hmemdc, rc, hbrush 
                     DELETEOBJECT(hbrush) 
                     holdbb = SELECTOBJECT(hmemdb, hhighl) 
                     BITBLT hmemdc, 110, 100, 51, 10, hmemdb, 108, 54, %SRCCOPY 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  ELSE
                     holdbb = SELECTOBJECT(hmemdb, hbackg) 
                     BITBLT hmemdc, 0, 0, %GAME_W, %GAME_H, hmemdb, 0, 0, %SRCCOPY 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     holdbb = SELECTOBJECT(hmemdb, hvirbg) 
                     FOR index = 1 TO 3
                        DO
                           IF config.gameover THEN
                              IF config.switchframe THEN
                                 x = (24 * 1) 
                              ELSE
                                 x = (24 * 3) 
                              END IF
                              y = (24 * (index - 1)) 
                              BITBLT hmemdc, bigv(index).x, bigv(index).y + bigv(index).yo, 24, 24, hmemdb, x, y, %SRCCOPY 
                           ELSE
                              SELECT CASE bigv(index).state
                                 CASE %VIRUS_NORMAL
                                    IF (config.curframe MOD 8)=0 THEN
                                       bigv(index).frame = ((bigv(index).frame) + 1) 
                                    END IF
                                    offframe = bigv(index).frame MOD 4 
                                    sprframe = CHOOSE&(offframe + 1, 1, 2, 3, 2) 
                                    x = (24 * (sprframe - 1)) 
                                    y = (24 * (index - 1)) 
                                 CASE %VIRUS_DEAD
                                    bigv(index).steps = ((bigv(index).steps) + 1) 
                                    IF bigv(index).steps < 20 THEN
                                       x = (24 * (7 - 1)) 
                                       y = (24 * (index - 1)) 
                                    ELSE
                                       EXIT DO
                                    END IF
                                 CASE %VIRUS_FLOOR
                                    IF (config.curframe MOD 3)=0 THEN
                                       bigv(index).frame = ((bigv(index).frame) + 1) 
                                    END IF
                                    bigv(index).steps = ((bigv(index).steps) + 1) 
                                    offframe = bigv(index).frame MOD 2 
                                    sprframe = CHOOSE&((offframe + 1), 5, 6) 
                                    x = (24 * (sprframe - 1)) 
                                    y = (24 * (index - 1)) 
                                    IF bigv(index).steps < 7 THEN
                                       SELECT CASE bigv(index).steps
                                          CASE 1
                                             bigv(index).yo = -5 
                                          CASE 2
                                             bigv(index).yo = -9 
                                          CASE 3
                                             bigv(index).yo = -11 
                                          CASE 4
                                             bigv(index).yo = -9 
                                          CASE 5
                                             bigv(index).yo = -5 
                                          CASE 6
                                             bigv(index).yo = 0 
                                       END SELECT
                                    END IF
                                    IF bigv(index).steps > 80 THEN
                                       IF REMAININGVIRUS(bigv(index).color) THEN
                                          bigv(index).state = %VIRUS_NORMAL 
                                       ELSE
                                          config.fanfare = 1 
                                          SNDPLAYSOUND(bycopy "sfx" & FORMAT$(11), %SND_RESOURCE OR %SND_ASYNC) 
                                          bigv(index).state = %VIRUS_DEAD 
                                          bigv(index).steps = 0 
                                       END IF
                                    END IF
                              END SELECT
                              BITBLT hmemdc, bigv(index).x, bigv(index).y + bigv(index).yo, 24, 24, hmemdb, x, y, %SRCCOPY 
                           END IF
                           EXIT DO
                        IterLabel0604:
                        LOOP
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     holdbb = SELECTOBJECT(hmemdb, hvirsm) 
                     FOR xa = 0 TO 9
                        FOR ya = 0 TO 17
                           SELECT CASE area(xa,ya).block
                              CASE %AREA_VIRUS
                                 x = (7 * config.switchframe) 
                                 y = (7 * (area(xa,ya).color - 1)) 
                                 BITBLT hmemdc, %BOTTLE_X + ((xa * 8) - 8), %BOTTLE_Y + ((ya * 8) - 8), 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %AREA_HALFPILL
                                 x = (7 * 8) 
                                 y = (7 * (area(xa,ya).color - 1)) 
                                 BITBLT hmemdc, %BOTTLE_X + ((xa * 8) - 8), %BOTTLE_Y + ((ya * 8) - 8), 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %AREA_PILL
                                 x = (7 * (area(xa,ya).dir + 1)) 
                                 y = (7 * (area(xa,ya).color - 1)) 
                                 BITBLT hmemdc, %BOTTLE_X + ((xa * 8) - 8), %BOTTLE_Y + ((ya * 8) - 8), 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %AREA_BLOCK
                                 IF area(xa,ya).step THEN
                                    IF ISFALSE(config.stageclear) THEN
                                       area(xa,ya).step = ((area(xa,ya).step) + 1) 
                                    END IF
                                    x = (7 * 6) 
                                    y = (7 * (area(xa,ya).color - 1)) 
                                    IF area(xa,ya).step > 10 THEN
                                       area(xa,ya).block = 0 
                                       area(xa,ya).color = 0 
                                       area(xa,ya).step = 0 
                                    END IF
                                 ELSE
                                    x = (7 * 8) 
                                    y = (7 * (area(xa,ya).color - 1)) 
                                 END IF
                                 BITBLT hmemdc, %BOTTLE_X + ((xa * 8) - 8), %BOTTLE_Y + ((ya * 8) - 8), 7, 7, hmemdb, x, y, %SRCCOPY 
                           END SELECT
                        NEXT 
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     IF config.stageclear THEN
                        holdbb = SELECTOBJECT(hmemdb, hmario) 
                        IF config.switchframe THEN
                           xne = (34 * 5) 
                        ELSE
                           xne = (34 * 6) 
                        END IF
                        BITBLT hmemdc, 188, 68, 30, 40, hmemdb, xne, 0, %SRCCOPY 
                        holdbb = SELECTOBJECT(hmemdb, holdbb) 
                        IF config.loseframe > 60 THEN
                           holdbb = SELECTOBJECT(hmemdb, hvirbg) 
                           rc.nleft = 96 
                           rc.nright = (96 + 64) 
                           rc.ntop = 136 
                           rc.nbottom = (136 + 64) 
                           hbrush = CREATESOLIDBRUSH(RGB(0, 0, 0)) 
                           FILLRECT hmemdc, rc, hbrush 
                           DELETEOBJECT(hbrush) 
                           BITBLT hmemdc, 97, 90, 62, 71, hmemdb, 250, 0, %SRCCOPY 
                           holdbb = SELECTOBJECT(hmemdb, holdbb) 
                        END IF
                     ELSEIF config.gameover THEN
                        holdbb = SELECTOBJECT(hmemdb, hmario) 
                        BITBLT hmemdc, 188, 68, 40, 40, hmemdb, 34 * 7, 0, %SRCCOPY 
                        holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     ELSE
                        holdbb = SELECTOBJECT(hmemdb, hmario) 
                        SELECT CASE config.marioframe
                           CASE 3, 4, 5
                              xne = ((config.marioframe - 1) * 34) 
                              wnf = 30 
                        END SELECT
                        BITBLT hmemdc, 188, 68, wnf, 40, hmemdb, xne, 0, %SRCCOPY 
                        holdbb = SELECTOBJECT(hmemdb, holdbb) 
                        holdbb = SELECTOBJECT(hmemdb, hvirsm) 
                        SELECT CASE config.nextpill.dir
                           CASE %DIR_UP
                              xne = (7 * (%DIR_DN + 1)) 
                              y = (7 * (config.nextpill.lfcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                              xne = (7 * (%DIR_UP + 1)) 
                              y = (7 * (config.nextpill.rtcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y - 8, 7, 7, hmemdb, xne, y, %SRCCOPY 
                           CASE %DIR_DN
                              xne = (7 * (%DIR_DN + 1)) 
                              y = (7 * (config.nextpill.rtcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                              xne = (7 * (%DIR_UP + 1)) 
                              y = (7 * (config.nextpill.lfcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y - 8, 7, 7, hmemdb, xne, y, %SRCCOPY 
                           CASE %DIR_RT
                              xne = (7 * (%DIR_LF + 1)) 
                              y = (7 * (config.nextpill.lfcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                              xne = ((7 * (%DIR_RT + 1))) 
                              y = (7 * (config.nextpill.rtcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x + 8, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                           CASE %DIR_LF
                              xne = (7 * (%DIR_LF + 1)) 
                              y = (7 * (config.nextpill.rtcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                              xne = ((7 * (%DIR_RT + 1))) 
                              y = (7 * (config.nextpill.lfcolor - 1)) 
                              BITBLT hmemdc, config.nextpill.x + 8, config.nextpill.y, 7, 7, hmemdb, xne, y, %SRCCOPY 
                        END SELECT
                     END IF
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     IF config.curpill.active THEN
                        IF ISFALSE(config.falling) AND ISFALSE(config.throwpill) THEN
                           holdbb = SELECTOBJECT(hmemdb, hvirsm) 
                           SELECT CASE config.curpill.dir
                              CASE %DIR_UP
                                 x = (7 * (%DIR_DN + 1)) 
                                 y = (7 * (config.curpill.lfcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                                 x = (7 * (%DIR_UP + 1)) 
                                 y = (7 * (config.curpill.rtcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y - 8, 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %DIR_DN
                                 x = (7 * (%DIR_DN + 1)) 
                                 y = (7 * (config.curpill.rtcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                                 x = (7 * (%DIR_UP + 1)) 
                                 y = (7 * (config.curpill.lfcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y - 8, 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %DIR_RT
                                 x = (7 * (%DIR_LF + 1)) 
                                 y = (7 * (config.curpill.lfcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                                 x = ((7 * (%DIR_RT + 1))) 
                                 y = (7 * (config.curpill.rtcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x + 8, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                              CASE %DIR_LF
                                 x = (7 * (%DIR_LF + 1)) 
                                 y = (7 * (config.curpill.rtcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                                 x = ((7 * (%DIR_RT + 1))) 
                                 y = (7 * (config.curpill.lfcolor - 1)) 
                                 BITBLT hmemdc, config.curpill.x + 8, config.curpill.y, 7, 7, hmemdb, x, y, %SRCCOPY 
                           END SELECT
                           holdbb = SELECTOBJECT(hmemdb, holdbb) 
                        END IF
                     END IF
                     IF config.gameover THEN
                        IF config.loseframe > 60 THEN
                           holdbb = SELECTOBJECT(hmemdb, hvirbg) 
                           IF config.switchframe THEN
                              x = 168 
                              y = 0 
                           ELSE
                              x = 208 
                              y = 0 
                           END IF
                           rcn12.nleft = 96 
                           rcn12.nright = (96 + 64) 
                           rcn12.ntop = 136 
                           rcn12.nbottom = (136 + 64) 
                           hbrush = CREATESOLIDBRUSH(RGB(0, 0, 0)) 
                           FILLRECT hmemdc, rcn12, hbrush 
                           DELETEOBJECT(hbrush) 
                           BITBLT hmemdc, 112, 144, 40, 56, hmemdb, x, y, %SRCCOPY 
                           holdbb = SELECTOBJECT(hmemdb, holdbb) 
                        END IF
                     END IF
                     IF config.score > config.hiscore THEN
                        config.hiscore = config.score 
                     END IF
                     tn15 = config.hiscore 
                     sn13 = FORMAT$(tn15, REPEAT$(7, "0")) 
                     x = 16 
                     y = 56 
                     holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                     FOR in14 = 1 TO 7
                        nxn16 = VAL(MID$(sn13, in14, 1)) * 7 
                        BITBLT hmemdc, x, y, 7, 7, hmemdb, nxn16, ((1 - 1) * 7), %SRCCOPY 
                        x = ((x) + 8) 
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     tn19 = config.score 
                     sn17 = FORMAT$(tn19, REPEAT$(7, "0")) 
                     x = 16 
                     y = 80 
                     holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                     FOR in18 = 1 TO 7
                        nxn1a = VAL(MID$(sn17, in18, 1)) * 7 
                        BITBLT hmemdc, x, y, 7, 7, hmemdb, nxn1a, ((1 - 1) * 7), %SRCCOPY 
                        x = ((x) + 8) 
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     tn1d = config.lstart 
                     sn1b = FORMAT$(tn1d, REPEAT$(2, "0")) 
                     x = 207 
                     y = 145 
                     holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                     FOR in1c = 1 TO 2
                        nxn1e = VAL(MID$(sn1b, in1c, 1)) * 7 
                        BITBLT hmemdc, x, y, 7, 7, hmemdb, nxn1e, ((1 - 1) * 7), %SRCCOPY 
                        x = ((x) + 8) 
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     holdbb = SELECTOBJECT(hmemdb, hhighl) 
                     SELECT CASE config.speed
                        CASE 1
                           BITBLT hmemdc, 207, 169, 25, 9, hmemdb, 109, 23, %SRCCOPY 
                        CASE 2
                           BITBLT hmemdc, 207, 169, 25, 9, hmemdb, 109, 32, %SRCCOPY 
                        CASE 3
                           BITBLT hmemdc, 207, 169, 25, 9, hmemdb, 109, 41, %SRCCOPY 
                     END SELECT
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                     tn21 = config.viruses 
                     sn1f = FORMAT$(tn21, REPEAT$(2, "0")) 
                     x = 207 
                     y = 194 
                     holdbb = SELECTOBJECT(hmemdb, hnumbr) 
                     FOR in20 = 1 TO 2
                        nxn22 = VAL(MID$(sn1f, in20, 1)) * 7 
                        BITBLT hmemdc, x, y, 7, 7, hmemdb, nxn22, ((1 - 1) * 7), %SRCCOPY 
                        x = ((x) + 8) 
                     NEXT 
                     holdbb = SELECTOBJECT(hmemdb, holdbb) 
                  END IF
            END SELECT
            GETCLIENTRECT(CB.hndl, rc) 
            STRETCHBLT hdc, 0, 0, rc.nright - rc.nleft, rc.nbottom - rc.ntop, hmemdc, 0, 0, %GAME_W, %GAME_H, %SRCCOPY 
            DELETEOBJECT(SELECTOBJECT(hmemdc, holdbm)) 
            DELETEDC(hmemdc) 
            DELETEDC(hmemdb) 
            ENDPAINT(CB.hndl, ps) 
         END IF
   END SELECT
END FUNCTION

