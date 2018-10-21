'  RADTABS Code 02/12/08
#RESOURCE "RADTABS.pbr"                                   ' RADTABS resource file
 TYPE RECT                                                ' need by DPlotJr
  nLeft AS LONG                                           ' from Win32API.inc file
  nTop AS LONG                                            ' coordinates of upper-left and
  nRight AS LONG                                          ' lower-right corners
  nBottom AS LONG                                         ' of a rectangle
 END TYPE
'
' HydeSoft Computing, Inc. DPlotJr package downloaded by K.F. Eckerman
#INCLUDE "PB_DPlot.inc"                                   ' DPlotJr's include file
'
' Perfect Sync, Inc's tools license is owed by K.F. Eckerman
' license enables distribution of DLLs but not the include files below
' © Copyright 2002 Perfect Sync, Inc.
#INCLUDE "c:\ConTools\CT_Pro.INC"                         ' Console and Graphic tools
#INCLUDE "c:\Gfxtools\GfxT_Pro.INC"                       ' include files.
'
' integer equates used throughout the code
 %indx = 1     : %irad = 2     : %ibet = 3     : %iaug = 4     : %ineu = 5
 %iout = 6     : %mspec = 30   : %xblack = 0   : %xblue = 1    : %xyellow = 14
 %xwhite = 7   : %xBright = 8  : %F1Key = 1059 : %F2Key = 1060 : %F3Key = 1061
 %F4Key = 1062 : %F5Key = 1063 : %F6Key = 1064 : %F7Key = 1065 : %chand = 32649
 %IDYES = 6&   : %IDNO = 7&    : %TRUE  = -1   : %false = NOT %true
 %NULL = 0                                                 ' Some equates from
 %SW_MAXIMIZE = 3                                          ' the Win32API file
 %MAX_PATH = 260                                           ' as need by a few
 %SW_SHOWNORMAL = 1                                        ' functions
 %RT_BITMAP = 2                                            '
' character equates used in package
 $sym = "H-HeLiBeB-C-N-O-F-NeNaMgAlSiP-S-ClArK-CaScTiV-CrMnFeCoNiCuZnGaGeAsSe" + _
        "BrKrRbSrY-ZrNbMoTcRuRhPdAgCdInSnSbTeI-XeCsBaLaCePrNdPmSmEuGdTbDyHoEr" + _
        "TmYbLuHfTaW-ReOsIrPtAuHgTlPbBiPoAtRnFrRaAcThPaU-NpPuAmCmBkCfEsFmMdNoLr"
'
 $Code = "RADTABS"         : $Version = "Ver. 2.2"
 $DecFile = "MIRD-07.NDX"  : $DEFAULT_MSGBOX_CAPTION =  "MIRD-07 Data Set"
 $data = "DATA\"           : $OutDir = "OutPut\"       : $Report = "Report\"
 $Mess = "View/Extract MIRD Nuclear Decay Data                K.F. Eckerman & A. Endo"
 $fmt1 = "##.###^^^^"
 $fline1 = "<F1>=Export <F2>=Chain  <F3>=Plots <F4>=Tables <F5>=Unknown <F6>=Help <F7>=About"
 $Prompt = " Press a key (or left click mouse) to continue..."
 $MIRD07NDX = "B33998E6"  : $MIRD07BET = "FC82462B" : $MIRD07ACK = "D40A0EA6"
 $MIRD07NSF = "589614D0"  : $MIRD07RAD = "29C0DD79"
 $Title = "RADTABS - A Summary of Radiation Emissions"
'
' Declarations for function routines of RADTABS
 DECLARE FUNCTION izmass(a$) AS LONG
 DECLARE FUNCTION Ibinry(a$) AS LONG
 DECLARE FUNCTION ExpFun(a##) AS EXT
 DECLARE FUNCTION ElName(a&) AS STRING
 DECLARE FUNCTION izwho(a&, b&) AS LONG
 DECLARE FUNCTION Expf1(a##, b#) AS EXT
 DECLARE FUNCTION MSGBOX(a$, b$) AS LONG
 DECLARE FUNCTION InKeyCode(sKey$) AS LONG
 DECLARE FUNCTION CenterMess(a$, i&) AS STRING
 DECLARE FUNCTION GetFileCount (FileSpec$) AS LONG
 DECLARE FUNCTION ResABC(a$, b$, c$, d$, e$) AS STRING
 DECLARE FUNCTION IcutOff(a#(), b#(), c#(), n&) AS LONG
 DECLARE FUNCTION Timest(a AS STRING*8, b AS STRING*2) AS DOUBLE
 DECLARE FUNCTION RefreshWindow(BYVAL lPlaceHolder AS LONG) AS LONG
 DECLARE FUNCTION GetResourceBitMapSize (szBM AS ASCIIZ, n&, m&) AS LONG
 DECLARE FUNCTION CRC32(BYVAL dwOffset AS DWORD, dwLen AS DWORD) AS DWORD
 DECLARE FUNCTION invect(clist() AS STRING *7, citem AS STRING * 7, n&) AS LONG
 DECLARE FUNCTION LockResource LIB "KERNEL32.DLL" ALIAS "LockResource" (BYVAL hResData AS DWORD) AS DWORD
 DECLARE FUNCTION GetModuleHandle LIB "KERNEL32.DLL" ALIAS "GetModuleHandleA" (lpModuleName AS ASCIIZ) _
                  AS DWORD
 DECLARE FUNCTION ShowWindow LIB "USER32.DLL" ALIAS "ShowWindow" (BYVAL hWnd AS DWORD, BYVAL nCmdShow AS LONG)_
                  AS LONG
 DECLARE FUNCTION LoadResource LIB "KERNEL32.DLL" ALIAS "LoadResource" (BYVAL hInstance AS DWORD, _
                  BYVAL hResInfo AS DWORD) AS LONG
 DECLARE FUNCTION FindResource LIB "KERNEL32.DLL" ALIAS "FindResourceA" (BYVAL hInstance AS DWORD, _
                  lpName AS ASCIIZ, lpType AS ASCIIZ) AS LONG
 DECLARE FUNCTION ShellExecute LIB "SHELL32.DLL" ALIAS "ShellExecuteA" (BYVAL hwnd AS DWORD, _
                  lpOperation AS ASCIIZ, lpFile AS ASCIIZ, lpParameters AS ASCIIZ, lpDirectory AS ASCIIZ, _
                  BYVAL nShowCmd AS LONG) AS DWORD
'
 TYPE IndexType                        ' structure of MIRD-07.NDX file
   Nuke AS STRING * 7                  ' nuclide name; e.g., Cl-38, Tc-99m
   t AS STRING * 8                     ' physical half-life of nuclide
   tu AS STRING * 2                    ' units of T1/2
   Mode AS STRING * 8                  ' decay mode
   mdec AS STRING * 7                  ' location of nuclide in MIRD-07.RAD
   mbet AS STRING * 7                  ' locatation of nuclide in MIRD-07.BET
   mack AS STRING * 7                  ' location of nuclide in MIRD-07.ACK
   mneu AS STRING * 6                  ' location of nuclide in MIRD-07.NSF
   dmy1 AS STRING * 1                  ' a space
   dau1 AS STRING * 8                  ' daughter 1
   idau1 AS STRING * 5                 ' location of daughter 1 in MIRD-07.NDX
   bf1 AS STRING * 11                  ' branching fraction to daughter 1
   dmy2 AS STRING * 1                  ' a space
   dau2 AS STRING * 8                  ' daughter 2
   idau2 AS STRING * 5                 ' location of daughter 2 in MIRD-07.NDX
   bf2 AS STRING * 11                  ' branching fraction to daughter 2
   dmy3 AS STRING * 1                  ' a space
   dau3 AS STRING * 8                  ' daughter 3
   idau3 AS STRING * 5                 ' location of daughter 3 in MIRD-07.NDX
   bf3 AS STRING * 11                  ' branching fraction to daughter 3
   ea AS STRING * 7                    ' alpha MeV/nt
   eb AS STRING * 8                    ' electron MeV/nt
   eg AS STRING * 8                    ' photon MeV/nt
   dmy4 AS STRING * 1                  ' a blank space
   np10 AS STRING * 3                  ' # photon < 10 keV
   npg10 AS STRING * 4                 ' # photon >= 10 keV
   nbet AS STRING * 4                  ' # beta particles
   nel AS STRING * 5                   ' # electrons
   nalpha AS STRING * 4                ' # alpha particles
   dmy5 AS STRING * 1                  ' a space
   Mass AS STRING * 10                 ' atomic mass
   Kair AS STRING * 10                 ' air kerma rate constant
   Kpts AS STRING * 9                  ' point source air kerma
   CrLf AS STRING * 2                  '
 END TYPE

 TYPE HeadType                         ' structure of first recond in the
   i1 AS STRING * 4                    ' NDX file. I1 is first data record,
   i2 AS STRING * 4                    ' I2 is last data record in the
   dmy AS STRING * 193                 ' file.
   CrLf AS STRING * 2
 END TYPE

 TYPE DecayType                        ' structure of data records in RAD file
   icode AS STRING * 2                 ' radiation type,
   Freq AS STRING * 12                 ' frequency #/nt,
   E AS STRING * 12                    ' energy (MeV), and
   jcode AS STRING * 3                 ' a mnumonic
   CrLf AS STRING * 2
 END TYPE

 TYPE DecayHtype                       ' structure of nuclide record in RAD file
   Nuke AS STRING * 7                  ' nuclide
   dmy1 AS STRING * 3                  ' spaces
   t AS STRING * 8                     ' physical half-life of nuclide
   tu AS STRING * 2                    ' units of T1/2
   dmy2 AS STRING * 4                  ' spaces
   ndec AS STRING * 5                  ' number of radiation records
   CrLf AS STRING * 2
 END TYPE

 TYPE BetaType                         ' structure of data records in BET file
    Eelc AS STRING * 8                 ' Energy (MeV)
    Freq AS STRING * 9                 ' frequency #/MeV/nt
    CrLf AS STRING * 2
 END TYPE

 TYPE BetaHead                         ' structure of nuclide record in BET file
    Nuke AS STRING * 7                 ' nuclide
    nbet AS STRING * 10                ' number of data records
    CrLf AS STRING * 2
 END TYPE

 TYPE AugerHead                        ' structure of nuclide record in ACK file
    Nuke AS STRING * 7                 ' nuclide
    dmy1 AS STRING * 20                ' space
    naug AS STRING * 5                 ' number of data records
    CrLf AS STRING * 2
 END TYPE

 TYPE AugerData                        ' structure of data records in ACK file
    Freq AS STRING * 11                ' #/nt
    E AS STRING * 12                   ' Energy (eV)
    tran AS STRING *9                  ' atomic transition
    CrLf AS STRING * 2
 END TYPE

 TYPE NeutHead                         ' structure of nuclide record in NSF file
   Nuke AS STRING * 8                  ' nuclide
   sfnt AS STRING * 9                  ' sp fission branching fraction
   dmy AS STRING * 9                   ' space
   npts AS STRING * 3                  ' number of data records
   CrLf AS STRING * 2
 END TYPE

 TYPE NeutRecord                       ' structure of data records in NSF file
   E1 AS STRING * 8                    ' low bound of energy bin (MeV)
   E2 AS STRING * 9                    ' upper bound of energy bin (MeV)
   yield AS STRING * 12                ' #/nt
   CrLf AS STRING * 2
 END TYPE

 TYPE tagBitMap                        ' need by GetResourceBitMapSize function
   bmType AS LONG
   bmWidth AS LONG
   bmHeight AS LONG
 END TYPE

 DEFLNG I-N
 DEFDBL A-H, O-Z

'----------------------------------------------------------------------------------------
  FUNCTION PBMAIN() AS LONG
'----------------------------------------------------------------------------------------
   CONSOLE NAME $Title
   CONSOLE SET SCREEN 34, 80
   hWnd& = CONSHNDL
   ShowWindow hWnd&, %SW_Maximize
   CONSOLE GET SIZE TO w&, h&
   DESKTOP GET CLIENT TO ncWidth&, ncHeight&
   DESKTOP GET LOC TO x&, y&
   CONSOLE SET LOC x& + (ncWidth& - w&) \ 2, y& + (ncHeight& - h&) \ 2
   ConsoleToolsAuthorize %MY_GFXT_AUTHCODE                ' Authorize Console Tools
   COLOR %xWhite + %xBright, %xBlue
   CLS
   lresult = InitConsoleTools(hWnd&, 0, 5, 3, 0, 0)
'
'  User-Define Type variables
   DIM Dp AS GLOBAL Dplot
   DIM BetHed AS GLOBAL BetaHead
   DIM BetSpec AS GLOBAL BetaType
   DIM DecHed AS GLOBAL DecayHtype
   DIM DecDat AS GLOBAL DecayType
   DIM IndHed AS GLOBAL HeadType
   DIM IndDat AS GLOBAL IndexType
   DIM AugHed AS GLOBAL AugerHead
   DIM AugDat AS GLOBAL AugerData
   DIM NeuHed AS GLOBAL NeutHead
   DIM NeuDat AS  GLOBAL NeutRecord
'  Globabl arrays
   DIM ealpha(1 TO %mspec) AS GLOBAL DOUBLE
   DIM ebeta(1 TO %mspec)  AS GLOBAL DOUBLE
   DIM egamm(1 TO %mspec)  AS GLOBAL DOUBLE
   DIM zlmr(1 TO %mspec)   AS GLOBAL EXT
   DIM fhold(1 TO %mspec)  AS GLOBAL DOUBLE
   DIM branch(1 TO %mspec, 1 TO %mspec) AS GLOBAL DOUBLE
   DIM nucnam(1 TO %mspec) AS GLOBAL STRING * 7
   DIM thalf(1 TO %mspec)  AS GLOBAL STRING * 8
   DIM named(1 TO %mspec)  AS GLOBAL STRING * 7
   DIM iu(1 TO %mspec)     AS GLOBAL STRING * 2
   DIM maxi(1 TO %mspec)   AS GLOBAL LONG
   DIM iptb(1 TO %mspec)  AS GLOBAL LONG
   DIM iparb(1 TO %mspec) AS GLOBAL LONG
   DIM mpath(1 TO %mspec, 1 TO %mspec) AS GLOBAL LONG
'  global variables
   DIM nspec AS GLOBAL LONG
   DIM ipt   AS GLOBAL LONG
   DIM ibrch AS GLOBAL LONG
   DIM ipar  AS GLOBAL LONG
   DIM ieob  AS GLOBAL LONG
   DIM szBM AS ASCIIZ * 6
'
   GraphicsToolsAuthorize %MY_GFXT_AUTHCODE               ' Authorize Graphics Tools.
'
   n = GetFileCount($data + "*.ndx")                      ' check for data files
   IF n = 0 THEN
      BEEP
      PRINT " No decay data files exist in the folder " + $data + "."
      LOCATE 14, 22
      PRINT " Press any key to exit.";
      WAITKEY$
      EXIT FUNCTION
   END IF
   szBM = "cover"                                         ' bitmap of pub cover
   IF ISTRUE GetResourceBitmapSize (szBm, nWidth, nHEight) THEN ' get its size
'     print "BitMap "; szBM; " is "; nWidth; " pixels wide x "; nHeight; " pixels High"
   ELSE
      EXIT FUNCTION
   END IF
'  open all data files
   FileRoot$ = $data + LEFT$($DecFile, INSTR($DecFile, "."))
   OPEN FileRoot$ + "ndx" FOR RANDOM AS %indx LEN = LEN(IndDat)
   OPEN FileRoot$ + "rad" FOR RANDOM AS %irad LEN = LEN(DecDat)
   OPEN FileRoot$ + "bet" FOR RANDOM AS %ibet LEN = LEN(BetSpec)
   OPEN FileRoot$ + "ack" FOR RANDOM AS %iaug LEN = LEN(AugDat)
   OPEN FileRoot$ + "nsf" FOR RANDOM AS %ineu LEN = LEN(NeuDat)
'
   CALL GetNukeLst(NukeCover$)                           ' Get string of elements in collection
'
   MOUSE 3, DOUBLE, DOWN                                 ' Turn on trapping of mouse clicks
   MOUSE ON
'
   ConsoleGfx 2, 2, 79, 28                               ' Fill console w graphics window.
   iwidth = GfxConvert(%X_PIXELS_TO_UNITS, GfxMetrics(%GFX_DRAWING_WIDTH) )
   jwidth = GfxConvert(%Y_PIXELS_TO_UNITS, GfxMetrics(%GFX_DRAWING_HEIGHT))
   lResult& = StretchImage($data + "pchart.emf", iwidth, jwidth) ' show periodic table
'
   THREAD CREATE RefreshWindow(0) TO lResult             ' thread to refresh graphic window
   THREAD CLOSE lResult TO lResult                       ' don't need to monitor the thread.
'
   COLOR %xYellow, %xBlue                                ' the print statement
   LOCATE 31, 2                                          '
   PRINT "Click on an element to list its radioisotopes."
   PRINT " Press <Esc> to exit RADTABS."
   GfxWindow %GFX_SHOW                                   ' show the graphics window
   DESKTOP GET CLIENT TO ncWidth&, ncHeight&             ' now show the pub cover
   ix& = (ncWidth& - nWidth&)\2                          ' center graphic on desktop
   iy& = (ncHeight& - nHeight&)\2                        '
   GRAPHIC WINDOW "", ix&, iy&, nWidth&, nHeight& TO hGW1&  ' splash the monograph
   GRAPHIC ATTACH hGW1&, 0, REDRAW                          ' cover on the screen
   GRAPHIC RENDER "cover", (0, 0) - (nWidth&-1, nHeight&-1) ' within the graphic window
   GRAPHIC REDRAW
   GRAPHIC SET FOCUS
   SLEEP 1000                                             ' wait a bit and end splash
   GRAPHIC WINDOW END                                     ' close graphic bitmap and put
   CONSOLE SET FOCUS                                      ' set focus on the console
'
   DO                                                     ' loop over user's input
      CURSOR OFF
      GfxWindow %GFX_SHOW                                 ' show the graphics window
      REDIM Listx$(1 TO 30)                               ' zero list of isotopes
      Nukex$ = ""                                         ' blank nuke
      LOCATE 34, 1                                        ' note this is the last
      COLOR %xBlack, %xWhite                              ' line of the screen
      PRINT $fline1;                                      ' hence the ; at the end of
      COLOR %xYellow, %xBlue                              ' the print statement
      LOCATE 31, 2                                        '
      PRINT "Click on an element to list its radioisotopes."
      PRINT " Press <Esc> to exit RADTABS.";
'
      DO                                                  ' loop over periodic table graphic
         iz = 0                                           ' set atomic number to zero
         ix = MouseOverX                                  '
         IF ix <> %GFX_NONE THEN                          ' mouse is over graphic
            iy = MouseOverY                               ' get row/cell of table
            iz = izwho(ix, iy)                            ' and atomic number
            LOCATE 31, 60                                 ' udate screen
            IF iz > 0 THEN                                ' mouse over an element
               SLEEP 1                                    ' limit CPU utilization
               Chx$ = MID$($sym, (iz -1) * 2 + 1, 2)      ' get symbol based on Z
               PRINT TRIM$(ElName(iz)) + ": ";            ' show element name
               IF INSTR(NukeCover$, Chx$) > 0 THEN        ' we have data for this element
                  IF INSTR(Chx$, "-") = 0 THEN            ' display element name
                     PRINT Chx$ + STRING$(21 - LEN(ElName(iz)) - 4, " ")
                  ELSE
                     PRINT LEFT$(Chx$,1) + STRING$(22 - LEN(ElName(iz)) - 4, " ")
                  END IF
               ELSE                                       ' don't have data for element
                  PRINT "No Data" + STRING$(16 - LEN(ElName(iz)) - 4, " ")
               END IF
            ELSE                                          ' not over element so
               SLEEP 1                                    ' limit CPU utilization
               PRINT STRING$(21, " ")                     ' blank out the text
            END IF
         ELSE                                             ' mouse not over table entry
            SLEEP 1
         END IF
'
         skey$ = INKEY$                                   ' keyboard/mouse event
         IF skey$ = $ESC THEN                             ' escape key
            CLOSE                                         ' close and
            EXIT FUNCTION                                 ' exit program
         ELSEIF LEN(skey$) > 0 AND LEN(skey$) < 3 THEN    ' if function key pressed
            Ikey = InKeyCode(skey$)                       ' get key code
            IF Ikey = %F1key THEN                         ' not active/need nuclide
               INPUT FLUSH
               iresponse = MSGBOX("A radionuclide needs to be selected;", "")
               IF iresponse = %IDNO THEN
                  CLOSE
                  EXIT FUNCTION
               END IF
            ELSEIF Ikey = %F2key THEN                    ' not active/need nuclide
               INPUT FLUSH
               iresponse = MSGBOX("A radionuclide needs to be selected;", "")
               IF iresponse = %IDNO THEN
                  CLOSE
                  EXIT FUNCTION
               END IF
            ELSEIF Ikey = %F3key THEN                    ' not active/need nuclide
               INPUT FLUSH
               iresponse = MSGBOX ("A radionuclide needs to be selected;", "")
               IF iresponse = %IDNO THEN
                  CLOSE
                  EXIT FUNCTION
               END IF
            ELSEIF Ikey = %F4key THEN
               GfxWindow %GFX_FREEZE                      ' freeze graphic
               CALL TableGen                              ' create user requested tables
               GfxWindow %GFX_UNFREEZE                    ' unfreeze and
            ELSEIF Ikey = %F5key THEN
               GfxWindow %GFX_HIDE                        ' hide graphic
               CALL Searchem                              ' search data files by energy
               GfxWindow %GFX_SHOW                        ' hide graphic
               CURSOR OFF                                 ' set cursor off
            ELSEIF Ikey = %F6key THEN
               GfxWindow %GFX_FREEZE                      ' freeze graphic
               CALL Helpem                                ' user requested help
               GfxWindow %GFX_UNFREEZE                    ' unfreeze graphic
            ELSEIF Ikey = %F7key THEN
               GfxWindow %GFX_HIDE                        ' hide graphic
               CALL Aboutem                               ' about text
               GfxWindow %GFX_SHOW                        ' Show the graphics window
            END IF
         ELSEIF LEN(skey$) = 4 AND iz > 0 THEN            ' an element was selected
            EXIT LOOP                                     ' so exit and get isotope
         END IF
      LOOP
'
      IF INSTR(NukeCover$, Chx$) > 0 THEN                 ' if collection includes this element
         CALL Listex(Chx$, Listx$(), nlist)               ' then get the list of isotopes
         IF nlist = 1 THEN                                ' if single isotope then
            i = 1                                         ' we have no choice
            Nukex$ = LEFT$(Listx$(i), 7)
         ELSE
            IF nlist < 11 THEN                            ' if < 11 then use small list box
               GfxWindow %GFX_FREEZE                      ' free graphic and clear
               INPUT FLUSH                                ' input buffer
               list$ = ConsoleListBox(1, %CONSOLE_CENTER, 0, _
                       "Select Radioisotope of " + ElName(iz) + "...", "MIRD-07 Decay Data", _
                       Listx$(), 1, %RETURN_INDEX, 0)
            ELSE                                          ' use large list box after
               GfxWindow %GFX_FREEZE                      ' freezing graphic and
               INPUT FLUSH                                ' flushing input buffers
               list$ = ConsoleListBox(3, %CONSOLE_CENTER, 0, _
                       "Select Radioisotope of " + ElName(iz) + "...", "MIRD-07 Decay Data", _
                       Listx$(), 1, %RETURN_INDEX, 0)
            END IF
            IF LEN(list$) = 0 THEN                        ' user cancelled selection
               i = 0
            ELSE                                          ' user selected a radionuclide
               i = VAL(list$)
               Nukex$ = LEFT$(Listx$(i), 7)
            END IF
         END IF
         IF i = 0 THEN EXIT IF                            ' no nuclide selected
'
         GfxWindow %GFX_HIDE                              ' hide the frozen graphic
         CLS                                              ' clear screen and now
         CALL ShowTable(Nukex$)                           ' show the nuclide's data
         CURSOR ON                                        ' turn cursor on
         LOCATE 30, 1                                     ' and print prompt line
         COLOR %xYellow
         PRINT $Prompt;
'
         DO                                               ' loop over response to summary table
            INPUT FLUSH
            a$ = WAITKEY$                                 ' a wait the user selecting an action
            IF a$ = $ESC THEN                             ' <Esc> pressed so we
               EXIT LOOP                                  ' exit the loop, back to graphic
            END IF
            Ikey = InKeyCode(a$)                          ' process other user input
            IF Ikey = %F1key THEN
               PCOPY 1, 2                                 ' export the data for this
               CALL Exportem(Nukex$)                      ' nuclide to flat ascii
               PCOPY 2, 1                                 ' files
            ELSEIF Ikey = %F2key THEN
               PCOPY 1, 2                                 ' display decay chain for this
               CALL Chain(Nukex$, 0)                      ' nuclide and the table of
               PCOPY 2, 1                                 ' cummulative energy
               ip = ibinry(Nukex$)                        ' chain may have destroyed parent's
               GET %indx, ip, IndDat                      ' IndDat record - so read again.
            ELSEIF Ikey = %F3key THEN
               CALL Plotem(Nukex$)                        ' line plots of emissions
            ELSEIF Ikey = %F4key THEN
               CALL TableGen                              ' user requested tables
               ip = ibinry(Nukex$)                        ' TableGen may have destroyed parent's
               GET %indx, ip, IndDat                      ' IndDat record - so read again.
            ELSEIF Ikey = %F5key THEN
               PCOPY 1, 2                                 ' identify alpha or photon
               CALL Searchem                              ' emitter given emitted
               ip = ibinry(Nukex$)                        ' Searchem may have destroyed parent's
               GET %indx, ip, IndDat                      ' IndDat record - so read again.
               PCOPY 2, 1                                 ' energy
               LOCATE 30, LEN($Prompt) + 1                ' re-position cursor
               CURSOR ON
            ELSEIF Ikey = %F6key THEN
               CALL Helpem                                ' help
               LOCATE 30, LEN($Prompt) + 1
            ELSEIF Ikey = %F7key THEN
               PCOPY 1, 2
               CALL Aboutem                               ' usual about
               PCOPY 2, 1
               LOCATE 30, LEN($Prompt) + 1                ' re-position cursor
               CURSOR ON
            END IF
         LOOP WHILE (Ikey > 1058 AND Ikey < 1066)
         CLS
         GfxWindow %GFX_UNFREEZE                          ' unfreeze graphic
      ELSE
         INPUT FLUSH
         iresponse = MSGBOX("No isotopes of " + ElName(iz) + _
                            " in MIRD-07;", "")
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF                                       '   or
         ELSE                                             ' just quit
            CLOSE
            EXIT FUNCTION
        END IF
      END IF
   LOOP
   CLOSE
 END FUNCTION

'----------------------------------------------------------------------------------------
 SUB ShowTable (Nukex$)
'----------------------------------------------------------------------------------------
   DIM SumEnergy(1 TO 13), TotalFreq(1 TO 13), NumberRad(1 TO 13)
   cMeVtoJoule = 1.6022E-13
'
   DATA "Gamma rays"        , "X-rays"       , "Annh photons"    , "Beta +"
   DATA "Beta -"            , "IC electrons" , "Auger electrons" , "Alpha particles"
   DATA "Fission fragments" , "Neutrons"     , "Prompt gamma"    , "Delayed gamma"
   DATA "Delayed beta"
'
   nucnam(1) = Nukex$                                     ' parent of chain - nucnam()
   LOCATE 34, 1                                           ' start screen output
   COLOR %xBlack, %xWhite
   PRINT $fline1;
   LOCATE 3, 1
   COLOR %xYellow, %xBlue
   PRINT CenterMess("Summary of " + TRIM$(Nukex$) + " Emissions", 80)
   COLOR %xWhite + %xBright
   PRINT
   ip = ibinry(Nukex$)                                    ' get pointer into NDX file
   GET %indx, ip, IndDat                                  ' get nuclide NDX record
'
   CALL RedDecay(ip, NumberRad(), SumEnergy(), TotalFreq(), SpA)
'
'  all radiations read from RAD file, now write inf out.
'
   Ls$ = " Half-Life : " + LTRIM$(IndDat.T) + " " + IndDat.Tu
   ispace = 80 - (LEN(Ls$) + 25)
   PRINT Ls$ + STRING$(ispace, " ") + "SpA =" + USING$($fmt1, SpA) + " TBq/kg"
   Ls$ = " Decay Mode: " + LEFT$(IndDat.mode, 2) + " " + _
                           MID$(IndDat.mode, 3, 2) + " " + _
                           MID$(IndDat.mode, 5, 2)
   ispace = 80 - (LEN(Ls$) + 25)
   PRINT Ls$ + STRING$(ispace," ") + "Data files: MIRD-07"
   PRINT
'
'  this block lists the radioactive daughters for the isotope
'
   IF VAL(IndDat.idau1) > 0 THEN                          ' we have a radioactive
      PRINT " Radioactive daughters & branching fractions"
      branch1 = VAL(IndDat.bf1)                           ' daughter with branching
      idau1 = VAL(IndDat.idau1)                           ' fraction and index number
      IF VAL(IndDat.idau2) > 0 THEN                       ' check to see if a second
         branch2 = VAL(IndDat.bf2)                        ' daughter is present
         idau2 = VAL(IndDat.idau2)
         IF VAL(IndDat.idau3) <> 0 THEN                   ' if second present how
            branch3 = VAL(IndDat.bf3)                     ' about a third daughter
            idau3 = VAL(IndDat.idau3)
         ELSE
            idau3 = 0                                     ' if no 3rd set idau3 = 0
         END IF
      ELSE
         idau2 = 0                                        ' if no 2nd daughter the set
         idau3 = 0                                        ' idau2 & idau3 to 0
      END IF
      IF idau1 < 9999 THEN                                ' 9999 flag for spont fission
         PRINT " " + IndDat.Dau1 + USING$($fmt1, branch1) + " ";
      ELSE
         PRINT "    SF " + USING$($fmt1, branch1) + " ";
      END IF
      IF idau2 > 0 AND idau2 < 9999 THEN
         PRINT " " + IndDat.Dau2 + USING$($fmt1, branch2)+ " ";
      ELSEIF idau2 = 9999 THEN
         PRINT "     SF    " + USING$($fmt1, branch2) + " ";
      END IF
      IF idau3 > 0 AND idau3 < 9999 THEN
         PRINT " " + IndDat.Dau3 + USING$($fmt1, branch3)+ " ";
      ELSEIF idau3 = 9999 THEN
         PRINT "     SF    " + USING$($fmt1, branch3) +" ";
      END IF
      PRINT
   END IF
'
   PRINT                                                  ' now print the data table
   PRINT
   PRINT STRING$(31," ") + "Yield     Energy   Mean Energy"
   PRINT STRING$(31," ") + "ä Yi      ä Yi*Ei   äYi*Ei/äYi   Delta"
   PRINT "      Radiation       Number   (/nt)     (MeV/nt)    (MeV)    (Gy kg/nt)"
   PRINT "     " + STRING$(67, "-")
   TotalEnergy = 0.0#
   Delta = 0.0#
   FOR ic = 1 TO 13
      IF NumberRad(ic) > 0 THEN
         PRINT "     " + READ$(ic) + STRING$(17 - LEN(READ$(ic))," ") + _
               USING$("#####", NumberRad(ic)) + " " + _
               USING$($fmt1, TotalFreq(ic)) + " " + _
               USING$($fmt1, SumEnergy(ic)) + " " + _
               USING$($fmt1, SumEnergy(ic) / TotalFreq(ic)) + " " + _
               USING$($fmt1, cMeVtoJoule * SumEnergy(ic))
         TotalEnergy = TotalEnergy + SumEnergy(ic)
         Delta = Delta + cMeVtoJoule * SumEnergy(ic)
      END IF
   NEXT ic
   PRINT TAB(33) "Totals " + USING$($fmt1, TotalEnergy) + STRING$(12, " ") + _
                             USING$($fmt1, Delta)
   PRINT
'
   IF VAL(IndDat.Kpts) > 0.0 THEN
      PRINT TAB(10) USING$ ("Point Source Air-Kerma Coefficient =##.##^^^^ Gy m^2/(Bq s)", VAL(IndDat.Kpts))
   END IF
   IF VAL(IndDat.Kair) > 0.0 THEN
      PRINT TAB(21) USING$ ("Air Kerma-Rate Constant =##.##^^^^ Gy m^2/(Bq s)", VAL(IndDat.Kair))
   END IF
'
   SLEEP 100
 END SUB

'----------------------------------------------------------------------------------------
 SUB RedDecay(ip, NumberRad(), SumEnergy(), TotalFreq(), SpA)
'----------------------------------------------------------------------------------------
   jdec = VAL(IndDat.mdec)                                ' location of rad records
   GET %irad, jdec, DecHed                                ' get the record
   ndecay = VAL(DecHed.ndec)                              ' number of radiations
   IF VAL(IndDat.Mass) = 0 THEN                           ' compute specific activity
      SpA = 0#
   ELSE
      SpA = 6.0231D+23 * 0.693147# / Timest(IndDat.T, IndDat.tu) / VAL(IndDat.Mass)
      SpA = 1.0D-09 * SpA                                 ' (TBq/kg)
   END IF
   FOR i = 1 TO 13                                        ' zero radiation cummulators
      NumberRad(i) = 0                                    ' # radiations of type i
      SumEnergy(i) = 0#                                   ' total energy of type i
      TotalFreq(i) = 0#                                   ' total frequency of type i
   NEXT i

   FOR i = 1 TO ndecay                                    ' get radiation records
      GET %irad, , DecDat                                 ' begining at IndDat.mdec
      ic = VAL(DecDat.icode)                              ' radiation icode
      Freq = VAL(DecDat.Freq)                             ' we don't have zero
      Enr = VAL(DecDat.E)                                 ' frequencies or energies
      IF ic < 9 THEN                                      ' note use of mnemonic (jcode)
         IF ic = 1 AND DecDat.jcode = " PG" THEN          ' prompt SF photons
            INCR NumberRad(11)
            SumEnergy(11) = SumEnergy(11) + Freq * Enr
            TotalFreq(11) = TotalFreq(11) + Freq
         ELSEIF ic = 1 AND DecDat.jcode = " DG" THEN      ' delayed SF photons
            INCR NumberRad(12)
            SumEnergy(12) = SumEnergy(12) + Freq * Enr
            TotalFreq(12) = TotalFreq(12) + Freq
         ELSEIF ic = 5 AND DecDat.jcode = " DB" THEN      ' delayed SF beta
            INCR NumberRad(13)
            SumEnergy(13) = SumEnergy(13) + Freq * Enr
            TotalFreq(13) = TotalFreq(13) + Freq
         ELSE                                             ' rest of radiations
            INCR NumberRad(ic)
            SumEnergy(ic) = SumEnergy(ic) + Freq * Enr
            TotalFreq(ic) = TotalFreq(ic) + Freq
         END IF
      ELSEIF ic = 9 THEN                                  ' alpha recoil add energy
         SumEnergy(8) = SumEnergy(8) + Freq * Enr         ' to that of alpha (8) but
                                                          ' no incr of NumberRad(8)
      ELSEIF ic = 10 THEN                                 ' fission fragment
         INCR NumberRad(9)
         SumEnergy(9) = SumEnergy(9) + Freq * Enr
         TotalFreq(9) = TotalFreq(9) + Freq
      ELSEIF ic = 11 THEN                                 ' SF neutron emission
         INCR NumberRad(10)
         SumEnergy(10) = SumEnergy(10) + Freq * Enr
         TotalFreq(10) = TotalFreq(10) + Freq
      ELSEIF ic < 1 OR ic > 11 THEN                       ' trap any error
         PRINT "i, irec, ndecay, ic, Freq ="; i, mdecay+i, ndecay, ic, Freq
         PRINT "This is a fatal error. Abort and check data files."
         WAITKEY$
      END IF
   NEXT i
 END SUB

'----------------------------------------------------------------------------------------
 SUB Exportem(nukex$)
'----------------------------------------------------------------------------------------
   CLS                                                   ' clear screen
   LOCATE 2, 1
   COLOR %xYellow
   PRINT CenterMess("Export Decay Data of " + TRIM$(Nukex$) + " to ASCII data files", 80)
   COLOR %xWhite + %xBright
   PRINT
   OutFile$ = $OutDir + TRIM$(Nukex$) + ".RAD"           ' open output file
   OPEN OutFile$ FOR OUTPUT AS %iout
   PRINT #%iout, "Output File "+ OutFile$ + " for " + Nukex$
   mrads = VAL(IndDat.mdec)                              '
   GET %irad, mrads, DecHed
   PRINT #%iout, DecHed.nuke + "   " + DecHed.T + DecHed.tu + "    " + DecHed.ndec
   PRINT #%iout, "T1/2 = "; TRIM$(IndDat.T) + TRIM$(IndDat.tu) + " Decay Mode: "; TRIM$(IndDat.Mode)
   nrecord = VAL(DecHed.ndec)
   PRINT #%iout, "Radiations of each type listed in increasing energy"
'
   IF VAL(IndDat.np10) + VAL(IndDat.npg10) > 0 THEN
      PRINT #%iout, "Number of photon radiations:"; VAL(IndDat.np10) + VAL(IndDat.npg10)
   END IF
'
   IF VAL(IndDat.nbet) > 0 THEN
      PRINT #%iout, "Number of beta radiations:"; VAL(IndDat.nbet)
   END IF
'
   IF VAL(IndDat.nel) > 0 THEN
      PRINT #%iout, "Number of monoenergetic electron radiations:"; VAL(IndDat.nel)
   END IF
'
   IF VAL(IndDat.nalpha) > 0 THEN
      PRINT #%iout, "Number of alpha radiations:"; VAL(IndDat.nalpha)
      PRINT #%iout, "Number of alpha recoil radiations:"; VAL(IndDat.nalpha)
   END IF
'
   PRINT #%iout, "ICODE  Y (/nt) E(MeV) Mnemonic"
   PRINT #%iout, "START RADIATION RECORDS"
   FOR i = 1 TO nrecord
      GET %irad, , DecDat
      PRINT #%iout, DecDat.icode; DecDat.Freq; DecDat.E; DecDat.jcode
   NEXT i
   PRINT #%iout, "END RADIATION RECORDS"
   CLOSE #%iout
   PRINT
   PRINT " File "; TRIM$(RIGHT$(OutFile$, -INSTR(OutFile$, "\"))); " written in the OUTPUT folder."
'
   IF VAL(IndDat.mbet) > 0 THEN
      OutFile$ = $OutDir + TRIM$(Nukex$) + ".BET"
      OPEN OutFile$ FOR OUTPUT AS %iout
      PRINT #%iout, "Output File "+ OutFile$ + " for " + Nukex$
      mbeta = VAL(IndDat.mbet)
      GET %ibet, mbeta, BetHed
      PRINT #%iout, BetHed.Nuke + BetHed.nbet
      PRINT #%iout, "Beta Spectrum for "; Nukex$
      PRINT #%iout, "Number of energy points: "; TRIM$(BetHed.nbet)
      PRINT #%iout, "E(MeV)  P(E) dE"
      nrecord = VAL(BetHed.nbet)
      PRINT #%iout, "START RADIATION RECORDS"
      FOR i = 1 TO nrecord
         GET %ibet, , BetSpec
         PRINT #%iout, BetSpec.Eelc; BetSpec.Freq
      NEXT i
      PRINT #%iout, "END RADIATION RECORDS"
      CLOSE #%iout
      PRINT " File "; TRIM$(RIGHT$(OutFile$, -INSTR(OutFile$, "\"))); " written in the OUTPUT folder."
   END IF
'
   IF VAL(IndDat.mack) > 0 THEN
      OutFile$ = $OutDir + TRIM$(Nukex$) + ".ACK"
      OPEN OutFile$ FOR OUTPUT AS %iout
      PRINT #%iout, "Output File "+ OutFile$ + " for " + Nukex$
      mauger = VAL(IndDat.mack)                    ' location of detailed auger
      GET %iaug, mauger, AugHed
      PRINT #%iout, AugHed.Nuke + STRING$(20, " ") + AugHed.naug
      PRINT #%iout, "Auger/Coster-Kronig Spectrum for "; Nukex$
      PRINT #%iout, "Number of electrons: "; TRIM$(AugHed.naug)
      PRINT #%iout, "START RADIATION RECORDS"
      PRINT #%iout, "  Y(/nt)       E(eV)   transition"
      nrecord = VAL(AugHed.naug)
      FOR i = 1 TO nrecord
         GET %iaug, , AugDat
         PRINT #%iout, AugDat.Freq; AugDat.E; AugDat.tran
      NEXT i
      PRINT #%iout, "END RADIATION RECORDS"
      CLOSE #%iout
      PRINT " File "; TRIM$(RIGHT$(OutFile$, -INSTR(OutFile$, "\"))); " written in the OUTPUT folder."
   END IF
'
   IF VAL(IndDat.mneu) > 0 THEN
      OutFile$ = $OutDir + TRIM$(Nukex$) + ".NSF"
      OPEN OutFile$ FOR OUTPUT AS %iout
      PRINT #%iout, "Output File "+ OutFile$ + " for " + Nukex$
      meutron = VAL(IndDat.mneu)
      GET %ineu, meutron, NeuHed
      PRINT #%iout, NeuHed.Nuke + NeuHed.sfnt + STRING$(9, " ") + NeuHed.npts
      PRINT #%iout, "Neutron Spectrum for "; Nukex$
      PRINT #%iout, "Number of energy bins: "; TRIM$(NeuHed.npts)
      PRINT #%iout, "E1 (MeV) E2 (MeV) Yield (/nt)"
      PRINT #%iout, "START RADIATION RECORDS"
      nrecord = VAL(NeuHed.npts)
      FOR j = 1 TO nrecord
         GET %ineu, , NeuDat
         PRINT #%iout, NeuDat.E1; NeuDat.E2; NeuDat.Yield
      NEXT j
      PRINT #%iout, "END RADIATION RECORDS"
      CLOSE #%iout
      PRINT " File "; TRIM$(RIGHT$(OutFile$, -INSTR(OutFile$, "\"))); " written in the OUTPUT folder."
   END IF
'
   LOCATE 30
   COLOR %xYellow
   PRINT $Prompt;
   INPUT FLUSH
   WAITKEY$
   SLEEP 100
 END SUB

'----------------------------------------------------------------------------------------
 SUB Plotem(nuke$)
'----------------------------------------------------------------------------------------
'  routine uses DPlotJr to create plots of the emitted radiations
   REDIM x(0 TO 100) AS LOCAL SINGLE                     ' DPlotJr variables
   REDIM y(0 TO 100) AS LOCAL SINGLE
   DIM cmds AS LOCAL STRING
   Dp.Version = %DPLOT_DDE_VERSION
   Dp.hwnd = CONSHNDL
   Dp.DataFormat = %DATA_XYXY
   Dp.LegendX = 0.05
   Dp.LegendY = 0.05
   ylow = 1.0E+30                                         ' lower yield initializes
   mrads = VAL(IndDat.mdec)                               ' location of records NDX
   IF GetFileCount($data + "DPlotJr.ini") > 0 THEN        ' if file present then
      MaxPlot = %true                                     ' plots are expanded
   ELSE                                                   ' else
      MaxPlot = %false                                    ' displayed in cascade
   END IF
   iplots = 0                                             ' counter of plots
   ngamma = VAL(IndDat.np10) + VAL(IndDat.npg10)          ' # photons
   nbeta = VAL(IndDat.nbet)                               ' # beta transitions
   nelectron = VAL(IndDat.nel)                            ' # electrons
   nalpha = VAL(IndDat.nalpha)                            ' # alpha
'
'  beta sprectra
'
   IF VAL(IndDat.mbet) > 0 THEN
      INCR iplots
      mbeta = VAL(IndDat.mbet)                            ' locate of beta records
      GET %ibet, mbeta, BetHed                            ' get head records amd
      ne = VAL(BetHed.nbet)                               ' number of spectral points
      REDIM x(0 TO ne - 1)                                ' dimension plot arrays
      REDIM y(0 TO ne - 1)
      FOR j = 0 TO Ne - 1                                 ' fill the arrays
         GET %ibet, , BetSpec
         x(j) = VAL(BetSpec.Eelc)
         y(j) = VAL(BetSpec.Freq)
      NEXT j
      Dp.Legend(0) = ""                                   ' no legend
      Dp.Legend(1) = ""                                   ' no legend
      Dp.SymbolType(0) = 0                                ' no plot symbol
      Dp.LineType(0) = %LINESTYLE_SOLID                   ' solid line
      Dp.MaxCurves = 2                                    ' Must be >= # curves
      Dp.MaxPoints = ne                                   ' Anything >= NP will do
      Dp.NumCurves = 1                                    ' 1 curve
      Dp.ScaleCode = %SCALE_LINEARX_LINEARY               ' linear both axis
      Dp.NP(0) = ne                                       ' # points
      Dp.Title(0) = TRIM$(Nuke$) + " Beta Spectrum"       ' title
      Dp.XAxis = "Energy (MeV)" & CHR$(0)                 ' x & y axis labels
      Dp.YAxis = "Y(E) dE (/MeV/nt)" & CHR$(0)
      cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
      cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) & " Beta Spectrum" & CHR$(34) & ")]"
      IF ngamma + nelectron + nalpha = 0 THEN             ' max plot of only plot
         cmds = cmds & "[DocMaximize()][ClearEditFlag()]"
      ELSE
         IF ISTRUE MaxPlot THEN                           ' max by user
            cmds = cmds & "[DocMaximize()][ClearEditFlag()]"
         ELSE
            cmds = cmds & "[ClearEditFlag()]"             ' else cascade
         END IF
      END IF
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' create the plot
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
            iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF
         ELSE                                             ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
'
'  photons
'
   IF ngamma > 1 THEN                                     ' plot photons if n > 1
      INCR iplots
      REDIM x(0 TO 3 * ngamma - 1)                        ' dim the arrays
      REDIM y(0 TO 3 * ngamma - 1)
      ylow = 1.0E+30
      nx = 0 : ng = 0 : npp = 0 : npg = 0 : ndg = 0       ' zero some counters
      GET %irad, mrads, DecHed                            ' get nuclide head record
      FOR j = 1 TO Ngamma                                 ' run through photon
         GET %irad, , DecDat                              ' records to see the data
         yx = VAL(DecDat.Freq)                            ' and find lowest yield
         IF yx < ylow THEN ylow = yx                      '
         IF VAL(DecDat.icode) = 2 THEN                    '
            INCR nx                                       ' incr x-ray count
         ELSEIF VAL(DecDat.icode) = 3 THEN
            INCR npp                                      ' incr annhilation count
         ELSEIF VAL(DecDat.icode) = 1 AND DecDat.jcode = " PG" THEN
            INCR npg                                      ' incr prompt gamma
         ELSEIF VAL(DecDat.icode) = 1 AND DecDat.jcode = " DG" THEN
            INCR ndg                                      ' incr delayed gamma
         ELSE
            INCR ng                                       ' incr gamma
         END IF
      NEXT j
      ylow = 10^INT(LOG10(ylow))                          ' round low yield
      ix = -1
      ig = 3 * nx - 1                                     ' set the boundaries
      ipp = 3 * (nx + ng) - 1                             ' for the x & y arrays to
      ipg = 3 * (nx + ng + npp) - 1                       ' hold the different
      idg = 3 * (nx + ng +  npp + npg) - 1                ' photon data
      GET %irad, mrads, DecHed                            ' get again the header
      FOR j = 1 TO Ngamma                                 ' and go through the
         GET %irad, , DecDat                              ' data filling the arrays
         IF VAL(DecDat.icode) = 2 THEN                    ' x-ray data
            INCR ix
            x(ix) = VAL(DecDat.E)
            y(ix) = ylow
            INCR ix
            x(ix) = x(ix - 1)
            y(ix) = VAL(DecDat.Freq)
            INCR ix
            y(ix) = x(ix - 1)
            y(ix) = ylow
         ELSEIF VAL(DecDat.icode) = 3 THEN                ' annihilation
            INCR ipp
            x(ipp) = VAL(DecDat.E)
            y(ipp) = ylow
            INCR ipp
            x(ipp) = x(ipp - 1)
            y(ipp) = VAL(DecDat.Freq)
            INCR ipp
            x(ipp) = x(ipp - 1)
            y(ipp) = ylow
         ELSEIF VAL(DecDat.icode) = 1 AND DecDat.jcode = " PG" THEN 'prompt gamma
            INCR ipg
            x(ipg) = VAL(DecDat.E)
            y(ipg) = ylow
            INCR ipg
            x(ipg) = x(ipg - 1)
            y(ipg) = VAL(DecDat.Freq)
            INCR ipg
            x(ipg) = x(ipg - 1)
            y(ipg) = ylow
         ELSEIF VAL(DecDat.icode) = 1 AND DecDat.jcode = " DG" THEN  ' dealayed gamma
            INCR idg
            x(idg) = VAL(DecDat.E)
            y(idg) = ylow
            INCR idg
            x(idg) = x(idg - 1)
            y(idg) = VAL(DecDat.Freq)
            INCR idg
            x(idg) = x(idg - 1)
            y(idg) = ylow
         ELSE                                             ' gamma
            INCR ig
            x(ig) = VAL(DecDat.E)
            y(ig) = ylow
            INCR ig
            x(ig) = x(ig - 1)
            y(ig) = VAL(DecDat.Freq)
            INCR ig
            x(ig) = x(ig - 1)
            y(ig) = ylow
         END IF
      NEXT j
'
      Dp.MaxCurves = 6                                    ' > # of curves
      Dp.MaxPoints = 3 * ngamma                           ' Anything >= # points
      Dp.Legend(0) = "Photons"                            ' need a legend
      IF ISTRUE MaxPlot THEN                              ' do a user wishes
         cmds = "[DocMaximize()][AutoScale()][NumTicks(1,10,10)]"
      ELSE
         cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
      END IF
      cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(6,14,  0,0,0,0,0,""Arial"")]"
      ncur = 0
      IF nx > 0 THEN                                      ' we have x-rays
         INCR ncur
         Dp.SymbolType(ncur-1) = 0
         Dp.LineType(ncur-1) = %LINESTYLE_SOLID
         Dp.NP(ncur - 1) = 3 * nx
         Dp.Legend(ncur) = "X-rays"
'        set color to magenata
         cmds = cmds + "[LineWidth(" + STR$(ncur,1) + ",30)][color(" + STR$(ncur,1) + ", 255, 0, 255)]"
      END IF
      IF ng > 0 THEN                                      ' we have gammas
         INCR ncur
         Dp.SymbolType(ncur-1) = 0
         Dp.LineType(ncur-1) = %LINESTYLE_SOLID
         Dp.NP(ncur - 1) = 3 * ng
         Dp.Legend(ncur) = "Gamma rays"
'        set color to red
         cmds = cmds + "[LineWidth(" + STR$(ncur,1) + ",30)][color(" + STR$(ncur,1) + ", 255, 0, 0)]"
      END IF
      IF npp > 0 THEN                                     ' we have annihilation
         INCR ncur
         Dp.SymbolType(ncur-1) = 0
         Dp.LineType(ncur-1) = %LINESTYLE_SOLID
         Dp.NP(ncur - 1) = 3 * npp
         Dp.Legend(ncur) = "Annihilation quanta"
'        set color to black
         cmds = cmds + "[LineWidth(" + STR$(ncur,1) + ",30)][color(" + STR$(ncur, 1) + ", 0, 0, 0)]"
      END IF
      IF npg > 0 THEN                                     ' we have prompt gammas
         INCR ncur
         Dp.SymbolType(ncur-1) = 0
         Dp.LineType(ncur-1) = %LINESTYLE_SOLID
         Dp.NP(ncur - 1) = 3 * npg
         Dp.Legend(ncur) = "Prompt gamma rays"
'        set color to blue
         cmds = cmds + "[LineWidth(" + STR$(ncur,1) + ",30)][color(" + STR$(ncur, 1) + ", 0, 0, 255)]"
      END IF
      IF ndg > 0 THEN                                     ' we have delayed gammas
         INCR ncur
         Dp.SymbolType(ncur-1) = 0
         Dp.LineType(ncur-1) = %LINESTYLE_SOLID
         Dp.NP(ncur - 1) = 3 * ndg
         Dp.Legend(ncur) = "Delayed gamma rays"
'        set color to cyan
         cmds = cmds + "[LineWidth(" + STR$(ncur,1) + ",30)][color(" + STR$(ncur, 1) + ", 0, 255, 255)]"
      END IF
      Dp.NumCurves = ncur                                 ' # curves
      Dp.ScaleCode = %SCALE_LOGX_LOGY                     ' log-log scale
      Dp.Title(0) = TRIM$(Nuke$) + " Photon Line Spectrum"
      Dp.XAxis = "Photon energy (MeV)" & CHR$(0)
      Dp.YAxis = "Yield per nuclear transformation" & CHR$(0)
      cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) & " Photon Line Spectrum" & CHR$(34) & ")]"
      cmds = cmds & "[ClearEditFlag()]"
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' plot the info
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
           iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF
         ELSE                                             ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
'
'  electrons
'
   IF nelectron > 1 THEN                                  ' discrete electrons
      INCR iplots
      GET %irad, mrads + ngamma + nbeta, DecDat
      na = 0                                              ' auger-CK electrons
      ni = 0                                              ' int conv electrons
      ylow = 1.0E+30
      FOR j = 1 TO nelectron                              ' read through the data
         GET %irad, , DecDat
         IF VAL(DecDat.icode) = 7 THEN                    ' counting auger
            INCR na
         ELSE                                             ' and
            INCR ni                                       ' ic electrons
         END IF
         yx = VAL(DecDat.Freq)                            ' and finding the
         IF yx < ylow THEN ylow = yx                      ' low yield
      NEXT j
      REDIM x(0 TO 3 * nelectron - 1)                     ' dimension the
      REDIM y(0 TO 3 * nelectron - 1)                     ' x & y arrays
      ylow = 10^INT(LOG10(ylow))                          ' round low yield
      GET %irad, mrads + ngamma + nbeta, DecDat           ' reposition read
      IF na * ni > 0 THEN                                 ' here we have both na & ni
         i1 = -1
         i2 = -1
         ioffset = 3 * na - 1
         FOR j = 1 TO nelectron                           ' now read electrons
            GET %irad, , DecDat                           ' get record
            IF VAL(DecDat.icode) = 7 THEN                 ' we have an Auger record
               INCR i1
               x(i1) = VAL(DecDat.E)
               y(i1) = ylow
               INCR i1
               x(i1) = x(i1 - 1)
               y(i1) = VAL(DecDat.Freq)
               INCR i1
               x(i1) = x(i1 - 1)
               y(i1) = ylow
            ELSE                                          ' here for ic record
               INCR ioffset
               x(ioffset) = VAL(DecDat.E)
               y(ioffset) = ylow
               INCR ioffset
               x(ioffset) = x(ioffset - 1)
               y(ioffset) = VAL(DecDat.Freq)
               INCR ioffset
               x(ioffset) = x(ioffset - 1)
               y(ioffset) = ylow
            END IF
         NEXT j
         Dp.MaxCurves = 3                                 ' >= # of curves
         Dp.MaxPoints = 3 * nelectron                     ' Anything >= # points
         Dp.NumCurves = 2                                 ' we have 2 curves
         Dp.ScaleCode = %SCALE_LOGX_LOGY                  ' log-log plot
         Dp.NP(0) = 3 * na                                ' # auger points
         Dp.Np(1) = 3 * ni                                ' # ic points
         Dp.Legend(0) = "Electrons"                       ' we need a legend
         Dp.Legend(1) = "Auger-CK"                        ' with Auger label
         Dp.Legend(2) = "Int. Conversion"                 ' and IC label
         Dp.SymbolType(0) = 0                             ' no symbols for
         Dp.SymbolType(1) = 0                             ' either plot
         Dp.LineType(0) = %LINESTYLE_SOLID                ' both with solid
         Dp.LineType(1) = %LINESTYLE_SOLID                ' lines
         Dp.Title(0) = TRIM$(Nuke$) + " Monoenergetic Electron Line Spectrum" 'Title
         Dp.XAxis = "Electron energy (MeV)" & CHR$(0)     ' xaxis label
         Dp.YAxis = "Yield per nuclear transformation" & CHR$(0) ' yaxis label
         IF ISTRUE MaxPlot THEN                           ' do users wish
            cmds = "[DocMaximize()][AutoScale()][NumTicks(1,10,10)]"
         ELSE
            cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
         END IF
         cmds = cmds + "[LineWidth(1,30)][LineWidth(2,30)]"
         cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
         cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
         cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
         cmds = cmds & "[TextFont(6,14,  0,0,0,0,0,""Arial"")]"
         cmds = cmds & "[color(1,255,0,255)][color(2,255,0,0)][NumTicks(1,10,10)]"
         cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) &" Discrete Electron Spectrum" & CHR$(34) & ")]"
         cmds = cmds & "[ClearEditFlag()]"
      ELSE                                                ' only one type of electron
         i1 = -1
         FOR j = 1 TO nelectron                           ' now read through and fill
            GET %irad, , DecDat                           ' the arrays
            INCR i1
            x(i1) = VAL(DecDat.E)
            y(i1) = ylow
            INCR i1
            x(i1) = x(i1 - 1)
            y(i1) = VAL(DecDat.Freq)
            INCR i1
            x(i1) = x(i1 - 1)
            y(i1) = ylow
         NEXT j
         Dp.MaxCurves = 3                                 ' >= # of curves
         Dp.MaxPoints = 3 * nelectron                     ' >= # points
         Dp.NumCurves = 1                                 ' # of curves
         Dp.ScaleCode = %SCALE_LOGX_LOGY                  ' log-log plot
         Dp.NP(0) = 3 * nelectron                         ' # points
         Dp.SymbolType(0) = 0                             ' no symbols
         Dp.LineType(0) = %LINESTYLE_SOLID                ' solid line
         Dp.Legend(0) = ""                                ' we don't need a legend
         Dp.Legend(1) = ""                                ' and entry
         IF na > 0 THEN                                   ' identify electron type
            Dp.Title(0) = Nuke$ + " Auger-CK Electron Line Spectrum"
         ELSE                                             ' in the title
            Dp.Title(0) = Nuke$ + " Internal Conversion Electron Line Spectrum"
         END IF
         Dp.XAxis = "Electron energy (MeV)" & CHR$(0)     ' label x & y axis
         Dp.YAxis = "Yield per nuclear transformation" & CHR$(0)
         IF ISTRUE MaxPlot THEN                           ' as user wishes
            cmds = "[AutoScale()][NumTicks(1,10,10)]"
         ELSE
            cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
         END IF
         cmds = cmds + "[LineWidth(1,30)][LineWidth(2,30)]"
         cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
         cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
         cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
         cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) &" Discrete Electron Spectrum" & CHR$(34) & ")]"
         cmds = cmds & "[ClearEditFlag()]"
      END IF
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' do the plot
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
            iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF
         ELSE                                             ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
'
'  alpha particles
'
   IF nalpha > 1 THEN                                     ' we have alphas
      INCR iplots
      REDIM x(0 TO 3 * nalpha - 1)                        ' dim the arrays
      REDIM y(0 TO 3 * nalpha - 1)
      GET %irad, mrads + ngamma + nbeta + nelectron, DecDat ' position
      ylow = 1.0E+30
      FOR j = 1 TO nalpha                                 ' read throught the data
         GET %irad, , DecDat
         yx = VAL(DecDat.Freq)
         IF yx < ylow THEN ylow = yx
      NEXT j
      ylow = 10^INT(LOG10(ylow))                          ' rounded yield
      GET %irad, mrads + ngamma + nbeta + nelectron, DecDat 'reposition
      ii = -1
      FOR j = 1 TO nalpha                                 ' now get the data
         GET %irad, , DecDat
         INCR ii
         x(ii) = VAL(DecDat.E)
         y(ii) = ylow
         INCR ii
         x(ii) = x(ii - 1)
         y(ii) = VAL(DecDat.Freq)
         INCR ii
         x(ii) = x(ii - 1)
         y(ii) = ylow
      NEXT j
      Dp.Legend(0) = ""                                   ' no legend
      Dp.Legend(1) = ""
      Dp.SymbolType(0) = 0                                ' no sysmbol
      Dp.LineType(0) = %LINESTYLE_SOLID                   ' solid line
      Dp.MaxCurves = 3                                    ' >= # of curves
      Dp.MaxPoints = 3 * nalpha                           ' >= # points
      Dp.NumCurves = 1                                    ' # curves
      Dp.ScaleCode = %SCALE_LINEARX_LOGY                  ' log log scale
      Dp.NP(0) = 3 * nalpha                               ' # points
      Dp.Title(0) = TRIM$(Nuke$) + " Alpha Particle Line Spectrum" ' title
      Dp.XAxis = "Alpha particle energy (MeV)" & CHR$(0)  ' x & y axis labels
      Dp.YAxis = "Yield per nuclear transformation" & CHR$(0)
      IF ISTRUE MaxPlot THEN                              ' as user wishes
         cmds = "[DocMaximize()][AutoScale()][NumTicks(1,10,10)]"
      ELSE
         cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
      END IF
      cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) &" Alpha Spectrum"  & CHR$(34) & ")]"
      cmds = cmds & "[ClearEditFlag()]"
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' do the plot
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
            iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF
         ELSE                                             ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
'
'  Auger-CK electrons
'
   IF VAL(IndDat.mack) > 0 THEN                           ' have detailed spectrum
      INCR iplots
      mauger = VAL(IndDat.mack)
      GET %iaug, mauger, AugHed                           ' get head record
      nauger = VAL(AugHed.naug)                           ' # points
      REDIM x(0 TO 3 * nauger - 1)                        ' dim x & y arrays
      REDIM y(0 TO 3 * nauger - 1)
      ylow = 1.0E+30
      FOR j = 1 TO Nauger                                 ' read through to
         GET %iaug, , AugDat                              ' find min yield
         yx = VAL(AugDat.Freq)
         IF yx < ylow THEN ylow = yx
      NEXT i
      ylow = 10^INT(LOG10(ylow))                          ' round min yield
      GET %iaug, mauger, AugHed                           ' reposition
      ii = -1
      FOR j = 1 TO Nauger                                 ' now get the data
         GET %iaug, , AugDat
         INCR ii
         x(ii) = VAL(AugDat.E)
         y(ii) = ylow
         INCR ii
         x(ii) = x(ii - 1)
         y(ii) = VAL(AugDat.Freq)
         INCR ii
         x(ii) = x(ii - 1)
         y(ii) = ylow
      NEXT j
      Dp.Legend(0) = ""                                   ' no legend
      Dp.Legend(1) = ""                                   '
      Dp.MaxCurves = 3                                    '  >= # of curves
      Dp.MaxPoints = 3 * nauger                           '  >= # points
      Dp.NumCurves = 1                                    ' # curves
      Dp.ScaleCode = %SCALE_LOGX_LOGY                     ' log log axis
      Dp.NP(0) = 3 * nauger                               ' # points
      Dp.Title(0) = TRIM$(Nuke$) + " Detailed Auger-CK Electron Line Spectrum" 'Title
      Dp.XAxis = "Auger electron energy (eV)" & CHR$(0)   ' axis labels
      Dp.YAxis = "Yield per nuclear transformation" & CHR$(0)
      IF ISFALSE MaxPlot THEN                             ' as users wishes
         cmds = "[DocMaximize()][AutoScale()][NumTicks(1,10,10)]"
      ELSE
         cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
      END IF
      cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) & " Detailed Auger-CK Spectrum" & CHR$(34) & ")]"
      cmds = cmds & "[LineWidth(1,30)][ClearEditFlag()]"
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' do the plot
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
            iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                       ' try again
            EXIT IF
         ELSE                                             ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
'
'  neutrons
'
   IF VAL(IndDat.mneu) > 0 THEN                           ' we have neutrons
      INCR iplots
      meutron = VAL(IndDat.mneu)                          ' location of data
      GET %ineu, meutron, NeuHed                          ' get head record
      neutron = VAL(NeuHed.npts)                          ' number of points
      REDIM x(0 TO 2 * neutron - 1)                       ' dim arrays
      REDIM y(0 TO 2 * neutron - 1)
      ii = -1
      FOR j = 1 TO neutron                                ' get the data
         GET %ineu, , NeuDat
         INCR ii
         x(ii) = VAL(NeuDat.E1)
         y(ii) = VAL(NeuDat.Yield)
         INCR ii
         x(ii) = VAL(NeuDat.E2)
         y(ii) = y(ii-1)
      NEXT j
      Dp.Legend(0) = ""                                   ' no leged
      Dp.Legend(1) = ""
      Dp.MaxCurves = 2                                    ' >= # of curves
      Dp.MaxPoints = 2 * neutron                          ' >= # of points
      Dp.NumCurves = 1                                    ' # curves
      Dp.ScaleCode = %SCALE_LOGX_LOGY                     ' log log axis
      Dp.NP(0) = 2 * neutron                              ' # points
      Dp.LineType(0) =  %LINESTYLE_SOLID                  ' solid line
      Dp.Title(0) = TRIM$(Nuke$) + " Spontaneous Fission Neutron Spectrum"
      Dp.XAxis = "Neutron energy (MeV)" & CHR$(0)         ' axis labels
      Dp.YAxis = "Yield per nuclear transformation" & CHR$(0)
      IF ISTRUE MaxPlot THEN                              ' as user wishes
         cmds = "[DocMaximize()][AutoScale()][NumTicks(1,10,10)]"
      ELSE
         cmds = "[WindowCascade()][AutoScale()][NumTicks(1,10,10)]"
      END IF
      cmds = cmds & "[TextFont(2,16,700,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(4,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[TextFont(5,14,  0,0,0,0,0,""Arial"")]"
      cmds = cmds & "[Caption(" & CHR$(34) & " " & TRIM$(Nuke$) & " Spontaneous Fission Neutron Spectrum" & CHR$(34) & ")]"
      cmds = cmds & "[ClearEditFlag()]"
      iret = DPlot_Plot(Dp, x(0), y(0), cmds)             ' do the plot
      IF iret < 0 THEN
         INPUT FLUSH
         IF iret = -1 THEN
            iresponse = MSGBOX("Can not find DPlotJr. ", "")
         ELSEIF iret = -2 THEN
            iresponse = MsgBox("Can not establish DDE connection. ", "")
         ELSEIF iret = -3 THEN
            iresponse = MsgBox("Data format error. ", "")
         ELSEIF iret = -4 THEN
            iresponse = MsgBox("Incorrect DPlotJr Ver. number specified. ","")
         END IF
         IF iresponse = %IDYES THEN                ' try again
            EXIT IF
         ELSE                                      ' just quit
            EXIT SUB
         END IF
      END IF
   END IF
   SLEEP 100
   IF iplots = 0 THEN
      iresponse = MSGBOX("Single emission - no plots generated, ",_
                          TRIM$(Nuke$) + " Decay Data")
   END IF
 END SUB

'----------------------------------------------------------------------------------------
 SUB TableGen
'----------------------------------------------------------------------------------------
'  routine creates tables per user's request. the created file will be opened by
'  the user's application that opens files with extension TXT.
   LOCAL zText AS ASCIIZ * %MAX_PATH
   LOCAL hInst AS DWORD, lRes AS DWORD, lRes2 AS DWORD, lRes3 AS DWORD
'
   DIM ListTab$(1 TO 16)                                  ' request list
   ListTab$(1)  = "Atomic Number(Z)"
   ListTab$(2)  = "Physical Half-life"
   ListTab$(3)  = "Total Emitted Energy"
   ListTab$(4)  = "Decay by Alpha Emission"
   ListTab$(5)  = "Decay by Beta Minus Emission"
   ListTab$(6)  = "Decay by Beta Plus Emission"
   ListTab$(7)  = "Decay by Internal Transition"
   ListTab$(8)  = "Decay by Spontaneous Fission"
   ListTab$(9)  = "Detailed Auger-CK Spectra"
   ListTab$(10)  = "Principle Alpha Emission"
   ListTab$(11) = "Principle Beta Transition"
   ListTab$(12) = "Principle Photon Emission"
   ListTab$(13) = "Point Source Air-Kerma Coefficient"
   ListTab$(14) = "Serial Decay Chains"
   ListTab$(15) = "Dimensions of MIRD-07 Collection"
   ListTab$(16) = "Check Integrity of Installed Files"
   GET %indx, 1, IndHed
   i1 = CINT(VAL(IndHed.i1))
   i2 = CINT(VAL(IndHed.i2))
   ipos = 1                                                    ' position on menu
'
   DO
      GfxWindow %GFX_FREEZE                                    ' freeze graphic
      REDIM NucLst(i2 - i1 + 1) AS STRING                      ' dim nuclide array
      REDIM Izsort(i2 - i1 + 1), Rsort(i2 - i1 + 1)            ' dim sort arrays
      INPUT FLUSH
      list$ = ConsoleListBox(3, %CONSOLE_CENTER, 0, _          ' get user response
              "Tabulate  by:", "MIRD-07 Collection", _
              ListTab$(), ipos, %RETURN_INDEX, 0)
      IF LEN(list$) = 0 THEN
         GfxWindow %GFX_UNFREEZE                               ' unfreeze graphic
         EXIT SUB                                              ' and exit
      END IF
      GfxWindow %GFX_UNFREEZE                                  ' freeze graphic
      CALL ShowMSG(0)                                          ' show message
      ipos = VAL(list$)                                        ' item to do
      zText = $OutDir + "NucList-" + TRIM$(list$) + ".TXT"     ' file name
      OPEN zText FOR OUTPUT AS %iout                           ' open file for output
'
      SELECT CASE ipos
'
         CASE 1                                                ' by Atomic Number
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               Nukex$ = TRIM$(IndDat.nuke)
               NucLst(irec - i1 + 1) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + _
                                       USING$("##.#####", VAL(IndDat.ea)) + _
                                       USING$("##.#####", VAL(IndDat.eb)) + _
                                       USING$("##.#####", VAL(IndDat.eg))
               IZsort(irec - i1 + 1) = izmass(Nukex$)
            NEXT irec
            n = i2 - i1 + 1
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)           ' sort results by atomic number
            izold = 0
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Ordered by Atomic Number"
            PRINT #%iout, ""
            PRINT #%iout, "                                      Decay    --- Energy (MeV/nt) ---"
            PRINT #%iout, "   Z Element     Nuclide     T1/2     Mode      Alpha   Beta   Photon"
            nelement = 0                                   ' count the elements
            FOR i = 1 TO n
               iz = INSTR($sym, LEFT$(NucLst(i), 2))\2 + 1 ' get z from 1st 2 characters
               IF iz <> izold THEN                         ' print element name if new Z
                  PRINT #%iout, USING$("#### ", iz);
                  PRINT #%iout, ElName(iz) + STRING$(14 - LEN(ElName(iz)), " ");
                  INCR nelement                            ' incr element count
                  izold = iz
               ELSE
                  PRINT #%iout, STRING$(19, " ");
               END IF
               PRINT #%iout, NucLst(i)
               Nukex$ = LEFT$(NucLst(i), 7)
               iptr = ibinry(Nukex$)                        ' get NDX locations
               GET %indx, iptr, IndDat                      ' get NDX record
               IF IndDat.dau1 <> STRING$(8, " ") THEN       ' get daughter info
                  PRINT #%iout, STRING$(38, " "); IndDat.dau1; ' IndDat.bf1
                  IF IndDat.bf1 = " 1.0000E+00" THEN
                     PRINT #%iout, " 1.00*"
                  ELSE
                     PRINT #%iout, RTRIM$(IndDat.bf1) + "*"
                  END IF
                  IF IndDat.dau2 <> STRING$(8, " ") THEN
                     PRINT #%iout, STRING$(38, " "); IndDat.dau2; RTRIM$(IndDat.bf2) + "*"
                     IF IndDat.dau3 <> STRING$(8, " ") THEN
                        PRINT #%iout, STRING$(38, " "); IndDat.dau3; RTRIM$(IndDat.bf3) + "*"
                     END IF
                  END IF
               END IF
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides of ";
            PRINT #%iout, TRIM$(USING$("#####", nelement));
            PRINT #%iout, " elements."
            PRINT #%iout, "* radioactive daughter and fraction of parent decays forming daughter."
            CLOSE %iout                                      ' close the file
'
         CASE 2                                              ' list by increasing T1/2
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               Nukex$ = TRIM$(IndDat.nuke)
               NucLst(irec - i1 + 1) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + _
                                       USING$("##.#####", VAL(IndDat.ea)) + _
                                       USING$("##.#####", VAL(IndDat.eb)) + _
                                       USING$("##.#####", VAL(IndDat.eg))
               Rsort(irec - i1 + 1) = Timest(IndDat.T, IndDat.tu)
            NEXT irec
            n = i2 - i1 + 1
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez(NucLst(), Rsort(), n)               ' sort by t1/2
            PRINT #%iout," Nuclides of the MIRD-07 Collection Ordered by Half-Life"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay    --- Energy (MeV/nt) ---"
            PRINT #%iout, "  Nuclide    T1/2    Mode      Alpha   Beta   Photon "
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            CLOSE %iout
'
         CASE 3                                       ' by total emitted energy
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               Nukex$ = TRIM$(IndDat.nuke)
               NucLst(irec - i1 + 1) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode
               Rsort(irec - i1 + 1) = VAL(IndDat.ea) + VAL(IndDat.eb) + VAL(IndDat.eg)
               IF INSTR(IndDat.mode, "SF") > 0 THEN
                  iradrec = VAL(IndDat.mdec)
                  GET %irad, iradrec, DecHed
                  iradrec = iradrec + VAL(DecHed.ndec) - 1
                  GET %irad, iradrec, DecDat
                  Rsort(irec - i1 + 1) = Rsort(irec - i1 + 1) + VAL(DecDat.e) * VAL(DecDat.Freq)
                  GET %irad, , DecDat
                  Rsort(irec - i1 + 1) = Rsort(irec - i1 + 1) + VAL(DecDat.e) * VAL(DecDat.Freq)
               END IF
            NEXT irec
            n = i2 - i1 + 1
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez (NucLst(), Rsort(), n)
            PRINT #%iout," Nuclides of the MIRD-07 Collection Ordered by Emitted Energy"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay     Energy"
            PRINT #%iout, " Nuclide   T(1/2)    Mode     (Mev/nt)"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i) + USING$("##.#####", Rsort(i))
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            CLOSE %iout
'
         CASE 4                                       ' alpha emitters
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF INSTR(IndDat.mode, "A") <> 0 THEN
                  Nukex$ = TRIM$(IndDat.nuke)
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + _
                              IndDat.mode + IndDat.ea
                  IZsort(n) = izmass(Nukex$)
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Decaying by Alpha Emission"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay    Energy"
            PRINT #%iout, "  Nuclide   T1/2     Mode    (MeV/nt)"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, " Energy includes kinetic energy of alpha and recoil nucleus."
            CLOSE #%iout
'
         CASE 5                                       ' Decay by Beta- Emission
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF INSTR(IndDat.mode, "B-") <> 0 THEN
                  INCR n
                  Nukex$ = IndDat.nuke
                  IZsort(n) = izmass(Nukex$)
                  ngamma = VAL(IndDat.np10) + VAL(IndDat.npg10)
                  nbetrec = VAL(IndDat.nbet)
                  ndec = VAL(IndDat.mdec)
                  i = 0
                  Ebar = 0.0#
                  Yield = 0.0#
                  iplus = %false
                  DO
                     INCR i
                     GET %irad, ndec + ngamma + i, DecDat
                     IF DecDat.jcode = " B-" THEN
                        Ebar = Ebar + VAL(DecDat.Freq) * VAL(DecDat.E)
                        Yield = Yield + VAL(DecDat.Freq)
                     ELSE
                        iplus = %true
                     END IF
                  LOOP WHILE i < nbetrec
                  Ebar = Ebar/Yield
                  mbeta = VAL(IndDat.mbet)
                  GET %ibet, mbeta, BetHed
                  Ne = VAL(BetHed.nbet)
                  GET %ibet, mbeta + Ne, BetSpec
                  Emax = VAL(BetSpec.Eelc)
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + _
                              USING$("##.###^^^^", Yield) + _
                              USING$("##.#####", Ebar) + _
                              USING$("##.#####", Emax)
                  IF ISTRUE iplus THEN
                     NucLst(n) = NucLst(n) + "*"
                  END IF
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Decaying by Beta Minus Emission"
            PRINT #%iout, ""
            PRINT #%iout, "                                         --- Energy ---"
            PRINT #%iout, "                     Decay      Yield    Mean  End Point"
            PRINT #%iout, "  Nuclide    T1/2    Mode       (/nt)    (MeV)   (MeV)"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by beta minus emission."
            PRINT #%iout, "* Endpoint energy of composite beta minus and beta plus spectrum."
            CLOSE %iout
'
         CASE 6                                       ' Decay by Beta+ Emission
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF INSTR(IndDat.mode, "B+") <> 0 THEN
                  Nukex$ = IndDat.nuke
                  INCR n
                  IZsort(n) = izmass(Nukex$)
                  ngamma = VAL(IndDat.np10) + VAL(IndDat.npg10)
                  nbetrec = VAL(IndDat.nbet)
                  ndec = VAL(IndDat.mdec)
                  i = 0
                  Ebar = 0.0#
                  Yield = 0.0#
                  iminus = %false
                  DO
                     INCR i
                     GET %irad, ndec + ngamma + i, DecDat
                     IF DecDat.jcode = " B+" THEN
                        Ebar = Ebar + VAL(DecDat.Freq) * VAL(DecDat.E)
                        Yield = Yield + VAL(DecDat.Freq)
                     ELSE
                        iminus = %true
                     END IF
                  LOOP WHILE i < nbetrec
                  Ebar = Ebar/Yield
                  mbeta = VAL(IndDat.mbet)
                  GET %ibet, mbeta, BetHed
                  Ne = VAL(BetHed.nbet)
                  GET %ibet, mbeta + Ne, BetSpec
                  Emax = VAL(BetSpec.Eelc)
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + _
                              USING$("##.###^^^^", Yield) + _
                              USING$("##.#####", Ebar) + _
                              USING$("##.#####", Emax)
                  IF ISTRUE iminus THEN
                     NucLst(n) = NucLst(n) + "*"
                  END IF
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Decaying by Beta Plus Decay"
            PRINT #%iout, ""
            PRINT #%iout, "                                         --- Energy ---"
            PRINT #%iout, "                     Decay      Yield    Mean  End Point"
            PRINT #%iout, "  Nuclide    T1/2    Mode       (/nt)    (MeV)   (MeV)"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by beta plus emission."
            PRINT #%iout, "* Endpoint energy of composite beta minus and beta plus spectrum."
            CLOSE %iout
'
         CASE 7                                       ' Decay by Internal Transition
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF INSTR(IndDat.mode, "IT") <> 0 THEN
                  Nukex$ = IndDat.nuke
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode
                  IZsort(n) = izmass(Nukex$)
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Decaying by Internal Transition"
            PRINT #%iout, ""
            PRINT #%iout, "  Nuclide    T1/2    Decay Mode"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by internal transition."
            CLOSE %iout
'
         CASE 8                                       ' Spontaneous fission
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF INSTR(IndDat.mode, "SF") <> 0 THEN
                  Nukex$ = IndDat.nuke
                  ipt = VAL(IndDat.mneu)
                  GET %ineu, ipt, NeuHed
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + NeuHed.sfnt
                  IZsort(n) = izmass(Nukex$)
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Decaying by Spontaneous Fission"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay"
            PRINT #%iout, "  Nuclide    T1/2    Mode    Fission/nt"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by spontaneous fission."
            CLOSE %iout
'
         CASE 9                                       ' Detailed Auger-CK Spectra
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.mack) > 0 THEN
                  INCR n
                  Nukex$ = TRIM$(IndDat.nuke)
                  GET %iaug, VAL(IndDat.mack), AugHed
                  na = VAL(AugHed.naug)
                  x = 0.0 : y = 0.0
                  FOR i = 1 TO na
                     GET %iaug, , AugDat
                     x = x + VAL(AugDat.Freq)
                     y = y + 1.0E-06 * VAL(AugDat.E) * VAL(AugDat.Freq)
                  NEXT i
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + _
                              IndDat.mode + AugHed.naug + "  " +_
                              USING$("##.#####", x) + _
                              USING$("###.####", y)
                  IZsort(n) = izmass(Nukex$)
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection with a Detailed Auger-CK Spectrum"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay  Spectrum  Yield   Energy"
            PRINT #%iout, " Nuclide    T1/2     Mode       N     (/nt)  (MeV/nt)"
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides with a detailed Auger-CK spectrum."
            CLOSE %iout
'
         CASE 10                                       ' Principle Alpha Emission
            PRINT #%iout, "Nuclides of the MIRD-07 Collection Order by Energy of Principle Alpha Transition"
            PRINT #%iout, ""
            PRINT #%iout, " Nuclide    T1/2    Decay Mode  E(MeV) Yield(/nt)"
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.nalpha) <> 0 THEN
                  na = VAL(IndDat.nalpha)
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode
                  irecord = VAL(IndDat.mdec) + VAL(IndDat.np10) + VAL(IndDat.npg10) + _
                            VAL(IndDat.nbet) + VAL(IndDat.nel)
                  prime = 0#
                  FOR i = 1 TO na
                     GET %irad, irecord + i, DecDat
                     IF VAL(DecDat.freq) > prime THEN
                        prime = VAL(DecDat.freq)
                        irechold = irecord + i
                     END IF
                  NEXT i
                  GET %irad, irechold, DecDat
                  Rsort(n) = VAL(DecDat.E)
                  NucLst(n) = NucLst(n) + USING$("###.####", VAL(DecDat.E)) + DecDat.Freq
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez(NucLst(), Rsort(), n)
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "Energy is that of the alpha particle."
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by alpha emission."
            CLOSE %iout
'
         CASE 11                                      ' Principle Beta Transition
            PRINT #%iout, "Nuclides of the MIRD-07 Collection Order by Average Energy of Principle Beta Transition"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay     Energy   Yield"
            PRINT #%iout, " Nuclide    T1/2     Mode       (MeV)   (/nt)"
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.nbet) <> 0 THEN
                  na = VAL(IndDat.nbet)
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode
                  irecord = VAL(IndDat.mdec) + VAL(IndDat.np10) + VAL(IndDat.npg10)
                  prime = 0#
                  FOR i = 1 TO na
                     GET %irad, irecord + i, DecDat
                     IF VAL(DecDat.freq) > prime THEN
                        prime = VAL(DecDat.freq)
                        irechold = irecord + i
                     END IF
                  NEXT i
                  GET %irad, irechold, DecDat
                  Rsort(n) = VAL(DecDat.E)
                  NucLst(n) = NucLst(n) + USING$(" ##.####", VAL(DecDat.E)) + USING$(" ##.#####", VAL(DecDat.Freq))
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez(NucLst(), Rsort(), n)
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides that decay by beta emission."
            CLOSE %iout
'
         CASE 12                                      ' Principle Photon Transition
            PRINT #%iout, "Nuclides of the MIRD-07 Collection Order by Energy of Principle Photon"
            PRINT #%iout, ""
            PRINT #%iout, "                     Decay    Energy  Yield"
            PRINT #%iout, " Nuclide    T1/2     Mode     E(MeV)  (/nt)"
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.npg10) > 0 THEN
                  na = VAL(IndDat.npg10)
                  INCR n
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode
                  irecord = VAL(IndDat.mdec) + VAL(IndDat.np10)
                  prime = 0#
                  FOR i = 1 TO na
                     GET %irad, irecord + i, DecDat
                     IF VAL(DecDat.freq) > prime THEN
                        prime = VAL(DecDat.freq)
                        irechold = irecord + i
                     END IF
                  NEXT i
                  GET %irad, irechold, DecDat
                  Rsort(n) = VAL(DecDat.E)
                  NucLst(n) = NucLst(n) + USING$("##.####", VAL(DecDat.E)) + _
                              USING$(" ##.####", VAL(DecDat.Freq))
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez(NucLst(), Rsort(), n)
            FOR i = 1 TO n
               PRINT #%iout, "  " + NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "Restricted to photon of energy greater than 10 keV."
            PRINT #%iout, "MIRD-07 collection contains ";
            PRINT #%iout, TRIM$(USING$("#####", n));
            PRINT #%iout, " radionuclides with photon emission."
            CLOSE %iout
'
         CASE 13                                      ' Point source kerma coefficient
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.Kpts) > 0.0 THEN
                  INCR n
                  Nukex$ = TRIM$(IndDat.nuke)
                  NucLst(n) = IndDat.nuke + IndDat.t + " " + IndDat.tu + " " + IndDat.mode + _
                              " " + IndDat.Kpts
                  Rsort(n) = VAL(IndDat.Kpts)
               END IF
            NEXT irec
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE Rsort(n)
            CALL Sortez (NucLst(), Rsort(), n)
            PRINT #%iout, " Nuclides of the MIRD-07 Collection Ordered by Air Kerma-Rate Coefficient"
            PRINT #%iout, ""
            PRINT #%iout, "                       Decay    Air Kerma"
            PRINT #%iout, "   Nuclide*    T1/2    Mode    Gy m^2/(Bq s)"
            FOR i = 1 TO n
               PRINT #%iout, "    "; NucLst(i)
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            PRINT #%iout, "*Only nuclides listed with non-zero coefficient."
            CLOSE %iout
'
         CASE 14                                      ' Serial Decay Chains
            n = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               IF VAL(IndDat.iDau1) > 0 THEN
                  INCR n
                  Nukex$ = TRIM$(IndDat.nuke)
                  NucLst(n) = Nukex$
                  IZsort(n) = izmass(Nukex$)
               END IF
            NEXT i
            REDIM PRESERVE NucLst(n) AS STRING
            REDIM PRESERVE IZsort(n)
            CALL Sortem (NucLst(), IZsort(), n)
            FOR i = 1 TO n
               Nukex$ = NucLst(i)
               PRINT #%iout, Nukex$
               CALL Chain(nukex$, 2)
               PRINT #%iout, " "
            NEXT i
            PRINT #%iout, STRING$(10, "-")
            CLOSE %iout

         CASE 15                                      ' Dimensions of Database
            PRINT #%iout, "Maximum Dimensions of the MIRD-07 Collection"
            PRINT #%iout, ""
            MaxRads = 0      : MaxBetas = 0       : MaxElectrons = 0   : MaxPhotons = 0
            MaxPhotons1 = 0  : MaxPhotons2 = 0    : MaxAlphas = 0      : MaxBetaGrid = 0
            MaxAugerGrid = 0 : MaxNeutronGrid = 0 : MaxChainLength = 0 : MaxIsotopes = 0
            Nisotope = 1     : NukeCover$ = ""    : NumElements = 0
            FOR irec = i1 TO i2
               GET %indx, irec, IndDat
               Nukex$ = IndDat.Nuke
               Chx$ = LEFT$(Nukex$, 2)
               IF Chx$ = NukeCover$ THEN
                  INCR Nisotope
               ELSE
                  INCR NumElements
                  IF Nisotope > MaxIsotopes THEN
                     MaxIsotopes = Nisotope
                     MaxElement$ = NukeCover$
                  END IF
                  NukeCover$ = Chx$
                  Nisotope = 1
               END IF
               ipt = VAL(IndDat.mdec)
               GET %irad, ipt, DecHed
               n = VAL(DecHed.ndec)
               IF n > MaxRads THEN
                  MaxRads = n
                  MaxRadsNuke$ = Nukex$
               END IF
               n = VAL(IndDat.nbet)
               IF n > MaxBetas THEN
                  MaxBetas = n
                  MaxBetasNuke$ = Nukex$
               END IF
               n = VAL(IndDat.nel)
               IF n > MaxElectrons THEN
                  MaxElectrons = n
                  MaxElectronsNuke$ = Nukex$
               END IF
               n = VAL(IndDat.np10)
               IF n > MaxPhotons1 THEN
                  MaxPhotons1 = n
                  MaxPhotons1Nuke$ = Nukex$
               END IF
               n = VAL(IndDat.npg10)
               IF n > MaxPhotons2 THEN
                  MaxPhotons2 = n
                  MaxPhotons2Nuke$ = Nukex$
               END IF
               n = VAL(IndDat.npg10) + VAL(IndDat.np10)
               IF n > MaxPhotons THEN
                  MaxPhotons = n
                  MaxPhotonsNuke$ = Nukex$
               END IF
               n = VAL(IndDat.nalpha)
               IF n > MaxAlphas THEN
                  MaxAlphas = n
                  MaxAlphasNuke$ = Nukex$
               END IF
               ipt = VAL(IndDat.mbet)
               IF ipt > 0 THEN
                  GET %ibet, ipt, BetHed
                  n = VAL(BetHed.nbet)
                  IF n > MaxBetaGrid THEN
                     MaxBetaGrid = n
                     MaxBetaGridNuke$ = Nukex$
                  END IF
               END IF
               ipt = VAL(IndDat.mack)
               IF ipt > 0 THEN
                  GET %iaug, ipt, AugHed
                  n = VAL(AugHed.naug)
                  IF n > MaxAugerGrid THEN
                     MaxAugerGrid = n
                     MaxAugerGridNuke$ = Nukex$
                  END IF
               END IF
               ipt = VAL(IndDat.mneu)
               IF ipt > 0 THEN
                  GET %ineu, ipt, NeuHed
                  n = VAL(NeuHed.npts)
                  IF n > MaxNeutronGrid THEN
                     MaxNeutronGrid = n
                     MaxNeutronGridNuke$ = Nukex$
                  END IF
               END IF
               IF INSTR(IndDat.Dau1, "-") > 0 THEN
                  CALL Chain(Nukex$, 1)
                  IF nspec > MaxChainLength THEN
                     MaxChainLength = nspec
                     MaxChainLengthNuke$ = Nukex$
                  END IF
               END IF
            NEXT irec
            PRINT #%iout, " Radionuclides       -"; USING$("#####", I2-i1+1)
            PRINT #%iout, " Radiation Records   -"; USING$("#####", MaxRads); " ("; TRIM$(MaxRadsNuke$);")"
            PRINT #%iout, " Beta Transitions    -"; USING$("#####", MaxBetas); " ("; TRIM$(MaxBetasNuke$);")"
            PRINT #%iout, " Alpha Transitions   -"; USING$("#####", MaxAlphas); " ("; TRIM$(MaxAlphasNuke$);")"
            PRINT #%iout, " Photons: E < 10 keV -"; USING$("#####", MaxPhotons1); " ("; TRIM$(MaxPhotons1Nuke$);")"
            PRINT #%iout, " Photons: E > 10 keV -"; USING$("#####", MaxPhotons2); " ("; TRIM$(MaxPhotons2Nuke$);")"
            PRINT #%iout, "               Total -"; USING$("#####", MaxPhotons); " ("; TRIM$(MaxPhotonsNuke$);")"
            PRINT #%iout, " Discrete Electrons  -"; USING$("#####", MaxElectrons); " ("; TRIM$(MaxElectronsNuke$);")"
            PRINT #%iout, " "
            PRINT #%iout, " Spectral Dimensions"
            PRINT #%iout, "   Beta spectrum     -"; USING$("#####", MaxBetaGrid); " ("; TRIM$(MaxBetaGridNuke$);")"
            PRINT #%iout, "   Auger-CK spectrum -"; USING$("#####", MaxAugerGrid); " ("; TRIM$(MaxAugerGridNuke$);")"
            PRINT #%iout, "   Neutron spectrum  -"; USING$("#####", MaxNeutronGrid); " ("; TRIM$(MaxNeutronGridNuke$);")"
            PRINT #%iout, " "
            PRINT #%iout, " Decay Chain"
            PRINT #%iout, "   Chain length      -"; USING$("#####", MaxChainLength); " ("; TRIM$(MaxChainLengthNuke$);")"
            PRINT #%iout, " "
            PRINT #%iout, " Elements"
            PRINT #%iout, "   Number            -"; USING$("#####", NumElements)
            PRINT #%iout, "   Radioisotopes     -"; USING$("#####", MaxIsotopes); " ("; ElName(INSTR($sym, MaxElement$) \ 2 + 1); ")"
            PRINT #%iout, " "
            PRINT #%iout, " Data File             Record Length   # records"
            PRINT #%iout, "   MIRD-07.NDX       -"; USING$("     ####          ######", LEN(IndDat) - 2, LOF(%indx)/LEN(IndDat))
            PRINT #%iout, "   MIRD-07.RAD       -"; USING$("     ####          ######", LEN(DecDat) - 2, LOF(%irad)/LEN(DecDat))
            PRINT #%iout, "   MIRD-07.BET       -"; USING$("     ####          ######", LEN(BetSpec) - 2, LOF(%ibet)/LEN(BetSpec))
            PRINT #%iout, "   MIRD-07.ACK       -"; USING$("     ####          ######", LEN(AugDat) - 2, LOF(%iaug)/LEN(AugDat))
            PRINT #%iout, "   MIRD-07.NSF       -"; USING$("     ####          ######", LEN(NeuDat) - 2, LOF(%ineu)/LEN(NeuDat))
            PRINT #%iout, STRING$(10, "-")
            CLOSE %iout
'
         CASE 16                                      ' integrity of data files
            PRINT #%iout, "Check Integrity of MIRD-07 Data Files - Verify Checksum"
            PRINT #%iout, ""
            DIM CRC AS DWORD, Buffer AS STRING
            PRINT #%iout," Integrity of installed data files:"
            PRINT #%iout, ""
'           checking index file
            CLOSE #%indx
            OPEN $data + $DecFile FOR BINARY AS #%indx
            GET$ #%indx, LOF(#%indx), Buffer
            CLOSE #%indx
            CRC = CRC32(BYVAL STRPTR(Buffer), BYVAL LEN(Buffer))
            IF HEX$(CRC,8) = $MIRD07NDX THEN
               PRINT #%iout, " 1.  File "; $DecFile ; " is OK."
            ELSE
               PRINT #%iout, " 1.  File "; $DecFile ; " maybe corrupted - Checksum is "; HEX$(CRC,8)
            END IF
'           Checking bet file
            CLOSE #%ibet
            FileIn$ = LEFT$($DecFile, INSTR($DecFile, ".") ) + "BET"
            OPEN $data + FileIn$ FOR BINARY AS #%ibet
            GET$ #%ibet, LOF(#%ibet), Buffer
            CLOSE #%ibet
            CRC = CRC32(BYVAL STRPTR(Buffer), BYVAL LEN(Buffer))
            IF HEX$(CRC,8) = $MIRD07BET THEN
               PRINT #%iout, " 2.  File "; FileIn$ ; " is OK."
            ELSE
               PRINT #%iout, " 2.  File "; FileIn$ ; " maybe corrupted - Checksum is "; HEX$(CRC,8)
            END IF
'           checking ACK file
            CLOSE #%iaug
            FileIn$ = LEFT$($DecFile, INSTR($DecFile, ".") ) + "ACK"
            OPEN $data + FileIn$ FOR BINARY AS #%iaug
            GET$ #%iaug, LOF(#%iaug), Buffer
            CLOSE #%iaug
            CRC = CRC32(BYVAL STRPTR(Buffer), BYVAL LEN(Buffer))
            IF HEX$(CRC,8) = $MIRD07ACK THEN
               PRINT #%iout, " 3.  File "; FileIn$ ; " is OK."
            ELSE
               PRINT #%iout, " 3.  File "; FileIn$ ; " maybe corrupted - Checksum is "; HEX$(CRC,8)
            END IF
'           checking NSF file
            CLOSE #%ineu
            FileIn$ = LEFT$($DecFile, INSTR($DecFile, ".") ) + "NSF"
            OPEN $data + FileIn$ FOR BINARY AS #%ineu
            GET$ #%ineu, LOF(#%ineu), Buffer
            CLOSE #%ineu
            CRC = CRC32(BYVAL STRPTR(Buffer), BYVAL LEN(Buffer))
            IF HEX$(CRC,8) = $MIRD07NSF THEN
               PRINT #%iout, " 4.  File "; FileIn$ ; " is OK."
            ELSE
               PRINT #%iout, " 4.  File "; FileIn$ ; " maybe corrupted - Checksum is "; HEX$(CRC,8)
            END IF
'           checking RAD file
            CLOSE #%irad
            FileIn$ = LEFT$($DecFile, INSTR($DecFile, ".") ) + "RAD"
            OPEN $data + FileIn$ FOR BINARY AS #%irad
            GET$ #%irad, LOF(#%irad), Buffer
            CLOSE #%irad
            CRC = CRC32(BYVAL STRPTR(Buffer), BYVAL LEN(Buffer))
            IF HEX$(CRC,8) = $MIRD07RAD THEN
               PRINT #%iout, " 5.  File "; FileIn$ ; " is OK."
            ELSE
               PRINT #%iout, " 5.  File "; FileIn$ ; " maybe corrupted - Checksum is "; HEX$(CRC,8)
            END IF
            PRINT #%iout, STRING$(10, "-")
            CLOSE
            FileRoot$ = $data + LEFT$($DecFile, INSTR($DecFile, "."))
            OPEN FileRoot$ + "ndx" FOR RANDOM AS %indx LEN = LEN(IndDat)
            OPEN FileRoot$ + "rad" FOR RANDOM AS %irad LEN = LEN(DecDat)
            OPEN FileRoot$ + "bet" FOR RANDOM AS %ibet LEN = LEN(BetSpec)
            OPEN FileRoot$ + "ack" FOR RANDOM AS %iaug LEN = LEN(AugDat)
            OPEN FileRoot$ + "nsf" FOR RANDOM AS %ineu LEN = LEN(NeuDat)
      END SELECT
'
      CALL ShowMSG(1)                                       ' remove message
'     open the file using notepad or user's ascii editor assigned to handle TXT files
      ShellExecute BYVAL %Null, "open", zText, BYVAL %Null, BYVAL %Null, %SW_SHOWNORMAL
      INCR ipos
      IF ipos > 16 THEN ipos = 1
   LOOP
 END SUB

'----------------------------------------------------------------------------------------
 SUB Searchem
'----------------------------------------------------------------------------------------
'  routine search collection to find nuclide emitting alpha/photon of user specified
'  energy. The file is displayed by user's editor.
   LOCAL zText AS ASCIIZ * %MAX_PATH
   CURSOR ON
   CLS
   LOCATE 2, 1
   COLOR %xYellow
   PRINT CenterMess("Identify Nuclides Emitting Alpha/Photon of Specified Energy", 80)
   COLOR %xWhite + %xBright
   PRINT
   PRINT " Search results written to file SCRATCH.TXT in OUTPUT folder."
   Quest$ = " Radiation [a]lpha or [p]hoton radiations or [e]xit search"
   LOCATE 6, 1
   resp$ = ResABC(Quest$, "a", "p", "e", "a")
   IF resp$ = "e" THEN
      CLS
      LOCATE 34, 1                               ' note this is the last
      COLOR %xBlack, %xWhite                     ' line of the screen
      PRINT $fline1;                             ' hence the ; at the end of
      COLOR %xYellow, %xBlue                     ' the print statement
      LOCATE 31, 2
      PRINT "Click on an element to list its radioisotopes."
      PRINT " Press <Esc> to exit RADTABS.";
      EXIT SUB
   END IF
   OPEN "OUTPUT\Scratch.TXT" FOR OUTPUT AS %iout
   PRINT #%iout, "Identify Nuclide in MIRD-07 Collection with Observed Emission"
   PRINT #%iout, ""
   IF resp$ = "a" THEN
      PRINT " List nuclides emitting an alpha of energy E +/- dE."
      LINE INPUT " Enter alpha energy E (MeV) -> "; Ls$
      Ex = VAL(Ls$)
      LINE INPUT " Enter delta on alpha energy dE -> "; Ls$
      dE = VAL(Ls$)
      Elow = Ex - dE
      Ehigh = Ex + dE
      CLS
      LOCATE 2,1
      COLOR %xYellow
      PRINT CenterMess("Identify Nuclides Emitting Alpha/Photon of Specified Energy", 80)
      COLOR %xWhite + %xBright
      PRINT
      PRINT " Alpha emitters of energy between"; USING$("##.#### and##.#### MeV", Elow, Ehigh)
      PRINT #%iout, " Alpha emitters of energy"; USING$("##.#### +/-##.#### MeV", Ex, dE)
      GET %indx, 1, IndHed
      istart = VAL(IndHed.i1)
      ifinal = VAL(IndHed.i2)
      FOR i = istart TO ifinal
         GET %indx, i, IndDat
         IF VAL(IndDat.nalpha) > 0 THEN
            nrecord = VAL(IndDat.nalpha)
            nukex$ = IndDat.nuke
            ialpha = VAL(IndDat.np10) + VAL(IndDat.npg10) + _
                     VAL(IndDat.nbet) + VAL(IndDat.nel) + VAL(IndDat.mdec)
            FOR j = 1 TO nrecord
               GET %irad, ialpha + j, DecDat
               Ea = VAL(DecDat.E)
               IF Ea > Elow AND Ea < Ehigh THEN
                  PRINT TAB(2) Nukex$; USING$(" E = ##.#### Yield (/nt) ", VAL(DecDat.E));
                  PRINT #%iout, TAB(5) Nukex$; USING$(" E = ##.#### Yield (/nt) ", VAL(DecDat.E));
                  IF VAL(DecDat.Freq) < 0.0001 THEN
                     PRINT "<  0.0001"
                     PRINT #%iout, "<  0.0001"
                  ELSE
                     PRINT USING$("= ##.####", VAL(DecDat.Freq))
                     PRINT #%iout, USING$("= ##.####", VAL(DecDat.Freq))
                  END IF
               END IF
               IF Ea > Ehigh THEN EXIT FOR
            NEXT j
         END IF
      NEXT i
   ELSEIF resp$ = "p" THEN
      PRINT " List nuclides emitting a photon of energy E +/- dE."
      LINE INPUT " Enter photon energy E (MeV) -> "; Ls$
      Ex = VAL(Ls$)
      LINE INPUT " Enter delta on photon energy dE -> "; Ls$
      dE = VAL(Ls$)
      Elow = Ex - dE
      Ehigh = Ex + dE
      CLS
      LOCATE 2,1
      COLOR %xYellow
      PRINT CenterMess("Identify Nuclides Emitting Alpha/Photon of Specified Energy", 80)
      COLOR %xWhite + %xBright
      PRINT
      PRINT " Photon emitters of energy between"; USING$("##.#### and##.#### MeV", Elow, Ehigh)
      PRINT #%iout, " Photon emitters of energy"; USING$("##.#### +/-##.#### MeV", Ex, dE)
      GET %indx, 1, IndHed
      istart = VAL(IndHed.i1)
      ifinal = VAL(IndHed.i2)
      FOR i = istart TO ifinal
         GET %indx, i, IndDat
         nrecord = VAL(IndDat.np10) + VAL(IndDat.npg10)
         IF nrecord > 0 THEN
            nukex$ = IndDat.nuke
            istart = VAL(IndDat.mdec)
            FOR j = 1 TO nrecord
               GET %irad, istart + j, DecDat
               Ea = VAL(DecDat.E)
               IF Ea > Elow AND Ea < Ehigh THEN
                  PRINT TAB(2) Nukex$; USING$(" E = ##.#### Yield (/nt) ", VAL(DecDat.E));
                  PRINT #%iout, TAB(5) Nukex$; USING$(" E = ##.#### Yield (/nt) ", VAL(DecDat.E));
                  IF VAL(DecDat.Freq) < 0.0001 THEN
                     PRINT "<  0.0001"
                     PRINT #%iout, "<  0.0001"
                  ELSE
                     PRINT USING$("= ##.####", VAL(DecDat.Freq))
                     PRINT #%iout, USING$("= ##.####", VAL(DecDat.Freq))
                  END IF
               END IF
               IF Ea > Ehigh THEN EXIT FOR
            NEXT j
         END IF
      NEXT i
   END IF
   PRINT #%iout, "The search is complete."
   CLOSE %iout
   PRINT " The search is complete."
   PRINT
   zText = "output\scratch.txt"
   ShellExecute BYVAL %Null, "open", zText, BYVAL %Null, BYVAL %Null, %SW_SHOWNORMAL
   PRINT
   COLOR %xYellow
   PRINT $Prompt;
   INPUT FLUSH
   WAITKEY$
   CLS
   LOCATE 34, 1                               ' note this is the last
   COLOR %xBlack, %xWhite                     ' line of the screen
   PRINT $fline1;                             ' hence the ; at the end of
   COLOR %xYellow, %xBlue                     ' the print statement
   LOCATE 31, 2
   PRINT "Click on an element to list its radioisotopes."
   PRINT " Press <Esc> to exit RADTABS.";
 END SUB

'----------------------------------------------------------------------------------------
 SUB Helpem
'----------------------------------------------------------------------------------------
'  help displays documents in the Report folder. Note used applications assigned to
'  open TXT and PDF files.
   LOCAL zText AS ASCIIZ * %MAX_PATH
   DIM sInputFile$(1 TO 20)
   CALL ShowMSG(0)
   Filex$ = UCASE$(DIR$($Report + "*.*"))
   nfile = 1
   iord = 0
   IF LEN(Filex$) > 0 THEN
      sInputFile$(nfile) = UCASE$(Filex$)
      DO
         INCR nfile
         IF nfile > 20 THEN EXIT DO
         Filex$ = UCASE$(DIR$)
         IF Filex$ = "USER GUIDE.PDF" THEN iord = nfile
         IF LEN(Filex$) = 0 THEN
            sInputFile$(nfile) = ""
            EXIT DO
         END IF
         sInputFile$(nfile) = Filex$
      LOOP
      DECR nfile
   END IF
'  Order the files so the guide is a top of the menu.
   IF iord <> 0 THEN
      SWAP sInputFile$(1), sInputFile$(iord)
   END IF
   iord = 0
   FOR i = 1 TO nfile
      IF sInputFile$(i) = "MIRD-07 LIST.TXT" THEN iord = i
   NEXT i
   IF iord <> 0 THEN
      SWAP sInputFile$(2), sInputFile$(iord)
   END IF
   iord = 0
   FOR i = 1 TO nfile
      IF sInputFile$(i) = "AUGER LIST.TXT" THEN iord = i
   NEXT i
   IF iord <> 0 THEN
      SWAP sInputFile$(3), sInputFile$(iord)
   END IF
'
   LOCATE 2, 1
   COLOR %xYellow
   ipoint = 1
   SLEEP 600
   CALL ShowMSG(1)
   DO
      GfxWindow %GFX_FREEZE                               ' freeze graphic
      ist$ = ConsoleListBox(1, %CONSOLE_CENTER, 0, _
             "Select File or Escape to Quit", "Files in REPORT Folder ", _
             sInputFile$(), ipoint, %RETURN_INDEX, 0)
      GfxWindow %GFX_UNFREEZE                             ' unfreeze graphic
      IF LEN(ist$) = 0 THEN EXIT DO
      ipoint = VAL(ist$)
      zText = "report\" + sInputFile$(ipoint)             ' selected report
'     PDF and TXT file will be open by user's assigned application
      ShellExecute BYVAL %Null, "open", zText, BYVAL %Null, BYVAL %Null, %SW_SHOWNORMAL
      INCR ipoint
      IF ipoint > nfile THEN ipoint = 1
   LOOP
 END SUB

'----------------------------------------------------------------------------------------
 SUB Aboutem
'----------------------------------------------------------------------------------------
'  usual software about screen
   LOCAL Nfile&, hgw1&
   LOCAL nWidth&, nHeight&, w&, h&, ncWidth&, ncHeight&, x&, y&
   LOCAL BitName$
   CLS                                                    ' clear screen
   LOCATE 3, 1
   COLOR %xYellow
   PRINT CenterMess("About " + $Code, 79)
   PRINT CenterMess($Version, 80)
   COLOR %xWhite + %xBright
   LOCATE 6, 1
   PRINT TAB(3) "The " + $code + " software provides access to the unabridged nuclear decay
   PRINT TAB(3) "data assembled during the preparation of the MIRD monograph entitled"
   PRINT TAB(3) "'Radionuclide Data and Decay Schemes'. The software and its associated"
   PRINT TAB(3) "data files can be used directly in the calculation of radiation dose."
   PRINT
   PRINT TAB(3) "This work was carried out by K.F. Eckerman at Oak Ridge National Laboratory"
   PRINT TAB(3) "(ORNL) and A. Endo at Japan Atomic Energy Agency (JAEA) under an agreement"
   PRINT TAB(3) "of cooperation between JAEA and the US Environmental Protection Agency."
   PRINT
   PRINT TAB(3) "The software was developed for Windows 98/2000/XP/Vista operating systems. "
   PRINT TAB(3) "Portions are copyright by Perfect Sync, Inc. Problems should be reported to"
   PRINT TAB(3) "K. Eckerman (kfe@ornl.gov) or A. Endo (endo.akira3@jaea.go.jp)."
   PRINT
   PRINT TAB(3) "The friendship bell, next screen, in the home city of K. Eckerman was"
   PRINT TAB(3) "manufactured in the home city of A. Endo."
   PRINT
   PRINT TAB(3) "'This bronze bell was designed in Oak Ridge and cast in Japan in 1993 to"
   PRINT TAB(3) "serve as a symbol of the bonds of friendship and mutual regard that have"
   PRINT TAB(3) "developed between Oak Ridge and Japan over the past fifty years ... This"
   PRINT TAB(3) "bell further serves as a symbol of our mutal longing and pledge to work "
   PRINT TAB(3) "for freedom, well-being, justice and peace for all the people of the world"
   PRINT TAB(3) "in the years to come. (from the bell dedication).'"
   CURSOR ON
   LOCATE 31, 1
   COLOR %xYellow
   PRINT $Prompt;
   INPUT FLUSH
   WAITKEY$
   CURSOR OFF
   DIM szBM AS ASCIIZ * 5                                    ' show the freedom bell
   szBM = "bell"
   IF ISTRUE GetResourceBitmapSize (szBm, nWidth, nHeight) THEN
'     print "BitMap "; szBM; " is "; nWidth; " pixels wide x "; nHeight; " pixels High"
   ELSE
      PRINT "Bitmap resource "; szBM; " not found or corrupt"
      WAITKEY$
      EXIT SUB                                               ' just exit sub if error
   END IF
   DESKTOP GET CLIENT TO ncWidth&, ncHeight&
   ix& = (ncWidth& - nWidth&)\2                              ' center graphic on desktop
   iy& = (ncHeight& - nHeight&)\2                            '
   GRAPHIC WINDOW "Oak Ridge Friendship Bell", ix&, iy&, nWidth&, nHeight& TO hGW1&
   GRAPHIC ATTACH hGW1&, 0, REDRAW
   GRAPHIC RENDER "bell", (0, 0) - (nWidth&-1, nHeight&-1)   ' within the graphic window
   GRAPHIC REDRAW
   GRAPHIC SET FOCUS
   SLEEP 2500
   GRAPHIC WINDOW END                                        ' Close bitmap and put
   CONSOLE SET FOCUS                                         ' the focus on console
   CLS
   LOCATE 34, 1                                              ' note this is the last
   COLOR %xBlack, %xWhite                                    ' line of the screen
   PRINT $fline1;                                            ' hence the ; at the end
   COLOR %xYellow, %xBlue                                    ' of print statement
   LOCATE 31, 2
   PRINT "Click on an element to list its radioisotopes."
   PRINT " Press <Esc> to exit RADTABS.";
 END SUB

'----------------------------------------------------------------------------------------
 SUB ShowMSG(iaction)
'----------------------------------------------------------------------------------------
'  routine to display the bitmap 'message' in the resource file. called with iaction = 0
'  displays the bitmap, called with iaction <> 0 removes the graphic window. the values
'  of nWidth and nHeight should be redefined if the 'patient.bmp' is changed. the
'  following code fragment is used to get these data from the resource file:
'  DIM szBM AS ASCIIZ * 8
'  szBM = "MESSAGE"
'  IF ISTRUE GetResourceBitmapSize (szBm, nWidth, nHEight) THEN
'     print "BitMap "; szBM; " is "; nWidth; " pixels wide x "; nHeight; " pixels High"
'  ELSE
'     Print "Bitmap resource "; szBM; " not found or corrupt"
'  END IF
   IF iaction = 0 THEN                                        ' display message
      nWidth& = 342 : nHeight& = 67
      DESKTOP GET CLIENT TO ncWidth&, ncHeight&
      ix& = (ncWidth& - nWidth&)\2                            ' center graphic on desktop
      iy& = (ncHeight& - nHeight&)\2                          '
      GRAPHIC WINDOW "", ix&, iy&, nWidth&, nHeight& TO hGW1& '
      GRAPHIC ATTACH hGW1&, 0, REDRAW                         '  within the graphic window
      GRAPHIC RENDER "message", (0, 0) - (nWidth&-1, nHeight&-1)
      GRAPHIC REDRAW
      GRAPHIC SET FOCUS
   ELSE
      GRAPHIC WINDOW END                                      ' Close graphic bitmap and
      CONSOLE SET FOCUS                                       ' put focus on console
   END IF
 END SUB

'========================================================================================
' Decay chain collection of routines
'========================================================================================
 SUB Chain(nuke$, icall)
'----------------------------------------------------------------------------------------
'  chain and its subroutines may result in the global IndDat not be that of the parent.
'  the parent's record in the NDX file should be re-established to ensure IndDat contains
'  the parent data.
'
   IF icall = 0 THEN                                      ' icall <> 0 when called from
      CLS                                                 ' TableGen (case 12)
      LOCATE 2, 1
      COLOR %xYellow
   END IF
'
   DIM eat(1 TO %mspec), ebt(1 TO %mspec), egt(1 TO %mspec)
   zln2## = 0.693147181##
   zero = 0.0#
   ibrch = 0                                              ' initialize parameters
   ipar = 1
   nspec = 1
   ieob = %true                                           ' end of branch
   nucnam(1) = nuke$                                      ' members of chain
   FOR i = 1 TO %mspec
      FOR j = 1 TO %mspec
         branch(i, j) = zero
      NEXT j
   NEXT i
   branch(1, 1) = 1.0#                                    ' activities on diagonal
   zlmr(1) = 0.0##                                        ' 1 unit of parent/others zero
   IF imax < 0 THEN EXIT SUB                              ' only initialize chain parameters.
   ipob = %false                                          ' check for a branch in chain
'
   DO                                                     ' this loop build the nucnam()
      CALL Frward(ipob)                                   ' array - Frward reads down a
      CALL Recver                                         ' chain Recver directs read
   LOOP WHILE ISFALSE ieob                                ' of branches - end of branches
'
'  check if SF and move it to last position in nucnam()
   ispon = 0
   FOR i = 1 TO nspec
      IF INSTR(nucnam(i), "SF") > 0 THEN
         ispon = i
         EXIT FOR
      END IF
   NEXT i
   IF ispon > 0 THEN
      FOR i = ispon TO nspec - 1
         nucnam(i) = nucnam(i+1)
      NEXT i
      nucnam(nspec) = "SF"
   END IF
'
   CALL BldChain                                          ' get chain parameters and call
   IF ISTRUE ipob THEN CALL Order                         ' Order if any branch existed
'
   FOR i = 1 TO nspec
      IF INSTR(nucnam(i), "SF") <> 0 THEN
         zlmr(i) = 0.0##
      ELSE
         zlmr(i) = zln2## / timest(thalf(i), iu(i))
      END IF
   NEXT i
'
   IF icall = 1 THEN EXIT SUB                             ' exit if called by TableGen (case 12)
'
   IF nspec >  1  THEN
      text$ = TRIM$(nucnam(1)) + " Decay Chain:" + _
              " Half-lives and Branching Fractions"
   ELSE
      text$ = TRIM$(nucnam(1)) + " Decay Chain:" + _
              " No radioactive daughters."
   END IF
   IF icall = 0 THEN
      PRINT CenterMess(Text$, 80)
      COLOR %xWhite + %xBright
   END IF
'
   CALL Printm (icall)
   IF icall > 0 THEN EXIT SUB
'
   CALL Pathx                                          '
'
   timess = 3.1558E+09                                      ' 100 y in seconds
   text$ = ": Activity, Transformations, and Cumulative Energies " +_
           "(MeV) at 100y"
   IF nspec > 7 THEN
      PRINT
      COLOR %xYellow
      PRINT CenterMess(TRIM$(NucNam(1)) + Text$,80)
      COLOR %xWhite + %xBright
   ELSE
      PRINT
      COLOR %xYellow
      PRINT CenterMess(TRIM$(nucnam(1))+ Text$, 80)
      COLOR %xWhite + %xBright
   END IF
   PRINT "    Nuclide     T1/2    A(t)/Ao   nt/Ao(s)  Ealpha" +_
         "   Electron   Ephoton"
   ea = zero : eb = zero :  eg = zero
   REDIM eat(1 TO nspec)
   REDIM ebt(1 TO nspec)
   REDIM egt(1 TO nspec)
   FOR ispec = 1 TO nspec
      IF INSTR(nucnam(ispec), "SF") = 0 THEN
         CALL Birch(rx1, rx2, timess, ispec)
         ea = ea + rx2 * ealpha(ispec)
         eb = eb + rx2 * ebeta(ispec)
         eg = eg + rx2 * egamm(ispec)
         PRINT USING$("###",ispec) + " " + nucnam(ispec)+ _
                      " " + thalf(ispec) + iu(ispec) +_
               USING$("##.###^^^^",rx1) + USING$("##.###^^^^",rx2) + _
               USING$("##.###^^^^",ea) + USING$("##.###^^^^",eb) + _
               USING$("##.###^^^^",eg)
         eat(ispec) = ea : ebt(ispec) = eb : egt(ispec) = eg
      END IF
   NEXT ispec
'
   IF nspec > 1 AND INSTR(nucnam(nspec), "SF") = 0 THEN
      IF LEFT$(nucnam(nspec), 2) = "SF" THEN
         imax = icutoff(eat(), ebt(), egt(), nspec-1)
      ELSE
         imax = icutoff(eat(), ebt(), egt(), nspec)
      END IF
      PRINT
      IF imax = nspec THEN
         PRINT " In dosimetric calculation the full chain should be considered."
      ELSE
         PRINT " In dosimetric calculations the " + TRIM$(nucnam(1)) +_
               " chain can be truncated at member" + STR$(imax) + "."
      END IF
   END IF
   LOCATE 30
   COLOR %xYellow
   PRINT $Prompt;
   INPUT FLUSH
   WAITKEY$
   SLEEP 100
 END SUB

'------------------------------------------------------------------------------
 SUB Frward(ipob)
'------------------------------------------------------------------------------
'  routine to read down a serial chain to build the nucnam() array - branches
'  are detect as the chain is read - Recver will recover the branches, last
'  last in, first out, and direct frward reading of new chain sequence
   DIM nukex AS LOCAL STRING * 7
   IF ipar = 1 THEN                                    ' dealing with parent
      nuke$ = nucnam(ipar)                             ' extract name and
      ipt = ibinry(nuke$)                              ' get its pointer
      IF ipt = 0 THEN                                  ' we do not need this
        nspec = 0                                      ' test but ...
        EXIT SUB                                       ' exit if trouble
      END IF
   END IF
   DO                                                  ' now start down the
      IF ipt < 9999 THEN                               ' chain adding the
         GET %indx, ipt, IndDat                        ' new members to
         nukex = IndDat.nuke                           ' the array
         CALL Bldvect(nucnam(), Nukex, nspec)          ' nucnam() and
         id1 = CLNG(VAL(IndDat.idau1))                 ' following the first
         IF id1 > 0 THEN                               ' daughter by adding
            nukex = IndDat.Dau1                        ' it to nucnam() and
            CALL Bldvect(nucnam(), nukex, nspec)       ' noted an branches
            id2 = CLNG(VAL(IndDat.idau2))
            IF id2 > 0 THEN
               ieob = %false
               ipob = %true
               INCR ibrch
               nukex = IndDat.Dau2                     ' add 2nd daughter
               named(ibrch) = nukex                    ' to named()
               iptb(ibrch) = VAL(IndDat.idau2)         ' store its NDX pointer
               CALL Bldvect(nucnam(), nukex, nspec)    ' add to nucnam()
               id3  = CLNG(VAL(IndDat.idau3))          '
               IF id3 > 0 THEN                         ' process 3rd daughter
                  nukex = IndDat.Dau3
                  INCR ibrch
                  ieob = %false
                  named(ibrch) = nukex
                  iptb(ibrch) = VAL(IndDat.idau3)
                  CALL Bldvect(nucnam(), nukex, nspec)' add to nucnam()
               END IF
            END IF
         END IF
      END IF                                           ' member processed
      ipt = id1                                        ' now treat 1st daughter
   LOOP WHILE id1 > 0                                  ' loop till end of
 END SUB                                               ' this sequence

'------------------------------------------------------------------------------
  SUB Recver
'------------------------------------------------------------------------------
'  routine recovers info on branch point in chain that were detected by
'  frward and direct reading of down the new branch.
'
   IF ibrch = 0 THEN                               ' no branches to treat
      ieob = %true                                 ' set end of branch and
      EXIT SUB                                     ' exit sub
   END IF
'
   WHILE ibrch > 0                                 ' loop over branches
'
      ipar = iparb(ibrch)                          ' index of branch point
      ipt = iptb(ibrch)                            ' NDX pointer
      nuke$ = named(ibrch)                         ' nuclide. now check
      FOR i = 1 TO nspec                           ' if in nucnam(i)
         IF INSTR(nucnam(i), nuke$) > 0 THEN       ' member in nucnam
            DECR ibrch                             ' branch converged
            IF ibrch = 0 THEN                      ' decr ibrch and exit.
               ieob = %true                      '=======
               EXIT SUB                            ' return here for
            END IF                                 ' next branch
         END IF
      NEXT i
      EXIT SUB
   WEND
 END SUB

'------------------------------------------------------------------------------
 SUB BldChain
'------------------------------------------------------------------------------
'  routine reads the data for the chain members (nucnam() array) and gets the
'  branching fractions, etc.
   DIM daughter AS LOCAL STRING * 7
   FOR i = 1 TO nspec
      Nuke$ = nucnam(i)
      IF INSTR(Nuke$, "SF") > 0 THEN EXIT FOR             ' note SF is last member
      iptr = ibinry(nuke$)
      GET %indx, iptr, IndDat
      thalf(i) = IndDat.t
      iu(i) = IndDat.Tu
      ealpha(i) = VAL(IndDat.ea)
      ebeta(i) = VAL(IndDat.eb)
      egamm(i) = VAL(IndDat.eg)
      IF CLNG(VAL(IndDat.idau1)) > 0 THEN
         Daughter = IndDat.Dau1
         ip = invect(nucnam(), Daughter, nspec)
         branch(i, ip) = VAL(IndDat.bf1)
         IF CLNG(VAL(IndDat.idau2)) > 0 THEN
            Daughter = IndDat.Dau2
            ip = invect(nucnam(), Daughter, nspec)
            branch(i, ip) = VAL(IndDat.bf2)
            IF CLNG(VAL(IndDat.idau3)) > 0 THEN
               Daughter = IndDat.Dau3
               ip = invect(nucnam(), Daughter, nspec)
               branch(i, ip) = VAL(IndDat.bf3)
            END IF
         END IF
      END IF
   NEXT i
 END SUB

'------------------------------------------------------------------------------
 SUB Order
'------------------------------------------------------------------------------
'  routine order the chain members so daughter index > parent
'  routine has to be called if a branch had occurred
'
   DIM rsave(1 TO %mspec), csave(1 TO %mspec)
   ipass = 0
   DO
      imove = 0                                  ' number to move
      INCR ipass                                 ' ipass + 1
      IF ipass > 4 * nspec THEN
         PRINT "Failure in order: greater than" + STR$(ipass) +_
         " passes for " + TRIM$(nucnam(1)) + "."
         INPUT FLUSH
         WAITKEY$
         EXIT SUB
      END IF
'
      FOR i = 1 TO nspec
         FOR j = 1 TO i-1
            IF branch(i, j) > 0.0 THEN
               ip = i
               jp = j
               imove = 1
               nuke$ = nucnam(ip)
               thold$ = thalf(ip)
               ea = ealpha(ip)
               eb = ebeta(ip)
               eg = egamm(ip)
               ix$ = iu(ip)
               FOR j = 1 TO nspec
                  rsave(j) = branch(ip, j)
               NEXT j
               FOR i = ip - 1 TO jp STEP -1
                  nucnam(i + 1) = nucnam(i)
                  thalf(i + 1) = thalf(i)
                  ealpha(i + 1) = ealpha(i)
                  ebeta(i + 1) = ebeta(i)
                  egamm(i + 1) = egamm(i)
                  iu(i + 1) = iu(i)
                  FOR j = 1 TO nspec
                     branch(i + 1, j) = branch(i, j)
                  NEXT j
               NEXT i
               nucnam(jp) = nuke$
               thalf(jp) = thold$
               iu(jp) = ix$
               ealpha(jp) = ea
               ebeta(jp) = eb
               egamm(jp) = eg
               FOR j = 1 TO nspec
                  branch(jp, j) = rsave(j)
               NEXT j
               FOR i = 1 TO nspec
                  csave(i) = branch(i, ip)
               NEXT i
               FOR j = ip - 1 TO jp STEP -1
                  FOR i = 1 TO nspec
                     branch(i, j + 1) = branch(i, j)
                  NEXT i
               NEXT j
               FOR i = 1 TO nspec
                  branch(i, jp) = csave(i)
               NEXT i
            END IF
         NEXT j
      NEXT i
      IF imove = 0 THEN EXIT LOOP                    ' nothing to move
   LOOP
 END SUB

'----------------------------------------------------------------------------------------
 SUB Printm (icall)
'----------------------------------------------------------------------------------------
'  routine prints the serial decay chain
   IF nspec = 1 THEN
      PRINT
      PRINT "   Nuclide  Halflife"
      COLOR %xWhite + %xBright
      PRINT USING$("##",nspec) + " " + nucnam(nspec) + " " +_
            thalf(nspec) + iu(nspec)
   ELSE
      IF icall > 0 THEN
         PRINT #%iout, STRING$(23, " ") + STRING$(16, "-") + "  Daughter Products  " + STRING$(16, "-")
         PRINT #%iout, "    Nuclide  Halflife    f1" + STRING$(6," ") + _
               "Nuclide   f2" + STRING$(6, " ") + _
               "Nuclide   f3" + STRING$(6," ") + "Nuclide"
         FOR i = 1 TO nspec
            IF INSTR(nucnam(i), "SF") = 0 THEN
              PRINT #%iout, USING$(" ##",i) + " " + nucnam(i)+" " + thalf(i) + iu(i);
              FOR j = 1 TO nspec
                IF i <> j AND branch(i, j) > 0.0# THEN
                   PRINT #%iout, USING$("##.###^^^^", branch(i,j)) + " " + nucnam(j);
                END IF
              NEXT j
              PRINT #%iout, " "
            END IF
         NEXT i
      ELSE
         PRINT
         PRINT STRING$(23, " ") + STRING$(16, "-") + "  Daughter Products  " + STRING$(16, "-")
         PRINT "    Nuclide  Halflife    f1" + STRING$(6," ") + _
               "Nuclide   f2" + STRING$(6, " ") + _
               "Nuclide   f3" + STRING$(6," ") + "Nuclide"
         FOR i = 1 TO nspec
            IF INSTR(nucnam(i), "SF") = 0 THEN
              PRINT USING$(" ##",i) + " " + nucnam(i)+" " + thalf(i) + iu(i);
              FOR j = 1 TO nspec
                IF i <> j AND branch(i, j) > 0.0# THEN
                   PRINT USING$("##.###^^^^", branch(i,j)) + " " + nucnam(j);
                END IF
              NEXT j
              PRINT
            END IF
         NEXT i
      END IF
      IF icall > 0 THEN EXIT SUB
      IF nspec >  7 THEN
         PRINT
         COLOR %xYellow
         LOCATE 30
         PRINT $Prompt;
         INPUT FLUSH
         WAITKEY$
         SLEEP 100
         CLS
      END IF
   END IF
'
 END SUB

'----------------------------------------------------------------------------------------
 SUB Pathx
'----------------------------------------------------------------------------------------
'  trace out the different pathways.
'  adapted from A. Birchall, Health Phys. 50, 3, 389-397, 1986.
'
   FOR i = 1 TO nspec
      maxi(i) = 0
      FOR j = 1 TO nspec
         mpath(i,j) = 0
      NEXT j
   NEXT i
   FOR j = 2 TO nspec
      FOR i = 1 TO j - 1
         IF branch(i, j) <> 0.# THEN
            INCR maxi(j)
            mpath(maxi(j), j) = i
         END IF
      NEXT i
   NEXT j
 END SUB

'----------------------------------------------------------------------------------------
 SUB Birch(x1, x2, t, imem)
'----------------------------------------------------------------------------------------
'  routine traces out a serial chain
'  adapted from A. Birchall, Health Phys. 50, 3, 389-397, 1986.
   DIM b(1 TO %mspec), b0(1 TO %mspec), zkt##(1 TO %mspec)
   DIM zk##(1 TO %mspec), mark(1 TO %mspec), jpath(1 TO %mspec)
   DIM ipath(1 TO %mspec)
'
'  trace the pathway backwards from imem to decide which elements
'  of the Mpath matrix to choose.
'
   x1 = 0.# : x2 = 0.#
   FOR i = 1 TO nspec
      mark(i) = 1
      b(i) = branch(i, i)
   NEXT i
31:
   nmem = 1
   jpath(1) = imem
   IF maxi(imem) <> 0 THEN
      DO
         imem = mpath(mark(imem), imem)
         INCR nmem
         jpath(nmem) = imem
      LOOP WHILE maxi(imem) > 0
   END IF
'
   FOR i = 1 TO nmem
      ipath(i) = jpath(nmem - i + 1)
   NEXT i
   imem = ipath(nmem)
   FOR i = 1 TO nmem
      b0(i) = b(ipath(i))
      zkt##(i) = zlmr(ipath(i))
      IF i <  nmem THEN
         zk##(i) =  CEXT(branch(ipath(i), ipath(i + 1)) * zkt##(i))
      ELSE
         zk##(i) = zkt##(i)
      END IF
   NEXT i
   CALL Batman(b0(), zk##(), zkt##(), an1, an2, t, nmem)
   x1 = x1 + an1
   x2 = x2 + an2
   FOR i = 1 TO nmem
      b(ipath(i)) = 0.0#
      IF i > 1 THEN
         IF mark(ipath(i)) <> maxi(ipath(i)) THEN
            m = ipath(i)
            INCR mark(m)
            FOR j = 1 TO m - 1
               mark(j) = 1
               b(j) = branch(j, j)
            NEXT j
            GOTO 31
         END IF
      END IF
   NEXT i
   imem = ipath(nmem)
 END SUB

'----------------------------------------------------------------------------------------
 SUB Batman(b0(), zk##(), zkt##(), an1, an2, t, n)
'----------------------------------------------------------------------------------------
'  routine solves bateman equation returning activity (an1) and time integrated
'  activity an2 over time t
   DIM s1 AS LOCAL EXT
   DIM s2 AS LOCAL EXT
   DIM ss1 AS LOCAL EXT
   DIM ss2 AS LOCAL EXT
   DIM prod AS LOCAL EXT
   DIM ann1 AS LOCAL EXT
   DIM ann2 AS LOCAL EXT
   DIM tt AS LOCAL EXT
'
   an1 = 0# : an2 = 0# : ann1 = 0## : ann2 = 0## : tt = CEXT(t)
   FOR i = 1 TO n
     IF b0(i) <> 0# THEN
        s1 = 0## : s2 = 0## : ss1 = 0## : ss2 = 0##
        FOR j = i TO n
           prod = zkt##(n) / zk##(n) * zk##(j) / zkt##(i)
           FOR k = i TO n
              IF k <> j THEN prod = prod * zk##(k) / (zkt##(k) - zkt##(j))
           NEXT k
           IF prod < 0## THEN
              s1 = s1 + ABS(prod) * expfun(-zkt##(j) * tt)
              ss1 = ss1 + ABS(prod) * expf1(zkt##(j), t)
           ELSE
              s2 = s2 + prod * expfun(-zkt##(j) * tt)
              ss2 = ss2 + prod * expf1(zkt##(j), t)
           END IF
        NEXT j
'       only positive values are retained; negatives are zero
        IF s2 > s1 THEN  ann1 = ann1 + CEXT(b0(i)) * (s2 - s1)
        IF ss2 > ss1 THEN ann2 = ann2 + CEXT(b0(i)) * (ss2 - ss1)
     END IF
   NEXT i
   an1 = ann1
   an2 = ann2
 END SUB
'----------------------------------------------------------------------------------------
' End of chain routines
'========================================================================================

'----------------------------------------------------------------------------------------
 SUB Sortem (a$(), ii(), n)
'----------------------------------------------------------------------------------------
' routine sorts a$() based on integer array ii()
   DO
      iOutOfOrder = %False
      FOR i = 1 TO n - 1
         IF ii(i) > ii(i+1) THEN
             SWAP a$(i+1), a$(i)
             SWAP ii(i+1), ii(i)
             iOutOfOrder = %True
         END IF
      NEXT i
   LOOP WHILE ISTRUE iOutOfOrder
 END SUB

'----------------------------------------------------------------------------------------
 SUB Sortez (a$(), r(), n)
'----------------------------------------------------------------------------------------
'  routine sorts a$() based on real sort array r().
   DO
      iOutOfOrder = %False
      FOR i = 1 TO n - 1
         IF r(i) > r(i+1) THEN
             SWAP a$(i+1), a$(i)
             SWAP r(i+1), r(i)
             iOutOfOrder = %True
         END IF
      NEXT i
   LOOP WHILE ISTRUE iOutOfOrder
 END SUB

'----------------------------------------------------------------------------------------
 SUB GetNukeLst(NukeCover$)
'----------------------------------------------------------------------------------------
'  routine assemble string of the chemical sysmbols for nuclides in the collection
   GET %indx, 1, IndHed
   i1 = CINT(VAL(IndHed.i1))
   i2 = CINT(VAL(IndHed.i2))
   NukeCover$ = ""
   FOR irec = i1 TO i2                                    ' read NDX record
      GET %indx, irec, IndDat
      Chx$ = LEFT$(IndDat.nuke, 2)
      IF INSTR(NukeCover$, Chx$) = 0 THEN                 ' check string and
         NukeCover$ = NukeCover$ + Chx$                   ' add chemical symbol
      END IF                                              ' if not present
   NEXT irec
 END SUB

'----------------------------------------------------------------------------------------
 SUB Listex(Chx$, Listx$(), nlist)
'----------------------------------------------------------------------------------------
'  routine assemble list of radisotopes of element Chx$
   REDIM Izval(1 TO 30)                                   ' no more than 30 per element
   GET %indx, 1, IndHed
   i1 = CLNG(VAL(IndHed.i1))
   i2 = CLNG(VAL(IndHed.i2))
   icnt = 0
   ifound = %false                                        ' set ifound to false
   FOR irec = i1 TO i2                                    ' read through NDX file
      GET %indx, irec, IndDat                             ' get a record
      IF LEFT$(IndDat.nuke, 2) = Chx$ THEN                ' found a match
         ifound = %true                                   ' set ifound to true
         INCR icnt                                        ' incr counter
         IF LEN(RTRIM$(IndDat.nuke)) < 6 THEN             ' add nuke to Listx$()
            Listx$(icnt) = IndDat.nuke + " " + CHR$(9)    ' with some
         ELSE                                             ' padding
            Listx$(icnt) = IndDat.nuke + " "              ' and include T1/2
         END IF
         Listx$(icnt) = Listx$(icnt) + IndDat.mode + CHR$(9) + IndDat.t + IndDat.Tu
         nuclide$ = IndDat.nuke                           ' get unique integer
         Izval(icnt) = IZMass(nuclide$)                   ' add add to sort array
      ELSEIF ISTRUE ifound AND LEFT$(IndDat.nuke, 2) <> Chx$ THEN
         EXIT FOR                                         ' exit for if read past
      END IF                                              ' the nukes of element
   NEXT irec
   nlist = icnt                                           ' # isotopes of element
   CALL Sortem(Listx$(), Izval(), nlist)                  ' sort by mass in effect
 END SUB

'------------------------------------------------------------------------------
 SUB bldvect(clist() AS STRING * 7, citem AS STRING * 7, n)
'------------------------------------------------------------------------------
'  routine builds character vector clist(n)
'
   IF n = 0 THEN
      n = 1
      clist(n) = citem
   ELSE
      FOR i = 1 TO n
         IF clist(i) = citem THEN EXIT SUB
      NEXT i
      INCR n
      clist(n) = citem
   END IF
 END SUB

'----------------------------------------------------------------------------------------
 FUNCTION ElName(iz) AS STRING
'----------------------------------------------------------------------------------------
'  function returns element name given Z
   DATA "Hydrogen",     "Helium",     "Lithium",     "Beryllium",    "Boron"
   DATA "Carbon"  ,     "Nitrogen",   "Oxygen",      "Fluorine",     "Neon"
   DATA "Sodium",       "Magnesium",  "Aluminum",    "Silicon",      "Phosphorus"
   DATA "Sulphur",      "Chlorine",   "Argon",       "Potassium",    "Calcium"
   DATA "Scandium",     "Titanium",   "Vanadium",    "Chromium",     "Manganese"
   DATA "Iron",         "Cobalt",     "Nickel",      "Copper",       "Zinc"
   DATA "Gallium",      "Germanium",  "Arsenic",     "Selenium",     "Bromine"
   DATA "Krypton",      "Rubidium",   "Strontium",   "Yttrium",      "Zirconium"
   DATA "Niobium",      "Molybdenum", "Technetium",  "Ruthenium",    "Rhodium"
   DATA "Palladium",    "Silver",     "Cadmium",     "Indium",       "Tin"
   DATA "Antimony",     "Tellurium",  "Iodine",      "Xenon",        "Cesium"
   DATA "Barium",       "Lanthanum",  "Cerium",      "Praseodymium", "Neodymium"
   DATA "Promethium",   "Samarium",   "Europium",    "Gadolinium",   "Terbium"
   DATA "Dysprosium",   "Holmium",    "Erbium",      "Thulium",      "Ytterbium"
   DATA "Lutetium",     "Hafnium",    "Tantalum",    "Tungsten",     "Rhenium"
   DATA "Osmium",       "Iridium",    "Platinum",    "Gold",         "Mercury"
   DATA "Thallium",     "Lead",       "Bismuth",     "Polonium",     "Astatine"
   DATA "Radon",        "Francium",   "Radium",      "Actinium",     "Thorium"
   DATA "Protactinium", "Uranium",    "Neptunium",   "Plutonium",    "Americium"
   DATA "Curium",       "Berkelium",  "Californium", "Einsteinium",  "Fermium"
   DATA "Mendelevium" , "Nobelium",   "Lawrencium"
   FUNCTION = READ$(iz)
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION IcutOff(eat(), ebt(), egt(), nspec)
'----------------------------------------------------------------------------------------
'  function determines length of chain for which 99% of cummulative energy of alpha,
'  electrons, and photons is considered - full chain of nspec members
   ia = 1 : ib = 1 : ig = 1
   ea = eat(nspec) : eb = ebt(nspec) : eg = egt(nspec)
   IF nspec = 1 THEN
      FUNCTION = 1
      EXIT FUNCTION
   ELSE
      IF ea > 0.0# THEN                         ' cut point for alphas
         FOR i = nspec-1 TO 1 STEP -1
            IF eat(i) < 0.99# * ea THEN
               ia = i + 1
               EXIT FOR
            END IF
         NEXT i
      END IF
'
      IF eb > 0.0# THEN                         ' cut point for electrons
         FOR i = nspec-1 TO 1 STEP -1
            IF ebt(i) < 0.99# * eb THEN
               ib = i + 1
               EXIT FOR
            END IF
         NEXT i
      END IF
'
      IF eg >  0.0# THEN                        ' cut point for photons
         FOR i = nspec-1 TO 1 STEP -1
            IF egt(i) < 0.99# * eg THEN
               ig = i + 1
               EXIT FOR
            END IF
         NEXT i
      END IF
   END IF
   FUNCTION = MAX&(ig, ia, ib)                  ' max of the three cutpoints
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION CenterMess(a$, n) AS STRING
'----------------------------------------------------------------------------------------
'  function returns string of length n with a$ centered
   IF LEN(LTRIM$(a$)) >= n THEN
      FUNCTION = LEFT$(a$, n)
   ELSE
      tmp$ = STRING$((n - LEN(a$)) \ 2, " ") + a$
      FUNCTION = tmp$ + STRING$(n - LEN(tmp$), " ")
   END IF
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION Ibinry (Taget$) AS LONG
'----------------------------------------------------------------------------------------
'  function returns record number of Taget$ in NDX file based on binary search
   GET %indx, 1, IndHed                  ' first read the head record - 1st record
   l1 = VAL(IndHed.i1)                   ' index of 1st data record
   l2 = VAL(IndHed.i2)                   ' index of last data records
   FUNCTION = 0                          ' set zero if not found by search
   DO                                    ' start the a bisection
      itry = (l1 + l2) \ 2               ' search of the file. note it is
      GET %indx, itry, IndDat            ' sorted by nuclide field. in
      IF IndDat.nuke < Taget$ THEN       ' less than 10 reads we will find
         l1 = itry + 1                   ' the record for the target.
      ELSEIF IndDat.nuke > Taget$ THEN   '
         l2 = itry - 1                   '
      ELSE                               '
         FUNCTION = itry                 ' found it, set function to record
         EXIT LOOP                       ' and exit the loop & return
      END IF                             '
   LOOP WHILE l1 < l2 + 1                ' loop if not found
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION IZWho(ix, iy)
'----------------------------------------------------------------------------------------
'  function return z of element under mouse cursor (ix, iy) of the periodic table
'
   GfxCursor 0                                ' no visible cursor
   irow = CINT((CSNG(iy) - 67.)/46.5 + 0.4)   ' relationship derived somewhat by
   icol = CINT((CSNG(ix) - 11.)/55.4 + 0.4)   ' trail and error
   SELECT CASE irow                           ' first go roll on periodic table
      CASE 1                                  ' and the work the column issues
         IF icol = 1 THEN
            FUNCTION = 1                      ' if over a meaningful cells then
            GfxCursor %chand                  ' the hand cursor
         ELSEIF icol = 18 THEN
            FUNCTION = 2
            GfxCursor %chand
         END IF
      CASE 2
         IF icol > 0 AND icol < 3 THEN
            FUNCTION = 2 + icol
            GfxCursor %chand
         ELSEIF icol > 12 AND icol < 19 THEN
            FUNCTION = 5 + icol - 13
            GfxCursor %chand
         END IF
      CASE 3
         IF icol > 0 AND icol < 3 THEN
            izwho = 10 + icol
            GfxCursor %chand
         ELSEIF icol > 12 AND icol < 19 THEN
            FUNCTION = 13 + icol - 13
            GfxCursor %chand
         END IF
      CASE 4 TO 5
         IF icol > 0 AND icol < 19 THEN
            FUNCTION = (irow-4)*18 + icol + 18
            GfxCursor %chand
         END IF
      CASE 6
         IF icol > 0 AND icol < 4 THEN
            FUNCTION = 55 + icol - 1
            GfxCursor %chand
         ELSEIF icol > 3 AND icol < 19 THEN        ' break for lanthanide series
            FUNCTION = 72 + icol - 4               ' on row 9
            GfxCursor %chand
         END IF
      CASE 7
         IF icol > 0 AND icol < 4 THEN
            FUNCTION = 87 + icol - 1
            GfxCursor %chand
         END IF
      CASE 8                                       ' lanthanide series
         IF icol > 4 AND icol < 19 THEN
            FUNCTION = 58 + icol -5
            GfxCursor %chand
         END IF
      CASE 9                                       ' actinide series
         IF icol > 4 AND icol < 19 THEN
            FUNCTION = 90 + icol - 5
            GfxCursor %chand
         END IF
      CASE ELSE
         FUNCTION = 0
         GfxCursor 0
   END SELECT
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION MSGBOX(sText$, sCaption$) AS LONG
'----------------------------------------------------------------------------------------
'  function displays message and get user response
   uType = %YESNO + %APPLMODAL
   IF LEN(sCaption$) = 0 THEN           ' if null use DEFAULT
      sCap$ = $DEFAULT_MSGBOX_CAPTION
   ELSE
      sCap$ = sCaption$
   END IF
   FUNCTION = ConsoleMessageBox(sText$ + " continue?", utype, sCap$, 0, 0)
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION RefreshWindow(BYVAL lPlaceHolder AS LONG) AS LONG
'----------------------------------------------------------------------------------------
' function of thread to refresh graphics window every 1/4 second.
   DO
      SLEEP 250
      GfxRefresh 0
   LOOP
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION IZMass(Nuke$) AS LONG
'----------------------------------------------------------------------------------------
'  function construct unique integer key as f(z, A, metastable) for nuclide
   ione = 0
   iz = INSTR($sym, LEFT$(Nuke$, 2))\2 + 1
   IF INSTR(2, Nuke$, "m") > 0 THEN
      ia = ABS(VAL(MID$(Nuke$, 3, INSTR(2, Nuke$, "m") -3)))
      ione = 1
   ELSEIF INSTR(2, Nuke$, "n") > 0 THEN
      ia = ABS(VAL(MID$(Nuke$, 3, INSTR(2, Nuke$, "n") -3)))
      ione = 1
   ELSE
      ia = ABS(VAL(RIGHT$(Nuke$, LEN(Nuke$)-2)))
   END IF
   FUNCTION = iz * 10000 + ia * 10 + ione
 END FUNCTION

'------------------------------------------------------------------------------
 FUNCTION invect(clist() AS STRING *7, citem AS STRING * 7, n)AS LONG
'------------------------------------------------------------------------------
'  function returns the index of citem in the array clist(m). if not in
'  clist a zero is returned
'
   IF n = 0 THEN
      PRINT "Error in invect, no elements in array"
      WAITKEY$
   ELSE
      FOR i = 1 TO n
         IF clist(i) = citem THEN
            FUNCTION = i
            EXIT FUNCTION
         END IF
      NEXT i
      FUNCTION = 0
   END IF
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION Timest (T AS STRING*8, ix AS STRING*2)
'----------------------------------------------------------------------------------------
'  function computes T1/2 (s) given T and its units
'
   tp = VAL(T)
   SELECT CASE TRIM$(ix)
      CASE "ys"                         ' yocto
         FUNCTION = 1D-24 * tp
      CASE "zs"                         ' zepto
         FUNCTION = 1D-21 * tp
      CASE "as"                         ' atto
         FUNCTION = 1D-18 * tp
      CASE "fs"                         ' femto
         FUNCTION = 1.0D-15 * tp
      CASE "ps"                         ' pico
         FUNCTION = 1.0D-12 * tp
      CASE "ns"                         ' nano
         FUNCTION = 1.0D-09 * tp
      CASE "us"                         ' micro
         FUNCTION = 1.0E-06 * tp
      CASE "ms"                         ' milli
         FUNCTION = 1.0E-03 * tp
      CASE "s"                          ' second
         FUNCTION = tp * 1.0#
      CASE "m"                          ' minute
         FUNCTION = 60# * tp
      CASE "h"                          ' hour
         FUNCTION = 3600# * tp
      CASE "d"                          ' day
         FUNCTION = 86400# * tp
      CASE "y"                          ' Gregorian year
         FUNCTION = 3.1556952D+07 * tp
      CASE "ky"                         ' kilo
         FUNCTION = 3.1556952D+10 * tp
      CASE "my"                         ' mega
         FUNCTION = 3.1556952D+13 * tp
      CASE "gy"                         ' giga
         FUNCTION = 3.1556952D+16 * tp
      CASE "ty"                         ' tera
         FUNCTION = 3.1556952D+19 * tp
      CASE "py"                         ' peta
         FUNCTION = 3.1556952D+22 * tp
      CASE "ey"                         ' exa
         FUNCTION = 3.1556952D+25 * tp
      CASE "zy"                         ' zetta
         FUNCTION = 3.1556952D+28 * tp
      CASE "yy"                         ' yotta
         FUNCTION = 3.1556952D+31 * tp
      CASE ELSE
         PRINT "Error in Timest; can not translate time units "+ ix$
         WAITKEY$
    END SELECT
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION GetFileCount(FileSpec$) AS LONG
'----------------------------------------------------------------------------------------
'   function evaluates a file specification and returns the number of files
'   matching the specification.  Wild card characters ("*" and "?") are permitted.
'   Drive and directory path specifications may also be included in filespec$.
'
   DIM FileCnt AS LOCAL INTEGER
   IF LEN(DIR$(FileSpec$)) = 0 THEN                       ' ensure filespec is valid.
      FileCnt = 0                                         ' If not return zero.
   ELSE                                                   ' if valid then
      FileCnt = 1                                         '  count it and
      DO WHILE LEN(DIR$) > 0                              ' loop to count
         INCR FileCnt                                     ' other files
      LOOP
   END IF
   FUNCTION = FileCnt
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION InKeyCode(sKey$)
'----------------------------------------------------------------------------------------
'  function returns a unique integer for any key captured by Inkey$ or Waitkey$
   SELECT CASE LEN(skey$)
      CASE 1                                              ' Ordinary key
         FUNCTION = ASC(sKey$)
      CASE 2                                              ' Extended key
         FUNCTION = 1000 + ASC(sKey$,2)
      CASE ELSE
         FUNCTION = 0
   END SELECT
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION Expf1(zlm##, t) AS EXT
'----------------------------------------------------------------------------------------
'  funtion to compute [1.0 - exp(-lm * t)] / lm.
   DIM zlmt AS LOCAL EXT
   zlmt = zlm## * CEXT(t)
   IF zlmt <  1.0D-13## THEN                              ' use series expansion
      FUNCTION = CEXT(t)*(1.0##-zlmt/2.0##*(1.0##-zlmt/3.0##*(1.0##-zmlt/4.0##)))
   ELSE                                                   ' straight evaluation
      FUNCTION = (1.0## - EXP(-zlmt)) / CEXT(zlm##)
   END IF
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION ExpFun(t##) AS EXT
'----------------------------------------------------------------------------------------
'  function to compute exp(t)
   IF t## < -180.0## THEN                                 ' set to zero
      FUNCTION = 0.0##
   ELSE
      FUNCTION = EXP(t##)
   END IF
 END FUNCTION

'----------------------------------------------------------------------------------------
 FUNCTION ResABC(Quest$, a$, b$, c$, def$) AS STRING
'----------------------------------------------------------------------------------------
'  function attaches "([a]/b)?" or "(a/b/[c])?" to Quest$, and user's response is
'  returned as lower case.  The values of a$, b$, c$, the default (def$) passed
'  to the function must be lower case. If c$ = "#" then the option is a/b.
'
   tex$ = Quest$ + " ("
   IF def$ = a$ THEN
      tex$ = tex$ + "[" + a$ + "]/"
   ELSE
      tex$ = tex$ + a$ + "/"
   END IF
   IF def$ = b$ THEN
      tex$ = tex$ + "[" + b$ + "]"
   ELSE
      tex$ = tex$ + b$
   END IF
   IF c$ <> "#" THEN
      IF def$ = c$ THEN
         tex$ = tex$ + "/[" + c$ + "])? "
      ELSE
         tex$ = tex$ + "/" + c$ + ")? "
      END IF
   ELSE
      tex$ = tex$ + ")? "
   END IF
   icol = LEN(tex$) + 1
   irow = CURSORY
   LOCATE irow
   PRINT tex$;
   DO
      iok = %true
      LOCATE irow, icol
      INPUT Resp$
      IF LEN(TRIM$(Resp$)) = 0 THEN Resp$ = def$
      Resp$ = LCASE$(Resp$)
      IF Resp$ = a$ OR Resp$ =  b$ OR Resp$ = c$ THEN
         irow = CURSORY
         PRINT STRING$(50, " ")
         LOCATE irow
         FUNCTION = Resp$
      ELSE
         iok = %false
         PRINT " Incorrect response, try again.";
         LOCATE irow, icol
         PRINT STRING$(10, " ")
      END IF
   LOOP WHILE ISFALSE iok
 END FUNCTION

'------------------------------------------------------------------------------
 FUNCTION GetResourceBitMapSize (szBM AS ASCIIZ, nWidth&, nHeight&) AS LONG
'------------------------------------------------------------------------------
'  function to get bitmap parameters from resource file
   LOCAL pBM AS tagBitMap PTR, hInst AS LONG, lres AS LONG, lres2 AS LONG
   FUNCTION = %FALSE                                      ' default = fail
   hInst = GetModuleHandle ("")                           ' resource file
   lRes  = FindResource(hInst, szBM, BYVAL %RT_BITMAP)
   IF lRes  THEN lRes2 = LoadResource(hInst, lRes)        ' if found, load the BMP
   IF lRes2 THEN pBM = LockResource(lRes2)                ' lock to get a pointer
   IF pBM  THEN                                           ' to the BMP file
      nWidth&  = @pBM.BmWidth
      nHeight& = @pBM.bmHeight
      FUNCTION  =  %TRUE                                  ' SUCCESS!!
   END IF
 END FUNCTION

'------------------------------------------------------------------------------
 FUNCTION CRC32(BYVAL dwOffset AS DWORD, dwLen AS DWORD) AS DWORD
'------------------------------------------------------------------------------
' CRC32, by Wayne Diamond, 9th December 2002
 ! mov esi, dwOffset  ;esi = ptr to buffer
 ! mov edi, dwLen     ;edi = length of buffer
 ! mov ecx, -1        ;ecx = -1
 ! mov edx, ecx       ;edx = -1
 nextbyte:           ';next byte from butter
 ! xor eax, eax       ;eax = 0
 ! xor ebx, ebx       ;ebx = 0
 ! lodsb              ;get next byte
 ! xor al, cl         ;xor al with cl
 ! mov cl, ch         ;cl = ch
 ! mov ch, dl         ;ch = dl
 ! mov dl, dh         ;dl = dh
 ! mov dh, 8          ;dh = 8
 nextbit:            ';next bit in the byte
 ! shr bx, 1          ;shift bits in bx right by 1
 ! rcr ax, 1          ;(rotate through carry) bits in ax by 1
 ! jnc nocarry        ;jump to nocarry if carry flag not set
 ! xor ax, &h08320    ;xor ax with 33568
 ! xor bx, &h0EDB8    ;xor bx with 60856
 nocarry:            ';if carry flag wasn't set
 ! dec dh             ;dh = dh - 1
 ! jnz nextbit        ;if dh isnt zero, jump to nextbit
 ! xor ecx, eax       ;xor ecx with eax
 ! xor edx, ebx       ;xor edx with ebx
 ! dec edi            ;finished with that byte, decrement counter
 ! jnz nextbyte       ;if edi counter isnt at 0, jump to nextbyte
 ! not edx            ;invert edx bits - 1s complement
 ! not ecx            ;invert ecx bits - 1s complement
 ! mov eax, edx       ;mov edx into eax
 ! rol eax, 16        ;rotate bits in eax left by 16 places
 ! mov ax, cx         ;mov cx into ax
 ! mov FUNCTION, eax  ;crc32 result is in eax
 END FUNCTION
