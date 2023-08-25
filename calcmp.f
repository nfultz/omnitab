      SUBROUTINE PLOTT(XPAGE,YPAGE,IPEN)
C           ORIGINALLY CALLED PLOT
C           U. KATTNER [MODIFICATIONS: W. ANDERSON, D. KAHANER] (NIST)
C           MODIFIED JULY 1992 BY ALAN HECKERT TO WORK AS A STAND
C           ALONE CALCOMP COMPATIBLE LIBRARY.
C           MADE MODIFICATIONS TO EXISTING POSTSCRIPT, TEKTRONIX,
C           HP-GL, QMS DRIVERS.  HAVE NOT STARTED WORKING ON THE 
C           PC DRIVER. 
C           ADDED X11 DRIVER (USED THE X11 DRIVER FROM DATAPLOT).
C
C           POSSIBLE ENHANCEMENTS:
C           1) USE HARDWARE CHARACTERS IN SYMBOL.  THIS WOULD ALLOW
C              TAKING ADVANTAGE OF THE POSTSCRIPT TYPESET QUALITY
C              FONTS.  LESS IMPORTANT FOR THE OTHER SUPPORTED 
C              DRIVERS, BUT SOMETIMES HARDWARE IS FASTER.
C           2) ADD ADDITIONAL DRIVERS.  I HAVE IN MIND ADDING CGM
C              SUPPORT (TO ALLOW IMPORTING TO POST-PROCESSORS).  OTHERS
C              WILL BE CONSIDERED AS NEED ARISES.
C           3) CONVERT POSTSCRIPT OUTPUT TO ENCAPSULATED FORMAT 
C              (PACKAGES SUCH AS WORD-PERFECT REQUIRE ENCAPSULATED
C              FORMAT WHEN IMPORTING POSTSCRIPT FILES).
C           4) USE NEWPEN TO SUPPORT COLOR (CURRENTLY SETUP TO SET
C              THE LINE DASH PATTERN TO EMULATE COLOR).
C              (THIS WAS COMPLETED JULY 1992).
C           5) ADDED NUMBER ROUTINE JULY 1992.
C           6) ADDED LINE ROUTINE JULY 1992.
C
C           THESE ROUTINES WERE RENAMED FOR OMNITAB TO ALLOW 
C           OMNITAB TO BE LINKED WITH A LOCAL CALCOMP LIBRARY.
C           ROUTINE                NEW NAME
C           =======                ========
C           PLOTS                  PLOTST
C           PLOT                   PLOTT
C           SYMBOL                 SYMBLT
C           SPLOT                  SPLOT (OMNITAB USE OWN SPLOTS)
C           NUMBER                 NUMBRT
C           LINE                   LINET
C           DASHS                  DASHST
C           WHERE                  WHERET
C           NEWPEN                 NEWPNT
C           SYMBS                  SYMBS
C
      CHARACTER*1 HIX,HIY,LOX,LOY,LSB, PC(2:3)
      INTEGER IPEN, II,IP,IP0,IPL,IX0,IY0,IPATH
C
C  FOLLOWING IS FOR SUN (COMPILE WITH -r8, WHICH MAKES DEFAULT
C  INTEGER 8 BYTES.  HOWEVER, THIS SCREWS UP CALL TO X11 LIBRARY.
C  EXPLICITLY DECLARE AS INTEGER*4 TO GET AROUND THIS.  CAN LEAVE
C  THIS OFF IF NOT USING -r8 OPTION.
C
CCCCC INTEGER*4 IX(51), IY(51), L
CCCCC INTEGER*4 IXTEMP, IYTEMP, IOR, IX11X, IX11Y, IERR
CCCCC INTEGER*4 IBACK2, IPIX, ICODE, IDST 
      INTEGER IX(51), IY(51), L
      INTEGER IXTEMP, IYTEMP, IOR, IX11X, IX11Y, IERR
      INTEGER IBACK2, IPIX, ICODE, IDST 
      REAL XPAGE,YPAGE, RX,RY
      CHARACTER*1 FF,ESC,GS
      CHARACTER*132 STRING,TEMP
      LOGICAL OUTB0,OUTBL, LNPEN,BATON
      INTEGER IDEVIC,NPEN, IXL,IYL,NX0,NY0,NXM,NYM
      REAL PX,PY,PX0,PY0, XL,YL,FL
      CHARACTER*1 CJUNK
C.....VARIABLES USED IN PLOTS
      CHARACTER TERM*3
C      INTEGER IBUF(*),NLOC,LDEV
C.....VARIABLES USED IN NEWPEN
      INTEGER INP
C.....VARIABLES USED IN WHERE
      REAL RXPAGE,RYPAGE,RFACT
C
C.....VARIABLES USED BY X11 DRIVER
      CHARACTER*4 X11FLG
CCCCC INTEGER*4 IDSPLY(80)
      INTEGER IDSPLY(80)
C
      INTEGER IBACK, IMAXCL
      REAL AXSIZE, AYSIZE
      CHARACTER*80 CPOST,CHPGL,CHPGL2,CQMS,CTEKT
C
      COMMON/VGCALC/ IBACK, IFORE, AXSIZE, AYSIZE, IMAXCL
      COMMON/VGPNF/ IDEVIC,IDST,NPEN,NUNIT
      COMMON/VGBT/BATON
      COMMON/VGX11/IDSPLY
      COMMON/VGX112/X11FLG
      COMMON/VGFLAG/IPOST,IHPGL,IHPGL2,IQMS,ITEKT,IX11
      COMMON/VGNAME/CPOST,CHPGL,CHPGL2,CQMS,CTEKT
C
      SAVE FF,ESC,GS
      SAVE NX0,NY0,NXM,NYM,PX,PY,PX0,PY0,JDCOL0,JDROW0
      SAVE L,STRING,IX,IY
      SAVE XL,YL,FL,IXL,IYL,OUTBL,/VGBT/,/VGPNF/
C
      EXTERNAL XINIT, XERASE, XCLEAR, XFORE, XEND, XLATTR, XDRAW
      DATA PC/'D','U'/
C
C
20    FORMAT(2(',',I5))
30    FORMAT('^',A1,I5.5,':',I5.5)
40    FORMAT(2I5,A2)
C  NOTE: XTERM TEKT EMULATOR SEEMS TO NEED A LEADING SPACE
C        MODIFY THIS FORMAT (I.E., DELETE THE 1X) IF THIS CAUSES
C        A PROBLEM (E.G., I THINK THE NCSA TEKT EMULATOR CHOKES ON
C        THE LEADING SPACE.
102   FORMAT(1X,A)
100   FORMAT(A)
101   FORMAT('SC 0,',I6,', 0,',I6,';')
C
C-----------------------------------------------------------------------
C     |IPEN| = 2   : "PEN DOWN" DURING MOVEMENT
C            = 3   : "PEN UP"   DURING MOVEMENT
C      IPEN  < 0   : DEFINE NEW ORIGIN
C      IPEN  = 999 : FINISH PLOTTING AND EXIT GRAPHIC MODE
C-----------------------------------------------------------------------
C

      LNPEN = .FALSE.
      IP0 = IPEN
C
1000  XL = XPAGE
      YL = YPAGE
C
1010  IF (IP0.EQ.999) THEN
        IP = 3
      ELSE
        IP = IABS(IP0)
      ENDIF
      IPL = IP
      IX0 = NX0+NINT(XL*PX)
      IY0 = NY0+NINT(YL*PY)
      OUTB0 = (IX0.LT.0.OR.IX0.GT.NXM.OR.IY0.LT.0.OR.IY0.GT.NYM)
      IF (IP.EQ.2.AND.OUTB0.AND.OUTBL) GOTO 1900
      IF (IP.EQ.2.AND.OUTBL.AND..NOT.OUTB0) IP = 3
C
C.... IF NEW LINE PLOT BUFFER CONTENT
C
      IF (IP.EQ.3.AND.L.GT.0) THEN
        IF (IDEVIC.LT.10) THEN
          IF (L.GT.6) WRITE(NUNIT,102) STRING(:L)
        ELSEIF (IDEVIC.LT.20) THEN
          IF (L.GT.1.AND..NOT.BATON) CALL AGRAFF(II,IDST,L,IX,IY)
        ELSEIF (IDEVIC.EQ.20) THEN
          IF (STRING(2:2).EQ.'D'.OR.L.GT.16)
     &      WRITE(NUNIT,100) STRING(:L)//';'
        ELSEIF (IDEVIC.EQ.21) THEN
          IF (STRING(2:2).EQ.'D'.OR.L.GT.13) WRITE(NUNIT,100) STRING(:L)
        ELSEIF (IDEVIC.EQ.22) THEN
          IF (STRING(12:12).EQ.'D'.OR.L.GT.12) THEN
            WRITE(NUNIT,100) STRING(:L)
            WRITE(NUNIT,100) 'stroke'
          ENDIF
        ELSEIF (IDEVIC.EQ.23) THEN
          IF (STRING(2:2).EQ.'D'.OR.L.GT.16)
     &      WRITE(NUNIT,100) STRING(:L)//';'
        ELSEIF (IDEVIC.EQ.30) THEN
          IF(X11FLG.EQ.'ON')THEN
            IF (L.GT.1) CALL XDRAW(IX,IY,L)
          ENDIF
        ENDIF
        L = 0
      ENDIF
      IF (LNPEN) GOTO 4000
      IF (IP0.EQ.999) GOTO 1999
      IF (IP.EQ.2.AND..NOT.OUTB0.AND..NOT.OUTBL) GOTO 1100
      IF (IPL.EQ.3.AND..NOT.OUTB0) GOTO 1100
      IF (IPL.EQ.3.AND.OUTB0) GOTO 1900
C
C.... CLIP CURVE IF IT LEAVES THE PLOT AREA
C
      IF (IX0.EQ.IXL) THEN
        RX = 0.0
      ELSE
        RX = FLOAT(IYL-IY0)/FLOAT(IXL-IX0)
      ENDIF
      IF (IY0.EQ.IYL) THEN
        RY = 0.0
      ELSE
        RY = FLOAT(IXL-IX0)/FLOAT(IYL-IY0)
      ENDIF
      IF (IX0.LT.0.OR.IXL.LT.0) THEN
        IY0 = IY0+NINT(FLOAT(-IX0)*RX)
        IX0 = 0
      ELSEIF (IX0.GT.NXM.OR.IXL.GT.NXM) THEN
        IY0 = IY0+NINT(FLOAT(NXM-IX0)*RX)
        IX0 = NXM
      ENDIF
      IF (IY0.LT.0.OR.IYL.LT.0) THEN
        IX0 = IX0+NINT(FLOAT(-IY0)*RY)
        IY0 = 0
      ELSEIF (IY0.GT.NYM.OR.IYL.GT.NYM) THEN
        IX0 = IX0+NINT(FLOAT(NYM-IY0)*RY)
        IY0 = NYM
      ENDIF
C
C.... PLOT THE POINT        
C
1100  IF (IDEVIC.LT.10) THEN
C.... Encode X,Y for Tektronix and store in string
        IF (IP.EQ.3) THEN
          STRING = GS
          L = 1
        ELSE
          IF (L.GT.75) THEN
            WRITE(NUNIT,102) STRING(:L)
            TEMP = GS//STRING(L:L)
            STRING=TEMP
            L = 2
          ENDIF
        ENDIF
        HIY = CHAR(32+MOD(IY0/128,32))
        LSB = CHAR(96+MOD(IX0,4)+MOD(IY0,4)*4)
        LOY = CHAR(96+MOD(IY0/4,32))
        HIX = CHAR(32+MOD(IX0/128,32))
        LOX = CHAR(64+MOD(IX0/4,32))
        TEMP = STRING(:L)//HIY//LSB//LOY//HIX//LOX
        STRING=TEMP
        L = L+5
      ELSEIF (IDEVIC.LT.20) THEN
C.....STORE X AND Y IN IX AND IY, CALL AGRAFF FOR IBM-PC
        IF (L.EQ.51) THEN
          CALL AGRAFF(II,IDST,L,IX,IY)
          IX(1) = IX(51)
          IY(1) = IY(51)
          L = 1
        ENDIF
        L = L+1
        IX(L) = IX0
        IY(L) = IY0
      ELSEIF (IDEVIC.EQ.20) THEN
C.....STORE X AND Y IN STRING WITH HPGL SYNTAX
        IF (IP.EQ.3) THEN
          STRING = 'PU'
          WRITE(STRING(3:14),20) IX0,IY0
          TEMP = STRING(:14)//'PD'
          STRING=TEMP
          L = 16
        ELSE
          IF (L.GT.119) THEN
            WRITE(NUNIT,100) STRING(:L)//';'
            STRING = 'PD'
            L = 2
          ENDIF
          L = L+12
          WRITE(STRING(L-11:L),20) IX0,IY0
        ENDIF
      ELSEIF (IDEVIC.EQ.21) THEN
C.....STORE X AND Y IN STRING WITH QMS SYNTAX
        IF (L.GT.119) THEN
          WRITE(NUNIT,100) STRING(:L)
          L = 0
        ENDIF
        L = L+13
        WRITE(STRING(L-12:L),30) PC(IP),IX0,IY0
      ELSEIF (IDEVIC.EQ.22) THEN
C.....STORE X AND Y IN STRING WITH POSTSCRIPT SYNTAX
        IF (L.GT.120) THEN
C           IN CASE THERE ARE TOO MANY POINTS, PUT OUT FIRST PART
          IF(IPATH.GT.1000) THEN
             WRITE(NUNIT,100)STRING(:L)
             WRITE(NUNIT,100)'stroke'
             STRING(1:12)=STRING(L-11:L-2)//' U'
             L=12
             IPATH=0
          ELSE
              WRITE(NUNIT,100) STRING(:L)
              L = 0
          ENDIF
        ENDIF
        L = L+12
        IF(IP.EQ.2)THEN
           IPATH=IPATH+2
        ELSE
           IPATH=0
        ENDIF
        WRITE(STRING(L-11:L),40) IX0,IY0,PC(IP)
      ELSEIF (IDEVIC.EQ.23) THEN
C.....STORE X AND Y IN STRING WITH HPGL SYNTAX
        IF (IP.EQ.3) THEN
          STRING = 'PU'
          WRITE(STRING(3:14),20) IX0,IY0
          TEMP = STRING(:14)//'PD'
          STRING=TEMP
          L = 16
        ELSE
          IF (L.GT.119) THEN
            WRITE(NUNIT,100) STRING(:L)//';'
            STRING = 'PD'
            L = 2
          ENDIF
          L = L+12
          WRITE(STRING(L-11:L),20) IX0,IY0
        ENDIF
      ELSEIF (IDEVIC.EQ.30) THEN
C.....X11
        IF(X11FLG.EQ.'ON')THEN
          IF (L.EQ.51) THEN
            CALL XDRAW(IX,IY,L)
            IX(1) = IX(51)
            IY(1) = IY(51)
            L = 1
          ENDIF
          L = L+1
          IX(L) = IX0
          IY(L) = IY0
        ENDIF
      ENDIF
      IF (IPL.NE.IP) THEN
        OUTBL = .FALSE.
        GOTO 1000
      ENDIF
C
C.....SAVE VALUES AND STATUS
C
1900  IXL = IX0
      IYL = IY0
      OUTBL = OUTB0
C
C.....New origin
C
      IF (IP0.GT.0) RETURN
      NX0 = IX0
      NY0 = IY0
      RETURN
C
C.....FINISH PLOTTING
C
1999  IF (IDEVIC.LT.10) THEN
        IF (IDEVIC.EQ.2) THEN
          WRITE(NUNIT,102) ESC//'%!2'
        ELSEIF (IDEVIC.EQ.3) THEN
          WRITE(NUNIT,102) ESC//'[?38l'
          WRITE(NUNIT,102) ESC//'[61"p'
        ENDIF
        WRITE(*,*) ' ?'
        READ(*,'(A1)') CJUNK
      ELSEIF (IDEVIC.LT.20) THEN
        CALL AGRAF0(II,1)
        WRITE(*,*) ' ?'
        READ(*,'(A1)') CJUNK
      ELSEIF (IDEVIC.EQ.20) THEN
        WRITE(NUNIT,100) 'SP;'
      ELSEIF (IDEVIC.EQ.21) THEN
        WRITE(NUNIT,100) '^IGE^O'
        WRITE(NUNIT,100) '^IMH0025010000^IMV0050008250'
        WRITE(NUNIT,100) '^,'
        WRITE(NUNIT,100) '^-^PN^-'
      ELSEIF (IDEVIC.EQ.22) THEN
        WRITE(NUNIT,100) 'stroke showpage grestore'
      ELSEIF (IDEVIC.EQ.23) THEN
        WRITE(NUNIT,100) 'SP;'
        WRITE(NUNIT,100) ESC//'E'
      ELSEIF (IDEVIC.EQ.30) THEN
        IF(X11FLG.EQ.'ON')THEN
          CALL XCLEAR
          WRITE(*,*) ' ?'
          READ(*,'(A1)') CJUNK
C
C  ONLY CALL XEND WHEN READY TO TERMINATE PROGRAM.  SEEMS TO BE A 
C  PROBLEM IN x11src.c THAT CANNOT RE-OPEN WINDOW.
C
CCCCC     CALL XEND
        END IF
      ENDIF
CCCCC CLOSE(60)
      RETURN
C***********************************************************************
C*     GET PARAMETERS FOR DEVICE
C***********************************************************************
      ENTRY PLOTST(IDEV,NLOC,LDEV)
C        ORIGINAL NAME PLOTS       
C                  (IBUF,NLOC,LDEV)
C
C  NOTE: THE PARAMETERS PASSED ARE NOT USED IN THE SAME WAY 
C        AS ON CALCOMP.  USE THE FOLLOWING:
C 
C        IDEV = DEVICE IDENTIFIER
C               0 - TEKTRONIK (READ fort.50 FOR DIFFERENT MODEL
C               1 - TEKTRONIX 41XX
C               2 - VT-100 USING TEKTRONIX EMULATION
C               3 - VT-200 USING TEKTRONIX EMULATION
C              10 - IBM PC (NOT YET SUPPORTED)
C              20 - HP-GL
C              21 - QMS (QUIC PROTOCOL)
C              22 - POSTSCRIPT 
C              23 - HP-GL/2 (FOR LASERJET III)
C              30 - X11
C         NLOC = NOT USED
C         LDEV = UNIT NUMBER TO USE (IGNORED IF <= 0)
C
      FF  = CHAR(12)
      ESC = CHAR(27)
      GS  = CHAR(29)
C
      IF(IDEV.GE.0)THEN   
        IF(IDEV.EQ.0)IDEVIC=0
        IF(IDEV.EQ.1)IDEVIC=1
        IF(IDEV.EQ.2)IDEVIC=2
        IF(IDEV.EQ.3)IDEVIC=3
        IF(IDEV.EQ.10)IDEVIC=10
        IF(IDEV.EQ.20)IDEVIC=20
        IF(IDEV.EQ.21)IDEVIC=21
        IF(IDEV.EQ.22)IDEVIC=22
        IF(IDEV.EQ.23)IDEVIC=23
        IF(IDEV.EQ.30)IDEVIC=30
      ENDIF 
      IF(LDEV.GT.0)THEN
        NUNIT=LDEV
      ELSE
        NUNIT=60
      ENDIF
C
      IF (IDEVIC.EQ.99) RETURN
      NPEN = 1
      L = 0
C-----------------------------------------------------------------------
C      0 <= IDEVIC < 10 : TEKTRONIX
C     10  = IDEVIC      : IBM-PC, USING LIBRARY LGRAF
C     20 <= IDEVIC      : HARDCOPY DEVICES
C     30  = IDEVIC      : X11
C-----------------------------------------------------------------------
C     TEKTRONIX
C-----------------------------------------------------------------------
      IF (IDEVIC.EQ.0) THEN
C.......................................................................
C     READ FILE WITH TERMINAL DEFINITION,
C     OTHERWISE ASSUME TEKTRONIX 4014
C.......................................................................
        IF(NUNIT.NE.6 .AND. ITEKT.EQ.0)THEN
          OPEN(UNIT=NUNIT,FILE=CTEKT,ERR=2009)
          ITEKT=1
        ENDIF
 2009   CONTINUE
CCCCC   TERM = 'TK0'
CCCCC   OPEN(50,STATUS='OLD',ERR=2000)
CCCCC   READ(50,100) TERM
CCCCC   CLOSE(50)
2000    CONTINUE
C.......TEKTRONIX 41XX
CCCCC   IF (TERM.EQ.'TK1'.OR.TERM.EQ.'TKB') THEN
CCCCC     IDEVIC = 1
        IF (IDEVIC.EQ.1) THEN
          WRITE(NUNIT,102) ESC//FF
          WRITE(NUNIT,102) ESC//'KA0'
C.......TEKTRONIX 410X, EMULATING A VT100
CCCCC   ELSEIF (TERM.EQ.'VT1') THEN
CCCCC     IDEVIC = 2
        ELSEIF (IDEVIC.EQ.2) THEN
          WRITE(NUNIT,102) ESC//'[H'//ESC//'[J'
          WRITE(NUNIT,102) ESC//'%!0'
          WRITE(NUNIT,102) ESC//'KA0'
C.......VT200
CCCCC   ELSEIF (TERM.EQ.'VT2') THEN
CCCCC     IDEVIC = 3
        ELSEIF (IDEVIC.EQ.3) THEN
          WRITE(NUNIT,102) ESC//'[?38h'
        ENDIF
C.......TEKTRONIX 4014
        WRITE(NUNIT,102) ESC//FF
        WRITE(NUNIT,102) ESC//'9'
        NX0 = 0
        NY0 = 0
        NXM = 4095
CCCCC   NYM = 4095
        NYM = 3123
CCCCC   PX0 = 4096./25.
        PX0 = 4096./AXSIZE
CCCCC   PY0 = PX0
        PY0 = 3124./AYSIZE
        IDST = 96
C-----------------------------------------------------------------------
C     IBM-PC, USING LGRAF
C-----------------------------------------------------------------------
      ELSEIF (IDEVIC.EQ.10) THEN

        NXM=JDCOL0
        NYM=JDROW0
        PX0 =  FLOAT(NXM)/25.
        PY0 = -FLOAT(NYM)/18.75
        NXM = NXM-1
        NYM = NYM-1
        NX0 = 0
        NY0 = NYM
        IDST = 1
C-----------------------------------------------------------------------
C     X11 (USE DATAPLOT X11 DRIVER)
C-----------------------------------------------------------------------
      ELSEIF (IDEVIC.EQ.30) THEN
        IXTEMP=0
        IYTEMP = 0
        IOR = 0
        IX11X = 0
        IX11Y = 0
C
C  SET IDSPLY IN CALLING PROGRAM (THIS ALLOWS OTHER THAN DEFAULT
C  NAME).  IF IDSPLY(1)=-1, THIS MEANS USER HAS NOT SET.
C
        IF(IDSPLY(1).EQ.-1)THEN
          IDSPLY(1) = ICHAR('D')
          IDSPLY(2) = ICHAR('E')
          IDSPLY(3) = ICHAR('F')
          IDSPLY(4) = ICHAR('A')
          IDSPLY(5) = ICHAR('U')
          IDSPLY(6) = ICHAR('L')
          IDSPLY(7) = ICHAR('T')
          IDSPLY(8) = 0
        ENDIF
        IERR = 0
C
C  INITIALIZE X11
C
        IF(IX11.NE.0)THEN
          IXTEMP=NXM + 1
          IYTEMP=NYM + 1
          IX11X = IXTEMP
          IX11Y = IYTEMP
          GOTO3099
        ENDIF
        CALL XINIT(IXTEMP,IYTEMP,IOR,IX11X,IX11Y,IDSPLY,IERR)
        IF(IERR.EQ.1)THEN
          PRINT *,'UNABLE TO OPEN X11 CONNECTION'
          X11FLG = 'OFF'
          RETURN
        ELSE IF(IERR.EQ.2)THEN
          PRINT *,'X11 ALREADY OPEN'
          X11FLG = 'ON'
        ELSE
          X11FLG = 'ON'
        END IF
        IX11=1
        PRINT *,' RE-SIZE WINDOW WITH MOUSE (IF DESIRED), THEN '
        PRINT *,' ENTER A <CR>:'
        READ(*,'(A1)') CJUNK 
C
C  CLEAR X11 SCREEN TO BACKGROUND COLOR, GET DIMENSIONS AFTER
C  USER HAS OPPORTUNITY TO RESIZE.
C  IBACK = 0 FOR BLACK BACGROUND, 1 FOR WHITE BACKGROUND, 2-64
C  FOR A SPECIFIC COLOR.
C  IPIX =1 FOR BACKGROUND PIXMAP (TO ALLOW SCREEN UPDATING), 0 TO
C  DISABLE PIXMAP.
C
        IXTEMP=IX11X
        IYTEMP=IX11Y
 3099   CONTINUE
        IPIX=1
        IBACK2=IBACK
        IF(IBACK2.LT.0 .OR. IBACK2.GT.64) IBACK2=0
        CALL XERASE(IXTEMP,IYTEMP,IOR,IX11X,IX11Y,IBACK2,IPIX)
        ICODE = IFORE
        CALL XFORE(ICODE)
        NXM = IX11X
        NYM = IX11Y
        PX0 =  REAL(NXM)/AXSIZE
        PY0 = -REAL(NYM)/AYSIZE
        NXM = NXM - 1
        NYM = NYM - 1
        NX0 = 0
        NY0 = NYM
        IDST = 0
C-----------------------------------------------------------------------
C     HARDCOPY DEVICES
C-----------------------------------------------------------------------
C.....HPGL-PLOTTER
      ELSEIF (IDEVIC.EQ.20) THEN
        IF(IHPGL.EQ.0)THEN
          OPEN(UNIT=NUNIT,FILE=CHPGL,ERR=2019)
          IHPGL=1
        ENDIF
 2019   CONTINUE
        WRITE(NUNIT,100) 'IN;'
        WRITE(NUNIT,100) 'SI0.33,0.5;'
        WRITE(NUNIT,100) 'SP1;'
        NX0 = 0
        NY0 = 0
        PX0 = 1000.
        PY0 = PX0
CCCCC   NXM = 99999
CCCCC   NYM = 40000
C  SEEM TO NEED A LITTLE FUDGE FACTOR
        NXM = AXSIZE*PX0+200.5
        NYM = AYSIZE*PY0+200.5
CCCCC   PX0 = 400.
        WRITE(NUNIT,101) NYM, NXM
        IDST = 32
        CALL NEWPNT(IFORE)
C.....QMS-LASERGRAFIX
      ELSEIF (IDEVIC.EQ.21) THEN
        IF(IQMS.EQ.0)THEN
          OPEN(UNIT=NUNIT,FILE=CQMS,ERR=2029)
          IQMS=1
        ENDIF
 2029   CONTINUE
        WRITE(NUNIT,100) '^PY^-'
        WRITE(NUNIT,100) '^IOL^IMH0038010620^IMV0035008150^-'
        WRITE(NUNIT,100) '^F^IGV^PW03'
        NX0 = 0
        IF(AYSIZE.LE.7.8)THEN
          NY0 = 7800
          ASCALY=1.0
        ELSE
CCCCC     NY0=AYSIZE*1000.
          NY0=7800
          ASCALY=AYSIZE/7.8
        ENDIF
        NYM = NY0
        IF(AXSIZE.LE.10.24)THEN
          NXM = 10240
          ASCALX=1.0
        ELSE
CCCCC     NXM=AXSIZE*1000.
          NXM=10240
          ASCALX=AXSIZE/10.24
        ENDIF
CCCCC   PX0 = 1000.0/2.54
        PX0 = 1000.0/ASCALX
        PY0 = -1000.0/ASCALY
        IDST = 48
C.....POSTSCRIPT
      ELSEIF (IDEVIC.EQ.22) THEN
        IF(IPOST.EQ.0)THEN
          OPEN(UNIT=NUNIT,FILE=CPOST,ERR=2039)
          IPOST=1
        ENDIF
 2039   CONTINUE
        WRITE(NUNIT,100)
     *    '%! IDENTIFIES THE START OF A POSTSCRIPT PROGRAM'
        WRITE(NUNIT,100) 'gsave'
        WRITE(NUNIT,100) '540 0 translate 90 rotate'
        WRITE(NUNIT,100) '0 -72 translate'
        WRITE(NUNIT,100) '0.24 0.24 scale'
        WRITE(NUNIT,100) '114 105 translate'
        WRITE(NUNIT,100) '/U {moveto }def'
        WRITE(NUNIT,100) '/D {lineto }def'
        WRITE(NUNIT,100) 'newpath'
        WRITE(NUNIT,100) '1 setlinecap'
        WRITE(NUNIT,100) '1 setlinewidth'
        WRITE(NUNIT,100) '0 0 U'
        NX0 = 0
        NY0 = 0
        NXM = 3072
        NYM = 2340
CCCCC   2.54 converts to dots/per cm.  Use 1.0 to leave as dot/inch.
CCCCC   Use 1.1 as a fudge factor (account for margin, etc.)
CCCCC   PX0 = 300./2.54
        IF(AXSIZE.LE.10.24)THEN
          ASCALX=1.0
        ELSE
          ASCALX=AXSIZE/10.24
        ENDIF
        IF(AYSIZE.LE.7.8)THEN
          ASCALY=1.0
        ELSE
          ASCALY=AYSIZE/7.8
        ENDIF
        PX0 = 300./ASCALX
        PY0 = 300./ASCALY
        IDST = 0
        CALL NEWPNT(IFORE)
C.....HPGL/2 LaserJet III(ljt)
      ELSEIF (IDEVIC.EQ.23) THEN
        IF(IHPGL2.EQ.0)THEN
          OPEN(UNIT=NUNIT,FILE=CHPGL2,ERR=2049)
          IHPGL2=1
        ENDIF
 2049   CONTINUE
        WRITE(NUNIT,100) ESC//'E'
        WRITE(NUNIT,100) ESC//'%0B'
        WRITE(NUNIT,100) 'IN;'
        WRITE(NUNIT,100) 'RO90'
        WRITE(NUNIT,100) 'SI0.33,0.5;'
        WRITE(NUNIT,100) 'SP1;'
        NX0 = 0
        NY0 = 0
        IF(AYSIZE.LE.7.8)THEN
          NYM=7800
          ASCALY=1.0
        ELSE
          NYM=7800
          ASCALY=AYSIZE/(7.8+0.2)
        ENDIF
        IF(AXSIZE.LE.10.24)THEN
          NXM=10240
          ASCALX=1.0
        ELSE
          NXM=10240 
          ASCALX=AXSIZE/(10.24+0.2)
        ENDIF
        PX0 = 1000./ASCALX
        PY0 = 1000./ASCALY
CCCCC   NXM = 99999
CCCCC   NYM = 40000
CCCCC   PX0 = 400.
CCCCC   WRITE(NUNIT,101) NYM, NXM
        IDST = 32
      ENDIF
C-----------------------------------------------------------------------
C.....FOR ALL DEVICES
      PX = PX0
      PY = PY0
      XL = 0.
      YL = 0.
      FL = 1.
      IXL = NX0
      IYL = NY0
      OUTBL = .FALSE.
      RETURN

      ENTRY SETSIZ(IDCOL0,IDROW0)
C         ORIGINAL NAME SETSIZ
      JDCOL0=IDCOL0
      JDROW0=IDROW0
      RETURN

C***********************************************************************
C     CAlCOMP ROUTINE NEWPEN
C***********************************************************************
      ENTRY NEWPNT(INP)
C         ORIGINAL NAME NEWPEN
C
410   FORMAT(1X,2A1)
420   FORMAT('SP',I2,';')
430   FORMAT('^PW',I2.2)
440   FORMAT(I2,' setlinewidth')
C
      IF (NPEN.EQ.INP) RETURN
C
      LNPEN = .TRUE.
      IP0 = 3
      GOTO 1010
C
4000  NPEN = INP
      IF (IDEVIC.LT.10) THEN
        IF (2*(NPEN/2).EQ.NPEN) THEN
          IF (IDST.LT.102) IDST = IDST+8
        ELSE
          IF (IDST.GT.102) IDST = IDST-8
        ENDIF
        WRITE(NUNIT,410) ESC,CHAR(IDST)
      ELSEIF (IDEVIC.EQ.10) THEN
        IDST = MOD(IDST,16)+16*MOD(NPEN-1,16)
      ELSEIF (IDEVIC.EQ.20) THEN
        NTEMP=MOD(NPEN-1,IMAXCL)+1
        WRITE(NUNIT,420) NTEMP
CCCCC   WRITE(NUNIT,420) NPEN
      ELSEIF (IDEVIC.EQ.21) THEN
        WRITE(NUNIT,430) MOD(NPEN-1,8)*2+3
      ELSEIF (IDEVIC.EQ.22) THEN
        IF(IMAXCL.LE.1)THEN
          WRITE(NUNIT,440) MOD(NPEN-1,8)*2+1
        ELSE
          NTEMP=MOD(NPEN,16)
C
C ******************************************************
C **  FOLLOWING COLOR TABLE USED:                     **
C **       NPEN        COLOR                          **
C **         0         BLACK                          **
C **         1         RED                            **
C **         2         GREEN                          **
C **         3         YELLOW                         **
C **         4         BLUE                           **
C **         5         MAGENTA                        **
C **         6         CYAN                           **
C **         7         WHITE                          **
C **         8         YELLOW ORANGE (ORANGE)         **
C **         9         YELLOW GREEN                   **
C **        10         BLUE GREEN                     **
C **        11         GREEN BLUE                     **
C **        12         BLUE VIOLET (OR PURPLE)        **
C **        13         VIOLET RED                     **
C **        14         DARK GREY                      **
C **        15         LIGHT GREY                     **
C ******************************************************
C
          IF(NTEMP.EQ.0)TEMP(1:26)='0.   0.   0.   setrgbcolor'
          IF(NTEMP.EQ.1)TEMP(1:26)='1.   0.   0.   setrgbcolor'
          IF(NTEMP.EQ.2)TEMP(1:26)='0.   1.   0.   setrgbcolor'
          IF(NTEMP.EQ.3)TEMP(1:26)='1.   1.   0.   setrgbcolor'
          IF(NTEMP.EQ.4)TEMP(1:26)='0.   0.   1.   setrgbcolor'
          IF(NTEMP.EQ.5)TEMP(1:26)='1.   0.   1.   setrgbcolor'
          IF(NTEMP.EQ.6)TEMP(1:26)='0.   1.   1.   setrgbcolor'
          IF(NTEMP.EQ.7)TEMP(1:26)='1.   1.   1.   setrgbcolor'
          IF(NTEMP.EQ.8)TEMP(1:26)='1.   0.5  0.   setrgbcolor'
          IF(NTEMP.EQ.9)TEMP(1:26)='0.5  1.   0.   setrgbcolor'
          IF(NTEMP.EQ.10)TEMP(1:26)='0.   1.   0.5  setrgbcolor'
          IF(NTEMP.EQ.11)TEMP(1:26)='0.   0.5  1.   setrgbcolor'
          IF(NTEMP.EQ.12)TEMP(1:26)='0.5  0.   1.   setrgbcolor'
          IF(NTEMP.EQ.13)TEMP(1:26)='1.   0.   0.5  setrgbcolor'
          IF(NTEMP.EQ.14)TEMP(1:26)='0.33 0.33 0.33 setrgbcolor'
          IF(NTEMP.EQ.15)TEMP(1:26)='0.66 0.66 0.66 setrgbcolor'
          WRITE(NUNIT,'(A80)') TEMP
        ENDIF
      ELSEIF (IDEVIC.EQ.23) THEN
        WRITE(NUNIT,420) NPEN
      ELSEIF (IDEVIC.EQ.30) THEN
        IF(X11FLG.EQ.'ON')THEN
          ICODE= MOD(NPEN,65)
          CALL XFORE(ICODE)
        ENDIF
      ENDIF
      LNPEN = .FALSE.
      GOTO 1010
C***********************************************************************
*     CAlCOMP ROUTINE WHERE
C***********************************************************************
      ENTRY WHERET(RXPAGE,RYPAGE,RFACT)
C         ORIGINAL NAME WHERE
C
      RXPAGE = XL
      RYPAGE = YL
      RFACT  = FL
      RETURN
      END


      SUBROUTINE SPLOT(X1,Y1,X2,Y2,KSTAT)
C  DRAWS A LINE SEGMENT WHILE MINIMIZING PEN LIFTING.
C           ORIGINAL NAME SPLOT
      SAVE X2OLD,Y2OLD
      IF(X1.NE.X2OLD.OR.Y1.NE.Y2OLD.OR.KSTAT.EQ.0)THEN
        CALL PLOTT(X1,Y1,3)
        KSTAT=1
      ENDIF
      X2OLD=X2
      Y2OLD=Y2
      CALL PLOTT(X2,Y2,2)
      RETURN
      END
      SUBROUTINE SYMBS(X,Y,HEIGHT,STRING,ANGLE,ICOLOR)
C         ORIGINAL NAME SYMBS
C  TRANSFORMS (X,Y), COORDINATES OF CENTER OF FIRST LETTER OF STRING,
C  TO (XPAGE,YPAGE), COORDINATES OF LOWER LEFT CORNER OF FIRST LETTER.
      CHARACTER*(*) STRING
      INTEGER VGPLEN
      NCHAR=VGPLEN(STRING)
      CALL NEWPNT(ICOLOR)
      RANG=.0174533*ANGLE
      A=.707107*HEIGHT
      XPAGE=X-A*COS(RANG+.785398)
      YPAGE=Y-A*SIN(RANG+.785398)
      CALL SYMBLT(XPAGE,YPAGE,HEIGHT,STRING,ANGLE,NCHAR)
      END


      SUBROUTINE NUMBRT(X,Y,H,ANUM,THETA,IFRAC)
C
C     TO CONVERT REAL VARIABLE TO ITS APPROPRIATE FIXED DECIMAL
C     EQUIVALENT AND THEN PLOT IT.
C
C     IFRAC > 0  - NUMBER OF DIGITS TO RIGHT OF DECIMAL POINT
C     IFRAC = 0  - INTEGER PLUS DECIMAL POINT
C     IFRAC =-1  - INTEGER WITH NO DECIMAL POINT
C     IFRAC <-1  - WILL SCALE FNUM AND ROUND THE NUMBER TO AN INTEGER
C                  WITH NO DECIMAL POINT.  ANUM WILL BE DIVIDED BY
C                  POWER OF (|LFRAC| -1), I.E., MOVE DECIMAL PLACE
C                  LEFT (|LFRAC|-1) PLACES.  E.G., LFRAC=-2 AND ANUM
C                  =127.8 WILL BE PLOTTED AS 13.
C
      CHARACTER*16 STRING
      CHARACTER*16 IFRMT 
      CHARACTER*1 IQUOTE
C
      IQUOTE=CHAR(39)
C
      STRING=' '
      NUM=INT(ANUM)
      FRACT=ABS(ANUM - NUM)
C
C  CALCULATE NUMBER OF DIGITS TO LEFT OF DECIMAL POINT.
C  
      IF(ANUM.GT.0.0)THEN
        TEMP=ALOG10(ANUM)
        IF(TEMP.LE.0.1)THEN
          NLEFT=1
        ELSE
          NLEFT=TEMP+1.0
        END IF
      ELSE IF(ANUM.LT.0.0)THEN
        NLEFT=ALOG10(-AANUM)+2.0
      ELSE
        NLEFT=1
      ENDIF
C
C  LFRAC > 0 CASE.     
C
      IF(IFRAC.GT.0)THEN
        IFRMT='(I  , . ,I  )'
        IFRMT(6:6)=IQUOTE
        IFRMT(8:8)=IQUOTE
        WRITE(IFRMT(3:4),'(I2)')NLEFT
        WRITE(IFRMT(11:12),'(I2)')IFRAC
        WRITE(STRING,IFRMT) NUM, FRACT
      ELSE IF(IFRAC.EQ.0) THEN
        IFRMT='(I  , . )'
        IFRMT(6:6)=IQUOTE
        IFRMT(8:8)=IQUOTE
        WRITE(IFRMT(3:4),'(I2)')NLEFT
        WRITE(STRING,IFRMT) NUM
      ELSE IF(IFRAC.EQ.0) THEN
        IFRMT='(I  , . )'
        IFRMT(6:6)=IQUOTE
        IFRMT(8:8)=IQUOTE
        WRITE(IFRMT(3:4),'(I2)')NLEFT
        WRITE(STRING,IFRMT) NUM
      ELSE IF(IFRAC.EQ.-1)THEN
        IFRMT='(I  )'
        WRITE(IFRMT(3:4),'(I2)')NLEFT
        WRITE(STRING,IFRMT) NUM
      ELSE
        IPOWER=ABS(IFRAC) - 1
        ANUMT = ANUM/10**IPOWER
        NUM = INT(ANUMT)
        IFRMT='(I  )'
        WRITE(IFRMT(3:4),'(I2)')NLEFT
        WRITE(STRING,IFRMT) NUM
      ENDIF
C
      CALL SYMBLT(X,Y,H,STRING,THETA,N)
      RETURN
      END


      SUBROUTINE LINET (X,Y,N,INC,LINTYP,INTEQ) 
C
C     CALL LINE (XARRAY, YARRAY, NPTS, INC, LINTYP, INTEQ)
C      
C       XARRAY  IS THE NAME OF THE ARRAY CONTAINING THE VARIABLES TO BE
C               PLOTTED AS THE ABSCISSAS. 
C       YARRAY  IS THE NAME OF THE ARRAY CONTAINING THE VARIABLES TO BE
C               PLOTTED AS THE ORDINATES.                  
C       NPTS    IS THE NUMBER OF DATA POINTS TO BE PLOTTED... 
C               IF NPTS IS LESS THAN 0, A SMOOTH CURVE IS DRAWN THROUGH
C                                       THE POINTS.                   
C               IF NPTS IS GREATER THAN 0, A STRAIGHT LINE IS DRAWN  
C                                       CONNECTING DATA POINTS.     
C       INC     IS THE INCREMENT BETWEEN ELEMENTS IN THE ARRAY.    
C               INC IS GREATER THAN 1 IF THE VALUES TO BE PLOTTED ARE
C               IN A MIXED ARRAY.                                   
C       LINTYP  CONTROLS THE TYPE OF PLOT PRODUCED...              
C               IF LINTYP=0   A LINE IS PLOTTED CONNECTING SUCCESSIVE
C                             DATA POINTS.                          
C               IF LINTYP=1   A LINE PLOT WITH A SYMBOL AT EACH DATA
C                             POINT IS PRODUCED.                   
C               IF LINTYP=2   A LINE PLOT WITH A SYMBOL AT EVERY 2ND
C                             DATA POINT IS PRODUCED.              
C               IF LINTYP=N   A LINE PLOT WITH A SYMBOL AT EVERY NTH
C                             DATA POINT IS PRODUCED.              
C               IF LINTYP=-N  A SYMBOL APPEARS AT EVERY NTH DATA POINT,
C                             NO CONNECTING LINES ARE PLOTTED.      
C       INTEQ   IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE PLOTTED 
C               AT A DATA POINT.   
C                                 
C                                
      REAL X(*), Y(*)
      INTEGER N,INC,LINTYP,INTEQ
      CHARACTER*1 IBCD
C
      IF(N.LT.2)RETURN
C
C  GET THE SCALE FACTORS
C
CCCCC CALL WHERET(AX,AY,AFACT)
C 
C  ASSUME ORGIN IS 0.0, 0.0 (I.E., AX AND AY BOTH 0.0)
C  OMNITAB MOVES TO POSITION OF FIRST POINT, NOT THE ORIGIN OF
C  THE AXIS
C
      AX=0.0
      AY=0.0
      XMIN=X(N+1)
      XDELTA=X(N+2)
      YMIN=Y(N+1)
      YDELTA=Y(N+2)
C
      IF(LINTYP.EQ.0)THEN
        ITEMP=3
        XNEW=AX + (X(1)-XMIN)/XDELTA
        YNEW=AY + (Y(1)-YMIN)/YDELTA
        CALL PLOTT(XNEW,YNEW,ITEMP)
        ITEMP=2
        INEXT=1+INC
        IF(INEXT.GT.N)RETURN
        DO100I=INEXT,N,INC
          XNEW=AX + (X(I)-XMIN)/XDELTA
          YNEW=AY + (Y(I)-YMIN)/YDELTA
          CALL PLOTT(XNEW,YNEW,ITEMP)
 100    CONTINUE
      ELSE
        ITEMP=3
        XNEW=AX + (X(1)-XMIN)/XDELTA
        YNEW=AY + (Y(1)-YMIN)/YDELTA
        CALL PLOTT(XNEW,YNEW,ITEMP)
        IF(LINTYP.GT.0)THEN
          ITEMP=2
          INEXT=1+INC
          IF(INEXT.GT.N)RETURN
          DO200I=INEXT,N,INC
            XNEW=AX + (X(I)-XMIN)/XDELTA
            YNEW=AY + (Y(I)-YMIN)/YDELTA
            CALL PLOTT(XNEW,YNEW,ITEMP)
 200      CONTINUE
        ENDIF
        INCT=IABS(LINTYP)*INC
        HEIGHT=0.14
        ANGLE=0.0
        NCHAR=1
        IBCD=CHAR(INTEQ)
        DO300I=1,N,INCT
          XNEW=AX + (X(I)-XMIN)/XDELTA
          YNEW=AY + (Y(I)-YMIN)/YDELTA
          CALL SYMBLT(XNEW,YNEW,HEIGHT,IBCD,ANGLE,NCHAR)
  300   CONTINUE
      ENDIF
C
      RETURN
      END  


      SUBROUTINE SYMBLT(XPAGE,YPAGE,HEIGHT,IBCD,ANGLE,NCHAR)
C          ORIGINAL NAME SYMBLS
C***********************************************************************
C*     SUBROUTINE SYMBOL
C*     TO EMULATE CAlCOMP ROUTINE SYMBOL, STANDARD CALL
C*         U. KATTNER NBS, 1 SEPTEMBER 1987  -  VERSION 1.0
C*         MODIFIED 4/12/89  D. KAHANER
C*         COMMON VGSSIZ ADDED 5 JULY 89 (DRA)
C***********************************************************************
      CHARACTER IBCD*(*)
      INTEGER NCHAR, I,ICHR,ICNT,IL,J
      REAL XPAGE,YPAGE,HEIGHT,ANGLE, PI2
      REAL ALPHA,CH,COSA,HCOS,HSIN,SINA,X,X0,Y,Y0, ARRAY(17),DUMMY(1)
      PARAMETER (PI2=6.283185)

      COMMON/VGSSIZ/SUBSZ,SUPRSZ
      SAVE X0,Y0,/VGSSIZ/
C
200   FORMAT(I3)
C
      IF (XPAGE.NE.999.0.AND.YPAGE.NE.999.0) THEN
        X0 = XPAGE
        Y0 = YPAGE
      ENDIF
C
      ALPHA = PI2*AMOD(ANGLE,360.0)/360.0
      IF (ALPHA.LT.0.) ALPHA = PI2+ALPHA
      COSA = COS(ALPHA)
      SINA = SIN(ALPHA)
      HCOS = 0.
      HSIN = 0.
C
CCC      IL = LEN(IBCD)
CCC   IF (IL.GT.NCHAR) IL = NCHAR
      IL=NCHAR
      IF (IL.EQ.0)RETURN
C
      CALL DASHST(ARRAY,-16)
      ICNT = NINT(ARRAY(17))
      IF (ICNT.NE.0) CALL DASHST(DUMMY,0)
C
      J = 0
      DO 2000 I = 1,IL
      CH = HEIGHT
      X = X0
      Y = Y0
1000  J = J+1
      ICHR = ICHAR(IBCD(J:J))
      IF (ICHR.EQ.95) THEN
C....._ : NEXT CHARACTER IS A SUBSCRIPT
        CH = SUBSZ*CH
        X = X+0.5*CH*SINA
        Y = Y-0.5*CH*COSA
        GOTO 1000
CCC                DKK CHANGED FROM 63 (?) TO 92 (\)
      ELSEIF (ICHR.EQ.92) THEN
C.....\ : NEXT 3 CHARACTERS ARE THE CODE NUMBER OF A CHARACTER
        J = J+1
        READ(IBCD(J:),200,ERR=9000) ICHR
        J = J+2
      ELSEIF (ICHR.EQ.94) THEN
C.....^ : NEXT CHARACTER IS A SUPERSCRIPT
        CH = SUPRSZ*CH
        X = X-0.9*CH*SINA
        Y = Y+0.9*CH*COSA
        GOTO 1000
      ELSEIF (ICHR.EQ.124) THEN
C.....| : NEXT CHARACTER IS FROM THE EXTENDED CHARACTER SET
C            UNLESS IT IS _,^ THEN PRINT ITSELF.
        J = J+1
        ICHR=ICHAR(IBCD(J:J))
        IF(ICHR.EQ.95)THEN
C           DRAW _ A BIT SMALLER (MAGIC NUMBER DKK 4/12/89)
           CH = 0.5*CH
        ELSEIF(ICHR.EQ.94)THEN
C           DRAW ^ A BIT SMALLER & RAISE IT SOME (MAGIC NUMBER DKK 4/12/89)
           CH = 0.6*CH
           X = X-0.6*CH*SINA
           Y = Y+0.6*CH*COSA 
        ELSE
C           USE EXTENDED CHARACTER
           ICHR = ICHR+128
        ENDIF
      ENDIF
      IF (ICHR.EQ.127 .OR. ICHR.EQ.160) THEN
CCCC.....BACKSPACE
        X0 = X0-HCOS
        Y0 = Y0-HSIN
        GOTO 2000
      ENDIF
C
      CALL SYMBL2(X,Y,CH,ICHR,ANGLE,-1)
      HCOS = CH*COSA
      HSIN = CH*SINA
      X0 = X0+HCOS
      Y0 = Y0+HSIN
2000  CONTINUE
C
9000  IF (ICNT.NE.0) CALL DASHST(ARRAY,ICNT)
      RETURN
      END

      SUBROUTINE SYMBL2(XPAGE,YPAGE,HEIGHT,INTEQ,ANGLE,ICODE)
C***********************************************************************
C*       ORIGINAL NAME SYMBOL
C*     TO EMULATE CAlCOMP ROUTINE SYMBOL, SPECIAL CALL
C*     URSULA KATTNER, NBS, 26 SEPTEMBER 1987  -  VERSION 1.0
C*     DAVID KAHANER,  NBS, 25 MAY 1988
C*     CHARACTER FONT IS STORED IN AN ARRAY OF CHARACTER*1 FONT(SIZE)
C**********************************************************************
      INTEGER INTEQ,ICODE, ICNT,IP,IPEN,IX,IY,NX,NY
      REAL XPAGE,YPAGE,HEIGHT,ANGLE, PI2
      REAL ALPHA,COSA,CX,CY,HCOS,HCOS4,HSIN,HSIN4,SINA,X,Y,X0,Y0
      REAL RARRAY(17),DUMMY(1)
      CHARACTER FONT(5624)
      INTEGER IFPTR(0:255)
      PARAMETER (PI2=6.283185)
C
      COMMON/VGFNT/ FONT
      COMMON/VGFPT/ IFPTR
C
      SAVE X0,Y0
      SAVE /VGFNT/,/VGFPT/
C
      IF (XPAGE.NE.999.0.AND.YPAGE.NE.999.0) THEN
        X0 = XPAGE
        Y0 = YPAGE
      ENDIF
C
C.....ICODE = -1 : "PEN UP"; ICODE = -2 : "PEN DOWN"
C
      IF (ICODE.EQ.-2) CALL PLOTT(X0,Y0,4+ICODE)
C
      IF (IFPTR(INTEQ).EQ.0) RETURN
C
      CALL DASHST(RARRAY,-16)
      ICNT = NINT(RARRAY(17))
      IF (ICNT.NE.0) CALL DASHST(DUMMY,0)
C
      ALPHA = PI2*AMOD(ANGLE,360.0)/360.0
      IF (ALPHA.LT.0.) ALPHA = PI2+ALPHA
      COSA = COS(ALPHA)
      SINA = SIN(ALPHA)
      HCOS = HEIGHT*COSA
      HSIN = HEIGHT*SINA
      HCOS4 = 0.043*HCOS
      HSIN4 = 0.043*HSIN
C
      IF (INTEQ.EQ.127.OR.INTEQ.EQ.174.OR.INTEQ.EQ.223) THEN
        X0 = X0-HCOS
        Y0 = Y0-HSIN
      ENDIF
C
      IF (INTEQ.LT.32) THEN
        NX = 64
        NY = 64
      ELSE
        NX = 64-11
        NY = 64-9
      ENDIF
C
      IP = IFPTR(INTEQ)
1000  IPEN = 3
1010  IP = IP+2
      IX = ICHAR(FONT(IP))
      IY = ICHAR(FONT(IP+1))
      IF (IX.EQ.126) GOTO 1100
      CX = FLOAT(IX-NX)
      CY = FLOAT(IY-NY)
      X = CX*HCOS4-CY*HSIN4+X0
      Y = CY*HCOS4+CX*HSIN4+Y0
      CALL PLOTT(X,Y,IPEN)
      IPEN = 2
      GOTO 1010
1100  IF (IY.NE.126) GOTO 1000
C
      IF (INTEQ.LT.32.OR.INTEQ.EQ.127) GOTO 2000
      X0 = X0+HCOS
      Y0 = Y0+HSIN
2000  CALL PLOTT(X0,Y0,3)
C
      IF (ICNT.NE.0) CALL DASHST(RARRAY,ICNT)
      RETURN
      END


      SUBROUTINE DASHST(ARRAY,ICNT)
C***********************************************************************
C*     ORIGINAL NAME DASHS
C*     TO EMULATE CAlCOMP ROUTINE DASHS
C*       U. KATTNER NBS, 1 SEPTEMBER 1987  -  VERSION 1.0
C***********************************************************************
      INTEGER ICNT, IDEVIC,NPEN, I
      REAL ARRAY(*), X,Y,F
      INTEGER JTEK(0:4),JPC(0:8)
      CHARACTER JHP(0:6)*1,JQMS(0:15)*1,JPSC(0:4)*13
      CHARACTER*4 X11FLG
CCCCC INTEGER*4 IDSPLY(80)
CCCCC INTEGER*4 IDST
      INTEGER IDSPLY(80)
      INTEGER IDST
C
      COMMON/VGPNF/ IDEVIC,IDST,NPEN,NUNIT
      COMMON/VGX11/IDSPLY
      COMMON/VGX112/X11FLG
      SAVE I,/VGPNF/
C
      DATA JTEK/96,97,99,98,100/
      DATA JPC / 1, 2, 6, 3,  4, 5, 7, 8, 9/
CCCCC DATA JHP /' ','1','2','4', '3','5','6'/
      DATA JHP /' ','2','1','4', '3','5','6'/
      DATA JQMS/'0','3','6','8',
     &          '1','2','4','5','7','9','A','B','C','D','E','F'/
      DATA JPSC/'[]','[12 12]','[60 12]','[60 12 12 12]', '[36 24]'/
C
10    FORMAT(2A1)
11    FORMAT(1X,2A1)
20    FORMAT('LT',A1,',',A1,';')
30    FORMAT('^V',A1)
40    FORMAT(A14,' 0 setdash')
C
C-----------------------------------------------------------------------
C     DEFAULT DASH STYLES OF THE DEVICES ARE USED.
C     STANDARD:  0 = SOLID
C                1 = DOTTED
C                2 = DASHED
C                3 = CHAIN-DOTTED
C     FOR HIGHER NUMBERS, THE DASH STYLE DEPENDS ON THE DEVICE.
C-----------------------------------------------------------------------
C
      IF (ICNT.LT.0) THEN
        ARRAY(1-ICNT) = FLOAT(I)
        RETURN
      ENDIF
C
      IF (ICNT.EQ.I) RETURN
C
      CALL WHERET(X,Y,F)
      CALL PLOTT(X,Y,3)
      IF (IDEVIC.LT.10) THEN
        I = MOD(ICNT,5)
        IDST = JTEK(I)
        IF (2*(NPEN/2).EQ.NPEN) IDST = IDST+8
        WRITE(NUNIT,11) CHAR(27),CHAR(IDST)
      ELSEIF (IDEVIC.LT.20) THEN
        I = MOD(ICNT,9)
        IDST = JPC(I)+16*(IDST/16)
      ELSEIF (IDEVIC.EQ.20) THEN
        I = MOD(ICNT,7)
        WRITE(NUNIT,20) JHP(I),JHP(I)
      ELSEIF (IDEVIC.EQ.21) THEN
        I = MOD(ICNT,16)
        WRITE(NUNIT,30) JQMS(I)
      ELSEIF (IDEVIC.EQ.22) THEN
        I = MOD(ICNT,5)
        WRITE(NUNIT,40) JPSC(I)
      ELSEIF (IDEVIC.EQ.23) THEN
        I = MOD(ICNT,7)
        WRITE(NUNIT,20) JHP(I),JHP(I)
      ELSEIF (IDEVIC.EQ.30) THEN
        IF(X11FLG.EQ.'ON')THEN
          I = MOD(ICNT,4)
          ICODE=2
          CALL XLATTR(I,ICODE)
        ENDIF
      ENDIF
      CALL PLOTT(X,Y,3)
C
      RETURN
      END

      INTEGER FUNCTION VGISTR(C)
C   COMPUTES THE POSITION OF THE LAST NONBLANK CHARACTER OF A STRING.
      CHARACTER C*(*)
      INTEGER I
      DO 100 I=LEN(C),1,-1
          IF(C(I:I).NE.' ')GOTO 200
 100  CONTINUE
      VGISTR=0
      RETURN
 200  VGISTR=I
      RETURN
      END

       INTEGER FUNCTION VGPLEN(STRING)
C  COMPUTES STRING PRINTING LENGTH, IE, LENGTH AFTER THROWING OUT
C  TRAILING BLANKS AND CONTROL CHARACTERS
      INTEGER VGISTR
      CHARACTER STRING*(*),CH*1
      LEN1=VGISTR(STRING)
      VGPLEN=LEN1
      DO 10 I=1,LEN1
        CH=STRING(I:I)
        IF(CH.EQ.'_'.OR.CH.EQ.'^'.OR.CH.EQ.'|'.OR.CH.EQ.'\\')THEN
           VGPLEN=VGPLEN-1
C                REDUCE COUNT BY TWO MORE IF CHARACTER CODE FOLLOWS
           IF(CH.EQ.'\\')VGPLEN=VGPLEN-2
C             DON'T COUNT SPECIAL CHARACTERS |, _, ^, UNLESS PRECEDED BY |,
C                  THEN THEY ARE TO BE PRINTED.
           IF(I.GT.1 .AND. (STRING(I-1:I-1).EQ.'|'))THEN
               VGPLEN=VGPLEN+1
C                  '||' AND SPECIAL CHARACTER
               IF(I.GT.2 .AND. STRING(I-2:I-2).EQ.'|') VGPLEN=VGPLEN-1
           ENDIF
        ENDIF
 10   CONTINUE
      RETURN 
      END

      BLOCK DATA FINIT
C***********************************************************************
C     INITIALIZE CHARACTER FONT FROM THE HERSHEY FONTS
C     URSULA KATTNER, NBS, 6 OCTOBER 1987  -  VERSION 1.0
C     MODIFIED BY G. CANDELA & D. KAHANER
C***********************************************************************
      CHARACTER F1(100)
      CHARACTER F2(100)
      CHARACTER F3(100)
      CHARACTER F4(100)
      CHARACTER F5(100)
      CHARACTER F6(100)
      CHARACTER F7(100)
      CHARACTER F8(100)
      CHARACTER F9(100)

      CHARACTER F10(100)
      CHARACTER F11(100)
      CHARACTER F12(100)
      CHARACTER F13(100)
      CHARACTER F14(100)
      CHARACTER F15(100)
      CHARACTER F16(100)
      CHARACTER F17(100)
      CHARACTER F18(100)

      CHARACTER F19(100)
      CHARACTER F20(100)
      CHARACTER F21(100)
      CHARACTER F22(100)
      CHARACTER F23(100)
      CHARACTER F24(100)
      CHARACTER F25(100)
      CHARACTER F26(100)
      CHARACTER F27(100)

      CHARACTER F28(100)
      CHARACTER F29(100)
      CHARACTER F30(100)
      CHARACTER F31(100)
      CHARACTER F32(100)
      CHARACTER F33(100)
      CHARACTER F34(100)
      CHARACTER F35(100)
      CHARACTER F36(100)

      CHARACTER F37(100)
      CHARACTER F38(100)
      CHARACTER F39(100)
      CHARACTER F40(100)
      CHARACTER F41(100)
      CHARACTER F42(100)
      CHARACTER F43(100)
      CHARACTER F44(100)
      CHARACTER F45(100)
      CHARACTER F46(100)

      CHARACTER F47(100)
      CHARACTER F48(100)
      CHARACTER F49(100)
      CHARACTER F50(100)
      CHARACTER F51(100)
      CHARACTER F52(100)
      CHARACTER F53(100)
      CHARACTER F54(100)
      CHARACTER F55(100)
      CHARACTER F56(100)
      CHARACTER F57(24)

      INTEGER IFPTR(256)
      INTEGER  J

      COMMON/VGFNT/F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,
     *            F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,
     *            F21,F22,F23,F24,F25,F26,F27,F28,F29,F30,
     *            F31,F32,F33,F34,F35,F36,F37,F38,F39,F40,
     *            F41,F42,F43,F44,F45,F46,F47,F48,F49,F50,
     *            F51,F52,F53,F54,F55,F56,F57
      COMMON/VGFPT/ IFPTR
      SAVE /VGFNT/,/VGFPT/

      DATA (IFPTR(J),J=1,128)
     &/   1,  39,  53,  65,  77,  91, 117, 131, 145, 165, 195
     &, 217, 265, 297, 323, 359, 395, 467, 523, 559, 595, 633,5611,   0
     &,   0,   0,   0,   0,   0,   0,   0, 679, 763, 767, 787, 801, 827
     &, 883, 949,1021,1029,1053,1077,1097,1111,1131,1139,1153,1161,1199
     &,1211,1243,1277,1293,1331,1381,1395,1457,1507,1533,1565,1575,1589
     &,1599,1643,1731,1751,1801,1841,1875,1901,1921,1969,1989,1997,2021
     &,2041,2055,2081,2101,2147,2177,2229,2265,2309,2323,2347,2361,2387
     &,2401,2417,2437,2449,2457,2469,2483,2491,2499,2537,2575,2607,2645
     &,2683,2703,2751,2775,2795,2821,2841,2849,2889,2913,2951,2989,3027
     &,3047,3085,3105,3129,3143,3169,3183,3205,3225,3271,3279,3325,   0
     &/
      DATA (IFPTR(J),J=129,256)
     &/   1,  39,  53,  65,  77,  91, 117, 131, 145, 165, 195, 217, 265
     &, 297, 323, 359, 395, 467, 523, 559, 595, 633,5611,   0,   0,   0
     &,   0,   0,   0,   0,   0, 679,   0,   0,   0,3349,3369,3423,   0
     &,3455,3485,3501,3517,3571,   0,   0,3591,3617,   0,   0,   0,   0
     &,   0,   0,   0,   0,   0,   0,3629,3647,3677,3693,3713,1599,3729
     &,3791,3811,3861,3881,3901,3927,3971,3985,3999,4007,4043,4063,4077
     &,4103,4123,4169,4189,4241,4271,4293,4307,4347,4383,4419,4439,4477
     &,   0,2449,   0,   0,4497,2491,4505,4555,4619,4659,4709,4749,4793
     &,4829,4859,4879,4933,4973,4993,5037,5067,5105,5133,5189,5229,5267
     &,5285,5319,5367,5415,5475,5517,   0,3271,   0,5565,   0/
C
      DATA(F1(J),J=1,100)/'9','G','?','G','<','F',':','D','9','A',
     &'9','?',':','<','<',':','?','9','A','9',
     &'D',':','F','<','G','?','G','A','F','D',
     &'D','F','A','G','?','G','~','~',':','F',
     &':','F',':',':','F',':','F','F',':','F',
     &'~','~','9','G','@','H','9','<','G','<',
     &'@','H','~','~','9','G','@','8','9','D',
     &'G','D','@','8','~','~',':','F','@','J',
     &':','@','@','6','F','@','@','J','~','~',
     &'8','H','@','I','>','C','8','C','=','?'/
      DATA(F2(J),J=1,100)/';','9','@','=','E','9','C','?','H','C',
     &'B','C','@','I','~','~','9','G','@','G',
     &'@','9','~','>','9','@','G','@','~','~',
     &';','E',';','E','E',';','~','>','E','E',
     &';',';','~','~',';','E','@','F','@',':',
     &'~','>',';','C','E','=','~','>','E','C',
     &';','=','~','~',':','F','>','F','>','B',
     &':','B',':','>','>','>','>',':','B',':',
     &'B','>','F','>','F','B','B','B','B','F',
     &'>','F','~','~','9','G','@','H','9','<'/
      DATA(F3(J),J=1,100)/'G','<','@','H','~','>','@','8','G','D',
     &'9','D','@','8','~','~','<','D','?','D',
     &'=','C','<','A','<','?','=','=','?','<',
     &'A','<','C','=','D','?','D','A','C','C',
     &'A','D','?','D','~','>','=','A','=','?',
     &'~','>','>','B','>','>','~','>','?','C',
     &'?','=','~','~','<','D','<','D','<','<',
     &'D','<','D','D','<','D','~','>','=','C',
     &'=','=','~','>','>','C','>','=','~','>',
     &'?','C','?','=','~','~',';','E','@','E'/
      DATA(F4(J),J=1,100)/';','@','@',';','E','@','@','E','~','>',
     &'<','@','D','@','~','>','@','D','@','<',
     &'~','~',':','C',':','@','C',';','C','E',
     &':','@','~','>','=','@','B','=','~','>',
     &'=','@','B','C','~','>','@','@','B','?',
     &'~','>','@','@','B','A','~','~','=','F',
     &'F','@','=','E','=',';','F','@','~','>',
     &'C','@','>','C','~','>','C','@','>','=',
     &'~','>','@','@','>','A','~','>','@','@',
     &'>','?','~','~','<','D','?','D','=','C'/
      DATA(F5(J),J=1,100)/'<','A','<','?','=','=','?','<','A','<',
     &'C','=','D','?','D','A','C','C','A','D',
     &'?','D','~','>','=','A','=','?','~','>',
     &'>','B','>','>','~','>','?','C','?','=',
     &'~','>','@','C','@','=','~','>','A','C',
     &'A','=','~','>','B','B','B','>','~','>',
     &'C','A','C','?','~','~','<','D','<','D',
     &'<','<','D','<','D','D','<','D','~','>',
     &'=','C','=','=','~','>','>','C','>','=',
     &'~','>','?','C','?','=','~','>','@','C'/
      DATA(F6(J),J=1,100)/'@','=','~','>','A','C','A','=','~','>',
     &'B','C','B','=','~','>','C','C','C','=',
     &'~','~',';','E','@','F',';','=','E','=',
     &'@','F','~','>','@','C','=','>','~','>',
     &'@','C','C','>','~','>','@','@','?','>',
     &'~','>','@','@','A','>','~','~',';','E',
     &'@',':','E','C',';','C','@',':','~','>',
     &'@','=','C','B','~','>','@','=','=','B',
     &'~','>','@','@','A','B','~','>','@','@',
     &'?','B','~','~',';','E','@','E',';','@'/
      DATA(F7(J),J=1,100)/'@',';','E','@','@','E','~','>','<','?',
     &'A','D','~','>','=','>','B','C','~','>',
     &'>','=','C','B','~','>','?','<','D','A',
     &'~','~',':','F','@','F','<',';','F','B',
     &':','B','D',';','@','F','~','>','@','@',
     &'@','F','~','>','@','@',':','B','~','>',
     &'@','@','<',';','~','>','@','@','D',';',
     &'~','>','@','@','F','B','~','~','5','K',
     &'>','K',';','J','8','H','6','E','5','B',
     &'5','>','6',';','8','8',';','6','>','5'/
      DATA(F8(J),J=1,100)/'B','5','E','6','H','8','J',';','K','>',
     &'K','B','J','E','H','H','E','J','B','K',
     &'>','K','~','>',';','E',':','D',';','C',
     &'<','D',';','E','~','>','E','E','D','D',
     &'E','C','F','D','E','E','~','>',':','<',
     &'<',':','?','9','A','9','D',':','F','<',
     &'~','~','8','H','~','~',';','E','@','L',
     &'@','>','~','>','@','9','?','8','@','7',
     &'A','8','@','9','~','~','8','H','<','L',
     &'<','E','~','>','D','L','D','E','~','~'/
      DATA(F9(J),J=1,100)/'6','K','A','L',':','0','~','>','G','L',
     &'@','0','~','>',':','A','H','A','~','>',
     &'9',';','G',';','~','~','6','J','>','P',
     &'>','3','~','>','B','P','B','3','~','>',
     &'G','I','E','K','B','L','>','L',';','K',
     &'9','I','9','G',':','E',';','D','=','C',
     &'C','A','E','@','F','?','G','=','G',':',
     &'E','8','B','7','>','7',';','8','9',':',
     &'~','~','4','L','I','L','7','7','~','>',
     &'<','L','>','J','>','H','=','F',';','E'/
      DATA(F10(J),J=1,100)/'9','E','7','G','7','I','8','K',':','L',
     &'<','L','>','K','A','J','D','J','G','K',
     &'I','L','~','>','E','>','C','=','B',';',
     &'B','9','D','7','F','7','H','8','I',':',
     &'I','<','G','>','E','>','~','~','3','M',
     &'J','C','J','D','I','E','H','E','G','D',
     &'F','B','D','=','B',':','@','8','>','7',
     &':','7','8','8','7','9','6',';','6','=',
     &'7','?','8','@','?','D','@','E','A','G',
     &'A','I','@','K','>','L','<','K',';','I'/
C
      DATA(F11(J),J=1,100)/';','G','<','D','>','A','C',':','E','8',
     &'G','7','I','7','J','8','J','9','~','~',
     &'<','D','A','L','?','E','~','~','9','G',
     &'D','P','B','N','@','K','>','G','=','B',
     &'=','>','>','9','@','5','B','2','D','0',
     &'~','~','9','G','<','P','>','N','@','K',
     &'B','G','C','B','C','>','B','9','@','5',
     &'>','2','<','0','~','~','8','H','@','F',
     &'@',':','~','>',';','C','E','=','~','>',
     &'E','C',';','=','~','~','3','M','@','I'/
      DATA(F12(J),J=1,100)/'@','7','~','>','7','@','I','@','~','~',
     &';','E','A','8','@','7','?','8','@','9',
     &'A','8','A','6','@','4','?','3','~','~',
     &'3','M','7','@','I','@','~','~',';','E',
     &'@','9','?','8','@','7','A','8','@','9',
     &'~','~','5','K','I','P','7','0','~','~',
     &'6','J','?','L','<','K',':','H','9','C',
     &'9','@',':',';','<','8','?','7','A','7',
     &'D','8','F',';','G','@','G','C','F','H',
     &'D','K','A','L','?','L','~','~','6','J'/
      DATA(F13(J),J=1,100)/'<','H','>','I','A','L','A','7','~','~',
     &'6','J',':','G',':','H',';','J','<','K',
     &'>','L','B','L','D','K','E','J','F','H',
     &'F','F','E','D','C','A','9','7','G','7',
     &'~','~','6','J',';','L','F','L','@','D',
     &'C','D','E','C','F','B','G','?','G','=',
     &'F',':','D','8','A','7','>','7',';','8',
     &':','9','9',';','~','~','6','J','C','L',
     &'9','>','H','>','~','>','C','L','C','7',
     &'~','~','6','J','E','L',';','L',':','C'/
      DATA(F14(J),J=1,100)/';','D','>','E','A','E','D','D','F','B',
     &'G','?','G','=','F',':','D','8','A','7',
     &'>','7',';','8',':','9','9',';','~','~',
     &'6','J','F','I','E','K','B','L','@','L',
     &'=','K',';','H',':','C',':','>',';',':',
     &'=','8','@','7','A','7','D','8','F',':',
     &'G','=','G','>','F','A','D','C','A','D',
     &'@','D','=','C',';','A',':','>','~','~',
     &'6','J','G','L','=','7','~','>','9','L',
     &'G','L','~','~','6','J','>','L',';','K'/
      DATA(F15(J),J=1,100)/':','I',':','G',';','E','=','D','A','C',
     &'D','B','F','@','G','>','G',';','F','9',
     &'E','8','B','7','>','7',';','8',':','9',
     &'9',';','9','>',':','@','<','B','?','C',
     &'C','D','E','E','F','G','F','I','E','K',
     &'B','L','>','L','~','~','6','J','F','E',
     &'E','B','C','@','@','?','?','?','<','@',
     &':','B','9','E','9','F',':','I','<','K',
     &'?','L','@','L','C','K','E','I','F','E',
     &'F','@','E',';','C','8','@','7','>','7'/
      DATA(F16(J),J=1,100)/';','8',':',':','~','~',';','E','@','E',
     &'?','D','@','C','A','D','@','E','~','>',
     &'@','9','?','8','@','7','A','8','@','9',
     &'~','~',';','E','@','E','?','D','@','C',
     &'A','D','@','E','~','>','A','8','@','7',
     &'?','8','@','9','A','8','A','6','@','4',
     &'?','3','~','~','4','L','H','I','8','@',
     &'H','7','~','~','3','M','7','C','I','C',
     &'~','>','7','=','I','=','~','~','4','L',
     &'8','I','H','@','8','7','~','~','7','I'/
      DATA(F17(J),J=1,100)/':','G',':','H',';','J','<','K','>','L',
     &'B','L','D','K','E','J','F','H','F','F',
     &'E','D','D','C','@','A','@','>','~','>',
     &'@','9','?','8','@','7','A','8','@','9',
     &'~','~','3','N','E','D','D','F','B','G',
     &'?','G','=','F','<','E',';','B',';','?',
     &'<','=','>','<','A','<','C','=','D','?',
     &'~','>','E','G','D','?','D','=','F','<',
     &'H','<','J','>','K','A','K','C','J','F',
     &'I','H','G','J','E','K','B','L','?','L'/
      DATA(F18(J),J=1,100)/'<','K',':','J','8','H','7','F','6','C',
     &'6','@','7','=','8',';',':','9','<','8',
     &'?','7','B','7','E','8','G','9','~','~',
     &'7','I','@','L','8','7','~','>','@','L',
     &'H','7','~','>',';','>','E','>','~','~',
     &'5','J','9','L','9','7','~','>','9','L',
     &'B','L','E','K','F','J','G','H','G','F',
     &'F','D','E','C','B','B','~','>','9','B',
     &'B','B','E','A','F','@','G','>','G',';',
     &'F','9','E','8','B','7','9','7','~','~'/
      DATA(F19(J),J=1,100)/'6','K','H','G','G','I','E','K','C','L',
     &'?','L','=','K',';','I',':','G','9','D',
     &'9','?',':','<',';',':','=','8','?','7',
     &'C','7','E','8','G',':','H','<','~','~',
     &'5','J','9','L','9','7','~','>','9','L',
     &'@','L','C','K','E','I','F','G','G','D',
     &'G','?','F','<','E',':','C','8','@','7',
     &'9','7','~','~','6','I',':','L',':','7',
     &'~','>',':','L','G','L','~','>',':','B',
     &'B','B','~','>',':','7','G','7','~','~'/
      DATA(F20(J),J=1,100)/'6','H',':','L',':','7','~','>',':','L',
     &'G','L','~','>',':','B','B','B','~','~',
     &'6','K','H','G','G','I','E','K','C','L',
     &'?','L','=','K',';','I',':','G','9','D',
     &'9','?',':','<',';',':','=','8','?','7',
     &'C','7','E','8','G',':','H','<','H','?',
     &'~','>','C','?','H','?','~','~','5','K',
     &'9','L','9','7','~','>','G','L','G','7',
     &'~','>','9','B','G','B','~','~','<','D',
     &'@','L','@','7','~','~','8','H','D','L'/
C
      DATA(F21(J),J=1,100)/'D','<','C','9','B','8','@','7','>','7',
     &'<','8',';','9',':','<',':','>','~','~',
     &'5','J','9','L','9','7','~','>','G','L',
     &'9','>','~','>','>','C','G','7','~','~',
     &'6','G',':','L',':','7','~','>',':','7',
     &'F','7','~','~','4','L','8','L','8','7',
     &'~','>','8','L','@','7','~','>','H','L',
     &'@','7','~','>','H','L','H','7','~','~',
     &'5','K','9','L','9','7','~','>','9','L',
     &'G','7','~','>','G','L','G','7','~','~'/
      DATA(F22(J),J=1,100)/'5','K','>','L','<','K',':','I','9','G',
     &'8','D','8','?','9','<',':',':','<','8',
     &'>','7','B','7','D','8','F',':','G','<',
     &'H','?','H','D','G','G','F','I','D','K',
     &'B','L','>','L','~','~','5','J','9','L',
     &'9','7','~','>','9','L','B','L','E','K',
     &'F','J','G','H','G','E','F','C','E','B',
     &'B','A','9','A','~','~','5','K','>','L',
     &'<','K',':','I','9','G','8','D','8','?',
     &'9','<',':',':','<','8','>','7','B','7'/
      DATA(F23(J),J=1,100)/'D','8','F',':','G','<','H','?','H','D',
     &'G','G','F','I','D','K','B','L','>','L',
     &'~','>','A',';','G','5','~','~','5','J',
     &'9','L','9','7','~','>','9','L','B','L',
     &'E','K','F','J','G','H','G','F','F','D',
     &'E','C','B','B','9','B','~','>','@','B',
     &'G','7','~','~','6','J','G','I','E','K',
     &'B','L','>','L',';','K','9','I','9','G',
     &':','E',';','D','=','C','C','A','E','@',
     &'F','?','G','=','G',':','E','8','B','7'/
      DATA(F24(J),J=1,100)/'>','7',';','8','9',':','~','~','8','H',
     &'@','L','@','7','~','>','9','L','G','L',
     &'~','~','5','K','9','L','9','=',':',':',
     &'<','8','?','7','A','7','D','8','F',':',
     &'G','=','G','L','~','~','7','I','8','L',
     &'@','7','~','>','H','L','@','7','~','~',
     &'4','L','6','L',';','7','~','>','@','L',
     &';','7','~','>','@','L','E','7','~','>',
     &'J','L','E','7','~','~','6','J','9','L',
     &'G','7','~','>','G','L','9','7','~','~'/
      DATA(F25(J),J=1,100)/'7','I','8','L','@','B','@','7','~','>',
     &'H','L','@','B','~','~','6','J','G','L',
     &'9','7','~','>','9','L','G','L','~','>',
     &'9','7','G','7','~','~','9','G','D','P',
     &'=','P','=','0','D','0','~','~','5','K',
     &'7','P','I','0','~','~','9','G','<','P',
     &'C','P','C','0','<','0','~','~','8','H',
     &'@','N','8','@','~','>','@','N','H','@',
     &'~','~','4','L','4','4','L','4','~','~',
     &'<','D','?','L','A','E','~','~','7','J'/
      DATA(F26(J),J=1,100)/'F','E','F','7','~','>','F','B','D','D',
     &'B','E','?','E','=','D',';','B',':','?',
     &':','=',';',':','=','8','?','7','B','7',
     &'D','8','F',':','~','~','6','I',':','L',
     &':','7','~','>',':','B','<','D','>','E',
     &'A','E','C','D','E','B','F','?','F','=',
     &'E',':','C','8','A','7','>','7','<','8',
     &':',':','~','~','7','I','F','B','D','D',
     &'B','E','?','E','=','D',';','B',':','?',
     &':','=',';',':','=','8','?','7','B','7'/
      DATA(F27(J),J=1,100)/'D','8','F',':','~','~','7','J','F','L',
     &'F','7','~','>','F','B','D','D','B','E',
     &'?','E','=','D',';','B',':','?',':','=',
     &';',':','=','8','?','7','B','7','D','8',
     &'F',':','~','~','7','I',':','?','F','?',
     &'F','A','E','C','D','D','B','E','?','E',
     &'=','D',';','B',':','?',':','=',';',':',
     &'=','8','?','7','B','7','D','8','F',':',
     &'~','~',';','G','E','L','C','L','A','K',
     &'@','H','@','7','~','>','=','E','D','E'/
      DATA(F28(J),J=1,100)/'~','~','7','J','F','E','F','5','E','2',
     &'D','1','B','0','?','0','=','1','~','>',
     &'F','B','D','D','B','E','?','E','=','D',
     &';','B',':','?',':','=',';',':','=','8',
     &'?','7','B','7','D','8','F',':','~','~',
     &'7','J',';','L',';','7','~','>',';','A',
     &'>','D','@','E','C','E','E','D','F','A',
     &'F','7','~','~','<','D','?','L','@','K',
     &'A','L','@','M','?','L','~','>','@','E',
     &'@','7','~','~',';','E','@','L','A','K'/
      DATA(F29(J),J=1,100)/'B','L','A','M','@','L','~','>','A','E',
     &'A','4','@','1','>','0','<','0','~','~',
     &'7','H',';','L',';','7','~','>','E','E',
     &';',';','~','>','?','?','F','7','~','~',
     &'<','D','@','L','@','7','~','~','3','M',
     &'7','E','7','7','~','>','7','A','9','D',
     &';','E','=','E','?','D','@','A','@','7',
     &'~','>','@','A','B','D','D','E','F','E',
     &'H','D','I','A','I','7','~','~','7','J',
     &';','E',';','7','~','>',';','A','>','D'/
      DATA(F30(J),J=1,100)/'@','E','C','E','E','D','F','A','F','7',
     &'~','~','7','J','?','E','=','D',';','B',
     &':','?',':','=',';',':','=','8','?','7',
     &'B','7','D','8','F',':','G','=','G','?',
     &'F','B','D','D','B','E','?','E','~','~',
     &'6','I',':','E',':','0','~','>',':','B',
     &'<','D','>','E','A','E','C','D','E','B',
     &'F','?','F','=','E',':','C','8','A','7',
     &'>','7','<','8',':',':','~','~','7','J',
     &'F','E','F','0','~','>','F','B','D','D'/
C
      DATA(F31(J),J=1,100)/'B','E','?','E','=','D',';','B',':','?',
     &':','=',';',':','=','8','?','7','B','7',
     &'D','8','F',':','~','~','9','F','=','E',
     &'=','7','~','>','=','?','>','B','@','D',
     &'B','E','E','E','~','~','8','I','F','B',
     &'E','D','B','E','?','E','<','D',';','B',
     &'<','@','>','?','C','>','E','=','F',';',
     &'F',':','E','8','B','7','?','7','<','8',
     &';',':','~','~',';','G','@','L','@',';',
     &'A','8','C','7','E','7','~','>','=','E'/
      DATA(F32(J),J=1,100)/'D','E','~','~','7','J',';','E',';',';',
     &'<','8','>','7','A','7','C','8','F',';',
     &'~','>','F','E','F','7','~','~','8','H',
     &':','E','@','7','~','>','F','E','@','7',
     &'~','~','5','K','8','E','<','7','~','>',
     &'@','E','<','7','~','>','@','E','D','7',
     &'~','>','H','E','D','7','~','~','8','I',
     &';','E','F','7','~','>','F','E',';','7',
     &'~','~','8','H',':','E','@','7','~','>',
     &'F','E','@','7','>','3','<','1',':','0'/
      DATA(F33(J),J=1,100)/'9','0','~','~','8','I','F','E',';','7',
     &'~','>',';','E','F','E','~','>',';','7',
     &'F','7','~','~','9','G','B','P','@','O',
     &'?','N','>','L','>','J','?','H','@','G',
     &'A','E','A','C','?','A','=','@','?','?',
     &'A','=','A',';','@','9','?','8','>','6',
     &'>','4','?','2','@','1','B','0','~','~',
     &'<','D','@','P','@','0','~','~','9','G',
     &'>','P','@','O','A','N','B','L','B','J',
     &'A','H','@','G','?','E','?','C','A','A'/
      DATA(F34(J),J=1,100)/'C','@','A','?','?','=','?',';','@','9',
     &'A','8','B','6','B','4','A','2','@','1',
     &'>','0','~','~','4','L','7','@','8','B',
     &':','C','<','C','>','B','B','?','D','>',
     &'F','>','H','?','I','A','~','~','3','M',
     &'G','I','9','7','~','>','7','C','I','C',
     &'~','>','7','=','I','=','~','~','7','J',
     &'G','J','F','L','D','L','C','K','B','I',
     &'A','D','@','?','?','<','>',':','<','8',
     &':','7','8','7','7','8','7',':','8',';'/
      DATA(F35(J),J=1,100)/':',';','<',':','?','8','B','7','D','7',
     &'G','8','I',':','~','>','<','A','D','A',
     &'~','~','3','M','@','I','?','H','@','G',
     &'A','H','@','I','~','>','7','@','I','@',
     &'~','>','@','9','?','8','@','7','A','8',
     &'@','9','~','~','9','G','?','L','=','K',
     &'<','I','<','G','=','E','?','D','A','D',
     &'C','E','D','G','D','I','C','K','A','L',
     &'?','L','~','~','3','M','=','E','7','@',
     &'=',';','~','>','7','@','I','@','~','~'/
      DATA(F36(J),J=1,100)/'3','M','C','E','I','@','C',';','~','>',
     &'7','@','I','@','~','~','4','M','J','?',
     &'I','=','G','<','E','<','C','=','B','>',
     &'?','B','>','C','<','D',':','D','8','C',
     &'7','A','7','?','8','=',':','<','<','<',
     &'>','=','?','>','B','B','C','C','E','D',
     &'G','D','I','C','J','A','J','?','~','~',
     &'4','L','@','H','@','7','~','>','8','@',
     &'H','@','~','>','8','7','H','7','~','~',
     &'4','L',';','N',':','M',';','L','<','M'/
      DATA(F37(J),J=1,100)/';','N','~','>','E','N','D','M','E','L',
     &'F','M','E','N','~','~','/','P','2','E',
     &'7','E','@','7','P','X','~','~','3','M',
     &'7','C','I','C','C','H','~','>','I','=',
     &'7','=','=','8','~','~','4','L','7','C',
     &'8','E',':','F','<','F','>','E','B','B',
     &'D','A','F','A','H','B','I','D','~','>',
     &'7','=','I','=','~','~','4','L','I','I',
     &'7','C','I','=','~','>','7',';','I',';',
     &'~','~','3','M','7','E','I','E','~','>'/
      DATA(F38(J),J=1,100)/'7','@','I','@','~','>','7',';','I',';',
     &'~','~','4','L','7','I','I','C','7','=',
     &'~','>','7',';','I',';','~','~','8','H',
     &'D','I','C','K','A','L','?','L','=','K',
     &'<','I','<','G','=','E','B','B','D','@',
     &'E','>','E','<','D',':','B','8','~','>',
     &'>','D','<','B',';','@',';','>','<','<',
     &'>',':','C','7','D','5','D','3','C','1',
     &'A','0','?','0','=','1','<','3','~','~',
     &'7','I','@','L','8','7','~','>','@','L'/
      DATA(F39(J),J=1,100)/'H','7','~','>',';','>','E','>','~','~',
     &'5','J','9','L','9','7','~','>','9','L',
     &'B','L','E','K','F','J','G','H','G','F',
     &'F','D','E','C','B','B','~','>','9','B',
     &'B','B','E','A','F','@','G','>','G',';',
     &'F','9','E','8','B','7','9','7','~','~',
     &'5','K','9','L','9','7','~','>','G','L',
     &'G','7','~','>','9','B','G','B','~','~',
     &'7','I','@','L','8','7','~','>','@','L',
     &'H','7','~','>','8','7','H','7','~','~'/
      DATA(F40(J),J=1,100)/'6','I',':','L',':','7','~','>',':','L',
     &'G','L','~','>',':','B','B','B','~','>',
     &':','7','G','7','~','~','6','J','@','L',
     &'@','7','~','>','>','G',';','F',':','E',
     &'9','C','9','@',':','>',';','=','>','<',
     &'B','<','E','=','F','>','G','@','G','C',
     &'F','E','E','F','B','G','>','G','~','~',
     &'6','G',':','L',':','7','~','>',':','L',
     &'F','L','~','~','6','J','9','L','G','7',
     &'~','>','9','7','G','L','~','~','<','D'/
C
      DATA(F41(J),J=1,100)/'@','L','@','7','~','~','4','L','J','N',
     &'J','O','I','P','G','P','E','O','C','M',
     &'B','K','A','H','>','8','=','4','<','2',
     &';','1','9','0','7','0','6','1','6','2',
     &'~','~','5','J','9','L','9','7','~','>',
     &'G','L','9','>','~','>','>','C','G','7',
     &'~','~','7','I','@','L','8','7','~','>',
     &'@','L','H','7','~','~','4','L','8','L',
     &'8','7','~','>','8','L','@','7','~','>',
     &'H','L','@','7','~','>','H','L','H','7'/
      DATA(F42(J),J=1,100)/'~','~','5','K','9','L','9','7','~','>',
     &'9','L','G','7','~','>','G','L','G','7',
     &'~','~','5','K','>','L','<','K',':','I',
     &'9','G','8','D','8','?','9','<',':',':',
     &'<','8','>','7','B','7','D','8','F',':',
     &'G','<','H','?','H','D','G','G','F','I',
     &'D','K','B','L','>','L','~','~','5','K',
     &'9','L','9','7','~','>','G','L','G','7',
     &'~','>','9','L','G','L','~','~','5','K',
     &'>','L','<','K',':','I','9','G','8','D'/
      DATA(F43(J),J=1,100)/'8','?','9','<',':',':','<','8','>','7',
     &'B','7','D','8','F',':','G','<','H','?',
     &'H','D','G','G','F','I','D','K','B','L',
     &'>','L','~','>','=','B','C','B','~','~',
     &'5','J','9','L','9','7','~','>','9','L',
     &'B','L','E','K','F','J','G','H','G','E',
     &'F','C','E','B','B','A','9','A','~','~',
     &'7','I','9','L','@','B','9','7','~','>',
     &'9','L','G','L','~','>','9','7','G','7',
     &'~','~','8','H','@','L','@','7','~','>'/
      DATA(F44(J),J=1,100)/'9','L','G','L','~','~','7','I','9','G',
     &'9','I',':','K',';','L','=','L','>','K',
     &'?','I','@','E','@','7','~','>','G','G',
     &'G','I','F','K','E','L','C','L','B','K',
     &'A','I','@','E','~','~','7','I','8','7',
     &'@','J','H','7','~','>',';','>','E','>',
     &'~','>','A','I','C','K','C','L','A','N',
     &'?','N','=','L','=','K','?','I','A','I',
     &'~','~','6','J','9','7','=','7',':','>',
     &'9','B','9','F',':','I','<','K','?','L'/
      DATA(F45(J),J=1,100)/'A','L','D','K','F','I','G','F','G','B',
     &'F','>','C','7','G','7','~','~','7','I',
     &'9','L','G','L','~','>','=','B','C','B',
     &'~','>','9','7','G','7','~','~','5','K',
     &'@','L','@','7','~','>','7','F','8','F',
     &'9','E',':','A',';','?','<','>','?','=',
     &'A','=','D','>','E','?','F','A','G','E',
     &'H','F','I','F','~','~','6','J','G','L',
     &'9','7','~','>','9','L','G','L','~','>',
     &'9','7','G','7','~','~','4','L','7','N'/
      DATA(F46(J),J=1,100)/'I','N','~','~','6','K','?','E','=','D',
     &';','B',':','@','9','=','9',':',':','8',
     &'<','7','>','7','@','8','C',';','E','>',
     &'G','B','H','E','~','>','?','E','A','E',
     &'B','D','C','B','E',':','F','8','G','7',
     &'H','7','~','~','7','J','C','L','A','K',
     &'?','I','=','E','<','B',';','>',':','8',
     &'9','0','~','>','C','L','E','L','G','J',
     &'G','G','F','E','E','D','C','C','@','C',
     &'~','>','@','C','B','B','D','@','E','>'/
      DATA(F47(J),J=1,100)/'E',';','D','9','C','8','A','7','?','7',
     &'=','8','<','9',';','<','~','~','6','J',
     &'7','A','8','C',':','E','<','E','=','D',
     &'=','B','<','>',':','7','~','>','<','>',
     &'>','B','@','D','B','E','D','E','F','C',
     &'F','@','E',';','B','0','~','~','7','I',
     &'B','E','?','E','=','D',';','B',':','?',
     &':','<',';','9','<','8','>','7','@','7',
     &'B','8','D',':','E','=','E','@','D','C',
     &'B','E','@','G','?','I','?','K','@','L'/
      DATA(F48(J),J=1,100)/'B','L','D','K','F','I','~','~','8','H',
     &'E','C','D','D','B','E','?','E','=','D',
     &'=','B','>','@','A','?','~','>','A','?',
     &'=','>',';','<',';',':','<','8','>','7',
     &'A','7','C','8','E',':','~','~','5','K',
     &'=','D',';','C','9','A','8','>','8',';',
     &'9','9',':','8','<','7','?','7','B','8',
     &'E',':','G','=','H','@','H','C','F','E',
     &'D','E','B','C','@','?','>',':',';','0',
     &'~','~','7','J','8','B',':','D','<','E'/
      DATA(F49(J),J=1,100)/'=','E','?','D','@','C','A','@','A','<',
     &'@','7','~','>','H','E','G','B','F','@',
     &'@','7','>','3','=','0','~','~','7','I',
     &'9','E',';','E','=','C','C','2','E','0',
     &'G','0','~','>','H','E','G','C','E','@',
     &';','5','9','2','8','0','~','~',':','E',
     &'@','E','>','>','=',':','=','8','>','7',
     &'@','7','B','9','C',';','~','~','7','I',
     &'E','=','E','@','D','C','C','D','A','E',
     &'?','E','=','D',';','B',':','?',':','<'/
      DATA(F50(J),J=1,100)/';','9','<','8','>','7','@','7','B','8',
     &'D',':','E','=','F','B','F','G','E','J',
     &'D','K','B','L','@','L','>','K','<','I',
     &'~','~','7','I','=','E','9','7','~','>',
     &'G','D','F','E','E','E','C','D','?','@',
     &'=','?','<','?','~','>','<','?','>','>',
     &'?','=','A','8','B','7','C','7','D','8',
     &'~','~','8','H','9','L',';','L','=','K',
     &'>','J','F','7','~','>','@','E',':','7',
     &'~','~','6','K','=','E','7','0','~','>'/
C
      DATA(F51(J),J=1,100)/'<','A',';','<',';','9','=','7','?','7',
     &'A','8','C',':','E','>','~','>','G','E',
     &'E','>','D',':','D','8','E','7','G','7',
     &'I','9','J',';','~','~','7','I',':','E',
     &'=','E','<','?',';',':',':','7','~','>',
     &'G','E','F','B','E','@','C','=','@',':',
     &'=','8',':','7','~','~','8','I','@','E',
     &'>','D','<','B',';','?',';','<','<','9',
     &'=','8','?','7','A','7','C','8','E',':',
     &'F','=','F','@','E','C','D','D','B','E'/
      DATA(F52(J),J=1,100)/'@','E','~','~','5','K','>','E',':','7',
     &'~','>','C','E','D','?','E',':','F','7',
     &'~','>','7','B','9','D','<','E','I','E',
     &'~','~','5','J','6','A','7','C','9','E',
     &';','E','<','D','<','B',';','=',';',':',
     &'<','8','=','7','?','7','A','8','C',';',
     &'D','=','E','@','F','E','F','H','E','K',
     &'C','L','A','L','@','J','@','H','A','E',
     &'C','B','E','@','H','>','~','~','7','I',
     &';','?',';','<','<','9','=','8','?','7'/
      DATA(F53(J),J=1,100)/'A','7','C','8','E',':','F','=','F','@',
     &'E','C','D','D','B','E','@','E','>','D',
     &'<','B',';','?','7','0','~','~','7','K',
     &'I','E','?','E','=','D',';','B',':','?',
     &':','<',';','9','<','8','>','7','@','7',
     &'B','8','D',':','E','=','E','@','D','C',
     &'C','D','A','E','~','~','6','J','A','E',
     &'>','7','~','>','8','B',':','D','=','E',
     &'H','E','~','~','6','J','7','A','8','C',
     &':','E','<','E','=','D','=','B',';','<'/
      DATA(F54(J),J=1,100)/';','9','=','7','?','7','B','8','D',':',
     &'F','>','G','B','G','E','~','~','6','J',
     &'9','7','9','H',':','J',';','K','>','L',
     &'B','L','E','K','F','I','F','G','E','E',
     &'C','D','?','C','A','C','D','B','F','@',
     &'G','>','G',';','F','9','E','8','B','7',
     &'>','7',';','9','~','~','4','K','<','E',
     &':','D','8','A','7','>','7',';','8','8',
     &'9','7',';','7','=','8','?',';','~','>',
     &'@','?','?',';','@','8','A','7','C','7'/
      DATA(F55(J),J=1,100)/'E','8','G',';','H','>','H','A','G','D',
     &'F','E','~','~','8','H','B','L','@','K',
     &'?','J','?','I','@','H','C','G','F','G',
     &'~','>','C','G','@','F','>','E','=','C',
     &'=','A','?','?','B','>','D','>','~','>',
     &'B','>','>','=','<','<',';',':',';','8',
     &'=','6','A','4','B','3','B','1','@','0',
     &'>','0','~','~','4','K','D','L','<','0',
     &'~','>','5','A','6','C','8','E',':','E',
     &';','D',';','B',':','=',':',':',';','8'/
      DATA(F56(J),J=1,100)/'=','7','?','7','B','8','D',':','F','=',
     &'H','B','I','E','~','~','8','G','B','L',
     &'@','K','?','J','?','I','@','H','C','G',
     &'F','G','~','>','F','G','B','E','?','C',
     &'<','@',';','=',';',';','<','9','>','7',
     &'A','5','B','3','B','1','A','0','?','0',
     &'>','2','~','~','4','L','7','C','8','E',
     &':','F','<','F','>','E','B','B','D','A',
     &'F','A','H','B','I','D','~','>','7','=',
     &'8','?',':','@','<','@','>','?','B','<'/
      DATA(F57(J),J=1,24)/'D',';','F',';','H','<','I','>','~','~',
     &'>','C','@','A','@','@','A','@','A','A','@','A','~','~'/
      END

      BLOCK DATA FINIT2
C***********************************************************************
C     THIS COMMON BLOCK ALLOWS USER TO SET SOME PARAMETERS THAT
C     ARE NOT EASILY SET THROUGH THE CALCOMP CALLS.
C
C       BATON  - USED BY VOLKSGRAPHER.  CAN BE IGNORED IN THIS 
C                CONTEXT.  INCLUDED SO THAT SOURCE CODE DOES NOT
C                NEED TO BE MODIFIED AND USER PROGRAM DOESN'T NEED TO
C                SET IT.
C       IBACK  - SET BACKGROUND COLOR.  IGNORED FOR POSTSCRIPT, HP-GL,
C                QMS, TEKTRONIX DRIVERS.  USE FOR X11.  THE DEFAULT 
C                IS BLACK (0).  CAN USE 1 FOR A WHITE BACKGROUND, 2-64
C                FOR VARIOUS COLORS.
C       IFORE  - DEFAULT FOREGROUND COLOR.  CURRENTLY ONLY USED BY
C                X11, POSTSCRIPT, AND HP-GL DRIVERS.  DEFAULT IS 1
C                (I.E., BLACK).
C       IMAXCL - MAXIMUM NUMBER OF ALLOWED COLORS.  IGNORED FOR QMS
C                AND TEKTRONIX SINCE THEY ARE BLACK AND WHITE.  THE
C                X11 AND POSTSCRIPT DRIVERS SUPPORT A FIXED NUMBER OF
C                COLORS (64 AND 16 RESPECTIVELY), SO IGNORE HERE AS
C                WELL.  INCLUDED PRIMARILY FOR HP-GL, SINCE DIFFERENT
C                PLOTTERS SUPPORT DIFFERENT NUMBER OF PENS (USE 8 BY
C                DEFAULT).
C       AXSIZE - MAXIMUM NUMBER OF INCHES TO USE.  CALCOMP LIBRARY DOES
C                NOT SPECIFY A SPECIFIC PAGE SIZE IN INCHES.  
C                VOLKSGRAPHER ASSUMED 10.24 x 7.8 INCHES.  USER CAN
C                SET THESE PARAMETERS IF USING A DIFFERENT ASSUMPTION.
C                ONLY NEED TO OVERRIDE THE DEFAULTS IF USING VALUES
C                GREATER THAN 10.24 AND 7.8 RESPECTIVELY.
C       IPOST  - FLAG IF FILE IS ALREADY OPEN (0=CLOSE, 1=OPEN)
C       IHPGL
C       IHPGL2
C       IQMS
C       ITEKT
C       CPOST  - FILE NAME FOR POSTSCRIPT, HP-GL, LASERJET, QMS, 
C       CHPGL    TEKTRONIX RESPECTIVELY
C       CQMS
C       CTEKT
C***********************************************************************
      INTEGER IBACK, IMAXCL
      INTEGER IDSPLY(80)
      CHARACTER*4 X11FLG
      REAL AXSIZE, AYSIZE
      LOGICAL BATON
      CHARACTER*80 CPOST,CHPGL,CHPGL2,CQMS,CTEKT
C
      COMMON/VGCALC/ IBACK, IFORE, AXSIZE, AYSIZE, IMAXCL
      COMMON/VGBT/BATON
      COMMON/VGFLAG/IPOST,IHPGL,IHPGL2,IQMS,ITEKT,IX11
      COMMON/VGNAME/CPOST,CHPGL,CHPGL2,CQMS,CTEKT
      COMMON/VGX11/IDSPLY
      COMMON/VGX112/X11FLG
C
      DATA BATON /.TRUE./
C
      DATA IBACK /0/
      DATA IFORE /1/
      DATA IMAXCL/8/
      DATA AXSIZE /10.24/
      DATA AYSIZE /7.80/
C
      DATA IPOST /0/
      DATA IHPGL /0/
      DATA IHPGL2 /0/
      DATA IQMS /0/
      DATA ITEKT /0/
      DATA IX11  /0/
C
      DATA CPOST /'post.dat'/
      DATA CHPGL /'hpgl.dat'/
      DATA CHPGL2 /'hpgl2.dat'/
      DATA CQMS /'qms.dat'/
      DATA CTEKT /'tektronix.dat'/
C
      DATA X11FLG / 'ON' /
      DATA IDSPLY(1) / -1 /
C
      END
