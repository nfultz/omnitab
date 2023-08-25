*TABLE
      SUBROUTINE TABLE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  TABLE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                    *** CROSS TABULATION PROCEDURE ***
C
C     INSTRUCTIONS -
C
C       L2= 1 - TABLE FREQUENCY  (N), (C), (C), ..., (C)
C       L2= 2 - TABLE SUM        (N), (C), (C), ..., (C) FOR (C)
C       L2= 3 - TABLE AVERAGE    (N), (C), (C), ..., (C) FOR (C)
C       L2= 4 - TABLE STDDEV     (N), (C), (C), ..., (C) FOR (C)
C       L2= 5 - TABLE MINIMUM    (N), (C), (C), ..., (C) FOR (C)
C       L2= 6 - TABLE MAXIMUM    (N), (C), (C), ..., (C) FOR (C)
C       L2= 7 - TABLE RANGE      (N), (C), (C), ..., (C) FOR (C)
C       L2= 8 - TABLE MEDIAN     (N), (C), (C), ..., (C) FOR (C)
C       L2= 9 - TABLE PERCENTAGE (N), (C), (C), ..., (C)
C       L2=10 - TABLE PROPORTION (N), (C), (C), ..., (C)
C       L2=11 - TABLE RPERCENT   (N), (C), (C), ..., (C)
C       L2=12 - TABLE CPERCENT   (N), (C), (C), ..., (C)
C       L2=13 - TABLE RPROPORT   (N), (C), (C), ..., (C)
C       L2=14 - TABLE CPROPORT   (N), (C), (C), ..., (C)
C
C       L2=15 - NTABLE FREQUENCY  (N), (C), (C), ..., (C)
C       L2=16 - NTABLE SUM        (N), (C), (C), ..., (C) FOR (C)
C       L2=17 - NTABLE AVERAGE    (N), (C), (C), ..., (C) FOR (C)
C       L2=18 - NTABLE STDDEV     (N), (C), (C), ..., (C) FOR (C)
C       L2=19 - NTABLE MINIMUM    (N), (C), (C), ..., (C) FOR (C)
C       L2=20 - NTABLE MAXIMUM    (N), (C), (C), ..., (C) FOR (C)
C       L2=21 - NTABLE RANGE      (N), (C), (C), ..., (C) FOR (C)
C       L2=22 - NTABLE MEDIAN     (N), (C), (C), ..., (C) FOR (C)
C       L2=23 - NTABLE PERCENTAGE (N), (C), (C), ..., (C)
C       L2=24 - NTABLE PROPORTION (N), (C), (C), ..., (C)
C       L2=25 - NTABLE RPERCENT   (N), (C), (C), ..., (C)
C       L2=26 - NTABLE CPERCENT   (N), (C), (C), ..., (C)
C       L2=27 - NTABLE RPROPORT   (N), (C), (C), ..., (C)
C       L2=28 - NTABLE CPROPORT   (N), (C), (C), ..., (C)
C
C     OPTIONAL FORMS WITH STORAGE.
C
C       L2= 1 TABLE FREQUENCY  (N), (C), (C), ..., (C)         STORE (C)
C       L2= 2 TABLE SUM        (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 3 TABLE AVERAGE    (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 4 TABLE STDDEV     (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 5 TABLE MINIMUM    (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 6 TABLE MAXIMUM    (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 7 TABLE RANGE      (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 8 TABLE MEDIAN     (N), (C), (C), ..., (C) FOR (C) STORE (C)
C       L2= 9 TABLE PERCENTAGE (N), (C), (C), ..., (C)         STORE (C)
C       L2=10 TABLE PROPORTION (N), (C), (C), ..., (C)         STORE (C)
C       L2=11 TABLE RPERCENT   (N), (C), (C), ..., (C)         STORE (C)
C       L2=12 TABLE CPERCENT   (N), (C), (C), ..., (C)         STORE (C)
C       L2=13 TABLE RPROPORT   (N), (C), (C), ..., (C)         STORE (C)
C       L2=14 TABLE CPROPORT   (N), (C), (C), ..., (C)         STORE (C)
C
C       L2=15 NTABLE FREQUENCY  (N), (C), (C), ...,(C)         STORE (C)
C       L2=16 NTABLE SUM        (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=17 NTABLE AVERAGE    (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=18 NTABLE STDDEV     (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=19 NTABLE MINIMUM    (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=20 NTABLE MAXIMUM    (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=21 NTABLE RANGE      (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=22 NTABLE MEDIAN     (N), (C), (C), ...,(C) FOR (C) STORE (C)
C       L2=23 NTABLE PERCENTAGE (N), (C), (C), ...,(C)         STORE (C)
C       L2=24 NTABLE PROPORTION (N), (C), (C), ...,(C)         STORE (C)
C       L2=25 NTABLE RPERCENT   (N), (C), (C), ..., (C)        STORE (C)
C       L2=26 NTABLE CPERCENT   (N), (C), (C), ..., (C)        STORE (C)
C       L2=27 NTABLE RPROPORT   (N), (C), (C), ..., (C)        STORE (C)
C       L2=28 NTABLE CPROPORT   (N), (C), (C), ..., (C)        STORE (C)
C
C     A CODE IS USED TO CONSTRUCT THE TABLE.
C       THE CODE FOR ANY CLASSIFICATION I IS FLOAT(J), WHERE
C         J IS THE JTH ORDERED LEVEL OF CLASSIFICATION I.
C       THE TOTAL CODE IS -
C         C2 + NL(2)*(C1-1) + NL(1)*NL(2)*(C3-1) +
C           NL(1)*NL(2)*NL(3)*(C4-1) + ...
C         WHERE C1 IS THE CODE FOR CLASSIFICATION 1,
C               C2 IS THE CODE FOR CLASSIFICATION 2, ETC, AND
C                 NL(I) IS THE NUMBER OF LEVELS OF CLASSIFICATION I.
C
C     INTERNAL STORAGE IN THE SCRATCH AREA ...
C       NL(I) CONTAINS NUMBER OF LEVELS OF CLASSIFICATION I.
C       A(I), I=1,NRMAX, CONTAINS TOTAL CODE.
C       A(I), I=L,2*NRMAX, CONTAINS CODE FOR CLASSIFICATION I, L=NRMAX+1
C       LEVELS OF CLASSIFICATION I ARE STORED IN A(J), J=K,K+NL(I)-1
C          FOR I=1, K = L, OTHERWISE K = K+NL(I-1).
C       TABLE IS STORED AFTER LEVELS OF CLASSIFICATIONS, ROW AFTER ROW.
C         SIZE OF THE TABLE IS NT = PRODUCT OF NL(I).
C         TABLE IS STORED IN A(I), I=M,M+NT-1.
C         ROW TOTALS (NROWS=NT/NL(2)) ARE STORED AFTER TABLE.
C         COLUMN TOTALS (NCOLS=NL(2)) ARE STORED AFTER ROW TOTALS.
C
C     TOTAL STORAGE REQUIRED = NRMAX + SUM OF NL(I) + NT + NROWS + NCOLS
C        IF NT IS LESS THAN NRMAX, TOTAL IS 2*NRMAX + SUM NL(I).
C     FOR TABLE AVERAGE, STORAGE IS INCREASED BY   NT.
C     FOR TABLE  STDDEV, STORAGE IS INCREASED BY 2*NT.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MARCH, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NL(8)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ABLANK, CONST, SUM
      REAL             FDIV, FSQRT
      REAL             SPCA, SPCB, SPCC
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /   0.0001 /
      DATA SPCB /   0.01   /
      DATA SPCC / 100.0    /
C
C     ==================================================================
C
C     MSTORE = 1, IF STORAGE IS     REQUESTED
C            = 0, IF STORAGE IS NOT REQUESTED.
C
      N = IARGS(1)
C
C     SET LTWO = L2 MOD (NITB).
C
      LTWO = L2
      IF (L2.GT.NITB) LTWO = L2 - NITB
C
C     ERROR CHECKING.
C
      IF (NRMAX.GT.IZERO) GO TO 10
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.GT.IONE) GO TO 30
  20  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  30  IF (KIND(IONE).EQ.IONE) GO TO 50
      IF (N.GT.IZERO .AND. N.LE.7) GO TO 40
      IF (LTWO.EQ.IONE .AND. N.EQ.8) GO TO 40
      IF (LTWO.GE.9 .AND. N.EQ.8) GO TO 40
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
C     CHECK NUMBER OF ARGUMENTS.
C
  40  MSTORE = NARGS - N - IONE
      IF (LTWO.GE.ITWO .AND. LTWO.LE.8) MSTORE = MSTORE - IONE
      IF (MSTORE.LT.IZERO .OR. MSTORE.GT.IONE) GO TO 20
C
      KSTORE = IARGS(NARGS)
      IF (LTWO.EQ.IONE .OR.  LTWO.GE.9)
     1    CALL HEADS (IARGS(2),N,IZERO,IONE)
      IF (LTWO.GT.IONE .AND. LTWO.LE.8)
     1    CALL HEADS (IARGS(2),N+IONE,IZERO,IONE)
      CALL CHKCOL
      GO TO 60
  50  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  60  NRMAX2 = ITWO * NRMAX
      IF (NRMAX2.LE.NS) GO TO 80
  70  CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
  80  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     COMPUTE TOTAL CODE.
C
      IRDATA = IARGS(N+2)
      L = IONE + NRMAX
      M = L
C
C     MTOTAL = 0, DO NOT PRINT TOTALS
C              1, PRINT ROW AND COLUMN TOTALS
C              2, ONLY PRINT COLUMN TOTALS AT BOTTOM
C              3, ONLY PRINT ROW TOTALS ON RIGHT
C
      MTOTAL = IONE
      IF (LTWO.EQ.ITHRE .OR. LTWO.EQ.IFOUR .OR. LTWO.EQ.8)
     1    MTOTAL = IZERO
      IF (LTWO.EQ.11 .OR. LTWO.EQ.13) MTOTAL = ITHRE
      IF (LTWO.EQ.12 .OR. LTWO.EQ.14) MTOTAL = ITWO
C
C     COMPUTE ORDERED DISTINCT LEVELS OF CLASSIFICATION I IN A(M),
C        AND NUMBER OF LEVELS IN N(I).
C
      DO 90 I=1,N
        K = IARGS(I+1)
        CALL UNIQUE (RC(K),NRMAX,A(1),A(M),NL(I))
        M = M + NL(I)
        IF (M.GT.NS) GO TO 70
        K = K + IONE
  90  CONTINUE
      IF (N.GT.IONE) GO TO 100
C
C     SPECIAL CASE, N=1
C
      K = IARGS(2)
      CALL SCODE (RC(K),A(L),A(1),NRMAX,NL(1))
      NT = NL(1)
      GO TO 130
 100  IF (NRMAX2+M.GT.NS) GO TO 70
      NSTART = L
      NT = IONE
      MAXSCR = NRMAX + M
      DO 120 I=1,N
        K = IARGS(I+1)
        CALL SCODE (RC(K),A(NSTART),A(M),NRMAX,NL(I))
        JJ = M
        DO 110 J=1,NRMAX
          IF (I.EQ.IONE) A(J) = FLOAT(NL(2)) * (A(JJ)-RONE)
          IF (I.EQ.ITWO) A(J) = A(J) + A(JJ)
          IF (I.GT.ITWO) A(J) = A(J) + FLOAT(NT) * (A(JJ)-RONE)
          JJ = JJ + IONE
 110    CONTINUE
        NSTART = NSTART + NL(I)
        NT = NT * NL(I)
        MAXSCR = MAXSCR + NT
        IF (MAXSCR.GT.NS) GO TO 70
        IF (LTWO.GE.ITHRE .AND. LTWO.LE.8 .AND. MAXSCR+NT.GT.NS)
     1     GO TO 70
        IF (LTWO.EQ.IFOUR .AND. MAXSCR+ITWO*NT.GT.NS) GO TO 70
 120  CONTINUE
C
 130  NCOLS  = NL(2)
      IF (N.EQ.IONE) NCOLS = IONE
      NROWS  = IDIV (NT,NCOLS,IND)
      NRPLSC = NROWS + NCOLS
      NTOTLS = NRPLSC + IONE
      IF (N.EQ.IONE .AND. NL(1).EQ.IONE) MTOTAL = IZERO
      IEND = M + NT - IONE
      IF (LTWO.EQ.ITHRE .OR. LTWO.EQ.7 .OR. LTWO.EQ.8) IEND = IEND + NT
      IF (LTWO.EQ.IFOUR) IEND = IEND + ITWO * NT
      IF (LTWO.EQ.8) IEND = MAX0 (IEND+NRMAX,M+NRMAX2)
      IF (IEND.GT.NS) GO TO 70
      IF (MTOTAL.EQ.IZERO) GO TO 140
      IEND = IEND + NTOTLS
      IF (LTWO.EQ.7) IEND = IEND + NTOTLS
      IF (IEND.GT.NS) GO TO 70
C
C     CLEAR SCRATCH AREA FOR TABLE.
C
 140  ABLANK = RPIFY
      IF (LTWO.EQ.IONE .OR. LTWO.GE.9) ABLANK = RZERO
      IF (LTWO.EQ.ITHRE .OR. LTWO.EQ.IFOUR) ABLANK = RZERO
      IF (LTWO.EQ.6 .OR. LTWO.EQ.7) ABLANK = RMIFY
      DO 150 I=M,IEND
        A(I) = ABLANK
 150  CONTINUE
C
C     ==================================================================
C
C     INITIALIZE.
C
      MMIN1  = M - IONE
      MPLSNT = M + NT - IONE
      IRCSUB = IRDATA
      MRTBEG = M + NT
      MCTBEG = MRTBEG + NROWS
      NTRC   = NT + NRPLSC
      ISUM   = M + NTRC
      NTPLSS = NT + NTOTLS
C
      GO TO (160,210,160,160,450,  520,520,690,160,160,
     1       160,160,160,160), LTWO
C
C     COMPUTE FREQUENCY TABLE.
C
 160  DO 170 I=1,NRMAX
        K = A(I) + SPCA
        K = K + MMIN1
        A(K) = A(K) + RONE
 170  CONTINUE
      IF (LTWO.EQ.ITHRE .OR. LTWO.EQ.IFOUR) GO TO 350
      IF (MTOTAL.EQ.IZERO) GO TO 900
C
C     COMPUTE ROW TOTALS.
C
      KA = M + NT
      K = M
      DO 190 I=1,NROWS
        DO 180 J=1,NCOLS
          A(KA) = A(KA) + A(K)
          K = K + IONE
 180    CONTINUE
        KA = KA + IONE
 190  CONTINUE
C
C     COMPUTE COLUMN TOTALS AND GRAND TOTAL.
C
      KA = KA - IONE
      KR = M
      A(ISUM) = RZERO
      J       = IONE
      DO 200 I=1,NT
        IF (N.EQ.IONE)  J = IONE
        IF (N.NE.IONE)  J = MOD (I,NL(2))
        IF (J.EQ.IZERO) J = NL(2)
        K = KA + J
        A(K) = A(K) + A(KR)
        A(ISUM) = A(ISUM) + A(KR)
        KR = KR + IONE
 200  CONTINUE
      IF (LTWO.EQ.9  .OR. LTWO.EQ.ITEN) GO TO 780
      IF (LTWO.EQ.11 .OR. LTWO.EQ.13)   GO TO 820
      IF (LTWO.EQ.12 .OR. LTWO.EQ.14)   GO TO 860
      GO TO 900
C
C     COMPUTE SUM TABLE.
C
 210  IRCSUB = IRDATA
      DO 240 I=1,NRMAX
        K = A(I) + SPCA
        K = K + MMIN1
        IF (A(K).NE.RPIFY) GO TO 220
        A(K) = RC(IRCSUB)
        GO TO 230
 220    A(K) = A(K) + RC(IRCSUB)
 230    IRCSUB = IRCSUB + IONE
 240  CONTINUE
C
C     COMPUTE ROW TOTALS.
C
      IRTSUB = MRTBEG - IONE
      ITSUB  = M
      DO 290 I=1,NROWS
        IRTSUB = IRTSUB + IONE
        IEMPTY = IZERO
        CALL SUMMAL (A(ITSUB),IZERO,SUM)
        DO 270 J=1,NCOLS
          IF (A(ITSUB).NE.RPIFY) GO TO 250
          IEMPTY = IEMPTY + IONE
          GO TO 260
 250      CALL SUMMAL (A(ITSUB),-IONE,SUM)
 260      ITSUB = ITSUB + IONE
 270    CONTINUE
        IF (IEMPTY.NE.NCOLS) GO TO 280
        A(IRTSUB) = RPIFY
        GO TO 290
 280    CALL SUMMAL (A(ITSUB),IONE,SUM)
        A(IRTSUB) = SUM
 290  CONTINUE
C
C     COMPUTE COLUMN TOTALS AND GRAND TOTAL = A(ISUM).
C
      A(ISUM)  = RZERO
      ICTSUB = MCTBEG - IONE
      DO 340 I=1,NCOLS
        ITSUB  = MMIN1 + I
        ICTSUB = ICTSUB + IONE
        IEMPTY = IZERO
        CALL SUMMAL (A(ITSUB),IZERO,SUM)
        DO 320 J=1,NROWS
          IF (A(ITSUB).NE.RPIFY) GO TO 300
          IEMPTY = IEMPTY + IONE
          GO TO 310
 300      CALL SUMMAL (A(ITSUB),-IONE,SUM)
 310      ITSUB = ITSUB + NCOLS
 320    CONTINUE
        IF (IEMPTY.NE.NROWS) GO TO 330
        A(ICTSUB) = RPIFY
        GO TO 340
 330    CALL SUMMAL (A(ITSUB),IONE,SUM)
        A(ICTSUB) = SUM
        A(ISUM)   = A(ISUM) + A(ICTSUB)
 340  CONTINUE
C
      GO TO 900
C
C     COMPUTE AVERAGE TABLE.
C
 350  IRCSUB = IRDATA
C
      DO 360 I=1,NRMAX
        K       = A(I) + SPCA
        KFREQ   = K + MMIN1
        KAVE    = KFREQ + NT
        A(KAVE) = A(KAVE) + FDIV (RC(IRCSUB),A(KFREQ),IND)
        IRCSUB  = IRCSUB + IONE
 360  CONTINUE
      IF (LTWO.EQ.IFOUR) GO TO 400
C
C     MOVE TABLE.
C
      ITASUB = MRTBEG
      ITFSUB = M
      DO 390 I=1,NT
        IF (A(ITFSUB).NE.ABLANK) GO TO 370
        A(ITFSUB) = RPIFY
        GO TO 380
 370    A(ITFSUB) = A(ITASUB)
 380    ITFSUB    = ITFSUB + IONE
        ITASUB    = ITASUB + IONE
 390  CONTINUE
C
      ABLANK = RPIFY
      GO TO 900
C
C     COMPUTE STANDARD DEVIATION TABLE.
C
 400  IRCSUB = IRDATA
C
C     COMPUTE SUM (X-XBAR) SQUARED.
C
      DO 410 I=1,NRMAX
        K      = A(I) + SPCA
        K      = K + MMIN1
        KAVE   = K + NT
        KSS    = KAVE + NT
        A(KSS) = A(KSS) + (RC(IRCSUB)-A(KAVE))**2
        IRCSUB = IRCSUB + IONE
 410  CONTINUE
C
      K = MMIN1
      KSS = MPLSNT + NT
      DO 440 I=1,NT
        K = K + IONE
        KSS = KSS + IONE
        IF (A(K).EQ.RONE) GO TO 420
        IF (A(K).GT.RONE) GO TO 430
        A(K) = RPIFY
        GO TO 440
 420    A(K) = RZERO
        GO TO 440
 430    A(K) = FSQRT (FDIV(A(KSS),A(K)-RONE,IND))
 440  CONTINUE
C
      ABLANK = RPIFY
      GO TO 900
C
C     COMPUTE MINIMUM TABLE.
C
 450  IRCSUB = IRDATA
      DO 470 I=1,NRMAX
        IASUB = A(I) + SPCA
        IASUB = IASUB + MMIN1
        IF (RC(IRCSUB).GE.A(IASUB)) GO TO 460
        A(IASUB) = RC(IRCSUB)
 460    IRCSUB = IRCSUB + IONE
 470  CONTINUE
C
C     COMPUTE ROW MINIMUMS.
C
      IRTSUB = MRTBEG - IONE
      ITSUB  = M
      DO 490 I=1,NROWS
        IRTSUB = IRTSUB + IONE
        DO 480 J=1,NCOLS
          A(IRTSUB) = AMIN1 (A(IRTSUB),A(ITSUB))
          ITSUB = ITSUB + IONE
 480    CONTINUE
 490  CONTINUE
C
C     COMPUTE COLUMN MINIMUMS AND GRAND MINIMUM = A(ISUM).
C
      ICTSUB  = MCTBEG - IONE
      A(ISUM) = A(MCTBEG)
      DO 510 I=1,NCOLS
        ITSUB  = MMIN1 + I
        ICTSUB = ICTSUB + IONE
        DO 500 J=1,NROWS
          A(ICTSUB) = AMIN1 (A(ICTSUB),A(ITSUB))
          ITSUB = ITSUB + NCOLS
 500    CONTINUE
        A(ISUM) = AMIN1 (A(ISUM),A(ICTSUB))
 510  CONTINUE
C
      ABLANK = RPIFY
      GO TO 900
C
C     COMPUTE MAXIMUM TABLE.
C
 520  IRCSUB = IRDATA
      DO 540 I=1,NRMAX
        IASUB = A(I) + SPCA
        IASUB = IASUB + MMIN1
        IF (RC(IRCSUB).LE.A(IASUB)) GO TO 530
        A(IASUB) = RC(IRCSUB)
 530    IRCSUB = IRCSUB + IONE
 540  CONTINUE
C
C     COMPUTE ROW MAXIMUMS.
C
      IRTSUB = MRTBEG - IONE
      ITSUB  = M
      DO 560 I=1,NROWS
        IRTSUB = IRTSUB + IONE
        DO 550 J=1,NCOLS
          A(IRTSUB) = AMAX1 (A(IRTSUB),A(ITSUB))
          ITSUB = ITSUB + IONE
 550    CONTINUE
        IF (A(IRTSUB).GT.RMIFY) GO TO 560
        A(IRTSUB) = RPIFY
 560  CONTINUE
C
C     COMPUTE COLUMN MAXIMUMS AND GRAND MAXIMUM = A(ISUM).
C
      ICTSUB  = MCTBEG - IONE
      A(ISUM) = A(MCTBEG)
      DO 580 I=1,NCOLS
        ITSUB  = MMIN1 + I
        ICTSUB = ICTSUB + IONE
        DO 570 J=1,NROWS
          A(ICTSUB) = AMAX1 (A(ICTSUB),A(ITSUB))
          ITSUB = ITSUB + NCOLS
 570    CONTINUE
        A(ISUM) = AMAX1 (A(ISUM),A(ICTSUB))
 580  CONTINUE
C
C     CHANGE RMIFY TO RPIFY (MINUS INFINITY TO PLUS INFINITY).
C
      IASUB = M
      DO 600 I=1,NT
        IF (A(IASUB).GT.RMIFY) GO TO 590
        A(IASUB) = RPIFY
 590    IASUB = IASUB + IONE
 600  CONTINUE
      ABLANK = RPIFY
C
      IF (LTWO.EQ.6) GO TO 900
C
C     COMPUTE RANGE TABLE.
C
C     COMPUTE MINIMUM.
C
      IBEG = M + NTPLSS
      IEND = IBEG + NTPLSS - IONE
      DO 610 I=IBEG,IEND
        A(I) = RPIFY
 610  CONTINUE
C
      IRCSUB = IRDATA
      DO 630 I=1,NRMAX
        IASUB = A(I) + SPCA
        IASUB = IASUB + ISUM
        IF (RC(IRCSUB).GE.A(IASUB)) GO TO 620
        A(IASUB) = RC(IRCSUB)
 620    IRCSUB = IRCSUB + IONE
 630  CONTINUE
C
C     COMPUTE ROW MINIMUMS.
C
      IRTSUB = MRTBEG - IONE + NTPLSS
      ITSUB  = M + NTPLSS
      DO 650 I=1,NROWS
        IRTSUB = IRTSUB + IONE
        DO 640 J=1,NCOLS
          A(IRTSUB) = AMIN1 (A(IRTSUB),A(ITSUB))
          ITSUB = ITSUB + IONE
 640    CONTINUE
 650  CONTINUE
C
C     COMPUTE COLUMN MINIMUMS AND GRAND MINIMUM = A(ISUM).
C
      ICTSUB = MCTBEG - IONE + NTPLSS
      MINSUM = ISUM + NTPLSS
      A(MINSUM) = A(ICTSUB+1)
      DO 670 I=1,NCOLS
        ITSUB  = ISUM + I
        ICTSUB = ICTSUB + IONE
        DO 660 J=1,NROWS
          A(ICTSUB) = AMIN1 (A(ICTSUB),A(ITSUB))
          ITSUB = ITSUB + NCOLS
 660    CONTINUE
        A(MINSUM) = AMIN1 (A(MINSUM),A(ICTSUB))
 670  CONTINUE
C
C     COMPUTE RANGE
C
      IMAXSB = MMIN1
      IMINSB = ISUM
      DO 680 I=1,NTPLSS
        IMAXSB = IMAXSB + IONE
        IMINSB = IMINSB + IONE
        IF (A(IMAXSB).GE.RPIFY) GO TO 680
        A(IMAXSB) = A(IMAXSB) - A(IMINSB)
 680  CONTINUE
C
      ABLANK = RPIFY
      GO TO 900
C
C     COMPUTE MEDIAN TABLE.
C
C     STORE DATA WHERE TABLE IS.
C
 690  IRCSUB = IRDATA
      ITSUB  = M
      DO 700 I=1,NRMAX
        A(ITSUB) = RC(IRCSUB)
        ITSUB  = ITSUB + IONE
        IRCSUB = IRCSUB + IONE
 700  CONTINUE
C
C     SORT DATA AND CARRY ALONG CODE.
C
      MHIER = MAX0 (MRTBEG,M+NRMAX)
      CALL SORT (A(M),A(MHIER),NRMAX,IONE)
      IHSUB = MHIER
      DO 710 I=1,NRMAX
        J = A(IHSUB) + SPCB
        A(IHSUB) = A(J)
        IHSUB = IHSUB + IONE
 710  CONTINUE
C
C     SORT CODE AND CARRY ALONG DATA.
C
      CALL SORT (A(MHIER),A(1),NRMAX,IONE)
      DO 720 I=1,NRMAX
        J = A(I) + SPCB
        IDSUB = MMIN1 + J
        A(I) = A(IDSUB)
 720  CONTINUE
C
C     INSERT PLUS INFINITY IN TABLE.
C
      ITSUB = M
      DO 730 I=1,NT
        A(ITSUB) = RPIFY
        ITSUB = ITSUB + IONE
 730  CONTINUE
C
C     DETERMINE K = NUMBER OF MEASUREMENTS IN CELL.
C
      K = IONE
      ICSUB  = MHIER + IONE
      IRCSUB = IZERO
      DO 760 I=2,NRMAX
        IF (A(ICSUB).NE.A(ICSUB-1)) GO TO 740
        K = K + IONE
        IF (I.EQ.NRMAX) GO TO 740
        GO TO 750
 740    IDSUB = IRCSUB + IDIV (K+IONE,ITWO,IND)
C
C     STORE MEDIAN IN TABLE.
C
        ITSUB = M + INT (A(ICSUB-1)+SPCA) - IONE
        MODK  = MOD (K,ITWO)
        IF (MODK.EQ.IONE) A(ITSUB) = A(IDSUB)
        IF (MODK.EQ.IZERO)
     1         A(ITSUB) = FDIV (A(IDSUB)+A(IDSUB+1),RTWO,IND)
        IRCSUB = IRCSUB + K
C
C       RESET K
C
        K = IONE
C
C     CONTINUE
C
 750    ICSUB = ICSUB + IONE
 760  CONTINUE
C
C     MEDIAN FOR LAST NUMBER IN COLUMN
C
      IF (A(ICSUB-1).LE.A(ICSUB-2)) GO TO 770
      ITSUB = M + INT (A(ICSUB-1) + SPCA) - IONE
      A(ITSUB) = A(IRCSUB+1)
C
 770  ABLANK = RPIFY
      GO TO 900
C
C     COMPUTE PERCENTAGES OR PROPORTIONS TABLE.
C
 780  CONST = RONE
      IF (LTWO.EQ.9) CONST = SPCC
      K = M
      DO 790 I=1,NTRC
        A(K) = CONST * FDIV (A(K),A(ISUM),IND)
        K = K + IONE
 790  CONTINUE
C
      DO 800 I=1,NROWS
        A(K) = CONST * FDIV (A(K),A(ISUM),IND)
        K = K + IONE
 800  CONTINUE
C
      DO 810 I=1,NCOLS
        A(K) = CONST * FDIV (A(K),A(ISUM),IND)
        K = K + IONE
 810  CONTINUE
      GO TO 900
C
C     COMPUTE RPERCENTAGES OR RPROPORTIONS TABLE.
C
 820  CONST = RONE
      IF (LTWO.EQ.11) CONST = SPCC
      K = MRTBEG
      ITSUB = M
      DO 840 I=1,NROWS
        DO 830 J=1,NCOLS
          A(ITSUB) = CONST * FDIV (A(ITSUB),A(K),IND)
          ITSUB = ITSUB + IONE
 830    CONTINUE
        K = K + IONE
 840  CONTINUE
C
      ITSUB = MRTBEG
      DO 850 I=1,NROWS
        A(ITSUB) = CONST
        ITSUB    = ITSUB + IONE
 850  CONTINUE
      GO TO 900
C
C     COMPUTE CPERCENTAGES OR CPROPORTIONS TABLE.
C
 860  CONST = RONE
      IF (LTWO.EQ.12) CONST = SPCC
      K = MCTBEG
      DO 880 I=1,NCOLS
        ITSUB = M + (I-IONE)
        DO 870 J=1,NROWS
          A(ITSUB) = CONST * FDIV (A(ITSUB),A(K),IND)
          ITSUB    = ITSUB + NCOLS
 870    CONTINUE
        K = K + IONE
 880  CONTINUE
C
      K = MCTBEG
      DO 890 I=1,NCOLS
        A(K) = CONST
        K    = K + IONE
 890  CONTINUE
      GO TO 900
C
C     ==================================================================
C
C     START AUTOMATIC PRINTING.
C
 900  CALL TABPRT (ISUM,L,M,MRTBEG,MTOTAL,
     1             N,NCOLS,NL,NROWS,NT,NTOTLS,ABLANK)
C
C     ..................................................................
C
C     START STORING.
C
      IF (MSTORE.NE.IZERO) CALL TABSTR (KSTORE,L,M,N,NCOLS,NROWS,NL,NT)
C
      RETURN
C
C     ==================================================================
C
      END
*TABPRT
      SUBROUTINE TABPRT (ISUM,L,M,MR,MT,N,NCOLS,NL,NROWS,NT,NTOTLS,AB)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TABPRT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT FOR TABLE INSTRUCTIONS.
C
C     MTOTAL = 0, DO NOT PRINT TOTALS
C              1, PRINT ROW AND COLUMN TOTALS
C              2, ONLY PRINT COLUMN TOTALS AT BOTTOM
C              3, ONLY PRINT ROW TOTALS ON RIGHT
C
C     MTOTAL IS COMPUTED IN PROGRAM UNIT TABLE AND PASSED AS MT.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JULY, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION KS(6), LINEPR(120), MA(11), NL(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             AB
      REAL             X
      REAL             SPCA
C
C     ..................................................................
C
      CHARACTER LA*1
      CHARACTER LHEAD*1
      CHARACTER LINEPR*1, MA*3
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IS    /  5 /
      DATA NX    / 11 /
      DATA LPAGE / 52 /
C
      DATA MA( 1), MA( 2), MA( 3), MA( 4), MA( 5), MA( 6) /
     1      '-WA',  'Y T',  'ABL',  'E O',  'F  ',  'OF ' /
      DATA MA( 7), MA( 8), MA( 9), MA(10), MA(11) /
     1      'MEA',  'SUR',  'EME',  'NTS',  'IN ' /
C
      DATA SPCA / 100.0 /
C
C     ==================================================================
C
C     START AUTOMATIC PRINTING.
C
C     SET LTWO = L2 MOD (NITB).
C
      LTWO = L2
      IF (L2.GT.NITB) LTWO = L2 - NITB
C
C     SET UP PAGE LENGTH.
C
      LPAGE = 52
      IF (NCRT.NE.IZERO) LPAGE = LENGTH - IFOUR
C
C     MPRINT = 1, FOR  TABLE INSTRUCTIONS
C            = 0, FOR NTABLE INSTRUCTIONS.
C
      MPRINT = IONE
      IF (L2.GT.NITB) MPRINT = IZERO
      IF (MPRINT.EQ.IONE) CALL PAGE (IFOUR)
      IF (MPRINT.EQ.IZERO .AND. NPAGE.EQ.IZERO) CALL PAGE (0)
C
C     PRINTING CONSTANTS.
C
C                (1)              (2)              (3)              (4)
C               ROWS             COLS             LEFT
C            (2) LEVELS       (1) LEVELS       OTHER LEVELS        TABLE
C
C     TYPE       NTR              NTC              NTL              NTT
C     BLANKS     NBR              NBC              NBL              NBT
C     WIDTH      NWR              NWC              NWL              NWT
C     DECIMALS   NDR              NDC              NDL              NDT
C
C     LENGTH    NL(2)            NL(1)              LL               NT
C     START     A(LC)            A(L)             A(LL)             A(M)
C
C     SET UP PRINTING CONSTANTS.
C
      K      = IZERO
      LC     = L + NL(1)
      LL     = L + NL(1) + NL(2)
      NM1    = N - IONE
      NM2    = N - ITWO
      NEMPTY = IZERO
      MSWDTH = IZERO
      NBST   = IONE
C
C     KM IS INDEX FOR PRINTING TOTALS AT RIGHT IF LL=1.
C
      KM = M + NT
C
C     KBT IS INDEX FOR PRINTING TOTALS AT BOTTOM IF LL=1.
C
      KBT = M + NT + NROWS
C
C     SET UP NW, ND, AND NB FOR PRINTING LEVELS AND TABLE.
C
C     COMPUTE NBR, NWR AND NDR FOR PRINTING ROW LEVELS.
C
      CALL MINNW (A(L),NL(1),IS,NX,LINEPR,IZERO,NW,ND,NWR,NDR)
      NTR = 7
      IF (NDR.EQ.IZERO) NTR = 9
      IF (NWR.LT.NX) GO TO 10
      CALL RFORMT (0,IS,A(L),RC(1),NL(1),NX-1,NWR,NDR,LINEPR,IRF)
      NTR = IONE
  10  NBR = ITWO
      NWR = MAX0 (NWR,ITHRE)
      IF (N.LT.ITHRE) NWR = MAX0 (NWR,6)
      IF (N.EQ.IONE) GO TO 40
C
C     COMPUTE NBC, NWC AND NDC FOR PRINTING COLUMN LEVELS.
C
      CALL MINNW (A(LC),NL(1),IS,NX,LINEPR,IZERO,NW,ND,NWC,NDC)
      NTC = 7
      IF (NDC.EQ.IZERO) NTC = 9
      IF (NWC.LT.NX) GO TO 20
      CALL RFORMT (0,IS,A(LC),RC(1),NL(1),NX-1,NWC,NDC,LINEPR,IRF)
      NTC = IONE
  20  NBC = ITWO
      NWC = MAX0 (NWC,ITHRE)
      IF (N.EQ.ITWO) GO TO 40
C
C     COMPUTE NBL, NWL AND NDL FOR PRINTING LEVELS ON LEFT.
C
      ITEMP = M - LL
      CALL MINNW (A(LL),ITEMP,IS,NX,LINEPR,IZERO,NW,ND,NWL,NDL)
      NTL = 7
      IF (NDL.EQ.IZERO) NTL = 9
      IF (NWL.LT.NX) GO TO 30
      CALL RFORMT (0,IS,A(LL),RC(1),ITEMP,NX-1,NWL,NDL,LINEPR,IRF)
      NTL = IONE
  30  NBL = ITWO
      NWL = MAX0 (NWL,ITHRE)
C
C     COMPUTE NBT, NWT AND NDT FOR PRINTING TABLE.
C
  40  IF (LTWO.LE.8) GO TO 50
      NTT = 7
      NWT = 6
      NDT = ITWO
      IF (LTWO.EQ.ITEN .OR. LTWO.GE.13) NDT = IFOUR
      GO TO 80
C     MOVE NON-EMPTY CELLS TO TOTAL CODE AREA.
C
  50  NFULL = IZERO
      ITSUB = M
      IASUB = IONE
      DO 70 I=1,NT
        IF (A(ITSUB).EQ.AB) GO TO 60
        NFULL = NFULL + IONE
        A(IASUB) = A(ITSUB)
        IASUB = IASUB + IONE
  60    ITSUB = ITSUB + IONE
  70  CONTINUE
C
      CALL MINNW (A(1),NFULL,IS,NX,LINEPR,IZERO,NW,ND,NWT,NDT)
      NTT = 7
      IF (NDT.EQ.IZERO) NTT = 9
      IF (NWT.LT.NX) GO TO 80
      CALL RFORMT (0,IS,A,RC(1),NFULL,NX-1,NWT,NDT,LINEPR,IRF)
      NTT = IONE
  80  NBT = ITWO
C
C     ADJUST NBC AND/OR NBT FOR DIFFERENCE BETWEEN NWC AND NWT
C
      IF (NWC.GT.NWT) NBT = NBT + (NWC-NWT)
      IF (NWT.GT.NWC) NBC = NBC + (NWT-NWC)
      NWT = MAX0 (NWT,ITHRE)
      IF (MT.EQ.IZERO .OR. MT.EQ.ITWO) GO TO 140
C
C     SET UP NWS, NDS AND NBS PRINTING CONSTANTS FOR PRINTING TOTALS.
C
      IF (LTWO.LE.8) GO TO 90
      NTS = 7
      NWS = 6
      NDS = ITWO
      IF (LTWO.EQ.ITEN .OR. LTWO.GE.13) NDS = IFOUR
      GO TO 120
C
C     MOVE NON-EMPTY CELLS TO TOTAL CODE AREA.
C
  90  ITSUB = MR
      A(1)  = ABS (A(ITSUB))
      A(2)  = A(1)
      DO 110 I=1,NTOTLS
        IF (A(ITSUB).EQ.AB) GO TO 100
        X = ABS (A(ITSUB))
        A(1)  = AMIN1 (X,A(1))
        A(2)  = AMAX1 (X,A(2))
 100    ITSUB = ITSUB + IONE
 110  CONTINUE
      A(1) = -A(1)
      A(2) = -A(2)
C
      CALL MINNW (A(1),ITWO,IS,NX,LINEPR,IZERO,NW,ND,NWS,NDS)
      NTS = 7
      IF (NDS.EQ.IZERO) NTS = 9
 120  NBS = ITWO
C
C     MSWDTH = TOTAL WIDTH FOR PRINTING TOTALS ON RIGHT.
C
      MSWDTH = NWS + NBS
      NDIFF  = MSWDTH - (NBT+NWT)
      IF (NDIFF.GT.IZERO) NBT = NBT + NDIFF
      IF (NDIFF.GT.IZERO) NBC = NBC + NDIFF
      IF (MSWDTH.GE.8) GO TO 130
      NBS    = NBS + (8-MSWDTH)
      MSWDTH = 8
 130  NBST   = MSWDTH - 6
C
C     MRWDTH = TOTAL WIDTH FOR PRINTING ROW LEVELS.
C
 140  MRWDTH = NWR + NBR
C
C     MLWDTH = TOTAL WIDTH FOR PRINTING OTHER LEVELS.
C
      MLWDTH = NWL + NBL
C
C     MTWDTH = TOTAL WIDTH OF CELL IN TABLE.
C
      MTWDTH = NWT + NBT
C
C     SET NB FOR BLANKING OUT LINEPR.
C
      NB = MRWDTH + NM2 * MLWDTH
      IF (N.EQ.IONE) NB = MRWDTH
      NB = MIN0 (NB,LWIDE)
C
C     DETERMINE NUMBER OF COLUMNS PER PAGE TO BE PRINTED, NCPERP.
C
      LENPG = LWIDE - MRWDTH - IONE
      IF (N.GT.ITWO) LENPG = LENPG - NM2 * MRWDTH
      NCPERP = IDIV (LENPG,MTWDTH,IND)
      NREMDR = MOD (NCOLS,NCPERP)
      IF (NCPERP.GT.IZERO) GO TO 150
      CALL ERROR (245)
      RETURN
C
C     ..................................................................
C
C     DETERMINE NUMBER OF PAGES TO BE PRINTED, NPAGES.
C
 150  NPAGES = IONE + IDIV (NCOLS,NCPERP,IND)
      IF (NREMDR.EQ.IZERO .AND. MT.EQ.IZERO) NPAGES = NPAGES - IONE
      IF (NREMDR.EQ.IZERO .AND. MT.EQ.ITWO ) NPAGES = NPAGES - IONE
C
C     ITOTSW = 0, IF THERE IS ROOM TO PRINT TOTALS ON RIGHT OR MT = 0,
C            = 1, IF THERE IS NOT ENOUGH ROOM TO PRINT TOTALS ON RIGHT.
C
      ITOTSW = IZERO
      IF (MT.EQ.IONE  .AND. NREMDR.EQ.IZERO .AND. N.GT.IONE)
     1          ITOTSW = IONE
      IF (MT.EQ.ITHRE .AND. NREMDR.EQ.IZERO .AND. N.GT.IONE)
     1          ITOTSW = IONE
C
C     DETERMINE LENGTH OF PAGE.
C
      LBREAK = LPAGE - N
      IF (N.EQ.IONE) GO TO 180
      MULT = NL(1) + IONE
      IF (N.EQ.ITWO) GO TO 170
      DO 160 I=3,N
        MULT = NL(I) * MULT
        IF (MULT.LE.LBREAK) GO TO 160
        MULT = IDIV (MULT,NL(I),IND)
        GO TO 170
 160  CONTINUE
 170  IF (MULT.GT.LBREAK) GO TO 180
      MRATIO = IDIV (LBREAK,MULT,IND)
      LBREAK = MULT * MRATIO
 180  NLINES = LBREAK + IONE
      IF (N.LT.ITHRE) GO TO 200
C
C     SET UP STARTING LOCATIONS FOR PRINTING LEVELS ON LEFT.
C        KS(I) = LOCATION OF FIRST LEVEL OF CLASSIFICATION I GT 2.
C
      KS(1) = M - NL(N)
      IF (N.LE.ITHRE) GO TO 200
      DO 190 IKS=2,NM2
        ITEMP = N - IKS + IONE
        KS(IKS) = KS(IKS-1) - NL(ITEMP)
 190  CONTINUE
C
C     OUTER LOOP ON NUMBER OF PAGES.
C
 200  KEND = 12 * (N+IONE)
      KBEG = KEND - 11
C
      DO 560 IPAGE=1,NPAGES
        NPGSWT = IZERO
        KL3    = L + NL(1) + (IPAGE - IONE) * NCPERP
        KT     = M + (IPAGE-IONE) * NCPERP
        IF (IPAGE.EQ.NPAGES .AND. NREMDR.NE.IZERO) NCPERP = NREMDR
C
C       (1)   PRINT TITLE.
C
        IF (IPAGE.NE.IONE) CALL PAGE (IFOUR)
C
        IF (LTWO.EQ.IONE)  WRITE (IPRINT,570) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.ITWO)  WRITE (IPRINT,580) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.ITHRE) WRITE (IPRINT,590) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.IFOUR) WRITE (IPRINT,600) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.IFIVE) WRITE (IPRINT,610) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.6)     WRITE (IPRINT,620) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.7)     WRITE (IPRINT,630) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.8)     WRITE (IPRINT,640) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,11), (LHEAD(K),K=KBEG,KEND)
        IF (LTWO.EQ.9)     WRITE (IPRINT,650) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.ITEN)  WRITE (IPRINT,660) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.11)    WRITE (IPRINT,670) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.12)    WRITE (IPRINT,680) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.13)    WRITE (IPRINT,690) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
        IF (LTWO.EQ.14)    WRITE (IPRINT,700) N, (MA(I),I=1,5), MA(6),
     1                     NRMAX, (MA(J),J=7,10)
C
        IF (IPAGE.GT.IONE) WRITE (IPRINT,740)
C
C       (2)   PRINT IDENTIFICATION.
C
        WRITE (IPRINT,720)
        JBEG = IONE
        DO 210 I=1,N
          JEND = JBEG + 11
          WRITE (IPRINT,710) LA(42), I, LA(43), (LHEAD(J),J=JBEG,JEND),
     1                        NL(I)
          JBEG = JBEG + 12
 210    CONTINUE
C
C       (3)   PRINT FREQUENCY TABLE.
C                A NROWS X NCOLS = NT TABLE IS PRINTED.
C                NB2 = (N-2) COLUMNS APPEAR ON THE LEFT.
C
C       INNER LOOP ON NUMBER OF ROWS PRINTED.
C
        KR = L
        DO 490 I=1,NROWS
          IF (NLINES.GT.IZERO .AND. NLINES.LT.LBREAK .AND.
     1        NPGSWT.NE.IZERO) GO TO 330
          NPGSWT = IONE
          IF (I.NE.IONE) CALL PAGE (IFOUR)
          IF (I.NE.IONE) WRITE (IPRINT,740)
C
C         (4)   PRINT HEADING AND LEVELS OF CLASSIFICATION TWO (IF N.NE.1).
C
          WRITE (IPRINT,720)
          IF (N.EQ.IONE) GO TO 280
C
C         BLANK OUT FIRST (N-1) COLUMNS.
C
          DO 220 II=1,NB
            LINEPR(II) = LA(45)
 220      CONTINUE
C
          K = MRWDTH - ITWO
          IF (N.GT.ITWO) K = K + (N-ITWO) * MLWDTH
          LINEPR(K)   = LA(42)
          LINEPR(K+1) = LA(3)
          LINEPR(K+2) = LA(43)
          K = K + ITHRE
C
C         KL2 IS INDEX FOR PRINTING LEVELS OF CLASS 2, IF N.NE.1.
C
          IF (IPAGE.EQ.NPAGES .AND. ITOTSW.EQ.IONE) GO TO 240
          KL2 = KL3
          DO 230 II=1,NCPERP
            CALL RFORMT (NTC,IS,RC,A(KL2),NBC,0,NWC,NDC,LINEPR(K),IRF)
            K   = K + MTWDTH
            KL2 = KL2 + IONE
 230      CONTINUE
C
 240      IF (MT.EQ.IZERO .OR. MT.EQ.ITWO) GO TO 260
          IF (IPAGE.NE.NPAGES) GO TO 260
          DO 250 II=1,NBST
            LINEPR(K) = LA(45)
            K = K + IONE
 250      CONTINUE
          LINEPR(K)   = LA(30)
          LINEPR(K+1) = LA(25)
          LINEPR(K+2) = LA(30)
          LINEPR(K+3) = LA(11)
          LINEPR(K+4) = LA(22)
          LINEPR(K+5) = LA(29)
          K = K + 5
          GO TO 270
 260      K = K - IONE
 270      WRITE (IPRINT,730) (LINEPR(II),II=1,K)
C
C       (5)   PRINT HEADINGS FOR OTHER CLASSIFICATIONS.
C
C       BLANK OUT FIRST (N-1) COLUMNS.
C
 280      DO 290 II=1,NB
            LINEPR(II) = LA(45)
 290      CONTINUE
C
          IF (N.LT.ITHRE) GO TO 310
          K = MLWDTH - ITWO
          DO 300 II=1,NM1
            LINEPR(K) = LA(42)
            LASUB = N - II + ITWO
            LINEPR(K+1) = LA(LASUB)
            LINEPR(K+2) = LA(43)
            IF (II.LT.NM2) K = K + MLWDTH
            IF (II.GE.NM2) K = K + MRWDTH
 300      CONTINUE
          K = K - MRWDTH + ITHRE
          LINEPR(K-2) = LA(2)
 310      IF (N.GE.ITHRE) GO TO 320
          LINEPR(MRWDTH-2) = LA(42)
          LINEPR(MRWDTH-1) = LA( 2)
          LINEPR(MRWDTH)   = LA(43)
          K = MRWDTH
 320      WRITE (IPRINT,730) (LINEPR(II),II=1,K)
          NLINES = IZERO
 330      IF (N.EQ.IONE) GO TO 360
          IF (NL(1).EQ.IONE) GO TO 360
          NCHROW = MOD (I,NL(1))
          IF (NCHROW.EQ.IONE) GO TO 360
          IF (NLINES.EQ.IZERO .AND. I.NE.IONE) K = K - MRWDTH + IONE
          IF (NL(1).EQ.ITWO) GO TO 340
          IF (NCHROW.EQ.ITWO) GO TO 340
          GO TO 410
C
C         BLANK OUT COLUMNS ON LEFT.
C
 340      DO 350 IB=1,NB
            LINEPR(IB) = LA(45)
 350      CONTINUE
          GO TO 410
C
C         PRINT LEVELS ON LEFT.
C
 360      K = IONE
          IF (N.LT.ITHRE) GO TO 400
          NLPROD = IDIV (NT,NL(2),IND)
          JRS = IONE
          DO 390 JL=1,NM2
            NREV   = N - JL + IONE
            NLPROD = IDIV (NLPROD,NL(NREV),IND)
            JMOD   = MOD (I,NLPROD)
            IF (JMOD.NE.IONE .AND. NL(1).NE.IONE) GO TO 380
            IF (NLINES.EQ.IZERO .AND. IPAGE.NE.IONE)
     1          KS(JL) = KS(JL) - NL(NREV)
            JLSUB = KS(JL)
C
C           CALL RFORMT FOR LEVEL TO BE PRINTED ON THE LEFT.
C
            CALL RFORMT (NTL,IS,RC,A(JLSUB),NBL,0,NWL,NDL,LINEPR(K),IRF)
C
C           INCREASE LEVEL OF CLASSIFICATION JL.
C
            KS(JL) = KS(JL) + IONE
C
C           RESET LEVELS OF LOWER CLASSIFICATIONS ONLY ONCE.
C
            IF (I.EQ.IONE) GO TO 380
            IF (JRS.EQ.IZERO) GO TO 380
            JRBEG = JL + IONE
            IF (JRBEG.GT.NM2) GO TO 380
            DO 370 JR=JRBEG,NM2
              NLSUB = N - JR + IONE
              KS(JR) = KS(JR) - NL(NLSUB)
 370          CONTINUE
            JRS = IZERO
 380        K = K + MLWDTH
 390      CONTINUE
 400      IF (N.EQ.IONE) GO TO 410
          IF (NL(1).NE.IONE) WRITE (IPRINT,720)
          IF (NL(1).NE.IONE) NLINES = NLINES + IONE
C
C         PRINT LEVEL FOR ROWS.
C
          KR = L
 410      CALL RFORMT (NTR,IS,RC,A(KR),NBR,0,NWR,NDR,LINEPR(K),IRF)
          KR = KR + IONE
C
C         (6)   PRINT ENTRIES IN TABLE.
C
          K   = K + MRWDTH
          KTT = KT
          IF (ITOTSW.EQ.IONE .AND. IPAGE.EQ.NPAGES) GO TO 450
          DO 440 JC=1,NCPERP
            IF (A(KTT).NE.RPIFY) GO TO 420
C
C           PRINT ASTERISK, IF ENTRY IN TABLE IS EMPTY.
C
            CALL RFORMT (11,IS,RC,A(KTT),NBT,0,NWT,NDT,LINEPR(K),IRF)
            K = K + MTWDTH
            LINEPR(K-1) = LA(41)
            NEMPTY = NEMPTY + IONE
            GO TO 430
C
C           PRINT ENTRY IN TABLE NORMALLY.
C
 420        CALL RFORMT (NTT,IS,RC,A(KTT),NBT,0,NWT,NDT,LINEPR(K),IRF)
            K   = K + MTWDTH
 430        KTT = KTT + IONE
 440      CONTINUE
          IF (N.EQ.IONE) KT = KT + IONE
          IF (N.NE.IONE) KT = KT + NL(2)
          IF (MT.EQ.IZERO .OR. MT.EQ.ITWO) GO TO 480
C
C         PRINT TOTALS ON RIGHT.
C
 450      IF (N.EQ.IONE) GO TO 480
          IF (IPAGE.NE.NPAGES) GO TO 480
          IF (A(KM).NE.RPIFY) GO TO 460
          CALL RFORMT (11,IS,RC,A(KM),NBS,0,NWS,NDS,LINEPR(K),IRF)
          K = K + MSWDTH
          LINEPR(K-1) = LA(41)
          GO TO 470
C
 460      CALL RFORMT (NTS,IS,RC,A(KM),NBS,0,NWS,NDS,LINEPR(K),IRF)
          K  = K + MSWDTH
 470      KM = KM + IONE
 480      K  = K - IONE
          WRITE (IPRINT,730) (LINEPR(LI),LI=1,K)
          NLINES = NLINES + IONE
          K = NM2 * MLWDTH + IONE
 490    CONTINUE
C
        IF (MT.EQ.IZERO .OR. MT.EQ.ITHRE) GO TO 560
        IF (NROWS.EQ.IONE) GO TO 560
C
C       PRINT TOTALS AT BOTTOM.
C
        K = MRWDTH - IFIVE
        IF (N.EQ.IONE)  K = K + IONE
        IF (N.GE.ITHRE) K = K + NM2 * MLWDTH
        DO 500 I=1,NB
          LINEPR(I) = LA(45)
 500    CONTINUE
C
        LINEPR(K)   = LA(30)
        LINEPR(K+1) = LA(25)
        LINEPR(K+2) = LA(30)
        LINEPR(K+3) = LA(11)
        LINEPR(K+4) = LA(22)
        IF (N.NE.IONE) LINEPR(K+5) = LA(29)
        K = K + 6
        IF (N.EQ.IONE) K = K - IONE
        IF (ITOTSW.EQ.IONE .AND. IPAGE.EQ.NPAGES) GO TO 540
        DO 530 I=1,NCPERP
          IF (A(KBT).NE.RPIFY) GO TO 510
          CALL RFORMT (11,IS,RC,A(KBT),NBT,0,NWT,NDT,LINEPR(K),IRF)
          K = K + MTWDTH
          LINEPR(K-1) = LA(41)
          GO TO 520
 510      CALL RFORMT (NTT,IS,RC,A(KBT),NBT,0,NWT,NDT,LINEPR(K),IRF)
          K   = K + MTWDTH
 520      KBT = KBT + IONE
 530    CONTINUE
        IF (N.GT.IONE .AND. IPAGE.EQ.NPAGES .AND. MT.NE.ITWO) GO TO 540
        K = K - IONE
        GO TO 550
 540    IF (LTWO.EQ.9 .OR. LTWO.EQ.11 .OR. LTWO.EQ.12) A(ISUM) = SPCA
        IF (LTWO.EQ.ITEN .OR. LTWO.EQ.13 .OR. LTWO.EQ.14) A(ISUM) = RONE
        CALL RFORMT (NTS,IS,RC,A(ISUM),NBS,0,NWS,NDS,LINEPR(K),IRF)
        K = K + MSWDTH - IONE
 550    WRITE (IPRINT,720)
        WRITE (IPRINT,730) (LINEPR(I),I=1,K)
        IF (I.EQ.NROWS) NLINES = IZERO
 560  CONTINUE
C
      IF (LTWO.GE.ITWO .AND. LTWO.LE.8) WRITE (IPRINT,750) NEMPTY, NT
C
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 570  FORMAT (1X,I1,4A3,A2,11HFREQUENCIES   ,1X,A3,I4,1X,4A3)
 580  FORMAT (1X,I1,4A3,A2, 4HSUMS          ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 590  FORMAT (1X,I1,4A3,A2, 8HAVERAGES      ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 600  FORMAT (1X,I1,4A3,A2,19HSTANDARD DEVIATIONS,1X,A3,I4,1X,4A3,1X,A3,
     1     12A1)
 610  FORMAT (1X,I1,4A3,A2, 8HMINIMUMS      ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 620  FORMAT (1X,I1,4A3,A2, 8HMAXIMUMS      ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 630  FORMAT (1X,I1,4A3,A2, 6HRANGES        ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 640  FORMAT (1X,I1,4A3,A2, 7HMEDIANS       ,1X,A3,I4,1X,4A3,1X,A3,12A1)
 650  FORMAT (1X,I1,4A3,A2,11HPERCENTAGES   ,1X,A3,I4,1X,4A3)
 660  FORMAT (1X,I1,4A3,A2,11HPROPORTIONS   ,1X,A3,I4,1X,4A3)
 670  FORMAT (1X,I1,4A3,A2,19HPERCENTAGES IN ROWS,1X,A3,I4,1X,4A3)
 680  FORMAT (1X,I1,4A3,A2,22HPERCENTAGES IN COLUMNS,1X,A3,I4,1X,4A3)
 690  FORMAT (1X,I1,4A3,A2,19HPROPORTIONS IN ROWS,1X,A3,I4,1X,4A3)
 700  FORMAT (1X,I1,4A3,A2,22HPROPORTIONS IN COLUMNS,1X,A3,I4,1X,4A3)
 710  FORMAT (8X,A1,I1,A1,3H = ,12A1,1H,,I3,7H LEVELS)
 720  FORMAT (1X)
 730  FORMAT (1X,119A1)
 740  FORMAT (16X,12HCONTINUATION)
 750  FORMAT (/1X,1X,2H* ,I4,8H OF THE ,I4,30H CELLS IN THE TABLE ARE EM
     1PTY.)
C
C     ==================================================================
C
      END
*TABSTR
      SUBROUTINE TABSTR (KSTORE,L,M,N,NCOLS,NROWS,NL,NT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TABSTR V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     STORE RESULTS FOR TABLE INSTRUCTIONS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1978.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NL(*)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C     STORE LEVELS AND TABLE IN WORKSHEET.
C
C     (1)   FIRST COLUMN.
C        ROW 1 = LENGTH OF TABLE = NROWS = NT/NL(2)
C        ROW 2 = WIDTH  OF TABLE = NL(2) = NCOLS
C        ROW 3 = SIZE   OF TABLE = NT = PRODUCT OF NL(I)
C        ROW 4 = NUMBER OF COLUMNS STORED = 1 + NL(2) + N
C        ROW 5 = COLUMN NUMBER WHERE STORAGE OF LEVELS STARTS
C        ROW 6 = COLUMN NUMBER WHERE STORAGE ENDS.
C
C     (2)   STORE NROWS X NCOLS TABLE.
C
C     (3)   FOR EACH CLASSIFICATION I,
C              STORE NUMBER OF LEVELS NL(I) IN ROW 1, AND
C              STORE LEVELS CONSECUTIVELY STARTING IN ROW 2.
C
      NL2 = NL(2)
      IF (N.EQ.IONE) NL2 = IONE
      IF (NROWS.LE.NROW) GO TO 10
      CALL ERROR (226)
      NROWS  = NROW
  10  NSTORE = KSTORE + NL2 + N
      NCLASS = N
      NSCOLS = NCOLS
      IF (NSTORE.LE.NCOL) GO TO 20
      CALL ERROR (213)
      NSTORE = NCOL
      IEND   = NSTORE - KSTORE + IONE
      NSCOLS = MIN0 (NCOLS,IEND-KSTORE)
      NCLASS = MIN0 (N,IEND-KSTORE-NSCOLS)
      GO TO 30
C
  20  IEND = IONE + NL2 + N
  30  DO 40 I=1,IEND
        IARGS(I) = KSTORE
        KIND(I)  = IZERO
        KSTORE   = KSTORE + IONE
  40  CONTINUE
      NARGS = IEND
      CALL CHKCOL
C
C     (1) STORE INFORMATION IN FIRST COLUMN.
C
      K       = IARGS(1)
      RC(K)   = FLOAT(NROWS)
      RC(K+1) = FLOAT (NL2)
      RC(K+2) = FLOAT (NT)
      RC(K+3) = FLOAT (IEND)
      RC(K+4) = FLOAT (NSTORE-N+IONE)
      RC(K+5) = FLOAT (NSTORE)
C
      IF (NSCOLS.LT.IONE) RETURN
C
C     (2)   STORE TABLE.
C
      DO 60 I=1,NSCOLS
        IRCSUB = IARGS(I+1)
        IASUB  = M + I - IONE
        DO 50 J=1,NROWS
          RC(IRCSUB) = A(IASUB)
          IF (A(IASUB).EQ.RPIFY) RC(IRCSUB) = RZERO
          IRCSUB = IRCSUB + IONE
          IASUB  = IASUB + NSCOLS
  50    CONTINUE
  60  CONTINUE
      IF (NCLASS.LT.IONE) RETURN
C
C     (3)   STORE LEVELS.
C
      KK = L
      ITEMP = IONE + NCOLS
      DO 80 I=1,NCLASS
        ITEMP = ITEMP + IONE
        K = IARGS(ITEMP)
        RC(K) = FLOAT(NL(I))
        JEND = NL(I) + IONE
        IF (JEND.GT.NROW) GO TO 10
        DO 70 J=2,JEND
          K = K + IONE
          RC(K) = A(KK)
          KK = KK + IONE
  70    CONTINUE
  80  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*TEKINT
      SUBROUTINE TEKINT
C
C **  NBS OMNITAB 1980 VERSION 6.03 10/22/82. TEKINT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE IS USED WITH TEKTRONIX COMMANDS.
C
C     INSTRUCTIONS ARE ...
C
C        TEKTRONIX PLOT                                       L2 = 16
C
C     TEKTRONIX PLOT (N) NO. OF CURVES (E) OPTS. (C),(C),...,(C) VS (C)
C     TEKTRONIX PLOT (N), (E), (C) VS (C), (C) VS (C),...,(C) VS (C)
C     TEKTRONIX PLOT (N), (E) Y LIMITS (K), (K),(C),(C),...,(C) VS (C)
C     TEKTRONIX PLOT (N),(E),(C),...,(C) VS (C), X-LIMITS (K), (K)
C     TEKTRONIX PLOT (N), (E), (K),(K), (C),...,(C) VS (C), (K), (K)
C     TEKTRONIX PLOT (N), (E), (K), (K), (C) VS (C),,(C) VS (C),(K),(K)
C
C        TEKTRONIX AXIS (K) Y-AXIS, (K) X-AXIS                L2 = 17
C
C        TEKTRONIX TERMINAL (N) TERMINAL NO., (N) BAUD RATE   L2 = 18
C     (DEFAULT PARAMETERS ARE 4014 FOR TERMINAL AND 9600 FOR BAUD.)
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTATION AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG,MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1982.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
C
      INCLUDE 'WRKSCR.H'
C
      DIMENSION IASCII(10)
C
      REAL             XLIMIT(4), YLIMIT(4)
      REAL             SPCA, SPCB, SPCC
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IASCII( 1) / 27 /
      DATA IASCII( 2) / 28 /
      DATA IASCII( 3) / 29 /
      DATA IASCII( 4) / 30 /
      DATA IASCII( 5) / 31 /
      DATA IASCII( 6) / 32 /
      DATA IASCII( 7) / 33 /
      DATA IASCII( 8) / 34 /
      DATA IASCII( 9) / 35 /
      DATA IASCII(10) / 36 /
      DATA SPCA /     0.5000001 /
      DATA SPCB /     9.0       /
      DATA SPCC /     9.0       /
C
C     ==================================================================
C
      IF (L2.NE.16) GO TO 20
C
C     INSTRUCTION IS TEKTRONIX PLOT.
C
      HOLDHG = HGT
      HOLDXH = XDH
      HGT    = TEKHGT
      XDH    = TEKXDH
      L2     = ITEN
      CALL CALCOM (XLIMIT,YLIMIT,LCR)
      IF (LCR.EQ.IZERO) GO TO 10
      IF (NERROR.NE.IZERO) GO TO 10
C
C      EVERYTHING O.K. CONTINUE WITH PLOTTING.
C     SET ITEKSW = 1. SWITCH TO DETRMINE STARTING ORIGIN OF PLOT.
C
      IF (ITEKSW .NE. -IONE) ITEKSW = IONE
      CALL CALPLT (XLIMIT,YLIMIT,LCR)
C
C     CLOSE INTERMEDIATE PLOT FILE AND
C     GENERATE PLOT ON TEKTRONIX SCREEN.
C
C     REWIND 46
C     CALL INTTEK (46)
C
  10  HGT =HOLDHG
      XDH = HOLDXH
      L2  = 16
C     REWIND 46
      RETURN
C
  20  IF (L2.EQ.18) GO TO 40
C
C     INSTRUCTION IS TEXTRONIX AXIS.
C
      IF (NERROR.NE.IZERO) RETURN
      IF (NARGS.LT.ITWO) GO TO 30
      IF (KIND(1).EQ.IZERO) ARGS(1) = IARGS(1)
      IF (KIND(2).EQ.IZERO) ARGS(2) = IARGS(2)
      TEKXDH = AINT (ARGS(2) + SPCA)
      TEKHGT = AINT (ARGS(1) + SPCA)
      IF (TEKXDH.LE.9.0 .AND. TEKHGT.LE.9.0) RETURN
C
C
C  MODIFIED JULY 1992.  NO LONGER NEED REQUIREMENT LIMITING TEKTRONIX
C  PLOT AXIS TO LESS THAN 9 INCHES.  NEXT 3 STATEMENTS COMMENTED OUT.
CCCCC CALL ERROR (305)
C
CCCCC IF (TEKXDH.GT.9.0) XDH = SPCB
CCCCC IF (TEKHGT.GT.9.0) XDH = SPCC
      RETURN
C
C
  30  CALL ERROR (29)
      RETURN
C
C     INSTRUCTION IS TEKTRONIX TERMINAL.
C
  40  IF (NARGS.EQ.2) GO TO 50
C
C     CALL ERROR TO INDICATE ARGUMENTS MUST BE 2.
C
      NARGS = IONE
      CALL ERROR (29)
      RETURN
C
C     CHECK FOR TYPE OF TEKTRONIX TERMINAL.
C
  50  IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)
      IF (IARGS(1).GT.4013) GO TO 60
C
C     SET ITEK(1,2) = 1, IF IARGS(1) = 4010 OR 4012.
C
      ITEK(1,2) = IASCII(2)
      GO TO 70
C
C     SET ITEK(1,2) = 2, IF IARGS(1) NOT 4010 OR 4012
C
  60  ITEK(1,2) = IASCII(3)
C
C     DETERMINE CHAR/SEC.
C
  70  IF (KIND(2).EQ.IONE) IARGS(2) = ARGS(2) + SPCA
      ISPEED = IARGS(2) / ITEN
      ISTOP  = 3
      IF (ISPEED.GE.10000) ISTOP = 4
      IF (ISPEED.LT.1000)  ISTOP = 2
      INDEX  = ISTOP
      DO 80 I = 1,ISTOP
        IDGHT         = MOD (ISPEED,ITEN) + IONE
        ISPEED        = ISPEED / ITEN
        ITEK(INDEX,1) = IASCII(IDGHT)
        INDEX         = INDEX - IONE
  80  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*THERMO
      SUBROUTINE THERMO
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. THERMO V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE THERMODYNAMIC INSTRUCTIONS.
C
C     INSTRUCTIONS EXECUTED ARE EINSTEIN, PFTRANSLATIONAL,
C        PFATOMIC, PARTFUNCTION, AND BOLDISTRIBUTION.
C
C     THE VALUES OF L2 ARE --
C        5 - EINSTEIN FUNCTION
C        6 - PFTRANS ( PARTITION FUNCTION TRANSLATIONAL)
C        7 - PFATOM ( P.F. ATOMIC)
C        8 - PARTFUNCTION
C        9 - BOLDISTRIBUTION (BOLZMAN DISTRIBUTION)
C
C     FORMS OF INSTRUCTIONS ARE AS FOLLOWS ...
C
C     EINSTEIN  TEMP IN (E) VIB FREQ IN WAVE NO IN (E) STORE IN (C) ON
C        EINSTEIN  TEMP IN (E) FREQ IN (E) GAS CONST R=(K) START IN (C)
C     PFTRANS TEMP IN (E) MOL WT M IN (E) START STORING IN (E)
C     PFATOM  TEMP (E) MOL WT M (E) WAVE NO (C) DEGEN G (C) STORE (C) ON
C     PARTFUNC TEMP IN (E) WAVE NO IN (C) G IN (C) START STORING IN (C)
C     BOLDISTRIBUTION TEMP IN (E) WAVE NO IN (C) G IN (C) START STORING
C
C     SEE USER'S MANUAL FOR DETAILS ON STORAGE BY ALL INSTRUCTIONS.
C
C     BOLDISTRIBUTION STORES THE PERCENTAGE OF MOLECULES IN EACH OF THE
C        VIBRATIONAL ENERGY LEVELS.  IF THERE ARE N ENERGY LEVELS,
C        BOLDISTRIBUTION WILL USE N COLUMNS FOR STORAGE.
C
C     DIMENSION QQ(NS2)
C
C               WRITTEN BY -
C                      R. MCCLENON,
C                      NSRDS
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                  ORIGINAL VERSION - DECEMBER, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             CP, E, F, FE, HBYT, HE
      REAL             R, S, T, WT
      REAL             FDPCON, FLOG
      REAL             SPCA, SPCB, SPCC
C
      DOUBLE PRECISION QQ(10)
      DOUBLE PRECISION X, EXX, EXDIF, G, Q0, Q1, Q2
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
      DOUBLE PRECISION DPCA, DPCB
C
      EQUIVALENCE (QQ(1),A(1))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 2.5     /
      DATA SPCB / 1.5     /
      DATA SPCC / 3.66495 /
C
      DATA DPCA / 2.5D0     /
      DATA DPCB / 1.43879D0 /
C
C     ==================================================================
C
      IWT = IZERO
      KK  = IONE
      J2  = L2 - IFOUR
      R   = RONE
      E   = RZERO
      IF (J2.LT.IONE .OR. J2.GE.6) RETURN
      GO TO (10,50,70,90,100), J2
C
C     ..................................................................
C
C     THIS IS EINSTEIN.
C
  10  IF (NARGS.GE.IFIVE) GO TO 350
      IF (NARGS.LT.ITHRE) GO TO 350
      IF (NARGS.GT.ITHRE) GO TO 20
      CALL ADRESS (ITHRE,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      II = IARGS(3)
      GO TO 30
C
  20  CALL ADRESS (IFOUR,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      IF (KIND(3).NE.IONE) GO TO 360
      R = ARGS(3)
      II = IARGS(4)
      IF (R) 390,390,30
  30  CALL ADRESS (ITWO,IFQ)
      IF (IFQ.GT.IZERO) GO TO 40
      F = ARGS(2)
      IF (F) 390,390,40
  40  IF (NCOL-II.GE.7) GO TO 110
      GO TO 370
C
C     THIS IS PFTRANSLATIONAL.
C
  50  IF (NARGS.NE.ITHRE) GO TO 350
      CALL ADRESS (ITHRE,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      II = IARGS(3)
      CALL ADRESS (ITWO,IWT)
      IF (IWT.GT.IZERO) GO TO 60
      WT = ARGS(2)
      IF (WT.LE.RZERO) GO TO 390
  60  IF (NCOL-II.GE.6) GO TO 110
      IF (NCOL-II.LT.6) GO TO 110
C
C     THIS IS PFATOMIC.
C
  70  IF (NARGS.NE.IFIVE) GO TO 350
      CALL ADRESS (IFIVE,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      II = IARGS(5)
      CALL ADRESS (ITWO,IWT)
      IF (IWT.GT.IZERO) GO TO 80
      WT = ARGS(2)
      IF (WT.LE.RZERO) GO TO 390
  80  CALL ADRESS (ITHRE,IFQ)
      IF (IFQ.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (IFOUR,IG)
      IF (IG.LT.IZERO) CALL ERROR (20)
      IF (NCOL-II.GE.6) GO TO 110
      IF (NCOL-II.LT.6) GO TO 370
C
C     THIS IS PARTFUNCTION.
C
  90  IF (NARGS.NE.IFOUR) GO TO 350
      CALL ADRESS (IFOUR,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      II = IARGS(4)
      CALL ADRESS (ITWO,IFQ)
      IF (IFQ.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (ITHRE,IG)
      IF (IG.LT.IZERO) CALL ERROR (20)
      IWT = IZERO
      IF (NCOL-II.GE.ITHRE) GO TO 110
      GO TO 370
C
C     THIS IS BOLDISTRIBUTION.
C
 100  IF (NARGS.NE.IFOUR) GO TO 350
      CALL ADRESS (IFOUR,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      IWT = IZERO
      II = IARGS(4)
      CALL ADRESS (ITWO,IFQ)
      IF (IFQ.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (ITHRE,IG)
      IF (IG.LT.IZERO) CALL ERROR (20)
C
C     ..................................................................
C
 110  CALL ADRESS (IONE,ITP)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      IF (ITP.GT.IZERO) GO TO 120
      T = ARGS(1)
      IF (T) 390,390,120
 120  IE = IZERO
      IF (NRMAX.LE.IZERO) GO TO 380
      IF (L2.LT.8) GO TO 150
      DO 140 JB=1,NROW
        J = NROW + IONE - JB
        IIG = IG + J - IONE
        IF (RC(IIG)) 400,140,130
 130    KK = J
        IF (KK.LE.NS2) GO TO 150
        IF (L2.LE.8) GO TO 150
        GO TO 340
 140  CONTINUE
      GO TO 410
C
 150  DO 330 J=1,NRMAX
        IF (ITP.LT.IONE) GO TO 160
        IIT = ITP + J - IONE
        T = RC(IIT)
        IF (T) 320,160,160
 160    IF (IWT.GT.IZERO) GO TO 170
        IF (IWT.LT.IZERO) GO TO 180
        WT = RONE
        GO TO 180
 170    IIW = IWT + J - IONE
        WT = RC(IIW)
        IF (WT) 320,180,180
 180    IF (L2.LT.8) GO TO 220
        Q0 = DZERO
        Q1 = DZERO
        Q2 = DZERO
        DO 210 JJ=1,KK
          IIF = IFQ + JJ - IONE
          IIG = IG + JJ - IONE
          E = RC(IIF)
          G = RC(IIG)
          IF (G) 400,190,190
 190      IF (E) 310,200,200
 200      X = DPCB * FDDIV (DBLE(E),DBLE(T),IND)
          EXX = FDEXP (-X)
          Q0 = Q0 + G*EXX
          Q1 = Q1 + G * X*EXX
          Q2 = Q2 + G * X*X*EXX
          IF (L2.LE.8) GO TO 210
          QQ(JJ) = G*EXX
 210    CONTINUE
        GO TO 230
C
 220    Q0 = DONE
        Q1 = DZERO
        Q2 = DZERO
        IF (L2.LT.6) GO TO 240
 230    FE = SPCA * FLOG(T) + SPCB*FLOG(WT) - SPCC + FDPCON (FDLOG(Q0))
        HE = DPCA + FDDIV (Q1,Q0,IND)
        S = FE + HE
        CP = DPCA + FDDIV (Q2,Q0,IND) - FDDIV (Q1,Q0,IND)
     1            * FDDIV (Q1,Q0,IND)
        HBYT = HE*T
        GO TO 260
 240    IF (IFQ.LT.IONE) GO TO 250
        IIF = IFQ + J - IONE
        E = RC(IIF)
        IF (E) 310,250,250
 250    X = DPCB * FDDIV (DBLE(E),DBLE(T),IND)
        EXX = FDEXP (-X)
        EXDIF = DONE - EXX
        FE = -FDLOG (EXDIF) * R
        HE = (X*FDDIV (EXX,EXDIF,IND))*R
        CP = R*X*X*FDDIV (EXX,EXDIF*EXDIF,IND)
        S = FE + HE
        HBYT = HE*T
 260    K = I + J - IONE
        IF (L2.EQ.8) GO TO 280
        IF (L2.GT.8) GO TO 290
        IF (L2.GE.6) GO TO 270
        RC(K) = E
        K = K + NROW
 270    RC(K) = T
        K = K + NROW
        RC(K) = FE
        K = K + NROW
        RC(K) = HE
        K = K + NROW
        RC(K) = S
        K = K + NROW
        RC(K) = CP
        K = K + NROW
        RC(K) = HBYT
        GO TO 330
 280    RC(K) = Q0
        K = K + NROW
        RC(K) = Q1
        K = K + NROW
        RC(K) = Q2
        GO TO 330
 290    IF (NCOL-II.LT.KK) GO TO 370
        DO 300 JJ=1,KK
          RC(K) = FDPCON ( FDDIV (QQ(JJ),Q0,IND) )
          K = K + NROW
 300    CONTINUE
        GO TO 330
C
 310    Q0 = DONE
        Q1 = DZERO
        Q2 = DZERO
        IF (IE.NE.IZERO) GO TO 230
        CALL ERROR (229)
        IE = IONE
        GO TO 230
 320    FE = RZERO
        HE = RZERO
        CP = RZERO
        S = RZERO
        HBYT = RZERO
        IF (IE.NE.IZERO) GO TO 260
        CALL ERROR (233)
        IE = IONE
        GO TO 260
 330  CONTINUE
      RETURN
C
C     ..................................................................
C
 340  CALL ERROR (23)
      RETURN
 350  CALL ERROR (10)
      RETURN
 360  CALL ERROR (20)
      RETURN
 370  CALL ERROR (11)
      RETURN
 380  CALL ERROR (9)
      RETURN
 390  CALL ERROR (3)
      RETURN
 400  CALL ERROR (25)
      RETURN
 410  CALL ERROR (224)
      RETURN
C
C     ==================================================================
C
      END
*TRANSF
      SUBROUTINE TRANSF
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TRANSF V 7.00  5/16/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROVIDE TRANSFORMATIONS  B=UAU(T) AND  C=U(I)AU
C
C     L2 = 1,    TRANSFORMATION  B=UAU(T)
C                   GENERAL FORM OF TRANSFORM
C                M(XAXT)     A(,) K,K   U(,)  N,K         STORE IN C(,)
C
C     L2 = 2,    BACK TRANSFORMATION  C=U(T)ALL
C                   GENERAL FORM OF BACKTRANS
C                M(XTAX)     A(,) N,N   U(,)  N,K          STORE IN C(,)
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1968.
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             FDPCON
C
      DOUBLE PRECISION X(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
C     CHECK TO SEE IF WE HAVE CORRECT NUMBER OF ARGUMENTS.
C
      IF (NARGS.NE.ITEN) CALL ERROR (10)
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT.
C
      IF (L2.EQ.ITWO) GO TO 10
      IF (IARGS(3).NE.IARGS(4) .OR. IARGS(3).NE.IARGS(8)) CALL ERROR (3)
      GO TO 20
  10  IF (IARGS(3).NE.IARGS(4) .OR. IARGS(3).NE.IARGS(7)) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE.
C        COMPUTE ADDRESSES.
C
  20  IF (NARGS.EQ.ITEN) GO TO 30
      IARGS(12) = IARGS(L2+5)
      IARGS(11) = IARGS(L2+5)
      GO TO 40
  30  IARGS(12) = IARGS(L2+6)
      IARGS(11) = IARGS(L2+6)
      GO TO 50
  40  IARGS(10) = IARGS(NARGS)
      IARGS( 9) = IARGS(NARGS-1)
      IARGS( 8) = IARGS(NARGS-2)
      IARGS( 7) = IARGS(NARGS-3)
      IARGS( 6) = IARGS(5)
      IARGS( 5) = IARGS(4)
      IARGS( 4) = IARGS(3)
  50  J = ITHRE
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 60
      IF (J.LT.IONE) GO TO 70
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  60  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS.
C
  70  IF (NERROR.NE.IZERO) RETURN
      IROWA = IARGS(3)
      ISP   = IONE
      IROWU = IARGS(11)
      IF (L2.EQ.ITWO) GO TO 80
      IADD1 = IONE
      IADD2 = NROW
      GO TO 90
  80  IADD1 = NROW
      IADD2 = IONE
  90  DO 130 J=1,IROWU
        DO 120 I=1,IROWU
          IUP = IARGS(5) + (I-IONE) * IADD1
          IA  = IARGS(1)
          IUT = IARGS(5) + (J-IONE) * IADD1
          CALL DSUMAL (X,IZERO,DSUM)
          DO 110 L=1,IROWA
            IU = IUP
            DO 100 K=1,IROWA
              X(1) = DBLE (RC(IU)) * DBLE (RC(IA)) * DBLE (RC(IUT))
              CALL DSUMAL (X,-IONE,DSUM)
              IU  = IU + IADD2
              IA  = IA + IONE
 100        CONTINUE
            IA  = IA + NROW - IROWA
            IUT = IUT + IADD2
 110      CONTINUE
          CALL DSUMAL (X,IONE,DSUM)
          A(ISP) = FDPCON (DSUM)
          ISP = ISP + IONE
 120    CONTINUE
 130  CONTINUE
C
C     STORE RESULTS IN WORKSHEET.
C
      IS = IONE
      IC = IARGS(9)
      DO 150 J=1,IROWU
        DO 140 I=1,IROWU
          RC(IC) = A(IS)
          IS = IS + IONE
          IC = IC + IONE
 140    CONTINUE
        IC = IC + NROW - IROWU
 150  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*TWCOEF
      SUBROUTINE TWCOEF (IWT,KCOEF,KR,KY,KW,NROWS,NCOLS,NZ,SDRC,SDCC,YC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TWCOEF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO PUT STATISTICS IN COEFFICIENTS COLUMN FOR TWOWAY.
C
C       IWT = 0, IF NO WEIGHTS SPECIFIED, NARGS = 4.
C             1, IF WEIGHTS    SPECIFIED, NARGS = 5.
C     KCOEF = TOP OF COLUMN WHERE STATISTICS ARE STORED.
C        KR = LOCATION OF RESIDUAL SUM OF SQUARES.     .
C        KY = LOCATION OF FIRST Y VALUE.
C        KW = LOCATION OF FIRST WEIGHT.
C     NROWS = NUMBER OF ROWS    IN TWOWAY TABLE.
C     NCOLS = NUMBER OF COLUMNS IN TWOWAY TABLE.
C      NRM1 = NROWS - 1
C      NCM1 = NCOLS - 1
C        NZ = NUMBER OF ZERO WEIGHTS.
C
C      KRRE = LOCATION OF RTH ROW EFFECT.
C      KCCE = LOCATION OF CTH COLUMN EFFECT.
C      KSDR = LOCATION OF STANDARD DEVIATION OF RTH ROW EFFECT.
C      KSDC = LOCATION OF STANDARD DEVIATION OF CTH COLUMN EFFECT.
C      SDRC = STANDARD DEVIATION OF RTH ROW    EFFECT WHEN WEIGHTS USED.
C      SDCC = STANDARD DEVIATION OF CTH COLUMN EFFECT WHEN WEIGHTS USED.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   MAY, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             SDRC, SDCC, YC
      REAL             TEMP(1)
      REAL             SUM, SUMWT, SUMY, SUMYSQ, YBAR
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
      KRRE = KCOEF + NROWS
      KCCE = KRRE  + NCOLS
      KSDR = KCCE  + NROWS + IONE
      KSDC = KSDR  + NCOLS
      NRM1 = NROWS - IONE
      NCM1 = NCOLS - IONE
C
C     ADJUST CONSTANT TERM FOR MIDRANGE SUBTRACTION.
C
      RC(KCOEF) = RC(KCOEF) + YC
C
C     MOVE COLUMN EFFECTS.
C
      K = KCCE - IONE
      DO 10 I=1,NCM1
        RC(K) = RC(K-1)
        K = K - IONE
  10  CONTINUE
C
C     COMPUTE RTH ROW EFFECT.
C
      CALL SUMMAL (RC(KCOEF+1),NRM1,SUM)
      RC(KRRE) = SUM
      IF (NRM1.EQ.IONE) RC(KRRE) = RC(KCOEF+1)
      RC(KRRE) = - RC(KRRE)
C
C     COMPUTE CTH COLUMN EFFECT.
C
      CALL SUMMAL (RC(KRRE+1),NCM1,SUM)
      RC(KCCE) = SUM
      IF (NCM1.EQ.IONE) RC(KCCE) = RC(KRRE+1)
      RC(KCCE) = - RC(KCCE)
C
C     MOVE STANDARD DEVIATIONS OF EFFECTS.
C
      K = KSDC - IONE
      DO 20 I=1,NCM1
        RC(K) = RC(K-1)
        K = K - IONE
  20  CONTINUE
C
C     COMPUTE STANDARD DEVIATION OF RTH ROW EFFECT.
C
      RC(KSDR) = RC(KSDR-1)
      IF (IWT.EQ.IONE) RC(KSDR) = SDRC
C
C     COMPUTE STANDARD DEVIATION OF CTH COLUMN EFFECT.
C
      RC(KSDC) = RC(KSDC-1)
      IF (IWT.EQ.IONE) RC(KSDC) = SDCC
C
C     ..................................................................
C
C     COMPUTE MISCELLANEOUS STATISTICS AT BOTTOM.
C
      NDF = NROWS + NCOLS - IONE
      K = KSDC + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      RC(K) = NRMAX - NZ
      K = K + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      RC(K) = NDF
      K = K + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      RC(K) = NRMAX - NDF - NZ
      K = K + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      RC( K) = FSQRT (FDIV(RC(KR),RC(KSDC+3),IND) )
      K = K + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      RC(K) = FDIV (RC(KR),RC(KSDC+3),IND)
C
C     COMPUTE MULTIPLE CORRELATION COEFFICIENT SQUARED.
C
      K = K + IONE
      IF (K-KCOEF+IONE.GT.NROW) GO TO 90
      IF (IWT.EQ.IONE) GO TO 40
      CALL SUMMAL (RC(KY),NRMAX,SUMY)
      IF (NRMAX.EQ.IONE) SUMY = RC(KY)
      YBAR = FDIV (SUMY,FLOAT(NRMAX),IND)
      GO TO 60
C
  40  J = KY
      L = KW
      CALL SUMMAL (TEMP,IZERO,SUMY)
      DO 50 I=1,NRMAX
        TEMP(1) = RC(L) * RC(J)
        CALL SUMMAL (TEMP,-IONE,SUMY)
        J = J + IONE
        L = L + IONE
  50  CONTINUE
      CALL SUMMAL (TEMP,IONE,SUMY)
      CALL SUMMAL (RC(KW),NRMAX,SUMWT)
      IF (NRMAX.EQ.IONE) SUMWT = RC(KW)
      YBAR = FDIV (SUMY,SUMWT,IND)
C
  60  L = KY
C
C     J IS NOT USED IF IWT = 0.
C
      J = KW
      CALL SUMMAL (TEMP,IZERO,SUMYSQ)
      DO 80 I=1,NRMAX
        TEMP(1) = (RC(L)-YBAR)**2
        IF (IWT.EQ.IZERO) GO TO 70
          TEMP(1) = RC(J) * TEMP(1)
          J = J +IONE
  70    CALL SUMMAL (TEMP,-IONE,SUMYSQ)
        L = L +IONE
  80  CONTINUE
      CALL SUMMAL (TEMP,IONE,SUMYSQ)
      RC(K) = RONE - FDIV (RC(KR),SUMYSQ,IND)
      RETURN
C
C     ..................................................................
C
  90  CALL ERROR (226)
      RETURN
C
C     ==================================================================
C
      END
*TWOWAY
      SUBROUTINE TWOWAY
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TWOWAY V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE OMNITAB TWOWAY ANALYSIS OF VARIANCE INSTRUCTION.
C
C     TWOWAY ANALYSIS FOR (R) BY (C) TABLE, DATA (C), START STORING (C)
C                                                (WEIGHTS IN COL (C))
C
C     FIFTH ARGUMENT IS USED ONLY IF WEIGHTS ARE UNEQUAL (E.G. IF ZERO
C        WEIGHTS ARE USED FOR MISSING OBSERVATIONS OR REJECTED OUTLIERS)
C     IF ZERO WTS ARE USED FOR M.O. THE ESTIMATES GIVEN ARE THE SAME AS
C        THOSE OBTAINED FROM DATA AUGMENTED USING THE M.O. FORMULA
C
C     TUKEY'S TEST FOR NON-ADDITIVITY IS NOT DONE IF WTS ARE SPECIFIED.
C     MID-RANGE IS SUBTRACTED BEFORE DOING FIT.
C     AUTOMATIC OUTPUT USING READABLE FORMAT.
C     TABLE OF STANDARDIZED RESIDUALS IS GIVEN ON PAGE TWO.
C
C     MEASUREMENTS ARE STORED IN THE COLUMN ROW BY ROW
C
C     RESTRICTIONS ...
C
C     R*C MUST = NRMAX, WHICH MUST BE LESS THAN OR EQUAL TO NO. OF ROWS
C     (X+R+C+2) MUST BE LESS THAN OR EQUAL TO THE NUMBER OF COLUMNS
C     R+C+6 MUST BE LESS THAN OR EQUAL TO NCOL
C     SIZE OF TABLE IS CONSTRAINED BY NS AND BY LSQ.
C
C     STORAGE ...
C
C     COEFFICIENTS ARE STORED IN COL (X+R+C-1)
C     RESIDUALS ARE STORED IN COL (X+R+C)
C     STANDARD DEVIATIONS OF PREDICTED VALUES ARE IN COL (X+R+C+1)
C     SUMS OF SQUARES ARE STORED IN COLUMN (X+R+C+2)
C
C     L2 = 6, FOR TWOWAY
C        = 7, FOR STWOWAY.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - APRIL, 1969.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(50)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             ACDGTS(1), PRDVAL(1)
      REAL             TEMP(1)
      REAL             AGREE, COLSS, CONS1, CONS2, DIGITS
      REAL             ROWSS, SDCC, SDRC, WC, WYSQ, YMDRNG
      REAL             FDIV
C
C     ....................................................................
C
      CHARACTER        MA*1
C
C     ==================================================================
C
C     SETUP CONSTANTS.
C
      NZW    = IZERO
      NROWS  = IARGS(1)
      NCOLS  = IARGS(2)
      NSIZE  = NROWS * NCOLS
      NPARAM = NROWS + NCOLS - IONE
      NRM1   = NROWS - IONE
      NRP1   = NROWS + IONE
      NCM1   = NCOLS - IONE
      NRESDF = NRM1 * NCM1
      NHALF  = IDIV (NRMAX,ITWO,IND)
      IASTRE = IARGS(4)
      LSTREA = IDIV ((NPARAM+IONE)*(NPARAM+ITWO),ITWO,IND) + IONE
      LSTREB = LSTREA + NRMAX * (NPARAM+IONE)
      CALL ADRESS (ITHRE,KY)
      IWT    = IZERO
      IF (NARGS.EQ.IFIVE) IWT = IONE
      IF (IWT.EQ.IONE) CALL ADRESS (IFIVE,KWT)
      IF (NROWS.LT.ITWO .OR. NCOLS.LT.ITWO) GO TO 80
C
C     ..................................................................
C
C     ERROR CHECKING.
C
      IF (NARGS.EQ.IFOUR) GO TO 20
      IF (NARGS.EQ.IFIVE) GO TO 30
  10  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  IF (IARGS(3).GE.IARGS(4)) GO TO 100
      GO TO 60
  30  IF (IWT.GT.IONE) GO TO 10
      IF (IARGS(5).LE.NCOL) GO TO 50
  40  CALL ERROR (11)
      RETURN
C
C     ..................................................................
C
  50  IF (IARGS(3).EQ.IARGS(5)) GO TO 80
      IF(IARGS(5).GE.IARGS(4).AND.IARGS(5).LE.IARGS(4)+ITWO+NCOLS+NROWS)
     1 GO TO 100
C
  60  DO 70 I=1,NARGS
        IF (KIND(I).NE.IZERO) GO TO 90
        IF (IARGS(I).LE.IZERO) GO TO 40
  70  CONTINUE
C
      IF (NRMAX.EQ.IARGS(1)*IARGS(2)) GO TO 110
C
  80  CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  90  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 100  CALL ERROR (32)
      RETURN
C
C     ..................................................................
C
 110  IF (NPARAM+IASTRE+ITWO.LT.NCOL) GO TO 120
      CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
 120  IF (ITWO*(NROWS+NCOLS+IFOUR).LE.NROW .AND.
     1          LSTREB+NPARAM.LT.NS) GO TO 130
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
 130  NARGS = NPARAM
      IF (NPARAM+7.GT.NCOL) GO TO 10
      IF (IWT.EQ.IZERO) CALL HEADS (IARGS(3),IONE ,IZERO,IONE)
      IF (IWT.EQ.IONE)  CALL HEADS (IARGS(3),ITHRE,IZERO,IONE)
C
      K = IASTRE
      DO 140 I=1,NPARAM
        KIND(I)  = IZERO
        IARGS(I) = K
        K = K + IONE
 140  CONTINUE
C
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     CONSTRUCT DESIGN MATRIX.
C
      K = IARGS(1)
      DO 150 I=1,NSIZE
        RC(K) = RONE
        K = K + IONE
 150  CONTINUE
C
      DO 190 I=2,NROWS
        K1 = IARGS(I)
        DO 170 K=1,NRM1
          DO 160 J=1,NCOLS
            K2 = K1 + NCOLS*(K-IONE) + J - IONE
            RC(K2) = RZERO
            IF (K.EQ.I-IONE) RC(K2) = RONE
 160      CONTINUE
 170    CONTINUE
        DO 180 J=1,NCOLS
          K2 = K1 + NCOLS*(NROWS-IONE) + J - IONE
          RC(K2) = -RONE
 180    CONTINUE
 190  CONTINUE
C
      DO 220 I=NRP1,NPARAM
        DO 210 K=1,NROWS
          DO 200 J=1,NCM1
            K2 = IARGS(I) + NCOLS*(K-1) + J - IONE
            RC(K2) = RZERO
            IF (J.EQ.I-NROWS) RC(K2) = RONE
 200      CONTINUE
          K2 = IARGS(I) + NCOLS*K - IONE
          RC(K2) = -RONE
 210    CONTINUE
 220  CONTINUE
C
C     ..................................................................
C
C     SETUP FOR LEAST SQUARES PROCEDURE.
C
      WC = RONE
      KWT = IONE
      IF (IWT.EQ.IONE) WC = RZERO
C
C     SUBTRACT MIDRANGE FROM Y MEASUREMENTS, IF YMIN AND YMAX AGREE TO
C        AT LEAST HALF A DIGIT.
C
      K = KY
      CONS1 = RC(K)
      CONS2 = RC(K)
C
      J = KWT - IONE
      DO 250 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 230
        J = J + IONE
        IF (RC(J).LE.RZERO) GO TO 240
 230    IF (RC(K).LT.CONS1) CONS1 = RC(K)
        IF (RC(K).GT.CONS2) CONS2 = RC(K)
 240    K = K + IONE
 250  CONTINUE
C
      YMDRNG = RZERO
C
      CALL ACCDIG (CONS2,CONS1,RSD,AGREE,IND)
C
      IF (AGREE.GE.RHALF) YMDRNG = FDIV (CONS1+CONS2,RTWO,IND)
C
      K = KY
      DO 260 I=1,NSIZE
        RC(K) = RC(K) - YMDRNG
        K = K + IONE
 260  CONTINUE
C
      NARGS = NROWS + NCOLS + 6
      DO 270 I=1,NARGS
        KIND(I) = IZERO
 270  CONTINUE
C
      J = IASTRE + NPARAM
      K = NARGS - ITHRE
      DO 280 I=1,4
        IARGS(K) = J
        J = J + IONE
        K = K + IONE
 280  CONTINUE
C
      CALL ADRESS (NARGS-ITHRE,KCOEF )
      CALL ADRESS (NARGS-ITWO ,KRES  )
      CALL ADRESS (NARGS-IONE ,KSYHAT)
      CALL ADRESS (NARGS      ,KSQFC )
      KSC  = KCOEF + NPARAM + ITWO
      KRSS = KSQFC + NPARAM
C
      IF (IWT.EQ.IZERO) GO TO 450
C
C     ..................................................................
C
C     COMPUTE INITIAL SUMS OF SQUARES WHEN WEIGHTS ARE SPECIFIED.
C
C     CHECK ON WEIGHTS.
C
C     THERE MUST BE AT LEAST ONE POSITIVE WEIGHT IN EACH ROW.
C
      K   = KWT - IONE
      MZW = NSIZE
      DO 320 I=1,NROWS
        NZW = IZERO
        DO 310 J=1,NCOLS
          K = K + IONE
          IF (RC(K)) 390,300,310
 300      NZW = NZW + IONE
          MZW = MZW - IONE
 310    CONTINUE
C
        IF (MZW.LE.IZERO) GO TO 380
        IF (NZW.EQ.NCOLS) GO TO 370
 320  CONTINUE
C
C     THERE MUST BE AT LEAST ONE POSITIVE WEIGHT IN EACH COLUMN.
C
      DO 360 I=1,NCOLS
        NZW = IZERO
        K   = KWT+I-IONE
        DO 350 J=1,NROWS
          IF (RC(K)) 390,330,340
 330      NZW = NZW + IONE
 340      K = K + NCOLS
 350    CONTINUE
C
        IF (NZW.EQ.NROWS) GO TO 370
 360  CONTINUE
      GO TO 400
C
C     ..................................................................
C
 370  CALL ERROR (22)
      RETURN
C
C     ..................................................................
C
 380  CALL ERROR (24)
      RETURN
C
C     ..................................................................
C
 390  CALL ERROR (25)
      RETURN
C
C     ..................................................................
C
 400  NZW = NSIZE - MZW
C
      IARGS(IFOUR) = IASTRE
      J = IFIVE
      K = IASTRE + NROWS
      DO 410 I=1,NCM1
        IARGS(J) = K
        J = J + IONE
        K = K + IONE
 410  CONTINUE
C
      K = IASTRE + IONE
      DO 420 I=1,NRM1
        IARGS(J) = K
        J = J + IONE
        K = K + IONE
 420  CONTINUE
C
C     FLIP Y AND WEIGHTS.
C
      J = KY
      K = KY + NSIZE - IONE
      L = KWT
      M = KWT + NSIZE - IONE
      DO 430 I=1,NHALF
        TEMP(1) = RC(J)
        RC(J)   = RC(K)
        RC(K)   = TEMP(1)
        J       = J + IONE
        K       = K - IONE
        TEMP(1) = RC(L)
        RC(L)   = RC(M)
        RC(M)   = TEMP(1)
        L       = L + IONE
        M       = M - IONE
 430  CONTINUE
C
      CALL LSQ (NRMAX,NPARAM,NROW,RC,RC(KY),RC(KWT),WC,YMDRNG,ITWO,
     1          IARGS(4),RC(KCOEF),RC(KRES),A(1),RC(KSC),
     2          RC(KSYHAT),RC(KSQFC),RC(KRSS),A(LSTREA),
     3          A(LSTREB),PRDVAL,ACDGTS,NUMIT,DIGITS)
C
      IF (NERROR.NE.IZERO) RETURN
C
C     COMPUTE STATISTICS NEEDED FROM FIRST FIT.
C
      CALL SUMMAL (RC(KSQFC+1),NCM1,COLSS)
      IF (NCM1.EQ.IONE) COLSS = RC(KSQFC+1)
      K = KSQFC + NCOLS
      CALL SUMMAL (RC(K),NRM1,ROWSS)
      IF (NRM1.EQ.IONE) ROWSS = RC(K)
C
      K    = KSC + NCM1 + NRM1
      SDRC = RC(K)
      K    = KSC + NCM1
      SDCC = RC(K)
C
C     REFLIP Y AND WEIGHTS.
C
      J = KY
      K = KY + NSIZE - IONE
      L = KWT
      M = KWT + NSIZE - IONE
      DO 440 I=1,NHALF
        TEMP(1) = RC(J)
        RC(J)   = RC(K)
        RC(K)   = TEMP(1)
        J       = J + IONE
        K       = K - IONE
        TEMP(1) = RC(L)
        RC(L)   = RC(M)
        RC(M)   = TEMP(1)
        L       = L + IONE
        M       = M - IONE
 440  CONTINUE
C
      NRESDF = NRM1 * NCM1 - NZW
C
C     ..................................................................
C
 450  J = IFOUR
      K = IASTRE
      DO 460 I=1,NPARAM
        IARGS(J) = K
        J = J + IONE
        K = K + IONE
 460  CONTINUE
C
      CALL LSQ (NRMAX,NPARAM,NROW,RC,RC(KY),RC(KWT),WC,YMDRNG,ITWO,
     1          IARGS(4),RC(KCOEF),RC(KRES),A(1),RC(KSC),
     2          RC(KSYHAT),RC(KSQFC),RC(KRSS),A(LSTREA),
     3          A(LSTREB),PRDVAL,ACDGTS,NUMIT,DIGITS)
C
      IF (NERROR.NE.IZERO) RETURN
C
C     ADD MIDRANGE BACK TO Y VALUES.
C
      K = KY
      DO 470 I=1,NRMAX
        RC(K) = RC(K) + YMDRNG
        K = K + IONE
 470  CONTINUE
C
C     COMPUTE UNCORRECTED TOTAL SUM OF SQUARES.
C
      IY = KY
      IW = KWT
      CALL SUMMAL (TEMP,IZERO,WYSQ)
      DO 490 I=1,NRMAX
        TEMP(1) = RC(IY)**2
        IF (IWT.EQ.IZERO) GO TO 480
          TEMP(1) = RC(IW) * TEMP(1)
          IW = IW + IONE
 480    CALL SUMMAL (TEMP,-IONE,WYSQ)
        IY = IY + IONE
 490  CONTINUE
      CALL SUMMAL (TEMP,IONE,WYSQ)
      RC(KRSS+1) = WYSQ
C
C     STORE COEFFICIENTS.
C
      CALL TWCOEF (IWT,KCOEF,KRSS,KY,KWT,NROWS,NCOLS,NZW,SDRC,SDCC,
     1             YMDRNG)
C
      IF (L2.EQ.7) RETURN
C
      IF (IWT.EQ.IONE) GO TO 500
C
C     ..................................................................
C
C
C     COMPUTE AND PRINT ANOVA WHEN NO WEIGHTS ARE SPECIFIED.
C
      CALL TWPRAV (KRSS,KY,KSQFC,NROWS,NCOLS,NRM1,NCM1,MA)
C
C     COMPUTE AND PRINT FOR TUKEY'S TEST.
C
      IF (NRESDF.GT.IONE) CALL TWPRNA (KCOEF,KY,NROWS,NCOLS,NRESDF,MA)
      IF (NCRT.NE.IZERO) CALL PAGE (0)
C
C     PRINT COEFFICIENTS, RANK SUMS, STANDARD DEVIATIONS, AND
C        RANK SUM STATISTICS.
C
      CALL TWPRCR (KCOEF,KY,NROWS,NCOLS,MA)
      GO TO 510
C
C     ..................................................................
C
C     PRINT ANOVAS WHEN WEIGHTS ARE SPECIFIED.
C
 500  CALL TWPRAW (KRSS,KSQFC,NROWS,NCOLS,NRM1,NCM1,MA,NRESDF,
     1             MZW,NZW,ROWSS,COLSS)
      IF (NCRT.NE.IZERO) CALL PAGE (0)
C     ..................................................................
C
C     PRINT COEFFICIENTS AND THEIR STANDARD DEVIATIONS.
C
      CALL TWPRCS (KCOEF,NROWS,NCOLS,MA)
C
C     ..................................................................
C
C     PRINT TABLE OF RESIDUALS.
C
 510  IF (NCRT.NE.IZERO) CALL PAGE (0)
      CALL TWPRTR (IWT,KWT,KCOEF,KRES,KSYHAT,NROWS,NCOLS)
C
      RETURN
C
C     ==================================================================
C
      END
*TWPRAV
      SUBROUTINE TWPRAV (KRSS,KY,KFC,NROWS,NCOLS,NRM1,NCM1,MA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TWPRAV V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT ANALYSIS OF VARIANCE WHEN WEIGHTS ARE NOT SPECIFIED.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     APRIL, 1978.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             TEMP(1)
      REAL             RESDF, SUMY, TOTLSS, YBAR
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        MA*1 
C
C     ==================================================================
C
      NRESDF = NRM1 * NCM1
      NTOTDF = NROWS * NCOLS - IONE
      RESDF  = FLOAT (NRESDF)
C
C     COMPUTE BETWEEN ROWS SUM OF SQUARES.
C
      K = KFC+ IONE
      CALL SUMMAL (RC(K),NRM1,A(1))
      IF (NRM1.EQ.IONE) A(1) = RC(K)
C
C     COMPUTE BETWEEN COLUMNS SUM OF SQUARES.
C
      K = K + NRM1
      CALL SUMMAL (RC(K),NCM1,A(2))
      IF (NCM1.EQ.IONE) A(2) = RC(K)
C
C     RESIDUAL SUM OF SQUARES.
C
      A(3) = RC(KRSS)
C
C     COMPUTE CORRECTED TOTAL SUM OF SQUARES.
C
      CALL SUMMAL (RC(KY),NRMAX,SUMY)
      IF (NRMAX.EQ.IONE) SUMY = RC(KY)
      YBAR = FDIV (SUMY,FLOAT(NRMAX),IND)
C
      J = KY
      CALL SUMMAL (TEMP,IZERO,TOTLSS)
      DO 10 I=1,NRMAX
        TEMP(1) = (RC(J)-YBAR)**2
        CALL SUMMAL (TEMP,-IONE,TOTLSS)
        J = J + IONE
  10  CONTINUE
      CALL SUMMAL (TEMP,IONE,TOTLSS)
      A(4) = TOTLSS
C
C     COMPUTE MEAN SQUARES.
C
      A(5) = FDIV (A(1),FLOAT(NRM1),IND)
      A(6) = FDIV (A(2),FLOAT(NCM1),IND)
      A(7) = FDIV (A(3),RESDF,IND)
C
C     COMPUTE F RATIOS AND SIGNIFICANCE LEVELS..
C
      A(8) = FDIV (A(5),A(7),IND)
      A(9) = FDIV (A(6),A(7),IND)
      CALL QFORF (RESDF,FLOAT(NRESDF),A(8),TEMP(1))
      A(10) = TEMP(1)
      CALL QFORF (FLOAT(NCM1),FLOAT(NRESDF),A(9),TEMP(1))
      A(11) = TEMP(1)
C
C     START PRINTING.
C
      CALL RFORMT (0,ISIGD,A   ,RC(1),  4,15,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (0,ISIGD,A(5),RC(1),  3,15,NW2,NDEC2,MA(18),IRF)
      NB1 = 17 - NW1
      NB2 = 17 - NW2
C
      CALL PAGE (IFOUR)
      WRITE (IPRINT,20) NROWS, NCOLS, (LHEAD(I),I=1,12)
      WRITE (IPRINT,40) (LA(39),I=1,70)
      WRITE (IPRINT,30)
      WRITE (IPRINT,40) (LA(39),I=1,70)
      CALL RFORMT (1,ISIGD,RC  , A(1),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A(5),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,50) NRM1, (MA(I),I=1,34), A(8), A(10)
C
      CALL RFORMT (1,ISIGD,RC  , A(2),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A(6),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,60) NCM1, (MA(I),I=1,34), A(9), A(11)
C
      CALL RFORMT (1,ISIGD,RC  , A(3),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A(7),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,70) NRESDF, (MA(I),I=1,34)
C
      CALL RFORMT (1,ISIGD,RC  , A(4),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      WRITE (IPRINT,80) NTOTDF, (MA(I),I=1,17)
      WRITE (IPRINT,40) (LA(39),I=1,70)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  20  FORMAT(/1X,4X,33H ANALYSIS OF VARIANCE FOR TWOWAY ,I2,3H X ,I2,10H
     1 TABLE OF ,12A1)
  30  FORMAT (1X,6HSOURCE, 7X,5H D.F.,2X,15HSUMS OF SQUARES, 4X,12HMEAN
     1SQUARES, 4X,16HF RATIO  F PROB.)
  40  FORMAT (1X,70A1)
  50  FORMAT (1X,13HBETWEEN ROWS ,I4,2X,34A1,2X,F7.3,F8.3)
  60  FORMAT (1X,13HBETWEEN COLS ,I4,2X,34A1,2X,F7.3,F8.3)
  70  FORMAT (1X,13HRESIDUALS    ,I4,2X,34A1)
  80  FORMAT (1X,13HTOTAL        ,I4,2X,17A1)
C
C     ==================================================================
C
      END
*TWPRAW
      SUBROUTINE TWPRAW (KR,KF,NR,NC,NRM1,NCM1,MA,NRDF,MZW,NZW,RS,CS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TWPRAW V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT ANALYSIS OF VARIANCE FOR TWOWAY WHEN WEIGHTS ARE SPECIFIED.
C
C     INPUT ...
C
C        KR   = LOCATION IN A(.) OF RESIDUAL SUM OF SQUARES.
C        KF   = LOCATION IN A(.) OF FIRST SQUARED FOURIER COEFFICIENT.
C        NR   = NUMBER OF ROWS IN TABLE.
C        NC   = NUMBER OF COLUMNS IN TABLE.
C        NRM1 = NR - 1.
C        NCM1 = NC - 1.
C        MA   = STORAGE ARRAY FOR PRINTING USED BY RFORMT.
C        NRDF = RESIDUAL DEGREES OF FREEDOM.
C        MZW  = NUMBER OF NONZERO WEIGHTS.
C        NZW  = NUMBER OF    ZERO WEIGHTS.
C        RS   = ROW    SUM OF SQUARES FROM FIRST FIT.
C        CS   = COLUMN SUM OF SQUARES FROM FIRST FIT.
C
C     STORAGE IN SCRATCH AREA A(.) ...
C
C     SOURCE               SS            MS          F           S
C
C     BETWEEN ROWS       A( 1)         A( 7)       A(12)       A(13)
C     BETWEEN COLS       A( 2)         A( 8)
C     RESIDUALS          A( 3)         A( 9)
C     TOTAL              A( 4)
C
C     BETWEEN ROWS       A( 5)         A(10)
C     BETWEEN COLS       A( 6)         A(11)       A(14)       A(15)
C     RESIDUALS          A( 3)         A( 9)
C     TOTAL              A( 4)
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     APRIL, 1978.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      REAL             CS, RS
      REAL             TEMP(1)
      REAL             CM1, RM1, RESDF, TOTLSS
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        MA*1
C
C     ==================================================================
C
      NROWS = NR
      NCOLS = NC
      KRSS  = KR
      KSQFC = KF
      RM1   = FLOAT (NRM1)
      CM1   = FLOAT (NCM1)
      RESDF = FLOAT (NRDF)
      NTOTDF = NROWS * NCOLS - IONE - NZW
C
C     COMPUTE BETWEEN ROWS SUM OF SQUARES.
C
      K = KSQFC+ IONE
      CALL SUMMAL (RC(K),NRM1,A(5))
      IF (NRM1.EQ.IONE) A(5) = RC(K)
C
C     COMPUTE BETWEEN COLUMNS SUM OF SQUARES.
C
      K = K + NRM1
      CALL SUMMAL (RC(K),NCM1,A(6))
      IF (NCM1.EQ.IONE) A(6) = RC(K)
C
C     RESIDUAL SUM OF SQUARES.
C
      A(3) = RC(KRSS)
C
C     COMPUTE MEAN SQUARES.
C
      A(10) = FDIV (A(5),RM1,IND)
      A(11) = FDIV (A(6),CM1,IND)
      A( 9) = FDIV (A(3),RESDF,IND)
C
C     COMPUTE F RATIOS AND SIGNIFICANCE LEVELS..
C
      A(14) = FDIV (A(11),A(9),IND)
      CALL QFORF (CM1,RESDF,A(14),TEMP(1))
      A(15) = TEMP(1)
C
C     COMPUTE SS, MS, F AND SIGNIFICANCE LEVEL FROM FIRST FIT.
C
      A( 1) = RS
      A( 2) = CS
      A( 7) = FDIV (A(1),RM1,IND)
      A( 8) = FDIV (A(2),CM1,IND)
      A(12) = FDIV (A(7),A(9),IND)
      CALL QFORF (RM1,RESDF,A(12),TEMP(1))
      A(13) = TEMP(1)
C
C     COMPUTE CORRECTED TOTAL SUM OF SQUARES.
C
      CALL SUMMAL (A,ITHRE,TOTLSS)
      A(4) = TOTLSS
C
C     SET UP PRINTING CONSTANTS.
C
      CALL RFORMT (0,ISIGD,A   ,RC(1),  6,15,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (0,ISIGD,A(7),RC(1),  5,15,NW2,NDEC2,MA(18),IRF)
      NB1 = 17 - NW1
      NB2 = 17 - NW2
C
C     START PRINTING.
C
      CALL PAGE (IFOUR)
      WRITE (IPRINT,10) NROWS, NCOLS, (LHEAD(I),I=1,12)
      WRITE (IPRINT,30) (LA(39),I=1,70)
      WRITE (IPRINT,20)
      WRITE (IPRINT,30) (LA(39),I=1,70)
C
C     FIRST ANOVA.
C
      CALL RFORMT (1,ISIGD,RC  , A( 1),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A( 7),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,40) NRM1, (MA(I),I=1,34), A(12), A(13)
C
      CALL RFORMT (1,ISIGD,RC  , A( 2),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A( 8),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,50) NCM1, (MA(I),I=1,34)
C
      CALL RFORMT (1,ISIGD,RC  , A( 3),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A( 9),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,60) NRDF, (MA(I),I=1,34)
C
      CALL RFORMT (1,ISIGD,RC  , A( 4),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      WRITE (IPRINT,70) NTOTDF, (MA(I),I=1,17)
      WRITE (IPRINT,30) (LA(39),I=1,70)
C
C     SECOND ANOVA.
C
      CALL RFORMT (1,ISIGD,RC  , A( 5),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A(10),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,40) NRM1, (MA(I),I=1,34)
C
      CALL RFORMT (1,ISIGD,RC  , A( 6),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A(11),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,50) NCM1, (MA(I),I=1,34), A(14), A(15)
C
      CALL RFORMT (1,ISIGD,RC  , A( 3),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC  , A( 9),NB2, 1,NW2,NDEC2,MA(18),IRF)
      WRITE (IPRINT,60) NRDF, (MA(I),I=1,34)
C
      CALL RFORMT (1,ISIGD,RC  , A( 4),NB1, 1,NW1,NDEC1,MA( 1),IRF)
      WRITE (IPRINT,70) NTOTDF, (MA(I),I=1,17)
      WRITE (IPRINT,30) (LA(39),I=1,70)
C
      WRITE (IPRINT,80) MZW, NZW, (LHEAD(I),I=25,36)
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  10  FORMAT(/1X,4X,33H ANALYSIS OF VARIANCE FOR TWOWAY ,I2,3H X ,I2,10H
     1 TABLE OF ,12A1)
  20  FORMAT (1X,6HSOURCE, 7X,5H D.F.,2X,15HSUMS OF SQUARES, 4X,12HMEAN
     1SQUARES, 4X,16HF RATIO  F PROB.)
  30  FORMAT (1X,70A1)
  40  FORMAT (1X,13HBETWEEN ROWS ,I4,2X,34A1,2X,F7.3,F8.3)
  50  FORMAT (1X,13HBETWEEN COLS ,I4,2X,34A1,2X,F7.3,F8.3)
  60  FORMAT (1X,13HRESIDUALS    ,I4,2X,34A1)
  70  FORMAT (1X,13HTOTAL        ,I4,2X,17A1)
  80  FORMAT (16X,39HA WEIGHTED LEAST SQUARES ANALYSIS USING/6X,I4,22H N
     1ON-ZERO WEIGHTS AND ,I4,17H ZERO WEIGHTS IN ,12A1)
C
C     ==================================================================
C
      END
*TWPRCR
      SUBROUTINE TWPRCR (KCOEF,KY,NROWS,NCOLS,MA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TWPRCR V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT COEFFICIENTS, THEIR STANDARD DEVIATIONS AND RANK STATISTICS
C        FOR TWOWAY WITHOUT USE OF WEIGHTS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GIATHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - APRIL, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             CEMAX, CEMIN, COLFRD, COLKEN
      REAL             CRMAX, CRMIN, REMAX, REMIN
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
      CHARACTER*1      ICHARE, ICHARR
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX / 25 /
      DATA NY / 15 /
      DATA NZ / 15 /
C
C     ==================================================================
C
C     PRINT TITLE.
C
      WRITE (IPRINT,60)
      WRITE (IPRINT,50) (LA(39),I=1,53)
C
C     SET UP PRINTING CONSTANTS FOR COEFFICIENTS.
C
      NOCOEF = IONE + NROWS + NCOLS
      J = KCOEF
C
      CALL RFORMT (IZERO,ISIGD,RC(J),A(1),NOCOEF,NX,NW,ND,MA( 1),IRF)
      NB = NX - NW
C
C     COMPUTE RANK SUMS AND RANK SUM STATISTICS.
C
      CALL TWRNKS (KY,NROWS,NCOLS,ROWFRD,COLFRD,ROWKEN,COLKEN)
C
C     PRINT GRAND MEAN.
C
      CALL RFORMT (IONE,ISIGD,A,RC(J),NB,IZERO,NW,ND,MA( 1),IRF)
      WRITE (IPRINT,70) (MA(L),L=1,25)
C
C     COMPUTE SMALLEST AND LARGEST ROW ESTIMATE AND ROW RANK SUM.
C
      K = KCOEF + IONE
      REMIN = RC(K)
      REMAX = RC(K)
      RRMIN = A(1)
      RRMAX = A(1)
      DO 10 I=1,NROWS
        IF (RC(K).LE.REMIN) REMIN = RC(K)
        IF (RC(K).GE.REMAX) REMAX = RC(K)
        IF ( A(I).LE.RRMIN) RRMIN = A(I)
        IF ( A(I).GE.RRMAX) RRMAX = A(I)
        K = K + IONE
  10  CONTINUE
C
C     PRINT ROW EFFECTS AND ROW RANK SUMS.
C
      NLINES = 25
      LINTOP = 60
      WRITE (IPRINT,50) (LA(39),I=1,53)
      IF (NCRT.NE.IZERO) THEN
      NLINES = IFOUR
      LINTOP = LENGTH 
      ENDIF
C
      DO 20 I=1,NROWS
        J = J + IONE
        ICHARE = LA(45)
        IF (RC(J).LE.REMIN) ICHARE = LA(22)
        IF (RC(J).GE.REMAX) ICHARE = LA(18)
        ICHARR = LA(45)
        IF ( A(I).LE.RRMIN) ICHARR = LA(22)
        IF ( A(I).GE.RRMAX) ICHARR = LA(18)
        CALL RFORMT (IONE,ISIGD,A,RC(J),NB,IZERO,NW,ND,MA( 1),IRF)
        WRITE (IPRINT,80) I, (MA(L),L=1,25), ICHARE, A(I), ICHARR
        NLINES = NLINES + IONE
        IF (NLINES.LE.LINTOP) GO TO 20
          NLINES = IONE
          CALL PAGE (IFOUR)
  20  CONTINUE
C
C     COMPUTE SMALLEST AND LARGEST COLUMN ESTIMATE AND COLUMN RANK SUM.
C
      K = KCOEF + NROWS + IONE
      L = NROWS + IONE
      CEMIN = RC(K)
      CEMAX = RC(K)
      CRMIN = A(NROWS+1)
      CRMAX = A(NROWS+1)
      DO 30 I=1,NCOLS
        IF (RC(K).LE.CEMIN) CEMIN = RC(K)
        IF (RC(K).GE.CEMAX) CEMAX = RC(K)
        IF ( A(L).LE.CRMIN) CRMIN = A(L)
        IF ( A(L).GE.CRMAX) CRMAX = A(L)
        K = K + IONE
        L = L + IONE
  30  CONTINUE
C
C     PRINT COLUMN EFFECTS AND COLUMN RANK SUMS.
C
      WRITE (IPRINT,50) (LA(39),I=1,53)
C
      K = NROWS + IONE
      DO 40 I=1,NCOLS
        J = J + IONE
        ICHARE = LA(45)
        IF (RC(J).LE.CEMIN) ICHARE = LA(22)
        IF (RC(J).GE.CEMAX) ICHARE = LA(18)
        ICHARR = LA(45)
        IF ( A(K).LE.CRMIN) ICHARR = LA(22)
        IF ( A(K).GE.CRMAX) ICHARR = LA(18)
        CALL RFORMT (IONE,ISIGD,A,RC(J),NB,IZERO,NW,ND,MA( 1),IRF)
        WRITE (IPRINT,90) I, (MA(L),L=1,25), ICHARE, A(K), ICHARR
        K = K + IONE
        IF (NLINES.LE.LINTOP) GO TO 40
          NLINES = IONE
          CALL PAGE (IFOUR)
  40  CONTINUE
      WRITE (IPRINT,50) (LA(39),I=1,53)
C
C     ..................................................................
C
C     PRINT STANDARD DEVIATIONS AND RANK SUM STATISTICS.
C
      IF (NLINES.GE.LINTOP-IFIVE) CALL PAGE (IFOUR)
      WRITE (IPRINT,50)
      WRITE (IPRINT,100)
C
C     SET UP PRINTING CONSTANTS FOR STANDARD DEVIATIONS.
C
      J    = KCOEF + NOCOEF
      A(1) = RC(J)
      J    = J + IONE
      A(2) = RC(J)
      J    = J + NROWS
      A(3) = RC(J)
      J    = J + NCOLS + ITHRE
      A(4) = RC(J)
      CALL RFORMT (IZERO,ISIGD,A,RC(1),IFOUR,NY,NW,ND,MA,IRF)
C
C     STD DEV OF GRAND MEAN AND KENDALL W STATISTIC FOR ROWS.
C
      J = KCOEF + NOCOEF
      CALL RFORMT (11,ISIGD,A,RC(J),IZERO,IZERO,NY,NZ,MA(1),IRF)
      CALL RFORMT (IONE,ISIGD,A,RC(J),IZERO,IZERO,NW,ND,MA(1),IRF)
      WRITE (IPRINT,110) (MA(K),K=1,15), ROWKEN
C
C     STD DEV OF ROW EFFECTS AND FRIEDMAN CHISQUARE STATISTIC FOR ROWS.
C
      J = J + IONE
      CALL RFORMT (11,ISIGD,A,RC(J),IZERO,IZERO,NY,NZ,MA(1),IRF)
      CALL RFORMT (IONE,ISIGD,A,RC(J),IZERO,IZERO,NW,ND,MA(1),IRF)
      WRITE (IPRINT,120) (MA(K),K=1,15), ROWFRD
C
C     STD DEV OF COLUMN EFFECTS AND KENDALL W STATISTIC FOR COLUMNS.
      J = J + NROWS
      CALL RFORMT (11,ISIGD,A,RC(J),IZERO,IZERO,NY,NZ,MA(1),IRF)
      CALL RFORMT (IONE,ISIGD,A,RC(J),IZERO,IZERO,NW,ND,MA(1),IRF)
      WRITE (IPRINT,130) (MA(K),K=1,15), COLKEN
C
C     RESIDUAL AND FRIEDMAN CHISQUARE STATISTIC FOR COLUMNS.
C
      J = J + NCOLS + ITHRE
      CALL RFORMT (11,ISIGD,A,RC(J),IZERO,IZERO,NY,NZ,MA(1),IRF)
      CALL RFORMT (IONE,ISIGD,A,RC(J),IZERO,IZERO,NW,ND,MA(1),IRF)
      WRITE (IPRINT,140) (MA(K),K=1,15), COLFRD
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  50  FORMAT (5X,60A1)
  60  FORMAT (/1X/5X,11HCOEFFICIENT,16X,8HESTIMATE,10X,8HRANK SUM)
  70  FORMAT (5X,10HGRAND MEAN,25A1)
  80  FORMAT (5X, 7HROW    ,I3,25A1,A1,8X,F8.1,A1)
  90  FORMAT (5X, 7HCOLUMN ,I3,25A1,A1,8X,F8.1,A1)
 100  FORMAT (5X,30HSAMPLE STANDARD DEVIATIONS ...,4X,28HRANK SUM TEST S
     1TATISTICS ...)
 110  FORMAT (7X,13HGRAND MEAN = ,15A1,6X,24HROWS - KENDALL W      = ,
     1        F7.3)
 120  FORMAT (7X,13HROW EFFECT = ,15A1,13X,17HFRIEDMAN CHISQ = ,F7.3)
 130  FORMAT (7X,13HCOL EFFECT = ,15A1,6X,24HCOLS - KENDALL W      = ,
     1        F7.3)
 140  FORMAT (7X,13HRESIDUAL   = ,15A1,13X,17HFRIEDMAN CHISQ = ,F7.3)
C
C     ==================================================================
C
      END
*TWPRCS
      SUBROUTINE TWPRCS (KCOEF,NROWS,NCOLS,MA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TWPRCS V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT COEFFICIENTS AND THEIR STANDARD DEVIATIONS FOR TWOWAY
C        WHEN WEIGHTS ARE SPECIFIED.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   MAY, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(52)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             TEMP
      REAL             REMIN, REMAX, RSMIN, RSMAX
      REAL             CEMIN, CEMAX, CSMIN, CSMAX
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
      CHARACTER*1      ICHARE, ICHARS
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX / 25 /
C
C     ==================================================================
C
C     PRINT TITLE.
C
      WRITE (IPRINT,60)
      WRITE (IPRINT,50) (LA(39),I=1,62)
C
C     SET UP PRINTING CONSTANTS.
C
      NOCOEF = IONE + NROWS + NCOLS
      J = KCOEF
      K = KCOEF + NOCOEF
C
      CALL RFORMT (IZERO,ISIGD,RC(J),A(1),NOCOEF,NX,NW1,ND1,MA( 1),IRF)
C
      L = K + NOCOEF
      TEMP = RC(L)
      RC(L) = RC(L+3)
      NCPLUS = NOCOEF + IONE
      CALL RFORMT (IZERO,ISIGD,RC(K),A(1),NCPLUS,NX,NW2,ND2,MA(27),IRF)
      RC(L) = TEMP
C
      NB1 = NX - NW1
      NB2 = NX - NW2
C
C     PRINT GRAND MEAN.
C
      CALL RFORMT (IONE,ISIGD,A,RC(J),NB1,IZERO,NW1,ND1,MA( 1),IRF)
      CALL RFORMT (IONE,ISIGD,A,RC(K),NB2,IZERO,NW2,ND2,MA(27),IRF)
      MA(26) = LA(45)
      MA(52) = LA(45)
      WRITE (IPRINT,70) (MA(J),J=1,52)
C
C     COMPUTE SMALLEST AND LARGEST ROW ESTIMATE AND ROW STD. DEV.
C
      J = KCOEF + IONE
      K = KCOEF + NOCOEF + IONE
      REMIN = RC(J)
      REMAX = RC(J)
      RSMIN = RC(K)
      RSMAX = RC(K)
      DO 10 I=1,NROWS
        IF (RC(J).LE.REMIN) REMIN = RC(J)
        IF (RC(J).GE.REMAX) REMAX = RC(J)
        IF (RC(K).LE.RSMIN) RSMIN = RC(K)
        IF (RC(K).GE.RSMAX) RSMAX = RC(K)
        J = J + IONE
        K = K + IONE
  10  CONTINUE
C
C     PRINT ROW EFFECTS AND THEIR STANDARD DEVIATIONS.
C
      NLINES = 26
      LINTOP = 60
      IF (NCRT.NE.IZERO) THEN
      NLINES = IFOUR
      LINTOP = LENGTH 
      ENDIF
C
      WRITE (IPRINT,50) (LA(39),I=1,62)
      J = KCOEF + IONE
      K = KCOEF + NOCOEF + IONE
      DO 20 I=1,NROWS
        ICHARE = LA(45)
        IF (RC(J).LE.REMIN) ICHARE = LA(22)
        IF (RC(J).GE.REMAX) ICHARE = LA(18)
        ICHARS = LA(45)
        IF (RC(K).LE.RSMIN) ICHARS = LA(22)
        IF (RC(K).GE.RSMAX) ICHARS = LA(18)
        CALL RFORMT (IONE,ISIGD,A,RC(J),NB1,IZERO,NW1,ND1,MA( 1),IRF)
        CALL RFORMT (IONE,ISIGD,A,RC(K),NB2,IZERO,NW2,ND2,MA(27),IRF)
        J = J + IONE
        K = K + IONE
        MA(26) = ICHARE
        MA(52) = ICHARS
        WRITE (IPRINT,80) I, (MA(JJ),JJ=1,52)
        NLINES = NLINES + IONE
        IF (NLINES.LE.LINTOP) GO TO 20
          NLINES = IONE
          CALL PAGE (IFOUR)
  20  CONTINUE
C
C     COMPUTE SMALLEST AND LARGEST COLUMN ESTIMATE AND COLUMN STD. DEV.
C
      J = KCOEF + NROWS + IONE
      K = J + NOCOEF
      CEMIN = RC(J)
      CEMAX = RC(J)
      CSMIN = RC(K)
      CSMAX = RC(K)
      DO 30 I=1,NCOLS
        IF (RC(J).LE.CEMIN) CEMIN = RC(J)
        IF (RC(J).GE.CEMAX) CEMAX = RC(J)
        IF (RC(K).LE.CSMIN) CSMIN = RC(K)
        IF (RC(K).GE.CSMAX) CSMAX = RC(K)
        J = J + IONE
        K = K + IONE
  30  CONTINUE
C
C     PRINT COLUMN EFFECTS AND THEIR STANDARD DEVIATIONS.
C
      WRITE (IPRINT,50) (LA(39),I=1,62)
      J = KCOEF + NROWS + IONE
      K = J + NOCOEF
      DO 40 I=1,NCOLS
        ICHARE = LA(45)
        IF (RC(J).LE.CEMIN) ICHARE = LA(22)
        IF (RC(J).GE.CEMAX) ICHARE = LA(18)
        ICHARS = LA(45)
        IF (RC(K).LE.CSMIN) ICHARS = LA(22)
        IF (RC(K).GE.CSMAX) ICHARS = LA(18)
        CALL RFORMT (IONE,ISIGD,A,RC(J),NB1,IZERO,NW1,ND1,MA( 1),IRF)
        CALL RFORMT (IONE,ISIGD,A,RC(K),NB2,IZERO,NW2,ND2,MA(27),IRF)
        J = J + IONE
        K = K + IONE
        MA(26) = ICHARE
        MA(52) = ICHARS
        WRITE (IPRINT,90) I, (MA(JJ),JJ=1,52)
        NLINES = NLINES + IONE
        IF (NLINES.LE.LINTOP) GO TO 40
          NLINES = IONE
          CALL PAGE (IFOUR)
  40  CONTINUE
C
C     PRINT RESIDUAL STANDARD DEVIATION.
C
      K = K + ITHRE
      CALL RFORMT (IONE,ISIGD,A,RC(K),NB2,IZERO,NW2,ND2,MA( 1),IRF)
      WRITE (IPRINT, 50) (LA(39),I=1,62)
      WRITE (IPRINT,100) (MA(J),J=1,25)
      WRITE (IPRINT, 50) (LA(39),I=1,62)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  50  FORMAT (5X,62A1)
  60  FORMAT (/1X/5X,11HCOEFFICIENT,16X,8HESTIMATE,17X,9HSTD. DEV.)
  70  FORMAT (5X,10HGRAND MEAN,52A1)
  80  FORMAT (5X, 7HROW    ,I3,52A1)
  90  FORMAT (5X, 7HCOLUMN ,I3,52A1)
 100  FORMAT (5X,10HRESIDUAL  ,26X,25A1)
C
C     ==================================================================
C
      END
*TWPRNA
      SUBROUTINE TWPRNA (KCOEF,KY,NROWS,NCOLS,NRESDF,MA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. TWPRNA V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT TUKEY'S TEST FOR NONADDITIVITY FOR TWOWAY INSTRUCTION.
C
C     STORAGE IN SCRATCH AREA A(.) ...
C
C        A(12) = NONADDITIVITY SUM OF SQUARES
C        A(13) = BALANCE SUM OF SQUARES
C        A(14) = RESIDUAL SUM OF SQUARES
C        A(15) = NONADDITIVITY MEAN SQUARE
C        A(16) = BALANCE MEAN SQUARE
C        A(17) = RESIDUAL MEAN SQUARE
C        A(18) = NONADDITIVITY F RATIO
C        A(19) = NONADDITIVITY SIGNIFICANCE LEVEL
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1978.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             TEMP(1)
      REAL             SSNA
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
C
C     ==================================================================
C
C     COMPUTE NONADDITIVITY SUM OF SQUARES.
C
      K  = KY
      KR = KCOEF + IONE
      CALL SUMMAL (TEMP,IZERO,SSNA)
      DO 20 I=1,NROWS
      KC = KCOEF + NROWS + IONE
        DO 10 J=1,NCOLS
          TEMP(1) = RC(KR) * RC(KC) * RC(K)
          CALL SUMMAL (TEMP,-IONE,SSNA)
          K  = K  + IONE
          KC = KC + IONE
  10    CONTINUE
        KR = KR + IONE
  20  CONTINUE
      CALL SUMMAL (TEMP,IONE,SSNA)
C
C      SUMS OF SQUARES.
C
      TEMP(1) = FDIV (A(1),FLOAT(NROWS),IND)
      TEMP(1) = TEMP(1) * FDIV (A(2),FLOAT(NCOLS),IND)
      A(12) = FDIV (SSNA*SSNA,TEMP(1),IND)
      A(13) = A(3) - A(12)
      A(14) = A(3)
C
C     MEAN SQUARES.
C
      A(15) = A(12)
      NBALDF = NRESDF - IONE
      A(16) = FDIV (A(13),FLOAT(NBALDF),IND)
      A(17) = A(7)
C
C     NONADDITIVITY F RATIO AND SIGNIFICANCE LEVEL.
C
      A(18) = FDIV (A(15),A(16),IND)
      CALL QFORF (RONE,FLOAT(NRESDF)-RONE,A(18),TEMP(1))
      A(19) = TEMP(1)
C
C     SET UP PRINTING CONSTANTS.
C
      CALL RFORMT (0,ISIGD,A(12),RC(1),  3,15,NW1,ND1,MA( 1),IRF)
      CALL RFORMT (0,ISIGD,A(15),RC(1),  3,15,NW2,ND2,MA(18),IRF)
      NB1 = 17 - NW2
      NB2 = 17 - NW2
C
C     PRINT NONADDITIVITY DF, SS, MS, F, AND P(F).
C
      CALL RFORMT (1,ISIGD,RC   ,A(12),NB1, 1,NW1,ND1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC   ,A(15),NB2, 1,NW2,ND2,MA(18),IRF)
      WRITE (IPRINT,30)
      WRITE (IPRINT,40) (LA(39),I=1,70)
      WRITE (IPRINT,50) (MA(I),I=1,34), A(18), A(19)
C
C     PRINT BALANCE DF, SS AND MS.
C
      CALL RFORMT (1,ISIGD,RC   ,A(13),NB1, 1,NW1,ND1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC   ,A(16),NB2, 1,NW2,ND2,MA(18),IRF)
      WRITE (IPRINT,60) NBALDF, (MA(I),I=1,34)
C
C     PRINT RESIDUAL DF, SS AND MS.
C
      CALL RFORMT (1,ISIGD,RC   ,A(14),NB1, 1,NW1,ND1,MA( 1),IRF)
      CALL RFORMT (1,ISIGD,RC   ,A(17),NB2, 1,NW2,ND2,MA(18),IRF)
      WRITE (IPRINT,70) NRESDF, (MA(I),I=1,34)
      WRITE (IPRINT,40) (LA(39),I=1,70)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  30  FORMAT (20X,31HTUKEY'S TEST FOR NON-ADDITIVITY)
  40  FORMAT (1X,70A1)
  50  FORMAT (1X,17HNON-ADDITIVITY  1,2X,34A1,2X,0PF7.3,F8.3)
  60  FORMAT (1X,13HBALANCE      ,I4,2X,34A1)
  70  FORMAT (1X,13HRESIDUALS    ,I4,2X,34A1)
C
C     ==================================================================
C
      END
*TWPRTR
      SUBROUTINE TWPRTR (IWT,KWT,KCOEF,KRES,KSDPV,NROWS,NCOLS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TWPRTR V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT TABLE OF RESIDUALS FOR TWOWAY INSTRUCTION.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   MAY, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             RESVAR
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
      MLINES = IDIV (NCOLS-IONE,8,IND) + ITWO
      NLINES = 40 + NROWS + NCOLS
      IF (IWT.EQ.IONE) NLINES = NLINES - ITHRE
      IF (NLINES+NROWS*MLINES.LE.60) GO TO 10
      IF (NCRT.NE.IZERO) GO TO 10
        CALL PAGE (IFOUR)
        NLINES = 6
        GO TO 20
C
  10  WRITE (IPRINT,80)
  20  WRITE (IPRINT,90) NROWS, NCOLS
      NOCOLP = MIN0 (8,NCOLS)
C
      K = KCOEF + ITWO * (IONE + NROWS + NCOLS) + IFOUR
      RESVAR = RC(K)
      DO 30 I=1,NOCOLP
        KIND(I)=I
  30  CONTINUE
C
      WRITE (IPRINT,100) (KIND(I),I=1,NOCOLP)
      WRITE (IPRINT,110)
C
      ISIZE = NROWS * NCOLS
      J = KRES
      K = KSDPV
      L = KWT
      DO 60 I=1,ISIZE
        IF (IWT.EQ.IONE) GO TO 40
        A(I) = FDIV (RC(J),FSQRT(RESVAR-RC(K)**2),IND)
        GO TO 50
  40    IF (RC(L).GT.RZERO) A(I) =
     1    FDIV (RC(J),FSQRT(FDIV(RESVAR,RC(L),IND)-RC(K)**2),IND)
        IF (RC(L).LE.RZERO) A(I) = RZERO
        L = L + IONE
  50    J = J + IONE
        K = K + IONE
  60  CONTINUE
C
      LINTOP = 60
      IF (NCRT.NE.IZERO) THEN
      NLINES = IFIVE
      LINTOP = LENGTH 
      ENDIF
      DO 70 I=1,NROWS
        WRITE (IPRINT,80)
        NOCOLP = NCOLS * (I-IONE)
        KA = NOCOLP + NCOLS
        KB = NOCOLP + IONE
        KC = NOCOLP + MIN0 (NCOLS,8)
        KD = KC + IONE
        WRITE (IPRINT,120) I, (A(K),K=KB,KC)
        IF (NCOLS.GT.8) WRITE (IPRINT,130) (A(K),K=KD,KA)
        NLINES = NLINES + MLINES
        IF (NLINES.LE.LINTOP) GO TO 70
          CALL PAGE (IFOUR)
          NLINES = IZERO
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  80  FORMAT (1X)
  90  FORMAT (/1X,5X,I2,3H X ,I2,36H TABLE OF RESIDUALS, STANDARDIZED BY
     1     /11X,49HDIVIDING EACH RESIDUAL BY ITS STANDARD DEVIATION.)
 100  FORMAT (/9H   COLUMN,3X,I4,4X,I4,4X,I4,4X,I4,4X,I4,4X,I4,4X,I4,
     1   4X,I4)
 110  FORMAT (8H    ROW )
 120  FORMAT (2X,I4,2X,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,
     1                 2X,F6.2,2X,F6.2)
 130  FORMAT (      8X,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,2X,F6.2,
     1                 2X,F6.2,2X,F6.2)
C
C     ==================================================================
C
      END
*TWRNKS
      SUBROUTINE TWRNKS (KY,NROWS,NCOLS,ROWFRD,COLFRD,ROWKEN,COLKEN)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TWRNKS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE KENDALL W AND FRIEDMAN CHISQUARED RANK STATISTICS.
C
C     INPUT ...
C
C        KY     = STARTING LOCATION OF Y IN RC(.).
C        NROWS  = NUMBER OF ROWS    IN TWOWAY TABLE.
C        NCOLS  = NUMBER OF COLUMNS IN TWOWAY TABLE.
C
C     OUTPUT ...
C
C        ROWFRD = FRIEDMAN CHISQUARED STATISTIC FOR ROW    RANK SUMS.
C        COLFRD = FRIEDMAN CHISQUARED STATISTIC FOR COLUMN RANK SUMS.
C        ROWKEN = KENDALL'S W STATISTIC FOR ROW    RANK SUMS.
C        COLKEN = KENDALL'S W STATISTIC FOR COLUMN RANK SUMS.
C
C     STORAGE ...
C
C        DESCRIPTION                   START STORE           LENGTH
C        -----------------------       -----------           ------
C        SUM OF RANKS IN ROWS             A(KD)               NROWS
C        SUM OF RANKS IN COLUMNS          A(KA)               NCOLS
C        HIERARCHY                        A(KH)               MAXRC
C        RANKS                            A(KR)               MAXRC
C        Y FOR RANKS IN COLUMNS           A(KS)               NROWS
C
C     REFERENCE ...
C        GIBBONS, JEAN DICKINSON (1971). NONPARAMETRIC STATISTICAL
C           INFERENCE. MCGRAW-HILL BOOK COMPANY. CHAPTER 13, P. 243 FF.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             COLFRD, COLKEN, ROWFRD, ROWKEN
      REAL             TEMP(1)
      REAL             C, CONST, CRSAVE, RRSAVE, R, SSCOLD, SSROWD
      REAL             STPRME, T, TROW, TCOL
      REAL             FDIV
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 12.0 /
C
C     ==================================================================
C
C     INITIALIAZATION.
C
      MAXRC = MAX0 (NROWS,NCOLS)
C
      KD = IONE
      KA = KD + NROWS
      KH = KA + NCOLS
      KR = KH + MAXRC
      KS = KR + MAXRC
C
      R  = FLOAT (NROWS)
      C  = FLOAT (NCOLS)
C
C     ZERO OUT SCRATCH AREA A(.).
C
      DO 10 I=1,KH
        A(I) = RZERO
  10  CONTINUE
      TROW = RZERO
      TCOL = RZERO
C
C     COMPUTE SUM OF RANKS IN ROWS FOR EACH ROW.
C
      JRCSUB = KY
      DO 30 I=1,NROWS
        CALL TWRANK (NCOLS,RC(JRCSUB),A(KH),A(KR),T)
        L = KA
        M = KR
        DO 20 J=1,NCOLS
          A(L) = A(L) + A(M)
          L = L + IONE
          M = M + IONE
  20    CONTINUE
        JRCSUB = JRCSUB + NCOLS
        TCOL = TCOL + T
  30  CONTINUE
C
C     COMPUTE SUM OF RANKS IN COLUMNS FOR EACH COLUMN.
C
      DO 60 I=1,NCOLS
C
C       MOVE Y TO SCRATCH AREA.
C
        K = KS
        L = KY + I - IONE
        DO 40 J=1,NROWS
          A(K) = RC(L)
          K = K + IONE
          L = L + NCOLS
  40    CONTINUE
        CALL TWRANK (NROWS,A(KS),A(KH),A(KR),T)
        M = KD
        N = KR
        DO 50 K=1,NROWS
          A(M) = A(M) + A(N)
          M = M + IONE
          N = N + IONE
  50    CONTINUE
        TROW = TROW + T
  60  CONTINUE
C
C     COMPUTE SUM OF SQUARED DEVIATIONS OF ROW RANK SUMS
C        FROM ROW RANK SUM MEAN.
C
      RRSAVE = FDIV (C*(R+RONE),RTWO,IND)
C
      K = KD
      CALL SUMMAL (TEMP,IZERO,SSROWD)
      DO 70 I=1,NROWS
        TEMP(1) = (A(K)-RRSAVE)**2
        CALL SUMMAL (TEMP,-IONE,SSROWD)
        K = K + IONE
  70  CONTINUE
      CALL SUMMAL (TEMP,IONE,SSROWD)
C
C     COMPUTE SUM OF SQUARED DEVIATIONS OF COLUMN RANK SUMS
C        FROM COLUMN RANK SUM MEAN.
C
      CRSAVE = FDIV (R*(C+RONE),RTWO,IND)
C
      K = KA
      CALL SUMMAL (TEMP,IZERO,SSCOLD)
      DO 80 I=1,NCOLS
        TEMP(1) = (A(K)-CRSAVE)**2
        CALL SUMMAL (TEMP,-IONE,SSCOLD)
        K = K + IONE
  80  CONTINUE
      CALL SUMMAL (TEMP,IONE,SSCOLD)
C
C     COMPUTE STATISTICS.
C
      CONST  = FDIV (R*C,SPCA,IND)
C
C     FOR ROWS ...
C
      STPRME = CONST * (R-RONE) * (R+RONE) - TROW
      ROWKEN = FDIV (SSROWD,C*STPRME,IND)
      ROWFRD = C * (R-RONE) * ROWKEN
C
C     FOR COLUMNS ...
C
      STPRME = CONST * (C-RONE) * (C+RONE) - TCOL
      COLKEN = FDIV (SSCOLD,R*STPRME,IND)
      COLFRD = R * (C-RONE) * COLKEN
C
C
      RETURN
C
C     ==================================================================
C
      END
*UCSUMS
      SUBROUTINE UCSUMS (Y,N,SINDEX,MAXI,UCSS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UCSUMS V 7.00  5/28/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO COMPUTE UNCORRECTED SUM OF SQUARES FOR A
C        SPECIFIC FACTOR.
C
C     NUMBER OF LEVELS OF FACTOR IS MAXI.
C        LEVELS OF FACTOR ARE IN INDEX(.).
C
C     UCSS = UNCORRECTED SUM OF SQUARES
C          = SUM OVER I OF YI. SQUARED, WHERE
C               YI. IS THE SUM OF ALL Y(J) FOR INDEX(J) = I, J=1,2,...,N.
C
C     NOTE, UCSS IS COMPUTED WITHOUT THE APPROPRIATE DIVISOR.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - MAY, 1978.
C                   CURRENT VERSION - MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION SINDEX(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             Y(*)
      REAL             SUM
C
      DOUBLE PRECISION UCSS
      DOUBLE PRECISION DTEMP(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
      K = N + IONE
      DO 20 I=1,MAXI
        CALL SUMMAL (Y(1),IZERO,SUM)
        ITAL = IZERO
        JJ   = IONE
        DO 10 J=1,N
          IF (INT(SINDEX(J)+.05).NE.I) GO TO 10
          CALL SUMMAL (Y(J),-IONE,SUM)
          ITAL = ITAL + IONE
          IF (ITAL.EQ.IONE) JJ = J
  10    CONTINUE
        CALL SUMMAL (Y(1),IONE,A(K))
        IF (ITAL.EQ.IONE) A(K) = Y(JJ)
        K = K + IONE
  20  CONTINUE
C
      K = N + IONE
      CALL DSUMAL (DTEMP,IZERO,DSUM)
      DO 30 I=1,MAXI
        DTEMP(1) = DBLE ( A(K) ) ** 2
        CALL DSUMAL (DTEMP,-IONE,DSUM)
        K = K + IONE
  30  CONTINUE
      CALL DSUMAL (DTEMP,IONE,DSUM)
      IF (MAXI.EQ.IONE) DSUM = DTEMP(1)
      UCSS = DSUM
      RETURN
C
C     ==================================================================
C
      END
*UNIPLT
      SUBROUTINE UNIPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UNIPLT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A UNIFORM
C              PROBABILITY PLOT.
C              THE PROTOTYPE UNIFORM DISTRIBUTION USED HEREIN
C              IS DEFINED ON THE UNIT INTERVAL (0,1).
C              THIS DISTRIBUTION HAS MEAN = 0.5
C              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
C              THIS DISTRIBUTION HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 1.
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE UNIFORM PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE UNIFORM DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE UNIFORM PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1975.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION M(10), MT(50)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             W(*), X(*), Y(*)
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER*1      M, MT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10) /
     1      ',',  ' ',  'S',  'L',  'O',  'P',  'E',  ' ',  '=',   ' ' /
C
      DATA ICA / 77 /
      DATA ICB / 65 /
      DATA ICC / 73 /
      DATA ICD / 38 /
      DATA ICE / 37 /
C
      DATA  NX / 20 /
C
C     ==================================================================
C
      AN = N
      CALL SORTPP (X,N,Y)
      CALL UNIMED (N,W)
      IF (LWIDE.GE.ICA                 )
     1  WRITE (IPRINT, 90) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.ICB .AND. LWIDE.LT.ICA)
     1  WRITE (IPRINT,100)    (LHEAD(I),I=1,12),N
      IF (LWIDE.LT.ICB                 )
     1  WRITE (IPRINT,110) N, (LHEAD(I),I=1,12)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = RHALF
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 10 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  10  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 20 I=1,N
        ATEMP(1) = (Y(I)-YBAR)*(W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  20  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 30 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
  30  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1)*WBAR
      CALL RFORMT (0,ISIGD,YINT,A(1),1,NX,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0, 0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 40 I=1,10
        MT(K) = M(I)
        K = K + IONE
  40  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,NX,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0, 0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+ICC.GT.LWIDE) GO TO 50
        WRITE (IPRINT,120) CC, (MT(J),J=1,K)
        GO TO 80
  50  CALL RFORMT (0,ISIGD,YINT,A(1),1,NX,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0, 0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 60 I=1,10
        MT(K) = M(I)
        K = K + IONE
  60  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,NX,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0, 0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+ICD.GT.LWIDE) GO TO 70
        WRITE (IPRINT,130) CC, (MT(J),J=1,K)
        GO TO 80
  70  IF (LWIDE.LT.ICE) GO TO 80
        WRITE (IPRINT,140) CC
  80  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  90  FORMAT (15X, 7HUNIFORM           ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 100  FORMAT (15X, 7HUNIFORM           ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 110  FORMAT ( 1X,3HN =,I5,6X, 7HUNIFORM           ,14H PROB PLOT OF ,
     1                           12A1)
 120  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 130  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 140  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*UNXRDC   
      SUBROUTINE  UNXRDC (LL,LONE,NTAPE)
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. UNXRDC V 7.00  6/ 9/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     EXECUTE INSTRUCTIONS READ UNIT OR CREAD UNIT.         
C         
C     INPUT ...     
C               LL  
C               LONE
C               NTAPE         
C         
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2844 
C                  ORIGINAL VERSION - FEBRUARY, 1978.       
C                   CURRENT VERSION -     JUNE, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IA(150), IB(150)        
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD        
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR        
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH         
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER LFMTP*80      
      CHARACTER IA*1
C
C     ==================================================================        
C         
      NCT   = IARGS(1)        
      IF (LONE.EQ.IONE) GO TO 20        
C         
C     INSTRUCTION IS CREAD.   
C         
      IF (IARGS(1).LE.IZERO .OR. KIND(1).NE.IZERO) CALL ERROR (3)     
      IF (NARGS.EQ.IONE) CALL ERROR (28)
      NARGS = NARGS - IONE    
      DO 10 I=1,NARGS         
        IARGS(I) = IARGS(I+1) 
        KIND(I)  = KIND(I+1)  
  10  CONTINUE      
C         
      CALL CHKCOL   
      IF (NERROR.NE.IZERO) RETURN       
      IREAD = ITWO  
      GO TO 40      
C         
C     ..................................................................        
C         
C     INSTRUCTION IS READ UNIT.         
C         
  20  IF (NARGS.GT.IZERO) GO TO 30      
      CALL ERROR (10)         
      RETURN        
C         
C     ..................................................................        
C         
  30  CALL CHKCOL   
      IF (NERROR.NE.IZERO) RETURN       
      IREAD = IONE  
  40  IC    = IZERO 
      ISWT  = IZERO 
      IFLG  = IZERO 
      IBLK  = IZERO 
      IPOS  = ITHRE 
      IF (LL.NE.IONE) GO TO 210         
C         
C     ..................................................................        
C         
C     AREA WHERE DATA IS READ INTO A, LOGICAL RECORD PICKED UP AND    
C        CONVERTED. 
C         
  50  IF (ISWT.NE.IZERO) GO TO 60       
      READ (NTAPE,300,END = 285) (IA(I+2),I=1,NCHPR)        
C         
C     ABOVE FORMAT MUST BE CHANGED, IF PHYSICAL RECORD EXCEEDS 132.   
C         
      IBLK = IZERO  
      IPOS = ITHRE  
      ISWT = IONE   
      CALL OMCONV (IA(3),IB(3),NCHPR)   
C         
C     CONVERT NUMBERS.        
C         
  60  MP = IPOS     
      IB(MP-1) = 11       
      MSTOP = MP + LURCD      
      JA = IZERO    
  70  K  = IB(MP)   
      IF (K.LT.ITEN) GO TO 90 
      MP = MP + IONE
      IF (MP.LT.MSTOP) GO TO 70         
  80  IBLK = IBLK + IONE      
      IPOS = IPOS + LURCD     
      IF (IBLK.EQ.NBLKPR) ISWT = IZERO  
      IF (LONE.EQ.ITHRE) GO TO 130      
      GO TO 140     
C         
  90  KRDPOS = MP   
      CALL AARGS (IB)         
      IF (KARG.LT.IZERO .AND. KRDPOS.GE.MSTOP) GO TO 100    
      IF (KARG.LT.IZERO .AND. KRDPOS.LT.MSTOP) GO TO 130    
      JA = JA + IONE
      ARGTAB(JA) = ARG        
      IF (KRDPOS.EQ.MSTOP) GO TO 80     
      IF (KRDPOS.LT.MSTOP) GO TO 120    
 100  MS = MSTOP - IONE       
      IB(MP-2) = 11       
      DO 110 I=MP,MS
        IB(I-1) = IB(I)       
 110  CONTINUE      
C         
      IB(MS) = 11         
      JA = JA - IONE
      MP = MP - IONE
      GO TO 90      
C         
 120  MP = KRDPOS   
      GO TO 70      
C         
 130  CALL ERROR (3)
      RETURN        
C         
C     ..................................................................        
C         
C     READ WITHOUT FORMAT.    
C         
 140  IF (JA.GE.NARGS) GO TO 160        
      JAB = JA + IONE         
      DO 150 I=JAB,NARGS      
        ARGTAB(I) = RZERO     
 150  CONTINUE      
C         
 160  IF (IREAD.EQ.ITWO) GO TO 180      
      DO 170 I=1,NARGS        
        IF (ARGTAB(I).NE.RZERO) GO TO 180         
 170  CONTINUE      
      GO TO 280     
C         
C     ..................................................................        
C         
 180  IF (IC.LT.NROW) GO TO 190         
      IF (IFLG.EQ.IZERO) CALL ERROR (201)         
      IFLG = IFLG + IONE      
      IF (IREAD.EQ.IONE) GO TO 50       
      IF (IFLG+IC.EQ.NCT) GO TO 280     
      GO TO 50      
C         
 190  DO 200 I=1,NARGS        
        K = IARGS(I) + IC     
        RC(K) = ARGTAB(I)     
 200  CONTINUE      
C         
      IC = IC + IONE
      IF (IREAD.EQ.IONE) GO TO 50       
      IF (IC.EQ.NCT) GO TO 280
      GO TO 50      
C         
C     READ UNIT WITH FORMAT.  
C         
 210  CALL PREPAK (ITWO,LL,I,LFMT,LFMTP,LFMTP,IND)      
      IF (IND.EQ.IZERO) GO TO 220       
      CALL ERROR (27)         
      RETURN        
C         
C     ..................................................................        
C         
 220  READ (NTAPE,LFMTP,END = 285) (ARGTAB(L),L = 1,NARGS)  
      IF (IREAD.NE.IONE) GO TO 240      
      DO 230 L=1,NARGS        
        IF (ARGTAB(L).NE.RZERO) GO TO 240         
 230  CONTINUE      
      GO TO 280     
C         
 240  IF (IC.GE.NROW .AND. IFLG.LE.IZERO) GO TO 260         
      IF (IC.GE.NROW .AND. IFLG.GT.IZERO) GO TO 270         
      DO 250 L=1,NARGS        
        K = IARGS(L)
        IARGS(L) = K + IONE   
        RC(K) = ARGTAB(L)     
 250  CONTINUE      
C         
      IC = IC + IONE
      IF (IREAD.EQ.IONE) GO TO 220      
      IF (IC.EQ.NCT) GO TO 280
      GO TO 220     
C         
 260  CALL ERROR (201)        
 270  IFLG = IFLG + IONE      
      IF (IREAD.EQ.IONE) GO TO 220      
      IF (IC+IFLG.EQ.NCT) GO TO 280     
      GO TO 220     
C         
 280  NROLD = NRMAX 
      NRMAX = IC    
      CALL ERROR (252)        
      ICC   = IC + IFLG       
      WRITE (ISCRT,290) ICC   
      RETURN        
 285  CALL ERROR (267)
      RETURN
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 290  FORMAT (5X,I4,34H DATA RECORDS READ BUT NOT PRINTED,27X)        
 300  FORMAT (132A1)
C         
C     ==================================================================        
C         
      END 
*UNXSET
      SUBROUTINE  UNXSET (LONE,NTAPE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. UNXSET V 7.00  6/ 9/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS SET UNIT AND CSET UNIT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1978.
C                   CURRENT VERSION -     JUNE, 1992.
C
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IA(150), IB(150)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER IA*1
C
C     ==================================================================
C
      NCT  = IZERO
      IROW = IONE
      IBLK = IZERO
      IPOS = ITHRE
      IF (LONE.EQ.IFIVE) GO TO 120
C
C     SET UNIT  L  INSTRUCTION.
C
      IF (NARGS.LT.IONE .OR. NARGS.GT.ITWO) CALL ERROR (10)
      CALL ADRESS (NARGS,JAD)
      IF (JAD.LT.IZERO) CALL ERROR (20)
      IF (NARGS.NE.ITWO) GO TO 10
      IROW = IARGS(1)
      IF (KIND(1).EQ.IONE) IROW = ARGS(1)
      IF (IROW.GT.NROW .OR. IROW.LE.IZERO) CALL ERROR (16)
  10  JSTOP = JAD + NROW - IONE
      IREAD = IONE
      JSTRT = JAD + IROW - IONE
  20  JDIF  = JSTOP - JSTRT + IONE
      ISWT  = IZERO
      IF (NERROR.NE.IZERO) RETURN
C
C     SET UNIT WITHOUT FORMAT.
C
      IFLG  = IZERO
      IC    = IZERO
      ICARD = IZERO
      GO TO 140
C
  30  IF (JA.EQ.IZERO .AND. IREAD.LE.IONE) GO TO 50
      IF (JA.EQ.IZERO) GO TO 60
      IF (IREAD.EQ.ITWO) GO TO 70
      DO 40 I=1,JA
        IF (ARGTAB(I).NE.RZERO) GO TO 70
  40  CONTINUE
C
  50  NROLD = NRMAX
      NRMAX = IC + IROW - IONE
      CALL ERROR (252)
      WRITE (ISCRT,230) IC
      RETURN
C
C     ..................................................................
C
  60  JA = IONE
      ARGTAB(1) = RZERO
  70  IF (IC+JA.LE.JDIF) GO TO 90
      IF (IFLG.EQ.IZERO) GO TO 80
      IFLG = IFLG + JA
C
      GO TO (140,110), IREAD
C
  80  CALL ERROR (201)
      IFLG = JA - JDIF + IC
      JA   = JDIF - IC
  90  DO 100 I=1,JA
        RC(JSTRT) = ARGTAB(I)
        JSTRT = JSTRT + IONE
        IC    = IC + IONE
 100  CONTINUE
C
      ICARD = ICARD + IONE
      IF (IREAD.EQ.IONE) GO TO 140
 110  IF (ICARD.GE.NCT) GO TO 50
      GO TO 140
C
C     INSTRUCTION IS CSET.
C
 120  IF (NARGS.LT.IONE .OR. NARGS.GT.ITHRE) CALL ERROR (10)
      NCT = IARGS(1)
      IF (KIND(1).NE.IZERO) NCT = ARGS(1)
      IREAD = ITWO
      IF (NARGS.NE.ITHRE) GO TO 130
      IROW = IARGS(2)
      IF (KIND(2).NE.IZERO) IROW = ARGS(2)
      IF (IROW.GT.NROW .OR. IROW.LE.IZERO) CALL ERROR (16)
 130  CALL ADRESS (NARGS,JAD)
      IF (JAD.LT.IZERO) CALL ERROR (20)
      JSTRT = JAD + IROW - IONE
      JSTOP = JAD + NROW - IONE
      GO TO 20
C
C     ..................................................................
C
C     AREA WHERE  DATA IS READ INTO A, LOGICAL RECORD PICKED UP AND
C        CONVERTED.
C
 140  IF (ISWT.NE.IZERO) GO TO 150
      READ (NTAPE,240,END = 225) (IA(I+2),I=1,NCHPR)
C
C     ABOVE FORMAT MUST BE CHANGED IF PHYSICAL RECORD EXCEEDS 132.
C
      IBLK = IZERO
      IPOS = ITHRE
      ISWT = IONE
      CALL OMCONV (IA(3),IB(3),NCHPR)
C
C     CONVERT NUMBERS.
C
 150  MP = IPOS
      IB(MP-1) = 44
      MSTOP = MP + LURCD
      JA = IZERO
 160  K = IB(MP)
      IF (K.LT.ITEN) GO TO 180
      MP = MP + IONE
      IF (MP.LT.MSTOP) GO TO 160
 170  IBLK = IBLK + IONE
      IPOS = IPOS + LURCD
      IF (IBLK.EQ.NBLKPR) ISWT = IZERO
      GO TO 30
C
 180  KRDPOS = MP
      CALL AARGS (IB)
      IF (KARG.LT.IZERO .AND. KRDPOS.GE.MSTOP) GO TO 190
      IF (KARG.LT.IZERO .AND. KRDPOS.LT.MSTOP) GO TO 220
      JA = JA + IONE
      ARGTAB(JA) = ARG
      IF (KRDPOS.EQ.MSTOP) GO TO 170
      IF (KRDPOS.LT.MSTOP) GO TO 210
 190  MS = MSTOP - IONE
      IB(MP-2) = 44
      DO 200 I=MP,MS
        IB(I-1) = IB(I)
 200  CONTINUE
C
      IB(MS) = 44
      JA = JA - IONE
      MP = MP - IONE
      GO TO 180
C
 210  MP = KRDPOS
      GO TO 160
C
 220  CALL ERROR (3)
      RETURN
 225  CALL ERROR (267)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 230  FORMAT (5X,I5, 33H DATA VALUES READ BUT NOT PRINTED,27X)
 240  FORMAT (132A1)
C
C     ==================================================================
C
      END
*UNXWRT   
      SUBROUTINE  UNXWRT (LL,NTAPE)     
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. UNXWRT V 7.00  4/28/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     EXECUTE INSTRUCTION WRITE UNIT.   
C         
C     INPUT ...     
C         
C        LL AND NTAPE.        
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899      
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - FEBRUARY, 1978.       
C                   CURRENT VERSION -    APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IA(150)       
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR        
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NLOCRM         
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH         
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER   LA*1
      CHARACTER   LFMTP*80      
      CHARACTER   IA*1
C
C     ==================================================================        
C         
C     INSTRUCTION IS WRITE UNIT.        
C         
      IF (NARGS.NE.IZERO) GO TO 10      
      CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
  10  CALL CHKCOL   
      IF (NERROR.NE.IZERO) RETURN       
      IF (LL.NE.IONE) GO TO 170         
C         
C     WRITE WITHOUT FORMAT.   
C         
      IVAL = IDIV (LURCD,IPLACE,IND)    
      IVR  = MOD (LURCD,IPLACE)         
      IF (IVAL.NE.IZERO) GO TO 20       
      CALL ERROR (245)        
      RETURN        
C         
C     ..................................................................        
C         
  20  IBLK = IZERO  
      IPOS = IONE   
      IS   = ISIGD - IONE     
      ND   = IS + ITWO        
      IRC  = IZERO  
      DO 90 I=1,NRMAX         
        DO 60 J=1,NARGS       
          JA   = IARGS(J) + I - IONE    
          CALL RFORMT (5,IS,A,RC(JA),0,0,IPLACE,ND,IA(IPOS),IRF)      
          IPOS = IPOS + IPLACE
          IRC  = IRC + IONE   
          IF (IRC.LT.IVAL) GO TO 60     
          IF (IVR.EQ.IZERO) GO TO 40    
          DO 30 JB=1,IVR      
            IA(IPOS) = LA(45) 
            IPOS = IPOS + IONE
  30      CONTINUE  
  40      IBLK = IBLK + IONE  
          IF (IBLK.LT.NBLKPR) GO TO 50  
          WRITE (NTAPE,210) (IA(L),L=1,NCHPR)     
          IPOS = IONE         
          IBLK = IZERO        
  50      IRC  = IZERO        
  60    CONTINUE    
C         
C       LAST LOGICAL RECORD FOR SOME ROW MUST BE PADDED WITH BLANKS.  
C         
        IF (IRC.EQ.IZERO) GO TO 90      
        ISTRT = IRC * IPLACE + IONE     
        DO 70 J=ISTRT,LURCD   
          IA(IPOS) = LA(45)   
          IPOS = IPOS + IONE  
  70    CONTINUE    
        IBLK = IBLK + IONE    
        IF (IBLK.LT.NBLKPR) GO TO 80    
        WRITE (NTAPE,210) (IA(J),J=1,NCHPR)       
        IPOS = IONE 
        IBLK = IZERO
  80    IRC  = IZERO
  90  CONTINUE      
C         
C     WRITE OUT NARGS ZEROS AND PAD PHYSICAL RECORD WITH BLANKS.      
C         
      CALL RFORMT (5,IS,RC,RZERO,0,0,IPLACE,ND,IA(IPOS),IRF)
      IHOLD = IPOS  
      IPOS  = IPOS + IPLACE   
      IRC   = IRC + IONE      
      JB    = IONE  
      IF (IRC.GE.IVAL) GO TO 130        
 100  IRCC  = IRC + IONE      
      DO 120 J=IRCC,IVAL      
        IH = IHOLD  
        DO 110 JA=1,IPLACE    
          IA(IPOS) = IA(IH)   
          IH = IH + IONE      
          IPOS = IPOS + IONE  
 110    CONTINUE    
        JB = JB + IONE        
 120  CONTINUE      
C         
      IBLK = IBLK + IONE      
 130  IF (JB.GE.NARGS) GO TO 140        
      IRC  = IZERO  
      IF (IBLK.LT.NBLKPR) GO TO 100     
      WRITE (NTAPE,210) (IA(I),I=1,NCHPR)         
      IPOS = IONE   
      IBLK = IZERO  
      GO TO 100     
C         
 140  IF (IPOS.GE.NCHPR) GO TO 160      
      DO 150 I=IPOS,NCHPR     
        IA(I) = LA(45)        
 150  CONTINUE      
C         
 160  WRITE (NTAPE,210) (IA(I),I=1,NCHPR)         
      RETURN        
C         
C     ..................................................................        
C         
C     WRITE WITH FORMAT.      
C         
 170  CALL PREPAK (ITWO,LL,I,LFMT,IA,LFMTP,IND)      
      IF (IND.EQ.IZERO) GO TO 180       
      CALL ERROR (27)         
      RETURN        
C         
C     ..................................................................        
C         
 180  DO 200 I=1,NRMAX        
        DO 190 J=1,NARGS      
          JA = IARGS(J) + I - IONE      
          ARGTAB(J) = RC(JA)  
 190    CONTINUE    
        WRITE (NTAPE,LFMTP) (ARGTAB(J),J=1,NARGS) 
 200  CONTINUE      
      WRITE (NTAPE,LFMTP) (RZERO,J=1,NARGS)       
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 210  FORMAT (132A1)
C         
C     ==================================================================        
C         
      END 
*VARFIT
      SUBROUTINE  VARFIT
C
C **  NBS OMNITAB 1980 VERSION 6.08  3/ 3/86. VARFIT V 7.00  5/12/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO COMPUTE AND PLOT VARIANCE FIT.
C
C     IF L1 = 22, L2 = 8 VFIT.
C     IF L1 = 22, L2 = 9 VPOLFIT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD. 20899.
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - MARCH, 1986.
C                   CURRENT VERSION -   MAY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IIRGS(100), IB(52)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON / FILE / IFILE, ISFILE, NUNIT(10)
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12)
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DIG, SD, SU, VARNCE
C
C     ..................................................................
C
      CHARACTER LA*1
      CHARACTER IB*1
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1
      CHARACTER SCREEN*4
C
C     ..................................................................
C
      EQUIVALENCE (IIRGS(1),LFMT(1))
C
C     ==================================================================
C
      SCREEN(1:4) = CHAR(27)//CHAR(91)//CHAR(50)//CHAR(74)
      LINES = 25
      IF (NCRT.NE.IZERO .AND. LENGTH.LE.LINES) LINES = 20
      LL2 = L2
      IF (L2.EQ.8) L2 = ITHRE
      IF (L2.EQ.9) L2 = IONE
      LSIND = IONE
      CALL LSFIT (JS,ISTART,M,N,VARNCE,SU,SD,MMTXR,MMTXC,NIT,DIG,LSIND)
      IF (NERROR.NE.IZERO) RETURN
C
C     IF JS = 0,  NO STORAGE REQUIRED.
C
      IF (JS.EQ.IZERO) GO TO 10
C
C     STORE RESULTS.
C
      CALL LSTORE (JS,IIRGS,ISTART,M,VARNCE,SU,N,SD,MMTXR,MMTXC)
C
C     START PRINTING IF POLYFIT OR FIT.
C
  10  CALL LSPRNT (M,N,IIRGS,SD,VARNCE,SU,NIT,DIG,NX,LSIND)
C
      ISUBB  = IONE
      ISUBRS = M + IONE
      ISUBR  = ISUBRS + N
      ISUBSB = ISUBR + IDIV ((M+IONE)*(M+ITWO),ITWO,IND)
      ISUBSY = ISUBSB + M + IONE
      ISUBS  = ISUBSY + N
      ISUBQ  = ISUBS + M + ITWO
      ISUBF  = ISUBQ + N * (M+IONE)
      ISUBAD = ISUBF + M + IONE
      ISUBYP = ISUBAD + M
      ISUBOY = ISUBYP + N
      ISUBIU = ISUBOY + N
      ISUBWT = ISUBIU + N
      ISUBPV = ISUBQ
      ISUBY = IIRGS(1)
C
      IF (KIND(2).EQ.IZERO) THEN
        IW     = IIRGS(2)
        IWST   = IONE
      ELSE
        IW     = IONE
        IWST   = ITWO
        WT     = ARGS(2)
      END IF
      IWS  = IW
      LPRT = IONE
C
C       COMPUTE PREDICTED RESPONSES AND STORE IN PLACE OF MATRIX Q
C          WHICH IS USED BY LSQ.
C
      JSUBPV = ISUBPV
      MSUBRS = ISUBRS
      JSUBY  = ISUBY
      DO 50 I=1,N
        A(JSUBPV) = RC(JSUBY) - A(MSUBRS)
        JSUBPV = JSUBPV + IONE
        JSUBY  = JSUBY + IONE
        MSUBRS = MSUBRS + IONE
  50  CONTINUE
C
C     COMPUTE PRINTING POSITION OF Y AXIS AND STORE IN IU VECTOR.
C
      KSUBRS = ISUBRS
      JSUBSY = ISUBSY
      JSUBOY = ISUBOY
      JSUBWT = ISUBWT
      RSMAX  = RZERO
      IW     = IWS
      DO 70 I=1,N
        IF (IWST.EQ.IONE) THEN
          WT        = RC(IW)
          A(JSUBWT) = RC(IW)
          IF (RC(IW).NE.RZERO) RC(IW) = RONE
          IW     = IW + IONE
          JSUBWT = JSUBWT + IONE
        END IF
        IF (WT.NE.RZERO) THEN
          X = FDIV (VARNCE,WT,IND)
          Z = FDIV (A(KSUBRS),FSQRT(X-A(JSUBSY)**2),IND)
          A(JSUBOY) = ABS (Z)
          RSMAX = AMAX1 (RSMAX,A(JSUBOY))
        ELSE
          A(JSUBOY) = RPIFY
        END IF
        JSUBSY = JSUBSY + IONE
        JSUBOY = JSUBOY + IONE
        KSUBRS = KSUBRS + IONE
  70  CONTINUE
C
      IF (RSMAX.GT.RONE) THEN
        RSMAX = AINT (RSMAX + RONE)
      ELSE
        RSMAX = RONE
      END IF
C
      JSUBRS = ISUBIU
      JSUBOY = ISUBOY
      DELTAY = RSMAX/FLOAT (LINES)
C
C     COMPUTE POINTS ON Y-AXIS FOR PLOTTING.
C
      DO 80 I = 1,N
        IF (A(JSUBOY).LE.RSMAX) THEN
          IZ = FDIV (A(JSUBOY),DELTAY,IND)
          IF (AMOD (A(JSUBOY),DELTAY).NE.RZERO) IZ = IZ + IONE
          A(JSUBRS) = IZ
        ELSE
          A(JSUBRS) = LINES
          A(JSUBOY)  = RZERO
        END IF
        JSUBRS = JSUBRS + IONE
        JSUBOY = JSUBOY + IONE
  80  CONTINUE
C
C     START PLOTTING FIRST GRAPH
C
      IF (NCRT.NE.IZERO) THEN
        WRITE (IPRINT,320) SCREEN
      ELSE
        CALL PAGE (IFOUR)
      ENDIF
C
 90   JSUBPV = ISUBPV
      IST = IONE
      IW = IWS
      IF (IWST.EQ.ITWO) THEN
        YMAX = A(JSUBPV)
        YMIN = A(JSUBPV)
        IST = ITWO
      END IF
      DO 100 I=1,N
        IF (IST.EQ.IONE) THEN
          IF (RC(IW).NE.RZERO) THEN
            YMAX = A(JSUBPV)
            YMIN = A(JSUBPV)
            IST = ITWO
          END IF
        ELSE
          IF (IWST.EQ.IONE .AND. RC(IW).EQ.RZERO) THEN
            ELSE
              YMIN = MIN (A(JSUBPV),YMIN)
              YMAX = MAX (A(JSUBPV),YMAX)
            END IF
        END IF
        IW = IW + IONE
        JSUBPV = JSUBPV + IONE
 100  CONTINUE
      YMM = FDIV (ABS (YMAX - YMIN),50.0,IND)
      IF (ABS (ABS(YMIN) - ABS(YMAX)).LT. .00005) GO TO 160
      IF ( LPRT .EQ. IONE ) THEN
         WRITE (IPRINT,250)
       ELSE
         WRITE (IPRINT,300)
       END IF
C
      YYPR = RSMAX
      WRITE (IPRINT,260) YYPR, (LA(39),I=1,44)
      LINE  = LINES - IONE
      NLINE = LINE
      DO 140 LNZ=1,NLINE
        YYPR = YYPR - RFIVE * DELTAY
C
C       THIS LOOP CONTROLS THE PRINTING OF 5 LINES OF THE GRAPH.
C
        DO 130 I=1,5
          DO 110 IJI=1,52
            IB(IJI) = LA(45)
 110      CONTINUE
          JSUBPV = ISUBPV
          JSUBRS = ISUBIU
          DO 120 IJI=1,N
            IF (INT(A(JSUBRS)+ .005).EQ.LINE) THEN
              IZ = FDIV (A(JSUBPV)-YMIN,YMM,IND)
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = LA(41)
            END IF
            JSUBRS = JSUBRS + IONE
            JSUBPV = JSUBPV + IONE
  120     CONTINUE
C
          IF (I.EQ.IFIVE) THEN
            WRITE (IPRINT,270) YYPR, (IB(IJI),IJI=1,51)
          ELSE
            WRITE (IPRINT,280) (IB(IJI),IJI=1,51)
          END IF
          LINE = LINE - IONE
          IF (LINE.EQ.IZERO) GO TO 150
 130    CONTINUE
 140  CONTINUE
C
 150  WRITE (IPRINT,260) YYPR, (LA(39),I=1,44)
C
C     FINSH GRAPH.
C
      YMMY = FDIV (YMAX-YMIN,RTWO,IND) + YMIN
      WRITE (IPRINT,290) YMIN, YMMY, YMAX
C
 160  GO TO (170,200), LPRT
C
C    FINISHED WITH FIRST PLOT.
C
 170  IF (L2.EQ.IONE .AND. M.LE.ITWO) GO TO 220
      IF (L2.EQ.ITHRE .AND. M.EQ.IONE) GO TO 220
      IF (NX.EQ.IONE) GO TO 220
      ISUBY  = IIRGS(1)
      JSUBOY = ISUBOY
      JSUBY  = ISUBY
      DO 180 I=1,N
        TEMP      = A(JSUBOY)
        A(JSUBOY) = RC(JSUBY)
        RC(JSUBY) = TEMP
        JSUBY  = JSUBY + IONE
        JSUBOY = JSUBOY + IONE
 180  CONTINUE
C
      IF (IWST.EQ.ITWO) THEN
        HOLDWT  = ARGS(2)
        ARGS(2) = RONE
      END IF
C
      CALL LSFIT (JS,ISTART,M,N,VARNCE,SU,SD,MMTXR,MMTXC,NIT,DIG,LSIND)
      LPRT = ITWO
      KSUBRS = ISUBRS
      JSUBPV = ISUBPV
      JSUBY  = IIRGS(1)
      IW     = IWS
      DO 190 I=1,N
        IF (IWST.EQ.IONE) THEN
          WT        = RC(IW)
          IW     = IW + IONE
        END IF
        IF (WT.NE.RZERO) THEN
          A(JSUBPV) = RC(JSUBY) - A(KSUBRS)
        ELSE
          A(JSUBPV) = RZERO
        END IF
        JSUBPV = JSUBPV + IONE
        JSUBY  = JSUBY  + IONE
        KSUBRS = KSUBRS + IONE
 190  CONTINUE
C
      IF (NCRT.NE.IZERO) THEN
        WRITE (IPRINT,*)  ' ... HIT ENTER KEY TO CONTINUE ...'
        READ (NUNIT(1),310,END=195)
 195    WRITE (IPRINT,320) SCREEN
      ELSE
        WRITE (IPRINT,240)
      ENDIF
C
      GO TO 90
C
C     FINISHED WITH 2ND GRAPH.  RESTORE WORKSHEET AND RETURN.
C
 200  ISUBY  = IIRGS(1)
      JSUBOY = ISUBOY
      JSUBY  = ISUBY
      JSUBWT = ISUBWT
      IW     = IWS
      DO 210 I=1,N
        RC(JSUBY) = A(JSUBOY)
        JSUBY  = JSUBY + IONE
        JSUBOY = JSUBOY + IONE
        IF (IWST.EQ.IONE) THEN
          RC(IW) = A(JSUBWT)
          IW     = IW +IONE
          JSUBWT = JSUBWT + IONE
        END IF
 210  CONTINUE
      IF (IWST.EQ.ITWO) THEN
        ARGS(2) = HOLDWT
      END IF
 220  L2 = LL2
      IF (L2.EQ.9 .AND. M.EQ.ITWO) THEN
        IWTS = IZERO
        IF (KIND(2).EQ.IZERO) IWTS = IONE
        ISUBW = IWS
        CALL PAGE (IFOUR)
        CALL CONFEL (ISUBB,251,IIRGS(4),N,VARNCE,RC(ISUBW),ARGS(2),
     1       IWTS,SU)
      END IF
C
C     CLEAR OUT ALL ARITHMETIC ERROR MESSAGES.
C
      DO 230 I=1,KMES
        MESS(I) = IZERO
 230  CONTINUE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 240  FORMAT (1H )
 250  FORMAT (7X, 53HABSOLUTE STANDARDIZED RESIDUALS VS PREDICTED RESPON
     1SE)
 260  FORMAT (1X,F5.2,1X,2(1H+,9A1),1H+,4A1,1HX,4A1,2(1H+,9A1),1H+)
 270  FORMAT (1X,F5.2,1H+,51A1,1H+)
 280  FORMAT (6X,1H-,51A1,1H-)
 290  FORMAT (1PE13.4,E26.4,10X,E10.4)
 300  FORMAT (3X,51HABSOLUTE STANDARDIZED RESIDUALS VS LINEAR PREDICTOR,
     135H OF ABSOLUTE STANDARDIZED RESIDUALS)
 310  FORMAT (1A1)
 320  FORMAT (1X,A4)
C
C     ==================================================================
C
      END
*VARRES
      SUBROUTINE VARRES (IWT,VARNCE,ISUBWT,ISUBSY,ISTORE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. VARRES V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE VARIANCE OF RESIDUALS.
C        VAR(RESIDUAL) = RESIDUAL VAR. / WEIGHT - VAR(PREDICTED VALUE).
C
C     IWT    = 0, IF COLUMN OF WEIGHTS IS NOT SPECIFIED.
C            = 1, IF COLUMN OF WEIGHTS IS     SPECIFIED.
C     VARNCE = RESIDUAL VARIANCE.
C     ISUBWT = ADRESS FOR WEIGHTS, IF WEIGHTS ARE USED.
C     ISUBSY = STARTING LOCATION IN A(.) OF VAR(PRED.VALUES).
C     ISTORE = STARTING LOCATION IN A(.) FOR STORING VAR(RESIDUALS).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             VARNCE
      REAL             TEMP
      REAL             FDIV
C
C     ==================================================================
C
      TEMP = VARNCE
      IF (IWT.EQ.IZERO .AND. ARGS(2).NE.RONE)
     1     TEMP = FDIV (ARGS(2),TEMP,IND)
      J = ISUBWT
      K = ISUBSY
      L = ISTORE
      DO 20 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 10
          TEMP = FDIV (VARNCE,RC(J),IND)
          J = J + IONE
  10    A(L) = TEMP - A(K)**2
        K = K + IONE
        L = L + IONE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*VECTOR
      SUBROUTINE VECTOR (B,J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. VECTOR V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     VECTORIZE REAL B INTO COLUMN STARTING AT POSITION J.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             B
C
C     ==================================================================
C
      IF (NRMAX.EQ.IZERO) RETURN
      K = J + NRMAX - IONE
      DO 10 I=J,K
        RC(I) = B
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*WEIPLT
      SUBROUTINE WEIPLT (X,Y,W,N,GAMMA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. WEIPLT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A WEIBULL
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = GAMMA).
C              THE PROTOTYPE WEIBULL DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE WEIBULL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE  WEIBULL DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT--A ONE-PAGE WEIBULL PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--DECEMBER  1972.
C     UPDATED         --MARCH     1975.
C     UPDATED         --SEPTEMBER 1975.
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-285
C                  ORIGINAL VERSION - NOVEMBER, 1975.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION M(10), MT(50)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             W(*), X(*), Y(*)
      REAL             GAMMA
      REAL             ATEMP(1), V(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER*1      M, MT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10) /
     1      ',',  ' ',  'S',  'L',  'O',  'P',  'E',  ' ',  '=',   ' ' /
C
C     ==================================================================
C
      AN = N
      IF (GAMMA.GT.RZERO) GO TO 10
      CALL ERROR (38)
      RETURN
C
C     ..................................................................
C
  10  CALL SORTPP (X,N,Y)
      CALL UNIMED (N,W)
      DO 20 I=1,N
        W(I) = (-FLOG (RONE-W(I))) ** FDIV (RONE,GAMMA,IND)
  20  CONTINUE
C
      IF (LWIDE.LT.NCW) GO TO 30
      V(1) = GAMMA
      CALL RFORMT (0,ISIGD,V,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,V(1),0,0,NW,ND,MT(1),IRF)
  30  IF (LWIDE.GE.108) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      IF (LWIDE.GE.NCW .AND. LWIDE.LT.108) WRITE (IPRINT,130)
     1     (LHEAD(I),I=1,12), N, (MT(J),J=1,NW)
      IF (LWIDE.LT.NCW) WRITE (IPRINT,140) (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      CALL SUMMAL (W,N,SUM2)
      IF (N.EQ.IONE) SUM2 = W(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = FDIV (SUM2,AN,IND)
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 40 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  40  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 50 I=1,N
        ATEMP(1) = (Y(I)-YBAR) * (W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  50  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 60 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
  60  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1) * WBAR
      CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 70 I=1,10
        MT(K) = M(I)
        K = K + IONE
  70  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+73.GT.LWIDE) GO TO 80
      WRITE (IPRINT,150) CC, (MT(J),J=1,K)
      GO TO 110
  80  CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 90 I=1,10
        MT(K) = M(I)
        K = K + IONE
  90  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+38.GT.LWIDE) GO TO 100
      WRITE (IPRINT,160) CC, (MT(J),J=1,K)
      GO TO 110
 100  IF (LWIDE.LT.37) GO TO 110
      WRITE (IPRINT,170) CC
 110  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 120  FORMAT (15X, 7HWEIBULL           ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 130  FORMAT ( 1X, 7HWEIBULL           ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 140  FORMAT ( 1X, 7HWEIBULL      ,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 150  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 160  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 170  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*WERRSS
      SUBROUTINE WERRSS (ISUBWT,ISUBY,ISUBH,N,SS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. WERRSS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO COMPUTE ERROR SUM OF SQUARES FOR LACK OF FIT TEST
C        USING WEIGHTS W(I).
C
C     SUBSCRIPT FOR PICKING UP Y AND W IS IN A(ISUBH).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             SS
      REAL             TEMP(1)
      REAL             SUMWTS, SUMWY, YBAR
      REAL             FDIV
C
C     ==================================================================
C
      J = ISUBH
      CALL SUMMAL (TEMP,IZERO,SUMWY)
      DO 10 I=1,N
        K = A(J) - RONE
        L = ISUBWT + K
        M = ISUBY  + K
        TEMP(1) = RC(L) * RC(M)
        CALL SUMMAL ( TEMP,-IONE,SUMWY)
        J = J + IONE
  10  CONTINUE
      CALL SUMMAL (TEMP(1),IONE,SUMWY)
      J = ISUBH
      CALL SUMMAL (TEMP,IZERO,SUMWTS)
      DO 20 I=1,N
        K = A(J) - RONE
        L = ISUBWT + K
        CALL SUMMAL (RC(L),-IONE,SUMWTS)
        J = J + IONE
  20  CONTINUE
      CALL SUMMAL (TEMP(1),IONE, SUMWTS)
      YBAR = FDIV (SUMWY,SUMWTS,IND)
C
      J = ISUBH
      CALL SUMMAL (TEMP,IZERO,SS)
      DO 30 I=1,N
        K = A(J) - RONE
        L = ISUBWT + K
        M = ISUBY  + K
        TEMP(1) = RC(L) * ( (RC(M)-YBAR)**2 )
        CALL SUMMAL (TEMP,-IONE,SS)
        J = J + IONE
  30  CONTINUE
      CALL SUMMAL (TEMP,IONE,SS)
      RETURN
C
C     ==================================================================
C
      END
*XPND
      SUBROUTINE XPND (T,K,Y,KND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   XPND V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE TAKES A ''STATEMENT'' REFERENCE AS STORED
C        AND EXPANDS IT INTO THE PROPER ARGUMENT WITH CHECKING.
C
C     K IS RETURNED 0 IF ARG IN STATEMENT IS ONE WORD LONG
C     K IS RETURNED 1 IF ARG IN STATEMENT IS TWO WORDS LONG.
C     K IS RETURNED -( ERROR NUMBER ) IF ERROR OCCURS.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             T(*)
      REAL             Y
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA  / 8208   /
C
      DATA SPCA / 8192.0 /
C
C     ==================================================================
C
      IT = -T(1)
      IF (IT.LT.16) GO TO 30
C
C     ''ROW, COL'' ENTRY
C
      IT = IT - ICA
      IF (IT.GT.IZERO .AND. IT.LE.NROW) GO TO 10
      K = -16
      RETURN
C
C     ..................................................................
C
10    IARGS(100) = ABS(T(2)) - SPCA
      KIND(100)  = IZERO
      CALL ADRESS (IHRD,J)
      IF (J.GT.IZERO) GO TO 20
      IF (J.LT.IZERO) K = -20
      IF (J.EQ.IZERO) K = -11
      RETURN
C
C     ..................................................................
C
20    J = J + IT
      KND = IZERO
      IF (T(2).LT.RZERO) KND = IONE
      Y = RC(J-1)
      K = IONE
      RETURN
C
C     ..................................................................
C
C     NRMAX, V, W, X, Y, Z,  REFERENCE.
C
30    IU = IDIV (IT,ITWO,IND)
      KND = IT - ITWO*IU
      K = IZERO
      IF (IU.LE.IONE) GO TO 40
      Y = VWXYZ(IU-2)
      RETURN
C
C     ..................................................................
C
40    Y = NRMAX
      RETURN
C
C     ==================================================================
C
      END
*XSTOP
      SUBROUTINE XSTOP
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.  XSTOP V 7.00  6/11/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS ROUTINE REWINDS THE SCRATCH UNIT AND PRINTS IT.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ITEMP(84)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER   LA*1
      CHARACTER   ITEMP*1
C
C     ..................................................................
C
C     EQUIVALENCE (ITEMP(1),A(1))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICB / 76 /
      DATA ICC / 80 /
      DATA ICD / 84 /
C
C     ==================================================================
C
      REWIND ISCRT
      IF (ISBFT.LT.IZERO) RETURN
      LLIST = IZERO
      LX = ICD
      IF (NDEMD.EQ.IONE .AND. LWIDE.LT.ICC) LX = ICB
      LXX = LX - IFOUR
      IF (NERROR.EQ.IZERO) LLIST = ITHRE
      NRR = NERROR
      NERROR = IZERO
      CALL PAGE (IZERO)
      NERROR = NRR
      LINMIN = IZERO
      WRITE (IPRINT,110)
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
  20  READ (ISCRT,120) ITEMP
      IF (ITEMP(1).EQ.LA(41)) GO TO 30
      IF (ITEMP(1).EQ.LA(36)) GO TO 70
      IF (ITEMP(1).EQ.LA(40)) GO TO 50
      IF (ITEMP(1).EQ.LA(44)) GO TO 40
      WRITE (IPRINT,130) (ITEMP(I),I=IFIVE,LX)
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
      GO TO 20
C
  30  CALL ERRPRT
      LINMIN = LINMIN + ITHRE
      GO TO 20
C
  40  LLIST = ITHRE
      IF (ITEMP(2).EQ.LA(1) .AND. NERROR.EQ.IZERO) LLIST = IZERO
      GO TO 20
C
  50  IF (ITEMP(2).EQ.LA(40)) GO TO 100
      WRITE (IPRINT,140) (ITEMP(I),I=ITWO,LXX)
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
      GO TO 20
C
  70  REWIND ISCRT
      IF (NERROR.GT.IONE) GO TO 80
      IF (NERROR.LT.IONE) GO TO 90
      WRITE (IPRINT,150)
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
      GO TO 90
C
  80  WRITE (IPRINT,160) NERROR
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
  90  LLIST = ITHRE
      WRITE (IPRINT,170)
      RETURN
C
C     ..................................................................
C
 100  WRITE (IPRINT,130) (ITEMP(I),I=ITHRE,LXX)
      LINMIN = LINMIN + IONE
      IF (LINMIN.GE.LENGTH) THEN
        CALL PAGE (0)
        LINMIN = IZERO
      ENDIF
      GO TO 20
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 110  FORMAT (//19X,43H LIST OF DATA, INSTRUCTIONS AND DIAGNOSTICS//)
 120  FORMAT (84A1)
 130  FORMAT ( 1X,84A1)
 140  FORMAT ( 1X,3A1,3X,80A1)
 150  FORMAT (///15X,20HONLY ONE FATAL ERROR)
 160  FORMAT (///15X,I4,7H ERRORS)
 170  FORMAT (/1X/11X,49H OMNITAB 80   PC VERSION 7.00    JULY  7, 1992  
     1  / 27X,17H DEVELOPED AT THE/
     2 71H NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, GAITHERSBURG,
     3 MD 20899/
     4 7X,' FOR INSTALLATION PROBLEMS, CONTACT ALAN HECKERT ',
     5 '301-975-2899.',
     6 ' FOR STATISTICAL PROBLEMS, CONTACT CARROL CROARKIN ',
     7 ' 301-975-2849')
C
C     ==================================================================
C
      END
*ZLCVAR   
      SUBROUTINE ZLCVAR (LH,IND,IA,LOC) 
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ZLCVAR V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     LOCATE VARIABLE IN INSTRUCTION DEFINED BY LABEL, ALABEL OR MLABEL.        
C         
C     INPUT ...     
C         
C     LH   IS LOCATION OF VARIABLE TO BE FOUND FOR LABEL.   
C         
C             IF THE COMMAND IS LABEL THEN,       
C                 IHEAD(6,I)=NO. OF CHAR/VARIABLE 
C                 IHEAD(1,I)=COLUMN NO. ASSIGNED TO VARIABLE.         
C         
C             IF COMMAND IS MLABEL OR ALABEL THEN,
C                 IHEAD(6,I)=N*100+ NO. OF CHAR/VARIABLE    
C                 IHEAD(1,I)=((C-1)*NROW+R)10000+M,         
C         
C                    WHERE M = NUMBER OF ROWS     
C                          N = NUMBER OF COLUMNS  
C                          R = STARTING ROW       
C                          C = STARTING COLUMN.   
C         
C     OUTPUT ...    
C         
C     IND  INDICATOR. IF IND = 0, VARIABLE WAS FOUND.       
C                            = 1, VARIABLE WAS NOT FOUND.   
C         
C         
C     IA    IS COLUMN NUMBER FOR LABEL. 
C           IA = ((C-1)*NROW+R)*100000+M(SIZE OF ROWS) FOR  
C              ALABEL OR MLABEL.        
C         
C     LOC = N(SIZE OF COLS)*100 + NUMBER OF CHARACTERS PER VARIABLE   
C         
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - AUGUST, 1969.       
C                   CURRENT VERSION -  APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IAA(100), IAC(4,12), ICF(1), LH(*), LHH(100)        
C         
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /PRHEAD/ IHEAD(6,50), NHEADS         
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
C         
      INCLUDE 'WRKSCR.H'
C         
      EQUIVALENCE (IAA(1),A(1)), (IAC(1,1),A(1))  
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C
      CHARACTER LHH*1
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
C     THE FOLLOWING CONSTANT IS (45*256+45)*256+45
C         
      DATA ICA / 2960685 /    
      DATA ICB /       6 /    
      DATA ICC /      12 /    
      DATA ICD /      44 /    
      DATA ICE /      46 /    
      DATA ICF(1) /   43 /    
C         
C     ==================================================================        
C         
      IND = IZERO   
      CALL LOCAT (ICF,IAA,IONE,LH,ICC,INDL,INDL1,IAA) 
      ISTOP = ICC   
      IF (INDL.NE.IZERO) ISTOP = IAA(1) - IONE    
      DO 30 I=1,ICC 
       IF (LH(I).EQ.ICE) GO TO 35 
        DO 20 II=1,4
          IAC(II,I) = ICA     
  20    CONTINUE    
        IF (I.GT.ISTOP) GO TO 30        
        JJ = I      
        CALL PACK (LH,IAC(1,I),LHH,JJ,IZERO)
  30  CONTINUE      
C         
  35  INDEX = IZERO 
      ISTP  = IZERO 
      DO 50 I=1,IHCNT         
        IF (IHEAD(ICB,I).GT.IZERO) GO TO 50       
        IAB = MOD (IABS(IHEAD(ICB,I)),IHRD)       
        IBB = IDIV (IAB,ITHRE,IND)      
        IF (MOD(IAB,ITHRE).NE.IZERO) IBB = IBB + IONE       
        DO 40 JA=1,IBB        
          IF (IAC(JA,IAB).NE.IHEAD(JA+1,I)) GO TO 50        
  40    CONTINUE    
        IF (LH(IAB+1).GE.ICE) GO TO 60  
        IF (LH(IAB+1).NE.ICD .AND. LH(IAB+1).NE.ICF(1)) GO TO 50      
        IF (IAB.LE.ISTP) GO TO 50       
        ISTP  = IAB 
        INDEX = I   
  50  CONTINUE      
C         
      IF (ISTP.EQ.IZERO) GO TO 70       
      IA  = IHEAD(1,INDEX)    
      LOC = IABS(IHEAD(ICB,INDEX))      
      RETURN        
C         
C     ..................................................................        
C         
  60  IND = -IONE   
      IA  = IHEAD(1,I)        
      LOC = IABS (IHEAD(ICB,I))         
      RETURN        
C         
C     ..................................................................        
C         
  70  IND = IONE    
      RETURN        
C         
C     ==================================================================        
C         
      END 
