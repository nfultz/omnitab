*SCC
      SUBROUTINE SCC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.    SCC V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT HANDLES INSTRUCTIONS OF THE FORM ...
C       XXXXXX OF COLS (C), (C), ..., (C) PUT IN COLS (C), (C), ..., (C)
C
C     FOR L2=11, INSTRUCTION IS - AVERAGE
C     FOR L2=12, INSTRUCTION IS - STDDEV
C     FOR L2=13, INSTRUCTION IS - MEDIAN
C     FOR L2=14, INSTRUCTION IS - RANGE
C     FOR L2=15, INSTRUCTION IS - PERCENTAGES
C     FOR L2=16, INSTRUCTION IS - PROPORTIONS
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
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
      REAL             AMAX, AMIN, AN, AVEX, CONST, SS, SUM, X, XCODE
      REAL             FDIV, FSQRT
C
      DOUBLE PRECISION SUMX, SQRTCT
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NRMAX.GT.IZERO) GO TO 10
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
  10  K = MOD (NARGS,ITWO)
      IF (K.EQ.IZERO .AND. NARGS.GE.ITWO) GO TO 20
        CALL ERROR (10)
        RETURN
C
C     ..................................................................
C
  20  CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      L  = IDIV (NARGS,ITWO,IND)
      AN = FLOAT (NRMAX)
      X  = RZERO
      DO 230 I=1,L
        M = IARGS(I)
        K = I + L
        N = IARGS(K)
        K = L2 - ITEN
        GO TO (30,50,90,150,30,30), K
C
C       AVERAGE.
C
  30    CALL SUMMAL (RC(M), IZERO,SUM)
        DO 40 J=1,NRMAX
          CALL SUMMAL (RC(M),-IONE,SUM)
          M = M + IONE
  40    CONTINUE
C
        CALL SUMMAL (RC(M), IONE,SUM)
        IF (L2.EQ.15) GO TO 170
        IF (L2.EQ.16) GO TO 180
        X = FDIV (SUM,AN,IND)
        GO TO 190
C
C       STANDARD DEVIATION.
C
  50    IF (NRMAX.GT.IONE) GO TO 60
        X = RZERO
        GO TO 190
C
  60    CALL CODEXX (RC(M),NRMAX,SUMX,AVEX,XCODE,SQRTCT,A(1),K)
        DO 70 J=1,NRMAX
          A(J) = A(J)**2
  70    CONTINUE
C
        CALL SUMMAL (A(1), IZERO,SUM)
        DO 80 J=1,NRMAX
          CALL SUMMAL (A(J),-IONE,SUM)
  80    CONTINUE
        CALL SUMMAL (A(1), IONE,SUM)
        SS = DBLE(SUM) - SQRTCT**2
        X = FSQRT ( FDIV (SS,AN-RONE,IND) )
        GO TO 190
C
C       RANGE.
C
  90    IF (NRMAX.GT.IONE) GO TO 100
        X = RZERO
        GO TO 190
C
 100    AMIN = RC(M)
        AMAX = RC(M)
        DO 140 J=2,NRMAX
          M = M + IONE
          IF (RC(M)-AMIN) 110,120,120
 110      AMIN = RC(M)
          GO TO 140
 120      IF (RC(M)-AMAX) 140,140,130
 130      AMAX = RC(M)
 140    CONTINUE
        X = AMAX - AMIN
        GO TO 190
C
C       MEDIAN.
C
 150    DO 160 J=1,NRMAX
          A(J) = RC(M)
          M = M + IONE
 160    CONTINUE
        CALL SORT (A(1),RC(N),NRMAX,IONE)
        K1 = MOD (NRMAX,ITWO)
        K2 = IDIV (NRMAX,ITWO,IND)
        IF (K1.EQ.IZERO) X = FDIV (A(K2)+A(K2+1),RTWO,IND)
        IF (K1.EQ.IONE) X = A(K2+1)
        GO TO 190
C
C       PERCENTAGES.
C
 170    CONST = FLOAT (IHRD)
        GO TO 210
C
C       PROPORTIONS.
C
 180    CONST = RONE
        GO TO 210
C
C
 190    DO 200 J=1,NRMAX
          RC(N) = X
          N = N + IONE
 200    CONTINUE
        GO TO 230
C
C
 210    M = M - NRMAX
        DO 220 J=1,NRMAX
          RC(N) = CONST * FDIV (RC(M),SUM,IND)
          M = M + IONE
          N = N + IONE
 220    CONTINUE
 230  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SCNARG
      SUBROUTINE SCNARG (LNAME,JA,MATSWT,IMAT,INDX)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCNARG V 7.00   4/21/92 **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE SCANS THE INPUT CARD FOR ARGUMENTS.
C     IF ILABEL IS NOT ZERO, THEN CARD IS ALSO SCANNED FOR VARIABLES.
C
C     INPUT.
C
C       LNAME   NUMBER OF WORDS IN COMMAND.
C
C     OUTPUT.
C
C       JA      AMOUNT OF INFORMATION STORED IN ARGTAB.
C       INDX    = 0, NO ERRORS ENCOUNTERD.
C               = 1, ERRORS FOUND.
C
C               REVISED BY -
C                      SALLY T. PEAVY,
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
      DIMENSION ICARD(100)
C
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             RELINC, T
      REAL             SPCA
C
      CHARACTER        KA*1
      CHARACTER        LA*1
      CHARACTER        NEWCRD*1
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA RELINC / 8192.0 /
C
      DATA SPCA / 8208.0 /
C
      DATA ICA /     44 /
      DATA ICB /     46 /
      DATA ICC /     35 /
      DATA ICD / 100000 /
      DATA ICE /     40 /
      DATA ICF /    100 /
C
C      =================================================================
C
      JA     = JA + IONE
      ISWT   = IONE
      KRDPST = KRDPOS
      MS     = KRDPOS
      MZA    = KRDPOS
      T      = RZERO
      KLABEL = IZERO
      DO 240 IPOS=KRDPST,LKARD
        K = KARD(KRDPOS)
        IF (K.NE.ICA .OR. ILABEL.LE.IZERO) GO TO 50
C
C       ILABEL IS NOT ZERO. CHECK DISCRIPTORS FOR
C         VARIABLE ARGUMENTS.
C
C       IF KLABEL = 0, DEFINE NUMERIC VALUE OF NEWCRD INTO ICARD
C
        IF (KLABEL .EQ. IZERO) THEN 
        KLABEL = IONE
        DO 6 KL =1, KRDEND
          KA = NEWCRD(KL)
          DO 4 KB = 1,74
            IF (KA .EQ. LA(KB)) ICARD(KL+2) = KB - IONE
   4      CONTINUE
   6    CONTINUE
        ENDIF 
        KARG = IZERO
C
        CALL BLANK
C
  10    IF (LNAME.LE.IZERO .OR. LNAME.GT.IFOUR) GO TO 40
        IF (MODE.EQ.ITWO .AND. NAME(1).EQ.IZERO) GO TO 40
        IF (L1.EQ.16 .AND. L2.GT.ITWO. AND. L2.LE.IFIVE) GO TO 40
        MZZ = KRDPOS
C
        CALL NONBLA (MZZ,K)
C
        MZA = KRDPOS
        IF (K.EQ.ICB) GO TO 150
        IF (K.GT.ICC) GO TO 40
C
        CALL ZLCVAR (ICARD(KRDPOS),IND,IA,LOC)
C
        IF (IND.GT.IZERO) GO TO 40
        IZSTP = IDIV (IA,ICD,IND)
        IA = MOD (IA,ICD)
        IF (IZSTP.GT.IZERO) GO TO 20
        ARG = IA
        ISWT = IFIVE
        IF (KARG.GT.IZERO) GO TO 30
        IF (KARG.LE.IZERO) GO TO 200
  20    IF (KARG.GT.IZERO) GO TO 90
        ARG = MOD (IZSTP,NROW)
        ISWT = ITWO
        GO TO 200
  30    IAB = MOD (LOC,ICF)
        KRDPOS = MZA + IAB
        ISWT = IONE
        IAB = IZERO
        IF (KARG.EQ.IZERO) GO TO 230
        MZZ = KRDPOS
C
        CALL NONBLA (MZZ,K)
C
        IF (K.NE.ICE) GO TO 90
        ARG2 = ARG
        ARG = T
        KARG = KARG - ITHRE
        KRDPOS = KRDPOS + IONE
        K = KARD(KRDPOS)
        IF (KARG.NE.IFIVE) GO TO 110
        IF (K.NE.ICE) GO TO 90
        KRDPOS = KRDPOS + IONE
        K = KARD(KRDPOS)
        GO TO 110
  40    IF (KARG.NE.IZERO) GO TO 90
  50    IF (K.GE.ITEN .AND. K.EQ.ICE) GO TO 70
        IF (K.GE.ITEN .AND. K.GT.ICE) GO TO 150
        IF (K.GE.ITEN .AND. K.LT.ICE) GO TO 190
C
C     NUMBER FOUND, CONVERT ARGUMENT. IF KARG RETURNED = 0, NUMBER IS
C     INTEGER,IF KARG = 1, NUMBER IS FLOATING POINT, IF KARG = -1, ERROR
C
        CALL AARGS (KARD)
C
        MATSWT = IZERO
        IF (KARG.EQ.IZERO) GO TO 200
        IF (KARG.LT.IZERO) GO TO 250
  60    ARGTAB(JA) = RZERO
        JA = JA + IONE
        GO TO 210
C
C     ASTERISK FOUND, CONVERT
C
C     IF BRACKETED BY SINGLE ASTERISKS, QUANTITY IS TO BE USED AS A
C     FLOATING POINT ARGUMENT.IF BRACKETED BY DOUBLE ASTERISKS, QUANTITY
C     IS TO BE TRUNCATED AND USED AS AN INTEGER ARGUMENT.
C
C     IF COMMAND IS EVALUATE BY-PASS CHECK ON ASTERISKS.
C
  70    KARG = IONE
        KRDPOS = KRDPOS + IONE
        IF (KARD(KRDPOS).NE.ICE) GO TO 80
        KARG = IZERO
        KRDPOS = KRDPOS + IONE
  80    MS = KRDPOS
C
        CALL ASTER
C
C     THE TERMINAL ASTERISK(S) HAVE BEEN CHECKED TO BE THE SAME AS THE
C     INTITAL SET (IF NO ERROR) AND M IS POINTING AT THE FIRST CHARACTER
C     AFTER THE LAST ASTERISK.
C
C     KARG RETURNED AS 1 = ERROR FOUND
C                      2 = FLOATING POINT CONSTANT, I.E.  *PI*
C                      3 = INTEGER NAMED VARIABLE,  I.E. **NRMAX**
C                      4 = FL. PT. NAMED VARIABLE,  I.E.  *NRMAX*
C                      5 = INTEGER ROW-COLUMN,      I.E. **3,40**
C                      6 = FL. PT. ROW-COLUMN,      I.E.  *1,2*
C                      7 = STRING OF ASTERISKS      I.E. ***
C                      8 = INTEGER ROW-COLUMN,      I.E. **3,VARIABLE**
C                      9 = FL. PT. ROW-COLUMN,      I.E.  *1,VARIABLE*
C
C     A STRING OF THREE OR MORE ASTERISKS IMPLIES -THRU-
C        EXAMPLE..
C           ERASE 1 2 3 4 12 13 14 15 16 20
C              IS EQUIVALENT TO
C           ERASE 1 *** 4, 12 *** 16, 20
C
C           PRINT 1 20 19 18 17 16 15 14
C              IS EQUIVALENT TO
C           PRINT 1, 20 *** 14
C
        GO TO (90,60,100,100,110,110,120,140,140), KARG
C
  90    KRDPOS = MS
        KARG = IZERO
        GO TO 240
C
 100    ARGTAB(JA) = -RTWO*ARG - FLOAT (KARG-ITHRE)
        GO TO 220
C
 110    ARGTAB(JA) = -(ARG+SPCA)
        ARG2 = ARG2 + RELINC
        IF (KARG.EQ.6) ARG2 = -ARG2
        JA = JA + IONE
        ARGTAB(JA) = ARG2
        GO TO 220
C
 120    IF (JA.GT.IZERO) GO TO 130
        CALL ERROR (211)
        GO TO 240
C
 130    ARGTAB(JA) = -RONE
        GO TO 230
C
C                            ARGTAB SETUP
C
C     IF ENTRY .GT. 0, IT IS AN INTEGER CONSTANT (I.E., COLUMN NUMBER)
C     TO WHICH A BIAS OF 8192 HAS BEEN ADDED.  THIS IS TO SAY THAT A
C     NEGATIVE INTEGER ARGUMENT MAY NOT BE EXPLICITLY GIVEN OR MODIFIED
C     TO BE LESS THAT -8191.
C
C     IF ENTRY .EQ.0, THE NEXT ENTRY IS A FLOATING POINT CONSTANT.
C
C     IF ENTRY .LT. 0, ARGUMENT IS A VARIABLE. SET SIGN POSITIVE AND..
C
C         IF ENTRY .LT. 16, IT IS A NAMED VARIABLE REFERENCE NUMBER
C
C             2,3  NRMAX      6,7    V       10,11    X
C                             8,9    W       12,13    Y
C                                            14,15    Z
C
C             V,W,X,Y,Z, ARE FOR PROGRAMMING CONVENIENCE ONLY AND DO NOT
C             AFFECT THE OPERATION OF OMNITAB.
C
C             IF ENTRY IS EVEN, CURRENT VALUE TO BE TRUNCATED AND USED
C             AS AN INTEGER ARGUMENT.
C             IF ENTRY IS ODD. THE CURRENT VALUE IS TO BE USED AS A
C             FLOATING POINT ARGUMENT.
C
C         IF ENTRY .GT. 16, IT IS A WORKSHEET REFERENCE (ROW,COLUMN) TO
C                  WHICH A BIAS OF 8208.0 HAS BEEN ADDED.
C             ENTRY - 8208 = ROW NUMBER
C             ABS(NEXT ENTRY) = COLUMN NUMBER TO WHICH A BIAS OF 8192.
C                  HAS BEEN ADDED.
C
C             IF NEXT ENTRY IS NEGATIVE, WORKSHEET CONTENTS ARE TO BE
C             USED AS A FLOATING POINT CONSTANT.  IF +, WORKSHEET VALUE
C             TO BE TRUNCATED AND USED AS AN INTEGER ARGUMENT.
C
 140    T = ARG
        GO TO 10
C
 150    IF (K.NE.ICB) GO TO 190
        INDX  =  IZERO
        RETURN
C
C     ..................................................................
C
 160    JA = JA + IONE
        ARG = IDIV (IZSTP,NROW,IND) + IONE
        ISWT = ITHRE
        GO TO 200
C
 170    JA = JA + IONE
        ARG = IA
        ISWT = IFOUR
        GO TO 200
C
 180    JA = JA + IONE
        ARG = IDIV (LOC,IHRD,IND)
        ISWT = IFIVE
        MATSWT = IONE
        IMAT = IMAT + IONE
        GO TO 200
C
  190   KRDPOS  =  KRDPOS + IONE
        GO TO 240
C
C     ARGUMENT IS AN INTEGER. ADD A BIAS OF 8192 THEN CHECK THAT IT IS
C     .GT. 0
C
 200    ARG = ARG + RELINC
        IF (ARG.GT.RZERO) GO TO 210
        CALL ERROR (18)
        GO TO 250
C
 210    ARGTAB(JA) = ARG
 220    NARGS = NARGS + IONE
        GO TO (230,160,170,180,30), ISWT
 230    JA  =  JA + IONE
 240  CONTINUE
C
 250  INDX  =  IONE
      RETURN
C
C      =================================================================
C
      END
*SCREEN
      SUBROUTINE SCREEN (RR,KX,NR,NDEF,IBIT,MBST,INTCPT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCREEN V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     **************************************************************** *
C                                                                      *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C                 CALL SCREEN(RR,KX,NR,NDEF,IBIT,MBST)                 *
C                                                                      *
C     RR   = UPPER TRIANGULAR PORTION OF (KX+1)*(KX+1) CORRELATION OR  *
C            PRODUCT MATRIX. VARIABLE KX+1 IS THE DEPENDENT VARIABLE.  *
C     KX   = NUMBER OF INDEPENDENT VARIABLES (3.LE.KX.LE.28)           *
C     NR   = DIMENSION OF RR (NR.GT.KX)                                *
C     NDEF = DEGREES OF FREEDOM FOR RR (NDEF.GT.KX)                    *
C     IBIT = SELECTION CRITERION CODE (1=R**2,2=ADJUSTED R**2,3=CP)    *
C     MBST = NUMBER OF BEST REGRESSIONS DESIRED (1.LE.MBST.LE.10)      *
C                                                                      *
C       MBST BEST REGRESSIONS FOR EACH SIZE SUBSET WHEN IBIT.EQ.1      *
C             MBST BEST REGRESSIONS IN TOTAL WHEN IBIT.GT.1            *
C                                                                      *
C     **************************************************************** *
C
C     ARRAY STORAGE REQUIRED FOR K=KX INDPENDENT VARIABLES AND M = K+1.
C         2*NL FOR XI AND XN, WHERE NL = M(M+1)(M+2)/6
C        4M**2 FOR ILI, ILM, MD AND NC
C      2*(11M) FOR CL AND RM
C          12M FOR CI, CN, CO, ID, IPI, IPN, NI, NN, TOLL, YI, YN AND ZC
C
C     TOTAL STORAGE EQUALS 2M(M+1)(M+2)/6 + 4M**2 +22M + 12M
C                   = (M**3 + 15*M**2 + 104*M)/3
C
C              ***   ARRAY STORAGE EQUIVALENCE TO A(.)  ***
C
C                 ARRAY             SIZE                  START
C
C                   XI               NL                       1
C                   XN               NL                    NL+1
C                 .............................................
C                  ILI             M**2           2*NL+       1
C                  ILN             M**2           2*NL+  M**2+1
C                   MD             M**2           2*NL+2*M**2+1
C                   NC             M**2           2*NL+3*M**2+1
C                 .............................................
C                   CL             11*M      2*NL+4*M**2+     1
C                   RM             11*M      2*NL+4*M**2+11*M+1
C                 .............................................
C                   CI                M      2*NL+4*M**2+22*M+1
C                   CN                M      2*NL+4*M**2+23*M+1
C                   CO                M      2*NL+4*M**2+24*M+1
C                   ID                M      2*NL+4*M**2+25*M+1
C                  IPI                M      2*NL+4*M**2+26*M+1
C                  IPN                M      2*NL+4*M**2+27*M+1
C                   NI                M      2*NL+4*M**2+28*M+1
C                   NN                M      2*NL+4*M**2+29*M+1
C                 TOLL                M      2*NL+4*M**2+30*M+1
C                   YI                M      2*NL+4*M**2+31*M+1
C                   YN                M      2*NL+4*M**2+32*M+1
C                   ZC                M      2*NL+4*M**2+33*M+1
C                 .............................................
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-921-3651
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION     ID(29),    IPI(29),   IPN(29),    NI(29),    NN(29)
      DIMENSION ILI(845), ILN(845), MD(845), NC(845)
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             RR(29,29)
      REAL             BOUND, CAB, RS, R2
      REAL             SIG, SS, TEMP, TOL, TWO
      REAL             FDIV
      REAL             SPCA, SPCB
C
C     ..................................................................
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA KO, NV / 10, 11 /
C
      DATA SPCA /   100.0 /
      DATA SPCB / 10000.0 /
C
C     ==================================================================
C
C     10=KO=NV-1     NL=(KX+1)*(KX+2)*(KX+3)/6      ND-1=NR-1
C                          NX=(KX+1)*(KX+2)/2
C
C                                 SET UP SIZE OF KZ, ND, NL AND NX.
C
      KZ = KX + IONE
      ND = KZ
      NL = IDIV (ND * (ND + IONE) * (ND + ITWO),6,IND)
      NX = IDIV (ND * (ND + IONE),ITWO,IND)
C
C                                 TEST INPUT.
C
      KZSIZE = ITWO * NL + IFOUR * ND ** 2 + 34 * ND
      IF (KZSIZE.GT.NS) CALL ERROR (23)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      IF (KX.GE.ITHRE .AND. KX.LT.ND .AND. NDEF.GT.KX .AND.
     1     MBST.GT.IZERO .AND. MBST.LE.KO .AND. KO.LE.NV .AND. NR.GT.KX
     2     .AND. IBIT.GE.IONE .AND. IBIT.LE.ITHRE) GO TO 10
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  10  SS = FDIV (RR(KZ,KZ),SPCA,IND)
      IF (IBIT.EQ.ITWO) SS = FDIV (SS,FLOAT(NDEF),IND)
      IF (SS.GT.RZERO) GO TO 30
  20  CALL ERROR (22)
      RETURN
C
C     ..................................................................
C
C                                 INITIALIZE.
C
  30  LSUBXI = IONE
      LSUBXC = IONE
      LSUBXN = NL + IONE
      LSUBLI = ITWO * NL + IONE
      LSUBLN = LSUBLI + KZ ** 2
      LSUBMD = LSUBLN + KZ ** 2
      LSUBNC = LSUBMD + KZ ** 2
      LSUBCL = LSUBNC + KZ ** 2
      LSUBRM = LSUBCL + 11 * KZ
      LSUBCI = LSUBRM + 11 * KZ
      LSUBCN = LSUBCI + KZ
      LSUBCO = LSUBCN + KZ
      LSUBID = LSUBCO + KZ
      LSUBPI = LSUBID + KZ
      LSUBPN = LSUBPI + KZ
      LSUBNI = LSUBPN + KZ
      LSUBNN = LSUBNI + KZ
      LSUBTL = LSUBNN + KZ
      LSUBYI = LSUBTL + KZ
      LSUBYN = LSUBYI + KZ
      LSUBZC = LSUBYN + KZ
      A(LSUBCN)  = RZERO
      A(LSUBCI)  = RZERO
      TOL    = FDIV (RER,SPCB,IND)
      TWO    = RTWO * RR(KZ,KZ) * FLOAT(NDEF)
      LOW    = KO - MBST + IONE
      LISUBL = IONE
      LNSUBL = IONE
      MDSUBL = IONE
      NCSUBL = IONE
      IDSUBL = IONE
      NPSUBL = IONE
      IPSUBL = IONE
      NISUBL = IONE
      NNSUBL = IONE
      ISUBLI = LISUBL
      ISUBNC = NCSUBL
      ISUBCL = LSUBCL
      ISUBRM = LSUBRM
      ISUBCO = LSUBCO
      KSUBRM = LSUBRM + KO
      ISUBID = IDSUBL
      ISUBPN = NPSUBL
      ISUBTL = LSUBTL
      NTLINE = 60
      IF (NCRT.NE.IZERO) NTLINE = LENGTH + ITHRE
      DO 50 L=1,KZ
        ID(ISUBID)  = IDIV ((KZ-IONE)*KZ*(KZ+IONE)-(KZ-L)*(KZ-L+IONE)*
     1                (KZ-L+ITWO),6,IND)
        IPN(ISUBPN)  = IONE
        ILI(ISUBLI)  = L
        A(KSUBRM)   = -TWO
        KSUBRM       = KSUBRM + 11
        A(ISUBCO)   = DTWO**(KX-L)
        NC(ISUBNC)   = L
        A(ISUBTL) = TOL * RR(L,L)
        IF (A(ISUBTL).LE.RZERO) GO TO 20
        JSUBCL = ISUBCL
        JSUBRM = ISUBRM
        DO 40 M=1,KO
          A(JSUBCL) = RZERO
          A(JSUBRM) = TWO
          JSUBCL     = JSUBCL + IONE
          JSUBRM     = JSUBRM + IONE
  40    CONTINUE
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
        ISUBCO = ISUBCO + IONE
        ISUBLI = ISUBLI + KZ
        ISUBNC = ISUBNC + KZ
        ISUBID = ISUBID + IONE
        ISUBPN = ISUBPN + IONE
        ISUBTL = ISUBTL + IONE
  50  CONTINUE
C
C                           STORE MATRICES AS VECTORS.
C
      LS     = IZERO
      ISUBXC = LSUBXC - IONE
      ISUBXN = LSUBXN
      ISUBMD = MDSUBL
      MSUBMD = MDSUBL - IONE
      DO 70 L=1,KZ
        KSUBMD = ISUBMD
        JSUBMD = MSUBMD + KZ * (L - IONE) + L
        DO 60 M=L,KZ
          LS         = LS + IONE
          ISUBXC     = ISUBXC + IONE
          MD(KSUBMD) = LS
          MD(JSUBMD) = LS
          A(ISUBXC) = RR(L,M)
          A(ISUBXN) = A(ISUBXC)
          RR(M,L)    = RR(L,M)
          ISUBXN     = ISUBXN + IONE
          KSUBMD     = KSUBMD + KZ
          JSUBMD     = JSUBMD + IONE
  60    CONTINUE
        ISUBMD = ISUBMD + IONE + KZ
  70  CONTINUE
C
C                             INVERT MATRIX STEPWISE.
C
      ISUBMD = MDSUBL + KZ ** 2 - IONE
      ISUB2  = MD(ISUBMD) + LSUBXC - IONE
      NSUBLI = LISUBL
      NSUBLN = LNSUBL
      NSUBMD = MDSUBL + KZ * (KZ - IONE) - IONE
      ISUBRM = LSUBRM - IONE + KO
      MSUBRM = LSUBRM
      ISUBCO = LSUBCO - IONE
      DO 90 N=1,KX
        J      = IZERO
        N1     = N
        ISUBLI = NSUBLI
        DO 80 LA=N,KX
          L      = ILI(ISUBLI)
          ISUBLI = ISUBLI + KZ
          ISUBMD = MDSUBL + KZ * (L -IONE) - IONE
          MSUBMD = NSUBMD + L
          ISUBMD = ISUBMD + L
          ISUBTL = LSUBTL + L - IONE
          ISUB1  = MD(ISUBMD) + LSUBXC - IONE
          IF (A(ISUB1).LT.A(ISUBTL)) GO TO 80
          ISUB3 = MD(MSUBMD) + LSUBXC - IONE
          RS = A(ISUB2) - FDIV (A(ISUB3)*A(ISUB3),A(ISUB1),IND)
          IF (RS.LT.A(ISUBRM)) J = LA
          MSUBCO = ISUBCO + L
          IF (RS.LT.A(MSUBRM)) CALL CPSTRE (RS,A(LSUBCI)+A(MSUBCO),
     1                                KO,A(LSUBCL),A(LSUBRM),N1,NV,ND)
  80    CONTINUE
        IF (J.EQ.IZERO) GO TO 100
        JSUBLI      = LISUBL + KZ * (J -IONE)
        M           = ILI(JSUBLI)
        ILI(JSUBLI) = ILI(NSUBLI)
        ILI(NSUBLI) = M
        ILN(NSUBLN) = M
        MSUBCO      = ISUBCO + M
        A(LSUBCI)  = A(LSUBCI) + A(MSUBCO)
        NSUBLI      = NSUBLI + KZ
        NSUBLN      = NSUBLN + KZ
        ISUBRM      = ISUBRM + 11
        MSUBRM      = MSUBRM + 11
        CALL PIVOT (A(LSUBXC),KZ,M,MD(MDSUBL),ND,NX)
  90  CONTINUE
C
      N      = KZ
 100  K      = N - IONE
      KP     = KZ * K + LISUBL
      KXSUBL = KZ * (KX - IONE) + LISUBL
      IF (K.NE.KX) WRITE (IPRINT,330) (ILI(I),I=KP,KXSUBL,KZ)
      IF (K.LT.ITHRE) RETURN
      KM = K - IONE
C
C     INTCPT - IONE = ADJUSTMENT FOR USING WITH NO CONSTANT TERM.
C
      SIG    = FDIV (RTWO*A(ISUBXC),FLOAT(NDEF-K+IONE-INTCPT),IND)
      A(LSUBYI)  = A(ISUBXC)
      A(LSUBYN)  = RR(KZ,KZ)
C
      NI(NISUBL) = K
      NN(NNSUBL) = K
      ISUBCL     = LSUBCL - IONE
      ISUBRM     = LSUBRM
      KSUBRM     = LSUBRM + 11 * (KZ - IONE)
      IF (IBIT.EQ.IONE) GO TO 130
      DO 120 M=1,K
        MSUBCL = ISUBCL
        MSUBRM = ISUBRM
        DO 110 L=1,KO
          IF (IBIT.EQ.ITWO)  RS = FDIV (A(MSUBRM),FLOAT(NDEF-M),IND)
          IF (IBIT.EQ.ITHRE) RS = A(MSUBRM) + SIG * FLOAT (M)
          MSUBCL = MSUBCL + IONE
          MSUBRM = MSUBRM + IONE
          IF (RS.GE.A(KSUBRM)) GO TO 110
          TEMP   = A(MSUBCL)
          CALL CPSTRE (RS,TEMP,KO,A(LSUBCL),A(LSUBRM),KZ,NV,ND)
 110    CONTINUE
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
 120  CONTINUE
C
 130  NREG =  IZERO
      NCAL =  ITWO
      MN   =  ITWO
      MV   = -IONE
C
C                                 STAGE  LOOP.
C
 140  JSUBRM = KSUBRM
      IF (MN.EQ.IONE) GO TO 240
      ISUBPN      = NPSUBL + MN - IONE
      IP          = IPN(ISUBPN)
      IPN(ISUBPN) = IP + IONE
      MV          = MV - IPN(ISUBPN+1) + IP + ITWO
      ISUBPI      = IPSUBL + MV - IONE
      IPI(ISUBPI) = IP
      MN          = MN - IONE
      ISUBPN      = ISUBPN - IONE
      IN          = IPN(ISUBPN)
      JC          = MV
      ISUBYI      = LSUBYI + IP - IONE
      BOUND       = A(ISUBYI)
      A(ISUBYI)  = TWO
C
C                              FIND LEAP FROM BOUNDS.
C
      ISUBRM = LSUBRM + LOW - IONE
      KSUBRM = LSUBRM + 11 * (KZ - IONE) + LOW - IONE
      DO 150 LB=IP,KM
        MT     = MN + KM - LB
        MSUBRM = ISUBRM + 11 * (MT - IONE)
        IF (IBIT.EQ.IONE .AND. A(MSUBRM).GT.BOUND) GO TO 160
        IF (IBIT.EQ.ITWO .AND. A(KSUBRM).GT.FDIV(BOUND,FLOAT(NDEF-MT),
     1     IND)) GO TO 160
        IF (IBIT.EQ.ITHRE .AND. A(KSUBRM).GT.BOUND+SIG*FLOAT(MT))
     1           GO TO 160
 150  CONTINUE
      GO TO 140
C
 160  LC = KM + IP - LB
      NREG = NREG + ITWO * (LC-IP+IONE)
      IF (IP.EQ.IONE) LC = K
C
C                         REGRESSIONS FROM INVERSE MATRIX.
C
      ISUBNI = NISUBL + IP
      ISUBNN = NNSUBL + IP
      KSUBLI = LISUBL + IP - IONE
      KSUBLN = LNSUBL + IN - IONE
      KSUBNN = NNSUBL + IN - IONE
      DO 200 LB=IP,LC
        LBB = LB
        CALL BACK (NC(NCSUBL),LBB,LI,IPI(IPSUBL),MV,RS,BOUND,ILI(LISUBL)
     1            ,JC,ID(IDSUBL),A(LSUBXI),MD(MDSUBL),
     2             IONE,NI(NISUBL),ND,KZ,NL,NCAL)
C
C                               RE-ORDER VARIABLES.
C
        M      = LB
        MSUBLN = KSUBLN + KZ * (M - IONE)
        MSUBLI = KSUBLI + KZ * (M - IONE)
        ISUBYI = LSUBYI + M - IONE
        IF (LB.GT.NN(KSUBNN)) GO TO 190
        LN = ILN(MSUBLN)
 170    IF (RS.LE.A(ISUBYI)) GO TO 180
        A(ISUBYI+1) = A(ISUBYI)
        NSUBLI       = MSUBLI - KZ
        NSUBLN       = MSUBLN - KZ
        ILI(MSUBLI)  = ILI(NSUBLI)
        ILN(MSUBLN)  = ILN(NSUBLN)
        M            = M - IONE
        MSUBLI       = MSUBLI - KZ
        MSUBLN       = MSUBLN - KZ
        ISUBYI       = ISUBYI - IONE
        GO TO 170
 180    ILI(MSUBLI)  = LI
        ILN(MSUBLN)  = LN
 190    A(ISUBYI+1) = RS
        NI(ISUBNI)   = LB
        NN(ISUBNN)   = LB
        ISUBNI       = ISUBNI + IONE
        ISUBNN       = ISUBNN + IONE
 200  CONTINUE
      IF (LC.EQ.K) LC = KM
      MI = K - MV
      JC = MN
C
C                         REGRESSIONS FROM PRODUCT MATRIX.
C
      ISUBRM = LSUBRM + 11 * (MI - IONE)
      KSUBRM = LSUBRM + 11 * (KZ - IONE)
      ISUBCI = LSUBCI + IP - IONE
      ISUBYI = LSUBYI + IP - IONE
      ISUBYN = LSUBYN + IP - IONE
      ISUBCO = LSUBCO - IONE
      DO 230 LB=IP,LC
        LBB        = LB
        ISUBCN     = LSUBCN + IN - IONE
        ISUBNC     = NCSUBL + IN - IONE
        KSUBYN     = LSUBYN + IN - IONE
        ISUBYI     = ISUBYI + IONE
        ISUBYN     = ISUBYN + IONE
        IS         = LB + IONE
        MSUBCN     = LSUBCN + LB
        A(MSUBCN) = A(KSUBYN)
        CALL BACK (NC(NCSUBL),LBB,L,IPN(NPSUBL),MN,A(ISUBYN),A(MSUBCN)
     1            ,ILN(LNSUBL),JC,ID(IDSUBL),A(LSUBXN),MD(MDSUBL),
     2             IZERO,NN(NNSUBL),ND,KZ,NL,NCAL)
        MSUBNC     = ISUBNC + KZ * (L - IONE)
        ISUB4      = NC(MSUBNC)
        MSUBCI     = LSUBCI + LB
        MSUBCO     = ISUBCO + ISUB4
        A(MSUBCI) = A(ISUBCI) - A(MSUBCO)
        A(MSUBCN) = A(ISUBCN) + A(MSUBCO)
        IF (A(ISUBYI).GE.A(ISUBRM)) GO TO 210
        CALL CPSTRE (A(ISUBYI),A(MSUBCI),KO,A(LSUBCL),A(LSUBRM),MI,
     1               NV,ND)
        IF (IBIT.EQ.IONE) GO TO 210
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYI),FLOAT(NDEF-MI),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYI) + FLOAT(MI) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCI),KO,A(LSUBCL),
     1      A(LSUBRM),KZ,NV,ND)
 210    MSUBRM = LSUBRM + 11 * (MN - IONE)
        IF (A(ISUBYN).GE.A(MSUBRM)) GO TO 220
        CALL CPSTRE (A(ISUBYN),A(MSUBCN),KO,A(LSUBCL),A(LSUBRM),MN,
     1               NV,ND)
        IF (IBIT.EQ.IONE) GO TO 220
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYN),FLOAT(NDEF-MN),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYN) + FLOAT(MN) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCN),KO,A(LSUBCL),
     1      A(LSUBRM),KZ,NV,ND)
 220    MN            = MN + IONE
        ISUBPN        = NPSUBL + MN - IONE
        IPN(ISUBPN+1) = IPN(ISUBPN) + IONE
        IN            = IS
 230  CONTINUE
      IF (LC.EQ.KM) MN = MN - IONE
      GO TO 140
C
C                                    OUTPUT.
C
 240  NLINES = 8 + IDIV (KX-IONE,ITWO,IND)
      ISUBCL = LSUBCL - 12
      ISUBRM = LSUBRM - 12
      DO 320 M=1,K
        MM     = M
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
        IF (NLINES+ITHRE.LE.NTLINE) GO TO 250
        CALL PAGE (IFOUR)
        NLINES = ITHRE
 250    IF (KO.GT.IONE .AND. M.EQ.IONE) WRITE (IPRINT,390)
        IF (KO.GT.IONE .AND. M.GT.IONE) WRITE (IPRINT,340) M
        NLINES = NLINES + ITWO
        IPRTSW = IZERO
        DO 310 LA=1,KO
          NCOF   = IONE
          L      = KO - LA + IONE
          MSUBRM = ISUBRM + L
          IF (A(MSUBRM).EQ.TWO) GO TO 320
          IF (IBIT.EQ.IONE)  R2 = SPCA - FDIV (A(MSUBRM),SS,IND)
          IF (IBIT.EQ.ITWO)  RS = FDIV (A(MSUBRM),FLOAT(NDEF-M),IND)
          IF (IBIT.EQ.ITHRE) RS = A(MSUBRM) + SIG * FLOAT(M)
          IF (IBIT.EQ.IONE .AND. LA.LE.MBST .OR. IBIT.GT.IONE
     1         .AND. RS.LE.A(JSUBRM)) NCOF = IZERO
          IF (IBIT.EQ.ITWO)  R2 = SPCA - FDIV (RS,SS,IND)
          IF (IBIT.EQ.ITHRE) R2 = RTWO * FDIV (RS,SIG,IND) - FLOAT(NDEF)
C
C           ADJUSTMENT TO ALLOW USE OF MODEL WHICH DOES NOT HAVE
C              A CONSTANT TERM FOR THE FIRST TERM.
C                 CHANGE SUGGESTED BY JAMES W. FRANE.
C
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IZERO) R2 = R2 - RONE
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IONE ) R2 = R2 + RONE
C
C                               DECODE LABELS.
C
          MSUBCL = ISUBCL + L
          CAB    = A(MSUBCL)
          MP     = IONE
          ISUBCO = LSUBCO - IONE
          ISUBPN = NPSUBL
          DO 260 I=1,KX
            ISUBCO      = ISUBCO + IONE
            IF (CAB.LT.A(ISUBCO)) GO TO 260
            IPN(ISUBPN) = I
            MP          = MP + IONE
            CAB         = CAB - A(ISUBCO)
            ISUBPN      = ISUBPN + IONE
 260      CONTINUE
C
          IF (NCOF.NE.IZERO) GO TO 280
          NLINES = NLINES + M + ITHRE
          IF (NLINES.LE.NTLINE) GO TO 270
          CALL PAGE (IFOUR)
          NLINES = M + 6
 270      CALL COEF (R2,MP,KZ,A(LSUBXC),RR,IPN(NPSUBL),NDEF,MM,ND,
     1               MD(MDSUBL),NX,IBIT,A(LSUBZC))
          GO TO 310
 280      IF (IPRTSW.GT.IZERO) GO TO 300
          NLINES = NLINES + M + IONE
          IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
          IF (NLINES.LE.NTLINE) GO TO 290
            CALL PAGE (IFOUR)
            NLINES = M + IFOUR
            IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
 290      WRITE (IPRINT,350)
          IPRTSW = IONE
 300      ISTPPN = NPSUBL + M - IONE
          IF (LWIDE.GE.110) WRITE (IPRINT,360) R2, (IPN(I),I=NPSUBL,
     1                             ISTPPN)
          IF (LWIDE.LT.110) WRITE (IPRINT,370) R2, (IPN(I),I=NPSUBL,
     1                             ISTPPN)
 310    CONTINUE
 320  CONTINUE
      NCAL = NCAL + ITWO * NREG
      WRITE (IPRINT,380) NREG, NCAL
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 330  FORMAT (1H1,1X,53HSCREEN-MATRIX IS SINGULAR.  VARIABLES DELETED AR
     1E .../4X,22I3/4X,6I3)
 340  FORMAT (/1X, 3X,16HREGRESSIONS WITH,I3,10H VARIABLES)
 350  FORMAT (1H / 9X,14HC(P) STATISTIC,2X,9HVARIABLES)
 360  FORMAT (1H ,12X,F8.3,5X,28I3)
 370  FORMAT (1H ,14X,F8.3,3X,15I3/26X,13I3)
 380  FORMAT (/1X,1X,I9,12H REGRESSIONS,2X,I10,11H OPERATIONS)
 390  FORMAT (1H , 3X,26HREGRESSION WITH 1 VARIABLE)
C
C     ==================================================================
C
      END
*SEC
      SUBROUTINE SEC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.    SEC V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS WHICH ARE FUNCTIONS WITH TWO ARGUMENTS.
C
C     THE FIRST ARGUMENT IS (E) (A CONSTANT, OR COLUMN NUMBER)
C     THE SECOND ARGUMENT IS (C), A COLUMN NUMBER.
C
C     INSTRUCTIONS ...
C
C        ERROR OF (E) PUT IN COLUMN (C)
C           ERF(X) = (2/SQRT(PI))*INTEGRAL (EXP(-T**2)) DT FROM 0 TO X
C           ERF(-X) = -ERF(X)
C           L2 = 18
C           SUBPROGRAM ERRINT SUPPLIED BY IRENE STEGUN, MARCH 1970.
C
C        CERF OF (E) PUT IN COLUMN (C)
C           ERFC(X) = 1 - ERF(X)
C           L2 = 19
C           SUBPROGRAM ERRINT SUPPLIED BY IRENE STEGUN, MARCH 1970.
C
C        SININTEGRAL OF (E) PUT IN COLUMN (C)
C           SI(X) = INTEGRAL (SIN(T)/T) DT FROM 0 TO X
C              SI(-X) = -SI(X)
C           L2 = 20
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        COSINTEGRAL OF (E) PUT IN COLUMN (C)
C           CI(X) = GAMMA + LN(X) + INTEGRAL (COS(T-1)/T) DT FROM 0 TO X
C              GAMMA IS EULERS CONSTANT = 0.5772156649...
C              CI(-X) = -CI(-X)
C           L2 = 21
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        COSINTEGRAL OF (E) PUT REAL IN COL (C), IMAGINARY IN COL (C)
C           CI(-X) = -CI(-X)
C
C        EINTEGRAL OF (E) PUT IN COLUMN (C)
C           EI(X) = -P.V. INTEGRAL (EXP(-T)/T) DT FROM -X TO INFINITY
C           L2 = 22
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        NEGEINTEGRAL OF (E) PUT IN COLUMN (C)
C           EXNEI(X) = EXP(-X)*EI(X), X GT 0
C           L2 = 23
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        HSININTEGRAL OF (E) PUT IN COLUMN (C)
C           SHI(X) = INTEGRAL (SINH(T-1)/T) DT FROM 0 TO X
C              SHI(-X) = -SHI(X)
C           L2 = 24
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        HCOSINTEGRAL OF (E) PUT IN COLUMN (C)
C           CHI(X) = GAMMA + LN(X) + INTEGRAL (COSH(T-1)/T) DT FROM 0 TO
C              GAMMA IS EULERS CONSTANT = 0.5772156649...
C           L2 = 25
C           SUBPROGRAM SICIEI SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C        HCOSINTEGRAL OF (E) PUT REAL IN COL (C), IMAGINARY IN COL (C)
C           CHI(-X) = CHI(X)-I(PI)
C
C        GAMMA OF (E) PUT IN COLUMN (C)
C           L1 = 15, L2 = 8.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MARCH, 1970.
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
      REAL             X, Y, YI
      REAL             FDPCON
C
      DOUBLE PRECISION CHI, CHII, CI, CII, DY, DYI, EI, EXNEI
      DOUBLE PRECISION SHI, SI, DX, Z
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.EQ.ITWO) GO TO 10
        IF (NARGS.EQ.ITHRE .AND. L2.EQ.21) GO TO 10
        IF (NARGS.EQ.ITHRE .AND. L2.EQ.25) GO TO 10
        CALL ERROR (10)
C
  10  IF (NRMAX.LE.IZERO) CALL ERROR (9)
      CALL ADRESS (IONE,J1)
      IF (J1.LT.IZERO) J1 = -J1
      CALL ADRESS (ITWO,J2)
      IF (J2.LT.IZERO) CALL ERROR (20)
      IF (NARGS.EQ.ITWO) GO TO 20
        CALL ADRESS(ITHRE,J3)
      IF (J3.LT.IZERO) CALL ERROR (20)
  20  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      LL = L2 - 17
      IF (L1.EQ.15 .AND. L2.EQ.8) LL = 9
      IF (KIND(1).EQ.IZERO) GO TO 300
C
C     ..................................................................
C
C     FIRST ARGUMENT IS A CONSTANT.
C
      DX = DBLE (ARGS(1))
      GO TO (110,120,130,140,150,160,170,180,190), LL
C
 110  CALL ERRINT (DX,DY,Z)
        GO TO 240
C
 120  CALL ERRINT (DX,Z,DY)
        GO TO 240
C
 130  CALL SICIEI (IONE,DX,DY,CI,CII,EI,EXNEI,SHI,CHI,CHII,IERR)
        GO TO 240
C
 140  IF (DX.LE.DZERO .AND. NARGS.EQ.ITWO) GO TO 210
        IF (NARGS.EQ.ITWO) GO TO 145
        IF (DX) 145,220,145
 145  CALL SICIEI (IONE,DX,SI,DY,DYI,EI,EXNEI,SHI,CHI,CHII,IERR)
        GO TO 240
C
 150  IF (DX) 210,210,155
 155  CALL SICIEI (ITWO,DX,SI,CI,CII,DY,EXNEI,SHI,CHI,CHII,IERR)
        GO TO 240
C
 160  IF (DX) 210,210,165
 165  CALL SICIEI (ITWO,DX,SI,CI,CII,EI,   DY,SHI,CHI,CHII,IERR)
        GO TO 240
C
 170  CALL SICIEI (ITHRE,DX,SI,CI,CII,EI,EXNEI, DY,CHI,CHII,IERR)
        GO TO 240
C
 180  IF (DX.LE.DZERO .AND. NARGS.EQ.ITWO) GO TO 210
        IF (NARGS.EQ.ITWO) GO TO 185
        IF (DX) 185,220,185
 185  CALL SICIEI (ITHRE,DX,SI,CI,CII,EI,EXNEI,SHI, DY, DYI,IERR)
        GO TO 240
C
 190  X = DX
      CALL GAMMA (X,Y)
      GO TO 250
C
C     ..................................................................
C
 210  CALL ERROR (111)
        GO TO 230
 220  CALL ERROR (113)
 230  Y = RZERO
      IF (NARGS.EQ.ITWO) GO TO 250
      YI = RZERO
      GO TO 260
 240  Y = FDPCON (DY)
 250  CALL VECTOR (Y,J2)
      IF (NARGS.EQ.ITWO) RETURN
      YI = FDPCON (DYI)
 260  CALL VECTOR (YI,J3)
      RETURN
C
C     ==================================================================
C
C     FIRST ARGUMENT IS A COLUMN NUMBER.
C
 300  DO 470 I=1,NRMAX
        DX = DBLE (RC(J1))
        GO TO (310,320,330,340,350,360,370,380,390), LL
C
 310    CALL ERRINT (DX,DY,Z)
          GO TO 440
C
 320    CALL ERRINT (DX,Z,DY)
          GO TO 440
C
 330    CALL SICIEI (IONE,DX,DY,CI,CII,EI,EXNEI,SHI,CHI,CHII,IERR)
          GO TO 440
C
 340    IF (DX.GT.DZERO) GO TO 345
        IF (NARGS.EQ.ITHRE .AND. DX.LT.DZERO) GO TO 345
        IF (NARGS.EQ.ITHRE) GO TO 420
          GO TO 410
C
 345    CALL SICIEI (IONE,DX,SI,DY,DYI,EI,EXNEI,SHI,CHI,CHII,IERR)
          GO TO 440
C
 350    IF (DX.LE.DZERO) GO TO 410
        CALL SICIEI (ITWO,DX,SI,CI,CII,DY,EXNEI,SHI,CHI,CHII,IERR)
          GO TO 440
C
 360    IF (DX.LE.DZERO) GO TO 410
        CALL SICIEI (ITWO,DX,SI,CI,CII,EI,   DY,SHI,CHI,CHII,IERR)
          GO TO 440
C
 370    CALL SICIEI (ITHRE,DX,SI,CI,CII,EI,EXNEI, DY,CHI,CHII,IERR)
          GO TO 440
C
 380    IF (DX.GT.DZERO) GO TO 385
        IF (NARGS.EQ.ITHRE .AND. DX.LT.DZERO) GO TO 385
        IF (NARGS.EQ.ITHRE) GO TO 420
          GO TO 410
 385    CALL SICIEI (ITHRE,DX,SI,CI,CII,EI,EXNEI,SHI, DY, DYI,IERR)
          GO TO 440
C
 390    X = DX
        CALL GAMMA (X,Y)
        GO TO 450
C
C     ..................................................................
C
 410    CALL ERROR (111)
          GO TO 430
 420    CALL ERROR (113)
 430    DY = DZERO
 440    Y = FDPCON (DY)
 450    J1 = J1 + IONE
        RC(J2) = Y
        IF (NARGS.EQ.ITWO) GO TO 460
          YI = FDPCON (DYI)
        RC(J3) = YI
        J3 = J3 + IONE
 460    J2 = J2 + IONE
 470  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SEEEC
      SUBROUTINE SEEEC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  SEEEC V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBPROGRAM HANDLES INSTRUCTIONS OF THE FORM
C        XXXXXX (E) (E) (E) (C)
C
C     FOR L1=14 AND L2=7, INSTRUCTION IS -
C       DAYS COMPUTED FOR MONTH (E) DAY (E) YEAR (E) PUT IN COLUMN (C)
C
C     COMPUTES TOTAL NUMBER OF DAYS FROM JANUARY 1, 100 AD.
C            1 =  1/ 1/100 AD,
C            2 =  1/ 2/100 AD, ETC.
C       685561 =  1/ 1/77.
C
C     ZERO IS NOT ALLOWED AS A DAY, MONTH OR YEAR.
C        FOR EXAMPLE, THE YEAR 2000 MUST BE WRITTEN AS 2000.
C     FOR 1901 TO 1999, THE LAST TWO DIGITS NEED ONLY BE GIVEN AND
C        THEN 1900 IS AUTOMATICALLY ADDED.
C     HENCE DAYS CAN NOT BE COMPUTED FOR FIRST 100 YEARS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1976.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(12)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X1, X2, X3
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA L( 1), L( 2), L( 3), L( 4), L( 5), L( 6) /
     1         0,    31,    59,    90,   120,   151 /
      DATA L( 7), L( 8), L( 9), L(10), L(11), L(12) /
     1       181,   212,   243,   273,   304,   334 /
C
      DATA ICA / 36524 /
      DATA ICB /  1900 /
      DATA ICC /   365 /
      DATA ICD /   366 /
      DATA ICE /   400 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      X1 = RZERO
      X2 = RZERO
      X3 = RZERO
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NARGS.NE.IFOUR) CALL ERROR (10)
      CALL ADRESS (IONE,J1)
        IF (J1.LT.IZERO) X1 = ARGS(1)
      CALL ADRESS (ITWO,J2)
        IF (J2.LT.IZERO) X2 = ARGS(2)
      CALL ADRESS (ITHRE,J3)
        IF (J3.LT.IZERO) X3 = ARGS(3)
      CALL ADRESS (IFOUR,J4)
        IF (J4.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     COMPUTE DAYS.
C
      NZERO = ICA
      DO 50 I=1,NRMAX
        IF (KIND(1).EQ.IZERO) X1 = RC(J1)
        IF (KIND(2).EQ.IZERO) X2 = RC(J2)
        IF (KIND(3).EQ.IZERO) X3 = RC(J3)
        IMONTH = X1 + RFIVE * RTEN**(-ISIGD)
          IF (IMONTH.LT.IONE) GO TO 10
        IDAY   = X2 + RFIVE * RTEN**(-ISIGD)
          IF (IDAY  .LT.IONE) GO TO 10
        IYEAR  = X3 + RFIVE * RTEN**(-ISIGD)
          IF (IYEAR .LT.IONE) GO TO 10
          IF (IYEAR.LT.IHRD) IYEAR = IYEAR + ICB
          ITEMP = IDIV (MXINT,ICD,IND) + IONE
          IF (IYEAR.LT.ITEMP) GO TO 20
            CALL ERROR (105)
            RETURN
C
  10        CALL ERROR (111)
            RETURN
C
  20    NDAYS = ICC*IYEAR + IDIV (IYEAR,IFOUR,IND) - IDIV (IYEAR,IHRD,
     1               IND) + IDIV (IYEAR,ICE,IND)
        NDAYS = NDAYS + L(IMONTH) + IDAY
        K = IFOUR * IDIV (IYEAR,IFOUR,IND)
        IF (IYEAR.NE.K) GO TO 40
        K = IHRD * IDIV (IYEAR,IHRD,IND)
        IF (IYEAR.NE.K) GO TO 30
        K = ICE * IDIV (IYEAR,ICE,IND)
        IF (IYEAR.NE.K) GO TO 40
  30    IF (IMONTH.LT.ITHRE) NDAYS = NDAYS - IONE
  40    RC(J4) = FLOAT (NDAYS-NZERO)
        IF (KIND(1).EQ.IZERO) J1 = J1 + IONE
        IF (KIND(2).EQ.IZERO) J2 = J2 + IONE
        IF (KIND(3).EQ.IZERO) J3 = J3 + IONE
        J4 = J4 + IONE
  50  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SEIC
      SUBROUTINE SEIC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SEIC V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE HANDLES INSTRUCTIONS WITH ARGUMENTS (E), (I), (C)
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - APRIL, 1970.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             RESULT, X
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.EQ.IZERO)   CALL ERROR (9)
      IF (NARGS.NE.ITHRE)   CALL ERROR (10)
      IF (KIND(2).NE.IZERO) CALL ERROR (20)
      CALL ADRESS (IONE,J)
      IF (J.LT.IZERO) J = -J
      CALL ADRESS (ITHRE,K)
      IF (K.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     ROUND X EQUAL TO (E) TO (I) SIGNIFICANT DIGITS, PUT IN COLUMN (C)
C
      N = IARGS(2)
      IF (N      .LE.IZERO) GO TO 30
      IF (N      .GT.NSIGD) GO TO 30
      IF (KIND(1).EQ.IZERO) GO TO 10
      X = ARGS(1)
      CALL SDRND (X,N,NSIGD,RESULT)
      CALL VECTOR (RESULT,K)
      RETURN
C
C     ..................................................................
C
  10  DO 20 I=1,NRMAX
        X = RC(J)
        CALL SDRND (X,N,NSIGD,RC(K))
        J = J + IONE
        K = K + IONE
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  CALL ERROR (3)
      RETURN
C
C     ==================================================================
C
      END
*SELECT
      SUBROUTINE SELECT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SELECT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 1,     SELECT
C     L2 = 2,     SEARCH
C     L2 = 3,     CENSOR
C     L2 = 5,     MATCH
C
C     SELECT IN (C) VALUES APPROX COL (C) TO WITHIN (K), PUT IN COL (C)
C
C     SELECT IN (C) VALUES APPROX COL (C) TO WITHIN (K) IN (C) TO (C)
C
C     SELECT IN (C) VALUES APPROX (C) TO WITHIN (K) IN (C) TO (C) NO (C)
C
C     SEARCH (C) FOR NOS IN (C) CORRESP VALUES FROM (C) INTO (C), ETC.
C
C     CENSOR COL (C) FOR (E), REPLACING BY (E), PUT IN COL (C)
C
C     MATCH COLUMN (C) WITH (E), EXTRACT (E), PUT IN COLUMN (C)
C
C               REVISED BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1970.
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
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ARG3, AT, X2, X3
C
C     ==================================================================
C
      L3 = L2
      IF (L2.GT.IFIVE) L2 = ITHRE
C
      GO TO (10,50,70,20,70), L2
C
  10  IF (KIND(3).NE.IZERO) GO TO 30
      CALL ERROR (3)
  20  RETURN
C
C     ..................................................................
C
  30  IARGS(3) = IARGS(2)
      KIND(3)  = IZERO
      IF (NARGS.LT.IFOUR) CALL ERROR (10)
      IF (NARGS.GT.IFOUR) GO TO 40
      IARGS(5) = IARGS(4)
      NARGS    = NARGS + IONE
      KIND(5)  = KIND(4)
  40  IF (NARGS.GT.6) CALL ERROR (10)
      IF (IARGS(4).LE.IARGS(5)) GO TO 90
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  50  IF (NARGS.LT.IFOUR) CALL ERROR (10)
      IF (ITWO*IDIV(NARGS,ITWO,IND).NE.NARGS) CALL ERROR (10)
  60  CALL CHKCOL
      GO TO 80
C
  70  IF (NARGS.GT.IFOUR) CALL ERROR (10)
      CALL ADRESS (IONE,I1)
      IF (I1.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (ITWO,I2)
      IF (I2.LT.IZERO) I2 = ITWO
      CALL ADRESS (ITHRE,I3)
      IF (I3.LT.IZERO) I3 = ITHRE
      CALL ADRESS (IFOUR,I4)
      IF (I4.LT.IZERO) CALL ERROR (20)
  80  IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      GO TO (100,210,280,20,280), L2
C
  90  IF (IARGS(5)-IARGS(4).LE.NRMAX-IONE) GO TO 60
      IARGS(5) = IARGS(4) + NRMAX - IONE
      GO TO 60
C
C     SELECT.
C
 100  DO 120 I=1,NRMAX
        L = IARGS(1) + I - IONE
        K = IARGS(2) + I - IONE
        J = IARGS(4) + I - IONE
        M = NRMAX + I
        A(I)  = RC(K)
        A(M)  = RC(L)
 110    RC(J) = RZERO
        IF (J.GE.I+IARGS(5)-IONE) GO TO 120
        J = NROW + J
        GO TO 110
 120  CONTINUE
C
      ARG3 = ABS (ARGS(3))
      DO 200 I=1,NRMAX
        K  = NRMAX + IONE
        L  = ITWO * NRMAX
        M  = ITHRE * NRMAX
        N  = IFOUR * NRMAX
        I1 = IARGS(4) + I - IONE
        J1 = IARGS(6) + I - IONE
        DO 130 J=K,L
          AT   = ABS (A(I) - A(J))
          IF (ARG3.LT.AT) GO TO 130
          M    = M + IONE
          A(M) = AT
          N    = N + IONE
          A(N) = A(J)
 130    CONTINUE
        IF (M-ITHRE*NRMAX.EQ.IONE) GO TO 140
        IF (M-ITHRE*NRMAX.GT.IONE) GO TO 150
        IF (NARGS.LE.IFIVE) GO TO 200
        RC(J1) = RZERO
        GO TO 200
 140    RC(I1) = A(N)
        IF (NARGS.LE.IFIVE) GO TO 200
        RC(J1) = RONE
        GO TO 200
 150    M1 = ITHRE * NRMAX + ITWO
 160    K2 = IZERO
        DO 170 J=M1,M
          IF (A(J).GE.A(J-1)) GO TO 170
          AT     = A(J)
          A(J)   = A(J-1)
          A(J-1) = AT
          N      = J + NRMAX
          AT     = A(N)
          A(N)   = A(N-1)
          A(N-1) = AT
          K2     = K2 + IONE
 170    CONTINUE
        IF (K2.GT.IZERO) GO TO 160
        N = IFOUR * NRMAX + IONE
 180    RC(I1) = A(N)
        I1 = I1 + NROW
        N  = N + IONE
        IF (N-M.GT.NRMAX) GO TO 190
        IF (I1.LE.I+IARGS(5)-IONE) GO TO 180
 190    IF (NARGS.LE.IFIVE) GO TO 200
        RC(J1) = M - ITHRE * NRMAX
 200  CONTINUE
      RETURN
C
C     ..................................................................
C
C     SEARCH.
C
 210  I1 = NARGS - IONE
      DO 240 I=1,NRMAX
        K    = IARGS(1) + I - IONE
        L    = IARGS(2) + I - IONE
        M    = NRMAX + I
        A(I) = RC(L)
        A(M) = RC(K)
        J1   = ITWO
        DO 220 N=3,I1,2
          L    = J1 * NRMAX + I
          M    = IARGS(N) + I - IONE
          A(L) = RC(M)
          J1   = J1 + IONE
 220    CONTINUE
        DO 230 N=4,NARGS,2
          M = IARGS(N) + I - IONE
          RC(M) = RZERO
 230    CONTINUE
 240  CONTINUE
C
      K = NRMAX + IONE
      L = ITWO * NRMAX
      DO 270 I=1,NRMAX
        AT = ABS ( RER * A(I) )
        DO 260 J=K,L
          IF (ABS(A(I)-A(J)).GT.AT) GO TO 260
          J1 = IONE
          DO 250 N=4,NARGS,2
            M     = IARGS(N) + I - IONE
            I1    = J1 * NRMAX + J
            RC(M) = A(I1)
            J1    = J1 + IONE
 250      CONTINUE
          GO TO 270
 260    CONTINUE
 270  CONTINUE
      RETURN
C
C     ..................................................................
C
C     CENSOR OR MATCH.
C
 280  IF (L3.EQ.ITHRE) L3 = 6
      L3 = L3 - IFOUR
      X3 = ARGS(3)
      X2 = ARGS(2)
      DO 390 I=1,NRMAX
        IF (KIND(3).EQ.IZERO) X3 = RC(I3)
        IF (KIND(2).EQ.IZERO) X2 = RC(I2)
C
        GO TO (290,300,320,330,340,350,360), L3
C
C       MATCH.
C
 290    IF (RC(I1)-X2) 380,370,380
C
C       CENSOR OR CENSOR LE.
C
 300    IF (RC(I1).LE.X2) GO TO 370
 310    RC(I4) = RC(I1)
        GO TO 380
C
C       CENSOR EQ.
C
 320    IF (RC(I1)-X2) 310,370,310
C
C       CENSOR GE.
C
 330    IF (RC(I1)-X2) 310,370,370
C
C       CENSOR GT.
C
 340    IF (RC(I1)-X2) 310,310,370
C
C       CENSOR LT.
C
 350    IF (RC(I1)-X2) 370,310,310
C
C       CENSOR NE.
C
 360    IF (RC(I1).EQ.X2) GO TO 310
 370    RC(I4) = X3
 380    I3 = I3 + IONE
        I4 = I4 + IONE
        I1 = I1 + IONE
        I2 = I2 + IONE
 390  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SETUP    
      SUBROUTINE SETUP (LG)   
C         
C **  NBS OMNITAB 2280 VERSION 6.01  2/25/81.  SETUP V 7.00  4/30/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     ADD A SWITCH TO DETERMINE WHETHER PRINTX OR RPRINT WILL BE USED 
C     INITIALIZE SWITCH TO ZERO FLEXIBLE FORMAT(RPRINT) WILL BE USED  
C     SWITCH WILL BE SET TO 1 IF FIXED OR FLOATING IS ENCOUNTERED     
C     IF PRINT COMMAND HAS DEC. ARGS FLEXIBLE FORMAT WILL BE USED     
C     SWITCH WILL NOT BE CHANGED        
C     FLEXIBLE COMMAND WILL CHANGE SWITCH TO 0    
C     IF PRINT WITH ALL INTEGER ARGS AND SWITCH=0 USE FLEXIBLE FORMAT 
C     IF PRINT WITH ALL INTEGER ARGS AND SWITCH=1,USE SPECIFIED FORMAT
C              (FIXED OR FLOATING)      
C         
C               REVISED BY -  
C                      SALLY T. PEAVY AND DAVID HOGBEN,     
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -   
C                   CURRENT VERSION - APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   

      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD     
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL        
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD     
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP 
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD        
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NE     
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP   
      COMMON /NRCOL/  IROW, ICOL
      COMMON /PCONST/ PC(40), JPC, NT(40)         
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR        
      COMMON /PRHEAD/ IHEAD(6,50), NHEADS         
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH         
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG         
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH    
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
      COMMON /VECCHR/ NTPR(120)
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                    ***   TYPE STATEMENTS   ***   

      REAL             SPCA, SPCB       
C         
C     ..................................................................
C
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
C
C     ..................................................................
C        
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER LA*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER NEWCRD*1
      CHARACTER NTPR*1
      CHARACTER LLA(14)*1     
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA SPCA / 9.0 /       
      DATA SPCB / 6.0 /       
      DATA SPCC / 9.0 /       
C         
      DATA ICA /    8 /       
      DATA ICB /  120 /       
      DATA ICD /   60 /       
      DATA ICE /    6 /       
      DATA ICF /   12 /       
      DATA ICG /  100 /       
      DATA ICH /    8 /       
      DATA ICI / 2000 /       
      DATA ICJ /  132 /       
      DATA ICM /   12 /       
C         
      DATA LLA(1), LLA(2), LLA(3), LLA(4), LLA(5), LLA(6), LLA(7),    
     1     LLA(8), LLA(9),LLA(10),LLA(11),LLA(12),LLA(13),LLA(14)     
     2/       '0',    '1',    '2',    '3',    '4',    '5',     '6',   
     3        '7',    '8',    '9',    ' ',    'E',    'F',     'P'/   
C         
C     ==================================================================        
C         
C     IF LG IS NEGATIVE, FIRST CARD WAS NOT 'OMNITAB' CARD  
C                  ZERO, FIRST CARD IS 'OMNITAB' CARD       
C              POSITIVE, SUBSEQUENT 'OMNITAB' CARD FOUND.   
C         
C     INTIALIZE NRL ONLY THE 1ST OMNITAB INSTRUCTION ENCOUNTERED.     
C         
      IF(LG.LE.IZERO) NRL = IZERO       
C         
      IF (LG.LT.IZERO) GO TO 20         
C         
C     GO THROUGH 'STOP' SEQUENCE IF NOT FIRST OMNITAB INSTRUCTION.    
C         
      IF (LG.GT.IZERO) CALL XSTOP       
C         
C     COPY NEW OMNITAB CARD INTO HOLDING AREA.    
C         
      DO 10 I=1,LENCRD        
        NOMNIT(I) = NEWCRD(I) 
        NOCARD(I) = NEWCRD(I) 
  10  CONTINUE      
C         
C     INITIALIZATION WHICH IS PERFORMED WITH EVERY OMNITAB INSTRUCTION.         
C         
  20  RMXINT = MMXINT         
      DMXINT = DBLE (RMXINT)  
      ISIGD  = MIN0 (ICA,NSIGD)         
C         
C     LWC IS THE WIDTH OF A WIDE CARRIAGE TERMINAL, CRT, OR PRINTER.  
C         
      LWC    = ICB  
C         
C     NCW IS THE WIDTH OF A NARROW CARRIAGE TERMINAL OR CRT DEVICE.   
C         
      NCW    = NCWIDE
C         
      NS2    = IDIV (NS,ITWO,IND)       
      PC(1)  = PI   
      PC(2)  = PI   
      PC(3)  = E    
      PC(4)  = E    
      RSD    = NSIGD
C         
C     INITIALIZE SYSTEM.      
C         
      DO 40 I=1,ICD 
        DO 30 J=1,ICE         
          ITLE(I,J) = LA(45)  
  30    CONTINUE    
  40  CONTINUE      
C         
      DO 50 I=1,ICF 
        IFMTX(I)=IFMTS(I)     
  50  CONTINUE      
C         
      DO 60 I=1,ICG 
        ARGS(I) = RZERO       
  60  CONTINUE      
C         
      DO 70 I=1,NRC 
        RC(I) = RZERO         
  70  CONTINUE      
C         
      DO 80 I=1,ICH 
       VWXYZ(I) = RZERO       
  80  CONTINUE      
C         
      DO 90 I=1,120 
        NTPR(I) = LA(45)      
  90  CONTINUE      
C         
      DO 100 I=1,4  
        NCHTIT(I) = IZERO     
 100  CONTINUE      
C         
      DO 110 I=1,KMES         
        MESS(I) = IZERO       
 110  CONTINUE      
C         
C     CLEAR IFMT, IHEAD.      
C         
      DO 130 I=1,ICE
        IFMT(I) = ' '         
 130  CONTINUE      
C         
      DO 150 I=1,NHEADS       
        DO 140 IJ=1,ICE       
          IHEAD(IJ,I) = IZERO 
 140    CONTINUE    
 150  CONTINUE      
C         
      IFG    = IZERO
      IHCNT  = IZERO
      ILABEL = IZERO
      IOSWT  = IZERO
      IOVFL  = IZERO
      IPRINT = NPRNT
      ISBFT  = IONE
      ISPD   = IONE 
      ISE    = IZERO
      JPC    = -IONE
      KRDEND = LENCRD         
      MODE   = IONE 
      LCOM   = ICI  
      LENGTH = NLENGT         
      LEVEL  = IZERO
      LKARD  = LENCRD + ITHRE 
      LLIST  = ITHRE
      LURCD  = ICJ  
      LWIDE  = NCWIDE
      NBLKPR = IONE 
      NCHPR  = LURCD * NBLKPR 
      NCOM   = IONE 
      NCRT   = IONE
      NDEMD  = IONE
      MNOE   = IHRD 
      NCOL   = ICOL
      NERR   = IZERO
      NERROR = IZERO
      NLOCRM = IZERO
      NLSWT  = IZERO
      NPAGE  = IZERO
      NPER   = ICM  
      NRMAX  = IZERO
      NROLD  = IZERO
      NROW   = IROW
      NSTMT  = IZERO
      NSTMTH = IZERO
      NTPE   = LPTAPE         
C         
C     INITIALIZE FORMAT FOR FIXED INSTRUCTION.    
C         
      INUM   = IDIV (LWIDE,IPLACE,IND)  
      INA    = IDIV (INUM,ITEN,IND)     
      IF (INA.EQ.IZERO) INA = ITEN      
      INB    = MOD (INUM,ITEN)
      IFMTX(4) = LLA(INA+1)   
      IFMTX(5) = LLA(INB+1)   
      INA    = IDIV (IPLACE,ITEN,IND)   
      IF (INA.EQ.IZERO) INA = ITEN      
      INB    = MOD (IPLACE,ITEN)        
      IFMTX(7) = LLA(INA+1)   
      IFMTX(8) = LLA(INB+1)   
C         
C     COMPUTE SEARCH CONSTANTS FOR LOOKUP OF IR(.).         
C         
      NIRMID = IDIV (NIR,ITWO,IND)      
      NIRQTR = IDIV (NIR,IFOUR,IND)     
      NIRTRD = NIRMID + NIRQTR
C         
C     REAL VARIABLES.         
C         
      HGT    = SPCA 
      XDH    = SPCB 
C         
C     SET DEFAULT VALUES FOR TEKTRONIX PLOT.      
C         
C     ITEK(I,1) IS SET TO 960 CHAR/SEC. 
C     ITEK(I,2) IS SET TO 2 TERMINAL TYPE (4014). 
C     ITEK(I,3) IS SET TO 1 BUFFER TYPE (4014).   
C     ITEK(I,4) IS SET TO 2,3,C FOR OPTIONS.      
C     ITEK(I,5) IS SET TO H FOR OPTIONS.
C     ITEK(I,6) IS SET TO C FOR OPTIONS.
C         
      DO 170 I=1,10 
        DO 160 J=1,6
          ITEK(I,J) = 32      
 160    CONTINUE    
 170  CONTINUE      
C         
      ITEK(1,1) = 57
      ITEK(2,1) = 54
      ITEK(3,1) = 48
      ITEK(1,2) = 50
      ITEK(1,3) = 49
      ITEK(1,4) = 50
      ITEK(2,4) = 44
      ITEK(3,4) = 51
      ITEK(4,4) = 44
      ITEK(5,4) = 67
      ITEK(1,5) = 72
      ITEK(1,6) = 67
C         
      TEKHGT    = SPCA        
      TEKXDH    = SPCC        
      ITEKSW    = IZERO       
      RETURN        
C         
C     ==================================================================        
C         
      END 
*SIEC
      SUBROUTINE SIEC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SIEC V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE HANDLES INSTRUCTIONS OF THE FORM (I), (E), (C)
C
C     INSTRUCTIONS ***
C        EXPINTEGRAL (N), (E), (C)
C           EN(X) = INTEGRAL (EXP(-X*T)DT/(T**N)
C           L2 = 26
C           SUBPROGRAM EXPINT SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C        EEXPINTEGRAL (N), (E), (C)
C           EXPENX = EXP(X)*EN(X)
C           L2 = 27
C           SUBPROGRAM EXPINT SUPPLIED BY IRENE STEGUN, DECEMBER 1975.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1975.
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
      REAL             RESULT
      REAL             FDPCON
C
      DOUBLE PRECISION ENX, EXPENX, RN, X
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NARGS.NE.ITHRE) CALL ERROR (10)
      IF (KIND(1).NE.IZERO) CALL ERROR (20)
      IF (IARGS(1).LT.IZERO) CALL ERROR (3)
      CALL ADRESS (ITWO,J)
      IF (J.LT.IZERO) J = -J
      CALL ADRESS (ITHRE,K)
      IF (K.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      N = IARGS(1)
      RN = N
      IF (KIND(2).EQ.IZERO) GO TO 60
C
C     ..................................................................
C
C     SECOND ARGUMENT IS A CONSTANT.
C
      X = ARGS(2)
      IF (X.GT.RZERO) GO TO 40
      IF (N.GT.IONE) GO TO 20
        CALL ERROR (111)
        GO TO 30
  20  IF (X.GE.RZERO) GO TO 40
        CALL ERROR (112)
  30    RESULT = RZERO
        GO TO 50
  40  CALL EXPINT (RN,X,ENX,EXPENX,IERR)
      IF (L2.EQ.26) RESULT = ENX
      IF (L2.EQ.27) RESULT = EXPENX
  50  CALL VECTOR (RESULT,K)
      RETURN
C
C     ..................................................................
C
C     SECOND ARGUMENT IS A COLUMN NUMBER.
C
  60  DO 120 I=1,NRMAX
        X  =  RC(J)
        IF (X.GT.RZERO) GO TO 90
        IF (N.GT.IONE) GO TO 70
          CALL ERROR (111)
          GO TO 80
  70    IF (X.GE.RZERO) GO TO 90
          CALL ERROR (112)
  80      RC(K) = RZERO
          GO TO 100
  90    CALL EXPINT (RN,X,ENX,EXPENX,IERR)
        IF (L2.EQ.26) RC(K) = FDPCON (ENX)
        IF (L2.EQ.27) RC(K) = FDPCON (EXPENX)
 100    J = J + IONE
        K = K + IONE
 120  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SLOMNI
      SUBROUTINE SLOMNI
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SLOMNI V 7.00  4/10/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE THE INSTRUCTIONS ...
C
C          I   STEM LEAF  OF COLUMN (C)
C         II   STEM LEAF  OF COL (C) PUT SCRAWL IN (C)
C        III   STEM LEAF  OF COL (C) SCRAWL (C) PUT DEPTH IN  (C)
C         IV   STEM LEAF  OF COL (C) FOR (I) , (J), (K), (L)
C          V   STEM LEAF  (C) FOR (I), (J), (K), (L), SCRAWL IN (C)
C         VI   STEM LEAF  (C) FOR (I) (J) (K) (L) SCRAWL (C) DEPTH (C)
C
C     COMMANDS IN  II, III, V AND VI  MAY BE PREFACED BY S TO
C        SUPPRESS AUTOMATIC PRINTING.
C
C     IF SCRAWL IS STORED, IT WILL BE AS FOLLOWS ...
C
C          ROW 1   N = NUMBER OF OBSERVATIONS
C          ROW 2   FIRST ORDERED VALUE
C          ROW 3   SECOND ORDERED VALUE
C          ROW 4   FIRST HINDGE
C          ROW 5   MEDIAN
C          ROW 6   SECOND HINDGE
C          ROW 7   N-1  ORDERED VALUE
C          ROW 8   N    ORDERED VALUE
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2485
C                  ORIGINAL VERSION - MARCH, 1974.
C                   CURRENT VERSION -  APRIL, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ISTEM(4)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      EQUIVALENCE (II,ISTEM(1)), (JJ,ISTEM(2))
      EQUIVALENCE (KK,ISTEM(3)), (LL,ISTEM(4))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 99 /
C
C     ==================================================================
C
      IF (NARGS.GE.IONE .AND. NARGS.LT.8) GO TO 20
  10  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  IF (NARGS.EQ.IONE  .AND. L2.EQ.ITWO) GO TO 210
      IF (NARGS.EQ.IFIVE .AND. L2.EQ.ITWO) GO TO 210
      ISCRL = IZERO
      ICASE = IONE
      IDPTH = IZERO
C
      GO TO (90,80,70,10,50,40,30), NARGS
C
  30  CALL ADRESS (7,IDPTH)
      IF (IDPTH.LT.IZERO) CALL ERROR (20)
  40  CALL ADRESS (6,ISCRL)
      IF (ISCRL.LT.IZERO) CALL ERROR (20)
  50  ICASE = IZERO
      DO 60 I=2,5
        ISTEM(I-1) = IARGS(I)
        IF (KIND(I).EQ.IONE) ISTEM(I-1) = ARGS(I)
        IF (ISTEM(I-1).GT.IZERO) GO TO 60
        IF (I.EQ.ITWO .AND. ISTEM(1).EQ.IZERO) GO TO 60
        CALL ERROR (36)
        RETURN
  60  CONTINUE
      GO TO 90
C
  70  CALL ADRESS (ITHRE,IDPTH)
      IF (IDPTH.LT.IZERO) CALL ERROR (20)
  80  CALL ADRESS (ITWO,ISCRL)
      IF (ISCRL.LT.IZERO) CALL ERROR (20)
  90  CALL ADRESS (IONE,IDATA)
      IF (IDATA.LT.IZERO) CALL ERROR (20)
      IF (NRMAX*ITHRE.GT.NS) CALL ERROR (23)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
      IF (ISCRL.EQ.IZERO) GO TO 100
      IF (NROW.GE.8) GO TO 100
      CALL ERROR (243)
      ISCRL = IZERO
 100  ID = IDATA
      DO 110 I=1,NRMAX
        A(I) = RC(ID)
        ID   = ID + IONE
 110  CONTINUE
C
      ID = ITWO * NRMAX + IONE
      IPRT = IZERO
      IF (L2.EQ.ITWO) GO TO 120
      IPRT = IONE
 120  LUPPER = LWIDE - IONE
 130  CALL MAINSL (A(1),A(NRMAX+1),NRMAX,ICASE,II,JJ,KK,LL,IPRT,IPRINT,
     1             LUPPER,ISCRL,IDPTH,NROW,RC(ISCRL),A(ID),IND)
      IF (IND.NE.IZERO) GO TO 160
      IF (IDPTH.EQ.IZERO) RETURN
      NDPTH = A(ID) + RONE
      IF (NDPTH.LE.NROW) GO TO 140
      CALL ERROR (226)
      NDPTH = NROW
 140  NSPV  = IDPTH
      IDD   = ID
      DO 150 I=1,NDPTH
        RC(NSPV) = A(IDD)
        NSPV = NSPV + IONE
        IDD = IDD + IONE
 150  CONTINUE
      RETURN
C
C     ..................................................................
C
 160  IF (IND.LT.IZERO) GO TO 220
C
      GO TO (170,170,180,190,190,  190,190,200,190,190), IND
C
 170  IF (ISCRL+IDPTH.EQ.IZERO) CALL ERROR (248)
      IF (ISCRL+IDPTH.NE.IZERO) CALL ERROR ( 14)
      RETURN
C
C     ..................................................................
C
 180  CALL ERROR (35)
      RETURN
C
C     ..................................................................
C
 190  CALL ERROR (36)
      RETURN
C
C     ..................................................................
C
 200  CALL ERROR (242)
      IF (NARGS.EQ.IONE .OR. NARGS.EQ.IFIVE) RETURN
      IPRT = IZERO
      GO TO 130
 210  CALL ERROR (236)
      RETURN
C
C     ..................................................................
C
 220  NARGS = ICA - IND
      CALL ERROR (244)
      RETURN
C
C     ==================================================================
C
      END
*SORDER
      SUBROUTINE SORDER
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SORDER V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 =  8 FOR SORT
C     L2 =  9 FOR ORDER
C     L2 = 14 FOR HIERARCHY
C
C     HIERARCHY OF COLUMN (C) PUT IN COL (C)
C
C        HIERARCHY PUTS THE ROW LOCATION OF THE SMALLEST NUMBER IN THE
C           FIRST COLUMN IN THE FIRST ROW OF THE SECOND COLUMN,  THE ROW
C           NUMBER OF THE SECOND SMALLEST NUMBER OF THE FIRST COLUMN IS
C           PUT IN THE SECOND ROW OF THE SECOND COLUMN, ... THE ROW
C           NUMBER OF THE LARGEST NUMBER OF THE FIRST COL IS PUT IN ROW
C           NRMAX OF THE SECOND COLUMN.  THE FIRST COLUMN IS UNCHANGED
C           BY THIS INSTRUCTION.
C
C     ORDER COLUMNS (C), (C), (C),  ETC
C
C        ORDER PLACES EACH ONE OF THE GIVEN COLUMNS IN NUMERICALLY
C           INCREASING ORDER.
C
C     SORT COL (C) CARRY ALONG COLUMNS (C), (C),  ETC
C
C        SORT PLACES THE FIRST COLUMN IN NUMERICALLY INCREASING ORDER
C           WHILE PRESERVING THE ROW RELATIONSHIPS AMONG THE GIVEN
C           COLUMNS.
C
C           SUBROUTINE BY CARLA MESSINA, 221.04  JUNE 1967
C
C               REVISED BY -
C                      SALLY PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1976.
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
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 0.05 /
C
C     ==================================================================
C
C      CHECK FOR PROPER ARGUMENTS
C
      CALL CHKCOL
      IF (NRMAX.EQ.IZERO) GO TO 120
      IF (L2.EQ.14) GO TO 80
C
C     COMMAND IS EITHER ORDER OR SORT
C
      IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.EQ.IONE) GO TO 30
      IF (L2.EQ.8) GO TO 30
C
C     COMMAND IS ORDER
C        CALL SORT TO SORT DATA FOR EACH COLUMN
C
  10  DO 20 I=1,NARGS
        K = IARGS(I)
        CALL SORT (RC(K),A(1),NRMAX,IZERO)
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COMMAND IS SORT
C        CALL SORT PROCEDURE TO SORT DATA OF FIRST COL AND
C        HIERARCHY FOR EQUAL VALUES OF DATA
C
C     SORT WITH ONE ARGUMENT
C
  30  IF (NARGS.EQ.IONE) GO TO 10
C
C     SORT AND CARRY ALONG COLS
C
      K = IARGS(1)
      CALL SORT (RC(K),A(1),NRMAX,IONE)
C
C     A(1),...,A(NRMAX) CONTAINS HIERARCHY. USE HIERARCHY AS
C         SUBSCRIPT TO PICK PROPER ROWS FOR CARRY ALONG COLUMNS,
C         AFTER MOVING CARRY ALONG COLUMNS INTO SCRATCH AREA.
C
      DO 50 I=2,NARGS
        L = IARGS(I)
        K = (I-IONE)*NROW + IONE
        DO 40 J=1,NRMAX
          A(K) = RC(L)
          L = L + IONE
          K = K + IONE
  40    CONTINUE
  50  CONTINUE
C
C     ORDER CARRY ALONG COLUMNS AND STORE IN WORK SHEET.
C
      DO 70 I=1,NRMAX
        K = A(I) + SPCA
        DO 60 J=2,NARGS
          L = IARGS(J) - IONE + I
          KK = K + (J-IONE)*NROW
          RC(L) = A(KK)
  60    CONTINUE
  70  CONTINUE
       RETURN
C
C     ..................................................................
C
C     HIERARCHY COMMAND
C        MOVE NUMBERS TO FIND HIERARCY INTO SCRATCH AREA.
C
  80  IF (NARGS.NE.ITWO) GO TO 110
      IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.EQ.IONE) GO TO 100
      L = IARGS(1)
      DO 90 I=1,NRMAX
        A(I) = RC(L)
        L = L + IONE
  90  CONTINUE
C
      K = IARGS(2)
      CALL SORT (A(1),RC(K),NRMAX,IONE)
      RETURN
C     NRMAX = 1 FOR HIERARCHY
 100  L = IARGS(2)
      RC(L) = RONE
      RETURN
C
C     ..................................................................
C
C     ILLEGAL NUMBER OF ARGUMENTS FOR ORDER.
C
 110  CALL ERROR (10)
      RETURN
C
C     NRMAX = IZERO
C
 120  CALL ERROR (9)
      RETURN
C
C     ==================================================================
C
      END
*SPINST
      SUBROUTINE SPINST (LETSGO,INT,INST)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. SPINST V 7.00  7/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C      CHECK TO SEE IF INSTRUCTION IS ONE OF THE FOLLOWING ...
C
C        OMNITAB,
C        FINISH,
C        FORMAT,
C        NOTE,
C        HEAD,
C        TITLE1, TITLE2, TITLE3, TITLE4, TITLEX, TITLEY,
C        FILE OR
C        STOP.
C
C     INPUT.
C
C        LETSGO = -1 IF FIRST INPUT STATMENT IS NOT OMNITAB,
C                  0 IF FIRST INPUT STATMENT IS OMNITAB,
C                  GREATER THAN ZERO IF NEW OMNITAB STATMENT IS FOUND.
C
C           INT = -1 INTIALIZE BY CALLING SETUP.
C               =  0 CHECK FOR SPECIAL INSTRUCTIONS.
C
C     OUTPUT.
C
C        INST = 0, IF INSTRUCTION IS ONE OF THE ABOVE EXCEPT STOP,
C             = 1, IF INSTRUCTION IS NOT OMNITAB OR ANY OTHER ABOVE,
C             = 2, IF INSTRUCTION IS STOP.
C             = 3, IF INSTRUCTION IS OMNITAB AND LANGUA MUST BE CALLED.
C             = 4, IF FIRST INSTRUCTION IS NOT OMNITAB.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - JANUARY, 1978.
C                   CURRENT VERSION -    JULY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /LANGUE/ LANGC, LANGP
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
      COMMON /VECCHR/ NTPR(120)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                 ***   TYPE STATEMENTS   ***
C
      LOGICAL OPENED
C
C     ...................................................................
C
      CHARACTER FILEME*73
      CHARACTER LA*1
      CHARACTER NEWCRD*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER NTPR*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 1000 /
      DATA ICB /   59 /
      DATA ICC /   60 /
      DATA ICD /   82 /
C
C     ==================================================================
C
C     CHECK THE FIRST NAME FOR SPECIAL NAMES ...
C        OMNITAB, FORMAT, NOTE, NOTE, HEAD, AND TITLES.
C
      INST = IZERO
      IF (INT.EQ.(-IONE)) GO TO 270
C
C     OMNITAB
C
      IF (NAME(1).EQ.NL(1) .AND. NAME(2).EQ.NL(2)) GO TO 10
      IF (LETSGO.GE.IZERO) GO TO 50
      CALL ERROR (266)
      LETSGO = IZERO
      INST = IONE
      GO TO 50
C
C     ..................................................................
C
C     IF NOT THE FIRST OMNITAB CARD, WRITE EOF RECORD.
C
  10  IF (MODE.LT.ITHRE) GO TO 20
      CALL ERROR (5)
      RETURN
C
  20  IF (LETSGO.NE.(-IONE)) WRITE (ISCRT,280)
      LETSGO = LETSGO + IONE
      IF (LETSGO.LE.IZERO .OR. IFG.EQ.IZERO) GO TO 40
      IF (ITEKSW.EQ.IZERO) CALL PLOTS (RC(1),ICA,NTPE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT (RZERO,RZERO,999)
C
C
  40  CALL SETUP (LETSGO)
      L1    = 14
      L2    = 4
      CALL KEYBRD
      L2 = LANGP
      IF (LANGC.NE.L2) INST = ITHRE
      IF (LANGC.NE.L2) L1   = 34
      RETURN
C
C     FINISH
C
  50  IF (NAME(1).NE.NL(3) .OR. NAME(2).NE.NL(4)) GO TO 70
      IF (MODE.NE.IFOUR) GO TO 60
      CALL ERROR (5)
      RETURN
C
  60  MODE = IONE
      CALL OUTPUT
      RETURN
C
C     ..................................................................
C
C     FORMAT
C
  70  IF (MODE.NE.IFOUR) CALL OUTPUT
      IF (NAME(1).NE.NL(11) .OR. NAME(2).NE.NL(12)) GO TO 80
      CALL XFORMT
      GO TO 240
C
C     NOTE
C
  80  IF (NAME(1).NE.NL(7) .OR. NAME(2).NE.NL(8)) GO TO 120
      K = KARD(KRDPOS)
      IF (K.EQ.IONE .OR. K.EQ.ITWO) GO TO 90
      IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)
      LX = KRDEND + ITWO
      IF (LWIDE+KRDPOS-IONE.LT.KRDEND) LX = LWIDE + KRDPOS - IONE
      WRITE (IPRINT,290) (NEWCRD(I-2),I=KRDPOS,LX)
      LNCNT = LNCNT + IONE
      GO TO 240
C
C     NOTE1 AND NOTE2.
C
  90  MA = KRDPOS + ICC
      KRDPOS = KRDPOS + IONE
      IF (MA.GT.ICD) MA = ICD
      MB = (K-IONE)*ICC + IONE
      MC = MB + ICB
      IF (K.NE.IONE .AND. K.NE.ITWO) GO TO 240
      DO 100 I=MB,MC
        NTPR(I) = LA(45)
 100  CONTINUE
C
      I = MB
      DO 110 J=KRDPOS,MA
        NTPR(I) = NEWCRD(J-2)
        I = I + IONE
 110  CONTINUE
      GO TO 240
C
C     HEAD
C
 120  IF (NAME(1).NE.NL(15) .OR. NAME(2).NE.NL(16)) GO TO 130
      CALL XHEAD
      GO TO 240
C
C     TITLES.   TITLEX = TITLE5, TITLEY = TITLE6
C
 130  IF (NAME(1).NE.NL(9)) GO TO 180
C
C     CHECK NAME TITLE
C
      IF (NAME(2).EQ.NL(10)) GO TO 140
C
C     CHECK TITLEX, TITLEY
C
      K = IFIVE
C
      IF (NAME(2).NE.NL(17) .AND. NAME(2).EQ.NL(18)) GO TO 150
      IF (NAME(2).NE.NL(17) .AND. NAME(2).NE.NL(18)) GO TO 180
      K = 6
      GO TO 150
C
 140  K = KARD(KRDPOS)
      IF (K.GE.IONE .AND. K.LE.IFOUR) GO TO 150
      CALL ERROR (209)
      K = IONE
 150  MM = MIN0 (KRDPOS+ICB,KRDEND+IONE)
      IF (NERROR.NE.IZERO) RETURN
      DO 160 I=1,60
        ITLE(I,K) = LA(45)
 160  CONTINUE
C
C     STORE PROPER TITLE AND COUNT NUMBER OF CHARACTERS UP TO LAST
C     NON-BLANK CHARACTER IN TITLE.
C     IF K = 1, 2, 3, OR 4 STORE COUNT IN NCHTIT(.).
C
      I     = IONE
      IBLKC = IZERO
      DO 170 MA=KRDPOS,MM
        ITLE(I,K) = NEWCRD(MA-1)
        IF (ITLE(I,K).NE.LA(45)) IBLKC = I
        I = I + IONE
 170  CONTINUE
      IF (K.LT.IFIVE) NCHTIT(K) = IBLKC
      GO TO 240
C
C     FILE
C
 180  IF (NAME(1).NE.NL(20) .OR. NAME(2).NE.NL(21)) GO TO 210
      INFILE = 51
 190  INQUIRE (INFILE, OPENED=OPENED)
      IF (OPENED) THEN
C
C       UNIT IS BEING USED.  FIND ANOTHER UNIT TO USE.
C
        INFILE = INFILE + IONE
        IF (INFILE.GT.IHRD) THEN
C
C         NO UNITS AVAILABLE
C
          CALL ERROR (47)
          RETURN
C
        ELSE
          GO TO 190
        ENDIF
      END IF
C
C     AVAILABLE UNIT IS FOUND. CHECK TO SEE IF MORE THAN 10 FILES
C     HAVE BEEN OPENED.
C
      IF (IFILE.EQ.ITEN) THEN
        CALL ERROR(47)
        RETURN
        END IF
C
C     PICK UP NAME OF FILE FROM FILE INSTRUCTION.
C
 200  IF (KARD(KRDPOS).EQ.46) THEN
C
C       NO FILE NAME GIVEN.
C
        CALL ERROR (47)
        RETURN
C
C     LOCATE NAME OF FILE.
C
      ELSE
        KRDPOS = KRDPOS + IONE
        IF (KARD(KRDPOS-1).EQ.44) GO TO 200
      END IF
C
C     SAVE UNIT NUMBER
C
      IFILE = IFILE + IONE
      NUNIT(IFILE) = INFILE
      INUNIT       = INFILE
C
      WRITE (ISCRT,290) (NEWCRD(I-3),I=KRDPOS,80)
      BACKSPACE ISCRT
      READ (ISCRT,300) FILEME
      BACKSPACE ISCRT
C
C     OPEN NEW FILE.
C
      OPEN (UNIT =INUNIT, FILE = FILEME)
      RETURN
C
C         STOP
C
 210  IF (NAME(1).NE.NL(5) .OR. NAME(2).NE.NL(6)) GO TO 260
      IF (MODE.NE.ITWO .OR. NERROR.NE.IZERO) GO TO 220
      BACKSPACE ISCRT
      CALL ERROR (252)
      CALL OUTPUT
 220  WRITE (ISCRT,280)
      IF (IFG.EQ.IZERO) GO TO 230
      IF (ITEKSW.EQ.IZERO) CALL PLOTS (RC(1),ICA,NTPE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT (RZERO,RZERO,999)
      BACKSPACE NTPE
C
 230  CALL XSTOP
      REWIND ISCRT
C
      INST = ITWO
      RETURN
C
C     ..................................................................
C
 240  IF (MODE.GE.ITHRE) CALL ERROR (202)
      IF (MODE.NE.ITWO .OR. NERROR.NE.IZERO) GO TO 250
      BACKSPACE ISCRT
      CALL ERROR (252)
      CALL OUTPUT
 250  IF (MODE.NE.ITHRE) MODE = IONE
      RETURN
C
C     ..................................................................
C
 260  INST = IONE
      RETURN
C
C     ..................................................................
C
C     THIS PATH IS ONLY EXECUTED WHEN OMNITAB IS 1ST INTIATED.
C
 270  CALL SETUP (LETSGO)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 280  FORMAT (1HZ,83X)
 290  FORMAT (    80A1)
 300  FORMAT (    A64)
C
C     ==================================================================
C
      END
*SPLITP
      SUBROUTINE SPLITP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SPLITP V 7.00  5/28/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     FORM OF INSTRUCTION IS ...
C     SPLIT PLOT FOR DATA (C) REPS (C) WHOLE PLOTS (C) SPLIT PLOTS (C)
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
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DF(7)
      REAL             CMIN, CMAX, R, RNGMID
      REAL             S, W
      REAL             FDIV, FDPCON
      REAL             RDELTA
C
      DOUBLE PRECISION D(7)
      DOUBLE PRECISION DR, DW, DS
      DOUBLE PRECISION RSS, WSS, SSS, RWBSS, WSBSS, CF, TSUMSQ
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA RDELTA / 0.0001 /
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NRMAX.LE.IZERO) CALL ERROR ( 9)
      IF (NARGS.NE.IFOUR) CALL ERROR (10)
C
      CALL ADRESS (1,IY)
      CALL ADRESS (2,IR)
      CALL ADRESS (3,IW)
      CALL ADRESS (4,IS)
C
      IF (IY.LT.IZERO) CALL ERROR (20)
      IF (IR.LT.IZERO) CALL ERROR (20)
      IF (IW.LT.IZERO) CALL ERROR (20)
      IF (IS.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     COMPUTE R = NUMBER OF REPLICATES
C             W = NUMBER OF WHOLE PLOTS
C             S = NUMBER OF SPLIT PLOTS.
C
      R = RC(IR)
      W = RC(IW)
      S = RC(IS)
C
      IRSUB = IR
      IWSUB = IW
      ISSUB = IS
C
      DO 10 I=1,NRMAX
        R = AMAX1 (R,RC(IRSUB))
        W = AMAX1 (W,RC(IWSUB))
        S = AMAX1 (S,RC(ISSUB))
        IRSUB = IRSUB + IONE
        IWSUB = IWSUB + IONE
        ISSUB = ISSUB + IONE
  10  CONTINUE
C
      DR = R
      DW = W
      DS = S
C
      NTOTAL = R * W * S + RDELTA
      IF (NTOTAL.NE.NRMAX) CALL ERROR (3)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     COMPUTE MIDRANGE.
C
      K    = IY
      CMIN = RC(K)
      CMAX = RC(K)
      DO 20 I=1,NRMAX
        IF (RC(K).LT.CMIN) CMIN = RC(K)
        IF (RC(K).GT.CMAX) CMAX = RC(K)
        K = K + IONE
  20  CONTINUE
      RNGMID = FDPCON ( FDDIV (DBLE(CMIN)+DBLE(CMAX),DTWO,IND) )
C
C     SUBTRACT MIDRANGE FROM Y IN WORKSHEET.
C
      K = IY
      DO 30 I=1,NRMAX
        RC(K) = RC(K) - RNGMID
        K = K + IONE
  30  CONTINUE
C
C     COMPUTE CORRECTION FACTOR.
C
      DO 40 I=1,NRMAX
        A(I) = IONE
  40  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,IONE,CF)
      CF = FDDIV (CF,DR*DW*DS,IND)
C
C     COMPUTE TOTAL SUM OF SQUARES.
C
      DO 50 I=1,NRMAX
        A(I) = I
  50  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,NRMAX,TSUMSQ)
      TSUMSQ = TSUMSQ - CF
C
C     COMPUTE REPLICATES X WHOLE PLOT BASIC SUM OF SQUARES.
C
      IRW   = R * W + RDELTA
      ISUBR = IR
      ISUBW = IW
      DO 60 I=1,NRMAX
        A(I) = RC(ISUBW) + W * (RC(ISUBR)-RONE) 
        ISUBR = ISUBR + IONE
        ISUBW = ISUBW + IONE
  60  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,IRW,RWBSS)
      RWBSS = FDDIV (RWBSS,DS,IND)
C
C     COMPUTE WHOLE X SPLIT PLOT BASIC SUM OF SQUARES.
C
      IWS   = W * S + RDELTA
      ISUBW = IW
      ISUBS = IS
      DO 70 I=1,NRMAX
        A(I) = RC(ISUBS) + S * (RC(ISUBW)-RONE) 
        ISUBW = ISUBW + IONE
        ISUBS = ISUBS + IONE
  70  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,IWS,WSBSS)
      WSBSS = FDDIV (WSBSS,DR,IND)
C
      INTR  = R + RDELTA
      IRSUB = IR
      DO 80 I=1,NRMAX
         A(I) = RC(IRSUB) 
         IRSUB = IRSUB + IONE
  80  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,INTR,RSS)
      RSS = FDDIV (RSS,DW*DS,IND)
C
      INTW = W + RDELTA
      IWSUB = IW
      DO 90 I=1,NRMAX
         A(I) = RC(IWSUB) 
         IWSUB = IWSUB + IONE
  90  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,INTW,WSS)
      WSS = FDDIV (WSS,DR*DS,IND)
C
      INTS = S + RDELTA
      ISSUB = IS
      DO 100 I=1,NRMAX
         A(I) = RC(ISSUB) 
         ISSUB = ISSUB + IONE
 100  CONTINUE
      CALL UCSUMS (RC(IY),NRMAX,A,INTS,SSS)
      SSS = FDDIV (SSS,DR*DW,IND)
C
C     COMPUTE SUMS OF SQUARES.
C
      D(1) = RSS - CF
      D(2) = WSS - CF
      D(3) = RWBSS - RSS - WSS + CF
      D(4) = SSS - CF
      D(5) = WSBSS - WSS - SSS + CF
      D(6) = TSUMSQ - D(1) - D(2) - D(3) - D(4) - D(5)
      D(7) = TSUMSQ
C
C     COVERT SUMS OF SQUARES TO SINGLE PRECISION.
C
      DO 110 I=1,7
        A(I) = FDPCON ( D(I) )
 110  CONTINUE
C
C     COMPUTE DEGREES OF FREEDOM.
C
      DF(1) = R - RONE
      DF(2) = W - RONE
      DF(3) = DF(1) * DF(2)
      DF(4) = S - RONE
      DF(5) = DF(2) * DF(4)
      DF(6) = DF(1) * W * DF(4)
      DF(7) = R * W * S - RONE
C
C     COMPUTE MEAN SQUARES.
C
      A( 8) = FDIV (A(1),DF(1),IND)
      A( 9) = FDIV (A(2),DF(2),IND)
      A(10) = FDIV (A(3),DF(3),IND)
      A(11) = FDIV (A(4),DF(4),IND)
      A(12) = FDIV (A(5),DF(5),IND)
      A(13) = FDIV (A(6),DF(6),IND)
C
C     COMPUTE F STATISTICS.
C
      A(14) = FDIV (A( 8),A( 9),IND)
      A(15) = FDIV (A(11),A(13),IND)
      A(16) = FDIV (A(12),A(13),IND)
C
C     PRINT SPLIT PLOT ANALYSIS OF VARIANCE.
C
      CALL OUTSPA (DF)
C
C     ADD BACK MIDRANGE TO Y IN WORKSHEET.
C
      K = IY
      DO 120 I=1,NRMAX
        RC(K) = RC(K) + RNGMID
        K = K + IONE
 120  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SPLOTS
      SUBROUTINE SPLOTS
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SPLOTS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR EXECUTING INSTRUCTION STATPLOTS OF COLUMN (C).
C        CALLS STAPLT TO PRODUCE FOUR STATISTICAL PLOTS ON ONE PAGE.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.IONE) CALL ERROR (10)
      IF (NRMAX.GT.IDIV (NRC,ITWO,IND)) CALL ERROR (23)
      CALL ADRESS (IONE,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (NRMAX.GE.IFIVE) GO TO 10
      NRM = IFIVE
      CALL ERROR (257)
      RETURN
C
C     ..................................................................
C
  10  K = J + IONE
      DO 30 I=2,NRMAX
        IF (RC(K)-RC(J)) 40,20,40
  20    K = K + IONE
  30  CONTINUE
C
      CALL ERROR (248)
      RETURN
C
C     ..................................................................
C
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      CALL STAPLT (RC(J),NRMAX)
      RETURN
C
C     ==================================================================
C
      END
*STAERR
      SUBROUTINE STAERR (IWT,ISTORE,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STAERR V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ERROR CHECKING FOR STATISTICAL ANALYSIS INSTRUCTIONS.
C
C     OUTPUT ...
C
C       IWT     = 1, IF NO WEIGHTS ARE GIVEN.
C               = 2, IF  WEIGHTS ARE GIVEN.
C
C       ISTORE  = 1, NO STORAGE IS REQUIRED.
C               = 2, STORAGE IS REQUIRED.
C
C       IND     = 0, NO INFORMATIVE DIAGNOSTICS AND CONTINUE EXECUTION.
C               = 1, INFORMATIVE DIAG. AND DO NOT CONTINUE EXECUTION.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -           1967.
C                   CURRENT VERSION - FEBRUARY, 1990.
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
C     ==================================================================
C
      IND = IZERO
C
C     IF COMMAND IS SSTATIS AND NO STORAGE COLUMN IS GIVEN. SET IND = 1.
C
      IF (L2.EQ.IONE .OR. NARGS.NE.IONE) GO TO 10
      CALL ERROR (236)
      IND = IONE
      RETURN
C
C     ..................................................................
C
C     IWT = 1, IF NO WEIGHTS ARE GIVEN
C     IWT = 2, IF WEIGHTS ARE GIVEN
C
  10  IWT    = IONE
      ISTORE = IONE
      IF (NARGS.EQ.IONE) GO TO 50
      IF (NARGS.GT.ITHRE) GO TO 30
      IF (NARGS.EQ.ITWO) GO TO 20
      IF (IARGS(NARGS).GT.IZERO) GO TO 20
      NARGS = NARGS - IONE
      IWT   = ITWO
      GO TO 40
  20  IF (IARGS(NARGS) + ITHRE.LE.NCOL) GO TO 30
      CALL ERROR (11)
      IND = IONE
      RETURN
C
C     ..................................................................
C
  30  ISTORE = ITWO
      IF (MOD(NARGS,ITHRE).EQ.IZERO) IWT = ITWO
C
  40  IF (NARGS.NE.ITWO .AND. NARGS.NE.ITHRE .AND. NARGS.NE.IFIVE
     1                  .AND. NARGS.NE.6) CALL ERROR (10)
  50  J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
      CALL CHKCOL
      IF (NRMAX*IFOUR.LE.NS-IHRD) RETURN
      CALL ERROR (214)
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
**STAPLT
      SUBROUTINE STAPLT (X,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STAPLT V 7.00  4/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS ROUTINE PRODUCES THE FOLLOWING 4 PLOTS--ALL ON THE SAME PAGE.
C          1) DATA PLOT - X(I) VERSUS I
C          2) AUTOREGRESSION PLOT - X(I) VERSUS X(I-1)
C          3) HISTOGRAM
C          4) NORMAL PROBABILITY PLOT
C
C     THESE PLOTS ARE USEFUL IN A UNIVARIATE ANALYSIS.
C
C     THE INPUT TO THIS ROUTINE IS THE SINGLE PRECISION VECTOR X OF
C        OBSERVATIONS, AND THE INTEGER VALUE N (= SAMPLE SIZE).
C
C     THE OUTPUT FROM THIS ROUTINE IS 1 PAGE WITH A DATA PLOT,
C        AN AUTOREGRESSION PLOT, A HISTOGRAM, AND A NORMAL PROBABILITY
C        PLOT ON A SINGLE PAGE.
C
C     THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS PROGRAM UNIT IS 6250.
C
C     PROGRAM UNITS NEEDED ARE SORT, UNIMED, NORPPF, SCRAWL AND RFORMT.
C
C     BASED UPON SUGGESTIONS OF DAVID HOGBEN, CODE WAS ORIGINALLY
C        WRITTEN BY JAMES J. FILLIBEN FOR DATAPAC, NOVEMBER, 1974.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1975.
C                   CURRENT VERSION -   APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IC(6)
      DIMENSION NSTORE(100)
      DIMENSION ISTORE(131)
      DIMENSION JGRAPH(102)
      DIMENSION KB(4), KD(4), KS(4), KT(4), KW(4)
      DIMENSION IGRAPH(21,51)
C
C        X                 = LA(34),
C        PERIOD            = LA(38),   MINUS          = LA(39),
C        PLUS              = LA(40),   ASTERISK       = LA(41),
C        RIGHT PARENTHESIS = LA(43),   COMMA          = LA(44),
C        BLANK             = LA(45),   EQUALS         = LA(46).
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
      REAL             X(*)
      REAL             ATEMP(1), CWIDTH(1), SV(6)
      REAL             XX(24), YLABLE(84)
      REAL             AI, AN, PPF, PPRXY
      REAL             S, SUM, SUM1, SUM2
      REAL             XBAR, Z
      REAL             FDIV, FSQRT
      REAL             SPCA, SPCB, SPCC, SPCD
C
C     ...........................................................
C
      CHARACTER        LA*1
      CHARACTER*1      IC, NSTORE
      CHARACTER*1      ISTORE, JGRAPH
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     NEEDED CONSTANTS.
C        21 IS THE NUMBER OF PLOTTING POSITIONS ON THE Y-AXIS
C        51 IS THE NUMBER OF PLOTTING POSITIONS ON THE X-AXIS
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     LBLEY  = NUMBER OF CHARACTERS ALLOWED FOR PRINTING VERTICAL SCALE
C
      DATA LBLEY / 11 /
C
      DATA SPCA /    1.5  /
      DATA SPCB /   26.0  /
      DATA SPCC / 1000.0  /
      DATA SPCD /    0.2  /
C
C     ==================================================================
C
      NSUBY2 = IDIV (NRC,ITWO,IND) + IONE
      NSUBSV = IONE
      NSUBNS = IONE
      NSUBXX = IONE
      NSUBXI = NSUBXX
      NSUB20 = NSUBXX + 4
      NSUB40 = NSUBXX + 8
      NSUB60 = NSUBXX + 12
      NSUB80 = NSUBXX + 16
      NSUBXA = NSUBXX + 20
      NSUBIS = IONE
      NSUBJG = IONE
      LBLEY1 = LBLEY + IONE
C
C     BLANK OUT THE GRAPH.
C
      DO 20 I=1,LYAXIS
        DO 10 J=1,LXAXIS
          IGRAPH(I,J) = IZERO
  10    CONTINUE
  20  CONTINUE
C
C     ..................................................................
C
C     PRODUCE THE FIRST PLOT (UPPER LEFT)--X(I) VERSUS I.
C
C     DETERMINE THE VERTICAL AXIS VECTOR Y2, THE HORIZONTAL AXIS VECTOR,
C        X2, AND THE PLOT SAMPLE SIZE N2 FOR THIS PARTICULAR PLOT.
C
      N2     = N
      ISUBY2 = NSUBY2
      DO 30 I=1,N2
        A(ISUBY2) = X(I)
        A(I)      = I
        ISUBY2     = ISUBY2 + IONE
  30  CONTINUE
C
      IPLOT = IONE
      CALL STAPTG (IGRAPH,IPLOT,LBLEY1,N2,XX(NSUBXI),XX(NSUBXA),
     1             XX(NSUB20),XX(NSUB40),XX(NSUB60),XX(NSUB80),
     2           KB(1),KD(1),KS(1),KT(1),KW(1),
     3           JGRAPH,YLABLE)
C
C     ..................................................................
C
C     PRODUCE THE SECOND PLOT (UPPER RIGHT)--X(I) VERSUS X(I-1).
C
C     DETERMINE THE VERTICAL AXIS VECTOR Y2, THE HORIZONTAL AXIS VECTOR,
C        X2, AND THE PLOT SAMPLE SIZE N2 FOR THIS PARTICULAR PLOT.
C
      N2     = N - IONE
      ISUBY2 = NSUBY2
      DO 40 I=1,N2
        IP1        = I + IONE
        A(ISUBY2) = X(IP1)
        A(I)      = X(I)
        ISUBY2     = ISUBY2 + IONE
  40  CONTINUE
C
      IPLOT = ITWO
      CALL STAPTG (IGRAPH,IPLOT,LBLEY1,N2,XX(NSUBXI),XX(NSUBXA),
     1             XX(NSUB20),XX(NSUB40),XX(NSUB60),XX(NSUB80),
     2           KB(1),KD(1),KS(1),KT(1),KW(1),
     3           JGRAPH,YLABLE)
C
C     ..................................................................
C
C     PRODUCE THE THIRD PLOT (LOWER LEFT)-A HISTOGRAM.
C
C     COMPUTE THE SAMPLE MEAN AND SAMPLE STANDARD DEVIATION.
C
      N2 = 51
      AN = N
      CALL SUMMAL (X(1),N,SUM)
      IF (N.EQ.IONE) SUM = X(1)
      XBAR = FDIV (SUM,AN,NF)
      CALL SUMMAL (X(1),IZERO,SUM)
      DO 50 I=1,N
        ATEMP(1) = (X(I)-XBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM)
  50  CONTINUE
      CALL SUMMAL (ATEMP,IONE,SUM)
      S = FSQRT (FDIV(SUM,AN-RONE,NF))
C
C     FORM THE BASIC FREQUENCY TABLE WHICH CORRESPONDS TO A HISTOGRAM
C        WITH 51 CLASSES AND A CLASS WIDTH OF TWO TENTHS OF
C        A SAMPLE STANDARD DEVIATION.
C
      ISUBY2 = NSUBY2
      DO 60 I=1,51
        A(ISUBY2) = RZERO
        ISUBY2     = ISUBY2 + IONE
  60  CONTINUE
C
      NUMOUT = IZERO
      DO 80 I=1,N
        Z = FDIV (X(I)-XBAR,S,NF)
        IF (ABS(Z).LE.RFIVE) GO TO 70
        NUMOUT = NUMOUT + IONE
  70    MT     = RFIVE * (Z+RFIVE) + SPCA
        ISUBY2 = NSUBY2 + MT - IONE
        IF (MT.GE.IONE .AND. MT.LE.LXAXIS) A(ISUBY2) = A(ISUBY2)+RONE
  80  CONTINUE
C
      ISUBY2 = NSUBY2 - IONE
      DO 90 I=1,LXAXIS
        ISUBY2     = ISUBY2 + IONE
        IF (A(ISUBY2).GT.RZERO) GO TO 90
        A(ISUBY2) = -SPCC
  90  CONTINUE
C
      DO 100 I=1,LXAXIS
        AI    = I
        A(I) = XBAR + S * FDIV (AI-SPCB,RFIVE,NF)
 100  CONTINUE
C
      CWIDTH(1) = SPCD * S
C
      IPLOT = ITHRE
      CALL STAPTG (IGRAPH,IPLOT,LBLEY1,N2,XX(NSUBXI),XX(NSUBXA),
     1             XX(NSUB20),XX(NSUB40),XX(NSUB60),XX(NSUB80),
     2           KB(1),KD(1),KS(1),KT(1),KW(1),
     3           JGRAPH,YLABLE)
C
C     ..................................................................
C
C     PRODUCE THE FOURTH PLOT (LOWER RIGHT), A NORMAL PROBABILITY PLOT.
C
C     DETERMINE THE VERTICAL AXIS VECTOR Y2, THE HORIZONTAL AXIS VECTOR,
C        X2, AND THE PLOT SAMPLE SIZE N2 FOR THIS PARTICULAR PLOT.
C
      N2     = N
      DO 110 I=1,N
        A(I) = X(I)
 110  CONTINUE
C
      CALL SORT (A(1),A(NSUBY2),N,IZERO)
      ISUBY2 = NSUBY2
      DO 120 I=1,N
        A(ISUBY2) = A(I)
        ISUBY2     = ISUBY2 + IONE
 120  CONTINUE
C
      CALL SSCRWL (A(1),N2,IC(1),SV(NSUBSV),IND)
      ISC    = IZERO
      ISUBNS = NSUBNS
      DO 130 I=1,100
        NSTORE(ISUBNS) = LA(45)
        ISUBNS         = ISUBNS + IONE
 130  CONTINUE
C
      ISUBNS = NSUBNS - IONE
      ISUBSV = NSUBSV + IONE
      DO 140 IS=2,6
        ISUBNS           = ISUBNS + IONE
        ISC              = ISC + IONE
        NSTORE(ISUBNS+1) = IC(IS)
        NSTORE(ISUBNS+2) = LA(46)
        NSTORE(ISUBNS+3) = LA(45)
        CALL RFORMT (0,ISIGD,SV(ISUBSV),A(NSUBY2),1,12,MW,MND,
     1              NSTORE(ISUBNS+4),IRF)
        CALL RFORMT (1,ISIGD,A(NSUBY2),SV(ISUBSV),0,0,MW,MND,
     1  NSTORE(ISUBNS+4),IRF)
        ISUBSV         = ISUBSV + IONE
        ISC            = MW + ISC + IFOUR
        ISUBNS         = MW + ISUBNS + IFOUR
        NSTORE(ISUBNS) = LA(44)
        IF (ISC.LE.36) IS1 = ISC
        IF (IS.EQ.6) NSTORE(ISUBNS) = LA(43)
 140  CONTINUE
C
      IS2 = IS1 + IONE
      CALL SUNIMD (N,A(1))
      DO 150 I=1,N
        CALL SNRPPF (A(I),PPF,IRF)
        A(I) = PPF
 150  CONTINUE
C
C     COMPUTE CORRELATION COEFFICIENT.
C
      CALL SUMMAL (A(1),IZERO,SUM1)
      ISUBY2 = NSUBY2
      DO 160 I=1,N
        ATEMP(1) = A(I) * (A(ISUBY2)-XBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM1)
        ISUBY2     = ISUBY2 + IONE
 160  CONTINUE
C
      CALL SUMMAL (ATEMP,IONE,SUM1)
      CALL SUMMAL (A(1),IZERO,SUM2)
      DO 170 I=1,N
        ATEMP(1) = A(I)**2
        CALL SUMMAL (ATEMP,-IONE,SUM2)
 170  CONTINUE
C
      CALL SUMMAL (ATEMP,IONE,SUM2)
      PPRXY = FDIV (SUM1,FSQRT(FLOAT(N-IONE))*S*FSQRT(SUM2),NF)
C
      IPLOT = IFOUR
      CALL STAPTG (IGRAPH,IPLOT,LBLEY1,N2,XX(NSUBXI),XX(NSUBXA),
     1             XX(NSUB20),XX(NSUB40),XX(NSUB60),XX(NSUB80),
     2           KB(1),KD(1),KS(1),KT(1),KW(1),
     3           JGRAPH,YLABLE)
C
C     ..................................................................
C
      CALL STAPTT (X,XX(NSUBXX),XBAR,PPRXY,IGRAPH,ISC,IS1,IS2,N2,S,
     1             ISTORE(NSUBIS),JGRAPH(NSUBJG),NSTORE,
     2             SV,KB,KD,KS,KT,KW,YLABLE)
      CALL STAPTB (IGRAPH,ISTORE(NSUBIS),JGRAPH(NSUBJG),ISC,IS1,IS2,N,
     1             NUMOUT,N2,CWIDTH,PPRXY,S,X,XBAR,XX(NSUBXX),NSTORE,
     2             SV,KB,KD,KS,KT,KW,YLABLE)
      RETURN
C
C     ==================================================================
C
      END
*STAPRT
      SUBROUTINE STAPRT (DELX,ND,NDIFFM,NXCOL,NZW,IWT,IXNM1,NXWT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STAPRT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE THE PRINTING FOR STATISTICAL ANALYSIS INSTRUCTIONS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -        1967.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION  IA( 120), IIA(2), IB(10), ID(10), IN(6)
      DIMENSION  ITO(4)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             DELX
      REAL             R(3)
      REAL             ABSTA, T, TA, Y
      REAL             FDIV, FSQRT
      REAL             SPCA
C
C     ..................................................................
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER*1      IA, ITO
      CHARACTER        IN*3
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IN(1), IN(2), IN(3), IN(4), IN(5), IN(6) /
     1     '(DI', 'STR', 'IBU', 'TIO', 'N-F', 'REE' /
C
      DATA NXLSD / 12 /
C
      DATA NW1 / 20 /
      DATA NW2 / 35 /
C
      DATA SPCA / 0.0001 /
C
C     ==================================================================
C
      LSIZSA = IDIV (NS-IHRD,IFOUR,IXND)
      LSUBSA = LSIZSA + 101
      LW     = IONE
C
C     START AUTOMATIC PRINTING.
C
      CALL PAGE (IFOUR)
      ITO(1) = LA(45)
      ITO(2) = LA(30)
      ITO(3) = LA(25)
      ITO(4) = LA(45)
      IIA(1)  = NXCOL
      IF (IWT.EQ.ITWO) GO TO 10
C
C     PRINT TITLE WHEN NO WEIGHTS SPECIFIED.
C
      CALL HEADS (IIA,IONE,IZERO,IONE)
      WRITE (IPRINT,340) (LHEAD(I),I=1,12)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,610)
      WRITE (IPRINT,350) NZW, NDIFFM
      GO TO 20
C
C     PRINT TITLE WHEN WEIGHTS ARE SPECIFIED.
C
  10  IIA(2) = NXWT
      CALL HEADS (IIA,ITWO,IZERO,IONE)
      WRITE (IPRINT,340) (LHEAD(I),I=1,12)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,610)
      WRITE (IPRINT,360) (LHEAD(I),I=13,24), NRMAX, NZW
      WRITE (IPRINT,370)
C
C     PRINT FREQUENCY DISTRIBUTION.
C
  20  DO 30 I=1,10
        IB(I) = A(I+50)
  30  CONTINUE
C
      ARGS(1) = DELX
      NND = ND
      IT  = 7
      NDT = IZERO
      CALL MINNW (ARGS,1,NND,NXLSD+IONE,IA,IZERO,NWID,NDEC,NWT,NDT)
      CALL RFORMT (11,NND,ARGS,A(1),0,0,NXLSD,NDT,IA(1),IRF)
      IF (NWT.LE.NXLSD) GO TO 40
        IT = IONE
        NND = MIN0 (NND,7)
        CALL RFORMT (0,NND,ARGS,A(1),1,NXLSD,NWT,NDT,IA(1),IRF)
  40  CALL RFORMT (IT,NND,ARGS,DELX,0,0,NWT,NDT,IA(1),IRF)
      WRITE (IPRINT,380) (IA(I),I=1,12), (IB(J),J=1,10)
C
C     PRINT FREQUENCY DISTRIBUTION OF LEAST SIGNIFICANT DIGIT.
C
      ISUBSA = LSIZSA + LSUBSA
      CALL STALSD (ND,NZW,A(ISUBSA),IA,ID,IND)
      IF (IND.EQ.IZERO) WRITE (IPRINT,390) (ID(I),I=1,10)
      IF (IND.NE.IZERO) WRITE (IPRINT,610)
      IF (IND.NE.IZERO) WRITE (IPRINT,610)
C
      DO 50 I=1,10
        A(I+80) = A(I+50)
  50  CONTINUE
C
      DO 60 I=1,13
        A(I+50) = A(I+60)
  60  CONTINUE
C
      CALL RFORMT (0,ND,A,R(1),63,19,NWID,NDEC,IA(1),IRF)
C
      J = 63
      DO 70 I=1,13
        A(J+10) = A(J)
        J       = J - IONE
  70  CONTINUE
C
      DO 80 I=1,10
        A(I+50) = A(I+80)
  80  CONTINUE
      NB = 20 - NWID
C
C     PRINT MEASURES OF LOCATION.
C
      J = IONE
      DO 90 I=4,7
        K = I
        IF (IWT.EQ.IONE .AND. I.EQ.IFOUR) K = K - IONE
        IF (IWT.EQ.ITWO .AND. I.EQ.7)     K = K + IONE
        CALL RFORMT (1,ND,R,A(K),NB,0,NWID,NDEC,IA(J),IRF)
        J = J + 20
  90  CONTINUE
      WRITE (IPRINT,400) (IA(I),I=1,80)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,610)
C
C     PRINT MEASURES OF DISPERSION.
C
      J = IONE
      DO 100 I=9,14
        K = I
        IF (I.EQ.ITEN) K = 14
        IF (I.EQ.13) K = 61
        IF (I.EQ.14) K = 13
        CALL RFORMT (1,ND,R,A(K),NB,0,NWID,NDEC,IA(J),IRF)
        J = J + 20
 100  CONTINUE
      WRITE (IPRINT,410) (IA(I),I=1,120)
      IF (NCRT.EQ.IONE) CALL PAGE(4)
C
C     PRINT STANDARD DEVIATION OF MEAN.
C
      CALL RFORMT (1,ND,R,A(10),NB,0,NWID,NDEC,IA(1),IRF)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,610)
      WRITE (IPRINT,420) (IA(I),I=1,20)
C
C     PRINT TREND STATISTICS.
C
      CALL RFORMT (1,ND,R,A(19),NB,0,NWID,NDEC,IA(1),IRF)
      CALL RFORMT (1,ND,R,A(62),NB,0,NWID,NDEC,IA(21),IRF)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,610) 
      WRITE (IPRINT,430) (IA(I),I=1,20), A(22), (IA(J),J=21,40), A(63)
C
C     PRINT OTHER TESTS FOR NON-RANDOMNESS.
C
      K = 20 - NDEC - IONE
C
C          1.   NUMBER OF RUNS UP AND DOWN, Z VALUE.
C
      CALL RFORMT (11,ND,R,A(23),0,0,NW1,IZERO,IA(1),IRF)
      CALL RFORMT (9,ND,R,A(23),0,0,K,IZERO,IA(1),IRF)
      A(98) = FDIV (A(23)-A(24),A(25),IND)
C
C          2.   MEAN SQUARE SUCCESSIVE DIFFERENCE.
C
      CALL RFORMT (1,ND,R,A(26),NB,0,NWID,NDEC,IA(21),IRF)
C
C          3.   MS SUCC DIFF/2*VARIANCE, Z VALUE.
C
      T = FDIV (A(27),RTWO,IND)
      CALL RFORMT (1,ND,R,T,NB,0,NWID,NDEC,IA(41),IRF)
      Y = FDIV (RTWO*A(2)-RTHRE-A(37),RTWO*A(2)*(A(2)-RONE),IND)
      TA = FDIV (T-RONE,FSQRT(Y),IND)
C
C     DEVIATIONS FROM ARITHMETIC MEAN.
C
C          4.   NUMBER OF - SIGNS, NUMBER OF + SIGNS.
C
      CALL RFORMT (11,ND,R,A(29),0,0,NW1,IZERO,IA(61),IRF)
      CALL RFORMT (9,ND,R,A(29),0,0,K,IZERO,IA(61),IRF)
      KB = A(28) + SPCA
C
C          5.   NUMBER OF RUNS, Z VALUE.
C
      CALL RFORMT (11,ND,R,A(30),0,0,NW1,IZERO,IA(81),IRF)
      CALL RFORMT (9,ND,R,A(30),0,0,K,IZERO,IA(81),IRF)
      A(99) = FDIV (A(30)-A(31),A(32),IND)
C
C          6.   AUTOCORRELATION COEFFICIENT.
C
      CALL RFORMT (1,ND,R,A(64),NB,0,NWID,NDEC,IA(101),IRF)
C
      WRITE (IPRINT,440) (IA(J1),J1=1,20), A(98), (IA(J2),J2=21,40),
     1                   (IA(J3),J3=41,60), TA, (IA(J4),J4=61,80), KB,
     2                   (IA(J5),J5=81,100), A(99), (IA(J6),J6=101,120)
C
C     PRINT CONFIDENCE INTERVALS.
C
      NCI = ITHRE
      IF (NZW.LT.IFIVE) NCI = ITWO
      WRITE (IPRINT,450)
      A(91) = A(15)
      A(92) = A(17)
      A(93) = A(65)
      CALL RFORMT (0,ND,A(91),RC(1),NCI,15,NWS,NDS,IA(1),IRF)
      A(91) = A(16)
      A(92) = A(18)
      A(93) = A(66)
      CALL RFORMT (0,ND,A(91),RC(1),NCI,15,NWT,NDT,IA(1),IRF)
C
C     MEAN.
C
      CALL RFORMT (1,ND,R,A(15),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 110 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 110  CONTINUE
C
      CALL RFORMT (1,ND,R,A(16),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,460) (IA(I),I=1,K)
C
C     MEDIAN.
C
      IF (NZW.LT.IFIVE) GO TO 130
      CALL RFORMT (11,ND,R,A(65),0,0,NW2,IZERO,IA(1),IRF)
      CALL RFORMT (1,ND,R,A(65),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 120 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 120  CONTINUE
C
      CALL RFORMT (1,ND,R,A(66),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,470) (IA(I),I=1,35), (IN(J),J=1,6), LA(43)
C
C     STANDARD DEVIATION.
C
 130  CALL RFORMT (1,ND,R,A(17),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 140 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 140  CONTINUE
C
      CALL RFORMT (1,ND,R,A(18),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,480) (IA(I),I=1,K)
C
C     PRINT TOLERANCE INTERVALS.
C
      WRITE (IPRINT,490)
      A(91) = A(67)
      A(92) = A(69)
      A(93) = A(71)
      CALL RFORMT (0,ND,A(91),RC(1),3,15,NWS,NDS,IA(1),IRF)
      A(91) = A(68)
      A(92) = A(70)
      A(93) = A(72)
      CALL RFORMT (0,ND,A(91),RC(1),3,15,NWT,NDT,IA(1),IRF)
C
C     50 PERCENT COVERAGE.
C
      CALL RFORMT (1,ND,R,A(67),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 150 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 150  CONTINUE
C
      CALL RFORMT (1,ND,R,A(68),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,500) (IA(I),I=1,K)
C
C     95 PERCENT COVERAGE.
C
      CALL RFORMT (1,ND,R,A(69),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 160 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 160  CONTINUE
C
      CALL RFORMT (1,ND,R,A(70),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,510) (IA(I),I=1,K)
C
C     99 PERCENT COVERAGE.
C
      CALL RFORMT (1,ND,R,A(71),0,0,NWS,NDS,IA(1),IRF)
      K = NWS + IONE
      DO 170 I=1,4
        IA(K) = ITO(I)
        K = K + IONE
 170  CONTINUE
C
      CALL RFORMT (1,ND,R,A(72),0,0,NWT,NDT,IA(K),IRF)
      K = K + NWT - IONE
      WRITE (IPRINT,520) (IA(I),I=1,K)
C
      WRITE (IPRINT,530) A(73)
      IF (NCRT.EQ.IONE) CALL PAGE(4)
C
C     PRINT OTHER STATISTICS.
C
      CALL RFORMT (1,ND,R,A(34),NB,0,NWID,NDEC,IA(1),IRF)
      CALL RFORMT (1,ND,R,A(44),NB,0,NWID,NDEC,IA(21),IRF)
      CALL RFORMT (1,ND,R,A(35),NB,0,NWID,NDEC,IA(41),IRF)
      CALL RFORMT (1,ND,R,A(45),NB,0,NWID,NDEC,IA(61),IRF)
      WRITE (IPRINT,540) (IA(I),I=1,80)
      CALL RFORMT (1,ND,R,A(46),NB,0,NWID,NDEC,IA(1),IRF)
      CALL RFORMT (1,ND,R,A(47),NB,0,NWID,NDEC,IA(21),IRF)
      CALL RFORMT (1,ND,R,A(48),NB,0,NWID,NDEC,IA(41),IRF)
      CALL RFORMT (1,ND,R,A(37),NB,0,NWID,NDEC,IA(61),IRF)
      CALL RFORMT (1,ND,R,A(49),NB,0,NWID,NDEC,IA(81),IRF)
      CALL RFORMT (1,ND,R,A(50),NB,0,NWID,NDEC,IA(101),IRF)
      WRITE (IPRINT,550) (IA(I),I=1,120)
      IF (ISBFT.NE.IZERO) GO TO 330
C
C     ..................................................................
C
C     PRINT SECOND PAGE.
C
C     DETERMINE JS, JWID, JDEC, AND JB FOR PRINTING X(I) AND X(J).
C
      JS     = ND + IONE
      JX     = 13 - IWT
 180  JS     = JS - IONE
      ISUBSA = LSIZSA + LSUBSA
      CALL RFORMT (0,JS,A(ISUBSA),A(1),NZW,JX,JWID,JDEC,IA(1),IRF)
      IF (IRF.EQ.6 .AND. JS.GT.IONE) GO TO 180
      JB = JX + IONE - JWID
C
C     DETERMINE KS, KWID, KDEC, AND KB FOR PRINTING X(I) - XBAR.
C
      KS     = ND + IONE
      KX     = 12 - IWT
 190  KS     = KS - IONE
      ISUBSA = ITWO * LSIZSA + LSUBSA
      CALL RFORMT (0,KS,A(ISUBSA),A(1),NZW,KX,KWID,KDEC,IA(1),IRF)
      IF (IRF.EQ.6 .AND. KS.GT.IONE) GO TO 190
      KB = KX + IONE - KWID
C
C     DETERMINE LS, LWID, LDEC, AND LB FOR PRINTING X(J+1) - X(J).
C
      KSUBSA = ITWO * LSIZSA + INT(A(101)+0.5)
      T      = A(KSUBSA+100)
      KSUBSA = ITWO * LSIZSA + INT(A(102)+0.5)
      R(1) = ABS (A(KSUBSA+100) - T)
      IF (R(1).LE.RZERO) R(1) = RONE
      R(2) = R(1)
      DO 200 I=1,IXNM1
        KSUBSA = ITWO * LSIZSA + INT(A(I+101)+0.5)
        TA     = A(KSUBSA+100) - T
        ABSTA = ABS (TA)
        IF (ABSTA.LE.RZERO) ABSTA = RONE
        IF (ABSTA.LT.R(1))  R(1) = ABSTA
        IF (ABSTA.GT.R(2))  R(2) = ABSTA
        T  = A(KSUBSA+100)
 200  CONTINUE
C
      LS = ND + IONE
      LX = 12 - IWT
 210  LS = LS - IONE
      CALL RFORMT (0,LS,R,A(1),2,LX,LWID,LDEC,IA(1),IRF)
      IF (IRF.EQ.6 .AND. LS.GT.IONE) GO TO 210
      LB = LX + IONE - LWID
C
C     DETERMINE MS, NUMBER OF SIGNIFICANT DIGITS FOR WEIGHTS.
C
      IF (IWT.EQ.IONE) GO TO 220
      NX = 13
      LW = IARGS(2)
      CALL RFORMT (0,ND,RC(LW),A(1),NZW,NX,MWID,MDEC,IA(1),IRF)
      MS = ITHRE + NX - MWID
      MS = MAX0 (ITHRE,MS)
      CALL RFORMT (0,MS,RC(LW),A(1),NZW,8,MWID,MDEC,IA(1),IRF)
C
 220  KSUBSA = ITWO * LSIZSA + INT(A(101)+0.5)
      T      = A(KSUBSA+100)
      LINEP  = 50
      LINE   = IZERO
      ISUBSA = LSIZSA + LSUBSA
      JSUBSA = ITWO * LSIZSA + LSUBSA
      KSUBSA = LSUBSA
      DO 290 I=1,IXNM1
        IF (LINEP.LT.50) GO TO 240
        LINEP = IZERO
        CALL PAGE (IFOUR)
        WRITE (IPRINT,560)
        IF (IWT.EQ.IONE) GO TO 230
        WRITE (IPRINT,590)
        GO TO 240
 230    WRITE (IPRINT,570)
 240    K  = ITWO * LSIZSA + INT(A(I+101)+0.5)
        TA = A(K+100) - T
        IF (IWT.EQ.IONE) GO TO 270
 250    IF (RC(LW).NE.RZERO) GO TO 260
        LW = LW + IONE
        GO TO 250
 260    CALL RFORMT (1,JS,A,A(ISUBSA),JB,0,JWID,JDEC,IA(1),IRF)
        CALL RFORMT (1,KS,A,A(JSUBSA),KB,0,KWID,KDEC,IA(13),IRF)
        CALL RFORMT (1,MS,A,RC(LW),0,0,MWID,MDEC,IA(24),IRF)
        CALL RFORMT (1,JS,A,T,JB,0,JWID,JDEC,IA(32),IRF)
        CALL RFORMT (1,LS,A,TA,LB,0,LWID,LDEC,IA(44),IRF)
        WRITE (IPRINT,600) I, (IA(J1),J1=1,12), A(KSUBSA),
     1        (IA(J2),J2=13,31), INT(A(I+100)+0.5), (IA(J3),J3=32,54)
        LW = LW + IONE
        GO TO 280
 270    CALL RFORMT (1,JS,A,A(ISUBSA),JB,0,JWID,JDEC,IA(1),IRF)
        CALL RFORMT (1,KS,A,A(JSUBSA),KB,0,KWID,KDEC,IA(14),IRF)
        CALL RFORMT (1,JS,A,T,JB,0,JWID,JDEC,IA(26),IRF)
        CALL RFORMT (1,LS,A,TA,LB,0,LWID,LDEC,IA(39),IRF)
        WRITE (IPRINT,580) I, (IA(J1),J1=1,13), A(KSUBSA),
     1       (IA(J2),J2=14,25), INT(A(I+100)+0.5), (IA(J3),J3=26,50)
 280    T      = A(K+100)
        LINE   = LINE + IONE
        KSUBSA = KSUBSA + IONE
        JSUBSA = JSUBSA + IONE
        ISUBSA = ISUBSA + IONE
        IF (LINE.NE.ITEN) GO TO 290
        LINE  = IZERO
        LINEP = LINEP + ITEN
        WRITE (IPRINT,610)
 290  CONTINUE
C
      ISUBSA = LSIZSA + NZW
      JSUBSA = ITWO * LSIZSA + NZW
      KSUBSA = ITHRE * LSIZSA + NZW
      IF (IWT.EQ.IONE) GO TO 320
 300  IF (RC(LW).NE.RZERO) GO TO 310
      LW = LW + IONE
      GO TO 300
 310  CALL RFORMT (1,JS,A,A(JSUBSA+100),JB,0,JWID,JDEC,IA(1),IRF)
      CALL RFORMT (1,KS,A,A(KSUBSA+100),KB,0,KWID,KDEC,IA(13),IRF)
      CALL RFORMT (1,MS,A,RC(LW),0,0,MWID,MDEC,IA(24),IRF)
      CALL RFORMT (1,JS,A,T,JB,0,JWID,JDEC,IA(32),IRF)
      WRITE (IPRINT,600) NZW, (IA(J1),J1=1,12), A(ISUBSA+100),
     1   (IA(J2),J2=13,31), INT(A(NZW+100)+0.5), (IA(J3),J3=32,43)
      GO TO 330
 320  CALL RFORMT (1,JS,A,A(JSUBSA+100),JB,0,JWID,JDEC,IA(1),IRF)
      CALL RFORMT (1,KS,A,A(KSUBSA+100),KB,0,KWID,KDEC,IA(14),IRF)
      CALL RFORMT (1,JS,A,T,JB,0,JWID,JDEC,IA(26),IRF)
      WRITE (IPRINT,580) NZW, (IA(J1),J1=1,13), A(ISUBSA+100),
     1     (IA(J2),J2=14,25), INT(A(NZW+100)+0.5),(IA(J3),J3=26,38)
 330  RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 340  FORMAT (2X,24HSTATISTICAL ANALYSIS OF ,12A1)
 350  FORMAT (4X,24HNUMBER OF MEASUREMENTS =,I6,
     1   32H, NO. OF DISTINCT MEASUREMENTS =,I6/)
 360  FORMAT (4X,11HWEIGHTS IN ,12A1,11H OF LENGTH ,I4,6H WITH ,I4,17H N
     1ON-ZERO WEIGHTS)
 370  FORMAT (5X,65HALL COMPUTATIONS ARE BASED ON OBSERVATIONS WITH NON-
     1ZERO WEIGHTS.)
 380  FORMAT (8X,48HFREQUENCY DISTRIBUTION WITH 10 CLASSES OF LENGTH,
     1   12A1/7X,10I6)
 390  FORMAT (8X,61HFREQUENCY DISTRIBUTION OF LEAST SIGNIFICANT DIGIT (0
     1,1,...,9)/7X,10I6)
 400  FORMAT (/1X, 1X,20HMEASURES OF LOCATION/
     1 5X,38HARITHMETIC MEAN  . . . . . . . . . . .,20A1/
     2 5X,38HMEDIAN . . . . . . . . . . . . . . . .,20A1/
     3 5X,38HMID-RANGE  . . . . . . . . . . . . . .,20A1/
     4 5X,38HMID-MEAN (25 PERCENT TRIMMED MEAN) . .,20A1)
 410  FORMAT (     2X,22HMEASURES OF DISPERSION/
     1 5X,38HSTANDARD DEVIATION . . . . . . . . . .,20A1/
     2 5X,38H  AS PERCENT OF MEAN (COEF. OF VAR.) .,20A1/
     3 5X,38HRANGE  . . . . . . . . . . . . . . . .,20A1/
     4 5X,38HMEAN DEVIATION . . . . . . . . . . . .,20A1/
     5 5X,38HINTER-QUARTILE RANGE . . . . . . . . .,20A1/
     6 5X,38HVARIANCE . . . . . . . . . . . . . . .,20A1)
 420  FORMAT (2X,41HSTANDARD DEVIATION OF MEAN. . . . . . . .,20A1)
 430  FORMAT (1X, 1X,16HTREND STATISTICS/
     1 5X,38HSLOPE, SIGNIFICANCE LEVEL  . . . . . .,20A1,F9.3/
     2 5X,38HQUADRATIC COEFF., SIGNIFICANCE LEVEL .,20A1,F9.3)
 440  FORMAT (2X,31H OTHER TESTS FOR NON-RANDOMNESS/
     1 5X,38HNUMBER OF RUNS UP AND DOWN, Z VALUE  .,20A1,F9.3/
     2 5X,38HMEAN SQUARE SUCCESSIVE DIFFERENCE  . .,20A1/
     3 5X,38H   MS SUCC DIFF/2(VARIANCE), Z VALUE .,20A1,F9.3/
     4 5X,38HDEVIATIONS FROM ARITHMETIC MEAN       /
     5 5X,38H   NUMBER OF - SIGNS, + SIGNS  . . . .,20A1,I5/
     6 5X,38H   NUMBER OF RUNS, Z VALUE . . . . . .,20A1,F9.3/
     7 5X,38HAUTOCORRELATION COEFFICIENT  . . . . .,20A1)
 450  FORMAT (/1X, 1X,51HA TWO-SIDED 95 PERCENT CONFIDENCE INTERVAL FOR 
     1 THE)
 460  FORMAT (5X,10HMEAN IS   ,35A1)
 470  FORMAT (5X,10HMEDIAN IS ,35A1,1X,6A3,A1)
 480  FORMAT (5X,10HS.D. IS   ,35A1)
 490  FORMAT (/1X, 1X,61HSTATISTICAL TOLERANCE INTERVAL WITH 95 PERCENT 
     1CONFIDENCE FOR)
 500  FORMAT (5X,26H50 PCT NORMAL COVERAGE IS ,35A1)
 510  FORMAT (5X,26H95 PCT NORMAL COVERAGE IS ,35A1)
 520  FORMAT (5X,26H99 PCT NORMAL COVERAGE IS ,35A1)
 530  FORMAT (5X,48HINTERVAL FROM MIN TO MAX HAS DIST.-FREE COVERAGE,
     1   F6.2)
 540  FORMAT (/1X, 1X,16HOTHER STATISTICS/
     1 5X,38HMINIMUM  . . . . . . . . . . . . . . .,20A1/
     2 5X,38H   SECOND MINIMUM  . . . . . . . . . .,20A1/
     3 5X,38HMAXIMUM  . . . . . . . . . . . . . . .,20A1/
     4 5X,38H   SECOND MAXIMUM  . . . . . . . . . .,20A1)
 550  FORMAT ( 5X,
     1    38H(MEAN-MINIMUM)/STANDARD DEVIATION  . .,20A1/
     2 5X,38H(MAXIMUM-MEAN)/STANDARD DEVIATION  . .,20A1/
     3 5X,38HSQRT(B1), SKEWNESS COEFFICIENT . . . .,20A1/
     4 5X,38HB2, KURTOSIS COEFFICIENT . . . . . . .,20A1/
     5 5X,38HLOWER QUARTILE . . . . . . . . . . . .,20A1/
     6 5X,38HUPPER QUARTILE . . . . . . . . . . . .,20A1)
 560  FORMAT (1H ,14X,12HOBSERVATIONS,22X,20HORDERED OBSERVATIONS)
 570  FORMAT (/1X,3X,1HI,6X,4HX(I),7X,4HRANK,2X,9HX(I)-MEAN,7X,3HNO.,6X
     1   ,4HX(J),4X,11HX(J+1)-X(J))
 580  FORMAT (1X,I4,13A1,F8.1,12A1,5X,I4,13A1,12A1)
 590  FORMAT (/1X,3X,1HI,5X,4HX(I),6X,4HRANK,2X,9HX(I)-MEAN,3X,6HWEIGHT,
     1   2X,3HNO.,5X,4HX(J),3X,11HX(J+1)-X(J))
 600  FORMAT (1X,I4,12A1,F7.1,11A1,1X,8A1,1X,I4,12A1,11A1)
 610  FORMAT (1H )
C
C     ==================================================================
C
      END
*STAPTB
      SUBROUTINE STAPTB (IGF,ISR,JF,ISC,IS1,IS2,N,NT,N2,CW,PX,S,X,XB,XX,
     1                   NSTORE,SV,KB,KD,KS,KT,KW,YLABLE)        
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STAPTB V 7.00  4/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT THE LAST TWO GRAPHS OF STATPLOTS.
C     TWO GRAPHS ARE PRINTED SIDE BY SIDE ON BOTTOM HALF OF PAGE,
C     IF WIDTH IS ENOUGH,
C     OTHERWISE A NEW PAGE IS STARTED AND THE FOURTH GRAPH
C     IS PRINTED BELOW THE THIRD.
C
C     BASED UPON SUGGESTIONS OF DAVID HOGBEN, CODE WAS ORIGINALLY
C        WRITTEN BY JAMES J. FILLIBEN FOR DATAPAC, NOVEMBER, 1974.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1975.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ISR(131), JSTORE(66), NSTORE(*)
      DIMENSION KB(*), KD(*), KS(*), KT(*), KW(*)
      DIMENSION IGF(21,*), JF(*), JHOLD(21,52), NHOLD(21,12)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             CW(*), X(*), XX(*)
      REAL             PX, S, XB
      REAL             SV(*), YLABLE(*), Y2(1)
      REAL             CWIDSD, PPRXY, XBAR
      REAL             SPCA
C
C     ................................................................
C
      CHARACTER        LA*1
      CHARACTER*1      ISR, JF, JSTORE, NSTORE
      CHARACTER*1      JHOLD, JSYM, NHOLD
C
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     NEEDED CONSTANTS.
C        21 IS THE NUMBER OF PLOTTING POSITIONS ON THE Y-AXIS
C        51 IS THE NUMBER OF PLOTTING POSITIONS ON THE X-AXIS
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     LBLEY  = NUMBER OF CHARACTERS ALLOWED FOR PRINTING VERTICAL SCALE
C
      DATA LBLEY / 11 /
C
      DATA ICA /  1000 /
      DATA ICB / 10000 /
C
      DATA NW2 / 16 /
C
      DATA SPCA /    0.2  /
C
C     ==================================================================
C
      LXIS   = LXAXIS - IONE
      IXRLT  = LXAXIS + IONE
      IXRRT  = ITWO * LXAXIS
      MIDDLE = IDIV (LXAXIS-IONE,ITWO,IND) + IONE
      LBLEY1 = LBLEY + IONE
      LBLEY2 = ITWO * LBLEY1
      LBLEY3 = LBLEY1 + IONE
      IYMID  = IDIV (1+LYAXIS,ITWO,IND)
      IYUQ   = IDIV (LYAXIS-IONE,IFOUR,IND) + IONE
      IYLQ   = IDIV (ITHRE*(LYAXIS-IONE),IFOUR,IND) + IONE
      NSUBNS = IONE
      NSUBYL = IONE
      CWIDSD = SPCA
      PPRXY  = PX
      XBAR   = XB
C
      ISUBYL = NSUBYL + 42
      JSUBYL = NSUBYL + 63
      DO 50 I=1,LYAXIS
        CALL RFORMT (KT(3),KS(3),Y2,YLABLE(ISUBYL),
     1              KB(3),0,KW(3),KD(3),ISR(1),IRF)
        ISUBYL = ISUBYL + IONE
        ISR(LBLEY1) = LA(45)
        CALL RFORMT (KT(4),KS(4),Y2,YLABLE(JSUBYL),
     1               KB(4),0,KW(4),KD(4),
     2               ISR(LBLEY3),IRF)
        JSUBYL = JSUBYL + IONE
        ISR(LBLEY2) = LA(45)
C
C       UNPACK PLOTTING CHARACTER.
C
        K = LXAXIS
        DO 10 J=1,LXAXIS
          I1 = IDIV (MOD(IGF(I,J),ICA),IHRD,IND)
          IF (I1.EQ.IZERO) JF(J) = LA(45)
          IF (I1.EQ.ITHRE) JF(J) = LA(34)
          K = K + IONE
          I2 = IDIV (MOD(IGF(I,J),ICB),ICA,IND)
          IF (I2.EQ.IZERO) JF(K) = LA(45)
          IF (I2.EQ.IONE) JF(K) = LA(38)
          IF (I2.EQ.ITWO) JF(K) = LA(41)
  10    CONTINUE
C
C       PRINT PLOTS 3 AND 4.
C
        JSYM = LA(39)
        IF (I.EQ.IONE .OR. I.EQ.IYMID .OR. I.EQ.LYAXIS) JSYM = LA(40)
        IF (I.EQ.IYUQ .OR. I.EQ.IYLQ) JSYM = LA(40)
C
C       TWO PLOTS TO A PAGE.
C          SAVE GRAPH FOUR FOR LATER PLOTTING.
C
        IF (LWIDE.GE.LWC) GO TO 40
        JHOLD(I,1) = JSYM
        JJ = IONE
        DO 20 IJ=IXRLT,IXRRT
          JJ = JJ + IONE
          JHOLD(I,JJ) = JF(IJ)
  20    CONTINUE
C
        DO 30 IJ=1,LBLEY1
          NHOLD(I,IJ) = ISR(IJ+12)
  30    CONTINUE
C
C     PRINT PLOT 3 LINE BY LINE
C
        WRITE (IPRINT,190) (ISR(K),K=1,LBLEY1), JSYM,
     1                     (JF(J),J=1,LXAXIS), JSYM
        GO TO 50
C
C       PRINT PLOTS 3 AND 4 LINE BY LINE.
C
  40    WRITE (IPRINT,190) (ISR(K),K=1,LBLEY1), JSYM,
     1                     (JF(J),J=1,LXAXIS), JSYM,
     2                     (ISR(K),K=13,24), JSYM,
     3                     (JF(J),J=IXRLT,IXRRT), JSYM
C
  50  CONTINUE
C
C     PRINT HORIZONTAL BORDER AT BOTTOM.
C
      DO 70 J2=1,LXIS,10
        J3 = J2 + 9
        DO 60 J1=J2,J3
          JF(J1) = LA(39)
  60    CONTINUE
        JF(J2) = LA(40)
  70  CONTINUE
C
      JF(MIDDLE) = LA(25)
      IF (LWIDE.GE.LWC) WRITE (IPRINT,180) LA(45), (JF(J),J=1,LXIS),
     1              LA(40), LA(45), LA(45), (JF(J),J=1,LXIS), LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,180) LA(45), (JF(J),J=1,LXIS),
     1                                     LA(40)
C
C     PRINT HORIZONTAL SCALE.
C
      DO 80 LJ=1,131
        ISR(LJ) = LA(45)
  80  CONTINUE
C
      LLJ = 7
      DO 100 LJ=3,24,4
C
C       LEFT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(LJ),IONE,MS,MS+IFIVE,ISR(LLJ),IZERO,MW,MD,MWX,
     1              MDX)
        IF (MWX.LT.9) GO TO 90
        LT = IONE
        MS = IFOUR
        CALL RFORMT (0,MS,XX(LJ),X(1),1,9,MWX,MDX,ISR(1),IRF)
  90    MMW = 9 - MWX
        CALL RFORMT (LT,MS,X,XX(LJ),MMW,0,MWX,MDX,ISR(LLJ),IRF)
C
C       RIGHT PLOT.
C
        CALL MINNW (XX(LJ+1),IONE,IFOUR,9,ISR(LLJ+66),IZERO,MW,MD,
     1              MWX,MDX)
        MMW = 9 - MWX
        CALL RFORMT (1,4,X,XX(LJ+1),MMW,0,MW,MD,ISR(LLJ+66),IRF)
        LLJ = LLJ + ITEN
 100  CONTINUE
C
      IF (LWIDE.GE.LWC) GO TO 130
      WRITE (IPRINT,200) (ISR(LJ),LJ=1,66)
      WRITE (IPRINT,210)
      WRITE (IPRINT,260)
C
C     SAVE HORIZONTAL SCALE FOR GRAPH FOUR.
C
      DO 110 LJ=1,65
        JSTORE(LJ) = ISR(LJ+66)
 110  CONTINUE
C
C     PRINT INFORMATION AT BOTTOM OF GRAPH 3 -  HISTOGRAM.
C
      DO 120 LI=101,131
        ISR(LI) = LA(45)
 120  CONTINUE
C
      WRITE (IPRINT,220) N
      CALL RFORMT (0,ISIGD,CW,Y2(1),1,14,MW,MND,ISR(101),IRF)
      CALL RFORMT (1,ISIGD,Y2,CW(1),0,0,MW,MND,ISR(101),IRF)
      WRITE (IPRINT,230) CWIDSD, (ISR(K),K=101,114)
      WRITE (IPRINT,240) NT
      WRITE (IPRINT,250)
      GO TO 140
 130  WRITE (IPRINT,200) (ISR(LK),LK=1,131)
      WRITE (IPRINT,210)
      GO TO 160
C
C     PRINT FOURTH GRAPH BELOW THIRD GRAPH BECAUSE LWIDE IS NOT 120.
C
 140  WRITE (IPRINT,260)
      WRITE (IPRINT,270)
      WRITE (IPRINT,180) LA(45), (JF(J),J=1,LXIS), LA(40), LA(45)
      DO 150 I=1,LYAXIS
C
C       PRINT PLOT 4 LINE BY LINE.
C
        WRITE (IPRINT,190) (NHOLD(I,J),J=1,LBLEY1), (JHOLD(I,K),K=1,52),
     1                      JHOLD(I,1)
 150  CONTINUE
      WRITE (IPRINT,180) LA(45), (JF(J),J=1,LXIS), LA(40)
      WRITE (IPRINT,200) (JSTORE(LJ),LJ=1,65)
      RETURN
C
C     ..................................................................
C
C     PRINT INFORMATION AT BOTTOM OF PAGE.
C
 160  WRITE (IPRINT,260)
      DO 170 LI=101,131
        ISR(LI) = LA(45)
 170  CONTINUE
C
      WRITE (IPRINT,220) N, LA(38), PPRXY
      CALL RFORMT (0,ISIGD,CW,Y2(1),1,14,MW,MND,ISR(101),IRF)
      CALL RFORMT (1,ISIGD,Y2,CW(1),0,0,MW,MND,ISR(101),IRF)
      NSSTOP = NSUBNS + IS1 - IONE
      WRITE (IPRINT,230) CWIDSD, (ISR(K),K=101,114), LA(38), N2,
     1                  (NSTORE(IS),IS=NSUBNS,NSSTOP)
      NSSTRT = NSUBNS + IS2 - IONE
      NSSTOP = NSUBNS + ISC - IONE
      WRITE (IPRINT,240) NT, LA(38), (NSTORE(IS),IS=NSSTRT,NSSTOP)
C
      SV(1) = XBAR
      CALL RFORMT (0,ISIGD,SV(1),Y2(1),1,15,MW,MND,ISR(1),IRF)
      CALL RFORMT (11,ISIGD,Y2,XBAR,0,0,NW2,MND,ISR(1),IRF)
      CALL RFORMT (1,ISIGD,Y2,XBAR,0,0,MW,MND,ISR(1),IRF)
      ISR(MW+1) = LA(44)
C
      SV(1) = S
      CALL RFORMT (0,ISIGD,SV(1),Y2(1),1,15,MW,MND,ISR(1),IRF)
      CALL RFORMT (1,ISIGD,Y2,S,0,0,MW,MND,ISR(17),IRF)
      MW = 16 + MW
      WRITE (IPRINT,250) LA(38), (ISR(IS),IS=1,16),
     1                           (ISR(IS),IS=17,MW)
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 180  FORMAT (1H ,12X,53A1,13X,53A1)
 190  FORMAT (1H ,12A1,53A1,1X,12A1,53A1)
 200  FORMAT (1H ,131A1)
 210  FORMAT (1H ,11X,53H -5        -3        -1         1         3    
C     ==================================================================
     1     5)
 220  FORMAT (1H , 9X,25HNUMBER OF MEASUREMENTS = ,I5,32X,
     1            A1,7X,44H PROBABILITY PLOT CORRELATION COEFFICIENT = ,
     2   F6.4)
 230  FORMAT (1H ,9X,14HCLASS WIDTH = ,F3.1,20H STANDARD DEVIATIONS,
     1 3H = ,14A1,8X,A1,7X,11H SCRAWL (N=,I4,1H,,36A1)
 240  FORMAT (1H ,6X,I4,42H OBSERVATIONS WERE IN EXCESS OF 5 STANDARD,
     1 11H DEVIATIONS,8X,A1,7X,52A1)
 250  FORMAT (1H , 9X,59HABOUT THE SAMPLE MEAN AND WERE NOT PRINTED IN T
     1HE HISTOGRAM,3X,A1,7X, 8H MEAN = ,16A1,13H STD. DEV. = ,15A1)
 260  FORMAT (1H )
 270  FORMAT (27X,23HNORMAL PROBABILITY PLOT)
C
C     ==================================================================
C
      END
*STAPTG
      SUBROUTINE STAPTG (IGF,JT,LI,N2,XN,XM,X20,X4,X6,X8,KB,KD,KS,KT,KW,
     1                   JGRAPH,YLABLE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STAPTG V 7.00  4/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     OPERATES ON A PARTICULAR PLOT FOR ONE OF THE STATPLOTS.
C
C     INPUT ...
C
C       IGF,JT,LI AND N2.
C
C     OUTPUT ...
C
C       XN,XMX,X20,X4,X6 AND X8.
C
C     BASED UPON SUGGESTIONS OF DAVID HOGBEN, CODE WAS ORIGINALLY
C        WRITTEN BY JAMES J. FILLIBEN FOR DATAPAC, NOVEMBER, 1974.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION  - OCTOBER, 1975.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ISTORE(120)
      DIMENSION KB(*), KD(*), KS(*), KT(*), KW(*)
      DIMENSION IGF(21,*), JGRAPH(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      REAL             XN(*), XM(*), X20(*), X4(*), X6(*), X8(*)
      REAL             YLABLE(*)
      REAL             ANUM, BNUM, HEIGHT
      REAL             RATIOX, RATIOY, WIDTH
      REAL             XMAX2, XMIN2, YMAX, YMIN, YPROD
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC, SPCD
C
C     .............................................................
C
      CHARACTER       ISTORE*1, JGRAPH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     NEEDED CONSTANTS.
C        21 IS THE NUMBER OF PLOTTING POSITIONS ON THE Y-AXIS
C        51 IS THE NUMBER OF PLOTTING POSITIONS ON THE X-AXIS
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     LBLEY  = NUMBER OF CHARACTERS ALLOWED FOR PRINTING VERTICAL SCALE
C
      DATA LBLEY / 11 /
C
      DATA SPCA /    0.20 /
      DATA SPCB /    0.40 /
      DATA SPCC /    0.60 /
      DATA SPCD /    0.80 /
C
C     ==================================================================
C
      IPLOT  = JT
      LBLEY1 = LI
      HEIGHT = FLOAT(LYAXIS) - RONE
      WIDTH  = FLOAT(LXAXIS) - RONE
      NSUBY2 = IDIV ( NRC,ITWO,IND) + IONE
      NSUBJG = IONE
      NSUBYL = IONE
C
C     DETERMINE THE VALUES TO BE LISTED ON THE VERTICAL (Y) AXIS.
C
      YMIN = A(NSUBY2)
      YMAX = A(NSUBY2)
      ISUBY2 = NSUBY2
      DO 10 I=1,N2
        IF (A(ISUBY2).LT.YMIN) YMIN = A(ISUBY2)
        IF (A(ISUBY2).GT.YMAX) YMAX = A(ISUBY2)
        ISUBY2 = ISUBY2 + IONE
 10   CONTINUE
      IF (IPLOT.EQ.ITHRE) YMIN = RZERO
C
      KSUBYL = NSUBYL + 21 * (IPLOT - IONE)
      ISUBYL = KSUBYL
      DO 20 I=1,LYAXIS
        ANUM  = I - IONE
        BNUM  = LYAXIS - I
        YPROD = ANUM * YMIN + BNUM * YMAX
        YLABLE(ISUBYL) = FDIV (YPROD,HEIGHT,NF)
        ISUBYL = ISUBYL + IONE
  20  CONTINUE
C
C     SETUP FOR VERTICAL SCALE USING LBLEY CHARACTERS
C        IF MINIMUM WIDTH LE LBLEY, USE F LBLEY.MDY
C                         GT LBLEY, USE R FORMAT WITH LBLEY-5 S. DIGITS.
C
      KS(IPLOT) = MIN0 (ISIGD-IONE,LBLEY)
      CALL MINNW (YLABLE(KSUBYL),LYAXIS,KS(IPLOT),LBLEY1,JGRAPH(NSUBJG),
     1            IZERO,KW(IPLOT),KD(IPLOT),MWY,MDY)
      IF (MWY.GT.LBLEY) GO TO 30
      KT(IPLOT) = 7
      KW(IPLOT) = LBLEY
      KD(IPLOT) = MDY
      GO TO 40
  30  KS(IPLOT) = LBLEY - IFIVE
      KT(IPLOT) = IONE
      CALL RFORMT (0,KS(IPLOT),YLABLE(KSUBYL),A(NSUBY2),LYAXIS,LBLEY,
     1             KW(IPLOT),KD(IPLOT),ISTORE(1),IRF)
  40  KB(IPLOT) = LBLEY - KW(IPLOT)
C
C     DETERMINE XMIN, XMAX, XMID, X20 ( = THE 20% POINT), AND
C        X4  ( = THE 40% POINT) ETC.
C
      XMIN2 = A(1)
      XMAX2 = A(1)
      DO 50 I=1,N2
        IF (A(I).LT.XMIN2) XMIN2 = A(I)
        IF (A(I).GT.XMAX2) XMAX2 = A(I)
  50  CONTINUE
C
      XN(IPLOT)   = XMIN2
      XM(IPLOT)   = XMAX2
      X20(IPLOT)  = SPCD * XMIN2 + SPCA * XMAX2
      X4(IPLOT)   = SPCC * XMIN2 + SPCB * XMAX2
      X6(IPLOT)   = SPCB * XMIN2 + SPCC * XMAX2
      X8(IPLOT)   = SPCA * XMIN2 + SPCD * XMAX2
C
C     DETERMINE THE (X,Y) PLOT POSITIONS AND PACK.
C
      IP     = ITEN**(IPLOT-IONE)
      RATIOY = FDIV (HEIGHT,YMAX-YMIN,NF)
      RATIOX = FDIV (WIDTH,XM(IPLOT)-XN(IPLOT),NF)
      ISUBY2 = NSUBY2 - IONE
      DO 80 I=1,N2
        ISUBY2 = ISUBY2 + IONE
        IF (IPLOT.EQ.ITHRE .AND. A(ISUBY2).LE.RZERO) GO TO 80
        MX = RATIOX * (A(I)-XN(IPLOT)) + RHALF
        MX = MX + IONE
        IF (MX.LT.IONE) MX = IONE
        IF (MX.GT.LXAXIS) MX = LXAXIS
        MY = RATIOY * (A(ISUBY2)-YMIN) + RHALF
        MY = LYAXIS - MY
        IF (MY.LT.IONE) MY = IONE
        IF (MY.GT.LYAXIS) MY = LYAXIS
        IT = IDIV (IGF(MY,MX),IP,IND)
        IF (IPLOT.EQ.ITHRE) GO TO 60
        IF (IT.NE.ITWO) IGF(MY,MX) = IGF(MY,MX) + IP
        GO TO 80
C
  60    IF (IT.EQ.ITHRE) GO TO 80
        IGF(MY,MX) = IGF(MY,MX) + ITHRE * IP
        MY1 = MY + IONE
        IF (MY1.GT.LYAXIS) GO TO 80
        DO 70 IY=MY1,LYAXIS
          IGF(IY,MX) = IGF(IY,MX) + ITHRE * IP
  70    CONTINUE
  80  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STAPTT
      SUBROUTINE STAPTT (X,XX,XBR,PXY,IGRF,ISC,IS1,IS2,N2,S,ISTORE,JGRF,
     1 NSTORE,SV,KB,KD,KS,KT,KW,YLABLE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STAPTT V 7.00  4/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     PRINT THE FIRST TWO GRAPHS OF STATPLOTS.
C     TWO GRAPHS ARE PRINTED SIDE BY SIDE ON TOP HALF OF PAGE,
C     IF WIDTH IS ENOUGH,
C     OTHERWISE SECOND GRAPH IS PRINTED BELOW FIRST.
C
C     BASED UPON SUGGESTIONS OF DAVID HOGBEN, CODE WAS ORIGINALLY
C        WRITTEN BY JAMES J. FILLIBEN FOR DATAPAC, NOVEMBER, 1974.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1975.
C                   CURRENT VERSION -    APRIL, 1991.
C
      DIMENSION ISTORE(*), NSTORE(*)
      DIMENSION KB(*), KD(*), KS(*), KT(*), KW(*)
      DIMENSION IGRF(21,*), JGRF(102), JHOLD(21,52), NHOLD(21,12)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X(*), XX(*)
      REAL             PXY, S, XBR
      REAL             SV(*), Y2(1)
      REAL             YLABLE(*)
C
C     ................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER*1      ISTORE, JGRF, NSTORE
      CHARACTER*1      JHOLD, JSYM, NHOLD
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     NEEDED CONSTANTS.
C        21 IS THE NUMBER OF PLOTTING POSITIONS ON THE Y-AXIS
C        51 IS THE NUMBER OF PLOTTING POSITIONS ON THE X-AXIS
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     LBLEY  = NUMBER OF CHARACTERS ALLOWED FOR PRINTING VERTICAL SCALE
C
      DATA LBLEY / 11 /
C
      DATA NW1 /  9 /
      DATA NW2 / 16 /
C
C     ==================================================================
C
      LXIS   = LXAXIS - IONE
      IXRLT  = LXAXIS + IONE
      IXRRT  = ITWO * LXAXIS
      MIDDLE = IDIV (LXAXIS-IONE,ITWO,IND) + IONE
      LBLEY1 = LBLEY + IONE
      LBLEY2 = ITWO * LBLEY1
      LBLEY3 = LBLEY1 + IONE
      IYUQ   = IDIV (LYAXIS-IONE,IFOUR,IND) + IONE
      IYLQ   = IDIV (ITHRE*(LYAXIS-IONE),IFOUR,IND) + IONE
      NSUBNS = IONE
      NSUBYL = IONE
C
C     PRINT TOP HALF OF PAGE.
C
      CALL HEADS (IARGS(1),IONE,IZERO,IONE)
C
      CALL PAGE (IFOUR)
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,170) (LHEAD(I),I=1,12)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,280) (LHEAD(I),I=1,12)
C
C     PRINT HORIZONTAL BORDER AT TOP.
C
C        LA(39) = 1H-
C        LA(40) = 1H+
C        LA(25) = 1HO
C
      DO 20 J2=1,LXIS,10
        J3 = J2 + 9
        DO 10 J1=J2,J3
          JGRF(J1) = LA(39)
  10    CONTINUE
        JGRF(J2) = LA(40)
  20  CONTINUE
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS),
     1              LA(40), LA(45), LA(45), (JGRF(J),J=1,LXIS), LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS),
     1                                     LA(40)
C
      ISUBYL = NSUBYL
      JSUBYL = NSUBYL + 21
      DO 70 I=1,LYAXIS
        CALL RFORMT (KT(1),KS(1),Y2,YLABLE(ISUBYL),KB(1),
     1               0,KW(1),KD(1),ISTORE(1),IRF)
        ISUBYL         = ISUBYL + IONE
        ISTORE(LBLEY1) = LA(45)
        CALL RFORMT (KT(2),KS(2),Y2,YLABLE(JSUBYL),
     1               KB(2),0,KW(2),KD(2),
     2               ISTORE(LBLEY3),IRF)
        JSUBYL         = JSUBYL + IONE
        ISTORE(LBLEY2) = LA(45)
C
C     UNPACK PLOTTING CHARACTER.
C
        K = LXAXIS
C
C       LA(45) = 1H
C       LA(38) = 1H.
C       LA(41) = 1H*
C
        DO 30 J=1,LXAXIS
          I1 = MOD (IGRF(I,J),ITEN)
          IF (I1.EQ.IZERO) JGRF(J) = LA(45)
          IF (I1.EQ.IONE)  JGRF(J) = LA(38)
          IF (I1.EQ.ITWO)  JGRF(J) = LA(41)
          K = K + IONE
          I2 = IDIV (MOD(IGRF(I,J),IHRD),ITEN,IND)
          IF (I2.EQ.IZERO) JGRF(K) = LA(45)
          IF (I2.EQ.IONE)  JGRF(K) = LA(38)
          IF (I2.EQ.ITWO)  JGRF(K) = LA(41)
  30    CONTINUE
C
C       PRINT PLOTS 1 AND 2.
C
        JSYM = LA(39)
        IF (I.EQ.IONE .OR. I.EQ.MIDDLE .OR. I.EQ.LYAXIS) JSYM = LA(40)
        IF (I.EQ.IYUQ .OR. I.EQ.IYLQ) JSYM = LA(40)
        IF (LWIDE.GE.LWC) GO TO 60
C
C       SAVE GRAPH TWO FOR LATER PLOTTING.
C
        JHOLD(I,1) = JSYM
        JJ = IONE
        DO 40 IJ=IXRLT,IXRRT
          JJ = JJ + IONE
          JHOLD(I,JJ) = JGRF(IJ)
  40    CONTINUE
C
        DO 50 IJ=1,LBLEY1
          NHOLD(I,IJ) = ISTORE(IJ+12)
  50    CONTINUE
C
        WRITE (IPRINT,190) (ISTORE(K),K=1,LBLEY1), JSYM,
     1                     (JGRF(J),J=1,LXAXIS),  JSYM
        GO TO 70
C
C       PRINT PLOTS 1 AND 2 LINE BY LINE.
C
  60    WRITE (IPRINT,190) (ISTORE(K),K=1,LBLEY1), JSYM,
     1               (JGRF(J),J=1,LXAXIS), JSYM, (ISTORE(K),K=13,24),
     2                JSYM, (JGRF(J),J=IXRLT,IXRRT), JSYM
C
  70  CONTINUE
C
C     PRINT HORIZONTAL BORDER AT BOTTOM.
C
      DO 90 J2=1,LXIS,10
        J3 = J2 + 9
        DO 80 J1=J2,J3
          JGRF(J1) = LA(39)
  80    CONTINUE
        JGRF(J2) = LA(40)
  90  CONTINUE
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS),
     1              LA(40), LA(45), LA(45), (JGRF(J),J=1,LXIS), LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS),
     1                                     LA(40)
C
C     PRINT HORIZONTAL SCALE.
C
      DO 100 LJ=1,131
        ISTORE(LJ) = LA(45)
 100  CONTINUE
C
      LLJ = 7
      DO 120 LJ=1,24,4
C
C       LEFT PLOT.
C
        CALL RFORMT (7,5,X,XX(LJ),0,0,NW1,IONE,ISTORE(LLJ),IRF)
C
C       RIGHT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(LJ+1),IONE,MS,MS+IFIVE,ISTORE(LLJ+66),IZERO,MW,
     1              MD,MWX,MDX)
        IF (MWX.LT.9) GO TO 110
        LT = IONE
        MS = IFOUR
        CALL RFORMT (0,MS,XX(LJ+1),X(1),1,9,MWX,MDX,ISTORE(1),IRF)
 110    MMW = 9 - MWX
        CALL RFORMT (LT,MS,X,XX(LJ+1),MMW,0,MWX,MDX,ISTORE(LLJ+66),IRF)
        LLJ = LLJ + ITEN
 120  CONTINUE
      IF (LWIDE.GE.LWC) GO TO 150
C
C     PRINT SECOND GRAPH BELOW FIRST GRAPH BECAUSE LWIDE IS NOT 120.
C
      WRITE (IPRINT,200) (ISTORE(LJ),LJ=1,66)
      WRITE (IPRINT,260)
      WRITE (IPRINT,270)
      WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS), LA(40)
      DO 130 I=1,LYAXIS
C
C     PRINT PLOT 2 LINE BY LINE.
C
        WRITE (IPRINT,190) (NHOLD(I,J),J=1,LBLEY1), (JHOLD(I,K),K=1,52),
     1                      JHOLD(I,1)
 130  CONTINUE
      WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS), LA(40)
      WRITE (IPRINT,200) (ISTORE(LJ),LJ=67,131)
C
C     PRINT INFORMATION AT BOTTOM OF GRAPH 2.
C
      DO 140 LI=101,131
        ISTORE(LI) = LA(45)
 140  CONTINUE
C
      WRITE (IPRINT,260)
      WRITE (IPRINT,220) PXY
      NSSTOP = NSUBNS + IS1 - IONE
      JSUBNS = NSUBNS + IS2 - IONE
      JSSTOP =NSUBNS + ISC - IONE
      WRITE (IPRINT,230) N2, (NSTORE(IS),IS=NSUBNS,NSSTOP)
      WRITE (IPRINT,240) (NSTORE(IS),IS=JSUBNS,JSSTOP)
C
      SV(1) = XBR
      CALL RFORMT (0,ISIGD,SV(1),Y2(1),1,15,MW,MND,ISTORE(1),IRF)
      CALL RFORMT (11,ISIGD,Y2,XBR,0,0,NW2,MND,ISTORE(1),IRF)
      CALL RFORMT (1,ISIGD,Y2,XBR,0,0,MW,MND,ISTORE(1),IRF)
      ISTORE(MW+1) = LA(44)
C
      SV(1) = S
      CALL RFORMT (0,ISIGD,SV(1),Y2(1),1,15,MW,MND,ISTORE(1),IRF)
      CALL RFORMT (1,ISIGD,Y2,S,0,0,MW,MND,ISTORE(17),IRF)
      MW = 16 + MW
      WRITE (IPRINT,250) (ISTORE(IS),IS=1,16),
     1                           (ISTORE(IS),IS=17,MW)
C
      JGRF(MIDDLE) = LA(25)
      GO TO 160
C
 150  WRITE (IPRINT,200) (ISTORE(LJ),LJ=1,131)
C
C     ..................................................................
C
C     PRINT BOTTOM HALF OF PAGE.
C
      WRITE (IPRINT,260)
C
      WRITE (IPRINT,210)
C
C     PRINT HORIZONTAL BORDER AT TOP.
C
      JGRF(MIDDLE) = LA(25)
      WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS), LA(40), LA(45),
     1                   LA(45), (JGRF(J),J=1,LXIS), LA(40)
      RETURN
C
C     ..................................................................
C
C     SET UP FOR GRAPH THREE WHEN TWO PLOTS PER PAGE ARE TO BE DONE.
C
 160  CALL PAGE (IFOUR)
      WRITE (IPRINT,290)
      WRITE (IPRINT,180) LA(45), (JGRF(J),J=1,LXIS), LA(40)
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 170  FORMAT (1H ,10X,10X,8HPLOT OF ,12A1,16H, X(I), VERSUS I,
     1   35X,8HPLOT OF ,18HX(I) VERSUS X(I-1))
 180  FORMAT (1H ,12X,53A1,13X,53A1)
 190  FORMAT (1H ,12A1,53A1,1X,12A1,53A1)
 200  FORMAT (1H ,131A1)
 210  FORMAT (1H ,34X,9HHISTOGRAM,50X,23HNORMAL PROBABILITY PLOT)
 220  FORMAT (1H ,6X,44H PROBABILITY PLOT CORRELATION COEFFICIENT = ,
     1    F6.4)
 230  FORMAT (1H ,6X,11H SCRAWL (N=,I4,1H,,36A1)
 240  FORMAT (1H ,6X,52A1)
 250  FORMAT (1H ,6X, 8H MEAN = ,16A1,13H STD. DEV. = ,15A1)
 260  FORMAT (1H )
 270  FORMAT (1H ,26X,26HPLOT OF X(I) VERSUS X(I-1))
 280  FORMAT (1H ,10X,10X,8HPLOT OF ,12A1,16H, X(I), VERSUS I)
 290  FORMAT (1H ,34X,9HHISTOGRAM)
C
C     ==================================================================
C
      END
*STAQDS
      SUBROUTINE STAQDS (IWT,ISTORE,NZW,WT,SUM,SUMWT,S2,ASUMWT,SA,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STAQDS V 7.00  7/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE QUADRATIC EFFECT AND SIGNIFICANCE LEVEL
C        FOR STATISTICAL ANALYSIS INSTRUCTIONS.
C
C     INPUT ...
C
C          IWT    = 1,     NO WEIGHTS ARE USED.
C                 = 2,     WEIGHTS ARE USED.
C
C          ISTORE = 1,     NO STORAGE IS REQUIRED.
C                 = 2,     STORAGE IS REQUIRED.
C
C     OUTPUT ...
C
C          NZW     NUMBER OF NON-ZERO WEIGHTS.
C           WT     SUM OF NON-ZERO WEIGHTS(I).
C          SUM     SUM OF X(I).
C        SUMWT     SUM OF WEIGHTS(I)*X(I).
C           S2     SUM OF WEIGHTS(I)*X(I)**2.
C       ASUMWT     ABSOLUTE SUM OF WEIGHTS(I)*X(I).
C          IND   = 0, CONTINUE COMPUTATION UPON RETURN TO STATIS.
C                = 1, DO NOT CONTINUE COMPUTATION UPON RETURN TO STATIS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -        1967.
C                   CURRENT VERSION -  JULY, 1991.
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
      REAL             ASUMWT, SUM, SUMWT, S2, WT
      REAL             SA(*)
      REAL             F, QCOEF
C
C     ==================================================================
C
      ISIZSA = IDIV (NS-IHRD,IFOUR,IXND)
      ISUBSA = ISIZSA + 1
      K      = IARGS(1)
      M      = IONE
      NZW    = NRMAX
      IND    = IZERO
C
C     COMPUTE QUADRATIC EFFECT AND SIGNIFICANCE LEVEL.
C
      IF (IWT.EQ.ITWO) GO TO 30
       CALL QUADLS (A(101),RC(K),SA(ISUBSA),IZERO,NRMAX,
     1              SA(ISUBSA),QCOEF,F)
      A(62) = QCOEF
      CALL QFORF (RONE,FLOAT(NZW-ITWO),F,A(63))
C
C     NO WEIGHTS GIVEN OR USED.
C
      CALL SUMMAL (RC(K),NRMAX,SUM)
      IF (NRMAX.EQ.IONE) SUM = RC(K)
C
C     SUM =  SUM OF X(I) OR DATA.
C
      WT     = RZERO
      LSUBSA = ISUBSA
      KSUBSA = ISIZSA + ISUBSA
      JSUBSA = ITWO * ISIZSA + ISUBSA
C
      DO 10 I=1,NRMAX
        SA(KSUBSA) = ABS(RC(K))
        SA(JSUBSA) = RC(K)**2
        SA(LSUBSA) = RC(K)
        LSUBSA     = LSUBSA + IONE
        KSUBSA     = KSUBSA + IONE
        JSUBSA     = JSUBSA + IONE
        K          = K + IONE
        M          = M + IONE
        WT         = WT + RONE
  10  CONTINUE
C
      KSUBSA = ISIZSA + ISUBSA
      CALL SUMMAL (SA(KSUBSA),NRMAX,ASUMWT)
      IF (NRMAX.EQ.IONE) ASUMWT = SA(KSUBSA)
      JSUBSA = ITWO * ISIZSA + ISUBSA
      CALL SUMMAL (SA(JSUBSA),NRMAX,S2)
      IF (NRMAX.EQ.IONE) S2 = SA(JSUBSA)
      LSUBSA = ISUBSA
      DO 20 I=1,NRMAX
        SA(KSUBSA) = SA(LSUBSA)
        SA(JSUBSA) = RONE
        KSUBSA     = KSUBSA + IONE
        LSUBSA     = LSUBSA + IONE
        JSUBSA     = JSUBSA + IONE
  20  CONTINUE
C
C     ASUMWT =  SUM OF ABS(X(I)).
C        S2 =  SUM OF X(I)**2
C        SA(2*ISIZSA+I) VECTOR CONTAINS X(I).
C
      SUMWT = SUM
      RETURN
C
C     ..................................................................
C
C     WEIGHTS ARE GIVEN.
C
  30  MA = IARGS(2)
C
C     COMPUTE QUADRATIC EFFECT AND SIGNIFICANCE LEVEL.
C
      CALL QUADLS (A(101),RC(K),RC(MA),IONE,NRMAX,SA(ISUBSA),QCOEF,F)
      A(62) = QCOEF
C
      NEGWT = IZERO
C
C     USE ONLY NON-ZERO WEGHTS AND CORRESPONDING X(I).
C
      KSUBSA = ITWO * ISIZSA + M
      JSUBSA = ITHRE * ISIZSA + M
      LSUBSA = ISIZSA + M
      DO 50 I=1,NRMAX
        IF (RC(MA).EQ.RZERO) GO TO 40
        IF (RC(MA).LT.RZERO) NEGWT = NEGWT + IONE
        SA(KSUBSA) = RC(K) * RC(MA)
        SA(JSUBSA) = RC(MA)
        SA(LSUBSA) = RC(K)
        KSUBSA     = KSUBSA + IONE
        JSUBSA     = JSUBSA + IONE
        LSUBSA     = LSUBSA + IONE
        M          = M + IONE
  40    K          = K + IONE
        MA         = MA + IONE
  50  CONTINUE
      NZW = M - IONE
C
C     NZW =  NUMBER OF NON-ZERO WEIGHTS.
C
      IF (NZW.GT.IZERO .AND. NEGWT.EQ.IZERO) GO TO 60
      IF (NEGWT.GT.IZERO .AND. ISTORE.EQ.IONE) CALL ERROR (223)
      IF (NEGWT.GT.IZERO .AND. ISTORE.EQ.ITWO) CALL ERROR (15)
      IF (NZW.EQ.IZERO .AND. ISTORE.EQ.IONE) CALL ERROR (224)
      IF (NZW.EQ.IZERO .AND. ISTORE.EQ.ITWO) CALL ERROR (46)
      IND = IONE
      RETURN
C
C     ..................................................................
C
  60  CALL SUMMAL (SA(ISUBSA),NZW,SUM)
      IF (NZW.EQ.IONE) SUM = SA(ISUBSA)
      KSUBSA = ISIZSA + ISUBSA
      CALL SUMMAL (SA(KSUBSA),NZW,SUMWT)
      IF (NZW.EQ.IONE) SUMWT = SA(KSUBSA)
      JSUBSA = ITWO * ISIZSA + ISUBSA
      CALL SUMMAL (SA(JSUBSA),NZW,WT)
      IF (NZW.EQ.IONE) WT = SA(JSUBSA)
C
C     SUM =  SUM OF X(I).
C        SUMWT =  SUM OF X(I)*WEIGHTS.
C        WT =  SUM OF NON-ZERO WEIGHTS.
C
      CALL QFORF (RONE,FLOAT(NZW-ITWO),F,A(63))
      LSUBSA = ISUBSA
      DO 70 I=1,NZW
        SA(KSUBSA) = SA(KSUBSA) * SA(LSUBSA)
        KSUBSA     = KSUBSA + IONE
        LSUBSA     = LSUBSA + IONE
  70  CONTINUE
      KSUBSA = ISIZSA + ISUBSA
      CALL SUMMAL (SA(KSUBSA),NZW,S2)
      IF (NZW.EQ.IONE) S2 = SA(KSUBSA)
C
C     S2 =  SUM OF X(I)**2*WEIGHTS
C
      JSUBSA = ITWO * ISIZSA + ISUBSA
      LSUBSA = ISUBSA
      DO 80 I=1,NZW
        SA(KSUBSA) = ABS (SA(LSUBSA)) * SA(JSUBSA)
        KSUBSA     = KSUBSA + IONE
        JSUBSA     = JSUBSA + IONE
        LSUBSA     = LSUBSA + IONE
  80  CONTINUE
C
C     ASUMWT =  SUM OF ABS(X(I))*WEIGHTS.
C
      KSUBSA = ISIZSA + ISUBSA
      CALL SUMMAL (SA(KSUBSA),NZW,ASUMWT)
      IF (NZW.EQ.IONE) ASUMWT = SA(KSUBSA)
C
C     SA(2*ISIZSA+I) - X(I) OF NON ZERO WEIGHTS.
C
      LSUBSA = ISUBSA
      DO 90 I=1,NZW
        SA(KSUBSA) = SA(LSUBSA)
        KSUBSA     = KSUBSA + IONE
        LSUBSA     = LSUBSA + IONE
  90  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STASTR
      SUBROUTINE STASTR (NZW)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STASTR V 7.00  7/25/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     STORE RESULTS FOR STATISTICAL ANALYSIS INSTRUCTIONS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORTORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -           1967.
C                   CURRENT VERSION -     JULY, 1991.
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
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NSTORE / 73 /
C
C     ==================================================================
C
      ISIZSA = IDIV (NS-IHRD,IFOUR,IXND)
      ISUBSA = ISIZSA + 101
      ISUBMB = ISIZSA + ISUBSA - IONE
      ISUBI1 = ISUBSA
      ISUBI3 = ISUBSA + ITWO * ISIZSA
      IF (NARGS.EQ.ITWO .OR. NARGS.EQ.ITHRE) GO TO 10
      L = IARGS(NARGS-3)
      M = IARGS(NARGS-2)
      K = IARGS(NARGS-1)
      J = IARGS(NARGS)
      GO TO 20
C
  10  L = IARGS(NARGS)
      M = L + NROW
      K = M + NROW
      J = K + NROW
  20  DO 30 I=1,NZW
        MB     = INT(A(I+100)+0.5) + ISUBMB
        RC(K)  = A(MB)
        RC(M)  = A(ISUBI1)
        RC(J)  = A(ISUBI3)
        ISUBI1 = ISUBI1 + IONE
        ISUBI3 = ISUBI3 + IONE
        M      = M + IONE
        K      = K + IONE
        J      = J + IONE
  30  CONTINUE
C
      IF (NZW.EQ.NRMAX) GO TO 50
      NZW1 = NZW + IONE
      DO 40 I=NZW1,NRMAX
        RC(M) = RZERO
        RC(K) = RZERO
        RC(J) = RZERO
        M = M + IONE
        K = K + IONE
        J = J + IONE
  40  CONTINUE
C
  50  NTOP = MIN0 (NSTORE,NROW)
      DO 60 I=1,NTOP
        RC(L) = A(I)
        L = L + IONE
  60  CONTINUE
C
      IF (NRMAX.LE.NSTORE) RETURN
      IBEG = NSTORE + IONE
      DO 70 I=IBEG,NRMAX
        RC(L) = RZERO
        L = L + IONE
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STATIS
      SUBROUTINE STATIS
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STATIS V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE STATISTICAL ANALYSIS INSTRUCTION.
C
C     FORMS OF INSTRUCTION -
C       STATISTICAL  ANAL OF COL (C)
C       STATISTICAL  ANAL OF COL (C) PUT IN COL (C) AND NEXT 3 COLS
C       STATISTICAL  ANAL OF COL (C) WTS (C) STORE (C) AND NEXT 3 COLS
C       STATISTICAL  ANAL OF COL (C) WTS IN COL (C) DONT STORE IN (-C)
C       STATISTICAL  ANAL OF COL (C) PUT IN COLS (C), (C), (C) AND (C)
C       STATISTICAL  ANAL OF COL (C) WTS IN (C) STORE IN (C),(C),(C),(C)
C       SSTATISTICAL ANAL OF COL (C) PUT IN (C) AND NEXT THREE COLS
C       SSTATISTICAL ANAL OF COL (C) WTS (C) STORE (C) AND NEXT 3 COLS
C       SSTATISTICAL ANAL OF COL (C) PUT IN COLS (C), (C), (C) AND (C)
C       SSTATISTICAL ANAL OF COL (C) WTS IN (C) PUT IN (C),(C),(C),(C)
C
C               ORIGINALLY WRITTEN BY SALLY T. PEAVY.
C               REVISED BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -        1967.
C                   CURRENT VERSION - APRIL, 1992.
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
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             AKCON(4), AT5(6), BCON(4), BKCON(4), CK1(6)
      REAL             R(3), RK2(6), XK1(7),  YK2(7)
      REAL             ADEV, AK, ASUMWT, COVER, DELX, DEV, DEVI, DEVWT
      REAL             DEV2, DEV3, DEV4, DIF, RANGE, SUM, SUMWT
      REAL             S2, T, TA, TK1, TK2, TSUM, TWSUM, TWT
      REAL             T50MN, T50MX, T95MN, T95MX, T99MN, T99MX
      REAL             V, VNU, V2, V2M1, WT, Y, Y1, Z, Z1
      REAL             FDIV, FDPCON, FSQRT
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF, SPCG, SPCH
C
C     ..................................................................
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DSUM, DWA(1)
C
C     ..................................................................
C
      EQUIVALENCE (DEV2,R(1)), (DEV3,R(2)), (DEV4,R(3))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA AKCON(1), AKCON(2), AKCON(3), AKCON(4) /
     1     -0.51732, -0.61863, -0.04122,  0.55897 /
C
      DATA   AT5(1),   AT5(2),   AT5(3),   AT5(4),   AT5(5),   AT5(6) /
     1    1.9599640,2.3722712,2.8224986,2.5558497,1.5895341,0.7328982 /
C
      DATA  BCON(1),  BCON(2),  BCON(3),  BCON(4) /
     1      3.6948 , -1.6561 ,  0.406  ,  2.7764  /
C
      DATA BKCON(1), BKCON(2), BKCON(3), BKCON(4) /
     1      7.45894, -0.89082,  0.61522,  2.56706 /
C
      DATA   CK1(1),   CK1(2),   CK1(3),   CK1(4),   CK1(5),   CK1(6)  /
     1     -0.70285, -0.02006, -0.01687, -0.01447, -0.01263,  0.67839  /
C
      DATA   RK2(1),   RK2(2),   RK2(3),   RK2(4),   RK2(5),   RK2(6)  /
     1     -1.49016,  0.13384,  0.09764,  0.07476,  0.05931,  1.68641  /
C
      DATA  XK1(1),  XK1(2),    XK1(3),     XK1(4) /
     1  -40.343875, 14.1365, -2.743342, 0.84143957 /
      DATA  XK1(5),        XK1(6),      XK1(7) /
     1    0.001066, -6.3701507E-6, 1.749484E-8 /
C
      DATA  YK2(1),     YK2(2),    YK2(3),    YK2(4) /
     1   50.298233, -11.395210, 6.0537922, 1.1542370 /
      DATA      YK2(5),       YK2(6),       YK2(7) /
     1   -9.8051279E-4, 5.5609437E-6, 1.4584433E-8 /
C
      DATA SPCA /   1.0E-5 /
      DATA SPCB /   0.25   /
      DATA SPCC /   0.75   /
      DATA SPCD /  12.0    /
      DATA SPCE /  16.0    /
      DATA SPCF /  29.0    /
      DATA SPCG /  90.0    /
      DATA SPCH / 100.0    /
C
C     ==================================================================
C
      ND = ISIGD
      DO 10 I=1,60
        A(I) = RZERO
  10  CONTINUE
C
      NXC   = IARGS(1) 
      NXW   = IARGS(2)
      A(1)  = NRMAX
C
C     ERROR CHECKING.
C
      CALL STAERR (IWT,ISTORE,IND)
C
C     IF IND = 1, INFORMATIVE DIAGNOSTIC HAS BEEN PRINTED AND
C       COMPUTATION IS NOT NECESSARY.
C
      IF (IND.EQ.IONE) RETURN
      IF (NERROR.NE.IZERO) RETURN
      ISIZSA = IDIV (NS-IHRD,IFOUR,IXND)
      ISUBSA = ISIZSA + 101
C
C     ..................................................................
C
C     COMPUTE QUADRATIC EFFECT AND SIGNIFICANCE LEVEL.
C
      CALL STAQDS (IWT,ISTORE,NZW,WT,SUM,SUMWT,S2,ASUMWT,A(101),INDX)
C
C     IF INDX = 1, INFORMATIVE DIAGNOSTIC HAS BEEN PRINTED
C         AND DO NOT CONTINUE WITH STATIS.
C
      IF (INDX.NE.IZERO) RETURN
C
C     A(1),...,A(100) CONTAIN RESULTS FOR AUTOMATIC PRINTING
C        AND STORING, IF SO REQUIRED.
C
C     A( 2) =  NO. OF NON-ZERO WIGHTS
C     A( 3) =  UN-WEIGHTED MEAN
C     A( 4) =  WEIGHTED MEAN
C     A(24) =  EXPECTED NO. OF RUNS
C     A(25) =  S.D. OF NO. OF RUNS
C     A(39) =  WEIGHTED SUM OF SQUARES
C     A(42) =  WEIGHTED SUM OF ABSOLUTE VALUES
C     A(43) =  WEIGHTED AVERAGE ABSOLUTE VALUES
C
      A( 2) = NZW
      A( 3) = FDIV (SUM,A(2),IND)
      A( 4) = FDIV (SUMWT,WT,IND)
      A(24) = FDIV (RTWO*A(2)-RONE,RTHRE,IND)
      A(25) = FSQRT (FDIV (SPCE*A(2)-SPCF,SPCG,IND))
      A(39) = S2
      A(42) = ASUMWT
      A(43) = FDIV (ASUMWT,WT,IND)
      IXNM1 = NZW - IONE
C
C     SORT X IN A(K), K=1 ... NRMAX, AND PUT HIERARCHY IN ISA(I).
C
      CALL SORT (A(ISUBSA),A(101),NZW,IONE)
C
C     COMPUTE AUTOCORRELATION COEFFICIENT.
C
      JSUBSA = ISUBSA + ISIZSA
      CALL AUTCOR (A(JSUBSA),NZW,A(64))
C
C     COMPUTE NUMBER OF DISTINCT MEASUREMENTS AND
C        CONVERT REAL HIERARCHY TO INTEGER HIERARCHY.
C
      NDIFFM = NZW
      KSUBSA = ISUBSA
      DO 30 I=1,NZW
        IF (I.EQ.IONE) GO TO 20
        IF (A(KSUBSA).NE.A(KSUBSA-1)) GO TO 20
        NDIFFM = NDIFFM - IONE
  20    KSUBSA = KSUBSA + IONE
  30  CONTINUE
      IF (NDIFFM.GT.IONE) GO TO 40
        IF (ISTORE.EQ.IONE) CALL ERROR (248)
        IF (ISTORE.EQ.ITWO) CALL ERROR ( 14)
        RETURN
C
C     COMPUTE INTER-QUARTILE RANGE = UPPER HINGE - LOWER HINGE.
C
  40  CALL PCTILE (A(ISUBSA),NZW,SPCB,A(49))
      CALL PCTILE (A(ISUBSA),NZW,SPCC,A(50))
      A(61) = A(50) - A(49)
C
C     COMPUTE SECOND MINIMUM AND SECOND MAXIMUM.
C
      A(44)  = A(ISUBSA+1)
      KSUBSA = ISIZSA + NZW - IONE + 100
      A(45)  = A(KSUBSA)
C
      NALPHA = SPCB * A(2)
      IXA    = NALPHA + IONE
      IXNA   = NZW - NALPHA
      CALL DSUMAL (DWA,IZERO,DSUM)
C
      KSUBSA = ISIZSA + IXA +100
      DO 50 I=IXA,IXNA
        M      = ITHRE * ISIZSA + INT(A(I+100) + 0.5 + SPCA) + 100
        DWA(1) = DBLE (A(KSUBSA)) * DBLE (A(M))
        CALL DSUMAL (DWA,-IONE,DSUM)
        KSUBSA = KSUBSA + IONE
  50  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      TWSUM  = FDPCON (DSUM)
      I      = IXNA - IXA + IONE
      KSUBSA = ISIZSA + IXA + 100
      CALL SUMMAL (A(KSUBSA),I,TSUM)
      IF (I.EQ.IONE) TSUM = A(KSUBSA)
      CALL DSUMAL (DWA,IZERO,DSUM)
      DO 60 I=IXA,IXNA
        M      = ITHRE * ISIZSA + INT(A(I+100) + 0.5 + SPCA) + 100
        DWA(1) = DBLE(A(M))
        CALL DSUMAL (DWA,-IONE,DSUM)
  60  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      TWT = FDPCON(DSUM)
C
C     A( 7) = 25 PCT UN-WEIGHTED TRIMMED MEAN
C     A( 8) = 25 PCT WEIGHTED TRIMMED MEAN
C
      A( 7) = FDIV (TSUM,A(2)-RTWO*FLOAT(NALPHA),IND)
      A( 8) = FDIV (TWSUM,TWT,IND)
      N2    = IDIV (NZW+IONE,ITWO,IND) + ISIZSA + 100
C
C     A( 5) = MEDIAN
C     A( 6) = MID-RANGE
C     A(11) = RANGE
C     A(34) = MINUMUM OF X(I)
C     A(35) = MAXIMUM OF X(I)
C
      A( 5) = A(N2)
      IF (MOD(NZW,ITWO).EQ.IZERO) A(5) = FDIV (A(5)+A(N2+1),RTWO,IND)
      KSUBSA = ISIZSA + NZW + 100
      A( 6)  = FDIV (A(ISUBSA)+A(KSUBSA),RTWO,IND)
      A(11)  = A(KSUBSA) - A(ISUBSA)
      A(34)  = A(ISUBSA)
      A(35)  = A(KSUBSA)
C
C     COMPUTE AND STORE FREQUENCY DISTRIBUTION IN A(51), ..., A(60).
C
      RANGE = A(11)
      CALL STAFRQ (NZW,RANGE,A(ISUBSA),A(51),DELX)
C
C     COMPUTE DIFFERENCES BETWEEN SUCCESSIVE SORTED X(I)
C        AND PUT IN A(3*ISIZSA+I).
C
      KSUBSA = ITWO * ISIZSA + ISUBSA
      CALL STADIF (NZW,A(ISUBSA),A(KSUBSA))
C
C     COMPUTE RANKS AND STORE IN A(I).
C
      CALL STARNK (NZW,A(101),A(KSUBSA),A(ISUBSA))
      ICI    = IZERO
      IPLUS  = IZERO
      IMINUS = IZERO
      IDRUNS = IZERO
      IC     = IZERO
C
      CALL DSUMAL (DWA,IZERO,DSUM)
      AK   = RONE
      KWT  = IARGS(2)
      NRXX = KWT + NRMAX - IONE
      TA   = RONE
C
C     A(3*ISIZSA+I) =  X(I)-WEIGHTED MEAN=RESIDUALS.
C
      KSUBSA = ISIZSA + ISUBSA
      LSUBSA = KSUBSA + ISIZSA
      DO 140 I=1,NZW
        T          = A(KSUBSA) - A(4)
        KSUBSA     = KSUBSA + IONE
        A(LSUBSA) = T
        LSUBSA     = LSUBSA + IONE
        IF (IWT.EQ.IONE) GO TO 90
  70    IF (RC(KWT).NE.RZERO) GO TO 80
        IF (KWT.GE.NRXX) GO TO 100
        KWT = KWT + IONE
        GO TO 70
C
  80    TA = RC(KWT)
  90    DWA(1) = DBLE (TA) * DBLE (T)**2
        CALL DSUMAL (DWA,-IONE,DSUM)
 100    IF (T.LT.RZERO) GO TO 110
        IPLUS = IPLUS + IONE
        ICI = IONE
        GO TO 120
C
 110    IMINUS = IMINUS + IONE
        ICI = -IONE
 120    IF (IC.EQ.ICI) GO TO 130
        IC = ICI
        IDRUNS = IDRUNS + IONE
 130    KWT = KWT + IONE
 140  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      DEVWT  = FDPCON(DSUM)
      KSUBSA = ITWO * ISIZSA + ISUBSA
      CALL SUMMAL (A(KSUBSA),NZW,DEV)
      IF (NZW.EQ.IONE) DEV = A(KSUBSA)
      CALL DSUMAL (DWA,IZERO,DSUM)
      DO 150 I=1,NZW
        DWA(1) = DBLE(ABS(A(KSUBSA)))
        KSUBSA = KSUBSA + IONE
        CALL DSUMAL (DWA,-IONE,DSUM)
 150  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      ADEV = FDPCON (DSUM)
      DO 170 IV=2,4
        CALL DSUMAL (DWA,IZERO,DSUM)
        KSUBSA = ITWO * ISIZSA + ISUBSA
        DO 160 I=1,NZW
          DWA(1) = DBLE(A(KSUBSA)) ** IV
          KSUBSA = KSUBSA + IONE
          CALL DSUMAL (DWA,-IONE,DSUM)
 160    CONTINUE
        CALL DSUMAL (DWA,IONE,DSUM)
        R(IV-1) = FDPCON (DSUM)
 170  CONTINUE
C
C     A(13) = VARIANCE
C     A( 9) = STANDARD DEVIATION
C     A(10) = STANDARD DEVIATION OF MEAN
C     A(14) = COEFFICIENT OF VARIANCE
C     A(28) = NO. OF PLUS SIGNS
C     A(29) = NO. OF MINUS SIGNS
C     A(31) = EXPECTED NO. OF RUNS
C     A(32) = S.D. OF RUNS
C     A(36) = BETA ONE
C     A(37) = BETA TWO
C     A(38) = WEIGHTED SUM OF X(I)
C     A(40) = WEIGHTED SUM OF DEVATION SQUARED
C     A(30) = NO. OF RUNS
C     A(33) = (NO. OF RUNS - EXPECTED NO. OF RUNS)/S.D. OF RUNS
C     A(19) = SLOPE
C     A(20) = S.D. OF SLOPE
C     A(21) = T (SLOPE/S.D. OF SLOPE)
C
      CALL DSUMAL (DWA,IZERO,DSUM)
      KSUBSA = ITWO * ISIZSA + ISUBSA
      DO 180 I=1,NZW
        DWA(1) = DBLE (A(KSUBSA)) * DBLE (AK)
        KSUBSA = KSUBSA + IONE
        AK = AK + RONE
        CALL DSUMAL (DWA,-IONE,DSUM)
 180  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      DEVI  = FDPCON (DSUM)
      A(13) = FDIV (DEVWT,A(2)-RONE,IND)
      A( 9) = FSQRT (A(13))
      A(10) = FDIV (A(9),FSQRT(WT),IND)
      A(14) = SPCH * FDIV (A(9),A(4),IND)
      A(14) = ABS (A(14))
      A(28) = IPLUS
      A(29) = IMINUS
      A(31) = RONE + FDIV (RTWO*A(28)*A(29),A(2),IND)
      A(32) = FSQRT(FDIV(RTWO*A(28)*A(29)*(RTWO*A(28)*A(29)-A(28)-A(29))
     1             ,(A(28)+A(29))**2*(A(2)-RONE),IND))
      Y     = FDIV (DEV3,A(2),IND)
      Z     = FDIV (A(2)-RONE,A(2),IND)
      A(36) = FDIV (Y**2,(Z*A(13))**3,IND)
      A(48) = SIGN (FSQRT(A(36)),DEV3)
      Y     = FDIV (DEV4,A(2),IND)
      A(37) = FDIV (Y,(Z*A(13))**2,IND)
      A(38) = SUMWT
      A(40) = DEVWT
      A(30) = IDRUNS
      A(33) = FDIV (A(30)-A(31),A(32),IND)
      A(19) = FDIV (SPCD*DEVI,A(2)*(A(2)**2-RONE),IND)
      A(20) = FSQRT ((FDIV (RONE,A(2)-RTWO,IND)) * (SPCD *
     1                FDIV (DEV2,A(2)*(A(2)**2-RONE),IND)-A(19)**2))
      A(21) = FDIV (A(19),A(20),IND)
C
      TA   = A(21) * A(21)
      CALL QFORF (RONE,A(2)-RTWO,TA,A(22))
      DIF    = IZERO
      IRUN   = IONE
      KSUBSA = ISIZSA + ISUBSA + IONE
      DO 190 I=2,NZW
        TA     = A(KSUBSA) - A(KSUBSA-1)
        KSUBSA = KSUBSA + IONE
        IF (TA.NE.RZERO) GO TO 200
 190  CONTINUE
C
 200  CALL DSUMAL (DWA,IZERO,DSUM)
      KSUBSA = ISIZSA + ISUBSA + IONE
      DO 210 I=2,NZW
        T      = A(KSUBSA) - A(KSUBSA-1)
        KSUBSA = KSUBSA + IONE
        DWA(1) = DBLE (T) ** 2
        CALL DSUMAL (DWA,-IONE,DSUM)
        IF (TA*T.GE.RZERO) GO TO 210
        TA = T
        IRUN = IRUN + IONE
 210  CONTINUE
C
      CALL DSUMAL (DWA,IONE,DSUM)
      DIF = FDPCON (DSUM)
C
C     A(23) = NO. OF  RUNS UP AND DOWN
C     A(26) = MEAN SQ. SUCCESSIVE DIFFERENCE
C     A(27) = MEAN SQ. SUCCESSIVE DIFFERNCE/VARIANCE
C     A(41) = STUDENTS'S T
C     A(12) = MEAN DEVIATION
C
      A(23) = IRUN
      A(26) = FDIV (DIF,A(2)-RONE,IND)
      A(27) = FDIV (A(26),A(13),IND)
      A(41) = FDIV (A(4)*FSQRT(WT),A(9),IND)
      A(12) = FDIV (ADEV,A(2),IND)
C
      NU  = NZW - IONE
      VNU = NU
      T   = RZERO
      TK1 = RZERO
      TK2 = RZERO
      IF (NU.GE.IFIVE) GO TO 250
      DO 240 I=1,4
        II = I
        IF (NU.NE.IZERO) GO TO 220
        V = RZERO
        GO TO 230
 220    V = IDIV (II,NU,IND)
 230    T = T + BCON(I) * V
        TK2 = BKCON(I) * V + TK2
        TK1 = TK1 + AKCON(I) * V
 240  CONTINUE
      GO TO 300
C
 250  Y  = FDIV (AT5(6),VNU,IND)+AT5(5)
      Z  = FDIV (Y,VNU,IND) + AT5(4)
      Y1 = FDIV (Z,VNU,IND) + AT5(3)
      Z1 = FDIV (Y1,VNU,IND) + AT5(2)
      T  = FDIV (Z1,VNU,IND) + AT5(1)
      IF (NU.GT.ITEN) GO TO 270
      DO 260 I=1,6
        V   = IDIV (I+IFOUR,NU,IND)
        TK1 = TK1 + CK1(I) * V
        TK2 = TK2 + RK2(I) * V
 260  CONTINUE
      GO TO 300
C
 270  IF (NU.GT.IHRD) GO TO 290
      DO 280 I=1,7
        V   = VNU ** (I-4)
        TK1 = TK1 + XK1(I) * V
        TK2 = TK2 + YK2(I) * V
 280  CONTINUE
      GO TO 300
C
 290  V2   = FSQRT (RTWO*VNU)
      V2M1 = FSQRT (RTWO*VNU-RONE)
      TK1  = FDIV (V2,AT5(1)+V2M1,IND)
      TK2  = FDIV (V2,-AT5(1)+V2M1,IND)
C
C     A(15) = LOWER CONFIDENCE LIMIT, MEAN
C     A(16) = UPPER CONFIDENCE LIMIT, MEAN
C     A(17) = LOWER CONFIDENCE LIMIT, S.D.
C     A(18) = UPPER CONFIDENCE LIMIT, S.D.
C
 300  A(15) = A(4) - T * A(10)
      A(16) = A(4) + T * A(10)
      A(17) = TK1 * A(9)
      A(18) = TK2 * A(9)
C
C     COMPUTE 95 PERCENT CONFIDENCE INTERVAL FOR THE MEDIAN.
C
      A(65) = RZERO
      A(66) = RZERO
      IF (NZW.LT.IFIVE) GO TO 310
      CALL MEDCI (NZW,I,J)
      K     = ITWO * ISIZSA + INT(A(I+100) + 0.5 + SPCA) + 100
      A(65) = A(K)
      K     = ITWO * ISIZSA + INT(A(J+100) + 0.5 + SPCA) + 100
      A(66) = A(K)
C
C     COMPUTE TOLERANCE INTERVALS.
C
 310  KSUBSA = ISIZSA + ISUBSA
      CALL TOLLIM (A(KSUBSA),NZW,T50MN,T50MX,T95MN,T95MX,T99MN,T99MX,
     1             COVER,IND)
      A(67) = T50MN
      A(68) = T50MX
      A(69) = T95MN
      A(70) = T95MX
      A(71) = T99MN
      A(72) = T99MX
      A(73) = SPCH * COVER
C
C     COMPUTE (MEAN-MINIMUM)/S.D. AND (MAXIMUM-MEAN)/S.D.
C
      A(99) = A(3)
      IF (IWT.EQ.ITWO) A(99) = A(4)
      A(46) = FDIV (A(99)-A(34),A(9),IND)
      A(47) = FDIV (A(35)-A(99),A(9),IND)
C
      IF (L2.EQ.ITWO) GO TO 320
C
C     CALL STAPRT TO DO AUTOMATIC PRINTING.
C
      CALL STAPRT (DELX,ND,NDIFFM,NXC,NZW,IWT,IXNM1,NXW)
C
C     CHECK IF STORAGE IS REQUESTED.
C
 320  IF (ISTORE.EQ.IONE) RETURN
C
C     CALL STASTR TO STORE RESULTS.
C
      CALL STASTR (NZW)
      RETURN
C
C     ==================================================================
C
      END
*STREDD
      SUBROUTINE STREDD (L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STREDD V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     STORE (DIVIDED) DIFFERENCES COMPUTED BY PROGRAM UNIT DIFFER.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
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
C     ==================================================================
C
      IF (L2.GE.9) CALL CHKCOL
C
      IBEG = L + IONE
      IEND = NARGS
      IF (IEND.LT.IBEG) RETURN
      JEND = NRMAX
      N    = NRMAX + IONE
      DO 20 I=IBEG,IEND
        JEND = JEND - IONE
        M    = IARGS(I)
        DO 10 J=1,JEND
          RC(M) = A(N)
          M = M + IONE
          N = N + IONE
  10    CONTINUE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
