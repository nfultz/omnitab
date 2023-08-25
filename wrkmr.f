**MATRIX
      SUBROUTINE MATRIX
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MATRIX V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     L2=1  ADD MATRICES A+B  MADD A(,) N,M, TO B(,) N,M AND STORE IN C
C     L2=2  SUB MATRICES A-B  MSUB A(,) N,M FROM B(,)N,M AND STORE IN C
C     L2=3  TRANSPOSE MATRIX MTRANS A(,) N,M AND STORE IN C(,)
C              TRANSPOSE ARRAY ATRANS A(,) N,M AND STORE IN C(,)
C     L2=4  ARRAY ADD       AADD
C     L2=5  ARRAY SUBTRACT  ASUB
C     L2=6  ARRAY MULTIPLY  AMULT
C     L2=7  ARRAY DIVIDE    ADIV
C     L2=8  ARRAY RAISE     ARAISE
C
C                GENERAL FORMS FOR ARRAY OPERATIONS
C                   A(,) N,M B(,) N,K STORE IN C(,) ARRAY BY ARRAY
C                   A(,) N,M B(,)     STORE IN C(,) ARRAY BY ARRAY
C                   A(,) N,M      K   STORE IN C(,) ARRAY BY COLUMN
C                   A(,) N,M      X   STORE IN C(,) ARRAY BY CONSTANT
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
      REAL             FDIV, FEXP2
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C     CHECK TO SEE IF WE HAVE CORRECT NUMBER OF ARGUMENTS.
C
      NP = NARGS
      IF (L2.EQ.ITHRE) GO TO 10
      IF (L2.GT.ITHRE) GO TO 20
      IF (NARGS.NE.8 .AND. NARGS.NE.ITEN) GO TO 30
      GO TO 40
C
  10  IF (NARGS.NE.6) GO TO 30
      GO TO 40
C
  20  IF (NARGS.LT.7 .OR. NARGS.GT.ITEN .OR. NARGS.EQ.9) GO TO 30
      GO TO 40
C
  30  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS.
C
  40  IF (L2.GT.ITHRE .AND. NARGS.EQ.7) GO TO 60
  50  J = NARGS
      CALL CKIND (J)
      IF (J.EQ.IZERO) GO TO 70
      CALL ERROR (3)
      GO TO 70
C
  60  ISAVE = KIND(NARGS)
      KIND(NARGS)   = KIND(NARGS-2)
      KIND(NARGS-2) = KIND(NARGS-1)
      KIND(NARGS-1) = ISAVE
      NARGS = NARGS - IONE
      GO TO 50
C
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT, IF THEY ARE GIVEN.
C
  70  IF (NP.NE.ITEN) GO TO 80
      IF (IARGS(3).EQ.IARGS(7) .AND. IARGS(4).EQ.IARGS(8)) GO TO 80
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
C     CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE.
C
  80  IF (L2.LT.ITHRE .OR. L2.GT.ITHRE .AND. KIND(NP).EQ.IZERO) GO TO 90
      J = ITWO
      GO TO 100
C
  90  J = ITHRE
      IARGS(12) = IARGS(4)
      IARGS(11) = IARGS(3)
      IF (NP.EQ.ITEN) GO TO 140
      IARGS(10) = IARGS(NP)
      IARGS( 9) = IARGS(NP-1)
 100  IF (NP.EQ.8 .OR. NP.EQ.7 .AND. KIND(NP).NE.IZERO) GO TO 120
      IF (NP.EQ.6) GO TO 110
      IARGS( 6) = IARGS(5)
      IARGS( 8) = IONE
      IARGS( 7) = IARGS(3)
      IARGS( 5) = IONE
      GO TO 140
C
 110  IARGS( 8) = IARGS(3)
      IARGS( 7) = IARGS(4)
      GO TO 140
C
 120  IF (NP.EQ.8) GO TO 130
      IARGS( 5) = IARGS(6)
      IARGS( 6) = IARGS(7)
 130  IARGS( 8) = IARGS(4)
      IARGS( 7) = IARGS(3)
 140  CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 150
      IF (J.LT.IONE) GO TO 160
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
 150  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK TO SEE IF THERE WERE PREVIOUS ERRORS.
C
 160  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     COMPUTE IN SCRATCH AREA ...
C        SUM ELEMENTS.
C        SUBTRACT ELEMENTS.
C        PRODUCTS AND QUOTIENTS FORMED USING DOUBLE PRECISION.
C        TRANSPOSE.
C
      IROW   = IARGS(3)
      ICOL   = IARGS(4)
      NROWPP = NROW
      NROWP  = NROW
      IBP    = IARGS(5)
      IF (L2.LT.ITHRE) GO TO 170
      IF (L2.GT.ITHRE) GO TO 200
      IIB    = ICOL
      JJB    = IROW
      NROWPP = IZERO
      K      = IONE
      GO TO 190
C
 170  NROWP  = NROW
 180  IIB    = IROW
      JJB    = ICOL
      K      = IZERO
 190  IS     = IONE
      IAP    = IARGS(1)
      GO TO 230
C
 200  IF (NP.GE.8) GO TO 170
      IF (KIND(NP).EQ.IONE) GO TO 210
      GO TO 220
C
 210  IARGS(9) = IARGS(5)
 220  NROWP  = IZERO
      GO TO 180
C
 230  DO 420 J=1,JJB
C
C       COMPUTE ADDRESSES.
C
        IA = IAP + (J-IONE) * K
        IB = IBP
        DO 410 I=1,IIB
C
          GO TO (240,250,290,300,310,320,330,350), L2
C
 240      A(IS) = RC(IA) + RC(IB)
          GO TO 380
C
 250      A(IS) = RC(IA) - RC(IB)
          GO TO 380
C
 260      A(IS) = RC(IA) * RC(IB)
          GO TO 380
C
 270      IF (RC(IB).EQ.RZERO) GO TO 280
          A(IS) = FDIV (RC(IA),RC(IB),IND)
          GO TO 380
C
 280      A(IS) = RZERO
          GO TO 380
C
 290      A(IS) = RC(IA)
          IA = IA + NROW
          GO TO 400
C
 300      IF (NP.GE.8 .OR. KIND(NP).EQ.IZERO .AND. NP.LT.8) GO TO 240
          A(IS) = RC(IA) + ARGS(NP-2)
          GO TO 390
C
 310      IF (NP.GE.8 .OR. KIND(NP).EQ.IZERO .AND. NP.LT.8) GO TO 250
          A(IS) = RC(IA) - ARGS(NP-2)
          GO TO 390
C
 320      IF (NP.GE.8 .OR. KIND(NP).EQ.IZERO .AND. NP.LT.8) GO TO 260
          A(IS) = RC(IA) * ARGS(NP-2)
          GO TO 380
C
 330      IF (NP.GE.8 .OR. NP.LT.8 .AND. KIND(NP).EQ.IZERO) GO TO 270
          IF (ARGS(NP-2).EQ.RZERO) GO TO 340
          A(IS) = FDIV (RC(IA),ARGS(NP-2),IND)
          GO TO 380
C
 340      A(IS) = RZERO
          GO TO 380
C
 350      IF (NP.GE.8 .OR. NP.LT.8 .AND. KIND(NP).EQ.IZERO) GO TO 360
          IF (RC(IA).EQ.RZERO) GO TO 370
          A(IS) = FEXP2 (RC(IA),ARGS(NP-2))
          GO TO 380
C
 360      IF (RC(IA).EQ.RZERO) GO TO 370
          A(IS) = FEXP2 (RC(IA),RC(IB))
          GO TO 380
C
 370      A(IS) = RZERO
 380      IB = IB + IONE
 390      IA = IA + IONE
 400      IS = IS + IONE
 410    CONTINUE
        IAP = IAP + NROWPP
        IBP = IBP + NROWP
 420  CONTINUE
C
C     MOVE ...
C        SUMS TO WORKSHEET.
C        DIFFERENCES TO WORKSHEET.
C        ARRAY PRODUCT TO WORKSHEET.
C        ARRAY QUOTIENT TO WORKSHEET.
C        TRANSPOSE TO WORKSHEET.
C        RAISED MATRIX TO WORKSHEET.
C
      IF (L2.NE.ITHRE) GO TO 430
      ICP = IARGS(5)
      GO TO 440
 430  ICP = IARGS(9)
 440  IS  = IONE
      DO 460 J=1,JJB
        IC = ICP
        DO 450 I=1,IIB
          RC(IC) = A(IS)
          IC = IC + IONE
          IS = IS + IONE
 450    CONTINUE
        ICP = ICP + NROW
 460  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MDAMAD
      SUBROUTINE MDAMAD
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MDAMAD V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO PRE OR POST MULTIPLY A MATRIX BY A DIAGONAL MATRIX
C        STORED AS A COLUMN.
C
C     L2=1      M(AD)
C         MATRIX A IS POSTMULTIPLIED BY THE DIAGONAL D STORED IN COL I
C             GENERAL FORM OF INSTRUCTION IS ...
C      M(AD) MATRIX (R),(C) SIZE (R)X(C) BY DIAG IN COL (C) IN COL (C)
C
C     L2=2      M(AD)
C         MATRIX A IS PREMULTIPLIED BY THE DIAGONAL D STORED IN COL I
C             GENERAL FORM OF INSTRUCTION IS ...
C     M(AD) MATRIX IN (R),(C) SIZE (R)X(C) IN COL (C) PUT IN (R),(C)
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1967.
C                   CURRENT VERSION -  FEBRUARY, 1990.
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
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.7) CALL ERROR (10)
C
C     CHECK TO SEE THAT ALL ARGUMENTS ARE INTEGERS
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE
C     COMPUTE ADDRESSES OF COLUMNS
C
      IARGS(12) = IARGS(4)
      IARGS(11) = IARGS(3)
      IARGS(10) = IARGS(7)
      IARGS( 9) = IARGS(6)
      IARGS( 8) = IONE
      GO TO (10,20), L2
C
  10  IARGS( 7) = IARGS(4)
      GO TO 30
C
  20  IARGS( 7) = IARGS(3)
C
  30  IARGS( 6) = IARGS(5)
      IARGS( 5) = IONE
      J = ITHRE
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 40
      IF (J.LT.IONE) GO TO 50
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  40  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS
C
  50  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IP = IARGS(4)
      JP = IARGS(3)
      GO TO (70,60), L2
  60  I1 = IZERO
      I2 = IONE
      GO TO 80
C
  70  I1 = IONE
      I2 = IZERO
  80  IA = IARGS(1)
      IDP = IARGS(5)
      IB = IARGS(9)
      DO 100 I=1,IP
        ID = IDP
        DO 90 J=1,JP
          RC(IB) = RC(ID)*RC(IA)
          ID = ID + I2
          IA = IA + IONE
          IB = IB + IONE
  90    CONTINUE
        IB = IB + NROW - JP
        IA = IA + NROW - JP
        IDP = IDP + I1
 100  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MEIGEN
      SUBROUTINE MEIGEN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MEIGEN V 7.00 10/ 3/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE EIGENVALUES AND EIGENVECTORS.
C
C     FORM OF INSTRUCTIONS ...
C
C     MEIGEN  (R),(C) SIZE (R)X(C) STORE VALUES IN COL (C)
C     MEIGEN  (R),(C) SIZE (R)X(C) STORE VECTORS IN (R),(C)
C     MEIGEN  (R),(C) SIZE (R)X(C) PUT VALUES IN (C) VECTORS IN (R),(C)
C
C        NARGS = 5,  COMPUTE ONLY EIGENVALUES.
C        NARGS = 6,  COMPUTE ONLY EIGENVECTORS.
C        NARGS = 7,  COMPUTE EIGENVALUES AND VECTORS.
C
C               WRITTEN BY -
C                      RUTH N.VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY, 
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   APRIL, 1968.
C                   CURRENT VERSION - OCTOBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ISWCH(2)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C     CHECK TO BE SURE THAT MATRIX IS NO BIGGER THAN NS.
C
      IF (IARGS(3).LT.IARGS(4)) GO TO 130
      IF (IARGS(3).GT.IARGS(4)) GO TO 140
  10  IF (NROW*IARGS(3)+ITWO*IARGS(3).GT.NS) CALL ERROR (23)
C
C     CHECK FOR CORRECT NUMBER OF ARGUMENTS.
C
      IF (NARGS.LT.IFIVE .OR. NARGS.GT.7) CALL ERROR (10)
C
C     CHECK TO SEE IF ARGUMENTS ARE ALL INTEGERS.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (20)
C
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE.
C        COMPUTE ADDRESSES.
C
      ISWCH(2) = NARGS - IFOUR
      ISWCH(1) = IZERO
      IF (NARGS.EQ.6) GO TO 20
      IADD     = IONE
      CALL ADRESS (IFIVE,J)
      IF (J.LT.IZERO) CALL ERROR (20)
C
C     J CONTAINS ADDRESS OF COLUMN.
C
      IF (NARGS.EQ.IFIVE) GO TO 30
      IARGS(5) = IARGS(6)
      IARGS(6) = IARGS(7)
  20  IADD     = ITWO
      IARGS(7) = IARGS(3)
      IARGS(8) = IARGS(4)
      GO TO 40
C
  30  ISWCH(1) = IONE
  40  CALL MTXCHN (IADD)
      IF (IADD.GT.IONE) GO TO 50
      IF (IADD.LT.IONE) GO TO 60
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  50  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS.
C
  60  IG = IARGS(1)
C
C     CHECK TO SEE IF MATRIX IS SYMMETRIC.
C
      CALL SYMX (RC(IG),NROW,IARGS(3),K)
      IF (K.LT.IONE) GO TO 70
C
C     MATRIX IS SYMMETRIC.
C
      CALL ERROR (31)
      RETURN
C
C     ..................................................................
C
  70  IF (NERROR.NE.IZERO) RETURN
      IGP   = IARGS(5)
      ISUBE = IARGS(3) + IONE
      ISUBZ = IARGS(3) + ISUBE
      CALL TRED2 (NROW,IARGS(3),RC(IG),A(1),A(ISUBE),A(ISUBZ))
      CALL TQL2 (NROW,IARGS(3),A(1),A(ISUBE),A(ISUBZ),IND)
      N = IARGS(3)
      IF (IND.EQ.IZERO) GO TO 80
      N = IND - IONE
      NARGS = N
      CALL ERROR (256)
  80  IF (ISWCH(2).EQ.ITWO) GO TO 100
      DO 90 I=1,N
        RC(J) = A(I)
        J = J + IONE
  90  CONTINUE
C
      IF (ISWCH(2).EQ.IONE) RETURN
 100  IGPA = IGP
      DO 120 I=1,N
        IGPA = IGPA + (I-IONE) * NROW
        ISUBZZ = ISUBZ + (I-IONE) * NROW
        DO 110 II=1,N
          RC(IGPA) = A(ISUBZZ)
          IGPA = IGPA + IONE
          ISUBZZ = ISUBZZ + IONE
 110    CONTINUE
        IGPA = IGP
 120  CONTINUE
C
C     RC(IG) IS LOCATION OF MATRIX TO BE DIAGONALIZED.
C
C     IARG(3) GIVES SIZE OF MATRIX.
C
C     ISWCH(1) = 1,     IF ONLY EIGENVALUES ARE TO BE COMPUTED.
C              = 0,     COMPUTE EIGENVALUES AND EIGENVECTORS.
C
C     ISWCH(2) = NARGS-4 AND IS USED FOR STORING RESULTS.
C
C     A IS LOCATION OF SCRATCH AREA.
C
C     RC(J) TELLS WHERE TO STORE EIGENVALUES.
C
C     RC(IGP) IS WHERE EIGENVECTORS ARE STORED.
C
      RETURN
C
C     ..................................................................
C
 130  IARGS(4) = IARGS(3)
      GO TO 150
C
 140  IARGS(3) = IARGS(4)
 150  CALL ERROR (210)
      GO TO 10
C
C     ==================================================================
C
      END
*MESSGE
      SUBROUTINE MESSGE
C
C **  NBS OMNITAB 1980 VERSION 7.00  9/20/90. MESSGE V 7.00  4/28/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE WRITES OUT A SYSTEM MESSAGE TO THE USER 
C
C               WRITTEN BY -
C                      SHIRLEY G. BREMER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                      GAITHERSBURG MD,  20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1990
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
      COMMON /NRCOL/ IROW, ICOL
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
C
      INCLUDE 'WRKSCR.H' 
C
C     ==================================================================
      IPRINT = NPRNT
C
      WRITE (IPRINT,10)
      WRITE (IPRINT,20)
      WRITE (IPRINT,30)
      WRITE (IPRINT,40)
      WRITE (IPRINT,50)
      WRITE (IPRINT,70)
      WRITE (IPRINT,80)
      WRITE (IPRINT,90)
      WRITE (IPRINT,100) NRC
      WRITE (IPRINT,110) IROW, ICOL, NLENGT
      WRITE (IPRINT,120)
      WRITE (IPRINT,130)
C
10    FORMAT (5X, '*****************************************************
     1')
20    FORMAT (5X, '*                     OMNITAB 80                    *
     1'/      5X, '*                  PC  VERSION 7.00                 *
     2')
30    FORMAT (5X, '*  INTERPRETIVE SYSTEM FOR STATISTICAL & NUMERICAL  *
     1')
40    FORMAT (5X, '*                    DATA ANALYSIS                  *
     1')
50    FORMAT (5X, '*  AUTHORS: SALLY T. PEAVY AND RUTH N. VARNER       *
     1')
70    FORMAT (5X, '*  NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY   *
     1')
80    FORMAT (5X, '*****************************************************
     1')
90    FORMAT (5X, '                                                   ')
100   FORMAT (5X,'MAXIMUM SIZE OF THE WORKSHEET (ROWS X COLUMNS) = ',I7)
110   FORMAT (5X, 'DEFAULTS: (1) WORKSHEET = ',I4,' ROWS X ', I3,' COLUM
     1NS,'/15X,'(2) INTERACTIVE, CRT AND BRIEF MODE ON,'/15X,'(3) PAGE L
     2ENGTH IS ',I3,'.')
120   FORMAT (5X, '                                                   ') 
130   FORMAT (5X, '>>>>> NOTE: ENTER THE INSTRUCTION OMNITAB <<<<<')
C     ==================================================================
C
      RETURN
      END
*MISC2
      SUBROUTINE MISC2
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MISC2 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 1,  CLOSE UP ROWS HAVING (K) IN COLUMNS (C), ... (C), ETC
C     L2 = 2,  COUNT LENGTH OF COLUMN (C), PUT IN COLUMN (C)
C     L2 = 3,  SHORTEN COL (C) FOR COL (C) = (K) PUT IN (C) AND COL (C)
C     L2 = 4,  EXPAND (E) TO (N) POWER, INTERVALS (N) STORE (C)
C     L2 = 5,  DUPLICATE (N) TIMES ARRAY (R),(C) SIZE (R)X(C) IN (R),(C)
C
C     IN EXPAND, THE POWERS DO NOT HAVE TO BE INTEGERS.
C
C               WRITTEN BY -
C                      CARLA G. MESSINA,
C                      NSRDC,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JULY, 1967.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ARG1, CC, TOLER
      REAL             FEXP2
C
C     ==================================================================
C
      IF (NARGS.GE.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  GO TO (20,40,20,210,360), L2
C
  20  IF (KIND(L2).GT.IZERO) GO TO 30
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  30  KIND(L2) = IZERO
      ARG1 = ARGS(L2)
      IARGS(L2) = IARGS(L2+1)
  40  CALL CHKCOL
      DO 50 I=1,NARGS
        IARGS(I) = IARGS(I) - IONE
  50  CONTINUE
C
      IF (L2.LE.ITWO) GO TO 60
      IF (NARGS.EQ.IFIVE) GO TO 60
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  60  IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.GT.IZERO) GO TO 70
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  70  IF (L2.EQ.ITWO) GO TO 110
      IF (L2.GT.ITWO) GO TO 140
C
C     CLOSE UP.
C
      DO 100 J=2,NARGS
        K = IARGS(J)
        M = IONE
        DO 80 I=1,NRMAX
          J1 = K + I
          IF (RC(J1).EQ.ARG1) GO TO 80
          K1 = K + M
          RC(K1) = RC(J1)
          M = M + IONE
  80    CONTINUE
        IF (M.GT.NRMAX) GO TO 100
        DO 90 I=M,NRMAX
          J1 = K + I
          RC(J1) = RZERO
  90    CONTINUE
 100  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COUNT.
C
 110  J1 = NRMAX
      J = IARGS(1) + NRMAX
      DO 120 I=1,NRMAX
        IF (RC(J).NE.RZERO) GO TO 130
        J1 = J1 - IONE
        J  = J - IONE
 120  CONTINUE
 130  ARG1 = J1
      IARGS(2) = IARGS(2) + IONE
      CALL VECTOR (ARG1,IARGS(2))
      RETURN
C
C     ..................................................................
C
C     SHORTEN.
C
 140  IF (NRMAX.LT.ITWO) RETURN
      DO 160 K=2,NRMAX
        J1 = IARGS(2) + K
        IF (ARG1.EQ.RC(J1-1)) GO TO 170
        IF (ARG1.GT.RC(J1-1)) GO TO 150
        IF (ARG1-RC(J1)) 160,180,180
 150    IF (ARG1.LE.RC(J1)) GO TO 180
 160  CONTINUE
C
      CALL ERROR (203)
      GO TO 190
C
 170  NROLD = NRMAX
      NRMAX = K - IONE
      CALL ERROR (252)
      GO TO 190
 180  NROLD = NRMAX
      NRMAX = K
      CALL ERROR (252)
 190  DO 200 I=1,NRMAX
        K     = IARGS(1) + I
        J     = IARGS(4) + I
        M     = IARGS(5) + I
        K1    = IARGS(2) + I
        RC(M) = RC(K1)
        RC(J) = RC(K)
 200  CONTINUE
      RETURN
C
C     ..................................................................
C
C     EXPAND.
C
 210  IF (NARGS.EQ.IFOUR) GO TO 220
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 220  CALL ADRESS (IFOUR,K1)
      IF (K1.GE.IZERO) GO TO 230
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 230  IF (KIND(1).NE.IZERO) GO TO 260
      CALL ADRESS (IONE,IARGS(1))
      IF (IARGS(1).GE.IZERO) GO TO 240
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 240  K = IARGS(1) - IONE
      DO 250 I=1,NRMAX
        J = K + I
        A(I) = RC(J)
 250  CONTINUE
      GO TO 280
C
 260  DO 270 I=1,NRMAX
        A(I) = ARGS(1)
 270  CONTINUE
C
 280  IF (KIND(2).NE.IZERO) GO TO 290
      ARGS(2) = IARGS(2)
 290  IF (KIND(3).NE.IZERO) GO TO 300
      ARGS(3) = IARGS(3)
 300  IF (ARGS(2)*ARGS(3).GT.RZERO) GO TO 310
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 310  IF (ABS(ARGS(3)).LE.ABS(ARGS(2))) GO TO 320
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 320  IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.GT.IZERO) GO TO 330
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
 330  CC = ARGS(3)
 340  DO 350 I=1,NRMAX
        K = K1 - IONE + I
        RC(K) = FEXP2 (A(I),CC)
 350  CONTINUE
C
      TOLER = RFIVE * RTEN * RER
      IF (ABS(CC)+TOLER.GE.ABS(ARGS(2))) RETURN
      CC = CC + ARGS(3)
      IARGS(4) = IARGS(4) + IONE
      CALL ADRESS (IFOUR,K1)
      IF (K1.GE.IZERO) GO TO 340
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
C     DUPLICATE.
C
 360  IF (NARGS.EQ.7) GO TO 370
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 370  IF (IARGS(1).GT.IZERO) GO TO 380
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
 380  K1 = MAX0 (IARGS(1)*IARGS(4)+IARGS(6)-IONE,NRMAX)
      IF (K1.LE.NROW) GO TO 390
      CALL ERROR (16)
      RETURN
C
C     ..................................................................
C
 390  J = 7
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
      NARGS = 6
      NDUP  = IARGS(1)
      IARGS(61) = IARGS(6)
      IARGS(62) = IARGS(7)
      IARGS(63) = IARGS(4)
      IARGS(64) = IARGS(5)
      IARGS(65) = IARGS(6)
      IARGS(66) = IARGS(7)
      DO 400 I=1,6
        IARGS(I) = IARGS(I+1)
 400  CONTINUE
C
      CALL MOVE
      IF (NERROR.NE.IZERO) RETURN
      IF (NDUP.EQ.IONE) GO TO 430
      DO 420 I=2,NDUP
        DO 410 J=1,6
          IARGS(J) = IARGS(J+60)
 410    CONTINUE
        IARGS(5) = IARGS(65) + (I-IONE) * IARGS(63)
        CALL MOVE
        IF (NERROR.NE.IZERO) RETURN
 420  CONTINUE
 430  NROLD = NRMAX
      NRMAX = K1
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*MKRON
      SUBROUTINE MKRON
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MKRON V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     KRONECKER PRODUCT OF TWO MATRICES A(N,C)*B(M,K)=D
C
C     FIRST FOUR ARGUMENTS DEFINE MATRIX A, STARTING POSITION AND SIZE
C     NEXT  FOUR ARGUMENTS DEFINE MATRIX B, STARTING POSITION AND SIZE
C     LAST TWO ARGUMENTS INDICATE WHERE RESULT IS TO BE STORED.
C
C     INSTRUCTION IS ...
C     MKRONECKER (R),(C) SIZE (R),(C) BY (R),(C) SIZE (R)X(C) IN (R),(C)
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1967.
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
      REAL             T
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.ITEN) CALL ERROR (10)
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IARGS(11) = IARGS(3) * IARGS(7)
      IARGS(12) = IARGS(4) * IARGS(8)
      J = ITHRE
      CALL MTXCHK (J)
      IF (J.EQ.IZERO) GO TO 10
      CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
  10  NRA = IARGS(3)
      NCA = IARGS(4)
      NRB = IARGS(7)
      NCB = IARGS(8)
      NDS = IONE
      KA = IARGS(1)
C
      DO 50 ICA=1,NCA
        LA = IARGS(5)
        DO 40 ICB=1,NCB
          K = KA
          DO 30 IRA=1,NRA
            T = RC(K)
            K = K + IONE
            L = LA
            DO 20 IRB=1,NRB
              A(NDS) = T * RC(L)
              L = L + IONE
              NDS = NDS + IONE
  20        CONTINUE
  30      CONTINUE
          LA = LA + NROW
  40    CONTINUE
        KA = KA + NROW
  50  CONTINUE
C
      NCR = IARGS(11)
      NCC = IARGS(12)
      NDS = IONE
      KA = IARGS(9)
C
      DO 70 I=1,NCC
        K = KA
        DO 60 J=1,NCR
          RC(K) = A(NDS)
          NDS = NDS + IONE
          K = K + IONE
  60    CONTINUE
        KA = KA + NROW
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MMULT
      SUBROUTINE MMULT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MMULT V 7.00  5/16/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     MULTIPLY MATRICES.
C
C        GENERAL FORM OF MMULT IS ...
C           MMULT A(,) N,K, BY B(,) K,M AND STORE IN C(,)
C
C               WRITTEN BY -
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1967.
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
      DOUBLE PRECISION XP(1)
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
      IF (J.EQ.IZERO) GO TO 10
      CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT.
C
  10  IF (IARGS(4).NE.IARGS(7)) CALL ERROR (26)
C
C     CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE.
C        FIND COLUMN ADDRESSES.
C
      IARGS(12) = IARGS(NARGS-2)
      IARGS(11) = IARGS(3)
      J = ITHRE
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 20
      IF (J.LT.IONE) GO TO 30
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS.
C
  30  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IROWA = IARGS(3)
      ICOLA = IARGS(4)
      ICOLB = IARGS(8)
C
C     BEGIN MULTIPLICATION.
C
      ISP = IONE
      IBP = IARGS(5)
      DO 60 ICB=1,ICOLB
        IAP = IARGS(1)
        DO 50 IRA=1,IROWA
          IA = IAP
          IB = IBP
C
C     CALL ROUTINE TO SUM.
C

          CALL DSUMAL (XP,IXERO,DSUM)
          DO 40 J=1,ICOLA
            XP(1) = DBLE (RC(IA)) * DBLE (RC(IB))
            CALL DSUMAL (XP,-IONE,DSUM)
            IA     = IA + NROW
            IB     = IB + IONE
  40      CONTINUE
          CALL DSUMAL (XP,IONE,DSUM)
          A(ISP) = FDPCON (DSUM)
          ISP = ISP + IONE
          IAP = IAP + IONE
  50    CONTINUE
        IBP = IBP + NROW
  60  CONTINUE
C
C     STORE MATRIX PRODUCT.
C
      IS = IONE
      IC = IARGS(9)
      DO 80 J=1,ICOLB
        DO 70 I=1,IROWA
          RC(IC) = A(IS)
          IS = IS + IONE
          IC = IC + IONE
  70    CONTINUE
        IC = IC + NROW - IROWA
  80  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MOP
      SUBROUTINE MOP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.    MOP V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO DO MDEFINE,ADEFINE,MZERO,AZERO,MERASE,AERASE,MIDENT
C
C     INSTRUCTIONS ARE AS FOLLOWS ...
C
C        MDEFINE  MATRIX IN (R),(C) SIZE N X M TO EQUAL (K)
C        ADEFINE  ARRAY  IN (R),(C) SIZE N X M TO EQUAL (K)
C        MZERO    MATRIX IN (R),(C) SIZE N X M
C        AZERO    ARRAY  IN (R),(C) SIZE N X M
C        MERASE   MATRIX IN (R),(C) SIZE N X M
C        AERASE   ARRAY  IN (R),(C) SIZE N X M
C        MIDENT   MATRIX IN (R),(C) SIZE N X N
C        MDIAGO   MATRIX IN (R),(C) SIZE N X M  EQUAL TO (E) ON DIAGONAL
C
C     L2=1  MDEFINE,ADEFINE
C     L2=2  MZERO,AZERO,MERASE,AERASE
C     L2=3  MIDENT
C     L2=4  MDIAGONAL
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1968.
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
      REAL             CONST, CONSTA
C
C     ==================================================================
C
      CONST  = RZERO
      CONSTA = RZERO
      GO TO (10,60,70,80), L2
C
  10  IF (NARGS.NE.IFIVE) CALL ERROR (10)
      IF (KIND(NARGS).NE.IONE) CALL ERROR (3)
      IF (NARGS.EQ.IFOUR) IARGS(4) = IARGS(3)
      CONST  = ARGS(NARGS)
      CONSTA = ARGS(NARGS)
      J = NARGS - IONE
  20  CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (20)
      J = IONE
      CALL MTXCHK (J)
      IF (J.NE.IZERO) CALL ERROR (17)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      JB = IARGS(1)
      N  = IARGS(3)
      K  = IARGS(4)
      JA = JB
      IF (L2.EQ.IFOUR) GO TO 90
      DO 50 KA=1,K
        JC = JB
        DO 30 NA=1,N
          RC(JC) = CONST
          JC = JC + IONE
  30    CONTINUE
        IF (KA.GT.N) GO TO 40
        RC(JA) = CONSTA
        JA = JA + NROW + IONE
  40    JB = JB + NROW
  50  CONTINUE
      RETURN
C
C     ..................................................................
C
  60  IF (NARGS.NE.IFOUR) CALL ERROR (10)
      CONST  = RZERO
      CONSTA = RZERO
      J = NARGS
      IF (NARGS.EQ.IFOUR) GO TO 20
      IARGS(4) = IARGS(3)
      J = NARGS - IONE
      GO TO 20
C
  70  IF (NARGS.NE.IFOUR) CALL ERROR (10)
      CONST  = RZERO
      CONSTA = RONE
      J = NARGS
      GO TO 20
C
  80  J = NARGS - IONE
      IF (NARGS.NE.IFIVE) CALL ERROR (10)
      GO TO 20
C
  90  IF (KIND(NARGS).EQ.IZERO) GO TO 110
      DO 100 NA=1,N
        RC(JB) = ARGS(NARGS)
        JB = JB + IONE + NROW
 100  CONTINUE
      RETURN
C
C     ..................................................................
C
 110  KIND(5) = IZERO
      CALL ADRESS (IFIVE,M)
      IF (M.GE.IZERO) GO TO 120
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 120  IF (NERROR.NE.IZERO) RETURN
      DO 130 NA=1,N
        A(NA) = RC(M)
        M = M + IONE
 130  CONTINUE
C
      DO 140 NA=1,N
        RC(JB) = A(NA)
        JB = JB + IONE + NROW
 140  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MORTHO
      SUBROUTINE MORTHO
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MORTHO V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ORTHONORMALIZATION PROGRAM UNIT.
C
C     MORTHO MAT (R),(C) SIZE (R)X(C) WTS (E) ORTHO VECS IN (R),(C)
C
C        OR
C
C     MORTHO MAT (R),(C) SIZE (R)X(C) WTS (E) IN (R),(C) TRANS (R),(C)
C
C     L2 = 5.
C
C               WRITTEN BY -
C                      PHILIP J.WALSH
C
C                  REVISED BY -
C                      DAVID HOGBEN AND SALLY T PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2844
C                  ORIGINAL VERSION -     JULY, 1967.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IIRGS(100)
      DIMENSION IMTRXA(2,3)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DENOM, DK, FI, FM, FN, SS, SSQ, SU
      REAL             TOLER, WSUM
      REAL             FDIV,FSQRT
C
      DOUBLE PRECISION DK2, SUM
      DOUBLE PRECISION FDSQRT
C
      EQUIVALENCE (IIRGS(1),LFMT(1))
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NARGS.NE.7 .AND. NARGS.NE.9) CALL ERROR (10)
      IF (IARGS(3).GE.IARGS(4)) GO TO 10
      CALL ERROR (26)
      RETURN
C
C     ..................................................................
C
  10  CALL ADRESS (ITWO,IXM)
      IF (IXM.LT.IZERO) CALL ERROR (11)
      IF (IARGS(1)+IARGS(3)-IONE.GT.NROW) CALL ERROR (17)
      IF (IARGS(2)+IARGS(4)-IONE.GT.NCOL) CALL ERROR (17)
      IXM = IXM - IONE + IARGS(1)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      TOLER = RTEN * RTEN * RER
      NMUI  = IONE
      J     = 7
      JJ    = IONE
  20  CALL ADRESS (J,IMTRXA(JJ,1))
      IF (IMTRXA(JJ,1).GT.IZERO) GO TO 40
  30  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  40  IMTRXA(JJ,2) = IARGS(3)
      IF (JJ.EQ.ITWO) IMTRXA(JJ,2) = IARGS(4)
      IMTRXA(JJ,3) = IARGS(4)
      IF (IARGS(J-1).GT.NROW) GO TO 30
      IMTRXA(JJ,1) = IMTRXA(JJ,1) - IONE + IARGS(J-1)
      IF (IARGS(J)+IARGS(4)-IONE.GT.NCOL)
     1          IMTRXA(JJ,3) = NCOL - IARGS(J) + IONE
      IF (IARGS(J-1)+IARGS(3)-IONE.GT.NROW)
     1          IMTRXA(JJ,2) = NROW - IARGS(J-1) + IONE
      IF (JJ.EQ.ITWO .OR. NARGS.EQ.7) GO TO 50
      J  = 9
      JJ = ITWO
      GO TO 20
C
  50  IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      SS  = RZERO
      SU  = RZERO
      SSQ = RZERO
      L22 = IZERO
      IF (IMTRXA(1,2).NE.IARGS(3) .OR. IMTRXA(1,3).NE.IARGS(4))
     1         CALL ERROR (213)
      IF (NARGS.EQ.7) GO TO 60
      IF (IMTRXA(2,2).NE.IARGS(4) .OR. IMTRXA(2,3).NE.IARGS(4))
     1         CALL ERROR (213)
  60  IF (KIND(5).EQ.IONE) GO TO 70
      CALL ADRESS (IFIVE,IIRGS(2))
      IIRGS(1) = IIRGS(2)
      IF (IIRGS(2).GT.IZERO) GO TO 80
      CALL ERROR (11)
      RETURN
C
C     ..................................................................
C
  70  SU = IARGS(3)
      IF (ARGS(5).LE.RZERO) CALL ERROR (25)
      NMUI    = ITWO
      KIND(2) = IONE
      ARGS(2) = ARGS(5)
  80  M = IARGS(4)
      DO 90 I=1,M
        IIRGS(I+3) = IXM + (I-IONE) * NROW
  90  CONTINUE
C
      N    = IARGS(3)
      FN   = N
      WSUM = RZERO
      IF (NMUI.EQ.ITWO) GO TO 120
      SU   = RZERO
      L22  = IIRGS(2)
      L22A = L22
      DO 110 I=1,N
        IF (RC(L22A).GT.RZERO) GO TO 100
        IF (RC(L22A).EQ.RZERO) GO TO 110
        CALL ERROR (25)
        RETURN
 100    SU   = SU + RONE
        WSUM = WSUM + RC(L22A)
        L22A = L22A + IONE
 110  CONTINUE
C
 120  FM = M
      IF (SU.EQ.FM) GO TO 130
      IF (SU.GT.FM) GO TO 140
      CALL ERROR (24)
      RETURN
C
C     ..................................................................
C
 130  DENOM = RZERO
      GO TO 150
C
 140  DENOM = FSQRT (SU-FM)
 150  NPM = N + M
      M2  = M + IONE
      N2  = N + IONE
      MD1 = IDIV (M*(M2),ITWO,IND)
      ND1 = M2 * NPM
C
C     X REQUIRES ND1 CELLS.
C        GET A    (ND1 + 1) FOR START OF PK.
C
      ND2 = M * NPM
      MD3 = ND2 + N
      ND3 = ND1
C
C     ADD NPM TO REACH XP.
C
      ND4 = ND3 + NPM
C
C     ADD NPM TO REACH QK.
C
      ND5 = ND4 + NPM
C
C     ADD (M+1) TO REACH CV.
C
      ND6 = ND5 + M2
C
C     ADD (M*(M+1))/2 + M  TO REACH VCV.
C
      ND66 = MD1 + M
      ND7  = ND6 + ND66
C
C     ADD THE SAME AMOUNT TO REACH Q.
C
      ND8 = ND7 + ND66
C
C     Q IS (M+1) CELLS LONG  THEN COMES Q2.
C
      ND9 = ND8 + M2
C
C     Q2 E AND EP ARE EACH M CELLS LONG.
C
      ND10 = ND9 + M
      ND11 = ND10 + M
      ND12 = ND11 + M
C
C     THE A MATRIX IS NEXT.
C
      ND13 = ND12 + MD1
C
C     GRAM FACTOR STORAGE.
C
      ND14 = ND13 + M2
C
C     ENF.
C        CV DIAGONALS.
C
      ND16 = ND14 + M
C
C     VCV DIAGONALS.
C
      ND17 = ND16 + M
      ND18 = ND17 + M
      ND19 = ND18 + NPM
      ND20 = ND19 + N
      IF (ND20.GT.NS) CALL ERROR (23)
      IF (NERROR.NE.IZERO) RETURN
      NRBAR = IONE
      I     = IFOUR
C
C     FIND OUT IF ALL X(I) = 1.0, IF SO SET NX=1 AND MX=M-1.
C
      L33 = IIRGS(4)
      DO 170 NW6=1,N
        IF (ABS(RC(L33)-RZERO).LE.TOLER) GO TO 160
C       NX  = IZERO
        GO TO 180
 160    L33 = L33 + IONE
 170  CONTINUE
C
 180  CONTINUE
      MXARGS = M + IFOUR
      L44    = MXARGS - IONE
      J      = IONE
      DO 200 I1=I,L44
        K1  = J
        L33 = IIRGS(I1)
        K2  = K1
        DO 190 I2=1,N
          A(K2) = RC(L33)
          K2    = K2 + IONE
          L33   = L33 + IONE
 190    CONTINUE
        J = J + NPM
 200  CONTINUE
C
C     GENERATE IDENTITY MATRIX AUGMENTATION.
C
      K1 = N2
      DO 220 K=1,M
        K2 = K1
        DO 210 I=1,M
          A(K2) = RZERO
          K2    = K2 + IONE
 210    CONTINUE
        K2 = K1 + K - IONE
        K1 = K1 + NPM
        A(K2) = RONE
 220  CONTINUE
C
C     BEGIN THE G.S. PROCESS.
C
      NBEI = IONE
      NRHI = IONE
      I18  = IONE + ND13
      NGAI = ITWO
      NSII = ITWO
      NDEI = IONE
      NNUI = IONE
      LZ1  = IONE
      LZ2  = IONE
C
C     K CONTROLS WHOLE  LOOP.
C
      K    = IONE
 230  NTHI = IONE
 240  NALI = IONE
      NOMI = IONE
      NJ = ND3 + N + IONE
      DO 250 J=1,M
        A(NJ) = RZERO
        NJ    = NJ + IONE
 250  CONTINUE
C
C     BOX 6.
C
 260  KD1  = (K-IONE) * NPM
      I1   = ND3 + IONE
      I2   = KD1 + IONE
      L22A = L22
      DO 290 I=1,N
        IF (NMUI.EQ.ITWO) GO TO 270
C
C       PK(I)
C
        A(I1) = A(I2) * RC(L22A)
        L22A  = L22A + IONE
        GO TO 280
 270    A(I1) = A(I2) * ARGS(2)
 280    I1 = I1 + IONE
        I2 = I2 + IONE
 290  CONTINUE
C
      IF (NOMI.EQ.ITWO) GO TO 320
      IA1 = IONE
      IA2 = ND5 + IONE
      DO 310 I=1,K
        I2  = IA1
        SUM = RZERO
        J2  = ND3 + IONE
        DO 300 J=1,NPM
          SUM = SUM + A(J2) * A(I2)
          I2  = I2 + IONE
          J2  = J2 + IONE
 300    CONTINUE
C
C       QK(I).
C
        A(IA2) = SUM
        IA1 = IA1 + NPM
        IA2 = IA2 + IONE
 310  CONTINUE
      GO TO 350
C
 320  DK2  = RZERO
      I1   = (K-IONE) * NPM + IONE
      IND3 = ND3 + IONE
      DO 330 I=1,NPM
        DK2  = DK2 + A(IND3) * A(I1)
        I1   = I1 + IONE
        IND3 = IND3 + IONE
 330  CONTINUE
      DK = FDSQRT(DK2)
C
C     GRAM FACTORS.
C
      A(I18) = DK
      I18    = I18 + IONE
      K1     = (K-IONE) * NPM + IONE
      DO 340 I=1,NPM
        A(K1) = FDIV (A(K1),DK,IND)
        K1    = K1 + IONE
 340  CONTINUE
      NOMI = IONE
      GO TO 260
C
C     BOX 8.
C
 350  IF (NDEI.EQ.ITWO) GO TO 410
      LZ1 = -LZ1
      IF (LZ1.LT.IZERO) GO TO 400
C
C     BOX 8A.
C
 360  K1    = K - IONE
      IRUTH = ND5 + IONE
      DO 370 I=1,K1
        A(IRUTH) = - A(IRUTH)
        IRUTH    = IRUTH + IONE
 370  CONTINUE
C
      IRUTH    = K + ND5
      A(IRUTH) = RONE
      J2       = ND4 + IONE
      DO 390 I=1,NPM
        SUM = RZERO
        J1  = I
        J3  = ND5 + IONE
        DO 380 J=1,K
          SUM = SUM + A(J1) * A(J3)
          J1  = J1 + NPM
          J3  = J3 + IONE
 380    CONTINUE
C
C        XP(I).
C
        A(J2) = SUM
        J2    = J2 + IONE
 390  CONTINUE
      GO TO 470
C
C     BOX 8B    GET QK(I18).
C
 400  ISAL    = I18 + M2
      IRUTH   = ND5 + K
      A(ISAL) = FSQRT (A(IRUTH))
      GO TO 360
C
C     NDE1.
C
 410  LZ2 = -LZ2
      IF (LZ2.GT.IZERO) GO TO 360
C
C     GET E AND OTHER VECTORS.
C
      IND5 = ND5 + IONE
      IND8 = ND8 + IONE
      DO 420 I=1,M
        IND5 = ND5 + I
        IND9 = ND9 + I
        IND8 = ND8 + I
        A(IND8) = A(IND5)
        A(IND9) = A(IND5) * A(IND5)
 420  CONTINUE
C
      A(IND8+1) = A(IND5+1)
      A(ND10+1) = A(IND8+1) - A(ND9+1)
      IND10 = ND10 + IONE
      IND9  = ND9 + IONE
      DO 430 J=2,M
        IND10 = IND10 + IONE
        IND9  = IND9 + IONE
        A(IND10) = A(IND10-1) - A(IND9)
 430  CONTINUE
C
      FI    = RONE
      IND10 = ND10
      IND11 = ND11
      DO 460 I=1,M
        IND10 = IND10 + IONE
        IND11 = IND11 + IONE
        IF (FN.LE.FI) GO TO 450
        IF (A(IND10).GE.RZERO) GO TO 440
        A(IND11) = - FSQRT (ABS (FDIV (A(IND10),FN-FI,IND) ) )
        GO TO 460
 440    A(IND11) =  FSQRT (FDIV (A(IND10),FN-FI,IND) )
        GO TO 460
 450    A(IND10) = - RONE
        FI = FI + RZERO
 460  CONTINUE
      GO TO 360
C
 470  GO TO (520,480,550), NTHI
C
 480  IND18 = ND18 + IONE
      IND4  = ND4 + IONE
      DO 490 I=1,N
        A(IND18) = A(IND4)
        IND18    = IND18 + IONE
        IND4     = IND4 + IONE
 490  CONTINUE
C
      NI = N + IONE
      DO 500 I=1,M
        KK1    = ND18 + NI
        IND4   = ND4 + NI
        A(KK1) = - A(IND4)
        NI     = NI + IONE
 500  CONTINUE
C
      IND4  = ND4
      IND19 = ND19
      DO 510 I=1,N
        IND4  = IND4 + IONE
        IND19 = IND19 + IONE
        A(IND19) = A(IND4)
 510  CONTINUE
C
      NTHI = ITHRE
 520  K1   = (K-1) * NPM + IONE
      IND4 = ND4 + IONE
      DO 530 I=1,NPM
        A(K1) = A(IND4)
        K1    = K1 + IONE
        IND4  = IND4 + IONE
 530  CONTINUE
C
      IF (NALI.EQ.ITWO) GO TO 540
      NOMI = ITWO
      NALI = ITWO
      GO TO 260
C
 540  IF (K.GE.M) GO TO 570
      K = K + IONE
      GO TO 230
C
 550  IF (NNUI.EQ.ITWO) GO TO 560
      NNUI = ITWO
      GO TO 650
C
 560  SS  = FDIV (DK,DENOM,IND)
      SSQ = SS * SS
      GO TO 650
 570  IF (NBEI.EQ.ITWO) GO TO 550
C
C     GET THE A MATRIX.
C
      K1 = IONE
      DO 590 I=1,M
        I1 = I * N + (I-1) * M
        DO 580 J=1,I
          I2 = J + I1
          K2 = K1 + ND12
          A(K2) = A(I2)
          K1 = K1 + IONE
 580    CONTINUE
 590  CONTINUE
C
      NDEI = ITWO
      NBEI = ITWO
      NTHI = ITWO
      K    = K + IONE
      IF (NGAI.EQ.IONE) GO TO 550
C
C     GET CV MATRIC.
C
      DO 620 IL=1,M
        LOC = ND6 + IDIV (IL*(IL-1),ITWO,IND) + IONE
        DO 610 J=1,IL
          SUM = RZERO
          DO 600 KK=IL,M
            LOC1 = ND12 + IDIV (KK*(KK-1),ITWO,IND) + IL
            LOC2 = ND12 + IDIV (KK*(KK-1),ITWO,IND) + J
            SUM  = SUM + A(LOC1) * A(LOC2)
 600      CONTINUE
          A(LOC) = SUM
          LOC = LOC + IONE
 610    CONTINUE
 620  CONTINUE
C
      J1 = ITHRE + ND6
      J  = ND16 + ITWO
      A(ND16+1) = FSQRT (A(ND6+1))
      IF (M.EQ.IONE) GO TO 640
      DO 630 I=2,M
C
C       THE ARGUMENT IN THE FOLLOWING SQRT OCCASIONALLY IS NEGATIVE.
C
        A(J) = FSQRT (A(J1))
        J    = J + IONE
        J1   = J1 + I + IONE
 630  CONTINUE
C
 640  NGAI = IONE
      GO TO 550
C
 650  IF (NRHI.EQ.ITWO) GO TO 700
 660  IF (NRBAR.EQ.IZERO) GO TO 730
      NRBAR = NRBAR - IONE
      NTHI  = ITWO
      NRHI  = ITWO
      L11   = IIRGS(1) - IONE
      L11A  = L11 + IONE
      I1    = ND2 + IONE
      DO 680 I=1,N
        IF (NMUI.EQ.IONE) GO TO 670
        A(I1) = ARGS(2)
        GO TO 680
 670    A(I1) = RC(L11A)
        L11A  = L11A + IONE
        I1    = I1 + IONE
 680  CONTINUE
C
      I1 = MD3 + IONE
      DO 690 I=1,M
        A(I1) = RZERO
        I1 = I1 + IONE
 690  CONTINUE
      GO TO 240
C
 700  IF (NSII.EQ.IONE) GO TO 660
C
C     GET VCV AND DEV AND COEF.
C
      IND7 = ND7 + IONE
      IND6 = ND6 + IONE
      DO 710 I=1,MD1
        A(IND7) = SSQ * A(IND6)
        IND7 = IND7 + IONE
        IND6 = IND6 + IONE
 710  CONTINUE
      IND16 = ND16 + IONE
      IND17 = ND17 + IONE
      DO 720 I=1,M
        A(IND17) = SS * A(IND16)
        IND16 = IND16 + IONE
        IND17 = IND17 + IONE
 720  CONTINUE
      GO TO 660
C
C     THE CALCULATIONS ARE COMPLETED. NOW OUTPUT THE RESULTS.
C
 730  A(ND8+1) = A(ND8+1) + FSQRT (WSUM)
      A(ND9+1) = A(ND8+1)**2
      IST  = IMTRXA(1,1)
      K    = IONE
      MMTXC = IMTRXA(1,3)
      MMTXR = IMTRXA(1,2)
      DO 750 I=1,MMTXC
        KK = K
        ISTRR = IST
        DO 740 J=1,MMTXR
          RC(ISTRR) = A(KK)
          KK    = KK + IONE
          ISTRR = ISTRR + IONE
 740    CONTINUE
        K   = K + NPM
        IST = IST + NROW
 750  CONTINUE
C
      IF (NARGS.EQ.7) RETURN
      IND7  = ND12 + IONE
      IST   = IMTRXA(2,1)
      MMTXR = IMTRXA(2,2)
      MMTXC = IMTRXA(2,3)
C
C     STORE VARIANCE COVARIANCE MATRIX IN WORKSHEET.
C
      ISTR  = IST
      ISTC  = IST
      MSTOP = MIN0 (MMTXC,MMTXR)
      DO 770 I=1,MSTOP
        ISTRR = ISTR
        ISTCC = ISTC
        DO 760 J=1,I
          RC(ISTRR) = RZERO
          RC(ISTCC) = A(IND7)
          IND7  = IND7 + IONE
          ISTRR = ISTRR + NROW
          ISTCC = ISTCC + IONE
 760    CONTINUE
        ISTR = ISTR + IONE
        ISTC = ISTC + NROW
 770  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MOVE
      SUBROUTINE MOVE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   MOVE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE MOVE (R),(C) SIZE RXC TO (R),(C)
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
      IF (NARGS.EQ.6) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IARGS(9) = IARGS(1) + IARGS(3) - IONE
      IARGS(13) = IARGS(5) + IARGS(3) - IONE
      IF (KIND(1)+KIND(3)+KIND(4)+KIND(5).EQ.IZERO) GO TO 20
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  20  IF (IARGS(1).GT.IZERO .AND. IARGS(3).GT.IZERO .AND. IARGS(5).GT.
     1   IZERO .AND. IARGS(9).LE.NROW .AND. IARGS(13).LE.NROW) GO TO 30
      CALL ERROR (16)
      RETURN
C
C     ..................................................................
C
  30  IARGS(10) = IARGS(2) + IARGS(4) - IONE
      KIND(10)  = IZERO
      IARGS(14) = IARGS(6) + IARGS(4) - IONE
      KIND(14)  = IZERO
      DO 50 J=2,14,4
        I = J
        CALL ADRESS (I,IARGS(I))
        IF (IARGS(I).GE.IZERO) GO TO 40
        CALL ERROR (20)
        RETURN
  40    IARGS(I) = IARGS(I) - IONE
  50  CONTINUE
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     IF MOVE IS UP,   IR = -1, IF DOWN,  IR = +1
C     IF MOVE IS LEFT, IC = -1, IF RIGHT, IC = +1
C
C     DIRECTION OF MOVE IS SUCH THAT THE TWO AREAS CAN BE OVERLAPPING
C        AND IT WILL BE DONE PROPERLY.
C
      IR  = ISIGN (1,IARGS(5)-IARGS(1))
      IC  = ISIGN (IONE,IARGS(6)-IARGS(2))
      MM  = IARGS(4*IR+5) + IARGS(4*IC+6)
      NN  = IARGS(4*IR+9) + IARGS(4*IC+10)
      IC  = IC * NROW
      MMM = IARGS(3)
      NNN = IARGS(4)
      DO 70 J=1,NNN
        M = MM
        N = NN
        DO 60 I=1,MMM
          RC(N) = RC(M)
          M = M - IR
          N = N - IR
  60    CONTINUE
        MM = MM - IC
        NN = NN - IC
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MPROP
      SUBROUTINE MPROP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MPROP V 7.00  8/27/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTIONS ARE AS FOLLOWS ...
C         X = A OR M DEPENDING UPON WHETHER COMMAND IS APROPE OR MPROPE.
C
C     I   XPROPE OF MATRIX (R,C) NO OF ROWS (R) COLS (C)
C            INFORMATION PRINTED AND NO STORAGE.
C
C     II  XPROPE MATRIX (R,C) SIZE (R)X(C) PROPERTIES IN COL (C)
C            PROPERTIES PRINTED AND STORED.
C
C     III XPROPE MATRIX (R,C) (R)X(C) PROP (C) COL NORMS (R,C)
C            SAME AS II PLUS STORAGE OF COLUMN NORMS.
C
C     IV  XPROPE (R,C) (R)X(C), PROP (C) COL NORMS (R,C) ROW NORMS (R,C)
C            SAME AS III PLUS STORAGE OF ROW NORMS, ALSO (R,C+1) OF NORM
C               AVERAGES WILL CONTAIN GRAND AVERAGE, IF X=A.
C
C     V   XPROPE (R,C) (R)X(C) COL NORMS (R,C)
C            SAME AS III, EXCEPT PROPERTIES WILL NOT BE STORED.
C
C     VI  XPROPE (R,C) (R)X(C), COL NORMS (R,C) ROW NORMS (R,C)
C            SAME AS IV, EXCEPT PROPERTIES WILL NOT BE STORED.
C
C     VII SXPROP
C            IF COMMANDS II-VI ARE PREFACED WITH AN S PRINTING
C               OF PROPERTIES WILL BE SUPPRESSED.
C
C     L2  OPTIONS ...
C         L2 = 1,  MPROPERTIES
C         L2 = 3,  SMPROPERTIES
C         L2 = 2,  APROPERTIES
C         L2 = 4,  SAPROPERTIES
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MAY, 1968.
C                   CURRENT VERSION - AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IPROP(5)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ABSMN, ABSMNZ, ABSMX, AMN, AMX, ANRMX, AVG
      REAL             FIN, FJK, RCAB, SCSQ, SRSQ, SSQ
      REAL             TEMP, TRACE, TR2
      REAL             FDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX / 25 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (L2.LE.ITWO .OR. NARGS.NE.IFOUR) GO TO 10
      CALL ERROR (236)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.LT.IFOUR .OR. NARGS.GT.9) CALL ERROR (10)
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
      K = IFIVE
      J = IONE
      IF (NARGS.EQ.IFIVE) GO TO 30
      IF (NARGS.LT.IFIVE) GO TO 60
      IF (NARGS.EQ.6 .OR. NARGS.EQ.8) GO TO 40
      IS = IARGS(5)
      J = ITWO
      K = 9
      IF (NARGS.EQ.7) GO TO 20
      IARGS(11) = IARGS(3)
      IARGS(12) = IONE
      IARGS(10) = IARGS(9)
      IARGS( 9) = IARGS(8)
      K = 13
      J = ITHRE
      IF (L2.GT.ITWO) IARGS(11) = IARGS(11) + IONE
  20  IARGS(5) = IARGS(6)
      IARGS(6) = IARGS(7)
      IARGS(7) = IONE
      IARGS(8) = IARGS(4)
      IARGS(K) = IS
      KIND(K)  = IZERO
  30  CALL ADRESS (K,KPROP)
      IF (KPROP.LT.IZERO) CALL ERROR (20)
      GO TO 60
C
  40  J = ITWO
      IF (NARGS.EQ.6) GO TO 50
      IARGS( 9) = IARGS(7)
      IARGS(10) = IARGS(8)
      IARGS(11) = IARGS(3)
      IARGS(12) = IONE
      J = ITHRE
      IF (L2.GT.ITWO) IARGS(11) = IARGS(11) + IONE
  50  IARGS( 7) = IONE
      IARGS( 8) = IARGS(4)
  60  CALL MTXCHL (J)
      KARGS = K
      IF (J.NE.IZERO) CALL ERROR (17)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      K = IARGS(1)
      IPV = 18
      IF (L2.EQ.ITWO .OR. L2.EQ.IFOUR) GO TO 100
C
C     INSTRUCTION IS MPROPERTIES.
C
C     IS MATRIX SQUARE.
C
      IF (IARGS(3).NE.IARGS(4)) GO TO 80
C
C     YES.
C
      IF (ITWO*((IARGS(3))**2)+IFOUR*IARGS(3)+IHRD.LE.NS) GO TO 70
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
C     COMPUTE SPECIFIC INFORMATION FOR SQUARE MATRIX.
C
  70  CALL MPRSPC (K,IPROP,IND)
      IF (IND.NE.IZERO) GO TO 450
      IPV = 19
      GO TO 100
C
  80  CALL ORTHRV (RC(K),NROW,IARGS(3),IARGS(4),IPROP(4),A(1))
      DO 90 I=19,31
        A(I) = RZERO
  90  CONTINUE
C
 100  CALL RCSUM (RC(1),K,NROW,IARGS(3),IARGS(4),A(101),IB,RPIFY)
      IF (IB.NE.IZERO) CALL ERROR (110)
      L = IARGS(3)
      IF (L.GT.IARGS(4)) L = IARGS(4)
      ITRACE = L
      IPOS   = IZERO
      KZERO  = IZERO
      INEG   = IZERO
      KSTP   = (IARGS(3)+IARGS(4)) * ITWO + 106
      KSPT   = KSTP
      AMX    = RC(K)
      AMN    = AMX
      LB     = IARGS(3) + IARGS(4)
      AVG    = FDIV (A(LB+102),FLOAT(IARGS(3)*IARGS(4)),IND)
      ABSMX  = ABS (AMX)
      ABSMN  = ABS (AMN)
      ABSMNZ = ABSMN
      SSQ    = RZERO
      SRSQ   = RZERO
      SCSQ   = RZERO
      IF (ABSMNZ.EQ.RZERO) ABSMNZ = RPIFY
      KA     = K
      CALL SUMMAL (A,IZERO,TRACE)
      DO 110 I=1,L
        A(KSTP) = RC(KA)
        CALL SUMMAL (A(KSTP),-IONE,TRACE)
        KSTP = KSTP + IONE
        KA = KA + NROW + IONE
 110  CONTINUE
C
      CALL SUMMAL (A,IONE,TRACE)
      IN   = IARGS(3)
      JK   = IARGS(4)
      KA   = K
      FIN  = IN
      FJK  = JK
      KSTP = KSPT
      LSTP = IONE
      IF (KSTP+IN*JK.GT.NS) LSTP = ITWO
      DO 220 J=1,JK
        KB = KA
        DO 210 I=1,IN
          KC = IARGS(4) + I
          A(KSTP) = (RC(KB) - FDIV (A(KC+100),FJK,IND))**2
          KSTP = KSTP + IONE
          IF (LSTP.NE.ITWO) GO TO 120
          IF (KSTP.LE.NS) GO TO 120
          KSTP = NS - KSPT
          CALL SUMMAL (A(KSPT),KSTP,TEMP)
          IF (KSTP.EQ.IONE) TEMP = A(KSPT)
          A(KSPT) = TEMP
          KSTP = KSPT + IONE
 120      IF (RC(KB)) 130,140,150
 130      INEG = INEG + IONE
          GO TO 160
C
 140      KZERO = KZERO + IONE
          GO TO 160
C
 150      IPOS = IPOS + IONE
 160      IF (AMX.GT.RC(KB)) GO TO 170
          AMX = RC(KB)
          GO TO 180
C
 170      IF (AMN.GT.RC(KB)) AMN = RC(KB)
 180      RCAB = ABS(RC(KB))
          IF (ABSMX.LT.RCAB) ABSMX = RCAB
          IF (ABSMN.GT.RCAB) ABSMN = RCAB
          IF (ABSMNZ.GT.RCAB .AND. RCAB.GT.RZERO) GO TO 190
          IF (ABSMNZ.GT.RCAB .AND. RCAB.LE.RZERO) GO TO 200
          GO TO 200
C
 190      ABSMNZ = RCAB
 200      KB = KB + IONE
 210    CONTINUE
        KA = KA + NROW
 220  CONTINUE
C
      IF (ABSMNZ.EQ.RPIFY) ABSMNZ = RZERO
      MSTP = IZERO
      KSTP = KSTP - KSPT
      CALL SUMMAL (A(KSPT),KSTP,SRSQ)
      IF (KSTP.EQ.IONE) SRSQ = A(KSPT)
 230  KSTP = KSPT
      KA = K
      DO 280 J=1,JK
        KB = KA
        DO 270 I=1,IN
          IF (MSTP.LE.IZERO) GO TO 240
          A(KSTP) = (RC(KB) - FDIV (A(J+100),FIN,IND))**2
          GO TO 250
 240      A(KSTP) = (RC(KB)-AVG)**2
 250      KSTP = KSTP + IONE
          IF (LSTP.EQ.IONE) GO TO 260
          IF (KSTP.LE.NS) GO TO 260
          KSTP = NS - KSPT
          CALL SUMMAL (A(KSPT),KSTP,TEMP)
          IF (KSTP.EQ.IONE) TEMP = A(KSPT)
          A(KSPT) = TEMP
          KSTP = KSPT + IONE
 260      KB = KB + IONE
 270    CONTINUE
        KA = KA + NROW
 280  CONTINUE
C
      KSTP = KSTP - KSPT
      IF (MSTP.GT.IZERO) GO TO 290
      CALL SUMMAL (A(KSPT),KSTP,SSQ)
      IF (KSTP.EQ.IONE) SSQ = A(KSPT)
      MSTP = IONE
      GO TO 230
C
 290  CALL SUMMAL (A(KSPT),KSTP,SCSQ)
      IF (KSTP.EQ.IONE) SCSQ = A(KSPT)
      IF (L2.EQ.ITWO .OR. L2.EQ.IFOUR .OR. IARGS(3).NE.IARGS(4))
     1       GO TO 340
      ISTOCR = IZERO
      ISTCHC = IZERO
      IF (AMN.LT.RZERO) GO TO 330
      DO 300 J=1,JK
        IF (A(J+100).EQ.RONE) GO TO 300
        GO TO 310
 300  CONTINUE
C
      ISTCHC = ITWO
 310  DO 320 I=1,IN
        M = I + JK
        IF (A(M+100).EQ.RONE) GO TO 320
        GO TO 330
 320  CONTINUE
C
      ISTOCR = IONE
 330  A(31)  = ISTCHC + ISTOCR
 340  A(1)   = TRACE
      TR2    = RZERO
      KB     = K
      JKK    = JK
      IF (JK.GT.IN) JKK = IN
      KSTP   = KSPT
      DO 370 J=2,JKK
        KA = K
        KB = KB + NROW + IONE
        KC = K + J - IONE
        KD = K + (J-IONE) * NROW
        II = J - IONE
        DO 360 I=1,II
          A(KSTP) = RC(KA) * RC(KB) - RC(KC) * RC(KD)
          KSTP = KSTP + IONE
          IF (LSTP.EQ.IONE) GO TO 350
          IF (KSTP.LE.NS) GO TO 350
          KSTP = NS - KSPT
          CALL SUMMAL (A(KSPT),KSTP,TEMP)
          IF (KSTP.EQ.IONE) TEMP = A(KSPT)
          A(KSPT) = TEMP
          KSTP = KSPT + IONE
 350      KA = KA + NROW + IONE
          KC = KC + NROW
          KD = KD + IONE
 360    CONTINUE
 370  CONTINUE
C
      KSTP  = KSTP - KSPT
      CALL SUMMAL (A(KSPT),KSTP,TR2)
      IF (KSTP.EQ.IONE) TR2 = A(KSPT)
      A( 2) = TR2
      A( 3) = AMX
      A( 4) = AMN
      A( 5) = ABSMX
      A( 6) = ABSMN
      A( 7) = ABSMNZ
      A( 8) = IPOS
      A( 9) = KZERO
      A(10) = INEG
      A(11) = A(LB+101)
      A(12) = AVG
      A(13) = A(LB+103)
      A(14) = SSQ
      A(15) = SRSQ
      A(16) = SCSQ
      A(17) = A(LB+104)
      A(18) = FDIV (A(17),FLOAT(IARGS(3)*IARGS(4)),IND)
      IF (NERROR.NE.IZERO) RETURN
      IF (L2.GE.ITHRE) GO TO 380
      CALL MPRPNT (IPROP,IPV,KARGS,ITRACE,ISTOCR,ISTCHC)
 380  IF (NARGS.EQ.IFOUR) RETURN
      IF (MOD(NARGS,ITWO).EQ.IZERO) GO TO 420
      IP = 31
      IF (IARGS(3).EQ.IARGS(4)) GO TO 400
      IP = 27
      DO 390 I=19,NX
        A(I) = RZERO
 390  CONTINUE
C
 400  IF (MOD(L2,ITWO).EQ.IZERO) IP = 18
      IF (NROW.LT.IP) IP = NROW
      DO 410 I=1,IP
        RC(KPROP) = A(I)
        KPROP = KPROP + IONE
 410  CONTINUE
C
      IF (NARGS.EQ.IFIVE) RETURN
 420  KA = IARGS(5)
      ANRMX = IARGS(3)
      DO 430 I=1,JK
        RC(KA) = FDIV (A(I+100),ANRMX,IND)
        KA = KA + NROW
 430  CONTINUE
C
      IF (NARGS.LT.8) RETURN
      KA = IARGS(9)
      ANRMX = IARGS(4)
      KB = JK + 101
      DO 440 I=1,IN
        RC(KA) = FDIV (A(KB),ANRMX,IND)
        KA = KA + IONE
        KB = KB + IONE
 440  CONTINUE
      IF (L2.GT.ITWO) RC(KA) = AVG
      RETURN
C
C     ..................................................................
C
 450  CALL ERROR (22)
      RETURN
C
C     ==================================================================
C
      END
*MPRPNT
      SUBROUTINE MPRPNT (IPROP,IPV,KARGS,ITRACE,ISTOCR,ISTCHC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. MPRPNT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE THE AUTOMATIC PRINTING
C        FOR MPROPERTIES AND APROPERTIES INSTRUCTIONS.
C
C     ALL ARGUMENTS ARE INPUT VALUES.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - APRIL, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IFT(6,2),    IPROP(*)
      DIMENSION IRSLT(6), IRSLTA(2,5), IRSLTP(11)
      DIMENSION JNEG(25),   JZERO(25)
      DIMENSION  MFT(13),    MPFT(53), MSAVE(25), MSTORE(25), N(2000)
      DIMENSION NXN(100)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
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
      REAL             ANEG, POS, ZER
C
C      ..................................................................
C
      CHARACTER        LHFMT*80
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER*1      N, MSTORE, JZERO, JNEG, MFT, MPFT, IFT
      CHARACTER*3      NO, IYES, IBLK, LOWRA, LOWRB, IPPRA, IPPRB,
     1                 IANDA, IANDB, IRWA, IRWB, ICLMA, ICLMB,
     2                 IBTHH, IBTHHA, IBTHA, IBTHB, NOA, NOAB
       CHARACTER*3     IRSLT, IRSLTA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA     NO,   IYES,   IBLK,  LOWRA,  LOWRB,  IPPRA,  IPPRB /
     1      ' NO',  'YES',  '   ',  ' LO',  'WER',  ' UP',  'PER' /
      DATA  IANDA,  IANDB,   IRWA,   IRWB,  ICLMA,  ICLMB /
     1      ' AN',  'D  ',  '   ',  'ROW',  'COL',  'UMN' /
      DATA  IBTHH, IBTHHA,  IBTHA,  IBTHB,    NOA,   NOAB /
     1      '   ',  '  T',  'WO-',  'WAY',  '   ',  ' NO' /
C
      DATA MFT(1), MFT(2), MFT( 3), MFT( 4), MFT( 5), MFT( 6), MFT( 7) /
     1        'S',    'T',     'A',     'R',     'T',     'I',     'N' /
      DATA MFT(8), MFT(9), MFT(10), MFT(11), MFT(12), MFT(13) /
     1        'G',    ' ',     'A',     'T',     ' ',     '(' /
C
      DATA IFT(1,1), IFT(2,1), IFT(3,1), IFT(4,1), IFT(5,1), IFT(6,1) /
     1          'M',      'A',      'T',      'R',      'I',      'X' /
      DATA IFT(1,2), IFT(2,2), IFT(3,2), IFT(4,2), IFT(5,2), IFT(6,2) /
     1          'A',      'R',      'R',      'A',      'Y',      ' ' /
C
      DATA NX  / 25 /
      DATA ICA / 100000 /
C
C     ==================================================================
C
      JK    = IARGS(4)
      IN    = IARGS(3)
      LOC   = IARGS(1)
      IF (IARGS(2).GT.IONE) LOC = MOD (LOC,(IARGS(2)-IONE)*NROW)
      IPOS  = A(8)
      KZERO = A(9)
      INEG  = A(10)
      CALL PAGE (IFOUR)
      INN   = JK + 102 + IN
      INNI  = INN
      DO 10 I=1,7
        A(INNI) = A(I)
        A(INNI+7) = A(I+10)
        INNI = INNI + IONE
  10  CONTINUE
C
      INNI = INN + 14
      A(INNI) = A(18)
      IF (IPV.EQ.18) GO TO 20
      A(INNI+1) = A(19)
      INNI = INNI + IONE
  20  CALL RFORMT (0,ISIGD,A(INN),RC(1),IPV-3,22,NWIDTH,NDEC,N(INN),IRF)
      NBL   = NX - NWIDTH
      ISP   = INN
      INSPV = ISP
      INNI  = INNI + IONE
      ISPT  = INNI
      DO 30 I=1,15
        CALL RFORMT (1,ISIGD,RC,A(ISP),NBL,0,NWIDTH,NDEC,N(INNI),IRF)
        ISP  = ISP + IONE
        INNI = INNI + NX
  30  CONTINUE
C
      INNI = ISPT + 174
      DO 40 I=1,53
        MPFT(I) = LA(45)
  40  CONTINUE
C
      ISIZE = IARGS(3)
      IA = IONE
      DO 160 I=1,4
        IB = IZERO
  50    IB = IB + IONE
        MSAVE(IB) = MOD (ISIZE,ITEN)
        ISIZE = IDIV (ISIZE,ITEN,IND)
        IF (ISIZE.NE.IZERO) GO TO 50
        IC = IB
        DO 60 ID=1,IB
          IF = MSAVE(IC) + IONE
          MPFT(IA) = LA(IF)
          IA = IA + IONE
          IC = IC - IONE
  60    CONTINUE
C
        GO TO (70,80,130,140), I
C
  70    MPFT(IA) = LA(34)
        ISIZE = IARGS(4)
        GO TO 150
C
  80    MPFT(IA) = LA(45)
        DO 90 ID=1,6
          IA = IA + IONE
          MPFT(IA) = IFT(ID,L2)
  90    CONTINUE
        IF (L2.EQ.ITWO) GO TO 100
        IA = IA + IONE
        MPFT(IA) = LA(45)
 100    IF (ILABEL.EQ.IZERO) GO TO 110
        IL = ((IARGS(2)-IONE)*NROW+LOC) * ICA + IARGS(3)
        LC = IARGS(4)
        CALL PREPAK (ITHRE,LC,IL,NXN,MPFT(IA+2),LHFMT,INDD)
        IF (INDD.NE.IZERO) GO TO 110
        MPFT(IA+1)  = LA(42)
        MPFT(IA+14) = LA(43)
        IA = IA + 15
        MPFT(IA) = LA(45)
 110    DO 120 ID=1,13
          IA = IA + IONE
          MPFT(IA) = MFT(ID)
 120    CONTINUE
        ISIZE = LOC
        GO TO 150
C
 130    MPFT(IA) = LA(44)
        ISIZE = IARGS(2)
        GO TO 150
C
 140    MPFT(IA) = LA(43)
 150    IA = IA + IONE
 160  CONTINUE
C
      IA     = IA + IONE
      POS    = IPOS
      NC     = NDEC
      NW     = NWIDTH - IONE - NC
      CALL RFORMT (9,ISIGD,A,POS,NBL,0,NW,IZERO,MSTORE(1),IRF)
      ZER    = KZERO
      CALL RFORMT (9,ISIGD,A,ZER,NBL,0,NW,IZERO,JZERO(1),IRF)
      ANEG   = INEG
      CALL RFORMT (9,ISIGD,A,ANEG,NBL,0,NW,IZERO,JNEG(1),IRF)
      NWIDP1 = NX - NC
      DO 170 I=NWIDP1,NX
        MSTORE(I) = LA(45)
        JZERO(I)  = LA(45)
        JNEG(I)   = LA(45)
 170  CONTINUE
C
      IF (L2.NE.IONE) GO TO 300
      WRITE (IPRINT,450) (MPFT(I),I=1,53)
      IF (NCRT.EQ.IONE) GO TO 180
      IF (MOD(NARGS,ITWO).EQ.IZERO) GO TO 175
      CALL HEADS (IARGS(KARGS),IONE,IZERO,IONE)
      WRITE (IPRINT,460) (LHEAD(I),I=1,12)
 175  WRITE (IPRINT,550)
 180  WRITE (IPRINT,470)
      MSPT = ISPT + 49
      WRITE (IPRINT,480) ITRACE, (N(I),I=ISPT,MSPT)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      NSPT = MSPT + 125
      MSPT = MSPT + IONE
      WRITE (IPRINT,482) (N(I),I=MSPT,NSPT)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      NSPT = NSPT + IONE
      WRITE (IPRINT,485) (N(I),I=NSPT,INNI), MSTORE, JZERO, JNEG
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      ISPT = INNI + IONE
      INNI = INNI + 200
      WRITE (IPRINT,490) (N(I),I=ISPT,INNI)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      IF (NCRT.EQ.IONE) CALL PAGE (0)
      WRITE (IPRINT,500)
      IF (IARGS(3).NE.IARGS(4)) GO TO 320
      CALL RFORMT (9,ISIGD,RC,A(20),NBL,0,NW,IZERO,JNEG(1),IRF)
      DO 190 I=NWIDP1,NX
        JNEG(I) = LA(45)
 190  CONTINUE
C
      CALL RFORMT (1,ISIGD,RC,A(19),NBL,0,NWIDTH,NDEC,N(INNI+1),IRF)
      INNK = INNI + NX
      INNI = INNI + IONE
      WRITE (IPRINT,510) (N(I),I=INNI,INNK), JNEG
      ISP  = INSPV
      DO 200 I=1,3
        CALL RFORMT (1,2,RC,A(I+20),NBL,0,NWIDTH,NDEC,N(ISP),IRF)
        ISP = ISP + NX
 200  CONTINUE
C
      INNI = INSPV + 74
      WRITE (IPRINT,520) (N(I),I=INSPV,INNI)
      DO 210 I=1,6
        IRSLT(I)  = IYES
        IRSLTP(I) = A(I+23)
        IF (A(I+23).EQ.RZERO) IRSLT(I) = NO
 210  CONTINUE
C
C     SET IRSLT(I),I=1,6  FOR YES OR NO. ALSO A(I),I=24,29.
C
      DO 220 I=1,5
        IRSLTA(1,I) = IBLK
        IRSLTA(2,I) = IBLK
 220  CONTINUE
C
      IRSLTA(1,3) = NOA
      IRSLTA(2,3) = NOAB
      IA30        = A(30)
      IF (IA30.EQ.IZERO) GO TO 250
      IF (IA30.EQ.ITWO) GO TO 240
      IF (IA30.GT.IONE) GO TO 230
      IRSLTA(1,1) = IPPRA
      IRSLTA(2,1) = IPPRB
      GO TO 250
C
 230  IRSLTA(1,1) = IPPRA
      IRSLTA(2,1) = IPPRB
      IRSLTA(1,2) = IANDA
      IRSLTA(2,2) = IANDB
 240  IRSLTA(1,3) = LOWRA
      IRSLTA(2,3) = LOWRB
 250  IRSLTA(1,5) = NOA
      IRSLTA(2,5) = NOAB
      IF (ISTOCR+ISTCHC.EQ.ITHRE) GO TO 270
      IF (ISTOCR.EQ.IZERO) GO TO 260
      IRSLTA(1,5) = IRWA
      IRSLTA(2,5) = IRWB
      GO TO 280
C
 260  IF (ISTCHC.EQ.IZERO) GO TO 280
      IRSLTA(1,5) = ICLMA
      IRSLTA(2,5) = ICLMB
      GO TO 280
C
 270  IRSLTA(1,4) = IBTHH
      IRSLTA(2,4) = IBTHHA
      IRSLTA(1,5) = IBTHA
      IRSLTA(2,5) = IBTHB
 280  IRSLTP(7)   = A(30)
      IRSLTP(8)   = A(31)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      WRITE (IPRINT,530) (IRSLT(I),IRSLTP(I),I=1,6)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      WRITE (IPRINT,540) ((IRSLTA(I,J),I=1,2),J=1,3), IRSLTP(7),
     1                   ((IRSLTA(I,J),I=1,2),J=4,5), IRSLTP(8)
      IF (NCRT.EQ.IZERO) THEN
        DO 290 I=1,2
          WRITE (IPRINT,550)
 290    CONTINUE
      ENDIF
C
      WRITE (IPRINT,560)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      WRITE (IPRINT,562)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      WRITE (IPRINT,565)
      RETURN
C
C     ..................................................................
C
C     APROPERTIES PRINTING.
C
 300  WRITE (IPRINT,450) (MPFT(I),I=1,53)
      IF (NCRT.EQ.IONE) GO TO 310
      IF (MOD(NARGS,ITWO).EQ.IZERO) GO TO 305
      CALL HEADS (IARGS(KARGS),IONE,IZERO,IONE)
      WRITE (IPRINT,460) (LHEAD(I),I=1,12)
 305  WRITE (IPRINT,550)
 310  WRITE (IPRINT,470)
      MSPT = ISPT + 49
      WRITE (IPRINT,480) ITRACE, (N(I),I=ISPT,MSPT)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      NSPT = MSPT + 125
      MSPT = MSPT + IONE
      WRITE (IPRINT,482) (N(I),I=MSPT,NSPT)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      NSPT = NSPT + IONE
      WRITE (IPRINT,485) (N(I),I=NSPT,INNI), MSTORE, JZERO, JNEG
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,550)
      ISPT = INNI + IONE
      INNI = INNI + 200
      WRITE (IPRINT,490) (N(I),I=ISPT,INNI)
      RETURN
C
C     ..................................................................
C
C     MPROPERTIES PRINTING FOR A NON-SQUARE MATRIX.
C
 320  DO 330 I=1,2
        IRSLTA(1,I) = NOA
        IRSLTA(2,I) = NOAB
 330  CONTINUE
C
      IF (IPROP(4).EQ.ITWO) GO TO 430
      IF (IABS(IPROP(4)).GE.IFOUR) GO TO 380
      DO 340 I=1,2
        IRSLTA(1,I) = IRWA
        IRSLTA(2,I) = IRWB
 340  CONTINUE
C
      IF (IPROP(4).GT.IZERO) GO TO 350
      A(26) = ITWO
      GO TO 360
C
 350  A(26) = IONE
 360  IF (IPROP(5).GT.IZERO) GO TO 370
      A(27) = ITWO
      GO TO 430
C
 370  A(27) = IONE
      GO TO 430
C
 380  DO 390 I=1,2
        IRSLTA(1,I) = ICLMA
        IRSLTA(2,I) = ICLMB
 390  CONTINUE
C
      IF (IPROP(4).GT.IZERO) GO TO 400
      A(26) = IFOUR
      GO TO 410
C
 400  A(26) = ITHRE
 410  IF (IPROP(5).GT.IZERO) GO TO 420
      A(27) = IFOUR
      GO TO 430
C
 420  A(27) = ITHRE
 430  IRSLTP(1) = A(26)
      IRSLTP(2) = A(27)
      WRITE (IPRINT,570) ((IRSLTA(J,I),J=1,2),IRSLTP(I),I=1,2)
      IF (NCRT.EQ.IZERO) THEN
        DO 440 I=1,20
          WRITE (IPRINT,550)
 440    CONTINUE
      ENDIF
C
      WRITE (IPRINT,580)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 450  FORMAT (19H PROPERTIES OF THE ,53A1)
 460  FORMAT (/   6X,31HROW - FOR PROPERTIES STORED IN ,12A1)
 470  FORMAT (    4X,7HGENERAL)
 480  FORMAT (8X,11H1   TRACE (,I3,13H VALUES USED),9X,25A1/
     1 7X,37H 2   TRACE NUMBER TWO                ,25A1)
 482  FORMAT ( 
     1 7X,37H 3   MAXIMUM NUMBER                  ,25A1/
     2 7X,37H 4   MINUMUM NUMBER                  ,25A1/
     3 7X,37H 5   MAXIMUM NUMBER IN ABSOLUTE VALUE,25A1/
     4 7X,37H 6   MINUMUM NUMBER IN ABSOLUTE VALUE,25A1/
     5 7X,37H 7   MINIMUM NON-ZERO ABSOLUTE VALUE ,25A1)
 485  FORMAT (
     1 7X,37H 8   NUMBER OF POSITIVE NUMBERS      ,25A1/
     2 7X,37H 9   NUMBER OF ZERO NUMBERS          ,25A1/
     3 7X,37H10   NUMBER OF NEGATIVE NUMBERS      ,25A1)
 490  FORMAT (7X,37H11   SUM OF TERMS                     , 25A1/
     1 7X,37H12   AVERAGE                         ,25A1/
     2 7X,37H13   SUM OF SQUARES                  ,25A1/
     3 7X,37H14   SUM OF SQUARES ABOUT MEAN       ,25A1/
     4 7X,37H15   WITHIN ROWS SUM OF SQUARES      ,25A1/
     5 7X,37H16   WITHIN COLS SUM OF SQUARES      ,25A1/
     6 7X,37H17   SUM OF ABSOLUTE VALUES          ,25A1/
     7 7X,37H18   AVERAGE OF ABSOLUTE VALUES      ,25A1)
 500  FORMAT (     4X   ,8HSPECIFIC )
 510  FORMAT (7X,37H19   DETERMINANT                      ,25A1   /
     1 7X,37H20   RANK                            ,25A1)
 520  FORMAT (21X,5HNORMS/
     1 7X,37H21   SQUARE ROOT OF SUM OF B(I,J)**2 ,25A1/
     2 7X,37H22   N*MAXIMUM(B(I,J))               ,25A1 /
     3 7X,37H23   MAXIMUM VALUE OF ROW SUM        ,25A1 )
 530  FORMAT ( 1X,6X,27H24   NORMALITY             ,28X,A3,2H*(,I1,1H)/
     1 7X,33H25   SYMMETRY                      ,22X,A3,2H*(,I1,1H)/
     2 7X,33H26   SKEW SYMMETRY                 ,22X,A3,2H*(,I1,1H)/
     3 7X,33H27   DIAGONALITY                   ,22X,A3,2H*(,I1,1H)/
     4 7X,33H28   ORTHOGONALITY:  A'A = I       ,22X,A3,2H*(,I1,1H)/
     5 7X,2H29,19X,21HA'A = DIAGONAL MATRIX,13X,A3,2H*(,I1,1H))
 540  FORMAT ( 1X,6X,15H30   TRIANGULAR,26X,3A3,A1,2A3,3H**(,I1,1H)/
     1 7X,39H31   STOCHASTIC (ROW AND/OR COL SUMS=1),5X,4A3,4H***(,I1,
     2 1H))
 550  FORMAT (1H )
 560  FORMAT (2X,70H*   IF ANSWER IS NO: (R,C)= 0. IF ANSWER IS YES: (R,
     1C)= 1, IF EXACT OR/
     26X,30H(R,C)= 2, IF TOLERANCE IS MET.)
 562  FORMAT (   2X,10HTRIANGULAR/
     12X,65H**  (R,C)=0, IF ANSWER IS NO. (R,C)=1, IF UPPER PART OF MATR
     2IX=0.     /6X,63H(R,C)=2, IF LOWER PART=0. (R,C)=3, IF ALL OFF DIA
     3GONAL TERMS=0.)
 565  FORMAT (        2X,10HSTOCHASTIC/2X,69H*** (R,C)=0, IF MATRIX IS N
     6OT STOCHASTIC. (R,C)=1, IF EACH ROW SUM=1./6X,63H(R,C)=2, IF EACH 
     7COL SUM=1. (R,C)=3, IF EACH ROW AND COL SUM=1.)
 570  FORMAT (7X,31H28   ORTHOGONALITY:  A'A = I   ,21X,2A3,2H*(,I1,1H)/
     1 7X,2H29,19X,21HA'A = DIAGONAL MATRIX,10X,2A3,2H*(,I1,1H))
 580  FORMAT (4X,40H*  (R,C)=0, IF MATRIX IS NOT ORTHOGONAL./7X,47H(R,C)
     1=1 OR 2, IF MATRIX IS ORTHOGONAL ROW WISE./7X,50H(R,C)=3 OR 4, IF
     2MATRIX IS ORTHOGONAL COLUMN WISE./5X,47H( (R,C)=I: IF I=1 OR 3, OR
     3THOGONALITY IS EXACT;/16X,51HIF I=2 OR 4, RELATIVE WITHIN ERROR BO
     4UND OF .1E-6.))
C
C     ==================================================================
C
      END
*MPRSPC
      SUBROUTINE MPRSPC (K,IPROP,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MPRSPC V 7.00  5/21/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE THE SPECIFIC PROPERTIES OF A SQUARE MATRIX.
C
C     INPUT ...
C
C        K THE STARTING LOCATION OF THE MATRIX.
C
C     OUTPUT ...
C
C       IPROP RESULTS FROM CHECKING PROPERTIES OF MATRIX.
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
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION  IPROP(*)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ERR(4)
      REAL             DET, RANK
C
C     ==================================================================
C
      ISUB1   = IHRD
      ISUBUL  = IARGS(3) * IARGS(3) + ISUB1
      ISUBB   = IARGS(3) * IARGS(3) + ISUBUL
      ISUBR   = IARGS(3) + ISUBB
      ISUBDX  = IARGS(3) + ISUBR
      ISUBPS  = IARGS(3) + ISUBDX
      CALL INVCHK (RC(K),NROW,A(ISUB1),A(ISUBUL),IARGS(3),A(ISUBB),
     1             A(ISUBR),A(ISUBDX),A(1),A(ISUBPS),IONE,ERR,IND)
      IF (IND.NE.IZERO) RETURN
      KA = K
      M  = IHRD
      L  = IARGS(3)
      DO 20 I=1,L
        KB = KA
        DO 10 J=1,L
          A(M) = RC(KB)
          KB = KB + IONE
          M = M + IONE
  10    CONTINUE
        KA = KA + NROW
  20  CONTINUE
C
      CALL DETRNK (A(100),L,L,DET,RANK)
      CALL TRIMAT (RC(K),IARGS(3),INDU,INDB)
      CALL PROCHK (RC(K),NROW,IARGS(3),IARGS(4),IPROP,A(1))
      A(30) = RZERO
      IF (INDU.EQ.IZERO) A(30) = A(30) + RONE
      IF (INDB.EQ.IZERO) A(30) = A(30) + RTWO
      A(19) = DET
      A(20) = RANK
      A(21) = ERR(1)
      A(22) = ERR(2)
      A(23) = ERR(3)
      A(24) = RZERO
      IF (IPROP(3).EQ.IZERO) A(24) = RONE
      IF (IPROP(3).EQ.IONE)  A(24) = RTWO
      A(25) = RZERO
      IF (IPROP(2).LT.ITWO)  A(25) = IPROP(2) + IONE
      A(26) = RZERO
      IF (IPROP(2).GT.ITWO)  A(26) = IPROP(2) - ITWO
      A(27) = RZERO
      IF (IPROP(1).EQ.IZERO) A(27) = RONE
      A(28) = RZERO
      IF (IPROP(4).LT.ITWO)  A(28) = IPROP(4) + IONE
      A(29) = RZERO
      IF (IPROP(5).LT.ITWO)  A(29) = IPROP(5) + IONE
      RETURN
C
C     ==================================================================
C
      END
*MRAISE
      SUBROUTINE MRAISE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MRAISE V 7.00  5/21/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     RAISE A MATRIX TO A POWER.
C
C        GENERAL FORMS OF MRAISE ARE ...
C           MRAISE (R),(C) SIZE (R),(C) TO (M) POWER PUT IN (R),(C)
C
C               POWER, M, MAY BE INTEGER OR REAL.
C                  IF M = 0, IDENTITY MATRIX IS COMPUTED.
C
C               WRITTEN BY -
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1967.
C                   CURRENT VERSION -       MAY, 1991.
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
      REAL             FDPCON
C
      DOUBLE PRECISION XP(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.7) CALL ERROR (10)
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGER.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.EQ.IZERO) GO TO 20
      IF (KIND(NARGS-2).NE.IZERO) GO TO 10
      CALL ERROR (3)
      GO TO 20
C
  10  IARGS(NARGS-2) = ARGS(NARGS-2)
C
C     CHECK TO SEE IF M (POWER) IS NEGATIVE.
C
  20  IF (IARGS(NARGS-2).LT.IZERO) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT.
C
      IF (IARGS(3).NE.IARGS(4)) CALL ERROR (3)
C
C     CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE.
C
      NPOW     = IARGS(NARGS-2) - IONE
      IARGS(5) = IARGS(NARGS-1)
      IARGS(6) = IARGS(NARGS)
      IARGS(7) = IARGS(3)
      IARGS(8) = IARGS(4)
      J = ITWO
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 30
      IF (J.LT.IONE) GO TO 40
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  30  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK TO SEE IF PREVIOUS ERRORS.
C
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      ISIZE = IARGS(3)
      ISAV = IARGS(5)
C
C     BEGIN MULTIPLICATION.
C
C     MOVE ORIGINAL MATRIX TO SCRATCH AREA  (COLUMNWISE).
C
      IF (NPOW.EQ.IZERO) GO TO 50
      IF (NPOW.GT.IZERO) GO TO 60
      IEXT = IONE
      GO TO 70
C
  50  IEXT = ITWO
      GO TO 70
C
  60  IEXT = ITHRE
  70  IP   = IARGS(1)
      IC   = IONE
      DO 170 J=1,ISIZE
        DO 130 I=1,ISIZE
C
          GO TO (80,110,120), IEXT
C
  80      IF (I.EQ.J) GO TO 90
          RC(ISAV) = RZERO
          GO TO 100
C
  90      RC(ISAV) = RONE
 100      ISAV = ISAV + IONE
          GO TO 130
C
 110      RC(ISAV) = RC(IP)
          IP = IP + IONE
          GO TO 100
C
 120      A(IC) = RC(IP)
          IC = IC + IONE
          IP = IP + IONE
 130    CONTINUE
C
        GO TO (140,150,160), IEXT
C
 140    ISAV = ISAV + NROW - ISIZE
        GO TO 170
C
 150    ISAV = ISAV + NROW - ISIZE
 160    IP = IP + NROW - ISIZE
 170  CONTINUE
C
      IF (IEXT.LE.ITWO) RETURN
      IXP = NS - ISIZE * ITWO
      DO 240 K=1,NPOW
        ISAVP = IARGS(5)
        IMP   = NS2
        IF (K.GT.IONE) GO TO 180
        IRP   = IARGS(1)
        GO TO 190
C
 180    IRP = IARGS(5)
 190    DO 230 I=1,ISIZE
          ISAV = ISAVP
          IC   = IONE
          IR   = IRP
          IX   = IXP
C
C         SAVE ROW OF MATRIX.
C
          DO 200 J=1,ISIZE
            A(IX) = RC(IR)
            IX = IX - IONE
            IR = IR + NROW
 200      CONTINUE
          DO 220 J=1,ISIZE
            IX = IXP
            IM = IMP
            CALL DSUMAL (XP,IZERO,DSUM)
            DO 210 JP=1,ISIZE
              XP(1) = DBLE (A(IX)) * DBLE (A(IC))
              CALL DSUMAL (XP,-IONE,DSUM)
              IM = IM - IONE
              IX = IX - IONE
              IC = IC + IONE
 210          CONTINUE
            CALL DSUMAL (XP,IONE,DSUM)
            RC(ISAV) = FDPCON (DSUM)
C
            ISAV = ISAV + NROW
 220      CONTINUE
          ISAVP = ISAVP + IONE
          IRP   = IRP + IONE
 230    CONTINUE
 240  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MSCROW
      SUBROUTINE MSCROW
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MSCROW V 7.00  4/18/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 1,    PARSUM OF COL (C) , PUT IN COL (C)
C     L2 = 2,    PARPRODUCT OF COL (C), PUT IN COL (C)
C     L2 = 3,    RMS OF COL (C), PUT IN COL (C)
C     L2 = 4,    SUM COL (C), PUT IN COL (C)
C                SUM COL (C) FROM ROW (R) TO ROW (R) STORE IN COL (C)
C                SUM COL (C)  FROM ROWS (R), (R), (R), ETC PUT IN (C)
C
C               WRITTEN BY -
C                      CARLA MESSINA
C               MODIFIED BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JUNE, 1967.
C                   CURRENT VERSION -    APRIL, 1991.
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
      REAL             ELEM, FNRMAX
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION X(1)
      DOUBLE PRECISION DSUM
C
      EQUIVALENCE (X(1),A(1))
C
C     ==================================================================
C
      IF (NARGS.EQ.ITWO) GO TO 10
      IF (NARGS.GE.IFOUR .AND. L2.EQ.IFOUR) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (NRMAX.GT.IZERO) GO TO 20
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  20  CALL ADRESS (IONE,J1)
      IF (J1.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (NARGS,J2)
      IF (J2.LT.IZERO) CALL ERROR (20)
      IF (NARGS.LT.ITHRE) GO TO 80
      NARG1 = NARGS - IONE
      DO 30 I=2,NARG1
        IF (KIND(I).NE.IZERO)  CALL ERROR (20)
        IF (IARGS(I).LE.IZERO) CALL ERROR (16)
        IF (IARGS(I).GT.NROW)  CALL ERROR (16)
  30  CONTINUE
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (NARGS.GT.IFOUR) GO TO 60
C
C     SUM FROM ROW (R) TO ROW (R).
C
      IF (IARGS(2).LE.IARGS(3)) GO TO 40
      I = IARGS(2)
      IARGS(2) = IARGS(3)
      IARGS(3) = I
  40  JA = IZERO
  50  J  = J1 + IARGS(2)
      JA = JA + IONE
      A(JA) = RC(J-1)
      IARGS(2) = IARGS(2) + IONE
      IF (IARGS(2).LE.IARGS(3)) GO TO 50
      CALL SUMMAL (A(1),JA,ELEM)
      IF (JA.EQ.IONE) ELEM = A(1)
      GO TO 170
C
C     SUM DISCRETE ROWS.
C
  60  DO 70 I=2,NARG1
        J = J1 + IARGS(I)
        A(I) = RC(J-1)
  70  CONTINUE
      CALL SUMMAL (A(2),NARG1-IONE,ELEM)
      IF (NARG1-IONE.EQ.IONE) ELEM = A(2)
      GO TO 170
C
  80  IF (NERROR.NE.IZERO) RETURN
      FNRMAX = NRMAX
C
C     PARSUM, PARPRODUCT.
C
      IF (L2.EQ.ITHRE) GO TO 130
      IF (L2.GT.ITHRE) GO TO 160
      J = L2 - IONE
      RC(J2) = RC(J1)
      IF (NRMAX.EQ.IONE) RETURN
      IF (J.EQ.IZERO) GO TO 100
      DO 90 I=2,NRMAX
        J1 = J1 + IONE
        J2 = J2 + IONE
        RC(J2) = RC(J2-1) * RC(J1)
  90  CONTINUE
      RETURN
C
C     ..................................................................
C
 100  DO 110 JJ=2,NRMAX
        I = JJ
        CALL SUMMAL (RC(J1),I,A(I))
      IF (I.EQ.IONE) A(I) = RC(J1)
 110  CONTINUE
C
      DO 120 I=2,NRMAX
        J2 = J2 + IONE
        RC(J2) = A(I)
 120  CONTINUE
      RETURN
C
C     ..................................................................
C
C     RMS.
C
 130  IF (NRMAX.LE.NS2) GO TO 140
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
 140  CALL DSUMAL (X(1),0,DSUM)
      DO 150 I=1,NRMAX
        J = J1 + I
        X(1) = RC(J-1)**2
        CALL DSUMAL (X(1),-1,DSUM)
 150  CONTINUE
C
      CALL DSUMAL (X(1),1,DSUM)
      IF (NRMAX.EQ.IONE) DSUM = X(1)
      ELEM = FDPCON (DSUM)
      ELEM = FSQRT (FDIV (ELEM,FNRMAX,IND))
      GO TO 170
C
C     SUM ENTIRE COLUMN.
C
 160  CALL SUMMAL (RC(J1),NRMAX,ELEM)
      IF (NRMAX.EQ.IONE) ELEM = RC(J1)
C
 170  CALL VECTOR (ELEM,J2)
      RETURN
C
C     ==================================================================
C
      END
*MTRIAN
      SUBROUTINE MTRIAN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MTRIAN V 7.00  5/20/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRIANGULARIZATION OF NON-SINGULAR, REAL SYMMETRIC MATRIX
C     A=TT'  LOWER TRIANGLE IS COMPUTED
C
C     INSTRUCTION IS ...
C     MTRIAN A(R,C),R,C, STORE T IN (R,C)
C               OR
C     MTRIAN  A(R,C),R,C, STORE T IN (R,C) AND T INVERSE (R,C)
C
C     THE UPPER TRIANGLE IS SET = 0.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1967.
C                   CURRENT VERSION -      MAY, 1991.
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
      REAL             S, SUM1
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION XP(1)
      DOUBLE PRECISION DSUM1, DSUM2
C
C
C     ==================================================================
C
      J = ITWO
      IF (NARGS.EQ.6 .OR. NARGS.EQ.8) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (IARGS(3).EQ.IARGS(4)) GO TO 20
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  20  IF (NARGS.EQ.6) GO TO 30
      J = ITHRE
      IARGS( 9) = IARGS(7)
      IARGS(10) = IARGS(8)
      IARGS(11) = IARGS(3)
      IARGS(12) = IARGS(4)
  30  IARGS( 7) = IARGS(3)
      IARGS( 8) = IARGS(4)
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 40
      IF (J.LT.IONE) GO TO 50
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  40  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
  50  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IR  = IARGS(3)
C     IRM = IR - IONE
      K   = IARGS(1)
      DO 70 I=1,IR
        IF (RC(K).GT.RZERO) GO TO 60
C
C       MATRIX CAN NOT BE TRIANGULIZED SINCE ONE OF THE TERMS ON
C          THE DIAGONAL IS ZERO OR LESS.
C
        CALL ERROR (35)
        RETURN
  60    K = K + IONE + NROW
  70  CONTINUE
C
      K = IARGS(1)
      CALL SYMV (RC(K),IR,M)
      IF (M.LE.IONE) GO TO 80
C
C     NON-SYMMETRIC MATRIX.
C
      CALL ERROR (31)
      RETURN
C
C     ..................................................................
C
  80  M    = ITWO
      A(1) = FSQRT (RC(K))
      K    = K + IONE
      DO 90 I=2,IR
        A(M) = FDIV (RC(K),A(1),IND)
        K = K + IONE
        M = M + IONE
  90  CONTINUE
C
      KA = IARGS(1)
      KB = KA + NROW + IONE
      MA = ITWO
      DO 150 KK=2,IR
        I      = KK
        MB     = MA
        CALL DSUMAL (XP,IZERO,DSUM1) 
        XP(1) = RC(KB)
        CALL DSUMAL (XP,-IONE,DSUM1)
        M      = (I-IONE) * IR + I
        II     = I - IONE
        DO 100 J=1,II
          XP(1) = -(DBLE(A(MB))**2)
          CALL DSUMAL (XP,-IONE,DSUM1)
          MB   = MB - IR
 100    CONTINUE
        CALL DSUMAL (XP,IONE,DSUM1)
        IF (DSUM1.GT.RZERO) GO TO 110
C
C       LEADING SUBMATRIX IS SINGULAR.
C
        CALL ERROR (22)
        RETURN
C
C     ..................................................................
C
 110    S    = FDPCON (DSUM1)
        S    = FSQRT (S)
        A(M) = S
        M    = M + IONE
        IF (I.EQ.IR) GO TO 140
        IP   = I + IONE
        KC   = KB + IONE
        DO 130 J=IP,IR
          CALL DSUMAL (XP,IZERO,DSUM1)
          XP(1) = RC(KC)
          CALL DSUMAL (XP,-IONE,DSUM1)
          KC     = KC + IONE
          MC     = J
          MD     = I
          DO 120 JJ=1,II
            XP(1) = -DBLE(A(MC)) * DBLE(A(MD))
            CALL DSUMAL (XP,-IONE,DSUM1)
            MC   = MC + IR
            MD   = MD + IR
 120      CONTINUE
          CALL DSUMAL (XP,IONE,DSUM1)
          SUM1 = FDPCON(DSUM1)
          A(M) = FDIV (SUM1,S,IND)
          M    = M + IONE
 130    CONTINUE
        MA = MA + IR + IONE
 140    KB = KB + NROW + IONE
 150  CONTINUE
C
      K  = IARGS(5) - IONE
      KB = IARGS(5)
      DO 190 I=1,IR
        KA = K + I
        M  = (I-IONE) * IR + I
        KC = KB
        DO 160 J=I,IR
          RC(KA) = A(M)
          KA = KA + IONE
          M  = M + IONE
 160    CONTINUE
        IF (I.EQ.IONE) GO TO 180
        II = I - IONE
        DO 170 J=1,II
          RC(KC) = RZERO
          KC = KC + IONE
 170    CONTINUE
 180    KB = KB + NROW
        K  = K + NROW
 190  CONTINUE
C
      IF (NARGS.EQ.6) RETURN
      KC = IARGS(5)
      DO 230 I=1,IR
        M    = (I-IONE) * IR + I
        A(M) = FDIV (RONE,RC(KC),IND)
        IF (I.EQ.IR) GO TO 220
        M    = M + IONE
        IP   = I + IONE
        KB   = KC + NROW + IONE
        JC   = IONE
        DO 210 J=IP,IR
          KA = KC + J - I
          MA = (I-IONE) * IR + I
          CALL DSUMAL (XP,IZERO,DSUM2)
          DO 200 JA=1,JC
            XP(1) = DBLE (RC(KA)) * DBLE (A(MA))
            CALL DSUMAL (XP,-IONE,DSUM1)
            MA   = MA + IONE
            KA   = KA + NROW
 200      CONTINUE
          CALL DSUMAL (XP,IONE,DSUM2)
          S    = FDPCON (DSUM2)
          A(M) = FDIV (-S,RC(KB),IND)
          KB   = KB + NROW + IONE
          M    = M + IONE
          JC   = JC + IONE
 210    CONTINUE
 220    KC = KC + NROW + IONE
 230  CONTINUE
C
      K  = IARGS(9) - IONE
      KB = IARGS(9)
      DO 270 I=1,IR
        KA = K + I
        M  = (I-IONE) * IR + I
        KC = KB
        DO 240 J=I,IR
          RC(KA) = A(M)
          KA     = KA + IONE
          M      = M + IONE
 240    CONTINUE
        IF (I.EQ.IONE) GO TO 260
        II = I - IONE
        DO 250 J=1,II
          RC(KC) = RZERO
          KC     = KC + IONE
 250    CONTINUE
 260    KB = KB + NROW
        K  = K + NROW
 270  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MXTX
      SUBROUTINE MXTX
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   MXTX V 7.00  5/20/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     MULTIPLY MATRIX A BY ITS TRANSPOSE.
C        OR TRANSPOSE OF MATRIX A BY MATRIX A.
C
C        L2 = 1,  MULTIPLY MATRIX BY ITS TRANSPOSE.
C           GENERAL FORM OF INSTRUCTION ...
C              M(XXT)  A(,) N,K,  STORE IN  C(,)    N,K DEFINE X
C
C        L2 = 2,  MULTIPLY TRANSPOSE OF MATRIX BY ITSELF.
C           GENERAL FORM OF INSTRUCTION ...
C              M(XTX)  A(,) N,K  STORE IN  C(,)    N,K  DEFINE X
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - APRIL, 1968.
C                   CURRENT VERSION -   MAY, 1991.
C
C     ==============================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION AP(3000)
C
      EQUIVALENCE (AP(1),A(1))
C
C     ==================================================================
C
C     CHECK FOR CORRECT NUMBER OF AGRUMENTS.
C
C     DECIDE WHETHER COMMAND IS M(XAX') OR M(X'AX).
C     L2 = 3, MEANS M(XAX'). L2 = 2, NARGS.GT. 6, MEANS M(X'AX).
C
      IF (L2.GT.ITWO) GO TO 10
      IF (L2.LT.ITWO) GO TO 20
      IF (NARGS.LE.6) GO TO 20
  10  L2 = IFOUR - L2
      CALL TRANSF
      RETURN
C
C     ..................................................................
C
  20  IF (NARGS.NE.6) CALL ERROR (10)
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE.
C        COMPUTE ADDRESSES.
C
      IF (L2.EQ.ITWO) GO TO 30
      IARGS(8) = IARGS(3)
      IARGS(7) = IARGS(3)
      GO TO 40
C
  30  IARGS(8) = IARGS(4)
      IARGS(7) = IARGS(4)
  40  J = ITWO
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 50
      IF (J.LT.IONE) GO TO 60
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  50  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS.
C
  60  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IG = IARGS(1)
      CALL MXTXP (RC(IG),NROW,IARGS(3),IARGS(4),A,L2)
      IF (L2.EQ.ITWO) GO TO 70
      NROWP = IARGS(3)
      GO TO 80
C
  70  NROWP = IARGS(4)
  80  NCOLP = NROWP
      IG    = IARGS(5)
      CALL STORMT (RC(IG),NROW,NROWP,NCOLP,A)
C
C     MOVE FROM SCRATCH AREA TO STORAGE.
C
      RETURN
C
C     ==================================================================
C
      END
*NBRAN
      SUBROUTINE NBRAN (IRAN,KRAN,N,P,NPAR,X,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NBRAN V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE N NEGATIVE BINOMIAL RANDOM NUMBERS IN X(.)
C
C     BASED ON DATAPAC PROGRAM UNIT NBRAN WRITTEN BY JAMES J. FILLIBEN.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1978.
C                   CURRENT VERSION -  FEBRUARY, 1991.
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
      REAL             X(*)
      REAL             P
      REAL             PCUT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA PCUT / 0.1 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
         IND = IONE
         RETURN
C
C     ..................................................................
C
  10  IF (NPAR.GE.IONE) GO TO 20
         IND = IONE
         RETURN
C
C     ==================================================================
C
C     BRANCH TO THE FASTER GENERATION METHOD DEPENDING UPON
C        THE SIZE OF P.
C
  20  IF (P.LT.PCUT) GO TO 60
C
C     IF P IS NOT SMALL, USE THE FACT THAT THE WAITING TIME FOR NPAR
C        SUCCESSES IN BERNOULLI TRIALS HAS A NEGATIVE BINOMIAL
C           DISTRIBUTION.
C
      DO 50 I=1,N
        ISUM = IZERO
        J   = IONE
  30    CALL BINRAN (IRAN,KRAN,IONE,P,IONE,X(I),IND)
        IB  = X(I) + RHALF
        ISUM = ISUM + IB
        IF (ISUM.EQ.NPAR) GO TO 40
        J   = J + IONE
        GO TO 30
  40    X(I) = J - NPAR
  50  CONTINUE
      RETURN
C
C     ..................................................................
C
C     IF P IS SMALL, GENERATE N NEGATIVE BINOMIAL RANDOM NUMBERS USING
C        THE FACT THAT THE SUM OF GEOMETRIC VARIATES IS A NEGATIVE
C           BINOMIAL VARIATE.
C           TRIALS HAS A GEOMETRIC DISTRIBUTION.
C
  60  DO 80 I=1,N
        ISUM = IZERO
        DO 70 J=1,NPAR
          CALL GEORAN (IRAN,KRAN,1,P,A,IND)
          IG   = A(1) + RHALF
          ISUM = ISUM + IG
  70    CONTINUE
        X(I) = ISUM
  80  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*NORPLT
      SUBROUTINE NORPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NORPLT V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A NORMAL (GAUSSIAN)
C              PROBABILITY PLOT.
C              THE PROTOTYPE NORMAL DISTRIBUTION USED HEREIN
C              HAS MEAN = 0 AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/SQRT(2*PI)) * EXP(-X*X/2).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE NORMAL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE NORMAL DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE NORMAL PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, NORPPF, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIEN
C                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-11
C               --RYAN AND JOINER, 'NORMAL PROBABILITY PLOTS AND TESTS
C                 FOR NORMALITY'  PENNSYLVANIA
C                 STATE UNIVERSITY REPORT.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
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
C                   CURRENT VERSION - DECVEMBER, 1989.
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
      REAL             AN, CC, PPF, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        M*1, MT*1
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
      CALL SORTPP (X,N,Y)
      CALL UNIMED (N,W)
      DO 10 I=1,N
        CALL NORPPF (W(I),PPF,IND1)
        W(I) = PPF
        IF (IND1.NE.IZERO) CALL ERROR (249)
  10  CONTINUE
C
      IF (LWIDE.GE.76) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.64 .AND. LWIDE.LT.76) WRITE (IPRINT,110)
     1     (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.64) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = RZERO
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 20 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  20  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 30 I=1,N
        ATEMP(1) = Y(I) * W(I)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  30  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 40 I=1,N
        ATEMP(1) = W(I)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
  40  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1) * WBAR
      CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 50 I=1,10
        MT(K) = M(I)
        K = K + IONE
  50  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+73.GT.LWIDE) GO TO 60
      WRITE (IPRINT,130) CC, (MT(J),J=1,K)
      GO TO 90
  60  CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
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
      IF (K+38.GT.LWIDE) GO TO 80
      WRITE (IPRINT,140) CC, (MT(J),J=1,K)
      GO TO 90
  80  IF (LWIDE.LT.37) GO TO 90
      WRITE (IPRINT,150) CC
  90  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 100  FORMAT (15X, 6HNORMAL            ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X, 6HNORMAL            ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X, 6HNORMAL            ,14H PROB PLOT OF ,
     1                           12A1)
 130  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 140  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 150  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*OANOVA
      SUBROUTINE OANOVA (SU,IBS,FM,M,ISUBQ,SSQ,IHC,NSU,MA,LN,MCL)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. OANOVA V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE AND PRINT ANALYSIS OF VARIANCE.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1969.
C                   CURRENT VERSION -   APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IHC(*), MA(*)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
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
      REAL             FM, SSQ, SU
      REAL             ASUM, F1, F2, PF1, PF2, RESMS, RESSS, SSU
      REAL             SUM, VR, V1F2
      REAL             FDIV, FDPCON
C
C     ...................................................................
C
      DOUBLE PRECISION FDDIV
C
C     ...................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        IHC*3, MA*1
      CHARACTER        LDEMRK*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LDEMRK/ '-' /
C
C     ==================================================================
C
      MXLINE = MCL
      JSUBS = IBS + M
      RESSS = A(JSUBS)
      SUM = A(JSUBS+1)
      RESMS = FDPCON (FDDIV (DBLE(SUM),DBLE(SU),IND))
      IT = IONE
      IF (L2.EQ.ITHRE) IT = ITHRE
      JSPV = IONE
      IF (LWIDE.LT.LWC) JSPV = ITWO
      JSUBS = IBS + M - IONE
      VR = SU - FM
      JSUBQ = ISUBQ + M - IONE
      A(JSUBQ) = RESSS
      IF (M.EQ.IONE) GO TO 20
      DO 10 II=2,M
        JSUBQ = JSUBQ - IONE
        A(JSUBQ) = A(JSUBQ+1) + A(JSUBS)
        JSUBS = JSUBS - IONE
  10  CONTINUE
C
  20  JSUBC = ISUBQ + M
      CALL RFORMT (IZERO,ISIGD,A(JSUBS),RC(1),M+2,17,NW1,NDEC1,MA,IRF)
      CALL RFORMT (IZERO,ISIGD,A(JSUBS),RC(1),M,17,NW2,NDEC2,MA,IRF)
      SSU = SU
      KSUBC = JSUBC
      DO 30 I=1,M
        SSU = SSU - RONE
        A(KSUBC) = FDIV (A(JSUBQ),SSU,IND)
        KSUBC = KSUBC + IONE
        JSUBQ = JSUBQ + IONE
  30  CONTINUE
C
      JSUBQ = JSUBQ - M
      CALL RFORMT (IZERO,ISIGD,A(JSUBC),RC(1),M,17,NW3,NDEC3,MA,IRF)
      LINE = LN + JSPV + IONE
      LENINC = IDIV (MXLINE-JSPV-6,ITEN,IND)
C
C     MAXIMUM NUMBER OF LINES PER PAGE, MULTIPLE OF 10.
C
      LENINC = ITEN * LENINC
      KSUBS = JSUBS
      KSUBQ = JSUBQ
      IF (LENINC.EQ.IZERO) LENINC = IFIVE
      KSTOP = 69
      IF (L2.EQ.IONE) KSTOP = 63
      KSTOPA = 114
      IF (L2.EQ.IONE) KSTOPA = 111
      DO 290 ISPV=1,JSPV
        V1F2  = FM + RONE
        ASUM  = RZERO
        SSU   = SU
        NSUA  = NSU
        JSUBS = KSUBS
        JSUBQ = KSUBQ
        LINC  = IZERO
        NBOT  = IONE
        ISWT  = IZERO
        LINE  = LINE + IFIVE
        IF (LINE.GE.MXLINE) GO TO 40
        IF (LINE+IFIVE.GT.MXLINE) GO TO 40
        LINC = IDIV (MXLINE-LINE,ITEN,IND)
        NTOP = LINC * ITEN
        IF (NTOP.EQ.IZERO) NTOP = IFOUR
        GO TO 50
  40    CALL PAGE (4)
        LINE = JSPV + 9
        NTOP = LENINC
        LINC = IDIV (MXLINE-LINE,ITEN,IND)
  50    NPG = IDIV (M,LENINC,IND) + IONE
        IF (NTOP.GT.M) NTOP = M
        NTEMP = MOD(M,LENINC)
        IF (NTEMP.GT.LINC*ITEN) NPG = NPG + IONE
        DO 220 IPAGE=1,NPG
          IF (NBOT.GT.NTOP) GO TO 230
          IF (ISWT.EQ.IZERO) GO TO 60
          CALL PAGE (IFOUR)
          LINE = JSPV + 9
  60      WRITE (IPRINT,350)
          ISWT = IONE
          IF (LWIDE.LT.LWC) GO TO 70
          IF (IPAGE.EQ.IONE) WRITE (IPRINT,300)
          IF (IT.EQ.IONE) WRITE (IPRINT,310) IHC(IT),IHC(IT+1)
          IF (IT.EQ.ITHRE) WRITE (IPRINT,400)
          WRITE (IPRINT,510) (LDEMRK,KLINE=1,KSTOPA)
          GO TO 110
  70      IF (ISPV.NE.IONE .OR. IPAGE.NE.IONE) GO TO 80
          IF (IT.EQ.IONE) WRITE (IPRINT,360)
          IF (IT.EQ.ITHRE) WRITE (IPRINT,380)
  80      IF (ISPV.EQ.ITWO) GO TO 90
          IF (IT.EQ.IONE) WRITE (IPRINT,370) IHC(IT),IHC(IT+1)
          IF (IT.EQ.ITHRE) WRITE (IPRINT,390)
          GO TO 100
  90      IF (IT.EQ.IONE) WRITE (IPRINT,430) IHC(IT),IHC(IT+1)
          IF (IT.EQ.ITHRE) WRITE (IPRINT,460)
 100      WRITE (IPRINT,510) (LDEMRK,KLINE=1,KSTOP)
 110      DO 210 I=NBOT,NTOP
            NSUA = NSUA - IONE
            ASUM = ASUM + A(JSUBS)
            SSU = SSU - RONE
            IF (ABS(SSU).GT.RZERO) GO TO 130
            RESMS = RZERO
 120        F1 = RZERO
            F2 = RZERO
            PF1 = RONE
            PF2 = RONE
            GO TO 140
 130        RESMS = FDIV (A(JSUBQ),SSU,IND)
            V1F2 = V1F2 - RONE
            IF (ABS(RESMS).LE.RZERO) GO TO 120
C
C       NEVER POOL.
C
            F1 = FDIV (A(JSUBS),SSQ,IND)
            CALL QFORF (RONE,VR,F1,PF1)
C
C       TEST HIGHER SUB-HYPOTHESES.
C
            F2 = FDIV (FDIV (A(JSUBQ)+A(JSUBS)-RESSS,V1F2,IND),SSQ,IND)
            CALL QFORF (V1F2,VR,F2,PF2)
 140        II = IABS (I-1)
            CALL RFORMT (1,ISIGD,RC,A(JSUBS),20-NW1,1,NW1,NDEC1,MA,IRF)
            CALL RFORMT (1,ISIGD,RC,RESMS,20-NW3,1,NW3,NDEC3,MA(41),IRF)
            IF (LWIDE.GE.LWC) GO TO 180
            IF (L2.EQ.ITHRE) GO TO 160
            IF (ISPV.EQ.ITWO) GO TO 150
            WRITE (IPRINT,410) II, (MA(I1),I1=1,20), F1, PF1
            GO TO 200
 150        WRITE (IPRINT,420) II, (MA(I2),I2=41,60), NSUA, F2, PF2
            GO TO 200
 160        CALL HEADS (IARGS(I+3),IONE,IZERO,IONE)
            IF (ISPV.EQ.ITWO) GO TO 170
            WRITE (IPRINT,440) (LHEAD(I1),I1=1,12), (MA(I1),I1=1,20),
     1                          F1, PF1
            GO TO 200
 170        WRITE (IPRINT,450) (LHEAD(I1),I1=1,12), (MA(I2),I2=41,60),
     1                          NSUA, F2, PF2
            GO TO 200
 180        IF (L2.EQ.IONE) GO TO 190
            CALL HEADS (IARGS(I+3),IONE,IZERO,IONE)
            WRITE (IPRINT,470) (LHEAD(I1),I1=1,12), (MA(I1),I1=1,20),
     1                         (MA(I2),I2=41,60), NSUA, F1, PF1, F2, PF2
            GO TO 200
 190        WRITE (IPRINT,320) II, (MA(I1),I1=1,20), (MA(I2),I2=41,60),
     1                         NSUA, F1, PF1, F2, PF2
 200        JSUBQ = JSUBQ + IONE
            JSUBS = JSUBS + IONE
 210      CONTINUE
          LINE = LINE + NTOP - NBOT + IONE
          NBOT = NTOP + IONE
          NTOP = NTOP + LENINC
          IF (NTOP.GT.M) NTOP = M
          IF (JSPV.EQ.IONE) WRITE(IPRINT,510) (LDEMRK,KLINE=1,KSTOPA)
          IF (JSPV.EQ.ITWO) WRITE(IPRINT,510) (LDEMRK,KLINE=1,KSTOP)
 220    CONTINUE
 230    F1 = RESSS
        CALL RFORMT (1,ISIGD,RC,F1,20-NW1,1,NW1,NDEC1,MA,IRF)
        F2 = SUM
        CALL RFORMT (1,ISIGD,RC,F2,20-NW1,1,NW1,NDEC1,MA(21),IRF)
        IF (JSPV.EQ.ITWO) GO TO 260
        LINE = LINE + IFOUR
        IF (LINE.LE.MXLINE) GO TO 240
        LINE = ITHRE
 240    IF (L2.GT.ITWO) GO TO 250
        WRITE (IPRINT,500) (MA(I),I=1,20),
     1                      NSUA, (LDEMRK,KLINE=1,61), (MA(I),I=21,40),
     2                      NSU, (LDEMRK,KLINE=1,61)
        GO TO 280
 250    WRITE (IPRINT,330) (MA(I),I=1,20), NSUA
        WRITE (IPRINT,340) (LDEMRK,KLINE=1,64), (MA(I),I=21,40), NSU,
     1                     (LDEMRK,KLINE = 1,64)
        GO TO 280
 260    IF (ISPV.EQ.JSPV) GO TO 280
        LINE = LINE + ITHRE
        IF (LINE.GT.MXLINE) CALL PAGE (IFOUR)
        IF (LINE.GT.MXLINE) LINE = ITHRE
        IF (IT.EQ.ITHRE) GO TO 270
        WRITE (IPRINT,490) (MA(I),I=1,20), NSUA,
     1                     (LDEMRK,KLINE=1,47), (MA(I),I=21,40), NSU,
     2                     (LDEMRK,KLINE=1,47)
        GO TO 280
 270    WRITE (IPRINT,480) (MA(I),I=1,20), NSUA,
     1                     (LDEMRK,KLINE=1,54), (MA(I),I=21,40), NSU,
     2                     (LDEMRK,KLINE=1,54)
 280    LN = LINE
        IF (JSPV.EQ.IONE) RETURN
 290  CONTINUE
      LN = LINE
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 300  FORMAT (45X,20HANALYSIS OF VARIANCE/13X,85H-DEPENDENT ON ORDER IND
     1EPENDENT VARIABLES ARE ENTERED, UNLESS VECTORS ARE ORTHOGONAL-)
 310  FORMAT (1H /1X,2A3,2X,21H SS=RED. DUE TO COEF.,5X,
     3      21H   CUM. RESIDUAL MS  ,6H  D.F.,5X,11H  F(COEF=0),3X,6H  P
     4(F),5X,11H F(COEFS=0),3X,6H  P(F))
 320  FORMAT (1X,I4,6X,20A1,5X,20A1,I6,2(2X,F14.3,F9.3))
 330  FORMAT (1X,10HRESIDUAL  ,3X,20A1,25X,I6)
 340  FORMAT (1X,64A1/1X,  10HTOTAL     ,3X,20A1,25X,I6/1X,64A1)
 350  FORMAT(1H /)
 360  FORMAT( 23X,20HANALYSIS OF VARIANCE/
     1 6X,54H-DEPENDENT ON ORDER INDEPENDENT VARIABLES ARE ENTERED,/
     2 18X,30HUNLESS VECTORS ARE ORTHOGONAL-)
 370  FORMAT (1H /
     1 1X,2A3,4X,21H SS=RED. DUE TO COEF.,14X,18HF(COEF=0)     P(F))
 380  FORMAT( 26X,20HANALYSIS OF VARIANCE/
     1 9X,54H-DEPENDENT ON ORDER INDEPENDENT VARIABLES ARE ENTERED,/
     2 21X,30HUNLESS VECTORS ARE ORTHOGONAL-)
 390  FORMAT (1H /
     1 2X,10HINDEP VAR.,
     2 4X,21H SS=RED. DUE TO COEF.,15X,18HF(COEF=0)     P(F))
 400  FORMAT (1H /1X,10HINDEP VAR.,3X,20HSS=RED. DUE TO COEF.,
     1   4X,21H   CUM. RESIDUAL MS  ,6H  D.F.,5X,11H  F(COEF=0),3X,6H  P
     2(F),5X,11H F(COEFS=0),3X,6H  P(F))
 410  FORMAT(1X,I4,6X,20A1,13X,F11.3,F9.3)
 420  FORMAT(1X,I4,6X,20A1,3X,I6,4X,F11.3,F9.3)
 430  FORMAT(1X,2A3,4X,19H   CUM. RESIDUAL MS,6X, 4HD.F.,5X,
     1 19HF(COEFS=0)     P(F))
 440  FORMAT(1X,12A1,4X,20A1,13X,F11.3,F9.3)
 450  FORMAT(1X,12A1,6X,20A1,2X,I6,3X,F11.3,F9.3)
 460  FORMAT(1HO/2X,10HINDEP VAR.,5X,19H   CUM. RESIDUAL MS,7X,4HD.F.,4X
     1,19HF(COEFS=0)     P(F))
 470  FORMAT(1X,12A1,1X,20A1,5X,20A1,I6,2(2X,F14.3,F9.3))
 480  FORMAT(1X,10H RESIDUAL ,6X,20A1,6X,6HD.F. =,I6/
     1 1X,54A1/2X,5HTOTAL,10X,20A1,6X,6HD.F. =,I6/1X,54A1)
 490  FORMAT(1X,10HRESIDUAL  ,20A1,5X,6HD.F. =,I6/
     1 1X,47A1/1X,5HTOTAL,5X,20A1,5X,6HD.F. =,I6/1X,47A1)
 500  FORMAT(1X,10HRESIDUAL  ,20A1,25X,I6/
     1 1X,61A1/1X,5HTOTAL,5X,20A1,25X,I6/1X,61A1)
 510  FORMAT (1X,114A1)
C
C     ==================================================================
C
      END
*OCOEFF
      SUBROUTINE OCOEFF (M,IS,IH,MA,IAD,NSU,SS,LN,ML,NUMIT,DIGITS)
C
C **  NBS OMNITAB 1580 VERSION 6.01  1/ 1/81. OCOEFF V 7.00 11/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1969.
C                   CURRENT VERSION - NOVEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IH(*), JJSUB(1), MA(*)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             DIGITS, SS
      REAL             TEMP(1)
      REAL             ACCRCY, F1
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        IH*3, MA*1
      CHARACTER        LDEMRK*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LDEMRK / '-' /
C
C     ==================================================================
C
      MXLINE = ML
      ISUBSB = IS
      ISUBAD = IAD
      IT = IONE
      IF (L2.EQ.ITHRE) IT = ITHRE
      CALL RFORMT (0,ISIGD,A(1),     RC(1),M,17,NW1,NDEC1,MA,IRF)
      CALL RFORMT (0,ISIGD,A(ISUBSB),RC(1),M,17,NW2,NDEC2,MA,IRF)
      LINE   = LN + 6
      LENINC = IDIV (MXLINE-6,ITEN,IND)
C
C     MAXIMUM NUMBER OF LINES PER PAGE, MULTIPLE OF 10.
C
      LENINC = ITEN * LENINC
      IF (LENINC.EQ.IZERO) LENINC = IFIVE
      NPG    = IDIV (M,LENINC,IND) + IONE
      JSUBB  = IONE
      JSUBSB = ISUBSB
      NBOT   = IONE
      ISWT   = IZERO
      LINC   = IDIV (MXLINE-LINE,ITEN,IND)
      NTOP   = LINC * ITEN
      IF (NTOP.EQ.IZERO) NTOP   = IFIVE
      IF (NTOP.GT.M) NTOP = M
      NTEMP = MOD (M,LENINC)
      IF (NTEMP.GT.LINC*ITEN) NPG = NPG + IONE
      LINE   = LINE + NTOP
      KLNTOP = IZERO
      DO 90 IPAGE=1,NPG
        IF (ISWT.EQ.IZERO) GO TO 10
        CALL PAGE (IFOUR)
        LINE = 6 + LENINC
C
C       WRITE TWO LINES OF BLANK.
C
  10    WRITE (IPRINT,120)
C
C       PRINT TITLE.
C
        ISWT = IONE
        IF (IPAGE.EQ.IONE) WRITE (IPRINT,130)
        IF (L2.EQ.ITHRE) GO TO 20
        WRITE (IPRINT,140) IH(IT), IH(IT+1)
        KLNTOP = 67
        GO TO 30
C
  20    WRITE (IPRINT,150)
        KLNTOP = 71
  30    WRITE (IPRINT,160) (LDEMRK,KLINE=1,KLNTOP)
        DO 80 J=NBOT,NTOP
          F1 = RZERO
          IF (A(JSUBSB).NE.RZERO) GO TO 40
          IF (A(JSUBB).LT.RZERO) F1 = RMIFY
          IF (A(JSUBB).GT.RZERO) F1 = RPIFY
          GO TO 50
  40      F1 = FDIV (A(JSUBB),A(JSUBSB),INDV)
  50      JJ = IABS (J-1)
          IF (L2.EQ.ITHRE) JJ = IARGS(J+3)
          JJSUB(1) = JJ
          IF (L2.EQ.ITHRE) CALL HEADS (JJSUB(1),IONE,IZERO,IONE)
          CALL RFORMT (1,ISIGD,RC,A(JSUBB), 18-NW1,1,NW1,NDEC1,MA,IRF)
          CALL RFORMT (1,ISIGD,RC,A(JSUBSB),18-NW2,1,NW2,NDEC2,MA(19),
     1                  IRF)
          CALL ACCDIG (A(JSUBB),A(ISUBAD),RSD,ACCRCY,IND)
          IF (L2.EQ.ITHRE) GO TO 60
          WRITE (IPRINT,170) JJ, (MA(I1),I1=1,36), F1, ACCRCY
          GO TO 70
  60      WRITE (IPRINT,180) (LHEAD(I1),I1=1,12), (MA(I1),I1=1,36),
     1                          F1, ACCRCY
  70      ISUBAD = ISUBAD + IONE
          JSUBSB = JSUBSB + IONE
          JSUBB  = JSUBB  + IONE
          LINE   = LINE + IONE
  80    CONTINUE
        NBOT = NTOP + IONE
        NTOP = NTOP + LENINC
        IF (NTOP.LE.M) GO TO 90
        NTOP = M
        IF (NBOT.GT.NTOP) GO TO 100
  90  CONTINUE
 100  NSUA = NSU - M
      TEMP(1) = SS
      CALL RFORMT (0,ISIGD,TEMP,A(1),1,17,NW6,NDEC6,MA,IRF)
      CALL RFORMT (1,ISIGD,A,SS,18-NW6,0,NW6,NDEC6,MA,IRF)
      WRITE (IPRINT,160) (LDEMRK,KLINE=1,KLNTOP)
      LINE = LINE + IONE
      IF (LINE+ITWO.LE.MXLINE) GO TO 110
      CALL PAGE (IFOUR)
      LINE = 8
 110  WRITE (IPRINT,190) (MA(I),I=1,18), NSU, M, NSUA
      LINE = LINE + ITHRE
      IF (LWIDE.LT.89)  LINE = LINE + IONE
      IF (LWIDE.LT.LWC) LINE = LINE + IONE
      IF (LINE+IFOUR.GT.MXLINE) CALL PAGE (4)
      IF (LWIDE.GE.LWC) WRITE (IPRINT,200)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,210)
C
C     PRINT NUMBER OF ITERATIONS AND NUMBER OF DIGITS IN AGREEMENT.
C
      WRITE (IPRINT,160)
      NUMITS = - NUMIT
      IF (NUMIT.LT.IZERO) WRITE (IPRINT,220) NUMITS
      IF (NUMIT.GE.IZERO) WRITE (IPRINT,230) NUMIT
      IF (LWIDE.GE.89) WRITE (IPRINT,240) DIGITS
      IF (LWIDE.LT.89) WRITE (IPRINT,250) DIGITS
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 120  FORMAT (1X/1X)
 130  FORMAT (20X,32HESTIMATES FROM LEAST SQUARES FIT//)
 140  FORMAT (1X, 2A3,         4X,11HCOEFFICIENT,5X,14HS.D. OF COEFF.,
     1        8X, 5HRATIO,     5X, 9HACCURACY*)
 150  FORMAT (1X,10HINDEP VAR.,6X,11HCOEFFICIENT,6X,14HS.D. OF COEFF.,
     1        7X, 5HRATIO,     3X, 9HACCURACY*)
 160  FORMAT (1X,71A1)
 170  FORMAT (1X,I4,2X, 36A1,2X,F9.2,6X,F5.2,8X,36A1,3X,F9.2)
 180  FORMAT (1X,12A1,1X,36A1,1X,F9.2,4X,F5.2)
 190  FORMAT ( 2X,30HRESIDUAL STANDARD DEVIATION = ,3X,18A1/4X,
     1   28HBASED ON DEGREES OF FREEDOM ,9X,I4,3H - ,I3,3H = ,I3/)
 200  FORMAT (1X,119H*THE NUMBER OF CORRECTLY COMPUTED DIGITS IN EACH CO
     1EFFICIENT USUALLY DIFFERS BY LESS THAN 1 FROM THE NUMBER GIVEN HER
     2E.)
 210  FORMAT (1X,60H*THE NUMBER OF CORRECTLY COMPUTED DIGITS IN EACH COE
     1FFICIENT/4X,58HUSUALLY DIFFERS BY LESS THAN 1 FROM THE NUMBER GIVE
     2N HERE.)
 220  FORMAT (1X,29HTHE NUMBER OF ITERATIONS WAS ,I1,24H.  SCALING WAS N
     1OT USED.)
 230  FORMAT (1X,29HTHE NUMBER OF ITERATIONS WAS ,I1,20H.  SCALING WAS U
     1SED.)
 240  FORMAT (1X,88HTHE AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN IN
     1ITIAL SOLUTION AND 1ST ITERATION IS ,F6.2,1H.)
 250  FORMAT (1X,70HTHE AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN IN
     1ITIAL SOLUTION AND/4X,17H1ST ITERATION IS ,F6.2,1H.)
C
C     ==================================================================
C
      END
*OCOVAR
      SUBROUTINE OCOVAR (M,ISUBQ,MD1,IHC,MA,IHT,MCNT,MXLINE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OCOVAR V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT VARIANCE-COVARIANCE MATRIX.
C        PRINT CORRELATIONS ABOVE THE DIAGONAL.
C
C     THE LOCATION OF THE (ITH,JTH) ELEMENT OF THE VARIANCE-COVARIANCE
C        MATRIX IS STORED IN A(ISUBQ-1+L), FOR I GREATER THAN J, WHERE
C           L = I + (J-1)(2M-J)/2.
C
C     K  = INDEX FOR BLOCKS OF PRINTING.
C     I  = INDEX FOR ROWS.
C     J  = INDEX FOR COLUMNS.
C
C     KI = INDEX FOR PRINTING COLUMNS NUMBERS FOR I.
C     KJ = INDEX FOR PRINTING COLUMN NUMBERS FOR J.
C     KM = INDEX FOR PRINTING MA(.).
C     KH = INDEX FOR PRINTING IHT(.).
C     KL = SWITCH FOR L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   OCTOBER, 1969.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IHC(*), IHT(*), MA(*)
C
      COMMON /ABCDEF/ LA(74)
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
C                    ***   SPECIFICATION STATEMENTS   ***
C
C     REAL             CORREL, DENII, DENOMA
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        IHC*3, MA*1
C 
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MCOL /  7 /
      DATA NX   / 15 /
C
      DATA SPCA / 0.1234 /
C
C     ==================================================================
C
C     INITIALIZATION.
C
      JSUBQ = ISUBQ
      NX2   = NX - ITWO
C
C     SET UP RFORMT CONSTANTS.
C
      CORREL     = A(JSUBQ-1)
      A(JSUBQ-1) = SPCA + SPCA * RTEN ** (ISIGD-IFOUR)
      CALL RFORMT (0,ISIGD,A(JSUBQ-1),RC(1),MD1+1,NX2,NWC,NDC,MA(1),IRF)
      A(JSUBQ-1) = CORREL
C
      NB  = NX - NWC
      NWR = NWC - NDC + IFOUR
      IF (NWR.LT.IFOUR) NWR = NWC
      ICOL = MCOL
      IF (LWIDE.LT.LWC) ICOL = IFOUR
      LINE = MCNT
      KL   = -IONE
      IF (L2.EQ.ITHRE) KL =  ITHRE
      KJBEG = KL + IONE
      JBEG = IONE
      MONE = M + IONE
      MTWO = ITWO * M
C
C     KEND = NUMBER OF BLOCKS OF PRINTING.
C
      KEND = IONE + IDIV (M-IONE,ICOL,IND)
      DO 90 K=1,KEND
        KJEND = MIN0 (M+KL,KJBEG+ICOL-IONE)
        WRITE (IPRINT,140)
        LINE = LINE + ITWO
C
        IF (K.GT.IONE) GO TO 10
          WRITE (IPRINT,140)
        IF (LWIDE.GE.LWC) WRITE (IPRINT,100)
        IF (LWIDE.LT.LWC) WRITE (IPRINT,110)
        LINE = LINE + IONE
        IF (LWIDE.LT.LWC) LINE = LINE + IONE
  10    IF (L2.GT.ITWO) GO TO 30
        KHEND = KJEND + IONE - KJBEG
        DO 20 KH=1,KHEND
          IHT(KH) = KJBEG - IONE + KH
  20    CONTINUE
        WRITE (IPRINT,120) IHC(L2), IHC(L2+1), (IHT(KH),KH=1,KHEND)
        GO TO 40
C
  30    WRITE (IPRINT,120) IHC(L2),IHC(L2+1),
     1                    (IARGS(KJ),KJ=KJBEG,KJEND)
C
  40    WRITE (IPRINT,130)
        LINE = LINE + ITHRE
C
C       I IS FOR LOOP ON ROWS.
C
        LOCII  = JSUBQ
        INITIJ = JSUBQ + IDIV ((JBEG-IONE)*(MTWO-JBEG),ITWO,IND)
        INITJI = JSUBQ + JBEG - IONE
        INITJJ = INITIJ + JBEG - IONE
        JEND = MIN0 (JBEG+ICOL-IONE,M)
C
        DO 80 I=1,M
          LOCIJ = INITIJ
          LOCJI = INITJI
          LOCJJ = INITJJ
          DENII = RZERO
          IF (A(LOCII).GT.RZERO) DENII = FSQRT ( A(LOCII) )
          L  = IONE
          KW = 106
          CALL RFORMT (11,ISIGD,RC,A(1),0,0,KW,NDC,MA,IRF)
C
C         CLEAR MA(.) BECAUSE FIXED FORMAT USED.
C
          CALL PUTCH (LA(45),120,MA)
C
C         J IS FOR LOOP ON COLUMNS.
C
          DO 70 J=JBEG,JEND
            IF (J.GT.I) GO TO 50
C
C           CALL RFORMT FOR VARIANCE-COVARIANCE ELEMENT.
C
            CALL RFORMT (1,ISIGD,RC,A(LOCIJ),NB,0,NWC,NDC,MA(L),IRF)
            GO TO 60
C
C           COMPUTE CORRELATION.
C
  50        DENOMA = RZERO
            IF (A(LOCJJ).GT.RZERO) DENOMA = DENII * FSQRT ( A(LOCJJ) )
            CORREL = FDIV (A(LOCJI),DENOMA,IND)
            CALL RFORMT (7,ISIGD,A,CORREL,NB,0,NWR,IFOUR,MA(L),IRF)
  60        LOCIJ = LOCIJ + M - J
            LOCJI = LOCJI + IONE
            LOCJJ = LOCJJ + (MONE-J)
            L     = L + NX
  70      CONTINUE
C
          KMEND = NX * (JEND - JBEG + IONE)
          KI    = I - IONE
          IF (L2.EQ.ITHRE) KI = IARGS(I+3)
          WRITE (IPRINT,130) KI, (MA(KM),KM=1,KMEND)
          LINE   = LINE + IONE
          LOCII  = LOCII + (MONE-I)
          INITIJ = INITIJ + IONE
          INITJI = INITJI + (M - I)
          IF (LINE.LT.MXLINE) GO TO 80
            CALL PAGE (4)
            LINE = ITHRE
  80    CONTINUE
        JBEG = JBEG + ICOL
        KJBEG = KJBEG + ICOL
        IF (LINE+7.LT.MXLINE) GO TO 90
          CALL PAGE (4)
          LINE = ITHRE
  90  CONTINUE
      MCNT = LINE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 100  FORMAT (10X,101HSAMPLE VARIANCE-COVARIANCE MATRIX OF THE ESTIMATED
     1 COEFFICIENTS WITH CORRELATIONS ABOVE THE DIAGONAL.)
 110  FORMAT (5X,63HSAMPLE VARIANCE-COVARIANCE MATRIX OF THE ESTIMATED C
     1OEFFICIENTS/18X,37HWITH CORRELATIONS ABOVE THE DIAGONAL.)
 120  FORMAT (/1X,2A3,1X,7(6X,I5,4X))
 130  FORMAT (1X,I4,3X,106A1)
 140  FORMAT (3H   )
C
C     ==================================================================
C
      END
*OMNIT
      SUBROUTINE OMNIT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  OMNIT V 7.00  7/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     *********   THIS IS THE PRIMARY OMNITAB PROGRAM UNIT   ***********
C
C     MODE = 1,  MEANS OMNITAB IS IN INTERPRETIVE MODE
C          = 2,  DATA MODE (READ, SET)
C          = 3,  STORAGE MODE BETWEEN BEGIN AND FINISH
C          = 4,  IMPLIED STORAGE MODE (STATEMENT NUMBER GIVEN).
C
C               REVISED BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - JANUARY, 1968.
C                   CURRENT VERSION -    july, 1992.
C
C     ..................................................................
C
C                         ***   STRUCTURE CHART   ***
C
C                           WRITTEN BY DAVID HOGBEN
C
C     NOTES ...
C
C        (1)   FUNCTIONS SUCH AS FDIV AND IDIV ARE NOT INCLUDED.
C        (2)   IF A PROGRAM UNIT IS SHOWN MORE THAN ONCE, THE STRUCTURE
C                 FOR ANY UNIT ON THE RIGHT IS NOT SHOWN. FOR EXAMPLE,
C                 SPINST CALLS ERROR, BUT THE PROGRAM UNITS CALLED BY
C                 ERROR ARE NOT SHOWN HERE, BECAUSE THEY ARE SHOWN
C                 ELSEWHERE.
C        (3)   MOST OMNITAB INSTRUCTIONS ARE EXECUTED BY PROGRAM UNITS
C                 CALLED BY XCUTEA AND XCUTEB.  XCUTEB IS CALLED BY
C                 XCUTEA.  ALL THE PROGRAM UNITS CALLED BY XCUTEA ARE
C                 SYMBOLIZED BY * A *.  ALL THE PROGRAM UNITS CALLED BY
C                 XCUTEB ARE SYMBOLIZED BY * B *.
C
C     ..........     ..........     ..........     ..........
C     . OMNIT  ....... ERROR  ....... ERRPRT ....... INFERR .
C     ..........  .  ..........  .  ..........  .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... RTHERR .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... FTLERR .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... CALERR .
C                 .              .                 ..........
C                 .              .
C                 .              .  ..........
C                 .              .... RNDOWN .
C                 .                 ..........
C                 .
C                 .  ..........     ..........
C                 .... INPUT  ....... OMCONV .
C                 .  ..........     ..........
C                 .
C                 .  ..........
C                 .... OUTPUT .
C                 .  ..........
C                 .
C                 .... STMT   .
C                 .  ..........
C                 .
C                 .  ..........
C                 .... NNAME  .
C                 .  ..........
C                 .
C                 .  ..........     ..........
C                 .... SPINST ....... ERROR  .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... SETUP  ....... XSTOP  .
C                 .              .  ..........     ..........
C                 .              .
C                 .              .  ..........
C                 .              .... OUTPUT .
C                 .              .  ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... XFORMT ....... PCKFMT .
C                 .              .  ..........  .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... ERROR  .
C                 .              .                 ..........
C                 .              .
C                 .                 ..........
C                 .              .... PAGE   .
C                 .              .  ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... XHEAD  ....... AARGS  .
C                 .              .  ..........  .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... ERROR  .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .  .......... ..........
C                 .              .              .... PREPAK ... PACK   .
C                 .              .                 .......... ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... XSTOP  ....... PAGE   .
C                 .                 ..........  .  ..........
C                 .                             .
C                 .                             .  ..........
C                 .                             .... ERRPRT .
C                 .                                ..........
C                 .
C                 .  ..........
C                 .... LOOKUP .
C                 .  ..........
C                 .
C                 .  ..........     .......... 
C                 .... SCNARG ....... BLANK  .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... NONBLA .
C                 .              .  ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... ZLCVAR ....... PACK   .
C                 .              .  ..........     ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... AARGS  ....... ERROR  .
C                 .              .  ..........     ..........
C                 .              .
C                 .              .  ..........     ..........
C                 .              .... ASTER  ....... NONBLA .
C                 .              .  ..........  .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... AARGS  .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... NNAME  .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .... PHYCON .
C                 .              .              .  ..........
C                 .              .              .
C                 .              .              .  ..........
C                 .              .              .... VARCON .
C                 .              .                 ..........
C                 .              .
C                 .              .  ..........
C                 .              .... ERROR  .
C                 .                 ..........
C                 .
C                 .  ..........     ..........
C                 .... EXPAND ....... ERROR  .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .... XPND   ....... ADRESS .
C                 .                 ..........     ..........
C                 .
C                 .  ..........     ..........
C                 .... REDATA ....... ERROR  .
C                 .  ..........     ..........
C                 .
C                 .... STORE  ....... LOCATE .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... ERROR  .
C                 .                 ..........
C                 .              .
C                 .              .  ..........
C                 .              .... REPCHK .
C                 .                 ..........
C                 .
C                 .  ..........     ..........
C                 .... XCUTEA ....... * A *  .
C                 .  ..........  .  ..........
C                                .
C                                .  ..........     ..........
C                                .... XCUTEB ....... * B *  .
C                                   ..........     ..........
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IMP(4)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REDSET/ IFLAG, ISRFLG, JY, NDROW, NNARG
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
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
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LETSGO / -1 /
C
C     ==================================================================
C
      OPEN (UNIT=ISCRT, STATUS='SCRATCH')
      REWIND ISCRT
c
c     the following open statement shold be commented out if not using 
c     a convex machine
c
       open (nprnt,form='print')
C
C     SAVE INUNIT VALUE
C
      IFILE = IONE
      NUNIT(IFILE) = INUNIT
      ISFILE = IZERO
C
C     ------------------------------------------------------------------
C
      INT = -IONE
      CALL SPINST (LETSGO,INT,INST)
  10  IF (MODE.EQ.ITHRE) NSTMT = NSTMT + ITEN
      IF (NDEMD.NE.IZERO .AND. NERROR.NE.IZERO) NSTMT = NSTMT - ITEN
      IF (MODE.EQ.IFOUR) MODE = IONE
      IF (INUNIT.EQ.5) NERROR = IZERO
      DO 20 I=1,8
        NAME(I) = IZERO
  20  CONTINUE
C
      DO 30 I=1,4
        IMP(I) = IZERO
  30  CONTINUE
C
      NARGS  = IZERO
      KARG   = IZERO
      MATSWT = IZERO
      LNAME  = IZERO
      IMAT   = IZERO
      JA     = IZERO
C
C     CHECK FOR ACCUMULATED ERRORS DURING LAST EXECUTED COMMAND.
C
      CALL ERROR (0)
  40  CALL INPUT
C
C     IF ISFILE IS ONE, ALL FILES OPENED ARE AT THE END.
C
      IF (ISFILE.NE.IZERO) RETURN
C
C     SCANNING BEGINS WITH THE THIRD CHARACTER. THE FIRST TWO ARE DUMMY
C     TO KEEP THE PROGRAM OUT OF TROUBLE.  SCANNING TERMINATES WITH A $
C     A $ HAS BEEN PLANTED IN THE (KRDEND+1)-TH POSITION.
C
      KRDPOS = ITWO
  50  KRDPOS = KRDPOS + IONE
      K = KARD(KRDPOS)
      IF (K.LT.ITEN) GO TO 60
      IF (K.LT.36) GO TO 120
      IF (K.GT.46) GO TO 70
      IF (K.EQ.46) GO TO 70
      IF (K.EQ.40) GO TO 180
      GO TO 50
C
C     A NUMBER IS THE FIRST ALPHANUMERIC CHARACTER ENCOUNTERED, ERROR IF
C        IN MODE 3.
C
  60  CALL OUTPUT
      IF (MODE.NE.ITHRE) GO TO 80
      CALL ERROR (6)
      GO TO 40
C
  70  IF (MODE.NE.IFOUR) CALL OUTPUT
      GO TO 10
C
C     CHECK FOR  * OR '
C
  80  IF (LETSGO.NE.(-IONE)) GO TO 90
      CALL ERROR (4)
      GO TO 10
C
C     ..................................................................
C
  90  CALL STMT (NSTMT)
      IF (KARG.EQ.IZERO) GO TO 100
      IF (MODE.EQ.ITWO) GO TO 170
      CALL ERROR (2)
      GO TO 40
C
C     IF AN ILLEGAL STATEMENT NUMBER WAS FOUND, KARG = 1.
C        (KARG = 0, IF LEGAL)
C
 100  IF (MODE.NE.ITWO) GO TO 110
      IF (LLIST.NE.IZERO) BACKSPACE ISCRT
      CALL ERROR (252)
      CALL OUTPUT
 110  MODE = IFOUR
C
C     M IS POINTING AT THE FIRST LETTER ON THE CARD, ASSEMBLE NAME.
C
 120  CALL NNAME (NAME(1))
      IMP(1) = KRDPOS + IONE
      IF (KARD(KRDPOS).LT.ITEN) IMP(1) = KRDPOS
C
C     CHECK THE FIRST NAME FOR SPECIAL NAMES...
C        OMNITAB, FORMAT, NOTE1, NOTE2, NOTE, HEAD, TITLES.
C
      INT = IZERO
      CALL SPINST (LETSGO,INT,INST)
C
C     UPON RETURN, IF INST
C       = 0, INSTRUCTION IS ONE OF THE SPECIAL COMMANDS. READ NEXT CARD.
C       = 1, INSTRUCTION IS NOT A SPECIAL COMMAND AND CONTINUE.
C       = 2, INSTRUCTION IS STOP. TERMINATE RUN.
C       = 3, INSTRUCTION IS OMNITAB. CALL LANGUA VIA XCUTEA.
C       = 4, IF FIRST INSTRUCTION IS NOT OMNITAB.
C
      IF (INST.EQ.IZERO .OR. INST.EQ.IFOUR) GO TO 10
      IF (INST.EQ.ITWO) RETURN
      IF (INST.EQ.ITHRE) GO TO 270
C
C     KRDPOS IS POINTING AT THE FIRST NON-LETTER AFTER NAME. LOOK FOR
C        POSSIBLE NAME QUALIFIER OR ARGUMENTS OR END OF CARD.
C
      DO 150 I=3,8,2
 130    K = KARD(KRDPOS)
        IF (K.LT.ITEN) GO TO 160
        IF (K.LT.36) GO TO 140
        IF (K.EQ.40 .OR. K.EQ.46) GO TO 160
        KRDPOS = KRDPOS + IONE
        GO TO 130
C
C     A LETTER FOUND, ASSEMBLE SECOND NAME (COMMAND QUALIFIER).
C
 140    CALL NNAME (NAME(I))
        II  = I
        IJM = IDIV (II,ITWO,IND) + IONE
        IMP(IJM) = KRDPOS
 150  CONTINUE
C
 160  CALL LOOKUP (LNAME)
      IF (LNAME.EQ.IFIVE) GO TO 190
      IF (LNAME.GT.IZERO) KRDPOS = IMP(LNAME)
      IF (KARD(KRDPOS).EQ.46) GO TO 190
      IF (KARD(KRDPOS-1).EQ.44) KRDPOS = KRDPOS - IONE
      GO TO 180
C
C     SCAN FOR ARGUMENTS AND END OF CARD
C
 170  KRDPOS = ITHRE
C
  180 CALL SCNARG (LNAME,JA,MATSWT,IMAT,INDX)
C
C     IF INDX = 1 READ IN NEXT CARD, OTERWISE CONTINUE.
C
      IF (INDX.EQ.IONE) GO TO 10
 190  IF (JA.EQ.IZERO) JA = IONE
      IF (MODE.NE.ITWO .OR. NAME(1).NE.IZERO) GO TO 210
C
C     IN INPUT MODE AND NO POSSIBLE NAME, RETURN TO SET OR READ ROUTINE
C
 200  CALL EXPAND (JA,ARGTAB)
C
      CALL REDATA
C
      GO TO 10
C
C     LOOK UP NAME (AND POSSIBLE QUALIFIER) IN DICTIONARY. RETURN
C        COORDINATES OF ENTRY. IF L1 = 0, NAME NOT FOUND.
C
 210  IF (L1.NE.IZERO) GO TO 220
      IF (MODE.EQ.ITWO) GO TO 200
      CALL ERROR (IONE)
      GO TO 10
C
C     NAME FOUND
C
 220  IF (MODE.NE.ITWO) GO TO 230
      IF (LLIST.NE.IZERO) BACKSPACE ISCRT
      IF (NERROR.EQ.IZERO) CALL ERROR (252)
C
      CALL OUTPUT
C
      MODE = IONE
C
 230  IF (MODE.EQ.IONE) GO TO 240
C
C     CHECK TO SEE IF INSTRUCTION CAN BE STORED.
C
      LSWTCH = IHRD * L1 + L2
      CALL REPCHK (LSWTCH,LIND)
C
C     IF LIND = 1, INSTRUCTION CAN NOT BE STORED.
C
      IF (LIND.EQ.IONE) GO TO 10
      CALL STORE (JA)
C
      GO TO 10
C
 240  CALL EXPAND (JA,ARGTAB)
C
      IF (MATSWT.EQ.IZERO) GO TO 270
      IF (IMAT.EQ.IONE) GO TO 270
      NARGS = NARGS - ITWO
      IF (L1.EQ.22 .AND. L2.EQ.IFIVE) GO TO 260
      IF (L1.EQ.17 .AND. L2.EQ.IFOUR) GO TO 260
      IF (L1.EQ.27 .AND. L2.LE.IFOUR) GO TO 260
      GO TO 270
C
  260 IF (IMAT.EQ.ITWO) GO TO 270
C
C     COMMAND IS EITHER MORTHO, MTRIAN, (S)APROPE OR (S)MPROPE.
C       MOVE THE LAST TWO ARGUMENTS IN IARGS AND KIND TO THE
C       THIRD AND FOURTH ARGUMENT FROM THE END.
C
      IARGS(NARGS-2) = IARGS(NARGS)
      IARGS(NARGS-3) = IARGS(NARGS-1)
      KIND( NARGS-2) = KIND(NARGS)
      KIND( NARGS-3) = KIND(NARGS-1)
      NARGS          = NARGS - ITWO
C
C     ..................................................................
C
 270  CALL XCUTEA
      GO TO 10
C
C     ==================================================================
C
      END
*ONEWAY
      SUBROUTINE ONEWAY
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ONEWAY V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ONEWAY STATISTICAL ANALYSIS INSTRUCTIONS.
C
C     ONEWAY ANALYSIS OF COLUMN (C) WITH GROUP NUMBERS IN COLUMN (C)
C     ONEWAY ANAL. FOR (C) GROUP NO (C) PUT STATS IN (C) AND NEXT 3 COLS
C     ONEWAY ANAL. (C) GROUP (C) PUT GROUP (C) NO. (C) MEAN (C) S.D. (C)
C     SONEWAY DATA (C) GROUP NO (C) STORE (C)
C     SONEWAY DATA (C) GROUP (C) STORE GROUP (C NO (C) MEAN (C) S.D. (C)
C
C     TAG NUMBERS DIFFERENTIATE BETWEEN GROUPS
C        WHEN TAG IS ZERO, ZERO WEIGHT IS GIVEN TO MEASUREMENTS
C
C     NUMBER OF GROUPS MUST BE GREATER THAN 1
C        AND MUST NOT EXCEED NLNTH2 = NLNTH1/5 = NS/25
C
C     NRMAX MUST NOT EXCEED NLNTH1 = NS/5
C
C     SLOPE IN ANOVA IS ONLY GIVEN IF FPROB FOR BETWEEN IS LESS THAN .1
C
C     EXECUTION TIME CAN BE CONSIDERABLY SHORTENED USING LESS ACCURATE
C        VERSION OF FPPT.
C
C
C     STRUCTURE CHART.
C
C        ..........         ..........
C        . ONEWAY ........... RANKO  .
C        ..........    .    ..........
C                      .
C                      .    ..........
C                      ...... TPCTPT .
C                      .    ..........
C                      .
C                      .    ..........
C                      ...... FPPT   .
C                      .    ..........
C                      .
C                      .    ..........
C                      ...... OWPRAV .
C                      .    ..........
C                      .
C                      .    ..........
C                      ...... OWPRES .
C                      .    ..........
C                      .
C                      .    ..........     ..........
C                      ...... OWPRCL ....... TPCTPT .
C                      .    ..........     ..........
C                      .
C                      ...... OWPRMC ....... SRPPT5 .
C                      .    ..........     ..........
C                      .
C                      .    ..........
C                      ...... OWPRHV .
C                      .    ..........
C                      .
C                      .    ..........        ..........
C                      ...... OWPRBP .......... PLTPOS .
C                      .    ..........   .    ..........
C                      .                 .
C                      .    ..........   .    ..........
C                      ...... OWSTRE .   ...... BLNKIN .
C                           ..........   .    ..........
C                                        .
C                                        .    ..........
C                                        ...... OSCRWL .
C                                        .    ..........
C                                        .
C                                        .    ..........    ..........
C                                        ...... BOXPLT ...... BLNKIN .
C                                             ..........    ..........
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1969.
C                   CURRENT VERSION -   APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(120), NB(6), ND(6), NW(6)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             ATEMP(1)
      REAL             BTEMP, DELTA
      REAL             FDIV, FLOG, FSQRT
      REAL             ALPHA, SPCA, SPCB, SPCC
C
C     ...................................................................
C
      CHARACTER         MA*1
C
C     ...................................................................
C
C     NLNTH1 = LENGTH OF ARRAYS A(.).
C
C     NLNTH2 = LENGTH OF ARRAYS B(.).
C
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ALPHA / 0.05   /
      DATA DELTA / 0.0001 /
C
      DATA SPCA /  0.4 /
      DATA SPCB /  6.0 /
      DATA SPCC / 12.0 /
C
C     ==================================================================
C
      NLNTH1 = IDIV (NS,IFIVE,IND)
      NLNTH2 = IDIV (NLNTH1,IFIVE,IND)
C
C     ERROR CHECKING
C
      IF (NRMAX.LE.NLNTH1) GO TO 20
        CALL ERROR (23)
        RETURN
C
C     ..................................................................
C
  20  IF (NRMAX.GT.IZERO) GO TO 30
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
  30  IF (NARGS.EQ.6) GO TO 70
      IF (NARGS.EQ.ITWO .AND. L2.EQ.13) GO TO 70
      IF (NARGS.EQ.ITWO .AND. L2.EQ.14) GO TO 40
      IF (NARGS.EQ.ITHRE) GO TO 50
        CALL ERROR (10)
        RETURN
C
C     ..................................................................
C
  40    CALL ERROR (236)
      RETURN
C
C     ..................................................................
C
  50  DO 60 I=4,6
        IARGS(I) = IARGS(I-1) + IONE
        KIND(I) = IZERO
  60  CONTINUE
      NARGS = 6
  70  CALL HEADS (IARGS(1),ITWO,IZERO,IONE)
      CALL CHKCOL
      GO TO 100
  80    CALL ERROR (28)
        RETURN
C
C     ..................................................................
C
  90    CALL ERROR (35)
        RETURN
C
C     ..................................................................
C
 100  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     START COMPUTING.
C
C                     ***   STORAGE   ***
C
C         (A)     (B)           (C)                      (D)
C
C     A1        SORTED X     SORTED X                  B1 TO B5
C     A2   X       X         HIERARCHY                 B6 TO B10
C     A3       HIERARCHY    SORT X WITHIN SORT T    SORT X WITHIN SORT T
C     A4   T       T         SORTED T                  SORTED T
C     A5          RANK         RANK                      RANK
C
C     A.   MOVE Y AND TAG TO SCRATCH AREA, IF TAG IS NOT EQUAL TO ZERO.
C
      NSUBA1 = IONE
      NSUBA2 = NSUBA1 + NLNTH1
      NSUBA3 = NSUBA2 + NLNTH1
      NSUBA4 = NSUBA3 + NLNTH1
      NSUBA5 = NSUBA4 + NLNTH1
      NSUBB1 = IONE
      NSUBB2 = NSUBB1 + NLNTH2
      NSUBB3 = NSUBB2 + NLNTH2
      NSUBB4 = NSUBB3 + NLNTH2
      NSUBB5 = NSUBB4 + NLNTH2
      NSUBB6 = NSUBB5 + NLNTH2
      NSUBB7 = NSUBB6 + NLNTH2
      NSUBB8 = NSUBB7 + NLNTH2
      NSUBB9 = NSUBB8 + NLNTH2
      NSUBBT = NSUBB9 + NLNTH2
      IXSUB  = IARGS(1) - IONE
      ITSUB  = IARGS(2) - IONE
C     NZERO  = IZERO
      ISUBA2 = NSUBA2
      ISUBA4 = NSUBA4
      ITOTAL = IZERO
      DO 110 I=1,NRMAX
        IXSUB      = IXSUB + IONE
        ITSUB      = ITSUB + IONE
        A(ISUBA2)  = RC(IXSUB)
        A(ISUBA4)  = RC(ITSUB)
        IF (A(ISUBA2).EQ.RZERO) ITOTAL = ITOTAL + IONE
        ISUBA2     = ISUBA2 + IONE
        ISUBA4     = ISUBA4 + IONE
 110  CONTINUE
      IF (ITOTAL.NE.NRMAX) GO TO 115
      CALL ERROR (265)
      RETURN
C
C     B.   COMPUTE RANKS
C
 115  CALL RANKO (NRMAX,A(NSUBA2),A(NSUBA1),A(NSUBA3),A(NSUBA5),
     1            ATEMP(1))
C
      ISUBA1 = NSUBA1
      ISUBA3 = NSUBA3
      DO 120 I=1,NRMAX
        L          = A(ISUBA3) + DELTA
        L          = L + NSUBA2 - IONE
        A(ISUBA1) = A(L)
        ISUBA1     = ISUBA1 + IONE
        ISUBA3     = ISUBA3 + IONE
 120  CONTINUE
C
      ISUBA2 = NSUBA2
      ISUBA4 = NSUBA4
      DO 130 I=1,NRMAX
        A(ISUBA2)  = A(ISUBA4)
        ISUBA2     = ISUBA2 + IONE
        ISUBA4     = ISUBA4 + IONE
 130  CONTINUE
C
      ISUBA3 = NSUBA3
      ISUBA4 = NSUBA4
      DO 140 I=1,NRMAX
        L          = A(ISUBA3) + DELTA
        L          = L + NSUBA2 - IONE
        A(ISUBA4) = A(L)
        ISUBA3     = ISUBA3 + IONE
        ISUBA4     = ISUBA4 + IONE
 140  CONTINUE
C
      ISUBA2 = NSUBA2
      ISUBA5 = NSUBA5
      DO 150 I=1,NRMAX
        A(ISUBA2)  = A(ISUBA5)
        ISUBA2     = ISUBA2 + IONE
        ISUBA5     = ISUBA5 + IONE
 150  CONTINUE
C
      ISUBA3 = NSUBA3
      ISUBA5 = NSUBA5
      DO 160 I=1,NRMAX
        L          = A(ISUBA3) + DELTA
        L          = L + NSUBA2 - IONE
        A(ISUBA5)  = A(L)
        ISUBA3     = ISUBA3 + IONE
        ISUBA5     = ISUBA5 + IONE
 160  CONTINUE
C
C     C.   SORT TAG IN A4() AND THEN SORT X WITHIN T
C
      CALL SORT (A(NSUBA4),A(NSUBA2),NRMAX,1)
C
      ISUBA2 = NSUBA2
      ISUBA3 = NSUBA3
      DO 170 I=1,NRMAX
        L          = A(ISUBA2) + DELTA
        A(ISUBA3)  = A(L)
        ISUBA2     = ISUBA2 + IONE
        ISUBA3     = ISUBA3 + IONE
 170  CONTINUE
C
      ISUBA1 = NSUBA1
      ISUBA5 = NSUBA5
      DO 180 I=1,NRMAX
        A(ISUBA1)  = A(ISUBA5)
        ISUBA1     = ISUBA1 + IONE
        ISUBA5     = ISUBA5 + IONE
 180  CONTINUE
C
      ISUBA2 = NSUBA2
      ISUBA5 = NSUBA5
      DO 190 I=1,NRMAX
        L          = A(ISUBA2) + DELTA
        A(ISUBA5)  = A(L)
        ISUBA2     = ISUBA2 + IONE
        ISUBA5     = ISUBA5 + IONE
 190  CONTINUE
C
C     COMPUTE K = NUMBER OF GROUPS.
      K      = NRMAX
      ISUBA4 = NSUBA4 + IONE
      DO 220 I=2,NRMAX
        IF (A(ISUBA4)-A(ISUBA4-1)) 210,200,210
 200    K      = K - IONE
 210    ISUBA4 = ISUBA4 + IONE
 220     CONTINUE
C
      IF (NRMAX.LE.K) GO TO 80
      IF (K.LT.ITWO)  GO TO 90
      IF (K.GT.NLNTH2) GO TO 30
C
C     COMPUTE NI,MEAN,S(R),SETUP MIN + MAX,IBAR, FOR I = 1,K
C
      IEND = ITWO * NLNTH1
      DO 230 I=1,IEND
        A(I) = RZERO
  230  CONTINUE
      A(49) = SPCC * ATEMP(1)
      A(56) = NRMAX
C
C                 ***   STORAGE FOR I = 1, 2, ... , K   ***
C
C     B1 = STATISTICS
C     B2 = T(I)            B3 = N(I)                B4 = XBAR(I)
C     B5 = SD(I)           B6 = S(R)                B7 = MINIMUM
C     B8 = MAXIMUM         B9 = ORDERED XBAR(I)    B10 = N(I) FOR B9
C
C     IN SECOND SECTION OF PRINTING OF ESTIMATES
C          S(R) IN B6 IS REPLACED BY SDXBAR(I)
C       MINIMUM IN B7 IS REPLACED BY LOWER CONFIDENCE LIMIT
C       MAXIMUM IN B8 IS REPLACED BY UPPER CONFIDENCE LIMIT
C
C     IN PRINTING OF TESTS FOR HOMOGENEITY OF VARIANCES -
C       N(I)   IN B10 IS REPLACED BY W(I)
C
C     B8 = ORDERED XBAR,   B9 = N CARRIED ALONG,   B10 = W.
C
C     STORAGE IN A(I), I=1,92  (COMMENTS NOT CAREFULLY EDITED)
C
C     A( 1) = BETWEEN SS
C     A( 2) = SLOPE SS
C     A( 3) = DEVIATIONS ABOUT LINE SS
C     A( 4) = WITHIN SS
C     A( 5) = TOTAL SS
C     A( 6) = BETWEEN MS
C     A( 7) = SLOPE MS
C     A( 8) = DEVIATIONS ABOUT LINE MS
C     A( 9) = WITHIN MS
C     A(10) = TOTAL MS
C     A(11) = BETWEEN GROUPS F RATIO
C     A(12) = SLOPE F RATIO
C     A(13) = DEVIATIONS ABOUT LINE F RATIO
C     A(14) = BETWEEN GROUPS F SIGNIFICANCE LEVEL
C     A(15) = SLOPE F SIGNIFICANCE LEVEL
C     A(16) = DEVIATIONS ABOUT LINE F SIGNIFICANCE LEVEL
C     A(17) = KRUSKAL-WALLIS H STATISTIC
C     A(18) = SUM (1/N(I))
C     A(19) = F STATISTIC ASSOCIATED WITH KRUSKAL-WALLIS H STATISTIC
C     A(20) = SIGNIFICANCE LEVEL OF KRUSKAL-WALLIS F (OR H)
C     A(21) = GRAND MEAN
C     A(22) = MINIMUM XBAR(I)
C     A(23) = MAXIMUM XBAR(I)
C     A(24) = MIMIMUM S(I)
C     A(25) = MAXIMUM S(I)
C     A(26) = MINIMUM X(I)
C     A(27) = MAXIMUM X(I)
C     A(28) = MIDRANGE
C     A(29) = SMALLEST MAXIMUM
C     A(30) = MAXIMUM N(I)
C     A(31) = SQRT WITHIN MS
C     A(32) = SQRT SUM OF X - XBAR SQUARED
C     A(33) = SQRT TOTAL MS
C     A(34) = SQRT WITHIN MS / NRMAX
C     A(35) = SQRT BETWEEN MS / NRMAX
C     A(36) = SQRT TOTAL MS / NRMAX
C     A(37) = T(.05,NRMAX-K)
C     A(38) = T(.05,K-1)
C     A(39) = T(.05,NRMAX-1)
C     A(40) = MINIMUM S.D. OF XBAR
C     A(41) = FIXED MODEL LOWER CONFIDENCE LIMIT FOR MEAN
C     A(42) = RANDOM MODEL LOWER CONFIDENCE LIMIT FOR MEAN
C     A(43) = UNGROUPED MODEL LOWER CONFIDENCE LIMIT FOR MEAN
C     A(44) = FIXED MODEL UPPER CONFIDENCE LIMIT FOR MEAN
C     A(45) = RANDOM MODEL UPPER CONFIDENCE LIMIT FOR MEAN
C     A(46) = UNGROUPED MODEL UPPER CONFIDENCE LIMIT FOR MEAN
C     A(47) = OMEGA HAT SQUARED
C     A(48) = SUM OF X - XBAR SQUARED
C     A(49) = T
C     A(50) = MAXIMUM S.D. OF XBAR
C     A(51) = COCHRANS C
C     A(52) = SIGNIFCANCE LEVEL FOR COCHRANS C
C     A(53) = BARTLETT F
C     A(54) = SIGNIFICANCE LEVEL FOR BARTLETT F
C     A(55) = F*
C     A(56) = NRMAX
C     A(57) = LARGEST VARIANCE / SMALLEST VARIANCE
C     A(58) = MINIMUM SUM OF RANKS
C     A(59) = MAXIMUM SUM OF RANKS
C     A(60) = LARGEST MINIMUM
C     A(61) = I BAR
C     A(62) = CORRECTION FACTOR FOR KRUSKAL-WALLIS
C     A(63) = F1 D.F. FOR KRUSKAL-WALLIS F
C     A(64) = F2 D.F. FOR KRUSKAL-WALLIS F
C     A(65) = V FOR KRUSKAL-WALLIS F
C     A(66) = M FOR KRUSKAL-WALLIS F
C     A(67) = CHARACTER FOR XBAR = IBLANK, IHIGH, OR ILOW
C     A(68) = CHARACTER FOR SD. = IBLANK, IHIGH, OR ILOW.
C     A(69) = S.D. OF GRAND MEAN
C     A(70) = NRMAX - K
C     A(71) = DELTA IN MULTIPLE COMPARISIONS
C     A(72) = T WITH N(I)-1 D.F.
C     A(73) = K-1
C     A(74) = SUM N(I) * (I-IBAR)**2
C     A(75) = F(.05,K,NRMAX-K)
C     A(76) = S * SQRT(K-1) * F
C     A(77) = NRMAX/(NRMAX+1)
C     A(78) = SUM OF N(I) CUBED
C     A(79) = CRITICAL VALUE OF STUDENTIZED RANGE
C     A(80) = SUM OF 1/(N(I)-1)
C     A(81) = M FOR F
C     A(82) = SUM OF S(I) SQUARED
C     A(83) = INTEGRAL PART OF N-1+0.5
C     A(84) = A FOR F
C     A(85) = D.F.
C     A(86) = WEIGHTED MEAN
C     A(87) = SUM OF WEIGHTS
C     A(88) = DENOMINATOR D.F. FOR F*
C     A(89) = SUM OF N(I) CUBED
C     A(90) = K0
C     A(91) = NUMBER OF N(I) NOT EQUAL TO ONE
C     A(92) = I-J+1 FOR MULTIPLE COMPARISIONS
C     A(93) = UNWEIGHTED GRAND MEAN
C
      A(NSUBB2)  = A(NSUBA4)
      A(NSUBB7)  = A(NSUBA3)
      ISUBA4     = NSUBA4
      ISUBA3     = NSUBA3
      ISUBA5     = NSUBA5
      ISUBB2     = NSUBB2
      ISUBB3     = NSUBB3
      ISUBB4     = NSUBB4
      ISUBB6     = NSUBB6
      ISUBB7     = NSUBB7
      ISUBB8     = NSUBB8
      DO 260 I=1,NRMAX
        IF (I.EQ.IONE) GO TO 250
        IF (A(ISUBA4)-A(ISUBA4-1)) 250,250,240
 240    A(ISUBB8)  = A(ISUBA3-1)
        ISUBB2     = ISUBB2 + IONE
        ISUBB3     = ISUBB3 + IONE
        ISUBB4     = ISUBB4 + IONE
        ISUBB6     = ISUBB6 + IONE
        ISUBB7     = ISUBB7 + IONE
        ISUBB8     = ISUBB8 + IONE
        A(ISUBB7)  = A(ISUBA3)
        A(ISUBB2)  = A(ISUBA4)
 250    A(ISUBB3)  = A(ISUBB3) + RONE
        A(ISUBB4)  = A(ISUBB4) + A(ISUBA3)
        A(ISUBB6)  = A(ISUBB6) + A(ISUBA5)
        A(21)      = A(21) + A(ISUBA3)
        ISUBA4     = ISUBA4 + IONE
        ISUBA3     = ISUBA3 + IONE
        ISUBA5     = ISUBA5 + IONE
 260  CONTINUE
C
      ISUBA4     = NSUBA4 + NRMAX - IONE
      ISUBA3     = NSUBA3 + NRMAX - IONE
      ISUBB2     = NSUBB2 + K - IONE
      ISUBB8     = NSUBB8 + K - IONE
      A(ISUBB2)  = A(ISUBA4)
      A(ISUBB8)  = A(ISUBA3)
C
      A(21) = FDIV (A(21),A(56),IND)
C
      CALL SUMMAL (A(NSUBA1),IZERO,A(61))
      ISUBB3 = NSUBB3
      ISUBB4 = NSUBB4
      DO 270 I=1,K
        ATEMP(1) = A(ISUBB3) * FLOAT(I)
        CALL SUMMAL (ATEMP,-IONE,A(61))
        A(ISUBB4) = FDIV (A(ISUBB4),A(ISUBB3),IND)
        IF (A(ISUBB3).GT.RONE) M6 = I
        ISUBB3 = ISUBB3 + IONE
        ISUBB4 = ISUBB4 + IONE
 270  CONTINUE
C
      CALL SUMMAL (A(NSUBA1),IONE,A(61))
      A(61) = FDIV (A(61),A(56),IND)
C
C     COMPUTE   SD'S, SS.
C
C     COMPUTE BETWEEN SUM OF SQUARES AND UNWEIGHTED GRAND MEAN
C
      ISUBB3 = NSUBB3
      ISUBB4 = NSUBB4
      CALL SUMMAL (A(NSUBA1),IZERO,A(1))
      DO 290 I=1,K
        JEND = A(ISUBB3)
        DO 280 J=1,JEND
          ATEMP(1) = (A(ISUBB4)-A(21))**2
          CALL SUMMAL (ATEMP,-IONE,A(1))
 280    CONTINUE
        A(93)  = A(93)  + A(ISUBB4)
        ISUBB3 = ISUBB3 + IONE
        ISUBB4 = ISUBB4 + IONE
 290  CONTINUE
      A(93) = FDIV (A(93),FLOAT(K),IND)
C
      CALL SUMMAL (A(NSUBA1),IONE,A(1))
C
C     COMPUTE WITHIN SUM OF SQUARES
C
      ISUBB3 = NSUBB3
      ISUBB4 = NSUBB4
      ISUBA3 = NSUBA3
      CALL SUMMAL (A(NSUBA1),IZERO,A(4))
      DO 310 I=1,K
        JEND = A(ISUBB3)
        DO 300 J=1,JEND
          ATEMP(1) = (A(ISUBA3)-A(ISUBB4))**2
          CALL SUMMAL (ATEMP,-IONE,A(4))
          ISUBA3   = ISUBA3 + IONE
 300    CONTINUE
        ISUBB3 = ISUBB3 + IONE
        ISUBB4 = ISUBB4 + IONE
 310  CONTINUE
C
      CALL SUMMAL (A(NSUBA1),IONE,A(4))
C
C     COMPUTE TOTAL SUM OF SQUARES
C
      ISUBA3 = NSUBA3
      ISUBB3 = NSUBB3
      CALL SUMMAL (A(NSUBA1),IZERO,A(5))
      DO 330 I=1,K
        JEND = A(ISUBB3)
        DO 320 J=1,JEND
          ATEMP(1) = (A(ISUBA3)-A(21))**2
          CALL SUMMAL (ATEMP,-IONE,A(5))
          ISUBA3   = ISUBA3 + IONE
 320    CONTINUE
        ISUBB3 = ISUBB3 + IONE
 330  CONTINUE
C
      CALL SUMMAL (A(NSUBA1),IONE,A(5))
C
C     COMPUTE STANDARD DEVIATIONS
C
      ISUBA3 = NSUBA3
      ISUBB3 = NSUBB3
      ISUBB4 = NSUBB4
      ISUBB5 = NSUBB5
      DO 350 I=1,K
        JEND = A(ISUBB3)
        CALL SUMMAL (A(NSUBA1),IZERO,A(ISUBB5))
        DO 340 J=1,JEND
          ATEMP(1) = (A(ISUBA3)-A(ISUBB4))**2
          CALL SUMMAL (ATEMP,-IONE,A(ISUBB5))
          ISUBA3   = ISUBA3 + IONE
 340    CONTINUE
        CALL SUMMAL (A(NSUBA1),IONE,A(ISUBB5))
        ISUBB3 = ISUBB3 + IONE
        ISUBB4 = ISUBB4 + IONE
        ISUBB5 = ISUBB5 + IONE
 350  CONTINUE
C
      A(17) = RZERO
      A(22) = A(NSUBB4)
      A(23) = A(NSUBB4)
      A(58) = A(NSUBB6)
      A(59) = A(NSUBB6)
      A(24) = FSQRT (A(NSUBB5))
      A(25) = RZERO
      A(30) = A(NSUBB3)
      A(26) = A(NSUBB7)
      A(27) = A(NSUBB8)
      A(48) = RZERO
      ISUBB3 = NSUBB3
      ISUBB4 = NSUBB4
      ISUBB5 = NSUBB5
      ISUBB6 = NSUBB6
      ISUBB7 = NSUBB7
      ISUBB8 = NSUBB8
      DO 410 I=1,K
        IF (A(ISUBB3)-RONE) 400,390,360
 360    A(ISUBB5) = FSQRT (FDIV(A(ISUBB5),A(ISUBB3)-RONE,IND))
        IF (A(ISUBB5)) 380,380,370
 370    A(81) = A(81) + (A(ISUBB3)-RONE) * FLOG (A(ISUBB5)*A(ISUBB5))
 380    A(91) = A(91) + RONE
        A(25) = AMAX1 (A(25),A(ISUBB5))
        A(24) = AMIN1 (A(24),A(ISUBB5))
        A(80) = A(80) + FDIV (RONE,A(ISUBB3)-RONE,IND)
 390    A( 2) = A(2) + A(ISUBB3) * (FLOAT(I)-A(61))*(A(ISUBB4)-A(21))
        A(74) = A(74) + A(ISUBB3)*((FLOAT(I)-A(61))**2)
        A(30) = AMAX1 (A(30),A(ISUBB3))
        A(22) = AMIN1 (A(22),A(ISUBB4))
        A(23) = AMAX1 (A(23),A(ISUBB4))
        A(58) = AMIN1 (A(58),A(ISUBB6))
        A(59) = AMAX1 (A(59),A(ISUBB6))
        A(26) = AMIN1 (A(26),A(ISUBB7))
        A(27) = AMAX1 (A(27),A(ISUBB8))
        A(17) = A(17) + FDIV (A(ISUBB6)**2,A(ISUBB3),IND)
        A(18) = A(18) + FDIV (RONE,A(ISUBB3),IND)
        A(48) = A(48) + (A(ISUBB4)-A(93))**2
        A(82) = A(82) + A(ISUBB5)**2
        A(89) = A(89) + A(ISUBB3)**2
        A(78) = A(78) + A(ISUBB3)**3
 400    ISUBB3 = ISUBB3 + IONE
        ISUBB4 = ISUBB4 + IONE
        ISUBB5 = ISUBB5 + IONE
        ISUBB6 = ISUBB6 + IONE
        ISUBB7 = ISUBB7 + IONE
        ISUBB8 = ISUBB8 + IONE
 410  CONTINUE
C
      A(28) = FDIV (A(27)+A(26),RTWO,IND)
      A(86) = FDIV (A(86),A(87),IND)
      A( 2) = FDIV (A(2)**2,A(74),IND)
      A( 3) = A(1) - A(2)
C
C     DEGREES OF FREEDOM FOR ANOVA
C
      M1    = K - IONE
      A(73) = FLOAT (M1)
      M2    = IONE
      M3    = K - ITWO
      M4    = NRMAX - K
      A(70) = FLOAT (M4)
      M5    = NRMAX - IONE
C
C     MEAN SQUARES
C
      A( 6) = FDIV (A(1),A(73),IND)
      A( 7) = FDIV (A(2),FLOAT(M2),IND)
      A( 8) = FDIV (A(3),FLOAT(M3),IND)
      A( 9) = FDIV (A(4),FLOAT(M4),IND)
      A(10) = FDIV (A(5),FLOAT(M5),IND)
      A(11) = FDIV (A(6),A(9),IND)
      A(12) = FDIV (A(7),FDIV(A(3)+A(4),A(56)-RTWO,IND),IND)
      A(13) = FDIV (A(8),A(9),IND)
      BTEMP = A(73)
      CALL QFORF (BTEMP,FLOAT(M4),A(11),ATEMP(1))
      A(14) = ATEMP(1)
      CALL QFORF (FLOAT(M2),FLOAT(M4),A(12),ATEMP(1))
      A(15) = ATEMP(1)
      CALL QFORF (FLOAT(M3),FLOAT(NRMAX-ITWO),A(13),ATEMP(1))
      A(16) = ATEMP(1)
C
C     COMPUTE FOR KRUSKAL-WALLIS TEST
C
      A(77) = NRMAX * (NRMAX + IONE)
      A(17) = FDIV (SPCC*A(17),A(77),IND)-RTHRE*FLOAT(NRMAX+IONE)
      A(62) = RONE - FDIV (A(49),FLOAT(NRMAX**3-NRMAX),IND)
      A(17) = FDIV (A(17),A(62),IND)
      A(66) = FDIV (FLOAT(NRMAX**3)-A(78),A(77),IND)
      A(66) = FDIV (A(66),A(62),IND)
      IF (A(66)-A(17))  420,420,430
 420  A(20) = RZERO
      GO TO 440
 430  A(65) = FLOAT (ITHRE*K*M3+NRMAX*(ITWO*K*(K-ITHRE)+IONE) )
      A(65) = FLOAT (ITWO*M1) - FDIV (SPCA*A(65),A(77),IND)
      A(65) = A(65) - FDIV (SPCB*A(18),RFIVE,IND)
      A(63) = A(73) * (A(73)*(A(66)-A(73))-A(65))
      A(63) = FDIV (A(63),RHALF*A(65)*A(66),IND)
      A(64) = (A(66)-A(73)) * FDIV (A(63),A(73),IND)
      A(19) = FDIV (A(17)*(A(66)-A(73)),A(73)*(A(66)-A(17)),IND)
      CALL QFORF (AINT(A(63)+RHALF),AINT(A(64)+RHALF),A(19),ATEMP(1))
      A(20) = ATEMP(1)
C
C     COMPUTE TOTAL STATISTICS
C
 440  A(31) = FSQRT (A(9))
      A(32) = FSQRT (FDIV (A(48),A(73),IND))
      A(33) = FSQRT (A(10))
      A(34) = FDIV (A(31),FSQRT(A(56)),IND)
      A(35) = FDIV (A(32),FSQRT(FLOAT(K)),IND)
      A(36) = FDIV (A(33),FSQRT(A(56)),IND)
      CALL TPCTPT (FLOAT(M4),A(37))
      CALL TPCTPT (A(73),ATEMP(1))
      A(38) = ATEMP(1)
      CALL TPCTPT (FLOAT(M5),A(39))
      A(41) = A(21) - A(34) * A(37)
      A(42) = A(93) - A(35) * A(38)
      A(43) = A(21) - A(36) * A(39)
      A(44) = A(21) + A(34) * A(37)
      A(45) = A(93) + A(35) * A(38)
      A(46) = A(21) + A(36) * A(39)
C
      CALL FPPT (A(73),FLOAT(M4),ALPHA,ATEMP(1))
      A(75) = ATEMP(1)
      A(76) = A(31) * FSQRT (A(73)*A(75))
C
C     TESTS FOR HOMOGENEITY OF VARIANCES
C
      A(51) = FDIV (A(25)*A(25),A(82),IND)
      A(83) = AINT (FDIV (A(56),FLOAT(K),IND)-RHALF)
      A(57) = FDIV ((A(91)-RONE)*A(51),RONE-A(51),IND)
      BTEMP = A(83)
      CALL QFORF (BTEMP,A(83)*(A(91)-RONE),A(57),ATEMP(1))
      A(52) = (A(91)-RONE) * ATEMP(1)
      IF (A(52).GT.RONE) A(52) = RONE
      A(57) = (FDIV (A(25),A(24),IND))**2
      A(81) = A(70) * FLOG (A(9))-A(81)
      A(84) = FDIV (A(80)-FDIV(RONE,A(70),IND),RTHRE*A(73),IND)
      A(85) = FDIV (A(91)+RONE,A(84)*A(84),IND)
      A(53) = FDIV (A(85)*A(81),(A(91)-RONE)*(FDIV(A(85),(RONE-A(84)
     1   + FDIV (RTWO,A(85),IND)),IND)-A(81)),IND)
      CALL QFORF (A(91)-RONE,AINT(A(85)+RHALF),A(53),ATEMP(1))
      A(54) = ATEMP(1)
      A(90) = FDIV (A(56)-FDIV(A(89),A(56),IND),A(73),IND)
      A(47) = FDIV (A(6)-A(9),A(90),IND)
C
C     COMPUTE LARGEST MINIMUM IN A(60) AND SMALLEST MAXIMUM IN A(29)
C
      A(60) = A(NSUBB7)
      ISUBB7 = NSUBB7
      A(29) = A(NSUBB8)
      ISUBB8 = NSUBB8
      DO 450 I=1,K
        IF (A(ISUBB7).GE.A(60)) A(60) = A(ISUBB7)
        IF (A(ISUBB8).LE.A(29)) A(29) = A(ISUBB8)
        ISUBB7 = ISUBB7 + IONE
        ISUBB8 = ISUBB8 + IONE
 450    CONTINUE
C
C                   *** COMPUTATIONS ARE NOW COMPLETE ***
C                EXCEPT FOR COMPUTATIONS DONE WHILE PRINTING
C
C     ================================================================
C
      M7 = L2 - 12
      GO TO (460,470), M7
C
C     AUTOMATIC PRINTING WHEN L2 = 13
C
 460  CALL PAGE (IFOUR)
C
C     (1)   PRINT ANALYSIS OF VARIANCE.
C     (2)   PRINT KRUSKAL-WALLIS TEST.
C
      CALL OWPRAV (K,M1,M3,M4,M5,MA,NB,ND,NW)
      IF (NCRT.NE.IZERO) CALL PAGE(0)
C
C     (3)   PRINT ESTIMATES -
C              GROUP, NUMBER, MEAN, MINIMUM, MAXIMUM, SUM, RANKS
C
      CALL OWPRES (K,MA,NB,ND,NW,MT,NSD,A(NSUBB2),A(NSUBB3),
     1             A(NSUBB4),A(NSUBB6),A(NSUBB7),A(NSUBB8))
C
C     (4)   COMPUTE CONFIDENCE LIMITS AND STANDARD DEVIATION OF MEAN.
C
      CALL OWPRCL (K,M6,MA,MT,NB,ND,NW,NSD,A(NSUBB2),A(NSUBB3),
     1             A(NSUBB4),A(NSUBB5),A(NSUBB6),A(NSUBB7),
     2             A(NSUBB8),A(NSUBB9))
C
C     (5)   COMPUTE AND PRINT FOR MULTIPLE COMPARISIONS
C
      CALL OWPRMC (K,M4,MA,ND,NW,A(NSUBB3),A(NSUBB4),A(NSUBB6),
     1             A(NSUBB9),A(NSUBBT))
C
C     (6)   TESTS FOR HOMOGENIETY OF VARIANCES.
C     (7)   COMPONENT OF VARIATION.
C
      IF (A(91).GE.RTWO) CALL OWPRHV (K,M3,MA,ND,NW,A(NSUBB3),
     1                                A(NSUBB4),A(NSUBB5),A(NSUBBT))
C
C     (8)   PRINT BOX PLOTS ONLY IF REMOTE COMMAND IS ENVOKED.
C
      IF (NCRT.EQ.IZERO)
     1CALL OWPRBP(K,MA,MT,NB,ND,NW,NSD,A(NSUBA3),A(NSUBB2),A(NSUBB3))
C
C     ================================================================
C
C     STORE TAGS, NUMBERS, MEANS, AND STANDARD DEVIATIONS.
C
 470  IF (NARGS.EQ.ITWO) RETURN
C
      CALL OWSTRE (K,A(NSUBB2),A(NSUBB3),A(NSUBB4),A(NSUBB5))
C
      RETURN
C
C     ================================================================
C
      END
*ONLPLT
      SUBROUTINE ONLPLT
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. ONLPLT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PLOT MAXIMUM OF 5 CURVES.
C        IF MORE THAN ONE POINT FALLS ON THE SAME POSITION, 
C        A TALLY IS KEPT AND THE NUMBER IS PRINTED.  THE USER MAY
C        PROVIDE THE BOUNDS ON THE X,Y COORDINATES.  IF BOUNDS ARE
C        PROVIDED, THEY MUST APPEAR IN PAIRS AS REAL NUMBERS. IF A PAIR
C        OF REAL NUMBERS IS EQUAL, THE PROGRAM ASSUMES THAT BOUNDS HAVE
C        NOT BEEN SPECIFIED BY THE USER AND BOUNDS WILL BE CALCULATED.
C        IF MORE THAN NINE POINTS COINCIDE, AN X IS PRINTED.
C
C     INSTRUCTIONS WHICH NEED ONLPLT ARE AS FOLLOWS ...
C
C       I   PLOT Y (C),(C),... X (C)
C      II   PLOT Y (C),(C),....,(YMIN,YMAX) X (C) (XMIN,XMAX)
C     III   PLOT Y (C),(C),....,(YMIN,YMAX) X (C) 
C      IV   PLOT Y (C),(C),.... V (C) (XMIN,XMAX) 
C       V   PLOT Y (C),(C),.... X (XMIN,XMAX) (YMIN,YMAX)
C      VI   NPLOT SAME AS I THRU V EXCEPT PRINTING CONTINUES
C              WITHOUT STARTING A NEW PAGE.
C     VII   CPLOT SAME AS I THRU V EXCEPT AN EXTRA ARGUMENT IS ADDED. 
C             EACH Y ARGUMENT IS FOLLOWED BY AN EXTRA ARGUMENT IN THE 
C             INSTRUCTION ... 
C             (A) INDICATES THE CHARACTER(S) TO BE USED IN PLOTING THE C
C             (B) AND THE ARGUMENT MAY BE A CONSTANT OR A COLUMN NUMBER.
C                 (1) IF THE ARGUMENT IS A CONSTANT THE LA(ARGUMENT +1)
C                     CHARACTER WILL BE USED FOR ALL PLOTTING POINTS
C                     FOR THAT CURVE.
C                 (2) IF THE ARGUMENT IS A COLUMN NUMBER, NUMBER IN COL
C                     ONE OF THAT ARGUMENT WILL POINT TO THE CHARACTER
C                     TO BE USED FOR THE FIRST POINT, ROW TWO VALUE
C                     THE CHARACTER FOR THE SECOND POINT, ETC.
C                 (3) THE VALUES IN THE COLUMN NUMBER OR CONSTANT MUST
C                     BE BETWEEN 0 AND 48.
C                     THE CARACTER USED IS LA(VALUE+1).
C                     IF OTHER VALUES ARE USED FOR ANY CURVE(S), THEY 
C                     WILL BE IGNORED AND CHARACTER USED IS SAME AS PLOT
C             (C) IF MORE THAN ONE POINT FALLS ON THE SAME SPOT,
C                 THE CHARACTER PRINTED IS FOR THE LAST POINT.
C     VIII  NCPLOT COMBINATION OF VI AND VII.
C     IX    GRAPH OF Y IN (C),...,(C) VS X IN (C).
C             (A) SAME AS I EXCEPT X AND Y LOWER AND UPPER
C                 LIMITS ARE DETERMINED BY GRAPH SUBROUTINE.
C
C     ERRORS ...
C
C     WHEN TYPE II INSTRUCTION IS USED, THERE MUST BE TWO PAIRS OF REAL
C        NUMBERS. OTHERWISE, THE FOLLOWING MESSAGE IS PRINTED
C           ' Y BOUNDS ARE NOT SET UP CORRECTLY'
C
C       I IF BOUNDS ARE PROVIDED, THEN THERE MUST BE FOUR REAL NUMBERS.
C      II IF A SINGLE REAL NO. APPEARS AHEAD OF COLUMN NOS., THE FOLLOW-
C         ING MESSAGE WILL BE PRINTED AND NO PLOTTING WILL TAKE PLACE 
C           ' Y BOUNDS ARE NOT SET UP CORRECTLY'
C     III IF A PLOT COMMAND ENDS WITH ONE REAL NO, THE FOLLOWING MESSAGE
C         WILL BE PRINTED AND PLOTTING WILL BE TERMINATED
C           ' X BOUNDS ARE N4T SET UP CORRECTLY'
C
C     L2 =  5,     PLOT
C     L2 =  6,     PAGE PLOT
C     L2 =  7,     NPLOT
C     L2 = 15,     CPLOT
C     L2 = 16,     NCPLOT
C     L2 = 19,     NICE PLOT
C     L2 = 20,     NICE NPLOT 
C     L2 = 21,     NICE CPLOT 
C     L2 = 22,     NICE NCPLOT
C
C     IF LWIDE IS LESS THAN 75, THEN PAGE PLOT WILL BE REDUCED ON
C        X-AXIS WIDTH.
C     FOR PLOT, X-AXIS WILL BE AS LARGE AS LWIDE WILL ALLOW.
C        IF LWIDE IS LESS THAN 50, NO PLOTTING WILL BE DONE.
C
C     LENGTH CONTROLS LENGTH OF Y AXIS, WHICH IS
C        10*N+5 LESS THAN OR EQUAL TO LENGTH,
C        WHERE N = NUMBER OF INTERVALS OF 10 LINES.
C     IF LENGTH IS LESS THAN 15 NO PLOTTING WILL BE DONE.
C
C     POINTS ON X AND Y AXES WILL BE IN READABLE FORMAT WHENEVER
C        POSSIBLE.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -  JANUARY, 1968.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION    IAA(1000), IBOOL(110) 
      DIMENSION    IPR(151),    KCCL(6), KSPACE(6)
      DIMENSION    MPRINT(125),  MTITX(60) 
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             XP(6)
      REAL             XAN, XDOWN, XMAX, XMIN
      REAL             XUP, YAN, YDOWN
      REAL             YMAX, YMIN, YUP
      REAL             SPCA
C
C     .....................................................................
C
      CHARACTER LA*1, IPR*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER MTITX*1, MPRINT*1
C
C     ....................................................................
C
      EQUIVALENCE ( MTITX(1),ITLE(1,5)) 
      EQUIVALENCE (   IAA(1),  A(   1)) 
      EQUIVALENCE (KSPACE(1), IAA(461)) 
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXNWX / 10 /
C
      DATA SPCA /  1.0005 /
C
C     ==================================================================
C
C     INITIAL SWITCHES.
C
      IF (NARGS.LT.ITWO) THEN
       CALL ERROR(10)
       RETURN
      ENDIF
      IF (NRMAX.GT.IZERO) GO TO 10
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  10  IBOOL(1) = 38 
      IBOOL(2) = 41 
      IBOOL(3) = 40 
      IBOOL(4) = 44 
      IBOOL(5) = 39 
      IF (NARGS.LE.6) GO TO 40
      NTOP   = NARGS
      IF (NTOP.GT.41) NTOP = 36
      J = 6
      DO 30 II=1,3
        ICHAR = IONE
        DO 20 I=1,NTOP
          IBOOL(J) = ICHAR
          J        = J + IONE 
          ICHAR    = ICHAR + IONE
  20    CONTINUE
        IF (J.GT.NARGS) GO TO 40
        IBOOL(J)   = 38
        IBOOL(J+1) = 41
        IBOOL(J+2) = 40
        IBOOL(J+3) = 44
        IBOOL(J+4) = 39
        J          = J + IFIVE
        IF (J.GT.NARGS) GO TO 40
        IF (41 * (II + 1).GT.NARGS) NTOP = NARGS - 41 * II - IFIVE
  30  CONTINUE
C
  40  CONTINUE   
      IF (L2.LT.15 .OR. L2.EQ.19 .OR. L2.EQ.20) GO TO 100
C
C     COMMAND IS EITHER CPLOT, NCPLOT, NICE CPLOT OR NICE NCPLOT.
C
C     NCPLT = IONE
      I = ITHRE
      IS = IONE
      NARGSA = NARGS
      IF (NARGS.LT.ITHRE) THEN
        CALL ERROR (10)
        RETURN
      ENDIF
      IF (KIND(NARGSA).EQ.IONE) NARGSA = NARGSA - ITWO
      IF (KIND(NARGSA).EQ.IONE) NARGSA = NARGSA - ITWO
      IF (KIND(NARGSA-2).EQ.IONE .AND. KIND(NARGSA-1).EQ.IONE)
     1     NARGSA = NARGSA - ITWO
  50  IF (KIND(I-1).EQ.IONE) GO TO 60
C
C     COLUMN NUMBER FOR SYMBOL. SET UP IBOOL AS NEGATIVE ADDRESS FOR
C     THAT COLUMN.
C
      CALL ADRESS (I-IONE,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
      IBOOL(IS) = -J
      IS = IS + IONE
      GO TO 80
C
C     CONSTANT FOR SYMBOL. USE APROPRIATE CHARACTER IN IBOOL(I).
C
  60  JA = ARGS(I-1) + SPCA
      IF (JA.EQ.11) JA = IONE 
      IF (JA.GT.ITEN) JA = JA - IONE
      IF (JA.LE.IZERO .OR. JA.GT.48) GO TO 70
      IBOOL(IS) = JA
  70  IS = IS + IONE
  80  DO 90 IA=I,NARGS
        IARGS(IA-1) = IARGS(IA)
        KIND(IA-1)  = KIND(IA)
        ARGS(IA-1)  = ARGS(IA)
  90  CONTINUE
C
      NARGS = NARGS - IONE
      I = I + IONE
      NARGSA = NARGSA - IONE
      IF (I.LE.NARGSA) GO TO 50
 100  ISWT  = IONE
      XUP   = RPIFY 
      XDOWN = RMIFY 
      YUP   = RPIFY 
      YDOWN = RMIFY 
      LW    = IDIV (LWIDE-19,ITEN,IND)
      IF (LW.GE.IFOUR) GO TO 110
      CALL ERROR (246)
      RETURN
C
C     ..................................................................
C
 110  IF (LW.GT.ITEN) LW = ITEN
      LENG = LENGTH + IFIVE
      LENB = IDIV (LENG-IFIVE,ITEN,IND) 
      LENA = LENB + IONE
      IF (LENA.GT.IONE) GO TO 120
      CALL ERROR (255)
      RETURN
C
C     ..................................................................
C
 120  IF (L2.NE.6) GO TO 140
      IF (LW.GE.6) GO TO 130
      CALL ERROR (247)
      GO TO 140
 130  LW = 6
 140  IF (NARGS.EQ.ITWO) GO TO 170
      IF (KIND(NARGS).EQ.IZERO) GO TO 160
      IF (KIND(NARGS).NE.KIND(NARGS-1)) GO TO 220 
C
C     X OR Y BOUNDS ARE PROVIDED.
C
      IF (KIND(NARGS-2).EQ.IZERO) GO TO 150
      IF (KIND(NARGS-3).EQ.IZERO) GO TO 220
      ISWT  = IFIVE 
      YUP   = ARGS(NARGS)
      YDOWN = ARGS(NARGS-1)
      XUP   = ARGS(NARGS-2)
      XDOWN = ARGS(NARGS-3)
      NARGS = NARGS - IFOUR
      IF (L2.GE.19) CALL ERROR (258)
      GO TO 170
C
C     X BOUNDS ARE PROVIDED.
C
 150  ISWT  = ITHRE 
      XUP   = ARGS(NARGS)
      XDOWN = ARGS(NARGS-1)
      NARGS = NARGS - ITWO
      IF (NARGS.EQ.ITWO .AND. L2.LT.19) GO TO 170 
      IF (L2.LT.19) GO TO 160 
      IF (KIND(NARGS-1).EQ.IZERO) CALL ERROR (258)
C
C     CHECK TO SEE IF THERE ARE Y BOUNDS.
C
 160  IF (KIND(NARGS-1).NE.KIND(NARGS-2)) GO TO 220
      IF (KIND(NARGS-1).EQ.IZERO) GO TO 170
C
C     Y LIMITS ARE PROVIDED.
C
      ISWT  = ISWT + IONE
      YUP   = ARGS(NARGS-1)
      YDOWN = ARGS(NARGS-2)
      IARGS(NARGS-2) = IARGS(NARGS)
      KIND(NARGS-2) = IZERO
      NARGS = NARGS - ITWO
      IF (L2.GE.19) CALL ERROR (258)
 170  DO 180 I=1,NARGS
        KCCL(I) = IARGS(I)
 180  CONTINUE
C
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     NO ERROR FOUND IN COLUMN NUMBERS. 
C
C     ==================================================================
C
      INDS = IZERO
      IF (L2.GE.19) CALL GRAPH (XMAX,XMIN,XAN,YMAX,YMIN,YAN,INDS)
      IF (L2.GE.19 .AND. INDS.NE.IZERO) RETURN
      IF (L2.LT.19) CALL ONPLTB (ISWT,XAN,XDOWN,XMAX,XMIN,XUP,
     1                           YAN,YDOWN,YMAX,YMIN,YUP)
      IF (L2.GE.19) ISWT = IONE
      CALL ONPLTH (IBOOL,ISWT,KCCL,LW,XAN,XMAX,XMIN,XP,YAN,YMAX,YMIN,
     1             IPR,IR)
      IF (IR.NE.IZERO) RETURN 
C
C     ..................................................................
C
      CALL ONPLTG (IBOOL,LENG,LW,XMAX,XMIN,YMAX,YMIN,IPR,MPRINT)
      MWA = IDIV (LW,ITWO,IND) + IONE
      IF (LW.EQ.7) MWA = IFIVE
      CALL PUTCH (LA(45),120,MPRINT)
C
      JX = ITEN
C
      DO 200 K=1,MWA
        ITYPE = 7
        CALL MINNW (XP(K),1,5,MAXNWX+ITEN,MPRINT(JX),IZERO,NW,ND,MW,MD)
        IF (MW.LE.MAXNWX) GO TO 190
          ITYPE = IONE
          CALL MINNW (XP(K),1,5,MAXNWX,MPRINT(JX),IZERO,NW,ND,MW,MD)
          CALL PUTCH (LA(45),MAXNWX+IONE,MPRINT(JX))
 190    NBX = MAXNWX - MW
        CALL RFORMT (ITYPE,5,A,XP(K),NBX,1,MW,MD,MPRINT(JX),IRF)
        JX = JX + KSPACE(K)
 200  CONTINUE
C
      JX = JX - IONE - KSPACE(MWA) + MAXNWX
      WRITE (IPRINT,230) (MPRINT(I),I=1,JX)
      MWD = LWIDE - 60
      IF (MWD.GT.IONE) GO TO 210
      MWC = LWIDE - IONE
      WRITE (IPRINT,230) (MTITX(I),I=1,MWC)
      RETURN
C
C     ..................................................................
C
 210  MWD = IDIV (MWD,ITWO,IND)
      IF (L2.EQ.6) MWD = 14
      WRITE (IPRINT,230) (LA(45),I=1,MWD), MTITX
      RETURN
C
C     ..................................................................
C
C     PRINT 'X BOUNDS ARE NOT SET UP CORRECTLY'.
C
 220  CALL ERROR (20)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 230  FORMAT (1X,119A1)
C
C     ==================================================================
C
      END 
*ONPLTB
      SUBROUTINE ONPLTB (IWT,XAN,XDN,XMA,XMI,XUP,YAN,YDN,YMA,YMI,YUP)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ONPLTB V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DETERMINE THE BOUNDERIES AND NUMBER OF POINTS TO BE PLOTTED.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1968.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             XAN, XDN, XMA, XMI, XUP
      REAL             YAN, YDN, YMA, YMI, YUP
      REAL             XAP, XDOWN, XMAX, XMIN
      REAL             YAP, YDOWN, YMAX, YMIN
C
C     ==================================================================
C
C     SEARCH FOR MAX AND MIN ON AXIS,  IF BOUNDS ARE NOT PROVIDED.
C        OTHERWISE, TALLY NUMBER OF POINTS THAT FALL OUTSIDE OF BOUNDS.
C
      RZERO = FLOAT (IZERO)
      XDOWN = XDN
      YDOWN = YDN
      XMAX  = RZERO
      XMIN  = RZERO
      YMAX  = RZERO
      YMIN  = RZERO
      KY    = IZERO
      ISWT  = IWT
      M     = NARGS - IONE
      IF (XUP.GE.XDOWN) GO TO 10
      XAP = XDOWN
      XAN = XUP
      GO TO 20
  10  XAP = XUP
      XAN = XDOWN
  20  IF (YUP.GE.YDOWN) GO TO 30
      YAP = YDOWN
      YAN = YUP
      GO TO 40
  30  YAP = YUP
      YAN=YDOWN
  40  K1 = IARGS(NARGS)
      K2 = K1 - IONE + NRMAX
      IF (ISWT.GT.ITWO) GO TO 140
      IF (ISWT.EQ.ITWO) GO TO 70
      XMAX = RC(K1)
      XMIN = XMAX
      DO 60 I=K1,K2
        IF (XMAX.GE.RC(I)) GO TO 50
        XMAX = RC(I)
        GO TO 60
  50    IF (XMIN.LE.RC(I)) GO TO 60
        XMIN = RC(I)
  60  CONTINUE
      GO TO 130
  70  KEY = IONE
      DO 120 IK=1,M
        IKK = IARGS(IK)
        DO 110 I=K1,K2
          IF (RC(IKK).GE.YAN .AND. RC(IKK).LE.YAP .AND. KEY.EQ.IONE)
     1         GO TO 80
          IF (RC(IKK).GE.YAN .AND. RC(IKK).LE.YAP .AND. KEY.EQ.ITWO)
     1         GO TO 90
          GO TO 100
  80      XMAX = RC(I)
          XMIN = XMAX
          KEY  = ITWO
          GO TO 100
  90      IF (XMAX.LT.RC(I)) XMAX = RC(I)
          IF (XMIN.GT.RC(I)) XMIN = RC(I)
 100      IKK = IKK + IONE
 110    CONTINUE
 120  CONTINUE
C
      IF (KEY.EQ.ITWO) GO TO 130
      XMIN = XDOWN
      XMAX = XUP
C
 130  XAP = XMAX
      XAN = XMIN
 140  GO TO (160,260,150,260,260), ISWT
 150  KEY = ITWO
      GO TO 170
 160  KEY = IONE
 170  DO 250 J=1,M
        K1 = IARGS(NARGS)
        K3 = IARGS(J)
        K4 = K3 - IONE + NRMAX
        IF (J.GT.IONE) GO TO 180
        YMAX = RC(K3)
        YMIN = YMAX
        KY   = IONE
 180    IF (KEY.EQ.ITWO) GO TO 200
        DO 190 I=K3,K4
          IF (YMAX.LT.RC(I)) YMAX = RC(I)
          IF (YMIN.GT.RC(I)) YMIN = RC(I)
 190    CONTINUE
        GO TO 250
 200    DO 240 I=K3,K4
          IF (RC(K1).GE.XAN .AND. RC(K1).LE.XAP .AND. KY.EQ.IONE)
     1        GO TO 210
          IF (RC(K1).GE.XAN .AND. RC(K1).LE.XAP .AND. KY.EQ.ITWO)
     1        GO TO 220
          GO TO 230
 210      YMAX = RC(I)
          YMIN = RC(I)
          KY   = ITWO
          GO TO 230
 220      IF (YMAX.LT.RC(I)) YMAX = RC(I)
          IF (YMIN.GT.RC(I)) YMIN = RC(I)
 230      K1 = K1 + IONE
 240    CONTINUE
 250  CONTINUE
C
      YAP = YMAX
      YAN = YMIN
      IF (ISWT.EQ.IONE) GO TO 280
      GO TO 270
 260  YMAX  = YUP
      YMIN  = YDOWN
      IF (ISWT.EQ.ITWO) GO TO 280
 270  XMAX = XUP
      XMIN = XDOWN
 280  YMI = YMIN
      YMA = YMAX
      XMI = XMIN
      XMA = XMAX
      RETURN
C
C     ==================================================================
C
      END
*ONPLTG
      SUBROUTINE ONPLTG (IBOOL,LENG,LW,XMA,XMI,YMA,YMI,IPR,MPRINT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. ONPLTG V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PLOT BOUNDERY AND POINTS FOR PLOT AND PAGE PLOT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -  JANUARY, 1968.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION    IAA(1000), IBOOL(*),   IDGT(9), INM(11)
      DIMENSION    IPR(*)
      DIMENSION    MPRINT(*), MTIT(60)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             XMA, XMI, YMA, YMI
      REAL             YSS(100)
      REAL             XDELTA, XL, XMAX, XMIN
      REAL             XT, YDELTA, YL
      REAL             YMAX, YMIN, YP, YS, YT
      REAL             FDIV
      REAL             SPCA
C
C     ..................................................................
C
      CHARACTER LA*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1,IPR*1
      CHARACTER IDGT*1, MTIT*1  , LTITX*1, INM*1, MPRINT*1
C
C     .................................................................
C
      EQUIVALENCE (  IDGT(1),   LA( 3)) 
      EQUIVALENCE (  MTIT(1),ITLE(1,6)) 
      EQUIVALENCE (  IAA(1),  A(   1))
      EQUIVALENCE (  YSS(1),  A(501))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXNWY / 10 /
C
      DATA SPCA /  1.0005 /
C
C     ==================================================================
C
      XMAX  = XMA
      XMIN  = XMI
      YMAX  = YMA
      YMIN  = YMI
      NCPLT = IZERO 
      IF (L2.GE.15) NCPLT = IONE
      LENB  = IDIV (LENG-IFIVE,ITEN,IND)
      LEN   = LENB * ITEN
      LENA  = LENB + IONE
      IPT   = LW * ITEN
      IPTX  = IPT + IONE
      IPTXX = IPTX + IONE
      M = NARGS - IONE
C
C     DETERMINE X AND Y INCREMENTS FOR PLOT.
C
      YDELTA = FDIV (YMAX-YMIN,FLOAT(LEN),IND)
      K1 = IARGS(NARGS)
      XDELTA = FDIV (XMAX-XMIN,FLOAT(IPT),IND)
      YL = YMAX - FDIV (YDELTA,RTWO,IND)
      YT = YMAX
      KYTL = IONE
      IF (YMAX.LT.YMIN) KYTL = ITWO
      KXTL = IONE
      IF (XMAX.LT.XMIN) KXTL = ITWO
      ITB = IONE
      YSS(1) = YMIN 
      YSS(LENA) = YMAX
      RDELTA = RHALF * YDELTA 
      DO 10 I=2,LENB
        YSS(I) = YSS(I-1) + RTEN * YDELTA
        IF (ABS(YSS(I)).LT.RDELTA) YSS(I) = RZERO 
  10  CONTINUE
C
      ITYPE = 7
      CALL MINNW (YSS,LENA,5,MAXNWY+ITEN,INM,IZERO,NW,ND,MW,MD)
      IF (MW.LE.MAXNWY) GO TO 20
        ITYPE = IONE
        CALL RFORMT (0,5,YSS,YS,LENA,MAXNWY,MW,MD,INM,IRF)
  20  NBY = MAXNWY + IONE - MW
C
      CALL PUTCH (LA(45),MAXNWY+IONE,INM)
C
C     THE I LOOP CONTROLS THE LENA DIVISIONS OF THE Y ORDINATE.
C
      DO 200 I=1,LENA
        L = IONE
C
C       THE J LOOP IS FOR EACH LINE OF PRINT WITHIN THE DIVISIONS.
C
        DO 190 J=1,10
C
C         BLANK OUT PRINT BUFFER LINE.
C
          CALL PUTCH (LA(45),IPTX,MPRINT)
C
C         THE KK INDEX IS FOR EACH CURVE.
C
          DO 160 KK=1,M
            K3 = IARGS(KK)
            IA = IZERO
            K4 = K3 - IONE + NRMAX
            K5 = K1 
C
C          THIS DETERMINES IF Y(K) VALUE IS ON THE PRESENT PRINT LINE.
C
            DO 150 K=K3,K4
              IF (KYTL.EQ.ITWO) GO TO 30
              IF (RC(K).GT.YT) GO TO 140
              IF (RC(K)-YL) 140,140,40
  30          IF (RC(K).GT.YL) GO TO 140
              IF (RC(K).LE.YT) GO TO 140
C
C             YES.  Y(K) BELONGS ON THIS PRINT LINE. THEREFORE,
C                DETERMINE WHERE ALL THE RC(K5) FALL ON THE X-AXIS.
C
  40          XL = XMIN
              XT = XMIN + FDIV (XDELTA,RTWO,IND)
              DO 130 KA=1,IPTX
                IF (KXTL.EQ.IONE) GO TO 50
                IF (RC(K5).LE.XT) GO TO 120
                IF (RC(K5)-XL) 60,60,120
  50            IF (RC(K5).LT.XL) GO TO 120
                IF (RC(K5).GE.XT) GO TO 120
  60            IF (MPRINT(KA).NE.LA(45) .AND. NCPLT.EQ.IZERO) GO TO 80
C
C               DETERMINE SYMBOL TO BE USED.
C
                IF (IBOOL(KK).GT.IZERO) GO TO 70
                IS = IA - IBOOL(KK)
                KKA = RC(IS) + SPCA
                IF (KKA.EQ.11) KKA = IONE
                IF (KKA.GT.ITEN) KKA = KKA - IONE 
                IF (KKA.LE.IZERO .OR. KKA.GT.48) KKA = IONE 
                MPRINT(KA) = LA(KKA)
                GO TO 140
  70            KKA = IBOOL(KK)
                MPRINT(KA) = LA(KKA)
                GO TO 140
C
C               IF MORE THEN ONE POINT FALLS ON THE PRINT POSITION,
C                  TALLY THE NUMBER OF POINTS.
C
  80            IF (MPRINT(KA).NE.IDGT(8)) GO TO 90
                MPRINT(KA) = LA(34)
                GO TO 140
  90            IF (MPRINT(KA).EQ.LA(34)) GO TO 140
                DO 100 KKK=1,7
                  IF (MPRINT(KA).EQ.IDGT(KKK)) GO TO 110
 100            CONTINUE
                MPRINT(KA) = IDGT(1)
                GO TO 140
 110            MPRINT(KA) = IDGT(KKK+1)
                GO TO 140
 120            XL = XT
                XT = XT + XDELTA
 130          CONTINUE
 140          K5 = K5 + IONE
              IA = IA + IONE
 150        CONTINUE
 160      CONTINUE
          LTITX = MTIT(ITB)
          IF (ITB.GT.60) LTITX = LA(45) 
          YP = YT * YL
          YT = YL
          YL = YL - YDELTA
          IF (L.EQ.ITWO) GO TO 170
          IYS = LENA + IONE - I
          YS  = YSS (IYS)
          CALL RFORMT (ITYPE,5,A,YS,NBY,0,MW,MD,INM,IRF)
C
C         THIS PATH IS EXECUTED ONCE IN EVERY DIVISION OF THE Y-AXIS. 
C            EVERY TENTH LINE, STARTING WITH ZERO LINE.
C
          MPRINT(103) = LA(34)
          IF (YP.GT.RZERO) MPRINT(103) = LA(40)
          MPRINT(IPTXX) = MPRINT(103)
          WRITE (IPRINT,220) LTITX, INM, MPRINT(103),
     1                       (MPRINT(IX),IX=1,IPTXX)
          L = ITWO
          IF (I.LE.LENB) GO TO 180
          GO TO 210 
 170      MPRINT(103) = LA(39)
          IF (YP.LE.RZERO) MPRINT(103) = LA(34)
          MPRINT(IPTXX) = MPRINT(103)
C
C         PRINT LINE.
C
          WRITE (IPRINT,230) LTITX, MPRINT (103), 
     1                       (MPRINT(IX),IX=1,IPTXX)
 180      ITB = ITB + IONE
 190    CONTINUE
 200  CONTINUE
C
 210  WRITE (IPRINT,240) (IPR(I),I=1,IPTX)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 220  FORMAT (1X,115A1)
 230  FORMAT (1X,A1,11X,103A1)
 240  FORMAT (14X,101A1)
C
C     ==================================================================
C
      END 
*ONPLTH
      SUBROUTINE ONPLTH (IBOOL,IT,KCCL,LW,XA,XMA,XMI,XP,YA,YMA,YMI,IPR,
     1                   IRF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. ONPLTH V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SET UP HEADINGS FOR PLOT OR PAGE PLOT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -  JANUARY, 1968.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION   IAA(1000), IBOOL(*),   IH(12,8) 
      DIMENSION    IPR(*),   ISYM(5),  KCCL(*), KSPACE(6) 
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             XP(6)
      REAL             XA, XMA, XMI, YA, YMA, YMI 
      REAL             XAN, XAP, XDELTA, XMAX, XMIN, XR
      REAL             XXP, XXPX, YAN, YAP
      REAL             YMAX, YMIN
      REAL             FDIV
      REAL             SPCA
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER*1      IPR, ISYM
      CHARACTER        LHEAD*1
      CHARACTER        IH*1
C
C     ...................................................................
C
      EQUIVALENCE ( LHEAD(1),  IH(1,1)), (  IAA(1),  A(   1))
      EQUIVALENCE (KSPACE(1), IAA(461)) 
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 20.0    /
C
C     ==================================================================
C
      NPLT = IZERO
      NTOT = IZERO
      IF (L2.EQ.7 .OR. L2.EQ.16 .OR. L2.EQ.20 .OR. L2.EQ.22) NPLT = IONE
      NCN  = IZERO
      IPT  = LW * ITEN
      LWF  = IDIV (IPT,ITWO,IND)
      IPTX = IPT + IONE
      M    = NARGS - IONE
      IRF  = IZERO
      MM   = M
      IF (MM.GT.IFIVE) MM = IFIVE
      ISWT  = IT
      XAN    = XA
      XMAX   = XMA
      XMIN   = XMI
      YAN    = YA
      YMAX   = YMA
      YMIN   = YMI
      XDELTA = FDIV (XMAX-XMIN,FLOAT(IPT),IND)
      IF (ISWT.EQ.IONE) GO TO 50
      XAP = XMAX
      IF (XAN.NE.XMIN) XAP = XMIN
      YAP = YMAX
      IF (YAN.NE.YMIN) YAP = YMIN
      DO 40 J=1,M
        K1 = IARGS(NARGS)
        K3 = IARGS(J)
        K4 = K3 - IONE + NRMAX
        DO 30 I=K3,K4
          IF (RC(I).GT.YAP .OR. RC(I).LT.YAN) GO TO 10
          IF (RC(K1).LE.XAP .AND. RC(K1).GE.XAN) GO TO 20
  10      NCN = NCN + IONE
  20      K1 = K1 + IONE
  30    CONTINUE
  40  CONTINUE
C
      NTOT = M * NRMAX - NCN
      IF (NTOT.GT.IZERO) GO TO 50
      CALL ERROR (238)
      IRF = IONE
      RETURN
C
C     ..................................................................
C
C     DETERMINE TYPE OF HEADINGS TO BE PRINTED.
C
  50  IF (YMIN.EQ.YMAX .OR. XMIN.EQ.XMAX) GO TO 290
      CALL HEADS (KCCL,NARGS,IZERO,IONE)
      KZ = 18 * NARGS + IFIVE 
      K = IFOUR
      IF (L2.EQ.6 .AND. ISWT.GT.IONE) K = ITWO
      IF (KZ.LE.LWIDE) GO TO 60
      K = ITWO
      KZ = KZ - 18
      IF (KZ.LE.LWIDE) GO TO 60
      K = IZERO
  60  IF (NPLT.EQ.IZERO) CALL PAGE (K)
      DO 70 I=1,5
        KKA = IBOOL(I)
        ISYM(I) = LA(45)
        IF (KKA.LT.IZERO) GO TO 70
        ISYM(I) = LA(KKA)
  70  CONTINUE
C
      IF (K.NE.IFOUR .OR. L2.EQ.6) GO TO 80
      WRITE (IPRINT,310) (IH(I,NARGS),I=1,12), ((IH(I,J),I=1,12),
     1                                            ISYM(J),J=1,MM)
      GO TO 100
  80  WRITE (IPRINT,340) (IH(I,NARGS),I=1,12)
      IF (K.EQ.IZERO) GO TO 90
      WRITE (IPRINT,330) ((IH(I,J),I=1,12),ISYM(J),J=1,MM)
      GO TO 100
  90  WRITE (IPRINT,350) ((IH(I,J),I=1,12),ISYM(J),J=1,MM)
 100  XDELTA = FDIV (XMAX-XMIN,FLOAT(IPT),IND)
      IF (ISWT.LE.IONE) GO TO 110
      WRITE (IPRINT,300) NTOT, NCN
 110  XP(1) = XMIN
      MWB = IDIV (LW,ITWO,IND)
      MWA = MWB + IONE
      IF (LW.EQ.7) MWA = IFIVE
      XP(MWA) = XMAX
      XR = SPCA * XDELTA
      KSPACE(1) = 20
      KSPACE(MWA) = 20
      MWD = 20
      MWC = ITWO
      IF (MOD(LW,ITWO).EQ.IONE .AND. LW.GT.IFIVE) GO TO 130 
      IF (MOD(LW,ITWO).EQ.IONE .AND. LW.LE.IFIVE) GO TO 140 
      DO 120 I=2,MWB
        KSPACE(I) = 20
        XP(I) = XP(I-1) + XR
 120  CONTINUE
C
      GO TO 160
 130  XP(2) = XP(1) + XR
      XP(4) = XP(5) - XR
      MWC   = ITHRE 
      KSPACE(4) = 20
      GO TO 150
 140  KSPACE(1) = 25
 150  XP(MWC) = XMIN + FDIV (XMAX-XMIN,RTWO,IND)
      MWD = IDIV (IPT,ITWO,IND)-(MWC-ITWO)*20
      KSPACE(2) = MWD
      KSPACE(3) = MWD
 160  DO 170 J=1,100
        IPR(J) = LA(39)
 170  CONTINUE
C
      DO 180 I=1,101,10
        IPR(I) = LA(40)
 180  CONTINUE
C
      IF (MOD(LW,ITWO).EQ.IONE) IPR(LWF+1) = LA(40)
      IF (XMIN*XMAX.GE.RZERO) GO TO 280 
      DO 190 I=1,MWA
        IF (ABS(XP(I)).LE.RTEN * RER) XP(I) = RZERO
 190  CONTINUE
C
      DO 200 I=2,MWA
        IF (XP(I-1)*XP(I).LE.RZERO) GO TO 210
 200  CONTINUE
C
      GO TO 280
 210  JJ = MWD
      JZ  = IZERO
      JX  = I - ITWO
      XXP = XP(I-1) + XDELTA
      IF (MOD(LW,ITWO).EQ.IZERO) GO TO 240
      IF (LW.NE.IFIVE) GO TO 220
      JZ  = JX
      JX  = IZERO
      GO TO 240
 220  IF (JX.GT.IONE) GO TO 230
      IF (JX.EQ.IONE) GO TO 240
      JJ  = 20
      GO TO 240
 230  IF (JX.EQ.ITHRE) JJ = 20
      JZ  = JX - IONE
      JX  = IONE
 240  DO 260 J=1,JJ 
        IF (ABS(XXP).GT.ABS(XDELTA)) GO TO 250
        XXPX = ABS (XXP+XDELTA)
        IF (XXPX.GT.ABS(XDELTA)) GO TO 270
        IF (ABS(XXP).LE.XXPX) GO TO 270 
 250    XXP = XXP + XDELTA
 260  CONTINUE
C
      J = 20
 270  N = JX * 20 + JZ * MWD + J + IONE 
      IPR(N) = LA(34)
 280  WRITE (IPRINT,320) (IPR(K),K=1,IPTX)
      RETURN
 290  CALL ERROR (241)
      IRF = IONE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 300  FORMAT (15H POINTS PLOTTED, I5,35H, POINTS NOT PLOTTED - OUT OF BO
     1UND,I5)
 310  FORMAT (6H HOR- ,12A1,6H,VER- ,5(12A1,2H (,A1,3H), )) 
 320  FORMAT (14X,101A1)
 330  FORMAT (6H VER- ,5(12A1,2H (,A1,3H), ))
 340  FORMAT (6H HOR- ,12A1)
 350  FORMAT (6H VER  ,2(12A1,2H (,A1,3H), ),12A1,2H (,A1,2H),/
     1 6X,2(12A1,2H (,A1,3H), ))
C
C     ==================================================================
C
      END 
*OPONE    
      SUBROUTINE OPONE (N,MX,NX,ISUBSY,ISUBPV,ISUBRS,MA,SSQ,IXX,MCN,MXL)        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  OPONE V 7.00  9/10/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C      PRINT PAGE 1 OF POLYFIT AND FIT. 
C         
C     ALL FORMAL ARGUMENTS ARE INPUT ...
C         
C          N = NUMBER OF VALUES OF THE DEPENDENT VARIABLE.  
C         MX = MAXIMUM (1,M-NX).        
C         NX = 0, IF INSTRUCTION IS POLYFIT OR SPOLYFIT.    
C              0,  IF THE FIRST PREDICTOR IS NOT IDENTICALLY EQUAL TO 1.        
C              1,  IF THE FIRST PREDICTOR IS     IDENTICALLY EQUAL TO 1.        
C     ISUBSY = THE 1ST SUBSCRIPT OF THE S. D. OF PRED. VALUES IN A(.).
C     ISUBPV = THE 1ST SUBSCRIPT OF THE PRED. VALUES IN A(.).         
C     ISUBRS = THE 1ST SUBSCRIPT OF THE RESIDUALS IN A(.).  
C         MA = THE VECTOR WHERE RFORMT STORES NUMBERS TO BE PRINTED.  
C        SSQ = RESIDUAL STANDARD DEVIATION.       
C         IX = THE LOCATION OF THE SUBSCRIPT FOR THE 1ST DEP. VAR.    
C         
C               WRITTEN BY -  
C                      DAVID HOGBEN,    
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -   OCTOBER, 1969.       
C                   CURRENT VERSION - SEPTEMBER, 1991.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IIRGS(100), MA(*)     
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
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
      REAL             SSQ    
      REAL             STDRES, WT       
      REAL             FDIV, FSQRT      
C         
C     ...................................................................
C
      CHARACTER LHEAD*1
      CHARACTER MA*1
C         
C     ...................................................................
C
      EQUIVALENCE (IIRGS(1),LFMT(1))    
C         
C     ==================================================================        
C         
      MCNT   = MCN  
      MXLINE = MXL  
      ISWCH  = IZERO
      NBOT   = IONE 
      NTOP   = N    
      LENINC = IONE 
      IPG    = IONE 
      IW     = IZERO
      IX2    = IONE 
      IX3    = IONE 
      STDRES = RZERO
      IF (N+MCNT+ITHRE.LE.MXLINE) GO TO 20        
C         
C     DETERMINE MAXIMUM NO. OF LINES FOR PG. 2, ETC., MULTIPLE OF 10. 
C         
      LENINC = MXLINE - 8     
      LENINC = IDIV (LENINC,ITEN,IND)   
      LENINC = ITEN * LENINC  
C         
C     DETERMINE MAX. NO. OF LINES FOR PG. 1, MULTIPLE OF 10.
C         
      LINC = MXLINE - MCNT - ITHRE      
      LINC = IDIV (LINC,ITEN,IND)       
      LINC = ITEN * LINC      
      NTOP = LINC   
      IF (NTOP.GT.IZERO) GO TO 10       
      CALL PAGE (IFOUR)       
      NTOP = LENINC 
  10  IPG  = IDIV (N-NTOP,LENINC,IND) + IONE      
      IF (MOD(N-NTOP,LENINC).NE.IZERO) IPG = IPG + IONE     
  20  LINE = MCNT + NTOP + ITHRE        
      IX = IIRGS(IXX)         
      IY = IIRGS(1) 
C         
C       COMPUTE PREDICTED RESPONSES AND STORE IN PLACE OF MATRIX Q    
C          WHICH IS USED BY LSQ.        
C         
        JSUBPV = ISUBPV       
        JSUBRS = ISUBRS       
        JSUBY  = IIRGS(1)     
        DO 30 I=1,N 
          A(JSUBPV) = RC(JSUBY) - A(JSUBRS)       
          JSUBPV = JSUBPV + IONE        
          JSUBY  = JSUBY + IONE         
          JSUBRS = JSUBRS + IONE        
  30    CONTINUE    
      JSUBPV = ISUBPV         
      JSUBSY = ISUBSY         
      JSUBRS = ISUBRS         
      LL    = IZERO 
      NSD   = ISIGD 
      NWM   = 18    
      NSP11 = 9     
      NWMA  = 17    
      NWMB  = 17    
      NWMC  = 18    
      IST   = IONE  
      IF (KIND(2).EQ.IZERO) IW = IIRGS(2)         
      DO 210 IPAGE=1,IPG      
        IF (LWIDE.LT.LWC) GO TO 80      
        IF (L2.EQ.IONE) GO TO 40        
        IF (MX.GT.1) GO TO 50 
  40    WRITE (IPRINT,220) (LHEAD(I1),I1=25,36), (LHEAD(I2),I2=13,24) 
        GO TO 70    
C         
  50    IF (MX.GT.ITWO) GO TO 60        
        WRITE (IPRINT,230) (LHEAD(I),I=25,36), (LHEAD(I),I=37,48),    
     1       (LHEAD(I),I=13,24)         
        GO TO 70    
C         
  60    CALL HEADS (IARGS(NX+6),IONE,IZERO,IONE)  
        WRITE (IPRINT,240) (LHEAD(I),I=25,36), (LHEAD(I),I=37,48),    
     1       (LHEAD(I),I=1,24)
  70    IF (ISWCH.GT.IZERO) GO TO 130   
        IF (L2.EQ.IONE .OR. MX.EQ.IONE) GO TO 100 
        IF (IST.GT.IONE) GO TO 100      
        IX2 = IIRGS(NX+5)     
        NWM = ITWO * IDIV (ITWO,MX,IRF) 
        LL  = IFOUR - NWM     
        NSD = MIN0 (IFOUR+NWM,ISIGD)    
        NWM = NWM + ITEN      
        ISWCH = IONE
        CALL RFORMT (IZERO,NSD,RC(IX2),A(1),N,NWM,NW2,NDEC2,MA(1),IRF)
        IF (MX.EQ.ITWO) GO TO 100       
        IX3 = IIRGS(NX+6)     
        CALL RFORMT (IZERO,NSD,RC(IX3),A(1),N,NWM,NW3,NDEC3,MA(1),IRF)
        GO TO 100   
C         
  80    IST = ITWO  
        IF (KIND(2).EQ.IONE .AND. ARGS(2).EQ.RONE) IST = ITHRE        
        IF (IST.EQ.ITHRE) GO TO 90      
        WRITE (IPRINT,280) (LHEAD(I),I=13,24)     
        IF (ISWCH.GT.IZERO) GO TO 130   
        ISWCH = IONE
        NWM   = 15  
        NWMA  = 15  
        NWMB  = 14  
        NSP11 = 7   
        NWMC  = 15  
        GO TO 100   
C         
  90    WRITE (IPRINT,290) (LHEAD(I),I=13,36)     
        IF (ISWCH.GT.IZERO) GO TO 130   
        ISWCH = IONE
        NWM   = 13  
        NWMA  = 13  
        NWMB  = 13  
        NWMC  = 13  
 100    CALL RFORMT (IZERO,NSD,RC(IX ),A(1),N,NWM,NW1,NDEC1,MA(1),IRF)
        CALL RFORMT (IZERO,ISIGD,RC(IY),A(1),N,NWMC-LL,NW4,NDEC4,     
     1      MA(1),IRF)        
        CALL RFORMT (IZERO,ISIGD,A(JSUBPV),RC(1),N,NWMA-LL,NW5,NDEC5, 
     1      MA(1),IRF)        
        CALL RFORMT (IZERO,ISIGD,A(JSUBSY),RC(1),N,NWMB-LL,NW6,NDEC6, 
     1      MA(1),IRF)        
        CALL RFORMT (IZERO,ISIGD,A(JSUBRS),RC(1),N,17-LL,NW7,NDEC7,   
     1      MA(1),IRF)        
        IF (KIND(2).EQ.IONE) GO TO 110  
        CALL RFORMT (0,IFOUR,RC(IW),A(1),N,NSP11+IONE,NW9,NDEC9,      
     1      MA(1),IRF)        
        IF (NW9.GT.NSP11) CALL RFORMT (0,IFOUR,RC(IW),A(1),N,         
     1      NSP11-IFIVE,NW9,NDEC9,MA(1),IRF)      
        GO TO 120   
 110    CALL RFORMT (IZERO,IFOUR,ARGS(2),A(1),1,NSP11,NW9,NDEC9,      
     1      MA(1),IRF)        
        CALL RFORMT (IONE,IFOUR,A,ARGS(2),NSP11+2-NW9,1,NW9,NDEC9,    
     1      MA(98),IRF)       
 120    WT = ARGS(2)
 130    DO 200 I=NBOT,NTOP    
          CALL RFORMT (IONE,NSD,A,RC(IX),NWM+ITWO-NW1,IONE,NW1,NDEC1, 
     1        MA(1),IRF)      
          IF (IST.GT.IONE) GO TO 140    
          IF (L2.EQ.IONE .OR. MX.EQ.IONE) GO TO 140         
          CALL RFORMT (IONE,NSD,A,RC(IX2),NWM+ITWO-NW2,IONE,NW2,NDEC2,
     1        MA(NWM+3),IRF)  
          IX2 = IX2 + IONE    
          IF (MX.EQ.ITWO) GO TO 140     
          CALL RFORMT (IONE,NSD,A,RC(IX3),12-NW3,IONE,NW3,NDEC3,      
     1        MA(25),IRF)     
          IX3 = IX3 + IONE    
 140      CALL RFORMT (IONE,ISIGD,A,RC(IY),NWMC+ITWO-LL-NW4,IONE,NW4, 
     1         NDEC4,MA(4*LL+21),IRF)   
          CALL RFORMT (IONE,ISIGD,A,A(JSUBPV),NWMA+ITWO-LL-NW5,IONE,NW5,        
     1        NDEC5,MA(3*LL+41),IRF)    
          CALL RFORMT (IONE,ISIGD,A,A(JSUBSY),NWMB+ITWO-LL-NW6,IONE,NW6,        
     1        NDEC6,MA(2*LL+60),IRF)    
          CALL RFORMT (IONE,ISIGD,A,A(JSUBRS),19-LL-NW7,IONE,NW7,NDEC7,         
     1        MA(LL+79),IRF)  
          IF (KIND(2).EQ.IONE) GO TO 150
          CALL RFORMT (IONE,IFOUR,A,RC(IW),NSP11+ITWO-NW9,IONE,NW9,   
     1        NDEC9,MA(98),IRF)         
          WT = RC(IW)         
          IW = IW + IONE      
 150      IF (WT.GT.RZERO) STDRES = FDIV (A(JSUBRS),FSQRT   
     1      (FDIV(SSQ,WT,IND) - A(JSUBSY)**2),IND)
          IF (WT.LE.RZERO) STDRES = RZERO         
          GO TO (160,170,180), IST      
 160      WRITE (IPRINT,250) I, (MA(II),II=1,97), STDRES,   
     1         (MA(II),II=98,108)       
          GO TO 190 
C         
 170      WRITE (IPRINT,260) I ,(MA(II),II=21,37), (MA(II),II=41,57), 
     1         (MA(II),II=60,75), STDRES, (MA(II),II=98,106)
          GO TO 190 
C         
 180      WRITE (IPRINT,270) I, (MA(II),II=21,35), (MA(II),II=41,55), 
     1         (MA(II),II=60,74), STDRES, (MA(II),II=1,13)  
 190      IX = IX + IONE      
          IY = IY + IONE      
          JSUBPV = JSUBPV + IONE        
          JSUBSY = JSUBSY + IONE        
          JSUBRS = JSUBRS + IONE        
 200    CONTINUE    
        IF (IPG.EQ.IPAGE) GO TO 210     
        CALL PAGE (IFOUR)     
        NBOT = NTOP + IONE    
        NTOP = NTOP + LENINC  
        IF (NTOP.GT.N) NTOP = N         
 210  CONTINUE      
      LINE = NTOP - NBOT +IONE + MCNT + ITHRE     
      MCN = LINE    
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 220  FORMAT (/1X/10X,10HINDEP VAR.,10X,8HRESPONSE,12X,9HPREDICTED,9X,
     1 12HSTD. DEV. OF,25X,4HSTD./      
     2 2X,3HROW,5X,12A1,8X,12A1, 8X,8HRESPONSE, 9X,14HPRED. RESPONSE, 
     3 7X,9HRESIDUALS,8X,4HRES.,2X,7HWEIGHTS/)    
 230  FORMAT (1HO/9X,21HINDEPENDENT VARIABLES,8X,8HRESPONSE,10X,      
     1 9HPREDICTED,6X,12HSTD. DEV. OF,22X,4HSTD./ 
     2 2X,3HROW,1X,12A1,1X,12A1,7X,12A1,6X,8HRESPONSE,6X,   
     3 14HPRED. RESPONSE,5X,9HRESIDUALS,7X,4HRES.,3X,7HWEIGHTS/)      
 240  FORMAT (/1X/13X,21HINDEPENDENT VARIABLES,11X,8HRESPONSE,8X,     
     1 9HPREDICTED,4X,12HSTD. DEV. OF,19X,4HSTD./ 
     2 2X,3HROW,1X,12A1,1X,12A1,1X,12A1,1X,12A1,4X,8HRESPONSE,4X,     
     3 14HPRED. RESPONSE,3X,9HRESIDUALS,6X,4HRES.,3X,7HWEIGHTS/)      
 250  FORMAT (1X,I4,97A1,0PF7.2,11A1)   
 260  FORMAT (1X,I5,50A1,F7.2,9A1)      
 270  FORMAT (1X,I4,45A1,F7.2,2X,13A1)  
 280  FORMAT (/1X/11X,8HRESPONSE,8X,9HPREDICTED,7X,         
     1 12HSTD. DEV. OF,4X,4HSTD./       
     2 3X,3HROW,3X,12A1,6X,8HRESPONSE,7X,14HPRED. RESPONSE, 
     3 3X,13HRES.  WEIGHTS /) 
 290  FORMAT (/1X/8X,8HRESPONSE,7X,9HPREDICTED,4X,
     1 12HSTD. DEV. OF,5X,4HSTD.,3X,10HINDEP VAR./
     2 2X,3HROW,3X,12A1,3X,8HRESPONSE,5X,14HPRED. RESPONSE, 
     3 3X,4HRES.,3X,12A1/)    
C         
C     ==================================================================        
C         
      END 
*ORTPLT
      SUBROUTINE ORTPLT (ISUBRS,ISUBSY,N,SSQ,ISUBPV,ISBY,IB,IXA,IWS,IBR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. ORTPLT V 7.00  4/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM IS USED BY ORTHO TO GENERATE PLOTS
C
C     ISUBRS SUBSCRIPT FOR RESIDUALS.
C     ISUBSY SUBSCRIPT POINTS TO STD. DEV. OF PRED. VALUES IN VECTOR A
C     N      NUMBER OF VARIABLES
C     SSQ    SQUARE OF RESIDUAL STANDARD DEVIATION
C     ISUBPV IS SUBSCRIPT FOR PREDICTED VALUES IN VECTOR A
C     IB     SCRATCH VECTOR NEEDED BY THIS PROCEDURE
C     IXA    SUSCRIPT FOR VARIABLE X IN VECTOR RC
C     IWS    SUBSCRIPT FOR WEIGHTS IN VECTOR RC
C     IBR    SWITCH TO CONTROL OUTPUT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG,MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1969.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IB(*)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
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
      REAL             SSQ
      REAL             WT, X, YMAX, YMIN, Z
      REAL             FDIV, FSQRT
      REAL             SPCA
C
C     ...................................................................
C
      CHARACTER        IB*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /  0.3 /
C
C     ==================================================================
C
      ISUBY  = ISBY
      IBRIEF = IBR
      IW     = IWS
      IWST   = IONE
      IF (KIND(2).EQ.IZERO) GO TO 10
      IWST   = ITWO
      WT     = ARGS(2)
  10  JSUBRS = ISUBPV + N
      KSUBRS = ISUBRS
      JSUBSY = ISUBSY
      NZW    = N
C
C       COMPUTE PREDICTED RESPONSES AND STORE IN PLACE OF MATRIX Q
C          WHICH IS USED BY LSQ.
C
        JSUBPV = ISUBPV
        MSUBRS = ISUBRS
        JSUBY = ISUBY
        DO 20 I=1,N
          A(JSUBPV) = RC(JSUBY) - A(MSUBRS)
          JSUBPV = JSUBPV + IONE
          JSUBY = JSUBY + IONE
          MSUBRS = MSUBRS + IONE
  20    CONTINUE
C
C     COMPUTE PRINTING POSITION OF Y AXIS AND STORE IN A VECTOR.
C
      DO 50 I=1,N
        IF (IWST.EQ.ITWO) GO TO 30
        WT = RC(IW)
        IW = IW + IONE
        IF (WT.NE.RZERO) GO TO 30
        A(JSUBRS) = 25.0
        NZW = NZW - IONE
        GO TO 40
  30    X = FDIV (SSQ,WT,IND)
        Z = FDIV (A(KSUBRS),FSQRT(X-A(JSUBSY)**2),IND)
        IZ = FDIV (Z,SPCA,IND)
        IF (Z.GT.RZERO .AND. AMOD(Z,SPCA).NE.RZERO) IZ = IZ + IONE
        A(JSUBRS) = IZ + 12
        IF (A(JSUBRS).LE.RZERO) A(JSUBRS) = -IONE
        IF (A(JSUBRS).GT.24.0) A(JSUBRS) = -24
  40    JSUBSY = JSUBSY + IONE
        JSUBRS = JSUBRS + IONE
        KSUBRS = KSUBRS + IONE
  50  CONTINUE
C
      JSUBPV = ISUBPV
      IST = IONE
      IW = IWS
      DO 100 I=1,N
        IF (IWST.EQ.ITWO) GO TO 60
        IF (RC(IW).EQ.RZERO) GO TO 90
  60    IF (IST.EQ.ITWO) GO TO 70
        YMAX = A(JSUBPV)
        YMIN = A(JSUBPV)
        IST = ITWO
        GO TO 90
  70    IF (YMIN.LE.A(JSUBPV)) GO TO 80
        YMIN = A(JSUBPV)
        GO TO 90
  80    IF (YMAX.LT.A(JSUBPV)) YMAX = A(JSUBPV)
  90    IW = IW + IONE
        JSUBPV = JSUBPV + IONE
 100  CONTINUE
C
      IF (LWIDE.LT.LWC) GO TO 110
C
C     CALL PROCEDURE TO DO 4 PLOTS/PAGE.
C
      CALL LSPLT4 (YMAX,YMIN,N,NZW,ISUBPV,IB,IXA,IWS,IBRIEF)
      RETURN
C
C     ..................................................................
C
C     CALL PROCEDURE TO DO 2 PLOTS/PAGE.
C
 110  CALL LSPLT2 (YMAX,YMIN,N,NZW,ISUBPV,IB,IXA,IWS,IBRIEF)
      RETURN
C
C     ==================================================================
C
      END
*OUTLOF
      SUBROUTINE OUTLOF (SS,NDFREG,NDFERR,NOFNZW,NLINES,MXLINE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OUTLOF V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT LACK OF FIT ANALYSIS OF VARIANCE.
C
C     PROGRAM UNIT PRODUCES 15 LINES OF PRINTING, UNLESS THERE IS A
C       SINGULARITY IN WHICH CASE THE NUMBER OF LINES PRINTED IS 3 OR 4.
C
C     VECTOR SS(.) CONTAINS NECESSARY SUMS OF SQUARES.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - DECEMBER, 1977.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION LINEPR(30)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             SS(*)
      REAL             MS(4)
      REAL             F, FSL 
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LINEPR*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     IF NXSS IS CHANGED, THEN CORRESPONDING CHANGE MUST BE MADE
C        IN 16A1 IN FORMATS 80, 90, 100, 110, AND 120.
C     IF NXMS IS CHANGED, THEN CORRESPONDING CHANGE MUST BE MADE
C        IN 14A1 IN FORMATS 80, 90, 100, AND 110. 
C
      DATA NXSS   / 16 /
      DATA NXMS   / 14 /
C
      DATA LINECH / 39 /
C
C     ==================================================================
C
C     CHECK ON SINGULARITIES. 
C
      NDFRES = NOFNZW - NDFREG
C
      IF (NDFERR.GT.IZERO) GO TO 20
      NLINES = NLINES + ITWO
      IF (NLINES.LE.MXLINE) GO TO 10
      CALL PAGE (4) 
      NLINES = ITHRE
  10  WRITE (IPRINT,70)
      WRITE (IPRINT,70)
      WRITE (IPRINT,130)
      RETURN
C
C     ..................................................................
C
  20  IF (NDFERR.LT.NDFRES) GO TO 40
      NLINES = NLINES + ITHRE 
      IF (NLINES.LE.MXLINE) GO TO 30
      CALL PAGE (4) 
      NLINES = ITHRE
  30  WRITE (IPRINT,70)
      WRITE (IPRINT,70)
      WRITE (IPRINT,140)
      RETURN
C
C     ..................................................................
C
C     COMPUTE MEAN SQUARES.
C
  40  NDFLOF = NDFRES - NDFERR
      MS(1)  = FDIV (SS(1),FLOAT(NDFREG),IND)
      MS(2)  = FDIV (SS(2),FLOAT(NDFRES),IND)
      MS(3)  = FDIV (SS(3),FLOAT(NDFLOF),IND)
      MS(4)  = FDIV (SS(4),FLOAT(NDFERR),IND)
C
C     PRINT TITLE.
C
      NLINES = NLINES + 15
      IF (NLINES.LE.MXLINE .OR. ISBFT.EQ.IONE) GO TO 50
      CALL PAGE (4) 
      NLINES = 17
  50  WRITE (IPRINT,70)
      WRITE (IPRINT,70)
      WRITE (IPRINT,60)
C
      LINEMX = NCW - IONE
      WRITE (IPRINT,70) (LA(LINECH),I=1,LINEMX)
C
C     COMPUTE CONSTANTS FOR RFORMT.
C
      CALL RFORMT (0,ISIGD,SS,RC(1), 5,NXSS,NWSS,NDSS,LINEPR( 1),IRF) 
      NBSS = NXSS - NWSS
      CALL RFORMT (0,ISIGD,MS,RC(1), 4,NXMS,NWMS,NDMS,LINEPR( 1),IRF) 
      NBMS = NXMS - NWMS
C
C     IF NWIDTH, BELOW, EXCEEDS DIMENSION OF LINEPR(.), THEN
C       LINEPR(.) MUST BE REDIMENSIONED.
C
      NWIDTH = NXSS + NXMS
C
C     PRINT REGRESSION DF, SUM OF SQUARES AND MEAN SQUARE.
C
      CALL RFORMT (1,ISIGD,RC,SS(1),NBSS, 1,NWSS,NDSS,LINEPR( 1),IRF) 
      CALL RFORMT (1,ISIGD,RC,MS(1),NBMS, 0,NWMS,NDMS,LINEPR(17),IRF) 
      WRITE (IPRINT,80) NDFREG, (LINEPR(I),I=1,NWIDTH)
C
C     PRINT RESIDUAL DF, SUM OF SQUARES AND MEAN SQUARE.
C
      CALL RFORMT (1,ISIGD,RC,SS(2),NBSS, 1,NWSS,NDSS,LINEPR( 1),IRF) 
      CALL RFORMT (1,ISIGD,RC,MS(2),NBMS, 0,NWMS,NDMS,LINEPR(17),IRF) 
      WRITE (IPRINT,90) NDFRES, (LINEPR(I),I=1,NWIDTH)
      WRITE (IPRINT,70) (LA(LINECH),I=1,LINEMX)
C
C     PRINT LACK OF FIT DF, SUM OF SQUARES, MEAN SQUARE, F, AND P(F). 
C
      CALL RFORMT (1,ISIGD,RC,SS(3),NBSS, 1,NWSS,NDSS,LINEPR( 1),IRF) 
      CALL RFORMT (1,ISIGD,RC,MS(3),NBMS, 0,NWMS,NDMS,LINEPR(17),IRF) 
      F = FDIV (MS(3),MS(4),IND)
      CALL QFORF (FLOAT(NDFLOF),FLOAT(NDFERR),F,FSL)
      WRITE (IPRINT,100) NDFLOF, (LINEPR(I),I=1,NWIDTH), F, FSL
C
C     PRINT ERROR DF, SUM OF SQUARES AND MEAN SQUARE.
C
      CALL RFORMT (1,ISIGD,RC,SS(4),NBSS, 1,NWSS,NDSS,LINEPR( 1),IRF) 
      CALL RFORMT (1,ISIGD,RC,MS(4),NBMS, 0,NWMS,NDMS,LINEPR(17),IRF) 
      WRITE (IPRINT,110) NDFERR, (LINEPR(I),I=1,NWIDTH)
      WRITE (IPRINT,70) (LA(LINECH),I=1,LINEMX)
C
C     PRINT TOTAL DF AND SUM OF SQUARES.
C
      CALL RFORMT (1,ISIGD,RC,SS(5),NBSS, 1,NWSS,NDSS,LINEPR( 1),IRF) 
      WRITE (IPRINT,120) NOFNZW, (LINEPR(I),I=1,NXSS)
      WRITE (IPRINT,70) (LA(LINECH),I=1,LINEMX)
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  60  FORMAT (20X,32HLACK OF FIT ANALYSIS OF VARIANCE/5X,
     1 57HEVIDENCE OF LACK OF FIT HERE MAY INVALIDATE OTHER RESULTS
     2       //1X,6HSOURCE,8X,4HD.F.,4X,14HSUM OF SQUARES,5X,
     3           12HMEAN SQUARES,4X,7HF-VALUE,3X,4HP(F) )
  70  FORMAT (1X,71A1)
  80  FORMAT (1X,11HREGRESSION ,1X,I5,3X,16A1,3X,14A1)
  90  FORMAT (1X,11HRESIDUAL   ,1X,I5,3X,16A1,3X,14A1)
 100  FORMAT (1X,11HLACK OF FIT,1X,I5,3X,16A1,3X,14A1,3X,F7.3,3X,F5.3)
 110  FORMAT (1X,11HERROR      ,1X,I5,3X,16A1,3X,14A1)
 120  FORMAT (1X,11HTOTAL      ,1X,I5,3X,16A1)
 130  FORMAT (1X,65HTHERE ARE NO REPLICATIONS FOR A LACK OF FIT ANALYSIS
     1 OF VARIANCE.)
 140  FORMAT (1X,60HTHE RESIDUAL SUM OF SQUARES IS DUE ENTIRELY TO REPLI
     1CATIONS./4X,48HA LACK FIT ANALYSIS OF VARIANCE IS NOT POSSIBLE.)
C
C     ==================================================================
C
      END 
*OUTSPA
      SUBROUTINE OUTSPA (DF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OUTSPA V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO PRINT ANALYSIS OF VARIANCE FOR SPLIT-PLOT DESIGN.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -       MAY, 1978.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION INTDF(7), LINEPR (28)
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
      REAL             DF(*)
      REAL             PF
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LINEPR*1  
      CHARACTER        LHEAD*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX / 14 /
C
C     ==================================================================
C
C     COMPUTE DEGREES OF FREEDOM AS INTEGERS.
C
      DO 10 I=1,7
        INTDF(I) = DF(I)
  10  CONTINUE
C
      INTR = INTDF(1) + IONE
      INTW = INTDF(2) + IONE
      INTS = INTDF(4) + IONE
C
C
C     PRINT TITLE.
C
      CALL PAGE (IFOUR)
      CALL HEADS (IARGS(1),IFOUR,IZERO,IONE)
C
      WRITE (IPRINT, 20)
      WRITE (IPRINT, 30) NRMAX, (LHEAD(I),I= 1,12)
      WRITE (IPRINT, 40)  INTR, (LHEAD(I),I=13,24)
      WRITE (IPRINT, 50)  INTW, (LHEAD(I),I=25,36)
      WRITE (IPRINT, 60)  INTS, (LHEAD(I),I=37,48)
C
      CALL RFORMT (0,ISIGD,A(1),RC(1),7,NX,NWSS,NDSS,LINEPR,IRF)
      CALL RFORMT (0,ISIGD,A(8),RC(1),6,NX,NWMS,NDMS,LINEPR,IRF)
      NBSS = NX - NWSS
      NBMS = NX - NWMS
C
      WRITE (IPRINT, 70)
      WRITE (IPRINT, 80) (LA(39),I=1,71)
C
C     PRINT REPLICATES LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 1),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A( 8),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      WRITE (IPRINT, 90) ( LHEAD(I),I=13,24), INTDF(1),
     1                   (LINEPR(J),J= 1,28)
C
C     PRINT WHOLE-PLOTS LINE. 
C
      CALL RFORMT (1,ISIGD,RC,A( 2),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A( 9),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      CALL QFORF (DF(2),DF(3),A(14),PF) 
      WRITE (IPRINT,100) ( LHEAD(I),I=25,36), INTDF(2),
     1                   (LINEPR(J),J= 1,28), A(14), PF
C
C     PRINT R X W INTERACTION LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 3),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A(10),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      WRITE (IPRINT,110) INTDF(3), (LINEPR(I),I=1,28)
C
C     PRINT SPLIT-PLOT LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 4),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A(11),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      CALL QFORF (DF(4),DF(6),A(15),PF) 
      WRITE (IPRINT,120) ( LHEAD(I),I=37,48), INTDF(4),
     1                   (LINEPR(J),J= 1,28), A(15), PF
C
C     PRINT W X S INTERACTION LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 5),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A(12),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      CALL QFORF (DF(5),DF(6),A(16),PF) 
      WRITE (IPRINT,130) INTDF(5), (LINEPR(I),I=1,28), A(16), PF
C
C     PRINT RESIDUAL LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 6),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      CALL RFORMT (1,ISIGD,RC,A(13),NBMS,0,NWMS,NDMS,LINEPR(15),IRF)
      WRITE (IPRINT,140) INTDF(6), (LINEPR(I),I=1,28)
C
      WRITE (IPRINT, 80) (LA(39),I=1,71)
C
C     PRINT TOTAL LINE.
C
      CALL RFORMT (1,ISIGD,RC,A( 7),NBSS,0,NWSS,NDSS,LINEPR( 1),IRF)
      WRITE (IPRINT,150)  INTDF(7), (LINEPR(I),I=1,14)
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  20  FORMAT (9X,55HANALYSIS OF VARIANCE FOR SPLIT-PLOT EXPERIMENTAL DES
     1IGN/
     2 2X,69HMODEL IS Y(IJK) = MU + R(I) + W(J) + ETA(IJ) + S(K) +WS(JK)
     3 + E(IJK),/4X,
     4 66HWHERE ETA AND E ARE RANDOM AND MU, W, S, AND WS ARE FIXED EFFE
     5CTS./)
  30  FORMAT (18X,I4,1X,19HMEASUREMENTS (Y) IN,1X,12A1)
  40  FORMAT (18X,I4,1X,19HREPLICATES   (R) IN,1X,12A1)
  50  FORMAT (18X,I4,1X,19HWHOLE-PLOTS  (W) IN,1X,12A1)
  60  FORMAT (18X,I4,1X,19HSPLIT-PLOTS  (S) IN,1X,12A1)
C
  70  FORMAT (/1X,6HSOURCE,13X,4HD.F.,2X,14HSUM OF SQUARES, 
     1   4X,12HMEAN SQUARES,2X,7HF-RATIO,3X,4HP(F))
  80  FORMAT (1X,71A1)
C
  90  FORMAT (1X,3H(R),1X,12A1,I7,2X,14A1,2X,14A1)
 100  FORMAT (1X,3H(W),1X,12A1,I7,2X,14A1,2X,14A1,2X,F7.3,2X,F5.3)
 110  FORMAT (1X,17HR X W INTERACTION,2X,I4,2X,14A1,2X,14A1)
 120  FORMAT (1X,3H(S),1X,12A1,I7,2X,14A1,2X,14A1,2X,F7.3,2X,F5.3)
 130  FORMAT (1X,17HW X S INTERACTION,2X,I4,2X,14A1,2X,14A1,2X,F7.3,
     1     2X,F5.3) 
 140  FORMAT (1X,8HRESIDUAL,11X,I4,2X,14A1,2X,14A1)
 150  FORMAT (1X,5HTOTAL,14X,I4,2X,14A1)
C
C     ==================================================================
C
      END 
*OWPRAV
      SUBROUTINE OWPRAV (K,M1,M3,M4,M5,MA,NB,ND,NW)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRAV V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT ANALYSIS OF VARIANCE FOR ONEWAY INSTRUCTION.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER, 1977.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(72), NB(*), ND(*), NW(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
       INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             ALPHA
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ALPHA / 0.10 /
C
C     ==================================================================
C
C     PRINT ANALYSIS OF VARIANCE.
C
      DO 10 I=1,26
        MA(I) = LA(39)
  10  CONTINUE
C
      WRITE (IPRINT,70) NRMAX,(LHEAD(I),I=1,12),K,(LHEAD(J),J=13,24), 
     1                   (MA(I),I=1,26),(MA(J),J=1,26)
C
      IF (A(14)-ALPHA) 20,20,30
  20  CALL RFORMT ( 0,ISIGD,A    ,RC(1),5,15,NW(1),ND(1),MA(1),IRF)
      CALL RFORMT ( 0,ISIGD,A( 6),RC(1),4,15,NW(2),ND(2),MA(1),IRF)
      GO TO 40
  30  A(97) = A(1)
      A(98) = A(4)
      A(99) = A(5)
      CALL RFORMT ( 0,ISIGD,A(97),RC(1),3,15,NW(1),ND(1),MA(1),IRF)
      A(98) = A(6)
      A(99) = A(9)
      CALL RFORMT ( 0,ISIGD,A(98),RC(1),2,15,NW(2),ND(2),MA(1),IRF)
  40  NB(1) = 15 - NW(1)
      NB(2) = 15 - NW(2)
      CALL RFORMT ( 1,ISIGD,RC,A(1),NB(1),0,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC,A(6),NB(2),0,NW(2),ND(2),MA(16),IRF)
      WRITE (IPRINT,80) M1,(MA(I),I=1,30),A(11),A(14)
C
      IF (K.LT.ITHRE) GO TO 60
      IF (A(14)-ALPHA) 50,50,60
  50  CALL RFORMT ( 1,ISIGD,RC,A(2),NB(1),0,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC,A(7),NB(2),0,NW(2),ND(2),MA(16),IRF)
      WRITE (IPRINT,90)    (MA(I),I=1,30),A(12),A(15)
C
      CALL RFORMT ( 1,ISIGD,RC,A(3),NB(1),0,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC,A(8),NB(2),0,NW(2),ND(2),MA(16),IRF)
      WRITE (IPRINT,100) M3,(MA(I),I=1,30),A(13),A(16)
C
  60  CALL RFORMT ( 1,ISIGD,RC,A(4),NB(1),0,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC,A(9),NB(2),0,NW(2),ND(2),MA(16),IRF)
      WRITE (IPRINT,110) M4,(MA(I),I=1,30)
C
      CALL RFORMT ( 1,ISIGD,RC,A(5),NB(1),0,NW(1),ND(1),MA( 1),IRF)
      WRITE (IPRINT,120) M5,(MA(I),I=1,15)
C
C     (2)   PRINT KRUSKAL-WALLIS TEST
C
      WRITE (IPRINT,130) A(20)
C
      RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (1)   ANOVA.
C
  70  FORMAT (9X,19HONEWAY ANALYSIS OF ,I4,17H MEASUREMENTS IN ,12A1/5X,
     1 17H CLASSIFIED INTO ,I3,24H GROUPS WITH NUMBERS IN ,12A1//
     2  23X,26A1/23X,26HI  ANALYSIS OF VARIANCE  I/23X,26A1//
     3   1X,6HSOURCE,10X,4HD.F.,3X,15HSUMS OF SQUARES,4X,
     4  12HMEAN SQUARES,3X,7HF RATIO,2X,5HPROB./) 
  80  FORMAT (1X,14HBETWEEN GROUPS,1X,I4,4X,15A1,2X,15A1,1X,F7.3,F8.3)
  90  FORMAT (3X,5HSLOPE,13X,1H1,2X,15A1,2X,15A1,3X,F7.3,F6.3)
 100  FORMAT (3X,10HABOUT LINE,5X,I4,2X,15A1,2X,15A1,3X,F7.3,F6.3)
 110  FORMAT (1X,13HWITHIN GROUPS,2X,I4,4X,15A1,2X,15A1)
 120  FORMAT (1X,5HTOTAL, 9X,I5,4X,15A1)
C
C     (2)   KRUSKAL-WALLIS.
C
 130  FORMAT (/1X,9X,53HKRUSKAL-WALLIS RANK TEST FOR DIFFERENCE BETWEEN 
     1MEANS/16X,36HSIGNIFICANCE LEVEL IS APPROXIMATELY ,F5.3/)
C
C     ================================================================
C
      END 
*OWPRBP
      SUBROUTINE OWPRBP (K,MA,MT,NB,ND,NW,NSD,A3,B2,B3)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRBP V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT BOX PLOTS FOR ONEWAY INSTRUCTION..
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
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*), MB(14), MC(39), NB(*), ND(*), NSPOS(5), NW(*) 
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
      REAL             B2(*), B3(*) 
      REAL             A3(*)
      REAL             SV(6)
C
C     .................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
      CHARACTER        MB*1, MC*1
C
C
C     ==================================================================
C
C     PRINT BOX PLOTS.
C
      CALL PAGE (IFOUR)
C
      DO 10 I=1,15
        MA(I) = LA(39)
  10  CONTINUE
C
      WRITE (IPRINT,80) (MA(I),I=1,15),(MA(J),J=1,15)
C
C     COMPUTE SCALE USING MINIMUM, MID-RANGE, AND MAXIMUM
C
      CALL RFORMT ( 0,ISIGD,A(26),RC(1), 3,13,NW(1),ND(1),MC( 1),IRF) 
      NB(1) = 13 - NW(1)
      CALL RFORMT ( 1,ISIGD,RC,A(26),NB(1), 1,NW(1),ND(1),MC( 1),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(28),NB(1), 1,NW(1),ND(1),MC(14),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(27),NB(1), 1,NW(1),ND(1),MC(27),IRF) 
C
      WRITE (IPRINT,90) (MC(I),I=1,39)
      NLINE = 7
C
      IXSUB = IONE
      CALL BLNKIN (MB(1),14)
C
      DO 60 I=1,K
        N = B3(I)
        NLINE = NLINE + ITWO
        IF (N.GE.IFIVE) NLINE = NLINE + IFOUR
        IF (NLINE.LE.60) GO TO 20
          CALL PAGE (IZERO)
          WRITE (IPRINT,70)
          WRITE (IPRINT,90) (MC(II),II=1,39)
          NLINE = 6 
          IF (N.GE.IFIVE) NLINE = NLINE + IFOUR
  20    WRITE (IPRINT,70)
        CALL RFORMT (MT,NSD,A,B2(I),NB(6),1,NW(6),ND(6),MB( 1),IRF)
        CALL RFORMT ( 9,  5,A,B3(I),    1,0,IFOUR, IZERO,MB( 9),IRF)
        IF (N.GE.IFIVE) GO TO 40
        CALL PLTPOS (A3(IXSUB),N,A(26),A(27),51,NSPOS(1))
        CALL BLNKIN (MA(1),51)
C
        DO 30 J=1,N 
          JJ = NSPOS(J)
          MA(JJ) = LA(34)
  30    CONTINUE
C
        WRITE (IPRINT,70) (MB(II),II=1,14), (MA(JJ),JJ=1,51)
        GO TO 50
  40    CALL OSCRWL (A3(IXSUB),N,SV(1),IRF)
        CALL PLTPOS (SV(2),5,A(26),A(27),51,NSPOS(1))
        CALL BOXPLT (MB(1),14,MA(1),NSPOS(1),5)
  50    IXSUB = IXSUB + N
  60  CONTINUE
      RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  70  FORMAT (1X,71A1)
  80  FORMAT (28X,15A1/28X,15HI  BOX PLOTS  I/28X,15A1/)
  90  FORMAT ( 9X,13A1,12X,13A1,12X,13A1/3X,11HGROUP   NO.,1X,
     1   51HO----+----I----+----I----O----I----+----I----+----O)
C
C     ================================================================
C
      END 
*OWPRCL
      SUBROUTINE OWPRCL(K,M6,MA,MT,NB,ND,NW,NSD,B2,B3,B4,B5,B6,B7,B8,B9)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRCL V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE AND PRINT CONFIDENCE LIMITS AND STANDARD DEVIATION
C        OF MEAN FOR ONEWAY.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - APRIL, 1978.
C                   CURRENT VERSION - APRIL, 1992
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*), NB(*), ND(*), NW(*)
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
      REAL               B2(*), B3(*), B4(*), B5(*)
      REAL               B6(*), B7(*), B8(*), B9(*)
      REAL               FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER          LA*1
      CHARACTER          MA*1
C
C
C     ==================================================================
C
C     COMPUTE CONFIDENCE LIMITS AND STANDARD DEVIATION OF MEAN.
C
C        REPLACE SUM RANKS IN B6() BY STANDARD DEVIATION OF MEAN.
C        REPLACE MINIMUM IN   B7() BY LOWER CONFIDENCE LIMIT
C        REPLACE MAXIMUM IN   B8() BY UPPER CONFIDENCE LIMIT
C
      IF (K.GT.14 .AND. NCRT.EQ.IZERO) CALL PAGE (IFOUR)
      IF (NCRT.NE.IZERO) CALL PAGE (0)
      A(40) = FDIV (B5(1),FSQRT(B3(1)),IND)
      A(50) = A(40) 
      LENCT = ITHRE
      DO 20 I=1,K
        IF (B3(I)-RONE) 20,20,10
  10    B6(I) = FDIV (B5(I),FSQRT(B3(I)),IND)
        IF (B6(I).LE.A(40)) A(40) = B6(I)
        IF (B6(I).GE.A(50)) A(50) = B6(I)
        CALL TPCTPT (B3(I)-RONE,A(72))
        B7(I) = B4(I) - B6(I)*A(72)
        B8(I) = B4(I) + B6(I)*A(72)
  20  CONTINUE
C
      DO 40 I=1,K
        IF (B3(I)-RONE) 30,30,50
  30    B7(I) = B7(M6)
  40  CONTINUE
C
      B8(I) = B8(M6)
  50  L = K + ITHRE 
      DO 60 I=1,K
        B9 (I) = B5(I)
  60  CONTINUE
C
      DO 70 I=1,ITHRE
        M = I + K
        B9(M) = A(I+30)
  70  CONTINUE
C
      CALL RFORMT ( 0,ISIGD,B9,RC(1),L    ,13,NW(1),ND(1),MA( 1),IRF) 
      DO 80 I=1,K
        B9(I) = B6(I)
  80  CONTINUE
C
      DO 90 I=1,3
        M = I + K
        B9(M) = A(I+33)
  90  CONTINUE
C
      CALL RFORMT ( 0,ISIGD,B9,RC(1),L    ,13,NW(2),ND(2),MA( 1),IRF) 
      DO 100 I=1,K
        B9 (I) = B7(I)
 100  CONTINUE
C
      DO 110 I=1,ITHRE
        M = I + K
        B9(M) = A(I+40)
 110  CONTINUE
C
      CALL RFORMT ( 0,ISIGD,B9,RC(1),L    ,13,NW(3),ND(3),MA( 1),IRF) 
      DO 120 I=1,K
        B9 (I) = B8(I)
 120  CONTINUE
C
      DO 130 I=1,ITHRE
        M = I + K
        B9(M) = A(I+43)
 130  CONTINUE
C
      CALL RFORMT ( 0,ISIGD,B9,RC(1),L    ,13,NW(4),ND(4),MA( 1),IRF) 
      NB(1) = 13 - NW(1)
      NB(2) = 13 - NW(2)
      NB(3) = IDIV (27-NW(3)-NW(4),ITWO,IND)
C
      L = 36 + NB(3) + NW(3)
      MA(L+1) = LA(45)
      MA(L+2) = LA(30)
      MA(L+3) = LA(25)
      MA(L+4) = LA(45)
      L = L + IFIVE 
      IEND = L + NW(4) - IONE 
C
      WRITE (IPRINT,180)
      DO 160 I=1,K
        MA(22) = LA(45)
        MA(36) = LA(45)
        CALL RFORMT (MT,NSD  ,A ,B2(I),NB(6), 1,NW(6),ND(6),MA( 1),IRF)
        IF (B3(I)-RONE) 140,140,150
 140      WRITE (IPRINT,190) (MA(J),J=1,8)
          GO TO 155 
 150    IF (B5(I).LE.A(24)) MA(22) = LA(22)
        IF (B5(I).GE.A(25)) MA(22) = LA(18)
        IF (B6(I).LE.A(40)) MA(36) = LA(22)
        IF (B6(I).GE.A(50)) MA(36) = LA(18)
        CALL RFORMT ( 1,ISIGD,A ,B5(I),NB(1), 1,NW(1),ND(1),MA( 9),IRF)
        CALL RFORMT ( 1,ISIGD,A ,B6(I),NB(2), 1,NW(2),ND(2),MA(23),IRF)
        CALL RFORMT ( 1,ISIGD,A ,B7(I),NB(3), 0,NW(3),ND(3),MA(37),IRF)
        CALL RFORMT ( 1,ISIGD,A ,B8(I),    0, 0,NW(4),ND(4),MA( L),IRF)
        WRITE (IPRINT,200)(MA(II),II=1,IEND)
 155    IF (NCRT.NE.IZERO) THEN
          LENCT = LENCT + IONE
          IF ( LENCT.GE.LENGTH) THEN
            CALL PAGE (0)
            LENCT = IZERO
          ENDIF
        ENDIF
 160  CONTINUE
C
      DO 170 I=1,50 
        MA(I) = LA(39)
 170  CONTINUE
C
      IF (LENCT + 4.GT.LENGTH) CALL PAGE (0)
      WRITE (IPRINT,210) (MA(I),I=1,50) 
C
      L = 26 + NB(3) + NW(3)
      MA(L+1) = LA(45)
      MA(L+2) = LA(30)
      MA(L+3) = LA(25)
      MA(L+4) = LA(45)
      L = L + IFIVE 
      IEND = L + NW(4) - IONE 
C
      CALL RFORMT ( 1,ISIGD,RC,A(31),NB(1), 1,NW(1),ND(1),MA( 1),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(34),NB(2), 1,NW(2),ND(2),MA(14),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(41),NB(3), 0,NW(3),ND(3),MA(27),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(44),    0, 0,NW(4),ND(4),MA( L),IRF) 
      WRITE (IPRINT,220) (MA(II),II=1,IEND)
C
      CALL RFORMT ( 1,ISIGD,RC,A(32),NB(1), 1,NW(1),ND(1),MA( 1),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(35),NB(2), 1,NW(2),ND(2),MA(14),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(42),NB(3), 0,NW(3),ND(3),MA(27),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(45),    0, 0,NW(4),ND(4),MA( L),IRF) 
      WRITE (IPRINT,230) (MA(II),II=1,IEND)
C
      CALL RFORMT ( 1,ISIGD,RC,A(33),NB(1), 1,NW(1),ND(1),MA( 1),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(36),NB(2), 1,NW(2),ND(2),MA(14),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(43),NB(3), 0,NW(3),ND(3),MA(27),IRF) 
      CALL RFORMT ( 1,ISIGD,RC,A(46),    0, 0,NW(4),ND(4),MA( L),IRF) 
      WRITE (IPRINT,240) (MA(II),II=1,IEND)
      RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 180  FORMAT (/1X,2X,5HGROUP,6X,11HWITHIN S.D.,3X,12HS.D. OF MEAN,
     1   5X,24H95 PCT CONF INT FOR MEAN/)
 190  FORMAT (1X,8A1,3X,
     1  60H*** ESTIMATES ARE NOT AVAILABLE BECAUSE SAMPLE SIZE IS 1 ***)
 200  FORMAT (1X,8A1,3X,14A1,1X,14A1,1X,30A1)
 210  FORMAT (1X,5HMODEL,7X,12A1,3X,12A1,5X,26A1) 
 220  FORMAT (2X,    5HFIXED,5X,13A1,2X,13A1,2X,30A1)
 230  FORMAT (2X,   6HRANDOM,4X,13A1,2X,13A1,2X,30A1)
 240  FORMAT (2X,9HUNGROUPED,1X,13A1,2X,13A1,2X,30A1)
C
C     ================================================================
C
      END 
*OWPRES
      SUBROUTINE OWPRES (K,MA,NB,ND,NW,MT,NSD,B2,B3,B4,B6,B7,B8)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRES V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT ESTIMATES FOR ONEWAY ...
C        GROUP, NUMBER, MEAN, MINIMUM, MAXIMUM, SUM, AND RANKS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORTORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - APRIL, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*), NB(*), ND(*), NW(*)
C
      COMMON /ABCDEF/ LA(74)
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
      REAL             B2(*), B3(*), B4(*)
      REAL             B6(*), B7(*), B8(*)
      REAL             DELTA
      REAL             FLOG10 
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DELTA / 0.0001 /
C
C     ==================================================================
C
C     PRINT ESTIMATES - GROUP,NO.,MEAN,MINIMUM,MAXIMUM,SUM RANKS.
C
      DO 10 I=1,15
        MA(I) = LA(39)
  10  CONTINUE
C
      WRITE (IPRINT,90) (MA(I),I=1,15),(MA(J),J=1,15)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,130)
C
C     DETERMINE IF ALL TAGS IN B2(.) ARE INTEGERS.  MT = 7, NO
C                                                        9, YES
      MT = 9
      DO 30 I=1,K
        A(67) = AINT ( B2(I) )
        IF (ABS(A(67)-B2(I))-RFIVE*FLOAT(ITEN**(-ISIGD))) 30,30,20
  20      MT = 7
          GO TO 40
  30  CONTINUE
C
      NW(6) = FLOG10 ( B2(K)+DELTA ) + RTWO
      ND(6) = IZERO 
      NSD   = NW(6) - IONE
      GO TO 60
  40  NSD = ISIGD - IONE
      CALL MINNW (B2(1),K,NSD,NSD+IFIVE,MA(1),0,NW(6),ND(6),MW,MD)
      IF (MW.LE.8) GO TO 50
      MT  = IONE
      NSD = ITHRE
      MW  = 8
      CALL RFORMT (0,NSD,B2,A(1),K,8,NW(6),ND(6),MA,IRF)
      GO TO 60
  50  NW(6) = MW
      ND(6) = MD
  60  NB(6) = 8 - NW(6)
C
      CALL RFORMT ( 0,ISIGD,B4(1),RC(1),    K,13,NW(3),ND(3),MA( 1),IRF)
      CALL RFORMT ( 0,ISIGD,B7(1),RC(1),    K,13,NW(4),ND(4),MA( 1),IRF)
      CALL RFORMT ( 0,ISIGD,B8(1),RC(1),    K,13,NW(5),ND(5),MA( 1),IRF)
      NB(3) = 13 - NW(3)
      NB(4) = 13 - NW(4)
      NB(5) = 13 - NW(5)
C
      LENCT = IFIVE
      DO 70 I=1,K
        MA(22) = LA(45)
        MA(36) = LA(45)
        MA(50) = LA(45)
        MA(51) = LA(45)
        IF (B4(I).LE.A(22)) MA(22) = LA(22)
        IF (B4(I).GE.A(23)) MA(22) = LA(18)
        IF (B7(I).LE.A(26)) MA(36) = LA(22)
        IF (B7(I).GE.A(60)) MA(36) = LA(18)
        IF (B8(I).LE.A(29)) MA(50) = LA(22)
        IF (B8(I).GE.A(27)) MA(50) = LA(18)
        IF (B6(I).LE.A(58)) MA(51) = LA(22)
        IF (B6(I).GE.A(59)) MA(51) = LA(18)
C
        JJ = B3(I)
        CALL RFORMT (MT,NSD  ,A  ,B2(I),NB(6), 1,NW(6),ND(6),MA( 1),IRF)
        CALL RFORMT ( 1,ISIGD,A  ,B4(I),NB(3), 1,NW(3),ND(3),MA( 9),IRF)
        CALL RFORMT ( 1,ISIGD,A  ,B7(I),NB(4), 1,NW(4),ND(4),MA(23),IRF)
        CALL RFORMT ( 1,ISIGD,A  ,B8(I),NB(5), 1,NW(5),ND(5),MA(37),IRF)
        WRITE (IPRINT,100) (MA(J),J=1,8),JJ,(MA(II),II=9,50),B6(I),
     1     MA(51)
        IF (NCRT.NE.IZERO) THEN
          LENCT = LENCT + IONE
          IF (LENCT.GE.LENGTH) THEN
            CALL PAGE (0)
            LENCT = IZERO
          ENDIF
        ENDIF
  70  CONTINUE
C
      DO 80 I=1,53
        MA(I) = LA(39)
  80  CONTINUE
C
      IF (NCRT.NE.IZERO .AND. LENCT + ITWO .GT.LENGTH) CALL PAGE (0)
      WRITE (IPRINT,110) (MA(I),I=1,53) 
C
      CALL RFORMT ( 1,ISIGD,RC   ,A(21),NB(3), 1,NW(3),ND(3),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC   ,A(26),NB(4), 1,NW(4),ND(4),MA(14),IRF)
      CALL RFORMT ( 1,ISIGD,RC   ,A(27),NB(5), 1,NW(5),ND(5),MA(27),IRF)
      WRITE (IPRINT,120) NRMAX,(MA(II),II=1,39)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,130)
      RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (3)   ESTIMATES - GROUP, NO., SUM RANKS, MEAN, MINIMUM, MAXIMUM.
C
  90  FORMAT (29X,15A1/29X,15HI  ESTIMATES  I/29X,15A1//3X,5HGROUP,
     1   4X,3HNO.,7X,4HMEAN,9X,7HMINIMUM,9X,7HMAXIMUM,5X,9HSUM RANKS)
 100  FORMAT (1X,8A1,1X,I5,2X,14A1,1X,14A1,2X,14A1,1X,F8.1,A1)
 110  FORMAT (2X,6A1,3X,4A1,2X,12A1,3X,12A1,4X,12A1,4X,7A1) 
 120  FORMAT (3X,5HTOTAL,2X,I5,2X,13A1,2X,13A1,3X,13A1)
 130  FORMAT (1H )
C
C     ================================================================
C
      END 
*OWPRHV
      SUBROUTINE OWPRHV (K,M3,MA,ND,NW,B3,B4,B5,B10)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRHV V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT TESTS FOR HOMOGENEITY OF VARIANCES FOR ONEWAY INSTRUCTION 
C        AND ALSO PRINT COMPONENT OF VARIATION.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER, 1977.
C                   CURRENT VERSION -     APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*), ND(*), NW(*)
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
      REAL             B3(*), B4(*), B5(*), B10(*)
      REAL             ALPHA, ATEMP
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ALPHA / 0.10 /
C
C     ==================================================================
C
C     (1)   PRINT TESTS FOR HOMOGENEITY OF VARIANCE.
C
      IF (NCRT.NE.IZERO) CALL PAGE (0)
      DO 10 I=1,40
        MA(I) = LA(39)
  10  CONTINUE
C
      CALL RFORMT ( 0,ISIGD,A(57),RC(1),1,20,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC   ,A(57),0, 0,NW(1),ND(1),MA(41),IRF)
      M9 = NW(1) + 40
      WRITE (IPRINT,60) (MA(I),I=1,40),(MA(J),J=1,40),A(51),A(52),
     1   A(53),A(54),(MA(JJ),JJ=41,M9)
C
      IF (A(52).GT.ALPHA .AND. A(54).GT.ALPHA) GO TO 40
C
C     COMPUTE AND PRINT APPROXIMATE F TEST FOR DIFFERENCE BETWEEN
C        MEANS IN PRESENCE OF HETEROGENEOUS VARIANCE.
C           REF. SNEDECOR, 288, 5TH ED. 
C
C     REPLACE N(I) FOR ORDERED XBAR(I) BY W(I) IN B10(I).
C
      DO 20 I=1,K
        B10(I) = FDIV (B3(I),B5(I)**2,IND)
        A(86)  = A(86) + B10(I)*B4(I)
        A(87)  = A(87) + B10(I)
  20  CONTINUE
      A(86) = FDIV (A(86),A(87),IND)
C
      DO 30 I=1,K
        A(55) = A(55) + B10(I) * (B4(I)-A(86))**ITWO
        A(88) = A(88) + FDIV ((RONE-FDIV(B10(I),A(87),IND))**2,
     1                       B3(I)-RONE,IND)
  30  CONTINUE
C
      A(55) = FDIV (A(55),A(73),IND)
      A(88) = FDIV (A(88),FLOAT(K**2)-RONE,IND)
      A(55) = FDIV (A(55),RONE+RTWO*FLOAT(M3)*A(88),IND)
      A(88) = FDIV (RONE,RTHRE*A(88),IND)
      A(88) = AINT (A(88)+RHALF)
      CALL QFORF (A(73),A(88),A(55),ATEMP)
      A(56) = ATEMP 
      WRITE (IPRINT,70) A(56) 
C
C     (2)   PRINT COMPONENT OF VARIATION.
C
  40  CALL RFORMT ( 0,ISIGD,A(47),RC(1),1,25,NW(1),ND(1),MA( 1),IRF)
      CALL RFORMT ( 1,ISIGD,RC   ,A(47),0, 0,NW(1),ND(1),MA(79),IRF)
      MM = NW(1) + 78
C
      DO 50 I=1,39
        MA(I) = LA(39)
  50  CONTINUE
C
      WRITE (IPRINT,80) (MA(I),I=1,39),(MA(J),J=1,39),(MA(JJ),JJ=79,MM)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  60  FORMAT (/1X,15X,40A1/16X,40HI  TESTS FOR HOMOGENEITY OF VARIANCES 
     1 I/16X,40A1//1X,13HCOCHRAN'S C =, 
     2 30H MAX VARIANCE/SUM(VARIANCES) =,F6.4,14H, APPROX SL = ,F5.3/ 
     3   1X,16HBARTLETT-BOX F =,F9.3,23H, SIGNIFICANCE LEVEL = ,F5.3/ 
     4 1X,37HMAXIMUM VARIANCE / MINIMUM VARIANCE =,20A1)
  70  FORMAT (1X,66HASSUMING HETROGENEOUS VARIANCE, APPROX. BETWEEN MEAN
     1S F-TEST SL = ,F5.3)
  80  FORMAT (/1X,16X,39A1/17X,39HI  MODEL II - COMPONENTS OF VARIANCE  
     1I/17X,39A1//
     2     1X,46HESTIMATE OF BETWEEN COMPONENT OF VARIATION IS ,25A1) 
C
C     ==================================================================
C
      END 
*OWPRMC
      SUBROUTINE OWPRMC (K,M4,MA,ND,NW,B3,B4,B6,B9,B10)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. OWPRMC V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTATIONS AND PRINTING FOR MULTIPLE COMPARISION OF MEANS
C        FOR ONEWAY INSTRUCTION.  FORMERLY PART OF PROGRAM UNIT ONEWAY.
C
C     K EQUALS THE NUMBER OF GROUPS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - MARCH, 1978.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*), ND(*), NW(*)
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
      REAL             B3(*), B4(*), B6(*), B9(*), B10(*)
      REAL             ALPHA, UDF, VDF
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ALPHA / 0.10 /
C
C     ==================================================================
C
      CALL PAGE (IFOUR)
      NLINE = IZERO 
C
C     SORT XBAR FOR MULTIPLE COMPARISIONS OF MEANS
C
      DO 10 I=1,K
        B9(I) = B4(I)
  10  CONTINUE
C
      CALL SORT (B9,B6,K,IONE)
C
      DO 20 I=1,K
        J = B6(I)
        B10(I) = B3(I)
  20  CONTINUE
C
      IF (A(14).GE.ALPHA) RETURN
      IF (M4.LT.IFOUR) GO TO 140
C
      DO 30 I=1,44
        MA(I) = LA(39)
  30  CONTINUE
      WRITE (IPRINT,250) (MA(I),I=1,44),(MA(J),J=1,44)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,310)
      WRITE (IPRINT,255)
C
      NLINE = NLINE + 7
C
C     NEWMAN-KEULS-HARTLEY
C
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,310)
      WRITE (IPRINT,260)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,310)
      NLINE = NLINE + ITWO 
      J = IONE
      M = IZERO
  40  I = K
  50  IF (I.LE.M) GO TO 130
      IF (I.EQ.J) GO TO 70
      A(71) = ABS (B9(I)-B9(J))
C
C     MANDEL APPROXIMATION TO PERCENT POINT OF STUDENTIZED RANGE
C
      A(92) = I - J + IONE
      UDF   = A(92) 
      VDF   = A(70) 
      CALL SRPPT5 (UDF,VDF,A(79))
      A(79) = A(79) * FSQRT (RHALF*(FDIV(RONE,B10(I),IND) + 
     1                       FDIV (RONE,B10(J),IND))) * A(31)
      IF (A(71)-A(79)) 70,70,60
  60  I = I - IONE
      GO TO 50
  70  IF (J.EQ.IONE) GO TO 90 
      IF (J.GT.M) GO TO 80
      WRITE (IPRINT,290)
      GO TO 90
  80  WRITE (IPRINT,300)
  90  JJ = IONE
      NLINE = NLINE + IONE
      DO 120 L=J,I
        CALL RFORMT ( 0,ISIGD,B9(L),RC(1),1,13,NW(3),ND(3),MA( 1),IRF)
        IF (JJ+NW(3)+IONE.LE.71) GO TO 100
C
C     OVERFLOW ON LINE.
C
        JJ = JJ - IONE
        WRITE (IPRINT,280) (MA(II),II=1,JJ)
        NLINE = NLINE + IONE
        JJ = IONE
 100    CALL RFORMT ( 1,ISIGD,RC   ,B9(L),1, 0,NW(3),ND(3),MA(JJ),IRF)
        JJ = JJ + NW(3) + IONE
        IF (L.NE.I) GO TO 110 
C
C     END OF LOOP
C
        JJ = JJ - IONE
        WRITE (IPRINT,280) (MA(II),II=1,JJ)
        GO TO 120
 110    MA(JJ) = LA(44)
        JJ = JJ + IONE
        IF (NCRT.NE.IZERO .AND. NLINE.GT. LENGTH) THEN
          NLINE = IZERO
          CALL PAGE (0)
        ENDIF 
 120  CONTINUE
C
      IF (I.GE.K) GO TO 140
      M = I
 130  J = J + IONE
      GO TO 40
C
C     SCHEFFE METHOD
C
 140  IF (NCRT.NE.IZERO .AND. NLINE + ITWO.GT.LENGTH) THEN
        NLINE = IZERO
        CALL PAGE (0)
      ENDIF
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,310)
      WRITE (IPRINT,270)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,310)
      NLINE = NLINE + ITWO
      J = IONE
      M = IZERO
 150  I = K
 160  IF (I.LE.M) GO TO 240
      IF (I.EQ.J) GO TO 180
      A(71) = ABS (B9(I)-B9(J))
      A(79) = A(76) * FSQRT (FDIV (RONE,B10(I),IND) +
     1                       FDIV (RONE,B10(J),IND) )
      IF (A(71)-A(79)) 180,180,170
 170  I = I - IONE
      GO TO 160
 180  IF (J.EQ.IONE) GO TO 200
      IF (J.GT.M) GO TO 190
      WRITE (IPRINT,290)
      GO TO 200
 190  WRITE (IPRINT,300)
 200  JJ = IONE
      NLINE = NLINE + IONE
      DO 230 L = J,I
        CALL RFORMT ( 0,ISIGD,B9(L),RC(1),1,13,NW(3),ND(3),MA( 1),IRF)
        IF (JJ+NW(3)+IONE.LE.71) GO TO 210
C
C       OVERFLOW ON LINE.
C
        JJ = JJ - IONE
        WRITE (IPRINT,280) (MA(II),II=1,JJ)
        NLINE = NLINE + IONE
        JJ = IONE
 210    CALL RFORMT ( 1,ISIGD,RC   ,B9(L),1, 0,NW(3),ND(3),MA(JJ),IRF)
        JJ = JJ + NW(3) + IONE
        IF (L.NE.I) GO TO 220 
C
C       END OF LOOP 
C
        JJ = JJ - IONE
        WRITE (IPRINT,280) (MA(II),II=1,JJ)
        GO TO 230
 220    MA(JJ) = LA(44)
        JJ = JJ + IONE
        IF (NCRT.NE.IZERO .AND. NLINE.GT. LENGTH) THEN
          NLINE = IZERO
          CALL PAGE (0)
        ENDIF 
 230    CONTINUE
C
      IF (I.GE.K) RETURN
      M = I
 240  J = J + IONE
      GO TO 150
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 250  FORMAT (14X,44A1/14X,44HI  PAIRWISE MULTIPLE COMPARISION OF MEANS 
     1 I/14X,44A1)
 255  FORMAT (
     1 1H ,68HTHE MEANS ARE PUT IN INCREASING ORDER IN GROUPS SEPARATED 
     2BY ***.  A/1H ,70HMEAN IS ADJUDGED NON-SIGNIFICANTLY DIFFERENT FRO
     3M ANY MEAN IN THE SAME/1H ,67HGROUP AND SIGNIFICANTLY DIFFERENT AT
     4 THE .05 LEVEL FROM ANY MEAN IN/1H ,68HANOTHER GROUP.  ***** INDIC
     5ATES ADJACENT GROUPS HAVE NO COMMON MEAN.)
 260  FORMAT ( 1X,8X,52H-   NEWMAN-KEULS TECHNIQUE, HARTLEY MODIFICATION
     1   -/ 4X,66H(APPROXIMATE, IF THE NUMBERS OF MEASUREMENTS IN THE GR
     2OUPS DIFFER) )
 270  FORMAT ( 1X,22X,25H-   SCHEFFE TECHNIQUE   - )
 280  FORMAT (1X,71A1)
 290  FORMAT (6X,3H***)
 300  FORMAT (5X,5H*****)
 310  FORMAT (1H )
C
C     ================================================================
C
      END 
*OWSTRE
      SUBROUTINE OWSTRE (K,B2,B3,B4,B5)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. OWSTRE V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     STORE TAGS, NUMBERS, MEANS AND STANDARD DEVIATIONS FOR ONEWAY.
C
C     K = NUMBER OF GROUPS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1977.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             B2(*), B3(*), B4(*),  B5(*)
C
C     ================================================================
C
C     STORE TAGS, NUMBERS, MEANS, AND STANDARD DEVIATIONS.
C
      ITSUB  = IARGS(3)
      INSUB  = IARGS(4)
      IXBSUB = IARGS(5)
      ISDSUB = IARGS(6)
C
      DO 10 I=1,K
C
C     TAG
C
        RC(ITSUB)  = B2(I)
C
C     N
C
        RC(INSUB)  = B3(I)
C
C     XBAR
C
        RC(IXBSUB) = B4(I)
C
C     STANDARD DEVIATION
C
        RC(ISDSUB) = B5(I)
C
        ITSUB  = ITSUB  + IONE
        INSUB  = INSUB  + IONE
        IXBSUB = IXBSUB + IONE
        ISDSUB = ISDSUB + IONE
  10  CONTINUE
C
      RETURN
C
C     ================================================================
C
      END
*PARPLT
      SUBROUTINE PARPLT (X,Y,W,N,GAMMA) 
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PARPLT V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A PARETO 
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = GAMMA).
C              THE PROTOTYPE PARETO DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL X EQUAL TO
C              OR GREATER THAN 1,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = GAMMA / (X**(GAMMA+1)).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS 
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE PARETO PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN 
C              IS THE  PARETO DISTRIBUTION
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
C     OUTPUT--A ONE-PAGE PARETO PROBABILITY PLOT. 
C     PRINTING--YES.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 7500.
C                 --GAMMA SHOULD BE POSITIVE.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 233-249.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS
C                 GAITHERSBURG, MD 20899
C                 PHONE  301-975-2845 
C     ORIGINAL VERSION--NOVEMBER  1975. 
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
C                   CURRENT VERSION - DECEMBER, 1989.
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
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        M*1, MT*1
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
C     SORT THE DATA.
C
  10  CALL SORTPP (X,N,Y)
C
C     GENERATE UNIFORM ORDER STATISTIC MEDIANS.
C
      CALL UNIMED (N,W)
C
C     COMPUTE PARETO DISTRIBUTION ORDER STATISTIC MEDIANS.
C
      DO 20 I=1,N
        W(I) = (RONE-W(I)) ** FDIV (-RONE,GAMMA,IND)
        IF (IND.NE.IZERO) CALL ERROR (106)
  20  CONTINUE
C
C
C     PLOT THE ORDERED OBSERVATIONS VERSUS ORDER STATISTICS MEDIANS.
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
      IF (IND.NE.IZERO) CALL ERROR (106)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
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
 120  FORMAT (15X, 6HPARETO            ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 130  FORMAT ( 1X, 6HPARETO            ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 140  FORMAT ( 1X, 6HPARETO       ,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 150  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 160  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 170  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END 
*PCKFMT   
      SUBROUTINE PCKFMT (IND) 
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PCKFMT V 6\7.00  9/12/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C          PACK FORMAT  IN IFMT.        
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -    AUGUST, 1969.       
C                   CURRENT VERSION - SEPTEMBER, 1991.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IAA(80)       
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                    ***   TYPE STATEMENTS   ***   
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1
      CHARACTER LA*1, NEWCRD*1
      CHARACTER IAA*1         
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
C     THE FOLLOWING CONSTANT IS (45*256+45)*256+45
C         
      DATA ICA /      41 /    
      DATA ICB /      42 /    
      DATA ICC /      45 /    
      DATA ICD /      46 /    
C         
C     ==================================================================        
C         
C     PACK AND STORE FORMAT.  
C         
      IND = IZERO   
      K   = KARD(KRDPOS)      
  10  KRDPOS = KRDPOS + IONE  
      IF (KARD(KRDPOS).NE.ICA .AND. KARD(KRDPOS).NE.ICD) GO TO 10     
      IF (KARD(KRDPOS).NE.ICA .AND. KARD(KRDPOS).EQ.ICD) GO TO 60     
      KK = IONE     
      KA = IZERO    
      MA = KRDPOS + IONE      
      KR = KRDEND + ITHRE     
      DO 20 I=MA,KR 
        IF (KARD(I).EQ.ICA) KK = KK + IONE        
        IF (KARD(I).NE.ICB) GO TO 20    
        KA = KA + IONE        
        IF (KA.EQ.KK) GO TO 30
  20  CONTINUE      
      GO TO 60      
C         
  30  MB = KRDPOS - ITWO      
      IM = I - KRDPOS + IONE  
      DO 40 JA=1,LENCRD       
        IAA(JA) = LA(ICC)     
  40  CONTINUE      
C         
      DO 50 JA=1,IM 
        IAA(JA) = NEWCRD(MB)  
        MB = MB + IONE        
        DO 45 JAA=49,74
          IF (IAA(JA) .NE. LA(JAA)) GO TO 45
          IAA(JA) = LA(JAA-38)
          GO TO 50
  45    CONTINUE
  50  CONTINUE      
C         
      WRITE (ISCRT,70) (IAA(JA),JA=1,LENCRD)      
      BACKSPACE ISCRT         
      READ (ISCRT,80) IFMT (K-9)        
      BACKSPACE ISCRT         
      RETURN        
C         
C     ..................................................................        
C         
  60  IND = IND + IONE        
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
  70  FORMAT (80A1) 
  80  FORMAT (1A80) 
C         
C     ==================================================================        
C         
      END 
*PDMOTE
      SUBROUTINE PDMOTE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PDMOTE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 10,      FOR PROMOTE.
C     L2 = 11,      FOR DEMOTE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
      IST = IZERO
      L2  = L2 - ITEN
      IF (MOD(NARGS,ITWO).NE.IZERO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (KIND(1).EQ.IONE) GO TO 20
      NR = IARGS(1)
      IARGS(1) = IONE
      CALL CHKCOL
      GO TO 30
C
  20  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
C     IF NUMBER OF ROWS TO BE MOVED IS NEGATIVE, FLIP INSTRUCTIONS.
C        I.E.,  PROMOTE -6  IS THE SAME AS   DEMOTE  6   .
C
  30  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (NR.GE.IZERO) GO TO 40
      L2 = IONE - L2
      NR = -NR
  40  NARGS = NARGS - IONE
C
C     CHECK DISTANCE OF MOVE.
C
      IF (L2.EQ.IZERO) GO TO 50
      IF (NR+NRMAX.LE.NROW) GO TO 110
      CALL ERROR (231)
      NROLD = NRMAX
      NRMAX = NROW - NR
      IST   = IONE
      IF (NRMAX.GT.IZERO) GO TO 110
      NRMAX = NROLD
      CALL ERROR (16)
      RETURN
C
C     ..................................................................
C
  50  NDIFF = NRMAX - NR
      IF (NDIFF.GE.IZERO) GO TO 60
      CALL ERROR (230)
      NDIFF = IZERO
      NR    = NRMAX
  60  IF (NARGS.GT.IZERO) GO TO 110
      J = IARGS(1) - IONE
      DO 100 I1=1,NCOL
        K1 = J + IONE
        IF (NDIFF.EQ.IZERO) GO TO 80
        K2 = K1 + NR
        DO 70 I2=1,NDIFF
          RC(K1) = RC(K2)
          K1 = K1 + IONE
          K2 = K2 + IONE
  70    CONTINUE
  80    DO 90 I3=1,NR
          RC(K1) = RZERO
          K1 = K1 + IONE
  90    CONTINUE
        J = J + NROW
 100  CONTINUE
      RETURN
C
C     ..................................................................
C
 110  LIMIT = NARGS
      IF (LIMIT.EQ.IZERO) LIMIT = ITWO * NCOL
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     START PROMOTING OR DEMOTING.
C
      DO 190 I=1,LIMIT,2
        IF (NARGS.NE.IZERO) GO TO 120
        K1 = IARGS(1)
        K2 = K1
        IARGS(1) = IARGS(1) + NROW
        GO TO 130
 120    K1 = IARGS(I+1)
        K2 = IARGS(I+2)
 130    IF (L2.EQ.IZERO) GO TO 150
C
C     DEMOTE COL AT K1 TO COL AT K2.
C
        K1 = K1 + NRMAX
        K2 = K2 + NRMAX + NR
        DO 140 J=1,NRMAX
          K1 = K1 - IONE
          K2 = K2 - IONE
          RC(K2) = RC(K1)
 140    CONTINUE
        GO TO 190
C
C     PROMOTE COL AT K1 TO COL AT K2.
C
 150    JJ = NRMAX - NR
        IF (JJ.EQ.IZERO) GO TO 170
        K1 = K1 + NR
        DO 160 J=1,JJ
          RC(K2) = RC(K1)
          K1 = K1 + IONE
          K2 = K2 + IONE
 160    CONTINUE
C
C     IF PROMOTE ARRAY, FILL RST OF COLUMN WITH ZEROES.
C
        IF (NARGS.NE.IZERO) GO TO 190
 170    JJ = JJ + IONE
        DO 180 J=JJ,NRMAX
          RC(K2) = RZERO
          K2 = K2 + IONE
 180    CONTINUE
 190  CONTINUE
C
      IF (IST.EQ.IZERO) NROLD = NRMAX
      IF (L2.EQ.IZERO) RETURN
      NRMAX = NRMAX + NR
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*PERRSS
      SUBROUTINE PERRSS (ISUBY,ISUBH,N,SS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PERRSS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO COMPUTE ERROR SUM OF SQUARES FOR LACK OF FIT TEST
C        WITHOUT USING WEIGHTS.
C
C        SUBSCRIPT FOR PICKING UP Y IS IN A(ISUBH).
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
      REAL             SUMY, YBAR
      REAL             FDIV
C
C     ==================================================================
C
      J = ISUBH
      CALL SUMMAL (TEMP,IZERO,SUMY)
      DO 10 I=1,N
        K = A(J) - RONE
        M = ISUBY  + K
        CALL SUMMAL (RC(M),-IONE,SUMY)
        J = J + IONE
  10  CONTINUE
      CALL SUMMAL (TEMP(1),IONE,SUMY)
      YBAR = FDIV (SUMY,FLOAT(N),IND)
C
      J = ISUBH
      CALL SUMMAL (TEMP,IZERO,SS)
      DO 20 I=1,N
        K = A(J) - RONE
        M = ISUBY  + K
        TEMP(1) = (RC(M)-YBAR)**2
        CALL SUMMAL (TEMP,-IONE,SS)
        J = J + IONE
  20  CONTINUE
      CALL SUMMAL (TEMP,IONE,SS)
      RETURN
C
C     ==================================================================
C
      END
*PLOTCE
      SUBROUTINE PLOTCE (N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PLOTCE V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PLOT 95 PERCENT CONFIDENCE ELIPSE FOR POLYFIT OF 1 DEGREE.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1977.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IA(131), INM(11)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             XSCALE(1)
      REAL             RINC, X, XBOT, XDELTA, XMAX, XMAXP, XMIN, XMINP
      REAL             XTOP, YBOT, YDELTA, YD10, YMAX, YMAXP, YMIN
      REAL             YMINP, YTOP
C
C     ..................................................................
C
      CHARACTER LA*1
      CHARACTER IA*1, INM*1, ISLOPE*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NXSCLE /  7 /
C
      DATA    NXY / 11 /
      DATA    NSY /  5 /
      DATA ISEVEN /  7 /
      DATA   NINE /  9 /
C
C     ==================================================================
C
C     LOCATE MIN AND MAX OF Y AND X.
C
      NSUBX  = N + ITHRE
      YMIN   = A(3)
      YMAX   = A(3)
      XMIN   = A(NSUBX)
      XMAX   = A(NSUBX)
      JSUBX  = NSUBX
      INTERC = IONE
      DO 10 I=1,N
        IF (YMIN.GT.A(I+2)) YMIN = A(I+2)
        IF (YMAX.LT.A(I+2)) YMAX = A(I+2)
        IF (XMIN.GT.A(JSUBX)) XMIN = A(JSUBX)
        IF (XMAX.LT.A(JSUBX)) XMAX = A(JSUBX)
        JSUBX = JSUBX + IONE
  10  CONTINUE
C
C     DETERMINE IF PLOT IS TO BE 51 POSITIONS OR 101 ON THE X-AXIS.
C
      IF (LWIDE.LT.LWC) GO TO 20
C
C     PRINT POSITIONS ON X-AXIS ARE 101.
C
      MXINT = ITEN
      MXPOS = 101
      MXPOS1 = MXPOS + ITWO
C
C     PRINT TITLE.
C
      WRITE (IPRINT,410)
C
C     PRINT TOP X-AXIS.
C
      WRITE (IPRINT,420)
      GO TO 30
C
C     PRINT POSTIONS ON X-AXIS ARE 51.
C
  20  MXINT  = IFIVE
      MXPOS  = 51
      MXPOS1 = MXPOS + ITWO
C
C     PRINT TITLE.
C
      WRITE (IPRINT,400)
C
      WRITE (IPRINT,430)
C
C     DETERMINE NUMBER OF INCREMENTS ON Y-AXIS.
C
  30  IF (LENGTH.GE.15) GO TO 40
      CALL ERROR (255)
      RETURN
C
C     ..................................................................
C
  40  LINE = IDIV (LENGTH-IFIVE,ITEN,IND)
      IF (NCRT.EQ. IZERO) LINE = IFIVE
      NYINC = LINE * ITEN
      ISUBSP = IDIV (LINE,ITWO,IND) * ITEN + ITWO
      ISUBL = IONE
      ISLOPE = LA(45)
C
C     SCALE2 DETERMINES NICE MIN AND MAX FOR Y-AXIS.
C
      CALL SCALE2 (YMIN,YMAX,NYINC,YMINP,YMAXP,YDELTA,IFAULT)
C
C     DETERMINE NICE MIN AND MAX FOR X-AXIS.
C
      CALL SCALE2 (XMIN,XMAX,MXPOS-IONE,XMINP,XMAXP,XDELTA,IFAULT)
C
C     CHANGE SIGNS OF Y, SORT, AND CHANGE SIGNS BACK.
C
      DO 50 I=1,N
        A(I+2) = -A(I+2)
  50  CONTINUE
C
      NSUBH = ITWO * N + ITHRE
      CALL SORT (A(3),A(NSUBH),N,IZERO)
      DO 60 I=1,N
        A(I+2) = -A(I+2)
  60  CONTINUE
C
      YTOP = YMAXP
      YBOT = YTOP - RHALF * YDELTA
      NSUBP = ITHRE * N + ITHRE
      NSUBPT = NSUBP + MXPOS1 - IONE
      NSUBYL = NSUBPT + IONE
      JSUBYL = NSUBYL
      A(JSUBYL) = YMAXP
      JSUBYL = JSUBYL + IONE
      YD10 = RTEN * YDELTA
      IF (LINE.LE.IONE) GO TO 80
      DO 70 I=2,LINE
        A(JSUBYL) = A(JSUBYL-1) - YD10
        JSUBYL = JSUBYL + IONE
  70  CONTINUE
C
  80  A(JSUBYL) = YMINP
      NSX       = NSY
C
      CALL MINNW (A(NSUBYL),LINE+1,NSY,NXY,INM,0,NW,ND,NWY,NDY)
C
      NBY = NXY - NWY
      ITYPE = ISEVEN
      IF (NDY.EQ.IZERO) ITYPE = NINE
C
C     IF INCREMENTS ON Y-AXIS ARE SMALL PRINT WITH E FORMAT.
C
      IF (ABS (YD10).GT.1.0E-7) GO TO 90
      ITYPE = IFIVE
      NSX   = NSY - IONE
C
  90  LINE1  = LINE + IONE
      JSUBY  = ITHRE
      JSUBH  = NSUBH
      JSUBYL = NSUBYL
      ISWCH  = IZERO
      ISUBT  = IONE
      DO 320 I=1,LINE1
        DO 310 K=1,10
C
C         CLEAR PRINT AREA.
C
          DO 100 J=2,MXPOS1
            IA(J) = LA(45)
 100      CONTINUE
          IF (ISWCH.EQ.IONE) GO TO 140
          IF (YBOT.GT.A(2)) GO TO 150
          ISWCH  = IONE
          X      = A(1)
          XBOT   = XMINP
          XTOP   = XBOT + RHALF * XDELTA
          DO 130 JA=1,MXPOS
            IF (X.LT.XBOT .OR. X.GT.XTOP) GO TO 120
            INTERC = JA
            DO 110 JB=1,INTERC
              IA(JB) = LA(39)
 110          CONTINUE
            IA(INTERC) = LA(34)
            GO TO 150
 120        XBOT = XTOP
            XTOP = XTOP + XDELTA
 130      CONTINUE
 140      IA(INTERC) = LA(2)
 150      IA(1) = LA(39)
          IA(MXPOS1) = LA(39)
C
C         LOCATE Y FOR PRINT LINE.
C
          DO 180 J=1,N
            IF (JSUBY.GT.N) GO TO 190
            IF (YBOT.GT.A(JSUBY)) GO TO 190
C
C         THERE IS A Y FOR THIS LINE.
C            DETERMINE PRINT POSITION FOR X.
C
            JSUBY = JSUBY + IONE
            JSUBX = A(JSUBH) + RHALF
            JSUBH = JSUBH + IONE
            JSUBX = JSUBX + N + ITWO
            X = A(JSUBX)
            XBOT = XMINP
            XTOP = XBOT + RHALF * XDELTA
            DO 170 JA=1,MXPOS
              IF (X.LT.XBOT .OR. X.GT.XTOP) GO TO 160
              IA(JA+1) = LA(41)
              GO TO 180
 160          XBOT = XTOP
              XTOP = XTOP + XDELTA
 170          CONTINUE
 180      CONTINUE
 190      GO TO (200,210,220,230,220,240,220,250,220,260,270), ISUBL
 200      IF (ISUBT.GE.ISUBSP) GO TO 210
          ISUBT = ISUBT + IONE
          GO TO 280
 210      ISLOPE = LA(29)
          ISUBL = ITHRE
          GO TO 280
 220      ISLOPE = LA(45)
          ISUBL = ISUBL + IONE
          GO TO 280
 230      ISLOPE = LA(22)
          ISUBL = ISUBL + IONE
          GO TO 280
 240      ISLOPE = LA(25)
          ISUBL = ISUBL + IONE
          GO TO 280
 250      ISLOPE = LA(26)
          ISUBL = ISUBL + IONE
          GO TO 280
 260      ISLOPE = LA(15)
          ISUBL = ISUBL + IONE
          GO TO 280
 270      ISLOPE = LA(45)
 280      IF (K.EQ.IONE) GO TO 290
          WRITE (IPRINT,440) ISLOPE, (IA(J),J=1,MXPOS1)
          GO TO 300
 290      IA(1) = LA(40)
          IA(MXPOS1) = LA(40)
          CALL RFORMT (ITYPE,NSX,RC,A(JSUBYL),NBY,0,NWY,NDY,INM,IRF)
          JSUBYL = JSUBYL + IONE
          WRITE (IPRINT,450) ISLOPE, (INM(J),J=1,NXY),
     1                      (IA(J),J=1,MXPOS1)
          IF (I.EQ.LINE1) GO TO 320
 300      YTOP = YBOT
          YBOT = YBOT - YDELTA
 310    CONTINUE
 320  CONTINUE
C
      MXINT1    = MXINT + IONE
      XSCALE(1) = XMINP
      NX        = NXSCLE
      NSD       = ISIGD - IONE
      INC       = ITEN
      KSTOP     = MXINT1
      RINC      = RTEN
C
      DO 330 I=1,MXINT1
        CALL MINNW (XSCALE,1,NSD,NX+IFIVE,IA(1),IZERO,NNW,NND,NW,ND)
        XSCALE(1) = XSCALE(1) + RINC * XDELTA
        IF (NW.GT.NX) GO TO 340
 330  CONTINUE
      GO TO 350
C
 340  NX  = NX + IFOUR
      NSD = NSD - IONE
      INC = ITWO * ITEN
      IF (LWIDE.LT.LWC) INC = IFIVE * IFIVE
      RINC  = FLOAT (INC)
      KSTOP = IDIV (MXINT,ITWO,IND) + IONE
C
 350  K         = IONE
      XSCALE(1) = XMINP
      DO 370 I=1,KSTOP
        ITYPE = ISEVEN
        CALL MINNW (XSCALE,1,NSD,NX+IFIVE,IA(K),IZERO,NNW,NND,NW,ND)
        IF (NW.LE.NX) GO TO 360
        NSD = NX - IFIVE
        CALL RFORMT (0,NSD,XSCALE,A(1),1,NX,NW,ND,IA,IRF)
        ITYPE = IONE
 360    NB    = ITEN - NW
        IF (ND.EQ.IZERO .AND. ITYPE.EQ.ISEVEN) ITYPE = NINE
        CALL PUTCH (LA(45),ITEN + INC,IA(K))
        CALL RFORMT (ITYPE,NSD,A,XSCALE(1),NB,1,NW,ND,IA(K),IRF)
        K         = K + INC
        XSCALE(1) = XSCALE(1) + RINC * XDELTA
 370  CONTINUE
C
C     PLOTTING IS FINISHED, EXCEPT FOR BOTTOM X-AXIS AND SCALES.
C
      IF (LWIDE.LT.LWC) GO TO 380
C
C     PRINT POSTIONS ON X-AXIS ARE 101.
C
      WRITE (IPRINT,420)
      GO TO 390
C
C     PRINT POSTIONS ON X-AXIS ARE 51.
C
 380  IF (KSTOP.GT.ITHRE) WRITE (IPRINT,430)
      IF (KSTOP.LE.ITHRE) WRITE (IPRINT,490)
C
C     PRINT SCALE FOR HORIZONTAL AXIS.
 390  MXINTT = KSTOP * INC
      WRITE (IPRINT,460) (IA(I),I=1,MXINTT)
      IF (LWIDE.GE.LWC) WRITE (IPRINT,470)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,480)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 400  FORMAT ( 2X,64H95 PERCENT CONFIDENCE ELLIPSE FOR INTERCEPT AND SLO
     1PE PARAMETERS/)
 410  FORMAT (32X,64H95 PERCENT CONFIDENCE ELLIPSE FOR INTERCEPT AND SLO
     1PE PARAMETERS/)
 420  FORMAT (14X,10(10H+---------),1H+)
 430  FORMAT (14X,5(10H+---------),1H+)
 440  FORMAT (1X,A1,11X,103A1)
 450  FORMAT (1X,12A1,103A1)
 460  FORMAT (9X,120A1)
 470  FORMAT (60X,9HINTERCEPT)
 480  FORMAT (35X,9HINTERCEPT)
 490  FORMAT (14X,2(10H+---------),10H+----X----,2(10H+---------),1H+)
C
C     ==================================================================
C
      END
*PLOT24
      SUBROUTINE PLOT24
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PLOT24 V 7.00  4/16/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS ...
C
C        FOURPLOTS OF (Y1) (X1) (Y2) (X2) (Y3) (X3) (Y4) (X4) $  L2 = 18
C        TWOPLOTS  OF (Y1) (X1) (Y2) (X2)                     $  L2 = 19
C
C     LIMITS MAY BE SPECIFIED FOR Y OR X OR BOTH.
C
C     NEEDED CONSTANTS IN DATA STATEMENTS.
C        21 IS THE NUMBER OF PLOTTING POSITIONS ON THE Y-AXIS
C        51 IS THE NUMBER OF PLOTTING POSITIONS ON THE X-AXIS
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IGRAPH(21,51), JGRAPH(102)
      DIMENSION IYARGS(4), IXARGS(4)
      DIMENSION LIMITY(4), LIMITX(4)
C
C     FOR I = 1, ..., NGRAPH = NUMBER OF PLOTS.
C
C        IXARGS(I) = X COLUMN NUMBER FOR ITH GRAPH
C        IYARGS(I) = Y COLUMN NUMBER FOR ITH GRAPH
C        LIMITX(I) = 0, IF LIMITS NOT SPECIFIED FOR X
C                    1, IF LIMITS     SPECIFIED FOR X
C        LIMITY(I) = 0, IF LIMITS NOT SPECIFIED FOR Y
C                    1, IF LIMITS     SPECIFIED FOR Y
C         XBLIM(I) = SPECIFIED LOWER LIMIT FOR X
C         XTLIM(I) = SPECIFIED UPPER LIMIT FOR X
C         YBLIM(I) = SPECIFIED LOWER LIMIT FOR Y
C         YTLIM(I) = SPECIFIED UPPER LIMIT FOR Y
C
      DIMENSION NPTOUT(4), NPTIN(4)
      DIMENSION KB(4), KD(4), KS(4), KT(4), KW(4)
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
C                         ***   TYPE STATEMENTS   ***
C
      REAL             XBLIM(4), XTLIM(4)
      REAL             YBLIM(4), YTLIM(4)
      REAL             YLABLE(84), XX(24)
      REAL             XMIN(4), XMAX(4)
      REAL             X20(4), X40(4), X60(4), X80(4)
C
C     ...................................................................
C
      CHARACTER  JGRAPH*1
C
C     ..................................................................
C
      EQUIVALENCE (XMIN(1),XX( 1)), ( X20(1),XX( 5))
      EQUIVALENCE ( X40(1),XX( 9)), ( X60(1),XX(13))
      EQUIVALENCE ( X80(1),XX(17)), (XMAX(1),XX(21))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     ==================================================================
C
C     ERROR CHECKING.
C
C     NGRAPH = NUMBER OF PLOTS.
C
      NGRAPH = ITWO
      IF (L2.EQ.18) NGRAPH = IFOUR
      NHEAD = ITWO * NGRAPH
C
      IF (NRMAX.GT.IZERO) GO TO 10
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.GE.NHEAD) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  K = MOD(NARGS,ITWO)
      IF (K.EQ.IZERO) GO TO 30
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  30  IF (NRMAX.LE.IDIV(NRC,ITWO,IND)) GO TO 40
      CALL ERROR (23)
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     INITIALIZATION.
C
C     ZERO OUT LIMITY AND LIMITX.
C
      DO 50 I=1,NGRAPH
        LIMITY(I) = IZERO
        LIMITX(I) = IZERO
  50  CONTINUE
C
C     DETERMINE IY Y AXIS AND/OR X AXIS LIMITS ARE SPECIFIED
C        AND IF SO, SETUP LIMITY(I),IYARGS(I),YBLIM(I),YTLIM(I)
C                     AND LIMITX(I),IXARGS(I),XBLIM(I),XTLIM(I).
C
      K             = ITWO
      KIND(NARGS+1) = IZERO
      DO 80 I=1,NGRAPH
        IYARGS(I) = IARGS(K-1)
        IXARGS(I) = IARGS(K)
        IF (KIND(K).EQ.IZERO) GO TO 60
C
C       LIMITS SPECIFIED FOR Y AXIS.
C
        LIMITY(I) = IONE
        YBLIM(I)  = ARGS(K)
        YTLIM(I)  = ARGS(K+1)
        IXARGS(I)      = IARGS(K+2)
        IF (KIND(K+3).EQ.IZERO) GO TO 70
C
C       LIMITS SPECIFIED FOR Y AXIS AND X AXIS.
C
        LIMITX(I) = IONE
        XBLIM(I)  = ARGS(K+3)
        XTLIM(I)  = ARGS(K+4)
        GO TO 70
  60    IF (KIND(K+1).EQ.IZERO) GO TO 70
C
C       LIMITS SPECIFIED FOR X AXIS ONLY.
C
        LIMITX(I) = IONE
        XBLIM(I)  = ARGS(K+1)
        XTLIM(I)  = ARGS(K+2)
C
  70    K     = K + ITWO + ITWO * LIMITX(I) + ITWO * LIMITY(I)
        IF (K.GT.NARGS) GO TO 90
  80  CONTINUE
C
C     BLANK OUT THE GRAPH.
C
  90  DO 110 I=1,LYAXIS
        DO 100 J=1,LXAXIS
          IGRAPH(I,J) = IZERO
 100    CONTINUE
 110  CONTINUE
C
      DO 120 I=1,NGRAPH
        I2 = ITWO * I
        I1 = I2 - IONE
        IARGS(I1) = IYARGS(I)
        IARGS(I2) = IXARGS(I)
 120  CONTINUE
C
      CALL HEADS (IARGS(1),NHEAD,IZERO,IONE)
C
      DO 130 I=1,NHEAD
        KIND(I) = IZERO
 130  CONTINUE
C
C     ==================================================================
C
C     WORK ON A PARTICULAR PLOT.
C
      DO 140 IPLOT=1,NGRAPH
        ISUB  = ITWO * IPLOT
        CALL ADRESS (ISUB-IONE,J)
        IF (J.LT.IZERO) CALL ERROR (20)
        CALL ADRESS (ISUB,K)
        IF (K.LT.IZERO) CALL ERROR (20)
 140  CONTINUE
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     SET UP THE PLOT FOR EACH GRAPH.
C
      CALL PLT24G (IGRAPH,NGRAPH,JGRAPH,LIMITY,LIMITX,YBLIM,YTLIM,
     1             XBLIM,XTLIM,YLABLE,XMIN,XMAX,X20,X40,X60,X80,
     2             NPTOUT,NPTIN,KB,KD,KS,KT,KW)
C
C     PRINT THE FIRST TWO GRAPHS.
C
      CALL PLT24T (IGRAPH,JGRAPH,YLABLE,XX,NPTOUT,NPTIN,KB,KD,KS,KT,KW)
C
      IF (NGRAPH.EQ.ITWO) RETURN
C
C     ..................................................................
C
C     PRINT THE LAST TWO GRAPHS.
C
      CALL PLT24B (IGRAPH,JGRAPH,YLABLE,XX,NPTOUT,NPTIN,KB,KD,KS,KT,KW)
      RETURN
C
C     ==================================================================
C
      END
*PLT24G
      SUBROUTINE PLT24G (IGRAPH,NGRAPH,JGRAPH,LIMITY,LIMITX,YBLIM,YTLIM,
     1                   XBLIM,XTLIM,YLABLE,XMIN,XMAX,X20,X40,X60,X80,
     2                   NPTOUT,NPTIN,KB,KD,KS,KT,KW)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PLT24G V 7.00  7/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PREPARE GRAPHS FOR TWOPLOTS OR FOURPLOTS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, DC 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -     JULY, 1991.

C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION JGRAPH(*)
      DIMENSION IGRAPH(21,*)
      DIMENSION KB(*), KD(*), KS(*), KT(*), KW(*)
      DIMENSION LIMITY(*), LIMITX(*)
      DIMENSION NPTOUT(*),  NPTIN(*)
C
C     FOR I = 1, ..., NGRAPH = NUMBER OF PLOTS.
C
C        LIMITX(I) = 0, IF LIMITS NOT SPECIFIED FOR X
C                    1, IF LIMITS     SPECIFIED FOR X
C        LIMITY(I) = 0, IF LIMITS NOT SPECIFIED FOR Y
C                    1, IF LIMITS     SPECIFIED FOR Y
C         XBLIM(I) = SPECIFIED LOWER LIMIT FOR X
C         XTLIM(I) = SPECIFIED UPPER LIMIT FOR X
C         YBLIM(I) = SPECIFIED LOWER LIMIT FOR Y
C         YTLIM(I) = SPECIFIED UPPER LIMIT FOR Y
C
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
      REAL             XBLIM(*), XTLIM(*) 
      REAL             XMIN(*), XMAX(*), X20(*), X40(*)
      REAL             X60 (*), X80 (*)
      REAL             YBLIM(*), YTLIM(*)
      REAL             YL(5), YLABLE(*)
      REAL             FDIV
      REAL             DELTA, HEIGHT, RATIOX, RATIOY, WIDTH
      REAL             XMAX2, XMIN2
      REAL             SPCA, SPCB, SPCC, SPCD
C
C     ................................................................
C
      CHARACTER        JGRAPH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C
      DATA SPCA / 0.20 /
      DATA SPCB / 0.40 /
      DATA SPCC / 0.60 /
      DATA SPCD / 0.80 /
C
C     ==================================================================
C
C
      LSUBY2 = IDIV (NRC,ITWO,IND) + IONE
      LSUBJG = IONE
      LBLEY  = 11
      LBLEY1 = LBLEY + IONE
C
      HEIGHT = 20.
      WIDTH  = 50.
C
      ISUBYL = IONE
      KSUBYL = 6
      JSUBYL = 11
      MSUBYL = 16
      NSUBYL = 21
      DO 120 IPLOT=1,NGRAPH
        CALL ADRESS (ITWO * IPLOT - IONE,J)
        CALL ADRESS (ITWO*IPLOT,K)
        ISUBY2 = LSUBY2
        DO 10 I=1,NRMAX
          A(ISUBY2) = RC(J)
          A(I)       = RC(K)
          ISUBY2     = ISUBY2 + IONE
          J          = J + IONE
          K          = K + IONE
  10    CONTINUE
C
C       ================================================================
C
C       OPERATE ON A PARTICULAR PLOT.
C
C       DETERMINE THE VALUES TO BE LISTED ON THE VERTICAL (Y) AXIS.
C
        IF (LIMITY(IPLOT).EQ.IZERO) GO TO 20
        YL(5) = YBLIM(IPLOT)
        YL(1) = YTLIM(IPLOT)
        GO TO 40
  20    YL(5)  = A(LSUBY2)
        YL(1)  = A(LSUBY2)
        ISUBY2 = LSUBY2
        DO 30 I=1,NRMAX
          IF (A(ISUBY2).LT.YL(5)) YL(5) = A(ISUBY2)
          IF (A(ISUBY2).GT.YL(1)) YL(1) = A(ISUBY2)
          ISUBY2 = ISUBY2 + IONE
  30    CONTINUE
C
  40    DELTA = YL(1) + YL(5)
        YL(2) = FDIV (5.*YL(5)+15.*YL(1),HEIGHT,IND)
        YL(3) = FDIV (DELTA,RTWO,IND)
        YL(4) = FDIV (15.*YL(5)+5.*YL(1),HEIGHT,IND)
C
C     SETUP FOR VERTICAL SCALE USING LBLEY CHARACTERS
C        IF MINIMUM WIDTH LE LBLEY, USE F LBLEY.MDY
C                         GT LBLEY, USE R FORMAT WITH LBLEY-5 S. DIGITS.
C
        KS(IPLOT) = MIN0 (ISIGD-IONE,LBLEY)
        CALL MINNW (YL(1),5,KS(IPLOT),LBLEY1,JGRAPH(LSUBJG),0,
     1              KW(IPLOT),KD(IPLOT),MWY,MDY)
        IF (MWY.GT.LBLEY) GO TO 50
        KT(IPLOT) = 7
        KW(IPLOT) = LBLEY
        KD(IPLOT) = MDY
        GO TO 60
  50    KS(IPLOT) = MIN0 (ISIGD,LBLEY-IFIVE)
        KT(IPLOT) = IONE
        CALL RFORMT (0,KS(IPLOT),YL,A(LSUBY2),5,LBLEY,KW(IPLOT),
     1               KD(IPLOT),JGRAPH(1),IRF)
  60    KB(IPLOT) = LBLEY - KW(IPLOT)
C
        YLABLE(ISUBYL) = YL(1)
        YLABLE(KSUBYL) = YL(2)
        YLABLE(JSUBYL) = YL(3)
        YLABLE(MSUBYL) = YL(4)
        YLABLE(NSUBYL) = YL(5)
C
C       DETERMINE XMIN, XMAX, XMID, X20 ( = THE 20% POINT), ETC.
C
        IF (LIMITX(IPLOT).EQ.IZERO) GO TO 70
        XMIN2 = XBLIM(IPLOT)
        XMAX2 = XTLIM(IPLOT)
        GO TO 90
  70    XMIN2 = A(1)
        XMAX2 = A(1)
        DO 80 I=1,NRMAX
          IF (A(I).LT.XMIN2) XMIN2 = A(I)
          IF (A(I).GT.XMAX2) XMAX2 = A(I)
  80    CONTINUE
C
  90    XMIN(IPLOT) = XMIN2
        XMAX(IPLOT) = XMAX2
        X20(IPLOT)  = SPCD * XMIN2 + SPCA * XMAX2
        X40(IPLOT)  = SPCC * XMIN2 + SPCB * XMAX2
        X60(IPLOT)  = SPCB * XMIN2 + SPCC * XMAX2
        X80(IPLOT)  = SPCA * XMIN2 + SPCD * XMAX2
C
C       DETERMINE THE (X,Y) PLOT POSITIONS AND PACK.
C
        IP = ITEN**(IPLOT-1)
        RATIOY = FDIV (HEIGHT,YL(1)-YL(5),NF)
        RATIOX = FDIV (WIDTH,XMAX(IPLOT)-XMIN(IPLOT),NF)
        NPTOUT(IPLOT) = IZERO
        ISUBY2        = LSUBY2 - IONE
        DO 110 I=1,NRMAX
          ISUBY2 = ISUBY2 + IONE
          MX     = RATIOX * (A(I)-XMIN(IPLOT)) + RHALF
          MX     = MX + IONE
          IF (MX.LT.IONE) GO TO 100
          IF (MX.GT.51) GO TO 100
          MY     = RATIOY * (A(ISUBY2)-YL(5)) + RHALF
          MY = 21 - MY
          IF (MY.LT.IONE) GO TO 100
          IF (MY.GT.21) GO TO 100
          IT = IDIV (IGRAPH(MY,MX),IP,IND)
          IF (IT.NE.ITWO) IGRAPH(MY,MX) = IGRAPH(MY,MX) + IP
          GO TO 110
 100      NPTOUT(IPLOT) = NPTOUT(IPLOT) + IONE
 110    CONTINUE
        NPTIN(IPLOT) = NRMAX - NPTOUT(IPLOT)
        ISUBYL = ISUBYL + 21
        KSUBYL = KSUBYL + 21
        JSUBYL = JSUBYL + 21
        MSUBYL = MSUBYL + 21
        NSUBYL = NSUBYL + 21
 120  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*PLT24T
      SUBROUTINE PLT24T (IGRAPH,JGRAPH,YLABLE,XX,NPTOUT,NPTIN,KB,KD,KS,
     1                   KT,KW)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PLT24T V 7.00  9/10/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PLOT FIRST TWO GRAPHS OF FOURPLOTS OR TWOPLOTS.
C
C     IF LWIDE = LWC, PLOT TWO GRAPHS ON TOP OF PAGE,
C                     OTERWISE PLOT GRAPH TWO BELOW GRAPH ONE.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  FEBRUARY, 1977.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IGRAPH(21,*)
      DIMENSION ISTORE(131), JGRAPH(*)
      DIMENSION JHOLD(21,52), NHOLD(21,12)
      DIMENSION KB(*), KD(*), KS(*), KT(*), KW(*)
      DIMENSION NPTOUT(*),  NPTIN(*)
C
      COMMON /ABCDEF/ LA(74)
C
C          X                 = LA(34),
C          PERIOD            = LA(38),   MINUS          = LA(39),
C          PLUS              = LA(40),   ASTERISK       = LA(41),
C          RIGHT PARENTHESIS = LA(43),   COMMA          = LA(44),
C          BLANK             = LA(45),   EQUALS         = LA(46).
C
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
      REAL             X(1)
      REAL             XX(*)
      REAL             YLABLE(*)
C
C     ................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER*1      ISTORE, JGRAPH, JHOLD, JSYM, NHOLD
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LYAXIS / 21 /
      DATA LXAXIS / 51 /
C
C     ==================================================================
C
      NRC2   = NRC/ITWO+IONE
      LSUBIS = IONE
      LSUBJG = IONE
      LXIS   = LXAXIS - IONE
      JGSTOP = LSUBJG + LXIS - IONE
      JXSTOP = LSUBJG + LXAXIS - IONE
      IXRLT  = LXAXIS + IONE
      IXRRT  = ITWO * LXAXIS
      JLSTOP = LSUBJG + IXRLT - IONE
      JRSTOP = LSUBJG + IXRRT - IONE
C
C     LBLEY  = NUMBER OF CHARACTERS ALLOWED FOR PRINTING VERTICAL SCALE.
C
      LBLEY  = 11
      LBLEY1 = LBLEY + IONE
      LBLEY2 = ITWO * LBLEY1
      LBLEY3 = LBLEY1 + IONE
      LSUBL1 = LSUBIS + LBLEY1 - IONE
      LSUBL2 = LSUBIS + LBLEY2 - IONE
      LSUBL3 = LSUBIS + LBLEY3 - IONE
      ISSTOP = LSUBIS + LBLEY1 - IONE
      KSUBIS = LSUBIS + 12
      KSSTOP = LSUBIS + 23
C
      IYMID  = IDIV (IONE+LYAXIS,ITWO,IND)
      IYUQ   = IDIV (LYAXIS-IONE,IFOUR,IND) + IONE
      IYLQ   = IDIV (ITHRE*(LYAXIS-IONE),IFOUR,IND) + IONE
C
C     PRINT TOP HALF OF PAGE.
C
      CALL PAGE (4)
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,160) (LHEAD(I),I=1,48)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,220) (LHEAD(I),I=1,24)
C
C     PRINT HORIZONTAL BORDER AT TOP.
C
C     LA(39) = 1H-
C     LA(40) = 1H+
C     LA(25) = 1HO
C
      ISUBJG = LSUBJG
      DO 20 J2=1,LXIS,10
        J3     = J2 + 9
        JSUBJG = LSUBJG + J2 - IONE
        DO 10 J1=J2,J3
          JGRAPH(JSUBJG) = LA(39)
          JSUBJG         = JSUBJG + IONE
  10    CONTINUE
        JGRAPH(ISUBJG) = LA(40)
        ISUBJG         = ISUBJG + ITEN
  20  CONTINUE
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,170) LA(45),
     1              (JGRAPH(J),J=LSUBJG,JGSTOP),LA(40),
     2              LA(45),LA(45),(JGRAPH(J),J=LSUBJG,JGSTOP),LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,170) LA(45),
     1              (JGRAPH(J),J=LSUBJG,JGSTOP),LA(40)
C
      DO 70 I=1,LYAXIS
        MARK = MOD (I,5)
        MT = 11
        IF (MARK.EQ.IONE) MT = KT(1)
        CALL RFORMT (MT,KS(1),A(NRC2),YLABLE(I),KB(1),0,
     1               KW(1),KD(1),ISTORE(LSUBIS),IRF)
        IF (MARK.EQ.IONE) MT = KT(2)
        ISTORE(LSUBL1) = LA(45)
        CALL RFORMT (MT,KS(2),A(NRC2),YLABLE(I+21),KB(2),0,
     1               KW(2),KD(2),ISTORE(LSUBL3),IRF)
        ISTORE(LSUBL2) = LA(45)
C
C       UNPACK PLOTING CHARACTER.
C
C
C       LA(45) = 1H
C       LA(38) = 1H.
C       LA(41) = 1H*
C
        ISUBJG = LSUBJG
        JSUBJG = LSUBJG + LXAXIS - IONE
        DO 30 J=1,LXAXIS
          I1 = MOD(IGRAPH(I,J),ITEN)
          IF (I1.EQ.IZERO) JGRAPH(ISUBJG) = LA(45)
          IF (I1.EQ.IONE) JGRAPH(ISUBJG)  = LA(38)
          IF (I1.EQ.ITWO) JGRAPH(ISUBJG)  = LA(41)
          JSUBJG = JSUBJG + IONE
          I2 = IDIV (MOD(IGRAPH(I,J),IHRD),ITEN,IND)
          IF (I2.EQ.IZERO) JGRAPH(JSUBJG) = LA(45)
          IF (I2.EQ.IONE) JGRAPH(JSUBJG)  = LA(38)
          IF (I2.EQ.ITWO) JGRAPH(JSUBJG)  = LA(41)
          ISUBJG = ISUBJG + IONE
  30    CONTINUE
C
C     PRINT PLOTS 1 AND 2.
C
        JSYM = LA(39)
        IF (I.EQ.IONE .OR. I.EQ.IYMID .OR. I.EQ.LYAXIS) JSYM = LA(40)
        IF (I.EQ.IYUQ .OR. I.EQ.IYLQ) JSYM = LA(40)
        IF (LWIDE.GE.LWC) GO TO 60
C
C     SAVE GRAPH TWO FOR LATER PLOTTING.
C
        JHOLD(I,1) = JSYM
        JJ         = IONE
        ISUBJG     = JLSTOP
        DO 40 IJ=IXRLT,IXRRT
          JJ = JJ + IONE
          JHOLD(I,JJ) = JGRAPH(ISUBJG)
          ISUBJG      = ISUBJG + IONE
  40    CONTINUE
C
        ISUBL1 = LSUBIS
        DO 50 IJ=1,LBLEY1
          NHOLD(I,IJ) = ISTORE(ISUBL1+12)
         ISUBL1       = ISUBL1 + IONE
  50    CONTINUE
        WRITE (IPRINT,180) (ISTORE(K),K=LSUBIS,ISSTOP), JSYM,
     1                     (JGRAPH(J),J=LSUBJG,JXSTOP), JSYM
        GO TO 70
C
C       PRINT PLOTS 1 AND 2 LINE BY LINE.
C
  60    WRITE (IPRINT,180) (ISTORE(K),K=LSUBIS,ISSTOP), JSYM,
     1     (JGRAPH(J),J=LSUBJG,JXSTOP),JSYM,(ISTORE(K),K=KSUBIS,KSSTOP),
     2         JSYM, (JGRAPH(J),J=JLSTOP,JRSTOP), JSYM
C
  70  CONTINUE
C
C     PRINT HORIZONTAL BORDER AT BOTTOM.
C
      ISUBJG = LSUBJG
      DO 90 J2=1,LXIS,10
        JSUBJG = LSUBJG + J2 - IONE
        J3 = J2 + 9
        DO 80 J1=J2,J3
          JGRAPH(JSUBJG) = LA(39)
          JSUBJG         = JSUBJG + IONE
  80    CONTINUE
C
        JGRAPH(ISUBJG) = LA(40)
        ISUBJG         = ISUBJG + ITEN
  90  CONTINUE
C
      IF (LWIDE.GE.LWC) WRITE (IPRINT,170) LA(45),
     1                (JGRAPH(J),J=LSUBJG,JGSTOP),LA(40),LA(45),LA(45),
     2                (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,170) LA(45),
     1                               (JGRAPH(J),J=LSUBJG,JGSTOP),LA(40)
C
C     PRINT HORIZONTAL SCALE.
C
      ISUBIS = LSUBIS
      DO 100 LJ=1,131
        ISTORE(ISUBIS) = LA(45)
        ISUBIS         = ISUBIS + IONE
 100  CONTINUE
C
      ISUBIS = LSUBIS + 6
      DO 130 LJ=1,24,4
C
C       LEFT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(LJ),IONE,MS,MS+IFIVE,ISTORE(ISUBIS),0,MW,MD,
     1               MWX,MDX)
        IF (MWX.LT.9) GO TO 110
        LT = IONE
        MS = IFOUR
        CALL RFORMT (0,MS,XX(LJ),X(1),1,9,MWX,MDX,ISTORE(LSUBIS),
     1               IRF)
 110    MMW = 9 - MWX
        CALL RFORMT (LT,MS,X,XX(LJ),MMW,0,MWX,MDX,ISTORE(ISUBIS),
     1               IRF)
C
C       RIGHT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(LJ+1),IONE,MS,MS+IFIVE,ISTORE(ISUBIS+66),0,
     1               MW,MD,MWX,MDX)
        IF (MWX.LT.9) GO TO 120
        LT = IONE
        MS = IFOUR
        CALL RFORMT (0,MS,XX(LJ+1),X(1),1,9,MWX,MDX,ISTORE(LSUBIS),
     1               IRF)
 120    MMW = 9 - MWX
        CALL RFORMT (LT,MS,X,XX(LJ+1),MMW,0,MWX,MDX,
     1               ISTORE(ISUBIS+66),IRF)
        ISUBIS = ISUBIS + ITEN
 130  CONTINUE
C
      JKSTOP = LSUBIS + 130
      LJSTOP = LSUBIS + 65
      JSUBIS = LJSTOP + IONE
      IF (LWIDE.GE.LWC) GO TO 150
C
C     PRINT SECOND GRAPH BELOW FIRST GRAPH BECAUSE LWIDE IS NOT 120.
C
      WRITE (IPRINT,190) (ISTORE(LJ),LJ=LSUBIS,LJSTOP)
      WRITE (IPRINT,200) NPTIN(1), NPTOUT(1)
      WRITE (IPRINT,210)
      WRITE (IPRINT,220) (LHEAD(I),I=25,48)
      WRITE (IPRINT,170) LA(45), (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40)
      DO 140 I=1,LYAXIS
C
C       PRINT PLOT 2 LINE BY LINE.
C
        WRITE (IPRINT,180) (NHOLD(I,J),J=1,LBLEY1), (JHOLD(I,K),K=1,52),
     1                      JHOLD(I,1)
 140  CONTINUE
C
      WRITE (IPRINT,170) LA(45),(JGRAPH(J),J= LSUBJG,JGSTOP),LA(40)
      WRITE (IPRINT,190) (ISTORE(LJ),LJ=JSUBIS,JKSTOP)
      WRITE (IPRINT,200) NPTIN(2),NPTOUT(2)
      RETURN
C
C     ..................................................................
C
 150  WRITE (IPRINT,190) (ISTORE(LJ),LJ=LSUBIS,JKSTOP)
      WRITE (IPRINT,200) NPTIN(1),NPTOUT(1),
     1                   NPTIN(2),NPTOUT(2)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 160  FORMAT (1H ,19X,8HPLOT OF ,12A1,8H VERSUS ,12A1,
     1            26X,8HPLOT OF ,12A1,8H VERSUS ,12A1)
 170  FORMAT (1H ,12X,53A1,13X,53A1)
 180  FORMAT (1H ,12A1,53A1,1X,12A1,53A1)
 190  FORMAT (1H ,131A1)
 200  FORMAT (/1X,1X,2(8X,I4,16H POINTS PLOTTED ,I4,
     1   33H POINTS OUT OF BOUNDS NOT PLOTTED)/)
 210  FORMAT (1H )
 220  FORMAT (1H ,19X,8HPLOT OF ,12A1,8H VERSUS ,12A1)
C
C     ==================================================================
C
      END


*POIPLT
      SUBROUTINE POIPLT (X,Y,Z,W,N,ALAMBA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. POIPLT V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A POISSON
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = ALAMBA).
C              THE PROTOTYPE POISSON DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL DISCRETE NON-NEGATIVE X,
C              AND HAS THE PROBABILITY FUNCTION
C              F(X) = (EXP(-ALAMBA)) * (ALAMBA**X) / (X FACTORIAL).
C              THE PROTOTYPE DISTRIBUTION RESTRICTIONS OF
C              DISCRETENESS AND NON-NEGATIVENESS
C              MENTIONED ABOVE DO NOT CARRY OVER TO THE
C              INPUT VECTOR X OF OBSERVATIONS TO BE ANALYZED.
C              THE INPUT OBSERVATIONS IN X MAY BE DISCRETE, CONTINUOUS,
C              NON-NEGATIVE, OR NEGATIVE.
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE POISSON PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE POISSON DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = ALAMBA.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ALAMBA = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                ALAMBA SHOULD BE POSITIVE.
C     OUTPUT--A ONE-PAGE POISSON PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--ALAMBA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT,
C                                         CHSCDF, NORPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--FOR LARGE VALUES OF ALAMBA (IN EXCESS OF 500.)
C              THIS SUBROUTINE USES THE NORMAL APPROXIMATION TO
C              THE POISSON.  THIS IS DONE TO SAVE EXECUTION TIME
C              WHICH INCREASES AS A FUNCTION OF ALAMBA AND WOULD
C              BE EXCESSIVE FOR LARGE VALUES OF ALAMBA.
C     REFERENCES--FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1969, PAGES 87-121.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--NOVEMBER  1974.
C     UPDATED         --AUGUST    1975.
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
C                   CURRENT VERSION - DECEMBER, 1989.
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
      REAL             W(*), X(*), Y(*), Z(*)
      REAL             ALAMBA
      REAL             ATEMP(1), V(1), YINT(1), YSLOPE(1)
      REAL             AN, ARG1, CC, CDF, CUTOFF, HOLD, PPF, SQALAM
      REAL             SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FSQRT
      REAL             SPCA, SPCB
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        M*1, MT*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10) /
     1      ',',  ' ',  'S',  'L',  'O',  'P',  'E',  ' ',  '=',   ' ' /
C
      DATA SPCA / 500.0 /
      DATA SPCB /   6.0 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENT FOR ERRORS.
C
      AN = N
      CUTOFF = SPCA
      IF (ALAMBA.GT.RZERO) GO TO 10
      CALL ERROR (38)
C
C     SORT THE DATA.
C
  10  CALL SORTPP (X,N,Y)
C
C     GENERATE UNIFORM ORDER STATISTIC MEDIANS.
C
      CALL UNIMED (N,W)
C
C     IF THE INPUT ALAMBA VALUE IS LARGE (IN EXCESS OF
C        CUTOFF VALUE OF 500.0), THEN USE THE NORMAL
C        APPROXIMATION TO THE POISSON.
C
      IF (ALAMBA.LE.CUTOFF) GO TO 30
      SQALAM = FSQRT(ALAMBA)
      DO 20 I=1,N
        CALL NORPPF (W(I),PPF,IND1)
        IF (IND1.NE.IZERO) CALL ERROR (249)
        W(I) = ALAMBA + PPF * SQALAM
  20  CONTINUE
C
      GO TO 170
  30  CONTINUE
C
C     DETERMINE WHICH UNIFORM ORDER STATISTIC MEDIAN IS ASSOCIATED
C        WITH THE CLOSEST INTEGER TO ALAMBA.
C
      DO 40 I=1,N
        Z(I) = -RONE
  40  CONTINUE
C
C
      ILAMBA = ALAMBA + RHALF
      ARG1 = RTWO * ALAMBA
      IARG2 = ITWO * (ILAMBA+IONE)
      CALL CHSCDF (ARG1,IARG2,CDF,IND1)
      IF (IND1.NE.IZERO) CALL ERROR (249)
      CDF = RONE - CDF
      DO 50 J=1,N
        IF (W(J).GT.CDF) GO TO 60
  50  CONTINUE
C
  60  JM1 = J - IONE
      Z(JM1) = ILAMBA
C
C     FILL IN THE POISSON ORDER STATISTIC MEDIANS BELOW ALAMBA.
C
      IMAX = SPCB * FSQRT(ALAMBA)
      DO 90 I=1,IMAX
        K = ILAMBA - I
        IF (K.LT.IZERO) GO TO 100
        IARG2 = ITWO * (K+IONE)
        CALL CHSCDF (ARG1,IARG2,CDF,IND1)
        IF (IND1.NE.IZERO) CALL ERROR (249)
        CDF = RONE - CDF
        DO 70 J=1,N
          IF (W(J).GT.CDF) GO TO 80
  70    CONTINUE
C
  80    JM1 = J - IONE
        IF (JM1.LE.IZERO) GO TO 100
        IF (Z(JM1).LT.(-RHALF)) Z(JM1) = K
  90  CONTINUE
C
C     FILL IN THE POISSON ORDER STATISTIC MEDIANS ABOVE ALAMBA.
C
 100  DO 130 I=1,IMAX
        K = ILAMBA + I
        IARG2 = ITWO * (K+IONE)
        CALL CHSCDF (ARG1,IARG2,CDF,IND1)
        IF (IND1.NE.IZERO) CALL ERROR (249)
        CDF = RONE - CDF
        DO 110 J=1,N
          IF (W(J).GT.CDF) GO TO 120
 110    CONTINUE
C
        Z(N) = K
        GO TO 140
 120    JM1 = J - IONE
        IF (Z(JM1).LT.(-RHALF)) Z(JM1) = K
 130  CONTINUE
C
C     FILL IN THE EMPTY HOLES IN THE POISSON ORDER STATISTIC MEDIAN
C        Z MATRIX WITH THE PROPER VALUES.
C     THEN FOR SAKE OF CONSISTENCY WITH OTHER
C        PROBABILITY PLOT SUBROUTINES, COPY THE Z VECTOR
C        INTO THE W VECTOR.
C
 140  HOLD = Z(N)
      DO 150 IREV=1,N
        I = N - IREV + IONE
        IF (Z(I).GE.(-RHALF)) HOLD = Z(I)
        IF (Z(I).LT.(-RHALF)) Z(I) = HOLD
 150  CONTINUE
C
      DO 160 I=1,N
        W(I) = Z(I)
 160  CONTINUE
C
C
 170  V(1) = ALAMBA
      CALL RFORMT (0,ISIGD,V,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,V(1),0,0,NW,ND,MT(1),IRF)
      IF (LWIDE.GE.108) WRITE (IPRINT,260) N, (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      IF (LWIDE.GE.NCW .AND. LWIDE.LT.108) WRITE (IPRINT,270)
     1     (LHEAD(I),I=1,12), N, (MT(J),J=1,NW)
      IF (LWIDE.LT.NCW) WRITE (IPRINT,280) (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      CALL PRPLOT (Y,W)
C
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      CALL SUMMAL (W,N,SUM2)
      IF (N.EQ.IONE) SUM2 = W(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = FDIV (SUM2,AN,IND)
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 180 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
 180  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 190 I=1,N
        ATEMP(1) = (Y(I)-YBAR) * (W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
 190  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 200 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
 200  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1) * WBAR
      CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 210 I=1,10
        MT(K) = M(I)
        K = K + IONE
 210  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+73.GT.LWIDE) GO TO 220
      WRITE (IPRINT,290) CC, (MT(J),J=1,K)
      GO TO 250
 220  CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 230 I=1,10
        MT(K) = M(I)
        K = K + IONE
 230  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+38.GT.LWIDE) GO TO 240
      WRITE (IPRINT,300) CC, (MT(J),J=1,K)
      GO TO 250
 240  IF (LWIDE.LT.37) GO TO 250
      WRITE (IPRINT,310) CC
 250  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 260  FORMAT (15X, 7HPOISSON           ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 270  FORMAT ( 1X, 7HPOISSON           ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 280  FORMAT ( 1X, 7HPOISSON      ,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 290  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 300  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 310  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*PRBCDF
      SUBROUTINE PRBCDF
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRBCDF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM FOR COMPUTING CUMULATIVE DISTRIBUTION FUNCTIONS.
C
C     EXAMPLE -
C        NORMAL CUMULATIVE OF (E) PUT IN COLUMN (C)
C
C     ..................................................................
C
C          *** VALUES OF L2 FOR 30 UNIVARIATE DISTRIBUTIONS ***
C                     CONTINUOUS  1-18
C                     NONCENTRAL 19-21
C                       DISCRETE 22-30
C
C      1. NORMAL            2. LOG-NORMAL           3. HALF-NORMAL
C      4. STUDENTS T        5. CHI SQUARED          6. GAMMA
C      7. F                 8. BETA                 9. STUDENTIZED RANGE
C     10. UNIFORM          11. CAUCHY              12. TUKEY LAMBDA
C     13. EXTREME VALUE    14. WEIBULL             15. PARETO
C     16. EXPONENTIAL      17. DOUBLE EXPONENTIAL  18. LOGISTIC
C     19. NONCENTRAL T     20. NONCENTRAL CHISQ    21. NONCENTRAL F
C     22. BERNOULLI        23. BINOMIAL            24. NEGATIVE BINOMIAL
C     25. POISSON          26. GEOMETRIC           27. HYPERGEOMETRIC
C     28. DISCRETE         29. PASCAL              30. MULTINOMIAL
C
C     ..................................................................
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ARG, CDF, DELTA, PARAM, PARAM1, PARAM2
      REAL             PAR1, PAR2, V1, V2, X, XI
      REAL             FDIV, FDPCON, FLOG
C
      DOUBLE PRECISION DX, DPARAM
      DOUBLE PRECISION BETAX, BETAAB, DPDF, DCDF
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV, FDEXP
      DOUBLE PRECISION DATAN
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      L = MOD(L2,30)
        IF (L.EQ.IZERO) L = 30
      DPARAM = DZERO
C
      GO TO (10,10,10,10,10,10,10,10,30,10,
     1       10,10,10,10,10,10,10,10,30,30,
     2       30,30,10,10,10,10,30,30,30,30), L
C
 10   IF (NRMAX.GT.IZERO) GO TO 20
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
C     CHECK ON NUMBER OF ARGUMENTS.
C
 20   IF (NARGS.LT.ITWO) GO TO 60
C
      GO TO (70,70,70,50,50,50,40,40,30,70,
     1       70,50,50,50,50,70,70,70,50,30,
     2       30,30,40,40,50,50,30,30,30,30), L
C
C     INSTRUCTION DOES NOT EXIST.
C
 30   CALL ERROR ( 1)
      RETURN
C
C     ..................................................................
C
 40   IF (NARGS.NE.IFOUR) GO TO 60
      CALL ADRESS (ITWO,J2)
      IF (J2.LT.IZERO) PARAM1 = ARGS(2)
      CALL ADRESS (ITHRE,J3)
      IF (J3.LT.IZERO) PARAM2 = ARGS(3)
      GO TO 70
C
  50  PARAM  = RZERO
C
      IF (NARGS.NE.ITHRE .AND. L2.NE.13) GO TO 60
      IF (NARGS.EQ.ITWO .AND. L2.EQ.13) GO TO 70
      CALL ADRESS (ITWO,J2)
      IF (J2.GE.IZERO) GO TO 70
      PARAM  = ARGS(2)
      DPARAM = PARAM
      GO TO 70
C
 60   CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 70   CALL ADRESS (NARGS,K)
      CALL ADRESS (IONE,J1)
      IF (J1.GE.IZERO) GO TO 80
      X  = ARGS(1)
      DX = X
  80  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DELTA  = RTEN * RER
C
      NRGSM1 = NARGS - IONE
      KNDSUM = IZERO
      DO 90 I=1,NRGSM1
        KNDSUM = KNDSUM + KIND(I)
  90  CONTINUE
C
C     IXTIND = FAULT INDICATOR FOR ERROR 235.
C     JXTIND = FAULT INDICATOR FOR ERROR 249.
C     KXTIND = FAULT INDICATOR FOR ERROR 258.
C     IDFIND = FAULT INDICATOR FOR ERROR 207.
C     JDFIND = FAULT INDICATOR FOR ERROR 208.
C
      IXTIND = IZERO
      JXTIND = IZERO
      KXTIND = IZERO
      IDFIND = IZERO
      JDFIND = IZERO
C
      DO 500 I=1,NRMAX
        IF (I.GT.IONE .AND. KNDSUM.EQ.NRGSM1) GO TO 440
        IF (KIND(1).EQ.IZERO) X  = RC(J1)
        IF (KIND(1).EQ.IZERO) DX = X
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) PARAM  = RC(J2)
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) DPARAM = PARAM
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM1 = RC(J2)
        IF (KIND(3).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM2 = RC(J3)
        IND = IZERO
C
        GO TO (101,102,103,104,105,106,107,108, 30,110,
     1         111,112,113,114,115,116,117,118, 30, 30,
     2          30, 30,123,124,125,126, 30, 30, 30, 10), L
C
C     ..................................................................
C
C       NORMAL.
C
 101    CALL NORCDF (X,CDF)
        GO TO 430
C
C       LOG-NORMAL.
C
 102    IF (X.LE.RZERO) GO TO 330
        ARG = FLOG (X)
        CALL NORCDF (ARG,CDF)
        GO TO 430
C
C       HALF-NORMAL.
C
 103    IF (X.LE.RZERO) GO TO 330
        CALL NORCDF (X,CDF)
        CDF = RTWO * CDF - RONE
        GO TO 430
C
C       STUDENTS T.
C
 104    NU = PARAM + DELTA
        IF (NU.LT.IONE) IDFIND = IDFIND + IONE
        IF (NU.LT.IONE) NU = IONE
        IF (PARAM.NE.AINT(PARAM)) JDFIND = JDFIND + IONE
        CALL   TCDF (X,NU,CDF,IND)
        GO TO 420
C
C       CHISQUARE.
C
 105    NU = PARAM + DELTA
        IF (NU.LT.IONE) IDFIND = IDFIND + IONE
        IF (NU.LT.IONE) NU = IONE
        IF (PARAM.NE.AINT(PARAM)) JDFIND = JDFIND + IONE
        CALL CHSCDF (X,NU,CDF,IND)
        IF (IND.EQ.ITHRE) JXTIND = JXTIND + IONE
        GO TO 420
C
C       GAMMA.
C
 106    CALL GAMCDF (X,PARAM,CDF,IND)
        IF (IND.NE.IZERO) GO TO 206
        GO TO 420
C
C       F.
C
 107    NU1  = PARAM1 + DELTA
        NU2  = PARAM2 + DELTA
        V1   = NU1
        V2   = NU2
        PAR1 = FDIV (V2,RTWO,JIND)
        PAR2 = FDIV (V1,RTWO,JIND)
        XI   = FDIV (V2,V2+V1*X,JIND)
        CALL DIXAB (PAR1,PAR2,XI,BETAX,BETAAB,DPDF,DCDF,IND)
        IF (IND.NE.IZERO) GO TO 208
        CDF = FDPCON (DONE-DCDF)
        GO TO 410
C
C       BETA.
C
 108    CALL DIXAB (PARAM1,PARAM2,X,BETAX,BETAAB,DPDF,DCDF,IND)
        CDF = FDPCON (DCDF)
        IF (IND.NE.IZERO) GO TO 208
        GO TO 410
C
C       UNIFORM.
C
 110    IF (X.LT.RZERO) GO TO 330
        IF (X.GT.RONE ) GO TO 331
        CDF = X
        GO TO 430
C
C       CAUCHY.
C
 111    CDF = FDPCON ( DHALF + FDDIV (DONE,DPI,JIND) * DATAN (DX) )
        GO TO 430
C
C       LAMBDA.
C
 112    CALL LAMCDF (X,PARAM,CDF,IND)
        IF (IND.EQ.IONE) CALL ERROR (114)
        GO TO 420
C
C       EXTREME.
C
 113    IF (NARGS.EQ.ITWO) CDF = FDPCON ( DONE - FDEXP ( -FDEXP(-DX) ) )
        IF (NARGS.EQ.ITHRE .AND.     X.LE.RZERO) GO TO 321
        IF (NARGS.EQ.ITHRE .AND. PARAM.LE.RZERO) GO TO 325
        IF (NARGS.EQ.ITHRE .AND. X.NE.RZERO) CDF = FDPCON (
     1                                    FDEXP ( - (DX**(-DPARAM) ) ) )
        IF (NARGS.EQ.ITWO)  GO TO 430
        IF (NARGS.EQ.ITHRE) GO TO 420
C
C       WEIBULL.
C
 114    IF (X.LE.RZERO .OR. PARAM.LE.RZERO) GO TO 214
        CDF = FDPCON ( DONE - FDEXP (-(DX**DPARAM) ) )
        GO TO 420
C
C       PARETO.
C
 115    IF (X.LT.RONE .OR. PARAM.LE.RZERO) GO TO 215
        CDF = FDPCON ( DONE - DX ** (-DPARAM) )
        GO TO 420
C
C       EXPONENTIAL.
C
 116    IF (X.LT.RZERO) GO TO 330
        CDF = FDPCON ( DONE - FDEXP (-DX) )
        GO TO 430
C
C       DOUBLE EXPONENTIAL.
C
 117    IF (X.LE.RZERO) CDF = FDPCON ( DHALF * FDEXP (DX) )
        IF (X.GT.RZERO) CDF = FDPCON ( DONE - DHALF * FDEXP (-DX) )
        GO TO 430
C
C       LOGISTIC.
C
 118    IF (X.LT.RZERO) CDF = FDPCON (
     1                           FDDIV (FDEXP(DX),DONE+FDEXP(DX),JIND) )
        IF (X.GE.RZERO) CDF = FDPCON (FDDIV (DONE,DONE+FDEXP(-DX),JIND))
        GO TO 430
C
C       BINOMIAL.
C
 123    NPAR = PARAM1 + DELTA
        CALL BINCDF (X,PARAM2,NPAR,CDF,IND)
        IF (IND.NE.IZERO) GO TO 223
        GO TO 410
C
C       NEGATIVE BINOMIAL.
C
 124    NPAR = PARAM1 + DELTA
        CALL  NBCDF (X,PARAM2,NPAR,CDF,IND)
        IF (IND.NE.IZERO) GO TO 223
        GO TO 410
C
C       POISSON.
C
 125    CALL POICDF (X,PARAM,CDF,IND)
        IF (IND.NE.IZERO) GO TO 225
        GO TO 420
C
C       GEOMETRIC.
C
 126    IF (X.LT.RZERO) GO TO 226
        IF (PARAM.LE.RZERO .OR. PARAM.GE.RONE) GO TO 325
        CDF = FDPCON ( DONE - (DONE-DPARAM) ** (DX+DONE) )
        GO TO 420
C
C     ..................................................................
C
 206    IF (IND.EQ.IONE)  CALL ERROR (114)
        IF (IND.EQ.ITWO)  CALL ERROR (115)
        IF (IND.EQ.ITHRE) JXTIND = JXTIND + IONE
        GO TO 420
C
 208    IF (IND.EQ.ITWO .OR. IND.EQ.IFIVE) JXTIND = JXTIND + IONE
        IF (IND.EQ.IONE) JDFIND = JDFIND + IONE
        IF (IND.EQ.ITHRE) CALL ERROR (114)
        IF (IND.EQ.IFOUR) KXTIND = KXTIND + IONE
        IF (IND.EQ.6) CALL ERROR (115)
        GO TO 410
C
 214    IF (X.LE.RZERO .AND. PARAM.LE.RZERO) GO TO 320
        IF (X.LE.RZERO) GO TO 321
        GO TO 325
C
 215    IF (X.LT.RONE .AND. PARAM.LE.RZERO) GO TO 320
        IF (X.LE.RONE) GO TO 321
        GO TO 325
C
 223    IF (IND.EQ.IONE)  CALL ERROR (114)
        IF (IND.EQ.ITWO)  IXTIND = IONE
        IF (IND.EQ.ITHRE) CALL ERROR (115)
        IF (IND.EQ.IFOUR) CALL ERROR (115)
        GO TO 410
C
 225    IF (IND.EQ.IONE)  CALL ERROR (114)
        IF (IND.EQ.ITWO)  IXTIND = IONE
        IF (IND.EQ.ITHRE) CALL ERROR (115)
        GO TO 420
C
 226    IF (PARAM.LE.RZERO .OR. PARAM.GE.RONE) GO TO 320
        GO TO 321
C
C     ..................................................................
C
 320    CALL ERROR (115)
 321    CDF = RZERO
        CALL ERROR (114)
        GO TO 420
C
 325    CDF = RZERO
        CALL ERROR (115)
        GO TO 420
C
 330    CDF = RZERO
        CALL ERROR (114)
        GO TO 430
C
 331    CDF = RONE
        KXTIND = KXTIND + IONE
        GO TO 430
C
C     ..................................................................
C
 410    IF (KIND(3).EQ.IONE) GO TO 420
        J3 = J3 + IONE
 420    IF (KIND(2).EQ.IONE) GO TO 430
        J2 = J2 + IONE
 430    IF (KIND(1).EQ.IONE) GO TO 440
        J1 = J1 + IONE
 440    RC(K) = CDF
        K  = K + IONE
 500  CONTINUE
C
C     ..................................................................
C
      IF (IXTIND.NE.IZERO) CALL ERROR (235)
      IF (JXTIND.NE.IZERO) CALL ERROR (249)
      IF (KXTIND.NE.IZERO) CALL ERROR (258)
      IF (IDFIND.NE.IZERO) CALL ERROR (207)
      IF (JDFIND.NE.IZERO) CALL ERROR (208)
      RETURN
C
C     ==================================================================
C
      END
*PRBPDF
      SUBROUTINE PRBPDF
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRBPDF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM FOR COMPUTING PROBABILITY DENSITY FUNCTIONS.
C
C     EXAMPLE -
C        NORMAL DENSITY OF (E) PUT IN COLUMN (C)
C
C     ..................................................................
C
C          *** VALUES OF L2 FOR 30 UNIVARIATE DISTRIBUTIONS ***
C                     CONTINUOUS  1-18
C                     NONCENTRAL 19-21
C                       DISCRETE 22-30
C
C      1. NORMAL            2. LOG-NORMAL           3. HALF-NORMAL
C      4. STUDENTS T        5. CHI SQUARED          6. GAMMA
C      7. F                 8. BETA                 9. STUDENTIZED RANGE
C     10. UNIFORM          11. CAUCHY              12. TUKEY LAMBDA
C     13. EXTREME VALUE    14. WEIBULL             15. PARETO
C     16. EXPONENTIAL      17. DOUBLE EXPONENTIAL  18. LOGISTIC
C     19. NONCENTRAL T     20. NONCENTRAL CHISQ    21. NONCENTRAL F
C     22. BERNOULLI        23. BINOMIAL            24. NEGATIVE BINOMIAL
C     25. POISSON          26. GEOMETRIC           27. HYPERGEOMETRIC
C     28. DISCRETE         29. PASCAL              30. MULTINOMIAL
C
C     ..................................................................
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DELTA, PARAM1, PARAM2,PDF, X, XI
      REAL             FDPCON
C
      DOUBLE PRECISION DX, DPARAM, DMULT
      DOUBLE PRECISION BETAX, BETAAB, DPDF, DCDF
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG, FDSQRT
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      L = MOD(L2,30)
        IF (L.EQ.IZERO) L = 30
C
      GO TO (10,10,10,30,30,30,30,10,30,10,
     1       10,10,10,10,10,10,10,10,30,30,
     2       30,30,10,10,10,10,30,30,30,30), L
C
 10   IF (NRMAX.GT.IZERO) GO TO 20
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
C    CHECK ON NUMBER OF ARGUMENTS.
C
 20   IF (NARGS.LT.ITWO) GO TO 60
C
      GO TO (70,70,70,50,50,50,40,40,30,70,
     1       70,50,50,50,50,70,70,70,50,30,
     2       30,30,40,40,50,50,30,30,30,30), L
C
C     INSTRUCTION DOES NOT EXIST.
C
 30   CALL ERROR ( 1)
      RETURN
C
C     ..................................................................
C
C     TWO PARAMETERS SPECIFIED.
C
 40   IF (NARGS.NE.IFOUR) GO TO 60
      CALL ADRESS (ITWO,J2)
      IF (J2.LT.IZERO) PARAM1 = ARGS(2)
      CALL ADRESS (ITHRE,J3)
      IF (J3.LT.IZERO) PARAM2 = ARGS(3)
      GO TO 70
C
C     ONE PARAMETER SPECIFIED.
C
  50  PARAM  = RZERO
C
      IF (NARGS.NE.ITHRE .AND. L2.NE.13) GO TO 60
      IF (NARGS.EQ.ITWO .AND. L2.EQ.13) GO TO 70
      CALL ADRESS (ITWO,J2)
      IF (J2.GE.IZERO) GO TO 70
      PARAM  = ARGS(2)
      DPARAM = PARAM
      GO TO 70
C
  60  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     NO PARAMETER SPECIFIED.
C
 70   CALL ADRESS (NARGS,K)
      CALL ADRESS (IONE,J1)
      IF (J1.GE.IZERO) GO TO 80
      X  = ARGS(1)
      DX = X
  80  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DELTA  = RTEN * RER
C
      NRGSM1 = NARGS - IONE
      KNDSUM = IZERO
      DO 90 I=1,NRGSM1
        KNDSUM = KNDSUM + KIND(I)
  90  CONTINUE
C
C     IXTIND = FAULT INDICATOR FOR ERROR 235.
C     JXTIND = FAULT INDICATOR FOR ERROR 249.
C     IDFIND = FAULT INDICATOR FOR ERROR 208.
C
      IXTIND = IZERO
      JXTIND = IZERO
      IDFIND = IZERO
C
      DO 500 I=1,NRMAX
        IF (I.GT.IONE .AND. KNDSUM.EQ.NRGSM1) GO TO 440
        IF (KIND(1).EQ.IZERO) X  = RC(J1)
        IF (KIND(1).EQ.IZERO) DX = X
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) PARAM  = RC(J2)
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) DPARAM = PARAM
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM1 = RC(J2)
        IF (KIND(3).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM2 = RC(J3)
        IND = IZERO
C
        GO TO (101,102,103, 30, 30, 30, 30,108, 30,110,
     1         111,112,113,114,115,116,117,118, 30, 30,
     2          30, 30,123,124,125,126, 30, 30, 30, 10), L
C
C     ..................................................................
C
C       NORMAL.
C
 101    DMULT = FDDIV (DONE,FDSQRT(DTWO*DPI),JIND)
        PDF   = FDPCON ( DMULT * FDEXP (-FDDIV (DX**2,DTWO,JIND) ) )
        GO TO 430
C
C       LOG-NORMAL.
C
 102    IF (X.LE.RZERO) GO TO 330
        DMULT = FDDIV (DONE,DX*FDSQRT(DTWO*DPI),JIND)
        PDF   = FDPCON ( DMULT * FDEXP (-FDDIV(FDLOG(DX)**2,DTWO,JIND)))
        GO TO 430
C
C       HALF-NORMAL.
C
 103    IF (X.LT.RZERO) GO TO 330
        DMULT = FDSQRT ( FDDIV (DTWO,DPI,JIND) )
        PDF   = FDPCON ( DMULT * FDEXP (-FDDIV(DX**2,DTWO,JIND)) )
        GO TO 430
C
C       BETA.
C
 108    CALL DIXAB (PARAM1,PARAM2,X,BETAX,BETAAB,DPDF,DCDF,IND)
C
C      PROCEDURE DIXAB COMPUTES DPDF = X**A*(1-X)**B/B(A,B).
C      THE DEFINITON OF BETA P.D.F. IS X**(A-1)*(1-X)**(B-1)/B(A,B).
C
        PDF = FDIV (FDPCON (DPDF),X * (RONE - X),IND)
        IF (IND.NE.IZERO) GO TO 208
        GO TO 410
C
C       UNIFORM.
C
 110    IF (X.LT.RZERO .OR. X.GT.RONE) GO TO 330
        PDF = RONE
        GO TO 430
C
C       CAUCHY.
C
 111    DMULT = FDDIV (DONE,DPI,JIND)
        PDF   = FDPCON ( DMULT * FDDIV (DONE,DONE+DX**2,JIND) )
        GO TO 430
C
C       LAMBDA
C
 112    CALL LAMPDF (X,PARAM,PDF,IND)
        IF (IND.NE.IZERO) GO TO 321
        GO TO 420
C
C       EXTREME.
C
 113    IF (NARGS.EQ.ITWO)  PDF = FDPCON (FDEXP(-DX)*FDEXP(-FDEXP(-DX)))
        IF (NARGS.EQ.ITHRE .AND.     X.LE.RZERO) GO TO 321
        IF (NARGS.EQ.ITHRE .AND. PARAM.LE.RZERO) GO TO 325
        IF (NARGS.EQ.ITHRE) PDF = FDPCON ( DPARAM *
     1                     (DX**(-DPARAM-DONE))*FDEXP(-(DX**(-DPARAM))))
        IF (NARGS.EQ.ITWO)  GO TO 430
        IF (NARGS.EQ.ITHRE) GO TO 420
C
C       WEIBULL.
C
 114    IF (X.LE.RZERO .OR. PARAM.LE.RZERO) GO TO 214
        PDF = FDPCON ( DPARAM*(DX**(DPARAM-DONE))*FDEXP (-(DX**DPARAM)))
        GO TO 420
C
C       PARETO.
C
 115    IF (X.LT.RONE .OR. PARAM.LE.RZERO) GO TO 215
        PDF = FDPCON ( FDDIV (DPARAM,DX**(DPARAM+DONE),JIND) )
        GO TO 420
C
C       EXPONENTIAL.
C
 116    IF (X.LT.RZERO) GO TO 330
        PDF = FDPCON ( FDEXP (-DX) )
        GO TO 430
C
C       DOUBLE EXPONENTIAL.
C
 117    PDF = FDPCON ( DHALF * FDEXP (-DABS(DX)) )
        GO TO 430
C
C       LOGISTIC.
C
 118    PDF = FDPCON ( FDDIV (FDEXP(DX),(DONE+FDEXP(DX))**2,JIND) )
        GO TO 430
C
C       BINOMIAL.
C
 123    N  = PARAM1 + DELTA
        NX = X + DELTA
        XI = FLOAT (NX)
        CALL BINPDF (XI,N,PARAM2,PDF,IND)
        IF (IND.NE.IZERO) GO TO 223
        GO TO 410
C
C       NEGATIVE BINOMIAL.
C
 124    N  = PARAM1 + DELTA
        NX = X + DELTA
        XI = FLOAT (NX)
        CALL NBPDF (XI,N,PARAM2,PDF,IND)
        IF (IND.NE.IZERO) GO TO 223
        GO TO 410
C
C       POISSON.
C
 125    NX = X + DELTA
        XI = FLOAT (NX)
        CALL POIPDF (XI,PARAM,PDF,IND)
        IF (IND.NE.IZERO) GO TO 225
        GO TO 420
C
C       GEOMETRIC.
C
 126    IF (X.LT.RZERO) GO TO 226
        IF (PARAM.LE.RZERO .OR. PARAM.GE.RONE) GO TO 325
        PDF = DPARAM * (DONE-DPARAM) ** DX
        GO TO 420
C
C     ..................................................................
C
 208    IF (IND.EQ.ITWO .OR. IND.EQ.IFIVE) JXTIND = JXTIND + IONE
        IF (IND.EQ.IONE) IDFIND = IDFIND + IONE
        IF (IND.EQ.ITHRE .OR. IND.EQ.IFOUR) CALL ERROR (114)
        IF (IND.EQ.6) CALL ERROR (115)
        GO TO 410
C
 214    IF (X.LE.RZERO .AND. PARAM.LE.RZERO) GO TO 320
        IF (X.LE.RZERO) GO TO 321
        GO TO 325
C
 215    IF (X.LT.RONE .AND. PARAM.LE.RZERO) GO TO 320
        IF (X.LE.RONE) GO TO 321
        GO TO 325
C
 223    IF (IND.LE.ITWO)  GO TO 315
        IF (IND.EQ.ITHRE) GO TO 310
        IF (IND.EQ.IFOUR) IXTIND = IONE
        GO TO 410
C
 225    IF (IND.LE.ITWO)  GO TO 325
        IF (IND.EQ.ITHRE) GO TO 321
        IF (IND.EQ.IFOUR) IXTIND = IONE
        GO TO 420
C
 226    IF (PARAM.LE.RZERO .OR. PARAM.GE.RONE) GO TO 320
        GO TO 321
C
C     ..................................................................
C
 310    PDF = RZERO
        CALL ERROR (114)
        GO TO 410
C
 315    PDF = RZERO
        CALL ERROR (115)
        GO TO 410
C
 320    CALL ERROR (115)
 321    PDF = RZERO
        CALL ERROR (114)
        GO TO 420
C
 325    PDF = RZERO
        CALL ERROR (115)
        GO TO 420
C
 330    PDF = RZERO
        CALL ERROR (114)
        GO TO 430
C
C     ..................................................................
C
 410    IF (KIND(3).EQ.IONE) GO TO 420
        J3 = J3 + IONE
 420    IF (KIND(2).EQ.IONE) GO TO 430
        J2 = J2 + IONE
 430    IF (KIND(1).EQ.IONE) GO TO 440
        J1 = J1 + IONE
 440    RC(K) = PDF
        K  = K + IONE
 500  CONTINUE
C
C     ..................................................................
C
      IF (IXTIND.NE.IZERO) CALL ERROR (235)
      IF (JXTIND.NE.IZERO) CALL ERROR (249)
      IF (IDFIND.NE.IZERO) CALL ERROR (208)
      RETURN
C
C     ==================================================================
C
      END
*PRBPLT
      SUBROUTINE PRBPLT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRBPLT V 7.00  7/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRODUCE PROBABILITY PLOTS.
C
C          ... VALUES OF L2 FOR 30 UNIVARIATE DISTRIBUTIONS ...
C                     CONTINUOUS  1-18
C                     NONCENTRAL 19-21
C                       DISCRETE 22-30
C
C      1. NORMAL            2. LOG-NORMAL           3. HALF-NORMAL
C      4. STUDENTS T        5. CHI SQUARED          6. GAMMA
C      7. F                 8. BETA                 9. STUDENTIZED RANGE
C     10. UNIFORM          11. CAUCHY              12. TUKEY LAMBDA
C     13. EXTREME VALUE    14. WEIBULL             15. PARETO
C     16. EXPONENTIAL      17. DOUBLE EXPONENTIAL  18. LOGISTIC
C     19. NONCENTRAL T     20. NONCENTRAL CHISQ    21. NONCENTRAL F
C     22. BERNOULLI        23. BINOMIAL            24. NEGATIVE BINOMIAL
C     25. POISSON          26. GEOMETRIC           27. HYPERGEOMETRIC
C     28. DISCRETE         29. PASCAL              30. MULTINOMIAL
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1975.
C                   CURRENT VERSION -     JULY, 1992.
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
C
      INCLUDE 'WRKSCR.H'
C
      REAL             HOLD, PARAM
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 30 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      L = MOD (L2,ICA)
      IF (L.EQ.IZERO) L = ICA
C
      GO TO (20,20,20,10,10,  20,10,10,10,20,
     1       20,20,20,20,20,  20,20,20,10,10,
     2       10,10,10,10,20,  10,10,10,10,10), L
C
  10  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
  20  IF (NRMAX.GT.IZERO) GO TO 30
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  30  IF (NRMAX.LE.NS2) GO TO 40
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
  40  IF (L.NE.25 .OR. NRMAX.LE.IDIV(NS,ITHRE,IND)) GO TO 50
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
  50  GO TO (70,70,70,60,60,  80,60,60,60,70,
     1       70,80,70,80,80,  70,70,70,60,60,
     2       60,60,60,60,80,  60,60,60,60,60), L
C
  60  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
  70  IF (NARGS.EQ.IONE) GO TO 110
      IF (L.EQ.13) GO TO 80
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  80  IF (KIND(1).EQ.IONE) GO TO 90
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  90  IF (NARGS.EQ.ITWO) GO TO 100
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 100  PARAM = ARGS(1)
      IF (L.EQ.12) GO TO 110
      IF (ARGS(1).GT.RZERO) GO TO 110
      CALL ERROR (206)
      RETURN
C
C     ..................................................................
C
 110  CALL ADRESS (NARGS,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
      HOLD = RC(J)
      K = J
      DO 120 I=1,NRMAX
        IF (RC(K).NE.HOLD) GO TO 130
        K = K + IONE
 120  CONTINUE
      CALL ERROR (248)
      RETURN
C
C     ..................................................................
C
 130  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      LNGTH = NRMAX
      M = NRMAX + IONE
      IF (NCRT.EQ.IZERO) THEN
        CALL PAGE (IFOUR)
      ELSE
        CALL PAGE (IZERO)
      ENDIF
      CALL HEADS (IARGS(NARGS),IONE,IZERO,IONE)
C
      GO TO (150,160,170,140,140,  180,140,140,140,190,
     1       200,210,220,230,240,  250,260,270,140,140,
     2       140,140,140,140,280,  140,140,140,140,140), L
C
 140  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
 150  CALL NORPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 160  CALL LGNPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 170  CALL HFNPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 180  CALL GAMPLT (RC(J),A(1),A(M),LNGTH,PARAM)
      RETURN
C
C     ..................................................................
C
 190  CALL UNIPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 200  CALL CAUPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 210  CALL LAMPLT (RC(J),A(1),A(M),LNGTH,PARAM)
      RETURN
C
C     ..................................................................
C
 220  IF (NARGS.EQ.IONE) CALL EV1PLT (RC(J),A(1),A(M),LNGTH)
      IF (NARGS.EQ.ITWO) CALL EV2PLT (RC(J),A(1),A(M),LNGTH,PARAM)
      RETURN
C
C     ..................................................................
C
 230  CALL WEIPLT (RC(J),A(1),A(M),LNGTH,PARAM)
      RETURN
C
C     ..................................................................
C
 240  CALL PARPLT (RC(J),A(1),A(M),LNGTH,PARAM)
      RETURN
C
C     ..................................................................
C
 250  CALL EXPPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 260  CALL DEXPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 270  CALL LOGPLT (RC(J),A(1),A(M),LNGTH)
      RETURN
C
C     ..................................................................
C
 280  MM = M + NRMAX
      CALL POIPLT (RC(J),A(1),A(M),A(MM),LNGTH,PARAM)
      RETURN
C
C     ==================================================================
C
      END
*PRBPPF
      SUBROUTINE PRBPPF
C
C **  NBS OMNITAB 1980 VERSION 6.04 11/ 9/84. PRBPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM FOR COMPUTING PERCENT POINT FUNCTIONS.
C            = 100, OTHER.
C
C     EXAMPLE -
C        NORMAL PERCENTILE OF (E) PUT IN COLUMN (C)
C
C     ..................................................................
C
C          *** VALUES OF L2 FOR 30 UNIVARIATE DISTRIBUTIONS ***
C                     CONTINUOUS  1-18
C                     NONCENTRAL 19-21
C                       DISCRETE 22-30
C
C      1. NORMAL            2. LOG-NORMAL           3. HALF-NORMAL
C      4. STUDENTS T        5. CHI SQUARED          6. GAMMA
C      7. F                 8. BETA                 9. STUDENTIZED RANGE
C     10. UNIFORM          11. CAUCHY              12. TUKEY LAMBDA
C     13. EXTREME VALUE    14. WEIBULL             15. PARETO
C     16. EXPONENTIAL      17. DOUBLE EXPONENTIAL  18. LOGISTIC
C     19. NONCENTRAL T     20. NONCENTRAL CHISQ    21. NONCENTRAL F
C     22. BERNOULLI        23. BINOMIAL            24. NEGATIVE BINOMIAL
C     25. POISSON          26. GEOMETRIC           27. HYPERGEOMETRIC
C     28. DISCRETE         29. PASCAL              30. MULTINOMIAL
C
C     ==================================================================
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ARG, DELTA, P, PPF, PARAM
      REAL             PARAM1, PARAM2, RATIO
      REAL             FDIV, FDPCON, FEXP
C
      DOUBLE PRECISION DP, DPARAM, DARG
      DOUBLE PRECISION FDDIV, FDCOS, FDLOG, FDSIN
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      L = MOD(L2,30)
        IF (L.EQ.IZERO) L = 30
      PARAM1 = RZERO
C
      GO TO (10,10,10,10,10,10,10,30,30,10,
     1       10,10,10,10,10,10,10,10,30,30,
     2       30,30,10,10,10,10,30,30,30,30), L
C
  10  IF (NRMAX.GT.IZERO) GO TO 20
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
C    CHECK ON NUMBER OF ARGUMENTS.
C
  20  IF (NARGS.LT.ITWO) GO TO 60
C
      GO TO (70,70,70,50,50,50,40,30,30,70,
     1       70,50,50,50,50,70,70,70,50,30,
     2       30,30,40,40,50,50,30,30,30,30), L
C
C     INSTRUCTION DOES NOT EXIST.
C
  30  CALL ERROR ( 1)
      RETURN
C
C     ..................................................................
C
C     TWO PARAMETERS SPECIFIED.
C
  40  PARAM2 = RZERO
      IF (NARGS.NE.IFOUR) GO TO 60
      CALL ADRESS (ITWO,J2)
      IF (J2.LT.IZERO) PARAM1 = ARGS(2)
      CALL ADRESS (ITHRE,J3)
      IF (J3.LT.IZERO) PARAM2 = ARGS(3)
      GO TO 70
C
C     ONE PARAMETER SPECIFIED.
C
  50  IF (NARGS.NE.ITHRE .AND. L2.NE.13) GO TO 60
      IF (NARGS.EQ.ITWO .AND. L2.NE.13) GO TO 70
      CALL ADRESS (ITWO,J2)
      IF (J2.GE.IZERO) GO TO 70
      PARAM  = ARGS(2)
      DPARAM = PARAM
      GO TO 70
C
  60  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     NO PARAMETER SPECIFIED.
C
  70  CALL ADRESS (NARGS,K)
      CALL ADRESS (IONE,J1)
      IF (J1.GE.IZERO) GO TO 80
      P  = ARGS(1)
      DP = P
  80  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DELTA  = RTEN * RER
C
      NRGSM1 = NARGS - IONE
      KNDSUM = IZERO
      DO 90 I=1,NRGSM1
        KNDSUM = KNDSUM + KIND(I)
  90  CONTINUE
C
C     JXTIND = FAULT INDICATOR FOR ERROR 249.
C     IDFIND = FAULT INDICATOR FOR ERROR 207.
C     JDFIND = FAULT INDICATOR FOR ERROR 208.
C
      JXTIND = IZERO
      IDFIND = IZERO
      JDFIND = IZERO
C
      DO 500 I=1,NRMAX
        IF (I.GT.IONE .AND. KNDSUM.EQ.NRGSM1) GO TO 440
        IF (KIND(1).EQ.IZERO) P  = RC(J1)
        IF (KIND(1).EQ.IZERO) DP = P
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) PARAM  = RC(J2)
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.ITHRE) DPARAM = PARAM
        IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM1 = RC(J2)
        IF (KIND(3).EQ.IZERO .AND. NARGS.EQ.IFOUR) PARAM2 = RC(J3)
        IND = IZERO
        IF (P.LT.RZERO .OR. P.GT.RONE) GO TO 300
C
        GO TO (101,102,103,104,105,106,107, 30, 30,110,
     1         111,112,113,114,115,116,117,118, 30, 30,
     2          30, 30,123,124,125,126, 30, 30, 30, 10), L
C
C     ..................................................................
C
C       NORMAL.
C
 101    CALL NORPPF (P,PPF,IND)
        IF (IND.NE.IZERO) GO TO 300
        GO TO 430
C
C       LOG-NORMAL.
C
 102    CALL NORPPF (P,PPF,IND)
        IF (IND.NE.IZERO) GO TO 300
        PPF = FEXP (PPF)
        GO TO 430
C
C       HALF-NORMAL.
C
 103    IF (P.EQ.RZERO) GO TO 301
        ARG = FDIV (RONE+P,RTWO,JIND)
        CALL NORPPF (ARG,PPF,IND)
        IF (PPF.LE.RZERO) PPF = RZERO
        GO TO 430
C
C       STUDENTS T.
C
 104    NU = PARAM + DELTA
        IF (NU.LT.IONE) IDFIND = IDFIND + IONE
        IF (NU.LT.IONE) NU = IONE
        IF (PARAM.NE.AINT(PARAM)) JDFIND = JDFIND + IONE
        CALL   TPPF (P,NU,PPF,IND)
        IF (IND.NE.IZERO) GO TO 300
        GO TO 420
C
C       CHISQUARE.
C
 105    NU = PARAM + DELTA
        IF (NU.LT.IONE) IDFIND = IDFIND + IONE
        IF (NU.LT.IONE) NU = IONE
        IF (PARAM.NE.AINT(PARAM)) JDFIND = JDFIND + IONE
        IF (P.EQ.RZERO) GO TO 301
        CALL CHSPPF (P,NU,PPF,IND)
        IF (IND.EQ.IFOUR) JXTIND = JXTIND + IONE
        GO TO 420
C
C       GAMMA.
C
 106    IF (P.EQ.RZERO) GO TO 301
        CALL GAMPPF (P,PARAM,PPF,IND)
        IF (IND.NE.IZERO) GO TO 310
        GO TO 420
C
C       F.
C
 107    NU1 = PARAM1 + DELTA
        NU2 = PARAM2 + DELTA
        V1  = NU1
        V2  = NU2
        IF (P.EQ.RONE) GO TO 300
        IF (NU1.LE.IZERO .OR. NU2.LE.IZERO) IDFIND = IONE
        IF (NU1.LE.IZERO) NU1 = IONE
        IF (NU2.LE.IZERO) NU2 = IONE
        IF (ABS (PARAM1 - V1).GT.RTWO*DELTA) JDFIND = IONE
        IF (ABS (PARAM2 - V2).GT.RTWO*DELTA) JDFIND = IONE
        CALL FPPF (P, NU1, NU2, PPF, IND)
        GO TO 410
C
C       UNIFORM.
C
 110    IF (P.EQ.RZERO) GO TO 301
        IF (P.EQ.RONE)  GO TO 302
        PPF = P
        GO TO 430
C
C       CAUCHY.
C
 111    IF (P.EQ.RZERO .OR. P.EQ.RONE) GO TO 300
        DARG = DPI * DP
        PPF = - FDPCON ( FDDIV (FDCOS(DARG),FDSIN(DARG),JIND) )
        GO TO 430
C
C       LAMBDA.
C
 112    CALL LAMPPF (P,PARAM,PPF,IND)
        IF (IND.NE.IZERO) GO TO 300
        GO TO 420
C
C       EXTREME.
C
 113    IF (P.EQ.RZERO .OR. P.EQ.RONE) GO TO 300
        IF (NARGS.EQ.ITWO)  PPF = - FDPCON (
     1                        FDLOG ( FDLOG ( FDDIV (DONE,DP,JIND) ) ) )
        IF (NARGS.EQ.ITHRE) PPF =   FDPCON (
     1                       (-FDLOG (DP)) ** FDDIV(-DONE,DPARAM,JIND) )
        IF (NARGS.EQ.ITWO)  GO TO 430
        IF (NARGS.EQ.ITHRE) GO TO 420
C
C       WEIBULL.
C
 114    IF (P.EQ.RZERO) GO TO 301
        IF (P.EQ.RONE)  GO TO 300
        PPF = FDPCON ( (-FDLOG (DONE-DP)) ** FDDIV (DONE,DPARAM,JIND) )
        GO TO 420
C
C       PARETO.
C
 115    IF (PARAM.LE.RZERO) GO TO 320
        IF (P.EQ.RZERO) GO TO 302
        IF (P.EQ.RONE)  GO TO 300
        PPF = FDPCON ( (DONE-DP) ** FDDIV (-DONE,DPARAM,JIND) )
        GO TO 420
C
C       EXPONENTIAL.
C
 116    IF (P.EQ.RZERO) GO TO 301
        IF (P.EQ.RONE)  GO TO 300
        PPF = - FDPCON ( FDLOG (DONE-DP) )
        GO TO 430
C
C       DOUBLE EXPONENTIAL.
C
 117    IF (P.EQ.RZERO .OR. P.EQ.RONE) GO TO 300
        IF (P.LE.RHALF) PPF =   FDPCON ( FDLOG (DTWO*DP) )
        IF (P.GT.RHALF) PPF = - FDPCON ( FDLOG (DTWO*(DONE-DP)) )
        GO TO 430
C
C       LOGISTIC.
C
 118    IF (P.EQ.RZERO .OR. P.EQ.RONE) GO TO 300
        PPF = FDPCON ( FDLOG ( FDDIV (DP,DONE-DP,JIND) ) )
        GO TO 430
C
C       BINOMIAL.
C
 123    N = PARAM1 + DELTA
        CALL BINPPF (P,PARAM2,N,PPF,IND)
        IF (IND.NE.IZERO) GO TO 310
        GO TO 410
C
C       NEGATIVE BINOMIAL.
C
 124    N = PARAM1 + DELTA
        CALL  NBPPF (P,PARAM2,N,PPF,IND)
        IF (IND.NE.IZERO) GO TO 310
        GO TO 410
C
C       POISSON.
C
 125    CALL POIPPF (P,PARAM,PPF,IND)
        IF (IND.NE.IZERO) GO TO 310
        GO TO 420
C
C       GEOMETRIC.
C
 126    IF (P.EQ.RZERO .OR. P.EQ.RONE) GO TO 300
        IF (PARAM.LE.RZERO .OR. PARAM.GE.RONE) GO TO 320
        RATIO = FDPCON ( FDDIV (FDLOG(DONE-DP),FDLOG(DONE-DPARAM),JIND))
        PPF = AINT (RATIO-RHALF)
        GO TO 420
C
C     ..................................................................
C
 300    CALL ERROR (114)
 301    PPF = RZERO
        GO TO (430,420,410), NRGSM1
C
 302    PPF = RONE
        GO TO (430,420,410), NRGSM1
C
 310    IF (IND.EQ.IFOUR) JXTIND = JXTIND + IONE
        IF (IND.NE.IFOUR) CALL ERROR (115)
        GO TO (430,420,410), NRGSM1
C
 320    PPF = RZERO
        CALL ERROR (115)
        GO TO (430,420,410), NRGSM1
C
C     ..................................................................
C
 410    IF (KIND(3).EQ.IONE) GO TO 420
        J3 = J3 + IONE
 420    IF (KIND(2).EQ.IONE) GO TO 430
        J2 = J2 + IONE
 430    IF (KIND(1).EQ.IONE) GO TO 440
        J1 = J1 + IONE
 440    RC(K) = PPF
        K  = K + IONE
 500  CONTINUE
C
C     ..................................................................
C
      IF (JXTIND.NE.IZERO) CALL ERROR (249)
      IF (IDFIND.NE.IZERO) CALL ERROR (207)
      IF (JDFIND.NE.IZERO) CALL ERROR (208)
      RETURN
C
C     ==================================================================
C
      END
*PRBRAN
      SUBROUTINE PRBRAN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRBRAN V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT FOR COMPUTING RANDOM SAMPLES.
C
C     EXAMPLE -
C
C        NORMAL RANDOM SAMPLE PUT IN COLUMN (C)
C           OR
C        NORMAL RANDOM START (N) PUT IN COLUMN (C) $ DISCARD FIRST (N-1)
C
C     ALTHOUGH NOT USED AS IS, MANY DATAPAC PROGRAM UNITS WRITTEN
C        BY JAMES J. FILLIBEN, WERE USED TO CONSTRUCT THIS PROGRAM UNIT.
C
C        APPROPRIATE REFERENCES ARE CONTAINED IN DATAPAC PROGRAM UNITS
C           WITH NAMES HAVING RAN AS THE LAST THREE CHHARACTERS.
C
C
C     ..................................................................
C
C          *** VALUES OF L2 FOR 30 UNIVARIATE DISTRIBUTIONS ***
C                     CONTINUOUS  1-18
C                     NONCENTRAL 19-21
C                       DISCRETE 22-30
C
C      1. NORMAL            2. LOG-NORMAL           3. HALF-NORMAL
C      4. STUDENTS T        5. CHI SQUARED          6. GAMMA
C      7. F                 8. BETA                 9. STUDENTIZED RANGE
C     10. UNIFORM          11. CAUCHY              12. TUKEY LAMBDA
C     13. EXTREME VALUE    14. WEIBULL             15. PARETO
C     16. EXPONENTIAL      17. DOUBLE EXPONENTIAL  18. LOGISTIC
C     19. NONCENTRAL T     20. NONCENTRAL CHISQ    21. NONCENTRAL F
C     22. BERNOULLI        23. BINOMIAL            24. NEGATIVE BINOMIAL
C     25. POISSON          26. GEOMETRIC           27. HYPERGEOMETRIC
C     28. DISCRETE         29. PASCAL              30. MULTINOMIAL
C
C     ==================================================================
C
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ADEN, ANUM(1), ARG, CONST, DELTA
      REAL             PARAM, PARAM1, PARAM2
      REAL             SUM, U, UNIRAN
      REAL             FCOS, FDIV, FEXP, FLOG, FSIN, FSQRT
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 0.001 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      L = MOD(L2,30)
        IF (L.EQ.IZERO) L = 30
C
      NDELET = IZERO
      DELTA  = RTEN * RER
C
      GO TO (20,20,20,30,30,30,40,40,10,20,
     1       20,30,20,30,30,20,20,20,10,10,
     2       10,10,40,40,30,30,10,10,10,10), L
C
C     INSTRUCTION DOES NOT EXIST.
C
  10  CALL ERROR ( 1)
      RETURN
C
C     ..................................................................
C
C     CHECK ON NUMBER OF ARGUMENTS.
C
C     NO PARAMETER SPECIFIED.
C
C     CHECK ON EXTREME DISTRIBUTION.
C
  20  IF (L.EQ.13 .AND. NARGS.EQ.ITWO  .AND. KIND(1).EQ.IONE) GO TO 30
      IF (L.EQ.13 .AND. NARGS.EQ.ITHRE .AND. KIND(2).EQ.IONE) GO TO 30
C
      IF (NARGS.EQ.IONE) GO TO 90
      IF (NARGS.NE.ITWO) GO TO 60
      IF (L.NE.ITEN) GO TO 50
C
C     TREAT UNIFORM DISTRIBUTION AS A SPECIAL CASE.
C
      IF (KIND(1) .EQ.IONE) NDELET = ARGS(1) + DELTA - RONE
      IF (KIND(1).EQ.IZERO) NDELET = IARGS(1) - IONE
      IF (NDELET.LT.IZERO) GO TO 70
      GO TO 90
C
C     ONE PARAMETER SPECIFIED.
C
  30  PARAM = ARGS(NARGS-1)
      IF (KIND(NARGS-1).EQ.IZERO) GO TO 80
      IF (NARGS.EQ.ITWO) GO TO 90
      IF (NARGS.NE.ITHRE) GO TO 60
      GO TO 50
C
C     TWO PARAMETERS SPECIFIED.
C
  40  PARAM1 = ARGS(NARGS-2)
      PARAM2 = ARGS(NARGS-1)
      IF (KIND(NARGS-2).EQ.IZERO) GO TO 80
      IF (KIND(NARGS-1).EQ.IZERO) GO TO 80
      IF (NARGS.EQ.ITHRE) GO TO 90
      IF (NARGS.NE.IFOUR) GO TO 60
      GO TO 50
C
C     RESET NDELET.
C
  50  NDELET = IARGS(1) - IONE
      IF (KIND(1).EQ.IONE) GO TO 70
      IF (NDELET.GE.IZERO) GO TO 90
C
C     ILLEGAL NUMBER OF ARGUMENTS.
C
  60  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     ILLEGAL ARGUMENT.
C
  70  CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
C     IMPROPER TYPE OF ARGUMENT.
C
  80  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  90  CALL ADRESS (NARGS,K)
      IF (NRMAX.GT.IZERO) GO TO 100
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
 100  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     INITIALIZE UNIRAN.
C
      A(1) = UNIRAN (IRAN,KRAN,-IONE)
C
      IEND   = IDIV (NDELET,NS,JIND) + IONE
      NUMBRX = NDELET - (IEND-IONE) * NS
      IF (NDELET.EQ.IZERO) GO TO 140
      IF (L.EQ.ITEN .AND. KIND(1).EQ.IONE) THEN
        RANDOM = ARGS(1) + DELTA
        IF (RANDOM.LT.RONE) RANDOM = 8192. * RANDOM
        IRAN = MOD (IFIX(RANDOM),8192)
        GO TO 140
      END IF
      GO TO (130,130,130,130,130,130,130,130, 10,110,
     1       110,110,110,110,110,110,110,110, 10, 10,
     2        10, 10,130,130,130,130, 10, 10, 10, 10), L
C
C     DISCARD FIRST N UNIFORM RANDOM NUMBERS.
C
 110  DO 120 I=1,NDELET
        U = UNIRAN (IRAN,KRAN,IZERO)
 120  CONTINUE
      GO TO 140
C
C     SETUP FOR DISCARDING RANDOM NUMBERS.
C
 130  NUMBR  = NS
C
 140  GO TO (1010,1020,1030,1040,1050,1060,1070,1080,  30,1100,
     1       1110,1120,1130,1140,1150,1160,1170,1180,  30,  30,
     2         30,  30,1230,1240,1250,1260,  30,1280,  30,  30), L
C
C     ..................................................................
C
C     NORMAL.
C
1010  IF (NDELET.EQ.IZERO) GO TO 1012
      DO 1011 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL NORRAN (IRAN,KRAN,NUMBR,A)
1011  CONTINUE
C
1012  CALL NORRAN (IRAN,KRAN,NRMAX,RC(K))
      RETURN
C
C     ..................................................................
C
C     LOG-NORMAL.
C
1020  IF (NDELET.EQ.IZERO) GO TO 1022
      DO 1021 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL NORRAN (IRAN,KRAN,NUMBR,A)
1021  CONTINUE
C
1022  CALL NORRAN (IRAN,KRAN,NRMAX,A)
      DO 1023 I=1,NRMAX
        RC(K) = FEXP (A(I))
        K     = K + IONE
1023  CONTINUE
C
C     ..................................................................
C
C     HALF-NORMAL.
C
1030  IF (NDELET.EQ.IZERO) GO TO 1032
      DO 1031 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL NORRAN (IRAN,KRAN,NUMBR,A)
1031  CONTINUE
C
1032  CALL NORRAN (IRAN,KRAN,NRMAX,A)
      DO 1033 I=1,NRMAX
        RC(K) = ABS (A(I))
        K     = K + IONE
1033  CONTINUE
      RETURN
C
C     ..................................................................
C
C     STUDENTS T.
C
C        A STUDENTS T VARIATE WITH NU DEGREES OF FREEDOM EQUALS
C           A STANDARD NORMAL VARIATE DIVIDED BY A THE SQUARE ROOT OF
C              A CHISQUARE VARIATE WITH NU DEGREES OF FREEDOM DIVIDED
C                 BY ITS DEGREES OF FREEDOM.
C
1040  NU = PARAM + DELTA
      IF (NU.LT.IONE) CALL ERROR (207)
      IF (NU.LT.IONE) NU = IONE
      IF (PARAM.NE.AINT(PARAM)) CALL ERROR (208)
C
      IF (NDELET.EQ.IZERO) GO TO 1042
      DO 1041 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL NORRAN (IRAN,KRAN,ITWO*NUMBR-IONE,A)
        CALL CHSRAN (IRAN,KRAN,NUMBR,NU,A)
1041  CONTINUE
C
1042  DO 1043 I=1,NRMAX
        CALL NORRAN (IRAN,KRAN,1,ANUM)
        CALL CHSRAN (IRAN,KRAN,1,NU,A)
        ADEN = A(1)
        ADEN  = FSQRT ( FDIV (ADEN,PARAM,JIND) )
        RC(K) = FDIV (ANUM(1),ADEN,JIND)
        K     = K + IONE
1043  CONTINUE
      RETURN
C
C     CHISQUARE.
C
1050  NU = PARAM + DELTA
      IF (NU.LT.IONE) CALL ERROR (207)
      IF (NU.LT.IONE) NU = IONE
      IF (PARAM.NE.AINT(PARAM)) CALL ERROR (208)
C
      IF (NDELET.EQ.IZERO) GO TO 1052
      DO 1051 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL CHSRAN (IRAN,KRAN,NUMBR,NU,A)
1051  CONTINUE
C
1052  CALL CHSRAN (IRAN,KRAN,NRMAX,NU,RC(K))
      RETURN
C
C     ..................................................................
C
C     GAMMA.
C
1060  IF (NDELET.EQ.IZERO) GO TO 1062
      DO 1061 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL GAMRAN (IRAN,KRAN,NUMBR,PARAM,A,IND)
          IF (IND.EQ.IONE) GO TO 2000
1061  CONTINUE
C
1062  CALL GAMRAN (IRAN,KRAN,NRMAX,PARAM,RC(K),IND)
        IF (IND.EQ.IONE) GO TO 2000
      RETURN
C
C     ..................................................................
C
C     F.
C
1070  NU1 = PARAM1 + DELTA
      NU2 = PARAM2 + DELTA
      IF (NU1.LT.IONE) CALL ERROR (207)
      IF (NU1.LT.IONE) NU1 = IONE
      IF (NU2.LT.IONE) CALL ERROR (207)
      IF (NU2.LT.IONE) NU2 = IONE
      IF (PARAM1.NE.AINT(PARAM1)) CALL ERROR (208)
      IF (PARAM2.NE.AINT(PARAM2)) CALL ERROR (208)
C
C        F EQUALS CHISQUARE WITH NU1 DEGREES OF FREEDOM DIVIDED
C           BY NU1 OVER CHISQUARE WITH NU2 DEGREES OF FREEDOM
C              DIVIDED BY NU2.
C
      IF (NDELET.EQ.IZERO) GO TO 1072
      DO 1071 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL CHSRAN (IRAN,KRAN,NUMBR,NU1,A)
        CALL CHSRAN (IRAN,KRAN,NUMBR,NU2,A)
1071  CONTINUE
C
1072  DO 1073 I=1,NRMAX
        CALL CHSRAN (IRAN,KRAN,IONE,NU1,A)
        ANUM(1) = A(1)
        CALL CHSRAN (IRAN,KRAN,IONE,NU2,A)
        ADEN = A(1)
        ANUM(1)  = FDIV (ANUM(1),PARAM1,JIND)
        ADEN  = FDIV (ADEN,PARAM2,JIND)
        RC(K) = FDIV (ANUM(1),ADEN,JIND)
        K     = K + IONE
1073  CONTINUE
      RETURN
C
C     ..................................................................
C
C     BETA.
C
1080  IF (NDELET.EQ.IZERO) GO TO 1082
      DO 1081 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL BETRAN (IRAN,KRAN,NUMBR,PARAM1,PARAM2,A,IND)
          IF (IND.EQ.IONE) GO TO 2000
1081  CONTINUE
C
1082  CALL BETRAN (IRAN,KRAN,NRMAX,PARAM1,PARAM2,RC(K),IND)
        IF (IND.EQ.IONE) GO TO 2000
      RETURN
C
C     ..................................................................
C
C     UNIFORM.
C
1100  DO 1101 I=1,NRMAX
        RC(K) = UNIRAN (IRAN,KRAN,IZERO)
        K     = K + 1
1101  CONTINUE
      RETURN
C
C     ..................................................................
C
C     CAUCHY.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1110  DO 1111 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        ARG   = PI * U
        RC(K) = - FDIV (FCOS(ARG),FSIN(ARG),JIND)
        K     = K + IONE
1111  CONTINUE
      RETURN
C
C     ..................................................................
C
C     LAMBDA.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1120  DO 1122 I=1,NRMAX
        U = UNIRAN (IRAN,KRAN,IZERO)
        IF ((-SPCA).LT.PARAM .AND. PARAM.LT.SPCA)
     1       RC(K) = FLOG ( FDIV (U,RONE-U,JIND) )
        IF ((-SPCA).LT.PARAM .AND. PARAM.LT.SPCA) GO TO 1121
        RC(K) = FDIV (U**PARAM-(RONE-U)**PARAM,PARAM,JIND)
1121    K     = K + IONE
1122  CONTINUE
      RETURN
C
C     ..................................................................
C
C     EXTREME.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1130  IF (NARGS.EQ.ITHRE)  GO TO 1132
      IF (NARGS.EQ.ITWO .AND. KIND(1).EQ.IONE)  GO TO 1132
C
      DO 1131 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = - FLOG ( FLOG ( FDIV (RONE,U,JIND) ) )
        K     = K + IONE
1131  CONTINUE
      RETURN
C
1132  IF (PARAM.LE.RZERO) GO TO 2000
C
      CONST = - FDIV (RONE,PARAM,JIND)
      DO 1133 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = ( - FLOG ( U ) ) ** CONST
        K     = K + IONE
1133  CONTINUE
      RETURN
C
C     ..................................................................
C
C     WEIBULL.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1140  IF (PARAM.LE.RZERO) GO TO 2000
C
      CONST = FDIV (RONE,PARAM,JIND)
      DO 1141 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = ( - FLOG (RONE-U) ) ** CONST
        K     = K + IONE
1141  CONTINUE
      RETURN
C
C     ..................................................................
C
C     PARETO.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1150  IF (PARAM.LE.RZERO) GO TO 2000
C
      CONST = - FDIV (RONE,PARAM,JIND)
      DO 1151 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = (RONE-U) ** CONST
        K     = K + IONE
1151  CONTINUE
      RETURN
C
C     ..................................................................
C
C     EXPONENTIAL.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1160  DO 1161 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = - FLOG ( U )
        K     = K + IONE
1161  CONTINUE
      RETURN
C
C     ..................................................................
C
C     DOUBLE EXPONENTIAL.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1170  DO 1171 I=1,NRMAX
        U = UNIRAN (IRAN,KRAN,IZERO)
        IF (U.LE.RHALF) RC(K) =   FLOG (RTWO*U)
        IF (U.GT.RHALF) RC(K) = - FLOG (RTWO*(RONE-U))
        K = K + IONE
1171  CONTINUE
      RETURN
C
C     ..................................................................
C
C     LOGISTIC.
C
C        USE THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
1180  DO 1181 I=1,NRMAX
        U     = UNIRAN (IRAN,KRAN,IZERO)
        RC(K) = FLOG ( FDIV (U,RONE-U,JIND) )
        K     = K + IONE
1181  CONTINUE
      RETURN
C
C     ..................................................................
C
C     BINOMIAL.
C
1230  NPAR = PARAM1
      IF (NDELET.EQ.IZERO) GO TO 1232
      DO 1231 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL BINRAN (IRAN,KRAN,NUMBR,PARAM2,NPAR,A,IND)
          IF (IND.EQ.IONE) GO TO 2000
1231  CONTINUE
C
1232  CALL BINRAN (IRAN,KRAN,NRMAX,PARAM2,NPAR,RC(K),IND)
        IF (IND.EQ.IONE) GO TO 2000
      RETURN
C
C     ..................................................................
C
C     NEGATIVE BINOMIAL.
C
1240  NPAR = PARAM1 + DELTA
      IF (NDELET.EQ.IZERO) GO TO 1242
      DO 1241 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL NBRAN (IRAN,KRAN,NUMBR,PARAM2,NPAR,A,IND)
          IF (IND.EQ.IONE) GO TO 2000
1241  CONTINUE
C
1242  CALL NBRAN (IRAN,KRAN,NRMAX,PARAM2,NPAR,RC(K),IND)
        IF (IND.EQ.IONE) GO TO 2000
      RETURN
C
C     ..................................................................
C
C     POISSON.
C
1250  IF (PARAM.LE.RZERO) GO TO 2000
C
C        THE DISTRIBUTION OF EXPONENTIAL WAITING TIMES IS POISSON.
C           WAITING TIMES IS POISSON.
C
      IF (NDELET.EQ.IZERO) GO TO 1254
      DO 1253 I=1,NRMAX
        SUM = RZERO
        J   = IONE
1251    U   = UNIRAN (IRAN,KRAN,IZERO)
        IF (U.GE.RONE) GO TO 1252
        E   = - FLOG (RONE-U)
        SUM = SUM + E
        IF (SUM.GT.PARAM) GO TO 1252
        J   = J + IONE
        GO TO 1251
1252    A(1) = J - IONE
1253  CONTINUE
C
1254  DO 1257 I=1,NRMAX
        SUM = RZERO
        J   = IONE
1255    U   = UNIRAN (IRAN,KRAN,IZERO)
        IF (U.GE.RONE) GO TO 1256
        E   = - FLOG (RONE-U)
        SUM = SUM + E
        IF (SUM.GT.PARAM) GO TO 1256
        J   = J + IONE
        GO TO 1255
1256    RC(K) = J - IONE
        K     = K +IONE
1257  CONTINUE
      RETURN
C
C     ..................................................................
C
C     GEOMETRIC.
C
1260  IF (NDELET.EQ.IZERO) GO TO 1262
      DO 1261 I=1,IEND
        IF (I.EQ.IEND) NUMBR = NUMBRX
        CALL GEORAN (IRAN,KRAN,NUMBR,PARAM,A,IND)
          IF (IND.EQ.IONE) GO TO 2000
1261  CONTINUE
C
1262  CALL GEORAN (IRAN,KRAN,NRMAX,PARAM,RC(K),IND)
        IF (IND.EQ.IONE) GO TO 2000
      RETURN
C
C     ..................................................................
C
C     DISCRETE.
C
1280  CONTINUE
      RETURN
C
C     ..................................................................
C
2000  CALL ERROR (38)
      RETURN
C
C     ==================================================================
C
      END
*PRESVE
      SUBROUTINE PRESVE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRESVE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE STRUVE ZERO AND STRUVE ONE INSTRUCTIONS.
C
C     L2 = 35, K = 0
C                      STRUVE ZERO OF (E) PUT IN COLUMN (C)
C     L2 = 36, K = 1
C                      STRUVE ONE  OF (E) PUT IN COLUMN (C)
C
C               WRITTEN BY -
C                      DAVID HOGBEN AND SALLY T. PEAVY,
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
      REAL             S
      REAL             FDPCON
C
      DOUBLE PRECISION W(100)
      DOUBLE PRECISION X, Y, Z
C
      EQUIVALENCE (A(1),W(1))
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.LE.IZERO) CALL ERROR (10)
      IF (NARGS.EQ.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL ADRESS (NARGS,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      X = ARGS(1)
      IF (KIND(1).EQ.IONE) GO TO 20
      CALL ADRESS (1,JA)
  20  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (KIND(1).EQ.IZERO) GO TO 30
      CALL STRUVE (X,Y,Z,W)
      IF (L2.EQ.35) S = FDPCON (Y)
      IF (L2.EQ.36) S = FDPCON (Z)
      CALL VECTOR (S,J)
      RETURN
C
C     ..................................................................
C
  30  DO 40 I=1,NRMAX
        X = RC(JA)
        JA = JA + IONE
        CALL STRUVE (X,Y,Z,W)
        IF (L2.EQ.35) S = FDPCON (Y)
        IF (L2.EQ.36) S = FDPCON (Z)
        RC(J) = S
        J = J + IONE
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*PRINTX   
      SUBROUTINE PRINTX       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PRINTX V 7.00  5/ 7/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     EXECUTE THE FOLLOWING INSTRUCTIONS ...      
C         
C        PRINT // COLUMNS (C), (C), (C), (C), ETC 
C              (PRINT WITH FORMAT //)   
C        PRINT   COLUMNS (C), (C), (C), (C) ETC   
C               USE RPRINT IF ALL ARGS ARE INTEGER, UNLESS IOSWT IS ON.         
C        PRINT  (USING ARGS AS IN RPRINT)         
C               ALWAYS USE RPRINT       
C         
C     L1 = 2,     PRINT       
C     L1 = 8,     NPRINT      
C         
C                  ORIGINAL VERSION -   
C                   CURRENT VERSION - JANUARY, 1990.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION MARG(120)     
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                    ***   TYPE STATEMENTS   ***   
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER*80 LFMTP      
      CHARACTER LA*1
      CHARACTER MARG*1
C
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA NWIDTH / 15 /      
C         
C     ==================================================================        
C         
      IF (NARGS.GT.IZERO) GO TO 10      
      CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
  10  IF (L2.EQ.IONE .AND. IOSWT.GT.IZERO) GO TO 200        
      IF (L2.EQ.IONE) GO TO 20
      CALL PREPAK (ITWO,L2,L1,LFMT,MARG,LFMTP,IND)     
      IF (IND.EQ.IZERO) GO TO 30        
      CALL ERROR (222)        
  20  IF (NPAGE.EQ.IZERO .AND. L1.EQ.8) CALL PAGE (IZERO)   
      CALL RPRINT   
      RETURN        
C         
C     ..................................................................        
C         
  30  IP  = IONE    
  40  CALL CHKCOL   
      IBB = IZERO   
      IF (NERROR.NE.IZERO) RETURN       
      IB  = IZERO   
      IA  = IONE    
      ICP = IZERO   
      IF (IP.EQ.ITWO) GO TO 50
      IB  = NARGS   
      GO TO 80      
C         
  50  IBB = NARGS   
  60  IF (IBB.GT.INUM) GO TO 70         
      IB  = IBB + IB
      IC  = IBB     
      IBB = IZERO   
      GO TO 80      
C         
  70  IBB = IBB - INUM        
      IB  = INUM + IB         
      IC  = INUM    
  80  LL  = NRMAX   
      LEN = LENGTH  
  90  IF (LL.GT.LEN .AND. IP.EQ.ITWO) GO TO 100   
      J   = LL      
      LL  = IZERO   
      GO TO 110     
C         
 100  LL  = LL - LEN
      J   = LEN     
 110  IF (L1.EQ.8 .AND. NPAGE.NE.IZERO) GO TO 130 
      IF (L1.EQ.8) GO TO 220  
      CALL PAGE (IFOUR)       
      IF (IP.EQ.IONE) GO TO 120         
      CALL HEADS (LFMT(IA),IC,ICP,IZERO)
 120  WRITE (IPRINT,240)      
 130  DO 190 M=1,J  
        DO 140 I=IA,IB        
          K = IARGS(I)        
          IARGS(I) = IARGS(I) + IONE    
          ARGS(I) = RC(K)     
 140    CONTINUE    
        IF (IOSWT.EQ.ITWO .AND. L2.EQ.IONE) GO TO 150       
        IF (IP.EQ.ITWO) GO TO 180       
        WRITE (IPRINT,LFMTP) (ARGS(I),I=1,NARGS)  
        GO TO 190   
 150    DO 160 I=1,120        
          MARG(I) = LA(45)    
 160    CONTINUE    
        IBA = IONE  
        DO 170 I=IA,IB        
          CALL RFORMT (9,ISIGD,A,ARGS(I),0,0,NWIDTH,IZERO,MARG(IBA),IRF)        
          IBA = IBA + 15      
 170    CONTINUE    
        IBA = IBA - IONE      
        WRITE (IPRINT,230) (MARG(I),I=1,IBA)      
        IF (MOD(M,ITEN).EQ.IZERO) WRITE (IPRINT,240)        
        GO TO 190   
 180    WRITE (IPRINT,IFMTX) (ARGS(I),I=IA,IB)    
        IF (MOD(M,ITEN).EQ.IZERO) WRITE (IPRINT,240)        
 190  CONTINUE      
C         
      ICP = IONE    
      IF (LL.GT.IZERO) GO TO 90         
      IF (IP.EQ.IONE) RETURN  
      IF (IBB.EQ.IZERO) RETURN
      IF (L1.EQ.8) WRITE (IPRINT,240)   
      IA = IB + IONE
      ICP = IZERO   
      GO TO 60      
C         
C     USE STANDARD OR SPECIFIED FORMAT. 
C         
 200  IP = ITWO     
      DO 210 I=1,NARGS        
        LFMT(I) = IARGS(I)    
 210  CONTINUE      
      GO TO 40      
C         
 220  CALL PAGE (IZERO)       
      GO TO 130     
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 230  FORMAT (120A1)
 240  FORMAT (1X)   
C         
C     ==================================================================        
C         
      END 
*PROROW
      SUBROUTINE PROROW
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PROROW V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS ...
C
C        L2 = 1, ROWSUM     L2 = 2, PRODUCT
C
C               WRITTEN BY -
C                      CARLA MESSINA,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1967.
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
      REAL             X(1)
      REAL             SUM, TEMP
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.GE.ITHRE) GO TO 10
      IF (NARGS.EQ.IONE .AND. KIND(1).EQ.IZERO) GO TO 140
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL CHKCOL
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DO 20 I=1,NRMAX
        A(I) = RZERO
        IF (L2.EQ.IONE) GO TO 20
        A(I) = RONE
  20  CONTINUE
C
      IF (NARGS.GE.IFOUR) GO TO 80
      IF (IARGS(1).LE.IARGS(2)) GO TO 30
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  30  K = IARGS(1)
      DO 50 I=1,NRMAX
        J = K + I - IONE
        IF (L2.EQ.ITWO) GO TO 40
        A(I) = A(I) + RC(J)
        GO TO 50
  40    A(I) = A(I) * RC(J)
  50  CONTINUE
C
      IF (IARGS(1)+NROW.GT.IARGS(2)) GO TO 60
      IARGS(1) = IARGS(1) + NROW
      GO TO 30
  60  K = IARGS(NARGS)
      DO 70 I=1,NRMAX
        J = K + I - IONE
        RC(J) = A(I)
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
  80  II = NARGS - IONE
      IF (L2.EQ.ITWO) GO TO 110
      DO 100 I=1,NRMAX
        JA = NRMAX
        DO 90 L=1,II
          J = IARGS(L) + I - IONE
          JA = JA + IONE
          A(JA) = RC(J)
  90    CONTINUE
C
        CALL SUMMAL (A(NRMAX+1),II,TEMP)
        IF (II.EQ.IONE) TEMP = A(NRMAX+1)
        A(I) = TEMP
 100  CONTINUE
      GO TO 60
C
 110  DO 130 L=1,II
        K = IARGS(L)
        DO 120 I=1,NRMAX
          J = K + I - IONE
          A(I) = A(I) * RC(J)
 120    CONTINUE
 130  CONTINUE
      GO TO 60
C
 140  CALL ADRESS (IONE,J)
      IF (J.GT.IZERO) GO TO 150
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
 150  DO 170 I=1,NRMAX
        IR = I
        CALL SUMMAL (X,IZERO,SUM)
        DO 160 K=1,NCOL
          X(1) = RC(IR)
          CALL SUMMAL (X,-IONE,SUM)
          IR = IR + NROW
 160    CONTINUE
        CALL SUMMAL (X,IONE,SUM)
        RC(J) = SUM
        J = J + IONE
 170  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*PRTABR  
      SUBROUTINE PRTABR (NA,NPAR,NRJ)   
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PRTABR V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PRINT ROW FOR ABRIDGE INSTRUCTION.
C         
C     IF NUMBER IS FLOATED WITH R FORMAT, ASTERISK IS PUT AT END.     
C         
C               WRITTEN BY -  
C                      DAVID HOGBEN,    
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - MARCH, 1978.       
C                   CURRENT VERSION - APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION  ITYPE(100), NBLANK(100), NCOUNT(100), NDECS(100)     
      DIMENSION     NP(120), NSIGDS(100), NWIDTH(100), NWMAX(100)     
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
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
      CHARACTER   LA*1
      CHARACTER   NP*1
C
C     ...................................................................
C
      EQUIVALENCE (NWIDTH(1),A(1001))  
      EQUIVALENCE ( NDECS(1),A(1101)), (NBLANK(1),A(1201))  
      EQUIVALENCE (NCOUNT(1),A(1401)), (NWMAX(1),A(1501))   
      EQUIVALENCE (NSIGDS(1),A(1601)), (ITYPE(1),A(1701))   
C         
C     ==================================================================        
C         
      I1 = IONE     
      I2 = IZERO    
  10  L  = IONE     
      I2 = MIN0 (INUM,NA) + I2
      NA = NA - INUM
      DO 40 I=I1,I2 
        K = IARGS(I) + NRJ    
        IF (ITYPE(I).GT.IONE) GO TO 20  
        CALL RFORMT (0,NSIGDS(I),RC(K),A(1),1,NWMAX(I),NWIDTH(I),     
     1      NDECS(I),NP(1),IRF)         
  20    IF (NPAR.EQ.IONE) NBLANK(I) = IPLACE - NWIDTH(I)    
        CALL RFORMT (ITYPE(I),NSIGDS(I),A,RC(K),NBLANK(I),0,NWIDTH(I),
     1      NDECS(I),NP(L),IRF)         
        IF (IRF.NE.IZERO .AND. IRF.NE.IFIVE .AND. IRF.NE.ITEN .AND.   
     1     IRF.NE.11 .AND. IRF.NE.14) GO TO 50    
        L = L + NWIDTH(I) + NBLANK(I)   
        IF (IRF.LT.11 .OR. IRF.EQ.14) GO TO 40    
        I5 = L - NDECS(I) + NSIGDS(I) - IONE      
        IF (NP(I5).EQ.LA(39)) GO TO 30  
        IF (NP(I5).NE.LA(40)) GO TO 40  
  30    K = L - NWIDTH(I) - IONE        
        NP(K) = LA(41)        
  40  CONTINUE      
C         
      NL = MIN0 (L-IONE,LWIDE)
      WRITE (IPRINT,60) (NP(I),I=2,NL)  
      IF (NA.LE.IZERO) RETURN 
      I1 = I2 + IONE
      GO TO 10      
C         
  50  CALL ERROR (3)
      RETURN        
C         
C     ==================================================================        
C         
C                          ***   FORMAT STATEMENTS   ***    
C         
  60  FORMAT (1X,119A1)       
C
C     ==================================================================        
C         
      END 
*PRTDD
      SUBROUTINE PRTDD (L,NCOMP)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PRTDD  V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT (DIVIDED) DIFFERENCES COMPUTED BY PROGRAM UNIT DIFFER.
C
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
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION LT(9), MA(135), NB(8), ND(8), NW(8)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TPRNTC/ LHEAD(96)
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             B(1)
C
C     ..................................................................
C
      CHARACTER        LHEAD*1
      CHARACTER        MA*1
      CHARACTER        LT*3
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LT(1),LT(2),LT(3),LT(4),LT(5),LT(6),LT(7),LT(8),LT(9)/
     1     ' X ',' Y ','1ST','2ND','3RD','4TH','5TH','6TH','7TH'/
C
      DATA NX / 15 /
C
C     ==================================================================
C
C     DETERMINE THE MAXIMUM NUMBER OF COLUMNS TO BE PRINTED.
C
      MC = IDIV (LWIDE,NX,IND)
      MC = MIN0 (MC,8)
      MC = MIN0 (MC,NRMAX+L-IONE)
      MC = MIN0 (MC,NCOMP+L)
      IF (MC.GT.L) GO TO 10
      CALL ERROR (245)
      RETURN
C
C     ..................................................................
C
  10  MCU = MC + IONE
      MMC = MOD (MC,ITWO)
C
C     SETUP HEADS AND IARGS.
C
      CALL HEADS (IARGS(1),MC,IZERO,IONE)
      CALL CHKCOL
C
C     DETERMINE THE MAXIMUM NUMBER OF COLUMNS PRINTED ON ONE LINE.
C
      MCL = IDIV (MC+IONE,ITWO,IND)
C
C     SETUP
C
      IIIEND = (NX-ITHRE) * MIN0 (MC,NARGS)
      K  = IONE
      MX = NX - ITWO
      DO 20 I=L,MC
        N = NRMAX - I + L
        CALL RFORMT (0,ISIGD,A(K),B(1),N,MX,NW(I),ND(I),MA(1),IRF)
        K = K + N
  20  CONTINUE
C
      DO 30 I=L,MC
        NB(I) = NX - NW(I)
  30  CONTINUE
C
      N2 = ITWO * NRMAX
      IF (L2.EQ.8) GO TO 90
C
C     START AUTOMATIC PRINTING OF DIFFERENCES.
C
      IEND = NRMAX - IONE
      DO 80 I=1,IEND
        I1 = MOD (I,25)
        IF (I1.NE.IONE) GO TO 50
        CALL PAGE (IFOUR)
        WRITE (IPRINT,140) (LHEAD(III),III=1,IIIEND)
        WRITE (IPRINT,150) (LT(II),II=2,MCU)
        WRITE (IPRINT,150)
  50    DO 70 J=1,2
          KBEG = J
          KEND = MC
          IF (J.EQ.IONE .AND. MMC.EQ.IZERO) KEND = KEND - IONE
          IF (J.EQ.ITWO .AND. MMC.EQ.IONE) KEND = KEND - IONE
          IF (I.LT.MCL) KEND = ITWO*(I-IONE) + KBEG
          IF (NRMAX-I.LT.MCL .AND. I.GE.MCL)
     1       KEND = ITWO*(NRMAX-I) + (ITWO-J)
          JC = IZERO
          M1 = IONE
          N = I + (J-IONE)*NRMAX
          DO 60 K=KBEG,KEND,2
            JC = JC + IONE
            CALL RFORMT (1,ISIGD,B,A(N),NB(K), 0,NW(K),ND(K),MA(M1),IRF)
            N = N + N2 -ITWO*K
            M1 = M1 + NX
  60      CONTINUE
        JJEND = NX * JC
        IF (J.EQ.IONE) WRITE (IPRINT,160) (MA(JJ),JJ=2,JJEND)
        IF (J.EQ.ITWO) WRITE (IPRINT,170) (MA(JJ),JJ=1,JJEND)
  70    CONTINUE
  80  CONTINUE
C
      CALL RFORMT (1,ISIGD,B,A(NRMAX),NB(1),0,NW(1),ND(1),MA(1),IRF)
      WRITE (IPRINT,160) (MA(I),I=2,NX)
      RETURN
C
C     ..................................................................
C
C
C     START AUTOMATIC PRINTING OF DIVIDED DIFFERENCES.
C
C     SETUP
C
  90  L0 = IARGS(1)
      CALL RFORMT (0,ISIGD,RC(L0),A(1),NRMAX,MX,NW(1),ND(1),MA(1),IRF)
      NB(1) = NX - NW(1)
C
      IEND = NRMAX-IONE
      DO 130 I=1,IEND
        CALL RFORMT (1,ISIGD,A,RC(L0),NB(1),0,NW(1),ND(1),MA(1),IRF)
        L0 = L0 + IONE
        I2 = MOD (I,50)
        IF (I2.NE.IONE) GO TO 100
        CALL PAGE(IFOUR)
        WRITE (IPRINT,140) (LHEAD(III),III=1,IIIEND)
        WRITE (IPRINT,150) (LT(II),II=1,MC)
        WRITE (IPRINT,150)
 100    DO 120 J=1,2
          KBEG = J
          KEND = MC
          IF (J.EQ.IONE .AND. MMC.EQ.IONE) KEND = KEND - IONE
          IF (J.EQ.ITWO .AND. MMC.EQ.IZERO) KEND = KEND - IONE
          IF (I.LT.MCL) KEND = ITWO*(I-IONE)+KBEG
          IF (NRMAX-I.LT.MCL .AND. I.GE.MCL)
     1          KEND = ITWO*(NRMAX-I) + (ITWO-J)
          JC = ITWO - J
          M1 = IONE + NX * (ITWO-J)
          N = I + (J-IONE)*NRMAX
          DO 110 K=KBEG,KEND,2
            JC = JC + IONE
            M = K + IONE
            CALL RFORMT (1,ISIGD,B,A(N),NB(M), 0,NW(M),ND(M),MA(M1),IRF)
            N = N + N2 - ITWO*K
            M1 = M1 + NX
 110      CONTINUE
          JJEND = NX * JC
          IF (J.EQ.IONE) WRITE (IPRINT,180) (MA(JJ),JJ=2,JJEND)
          IF (J.EQ.ITWO) WRITE (IPRINT,190) (MA(JJ),JJ=1,JJEND)
 120    CONTINUE
 130  CONTINUE
C
      CALL RFORMT (1,ISIGD,A,RC(L0),NB(1),0,NW(1),ND(1),MA(1),IRF)
      CALL RFORMT (1,ISIGD,B,A(NRMAX),NB(2),0,NW(2),ND(2),MA(16),IRF)
      WRITE (IPRINT,180) (MA(I),I=2,30)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 140  FORMAT (8(3X,12A1))
 150  FORMAT (7X,7(A3,12X),A3)
 160  FORMAT (1X,14A1,3(15X,15A1))
 170  FORMAT (4(15X,15A1))
 180  FORMAT (1X,29A1,3(15X,15A1))
 190  FORMAT (30X, 3(15A1,15X))
C
C     ==================================================================
C
      END
*PRTMNL   
      SUBROUTINE PRTMNL (I1,I2,I4,LEN,NPAR,NLINES)       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PRTMNL V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PRINT LINE BY LINE FOR PRINT AND NPRINT INSTRUCTIONS. 
C         
C     IF NUMBER IS FLOATED WITH R FORMAT, ASTERISK IS PUT AT END.     
C         
C     NUMBERS ARE PRINTED IN BLOCKS OF 5 IF NRMAX IS LESS THAN 49,    
C        OTHERWISE THEY ARE PRINTED IN BLOCKS OF TEN.       
C         
C     IRGS(I) NEEDED FOR HEADS BECAUSE CHKCOL IS USED       
C         
C               WRITTEN BY -  
C                      DAVID HOGBEN,    
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - MARCH, 1978.       
C                   CURRENT VERSION - APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION  IRGS(100), ITYPE(100), NBLANK(100), NCOUNT(100)      
      DIMENSION NDECS(100),    NP(120), NSIGDS(100), NWIDTH(100)      
      DIMENSION NWMAX(100)    
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
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
      CHARACTER   LA*1
      CHARACTER   LHEAD*1
      CHARACTER   NP*1
C
C     ..................................................................
C
      EQUIVALENCE (NWIDTH(1),A(1001)), (NDECS(1),A(1101))   
      EQUIVALENCE (NBLANK(1),A(1201)), ( IRGS(1),A(1301))   
      EQUIVALENCE (NCOUNT(1),A(1401)), (NWMAX(1),A(1501))   
      EQUIVALENCE (NSIGDS(1),A(1601)), (ITYPE(1),A(1701))   
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA NSD / 5 /
C         
C     ==================================================================        
C      
      JMAX = IONE   
      DO 90 J=1,NRMAX         
        L = IONE    
        DO 30 I=I1,I2         
          IT = ITYPE(I)       
          K = IARGS(I) + J - IONE       
C         
C         PRINT BLANKS IF NCOUNT(I) IS LESS THAN NRMAX.     
C         
          IF (J.GT.NCOUNT(I)) IT = 11   
C         
C         - MAIN USE OF RFORMT -        
C            CALL RFORMT FOR PRINTING ONE LINE.   
C         
          CALL RFORMT (IT,NSIGDS(I),A,RC(K),NBLANK(I),0,NWIDTH(I),    
     1        NDECS(I),NP(L),IRF)       
          IF (IT.GT.IONE .AND. IRF.NE.7 .AND. IRF.NE.11. AND. IRF.NE.12)        
     1      GO TO 10
          IF (IT.GT.IONE) GO TO 100     
  10      L = L + NWIDTH(I) + NBLANK(I) 
          IF (IRF.LT.11 .OR. IRF.EQ.14) GO TO 30  
C         
C         PUT IN ASTERISK IF R FORMAT IS FORCED INTO E FORMAT.        
C         
          I5 = L - NDECS(I) + NSIGDS(I) - IONE    
          IF (NP(I5).EQ.LA(39)) GO TO 20
          IF (NP(I5).NE.LA(40)) GO TO 30
  20      K = L - NWIDTH(I) - IONE      
          NP(K) = LA(41)      
  30    CONTINUE    
C         
C       I = 2 COMPENSATES FOR 1X.       
C         
        NL = L - IONE         
        WRITE (IPRINT,110) (NP(I),I=2,NL)         
        NLINES = NLINES + IONE
C         
C       PRINT IN BLOCKS OF FIVE.        
C         
        IF (J.EQ.NRMAX) GO TO 40        
        IF (MOD(JMAX,IFIVE).EQ.IZERO .AND. NRMAX.LE.LEN-ITWO)  THEN        
           WRITE (IPRINT,110)       
           NLINES = NLINES + IONE
        ENDIF
        IF (MOD(JMAX,ITEN).EQ.IZERO .AND. NRMAX.GT.LEN-ITWO) THEN
            WRITE (IPRINT,110)        
            NLINES = NLINES + IONE
        END IF
C         
C       CALL NEW PAGE IF NRMAX IS GREATER THAN LENGTH.      
C         
  40    IF (NLINES.LT.LEN) GO TO 80         
        IF (J.EQ.NRMAX) GO TO 80        
        IF (L1.NE.8) CALL PAGE (4)      
        IF (L1.EQ.8) GO TO 80 
        JMAX = IZERO
        NLINES = IZERO
        I5 = 12 * I4
        IF (NPAR.GT.IONE) GO TO 50      
        WRITE (IPRINT,120) (LHEAD(I6),I6=1,I5)    
        NLINES = NLINES + IONE
        GO TO 70    
C         
C       PUT IN COLUMN HEADING IF FIRST ARGUMENT IS NOT A COLUMN NUMBER.         
C         
  50    CALL RFORMT (11,1,RC,RZERO,0,0,NWID,IZERO,NP(1),IRF)
        L = IONE    
        DO 60 I=I1,I2         
          L = L + NWIDTH(I) + NBLANK(I) 
          IF (NWIDTH(I).LT.6 .OR. NCOUNT(I).EQ.IZERO) GO TO 60        
          A(200) = IRGS(I)    
          CALL RFORMT (9,NSD,RC,A(200),0,0,IFIVE,IZERO,NP(L-6),IRF)   
          NP(L-1) = LA(45)    
          IF (NWIDTH(I).LT.12) GO TO 60 
C         
C         INSERT THE WORD COLUMN.       
C         
          NP(L-12) = LA(13)   
          NP(L-11) = LA(25)   
          NP(L-10) = LA(22)   
          NP(L- 9) = LA(31)   
          NP(L- 8) = LA(23)   
          NP(L- 7) = LA(24)   
  60    CONTINUE    
C         
        WRITE (IPRINT,110) (NP(I),I=2,L)
        NLINES = NLINES + IONE
  70    WRITE (IPRINT,110)    
        NLINES = NLINES + IONE
  80    JMAX = JMAX + IONE    
  90  CONTINUE      
      RETURN        
C         
C     ..................................................................        
C         
 100  CALL ERROR (3)
      RETURN        
C         
C     ==================================================================        
C         
C                          ***   FORMAT STATEMENTS   ***    
C         
 110  FORMAT (1X,119A1)       
 120  FORMAT (8(3X,12A1))     
C         
C     ==================================================================        
C         
      END 
*PUNCH    
      SUBROUTINE PUNCH        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.  PUNCH V 7.00  1/ 2/90. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     THE COMMAND PUNCH MAY BE USED IN THE FOLLOWING WAYS   
C        PUNCH    COL (C), (C), (C), (C)  (4 COLUMN LIMIT)  
C        PUNCH // COL (C), (C), (C), (C), ETC  (ACCORDING TO FORMAT //)         
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER IST*1, ISTA*1, LFMTP*80 
C         
C     ==================================================================        
C         
C     L1 = 3  PUNCH 
C         
      IF (NARGS.NE.IZERO) GO TO 20      
      CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
C     IF L2 = 1 ONLY 4 COLUMNS CAN BE PRINTED     
C         
  20  IF (L2.NE.IONE) GO TO 30
      NARGS = MIN0 (NARGS,IFOUR)        
      GO TO 40      
C         
  30  CALL PREPAK (ITWO,L2,L1,LFMT,LFMTP,LFMTP,IND)     
      IF (IND.NE.IZERO) GO TO 80        
C         
  40  CALL CHKCOL   
      IF (NERROR.NE.IZERO) RETURN       
      IST = IFMTX(4)
      ISTA = IFMTX(5)         
      IFMTX(3) = ' '
      IFMTX(4) = '4'
      DO 70 I=1,NRMAX         
        DO 50 J=1,NARGS       
          K = IARGS(J)        
          IARGS(J) = K + IONE 
          ARGS(J) = RC(K)     
  50    CONTINUE    
        IF (L2.NE.IONE) GO TO 60        
        WRITE (IPUNCH,IFMTX) (ARGS(K),K=1,NARGS)  
        GO TO 70    
  60    WRITE (IPUNCH, LFMTP) (ARGS(K),K=1,NARGS) 
  70  CONTINUE      
C         
      IFMTX(4) = IST
      IFMTX(5) = ISTA         
      RETURN        
  80  CALL ERROR (222)        
      L2 = IONE     
      GO TO 40      
C         
C     ==================================================================        
C         
      END 
*RANKS
      SUBROUTINE RANKS
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  RANKS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     FORM OF INSTRUCTION -
C        RANKS OF VALUES IN COLUMN (C) PUT IN COLUMN (C)
C
C     THE ADJUSTMENT FOR TIES, T, IS PUT IN ROW (NRMAX+1),
C        IF NRMAX IS LESS THAN NROW.
C            T=(1/12)*SUM(T-1)*T*(T+1)
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1970.
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
      REAL             TEMP
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.EQ.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL ADRESS (IONE,J1)
      CALL ADRESS (ITWO,J2)
      IF (J1.LT.IZERO .OR. J2.LT.IZERO) CALL ERROR (20)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      NPLUS2 = NRMAX + ITWO
      CALL RANKO (NRMAX,RC(J1),A(2),A(NPLUS2),RC(J2),TEMP)
      IF (NRMAX.GE.NROW) RETURN
      JANR = J2 + NRMAX
      RC(JANR) = TEMP
      RETURN
C
C     ==================================================================
C
      END
*READX    
      SUBROUTINE READX        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  READX V 7.00  5/ 7/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     EXECUTE READ AND READ WITH A FORMAT INSTRUCTIONS.     
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD        
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /REDSET/ IFLAG, ISRFLG, JY, NDROW, NNARG       
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                        ***   TYPE STATEMENTS   ***   
C         
      CHARACTER LA*1
      CHARACTER LFMTP*80      
      CHARACTER*1 IRV(1)
C         
C     ==================================================================        
C         
      IF (L2.NE.IONE) GO TO 60
      ISRFLG = IZERO
      IF (NARGS.GT.IZERO) GO TO 20      
  10  CALL ERROR (10)         
      GO TO 50      
  20  MODE = ITWO   
      CALL CHKCOL   
      IF (NERROR.NE.IZERO) GO TO 50     
      DO 30 I=1,NARGS         
        LFMT(I)  = IARGS(I)   
        IARGS(I) = IZERO      
        ARGS(I)  = RZERO      
  30  CONTINUE      
C         
      IFLAG = IZERO 
      JY    = IZERO 
      NNARG = NARGS 
      NROLD = NRMAX 
      RETURN        
C         
C     ..................................................................        
C         
  40  MODE  = ITWO  
  50  IFLAG = IONE  
      RETURN        
C         
C     ..................................................................        
C         
C              FORMATTED READ 
C              READ X N C C C C         
C         
C              N = NUMBER OF CARDS TO READ. IF N = 0, READ UNTIL A    
C                 BLANK CARD IS FOUND   
C              X IS THE FORMAT IDENTIFIER, A,B,C,D,E,F      
C         
  60  IF (NARGS.LE.IONE) GO TO 10       
C         
C              SETUP FORMAT.  
C         
      CALL PREPAK (ITWO,L2,I,LFMT,IRV,LFMTP,IND)      
      IF (IND.NE.IZERO) CALL ERROR (27) 
      IF (NERROR.NE.IZERO) GO TO 40     
C         
C              CHECK AND CONVERT ARGUMENTS.       
C         
      DO 70 J=2,NARGS         
        I = J       
        CALL ADRESS (I,IARGS(I))        
        IF (IARGS(I).LT.IZERO) GO TO 150
  70  CONTINUE      
      IF (IARGS(1).LT.IZERO) CALL ERROR (20)      
      IF (NERROR.NE.IZERO) RETURN       
      IF (IARGS(1).GT.IZERO) GO TO 80   
      N = NRC       
      GO TO 90      
  80  N = IARGS(1)  
  90  DO 130 I=1,N  
        READ (INUNIT,LFMTP) (ARGS(L),L = 2,NARGS) 
C         
C              CHECK IF LOOKING FOR BLANK CARD.   
C         
        IF (IARGS(1).NE.IZERO) GO TO 110
        DO 100 L=2,NARGS      
          IF (ARGS(L).NE.RZERO) GO TO 110    
 100    CONTINUE    
C         
C              BLANK CARD FOUND, TERMINATE READ.  
C         
        GO TO 140   
C         
C              IF THERE IS TOO MUCH DATA, DO NOT ENTER EXCESS.        
C         

 110    IF (I.GT.NROW) GO TO 130        
        DO 120 L=2,NARGS      
          K = IARGS(L)        
          IARGS(L) = K + IONE 
          RC(K) = ARGS(L)     
 120    CONTINUE    
 130  CONTINUE      
C         
      I = N + IONE  
 140  I = I - IONE  
      NROLD = NRMAX 
      NRMAX = MIN0 (I,NROW)   
      CALL ERROR (252)        
      WRITE (ISCRT,160) I     
      IF (I.GT.NROW) CALL ERROR (201)   
      RETURN        
C         
C     ..................................................................        
C         
 150  CALL ERROR (20)         
      MODE = IONE   
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 160  FORMAT (5X,I4,33H DATA CARD(S) READ BUT NOT LISTED, 28X)        
C         
C     ==================================================================        
C         
      END 
*RDWOUT
      SUBROUTINE RDWOUT (IRAN,KRAN,NPOPLN,NSAMPL,NSTART,U,RNDGIT,NFAULT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RDWOUT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO PRODUCE NSAMPL PSEUDO RANDOM INTEGERS FROM
C        POPULATION OF SIZE NPOPLN.  THE RESULT IS STORED IN VECTOR N.
C           U IS STORAGE VECTOR FOR PSEUDO UNIFORM RANDOM NUMBERS.
C
C     SAMPLING IS *** WITHOUT *** REPLACEMENT.
C
C     NSTART = STARTING VALUE FOR PSEUDO RANDOM NUMBER GENERATOR.
C
C     U MUST BE DIMENSIONED NPOPLN+NSAMPLN OR GREATER IN CALLING
C        PROGRAM UNIT.
C
C     NFAULT = 0, IF EVERYTHING IS OK
C              1, IF NPOPLN IS LESS THAN 1
C              2, IF NSAMPL IS LESS THAN 1
C              3, IF NSAMPL IS GREATER THAN NPOPLN.
C              4, IF NSAMPL PLUS NPOPLN IS GREATER THAN NS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1991.
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
      REAL             U(*), RNDGIT(*)
      REAL             X, Y
      REAL             UNIRAN
C
C     ==================================================================
C
      NFAULT = IZERO
      IF (NPOPLN.LT.IONE)      NFAULT = IONE
      IF (NSAMPL.LT.IONE)      NFAULT = ITWO
      IF (NSAMPL.GT.NPOPLN)    NFAULT = ITHRE
      IF (NPOPLN+NSAMPL.GT.NS) NFAULT = IFOUR
      IF (NFAULT.NE.IZERO)     RETURN
C
      X = RONE
      Y = FLOAT (NPOPLN)
      IF (NSTART.LE.IONE) GO TO 20
C
      IEND = NSTART - IONE
      DO 10 I=1,IEND
        U(1) = UNIRAN (IRAN,KRAN,IZERO)
  10  CONTINUE
C
  20  KSUB = NPOPLN + IONE
      ISUB = KSUB
      DO 30 I=1,NPOPLN
        U(I) = I
  30  CONTINUE
C
      DO 40 I=1,NSAMPL
        U(ISUB) = UNIRAN (IRAN,KRAN,IZERO)
        ISUB    = ISUB + IONE
  40  CONTINUE
C
      ISUB = KSUB
      DO 50 I=1,NSAMPL
        K    = X + Y * U(ISUB)
        IF (K.GT.NPOPLN) K = NPOPLN
        M    = U(I)
        U(I) = U(K)
        U(K) = M
        X    = X + RONE
        Y    = Y - RONE
        ISUB = ISUB + IONE
  50  CONTINUE
C
      DO 60 I=1,NSAMPL
        RNDGIT(I) = U(I)
  60  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*RECODE
      SUBROUTINE RECODE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RECODE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR CODING DATA.
C
C     INSTRUCTIONS - WITH VALUES OF L2 ...
C
C       (10) REPLACE THE VALUE (K) IN COL (C) BY (K) IN COL (C)
C            REPLACE VALUES FROM (K) TO (K) IN COL (C) BY (K) IN COL (C)
C       (11) RECODE COLUMN (C) INTO COLUMN (C)
C       (12) CODE COLUMN (C) USING LENGTH (K) AND PUT IN COLUMN (C)
C            CODE COL (C) START AT (K) USE LENGTH (K) PUT IN COLUMN (C)
C
C     NO PRINTING.
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
      REAL             RCMIN, X, Y
      REAL             FDIV
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      M = NRMAX+IONE
      IF (L2.GT.ITEN) GO TO 10
      IF (NARGS.EQ.IFOUR .OR. NARGS.EQ.IFIVE) GO TO 10
        CALL ERROR (10)
        RETURN
C
C     ..................................................................
C
 10   IF (L2.EQ.ITEN) GO TO 40
      IF (L2.EQ.12) GO TO 20
      IF (NARGS.NE.ITWO) CALL ERROR (10)
      IF (M+NRMAX.LT.NS) GO TO 40
        CALL ERROR (23)
        RETURN
C
C     ..................................................................
C
 20   IF (NARGS.NE.ITHRE .AND. NARGS.NE.IFOUR) CALL ERROR (10)
      IF (KIND(NARGS-1).EQ.IZERO) ARGS(NARGS-1) = FLOAT(IARGS(NARGS-1))
      Y = ARGS(NARGS-1)
      IF (Y) 40,30,40
 30     CALL ERROR (3)
        RETURN
 40   CALL ADRESS (NARGS,J2)
      IF (J2.LT.IZERO) CALL ERROR (20)
      K = NARGS-ITWO
        IF (L2.NE.ITEN) K = IONE
      CALL ADRESS (K,J1)
      IF (J1.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     START COMPUTING.
C
      L = L2 - 9
      GO TO (50,120,130), L
C
C     REPLACE.
C
  50  K = NARGS - IONE
      IF (KIND(1).EQ.IZERO) ARGS(1) = FLOAT(IARGS(1))
      IF (KIND(2).EQ.IZERO .AND. NARGS.EQ.IFIVE)
     1     ARGS(2) = FLOAT ( IARGS(2) )
      DO 110 I=1,NRMAX
        IF (RC(J1)-ARGS(1)) 90,60,70
  60    IF (NARGS.EQ.IFOUR) GO TO 80
  70    IF (NARGS.EQ.IFOUR) GO TO 90
        IF (RC(J1)-ARGS(2)) 80,80,90
  80    RC(J2) = ARGS(K)
        GO TO 100
  90    RC(J2) = RC(J1)
 100    J1 = J1 + IONE
        J2 = J2 + IONE
 110  CONTINUE
      RETURN
C
C     ..................................................................
C
C     RECODE.
C
 120  CALL UNIQUE (RC(J1),NRMAX,  A(1), A(M),N)
      CALL SCODE  (RC(J1), A(M),RC(J2),NRMAX,N)
      RETURN
C
C     CODE
C
 130  IF (NARGS.EQ.IFOUR) GO TO 170
C
C     FIND MINIMUM = RCMIN
C
      RCMIN = RC(J1)
      K = J1
      DO 160 I=1,NRMAX
        IF (RC(K)-RCMIN) 140,140,150
 140    RCMIN = RC(K)
 150    K = K + IONE
 160  CONTINUE
      GO TO 180
C
 170  IF (KIND(2).EQ.IZERO) ARGS(2) = FLOAT(IARGS(2))
      RCMIN = ARGS(2)
C
 180  DO 200 I=1,NRMAX
        X = FDIV (RC(J1)-RCMIN,Y,IND)
        N = X + RONE
        IF (N.GT.IZERO) GO TO 190
        N = IZERO
 190    RC(J2) = FLOAT (N)
        J1 = J1 + IONE
        J2 = J2 + IONE
 200  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*REDATA
      SUBROUTINE REDATA
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REDATA V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     READ DATA FOLLOWING SET OR READ INSTRUCTION.
C       IF ISRFLG = 0 DATA IS READ FOR READ INSTRUCTION.
C       IF ISRFLG NOT EQUAL 0, DATA IS READ FOR SET INSTRUCTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /REDSET/ IFLAG, ISRFLG, JY, NDROW, NNARG
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
      IF (ISRFLG.NE.IZERO) GO TO 60
C
C     STORE THE DATA WHICH FOLLOWS A READ INSTRUCTION IN THE WORKSHEET.
C
      IF (IFLAG.NE.IZERO) RETURN
      IF (JY.LT.NROW) GO TO 10
      IFLAG = IONE
      CALL ERROR (201)
C
C     NNARG CONTAINS NARGS OF READ INSTRUCTION.
C     LFMT(1) THRU LFMT(NNAGR) CONTAINS ADDRESSES OF TOPS OF COLUMNS.
C
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.GE.NNARG) GO TO 30
      NNS = NARGS + IONE
      DO 20 I=NNS,NNARG
        KIND(I) = IZERO
        IARGS(I) = IZERO
  20  CONTINUE
C
  30  DO 50 I=1,NNARG
        K = LFMT(I) + JY
        IF (KIND(I).EQ.IZERO) GO TO 40
        RC(K) = ARGS(I)
        GO TO 50
  40    RC(K) = IARGS(I)
  50  CONTINUE
C
C     JY IS CARD COUNT. IT COUNTS FROM ZERO.
C
      JY = JY + IONE
      NRMAX = JY
      RETURN
C
C     ..................................................................
C
C     CHECK IF END OF ROW HAS BEEN EXCEEDED PREVIOUSLY IN THIS SET.
C
C     JY IS WHERE NEXT DATA ITEM IS TO GO IN COLUMN
C     JJ IS WHERE LAST DATA ITEM OF THIS SET IS TO GO
C     NDROW IS ADDRESS OF LAST ELEMENT OF ROW.
C
   60 IF (IFLAG.NE.IZERO .OR. NARGS.EQ.IZERO) RETURN
      JJ = JY + NARGS - IONE
      IF (JJ.LE.NDROW) GO TO 70
      CALL ERROR (201)
      IFLAG = IONE
      IF (JY.GT.NDROW) RETURN
      JJ = NDROW
  70  K = IONE
      DO 100 I=JY,JJ
        IF (KIND(K).EQ.IZERO) GO TO 80
        RC(I) = ARGS(K)
        GO TO 90
  80    RC(I) = IARGS(K)
  90    K = K + IONE
 100  CONTINUE
C
      JY = JJ + IONE
      NRMAX = JJ - NDROW + NROW
      RETURN
C
C     ==================================================================
C
      END
*REGLOF
      SUBROUTINE REGLOF (M,IYSUB,ISASUB,IFSSUB,NLINES,MXLINE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REGLOF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO COMPUTE AND PRINT LACK OF FIT ANOVA.
C
C     STRUCTURE CHART ...
C
C                      ..........
C                      . REGLOF .
C                      ..........
C                          .
C                          .
C           ..............................................
C           .                    .                       .
C           .                    .                       .
C     ..........              .......               ..........
C     . LOFIND .            . WEIGHTS .             . OUTLOF .
C     ..........              .......               ..........
C          .                     .
C          .                     .
C     ..........          ...............
C     . MCHROW .          .             .
C     ..........          .             .
C                     ........       .......
C                     .  NO  .       . YES .
C                     ........       .......
C                         .             .
C                         .             .
C                    ..........     ..........
C                    . PERRSS .     . WERRSS .
C                    ..........     ..........
C
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
      REAL             SS(5)
C
C     ==================================================================
C
C     INITIALIZATION.
C
      IWT = IZERO
      IF (KIND(2).EQ.IZERO) IWT = IONE
      IF (IWT.EQ.IONE) CALL ADRESS (ITWO,IWTSUB)
C
      NOFVEC = M
      IF (L2.EQ.IONE) NOFVEC = IONE
C
C     MOVE X MATRIX TO SCRATCH AREA.
C
      ISUB2 = ISASUB + NRMAX
C
      L = ISUB2
      DO 20 J=1,NOFVEC
        CALL ADRESS (J+ITHRE,K)
        N = IWTSUB
        DO 10 I=1,NRMAX
          A(L) = RC(K)
          K = K + IONE
          L = L + IONE
  10    CONTINUE
  20  CONTINUE
C
C     COMPUTE INDEX.
C
      CALL LOFIND (A(ISUB2),NRMAX,NOFVEC,A(ISASUB))
C
C     ADJUST INDEX FOR NONPOSITIVE WEIGHTS.
C
      NOZWTS = IZERO
      IF (IWT.EQ.IZERO) GO TO 50
      K = ISASUB
      L = IWTSUB
      DO 40 I=1,NRMAX
        IF (RC(L).GT.RZERO) GO TO 30
        A(K) = -RONE
        NOZWTS = NOZWTS + IONE
  30    K = K + IONE
        L = L + IONE
  40  CONTINUE
C
C     SORT INDEX.
C
  50  CALL SORT (A(ISASUB),A(ISUB2),NRMAX,IZERO)
C
      NOFNZW = NRMAX - NOZWTS
      K = ISUB2 - IONE
      IMAX = A(K)
      NDFERR = IZERO
      IF (IMAX.EQ.NOFNZW) GO TO 130
C
C     COMPUTE ERROR D.F. AND SUM OF SQUARES.
C
      J  = NOZWTS + IONE
      K  = ISASUB + NOZWTS + IONE
      IY = ISUB2 - IONE
      IPTSUM = ISUB2
      IBEG = NOZWTS + ITWO
      DO 120 I=IBEG,NRMAX
        IF (A(K)-A(K-1)) 70,70,80
  70    IF (I.LT.NRMAX) GO TO 110
        N = I - J + IONE
        GO TO 90
  80    N = I - J
  90    IF (N.EQ.IONE) GO TO 100
        NDFERR = NDFERR + N - IONE
        ISUBH = IY + J
        IF (IWT.EQ.IZERO) CALL PERRSS (       IYSUB,ISUBH,N,A(IPTSUM))
        IF (IWT.EQ. IONE) CALL WERRSS (IWTSUB,IYSUB,ISUBH,N,A(IPTSUM))
        IPTSUM = IPTSUM + IONE
 100    J = I
 110    K = K + IONE
 120  CONTINUE
      IF (NDFERR.GE.NOFNZW-M) GO TO 130
      CALL SUMMAL (A(ISUB2),IPTSUM-ISUB2,SS(4))
      IF (IPTSUM-ISUB2.EQ.IONE) SS(4) = A(ISUB2)
      IF (IWT.EQ.IZERO .AND. ARGS(2).NE.RONE) SS(4) = ARGS(2) * SS(4)
C
C     PRINT ANOVA.
C
      CALL SUMMAL (A(IFSSUB),M,SS(1))
      IF (M.EQ.IONE) SS(1) = A(IFSSUB)
      K = IFSSUB + M
      SS(2) = A(K)
      SS(5) = A(K+1)
      SS(3) = SS(2) - SS(4)
C
 130  MLINES = NLINES
      NXLINE = MXLINE
      CALL OUTLOF (SS,M,NDFERR,NOFNZW,MLINES,NXLINE)
C
      NLINES = MLINES
      RETURN
C
C     ==================================================================
C
      END
*REL1FT
      SUBROUTINE REL1FT (N,NRANK,ICOEF,IRES)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REL1FT V 7.00  5/ 9/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C    PROGRAM UNIT IS USED TO UTILIZE SUGGESTION OF
C        C. J. WITZGALL AND K. L. HOFFMAN.
C
C     DELETE ALL ROWS OF Y AND X MATRICS FOR WHICH RESIDUALS
C        ARE NONZERO.
C
C     SOLVE RESULTING SYSTEM OF EQUATIONS TO OBTAIN ESTIMATES
C        OF UNKNOWN PARAMETERS.
C
C     RECALCULATE RESIDUALS AND SUM OF ABSOLUTE VALUE OF RESIDUALS.
C
C     STORAGE REQUIREMENTS LIMIT N TO 66, WHEN NS = 135000.
C        3 * N**2 + 5*N MUST BE LESS THAN OR EQUAL TO NS.
C
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY, 
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1978.
C                   CURRENT VERSION -      MAY, 1991.
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
      REAL             ERR(4), TEMP(1)
      REAL             SUM
C
C     ==================================================================
C
C     MOVE X AND Y TO SCRATCH AREA.
C
      L = IFOUR
      IASUB = IONE
      DO 30 I=1,N
        IXSUB = IARGS(L)
        IRSUB = IRES
        DO 20 J=1,NRMAX
          IF (RC(IRSUB).NE.RZERO) GO TO 10
          A(IASUB) = RC(IXSUB)
          IASUB = IASUB + IONE
  10      IXSUB = IXSUB + IONE
          IRSUB = IRSUB + IONE
  20    CONTINUE
        L = L + IONE
  30  CONTINUE
C
      ISUBY = IASUB
      IRSUB = IRES
      K     = IARGS(1)
      DO 50 I=1,NRMAX
        IF (RC(IRSUB).NE.RZERO) GO TO 40
        A(IASUB) = RC(K)
        IASUB = IASUB + IONE
  40    K     = K + IONE
        IRSUB = IRSUB + IONE
  50  CONTINUE
C
C     SOLVE SYSTEM OF EQUATIONS.
C
      ISUBXI = IONE + NRANK * NRANK + NRANK
      ISUBUL = ISUBXI + NRANK * NRANK
      ISUBB  = ISUBUL + NRANK * NRANK
      ISUBR  = ISUBB  + NRANK
      ISUBDX = ISUBR  + NRANK
      ISUBPS = ISUBDX + NRANK
      CALL INVCHK (A,NRANK,A(ISUBXI),A(ISUBUL),NRANK,A(ISUBB),A(ISUBR),
     1            A(ISUBDX),A(ISUBY),A(ISUBPS),ITWO,ERR,IND)
      IF (IND.NE.IZERO) CALL ERROR (249)
C
C     STORE COEFFICIENTS IN WORKSHEET.
C
      K = ICOEF
      L = ISUBXI
      DO 60 I=1,N
        RC(K) = A(L)
        K = K + IONE
        L = L + IONE
  60  CONTINUE
C
C     COMPUTE RESIDUALS AND STORE IN WORKSHEET.
C
      IY = IARGS(1)
      IR = IRES
      DO 90 I=1,NRMAX
        IF (RC(IR).EQ.RZERO) GO TO 80
        L = ICOEF
        CALL SUMMAL (TEMP,IZERO,SUM)
        M = IARGS(IFOUR) + I - IONE
        DO 70 J=1,N
          K = ITHRE + J
          M = IARGS(K) + I - IONE
          TEMP(1) = RC(L) * RC(M)
          CALL SUMMAL (TEMP,-IONE,SUM)
          L = L + IONE
  70    CONTINUE
        CALL SUMMAL (TEMP,IONE,SUM)
        IF (N.EQ.IONE) SUM = RC(ICOEF) * RC(M)
        RC(IR) = RC(IY) - SUM
        K  = K + IONE
  80    IR = IR + IONE
        IY = IY + IONE
  90  CONTINUE
C
C     COMPUTE SUM OF ABSOLUTE RESIDUALS AND STORE IN WORKSHEET.
C
      L = ICOEF + N
      IF (N+IONE.LE.NROW) GO TO 100
        CALL ERROR (226)
        RETURN
C
C     ..................................................................
C
 100  K = IRES
      CALL SUMMAL (TEMP,IZERO,SUM)
      DO 110 I=1,NRMAX
        TEMP(1) = ABS ( RC(K) )
        CALL SUMMAL (TEMP,-IONE,SUM)
        K = K + IONE
 110  CONTINUE
      CALL SUMMAL (TEMP,IONE,SUM)
      RC(L) = SUM
      RETURN
C
C     ==================================================================
C
      END
*RNDSMP
      SUBROUTINE RNDSMP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RNDSMP V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE SAMPLE WITHR AND SAMPLE WITHOUTR INSTRUCTIONS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
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
      REAL             UNIRAN
C
C     ==================================================================
C
      ISTART = IZERO
      IF (NARGS.LT.ITHRE .OR. NARGS.GT.IFOUR) GO TO 20
      IF (KIND(NARGS-2).NE.IZERO) IARGS(NARGS-2) = ARGS(NARGS-2)
      IF (KIND(NARGS-1).NE.IZERO) IARGS(NARGS-1) = ARGS(NARGS-1)
      IF (NARGS.EQ.ITHRE) GO TO 10
      IF (KIND(1).NE.IZERO) IARGS(1) = ARGS(1)
      ISTART = IARGS(1)
      IF (ISTART.LT.IZERO) GO TO 70
  10  CALL ADRESS (NARGS,K)
      IF (NERROR.NE.IZERO) RETURN
      GO TO 30
C
  20  CALL ERROR (10)
      RETURN
C
C
C     ==================================================================
C
  30  NSAMPL = IARGS(NARGS-2)
      NPOPLN = IARGS(NARGS-1)
      IF (NSAMPL.LE.NROW) GO TO 40
        NSAMPL = NROW
        CALL ERROR (226)
C
  40  A(1) = UNIRAN (IRAN,KRAN,-IONE)
      IF (L2.EQ.10) GO TO 50
      CALL RDWITH (IRAN,KRAN,NPOPLN,NSAMPL,ISTART,A,RC(K),IND)
      GO TO 60
  50  CALL RDWOUT (IRAN,KRAN,NPOPLN,NSAMPL,ISTART,A,RC(K),IND)
C
C     ..................................................................
C
      IF (IND.NE.IFOUR) GO TO 60
      CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
  60  IF (IND.NE.IZERO) GO TO 70
      NROLD = NRMAX
      NRMAX = NSAMPL
      CALL ERROR (252)
      RETURN
C
C     ..................................................................
C
  70  CALL ERROR (3)
      RETURN
C
C     ==================================================================
C
      END
*RPRINT   
      SUBROUTINE RPRINT       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. RPRINT V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PROCEDURE FOR PRINTING INSTRUCTIONS.        
C         
C     DIFFERENT FORMS OF PRINTING INSTRUCTIONS -  
C         
C     PRINT COLUMNS (C), (C), ..., (C)  
C        PRINT COLUMNS (C) ... (C) WITH (K) SIGNIFICANT DIGITS        
C        PRINT COLS (C)...(C) WITH (K) S DIGITS, (C)...(C) WITH (K), ETC        
C        PRINT (K) COLS (C) WITH (S) S.D., (C) WITH (S)  S.D., ETC.   
C        PRINT (K) COLS (C) (S) S.D. (M) MAX WIDTH, (C) (S) (M), ...  
C        PRINT (K) COLS (C), (S), (M), (B) BLANKS, (C) (S) (M) (B) ...
C         
C     NPRINT COLUMNS (C), (C), ..., (C) 
C        NPRINT COLUMNS (C) ... (C) WITH (K) SIGNIFICANT DIGITS       
C        NPRINT COLS (C)...(C) WITH (K) S DIGITS, (C)...(C) WITH (K) ...        
C        NPRINT (K) COLS (C) WITH (S) S.D., (C) WITH (S)  S.D., ETC.  
C        NPRINT (K) COLS (C) (S) S.D. (M) MAX WIDTH, (C) (S) (M), ... 
C        NPRINT (K) COLS (C), (S), (M), (B) BLANKS, (C) (S) (M) (B) ...         
C         
C     ABRIDGE ROW (R) OF COLUMNS (C), (C), ..., (C)         
C        ABRIDGE ROW (R) OF COLS (C) ... (C) WITH (K) SIGNIFICANT DIGITS        
C        ABRIDGE (R) OF (C)...(C) WITH (K) S.D., (C)...(C) WITH (K), ETC        
C        ABRIDGE (R) (K) COLS (C) WITH (S) S.D., (C) WITH (S)  S.D., ETC        
C        ABRIDGE (R) (K) COLS (C) (S) SD (M) MAX WIDTH, (C) (S) (M), ...        
C        ABRIDGE (R) (K) COLS (C),(S),(M),(B) BLANKS (C) (S) (M) (B) ...        
C         
C     ..................................................................        
C         
C     IN LAST TWO OPTIONS OF PRINT, NPRINT, AND ABRIDGE     
C        IPEW.D FORMAT IS OBTAINED BY SETTING S = D AND M = 0         
C          FW.D FORMAT IS OBTAINED BY SETTING S = D AND M = -W        
C          IW   FORMAT IS OBTAINED BY SETTING S = 0 AND M - -(W+1)    
C         
C     ..................................................................        
C         
C     REPLACES TRAILING ZEROS BY BLANKS IF COUNT IS LESS THAN NRMAX   
C         
C      IF FIRST ARGUMENT IS NOT A COLUMN NUMBER - 
C        (1)   HEADING IS GIVEN IF WIDTH IS GREATER THAN OR EQUAL TO 12         
C        (2)   COLUMN NUMBER IF WIDTH IS GREATER THAN 5 AND LESS THAN 12        
C        (3)   NONE, IF WIDTH IS LESS THAN 6.     
C         
C     IF NUMBER IS FLOATED WITH R FORMAT, ASTERISK IS PUT AT END.     
C         
C     NUMBERS ARE PRINTED IN BLOCKS OF 5 IF NRMAX IS LESS THAN 49,    
C        OTHERWISE THEY ARE PRINTED IN BLOCKS OF TEN.       
C         
C     IRGS(I) NEEDED FOR HEADS BECAUSE CHKCOL IS USED       
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
      DIMENSION  IRGS(100), ITYPE(100), NBLANK(100), NCOUNT(100)      
      DIMENSION NDECS(100),    NP(120), NSIGDS(100), NWIDTH(100)      
      DIMENSION NWMAX(100)    
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
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
      REAL             SPCA   
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        NP*1
C
C     ...................................................................
C         
      EQUIVALENCE (NWIDTH(1),A(1001)), (NDECS(1),A(1101))   
      EQUIVALENCE (NBLANK(1),A(1201)), ( IRGS(1),A(1301))   
      EQUIVALENCE (NCOUNT(1),A(1401)), (NWMAX(1),A(1501))   
      EQUIVALENCE (NSIGDS(1),A(1601)), (ITYPE(1),A(1701))   
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA NSD, NWID, NZERO / 5, 119, 0 /         
C         
      DATA SPCA / 0.000001 /  
C         
C     ==================================================================        
C         
      LEN    = LENGTH  
      NLINES = IZERO
C         
C     ERROR CHECKING.         
C         
      IF (L1.NE.6) GO TO 20   
      NARGS = NARGS - IONE    
      IF (KIND(1).EQ.IONE) GO TO 50     
      IF (IARGS(1).LE.IZERO .OR. IARGS(1).GT.NROW) GO TO 60 
      NRJ = IARGS(1) - IONE   
      IF (NARGS.LT.IONE) GO TO 30       
      DO 10 I=1,NARGS         
        KIND(I)  = KIND(I+1)  
        IARGS(I) = IARGS(I+1) 
        ARGS(I)  = ARGS(I+1)  
  10  CONTINUE      
  20  IF (NARGS.GT.IZERO) GO TO 40      
  30  CALL ERROR (10)         
      RETURN        
C         
C     ..................................................................        
C         
  40  IF (NRMAX.GT.IZERO) GO TO 70      
      CALL ERROR (9)
      RETURN        
C         
C     ..................................................................        
C         
  50  CALL ERROR (3)
      RETURN        
C         
C     ..................................................................        
C         
  60  CALL ERROR (16)         
      RETURN        
C         
C     ==================================================================        
C         
C     START PRINTING.         
C         
  70  IF (NERROR.NE.IZERO) RETURN       
      NPAR = IONE   
      IF (KIND(1).EQ.IONE .OR. KIND(NARGS).EQ.IONE) GO TO 90
C         
C     ALL ARGUMENTS ARE INTEGERS.       
C         
      DO 80 I=1,NARGS         
        ITYPE(I)  = IONE      
        NSIGDS(I) = ISIGD     
        NWMAX(I)  = IPLACE - ITWO       
        IRGS(I)   = IARGS(I)  
  80  CONTINUE      
      CALL CHKCOL   
      NA = NARGS    
      GO TO 200     
C         
C     LAST ARGUMENT IS NOT AN INTEGER, NSIGDS IS GIVEN.     
C         
  90  IF (KIND(1).EQ.IONE) GO TO 140    
      L = IZERO     
      DO 100 I=1,NARGS        
        IF (KIND(I).EQ.IZERO) GO TO 100 
        L = L + IONE
        ARGS(L) = ARGS(I)     
 100  CONTINUE      
C         
      NL = IZERO    
      DO 130 I=1,NARGS        
        IF (KIND(I).EQ.IZERO) GO TO 110 
        NL = NL + IONE        
        GO TO 120   
 110    I2 = I - NL 
        IARGS(I2)  = IARGS(I) 
        ITYPE(I2)  = IONE     
        NSIGDS(I2) = ARGS(NL+1) + SPCA  
        IF (NSIGDS(I2).LT.IONE .OR. NSIGDS(I2).GT.ISIGD) GO TO 50     
        NWMAX(I2)  = IPLACE - ITWO      
        IRGS(I2)   = IARGS(I2)
 120    KIND(I2)   = IZERO    
 130  CONTINUE      
C         
      NA = NARGS - NL         
      NARGS = NA    
      CALL CHKCOL   
      GO TO 200     
C         
C     FIRST ARGUMENT IS NOT AN INTEGER, PARAMETERS ARE GIVEN.         
C         
 140  IF (ARGS(1).LE.RZERO) GO TO 50    
      NOCOLS = ARGS(1) + SPCA 
C         
C     NPAR = 1, FOR FORMS 1, 2, AND 3   
C            2, FOR FORM  4   
C            3, FOR FORM  5   
C            4, FOR FORM  6   
C         
      NPAR = IDIV (NARGS-IONE,NOCOLS,IND)         
      IF (NPAR.NE.ITWO .AND. NPAR.NE.ITHRE .AND. NPAR.NE.IFOUR) GO TO 30        
      A(1) = ABS (FLOAT(NPAR) * ARGS(1) + RONE - FLOAT(NARGS) )       
      IF (A(1).GT.RZERO) GO TO 30       
      NA = ARGS(1)  
      DO 190 I=1,NA 
C         
C     ISUB IS COLUMN NUMBER.  
C         
        ISUB = ITWO + NPAR * (I-IONE)   
        IRGS(I) = IARGS(ISUB) 
        IARGS(I) = IARGS(ISUB)
        CALL ADRESS (ISUB,IARGS(I))     
        IF (IARGS(I).LT.IZERO) CALL ERROR (20)    
        IF (NERROR.NE.IZERO) RETURN     
C         
C     NSIGDS(I) = (S)         
C     NWMAX (I) = (M)         
C     NBLANK(I) = (B)         
C         
        NSIGDS(I) = IARGS(ISUB+1)       
        NWMAX(I) = 22         
        IF (NPAR.GE.ITHRE) NWMAX(I) = IARGS(ISUB+2)         
        NBLANK(I) = ITHRE     
        IF (NPAR.EQ.IFOUR) NBLANK(I) = IARGS(ISUB+3)        
C         
C     READABLE.    (S) GT 0, (M) GT 0   
C         
        IF (NWMAX(I).GT.IZERO) GO TO 170
C         
C     INTEGER.     (S) EQ 0   
C         
        IF (NSIGDS(I).EQ.IZERO) GO TO 160         
C         
C     FIXED.       (S) GT 0, (M) LT 0   
C         
        IF (NWMAX(I).LT.IZERO) GO TO 150
C         
C     FLOATING.    (S) GT 0, (M) EQ 0   
C         
        ITYPE(I) = IFIVE      
        NWIDTH(I) = NSIGDS(I) + IFIVE   
        NDECS(I) = NSIGDS(I) + ITWO     
        GO TO 180   
C         
C     FIXED.        
C         
 150    ITYPE(I)  = 7         
        NWIDTH(I) = -NWMAX(I) 
        NDECS(I)  = NSIGDS(I) 
        NSIGDS(I) = ISIGD     
        GO TO 180   
C         
C     INTEGER.      
C         
 160    ITYPE(I)  = 9         
        NWIDTH(I) = -NWMAX(I) - IONE    
        NDECS(I)  = IZERO     
        NSIGDS(I) = ISIGD     
        GO TO 180   
C         
C     READABLE.     
C         
 170    ITYPE(I) = IONE       
C         
 180    IF (NSIGDS(I).LT.IONE .OR. NSIGDS(I).GT.ISIGD) GO TO 50       
 190  CONTINUE      
      IF (NPAR.EQ.IFOUR .AND. NBLANK(1).LT.IONE) NBLANK(1) = IONE     
C         
C     INITIALIZE AND CALL RFORMT.       
C         
 200  IF (NERROR.NE.IZERO) RETURN       
      IF (L1.NE.6) GO TO 210  
C         
C     ABRIDGE.      
C         
      CALL PRTABR (NA,NPAR,NRJ)         
      RETURN        
C         
C     ..................................................................        
C         
 210  I1 = IONE     
      DO 260 I=1,NA 
        K = IARGS(I)
C         
C       DETERMINE COUNT OF COL I.       
C         
        K1 = K + NRMAX - IONE 
        NCOUNT(I) = NRMAX     
        DO 220 J=1,NRMAX      
          IF (ABS(RC(K1)).GT.RZERO) GO TO 230     
          NCOUNT(I) = NCOUNT(I) - IONE  
          K1 = K1 - IONE      
 220    CONTINUE    
 230    IF (NCOUNT(I).GT.NRMAX-ITHRE) NCOUNT(I) = NRMAX     
        IF (NCOUNT(I).GT.IZERO) GO TO 240         
          NWIDTH(I) = NWMAX(I)
          NDECS(I) = IZERO    
          GO TO 250 
 240    IF (NWMAX(I).LE.IZERO) GO TO 250
        CALL RFORMT (0,NSIGDS(I),RC(K),A(1),NCOUNT(I),NWMAX(I),       
     1      NWIDTH(I),NDECS(I),NP(1),IRF)         
 250    IF (NPAR.EQ.IONE) NBLANK(I) = IPLACE - NWIDTH(I)    
 260  CONTINUE      
C         
C     COMPUTE I2, END OF DO INDEX FOR COLUMNS.    
C   
      IF (L1.NE.8) CALL PAGE (4)       
 270  I4     = IZERO    
      DO 290 I=I1,NA
        I4 = I4 + NWIDTH(I) + NBLANK(I) 
        IF (I4.LE.LWIDE) GO TO 280      
        I4 = I - I1 
        GO TO 300   
 280    IF (I.EQ.NA) I4 = NA - I1 + IONE
 290  CONTINUE      
C         
 300  I2 = I4 + I1 - IONE     
      NARGS = I2 - I1 + IONE  
C         
C     PUT IN COL HEADING IF FIRST ARG NOT A COL NO.         
C         
      IF (NPAR.EQ.IONE .OR. L1.EQ.8) GO TO 320    
      CALL RFORMT (11,1,RC,RZERO,0,0,NWID,NZERO,NP(1),IRF)  
      L = IONE      
      DO 310 I=I1,I2
        L = L + NWIDTH(I) + NBLANK(I)   
        IF (NWIDTH(I).LT.6 .OR. NCOUNT(I).EQ.IZERO) GO TO 310         
        A(200) = IRGS(I)      
        CALL RFORMT (9,NSD,RC,A(200),0,0,IFIVE,NZERO,NP(L-6),IRF)     
        NP(L-1) = LA(45)      
        IF (NWIDTH(I).LT.12) GO TO 310  
C         
C       INSERT THE WORD COLUMN.         
C         
        NP(L-12) = LA(13)     
        NP(L-11) = LA(25)     
        NP(L-10) = LA(22)     
        NP(L- 9) = LA(31)     
        NP(L- 8) = LA(23)     
        NP(L- 7) = LA(24)     
 310  CONTINUE      
      WRITE (IPRINT,380) (NP(I),I=2,L)  
      NLINES = NLINES + IONE
      GO TO 350     
 320  IF (L1.EQ.8) GO TO 360  
      CALL HEADS (IRGS(I1),I4,IZERO,IONE)         
      DO 340 I=I1,I2
        IF (NCOUNT(I).GT.IZERO) GO TO 340         
        I5 = 12 * (I-I1) + IONE         
        DO 330 I6=1,12        
          LHEAD(I5) = LA(45)  
          I5 = I5 + IONE      
 330    CONTINUE    
 340  CONTINUE      
C         
      I5 = 12 * I4  
      WRITE (IPRINT,390) (LHEAD(I6),I6=1,I5)      
      NLINES = NLINES + IONE
 350  WRITE (IPRINT,380)      
      NLINES = NLINES + IONE
C         
C     MAIN LOOP.    
C         
 360  CALL PRTMNL (I1,I2,I4,LEN,NPAR,NLINES)
C         
C     ADJUST FOR MORE THAN 8 COLUMNS.   
C         
      IF (I2.EQ.NA) RETURN    
      WRITE (IPRINT,380)      
      NLINES = NLINES + IONE
      I1 = I2 + IONE
      IF (L1.EQ.8 .OR. NLINES.LT.LEN) GO TO 270
      CALL PAGE (4)
      NLINES = IZERO
      GO TO 270     
C         
C     ==================================================================        
C         
C                          ***   FORMAT STATEMENTS   ***    
C         
 380  FORMAT (1X,119A1)       
 390  FORMAT (8(3X,12A1))     
C         
C     ==================================================================        
C         
      END 
 
