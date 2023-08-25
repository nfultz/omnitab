*AARGS
      SUBROUTINE AARGS (MARD)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  AARGS V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ASSEMBLES A FLOATING POINT NUMBER FROM A STRING OF
C        DIGITS ETC.  KRDPOS INITIALLY POINTS AT THE FIRST NUMBER.  IT
C        IS LEFT POINTING AT THE FIRST CHARACTER AFTER THE NUMBER.
C
C     VALUE RETURNED IN ARG IS
C
C        KARG =  1, FLOATING POINT
C             =  0, INTEGER
C             = -1, ERROR
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MARD(*)
C
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
C
      REAL             SIGN
      REAL             FDIV, FEXP2
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 14 /
      DATA ICB / 37 /
      DATA ICC / 38 /
      DATA ICD / 39 /
      DATA ICE / 44 /
C
C     ==================================================================
C
      ARG  = MARD(KRDPOS)
      SIGN = RONE
      JEXP = IZERO
      IXS  = IONE
      IEXP = IZERO
      KARG = IZERO
C
C     LOOK BACK FOR MINUS SIGN AND/OR DECIMAL POINT.
C
      K = MARD(KRDPOS-1)
      IF (K.NE.ICB) GO TO 10
      KARG = IONE
      IEXP = -IONE
      K = MARD(KRDPOS-2)
C
  10  IF (K.EQ.ICC) SIGN = -RONE
C
  20  KRDPOS = KRDPOS + IONE
      K = MARD(KRDPOS)
      IF (K.GE.ITEN) GO TO 30
      IEXP = IEXP - KARG
      ARG = RTEN*ARG + FLOAT(K)
      GO TO 20
C
  30  IF (K.NE.ICB) GO TO 50
C
C     DECIMAL POINT FOUND.
C
      IF (KARG.EQ.IZERO) GO TO 40
      CALL ERROR (3)
      KARG = -IONE
      RETURN
C
C     ..................................................................
C
  40  KARG = IONE
      GO TO 20
C
C     CHECK FOR EXPONENT   E X, E+X, E-X, +X, -X.
C
  50  IF (K.NE.ICA) GO TO 60
      KRDPOS = KRDPOS + IONE
      K = MARD(KRDPOS)
      IF (K.EQ .ICE) GO TO 70
      IF (K.LT.ITEN) GO TO 90
C
  60  IF (K.EQ.ICC) GO TO 80
      IF (K.NE.ICD) GO TO 110
C
  70  IF (L1.EQ.16 .AND. L2.EQ.6) GO TO 110
      KRDPOS = KRDPOS + IONE
      K = MARD(KRDPOS)
      IF (K.GE.ITEN) GO TO 110
      GO TO 90
C
  80  IF (L1.EQ.16 .AND. L2.EQ.6) GO TO 110
      IXS = -IONE
      GO TO 70
C
C
  90  KARG = KARG + IONE
 100  JEXP = ITEN*JEXP + K
      KRDPOS = KRDPOS + IONE
      K = MARD(KRDPOS)
      IF (K.LT.ITEN) GO TO 100
C
C     DONE WITH ARGUMENT.
C
 110  IF (KARG.EQ.IZERO) GO TO 150
      KARG = IONE
      IEXP = IXS*JEXP + IEXP
C
C     THE FOLLOWING CODING YIELDS MORE ACCURATE RESULTS THAN THE
C     OBVIOUS    ARG = ARG * 10. * IEXP
C
      JEXP = IABS (IEXP)
      IF (JEXP.GE.IFIX(RALOG)) GO TO 140
      IF (IEXP.EQ.IZERO) GO TO 150
      IF (IEXP.GT.IZERO) GO TO 130
      ARG = FDIV (ARG,FEXP2(RTEN,FLOAT(JEXP)),IND)
      GO TO 150
C
 130  ARG = ARG * FEXP2 (RTEN,FLOAT(JEXP))
      GO TO 150
C
 140  CALL ERROR (102)
      ARG = RZERO
      RETURN
C
C     ..................................................................
C
 150  ARG = SIGN * ARG
      IF (KARG.EQ.IZERO .AND. L1.EQ.16 .AND. L2.EQ.6) ARG = ABS (ARG)
      RETURN
C
C     ==================================================================
C
      END
*ACCDIG
      SUBROUTINE ACCDIG (X,Y,RDIGTS,ACURCY,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ACCDIG V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT ...
C
C       X      = FIRST  OF TWO REAL NUMBERS TO BE COMPARED.
C       Y      = SECOND OF TWO REAL NUMBERS TO BE COMPARED.
C       RDGITS = NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.
C
C     OUTPUT ...
C
C       ACURCY = - LOG10 (ABS((X-Y)/Y)))
C       IND    = O, IF TOLERANCE IS     SATISFIED.
C              = 1, IF TOLERANCE IS NOT SATISIFIED.
C
C     TOLERANCE ...
C
C        X AND Y ARE CONSIDERED EQUAL WITHIN RDGITS RELATIVE TOLERANCE,
C           IF ACURCY IS GREATER THAN RDGITS.
C
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             X, Y, RDIGTS, ACURCY
      REAL             DIFF
      REAL             FLOG10
C
C     ==================================================================
C
      IND = IZERO
      DIFF = X - Y
      IF (DIFF.NE.RZERO) GO TO 10
      ACURCY = RSD
      RETURN
C
C     ..................................................................
C
  10  IF (Y.NE.RZERO) GO TO 20
      ACURCY = - FLOG10 (ABS (X))
      GO TO 30
C
C     ..................................................................
C
  20  ACURCY = - FLOG10 ( ABS(DIFF) ) + FLOG10 ( ABS(Y) )
C
  30  IF (ACURCY.GE.RDIGTS) RETURN
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*ADRESS
      SUBROUTINE ADRESS (I,J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ADRESS V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALCULATE J = ADDRESS OF ARGUMENT (I).
C
C        J =       -I, IF ARGUMENT (I) IS REAL,
C          =        0, IF ARGUMENT (I) IS ILLEGAL COLUMN NUMBER,
C          =  ADDRESS, IF ARGUMENT (I)      LEGAL COLUMN NUMBER.
C
C     IF J = 0, ERROR(11) IS CALLED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (KIND(I).EQ.IZERO) GO TO 10
      J = -I
      RETURN
C
C     ..................................................................
C
  10  IF (IARGS(I).GE.IONE .AND. IARGS(I).LE.NCOL) GO TO 20
      J = IZERO
      CALL ERROR (11)
      RETURN
C
C     ..................................................................
C
  20  J = NROW*(IARGS(I)-IONE) + IONE
      RETURN
C
C     ==================================================================
C
      END
*ASTER
      SUBROUTINE ASTER
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  ASTER V 7.00 10/26/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ASTERISKS HAVE BEEN FOUND, LOOK FOR A SPECIAL FORM OF ARGUMENT.
C
C     FORMS CAN BE..
C
C      *PHYCON*       A PHYSICAL CONSTANT NAME, FLOATING POINT NO.
C     **VARCON**      A VARIABLE TO BE USED AS AN INTEGER (TRUNCATED)
C      *VARCON*       A VARIABLE TO BE USED AS A FLOATING POINT NUMBER
C     **ROW,COLUMN**  A WORKSHEET ENTRY TO BE USED AS INTEGER, TRUNCATED
C      *ROW,COLUMN*   A WORKSHEET ENTRY TO BE USED AS FLOATING POINT NO.
C
C        A VARIABLE IS EITHER V, W, X, Y, OR Z.
C
C     NONBLA IS A PROCEDURE WHICH RETURNS THE NEXT NON-BLANK CHARACTER
C        IN THE CARD AND ALSO POINTS KRDPOS AT IT.
C
C        KARG = 1, SINGLE *
C             = 0, DOUBLE *
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NAM(2)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             T
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 36 /
      DATA ICB / 40 /
      DATA ICC / 43 /
      DATA ICD /  8 /
C
C     ==================================================================
C
      L = KARG
      MZZ = KRDPOS
      CALL NONBLA (MZZ,K)
      IF (K.NE.ICB) GO TO 20
C
C     A LONG LINE OF ASTERISKS FOUND, SKIP OVER THEM AND IGNORE.
C
      KARG = 7
  10  KRDPOS = KRDPOS + IONE
      IF (KARD(KRDPOS).EQ.ICB) GO TO 10
      RETURN
C
C     ..................................................................
C
  20  IF (K.GE.ICA) GO TO 70
      IF (K.GE.ITEN) GO TO 50
C
C     NUMBER IS FIRST NON-BLANK CHARACTER, SET N = COMMA.
C
      N = ICC
      T = RZERO
  30  CALL AARGS (KARD)
      IF (KARG.NE.IZERO) GO TO 70
      MZZ = KRDPOS
      CALL NONBLA (MZZ,MZA)
      IF (MZA.NE.N) GO TO 70
      IF (N.EQ.ICB) GO TO 40
      MZZ = KRDPOS + IONE
      CALL NONBLA (MZZ,MZA)
      IF (MZA.GE.ITEN) GO TO 120
C
C     SET N = ASTERISK.
C
      N = ICB
      T = ARG
      GO TO 30
C
  40  ARG2 = ARG
      ARG = T
      KARG = IFIVE
      GO TO 100
C
C     LETTER FOUND FIRST.
C
  50  CALL NNAME (NAM(1))
      CALL PHYCON (NAM(1))
      IF (ARG.EQ.RZERO) GO TO 60
C
C     PHYSICAL CONSTANT FOUND, SET KARG = 1.
C
      KARG = IONE
      IF (L.LT.IZERO) GO TO 70
      GO TO 90
C
C     NAME NOT IN PHYSICAL CONSTANT LIST, TRY VARIABLE LIST.
C
  60  CALL VARCON (NAM(1))
      IF (ARG.NE.RZERO) GO TO 80
  70  KARG = IONE
      RETURN
C
C     ..................................................................
C
  80  KARG = ITHRE
  90  MZZ = KRDPOS
      CALL NONBLA (MZZ,MZA)
      IF (MZA.NE.ICB) GO TO 70
 100  KRDPOS = KRDPOS + IONE
C
C     CHECK THAT THE NUMBER OF ASTERISKS AT THE END OF THE EXPRESSION
C     IS THE SAME AS AT THE BEGINNING. L=0 MEANS 2, L=1 MEANS 1.
C
      IF (L.NE.IZERO .AND. KARD(KRDPOS).EQ.ICB) GO TO 70
      IF (L.NE.IZERO) GO TO 110
      IF (KARD(KRDPOS).NE.ICB .OR. KARD(KRDPOS+1).EQ.ICB) GO TO 70
 110  IF (KARD(KRDPOS).NE.46) KRDPOS = KRDPOS + IONE
      KARG = KARG + L
      RETURN
C
C     ..................................................................
C
 120  KARG = ICD + L
      RETURN
C
C     ==================================================================
C
      END
*ATOI
      SUBROUTINE ATOI (NALPHA,N,M,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81.   ATOI V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CONVERTS DIGITS OF ALPHANUMERIC REPRESENTATION OF A NUMBER
C        TO AN INTEGER
C
C     NALPHA CONTAINS ALPHANUMERIC REPRESENTATION IN A1 FORMAT OBTAINED
C        BY USING SUBROUTINE RFORMT
C
C     N IS THE LENGTH OF NALPHA
C
C     M IS THE INTEGER REPRESENTATION
C
C     IND = 0, IF EVERYTHING IS OK.
C           1, IF N LESS THAN 1
C           2, IF INTEGER IS TOO LARGE
C           3, IF ALPHA CHARACTER FOUND
C              OTHER THAN PLUS,MINUS,
C              BLANK OR PERIOD.  M SET
C              EQUAL TO ZERO
C     MINUS SIGN IS TREATED AS A BLANK.
C     DECIMAL POINT IS IGNORED.  BLANKS
C      ARE TREATED AS ZEROS.
C
C     IF -1.4236 IS IN L(1), L(2), L(3), L(4), L(5), L(6) AND L(7), THEN
C        CALL ATOI (L,6,M,IND) COMPUTES M = 1423
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1972.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NALPHA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /SLCONS/ MXLIN, MXWDTH
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER    LA*1
      CHARACTER    NALPHA*1
C
C     ==================================================================
C
      IND = IZERO
      IF (N.LT.IONE) GO TO 60
      M = IZERO
      K = N
      J2 = IONE
      MXINT1 = IDIV (MXINT,ITEN,IND)
      DO 50 I=1,N
        IF (NALPHA(K).EQ.LA(38)) GO TO 40
        IF (NALPHA(K).NE.LA(45)) GO TO 10
        J1 = IZERO
        GO TO 30
  10    IF (NALPHA(K).EQ. LA(40)) GO TO 50
        IF (NALPHA(K).EQ.LA(39)) GO TO 50
        DO 20 JJ=1,10
          IF (NALPHA(K).NE.LA(JJ)) GO TO 20
          J1 = JJ - IONE
          GO TO 30
  20    CONTINUE
C
        IND = ITHRE
        M = IZERO
        RETURN
C
C     ..................................................................
C
  30    M = M + J1*J2
        IF (J2.GT.MXINT1) GO TO 70
        J2 = J2 * ITEN
  40    K = K - IONE
  50  CONTINUE
      RETURN
C
C     ..................................................................
C
  60  IND = IONE
      RETURN
C
C     ..................................................................
C
  70  IND = ITWO
      RETURN
C
C     ==================================================================
C
      END
*AUTCOR
      SUBROUTINE AUTCOR (X,N,R)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  AUTCO V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE AUTOCORRELATION COEFFICIENT.
C        COMPUTES THE PEARSON PRODUCT MOMENT CORRELATION COEFFICIENT
C        BETWEEN X2,X3,...,XN AND X1,X2,...,XN-1.
C
C     INPUT  - X, VECTOR OF MEASUREMENTS
C              N, NUMBER OF MEASUREMENTS.
C     OUTPUT - R, AUTOCORRELATION COEFFICIENT.
C
C         SUM (X-XBAR)*(Y-YBAR)
C     R = --------------------------------------
C         SQRT SUM((X-XBAR)**2)*SUM((Y=YBAR)**2)
C
C       X = X(2),X(3),...,X(N)
C       Y = X(1),X(2),...,X(N-1).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A3(1), A4(1), A5(1), X(N)
      REAL             ANM1, A1, A2, R, SUM, SUMPRD, SUM1, SUM2
      REAL             XBAR1, XBAR2
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
      IF (N.GT.ITWO) GO TO 10
      R = RZERO
      RETURN
C
  10  ANM1 = FLOAT(N-IONE)
      CALL SUMMAL (X(2),N-IONE,SUM)
      IF (N.EQ.ITWO) SUM = X(2)
      XBAR1 = FDIV(SUM,ANM1,IND)
      CALL SUMMAL (X(1),N-IONE,SUM)
      IF (N.EQ.ITWO) SUM = X(1)
      XBAR2 = FDIV(SUM,ANM1,IND)
C
C     COMPUTE SUM (X-XBAR)**2.
C
      CALL SUMMAL (A3,IZERO,SUM1)
      DO 20 I=2,N
        A1 = X(I) - XBAR1
        A3(1) = A1 * A1
        CALL SUMMAL (A3,-IONE,SUM1)
  20  CONTINUE
      CALL SUMMAL (A3,IONE,SUM1)
C
C     COMPUTE SUM (Y-YBAR)**2.
C
      CALL SUMMAL (A4,IZERO,SUM2)
      DO 30 I=2,N
        A2 = X(I-1) - XBAR2
        A4(1) = A2 * A2
        CALL SUMMAL (A4,-IONE,SUM2)
  30  CONTINUE
      CALL SUMMAL (A4,IONE,SUM2)
C
C     COMPUTE SUM OF (X-XBAR)*(Y-YBAR).
C
      CALL SUMMAL (A5,IZERO,SUMPRD)
      DO 40 I=2,N
        A1 = X(I) - XBAR1
        A2 = X(I-1) - XBAR2
        A5(1) = A1 * A2
        CALL SUMMAL (A5,-IONE,SUMPRD)
  40  CONTINUE
      CALL SUMMAL (A5,IONE,SUMPRD)
C
C     COMPUTE AUTOCORRELATION COEFFICIENT.
C
      R = FDIV (SUMPRD,FSQRT(SUM1*SUM2),IND)
      IF (R.GE.RONE) R = RONE
      IF (R.LE.(-RONE)) R = -RONE
      RETURN
C
C     ==================================================================
C
      END
*BACK
      SUBROUTINE BACK (NC,LB,L,K,MV,RS,A,I,JC,ID,XI,MD,II,NI,ND,KZ,NL,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   BACK V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                         LOOK BACK COMPUTATION OF RSS
C
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
C                   REGRESSIONS BY LEAPS AND BOUNDS
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
C                     G.M.FURNIVAL AND R.W.WILSON
C               YALE UNIVERSITY AND U.S. FOREST SERVICE
C                           VERSION 11/11/74
C
C               ADAPTED TO OMNITAB BY -
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
      DIMENSION I(ND,ND), ID(ND), K(ND), NC(ND,ND), NI(ND), MD(ND,ND)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             XI(NL)
      REAL             A, RS
      REAL             B
      REAL             FDIV
C
C     ==================================================================
C
C                               FIND SOURCE MATRIX.
C
  10  ISUB1 = K(JC)
      IF (LB.LE.NI(ISUB1)) GO TO 20
      JC = JC - IONE
      GO TO 10
C
C                            ADJUST FOR PREVIOUS PIVOTS.
C
  20  ISUB2 = IONE
      ISUB3 = IONE
      DO 50 J=JC,MV
        IN    = K(J)
        L     = I(IN,LB)
        MM    = ID(IN)
        ISUB2 = MM + MD(L,KZ)
        ISUB3 = MM + MD(L,L)
        IF (J.EQ.MV) GO TO 60
        IS    = K(J+1)
        ISUB4 = ID(IS) + MD(LB,KZ)
        IP    = I(IN,IS-1)
        ISUB5 = MM + MD(IP,L)
        ISUB6 = MM + MD(IP,IP)
        ISUB7 = MM + MD(IP,KZ)
        B     = FDIV (XI(ISUB5),XI(ISUB6),IND)
        KA    = IS
  30    IF (KA.GT.LB) GO TO 40
        KN    = I(IN,KA)
        ISUB8 = ID(IS) + MD(KA,LB)
        ISUB9 = MM + MD(KN,L)
        ISUB0 = MM + MD(KN,IP)
        XI(ISUB8) = XI(ISUB9) - B * XI(ISUB0)
        KA    = KA + IONE
        GO TO 30
  40    XI(ISUB4) = XI(ISUB2) - B * XI(ISUB7)
        NI(IS) = LB
        I(IS,LB) = LB
        N = N + ITHRE + LB - IS
        IF (II.EQ.IZERO) NC(IS,LB) = NC(IN,L)
  50  CONTINUE
C
C                                 CURRENT PIVOT.
C
  60  RS = A - FDIV (XI(ISUB2)*XI(ISUB2),XI(ISUB3),IND)
      RETURN
C
C     ==================================================================
C
      END
*BEGIN
      SUBROUTINE BEGIN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  BEGIN V 7.00  4/10/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE BEGIN OR SCAN INSTRUCTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
C
C     ==================================================================
C
      IF (L2.EQ.IONE) GO TO 20
C
C     SCAN CARD UP TO AND INCLUDING CARD COLUMN (C).
C
      IF (NARGS.GT.IONE) CALL ERROR (221)
      IF (NARGS.GE.IONE .AND. KIND(1).EQ.IZERO .AND. IARGS(1).GE.6
     1     .AND. IARGS(IONE).LE.LENCRD) GO TO 10
      CALL ERROR (205)
      RETURN
C
C     ..................................................................
C
  10  KRDEND = IARGS(1)
      IF (LEVEL.GT.IZERO) GO TO 30
      RETURN
C
C     ..................................................................
C
C     BEGIN STORING INSTRUCTIONS AT NUMBER (C).
C        IF NO NUMBER IS GIVEN, 1 IS ASSUMED.
C
  20  IF (MODE.EQ.IONE) GO TO 40
  30  CALL ERROR (5)
      RETURN
C
C     ..................................................................
C
  40  IF (NARGS.LT.IONE) GO TO 50
      IF (NARGS.EQ.IONE) GO TO 60
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  50  NSTMT = IZERO
      GO TO 90
C
  60  IF (KIND(1).EQ.IZERO) GO TO 70
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  70  IF (IARGS(1).GT.IZERO .AND. IARGS(1).LT.1000) GO TO 80
      CALL ERROR (2)
      RETURN
C
C     ..................................................................
C
  80  NSTMT = ITEN * (IARGS(1)-IONE)
  90  MODE = ITHRE
      RETURN
C
C     ==================================================================
C
      END
*BEJN
      SUBROUTINE BEJN (IST,R,Z)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   BEJN V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     IF IST = 0,  ENTRY IS FOR BEJN.
C     IF IST = 1,  ENTRY IS FOR BEIN.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION R(*)
      DOUBLE PRECISION A, B, C, D, E, F, G, P, Q, X, Y, Z
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE
      DOUBLE PRECISION DPCF, DPCG
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA  DPCA,    DPCB,   DPCC,    DPCD /
     1    60.0D0, 100.0D0, 15.0D0, 0.5D-30 /
C
      DATA  DPCE,   DPCF,    DPCG /
     1     6.0D0, 2.0D-9, 5.0D-20 /
C
C     ==================================================================
C
      Y = DONE
      IF (IST.NE.IZERO) Y = -DONE
      X = Z
      DO 10 N=1,100
        R(N) = DZERO
  10  CONTINUE
C
      LA = IZERO
      IF (X.LE.DPCA) GO TO 30
      LA = IONE
      IF (X.LE.DPCB) GO TO 20
      CALL ERROR (225)
      RETURN
C
C     ..................................................................
C
  20  X = FDDIV (X,DTWO,IND)
  30  A = FDDIV (X,DTWO,IND)
      IF (X.GT.DPCC) GO TO 100
      B = DONE
      C = DONE
      DO 40 N=1,100
        J = N
        B = FDDIV (B*A,C,IND)
        C = C + DONE
        IF (B.LT.DPCD) GO TO 50
  40  CONTINUE
C
  50  D = FDDIV (B*A,C,IND)
      A = A**2
      K = X + DPCE
      E = K
      F = K + J
      G = F + DONE
      P = DONE
      Q = DONE
      DO 60 N=1,K
        P = DONE - FDDIV (P*A,E*F,IND) * Y
        Q = DONE - FDDIV (Q*A,E*G,IND) * Y
        E = E - DONE
        F = F - DONE
        G = G - DONE
  60  CONTINUE
C
      R(J+1) = B * P
      R(J+2) = D * Q
  70  DO 80 N=1,J
        K = J - N + IONE
        A = K
        R(K) = FDDIV (DTWO*A*R(K+1),X,IND) - R(K+2) * Y
  80  CONTINUE
C
      IF (LA.EQ.IZERO) RETURN
      LA = LA - IONE
      A = R(1) * R(100)
      B = DZERO
      DO 90 N=1,99
        K = IHRD - N
        A = A + R(N+1) * R(K)
        B = B + R(N) * R(K)
  90  CONTINUE
C
      J = 98
      R(100) = A
      R(99) = B
      X = Z
      GO TO 70
 100  K = (DONE+DHALF) * X
      B = DONE
      C = K
      DO 110 N=1,K
        B = FDDIV (A*B,C,IND)
        C = C - DONE
 110  CONTINUE
C
      P = DPCF
      IF (LA.EQ.IONE) P = DPCG
      C = K + IONE
      DO 120 N=1,30
        J = K + N
        B = FDDIV (B*A,C,IND)
        C = C + DONE
        IF (B.LT.P) GO TO 50
        IF (J.EQ.98) GO TO 50
 120  CONTINUE
C
      GO TO 50
C
C     ==================================================================
C
      END
*BEZERO
      SUBROUTINE BEZERO (A,B,M,L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BEZERO V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION A(*), B(*)
      DOUBLE PRECISION AA, AB, AC, X, Y
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DPC(22)
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF, DPCG
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPC( 1) /  2.4048255770D0 /
      DATA DPC( 2) /  0.5191474973D0 /
      DATA DPC( 3) /  5.5200781103D0 /
      DATA DPC( 4) / -0.3402648065D0 /
      DATA DPC( 5) /  8.6537279129D0 /
      DATA DPC( 6) /  0.2714522999D0 /
      DATA DPC( 7) / 11.7915344391D0 /
      DATA DPC( 8) / -0.2324598314D0 /
      DATA DPC( 9) / 14.9309177086D0 /
      DATA DPC(10) /  0.2065464331D0 /
      DATA DPC(11) / 18.0710639679D0 /
      DATA DPC(12) / -0.1877288030D0 /
      DATA DPC(13) / 21.2116366299D0 /
      DATA DPC(14) /  0.1732658942D0 /
      DATA DPC(15) / 24.3524715308D0 /
      DATA DPC(16) / -0.1617015507D0 /
      DATA DPC(17) / 27.4934791320D0 /
      DATA DPC(18) /  0.1521812138D0 /
      DATA DPC(19) / 30.6346064684D0 /
      DATA DPC(20) / -0.1441659777D0 /
      DATA DPC(21) / 33.7758202136D0 /
      DATA DPC(22) /  0.1372969434D0 /
C
      DATA DPCA /      3779.0D0 /
      DATA DPCB /   6277237.0D0 /
      DATA DPCC /         7.0D0 /
      DATA DPCD /        31.0D0 /
      DATA DPCE /         5.0D0 /
      DATA DPCF /        56.0D0 /
      DATA DPCG / 1.595769122D0 /
C
      DATA   IC /        44   /
C
C     ==================================================================
C
      KB = IONE
      N = M
  10  J = IFOUR * N - IONE
      IF (J.GT.IC) GO TO 20
C
      I = ITWO * (N-IONE) + IONE
      X = DPC(I)
      Y = DPC(I+1)
      GO TO 30
C
  20  X  = J
      X  = X * DPI
      AA = FDDIV (DONE,X**2,IND)
      AB = DPCA - FDDIV (DPCB*AA,DPCC,IND)
      AB = DPCD - FDDIV (AA*AB,DPCE,IND)
      AB = DONE - FDDIV (AA*AB,DTHRE,IND)
      AB = DONE + DTWO * AA * AB
      J  = IDIV (N,ITWO,IND)
      J  = ITWO * J
      AC = DONE
      IF (J.EQ.N) AC = -DONE
      Y  = DONE - FDDIV (AA**2*DPCF,DTHRE,IND)
      Y  = FDDIV (DPCG*AC*Y,FDSQRT(X),IND)
      X  = FDDIV (X*AB,DFOR,IND)
C
  30  A(KB) = X
      B(KB) = Y
      N  = N + IONE
      KB = KB + IONE
      IF (KB.LE.L) GO TO 10
      RETURN
C
C     ================================================================
C
      END
*BEZONE
      SUBROUTINE BEZONE (A,B,M,L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BEZONE V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION A(*), B(*)
      DOUBLE PRECISION R, S, T, X, Y
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DPC(22)
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPC( 1) /  3.8317059702D0 /
      DATA DPC( 2) / -0.4027593957D0 /
      DATA DPC( 3) /  7.0155866698D0 /
      DATA DPC( 4) /  0.3001157525D0 /
      DATA DPC( 5) / 10.1734681351D0 /
      DATA DPC( 6) / -0.2497048771D0 /
      DATA DPC( 7) / 13.3236919363D0 /
      DATA DPC( 8) /  0.2183594072D0 /
      DATA DPC( 9) / 16.4706300509D0 /
      DATA DPC(10) / -0.1964653715D0 /
      DATA DPC(11) / 19.6158585105D0 /
      DATA DPC(12) /  0.180063375 D0 /
      DATA DPC(13) / 22.7600843806D0 /
      DATA DPC(14) / -0.1671846005D0 /
      DATA DPC(15) / 25.9036720876D0 /
      DATA DPC(16) /  0.1567249863D0 /
      DATA DPC(17) / 29.0468285349D0 /
      DATA DPC(18) / -0.1480111100D0 /
      DATA DPC(19) / 32.1896799110D0 /
      DATA DPC(20) /  0.1406057982D0 /
      DATA DPC(21) / 35.3323075501D0 /
      DATA DPC(22) / -0.1342112403D0 /
C
      DATA DPCA /       157.2D0 /
      DATA DPCB /    130080.6D0 /
      DATA DPCC /         7.0D0 /
      DATA DPCD /        24.0D0 /
      DATA DPCE /        81.6D0 /
      DATA DPCF / 1.595769122D0 /
C
      DATA   IC /     46     /
C
C     ==================================================================
C
      KB = IONE
      N = M
  10  J = IFOUR * N + IONE
      IF (J.GT.IC) GO TO 20
C
      I = ITWO * (N-IONE) + IONE
      X = DPC(I)
      Y = DPC(I+1)
      GO TO 30
C
  20  X = J
      X = X * DPI
      R = FDDIV (DONE,X**2,IND)
      S = DONE - DSIX * R * (DONE - R * (DONE - R * (DPCA -
     1           FDDIV (DPCB*R,DPCC,IND) ) ) )
      J = IDIV (N,ITWO,IND)
      J = ITWO * J
      T = DONE
      IF (J.NE.N) T = -DONE
      Y = FDDIV (T*DPCF*(DONE+R**2*DPCD*(DONE-DPCE*R)),
     1           FDSQRT (X),IND)
      X = FDDIV (S*X,DFOR,IND)
C
  30  A(KB) = X
      B(KB) = Y
      N  = N  + IONE
      KB = KB + IONE
      IF (KB.LE.L) GO TO 10
      RETURN
C
C     ==================================================================
C
      END
*BINCDF
      SUBROUTINE BINCDF (X,P,N,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BINCDF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE X
C              FOR THE BINOMIAL DISTRIBUTION
C              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
C              PARAMETER = P,
C              AND INTEGER 'NUMBER OF BERNOULLI TRIALS'
C              PARAMETER = N.
C              THE BINOMIAL DISTRIBUTION USED
C              HEREIN HAS MEAN = N*P
C              AND STANDARD DEVIATION = SQRT(N*P*(1-P)).
C              THIS DISTRIBUTION IS DEFINED FOR ALL
C              DISCRETE INTEGER X BETWEEN 0 (INCLUSIVELY)
C              AND N (INCLUSIVELY).
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = C(N,X) * P**X * (1-P)**(N-X).
C              WHERE C(N,X) IS THE COMBINATORIAL FUNCTION
C              EQUALING THE NUMBER OF COMBINATIONS OF N ITEMS
C              TAKEN X AT A TIME.
C              THE BINOMIAL DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF
C              SUCCESSES IN N BERNOULLI (0,1)
C              TRIALS WHERE THE PROBABILITY OF SUCCESS
C              IN A SINGLE TRIAL = P.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE
C                                AT WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE INTEGRAL-VALUED,
C                                AND BETWEEN 0.0 (INCLUSIVELY)
C                                AND N (INCLUSIVELY).
C                     --P      = THE SINGLE PRECISION VALUE
C                                OF THE 'BERNOULLI PROBABILITY'
C                                PARAMETER FOR THE BINOMIAL
C                                DISTRIBUTION.
C                                P SHOULD BE BETWEEN
C                                0.0 (EXCLUSIVELY) AND
C                                1.0 (EXCLUSIVELY).
C                     --N      = THE INTEGER VALUE
C                                OF THE 'NUMBER OF BERNOULLI TRIALS'
C                                PARAMETER.
C                                N SHOULD BE A POSITIVE INTEGER.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF
C             FOR THE BINOMIAL DISTRIBUTION
C             WITH 'BERNOULLI PROBABILITY' PARAMETER = P
C             AND 'NUMBER OF BERNOULLI TRIALS' PARAMETER = N.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE INTEGRAL-VALUED,
C                   AND BETWEEN 0.0 (INCLUSIVELY)
C                   AND N (INCLUSIVELY).
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C                 --N SHOULD BE A POSITIVE INTEGER.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--NOTE THAT EVEN THOUGH THE INPUT
C              TO THIS CUMULATIVE
C              DISTRIBUTION FUNCTION SUBROUTINE
C              FOR THIS DISCRETE DISTRIBUTION
C              SHOULD (UNDER NORMAL CIRCUMSTANCES) BE A
C              DISCRETE INTEGER VALUE,
C              THE INPUT VARIABLE X IS SINGLE
C              PRECISION IN MODE.
C              X HAS BEEN SPECIFIED AS SINGLE
C              PRECISION SO AS TO CONFORM WITH THE DATAPAC
C              CONVENTION THAT ALL INPUT ****DATA****
C              (AS OPPOSED TO SAMPLE SIZE, FOR EXAMPLE)
C              VARIABLES TO ALL
C              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
C              THIS CONVENTION IS BASED ON THE BELIEF THAT
C              1) A MIXTURE OF MODES (FLOATING POINT
C              VERSUS INTEGER) IS INCONSISTENT AND
C              AN UNNECESSARY COMPLICATION
C              IN A DATA ANALYSIS; AND
C              2) FLOATING POINT MACHINE ARITHMETIC
C              (AS OPPOSED TO INTEGER ARITHMETIC)
C              IS THE MORE NATURAL MODE FOR DOING
C              DATA ANALYSIS.
C
C     REFERENCES--HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGE 38.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 945, FORMULAE 26.5.24 AND 
C                 26.5.28, AND PAGE 929.
C               --JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1969, PAGES 50-86,
C                 ESPECIALLY PAGES 63-64.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 135-142.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 120-125.
C               --MOOD AND GRABLE, INTRODUCTION TO THE THEORY
C                 OF STATISTICS, EDITION 2, 1963, PAGES 64-69.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGES 264-272.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY 
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--NOVEMBER  1975.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, P, CDF
      REAL             AN, DEL, FINTX
      REAL             SPCA, SPCB, SPCC
      REAL             FDIV, FDPCON
C
      DOUBLE PRECISION TERM(1)
      DOUBLE PRECISION DX, ANU1, ANU2, Z, SUM, AI, COEF1, COEF2
      DOUBLE PRECISION ARG, COEF
      DOUBLE PRECISION THETA, SINTH, COSTH, A, B
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DATAN
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 0.001  /
      DATA SPCB / 0.0001 /
      DATA SPCC / 0.1    /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      AN = N
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
        IND = ITHRE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (N.GT.IZERO) GO TO 20
        IND = IFOUR
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  20  IF (X.GE.RZERO .AND. X.LE.AN) GO TO 30
        IND = IONE
        IF (X.LT.RZERO) CDF = RZERO
        IF (X.GT.AN   ) CDF = RONE
        RETURN
C
C     ..................................................................
C
  30  INTX = X + SPCB
      FINTX = INTX
      DEL = X - FINTX
      IF (DEL.LT.RZERO) DEL = -DEL
      IF (DEL.GT.SPCA) IND = ITWO
C
C     ---   START POINT   ----------------------------------------------
C
C     TREAT IMMEDIATELY THE SPECIAL CASE OF X = N,
C     IN WHICH CASE CDF = 1.0.
C
      INTX = X + SPCB
      CDF = RONE
      IF (INTX.EQ.N) RETURN
C
C     EXPRESS THE BINOMIAL CUMULATIVE DISTRIBUTION
C        FUNCTION IN TERMS OF THE EQUIVALENT F
C        CUMULATIVE DISTRIBUTION FUNCTION,
C        AND THEN EVALUATE THE LATTER.
C
      AN = N
      DX = FDIV (P,RONE-P,JIND) * FDIV (AN-X,X+RONE,JIND)
      NU1 = RTWO * (X+RONE) + SPCC
      NU2 = RTWO * (AN-X) + SPCC
      ANU1 = NU1
      ANU2 = NU2
      Z = FDDIV (ANU2,ANU2+ANU1*DX,JIND)
C
C     DETERMINE IF NU1 AND NU2 ARE EVEN OR ODD.
C
      IFLAG1 = NU1 - ITWO * IDIV (NU1,ITWO,JIND)
      IFLAG2 = NU2 - ITWO * IDIV (NU2,ITWO,JIND)
      IF (IFLAG1.EQ.IZERO) GO TO 40
      IF (IFLAG2.EQ.IZERO) GO TO 70
      GO TO 100
C
C     DO THE NU1 EVEN AND NU2 EVEN OR ODD CASE.
C
  40  SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      IMAX = IDIV (NU1-ITWO,ITWO,JIND)
      IF (IMAX.LE.IZERO) GO TO 60
      DO 50 I=1,IMAX
        AI      = I
        COEF1   = DTWO * (AI-DONE)
        COEF2   = DTWO * AI
        TERM(1) = TERM(1) * FDDIV (ANU2+COEF1,COEF2,JIND) * (DONE-Z)
        CALL DSUMAL (TERM,-IONE,SUM)
  50  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  60  SUM = SUM + DONE
      SUM = Z**FDDIV (ANU2,DTWO,JIND) * SUM
      CDF = FDPCON (SUM)
      RETURN
C
C     ..................................................................
C
C     DO THE NU1 ODD AND NU2 EVEN CASE.
C
  70  SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      IMAX = IDIV (NU2-ITWO,ITWO,JIND)
      IF (IMAX.LE.IZERO) GO TO 90
      DO 80 I=1,IMAX
        AI      = I
        COEF1   = DTWO * (AI-DONE)
        COEF2   = DTWO * AI
        TERM(1) = TERM(1) * FDDIV (ANU1+COEF1,COEF2,JIND) * Z
        CALL DSUMAL (TERM,-IONE,SUM)
  80  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  90  SUM = SUM + DONE
      CDF = FDPCON (DONE - ((DONE-Z)**FDDIV(ANU1,DTWO,JIND)) * SUM )
      RETURN
C
C     ..................................................................
C
C     DO THE NU1 ODD AND NU2 ODD CASE.
C
 100  SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      ARG = FDSQRT (FDDIV(ANU1,ANU2,JIND)*DX)
      THETA = DATAN (ARG)
      SINTH = FDDIV (ARG,FDSQRT(DONE+ARG*ARG),JIND)
      COSTH = FDDIV (DONE,FDSQRT(DONE+ARG*ARG),JIND)
      IF (NU2.EQ.IONE) GO TO 130
      IF (NU2.EQ.ITHRE) GO TO 120
      IMAX = NU2 - ITWO
      DO 110 I=3,IMAX,2
        AI = I
        COEF1 = AI - DONE
        COEF2 = AI
        TERM(1) = TERM(1) * FDDIV (COEF1,COEF2,JIND) * (COSTH*COSTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 110  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 120  SUM = SUM + DONE
      SUM = SUM * SINTH * COSTH
C
 130  A = FDDIV (DTWO,DPI,JIND) * (THETA+SUM)
      SUM = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      B = DZERO
      IF (NU1.EQ.IONE)  GO TO 190
      IF (NU1.EQ.ITHRE) GO TO 150
      IMAX = NU1 - ITHRE
      DO 140 I=1,IMAX,2
        AI = I
        COEF1 = AI
        COEF2 = AI + DTWO
        TERM(1) = TERM(1) * FDDIV(ANU2+COEF1,COEF2,JIND) * (SINTH*SINTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 140  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 150  SUM = SUM + DONE
      SUM = SUM * SINTH * (COSTH**N)
      COEF = DONE
      IEVODD = NU2 - ITWO * IDIV (NU2,ITWO,JIND)
      IMIN = ITHRE
      IF (IEVODD.EQ.IZERO) IMIN = ITWO
      IF (IMIN.GT.NU2) GO TO 170
      DO 160 I=IMIN,NU2,2
        AI = I
        COEF = FDDIV (AI-DONE,AI,JIND) * COEF
 160  CONTINUE
C
 170  COEF = COEF * ANU2
      IF (IEVODD.EQ.IZERO) GO TO 180
      COEF = COEF * FDDIV (DTWO,DPI,JIND)
C
 180  B = COEF * SUM
C
 190  CDF = FDPCON ( DONE - (A-B) )
      RETURN
C
C
C     ==================================================================
C
      END
*BINPDF
      SUBROUTINE BINPDF (X,N,P,PDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BINPDF V 7.00 2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE THE BINOMIAL P.D.F.
C
C     USES RECURRENCE RELATION IN DOUBLE PRECISION.
C
C        F(X+1) = (N-X) * P *  F(X)
C                 -----   -
C                 (X+1)   Q
C
C      IND = FAULT INDICATOR.
C          = 0, IF EVERYTHING IS OK.
C          = 1, IF N IS NOT POSITIVE.
C          = 2, IF P IS NOT BETWEEN ZERO AND ONE.
C          = 3, IF X IS NEGATIVE OR GREATER THAN N.
C          = 4, IF X IS NOT INTEGRAL.
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, P, PDF
      REAL             RX, RN, RIX
      REAL             FDPCON
C
      DOUBLE PRECISION DP
      DOUBLE PRECISION DQ, DNUM, DDEN, DPDIVQ, TERM
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      IND = IZERO
      RX  = X
      RN  = N
      IF (RX.GE.RZERO .AND. RX.LE.RN) GO TO 10
        IND = ITHRE
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (N.GT.IZERO) GO TO 20
        IND = IONE
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
  20  IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 30
        IND = ITWO
        PDF = RZERO
        RETURN
  30  M   = RX
      RIX = M
C
C     X MUST BE INTEGRAL.
C
      IF (RIX.EQ.RX) GO TO 40
        IND = IFOUR
C
C     ==================================================================
C
  40  DP   = P
      DQ   = DONE - DP
      TERM = DQ ** N
      IF (M.EQ.IZERO) GO TO 60
C
      DNUM   = RN
      DDEN   = DONE
      DPDIVQ = FDDIV (DP,DQ,JIND)
      DO 50 I=1,M
        TERM = FDDIV (DNUM,DDEN,JIND) * DPDIVQ * TERM
        DNUM = DNUM - DONE
        DDEN = DDEN + DONE
  50  CONTINUE
C
  60  PDF = FDPCON (TERM)
      RETURN
C
C     ==================================================================
C
      END
*BINPPF
      SUBROUTINE BINPPF (P,PPAR,N,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BINPPF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
C              FOR THE BINOMIAL DISTRIBUTION
C              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
C              PARAMETER = PPAR,
C              AND INTEGER 'NUMBER OF BERNOULLI TRIALS'
C              PARAMETER = N.
C              THE BINOMIAL DISTRIBUTION USED
C              HEREIN HAS MEAN = N*PPAR
C              AND STANDARD DEVIATION = SQRT(N*PPAR*(1-PPAR)).
C              THIS DISTRIBUTION IS DEFINED FOR ALL
C              DISCRETE INTEGER X BETWEEN 0 (INCLUSIVELY)
C              AND N (INCLUSIVELY).
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = C(N,X) * PPAR**X * (1-PPAR)**(N-X).
C              WHERE C(N,X) IS THE COMBINATORIAL FUNCTION
C              EQUALING THE NUMBER OF COMBINATIONS OF N ITEMS
C              TAKEN X AT A TIME.
C              THE BINOMIAL DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF
C              SUCCESSES IN N BERNOULLI (0,1)
C              TRIALS WHERE THE PROBABILITY OF SUCCESS
C              IN A SINGLE TRIAL = PPAR.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (INCLUSIVELY)
C                                AND 1.0 (INCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --PPAR   = THE SINGLE PRECISION VALUE
C                                OF THE 'BERNOULLI PROBABILITY'
C                                PARAMETER FOR THE BINOMIAL
C                                DISTRIBUTION.
C                                PPAR SHOULD BE BETWEEN
C                                0.0 (EXCLUSIVELY) AND
C                                1.0 (EXCLUSIVELY).
C                     --N      = THE INTEGER VALUE
C                                OF THE 'NUMBER OF BERNOULLI TRIALS'
C                                PARAMETER.
C                                N SHOULD BE A POSITIVE INTEGER.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
C             FUNCTION VALUE PPF
C             FOR THE BINOMIAL DISTRIBUTION
C             WITH 'BERNOULLI PROBABILITY' PARAMETER = PPAR
C             AND 'NUMBER OF BERNOULLI TRIALS' PARAMETER = N.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C
C     RESTRICTIONS--PPAR SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C                 --N SHOULD BE A POSITIVE INTEGER.
C                 --P SHOULD BE BETWEEN 0.0 (INCLUSIVELY)
C                   AND 1.0 (INCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF, BINCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION AND DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--NOTE THAT EVEN THOUGH THE OUTPUT
C              FROM THIS DISCRETE DISTRIBUTION
C              PERCENT POINT FUNCTION
C              SUBROUTINE MUST NECESSARILY BE A
C              DISCRETE INTEGER VALUE,
C              THE OUTPUT VARIABLE PPF IS SINGLE
C              PRECISION IN MODE.
C              PPF HAS BEEN SPECIFIED AS SINGLE
C              PRECISION SO AS TO CONFORM WITH THE DATAPAC
C              CONVENTION THAT ALL OUTPUT VARIABLES FROM ALL
C              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
C              THIS CONVENTION IS BASED ON THE BELIEF THAT
C              1) A MIXTURE OF MODES (FLOATING POINT
C              VERSUS INTEGER) IS INCONSISTENT AND
C              AN UNNECESSARY COMPLICATION
C              IN A DATA ANALYSIS; AND
C              2) FLOATING POINT MACHINE ARITHMETIC
C              (AS OPPOSED TO INTEGER ARITHMETIC)
C              IS THE MORE NATURAL MODE FOR DOING
C              DATA ANALYSIS.
C
C     REFERENCES--JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1969, PAGES 50-86,
C                 ESPECIALLY PAGE 64, FORMULA 36.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 36-41.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 929.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 135-142.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 120-125.
C               --MOOD AND GRABLE, INTRODUCTION TO THE THEORY
C                 OF STATISTICS, EDITION 2, 1963, PAGES 64-69.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGES 264-272.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--NOVEMBER  1975.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             P, PPAR, PPF
      REAL             AMEAN, AN, PF0, P0, P1, P2
      REAL             QFN, SD, X0, X1, X2, ZPPF
      REAL             FDPCON, FSQRT
C
      DOUBLE PRECISION DPPAR
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GE.RZERO .AND. P.LE.RONE) GO TO 10
        IND = IONE
        PPF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (PPAR.GT.RZERO .AND. PPAR.LT.RONE) GO TO 20
        IND = ITWO
        PPF = RZERO
        RETURN
C
C     ..................................................................
C
  20  IF (N.GE.IONE) GO TO 30
        IND = ITHRE
        PPF = RZERO
        RETURN
C
C     ..................................................................
C
C
C     ---   START POINT   ----------------------------------------------
C
  30  AN    = N
      DPPAR = PPAR
      PPF   = RZERO
      IX0   = IZERO
      IX1   = IZERO
      IX2   = IZERO
      P0    = RZERO
      P1    = RZERO
      P2    = RZERO
C
C     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
C        1) P  =  0.0 OR 1.0
C        2) P  =  0.5 AND PPAR  =  0.5
C        3) PPF  =  0 OR N
C
      IF (P.EQ.RZERO) GO TO 40
      IF (P.EQ.RONE)  GO TO 50
      IF (P.EQ.RHALF .AND. PPAR.EQ.RHALF) GO TO 60
      PF0 = FDPCON ( (DONE-DPPAR) ** N )
      QFN = FDPCON ( DONE - (DPPAR**N) )
      IF (P.LE.PF0) GO TO 40
      IF (P.GT.QFN) GO TO 50
      GO TO 70
C
  40  PPF = RZERO
      RETURN
C
C     ..................................................................
C
  50  PPF = N
      RETURN
C
C     ..................................................................
C
  60  PPF =  IDIV (N,ITWO,JIND)
      RETURN
C
C     ..................................................................
C
C     DETERMINE AN INITIAL APPROXIMATION TO THE BINOMIAL
C        PERCENT POINT BY USE OF THE NORMAL APPROXIMATION
C        TO THE BINOMIAL.
C        (SEE JOHNSON AND KOTZ, DISCRETE DISTRIBUTIONS,
C        PAGE 64, FORMULA 36).
C
  70  AMEAN = AN * PPAR
      SD    = FSQRT (AN*PPAR*(RONE-PPAR))
      CALL NORPPF (P,ZPPF,NIND)
      X2    = AMEAN - RHALF + ZPPF * SD
      IX2   = X2
C
C     CHECK AND MODIFY (IF NECESSARY) THIS INITIAL
C        ESTIMATE OF THE PERCENT POINT
C        TO ASSURE THAT IT BE IN THE CLOSED INTERVAL 0 TO N.
C
      IF (IX2.LT.IZERO) IX2 = IZERO
      IF (IX2.GT.N)     IX2 = N
C
C     DETERMINE UPPER AND LOWER BOUNDS ON THE DESIRED
C        PERCENT POINT BY ITERATING OUT (BOTH BELOW AND ABOVE)
C        FROM THE ORIGINAL APPROXIMATION AT STEPS
C        OF 1 STANDARD DEVIATION.
C
C     THE RESULTING BOUNDS WILL BE AT MOST
C        1 STANDARD DEVIATION APART.
C
      IX0 = IZERO
      IX1 = N
      ISD = SD + RONE
      X2  = IX2
      CALL BINCDF (X2,PPAR,N,P2,NIND)
C
      IF (P2.LT.P) GO TO 80
      GO TO 110
C
  80  IX0 = IX2
  90    IX2 = IX0 + ISD
        IF (IX2.GE.IX1) GO TO 140
        X2  = IX2
        CALL BINCDF (X2,PPAR,N,P2,NIND)
        IF (P2.GE.P) GO TO 100
        IX0 = IX2
      GO TO 90
C
 100  IX1 = IX2
      GO TO 140
C
 110  IX1 = IX2
 120    IX2 = IX1 - ISD
        IF (IX2.LE.IX0) GO TO 140
        X2  = IX2
        CALL BINCDF (X2,PPAR,N,P2,NIND)
        IF (P2.LT.P) GO TO 130
        IX1 = IX2
      GO TO 120
C
 130  IX0 = IX2
C
 140  IF (IX0.EQ.IX1) GO TO 150
      GO TO 180
C
 150  IF (IX0.EQ.IZERO) GO TO 160
      IF (IX0.EQ.N)     GO TO 170
      GO TO 250
C
 160  IX1 = IX1 + IONE
      GO TO 180
C
 170  IX0 = IX0 - IONE
C
C     COMPUTE BINOMIAL PROBABILITIES FOR THE
C        DERIVED LOWER AND UPPER BOUNDS.
C
 180  X0 = IX0
      X1 = IX1
      CALL BINCDF (X0,PPAR,N,P0,NIND)
      CALL BINCDF (X1,PPAR,N,P1,NIND)
C
C     CHECK THE PROBABILITIES FOR PROPER ORDERING.
C
      IF (P0.LT.P .AND. P.LE.P1) GO TO 210
      IF (P0.EQ.P)  GO TO 190
      IF (P1.EQ.P)  GO TO 200
      GO TO 250
C
 190  PPF = IX0
      RETURN
C
C     ..................................................................
C
 200  PPF = IX1
      RETURN
C
C     ..................................................................
C
C     THE STOPPING CRITERION IS THAT THE LOWER BOUND
C        AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
C
C     CHECK TO SEE IF IX1  =  IX0 + 1;
C        IF SO, THE ITERATIONS ARE COMPLETE;
C        IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
C        CHECK PROBABILITIES, AND CONTINUE ITERATING
C        UNTIL IX1  =  IX0 + 1.
C
 210  IX0P1 = IX0 + IONE
      IF (IX1.EQ.IX0P1) GO TO 240
      IX2   = IDIV (IX0+IX1,ITWO,JIND)
      IF (IX2.EQ.IX0)   GO TO 250
      IF (IX2.EQ.IX1)   GO TO 250
      X2    = IX2
      CALL BINCDF (X2,PPAR,N,P2,NIND)
      IF (P0.LT.P2 .AND. P2.LT.P1) GO TO 220
      GO TO 250
 220  IF (P2.LE.P)  GO TO 230
      IX1 = IX2
      P1  = P2
      GO TO 210
C
 230  IX0 = IX2
      P0  = P2
      GO TO 210
C
 240  PPF = IX1
      IF (P0.EQ.P) PPF = IX0
      RETURN
C
 250  IND = IFOUR
      RETURN
C
C     ==================================================================
C
      END
*BINTJO
      DOUBLE PRECISION FUNCTION BINTJO (X,A)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BINTJO V 7.00  2/26/91. **
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION A(*), B, C, X, Z
      DOUBLE PRECISION DBEJ, FDDIV
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF, DPCG, DPCH
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA /   100.0D0 /
      DATA DPCB /    60.0D0 /
      DATA DPCC /     9.0D0 /
      DATA DPCD /   225.0D0 /
      DATA DPCE / 11025.0D0 /
      DATA DPCF /    45.0D0 /
      DATA DPCG /  1575.0D0 /
      DATA DPCH / 99225.0D0 /
C
C     ==================================================================
C
      Z = DABS (X)
      B = DZERO
      IF (Z-DZERO)  10,70,10
  10  IF (Z-DPCA) 20,20,50
  20  CALL BEJN (IZERO,A,Z)
      IF (Z-DPCB) 30,30,60
  30  B = DZERO
      DO 40 N=2,100,2
        B = B + A(N)
  40  CONTINUE
C
      B = DTWO * B
      GO TO 70
  50  A(1) = DBEJ (Z,IZERO,IONE)
      A(2) = DBEJ (Z, IONE,IONE)
  60  C = FDDIV (DONE,Z**2,IND)
      B = DONE + A(2) * (DONE - C * (DONE-C * (DPCC-C*(DPCD-C*DPCE)) ) )
      C = DONE - C*(DTHRE - C*(DPCF - C*(DPCG - DPCH*C)))
      B = B - FDDIV (A(1)*C,Z,IND)
  70  BINTJO = B
      RETURN
C
C     ==================================================================
C
      END
*BJORCK
      SUBROUTINE BJORCK (X,B,NP,A,F)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BJORCK V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT IS X,B AND NP.
C     OUTPUT IS F.
C
C     ADAPTED FROM SUBROUTINE 'BJORCK' WRITTEN BY WILLIAM J. HALL,
C     NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, BOULDER, COL. 
C     THIS ROUTINE USES THE MODIFIED GRAM-SCHMIDT ALGORITHM GIVEN
C     BY AKE BJORCK IN 'SOLVING LINEAR LEAST SQUARES PROBLEMS BY
C     LINEAR LEAST SQUARES PROBLEMS BY GRAM-SCHMIDT ORTHOGONALIZATION', 
C     'BIT' VOL. 7 (1967), PAGES 1-21.
C
C               ADAPTED TO OMNITAB BY -
C                      ROY H. WAMPLER AND M. STUART SCOTT,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JULY, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(3,*), B(*), X(*)
      REAL             F
      REAL             FDPCON
C
      DOUBLE PRECISION C(3,3), D(3), R(3), Y(4)
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C     INITIALIZE A AND FORM SUM OF SQUARES OF THE B VECTOR
C
      CALL DSUMAL (D,IZERO,DSUM)
      DO 10 I=1,NP
        A(1,I) = RONE
        A(2,I) = X(I)
        A(3,I) = X(I)*X(I)
        D(1) = DBLE ( B(I)*B(I) )
        CALL DSUMAL (D,-IONE,DSUM)
  10  CONTINUE
      CALL DSUMAL (D,IONE,DSUM)
      Y(4) =  DSUM
C
      NF   = ITHRE
      D(1) = DZERO
      Y(1) = DZERO
      DO 20 I=1,NP
        D(1) = DBLE (A(1,I)) * DBLE( A(1,I)) + D(1)
        Y(1) = DBLE (A(1,I)) * DBLE (B(I)) + Y(1)
  20  CONTINUE
C
      Y(1) = FDDIV (Y(1),D(1),IND)
      IR   = IZERO
      DO 70 K=2,NF
        DO 50 J=K,NF
          IR    = IR + IONE
          R(IR) = DZERO
          DO 30 I=1,NP
            R(IR) = DBLE(A(K-1,I)) * DBLE(A(J,I)) + R(IR)
  30      CONTINUE
          R(IR) = FDDIV (R(IR),D(K-1),IND)
          DO 40 I=1,NP
            A(J,I) = A(J,I) - A(K-1,I) * FDPCON (R(IR))
  40      CONTINUE
  50    CONTINUE
        D(K) = DZERO
        Y(K) = DZERO
        DO 60 I=1,NP
          B(I) = B(I) - A(K-1,I) * FDPCON (Y(K-1))
          Y(K) = DBLE(A(K,I)) * DBLE(B(I)) + Y(K)
          D(K) = DBLE(A(K,I)) * DBLE(A(K,I)) + D(K)
  60    CONTINUE
        Y(K) = FDDIV (Y(K),D(K),IND)
  70  CONTINUE
C
      IRS = -NF
      DO 110 K=1,NF
        IRS=IRS+NF-K+IONE
        IR=IRS
        DO 100 JJ=1,K
          J=K-JJ+IONE
          C(K,J) = Y(J)
          IF (JJ.LE.IONE) GO TO 90
          DO 80 I=2,JJ
            KK = K - I + ITWO
            C(K,J) = C(K,J) - C(K,KK)*R(IR)
            IR = IR - IONE
  80      CONTINUE
  90      IR=IR-NF+K
 100    CONTINUE
 110  CONTINUE
C
      DO 120 I=1,NF
        Y(I) = Y(I) * FDSQRT (D(I))
 120  CONTINUE
C
      F = FDDIV (Y(3)*Y(3)*DBLE(FLOAT(NP-ITHRE)),Y(4)-Y(1)*Y(1)
     1          -Y(2)*Y(2)-Y(3)*Y(3),IND)
      RETURN
C
C     ==================================================================
C
      END
*BLANK
      SUBROUTINE BLANK
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  BLANK V 7.00 10/26/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT STARTS SCAN OF KARD(KRDPOS) UNTIL IT LOCATES A BLANK.
C        KRDPOS IS POINTING TO THE BLANK OR THE END OF THE CARD.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1976.
C                   CURRENT VERSION -  OCTOBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 44 /
C
C     ==================================================================
C
      ISTART = KRDPOS
      DO 10 I=ISTART,LKARD
        IF (KARD(KRDPOS).EQ.ICA) GO TO 20
        IF (KRDPOS.EQ.KRDEND+ITWO)  GO TO 20
        KRDPOS = KRDPOS + IONE
  10  CONTINUE
  20  RETURN
C
C     ==================================================================
C
      END
*BLNKIN
      SUBROUTINE BLNKIN (LINE,LENGTH)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. BLNKIN V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR PUTTING (LENGTH) A1 BLANKS INTO VECTOR LINE
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1976.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION LINE(*)
C
      COMMON /ABCDEF/ LA(74)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER     LA*1
      CHARACTER     LINE*1
C
C     ==================================================================
C
      DO 10 I=1,LENGTH
        LINE(I) = LA(45)
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*BOXPLT
      SUBROUTINE BOXPLT (NTITLE,LT,LINE,NPLPOS,NWIDE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. BOXPLT V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR DRAWING BOX-PLOT HORIZONTALLY.
C
C     NTITLE = A1 HOLLERITH STRING TITLE
C         LT = LENGTH ON NTITLE
C       LINE = VECTOR USED FOR PLOTTING
C     NPLPOS = PLOTTING POSITIONS OF SCRAWL
C      NWIDE = WIDTH OF BOX.  NWIDE = 1,5,7,9, ...
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1976.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION LINE(*), NPLPOS(*), NTITLE(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER     LA*1
      CHARACTER*1   LINE, NTITLE
C
C     ==================================================================
C
      IBEG = LT + NPLPOS(1)
      IEND = LT + NPLPOS(5)
C
C     (1)   TOP LINE.
C
      CALL BLNKIN (LINE(1),IEND)
      JBEG = LT + NPLPOS(2)
      JEND = LT + NPLPOS(4)
      K    = IDIV (NWIDE-ITHRE,ITWO,IND)
      IF (NWIDE.LT.IFIVE) GO TO 30
      DO 10 J=JBEG,JEND
        LINE(J) = LA(39)
  10  CONTINUE
C
      WRITE (IPRINT,80) (LINE(I),I=1,JEND)
C
C     (2)   PRINT SIDES ABOVE CENTER.
C
      CALL BLNKIN (LINE(1),IEND)
      DO 20 I=1,K
        LINE(JBEG) = LA(19)
        LINE(JEND) = LA(19)
        WRITE (IPRINT,80) (LINE(J),J=1,JEND)
  20  CONTINUE
C
C     (3)   PRINT CENTER LINE.
C
C              PUT IN TITLE.
C
      CALL BLNKIN (LINE(1),IEND)
  30  DO 40 I=1,LT
        LINE(I) = NTITLE(I)
  40  CONTINUE
C
      DO 50 I=IBEG,IEND
        LINE(I) = LA(39)
  50  CONTINUE
C
      M = JEND - JBEG + IONE
      CALL BLNKIN (LINE(JBEG),M)
      LINE(JBEG) = LA(18)
      LINE(IBEG) = LA(41)
      L = LT + NPLPOS(3)
      LINE(JEND) = LA(18)
      LINE(IEND) = LA(41)
      LINE(   L) = LA(23)
      WRITE (IPRINT,80) (LINE(I),I=1,IEND)
      IF (NWIDE.LT.IFIVE) RETURN
C
C     (4)   PRINT SIDES BELOW CENTER LINE.
C
      CALL BLNKIN (LINE(1),IEND)
      DO 60 I=1,K
        LINE(JBEG) = LA(19)
        LINE(JEND) = LA(19)
        WRITE (IPRINT,80) (LINE(J),J=1,JEND)
  60  CONTINUE
C
C     (5)   PRINT BOTTOM LINE.
C
      CALL BLNKIN (LINE(1),IEND)
      DO 70 J=JBEG,JEND
        LINE(J) = LA(39)
  70  CONTINUE
C
      WRITE (IPRINT,80) (LINE(I),I=1,JEND)
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
  80  FORMAT (1X,119A1)
C
C     ==================================================================
C
      END
*BRCOL
      SUBROUTINE BRCOL (V1,V2,RMLT,M1,IOUT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  BRCOL V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     BARRODALE-ROBERTS PROGRAM UNIT CALLED BY BRCOL.
C
C               ADAPTED TO OMNITAB BY -
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
      REAL             V1(*), V2(*)
      REAL             RMLT
      REAL             FDPCON
C
C     ==================================================================
C
      DO 10 I=1,M1
        IF (I.EQ.IOUT) GO TO 10
        V1(I) = FDPCON ( DBLE (V1(I)) - DBLE (V2(I)) * DBLE (RMLT) )
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*BRL1
      SUBROUTINE BRL1 (M,N,M2,N2,A,B,TOLER,X,E,AIS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  BRL1 V 7.00   5/28/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE USES A MODIFICATION OF THE SIMPLEX METHOD
C        OF LINEAR PROGRAMMING TO CALCULATE AN L1 SOLUTION TO AN
C        OVER-DETERMINED SYSTEM OF LINEAR EQUATIONS.
C
C     DESCRIPTION OF PARAMETERS ...
C
C     M      NUMBER OF EQUATIONS.
C     N      NUMBER OF UNKNOWNS (M.GE.N).
C     M2     SET EQUAL TO M+2 FOR ADJUSTABLE DIMENSIONS.
C     N2     SET EQUAL TO N+2 FOR ADJUSTABLE DIMENSIONS.
C     A      TWO DIMENSIONAL REAL ARRAY OF SIZE (M2,N2).
C            ON ENTRY, THE COEFFICIENTS OF THE MATRIX MUST BE
C            STORED IN THE FIRST M ROWS AND N COLUMNS OF A.
C            THESE VALUES ARE DESTROYED BY THE SUBROUTINE.
C     B      ONE DIMENSIONAL REAL ARRAY OF SIZE M. ON ENTRY, B
C            MUST CONTAIN THE RIGHT HAND SIDE OF THE EQUATIONS.
C            THESE VALUES ARE DESTROYED BY THE SUBROUTINE.
C     TOLER  A SMALL POSITIVE TOLERANCE. EMPIRICAL EVIDENCE
C            SUGGESTS TOLER=10**(-D*2/3) WHERE D REPRESENTS
C            THE NUMBER OF DECIMAL DIGITS OF ACCURACY AVALABLE
C            (SEE DESCRIPTION).
C     X      ONE DIMENSIONAL REAL ARRAY OF SIZE N. ON EXIT, THIS
C            ARRAY CONTAINS A SOLUTION TO THE L1 PROBLEM.
C     E      ONE DIMENSIONAL REAL ARRAY OF SIZE M. ON EXIT, THIS
C            ARRAY CONTAINS THE RESIDUALS IN THE EQUATIONS.
C     AIS    ARRAY OF SIZE M USED FOR WORKSPACE.
C
C     ON EXIT FROM THE SUBROUTINE, THE ARRAY A CONTAINS THE
C        FOLLOWING INFORMATION ...
C
C     A(M+1,N+1)  THE MINIMUM SUM OF THE ABSOLUTE VALUES OF
C                 THE RESIDUALS.
C     A(M+1,N+2)  THE RANK OF THE MATRIX OF COEFFICIENTS.
C     A(M+2,N+1)  EXIT CODE WITH VALUES.
C                 0 - OPTIMAL SOLUTION WHICH IS PROBABLY NON-
C                     UNIQUE (SEE DESCRIPTION).
C                 1 - UNIQUE OPTIMAL SOLUTION.
C                 2 - CALCULATIONS TERMINATED PREMATURELY DUE TO
C                     ROUNDING ERRORS.
C     A(M+2,N+2)  NUMBER OF SIMPLEX ITERATIONS PERFORMED.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY
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
      DIMENSION AIS(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*), B(M), E(M), X(N)
      REAL             TOLER
      REAL             CMULT, D, PIVOT, RMIN, RMAX, SUM
      REAL             FDIV
C
      LOGICAL STAGE, TEST
C
C     ==================================================================
C
C     INITIALIZATION.
C
      M1 = M + IONE
      N1 = N + IONE
      M2J = M2
      DO 10 J=1,N
        A(M2J) = J
        M2J    = M2J + M2
        X(J) = RZERO
  10  CONTINUE
C
      IN2 = (N2 - IONE) * M2 + IONE
      IN1 = (N1 - IONE) * M2 + IONE
      DO 40 I=1,M
        A(IN2) = N + I
        A(IN1) = B(I)
        IN2     = IN2 + IONE
        IN1     = IN1 + IONE
        IF (B(I).GE.RZERO) GO TO 30
        IJ = I
        DO 20 J=1,N2
          A(IJ) = - A(IJ)
          IJ    = IJ + M2
  20    CONTINUE
  30    E(I) = RZERO
  40  CONTINUE
C
C     COMPUTE THE MARGINAL COSTS.
C
C     PROGRAM UNIT SUMMAL USED INSTEAD OF SUMMING IN DOUBLE PRECISION.
      DO 50 I=1,N1
        I1 = (I - IONE) * M2 + IONE
        CALL SUMMAL (A(I1),M,SUM)
        M1I     = (I - IONE) * M2 + M1
        A(M1I) = SUM
        IF (M.EQ.IONE) A(M1I) = A(I1)
  50  CONTINUE
C
C     STAGE I.
C
C     DETERMINE THE VECTOR TO ENTER THE BASIS.
C
      STAGE = .TRUE.
      KOUNT = IZERO
      KR    = IONE
      KL    = IONE
      IN    = IONE
  60  RMAX  = - RONE
      DO 70 J=KR,N
        M2J = J * M2  
        IF (ABS(A(M2J)).GT.FLOAT(N)) GO TO 70
        M1J = (J -IONE) * M2 + M1
        D = ABS (A(M1J))
        IF (D.LE.RMAX) GO TO 70
        RMAX = D
        IN = J
  70  CONTINUE
      M1IN = (IN - IONE) * M2 + M1
      IF (A(M1IN).GE.RZERO) GO TO 90
      IIN = (IN -IONE) * M2 + IONE
      DO 80 I=1,M2
        A(IIN) = - A(IIN)
        IIN = IIN + IONE
  80  CONTINUE
C
C     DETERMINE THE VECTOR TO LEAVE THE BASIS.
C
  90  K    = IZERO
      TEST = .FALSE.
      IIN = (IN - IONE) * M2 + KL
      IN1 = (N1 - IONE) * M2 
      DO 100 I=KL,M
        D = A(IIN)
        IIN = IIN + IONE
        IF (D.LE.TOLER) GO TO 100
        IN11 = IN1 + I
        K = K + IONE
        B(K) = FDIV (A(IN11),D,IND)
        AIS(K) = I
        TEST = .TRUE.
 100  CONTINUE
 110  IF (K.GT.IZERO) GO TO 120
      TEST = .FALSE.
      GO TO 140
C
 120  RMIN = RPIFY
      DO 130 I=1,K
        IF (B(I).GE.RMIN) GO TO 130
        J    = I
        RMIN = B(I)
        IOUT = AIS(I) + .05
 130  CONTINUE
C
      B(J) = B(K)
      AIS(J) = AIS(K)
      K = K - IONE
C
C     CHECK FOR LINEAR DEPENDENCE IN STAGE I.
C
 140  IF (TEST .OR. .NOT.STAGE) GO TO 160
      IKR = (KR - IONE) * M2 + IONE
      IIN = (IN - IONE) * M2 + IONE
      DO 150 I=1,M2
        D = A(IKR)
        A(IKR) = A(IIN)
        A(IIN) = D
        IKR = IKR + IONE
        IIN = IIN + IONE
 150  CONTINUE
C
      KR = KR + IONE
      GO TO 240
C
 160  IF (TEST) GO TO 170
      M2N1 = N1 * M2
      A(M2N1) = RTWO
      GO TO 330
C
 170  IOUTIN = (IN -IONE) * M2 + IOUT
      PIVOT = A(IOUTIN)
C
C     RELATIVE TOLERANCE IS USED INSTEAD OF ABSOLUTE TOLERANCE.
C
      M1IN = (IN - IONE) * M2 + M1
      IF (A(M1IN)-PIVOT-PIVOT.LE.TOLER) GO TO 190
      IOUTJ = (KR - IONE) * M2 + IOUT
      M1J = (KR - IONE) * M2 + M1
      DO 180 J=KR,N1
        D = A(IOUTJ)
        A(M1J) = A(M1J) - D - D
        M1J = M1J + M2
        A(IOUTJ) = - D
        IOUTJ = IOUTJ + M2
 180  CONTINUE
      IOUTN2 = (N2 - IONE) * M2 + IOUT
      A(IOUTN2) = - A(IOUTN2)
      GO TO 110
C
C     PIVOT ON A(IOUT,IN).
C
 190  IOUTP = (KR - IONE) * M2 + IOUT
      IOUTJ = IOUTP
      DO 200 J=KR,N1
        IF (J.EQ.IN) GO TO 195
        A(IOUTJ) = FDIV (A(IOUTJ),PIVOT,IND)
 195    IOUTJ = IOUTJ + M2
 200  CONTINUE
C
      IOUTJ = IOUTP 
      IN1 = (IN - IONE) * M2 + IONE
      DO 210 J=KR,N1
        IF(J.EQ.IN) GO TO 205
        CMULT = A(IOUTJ)
        J1 = (J - IONE) * M2 + IONE
        CALL BRCOL (A(J1),A(IN1),CMULT,M1,IOUT)
 205    IOUTJ = IOUTJ + M2
 210  CONTINUE
C
      INSPV = (IN - IONE) * M2
      DO 220 I=1,M1
        IIN = INSPV + I
        IF (I.EQ.IOUT) GO TO 220
        A(IIN) = - FDIV (A(IIN),PIVOT,IND)
 220  CONTINUE
C
      IOUTN2 = (N2 - IONE) * M2 + IOUT
      M2IN   = IN * M2
      IOUTIN = INSPV + IOUT
      A(IOUTIN) = FDIV (RONE,PIVOT,IND)
      D = A(IOUTN2)
      A(IOUTN2) = A(M2IN)
      A(M2IN) = D
      KOUNT = KOUNT + IONE
      IF (.NOT.STAGE) GO TO 250
C
C     INTERCHANGE ROWS IN STAGE I.
C
      IOUTJ = IOUTP 
      KL = KL + IONE
      DO 230 J=KR,N2
        KOUNTJ = (J - IONE) * M2 + KOUNT
        D = A(IOUTJ)
        A(IOUTJ) = A(KOUNTJ)
        A(KOUNTJ) = D
        IOUTJ = IOUTJ + M2
 230  CONTINUE
C
 240  IF (KOUNT+KR.NE.N1) GO TO 60
C
C     STAGE II.
C
      STAGE = .FALSE.
C
C     DETERMINE THE VECTOR TO ENTER THE BASIS.
C
 250  RMAX = - RPIFY
      M1J = (KR - IONE) * M2 + M1
      DO 270 J=KR,N
        D = A(M1J)
        M1J = M1J + M2
        IF (D.GE.RZERO) GO TO 260
        IF (D.GT.(-RTWO)) GO TO 270
        D = -D - RTWO
 260    IF (D.LE.RMAX) GO TO 270
        RMAX = D
        IN = J
 270  CONTINUE
C
      IF (RMAX.LE.TOLER) GO TO 290
      M1IN = (IN - IONE) * M2 +M1
      IF (A(M1IN).GT.RZERO) GO TO 90
      IIN = (IN - IONE) * M2
      DO 280 I=1,M2
        IIN = IIN + IONE
        A(IIN) = - A(IIN)
 280  CONTINUE
C
      A(M1IN) = A(M1IN) - RTWO
      GO TO 90
C
C      PREPARE OUTPUT.
C
 290  L = KL - IONE
      IN1 = (N1 - IONE) * M2 
      IJP = (KR - IONE) * M2 
      DO 310 I=1,L
        IN1 = IN1 + IONE
        IJ = IJP + I
        IF (A(IN1).GE.RZERO) GO TO 310
        DO 300 J=KR,N2
        A(IJ) = - A(IJ)
        IJ = IJ + M2
 300    CONTINUE
 310  CONTINUE
C
      M2N1 = N1 * M2
      A(M2N1) = RZERO
      IF (KR.NE.IONE) GO TO 330
      M1J = M1
      DO 320 J=1,N
        D = ABS (A(M1J))
        M1J = M1J + M2
        IF (D.LE.TOLER .OR. RTWO-D.LE.TOLER) GO TO 330
 320  CONTINUE
C
      A(M2N1) = RONE
 330  IN1 = (N1 - IONE) * M2 
      IN2 = (N2 - IONE) * M2
      DO 360 I=1,M
        IN11 = IN1 + I
        IN22 = IN2 + I
        K = A(IN22)
        D = A(IN11)
        IF (K.GT.IZERO) GO TO 340
        K = - K
        D = - D
 340    IF (I.GE.KL) GO TO 350
        X(K) = D
        GO TO 360
C
 350    K = K - N
        E(K) = D
 360  CONTINUE
C
      M2N2 = M2 * N2
      M1N2 = (N2 - IONE) * M2 + M1
      A(M2N2) = KOUNT
      A(M1N2) = N1 - KR
C
C     PROGRAM UNIT SUMMAL USED INSTEAD OF SUMMING IN DOUBLE PRECISION.
C
      K = M - KL + IONE
      KLN1 = (N1 - IONE) * M2 + KL
      CALL SUMMAL (A(KLN1),K,SUM)
      M1N1 = (N1 - IONE) * M2 + M1
      A(M1N1) = SUM
      IF (K.EQ.IONE) A(M1N1) = A(KLN1)
      RETURN
C
C     ==================================================================
C
      END
*CALERR
      SUBROUTINE CALERR (I)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CALERR V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS SUBROUTINE PRINTS PROPER ERROR MESSAGES FOR CALCOMP COMMANDS.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      II = I - 300
      GO TO (101,102,103,104,105),II
C
 101  WRITE (IPRINT,301)
      GO TO 110
C
 102  WRITE (IPRINT,302)
      GO TO 110
C
 103  WRITE (IPRINT,303)
      GO TO 110
C
 104  WRITE (IPRINT,304)
      GO TO 110
C
 105  WRITE (IPRINT,305)
      GO TO 110
C
 110  IF (IPRINT.EQ.NPRNT) RETURN
C
C     ..................................................................
C
      GO TO (201,202,203,204,205),II
C
 201  WRITE (NPRNT,401)
      RETURN
C
 202  WRITE (NPRNT,402)
      RETURN
C
 203  WRITE (NPRNT,403)
      RETURN
C
 204  WRITE (NPRNT,404)
      RETURN
C
 205  WRITE (NPRNT,405)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 301  FORMAT(5X,47H* CALCOMP PLOTTING SPEED SPECIFIED NOT CORRECT,,18X/6
     1X,32H ZIP CODE OR HIGH SPEED ASSUMED.,32X)
 302  FORMAT (5X,45H* CALCOMP PAPER WIDTH IS NOT 12 OR 30 INCHES,,20X/6X
     1,26H OR Y HEIGHT IS TOO LARGE.,38X)
 303  FORMAT (5X,50H* EITHER SYMBOL OR JOINING SPECIFIED IS INCORRECT.,1
     15X/6X,32H SYMBOL . OR NO JOINING IS USED.,32X)
 304  FORMAT (5X,59H* TITLEX OR TITLEY IS TOO LONG FOR GRAPH. TITLE IS O
     1MITTED.,6X)
 305  FORMAT (5X,51H* HEIGHT OR WIDTH TOO LARGE. DEFAULT VALUE(S) USED.,
     114X)
C
C     ..................................................................
C
 401  FORMAT (10X,48HCALCOMP PLOTTING SPEED SPECIFIED IS NOT CORRECT,,12
     1X/12X,34HZIP CODE OR HIGH SPEED IS ASSUMED.,24X)
 402  FORMAT (10X,44H CALCOMP PAPER WIDTH IS NOT 12 OR 30 INCHES,,16X/12
     1X,25HOR Y HEIGHT IS TOO LARGE.,33X)
 403  FORMAT (10X,48HEITHER SYMBOL OR JOINING SPECIFIED IS INCORRECT.,12
     1X/12X,31HSYMBOL . OR NO JOINING IS USED.,27X)
 404  FORMAT (10X,57HTITLEX OR TITLEY IS TOO LONG FOR GRAPH. TITLE IS OM
     1ITTED.,3X)
 405  FORMAT (5X,49HHEIGHT OR WIDTH TOO LARGE. DEFAULT VALUE(S) USED.,
     116X)
C
C     ==================================================================
C
      END
*CALINT
      SUBROUTINE CALINT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CALINT 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALL PROPER CALCOMP SUBROUTINE FOR OVERLAY.
C
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JULY, 1972.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
C
      REAL             XLIMIT(4), YLIMIT(4)
C
C     ==================================================================
C
      IF (ITEKSW .NE. -IONE) ITEKSW = IZERO
      CALL CALCOM (XLIMIT,YLIMIT,LCR)
      IF (LCR.EQ.IZERO) RETURN
      IF (NERROR.NE.IZERO) RETURN
      CALL CALPLT (XLIMIT,YLIMIT,LCR)
      WRITE (ISCRT,30)
      WRITE (ISCRT,20) IFG
      WRITE (ISCRT,30)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  20  FORMAT (10X,20(1H.),5HGRAPH,I5,12H WAS PLOTTED,18(1H.),14X)
  30  FORMAT (84X)
C
C     ==================================================================
C
      END
*CALTIK
      SUBROUTINE CALTIK (X,Y,XWDTH,YHGT,BUF,NBUF,ITLX,ITLY,ITLO,IFG,NBF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CALTIK V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DO THE FOLLOWING FOR CALCOMP INSTRUCTIONS ...
C
C        (1)  DRAW ALL FOUR AXES AND PUT IN TIC MARKS
C        (2)  PRINTS VALUES AT TIC MARKS
C        (3)  PRINT TITLEX, TITLEY GRAPH NO AND IF POSSIBLE OMNITAB CARD
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
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
C
      DIMENSION IGR(2), ITLO(*), ITLX(*), ITLY(*), NBF(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             X(*), Y(*), BUF(*)
      REAL             XWDTH, YHGT
      REAL             ANUM, HXHT, HYHT, OMHT, OMTLE, SPACE, XP, XTLE
      REAL             YP, YTLE
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE
      REAL             SPCF, SPCG, SPCH, SPCI, SPCJ
C
C     ..................................................................
C
      CHARACTER IBLANK*1, IGR*3
      CHARACTER ITLO*1, ITLX*1, ITLY*1, NBF*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IBLANK /  ' '   /
C
      DATA IGR(1) / 'GRA'  /
      DATA IGR(2) / 'PH '  /
C
      DATA SPCA /   0.07 /
      DATA SPCB /   0.10 /
      DATA SPCC /   0.14 /
      DATA SPCD /   0.21 /
      DATA SPCE /   0.30 /
      DATA SPCF /   0.40 /
      DATA SPCG /   0.80 /
      DATA SPCH /  80.0  /
      DATA SPCI /  90.0  /
      DATA SPCJ / 999.0  /
C
C     ==================================================================
C
      X(3) = X(1)
      X(4) = FDIV (X(2)-X(1),XWDTH,IND)
      Y(3) = Y(1)
      Y(4) = FDIV (Y(2)-Y(1),YHGT,IND)
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,RZERO,ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,RZERO,ITHRE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,-SPCB,ITWO)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,-SPCB,ITWO)
      N = NBUF
      BUF(N+1) = X(3)
      IHGT = XWDTH
      NA = N + ITWO
      DO 10 I=1,IHGT
        BUF(NA) = BUF(NA-1) + X(4)
        NA = NA + IONE
  10  CONTINUE
C
      NRD  = ITHRE
      ANUM = AMAX1 (ABS(X(3)),ABS(BUF(NA-1)))
      NUM  = ANUM
      DO 20 I=1,3
        IF (NUM.LE.ITEN**NRD) GO TO 30
        NRD = NRD + IONE
  20  CONTINUE
C
  30  CALL RFORMT (0,NRD,BUF(N+1),X(1),IHGT+1,8,NW,ND,NBF(NA),IRF)
      NC = NA
      IIHGT = IHGT + IONE
      DO 40 I=1,IIHGT
        NB = N + I
        CALL RFORMT (1,NRD,X,BUF(NB),10-NW,1,NW,ND,NBF(NA),IRF)
        NA = NA + ITEN
  40  CONTINUE
C
      XP = RZERO
      DO 50 I=1,IHGT
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XP,RZERO,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XP,RZERO,ITHRE)
        XP = XP + RONE
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XP,RZERO,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XP,RZERO,ITWO)
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XP,-SPCB,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XP,-SPCB,ITWO)
  50  CONTINUE
C
      IF (ITEKSW.EQ.IZERO)
     1CALL SYMBOL (-RHALF,-SPCE,SPCB,NBF(NC),RZERO,IONE)
      IF (ITEKSW.NE.IZERO)
     1CALL SYMBLT (-RHALF,-SPCE,SPCB,NBF(NC),RZERO,IONE)
      NAA = NA - IONE
      NCC = NC + IONE
C
C     LABEL TIC MARKS ON X-AXIS.
C
      DO 60 I=NCC,NAA
        IF (ITEKSW.EQ.IZERO)
     1  CALL SYMBOL (SPCJ,SPCJ,SPCB,NBF(I),RZERO,IONE)
        IF (ITEKSW.NE.IZERO)
     1  CALL SYMBLT (SPCJ,SPCJ,SPCB,NBF(I),RZERO,IONE)
  60  CONTINUE
C
C     DETERMINE IF TITLES ARE TO BE PRINTED AND IF SO NOW MANY
C        CHARACTERS.
C
      KTLE = IZERO
      DO 70 I=1,60
        IF (ITLX(I).NE.IBLANK) GO TO 80
  70  CONTINUE
      GO TO 160
C
  80  KXSTRT = I
      I = 61
  90  I = I - IONE
      IF (ITLX(I).EQ.IBLANK .AND. I.GT.KXSTRT) GO TO 90
      IF (ITLX(I).EQ.IBLANK .AND. I.LE.KXSTRT) GO TO 160
      KXSTP = I
C
C     TOTAL NUMBER OF CHARACTERS IN X TITLE.
C
      KXCHAR = KXSTP - KXSTRT + IONE
      M = IONE
      HXHT = SPCD
 100  SPACE = HXHT * FLOAT (KXCHAR)
      IF (SPACE.LE.XWDTH+RONE) GO TO 140
C
      GO TO (110,120,130), M
C
 110  M = ITWO
      HXHT = SPCC
      GO TO 100
C
 120  M = ITHRE
      HXHT = SPCB
      GO TO 100
C
C     INFORMATIVE DIAGNOSTIC, IF TITLE CAN NOT BE PRINTED.
C
 130  CALL ERROR (304)
      KTLE = IONE
      GO TO 160
C
C     PRINT X TITLE.
C
 140  XTLE = FDIV (XWDTH-SPACE,RTWO,IND)
      IF (ITEKSW.EQ.IZERO)
     1CALL SYMBOL (XTLE,-.45-HXHT,HXHT,ITLX(KXSTRT),RZERO,IONE)
      IF (ITEKSW.NE.IZERO)
     1CALL SYMBLT (XTLE,-.45-HXHT,HXHT,ITLX(KXSTRT),RZERO,IONE)
      KXSTT = KXSTRT + IONE
      DO 150 I=KXSTT,KXSTP
        IF (ITEKSW.EQ.IZERO)
     1  CALL SYMBOL (SPCJ,SPCJ,HXHT,ITLX(I),RZERO,IONE)
        IF (ITEKSW.NE.IZERO)
     1  CALL SYMBLT (SPCJ,SPCJ,HXHT,ITLX(I),RZERO,IONE)
 150  CONTINUE
C
C     DRAW RIGHT HAND AXIS AND TIC MARKS.
C
 160  IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH,RZERO,ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH,RZERO,ITHRE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH+SPCB,RZERO,ITWO)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH+SPCB,RZERO,ITWO)
      IYHGT = YHGT
      YP = IZERO
      DO 170 I=1,IYHGT
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH,YP,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH,YP,ITHRE)
        YP = YP + RONE
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH,YP,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH,YP,ITWO)
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH+SPCB,YP,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH+SPCB,YP,ITWO)
 170  CONTINUE
C
C     DRAW TOP AXIS.
C
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH,YHGT,ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH,YHGT,ITHRE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH,YHGT+SPCB,ITWO)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH,YHGT+SPCB,ITWO)
      XP = RZERO
      DO 180 I=1,IHGT
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH-XP,YHGT,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH-XP,YHGT,ITHRE)
        XP = XP + RONE
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH-XP,YHGT,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH-XP,YHGT,ITWO)
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XWDTH-XP,YHGT+SPCB,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XWDTH-XP,YHGT+SPCB,ITWO)
 180  CONTINUE
C
C     PRINT OMNITAB CARD AND GRAPH NUMBER.
C
      IF (ISBFT.NE.IZERO) GO TO 220
      OMHT = SPCB
      IF (XWDTH+RONE.GE.OMHT*SPCH) GO TO 190
      OMHT = SPCA
      IF (XWDTH+RONE.LT.OMHT*SPCH) GO TO 210
 190  OMTLE = FDIV (XWDTH-OMHT*SPCH,RTWO,IND)
      IF (ITEKSW.EQ.IZERO)
     1CALL SYMBOL (OMTLE,YHGT+SPCG,OMHT,ITLO(1),RZERO,IONE)
      IF (ITEKSW.NE.IZERO)
     1CALL SYMBLT (OMTLE,YHGT+SPCG,OMHT,ITLO(1),RZERO,IONE)
      DO 200 I=2,80
      IF (ITEKSW.EQ.IZERO)
     1  CALL SYMBOL (SPCJ,SPCJ,OMHT,ITLO(I),RZERO,IONE)
      IF (ITEKSW.NE.IZERO)
     1  CALL SYMBLT (SPCJ,SPCJ,OMHT,ITLO(I),RZERO,IONE)
 200  CONTINUE
C
 210  IF (ITEKSW.EQ.IZERO) THEN
        CALL SYMBOL (FDIV(XWDTH,RTWO,IND)-RONE,YHGT+SPCF,SPCD,IGR(1),
     1             RZERO,ITHRE)
        CALL SYMBOL (SPCJ,SPCJ,SPCD,IGR(2),RZERO,ITHRE)
        CALL NUMBER (SPCJ,SPCJ,SPCD,FLOAT(IFG),RZERO,-IONE)
      END IF
C
C     PLOT LEFT AXIS OR Y-AXIS.
C
 220  IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,YHGT,ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,YHGT,ITHRE)
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (-SPCB,YHGT,ITWO)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (-SPCB,YHGT,ITWO)
      YP = RZERO
      DO 230 I=1,IYHGT
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,YHGT-YP,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,YHGT-YP,ITHRE)
        YP = YP + RONE
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,YHGT-YP,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,YHGT-YP,ITWO)
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (-SPCB,YHGT-YP,ITWO)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (-SPCB,YHGT-YP,ITWO)
 230  CONTINUE
C
C     GET INCREMENT VALUES READY FOR Y-AXIS.
C
      BUF(N+1) = Y(3)
      NA = N + ITWO
      DO 240 I=1,IYHGT
        BUF(NA) = BUF(NA-1) + Y(4)
        NA = NA + IONE
 240  CONTINUE
C
      NRD  = ITHRE
      ANUM = AMAX1 (ABS(Y(3)),ABS(BUF(NA-1)))
      NUM  = ANUM
      DO 250 I=1,3
        IF (NUM.LE.ITEN**NRD) GO TO 260
        NRD = NRD + IONE
 250  CONTINUE
C
 260  CALL RFORMT (0,NRD,BUF(N+1),X(1),IYHGT+1,8,NW,ND,NBF(NA),IRF)
      IIYHGT = IYHGT + IONE
      NC = NA
      DO 270 I=1,IIYHGT
        NB = N + I
        CALL RFORMT (1,NRD,X,BUF(NB),10-NW,1,NW,ND,NBF(NA),IRF)
        NA = NA + ITEN
 270  CONTINUE
C
      IF (ITEKSW.EQ.IZERO)
     1CALL SYMBOL (-SPCE,-RHALF,SPCB,NBF(NC),SPCI,IONE)
      IF (ITEKSW.NE.IZERO)
     1CALL SYMBLT (-SPCE,-RHALF,SPCB,NBF(NC),SPCI,IONE)
      NCC = NC + IONE
      NAA = NA - IONE
      DO 280 I=NCC,NAA
        IF (ITEKSW.EQ.IZERO)
     1  CALL SYMBOL (SPCJ,SPCJ,SPCB,NBF(I),SPCI,IONE)
        IF (ITEKSW.NE.IZERO)
     1  CALL SYMBLT (SPCJ,SPCJ,SPCB,NBF(I),SPCI,IONE)
 280  CONTINUE
C
C     DETERMINE IF THERE IS A Y-TITLE AND IF SO HOW MANY CHARACTERS.
C
      DO 290 I=1,60
        IF (ITLY(I).NE.IBLANK) GO TO 300
 290  CONTINUE
      RETURN
C
C     ..................................................................
C
 300  KYSTRT = I
      I = 61
 310  I = I - IONE
      IF (ITLY(I).EQ.IBLANK .AND. I.LE.KYSTRT) RETURN
      IF (ITLY(I).EQ.IBLANK .AND. I.GT.KYSTRT) GO TO 310
      KYSTP = I
C
C     TOTAL NUMBER OF CHARACTERS IN Y-TITLE.
C
      KYCHAR = KYSTP - KYSTRT + IONE
      M = IONE
      HYHT = SPCD
 320  SPACE = HYHT * FLOAT (KYCHAR)
      IF (SPACE.LE.YHGT+RONE) GO TO 360
C
      GO TO (330,340,350), M
C
 330  M = ITWO
      HYHT = SPCC
      GO TO 320
C
 340  M = ITHRE
      HYHT = SPCB
      GO TO 320
C
 350  IF (KTLE.EQ.IZERO) CALL ERROR (304)
      RETURN
C
C     ..................................................................
C
C     PRINT Y TITLE.
C
 360  YTLE = FDIV (YHGT-SPACE,RTWO,IND)
      IF (ITEKSW.EQ.IZERO)
     1CALL SYMBOL (-RHALF-HYHT,YTLE,HYHT,ITLY(KYSTRT),SPCI,IONE)
      IF (ITEKSW.NE.IZERO)
     1CALL SYMBLT (-RHALF-HYHT,YTLE,HYHT,ITLY(KYSTRT),SPCI,IONE)
      KYSTT = KYSTRT + IONE
      DO 370 I=KYSTT,KYSTP
        IF (ITEKSW.EQ.IZERO)
     1  CALL SYMBOL (SPCJ,SPCJ,HYHT,ITLY(I),SPCI,IONE)
        IF (ITEKSW.NE.IZERO)
     1  CALL SYMBLT (SPCJ,SPCJ,HYHT,ITLY(I),SPCI,IONE)
 370  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CBEI
      SUBROUTINE CBEI (R,S,A,B,C,D)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   CBEI V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C         COMPUTE I0(Z) AND I1(Z) FOR COMPLEX ARGUMENT  R*E(IS)=Z
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION A, B, C, D, R, S 
      DOUBLE PRECISION AA, E, F, G, H, P, Q, T, U, V, W, X, Y, Z
      DOUBLE PRECISION FDCOS, FDDIV, FDEXP, FDSIN, FDSQRT
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA /  3.0D0         /
      DATA DPCB /  8.0D0         /
      DATA DPCC /  6.283185307D0 /
      DATA DPCD /  0.5D-10       /
      DATA DPCE / 15.5D0         /
C
C     ==================================================================
C
      E = FDCOS (S) 
      F = FDSIN (S) 
      IF (R.GT.DPCE) GO TO 30 
      P = DONE-DTWO*F**2
      AA = P
      Q = DTWO*E*F
      W = Q
      A = DONE
      B = DZERO
      C = DONE
      U = DZERO
      G = DONE
      T = DTWO
      X = FDDIV (R,DTWO,IND)**2
      V = X
      Y = X
      DO 10 N=1,60
        Z = FDDIV (DONE,G**2,IND)
        H = FDDIV (DONE,G*T,IND)
        A = A+X*Z*P 
        B = B+X*Z*Q 
        C = C+V*H*P 
        U = U+V*H*Q 
        X = X*Y*Z
        IF (X.LT.DPCD) GO TO 20
        V = V*Y*H
        Z = P
        P = Z*AA-Q*W
        Q = Q*AA+Z*W
        G = G+DONE
        T = T+DONE
  10  CONTINUE
C
  20  D = FDDIV (R*(C*F+U*E),DTWO,IND)
      C = FDDIV (R*(C*E-U*F),DTWO,IND)
      RETURN
C
C     ..................................................................
C
  30  Z = FDDIV (FDEXP(R*E),FDSQRT(DPCC*R),IND)
      X = FDDIV (S,DTWO,IND) -R * F
      Y = Z * FDCOS (X)
      Z = Z * FDSIN (X)
      W = -DONE
      G = DONE
      H = DPCA
      P = E
      Q = F
      T = DONE
      U = DZERO
      V = DONE
      X = DZERO
      A = DONE
      B = FDDIV (DONE,DPCB*R,IND)
      C = B
      D = B
      DO 40 N=1,20
        AA = FDDIV (B*G**2,A,IND)
        T = T+AA*P
        U = U+AA*Q
        AA = FDDIV (C*W*H,A,IND)
        V = V+AA*P
        X = X+AA*Q
        B = FDDIV (B*D*G**2,A,IND)
        IF (B.LT.DPCD) GO TO 50
        C = FDDIV (C*D*W*H,A,IND)
        W = W+DTWO
        G = G+DTWO
        H = H+DTWO
        A = A+DONE
        AA = P*E-Q*F
        Q = F*P+E*Q 
        P = AA
  40  CONTINUE
C
  50  A = Y*T-Z*U
      B = -(Y*U+T*Z)
      C = Y*V-Z*X
      D = -(Y*X+Z*V)
      RETURN
C
C     ==================================================================
C
      END 
*CHKCOL
      SUBROUTINE CHKCOL
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CHKCOL V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CHECK THAT ALL (NARGS) ARGUMENTS ARE LEGAL COLUMN NUMBERS AND
C         CONVERT THEM TO THEIR BEGINNING ADDRESSES IN IARGS.
C
C      IF NARGS IS LESS THAN 1,      CALL ERROR (10),
C      IF ILLEGAL COLUMN NUMBER,     CALL ERROR (11), (BY ADRESS)
C      IF IMPROPER TYPE OF ARGUMENT, CALL ERROR (20),
C         OTHERWISE EVERYTHING IS OK.
C
C               MODIFIED BY - 
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
C
C     ==================================================================
C
      IF (NARGS.GT.IZERO) GO TO 10
        CALL ERROR (10)
        RETURN
C
C     ..................................................................
C
  10  DO 20 I=1,NARGS
        II = I
        CALL ADRESS (II,K)
        IARGS(I) = K
        IF (IARGS(I).LT.IZERO) CALL ERROR (20)
  20  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END 
*CHSCDF
      SUBROUTINE CHSCDF (X,NU,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82. CHSCDF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGE 176,
C                 FORMULA 28, AND PAGE 180, FORMULA 33.1.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 50-55.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 122-131.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --MAY       1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --OCTOBER   1976.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, CDF
      REAL             AMEAN, ANU, CDFN, DANU, SD, SPCHI, U, Z
      REAL             SPCA, SPCB
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION TERM(5)
      DOUBLE PRECISION DX, CHI, SUM, AI, DCDFN
      DOUBLE PRECISION DNU
      DOUBLE PRECISION DFACT, DPOWER
      DOUBLE PRECISION DW
      DOUBLE PRECISION D1, D2, D3
      DOUBLE PRECISION B11, B21
      DOUBLE PRECISION B31, B32, B41, B42, B43
      DOUBLE PRECISION DPCA
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG, FDSQRT
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NUCUT / 1000 /
C
      DATA SPCA / 200.0 /
      DATA SPCB / 100.0 /
C
      DATA DPOWER /   0.33333333333333D0 /
      DATA B11    /   0.33333333333333D0 /
      DATA B21    /  -0.02777777777778D0 /
      DATA B31    /  -0.00061728395061D0 /
      DATA B32    / -13.0D0              /
      DATA B41    /   0.00018004115226D0 /
      DATA B42    /   6.0D0              /
      DATA B43    /  17.0D0              /
C
      DATA DPCA   /  4.5D0               /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IND = IZERO
      IF (NU.GT.IZERO) GO TO 10
        IND = IONE
        GO TO 30
  10  IF (X.GE.RZERO) GO TO 20
        IND = IONE
        GO TO 30
C
C     ---   START POINT   ----------------------------------------------
C
  20  DX = X
      ANU = NU
      DNU = NU
C
C     IF X IS NON-POSITIVE, SET CDF  =  0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF  =  0.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF  =  0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF  =  1.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF  =  1.0 AND RETURN.
C
      IF (X.LE.RZERO) GO TO 30
      AMEAN = ANU
      SD = FSQRT (RTWO*ANU)
      Z = FDIV (X-AMEAN,SD,JIND)
      IF (NU.LT.ITEN .AND. Z.LT.(-SPCA)) GO TO 30
      IF (NU.GE.ITEN .AND. Z.LT.(-SPCB)) GO TO 30
      IF (NU.LT.ITEN .AND. Z.GT.SPCA) GO TO 40
      IF (NU.GE.ITEN .AND. Z.GT.SPCB) GO TO 40
      GO TO 50
C
  30  CDF = RZERO
      RETURN
C
C     ..................................................................
C
  40  CDF = RONE
      RETURN
C
C     ..................................................................
C
C
C     DISTINGUISH BETWEEN 3 SEPARATE REGIONS
C     OF THE (X,NU) SPACE.
C     BRANCH TO THE PROPER COMPUTATIONAL METHOD
C     DEPENDING ON THE REGION.
C     NUCUT HAS THE VALUE 1000.
C
  50  IF (NU.LT.NUCUT) GO TO 60
      IF (NU.GE.NUCUT .AND. X.LE.ANU) GO TO 120
      IF (NU.GE.NUCUT .AND. X.GT.ANU) GO TO 130
      IND = ITWO
      RETURN
C
C     ..................................................................
C
C     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE
C     (THAT IS, WHEN NU IS SMALLER THAN 1000).
C     METHOD UTILIZED--EXACT FINITE SUM
C     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5).
C
  60  CHI = FDSQRT (DX)
      IEVODD = NU - ITWO * IDIV (NU,ITWO,JIND)
      CALL DSUMAL (TERM,IZERO,SUM)
      IF (IEVODD.EQ.IZERO) GO TO 70
C
      SUM     = DZERO
      TERM(1) = FDDIV (DONE,CHI,JIND)
      IMIN    = IONE
      IMAX    = NU - IONE
      GO TO 80
C
  70  SUM     = DONE
      TERM(1) = DONE
      CALL DSUMAL (TERM,-IONE,SUM)
      IMIN = ITWO
      IMAX = NU - ITWO
C
  80  IF (IMIN.GT.IMAX) GO TO 100
      DO 90 I=IMIN,IMAX,2
        AI      = I
        TERM(1) = TERM(1) * FDDIV (DX,AI,JIND)
        CALL DSUMAL (TERM,-IONE,SUM)
  90  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 100  SUM = SUM * FDEXP (FDDIV (-DX,DTWO,JIND))
      IF (IEVODD.EQ.IZERO) GO TO 110
      SUM = FDSQRT (FDDIV(DTWO,DPI,JIND)) * SUM
      SPCHI = CHI
      CALL NORCDF (SPCHI,CDFN)
      DCDFN = CDFN
      SUM = SUM + DTWO * (DONE-DCDFN)
 110  CDF = FDPCON (DONE - SUM)
      RETURN
C
C     ..................................................................
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LESS THAN OR EQUAL TO NU.
C     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28).
C
 120  DFACT = DPCA * DNU
      U = FDPCON ( ((FDDIV(DX,DNU,JIND)**DPOWER)-DONE+
     1      FDDIV(DONE,DFACT,JIND))*FDSQRT(DFACT) )
      CALL NORCDF (U,CDFN)
      CDF = CDFN
      RETURN
C
C     ..................................................................
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LARGER THAN NU.
C     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1).
C
 130  DW      = FDSQRT (DX-DNU-DNU*FDLOG(FDDIV(DX,DNU,JIND)))
      DANU    = FDSQRT (FDDIV (DTWO,DNU,JIND))
      D1      = DW
      D2      = DW**2
      D3      = DW**3
      TERM(1) = DW
      TERM(2) = B11 * DANU
      TERM(3) = B21 * D1 * (DANU**2)
      TERM(4) = B31 * (D2+B32) * (DANU**3)
      TERM(5) = B41 * (B42*D3+B43*D1) * (DANU**4)
      CALL DSUMAL (TERM,IFIVE,SUM)
      U = FDPCON(SUM)
      CALL NORCDF (U,CDFN)
      CDF = CDFN
      RETURN
C
C     ==================================================================
C
      END
*CHSPPF
      SUBROUTINE CHSPPF (P,NU,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CHSPPF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THE CHI-SQUARED DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL NON-NEGATIVE X,
C              AND ITS PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN REFERENCES 2, 3, AND 4 BELOW.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION 
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE 
C                                (BETWEEN 0.0 (INCLUSIVELY) 
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT 
C                                FUNCTION IS TO BE EVALUATED.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION . 
C             VALUE PPF FOR THE CHI-SQUARED DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C                 --P SHOULD BE BETWEEN 0.0 (INCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C
C     ACCURACY--(ON THE UNIVAC 1108, EXEC 8 SYSTEM AT NBS)
C               COMPARED TO THE KNOWN NU = 2 (EXPONENTIAL)
C               RESULTS, AGREEMENT WAS HAD OUT TO 6 SIGNIFICANT
C               DIGITS FOR ALL TESTED P IN THE RANGE P = .001 TO
C               P = .999.  FOR P = .95 AND SMALLER, THE AGREEMENT
C               WAS EVEN BETTER--7 SIGNIFICANT DIGITS.
C               (NOTE THAT THE TABULATED VALUES GIVEN IN THE WILK,
C               GNANADESIKAN, AND HUYETT REFERENCE BELOW, PAGE 20,
C               ARE IN ERROR FOR AT LEAST THE GAMMA = 1 CASE--
C               THE WORST DETECTED ERROR WAS AGREEMENT TO ONLY 3
C               SIGNIFICANT DIGITS (IN THEIR 8 SIGNIFICANT DIGIT TABLE)
C               FOR P = .999.)
C     REFERENCES--WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15,
C                 ESPECIALLY PAGES 3-5. 
C               --NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41,
C                 AND PAGES 940-943.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 46-51.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE@D  301-921-2315 
C     ORIGINAL VERSION--SEPTEMBER 1975. 
C     UPDATED         --NOVEMBER  1975. 
C
C               ADAPTED TO OMNITAB BY - 
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             P, PPF 
      REAL             FDPCON
C
      DOUBLE PRECISION D(10), DTERM(9)
      DOUBLE PRECISION Z, Z2, Z3, Z4, Z5, DEN, A, B, C, G
      DOUBLE PRECISION XMIN0, XMIN, AI, XMAX, DP, DX, PCALC, XMID
      DOUBLE PRECISION XLOWER, XUPPER, XDEL
      DOUBLE PRECISION SUM, TERM, CUT1, CUT2, AJ, CUTOFF, T, DNU, DGAMMA
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
      DOUBLE PRECISION DPCA, DPCB, DPCC 
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXIT / 10000 /
C
      DATA ICA / 30000 /
C
      DATA C    /  0.918938533204672741D0  /
C
      DATA D( 1) /  0.833333333333333333D-1 /
      DATA D( 2) / -0.277777777777777778D-2 /
      DATA D( 3) /  0.793650793650793651D-3 /
      DATA D( 4) / -0.595238095238095238D-3 /
      DATA D( 5) /  0.841750841750841751D-3 /
      DATA D( 6) / -0.191752691752691753D-2 /
      DATA D( 7) /  0.641025641025641025D-2 /
      DATA D( 8) / -0.295506535947712418D-1 /
      DATA D( 9) /  0.179644372368830573D0  /
      DATA D(10) / -0.139243221690590111D1  /
C
      DATA DPCA /  1.0D-10 /
      DATA DPCB /  1.0D10  /
      DATA DPCC / 10.0D0   /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GE.RZERO .AND. P.LT.RONE) GO TO 10
        IND = IONE
        PPF = RZERO 
        RETURN
C
C     ..................................................................
C
  10  IF (NU.GE.IONE) GO TO 20
        IND = ITHRE 
        PPF = RZERO 
        RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
C     EXPRESS THE CHI-SQUARED DISTRIBUTION PERCENT POINT
C        FUNCTION IN TERMS OF THE EQUIVALENT GAMMA
C        DISTRIBUTION PERCENT POINT FUNCTION,
C        AND THEN EVALUATE THE LATTER.
C
20    DP     = P
      DNU    = NU
      DGAMMA = FDDIV (DNU,DTWO,JIND)
      XMID   = DONE 
      XLOWER = DONE 
      XUPPER = DONE 
C
C     COMPUTE THE GAMMA FUNCTION USING THE ALGORITHM IN THE 
C        NBS APPLIED MATHEMATICS SERIES REFERENCE.
C
C     THIS GAMMA FUNCTION NEED BE CALCULATED ONLY ONCE.
C        IT IS USED IN THE CALCULATION OF THE CDF BASED ON
C        THE TENTATIVE VALUE OF THE PPF IN THE ITERATION.
C
      Z   = DGAMMA
      DEN = DONE
C
  30  IF (Z.GE.DPCC) GO TO 40 
      DEN = DEN * Z 
      Z   = Z + DONE
      GO TO 30
C
  40  Z2       = Z * Z
      Z3       = Z * Z2
      Z4       = Z2 * Z2
      Z5       = Z2 * Z3
      A        = (Z-DHALF) * FDLOG(Z) - Z + C
      DTERM(1) = FDDIV (D(1),Z,JIND)
      DTERM(2) = FDDIV (D(2),Z3,JIND)
      DTERM(3) = FDDIV (D(3),Z5,JIND)
      DTERM(4) = FDDIV (D(4),Z2*Z5,JIND)
      DTERM(5) = FDDIV (D(5),Z4*Z5,JIND)
      DTERM(6) = FDDIV (D(6),Z*Z5*Z5,JIND)
      DTERM(7) = FDDIV (D(7),Z3*Z5*Z5,JIND)
      DTERM(8) = FDDIV (D(8),Z5*Z5*Z5,JIND)
      DTERM(9) = FDDIV (D(9),Z2*Z5*Z5*Z5,JIND)
      CALL DSUMAL (DTERM,9,B) 
      G = FDDIV (FDEXP(A+B),DEN,JIND)
C
C     DETERMINE LOWER AND UPPER LIMITS ON THE DESIRED 100P
C        PERCENT POINT.
C
      ILOOP  = IONE 
      XMIN0  = (DP*DGAMMA*G) ** FDDIV (DONE,DGAMMA,JIND)
      XMIN   = XMIN0
      ICOUNT = IONE 
C
  50  AI   = ICOUNT 
      XMAX = AI * XMIN0
      DX   = XMAX
      GO TO 130
C
  60  IF (PCALC.GE.DP) GO TO 70
      XMIN   = XMAX 
      ICOUNT = ICOUNT+IONE
      IF (ICOUNT.LE.ICA) GO TO 50
  70  XMID = FDDIV (XMIN+XMAX,DTWO,JIND)
C
C     NOW ITERATE BY BISECTION UNTIL THE DESIRED ACCURACY IS ACHIEVED.
C
      ILOOP  = ITWO 
      XLOWER = XMIN 
      XUPPER = XMAX 
      ICOUNT = IZERO
C
  80  DX = XMID
      GO TO 130
C
  90  IF (PCALC.EQ.DP) GO TO 120
      IF (PCALC.GT.DP) GO TO 100
      XLOWER = XMID 
      XMID   = FDDIV (XMID+XUPPER,DTWO,JIND)
      GO TO 110
C
 100  XUPPER = XMID 
      XMID   = FDDIV (XMID+XLOWER,DTWO,JIND)
 110  XDEL   = XMID - XLOWER
      IF (XDEL.LT.DZERO) XDEL = -XDEL
      ICOUNT = ICOUNT + IONE
      IF (XDEL.LT.DPCA .OR. ICOUNT.GT.IHRD) GO TO 120
      GO TO 80
C
 120  PPF = FDPCON ( DTWO * XMID )
      RETURN
C
C     ..................................................................
C
C     THIS SECTION BELOW IS LOGICALLY SEPARATE FROM THE ABOVE.
C
C     THIS SECTION COMPUTES A CDF VALUE FOR ANY GIVEN TENTATIVE
C        PERCENT POINT X VALUE AS DEFINED IN EITHER OF THE 2
C        ITERATION LOOPS IN THE ABOVE CODE.
C
C     COMPUTE T-SUB-Q AS DEFINED ON PAGE 4 OF THE WILK, GNANADESIKAN, 
C        AND HUYETT REFERENCE.
C
 130  SUM  = FDDIV (DONE,DGAMMA,JIND)
      TERM = FDDIV (DONE,DGAMMA,JIND)
      CUT1 = DX - DGAMMA
      CUT2 = DX * DPCB
      DO 140 J=1,MAXIT
        AJ     = J
        TERM   =  FDDIV (DX*TERM,DGAMMA+AJ,JIND)
        SUM    = SUM + TERM
        CUTOFF = CUT1 + FDDIV (CUT2*TERM,SUM,JIND)
        IF (AJ.GT.CUTOFF) GO TO 150
 140  CONTINUE
      IND = IFOUR
      PPF = RZERO
      RETURN
C
 150  T     = SUM
      PCALC = (DX**DGAMMA) * (FDEXP(-DX)) * FDDIV (T,G,JIND)
      IF (ILOOP.EQ.IONE) GO TO 60
      GO TO 90
C
C     ==================================================================
C
      END 
*CINDEX
      SUBROUTINE CINDEX (MA,I,J,K,INDEX,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. CINDEX V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTES INDEX FOR DETERMINING LINE NUMBER
C        USING PARAMETERS I,J,K, DEFINED IN SUBPROGRAMS MAINSL AND SANDL
C        FOR ALPHA REPRESENTAION OF A NUMBER STORED IN MA(.)
C        IN A1 FORMAT BY SUBPROGRAM RFORMT.
C
C     IND = 0, IF EVERYTHING IS OK.
C           1, IF A FAULT IS FOUND AND
C              INDEX SET EQUAL TO ZERO
C
C     IF MA(J+1)=IPEROD, MA(J+2) IS USED TO COMPUTE MM.
C     IF MA(J) = IPEROD OR MA(J-I+1) = IPEROD, A FAULT OCCURS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1972.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER    LA*1
      CHARACTER    MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 20 /
C
C     ==================================================================
C
      IND = IZERO
      IF (MA(J).EQ.LA(38)) GO TO 70
      IF (I.EQ.IZERO) GO TO 30
      I1 = J - I + IONE
      J1 = J + IONE
      IF (I1.LT.IONE) GO TO 70
      IF (MA(I1).EQ.LA(38)) GO TO 70
  10  IF (MA(J1).NE.LA(38)) GO TO 20
      J1 = J1 + IONE
      GO TO 10
C
  20  CALL ATOI (MA(J1),IONE,MM,IND1)
      J1 = J1 + IONE
      CALL ATOI (MA(I1),I,M,IND2)
      IF (IND1.GT.IZERO .OR. IND2.GT.IZERO) GO TO 70
      INDEX = IDIV (K*M,ITWO,INDA) + IONE + IDIV (K*MM,ICA,INDA)
      GO TO 60
C
C     ..................................................................
C
  30  CALL ATOI (MA(1),J,M,IND1)
      IF (IND1.NE.IZERO) GO TO 70
      MSP = ITEN
      MM = IZERO
      DO 40 IS=1,J
        IF (MSP.GT.M) GO TO 50
        MSP = MSP * ITEN
        MM = MM + IONE
  40  CONTINUE
C
  50  LL = IDIV (M,(IDIV(MSP,ITEN,INDA)),INDA)
      INDEX = (ITHRE*K)*MM + IDIV (K*K*LL+ITWO,ITHRE*K,INDA) + IONE
  60  IF (INDEX.GT.IZERO) RETURN
  70  INDEX = IZERO
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*CKIND
      SUBROUTINE CKIND (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  CKIND V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THE FIRST J VALUES OF KIND ARE CHECKED.
C
C        J = 0, IF ALL VALUES OF KIND EQUAL ZERO
C          = 1, IF ALL VALUES OF KIND EQUAL ONE
C          = 2, IF SOME VALUES OF KIND EQUAL ZERO AND SOME EQUAL ONE.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
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
C
C     ==================================================================
C
      JA = J
      J = IZERO
      DO 10 I=1,JA
        IF (KIND(I).NE.IZERO) GO TO 20
  10  CONTINUE
      RETURN
C
C     ..................................................................
C
  20  J = IONE
      DO 30 I=1,JA
        IF (KIND(I).NE.IONE) GO TO 40
  30  CONTINUE
      RETURN
C
C     ..................................................................
C
  40  J = ITWO
      RETURN
C
C     ==================================================================
C
      END
*CMPARA
      SUBROUTINE CMPARA (X1,X2,X3,Y1,Y2,Y3,X,Y)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CMPARA V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CMPARA IS USED BY THE INSTRUCTION MAXMIN IN PROGRAM UNIT CMSEPA.
C
C               WRITTEN BY -
C                      CARLA MESSINA,
C                      NSRDS
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JUNE, 1968.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, X1, X2, X3, Y, Y1, Y2, Y3
      REAL             A, B, C
      REAL             FDIV
C
C     ==================================================================
C
      A = FDIV ((Y2-Y3)*(X2-X1)-(Y2-Y1)*(X2-X3),(X2-X1)*(X2**2-X3**2)-
     1     (X2-X3)*(X2**2-X1**2),IND)
      B = FDIV ((Y2-Y1)-A*(X2**2-X1**2),X2-X1,IND)
      C = -A*X3**2 - B*X3 + Y3
      X = FDIV (-B,RTWO*A,IND)
      Y = A*X**2 + B*X + C
      RETURN
C
C     ==================================================================
C
      END
*CNTNT1
      SUBROUTINE CNTNT1 (M)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CNTNT1 V 7.00 12/ 7 59. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT CALLED BY CNTNTS TO ...
C        PRINT SUBSECTION TITLES FOR CONTENTS INSTRUCTION.
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
C                   CURRENT VERSION -  DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C     PRINT SUBSECTION TITLES.
C
      GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112,113), M
C
 101  WRITE (IPRINT,501)
      RETURN
C
 102  WRITE (IPRINT,502)
      RETURN
C
 103  WRITE (IPRINT,503)
      RETURN
C
 104  WRITE (IPRINT,504)
      RETURN
C
 105  WRITE (IPRINT,505)
      RETURN
C
 106  WRITE (IPRINT,506)
      RETURN
C
 107  WRITE (IPRINT,507)
      RETURN
C
 108  WRITE (IPRINT,508)
      RETURN
C
 109  WRITE (IPRINT,509)
      RETURN
C
 110  WRITE (IPRINT,510)
      RETURN
C
 111  WRITE (IPRINT,511)
      RETURN
C
 112  WRITE (IPRINT,512)
      RETURN
C
 113  WRITE (IPRINT,513)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (1)   CONTROL INSTRUCTIONS.
C
 501  FORMAT (
     1        5X,37H1.1   ESSENTIAL CONTROL INSTRUCTIONS./
     2        5X,43H1.2   CONTROL INSTRUCTIONS FOR FLEXIBILITY./
     3        5X,20H1.3   USE OF LABELS./
     4        5X,36H1.4   USE OF OMNITAB WITH TERMINALS./
     5        5X,31H1.5   CONTROLLING SIZE OF PAGE./
     6        5X,37H1.6   CONTROLLING AMOUNT OF PRINTING./
     7        5X,27H1.7   MULTILINGUAL OMNITAB./)
C
C     (2)   ENTERING AND PRINTING DATA.
C
 502  FORMAT (
     1        5X,39H2.1   ENTERING DATA INTO THE WORKSHEET./
     2        5X,35H2.2   COMMON PRINTING INSTRUCTIONS./
     3        5X,24H2.3   DETAILED PRINTING./
     4        5X,42H2.4   OPTIONAL FORMS OF READABLE PRINTING./
     5        5X,37H2.5   FORMATTED READING AND PRINTING./
     6        5X,35H2.6   PRINTING ARRAYS AND MATRICES./
     7        5X,31H2.7   PUNCHING DATA ONTO CARDS./
     8        5X,32H2.8   USE OF PERIPHERAL DEVICES./)
C
C     (3)   PLOTTING DATA.
C
 503  FORMAT (
     1        5X,34H3.1   BASIC PLOTTING INSTRUCTIONS./
     2        5X,22H3.2   CHARACTER PLOTS./
     3        5X,29H3.3   PLOTS WITH NICE SCALES./
     4        5X,23H3.4   TITLES FOR PLOTS./
     5        5X,30H3.5   CONTROL OF SIZE OF PLOT./
     6        5X,30H3.6   MULTIPLE PLOTS PER PAGE./
     7        5X,29H3.7   USE OF CALCOMP PLOTTER./
     8        5X,24H3.8   STATISTICAL PLOTS./
     9        5X,27H3.9   PROBABILITY PLOTTING./
     A        5X,31H3.10  USE OF TEKTRONIX PLOTTER./)
C
C     (4)   ARITHMETIC INSTRUCTIONS.
C
 504  FORMAT (
     1        5X,24H4.1   SIMPLE ARITHMETIC./
     2        5X,29H4.2   MORE SIMPLE ARITHMETIC./
     3        5X,36H4.3   LOGARITHMS AND EXPONENTIATION./
     4        5X,31H4.4   TRIGONOMETRIC OPERATIONS./
     5        5X,24H4.5   TRIPLE OPERATIONS./
     6        5X,40H4.6   EVALUATION OF FORTRAN EXPRESSIONS./
     7        5X,25H4.7   DATA SUMMARIZATION./
     8        5X,25H4.8   COMPLEX ARITHMETIC./)
C
C     (5)   DATA MANIPULATION.
C
 505  FORMAT (
     1        5X,26H5.1   DEFINING OPERATIONS./
     2        5X,18H5.2   MOVING DATA./
     3        5X,30H5.3   MANIPULATIVE OPERATIONS./
     4        5X,19H5.4   SORTING DATA./
     5        5X,24H5.5   SEARCH OPERATIONS./
     6        5X,19H5.6   EDITING DATA./)
C
C     (6)   STATISTICAL ANALYSIS.
C
 506  FORMAT (
     1        5X,23H6.1   BASIC STATISTICS./
     2        5X,24H6.2   STATISTICAL PLOTS./
     3        5X,37H6.3   ANALYSIS OF ONE COLUMN OF DATA./
     4        5X,17H6.4   REGRESSION./
     5        5X,50H6.5   SELECTION OF VARIABLES IN LINEAR REGRESSION./
     6        5X,39H6.6   ANALYSIS OF DESIGNED EXPERIMENTS./
     7        5X,27H6.7   CORRELATION ANALYSIS./
     8        5X,44H6.8   ANALYSIS OF TWOWAY CONTINGENCY TABLES./
     9        5X,39H6.9   TABLE MAKING OR CROSS TABULATION./)
C
C     (7)   PROBABILITY.
C
 507  FORMAT (
     1        5X,36H7.1   PROBABILITY DENSITY FUNCTIONS./
     2        5X,40H7.2   CUMULATIVE DISTRIBUTION FUNCTIONS./
     3        5X,30H7.3   PERCENT POINT FUNCTIONS./
     4        5X,21H7.4   RANDOM SAMPLES./
     5        5X,27H7.5   PROBABILITY PLOTTING./
     6        5X,30H7.6   RANDOM SAMPLE OF DIGITS.)
C
C     (8)   NUMERICAL ANALYSIS.
C
 508  FORMAT (
     1        5X,24H8.1   SPECIAL INTEGRALS./
     2        5X,18H8.2   POLYNOMIALS./
     3        5X,18H8.3   DIFFERENCES./
     4        5X,16H8.4   ITERATION./
     5        5X,15H8.5   ANALYSIS./
     6        5X,18H8.6   INTEGRATION./)
C
C     (9)   REPEATED USE OF INSTRUCTIONS.
C
 509  FORMAT (
     1        5X,25H9.1   REPEATED EXECUTION./
     2        5X,32H9.2   INCREMENTING INSTRUCTIONS./
     3        5X,33H9.3   BRANCHING, THREE ARGUMENTS./
     4        5X,31H9.4   BRANCHING, TWO ARGUMENTS./)
C
C     (10)    ARRAY OPERATIONS.
C
 510  FORMAT (
     1        5X,18H10.1   ARITHMETIC./
     2        5X,25H10.2   DATA MANIPULATION./
     3        5X,21H10.3   SUMMARIZATION./
     4        5X,30H10.4   PROPERTIES OF AN ARRAY./
     5        5X,16H10.5   PRINTING./
     6        5X,23H10.6   MATRIX SYNONYMS./)
C
C     (11)   MATRIX OPERATIONS.
C
 511  FORMAT (
     1        5X,27H11.1   DEFINING OPERATIONS./
     2        5X,25H11.2   MOVING OPERATIONS./
     3        5X,25H11.3   MATRIX ARITHMETIC./
     4        5X,37H11.4   SPECIAL MATRIX MULTIPLICATION./
     5        5X,23H11.5   MATRIX ANALYSIS./
     6        5X,18H11.6   PROPERTIES./
     7        5X,16H11.7   PRINTING./)
C
C     (12)   BESSEL FUNCTIONS.
C
 512  FORMAT (
     1        5X,51H12.1   FIRST AND SECOND FUNCTIONS OF ORDER 0 AND 1./
     2        5X,26H12.2   MODIFIED FUNCTIONS./
     3        5X,51H12.3   MODIFIED FUNCTIONS, EXTREME VALUED ARGUMENT./
     4        5X,25H12.4   COMPLEX FUNCTIONS./
     5        5X,48H12.5   COMPLEX FUNCTIONS, EXTREME REAL ARGUMENT./
     6        5X,46H12.6   COMPLEX FUNCTIONS WITH ARBITRARY ANGLE./
     7        5X,48H12.7   COMPLEX FUNCTIONS, EXTREME REAL ARGUMENT./
     8        5X,33H12.8   ZEROS OF BESSEL FUNCTIONS./
     9        5X,35H12.9   BESSEL FUNCTIONS OF ORDER N./
     A        5X,16H12.10  INTEGRAL./)
C
C     (12)   THERMODYNAMICS.
C
 513  FORMAT (
     1        5X,36H13.1   TEMPERATURE SCALE CONVERSION./
     2        5X,39H13.2   SYSTEMS OF UNITS OF MEASUREMENT./
     3        5X,24H13.3   MOLECULAR WEIGHT./
     4        5X,27H13.4   PROPERTIES OF STATE./)
C
C     ==================================================================
C
      END
*CNTNT2
      SUBROUTINE CNTNT2 (M,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CNTNT2 V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT CALLED BY CNTNTS TO
C        PRINT LIST OF COMMANDS FOR SECTIONS 1 TO 4.
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
C                   CURRENT VERSION -  DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C     PRINT LIST OF COMMANDS IN SUBSECTION.
C
      GO TO (201,202,203,204), M
C
 201  GO TO (301,302,303,304,305,306,307),N
C
 202  GO TO (308,309,310,311,312,313,314,315),N
C
 203  GO TO (316,317,318,319,320,321,322,323,324,325),N
C
 204  GO TO (326,327,328,329,330,331,332,333),N
C
C     ..................................................................
C
C     (1)   CONTROL INSTRUCTIONS.
C
 301  WRITE (IPRINT,601)
      RETURN
C
 302  WRITE (IPRINT,602)
      RETURN
C
 303  WRITE (IPRINT,603)
      RETURN
C
 304  WRITE (IPRINT,604)
      RETURN
C
 305  WRITE (IPRINT,605)
      RETURN
C
 306  WRITE (IPRINT,606)
      RETURN
C
 307  WRITE (IPRINT,607)
      RETURN
C
C     (2)   ENTERING AND PRINTING DATA.
C
 308  WRITE (IPRINT,608)
      RETURN
C
 309  WRITE (IPRINT,609)
      RETURN
C
 310  WRITE (IPRINT,610)
      RETURN
C
 311  WRITE (IPRINT,611)
      RETURN
C
 312  WRITE (IPRINT,612)
      RETURN
C
 313  WRITE (IPRINT,613)
      RETURN
C
 314  WRITE (IPRINT,614)
      RETURN
C
 315  WRITE (IPRINT,615)
      RETURN
C
C     (3)   PLOTTING DATA.
C
 316  WRITE (IPRINT,616)
      RETURN
C
 317  WRITE (IPRINT,617)
      RETURN
C
 318  WRITE (IPRINT,618)
      RETURN
C
 319  WRITE (IPRINT,619)
      RETURN
C
 320  WRITE (IPRINT,620)
      RETURN
C
 321  WRITE (IPRINT,621)
      RETURN
C
 322  WRITE (IPRINT,622)
      RETURN
C
 323  WRITE (IPRINT,623)
      RETURN
C
 324  WRITE (IPRINT,624)
      RETURN
C
 325  WRITE (IPRINT,625)
      RETURN
C
C     (4)   ARITHMETIC INSTRUCTIONS.
C
 326  WRITE (IPRINT,626)
      RETURN
C
 327  WRITE (IPRINT,627)
      RETURN
C
 328  WRITE (IPRINT,628)
      RETURN
C
 329  WRITE (IPRINT,629)
      RETURN
C
 330  WRITE (IPRINT,630)
      RETURN
C
 331  WRITE (IPRINT,631)
      RETURN
C
 332  WRITE (IPRINT,632)
      RETURN
C
 333  WRITE (IPRINT,633)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (1)   CONTROL INSTRUCTIONS.
C
 601  FORMAT (
     1  1X,13HOMNITAB, STOP)
 602  FORMAT (
     1  1X,36HDIMENSION, LIST, NO LIST, NULL, SCAN)
 603  FORMAT (
     1  1X,21HLABEL, ALABEL, MLABEL)
 604  FORMAT (
     1  1X,42HINTERACTIVE, LOCAL, REMOTE, CRT, TERMINAL,
     2 /1X,18HCONTENTS, DESCRIBE)
 605  FORMAT (
     1  1X,13HLENGTH, WIDTH)
 606  FORMAT (
     1  1X,11HBRIEF, FULL)
 607  FORMAT (
     1  1X,53HDANSK, DEUTSCH, ENGLISH, ESPANOL, FRANCAIS, ITALIANO,
     2 /1X,57HJAPANESE, NEDERLANDS, NORSK, PORTUGESE, SLOVENE, SVENSKA,
     3 /1X,20HYUGOSLAV, VOCABULARY)
C
C     (2)   ENTERING AND PRINTING DATA.
C
 608  FORMAT (
     1  1X,19HGENERATE, READ, SET)
 609  FORMAT (
     1  1X,49HPRINT, NPRINT, ABRIDGE, FIXED, FLOATING, FLEXIBLE)
 610  FORMAT (
     1  1X,54HHEAD, NEW PAGE, NOTE, NOTE1, NOTE2, PRINT NOTE, SPACE,
     2 /1X,30HTITLE1, TITLE2, TITLE3, TITLE4)
 611  FORMAT (
     1  1X,22HPRINT, ABRIDGE, NPRINT)
 612  FORMAT (
     1  1X,56HFORMAT 'L', READ 'L', PRINT 'L', NPRINT 'L', ABRIDGE 'L')
 613  FORMAT (
     1  1X,38HAPRINT, MPRINT, APRINT 'L', MPRINT 'L')
 614  FORMAT (
     1  1X,16HPUNCH, PUNCH 'L')
 615  FORMAT (
     1  1X, 5HUNIT,
     2 /1X,54HBACKSPACE UNIT 'L', ENDFILE UNIT 'L', REWIND UNIT 'L',
     3 /1X,43HSKIP UNIT 'L', SET UNIT 'L', CSET UNIT 'L',
     4 /1X,33HREAD UNIT 'L', READ UNIT 'L' 'L',
     5 /1X,35HCREAD UNIT 'L', CREAD UNIT 'L' 'L',
     6 /1X,34HWRITE UNIT 'L', WRITE UNIT 'L' 'L')
C
C     (3)   PLOTTING DATA.
C
 616  FORMAT (
     1  1X,22HPLOT, NPLOT, PAGE PLOT)
 617  FORMAT (
     1  1X,13HCPLOT, NCPLOT)
 618  FORMAT (
     1  1X,46HNICE PLOT, NICE CPLOT, NICE NPLOT, NICE NCPLOT)
 619  FORMAT (
     1  1X,14HTITLEX, TITLEY)
 620  FORMAT (
     1  1X,13HLENGTH, WIDTH)
 621  FORMAT (
     1  1X,19HFOURPLOTS, TWOPLOTS)
 622  FORMAT (
     1  1X,13HCALCOMP PLOT,
     2 /1X,42HCALCOMP AXIS, CALCOMP SIZE, CALCOMP PAPER,
     3 /1X,42HCALCOMP SPEED, CALCOMP SLOW, CALCOMP FAST,
     4 /1X,16HCALCOMP UNIT 'L')
 623  FORMAT (
     1  1X,55HHISTOGRAM, NHISTOGRAM, STATPLOTS, STEM LEAF, SSTEM LEAF)
 624  FORMAT (
     1  1X,36HCAUCHY PLOT,      DEXPONENTIAL PLOT,
     2 /1X,31HEXPONENTIAL PLOT, EXTREME PLOT,
     3 /1X,34HGAMMA PLOT,       HALFNORMAL PLOT,
     4 /1X,32HLAMBDA PLOT,      LOGISTIC PLOT,
     5 /1X,30HLOGNORMAL PLOT,   NORMAL PLOT,
     6 /1X,31HPARETO PLOT,      UNIFORM PLOT,
     7 /1X,12HWEIBULL PLOT)
 625  FORMAT (
     1  1X,15HTEKTRONIX PLOT,
     2 /1X,34HTEKTRONIX AXIS, TEKTRONIX TERMINAL)
C
C     (4)   ARITHMETIC OPERATIONS.
C
 626  FORMAT (
     1  1X,38HADD, SUBTRACT, MULTIPLY, DIVIDE, RAISE)
 627  FORMAT (
     1  1X,42HABSOLUTE, CHANGE, RECIPROCAL, SQRT, SQUARE)
 628  FORMAT (
     1  1X,50HLOGE, LOGTEN, ANTILOG, EXPONENTIAL, NEGEXPONENTIAL)
 629  FORMAT (
     1  1X,55H COS,   COT,   SIN,   TAN,   COSD,  COTD,  SIND,  TAND,
     2 /1X,55HACOS,  ACOT,  ASIN,  ATAN,  ACOSD, ACOTD, ASIND, ATAND,
     3 /1X,54H COSH,  COTH,  SINH,  TANH, ACOSH, ACOTH, ASINH, ATANH)
 630  FORMAT (
     1  1X,39HADD, SUBTRACT, MULTIPLY, DIVIDE, RAISE,
     2 /1X,35HABSOLUTE, RECIPROCAL, SQRT, SQUARE,
     3 /1X,20HFRACTIONAL, INTEGER,
     4 /1X,51HLOGE, LOGTEN, ANTILOG, EXPONENTIAL, NEGEXPONENTIAL,
     5 /1X,55H COS,   COT,   SIN,   TAN,   COSD,  COTD,  SIND,  TAND,
     6 /1X,55HACOS,  ACOT,  ASIN,  ATAN,  ACOSD, ACOTD, ASIND, ATAND,
     7 /1X,54H COSH,  COTH,  SINH,  TANH, ACOSH, ACOTH, ASINH, ATANH)
 631  FORMAT (
     1  1X, 8HEVALUATE)
 632  FORMAT (
     1  1X,37HACCURACY, FRACTIONAL, INTEGER, ROUND,
     2 /1X,21HSUM, ROW SUM, PARSUM,
     3 /1X,28HEXPAND, PRODUCT, PARPRODUCT,
     4 /1X,33HPERCENTAGE, PROPORTION, RMS, DAYS)
 633  FORMAT (
     1  1X,57HCADD, CSUBTRACT, CMULTIPLY, CDIVIDE, CPOLAR, CRECTANGULAR)
C
C     ==================================================================
C
      END
*CNTNT3
      SUBROUTINE CNTNT3 (M,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CNTNT3 V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT CALLED BY CNTNTS TO
C        PRINT LIST OF COMMANDS FOR SECTIONS 5 TO 8.
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
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C     PRINT LIST OF COMMANDS IN SUBSECTION.
C
      GO TO (205,206,207,208), M
C
 205  GO TO (333,334,335,336,337,338),N
C
 206  GO TO (339,340,341,342,343,344,345,346,347),N
C
 207  GO TO (348,349,350,351,352,353),N
C
 208  GO TO (354,355,356,357,358,359),N
C
C     ..................................................................
C
C     (5)   DATA MANIPULATION.
C
 333  WRITE (IPRINT,633)
      RETURN
C
 334  WRITE (IPRINT,634)
      RETURN
C
 335  WRITE (IPRINT,635)
      RETURN
C
 336  WRITE (IPRINT,636)
      RETURN
C
 337  WRITE (IPRINT,637)
      RETURN
C
 338  WRITE (IPRINT,638)
      RETURN
C
C     (6)   STATISTICAL ANALYSIS.
C
 339  WRITE (IPRINT,639)
      RETURN
C
 340  WRITE (IPRINT,640)
      RETURN
C
 341  WRITE (IPRINT,641)
      RETURN
C
 342  WRITE (IPRINT,642)
      RETURN
C
 343  WRITE (IPRINT,643)
      RETURN
C
 344  WRITE (IPRINT,644)
      RETURN
C
 345  WRITE (IPRINT,645)
      RETURN
C
 346  WRITE (IPRINT,646)
      RETURN
C
 347  WRITE (IPRINT,647)
      RETURN
C
C     (7)   PROBABILITY.
C
 348  WRITE (IPRINT,648)
      RETURN
C
 349  WRITE (IPRINT,649)
      RETURN
C
 350  WRITE (IPRINT,650)
      RETURN
C
 351  WRITE (IPRINT,651)
      RETURN
C
 352  WRITE (IPRINT,652)
      RETURN
C
 353  WRITE (IPRINT,653)
      RETURN
C
C     (8)   NUMERICAL ANALYSIS.
C
 354  WRITE (IPRINT,654)
      RETURN
C
 355  WRITE (IPRINT,655)
      RETURN
C
 356  WRITE (IPRINT,656)
      RETURN
C
 357  WRITE (IPRINT,657)
      RETURN
C
 358  WRITE (IPRINT,658)
      RETURN
C
 359  WRITE (IPRINT,659)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (5)   DATA MANIPULATION.
C
 633  FORMAT (
     1  1X,38HCOUNT, DEFINE, ERASE, RESET, RESET 'V')
 634  FORMAT (
     1  1X,42HPROMOTE, DEMOTE, DUPLICATE, MOVE, EXCHANGE)
 635  FORMAT (
     1  1X,50HCENSOR, CLOSE UP, FLIP, INSERT, SEPARATE, SHORTEN,
     2 /1X,32HCENSOR EQ, CENSOR LT, CENSOR LE,
     3 /1X,31HCENSOR NE, CENSOR GT, CENSOR GE)
 636  FORMAT (
     1  1X,22HORDER, SORT, HIERARCHY)
 637  FORMAT (
     1  1X,21HMATCH, SEARCH, SELECT)
 638  FORMAT (
     1  1X,51HCODE, RECODE, REPLACE, CHOOSE, RETAIN, OMIT, DELETE)
C
C     (6)   STATISTICAL ANALYSIS.
C
 639  FORMAT (
     1  1X,49HAVERAGE, MEDIAN, STDDEV, RANGE, MINIMUM, MAXIMUM,
     2 /1X,16HFREQUENCY, RANKS)
 640  FORMAT (
     1  1X,55HHISTOGRAM, NHISTOGRAM, STATPLOTS, STEM LEAF, SSTEM LEAF)
 641  FORMAT (
     1  1X,25HSTATISTICAL, SSTATISTICAL)
 642  FORMAT (
     1  1X,36HFIT, POLYFIT, SFIT, SPOLYFIT, LARFIT)
 643  FORMAT (
     1  1X, 6HBESTCP)
 644  FORMAT (
     1  1X,44HONEWAY, TWOWAY, SPLIT PLOT, SONEWAY, STWOWAY)
 645  FORMAT (
     1  1X,25HCORRELATION, SCORRELATION)
 646  FORMAT (
     1  1X,11HCONTINGENCY)
 647  FORMAT (
     1  1X,54H TABLE FREQUENCY,   TABLE MAXIMUM,      TABLE MINIMUM,
     2 /1X,53H TABLE SUM,         TABLE AVERAGE,      TABLE MEDIAN,
     3 /1X,58H TABLE PERCENTAGE,  TABLE RPERCENTAGE,  TABLE CPERCENTAGE,
     4 /1X,58H TABLE PROPORTION,  TABLE RPROPORTION,  TABLE CPROPORTION,
     5 /1X,33H TABLE RANGE,       TABLE STDDEV,
     6 /1X,54HNTABLE FREQUENCY,  NTABLE MAXIMUM,     NTABLE MINIMUM,
     7 /1X,53HNTABLE SUM,        NTABLE AVERAGE,     NTABLE MEDIAN,
     8 /1X,58HNTABLE PERCENTAGE, NTABLE RPERCENTAGE, NTABLE CPERCENTAGE,
     9 /1X,58HNTABLE PROPORTION, NTABLE RPROPORTION, NTABLE CPROPORTION,
     A /1X,32HNTABLE RANGE,      NTABLE STDDEV)
C
C     (7)   PROBABILITY.
C
 648  FORMAT (
     1  1X,39HBETA DENSITY,         BINOMIAL DENSITY,
     2 /1X,43HCAUCHY DENSITY,       DEXPONENTIAL DENSITY,
     3 /1X,38HEXPONENTIAL DENSITY,  EXTREME DENSITY,
     4 /1X,41HGEOMETRIC DENSITY,    HALFNORMAL DENSITY,
     5 /1X,39HLAMBDA DENSITY,       LOGISTIC DENSITY,
     6 /1X,42HLOGNORMAL DENSITY,    NEGBINOMIAL DENSITY,
     7 /1X,37HNORMAL DENSITY,       PARETO DENSITY,
     8 /1X,38HPOISSON DENSITY,      UNIFORM DENSITY,
     9 /1X,15HWEIBULL DENSITY)
 649  FORMAT (
     1  1X,45HBETA CUMULATIVE,         BINOMIAL CUMULATIVE,
     2 /1X,47HCAUCHY  CUMULATIVE,      CHISQUARED CUMULATIVE,
     3 /1X,48HDEXPONENTIAL CUMULATIVE, EXPONENTIAL CUMULATIVE,
     4 /1X,38HEXTREME CUMULATIVE,      F CUMULATIVE,
     5 /1X,46HGAMMA CUMULATIVE,        GEOMETRIC CUMULATIVE,
     6 /1X,43HHALFNORMAL CUMULATIVE,   LAMBDA CUMULATIVE,
     7 /1X,46HLOGISTIC CUMULATIVE,     LOGNORMAL CUMULATIVE,
     8 /1X,43HNEGBINOMIAL CUMULATIVE,  NORMAL CUMULATIVE,
     9 /1X,44HPARETO CUMULATIVE,       POISSON CUMULATIVE,
     A /1X,44HT CUMULATIVE,            UNIFORM CUMULATIVE,
     B /1X,38HWEIBULL CUMULATIVE,      F PROBABILITY)
 650  FORMAT (
     1  1X,42HBINOMIAL PERCENTILE,    CAUCHY PERCENTILE,
     2 /1X,48HCHISQUARED PERCENTILE,  DEXPONENTIAL PERCENTILE,
     3 /1X,43HEXPONENTIAL PERCENTILE, EXTREME PERCENTILE,
     4 /1X,45HGAMMA PERCENTILE,       GEOMETRIC PERCENTILE,
     5 /1X,42HHALFNORMAL PERCENTILE,  LAMBDA PERCENTILE,
     6 /1X,45HLOGISTIC PERCENTILE,    LOGNORMAL PERCENTILE,
     7 /1X,42HNEGBINOMIAL PERCENTILE, NORMAL PERCENTILE,
     8 /1X,43HPARETO PERCENTILE,      POISSON PERCENTILE,
     9 /1X,43HT PERCENTILE,           UNIFORM PERCENTILE,
     A /1X,18HWEIBULL PERCENTILE)
 651  FORMAT (
     1  1X,37HBETA RANDOM,         BINOMIAL RANDOM,
     2 /1X,38HCAUCHY RANDOM,       CHISQUARE RANDOM,
     3 /1X,40HDEXPONENTIAL RANDOM, EXPONENTIAL RANDOM,
     4 /1X,30HEXTREME RANDOM,      F RANDOM,
     5 /1X,38HGAMMA RANDOM,        GEOMETRIC RANDOM,
     6 /1X,35HHALFNORMAL RANDOM,   LAMBDA RANDOM,
     7 /1X,38HLOGISTIC RANDOM,     LOGNORMAL RANDOM,
     8 /1X,35HNEGBINOMIAL RANDOM,  NORMAL RANDOM,
     9 /1X,36HPARETO RANDOM,       POISSON RANDOM,
     A /1X,36HT RANDOM,            UNIFORM RANDOM,
     B /1X,14HWEIBULL RANDOM)
 652  FORMAT (
     1  1X,36HCAUCHY PLOT,      DEXPONENTIAL PLOT,
     2 /1X,31HEXPONENTIAL PLOT, EXTREME PLOT,
     3 /1X,34HGAMMA PLOT,       HALFNORMAL PLOT,
     4 /1X,32HLAMBDA PLOT,      LOGISTIC PLOT,
     5 /1X,30HLOGNORMAL PLOT,   NORMAL PLOT,
     6 /1X,31HPARETO PLOT,      POISSON PLOT,
     7 /1X,30HUNIFORM PLOT,     WEIBULL PLOT)
 653  FORMAT (
     1  1X,29HSAMPLE WITHR, SAMPLE WITHOUTR)
C
C     (8)   NUMERICAL ANALYSIS.
C
 654  FORMAT (
     1  1X,19HERROR, CERF, GAMMA,
     2 /1X,36HELLIPTICAL FIRST, ELLIPTICAL SECOND,
     3 /1X,24HSTRUVE ZERO, STRUVE ONE,
     4 /1X,53HCOSINTEGRAL, SININTEGRAL, HCOSINTEGRAL, HSININTEGRAL,
     5 /1X,50HEXPINTEGRAL, EEXPINTEGRAL, EINTEGRAL, NEGEINTEGRAL)
 655  FORMAT (
     1  1X,42HHERMITE, LAGUERRE, LEGENDRE, NORMLAGUERRE,
     2 /1X,22HTCHEBYSHEV, UCHEBYSHEV)
 656  FORMAT (
     1  1X,28HDIFFERENCES, DIVDIFFERENCES,
     2 /1X,29HSDIFFERENCES, SDIVDIFFERENCES)
 657  FORMAT (
     1  1X,24HISETUP, ITERATE, ISOLATE)
 658  FORMAT (
     1  1X,36HHARMONIC, INTERPOLATE, MAXMIN, SOLVE)
 659  FORMAT (
     1  1X,16HGAUSS QUADRATURE)
C
C     ==================================================================
C
      END
*CNTNT4
      SUBROUTINE CNTNT4 (M,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CNTNT4 V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT CALLED BY CNTNTS TO
C        PRINT LIST OF COMMANDS FOR SECTIONS 9 TO 13.
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
C                   CURRENT VERSION -  DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C     PRINT LIST OF COMMANDS IN SUBSECTION.
C
      GO TO (209,210,211,212,213), M
C
 209  GO TO (360,361,362,363),N
C
 210  GO TO (364,365,366,367,368,369),N
C
 211  GO TO (370,371,372,373,374,375,376),N
C
 212  GO TO (377,378,379,380,381,382,383,384,385,386),N
C
 213  GO TO (387,388,389,390),N
C
C     ..................................................................
C
C     (9)   REPEATED USE OF INSTRUCTIONS.
C
 360  WRITE (IPRINT,660)
      RETURN
C
 361  WRITE (IPRINT,661)
      RETURN
C
 362  WRITE (IPRINT,662)
      RETURN
C
 363  WRITE (IPRINT,663)
      RETURN
C
C     (10)    ARRAY OPERATIONS.
C
 364  WRITE (IPRINT,664)
      RETURN
C
 365  WRITE (IPRINT,665)
      RETURN
C
 366  WRITE (IPRINT,666)
      RETURN
C
 367  WRITE (IPRINT,667)
      RETURN
C
 368  WRITE (IPRINT,668)
      RETURN
C
 369  WRITE (IPRINT,669)
      RETURN
C
C     (11)   MATRIX OPERATIONS.
C
 370  WRITE (IPRINT,670)
      RETURN
C
 371  WRITE (IPRINT,671)
      RETURN
C
 372  WRITE (IPRINT,672)
      RETURN
C
 373  WRITE (IPRINT,673)
      RETURN
C
 374  WRITE (IPRINT,674)
      RETURN
C
 375  WRITE (IPRINT,675)
      RETURN
C
 376  WRITE (IPRINT,676)
      RETURN
C
C     (12)   BESSEL FUNCTIONS.
C
 377  WRITE (IPRINT,677)
      RETURN
C
 378  WRITE (IPRINT,678)
      RETURN
C
 379  WRITE (IPRINT,679)
      RETURN
C
 380  WRITE (IPRINT,680)
      RETURN
C
 381  WRITE (IPRINT,681)
      RETURN
C
 382  WRITE (IPRINT,682)
      RETURN
C
 383  WRITE (IPRINT,683)
      RETURN
C
 384  WRITE (IPRINT,684)
      RETURN
C
 385  WRITE (IPRINT,685)
      RETURN
C
 386  WRITE (IPRINT,686)
      RETURN
C
C     (12)   THERMODYNAMICS.
C
 387  WRITE (IPRINT,687)
      RETURN
C
 388  WRITE (IPRINT,688)
      RETURN
C
 389  WRITE (IPRINT,689)
      RETURN
C
 390  WRITE (IPRINT,690)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     (9)   REPEATED USE OF INSTRUCTIONS.
C
 660  FORMAT (
     1  1X,22HPERFORM, BEGIN, FINISH)
 661  FORMAT (
     1  1X,18HINCREMENT, RESTORE)
 662  FORMAT (
     1  1X,19HCOMPARE, IFEQ, IFNE)
 663  FORMAT (
     1  1X,34HIFEQ, IFNE, IFLT, IFLE, IFGT, IFGE)
C
C     (10)   MATRIX OPERATIONS.
C
 664  FORMAT (
     1  1X,43HAADD, ASUBTRACT, AMULTIPLY, ADIVIDE, ARAISE)
 665  FORMAT (
     1  1X,34HADEFINE, AERASE, AMOVE, ATRANSPOSE)
 666  FORMAT (
     1  1X,19HAAVERAGE, ACOALESCE)
 667  FORMAT (
     1  1X,25HAPROPERTIES, SAPROPERTIES)
 668  FORMAT (
     1  1X,18HAPRINT, APRINT 'L')
 669  FORMAT (
     1  1X,49HAADD       = MADD,        ASUBTRACT  = MSUBTRACT,
     2 /1X,46HADEFINE    = MDEFINE,     AERASE     = MERASE,
     3 /1X,50HAMOVE      = MMOVE,       ATRANSPOSE = MTRANSPOSE,
     4 /1X,23HAPRINT 'L' = MPRINT 'L')
C
C     (11)   MATRIX OPERATIONS.
C
 670  FORMAT (
     1  1X,37HMDEFINE, MDIAGONAL, MERASE, MIDENTITY)
 671  FORMAT (
     1  1X,49HMMOVE, MTRANSPOSE, MVECMAT, MMATVEC, MVECDIAGONAL)
 672  FORMAT (
     1  1X,55HMADD, MSUBTRACT, MMULTIPLY, MRAISE, MSCALAR, MKRONECKER)
 673  FORMAT (
     1  1X,28HM(AD), M(DA), M(AV), M(V'A),
     2 /1X,32HM(X'X), M(XX'), M(X'AX), M(XAX'))
 674  FORMAT (
     1  1X,39HMEIGEN, MINVERT, MORTHO, MTRIANGULARIZE)
 675  FORMAT (
     1  1X,25HMPROPERTIES, SMPROPERTIES)
 676  FORMAT (
     1  1X,18HMPRINT, MPRINT 'L')
C
C     (12)   BESSEL FUNCTIONS.
C
 677  FORMAT (
     1  1X,28HBJZERO, BJONE, BYZERO, BYONE)
 678  FORMAT (
     1  1X,28HBIZERO, BIONE, BKZERO, BKONE)
 679  FORMAT (
     1  1X,32HEXIZERO, EXIONE, EXKZERO, EXKONE)
 680  FORMAT (
     1  1X,32HKBIZERO, KBIONE, KBKZERO, KBKONE)
 681  FORMAT (
     1  1X,36HKEXIZERO, KEXIONE, KEXKZERO, KEXKONE)
 682  FORMAT (
     1  1X,28HCIZERO, CIONE, CKZERO, CKONE)
 683  FORMAT (
     1  1X,32HCEIZERO, CEIONE, CEKZERO, CEKONE)
 684  FORMAT (
     1  1X,25HZEROS BJZERO, ZEROS BJONE)
 685  FORMAT (
     1  1X,19HBESIN, BESJN, BESKN)
 686  FORMAT (
     1  1X, 5HINTJO)
C
C     (13)   THERMODYNAMICS.
C
 687  FORMAT (
     1  1X,10HCTOF, FTOC)
 688  FORMAT (
     1  1X, 7HCGS, SI)
 689  FORMAT (
     1  1X,13HATOMIC, MOLWT)
 690  FORMAT (
     1  1X,26HBOLDISTRIBUTION, EINSTEIN,
     2 /1X,39HPARTFUNCTION, PFATOMIC, PFTRANSLATIONAL)
C
C     ==================================================================
C
      END
*CNTNTS
      SUBROUTINE CNTNTS
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CNTNTS V 7.00  8/27/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTION CONTENTS.
C
C     PRINT SECTION OR SUBSECTION TITLES OR COMMANDS IN PART C.
C
C     IF NARGS = 0, PRINT SECTION TITLES.
C
C     IF NARGS = 1, AN INTEGER, PRINT SUBSECTION TITLES.
C
C     IF NARGS = 1, NOT AN INTEGER, PRINT COMMANDS IN SUBSECTION.
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
C                   CURRENT VERSION -    AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***

      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA N1  /  7 /
      DATA N2  /  8 /
      DATA N3  /  9 /
      DATA N4  /  8 /
      DATA N5  /  6 /
      DATA N6  /  9 /
      DATA N7  /  6 /
      DATA N8  /  6 /
      DATA N9  /  4 /
      DATA N10 /  6 /
      DATA N11 /  7 /
      DATA N12 / 10 /
      DATA N13 /  4 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.LE.IONE) GO TO 10
        CALL ERROR (10)
        RETURN
C
C     ==================================================================
C
  10  IF (NARGS.NE.IZERO) GO TO 20
C
C     PRINT SECTION TITLES.
C
      WRITE (IPRINT,400)
      RETURN
C
C     ..................................................................
C
C     M =    SECTION NUMBER.
C     N = SUBSECTION NUMBER.
C
  20  M = IARGS(1)
      N = IZERO
      IF (KIND(1).EQ.IZERO) GO TO 60
C
C     SUBSECTION IS GIVEN. DETERMINE WHICH ONE.
C
      M = IZERO
      DO 30 I = 3,83
        IF (KARD(I).LT.ITEN) GO TO 40
        IF (KARD(I).EQ.37 .AND. KARD(I+1).LT.ITEN) GO TO 60
  30  CONTINUE
      GO TO 60
C
  40  M = KARD(I)
      I = I + IONE
      IF (KARD(I).EQ.37) GO TO 50
      M = ITEN * M + KARD(I)
      I = I + IONE
  50  I = I + IONE
      IF (KARD(I).LT.ITEN) N = KARD(I)
      IF (KARD(I+1).LT.ITEN) N = ITEN * N + KARD(I+1)
C
C
  60  IF (M.GT.IZERO .AND. M.LT.14) GO TO 70
        CALL ERROR (3)
        RETURN
C
C     ..................................................................
C
  70  IF (N.NE.IZERO) GO TO 200
C
C     PRINT SUBSECTION TITLES.
C
      WRITE (IPRINT,500) M
C
      CALL CNTNT1 (M)
C
      WRITE (IPRINT,550)
C
      RETURN
C
C     ..................................................................
C
C     PRINT LIST OF COMMANDS IN SUBSECTION.
C
 200  GO TO (201,202,203,204,205,206,207,208,209,210,
     1       211,212,213), M
C
 201  IF (N.LT.IONE .OR. N.GT.N1 ) GO TO 250
      GO TO 240
C
 202  IF (N.LT.IONE .OR. N.GT.N2 ) GO TO 250
      GO TO 240
C
 203  IF (N.LT.IONE .OR. N.GT.N3 ) GO TO 250
      GO TO 240
C
 204  IF (N.LT.IONE .OR. N.GT.N4 ) GO TO 250
      GO TO 240
C
 205  IF (N.LT.IONE .OR. N.GT.N5 ) GO TO 250
      GO TO 240
C
 206  IF (N.LT.IONE .OR. N.GT.N6 ) GO TO 250
      GO TO 240
C
 207  IF (N.LT.IONE .OR. N.GT.N7 ) GO TO 250
      GO TO 240
C
 208  IF (N.LT.IONE .OR. N.GT.N8 ) GO TO 250
      GO TO 240
C
 209  IF (N.LT.IONE .OR. N.GT.N9 ) GO TO 250
      GO TO 240
C
 210  IF (N.LT.IONE .OR. N.GT.N10) GO TO 250
      GO TO 240
C
 211  IF (N.LT.IONE .OR. N.GT.N11) GO TO 250
      GO TO 240
C
 212  IF (N.LT.IONE .OR. N.GT.N12) GO TO 250
      GO TO 240
C
 213  IF (N.LT.IONE .OR. N.GT.N13) GO TO 250
      GO TO 240
C
 240  IF (N.LT.ITEN) WRITE (IPRINT,600) M, N
      IF (N.GE.ITEN) WRITE (IPRINT,610) M, N
C
      IF (M.GE.IONE  .AND. M.LE.IFOUR) CALL CNTNT2 (M  ,N)
      IF (M.GE.IFIVE .AND. M.LE.8    ) CALL CNTNT3 (M-4,N)
      IF (M.GE.9     .AND. M.LE.13   ) CALL CNTNT4 (M-8,N)
C
      WRITE (IPRINT,620)
C
      RETURN
C
C     ..................................................................
C
 250  CALL ERROR (3)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 400  FORMAT (1X,46HMAJOR SECTIONS OF DESCRIPTION OF INSTRUCTIONS.//
     1        5X,27H 1.   CONTROL INSTRUCTIONS./
     2        5X,33H 2.   ENTERING AND PRINTING DATA./
     3        5X,20H 3.   PLOTTING DATA./
     4        5X,28H 4.   ARITHMETIC OPERATIONS./
     5        5X,24H 5.   DATA MANIPULATION./
     6        5X,27H 6.   STATISTICAL ANALYSIS./
     7        5X,18H 7.   PROBABILITY./
     8        5X,25H 8.   NUMERICAL ANALYSIS./
     9        5X,35H 9.   REPEATED USE OF INSTRUCTIONS./
     A        5X,23H10.   ARRAY OPERATIONS./
     B        5X,24H11.   MATRIX OPERATIONS./
     C        5X,23H12.   BESSEL FUNCTIONS./
     D        5X,21H13.   THERMODYNAMICS.//
     E 1X,62HTO OBTAIN SUBSECTION TITLES, TYPE CONTENTS AND SECTION NUMB
     FER.)
C
C     ..................................................................
C
 500  FORMAT (2X,29HSUBSECTION TITLES IN SECTION ,I2,1H./)
C
 550  FORMAT (1X,60HTO OBTAIN LIST OF COMMANDS, TYPE CONTENTS AND SUBSEC
     1TION NO.)
C
 600  FORMAT (6X,28HLIST OF COMMANDS IN SECTION ,I2,1H.,I1,4H ...)
 610  FORMAT (6X,28HLIST OF COMMANDS IN SECTION ,I2,1H.,I2,4H ...)
C
 620  FORMAT (  / 6X,56HTO OBTAIN FORM OF INSTRUCTION TYPE DESCRIBE AND 
     1COMMAND.)
C
C     ==================================================================
C
      END
*CODEXX
      SUBROUTINE CODEXX (X,N,SUMX,AVEX,XCODE,SQRTCT,U,L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CODEXX V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR CODING X FOR ACCURATELY COMPUTING
C        SUM OF SQUARED DEVIATIONS FROM THE MEAN.
C
C     INPUT PARAMETERS ARE -
C
C            X = VECTOR OF MEASUREMENTS
C            N = LENGTH OF X
C
C     OUPUT PARAMETERS ARE -
C
C         SUMX = DOUBLE PRECISION SUM OF X MEASUREMENTS
C         AVEX = SINGLE PRECISION AVERAGE OF THE X MEASUREMENTS
C        XCODE = CODED VALUE TO BE USED INSTEAD OF AVERAGE FOR
C                   CUMPUTING DEVIATIONS ABOUT THE MEAN.
C                   XCODE IS THE VALUE OF X(I) CLOSEST TO AVEX.
C       SQRTCT = SQUARE ROOT OF CORRECTION TERM FOR COMPUTING
C                   SUM OF SQUARED DEVIATIONS ABOUT THE MEAN.
C
C                   SUM (X-AVEX)**2 = SUM(X-CODEX)**2 - SQRTCT**2,
C
C                   WHERE SQRTCT = (SUMX-N*XCODE)/SQRT(N)
C
C         U(I) = X(I) -XCODE, = CODED VALUES OF X
C            L = VALUE OF I FOR WHICH XCODE = X(I).
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT CODEXY.
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(*), U(*)
      REAL             AVEX, DELTA, XCODE
      REAL             FDPCON
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DN, SQRTCT, SUMX
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DX(1)
C
C     ==================================================================
C
C     COMPUTE AVEX.
C
      CALL DSUMAL (DX,IZERO,SUMX)
      DO 10 I=1,N
        DX(1) = DBLE ( X(I) )
        CALL DSUMAL (DX,-IONE,SUMX)
  10  CONTINUE
      CALL DSUMAL (DX,IONE,SUMX)
C
      DN = N
C
      AVEX = FDPCON ( FDDIV (SUMX,DN,IND) )
C
C     COMPUTE XCODE AND L.
C
      L = IONE
      DELTA = ABS (X(1)-AVEX)
      DO 30 I=2,N
        IF (ABS(X(I)-AVEX)-DELTA) 20,30,30
  20    L = I
        DELTA = ABS (X(I)-AVEX)
  30    CONTINUE
C
      XCODE = X(L)
C
C     COMPUTE CODED X = (X-XCODE).
C
      DO 40 I=1,N
        U(I) = X(I) - XCODE
  40  CONTINUE
C
C     COMPUTE CORRECTION TERM
C        FOR COMPUTING SUMX OF DEVIATIONS ABOUT THE MEAN.
C
      SQRTCT = FDDIV (SUMX-DN*DBLE(XCODE),FDSQRT(DN),IND)
C
      RETURN
C
C     ==================================================================
C
      END
*CODEXY
      SUBROUTINE CODEXY (X,N,SUMX,AVEX,XCODE,SQRTCT,U,L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CODEXY V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR CODING X FOR ACCURATELY COMPUTING
C        SUM OF SQUARED DEVIATIONS FROM THE MEAN.
C
C     INPUT PARAMETERS ARE -
C
C            X = VECTOR OF MEASUREMENTS
C            N = LENGTH OF X
C
C     OUPUT PARAMETERS ARE -
C
C         SUMX = DOUBLE PRECISION SUM OF X MEASUREMENTS
C         AVEX = SINGLE PRECISION AVERAGE OF THE X MEASUREMENTS
C        XCODE = CODED VALUE TO BE USED INSTEAD OF AVERAGE FOR
C                   CUMPUTING DEVIATIONS ABOUT THE MEAN.
C                   XCODE IS THE VALUE OF X(I) CLOSEST TO AVEX.
C       SQRTCT = SQUARE ROOT OF CORRECTION TERM FOR COMPUTING
C                   SUM OF SQUARED DEVIATIONS ABOUT THE MEAN.
C
C                   SUM (X-AVEX)**2 = SUM(X-CODEX)**2 - SQRTCT**2,
C
C                   WHERE SQRTCT = (SUMX-N*XCODE)/SQRT(N)
C
C         U(I) = X(I) -XCODE, = CODED VALUES OF X
C            L = VALUE OF I FOR WHICH XCODE = X(I).
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(*), U(*)
      REAL             AVEX, DELTA, XCODE
      REAL             FDPCON
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DN, SQRTCT, SUMX
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DX(1)
C
C     ==================================================================
C
C     COMPUTE AVEX.
C
      CALL DSUMAL (DX,IZERO,SUMX)
      DO 10 I=1,N
        DX(1) = DBLE ( X(I) )
        CALL DSUMAL (DX,-IONE,SUMX)
  10  CONTINUE
      CALL DSUMAL (DX,IONE,SUMX)
C
      DN = N
C
      AVEX = FDPCON ( FDDIV (SUMX,DN,IND) )
C
C     COMPUTE XCODE AND L.
C
      L = IONE
      DELTA = ABS (X(1)-AVEX)
      DO 30 I=2,N
        IF (ABS(X(I)-AVEX)-DELTA) 20,30,30
  20    L = I
        DELTA = ABS (X(I)-AVEX)
  30    CONTINUE
C
      XCODE = X(L)
C
C     COMPUTE CODED X = (X-XCODE).
C
      DO 40 I=1,N
        U(I) = X(I) - XCODE
  40  CONTINUE
C
C     COMPUTE CORRECTION TERM
C        FOR COMPUTING SUMX OF DEVIATIONS ABOUT THE MEAN.
C
      SQRTCT = FDDIV (SUMX-DN*DBLE(XCODE),FDSQRT(DN),IND)
C
      RETURN
C
C     ==================================================================
C
      END
*COEF
      SUBROUTINE COEF (R2,MP,KZ,XI,RR,IND,NDEF,M,ND,MD,NL,IB,ZC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   COEF V 7.00  8/27/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                     COMPUTES REGRESSION STATISTICS
C
C ******************************************************************** *
C                                                                      *
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C ******************************************************************** *
C
C               MODIFIED TO PFORT BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -    AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IND(ND), MD(ND,ND), NALPHA(15), NOUT(12)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             RR(29,29), XI(NL), ZC(ND)
      REAL             DBET, F, R2, VAR
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER NALPHA*1, NOUT*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NOUT( 1), NOUT( 2), NOUT( 3), NOUT( 4), NOUT( 5), NOUT( 6) /
     1          'R',      '*',      '*',      '2',      'R',      '*' /
      DATA NOUT( 7), NOUT( 8), NOUT( 9), NOUT(10), NOUT(11), NOUT(12) /
     1          '*',      '2',      'C',      '(',      'P',      ')' /
C
C     IF THE FOLLOWING VALUE IS CHANGED,
C        THE DIMENSION OF NALPHA MUST BE CHANGED AND
C        15A1 MUST BE CHANGED IN FORMAT 70.
C
      DATA NX / 15 /
C
C
C     ==================================================================
C
      IEND = IFOUR * IB
      IBEG = IEND - ITHRE
      WRITE (IPRINT,60) (NOUT(I),I=IBEG,IEND), R2
C
C                             FORM SUBMATRIX
C
        IND(MP) = KZ
      DO 20 I=1,MP
        DO 10 J=I,MP
          ISUB1 = MD(I,J)
          ISUB2 = IND(I)
          ISUB3 = IND(J)
          XI(ISUB1) = RR(ISUB2,ISUB3)
  10    CONTINUE
  20  CONTINUE
C
C                            INVERT SUBMATRIX
C
      DO 30 N=1,M
        NN = N
        CALL PIVOT (XI,MP,NN,MD,ND,NL)
  30  CONTINUE
C
      ISUB4 = MD(MP,MP)
      VAR = FDIV (XI(ISUB4),FLOAT(NDEF-M),IF)
C
      DO 40 I=1,M
        ISUB5 = MD(I,MP)
        ZC(I) = -XI(ISUB5)
 40   CONTINUE
C
      CALL RFORMT (0,ISIGD,ZC,XI(1), M,NX,LW,LD,NALPHA(1),IRF)
      LB = NX - LW
C
      DO 50 I=1,M
        DBET = ZC(I)
        ISUB6 = MD(I,I)
        CALL RFORMT (1,ISIGD,XI,ZC(I),LB, 1,LW,LD,NALPHA(1),IRF)
        F = -DBET*FDIV (DBET,XI(ISUB6)*VAR,IF)
        WRITE (IPRINT,70) IND(I), (NALPHA(J),J=1,NX), F
  50  CONTINUE
C
C       WRITE (IPRINT,80)
        RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
  60  FORMAT (  / 18X,4A1,3H = ,F7.3/1H ,6X,8HVARIABLE,6X,11HCOEFFICIENT
     1   ,7X,7HF RATIO)
  70  FORMAT (1H , 9X,I2,7X,15A1,5X,F7.3)
C 80  FORMAT (1H )
C
C     ==================================================================
C
        END
*COMELL
      DOUBLE PRECISION FUNCTION COMELL (Z,I)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82. COMELL V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C       COMPLETE ELLIPTIC INTEGRALS - FIRST AND SECOND KIND
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION A, B, C, D, E, P, Q, X, Z
      DOUBLE PRECISION FDDIV, FDLOG, FDSQRT
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF
      DOUBLE PRECISION DPCG, DPCH, DPCI, DPCJ, DPCK, DPCL
      DOUBLE PRECISION DPCM, DPCN, DPCO, DPCP, DPCQ
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NTIMES / 150 /
C
      DATA DPCA /     0.996D0               /
      DATA DPCB /     1.2D0                 /
      DATA DPCC /     0.25D0                /
      DATA DPCD /     1.66666666666666667D0 /
      DATA DPCE /  1255.0D0                 /
      DATA DPCF /   175.0D0                 /
      DATA DPCG /  2048.0D0                 /
      DATA DPCH /     1.08333333333333333D0 /
      DATA DPCI /     0.140625D0            /
      DATA DPCJ / 16384.0D0                 /
      DATA DPCK /     1.23333333333333333D0 /
      DATA DPCL /     9.765625D-2           /
      DATA DPCM /     0.1875D0              /
      DATA DPCN /     0.1171875D0           /
      DATA DPCO /     1.27904761904D0       /
      DATA DPCP /     0.1D-9                /
      DATA DPCQ /     1.251190476D0         /
C
C     ==================================================================
C
      X = Z
      IF (DABS(Z)-DONE) 30,10,20
  10  IF (I.EQ.ITWO) GO TO 30
  20  CALL ERROR (109)
      COMELL = DZERO
      RETURN
C
C     ..................................................................
C
  30  A = X
      B = FDSQRT (DONE-A)
      IF (X-DPCA) 40,40,90
  40  B = FDDIV (DONE-B,DONE+B,IND)
      A = B**2
      B = DONE + B
      C = DONE
      D = C
      E = DTWO
      IF (I.EQ.IONE) GO TO 50
      B = FDDIV (DONE,B,IND)
      D = -DONE
  50  P = A
      DO 70 N=1,NTIMES
        C = C + P * FDDIV (D,E,IND) ** 2
        P = P * A * FDDIV (D,E,IND) ** 2
        IF (P-DPCP) 80,60,60
  60    D = D + DTWO
        E = E + DTWO
  70  CONTINUE
C
  80  A = B * C * DHLFPI
      GO TO 140
C
  90  IF (B-DZERO) 110,100,110
 100  A = DZERO
      CALL ERROR (101)
      GO TO 120
C
 110  A = FDLOG ( FDDIV (DFOR,B,IND) )
 120  Q = B**2
      IF (I.GT.IONE) GO TO 130
      B = DPCC * (A-DONE)
      C = DPCI * (A-DPCD)
      D = DPCL * (A-DPCK)
      E = DPCE * FDDIV (A-DPCO,DPCJ,IND)
      A = A + Q*(B + Q*(C + Q*(D + Q*E)))
      GO TO 140
C
 130  B = DHALF * (A-DHALF)
      C = DPCM * (A-DPCH)
      D = DPCN * (A-DPCB)
      E = DPCF * FDDIV (A-DPCQ,DPCG,IND)
      A = DONE + Q * (B+Q*(C+Q*(D+Q*E)))
C
 140  COMELL = A
      RETURN
C
C     ==================================================================
C
      END
*CONVRT
      SUBROUTINE CONVRT (NME,M)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. CONVRT V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE CONVERTS INTEGER REPRESENTATION, NME, OF THREE CHARACTER
C        WORD INTO THREE CHARACTER WORD STORED IN M(1), M(2) AND M(3).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1975.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION M(*)
      DIMENSION LNAME(2)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER   LA*1
      CHARACTER   M*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MLTPER / 100000 /
C
      DATA ICA / 729 /
      DATA ICB /  27 /
C
C     ==================================================================
C
      LNAME(1) = IDIV (NME,MLTPER,IND)
      LNAME(2) = MOD (NME,MLTPER)
C
      K = IONE
      DO 10 I=1,2
        M1     = IDIV (LNAME(I),ICA,IND)
        MS     = MOD  (LNAME(I),ICA)
        M2     = IDIV (MS,ICB,IND)
        M3     = MOD  (MS,ICB)
        M(K  ) = LA(M1+10)
        M(K+1) = LA(M2+10)
        M(K+2) = LA(M3+10)
        IF (M1.EQ.IZERO) M(K  ) = LA(45)
        IF (M2.EQ.IZERO) M(K+1) = LA(45)
        IF (M3.EQ.IZERO) M(K+2) = LA(45)
        K      = K + ITHRE
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CPSTRE
      SUBROUTINE CPSTRE (RSS,CAB,KO,CL,RM,N,NS,ND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CPSTRE V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                  SAVES RSS:S AND LABELS FOR BEST REGRESSIONS
C ******************************************************************** *
C                                                                      *
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C ******************************************************************** *
C
C               MODIFIED TO PFORT BY -
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             CL(11,ND), RM(11,ND)
      REAL             CAB, RSS
C
C     ==================================================================
C
      DO 10 L=1,KO
        IF (CAB.EQ.CL(L,N)) RETURN
  10  CONTINUE
C
      L = IZERO
  20  L = L + IONE
        IF (RSS.GT.RM(L+1,N)) GO TO 30
        RM(L,N) = RM(L+1,N)
        CL(L,N) = CL(L+1,N)
        IF (L.EQ.NS) GO TO 30
      GO TO 20
C
  30  RM(L,N) = RSS
      CL(L,N) = CAB
      RETURN
C
C     ==================================================================
C
      END
*CRSPRD
      SUBROUTINE CRSPRD (X,N,M,INTCPT,CTERM,CP)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CRSPRD V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT FOR COMPUTING A CROSS PRODUCT OF DEVIATIONS ABOUT
C        MEAN MATRIX, CP().
C
C        INPUT X(N,M)
C              N = NUMBER OF MEASUREMENTS
C              M = NUMBER OF VARIABLES.
C         INTCPT = 0, CROSS PRODUCTS ABOUT ORIGIN ARE COMPUTED
C                = 1, CROSS PRODUCTS ABOUT MEAN   ARE COMPUTED.
C
C        STORAGE CONST(M).
C
C        OUTPUT CP(M,M) = CROSS PRODUCT MATRIX.
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(N,*)
      REAL             CP(29,29)
      REAL             AVEX, XCODE
      REAL             FDPCON
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION CTERM(*)
      DOUBLE PRECISION F, SUMNEG, SUMPOS, SUMX
C
C     ==================================================================
C
C     BEGIN COMPUTING.
C
C     COMPUTE CORRECTION TERM, CTERM(I), AND CODE X(I,J).
C
      IF (INTCPT.EQ.IONE) GO TO 20
      DO 10 I= 1,M
        CTERM(I) = DZERO
  10  CONTINUE
      GO TO 40
C
  20  DO 30 I=1,M
        CALL CODEXY (X(1,I),N,SUMX,AVEX,XCODE,CTERM(I),X(1,I),L)
  30  CONTINUE
C
C     COMPUTE (N-1)*VARIANCES.
C
  40  DO 60 I=1,M
        SUMPOS = DZERO
        SUMNEG = DZERO
        DO 50 J=1,N
          F = X(J,I)
          F = F**2
          SUMPOS = SUMPOS + DMAX1 (DZERO, F)
          SUMNEG = SUMNEG + DMAX1 (DZERO,-F)
  50    CONTINUE
        CP(I,I) = FDPCON ( (SUMPOS - SUMNEG) - CTERM(I)**2 )
  60  CONTINUE
C
C     COMPUTE CROSS PRODUCT MATRIX.
C
      IEND = M-IONE
      DO 90 I=1,IEND
        JBEG = I + IONE
        DO 80 J=JBEG,M
          SUMPOS = DZERO
          SUMNEG = DZERO
          DO 70 K=1,N
            F = DBLE(X(K,I))*DBLE(X(K,J))
            SUMPOS = SUMPOS + DMAX1 (DZERO, F)
            SUMNEG = SUMNEG + DMAX1 (DZERO,-F)
  70      CONTINUE
          CP(I,J) = FDPCON ( (SUMPOS - SUMNEG) - CTERM(I)*CTERM(J) )
          CP(J,I) = CP(I,J)
  80    CONTINUE
  90  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*CTCCDF
      SUBROUTINE CTCCDF (X,NU,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82. CTCCDF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--CTNCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGE 176,
C                 FORMULA 28, AND PAGE 180, FORMULA 33.1.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 50-55.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 122-131.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --MAY       1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --OCTOBER   1976.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, CDF
      REAL             AMEAN, ANU, CDFN, DANU, SD, SPCHI, U, Z
      REAL             SPCA, SPCB
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION TERM(5)
      DOUBLE PRECISION DX, CHI, SUM, AI, DCDFN
      DOUBLE PRECISION DNU
      DOUBLE PRECISION DFACT, DPOWER
      DOUBLE PRECISION DW
      DOUBLE PRECISION D1, D2, D3
      DOUBLE PRECISION B11, B21
      DOUBLE PRECISION B31, B32, B41, B42, B43
      DOUBLE PRECISION DPCA
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG, FDSQRT
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NUCUT / 1000 /
C
      DATA SPCA / 200.0 /
      DATA SPCB / 100.0 /
C
      DATA DPOWER /   0.33333333333333D0 /
      DATA B11    /   0.33333333333333D0 /
      DATA B21    /  -0.02777777777778D0 /
      DATA B31    /  -0.00061728395061D0 /
      DATA B32    / -13.0D0              /
      DATA B41    /   0.00018004115226D0 /
      DATA B42    /   6.0D0              /
      DATA B43    /  17.0D0              /
C
      DATA DPCA   /  4.5D0               /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IND = IZERO
      IF (NU.GT.IZERO) GO TO 10
        IND = IONE
        GO TO 30
  10  IF (X.GE.RZERO) GO TO 20
        IND = IONE
        GO TO 30
C
C     ---   START POINT   ----------------------------------------------
C
  20  DX = X
      ANU = NU
      DNU = NU
C
C     IF X IS NON-POSITIVE, SET CDF  =  0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF  =  0.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF  =  0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF  =  1.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF  =  1.0 AND RETURN.
C
      IF (X.LE.RZERO) GO TO 30
      AMEAN = ANU
      SD = FSQRT (RTWO*ANU)
      Z = FDIV (X-AMEAN,SD,JIND)
      IF (NU.LT.ITEN .AND. Z.LT.(-SPCA)) GO TO 30
      IF (NU.GE.ITEN .AND. Z.LT.(-SPCB)) GO TO 30
      IF (NU.LT.ITEN .AND. Z.GT.SPCA) GO TO 40
      IF (NU.GE.ITEN .AND. Z.GT.SPCB) GO TO 40
      GO TO 50
C
  30  CDF = RZERO
      RETURN
C
C     ..................................................................
C
  40  CDF = RONE
      RETURN
C
C     ..................................................................
C
C
C     DISTINGUISH BETWEEN 3 SEPARATE REGIONS
C     OF THE (X,NU) SPACE.
C     BRANCH TO THE PROPER COMPUTATIONAL METHOD
C     DEPENDING ON THE REGION.
C     NUCUT HAS THE VALUE 1000.
C
  50  IF (NU.LT.NUCUT) GO TO 60
      IF (NU.GE.NUCUT .AND. X.LE.ANU) GO TO 120
      IF (NU.GE.NUCUT .AND. X.GT.ANU) GO TO 130
      IND = ITWO
      RETURN
C
C     ..................................................................
C
C     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE
C     (THAT IS, WHEN NU IS SMALLER THAN 1000).
C     METHOD UTILIZED--EXACT FINITE SUM
C     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5).
C
  60  CHI = FDSQRT (DX)
      IEVODD = NU - ITWO * IDIV (NU,ITWO,JIND)
      CALL DSUMAL (TERM,IZERO,SUM)
      IF (IEVODD.EQ.IZERO) GO TO 70
C
      SUM     = DZERO
      TERM(1) = FDDIV (DONE,CHI,JIND)
      IMIN    = IONE
      IMAX    = NU - IONE
      GO TO 80
C
  70  SUM     = DONE
      TERM(1) = DONE
      CALL DSUMAL (TERM,-IONE,SUM)
      IMIN = ITWO
      IMAX = NU - ITWO
C
  80  IF (IMIN.GT.IMAX) GO TO 100
      DO 90 I=IMIN,IMAX,2
        AI      = I
        TERM(1) = TERM(1) * FDDIV (DX,AI,JIND)
        CALL DSUMAL (TERM,-IONE,SUM)
  90  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 100  SUM = SUM * FDEXP (FDDIV (-DX,DTWO,JIND))
      IF (IEVODD.EQ.IZERO) GO TO 110
      SUM = FDSQRT (FDDIV(DTWO,DPI,JIND)) * SUM
      SPCHI = CHI
      CALL CTNCDF (SPCHI,CDFN)
      DCDFN = CDFN
      SUM = SUM + DTWO * (DONE-DCDFN)
 110  CDF = FDPCON (DONE - SUM)
      RETURN
C
C     ..................................................................
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LESS THAN OR EQUAL TO NU.
C     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28).
C
 120  DFACT = DPCA * DNU
      U = FDPCON ( ((FDDIV(DX,DNU,JIND)**DPOWER)-DONE+
     1      FDDIV(DONE,DFACT,JIND))*FDSQRT(DFACT) )
      CALL CTNCDF (U,CDFN)
      CDF = CDFN
      RETURN
C
C     ..................................................................
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LARGER THAN NU.
C     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1).
C
 130  DW      = FDSQRT (DX-DNU-DNU*FDLOG(FDDIV(DX,DNU,JIND)))
      DANU    = FDSQRT (FDDIV (DTWO,DNU,JIND))
      D1      = DW
      D2      = DW**2
      D3      = DW**3
      TERM(1) = DW
      TERM(2) = B11 * DANU
      TERM(3) = B21 * D1 * (DANU**2)
      TERM(4) = B31 * (D2+B32) * (DANU**3)
      TERM(5) = B41 * (B42*D3+B43*D1) * (DANU**4)
      CALL DSUMAL (TERM,IFIVE,SUM)
      U = FDPCON(SUM)
      CALL CTNCDF (U,CDFN)
      CDF = CDFN
      RETURN
C
C     ==================================================================
C
      END
*CTNCDF
      SUBROUTINE CTNCDF (X,CDF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CTNCDF V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT NORCDF.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1978.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, CDF
      REAL             B1, B2, B3, B4, B5, P
      REAL             T, Y, Z
      REAL             FDIV, FEXP
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA B1 /  0.319381530 /
      DATA B2 / -0.356563782 /
      DATA B3 /  1.781478000 /
      DATA B4 / -1.821256000 /
      DATA B5 /  1.330274429 /
C
      DATA P  /  0.2316419   /
C
      DATA SPCA / 0.3989423 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C     NO INPUT ARGUMENT ERRORS POSSIBLE
C     FOR THIS DISTRIBUTION.
C
C     ---   START POINT   ----------------------------------------------
C
      Y = X
      Z = X
      IF (Y.LT.RZERO) Z = -Z
      T = FDIV (RONE,RONE+P*Z,JIND)
      CDF = RONE - (SPCA * FEXP (-RHALF*Z*Z)) * (B1*T+B2*T**2+B3*T**3 +
     1      B4*T**4+B5*T**5)
      IF (Y.LT.RZERO) CDF = RONE - CDF
C
      RETURN
C
C     ==================================================================
C
      END
