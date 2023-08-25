*MAINSL
      SUBROUTINE MAINSL (D,S,N,IE,I,J,K,L,IT,IU,LD,IL,IH,NH,SC,DH,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MAINSL V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DIAGRAM SHOWING FLOW OF THE 12 PROGRAM UNITS
C        AND ONE BLOCK DATA PROGRAM UNIT
C        IN THE STEM AND LEAF PACKAGE
C        DEVELOPED BY DAVID HOGBEN, S. T. PEAVY, AND W. L. NICHOLSON.
C           FEBRUARY, 1974.
C
C                       (-------)
C                       (CALLING)
C                       (PROGRAM)
C                       (-------)
C                           *
C                           *
C                           *
C                   (--------------)
C                   (    MAINSL    )
C                   (--------------)
C                    *      *    *
C                   *       *     *
C                  1       (2)     3
C                 *         *       *
C                *          *        *
C             (------)   (----)   (-----)
C             (PERSAL)   (RULE)***(SANDL)
C             (------)   (----)   (-----)
C                *                   *
C
C           ******              ******
C           *                   *
C           * (----)            *
C           *-(SORT)            *-*
C           * (----)            */
C           *                   *
C           *                  /*
C           * (----)          / * (------)
C           *-(INZP)         /  *-(RNDATM)
C           * (----)        /   * (------)
C           *              /    *
C           *             /     *
C           * (------)   /      * (------) (----)
C           *-(SCRAWL)  /       *-(CINDEX)-(ATOI)
C           * (------) /        * (------) (----)
C           *         /         *
C           *        /          *
C           * (------)          * (------)
C           *-(RFORMT)          *-(IXLINE)
C           * (------)          * (------)
C
C     PRINT J. W. TUKEY STEM AND LEAF DISPLAY
C
C     REFERENCE ---
C        EXPLORATORY DATA ANALYSIS,
C        J. W. TUKEY. (1977)
C        ADDISON-WESLEY.
C
C        ***** PARAMETERS ****
C         IN CALLING SEQUENCE
C
C        DATA
C
C      1. D = ORIGINAL VECTOR OF
C           DATA WHICH WILL BE SORTED
C           AND ABSOLUTE VALUES TAKEN.
C      2. SCRATCH = SCRATCH VECTOR
C           OF LENGTH N OR GREATER.
C      3. N = NUMBER OF MEASUREMENTS IN
C           DATA VECTOR, MUST BE
C           GREATER THAN 4.
C
C        PARAMETERS FOR RULE
C
C      4. IE = 0, I,J,K,L SPECIFIED
C               = 1, I,J,K,L COMPUTED
C      5. I = 0, MIXED STEM AND LEAF
C          NE 0, SIMPLE STEM AND LEAF
C      6. J = NUMBER OF SIGNIFICANT
C              DIGITS IN LARGEST STEM,
C              AND IS INCREASED BY 1 IF
C              DECIMAL POINT IS WITHIN
C              THE STEM
C      7. K = CELL WIDTH, FOR
C               I = 0, K = 1 OR 3
C                NE 0, K = 1,2,4 OR 10
C           K =  1, 2 STEMS/LINE I NE 0
C                   3 STEMS/LINE I = 0
C                2, STEM ON 1 LINE
C                3, STEM ON 1 LINE
C                4, STEM ON 2 LINES
C               10, STEM ON 5 LINES
C      8. L = NUMBER OF DIGITS IN LEAF
C
C        PRINTING
C
C      9. IT = 0, DISPLAY NOT PRINTED
C                1, DISPLAY PRINTED. IF
C                    IE = 0, AT MOST
C                    MXLIN LINES
C                N, DISPLAY PRINTED, NO
C                    MORE THAN N LINES
C                    IF IE = 0
C     10. IU = LOGICAL UNIT FOR PRINT
C     11. LD = MAXIMUM NUMBER OF
C                  CHARACTERS PER LINE
C
C        STORING
C
C     12. IL = 0, SCRAWL NOT STORED
C                  1, SCRAWL STORED
C     13. IH = 0, DEPTH NOT STORED
C                  1, DEPTH STORED
C     14. NH  = DIMENSION OF DEPTH
C     15. SC = VECTOR FOR STORING
C           SCRAWL, IF IL=1, MUST
C           BE DIMENSIONED 8 OR MORE
C     16. DH= VECTOR FOR STORING
C           DEPTH, IF IH=1.
C           DH(1) = M = NUMBER OF
C           VALUES STORED IN DH(2),
C           DH(3),...,DH(M+1).
C
C        FAULT INDICATOR
C
C     17. IND IS FAULT INDICATOR
C           IND = 0, SUCCESS
C                 1, N LESS THAN 5
C                 2, ALL DATA EQUAL OR
C                     ALL STEMS EQUAL
C                 3, FIELD WIDTH,MXWDTH
C                     NOT LARGE ENOUGH-
C                     CHANGE IN SLDATA
C                     AND CHANGE IB(40)
C                     IN COMMON/STEM/
C                 4, J LT 1, OR
C                     J GE NSIGD
C                 5, L LT 1, OR
C                     L GE NSIGD
C                 6, J+L GT NSIGD
C                 7, K IMPROPER
C                 8, LD IMPROPER -
C                     NOT LARGE ENOUGH
C                     OR TOO LARGE
C                     FOR PRINTING LEAF
C                 9, CANT COMPUTE INDEX
C                10, ROUNDING PROBLEMS
C                -N, NUMBER OF LINES
C                     TO BE PRINTED
C                     EXCEEDS SPECIFIED
C                     MAXIMUM, (N)
C                     STEMS NOT PRINTED
C
C     QUESTIONS CAN BE ANSWERED BY
C        DR. DAVID HOGBEN, (301) 921-2315
C
C     CHARACTER, INTERNAL OCTAL CODE,
C        AND PUNCHED CARD CODE USED IN
C        THE STEM AND LEAF PACKAGE.
C
C           A 06 12-1   M 22 11-4   Y 36  0-8
C           B 07 12-2   N 23 11-5   Z 37  0-9
C           C 10 12-3   O 24 11-6   0 60    0
C           D 11 12-4   P 25 11-7   1 61    1
C           E 12 12-5   Q 26 11-8   2 62    2
C           F 13 12-6   R 27 11-9   3 63    3
C           G 14 12-7   S 30  0-2   4 64    4
C           H 15 12-8   T 31  0-3   5 65    5
C           I 16 12-9   U 32  0-4   6 66    6
C           J 17 11-1   V 33  0-5   7 67    7
C           K 20 11-2   W 34  0-6   8 70    8
C           L 21 11-3   X 35  0-7   9 71    9
C
C           BLANK               05  NOTHING
C           EQUALS              44     3-8
C           PLUS                42  12
C           MINUS               41  11
C           ASTERISK            50  11-4-8
C           SLASH               74     0-1
C           LEFT PARENTHESIS    51   0-4-8
C           RIGHT PARENTHESIS   40  12-4-8
C           COMMA               56   0-3-8
C           DECIMAL POINT       75  12-3-8
C
C     ALL PRINTING IS DONE BY PROGRAM UNIT SANDL.
C
C     IN A MIXED STEM-AND-LEAF, (R)
C        ASTERISKS (IF ANY) INDICATE
C        THAT THE FIRST (R) DIGITS OF A
C        LEAF ARE ACTUALLY PART OF THE
C        STEM. E.G., 5* ( 082 IS USED
C        INSTEAD OF 50 ( 82
C
C     IF A CONTINUATION LINE IS NEEDED
C        TO PRINT ALL THE LEAVES
C        ASSOCIATED WITH A STEM, THE
C        DEPTH IS PRINTED ON THE LAST
C        CONTINUATION LINE, THE SCRAWL
C        IS PRINTED AND THE STEM IS NOT
C        PRINTED ON CONTINUATION LINES.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1974.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
C     PERSAL AND SANDL LABELED COMMON
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ
C
C     LABELED COMMON FOR RULE AND SANDL
C
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SLRVAR/ RI(5), SV(6)
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             D(*), DH(*), S(*), SC(*)
      REAL             FLOG10
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 50 /
C
C     ==================================================================
C
      IND = IZERO
      IF (N.GE.IFIVE) GO TO 10
      IND = IONE
      RETURN
C
C     ..................................................................
C
  10  IOUT   = IU
      IPET   = IT
      LUPPER = LD
      IF (LUPPER.GT.LWIDE) GO TO 40
      NZ     = N
      IDTHST = IZERO
      CALL PERSAL (D,S,NZ,IND)
      IF (IND.NE.IZERO) RETURN
      IF (IL.EQ.IZERO) GO TO 20
      SC(1) = NZ
      SC(2) = SV(2)
      SC(3) = D(2)
      IF (IN.GE.ITWO) SC(3) = -SC(3)
      SC(4) = SV(3)
      SC(5) = SV(4)
      SC(6) = SV(5)
      SC(7) = D(NZ-1)
      IF (IN.GE.NZ-IONE) SC(7) = -SC(7)
      SC(8) = SV(6)
  20  JLSWT = IONE
      IF (IH.GT.IZERO) IDTHST = NH
      IF (IT.EQ.IZERO .AND. IH.EQ.IZERO) RETURN
      IF (IE.EQ.IZERO) GO TO 30
      IF (IT.GT.IZERO) IPET = ICA * IFIX (FLOG10 (FLOAT(NZ)) )
      CALL RULE (D,DH,IE,IND)
      RETURN
C
C     ..................................................................
C
  30  ILEAF = I
      JZ = J
      KZ = K
      LZ = L
      CALL SANDL (D,DH,IE,IND)
      RETURN
C
C     ..................................................................
C
  40  IND = 8
      RETURN
C
C     ==================================================================
C
      END
*MCHROW
      SUBROUTINE MCHROW (X,N,M,IROW,JROW,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MCHROW V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO DETERMINE WHETHER
C        X(IROW,K) = X(JROW,K) FOR ALL K =1, 2, ..., M.
C
C     IND = 0, IF ROW IROW IS NOT IDENTICAL TO ROW JROW,
C         = 1, IF ROW IROW IS     IDENTICAL TO ROW JROW.
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
C
      REAL             X(N,M)
C
C     ==================================================================
C
      IND = IZERO
C
      DO 10 K=1,M
        IF (X(IROW,K)-X(JROW,K)) 20,10,20
  10  CONTINUE
C
      IND = IONE
C
  20  RETURN
C
C     ==================================================================
C
      END
*MEDCI
      SUBROUTINE MEDCI (N,KLOWER,KUPPER)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MEDCI V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE ORDER STATISTICS FOR A DISTRIBUTION FREE 95 PERCENT
C        CONFIDENCE INTERVAL FOR THE MEDIAN FOR A SAMPLE OF SIZE N.
C
C     THE PROBABILITY IS APPROXIMATELY 0.95 THAT THE POPULATION MEDIAN
C        WILL LIE BETWEEN X(KLOWER) AND X(KUPPER).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A, AN, B, BZERO, F, HALFSL, PTONE, Q
      REAL             V1, V2, X, XZERO
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA HALFSL / 0.025 /
      DATA PTONE  / 0.1   /
C
C     ==================================================================
C
      IF (N.GT.IFOUR) GO TO 10
C
C     95 PERCENT CONFIDENCE INTERVAL DOES NOT EXIST UNLESS N .GE. 5
C        ZERO RETURNED FOR KLOWER AND KUPPER.
C
        KLOWER = IZERO
        KUPPER = IZERO
        RETURN
C
C     ..................................................................
C
  10  AN = FLOAT (N)
      XZERO = FDIV (AN,RTWO,IND) - FSQRT (AN) - RHALF
      XZERO = AMAX1 ( RZERO,AINT(XZERO) )
      F  = FDIV (XZERO+RONE,AN-XZERO,IND)
      V1 = RTWO * (AN-XZERO)
      V2 = RTWO * (XZERO+RONE)
      CALL QFORF (V1,V2,F,Q)
      B = RONE-Q
      IF (B-HALFSL) 20,100,30
  20  X = XZERO + RONE
      GO TO 40
  30  X = XZERO - RONE
      GO TO 40
  40  BZERO = B
      F  = FDIV (X+RONE,AN-X,IND)
      V1 = RTWO * (AN-X)
      V2 = RTWO * (X+RONE)
      CALL QFORF (V1,V2,F,Q)
      B = RONE - Q
      IF (BZERO-HALFSL) 50,100,70
  50  IF (B -HALFSL) 60,110,90
  60  XZERO = X
      X = X + RONE
      GO TO 40
  70  IF (B -HALFSL) 90,110,80
  80  XZERO = X
      X = X - RONE
      GO TO 40
  90  A = ABS (BZERO-HALFSL)
      B = ABS (B -HALFSL)
      IF (A-B) 100,110,110
 100  X = XZERO
 110  KLOWER = X + (RONE+PTONE)
      KUPPER = N + IONE - KLOWER
      RETURN
C
C     ==================================================================
C
      END
*MINNW
      SUBROUTINE MINNW (X,N,NS,NX,NALPHA,NR,NW,ND,MW,MD)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.  MINNW V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO DETERMINE THE MINIMUM VALUE OF NW FROM RFORMT
C       NEEDED TO PRINT, WHEN TRAILING ZEROS ARE OMITTED.
C
C     INPUT PARAMETERS ARE -
C
C             X = ARRAY OF MEASUREMENTS
C             N = LENGTH OF X
C            NS = NUMBER OF SIGNIFICANT DIGITS ALLOWED
C            NX = MAXIMUM WIDTH ALLOWED
C        NALPHA = ARRAY FOR STORING ALPHA REPRESENTATION OF X(I)
C            NR = 0, ROUNDING ERROR NOT CONSIDERED (ALL NUMBERS EXACT)
C                 1, ROUNDING ERROR CONSIDERED, IF LAST DIGIT NOT EQUAL
C                       3,4,5,6 OR 7 FOR ALL X(I), I=1,N.
C
C     OUTPUT PARAMETERS ARE -
C
C            NW = NORMAL WIDTH
C            ND = NORMAL NUMBER OF DECIMAL PLACES
C            MW = WIDTH REQUIRED WHEN ONLY SIGNIFICANT DIGITS USED
C            MD = NUMBER OF DECIMAL PLACES WHEN ONLY SIGN. DIGITS USED
C
C     IF ND = 0, MW = NW AND MD = ND.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-28455
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NALPHA(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X(*)
      REAL             B(1)
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        NALPHA*1
C
C     ==================================================================
C
      CALL RFORMT (IZERO,NS,X,B(1),N,NX,NW,ND,NALPHA,IRF)
      IF (ND.GT.IZERO) GO TO 10
        MW = NW
        MD = ND
        RETURN
C
C     ..................................................................
C
  10  M = ND
      NROUND = IZERO
      IF (NR.EQ.IZERO) GO TO 50
C
C     CHECK TO SEE IF LAST DIGIT IS ALWAYS 0, 1, 2, 8, OR 9.
C        NROUND = 0, IF YES, AND
C        NROUND = 1, IF NO.
C
      DO 30 I=1,N
        L = NW
        CALL RFORMT (IONE,NS,B,X(I),IZERO,IZERO,NW,ND,NALPHA,IRF)
        DO 20 J=1,ND
          IF (NALPHA(L).EQ.LA(45)) GO TO 20
          IF (NALPHA(L).EQ.LA( 4)) GO TO 40
          IF (NALPHA(L).EQ.LA( 5)) GO TO 40
          IF (NALPHA(L).EQ.LA( 6)) GO TO 40
          IF (NALPHA(L).EQ.LA( 7)) GO TO 40
          IF (NALPHA(L).EQ.LA( 8)) GO TO 40
          GO TO 30
  20    CONTINUE
  30  CONTINUE
      GO TO 50
  40  NROUND = IONE
C
C     ..................................................................
C
  50  MS = NS - NROUND
      DO 90 I=1,N
        K = IZERO
        L = NW
        CALL RFORMT (IONE,MS,B,X(I),IZERO,IZERO,NW,ND,NALPHA,IRF)
        DO 70 J=1,ND
C
C         LA( 1) = 1H0
C         LA(38) = 1H.
C         LA(45) = 1H
C
          IF (NALPHA(L).EQ.LA(45)) GO TO 60
          IF (NALPHA(L).NE.LA( 1)) GO TO 80
  60      K = K + IONE
          L = L - IONE
  70    CONTINUE
  80    M = MIN0 (M,K)
        IF (K.NE.IZERO) GO TO 90
          MW = NW
          MD = ND
          RETURN
  90  CONTINUE
      MD = ND - M
      MW = NW - ND + MD
      RETURN
C
C     ==================================================================
C
      END
*MIST
      SUBROUTINE MIST (M,B,LCHK,NLA,IND,LN,MXLINE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   MIST V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PERFORM PRINTING FOR CORRELATION INSTRUCTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NBC(12)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             B(*)
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NC  / 6 /
C
C     ==================================================================
C
      NLU  = LENGTH + IFOUR
      IF (NCRT.EQ.IZERO) NLU = 56
      LINE = LN
      MM   = M - ITWO
      NN   = NRMAX - ITHRE
      M1   = IDIV (M-IONE,NC,JND) + IONE
      NCA  = IZERO
      NRA  = IZERO
      DO 130 KEN=1,M1
        NCP = MIN0 (NC,M-NCA)
        NRP = M - LCHK * NCA
        NLP = (IONE + IDIV (IND,7,JND)) * NRP + IFIVE
        IF (NLP.LT.NLU-NLA) GO TO 10
        CALL PAGE (IFOUR)
        NLA  = IZERO
        LINE = ITHRE
  10    DO 20 IYA=1,NCP
          I1 = NCA + IYA
          NBC(IYA) = IDIV (IARGS(I1+1)-IONE,NROW,JND) + IONE
  20    CONTINUE
C
        IF (NCA.GT.IZERO) GO TO 100
        LINE = LINE + ITHRE
        GO TO (30,40,50,60,70,80,90), IND
C
  30    WRITE (IPRINT,140)
        GO TO 100
C
  40    WRITE (IPRINT,150)
        GO TO 100
C
  50    WRITE (IPRINT,160) MM
        GO TO 100
C
  60    WRITE (IPRINT,170)
        GO TO 100
C
  70    WRITE (IPRINT,180)
        GO TO 100
C
  80    I1 = IDIV (IARGS(NRA+3)-IONE,NROW,JND) + IONE
        I2 = NCA * M + ITWO
        WRITE (IPRINT,190) NN, B(I2), I1, NBC(1)
        LINE = LINE + ITWO
        GO TO 100
C
  90    WRITE (IPRINT,200)
        LINE = LINE + IONE
C
 100    WRITE (IPRINT,210) (NBC(I),I=1,NCP)
        WRITE (IPRINT,220)
        LINE = LINE + ITHRE
        DO 120 NAGA=1,NRP
          NBR = NRA + NAGA
          I1 = NCA * M + NBR
          I2 = I1 + (NCP-MAX0(IZERO,LCHK*(NCP-NAGA))-IONE) * M
          NBR = IDIV (IARGS(NBR+1)-IONE,NROW,JND) + IONE
          IF (IND.NE.7 .OR. LINE + ITHRE.LE.MXLINE) GO TO 110
          CALL PAGE (IFOUR)
          LINE = ITHRE
 110      WRITE (IPRINT,230) NBR, (B(I),I=I1,I2,M)
          LINE = LINE + IONE
          IF (IND.NE.7) GO TO 120
          I1 = I1 + ITWO * M * M
          I2 = I2 + ITWO * M * M
          WRITE (IPRINT,240) (B(I),I=I1,I2,M)
          WRITE (IPRINT,220)
          LINE = LINE + ITWO
 120    CONTINUE
        NLA = NLA + NLP
        NRA = NRA + LCHK * NCP
        NCA = NCA + NCP
 130  CONTINUE
      LN = LINE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 140  FORMAT (     /1X,31HSIMPLE CORRELATION COEFFICIENTS)
 150  FORMAT (     /1X,70HSIGNIFICANCE LEVELS OF SIMPLE CORRELATION COEF
     1F'S (ASSUMING NORMALITY))
 160  FORMAT (     /1X,37HPARTIAL CORRELATION COEFFICIENTS WITH,I3,26H R
     1EMAINING VARIABLES FIXED)
 170  FORMAT (     /1X,70HSIGNIFICANCE LEVELS OF PARTIAL CORRELATION COE
     1F'S (ASSUMING NORMALITY))
 180  FORMAT (     /1X,58HSPEARMAN RANK CORRELATION COEFFICIENTS (ADJUST
     1ED FOR TIES))
 190  FORMAT ( /1X,5X,51HSIGNIFICANCE LEVEL OF QUADRATIC FIT OVER LINEAR
     1 FIT/10X,27HBASED ON F RATIO WITH 1 AND,I5,19H DEGREES OF FREEDOM/
     21H ,10X,12H(FOR EXAMPLE,F7.4,33H IS THE SIGNIFICANCE LEVEL OF THE/
     311X,26HQUADRATIC TERM WHEN COLUMN,I3,20H IS FITTED TO COLUMN,I3,1H
     4))
 200  FORMAT ( /1X, 2X,68HCONFIDENCE INTERVALS FOR SIMPLE CORRELATION CO
     1EFF'S (FISHER APPROX.)/3X,68H95 PER CENT LIMITS BELOW DIAGONAL, 99
     2 PER CENT LIMITS ABOVE DIAGONAL)
 210  FORMAT ( /1X,6HCOLUMN,10I10)
 220  FORMAT (1H )
 230  FORMAT (1H ,I4,4X,10F10.4)
 240  FORMAT (1H ,6X,10F10.4)
C
C     ==================================================================
C
      END
*MVELNG
      SUBROUTINE MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MVELNG V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO MOVE DATA FOR A NEW LANGUAGE, USED BY LOOKUP, INTO
C        IR(.), NL(.), ETC.
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
      DIMENSION L(*), LO(*), LD( *), LW(35,*), LU(*), LCXX(*)
      DIMENSION LC( *), LF(*), LP( *),   LT(*), LCH(*)
C
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD
      COMMON /ARRAYB/ IALPH(6), ICL(10,2), ICP(6), ID(8,2) 
      COMMON /ARRYBC/ ICOLHD(7)
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER ICOLHD*1
      CHARACTER LCH*1
C
C     ==================================================================
C
      DO 10 I=1,NIR
        IR(I,1) = L(I)
  10  CONTINUE
C
      DO 20 I=1,25
        NL(I) = LO(I)
  20  CONTINUE
C
      DO 30 I=1,NID
          ID(I,1) = LD(I)
  30  CONTINUE
C
      DO 50 I=1,NIRD
        DO 40 J=1,2
          IRD(I,J) = LW(I,J)
  40    CONTINUE
  50  CONTINUE
C
      DO 60 I=1,NITP
          ITP(I,1) = LU(I)
  60  CONTINUE
C
      DO 70 I=1,6
        ICP(I) = LCXX(I)
  70  CONTINUE
C
      DO 80 I=1,8
          ICL(I,1) = LC(I)
  80  CONTINUE
C
      DO 90 I=1,NDIST
          IDIST(I) = LF(I)
  90  CONTINUE
C
      DO 100 I=1,NPROP
          IPROP(I) = LP(I)
 100  CONTINUE
C
      DO 110 I=1,NITB
          ITB(I) = LT(I)
 110  CONTINUE
C
      DO 120 I=1,7
        ICOLHD(I) = LCH(I)
 120  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*MTXCHK
      SUBROUTINE MTXCHK (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MTXCHK V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     J AS INPUT = NO OF MATRICES TO BE CHECKED
C       IARGS(1), IARGS(5),...,IARGS(4*(J-1)+1) STARTING ROW OF MATRIX
C       IARGS(2), IARGS(6),...,IARGS(4*(J-1)+2) STARTING COL OF MATRIX
C       IARGS(3), IARGS(7),...,IARGS(4*(J-1)+3) NO. OF ROWS
C       IARGS(4), IARGS(8),...,IARGS(4*(J-1)+4) NO OF COLUMNS
C
C     UPON RETURN ...
C
C     J=0  IF ALL MATRICES ARE IN WORKSHEET AND
C       IARGS(1),IARGS(5),...,IARGS(4*(J-1)+1) WILL CONTAIN STARTING
C       ADDRESS OF MATRIX.
C     J GREATER THAN ZERO,  IF MATRIX IS NOT IN WORKSHEET.
C        J = 1, SOME IARGS ARE NEGATIVE
C        J = 2, MATRIX TO BIG FOR WORKSHEET.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1967.
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
C     ==================================================================
C
      JB = IFOUR * J
      J = IZERO
      DO 10 I=1,JB
        IF (IARGS(I).GT.IZERO) GO TO 10
        J = IONE
        RETURN
C
C     ..................................................................
C
  10  CONTINUE
      DO 20 I=1,JB,4
        IF (IARGS(I)+IARGS(I+2)-IONE.GT.NROW) GO TO 30
        IF (IARGS(I+1)+IARGS(I+3)-IONE.GT.NCOL) GO TO 30
        IARGS(I) = IARGS(I) + (IARGS(I+1)-IONE)*NROW
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  J = ITWO
      RETURN
C
C     ==================================================================
C
      END
*MTXCHL
      SUBROUTINE MTXCHL (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MTXCHL V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     J AS INPUT = NO OF MATRICES TO BE CHECKED
C       IARGS(1), IARGS(5),...,IARGS(4*(J-1)+1) STARTING ROW OF MATRIX
C       IARGS(2), IARGS(6),...,IARGS(4*(J-1)+2) STARTING COL OF MATRIX
C       IARGS(3), IARGS(7),...,IARGS(4*(J-1)+3) NO. OF ROWS
C       IARGS(4), IARGS(8),...,IARGS(4*(J-1)+4) NO OF COLUMNS
C
C     UPON RETURN ...
C
C     J=0  IF ALL MATRICES ARE IN WORKSHEET AND
C       IARGS(1),IARGS(5),...,IARGS(4*(J-1)+1) WILL CONTAIN STARTING
C       ADDRESS OF MATRIX.
C     J GREATER THAN ZERO,  IF MATRIX IS NOT IN WORKSHEET.
C        J = 1, SOME IARGS ARE NEGATIVE
C        J = 2, MATRIX TO BIG FOR WORKSHEET.
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT MTXCHK.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1967.
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
C     ==================================================================
C
      JB = IFOUR * J
      J = IZERO
      DO 10 I=1,JB
        IF (IARGS(I).GT.IZERO) GO TO 10
        J = IONE
        RETURN
C
C     ..................................................................
C
  10  CONTINUE
      DO 20 I=1,JB,4
        IF (IARGS(I)+IARGS(I+2)-IONE.GT.NROW) GO TO 30
        IF (IARGS(I+1)+IARGS(I+3)-IONE.GT.NCOL) GO TO 30
        IARGS(I) = IARGS(I) + (IARGS(I+1)-IONE)*NROW
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  J = ITWO
      RETURN
C
C     ==================================================================
C
      END
*MTXCHM
      SUBROUTINE MTXCHM (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MTXCHM V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     J AS INPUT = NO OF MATRICES TO BE CHECKED
C       IARGS(1), IARGS(5),...,IARGS(4*(J-1)+1) STARTING ROW OF MATRIX
C       IARGS(2), IARGS(6),...,IARGS(4*(J-1)+2) STARTING COL OF MATRIX
C       IARGS(3), IARGS(7),...,IARGS(4*(J-1)+3) NO. OF ROWS
C       IARGS(4), IARGS(8),...,IARGS(4*(J-1)+4) NO OF COLUMNS
C
C     UPON RETURN ...
C
C     J=0  IF ALL MATRICES ARE IN WORKSHEET AND
C       IARGS(1),IARGS(5),...,IARGS(4*(J-1)+1) WILL CONTAIN STARTING
C       ADDRESS OF MATRIX.
C     J GREATER THAN ZERO,  IF MATRIX IS NOT IN WORKSHEET.
C        J = 1, SOME IARGS ARE NEGATIVE
C        J = 2, MATRIX TO BIG FOR WORKSHEET.
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT MTXCHK.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1967.
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
C     ==================================================================
C
      JB = IFOUR * J
      J = IZERO
      DO 10 I=1,JB
        IF (IARGS(I).GT.IZERO) GO TO 10
        J = IONE
        RETURN
C
C     ..................................................................
C
  10  CONTINUE
      DO 20 I=1,JB,4
        IF (IARGS(I)+IARGS(I+2)-IONE.GT.NROW) GO TO 30
        IF (IARGS(I+1)+IARGS(I+3)-IONE.GT.NCOL) GO TO 30
        IARGS(I) = IARGS(I) + (IARGS(I+1)-IONE)*NROW
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  J = ITWO
      RETURN
C
C     ==================================================================
C
      END
*MTXCHN
      SUBROUTINE MTXCHN (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. MTXCHN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     J AS INPUT = NO OF MATRICES TO BE CHECKED
C       IARGS(1), IARGS(5),...,IARGS(4*(J-1)+1) STARTING ROW OF MATRIX
C       IARGS(2), IARGS(6),...,IARGS(4*(J-1)+2) STARTING COL OF MATRIX
C       IARGS(3), IARGS(7),...,IARGS(4*(J-1)+3) NO. OF ROWS
C       IARGS(4), IARGS(8),...,IARGS(4*(J-1)+4) NO OF COLUMNS
C
C     UPON RETURN ...
C
C     J=0  IF ALL MATRICES ARE IN WORKSHEET AND
C       IARGS(1),IARGS(5),...,IARGS(4*(J-1)+1) WILL CONTAIN STARTING
C       ADDRESS OF MATRIX.
C     J GREATER THAN ZERO,  IF MATRIX IS NOT IN WORKSHEET.
C        J = 1, SOME IARGS ARE NEGATIVE
C        J = 2, MATRIX TO BIG FOR WORKSHEET.
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT MTXCHK.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1967.
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
C     ==================================================================
C
      JB = IFOUR * J
      J = IZERO
      DO 10 I=1,JB
        IF (IARGS(I).GT.IZERO) GO TO 10
        J = IONE
        RETURN
C
C     ..................................................................
C
  10  CONTINUE
      DO 20 I=1,JB,4
        IF (IARGS(I)+IARGS(I+2)-IONE.GT.NROW) GO TO 30
        IF (IARGS(I+1)+IARGS(I+3)-IONE.GT.NCOL) GO TO 30
        IARGS(I) = IARGS(I) + (IARGS(I+1)-IONE)*NROW
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  J = ITWO
      RETURN
C
C     ==================================================================
C
      END
*MXTXP
      SUBROUTINE MXTXP (X,N,NP,K,A,L2)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MXTXP V 7.00  4/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     X        IS MATRIX TO BE USED
C     N        IS DIMENSIONED SIZE OF A
C     NP       IS NUMBER OF ROWS IN A
C     K        IS NUMBER OF COLUMNS IN A
C     A        IS SCRATCH AREA WHERE MATRIX IS TO BE STORED
C     L2 = 1,  MULTIPLY X TIMES X TRANSPOSED
C        = 2,  MULTIPLY X TRANSPOSED TIMES  X
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1968.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             A(*), X(*)
      REAL             FDPCON
C
      DOUBLE PRECISION XP(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
      IC = IONE
      IF (L2.EQ.ITWO) GO TO 40
C
C     COMPUTE X TIMES X TRANSPOSE.
C
      DO 30 KK=1,NP
        NI = IONE
        DO 20 I=1,NP
          CALL DSUMAL ( XP(1),0,DSUM)
          NJ = NI
          NKK = KK
          DO 10 J=1,K
            XP(1) = DBLE (X(NJ)) * DBLE (X(NKK))
            CALL DSUMAL (XP(1),-1,DSUM)
            NJ = NJ + N
            NKK = NKK + N
  10      CONTINUE
          CALL DSUMAL (XP(1),1,DSUM)
          IF (K.EQ.IONE) DSUM = XP(1)
          A(IC) = FDPCON (DSUM)
          IC = IC + IONE
          NI = NI + IONE
  20    CONTINUE
  30  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COMPUTE X TRANSPOSE TIMES X.
C
  40  NL = IONE
      DO 70 L=1,K
        NI = IONE
        DO 60 J=1,K
          CALL DSUMAL (XP(1),0,DSUM)
          NJ = NI
          NLL = NL
          DO 50 I=1,NP
            XP(1) = DBLE (X(NJ)) * DBLE (X(NLL))
            CALL DSUMAL (XP(1),-1,DSUM)
            NJ = NJ + IONE
            NLL = NLL + IONE
  50      CONTINUE
          CALL DSUMAL (XP(1),1,DSUM)
          IF (NP.EQ.IONE) DSUM = XP(1)
          A(IC) = FDPCON (DSUM)
          IC = IC + IONE
          NI = NI + N
  60    CONTINUE
        NL = NL + N
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*MXTXQ
      SUBROUTINE MXTXQ (X,N,NP,K,A,L2)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  MXTXQ V 7.00  4/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     X        IS MATRIX TO BE USED
C     N        IS NUMBER OF ROWS IN X
C     NP       IS NUMBER OF ROWS IN A
C     K        IS NUMBER OF COLUMNS IN A
C     A        IS WHERE MATRIX IS TO BE STORED
C     L2 = 1,  MULTIPLY X TIMES X TRANSPOSED
C        = 2,  MULTIPLY X TRANSPOSED TIMES  X
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT MXTXP.
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1968.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             A(*), X(*)
      REAL             FDPCON
C
      DOUBLE PRECISION XP(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
      IC = IONE
      IF (L2.EQ.ITWO) GO TO 40
C
C     COMPUTE X TIMES X TRANSPOSE.
C
      DO 30 KK=1,NP
        NI = 1
        DO 20 I=1,NP
          CALL DSUMAL (XP(1),0,DSUM)
          NJ = NI
          NKK = KK
          DO 10 J=1,K
            XP(1) = DBLE (X(NJ)) * DBLE (X(NKK))
            CALL DSUMAL (XP(1),-1,DSUM)
            NJ = NJ + N
            NKK = NKK + N
  10      CONTINUE
          CALL DSUMAL (XP(1),1,DSUM)
          IF (K.EQ.IONE) DSUM = XP(1)
          A(IC) = FDPCON (DSUM)
          IC = IC + IONE
          NI = NI + 1
  20    CONTINUE
  30  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COMPUTE X TRANSPOSE TIMES X.
C
  40  NL = IONE
      DO 70 L=1,K
        NI = 1
        DO 60 J=1,K
          CALL DSUMAL (XP(1),0,DSUM)         
          NJ = NI
          NLL = NL
          DO 50 I=1,NP
            XP(1) = DBLE (X(NJ)) * DBLE (X(NLL))
            CALL DSUMAL (XP(1),-1,DSUM)
            NJ = NJ + 1
            NLL = NLL + 1
  50      CONTINUE
          CALL DSUMAL (XP(1),1,DSUM)
          IF (NP.EQ.IONE) DSUM = XP(1)
          A(IC) = FDPCON (DSUM)
          IC = IC + IONE
          NI = NI + N
  60    CONTINUE
        NL = NL + N
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*NBCDF
      SUBROUTINE NBCDF (X,P,N,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NBCDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE X
C              FOR THE NEGATIVE BINOMIAL DISTRIBUTION
C              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
C              PARAMETER = P,
C              AND INTEGER 'NUMBER OF SUCCESSES IN BERNOULLI TRIALS'
C              PARAMETER = N.
C              THE NEGATIVE BINOMIAL DISTRIBUTION USED
C              HEREIN HAS MEAN = N*(1-P)/P
C              AND STANDARD DEVIATION = SQRT(N*(1-P)/(P*P))).
C              THIS DISTRIBUTION IS DEFINED FOR
C              ALL NON-NEGATIVE INTEGER X--X = 0, 1, 2, ... .
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = C(N+X-1,X) * P**N * (1-P)**X.
C              WHERE C(N+X-1,X) IS THE COMBINATORIAL FUNCTION
C              EQUALING THE NUMBER OF COMBINATIONS OF N+X-1 ITEMS
C              TAKEN N AT A TIME.
C              THE NEGATIVE BINOMIAL DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF FAILURES
C              BEFORE OBTAINING N SUCCESSES IN AN
C              INDEFINITE SEQUENCE OF BERNOULLI (0,1)
C              TRIALS WHERE THE PROBABILITY OF SUCCESS
C              IN A SINGLE TRIAL = P.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE
C                                AT WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE AND
C                                INTEGRAL-VALUED.
C                     --P      = THE SINGLE PRECISION VALUE
C                                OF THE 'BERNOULLI PROBABILITY'
C                                PARAMETER FOR THE NEGATIVE BINOMIAL
C                                DISTRIBUTION.
C                                P SHOULD BE BETWEEN
C                                0.0 (EXCLUSIVELY) AND
C                                1.0 (EXCLUSIVELY).
C                     --N      = THE INTEGER VALUE
C                                OF THE 'NUMBER OF SUCCESSES
C                                IN BERNOULLI TRIALS' PARAMETER.
C                                N SHOULD BE A POSITIVE INTEGER.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF
C             FOR THE NEGATIVE BINOMIAL DISTRIBUTION
C             WITH 'BERNOULLI PROBABILITY' PARAMETER = P
C             AND 'NUMBER OF SUCCESSES IN BERNOULLI TRIALS'
C             PARAMETER = N.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE AND INTEGRAL-VALUED.
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
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 945, FORMULAE 26.5.24 AND
C                 26.5.28, AND PAGE 929.
C               --JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1969, PAGES 122-142,
C                 ESPECIALLY PAGE 127.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 92-95.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 155-157, 210.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 130-131.
C               --WILLIAMSON AND BRETHERTON, TABLES OF
C                 THE NEGATIVE BINOMIAL PROBABILITY
C                 DISTRIBUTION, 1963.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGE 304.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
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
      REAL             CDF, X, P
      REAL             AK, AN2, DEL, FINTX
      REAL             FDIV, FDPCON
      REAL             SPCA, SPCB
C
      DOUBLE PRECISION TERM(1)
      DOUBLE PRECISION DX2, ANU1, ANU2, Z, SUM, AI, COEF1, COEF2
      DOUBLE PRECISION ARG, COEF
      DOUBLE PRECISION THETA, SINTH, COSTH, A, B
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DATAN
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 0.0001 /
      DATA SPCB / 0.001  /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
        IND = ITHRE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (N.GE.IONE) GO TO 20
        IND = IFOUR
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  20  IF (X.GE.RZERO) GO TO 30
        IND = IONE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  30  INTX = X + SPCA
      FINTX = INTX
      DEL = X - FINTX
      IF (DEL.LT.RZERO) DEL = -DEL
      IF (DEL.GT.SPCB) IND = ITWO
C
C     ---   START POINT   ----------------------------------------------
C
C     EXPRESS THE NEGATIVE BINOMIAL CUMULATIVE DISTRIBUTION
C     FUNCTION IN TERMS OF THE EQUIVALENT BINOMIAL
C     CUMULATIVE DISTRIBUTION FUNCTION,
C     AND THEN OPERATE ON THE LATTER.
C
      INTX = X + SPCA
      K = N - IONE
      N2 = N + INTX
C
C     EXPRESS THE BINOMIAL CUMULATIVE DISTRIBUTION
C     FUNCTION IN TERMS OF THE EQUIVALENT F
C     CUMULATIVE DISTRIBUTION FUNCTION,
C     AND THEN EVALUATE THE LATTER.
C
      AK = K
      AN2 = N2
      DX2 = FDIV (P,RONE-P,JIND) * FDIV (AN2-AK,AK+RONE,JIND)
      NU1 = ITWO * (K+IONE)
      NU2 = ITWO * (N2-K)
      ANU1 = NU1
      ANU2 = NU2
      Z = FDDIV (ANU2,ANU2+ANU1*DX2,JIND)
C
C     DETERMINE IF NU1 AND NU2 ARE EVEN OR ODD
C
      IFLAG1 = NU1 - ITWO * IDIV (NU1,ITWO,JIND)
      IFLAG2 = NU2 - ITWO * IDIV (NU2,ITWO,JIND)
      IF (IFLAG1.EQ.IZERO) GO TO 40
      IF (IFLAG2.EQ.IZERO) GO TO 70
      GO TO 100
C
C     DO THE NU1 EVEN AND NU2 EVEN OR ODD CASE
C
  40  SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      IMAX = IDIV (NU1-ITWO,ITWO,JIND)
      IF (IMAX.LE.IZERO) GO TO 60
      DO 50 I=1,IMAX
        AI = I
        COEF1 = DTWO * (AI-DONE)
        COEF2 = DTWO * AI
        TERM(1) = TERM(1) * FDDIV (ANU2+COEF1,COEF2,JIND) * (DONE-Z)
        CALL DSUMAL (TERM,-IONE,SUM)
  50  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  60  SUM = SUM + DONE
      SUM = Z ** FDDIV (ANU2,DTWO,JIND) * SUM
      CDF = FDPCON (DONE-SUM)
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
        AI = I
        COEF1 = DTWO * (AI-DONE)
        COEF2 = DTWO * AI
        TERM(1) = TERM(1) * FDDIV (ANU1+COEF1,COEF2,JIND) * Z
        CALL DSUMAL (TERM,-IONE,SUM)
  80  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  90  SUM = SUM + DONE
      CDF = FDPCON ( ((DONE-Z)**FDDIV(ANU1,DTWO,JIND)) * SUM )
      RETURN
C
C     ..................................................................
C
C     DO THE NU1 ODD AND NU2 ODD CASE.
C
 100  SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      ARG = FDSQRT (FDDIV(ANU1,ANU2,JIND)*DX2)
      THETA = DATAN (ARG)
      SINTH = FDDIV (ARG,FDSQRT(DONE+ARG*ARG),JIND)
      COSTH = FDDIV (DONE,FDSQRT(DONE+ARG*ARG),JIND)
      IF (NU2.EQ.IONE) GO TO 130
      IF (NU2.EQ.ITHRE) GO TO 120
      IMAX = NU2 - ITWO
      DO 110 I=3,IMAX,2
        AI      = I
        COEF1   = AI - DONE
        COEF2   = AI
        TERM(1) = TERM(1) * FDDIV (COEF1,COEF2,JIND) * (COSTH*COSTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 110  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 120  SUM = SUM + DONE
      SUM = SUM * SINTH * COSTH
C
 130  A       = FDDIV (DTWO,DPI,JIND) * (THETA+SUM)
      SUM     = DZERO
      TERM(1) = DONE
      CALL DSUMAL (TERM,IZERO,SUM)
      B = DZERO
      IF (NU1.EQ.IONE) GO TO 200
      IF (NU1.EQ.ITHRE) GO TO 160
      IMAX = NU1 - ITHRE
      DO 150 I=1,IMAX,2
        AI = I
        COEF1 = AI
        COEF2 = AI + DTWO
        TERM(1) = TERM(1) * FDDIV(ANU2+COEF1,COEF2,JIND) * (SINTH*SINTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 150  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 160  SUM = SUM + DONE
      SUM = SUM * SINTH * (COSTH**N)
      COEF = DONE
      IEVODD = NU2 - ITWO * IDIV (NU2,ITWO,JIND)
      IMIN = ITHRE
      IF (IEVODD.EQ.IZERO) IMIN = ITWO
      IF (IMIN.GT.NU2) GO TO 180
      DO 170 I=IMIN,NU2,2
        AI = I
        COEF = FDDIV (AI-DONE,AI,JIND) * COEF
 170  CONTINUE
C
 180  COEF = COEF * ANU2
      IF (IEVODD.EQ.IZERO) GO TO 190
      COEF = COEF * FDDIV (DTWO,DPI,JIND)
C
 190  B = COEF * SUM
C
 200  CDF = FDPCON (A-B)
      RETURN
C
C     ==================================================================
C
      END
*NBPDF
      SUBROUTINE NBPDF (X,N,P,PDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NBPDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE THE NEGATIVE BINOMIAL P.D.F.
C
C     USES RECURRENCE RELATION IN DOUBLE PRECISION.
C
C        F(X+1) = (N+X) * Q *  F(X)
C                 -----   -
C                 (X+1)
C
C
C      IND = FAULT INDICATOR.
C          = 0, IF EVERYTHING IS OK.
C          = 1, IF N IS NOT POSITIVE.
C          = 2, IF P IS NOT BETWEEN ZERO AND ONE.
C          = 3, IF X IS NEGATIVE.
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
      REAL             P, PDF, X
      REAL             RIX, RN, RX
      REAL             FDPCON
C
      DOUBLE PRECISION DP
      DOUBLE PRECISION DQ, DNUM, DDEN, TERM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IND = IZERO
      RX  = X
      RN  = N
      IF (RX.GE.RZERO) GO TO 10
        IND = ITHRE
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 20
        IND = ITWO
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
C
  20  IF (N.GT.IZERO) GO TO 30
        IND = IONE
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
  30  M   = RX
      RIX = M
      IF (RIX.EQ.RX) GO TO 40
        IND = IFOUR
C
C     ==================================================================
C
  40  DP   = P
      DQ   = DONE - DP
      TERM = DP ** N
      IF (M.EQ.IZERO) GO TO 60
C
      DNUM   = RN
      DDEN   = DONE
      DO 50 I=1,M
        TERM = FDDIV (DNUM,DDEN,JIND) * DQ * TERM
        DNUM = DNUM + DONE
        DDEN = DDEN + DONE
  50  CONTINUE
C
  60  PDF = FDPCON (TERM)
      RETURN
C
C     ==================================================================
C
      END
*NBPPF
      SUBROUTINE NBPPF (P,PPAR,N,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NBPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
C              FOR THE NEGATIVE BINOMIAL DISTRIBUTION
C              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
C              PARAMETER = PPAR,
C              AND INTEGER 'NUMBER OF SUCCESSES IN BERNOULLI TRIALS'
C              PARAMETER = N.
C              THE NEGATIVE BINOMIAL DISTRIBUTION USED
C              HEREIN HAS MEAN = N*(1-PPAR)/PPAR
C              AND STANDARD DEVIATION = SQRT(N*(1-PPAR)/(PPAR*PPAR))).
C              THIS DISTRIBUTION IS DEFINED FOR
C              ALL NON-NEGATIVE INTEGER X--X = 0, 1, 2, ... .
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = C(N+X-1,N) * PPAR**N * (1-PPAR)**X.
C              WHERE C(N+X-1,N) IS THE COMBINATORIAL FUNCTION
C              EQUALING THE NUMBER OF COMBINATIONS OF N+X-1 ITEMS
C              TAKEN N AT A TIME.
C              THE NEGATIVE BINOMIAL DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF FAILURES
C              BEFORE OBTAINING N SUCCESSES IN AN
C              INDEFINITE SEQUENCE OF BERNOULLI (0,1)
C              TRIALS WHERE THE PROBABILITY OF SUCCESS
C              IN A SINGLE TRIAL = PPAR.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (INCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --PPAR   = THE SINGLE PRECISION VALUE
C                                OF THE 'BERNOULLI PROBABILITY'
C                                PARAMETER FOR THE NEGATIVE BINOMIAL
C                                DISTRIBUTION.
C                                PPAR SHOULD BE BETWEEN
C                                0.0 (EXCLUSIVELY) AND
C                                1.0 (EXCLUSIVELY).
C                     --N      = THE INTEGER VALUE
C                                OF THE 'NUMBER OF SUCCESSES
C                                IN BERNOULLI TRIALS' PARAMETER.
C                                N SHOULD BE A POSITIVE INTEGER.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
C             FUNCTION VALUE PPF
C             FOR THE NEGATIVE BINOMIAL DISTRIBUTION
C             WITH 'BERNOULLI PROBABILITY' PARAMETER = PPAR
C             AND 'NUMBER OF SUCCESSES IN BERNOULLI TRIALS'
C             PARAMETER = N.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C
C     RESTRICTIONS--PPAR SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C                 --N SHOULD BE A POSITIVE INTEGER.
C                 --P SHOULD BE BETWEEN 0.0 (INCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF, NBCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, EXP, ALOG.
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
C                 DISTRIBUTIONS, 1969, PAGES 122-142,
C                 ESPECIALLY PAGE 127, FORMULA 22.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 92-95.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 929.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 155-157, 210.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 130-131.
C               --WILLIAMSON AND BRETHERTON, TABLES OF
C                 THE NEGATIVE BINOMIAL PROBABILITY
C                 DISTRIBUTION, 1963.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGE 304.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
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
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             P, PPAR, PPF
      REAL             AMEAN, AN, ARCSH, ARG, E, PF0, P0, P1, P2
      REAL             SD, SINH, X0, X1, X2, YMEAN, YPPF, YSD, ZPPF
      REAL             FDIV, FDPCON, FEXP, FLOG, FSQRT
      REAL             SPCA, SPCB
C
      DOUBLE PRECISION DPPAR
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /  0.375 /
      DATA SPCB /  0.75  /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GE.RZERO.AND.P.LT.RONE) GO TO 10
        IND = IONE
        PPF = RZERO
        IF (P.EQ.RONE) PPF = RPIFY
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
C        1) P  =  RZERO
C        2) P  =  RHALF AND PPAR  =  RHALF
C        3) PPF  =  0
C
      IF (P.EQ.RZERO) GO TO 40
      IF (P.EQ.RHALF .AND. PPAR.EQ.RHALF) GO TO 50
      PF0 = FDPCON (DPPAR**N)
      IF (P.GT.PF0) GO TO 60
C
  40  PPF = RZERO
      RETURN
C
C     ..................................................................
C
  50  PPF = N - IONE
      RETURN
C
C     ..................................................................
C
C     DETERMINE AN INITIAL APPROXIMATION TO THE NEGATIVE BINOMIAL
C        PERCENT POINT BY USE OF THE HYPERBOLIC ARCSIN
C        TRANSFORMATION OF THE NEGATIVE BINOMIAL
C        TO APPROXIMATE NORMALITY.
C        (SEE JOHNSON AND KOTZ, DISCRETE DISTRIBUTIONS,
C        PAGE 127, FORMULA 22).
C
 60   AMEAN = AN * FDIV (RONE-PPAR,PPAR,JIND)
      SD    = FSQRT (FDIV (AN*(RONE-PPAR),PPAR*PPAR,JIND) )
      ARG   = FSQRT ( FDIV (AMEAN+SPCA,AN-SPCB,JIND) )
      ARCSH = FLOG (ARG+FSQRT(ARG*ARG+RONE))
      YMEAN = (FSQRT(AN-RHALF)) * ARCSH
      YSD   = RHALF
      CALL NORPPF (P,ZPPF,NIND)
      YPPF  = YMEAN + ZPPF * YSD
      ARG   =  FDIV (YPPF,FSQRT(AN-RHALF),JIND)
      E     = FEXP (ARG)
      SINH  =  FDIV (E-FDIV(RONE,E,JIND),RTWO,JIND)
      X2    = - SPCA + (AN-SPCB) * SINH * SINH
      X2    = X2 + RHALF
      IX2   = X2
C
C     CHECK AND MODIFY (IF NECESSARY) THIS INITIAL
C        ESTIMATE OF THE PERCENT POINT
C        TO ASSURE THAT IT BE NON-NEGATIVE.
C
      IF (IX2.LT.IZERO) IX2 = IZERO
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
      IX1 = MXINT
      ISD = SD + RONE
      X2  = IX2
      CALL NBCDF (X2,PPAR,N,P2,NIND)
C
      IF (P2.GE.P) GO TO 90
C
      IX0 = IX2
  70    IX2 = IX0 + ISD
        IF (IX2.GE.IX1) GO TO 120
        X2  = IX2
        CALL NBCDF (X2,PPAR,N,P2,NIND)
        IF (P2.GE.P) GO TO 80
        IX0 = IX2
      GO TO 70
C
  80  IX1 = IX2
      GO TO 120
C
  90  IX1 = IX2
 100    IX2 = IX1 - ISD
        IF (IX2.LE.IX0) GO TO 120
        X2  = IX2
        CALL NBCDF (X2,PPAR,N,P2,NIND)
        IF (P2.LT.P) GO TO 110
        IX1 = IX2
      GO TO 100
C
 110  IX0 = IX2
C
 120  IF (IX0.NE.IX1)   GO TO 150
      IF (IX0.EQ.IZERO) GO TO 130
      IF (IX0.EQ.N)     GO TO 140
      GO TO 220
C
 130  IX1 = IX1 + IONE
      GO TO 150
C
 140  IX0 = IX0 - IONE
C
C     COMPUTE NEGATIVE BINOMIAL PROBABILITIES FOR THE
C        DERIVED LOWER AND UPPER BOUNDS.
C
 150  X0 = IX0
      X1 = IX1
      CALL NBCDF (X0,PPAR,N,P0,NIND)
      CALL NBCDF (X1,PPAR,N,P1,NIND)
C
C     CHECK THE PROBABILITIES FOR PROPER ORDERING.
C
      IF (P0.LT.P .AND. P.LE.P1) GO TO 180
      IF (P0.EQ.P)  GO TO 160
      IF (P1.EQ.P)  GO TO 170
      GO TO 220
C
 160  PPF = IX0
      RETURN
C
C     ..................................................................
C
 170  PPF = IX1
      RETURN
C
C     ..................................................................
C
C     THE STOPPING CRITERION IS THAT THE LOWER BOUND
C        AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
C        CHECK TO SEE IF IX1  =  IX0 + 1;
C        IF SO, THE ITERATIONS ARE COMPLETE;
C        IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
C        CHECK PROBABILITIES, AND CONTINUE ITERATING
C        UNTIL IX1  =  IX0 + 1.
C
 180  IX0P1 = IX0 + IONE
      IF (IX1.EQ.IX0P1) GO TO 210
      IX2   = IDIV (IX0+IX1,ITWO,JIND)
      IF (IX2.EQ.IX0)   GO TO 220
      IF (IX2.EQ.IX1)   GO TO 220
      X2    = IX2
      CALL NBCDF (X2,PPAR,N,P2,NIND)
      IF (P0.LT.P2 .AND. P2.LT.P1) GO TO 190
      GO TO 220
 190  IF (P2.LE.P)  GO TO 200
      IX1 = IX2
      P1  = P2
      GO TO 180
C
 200  IX0 = IX2
      P0  = P2
      GO TO 180
C
 210  PPF = IX1
      IF (P0.EQ.P) PPF = IX0
      RETURN
C
C     ..................................................................
C
C     COMPUTATIONAL PROBLEMS ENCOUNTERED.
C
 220  IND = IFOUR
      RETURN
C
C     ==================================================================
C
      END
*NNAME
      SUBROUTINE NNAME (NAME)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NNAME V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT ASSEMBLES A NAME UP TO THE FIRST NON-LETTER OR
C        TO SIX LETTERS, WHICHEVER IS FIRST.  THE INDEX, M, IS INITIALLY
C        POINTING AT THE FIRST LETTER, IT IS LEFT POINTING AT THE FIRST
C        NON-LETTER.
C
C     THE FIRST  THREE CHARACTERS GO INTO THE FIRST  WORD OF NAME
C     THE SECOND THREE CHARACTERS GO INTO THE SECOND WORD OF NAME
C
C        CONVERSION TABLE FOR ALPHABETIC TO NUMERIC AS USED BY OMNITAB.
C
C                  A       729        27         1
C                  B      1458        54         2
C                  C      2187        81         3
C                  D      2916       108         4
C                  E      3645       135         5
C                  F      4374       162         6
C                  G      5103       189         7
C                  H      5832       216         8
C                  I      6561       243         9
C                  J      7290       270        10
C                  K      8019       297        11
C                  L      8748       324        12
C                  M      9477       351        13
C                  N     10206       378        14
C                  O     10935       405        15
C                  P     11664       432        16
C                  Q     12393       459        17
C                  R     13122       486        18
C                  S     13851       513        19
C                  T     14580       540        20
C                  U     15309       567        21
C                  V     16038       594        22
C                  W     16767       621        23
C                  X     17496       648        24
C                  Y     18225       675        25
C                  Z     18954       702        26
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MISC(6), NAME(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C
C     ==================================================================
C
      DO 10 I=1,6
        MISC(I) = IZERO
  10  CONTINUE
C
      DO 20 I=1,6
        L = KARD(KRDPOS) - 9
        IF (L.LT.IONE .OR. L.GE.27) GO TO 40
        MISC(I) = L
        KRDPOS = KRDPOS + IONE
  20  CONTINUE
C
      ISTART = KRDPOS
      DO 30 I=ISTART,LKARD
        IF (KARD(KRDPOS).LT.ITEN .OR. KARD(KRDPOS).GE.36) GO TO 40
        KRDPOS = KRDPOS + IONE
  30  CONTINUE
C
  40  NAME(1) = MISC(3) + 27*(MISC(2) + 27*MISC(1))
      NAME(2) = MISC(6) + 27*(MISC(5) + 27*MISC(4))
      RETURN
C
C     ==================================================================
C
      END
*NONBLA
      SUBROUTINE NONBLA (ISTART,IFSTNB)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NONBLA V 7.00 11/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SEARCH KARD(.), STARTING AT KARD(ISTART), UNTIL THE FIRST NONBLANK
C        CHARACTER IS FOUND.
C
C     SET IFSTNB EQUAL TO THE FIRST NONBLANK CHARACTER.
C
C     RESET KRDPOS IN LABELED COMMON /SCNCRD/ TO LOCATION OF FIRST
C        NONBLANK CHARACTER IN KARD(.) AFTER KARD(ISTART-1).
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
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
      DO 10 I=ISTART,LKARD
        KRDPOS = I
        IF (KARD(I).NE.ICA) GO TO 20
  10  CONTINUE
C
  20  IFSTNB = KARD(KRDPOS)
      RETURN
C
C     ==================================================================
C
      END
*NORCDF
      SUBROUTINE NORCDF (X,CDF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NORCDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
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
      REAL             CDF, X
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
      DATA B4 / -1.821256    /
      DATA B5 /  1.330274    /
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
*NORPPF
      SUBROUTINE NORPPF (P,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NORPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2).
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     REFERENCES--ODEH AND EVANS, THE PERCENTAGE POINTS
C                 OF THE NORMAL DISTRIBUTION, ALGORTIHM 70,
C                 APPLIED STATISTICS, 1974, PAGES 96-97.
C               --EVANS, ALGORITHMS FOR MINIMAL DEGREE
C                 POLYNOMIAL AND RATIONAL APPROXIMATION,
C                 M. SC. THESIS, 1972, UNIVERSITY
C                 OF VICTORIA, B. C., CANADA.
C               --HASTINGS, APPROXIMATIONS FOR DIGITAL
C                 COMPUTERS, 1955, PAGES 113, 191, 192.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 933, FORMULA 26.2.23.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C               --THE KELLEY STATISTICAL TABLES, 1948.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 3-16.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 104-113.
C
C     COMMENTS--THE CODING AS PRESENTED BELOW
C               IS ESSENTIALLY IDENTICAL TO THAT
C               PRESENTED BY ODEH AND EVANS
C               AS ALGORTIHM 70 OF APPLIED STATISTICS.
C               THE PRESENT AUTHOR HAS MODIFIED THE
C               ORIGINAL ODEH AND EVANS CODE WITH ONLY
C               MINOR STYLISTIC CHANGES.
C             --AS POINTED OUT BY ODEH AND EVANS
C               IN APPLIED STATISTICS,
C               THEIR ALGORITHM REPRESENTES A
C               SUBSTANTIAL IMPROVEMENT OVER THE
C               PREVIOUSLY EMPLOYED
C               HASTINGS APPROXIMATION FOR THE
C               NORMAL PERCENT POINT FUNCTION--
C               THE ACCURACY OF APPROXIMATION
C               BEING IMPROVED FROM 4.5*(10**-4)
C               TO 1.5*(10**-8).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
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
C                  ORIGINAL VERSION - SEPTEMBER, 1978.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             P, PPF
      REAL             ADEN, ANUM, P0, P1, P2, P3, P4
      REAL             Q0, Q1, Q2, Q3, Q4, R, T
      REAL             FDIV, FLOG, FSQRT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA P0 / -0.322232431088    /
      DATA P1 / -1.0               /
      DATA P2 / -0.342242088547    /
      DATA P3 / -0.204231210245E-1 /
      DATA P4 / -0.453642210148E-4 /
C
      DATA Q0 /  0.993484626060E-1 /
      DATA Q1 /  0.588581570495    /
      DATA Q2 /  0.531103462366    /
      DATA Q3 /  0.103537752850    /
      DATA Q4 /  0.38560700634E-2  /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GT.RZERO .OR. P.LT.RONE) GO TO 10
      IND = IONE
      PPF = RZERO
      RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  10  IF (P.NE.RHALF) GO TO 20
      PPF = RZERO
      RETURN
C
C     ..................................................................
C
  20  R = P
      IF (P.GT.RHALF) R = RONE - R
      T = FSQRT (-RTWO*FLOG(R))
      ANUM = ((((T*P4+P3)*T+P2)*T+P1)*T+P0)
      ADEN = ((((T*Q4+Q3)*T+Q2)*T+Q1)*T+Q0)
      PPF  = T + FDIV (ANUM,ADEN,JIND)
      IF (P.LT.RHALF) PPF =  - PPF
      RETURN
C
C     ==================================================================
C
      END
*NORRAN
      SUBROUTINE NORRAN (IRAN,KRAN,N,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NORRAN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     GENERATE N NORMAL RANDOM NUMBERS USING THE ALGORITHM ON PAGE
C        104 OF KNUTH, DONALD E. (1969). THE ART OF COMPUTER
C           PROGRAMMING. VOL 2.,  ADDISON-WESLEY.
C
C        METHOD SUGGESTED BY GEORGE MARSAGLIA.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X(*)
      REAL             S, V1, V2, UNIRAN
      REAL             FDIV, FLOG, FSQRT
C
C     ==================================================================
C
      M = IDIV (N+IONE,ITWO,JIND)
      K = IONE
      DO 20 I=1,M
  10    V1 = RTWO * UNIRAN (IRAN,KRAN,IZERO) - RONE
        V2 = RTWO * UNIRAN (IRAN,KRAN,IZERO) - RONE
        S  = V1 ** 2 + V2 ** 2
        IF (S.GT.RONE) GO TO 10
        S  = FSQRT ( FDIV (-RTWO*FLOG(S),S,IND) )
        X(K) = S * V1
        IF (K.LE.N-IONE) X(K+1) = S * V2
        K  = K + ITWO
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*NORWEG
      SUBROUTINE NORWEG
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NORWEG V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION NOT AVAILABLE AT THIS TIME.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                      A337 ADMINISTRATION BUILDING
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER 1975.
C                   CURRENT VERSION -     APRIL 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(282), LO(25), LD( 8), LW(35,2), LU(10), LCXX(6)
      DIMENSION LC( 8), LF(30), LP( 5),   LT(14), LCH(7)
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      CHARACTER LCH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   ONE WORD COMMANDS IN IR(.).
C
      DATA L(  1) /   76002916 /
      DATA L(  2) /   77804132 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) /   81315796 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82513959 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514688 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84104131 /
      DATA L( 14) /   84204631 /
      DATA L( 15) /   84616038 /
      DATA L( 16) /   84616285 /
      DATA L( 17) /   88201247 /
      DATA L( 18) /  105401605 /
      DATA L( 19) /  109516173 /
      DATA L( 20) /  110109288 /
      DATA L( 21) /  110109297 /
      DATA L( 22) /  112706900 /
      DATA L( 23) /  117911372 /
      DATA L( 24) /  121607079 /
      DATA L( 25) /  125110206 /
      DATA L( 26) /  125110314 /
      DATA L( 27) /  125110422 /
      DATA L( 28) /  126301458 /
      DATA L( 29) /  126302016 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  127010314 /
      DATA L( 32) /  127010422 /
      DATA L( 33) /  128409723 /
      DATA L( 34) /  128701126 /
      DATA L( 35) /  132813156 /
      DATA L( 36) /  143613527 /
      DATA L( 37) /  160006939 /
      DATA L( 38) /  161206939 /
      DATA L( 39) /  161207668 /
      DATA L( 40) /  161208397 /
      DATA L( 41) /  161214677 /
      DATA L( 42) /  171610341 /
      DATA L( 43) /  172704146 /
      DATA L( 44) /  174310341 /
      DATA L( 45) /  175404146 /
      DATA L( 46) /  177010341 /
      DATA L( 47) /  178104146 /
      DATA L( 48) /  187503178 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  221802916 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614274 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) /  240410400 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255909297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) /  260511709 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  261013269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261102916 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261200000 /
      DATA L( 78) /  261202916 /
      DATA L( 79) /  261205832 /
      DATA L( 80) /  261310746 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  267802728 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  272102016 /
      DATA L( 86) /  274204374 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  305706944 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  306411480 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  318100000 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  318106674 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) /  413213986 /
      DATA L(103) /  414911421 /
      DATA L(104) /  424009316 /
      DATA L(105) /  429605873 /
      DATA L(106) /  429802774 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /  463700000 /
      DATA L(115) /  464103753 /
      DATA L(116) /  470317741 /
      DATA L(117) /  470711664 /
      DATA L(118) /  471301278 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486508618 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) / 1587106870 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  608013167 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  672812393 /
      DATA L(133) /  673003645 /
      DATA L(134) /  673014580 /
      DATA L(135) /  673503645 /
      DATA L(136) /  673514580 /
      DATA L(137) /  673703645 /
      DATA L(138) /  694213270 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) /  708908795 /
      DATA L(147) /  710613169 /
      DATA L(148) /  808211318 /
      DATA L(149) /  808219107 /
      DATA L(150) /  808411318 /
      DATA L(151) /  808419107 /
      DATA L(152) /  817806980 /
      DATA L(153) /  817807268 /
      DATA L(154) /  817808438 /
      DATA L(155) /  817808726 /
      DATA L(156) /  877703969 /
      DATA L(157) /  878215462 /
      DATA L(158) /  879304637 /
      DATA L(159) /  889004027 /
      DATA L(160) /  889705651 /
      DATA L(161) /  901014580 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916003645 /
      DATA L(165) /  916014729 /
      DATA L(166) /  950802916 /
      DATA L(167) /  952402403 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806933 /
      DATA L(170) /  952809734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) /  963001247 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  984316173 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) /  989417307 /
      DATA L(187) /  990014811 /
      DATA L(188) /  990403645 /
      DATA L(189) /  992711372 /
      DATA L(190) /  996407079 /
      DATA L(191) /  999301054 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1001102016 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) / 1005614839 /
      DATA L(198) / 1007602304 /
      DATA L(199) / 1007602539 /
      DATA L(200) / 1018413527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078508748 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1131816819 /
      DATA L(211) / 1142504131 /
      DATA L(212) / 1170912165 /
      DATA L(213) / 1170914431 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1181704797 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) / 1198914369 /
      DATA L(220) / 1208118396 /
      DATA L(221) / 1216503486 /
      DATA L(222) / 1216509902 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1315813986 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1327303692 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327615003 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1354810314 /
      DATA L(236) / 1355014431 /
      DATA L(237) / 1389413543 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1398713211 /
      DATA L(243) / 1399803746 /
      DATA L(244) / 1400201216 /
      DATA L(245) / 1198914369 /
      DATA L(246) / 1402214580 /
      DATA L(247) / 1408213667 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410802916 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1426816173 /
      DATA L(255) / 1427004267 /
      DATA L(256) / 1427414580 /
      DATA L(257) / 1428402322 /
      DATA L(258) / 1429809429 /
      DATA L(259) / 1432814580 /
      DATA L(260) /  861403407 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
      DATA L(268) / 1443109630 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462102916 /
      DATA L(272) / 1462105832 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) / 1521616819 /
      DATA L(278) / 1539803724 /
      DATA L(279) / 1569614580 /
      DATA L(280) / 1620914580 /
      DATA L(281) / 1648509429 /
      DATA L(282) / 1701414796 /
C
C     ..................................................................
C
C     (3)   COMMANDS EXECUTED BY OMNIT IN NL(.).
C
      DATA LO( 1), LO( 2), LO( 3), LO( 4), LO( 5), LO( 6), LO( 7) /
     1      11300,   7102,   4631,   7082,  14406,  11664,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,   8883,   4797,   9524,  15091,   8316 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       5916,   2916,   8908,   8907,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1327604185, 1509108316 /
      DATA LD( 3), LD( 4) / 1224502403, 1325802916 /
      DATA LD( 5), LD( 6) /   80106676,  117906959 /
      DATA LD( 7), LD( 8) /  992706959, 1065606959 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014580 /
      DATA LW( 2,1), LW( 2,2) /  252613986, 1574100000 /
      DATA LW( 3,1), LW( 3,2) / 1036400000, 1169803645 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  463514391 /
      DATA LW(12,1), LW(12,2) /  398107013, 1398911317 /
      DATA LW(13,1), LW(13,2) / 1169803645, 1200314580 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1910710935 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1131800000 /
      DATA LW(16,1), LW(16,2) / 1355000000, 1443100000 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364, 1296103403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1216501487 /
      DATA LW(23,1), LW(23,2) / 1429507101, 1200314580 /
      DATA LW(24,1), LW(24,2) / 1045203645, 1200314580 /
      DATA LW(25,1), LW(25,2) / 1045203645, 1065011475 /
      DATA LW(26,1), LW(26,2) / 1045203645,  263111475 /
      DATA LW(27,1), LW(27,2) / 1045203645, 1030309173 /
      DATA LW(28,1), LW(28,2) / 1389111993, 1703006318 /
      DATA LW(29,1), LW(29,2) / 1389111993, 1703006258 /
      DATA LW(30,1), LW(30,2) / 1472615081, 1200314580 /
      DATA LW(31,1), LW(31,2) / 1472615081,  138613851 /
      DATA LW(32,1), LW(32,2) / 1472615081, 1473309734 /
      DATA LW(33,1), LW(33,2) /  879404131, 1200314580 /
      DATA LW(34,1), LW(34,2) /  879404131,  138613851 /
      DATA LW(35,1), LW(35,2) /  879404131, 1473309734 /
C
C     ..................................................................
C
C     (6)   UNIT COMMANDS IN ITP(.).
C
      DATA LU( 1), LU( 2) / 1325802916,  267800837 /
      DATA LU( 3), LU( 4) / 1726214715, 1400600000 /
      DATA LU( 5), LU( 6) /  270514580,  402704629 /
      DATA LU( 7), LU( 8) / 1328006943, 1415711664 /
      DATA LU( 9), LU(10) /  148808548,          0 /
C
C     ..................................................................
C
C     (7)   CENSOR XX IN ICP(.).
C
      DATA LCXX(1), LCXX(2), LCXX(3), LCXX(4), LCXX(5), LCXX(6)/
     1        8883,    4104,    5238,    5643,    9288,   10341/
C
C     ..................................................................
C
C     (8)   CALCOMP IN ICL(.).
C
      DATA LC( 1), LC( 2) / 1462303645, 1428803753 /
      DATA LC( 3), LC( 4) / 1412003645, 1200314580 /
      DATA LC( 5), LC( 6) / 1419016767,  442014580 /
      DATA LC( 7), LC( 8) / 1170704131,  138613851 /
C
C     ..................................................................
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DATA LF( 1), LF( 2) / 1062909516,  916010629 /
      DATA LF( 3), LF( 4) /  587104767, 1458000000 /
      DATA LF( 5), LF( 6) /  241214331,  514309504 /
      DATA LF( 7), LF( 8) /  437400000,  161300729 /
      DATA LF( 9), LF(10) / 1433810400, 1569604797 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  431313270, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  430911318 /
      DATA LF(17), LF(18) /  307512083,  916007094 /
      DATA LF(19), LF(20) / 1030700000, 1029006094 /
      DATA LF(21), LF(22) / 1029300000,  161110632 /
      DATA LF(23), LF(24) /  171511295, 1034801715 /
      DATA LF(25), LF(26) / 1207814379,  525309632 /
      DATA LF(27), LF(28) /  652304138,  317802678 /
      DATA LF(29), LF(30) / 1171002226, 1005614837 /
C
C     ..................................................................
C
C     (10)   PROPERTIES OF DISTRIBUTIONS IN IPROP(.).
C
      DATA LP( 1), LP( 2) /  306514114,  276715634 /
      DATA LP( 3), LP( 4) / 1181702336, 1316303334 /
      DATA LP( 5)         / 1198914369             /
C
C     ..................................................................
C
C     (11)   TABLE MAKING IN ITB(.).
C
      DATA LT( 1), LT( 2) /  486512965, 1443100000 /
      DATA LT( 3), LT( 4) /  132813156, 1439503073 /
      DATA LT( 5), LT( 6) /  973406933,  952806933 /
      DATA LT( 7), LT( 8) / 1316305238,  961606602 /
      DATA LT( 9), LT(10) / 1181702336, 1216512087 /
      DATA LT(11), LT(12) / 1355913208,  262413208 /
      DATA LT(13), LT(14) / 1357211382,  263711382 /
C
C     ..................................................................
C
C     (13)   COLUMN IN ICOLHD(.).
C
      DATA LCH(1), LCH(2), LCH(3), LCH(4), LCH(5), LCH(6), LCH(7)/
     1        'K',    'O',    'L',    'O',    'N',    'N',    'E'/
C
C     ==================================================================
C
      CALL MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
      RETURN
C
C     ==================================================================
C
      END
*NOTEPR
      SUBROUTINE NOTEPR
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. NOTEPR V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT OUT NOTE(1) THRU NOTE(120).
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1970.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /VECCHR/ NTPR(120)
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      CHARACTER NTPR*1 
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 60 /
C
C     ==================================================================
C
      IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)
      LX = LWIDE - IONE
      IF (LX.GE.ICA) GO TO 10
      WRITE (IPRINT,20) (NTPR(I),   I=1,LX)
      WRITE (IPRINT,20) (NTPR(I+60),I=1,LX)
      RETURN
C
C     ..................................................................
C
  10  WRITE (IPRINT,20) (NTPR(I),I=1,LX)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  20  FORMAT (1X,120A1)
C
C     ==================================================================
C
      END
*NOZIP
      SUBROUTINE NOZIP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  NOZIP V 7.00  5/18/90. **
C
C     ==================================================================
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      WRITE (IPRINT,10)
10    FORMAT (63H INSTALLATION MUST PROVIDE PROPRIETARY CALCOMP PLOT SUB
     1ROUTINE.)
      RETURN
      END
*OMCONV
      SUBROUTINE OMCONV (NWCD,KRD,KRDEND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/26/81. OMCONV V 7.00  9/12/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS ROUTINE CONVERTS INPUT CARD IMAGES TO A STANDARD CODE SO
C        THAT OMNITAB CAN DEAL WITH THE CHARACTERS AS INTEGERS.
C
C     ARRAY LA CONTAINS ALL THE CHARACTERS USED BY OMNITAB, FORMATTED 1H
C
C     THIS ROUTINE IS INCLUDED ONLY FOR COMPLETENESS.  IT SHOULD BE
C        REWRITTEN IN ASSEMBLY LANGUAGE FOR EACH COMPUTER.  ALSO, IT
C        CANNOT MEET ANS STANDARDS BECAUSE ANS DOES NOT REQUIRE THAT DATA
C        READ WITH FORMAT A1 BE STORED THE SAME AS HOLLERITH DATA SETUP
C        WITH 1H ALTHOUGH THEY WILL BE THE SAME ON MOST COMPUTERS.
C
C     ALSO, ANS DOES NOT RECOGNIZE THE APOSTROPHE CHARACTER '
C
C     SPECIAL NOTE FOR BURROUGHS COMPUTERS ...
C        BURROUGHS COMPUTERS NORMALLY DO NOT DISTINGUISH BETWEEN
C        HOLERITH BLANK AND HOLLERITH MINUS (AND HOLLERITH CENTS SIGN 
C        AND HOLLERITH 12-0).  OMNITAB MUST BE COMPILED USING AN
C        OPTION TO PACK FOUR CHARACTERS PER WORD.  ALTERNATIVELY, THE 
C        TWO COMMENT STATEMENTS BELOW CONTAINING .IS. SHOULD HAVE THE 
C        C IN COLUMN 1 REMOVED AND A C INSERTED IN COLUMN 1 OF THE
C        FOLLOWING EXECUTABLE INSTRUCTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION KRD(*), NWCD(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER LA*1
      CHARACTER NWCD*1, K*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 45 /
      DATA ICB / 47 /
      DATA ICC / 74 /
C
C     ==================================================================
C
C
      DO 70 I=1,KRDEND
        K = NWCD(I) 
C       SPECIAL CASE TO CHECK FOR BLANKS.
C
C       IF (.NOT.K.IS.LA(45)) GO TO 30
C
        IF (K.NE.LA(ICA)) GO TO 30
        J = ICA
        GO TO 60
C
C       THE UPPER BOUND OR LIMIT ON J MUST BE CHANGED IF MORE CHARACTERS
C          ARE ADDED TO THE VECTOR LA IN LABEL COMMON ABCDEF.
C
  30    DO 40 J=1,ICC
C
C         IF (K.IS.LA(J)) GO TO 50
C
          IF (K.EQ.LA(J)) GO TO 50
  40    CONTINUE
        J = ICB
  50    IF (J.LE.48) GO TO 60 
        J = J - 38
  60    KRD(I) = J - IONE
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*ORTHRV
      SUBROUTINE ORTHRV (A,NROW,N,NCOL,IND,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ORTHRV V 7.00  4/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CHECK TO SEE IF MATRIX A IS ORTHOGONAL.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IND(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*), X(*)
      REAL             TOLNCE 
C
C     ==================================================================
C
C     IF NUMBER OR ROWS IS GREATER THAN NUMBER OF COLUMNS COMPUTE A'A.
C        OTHERWISE   AA'.
C
      TOLNCE = RTEN * RER
      IF (N.GT.NCOL) GO TO 10 
      L2P = IONE
      MP  = N
      GO TO 20
C
  10  L2P = ITWO
      MP  = NCOL
  20  CALL MXTXQ (A,NROW,N,NCOL,X,L2P)
      IC  = IONE
      IND(1) = IZERO
      IND(2) = IZERO
      DO 80 I=1,MP
        DO 70 J=1,MP
          IF (I.EQ.J) GO TO 30
          IF (X(IC).EQ.RZERO) GO TO 60
          IF (ABS(X(IC))-TOLNCE) 40,40,90
  30      IF (X(IC).EQ.RONE) GO TO 60
          IF (ABS(X(IC)-RONE).GT.TOLNCE) GO TO 50 
  40      IND(2) = IONE
          GO TO 60
  50      IND(1) = ITWO
  60      IC = IC + IONE
  70    CONTINUE
  80  CONTINUE
      GO TO 100
C
  90  IND(1) = ITWO 
      IND(2) = ITWO 
      RETURN
C
C     ..................................................................
C
 100  IF (IND(1).EQ.IZERO .AND. IND(2).EQ.IONE) IND(1) = IONE
      IF (N.EQ.NCOL) RETURN
C
C     SET UP INDICATORS FOR RECTANGULAR MATRICES. 
C
      IF (L2P.EQ.ITWO) GO TO 120
      IF (IND(1).EQ.IONE) GO TO 110
      IND(1) = ITHRE
      IND(2) = ITHRE
      RETURN
C
C     ..................................................................
C
 110  IND(1) = -ITHRE
      IND(2) = -ITHRE
      RETURN
C
C     ..................................................................
C
 120  IF (IND(1).EQ.IONE) GO TO 130
      IND(1) = IFOUR
      IND(2) = IFOUR
      RETURN
C
C     ..................................................................
C
C     IND(1) = 0 EXACT  ORTHOGONAL
C     IND(1) = 1 RELATIVE (TOLNCE) ORTHOGONAL
C     IND(1) = 2 NON-ORTHOGONAL
C
C     INDICATORS FOR RECTANGULAR MATRICES.
C
C     IND(1) = -3 RELATIVE ORTHOGONAL  ROWWISE
C     IND(1) =  3 EXACT ORTHOGONAL ROWWISE
C     IND(1) = -4 RELATIVE ORTHOGONAL  COLUMNWISE 
C     IND(1) =  4 EXACT ORTHOGONAL COLUMNWISE
C     IND(2) = -1 DIAGONAL TERMS  ARE 1.0 SE
C     IND(2) =  0 EXACT ORTHOGONAL NORMALIZED
C     IND(2) =  1 RELATIVE ORTHOGONAL NORMALIZED
C     IND(2) =  2 NON-ORTHOGONAL
C     IND(2) = -3 RELATIVE ROWWISE (NORMALIZED)
C     IND(2) =  3 EXACT ROWWISE (NORMALIZED)
C     IND(2) = -4 RELATIVE COLUMNWISE  (NORMALIZED)
C     IND(2) =  4 EXACT COLUMNWISE (NORMALIZED)
C
 130  IND(1) = -IFOUR
      IND(2) = -IFOUR
      RETURN
C
C     ==================================================================
C
      END 
*OSCRWL
      SUBROUTINE OSCRWL (X,N,SVALUE,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. OSCRWL V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT PRODUCES SCRAWL OF N NUMBERS STORED IN X(.).
C
C     X IS ASSUMED TO BE SORTED.
C
C     FOR I = 1, 2, 3, 4, 5, 6,
C
C        IT STORES NUMBER OF MEASUREMENTS, SMALLEST MEASUREMENT,
C           LOWER HINGE, MEDIAN, UPPER HINGE AND LARGEST MEASUREMENT
C           SVALUE (I).
C
C     FOR LOWER HINGE, MEDIAN AND UPPER HINGE, LET
C        L = INTEGER PART OF (N+1)/2.
C
C     FOR LOWER HINGE, USE M = (L+1)/2
C              MEDIAN, USE M = L
C         UPPER HINGE, USE M = N + 1 - (L+1)/2.
C
C        IF M IS     AN INTEGER, USE X(M).
C           M IS NOT AN INTEGER, USE (X(M)+X(M+1))/2
C
C     IND = 0, IF EVERYTHING IS OK.
C           1, IF N IS LESS THAN 1.
C
C               ADAPTION OF SCRAWL WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -  JANUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             SVALUE(*), X(*)
      REAL             FDIV
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IND = IZERO
      IF (N.GE.IONE) GO TO 10 
      IND = IONE
      RETURN
C
C     ..................................................................
C
C     START COMPUTING.
C
  10  SVALUE(1) = FLOAT (N)
      SVALUE(2) = X(1)
      SVALUE(6) = X(N)
C
      DO 20 I=3,5
        NTOP = N + IONE
        IF (I.NE.IFOUR) NTOP = IDIV (NTOP,ITWO,IND) + IONE
        M = IDIV (NTOP,ITWO,IND)
        IF (I.EQ.IFIVE) M = N + IONE - M
        SVALUE(I) = X(M)
        IF (MOD(NTOP,ITWO).EQ.IZERO) GO TO 20
          IF (I.EQ.IFIVE) M = M - IONE
          SVALUE(I) = FDIV (X(M)+X(M+1),RTWO,IND) 
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*OUTPUT
      SUBROUTINE OUTPUT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. OUTPUT V 7.00 11/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     WRITE RECORD ON SCRATCH UNIT.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER NEWCRD*1
C
C     ==================================================================
C
      IF (NERROR.EQ.IZERO .AND. LLIST.EQ.IZERO) RETURN
      IF (MODE.EQ.ITHRE) GO TO 10
      WRITE (ISCRT,20) (NEWCRD(I),I=1,LENCRD)
      RETURN
C
C     ..................................................................
C
  10  I = IDIV (NSTMT,ITEN,IND)
      WRITE (ISCRT,30) I, (NEWCRD(J),J=1,LENCRD)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  20  FORMAT (4X,80A1)
  30  FORMAT (1H+,I3,80A1)
C
C     ==================================================================
C
      END 
*PACK
      SUBROUTINE PACK (NWORD,MWORD,MMWORD,NO,IP)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.   PACK V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     NWORD   CONTAINS CHARACTERS TO BE PACKED OR UNPACKED
C     MWORD   THE PACKED CHARACTERS IN CODED FORM (SEE BELOW) 
C     MMWORD  UNPACKED CHARACTERS
C     NO      NO OF CHARACTERS TO BE PACKED OR UNPACKED
C     IP ...
C             IP = 0, PACK
C             IP = 1, UNPACK
C
C     THE CHARACTERS ARE PACKED IN A CODED FORM. EACH CHARACTER HAS BEEN
C     ASSIGNED A VALUE IN OMCONV. THIS VALUE IS 1 LESS THAN THE
C     SUBSCRIPT OF LA (IN LABELED COMMON ABCDEF) FOR THAT PARTICULAR
C     CHARACTER. THESE VALUES ARE STORED IN KARD. THE VALUES OF THE
C     CHARACTERS ARE PACKED AS FOLLOWS
C
C        MWORD(I) = (KARD(K)+1)*2**16 + (KARD(K-1)+1)*2**8 +KARD(K-2) +1
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1969.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MWORD(*), MMWORD(*), NWORD(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER LA*1
      CHARACTER MMWORD*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA /   256 /
      DATA ICB /    45 /
      DATA ICC / 65536 /
C
C     ==================================================================
C
      KB = IONE
      KA = IONE
      IF (IP.EQ.IONE) GO TO 50
C
C     PACK
C
  10  MWORD(KA) = IZERO
      DO 20 I=1,3
        MWORD(KA) = MWORD(KA)*ICA + NWORD(KB) + IONE
        KB = KB + IONE
        IF (KB.GT.NO) GO TO 30
  20  CONTINUE
C
      KA = KA + IONE
      GO TO 10
C
  30  NCE = MOD(NO,ITHRE)
      IF (NCE.EQ.IZERO) RETURN
      NCE = ITHRE - NCE
  40  MWORD(KA) = MWORD(KA)*ICA + ICB
      NCE = NCE - IONE
      IF (NCE.EQ.IZERO) RETURN
      GO TO 40
C
C     UNPACK
C
  50  NCA = NWORD(KB)
      NCD = ICC
      DO 70 I=1,3
        NCB = IDIV (NCA,NCD,IND)
        IF (NCB.EQ.IZERO) GO TO 60
        MMWORD(KA) = LA(NCB)
        KA = KA + IONE
        IF (KA.GT.NO) RETURN
        NCA = NCA - NCB*NCD
  60    NCD = IDIV (NCD,ICA,IND)
  70  CONTINUE
      KB = KB + IONE
      GO TO 50
C
C     ==================================================================
C
      END
*PAGE
      SUBROUTINE PAGE (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.   PAGE V 7.00  5/12/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     BRING UP A NEW PAGE AND PRINT OMNITAB CARD AND PAGE NUMBER, THEN
C               IF J = 0, DONE
C                  J = 1, PRINT TITLE1
C                  J = 2, PRINT TITLE1, TITLE2
C                  J = 3, PRINT TITLE1, TITLE2, TITLE3
C                  J = 4, PRINT TITLE1, TITLE2, TITLE3, TITLE4
C
C     NCHTIT(.) CONTAIN NUMBER OF CHARACTERS UP TO THE LAST NON-BLANK
C               CHARACTER FOR TITLE1, 2, 3, AND 4.
C
C     WHEN CRT IS USED, PROGRAM PAUSES AT THE END OF A PAGE
C        SO THAT CRT CAN BE CLEARED.
C        AFTER CRT IS CLEARED TYPE A '.'
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IPG (7)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER LA*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER IPG*1
      CHARACTER CHPRT*1
      CHARACTER SCREEN*4
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IPG(1), IPG(2), IPG(3), IPG(4) / ' ','P','G',' '/
C
      DATA ICA /  9 /
      DATA ICB / 10 /
      DATA ICC / 60 /
      DATA ICD / 80 /
      DATA ICE / 88 /
      DATA ICF / 78 /
C
C     ==================================================================
C
      SCREEN(1:4) = CHAR(27)//CHAR(91)//CHAR(50)//CHAR(74)
      IF (NERROR.NE.IZERO) RETURN
      CHPRT = CHAR(12)
      IF (NDEMD.EQ.IZERO) GO TO 10
      IF (IPRINT.NE.NPRNT) GO TO 10
      IF (NPAGE.EQ.IZERO) GO TO 5 
      WRITE (IPRINT,170)
      READ (NUNIT(1),180,END=10 ) 
   5  IF (ISBFT.EQ.IONE) WRITE (IPRINT,250) SCREEN
  10  NPAGE = NPAGE + IONE
      LWA = LWIDE - IONE
      IF (LWIDE.GT.ICD) GO TO 60
      LW = LWIDE - ICA
      IF (NPAGE.NE.IONE) GO TO 20
      NOCARD(LW)   = LA(45)
      NOCARD(LW+1) = LA(26)
      NOCARD(LW+2) = LA(11)
      NOCARD(LW+3) = LA(17)
      NOCARD(LW+4) = LA(15)
      NOCARD(LW+5) = LA(45)
      NOCARD(LW+6) = LA(45)
      NOCARD(LW+7) = LA(45)
      NOCARD(LW+8) = LA( 2)
      GO TO 50
C
  20  NPG = NPAGE
      LPG = IZERO
      ND = IHRD
      DO 40 I=1,3
        NPGA = IDIV (NPG,ND,IND)
        NPG = MOD (NPG,ND)
        IF (NPGA.EQ.IZERO .AND. LPG.EQ.IZERO) GO TO 30
        LWW = LW + I + IFIVE
        NOCARD(LWW) = LA(NPGA+1)
        LPG = IONE
  30    ND = IDIV (ND,ITEN,IND)
  40  CONTINUE
C
  50  IF (NDEMD.EQ.IONE .AND. IPRINT.EQ.NPRNT) GO TO 120
      WRITE (IPRINT,200) CHPRT,(NOCARD(I),I=1,LWA)
      GO TO 130
C
  60  IF (LWIDE.LT.IHRD) GO TO 80
      IF (NDEMD.EQ.IONE .AND. IPRINT.EQ.NPRNT) GO TO 70
      WRITE (IPRINT,220) CHPRT,NOCARD, NPAGE
      GO TO 130
C
  70  WRITE (IPRINT,210) NOCARD, NPAGE
      GO TO 130
C
  80  IF (LWIDE.GT.ICE) GO TO 100
      LPG = IDIV (NPAGE,IHRD,IND)
      LPGA = IDIV (NPAGE-LPG*IHRD,ITEN,IND)
      LPGB = MOD (NPAGE,ITEN)
      IPG(5) = LA(LPG+1)
      IF (LPG.EQ.IZERO) IPG(5) = LA(45)
      IPG(6) = LA(LPGA+1)
      IF (LPG.EQ.IZERO .AND. LPGA.EQ.IZERO) IPG(6) = LA(45)
      IPG(7) = LA(LPGB+1)
      LWB = LWIDE - ICB
      IF (NDEMD.EQ.IONE .AND. IPRINT.EQ.NPRNT) GO TO 90
      WRITE (IPRINT,200) CHPRT,(NOCARD(I),I=1,LWB), IPG
      GO TO 130
C
  90  WRITE (IPRINT,190) (NOCARD(I),I=1,LWB), IPG
      GO TO 130
C
 100  IF (NDEMD.EQ.IONE .AND. IPRINT.EQ.NPRNT) GO TO 110
      WRITE (IPRINT,240) CHPRT,(NOCARD(I),I=1,ICF), NPAGE
      GO TO 130
C
 110  WRITE (IPRINT,230) (NOCARD(I),I=1,ICF), NPAGE
      GO TO 130
 120  WRITE (IPRINT,190) (NOCARD(I),I=1,LWA)
 130  IF (J.LE.IZERO .OR. J.GT.IFOUR) RETURN
C
      LT1 = MIN0 (ICC,LWIDE-IONE)
      LT2 = MIN0 (ICC,LWIDE-ICC)
      IF (LT2.LE.IZERO) LT2 = IZERO
      LT3    = LT1
      LT4    = LT2
      IF (NCHTIT(2).GT.IZERO .AND. LT2.NE.IZERO) GO TO 140
      LT = IFIVE
      IF (NCHTIT(1).NE.IZERO) LT = MIN0 (LT1,NCHTIT(1))
      WRITE (IPRINT,190) (ITLE(I,1),I=1,LT)
      GO TO 150
C
 140  LT  = LT1
      LTA = MIN0 (LT2,NCHTIT(2))
      WRITE (IPRINT,190) (ITLE(I,1),I=1,LT), (ITLE(I,2),I=1,LTA)
 150  IF (J.LE.ITWO) RETURN
      IF (NCHTIT(4).GT.IZERO.AND.LT4.NE.IZERO) GO TO 160
      LT = IFIVE
      IF (NCHTIT(3).NE.IZERO) LT = MIN0 (LT3,NCHTIT(3))
      WRITE (IPRINT,190) (ITLE(I,3),I=1,LT)
      RETURN
C
C     ..................................................................
C
 160  LT  = LT3
      LTA = MIN0 (LT4,NCHTIT(4))
      WRITE (IPRINT,190) (ITLE(I,3),I=1,LT), (ITLE(I,4),I=1,LTA)
      RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 170  FORMAT (' ... HIT ENTER KEY TO CONTINUE ...')
 180  FORMAT (1A1)
 190  FORMAT (1X,120A1)
 200  FORMAT (1A1,120A1)
 210  FORMAT (1X,80A1,2X,4HPAGE,I4)
 220  FORMAT (1A1,80A1,2X,4HPAGE,I4)
 230  FORMAT ( 1X,78A1,2X,4HPAGE,I4)
 240  FORMAT (1A1,78A1,2X,4HPAGE,I4)
 250  FORMAT (1X,A4)
 
C     ================================================================
C
      END
*PCTILE
      SUBROUTINE PCTILE (X,N,P,XP)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PCTILE V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO COMPUTE THE P TH. PERCENTILE, XP, FOR A DISCRETE
C        SET OF N SORTED MEASUREMENTS IN VECTOR X.
C        USES THE DEFINITION ON P111 OF SNEDECOR, 5TH ED.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X(*)
      REAL             P, XP
      REAL             PK, PN, QK
C
C     ==================================================================
C
      IF (P-RONE) 20,10,10
  10    XP = (X(N)) 
        RETURN
  20  IF (P) 30,30,40
  30    XP = (X(1)) 
        RETURN
  40  PN = P * FLOAT (N+IONE) 
      K  = PN
      PK = PN - FLOAT (K)
      QK = RONE - PK
      XP = QK*X(K) + PK*X(K+1)
C
      RETURN
C
C     ==================================================================
C
      END 
*PDECOM
      SUBROUTINE PDECOM (KN,KM,TOL,W,WCC,ISING,M1,Q,D,R)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PDECOM V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE PDECOM USES A MODIFIED GRAM-SCHMIDT ALGORITHM TO OBTAIN
C        AN ORTHOGONAL QR-DECOMPOSITION OF THE INPUT MATRIX GIVEN IN Q.
C
C     INPUT PARAMETERS ...
C
C     KN    NUMBER OF OBSERVATIONS.
C     KM    NUMBER OF UNKNOWN COEFFICIENTS.
C     TOL   TOLERANCE USED IN DETERMINING THE RANK OF MATRIX Q.
C     W     VECTOR (KN BY 1) OF WEIGHTS.
C     WCC   CONSTANT USED FOR WEIGHTS INSTEAD OF VECTOR W(.).
C              IF WCC = 0, VECTOR W IS USED FOR WEIGHTS.
C              OTHERWISE WEIGHTS EQUAL WC.
C
C     INPUT AND OUTPUT PARAMETER ...
C
C     Q     MATRIX OF SIZE (KN BY KM+1) STORED AS A VECTOR. 
C           ON ENTRY, Q CONTAINS THE KM X-VECTORS (POSSIBLY SCALED) AND
C              THE Y-VECTOR OF OBSERVATIONS (POSSIBLY SCALED).
C           ON EXIT, Q CONTAINS RESULTS OF THE DECOMPOSITION.
C
C     OUTPUT PARAMETERS ...
C
C     ISING PARAMETER WHICH INDICATES IF MATRIX Q WAS FOUND TO BE
C              SINGULAR.
C              ISING = 0 MEANS MATRIX Q WAS FOUND TO BE OF RANK KM.
C              ISING = 1 MEANS MATRIX Q WAS FOUND TO BE SINGULAR.
C     M1    COMPUTED RANK OF MATRIX Q.
C     D     VECTOR (KM+1 BY 1) OF DIAGONAL ELEMENTS OBTAINED IN THE
C              DECOMPOSITION. 
C     R     MATRIX OBTAINED IN THE DECOMPOSITION, STORED AS A VECTOR OF
C              LENGTH (KM+1)*(KM+2)/2.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C
C               ADAPTED TO OMNITAB BY - 
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING LABORATORY,
C                      APPLIED MATHEMATICS DIVISION,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -   AUGUST, 1977.
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
      REAL             D(*), Q(*), R(*), W(*)
      REAL             TOL, WCC
      REAL             DMAX, DS, RSJ, TOL2, WW
      REAL             FDIV, FDPCON
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      WW    = WCC
      ISING = IONE
      M     = KM
      N     = KN
      M1    = IZERO 
      M2    = IDIV (M*(M+IONE),ITWO,IRR)
      DO 10 J=1,M
        D(J) = RZERO
  10  CONTINUE
C
      DO 20 L=1,M2
        R(L) = RZERO
  20  CONTINUE
C
      TOL2 = TOL * TOL
      DMAX = RZERO
      DO 110 I=1,M
C
C     STEP NUMBER I IN THE DECOMPOSITION.
C
        DSUM = DZERO
        DO 30 L=1,N 
          IF (WCC.LE.RZERO) WW = W(L)
          J = (I-IONE) * N + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (Q(J)) * DBLE (WW)
  30    CONTINUE
C
        D(I) = FDPCON (DSUM)
        DS = D(I)
        IF (I.GT.IONE) GO TO 40
        DMAX = D(1) 
        GO TO 50
C
  40    IF (DS.GT.DMAX) DMAX = D(I)
  50    DO 60 J=1,I 
          IF (D(J).LE.TOL2*DMAX) RETURN 
  60    CONTINUE
C
        IF (DS.EQ.RZERO) RETURN
        IPLUS1 = I + IONE
        IF (IPLUS1.GT.M) GO TO 100
C
C     BEGIN ORTHOGONALIZATION.
C
        LD = IDIV (ITWO*(I-1)*M-I*I+ITHRE*I,ITWO,IRR)
        K = IONE
        DO 90 J=IPLUS1,M
          DSUM = DZERO
          DO 70 L=1,N
            IF (WCC.LE.RZERO) WW = W(L) 
            JS = (I-IONE) * N + L
            JJ = (J-IONE) * N + L
            DSUM = DSUM + DBLE(Q(JS)) * DBLE(Q(JJ)) * DBLE (WW)
  70      CONTINUE
C
          L = LD + K
          R(L) = FDPCON (DSUM)
          R(L) = FDIV (R(L),DS,IRR)
          RSJ  = R(L)
          K    = K + IONE
          JJ   = (J-IONE) * N + IONE
          JS   = (I-IONE) * N + IONE
          DO 80 L=1,N
            Q(JJ) = Q(JJ) - RSJ * Q(JS) 
            JJ    = JJ + IONE 
            JS    = JS + IONE 
  80      CONTINUE
C
  90    CONTINUE
C
C     END ORTHOGONALIZATION.
C
 100    M1 = I
        IF (I.EQ.M-IONE) ISING = IZERO
 110  CONTINUE
C
C     END STEP NUMBER I.
C
      RETURN
C
C     ==================================================================
C
      END 
*PERSAL
      SUBROUTINE PERSAL (XS,X,N,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PERSAL V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PREPARE DATA FOR ANALYSIS FOR STEM-AND-LEAF DISPLAY.
C
C     ALL COMPUTATIONS ARE FOR VARIABLES IN LABELED COMMON.
C
C     SORT DATA, XS, USING SCRATCH AREA, X.
C
C     DETERMINE SIGNATURE IN, IZ, IP.
C
C     PUT SCRAWL IN IC(6) AND SV(6).
C
C     CALL RFORMT TO DETERMINE FIELD WIDTH, NW, AND NUMBER OF
C        DECIMAL PLACES, NDSL.
C
C     NUMBERS IN XS ARE CHANGED TO ABSOLUTE VALUE OF SORTED DATA.
C
C     LOCATE POSITION OF FIRST SIGNIFICANT DIGIT OF ABSOLUTE
C        MAXIMUM, JSTRT. IPONTR, FROM MAINSL IS USED IN COMPUTING JSTRT.
C
C     LOCATE POSITION OF PERIOD, IPER.
C        (IB IS SCRATCH AREA FOR RFORMT)
C
C     IND = 0,    EVERYTHING IS OK.
C     IND = 2,    ALL MEASUREMENTS EQUAL.
C     IND = 3,    NW TOO LARGE.
C     IND =10,    ROUNDING PROBLEMS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1973.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SLCONS/ MXLIN, MXWDTH
      COMMON /SLICHR/ IB(40), IC(6)
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SLRVAR/ RI(5), SV(6)
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X(*), XS(*)
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER        IB*1, IC*1
C
C     ==================================================================
C
      IND = IZERO
      IF (MXWDTH.LT.ISIGD+IFOUR) GO TO 90
      CALL SORT (XS,X,N,IZERO)
      IF (XS(N).GT.XS(1)) GO TO 10
      IND = ITWO
      RETURN
C
C     ..................................................................
C
C     DETERMINE SIGNATURE.
C
  10  CALL INZP (XS,N,MN,MZ,MP,IND1)
      IF (IND1.GT.IZERO) GO TO 80
      IN   = MN
      IZ   = MZ
      IP   = MP
      CALL SCRAWL (XS,N,IC,SV,RI)
      CALL RFORMT (0,ISIGD,XS,X(1),N,MXWDTH,MW,MND,IB,IRF)
      NWSL = MW
      NDSL = MND
      IF (NWSL.LT.MXWDTH) GO TO 20
      IND  = ITHRE
      RETURN
C
C     ..................................................................
C
C     TAKE ABSOLUTE VALUE OF XS(I).
C
  20  IF (IN.EQ.IZERO) GO TO 40
      DO 30 IS=1,IN
        XS(IS) = ABS(XS(IS))
  30  CONTINUE
C
C     GET CHARACTER REPRESENTATION OF X(1).
C
  40  CALL RFORMT (1,ISIGD,X,XS(1),0,0,MW,MND,IB(1),IRF)
      JSTRT = IONE
      JST = MXWDTH + ITWO
      CALL RFORMT (1,ISIGD,X,XS(N),0,0,MW,MND,IB(JST),IRF)
      IF (XS(1).GT.XS(N)) JST = IONE
      IW = MXWDTH + IONE
      DO 50 IS=1,IW
        IF (IB(JST).NE.LA(45)) GO TO 60
        JST = JST + IONE
        JSTRT = JSTRT + IONE
  50  CONTINUE
C
  60  IF (JSTRT.LE.IZERO) JSTRT = IONE
      IPER = NWSL - JSTRT + IONE
      DO 70 IS=1,IW
        IF (IB(IS).NE.LA(38)) GO TO 70
        IPER = IS - JSTRT + IONE
        RETURN
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
  80  IND = ITEN
      RETURN
C
C     ..................................................................
C
  90  IND = ITHRE
      RETURN
C
C     ==================================================================
C
      END
*PHYCON
      SUBROUTINE PHYCON (NAME)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PHYCON V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PHYSICAL CONSTANT LIST
C
C     ENTRIES ARE IN PAIRS, FIRST MKS VALUE, THEN CGS (ELECTROMAGNETIC).
C
C        PI        PI
C        E         BASE OF NATURAL LOGS 
C        C         SPEED OF LIGHT IN VACUUM
C        Q         ELEMENTARY CHARGE
C        N         AVOGADRO CONSTANT
C        ME        ELECTRON REST MASS
C        MP        PROTON REST MASS
C        F         FARADAY CONSTANT
C        H         PLANCK CONSTANT
C        ALPHA     FINE STRUCTURE CONSTANT
C        QME       CHARGE TO MASS RATIO FOR ELECTRON
C        RINF      RYDBERG CONSTANT
C        GAMMA     GYROMAGNETIC RATIO OF PROTON (CORRECTED FOR H2O)
C        MUB       BOHR MAGNETON
C        R         GAS CONSTANT
C        K         BOLTZMANN CONSTANT
C        CONE      FIRST RADIATION CONSTANT
C        CTWO      SECOND RADIATION CONSTANT
C        SIGMA     STEPHAN-BOLTZMANN CONSTANT
C        G         GRAVITATIONAL CONSTANT
C
C     IF NAME =  0, INSTRUCTION IS CGS
C             = -1, INSTRUCTION IS SI
C             GT 0, PICK UP APPROPRIATE PHYSICAL CONSTANT.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /PCONST/ PC(40), JPC, NT(40)
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
C
C     ==================================================================
C
      J = JPC
      IF (NAME.GT.IZERO) GO TO 10
      JPC = NAME
      RETURN
C
C     ..................................................................
C
  10  DO 20 I=1,20
        IF (NAME.EQ.NT(I)) GO TO 30
  20  CONTINUE
C
      ARG = RZERO
      RETURN
C
C     ..................................................................
C
  30  I = I + I + J 
      ARG = PC(I)
      RETURN
C
C     ==================================================================
C
      END 
*PINVRT
      SUBROUTINE PINVRT (M,R,D)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PINVRT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE PINVRT OBTAINS THE UNSCALED COVARIANCE MATRIX OF THE 
C        COEFFICIENTS, EQUAL TO THE INVERSE OF (X-TRANSPOSE)*W*X.
C        MATRIX R OBTAINED FROM SUBROUTINE PDECOM IS USED AS INPUT.
C        THIS MATRIX IS OVERWRITTEN AND ON EXIT WILL EQUAL THE DESIRED
C        INVERSE.
C
C     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
C        ABOVE THE PRINCIPAL DIAGONAL IS STORED.
C
C     LET V DENOTE THE INVERSE OF MATRIX (X-TRANSPOSE)*W*X.  ON EXIT, 
C        THE FIRST M ELEMENTS OF R ARE
C           V(1,1) V(1,2) ... V(1,M)
C        THE NEXT M-1 ELEMENTS OF R ARE 
C           V(2,2) V(2,3) ... V(2,M)
C        THE NEXT M-2 ELEMENTS OF R ARE 
C           V(3,3) V(3,4) ... V(3,M)
C        ETC.
C
C     INPUT PARAMETER ...
C
C     M    ORDER OF THE MATRIX WHOSE INVERSE IS SOUGHT (EQUAL TO THE
C             NUMBER OF COEFFICIENTS IN THE LEAST SQUARES FIT).
C
C     INPUT AND OUTPUT PARAMETER ...
C
C     R    ON ENTRY, R IS A MATRIX WHICH WAS OBTAINED IN THE QR-
C                    DECOMPOSITION.  STORED AS A VECTOR OF LENGTH
C                    (M+1)*(M+2)/2.
C          ON EXIT, R IS THE INVERSE OF (X-TRANSPOSE)*W*X.
C
C     INTERNAL PARAMETER ...
C
C     D    VECTOR (M+1 BY 1) USED AS WORK AREA.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C
C               ADAPTED TO OMNITAB BY - 
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING LABORATORY,
C                      APPLIED MATHEMATICS DIVISION,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-921-2315 
C                  ORIGINAL VERSION -   AUGUST, 1977.
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
      REAL             D(*), R(*)
      REAL             FDIV, FDPCON
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      DO 10 L=1,M
        LL = IDIV (ITWO*(L-IONE)*(M+IONE)-L*L+ITHRE*L,ITWO,IRR)
        R(LL) = FDIV (RONE,R(LL),IRR)
  10  CONTINUE
C
      IF (M.EQ.IONE) RETURN
      L = M
  20  J = L - IONE
      LJ = IDIV (ITWO*(J-IONE)*(M+IONE)-J*J+ITHRE*J,ITWO,IRR)
      INC = IZERO
      DO 30 K=L,M
        INC  = INC + IONE
        JK   = LJ + INC
        D(K) = R(JK)
  30  CONTINUE
C
      I = M
      DO 50 KA=J,M
        DSUM = DZERO
        IF (I.EQ.J) DSUM = DBLE (R(LJ)) 
        DO 40 K=L,M 
          JK    = MIN0 (K,I)
          LL    = IDIV (ITWO*(JK-IONE)*(M+IONE)-JK*JK+ITHRE*JK,ITWO,IRR)
          INC   = IABS (K-I)
          JK    = LL + INC
          DSUM = DSUM -DBLE (D(K)) * DBLE (R(JK)) 
  40    CONTINUE
        INC = I - J 
        JK = LJ + INC
        R(JK) = FDPCON (DSUM) 
        I = I - IONE
  50  CONTINUE
      L = L - IONE
      IF (L.GT.IONE) GO TO 20 
C
C     PACK VECTOR R.
C
      DO 70 I=2,M
        L = IDIV (ITWO*(I-IONE)*M-I*I+ITHRE*I,ITWO,IRR)
        DO 60 J=I,M 
          K = L + I - IONE
          R(L) = R(K)
          L = L + IONE
  60    CONTINUE
  70  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END 
*PIVOT
      SUBROUTINE PIVOT (XI,KP,N,MD,ND,NL)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  PIVOT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C              SYMETRIC PIVOT-RETURNS NEGATIVE INVERSE
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
C                   REGRESSIONS BY LEAPS AND BOUNDS
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
C                     G.M.FURNIVAL AND R.W.WILSON 
C               YALE UNIVERSITY AND U.S. FOREST SERVICE
C                           VERSION 11/11/74
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
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MD(ND,ND)
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             XI(NL) 
      REAL             B
      REAL             FDIV
C
C     ==================================================================
C
      ISUB1 = MD(N,N)
      XI(ISUB1) = FDIV (-RONE,XI(ISUB1),IND)
      DO 20 I=1,KP
        IF (I.EQ.N) GO TO 20
        ISUB2 = MD(I,N)
        ISUB3 = MD(N,N)
        B = XI(ISUB2) * XI(ISUB3)
        DO 10 J=I,KP
          ISUB4 = MD(I,J)
          ISUB5 = MD(J,N)
          IF (J.NE.N) XI(ISUB4) = XI(ISUB4) + B*XI(ISUB5)
  10    CONTINUE
        XI(ISUB2) = B
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*PLTPOS
      SUBROUTINE PLTPOS (X,N,XMIN,XMAX,L,NPOS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PLTPOS V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR DETERMINING PLOTTING POSITION, I, WHERE 
C        I = 1, 2, ... ,L
C
C     INPUT PARAMETERS ARE -
C
C           X = ARRAY OF MEASUREMENTS
C           N = LENGTH OF X ARRAY
C        XMIN = MINIMUM VALUE OF X(I)
C        XMAX = MAXIMUM VALUE OF X(I)
C           L = NUMBER OF PLOTTING POSITIONS
C
C     OUTPUT PARAMETERS ARE - 
C
C        NPOS = ARRAY OF PLOTTING POSITIONS, NPOS(I), WHERE 
C                  I = 1,2,...,N AND
C                  1 .LE. NPOS(I) .LE. L
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
C                  ORIGINAL VERSION -  JANUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NPOS(*)
C
      REAL             X(*)
      REAL             XMAX, XMIN
      REAL             DELTA, RANGE
      REAL             FDIV
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 1.5 /
C
C     ==================================================================
C
      RANGE = XMAX - XMIN
      DELTA = FDIV (RANGE,FLOAT(L-1),IND)
      DO 10 I=1,N
        NPOS(I) = SPCA + FDIV (X(I)-XMIN,DELTA,IND)
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*PLT24B
      SUBROUTINE PLT24B (IGRAPH,JGRAPH,YLABLE,XX,NPTOUT,NPTIN,KB,KD,KS,
     1                   KT,KW)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PLT24B V 7.00  8/27/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PLOT THE LAST TWO GRAPHS OF FOURPLOTS OR TWOPLOTS.
C
C     IF LWIDE = LWC, PLOT TWO GRAPHS ON BOTTOM OF PAGE,
C                     OTHERWISE START A NEW PAGE AND PLOT GRAPH FOUR
C                     BELOW GRAPH THREE.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -   AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IGRAPH(21,*)
      DIMENSION ISTORE(131), JGRAPH(*), JSTORE(66)
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
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X(1)
      REAL             XX(*)
      REAL             YLABLE(*)
C
C     .................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER*1      ISTORE, JGRAPH, JHOLD, JSTORE, JSYM, NHOLD
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
      LSUBXX = IONE
      LSUBIS = IONE
      LSUBJG = IONE
      LXIS   = LXAXIS - IONE
      IXRLT  = LXAXIS + IONE
      IXRRT  = ITWO * LXAXIS
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
      JGSTOP = LSUBJG + LXIS - IONE
C
      IYMID  = IDIV (IONE+LYAXIS,ITWO,IND)
      IYUQ   = IDIV (LYAXIS-IONE,IFOUR,IND) + IONE
      IYLQ   = IDIV (ITHRE*(LYAXIS-IONE),IFOUR,IND) + IONE
      IF (LWIDE.LT.LWC) GO TO 10
C
C     ..................................................................
C
C     PRINT BOTTOM HALF OF PAGE.
C
      WRITE (IPRINT,230)
C
      WRITE (IPRINT,180) (LHEAD(I),I=49,96)
C
C     PRINT HORIZONTAL BORDER AT TOP.
C
      WRITE (IPRINT,190) LA(45), (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40),
     1                   LA(45), LA(45), (JGRAPH(J),J=LSUBJG,JGSTOP),
     2                   LA(40)
      GO TO 20
C
C     SET UP FOR GRAPH THREE WHEN TWO PLOTS PER PAGE ARE TO BE DONE.
C
  10  CALL PAGE (IFOUR)
      WRITE (IPRINT,240) (LHEAD(I),I=49,72)
      WRITE (IPRINT,190) LA(45),(JGRAPH(J),J=LSUBJG,JGSTOP),LA(40)
C
  20  I3SUBY = 43
      I4SUBY = 64
      DO 70 I=1,LYAXIS
        MARK = MOD(I,IFIVE)
        MT = 11
        IF (MARK.EQ.IONE) MT = KT(3)
        CALL RFORMT (MT,KS(3),XX,YLABLE(I3SUBY),KB(3),0,
     1               KW(3),KD(3),ISTORE(LSUBIS),IRF)
        I3SUBY = I3SUBY + IONE
        ISTORE(LSUBL1) = LA(45)
        IF (MARK.EQ.IONE) MT = KT(4)
        CALL RFORMT (MT,KS(4),XX,YLABLE(I4SUBY),KB(4),0,
     1               KW(4),KD(4),ISTORE(LSUBL3),IRF)
        I4SUBY = I4SUBY + IONE
        ISTORE(LSUBL2) = LA(45)
C
C       UNPACK PLOTING CHARACTER.
C
        JSUBJG = LSUBJG
        KSUBJG = LSUBJG + LXAXIS - IONE
        DO 30 J=1,LXAXIS
          I1 = IDIV (MOD(IGRAPH(I,J),1000),IHRD,IND)
          IF (I1.EQ.IZERO) JGRAPH(JSUBJG) = LA(45)
          IF (I1.EQ.IONE) JGRAPH(JSUBJG)  = LA(38)
          IF (I1.EQ.ITWO) JGRAPH(JSUBJG)  = LA(41)
          KSUBJG = KSUBJG + IONE
          JSUBJG = JSUBJG + IONE
          I2     = IDIV (MOD(IGRAPH(I,J),10000),1000,IND)
          IF (I2.EQ.IZERO) JGRAPH(KSUBJG) = LA(45)
          IF (I2.EQ.IONE) JGRAPH(KSUBJG)  = LA(38)
          IF (I2.EQ.ITWO) JGRAPH(KSUBJG)  = LA(41)
  30    CONTINUE
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
        ISSTOP = LSUBIS + LBLEY1 - IONE
        JGSTOP = LSUBJG + LXAXIS - IONE
        IF (LWIDE.GE.LWC) GO TO 60
        JHOLD(I,1) = JSYM
        JJ         = IONE
        JSUBJG     = LSUBJG + IXRLT - IONE
        DO 40 IJ=IXRLT,IXRRT
          JJ          = JJ + IONE
          JHOLD(I,JJ) = JGRAPH(JSUBJG)
          JSUBJG = JSUBJG + IONE
  40    CONTINUE
        ISUBIS = LSUBIS
        DO 50 IJ=1,LBLEY1
          NHOLD(I,IJ) = ISTORE(ISUBIS+12)
          ISUBIS      = ISUBIS + IONE
  50    CONTINUE
C
C     PRINT PLOT 3 LINE BY LINE.
C
        WRITE (IPRINT,200) (ISTORE(K),K=LSUBIS,ISSTOP), JSYM,
     1                     (JGRAPH(J),J=LSUBJG,JGSTOP), JSYM
        GO TO 70
C
C     PRINT PLOTS 3 AND 4 LINE BY LINE.
C
  60    KSUBIS = LSUBIS + 12
        KSSTOP = LSUBIS + 23
        JSTART = LSUBJG + IXRLT - IONE
        JJSTOP = LSUBJG + IXRRT - IONE
        WRITE (IPRINT,200) (ISTORE(K),K=LSUBIS,ISSTOP), JSYM,
     1        (JGRAPH(J),J=LSUBJG,JGSTOP), JSYM,
     2        (ISTORE(K),K=KSUBIS,KSSTOP), JSYM,
     3        (JGRAPH(J),J=JSTART,JJSTOP), JSYM
C
  70  CONTINUE
C
C     PRINT HORIZONTAL BORDER AT BOTTOM.
C
      JSUBJ2 = LSUBJG
      DO 90 J2=1,LXIS,10
        J3     = J2 + 9
        JSUBJ1 = LSUBJG + J2 - IONE
        DO 80 J1=J2,J3
          JGRAPH(JSUBJ1) = LA(39)
          JSUBJ1         = JSUBJ1 + IONE
  80    CONTINUE
        JGRAPH(JSUBJ2) = LA(40)
        JSUBJ2         = JSUBJ2 + ITEN
  90  CONTINUE
      JGSTOP = LSUBJG + LXIS - IONE
      IF (LWIDE.GE.LWC) WRITE (IPRINT,190) LA(45),
     1             (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40),
     2             LA(45), LA(45), (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40)
      IF (LWIDE.LT.LWC) WRITE (IPRINT,190) LA(45),
     1                             (JGRAPH(J),J=LSUBJG,JGSTOP), LA(40)
C
C     PRINT HORIZONTAL SCALE.
C
      ISUBIS = LSUBIS
      DO 100 LJ=1,131
        ISTORE(ISUBIS) = LA(45)
        ISUBIS = ISUBIS + IONE
 100  CONTINUE
C
      LLJ    = 7
      JSUBXX = LSUBXX + ITWO
      JSUBIS = LSUBIS + LLJ - IONE
      DO 130 LJ=3,24,4
C
C       LEFT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(JSUBXX),1,MS,MS+IFIVE,ISTORE(JSUBIS),0,MW,MD,
     1              MWX,MDX)
        IF (MWX.LT.9) GO TO 110
        LT = IONE
        MS = IFOUR
        CALL RFORMT(0,MS,XX(JSUBXX),X(1),1,9,MWX,MDX,ISTORE(LSUBIS),IRF)
 110    MMW = 9 - MWX
        CALL RFORMT(LT,MS,X,XX(JSUBXX),MMW,0,MWX,MDX,ISTORE(JSUBIS),IRF)
C
C       RIGHT PLOT.
C
        LT = 7
        MS = MIN0 (ISIGD-IONE,7)
        CALL MINNW (XX(JSUBXX+1),IONE,MS,MS+IFIVE,ISTORE(JSUBIS+66),0,
     1              MW,MD,MWX,MDX)
        IF (MWX.LT.9) GO TO 120
        LT = IONE
        MS = IFOUR
        CALL RFORMT (0,MS,XX(JSUBXX+1),X(1),1,9,MWX,MDX,ISTORE(LSUBIS),
     1               IRF)
 120    MMW = 9 - MWX
        CALL RFORMT (LT,MS,X,XX(JSUBXX+1),MMW,0,MWX,MDX,
     1               ISTORE(JSUBIS+66),IRF)
        JSUBIS = JSUBIS + ITEN
        JSUBXX = JSUBXX + IFOUR
 130  CONTINUE
C
      JKSTOP = LSUBIS + 130
      LJSTOP = LSUBIS + 65
      JSUBIS = LJSTOP + IONE
      IF (LWIDE.GE.LWC) GO TO 150
      WRITE (IPRINT,210) (ISTORE(LJ),LJ=LSUBIS,LJSTOP)
      WRITE (IPRINT,220) NPTIN(3),NPTOUT(3)
C
C     SAVE HORIZONTAL SCALE FOR GRAPH FOUR.
C
      ISUBIS = LSUBIS + 66
      DO 140 LJ=1,65
        JSTORE(LJ) = ISTORE(ISUBIS)
        ISUBIS     = ISUBIS + IONE
 140  CONTINUE
      GO TO 160
C
 150  WRITE (IPRINT,210) (ISTORE(LK),LK=LSUBIS,JKSTOP)
      WRITE (IPRINT,220) NPTIN(3),NPTOUT(3),
     1                   NPTIN(4),NPTOUT(4)
      RETURN
C
C     ..................................................................
C
C     PRINT FOURTH GRAPH BELOW THIRD GRAPH BECAUSE LWIDE IS NOT 120.
C
 160  WRITE (IPRINT,230)
      WRITE (IPRINT,240) (LHEAD(I),I=73,96)
      WRITE (IPRINT,190) LA(45),(JGRAPH(J),J=LSUBJG,JGSTOP),LA(40),
     1                   LA(45)
      DO 170 I=1,LYAXIS
C
C       PRINT PLOT 4 LINE BY LINE.
C
        WRITE (IPRINT,200) (NHOLD(I,J),J=1,LBLEY1), (JHOLD(I,K),K=1,52),
     1                      JHOLD(I,1)
 170  CONTINUE
      WRITE (IPRINT,190) LA(45),(JGRAPH(J),J=LSUBJG,JGSTOP),LA(40)
      WRITE (IPRINT,210) (JSTORE(LJ),LJ=1,65)
      WRITE (IPRINT,220) NPTIN(4),NPTOUT(4)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 180  FORMAT (1H ,19X,8HPLOT OF ,12A1,8H VERSUS ,12A1,
     1            26X,8HPLOT OF ,12A1,8H VERSUS ,12A1)
 190  FORMAT (1H ,12X,53A1,13X,53A1)
 200  FORMAT (1H ,12A1,53A1,1X,12A1,53A1)
 210  FORMAT (1H ,131A1)
 220  FORMAT (   /2X,2(8X,I4,16H POINTS PLOTTED ,I4,
     1   33H POINTS OUT OF BOUNDS NOT PLOTTED)/)
 230  FORMAT (1H )
 240  FORMAT (1H ,19X,8HPLOT OF ,12A1,8H VERSUS ,12A1)
C
C     ==================================================================
C
      END
 
*POICDF
      SUBROUTINE POICDF (X,ALAMBA,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. POICDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE X
C              FOR THE POISSON DISTRIBUTION
C              WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = ALAMBA.
C              THE POISSON DISTRIBUTION USED
C              HEREIN HAS MEAN = ALAMBA
C              AND STANDARD DEVIATION = SQRT(ALAMBA).
C              THIS DISTRIBUTION IS DEFINED FOR
C              ALL DISCRETE NON-NEGATIVE INTEGER  X--X = 0, 1, 2, ... .
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = EXP(-ALAMBA) * ALAMBA**X / X!.
C              THE POISSON DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF EVENTS
C              IN THE INTERVAL (0,ALAMBA) WHEN
C              THE WAITING TIME BETWEEN EVENTS
C              IS EXPONENTIALLY DISTRIBUTED
C              WITH MEAN = 1 AND STANDARD DEVIATION = 1.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE
C                                AT WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE AND
C                                INTEGRAL-VALUED.
C                     --ALAMBA = THE SINGLE PRECISION VALUE
C                                OF THE TAIL LENGTH PARAMETER.
C                                ALAMBA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF
C             FOR THE POISSON DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER = ALAMBA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE AND INTEGRAL-VALUED.
C                 --ALAMBA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--THE SINGLE PRECISION TAIL LENGTH
C              PARAMETER ALAMBA IS     NOT     RESTRICTED
C              TO ONLY INTEGER VALUES.
C              ALAMBA CAN BE SET TO ANY POSITIVE REAL
C              VALUE--INTEGER OR NON-INTEGER.
C            --NOTE THAT EVEN THOUGH THE INPUT
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
C     REFERENCES--JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1969, PAGES 87-121,
C                 ESPECIALLY PAGE 114, FORMULA 93.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGE 112.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5,
C                 AND PAGE 929.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 146-154.
C               --COX AND MILLER, THE THEORY OF STOCHASTIC
C                 PROCESSES, 1965, PAGE 7.
C               --GENERAL ELECTRIC COMPANY, TABLES OF THE
C                 INDIVIDUAL AND CUMULATIVE TERMS OF POISSON
C                 DISTRIBUTION, 1962.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGES 259-261.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
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
      REAL             ALAMBA, CDF, X
      REAL             DEL, FINTX, GCDF, SPCHI
      REAL             FDPCON
      REAL             SPCA, SPCB
C
      DOUBLE PRECISION TERM(1)
      DOUBLE PRECISION DX, CHI, SUM, AI, DGCDF
      DOUBLE PRECISION FDDIV, FDSQRT, FDEXP
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 0.0001 /
      DATA SPCB / 0.001  /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (ALAMBA.GT.RZERO) GO TO 10
        IND = ITHRE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (X.GE.RZERO) GO TO 20
        IND = IONE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  20  INTX = X + SPCA
      FINTX = INTX
      DEL = X - FINTX
      IF (DEL.LT.RZERO) DEL = -DEL
      IF (DEL.GT.SPCB) IND = ITWO
C
C     ---   START POINT   ----------------------------------------------
C
C     EXPRESS THE POISSON CUMULATIVE DISTRIBUTION
C     FUNCTION IN TERMS OF THE EQUIVALENT CHI-SQUARED
C     CUMULATIVE DISTRIBUTION FUNCTION,
C     AND THEN EVALUATE THE LATTER.
C
      DX = ALAMBA
      DX = DTWO * DX
      NU = X + SPCA
      NU = ITWO * (IONE+NU)
C
      CHI = FDSQRT (DX)
      IEVODD = NU - ITWO * IDIV (NU,ITWO,JIND)
      CALL DSUMAL (TERM,IZERO,SUM)
      IF (IEVODD.EQ.IZERO) GO TO 30
C
      SUM     = DZERO
      TERM(1) = FDDIV (DONE,CHI,JIND)
      IMIN    = IONE
      IMAX    = NU - IONE
      GO TO 40
C
  30  SUM     = DONE
      TERM(1) = DONE
      CALL DSUMAL (TERM,-IONE,SUM)
      IMIN    = ITWO
      IMAX    = NU - ITWO
C
  40  IF (IMIN.GT.IMAX) GO TO 60
      DO 50 I=IMIN,IMAX,2
        AI = I
        TERM(1) = TERM(1) * FDDIV (DX,AI,JIND)
        CALL DSUMAL (TERM,-IONE,SUM)
  50  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  60  SUM = SUM * FDEXP (FDDIV (-DX,DTWO,JIND))
      IF (IEVODD.EQ.IZERO) GO TO 70
      SUM = (FDSQRT(FDDIV(DTWO,DPI,JIND))) * SUM
      SPCHI = CHI
      CALL NORCDF (SPCHI,GCDF)
      DGCDF = GCDF
      SUM = SUM + DTWO * (DONE-DGCDF)
  70  CDF = FDPCON (SUM)
C
      RETURN
C
C     ==================================================================
C
      END
*POIPDF
      SUBROUTINE POIPDF (X,PARAM,PDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. POIPDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE THE POISSON P.D.F.
C
C     USES RECURRENCE RELATION IN DOUBLE PRECISION.
C
C        F(X+1) = PARAM * F(X).
C                 -----
C                  X+1
C
C     IND = FAULT INDICATOR.
C
C        IND = 0, EVERYTHING IS OK.
C            = 2, PARAM OUTSIDE RANGE, I.E., NEGATIVE.
C            = 3, X OUTSIDE RANGE, I.E., NEGATIVE.
C            = 4, X NONINTEGRAL.
C
C               WRITTEN BY -
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
      REAL             PARAM, PDF, X
      REAL             RIX, RX
      REAL             FDPCON
C
      DOUBLE PRECISION DPARAM
      DOUBLE PRECISION DEN, TERM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FDDIV, FDEXP
C
C     ==================================================================
C
      IND = IZERO
      RX  = X
      IF (RX.GE.RZERO) GO TO 10
        IND = ITHRE
        PDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (PARAM.GT.RZERO) GO TO 20
        IND = ITWO
        PDF = RZERO
C
C     ..................................................................
C
  20  M   = RX
      RIX = M
      IF (RX.EQ.RIX) GO TO 30
        IND = IFOUR
C
C     ==================================================================
C
  30  DPARAM = PARAM
      DEN    = DONE
      TERM   = FDEXP (-DPARAM)
      IF (M.EQ.IZERO) GO TO 50
C
      DO 40 I=1,M
        TERM = FDDIV (DPARAM,DEN,JIND) * TERM
        DEN  = DEN + DONE
  40  CONTINUE
C
  50  PDF = FDPCON (TERM)
      RETURN
C
C     ==================================================================
C
      END
*POIPPF
      SUBROUTINE POIPPF (P,ALAMBA,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. POIPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
C              FOR THE POISSON DISTRIBUTION
C              WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = ALAMBA.
C              THE POISSON DISTRIBUTION USED
C              HEREIN HAS MEAN = ALAMBA
C              AND STANDARD DEVIATION = SQRT(ALAMBA).
C              THIS DISTRIBUTION IS DEFINED FOR
C              ALL DISCRETE NON-NEGATIVE INTEGER  X--X = 0, 1, 2, ... .
C              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
C              F(X) = EXP(-ALAMBA) * ALAMBA**X / X!.
C              THE POISSON DISTRIBUTION IS THE
C              DISTRIBUTION OF THE NUMBER OF EVENTS
C              IN THE INTERVAL (0,ALAMBA) WHEN
C              THE WAITING TIME BETWEEN EVENTS
C              IS EXPONENTIALLY DISTRIBUTED
C              WITH MEAN = 1 AND STANDARD DEVIATION = 1.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (INCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALAMBA = THE SINGLE PRECISION VALUE
C                                OF THE TAIL LENGTH PARAMETER.
C                                ALAMBA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
C             FUNCTION VALUE PPF
C             FOR THE POISSON DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER = ALAMBA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--ALAMBA SHOULD BE POSITIVE.
C                 --P SHOULD BE BETWEEN 0.0 (INCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF, POICDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, DEXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION AND DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--THE SINGLE PRECISION TAIL LENGTH
C              PARAMETER ALAMBA IS     NOT     RESTRICTED
C              TO ONLY INTEGER VALUES.
C              ALAMBA CAN BE SET TO ANY POSITIVE REAL
C              VALUE--INTEGER OR NON-INTEGER.
C            --NOTE THAT EVEN THOUGH THE OUTPUT
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
C                 DISTRIBUTIONS, 1969, PAGES 87-121,
C                 ESPECIALLY PAGE 102, FORMULA 36.1.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 108-113.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 929.
C               --FELLER, AN INTRODUCTION TO PROBABILITY
C                 THEORY AND ITS APPLICATIONS, VOLUME 1,
C                 EDITION 2, 1957, PAGES 146-154.
C               --COX AND MILLER, THE THEORY OF STOCHASTIC
C                 PROCESSES, 1965, PAGE 7.
C               --GENERAL ELECTRIC COMPANY, TABLES OF THE
C                 INDIVIDUAL AND CUMULATIVE TERMS OF POISSON
C                 DISTRIBUTION, 1962.
C               --OWEN, HANDBOOK OF STATISTICAL
C                 TABLES, 1962, PAGES 259-261.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
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
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             ALAMBA, P, PPF
      REAL             AMEAN, PF0, P0, P1, P2
      REAL             SD, X0, X1, X2, ZPPF
      REAL             FDPCON, FSQRT
C
      DOUBLE PRECISION DLAMBA
      DOUBLE PRECISION FDEXP
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
  10  IF (ALAMBA.GT.RZERO) GO TO 20
        IND = ITWO
        PPF = RZERO
        RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  20  DLAMBA = ALAMBA
      PPF    = RZERO
      IX0    = IZERO
      IX1    = IZERO
      IX2    = IZERO
      P0     = RZERO
      P1     = RZERO
      P2     = RZERO
C
C     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
C        1) P  =  RZERO
C        2) PPF  =  0
C
      IF (P.EQ.RZERO) GO TO 30
      PF0 = FDPCON ( FDEXP (-DLAMBA) )
      IF (P.LE.PF0)   GO TO 30
      GO TO 40
C
  30  PPF = RZERO
      RETURN
C
C     ..................................................................
C
C     DETERMINE AN INITIAL APPROXIMATION TO THE POISSON
C     PERCENT POINT BY USE OF THE NORMAL APPROXIMATION
C     TO THE POISSON.
C     (SEE JOHNSON AND KOTZ, DISCRETE DISTRIBUTIONS,
C     PAGE 102, FORMULA 36.IONE).
C
  40  AMEAN = ALAMBA
      SD    = FSQRT (ALAMBA)
      CALL NORPPF (P,ZPPF,IND)
      X2    = AMEAN - RONE + ZPPF * SD
      IX2   = X2
C
C     CHECK AND MODIFY (IF NECESSARY) THIS INITIAL
C        ESTIMATE OF THE PERCENT POINT
C        TO ASSURE THAT IT BE NON-NEGATIVE.
C
      IF (IX2.LT.IZERO) IX2 = IZERO
C
C     DETERMINE UPPER AND LOWER BOUNDS ON THE DESIRED
C        PERCENT POINT BY ITERATING OUT (BOTH BELOW AND ABOVE)
C        FROM THE ORIGINAL APPROXIMATION AT STEPS
C        OF 1 STANDARD DEVIATION.
C        THE RESULTING BOUNDS WILL BE AT MOST
C        1 STANDARD DEVIATION APART.
C
      IX0 = IZERO
      IX1 = MXINT
      ISD = SD + RONE
      X2  = IX2
      CALL POICDF (X2,ALAMBA,P2,NIND)
C
      IF (P2.GE.P) GO TO 70
C
      IX0 = IX2
  50    IX2 = IX0 + ISD
        IF (IX2.GE.IX1) GO TO 100
        X2  = IX2
        CALL POICDF (X2,ALAMBA,P2,NIND)
        IF (P2.GE.P)    GO TO 60
        IX0 = IX2
      GO TO 50
C
  60  IX1 = IX2
      GO TO 100
C
  70  IX1 = IX2
  80    IX2 = IX1 - ISD
        IF (IX2.LE.IX0) GO TO 100
        X2  = IX2
        CALL POICDF (X2,ALAMBA,P2,NIND)
        IF (P2.LT.P)    GO TO 90
        IX1 = IX2
      GO TO 80
C
  90  IX0 = IX2
 100  IF (IX0.NE.IX1)   GO TO 110
      IF (IX0.NE.IZERO) GO TO 180
      IX1 = IX1 + IONE
C
C     COMPUTE POISSON PROBABILITIES FOR THE
C        DERIVED LOWER AND UPPER BOUNDS.
C
 110  X0 = IX0
      X1 = IX1
      CALL POICDF (X0,ALAMBA,P0,NIND)
      CALL POICDF (X1,ALAMBA,P1,NIND)
C
C     CHECK THE PROBABILITIES FOR PROPER ORDERING.
C
      IF (P0.LT.P .AND. P.LE.P1) GO TO 140
      IF (P0.EQ.P)  GO TO 120
      IF (P1.EQ.P)  GO TO 130
      GO TO 180
C
 120  PPF = IX0
      RETURN
C
C     ..................................................................
C
 130  PPF = IX1
      RETURN
C
C     ..................................................................
C
C     THE STOPPING CRITERION IS THAT THE LOWER BOUND
C        AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
C        CHECK TO SEE IF IX1  =  IX0 + 1;
C        IF SO, THE ITERATIONS ARE COMPLETE;
C        IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
C        CHECK PROBABILITIES, AND CONTINUE ITERATING
C        UNTIL IX1  =  IX0 + 1.
C
 140  IX0P1 = IX0 + IONE
      IF (IX1.EQ.IX0P1) GO TO 170
      IX2   = IDIV (IX0+IX1,ITWO,JIND)
      IF (IX2.EQ.IX0)  GO TO 180
      IF (IX2.EQ.IX1)  GO TO 180
      X2    = IX2
      CALL POICDF (X2,ALAMBA,P2,NIND)
      IF (P0.LT.P2 .AND. P2.LT.P1) GO TO 150
      IF (P2.LE.P0)  GO TO 180
      IF (P2.GE.P1)  GO TO 180
 150  IF (P2.LE.P)   GO TO 160
      IX1 = IX2
      P1  = P2
      GO TO 140
C
 160  IX0 = IX2
      P0  = P2
      GO TO 140
C
 170  PPF = IX1
      IF (P0.EQ.P) PPF = IX0
      RETURN
C
C     ..................................................................
C
C     COMPUTATIONAL PROBLEMS ENCOUNTERED.
C
 180  IND = IFOUR
      RETURN
C
C     ==================================================================
C
      END
*PORTUG
      SUBROUTINE PORTUG
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PORTUG V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION NOT AVAILABLE AT THIS TIME.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                      A337 ADMINISTRATION BUILDING
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER 1975.
C                   CURRENT VERSION -     APRIL 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(282), LO(25), LD( 8), LW(35,2), LU(10), LCXX(6)
      DIMENSION LC( 8), LF(30), LP( 5),   LT(14), LCH(7)
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      CHARACTER LCH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   ONE WORD COMMANDS IN IR(.).
C
      DATA L(  1) / 1315306651 /
      DATA L(  2) /   77804132 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) /  429414827 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82514040 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514769 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84602445 /
      DATA L( 14) /   84204631 /
      DATA L( 15) / 1323916038 /
      DATA L( 16) / 1323916285 /
      DATA L( 17) /   88201247 /
      DATA L( 18) /  105401605 /
      DATA L( 19) /  109516173 /
      DATA L( 20) / 1349409288 /
      DATA L( 21) / 1349409297 /
      DATA L( 22) /  112706900 /
      DATA L( 23) /  117911372 /
      DATA L( 24) / 1326904240 /
      DATA L( 25) /  124710611 /
      DATA L( 26) /  124710618 /
      DATA L( 27) /  125110422 /
      DATA L( 28) /  126301458 /
      DATA L( 29) / 1365602016 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  127010395 /
      DATA L( 32) /  127010422 /
      DATA L( 33) /  128409723 /
      DATA L( 34) / 1368001126 /
      DATA L( 35) /  132813156 /
      DATA L( 36) /  143613527 /
      DATA L( 37) /  160006939 /
      DATA L( 38) /  161206939 /
      DATA L( 39) /  161207668 /
      DATA L( 40) /  161208397 /
      DATA L( 41) /  161214677 /
      DATA L( 42) /  171610341 /
      DATA L( 43) /  172704146 /
      DATA L( 44) /  174310341 /
      DATA L( 45) /  175404146 /
      DATA L( 46) /  177010341 /
      DATA L( 47) /  178104146 /
      DATA L( 48) /  187503178 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  221806651 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614436 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) /  240410400 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255909297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) /  260511709 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  261013269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261105103 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261200000 /
      DATA L( 78) /  261205103 /
      DATA L( 79) /  261205832 /
      DATA L( 80) /  261310746 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  267802728 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  272102016 /
      DATA L( 86) /  274204374 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  305706944 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  306411480 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  318100000 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  318106678 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) /  413213986 /
      DATA L(103) /  414910935 /
      DATA L(104) /  424009316 /
      DATA L(105) /  429605873 /
      DATA L(106) /  429802774 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /   84715842 /
      DATA L(115) /  464100838 /
      DATA L(116) /  470317741 /
      DATA L(117) /  470711664 /
      DATA L(118) /  471301278 /
      DATA L(119) /  480013566 /
      DATA L(120) / 1170902431 /
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525601215 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  608013167 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  672812393 /
      DATA L(133) /  673003645 /
      DATA L(134) /  673014580 /
      DATA L(135) /  673503645 /
      DATA L(136) /  673514580 /
      DATA L(137) /  673703645 /
      DATA L(138) /  694213270 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) /  708908795 /
      DATA L(147) /  710613153 /
      DATA L(148) /  808211318 /
      DATA L(149) /  808219107 /
      DATA L(150) /  808411318 /
      DATA L(151) /  808419107 /
      DATA L(152) /  817806980 /
      DATA L(153) /  817807268 /
      DATA L(154) /  817808438 /
      DATA L(155) /  817808726 /
      DATA L(156) /  877703969 /
      DATA L(157) /  878215462 /
      DATA L(158) /  879304637 /
      DATA L(159) /  889004027 /
      DATA L(160) /  889705651 /
      DATA L(161) /  901014580 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916001224 /
      DATA L(165) /  916016617 /
      DATA L(166) /  950806651 /
      DATA L(167) /  677101054 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806913 /
      DATA L(170) /  952809734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  955913152 /
      DATA L(175) /  963001247 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406913 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  984316173 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) /  989417307 /
      DATA L(187) /  990014811 /
      DATA L(188) /  990403645 /
      DATA L(189) /  992711372 /
      DATA L(190) /  962404240 /
      DATA L(191) / 1363801054 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1001102016 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) / 1005614839 /
      DATA L(198) / 1007602304 /
      DATA L(199) / 1007602539 /
      DATA L(200) / 1018413527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078508748 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1410711993 /
      DATA L(211) / 1142503996 /
      DATA L(212) / 1170912165 /
      DATA L(213) / 1170914431 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1181704797 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) / 1506702232 /
      DATA L(220) / 1208118256 /
      DATA L(221) / 1216503486 /
      DATA L(222) / 1216509902 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  397416083 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1208815004 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1327303692 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327615003 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1354810314 /
      DATA L(236) / 1355014431 /
      DATA L(237) / 1389413543 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1398713211 /
      DATA L(243) / 1399803735 /
      DATA L(244) / 1400201216 /
      DATA L(245) /  878902232 /
      DATA L(246) / 1388207876 /
      DATA L(247) / 1408213667 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1400010935 /
      DATA L(250) / 1400011124 /
      DATA L(251) / 1400011151 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1327611281 /
      DATA L(255) / 1427004267 /
      DATA L(256) /  251214100 /
      DATA L(257) / 1428402322 /
      DATA L(258) / 1429809424 /
      DATA L(259) / 1432814580 /
      DATA L(260) / 1296103403 /
      DATA L(261) / 1400514627 /
      DATA L(262) /  417801278 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
      DATA L(268) / 1426901215 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462105103 /
      DATA L(272) / 1462105832 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) /  349908780 /
      DATA L(278) / 1539803724 /
      DATA L(279) / 1569614580 /
      DATA L(280) / 1620914580 /
      DATA L(281) / 1648509429 /
      DATA L(282) / 1701414796 /
C
C     ..................................................................
C
C     (3)   COMMANDS EXECUTED BY OMNIT IN NL(.).
C
      DATA LO( 1), LO( 2), LO( 3), LO( 4), LO( 5), LO( 6), LO( 7) /
     1      11300,   7102,   4631,   7082,  11709,    838,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,  15633,   4797,   9524,   6928,  13276 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       2216,   3727,  15658,   9339,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1327604185,  692813276 /
      DATA LD( 3), LD( 4) / 1181704959, 1325802916 /
      DATA LD( 5), LD( 6) /   80106676, 1337812155 /
      DATA LD( 7), LD( 8) /  973312155, 1046212155 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014580 /
      DATA LW( 2,1), LW( 2,2) /  252613986, 1574100000 /
      DATA LW( 3,1), LW( 3,2) / 1036400000, 1169803645 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  463514391 /
      DATA LW(12,1), LW(12,2) /  398107013, 1398911317 /
      DATA LW(13,1), LW(13,2) / 1169803645, 1200314580 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1910710935 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1131800000 /
      DATA LW(16,1), LW(16,2) / 1355000000, 1443100000 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364, 1296103403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1216501487 /
      DATA LW(23,1), LW(23,2) / 1429507101, 1200314580 /
      DATA LW(24,1), LW(24,2) / 1045203645, 1200314580 /
      DATA LW(25,1), LW(25,2) / 1045203645, 1065011475 /
      DATA LW(26,1), LW(26,2) / 1045203645,  263111475 /
      DATA LW(27,1), LW(27,2) / 1045203645, 1030309173 /
      DATA LW(28,1), LW(28,2) / 1389111993, 1703006318 /
      DATA LW(29,1), LW(29,2) / 1389111993, 1703006258 /
      DATA LW(30,1), LW(30,2) / 1472615081, 1200314580 /
      DATA LW(31,1), LW(31,2) / 1472615081,  138613851 /
      DATA LW(32,1), LW(32,2) / 1472615081, 1473309734 /
      DATA LW(33,1), LW(33,2) /  879404131, 1200314580 /
      DATA LW(34,1), LW(34,2) /  879404131,  138613851 /
      DATA LW(35,1), LW(35,2) /  879404131, 1473309734 /
C
C     ..................................................................
C
C     (6)   UNIT COMMANDS IN ITP(.).
C
      DATA LU( 1), LU( 2) / 1325802916,  267800837 /
      DATA LU( 3), LU( 4) / 1726214715, 1400600000 /
      DATA LU( 5), LU( 6) /  270514580,  402704629 /
      DATA LU( 7), LU( 8) / 1328006943, 1415711664 /
      DATA LU( 9), LU(10) /  148808548,          0 /
C
C     ..................................................................
C
C     (7)   CENSOR XX IN ICP(.).
C
      DATA LCXX(1), LCXX(2), LCXX(3), LCXX(4), LCXX(5), LCXX(6)/
     1        8883,    4104,    5238,    5643,    9288,   10341/
C
C     ..................................................................
C
C     (8)   CALCOMP IN ICL(.).
C
      DATA LC( 1), LC( 2) / 1462303645, 1428803753 /
      DATA LC( 3), LC( 4) / 1412003645, 1200314580 /
      DATA LC( 5), LC( 6) / 1419016767,  442014580 /
      DATA LC( 7), LC( 8) / 1170704131,  138613851 /
C
C     ..................................................................
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DATA LF( 1), LF( 2) / 1062909516,  916010629 /
      DATA LF( 3), LF( 4) /  587104767, 1458000000 /
      DATA LF( 5), LF( 6) /  241214331,  514309504 /
      DATA LF( 7), LF( 8) /  437400000,  161300729 /
      DATA LF( 9), LF(10) / 1433810400, 1569604797 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  431313270, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  430911318 /
      DATA LF(17), LF(18) /  307512083,  916007094 /
      DATA LF(19), LF(20) / 1030700000, 1029006094 /
      DATA LF(21), LF(22) / 1029300000,  161110632 /
      DATA LF(23), LF(24) /  171511295, 1034801715 /
      DATA LF(25), LF(26) / 1207814379,  525309632 /
      DATA LF(27), LF(28) /  652304138,  317802678 /
      DATA LF(29), LF(30) / 1171002226, 1005614837 /
C
C     ..................................................................
C
C     (10)   PROPERTIES OF DISTRIBUTIONS IN IPROP(.).
C
      DATA LP( 1), LP( 2) /  306514114,  276715634 /
      DATA LP( 3), LP( 4) / 1181702336, 1316303334 /
      DATA LP( 5)         / 1506702232             /
C
C     ..................................................................
C
C     (11)   TABLE MAKING IN ITB(.).
C
      DATA LT( 1), LT( 2) /  486512965, 1443100000 /
      DATA LT( 3), LT( 4) /  132813156, 1439503073 /
      DATA LT( 5), LT( 6) /  973406933,  952806933 /
      DATA LT( 7), LT( 8) / 1316305238,  961606602 /
      DATA LT( 9), LT(10) / 1181702336, 1216512087 /
      DATA LT(11), LT(12) / 1355913208,  262413208 /
      DATA LT(13), LT(14) / 1357211382,  263711382 /
C
C     ..................................................................
C
C     (13)   COLUMN IN ICOLHD(.).
C
      DATA LCH(1), LCH(2), LCH(3), LCH(4), LCH(5), LCH(6), LCH(7)/
     1        'C',    'O',    'L',    'U',    'N',    'A',    ' '/
C
C     ==================================================================
C
      CALL MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
      RETURN
C
C     ==================================================================
C
      END
*PREPAK   
      SUBROUTINE PREPAK (N,IA,LOC,LH,LHH,LFMTP,IND)   
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PREPAK V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     INPUT ...     
C         
C     N = 1,  PACK HEAD IN IHEAD. IF MORE THAN 50 HEADINGS ARE STORED,
C               STACK OF HEADINGS IS PUSHED DOWN AND BOTTOM ONES      
C               ARE DISCARDED.
C         2,  PICK UP PROPER FORMAT NUMBER IN IA AND STORE IN LFMTP.  
C         3,  UNPACK THE HEADING OF COLUMN LOC INTO LHH.     
C         
C     IA   COLUMN NUMBER FOR THE HEADING TO BE PACKED OR FORMAT DESIRED.        
C         
C     LOC  CONTAINS THE COLUMN NUMBER OF THE HEADING PREPAK IS TRYING 
C          TO FIND, OR THE NUMBER OF CHARACTERS TO BE PACKED IN IHEAD.
C         
C     OUTPUT ...    
C         
C     LHH   IS WHERE THE HEADING IS STORED AS A1 LEFT JUSTIFIED AFTER IT        
C           IS UNPACKED.      
C         
C     LFMTP PROPER FORMAT PICKED AND RETURNED IN LFMTP WHEN N = 2.    
C         
C     IND   INDICATOR. IF IND = 0, CALL TO PREPAK WAS O.K.  
C                            = 1, RESULTS WERE NOT OBTAINED.
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
      DIMENSION LH(*), LHH(*)       
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /PRHEAD/ IHEAD(6,50), NHEADS         
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER LA*1
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER LFMTP*80      
      CHARACTER LHH*1
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
C     THE FOLLOWING CONSTANT IS (45*256+45) * 256 + 45      
C         
      DATA IFBLNK / 2960685 / 
C         
      DATA ICA /  100000 /    
      DATA ICB /       6 /    
      DATA ICC /      12 /    
      DATA ICD /      45 /    
C         
C     ==================================================================        
C         
      IND = IZERO   
      IF (N.GT.ITHRE) GO TO 220         
      GO TO (10,130,150), N   
C         
C     ..................................................................        
C         
C     PACK HEADS.   
C         
  10  IF (IHCNT.EQ.IZERO) GO TO 60      
C         
C     IHEAD(1,I) = COLUMN NUMBER FOR THAT HEADING.
C         
      JCD = IONE    
      IF (IA.GT.ICA) JCD = ICA
      IAB = IDIV (IA,JCD,IND) 
      DO 20 I=1,IHCNT         
        IHD = IDIV (IHEAD(1,I),JCD,IND) 
        IF (IAB.EQ.IHD) GO TO 90        
  20  CONTINUE      
C         
      KB = IHCNT    
      IF (IHCNT.GE.NHEADS) GO TO 120    
  30  DO 50 I=1,KB  
        KA = KB - I + ITWO    
        DO 40 K=1,ICB         
          IHEAD(K,KA) = IHEAD(K,KA-1)   
  40    CONTINUE    
  50  CONTINUE      
C         
  60  IHCNT = IHCNT + IONE    
  70  IHEAD(1,1) = IA         
      DO 80 I=2,5   
        IHEAD(I,1) = IFBLNK   
  80  CONTINUE      
C         
      IHEAD(ICB,1) = IZERO    
      ILOC = MOD (IABS(LOC),IHRD)       
      CALL PACK (LH,IHEAD(2,1),LHH,ILOC,IZERO)        
      IHEAD(ICB,1) = LOC      
      RETURN        
C         
C     ..................................................................        
C         
  90  IF (IHEAD(ICB,I).GT.IZERO) GO TO 110        
      IF (LOC.LT.IZERO) GO TO 100       
      CALL ERROR (250)        
      IND = IONE    
      RETURN        
C         
C     ..................................................................        
C         
 100  CALL ERROR (260)        
C         
C     ..................................................................        
C         
 110  IF (I.EQ.IONE) GO TO 70 
      KB = I - IONE 
      GO TO 30      
C         
 120  KB = NHEADS - IONE      
      IHCNT = KB    
      CALL ERROR (229)        
      GO TO 30      
C         
C     FIND PROPER FORMAT.     
C         
 130  IF (IA.LT.ITWO .OR. IA.GT.7) GO TO 220      
      IF (IFMT(IA-1).EQ.' ') GO TO 220  
      LFMTP = IFMT(IA-1)      
      RETURN        
C         
C     ..................................................................        
C         
C     SEARCH FOR HEADING AND UNPACK.    
C        IF HEADING IS FOUND IND = 0, OTHERWISE IND = 1.    
C         
 150  IF (IHCNT.EQ.IZERO) GO TO 220     
      ILH = IDIV (LOC,ICA,IND)
      DO 170 I=1,IHCNT        
        IHD = IDIV (IHEAD(1,I),ICA,IND) 
        IF (IHD.EQ.IZERO .AND. ILH.GT.IZERO) GO TO 170      
        IF (IHD.EQ.IZERO .AND. ILH.LE.IZERO) GO TO 160      
        IF (ILH.EQ.IZERO) GO TO 170     
        IF (LOC.NE.IHEAD(1,I)) GO TO 170
        ILL = IDIV (IABS(IHEAD(ICB,I)),IHRD,IND)  
        IF (ILL.EQ.IA) GO TO 180        
        GO TO 170   
 160    IHD = MOD (IHEAD(1,I),ICA)      
        IF (LOC.EQ.IHD) GO TO 180       
 170  CONTINUE      
C         
C     NO HEADING FOUND.       
C         
      GO TO 220     
C         
 180  CALL PACK (IHEAD(2,I),LH,LHH,ICC,IONE)
C         
      IF (IHEAD(ICB,I).GE.IZERO) RETURN 
      IHD = MOD (IABS(IHEAD(ICB,I)),IHRD)         
      IB = IDIV (ICC-IHD,ITWO,IND) + MOD (IHD,ITWO)         
      IF (IB.EQ.IZERO) RETURN 
      DO 200 II=1,IB
        IBCD = ICC  
        DO 190 I=ITWO,ICC     
          LHH(IBCD) = LHH(IBCD-1)         
          IBCD = IBCD - IONE  
 190    CONTINUE    
 200  CONTINUE      
C         
      DO 210 I=1,IB 
        LHH(I) = LA(ICD)       
 210  CONTINUE      
      RETURN        
C         
C     ..................................................................        
C         
 220  IND = IND + IONE        
      RETURN        
C         
C     ==================================================================        
C         
      END 
*PROCHK
      SUBROUTINE PROCHK (A,NROW,N,NCOL,IVEC,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. PROCHK V 7.00   4/23/91.**
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO CHECK FOR DIAGONAL, NORMAL, SYMMETRIC,
C        SKEW-SYMMETRIC AND ORTHOGONAL MATRIX.
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
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IVEC(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             A(*), X(*)
C
C     ==================================================================
C
C     SET ALL INDICATORS TO NO CONDITION
C
      DO 10 I=1,5
        IVEC(I) = ITWO
  10  CONTINUE
C
C     TEST TO SEE IF WE HAVE A DIAGONAL MATRIX
C     IF YES  IVEC(1)=0   IF NO IVEC(1)=2
C
      IJJ = IONE
      DO 30 I=1,N
        IJ = IJJ
        DO 20 J=1,N
          IF (I.EQ.J) GO TO 15
          IF (A(IJ)) 40,15,40
  15      IJ = IJ + IONE
  20    CONTINUE
        IJJ = IJJ + NROW
  30  CONTINUE
C
      IVEC(1) = IZERO
      IVEC(2) = IZERO
      IVEC(3) = IZERO
      GO TO 50
  40  IVEC(1) = ITWO
C
C     CHECK FOR SYMMETRY
C
      CALL SYMW (A,NROW,N,IVEC(2))
C
C     CHECK FOR SKEW SYMMETRY
C
      CALL SKSYMV (A,NROW,N,IRV)
      IF (IRV.GE.ITHRE) IVEC(2) = IRV
      IF (IVEC(2).EQ.ITWO) GO TO 50
      IVEC(3) = IZERO
C
C     CHECK FOR ORTHOGONAL MATRIX
C     IF A IS ORTHOGONAL IVEC(4)=0   OTHERWISE IVEC(4)=2
C
  50  CALL ORTHRV (A,NROW,N,NCOL,IVEC(4),X)
      RETURN
C
C     ==================================================================
C
      END
*PRPLOT
      SUBROUTINE PRPLOT(Y,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. PRPLOT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1975.
C                   CURRENT VERSION -   APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION  INM(11),    INMM(15), IPR(151), KSPACE(6)
      DIMENSION MBOOL(5), MPRINT(125), MTIT(60)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             X(*), Y(*)
      REAL             XP(6), YSS(6)
      REAL             ERR, XDELTA, XL, XMAX, XMIN
      REAL             XR, XT, XXP, XXPX, YDELTA
      REAL             YL, YMAX, YMIN, YP, YS, YT
      REAL             FDIV
      REAL             SPCA, SPCB
C
C     ...................................................................
C
      CHARACTER LA*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER INM*1, INMM*1, IPR*1, MBOOL*1, MPRINT*1, MTIT*1
C
C     ...................................................................
C
      EQUIVALENCE (MTIT(1),ITLE(1,6))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MBOOL(1), MBOOL(2), MBOOL(3), MBOOL(4), MBOOL(5) /
     1          '.',      '*',      '+',      ',',      '-' /
C
      DATA SPCA / 20.0 /
C
C     ==================================================================
C
      N     = NRMAX
      LW    = IDIV (LWIDE-19,ITEN,IND)
      IF (LW.GE.IFOUR) GO TO 10
      CALL ERROR (246)
      RETURN
C
C     ..................................................................
C
  10  IF (NCRT.NE.IZERO) THEN
C 
C PLOT ONLY HAS MANY INTERVALS AS POSSIBLE. EACH MAIN INTERVAL HAS 5
C SUB-INTERVALS.
C
        ILOOPY = LENGTH/IFIVE + IONE
        JLOOPY = IFIVE
        IF (ILOOPY.GT.6) THEN
          ILOOPY = LENGTH/ITEN +IONE
          JLOOPY = ITEN
        ENDIF
      ELSE
C 
C     PLOT HAS  5 MAIN INTERVALS. EACH MAIN INTERVAL HAS 10
C     SUB-INTERVALS.
C
        ILOOPY = 6
        JLOOPY = ITEN
      ENDIF
C
      SPCB = (ILOOPY - IONE) * JLOOPY
      IF (LW.GT.ITEN) LW = ITEN
      IPT = LW * ITEN
      LWF = IDIV (IPT,ITWO,IND)
      IPTX = IPT + IONE
      IPTXX = IPTX + IONE
      ERR = RTEN * RER
      XMAX = X(1)
      XMIN = XMAX
      DO 30 I=1,N
        IF (XMAX.GE.X(I)) GO TO 20
        XMAX = X(I)
        GO TO 30
  20    IF (XMIN.LE.X(I)) GO TO 30
        XMIN = X(I)
  30  CONTINUE
C
      YMAX  = Y(1)
      YMIN  = YMAX
      DO 40 I=1,N
        IF (YMAX.LT.Y(I)) YMAX = Y(I)
        IF (YMIN.GT.Y(I)) YMIN = Y(I)
  40  CONTINUE
C
      IF (YMIN.EQ.YMAX .OR. XMIN.EQ.XMAX) GO TO 440
      YDELTA = FDIV (YMAX-YMIN,SPCB,IND)
      XDELTA = FDIV (XMAX-XMIN,FLOAT(IPT),IND)
      YL = YMAX - FDIV (YDELTA,RTWO,IND)
      YT = YMAX
      XP(1) = XMIN
      MWB = IDIV (LW,ITWO,IND)
      MWA = MWB + IONE
      IF (LW.EQ.7) MWA = IFIVE
      XP(MWA) = XMAX
      XR = SPCA * XDELTA
      KSPACE(1) = 20
      KSPACE(MWA) = 20
      MWD = 20
      MWC = ITWO
      IF (MOD(LW,ITWO).EQ.IONE .AND. LW.GT.IFIVE) GO TO 60
      IF (MOD(LW,ITWO).EQ.IONE .AND. LW.LE.IFIVE) GO TO 70
      DO 50 I=2,MWB
        KSPACE(I) = 20
        XP(I) = XP(I-1) + XR
  50  CONTINUE
C
      GO TO 90
  60  XP(2) = XP(1) + XR
      XP(4) = XP(5) - XR
      MWC = ITHRE
      KSPACE(4) = 20
      GO TO 80
  70  KSPACE(1) = 25
  80  XP(MWC) = XMIN + FDIV (XMAX-XMIN,RTWO,IND)
      MWD = IDIV (IPT,ITWO,IND) - (MWC-ITWO) * 20
      KSPACE(2) = MWD
      KSPACE(3) = MWD
  90  DO 100 J=1,100
        IPR(J) = MBOOL(5)
 100  CONTINUE
C
      DO 110 I=1,101,10
        IPR(I) = MBOOL(3)
 110  CONTINUE
C
      IF (MOD(LW,ITWO).EQ.IONE) IPR(LWF+1) = MBOOL(3)
      IF (XMIN*XMAX.GE.RZERO) GO TO 210
      DO 120 I=1,MWA
        IF (ABS(XP(I)).LE.ERR) XP(I) = RZERO
 120  CONTINUE
C
      DO 130 I=2,MWA
        IF (XP(I-1)*XP(I).LE.RZERO) GO TO 140
 130  CONTINUE
C
      GO TO 210
 140  JJ = MWD
      JZ = IZERO
      JX = I - ITWO
      XXP = XP(I-1) + XDELTA
      IF (MOD(LW,ITWO).EQ.IZERO) GO TO 170
      IF (LW.NE.IFIVE) GO TO 150
      JZ = JX
      JX = IZERO
      GO TO 170
 150  IF (JX.GT.IONE) GO TO 160
      IF (JX.EQ.IONE) GO TO 170
      JJ = 20
      GO TO 170
 160  IF (JX.EQ.ITHRE) JJ = 20
      JZ = JX - IONE
      JX = IONE
 170  DO 190 J=1,JJ
        IF (ABS(XXP).GT.ABS(XDELTA)) GO TO 180
        XXPX = ABS (XXP+XDELTA)
        IF (XXPX.GT.ABS(XDELTA)) GO TO 200
        IF (ABS(XXP).LE.XXPX) GO TO 200
 180    XXP = XXP + XDELTA
 190  CONTINUE
C
      J = 20
 200  NN = JX * 20 + JZ * MWD + J + IONE
      IPR(NN) = LA(34) 
 210  WRITE (IPRINT,480) (IPR(K),K=1,IPTX)
      ITB = IONE
      YSS(1)      = YMIN
      YSS(ILOOPY) = YMAX
      KLOOPY      = ILOOPY - IONE
      DO 220 I=2,KLOOPY
        YSS(I) = YSS(I-1) + FLOAT(JLOOPY) * YDELTA
 220  CONTINUE
C
      NWX = 11
      CALL RFORMT (0,5,YSS,X(1),6,NWX,NW,ND,INMM,IRF)
      DO 230 I=1,11
        INMM(I) = LA(45)
 230  CONTINUE
      NWX = IZERO
C
C     THE I LOOP CONTROLS THE 5 DIVISIONS OF THE Y ORDINATE.
C
      DO 370 I=1,ILOOPY
        L = IONE
C
C       THE J LOOP IS FOR EACH LINE OF PRINT WITHIN THE DIVISIONS.
C
        DO 360 J=1,JLOOPY
C
C         BLANK OUT PRINT BUFFER LINE.
C
          DO 240 K=1,IPTX
            MPRINT(K) = LA(45)
 240      CONTINUE
          K5 = IONE
          DO 280 K=1,N
            IF (Y(K).GT.YT) GO TO 270
            IF (Y(K).LE.YL) GO TO 270
            XL = XMIN
            XT = XMIN + FDIV (XDELTA,RTWO,IND)
            DO 260 KA=1,IPTX
              IF (X(K5).LT.XL) GO TO 250
              IF (X(K5).GE.XT) GO TO 250
              MPRINT(KA) = MBOOL(2)
 250          XL = XT
              XT = XT + XDELTA
 260          CONTINUE
 270        K5 = K5 + IONE
 280      CONTINUE
          YP = YT * YL
          YT = YL
          YL = YL - YDELTA
          GO TO (290,340), L
 290      YS = YT + FDIV (YDELTA,RTWO,IND)
          IF (I.EQ.6) YS = YMIN
          DO 300 NZ=1,11
            INM(NZ) = LA(45)
 300      CONTINUE
          IF (ABS(YS).LE.ERR) YS = RZERO
          CALL RFORMT (1,5,X,YS,0,0,NW,ND,INMM,IRF)
C
C         THIS PATH IS EXECUTED ONCE IN EVERY DIVISION OF THE Y-AXIS.
C            EVERY TENTH LINE, STARTING WITH ZERO LINE.
C
          NT = 11
          NY = IZERO
          NX = 11
          DO 320 NZ=1,11
            IF (INMM(NX).EQ.LA(45) .AND. NY.LE.IZERO) GO TO 310
            IF (INMM(NX).EQ.LA(45) .AND. NY.GT.IZERO) GO TO 330
            INM(NT) = INMM(NX)
            NT = NT - IONE
            NY = IONE
 310        NX = NX - IONE
 320      CONTINUE
 330      MPRINT(103) = LA(34)
          IF (YP.GT.RZERO) MPRINT(103) = MBOOL(3)
          MPRINT(IPTXX) = MPRINT(103)
          WRITE (IPRINT,450) MTIT(ITB), INM, MPRINT(103),
     1                      (MPRINT(IX),IX=1,IPTXX)
          L = ITWO
          IF (I.LT.ILOOPY) GO TO 350
          GO TO 380
 340      MPRINT(103) = MBOOL(5)
          IF (YP.LE.RZERO) MPRINT(103) = LA(34)
          MPRINT(IPTXX) = MPRINT(103)
C
C         PRINT LINE.
C
          WRITE (IPRINT,460) MTIT(ITB), MPRINT(103),
     1                      (MPRINT(IX),IX=1,IPTXX)
 350      ITB = ITB + IONE
 360    CONTINUE
 370  CONTINUE
C
 380  WRITE (IPRINT,480) (IPR(I),I=1,IPTX)
      NWX = 13
      CALL RFORMT (0,5,XP,X(1),MWA,NWX,NW,ND,MPRINT,IRF)
      NWX = IZERO
      MWC = MWA * 13
      DO 390 I=1,120
        MPRINT(I) = LA(45)
 390  CONTINUE
C
      JX = 7
      JJ = IZERO
      DO 430 K=1,MWA
        J = IZERO
        DO 400 I=1,13
          IPR(I) = LA(45)
 400    CONTINUE
        CALL RFORMT (1,5,X,XP(K),0,0,NW,ND,IPR,IRF)
        DO 410 I=1,13
          IF (IPR(I).EQ.LA(45)) GO TO 410
          J = J + IONE
          IF (J.EQ.IONE) JJ = I
 410    CONTINUE
        JZ = J
        IF (JZ.GT.ITEN) JZ = ITEN
        JY = JX + IDIV (13-JZ,ITWO,IND) + IDIV(JZ,ITWO,IND)
        DO 420 I=1,J
          MPRINT(JY) = IPR(JJ)
          JY = JY + IONE
          JJ = JJ + IONE
 420    CONTINUE
        JX = JX + KSPACE(K)
 430  CONTINUE
C
      JX = JX - 7
      WRITE (IPRINT,470) (MPRINT(I),I=1,JX)
      RETURN
C
C     ..................................................................
C
 440  CALL ERROR (241)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 450  FORMAT (1X,115A1)
 460  FORMAT (1X,A1,11X,103A1)
 470  FORMAT (120A1)
 480  FORMAT (14X,101A1)
C
C     ==================================================================
C
      END
*PUTCH
      SUBROUTINE PUTCH (NCHAR,N,NVECTR)
C
C **  NBS OMNITAB 1977 VERSION 6.01  1/ 1/81.  PUTCH V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PUT THE CHARACTER NCHAR INTO VECTOR NVECTR N TIMES.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NVECTR(*)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER*1   NCHAR, NVECTR
C
C     ==================================================================
C
      DO 10 I=1,N
        NVECTR(I) = NCHAR
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
       END
*QFORF
      SUBROUTINE QFORF (VNU1,VNU2,F,Q)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  QFORF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE AREA UNDERNEATH THE F-DISTRIBUTION IN THE RIGHT TAIL.
C
C     INPUT ...
C                 VNU1 = NUMERATOR   DEGREES OF FREEDOM
C                 VNU2 = DENOMINATOR DEGREES OF FREEDOM
C                    F = VALUE OF F-DISTRIBUTION
C     OUTPUT ...
C                    Q = PROBABILITY OF EXCEEDING F.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -
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
      REAL             F, Q, VNU1, VNU2
      REAL             A, B, DELTA
      REAL             V1, V2, X
      REAL             FDIV, FDPCON
C
      DOUBLE PRECISION BETAX, BETAAB, PDF, CDF
C
C     ==================================================================
C
C
      DELTA  = RTEN * RER
C
      NU1 = VNU1 + DELTA
      NU2 = VNU2 + DELTA
      IF (NU1.LT.IONE) NU1 = IONE
      IF (NU2.LT.IONE) NU2 = IONE
      V1  = NU1
      V2  = NU2
C
      A = FDIV (V2,RTWO,IND)
      B = FDIV (V1,RTWO,IND)
      X = FDIV (V2,V2+V1*F,IND)
C
      IF (X.LT.RZERO) X = RZERO
      IF (X.GT.RONE)  X = RONE
      CALL DIXAB (A,B,X,BETAX,BETAAB,PDF,CDF,IERR)
      Q = FDPCON (CDF)
      RETURN
C
C     ==================================================================
C
      END
*QUADLS
      SUBROUTINE QUADLS (WTY,Y,W,NW,N,A,COEFF,F)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. QUADLS V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO COMPUTE QUADRATIC TREND EFFECT AND F STATISTIC
C        FOR MEASUREMENT Y(I), A QUADRATIC FUNCTION OF I.
C
C     INPUT IS Y, W, NW AND N.
C        NW = 0, IF ALL WEIGHTS ARE EQUAL TO 1.0
C             1, IF ANY ONE WEIGHT IS NOT EQUAL TO 1.0
C
C     OUTPUT IS COEFF AND F.
C
C     WTY IS A SCATCH AREA FOR SQRT(W(I))*Y(I).
C     THE ARRAY A IS USED AS A SCRATCH AREA.
C
C     X(I) IS SET EQUAL TO (I-IBAR), WHERE IBAR = (N+1)/2
C
C     THIS SUBROUTINE WAS ADAPTED BY ROY H. WAMPLER AND M. STUART SCOTT,
C     NATIONAL BUREAU OF STANDARDS, WASHINGTON, D. C., JULY 1969, FROM
C     A SUBROUTINE CALLED 'BJORCK' WHICH WAS WRITTEN BY WILLIAM J. HALL,
C     NATIONAL BUREAU OF STANDARDS, BOULDER, COL.  THIS ROUTINE USES THE
C     MODIFIED GRAM-SCHMIDT ALGORITHM GIVEN BY AKE BJORCK IN 'SOLVING
C     LINEAR LEAST SQUARES PROBLEMS BY GRAM-SCHMIDT ORTHOGONALIZATION',
C     'BIT' VOL. 7 (1967), PAGES 1-21.
C
C     MODIFIED 9/30/76 BY ROY H. WAMPLER TO HANDLE
C        WEIGHTED MEASUREMENTS AND TO GIVE THE COEFFICIENT
C        OF THE QUADRATIC TERM AS OUTPUT.
C
C               MODIFIED BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -  FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(N,*), W(*), WTY(*), Y(*)
      REAL             COEFF, F
      REAL             SQRTW, WEIGHT, X, XBAR
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION B(4), C(3,3), D(3), R(3)
      DOUBLE PRECISION FDDIV, FDSQRT
C
C     ==================================================================
C
C     INITIALIZE A AND FORM SUM OF SQUARES OF THE B VECTOR
C
      B(4) = DZERO
      XBAR = FDIV (FLOAT(N+IONE),RTWO,IND)
      WEIGHT = RONE
      DO 10 I=1,N
        X = FLOAT(I) - XBAR
        IF (NW.NE.IZERO) WEIGHT = W(I)
        SQRTW = WEIGHT
        IF (SQRTW.LE.RZERO) SQRTW = RZERO
        SQRTW = FSQRT (SQRTW)
        A(I,1) = SQRTW
        A(I,2) = SQRTW*X
        A(I,3) = SQRTW*X*X
        WTY(I) = SQRTW*Y(I)
        B(4)   = B(4) + DBLE (WTY(I)) * DBLE (WTY(I))
  10  CONTINUE
C
      NF   = ITHRE
      D(1) = DZERO
      B(1) = DZERO
      DO 20 I=1,N
        D(1) = D(1) + DBLE(A(I,1)) * DBLE (A(I,1))
        B(1) = B(1) + DBLE (A(I,1)) * DBLE (WTY(I))
  20  CONTINUE
C
      B(1) = FDDIV (B(1),D(1),IND)
      IR   = IZERO
      DO 70 K=2,NF
        DO 50 J=K,NF
          IR = IR + IONE
          R(IR) = DZERO
          DO 30 I=1,N
            R(IR) = R(IR) + DBLE (A(I,K-1)) * DBLE (A(I,J))
  30      CONTINUE
          R(IR) = FDDIV (R(IR),D(K-1),IND)
          DO 40 I=1,N
            A(I,J) = A(I,J) - A(I,K-1) * FDPCON (R(IR))
  40      CONTINUE
  50    CONTINUE
        D(K) = DZERO
        B(K) = DZERO
        DO 60 I=1,N
          WTY(I) = WTY(I) - A(I,K-1) * FDPCON (B(K-1))
          B(K) = B(K) + DBLE (A(I,K)) * DBLE (WTY(I))
          D(K) = D(K) + DBLE (A(I,K)) * DBLE (A(I,K))
  60    CONTINUE
        B(K) = FDDIV (B(K),D(K),IND)
  70  CONTINUE
C
      IRS = -NF
      DO 110 K=1,NF
        IRS = IRS + NF - K + IONE
        IR  = IRS
        DO 100 JJ=1,K
          J = K - JJ + IONE
          C(K,J) = B(J)
          IF (JJ.LE.IONE) GO TO 90
          DO 80 I=2,JJ
            KK = K - I + ITWO
            C(K,J) = C(K,J) - C(K,KK)*R(IR)
            IR = IR - IONE
  80      CONTINUE
  90     IR = IR - NF + K
 100    CONTINUE
 110  CONTINUE
C
      DO 120 I=1,NF
        B(I) = B(I) * FDSQRT (D(I))
 120  CONTINUE
C
      F = FDPCON (FDDIV (B(3)*B(3)*DBLE(FLOAT(N-ITHRE)),
     1       B(4)-B(1)*B(1)-B(2)*B(2)-B(3)*B(3),IND) )
      COEFF = FDPCON (C(3,3))
C
      RETURN
C
C     ==================================================================
C
      END
*RANKO
      SUBROUTINE RANKO (N,X,S,H,R,T)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  RANKO V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PUTS RANK OF N X'S IN VECTOR R.
C        VECTOR S IS USED FOR SORTED X, AND
C        VECTOR H IS USED FOR HIERARCHY.
C
C     X,S,H, AND R MUST BE DIMENSIONED N OR GREATER IN CALLING PROCEDURE
C
C     STORES CORRECTION FOR TIES IN T = (1/12)*SUM(T-1)*T*(T+1).
C     T=0  MEANS NO TIES.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -    APRIL, 1975.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             H(*), R(*), S(*), X(*)
      REAL             T
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
C     MOVE X TO S
C
      DO 10 I=1,N
        S(I) = X(I) 
  10  CONTINUE
C
C     SORT X IN S AND OBTAIN HIERARCHY IN H.
C
      CALL  SORT (S(1),H(1),N,1)
C
C     REPLACE S(I) BY I*.
C     LET K BE SUCH THAT S(I) = S(I-J+1),J=1,K. THEN I* = I-(K-1)/2.
C
      K = IONE
      T = IZERO
      DO 50 I=2,N
        IF (S(I)-S(I-1)) 30,20,30
  20    K = K + IONE
        GO TO 50
  30    DO 40 J=1,K 
          IJ = I - J
          S(IJ) = FLOAT (I-IONE) - FDIV (FLOAT(K-IONE),RTWO,IND)
  40    CONTINUE
        IF (K.GT.IONE) T = T +
     1       FDIV (FLOAT(K-IONE)*FLOAT(K)*FLOAT(K+IONE),SPCA,IND)
        K = IONE
  50  CONTINUE
C
      T = T + FDIV (FLOAT(K-IONE)*FLOAT(K)*FLOAT(K+IONE),SPCA,IND)
      DO 60 I=1,K
        K2 = N + IONE - I
        S(K2) = FLOAT (N) - FDIV (FLOAT(K-IONE),RTWO,IND)
  60  CONTINUE
C
C     OBTAIN RANKS BY ORDERING I* IN S(.) BY USING HIERARCHY IN H(.)
C
      DO 70 I=1,N
        J = H(I)
        R(J) = S(I) 
  70  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*RANKX
      SUBROUTINE RANKX (N,X,H,R,T)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  RANKX V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PUTS RANK OF N X'S IN VECTOR R. VECTOR H IS USED FOR STORAGE.
C
C     STORES CORRECTION FOR TIES IN T = (1/12)*SUM(T-1)*T*(T+1).
C        T=0  MEANS NO TIES.
C
C     X, H AND R MUST BE DIMENSIONED N OR GREATER.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -    APRIL, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             H(*), R(*), X(*) 
      REAL             T
      REAL             W
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
C     MOVE X TO R AND PUT I IN H
C
      DO 10 I=1,N
        H(I) = I
        R(I) = X(I) 
  10  CONTINUE
C
C     SORT X IN R, CARRY ALONG I IN H TO OBTAIN HIERARCHY IN H.
C     SORT USES 'PUSH-DOWN' METHOD. SEE ORGANICK, PAGE 84.
C
      K1 = N - IONE 
      DO 30 I=1,K1
        K2 = N - I
        DO 20 J=1,K2
          IF (R(J).LE.R(J+1)) GO TO 20
          W = R(J)
          R(J) = R(J+1)
          R(J+1) = W
          W = H(J)
          H(J) = H(J+1)
          H(J+1) = W
  20    CONTINUE
  30  CONTINUE
C
C     REPLACE R(I) BY I*.
C     LET K BE SUCH THAT R(I) = R(I-J+1),J=1,K. THEN I* = I-(K-1)/2.
C
      K = IONE
      T = RZERO
      DO 70 I=2,N
        IF (R(I)-R(I-1)) 50,40,50
  40    K = K + IONE
        GO TO 70
  50    W = FLOAT(I) - RONE - FDIV (FLOAT(K)-RONE,RTWO,IND) 
        DO 60 J=1,K 
          IJ = I - J
          R(IJ) = W 
  60    CONTINUE
        IF (K.GT.IONE) T = T +
     1       FDIV (FLOAT((K-IONE)*K*(K+IONE)),SPCA,IND)
        K = IONE
  70  CONTINUE
C
      T = T + FDIV (FLOAT((K-IONE)*K*(K+IONE)),SPCA,IND)
      W = FLOAT (N) - FDIV (FLOAT(K-IONE),RTWO,IND)
      DO 80 I=1,K
        K2 = N + IONE - I
        R(K2) = W
  80  CONTINUE
C
C     SORT H CARRY ALONG R TO OBTAIN RANKS IN R
C
      DO 100 I=1,K1 
        K2 = N - I
        DO 90 J=1,K2
          IF (H(J).LE.H(J+1)) GO TO 90
          W = H(J)
          H(J) = H(J+1)
          H(J+1) = W
          W = R(J)
          R(J) = R(J+1)
          R(J+1) = W
  90    CONTINUE
 100  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*RCSUM
      SUBROUTINE RCSUM (A,NSTART,NROW,N,K,R,IB,PIFTY)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  RCSUM V 7.00  7/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C       A         IS LOCATION OF MATRIX TO BE SUMMED ROW AND COLUMN WISE
C       NSTART    IS LOCATION OF UPPER LEFT CORNER OF MATRIX IN WORKSHEET
C       NROW      IS NUMBER OF ROWS IN WORKSHEET
C       N         IS NUMBER OF ROWS IN A
C       K         IS NUMBER OF COLS IN A
C       R         IS RESULTS. 
C                   R(1) ... R(K)   COL SUMS
C                   R(K+1).. R(K+N) ROW SUMS
C                   R(K+N+2)        GRAND SUM
C                   R(K+N+1)        SUM A(I,J) FOR ALL I,J. 
C                   R(K+N+3)        SUM A(I,J)**2 FOR ALL I,J.
C                   R(K+N+4)        SUM OF ABSOLUTE VALUES OF ALL A(I,J)
C       NE        IS NUMBER OF BINARY BITS IN MANTISSA OF REAL NUMBER.
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
C                   CURRENT VERSION -     JULY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
      REAL             A(*), R(*)
      REAL             PIFTY
      REAL             ASUM, SS, SUM
      REAL             FDPCON 
C
      DOUBLE PRECISION X(1)
      DOUBLE PRECISION S
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      IB = IZERO
      CALL DSUMAL (X,IZERO,S)
      ISTART = NSTART 
      DO 20 M =1,K
        JSTART = ISTART
        DO 10 I=1,N 
          X(1) = A(JSTART)
          CALL DSUMAL (X,-IONE,S)
          JSTART = JSTART + 1
  10    CONTINUE
        ISTART = ISTART + NROW
  20  CONTINUE
C
      CALL DSUMAL (X,IONE,S)
      IF (DABS(S).LE.DBLE(PIFTY)) GO TO 30
      IB  = IB + IONE
      S   = DZERO
  30  SUM = FDPCON (S)
C
      CALL DSUMAL (X,IZERO,S) 
      ISTART = NSTART
      DO 50 M =1,K
        JSTART = ISTART
        DO 40 I=1,N 
           X(1) = ABS(A(JSTART))
          CALL DSUMAL (X,-IONE,S)
          JSTART = JSTART + 1
  40    CONTINUE
        ISTART = ISTART + NROW
  50  CONTINUE
C
      CALL DSUMAL (X,IONE,S)
      IF (DABS(S).LE.DBLE(PIFTY)) GO TO 60
      IB   = IB + IONE
      S    = DZERO
  60  ASUM = FDPCON (S)
C
      CALL DSUMAL (X,IZERO,S) 
      ISTART = NSTART
      DO 80 M =1,K
        JSTART = ISTART
        DO 70 I=1,N 
           X(1) = (A(JSTART)) ** 2
          CALL DSUMAL (X,-IONE,S)
          JSTART = JSTART + 1
  70    CONTINUE
        ISTART = ISTART + NROW
  80  CONTINUE
C
      CALL DSUMAL (X,IONE,S)
      IF (DABS(S).LE.DBLE(PIFTY)) GO TO 90
      IB = IB + IONE
      S  = DZERO
  90  SS = FDPCON (S)
      ISTART = NSTART
      JSTART = ISTART
      DO 100 M=1,K
        CALL SUMMAL (A(JSTART),N,R(M))
        IF (N.EQ.IONE) R(M) = A(JSTART)
        JSTART = JSTART + NROW
 100  CONTINUE
C
      L = K + IONE
      ISTART = NSTART
      DO 130 I=1,N
        CALL DSUMAL (X,IZERO,S)
        JSTART = ISTART
        DO 110 M=1,K
          X(1) = A(JSTART)
          JSTART = JSTART + NROW
          CALL DSUMAL (X,-IONE,S)
 110    CONTINUE
        ISTART = ISTART + 1
        CALL DSUMAL (X,IONE,S)
        IF (DABS(S).LE.DBLE(PIFTY)) GO TO 120
        IB = IB + IONE
        S  = DZERO
 120    R(L) = FDPCON (S)
        L  = L + IONE
 130  CONTINUE
C
      R(L)   = SUM
      R(L+1) = SUM
      R(L+2) = SS
      R(L+3) = ASUM 
      RETURN
C
C     ==================================================================
C
      END 
*RDWITH
      SUBROUTINE RDWITH (IRAN,KRAN,NPOPLN,NSAMPL,NSTART,U,RNDGIT,NFAULT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RDWITH V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO PRODUCE NSAMPL PSEUDO RANDOM INTEGERS FROM
C        POPULATION OF SIZE NPOPLN.  THE RESULT IS STORED IN VECTOR N.
C           U IS STORAGE VECTOR FOR PSEUDO UNIFORM RANDOM NUMBERS.
C
C     SAMPLING IS *** WITH *** REPLACEMENT.
C
C     NSTART = STARTING VALUE FOR PSEUDO RANDOM NUMBER GENERATOR.
C
C     NFAULT = 0, IF EVERYTHING IS OK
C              1, IF NPOPLN IS LESS THAN 1
C              2, IF NSAMPL IS LESS THAN 1
C*
C               WRITTEN BY -
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             U(*), RNDGIT(*)
      REAL             A, B, UNIRAN
C
C     ==================================================================
C
      NFAULT = IZERO
      IF (NPOPLN.LT.IONE)   NFAULT = IONE
      IF (NSAMPL.LT.IONE)   NFAULT = ITWO
      IF (NFAULT.NE.IZERO)  RETURN
C
      A = RONE
      B = FLOAT (NPOPLN)
      IF (NSTART.LE.IONE) GO TO 20
C
      IEND = NSTART - IONE
      DO 10 I=1,IEND
        U(1) = UNIRAN (IRAN,KRAN,IZERO) 
  10  CONTINUE
C
  20  DO 30 I=1,NSAMPL
        U(I) = UNIRAN (IRAN,KRAN,IZERO) 
  30  CONTINUE
C
      DO 40 I=1,NSAMPL
        N = A + B * U(I)
        IF (N.GT.NPOPLN) N = NPOPLN
        RNDGIT(I) = N
  40  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END 
*REFLEX
      SUBROUTINE REFLEX (VX1,VY1,VX2,VY2)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REFLEX V 7.00  5/18/90. **
C
C     ==================================================================
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      WRITE (IPRINT,10)
10    FORMAT (57H INSTALLATION MUST PROVIDE PROPRIETARY REFLEX SUBROUTIN
     1E.)
C
      VX1 = VX1
      VY1 = VY1
      VX2 = VX2
      VY2 = VY2
C
      RETURN
      END
*REPCHK
      SUBROUTINE REPCHK (LSWTCH,LIND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REPCHK V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CHECK FOR INSTRUCTIONS WHICH ARE NOT PERMITTED IN THE REPEAT MODE.
C
C     IF SUCH AN INSTRUCTION IS FOUND, A FATAL ERROR MESSAGE OR
C     AN INFORMATIVE DIAGNOSTIC IS PRINTED.
C
C     LSWTCH = L1 * 100 + L2
C
C    LIND = 0 IF NO FATAL ERROR IS PRINTED AND
C    LIND = 1 IF A FATAL IS PRINTED.
C
C     THE VECTOR INST CONTAINS THE VALUES OF L1 AND L2 OF INSTRUCTIONS
C        WHICH CAUSE A FATAL ERROR MESSAGE TO BE PRINTED.
C
C           INSTRUCTION           INST        L1*100+L2
C
C             BEGIN                 1           1401
C             EVALUATE              2           1606
C             READ                  3            501
C             SCAN                  4           1402
C             SET                   5           1302
C             TAPE, UNIT            6           5029
C
C     THE VECTOR INSTA CONTAINS THE VALUES OF L1 AND L2 OF INSTRUCTIONS
C        WHICH CAUSE A INFORMATIVE DIAGNOSTIC TO BE PRINTED.
C
C           INSTRUCTION           INSTA       L1*100+L2
C
C             ALABEL                1           1604
C             CONTENTS              2           1506
C             DESCRIBE              3           1507
C             INTERACTIVE           4           1404
C             LABEL                 5           1603
C             MLABEL                6           1605
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
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
      DIMENSION INST( 6), INSTA(6)
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA INST ( 1) / 1401 /
      DATA INST ( 2) / 1606 /
      DATA INST ( 3) /  501 /
      DATA INST ( 4) / 1402 /
      DATA INST ( 5) / 1302 /
      DATA INST ( 6) / 5029 /
      DATA INSTA( 1) / 1604 /
      DATA INSTA( 2) / 1506 /
      DATA INSTA( 3) / 1507 /
      DATA INSTA( 4) / 1404 /
      DATA INSTA( 5) / 1603 /
      DATA INSTA( 6) / 1605 /
C
      DATA JINST     /    6 /
      DATA KINST     /    6 /
C
C     ==================================================================
C
      II   = LSWTCH
      LIND = 0
C
C     CHECK TO SEE IF FATAL ERROR SHOULD BE PRINTED.
C
      DO 10 I=1,JINST
        IF (II.EQ.INST(I)) GO TO 30
  10  CONTINUE
C
C     CHECK TO SEE IF INFORMATIVE DIAGNOSTIC SHOULD BE PRINTED.
C
      DO 20 I=1,KINST
        IF (II.EQ.INSTA(I)) GO TO 40
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
  30  CALL ERROR (5)
      LIND = 1
      RETURN
C
  40  CALL ERROR (264)
      LIND = 1
      RETURN
C
C     ==================================================================
C
      END
*REPINC
      SUBROUTINE REPINC (IJSWT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. REPINC V 7.00  1/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     IJSWT = 1,    COMMAND IS REPEAT.  INITIALIZE THINGS.
C     IJSWT = 2,    IN REPEAT MODE.
C     IJSWT = 3,    COMMAND IS INCREMENT OR RESTORE.
C
C        L2 = 6,    INCREMENT.
C        L2 = 8,    RESTORE
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   APRIL, 1969.
C                   CURRENT VERSION - JANUARY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
C
      REAL             T, Y
      REAL             SPCA, SPCB, SPCC
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 64 /
C
      DATA SPCA /   16.0 /
      DATA SPCB / 8192.0 /
      DATA SPCC / 8208.0 /
C
C     ==================================================================
C
      GO TO (300,420,10), IJSWT
C
  10  IF (L2.EQ.6) GO TO 20
      T = RZERO
      GO TO 30
  20  T = RONE
  30  IF (NARGS.GE.ITWO) GO TO 50
  40  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     GET STATEMENT NUMBER. CAN BE FLOATING OR INTEGER.
C
  50  IF (KIND(1).EQ.IZERO) GO TO 60
      J = RTEN * ARGS(1) + RHALF
      GO TO 70
C
  60  J = ITEN * IARGS(1)
  70  IF (J.GT.NSTMTH) GO TO 90
      JHOLD = J
      J     = LOCATE(J)
C
C     J HAS LOCATION OF COMMAND TO BE MODIFIED.
C
      IF (LEVEL.EQ.IZERO) GO TO 80
      IF (JHOLD.NE.INDEX(6,LEVEL)) GO TO 80
      CALL ERROR (30)
      RETURN
C
C     ..................................................................
C
  80  IF (J.GT.IZERO) GO TO 100
  90  CALL ERROR (13)
      RETURN
C
C     ..................................................................
C
C     JJ IS FIRST LOCATION OF THE NEXT STORED COMMAND.
C
 100  JJ = J + IFIX (COM(J+1))
C
C     CHECK THAT COMMAND HAS THE PROPER NUMBER OF ARGUMENTS
C
      IF (NARGS-IONE.NE.MOD(IFIX(COM(J+2)),ICA)) GO TO 40
      J    = J + ITHRE
C
C     SKIP OVER HEADER.
C
C     CHECK IF THIS COMMAND IS STORED. IF SO, PULL OUT INTO ARGTAB.
C        (ALL BUT FIRST ARGUMENT, WHICH IS STATEMENT NUMBER)
C
      IF (LEVEL.EQ.IZERO) GO TO 120
      IRMV = LOCATE(INDEX(6,LEVEL))
      K    = ITWO * NARGS
      DO 110 I=2,K
        ARGTAB(I) = COM(IRMV+4)
        IRMV      = IRMV + IONE
 110  CONTINUE
C
C     IRMV IS LOCATION OF THIS COMMAND.
C
 120  I = ITWO + KIND(1)
C
C     PERFORM INCREMENT OR RESTORE.  PICK UP ARGUMENT FROM
C        COMMAND TO BE MODIFIED AND EXAMINE IT.
C
 130  IF (COM(J).GT.RZERO) GO TO 190
      IF (COM(J).LT.RZERO) GO TO 230
C
C     FLOATING POINT CONSTANT.
C
      IF (ARGTAB(I).EQ.RZERO) GO TO 150
      IF (ARGTAB(I).GT.RZERO) GO TO 270
C
C     INCREMENT FL. PT. CONSTANT BY  'STATEMENT'.
C
      IF (ARGTAB(I).EQ.(-RONE)) GO TO 270
      CALL XPND (ARGTAB(I),K,Y,KND)
      IF (K.LT.IZERO) GO TO 200
      IF (KND.EQ.IZERO) GO TO 270
      COM(J+1) = T * COM(J+1) + Y
      J = J + ITWO
 140  I = I + K + IONE
      GO TO 180
C
 150  COM(J+1) = T * COM(J+1) + ARGTAB(I+1)
 160  J = J + ITWO
 170  I = I + ITWO
 180  IF (J.GE.JJ) RETURN
      GO TO 130
C
C     COLUMN NUMBER.
C
 190  IF (ARGTAB(I).GT.RZERO) GO TO 220
      IF (ARGTAB(I).EQ.RZERO) GO TO 270
C
C     INTEGER CONSTANT MODIFIED BY 'STATEMENT'.
C
      IF (ARGTAB(I).EQ.(-RONE)) GO TO 270
      CALL XPND (ARGTAB(I),K,Y,KND)
      IF (K.GE.IZERO .AND. KND.EQ.IZERO) GO TO 210
      IF (K.GE.IZERO .AND. KND.NE.IZERO) GO TO 270
 200  K = -K
      GO TO 280
C
 210  COM(J) = T * COM(J) + Y
      J = J + IONE
      GO TO 140
C
 220  COM(J) = T * (COM(J)-SPCB) + ARGTAB(I)
      IF (COM(J).LE.RZERO) GO TO 250
      J = J + IONE
      I = I + IONE
      GO TO 180
C
C     VARIABLE  *REFERENCE*
C        NRMAX, V, W, X, Y, Z CAN ONLY BE INCREMENTED, BY 0 OR 0.
C        WHETHER 0 OR 0.  INCREMENTS  X OR 'X' IS IMMATERIAL.
C
 230  IF (COM(J).LT.(-SPCA)) GO TO 240
      IF (COM(J).EQ.(-RONE)) GO TO 290
      IF (ARGTAB(I)+ARGTAB(I+1).NE.RZERO) GO TO 260
      J = J + IONE
      GO TO 170
C
C     *ROW,COL* REFERENCE.
C
 240  IF (ARGTAB(I).GE.SPCA) GO TO 270
      COM(J) = T * (COM(J)+SPCC) + ARGTAB(I)
      IF (COM(J).GT.(-SPCA)) GO TO 270
      IF (COM(J+1)*ARGTAB(I+1).LE.RZERO) GO TO 270
      Y = T * (ABS(COM(J+1))-SPCB) + ABS(ARGTAB(I+1))
      IF (Y.LE.RZERO) GO TO 270
      COM(J+1) = SIGN (Y,COM(J+1))
      GO TO 160
C
 250  CALL ERROR (18)
      RETURN
C
C     ..................................................................
C
 260  CALL ERROR (37)
      RETURN
C
C     ..................................................................
C
 270  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 280  CALL ERROR (K)
      RETURN
C
C     ..................................................................
C
C     *** (=THRU) IGNORE.  INCREM. OR RESTORE MAY OR MAY NOT
C        HAVE CORRESPONDING ***
C
 290  IF (ARGTAB(I).EQ.(-RONE)) I = I+IONE
      J = J + IONE
      GO TO 180
C
C     NESTED PERFORMS UP TO EIGHT LEVELS ARE ALLOWED.
C        CURRENT LEVEL IS STORED IN LEVEL.
C
C     INDEX(1,LEVEL) CONTAINS LOCATION OF COMMAND AT ARG1 (FIRST)
C     INDEX(2,LEVEL) CONTAINS RUNNING INDEX  FROM ARG1 TO ARG2
C     INDEX(3,LEVEL) CONTAINS LOCATION OF COMMAND AT ARG2(LAST)
C     INDEX(4,LEVEL) CONTAINS THIRD   ARG (REPEAT  COUNT)
C     INDEX(5,LEVEL) CONTAINS CURRENT LEVEL COUNTER  (1 TO  ARG3)
C     INDEX(6,LEVEL) CONTAINS STATEMENT NUMBER OF STATEMENT CURRENTLY
C                             BEING EXECUTED.
C
 300  IF (NARGS.GT.ITHRE) GO TO 320
      IF (NARGS.EQ.ITHRE) GO TO 330
      IF (NARGS.GT.IONE)  GO TO 310
      IF (NARGS.LT.IONE)  GO TO 320
C
C     SECOND ARGUMENT MISSING, MAKE SAME AS FIRST ARGUMENT.
C
      IARGS(2) = IARGS(1)
      KIND(2)  = KIND(1)
      ARGS(2)  = ARGS(1)
C
C     THIRD ARGUMENT MISSING, SET TO INTEGER 1.
C
 310  IARGS(3) = IONE
      KIND(3)  = IZERO
      GO TO 340
C
 320  CALL ERROR (10)
      GO TO 430
C
 330  IF (KIND(3).EQ.IZERO .AND. IARGS(3).GT.IZERO) GO TO 340
      CALL ERROR (3)
      GO TO 430
C
 340  DO 380 I=1,2
        IF (KIND(I).EQ.IZERO) GO TO 350
        IARGS(I) = RTEN * ARGS(I) + RHALF
        GO TO 360
 350    IARGS(I) = ITEN * IARGS(I)
 360    IF (IARGS(I).GT.NSTMTH) GO TO 370
        IARGS(I) = LOCATE(IARGS(I))
        IF (IARGS(I).GT.IZERO) GO TO 380
 370    CALL ERROR (13)
        GO TO 430
 380  CONTINUE
C
      IF (LEVEL.LT.8) GO TO 390
      CALL ERROR (19)
      GO TO 430
C
 390  IF (IARGS(2).LT.IARGS(1)) CALL ERROR (3)
      IF (NERROR.NE.IZERO) GO TO 430
      LEVEL = LEVEL + IONE
      INDEX(1,LEVEL) = IARGS(1)
      INDEX(3,LEVEL) = IARGS(2)
      INDEX(4,LEVEL) = IARGS(3)
      INDEX(5,LEVEL) = IZERO
C
C     OUTER LOOP.
C
 400  INDEX(5,LEVEL) = INDEX(5,LEVEL) + IONE
      IF (INDEX(5,LEVEL).LE.INDEX(4,LEVEL)) GO TO 410
C
C     FINISHED OUTER LOOP, REDUCE LEVEL BY 1.
C
      LEVEL = LEVEL - IONE
      IF (LEVEL.GT.IZERO) GO TO 420
      RETURN
C
C     ..................................................................
C
 410  INDEX(2,LEVEL) = INDEX(1,LEVEL)
 420  IRMV = INDEX(2,LEVEL)
      IF (IRMV.GT.INDEX(3,LEVEL)) GO TO 400
      INDEX(6,LEVEL) = COM(IRMV)
      K = COM(IRMV+1)
      INDEX(2,LEVEL) = INDEX(2,LEVEL) + K
      L2 = COM(IRMV+2)
      L1 = IDIV (L2,ICA,IND)
      NARGS = L2 - ICA * L1
      L2 = IDIV (L1,ICA,IND)
      L1 = L1 - ICA * L2
      CALL EXPAND (K-ITWO,COM(IRMV+3))
      RETURN
C
C     ..................................................................
C
 430  IJSWT = -IJSWT
      RETURN
C
C     ==================================================================
C
      END
*RESET
      SUBROUTINE RESET
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  RESET V 7.00 11/ 9/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE RESET INSTRUCTIONS.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)
C
C     ==================================================================
C
      IF (NARGS.EQ.IONE) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C         RESET NRMAX
C
  10  IF (NERROR.NE.IZERO) RETURN
      IF (L2.EQ.ITWO) RETURN
      IF (L2.GT.ITWO) GO TO 30
      IF (KIND(1).NE.IZERO) IARGS(1) = ARGS(1)
      IF (IARGS(1).GE.IZERO .AND. IARGS(1).LE.NROW) GO TO 20
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  20  NROLD = NRMAX 
      NRMAX = IARGS(1)
      CALL ERROR (252)
      RETURN
C
C     ..................................................................
C
C         RESET V,W,X,Y,Z
C
  30  IF (KIND(1).EQ.IZERO) ARGS(1) = IARGS(1)
      VWXYZ(L2-2) = ARGS(1)
      RETURN
C
C     ==================================================================
C
      END 
*RFORMT
      SUBROUTINE RFORMT (KTYPE,KDIGIT,X,XVALUE,K1,K2,KW,KD,NALPHA,KE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. RFORMT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                            *** DESCRIPTION ***
C
C     RFORMT IS A GENERAL-PURPOSE PORTABLE FORTRAN SUBROUTINE FOR USE IN
C        PRINTING REAL NUMBERS.
C
C     IT IS PRIMARILY INTENDED FOR PREPARING REAL NUMBERS TO BE PRINTED
C        IN READABLE FORM, I.E., WITH A CONSTANT NUMBER OF SIGNIFICANT
C        DIGITS AND THE DECIMAL POINT IN A CONSTANT POSITION.  THIS IS
C        IS CALLED R FORMAT.  IT CAN ALSO BE USED TO PRINT REAL NUMBERS
C        IN E, F, OR I FORMATS.
C
C     TO USE THE R FORMAT, IT IS NORMALLY NECESSARY TO USE RFORMT IN TWO
C        STAGES.  IN THE FIRST STAGE, WITH ITYPE = 0, NWIDTH AND NDECS
C        ARE CALCULATED.  IN THE SECOND STAGE, NWIDTH AND NDECS ARE USED
C        TO OBTAIN THE HOLLERITH CHARACTER STRING IN THE VECTOR NALPHA.
C
C     IN STAGE 2, REAL NUMBERS ARE CONVERTED INTO A HOLLERITH STRING AND
C        STORED IN THE VECTOR NALPHA FOR PRINTING WITH AN NA1 FORMAT.
C        THE HOLLERITH STRING IS PACKED ONE CHARACTER PER WORD.
C
C     ..................................................................
C
C                       *** STAGE 1 ARGUMENTS ***
C                       COMPUTE NWIDTH AND NDECS
C
C     INPUT ARGUMENTS -
C
C        (1)    ITYPE = 0
C        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS TO BE USED
C        (3)        X = VECTOR OF REAL NUMBERS DIMENSIONED AT LEAST N1
C                          IN CALLING PROGRAM UNIT
C        (4)   XVALUE = DUMMY ARGUMENT
C        (5)       N1 = LENGTH OF VECTOR X
C        (6)       N2 = MAXIMUM VALUE OF NWIDTH ALLOWED
C
C     OUTPUT ARGUMENTS -
C
C        (7)   NWIDTH = WIDTH OF FIELD NEEDED TO PRINT EVERY REAL NUMBER
C                          IN X IN R FORMAT
C        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT NEEDED
C                          TO PRINT NUMBERS IN X IN R FORMAT
C        (9)   NALPHA = DUMMY ARRAY ARGUMENT, WHICH MUST BE
C                                 DIMENSIONED IN CALLING PROGRAM UNIT
C       (10)   IFAULT = FAULT INDICATOR,
C                     = 0, IF EVERYTHING IS OK
C                     = 1, IF ITYPE IS NEGATIVE
C                     = 2, IF VALUE OF NDIGIT INVALID
C                     = 3, IF N1 IS NON-POSITIVE
C                     = 4, IF N2 IS LESS THAN NDIGIT+2
C                     = 5, IF CALCULATED VALUE OF NWIDTH EXCEEDS N2.
C                             NWIDTH IS RESET TO N2.
C                     = 6, IF CALCULATED NWIDTH EXCEEDS N2 AND NDIGIT+5
C                             EXCEEDS N2
C
C     ..................................................................
C
C                         *** STAGE 2 ARGUMENTS ***
C                      PUT HOLLERITH STRING IN NALPHA
C
C     INPUT ARGUMENTS -
C
C        (1)    ITYPE = TYPE OF FORMAT DESIRED,
C                     =  1, R FORMAT, NUMBER ZERO HAS BLANKS AFTER DEC.
C                             POINT, 1PEW.(D-1) FORMAT USED IF NECESSARY
C                     =  2, R FORMAT, ZERO CONVERTED NORMALLY
C                             1PEW.(D-1) FORMAT USED IF NECESSARY
C                     =  3, R FORMAT, ZERO HAS BLANKS AFTER DEC. POINT,
C                             0PEW.D FORMAT USED IF NECESSARY
C                     =  4, R FORMAT, ZEROS CONVERTED NORMALLY
C                             0PEW.D JORMAT USED IF NECESSARY
C                     =  5, 1PEW.D FORMAT
C                     =  6, 0PEW.D FORMAT
C                     =  7, FW.D FORMAT, WITH ROUNDING
C                     =  8, FW.D FORMAT, WITH TRUNCATION
C                     =  9, IW FORMAT, WITH ROUNDING
C                     = 10, IW FORMAT, WITH TRUNCATION
C                     = 11, NWIDTH+N1 BLANKS STORED IN NALPHA
C        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS USED
C        (3)        X = DUMMY ARRAY ARGUMENT, WHICH MUST BE
C                           DIMENSIONED IN CALLING PROGRAM UNIT
C        (4)   XVALUE = REAL NUMBER TO BE CONVERTED
C        (5)       N1 = NUMBER OF BLANKS ADDED TO FIELD IN NALPHA
C        (6)       N2 = 0, NA BLANKS INSERTED ON LEFT (BEGINNING)
C                     = 1, N1 BLANKS ARE CENTERED
C        (7)   NWIDTH = LENGTH OF FIELD (HOLLERITH STRING) EXCLUDING N2
C                          BLANKS
C        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT
C
C     OUTPUT ARGUMENTS -
C
C        (9)   NALPHA = HOLLERITH STRING REPRESENTATION OF XVALUE,
C                          OF LENGTH NWIDTH+N1
C       (10)   IFAULT = FAULT INDICATOR,
C                     =  0, IF EVERYTHING IS OK
C                     =  1, IF VALUE OF ITYPE IS NOT VALID
C                     =  2, IF VALUE OF NDIGIT IS NOT VALID
C                     =  3, IF N1 IS NON-POSITIVE
C                     =  7, IF VALUE OF N2 IS NOT ZERO OR ONE
C                     =  8, IF VALUE OF NWIDTH IS NOT VALID
C                     =  9, IF VALUE OF NDECS IS NOT VALID
C                     = 10, IF OVERFLOW OCCURS WITH F OR I FORMATS
C                     = 11, IF R FORMAT FORCED INTO E FORMAT
C                     = 12, IF R FORMAT REQUIRES E FORMAT AND
C                              NWIDTH IS TOO SMALL
C                     = 13, IF R FORMAT REQUIRES E FORMAT AND
C                              NDECS IS TOO SMALL
C                     = 14, IF ITYPE EQUALS 9 OR 10 AND NDECS DOES NOT
C                              EQUAL ZERO. ZERO IS USED FOR IDECS.
C
C     ..................................................................
C
C                           *** NOTES ***
C
C      1.   CAUTION.  IN STAGE 1 ITYPE MUST EQUAL ZERO OR RFORMT WILL
C              EXECUTE STAGE 2.
C      2.   IFAULT = 5, 10, 11 OR 14, INDICATES INFORMATIVE DIAGNOSTIC.
C              OTHERWISE NON-ZERO VALUES OF IFAULT INDICATE FATAL ERRORS
C              AND EXIT OCCURS WITHOUT ANY FURTHER CALCULATIONS OR ERROR
C              CHECKING.
C      3.   NDIGIT MUST BE GREATER THAN ZERO AND LESS THAN OR EQUAL TO
C              NSIGD.  SEE SECTION ON PORTABILITY BELOW FOR DEFINITION
C              OF NSIGD.
C      4.   X AND NALPHA MUST BE DIMENSIONED IN CALLING PROGRAM UNIT.
C      5.   RFORMT HANDLES REAL NUMBERS BETWEEN 10**(-100) AND 10**100,
C              EXCLUSIVELY.
C      6.   WHEN N2 = 1 IN STAGE 2, LARGEST NUMBER OF BLANKS IS ON RIGHT
C              IF N1 IS ODD.
C      7.   IN STAGE 1, NWIDTH INCLUDES POSITION FOR SIGN, EVEN
C              IF ALL NUMBERS ARE POSITIVE.  HOWEVER THERE ARE TWO
C              SPECIAL CASES ...
C                 (A) WHEN ALL X(I) = 0, IN WHICH CASE NWIDTH = 2
C                        AND NDECS = 0.
C                 (B) WHEN ALL X(I) ARE LESS THAN ONE IN ABSOLUTE VALUE
C                        AND AT LEAST ONE X(I) EQUALS ZERO. A POSITION
C                        FOR THE SIGN OF ZERO IS NOT INCLUDED IN NWIDTH.
C
C      8.   WITH R FORMAT, A DECIMAL POINT IS NOT STORED IN NALPHA IF
C              THE REAL NUMBER XVALUE EXCEEDS 10**NDIGIT.  IF NDIGIT=3,
C              1.23+03 IS STORED AS 1230 RATHER THAN 1230., TO EMPHASIZE
C              THAT THE ZERO IS NOT A SIGNIFICANT DIGIT.
C      9.   RFORMT DOES NO PRINTING.  PRINTING OF NALPHA WITH NA1 FORMAT
C              MUST BE DONE BY THE CALLING PROGRAM UNIT.
C     10.   WHEN ZERO IS PRINTED WITH R FORMAT, NDECS OVERRIDES NDIGIT.
C     11.   CAUTION.  IF IFAULT IS NOT EQUAL TO ZERO, NALPHA MAY NOT BE
C              BLANKED OUT.
C     12.   NALPHA IS UNCHANGED, IF ITYPE EQUALS ZERO.
C
C     ..................................................................
C
C                     *** USE OF E, F, AND I FORMATS ***
C
C     1.   1PEW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE =   5
C             NWIDTH =   W   = WIDTH OF FIELD
C             NDIGIT = (D+1) = NUMBER OF DIGITS
C
C          WITH D=6, 12.345678 IS WRITTEN AS 1.234568+01
C
C     2.   0PEW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 6
C             NWIDTH = W = WIDTH OF FIELD
C             NDIGIT = D = NUMBER OF DIGITS
C
C          WITH D=7, 12.345678 IS WRITTEN AS .1234568+02
C
C     3.   FW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 7 OR 8
C             NWIDTH = W = WIDTH OF FIELD
C              NDECS = D = NUMBER OF PLACES AFTER DECIMAL POINT
C
C     4.   IW FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 9 OR 10
C             NWIDTH = W = WIDTH OF FIELD
C              NDECS = 0
C
C     NOTES -
C        A.   FOR E FORMAT, NDECS MUST BE GREATER THAN OR EQUAL TO ZERO.
C                NSIGDS=NDECS IS SET EQUAL TO NDIGIT+2 BY RFORMT.
C        B.   WITH EW.D FORMAT, THE LETTER E IS NOT USED AFTER THE
C                NUMBER AND BEFORE THE SIGNED CHARACTERISTIC.
C        C.   WITH 0PEW.D FORMAT, ZERO IS NOT PUT BEFORE THE DECIMAL
C                POINT.
C        D.   WITH FW.D FORMAT AND THE ABSOLUTE VALUE OF NUMBER IS LESS
C                THAN ONE, ZERO IS NOT PUT ON LEFT OF DECIMAL POINT,
C                UNLESS D = 0.
C
C     ..................................................................
C
C                            *** PORTABILITY ***
C
C     RFORMT IS COMPLETELY PORTABLE EXCEPT FOR ONE MACHINE DEPENDENT
C        CONSTANT, NSIGD, SET IN THE DATA STATEMENT ON LINE RF 320.
C
C     NSIGD IS THE NUMBER OF SIGNIFICANT DECIMAL DIGITS IN THE COMPUTER.
C        NSIGD =  7, FOR A 32 BIT WORD COMPUTER (IBM)
C              =  8, FOR A 36 BIT WORD COMPUTER (UNIVAC), VALUE SET
C              = 10, FOR A 48 BIT WORD COMPUTER (BURROUGHS)
C              = 13, FOR A 60 BIT WORD COMPUTER (CDC).
C
C     CAUTION.  NSIGD MUST BE SMALL ENOUGH SO THAT 10**(NSIGD+1) IS A
C        VALID MACHINE INTEGER.  (THIS EXPLAINS WHY NSIGD EQUALS 13 AND
C        NOT 14 FOR A 60 BIT WORD COMPUTER.)
C
C     SOURCE LANGUAGE IS PFORT (A PORTABLE SUBSET OF ANS FORTRAN).
C
C     FORTRAN LIBRARY FUNCTION USED IS ALOG10,
C        WHICH APPEARS ON LINES RF 389, RF 391, AND RF 612.
C
C     STORAGE USED IS 1495 36 BIT WORDS WITH UNIVAC 1108 EXEC 8 COMPUTER
C
C     ..................................................................
C
C                           *** STATIC PROFILE ***
C
C     I/O STATEMENTS                 0
C     NONEXECUTABLE STATEMENTS      20
C     EXECUTABLE STATEMENTS        244
C        UNCONDITIONAL 160
C          CONDITIONAL  84
C     COMMENT STATEMENTS           532
C     --------------------------------
C     TOTAL NUMBER OF STATEMENTS   796
C     --------------------------------
C     CONTINUATION LINES             6
C     --------------------------------
C     NUMBER OF LINES OF CODE      802
C
C     ..................................................................
C
C                             *** REFERENCE ***
C
C     HOGBEN, DAVID (1977).  A FLEXIBLE PORTABLE FORTRAN PROGRAM UNIT
C        FOR READABLE PRINTING OF REAL NUMBERS.  IN PREPARATION.
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
C                  ORIGINAL VERSION -    APRIL, 1969.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             X(*)
      REAL             XVALUE
      REAL             ABSMAX, ABSMIN, ABSX, ABSXVA, X1, X2
      REAL             FLOG10
C
C......................................................................
C
      DOUBLE PRECISION Z, ZLOWER, ZUPPER
      DOUBLE PRECISION DFIVE, DTEN
      DOUBLE PRECISION FDDIV
C
C     ..................................................................
C
      CHARACTER LA*1 
      CHARACTER NALPHA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DFIVE, DTEN / 5.0D0, 10.0D0 /
C
C     ==================================================================
C
C     ADAPTIONS FOR OMNITAB.
C
C     NW IS USED INSTEAD OF NWIDTH
C     ND IS USED INSTEAD OF NDECS
C     IE IS USED INSTEAD OF IFAULT
C
      ITYPE  = KTYPE
      NDIGIT = KDIGIT
          N1 = K1
          N2 = K2
          NW = KW
          ND = KD
          IE = KE
C
C     GENERAL ERROR CHECKING.
C
      ZLOWER = ITEN ** NDIGIT
      ZUPPER = DTEN * ZLOWER
      IE = IZERO
      IF (ITYPE.GE.IZERO) GO TO 10
        IE = IONE
        GO TO 390
C
C     ..................................................................
C
  10  IF (NDIGIT.GT.IZERO .AND. NDIGIT.LE.ISIGD) GO TO 20
        IE = ITWO
        GO TO 390
C
C     ..................................................................
C
  20  IF (ITYPE.GT.IZERO) GO TO 80
C
C     ==================================================================
C
C                           *** STAGE 1 ***
C                       COMPUTE NWIDTH AND NDECS
C
C     STAGE 1 ERROR CHECKING
C
      IF (N1.GT.IZERO) GO TO 30
        IE = ITHRE
        GO TO 390
C
C     ..................................................................
C
C     N2 MUST BE LARGE ENOUGH FOR NDIGIT, DECIMAL POINT, AND SIGN.
C
  30  IF (N2.GE.NDIGIT+ITWO) GO TO 40
        IE = IFOUR
        GO TO 390
C
C     ..................................................................
C
C     (1)   COMPUTE MMIN, CHARACTERISTIC OF ABSMIN = MIN ABS VALUE X(I)
C             AND COMPUTE MMAX, CHARACTERISTIC OF ABSMAX = MAX ABS X(I).
C
  40  ABSX = ABS (X(1))
      IF (ABSX.LE.RZERO) ABSX = RONE
      ABSMIN = ABSX
      ABSMAX = ABSX
C
      K = IZERO
C
C     K IS USED IN TWO SPECIAL CASES ... WHEN
C        (A)  ALL X(I) EQUAL ZERO, AND
C        (B)  ABS (X(I)) IS LESS THAN 1.0, FOR ALL I, AND SOME X(I)=0.0.
C
      DO 50 I=1,N1
        ABSX = ABS (X(I))
        IF (ABSX.GE.RONE) K = IONE
        IF (ABSX.LE.RZERO) ABSX = RONE
        IF (ABSX.LT.ABSMIN) ABSMIN = ABSX
        IF (ABSX.GT.ABSMAX) ABSMAX = ABSX
  50  CONTINUE
C
      MMIN = FLOG10 (ABSMIN)
      IF (ABSMIN.LT.RONE) MMIN = MMIN - IONE
      MMAX = FLOG10 (ABSMAX)
      IF (ABSMAX.LT.RONE) MMAX = MMAX - IONE
C
C     ADJUST FOR POSSIBLE INCORRECT VALUES OF MMIN AND MMAX DUE TO
C        ERROR IN ALOG10 CALCULATION.
C
      Z = ABSMIN
      Z = Z * DTEN ** (NDIGIT-MMIN) + DFIVE
C
      IF (Z.LT.ZLOWER) MMIN = MMIN - IONE
      IF (Z.GE.ZUPPER) MMIN = MMIN + IONE
C
      Z = ABSMAX
      Z = Z * DTEN ** (NDIGIT-MMAX) + DFIVE
C
      IF (Z.LT.ZLOWER) MMAX = MMAX - IONE
      IF (Z.GE.ZUPPER) MMAX = MMAX + IONE
C
C     ..................................................................
C
C     (2)   USE MMIN AND MMAX TO COMPUTE NWIDTH AND NDECS.
C
      ND = NDIGIT - MMIN - IONE
      ND = MAX0 (IZERO,ND)
      NW = MMAX + ITHRE + ND
      IF (MMAX.LT.IZERO) NW = ND + ITWO
      IF (K.EQ.IONE) GO TO 60
C
C     ADJUST FOR SPECIAL CASE (B) DESCRIBED ON LINE RF 368
C
      IF (ABSMIN.LT.RONE .AND. ABSMAX.GE.RONE) NW = NW - IONE
C
C     ADJUST FOR SPECIAL CASE (A) DESCRIBED ON LINE RF 367
C
      IF (ABSMIN.LT.RONE .OR. ABSMAX.LT.RONE) GO TO 60
      NW = ITWO
      ND  = IZERO
C
  60  IF (NW.LE.N2) GO TO 390
C
C     NWIDTH IS TOO LARGE AND HAS TO BE ADJUSTED.
C
        IE = IFIVE
      IF (NDIGIT+IFIVE.LE.N2) GO TO 70
        IE = 6
        GO TO 390
C
C     ..................................................................
C
C
C     NDIGIT+2 = (NDIGIT-1) + (+XX), FOR EXPONENT OF FLOATING-POINT NO.
C
  70  ND = MAX0 (ND,NDIGIT+ITWO)
C
C     N2-3 = N2 - (SIGN+DIGIT+DECIMAL POINT).
C
      ND = MIN0 (ND,N2-ITHRE)
      NW = N2
      GO TO 390
C
C     ==================================================================
C
C                          ***** STAGE 2 *****
C                     PUT HOLLERITH STRING IN NALPHA
C
  80  ABSXVA = ABS (XVALUE)
C
C     STAGE 2 ERROR CHECKING
C
      IF (ITYPE.LT.12) GO TO 90
        IE = IONE
        GO TO 390
C
C     ..................................................................
C
  90  IF (N1.GE.IZERO) GO TO 100
        IE = ITHRE
        GO TO 390
C
C     ..................................................................
C
 100  IF (N2.EQ.IZERO .OR. N2.EQ.IONE) GO TO 110
        IE = 7
        GO TO 390
C
C     ..................................................................
C
 110  IF (ITYPE.LT.9 .AND. NW.LT.ND+ITWO) GO TO 120
      IF (NW.LE.IZERO) GO TO 120
      IF (ITYPE.GT.6) GO TO 130
      IF (ABSXVA.LE.RZERO .AND. NW.GE.ITWO .AND. ITYPE.LE.IFOUR)
     1     GO TO 130
C
C     CHECK WHETHER NWIDTH IS VALID.
C
      IF (NW.LT.NDIGIT+ITWO) GO TO 120
      IF (ITYPE.LT.IFIVE) GO TO 130
      IF (NW.GE.NDIGIT+IFIVE) GO TO 130
 120    IE = 8
        GO TO 390
C
C     ..................................................................
C
 130  IF (ND.GE.IZERO) GO TO 140
        IE = 9
        GO TO 390
C
C     ..................................................................
C
C         VARIABLES USED TO DEFINE FIELD WIDTH FOR R FORMAT
C
C                     -----------------------------
C                     I        NWIDTH             I
C          ----------------------------------------------
C          I  NBLANK  I     NDIFF     I   NDECS   I     I
C          ----------------------------------------------
C          I       NPONE              I
C          ----------------------------------------
C          I             LTOTAL                   I
C          ----------------------------------------------
C          I        NTOTAL = NWIDTH + N1                I
C          ----------------------------------------------
C
C     ..................................................................
C
C     (1)   INITIALIZATION.
C
C     CLEAR OUT NALPHA WITH BLANKS.
C
 140  NTOTAL = NW + N1
      DO 150 I=1,NTOTAL
        NALPHA(I) = LA(45)
 150  CONTINUE
C
      IF (ITYPE.EQ.11) GO TO 390
C
C     IF NECESSARY, CENTER BLANKS WITH LARGEST NUMBER ON RIGHT IF N1 ODD
C
      NBLANK = N1 - IDIV (N1+IONE,ITWO,IND) * N2
C
      MF    = IZERO
      MREAL = IZERO
      IDECS = ND
      IF (ITYPE.LT.9 .OR. IDECS.EQ.IZERO) GO TO 160
      IDECS = IZERO
      IE    = 14
 160  IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) IDECS = NDIGIT + ITWO
C
C     THE NEXT THREE STATEMENTS ARE USED TO SWITCH FROM F TO I FORMAT
C
      NSIGDS = NDIGIT
      IWIDTH = NW
      IF (ITYPE.EQ.9 .OR. ITYPE.EQ.ITEN) IWIDTH = IWIDTH + IONE
      NDIFF = IWIDTH - IDECS
      LTOTAL = IWIDTH + NBLANK
      NPONE = NDIFF + NBLANK
C
      IF (ABSXVA.GE.RONE) GO TO 200
      IF (ITYPE.LT.9 .AND. ABSXVA.GT.RZERO) GO TO 200
C
C     ..................................................................
C
C     (2)   XVALUE = 0. IS SPECIAL CASE.
C
      IF (ITYPE.LT.9) GO TO 180
C
C     INTEGER FORMAT
C
      IF (ABSXVA.LE.RHALF .OR. ITYPE.EQ.ITEN) GO TO 170
      NALPHA(LTOTAL-1) = LA(2)
        IF (XVALUE.LT.RZERO) NALPHA(LTOTAL-2) = LA(39)
      GO TO 390
C
C     ..................................................................
C
 170  NALPHA(LTOTAL-1) = LA(1)
      GO TO 390
C
C     ..................................................................
C
C     R FORMAT WITH ZERO STORED AS 0.
C
 180  NALPHA(NPONE  ) = LA(38)
      NALPHA(NPONE-1) = LA(1)
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITHRE) GO TO 390
      IF (ITYPE.EQ.ITWO .AND. IDECS.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.IFOUR .AND. IDECS.EQ.IZERO) GO TO 390
C
C     FIXED 0
C
      IF (ITYPE.EQ.7 .AND. ND.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.8 .AND. ND.EQ.IZERO) GO TO 390
C
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.8) NALPHA(NPONE-1) = LA(45)
C
C     ALL OTHER CASES
C
      IBEG = NPONE + IONE
      IEND = NPONE + IDECS
      DO 190 I=IBEG,IEND
        NALPHA(I) = LA(1)
 190  CONTINUE
C
C     ..................................................................
C
      IF (ITYPE.NE.IFIVE .AND. ITYPE.NE.6) GO TO 390
C
C     FLOATING
C
      NALPHA(LTOTAL-2) = LA(40)
      IF (ITYPE.EQ.IFIVE) GO TO 390
      NALPHA(NPONE  ) = LA(1)
      NALPHA(NPONE-1) = LA(38)
      GO TO 390
C
C     ..................................................................
C
C     (3)   COMPUTE M = CHARACTERISTIC OF ABSXVA = ABS(XVALUE) AND
C                  LL = (NSIGDS+1) INTEGER REPRESENTATION OF ABSXVA.
C              FOR XVALUE = -12.345678, M=1 AND LL=123456784, AN
C              ADDITIONAL DIGIT IN LL IS USED TO AVOID ROUNDOFF ERROR.
C
 200  M = FLOG10 (ABSXVA)
      IF (ABSXVA.LT.RONE) M = M - IONE
      Z = ABSXVA
      Z = Z * DTEN**(NSIGDS-M)
C
C     IF M IS COMPUTED ACCURATELY, ZLOWER .LE. Z .LT. ZUPPER
C
      IF (Z.GE.ZLOWER) GO TO 210
C
C     Z IS LESS THAN ZLOWER BECAUSE M IS ONE TOO LARGE.
C       ADJUST BY SUBTRACTING 1 FROM M AND MULTIPLYING Z BY 10.
C
      M = M - IONE
      Z = DTEN * Z
      GO TO 220
C
 210  IF (Z.LT.ZUPPER) GO TO 220
C
C     Z IS GREATER THAN OR EQUAL TO ZUPPER BECAUSE M IS ONE TOO SMALL.
C       ADJUST BY ADDING 1 TO M AND DIVIDING Z BY 10.
C
      M = M + IONE
      Z = FDDIV (Z,DTEN,IND)
C
 220  X1 = Z
      LL1 = X1
      X2 = Z - DBLE (X1)
      LL2 = X2
      LL = LL1 + LL2 + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 230
C
C     MAKE ADJUSTMENT WHEN LL IS TOO LARGE.
C
      M = M + IONE
      LL = IDIV (LL,ITEN,IND)
      GO TO 240
 230  IF (LL.GE.ITEN**NSIGDS) GO TO 240
C
C     MAKE ADJUSTMENT WHEN LL IS TOO SMALL.
C
      M = M - IONE
      LL = ITEN * LL
 240  IF (ITYPE.EQ.8 .OR. ITYPE.EQ.ITEN) LL = LL - IFIVE
      IF (ITYPE.LT.IFIVE) GO TO 290
      IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) GO TO 300
C
C     ..................................................................
C
C     (4)   FIXED AND INTEGER.
C
C     CHECK FOR OVERFLOW.
C
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
C
C     ADJUST NUMBER OF DIGITS (NSIGDS) AND LL.
C
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL - IFIVE
      LL = IDIV (LL,ITEN**(NDIGIT-NSIGDS),IND)
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 250
C
C     ADJUST FOR XVALUE ROUNDED TO ONE MORE DIGIT.
C
      M = M + IONE
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
C
C     CHECK FOR OVERFLOW CAUSED BY ROUNDING TO ONE MORE DIGIT.
C
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
C
C     CHECK FOR UNDERFLOW.
C
 250  IF (NSIGDS.GT.IZERO) GO TO 310
C
C     ADJUST FOR UNDERFLOW.  XVALUE ROUNDED TO IDECS EQUALS ZERO.
C
      IF (IDECS.EQ.IZERO) NALPHA(NPONE-1) = LA(1)
C
      DO 260 I=NPONE,LTOTAL
        NALPHA(I) = LA(1)
 260  CONTINUE
C
      NALPHA(NPONE) = LA(38)
      GO TO 390
C
C     ..................................................................
C
C     PUT IN ASTERISKS WHEN OVERFLOW OCCURS.
C
 270  IE = ITEN
      DO 280 I=1,NW
        ISUBSC = I + NBLANK
        NALPHA(ISUBSC) = LA(41)
 280  CONTINUE
      GO TO 390
C
C     ..................................................................
C
C     (5)   CHECK WHETHER R FORMAT IS FORCED INTO E FORMAT.
C
 290  IF (M.GE.NSIGDS-IONE-IDECS .AND. M.LT.NDIFF-ITWO) GO TO 310
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.GT.RZERO) GO TO 310
        IE = 11
      IF (NW.GE.NDIGIT+IFIVE .AND. ND.GE.NDIGIT+ITWO) GO TO 300
        IE = 13
      IF (NW.GE.NDIGIT+IFIVE) GO TO 390
        IE = 12
        GO TO 390
C
C     ..................................................................
C
C     (6)   FLOATING.
C
 300  MREAL = M
      M = IZERO
      MF = IONE
C
C     ..................................................................
C
C     (7)   STORE REPRESENTATION IN NALPHA.
C
 310  IF (M.LT.NSIGDS .AND. ITYPE.LT.9) NALPHA(NPONE) = LA(38)
      NINT = NPONE - IONE - M
      IF (M.LT.IZERO) NINT = NINT + IONE
      NEND = NINT + NSIGDS - IONE
      IF (M.GE.IZERO .AND. M.LT.NSIGDS-IONE) NEND = NEND + IONE
      DO 320 J=NINT,NEND
        I = NEND + NINT - J
        IF (I.EQ.NPONE) GO TO 320
        LL = IDIV (LL,ITEN,IND)
        NN = MOD (LL,ITEN)
        NALPHA(I) = LA(NN+1)
 320  CONTINUE
C
      IF (MF.EQ.IZERO) GO TO 340
C
C     ..................................................................
C
C     (8)   PUT IN EXPONENT FOR FLOATING POINT NUMBER.
C
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITWO .OR. ITYPE.EQ.IFIVE) GOTO 330
C
C     CHANGE FROM 1PE TO 0PE
C
      NALPHA(NINT+1) = NALPHA(NINT)
      NALPHA(NINT  ) = LA(38)
      MREAL = MREAL + IONE
C
 330  IF (MREAL.LT.IZERO) NALPHA(NEND+1) = LA(39)
      IF (MREAL.GE.IZERO) NALPHA(NEND+1) = LA(40)
      MREALA = IABS(MREAL)
      M1 = IDIV (MREALA,ITEN,IND)
      M2 = MOD (MREALA,ITEN)
      NALPHA(NEND+2) = LA(M1+1)
      NALPHA(NEND+3) = LA(M2+1)
C
C     ..................................................................
C
C     (9)   PUT IN MINUS SIGN IF XVALUE LESS THAN ZERO.
C
 340  IF (XVALUE.GE.RZERO) GO TO 350
        IF (M.GE.IZERO) NALPHA(NINT-1) = LA(39)
        IF (M.LT.IZERO) NALPHA(NPONE-1) = LA(39)
 350  IF (M.GE.(-IONE)) GO TO 370
C
C     PUT ZEROS AFTER DECIMAL POINT FOR ABSXVA LESS THAN 0.1
C
      IBEG = NPONE + IONE
      IEND = NINT - IONE
      DO 360 I=IBEG,IEND
        NALPHA(I) = LA(1)
 360  CONTINUE
      GO TO 390
C
C     ..................................................................
C
C     (10)   PUT IN NON-SIGNIFICANT ZEROS FOR LARGE INTEGERS.
C
 370  IF (M.LT.NSIGDS .OR. MF.NE.IZERO) GO TO 390
      IBEG = NINT + NSIGDS
      IEND = NPONE - IONE
      DO 380 I=IBEG,IEND
        NALPHA(I) = LA(1)
 380  CONTINUE
C
C     ..................................................................
C
 390  KW = NW
      KD = ND
      KE = IE
      IF (IE.EQ.IZERO .OR. IE.EQ.IFIVE .OR. IE.EQ.6 .OR. IE.EQ.ITEN
     1                .OR. IE.EQ.11    .OR. IE.GE.14) RETURN
C       CALL ERROR (259)
        RETURN
C
C     ==================================================================
C
      END
*RNDATM
      SUBROUTINE RNDATM (L,N,M,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. RNDATM V 7.00 11/30/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ROUND ALPHA REPRESENTATION OF A NUMBER AT POSITION M. 
C
C     NUMBER IS STORED IN A1 FORMAT IN VECTOR L BY PROGRAM UNIT RFORMT.
C
C     IND = 0,    IF ROUND IS SUCCESSFUL.
C           1,    IF ROUND NOT SUCCESSFUL
C           2,    IF N IS LESS THAN 1.
C
C     IF L(M) = IPEROD, RETURN.
C
C     BLANKS TO RIGHT OF M REMAIN UNCHANGED.
C
C     BLANKS AT M AND LEFT ARE SET EQUAL TO ZERO, IF THERE IS A
C        NUMERIC CHARACTER ON LEFT.
C
C     IF BLANK AT M, L(M) = 0 AND NUMERIC CHARACTERS ON RIGHT
C        ARE SET TO ZERO.
C
C     IF -1.4236 IS IN L(1), L(2), L(3), L(4), L(5), L(6) AND L(7), THEN
C        CALL RNDATM (L,M,6,IND) GIVES
C           L(1)=-, L(2)=1, L(3)=., L(4)=4
C           L(5)=2, L(6)=4, AND L(7)=0
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
C                   CURRENT VERSION -  NOVEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER   L*1
      CHARACTER   LA*1
      CHARACTER*1 IBLANK, IMINUS, IPEROD
C     ..................................................................
C
      EQUIVALENCE (IBLANK,LA(45))
      EQUIVALENCE (IMINUS,LA(39))
      EQUIVALENCE (IPEROD,LA(38))
C
C     ==================================================================
C
      IND = IZERO
      IF (M.LE.IONE) GO TO 240
      IF (N.GE.IONE) GO TO 10 
      IND = ITWO
      RETURN
C
C     ..................................................................
C
  10  IF (M.EQ.N) RETURN
      IF (M.GT.N) GO TO 240
      DO 30 I=1,N
        IF (L(I).EQ.IBLANK) GO TO 30
        IF (L(I).EQ.IMINUS) GO TO 30
        IF (L(I).EQ.IPEROD) GO TO 30
        DO 20 J=1,10
          IF (L(I).EQ.LA(J)) GO TO 30
  20    CONTINUE
        GO TO 240
  30  CONTINUE
C
      IF (L(M).EQ.IPEROD) GO TO 240
      K = M + IONE
      IF (L(K).NE.IPEROD) GO TO 40
      K = K + IONE
  40  IF (L(K).EQ.IBLANK) RETURN
      IF (L(M).NE.IBLANK) GO TO 80
      DO 50 I=1,M
        IF (L(I).NE.IBLANK) GO TO 60
  50  CONTINUE
C
      L(M) = LA(1)
      GO TO 80
  60  J1 = M
      DO 70 I=1,M
        IF (L(J1).NE.IBLANK) RETURN
        L(J1) = LA(1)
        J1 = J1 - IONE
  70  CONTINUE
C
  80  J = IZERO
      DO 90 I=1,10
        IF (L(K).EQ.LA(I)) GO TO 100
        J = J + IONE
  90  CONTINUE
C
 100  IF (J.GE.ITEN) GO TO 240
      JJ = IZERO
      DO 110 I=1,10 
        IF (L(M).EQ.LA(I)) GO TO 120
        JJ = JJ + IONE
 110  CONTINUE
C
 120  IF (JJ.GE.ITEN) GO TO 240
      IF (J.LE.IFOUR) GO TO 220
      IF (J.GT.IFIVE) GO TO 150
      K1 = K + IONE 
      DO 130 I=K1,N 
        IF (L(I).EQ.IPEROD) GO TO 130
        IF (L(I).EQ.IBLANK) GO TO 140
        IF (L(I).NE.LA(1)) GO TO 150
 130  CONTINUE
C
 140  KK = MOD (JJ,ITWO)
      IF (KK.EQ.IZERO) GO TO 220
C
C     ROUND UP.
C
 150  JJ = JJ + ITWO
      IF (JJ.LE.ITEN) GO TO 210
      JJ = IONE
      DO 200 ISAL=2,M
        K2 = M - ISAL + IONE
        IF (L(K2).EQ.IPEROD) GO TO 200
        IF (L(K2).EQ.IMINUS) GO TO 200
        IF (L(K2).NE.IBLANK) GO TO 160
        L(K2) = LA(2)
        GO TO 210
 160    K4 = IZERO
        DO 170 I=1,10
          IF (L(K2).EQ.LA(I)) GO TO 180 
          K4 = K4 + IONE
 170    CONTINUE
 180    K4 = K4 + ITWO
        IF (K4.GT.ITEN) GO TO 190
        L(K2) = LA(K4)
        GO TO 210
 190    L(K2) = LA(1)
 200  CONTINUE
      GO TO 240
C
 210  L(M) = LA(JJ) 
C
C     REPLACE DROPPED NUMBERS BY ZEROS. 
C
 220  DO 230 I=K,N
        IF (L(I).EQ.IBLANK) RETURN
        IF (L(I).EQ.IPEROD) GO TO 230
        L(I) = LA(1)
 230  CONTINUE
      RETURN
C
C     ..................................................................
C
 240  IND = IONE
      RETURN
C
C     ==================================================================
C
      END 
*RNDOWN
      SUBROUTINE RNDOWN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RNDOWN V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     IF AN ERROR IS MADE IN A STORED STATEMENT,
C        PRINT OUT EXACTLY WHEN AND WHERE IT OCCURRED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             A
      REAL             FDIV
C
C     ==================================================================
C
      A = FDIV (FLOAT(INDEX(6,LEVEL)),RTEN,IND)
      WRITE (ISCRT,30) A
      IF (NCRT.NE.IZERO) WRITE(IPRINT,30) A
      N = LEVEL - IONE
C
  10  IF (N.EQ.IZERO) GO TO 20
      IF (N.LT.IZERO) RETURN
      A = FDIV (FLOAT(INDEX(6,N)),RTEN,IND)
      WRITE (ISCRT,40) INDEX(5,N+1), INDEX(4,N+1), A
      IF(NCRT.NE.IZERO) WRITE (IPRINT,40) INDEX(5,N+1), INDEX(4,N+1), A
      N = N - IONE
      GO TO 10
C
  20  WRITE (ISCRT,50) INDEX(5,1), INDEX(4,1)
      IF (NCRT.NE.IZERO) WRITE (IPRINT,50) INDEX(5,1), INDEX(4,1)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  30  FORMAT (13X,34HIN INSTRUCTION AT STATEMENT NUMBER,F6.1,17X)
  40  FORMAT (15X,9HCYCLE NO.,I4,3H OF,I4,24H OF PERFORM AT STATEMENT,
     1     F6.1,5X)
  50  FORMAT (15X,9HCYCLE NO.,I4,3H OF,I4,31H OF EXTERNAL PERFORM STATEM
     1ENT.,4X)
C
C     ==================================================================
C
      END
*RTHERR
      SUBROUTINE RTHERR (J,INF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. RTHERR V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR PRINTING ARITHMETIC DIAGNOSTICS.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C     PRINT GENERAL ARITHMETIC MESSAGE. 
C
      WRITE (IPRINT,100) INF
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), J
C
C     ..................................................................
C
C     PRINT SPECIFIC ARITHMETIC MESSAGE.
C
 1    WRITE (IPRINT,101)
      RETURN
 2    WRITE (IPRINT,102)
      RETURN
 3    WRITE (IPRINT,103)
      RETURN
 4    WRITE (IPRINT,104)
      RETURN
 5    WRITE (IPRINT,105)
      RETURN
 6    WRITE (IPRINT,106)
      RETURN
 7    WRITE (IPRINT,107)
      RETURN
 8    WRITE (IPRINT,108)
      RETURN
 9    WRITE (IPRINT,109)
      RETURN
10    WRITE (IPRINT,110)
      RETURN
11    WRITE (IPRINT,111)
      RETURN
12    WRITE (IPRINT,112)
      RETURN
13    WRITE (IPRINT,113)
      RETURN
14    WRITE (IPRINT,114)
      RETURN
15    WRITE (IPRINT,115)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
100   FORMAT ( 5X,55H** ARITHMETIC FAULT IN ABOVE INSTRUCTION. ZERO RETU
     1RNED,I4,6H TIMES)
C
101   FORMAT (10X,38HSQRT, LOG OR RAISE OF NEGATIVE NUMBER.,22X)
102   FORMAT (10X,32HEXPONENT TOO SMALL OR TOO LARGE.,28X)
103   FORMAT (10X,60HVALUE OUT OF RANGE AND INVERSE FUNCTION CANNOT BE E
     1VALUATED.)
104   FORMAT (10X,33HX TOO LARGE FOR SIN(X) OR COS(X).,27X) 
105   FORMAT (10X,44HVALUE SCALED TO AVOID OVERFLOW OR UNDERFLOW.,16X)
106   FORMAT (10X,17HDIVISION BY ZERO.,43X)
107   FORMAT (10X,35HTRIGONOMETRIC FUNCTION NOT DEFINED.,25X)
108   FORMAT (10X,60HONE OF THE VALUES COMPARED IS ZERO. ABSOLUTE TOLERA
     1NCE USED.)
109   FORMAT (10X,58HX FOR ELLIPTICAL INTEGRAL IS GREATER THAN OR EQUAL 
     1TO ONE.,2X)
110   FORMAT (10X,39HOVERFLOW FROM USE OF THE SUM ALGORITHM.,21X)
111   FORMAT (10X,49HFUNCTION NOT DEFINED FOR NEGATIVE OR ZERO VALUES.,
     1   11X)
112   FORMAT (10X,41HFUNCTION NOT DEFINED FOR NEGATIVE VALUES.,19X)
113   FORMAT (10X,37HFUNCTION NOT DEFINED FOR ZERO VALUES.,23X)
114   FORMAT (10X,51HY = F(X) IS NOT DEFINED FOR A SPECIFIED VALUE OF X.
     1   , 9X)
115   FORMAT (10X,51HFUNCTION NOT DEFINED FOR SPECIFIED PARAMETER VALUE.
     1   , 9X)
C
C     ==================================================================
C
      END 
*RULE
      SUBROUTINE RULE (XS,DEPTH,ICASE,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   RULE V 7.00 11/30/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     RULE TO DETERMINE I, J, K, AND L FOR STEM AND LEAF DISPLAY.
C        DEVELOPED BY WESLEY NICHOLSON. 
C
C     RULE IS AS FOLLOWS.
C
C           START SEQUENCE USING I=1
C           (SIMPLE) AND LET L=1.
C           FOR EACH VALUE OF J
C             J=1,...,N
C           SET K=1,2,4 AND 10
C
C       PRIMARY RULE ...
C
C           FIRST PAIR OF (J,K) WHICH SATISFIES THE FOLLOWING
C       (1) A .GE. 0.2 AND A .LE. 0.5
C           WHERE A=(NO. OF LINES FROM
C           HINGE(1) TO HINGE(2))/
C           TOTAL LINES.
C       (2) B .LE. 0.15   B=MAX CELL
C       (3) K .GE. 2
C           IF NOT SUCCESSFUL, STOP WHEN EITHER
C       (A) J+L+1 .GT. NO. OF 
C           SIGNIFICANT DIGITS IN
C           VALUES. 
C       (B) B .LT. 0.03
C       (C) LINES .GT. 50(LOG10(N))
C
C       SECONDARY RULE IF PRIMARY RULEE CAN NOT BE SATISIFIED ...
C
C           FIRST PAIR OF (J,K) WHICH SATISFIES
C       (1) K .GE. 2
C       (2) LINES .GE. 30
C
C       FINAL RULE ...
C
C       (1) K EQUAL TO 4
C       (2) AND NUMBER OF LINES IS MAXIMUM AND LESS THAN 30 
C
C          **** FOR ONE WHO WISHES TO CHANGE RULE 
C          (1)  THE FOLLOWING VARIABLES IN COMMON 
C                LABEL /SLEAF/ MUST BE DEFINED
C                 ILEAF = I AS DEFINED IN MAINSL. 
C                     SUBROUTINE  SANDL WILL
C                     CHANGE ILEAF
C                 JZ = J  SEE MAINSL FOR DEF.
C                 KZ = K  SEE MAINSL FOR DEF.
C                 LZ = L  SEE MAINSL FOR DEF.
C          (2)  A CALL TO SUBROUTINE SANDL (XS,
C                IND,DEPTH) MUST BE MADE EVERY
C                TIME THE STEM AND LEAF ARE TO BE 
C                DONE.
C          (3)  THE DIMENSION VARIABLE TEST HAS
C                THE FOLLOWING INFORMATION
C                TEST(1)=A
C                TEST(2)=B
C                TEST(3)=LINES
C               THE ARRAY TEST IS DEFINED BY
C                SUBROUTINE SANDL
C               ISIGNF = NO. OF SIGNIFICANT
C                DIGITS
C
C          (4)  LABEL COMMON SLEAF AND STEM
C                MUST BE INCLUDED IN NEW RULE
C                PROCEDURE
C          (5)  THE ARGUMENTS OF THE PROCEDURE
C                RULE MUST HAVE
C                 XS,X,N,ICASE,I,J,K,L,DEPTH,IND
C                 PLUS WHATEVER OTHER ARGUMENTS
C                 ONE ADDS.
C               IF MORE ARGUMENTS ARE ADDED TO
C                RULE, THEN CALL TO RULE IN
C          ****  PROCEDURE MAINSL MUST BE CHANGED 
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - NOVEMBER, 1969.
C                   CURRENT VERSION - NOVEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION KSL(4)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SLRVAR/ RI(5), SV(6)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             DEPTH(*), XS(*)
      REAL             FLOG10 
      REAL             SPCA, SPCB, SPCC, SPCD
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA KSL(1), KSL(2), KSL(3), KSL(4) / 1, 2, 4, 10 /
C
      DATA ICA / 30 /
      DATA ICB / 50 /
C
      DATA SPCA / 0.005 /
      DATA SPCB / 0.03  /
      DATA SPCC / 0.15  /
      DATA SPCD / 0.20  /
C
C     ==================================================================
C
      IND    = IZERO
      IPRT   = IPET 
      IDTH   = IDTHST
      IDTHST = IZERO
      LINE   = ICB * IFIX (FLOG10(FLOAT(NZ)))
      JLSWT  = IZERO
      ISIGNF = IZERO
      IPET   = IZERO
      MAXLNE = IZERO
      II     = IONE 
      LZ     = IONE 
      JSAVE  = IZERO
      KSAVE  = IZERO
      JFIN   = IZERO
      LINMAX = IZERO
      JR     = IONE 
10    IF (JR.EQ.IPER) JR = JR + IONE
      IF (JR+LZ+IONE.GT.ISIGNF .AND. ISIGNF.GT.IZERO) GO TO 60
      DO 40 M=1,4
        KZ     = KSL(M)
        ILEAF  = II 
        JZ     = JR 
        CALL SANDL (XS,DEPTH,ICASE,IND) 
        IF (IND.EQ.ITWO) GO TO 50
        IF (IND.NE.IZERO) GO TO 140
        JLSWT  = IONE
        ITEST  = TEST(3) + SPCA
        IF (KZ.EQ.IONE) GO TO 20
        IF (TEST(1).LT.SPCD .OR. TEST(1).GT.RHALF) GO TO 20 
        IF (TEST(2).LE.SPCC) GO TO 110
  20    IF (TEST(2).LT.SPCB) GO TO 60
        IF (ITEST.GT.LINE) GO TO 60
        IF (KZ.EQ.IONE) GO TO 40
        IF (KSAVE.GT.IZERO) GO TO 40
        IF (KZ.NE.IFOUR) GO TO 30
        IF (LINMAX.GE.ITEST) GO TO 30
        JFIN   = JR 
        LINMAX = ITEST
  30    IF (MAXLNE.GE.ITEST) GO TO 40
        MAXLNE = ITEST
        IF (MAXLNE.LT.ICA) GO TO 40
        JSAVE  = JR 
        KSAVE  = KZ 
  40  CONTINUE
  50  JR = JR + IONE
      GO TO 10
C
C     SECONDARY RULE FOR DETERMINING STEM AND LEAF TO BE PRINTED.
C
  60  IF (KSAVE.EQ.IZERO) GO TO 100
C
C     FIRST OPTION OF SECONDARY RULE.
C
      K2 = KSAVE
      J2 = JSAVE
  70  IF (K2.EQ.IFOUR) GO TO 80
      IF (K2.GT.IFOUR) GO TO 90
      K1 = IONE
      K3 = IFOUR
      J1 = J2
      J3 = J2
      GO TO 120
C
  80  K1 = ITWO
      K3 = ITEN
      J1 = J2
      J3 = J2
      GO TO 120
C
  90  K1 = IFOUR
      J1 = J2
      K3 = IONE
      J3 = J2 + IONE
      IF (J3.EQ.IPER) J3 = J3 + IONE
      IF (J3+LZ+IONE.GT.ISIGNF) K3 = IZERO
      GO TO 120
C
C     FINAL RULE.
C
 100  K1 = ITWO
      K2 = IFOUR
      K3 = ITEN
      J1 = JFIN
      J2 = JFIN
      J3 = JFIN
      GO TO 120
C
C     PRIMARY RULE FOR PICKING STEM AND LEAF.
C
 110  K2     = KZ
      J2     = JR
      GO TO 70
C
 120  IF (IPRT.EQ.IZERO) GO TO 130
      IPET   = IONE 
      ILEAF  = II
      JZ     = J1
      KZ     = K1
      CALL SANDL (XS,DEPTH,ICASE,IND)
      IF (IND.NE.IZERO) GO TO 140
 130  IF (IDTH.GT.IZERO) IDTHST = IDTH
      ILEAF  = II
      JZ     = J2
      KZ     = K2
      CALL SANDL (XS,DEPTH,ICASE,IND)
      IF (IND.NE.IZERO) GO TO 140
      IF (IPRT.EQ.IZERO) RETURN
      IF (K3.EQ.IZERO) GO TO 140
      IDTHST = IZERO
      ILEAF  = II
      JZ     = J3
      KZ     = K3
      CALL SANDL (XS,DEPTH,ICASE,IND)
      IDTHST = IDTH 
 140  IPET   = IPRT 
      RETURN
C
C     ==================================================================
C
      END 
