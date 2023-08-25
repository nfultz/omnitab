*FLIP
      SUBROUTINE FLIP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FLIP V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     FLIP COL (C) INTO COL (C), (C) INTO (C), ETC.
C
C     IF NARGS = 0, FLIP THE ENTIRE WORKSHEET.
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
      REAL             B
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NARGS.EQ.IZERO) GO TO 20
      IF (MOD(NARGS,ITWO).EQ.IZERO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL CHKCOL
  20  IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.EQ.IONE) RETURN
      IF (NRMAX.GT.IONE) GO TO 40
      CALL ERROR (9)
      RETURN
C
C     ==================================================================
C
  40  KK = NRMAX - IONE
      K = IDIV(KK,ITWO,IND)
      IF (NARGS.EQ.IZERO) GO TO 70
C
      DO 60 I=1,NARGS,2
        M = IARGS(I)
        N = IARGS(I+1)
        MM = M + KK
        NN = N + KK
        MMM = M + K
        DO 50 J=M,MMM
          B = RC(J)
          RC(N) = RC(MM)
          RC(NN) = B
          N = N + IONE
          MM = MM - IONE
          NN = NN - IONE
  50    CONTINUE
  60  CONTINUE
      RETURN
C
C      FLIP ENTIRE ARRAY
C
  70  N = IONE
      IF (MOD(NRMAX,ITWO).EQ.IZERO) K = K + IONE
      DO 90 I=1,NCOL
        M = N
        MM = M + KK
        DO 80 J=1,K
          B = RC(M)
          RC(M) = RC(MM)
          RC(MM) = B
          M = M + IONE
          MM = MM - IONE
  80    CONTINUE
        N = N + NROW
  90  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*FPROB
      SUBROUTINE FPROB
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FPROB V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTION EXECUTED IS ...
C
C        F PROBABILITY V1 (E), V2 (E), F  (E), STORE Q IN COL (C)
C
C        WHERE V1 = NUMERATOR DEGREES OF FREEDOM
C              V2 = DENOMINATOR DEGREES OF FREEDOM AND
C               Q = RIGHT TAIL AREA OF F-DISTRIBUTION.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DELTA, F, Q, V1, V2
      REAL             V1I, V2I
C
C     ==================================================================
C
      IF (NARGS.NE.IFOUR) CALL ERROR (10)
      IF (KIND(NARGS).NE.IZERO) CALL ERROR (20)
      I1 = IONE
      I2 = IONE
      I3 = IONE
      L  = IONE
      M  = IONE
      N  = IONE
      V1 = RZERO
      V2 = RZERO
      CALL ADRESS (IONE,IARGS(1))
      IF (IARGS(1).GE.IZERO) GO TO 10
      I1 = ITWO
      V1 = ARGS(1)
      GO TO 20
  10  L = IARGS(1)
  20  CALL ADRESS (ITWO,IARGS(2))
      IF (IARGS(2).GE.IZERO) GO TO 30
      I2 = ITWO
      V2 = ARGS(2)
      GO TO 40
  30  M = IARGS(2)
  40  CALL ADRESS (ITHRE,IARGS(3))
      IF (IARGS(3).GE.IZERO) GO TO 50
      I3 = ITWO
      F  = ARGS(3)
      GO TO 60
  50  N = IARGS(3)
  60  CALL ADRESS (NARGS,K)
      IF (K.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
      IXTIND = IZERO
      IDFIND = IZERO
      JDFIND = IZERO
      DELTA  = RTEN * RTEN * RER
C
      IF (I1+I2+I3.NE.6) GO TO 140
      IF (V1.GE.RONE) GO TO 70
        V1I    = RONE
        IDFIND = IDFIND + IONE
        GO TO 80
  70  V1I = AINT (V1+DELTA)
      IF (ABS(V1I-V1).GT.DELTA) JDFIND = JDFIND + IONE
  80  IF (V2.GE.RONE) GO TO 90
        V2I    = RONE
        IDFIND = IDFIND + IONE
        GO TO 100
  90  V2I = AINT (V2+DELTA)
      IF (ABS(V2I-V2).GT.DELTA) JDFIND = JDFIND + IONE
 100  IF (F.GE.RZERO) GO TO 110
        Q      = RONE
        IXTIND = IXTIND + IONE
        GO TO 120
 110  CALL QFORF (V1I,V2I,F,Q)
 120  DO 130 I=1,NRMAX
        RC(K) = Q
        K = K + IONE
 130  CONTINUE
      GO TO 250
C
C     ..................................................................
C
 140  DO 240 I=1,NRMAX
        IF (I1.EQ.ITWO) GO TO 150
        V1 = RC(L)
        L = L + IONE
 150    IF (I2.EQ.ITWO) GO TO 160
        V2 = RC(M)
        M = M + IONE
 160    IF (I3.EQ.ITWO) GO TO 170
        F = RC(N)
        N = N + IONE
 170    IF (V1.GE.RONE) GO TO 180
          V1I    = RONE
          IDFIND = IDFIND + IONE
          GO TO 190
 180    V1I = AINT (V1+DELTA)
        IF (ABS(V1I-V1).GT.DELTA) JDFIND = JDFIND + IONE
 190    IF (V2.GE.RONE) GO TO 200
          V2I    = RONE
          IDFIND = IDFIND + IONE
          GO TO 210
 200    V2I = AINT (V2+DELTA)
        IF (ABS(V2I-V2).GT.DELTA) JDFIND = JDFIND + IONE
 210    IF (F.GE.RZERO) GO TO 220
          RC(K)  = RONE
          IXTIND = IXTIND + IONE
          GO TO 230
 220    CALL QFORF (V1I,V2I,F,RC(K))
 230    K = K + IONE
 240  CONTINUE
C
 250  IF (IXTIND.NE.IZERO) CALL ERROR (258)
      IF (IDFIND.NE.IZERO) CALL ERROR (207)
      IF (JDFIND.NE.IZERO) CALL ERROR (208)
C
      RETURN
C
C     ==================================================================
C
      END
*FRDIST
      SUBROUTINE FRDIST
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FRDIST V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C               ***** FORMS OF FREQUENCY INSTRUCTION *****
C
C   1 FREQUENCY OF COLUMN (C), PUT IN COLUMN (C)
C   2 FREQUENCY OF COL (C), USE (I) CELLS, PUT IN COLUMN (C)
C   3 FREQUENCY OF COL (C), USE (I) CELLS, OF LENGTH (A), PUT IN COL (C)
C   4 FREQUENCY OF COL (C), CELLS (I), LENGTH (A), START (A), IN COL (C)
C   5 FREQUENCY OF COL (C), LOWER (C), UPPER IN (C), FREQ IN (C)
C   6 FREQUENCY OF COL (C), USE (I) CELLS, PUT IN (C), (C), (C)
C   7 FREQUENCY OF COL (C), CELLS (I), LENGTH (A), PUT IN (C), (C), (C)
C   8 FREQUENCY OF (C), CELLS (I), LENGTH (A), START (A), IN (C),(C),(C)
C
C     RESETS NRMAX TO NUMBER OF CELLS.
C
C               WRITTEN BY -
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
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             CELL, STRT
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.GT.IZERO)  GO TO 10
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.GT.IONE .AND. NARGS.LT.8) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  CALL ADRESS (IONE,J1)
      IF (J1.GE.IZERO) GO TO 40
  30  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  40  CALL ADRESS (NARGS,J2)
      IF (J2.GE.IZERO) GO TO 50
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  50  NST = IONE
      LIMIT = IZERO
C
C     FORM (1)
C
      IF (NARGS.EQ.ITWO) GO TO 100
      IF (NARGS.EQ.IFOUR .AND. KIND(3).EQ.IZERO)  GO TO 90
      IF (KIND(2).EQ.IZERO)  GO TO 60
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  60  KN = IARGS(2)
C
C     FORM (2)
C
      IF (KN.LE.NROW)  GO TO 70
      CALL ERROR (226)
      KN = NROW
  70  IF (NARGS.EQ.ITHRE)  GO TO 110
      IF (KIND(3).EQ.IZERO) GO TO 90
      CELL = ARGS(3)
C
C     FORM (3)
C
      IF (NARGS.EQ.IFOUR) GO TO 120
      IF (KIND(4).EQ.IZERO) GO TO 90
      NST = IZERO
      STRT = ARGS(4)
C
C     FORM (4)
C
      IF (NARGS.EQ.IFIVE) GO TO 130
C
C     FORMS (5), (6), (7), (8)
C
  90  CALL ADRESS (NARGS-ITWO,J3)
      IF (J3.LT.IZERO) GO TO 30
      CALL ADRESS (NARGS-IONE,J4)
      IF (J4.LT.IZERO) GO TO 30
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      LIMIT = IONE
      JJ = NARGS - ITHRE
      GO TO (100,110,120,130), JJ
 100  KN = IZERO
 110  CELL = RZERO
 120  STRT = RZERO
 130  CALL FREQCY (RC(J1),A(1),NRMAX,KN,CELL,NST,STRT,LIMIT,
     1   RC(J3),RC(J4))
      NROLD = NRMAX
      NRMAX = KN
      CALL ERROR (252)
      DO 140 I=1,NRMAX
        RC(J2) = A(I)
        J2 = J2 + IONE
 140  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*FTLERR   
      SUBROUTINE FTLERR (INFA,INFB,NBRERR)        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FTLERR V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PRINT FATAL ERROR MESSAGE.        
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
      CHARACTER NEWCRD*1
C
C     ==================================================================        
C         
      IF (INUNIT .NE. 5) THEN
        WRITE (IPRINT,430) NEWCRD
        WRITE (IPRINT,410)      
      ENDIF
      IF ( INFB.EQ.IZERO) WRITE (IPRINT,420)      
C         
      GO TO (101,102,103,104,105,106,107,108,109,110,       
     1       111,112,113,114,115,116,117,118,119,120,       
     2       121,122,123,124,125,126,127,128,129,130,       
     3       131,132,133,134,135,136,137,138,139,140,       
     4       141,142,143,144,145,146,147), NBRERR 
C         
C     ..................................................................        
C         
 101  WRITE (IPRINT, 1)       
      RETURN        
C         
 102  WRITE (IPRINT, 2)       
      RETURN        
C         
 103  WRITE (IPRINT, 3)       
      RETURN        
C         
 104  WRITE (IPRINT, 4)       
      RETURN        
C         
 105  WRITE (IPRINT, 5)       
      RETURN        
C         
 106  WRITE (IPRINT, 6)       
      RETURN        
C         
 107  WRITE (IPRINT, 7)       
      RETURN        
C         
 108  WRITE (IPRINT, 8)       
      RETURN        
C         
 109  WRITE (IPRINT, 9)       
      RETURN        
C         
 110  WRITE (IPRINT,10)  INFA 
      RETURN        
C         
 111  WRITE (IPRINT,11) INFB  
      RETURN        
C         
 112  WRITE (IPRINT,12)       
      RETURN        
C         
 113  WRITE (IPRINT,13)       
      RETURN        
C         
 114  WRITE (IPRINT,14)       
      RETURN        
C         
 115  WRITE (IPRINT,15) NRC   
      RETURN        
C         
 116  WRITE (IPRINT,16) INFA  
      RETURN        
C         
 117  WRITE (IPRINT,17) INFA, INFB      
      RETURN        
C         
 118  WRITE (IPRINT,18)       
      RETURN        
C         
 119  WRITE (IPRINT,19)       
      RETURN        
C         
 120  WRITE (IPRINT,20)       
      RETURN        
C         
 121  WRITE (IPRINT,21)       
      RETURN        
C         
 122  WRITE (IPRINT,22)       
      RETURN        
C         
 123  WRITE (IPRINT,23)       
      RETURN        
C         
 124  WRITE (IPRINT,24)       
      RETURN        
C         
 125  WRITE (IPRINT,25)       
      RETURN        
C         
 126  WRITE (IPRINT,26)       
      RETURN        
C         
 127  WRITE (IPRINT,27)       
      RETURN        
C         
 128  WRITE (IPRINT,28)       
      RETURN        
C         
 129  WRITE (IPRINT,29) INFA  
      RETURN        
C         
 130  WRITE (IPRINT,30)       
      RETURN        
C         
 131  WRITE (IPRINT,31)       
      RETURN        
C         
 132  WRITE (IPRINT,32)       
      RETURN        
C         
 133  WRITE (IPRINT,33)       
      RETURN        
C         
C     THE NEXT 3 FATAL ERROR MESSAGES ARE USED BY STEM LEAF INSTRUCTIONS        
C         
 134  WRITE (IPRINT,34) INFA, INFB      
      RETURN        
C         
 135  WRITE (IPRINT,35)       
      RETURN        
C         
 136  WRITE (IPRINT,36)       
      RETURN        
C         
 137  WRITE (IPRINT,37)       
      RETURN        
C         
 138  WRITE (IPRINT,38)       
      RETURN        
C         
C     FATAL ERRORS IN LABEL INSTRUCTIONS.         
C         
 139  WRITE (IPRINT,39)       
      RETURN        
C         
 140  WRITE (IPRINT,40)       
      RETURN        
C         
 141  WRITE (IPRINT,41)       
      RETURN        
C         
 142  WRITE (IPRINT,42)       
      RETURN        
C         
 143  WRITE (IPRINT,43)       
      RETURN        
C         
 144  WRITE (IPRINT,44)       
      RETURN        
C         
 145  WRITE (IPRINT,45)       
      RETURN        
C         
 146  WRITE (IPRINT,46)       
      RETURN        
C         
 147  WRITE (IPRINT,47)       
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
   1  FORMAT (10X,23HCOMMAND DOES NOT EXIST.,37X) 
   2  FORMAT (10X,27HINCORRECT STATEMENT NUMBER.,33X)       
   3  FORMAT (10X,34HINCORRECT ARGUMENT IN INSTRUCTION.,26X)
   4  FORMAT (10X,30HFIRST COMMAND MUST BE OMNITAB.,30X)    
   5  FORMAT (10X,42HINSTRUCTION IS NOT ALLOWED IN REPEAT MODE.,18X)  
   6  FORMAT (10X,57HINSTRUCTIONS BETWEEN BEGIN AND FINISH CANNOT BE NUM        
     1BERED.,3X)    
   7  FORMAT (10X,49HFUNCTION NAME NOT FOLLOWED BY A LEFT PARENTHESIS.,         
     1   11X)       
   8  FORMAT (10X,60HLAST TERM IN EXPRESSION IS EITHER AN OPERATOR OR A         
     1FUNCTION.)    
   9  FORMAT (10X,10HNRMAX = 0.,50X)    
  10  FORMAT (10X,I4,37H IS AN INCORRECT NUMBER OF ARGUMENTS.,19X)    
  11  FORMAT (10X,25HCOLUMN NUMBER(S) OUTSIDE ,I4,18H COLUMN WORKSHEET.,        
     1   13X)       
  12  FORMAT (10X,31HTOO MANY NUMBERED INSTRUCTIONS.,29X)   
  13  FORMAT (10X,29HINSTRUCTION NUMBER NOT FOUND.,31X)     
  14  FORMAT (10X,39HALL NUMBERS IN THE COLUMN ARE THE SAME.,21X)     
  15  FORMAT (10X,39HDIMENSIONED WORKSHEET EXCEEDS LIMIT OF ,I5,1H.,15X)        
  16  FORMAT (10X,22HROW NUMBER(S) OUTSIDE ,I4,15H ROW WORKSHEET.,19X)
  17  FORMAT (10X,24HARRAY OR MATRIX OUTSIDE ,I4,7H ROW X ,I4,18H COLUMN        
     1 WORKSHEET.,3X)         
  18  FORMAT (10X,36HINTEGER ARGUMENT IS LESS THAN -8191.,24X)        
  19  FORMAT (10X,45HNUMBERED PERFORM INSTRUCTION EXECUTES ITSELF.,15X)         
  20  FORMAT (10X,26HIMPROPER TYPE OF ARGUMENT.,34X)        
  21  FORMAT (10X,27HINSTRUCTION MUST BE STORED.,33X)       
  22  FORMAT (10X,28HMATRIX IS (NEARLY) SINGULAR.,32X)      
  23  FORMAT (10X,26HINSUFFICIENT SCRATCH AREA.,34X)        
  24  FORMAT (10X,50HDEGREE IS GREATER THAN NUMBER OF NON-ZERO WEIGHTS.,        
     1   10X)       
  25  FORMAT (10X,33HNEGATIVE WEIGHTS MAY NOT BE USED.,27X) 
  26  FORMAT (10X,36HINCONSISTENT ROW AND COLUMN NUMBERS.,24X)        
  27  FORMAT (10X,41HMISSING OR INCORRECT FORMAT OR QUALIFIER.,19X)   
  28  FORMAT (10X,46HEACH AND EVERY GROUP HAS ONLY ONE MEASUREMENT.,14X)        
  29  FORMAT (10X,29HNUMBER OF ARGUMENTS SHOULD BE,I2,1H.,28X)        
  30  FORMAT (10X,49HAN INCREMENT INSTRUCTION CANNOT INCREMENT ITSELF.,         
     1   11X)       
  31  FORMAT (10X,24HMATRIX IS NOT SYMMETRIC.,36X)
  32  FORMAT (10X,57HSTORAGE COLUMN NUMBERS CANNOT EQUAL OTHER COLUMN NU        
     1MBERS.,3X)    
  33  FORMAT (10X,54HITERATIVE REFINEMENT FAILED TO CONVERGE TO A SOLUTI        
     1ON.,6X)       
  34  FORMAT (10X,9HNRMAX IS ,I2,39H  AND MUST BE GREATER THAN OR EQUAL         
     1TO ,I2,8X)    
  35  FORMAT (10X,44HVALUE OF SOME MEASUREMENT IS NOT ACCEPTABLE.,16X)
  36  FORMAT (10X,49HI,J,K AND/OR L ARE NOT DEFINED CORRECTLY, OR RULE,1        
     11X/12X,56HCANNOT DETERMINE PROPER PARAMETER VALUES FOR THESE DATA.        
     2,2X)
  37  FORMAT (10X,47HTHE ARGUMENTS *NRMAX*, **NRMAX**, *V*, OR **V**,13X        
     1 /12X,31HCAN ONLY BE INCREMENTED BY 0.0.,27X)         
  38  FORMAT (10X,51HFUNCTION NOT DEFINED FOR SPECIFIED PARAMETER VALUE.        
     1   ,9X)       
  39  FORMAT (10X,53HLABEL OF A COLUMN (ARRAY, MATRIX) CANNOT BE A NUMBE        
     1R.,7X)        
  40  FORMAT (10X,57HA LABEL MUST HAVE AT LEAST 1 AND LESS THAN 13 CHARA        
     1CTERS,,3X/10X,33HOR ILLEGAL CHARACTER(S) IN LABEL.,27X)         
  41  FORMAT (10X,25HNUMBER OF LABELS EXCEEDS ,I4,26H COLUMNS IN THE WOR        
     1KSHEET.,5X)   
  42  FORMAT (10X,56HLABEL IN ALABEL OR MLABEL MUST BE FOLLOWED BY 4 NUM        
     1BERS.,4X)     
  43  FORMAT (10X,27HTHE CHARACTER = IS MISSING.,33X)       
  44  FORMAT (10X,55HTHE NUMBER OF LEFT AND RIGHT PARENTHESES ARE NOT EQ        
     1UAL.,5X)      
  45  FORMAT (10X,41HILLEGAL OPERATOR, EXPRESSION OR FUNCTION.,19X)   
  46  FORMAT (10X,21HALL WEIGHTS ARE ZERO.,39X)   
  47  FORMAT (10X,50HNO UNITS AVAILABLE, OR MORE THAN 10 FILES USED, OR/        
     1        10X,22HNO FILE NAME SPECIFIED,38X)  
C         
C     ..................................................................        
C         
 410  FORMAT (/   4X,38H*** FATAL ERROR IN ABOVE INSTRUCTION -,28X)   
 420  FORMAT (10X,58HINSTRUCTION NOT LISTED BECAUSE NO LIST OR LIST 0 WA        
     1S USED.,1X)   
 430  FORMAT (1X,80A1)
C         
C     ==================================================================        
C         
      END 
*FUNCT
      SUBROUTINE FUNCT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FUNCT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE ALL TWO, THREE, AND FOUR ARGUMENT FUNCTIONS.
C
C     IF THE FIRST ARGUMENT IS A CONSTANT, THE FUNCION IS EVALUATED
C        ONLY ONCE.
C
C     L2 =  1  SIN         L2 =  2  COS          L2 =  3  TAN
C     L2 =  4  COT         L2 =  5  ARCSIN       L2 =  6  ARCCOS
C     L2 =  7  ARCTAN      L2 =  8  ARCCOT       L2 =  9  SIND
C     L2 = 10  COSD        L2 = 11  TAND         L2 = 12  COTD
C     L2 = 13  ASIND       L2 = 14  ACOSD        L2 = 15  ATAND
C     L2 = 16  ACOTD       L2 = 17  SQRT         L2 = 18  EXPONENT
C     L2 = 19  NEGEXP      L2 = 20  LOGE         L2 = 21  LOGTEN
C     L2 = 22  ANTILO      L2 = 23  SINH         L2 = 24  COSH
C     L2 = 25  TANH        L2 = 26  COTH         L2 = 27  ASINH
C     L2 = 28  ACOSH       L2 = 29  ATANH        L2 = 30  ACOTH
C     L2 = 31  ABSOLU      L2 = 32  INTEGE       L2 = 33  FRACTI
C     L2 = 34  SQUARE      L2 = 35  RECIPROCAL
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION II(4), KK(4)
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
      REAL             XD(4)
      REAL             AX, X, XA, XB, X2, X3, Y, YY, Z
      REAL             FCOS, FDIV, FEXP, FLOG, FLOG10
      REAL             FSIN, FSQRT, FTANH
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.LT.ITWO .OR. NARGS.GT.IFOUR) CALL ERROR (10)
      IF (NARGS.EQ.ITHRE) CALL ERROR (29)
      IF (NERROR.NE.IZERO) RETURN
      DO 10 I=1,NARGS
        J     = I
        KK(I) = IONE
        XD(I) = RZERO
        CALL ADRESS (J,II(J))
        IF (II(I).GT.IZERO) GO TO 10
        KK(I) = IZERO
        II(I) = I
        XD(I) = ARGS(I)
  10  CONTINUE
      I1 = II(1)
      I2 = II(2)
      I3 = II(3)
      I4 = II(4)
      X  = XD(1)
      X2 = XD(2)
      X3 = XD(3)
      XA = RZERO
      IF (KK(NARGS).EQ.IZERO) CALL ERROR (11)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      NR = NRMAX
      IF (NARGS.EQ.IFOUR) GO TO 20
      KK(4) = IONE
      I4 = I2
      IF (NARGS.EQ.ITHRE) I4 = I3
  20  IL = I4
      ILL = IONE
      IF (KK(1).EQ.IONE) X = RC(I1)
      IF (KK(1).EQ.IONE) GO TO 30
      IF (NARGS.EQ.ITWO) NR = IONE
      X = ARGS(1)
C
  30  DO 5060 I=1,NR
        IF (ILL.EQ.ITWO) GO TO 5040
        GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     1         1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     2         2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,
     3         3100,3200,3300,3400,3500), L2
C
C     ..................................................................
C
 100    X = FSIN (X)
        GO TO 5030
C
 200    X = FCOS (X)
        GO TO 5030
C
 300    AX = FCOS (X)
        IF (AX.EQ.RZERO) GO TO 5020
        X = FDIV (FSIN(X),AX,IND)
        GO TO 5030
C
 400    AX = FSIN (X)
        IF (AX.EQ.RZERO) GO TO 5010
        X = FDIV (FCOS(X),AX,IND)
        GO TO 5030
C
 500    Y = X**2
        IF (Y.GT.RHALF) GO TO 510
        X = ATAN (FDIV(X,FSQRT(RONE-Y),IND))
        GO TO 580
 510    Z = X
        IF (X.NE.RZERO) GO TO 520
        X = HALFPI
        GO TO 570
 520    IF (ABS(X).GT.RONE) GO TO 5010
        IF (ABS(X).EQ.RONE) GO TO 550
        YY = RONE
        DO 530 J=1,3
          Y = X**2
          IF (Y.LE.RHALF) GO TO 540
          YY = YY + YY
          X = Y + Y - RONE
 530    CONTINUE
C
        Y = X**2
 540    X = SIGN (FDIV(ATAN(FDIV(FSQRT(RONE-Y),X,IND)),YY,IND),Z)
        GO TO 560
 550    X = RZERO
 560    IF (Z.LT.RZERO) X = PI + X
 570    IF (L2.EQ.IFIVE .OR. L2.EQ.13) X = HALFPI - X
 580    IF (L2.GT.ITEN) X = DEG * X
        GO TO 5030
C
 600    GO TO 510
 700    X = ATAN (X)
        GO TO 5030
C
 800    IF (X.EQ.RZERO) GO TO 5010
        X = ATAN (FDIV(RONE,X,IND))
        GO TO 5030
C
 900    X = FSIN (RAD*X)
        GO TO 5030
C
1000    X = FCOS (RAD*X)
        GO TO 5030
C
1100    X = X * RAD
        GO TO 300
1200    X = X * RAD
        GO TO 400
1300    GO TO 500
1400    GO TO 510
1500    X = DEG * ATAN (X)
        GO TO 5030
C
1600    IF (X.EQ.RZERO) GO TO 5020
        X = DEG * ATAN (FDIV(RONE,X,IND))
        GO TO 5030
C
1700    X = FSQRT (X)
        GO TO 5030
C
1800    X = FEXP (X)
        GO TO 5030
C
1900    X = FEXP (-X)
        GO TO 5030
C
2000    X = FLOG (X)
        GO TO 5030
C
2100    IF (X.GT.RZERO) GO TO 2110
        X = RZERO
        CALL ERROR (101)
        GO TO 5030
2110    X = FLOG10 (X)
        GO TO 5030
C
2200    IF (X.GT.RALOG) GO TO 2210
        X = RTEN**X
        GO TO 5030
2210    X = RZERO
        CALL ERROR (102)
        GO TO 5030
C
2300    AX = FTANH (X)
        GO TO 2410
2400    AX = RONE
2410    Y = FEXP (X)
        X = RHALF * (Y + FDIV (RONE,Y,IND) ) * AX
        GO TO 5030
C
2500    X = FTANH (X)
        GO TO 5030
C
2600    Y = FTANH (X)
        IF (Y.EQ.RZERO) GO TO 5010
        X = FDIV (RONE,Y,IND)
        GO TO 5030
C
2700    X = SIGN (FLOG(ABS(X)+FSQRT(X**2+RONE)),X)
        GO TO 5030
C
2800    X = FLOG (ABS(X)+FSQRT(X**2-RONE))
        GO TO 5030
C
2900    IF (ABS(X).GE.RONE) GO TO 5010
C
        X = RHALF * FLOG (FDIV(RONE+X,RONE-X,IND))
        GO TO 5030
C
3000    IF (ABS(X).LE.RONE) GO TO 5010
        X = RHALF * FLOG (FDIV(X+RONE,X-RONE,IND))
        GO TO 5030
C
3100    X = ABS (X)
        GO TO 5030
C
3200    X = AINT (X)
        GO TO 5030
C
3300    X = X - AINT (X)
        GO TO 5030
C
3400    X = X * X
        GO TO 5030
C
3500  X = FDIV (RONE,X,IND)
        GO TO 5030
C
C     ..................................................................
C
5010    X = RZERO
        CALL ERROR (103)
        GO TO 5030
5020    X = RZERO
        CALL ERROR (107)
5030    XA = X
        XB = XA
        IF (KK(1).EQ.IONE) X = RC(I1+1)
        IF (NARGS.EQ.ITWO) GO TO 5050
5040    IF (KK(2).EQ.IONE) X2 = RC(I2)
        IF (KK(3).EQ.IONE) X3 = RC(I3)
        XB = XA * X2 + X3
5050    RC(I4) = XB
        I1 = I1 + KK(1)
        I2 = I2 + KK(2)
        I3 = I3 + KK(3)
        I4 = I4 + KK(4)
        IF (I.EQ.IONE .AND. NARGS.NE.ITWO .AND. KIND(1).EQ.IONE)
     1    ILL = ITWO
5060  CONTINUE
      IF (KIND(1).EQ.IONE .AND. NR.EQ.IONE) CALL VECTOR (RC(IL),IL)
      RETURN
C
C     ==================================================================
C
      END
*GAMMA
      SUBROUTINE GAMMA (X,G)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  GAMMA V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE THE COMPLETE GAMMA FUNCTION.
C
C     REDUCE X TO Z IN INTERVAL -0.5 TO +0.5 INCLUSIVE AND THEN USE
C        APPROXIMATION 6.1.34 OF NBS AMS 55 TO OBTAIN GAMMA(Z). THEN
C           USE RECURRENCE RELATION TO OBTAIN GAMMA(X).
C
C     SUGGESTIONS OF BRADLEY A. PEAVY ARE USED.
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
      INCLUDE 'WRKSCR.H'
C
      REAL             X, G
      REAL             RX, Z
      REAL             FDPCON
C
      DOUBLE PRECISION DZMULT(26), DZRAIS(26), DPROD(26)
      DOUBLE PRECISION DZ, DSMNEG, DSMPOS, DM, DG
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
      EQUIVALENCE (DZRAIS(1),A(1)), (DPROD(1),A(101))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DZMULT ( 1) /  1.0000000000000000D0 /
      DATA DZMULT ( 2) /  5.772156649015329D-1 /
      DATA DZMULT ( 3) / -6.558780715202538D-1 /
      DATA DZMULT ( 4) / -4.20026350340952D-2  /
      DATA DZMULT ( 5) /  1.665386113822915D-1 /
      DATA DZMULT ( 6) / -4.21977345555443D-2  /
      DATA DZMULT ( 7) / -9.6219715278770D-3   /
      DATA DZMULT ( 8) /  7.2189432466630D-3   /
      DATA DZMULT ( 9) / -1.1651675918591D-3   /
      DATA DZMULT (10) / -2.152416741149D-4    /
      DATA DZMULT (11) /  1.280502823882D-4    /
      DATA DZMULT (12) / -2.01348547807D-5     /
      DATA DZMULT (13) / -1.2504934821D-6      /
      DATA DZMULT (14) /  1.1330272320D-6      /
      DATA DZMULT (15) / -2.056338417D-7       /
      DATA DZMULT (16) /  6.1160950D-9         /
      DATA DZMULT (17) /  5.0020075D-9         /
      DATA DZMULT (18) / -1.1812746D-9         /
      DATA DZMULT (19) /  1.043427D-10         /
      DATA DZMULT (20) /  7.7823D-12           /
      DATA DZMULT (21) / -3.6968D-12           /
      DATA DZMULT (22) /  5.100D-13            /
      DATA DZMULT (23) / -2.06D-14             /
      DATA DZMULT (24) / -5.4D-15              /
      DATA DZMULT (25) /  1.4D-15              /
      DATA DZMULT (26) /  1.0D-16              /
C
C     ==================================================================
C
      RX = AINT (X)
C
C     IF X IS NOT AN INTEGER, USE APPROXIMATION.
C
      IF (X.NE.RX)    GO TO 20
C
C     GAMMA(X) IS NOT DEFINED FOR NEGATIVE INTEGERS.
C
      IF (X.GE.RZERO) GO TO 10
        G = RZERO
        CALL ERROR (114)
        RETURN
C
C     ..................................................................
C
C     X IS AN INTEGER IS A SPECIAL CASE.
C
  10  DG = DONE
C
C     GAMMA(0) = GAMMA(1) = GAMMA(2) = 1.
C
      IF (X.EQ.RZERO .OR. X.EQ.RONE .OR. X.EQ.RTWO) GO TO 110
      DM = RTWO
      M  = X - RTWO
      GO TO 70
C
C     REDUCE X TO Z IN INTERVAL BETWEEN -0.5 AND +0.5 INCLUSIVE.
C
  20  Z  = X - RX
      IF (Z.GT.RHALF)  Z = Z - RONE
      IF (Z.LT.(-RHALF)) Z = Z + RONE
      DZ = Z
C
C     USE APPROXIMATION 6.1.34 OF NBS AMS 55.
C
      DZRAIS(1) = DZ
      DO 30 I=2,26
        DZRAIS(I) = DZ * DZRAIS(I-1)
  30  CONTINUE
C
      DO 40 I=1,26
        DPROD(I) = DZMULT(I) * DZRAIS(I)
  40  CONTINUE
C
      CALL DSUMAL (DPROD,IZERO,DSMNEG)
      DO 50 I=1,26
        K = 27 - I
        IF (DPROD(K).LT.DZERO) CALL DSUMAL (DPROD(K),-IONE,DSMNEG)
  50  CONTINUE
      CALL DSUMAL (DPROD,IONE,DSMNEG)
C
      CALL DSUMAL (DPROD,IZERO,DSMPOS)
      DO 60 I=1,26
        K = 27 - I
        IF (DPROD(K).GT.DZERO) CALL DSUMAL (DPROD(K),-IONE,DSMPOS)
  60  CONTINUE
      CALL DSUMAL (DPROD,IONE,DSMPOS)
C
      DG = FDDIV (DONE,DSMPOS+DSMNEG,IND)
      IF (ABS(X).LE.RHALF) GO TO 110
C
C     OBTAIN GAMMA(X) USING RECURRENCE RELATION
C        GAMMA(X+1) = X*GAMMA(X)
C
      M    = ABS (RX)
      IF (ABS(X-RX).GT.RHALF) M = M + IONE
      IF (X.LT.RZERO) GO TO 90
      DM   = DZ
C
C     USE RECURRENCE RELATION WHEN X IS POSITIVE.
C
  70  DO 80 I=1,M
        DG = DM * DG
        DM = DM + DONE
  80  CONTINUE
      GO TO 110
C
C     USE RECURRENCE RELATION WHEN X IS NEGATIVE.
C
C        GAMMA(X) = GAMMA(X+1) / X.
C
  90  DM = DZ - DONE
      DO 100 I=1,M
        DG = FDDIV (DONE,DM,IND) * DG
        DM = DM - DONE
 100  CONTINUE
      GO TO 110
C
 110  G = FDPCON ( DG )
C
      RETURN
C
C     ==================================================================
C
      END
*GAMPLT
      SUBROUTINE GAMPLT (X,Y,W,N,GAMMA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GAMPLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A GAMMA
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = GAMMA).
C              THE PROTOTYPE GAMMA DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE GAMMA PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE  GAMMA DISTRIBUTION
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
C     OUTPUT--A ONE-PAGE GAMMA PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ABS, EXP, DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION AND DOUBLE PRECISION
C     LANGUAGE--ANSI FORTRAN.
C
C     REFERENCES--WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41.
C               --FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--NOVEMBER  1974.
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
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
      REAL             ACOUNT, AJ, AN, CC, CUTOFF, CUT1, CUT2
      REAL             G, P, PCALC, RGAMMA, RP, RX, SMALL, SUM, SUM1
      REAL             SUM2, SUM3, TENMIL, TERM, U, WBAR, XDEL, XLOWER
      REAL             XMAX, XMID, XMIN, XMIN0, XUPPER, YBAR
      REAL             FDIV, FEXP, FLOG, FSQRT
C
C     ..................................................................
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DC(10)
      DOUBLE PRECISION DB, DCO, DEN, DQ, DZ, DZ2, DZ3, DZ4, DZ5
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
      DOUBLE PRECISION DPCA
C
C     ..................................................................
C
      CHARACTER LHEAD*1
      CHARACTER M*1, MT*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10) /
     1      ',',  ' ',  'S',  'L',  'O',  'P',  'E',  ' ',  '=',   ' ' /
C
      DATA DCO    /  0.918938533204672741D+0 /
C
      DATA DC( 1) /  0.833333333333333333D-1 /
      DATA DC( 2) / -0.277777777777777778D-2 /
      DATA DC( 3) /  0.793650793650793651D-3 /
      DATA DC( 4) / -0.595238095238095238D-3 /
      DATA DC( 5) /  0.841750841750841751D-3 /
      DATA DC( 6) / -0.191752691752691753D-2 /
      DATA DC( 7) /  0.641025641025641025D-2 /
      DATA DC( 8) / -0.295506535947712418D-1 /
      DATA DC( 9) /  0.179644372368830573D+0 /
      DATA DC(10) / -0.139243221690590111D+1 /
C
      DATA I30K   / 30000 /
      DATA TENMIL / 10000000.0 /
      DATA SMALL  / 0.0000001 /
C
      DATA DPCA / 10.0D0 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      AN     = N
      RGAMMA = GAMMA
      U      = RZERO
      XMID   = RZERO
      XLOWER = RZERO
      XUPPER = RZERO
      IF (GAMMA.GT.RZERO) GO TO 10
      CALL ERROR (38)
C
C     COMPUTE THE GAMMA FUNCTION USING THE ALGORITHM IN THE
C        NBS APPLIED MATHEMATICS SERIES REFERENCE.
C
C     THIS GAMMA FUNCTION NEED BE CALCULATED ONLY ONCE.
C
C        IT IS USED IN THE CALCULATION OF THE CDF BASED ON
C        THE TENTATIVE VALUE OF THE PPF IN THE ITERATION.
C
  10  DZ = GAMMA
      DEN = DONE
  20  IF (DZ.GE.DPCA) GO TO 30
      DEN = DEN * DZ
      DZ = DZ + DONE
      GO TO 20
  30  DZ2 = DZ * DZ
      DZ3 = DZ * DZ2
      DZ4 = DZ2 * DZ2
      DZ5 = DZ2 * DZ3
      DQ = (DZ-DHALF) * FDLOG (DZ) - DZ + DCO
      DB = FDDIV (DC(1),DZ,IND) + FDDIV (DC(2),DZ3,IND) +
     1     FDDIV (DC(3),DZ5,IND) + FDDIV (DC(4),DZ2*DZ5,IND) +
     2     FDDIV (DC(5),DZ4*DZ5,IND) + FDDIV (DC(6),DZ*DZ5*DZ5,IND) +
     3     FDDIV (DC(7),DZ3*DZ5*DZ5,IND)  +
     4     FDDIV (DC(8),DZ5*DZ5*DZ5,IND) +
     5     FDDIV (DC(9),DZ2*DZ5*DZ5*DZ5,IND)
      G = FDDIV (FDEXP(DQ+DB),DEN,IND)
C
C     SORT THE DATA.
C
      CALL SORTPP (X,N,Y)
C
C     GENERATE UNIFORM ORDER STATISTC MEDIANS.
C
      CALL UNIMED (N,W)
C
C     GENERATE GAMMA ORDER STATISTIC MEDIANS.
C
C     DETERMINE LOWER AND UPPER BOUNDS ON THE DESIRED I-TH GAMMA
C        ORDER STATISTIC MEDIAN.
C     FOR EACH I, A LOWER BOUND IS GIVEN BY
C        (Y(I)*GAMMA*THE GAMMA FUNCTION OF GAMMA)**(1.0/GAMMA)
C        WHERE Y(I) IS THE CORRESPONDING UNIFORM (0,1) ORDER STATISIC
C        MEDIAN.
C     FOR EACH I EXCEPT I = N, AN UPPER BOUND IS GIVEN BY THE
C        (I+1)-ST GAMMA ORDER STATISTIC MEDIAN (ASSUMEDLY ALREADY
C        CALCULATED).
C     FOR I = N, AN UPPER BOUND IS DETERMINED BY COMPUTING
C        MULTIPLES OF THE LOWER BOUND FOR I = N UNTIL A LARGER
C        VALUE IS OBTAINED.
C     DUE TO THE ABOVE CONSIDERATIONS, THE GAMMA ORDER STATISTIC
C        MEDIANS WILL BE CALCULATED LARGEST TO SMALLEST, THAT IS,
C        IN THE FOLLOWING SEQUENCE:  W(N), W(N-1), ..., W(2), W(1).
C     NOTE ALSO THAT THE VECTOR W WILL AT VARIOUS TIMES
C        IN THE PROGRAM HAVE UNIFORM ORDER STATISTIC MEDIANS AND
C        THEN LATER GRADUALLY FILL UP WITH GAMMA ORDER STATISTIC
C        MEDIANS.
C
      I = N
      ITAIL = IZERO
  40  IF (ITAIL.EQ.IZERO) U = W(I)
      RP = U
      XMIN0 = (U*GAMMA*G) ** FDIV (RONE,GAMMA,IND)
      XMIN = XMIN0
      IF (I.EQ.N .OR. ITAIL.GE.IONE) GO TO 50
      IP1 = I + IONE
      XMAX = W(IP1)
      GO TO 80
  50  ILOOP = IONE
      ICOUNT = IONE
  60  ACOUNT = ICOUNT
      XMAX = ACOUNT * XMIN0
      RX = XMAX
      GO TO 240
  70  IF (PCALC.GE.RP) GO TO 80
      XMIN = XMAX
      ICOUNT = ICOUNT + IONE
      IF (ICOUNT.LE.I30K) GO TO 60
  80  XMID = FDIV (XMIN+XMAX,RTWO,IND)
C
C     AT THIS STAGE WE NOW HAVE LOWER AND UPPER LIMITS ON
C        THE DESIRED I-TH GAMMA ORDER STATISITC MEDIAN W(I).
C
C     NOW ITERATE BY BISECTION UNTIL THE DESIRED ACCURACY IS ACHIEVED
C        FOR THE I-TH GAMMA ORDER STATISITIC MEDIAN.
C
      ILOOP  = ITWO
      XLOWER = XMIN
      XUPPER = XMAX
      ICOUNT = IZERO
  90  RX = XMID
      GO TO 240
 100  IF (PCALC.EQ.RP) GO TO 130
      IF (PCALC.GT.RP) GO TO 110
      XLOWER = XMID
      XMID = FDIV (XMID+XUPPER,RTWO,IND)
      GO TO 120
 110  XUPPER = XMID
      XMID = FDIV (XMID+XLOWER,RTWO,IND)
 120  XDEL = ABS (XMID-XLOWER)
      ICOUNT = ICOUNT + IONE
      IF (XDEL.LT.SMALL .OR. ICOUNT.GT.IHRD) GO TO 130
      GO TO 90
 130  W(I) = XMID
      IF (I.LE.IONE) GO TO 140
      I = I - IONE
      GO TO 40
 140  CONTINUE
C
C     AT THIS POINT, THE GAMMA ORDER STATISTIC MEDIANS ARE ALL COMPUTED.
C        NOW PLOT OUT THE GAMMA PROBABILITY PLOT
C
      IF (LWIDE.LT.NCW) GO TO 150
      V(1) = GAMMA
      CALL RFORMT (0,ISIGD,V,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,V(1),0,0,NW,ND,MT(1),IRF)
 150  IF (LWIDE.GE.106) WRITE (IPRINT,270) N, (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      IF (LWIDE.GE.NCW .AND. LWIDE.LT.106) WRITE (IPRINT,280)
     1     (LHEAD(I),I=1,12), N, (MT(J),J=1,NW)
      IF (LWIDE.LT.NCW) WRITE (IPRINT,290) (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      CALL PRPLOT (Y,W)
C
C     COMPUTE ESTIMATES OF THE LOCATION AND SCALE PARAMETERS.
C
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      CALL SUMMAL (W,N,SUM2)
      IF (N.EQ.IONE) SUM2 = W(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = FDIV (SUM2,AN,IND)
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 160 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
 160  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 170 I=1,N
        ATEMP(1) = (Y(I)-YBAR) * (W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
 170  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 180 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
 180  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1) * WBAR
C
      CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 190 I=1,10
        MT(K) = M(I)
        K = K + IONE
 190  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+73.GT.LWIDE) GO TO 200
      WRITE (IPRINT,300) CC, (MT(J),J=1,K)
      GO TO 230
 200  CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
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
      IF (K+38.GT.LWIDE) GO TO 220
      WRITE (IPRINT,310) CC, (MT(J),J=1,K)
      GO TO 230
 220  IF (LWIDE.LT.37) GO TO 240
      WRITE (IPRINT,320) CC
 230  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C     THIS SECTION BELOW IS LOGICALLY SEPARATE FROM THE ABOVE.
C        THIS SECTION COMPUTES A CDF VALUE FOR ANY GIVEN TENTATIVE
C        PERCENT POINT X VALUE AS DEFINED IN EITHER OF THE 2
C        ITERATION LOOPS IN THE ABOVE CODE.
C
C     COMPUTE T-SUB-Q AS DEFINED ON PAGE 4 OF THE WILK, GNANADESIKAN,
C        AND HUYETT REFERENCE.
C
 240  SUM  = FDIV (RONE,GAMMA,IND)
      TERM = FDIV (RONE,GAMMA,IND)
      CUT1 = RX - RGAMMA
      CUT2 = RX * TENMIL
      DO 250 J=1,1100
        AJ = J
        TERM = FDIV (RX*TERM,RGAMMA+AJ,IND)
        SUM = SUM + TERM
        CUTOFF = CUT1 + FDIV (CUT2*TERM,SUM,IND)
        IF (AJ.GT.CUTOFF) GO TO 260
 250  CONTINUE
      CALL ERROR (249)
 260  P = SUM
C
C     PCALC MUST BE PROPERLY SCALED TO AVOID OVERFLOW/UNDERFLOW.
C
      PCALC = FEXP (RGAMMA*FLOG(RX)-RX) * FDIV (P,G,IND)
      IF (ILOOP.EQ.IONE) GO TO 70
      GO TO 100
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 270  FORMAT (15X, 5HGAMMA             ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 280  FORMAT (11X, 5HGAMMA             ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 290  FORMAT ( 1X, 5HGAMMA        ,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 300  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 310  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 320  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*GENER
      SUBROUTINE GENER
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  GENER V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE GENERATE INSTRUCTION.
C
C         NARGS MUST BE .GE. 4 AND EVEN
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             FDPCON
C
      DOUBLE PRECISION DA, DELTA, ENDER, S
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / 0.01D0 /
C
C     ==================================================================
C
      IF (NARGS.GE.IFOUR .AND. MOD(NARGS,ITWO).EQ.IZERO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     GET STORAGE COLUMN ADDRESS.
C
  10  CALL ADRESS (NARGS,J)
      IF (J.GT.IZERO) GO TO 20
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  20  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     CONVERT INTEGERS TO FLOATING POINT.
C
      DO 30 I=2,NARGS
        IF (KIND(I-1).EQ.IZERO) ARGS(I-1) = IARGS(I-1)
  30  CONTINUE
C
      DA    = DBLE (ARGS(1))
      RC(J) = FDPCON(DA)
      NDROW = J + NROW - IONE
      DO 60 I=4,NARGS,2
        IF (ARGS(I-3).GT.ARGS(I-1)) ARGS(I-2) = SIGN (ARGS(I-2),-RONE)
        DELTA = DBLE (ARGS(I-2))
        IF (DELTA.EQ.DZERO) CALL ERROR (3)
        IF (NERROR.NE.IZERO) RETURN
        S = DSIGN (DONE,DELTA)
        ENDER = DBLE (ARGS(I-1)) - DPCA * DBLE (ARGS(I-2))
  40    J = J + IONE
        DA = DA + DELTA
        RC(J) = FDPCON(DA)
        IF (S*(DA-ENDER).GE.RZERO) GO TO 50
C
C       NOT DONE.
C
        IF (J.LT.NDROW) GO TO 40
C
C       EXCEEDED COLUMN LENGTH.
C
        CALL ERROR (201)
        GO TO 70
C
C       PASSES GENERATE UPPER BOUND, SET IN UPPER BOUND.
C
  50    DA = DBLE (ARGS(I-1))
        RC(J) = FDPCON (DA)
  60  CONTINUE
C
  70  NROLD = NRMAX
      NRMAX = J - NDROW + NROW
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*GQUAD
      SUBROUTINE GQUAD
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  GQUAD V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE GAUSS QUADRATURE INSTRUCTION.
C
C     TAKEN FROM SYMBOLIC LISTING PAGE 251 (YELLOW COVER) JULY,1965
C
C     DOUBLE PRECISION USED TO AVOID NOISE IN 8TH DIGIT.
C        SLIGHT NOISE MAY BE LEFT DUE TO CONVERSION FROM DP TO SP.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             FDPCON
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION C, B, BMA, BPA, DELGQ, STORE1, STORE2
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / -0.861136311594053D0 /
      DATA DPCB / -0.339981043584856D0 /
      DATA DPCC /  0.347854845137454D0 /
      DATA DPCD /  0.652145154862546D0 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.EQ.IFIVE) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     N MUST BE AN EXACT MULTIPLE OF 4 AND LESS THAN NROW.
C
  10  NGQ = IARGS(1)
      IF (KIND(1).EQ.IONE)  NGQ = ARGS(1)
      IF (MOD(NGQ,IFOUR).EQ.IZERO .AND. NGQ.GT.IZERO) GO TO 30
  20  CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  30  IF (NGQ.GT.NROW) GO TO 20
C
C     RESET NRMAX IF NECESSARY.
C
      NROLD = NRMAX
      NRMAX = MAX0(NGQ,NRMAX)
      IF (NRMAX.NE.NROLD) CALL ERROR (252)
      CALL ADRESS (IFOUR,JPGQ)
      IF (JPGQ.LT.IZERO) CALL ERROR (20)
      JPGQ = JPGQ - IONE
      CALL ADRESS (IFIVE,JWGQ)
      IF (JWGQ.LT.IZERO) CALL ERROR (20)
      JWGQ = JWGQ - IONE
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      C = FLOAT(IARGS(2))
      IF (KIND(2).EQ.IONE)  C = ARGS(2)
      B = FLOAT(IARGS(3))
      IF (KIND(3).EQ.IONE)  B = ARGS(3)
      DELGQ = NGQ
      DELGQ = FDDIV (DFOR*(B-C),DELGQ,IND)
      DO 40 I=1,NGQ,4
        B        = C + DELGQ
        BPA      = FDDIV (B+C,DTWO,IND)
        BMA      = FDDIV (B-C,DTWO,IND)
        K1       = I + JPGQ
        K2       = I + JWGQ
        STORE1   = DPCA * BMA
        STORE2   = DPCB * BMA
        RC(K1)   = FDPCON (STORE1+BPA)
        RC(K1+1) = FDPCON (STORE2+BPA)
        RC(K1+2) = FDPCON (BPA-STORE2)
        RC(K1+3) = FDPCON (BPA-STORE1)
        RC(K2)   = FDPCON (DPCC*BMA)
        RC(K2+1) = FDPCON (DPCD*BMA)
        RC(K2+2) = RC(K2+1)
        RC(K2+3) = RC(K2)
        C        = B
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*GRAPH
      SUBROUTINE GRAPH (XMAXD,XMIND,XAN,YMAXD,YMIND,YAN,INDS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GRAPH V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DETERMINE X AND Y BOUNDERIES SUCH THAT INTERVALS ARE NICE NUMBERS.
C
C     NO INPUT PARAMETERS.  ALL FORMAL ARGUMENTS DEFINED BY GRAPH.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1978.
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
      REAL             XAN, XMAXD, XMIND
      REAL             YAN, YMAXD, YMIND
      REAL             DIST, XMAX, XMAXP, XMIN, XMINP
      REAL             YMAX, YMAXP, YMIN, YMINP
C
C     ==================================================================
C
C     SEARCH FOR MAX AND MIN ON AXIS.
C
      M    = NARGS - IONE
      INDS = IZERO
      K1 = IARGS(NARGS)
      K2 = K1 - IONE + NRMAX
      XMAX = RC(K1)
      XMIN = XMAX
      DO 20 I=K1,K2
        IF (XMAX.GE.RC(I)) GO TO 10
        XMAX = RC(I)
        GO TO 20
  10    IF (XMIN.LE.RC(I)) GO TO 20
        XMIN = RC(I)
  20  CONTINUE
      DO 50 J=1,M
        K3 = IARGS(J)
        K4 = K3 - IONE + NRMAX
        IF (J.GT.IONE) GO TO 30
        YMAX = RC(K3)
        YMIN = YMAX
  30    DO 40 I=K3,K4
          IF (YMAX.LT.RC(I)) YMAX = RC(I)
          IF (YMIN.GT.RC(I)) YMIN = RC(I)
  40    CONTINUE
  50  CONTINUE
C
C     CALL SCALE3 SUBROUTINE TO DETERMINE NICE BOUNDRY VALUES FOR X.
C
      CALL SCALE3 (XMIN,XMAX,NRMAX,XMINP,XMAXP,DIST,IND)
      IF (IND.NE.IZERO) CALL ERROR (241)
      INDS = INDS + IND
C
C     CALL SCALE3 SUBROUTINE TO DETERMINE NICE BOUNDRY VALUES FOR Y.
C
      CALL SCALE3 (YMIN,YMAX,NRMAX,YMINP,YMAXP,DIST,IND)
      IF (IND.NE.IZERO) CALL ERROR (241)
      INDS = INDS + IND
C
      YAN = YMINP
      XAN   = XMINP
      XMAXD = XMAXP
      XMIND = XMINP
      YMAXD = YMAXP
      YMIND = YMINP
      RETURN
C
C     ==================================================================
C
      END
*HARMON
      SUBROUTINE HARMON
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. HARMON V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     HARMONIC ANALYSIS OF COL (C) FOR (N) ORDINATES, PUT COEFFS IN (C)
C
C               WRITTEN BY -
C                      BRADLEY A. PEAVY
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                  ORIGINAL VERSION -    APRIL, 1978.
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
C
      INCLUDE 'WRKSCR.H'
C
      REAL             AA, AB, AC, AD, AE, AF, AG, BA, BB
      REAL             R2PI
      REAL             FDIV, FCOS, FSIN
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 1000 /
C
      DATA SPCA / 1.0E-30 /
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NRMAX.EQ.IZERO) CALL ERROR (9)
      IF (NARGS.EQ.ITHRE) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL ADRESS (NARGS,JA)
      IF (JA.GE.IZERO) GO TO 30
  20  CALL ERROR (20)
  30  CALL ADRESS (IONE,JB)
      IF (JB.LT.IZERO) GO TO 20
      IF  (KIND(2).NE.IZERO) GO TO 20
      N = IARGS(2)
      IF (N.GT.ITWO .AND. N.LE.NRMAX) GO TO 40
      CALL ERROR (3)
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     MOVE Y TO SCRATCH AREA.
C
      LNR = NRMAX
      IF (LNR.GT.ICA) LNR = ICA
      DO 50 I=1,LNR
        A(I) = RC(JB)
        JB = JB + IONE
  50  CONTINUE
C
      R2PI = RTWO * PI
      M  = IDIV (N,ITWO,IND)
      MM = M + JA
      K = ITWO*M
      L = IZERO
      IF (K.NE.N) L = IONE
      AB = N
      AA = FDIV (R2PI,AB,IND)
      RC(JA) = RZERO
      RC(MM) = RZERO
      AC = RONE
      KX = N
      KY = IZERO
      AG = SPCA
      DO 80 I=1,N
        RC(JA) = RC(JA) + A(I)
        RC(MM) = RC(MM) + AC*A(I)
        IF (KY.NE.IZERO) GO TO 70
        J = N - I + IONE
        IF (ABS(A(J)).GT.AG) GO TO 60
        KX = KX - IONE
        GO TO 70
  60    KY = IONE
  70   AC = -RONE*AC
  80  CONTINUE
C
      RC(JA) = FDIV (RC(JA),AB,IND)
      RC(MM) = FDIV (RC(MM),AB,IND)
      J = M + L - IONE
      KA = MM + IONE
      KK = JA +IONE
      DO 100 K=1,J
        BA = A(1)
        BB = RZERO
        AC = K
        AC = AC*AA
        AD =  FCOS(AC)
        AC =  FSIN(AC)
        AE = AC
        AF = AD
        DO 90 I=2,KX
          BA = BA + A(I)*AF
          BB = BB + A(I)*AE
          AG = AE*AD + AF*AC
          AF = AF*AD - AE*AC
          AE = AG
  90    CONTINUE
        RC(KK) = FDIV (RTWO*BA,AB,IND)
        RC(KA) = FDIV (RTWO*BB,AB,IND)
        KA = KA + IONE
        KK = KK + IONE
 100  CONTINUE
      IF (L.EQ.IZERO)  RC(KA) = RZERO
      RETURN
C
C     ==================================================================
C
      END
*HFNPLT
      SUBROUTINE HFNPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. HFNPLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A HALFNORMAL
C              PROBABILITY PLOT.
C              THE PROTOTYPE HALFNORMAL DISTRIBUTION USED HEREIN
C              HAS MEAN = SQRT(2/PI) = 0.79788456
C              AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (2/SQRT(2*PI)) * EXP(-X*X/2).
C              THE PROTOTYPE HALFNORMAL DISTRIBUTION USED HEREIN
C              IS THE DISTRIBUTION OF THE VARIATE X = ABS(Z) WHERE
C              THE VARIATE Z IS NORMALLY DISTRIBUTED
C              WITH MEAN 0 AND STANDARD DEVIATION 1.
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE HALFNORMAL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE HALFNORMAL DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE HALFNORMAL PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 7500.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, NORPPF, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--DANIEL, 'USE OF HALF-NORMAL PLOTS IN
C                 INTERPRETING FACTORIAL TWO-LEVEL EXPERIMENTS',
C                 TECHNOMETRICS, 1959, PAGES 311-341.
C               --FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 53, 59, 81, 83.
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
      REAL             X(*), Y(*), W(*)
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, Q, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FSQRT
C
C     ..................................................................
C
      CHARACTER LHEAD*1
      CHARACTER M*1, MT*1
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
        Q = W(I)
        Q = FDIV (Q+RONE,RTWO,IND)
        CALL NORPPF (Q,W(I),IND1)
        IF (IND1.NE.IZERO) CALL ERROR (249)
  10  CONTINUE
C
      IF (LWIDE.GE.81) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.69 .AND. LWIDE.LT.81) WRITE (IPRINT,110)
     1     (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.69) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      CALL SUMMAL (W,N,SUM2)
      IF (N.EQ.IONE) SUM2 = W(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = FDIV (SUM2,AN,IND)
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 20 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  20  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 30 I=1,N
        ATEMP(1) = (Y(I)-YBAR) * (W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  30  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 40 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
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
 100  FORMAT (15X,11HHALF-NORMAL       ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X,11HHALF-NORMAL       ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X,11HHALF-NORMAL       ,14H PROB PLOT OF ,
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
*HISTGM
      SUBROUTINE HISTGM
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. HISTGM V 7.00  1/15/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     HISTOGRAM FOR MIDPOINTS IN COLUMN (C), FREQUENCIES IN COLUMN (C)
C     NHISTOGRAM (C), (C)
C        L2=9 DOES NOT CALL NEW PAGE OR PRINT BLANK LINE
C          BETWEEN CELLS OR HEADING.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1969.
C                   CURRENT VERSION - JANUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION N(81)
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
      REAL             CONST, RELCUM, RELFR, SUMFR
      REAL             FDIV, FLOG10
      REAL             SPCA, SPCB, SPCC
C
C     ...................................................................
C
      CHARACTER LA*1
      CHARACTER LHEAD*1
      CHARACTER N*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 80 /
      DATA ICB / 13 /
C
      DATA SPCA / 0.01   /
      DATA SPCB / 0.001  /
      DATA SPCC / 0.0001 /
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NARGS.EQ.ITWO .OR. NARGS.EQ.ITHRE) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 20   CALL ADRESS (IONE,J1)
      IF (J1.GE.IZERO) GO TO 30
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 30   CALL ADRESS (ITWO,J2)
      IF (J2.LT.IZERO) CALL ERROR (3)
      IF (NRMAX.GT.IZERO) GO TO 40
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
 40   IF (NARGS.EQ.ITWO) GO TO 50
      CALL ADRESS (ITHRE,J3)
      IF (J3.LT.IZERO) CALL ERROR (20)
 50   IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     START COMPUTING.
C
      ISUBN = IONE
      JF    = J2
      IF (NARGS.EQ.ITHRE) JF = J3
      NWIDE = ITEN * IDIV (LWIDE-39,ITEN,IND)
      NWIDE = MIN0 (ICA,NWIDE)
      NWIDE = MAX0 (ITEN,NWIDE)
C
      IF (L2.EQ.9) GO TO 60
C
C     PRINT TITLE.
C
      CALL PAGE (IFOUR)
      IF (NARGS.EQ. ITWO) CALL HEADS (IARGS(1),ITWO,IZERO,IONE)
      IF (NARGS.EQ.ITHRE) CALL HEADS (IARGS(1),IONE,IZERO,IONE)
      IF (NARGS.EQ. ITWO) WRITE (IPRINT,150) (LHEAD(I),I = 1,24)
      IF (NARGS.EQ.ITHRE) WRITE (IPRINT,160) (LHEAD(I),I = 1,12)
      WRITE (IPRINT,170)
  60  IEND = NWIDE + IONE
C
C     COMPUTE TOTAL FREQUENCY.
C
      LOC1 = J1
      LOC2 = J2
      MAXFR = IZERO
      SUMFR  = RZERO
      IRCSUB = JF
      DO 70 I=1,NRMAX
        LFREQ = RC(IRCSUB) + SPCC
        MAXFR = MAX0 (MAXFR,LFREQ)
        IF (NARGS.EQ.ITWO) A(I) = RC(LOC1)
        IF (NARGS.EQ.ITHRE) A(I) = FDIV (RC(LOC1)+RC(LOC2),RTWO,IND)
        LOC1 = LOC1 + IONE
        LOC2 = LOC2 + IONE
        SUMFR = SUMFR + RC(IRCSUB)
        IRCSUB = IRCSUB + IONE
 70   CONTINUE
      MULTFR = IONE + IDIV (MAXFR-IONE,NWIDE,IND)
      CALL RFORMT (0,ISIGD,A,RC(J1),NRMAX,ICB,NW1,NDEC1,N(ISUBN),IRF)
      NBLANK = ICB - NW1
C
C     PRINT SCALE.
C
      J     = MULTFR
      KSUBN = ISUBN
      DO 110 I=1,IEND
        N(KSUBN) = LA(39)
        K        = MOD (I,ITEN)
        IF (K.NE.IONE) GO TO 100
        N(KSUBN) = LA(1)
        IF (I.EQ.IONE) GO TO 100
        CONST = FLOAT(J) + SPCA
        LEND = FLOG10 (CONST)
        LEND = LEND + IONE
        JJ = J
        DO 80 L=1,LEND
          MODJJ = MOD (JJ,ITEN)
          JJ = IDIV (JJ,ITEN,IND)
          NSUB = KSUBN - L
          N(NSUB) = LA(MODJJ+1)
  80    CONTINUE
        J = J + MULTFR
 100    KSUBN = KSUBN + IONE
 110    CONTINUE
C
      IEND = ITEN * (IDIV(MAXFR,ITEN*MULTFR,IND)+IONE) + IONE
      IEND = MIN0 (IEND,NWIDE+IONE) 
      WRITE (IPRINT,180) (N(I),I=ISUBN,IEND)
C
C     PRINT HISTOGRAM.
C
      LCUMFR = IZERO
      DO 140 I=1,NRMAX
        CALL RFORMT (1,ISIGD,RC,A(I),NBLANK,1,NW1,NDEC1,N(ISUBN),IRF)
        LFREQ  = RC(JF) + SPCB
        LCUMFR = LCUMFR + LFREQ
        RELCUM = FDIV (FLOAT(LCUMFR),SUMFR,IND)
        RELFR  = FDIV (RC(JF),SUMFR,IND)
        ICBST  = ICB
        IF (LFREQ.GT.IZERO) GO TO 120
        WRITE (IPRINT,190) (N(I1),I1=ISUBN,ICBST),RELCUM,LCUMFR,RELFR,
     1     LFREQ
        GO TO 130
 120    I2END = IONE + IDIV (LFREQ-IONE,MULTFR,IND)
        WRITE (IPRINT,190) (N(I1),I1=ISUBN,ICBST),RELCUM,LCUMFR,RELFR,
     1     LFREQ,(LA(40),I2=1,I2END)
 130     JF = JF + IONE
 140  CONTINUE
C
      WRITE (IPRINT,200)
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 150  FORMAT (1X,29HHISTOGRAM WITH MID-POINTS IN ,12A1,17H, FREQUENCIES 
     1IN ,12A1//)
 160  FORMAT (1X,30HHISTOGRAM WITH FREQUENCIES IN ,12A1//)
 170  FORMAT (3X,10HMID-POINTS,4X,3HRCF,4X,2HCF,4X,2HRF,4X,9HFREQUENCY)
 180  FORMAT (38X, 81A1)
 190  FORMAT (1X,13A1,1X,F5.3,1X,I5,1X,F5.3,1X,I4,2X,80A1)
 200  FORMAT (1H /1X,35HRCF = RELATIVE CUMULATIVE FREQUENCY/
     1            2X,25HCF = CUMULATIVE FREQUENCY/
     2            2X,23HRF = RELATIVE FREQUENCY)
C
C     ==================================================================
C
      RETURN
C
C     ==================================================================
C
      END
*IFS
      SUBROUTINE IFS
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/11/82.    IFS V 7.00  5/ 9/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE BRANCHING INSTRUCTIONS, WHICH MUST BE STORED.
C
C     INSTRUCTIONS ARE ...
C                      IFLT,  IFEQ,  IFGT,  IFGE,  IFNE,  IFLE, COMPARE
C     WITH L2 VALUES
C                         9,    10,    11,    12,    13,    14,      15
C     WITH NUMBER OF ARGUMENTS
C                         2,     X,     2,     2,     X,     2,       3
C     WHERE X IS EITHER TWO OR THREE.
C
C
C     IN COMPARE THE TEST IS FOR RELATIVE ERROR AND GOES
C
C          I  ARG1-ARG2  I      I        I
C          I  ---------  I .LT. I  ARG3  I
C          I    ARG2     I      I        I
C
C     IF ARG2 OR ARG1 IS 0., THEN ABSOLUTE ERROR WILL BE COMPUTED
C        USING ABS(ARG2-ARG1) .LT. ARG3 AND INFORMATIVE DIAGNOSTIC
C           MESSAGE WILL BE PRINTED.
C
C     IF IFEQ AND IFNE CONTAIN A THIRD ARGUMENT (TOLERANCE) ABSOLUTE
C     ERROR WILL BE COMPUTED
C               ABS(ARG1-ARG2) .LT. ABS (ARG3)
C     A GIVEN TOLERANCE IS IGNORED ON IFLT, IFLE, IFGT, IFGE
C     EXAMPLES OF HOW COMMANDS READ.
C     IFLT  8.32 LT EVERY ENTRY OF COL 34, CONDITION IS TRUE
C     IFGE EACH ELEM COL 1 .GE. CORRESP. ELEM. COL 5, COND. IS TRUE
C     IFEQ 2. .EQ. 5. CONDITION TRUE (USEFUL WHEN INCREMENTING ARGS. )
C
C     IF CONDITION IS FALSE, NO ACTION IS TAKEN.
C     IF CONDITION IS TRUE, THERE ARE TWO POSSIBILITIES..
C     1.  IF THE TEST COMMAND IS THE LAST ONE IN THE REPEAT LOOP
C         CURRENTLY BEING EXECUTED, THE LOOP IS TERMINATED (DROPPED
C         BACK TO THE NEXT OUTER LEVEL IF MORE THAN ONE LEVEL DEEP).
C     2.  IF THE TEST COMMAND IS NOT THE LAST ONE, ALL THAT HAPPENS IS
C         THAT THE REST OF THE LOOP IS NOT PERFORMED.  THAT IS, IF THE
C         LOOP COUNTER HAS NOT REACHED ITS UPPER  LIMIT, IT IS ADVANCED
C         ONE AND THE LOOP IS BEGUN FROM THE TOP AGAIN.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION II(3), K(3), NNN(7)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
C
      INCLUDE 'WRKSCR.H'
C
      LOGICAL TWOARG
C
      REAL             X(3)
      REAL             T, X1, X2, X3
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (LEVEL.GT.IZERO) GO TO 10
      CALL ERROR (21)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.EQ.ITWO .AND. L2.EQ.15) GO TO 160
      IF (NARGS.EQ.ITWO .AND. L2.NE.15) GO TO 30
      IF (NARGS.EQ.ITHRE) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  IF (L2.EQ.ITEN .OR. L2.EQ.13 .OR. L2.EQ.15) GO TO 30
      CALL ERROR (212)
      NARGS = ITWO
  30  DO 50 JJ=1,NARGS
        I = JJ
        CALL ADRESS (I,II(I))
        IF (II(I).GE.IZERO) GO TO 40
        II(I) = I
        X(I) = ARGS(I)
  40    K(I) = IONE - KIND(I)
  50  CONTINUE
C
      IF (NRMAX.EQ.IZERO.AND.KIND(1)+KIND(2).NE.ITWO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      NNN(4) = IZERO
      NNN(5) = IZERO
      NNN(6) = IZERO
      TWOARG = NARGS.EQ.ITWO
      I1     = II(1)
      I2     = II(2)
      I3     = II(3)
      X1     = X(1)
      X2     = X(2)
      X3     = X(3)
C
C     CHECK TO SEE IF NRMAX = 0, OR IF ALL ARGUMENTS
C     ARE CONSTANTS AND NOT COLUMN NUMBERS.
C     IF THIS IS TRUE, EXECUTE THE FOLLOWING LOOP ONCE.
C
      NTOP = NRMAX
      IF (NRMAX.EQ.IZERO) NTOP = IONE
      NTEMP = KIND(1) * KIND(2)
      IF (NARGS.EQ.ITHRE) NTEMP = NTEMP * KIND(3)
      IF (NTEMP.EQ.IONE) NTOP = IONE
      DO 140 I=1,NTOP
        IF (K(1).EQ.IONE) X1 = RC(I1)
        IF (K(2).EQ.IONE) X2 = RC(I2)
        IF (TWOARG)  IF (X1-X2)  90,100,110
C
C       CHECK EQ,NE WITHIN BOUNDS
C
        T = ABS (X1-X2)
        IF (L2.NE.15) GO TO 70
        IF (X1.NE.RZERO .AND. X2.NE.RZERO) GO TO 60
        CALL ERROR (108)
        GO TO 70
  60    T = ABS (FDIV (T,X2,IND) )
        IF (K(3).EQ.IONE) X3 = RC(I3)
  70    IF (T-ABS(X3)) 80,110,110
  80    NNN(5) = NNN(5) + IONE
        GO TO 120
C
C       CHECK IFS WITHOUT BOUNDS
C
  90    NNN(4) = NNN(4) + IONE
        GO TO 130
 100    NNN(5) = NNN(5) + IONE
        GO TO 130
 110    NNN(6) = NNN(6) + IONE
        GO TO 130
 120    I3 = I3 + K(3)
 130    I1 = I1 + K(1)
        I2 = I2 + K(2)
 140  CONTINUE
C
      NNN(1) = NNN(5) + NNN(6)
      NNN(2) = NNN(4) + NNN(6)
      NNN(7) = NNN(2)
      NNN(3) = NNN(4) + NNN(5)
      IF (NNN(L2-8).NE.IZERO) RETURN
      IF (INDEX(2,LEVEL).GT.INDEX(3,LEVEL)) GO TO 150
C
C     IF-COMMAND NOT AT END OF PERFORM LOOP, ADVANCE LOOP COUNT.
C
      INDEX(2,LEVEL) = INDEX(3,LEVEL) + IONE
      RETURN
C
C     ..................................................................
C
C
C     IF-COMMAND IS AT END OF PERFORM LOOP, TERMINATE LOOP.
C
 150  LEVEL = LEVEL - IONE
      RETURN
C
C     ..................................................................
C
 160  CALL ERROR (10)
      RETURN
C
C     ==================================================================
C
      END
*INTERP
      SUBROUTINE INTERP
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. INTERP V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C      GENERAL FORM OF INSTRUCTION IS ...
C
C      INTERPOLATE X IN COL (C) Y IN COL (C) LENGTH = (N) FOR THE FIRST
C        (N) VALUES OF XP IN COL (C), USE (N) POINTS, STORE IN COL (C)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             C
      REAL             FDIV, FSQRT
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 9.0 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.7) CALL ERROR (10)
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
C
C     CHECK NO. OF POINTS  LESS THAN OR EQUAL TO NRMAX AND POSITIVE
C
      IF (IARGS(3).LT.IZERO .OR. IARGS(4).LT.IZERO) CALL ERROR (3)
      IF (IARGS(3).GT.NROW .OR. IARGS(4).GT.NRMAX) CALL ERROR (3)
C
C     CHECK TO SEE IF WE HAVE MORE THAN TWO ENTRIES IN TABLE
C     COMPUTE COLUMN ADDRESSES
C
      IF (IARGS(3).LT.ITWO) CALL ERROR (3)
      LXY = IARGS(3)
      LXP = IARGS(4)
      IARGS(3) = IARGS(5)
      IARGS(4) = IARGS(7)
      NARGS = IFOUR
      CALL CHKCOL
C
C     CHECK FOR PREVIOUS ERRORS
C
      IF (NERROR.NE.IZERO)  RETURN
C
C     ==================================================================
C
      INDRV = IZERO
      IF (IARGS(6).LE.LXY) GO TO 10
      IARGS(6) = LXY
      INDRV = IONE
C
  10  IF (IARGS(6)**2+ITHRE*IARGS(6)+LXP.LE.NS) GO TO 20
      C = SPCA - RFOR * FLOAT (LXP-NS)
      IARGS(6) = FDIV (-RTHRE+FSQRT(C),RTWO,JND)
      INDRV = ITWO
C
  20  IA1 = IARGS(1)
      IA2 = IARGS(2)
      IA3 = IARGS(3)
      IA4 = ITHRE*IARGS(6) + LXP + IONE
      CALL INTRP (RC(IA1),RC(IA2),LXY,RC(IA3),A(1),LXP,IARGS(6),A(LXP+1)
     1,A(IA4),IND)
C
C     STORE RESULTS
C
      IA3 = IARGS(4)
      DO 30 I=1,LXP
        RC(IA3) = A(I)
        IA3 = IA3 + IONE
  30  CONTINUE
C
      IF (IND.NE.IZERO) CALL ERROR (261)
      IF (INDRV.EQ.IZERO) RETURN
      IF (INDRV.EQ.IONE) CALL ERROR (262)
      IF (INDRV.NE.IONE) CALL ERROR (263)
      RETURN
C
C     ==================================================================
C
      END
*INVERT
      SUBROUTINE INVERT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. INVERT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     MATRIX INVERSION, SOLUTION OF SYSTEM OF LINEAR EQUATIONS
C
C     MINVERT  (C),(C)  SIZE (R),(C) STORE (R),(C)
C     SOLVE    (C),(C)  SIZE (R),(C)  STORE Y VECTOR (C) STORE (C)
C
C     THE LARGEST MATRIX TO BE INVERTED, OR SYSTEM TO BE SOLVED, OF SIZE
C        N X N IS THE ONE FOR WHICH N SATISFIES THE FOLLOWING EQUATION
C           2 * N**2 + 4*N LESS THAN OR EQUAL TO NS,
C              WHERE NS IS THE SIZE OF THE SCRATCH AREA A(.).
C                 FOR NS =13500, N MUST BE LESS THAN 82.
C
C     L2 = 1   INVERT
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ERR(4)
      REAL             SERR
C
C     ==================================================================
C
      IF (NARGS.EQ.6) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) GO TO 110
      IF (IARGS(3).LT.IARGS(4)) GO TO 120
      IF (IARGS(3).GT.IARGS(4)) GO TO 130
  20  NARGS = 8
      KIND(7) = IZERO
      KIND(8) = IZERO
      IF (L2.EQ.IONE) GO TO 40
      IARGS(9) = IARGS(6)
      NARGS = 9
      KIND(9) = IZERO
      CALL ADRESS (NARGS,JE)
      IF (JE.GE.IZERO) GO TO 30
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  30  IARGS(6) = IARGS(5)
      IARGS(5) = IONE
      IARGS(8) = IONE
      GO TO 50
  40  IARGS(8) = IARGS(3)
  50  IARGS(7) = IARGS(3)
      J = ITWO
      CALL MTXCHL (J)
      IF (J.NE.IZERO) GO TO 160
      JA = IARGS(1)
      JB = IARGS(5)
      IF (ITWO*((IARGS(3))**2)+IFOUR*IARGS(3).GT.NS) GO TO 150
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      N = IARGS(3)
      ISUB1  = IONE
      ISUBUL = N * N + ISUB1
      ISUBB  = N * N + ISUBUL
      ISUBR  = N + ISUBB
      ISUBDX = N + ISUBR
      ISUBPS = N + ISUBDX
      CALL INVCHK (RC(JA),NROW,A(ISUB1),A(ISUBUL),N,A(ISUBB),A(ISUBR),
     1    A(ISUBDX),RC(JB),A(ISUBPS),L2,ERR,IND)
C
C     CHECK TO SEE IF MATRIX WAS INVERTED. YES, IF IND=0.
C
      IF (IND.NE.IZERO) GO TO 180
      IA = IARGS(3)
      IF (L2.EQ.ITWO) GO TO 80
C
C     STORE INVERTED MATRIX.
C
      DO 70 I=1,IA
        JC = JB
        JD = (I-IONE) * N + IONE
        DO 60 J=1,IA
          RC(JC) = A(JD)
          JC = JC + IONE
          JD = JD + IONE
  60    CONTINUE
        JB = JB + NROW
  70  CONTINUE
      GO TO 100
C
C     STORE RESULTS OF SOLUTION.
C
  80  JC = IONE
      DO 90 I=1,IA
        RC(JE) = A(JC)
        JC = JC + IONE
        JE = JE + IONE
  90  CONTINUE
C
C     DETERMINE SMALLEST ERROR BOUND.
C
 100  SERR = AMIN1 (ERR(1),ERR(2),ERR(3))
C
C     PRINT ROW AND COLUMNS DO NOT AGREE,
C         NUMBER OF COLUMNS IS SET EQUAL TO NUMBER OF ROWS.
C
      WRITE (ISCRT,190) SERR
      RETURN
C
C     ..................................................................
C
 110  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 120  IARGS(4) = IARGS(3)
      GO TO 140
 130  IARGS(3) = IARGS(4)
 140  CALL ERROR (210)
      GO TO 20
C
C     PRINT MATRIX TOO LARGE TO INVERT.
C
 150  CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
 160  IF (J.EQ.IONE) GO TO 170
      CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
 170  CALL ERROR (11)
      RETURN
C
C     ..................................................................
C
 180  CALL ERROR (22)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 190  FORMAT (5X, 4(1H+),43H SMALLEST ERROR BOUND ON INVERTED MATRIX IS,
     1    E8.1)
C
C     ==================================================================
C
      END
*ITERAT
      SUBROUTINE ITERAT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ITERAT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 1,        ITERATE
C     L2 = 2,        ISETUP
C     L2 = 3,        ISOLATE
C
C     ITERATE X IN (C), Y IN (C), DESIRED Y IN (C), START STORING (C)
C     ISETUP (C) IN (C), Y IN (C), DESIRED Y IN C STORE STARTING (C)
C     ISOLATE X IN (C) FOR Y IN COL (C) = (K) STORE IN (C) AND (C)
C        ISOLATE X IN (C) FOR Y IN (C)=(K), (N) POINTS, STORE (C), (C)
C
C        ITERATE AND ISETUP USE THREE COLUMNS FOR STORING RESULTS.
C        STORAGE FOR ITERATE AND ISETUP ARE NEW X ,AVERAGE BRACKETING X,
C           AVERAGE BRACKETTING Y, AND SUCCESSFUL Y
C
C               WRITTEN BY -
C                      CARLA G. MESSINA
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                  ORIGINAL VERSION -  OCTOBER, 1967.
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
C
      INCLUDE 'WRKSCR.H'
C
      REAL             DELT, DIV, POINT
      REAL             FDIV
C
C     ==================================================================
C
      INSERT = ITHRE
      POINT  = RZERO
      IF (NARGS.GT.IFOUR) GO TO 40
      IF (NARGS.EQ.IFOUR) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (L2.LE.ITWO) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  NARGS = 7
      DO 30 I=5,7
        KIND(I) = IZERO
        IARGS(I) = IARGS(I-1) + IONE
  30  CONTINUE
      IF (NROW.GE.IFIVE) GO TO 130
      CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
  40  IF (L2.EQ.ITHRE) GO TO 50
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  50  IARGS(7) = IARGS(5)
      KIND(7)  = KIND(5)
      IF (NARGS.EQ.6) GO TO 70
      IF (NARGS.LT.6) GO TO 60
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  60  NARGS    = 6
      IARGS(6) = IARGS(5)
      KIND(6)  = KIND(5)
      IARGS(5) = IARGS(4)
      KIND(5)  = KIND(4)
      GO TO 90
C
  70  IF (KIND(4).EQ.IZERO) GO TO 80
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  80  INSERT = IARGS(4)
      IARGS(4) = IARGS(5)
  90  IF (INSERT.GE.IONE) GO TO 100
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
 100  IF (NROW.GE.INSERT-ITWO) GO TO 110
      CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
 110  IF (KIND(3).NE.IZERO) GO TO 120
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
 120  POINT    = ARGS(3)
      IARGS(3) = IARGS(2)
      KIND(3)  = IZERO
 130  CALL CHKCOL
      IF (NRMAX.EQ.IONE) GO TO 140
      IF (NRMAX.GT.IONE) GO TO 150
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
 140  CALL ERROR (215)
      RETURN
C
C     ..................................................................
C
 150  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (L2.LT.ITWO) GO TO 220
      I1 = IARGS(1) - IONE
      DO 160 I=2,NRMAX
        K = I1 + I
        IF (RC(K-1).LT.RC(K)) GO TO 170
        IF (RC(K-1).GT.RC(K)) GO TO 190
 160  CONTINUE
      GO TO 210
C
 170  DO 180 I=2,NRMAX
        K = I1 + I
        IF (RC(K-1).GT.RC(K)) GO TO 210
 180  CONTINUE
      GO TO 220
C
 190  DO 200 I=2,NRMAX
        K = I1 + I
        IF (RC(K-1).LT.RC(K)) GO TO 210
 200  CONTINUE
      GO TO 220
C
 210  CALL ERROR (216)
 220  DO 230 I=1,NRMAX
        I1   = IARGS(1) + I - IONE
        I2   = IARGS(2) + I - IONE
        I3   = IARGS(3) + I - IONE
        A(I) = RC(I1)
        M    = I + NRMAX
        A(M) = RC(I2)
        M    = M + NRMAX
        A(M) = RC(I3)
 230  CONTINUE
C
      M     = IZERO
      M1    = IZERO
      IOVFL = IZERO
      IND2  = INSERT + ITWO
      DIV   = INSERT + IONE
      NDIV  = INSERT + IONE
      I1 = IARGS(4) - IONE
      I2 = IARGS(5) - IONE
      I3 = IARGS(6) - IONE
      I4 = IARGS(7) - IONE
      I5 = IARGS(4) - IONE
      IF (L2.LE.ITWO) GO TO 380
C
C                                  ISOLATE.
C
      K1 = NRMAX + IONE
      L1 = ITWO * NRMAX
      I5 = IARGS(5) - IONE
      IF (POINT.NE.A(K1)) GO TO 240
      M  = M + IONE
      I2 = I2 + IONE
      RC(I2) = A(1)
      M1 = M1 + IONE
      I3 = I3 + IONE
      RC(I3) = A(1)
 240  K1 = K1 + IONE
      I = IONE
      DO 320 K=K1,L1
        I = I + IONE
        IF (POINT.GT.A(K-1)) GO TO 250
        IF (POINT.EQ.A(K-1)) GO TO 320
        IF (POINT-A(K)) 320,270,260
 250    IF (POINT.EQ.A(K))   GO TO 270
        IF (POINT.GT.A(K))   GO TO 320
 260    IF (NROW-M.GE.IND2) GO TO 280
        IOVFL = IONE
        GO TO 310
C
 270    A(I-1) = A(I)
        IF (NROW-M.GE.IONE) GO TO 300
        IF (NROW-M.LT.IONE) GO TO 310
 280    M = M + IONE
        I2 = I2 + IONE
        RC(I2) = A(I-1)
        DELT = FDIV (A(I)-A(I-1),DIV,IND)
        DO 290 II=1,INSERT
          M = M + IONE
          I2 = I2 + IONE
          RC(I2) = RC(I2-1) + DELT
 290    CONTINUE
C
 300    M = M + IONE
        I2 = I2 + IONE
        RC(I2) = A(I)
 310    M1 = M1 + IONE
        I3 = I3 + IONE
        RC(I3) = FDIV (A(I-1)+A(I),RTWO,IND)
        IF (NROW.LE.M1) GO TO 350
 320  CONTINUE
C
      IF (M.LE.IZERO) GO TO 360
 330  NROLD = NRMAX
      NRMAX = M
      CALL ERROR (252)
 340  IF (IOVFL.EQ.IZERO) RETURN
 350  CALL ERROR (218)
      RETURN
C
C     ..................................................................
C
 360  DO 370 I=1,NRMAX
        I5 = I5 + IONE
        RC(I5) = A(I)
 370  CONTINUE
      CALL ERROR (217)
      RETURN
C
C     ..................................................................
C
 380  K1 = ITWO * NRMAX + IONE
      L1 = ITHRE * NRMAX
      DO 390 K=1,NRMAX
        IF (A(L1).NE.RZERO) GO TO 400
        L1 = L1 - IONE
 390  CONTINUE
C
      L1 = ITHRE * NRMAX
 400  NEWY = L1 - K1 + IONE
      IF (L2.EQ.ITWO) GO TO 410
      IF (L2.LT.ITWO) GO TO 590
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C                                  ISETUP.
C
 410  DO 570 K=K1,L1
        I = IONE
        L = NRMAX + IONE
        IF (A(K).NE.A(I)) GO TO 450
        IF (NROW-M.GE.IND2) GO TO 420
        IOVFL = IONE
        GO TO 440
 420    DO 430 II=1,IND2
          M = M + IONE
          I1 = I1 + IONE
          RC(I1) = A(I)
 430    CONTINUE
 440    M1 = M1 + IONE
        I2 = I2 + IONE
        RC(I2) = A(I)
        I3 = I3 + IONE
        RC(I3) = A(L)
        I4 = I4 + IONE
        RC(I4) = A(K)
        IF (NROW.GT.M1) GO TO 450
        CALL ERROR (218)
        RETURN
C
C     ..................................................................
C
 450    DO 560 I=2,NRMAX
          L = NRMAX + I
          IF (A(L-1).GT.A(K)) GO TO 460
          IF (A(L-1).EQ.A(K)) GO TO 560
          IF (A(L)-A(K)) 560,470,510
 460      IF (A(L).LT.A(K)) GO TO 510
          IF (A(L).GT.A(K)) GO TO 560
 470      IF (NROW-M.GE.IND2) GO TO 480
          IOVFL = IONE
          GO TO 500
 480      DO 490 J=1,IND2
            M = M + IONE
            I1 = I1 + IONE
            RC(I1) = A(I)
 490      CONTINUE
 500      M1 = M1 + IONE
          I2 = I2 + IONE
          RC(I2) = A(I)
          I3 = I3 + IONE
          RC(I3) = A(L)
          GO TO 550
 510      IF (NROW-M.GE.IND2) GO TO 520
          IOVFL = IONE
          GO TO 540
 520      DELT = FDIV (A(I)-A(I-1),DIV,IND)
          M = M + IONE
          I1 = I1 + IONE
          RC(I1) = A(I-1)
          DO 530 J=1,INSERT
            M = M + IONE
            I1 = I1 + IONE
            RC(I1) = RC(I1-1) + DELT
 530      CONTINUE
          M = M + IONE
          I1 = I1 + IONE
          RC(I1) = A(I)
 540      M1 = M1 + IONE
          I2 = I2 + IONE
          RC(I2) = FDIV (A(I)+A(I-1),RTWO,IND)
          I3 = I3 + IONE
          RC(I3) = FDIV (A(L)+A(L-1),RTWO,IND)
 550      I4 = I4 + IONE
          RC(I4) = A(K)
          IF (NROW.GT.M1) GO TO 570
          CALL ERROR (218)
          RETURN
 560    CONTINUE
 570  CONTINUE
 580  IF (M.LE.IZERO) GO TO 360
C
      IF (M.GT.NEWY) GO TO 330
      NROLD = NRMAX
      NRMAX = NEWY
      CALL ERROR (252)
      GO TO 340
C
C                                  ITERATE.
C
 590  II = IND2 * IDIV (NRMAX,IND2,IND)
      IF (II.GT.IZERO) GO TO 600
      CALL ERROR (215)
      RETURN
C
C     ..................................................................
C
 600  DO 740 K=K1,L1
        DO 730 K3=1,II,IND2
          DO 630 J=1,NDIV
            I = K3 + J
            L = NRMAX + I
            IF (A(L-1).LT.A(K)) GO TO 610
            IF (A(L-1).GT.A(K)) GO TO 620
            I = I - IONE
            L = L - IONE
            GO TO 640
 610        IF (A(L)-A(K)) 630,640,680
 620        IF (A(L).EQ.A(K)) GO TO 640
            IF (A(L).LT.A(K)) GO TO 680
 630      CONTINUE
          GO TO 730
 640      IF (NROW-M.GT.IND2) GO TO 650
          IOVFL = IONE
          GO TO 670
 650      DO 660 J=1,IND2
            M = M + IONE
            I1 = I1 + IONE
            RC(I1) = A(I)
 660      CONTINUE
 670      M1 = M1 + IONE
          I2 = I2 + IONE
          RC(I2) = A(I)
          I3 = I3 + IONE
          RC(I3) = A(L)
          GO TO 720
 680      IF (NROW-M.GE.IND2) GO TO 690
          IOVFL = IONE
          GO TO 710
 690      DELT = FDIV (A(I)-A(I-1),DIV,IND)
          M = M + IONE
          I1 = I1 + IONE
          RC(I1) = A(I-1)
          DO 700 J=1,INSERT
            M = M + IONE
            I1 = I1 + IONE
            RC(I1) = RC(I1-1) + DELT
 700      CONTINUE
          M = M + IONE
          I1 = I1 + IONE
          RC(I1) = A(I)
 710      M1 = M1 + IONE
          I2 = I2 + IONE
          RC(I2) = FDIV (A(I)+A(I-1),RTWO,IND)
          I3 = I3 + IONE
          RC(I3) = FDIV (A(L)+A(L-1),RTWO,IND)
 720      I4 = I4 + IONE
          RC(I4) = A(K)
          IF (NROW.GT.M1) GO TO 740
          CALL ERROR (218)
          RETURN
 730    CONTINUE
 740  CONTINUE
      GO TO 580
C
C     ==================================================================
C
      END
*LABEL    
      SUBROUTINE LABEL        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81.  LABEL V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     INSTRUCTIONS ARE ...    
C         
C     I.    LABEL  VAR1,VAR2,...,VARN                         L2=3    
C     II.   LABEL  VAR1,(C),VAR2,(C),...,VARN,(C) 
C     III.  LABEL  VAR1,VAR2,VAR3,(C),VAR4,...,VARN,(C)     
C     IV.   ALABEL VAR1,(R),(C),(M)X(N),VAR2,(R),(C),(M),(N)  L2=4    
C     V.    MLABEL VAR1,(R),(C),(M)X(N),VAR2,(R),(C),(M),(N)  L2=5    
C         
C     IF THE INSTRUCTION IS LABEL, THEN 
C         IA(I)   = LOC OF VARIABLE IN NEWCRD     
C         IA(I+1) = NUMBER OF CHARACTERS PER VARIABLE       
C         IA(I+2) = COLUMN NUMBER ASSIGNED TO VARIABLE.     
C         
C     IF THE INSTRUCTION IS MLABEL OR ALABEL, THEN
C         IA(I)   = LOC OF VARIABLE IN NEWCRD     
C         IA(I+1) = N*100+ NO. OF CHAR/VARIABLE   
C         IA(I+2) = ((C-1)*NROW+R)100000+M        
C         
C      WHERE M = NUMBER OF ROWS         
C            N = NUMBER OF COLUMNS      
C            R = STARTING ROW 
C            C = STARTING COLUMN.       
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - FEBRUARY, 1977.       
C                   CURRENT VERSION -    APRIL, 1992.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IA(100), LMTP(100)       
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
C         
      INCLUDE 'WRKSCR.H'
C         
      EQUIVALENCE (IA(1),A(100))        
C         
C     ==================================================================        
C         
C                 ***   TYPE STATEMENTS   ***
C         
      REAL             SPCA   
C         
C     ...................................................................
C
      CHARACTER LA*1
      CHARACTER NEWCRD*1
      CHARACTER LFMTP*80      
      CHARACTER LMTP*1
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA ICA / 100000 /     
      DATA ICB /     74 /     
C         
      DATA SPCA /  0.05 /     
C         
C     ==================================================================        
C         
C     CHECK FOR NONE ANSI CHARACTERS.   
C     
C     REDEFINE KARD SO UPPER AND LOWER CASE NUMERIC VALUES ARE USED.
C
      DO 20 I = 1,80
        DO 10 J = 1,ICB       
          IF (NEWCRD(I).EQ.LA(J)) GO TO 15        
  10    CONTINUE    
        CALL ERROR (40)       
        RETURN      
  15    KARD(I+2) = J - IONE
  20  CONTINUE      
C         
C     CHECK FOR LABELS.       
C         
      IF (ILABEL.EQ.IZERO) ILABEL = IONE
      ISWT = IZERO  
      DO 40 I=3,83  
        IF (KARD(I).EQ.44 .AND. ISWT.GT.IZERO) GO TO 30     
        IF (KARD(I).EQ.44 .AND. ISWT.LE.IZERO) GO TO 40     
        IF (KARD(I).GT.73 .AND. ISWT.LE.IONE) GO TO 50      
        IF (ISWT.EQ.IONE) GO TO 40      
        IF (ISWT.GT.IONE) GO TO 60      
        ISWT = IONE 
        GO TO 40    
  30    ISWT = ITWO 
  40  CONTINUE      
  50  CALL ERROR (3)
      RETURN        
C         
C     ..................................................................        
C         
C     LOCATE COMMAS AND POSITION OF COMMAS IN ARRAY NEWCRD. 
C     IND = NO. OF COMMAS FOUND.        
C     IA(I), I=1,..,IND POSITION OF COMMA IN NEWCRD.        
C         
  60  CALL NONBLA (I,K)       
      K = KRDPOS - ITWO       
      IA(1) = K - IONE        
      CALL LOCAT (43,IA,IONE,KARD(K+2),82-K,IND,IND1,IA(2))
      IND = IND + IONE        
      IF (IND.EQ.IONE) GO TO 80         
      DO 70 I=2,IND 
        IA(I) = IA(I) + IA(1) 
  70  CONTINUE      
C         
C     LOCATE LAST VARIABLE AND PLACE A COMMA AFTER IT IN ARRAY KARD.  
C         
  80  KRDPOS = IA(IND) + ITHRE
  90  IX = KRDPOS - ITWO      
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,KA)    
      IF (KA.EQ.46) GO TO 110 
 100  KRDPOS = KRDPOS + IONE  
      IF (KARD(KRDPOS).EQ.44) GO TO 90  
      IF (KARD(KRDPOS).LE.74) GO TO 100 
      IX = KRDPOS - ITWO      
 110  IND = IND + IONE        
      IA(IND) = IX  
      KARD(IX+2) = 43         
      INDA = IND - IONE       
      ISWT = IONE   
      ICNUM = IZERO 
      IST = IND + IONE        
      IC = IZERO    
      ISTB = IST - IONE       
 120  IC = IC + IONE
      ID = IA(IC) + IONE      
      CALL NONBLA (ID+ITWO,K) 
      ID = KRDPOS - ITWO      
      IF = IA(IC+1) - IONE    
      IFA = IF      
      DO 130 I=ID,IFA         
        IF (NEWCRD(IF).NE.LA(45)) GO TO 140       
        IF = IF - IONE        
 130  CONTINUE      
C         
C     DETERMINE NO. OF CHARACTERS IN VARIABLE.    
C         
 140  ILEN = IF - ID + IONE   
      IF (ILEN.GT.IZERO .AND. ILEN.LE.12) GO TO 150         
      CALL ERROR (40)         
      RETURN        
C         
C     ..................................................................        
C         
 150  IF (K.GE.ITEN) GO TO 200
C         
C     FIRST CHARACTER OF VARIABLE IS A CONSTANT.  
C     CHECK FOR COLUMN NUMBER OR VARIABLE.        
C         
      CALL AARGS (KARD)       
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,K)     
      IF (KRDPOS-ITWO.LT.IA(IC+1)) GO TO 170      
      IF (IC.GT.IONE) GO TO 180         
C         
C     FIRST DATUM AFTER COMMAND MUST BE A VARIABLE.         
C         
 160  CALL ERROR (39)         
      RETURN        
C         
C     ..................................................................        
C         
C     FIRST CHARACTER OF VARIABLE IS A NUMBER.    
C         
 170  KRDPOS = ID + ITWO      
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,K)     
      GO TO 200     
C         
C     COLUMN NUMBER SPECIFIED FOR PRECEEDING VARIABLE.      
C         
 180  IF (KARG.NE.IZERO) ARG = -RTWO    
      IE = ARG + SIGN (SPCA,ARG)        
C         
C     IF IE LE TO 0, COLUMN NUMBER IS NEGATIVE OR REAL.     
C     THIS IS NOT ALLOWED.    
C         
      IF (IE.LE.IZERO) GO TO 390        
      IF (IA(ISTB).NE.IZERO) GO TO 160  
C         
C     STORE COLUMN NUMBER ASSIGNED TO VARIABLE.   
C         
      IA(ISTB) = IE 
      INDA = INDA - IONE      
      IF (ISWT.EQ.IONE .AND. L2.LE.ITHRE) GO TO 310         
      IF (ISWT.EQ.IONE .AND. L2.GT.ITHRE) GO TO 290         
      ICNUM = ICNUM - IONE    
      IF (ICNUM.GE.IZERO) GO TO 190     
      IF (ICNUM.EQ.(-IONE)) CALL ERROR (212)      
      IA(ISTB) = IZERO        
      GO TO 310     
C         
 190  ISTB = ISTB + ITHRE     
      ISWT = ISWT + IONE      
      GO TO 310     
C         
C         
C     FIRST CHARACTER OF VARIABLE IS ALPHABETIC.  
C         
 200  IF (KARD(KRDPOS+1).NE.38 .OR. L2.NE.ITHRE) GO TO 250  
C         
C     COMMAND IS LABEL AND 2ND CHARACTER IS (-).  
C     IMPLIED LIST OF VARIABLES.        
C         
      KK = KARD(KRDPOS+2)     
      IF (K.LE.KK) GO TO 210  
C         
C     INFORMATIVE DIAGNOSTIC BECAUSE 3RD CHARACTER MUST     
C     BE A CHARACTER GREATER IN ALPABETICAL ORDER.
C         
      KK = K        
      CALL ERROR (253)        
 210  IF (ISWT.NE.ITHRE) GO TO 230      
      DO 220 IJ=ISTB,IST,3    
        IA(IJ) = IA(IJ-3) + IONE        
 220  CONTINUE      
C         
C     IMPLIED VARIABLES. (I.E. S-U).    
C     IA(IST)   = VARIABLE NAME OF ONE CHARACTER. 
C     IA(IST+1) = -1.         
C     IA(IST+2) = 0.
C         
 230  ISTB = IST - IONE       
      ICNUM = IZERO 
      DO 240 II=K,KK
        ICNUM = ICNUM + IONE  
        IA(IST) = II
        IA(IST+1) = -IONE     
        IA(IST+2) = IZERO     
        IST = IST + ITHRE     
        INDA = INDA + IONE    
 240  CONTINUE      
C         
      INDA = INDA - IONE      
      ISWT = ITWO   
      ISTB = ISTB + ITHRE     
      GO TO 310     
C         
C     STORE FOLLOWING INFORMATION IN IA.
C     IA(IST)   = LOCATION OF VARIABLE IN ARRAY NEWCRD.     
C     IA(IST+1) = LENGTH OF VARIABLE.   
C     IA(IST+2) = 0.
C         
 250  IA(IST) = ID  
      IA(IST+1) = ILEN        
      IA(IST+2) = IZERO       
      IF (ISWT.EQ.IONE) GO TO 280       
      IF (ISWT.NE.ITHRE) GO TO 270      
      DO 260 IJ=ISTB,IST,3    
        IA(IJ) = IA(IJ-3) + IONE        
 260  CONTINUE      
C         
 270  ISWT = IONE   
      ICNUM = IZERO 
      ISTB = IST - IONE       
C         
C     INCREMENT IST AND ISTB BY 3.      
C         
 280  IST = IST + ITHRE       
      ISTB = ISTB + ITHRE     
      GO TO 310     
C         
C     ..................................................................        
C         
 290  IC = IC + IONE
      ID = IA(IC) + IONE      
      CALL NONBLA (ID+ITWO,K) 
      IF (K.GE.ITEN) GO TO 400
      CALL AARGS (KARD)       
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,K)     
      IF (KRDPOS-ITWO.LT.IA(IC+1)) GO TO 400      
      IF (KARG.NE.IZERO) GO TO 390      
      IE = ARG + SIGN (SPCA,ARG)        
      IF (IE.LE.IZERO) GO TO 390        
      IA(ISTB) = ((IE-IONE) * NROW + IA(ISTB)) * ICA        
      INDA = INDA - IONE      
      IC = IC + IONE
      ID = IA(IC) + IONE      
      CALL NONBLA (ID+ITWO,K) 
      IF (K.GE.ITEN) GO TO 400
      CALL AARGS (KARD)       
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,K)     
      IE = ARG + SIGN (SPCA,ARG)        
      IF (KARG.NE.IZERO) GO TO 390      
      IA(ISTB) = IA(ISTB) + IE
      ID = KRDPOS - IONE      
      IF (KRDPOS-ITWO.LT.IA(IC+1)) THEN
        IF (K.EQ.33 .OR. K.EQ.71) GO TO 300
        GO TO 400
      ENDIF 
      INDA = INDA - IONE      
      IC = IC + IONE
      ID = IA(IC) + IONE      
 300  CALL NONBLA (ID+ITWO,K) 
      IF (K.GE.ITEN) GO TO 400
      CALL AARGS (KARD)       
      MZZ = KRDPOS  
      CALL NONBLA (MZZ,K)     
      IF (KRDPOS-ITWO.LT.IA(IC+1)) GO TO 400      
      IF (KARG.NE.IZERO) GO TO 390      
      IE = ARG + SIGN (SPCA,ARG)        
      IF (IE.LE.IZERO) GO TO 390        
      IA(ISTB-1) = IA(ISTB-1) + IE * IHRD         
      INDA = INDA - IONE      
C         
C     CHECK TO SEE IF FINISHED WITH ALL VARIABLES.
C         
 310  IF (IC.LT.IND-IONE) GO TO 120     
      IF (ISWT.NE.ITHRE) GO TO 330      
      DO 320 IJ=ISTB,IST,3    
        IA(IJ) = IA(IJ-3) + IONE        
 320  CONTINUE      
 330  IF (NERROR.NE.IZERO) RETURN       
C         
C     ENTER LABELS INTO IHEAD.
C         
      IST = IND + IONE        
      DO 380 I=1,INDA         
        J = IA(IST) + ITWO    
        LOC = IABS ( IA(IST+1) )        
        IL = IA(IST+2)        
        IF (IL.NE.IZERO) GO TO 340      
        IL = ILABEL 
        ILABEL = ILABEL + IONE
        IA(IST+2) = IL        
        IF (IL.LE.NCOL) GO TO 360       
        CALL ERROR (41)       
        RETURN      
 340    IF (IL.LE.NCOL .OR. L2.NE.ITHRE) GO TO 360
        CALL ERROR (11)       
        RETURN      
 350    CALL PREPAK (IONE,IL,-LOC,IA(IST),LMTP,LFMTP,INND)       
        GO TO 370   
 360    IF (IA(IST+1).EQ.(-IONE)) GO TO 350       
        CALL PREPAK (IONE,IL,-LOC,KARD(J),LMTP,LFMTP,INND)       
 370    IF (INND.GT.IZERO) RETURN       
        IST = IST + ITHRE     
 380  CONTINUE      
C         
C     CALL LABPNT TO PRINT MESSAGE OF COLUMN ASSIGNMENT.    
C         
      CALL LABPNT (IND,INDA)  
      RETURN        
C         
C     ..................................................................        
C         
 390  CALL ERROR (20)         
      RETURN        
C         
C     ..................................................................        
C         
 400  CALL ERROR (42)         
      RETURN        
C         
C     ==================================================================        
C         
      END 
*LABPNT
      SUBROUTINE LABPNT (IND,INDA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LABPNT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     WRITE MESSAGE INDICATING WHICH COLUMN NUMBERS ARE ASSIGNED
C     TO VARIABLE NAMES,OR
C     STARTING POSITIONS OF MATRIX (ARRAY) VARIABLE NAMES.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IA(100), IDA(5), IDATA(18), IP(72)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
      EQUIVALENCE (IA(1),A(100))
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER LA*1
      CHARACTER NEWCRD*1
      CHARACTER IDATA*1, IP*1 
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IDATA( 1), IDATA( 2), IDATA( 3), IDATA( 4), IDATA( 5) /
     1           'I',       'S',       ' ',       'I',       'N' /
      DATA IDATA( 6), IDATA( 7), IDATA( 8), IDATA( 9), IDATA(10) /
     1           ' ',       'C',       'O',       'L',       '  '/
      DATA IDATA(11), IDATA(12), IDATA(13), IDATA(14), IDATA(15) /
     1           'R',       'O',       'W',       ' ',       'S' /
      DATA IDATA(16), IDATA(17), IDATA(18) /
     1           'I',       'Z',       'E' /
C
      DATA ICA / 100000 /
C
C     ==================================================================
C
      CALL ERROR (251)
      IPRSWT = IZERO
      ISY    = IONE
      BACKSPACE ISCRT
      DO 10 I=1,70
        IP(I) = LA(45)
  10  CONTINUE
C
      IPP = IONE
      IST = IND + IONE
      DO 220 I=1,INDA
        IDSWT = IONE
        IACNT = MOD (IABS(IA(IST+1)),IHRD)
        IF (IPP+23+IACNT.GT.63) GO TO 200
  20    IF (IPP.EQ.IONE) GO TO 30
        IP(IPP) = LA(44)
        IPP = IPP + ITWO
  30    IP(IPP) = LA(42)
        IPP = IPP + IONE
        IF (IA(IST+1).EQ.(-IONE)) GO TO 50
        KK = IA(IST)
        KKS = KK + IACNT - IONE
        DO 40 II=KK,KKS
          IP(IPP) = NEWCRD(II)
          IPP = IPP + IONE
  40    CONTINUE
        GO TO 60
C
  50    ISTP = IA(IST) + IONE
        IP(IPP) = LA(ISTP)
        IPP = IPP + IONE
  60    IP(IPP) = LA(43)
        IPP = IPP + ITWO
        IF (L2.EQ.ITHRE) GO TO 80
        DO 70 II=1,5
          IP(IPP) = IDATA(II)
          IP(IPP+5) = IDATA(II+9)
          IPP = IPP + IONE
  70    CONTINUE
C
        IPP    = IPP + IFIVE
        IPRSWT = IZERO
        ISZ    = IDIV (IA(IST+2),ICA,INDD)
        ISY    = IDIV (ISZ,NROW,INDD) + IONE
        ISS    = MOD (ISZ,NROW)
        GO TO 100
  80    DO 90 II=1,9
          IP(IPP) = IDATA(II)
          IPP = IPP + IONE
  90    CONTINUE
        IPP = IPP + IONE
        ISS = IA(IST+2)
 100    IP(IPP) = LA(42)
        IPP = IPP + IONE
 110    IDD = IONE
 120    IDA (IDD) = MOD(ISS,ITEN)
        ISS = IDIV (ISS,ITEN**IDD,INDD)
        IDD = IDD + IONE
        IF (ISS.NE.IZERO) GO TO 120
        IDD = IDD - IONE
        IDC = IDD
        DO 130 II=1,IDD
          IDB = IDA(IDC)
          IP(IPP) = LA(IDB+1)
          IPP = IPP + IONE
          IDC = IDC - IONE
 130    CONTINUE
C
        IP(IPP) = LA(43)
        IPP = IPP + IONE
        IF (L2.EQ.ITHRE) GO TO 190
        IPRSWT = IPRSWT + IONE
        GO TO (140,160,180,190), IPRSWT
 140    DO 150 II=6,9
          IP(IPP) = IDATA(II)
          IPP = IPP + IONE
 150    CONTINUE
        IPP = IPP + IONE
        ISS = ISY
        GO TO 100
 160    DO 170 II=14,18
          IP(IPP) = IDATA(II)
          IPP = IPP + IONE
 170    CONTINUE
        IPP = IPP + IONE
        ISS = MOD (IA(IST+2),ICA)
        GO TO 100
 180    IP(IPP-1) = LA(34)
        ISS = IDIV (IA(IST+1),IHRD,INDD)
        GO TO 110
 190    IST = IST + ITHRE
        IF (I.NE.INDA .AND. L2.EQ.ITHRE) GO TO 220
        IDSWT = ITWO
 200    WRITE (ISCRT,230) LA(40),LA(40),LA(45),LA(45), (IP(II),II=1,61)
        WRITE (NPRNT,230) LA(45), LA(45),
     1                     LA(45), LA(45), (IP(II),II=1,61)
        IPP = IONE
        DO 210 II=1,70
          IP(II) = LA(45)
 210    CONTINUE
        IF (IDSWT.EQ.IONE) GO TO 20
 220  CONTINUE
C
      WRITE (ISCRT,230) LA(40), LA(40), LA(45), LA(45), (IP(II),II=1,61)
      WRITE (NPRNT,230) LA(45), LA(45), LA(45),
     1                    LA(45), (IP(II),II=1,27)
      IF (IPP.NE.IONE) WRITE (ISCRT,230) LA(40), LA(40), (LA(45),I=1,65)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 230  FORMAT (2A1,6X,65A1,9X)
C
C     ==================================================================
C
      END
*LAMPLT
      SUBROUTINE LAMPLT (X,Y,W,N,ALAMBA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LAMPLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A (TUKEY) LAMBDA DISTRIBUTION
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = ALAMBA).
C              IN GENERAL, THE PROBABILITY DENSITY FUNCTION
C              FOR THIS DISTRIBUTION IS NOT SIMPLE.
C              THE PERCENT POINT FUNCTION FOR THIS DISTRIBUTION IS
C              G(P) = ((P**ALAMBA)-((1-P)**ALAMBA)) / ALAMBA
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE LAMBDA PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE LAMBDA DISTRIBUTION
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
C                     --ALAMBA = THE SINGLE PRECISION VALUE OF LAMBDA
C                                (THE TAIL LENGTH PARAMETER).
C     OUTPUT--A ONE-PAGE LAMBDA PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     REFERENCES--FILLIBEN, 'TECHNIQUES FOR TAIL LENGTH ANALYSIS',
C                 PROCEEDINGS OF THE EIGHTEENTH CONFERENCE
C                 ON THE DESIGN OF EXPERIMENTS IN ARMY RESEARCH
C                 DEVELOPMENT AND TESTING (ABERDEEN, MARYLAND,
C                 OCTOBER, 1972), PAGES 425-450.
C               --HAHN AND SHAPIRO, STATISTICAL METHODS IN ENGINEERING,
C                 1967, PAGES 260-308.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY, 1969), PAGES 21-44, 229-231,
C                 PAGES 53-58.
C               --HASTINGS, MOSTELLER, TUKEY, AND WINDSOR,
C                 'LOW MOMENTS FOR SMALL SAMPLES:  A COMPARATIVE
C                 STUDY OF ORDER STATISTICS', ANNALS OF
C                 MATHEMATICAL STATISTICS 18 (1947),
C                 PAGES 413-426.
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
      REAL             ALAMBA
      REAL             ATEMP(1), V(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, Q, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
      REAL             SPCA
C
C     ..................................................................
C
      CHARACTER LHEAD*1
      CHARACTER M*1, MT*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10) /
     1      ',',  ' ',  'S',  'L',  'O',  'P',  'E',  ' ',  '=',   ' ' /
C
      DATA SPCA / 0.001 /
C
C     ==================================================================
C
      AN = N
      CALL SORTPP (X,N,Y)
      CALL UNIMED (N,W)
      DO 10 I=1,N
        Q = W(I)
        IF (-SPCA.LT.ALAMBA .AND. ALAMBA.LT.SPCA)
     1        W(I) = FLOG (FDIV(Q,RONE,IND))
        IF (-SPCA.LT.ALAMBA .AND. ALAMBA.LT.SPCA) GO TO 10
        W(I) = FDIV (Q**ALAMBA-(RONE-Q)**ALAMBA,ALAMBA,IND)
  10  CONTINUE
C
      IF (LWIDE.LT.NCW) GO TO 20
      V(1) = ALAMBA
      CALL RFORMT (0,ISIGD,V,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,V(1),0,0,NW,ND,MT(1),IRF)
  20  IF (LWIDE.GE.105) WRITE (IPRINT,110) N, (LHEAD(I),I=1,12),
     1      (MT(J),J=1,NW)
      IF (LWIDE.GE.NCW .AND. LWIDE.LT.105) WRITE (IPRINT,120)
     1      (LHEAD(I),I=1,12), N, (MT(J),J=1,NW)
      IF (LWIDE.LT.NCW) WRITE (IPRINT,130) (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = RZERO
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 30 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  30  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 40 I=1,N
        ATEMP(1) = Y(I) * W(I)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  40  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 50 I=1,N
        ATEMP(1) = W(I)**2
        CALL SUMMAL (ATEMP,-IONE,SUM3)
  50  CONTINUE
C
      CALL SUMMAL (W, IONE,SUM3)
      CC = FDIV (SUM2,FSQRT(SUM3*SUM1),IND)
      YSLOPE(1) = FDIV (SUM2,SUM3,IND)
      YINT(1) = YBAR - YSLOPE(1) * WBAR
      CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 60 I=1,10
        MT(K) = M(I)
        K = K + IONE
  60  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+73.GT.LWIDE) GO TO 70
      WRITE (IPRINT,140) CC, (MT(J),J=1,K)
      GO TO 100
  70  CALL RFORMT (0,ISIGD,YINT,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YINT(1),0,0,NW,ND,MT(1),IRF)
      K = NW + IONE
      DO 80 I=1,10
        MT(K) = M(I)
        K = K + IONE
  80  CONTINUE
C
      CALL RFORMT (0,ISIGD,YSLOPE,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,YSLOPE(1),0,0,NW,ND,MT(K),IRF)
      K = K + NW - IONE
      IF (K+38.GT.LWIDE) GO TO 90
      WRITE (IPRINT,150) CC, (MT(J),J=1,K)
      GO TO 100
  90  IF (LWIDE.LT.37) GO TO 100
      WRITE (IPRINT,160) CC
 100  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 110  FORMAT (15X, 6HLAMBDA            ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 120  FORMAT ( 1X, 6HLAMBDA            ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 130  FORMAT ( 1X, 6HLAMBDA       ,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 140  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 150  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 160  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*LANGUA
      SUBROUTINE LANGUA
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LANGUA V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     LANGUAGES ...
C                     ENGLISH,  FRENCH,   GERMAN,     SPANISH,   ITALIAN
C                   NORWEGIAN,  DANISH, JAPANESE, YUGOSLAVIAN, PORTUGESE
C                       DUTCH, SWEDISH,  SLOVENE,  VOCABULARY.
C
C     NOTE, DIMENSION OF SCRATCH AREA MUST BE AT LEAST 2887,
C         OTHERWISE VALUE OF NSIZE MUST BE CHANGED IN DATA STATEMENT.
C
C     ARRAYS KENG(.) AND KFOR(.)  ARE DIMENSIONED 550X2.
C        IF THE NUMBER OF COMMANDS IN THE VOCABULARY EXCEEDS 550,
C          THESE DIMENSIONS MUST BE CHANGED, THE
C           EQUIVALENCE STATEMENTS MUST BE CHANGED, AND THE VALUES
C           550 AND 2887 MUST BE CHANGED IN THE DATA STATEMENTS.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   MAY, 1973.
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION      NN(12),      MM(25), KAST(100)
      DIMENSION KENG(550,2), KFOR(550,2), KHAY(550)
      DIMENSION IRL1L2(282), IHIERY(282)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /LANGUE/ LANGC, LANGP
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
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
      CHARACTER*1 IASTER, IBLANK, KAST, MM, NN
C
C     .................................................................
C
      EQUIVALENCE ( IBLANK, LA(45)), ( IASTER, LA(41))
C
      EQUIVALENCE (KENG(1,1),A(   1)), (KFOR(1,1),A(1101))
      EQUIVALENCE (  KHAY(1),A(2201))
      EQUIVALENCE (IHIERY(1),A(   1))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NMEMLT / 100000 /
      DATA MAXCOM /    550 /
      DATA L2VOCB /     14 /
      DATA NSIZE  /   2887 /
C
      DATA ICA /   60 /
      DATA ICB /   25 /
      DATA ICC /   50 /
      DATA ICD /   24 /
      DATA ICE /   12 /
      DATA ICF /   13 /
C
      DATA IRL1L2(  1) / 1804 /
      DATA IRL1L2(  2) / 1810 /
      DATA IRL1L2(  3) / 1231 /
      DATA IRL1L2(  4) / 1231 /
      DATA IRL1L2(  5) / 1106 /
      DATA IRL1L2(  6) / 1809 /
      DATA IRL1L2(  7) / 1206 /
      DATA IRL1L2(  8) / 1214 /
      DATA IRL1L2(  9) / 1228 /
      DATA IRL1L2( 10) / 1208 /
      DATA IRL1L2( 11) / 1216 /
      DATA IRL1L2( 12) / 1230 /
      DATA IRL1L2( 13) / 1101 /
      DATA IRL1L2( 14) / 1501 /
      DATA IRL1L2( 15) / 1807 /
      DATA IRL1L2( 16) / 1807 /
      DATA IRL1L2( 17) / 1502 /
      DATA IRL1L2( 18) / 1604 /
      DATA IRL1L2( 19) / 2306 /
      DATA IRL1L2( 20) / 1806 /
      DATA IRL1L2( 21) / 1806 /
      DATA IRL1L2( 22) / 1222 /
      DATA IRL1L2( 23) / 2702 /
      DATA IRL1L2( 24) / 1808 /
      DATA IRL1L2( 25) / 1205 /
      DATA IRL1L2( 26) / 1213 /
      DATA IRL1L2( 27) / 1227 /
      DATA IRL1L2( 28) / 1805 /
      DATA IRL1L2( 29) / 1805 /
      DATA IRL1L2( 30) / 1207 /
      DATA IRL1L2( 31) / 1215 /
      DATA IRL1L2( 32) / 1229 /
      DATA IRL1L2( 33) / 3103 /
      DATA IRL1L2( 34) / 1803 /
      DATA IRL1L2( 35) / 1711 /
      DATA IRL1L2( 36) / 1502 /
      DATA IRL1L2( 37) / 1401 /
      DATA IRL1L2( 38) / 3038 /
      DATA IRL1L2( 39) / 3032 /
      DATA IRL1L2( 40) / 3039 /
      DATA IRL1L2( 41) / 2206 /
      DATA IRL1L2( 42) / 3006 /
      DATA IRL1L2( 43) / 3005 /
      DATA IRL1L2( 44) / 3002 /
      DATA IRL1L2( 45) / 3001 /
      DATA IRL1L2( 46) / 3008 /
      DATA IRL1L2( 47) / 3007 /
      DATA IRL1L2( 48) / 3109 /
      DATA IRL1L2( 49) / 1417 /
      DATA IRL1L2( 50) / 3004 /
      DATA IRL1L2( 51) / 3003 /
      DATA IRL1L2( 52) / 3201 /
      DATA IRL1L2( 53) / 3204 /
      DATA IRL1L2( 54) / 3026 /
      DATA IRL1L2( 55) / 3025 /
      DATA IRL1L2( 56) / 3028 /
      DATA IRL1L2( 57) / 3027 /
      DATA IRL1L2( 58) / 2503 /
      DATA IRL1L2( 59) / 2119 /
      DATA IRL1L2( 60) / 1310 /
      DATA IRL1L2( 61) / 2113 /
      DATA IRL1L2( 62) / 1708 /
      DATA IRL1L2( 63) / 3022 /
      DATA IRL1L2( 64) / 3021 /
      DATA IRL1L2( 65) / 3024 /
      DATA IRL1L2( 66) / 3023 /
      DATA IRL1L2( 67) / 3203 /
      DATA IRL1L2( 68) / 1112 /
      DATA IRL1L2( 69) / 1415 /
      DATA IRL1L2( 70) / 1506 /
      DATA IRL1L2( 71) / 2415 /
      DATA IRL1L2( 72) / 2411 /
      DATA IRL1L2( 73) / 1202 /
      DATA IRL1L2( 74) / 1210 /
      DATA IRL1L2( 75) / 1224 /
      DATA IRL1L2( 76) / 2121 /
      DATA IRL1L2( 77) / 1204 /
      DATA IRL1L2( 78) / 1212 /
      DATA IRL1L2( 79) / 1226 /
      DATA IRL1L2( 80) / 2302 /
      DATA IRL1L2( 81) / 1315 /
      DATA IRL1L2( 82) / 3206 /
      DATA IRL1L2( 83) / 3205 /
      DATA IRL1L2( 84) / 1423 /
      DATA IRL1L2( 85) / 3202 /
      DATA IRL1L2( 86) / 3101 /
      DATA IRL1L2( 87) / 1416 /
      DATA IRL1L2( 88) / 2103 /
      DATA IRL1L2( 89) / 1707 /
      DATA IRL1L2( 90) / 2311 /
      DATA IRL1L2( 91) / 1507 /
      DATA IRL1L2( 92) / 1107 /
      DATA IRL1L2( 93) / 2312 /
      DATA IRL1L2( 94) / 2312 /
      DATA IRL1L2( 95) / 1104 /
      DATA IRL1L2( 96) / 1108 /
      DATA IRL1L2( 97) / 1104 /
      DATA IRL1L2( 98) / 2305 /
      DATA IRL1L2( 99) / 2127 /
      DATA IRL1L2(100) / 3105 /
      DATA IRL1L2(101) / 2122 /
      DATA IRL1L2(102) / 2110 /
      DATA IRL1L2(103) / 2118 /
      DATA IRL1L2(104) / 1606 /
      DATA IRL1L2(105) / 2111 /
      DATA IRL1L2(106) / 1403 /
      DATA IRL1L2(107) / 3010 /
      DATA IRL1L2(108) / 3009 /
      DATA IRL1L2(109) / 3012 /
      DATA IRL1L2(110) / 3011 /
      DATA IRL1L2(111) / 1218 /
      DATA IRL1L2(112) / 2304 /
      DATA IRL1L2(113) / 2126 /
      DATA IRL1L2(114) / 2203 /
      DATA IRL1L2(115) / 1303 /
      DATA IRL1L2(116) / 1312 /
      DATA IRL1L2(117) / 2112 /
      DATA IRL1L2(118) / 1304 /
      DATA IRL1L2(119) / 1318 /
      DATA IRL1L2(120) / 1233 /
      DATA IRL1L2(121) / 2410 /
      DATA IRL1L2(122) / 3102 /
      DATA IRL1L2(123) / 1418 /
      DATA IRL1L2(124) / 1508 /
      DATA IRL1L2(125) / 1301 /
      DATA IRL1L2(126) / 3037 /
      DATA IRL1L2(127) / 2125 /
      DATA IRL1L2(128) / 1903 /
      DATA IRL1L2(129) / 2114 /
      DATA IRL1L2(130) / 2408 /
      DATA IRL1L2(131) / 2124 /
      DATA IRL1L2(132) / 1410 /
      DATA IRL1L2(133) / 1412 /
      DATA IRL1L2(134) / 1411 /
      DATA IRL1L2(135) / 1414 /
      DATA IRL1L2(136) / 1409 /
      DATA IRL1L2(137) / 1413 /
      DATA IRL1L2(138) / 1406 /
      DATA IRL1L2(139) / 2903 /
      DATA IRL1L2(140) / 1232 /
      DATA IRL1L2(141) / 1404 /
      DATA IRL1L2(142) / 2504 /
      DATA IRL1L2(143) / 3029 /
      DATA IRL1L2(144) / 1601 /
      DATA IRL1L2(145) / 2802 /
      DATA IRL1L2(146) / 2803 /
      DATA IRL1L2(147) / 2801 /
      DATA IRL1L2(148) / 3014 /
      DATA IRL1L2(149) / 3013 /
      DATA IRL1L2(150) / 3016 /
      DATA IRL1L2(151) / 3015 /
      DATA IRL1L2(152) / 3018 /
      DATA IRL1L2(153) / 3017 /
      DATA IRL1L2(154) / 3020 /
      DATA IRL1L2(155) / 3019 /
      DATA IRL1L2(156) / 1603 /
      DATA IRL1L2(157) / 1902 /
      DATA IRL1L2(158) / 2207 /
      DATA IRL1L2(159) / 1905 /
      DATA IRL1L2(160) / 1420 /
      DATA IRL1L2(161) / 2115 /
      DATA IRL1L2(162) / 1422 /
      DATA IRL1L2(163) / 1220 /
      DATA IRL1L2(164) / 1220 /
      DATA IRL1L2(165) / 1221 /
      DATA IRL1L2(166) / 1801 /
      DATA IRL1L2(167) / 2505 /
      DATA IRL1L2(168) / 2105 /
      DATA IRL1L2(169) / 2105 /
      DATA IRL1L2(170) / 2904 /
      DATA IRL1L2(171) / 1501 /
      DATA IRL1L2(172) / 1504 /
      DATA IRL1L2(173) / 1714 /
      DATA IRL1L2(174) / 1705 /
      DATA IRL1L2(175) / 1502 /
      DATA IRL1L2(176) / 1503 /
      DATA IRL1L2(177) / 2106 /
      DATA IRL1L2(178) / 2106 /
      DATA IRL1L2(179) / 1601 /
      DATA IRL1L2(180) / 1703 /
      DATA IRL1L2(181) / 1605 /
      DATA IRL1L2(182) / 2603 /
      DATA IRL1L2(183) / 2306 /
      DATA IRL1L2(184) / 1701 /
      DATA IRL1L2(185) / 1701 /
      DATA IRL1L2(186) / 3104 /
      DATA IRL1L2(187) / 2205 /
      DATA IRL1L2(188) / 2306 /
      DATA IRL1L2(189) / 2701 /
      DATA IRL1L2(190) / 1702 /
      DATA IRL1L2(191) / 1806 /
      DATA IRL1L2(192) / 1802 /
      DATA IRL1L2(193) / 1802 /
      DATA IRL1L2(194) / 1803 /
      DATA IRL1L2(195) / 1704 /
      DATA IRL1L2(196) / 1103 /
      DATA IRL1L2(197) / 1103 /
      DATA IRL1L2(198) / 2601 /
      DATA IRL1L2(199) / 2602 /
      DATA IRL1L2(200) / 1502 /
      DATA IRL1L2(201) / 1316 /
      DATA IRL1L2(202) / 2123 /
      DATA IRL1L2(203) / 1219 /
      DATA IRL1L2(204) / 2409 /
      DATA IRL1L2(205) / 1901 /
      DATA IRL1L2(206) / 1307 /
      DATA IRL1L2(207) / 3514 /
      DATA IRL1L2(208) / 2117 /
      DATA IRL1L2(209) / 1706 /
      DATA IRL1L2(210) / 2413 /
      DATA IRL1L2(211) / 2109 /
      DATA IRL1L2(212) / 2002 /
      DATA IRL1L2(213) / 2001 /
      DATA IRL1L2(214) / 3108 /
      DATA IRL1L2(215) / 1715 /
      DATA IRL1L2(216) / 1403 /
      DATA IRL1L2(217) / 3107 /
      DATA IRL1L2(218) / 3106 /
      DATA IRL1L2(219) / 1305 /
      DATA IRL1L2(220) / 2201 /
      DATA IRL1L2(221) / 2102 /
      DATA IRL1L2(222) / 2310 /
      DATA IRL1L2(223) / 1716 /
      DATA IRL1L2(224) / 1105 /
      DATA IRL1L2(225) / 1713 /
      DATA IRL1L2(226) / 2403 /
      DATA IRL1L2(227) / 1235 /
      DATA IRL1L2(228) / 1111 /
      DATA IRL1L2(229) / 1421 /
      DATA IRL1L2(230) / 1403 /
      DATA IRL1L2(231) / 1710 /
      DATA IRL1L2(232) / 1408 /
      DATA IRL1L2(233) / 1709 /
      DATA IRL1L2(234) / 2003 /
      DATA IRL1L2(235) / 1314 /
      DATA IRL1L2(236) / 2101 /
      DATA IRL1L2(237) / 2704 /
      DATA IRL1L2(238) / 1402 /
      DATA IRL1L2(239) / 2412 /
      DATA IRL1L2(240) / 1109 /
      DATA IRL1L2(241) / 1110 /
      DATA IRL1L2(242) / 2502 /
      DATA IRL1L2(243) / 2501 /
      DATA IRL1L2(244) / 2902 /
      DATA IRL1L2(245) / 1302 /
      DATA IRL1L2(246) / 2204 /
      DATA IRL1L2(247) / 2303 /
      DATA IRL1L2(248) / 1311 /
      DATA IRL1L2(249) / 1201 /
      DATA IRL1L2(250) / 1209 /
      DATA IRL1L2(251) / 1223 /
      DATA IRL1L2(252) / 2120 /
      DATA IRL1L2(253) / 2703 /
      DATA IRL1L2(254) / 1602 /
      DATA IRL1L2(255) / 2414 /
      DATA IRL1L2(256) / 2108 /
      DATA IRL1L2(257) / 1309 /
      DATA IRL1L2(258) / 2202 /
      DATA IRL1L2(259) / 1217 /
      DATA IRL1L2(260) / 1234 /
      DATA IRL1L2(261) / 2402 /
      DATA IRL1L2(262) / 2401 /
      DATA IRL1L2(263) / 1505 /
      DATA IRL1L2(264) / 1712 /
      DATA IRL1L2(265) / 2407 /
      DATA IRL1L2(266) / 1102 /
      DATA IRL1L2(267) / 1102 /
      DATA IRL1L2(268) / 2004 /
      DATA IRL1L2(269) / 3500 /
      DATA IRL1L2(270) / 1203 /
      DATA IRL1L2(271) / 1211 /
      DATA IRL1L2(272) / 1225 /
      DATA IRL1L2(273) / 5029 /
      DATA IRL1L2(274) / 1906 /
      DATA IRL1L2(275) / 1419 /
      DATA IRL1L2(276) / 1317 /
      DATA IRL1L2(277) / 2406 /
      DATA IRL1L2(278) / 1904 /
      DATA IRL1L2(279) / 5029 /
      DATA IRL1L2(280) / 2208 /
      DATA IRL1L2(281) / 2209 /
      DATA IRL1L2(282) / 1405 /
C
C     ==================================================================
C
      MXLINE = ICC
      IF (NCRT.NE.IZERO) MXLINE = LENGTH - ITHRE
      DO 10 I=1,NIR
        IR(I,2) = IRL1L2(I)
  10  CONTINUE
C
      IF (L2.NE.L2VOCB) LANGC = L2
C
      GO TO (20,30,40,50,60,70,80,90,100,110,120,130,140,150), L2
C
  20  CALL ENGLSH
      GO TO 570
C
  30  CALL FRENCH
      GO TO 570
C
  40  CALL GERMAN
      GO TO 570
C
  50  CALL SPANSH
      GO TO 570
C
  60  CALL ITALAN
      GO TO 570
C
  70  CALL NORWEG
      GO TO 570
C
  80  CALL DANISH
      GO TO 570
C
  90  CALL JAPANE
      GO TO 570
C
 100  CALL YUGOSL
      GO TO 570
C
 110  CALL PORTUG
      GO TO 570
C
 120  CALL DUTCH
      GO TO 570
C
 130  CALL SWEDSH
      GO TO 570
C
 140  CALL SLOVEN
      GO TO 570
C
C     ==================================================================
C
C     PRINT VOCABULARY.
C
C        STORE ENGLISH  NAME(J) IN KENG(J,I), I=1,2,3,4.
C        STORE LANGUAGE NAME(J) IN KFOR(J,I), I=1,2,3,4.
C        SORT KENG(1,1) AND PRINT.
C
C     ..................................................................
C
C                         ***   ERROR CHECKING   ***
C
 150  IF (NS.GT.NSIZE) GO TO 160
      CALL ERROR (23)
 160  IF (LWIDE.GE.ICA) GO TO 170
      CALL ERROR (245)
      RETURN
C
C     ..................................................................
C
 170  IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     INITIALIZE KENG(.) = KFOR(.) = 0.
C
      DO 190 I=1,MAXCOM
        DO 180 J=1,2
          KENG(I,J) = IZERO
          KFOR(I,J) = IZERO
 180    CONTINUE
 190  CONTINUE
C
      DO 200 I=1,ICB
        MM(I) = IBLANK
 200  CONTINUE
C
C     ..................................................................
C
C     STORE ENGLISH LANGUAGE NAME(J) IN KENG(J,I).
C
      IF (LANGC.NE.IONE) CALL ENGLSH
C
      CALL STRLNG (KENG,MAXCOM,MTOTAL,MM)
C
C     ==================================================================
C
C     CALL LANGUAGE IN EFFECT.
C
      GO TO (340,210,220,230,240,250,260,270,280,290,300,310,320), LANGC
C
 210  CALL FRENCH
      GO TO 330
C
 220  CALL GERMAN
      GO TO 330
C
 230  CALL SPANSH
      GO TO 330
C
 240  CALL ITALAN
      GO TO 330
C
 250  CALL NORWEG
      GO TO 330
C
 260  CALL DANISH
      GO TO 330
C
 270  CALL JAPANE
      GO TO 330
C
 280  CALL YUGOSL
      GO TO 330
C
 290  CALL PORTUG
      GO TO 330
C
 300  CALL DUTCH
      GO TO 330
C
 310  CALL SWEDSH
      GO TO 330
C
 320  CALL SLOVEN
      GO TO 330
C
C     ..................................................................
C
C     STORE FOREIGN LANGUAGE NAME(J) IN KFOR(J,I).
C
 330  CALL STRLNG (KFOR,MAXCOM,NTOTAL,MM)
C
C     ..................................................................
C
C     FIND LANGUAGE.
C
 340  CALL CONVRT (IL(1,1),NN(1))
      CALL CONVRT (IL(LANGC,1),NN(7))
C
C     AUTOMATIC PRINTING OF VOCABULARY.
C
C     INITIALIZE HIEARCHY IN KHAY(.).
C
      DO 350 I=1,MTOTAL
        KHAY(I) = I
 350  CONTINUE
C
C     SORT KENG(I,.) AND CARRY ALONG KFOR(.) AND KHAY(.).
C
      DO 380 I=1,2
        II   = ITHRE - I
        JEND = MTOTAL - IONE
        DO 370 J=1,JEND
          KEND = MTOTAL - J
          DO 360 K=1,KEND
            IF (KENG(K,II).LE.KENG(K+1,II)) GO TO 360
C
              M           = KENG(K,1)
              KENG(K,1)   = KENG(K+1,1)
              KENG(K+1,1) = M
C
              M           = KENG(K,2)
              KENG(K,2)   = KENG(K+1,2)
              KENG(K+1,2) = M
C
              IF (LANGC.EQ.IONE) GO TO 360
C
              M         = KHAY(K)
              KHAY(K)   = KHAY(K+1)
              KHAY(K+1) = M
C
 360      CONTINUE
 370    CONTINUE
 380  CONTINUE
C
C     ..................................................................
C
C     BEGIN PRINTING.
C
      LW     = IFOUR
      LTOTAL = MTOTAL
      IF (LWIDE.LT.LWC)  LW = ITWO
      IF (LANGC.NE.IONE) GO TO 430
      I1 = ITWO * IDIV (MTOTAL,ITWO,IND)
      J = IZERO
      DO 400 I=2,I1,2
        J = J + IONE
        DO 390 I2=1,2
          KFOR(J,I2) = KENG(I,I2)
 390    CONTINUE
 400  CONTINUE
      I1 = IDIV (MTOTAL,ITWO,IND)
      IF (MOD(MTOTAL,ITWO).EQ.IONE) I1 = I1 + IONE
      J = IONE
      DO 420 I=2,I1
        J = J + ITWO
        DO 410 I2=1,2
          KENG(I,I2) = KENG(J,I2)
 410    CONTINUE
 420  CONTINUE
C
      LTOTAL = I1
 430  JJ = IDIV (LTOTAL-IONE,LW,IND) + IONE
      J2 = LW * JJ
      IF (J2.LE.LTOTAL) GO TO 460
      J1 = LTOTAL + IONE
      DO 450 J3=J1,J2
        DO 440 I=1,2
          KENG(J3,I) = IZERO
          KFOR(J3,I) = IZERO
 440    CONTINUE
 450  CONTINUE
C
 460  J = IZERO
      DO 560 I1=1,JJ
        II = MOD(I1,MXLINE) - IONE
        IF (II.NE.IZERO) GO TO 470
        CALL PAGE (IFOUR)
        IF (LW.EQ.IFOUR) WRITE (IPRINT,600) NN,NN,NN,NN
        IF (LW.EQ.ITWO)  WRITE (IPRINT,620) NN,NN
 470    M1 = -ICD
        M2 = -IFOUR
        DO 550 I2=1,LW
          M1 = M1 + ICB
          J  = J + IONE
          JK = KHAY(J)
          IF (LANGC.NE.IONE) KAST(M1) = IASTER
          DO 540 I3=1,2
            M2 = M2 + 6
            M3 = M2 + ICE
            IF (KENG(J,I3).NE.KFOR(JK,I3)) KAST(M1) = IBLANK
            IF (I3.NE.ITWO) GO TO 530
C
C           TITLE, NOTE WITH NUMBERS.
C
            JNAME = IDIV (KENG(J,2),NMEMLT,IND)
            KNAME = IDIV (KFOR(JK,2),NMEMLT,IND)
            IF (JNAME.EQ.IZERO .OR. JNAME.GT.8) GO TO 500
            IF (JNAME.GT.ITWO) GO TO 480
            LL = JNAME + IONE
            KAST(M2-2) = LA(LL)
            GO TO 490
 480        LL = JNAME - IONE
            KAST(M2-1) = LA(LL)
 490        KENG(J,2) = MOD (KENG(J,2),NMEMLT)
 500        IF (KNAME.EQ.IZERO .OR. KNAME.GT.8) GO TO 530
            IF (KNAME.GT.ITWO) GO TO 510
            LL = KNAME + IONE
            KAST(M3-2) = LA(LL)
            GO TO 520
 510        LL = KNAME - IONE
            KAST(M3-1) = LA(LL)
 520        KFOR(JK,2) = MOD (KFOR(JK,2),NMEMLT)
 530        CALL CONVRT (KENG(J,I3),KAST(M2))
            CALL CONVRT (KFOR(JK,I3),KAST(M3))
 540      CONTINUE
C
          IF (KENG(J,1).EQ.IZERO) KAST(M1) = IBLANK
          M2 = M2 + ICF
 550    CONTINUE
C
        IF (LW.EQ.IFOUR) WRITE (IPRINT,610) (KAST(I4),I4=1,100)
        IF (LW.EQ.ITWO)  WRITE (IPRINT,610) (KAST(I5),I5=1, 50)
 560  CONTINUE
C
C     PRINT COLUMN AND NUMBER OF COMMANDS.
C
      IF (LANGC.NE.IONE)
     1  WRITE (IPRINT,650) MM,MTOTAL,(NN(I),I=1,6),NTOTAL,(NN(J),J=7,12)
      IF (LANGC.EQ.IONE)
     1  WRITE (IPRINT,660) MM,MTOTAL,(NN(J),J=1,6)
C
      IF (LW.EQ.ITWO) WRITE (IPRINT,630)
C
C     PRINT FOOTNOTE.
C
      WRITE (IPRINT,640)
C
C     ..................................................................
C
C     SORT IR(.) FOR LOOKUP.
C
 570  DO 580 I=1,NIR
        KHAY(I) = IR(I,2)
 580  CONTINUE
C
      CALL ISORT (IR(1,1),IHIERY,NIR,IONE)
C
      DO 590 I=1,NIR
        M       = IHIERY(I)
        IR(I,2) = KHAY(M)
 590  CONTINUE
      RETURN
C
C     ==================================================================
C
C                          ***   FORMAT STATEMENTS   ***
C
 600  FORMAT (119H LIST OF COMMANDS. ONLY THE FIRST SIX LETTERS ARE PRIN
     1TED.  ASTERISK INDICATES NO TRANSLATION OR COMMANDS ARE THE SAME./
     2         99H    DISTRIBUTION AND PROPERTY COMMANDS ARE PRINTED, BU
     3T NOT ALL COMBINATIONS ARE NECESSARILY VALID./4X,69HEACH COMMAND O
     4F TABLE MAKING TWO-WORD COMMANDS IS PRINTED SEPARATELY.//
     5   4(3X,6A1,8X,6A1,7X)/)
 610  FORMAT (4(2X,7A1,1X,6A1,1X,6A1,1X,6A1))
 620  FORMAT (1X,57HLIST OF COMMANDS. ONLY THE FIRST SIX LETTERS ARE PRI
     1NTED.//2(3X,6A1,8X,6A1,7X)/)
 630  FORMAT (/1X,59HASTERISK INDICATES NO TRANSLATION OR COMMANDS ARE T
     1HE SAME./1X,48HDISTRIBUTION AND PROPERTY NAMES ARE PRINTED, BUT/
     2   1X,43HNOT ALL COMBINATIONS ARE NECESSARILY VALID./1X,69HEACH CO
     3MMAND OF TABLE MAKING TWO-WORD COMMANDS IS PRINTED SEPARATELY.)
 640  FORMAT (/1X,56HA AFTER A COMMAND STANDS FOR A FORMAT OR UNIT QUALI
     1FIER./1X,56HV AFTER A RESET COMMAND STANDS FOR A VARIABLE QUALIFIE
     2R./1X,69HQ AFTER A UNIT COMMAND STANDS FOR BOTH A UNIT AND A FORMA
     3T QUALIFIER.)
 650  FORMAT (2X,25A1,5X,I4,1X,6A1,5H AND ,I4,1X,6A1,9H COMMANDS)
 660  FORMAT (2X,25A1,5X,I4,1X,6A1,9H COMMANDS)
C
C     ==================================================================
C
      END
*LARFIT
      SUBROUTINE LARFIT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LARFIT V 7.00  5/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE L1 OR LEAST ABSOLUTE RESIDUALS (LAR) FIT.
C
C     FORM OF INSTRUCTION IS ...
C
C     LARFIT Y (C) WTS (E) VECS (K) IN (C) ... (C) PUT COEFS (C) RES (C)
C
C     CURRENTLY, WEIGHTS MUST EQUAL 1.0.
C
C     USES BARRODALE-ROBERTS ALGORITHM AND CODE.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             TOLER
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NRMAX.LE.IZERO)   CALL ERROR ( 9)
      IF (KIND(2).NE.IONE)  CALL ERROR (20)
      IF (ARGS(2).NE.RONE)  CALL ERROR ( 3)
      K = IARGS(3)
      IF (NARGS-IFIVE.NE.K) CALL ERROR (10)
      IF (K.GT.NRMAX)       CALL ERROR (22)
      IF (K.LE.IZERO)       CALL ERROR ( 3)
      KIND(2)  = IZERO
      IARGS(2) = IARGS(4)
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     INITIALIZATION.
C
C     M = NUMBER OF MEASUREMENTS.
C     N = NUMBER OF UNKNOWN PARAMETERS.
C
      M     = NRMAX
      MTWO  = M + ITWO
      N     = K
      NTWO  = N + ITWO
      IF (MTWO*NTWO+M+M.GT.NS) CALL ERROR (23)
      IF (NERROR.NE.IZERO) RETURN
      TOLER = RTEN * RER
      ICOEF = IARGS(NARGS-1)
      IRES  = IARGS(NARGS)
C
C     MOVE X AND Y TO SCRATCH AREA.
C
      L = IFOUR
      IASUB = IONE
      DO 20 I=1,K
        IXSUB = IARGS(L)
        DO 10 J=1,NRMAX
          A(IASUB) = RC(IXSUB)
          IASUB = IASUB + IONE
          IXSUB = IXSUB + IONE
  10    CONTINUE
         L = L + IONE
         IASUB = IASUB + ITWO
  20  CONTINUE
C
      IASUB = IASUB + ITWO * MTWO
      ISUBY = IASUB
      K     = IARGS(1)
      DO 30 I=1,NRMAX
        A(IASUB) = RC(K)
        IASUB = IASUB + IONE
        K     = K + IONE
  30  CONTINUE
      IISSUB = IASUB
C
C     PERFORM L1 CALCULATIONS.
C
      CALL BRL1 (M,N,MTWO,NTWO,A,A(ISUBY),TOLER,RC(ICOEF),RC(IRES),
     1           A(IISSUB))
C
C     CHECK ACCURACY OF CALCULATIONS.
C
      K = MTWO * (N+1)
      IF (A(K).EQ.RZERO) CALL ERROR (228)
      IF (A(K).NE.RTWO ) GO TO 40
        CALL ERROR (22)
        RETURN
C
C     REFIT TO OBTAIN ADDITIONAL ACCURACY.
C
  40  IF (N.LE.66) GO TO 50
      L = ICOEF + N
      M1N1 = M + IONE + N * MTWO
      IF (L.LE.NROW) RC(L) = A(M1N1)
      IF (L.GT.NROW) CALL ERROR (226)
      RETURN
C
C     ..................................................................
C
  50  K = MTWO * NTWO - IONE
      NRANK = A(K) + TOLER
      CALL REL1FT (N,NRANK,ICOEF,IRES)
C
      RETURN
C
C     ==================================================================
C
      END
*LGNPLT
      SUBROUTINE LGNPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LGNPLT V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A LOGNORMAL
C              PROBABILITY PLOT.
C              THE PROTOTYPE LOGNORMAL DISTRIBUTION USED HEREIN
C              HAS MEAN = SQRT(E) = 1.64872127
C              AND STANDARD DEVIATION = SQRT(E*(E-1)) = 2.16119742.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/(X*SQRT(2*PI))) * EXP(-FLOG(X)*FLOG(X)/2)
C              THE PROTOTYPE LOGNORMAL DISTRIBUTION USED HEREIN
C              IS THE DISTRIBUTION OF THE VARIATE X = EXP(Z) WHERE
C              THE VARIATE Z IS NORMALLY DISTRIBUTED
C              WITH MEAN 0 AND STANDARD DEVIATION 1.
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE LOGNORMAL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE LOGNORMAL DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE LOGNORMAL PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, NORPPF, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, EXP.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 112-136.
C               --CRAMER, MATHEMATICAL METHODS OF STATISTICS,
C                 1946, PAGES 219-220.
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
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION M(10), MT(50)
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
      REAL             W(*), X(*), Y(*)
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, Q, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FEXP, FSQRT
C
C     ...................................................................
C
      CHARACTER LHEAD*1
      CHARACTER M*1, MT*1
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
        CALL NORPPF (W(I),Q,IND1)
        IF (IND1.NE.IZERO) CALL ERROR (249)
        W(I) = FEXP(Q)
  10  CONTINUE
C
      IF (LWIDE.GE.80) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.68 .AND. LWIDE.LT.80) WRITE (IPRINT,110)
     1     (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.68) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
      CALL PRPLOT (Y,W)
      CALL SUMMAL (Y,N,SUM1)
      IF (N.EQ.IONE) SUM1 = Y(1)
      CALL SUMMAL (W,N,SUM2)
      IF (N.EQ.IONE) SUM2 = W(1)
      YBAR = FDIV (SUM1,AN,IND)
      WBAR = FDIV (SUM2,AN,IND)
      CALL SUMMAL (Y,IZERO,SUM1)
      DO 20 I=1,N
        ATEMP(1) = (Y(I)-YBAR)**2
        CALL SUMMAL (ATEMP,-IONE,SUM1)
  20  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM1)
      CALL SUMMAL (Y,IZERO,SUM2)
      DO 30 I=1,N
        ATEMP(1) = (Y(I)-YBAR) * (W(I)-WBAR)
        CALL SUMMAL (ATEMP,-IONE,SUM2)
  30  CONTINUE
C
      CALL SUMMAL (Y, IONE,SUM2)
      CALL SUMMAL (W,IZERO,SUM3)
      DO 40 I=1,N
        ATEMP(1) = (W(I)-WBAR)**2
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
 100  FORMAT (15X,10HLOG-NORMAL        ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X,10HLOG-NORMAL        ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X,10HLOG-NORMAL        ,14H PROB PLOT OF ,
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
*LOGPLT
      SUBROUTINE LOGPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LOGPLT V 7.00 12/13/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A LOGISTIC
C              PROBABILITY PLOT.
C              THE PROTOTYPE LOGISTIC DISTRIBUTION USED HEREIN
C              HAS MEAN = 0 AND STANDARD DEVIATION = PI/SQRT(3).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(X) / (1+EXP(X)).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE LOGISTIC PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE LOGISTIC DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE LOGISTIC PROBABILITY PLOT.
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
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
C                 DISTRIBUTIONS--2, 1970, PAGES 1-21.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING  
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
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
C
C     ...................................................................
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
        W(I) = FLOG (FDIV(W(I),RONE-W(I),IND))
  10  CONTINUE
C
      IF (LWIDE.GE.78) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.66 .AND. LWIDE.LT.78) WRITE (IPRINT,110)
     1     (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.66) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
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
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  90  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
 100  FORMAT (15X, 8HLOGISTIC          ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X, 8HLOGISTIC          ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X, 8HLOGISTIC          ,14H PROB PLOT OF ,
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
*LSDIAG
      SUBROUTINE LSDIAG (M,N,VAR,ISUBQ,ISUBF,ISUBSY,ISUBRS,MXLINE,LINES)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LSDIAG V 7.00  7/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE AND PRINT DIAGNOSTIC INFORMATION FOR FIT AND POLYFIT.
C
C        1      = STARTING LOCATION IN A(.) OF COEFFICIENTS.
C
C     INPUT ...
C
C        M      = NUMBER OF INDEPENDENT VARIABLES.
C        VAR    = RESIDUAL VARIANCE.
C        ISUBQ  = STARTING LOCATION IN SCRATCH AREA A(.) OF LENGTH NRMAX
C        ISUBF  = STARTING LOCATION IN SCRATCH AREA A(.) OF LENGTH M.
C        ISUBSY = STARTING LOCATION IN A(.) OF S.D. OF PRED. VALUES.
C        ISUBRS = STARTING LOCATION IN A(.) OF RESIDUALS.
C        MXLINE = MAXIMUM NUMBER OF LINES PRINTED PER PAGE.
C        LINES  = NUMBER OF LINES PRINTED.
C
C     OUTPUT ...
C
C        LINES  = NUMBER OF LINES PRINTED.
C                    LINES IS BOTH INPUT AND OUTPUT.
C
C     REFERENCES ...
C
C        DANIEL, CUTHBERT AND WOOD, FRED S. (1971).  FITTING EQUATIONS
C           TO DATA. WILEY-INTERSCIENCE. PAGE 120.
C
C        COOK, DENNIS R. (1977).  DETECTION OF INFLUENTIAL OBSERVATIONS
C           IN LINEAR REGRESSION.  TECHNOMETRICS, 19, 15.
C
C        HOAGLIN, DAVID C. AND WELSCH, ROY E. (1978).  THE HAT MATRIX
C           IN REGRESSION AND ANOVA.  AMSTAT., 32, 12.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  MAY, 1978.
C                   CURRENT VERSION - JULY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IROW(20,5), LINEPR(25)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
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
      REAL             VAR
      REAL             DURWAT(1), STAT(20,5), TEMP(1)
      REAL             AM, H, SD, SUMW, SUMWX, SUMWZZ, SUMZSQ, TSQ
      REAL             X, XX, USWSSD
      REAL             SPCA
      REAL             FDIV, FSQRT
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER        LINEPR*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     IF THE FOLLOWING VALUE IS CHANGED, THE DIMENSIONS OF IROW AND STAT
C       MUST BE CHANGED.
C
      DATA MAXPRT / 20 /
C
C     IF THE FOLLOWING VALUE IS CHANGED, THE DIMENSION OF LINEPR
C        MUST BE CHANGED AND THE FORMAT INVOLVING DURBIN-WATSON
C        STATISTIC MUST ALSO BE CHANGED.
C
      DATA NX / 25 /
C
      DATA SPCA / 100000.0 /
C
C     ==================================================================
C
C     INITIALIZATION.
C
      IWT = IZERO
      JW  = IZERO
      IF (KIND(2).EQ.IONE) GO TO 10
      IWT = IONE
      CALL ADRESS (ITWO,ISUBWT)
      JW = ISUBWT - IONE
      IF (ISUBWT.GE.IZERO) GO TO 10
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  10  IF (NERROR.NE.IZERO) RETURN
      NZW = N
      NUMBER = MAXPRT
      IF (NZW.LT.1000) NUMBER = NUMBER - IFIVE
      IF (NZW.LT.IHRD) NUMBER = NUMBER - IFIVE
      NUMBER = MIN0 (NUMBER,NZW)
      AM = FLOAT (M)
      IF (ISBFT.EQ.IONE) GO TO 15
      LINES = LINES + NUMBER + 11
      IF (LINES.GT.MXLINE) CALL PAGE (IFOUR)
      IF (LINES.GT.MXLINE) LINES = NUMBER + 11
C
C     ==================================================================
C
C     (1)   STANDARDIZED RESIDUALS.
C
C        T(I) = STANDARDIZED RESIDUAL = RESIDUAL / S.D. OF RESIDUAL.
C
 15   CALL VARRES (IWT,VAR,ISUBWT,ISUBSY,ISUBQ)
C
      J = ISUBQ
      K = ISUBRS
      L = ISUBWT - IONE
      DO 40 I= 1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 20
          L = L + IONE
          IF (RC(L).GT.RZERO) GO TO 20
            A(J) = RZERO
            GO TO 30
  20    A(J) = FDIV (ABS(A(K)),FSQRT(A(J)),IND)
        IF (IND.NE.IZERO) A(J) = RPIFY
  30    J    = J   + IONE
        K    = K   + IONE
  40  CONTINUE
C
      CALL SORTLS (A(ISUBQ),NRMAX,NUMBER,STAT(1,1),IROW(1,1),IND)
C
      DO 50 I=1,NUMBER
        K = IROW(I,1) + ISUBRS - IONE
        IF (A(K).LT.RZERO) STAT(I,1) = - STAT(I,1)
  50  CONTINUE
C
C     ..................................................................
C
C     (2)   DIAGONAL OF HAT MATRIX.
C
C        H(I) = VAR(PREDICTED VALUE) / RESIDUAL VARIANCE.
C
      J = ISUBQ
      K = ISUBSY
      L = ISUBWT - IONE
      DO 80 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 60
          L = L + IONE
          IF (RC(L).GT.RZERO) GO TO 60
            A(J) = RZERO
            GO TO 70
  60    A(J) = FDIV (A(K)**2,VAR,IND)
  70    J = J + IONE
        K = K + IONE
  80  CONTINUE
C
      CALL SORTLS (A(ISUBQ),NRMAX,NUMBER,STAT(1,2),IROW(1,2),IND)
C
C     ..................................................................
C
C     (3)   COOK STATISTICS.
C
C        D(I) = ( T(I)**2/M ) * H(I) / (1-H(I))
C
      CALL VARRES (IWT,VAR,ISUBWT,ISUBSY,ISUBQ)
C
      J  = ISUBQ
      K  = ISUBSY
      KK = ISUBRS
      L  = ISUBWT - IONE
      DO 110 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 90
          L = L + IONE
          IF (RC(L).GT.RZERO) GO TO 90
            A(J) = RZERO
            GO TO 100
  90    H    = FDIV (A(K)**2,VAR,IND)
        TSQ  = FDIV (A(KK)**2,A(J),IND)
        A(J) = FDIV (TSQ,AM,IND) * FDIV (H,RONE-H,IND)
        IF (H.GE.(RONE-RTEN*RER)) A(J) = RPIFY
 100    J  = J  + IONE
        K  = K  + IONE
        KK = KK + IONE
 110  CONTINUE
C
      CALL SORTLS (A(ISUBQ),NRMAX,NUMBER,STAT(1,3),IROW(1,3),IND)
C
C     ..................................................................
C
C     (4)   DANIEL-WOOD WSSD STATISTIC.
C
C        WSSD = WEIGHTED SQUARED STANDARDIZED DISTANCE
C             = SUM (B(J)*(X(I,J)-XBAR(J))/S)**2
C
C     COMPUTE SUM OF WEIGHTS.
C
      SUMW = FLOAT (NRMAX)
      IF (IWT.EQ.IONE) CALL SUMMAL (RC(ISUBWT),NRMAX,SUMW)
      IF (NRMAX.EQ.IONE) SUMW = RC(ISUBWT)
C
      IF (L2.EQ.ITHRE) GO TO 210
C
C     COMPUTE WSSD FOR POLYFIT.
C
      IF (M.GT.IONE) GO TO 130
      J = ISUBQ
      DO 120 I=1,NRMAX
        A(J) = RZERO
        J    = J + IONE
 120  CONTINUE
      GO TO 290
C
 130  CALL ADRESS (IFOUR,ISUBX)
C
C     COMPUTE COLUMN AVERAGES.
C
      II = ISUBF
      DO 160 I=2,M
        K = ISUBX
        IF (IWT.EQ.IONE) L = ISUBWT
        CALL SUMMAL (TEMP,IZERO,SUMWX)
        DO 150 J=1,NRMAX
          TEMP(1) = RC(K) ** (I-1)
          IF (IWT.EQ.IZERO) GO TO 140
          TEMP(1) = TEMP(1) * RC(L)
          L = L + IONE
 140      CALL SUMMAL (TEMP,-IONE,SUMWX)
          K = K + IONE
 150    CONTINUE
        CALL SUMMAL (TEMP,IONE,SUMWX)
        A(II) = FDIV (SUMWX,SUMW,IND)
        II = II + IONE
 160  CONTINUE
C
C     COMPUTE WSSD(I).
C
      II = ISUBQ
      K = ISUBX
      DO 200 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 170
        JW = JW + IONE
        IF (RC(JW).GT.RZERO) GO TO 170
        A(II) = RZERO
        GO TO 190
 170    L  = ISUBF
        NN = ITWO
        CALL SUMMAL (TEMP,IZERO,USWSSD)
        X  = RC(K)
        XX = X
        DO 180 J=2,M
          TEMP(1) = A(NN) * (XX-A(L))
          XX      = XX * X
          TEMP(1) = TEMP(1)**2
          CALL SUMMAL (TEMP,-IONE,USWSSD)
          L  = L  + IONE
          NN = NN + IONE
 180    CONTINUE
        CALL SUMMAL (TEMP,IONE,USWSSD)
        A(II) = FDIV (USWSSD,VAR,IND)
 190    K  = K + IONE
        II = II + IONE
 200  CONTINUE
      GO TO 290
C
C     COMPUTE WSSD FOR FIT.
C
C     COMPUTE COLUMN AVERAGES.
C
 210  II = ISUBF
      DO 240 I=1,M
        IF (IWT.EQ.IONE) L = ISUBWT
        NN = I + ITHRE
        CALL ADRESS (NN,K)
        CALL SUMMAL (TEMP,IZERO,SUMWX)
        DO 230 J=1,NRMAX
          TEMP(1) = RC(K)
          IF (IWT.EQ.IZERO) GO TO 220
          TEMP(1) = TEMP(1) * RC(L)
          L = L + IONE
 220      CALL SUMMAL (TEMP,-IONE,SUMWX)
          K = K + IONE
 230    CONTINUE
        CALL SUMMAL (TEMP,IONE,SUMWX)
        A(II) = FDIV (SUMWX,SUMW,IND)
        II = II + IONE
 240  CONTINUE
C
C     COMPUTE WSSD(I).
C
      II = ISUBQ
      SD = FSQRT (VAR)
      DO 280 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 250
        JW = JW + IONE
        IF (RC(JW).GT.RZERO) GO TO 250
        A(II) = RZERO
        GO TO 270
 250    K  = ISUBF
        NN = IONE
        CALL SUMMAL (TEMP,IZERO,USWSSD)
        DO 260 J=1,M
          CALL ADRESS (J+ITHRE,L)
          L = L + I - IONE
          TEMP(1) = FDIV (A(NN)*(RC(L)-A(K)),SD,IND)
          TEMP(1) = TEMP(1)**2
          CALL SUMMAL (TEMP,-IONE,USWSSD)
          K = K + IONE
          NN = NN + IONE
 260    CONTINUE
        CALL SUMMAL (TEMP,IONE,USWSSD)
        IF (M.EQ.IONE) USWSSD = TEMP(1)
        A(II) = USWSSD
 270    II = II + IONE
 280  CONTINUE
C
C     SORT WSSD(.).
C
 290  CALL SORTLS (A(ISUBQ),NRMAX,NUMBER,STAT(1,4),IROW(1,4),IND)
C
C     ..................................................................
C
C     (5)   VARIANCE OF PREDICTED VALUE / VARIANCE OF RESIDUAL.
C
      CALL VARRES (IWT,VAR,ISUBWT,ISUBSY,ISUBQ)
C
      J  = ISUBQ
      K  = ISUBSY
      L  = ISUBWT - IONE
      DO 320 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 300
          L = L + IONE
          IF (RC(L).GT.RZERO) GO TO 300
            A(J) = RZERO
            GO TO 310
 300    IF (A(J).LT.RZERO) A(J) = RZERO
        A(J) = FDIV (A(K)**2,A(J),IND)
        IF (IND.NE.IZERO) A(J) = RPIFY
 310    J  = J  + IONE
        K  = K  + IONE
 320  CONTINUE
C
      CALL SORTLS (A(ISUBQ),NRMAX,NUMBER,STAT(1,5),IROW(1,5),IND)
C
C     ..................................................................
C
C     (6)   DURBIN-WATSON STATISTIC.
C
C        D = SUM (R(I)-R(I-1))**2 / SUM (R(I)**2)
C
C     UNIT WEIGHTS ARE USED, UNLESS WEIGHT IS ZERO.
C
C     MOVE RESIDUALS WITH NON-ZERO WEIGHTS TO A(J).
C
      J = ISUBQ
      K = ISUBRS
      L = ISUBWT - IONE
      DO 350 I=1,NRMAX
        IF (IWT.EQ.IZERO) GO TO 330
        L = L + IONE
        IF (RC(L).LE.RZERO) GO TO 340
 330    A(J) = A(K)
        J = J + IONE
 340    K = K + IONE
 350  CONTINUE
C
C     NUMERATOR.
C
      J = ISUBQ + IONE
      CALL SUMMAL (A,IZERO,SUMWZZ)
      DO 360 I=2,NZW
        TEMP(1) = (A(J)-A(J-1))**2
        CALL SUMMAL (TEMP,-IONE,SUMWZZ)
        J = J + IONE
 360  CONTINUE
      CALL SUMMAL (A,IONE,SUMWZZ)
C
C     DENOMINATOR.
C
      J = ISUBQ
      CALL SUMMAL (A,IZERO,SUMZSQ)
      DO 370 I=1,NZW
        TEMP(1) = A(J)**2
        CALL SUMMAL (TEMP,-IONE,SUMZSQ)
        J = J + IONE
 370  CONTINUE
      CALL SUMMAL (A,IONE,SUMZSQ)
C
      DURWAT(1) = FDIV (SUMWZZ,SUMZSQ,IND)
C
C     ..................................................................
C
C     PRINT RESULTS.
C
      WRITE (IPRINT,460)
      WRITE (IPRINT,400) NUMBER
      WRITE (IPRINT,410) (LA(39),I=1,71)
      WRITE (IPRINT,420)
      WRITE (IPRINT,410) (LA(39),I=1,71)
      IF (ISBFT.EQ.IONE) LINES = LINES + 8
C
      IF (STAT(1,4).GE.SPCA)
     1    CALL RFORMT (0,IFOUR,STAT(1,4),A(1),NUMBER,9,NW,ND,LINEPR,IRF)
      DO 390 I=1,NUMBER
        IF (ISBFT.EQ.IONE) THEN
          LINES = LINES +IONE
          IF (LINES.GE.MXLINE .AND. I.NE.NUMBER) THEN
            CALL PAGE (IFOUR)
            LINES = IFIVE
          ENDIF
        ENDIF
        IF (STAT(I,4).LT.SPCA) GO TO 380
          CALL RFORMT (1,IFOUR,A,STAT(I,4),9-NW,0,NW,ND,LINEPR,IRF)
          WRITE (IPRINT,440) IROW(I,1), STAT(I,1), IROW(I,2), STAT(I,2),
     1                       IROW(I,3), STAT(I,3), IROW(I,4),
     2    (LINEPR(J),J=1,9), IROW(I,5), STAT(I,5)
          GO TO 390
 380    WRITE (IPRINT,430) IROW(I,1), STAT(I,1),
     1 IROW(I,2), STAT(I,2), IROW(I,3), STAT(I,3), IROW(I,4), STAT(I,4),
     2 IROW(I,5), STAT(I,5)
 390  CONTINUE
      WRITE (IPRINT,410) (LA(39),I=1,71)
C
      CALL RFORMT (IZERO,ISIGD,DURWAT,A(1),1,NX,NW,ND,LINEPR,IRF)
      CALL RFORMT (IONE ,ISIGD,A,DURWAT(1),0, 0,NW,ND,LINEPR,IRF)
      WRITE (IPRINT,450) (LINEPR(I),I=1,NW)
      IF (ISBFT.EQ.IONE) LINES = LINES + ITHRE
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 400  FORMAT (4X,64HDIAGNOSTIC INFORMATION FOR IDENTIFYING INFLUENTIAL M
     1EASUREMENTS./1H ,
     2 19H     I  = ROW, FOR ,I2,16H LARGEST VALUES,,1X,
     3 29HT(I) = STANDARDIZED RESIDUAL,/1H ,
     4 33H   H(I) = DIAGONAL OF HAT MATRIX,,5X,
     2 22HD(I) = COOK STATISTIC,/1H ,
     6 32HWSSD(I) = DANIEL-WOOD STATISTIC,,6X,
     7 33HV(I) = VAR(YHAT) / VAR(RESIDUAL).)
 410  FORMAT (1X,119A1)
 420  FORMAT (5X,1HI,3X,4HT(I),6X,1HI,3X,4HH(I),6X,1HI,3X,4HD(I),6X,1HI,
     1   3X,7HWSSD(I),6X,1HI,3X,4HV(I))
 430  FORMAT (1X,I5,2X,F5.2,2X,I5,2X,F5.3,2X,I5,2X,F5.2,2X,I5,1X,F9.2,
     1        2X,I5,1X,F6.2)
 440  FORMAT (1X,I5,2X,F5.2,2X,I5,2X,F5.3,2X,I5,2X,F5.2,2X,I5,1X,9A1,
     1        2X,I5,1X,F6.2)
 450  FORMAT (/   6X,35HTHE DURBIN-WATSON STATISTIC IS D = ,25A1)
 460  FORMAT (/   )
C
C     ==================================================================
C
      END
*LSFIT    
      SUBROUTINE LSFIT (KS,KST,KM,KN,VAR,SSU,SSD,KMTR,KMTC,NI,DIGIT,LSI)        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  LSFIT V 7.00 12/13/89. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     POLYFIT  INSTRUCTIONS.  
C           L2 = 1  POLYFIT        L2 = 2 SPOLYFIT (NO AUTOMATIC PRINT)         
C        POLYFIT  Y IN (C) WTS (E) DEGREE (D) AND X IN (C)  
C          STORE  COEFFS IN (C), RESIDUALS IN (C) SD OF PREDICTED     
C          VALUES IN (C) FOURIER COEFFS IN (C)    
C          VARIANCE COVARIANCE MATRIX IN (R),(C). 
C         
C        SPOLYFIT  Y IN (C) WTS (E) DEGREE (D) AND X IN (C) 
C          STORE  COEFFS IN (C), RESIDUALS IN (C) SD OF PREDICTED     
C          VALUES IN (C) FOURIER COEFFS IN (C)    
C          VARIANCE COVARIANCE MATRIX IN (R),(C). 
C         
C        FIRST FOUR ARGUMENTS MUST BE SPECIFIED FOR POLYFIT.
C        FIRST FOUR ARGUMENTS PLUS AT LEAST ONE STORAGE ARGUMENT MUST 
C          BE SPECIFIED FOR SPOLYFIT.   
C        STORAGE WILL TAKE PLACE FOR THE STORAGE ARGUMENTS PROVIDED.  
C         
C        THE LARGEST VALUE (D) MAY BE IS AS FOLLOWS:       
C          NRMAX(D+5) + (D+1)**2/2 + 6.5*(D+1) + 6 + 0.5    
C             LESS THAN OR EQUAL TO 13500 (NS).   
C         
C     FIT INSTRUCTIONS.       
C           L2 = 3 FIT             L2 = 4 SFIT (NO AUTOMATIC PRINT).  
C        FIT Y IN (C) WTS (E) FOR (K) VARIABLES IN (C), (C),...,(C)   
C          STORE COEFFS IN (C), RESIDUALS IN (C) SD OF PREDICTED      
C          VALUES IN (C) FOURIER COEFFS IN (C)    
C          VARIANCE COVARIANCE MATRIX IN (R),(C). 
C         
C        SFIT Y IN (C) WTS (E) FOR (K) VARIABLES IN (C), (C),..., (C) 
C          STORE COEFFS IN (C), RESIDUALS IN (C), SD OF PREDICTED     
C          VALUES IN (C), FOURIER COEFFS IN (C)   
C          VARIANCE COVARIANCE MATRIX IN (R),(C). 
C         
C        FIRST  K+3 ARGUMENTS MUST BE SPECIFIED FOR FIT.    
C        FIRST  K+3 ARGUMENTS PLUS AT LEAST ONE STORAGE ARGUMENT MUST 
C          BE SPECIFIED FOR SFIT.       
C        STORAGE WILL TAKE PLACE FOR THE STORAGE ARGUMENTS PROVIDED.  
C         
C        TO DETERMINE THE LARGEST VALUE OF K THE FOLLOWING  
C          EQUATION MUST BE SOLVED.     
C          NRMAX*(K+4) + K**2/2 + 6.5*K + 6.5     
C             LESS THAN OR EQUAL TO 13500 (NS).   
C         
C     STORAGE IN SCRATCH AREA A(.).     
C         
C     LOCATION           LENGTH          DESCRIPTION        
C     --------           ------          -----------        
C     A(ISUBB)           K+1             COEFFICIENTS       
C     A(ISUBRS)          N               RESIDUALS
C     A(ISUBR)           (K+1)(K+2)/2    X'W X INVERSE      
C     A(ISUBSB)          K+1             STD. DEV. OF COEFFICIENTS    
C     A(ISUBSY)          N               STD. DEV. OF PREDICTED VALUES
C     A(ISUBS)           K+2             SQUARED FOURIER COEFFICIENTS 
C     A(ISUBQ)           N(K+1)          SCRATCH AREA FOR LSQ         
C     A(ISUBF)           K+1             SCRATCH AREA FOR LSQ         
C     A(ISUBAD)          K               HOLD ACC. DIGITS.  
C     A(ISUBYP)          N               HOLD RESIDUALS IN LSQ        
C         
C     STRUCTURE CHART ...     
C         
C                                 ..........      
C                                 . XSEG12 .      
C                                 ..........      
C                                     . 
C                                     . 
C         .........................................................   
C         .                    .                    .             .   
C         .                    .                    .             .   
C     ..........           ..........           ..........    ..........        
C     . TWOWAY .           . LSFIT  .           . LSTORE .    . LSPRNT .        
C     ..........           ..........           ..........    ..........        
C         .                 .      .                              .   
C         .                 .      .                              .   
C     ..........     ..........  ..........                   ..........        
C     .  LSQ   .     .  LSQ   .  . LSRND  .                   . OTHERS .        
C     ..........     ..........  ..........                   ..........        
C         
C     SEE PROCEDURE LSQ FOR COMPLETE STRUCTURE CHART OF LSQ.
C     SEE PROCEDURE LSPRNT FOR COMPLETE STRUCTURE CHART OF LSPRNT AND 
C       OTHERS.     
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - NOVEMBER, 1977.       
C                   CURRENT VERSION - DECEMBER, 1989.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IIRGS(100)    
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
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
C                         ***   TYPE STATEMENTS   ***   
C         
      REAL             DIGIT, SSD, SSU, VAR       
      REAL             AGREE, FM, RSS, SD, SU, SUMY2W, VARNCE, WC, WSUM         
      REAL             WW, YMAX, YMDRG1, YMDRNG, YMIN       
      REAL             ERR, SPCA, SPCB  
      REAL             FDIV, FDPCON, FSQRT        
C         
      DOUBLE PRECISION DYW(1) 
      DOUBLE PRECISION DSUMY2 
C
C     ...................................................................
C         
      EQUIVALENCE (IIRGS(1),LFMT(1))    
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA ERR /  1.E-6 /     
C         
      DATA SPCA / 6.5 /       
      DATA SPCB / 6.5 /       
C         
C     ==================================================================        
C         
C                         ***   ERROR CHECKING   ***        
C         
      IF (NRMAX.EQ.IZERO) CALL ERROR (9)
      IF (KIND(3).EQ.IONE) IARGS(3) = ARGS(3)     
C         
C     CHECK MAXIMUM SIZE OF SCRATCH AREA NEEDED.  
C         
      N = NRMAX     
C         
C     M = DEG OF POLYNOMIAL +1, IF POLYFIT OR     
C         (K) NUMBER OF VARIABLES, IF FIT.        
C         
      M = IARGS(3)  
      IF (L2.LE.ITWO) M = M + IONE      
      IF (M.LE.IZERO) CALL ERROR (3)    
      ISPACE = IFIX (FLOAT(N*(M+IFOUR)) + FDIV (FLOAT(M**2),RTWO,IND) +         
     1     SPCA * FLOAT(M) + SPCB) + LSI * N * ITWO         
      IF (LSI.EQ.IONE .AND. KIND(2).EQ.IONE) ISPACE = ISPACE + N      
      IF (ISPACE.GT.NS) CALL ERROR (23) 
      JNARGS = NARGS
      MMTXR  = IONE 
      MMTXC  = IONE 
      SU     = RZERO
      IF (L2.GT.ITWO) GO TO 10
C         
C     CHECK ARGUMENTS OF POLYFIT AND SPOLYFIT.    
C         
      IF (NARGS.GT.ITEN) CALL ERROR (10)
      IF (NARGS.EQ.9) CALL ERROR (10)   
      IF (NARGS.EQ.ITEN) JNARGS = JNARGS - ITWO   
      IF (NARGS.LT.IFOUR) CALL ERROR (10)         
      IX     = IONE 
      IFIT   = IZERO
      ISTART = IFIVE
      NNARGS = NARGS - IFOUR  
C         
C     EXTRA CHECKS ON SPOLYFIT.         
C         
      IF (L2.EQ.ITWO .AND. NARGS.EQ.IFOUR) CALL ERROR (236) 
      GO TO 30      
C         
C     CHECK ARGUMENTS FOR FIT AND SFIT. 
C         
  10  IFIT = NARGS - ITHRE - IARGS(3)   
      IF (IFIT.GT.6) CALL ERROR (10)    
      IF (IFIT.EQ.IFIVE) CALL ERROR (10)
      IF (IFIT.LT.IZERO) CALL ERROR (10)
      IF (IFIT.EQ.6) JNARGS = JNARGS - ITWO       
      IF (L2.EQ.ITHRE) GO TO 20         
C         
C     EXTRA CHECK FOR SFIT.   
C         
      IF (IFIT.EQ.IZERO) CALL ERROR (236)         
C         
C     CHECK THAT STORAGE COLUMNS FOR FIT AND POLYFIT ARE    
C        NOT THE SAME AS Y AND PREDICTED VALUES.  
C         
  20  NNARGS = IFIT 
      ISTART = IARGS(3) + IFOUR         
      IX = IARGS(3) 
  30  IF (NNARGS.LE.IZERO) GO TO 120    
      INARGS = NNARGS         
      IF (NNARGS.EQ.6) INARGS = INARGS - ITWO     
      INARGS = ISTART + INARGS - IONE   
C         
C     CHECK IF COLUMN Y IS USED AS STORAGE.       
C         
      DO 40 I=ISTART,INARGS   
        IF (IARGS(1).EQ.IARGS(I)) CALL ERROR (32) 
  40  CONTINUE      
C         
C     CHECK PREDICTOR COLUMNS AGAINST STORAGE COLUMNS.      
C         
      DO 60 I=1,IX  
        II = I + ITHRE        
        DO 50 J=ISTART,INARGS 
          IF (IARGS(II).EQ.IARGS(J)) CALL ERROR (32)        
  50    CONTINUE    
  60  CONTINUE      
C         
      IF (NNARGS.NE.6) GO TO 120        
C         
C     CHECK THAT THE STORAGE OF VARIANCE COVARIANCE MATRIX  
C        IS NOT THE SAME AS Y OR PREDICTOR VALUES.
C         
      JCVC = IARGS(NARGS)     
      IF (JCVC.LE.IZERO) GO TO 100      
      ICVC = JCVC   
      DO 70 I=1,M   
        IF (IARGS(1).EQ.ICVC .AND. N.GT.IARGS(NARGS-1)) CALL ERROR (32)         
        ICVC = ICVC + IONE    
  70  CONTINUE      
C         
      DO 90 I=1,IX  
        ICVC = JCVC 
        II = I + ITHRE        
        DO 80 J=1,M 
          IF (IARGS(II).EQ.ICVC .AND. N.GT.IARGS(NARGS-1))  
     1      CALL ERROR (32)   
          ICVC = ICVC + IONE  
  80    CONTINUE    
  90  CONTINUE      
C         
C     CHECK AREA FOR STORING VARIANCE COVARIANCE MATRIX FOR 
C        SUFFICIENT SPACE.    
C         
 100  MMTXR = M     
C         
C     THE ABOVE STATEMENT SHOULD BE REPLACED WITH THE FOLLOWING       
C        STATEMENT, IF PROCEDURES ARE AVAILABLE TO STORE GRAM FACTORS,
C        VECTOR NORMS AND GRAM DETERMINANTS IN THE FUTURE.  
C         
C100  MMTXR = M + ITHRE       
C         
      IF (IARGS(NARGS-1).LE.IZERO .OR. IARGS(NARGS-1).GT.NROW .OR.    
     1     KIND(NARGS-1).EQ.IONE) CALL ERROR (3)  
      IF (IARGS(NARGS).LE.IZERO .OR. IARGS(NARGS).GT.NCOL .OR.        
     1     KIND(NARGS).EQ.IONE) CALL ERROR (3)    
      IIRGS(NARGS-1) = NROW*(IARGS(NARGS)-IONE) + IARGS(NARGS-1)      
      IF (MMTXR-IONE+IARGS(NARGS-1).LE.NROW) GO TO 110      
      CALL ERROR (213)        
      MMTXR = NROW - IARGS(NARGS-1) - IONE        
 110  MMTXC = M     
      IF (MMTXC-IONE+IARGS(NARGS).LE.NCOL) GO TO 120        
      CALL ERROR (213)        
      MMTXC = NCOL - IARGS(NARGS) - IONE
C         
C     COMPUTE STARTING LOCATIONS FOR COLUMN NUMBERS.        
C         
 120  CALL ADRESS (IONE,IIRGS(1))       
      IF (IIRGS(1).LT.IZERO) CALL ERROR (20)      
      DO 130 I=IFOUR,JNARGS   
        II = I      
        CALL ADRESS (II,IIRGS(I))       
        IF (IIRGS(I).LT.IZERO) CALL ERROR (20)    
 130  CONTINUE      
      FM = FLOAT (M)
C         
C     CHECK WEIGHTS.
C         
      IF (KIND(2).EQ.IZERO) GO TO 150   
C         
C     CHECK WEIGHT CONSTANT AND SET UP SU, WSUM AND FM.     
C        SU   = SUM OF I FOR WEIGHTS(I) GREATER THAN ZERO.  
C        WSUM = SUM OF WEIGHTS(I).      
C        FM   = REAL NUMBER OF INTEGER M.         
C         
      IF (ARGS(2).GT.RZERO) GO TO 140   
      CALL ERROR (24)         
      GO TO 190     
C         
 140  SU = NRMAX    
      WSUM = SU * ARGS(2)     
      GO TO 190     
C         
C     CHECK COLUMN OF WEIGHTS.
C         
 150  CALL ADRESS (ITWO,IIRGS(2))       
      IF (IARGS(2).LT.IZERO) GO TO 210  
      IF (IARGS(2).EQ.IZERO) RETURN     
      SU = RZERO    
      II = IIRGS(2) 
      CALL SUMMAL (RC(II),N,WSUM)       
      IF (N.EQ.IONE) WSUM = RC(II)      
      DO 180 I=1,N  
        IF (RC(II)) 200,170,160         
 160    SU = SU + RONE        
 170    II = II + IONE        
 180  CONTINUE      
C         
 190  IF (SU.LT.FM) CALL ERROR (24)     
      GO TO 220     
C         
 200  CALL ERROR (25)         
      GO TO 220     
C         
 210  CALL ERROR (20)         
 220  IF (NERROR.NE.IZERO) RETURN       
C         
C                         ***   END OF ERROR CHECKING   *** 
C         
C     ==================================================================        
C         
C     RETURN, IF COMMAND IS SPOLYFIT OR SFIT AND NO STORAGE IS SPECIFIED.       
C         
      IF (L2.EQ.ITWO .AND. NARGS.EQ.IFOUR) RETURN 
      IF (L2.EQ.IFOUR .AND. IFIT.EQ.IZERO) RETURN 
C         
C     ISUBY   SUBSCRIPT FOR Y(1) IN ARRAY RC(.).  
C     ISUBW   SUBSCRIPT FOR W(1) IN ARRAY RC(.),  
C               OR = 1 IF ALL WEIGHTS ARE SPECIFIED AS A CONSTANT.    
C     ISUBB   SUBSCRIPT FOR B(1) - VECTOR OF COEFFS - IN ARRAY A(.).  
C     ISUBRS  SUBSCRIPT FOR RS(1) - VECTOR OF RESIDUALS - IN ARRAY A(.).        
C     ISUBR   SUBSCRIPT FOR R(1,1) - INVERSE OF X'W X MATRIX -        
C               IN ARRAY A(.).
C     ISUBSB  SUBSCRIPT FOR SB(1) - VECTOR OF STANDARD DEVIATIONS OF  
C               COEFFS. - IN ARRAY A(.).
C     ISUBSY  SUBSCRIPT FOR SY(1) - VECTOR OF STANDARD DEV. OF PRED.  
C               VALUES - IN ARRAY A(.). 
C     ISUBS   SUBSCRIPT FOR S(1) - VECTOR OF SQUARED FOURIER COEFFS. -
C               IN ARRAY A(.).
C     ISUBQ   SUBSCRIPT FOR Q(1) IN ARRAY A(.).   
C     ISUBF   SUBSCRIPT FOR F(1) IN ARRAY A(.).   
C               VECTORS Q AND F ARE USED INTERNALLY BY LSQ. 
C     ISUBAD  SUBSCRIPT FOR HOLDING ACC. DIGITS IN ARRAY A(.).        
C     ISUBYP  SUBSCRIPT FOR HOLDING RESIDUALS IN ARRAY A(.).
C               USED INTERNALLY BY LSQ. 
C         
      ISUBY  = IIRGS(1)       
      ISUBW  = IONE 
      IF (KIND(2).EQ.IZERO) ISUBW = IIRGS(2)      
      IT     = IONE 
      IF (L2.GE.ITHRE) IT = ITWO        
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
C         
      WC = RZERO    
      IF (KIND(2).EQ.IONE) WC = ARGS(2) 
C         
C     COMPUTE RESPONSE MID-RANGE IF COMMAND IS POLYFIT, SPOLYFIT      
C        OR IF FIRST IND. VECTOR OF FIT (SFIT) CONTAINS ALL ONES.     
C         
      YMDRNG = RZERO
      IF (L2.LE.ITWO) GO TO 240         
C         
C     COMMAND IS FIT (SFIT). CHECK 1ST IND. VECTOR.         
C         
      JSUBX = IIRGS(4)        
      DO 230 I=1,N  
        IF (ABS(RC(JSUBX)-RONE).GT.ERR) GO TO 270 
        JSUBX = JSUBX + IONE  
 230  CONTINUE      
      GO TO 240     
C         
C     COMPUTE MID-RANGE OF RESPONSE VECTOR.       
C         
C     DETERMINE LARGEST AND SMALLEST RESPONSE VALUE.        
C         
 240  JSUBY = ISUBY 
      YMIN = RC(JSUBY)        
      YMAX = RC(JSUBY)        
      DO 250 I=1,N  
        IF (RC(JSUBY).LT.YMIN) YMIN = RC(JSUBY)   
        IF (RC(JSUBY).GT.YMAX) YMAX = RC(JSUBY)   
        JSUBY = JSUBY + IONE  
 250  CONTINUE      
C         
      CALL ACCDIG (YMAX,YMIN,RSD,AGREE,IND)       
      IF (AGREE.LT.RHALF) GO TO 270     
C         
      YMDRG1 = FDIV (YMIN+YMAX,RTWO,IND)
      ISMTWO = ISIGD - ITWO   
      CALL LSRND (YMDRG1,ISMTWO,ISIGD,YMDRNG)     
C         
C     SUBTRACT Y MID-RANGE FROM EACH RESPONSE, IF YMIN AND YMAX AGREE 
C        TO AT LEAST HALF A DIGIT.      
C         
      JSUBY = ISUBY 
      DO 260 I=1,N  
        RC(JSUBY) = RC(JSUBY) - YMDRNG  
        JSUBY = JSUBY + IONE  
 260  CONTINUE      
C         
 270  CALL LSQ (N,M,NROW,RC,RC(ISUBY),RC(ISUBW),WC,YMDRNG,IT,IARGS(4),
     1    A(ISUBB),A(ISUBRS),A(ISUBR),A(ISUBSB),A(ISUBSY),A(ISUBS),RSS,         
     2    A(ISUBQ),A(ISUBF),A(ISUBYP),A(ISUBAD),NI,DIGIT)   
C         
C     RESTORE RESPONSE VECTOR TO ORIGINAL FORM, IF MID RANGE IS NOT 0.
C         
      IF (YMDRNG.EQ.RZERO) GO TO 290    
      JSUBY = ISUBY 
      DO 280 I=1,N  
        RC(JSUBY) = RC(JSUBY) + YMDRNG  
        JSUBY=JSUBY+IONE      
 280  CONTINUE      
C         
C     ADD Y MID-RANGE TO FIRST COEFFICIENT FROM FIT AND TO FIRST      
C        COEFFICIENT FROM REFIT TO PREDICTED VALUES.        
C         
      A(ISUBB) = A(ISUBB) + YMDRNG      
      A(ISUBAD) = A(ISUBAD) + YMDRNG    
C         
C     IF FATAL ERROR HAS OCCURED AFTER CALLING LSQ, RETURN.  ERROR    
C        MAY OCCUR IF MATRIX IS SINGULAR OR NO SOLUTION IS POSSIBLE.  
C         
 290  IF (NERROR.NE.IZERO) RETURN       
C         
C     VARNCE = RESIDUAL STAND. DEV. SQUARED.      
C     SD     = STANDARD DEVIATION = SQUARE ROOT OF VARIANCE.
C     SUMY2W = SUM OF Y(I)**2 * WEIGHTS(I).       
C         
      JS = NARGS - ISTART + IONE        
      VARNCE = FDIV (RSS,SU-FM,IND)     
      SD = FSQRT (VARNCE)     
      JSUBW = ISUBW 
      JSUBY = ISUBY 
      WW = ARGS(2)  
      CALL DSUMAL (DYW,IZERO,DSUMY2)    
      DO 300 I=1,N  
        IF (KIND(2).EQ.IZERO) WW = RC(JSUBW)      
        DYW(1) = DBLE (WW) * DBLE (RC(JSUBY))**2  
        JSUBY = JSUBY + IONE  
        JSUBW = JSUBW + IONE  
        CALL DSUMAL (DYW,-IONE,DSUMY2)  
 300  CONTINUE      
      CALL DSUMAL (DYW,IONE,DSUMY2)     
      SUMY2W = FDPCON (DSUMY2)
C         
C     STORE RESIDUAL SUM OF SQUARES AND TOTAL SUM OF SQUARES
C        AT END OF SQUARED FOURIER COEFFICIENTS.  
C         
      JSUBS = ISUBS + M       
      A(JSUBS) = RSS
      A(JSUBS+1) = SUMY2W     
C         
C     LEAST SQUARES FIT SUCCESSFUL.     
C         
      KS   = JS     
      KST  = ISTART 
      KM   = M      
      VAR  = VARNCE 
      SSU  = SU     
      KN   = N      
      SSD  = SD     
      KMTR = MMTXR  
      KMTC = MMTXC  
      RETURN        
C         
C     ==================================================================        
C         
      END 
*LSPLT2
      SUBROUTINE LSPLT2 (YMAX,YMIN,KN,KNZW,ISUBPV,IB,IXA,IWS,IBRIEF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LSPLT2 V 7.00  4/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM IS USED BY ORTPLT TO PRINT 2 PLOTS PER PAGE.
C       2 PLOTS OR 4 PLOTS ARE PRINTED DEPENDING ON WHETHER BRIEF
C          IS IN EFFECT.
C
C     YMAX       LARGEST VALUE ON Y-AXIS.
C     YMIN       SMALLEST VALUE ON Y-AXIS.
C     KN         NUMBER OF VARIABLES
C     ISUBPV     IS SUBSCRIPT FOR PREDICTED VALUES IN VECTOR A
C     IB         SCRATCH VECTOR NEEDED BY THIS PROCEDURE
C     IXA        SUSCRIPT FOR VARIABLE X IN VECTOR RC
C     IWS        SUBSCRIPT FOR WEIGHTS IN VECTOR RC
C     IBRIEF     SWITCH TO CONTROL OUTPUT.
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
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
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
      REAL             YMAX, YMIN
      REAL             AN, FDEN, GAMMA, RATIO, XMAX, XMIN, XMM
      REAL             YMM, YMMP, YMMY, YMX, YYPR
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF, SPCG, SPCH
C
C     ...................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        IB*1
      CHARACTER        ISYM*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /  0.1  /
      DATA SPCB /  0.14 /
      DATA SPCC /  1.5  /
      DATA SPCD /  3.75 /
      DATA SPCE /  4.91 /
      DATA SPCF /  8.0  /
      DATA SPCG / 25.0  /
      DATA SPCH / 50.0  /
C
C     ==================================================================
C
      IW   = IWS
      IWST = IONE
      IF (KIND(2).NE.IZERO) IWST = ITWO
      N     = KN
      NZW   = KNZW
      YMM   = FDIV (ABS(YMAX-YMIN),SPCH,IND)
      YMX   = FDIV (FLOAT(N-IONE),SPCH,IND)
      AN    = RZERO
      XMIN  = RZERO
      XMAX  = RZERO
      GAMMA = RZERO
      CALL PAGE (IFOUR)
      IPLOT = IONE
      IOUTL = IZERO
C
C     DO TWO PLOTS PER PAGE.
C
      WRITE (IPRINT,270)
      DO 230 NGRAPH=1,4
        YYPR = SPCD
        WRITE (IPRINT,290) YYPR, (LA(39),I=1,44)
        LINE = 24
        DO 140 LNZ=1,24
          YYPR = YYPR - SPCC
C
C         THIS LOOP CONTROLS THE PRINTING OF 5 LINES OF THE GRAPH.
C
          DO 130 I=1,5
            DO 10 IJI=1,102
              IB(IJI) = LA(45)
  10        CONTINUE
            GO TO (20,20,60,60), IPLOT
  20        JSUBPV = ISUBPV
            JSUBRS = ISUBPV + N
            DO 50 IJI=1,N
              ISYM  = LA(41)
              IUPLT = A(JSUBRS) + 0.5
              IF (LINE.EQ.24 .OR. LINE.EQ.IONE) THEN
                IF (IUPLT.EQ.-24 .OR. IUPLT.EQ.-IONE) THEN
                  IOUTL = IONE
                  ISYM  = LA(47)
                  IUPLT = IABS(IUPLT)
                END IF
              END IF
              IF (IUPLT.NE.LINE) GO TO 40
              IF (IPLOT.EQ.ITWO) GO TO 30
C
C             SET UP AND PLOT FIRST GRAPH.
C
              IZ = FDIV (FLOAT(IJI-IONE),YMX,IND) + RHALF
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
              GO TO 40
C
C             SET UP AND PLOT SECOND GRAPH.
C
  30          IZ = FDIV (A(JSUBPV)-YMIN,YMM,IND)
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
  40          JSUBRS = JSUBRS + IONE
              JSUBPV = JSUBPV + IONE
  50        CONTINUE
C
            GO TO 100
C
  60        JSUBRS = ISUBPV + N
            IX = IXA
            DO 90 IJI=1,N
              ISYM = LA(41)
              IUPLT = A(JSUBRS) + 0.5
              IF (LINE.EQ.24 .OR. LINE.EQ.IONE) THEN
                IF (IUPLT.EQ.-24 .OR. IUPLT.EQ.-IONE) THEN
                  IOUTL = IONE
                  ISYM  = LA(47)
                  IUPLT = IABS(IUPLT)
                END IF
              END IF
              IF (IUPLT.NE.LINE) GO TO 80
              IF (IPLOT.EQ.IFOUR) GO TO 70
C
C             SET UP AND PLOT THIRD GRAPH.
C
              IZ = FDIV (RC(IX)-XMIN,XMM,IND)
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
              GO TO 80
C
C             SET UP AND PLOT FOURTH GRAPH
C
  70          RATIO = FDIV (AN-GAMMA,FDEN,IND)
              YMM = SPCE * (RATIO**SPCB - (RONE-RATIO)**SPCB)
              AN = AN - RONE
              IF (AN.LT.RTWO .AND. NZW.LE.ITEN)
     1             GAMMA = FDIV (RONE,RTHRE,IND)
              IZ = FDIV (YMM,SPCA,IND)
              IZ = IZ + 26
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
  80          IX = IX + IONE
              JSUBRS = JSUBRS + IONE
  90        CONTINUE
C
 100        IF (I.NE.IFIVE) GO TO 110
            WRITE (IPRINT,310) YYPR, (IB(IJI),IJI=1,51)
            GO TO 120
C
 110        WRITE (IPRINT,320) (IB(IJI),IJI=1,51)
 120        LINE = LINE - IONE
            IF (LINE.EQ.IZERO) GO TO 150
 130      CONTINUE
 140    CONTINUE
C
 150    WRITE (IPRINT,290) YYPR, (LA(39),I=1,44)
        IPLOT = IPLOT + IONE
        GO TO (230,160,170,220,240), IPLOT
C
C       FINISH HORIZONTAL LINE AND LABELS FOR GRAPH ONE AND SET UP
C          HORIZONTAL LINE FOR GRAPH TWO.
C
 160    YMMP = YMX * SPCG + RONE
        WRITE (IPRINT,330) YMMP, N
        WRITE (IPRINT,260)
        WRITE (IPRINT,280)
        GO TO 230
C
C       FINSH GRAPH TWO AND SET UP GRAPH THREE.
C
 170    YMMY = FDIV (YMAX-YMIN,RTWO,IND) + YMIN
        WRITE (IPRINT,340) YMIN, YMMY, YMAX
        IF (IBRIEF.EQ.IONE) GO TO 250
        WRITE (IPRINT,260)
        IX = IXA
        IST = IONE
        IW = IWS
        DO 210 I=1,N
          IF (IWST.EQ.ITWO) GO TO 180
          IF (RC(IW).EQ.RZERO) GO TO 200
 180      IF (IST.EQ.ITWO) GO TO 190
          XMAX = RC(IX)
          XMIN = RC(IX)
          IST = ITWO
          GO TO 200
C
 190      IF (RC(IX).GT.XMAX) XMAX = RC(IX)
          IF (XMIN.GT.RC(IX)) XMIN = RC(IX)
 200      IW = IW + IONE
C
C         FINISH GRAPH THREE AND PREPARE FOR GRAPH FOUR.
C
          IX = IX + IONE
 210    CONTINUE
C
        XMM = FDIV (ABS(XMAX-XMIN),SPCH,IND)
        GAMMA = FDIV (PI,SPCF,IND)
        AN = NZW
        FDEN = AN - RTWO * GAMMA + RONE
        CALL PAGE (IFOUR)
        WRITE (IPRINT,300) (LHEAD(I),I=25,36)
        GO TO 230
C
 220    YMMY = FDIV (XMAX-XMIN,RTWO,IND) + XMIN
        WRITE (IPRINT,360) XMIN, YMMY, XMAX
        WRITE (IPRINT,260)
        WRITE (IPRINT,350)
 230  CONTINUE
C
C     FINISH GRAPH FOUR.
C
 240  WRITE (IPRINT,370)
      WRITE (IPRINT,260)
 250  IF (IOUTL.NE.IZERO) WRITE (IPRINT,380)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 260  FORMAT (1H )
 270  FORMAT (15X,36HSTANDARDIZED RESIDUALS VS ROW NUMBER)
 280  FORMAT (11X, 44HSTANDARDIZED RESIDUALS VS PREDICTED RESPONSE)
 290  FORMAT (1X,F5.2,1X,2(1H+,9A1),1H+,4A1,1HX,4A1,2(1H+,9A1),1H+)
 300  FORMAT (14X,26HSTANDARDIZED RESIDUALS VS ,12A1)
 310  FORMAT (1X,F5.2,1H+,51A1,1H+)
 320  FORMAT (6X,1H-,51A1,1H-)
 330  FORMAT (6X,3H1.0,18X,F9.4,16X,I5,2H.0 )
 340  FORMAT (1PE13.4,E26.4,10X,E10.4)
 350  FORMAT (8X, 49HNORMAL PROBABILITY PLOT OF STANDARDIZED RESIDUALS)
 360  FORMAT (1PE13.4,14X, E12.4, 8X, E12.4)
 370  FORMAT (5X,4H-2.5,22X,3H0.0,22X,3H2.5)
 380  FORMAT (5X,49HNOTE: $ DENOTES VALUES BEYOND INDICATED Y-LIMITS.)
C
C     ==================================================================
C
      END
*LSPLT4
      SUBROUTINE LSPLT4 (YMAX,YMIN,KN,KNZW,ISUBPV,IB,IXA,IWS,IBRIEF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LSPLT4 V 7.00  4/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM IS USED BY ORTPLT TO PRINT 4 PLOTS ON A PAGE.
C
C     YMAX   LARGEST VALUE ON Y AXIS.
C     YMIN   SMALLEST VALUE ON Y AXIS.
C     KN     NUMBER OF VARIABLES
C     ISUBPV IS SUBSCRIPT FOR PREDICTED VALUES IN VECTOR A
C     IB     SCRATCH VECTOR NEEDED BY THIS PROCEDURE
C     IXA    SUSCRIPT FOR VARIABLE X IN VECTOR RC
C     IWS    SUBSCRIPT FOR WEIGHTS IN VECTOR RC
C     IBRIEF SWITCH TO CONTROL OUTPUT.
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

      DIMENSION IB(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
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

      REAL             AN, FDEN, GAMMA, RATIO, XMAX, XMIN, XMM
      REAL             YMAX, YMIN, YMM, YMMY, YMX, YYPR
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF, SPCG, SPCH
      REAL             FDIV
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        IB*1
      CHARACTER        ISYM*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /  0.1  /
      DATA SPCB /  0.14 /
      DATA SPCC /  1.5  /
      DATA SPCD /  3.75 /
      DATA SPCE /  4.91 /
      DATA SPCF /  8.0  /
      DATA SPCG / 25.0  /
      DATA SPCH / 50.0  /
C
C     ==================================================================
C
      IW   = IWS
      IWST = IONE
      IF (KIND(2).NE.IZERO) IWST = ITWO
      N     = KN
      NZW   = KNZW
      YMM   = FDIV (ABS(YMAX-YMIN),SPCH,IND)
      YMX   = FDIV (FLOAT(N-IONE),SPCH,IND)
      AN    = RZERO
      XMIN  = RZERO
      XMAX  = RZERO
      GAMMA = RZERO
      CALL PAGE (IFOUR)
      IPLOT = IONE
      IOUTL = IZERO
C
C     FOUR PLOTS TO A PAGE.
C
      WRITE (IPRINT,200)
C
C     START PLOTTING.
C
      DO 170 NGRAPH=1,2
        YYPR = SPCD
        WRITE (IPRINT,210) YYPR, (LA(39),I=1,44), YYPR, (LA(39),I=1,44)
        LINE = 24
        DO 110 LNZ=1,24
          YYPR = YYPR - SPCC
C
C         THE FOLLOWING LOOP CONTROLS 5 LINES OF GRAPH PRINTING.
C
          DO 100 I=1,5
            DO 10 IJI=1,102
              IB(IJI) = LA(45)
  10        CONTINUE
            IF (IPLOT.EQ.ITWO) GO TO 40
C
C           PLOT THE TWO TOP GRAPHS..
C
            JSUBPV = ISUBPV
            JSUBRS = ISUBPV + N
            DO 30 IJI=1,N
              ISYM = LA(41)
              IUPLT = A(JSUBRS) +0.5
              IF (LINE.EQ.24 .OR. LINE.EQ.IONE) THEN
                IF (IUPLT.EQ.-24 .OR. IUPLT.EQ.-IONE) THEN
                  IOUTL = IONE
                  ISYM  = LA(47)
                  IUPLT = IABS(IUPLT)
                END IF
              END IF
              IF (IUPLT.NE.LINE) GO TO 20
              IZ = FDIV (FLOAT(IJI-IONE),YMX,IND) + RHALF
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
              IZ = FDIV (A(JSUBPV)-YMIN,YMM,IND)
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ+51) = ISYM
  20          JSUBRS = JSUBRS + IONE
              JSUBPV = JSUBPV + IONE
  30        CONTINUE
            GO TO 70
C
C           PLOT THE TWO BOTTOM GRAPHS.
C
  40        JSUBRS = ISUBPV + N
            IX = IXA
            DO 60 IJI=1,N
              ISYM = LA(41)
              IUPLT = A(JSUBRS) +0.5
              IF (LINE.EQ.24 .OR. LINE.EQ.IONE) THEN
                IF (IUPLT.EQ.-24 .OR. IUPLT.EQ.-IONE) THEN
                  IOUTL = IONE
                  ISYM  = LA(47)
                  IUPLT = IABS(IUPLT)
                END IF
              END IF
              IF (IUPLT.NE.LINE) GO TO 50
              IZ = FDIV (RC(IX)-XMIN,XMM,IND)
              IZ = IZ + IONE
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ) = ISYM
              RATIO = FDIV (AN-GAMMA,FDEN,IND)
              YMM = SPCE * (RATIO**SPCB - (RONE-RATIO)**SPCB)
              AN = AN - RONE
              IF (AN.LT.RTWO .AND. NZW.LE.ITEN)
     1               GAMMA = FDIV (RONE,RTHRE,IND)
              IZ = FDIV (YMM,SPCA,IND)
              IZ = IZ + 26
              IF (IZ.LE.IZERO) IZ = IONE
              IF (IZ.GT.51) IZ = 51
              IB(IZ+51) = ISYM
  50          IX = IX + IONE
              JSUBRS = JSUBRS + IONE
  60        CONTINUE
C
  70        IF (I.NE.IFIVE) GO TO 80
            WRITE (IPRINT,220) YYPR, (IB(IJI),IJI=1,51), YYPR,
     1          (IB(IJI),IJI=52,102)
            GO TO 90
  80        WRITE (IPRINT,230) (IB(IJI),IJI=1,102)
  90        LINE = LINE - IONE
            IF (LINE.EQ.IZERO) GO TO 120
 100      CONTINUE
 110    CONTINUE
C
 120    WRITE (IPRINT,210) YYPR, (LA(39),I=1,44), YYPR, (LA(39),I=1,44)
        IF (IPLOT.EQ.ITWO) GO TO 180
C
C       FINISH TOP HALF OF PLOTTING AND SET UP BOTTOM HALF.
C
        YMM = YMX * SPCG + RONE
        YMMY = FDIV (YMAX-YMIN,RTWO,IND) + YMIN
        WRITE (IPRINT,240) YMM, N, YMIN, YMMY, YMAX
        IF (IBRIEF.EQ.IONE) GO TO 190
        WRITE (IPRINT,250)
        IPLOT = ITWO
        IX = IXA
        IST = IONE
        IW = IWS
        DO 160 I=1,N
          IF (IWST.EQ.ITWO) GO TO 130
          IF (RC(IW).EQ.RZERO) GO TO 150
C
C         FINISH BOTTOM HALF OF GRAPHS.
C
 130      IF (IST.EQ.ITWO) GO TO 140
          XMAX = RC(IX)
          XMIN = RC(IX)
          IST = ITWO
          GO TO 150
 140      IF (RC(IX).GT.XMAX) XMAX = RC(IX)
          IF (XMIN.GT.RC(IX)) XMIN = RC(IX)
 150      IW = IW + IONE
          IX = IX + IONE
 160    CONTINUE
C
        XMM = FDIV (ABS(XMAX-XMIN),SPCH,IND)
        GAMMA = FDIV (PI,SPCF,IND)
        AN = NZW
        FDEN = AN - RTWO * GAMMA + RONE
        WRITE (IPRINT,260) (LHEAD(I),I=25,36)
 170  CONTINUE
C
 180  YMMY = FDIV (XMAX-XMIN,RTWO,IND) + XMIN
      WRITE (IPRINT,270) XMIN, YMMY, XMAX
 190  IF (IOUTL.NE.IZERO) WRITE (IPRINT,280)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 200  FORMAT (15X,36HSTANDARDIZED RESIDUALS VS ROW NUMBER,21X,
     1 44HSTANDARDIZED RESIDUALS VS PREDICTED RESPONSE)
 210  FORMAT (1X,F5.2,1X,2(1H+,9A1),1H+,4A1,1HX,4A1,2(1H+,9A1),1H+,
     1 4X,F5.2,1X,2(1H+,9A1)
     2 ,1H+,4A1,1HX,4A1,2(1H+,9A1),1H+)
 220  FORMAT (1X,F5.2,1H+,51A1,1H+,3X,F5.2,1H+,51A1,1H+)
 230  FORMAT (6X,1H-,51A1,1H-,8X,1H-,51A1,1H-)
 240  FORMAT (6X,3H1.0,18X,F9.4,16X,I5,2H.0 ,1PE15.4,E26.4,10X,E10.4)
 250  FORMAT (1H )
 260  FORMAT (14X,26HSTANDARDIZED RESIDUALS VS ,12A1,  17X,
     1 49HNORMAL PROBABILITY PLOT OF STANDARDIZED RESIDUALS)
 270  FORMAT (1PE13.4,14X, E12.4, 8X, E12.4,7X,4H-2.5,22X,3H0.0,22X,
     1 3H2.5)
 280  FORMAT (5X,49HNOTE: $ DENOTES VALUES BEYOND INDICATED Y-LIMITS.)
C
C     ==================================================================
C
      END
*LSPRNT
      SUBROUTINE LSPRNT (M,N,IIRGS,SD,VARNCE,SU,NUMIT,DIGITS,NX,LSIND)
C
C **  NBS OMNITAB 1980 VERSION 6.02  2/20/81. LSPRNT V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE DOES THE AUTOMATIC PRINTOUT FOR FIT AND POLYFIT.
C
C     INPUT ...
C
C            M  NUMBER OF INDEPENDENT VECTORS.
C            N  LENGTH OF RESPONSE AND IND. VECTORS.
C           SD  RESIDUAL STANDARD.
C       VARNCE  RESIDUAL STANDARD SQUARED.
C           SU  SUM OF WEIGHTS.
C
C     ==================================================================
C
C                         ***   STRUCTURE CHART   ***
C
C     NOTES ...
C
C        (1)   FUNCTIONS SUCH AS FDIV AND IDIV ARE NOT INCLUDED.
C        (2)   UTILITY PROGRAM UNITS SUCH AS RFORMT, PAGE, MINNW, ETC.
C                 ARE NOT INCLUDED.
C
C     ..........     ..........
C     . LSPRNT ....... OPONE  .
C     ..........  .  ..........
C                 .
C                 .  ..........     ..........
C                 .... LSDIAG ....... VARRES .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... SORTLS .
C                 .                 ..........
C                 .
C                 .  ..........     ..........
C                 .... ORTPLT ....... LSPLT2 .
C                 .  ..........  .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... LSPLT4 .
C                 .                 ..........
C                 .
C                 .  ..........
C                 .... OCOVAR .
C                 .  ..........
C                 .
C                 .  ..........     ..........     ..........
C                 .... REGLOF ....... LOFIND ....... MCHROW .
C                 .  ..........  .  ..........     ..........
C                 .              .
C                 .              .  ..........
C                 .              .... PERRSS .
C                 .              .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... WERRSS .
C                 .              .  ..........
C                 .              .
C                 .              .  ..........
C                 .              .... OUTLOF .
C                 .                 ..........
C                 .
C                 .  ..........
C                 .... OANOVA .
C                 .  ..........
C                 .
C                 .  ..........
C                 .... OCOEFF .
C                 .  ..........
C                 .
C                 .  ..........     ..........
C                 .... CONFEL ....... FNUALF .
C                    ..........  .  ..........
C                                .
C                                .  ..........
C                                .... ELIPSE .
C                                .  ..........
C                                .
C                                .  ..........     ..........
C                                .... PLOTCE ....... SCALE2 .
C                                   ..........     ..........
C
C     ==================================================================
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - AUGUST, 1977.
C                   CURRENT VERSION -  APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IIRGS(*), NWTWO(3)
      DIMENSION IHC(4), IHT(8), MA(120)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
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
C                         ***   TYPE STATEMENTS   ***
C
      REAL             DIGITS, SD, SU, VARNCE
      REAL             FM, TOLNCE
      REAL             SPCA
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LHEAD*1
      CHARACTER        IBC*1, IHC*3, MA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IHC(1), IHC(2), IHC(3), IHC(4) / ' TE', 'RM ', 'COL', 'UMN' /
C
      DATA IGHT   /  8 /
C
      DATA SPCA / 0.5E-5 /
C
C     ==================================================================
C
C     ISUBY   SUBSCRIPT FOR Y(1) IN ARRAY RC(.).
C     ISUBW   SUBSCRIPT FOR W(1) IN ARRAY RC(.),
C               OR = 1 IF ALL WEIGHTS ARE SPECIFIED AS A CONSTANT.
C     ISUBB   SUBSCRIPT FOR B(1) - VECTOR OF COEFFS - IN ARRAY A(.).
C     ISUBRS  SUBSCRIPT FOR RS(1) - VECTOR OF RESIDUALS - IN ARRAY A(.).
C     ISUBR   SUBSCRIPT FOR R(1,1) - INVERSE OF X'X MATRIX -
C               IN ARRAY A(.).
C     ISUBSB  SUBSCRIPT FOR SB(1) - VECTOR OF STANDARD DEVIATIONS OF
C               COEFFS. - IN ARRAY A(.).
C     ISUBSY  SUBSCRIPT FOR SY(1) - VECTOR OF STANDARD DEV. OF PRED.
C               VALUES - IN ARRAY A(.).
C     ISUBS   SUBSCRIPT FOR S(1) - VECTOR OF SQUARED FOURIER COEFFS. -
C               IN ARRAY A(.).
C     ISUBQ   SUBSCRIPT FOR Q(1) IN ARRAY A(.).
C     ISUBF   SUBSCRIPT FOR F(1) IN ARRAY A(.).
C               VECTORS Q AND F ARE USED INTERNALLY BY LSQ.
C     ISUBAD  SUBSCRIPT FOR HOLDING ACC. DIGITS IN ARRAY A(.).
C
      TOLNCE = RTEN * RER
C
      MXLINE = 60
      IF(ISBFT.EQ.IONE) MXLINE = LENGTH + IFOUR
      ISUBY = IIRGS(1)
      ISUBW = IONE
      IF (KIND(2).EQ.IZERO) ISUBW = IIRGS(2)
      ISUBB  = IONE
      ISUBRS = M + IONE
      ISUBR  = ISUBRS + N
      ISUBSB = ISUBR + IDIV ((M+IONE)*(M+ITWO),ITWO,IND)
      ISUBSY = ISUBSB + M + IONE
      ISUBS  = ISUBSY + N
      ISUBQ  = ISUBS + M + ITWO
      ISUBF  = ISUBQ + N * (M+IONE)
      ISUBAD = ISUBF + M + IONE
C
C     NX = 0 IF POLYFIT OR SPOLYFIT.
C     NX = 0 FOR FIT (SFIT) IF FIRST VARIABLE ALL X(I) NOT EQUAL ONE.
C     NX = 1 FOR FIT (SFIT) IF FIRST VARIABLE ALL THE X(I) = 1.
C     MX = M-NX OR 1 WHICHEVER IS GREATER.
C
      FM = M
      NX = IZERO
      ITITLE = IONE
      IF (L2.EQ.ITHRE) ITITLE = ITWO
      IPG      = IONE
      NWTWO(1) = IARGS(4)
      IF (L2.LE.ITWO) GO TO 30
      JSUBX = IIRGS(4)
      IF (M.GT.IONE) NWTWO(1) = IARGS(5)
      DO 20 I=1,N
        IF (ABS(RC(JSUBX)-RONE).LE.TOLNCE) GO TO 10
        GO TO 30
  10    JSUBX = JSUBX + IONE
  20  CONTINUE
      NX = IONE
      IF (M.GT.ITWO) NWTWO(1) = IARGS(6)
C
  30  MX = MAX0 (IONE,M-NX)
      NWTWO(2) = IARGS(1)
      NSU = SU + SPCA
      NW2 = IFOUR
      IF (L2.EQ.ITHRE .AND. MX.NE.M) NW2 = IFIVE
      NWTWO(3) = IARGS(NW2)
      CALL HEADS (NWTWO(1),ITHRE,IZERO,IONE)
      DO 40 I=1,12
        LHEAD(I+36) = LHEAD(I)
  40  CONTINUE
C
      DO 50 I=1,13
        LHEAD(I+48) = LA(45)
  50  CONTINUE
C
      ISTOP = 24
      IBEG = 13
      DO 60 I=1,12
        IF (LHEAD(IBEG).NE.LA(45)) GO TO 70
        IBEG = IBEG + IONE
  60  CONTINUE
C
  70  DO 80 I=1,12
        IF (LHEAD(ISTOP).NE.LA(45)) GO TO 90
        ISTOP = ISTOP - IONE
  80  CONTINUE
C
  90  ISUBH = 49
      DO 100 I=IBEG,ISTOP
        LHEAD(ISUBH) = LHEAD(I)
        ISUBH = ISUBH + IONE
 100  CONTINUE
C
      LHEAD(ISUBH) = LA(44)
      IXA = IFOUR
      IF (L2.EQ.ITHRE) IXA = IXA + (M-MX)
      IX = IXA
      MCNTX = IGHT
      ILN = IZERO
      IF (LWIDE.LT. LWC) ILN = ITWO
      DO 390 IPAGE=1,5
        IF (IPG.EQ.ITWO) GO TO 270
        IF (IPG.EQ.IFIVE) GO TO 270
        IF (IPG.EQ.IFOUR .AND. MCNTX+15+M+ILN.LE.MXLINE) GO TO 270
        CALL PAGE (IFOUR)
        MCNTX = IGHT
        IF (LWIDE.GE.LWC) GO TO 110
        WRITE (IPRINT,410) (LHEAD(I),I=49,61)
        GO TO 120
C
 110    WRITE (IPRINT,400) (LHEAD(I),I=49,61)
 120    GO TO (130,150), ITITLE
C
C       PRINT POLYFIT TITLE
C
 130    IF (LWIDE.GE.LWC) GO TO 140
        WRITE (IPRINT,430) IARGS(3), (LHEAD(I),I=25,36)
        GO TO 230
 140    WRITE (IPRINT,420) IARGS(3), (LHEAD(I),I=25,36)
        GO TO 230
C
C       TITLE FOR PRINT.
C
 150    II = IARGS(3) + ITHRE
        IBA = II
        IBC = LA(44)
        JP = ITEN
        JPA = 23
        JPB = 6
        IF (LWIDE.GE.LWC) GO TO 160
        JP = IFIVE
        JPA = 14
        JPB = IONE
 160    IF (II.GT.JP) II = JP
        IF (M.GT.IONE) GO TO 180
        IF (LWIDE.GE.LWC) GO TO 170
        WRITE (IPRINT,500) M, IARGS(4)
        GO TO 230
C
 170    WRITE (IPRINT,490) M, IARGS(4)
        GO TO 230
C
 180    IF (IBA.EQ.II) IBC = LA(45)
        IF (LWIDE.GE.LWC) GO TO 190
        WRITE (IPRINT,500) M, IARGS(4), LA(44), IARGS(II), IBC
        GO TO 200
 190    WRITE (IPRINT,490) M, IARGS(4), (LA(44),IARGS(I),I=5,II), IBC
 200    DO 220 J=1,9
          IF (M.LE.JPA*(J-IONE)+JPB+IONE) GO TO 230
          II = JPA * J + JP
          III = II - JPA + IONE
          II = MIN0 (II,IARGS(3)+ITHRE)
          IF (II.NE.III) GO TO 210
          WRITE (IPRINT,510) IARGS(II)
          MCNTX = MCNTX + IONE
          GO TO 230
 210      III = III + IONE
          IF (II.EQ.IBA) IBC = LA(45)
          WRITE (IPRINT,510) IARGS(III-1),(LA(44),IARGS(I),I=III,II),IBC
          MCNTX = MCNTX + IONE
 220    CONTINUE
 230    IF (KIND(2).EQ.IONE) GO TO 250
        NZW = NRMAX - NSU
        IF (LWIDE.GE.LWC) GO TO 240
        WRITE (IPRINT,450) NSU, NZW, IARGS(2)
        GO TO 270
 240    WRITE (IPRINT,440) NSU, NZW, IARGS(2)
        GO TO 270
 250    CALL RFORMT (0,ISIGD,ARGS(2),A(1),1,10,NW1,NDEC1,MA(1),IRF)
        CALL RFORMT (1,ISIGD,A,ARGS(2),0,0,NW1,NDEC1,MA(1),IRF)
        IF (LWIDE.GE.LWC) GO TO 260
        WRITE (IPRINT,470) NSU, (MA(I),I=1,10)
        GO TO 270
C
 260    WRITE (IPRINT,460) NSU, (MA(I),I=1,10)
 270    GO TO (280,300,330,360,370), IPG
 280    ISUBPV = ISUBQ
C
C       PRINTING PAGE 1 OF AUTOMATIC OUTPUT.
C
        IF (ISBFT.NE.IZERO) GO TO 290
        CALL OPONE (N,MX,NX,ISUBSY,ISUBPV,ISUBRS,MA,VARNCE,IX,MCNTX,
     1       MXLINE)
C
 290    CALL LSDIAG (M,NSU,VARNCE,ISUBQ,ISUBF,ISUBSY,
     1                     ISUBRS,MXLINE,MCNTX)
C
        IPG = ITWO
        GO TO 380
C
 300    IF (ISBFT.EQ.IONE) GO TO 320
        IF (NSU.GE.ITHRE) GO TO 310
        WRITE (IPRINT,480)
        GO TO 320
C
C       PRINTING FOUR PLOTS FOR PAGE 2 OF AUTOMATIC OUTPUT.
C
 310    CALL ORTPLT (ISUBRS,ISUBSY,N,VARNCE,ISUBPV,ISUBY,MA,IIRGS(IXA),
     1       IIRGS(2),ISBFT)
 320    IPG = ITHRE
        GO TO 380
C
C       PRINTING PAGE 3 OF AUTOMATIC OUTPUT.
C
 330    IF (ISBFT.EQ.IONE) GO TO 350
        MD1 = IDIV (M*(M+IONE),ITWO,IND)
        JSUBR = ISUBR
        JSUBQ = ISUBQ
        DO 340 I=1,MD1
          A(JSUBQ) = A(JSUBR) * VARNCE
          JSUBQ = JSUBQ + IONE
          JSUBR = JSUBR + IONE
 340    CONTINUE
        CALL OCOVAR (M,ISUBQ,MD1,IHC,MA,IHT,MCNTX,MXLINE)
C
C       PRINT LACK OF FIT ANALYSIS OF VARIANCE
C          WHEN THERE ARE REPEATED MEASUREMENTS.
C
 350    CALL REGLOF (M,ISUBY,ISUBQ,ISUBS,MCNTX,MXLINE)
C
C       PRINT ANALYSIS OF VARIANCE.
C
        CALL OANOVA (SU,ISUBS,FM,M,ISUBQ,VARNCE,IHC,NSU,MA,MCNTX,MXLINE)
        IPG = IFOUR
        GO TO 380
C
C       SAVE OLD COEFFS. AND S. D. OF COEF. BEFORE REFIT.
C
 360    CALL OCOEFF (M,ISUBSB,IHC,MA,ISUBAD,NSU,SD,
     1       MCNTX,MXLINE,NUMIT,DIGITS)
        IPG = IFIVE
        GO TO 390
C
 370    IF (L2.NE.IONE .OR. M.NE.ITWO) GO TO 380
        IF (LSIND.EQ.IONE) GO TO 380
        IWTS = IZERO
        IF (KIND(2).EQ.IZERO) IWTS = IONE
        CALL PAGE (IFOUR)
        CALL CONFEL (ISUBB,251,IIRGS(4),N,VARNCE,RC(ISUBW),ARGS(2),
     1       IWTS,SU)
 380    IF (IPG.EQ.IFIVE) RETURN
 390  CONTINUE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 400  FORMAT (/35X,31HLEAST SQUARES FIT OF RESPONSE, ,13A1)
 410  FORMAT (/  13X,31HLEAST SQUARES FIT OF RESPONSE, ,13A1)
 420  FORMAT (24X,26HAS A POLYNOMIAL OF DEGREE ,I2,26H. INDEPENDENT VARI
     1ABLE IS ,12A1)
 430  FORMAT ( 4X,26HAS A POLYNOMIAL OF DEGREE ,I2,26H. INDEPENDENT VARI
     1ABLE IS ,12A1)
 440  FORMAT (20X,6HUSING ,I4,22H NON-ZERO WEIGHTS AND ,I4,24H ZERO WEIG
     1HTS IN COLUMN ,I4)
 450  FORMAT ( 4X,6HUSING ,I4,22H NON-ZERO WEIGHTS AND ,I4,24H ZERO WEIG
     1HTS IN COLUMN ,I4)
 460  FORMAT (35X,6HUSING ,I4,19H NON-ZERO WEIGHTS =,10A1)
 470  FORMAT (13X,6HUSING ,I4,19H NON-ZERO WEIGHTS =,10A1)
 480  FORMAT (60H0 PLOTS ARE NOT PRINTED BECAUSE NO. OF POINTS IS LESS T
     1HAN 3)
 490  FORMAT (22X,24HAS A LINEAR FUNCTION OF ,I2, 34H INDEPENDENT VARIAB
     1LES IN COLUMNS ,I4,7(A1,I4))
 500  FORMAT ( 2X,24HAS A LINEAR FUNCTION OF ,I2, 34H INDEPENDENT VARIAB
     1LES IN COLUMNS ,I4,6(A1,I4))
 510  FORMAT (2X,I4,23(A1,I4))
C
C     ==================================================================
C
      END
*LSTORE
      SUBROUTINE LSTORE (JS,IIRGS,ISTS,M,VARNCE,SU,N,SD,MMTXR,MMTXC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. LSTORE V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE STORES RESULTS IN WORKSHEET FOR FIT, SFIT,
C       POLYFIT AND SPOLYFIT IF REQUESTED BY THE USER.
C
C     INPUT ...
C
C         JS    = 1 STORE COEFFS.
C         JS    = 2 STORE COEFFS. AND RESIDUALS.
C         JS    = 3 STORE COEFFS., RESIDUALS AND S. D. OF PRED. VALUES.
C         JS    = 4 AS ABOVE PLUS SQUARED FOURIER COEFFS.
C         JS    = 5 AS ABOVE PLUS VARIANCE COVARIANCE MATRIX.
C         IIRGS   ADDRESSES OF ARGUMENTS.
C         ISTS    STARTING ADRESS OF FIRST INDEPENDENT VARIABLE.
C         M       DEG. OF POLYNOMIAL OR NUMBER OF INDEPENDENT VARIABLES.
C         VARNCE  RESIDUAL STAND. DEV. SQUARED.
C         SU      NUMBER OF NON-ZERO WEIGHTS.
C         N       NUMBER OF ENTRIES IN THE INDEPENDENT VARIABLE.
C         SD      RESIDUAL STANDARD DEVIATION.
C         MMTXR   NUM. OF ROWS AVAILABLE TO STORE VARIANCE COV. MATRIX.
C         MMTXC   NUM. OF COLS AVAILABLE TO STORE VARIANCE COV. MATRIX.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
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
      DIMENSION IIRGS(*)
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
C                         ***   TYPE STATEMENTS   ***
C
      REAL             SD, SU, VARNCE
      REAL             YW(1)
      REAL             CCOEFS, FM, SW, WW, YBAR, YYBARS
      REAL             FDIV
C
C     ==================================================================
C
C     ISUBY   SUBSCRIPT FOR Y(1) IN ARRAY RC(.).
C     ISUBW   SUBSCRIPT FOR W(1) IN ARRAY RC(.),
C               OR = 1 IF ALL WEIGHTS ARE SPECIFIED AS A CONSTANT.
C     ISUBB   SUBSCRIPT FOR B(1) - VECTOR OF COEFFS - IN ARRAY A(.).
C     ISUBRS  SUBSCRIPT FOR RS(1) - VECTOR OF RESIDUALS - IN ARRAY A(.).
C     ISUBR   SUBSCRIPT FOR R(1,1) - INVERSE OF (X-TRANSPOSE)(W)(X)
C               MATRIX - IN ARRAY A(.).
C     ISUBSB  SUBSCRIPT FOR SB(1) - VECTOR OF STANDARD DEVIATIONS OF
C               COEFFS. - IN ARRAY A(.).
C     ISUBSY  SUBSCRIPT FOR SY(1) - VECTOR OF STANDARD DEV. OF PRED.
C               VALUES - IN ARRAY A(.).
C     ISUBS   SUBSCRIPT FOR S(1) - VECTOR OF SQUARED FOURIER COEFFS. -
C               IN ARRAY A(.).
C
      ISUBY = IIRGS(1)
      ISUBW = IONE
      IF (KIND(2).EQ.IZERO) ISUBW = IIRGS(2)
      ISUBB = IONE
      ISUBRS = M + IONE
      ISUBR = ISUBRS + N
      ISUBSB = ISUBR + IDIV ((M+IONE)*(M+ITWO),ITWO,IND)
      ISUBSY = ISUBSB + M + IONE
      ISUBS = ISUBSY + N
      FM = M
      ISTART = ISTS
C
C     STORE COEFFICIENTS.
C
      JCOEF = IIRGS(ISTART)
      JSUBB = ISUBB
      DO 10 I=1,M
        RC(JCOEF) = A(JSUBB)
        JCOEF = JCOEF + IONE
        JSUBB = JSUBB + IONE
  10  CONTINUE
C
C     STORE STANDARD DEVIATION OF COEFFICIENTS.
C
      M2 = ITWO * M
      IF (M2.GT.NROW) M2 = NROW
      MSTART = M + IONE
      IF (MSTART.GT.NROW) GO TO 150
      JSUBSB = ISUBSB
      DO 20 I=MSTART,M2
        RC(JCOEF) = A(JSUBSB)
        JCOEF = JCOEF + IONE
        JSUBSB = JSUBSB + IONE
  20  CONTINUE
C
C     CHECK IF THERE IS ENOUGH SPACE TO STORE REMAINDER VALUES.
C
      MEND = M2 + 6
      IF (MEND.GT.NROW) MEND = NROW
      MSWT = MEND - M2
      IF (MSWT.LE.IZERO) GO TO 150
      GO TO (140,130,120,110,100,30), MSWT
C
C     STORE MULTIPLE CORRELATION COEFFICIENT SQUARED,
C        PROVIDING FIRST VECTOR IS CONSTANT TERM.
C
  30  JSUBX = IIRGS(4)
      IF (L2.LE.ITWO) GO TO 50
      DO 40 I=1,N
        IF (RC(JSUBX).NE.RONE) GO TO 100
        JSUBX = JSUBX + IONE
  40  CONTINUE
C
  50  JSUBY = ISUBY
      JSUBW = ISUBW
C
C     COMPUTE YBAR = SUM OF WEIGHTS(I)*Y(I).
C
      CALL SUMMAL (YW,IZERO,YBAR)
      SW = RZERO
      WW = RZERO
      IF (KIND(2).EQ.IZERO) GO TO 60
      WW = ARGS(2)
      SW = FLOAT(N) * WW
  60  DO 80 I=1,N
        IF (KIND(2).EQ.IONE) GO TO 70
        WW = RC(JSUBW)
        SW = SW + WW
        JSUBW = JSUBW + IONE
  70    YW(1) = RC(JSUBY) * WW
        CALL SUMMAL (YW,-IONE,YBAR)
        JSUBY = JSUBY + IONE
  80  CONTINUE
      CALL SUMMAL (YW,IONE,YBAR)
      YBAR = FDIV (YBAR,SW,IND)
C
C     COMPUTE YYBARS = SUM OF WEIGHTS(I)*(Y(I)-YBAR)**2.
C
      JSUBY = ISUBY
      JSUBW = ISUBW
      CALL SUMMAL (YW,IZERO,YYBARS)
      DO 90 I=1,N
        IF (KIND(2).EQ.IZERO) WW = RC(JSUBW)
        YW(1) = (RC(JSUBY)-YBAR)**2 * WW
        CALL SUMMAL (YW,-IONE,YYBARS)
        JSUBW = JSUBW + IONE
        JSUBY = JSUBY + IONE
  90  CONTINUE
      CALL SUMMAL (YW,IONE,YYBARS)
      CCOEFS = RONE - FDIV (VARNCE*(SU-FM),YYBARS,IND)
      IF (CCOEFS.LT.RZERO) CCOEFS = RZERO
      IF (CCOEFS.GT.RONE) CCOEFS = RONE
      RC(JCOEF+5) = CCOEFS
C
C     STORE RESIDUAL VARIANCE.
C
 100  RC(JCOEF+4) = VARNCE
C
C     STORE RESIDUAL STANDARD DEVIATION.
C
 110  RC(JCOEF+3) = SD
C
C     STORE RESIDUAL DEGREES OF FREEDOM.
C
 120  RC(JCOEF+2) = SU - FM
C
C     STORE NUMBER OF VECTORS OR DEG. + 1.
C
 130  RC(JCOEF+1) = FM
C
C     STORE NUMBER OF NON-ZERO WEIGHTS.
C
 140  RC(JCOEF) = SU
C
C     CHECK IF RESIDUALS ARE TO BE STORED.
C
 150  IF (JS.EQ.IONE) RETURN
C
C     STORE RESIDUALS.
C
      JSUBRS = ISUBRS
      JRES = IIRGS(ISTART+1)
      DO 160 I=1,N
        RC(JRES) = A(JSUBRS)
        JSUBRS = JSUBRS + IONE
        JRES = JRES + IONE
 160  CONTINUE
      IF (JS.EQ.ITWO) RETURN
C
C     STORE STANDARD DEVIATIONS OF PREDICTED VALUES.
C
      JSUBSY = ISUBSY
      JSDPV = IIRGS(ISTART+2)
      DO 170 I=1,N
        RC(JSDPV) = A(JSUBSY)
        JSDPV = JSDPV + IONE
        JSUBSY = JSUBSY + IONE
 170  CONTINUE
      IF (JS.EQ.ITHRE) RETURN
C
C     STORE SQUARED FOURIER COEFFICIENTS.
C
      JSUBS = ISUBS
      JSFC = IIRGS(ISTART+3)
      DO 180 I=1,M
        RC(JSFC) = A(JSUBS)
        JSUBS = JSUBS + IONE
        JSFC = JSFC + IONE
 180  CONTINUE
      IF (NROW.EQ.(M+IONE)) GO TO 190
      IF (NROW.LT.(M+IONE)) GO TO 200
C
C     STORE SUM OF Y(I) * WEIGHTS(I).
C
      RC(JSFC+1) = A(JSUBS+1)
C
C     STORE THE RESIDUAL SUM OF SQUARES.
C
 190  RC(JSFC) = A(JSUBS)
      IF (JS.EQ.IFOUR) RETURN
C
C     STORE VARIANCE COVARIANCE MATRIX.
C
 200  JSUBR = ISUBR
      ISUBVC = IIRGS(NARGS-1)
      JMMTXR = MIN0 (M,MMTXR)
      JMMTXC = MIN0 (M,MMTXC)
C
C     STORE LOWER HALF OF VC MATRIX.
C
      DO 220 J=1,JMMTXC
        NSUBR = JSUBR
        IF (J.GT.JMMTXR) GO TO 230
        JSUBVC = ISUBVC
        DO 210 I=J,JMMTXR
          RC(JSUBVC) = A(NSUBR) * VARNCE
          NSUBR = NSUBR + IONE
          JSUBVC = JSUBVC + IONE
 210    CONTINUE
        JSUBR = JSUBR + M - J + IONE
        ISUBVC = ISUBVC + NROW + IONE
 220  CONTINUE
C
C     STORE UPPER HALF OF VC MATRIX.
C
 230  IF (MMTXC.EQ.IONE) GO TO 260
      ISUBVC = IIRGS(NARGS-1)
      JSUBR = ISUBR
      DO 250 I=1,JMMTXR
        JMMTX = I + IONE
        JSUBVC = ISUBVC + NROW
        NSUBR = JSUBR + IONE
        IF (I.GE.JMMTXC) GO TO 260
        DO 240 J=JMMTX,JMMTXC
          RC(JSUBVC) = A(NSUBR) * VARNCE
          JSUBVC = JSUBVC + NROW
          NSUBR = NSUBR + IONE
 240    CONTINUE
        ISUBVC = ISUBVC + NROW + IONE
        JSUBR = JSUBR + M - I + IONE
 250  CONTINUE
C
 260  RETURN
C
C     ==================================================================
C
      END

