*FCDF
      SUBROUTINE FCDF(X,NU1,NU2,CDF)
C
C **  NBS OMNITAB 1980 VERSION 6.04 11/12/84.   FCDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE F DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM
C              PARAMETERS = NU1 AND NU2.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU1    = THE INTEGER DEGREES OF FREEDOM
C                                FOR THE NUMERATOR OF THE F RATIO.
C                                NU1 SHOULD BE POSITIVE.
C                     --NU2    = THE INTEGER DEGREES OF FREEDOM
C                                FOR THE DENOMINATOR OF THE F RATIO.
C                                NU2 SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE F DISTRIBUTION
C             WITH DEGREES OF FREEDOM
C             PARAMETERS = NU1 AND NU2.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C                 --NU1 SHOULD BE A POSITIVE INTEGER VARIABLE.
C                 --NU2 SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF,CHSCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGES 946-947,
C                 FORMULAE 26.6.4, 26.6.5, 26.6.8, AND 26.6.15.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGE 83, FORMULA 20,
C                 AND PAGE 84, THIRD FORMULA.
C               --PAULSON, AN APPROXIMATE NORMAILIZATION
C                 OF THE ANALYSIS OF VARIANCE DISTRIBUTION,
C                 ANNALS OF MATHEMATICAL STATISTICS, 1942,
C                 NUMBER 13, PAGES 233-135.
C               --SCHEFFE AND TUKEY, A FORMULA FOR SAMPLE SIZES
C                 FOR POPULATION TOLERANCE LIMITS, 1944,
C                 NUMBER 15, PAGE 217.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--AUGUST    1972.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --OCTOBER   1976.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C               ADAPTED TO OMNITAB BY -
C                      SALLY PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1984.
C                   CURRENT VERSION - FEBRUARY, 1990.
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
C     ==================================================================
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DX, ANU1, ANU2, Z, SUM, TERM, AI, COEF1, COEF2
      DOUBLE PRECISION COEF, ARG
      DOUBLE PRECISION THETA, SINTH, COSTH, A, B
      DOUBLE PRECISION DATAN
      DOUBLE PRECISION DFACT1, DFACT2, DNUM, DDEN
      DOUBLE PRECISION DPOW1, DPOW2
      DOUBLE PRECISION DNU1, DNU2
      DOUBLE PRECISION TERM1, TERM2, TERM3
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPOW1 / 0.33333333333333D0 /
      DATA DPOW2 / 0.66666666666667D0 /
C
      DATA NUCUT2 / 1000 /
C
C     ==================================================================
C
      B = DZERO
C
C     IF X IS LESS THAN 0.0, SET CDF = 0.0 AND RETURN.
C
      IF(X.GE.RZERO) GO TO 10
      CDF = RZERO
      RETURN
C
C     ==================================================================
C
  10  DX   = X
      M    = NU1
      N    = NU2
      ANU1 = NU1
      ANU2 = NU2
      DNU1 = NU1
      DNU2 = NU2
C
C     IF X IS NON-POSITIVE, SET CDF = 0.0 AND RETURN.
C     IF NU2 IS 5 THROUGH 9 AND X IS MORE THAN 230
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF = 0.0 AND RETURN.
C     IF NU2 IS 10 OR LARGER AND X IS MORE THAN 150
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF = 0.0 AND RETURN.
C     IF NU2 IS 5 THROUGH 9 AND X IS MORE THAN 230
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF = 1.0 AND RETURN.
C     IF NU2 IS 10 OR LARGER AND X IS MORE THAN 150
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF = 1.0 AND RETURN.
C
      IF (X.LE.RZERO) GO TO 20
      IF (NU2.LE.IFOUR) GO TO 40
      T1     = FDDIV (DTWO, ANU1, IND)
      T2     = FDDIV (ANU2, ANU2-DTWO, IND)
      T3     = FDDIV (ANU1+ANU2-DTWO, ANU2-DFOR, IND)
      AMEAN  = T2
      SD     = FSQRT (T1 * T2 * T2 * T3)
      ZRATIO = FDIV (X-AMEAN, SD, IND)
      IF (NU2.LT.ITEN .AND. ZRATIO.LT.-3000.0) GO TO 20
      IF (NU2.GE.ITEN .AND. ZRATIO.LT.-150.0) GO TO 20
      IF (NU2.LT.ITEN .AND. ZRATIO.GT.3000.0) GO TO 30
      IF (NU2.GE.ITEN .AND. ZRATIO.GT.150.0) GO TO 30
       GO TO 40
  20  CDF = RZERO
      RETURN
C
C     ==================================================================
C
  30  CDF = RONE
      RETURN
C
C     ==================================================================
C
C     DISTINGUISH BETWEEN 6 SEPARATE REGIONS
C     OF THE (NU1,NU2) SPACE.
C     BRANCH TO THE PROPER COMPUTATIONAL METHOD
C     DEPENDING ON THE REGION.
C     NUCUT2 HAS THE VALUE 1000.
C
  40  IF (NU1.LT.NUCUT2 .AND. NU2.LT.NUCUT2) GO TO 50
      IF (NU1.GE.NUCUT2 .AND. NU2.GE.NUCUT2) GO TO 220
      IF (NU1.LT.IHRD .AND. NU2.GE.NUCUT2) GO TO 230
      IF (NU1.GE.IHRD .AND. NU2.GE.NUCUT2) GO TO 220
      IF (NU1.GE.NUCUT2 .AND. NU2.LT.IHRD) GO TO 240
      IF (NU1.GE.NUCUT2 .AND. NU2.GE.IHRD) GO TO 220
C
C     TREAT THE CASE WHEN NU1 AND NU2
C     ARE BOTH SMALL OR MODERATE
C     (THAT IS, BOTH ARE SMALLER THAN 1000).
C     METHOD UTILIZED--EXACT FINITE SUM
C     (SEE AMS 55, PAGE 946, FORMULAE 26.6.4, 26.6.5,
C     AND 26.6.8).
C
  50  Z      = FDDIV( ANU2, ANU2+ANU1*DX, IND)
      IFLAG1 = NU1 - ITWO * (NU1 / ITWO)
      IFLAG2 = NU2 - ITWO * (NU2 / ITWO)
      IF (IFLAG1.EQ.IZERO) GO TO 60
      IF (IFLAG2.EQ.IZERO) GO TO 90
       GO TO 120
C
C     DO THE NU1 EVEN AND NU2 EVEN OR ODD CASE
C
   60 CALL DSUMAL (TERM,IZERO,SUM)
      SUM  = DZERO
      TERM = DONE
      IMAX = (M - ITWO) / ITWO
      IF (IMAX.LE.IZERO) GO TO 80
C
      DO 70 I=1,IMAX
        AI    = I
        COEF1 = DTWO * (AI - DONE)
        COEF2 = DTWO * AI
        TERM  = TERM * FDDIV (ANU2+COEF1, COEF2, IND) * (DONE - Z)
        CALL DSUMAL (TERM,-IONE,SUM)
   70 CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
   80 SUM = SUM + DONE
      SUM = (Z ** FDDIV (ANU2, DTWO, IND)) * SUM
      CDF = DONE - SUM
      RETURN
C
C     ==================================================================
C
C     DO THE NU1 ODD AND NU2 EVEN CASE
C
   90 CALL DSUMAL (TERM,IZERO,SUM)
      SUM  = DZERO
      TERM = DONE
      IMAX = (N - ITWO) / ITWO
      IF (IMAX.LE.IZERO) GO TO 110
C
      DO 100 I=1,IMAX
        AI    = I
        COEF1 = DTWO * (AI - DONE)
        COEF2 = DTWO * AI
        TERM  = TERM * FDDIV (ANU1+COEF1, COEF2, IND) * Z
        CALL DSUMAL (TERM,-IONE,SUM)
 100  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 110  SUM = SUM + DONE
      CDF = ((DONE - Z) ** FDDIV (ANU1, DTWO, IND)) * SUM
      RETURN
C
C     ==================================================================
C
C     DO THE NU1 ODD AND NU2 ODD CASE
C
 120  CALL DSUMAL (TERM,IZERO,SUM)
      SUM   = DZERO
      TERM  = DONE
      ARG   = FDSQRT (FDDIV (ANU1, ANU2, IND) * DX)
      THETA = DATAN (ARG)
      SINTH = FDDIV (ARG, FDSQRT (DONE+ARG*ARG), IND)
      COSTH = FDDIV ( DONE, FDSQRT (DONE+ARG*ARG), IND)
      IF (N.EQ.IONE) GO TO 150
      IF (N.EQ.ITHRE) GO TO 140
      IMAX = N - ITWO
      DO 130 I=3,IMAX,2
        AI    = I
        COEF1 = AI - DONE
        COEF2 = AI
        TERM  = TERM * FDDIV (COEF1, COEF2, IND) * (COSTH * COSTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 130  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
C
 140  SUM = SUM + DONE
      SUM = SUM * SINTH * COSTH
C
 150  A = FDDIV (DTWO, DPI, IND) * (THETA + SUM)
C
      CALL DSUMAL (TERM,IZERO,SUM)
      SUM  = DZERO
      TERM = DONE
      IF (M.EQ.IONE) B = DZERO
      IF (M.EQ.IONE) GO TO 210
      IF (M.EQ.ITHRE) GO TO 170
      IMAX = M - ITHRE
      DO 160 I=1,IMAX,2
        AI    = I
        COEF1 = AI
        COEF2 = AI + DTWO
        TERM  = TERM * FDDIV (ANU2+COEF1, COEF2, IND) * (SINTH * SINTH)
        CALL DSUMAL (TERM,-IONE,SUM)
 160  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
 170  SUM    = SUM + DONE
      SUM    = SUM * SINTH * (COSTH ** N)
      COEF   = DONE
      IEVODD = N - ITWO * (N / ITWO)
      IMIN   = ITHRE
      IF (IEVODD.EQ.IZERO) IMIN = ITWO
      IF (IMIN.GT.N) GO TO 190
      DO 180 I=IMIN,N,2
        AI   = I
        COEF = FDDIV (AI-DONE, AI, IND) * COEF
 180  CONTINUE
C
 190  COEF = COEF * ANU2
      IF (IEVODD.EQ.IZERO) GO TO 200
      COEF = COEF * FDDIV (DTWO, DPI, IND)
C
 200  B = COEF * SUM
C
 210  CDF = A - B
      RETURN
C
C     ==================================================================
C
C     TREAT THE CASE WHEN NU1 AND NU2
C     ARE BOTH LARGE
C     (THAT IS, BOTH ARE EQUAL TO OR LARGER THAN 1000);
C     OR WHEN NU1 IS MODERATE AND NU2 IS LARGE
C     (THAT IS, WHEN NU1 IS EQUAL TO OR GREATER THAN 100
C     BUT SMALLER THAN 1000,
C     AND NU2 IS EQUAL TO OR LARGER THAN 1000);
C     OR WHEN NU2 IS MODERATE AND NU1 IS LARGE
C     (THAT IS WHEN NU2 IS EQUAL TO OR GREATER THAN 100
C     BUT SMALLER THAN 1000,
C     AND NU1 IS EQUAL TO OR LARGER THAN 1000).
C     METHOD UTILIZED--PAULSON APPROXIMATION
C     (SEE AMS 55, PAGE 947, FORMULA 26.6.15).
C
 220  DFACT1 = FDDIV (DONE, 4.5D0*DNU1, IND)
      DFACT2 = FDDIV (DONE, 4.5D0*DNU2, IND)
      DNUM   = ((DONE - DFACT2) * (DX ** DPOW1)) - (DONE - DFACT1)
      DDEN   = FDSQRT ((DFACT2 * (DX ** DPOW2)) + DFACT1)
      U      = FDDIV (DNUM, DDEN, IND)
      CALL NORCDF (U,GCDF)
      CDF = GCDF
      RETURN
C
C     ==================================================================
C
C     TREAT THE CASE WHEN NU1 IS SMALL
C     AND NU2 IS LARGE
C     (THAT IS, WHEN NU1 IS SMALLER THAN 100,
C     AND NU2 IS EQUAL TO OR LARGER THAN 1000).
C     METHOD UTILIZED--SHEFFE-TUKEY APPROXIMATION
C     (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 84, THIRD FORMULA).
C
 230  TERM1 = DNU1
      TERM2 = FDDIV (DNU1, DNU2, IND) * (DHALF * DNU1 - DONE)
      TERM3 = -FDDIV (DNU1, DNU2, IND) * DHALF
      U     = FDDIV (TERM1+TERM2, FDDIV (DONE, DX, IND)-TERM3, IND)
      CALL CHSCDF (U,NU1,CCDF,IND)
      CDF = CCDF
      RETURN
C
C     ==================================================================
C
C     TREAT THE CASE WHEN NU2 IS SMALL
C     AND NU1 IS LARGE
C     (THAT IS, WHEN NU2 IS SMALLER THAN 100,
C     AND NU1 IS EQUAL TO OR LARGER THAN 1000).
C     METHOD UTILIZED--SHEFFE-TUKEY APPROXIMATION
C     (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 84, THIRD FORMULA).
C
 240  TERM1 = DNU2
      TERM2 = FDDIV (DNU2, DNU1, IND) * (DHALF * DNU2 - DONE)
      TERM3 = -FDDIV (DNU2, DNU1, IND) * DHALF
      U     = FDDIV (TERM1+TERM2, DX-TERM3, IND)
      CALL CHSCDF (U,NU2,CCDF,IND)
      CDF = RONE - CCDF
      RETURN
C
C     ==================================================================
C
      END
*FCOS
      REAL             FUNCTION FCOS (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FCOS V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES COS (X) USING LIBRARY FUNCTION COS,
C           IF X IS LESS THAN OR EQUAL TO RTRG, OR
C        CALLS ERROR (104) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS GREATER THAN RTRG.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             X
C
C     ==================================================================
C
      IF (ABS(X).GT.RTRG) GO TO 10
      FCOS = COS (X)
      RETURN
C
C     ..................................................................
C
  10  CALL ERROR (104)
      FCOS = RZERO
      RETURN
C
C     ==================================================================
C
      END
*FDCOS
      DOUBLE PRECISION FUNCTION FDCOS(X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDCOS V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES THE DOUBLE PRECISION COSINE OF X.
C
C     IF THE ARGUMENT, X, EXCEEDS DSNCOS, THE FUNCTION VALUE IS SET
C       EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DCOS, X
C
C     ==================================================================
C
      IF (DABS(X)-DSNCOS) 10,10,20
C
  10  FDCOS = DCOS (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (104)
      FDCOS = DZERO
      RETURN
C
C     ==================================================================
C
      END
*FDDIV
      DOUBLE PRECISION FUNCTION FDDIV (FN,FD,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDDIV V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION PERFORMS DOUBLE PRECISION DIVISION.
C
C     IF THE DENOMINATOR EQUALS ZERO, THE RESULT IS SET EQUAL TO ZERO
C        AND THE INDICATOR, IND, IS SET EQUAL TO ONE.  OTHERWISE
C           IND EQUALS ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION FN, FD
C
C     ==================================================================
C
      IND = IZERO
      IF (FD-DZERO) 10,20,10
C
  10  FDDIV = FN/FD
      RETURN
C
C     ..................................................................
C
  20  FDDIV = DZERO
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*FDEXP
      DOUBLE PRECISION FUNCTION FDEXP (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDEXP V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION EVALUATES THE EXPONENTIAL OF X IN DOUBLE PRECISION.
C
C     IF THE ARGUMENT, X, EXCEEDS DEXP, THE FUNCTION VALUE IS SET EQUAL
C        TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION X, DEXP
C
C     ==================================================================
C
      IF (X-DXEXP) 10,10,20
C
  10  FDEXP = DEXP (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (102)
      FDEXP = DZERO
      RETURN
C
C     ==================================================================
C
      END
*FDIV
      REAL             FUNCTION FDIV (FN,FD,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FDIV V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        DIVIDES FN BY FD USING FORTRAN OPERATOR /,
C           IF X IS NOT EQUAL TO ZERO, OR
C        SETS FAULT INDICATOR EQUAL TO ONE,
C           IF X IS EQUAL TO ZERO.
C
C     FAULT INDICATOR, IND = 0, IF FN IS NOT EQUAL TO ZERO, AND
C                          = 1, IF FN IS     EQUAL TO ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             FN, FD
C
C     ==================================================================
C
      IND = IZERO
      IF (FD.EQ.RZERO) GO TO 10
      FDIV = FN / FD
      RETURN
C
C     ..................................................................
C
  10  FDIV = RZERO
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*FDLOG
      DOUBLE PRECISION FUNCTION FDLOG(X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDLOG V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES THE DOUBLE PRECISION LOG OF X.
C
C     IF THE ARGUMENT, X, IS LESS THAN OR EQUAL TO ZERO, THE FUNCTION
C        VALUE IS SET EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE
C           IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION X, DLOG
C
C     ==================================================================
C
      IF (X-DZERO) 20,20,10
C
  10  FDLOG = DLOG (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (101)
      FDLOG = DZERO
      RETURN
C
C     ==================================================================
C
      END
*FDPCON
      REAL             FUNCTION FDPCON (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDPCON V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     FUNCTION TO CONVERT DOUBLE PRECISION NUMBER TO REAL NUMBER BY
C        OCTAL ROUNDING INSTEAD OF TRUNCATION.
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
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             Y
C
      DOUBLE PRECISION X
      DOUBLE PRECISION XX, D
C
C     ==================================================================
C
      XX = X
      IF (XX.GT.DBLE(RPIFY)) XX = RPIFY
      IF (XX.LT.DBLE(RMIFY)) XX = RMIFY
C
      Y = XX
      D = Y
      FDPCON = XX + (XX-D)
C
      RETURN
C
C     ==================================================================
C
      END
*FDSIN
      DOUBLE PRECISION FUNCTION FDSIN (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDSIN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES THE DOUBLE PRECISION SIN OF X.
C
C     IF THE ARGUMENT, X, IS GREATER THAN DSNCOS, THE FUNCTION VALUE IS
C        SET EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION X, DSIN
C
C     ==================================================================
C
      IF (DABS(X)-DSNCOS) 10,10,20
C
  10  FDSIN = DSIN (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (104)
      FDSIN = DZERO
      RETURN
C
C     ==================================================================
C
      END
*FDSQRT
      DOUBLE PRECISION FUNCTION FDSQRT (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDSQRT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES THE DOUBLE PRECISION SQUARE ROOT OF X.
C
C     IF THE ARGUMENT, X, IS LESS THAN ZERO, THE FUNCTION VALUE IS SET
C        EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION X, DSQRT
C
C     ==================================================================
C
      IF (X-DZERO) 20,30,10
C
  10  FDSQRT = DSQRT (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (101)
  30  FDSQRT = DZERO
      RETURN
C
C     ==================================================================
C
      END
*FEXP
      REAL             FUNCTION FEXP (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FEXP V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES EXP (X) USING LIBRARY FUNCTION EXP,
C           IF X IS LESS THAN OR EQUAL TO REXP, OR
C        CALLS ERROR (102) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS GREATER THAN REXP.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             X
C
C     ==================================================================
C
      IF (X.GT.REXP) GO TO 20
      FEXP = EXP (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (102)
      FEXP = RZERO
      RETURN
C
C     ==================================================================
C
      END
*FEXP2
      REAL             FUNCTION FEXP2 (A,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FEXP2 V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES A**X AND CATCHES EXPONENTIATION ERRORS
C        BEFORE THE SYSTEM DOES.
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
      REAL             A, X
      REAL             RX
      REAL             FEXP, FLOG
C
C     ==================================================================
C
      RX = ABS (X) + RTEN * RER
      RX = SIGN (RX,X)
      IX = RX
      IF (ABS (X-FLOAT(IX)) - RTEN * RER) 10,10,20
  10  IF (IX.LT.IEXP) GO TO 30
  20  FEXP2 = FEXP (X*FLOG(A))
      RETURN
C
C     ..................................................................
C
  30  IF (A) 50,40,50
  40  IF (IX.EQ.IZERO) GO TO 60
      IF (IX.LT.IZERO) GO TO 70
  50  FEXP2 = A**IX
      RETURN
C
C     ..................................................................
C
  60  CALL ERROR (101)
      FEXP2 = RZERO
      RETURN
C
C     ..................................................................
C
  70  FEXP2 = RONE
      RETURN
C
C     ==================================================================
C
      END
*FIT4
      SUBROUTINE FIT4 (PX1,PY1,PX2,PY2,VECX1,VECY1,VECX3,VECY3)
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81.   FIT4 V 7.00  5/18/90. **        
C         
C     ==================================================================        
C         
      COMMON  /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      WRITE (IPRINT,10)
10    FORMAT (55H INSTALLATION MUST PROVIDE PROPRIETARY FIT4 SUBROUTINE.
     1)
C
      PX1 = PX1
      PY1 = PY1
      PX2 = PX2
      PY2 = PY2
      VECX1 = VECX1
      VECY1 = VECY1
      VECX3 = VECX3
      VECY3 = VECY3
C
      RETURN
      END
*FIXFLO   
      SUBROUTINE FIXFLO       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. FIXFLO V 7.00  1/11/89. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C               L2 =  3 FOR FIXED       
C               L2 =  4 FOR FLOATING    
C               L2 = 12 FOR FLEXIBLE    
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
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER   IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER   LLA(14)*1     
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA ICA / 12 /         
C         
      DATA LLA(1), LLA(2), LLA(3), LLA(4), LLA(5), LLA(6), LLA(7),    
     1     LLA(8), LLA(9),LLA(10),LLA(11),LLA(12),LLA(13),LLA(14)     
     2/       '0',    '1',    '2',    '3',    '4',    '5',     '6',   
     3        '7',    '8',    '9',    ' ',    'E',    'F',     'P'/   
C         
C     ==================================================================        
C         
      IF (L2.NE.ICA) GO TO 10 
      IOSWT = IZERO 
      RETURN        
C         
C     ..................................................................        
C         
  10  IF (L2.NE.IFOUR .OR. NARGS.NE.IZERO)  GO TO 20        
      I = ISIGD-IONE
      GO TO 50      
C         
  20  IF (NARGS.EQ.IONE) GO TO 30       
      CALL ERROR (10)         
      RETURN        
C         
C     ..................................................................        
C         
  30  IF (KIND(1).EQ.IZERO) GO TO 40    
      CALL ERROR (20)         
      RETURN        
C         
C     ..................................................................        
C         
  40  I = IARGS(1) - IDIV (L2,IFOUR,IND)
      IF (I.GE.IZERO .AND. I.LE.ISIGD) GO TO 50   
      I = ISIGD     
      CALL ERROR (237)        
C         
  50  IOSWT = IONE  
      IF (I.EQ.IZERO) GO TO 70
      INA = IDIV (I,ITEN,IND) 
      IF (INA.EQ.IZERO) INA = ITEN      
      INB = MOD (I,ITEN)      
      IFMTX(10) = LLA(INA+1)  
      IFMTX(11) = LLA(INB+1)  
      IF (L2.EQ.IFOUR) GO TO 60         
C         
C     SET UP FIXED FORMAT     
C         
      IFMTX(2) = LLA(1)       
      IFMTX(6) = LLA(13)      
      RETURN        
C         
C     SET UP FLOATING FORMAT  
C         
  60  IFMTX(2) = LLA(2)       
      IFMTX(3) = LLA(14)      
      IFMTX(6) = LLA(12)      
      RETURN        
C         
C     ..................................................................        
C         
  70  IOSWT = ITWO  
      RETURN        
C         
C     ==================================================================        
C         
      END 
*FLINE
      SUBROUTINE FLINE (X,Y,N,I,L,M)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FLINE V 7.00  5/18/90. **
C **  MODIFIED JULY 1992.  CALCOMP NOW CALLS THIS ROUTINE "LINE".  
C **  SIMPLY CALL LINE FOR CALCOMP, CALL LINET FOR OMNITAB CALCOMP
C **  COMPATIBLE (WHICH IS AN EMULATION ROUTINE).
C
C     ==================================================================
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      IF(ITEKSW.EQ.IZERO)THEN
        CALL LINE(X,Y,N,I,L,M)
      ELSE
        CALL LINET(X,Y,N,I,L,M)
      ENDIF
C
      RETURN
      END
*FLOG
      REAL             FUNCTION FLOG (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FLOG V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES NATURAL LOGARITHM OF X USING LIBRARY FUNCTION ALOG,
C           IF X IS POSITIVE, OR
C        CALLS ERROR (101) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS NONPOSITIVE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X
C
C     ==================================================================
C
      IF (X.GT.RZERO) GO TO 10
      CALL ERROR (101)
      FLOG = RZERO
      RETURN
C
C     ..................................................................
C
  10  FLOG = ALOG (X)
      RETURN
C
C     ==================================================================
C
      END
*FLOG10
      REAL             FUNCTION FLOG10 (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FLOG10 V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES LOG TO BASE 10 OF X USING LIBRARY FUNCTION ALOG10,
C           IF X IS POSITIVE, OR
C        CALLS ERROR (101) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS NONPOSITIVE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X
C
C     ==================================================================
C
      IF (X.GT.RZERO) GO TO 20
      CALL ERROR (101)
      FLOG10 = RZERO
      RETURN
C
C     ..................................................................
C
  20  FLOG10 = ALOG10 (X)
      RETURN
C
C     ==================================================================
C
      END
*FNUALF
      SUBROUTINE FNUALF (NU,ALPHA,FALPHA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FNUALF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTES F OF ALPHA WITH 2 AND NU DEGREES OF FREEDOM.
C
C     FALPHA IS THE VALUE OF THE F-DISTRIBUTION, WITH 2 AND NU DEGREES
C        OF FREEDOM, SUCH THAT THE AREA TO THE RIGHT OF FALPHA IS ALPHA.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             ALPHA, FALPHA
      REAL             FNU, FNUHAF, FNU2, TEMP
      REAL             FDIV
C
C     ==================================================================
C
      FNU = FLOAT (NU)
      FNU2 = FDIV (FNU,RTWO,IND)
      FNUHAF = FDIV (RTWO,FNU,IND)
      TEMP = ALPHA**(-FNUHAF) - RONE
      FALPHA = FNU2 * TEMP
      RETURN
C
C     ==================================================================
C
      END
*FPPF
      SUBROUTINE FPPF (P, NU1, NU2, PPF, IND)
C
C **  NBS OMNITAB 1980 VERSION 6.04 11/14/84.   FPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FOR THE F DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM
C              PARAMETERS = NU1 AND NU2.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --NU1    = THE INTEGER DEGREES OF FREEDOM
C                                FOR THE NUMERATOR OF THE F RATIO.
C                                NU1 SHOULD BE POSITIVE.
C                     --NU2    = THE INTEGER DEGREES OF FREEDOM
C                                FOR THE DENOMINATOR OF THE F RATIO.
C                                NU2 SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF FOR THE F DISTRIBUTION
C             WITH DEGREES OF FREEDOM
C             PARAMETERS = NU1 AND NU2.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN
C                   0.0 (INCLUSIVELY) AND 1.0 (EXCLUSIVELY).
C                 --NU1 SHOULD BE A POSITIVE INTEGER VARIABLE.
C                 --NU2 SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--FCDF, NORCDF, NORFPPF, CHSCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGES 946-947,
C                 FORMULAE 26.6.4, 26.6.5, 26.6.8, AND 26.6.15.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGE 83, FORMULA 20,
C                 AND PAGE 84, THIRD FORMULA.
C               --PAULSON, AN APPROXIMATE NORMAILIZATION
C                 OF THE ANALYSIS OF VARIANCE DISTRIBUTION,
C                 ANNALS OF MATHEMATICAL STATISTICS, 1942,
C                 NUMBER 13, PAGES 233-135.
C               --SCHEFFE AND TUKEY, A FORMULA FOR SAMPLE SIZES
C                 FOR POPULATION TOLERANCE LIMITS, 1944,
C                 NUMBER 15, PAGE 217.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2845
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MAY       1978.
C     UPDATED         --AUGUST    1979.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C               ADAPTED TO OMNITAB BY -
C                      SALLY PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1984.
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
C     ==================================================================
C
      TOL   = RER
      MAXIT = IHRD
      XMIN  = RZERO
      XMAX  = RPIFY
      XLOW  = XMIN
      XUP   = XMAX
C
      ANU1 = NU1
      ANU2 = NU2
C
      ANUM2 = FDIV (RONE,ANU2,IND)
      ANUM1 = FDIV (RONE,ANU1,IND)
      EXPF  = RHALF * (ANUM2 - ANUM1)
      SDF   = FSQRT (RHALF * (ANUM2 + ANUM1))
      CALL NORPPF(P,ZN,IND)
      XN   = EXPF + ZN * SDF
      XMID = FEXP (RTWO * XN)
C
      IF (P.NE.RZERO) GO TO 10
      PPF = XMIN
      RETURN
C
C     ==================================================================
C
  10  DO 70 I = 1,MAXIT
        X = XMID
        CALL FCDF (X,NU1,NU2,PCALC)
        IF (PCALC.EQ.P) GO TO 80
        IF (PCALC.GT.P) GO TO 40
C
  20    XLOW = XMID
        X    = XMID * RTWO
        IF (X.GE.XUP) GO TO 30
        XMID = X
        CALL FCDF (X,NU1,NU2,PCALC)
        IF (PCALC.EQ.P) GO TO 80
        IF (PCALC.LT.P) GO TO  20
        XUP  = X
  30    XMID = FDIV (XLOW+XUP,RTWO,IND)
        GO TO 60
C
  40    XUP = XMID
        X   = FDIV (XMID,RTWO,IND)
        IF (X.LE.XLOW) GO TO 50
        XMID = X
        CALL FCDF (X,NU1,NU2,PCALC)
        IF (PCALC.EQ.P) GO TO 80
        IF (PCALC.GT.P) GO TO 40
        XLOW = X
  50    XMID = FDIV (XLOW + XUP,RTWO,IND)
  60    XDEL = ABS (XMID - XLOW)
        IF (XDEL.LT.TOL) GO TO 80
  70  CONTINUE
C
  80  PPF = XMID
      RETURN
C
C     ==================================================================
C
      END
*FPPT
      SUBROUTINE FPPT (V11,V12,P10,XA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FPPT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE 5 PERCENT CRITICAL VALUE OF F-DISTRIBUTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             P10, V11, V12, XA
      REAL             P(5), X(5)
      REAL             H, P0, Q, V1, V2, W, XLMBDA, XMAX, XMIN, X0
      REAL             Y, YP, Z, ZZ
      REAL             TOLERA, TOLERB
      REAL             FDIV, FEXP, FSQRT
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF
      REAL             SPCG, SPCH, SPCI
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /   1.5      /
      DATA SPCB / 225.0      /
      DATA SPCC /  -4.91     /
      DATA SPCD /   0.14     /
      DATA SPCE /   6.0      /
      DATA SPCF /   0.666667 /
      DATA SPCG /   0.833333 /
      DATA SPCH /   1.9999   /
      DATA SPCI /   0.5001   /
C
C     ==================================================================
C
      TOLERA = RFIVE * RTEN * RER
      TOLERB = RTEN * TOLERA
      V1 = V11
      V2 = V12
      P0 = P10
C
C     CALLS QFORF AND THEN USES ISOLATE METHOD FOR SOLVING ITERATIVELY.
C
      IF (V1-SPCA) 10,10,20
C
C     USE STUDENT'S T
C     ONLY GOOD FOR P0 = 0.05
C
  10  CALL TPCTPT (V2,XA)
      XA = XA ** RTWO
      GO TO 70
C
  20  IF (V2-SPCA) 30,30,40
C
C     ONLY GOOD FOR P0 = 0.05
C
C     SHOULD USE STUDENT'S T
C
  30  XA = SPCB
      GO TO 70
C
C     TUKEY APPROXIMATION TO NORMAL PERCENT POINT.
C
  40  YP = SPCC * (P0**SPCD - (RONE-P0)**SPCD)
C
C     AMS 55 APPROXIMATION 26.5.22
C
      H = FDIV (RTWO,(FDIV(RONE,V1-RONE,IND) +
     1     FDIV (RONE,V2-RONE,IND)),IND)
      XLMBDA = FDIV (YP**2-RTHRE,SPCE,IND)
      W = FDIV (YP*FSQRT(H+XLMBDA),H,IND)
      IF (V1-V2) 50,60,50
  50  Y = FDIV (RONE,V1-RONE,IND)
      Z = FDIV (RONE,V2-RONE,IND)
      ZZ = FDIV (SPCF,H,IND)
      W = W - (Y-Z) * (XLMBDA+SPCG-ZZ)
C
C     AMS 55 APPROXIMATION 26.6.16
C
  60  XA = FEXP (RTWO*W)
  70  XMIN = RHALF * XA
      XMAX = RTWO * XA
      CALL QFORF (V1,V2,XMAX,Q)
      IF (Q.LE.P0) GO TO 80
      XA = SPCH * XMAX
      GO TO 70
C
  80  CALL QFORF (V1,V2,XMIN,Q)
      IF (P0.LE.Q) GO TO 90
      XA = SPCI * XMIN
      GO TO 70
C
  90  X0 = XA
      DO 140 I=1,5
        X(I) = XMIN + FLOAT(I-IONE) * FDIV (XMAX-XMIN,RFOR,IND)
        CALL QFORF (V1,V2,X(I),P(I))
        IF (P0-P(I)) 140,130,120
 120    XMAX = X(I)
        XMIN = X(I-1)
        GO TO 150
 130    XA = X(I)
        RETURN
 140  CONTINUE
 150  XA = FDIV (XMIN+XMAX,RTWO,IND)
C
C     EXIT IF EITHER TOLERANCE IS SATISFIED * ABSOLUTE 5E-6, REL. 5E-7
C
      IF (ABS(X0-XA).GT.TOLERB .AND. FDIV(ABS(X0-XA),XA,IND).GT.TOLERA)
     1     GO TO 90
      RETURN
C
C     ==================================================================
C
      END
*FRENCH
      SUBROUTINE FRENCH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FRENCH V 7.00  4/ 5/90. **
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
      DATA L( 13) /   84107110 /
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
      DATA L( 30) /  127600000 /
      DATA L( 31) /  127603024 /
      DATA L( 32) /  127605832 /
      DATA L( 33) /  128409723 /
      DATA L( 34) /  128701126 /
      DATA L( 35) /  132813156 /
      DATA L( 36) /  143613527 /
      DATA L( 37) /  260509626 /
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
      DATA L( 88) /  305706786 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  305813153 /
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
      DATA L(102) /  381300815 /
      DATA L(103) /  414904230 /
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
      DATA L(114) /  260616187 /
      DATA L(115) / 1207810752 /
      DATA L(116) /  470317741 /
      DATA L(117) /  241212965 /
      DATA L(118) / 1207810759 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) / 1216503492 /
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
      DATA L(138) /  130309626 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) /  708908901 /
      DATA L(147) / 1326614733 /
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
      DATA L(164) /  912600000 /
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
      DATA L(211) / 1142511318 /
      DATA L(212) / 1170912165 /
      DATA L(213) / 1170914431 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1327304190 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) /  559011889 /
      DATA L(220) / 1208118396 /
      DATA L(221) / 1216503486 /
      DATA L(222) /  132410292 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  397416191 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1327304190 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327614843 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1354810314 /
      DATA L(236) /  899810734 /
      DATA L(237) / 1389413543 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1398713211 /
      DATA L(243) / 1399803746 /
      DATA L(244) / 1400201216 /
      DATA L(245) / 1208804131 /
      DATA L(246) / 1402214580 /
      DATA L(247) / 1315202613 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410802916 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1426816173 /
      DATA L(255) / 1427004267 /
      DATA L(256) / 1427414580 /
      DATA L(257) /  695904153 /
      DATA L(258) / 1429809429 /
      DATA L(259) / 1432814580 /
      DATA L(260) / 1433101220 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439207094 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1427714409 /
      DATA L(268) / 1443100000 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1476900000 /
      DATA L(271) / 1477300000 /
      DATA L(272) / 1477700000 /
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
     1      11300,   7102,  14733,   9734,  14406,  11664,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,  13257,   4797,   9524,   6928,  13276 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1      14735,   3645,  13282,  13281,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) /  464104131,  692813276 /
      DATA LD( 3), LD( 4) / 1181704797,  900903645 /
      DATA LD( 5), LD( 6) /   80103839,  117906959 /
      DATA LD( 7), LD( 8) /  992706959, 1065606959 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014715 /
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
      DATA LW(16,1), LW(16,2) /  899810206, 1426909612 /
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
      DATA LP( 5)         /  559011889             /
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
     1        'C',    'O',    'L',    'U',    'M',    'N',    ' '/
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
*FREQCY
      SUBROUTINE FREQCY (X,F,N,K,C,NSTART,START,LIMITS,XL,XU)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FREQCY V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE TO CONSTRUCT FREQUENCY DISTRIBUTION IN VECTOR F FOR
C     VECTOR OF OBSERVATIONS X OF LENGTH N USING K CELLS OF LENGTH C.
C
C     IF C=0.0, THEN C IS DETERMINED BY SUBROUTINE. IF BOTH K AND C=0.0,
C     THEN BOTH K AND C ARE DETERMINED BY THE SUBROUTINE.
C
C     IF NSTART=1, START DETERMINED.  IF NSTART=0, START IS GIVEN.
C
C     IF LIMITS=1, LOWER CELL BOUNDARIES ARE PUT IN XL AND UPPER IN XU.
C
C     IF  C IS NEGATIVE, C IS SET  = ABS(C)
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             F(*), X(*), XL(*), XU(*)
      REAL             C, START
      REAL             RANGE, XMAX, XMIN
      REAL             FDIV, FLOG10
      REAL             SPCA, SPCB
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 1.5 /
      DATA SPCB / 3.3 /
C
C     ==================================================================
C
      IF (K.GT.IZERO) GO TO 10
      K = SPCA + SPCB * FLOG10 (FLOAT(N))
      K = MAX0 (K,IFIVE)
  10  XMIN = X(1)
      XMAX = X(1)
      DO 20 I=1,N
        IF (X(I).LT.XMIN) XMIN = X(I)
        IF (X(I).GT.XMAX) XMAX = X(I)
  20  CONTINUE
C
      IF (C) 40,30,40
  30  RANGE = XMAX - XMIN
      C = FDIV (RANGE,FLOAT(K-IONE),IND)
      IF (K.EQ.IONE)  C = RANGE
  40  C = ABS(C)
      IF (NSTART.EQ.IZERO)  GO TO 50
      START = XMIN - RHALF*C
      IF (K.EQ.IONE)  START = XMIN
  50  DO 60 I=1,K
        F(I) = RZERO
  60  CONTINUE
C
      DO 70 I=1,N
        J = FDIV (X(I)-START,C,IND) + RONE
        IF (J.LE.IZERO .OR. J.GT.K)  GO TO 70
        F(J) = F(J) + RONE
  70  CONTINUE
C
      IF (LIMITS.EQ.IZERO) RETURN
      XL(1) = START
      XU(1) = XL(1) + C
      DO 90 I=2,K
        XL(I) = XL(I-1) + C
        XU(I) = XL(I) + C
  90  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*FSIN
      REAL             FUNCTION FSIN (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FSIN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES SIN (X) USING LIBRARY FUNCTION SIN,
C           IF X IS LESS THAN OR EQUAL TO RTRG, OR
C        CALLS ERROR (104) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS GREATER THAN RTRG.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             X
C
C     ==================================================================
C
      IF (ABS(X).GT.RTRG) GO TO 20
      FSIN = SIN (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (104)
      FSIN = RZERO
      RETURN
C
C     ==================================================================
C
      END
*FSQRT
      REAL             FUNCTION FSQRT (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FSQRT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES SQUARE ROOT OF X USING LIBRARY FUNCTION SQRT,
C           IF X IS NONNEGATIVE, OR
C        CALLS ERROR (101) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS NEGATIVE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X
C
C     ==================================================================
C
      IF (X.LT.RZERO) GO TO 20
      FSQRT = SQRT (X)
      RETURN
C
C     ..................................................................
C
  20  CALL ERROR (101)
      FSQRT = RZERO
      RETURN
C
C     ==================================================================
C
      END
*FTANH
      REAL             FUNCTION FTANH (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FTANH V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES HYPERBOLIC TANGENT OF X USING LIBRARY FUNCTION TANH,
C           IF 2X IS LESS THAN OR EQUAL TO REXP, OR
C        SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF 2X IS GREATER THAN REXP.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             X
C
C     ==================================================================
C
      IF (ABS(RTWO*X).LE.REXP) GO TO 20
      FTANH = RZERO
      RETURN
C
C     ..................................................................
C
  20  FTANH = TANH (X)
      RETURN
C
C     ==================================================================
C
      END
*GAMCDF
      SUBROUTINE GAMCDF (X,GAMMA,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GAMCDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE GAMMA
C              DISTRIBUTION WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE GAMMA DISTRIBUTION USED
C              HEREIN HAS MEAN = GAMMA
C              AND STANDARD DEVIATION = SQRT(GAMMA).
C              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE
C                                AT WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE POSITIVE.
C                     --GAMMA  = THE SINGLE PRECISION VALUE
C                                OF THE TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C                 --X SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     ACCURACY--(ON THE UNIVAC 1108, EXEC 8 SYSTEM AT NBS)
C               COMPARED TO THE KNOWN GAMMA = 1 (EXPONENTIAL)
C               RESULTS, AGREEMENT WAS HAD OUT TO 7 SIGNIFICANT
C               DIGITS FOR ALL TESTED X.
C               THE TESTED X VALUES COVERED THE ENTIRE
C               RANGE OF THE DISTRIBUTION--FROM THE 0.00001
C               PERCENT POINT UP TO THE 99.99999 PERCENT POINT
C               OF THE DISTRIBUTION.
C
C     REFERENCES--WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15,
C                 ESPECIALLY PAGES 3-5.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 68-73.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, GAMMA, CDF
      REAL             FDPCON
C
      DOUBLE PRECISION D(10), DTERM(9)
      DOUBLE PRECISION DX, DGAMMA, AI, TERM, SUM, CUT1, CUT2, CUTOFF, T
      DOUBLE PRECISION Z, Z2, Z3, Z4, Z5, DEN, A, B, C, G
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DPCA, DPCB
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXIT / 10000 /
C
      DATA C     /  0.918938533204672741D0  /
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
      DATA D(10) / -0.139243221600590111D1  /
C
      DATA DPCA  / 10000000000.0D0          /
      DATA DPCB  /          10.0D0          /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (X.GT.RZERO) GO TO 10
        IND = IONE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
  10  IF (GAMMA.GT.RZERO) GO TO 20
        IND = ITWO
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  20  DX = X
      DGAMMA = GAMMA
C
C     COMPUTE THE GAMMA FUNCTION USING THE ALGORITHM IN THE
C     NBS APPLIED MATHEMATICS SERIES REFERENCE.
C
      Z = DGAMMA
      DEN = DONE
  30  IF (Z.GE.DPCB) GO TO 40
      DEN = DEN * Z
      Z = Z + DONE
      GO TO 30
C
  40  Z2       = Z * Z
      Z3       = Z * Z2
      Z4       = Z2 * Z2
      Z5       = Z2 * Z3
      A        = (Z-DHALF) * FDLOG (Z) - Z + C
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
C     COMPUTE T-SUB-Q AS DEFINED ON PAGE 4 OF THE WILK, GNANADESIKAN,
C     AND HUYETT REFERENCE
C
      SUM =  FDDIV (DONE,DGAMMA,JIND)
      TERM =  FDDIV (DONE,DGAMMA,JIND)
      CUT1 = DX - DGAMMA
      CUT2 = DX * DPCA
      DO 50 I=1,MAXIT
        AI = I
        TERM =  FDDIV (DX*TERM,DGAMMA+AI,JIND)
        SUM = SUM + TERM
        CUTOFF = CUT1 + FDDIV (CUT2*TERM,SUM,JIND)
        IF (AI.GT.CUTOFF) GO TO 60
  50  CONTINUE
C
      IND  =  ITHRE
      CDF = RONE
      RETURN
C
C     ..................................................................
C
  60  T = SUM
      CDF = FDPCON ( (DX**DGAMMA) * (FDEXP(-DX)) * FDDIV(T,G,JIND) )
C
      RETURN
C
C     ==================================================================
C
      END
*GAMPPF
      SUBROUTINE GAMPPF (P,GAMMA,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GAMPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GAMMA DISTRIBUTION
C              WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE GAMMA DISTRIBUTION USED
C              HEREIN HAS MEAN = GAMMA
C              AND STANDARD DEVIATION = SQRT(GAMMA).
C              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (EXCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
C             VALUE PPF FOR THE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     ACCURACY--(ON THE UNIVAC 1108, EXEC 8 SYSTEM AT NBS)
C               COMPARED TO THE KNOWN GAMMA = 1 (EXPONENTIAL)
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
C               --NATIONAL BUREAU  OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 68-73.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--NOVEMBER  1974.
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
      REAL             FDPCON
C
      DOUBLE PRECISION D(10), DTERM(9)
      DOUBLE PRECISION DP, DGAMMA
      DOUBLE PRECISION Z, Z2, Z3, Z4, Z5, DEN, A, B, C, G
      DOUBLE PRECISION XMIN0, XMIN, AI, XMAX, DX, PCALC, XMID
      DOUBLE PRECISION XLOWER, XUPPER, XDEL
      DOUBLE PRECISION SUM, TERM, CUT1, CUT2, AJ, CUTOFF, T
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
      DATA ICA   / 30000 /
C
      DATA C     /  0.918938533204672741D0  /
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
      DATA DPCA  / 10.0D0   /
      DATA DPCB  /  1.0D-10 /
      DATA DPCC  /  1.0D+10 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
      IND = IONE
      PPF = RZERO
      RETURN
C
C     ..................................................................
C
  10  IF (GAMMA.GT.RZERO) GO TO 20
      IND = ITWO
      PPF = RZERO
      RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  20  DP     = P
      DGAMMA = GAMMA
      XUPPER = DZERO
      XLOWER = DZERO
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
  30  IF (Z.GE.DPCA) GO TO 40
      DEN = DEN * Z
      Z   = Z + DONE
      GO TO 30
C
  40  Z2       = Z * Z
      Z3       = Z * Z2
      Z4       = Z2 * Z2
      Z5       = Z2 * Z3
      A        = (Z-DHALF) * FDLOG (Z) - Z + C
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
  50  AI   = ICOUNT
      XMAX = AI * XMIN0
      DX   = XMAX
      GO TO 130
C
  60  IF (PCALC.GE.DP) GO TO 70
      XMIN   = XMAX
      ICOUNT = ICOUNT+IONE
      IF (ICOUNT.LE.ICA) GO TO 50
  70  XMID   = FDDIV (XMIN+XMAX,DTWO,JIND)
C
C     NOW ITERATE BY BISECTION UNTIL THE DESIRED ACCURACY IS ACHIEVED.
C
      ILOOP  = ITWO
      XLOWER = XMIN
      XUPPER = XMAX
      ICOUNT = IZERO
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
      IF (XDEL.LT.DPCB .OR. ICOUNT.GT.IHRD) GO TO 120
      GO TO 80
C
 120  PPF = FDPCON (XMID)
      RETURN
C
C     ..................................................................
C
 130  SUM  = FDDIV (DONE,DGAMMA,JIND)
      TERM = FDDIV (DONE,DGAMMA,JIND)
      CUT1 = DX - DGAMMA
      CUT2 = DX * DPCC
      DO 140 J=1,MAXIT
        AJ     = J
        TERM   = DX * FDDIV (TERM,DGAMMA+AJ,JIND)
        SUM    = SUM + TERM
        CUTOFF = CUT1 + FDDIV (CUT2*TERM,SUM,JIND)
        IF (AJ.GT.CUTOFF) GO TO 150
 140  CONTINUE
      IND = IFOUR
      PPF = RZERO
      RETURN
C
C     ..................................................................
C
 150  T     = SUM
      PCALC = (DX**DGAMMA) * (FDEXP(-DX)) * FDDIV (T,G,JIND)
      IF (ILOOP.EQ.IONE) GO TO 60
      GO TO 90
C
C     ==================================================================
C
      END
*GAMRAN
      SUBROUTINE GAMRAN (IRAN,KRAN,N,GAMMA,X,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GAMRAN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     GENERATE N GAMMA RANDOM NUMBERS WITH PARAMETER GAMMA IN X(.).
C
C     WRITTEN BY GEORGE MARSAGLIA.
C
C        OBTAINED IN 1975 FROM MCGILL UNIVERSITY.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X(*)
      REAL             GAMMA
      REAL             Y(1)
      REAL             CC, CD, CL, CS, E
      REAL             PARAM, RGAMMA, RSQRT3, RTHIRD
      REAL             S, T, UNIRAN, Z, ZO
      REAL             FDIV, FLOG, FSQRT
C
C     ==================================================================
C
      IND = IZERO
      IF (GAMMA.GT.RZERO) GO TO 10
        IND = IONE
        RETURN
C
C     ..................................................................
C
  10  PARAM  = GAMMA
      RSQRT3 = FSQRT (RTHRE)
      RTHIRD = FDIV (RONE,RTHRE,JIND)
C
      S  = FDIV (RTHIRD,FSQRT(PARAM),JIND)
      ZO = RONE - RSQRT3 * S
      CC = PARAM * ZO**3 - RHALF * (S-RSQRT3)**2
      CL = RTHRE * PARAM - RONE
      CS = RONE - S * S
C
      DO 40 I=1,N
  20    CALL NORRAN (IRAN,KRAN,1,Y)
        Z  = S * Y(1) + CS
        IF (Z.LE.RZERO) GO TO 20
        RGAMMA = PARAM * Z ** 3
        E  = - FLOG (UNIRAN(IRAN,KRAN,IZERO))
        CD = E + RHALF * Y(1) ** 2 - RGAMMA + CC
        T  = RONE - FDIV (ZO,Z,JIND)
        IF (CD+CL*T*(RONE+T*(RHALF+RTHIRD*T)).GT.RZERO) GO TO 30
        IF (CD+CL*FLOG(FDIV(Z,ZO,JIND)).LT.RZERO) GO TO 20
  30    X(I) = RGAMMA
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*GEORAN
      SUBROUTINE GEORAN (IRAN,KRAN,N,P,X,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GEORAN V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     GENERATE N GEOMETRIC RANDOM NUMBERS FOR PARAMETER P.
C
C        USES THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
C     BASED ON DATAPAC PROGRAM UNIT GEORAN WRITTEN BY JAMES J. FILLIBEN.
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
      REAL             P
      REAL             ADEN, ANUM, ARATIO, ARG1, ARG2, RATIO, UNIRAN
      REAL             FDIV, FLOG
C
C     ==================================================================
C
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
        IND = IONE
        RETURN
C
C     ..................................................................
C
  10  DO 20 I=1,N
        X(I)   = UNIRAN (IRAN,KRAN,IZERO)
        IF (X(I).EQ.RZERO) GO TO 20
        ARG1   = RONE - X(I)
        ARG2   = RONE - P
        ANUM   = FLOG (ARG1)
        ADEN   = FLOG (ARG2)
        RATIO  = FDIV (ANUM,ADEN,JIND)
        IRATIO = RATIO
        X(I)   = IRATIO
        ARATIO = IRATIO
        IF (ARATIO.EQ.RATIO) X(I) = IRATIO - IONE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*GERMAN
      SUBROUTINE GERMAN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. GERMAN V 7.00  4/ 5/90. **
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
      DATA L( 13) /   84106714 /
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
      DATA L( 30) /  127600000 /
      DATA L( 31) /  127602916 /
      DATA L( 32) /  127605832 /
      DATA L( 33) /  128409723 /
      DATA L( 34) /  128701126 /
      DATA L( 35) /  132813156 /
      DATA L( 36) /  143613527 /
      DATA L( 37) /  111301114 /
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
      DATA L( 61) / 1619100878 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255909297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) / 1619105432 /
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
      DATA L( 88) /  305706948 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1619113940 /
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
      DATA L(102) / 1440903891 /
      DATA L(103) /  451708901 /
      DATA L(104) /  424009316 /
      DATA L(105) /  131514628 /
      DATA L(106) /  429802774 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /  111111710 /
      DATA L(115) /  464112245 /
      DATA L(116) /  470317741 /
      DATA L(117) /  470711664 /
      DATA L(118) /  543207106 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  415704219 /
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
      DATA L(138) /  413911078 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) /  708908795 /
      DATA L(147) /  710613370 /
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
      DATA L(164) /  916010249 /
      DATA L(165) /  916019097 /
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
      DATA L(219) /  130215067 /
      DATA L(220) / 1208118396 /
      DATA L(221) / 1216503486 /
      DATA L(222) / 1216509902 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1208904049 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1701503069 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327615003 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1354810314 /
      DATA L(236) / 1326605981 /
      DATA L(237) / 1389413543 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1398713211 /
      DATA L(243) / 1399803746 /
      DATA L(244) / 1400201216 /
      DATA L(245) / 1400618954 /
      DATA L(246) / 1402214580 /
      DATA L(247) / 1408213667 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410802916 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) /  130209158 /
      DATA L(255) / 1427004267 /
      DATA L(256) / 1427414580 /
      DATA L(257) / 1958413940 /
      DATA L(258) / 1429809429 /
      DATA L(259) / 1432814580 /
      DATA L(260) / 1296103411 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
      DATA L(268) / 1443109725 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1476900000 /
      DATA L(271) / 1477300000 /
      DATA L(272) / 1477700000 /
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
     1      11300,   7102,   4631,   7082,   5871,  14580,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,   3969,   4797,   9524,   1315,   3423 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       8440,   4374,   3994,   3993,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1327604185,  131503423 /
      DATA LD( 3), LD( 4) /  915605981,  890204023 /
      DATA LD( 5), LD( 6) /   79415462,  117906959 /
      DATA LD( 7), LD( 8) /  117906959,  117906959 /
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
      DATA LW(16,1), LW(16,2) / 1326605981, 1443109612 /
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
      DATA LP( 5)         /  130215067             /
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
     1        'S',    'P',    'A',    'L',    'T',    'E',    ' '/
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
*GOZIP
      SUBROUTINE GOZIP
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81.  GOZIP V 7.00  5/18/90. **        
C         
C     ==================================================================        
C         
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      WRITE (IPRINT,10)
10    FORMAT (63H INSTALLATION MUST PROVIDE PROPRIETARY CALCOMP PLOT SUB
     1ROUTINE.)
      RETURN
      END
*HEADS    
      SUBROUTINE HEADS (LOC,NOO,IN,IO)  
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81.  HEADS V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     INSERT COLUMN HEADINGS (IF AVAILABLE) WHEN NO FORMAT IS SPECIFIED.        
C        WHEN NO FORMAT IS SPECIFIED.   
C         
C     LOC  LOCATION WHERE COL NUMBERS ARE         
C     NOO  NO OF COLUMN HEADINGS TO LOOK FOR. NOO LESS THAN OR = 8.   
C     IN   IF IN = 0, NEW HEADINGS      
C          IF IN = 1, PRINT OUT HEADINGS FROM RREVIOUS PAGE 
C         
C     IF A HEADING EXISTS THE 12 CHARACTER  HEADING WILL BE PRINTED.  
C     OTHERWISE THE HEADING COLUMN XXXX IS TO BE USED WHERE XXXX IS THE         
C     NUMBER CONVERTED FOR DECIMAL PRINTOUT. THE HEADINGS ARE PRINTED 
C     OVER THE DATA WHICH IS IF FORMAT 1P8E15.6.  
C         
C     IO      = 0  PRINT  HEADINGS      
C     IO  NOT = 0  DO NOT PRINT HEADINGS
C         
C               REVISED BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING ANDAPPLIED MATHEMATICS LABORATORY,      
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
      DIMENSION LOC(*), LH(10)       
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARRYBC/ ICOLHD(7)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE         
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /TPRNTC/ LHEAD(96)
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      CHARACTER LA*1
      CHARACTER ICOLHD*1
      CHARACTER LFMTP*80      
      CHARACTER LHEAD*1
          
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA ICA /    8 /       
      DATA ICB /   15 /       
      DATA ICC /   12 /       
      DATA ICD / 1000 /       
C         
C     ==================================================================        
C         
      NO = NOO      
      IF (NO.GT.ICA) NO = ICA 
      IF (IPLACE.LT.ICB) RETURN         
      IF (IN.NE.IZERO) GO TO 80         
      IR = IONE     
      DO 70 I=1,NO  
        II = I      
        CALL PREPAK (ITHRE,II,LOC(I),LH,LHEAD(IR),LFMTP,IND)   
        IF (IND.NE.IZERO) GO TO 10      
        IR = IR + ICC         
        GO TO 70    
  10    DO 20 IS=1,7
          LHEAD(IR) = ICOLHD(IS)        
          IR = IR + IONE      
  20    CONTINUE    
        K = LOC(I)  
        KC = ICD    
        KD = IZERO  
        DO 60 IS=1,4
          KA = IDIV (K,KC,JND)
          IF (KA.NE.IZERO) GO TO 30     
          IF (KD.NE.IZERO) GO TO 40     
          LHEAD(IR) = LA(45)  
          GO TO 50  
  30      KD = IONE 
  40      KAP = KA + IONE     
          LHEAD(IR) = LA(KAP) 
  50      IR = IR + IONE      
          K = K - KA * KC     
          KC = IDIV (KC,ITEN,JND)       
  60    CONTINUE    
        LHEAD(IR) = LA(45)    
        IR = IR + IONE        
  70  CONTINUE      
  80  IF (IO.NE.IZERO) RETURN 
      IS = NO * ICC 
      WRITE (IPRINT,90) (LHEAD(I),I=1,IS)         
      RETURN        
C         
C     ==================================================================        
C         
C                          ***   FORMAT STATEMENTS   ***    
C         
  90  FORMAT (8(3X,12A1))     
C         
C     ==================================================================        
C         
      END 
*IDIV
      INTEGER FUNCTION IDIV (IN,ID,IND) 
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   IDIV V 7.00  2/21/90
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS INTEGER FUNCTION PERFORMS THE DIVISION IN/ID, WHEN
C        THE NUMERATOR, IN, AND THE DENOMINATOR, ID, ARE INTEGERS.
C        IF ID = 0, THE FUNCTION VALUE IS SET EQUAL TO ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
      IND = IZERO
      IF (ID.EQ.IZERO) GO TO 10
      IDIV = IN/ID
      RETURN
C
C     ..................................................................
C
  10  IDIV = IZERO
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END 
*IMPRUV
      SUBROUTINE IMPRUV (A,NASIZE,UL,NN,B,X,R,DX,APS,DIGITS,KEY)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. IMPRUV V 7.00  6/20/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE FROM CHAPTER 17 OF G. E. FORSYTHE AND C. B. MOLER'S
C     'COMPUTER SOLUTION OF LINEAR ALGEBRAIC SYSTEMS', PRENTICE-HALL
C     (1967).
C
C     A IS THE ORIGINAL MATRIX, UL IS FROM 'DECOMP', B IS THE RIGHT-HAND
C     SIDE, AND X IS THE SOLUTION FROM 'SOLVE'.  SUBROUTINE IMPROVES X
C     TO MACHINE ACCURACY AND SETS DIGITS EQUAL TO THE NUMBER OF DIGITS
C     OF X WHICH DO NOT CHANGE.
C         USES ABS(), AMAX1(), ALOG10() 
C
C               ADAPTED TO OMNITAB BY - 
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -     JUNE MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION APS(*)
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(*), B(*), DX(*), R(*), UL(*), X(*) 
      REAL             DIGITS 
      REAL             EPS, RXNORM, T, XNORM
      REAL             FDIV, FDPCON, FLOG10
C
      DOUBLE PRECISION DXA(1) 
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     FORSYTHE AND MOLER (PAGE 65) SUGGEST SETTING 'ITMAX' EQUAL TO
C     TWICE THE NUMBER OF DECIMAL DIGITS IN A (SINGLE PRECISION)
C     FLOATING-POINT NUMBER.
C     THUS 'ITMAX' IS MACHINE-DEPENDENT.
C
      DATA ITMAX / 20 /
C
C     ==================================================================
C
      KEY = IZERO
      N = NN
C
C     COMPUTE EPS
C     THE MAGNITUDE OF EPS IS MACHINE DEPENDENT.  IT IS THE LARGEST
C     NUMBER FOR WHICH 1.0 + EPS = 1.0 IS TRUE IN FLOATING POINT
C     ARITHMETIC.
C
      EPS = RONE
  10  EPS = RHALF * EPS
      IF (RONE+EPS.GT.RONE) GO TO 10
      XNORM = RZERO 
      DO 20 I=1,N
        XNORM = AMAX1 (XNORM,ABS(X(I))) 
  20  CONTINUE
C
      IF (XNORM) 40,30,40
  30    DIGITS = -FLOG10 (EPS)
        RETURN
C
C     ..................................................................
C
  40  DO 90 ITER=1,ITMAX
        DO 60 I=1,N 
          CALL DSUMAL (DXA,IZERO,DSUM)
          IJ = I
          DO 50 J=1,N
            DXA(1) = DBLE(A(IJ))*DBLE(X(J))
            IJ = IJ + NASIZE
            CALL DSUMAL (DXA,-IONE,DSUM)
  50      CONTINUE
          CALL DSUMAL (DXA,IONE,DSUM)
          DSUM = DBLE (B(I)) - DSUM
          R(I) = FDPCON (DSUM)
  60    CONTINUE
C
C        *** IT IS ESSENTIAL THAT A(I,J)*X(J) YIELD A DOUBLE PRECISION
C            RESULT AND THAT THE ABOVE + AND - BE DOUBLE PRECISION. ***
C
        CALL SOLVE (UL,N,R,DX,APS)
        RXNORM = RZERO
        DO 70 I=1,N 
          T = X(I)
          X(I) = X(I) + DX(I) 
          RXNORM = AMAX1 (RXNORM,ABS(X(I)-T))
   70   CONTINUE
        IF (ITER.NE.IONE) GO TO 80
        DIGITS = -FLOG10 (AMAX1(FDIV (RXNORM,XNORM,INDA),EPS) )
   80   IF (RXNORM-EPS*XNORM.LE.RZERO) RETURN
   90  CONTINUE
C
C     ITERATION DID NOT CONVERGE
C
      KEY = IZERO
      RETURN
C
C     ==================================================================
C
      END 
*INFERR      
      SUBROUTINE INFERR (I,INFA,INFC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. INFERR V 7.00  6/ 9/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT INFORMATIVE DIAGNOSTIC MESSAGES.  CALL ERROR (.) 200 AND UP.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (NCRT.EQ.IZERO) WRITE(IPRINT,301)
      II = I - 200
C
      GO TO (101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
     1       111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
     2       121, 122, 123, 124, 125, 126, 127, 128, 129, 130,
     3       131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
     4       141, 142, 143, 144, 145, 146, 147, 148, 149, 150,
     5       151, 152, 153, 154, 155, 156, 157, 158, 159, 160,
     6       161, 162, 163, 164, 165, 166, 167                ), II
C
C     ..................................................................
C
 101  WRITE (IPRINT,201) INFA
        RETURN
 102  WRITE (IPRINT,202)
        RETURN
 103  WRITE (IPRINT,203)
        RETURN
 104  WRITE (IPRINT,204)
        RETURN
 105  WRITE (IPRINT,302)
      WRITE (IPRINT,205)
        RETURN
 106  WRITE (IPRINT,302)
      WRITE (IPRINT,206)
        RETURN
 107  WRITE (IPRINT,207)
        RETURN
 108  WRITE (IPRINT,208)
        RETURN
 109  WRITE (IPRINT,209)
        RETURN
 110  WRITE (IPRINT,210)
        RETURN
 111  WRITE (IPRINT,211)
        RETURN
 112  WRITE (IPRINT,212)
        RETURN
 113  WRITE (IPRINT,213) INFA, INFC
        RETURN
 114  WRITE (IPRINT,214)
        RETURN
 115  WRITE (IPRINT,215) INFA
        RETURN
 116  WRITE (IPRINT,216)
        RETURN
 117  WRITE (IPRINT,217)
        RETURN
 118  WRITE (IPRINT,218) INFA
        RETURN
 119  WRITE (IPRINT,219)
        RETURN
 120  WRITE (IPRINT,220)
        RETURN
 121  WRITE (IPRINT,221)
        RETURN
 122  WRITE (IPRINT,222)
        RETURN
 123  WRITE (IPRINT,302)
      WRITE (IPRINT,223)
        RETURN
 124  WRITE (IPRINT,302)
      WRITE (IPRINT,224)
        RETURN
 125  WRITE (IPRINT,302)
      WRITE (IPRINT,225)
        RETURN
 126  WRITE (IPRINT,226) INFA
        RETURN
 127  WRITE (IPRINT,227)
        RETURN
 128  WRITE (IPRINT,228)
        RETURN
 129  WRITE (IPRINT,229)
        RETURN
 130  WRITE (IPRINT,230) INFA, INFA
      RETURN
 131  WRITE (IPRINT,231) INFA, INFA
        RETURN
 132  WRITE (IPRINT,232)
        RETURN
 133  WRITE (IPRINT,233)
        RETURN
 134  WRITE (IPRINT,234)
        RETURN
 135  WRITE (IPRINT,235)
        RETURN
 136  WRITE (IPRINT,302)
      WRITE (IPRINT,236)
        RETURN
 137  WRITE (IPRINT,237) INFA
        RETURN
 138  WRITE (IPRINT,302)
      WRITE (IPRINT,238)
        RETURN
 139  WRITE (IPRINT,239)
        RETURN
 140  WRITE (IPRINT,240) INFA, INFC
        RETURN
 141  WRITE (IPRINT,302)
      WRITE (IPRINT,241)
        RETURN
 142  WRITE (IPRINT,242)
        RETURN
 143  WRITE (IPRINT,243) INFA
        RETURN
 144  WRITE (IPRINT,244) INFA
        RETURN
 145  WRITE (IPRINT,302)
      WRITE (IPRINT,245) INFA
        RETURN
 146  WRITE (IPRINT,302)
      WRITE (IPRINT,246) INFA
        RETURN
 147  WRITE (IPRINT,247) INFA
        RETURN
 148  WRITE (IPRINT,302)
      WRITE (IPRINT,248)
        RETURN
 149  WRITE (IPRINT,249)
        RETURN
 150  WRITE (IPRINT,302)
      WRITE (IPRINT,250)
        RETURN
C
C     ERROR 251 IS A BLANK IN ORDER TO GET INFORMATIVE MESSAGE.
C        LABEL SUBROUTINE DOES THE REST OF THE ERROR MESSAGE WRITING.
C
 151  CONTINUE
        RETURN
C
C     IF ERROR (252) IS USED, NROLD MUST BE DEFINED.
C
 152  WRITE (IPRINT,252) INFC, INFA
        RETURN
 153  WRITE (IPRINT,253)
        RETURN
 154  WRITE (IPRINT,302)
      WRITE (IPRINT,254) INFA
        RETURN
 155  WRITE (IPRINT,302)
      WRITE (IPRINT,255) INFA
        RETURN
 156  WRITE (IPRINT,256) INFA
        RETURN
 157  WRITE (IPRINT,302)
      WRITE (IPRINT,257) INFA, INFC
        RETURN
 158  WRITE (IPRINT,258)
        RETURN
 159  WRITE (IPRINT,259)
        RETURN
 160  WRITE (IPRINT,260)
        RETURN
 161  WRITE (IPRINT,261)
        RETURN
 162  WRITE (IPRINT,262)
        RETURN
 163  WRITE (IPRINT,263)
      RETURN
 164  WRITE (IPRINT,302)
      WRITE (IPRINT,264)
        RETURN
 165  WRITE (IPRINT,302)
      WRITE (IPRINT,265)
        RETURN
 166  WRITE (IPRINT,266)
        RETURN
 167  WRITE (IPRINT,267)
        RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 201  FORMAT (10X,42HTOO MUCH DATA AFTER SET, READ OR GENERATE.,18X/12X,
     1   29HALL DATA WERE LOST AFTER ROW ,I4,1H.,24X)
 202  FORMAT (10X,45HTHE INSTRUCTION WAS EXECUTED, BUT NOT STORED.,15X)
 203  FORMAT (10X,30HVALUE REQUESTED WAS NOT FOUND.,30X)
 204  FORMAT (10X,37HCOLUMN NUMBER INCORRECT OR NOT FOUND.,23X)
 205  FORMAT (12X,25HITS MEANING IS NOT CLEAR.,33X)
 206  FORMAT (12X,51HFUNCTION NOT DEFINED FOR SPECIFIED PARAMETER VALUE.
     1   ,7X)
 207  FORMAT (10X,57HA VALUE OF DEGREES OF FREEDOM LESS THAN 1 WAS RESET
     1 TO 1.,3X)
 208  FORMAT (10X,58HA VALUE OF DEGREES OF FREEDOM WAS TRUNCATED TO AN I
     1NTEGER.,2X)
 209  FORMAT (10X,49HTITLE NUMBER MUST BE 1, 2, 3 OR 4 AND 1 WAS USED.,
     1   11X)
 210  FORMAT (10X,58HNO. ROWS NOT = TO NO. COLS. LARGEST SQUARE MATRIX W
     1AS USED,2X)
 211  FORMAT (10X,58HAN INCORRECT ASTERISK STRING IMPLYING THROUGH WAS I
     1GNORED.,2X)
 212  FORMAT (10X,50HUNNECESSARY ARGUMENTS IN INSTRUCTION WERE IGNORED.,
     1   10X)
 213  FORMAT (10X,22HMATRIX EXTENDS BEYOND ,I4,8H ROW BY ,I4,18H COLUMN 
     1WORKSHEET.,8X/12X,51HONLY PART OF THE MATRIX IS STORED IN THE WORK
     2SHEET.,9X)
 214  FORMAT (10X,26HINSUFFICIENT SCRATCH AREA.,34X)
 215  FORMAT (10X,8HNRMAX = ,I5,40H IS NOT LARGE ENOUGH TO ALLOW ITERATI
     1ON.,7X)
 216  FORMAT (10X,48H1ST COLUMN OF ISETUP OR ISOLATE IS NOT MONOTONIC,1
     12X/12X,16H OR IS CONSTANT.,42X)
 217  FORMAT (10X,33HITERATION DID NOT FIND ANY ROOTS.,27X)
 218  FORMAT (10X,I4,57H ROW WORKSHEET IS TOO SHORT TO ACCOMMODATE ALL T
     1HE VALUES,3X/12X,30HGENERATED BY THIS INSTRUCTION.,28X)
 219  FORMAT (10X,22HNO EXTREMA WERE FOUND.,38X)
 220  FORMAT (10X,49HA TRIAD OF X'S WITH AT LEAST TWO IDENTICAL VALUES,
     1  11X/12X,22HWAS FOUND AND IGNORED.,36X)
 221  FORMAT (10X,52HONLY THE FIRST ARGUMENT IN THE INSTRUCTION WAS USED
     1.,8X)
 222  FORMAT (10X,48HFORMAT WAS NOT FOUND.  READABLE FORMAT WAS USED.,
     1   12X)
 223  FORMAT (12X,38HONE, SOME OR ALL WEIGHTS ARE NEGATIVE.,20X)
 224  FORMAT (12X,21HALL WEIGHTS ARE ZERO.,37X)
 225  FORMAT (12X,44HVALUE OF FUNCTION IS TOO LARGE OR TOO SMALL.,14X)
 226  FORMAT (10X,44HCOLUMN NOT LONG ENOUGH TO STORE ALL NUMBERS.,16X/
     1   12X,6HFIRST ,I5,21H NUMBERS WERE STORED.,28X)
 227  FORMAT (10X,50HNOT ENOUGH DATA IN COLUMN TO RESTORE MATRIX/ARRAY.,
     1   11X/12X,25HDATA AVAILABLE WERE USED.,33X)
 228  FORMAT (10X,44HTHE OPTIMAL SOLUTION IS PROBABLY NOT UNIQUE.,16X)
 229  FORMAT (10X,51HMORE THAN 50 HEAD COLUMN INSTRUCTIONS AND/OR LABELS
     1,9X/12X,53HHAVE BEEN USED.  ONLY THE LAST 50 HAVE BEEN RETAINED.,
     2   5X)
 230  FORMAT (10X,38HATTEMPT TO PROMOTE FROM BELOW NRMAX = ,I5,1H.,16X/
     1     12X,29HFIRST ARGUMENT IS RESET TO = ,I5,1H.,23X)
 231  FORMAT (10X,28HATTEMPT TO DEMOTE BELOW THE ,I5,15H ROW WORKSHEET.,
     1   6X/12X,15HDATA BELOW ROW ,I5,9H IS LOST.,30X)
 232  FORMAT (10X,45HX FOR ELLIPTICAL INTEGRALS IS 1.0 OR GREATER.,15
     1X/12X,31HTHE RESULT IS SET EQUAL TO 0.0.,27X)
 233  FORMAT (10X,57HNEGATIVE VALUE(S) WERE ENCOUNTERED BY PARTITION FUN
     1CTION.,3X/12X,15H ZEROES STORED.,43X)
 234  FORMAT (10X,49HPOSITIVE, INSTEAD OF NEGATIVE, TEMPERATURES USED.,
     1   11X)
 235  FORMAT (10X,59HFOR Y = F(X,THETA), X OR THETA WAS TRUNCATED TO AN 
     1INTEGER., 1X)
 236  FORMAT (12X,52HCOMMAND BEGINS WITH S AND STORAGE MUST BE REQUESTED
     1.,6X)
 237  FORMAT (10X,48HNUMBER OF SIGNIFICANT DIGITS AFTER DECIMAL POINT,
     1   12X/12X,16HHAS BEEN SET TO ,I3,1H.,38X)
 238  FORMAT (12X,40HALL POINTS ARE OUTSIDE SPECIFIED LIMITS.,18X)
 239  FORMAT (10X,59HFIRST OF EQUAL ROW OR COLUMN TOTALS USED TO COMPUTE
     1 LAMBDA.,1X)
 240  FORMAT (10X,49HPARTIAL CORRELATION COEFFICIENTS ARE NOT DEFINED.,1
     11X/12X,I4,26H MEASUREMENTS MUST EXCEED ,I4,11H VARIABLES.,13X)
 241  FORMAT (12X,39HLOWER LIMIT OF AXIS EQUALS UPPER LIMIT.,19X)
 242  FORMAT (10X,50HPRINTING OF STEM AND LEAF DISPLAY IS NOT POSSIBLE.,
     1 10X/12X,39H INSTRUCTION WAS TREATED AS SSTEM LEAF.,19X)
 243  FORMAT (10X,I4,55H ROWS IN WORKSHEET ARW NOT ENOUGH FOR COMPLETE S
     1TORAGE.,1X)
 244  FORMAT (10X,11HDISPLAY IS ,I4,32H LINES, ONLY FIRST 99 DISPLAYED.,
     1 13X)
 245  FORMAT (12X, 8HWIDTH = ,I3,27H IS TOO SMALL OR TOO LARGE.,10X)
 246  FORMAT (12X, 8HWIDTH = ,I2,25H IS TOO SMALL FOR A PLOT.,23X)
 247  FORMAT (10X, 8HWIDTH = ,I2,49H IS INSUFFICIENT FOR PAGE PLOT.  BES
     1T PLOT GIVEN.,1X)
 248  FORMAT (12X,39HALL NUMBERS IN THE COLUMN ARE THE SAME.,19X)
 249  FORMAT (10X,59HCOMPUTING PROBLEMS ENCOUNTERED. RESULTS MAY BE MEAN
     1INGLESS.,1X)
 250  FORMAT (12X,39HCOLUMN WAS HEADED BY LABEL INSTRUCTION.,19X)
 252  FORMAT (10X,26HNRMAX HAS BEEN RESET FROM ,I5,4H TO ,I5,1H.,19X)
 253  FORMAT (10X,57HIMPLIED THROUGH FOR LABELS MUST BE IN ALPHABETICAL 
     1ORDER.,3X)
 254  FORMAT (10X, 9HLENGTH = ,I3,42H IS TOO SMALL.                     
     1       ,6X)
 255  FORMAT (12X, 9HLENGTH = ,I2,25H IS TOO SMALL FOR A PLOT.,22X)
 256  FORMAT (10X,56HITERATION FAILED TO FIND AN EIGENVALUE (OR EIGENVEC
     1TOR),/10X,I3,23HUNORDERED VALUES FOUND.,34X)
 257  FORMAT (12X,9HNRMAX IS ,I2,39H  AND MUST BE GREATER THAN OR EQUAL 
     1TO ,I2,6X)
 258  FORMAT (10X,59HVALUE OUTSIDE ALLOWABLE RANGE. RESULT WAS SET EQUAL
     1 TO 1.0.,1X)
 259  FORMAT (10X,57HPRINTING PROBLEM.  PLEASE CALL SALLY PEAVY, 301-921
     1-3651.,3X)
 260  FORMAT (10X,51HCOLUMN (ARRAY, MATRIX) HAS BEEN PREVIOUSLY LABELED.
     1,9X)
 261  FORMAT (10X,43HEXTRAPOLATION DONE FOR MORE THAN ONE DELTA.,17X)
 262  FORMAT (10X,40HORDER OF INTERPOLATION EQUALS LIST SIZE.,20X)
 263  FORMAT (10X,54HORDER OF INTERP WAS RESET DUE TO SIZE OF SCRATCH AR
     1EA.,6X)
 264  FORMAT (12X,29HINSTRUCTION CANNOT BE STORED.,29X)
 265  FORMAT (12X,44HALL NUMBERS IN THE COLUMN ARE EQUAL TO ZERO.,14X)
 266  FORMAT (12X,30HFIRST COMMAND MUST BE OMNITAB.,28X)
 267  FORMAT (12X,'END OF FILE ENCOUNTERED. NO MORE DATA AVAILABLE.',
     1 10X)
C
C     ..................................................................
C
 301  FORMAT (1X/6X,52H* INFORMATIVE DIAGNOSTIC FOR THE ABOVE INSTRUCTIO
     1N -,12X)
 302  FORMAT (10X,39HTHE INSTRUCTION WAS IGNORED BECAUSE ...,21X)
C
C     ==================================================================
C
      END
*INPUT
      SUBROUTINE INPUT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  INPUT V 7.00  7/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCESS THE READING OF INPUT RECORDS.
C
C     IF NDEMD = 0, INPUT IS CARD IMAGE FROM CARD READER OR TAPE.
C     IF NDEMD = 1, INPUT IS REAL-TIME FROM A KEYBOARD.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      CHARACTER  NEWCRD*1
C
C     ==================================================================
C
C     KRDKNT = KRDKNT + IONE
C
C     THE FOLLOWING STATEMENTS ARE NECESSARY TO
C     PRINT A PROMPT IF INPUT TO OMNITAB IS INTERACTIVE.
C     THIS ONLY NEEDED FOR VAX MACHINE.
C
      CALL VWHERE (INDVAX)
      IF (INDVAX.EQ.IONE) GO TO 20
C
C     IF (NDEMD.EQ.IZERO) GO TO 20
      IF (MODE .EQ.ITHRE) GO TO 10
C
C     NOTE ...
C
C        REMOVE C FROM COLUMN 1 OF THE NEXT WRITE STATEMENT IF A
C        SIGNAL FROM OMNITAB IS NECESSARY TO INDICATE IT IS READY
C        FOR THE NEXT INSTRUCTION FROM THE TERMINAL. IF THE OTG FORTRAN
C        COMPILER IS NOT USED, PLACE A C IN COLUMN 1 ON CALL COUA@('+')
C        STATEMENT.
C
      IF (IFILE.EQ. IONE) THEN
        WRITE (NPRNT,40)
c       CALL COUA@ ('+')
      ENDIF
      GO TO 20
C
  10  NSTMTA = IDIV (NSTMT,ITEN,IND)
C
C     NOTE ...
C
C        REMOVE C FROM COLUMN 1 OF THE NEXT WRITE STATEMENT IF A
C        SIGNAL FROM OMNITAB IS NECESSARY TO INDICATE IT IS READY
C        FOR THE NEXT INSTRUCTION FROM THE TERMINAL. IF THE OTG FORTRAN
C        COMPILER IS NOT USED, PLACE A C IN COLUMN 1 ON CALL COUA@('+'),
C        CALL PRINT_I4@ (NSTMTA) AND CALL COUA@('/') STATEMENTS.
C
      IF (NCRT.NE.IZERO) THEN
        WRITE (NPRNT,50) NSTMTA
c       CALL COUA@ ('+')
c       CALL PRINT_I4@ (NSTMTA)
c       CALL COUA@ ('/')
      ENDIF
C
  20  READ (INUNIT,60,END=30) (NEWCRD(I),I=1,LENCRD)
      KARD(IONE) = IZERO
      KARD(ITWO) = IZERO
      KARD(KRDEND+3) = 46
      CALL OMCONV (NEWCRD,KARD(ITHRE),KRDEND)
      RETURN
C
C     GET INFORMATION FROM PREVIOUS FILE.
C
  30  CLOSE (INUNIT)
      IFILE = IFILE - IONE
      IF (IFILE.EQ.IZERO) THEN
        INUNIT = NUNIT(1)
        IFILE  = IONE
        ISFILE = IONE
        RETURN
      ENDIF
C
      INUNIT = NUNIT(IFILE)
      IF (IFILE.EQ.IONE) NERROR = IZERO
C
C     NOTE ...
C
C        REMOVE C FROM COLUMN 1 OF THE NEXT WRITE STATEMENT IF A
C        SIGNAL FROM OMNITAB IS NECESSARY TO INDICATE IT IS READY
C        FOR THE NEXT INSTRUCTION FROM THE TERMINAL. IF THE OTG FORTRAN
C        COMPILER IS NOT USED, PLACE A C IN COLUMN 1 ON CALL COUA@('+')
C        STATEMENT.
C
      IF (IFILE.EQ.IONE .AND. INDVAX.EQ.IZERO) THEN
        WRITE (NPRNT,40)
c       CALL COUA@ ('+')
      ENDIF
      GO TO 20
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
C     NOTE ...
C
C        IF OPERATING SYSTEM DOES NOT INDICATE TO THE TERMINAL THAT IT
C        IS READY FOR THE NEXT INSTRUCTION, REMOVE THE C IN COLUMN 1 OF
C        THE NEXT TWO FORMAT STATEMENTS AND PLACE A C IN COLUMN 1 OF THE
C        PRESENT FORMAT STATEMENT LABELED 50.
C        THIS WILL CAUSE OMNITAB TO SIGNAL WITH THE + CHARACTER THAT
C        IT IS READY FOR THE NEXT OMNITAB INSTRUCTION.
C
   40  FORMAT ('$+')
   50 FORMAT ('$+',I3,'/ ')
C
C 50  FORMAT (2X,I3,' / ')
  60  FORMAT (80A1)
C
C     ==================================================================
C
      END
*INTRP
      SUBROUTINE INTRP (X,Y,NLIST,X1,RESULT,NX1,NORD,S,SA,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  INTRP V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALLING SEQUENCE ...
C        SUBROUTINE INTRP (X,Y,NLIST,X1,RESULT,NX1,NORD,S,SA,IND)
C
C     X        THE INDEPENDENT VALUE OF THE TABLE.  MUST BE IN ASCENDING
C              OR DESCENDING ORDER.  NEED NOT BE EVENLY SPACED.
C     Y        THE DEPENDENT VALUE OF THE TABLE
C     NLIST    LENGTH OF  X OR Y
C     X1       VALUES TO BE INTERPOLATED
C     RESULT   RESULT FROM INTERPOLATION
C     NX1      LENGTH OF X1 VECTOR
C     NORD     ORDER OF INTERPOLATION
C     S        SCRATCH AREA  S(3*NORD)
C     SA       SCRATCH AREA  SA(NORD,NORD)
C     IND      INDICATOR
C           IND=0  EVERYTHING FINE
C           IND=2  EXTRAPOLATION AND MORE THEN ONE DELTA
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             RESULT(*), S(*), SA(NORD,NORD), X(*), X1(*), Y(*)
      REAL             PROD, R, SUM, TEMP, XA
      REAL             FDIV
C
C     ==================================================================
C
      INDA = IZERO
      IND = IZERO
      NRD = NORD - IONE
      NDIR = IONE
      IF (X(1).GE.X(ITWO)) NDIR = ITWO
      I = IONE
      DO 240 II=1,NX1
        IC = IZERO
        XA = X1(II) 
        GO TO (10,190), NDIR
  10    IF (XA-X(1)) 20,170,30
  20    IF (ABS(XA-X(1)).GT.ABS(X(1)-X(2))) INDA = ITWO
        IC = IONE
        IA = IONE
        GO TO 80
C
  30    DO 40 IA=I,NLIST
          IF (X(IA)-XA) 40,180,60
  40    CONTINUE
C
  50    IF (ABS(X(NLIST)-XA).GT.ABS(X(NLIST)-X(NLIST-1))) INDA = ITWO 
        IA = NLIST - NRD
        IC = IONE
        GO TO 80
C
  60    IA = IA - IONE
        IF (X(IA)-XA) 70,180,60
  70    IF (IA+NRD.LE.NLIST) GO TO 80
        IC = IONE
        IA = NLIST - NRD
C
  80    IF (NRD.GT.IONE) GO TO 90
        TEMP = FDIV (XA-X(IA),X(IA+1)-X(IA),IND)
        RESULT(II) = Y(IA) + (Y(IA+1)-Y(IA))*TEMP 
        GO TO 230
C
  90    IF (IC.NE.IZERO) GO TO 100
        IA = IA - IDIV (NRD,ITWO,IND)
        IF (IA.LE.IZERO) IA = IONE
 100    NA = IA + NRD
        PROD = RONE 
        IZ = IONE
        IZA = NORD + IONE
C
        DO 110 IB=IA,NA
          S(IZ) = X(IB)
          S(IZA) = XA - X(IB) 
          PROD = PROD*S(IZA)
          IZ = IZ + IONE
          IZA = IZA + IONE
 110    CONTINUE
C
        NB = NRD + IONE
C
        DO 130 IAR=2,NB
          DO 120 IBR=IAR,NB
            SA(IBR-1,IAR-1) = S(IAR-1) - S(IBR)
            SA(IAR-1,IBR) = -SA(IBR-1,IAR-1)
 120      CONTINUE
 130    CONTINUE
C
        IZB = IZA
        IZC = NORD + IONE
C
        DO 150 IAR=1,NB
          SUM = S(IZC)
          DO 140 IBR=1,NRD
            SUM = SUM*SA(IBR,IAR)
 140      CONTINUE
          S(IZA) = FDIV (PROD,SUM,IND)
          IZC = IZC + IONE
          IZA = IZA + IONE
 150    CONTINUE
C
        R = RZERO
        IAX = IA
C
        DO 160 IX=1,NORD
          R = R + S(IZB)*Y(IAX)
          IAX = IAX + IONE
          IZB = IZB + IONE
 160    CONTINUE
C
        RESULT(II) = R
        GO TO 230
C
 170    RESULT(II) = Y(1)
        IA = IONE
        GO TO 230
C
 180    RESULT(II) = Y(IA)
        GO TO 230
C
 190    IF (XA-X(1)) 200,170,30
C
 200    DO 210 IA=I,NLIST
          IF (XA-X(IA)) 210,180,220
 210    CONTINUE
C
        GO TO 50
 220    IA = IA - IONE
        IF (XA-X(IA)) 70,180,220
 230    I = IA
 240  CONTINUE
      IND = IND + INDA
      RETURN
C
C     ==================================================================
C
      END 
*INVCHK
      SUBROUTINE INVCHK (A,M,AINV,UL,N,B,R,DX,Y,APS,L2P,ERR,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. INVCHK V 7.00  5/20/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM TO INVERT AND
C     TO SOLVE A LINEAR SYSTEM OF EQUATIONS, AX = B, USING SUBROUTINES
C     IN CHAPTER 17 OF G. E. FORSYTHE AND C. B. MOLER'S 'COMPUTER
C     SOLUTION OF LINEAR ALGEBRAIC SYSTEMS', PRENTICE-HALL (1967).
C
C     IN THE ALGOL VERSION OF THIS PROGRAM GIVEN IN CHAPTER 16 OF
C        FORSYTHE AND MOLER THERE ARE ADDITIONAL COMMENTS.
C
C     THIS SUBROUTINE INVERTS A MATRIX AND PROVIDES ALL THE CHECKS
C        DESCRIBED IN PAC-1
C
C     A IS THE MATRIX TO BE INVERTED
C
C     M IS THE SIZE OF A AS DIMENSIONED IN THE CALLING PROGRAM  A(M,M)
C
C     N IS THE SIZE OF A TO BE INVERTED 
C
C     AINV WILL CONTAIN THE INVERTED MATRIX IF INVERSION IS OBTAINABLE
C
C     UL,B,R,DX,APS ARE SCRATCH AREAS NEEDED BY DECOMP,SOLVE AND IMPRUV.
C
C     ERR  WILL CONTAIN THE 3 WAYS OF EVALUATING NORM CHECKS
C        ERR IS  A DIMENSIONED AS ERR(3)
C
C     IND IS AN INDICATOR
C        IND=0  MATRIX INVERTED AND ERROR CHECKS MADE
C        IND=1  MATRIX SINGULAR
C
C               ADAPTED TO OMNITAB BY - 
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                   CURRENT VERSION - JUNE, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION APS(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(*), AINV(*), B(*)
      REAL             DX(*), ERR(*), R(*), UL(*), Y(*)
      REAL             ANORM(2,3)
      REAL             DGT, DIGITS, SUM, TEMP
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
      CALL DECOMP (A,M,UL,N,B,APS,IND)
      IF (IND.NE.IZERO) RETURN
      DGT = RZERO
C
      DO 20 J=1,N
        DO 10 I=1,N 
          B(I) = RZERO
  10    CONTINUE
        B(J) = RONE 
        JI1 = (J - IONE) * N + 1
        CALL SOLVE (UL,N,B,AINV(JI1),APS)
        CALL IMPRUV (A,M,UL,N,B,AINV(JI1),R,DX,APS,DIGITS,IND)
        IF (IND.NE.IZERO) RETURN
        IF (J.EQ.IONE) DGT = DIGITS
        DGT = AMIN1 (DGT,DIGITS)
  20  CONTINUE
C
      ERR(4) = DGT
      DO 90 K=1,2
        DO 30 I=1,3 
          ANORM(K,I) = RZERO
  30    CONTINUE
C
        DO 80 I=1,N 
          SUM = RZERO
          IJ = I
          DO 70 J=1,N
            IF (K.EQ.ITWO) GO TO 40
            TEMP = ABS (AINV(IJ))
            GO TO 60
  40        TEMP = RZERO
            IL = I
            LJ = (J - IONE) * N + IONE
            DO 50 L=1,N
              TEMP = TEMP + A(IL)*AINV(LJ)
              IL = IL + M
              LJ = LJ + IONE
  50        CONTINUE
C
            IF (I.EQ.J) TEMP = RONE - TEMP
            TEMP = ABS (TEMP) 
  60        ANORM(K,1) = ANORM(K,1) + TEMP**2
            IF (ANORM(K,2).LT.TEMP) ANORM(K,2) = TEMP
            SUM = SUM + TEMP
            IJ = IJ + N
  70      CONTINUE
C
          IF (ANORM(K,3).LT.SUM) ANORM(K,3) = SUM 
  80    CONTINUE
C
        ANORM(K,1) = FSQRT (ANORM(K,1)) 
        ANORM(K,2) = FLOAT(N) * ANORM(K,2)
  90  CONTINUE
C
      DO 100 K=1,3
        ERR(K) = FDIV (ANORM(1,K)*ANORM(2,K),RONE-ANORM(2,K),INDA)
 100  CONTINUE
C
      IF (L2P.EQ.IONE) RETURN
C
      DO 110 J=1,N
        B(J) = Y(J) 
 110  CONTINUE
C
      CALL SOLVE (UL,N,B,AINV(1),APS) 
      CALL IMPRUV (A,M,UL,N,B,AINV(1),R,DX,APS,DIGITS,IND)
      IF (IND.NE.IZERO) RETURN
      ERR(4) = DIGITS
      RETURN
C
C     ==================================================================
C
      END 
*INZP
      SUBROUTINE INZP (X,N,IN,IZ,IP,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   INZP V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE NUMBER OF NEGATIVE VALUES, IN,
C             NUMBER OF     ZERO VALUES, IZ,
C             NUMBER OF POSITIVE VALUES, IP,
C             IN VECTOR X OF LENGTH N.
C
C     IND = 0, IF EVERYTHING IS OK
C           1, IF (IN+IZ+IP) NE TO N
C           2, IF N LESS THAN ONE AND
C                 SETS IN = IZ = IP = 0.
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
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(*)
C
C     ==================================================================
C
      IND = IZERO
      IN  = IZERO
      IZ  = IZERO
      IP  = IZERO
      IF (N.GE.IONE) GO TO 10 
      IND = ITWO
      RETURN
C
C     ..................................................................
C
  10  DO 50 I=1,N
        IF (X(I)) 20,30,40
  20    IN = IN + IONE
        GO TO 50
  30    IZ = IZ + IONE
        GO TO 50
  40    IP = IP + IONE
  50  CONTINUE
C
      M = IN + IZ + IP
      IF (M.EQ.N) RETURN
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END 
*ISORT
      SUBROUTINE  ISORT (IA,IB,N,NH)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  ISORT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SORTS INTEGER ARRAY IA(.) INTO INCREASING ORDER.
C
C     HIERARCHY FROM 1 TO N = JJ - II + 1 IS PUT IN INTEGER ARRAY IB(.).
C
C     IF NH GREATER THAN ZERO, SORT HIERARCHY  FOR EQUAL VALUES IN IA(.).
C     IF NH=0, DO NOT SORT HIERARCHY FOR EQUAL VALUES IN IA(.).
C
C     ALGORITHM 347, COMM ACM,12,3,185-187 (1969) BY R. C. SINGLETON. 
C                  MODIFIED AND IMPLEMENTED BY DAVID HOGBEN, SEL, NBS.
C               SEE COMMENTS IN COMM.,ACM,VOL 13. P54 (JAN,70) AND 624 (
C                P 54 (JAN, 70) AND 624 (OCT, 70) 
C
C               MODIFIED BY - 
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-921-2315 
C                  ORIGINAL VERSION -     JULY, 1977. 
C                   CURRENT VERSION - FEBRUARY, 1990. 
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C     ARRAYS IU(K) AND IL(K) PERMIT SORT OF 2**(K+1)-1 NUMBERS.  IF MORE
C        THAN 100,000 NUMBERS ARE TO BE SORTED, K SHOULD BE INCREASED.
C
      DIMENSION IA(*), IB(*), IU(16), IL(16)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
      II = IONE
      JJ = N
      DO 10 I=1,N
        IB(I) = I
  10  CONTINUE
C
      M = IONE
      I = II
      J = JJ
  20  IF (I.GE.J) GO TO 90
  30  K = I
      IJ = IDIV (J+I,ITWO,IND)
      IT = IA(IJ)
      IS = IB(IJ)
      IF (IA(I).LE.IT) GO TO 40
      IA(IJ) = IA(I)
      IB(IJ) = IB(I)
      IA(I) = IT
      IB(I) = IS
      IT = IA(IJ)
      IS = IB(IJ)
  40  L = J
      IF (IA(J).GE.IT) GO TO 60
      IA(IJ) = IA(J)
      IB(IJ) = IB(J)
      IA(J) = IT
      IB(J) = IS
      IT = IA(IJ)
      IS = IB(IJ)
      IF (IA(I).LE.IT) GO TO 60
      IA(IJ) = IA(I)
      IB(IJ) = IB(I)
      IA(I) = IT
      IB(I) = IS
      IT = IA(IJ)
      IS = IB(IJ)
      GO TO 60
  50  IA(L) = IA(K) 
      IB(L) = IB(K) 
      IA(K) = ITT
      IB(K) = ISS
  60  L = L - IONE
      IF (IA(L).GT.IT) GO TO 60
      ITT = IA(L)
      ISS = IB(L)
  70  K = K + IONE
      IF (IA(K).LT.IT) GO TO 70
      IF (K.LE.L) GO TO 50
      IF (L-I.LE.J-K) GO TO 80
      IL(M) = I
      IU(M) = L
      I = K
      M = M + IONE
      GO TO 100
  80  IL(M) = K
      IU(M) = J
      J = L
      M = M+IONE
      GO TO 100
  90  M = M - IONE
      IF (M.EQ.IZERO) GO TO 130
      I = IL(M)
      J = IU(M)
 100  IF (J-I.GE.11) GO TO 30 
      IF (I.EQ.II) GO TO 20
      I = I - IONE
 110  I = I + IONE
      IF (I.EQ.J) GO TO 90
      IT = IA(I+1)
      IS = IB(I+1)
      IF (IA(I).LE.IT) GO TO 110
      K = I
 120  IA(K+1) = IA(K)
      IB(K+1) = IB(K)
      K = K - IONE
      IF (IT.LT.IA(K)) GO TO 120
      IA(K+1) = IT
      IB(K+1) = IS
      GO TO 110
 130  IF (NH.EQ.IZERO) RETURN 
      II = IZERO
      DO 170 I=2,N
        IF (IA(I-1).LT.IA(I)) GO TO 140 
        IAB = IA(I-1)
        IABC = IA(I)
        IF (IAB.NE.IABC) GO TO 140
        IF (II.EQ.IZERO) II = I
        IF (I.NE.N) GO TO 170 
        ISTOP = N
        GO TO 150
 140    IF (II.EQ.IZERO) GO TO 170
        ISTOP = I - IONE
 150    INDA = IZERO
        DO 160 IJ=II,ISTOP
          IF (IB(IJ-1).LT.IB(IJ)) GO TO 160
          IT = IB(IJ-1)
          IB(IJ-1) = IB(IJ)
          IB(IJ) = IT
          INDA = IONE
 160    CONTINUE
        IF (INDA.NE.IZERO) GO TO 150
        II = IZERO
 170  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*ITALAN
      SUBROUTINE ITALAN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ITALAN V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY LAVINO RICCIARDI.   JUNE 1971.
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
      DATA L(  1) / 1461102916 /
      DATA L(  2) / 1493603160 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /  126111280 /
      DATA L(  5) / 1215502449 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82514040 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514769 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84107272 /
      DATA L( 14) / 1507504012 /
      DATA L( 15) /   84616038 /
      DATA L( 16) / 1469716285 /
      DATA L( 17) / 1466210292 /
      DATA L( 18) /  105401605 /
      DATA L( 19) / 1513801248 /
      DATA L( 20) /  110109288 /
      DATA L( 21) / 1494609297 /
      DATA L( 22) /  112706900 /
      DATA L( 23) / 1503011385 /
      DATA L( 24) / 1502714729 /
      DATA L( 25) /  124710206 /
      DATA L( 26) /  124710395 /
      DATA L( 27) /  124710422 /
      DATA L( 28) /  126301458 /
      DATA L( 29) / 1510815138 /
      DATA L( 30) /  127600000 /
      DATA L( 31) /  127605103 /
      DATA L( 32) /  127605832 /
      DATA L( 33) / 1181806608 /
      DATA L( 34) / 1513801258 /
      DATA L( 35) /  961606588 /
      DATA L( 36) / 1528713527 /
      DATA L( 37) /  694819198 /
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
      DATA L( 48) /  187515295 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  221802916 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614436 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) / 1138311468 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255309297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) /  260604875 /
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
      DATA L( 80) /  260614607 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  226913667 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  271515138 /
      DATA L( 86) /  233614613 /
      DATA L( 87) /  296813851 /
      DATA L( 88) / 1337009918 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1506714176 /
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
      DATA L(102) /  222802334 /
      DATA L(103) /  452713545 /
      DATA L(104) /  424009316 /
      DATA L(105) / 1393309540 /
      DATA L(106) /  416305679 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  417400000 /
      DATA L(112) / 1208904049 /
      DATA L(113) /  430906959 /
      DATA L(114) / 1326413482 /
      DATA L(115) / 1620914365 /
      DATA L(116) / 1653312159 /
      DATA L(117) / 1336701073 /
      DATA L(118) / 1640401713 /
      DATA L(119) /  480013566 /
      DATA L(120) /  417813235 /
      DATA L(121) /  309604125 /
      DATA L(122) /  440913152 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204132 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) / 1078611425 /
      DATA L(130) /  709411142 /
      DATA L(131) /  635410463 /
      DATA L(132) / 1400705103 /
      DATA L(133) / 1399901296 /
      DATA L(134) / 1399900918 /
      DATA L(135) / 1399907128 /
      DATA L(136) / 1399906939 /
      DATA L(137) / 1399007155 /
      DATA L(138) /  694213270 /
      DATA L(139) /  695804140 /
      DATA L(140) /  417813379 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104150 /
      DATA L(145) /  694819212 /
      DATA L(146) /  708908775 /
      DATA L(147) /  710613149 /
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
      DATA L(161) /  901014607 /
      DATA L(162) /  915601053 /
      DATA L(163) /  912600000 /
      DATA L(164) /  916010253 /
      DATA L(165) /  916003726 /
      DATA L(166) /  950802916 /
      DATA L(167) /  417813696 /
      DATA L(168) /  952300000 /
      DATA L(169) /  952314107 /
      DATA L(170) /  952309734 /
      DATA L(171) /  997204012 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  952515007 /
      DATA L(175) /  955910292 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406927 /
      DATA L(179) /  973416191 /
      DATA L(180) /  992711054 /
      DATA L(181) /  980201605 /
      DATA L(182) / 1007614837 /
      DATA L(183) / 1003501248 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984309297 /
      DATA L(186) / 1181806927 /
      DATA L(187) /  990014999 /
      DATA L(188) / 1506714018 /
      DATA L(189) /  992711385 /
      DATA L(190) /  992414729 /
      DATA L(191) /  992711062 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1000515138 /
      DATA L(194) / 1003501258 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) /  989414839 /
      DATA L(198) /  963115067 /
      DATA L(199) /  973416193 /
      DATA L(200) / 1018413527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) /  696104174 /
      DATA L(204) / 1046814992 /
      DATA L(205) /  442114905 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1045410751 /
      DATA L(209) / 1129514580 /
      DATA L(210) /  112810471 /
      DATA L(211) / 1142506940 /
      DATA L(212) / 1170919404 /
      DATA L(213) / 1170919482 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) /  416305679 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) /  316005590 /
      DATA L(220) / 1326413569 /
      DATA L(221) / 1216503341 /
      DATA L(222) / 1506714176 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1208904049 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316305328 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) /  416305679 /
      DATA L(231) / 1327308778 /
      DATA L(232) /  988606732 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1315314269 /
      DATA L(235) /  123311490 /
      DATA L(236) / 1337205986 /
      DATA L(237) / 1440713543 /
      DATA L(238) /  900407102 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) /  234002214 /
      DATA L(243) / 1393313677 /
      DATA L(244) /  417813158 /
      DATA L(245) / 1208306561 /
      DATA L(246) / 1434205602 /
      DATA L(247) /   81311424 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1400000000 /
      DATA L(250) / 1400005103 /
      DATA L(251) / 1400005832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1338411281 /
      DATA L(255) / 1389215696 /
      DATA L(256) / 1142506948 /
      DATA L(257) /  695904143 /
      DATA L(258) / 1434205605 /
      DATA L(259) / 1315306647 /
      DATA L(260) / 1296103403 /
      DATA L(261) / 1389214392 /
      DATA L(262) /  112614627 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1389201723 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1427615067 /
      DATA L(268) / 1426909504 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1476900000 /
      DATA L(271) / 1477600000 /
      DATA L(272) / 1477700000 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) /  110907156 /
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
     1      11300,   7102,  14733,   9734,  14406,  11664,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1        729,  14843,  11274,   4797,   9524,  14392,   9910 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       6959,   4178,  11284,  11283,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.) 
C
      DATA LD( 1), LD( 2) / 1336906917, 1439209910 /
      DATA LD( 3), LD( 4) / 1181704797,  889005365 /
      DATA LD( 5), LD( 6) / 1208714613, 1511301096 /
      DATA LD( 7), LD( 8) / 1001001096, 1073901096 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014607 /
      DATA LW( 2,1), LW( 2,2) /   81311468,          0 /
      DATA LW( 3,1), LW( 3,2) / 1078816065, 1169806940 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107121, 1215909882 /
      DATA LW(12,1), LW(12,2) /  398107121, 1398911317 /
      DATA LW(13,1), LW(13,2) / 1336911495,  316005590 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1910710935 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1570200000 /
      DATA LW(16,1), LW(16,2) / 1337205967, 1426909504 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) / 1296103403,  515114373 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) / 1216500000,  463605985 /
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
      DATA LF(15), LF(16) / 1317408892,  417410355 /
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
      DATA LP( 3), LP( 4) / 1181702336,  223315348 /
      DATA LP( 5)         /  316005590             /
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
     1        'C',    'O',    'L',    'O',    'N',    'N',    'A'/
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
*IXLINE
      SUBROUTINE IXLINE (IQ,IQN,IL,IXN,IX)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. IXLINE V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT ...
C
C     IQ  = ABSOLUTE LINE NUMBER STARTING FROM ZERO.
C              I.E., NUMBER OF LINES FROM ABSOLUTE ZERO.
C     IQN = LINE OF LAST NEGATIVE DATUM IN SORTED DATA.
C              (ZERO IF ALL NUMBERS ARE POSITIVE.)
C     IL  = VALUE OF IQ FOR FIRST ORDERED OBSERVATION.
C     IXN = LINE OF FIRST POSITIVE DATUM IN SORTED DATA.
C
C     OUTPUT ...
C
C     IX  = THE LINE NUMBER FOR THE NUMBER TO BE DISPLAYED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
      IF (IQ.LE.IQN) GO TO 10
      IX = IQ + IXN
      RETURN
C
C     ..................................................................
C
  10  IX = IL - IQ + IONE
      RETURN
C
C     ==================================================================
C
      END
*JAPANE
      SUBROUTINE JAPANE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. JAPANE V 7.00  4/ 5/90. **
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
      DATA L(  3) / 1910914616 /
      DATA L(  4) / 1910914616 /
      DATA L(  5) /   81315796 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82513959 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514688 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /  860900882 /
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
      DATA L( 61) /  805113689 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255909297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) /  860400788 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) / 1426701107 /
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
      DATA L( 88) / 1586815843 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1388504152 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) / 1681215309 /
      DATA L( 96) /  318103165 /
      DATA L( 97) / 1681215309 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) /  817315309 /
      DATA L(103) /  140509522 /
      DATA L(104) /  424009316 /
      DATA L(105) / 1500306859 /
      DATA L(106) /  860406859 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /  135114004 /
      DATA L(115) /  844403907 /
      DATA L(116) /  470317741 /
      DATA L(117) /  577908606 /
      DATA L(118) /  668414082 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) / 1124714418 /
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
      DATA L(138) /  598108046 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  625100991 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) / 1500306670 /
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
      DATA L(168) /  870908428 /
      DATA L(169) /  870908428 /
      DATA L(170) /  952809734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) /  963001247 /
      DATA L(176) /  972404043 /
      DATA L(177) /  870908605 /
      DATA L(178) /  870908605 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  984316173 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) /  989417307 /
      DATA L(187) /  990014811 /
      DATA L(188) / 1551308065 /
      DATA L(189) /  992711372 /
      DATA L(190) /  996407079 /
      DATA L(191) /  999301054 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1001102016 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) /  805704141 /
      DATA L(197) /  805704152 /
      DATA L(198) / 1007602304 /
      DATA L(199) / 1007602539 /
      DATA L(200) / 1018413527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) /  973415827 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078508748 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1131816819 /
      DATA L(211) / 1025100788 /
      DATA L(212) / 1170912165 /
      DATA L(213) /  202715710 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) /  860406859 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) / 1461111442 /
      DATA L(220) / 1461810985 /
      DATA L(221) / 1216503486 /
      DATA L(222) /   92313689 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  160406561 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) /  860406859 /
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
      DATA L(245) / 1125300000 /
      DATA L(246) / 1402214580 /
      DATA L(247) /  608506917 /
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
      DATA L(259) /  805505976 /
      DATA L(260) / 1433101220 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1499603908 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) /  608615309 /
      DATA L(267) /  608615309 /
      DATA L(268) / 1443100000 /
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
     1      11300,   7102,   4631,   7082,  14406,  11664,  11004 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3835,   9724,   1242,   4797,   9524,  12159,  10746 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       6522,   2952,   8908,   8907,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1124110248, 1215910746 /
      DATA LD( 3), LD( 4) / 1224502403, 1864315309 /
      DATA LD( 5), LD( 6) / 1078407116,  117906959 /
      DATA LD( 7), LD( 8) /  992706959, 1065606959 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014580 /
      DATA LW( 2,1), LW( 2,2) /  252613986, 1574100000 /
      DATA LW( 3,1), LW( 3,2) /  127013168,          0 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  463514391 /
      DATA LW(12,1), LW(12,2) /  398107013, 1398911317 /
      DATA LW(13,1), LW(13,2) / 1169803645, 1461111442 /
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
      DATA LP( 5)         / 1461111442             /
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
     1        'C',    'O',    'L',    'U',    'M',    'N',    ' '/
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
*KEYBRD   
      SUBROUTINE KEYBRD       
C         
C **  NBS OMNITAB 1980 VERSION 7.10  2/20/81. KEYBRD V 7.00  4/30/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     INSTRUCTIONS ...        
C         
C     INTERACTIVE   L1 = 14,  L2 =  4   
C         
C        INTERACTIVE
C        INTERACTIVE  (N)  NUMBER OF CHARACTERS PER LINE    
C         
C        IF (N) IS NOT GIVEN, THEN N = 72 IS ASSUMED.  IN EITHER      
C           CASE THIS INSTRUCTION PUTS OMNITAB IN INTERACTIVE MODE.   
C        IF FATAL ERROR OCCURS, ERROR WILL BE PRINTED RIGHT AFTER     
C           INSTRUCTION IS TYPED AND EXECUTION CONTINUES.   
C        WHENEVER OMNITAB IS READY FOR NEXT INSTRUCTION IT WILL TYPE +
C         
C     WIDTH         L1 = 14,  L2 =  5   
C         
C        WIDTH (N) MAX NUMBER OF CHARACTERS PER LINE        
C           IN EITHER BATCH OR INTERACTIVE MODE.  
C           IF INSTRUCTION IS NOT USED, N = 72 IS ASSIGNED.
C         
C           NO. OF CHARACTERS PER RECORD ON TAPE WILL BE 80.
C           NO. OF CHARACTERS PER CARD WILL STILL BE 80.    
C           NO. OF CHARACTERS PER INPUT LINE WILL STILL BE 80.        
C         
C     BRIEF         L1 = 14,  L2 = 17   
C         
C     FULL          L1 = 14,  L2 = 18   
C         
C     TERMINAL      L1 = 14,  L2 = 19   
C         
C     LENGTH        L1 = 14,  L2 = 20   
C         
C        IF NDEMD = 0, LENGTH WILL BE SET TO MINUMUM OF (LENGTH,59) AND         
C           WILL AFFECT THE ON LINE PRINT, PLOT AND PAGE PLOT COMMANDS.         
C        IF NDEMD GT 0, LENGTH WILL BE SET BY THE COMMAND LENGTH AND  
C           WILL AFFECT THE ON LINE PRINT, PLOT AND PAGE PLOT COMMANDS.         
C         
C     REMOTE        L1 = 14   L2 = 21   
C         
C     LOCAL         L1 = 14   L2 = 22   
C         
C     CRT           L1 = 14   L2 = 23   
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER, 1974.      
C                   CURRENT VERSION -     APRIL, 1992.      
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE         
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C         
C         
C     ==================================================================        
C         
C                    ***   TYPE STATEMENTS   ***   
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1, NEWCRD*1
      CHARACTER LLA(14)*1     
      CHARACTER RMFILE*73
C         
C     ==================================================================        
C         
C                    ***   DATA STATEMENTS   ***   
C         
      DATA LLA(1), LLA(2), LLA(3), LLA(4), LLA(5), LLA(6), LLA(7),    
     1     LLA(8), LLA(9),LLA(10),LLA(11),LLA(12),LLA(13),LLA(14)     
     2/       '0',    '1',    '2',    '3',    '4',    '5',     '6',   
     3        '7',    '8',    '9',    ' ',    'E',    'F',     'P'/   
C         
C     ==================================================================        
C         
      IF (L2.GE.21) GO TO 140 
      IF (L2.GE.17 .AND. L2.LE.19) GO TO 70       
      IF (L2.EQ.20) GO TO 100 
      IF (NARGS.EQ.IZERO .AND. L2.LE.IFOUR) GO TO 20        
      IF (NARGS.EQ.IZERO .AND. L2.GT.IFOUR) GO TO 60        
      IF (NARGS.GT.IONE) CALL ERROR (221)         
C         
C     ARGUMENT OF WIDTH AND INTERACTIVE IS SPECIFIED.       
C         
      IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)     
      IF (IARGS(1).GT.15 .AND. IARGS(1).LT.133) GO TO 10    
      CALL ERROR (245)        
      RETURN        
C         
C     ..................................................................        
C         
  10  LWIDE = IARGS(1)        
      IF (L2.EQ.IFIVE) GO TO 30         
C         
C     INTERACTIVE.  
C         
  20  NDEMD  = IONE 
      IF (IPRINT .NE. NPRNT) IPRINT = KBDOUT         
  30  INUM   = IDIV (LWIDE,IPLACE,IND)  
      INUMA  = IDIV (INUM,ITEN,IND)     
      IF (INUMA.EQ.IZERO) INUMA = ITEN  
      INUMB  = MOD (INUM,ITEN)
      IFMTX(4) = LLA(INUMA+1) 
      IFMTX(5) = LLA(INUMB+1) 
      DO 40 I=1,80  
        NOCARD(I) = NOMNIT(I) 
  40  CONTINUE      
      RETURN        
C         
C     ..................................................................        
C         
C 50  LWIDE = NCW   
C     GO TO 20      
  60  CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
C     INSTRUCTION IS BRIEF, FULL, OR TERMINAL.    
C         
  70  IF (L2.EQ.18) GO TO 80  
      IF (L2.GT.18) GO TO 90  
C         
C     L1= 14, L2=17 INSTRUCTION IS BRIEF.         
C         
      ISBFT = IONE  
      RETURN        
C         
C     ..................................................................        
C         
C     L1=14, L2=18 INSTRUCTION IS FULL. 
C         
  80  ISBFT = IZERO 
      RETURN        
C         
C     L1=14, L2=19  INSTRUCTION IS TERMINAL.      
C         
  90  ISBFT = -IONE 
      GO TO 20      
C         
C     LENGTH.       
C         
C     ERROR CHECKING.         
C         
 100  IF (NARGS.GT.IZERO) GO TO 110     
      CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
 110  IF (NARGS.GT.IONE) CALL ERROR (221)         
      IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)     
      IF (IARGS(1).GT.IZERO) GO TO 120  
      CALL ERROR (205)        
      RETURN        
C         
 120  IF (NDEMD.GT.IZERO) GO TO 130     
C         
C     ..................................................................        
C         
C     SET LENGTH = MIN0(IARGS(1),NLENGT) SINCE NOT IN INTERACTIVE MODE.         
C         
C     LENGTH = MIN0 (IARGS(1),NLENGT)   
C     IF (IARGS(1).GT.NLENGT) CALL ERROR (254)    
C     NLSWT = IONE  
C     IF (LENGTH.EQ.NLENGT .AND. IARGS(1).NE.NLENGT) NLSWT = IZERO    
C     RETURN        
C         
C     ..................................................................        
C         
C     SET LENGTH = IARGS(1), SINCE IN INTERACTIVE MODE.     
C
 130  IF (IARGS(1).LT.IFIVE) THEN
        CALL ERROR (254)  
      ELSE
        LENGTH = IARGS(1)       
        NLOCRM = IONE
      ENDIF
      NLSWT  = IONE 
      RETURN        
C         
C     ..................................................................        
C         
C     INSTRUCTION IS REMOTE FOR L2 = 21,
C                    LOCAL  FOR L2 = 22, AND      
C                    CRT    FOR L2 = 23.
C         
 140  IF (NERROR.NE.IZERO) RETURN       
      IF (L2.NE.21) GO TO 150 
      IPRINT = MPRNT
C
C     PICK UP NAME OF REMOTE FILE NAME IF GIVEN.
C     OTHERWISE USE DEFAULT FILE NAME 'OMNITAB.OUT'.
C
      ITEMP = IZERO
      DO 143 I = 1,79
        IF (KARD(I) .EQ. 44 .AND. ITEMP .EQ. IONE) GO TO 144
        IF (KARD(I) .NE. 44) ITEMP = IONE
 143  CONTINUE     
      I    = 79
 144  IPOS = I    
 145  IF (KARD(IPOS) .EQ. 46) THEN
C
C     NO FILE NAME GIVEN
C
        RMFILE = 'OMNITAB.OUT'
        GO TO 148
C
C     PICK UP FILE NAME
C
      ELSE
        IPOS = IPOS + IONE  
        IF(KARD(IPOS-1) .EQ. 44) GO TO 145
      END IF
C
        WRITE (ISCRT,160) (NEWCRD(I-3),I=IPOS,80)
        BACKSPACE ISCRT
        READ (ISCRT,170) RMFILE
        BACKSPACE ISCRT
C
C     OPEN REMOTE FILE
C
 148  OPEN (UNIT = MPRNT, FILE = RMFILE, access = 'APPEND')
      IF ( NLOCRM.EQ.IZERO) LENGTH = 50
      ISBOLD = ISBFT
      ISBFT  = IZERO
      NCRT   = IZERO
      RETURN        
C         
C     ..................................................................        
C         
 150  IF (L2.EQ.22) IPRINT = NPRNT      
      NCRT   = IONE         
      IF ( NLOCRM.EQ.IZERO) LENGTH = NLENGT
      ISBFT  = ISBOLD
      RETURN
C         
C     ==================================================================        
C         
C                          ***   FORMAT STATEMENTS   ***    
C         
 160  FORMAT ( 80A1)
 170  FORMAT (   A64)
C         
C     ==================================================================        
C         
      END 
*LAMCDF
      SUBROUTINE LAMCDF (X,ALAMBA,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LAMCDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE (TUKEY) LAMBDA DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = ALAMBA.
C              IN GENERAL, THE PROBABILITY DENSITY FUNCTION
C              FOR THIS DISTRIBUTION IS NOT SIMPLE.
C              THE PERCENT POINT FUNCTION FOR THIS DISTRIBUTION IS
C              G(P) = ((P**ALAMBA)-((1-P)**ALAMBA))/ALAMBA
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALAMBA = THE SINGLE PRECISION VALUE OF LAMBDA
C                                (THE TAIL LENGTH PARAMETER).
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE TUKEY LAMBDA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER = ALAMBA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--FOR ALAMBA NON-POSITIVE, NO RESTRICTIONS ON X.
C                 --FOR ALAMBA POSITIVE, X SHOULD BE BETWEEN (-1/ALAMBA)
C                   AND (+1/ALAMBA), INCLUSIVELY.
C
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--HASTINGS, MOSTELLER, TUKEY, AND WINDSOR,
C                 'LOW MOMENTS FOR SMALL SAMPLES:  A COMPARATIVE
C                 STUDY OF ORDER STATISTICS', ANNALS OF
C                 MATHEMATICAL STATISTICS, 18, 1947,
C                 PAGES 413-426.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 42-44, 53-58.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --MAY       1974.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, ALAMBA, CDF
      REAL             PDEL, PLOWER, PMAX, PMID, PMIN, PUPPER
      REAL             XCALC, XMAX, XMIN
      REAL             FDIV, FEXP
      REAL             SPCA, SPCB, SPCC
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 30 /
C
      DATA SPCA / 0.0001   /
      DATA SPCB / 0.001    /
      DATA SPCC / 0.000001 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IND = IZERO
      IF (ALAMBA.LE.RZERO) GO TO 20
      XMAX= FDIV (RONE,ALAMBA,JIND)
      XMIN = -XMAX
      IF (X.GE.XMIN .AND. X.LT.XMAX) GO TO 10
        IND = IONE
        IF (X.LT.XMIN) CDF = RZERO
        IF (X.GT.XMAX) CDF = RONE
        RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  10  XMAX = FDIV (RONE,ALAMBA,JIND)
      XMIN = -XMAX
      IF (X.LE.XMIN) CDF = RZERO
      IF (X.GE.XMAX) CDF = RONE
      IF (X.LE.XMIN .OR. X.GE.XMAX) RETURN
C
  20  IF (-SPCA.LT.ALAMBA .AND. ALAMBA.LT.SPCA) GO TO 30
      GO TO 50
C
  30  IF (X.GE.RZERO) GO TO 40
      CDF = FDIV (FEXP(X),RONE+FEXP(X),JIND)
      RETURN
C
C     ..................................................................
C
  40  CDF = FDIV (RONE,RONE+FEXP(-X),JIND)
      RETURN
C
C     ..................................................................
C
  50  IF (-SPCB.LT.ALAMBA .AND. ALAMBA.LT.SPCB) GO TO 30
      PMIN = RZERO
      PMID = RHALF
      PMAX = RONE
      PLOWER = PMIN
      PUPPER = PMAX
      ICOUNT = IZERO
  60  XCALC = FDIV (PMID**ALAMBA-(RONE-PMID)**ALAMBA,ALAMBA,JIND)
      IF (XCALC.EQ.X) GO TO 90
      IF (XCALC.GT.X) GO TO 70
      PLOWER = PMID
      PMID = FDIV (PMID+PUPPER,RTWO,JIND)
      GO TO 80
  70  PUPPER = PMID
      PMID = FDIV (PMID+PLOWER,RTWO,JIND)
  80  PDEL = ABS (PMID-PLOWER)
      ICOUNT = ICOUNT + IONE
      IF (PDEL.LT.SPCC .OR. ICOUNT.GT.ICA) GO TO 90
      GO TO 60
C
  90  CDF = PMID
      RETURN
C
C     ==================================================================
C
      END
*LAMPDF
      SUBROUTINE LAMPDF (X,ALAMBA,PDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LAMPDF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE (TUKEY) LAMBDA DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = ALAMBA.
C              IN GENERAL, THE PROBABILITY DENSITY FUNCTION
C              FOR THIS DISTRIBUTION IS NOT SIMPLE.
C              THE PERCENT POINT FUNCTION FOR THIS DISTRIBUTION IS
C              G(P) = ((P**ALAMBA)-((1-P)**ALAMBA))/ALAMBA
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALAMBA = THE SINGLE PRECISION VALUE OF LAMBDA
C                                (THE TAIL LENGTH PARAMETER).
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE TUKEY LAMBDA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER = ALAMBA.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--FOR ALAMBA NON-POSITIVE, NO RESTRICTIONS ON X.
C                 --FOR ALAMBA POSITIVE, X SHOULD BE BETWEEN (-1/ALAMBA)
C                   AND (+1/ALAMBA), INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--LAMCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--HASTINGS, MOSTELLER, TUKEY, AND WINDSOR,
C                 'LOW MOMENTS FOR SMALL SAMPLES:  A COMPARATIVE
C                 STUDY OF ORDER STATISTICS', ANNALS OF
C                 MATHEMATICAL STATISTICS, 18, 1947,
C                 PAGES 413-426.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 42-44, 53-58.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --AUGUST    1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C
C               ADPATED TO OMNITAB BY -
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
      REAL             X, ALAMBA, PDF
      REAL             CDF, SF, XMAX, XMIN
      REAL             FDIV
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (ALAMBA.LE.RZERO) GO TO 20
      XMAX = FDIV ( RONE,ALAMBA,JIND)
      XMIN = -XMAX
      IF (X.GE.XMIN .AND. X.LE.XMAX) GO TO 10
        IND = IONE
        PDF = RZERO
        RETURN
C
C     ==================================================================
C
C     ---   START POINT   ----------------------------------------------
C
  10  IF (X.GT.XMIN .AND. X.LT.XMAX)      GO TO 20
      IF (X.EQ.XMIN .AND. ALAMBA.LT.RONE) PDF = RZERO
      IF (X.EQ.XMAX .AND. ALAMBA.LT.RONE) PDF = RZERO
      IF (X.EQ.XMIN .AND. ALAMBA.EQ.RONE) PDF = RHALF
      IF (X.EQ.XMAX .AND. ALAMBA.EQ.RONE) PDF = RHALF
      IF (X.EQ.XMIN .AND. ALAMBA.GT.RONE) PDF = RONE
      IF (X.EQ.XMAX .AND. ALAMBA.GT.RONE) PDF = RONE
      RETURN
C
C     ..................................................................
C
  20  CALL LAMCDF (X,ALAMBA,CDF,IND)
      SF  = CDF ** (ALAMBA-RONE) + (RONE-CDF) ** (ALAMBA-RONE)
      PDF = FDIV ( RONE,SF,JIND)
      RETURN
C
C     ==================================================================
C
      END
*LAMPPF
      SUBROUTINE LAMPPF (P,ALAMBA,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LAMPPF V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE (TUKEY) LAMBDA DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = ALAMBA.
C              IN GENERAL, THE PROBABILITY DENSITY FUNCTION
C              FOR THIS DISTRIBUTION IS NOT SIMPLE.
C              THE PERCENT POINT FUNCTION FOR THIS DISTRIBUTION IS
C              G(P) = ((P**ALAMBA)-((1-P)**ALAMBA))/ALAMBA
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALAMBA = THE SINGLE PRECISION VALUE OF LAMBDA
C                                (THE TAIL LENGTH PARAMETER).
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF FOR THE TUKEY LAMBDA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER = ALAMBA.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--IF ALAMBA IS POSITIVE,
C                   THEN P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
C                   IF ALAMBA IS NON-POSITIVE,
C                   THEN P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231,
C                 PAGES 53-58.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --HASTINGS, MOSTELLER, TUKEY, AND WINDSOR,
C                 'LOW MOMENTS FOR SMALL SAMPLES:  A COMPARATIVE
C                 STUDY OF ORDER STATISTICS', ANNALS OF
C                 MATHEMATICAL STATISTICS, 18, 1947,
C                 PAGES 413-426.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             P, ALAMBA, PPF
      REAL             FDIV, FLOG
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
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IND = IZERO
      IF (P     .GT.RZERO .AND. P.LT.RONE) GO TO 10
      IF (ALAMBA.GT.RZERO .AND. P.EQ.RZERO) GO TO 10
      IF (ALAMBA.GT.RZERO .AND. P.EQ.RONE)   GO TO 10
        IND = IONE
        RETURN
C
C     ==================================================================
C
C     ---   START POINT   ----------------------------------------------
C
  10  IF((-SPCA).LT.ALAMBA .AND. ALAMBA.LT.SPCA) GO TO 20
      GO TO 30
C
  20  PPF = FLOG ( FDIV (P,RONE-P,JIND) )
      RETURN
C
C     ..................................................................
C
  30  PPF = FDIV (P**ALAMBA-(RONE-P)**ALAMBA,ALAMBA,JIND)
      RETURN
C
C     ==================================================================
C
      END
*LIST
      SUBROUTINE LIST (K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   LIST V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     K=0   INSTRUCTION EXECUTED IS LIST
C     K=1   INSTRUCTION EXECUTED IS NOLIST
C
C     LIST (WITH NO ARGUMENTS) = LIST 3
C     LIST 0 = NO LISTING
C     LIST 1 = LIST ONLY INFORMATIVE DIAGNOSTIC MESSAGES.
C     LIST 2 = LIST ONLY ARITHMETIC FAULT MESSAGES.
C     LIST 3 = LIST BOTH AITHMETIC AND INFORMATIVE MESSAGES.
C     LIST 4 = SUPPRESS BOTH ARITH. FAULTS AND INFORMATIVE DIAGNOSTICS
C
C     IF A FATAL ERROR OCCURS, LLIST IS SET TO AND KEPT AT 3
C
C               WRITTEN BY -
C                      RUTH N. VARNER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MARCH, 1968.
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
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
C
C     ==================================================================
C
      IF (K.EQ.IZERO) GO TO 20
C
C     NO LIST OR NOLIST
C
      IARGS(1) = IZERO
  10  IF (NERROR.EQ.IZERO) LLIST = IARGS(1)
      WRITE (ISCRT,30) IARGS(1)
      RETURN
C
C     ..................................................................
C
  20  IF (NARGS.EQ.IZERO .OR. IARGS(1).LT.IZERO .OR. IARGS(1).GT.IFOUR)
     1     IARGS(1) = ITHRE
      GO TO 10
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  30  FORMAT (1H,,I1,82X)
C
C     ==================================================================
C
      END
*LOCAT
      SUBROUTINE LOCAT (ISTPV,ISTRNG,LSTRNG,IFILE,LFILE,IND,IND1,LOCPST)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  LOCAT V 7.00  5/ 2/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM TO LOCATE A STRING (ISTRNG) OF LENGTH (LSTRNG) IN FILE
C          (IFILE) OF LENGTH (LFILE).
C
C     IND    =   0, IF STRING NOT FOUND.
C     IND    = (N), IF STRING FOUND N TIMES.
C     IND1   =   0, IF NO PARTIAL FIND AT END OF IFILE.
C     IND1   = (N), IF FIRST N CHARACTERS OF ISTRNG FOUND
C                      AT END OF IFILE.
C     LOCPST = LOCATION IN IFILE OF FIRST CHARACTER IN ISTRNG
C                      FOR I=1,IND.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - MARCH, 1973.
C                   CURRENT VERSION -   MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ISTRNG(*), IFILE(*), LOCPST(*)
C
C        ISTRNG, IFILE AND LOCPST MUST BE DIMENSIONED IN MAIN PROGRAM.
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C     INITIALIZE
C
       IND = IZERO
      IND1 = IZERO
         K = IONE
C
      IF (LFILE.LT.IONE .OR. LSTRNG.LT.IONE) GO TO 70
C
C     ..................................................................
C
      DO 60 I=1,LFILE
        IF (IFILE(K).EQ.46) GO TO 70
        IF (IFILE(K).NE.ISTPV) GO TO 40
C
C       FIRST CHARACTER IN ISTRNG FOUND, LOOK FOR REMAINING CHARACTERS.
C
        L = K
        IF (LSTRNG.EQ.IONE) GO TO 20
        DO 10 J=2,LSTRNG
          K = K + IONE
          M = J
          IF (K.GT.LFILE) GO TO 30
          IF (IFILE(K).NE.ISTRNG(M)) GO TO 50
C
C         NEXT CHARACTER FOUND.
C
  10  CONTINUE
C
C       ISTRNG FOUND.
C
  20    IND = IND + IONE
        LOCPST(IND) = L
        GO TO 40
C
C       PARTIAL ISTRNG FOUND.
C
  30    IND1 = M - IONE
        GO TO 70
  40    K = K + IONE
  50    IF (K.GT.LFILE) GO TO 70
  60  CONTINUE
  70  RETURN
C
C     ==================================================================
C
      END
*LOCATE
      FUNCTION LOCATE (L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LOCATE V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION SEARCHES THE LIST OF STORED COMMANDS TO SEE IF ONE
C        WITH STATEMENT NUMBER L EXISTS.  IF IT DOES, RETURN ITS
C        LOCATION.  IF IT DOESN'T EXIST, RETURN THE NEGATIVE OF THE
C        LOCATION OF THE NEXT HIGHER STATEMENT NUMBER.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
C
      REAL             AL
C
C     ==================================================================
C
      I = IONE
      AL = L
  10  IF (COM(I)-AL) 20,30,40
  20  I = I + IFIX ( COM(I+1) )
      GO TO 10
C
  30  LOCATE = I
      RETURN
C
C     ..................................................................
C
  40  LOCATE = -I
      RETURN
C
C     ==================================================================
C
      END
*LOFIND
      SUBROUTINE LOFIND (X,N,M,RINDEX)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LOFIND V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ASSIGNS THE VALUE 1 TO RINDEX(I) WHENEVER
C        ANY ROW OF X IS IDENTICAL TO THE FIRST ROW OF X.
C           SIMILARLY, IT ASSIGNS THE VALUE 2 TO THE NEXT GROUP
C           OF IDENTICAL ROWS, AND SO ON.
C
C     THIS PROGRAM USED TO DETERMINE NUMBER OF REPLICATIONS FOR
C        LACK OF FIT TEST PERFORMED BY A FIT OR POLYFIT INSTRUCTION.
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
      REAL             RINDEX(N), X(N,M)
      REAL             COUNT, REALN
C
C     ==================================================================
C
      REALN = N
      DO 10 I=1,N
        RINDEX(I) = REALN
  10  CONTINUE
C
      COUNT = RONE
      IEND = N - IONE
      DO 30 I=1,IEND
        II = I
        IF (RINDEX(I).LT.COUNT) GO TO 30
        RINDEX(I) = COUNT
        JBEG = I + IONE
        DO 20 J=JBEG,N
          JJ = J
          CALL MCHROW (X,N,M,II,JJ,IND)
          IF (IND.EQ.IZERO) GO TO 20
          RINDEX(J) = COUNT
  20    CONTINUE
        COUNT = COUNT + RONE
  30  CONTINUE
      IF (RINDEX(N).GE.COUNT) RINDEX(N) = COUNT
      RETURN
C
C     ==================================================================
C
      END
*LOOKUP
      SUBROUTINE LOOKUP (LNAME)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. LOOKUP V 7.00  1/ 9/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     LNAME = 0, IF COMMAND IS NOT FOUND.
C     LNAME = 1, FOR ONE WORD COMMANDS.
C     LNAME = 2, FOR TWO WORD COMMANDS.
C     LNAME = 3, FOR THREE WORD COMMANDS.
C     LNAME = 4, FOR FOUR WORD COMMANDS.
C     LNAME = 5, IF COMMAND IS NOT TO BE SCANNED
C                   (I.E., LABEL, PRINT NOTE ETC.)
C
C     CHECK TO SEE IF FIRST WORD AND SOMETIMES SECOND WORD
C        ON INSTRUCTION CARD IS A LEGITIMATE COMMAND.
C
C     IF COMMAND IS     FOUND, L1 AND L2 ARE ASSIGNED A VALUE
C     IF COMMAND IS NOT FOUND IN THE DICTIONARY, L1 IS SET EQUAL TO ZERO
C
C               REWRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   MARCH, 1968.
C                   CURRENT VERSION - JANUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD
      COMMON /ARRAYB/ IALPH(6), ICL(10,2), ICP(6), ID(8,2) 
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /LANGUE/ LANGC, LANGP
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA   /     100000 /
C
C     THE FOLLOWING DATA STATEMENT MAY HAVE TO BE CHANGED WHENEVER
C       NEW COMMANDS ARE INSERTED IN VECTOR IR(.,.).
C
C     THE VARIABLE IRINCR MUST = THE SUBSCRIPT NUMBER OF THE COMMAND
C       INCREMENT.
C
      DATA IRINCR   /        138 /
C
C
C     NAME(1) AND NAME(2) FOR UNIT.
C
      DATA INTCA / 1569614580 /
C
C     NAME(1) AND NAME(2) FOR TAPE.
C
      DATA INTCB / 1462303645 /
C
C     NAME(1) AND NAME(2) FOR CALCOMP.
C
      DATA INTCC /  222602605 /
C
C     NAME(1) AND NAME(2) FOR EXPONENTIAL.
C
      DATA INTCD /  430911318 /
C
C     NAME(1) AND NAME(2) FOR EXTREME.
C
      DATA INTCE /  431313270 /
C
C     NAME(1) AND NAME(2) FOR ZETA.
C
      DATA INTCF / 1910900729 /
C
C     ==================================================================
C
      L1 = IZERO
      LNAME  = IZERO
      NAME12 = ICA * NAME(1) + NAME(2)
      NAME34 = ICA * NAME(3) + NAME(4)
C
C     CHECK TO SEE IF COMMAND IS INCREMENT.
C
      IF (NAME12.NE.IR(IRINCR,1)) GO TO 10
      LNAME = IONE
      L1    = IDIV (IR(IRINCR,2),IHRD,IND)
      L2    = MOD (IR(IRINCR,2),IHRD)
      RETURN
C
C     ..................................................................
C
  10  IF (NAME12.GT.IR(NIRQTR,1)) GO TO 20
C
C     COMMAND IS IN 1ST QUARTER OF IR(.,.).
C
      ISTART = IONE
      ISTOP  = NIRQTR
      GO TO 50
  20  IF (NAME12.GT.IR(NIRMID,1)) GO TO 30
C
C     COMMAND IS IN 2ND QUARTER OF IR(.,.).
C
      ISTART = NIRQTR + IONE
      ISTOP  = NIRMID
      GO TO 50
  30  IF (NAME12.GT.IR(NIRTRD,1)) GO TO 40
C
C     COMMAND IS IN 3RD QUARTER OF IR(.,.).
C
      ISTART = NIRMID + IONE
      ISTOP  = NIRTRD
      GO TO 50
C
C     COMMAND IS IN 4TH QUARTER OF IR(.,.).
C
  40  ISTART = NIRTRD + IONE
      ISTOP  = NIR
  50  DO 60 I=ISTART,ISTOP
        IF (NAME12.NE.IR(I,1)) GO TO 60
        L1 = IDIV (IR(I,2),IHRD,IND)
        L2 = MOD (IR(I,2),IHRD)
        LNAME = IONE
        IF (L1.EQ.35) GO TO 320
        IF (L1.EQ.25 .AND. L2.EQ.ITHRE) GO TO 230
C
C      THE FOLLOWING CARD IS NEEDED ONLY FOR SET TAPE OR UNIT COMMAND.
C
        IF (L1.EQ.13 .AND. L2.EQ.ITWO) GO TO 100
        IF (L1.EQ.15 .AND. L2.EQ.   8) GO TO 250
        RETURN
  60  CONTINUE
C
      DO 70 I=1,NID
        IF (NAME12.NE.ID(I,1)) GO TO 70
        L1 = ID(I,2)
        LNAME = IONE
        IF (L1.EQ.IFIVE) GO TO 100
        GO TO 180
  70  CONTINUE
C
      DO 80 I=1,NIRD
        IF (NAME12.NE.IRD(I,1) .OR. NAME34.NE.IRD(I,2)) GO TO 80
        L1 = IDIV (IRD(I,3),IHRD,IND)
        L2 = MOD (IRD(I,3),IHRD)
        LNAME = ITWO
        IF (L1.EQ.51 .AND. L2.EQ.ITWO .OR. L1.EQ.53 .AND. L2.EQ.ITWO)
     1     LNAME = ITHRE
        RETURN
  80  CONTINUE
C
C     THE NEXT 9 STATEMENTS ARE USED FOR THE MULTILINGUAL VERSION.
C
      IF (LANGP.EQ.IZERO) GO TO 100
      DO 90 I=1,NIL
        IF (NAME12.NE.IL(I,1)) GO TO 90
        L1 = IDIV (IL(I,2),IHRD,IND)
        L2 = MOD (IL(I,2),IHRD)
        IF (L2.NE.14) LANGC = L2
        NAME(1) = IONE
        RETURN
  90  CONTINUE
C
C      THE FOLLOWING CARDS ARE NEEDED ONLY FOR TAPE OR UNIT COMMANDS.
C
 100  IF (NAME34.EQ.INTCA) GO TO 110
      IF (NAME34.EQ.INTCB) GO TO 110
      IF (L1.LT.IFIVE) GO TO 160
      IF (L1.EQ.IFIVE) GO TO 180
      IF (L1.GT.IFIVE) GO TO 340
C
 110  DO 150 I=1,NITP
        IF (NAME12.NE.ITP(I,1)) GO TO 150
        L1 = IDIV (ITP(I,2),IHRD,IND)
        L2 = MOD (ITP(I,2),IHRD)
        J = IFIVE
        LNAME = ITWO
        IF (L1.EQ.50) GO TO 190
 120    DO 130 JA=1,6
          IF (NAME(J).NE.IALPH(JA)) GO TO 130
          L2 = JA + L2
          LNAME = LNAME + IONE
          GO TO 140
 130    CONTINUE
        IF (J.EQ.IFIVE) GO TO 300
 140    IF (J.NE.IFIVE) RETURN
        L2 = (L2-IONE) * 7 + IONE
        J = 7
        GO TO 120
 150  CONTINUE
C
C     **** FOLLOWING STATEMENTS ARE FOR CALCOMP ************************
C
 160  IF (NAME12.NE.INTCC .AND. NAME12.NE.INTCF) GO TO 250
      DO 170 I=1,8
        IF (NAME34.NE.ICL(I,1)) GO TO 170
        L1 = IDIV (ICL(I,2),IHRD,IND)
        L2 = MOD (ICL(I,2),IHRD)
        J = IFIVE
        LNAME = ITWO
        IF (L2.EQ.15) GO TO 190
        RETURN
 170    CONTINUE
C
C     ***  END OF CALCOMP COMMANDS  ************************************
C
      GO TO 250
C
 180  L2 = IONE
      IF (L1.EQ.IONE) GO TO 210
      IF (L1.EQ.13) RETURN
      J = ITHRE
 190  DO 200 I=1,6
        IF (NAME(J).NE.IALPH(I)) GO TO 200
        L2 = L2 + I
        LNAME = LNAME + IONE
        RETURN
 200  CONTINUE
C
      IF (L1.NE.ITWO) RETURN
      IF (L2.NE.IONE) RETURN
      IF (NAME(3).NE.NL(7) .OR. NAME(4).NE.NL(8)) RETURN
C
C     THE COMMAND IS PRINT NOTE.
C
      L1 = 13
      L2 = 13
      LNAME = IFIVE
      RETURN
C
C     ..................................................................
C
 210  DO 220 I=1,5
        IF (NAME(3).NE.NALPH(I)) GO TO 220
        L2 = I + ITWO
        LNAME = ITWO
        RETURN
 220  CONTINUE
      RETURN
C
C     ..................................................................
C
 230  DO 240 I=1,6
        IF (NAME(3).NE.ICP(I)) GO TO 240
        L2 = I + IFIVE
        LNAME = ITWO
        RETURN
 240  CONTINUE
      RETURN
C
C     ..................................................................
C
C     SEARCH  FOR TWO WORD STATISTICAL COMMANDS:  DISTRIBUTION PROPERTY
C     L1 IS ASSIGNED TO PROPERTY
C     L2 IS ASSIGNED TO DISTRIBUTION
C
C     PROPERTY: THERE ARE TEN PROPERTIES
C       L1 = 55     PROPERTIES  DENSITY AND CUMULATIVE
C       L1 = 56     PROPERTIES  PERCENTILE AND RANDOM
C       L1 = 57     PROPERTIES  PLOT AND ANALYSIS
C       L1 = 58     PROPERTIES  CONFIDENCE AND TOLERANCE
C       L1 = 59     PROPERTIES  9 AND 10
C
C     DISTRIBUTION:   30  DISTRIBUTIONS
C       FOR PROPERTIES 1, 3, 5, 7, 9  (ODD)
C       L2 = 1,...,31
C       FOR PROPERTIES  2, 4, 6, 8, 10  (EVEN)
C       L2 = 32,...,62
C
 250  DO 260 I=1,NPROP
        IF (NAME34.EQ.IPROP(I)) GO TO 280
 260  CONTINUE
C
      IF (L1.EQ.15 .AND. L2.EQ.8) RETURN
      IF (NAME12.NE.INTCD) GO TO 270
      L1 = 12
      L2 = 18
      LNAME = IONE
      RETURN
C
C     ..................................................................
C
 270  IF (NAME12.NE.INTCE) GO TO 300
      L1 = 29
      L2 = IFOUR
      LNAME = IONE
      RETURN
C
C     ..................................................................
C
 280  L3 = IDIV (I-IONE,ITWO,IND)
      L1 = L3 + 55
      L3 = MOD (I-IONE,ITWO)
      DO 290 I=1,NDIST
        IF (NAME12.EQ.IDIST(I)) GO TO 310
 290  CONTINUE
C
 300  L1 = IZERO
      LNAME = IZERO
      RETURN
C
C     ..................................................................
C
 310  L2 = I + 31*L3
      LNAME = ITWO
      RETURN
C
C     ..................................................................
C
C     FIRST WORD OF COMMAND IS TABLE, NOW DETERMINE SECOND WORD.
C
 320  DO 330 I=1,NITB
        IF (NAME34.NE.ITB(I)) GO TO 330
        L2 = I + L2
        LNAME = ITWO
        RETURN
 330  CONTINUE
C
      L1 = IZERO
      L2 = IZERO
      LNAME = IZERO
      RETURN
C
C     ..................................................................
C
 340  IF (L1.NE.13) GO TO 160
      RETURN
C
C     ==================================================================
C
      END
*LSQ
      SUBROUTINE LSQ (N,M,NR,X,Y,W,H,C,IT,LC,B,Z,R,T,V,S,E,Q,F,P,A,ID,D)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.    LSQ V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE LSQ COMPUTES SOLUTIONS TO LINEAR LEAST SQUARES
C        PROBLEMS USING A MODIFIED GRAM-SCHMIDT ALGORITHM WITH
C        ITERATIVE REFINEMENT OF THE SOLUTION.
C
C     SUBROUTINES PDECOM, SLVE AND PINVRT ARE BASED ON ...
C        (1) ITERATIVE REFINEMENT OF LINEAR LEAST SQUARES SOLUTIONS II,
C            BY AKE BJORCK, BIT, VOL. 8 (1968), PP. 8-30.
C        (2) SOLUTIONS TO WEIGHTED LEAST SQUARES PROBLEMS BY MODIFIED
C            GRAM-SCHMIDT WITH ITERATIVE REFINEMENT, BY ROY H. WAMPLER,
C            ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE, VOL. 5 (1979),
C            TO APPEAR.
C
C     INPUT PARAMETERS ...
C
C       N     NUMBER OF OBSERVATIONS.
C       M     NUMBER OF UNKNOWN COEFFICIENTS (M LESS THAN OR EQUAL TO
C                N).
C       NR    NUMBER OF MAXIMUM ROWS IN X.
C       X     MATRIX (N BY M) OF INDEPENDENT VARIABLES WHICH ARE TO BE
C                FITTED, STORED AS A VECTOR OF LENGTH (N*M BY 1).  SEE
C                NOTES BELOW ON INPUT VECTOR LC FOR ADDITIONAL DETAILS.
C       Y     VECTOR OF OBSERVATIONS (N BY 1).
C       W     VECTOR OF WEIGHTS (N BY 1).
C       H     CONSTANT USED FOR WEIGHTS INSTEAD OF VECTOR W(.).
C                IF H = 0, VECTOR W IS USED FOR WEIGHTS, OTHERWISE
C                          WEIGHTS = H.
C                Y MID-RANGE HAS BEEN SUBTRACTED FROM Y-VECTOR IN THE
C                CALLING PROGRAM WHENEVER YMIN (SMALLEST RESPONSE) AND
C                YMAX (LARGEST RESPONSE) AGREE TO AT LEAST 0.5
C                DIGITS, AND
C                  (1) IT = 1 (POLYNOMIAL TYPE FIT), OR
C                  (2) IT = 2 (NON-POLYNOMIAL TYPE FIT) AND FIRST
C                      INDEPENDENT VECTOR OF X-MATRIX CONTAINS ALL ONES.
C                OTHERWISE, C EQUALS ZERO.
C                SUBTRACTION OF C FROM Y-VECTOR AFFECTS CERTAIN OUTPUT
C                QUANTITIES --
C                  (1) B(1), FIRST COEFFICIENT.
C                  (2) A(1), FIRST COEFFICIENT FROM REFIT.
C                  (3) S(1), FIRST SQUARED FOURIER COEFFICIENT.
C                THE VALUE OF S(1) IS ADJUSTED WITHIN THIS SUBROUTINE,
C                BUT THE VALUES OF B(1) AND A(1) ARE NOT ADJUSTED BEFORE
C                EXIT FROM THIS SUBROUTINE.
C       IT    PARAMETER WHICH SPECIFIES WHETHER OR NOT A POLYNOMIAL TYPE
C                FIT IS TO BE PERFORMED.
C                IT = 1 INDICATES POLYNOMIAL TYPE.
C                IT = 2 INDICATES NON-POLYNOMIAL TYPE.
C
C       LC    VECTOR (M BY 1) WHICH GIVES THE LOCATION WITHIN THE ARRAY
C                X OF THE INDEPENDENT VARIABLES TO WHICH THE
C                OBSERVATIONS ARE TO BE FITTED.
C
C             IF IT = 1, THE FUNCTION TO BE FITTED IS A POLYNOMIAL
C                HAVING THE FORM
C
C                Y(I) = B(1) + B(2)*Z(I) + B(3)*Z(I)**2 + ...
C                            + B(M)*Z(I)**(M-1) + ERROR, I=1,2,...,N.
C
C             IF LC(1) = J, THEN THE I-TH ELEMENT OF VECTOR Z IS LOCATED
C                AT X(JJ), WHERE JJ = (J-1)*NR + I.  POWERS OF Z ARE
C                GENERATED WHENEVER NEEDED.
C
C             IF IT = 2, THE FUNCTION TO BE FITTED HAS THE FORM
C
C                Y(I) = B(1)*Z1(I) + B(2)*Z2(I) + ... + B(M)*ZM(I) +
C                                                 ERROR, I=1,2,...,N.
C                (HERE THE M INDEPENDENT VARIABLES ARE DENOTED BY
C                Z1, Z2, ..., ZM.)
C
C             IF LC(J) = K, THEN THE I-TH ELEMENT OF VECTOR ZJ IS
C                LOCATED AT X(JJ) WHERE JJ = (K-1)*NR + I.
C
C     OUTPUT PARAMETERS ...
C
C       B     VECTOR OF COEFFICIENTS (M BY 1).
C       Z     VECTOR OF RESIDUALS (N BY 1).
C       R     UNSCALED COVARIANCE MATRIX, EQUAL TO INVERSE OF
C                (X-TRANSPOSE)*W*X (STORED AS A VECTOR OF LENGTH
C                (M+1)*(M+2)/2).
C       T     VECTOR OF STANDARD DEVIATIONS OF COEFFICIENTS (M+1 BY 1).
C       V     VECTOR OF STANDARD DEVIATIONS OF PREDICTED VALUES
C                (N BY 1).
C       S     VECTOR OF SQUARED FOURIER COEFFICIENTS (M+2 BY 1).  THE
C                FIRST M ELEMENTS OF THIS ARRAY ARE SUMS OF SQUARES
C                WHICH CAN BE USED IN AN ANALYSIS OF VARIANCE.  THE
C                LAST TWO ELEMENTS OF S ARE NOT COMPUTED IN THIS SUB-
C                ROUTINE BUT ARE RESERVED FOR QUANTITIES TO BE COMPUTED
C                IN THE CALLING PROGRAM.
C       E     RESIDUAL SUM OF SQUARES.
C       A     VECTOR OF COEFFICIENTS NEEDED FOR ACCURATE DIGITS
C                (M BY 1).  OBTAINED BY FITTING PREDICTED VALUES.
C       ID    = - NI, IF ISCALE IS 0
C                 NI, IF ISCALE IS 1,
C                     WHERE NI = NUMBER OF ITERATIONS REQUIRED IN
C                     SUBROUTINE SLVE.
C                     ISCALE = 0 MEANS DATA WERE NOT SCALED.
C                     ISCALE = 1 MEANS DATA WERE SCALED.
C       D     AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
C                SOLUTION AND THE FIRST ITERATION (IN SUBROUTINE SLVE).
C
C     INTERNAL PARAMETERS ...
C
C       Q     VECTOR USED AS WORK AREA (N*(M+1) BY 1).
C       F     VECTOR USED AS WORK AREA (M+1 BY 1).
C       P     VECTOR FOR SAVING RESIDUALS FROM FIRST CALL TO SLVE
C                (N BY 1).
C
C     THE INPUT ARRAYS X, Y AND W ARE LEFT UNCHANGED BY SUBROUTINE LSQ.
C
C     THE ARRAYS R, T, V AND S ARE USED INTERNALLY BEFORE BEING USED
C        FOR OUTPUT.
C
C     STRUCTURE CHART ...
C
C                      ..........
C                      .   LSQ  .
C                      ..........
C                          .
C                          .
C           ..............................................
C           .            .       .       .               .
C           .            .       .       .               .
C     ..........         .  ...........  .          ..........
C     . SCALE  .         .  . PDECOM  .  .          .  SLVE  .
C     ..........         .  ...........  .          ..........
C                        .               .
C                        .               .
C                   ..........      ..........
C                   . SDPRED .      . PINVRT .
C                   ..........      ..........
C
C     SUBROUTINES CALLED BY SUBROUTINE LSQ ARE ...
C        SCALE
C        PDECOM
C        SLVE
C        SDPRED
C        PINVRT
C
C     PRECISION OF THE ARITHMETIC CALCULATIONS ...
C        SINGLE PRECISION ARITHMETIC IS USED FOR ALL CALCULATIONS EXCEPT
C        THE DOUBLE PRECISION ACCUMULATION OF INNER PRODUCTS.  (THE
C        VARIABLE SUM (OR DSUM) IS DECLARED TO BE DOUBLE PRECISION IN
C        SUBROUTINE LSQ, SCALE, PDECOM, SLVE, SDPRED AND PINVRT.)  IT
C        IS ESSENTIAL FOR THE SUCCESS OF THE ITERATIVE REFINEMENT
C        PROCEDURE IN SUBROUTINE SLVE THAT INNER PRODUCTS BE ACCUMULATED
C        IN DOUBLE PRECISION.
C
C *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
C *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
C *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
C *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
C *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
C *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
C *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
C *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
C *      ADDITIONAL REMARKS ON HOW TO OMIT THE ITERATIVE REFINEMENT    *
C *      ARE GIVEN IN SUBROUTINE SLVE.                                 *
C *      IF ALL COMPUTING IS DONE IN DOUBLE PRECISION, THE VALUE OF    *
C *      ETA, A MACHINE DEPENDENT PARAMETER, SHOULD BE CHANGED SO THAT *
C *      ETA IS THE SMALLEST DOUBLE PRECISION NUMBER SUCH THAT         *
C *      1.0 + ETA IS GREATER THAN 1.0 IN DOUBLE PRECISION ARITHMETIC. *
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
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
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
      DIMENSION LC(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*), B(*), F(*), P(*), Q(*), R(*), S(*)
      REAL             T(*), V(*), W(*), X(*), Y(*), Z(*)
      REAL             C, D, E, H
      REAL             ETA, RESDF, RMS, RSS, SD, TOL, U, WC, WW, YINC
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION DX(1)
      DOUBLE PRECISION SUM
C
C     ==================================================================
C
      NN  = N
      MM  = M
      WC = H
      U   = RZERO
C
C     SET VALUE OF ETA, A MACHINE-DEPENDENT PARAMETER.
C        ETA IS THE SMALLEST POSITIVE REAL NUMBER FOR WHICH 1.0 + ETA IS
C        GREATER THAN 1.0 IN FLOATING-POINT ARITHMETIC.
C        THE VALUE ETA = 2.**(-26) IS APPROPRIATE FOR THE UNIVAC 1108.
C
      ETA = FDIV (RMXINT,RTWO,IRR) + RONE
      ETA = FDIV (RONE,ETA,IND)
C
C     SET VALUE OF TOL, A TOLERANCE USED IN DETERMINING THE RANK OF THE
C        SYSTEM OF EQUATIONS.
C
C     EMPIRICAL EVIDENCE SUGGESTS THAT TOL SHOULD BE CHOSEN NO SMALLER
C        THAN N*ETA.
C
      TOL = FLOAT (NN) * ETA
C
C     SET SCALE PARAMETER, ISCALE, EQUAL TO ZERO.
C        ISCALE = 0 INDICATES THAT A SOLUTION IS SOUGHT WITHOUT SCALING
C        THE INPUT DATA.
C
C     IN THE EVENT THAT THE ALGORITHM FAILS TO OBTAIN A SOLUTION WITH
C        UNSCALED DATA, ISCALE IS THEN SET EQUAL TO 1 AND ANOTHER
C        ATTEMPT IS MADE TO OBTAIN A SOLUTION WITH THE DATA SCALED.
C
      ISCALE = IZERO
      MP1 = MM + IONE
C
C     SET UP MATRIX Q, INPUT FOR SUBROUTINES SCALE AND PDECOM.
C
  10  IF (IT.EQ.ITWO) GO TO 50
C
C     CALL SUBROUTINE SCALE TO COMPUTE MEAN OF X-VECTOR (DENOTED BY U)
C        FOR POLYNOMIAL TYPE PROBLEMS, IF DATA ARE TO BE SCALED.
C
      IF (ISCALE.EQ.IONE) CALL SCALE (ISCALE,ITWO,NN,MM,IT,LC,NR,W,WC,
     1  X,U,Q,S,B,A,Z,R,F,IFAULT)
C
      DO 40 I=1,NN
        K = MM * NN + I
        Q(K) = Y(I)
        Q(I) = RONE
        IF (MM.EQ.IONE) GO TO 40
        JJ = (LC(1)-IONE) * NR + I
        DO 30 J=2,MM
          K = (J-IONE) * NN + I
          Q(K) = (X(JJ) - U) ** (J-IONE)
  30    CONTINUE
  40  CONTINUE
C
      GO TO 80
C
  50  DO 70 I=1,NN
        K = MM * NN + I
        Q(K) = Y(I)
        DO 60 J=1,MM
          JJ = (LC(J)-IONE) * NR + I
          K = (J-IONE) * NN + I
          Q(K) = X(JJ)
  60    CONTINUE
  70  CONTINUE
C
C     CALL SUBROUTINE SCALE TO COMPUTE VECTOR NORMS AND TO SET VALUES OF
C        SCALE FACTORS (F).
C
  80  CALL SCALE (ISCALE,IONE,NN,MM,IT,LC,NR,W,WC,X,U,Q,S,B,A,Z,R,F,
     1            IFAULT)
C
C     IFAULT IS SET EQUAL TO ONE IN SUBROUTINE SCALE WHEN A COLUMN OF
C        MATRIX X IS FOUND TO EQUAL ZERO.
C
      IF (IFAULT.EQ.IONE) GO TO 240
C
C     CALL SUBROUTINE PDECOM TO OBTAIN AN ORTHOGONAL QR-DECOMPOSITION OF
C        THE MATRIX CONTAINED IN Q ON ENTRY TO PDECOM.  ON RETURN FROM
C        PDECOM, M1 IS THE COMPUTED RANK OF THE SYSTEM OF EQUATIONS.
C        IF MATRIX Q IS FOUND TO BE SINGULAR, IS = 0 ON RETURN FROM
C        PDECOM.  OTHERWISE, IS = 1.
C
      CALL PDECOM (NN,MP1,TOL,W,WC,IS,M1,Q,T,R)
C
      IF (IS.EQ.IZERO) GO TO 100
      IF (M1.GT.IZERO) GO TO 90
      CALL ERROR (22)
      GO TO 240
C
C     ..................................................................
C
  90  IF (M1.EQ.MM) GO TO 100
      IF (ISCALE.EQ.IONE) CALL ERROR (22)
      IF (ISCALE.EQ.IONE) GO TO 240
      ISCALE = IONE
      GO TO 10
 100  IR = ISCALE
C
C     TRANSFER T(J) TO ARRAY R SO THAT T IS AVAILABLE FOR WORK AREA.
C
      DO 110 I=1,MP1
        LD = IDIV (ITWO*(I-IONE)*MP1-I*(I-ITHRE),ITWO,IRR)
        R(LD) = T(I)
 110  CONTINUE
C
C     CALL SUBROUTINE SLVE TO OBTAIN THE SOLUTION (COEFFICIENTS AND
C        RESIDUALS) OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT
C        IS USED TO IMPROVE (IF POSSIBLE) THE ACCURACY OF THE
C        INITIAL SOLUTION.  ON RETURN FROM SLVE, PARAMETER IR = 0 IF THE
C        ITERATIVE REFINEMENT PROCEDURE CONVERGED TO A SOLUTION.
C        OTHERWISE, IR = 1.
C
      CALL SLVE (NN,MM,NR,X,Y,W,WC,IT,LC,ETA,F,U,Q,T,R,IR,B,P,Z,V,S,NI)
C
      D = V(1)
C
      IF (IR.EQ.IZERO) GO TO 130
      IF (ISCALE.EQ.IONE) GO TO 120
      ISCALE = IONE
      GO TO 10
 120  CALL ERROR (33)
      GO TO 240
C
C     COMPUTATIONS NEEDED FOR COMPUTING ACCURATE DIGITS.
C        SUBROUTINE SLVE IS NOW CALLED TO OBTAIN A VECTOR OF
C        COEFFICIENTS (A) BY FITTING PREDICTED VALUES (Y - Z) INSTEAD OF
C        THE ORIGINAL OBSERVATIONS (Y).  A COMPARISON OF VECTOR B WITH
C        VECTOR A IS USED TO ASSESS THE ACCURACY OF VECTOR B.
C        THIS CALL TO SLVE IS OMITTED WHENEVER --
C           L1 = 24  (TWOWAY)
C           L2 =  2  (SPOLYFIT)
C           L2 =  4  (SFIT)
C
 130  IF (L1.EQ.24) GO TO 140
      IF (L2.EQ.ITWO .OR. L2.EQ.IFOUR) GO TO 140
C
      IZ  = ISCALE
      ITT = IT + ITWO
C
      CALL SLVE (NN,MM,NR,X,Y,W,WC,ITT,LC,ETA,F,U,Q,T,R,IZ,A,Z,P,V,S,NJ)
C
C     ..................................................................
C
C     COMPUTE SQUARED FOURIER COEFFICIENTS (S) NEEDED FOR ANALYSIS OF
C        VARIANCE.
C
 140  L = MP1
      DO 150 J=1,MM
        LD = IDIV (ITWO*(J-IONE)*(MM+IONE)-J*J+ITHRE*J,ITWO,IRR)
        S(J) = R(LD) * R(L)**2
        L = L + MP1 - J
 150  CONTINUE
C
C     CALL SUBROUTINE SCALE TO ADJUST RESIDUALS (Z) AND SQUARED
C        FOURIER COEFFICIENTS (S) FOR SCALING, IF DATA WERE SCALED.
C
      IF (ISCALE.EQ.IONE) CALL SCALE (ISCALE,ITHRE,NN,MM,IT,LC,NR,W,WC,
     1  X,U,Q,S,B,A,Z,R,F,IFAULT)
C
C     ADJUST THE FIRST SQUARED FOURIER COEFFICIENT IF Y MID-RANGE WAS
C        SUBTRACTED FROM Y-VECTOR.  IN THIS CASE C IS NONZERO.
C
      YINC = C
      IF (YINC.NE.RZERO) S(1) = R(1) * ( FDIV(R(MP1),F(MP1),IND) +
     1  FDIV(YINC,F(1),IRR) )**2
C
C     COMPUTE RESIDUAL SUM OF SQUARES (E) AND RESIDUAL STANDARD
C        DEVIATION (SD).
C
      CALL DSUMAL (DX,IZERO,SUM)
      WW = WC
      DO 160 I=1,NN
        IF (WC.LE.RZERO) WW = W(I)
        DX(1) = DBLE (Z(I)**2) * DBLE (WW)
        CALL DSUMAL (DX,-IONE,SUM)
 160  CONTINUE
      CALL DSUMAL (DX,IONE,SUM)
      RSS = FDPCON (SUM)
      IF (NN.EQ.MM) GO TO 170
      GO TO 180
C
 170  RMS = RZERO
      GO TO 210
C
 180  NOZWTS = IZERO
      IF (WC.GT.RZERO) GO TO 200
      DO 190 I=1,NN
        IF (W(I).NE.RZERO) GO TO 190
        NOZWTS = NOZWTS + IONE
 190  CONTINUE
 200  RESDF = NN - MM - NOZWTS
      RMS = FDIV (RSS,RESDF,IRR)
 210  SD = FSQRT (RMS)
      E = RSS
C
C     CALL SUBROUTINE SDPRED TO COMPUTE STANDARD DEVIATION OF PREDICTED
C        VALUES (V).
C
      CALL SDPRED (NN,MM,R,Q,T,SD,V)
C
C     CALL SUBROUTINE PINVRT TO OBTAIN THE INVERSE OF (X-TRANSPOSE)*W*X
C        USING RESULTS FROM PDECOM (MATRIX R) AS INPUT.
C
C     MATRIX R IS OVERWRITTEN AND WILL EQUAL THE DESIRED INVERSE UPON
C        RETURN TO SUBROUTINE LSQ.
C
C     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
C        ABOVE THE PRINCIPAL DIAGONAL IS STORED.  COMMENTS AT THE
C        BEGINNING OF SUBROUTINE PINVRT GIVE FURTHER DETAILS.
C
      CALL PINVRT (MM,R,T)
C
C     CALL SUBROUTINE SCALE TO ADJUST COEFFICIENTS (B AND A) AND
C        COVARIANCE MATRIX (R) FOR SCALING, IF DATA WERE SCALED.
C
      IF (ISCALE.EQ.IONE) CALL SCALE (ISCALE,IFOUR,NN,MM,IT,LC,NR,W,WC,
     1  X,U,Q,S,B,A,Z,R,F,IFAULT)
C
C     COMPUTE STANDARD DEVIATIONS OF COEFFICIENTS (T).
C
      DO 230 I=1,MM
        L = IDIV (ITWO*(I-IONE)*MM-I*I+ITHRE*I,ITWO,IRR)
        IF (R(L).GE.RZERO) GO TO 220
        R(L) = RZERO
 220    T(I) = FSQRT (R(L)*RMS)
 230  CONTINUE
C
C     SET VALUE OF ID.
C
 240  ID = NI
      IF (ISCALE.EQ.IZERO) ID = - ID
      RETURN
C
C     ==================================================================
C
      END
*LSRND
      SUBROUTINE LSRND (X,N,ISGD,XT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  LSRND V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT SDRND.
C
C     SUBROUTINE TO ROUND X TO N SIGNIFICANT DIGITS AND STORE IN XT.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE         
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X, XT
      REAL             XTEMP, X1, X2, Y
      REAL             FDPCON, FLOG10
C
      DOUBLE PRECISION Z
      DOUBLE PRECISION DPCA, DRNDA, DRNDB
      DOUBLE PRECISION FDDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA   DPCA / 10.0D0 /
C
C     ==================================================================
C
      IF (X) 20,10,20
  10  XT = RZERO
      RETURN
C
C     ..................................................................
C
  20  IF (N.LT.IONE) N = IONE
      IF (N.GT.ISGD) N = ISGD
      DRNDA = DPCA ** NSIGD
      DRNDB = DPCA *  DRNDA
      Y = ABS (X)
      M = FLOG10 (Y)
      IF (Y.LT.RONE) M = M - IONE
      Z = Y
      Z = Z * DPCA**(ISGD-M)
      IF (Z.LT.DRNDB) GO TO 30
      M = M + IONE
      Z = FDDIV (Z,DPCA,IND)
      GO TO 40
C
  30  IF (Z.GE.DRNDA) GO TO 40
      M   = M - IONE
      Z   = DPCA * Z
  40  X1  = Z
      LL1 = X1
      X2  = Z - DBLE (X1)
      LL2 = X2
      LL  = LL1 + LL2 + IFIVE
      LL1 = IDIV (LL,ITEN**(ISGD+IONE-N),IND)
      LL2 = LL1 * ITEN**(ISGD+IONE-N)
      LL2 = LL - LL2
      IF (N.EQ.ISGD) GO TO 60
      IF (IDIV(LL2,ITEN,IND).GT.IFIVE*ITEN**(ISGD-IONE-N)) GO TO 50
      IF (IDIV(LL2,ITEN,IND).LT.IFIVE*ITEN**(ISGD-IONE-N)) GO TO 60
      LL2 = MOD (LL1,ITWO)
      IF (LL2.LE.IZERO) GO TO 60
  50  LL1 = LL1 + IONE
  60  XTEMP = FLOAT (LL1)
      IF (M.EQ.N-IONE) GO TO 70
C
      Z     = XTEMP
      Z     = Z * DPCA**(M-N+IONE)
      XTEMP = FDPCON (Z)
  70  XT    = SIGN (XTEMP,X)
      RETURN
C
C     ==================================================================
C
      END
