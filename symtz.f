*TCDF
      SUBROUTINE TCDF (X,NU,CDF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82.   TCDF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X.
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
C             FUNCTION VALUE CDF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
C               --FEDERIGHI, EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S
C                 T-DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1959, PAGES 683-688.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 27-30.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 132-134.
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
      REAL             CDF, X
      REAL             ANU, CDFN, SD, Z
      REAL             FDIV, FDPCON, FSQRT
      REAL             SPCA, SPCB
C
      DOUBLE PRECISION TERM(3)
      DOUBLE PRECISION DX, DNU, C, CSQ, S, SUM, AI
      DOUBLE PRECISION DCDF, DCDFN
      DOUBLE PRECISION DPCA, B11
      DOUBLE PRECISION B21, B22, B23, B24, B25
      DOUBLE PRECISION B31, B32, B33, B34, B35, B36, B37
      DOUBLE PRECISION D1, D3, D5, D7, D9, D11
      DOUBLE PRECISION FDDIV, FDEXP, FDSQRT
      DOUBLE PRECISION DATAN
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NUCUT / 1000 /
C
      DATA SPCA / 3000.0 /
      DATA SPCB /  150.0 /
C
      DATA DPCA / 0.3989422804D0 /
C
      DATA B11 /   0.25D0             /
      DATA B21 /   0.01041666666667D0 /
      DATA B22 /   3.0D0              /
      DATA B23 /  -7.0D0              /
      DATA B24 /  -5.0D0              /
      DATA B25 /  -3.0D0              /
      DATA B31 /   0.00260416666667D0 /
      DATA B32 /   1.0D0              /
      DATA B33 / -11.0D0              /
      DATA B34 /  14.0D0              /
      DATA B35 /   6.0D0              /
      DATA B36 /  -3.0D0              /
      DATA B37 / -15.0D0              /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IND = IZERO
      IF (NU.GT.IZERO) GO TO 10
        IND = IFIVE
        CDF = RZERO
        RETURN
C
C     ..................................................................
C
C     ---   START POINT   ----------------------------------------------
C
  10  DX  = X
      ANU = NU
      DNU = NU
C
C     IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF =  0.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 150
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF =  0.0 AND RETURN.
C     IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF =  1.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 150
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF =  1.0 AND RETURN.
C
      IF (NU.LE.ITWO) GO TO 40
      SD = FSQRT (FDIV (ANU,ANU-RTWO,JIND) )
      Z = FDIV (X,SD,JIND)
      IF (NU.LT.ITEN .AND. Z.LT.(-SPCA)) GO TO 20
      IF (NU.GE.ITEN .AND. Z.LT.(-SPCB)) GO TO 20
      IF (NU.LT.ITEN .AND. Z.GT.SPCA) GO TO 30
      IF (NU.GE.ITEN .AND. Z.GT.SPCB) GO TO 30
      GO TO 40
C
  20  CDF = RZERO
      RETURN
C
C     ..................................................................
C
  30  CDF = RONE
      RETURN
C
C     ..................................................................
C
C     DISTINGUISH BETWEEN THE SMALL AND MODERATE
C     DEGREES OF FREEDOM CASE VERSUS THE
C     LARGE DEGREES OF FREEDOM CASE.
C
  40  IF (NU.GE.NUCUT) GO TO 100
C
C     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE
C     METHOD UTILIZED--EXACT FINITE SUM
C     (SEE AMS 55, PAGE 948, FORMULAE 26.7.3 AND 26.7.4).
C
      C  = FDSQRT( FDDIV (DNU,DX*DX+DNU,JIND) )
      CSQ = FDDIV (DNU,DX*DX+DNU,JIND)
      S = FDDIV (DX,FDSQRT(DX*DX+DNU),JIND)
      IMAX = NU - ITWO
      IEVODD = NU - ITWO * IDIV (NU,ITWO,JIND)
      CALL DSUMAL (TERM,IZERO,SUM)
      IF (IEVODD.EQ.IZERO) GO TO 50
C
      SUM = C
      IF (NU.EQ.IONE) SUM = DZERO
      TERM(1) = SUM
      CALL DSUMAL (TERM,-IONE,SUM)
      TERM(1) = C
      IMIN    = ITHRE
      GO TO 60
C
  50  SUM     = DONE
      TERM(1) = DONE
      CALL DSUMAL (TERM,-IONE,SUM)
      IMIN = ITWO
C
  60  IF (IMIN.GT.IMAX) GO TO 80
      DO 70 I=IMIN,IMAX,2
        AI = I
        TERM(1) = TERM(1) * FDDIV (AI-DONE,AI,JIND) * CSQ
        CALL DSUMAL (TERM,-IONE,SUM)
  70  CONTINUE
      CALL DSUMAL (TERM,IONE,SUM)
C
  80  SUM = SUM * S
      IF (IEVODD.EQ.IZERO) GO TO 90
      SUM = FDDIV (DTWO,DPI,JIND) * (DATAN (
     1             FDDIV (DX,FDSQRT(DNU),JIND)) + SUM)
  90  CDF = FDPCON ( DHALF + FDDIV (SUM,DTWO,JIND) )
      RETURN
C
C     ..................................................................
C
C     TREAT THE LARGE DEGREES OF FREEDOM CASE.
C     METHOD UTILIZED--TRUNCATED ASYMPTOTIC EXPANSION
C     (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 102, FORMULA 10;
C     SEE FEDERIGHI, PAGE 687).
C
 100  CALL NORCDF (X,CDFN)
      DCDFN   = CDFN
      D1      = DX
      D3      = DX ** 3
      D5      = DX ** 5
      D7      = DX ** 7
      D9      = DX ** 9
      D11     = DX ** 11
      TERM(1) = B11 * FDDIV (D3+D1,DNU,JIND)
      TERM(2) = B21 * FDDIV (B22*D7+B23*D5+B24*D3+B25*D1,DNU**2,JIND)
      TERM(3) = B31 * FDDIV (B32*D11+B33*D9+B34*D7+B35*D5+B36*D3+B37*D1,
     1                  DNU**3,JIND)
      CALL DSUMAL (TERM,IONE,SUM)
      DCDF = DCDFN - (DPCA * (FDEXP(FDDIV(-DX*DX,DTWO,JIND)))) * DCDF
      CDF = FDPCON (DCDF)
      RETURN
C
C     ==================================================================
C
      END
*TOLLIM
      SUBROUTINE TOLLIM (X,N,T50MN,T50MX,T95MN,T95MX,T99MN,T99MX,CVR,IF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TOLLIM V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES
C              LOWER AND UPPER NORMAL TOLERANCE LIMITS
C              (50 PERCENT COVERAGE, 95 PERCENT CONFIDENCE),
C              (95 PERCENT COVERAGE, 95 PERCENT CONFIDENCE),
C              (99 PERCENT COVERAGE, 95 PERCENT CONFIDENCE),
C              AND ADDITIONALLY THE DISTRIBUTION-FREE
C              COVERAGE BETWEEN THE
C              SAMPLE MINIMUM AND MAXIMUM
C              (95 PERCENT CONFIDENCE).
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                       N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C
C     OUTPUT ARGUMENTS--T50MN = THE SINGLE PRECISION VALUE OF
C                                THE LOWER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                50 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C                     --T50MX = THE SINGLE PRECISION VALUE OF
C                                THE UPPER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                50 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C                     --T95MN = THE SINGLE PRECISION VALUE OF
C                                THE LOWER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                95 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C
C                     --T95MX = THE SINGLE PRECISION VALUE OF
C                                THE UPPER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                95 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C                     --T99MN = THE SINGLE PRECISION VALUE OF
C                                THE LOWER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                99 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C                     --T99MX = THE SINGLE PRECISION VALUE OF
C                                THE UPPER NORMAL TOLERANCE
C                                LIMIT BASED ON
C                                99 PERCENT COVERAGE AND
C                                95 PERCENT CONFIDENCE.
C                     --COVER  = THE SINGLE PRECISION VALUE OF
C                                THE DISTRIBUTION-FREE
C                                COVERAGE BETWEEN THE
C                                SAMPLE MINIMUM AND MAXIMUM AND
C                                95 PERCENT CONFIDENCE.
C
C     OUTPUT--THE COMPUTED VALUES OF THE
C             1) LOWER NORMAL TOLERANCE LIMIT
C                (50 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             2) UPPER NORMAL TOLERANCE LIMIT
C                (50 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             3) LOWER NORMAL TOLERANCE LIMIT
C                (95 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             4) UPPER NORMAL TOLERANCE LIMIT
C                (95 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             5) LOWER NORMAL TOLERANCE LIMIT
C                (99 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             6) UPPER NORMAL TOLERANCE LIMIT
C                (99 PERCENT COVERAGE, 95 PERCENT CONFIDENCE)
C             7) DISTRIBUTION-FREE COVERAGE
C                (BETWEEN MIN AND MAX, 95 PERCENT CONFIDENCE)
C
C     PRINTING--NONE, UNLESS AN INPUT ARGUMENT
C               ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --THE MINIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 2.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     COMMENTS--THIS SUBROUTINE WAS DERIVED FROM THE MORE
C               GENERAL TOLERANCE LIMITS SUBROUTINE TOL
C               FROM THE DATAPAC SUBROUTINE LIBRARY.
C     ALGORITHMS UTILIZED--FOR THE NORMAL TOLERANCE LIMITS,
C                          THE WILKS ALGORITHM WAS UTILIZED
C                          FOR MODERATE AND LARGE N, AND THE
C                          GARDINER AND HULL ALGORITHM WAS
C                          UTILIZED FOR SMALL N.
C                        --FOR THE DISTRIBUTION-FREE COVERAGE,
C                          THE BISECTION ALGORTIHM WAS
C                          UTILIZED FOR THE ROOT (COVERAGE)
C                          DETERMINATION.
C
C     REFERENCES--GARDINER AND HULL, TECHNOMETRICS,
C                 1966, PAGES 115-122.
C               --WILKS, ANNALS OF MATHEMATICAL STATISTICS,
C                 1941, PAGE 92.
C               --MOOD AND GRAYBILL, PAGES 416-417.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2845
C     ORIGINAL VERSION - SEPTEMBER, 1976.
C      CURRENT VERSION -  FEBRUARY, 1990.
C
C     IF = FAULT INDICATOR,
C        = 0, IF EVERYTHING OK,
C        = 1, IF N IS NONPOSITIVE,
C        = 2, IF N EQUALS 1,
C        = 3, IF ALL VALUES OF X ARE THE SAME.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
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
      REAL             X(*)
      REAL             CVR, T50MN, T50MX, T95MN, T95MX, T99MN, T99MX
      REAL             A(3), B(3), C(3), RSMALL(5,3), TEMP(1)
      REAL             TMAX(3), TMIN(3), USMALL(6)
      REAL             AK, AN, AN1, CCALC, CONFID
      REAL             D1, D2, D3, D4, D5, D6, D7
      REAL             F, HOLD, R, SD, U, UNIV, VAR, XBAR, XDEL
      REAL             XLOWER, XMAX, XMID, XMIN, XUPPER, Z
      REAL             FDIV, FSQRT
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF, SPCG, SPCH
      REAL             SPCI, SPCJ, SPCK, SPCL, SPCM, SPCN, SPCO, SPCP
      REAL             SPCQ, SPCR, SPCS, SPCT, SPCU, SPCV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MXLOOP / 30 /
C
      DATA A(1), A(2), A(3) /  0.6745 , 1.9600 , 2.5758  /
      DATA B(1), B(2), B(3) /  0.33734, 0.97910, 1.2889  /
      DATA C(1), C(2), C(3) / -0.15460, 0.40675, 0.85514 /
C
      DATA CONFID / 0.95 /
C
      DATA RSMALL(1,1), RSMALL(1,2), RSMALL(1,3) /
     1          1.0505,      2.6463,      3.3266 /
      DATA RSMALL(2,1), RSMALL(2,2), RSMALL(2,3) /
     1          0.8557,      2.3624,      3.0368 /
      DATA RSMALL(3,1), RSMALL(3,2), RSMALL(3,3) /
     1          0.7929,      2.2457,      2.9128 /
      DATA RSMALL(4,1), RSMALL(4,2), RSMALL(4,3) /
     1          0.7622,      2.1815,      2.8422 /
      DATA RSMALL(5,1), RSMALL(5,2), RSMALL(5,3) /
     1          0.7442,      2.1408,      2.7963 /
C
      DATA USMALL(1) /  0.0    /
      DATA USMALL(2) / 15.9472 /
      DATA USMALL(3) /  4.4154 /
      DATA USMALL(4) /  2.9200 /
      DATA USMALL(5) /  2.3724 /
      DATA USMALL(6) /  2.0893 /
C
      DATA Z     / -1.644854 /
C
      DATA SPCA /       7.0 /
      DATA SPCB /       9.0 /
      DATA SPCC /       1.5 /
      DATA SPCD /       6.0 /
      DATA SPCE /      14.0 /
      DATA SPCF /      32.0 /
      DATA SPCG /     405.0 /
      DATA SPCH /     256.0 /
      DATA SPCI /     433.0 /
      DATA SPCJ /    4860.0 /
      DATA SPCK /       2.5 /
      DATA SPCL /      12.0 /
      DATA SPCM /     243.0 /
      DATA SPCN /     923.0 /
      DATA SPCO /    1472.0 /
      DATA SPCP /   25515.0 /
      DATA SPCQ /    3753.0 /
      DATA SPCR /    4353.0 /
      DATA SPCS /  289517.0 /
      DATA SPCT /  289717.0 /
      DATA SPCU / 9185400.0 /
      DATA SPCV /       3.5 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C
      IF = IZERO
      IF (N.GE.IONE) GO TO 10
      IF = IONE
      RETURN
C
C     ..................................................................
C
  10  IF (N.GT.IONE) GO TO 20
      IF = ITWO
      RETURN
C
C     ..................................................................
C
  20  HOLD = X(1)
      DO 30 I=2,N
        IF (X(I).NE.HOLD) GO TO 40
  30  CONTINUE
      IF = ITHRE
      RETURN
C
C     ==================================================================
C
  40  AN  = N
      AN1 = N - IONE
      F   = N - IONE
C
C     COMPUTE THE SAMPLE MEAN.
C
      CALL SUMMAL (X,N,XBAR)
      IF (N.EQ.IONE) XBAR = X(1)
      XBAR = FDIV (XBAR,AN,IND)
C
C     COMPUTE THE SAMPLE STANDARD DEVIATION.
C
      CALL SUMMAL (X,IZERO,VAR)
      DO 50 I=1,N
        TEMP(1) = (X(I)-XBAR)**2
        CALL SUMMAL (TEMP,-IONE,VAR)
  50  CONTINUE
      CALL SUMMAL (TEMP,IONE,VAR)
      VAR = FDIV (VAR,AN-RONE,IND)
      SD  = FSQRT (VAR)
C
C     COMPUTE THE NORMAL TOLERANCE LIMITS FOR 95 PERCENT CONFIDENCE.
C        LOOP THROUGH THE 3 COVERAGES ...
C     J = 1 IS 50 PERCENT, J=2 IS 95 PERCENT, J=3 IS 99 PERCENT.
C
      IF (N.LE.6) U = USMALL(N)
      IF (N.LE.6) GO TO 60
C
      D1 = RONE + Z * FDIV (FSQRT(RTWO),FSQRT(F),IND)
      D2 = RTWO * FDIV (Z**2-RONE,RTHRE*F,IND)
      D3 = FDIV (Z**3-SPCA*Z,SPCB*FSQRT(RTWO)*F**SPCC,IND)
      D4 = FDIV (SPCD*Z**4+SPCE*Z**2-SPCF,SPCG*F**2.0,IND)
      D5 = FDIV(SPCB*Z**5+SPCH*Z**3-SPCI*Z,SPCJ*FSQRT(RTWO)*F**SPCK,IND)
      D6 = FDIV (SPCL*Z**6-SPCM*Z**4-SPCN*Z**2+SPCO,SPCP*F**3.0,IND)
      D7 = FDIV (SPCQ*Z**7+SPCR*Z**5-SPCS*Z**3-SPCT*Z,SPCU
     1     *FSQRT(RTWO)*F**SPCV,IND)
C
      UNIV = D1 + D2 + D3 - D4 + D5 + D6 - D7
      U = FDIV (RONE,UNIV,IND)
      U = FSQRT (U)
C
  60  DO 70 J=1,3
        R = A(J) + FDIV (B(J),C(J)+AN,IND)
        IF (N.LE.IFIVE) R = RSMALL(N,J)
        AK = R * U
        TMIN(J) = XBAR - AK*SD
        TMAX(J) = XBAR + AK*SD
  70  CONTINUE
C
      T50MN = TMIN(1)
      T50MX = TMAX(1)
      T95MN = TMIN(2)
      T95MX = TMAX(2)
      T99MN = TMIN(3)
      T99MX = TMAX(3)
C
C     COMPUTE DISTRIBUTION-FREE TOLERANCE LIMITS.
C        FIRST DEFINE BOUNDS FOR THE COVERAGE, THEN ITERATE (BY THE
C        BISECTION METHOD) TO SOLVE FOR THE DESIRED COVERAGE.
C
      XMIN   = FDIV (RONE-CONFID,AN,IND) ** FDIV (RONE,AN1,IND)
      XMAX   = (RONE-CONFID) ** FDIV (RONE,AN,IND)
      XMID   = FDIV (XMIN+XMAX,RTWO,IND)
      XLOWER = XMIN
      XUPPER = XMAX
      ILOOP  = IZERO
C
  80  CCALC = RONE - AN * XMID**(N-1) + AN1 * XMID**N
      IF (CCALC.EQ.CONFID) GO TO 110
      IF (CCALC.LT.CONFID) GO TO 90
      XLOWER = XMID
      XMID   = FDIV (XMID+XUPPER,RTWO,IND)
      GO TO 100
C
  90  XUPPER = XMID
      XMID   = FDIV (XMID+XLOWER,RTWO,IND)
C
 100  XDEL   = ABS (XMID-XLOWER)
      ILOOP  = ILOOP + IONE
      IF (XDEL.GE.RER .AND. ILOOP.LE.MXLOOP) GO TO 80
C
 110  CVR = XMID
      RETURN
C
C     ==================================================================
C
      END
*TPCTPT
      SUBROUTINE TPCTPT (V,T)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TPCTPT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE, T, THE 5 PRECENT POINT OF STUDENT'S T-DISTRIBUTION WITH V
C        DEGREES OF FREEDOM.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             V, T
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE
      REAL             SPCF, SPCG, SPCH, SPCI, SPCJ
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 3.6948     /
      DATA SPCB / 1.6561     /
      DATA SPCC / 0.406      /
      DATA SPCD / 2.7764     /
      DATA SPCE / 1.959964   /
      DATA SPCF / 2.3722712  /
      DATA SPCG / 2.8224986  /
      DATA SPCH / 2.5558497  /
      DATA SPCI / 1.5895341  /
      DATA SPCJ / 0.73289821 /
C
C     ==================================================================
C
      IF (V.LE.RZERO) GO TO 30
      IF (V-AINT(V)) 30,10,30
  10  IF (V.GT.RFOR) GO TO 20
      T = SPCA*AINT(FDIV(RONE,V,IND)) - SPCB*AINT(FDIV(RTWO,V,IND)) +
     1    SPCC*AINT(FDIV(RTHRE,V,IND)) + SPCD*AINT(FDIV(RFOR,V,IND))
      RETURN
C
C     ..................................................................
C
  20  T = SPCE + FDIV (SPCF,V,IND) + FDIV (SPCG,V**2,IND) +
     1   FDIV(SPCH,V**3,IND) + FDIV(SPCI,V**4,IND) + FDIV(SPCJ,V**5,IND)
      RETURN
C
C     ..................................................................
C
  30  CALL ERROR (207)
      RETURN
C
C     ==================================================================
C
      END
*TPPF
      SUBROUTINE TPPF (P,NU,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82.   TPPF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THE STUDENT'S T DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL X,
C              AND ITS PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (EXCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
C             VALUE PPF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSIN, DCOS, DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C
C     COMMENT--FOR NU = 1 AND NU = 2, THE PERCENT POINT FUNCTION
C              FOR THE T DISTRIBUTION EXISTS IN SIMPLE CLOSED FORM
C              AND SO THE COMPUTED PERCENT POINTS ARE EXACT.
C            --FOR OTHER SMALL VALUES OF NU (NU BETWEEN 3 AND 6,
C              INCLUSIVELY), THE APPROXIMATION
C              OF THE T PERCENT POINT BY THE FORMULA
C              GIVEN IN THE REFERENCE BELOW IS AUGMENTED
C              BY 3 ITERATIONS OF NEWTON'S METHOD FOR
C              ROOT DETERMINATION.
C              THIS IMPROVES THE ACCURACY--ESPECIALLY FOR
C              VALUES OF P NEAR 0 OR 1.
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 949, FORMULA 26.7.5.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGE 102,
C                 FORMULA 11.
C               --FEDERIGHI, 'EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S T
C                 DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1969, PAGES 683-688.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 120-123.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--OCTOBER   1975.
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
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             P, PPF
      REAL             PPFN
      REAL             FDPCON
C
      DOUBLE PRECISION DNU, DP, DPPFN, DPPF, DCON, DARG, Z, S, C
      DOUBLE PRECISION TERM(5)
      DOUBLE PRECISION TERM1, TERM2, TERM3
      DOUBLE PRECISION SQRT2, B21
      DOUBLE PRECISION B31,B32,B33,B34
      DOUBLE PRECISION B41,B42,B43,B44,B45
      DOUBLE PRECISION B51,B52,B53,B54,B55,B56
      DOUBLE PRECISION D1,D3,D5,D7,D9
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION FDCOS, FDDIV, FDSIN, FDSQRT
      DOUBLE PRECISION DATAN
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA /  1.50D0  /
      DATA DPCB /  3.0D0   /
      DATA DPCC /  8.0D0   /
      DATA DPCD /  0.375D0 /
      DATA DPCE / 15.0D0   /
C
      DATA SQRT2 / 1.414213562D0 /
C
      DATA B21 /     0.25D0             /
      DATA B31 /     0.01041666666667D0 /
      DATA B32 /     5.0D0              /
      DATA B33 /    16.0D0              /
      DATA B34 /     3.0D0              /
      DATA B41 /     0.00260416666667D0 /
      DATA B42 /     3.0D0              /
      DATA B43 /    19.0D0              /
      DATA B44 /    17.0D0              /
      DATA B45 /   -15.0D0              /
      DATA B51 /     0.00001085069444D0 /
      DATA B52 /    79.0D0              /
      DATA B53 /   776.0D0              /
      DATA B54 /  1482.0D0              /
      DATA B55 / -1920.0D0              /
      DATA B56 /  -945.0D0              /
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
C     ---   START POINT   ----------------------------------------------
C
  10  DNU   = NU
      DP    = P
      MAXIT = IFIVE
C
      IF (NU.EQ.ITWO)  GO TO 20
      IF (NU.GE.ITHRE) GO TO 30
C
C     TREAT THE NU  =  1 (CAUCHY) CASE.
C
      DARG = DPI * DP
      PPF  = - FDPCON ( FDDIV (FDCOS(DARG),FDSIN(DARG),JIND) )
      RETURN
C
C     ..................................................................
C
C     TREAT THE NU  =  2 CASE.
C
  20  TERM1 = FDDIV (SQRT2,DTWO,JIND)
      TERM2 = DTWO * DP - DONE
      TERM3 = FDSQRT (DP*(DONE-DP))
      PPF   = FDPCON ( FDDIV (TERM1*TERM2,TERM3,JIND) )
      RETURN
C
C     ..................................................................
C
C     TREAT THE NU GREATER THAN OR EQUAL TO 3 CASE.
C
  30  CALL NORPPF (P,PPFN,NIND)
      DPPFN   = PPFN
      D1      = DPPFN
      D3      = DPPFN ** 3
      D5      = DPPFN ** 5
      D7      = DPPFN ** 7
      D9      = DPPFN ** 9
      TERM(1) = D1
      TERM(2) = FDDIV (B21*(D3+D1),DNU,JIND)
      TERM(3) = B31 * FDDIV (B32*D5+B33*D3+B34*D1,DNU**2,JIND)
      TERM(4) = B41 * FDDIV (B42*D7+B43*D5+B44*D3+B45*D1,DNU**3,JIND)
      TERM(5) = B51 * FDDIV (B52*D9 + B53*D7 + B54*D5 + B55*D3 + B56*D1,
     1                                                 DNU**4,JIND)
      CALL DSUMAL (TERM,IFIVE,DPPF)
      PPF   = FDPCON (DPPF)
      IF (NU.GE.7)     RETURN
      IF (NU.EQ.ITHRE) GO TO 40
      IF (NU.EQ.IFOUR) GO TO 60
      IF (NU.EQ.IFIVE) GO TO 80
      IF (NU.EQ.6   )  GO TO 100
      RETURN
C
C     ..................................................................
C
C     AUGMENT THE RESULTS FOR THE NU  =  3 CASE.
C
  40  DCON = DPI * (DP-DHALF)
      DARG = FDDIV (DPPF,FDSQRT(DNU),JIND)
      Z    = DATAN (DARG)
      DO 50 IPASS=1,MAXIT
        S = FDSIN (Z)
        C = FDCOS(Z)
        Z = Z - FDDIV (Z+S*C-DCON,DTWO*C*C,JIND)
  50  CONTINUE
      PPF =  FDPCON ( FDDIV (FDSQRT(DNU)*S,C,JIND) )
      RETURN
C
C     ..................................................................
C
C     AUGMENT THE RESULTS FOR THE NU  =  4 CASE.
C
  60  DCON = DTWO * (DP-DHALF)
      DARG = FDDIV (DPPF,FDSQRT(DNU),JIND)
      Z    = DATAN (DARG)
      DO 70 IPASS=1,MAXIT
        S = FDSIN (Z)
        C = FDCOS (Z)
        Z = Z - FDDIV ((DONE+DHALF*C*C)*S-DCON,DPCA*C*C*C,JIND)
  70  CONTINUE
      PPF =  FDPCON ( FDDIV (FDSQRT(DNU)*S,C,JIND) )
      RETURN
C
C     ..................................................................
C
C     AUGMENT THE RESULTS FOR THE NU  =  5 CASE.
C
  80  DCON = DPI * (DP-DHALF)
      DARG = FDDIV (DPPF,FDSQRT(DNU),JIND)
      Z    = DATAN(DARG)
      DO 90 IPASS=1,MAXIT
        S = FDSIN(Z)
        C = FDCOS(Z)
        Z = Z - FDDIV (Z+(C+FDDIV(DTWO,DPCB,JIND)*C*C*C)*S-DCON,
     1          FDDIV(DPCC,DPCB,JIND)*C**4,JIND)
  90  CONTINUE
      PPF = FDPCON ( FDSQRT(DNU) * FDDIV (S,C,JIND) )
      RETURN
C
C     ..................................................................
C
C     AUGMENT THE RESULTS FOR THE NU  =  6 CASE.
C
 100  DCON = DTWO * (DP-DHALF)
      DARG = FDDIV (DPPF,FDSQRT(DNU),JIND)
      Z    = DATAN (DARG)
      DO 110 IPASS=1,MAXIT
        S = FDSIN (Z)
        C = FDCOS (Z)
        Z = Z - FDDIV ((DONE+DHALF*C*C+DPCD*C**4)*S-DCON,
     1               FDDIV(DPCE,DPCC,JIND)*C**5,JIND)
 110  CONTINUE
      PPF = FDPCON ( FDSQRT(DNU) * FDDIV (S,C,JIND) )
      RETURN
C
C     ==================================================================
C
      END
*TQL2
      SUBROUTINE TQL2 (NM,N,D,E,Z,IERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   TQL2 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE IS  USED TO OBTAIN EIGENVECTORS AND EIGENVALUES
C     AND IS PART OF THE EISPACK PACKAGE CREATED AT ARGONNE NATIONAL LAB
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND
C     WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY,
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1,
C
C        E HAS BEEN DESTROYED,
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C               ADAPTED TO OMNITAB BY -
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             D(N), E(N), Z(NM,N)
      REAL             B, C, F, G, H, MACHEP, P, R, S
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
      MACHEP = RTWO**(-NSBB+1)
C
      IERR = IZERO
      IF (N .EQ. IONE) RETURN
C
      DO 10 I=2,N
        E(I-1) = E(I)
  10  CONTINUE
C
      F = RZERO
      B = RZERO
      E(N) = RZERO
C
      DO 110 L=1,N
        J = IZERO
        H = MACHEP * (ABS(D(L)) + ABS(E(L)))
        IF (B.LT.H) B = H
C
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
C
        DO 20 M=L,N
          IF (ABS(E(M)).LE.B) GO TO 30
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
  20    CONTINUE
C
  30    IF (M.EQ.L) GO TO 100
  40    IF (J .EQ. 30) GO TO 150
        J = J + IONE
C
C     ********** FORM SHIFT **********
C
        P = FDIV (D(L+1) - D(L),RTWO * E(L),IND)
        R = FSQRT (P*P+RONE)
        H = D(L) - FDIV (E(L),P + SIGN (R,P),IND)
C
        DO 50 I=L,N
          D(I) = D(I) - H
  50    CONTINUE
C
        F = F + H
C
C     ********** QL TRANSFORMATION **********
C
        P = D(M)
        C = RONE
        S = RZERO
        MML = M - L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
        DO 90 II=1,MML
          I = M - II
          G = C * E(I)
          H = C * P
          IF (ABS(P).LT.ABS(E(I))) GO TO 60
          C = FDIV (E(I),P,IND)
          R = FSQRT (C*C+RONE)
          E(I+1) = S * P * R
          S = FDIV (C,R,IND)
          C = FDIV (RONE,R,IND)
          GO TO 70
  60      C = FDIV (P,E(I),IND)
          R = FSQRT (C*C+RONE)
          E(I+1) = S * E(I) * R
          S = FDIV (RONE,R,IND)
          C = C * S
  70      P = C * D(I) - S * G
          D(I+1) = H + S * (C * G + S * D(I))
C
C     ********** FORM VECTOR **********
C
          DO 80 K=1,N
            H = Z(K,I+1)
            Z(K,I+1) = S * Z(K,I) + C * H
            Z(K,I) = C * Z(K,I) - S * H
  80      CONTINUE
C
  90    CONTINUE
C
        E(L) = S * P
        D(L) = C * P
        IF (ABS(E(L)) .GT. B) GO TO 40
 100    D(L) = D(L) + F
 110  CONTINUE
C
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
C
      DO 140 II=2,N
        I = II - IONE
        K = I
        P = D(I)
C
        DO 120 J=II,N
          IF (D(J).GE.P) GO TO 120
          K = J
          P = D(J)
 120    CONTINUE
C
        IF (K.EQ.I) GO TO 140
        D(K) = D(I)
        D(I) = P
C
        DO 130 J=1,N
          P = Z(J,I)
          Z(J,I) = Z(J,K)
          Z(J,K) = P
 130    CONTINUE
C
 140  CONTINUE
C
      RETURN
C
C     ..................................................................
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
 150  IERR = L
      RETURN
C
C     ==================================================================
C
      END
*TRED2
      SUBROUTINE TRED2 (NM,N,A,D,E,Z)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  TRED2 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE IS USED TO OBTAIN EIGENVECTORS AND EIGENVALUES AND
C        IS PART OF THE EISPACK PACKAGE CREATED AT ARGONNE NATIONAL LAB.
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A SYMMETRIC
C        TRIDIAGONAL MATRIX USING AND ACCUMULATING ORTHOGONAL
C        SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT ...
C
C        NM     MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C               ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C               DIMENSION STATEMENT,
C
C        N     IS THE ORDER OF THE MATRIX,
C
C        A     CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C              LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT ...
C
C        D     CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX,
C
C        E     CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C              MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO,
C
C        Z     CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX
C              PRODUCED IN THE REDUCTION,
C
C        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C        APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY.
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      SALLY T. PEAVY,
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(NM,N), D(N), E(N), Z(NM,N)
      REAL             F, G, H, HH, SCALE
      REAL             FDIV, FSQRT
C
C     ==================================================================
C
      DO 20 I=1,N
        DO 10 J=1,I
          Z(I,J) = A(I,J)
  10    CONTINUE
  20  CONTINUE
C
      IF (N.EQ.IONE) GO TO 160
C
C     ********** FOR I=N STEP -1 UNTIL 2 DO -- **********
C
      DO 150 II=2,N
        I = N + ITWO - II
        L = I - IONE
        H = RZERO
        SCALE = RZERO
        IF (L.LT.ITWO) GO TO 40
C
C       ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********
C
        DO 30 K=1,L
          SCALE = SCALE + ABS (Z(I,K))
  30    CONTINUE
C
        IF (SCALE.NE.RZERO) GO TO 50
  40    E(I) = Z(I,L)
        GO TO 140
C
  50    DO 60 K=1,L
          Z(I,K) = FDIV (Z(I,K),SCALE,IND)
          H = H + Z(I,K) * Z(I,K)
  60    CONTINUE
C
        F      = Z(I,L)
        G      = - SIGN (FSQRT(H),F)
        E(I)   = SCALE * G
        H      = H - F * G
        Z(I,L) = F - G
        F      = RZERO
C
        DO 100 J=1,L
          Z(J,I) = FDIV (Z(I,J),SCALE*H,IND)
          G = RZERO
C
C         ********** FORM ELEMENT OF A*U **********
C
          DO 70 K=1,J
            G = G + Z(J,K) * Z(I,K)
  70      CONTINUE
C
          JP1 = J + IONE
          IF (L.LT.JP1) GO TO 90
C
          DO 80 K=JP1,L
            G = G + Z(K,J) * Z(I,K)
  80      CONTINUE
C
C         ********** FORM ELEMENT OF P **********
C
  90      E(J) = FDIV (G,H,IND)
          F = F + E(J) * Z(I,J)
 100    CONTINUE
C
        HH = FDIV (F,H+H,IND)
C
C       ********** FORM REDUCED A **********
C
        DO 120 J=1,L
          F = Z(I,J)
          G = E(J) - HH * F
          E(J) = G
C
          DO 110 K=1,J
            Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
 110      CONTINUE
 120    CONTINUE
C
        DO 130 K=1,L
          Z(I,K) = SCALE * Z(I,K)
 130    CONTINUE
C
 140    D(I) = H
 150  CONTINUE
C
 160  D(1) = RZERO
      E(1) = RZERO
C
C     ********** ACCUMULATION OF TRANSFORMATION MATRICES **********
C
      DO 220 I=1,N
        L = I - IONE
        IF (D(I).EQ.RZERO) GO TO 200
C
        DO 190 J=1,L
          G = RZERO
C
          DO 170 K=1,L
            G = G + Z(I,K) * Z(K,J)
 170      CONTINUE
C
          DO 180 K=1,L
            Z(K,J) = Z(K,J) - G * Z(K,I)
 180      CONTINUE
 190    CONTINUE
C
 200    D(I) = Z(I,I)
        Z(I,I) = RONE
        IF (L.LT.IONE) GO TO 220
C
        DO 210 J=1,L
          Z(I,J) = RZERO
          Z(J,I) = RZERO
 210    CONTINUE
C
 220  CONTINUE
C
      RETURN
C     ==================================================================
C
      END
*TRIMAT
      SUBROUTINE TRIMAT (A,N,INDU,INDB)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TRIMAT V 7.00  5/21/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     TO DETERMINE IF A IS AN UPPER OR LOWER TRIANGULAR MATRIX
C
C     INPUT ...
C
C     A,    MATRIX TO BE CHECKED
C     NROW, DIMENSION SIZE OF A
C     N,    PRESENT SIZE OF A
C
C     OUTPUT
C
C     INDU INDICATOR ...
C          INDU=0, UPPER  TRIANGLE=0,  INDU=1, UPPER  TRIANGLE NOT ZERO
C          INDB=0, BOTTOM TRIANGLE=0,  INDB=1, BOTTOM TRIANGLE NOT ZERO
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1968.
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(*)
C
C     ==================================================================
C
      INDU = IONE
      INDB = IONE
      NN = N - IONE
      DO 60 I=1,NN
        II = I + IONE
        IP = (I -IONE) * NROW
        DO 50 J=II,N
          IJ = I + (J - IONE) * NROW
          JI = J + IP
          GO TO (10,20), INDU
  10      IF (A(IJ).NE.RZERO) INDU = ITWO
  20      GO TO (30,40), INDB
  30      IF (A(JI).NE.RZERO) INDB = ITWO
  40      IF (INDU.EQ.ITWO .AND. INDB.EQ.ITWO) GO TO 70
  50    CONTINUE
  60  CONTINUE
C
  70  INDU = INDU - IONE
      INDB = INDB - IONE
      RETURN
C
C     ==================================================================
C
      END
*TWRANK
      SUBROUTINE TWRANK (N,X,H,R,T)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. TWRANK V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT RANKX.
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
*UNIMED
      SUBROUTINE UNIMED (N,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UNIMED V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES THE N ORDER STATISTIC MEDIANS
C              FROM THE UNIFORM (RECTANGULAR)
C              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
C              THIS DISTRIBUTION HAS MEAN = 0.5
C              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION F(X) = 1.
C              THIS SUBROUTINE IS A SUPPORT SUBROUTINE FOR
C              ALL OF THE PROBABILITY PLOT SUBROUTINES
C              IN DATAPAC; IT IS RARELY USED BY THE
C              DATA ANALYST DIRECTLY.
C              A PROBABILITY PLOT FOR A GENERAL DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE I-TH ORDER STATISTIC MEDIAN FOR A GENERAL
C              DISTRIBUTION IS OBTAINED BY TRANSFORMING
C              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
C              BY THE PERCENT POINT FUNCTION OF THE DESIRED
C              DISTRIBUTION--HENCE THE IMPORTANCE OF BEING ABLE TO
C              GENERATE UNIFORM ORDER STATISTIC MEDIANS.
C              IT IS OF THEROETICAL INTEREST TO NOTE THAT
C              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
C              IN A SAMPLE OF SIZE N IS IDENTICALLY THE
C              MEDIAN OF THE BETA DISTRIBUTION
C              WITH PARAMETERS I AND N-I+1.
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF UNIFORM ORDER STATISTIC MEDIANS
C                                TO BE GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                UNIFORM ORDER STATISTIC MEDIANS
C                                WILL BE PLACED.
C     OUTPUT--THE N ORDER STATISTIC MEDIANS
C             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
C     PRINTING--NONE
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     REFERENCES--FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIEN
C                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-11
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--JUNE      1972.
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1975.
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
      REAL             AI, AN, GAM
      REAL             FDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA GAM / 0.3175 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (N.GT.IZERO) GO TO 10
        CALL ERROR (9)
        RETURN
C
C     ..................................................................
C
  10  AN     = N
      X(N)   = RHALF ** FDIV (RONE,AN,IND)
      X(1)   = RONE - X(N)
      NHALF  = IDIV (N,ITWO,IND) + IONE
      NEVODD = ITWO * IDIV (N,ITWO,IND)
      IF (N.NE.NEVODD) X(NHALF) = RHALF
      IF (N.LE.ITHRE) RETURN
      IMAX = IDIV (N,ITWO,IND)
      IF (IMAX.LE.IONE) RETURN
      DO 20 I=2,IMAX
        AI   = I
        IREV = N - I + IONE
        X(I) = FDIV (AI-GAM,AN-RTWO*GAM+RONE,IND)
        X(IREV) = RONE - X(I)
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*UNIQUE
      SUBROUTINE UNIQUE (D,L,X,Y,M)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UNIQUE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE (1) MOVES DATA IN D OF LENGTH L AND PUTS RESULT IN X.
C                (2) SORTS X OF LENGTH L AND PUTS RESULT IN X,
C                       AND PUTS HIERARCHY IN Y.
C                (3) REPLACES Y BY THE M DISTINCT VALUES OF X,
C                       IN INCREASING ORDER.
C
C     INPUT  - D, L
C     OUTPUT - X, Y, M
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     JULY, 1976.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             D(*), X(*), Y(*)
C
C     ==================================================================
C
C     (1)
C
      DO 10 I=1,L
        X(I) = D(I)
  10  CONTINUE
C
C     (2)
C
      CALL SORT (X,Y,L,IZERO)
C
C     (3)
C
      Y(1) = X(1)
      M = IONE
      DO 30 I=2,L
        IF (X(I)-X(I-1)) 20,30,20
  20      M = M+IONE
          Y(M) = X(I)
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*UNIRAN
      REAL             FUNCTION UNIRAN (IRAN,KRAN,ISTART)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UNIRAN V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     A PORTABLE FORTRAN RANDOM NUMBER GENERATOR FOR COMPUTERS
C        WITH WORD SIZE 16 BITS OR MORE.
C
C     FIRST  CALL              IS X = UNIRAN ( -1), TO INITIALIZE I,K,N.
C
C     SECOND CALL (IF DESIRED) IS X = UNIRAN (236), WHERE 236 IS CHOSEN
C                                                       BY THE USER.
C            SUBSEQUENT CALLS ARE X = UNIRAN (0).
C
C     ALGORITHM MIXES TWO FIBONACCI SEQUENCES.
C
C               WRITTEN BY -
C                      GEORGE MARSAGLIA, AT NBS.
C
C               ADAPTED FOR OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING LABORATORY,
C                      APPLIED MATHEMATICS DIVISION,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-921-2315
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION N(40)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             FTWO14
      REAL             FDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     FTWO14 = 16384.0 = 2.0**14
C     ITWO14 = 16384   =   2**14
C
      DATA FTWO14 / 16384.0 /
      DATA ITWO14 / 16384   /
C
C     L = FIBONACCI LAG
C            L MUST BE GREATER THAN TWO AND LESS THAN OR EQUAL TO 40.
C
      DATA L / 20 /
C
C     ==================================================================
C
      IF (ISTART.GE.IZERO) GO TO 20
C
C     SET INITIAL VALUES OF IRAN, KRAN, AND N(.).
C
      IRAN = IONE
      KRAN = IONE
C
C     ARRAY OF INTEGERS, NOT ALL EVEN.
C
      N( 1) =  521
      N( 2) =   28
      N( 3) = 8629
      N( 4) = 3626
      N( 5) =   36
      N( 6) = 3734
      N( 7) = 6529
      N( 8) = 1976
      N( 9) =   48
      N(10) = 9143
C
      DO 10 J=1,10
        N(J+10) = N(J)
        N(J+20) = N(J)
        N(J+30) = N(J)
  10  CONTINUE
C
      UNIRAN = IZERO
      RETURN
C
C     ==================================================================
C
  20  IF (ISTART.EQ.IZERO) GO TO 40
C
      KRAN = IONE
      N(1) = ISTART
      N(2) = ITWO*ISTART + IONE
      DO 30 J=3,L
        N(J) = N(J-1) + N(J-2)
        IF (N(J).GE.ITWO14) N(J) = N(J) - ITWO14
  30   CONTINUE
      IRAN = N(L-1) + N(L)
      IF (IRAN.GE.ITWO14) IRAN = IRAN - ITWO14
C
C     ..................................................................
C
  40  IRAN = IRAN + N(KRAN)
      IF (IRAN.GE.ITWO14) IRAN = IRAN - ITWO14
      N(KRAN) = IRAN
      KRAN = KRAN + IONE
      IF (KRAN.GT.L) KRAN = IONE
      UNIRAN = FDIV (FLOAT(IRAN),FTWO14,IND)
      RETURN
C
C     ==================================================================
C
      END
*UNITX
      SUBROUTINE  UNITX
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  UNITX V 7.00  6/ 4/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     READ UNIT 'L'  (C), ..., (C)        L1=45       L2=1,8,15,22,29,36
C     READ UNIT 'L' 'L' FORMAT (C) ETC    L1=45       L2=2-7,9-14,16-21,
C                                                        23-28,30-35,
C                                                        37-42
C
C     SET  UNIT 'L'  (C) ETC              L1=48       L2=1,8,15,22,29,36
C
C     CREAD UNIT 'L'                      L1=46       L2=1,8,15,22,29,36
C     CREAD UNIT 'L'  'L' FORMAT          L1=46       L2=2-7,9-14,16-21,
C                                                        23-28,30-35,
C                                                        37-42
C
C     CSET  UNIT 'L'                      L1=49       L2=1,8,15,22,29,36
C
C     WRITE UNIT 'L'  (C),..., (C)        L1=47       L2=1,8,15,22,29,36
C     WRITE UNIT 'L'  'L' FORMAT          L1=47       L2=2-7,9-14,16-21,
C                                                        23-28,30-35,
C                                                        37-42
C     BACKSPACE  UNIT 'L'                 L1=50       L2=23-28
C
C     ENDFILE UNIT  'L'                   L1=50       L2=2-7
C
C     REWIND UNIT  'L'                    L1=50       L2=9-14
C
C     SKIP UNIT    'L'                    L1=50       L2=16-21
C
C     UNIT                                L1=50       L2=29
C           THIS  BLOCK SIZE WILL APPLY TO ALL UNITS.
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
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
C
      DIMENSION NTAPEN(6)
C
      CHARACTER*9 NTAPEN
C
      LOGICAL STAT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NTAPEN(1) / 'UNITA.DAT' /
      DATA NTAPEN(2) / 'UNITB.DAT' /
      DATA NTAPEN(3) / 'UNITC.DAT' /
      DATA NTAPEN(4) / 'UNITD.DAT' /
      DATA NTAPEN(5) / 'UNITE.DAT' /
      DATA NTAPEN(6) / 'UNITF.DAT' /
C
C     ==================================================================
C
      LL    = MOD (L2,7)
      LLA   = IDIV (L2,7,IND)
      IF (LL.EQ.IZERO) LLA = LLA - IONE
      IF (LL.EQ.IZERO) LL = 7
      NTAPE = LTAPE + LLA
      INQUIRE (UNIT = NTAPE, OPENED = STAT)
      IF(.NOT.STAT) OPEN (UNIT = NTAPE,FILE = NTAPEN(LLA +IONE))
      LONE  = L1 - 44
C
      GO TO (10,10,20,30,30,40), LONE
C
C     ..................................................................
C
C     INSTRUCTION IS EITHER READ UNIT OR CREAD UNIT.
C
  10  CALL UNXRDC (LL,LONE,NTAPE)
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS WRITE UNIT.
C
  20  CALL UNXWRT (LL,NTAPE)
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS EITHER SET UNIT OR CSET UNIT.
C
  30  CALL UNXSET (LONE,NTAPE)
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS EITHER BACKSPACE, ENDFILE, REWIND, SKIP OR UNIT.
C
  40  CALL UNXSPC (LL,LLA)
      RETURN
C
C     ==================================================================
C
      END
*UNXSPC
      SUBROUTINE  UNXSPC (LL,LLA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. UNXSPC V 7.00  6/ 9/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS BACKSPACE, ENDFILE, REWIND, SKIP AND UNIT.
C
C     INPUT ...
C
C        LL AND LLA.
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
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR
C
C     ==================================================================
C
      IF (LL.EQ.IONE .AND. L2.NE.29) CALL ERROR (27)
      NTAPE = LTAPE + LL - ITWO
      LLA = LLA + IONE
C
      GO TO (10,20,30,30,70), LLA
C
C
C     ..................................................................
C
C     INSTRUCTION IS END FILE UNIT.
C
  10  IF (NERROR.EQ.IZERO) END FILE NTAPE
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS REWIND UNIT.
C
  20  IF (NERROR.EQ.IZERO) REWIND NTAPE
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS EITHER SKIP UNIT OR BACKSPACE UNIT.
C
  30  IF (NARGS.NE.IONE) CALL ERROR (10)
      JB = IARGS(1)
      IF (KIND(1).NE.IZERO) JB = ARGS(1)
      IF (JB.LE.IZERO) CALL ERROR (3)
      IF (NERROR.NE.IZERO) RETURN
      JA = IDIV (JB,NBLKPR,IND)
      IF (MOD(JB,NBLKPR).NE.IZERO) JA = JA + IONE
      IF (LLA.EQ.IFOUR) GO TO 50
C
C     SKIP JA RECORDS.
C
      DO 40 I=1,JA
        READ (NTAPE,90,END = 45) 
  40  CONTINUE
      RETURN
  45  CALL ERROR (267)
      RETURN
C
C     ..................................................................
C
C     BACKSPACE JA RECORDS.
C
  50  DO 60 I=1,JA
        BACKSPACE NTAPE
  60  CONTINUE
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS UNIT.
C
  70  IF (NARGS.EQ.IZERO .OR. NARGS.GT.ITWO) CALL ERROR (3)
      J = NBLKPR
      I = IARGS(1)
      IF (KIND(1).NE.IZERO) I = ARGS(1)
      IF (I.LE.IZERO) CALL ERROR (3)
      IF (NARGS.EQ.IONE) GO TO 80
      J = IARGS(2)
      IF (KIND(2).NE.IZERO) J = ARGS(2)
      IF (J.LE.IZERO) CALL ERROR (3)
  80  IF (NERROR.NE.IZERO) RETURN
      LURCD  = I
      NBLKPR = J
      NCHPR  = I * J
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  90  FORMAT (132A1)
C
C     ==================================================================
C
      END
*VARCON
      SUBROUTINE VARCON (NAME)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. VARCON V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C      DETERMINE WHETHER QUALIFIER IS NRMAX OR VARIABLE IN NALPHA(.)
C
C        NAMES CONSIDERED ARE ...
C
C             NRMAX AND V, W, X, Y, Z
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NAME(*)
C
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NRMAX1 / 10705 /
      DATA NRMAX2 /  1377 /
C
C     ==================================================================
C
      IF (NAME(1).NE.NRMAX1 .OR. NAME(2).NE.NRMAX2) GO TO 10
        ARG = RONE
        RETURN
C
C     ..................................................................
C
  10  DO 20 I=1,5
        J = I
        IF (NAME(1).EQ.NALPH(J) .AND. NAME(2).EQ.IZERO) GO TO 30
  20  CONTINUE
C
      J = IZERO
      RETURN
  30  ARG = J + ITWO
      RETURN
C
C     ==================================================================
C
      END
*VWHERE
          SUBROUTINE VWHERE (IND)
C
C         TO DETERMINE IF INPUT IS FROM A COMMAND PROC
C         OR FROM TERMINAL.
C         THIS PROCEDURE IS ONLY NEEDED FOR VAX MACHINE.
C         IND = 0 IF INPUT IS FROM TERMINAL AND
C         IND = 1 IF INPUT IS FROM A COMMAND PROC.
C
           IND = 0
          RETURN
          END
*XCUTEA
      SUBROUTINE  XCUTEA
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XCUTEA V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C     PROGRAM UNIT CALLS PROGRAM UNIT XSEGXX, WHERE
C        XX IS THE NUMBER OF THE SEGMENT CONTAINING THE PROGRAM UNIT
C           NEEDED TO EXECUTE THE PARTICULAR INSTRUCTION.
C
C     L1 =  1-10  FOR COMMANDS CONSISTING OF ONE OR TWO WORDS
C                    EXAMPLES RESET
C                             RESET X
C                             PRINT A
C
C     L1 = 11-20  FOR COMMANDS CONSISTING OF ONE WORD
C                    EXAMPLES ADD
C                             MPROP
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /OVRLAY/ JREPST
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
C
C     ==================================================================
C
  10  IF (L1.LT.21) GO TO 20
      CALL XCUTEB
      GO TO 9999
C
  20  GO TO ( 2004,2004,2004,2004,2004,2004,2004,2004, 900,1000,
     1       1100,2006,1300,1400,1500,1600,1700,1800,2002,2005), L1
C
C     ..................................................................
C
C     ( 9)   L1=9  AVAILABLE
C
 900  RETURN
C
C     (10)   L1=10 AVAILABLE
C
1000  RETURN
C
C     (11)   ADD, SUB, MULT, DIV, RAISE, SUBTRACT, DIVIDE, MULTIPLY
C
1100  IF(L2.LE.6) GO TO 2006
C
C            DIFFERENCE, DIVDIF,   SDIFFER,  SDIVDIF,
C
      IF (L2.GT.ITEN) GO TO 2015
      GO TO 2008
C
1300  GO TO (1301,2004,2004,2004,2008,2008,2008,1308,2004,1310,
     1       1311,2004,2005,2016,2008,2008,1317,1317,2008,2008,
     2       2008,2008), L2
C
C            GENERATE
C
1301  CALL GENER
      GO TO 9999
C
C            NEW PAGE
C
1308  CALL PAGE (IFOUR)
      GO TO 9999
C
C            CGS
C
1310  CALL PHYCON (0)
      GO TO 9999
C
C            SI
C
1311  CALL PHYCON (-IONE)
      GO TO 9999
C
C            TWOPLOTS, FOURPLOTS
C
1317  CALL PLOT24
      GO TO 9999
C
C     (14)   L1 = 14
C
1400  GO TO (2006,2006,1403,2005,2005,1406,1407,1408,2006,2006,
     1       2006,2006,2006,2006,2006,2016,2005,2005,2005,2005,
     2       2005,2005,2005), L2
C
C            REPEAT, EXECUTE, PERFORM
C
1403  JREPST = IONE
      CALL XSEG06
      IF (JREPST.GT.IZERO) GO TO 10
      GO TO 9999
C
C            INCREMENT
C
1406  JREPST = ITHRE
      CALL XSEG06
      GO TO 9999
C
C     L1 = 14, L2 = 7 UNAVAILABLE BECAUSE EXPAND DOES SOMETHING SPECIAL.
C
1407  CONTINUE
      GO TO 9999
C
C            RESTORE
C
1408  JREPST = ITHRE
      CALL XSEG06
      GO TO 9999
C
C     (15)   MDEFINE, MZERO, MERASE, MIDENT, MDIAGONAL,
C            ADEFINE, AZERO, AERASE, STATPLOTS
C            CONTENTS, DESCRIBE, GAMMA
C            SAMPLE WITHR, SAMPLE WITHOUTR
C
1500  GO TO (2001,2001,2001,2001,1505,1506,1507,2016,2022,2022), L2
C
1506  CALL CNTNTS
      GO TO 9999
C
1505  CALL SPLOTS
      GO TO 9999
C
1507  CALL DSCRIB
      GO TO 9999
C
C     (16)   MINVERT, INVERT, SOLVE, LABEL, ALABEL, MLABEL, EVALUATE
C
1600  GO TO (2003,2003,2005,2005,2005,2006), L2
C
C     (17)   L1 = 17
C
1700  GO TO (2001,2001,2001,2001,2007,2015,2015,2015,2015,2015,2016,
     1       2016,2016,2016,2016,2016), L2
C
C     (18)   MADD, MSUB, MTRANS, SCALAR, AMULT, MSUBTRACT
C            AADD, ASUB, ATRANS, ARAISE, ADIVIDE, ASUBTRACT, AMULTIPLY
C
1800  IF (L2.GT.8) GO TO 2007
      GO TO 2001
C
C            MMULT, MMULTIPLY
C            MRAISE
C            MKRONECKER
C            MTRIAN
C
2001  CALL XSEG01
      GO TO 9999
C
C     (19)   NORMLAGUERE, LAGUERE, HERMITE, LEGENDRE, TCHEBYSHEV, UCHEBYSHEV
C
2002  CALL XSEG02
      GO TO 9999
C
2003  CALL XSEG03
      GO TO 9999
C
C     ( 1)   RESET
C     ( 2)   PRINT, PRINT  A-F
C     ( 3)   PUNCH
C     ( 4)   APRINT, APRINT  A-F
C     ( 5)   READ, READ A-F
C     ( 6)   ABRIDGE
C     ( 7)   MPRINT, MPRINT A-F
C     ( 8)   NPRINT, NPRINT A-F
C            SET
C            FIXED, FLOATING, FLEXIBLE
C            SPACE
C
2004  CALL XSEG04
      GO TO 9999
C
C            PRINT NOTE
C            INTERACTIVE, WIDTH
C            BRIEF, FULL, TERMINAL, LENGTH, REMOTE, LOCAL, CRT
C     (20)   PARSUM, PARPRODUCT, RMS, SUM
C
2005  CALL XSEG05
      GO TO 9999
C
C     (12)   SIN, ASIN, SIND, ASIND, SINH, ASINH,
C            COS, ACOS, COSD, ACOSD, COSH, ACOSH,
C            TAN, ATAN, TAND, ATAND, TANH, ATANH, NEGEXP,
C            COT, ACOT, COTD, ACOTD, COTH, ACOTH,
C            ABS, ABSOLUTE, EXP, EXPONENT, LOG, LOGE, LOGTEN, ANTILOG,
C            SQRT, RAISE, INTEGER, FRACTION, SQUARE, RECIPROCAL
C
C            BEGIN, SCAN
C            COMPARE, IFEQ, IFGE, IFGT, IFLE, IFLT, IFNE
C
2006  JREPST = IZERO
      CALL XSEG06
      GO TO 9999
C
C            MEIGEN
C            ACOALES, AAVERA
C
2007  CALL XSEG07
      GO TO 9999
C
C            PLOT, NPLOT, CPLOT, NCPLOT
C            NICE PLOT, NICE NPLOT, NICE CPLOT, NICE NCPLOT
C            PAGE PLOT
C
2008  CALL XSEG08
      GO TO 9999
C
C            OMIT, DELETE, CHOOSE, RETAIN, REPLAC,
C            RECODE
C
2015  CALL XSEG15
      GO TO 9999
C
C            ROUND
C            DAYS
C            AVERAG, STDDEV, RANGE, MEDIAN, PERCEN, PROPOR
C
2016  CALL XSEG16
      GO TO 9999
C
2022  CALL XSEG22
      GO TO 9999
C
C     ..................................................................
C
9999  CALL ERROR (0)
      IF (LEVEL.LE.IZERO) RETURN
      JREPST = ITWO
      CALL XSEG06
      IF (LEVEL.LE.IZERO) RETURN
      GO TO 10
C
C     ==================================================================
C
      END
*XCUTEB
      SUBROUTINE  XCUTEB
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XCUTEB V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE MOST OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C     PROGRAM UNIT CALLS PROGRAM UNIT XSEGXX, WHERE
C        XX IS THE NUMBER OF THE SEGMENT CONTAINING THE PROGRAM UNIT
C           NEEDED TO EXECUTE THE PARTICULAR INSTRUCTION.
C
C     L1 = 21-50  FOR COMMANDS CONSISTING OF ONE WORD
C                    EXAMPLES SORT
C                             CORRELATION
C
C     L1 = 51-63  FOR COMMANDS CONSISTING OF TWO WORDS
C                    EXAMPLES CLOSE UP
C                             M(X'X)
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      LL = L1 - 20
C
      GO TO (2100,2200,2300,2400,2002,2001,2003,2002,2002,2011,
     1       2008,3200,3300,3400,2015,9000,9000,9000,9000,9000,
     2       9000,9000,9000,9000,2004,2004,2004,2004,2004,2004,
     3       2001,2001,2001,9000,2022,2022,2022,2022,2022,9000,
     4       9000, 9000, 9000), LL
C
C     ..................................................................
C
C     (21)   L1 = 21
C
2100  GO TO (2002,2002,2005,9000,2005,2005,9000,2002,2002,2005,
     1       2002,2002,2002,2002,2005,2005,2117,2016,2016,2016,
     2       2016,2016,2016,2016,2016,2016,2016), L2
C
C            NULL
C
2117  RETURN
C
C     (22)   POLYFIT, SPOLYFIT,  FIT,  SFIT,  MORTHO,  BESTCP, LARFIT
C
2200  IF (L2.EQ.IFIVE) GO TO 2007
      IF (L2.EQ.6) GO TO 2206
      IF (L2.EQ.7) GO TO 2003
      GO TO 2012
C
2206  CALL BESTCP
      RETURN
C
C     (23)   L1 = 23
C
2300  IF (L2.GT.6 .AND. L2.LT.ITEN) GO TO 9000
      GO TO 2005
C
C     (24)   L1 = 24
C
2400  GO TO (2401,2401,2010,2005,2016,2012,2012,2016,2016,2016,
     1       2003,2003,2010,2010,2020,2020), L2
C
C            STATIS, SSTATIS
C
2401  CALL STATIS
      RETURN
C
3200  IF (L2.LE.6) GO TO 2005
      IF (L2.LE.15) GO TO 3207
C
C     TEKTRONIX PLOT, TEKTRONIX AXIS, TEKTRONIX OPTIONS.
C
      CALL TEKINT
      RETURN
C
C            CALCOM TAPE, CALCOM SPEED, CALCOM SIZE, CALCOM PLOT
C
3207  CALL CALINT
      RETURN
C
C     (33)   STEM LEAF
C
3300  CALL SLOMNI
      RETURN
C
C     (34)   MULTILINGUAL.
C
3400  CALL LANGUA
      RETURN
C
C     (36)   .
C     (37)   .
C     (38)   .
C     (39)   .
C     (40)   .
C     (41)   .
C     (42)   .
C     (43)   .
C     (44)   .
C
C     (26)   MVECDIAG, MVECMAT, MMATVEC, MVECDIAGONAL
C     (51)   M(XX'), M(X'X),  M(XAX'),  M(X'AX)
C     (52)   M(AD), M(DA)
C     (53)   M(V'A), M(AV)
C
2001  CALL XSEG01
      RETURN
C
C            ROWSUM, ROW SUM, PRODUCT
C            SORT, ORDER, HIERARCHY
C            EXCHANGE
C            FLIP
C            CHANGE
C     (25)   SELECT, SEARCH, CENSOR, MATCH
C     (28)   ITERATE, ISETUP, ISOLATE
C     (29)   SEPARATE, INSERT, MAXMIN, EXTREMA
C
2002  CALL XSEG02
      RETURN
C
C            CORRELATION, SCORRELATION
C     (27)   MPROPERTIES, APROPERTIES, SMPROP, SAPROP
C
2003  CALL XSEG03
      RETURN
C
C     (45)   READ TAPE
C     (46)   CREAD TAPE
C     (47)   WRITE TAPE
C     (48)   SET TAPE
C     (49)   CSET TAPE
C     (50)   ENDFILE TAPE, REWIND TAPE, SKIP TAPE, BACKSPACE TAPE, UNIT
C
2004  CALL XSEG04
      RETURN
C
C            DEFINE
C            MAX, MAXIMUM, MIN, MINIMUM
C            LIST
C            NO LIST
C            CLOSE UP, COUNT, SHORTEN, EXPAND, DUPLICATE
C            MOVE, AMOVE, MMOVE
C            PROMOTE, DEMOTE
C            DIMENSION, DIM
C            GAUSS QUADRATURE
C     (32)   COMPLEX ARITHMETIC
C
2005  CALL XSEG05
      RETURN
C
C     MORTHO
C
2007  CALL XSEG07
      RETURN
C
C     (31)   THERMODYNAMIC INSTRUCTIONS.
C            CTOF, FTOC, ATOMIC
C            MOLWT, EINSTEIN, PFTRANS, PFATOM, PARTFUNCTION, BOLDIST
C
2008  CALL XSEG08
      RETURN
C
C            ONEWAY, SONEWAY
C            RANKS
C
2010  CALL XSEG10
      RETURN
C
C     (30)   BESSEL INSTRUCTIONS, HARMONIC, ELLIPTICAL, STRUVE
C
2011  CALL XSEG11
      RETURN
C
C            TWOWAY, STWOWAY
C
2012  CALL XSEG12
      RETURN
C
C     (35)   L1 = 35 FOR THE TABLE INSTRUCTIONS -
C            FREQUENCY, SUM, AVERAGE, STDDEV, MINIMUM, MAXIMUM, RANGE
C            MEDIAN, PERCENTAGES, PROPORTIONS,
C            RPERCENTAGE, CPERCENTAGE, RPROPORTION, CPROPORTION.
C
2015  CALL XSEG15
      RETURN
C
C            ERROR, CERF, SININTEGRAL, COSINTEGRAL, EINTEGRAL,
C            NEGEINTEGRAL, HSININTEGRAL, HCOSINTEGRAL
C            EXPINTEGRAL, EEXPINTEGRAL
C            HISTOGRAM, NHISTOGRAM
C            FREQUENCY
C            F PROBABILITY
C
2016  CALL XSEG16
      RETURN
C
C            CONTINGENCY
C            SPLIT PLOT
C
2020  CALL XSEG20
      RETURN
C
C     (55)   L1 = 55  THRU  59   DISTRIBUTION PROPERTY COMMANDS.
C     (56)   .
C     (57)   .
C     (58)   .
C     (59)   .
C     (60)   .
C     (61)   .
C     (62)   .
C     (63)   .
C
2022  CALL XSEG22
      RETURN
C
C     ..................................................................
C
9000  RETURN
C
C     ==================================================================
C
      END
*XFORMT
      SUBROUTINE XFORMT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XFORMT V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     LOOK FOR ONE OF THE LETTERS A-F FOLLOWED BY A NON-ALPHANUMERIC
C        CHARACTER.
C
C     A $ = 46 STOPS THE SCAN.
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
      DATA ICA / 15 /
      DATA ICB / 35 /
      DATA ICC / 46 /
C
C     ==================================================================
C
      ISTART = KRDPOS
      DO 20 I=ISTART,LKARD
        KRDPOS = KRDPOS + IONE
        IF (KARD(KRDPOS).LT.ITEN .OR. KARD(KRDPOS).GT.ICA) GO TO 10
        IF (KARD(KRDPOS+1).LE.ICB) GO TO 40
        GO TO 30
  10    IF (KARD(KRDPOS).EQ.ICC) GO TO 40
  20  CONTINUE
C
C     CALL PCKFMT TO STORE FORMAT.
C        IF IND=0  FORMAT IS O.K. AND STORED.
C        IF IND=1  NUMBER OF  (  DOES NOT EQUAL THE NUMBER OF ).
C
  30  CALL PCKFMT (IND)
      IF (IND.EQ.IZERO) RETURN
  40  CALL ERROR (205)
      RETURN
C
C     ==================================================================
C
      END
*XHEAD    
      SUBROUTINE XHEAD        
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  XHEAD V 7.00 10/29/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PICK UP COLUMN HEADING AND STORE IT IN LHEAD(.).      
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
      COMMON /SCNCHR/ NEWCRD(80)
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
       CHARACTER LA*1
       CHARACTER NEWCRD*1
       CHARACTER K*1
       CHARACTER LFMTP*80 
       CHARACTER LHH(1)*1    
C         
C     ==================================================================        
C         
      ISTART = KRDPOS         
      DO 10 I=ISTART,LKARD    
        IF (KARD(KRDPOS).LT.ITEN) GO TO 20        
        IF (KARD(KRDPOS).EQ.  46) GO TO 40        
        KRDPOS = KRDPOS + IONE
  10  CONTINUE      
      GO TO 40      
C         
  20  CALL AARGS (KARD)       
      I = ARG       
      IF (KARG.NE.IZERO .OR. I.LE.IZERO .OR. I.GT.NCOL) GO TO 40      
      IF (KARD(KRDPOS).EQ.36) GO TO 50  
      IF (KARD(KRDPOS).EQ.46) GO TO 40  
C         
      ISTART = KRDPOS + IONE  
      DO 30 J=ISTART,LKARD    
        KRDPOS = KRDPOS + IONE
        IF (KARD(KRDPOS).EQ.36) GO TO 50
        IF (KARD(KRDPOS).EQ.46) GO TO 40
  30  CONTINUE      
C         
  40  CALL ERROR (204)        
      RETURN        
C         
C     ..................................................................        
C         
C     SLASH FOUND. PICK UP NEXT 12 CHARACTERS IN A1 FORMAT AND PACK.  
C         
  50  IF (NERROR.NE.IZERO) RETURN       
      IR = 12       
      IF (KRDPOS+12.GT.KRDEND+ITHRE) IR = KRDEND + ITWO - KRDPOS      
      DO 70 J = 1, KRDEND
      K = NEWCRD (J)
        DO 60 J1 = 1, 74
        IF (K.EQ.LA(J1)) KARD (J+2) = J1 - IONE
  60    CONTINUE
  70  CONTINUE
      CALL PREPAK (IONE,I,IR,KARD(KRDPOS+1),LHH,LFMTP,IND)      
      RETURN        
C         
C     ==================================================================        
C         
      END 
*XSEG01
      SUBROUTINE  XSEG01
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG01 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 01.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.15)  GO TO 10
        CALL MOP
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.17) GO TO 60
      IF (L2.GT.4)  GO TO 60
      GO TO (20,30,40,50), L2
  20    CALL MMULT
        RETURN
C
C     ..................................................................
C
  30    CALL MRAISE
        RETURN
C
C     ..................................................................
C
  40    CALL MKRON
        RETURN
C
C     ..................................................................
C
  50    CALL MTRIAN
        RETURN
C
C     ..................................................................
C
  60  IF (L1.NE.18) GO TO 70
        CALL MATRIX
        RETURN
C
C     ..................................................................
C
  70  IF (L1.NE.26) GO TO 80
        CALL EXPCON
        RETURN
C
C     ..................................................................
C
  80  IF (L1.NE.51) GO TO 90
        CALL MXTX
        RETURN
C
C     ..................................................................
C
  90  IF (L1.NE.53) GO TO 100
        CALL ARYVEC
        RETURN
C
C     ..................................................................
C
 100    CALL MDAMAD
        RETURN
C
C     ==================================================================
C
      END
*XSEG02
      SUBROUTINE  XSEG02
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG02 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 02.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.19)  GO TO 10
        CALL ALLSUB
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.21) GO TO 60
      IF (L2.GT.2)  GO TO 20
        CALL PROROW
        RETURN
C
C     ..................................................................
C
  20  IF (L2.NE.8. AND. L2.NE.9 .AND. L2.NE.14) GO TO 30
        CALL SORDER
        RETURN
C
C     ..................................................................
C
  30  IF (L2.NE.11) GO TO 40
        CALL EXCHNG
        RETURN
C
C     ..................................................................
C
  40  IF (L2.NE.12) GO TO 50
        CALL FLIP
        RETURN
C
C     ..................................................................
C
  50  IF (L2.NE.13) GO TO 60
        CALL CHANGE
        RETURN
C
C     ..................................................................
C
  60  IF (L1.NE.25) GO TO 80
      IF (L2.EQ.4)  GO TO 70
        CALL SELECT
        RETURN
C
C     ..................................................................
C
  70    CALL INTERP
        RETURN
C
C     ..................................................................
C
  80  IF (L1.NE.28) GO TO 90
        CALL ITERAT
        RETURN
C
C     ..................................................................
C
  90    CALL CMSEPA
        RETURN
C
C     ==================================================================
C
      END
*XSEG03
      SUBROUTINE  XSEG03
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG03 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 03.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.22)  GO TO 10
        CALL LARFIT
        RETURN
C
C     ..................................................................
C
  10    CALL EXINVT
        RETURN
C
C     ==================================================================
C
      END
*XSEG04
      SUBROUTINE  XSEG04
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG04 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 04.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.GT.8) GO TO 70
      GO TO(10,20,30,40,50,60,40,20), L1
  10    CALL RESET
        RETURN
C
C     ..................................................................
C
  20    CALL PRINTX
        RETURN
C
C     ..................................................................
C
  30    CALL PUNCH
        RETURN
C
C     ..................................................................
C
  40    CALL APRINT
        RETURN
C
C     ..................................................................
C
  50    CALL READX
        RETURN
C
C     ..................................................................
C
  60    CALL ABRIDG
        RETURN
C
C     ..................................................................
C
  70  IF (L1.NE.13) GO TO 100
      IF (L2.NE.2) GO TO 80
        CALL SET
        RETURN
C
C     ..................................................................
C
  80  IF (L2.EQ.9) GO TO 90
        CALL FIXFLO
        RETURN
C
C     ..................................................................
C
  90    CALL SPACE
        RETURN
C
C     ..................................................................
C
 100    CALL UNITX
        RETURN
C
C     ==================================================================
C
      END
*XSEG05
      SUBROUTINE  XSEG05
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG05 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 05.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.13)  GO TO 10
        CALL NOTEPR
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.14) GO TO 20
        CALL KEYBRD
        RETURN
C
C     ..................................................................
C
  20  IF (L1.NE.16) GO TO 30
        CALL LABEL
        RETURN
C
C     ..................................................................
C
  30  IF (L1.NE.20) GO TO 40
        CALL MSCROW
        RETURN
C
C     ..................................................................
C
  40  IF (L1.NE.21) GO TO 80
      IF (L2.NE.ITHRE)  GO TO 50
        CALL DEFINZ
        RETURN
C
C     ..................................................................
C
  50  IF (L2.NE.IFIVE .AND. L2.NE.6) GO TO 60
        CALL EXTREM
        RETURN
C
C     ..................................................................
C
  60  IF (L2.NE.ITEN) GO TO 70
        CALL ERASE
        RETURN
C
C     ..................................................................
C
  70  IF (L1.NE.21) GO TO 80
        IF (L2.EQ.15) CALL LIST (IZERO)
        IF (L2.EQ.16) CALL LIST (IONE)
        RETURN
C
C     ..................................................................
C
  80  IF (L1.NE.23) GO TO 120
      IF (L2.GT.IFIVE)  GO TO 90
        CALL MISC2
        RETURN
C
C     ..................................................................
C
  90  IF (L2.NE.6) GO TO 100
        CALL MOVE
        RETURN
C
C     ..................................................................
C
 100  IF (L2.NE.ITEN .AND. L2.NE.11) GO TO 110
        CALL PDMOTE
        RETURN
C
C     ..................................................................
C
 110  IF (L2.NE.12) RETURN
        CALL DIMENS
        RETURN
C
C     ..................................................................
C
 120  IF (L1.NE.24) GO TO 130
        CALL GQUAD
        RETURN
C
C     ..................................................................
C
 130    CALL COMPLX
        RETURN
C
C     ==================================================================
C
      END
*XSEG06
      SUBROUTINE  XSEG06
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG06 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 06.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /OVRLAY/ JREPST
C
C     ==================================================================
C
      IF (JREPST.EQ.IZERO)  GO TO 10
        CALL REPINC (JREPST)
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.11) GO TO 20
        CALL ARITH
        RETURN
C
C     ..................................................................
C
  20  IF (L1.NE.12) GO TO 30
        CALL FUNCT
        RETURN
C
C     ..................................................................
C
  30  IF (L1.NE.14) GO TO 40
        IF (L2.LE.ITWO) CALL BEGIN
        IF (L2.GE.9) CALL IFS
        RETURN
C
C     ..................................................................
C
  40    CALL EVALOM
        RETURN
C
C     ==================================================================
C
      END
*XSEG07
      SUBROUTINE  XSEG07
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG07 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 07.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.17)  GO TO 10
        CALL MEIGEN
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.18) GO TO 20
        CALL COALES
        RETURN
C
C     ..................................................................
C
  20    CALL MORTHO
        RETURN
C
C     ==================================================================
C
      END
*XSEG08
      SUBROUTINE  XSEG08
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG08 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 08.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.11)  GO TO 10
        CALL DIFFER
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.13) GO TO 20
        CALL ONLPLT
        RETURN
C
C     ..................................................................
C
  20  IF (L2.GT.2) GO TO 30
        CALL CVTDEG
        RETURN
C
C     ..................................................................
C
  30  IF (L2.GT.4) GO TO 40
        CALL ATOMIC
        RETURN
C
C     ..................................................................
C
  40    CALL THERMO
        RETURN
C
C     ==================================================================
C
      END
*XSEG10
      SUBROUTINE  XSEG10
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG10 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 10.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L2.NE.3)  GO TO 10
        CALL RANKS
        RETURN
C
C     ..................................................................
C
  10    CALL ONEWAY
        RETURN
C
C     ==================================================================
C
      END
*XSEG11
      SUBROUTINE  XSEG11
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG11 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 11.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C     CALL PROGRAM UNITS WRITTEN BY B. A. PEAVY.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY AND DAVID HOGBEN,
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L2.NE.30 .AND. L2.NE.31) GO TO 10
      CALL ELLIPT
      RETURN
C
C     ..................................................................
C
  10  IF (L2.NE.35 .AND. L2.NE.36) GO TO 20
      CALL PRESVE
      RETURN
C
C     ..................................................................
C
  20  IF (L2.NE.37) GO TO 30
      CALL HARMON
      RETURN
C
C     ..................................................................
C
  30  IF (L2.GT.12) GO TO 40
      CALL BESEL1
      RETURN
C
C     ..................................................................
C
  40  IF (L2.GT.28) GO TO 50
      CALL BESEL2
      RETURN
C
C     ..................................................................
C
  50  CALL BESEL3
      RETURN
C
C     ==================================================================
C
      END
*XSEG12
      SUBROUTINE  XSEG12
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG12 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 12.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C     IF L1 = 22, CALL LSFIT OR VARFIT.
C     IF L1 = 24, CALL TWOWAY.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD. 20899.
C                          TELEPHONE 301-975-2844
C                  ORIGINAL VERSION -  JANUARY, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IIRGS(100)
C
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      REAL             DIG, SD, SU, VARNCE
C
      EQUIVALENCE (IIRGS(1),LFMT(1))
C
C     ==================================================================
C
      IF (L1.EQ.22) GO TO 10
      CALL TWOWAY
      RETURN
C
C     ..................................................................
C
C     CHECK TO SEE IF COMMAND IS VFIT OR VPOLYFIT.
C
  10  IF (L2.LT.8) GO TO 20
      CALL VARFIT
      RETURN
C
C     ..................................................................
C
C     COMMAND IS FIT, POLYFIT, SFIT OR SPOLYFIT
C
  20  LSIND = IZERO
      CALL LSFIT (JS,ISTART,M,N,VARNCE,SU,SD,MMTXR,MMTXC,NIT,DIG,LSIND)
      IF (NERROR.NE.IZERO) RETURN
C     IF JS = 0,  NO STORAGE REQUIRED.
C
      IF (JS.EQ.IZERO) GO TO 30
C
C     STORE RESULTS.
C
      CALL LSTORE (JS,IIRGS,ISTART,M,VARNCE,SU,N,SD,MMTXR,MMTXC)
C
C     START PRINTING IF POLYFIT OR FIT.
C
  30  IF (L2.EQ.ITWO .OR. L2.EQ.IFOUR) GO TO 40
      CALL LSPRNT (M,N,IIRGS,SD,VARNCE,SU,NIT,DIG,NX,LSIND)
C
C     CLEAR OUT ALL ARITHMETIC ERROR MESSAGES.
C
  40  DO 50 I=1,KMES
        MESS(I) = IZERO
  50  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*XSEG15
      SUBROUTINE  XSEG15
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG15 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 15.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.EQ.35) GO TO 20
      IF (L1.EQ.11) GO TO 10
      IF (L2.GT.9)  GO TO 10
        CALL EDITDA
        RETURN
C
C     ..................................................................
C
  10    CALL RECODE
        RETURN
C
C     ..................................................................
C
  20    CALL TABLE
        RETURN
C
C     ==================================================================
C
      END
*XSEG16
      SUBROUTINE  XSEG16
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG16 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 16.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.13)  GO TO 10
        CALL SEIC
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.14) GO TO 20
        CALL SEEEC
        RETURN
C
C     ..................................................................
C
  20  IF (L1.NE.15 .AND. L1.NE.21) GO TO 30
      IF (L1.EQ.21 .AND. L2.GE.26) GO TO 40
        CALL SEC
        RETURN
C
C     ..................................................................
C
  30  IF (L1.NE.17) GO TO 50
        CALL SCC
        RETURN
C
C     ..................................................................
C
  40    CALL SIEC
        RETURN
C
C     ..................................................................
C
  50  IF (L2.NE.5) GO TO 60
        CALL FPROB
        RETURN
C
C     ..................................................................
C
  60  IF (L2.NE.8 .AND. L2.NE.9) GO TO 70
        CALL HISTGM
        RETURN
C
C     ..................................................................
C
  70    CALL FRDIST
        RETURN
C
C     ==================================================================
C
      END
*XSEG20
      SUBROUTINE  XSEG20
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG20 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 20.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L2.NE.15)  GO TO 10
        CALL CONTB
        RETURN
C
C     ..................................................................
C
  10    CALL SPLITP
        RETURN
C
C     ==================================================================
C
      END
*XSEG22
      SUBROUTINE  XSEG22
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. XSEG22 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO CALL ALL PROGRAM UNITS IN SEGMENT 22.
C
C     CALL SPECIFIC PROGRAM UNIT TO EXECUTE OMNITAB INSTRUCTIONS
C        DEPENDING UPON THE VALUES OF L1 AND L2.
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
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.15)  GO TO 10
        CALL RNDSMP
        RETURN
C
C     ..................................................................
C
  10    CALL DISPRO
        RETURN
C
C     ==================================================================
C
      END
*YUGOSL
      SUBROUTINE YUGOSL
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. YUGOSL V 7.00  4/ 5/90. **
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
      DATA L( 13) /   84100000 /
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
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204132 /
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
      DATA L(219) / 1200314580 /
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
      DATA L(245) / 1400600000 /
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
      DATA L(260) / 1433101220 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
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
     1      11300,   7102,   4631,   7082,  14406,  11664,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,   8883,   4797,   9524,  12159,  10746 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       5968,   2916,   8908,   8907,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1327604185, 1215910746 /
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
      DATA LP( 5)         / 1200314580             /
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
