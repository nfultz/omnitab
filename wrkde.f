*DBEJ
      DOUBLE PRECISION FUNCTION DBEJ (X,N,M)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   DBEJ V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE BESSEL FUNCTIONS ...
C
C     L2 =  1   BJZERO  OF (E) PUT IN COLUMN (C)
C     L2 =  2   BJONE   OF (E) PUT IN COLUMN (C)
C     L2 =  3   BYZERO  OF (E) PUT IN COLUMN (C)
C     L2 =  4   BYONE   OF (E) PUT IN COLUMN (C)
C     L2 =  5   BIZERO  OF (E) PUT IN COLUMN (C)
C     L2 =  6   BIONE   OF (E) PUT IN COLUMN (C)
C     L2 =  6   BIONE   OF (E) PUT IN COLUMN (C)
C     L2 =  7   BKZERO  OF (E) PUT IN COLUMN (C)
C     L2 =  8   BKONE   OF (E) PUT IN COLUMN (C)
C     L2 =  9   EXIZERO OF (E) PUT IN COLUMN (C)
C     L2 = 10   EXIONE  OF (E) PUT IN COLUMN (C)
C     L2 = 11   EXKZERO OF (E) PUT IN COLUMN (C)
C     L2 = 12   EXKONE  OF (E) PUT IN COLUMN (C)
C
C               WRITTEN BY -
C                      BRADLEY A. PEAVY,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /DTCONS/ DALOG2, DEULER
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DALOG2, DEULER
      DOUBLE PRECISION S(120), ST(240), T(120)
      DOUBLE PRECISION B, C, D, DA, E, H, Y, X
      DOUBLE PRECISION FDCOS, FDDIV, FDEXP, FDLOG, FDSIN, FDSQRT
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF, DPCG
C
      EQUIVALENCE (ST(1),A(1)), (S(1),ST(1)), (T(1),ST(121))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / 16.5D0            /
      DATA DPCB /  0.4D0            /
      DATA DPCC /  6.8D0            /
      DATA DPCD /  0.91D0           /
      DATA DPCE /  0.636619772368D0 /
      DATA DPCF /  0.5D-10          /
      DATA DPCG /  6.283185307D0    /
C
C     ==================================================================
C
      IF (DABS(X)-DPCA) 10,10,90
  10  DA = FDDIV (X,DTWO,IND)**2
      J  = FDDIV (X,DPCB,IND) + DPCC
      B  = J
      C  = J + N
      D  = -DONE
      IF (M.GT.IONE) D = DONE
      IF (M.GT.ITHRE) GO TO 30
      Y  = DONE
      DO 20 I=1,J
        Y = DONE + FDDIV(Y*DA,B*C,IND)*D
        B = B - DONE
        C = C - DONE
  20  CONTINUE
C
      IF (N.GT.IZERO) Y = FDDIV (X*Y,DTWO,IND)
      GO TO 240
C
  30  E = DONE
      S(1) = DEULER
      S(61) = S(1) - DHALF
      DO 40 I=2,60
        S(I) = S(I-1) - FDDIV (DONE,E,IND)
        S(I+60) = S(I) - FDDIV (DONE,DTWO*(E+DONE),IND)
        E = E + DONE
  40  CONTINUE
C
      E = FDLOG ( FDDIV (X,DTWO,IND) )
      DO 50 I=1,120
        T(I) = S(I) + E
  50  CONTINUE
C
      IF (M.LT.6) GO TO 60
      IF (X-DEHT) 60,60,90
  60  IA = IZERO
      IF (N.GT.IZERO) IA = 60
      IF (M.GT.IFIVE) D = -DONE
      I = J + IA + IONE
      Y = T(I)
      DO 70 IB=1,J
        I = J - IB + IA + IONE
        Y = T(I) - FDDIV (D*DA*Y,B*C,IND)
        B = B - DONE
        C = C - DONE
  70  CONTINUE
C
      IF (N.GT.IZERO) Y = FDDIV (X*Y,DTWO,IND)
      IF (M.GT.IFIVE) GO TO 80
      Y = Y*DPCE
      IF (N.NE.IZERO) Y = FDDIV (-DPCE,X,IND) + Y
      GO TO 240
C
  80  Y = -Y
      IF (N.NE.IZERO) Y = FDDIV (DONE,X,IND) - Y
      GO TO 240
C
  90  DA = DEHT * X
      H = N
      H = (DTWO*H) ** 2
      T(1) = FDDIV (H-DONE,DA,IND)
      D = T(1)
      DO 120 I=2,20
        K = I
        B = I
        C = (ITWO*I-IONE) ** 2
        T(I) = FDDIV (H-C,DA*B,IND)
        E = D
        D = T(I) * D
        E = DABS ( FDDIV (D,E,IND) )
        IF (DABS(D)-DPCF) 130,100,100
 100    IF (E-DPCD) 110,110,130
 110    T(I+2) = DZERO
 120  CONTINUE
C
 130  DA = -DONE
      IF (M.LE.IONE ) GO TO 190
      IF (M.LE.ITHRE) GO TO 140
      IF (M.LE.IFIVE) GO TO 190
      DA = DONE
 140  Y = DONE
      DO 150 I=1,K
        J = K - I + IONE
        Y = DONE + DA*Y*T(J)
 150  CONTINUE
C
      DA = DONE
      IF (X-DXEXP) 160,170,170
 160  DA = FDEXP(X)
 170  IF (M.LE.IFIVE) GO TO 180
      Y = FDDIV (Y,DA * FDSQRT (DPCE*X),IND)
      GO TO 240
C
 180  Y = FDDIV (Y*DA,FDSQRT(DPCG*X),IND)
      GO TO 240
C
 190  Y = FDSQRT (DPI*X)
      J = IDIV (K,ITWO,IND)
      K = ITWO * J
      J = J - IONE
      DA = DONE
      H = DA
      DO 200 I=1,J
        IA = K - ITWO*I + IONE
        DA = DONE - DA * T(IA) * T(IA+1)
        H  = DONE - H * T(IA) * T(IA-1)
 200  CONTINUE
C
      DA = FDDIV (DONE-T(1)*T(2)*DA,Y,IND)
      H  = FDDIV (T(1)*H,Y,IND)
      B  = FDSIN (X)
      C  = FDCOS (X)
      D  = DA - H
      E  = DA + H
      IF (M.GT.ITWO) GO TO 220
      IF (N.EQ.IZERO) GO TO 210
      Y  = E*B - D*C
      GO TO 240
C
 210  Y = D*B + E*C
      GO TO 240
C
 220  IF (N.EQ.IZERO) GO TO 230
      Y  = -D*B - E*C
      GO TO 240
C
 230  Y  = E*B - D*C
C
 240  DBEJ = Y
      RETURN
C
C     ==================================================================
C
      END
*DEFINZ
      SUBROUTINE DEFINZ
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DEFINZ V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DEFINE (K) INTO COLUMN (C)
C     DEFINE (K) INTO ROW (C), COL (C)
C     DEFINE ROW (C), COL (C) INTO ROW (C), COL (C)
C     DEFINE ROW (C), COL (C) INTO COL (C).
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
      IF (NARGS.NE.ITWO) GO TO 10
      J = IONE
      IF (KIND(1).EQ.IZERO) CALL ADRESS(1,J)
      CALL ADRESS (ITWO,I)
      IF (I.LT.IZERO) GO TO 110
      IF (J.LT.IZERO) GO TO 110
      GO TO 60
  10  IF (NARGS.EQ.IFOUR) GO TO 40
      IF (NARGS.LT.ITHRE .OR. NARGS.GT.IFOUR) GO TO 100
      IF (KIND(1).EQ.IZERO) GO TO 40
C
  20  I = NARGS
      GO TO 80
C
  30  IF (NERROR.EQ.IZERO) RC(L) = ARGS(1)
      RETURN
C
C     ..................................................................
C
  40  I = ITWO
      GO TO 80
  50  ARGS(1) = RC(L)
      IF (NARGS.EQ.IFOUR) GO TO 20
      CALL ADRESS (ITHRE,I)
      IF (I.LT.IZERO) GO TO 110
  60  IF (NERROR.NE.IZERO) RETURN
      IF (NRMAX.EQ.IZERO) GO TO 70
      IF (KIND(1).EQ.IZERO .AND. NARGS.EQ.ITWO) GO TO 120
      CALL VECTOR (ARGS(1),I)
      RETURN
C
C     ..................................................................
C
  70  CALL ERROR (9)
      RETURN
C
C     CHECK AND CALCULATE WORKSHEET ENTRY LOCATION INTO L
C
  80  CALL ADRESS (I,L)
      IF (L.LT.IZERO) GO TO 110
      IF (KIND(I-1).EQ.IZERO .AND. IARGS(I-1).GT.IZERO
     1   .AND. IARGS(I-1).LE.NROW) GO TO 90
      CALL ERROR (16)
      RETURN
C
C     ..................................................................
C
  90  L = L + IARGS(I-1) - IONE
      IF (I.GT.ITWO) GO TO 30
      IF (I.LE.ITWO) GO TO 50
 100  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 110  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 120  DO 130 IJ=1,NRMAX
        RC(I) = RC(J)
        I = I + IONE
        J = J + IONE
 130  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*DEXPLT
      SUBROUTINE DEXPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DEXPLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A DOUBLE EXPONENTIAL (LAPLACE)
C              PROBABILITY PLOT.
C              THE PROTOTYPE DOUBLE EXPONENTIAL DISTRIBUTION USED HEREIN
C              HAS MEAN = 0 AND STANDARD DEVIATION = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5 * EXP(-ABS(X)).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE DOUBLE EXPONENTIAL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE DOUBLE EXPONENTIAL DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE DOUBLE EXPONENTIAL PROBABILITY PLOT.
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
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
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
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, Q, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
C
C     ..................................................................
C
      CHARACTER MT*1, M*1
      CHARACTER LHEAD*1
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
        IF (Q.LE.RHALF) W(I) = FLOG (RTWO*Q)
        IF (Q.GT.RHALF) W(I) = -FLOG (RTWO*(RONE-Q))
  10  CONTINUE
C
      IF (LWIDE.GE.88) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.76 .AND. LWIDE.LT.88) WRITE (IPRINT,110)
     1   (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.76) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
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
 100  FORMAT (15X,18HDOUBLE EXPONENTIAL,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X,18HDOUBLE EXPONENTIAL,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X,18HDOUBLE EXPONENTIAL,14H PROB PLOT OF ,
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
*DIFFER
      SUBROUTINE DIFFER
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DIFFER V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 =  7, DIFFERENCES               Y IN (C), PUT IN (C),...,(C)
C           8, DIVDIFFERENCES  X IN (C), Y IN (C), PUT IN (C),...,(C)
C           9, SDIFFERENCES              Y IN (C), PUT IN (C),...,(C)
C          10, SDIVDIFFERENCES X IN (C), Y IN (C), PUT IN (C),...,(C)
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
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             FDIV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX / 15 /
C
C     ==================================================================
C
C     ERROR CHECKING
C
      IF (NRMAX.GT.IZERO) GO TO 10
        CALL ERROR ( 9)
        RETURN
C
C     L = 1, FOR DIFFERENCES
C         2, FOR DIVIDED DIFFERENCES
C
  10  L = 1
        IF (L2.EQ.8 .OR. L2.EQ.ITEN) L = ITWO
      IF (NARGS.GE.L) GO TO 30
  20    CALL ERROR (10)
        RETURN
  30  CALL ADRESS (L,K)
      IF (K.GE.IZERO) GO TO 50
  40    CALL ERROR (20)
        RETURN
  50  IF (NARGS.GE.NRMAX+L) GO TO 20
      IF (L.EQ.IONE) GO TO 60
        CALL ADRESS (IONE,L0)
        IF (L0.LT.IZERO) GO TO 40
  60  IF (L2.EQ.7 .OR. L2.EQ.8) GO TO 80
        IF (NARGS.GT.L) GO TO 80
          CALL ERROR (236)
          RETURN
  70  CALL ERROR (245)
      RETURN
  80  IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     NCOMP = NUMBER OF DIFFERENCES COMPUTED
C
      N1 = IDIV (LWIDE,NX,IND) - L
      NCOMP = MAX0 (N1,NARGS-L)
      NCOMP = MIN0 (NCOMP,NRMAX-IONE)
      IF (NCOMP.LT.IONE) GO TO 70
C
C     CHECK ON AMOUNT OF SCRATCH AREA NEEDED.
C
      IEND = NCOMP
      NSMAX = NRMAX
      DO 90 I=1,IEND
        NSMAX = NSMAX + (NRMAX-I)
        IF (NSMAX.LE.NS) GO TO 90
          CALL ERROR (214)
          NCOMP = I - IONE
          GO TO 100
  90   CONTINUE
 100  IF (NCOMP.LT.IONE) GO TO 20
C
C     STORE Y IN SCRATCH AREA.
C
      J = K
      DO 110 I=1,NRMAX
        A(I) = RC(J)
        J = J + IONE
 110  CONTINUE
      GO TO (120,150), L
C
C     ..................................................................
C
C     COMPUTE DIFERENCES
C
 120  JEND = NRMAX
      M = NRMAX + IONE
      N = IONE
      DO 140 I=1,NCOMP
        JEND = JEND - IONE
        N = N + IONE
        DO 130 J=1,JEND
          A(M) = A(N) - A(N-1)
          M = M + IONE
          N = N + IONE
 130    CONTINUE
 140  CONTINUE
      GO TO 180
C
C     COMPUTE DIVIDED DIFERENCES
C
 150  JEND = NRMAX
      IND = IZERO
      M = NRMAX + IONE
      N = IONE
      DO 170 I=1,NCOMP
        JEND = JEND - IONE
        N = N + IONE
        LU = L0 + I
        LV = L0
        DO 160 J=1,JEND
          A(M) = A(N) - A(N-1)
          A(M) = FDIV (A(M),RC(LU)-RC(LV),IND)
          LU = LU + IONE
          LV = LV + IONE
          M = M + IONE
          N = N + IONE
 160    CONTINUE
 170  CONTINUE
      IF (IND.NE.IZERO) CALL ERROR (106)
 180  IF (L2.EQ.9 .OR. L2.EQ.ITEN) GO TO 190
C
C     ..................................................................
C
C     PRINT RESULTS.
C
      CALL PRTDD (L,NCOMP)
      IF (L2.EQ.7 .AND. NARGS.EQ.IONE) RETURN
      IF (L2.EQ.8 .AND. NARGS.EQ.ITWO) RETURN
C
C     ..................................................................
C
C     STORE RESULTS.
C
 190  CALL STREDD (L)
      RETURN
C
C     ==================================================================
C
      END
*DIMENS
      SUBROUTINE DIMENS
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DIMENS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DIMENSION INSTRUCTION.
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
      IF (NARGS.EQ.ITWO .AND. KIND(1)+KIND(2).NE.IZERO) GO TO 10
      IF (NARGS.EQ.ITWO .AND. KIND(1)+KIND(2).EQ.IZERO) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  20  IF (IARGS(1).GT.IZERO .AND. IARGS(2).GT.IZERO .AND.
     1     IARGS(1)*IARGS(2).LE.NRC) GO TO 30
      CALL ERROR (15)
      RETURN
C
C     ..................................................................
C
  30  NROW  = IARGS(1)
      NCOL  = IARGS(2)
      NROLD = NRMAX
      NRMAX = MIN0 (NROW,NRMAX)
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*EDITDA
      SUBROUTINE EDITDA
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EDITDA V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR EDITING DATA.
C
C          *****  INSTRUCTIONS - WITH VALUES OF L2  *****
C
C     (6) OMIT ROWS WITH (K) IN COLUMN (C) AND PUT IN COLUMN (C)
C         OMIT ROWS WITH VALUES BETWEEN (K) AND (K) IN (C) PUT IN (C)
C         OMIT (K) FROM (C) CORR ROWS FROM (C)...(C) PUT IN (C)...(C)
C         OMIT (K) TO (K) FROM (C) CORR ROWS FROM (C)...(C) IN (C)...(C)
C     (7) DELETE ALL ROWS HAVING (K) IN COLS (C)...(C) PUT IN (C)...(C)
C         DELETE FROM (K) TO (K) IN COLS (C)...(C) PUT IN COLS (C)...(C)
C     (8) CHOOSE ROWS = (K) IN (C) CORR ROWS OF (C)...(C) INTO (C)...(C)
C         CHOOSE FROM (K) TO (K) IN (C) CORR (C)...(C) PUT IN (C)...(C)
C     (9) RETAIN FROM (K) TO (K) IN COLS (C)...(C) PUT IN COLS (C)...(C)
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
C                   CURRENT VERSION - FEBRUARY, 1990
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
C     ERROR CHECKING.
C
      IF (NRMAX.GT.IZERO) GO TO 10
        CALL ERROR ( 9)
        RETURN
  10  N = IDIV(NARGS-IONE,ITWO,IND)
      IF (N.GT.IZERO) GO TO 30
  20    CALL ERROR (10)
        RETURN
  30  NMOD = MOD (NARGS,ITWO)
      IF (KIND(1).EQ.IONE) GO TO 50
  40    CALL ERROR (20)
        RETURN
  50  IF (KIND(2).EQ.IZERO .AND. NMOD.EQ.IZERO) GO TO 40
      IF (L2.EQ.9 .AND. NMOD.NE.IZERO) GO TO 20
      M = NARGS - ITWO + NMOD
      DO 60 I=1,M
        K = I + ITWO - NMOD
        IARGS(I) = IARGS(K)
  60  CONTINUE
      NARGS = M
      KIND(1) = IZERO
      KIND(2) = IZERO
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     STORE COLUMNS IN SCRATCH AREA.
C
      KA = IONE
      DO 80 I=1,N
        KW = IARGS(I)
        DO 70 J=1,NRMAX
          A(KA) = RC(KW)
          KA = KA + IONE
          KW = KW + IONE
  70    CONTINUE
  80  CONTINUE
      NEWRMX = NRMAX
      JBEG = IDIV(NARGS,ITWO,IND) + IONE
      IF (L2.LT.8) GO TO 90
      GO TO 240
C
C     ==================================================================
C
C     COMPUTING FOR OMIT AND DELETE.
C
  90  IF (L2.EQ.6) GO TO 160
      IF (N.EQ.IONE) GO TO 160
      DO 150 I=2,N
        KA = IONE
        KW = IARGS(I)
        DO 140 J=1,NRMAX
          IF (RC(KW)-ARGS(1)) 130,100,110
 100      A(KA) = ARGS(1)
          GO TO 130
 110      IF (NMOD.EQ.IONE) GO TO 130
          IF (RC(KW)-ARGS(2)) 120,120,130
 120    A(KA) = ARGS(2)
 130    KA = KA+IONE
        KW = KW+IONE
 140    CONTINUE
 150    CONTINUE
C
 160  M = IZERO
      KA = IONE
      DO 230 I=1,NRMAX
        IF (A(KA)-ARGS(1)) 200,170,180
 170    IF (NMOD.EQ.IONE) GO TO 190
 180    IF (NMOD.EQ.IONE) GO TO 200
        IF (A(KA)-ARGS(2)) 190,190,200
C
C     SUBTRACT ONE FROM NEWRMX BECAUSE ROW OMITTED.
C
 190    NEWRMX = NEWRMX - IONE
        GO TO 220
C
C     MOVE DATA FROM SCRATCH AREA TO WORKSHEET
C
 200    DO 210 J=JBEG,NARGS
          KW = IARGS(J) + M
          KK = I + NRMAX*(J-N-IONE)
          RC(KW) = A(KK)
 210  CONTINUE
        M = M+IONE
 220    KA = KA+IONE
 230  CONTINUE
      GO TO 300
C
C     ==================================================================
C
C     COMPUTING FOR CHOOSE AND RETAIN.
C
 240  L = IONE
        IF (L2.EQ.9) L = N
      M = IZERO
      DO 290 I=1,NRMAX
        DO 270 K=1,L
          KA = I + NRMAX*(K-IONE)
          IF (A(KA)-ARGS(1)) 290,250,260
 250      IF (NMOD.EQ.IONE) GO TO 270
 260      IF (NMOD.EQ.IONE) GO TO 290
          IF (A(KA)-ARGS(2)) 270,270,290
 270      CONTINUE
C
C     MOVE DATA FROM SCRATCH AREA TO WORKSHEET.
C
          DO 280 J=JBEG,NARGS
            KW = IARGS(J) + M
            KK = I + NRMAX*(J-N-IONE)
            RC(KW) = A(KK)
 280  CONTINUE
          M = M+IONE
 290      CONTINUE
C
      NEWRMX = M
C
C     ==================================================================
C
C     RESET AREA BELOW NEWRMX TO 0.0.
C
 300  IBEG = NEWRMX+IONE
      DO 320 J=JBEG,NARGS
        KW = IARGS(J) + NEWRMX
        DO 310 I=IBEG,NRMAX
          RC(KW) = RZERO
          KW = KW+IONE
 310  CONTINUE
 320    CONTINUE
C
C     RESET NRMAX AND CALL INFORMATIVE DIAGNOSTIC.
C
      NROLD = NRMAX
      NRMAX = NEWRMX
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*ELLIPT
      SUBROUTINE ELLIPT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ELLIPT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE ELLIPTICAL FIRST AND ELLIPTICAL SECOND INSTRUCTIONS.
C
C     L2=30, K=1
C       ELLIPTICAL FIRST OF (E) PUT IN (C)
C     L2=31, K=2
C       ELLIPTICAL SECOND OF (E) PUT IN (C)
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
      DOUBLE PRECISION X
      DOUBLE PRECISION COMELL
C
      REAL             FDPCON
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NARGS.EQ.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL ADRESS (ITWO,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      X = ARGS(1)
      IF (KIND(1).EQ.IONE) GO TO 20
      CALL ADRESS (IONE,JA)
      IF (JA.LT.IZERO) CALL ERROR (20)
  20  IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      K = IONE
      IF (L2.EQ.31) K = ITWO
C
      DO 40 N=1,NRMAX
        IF (KIND(1).EQ.IONE) GO TO 30
        X = RC(JA)
        JA = JA + IONE
  30    RC(J) = FDPCON (COMELL(X,K) )
        J = J + IONE
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*EVAL
      SUBROUTINE EVAL (IW2,IWW2,W2,ITYPE,ISTART,ISTOP,ANS,ISUB,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81.   EVAL V 7.00 11/02/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--EVALUATE A STRING OF CODE THAT CONTAINS ONLY
C        VALUES, OPERATIONS, AND LIBRARY FUNCTIONS.
C        DATE--NOVEMBER 1, 1976.
C
C               ADAPTED TO OMNITAB BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1978.
C                   CURRENT VERSION - NOVEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IW2(*)
      DIMENSION ITYPE(*), IWW2(*)
      DIMENSION IOP(100), IOPC(100)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             W2(*)
      REAL             ANS
      REAL             TERM(100)
      REAL             FCOS, FDIV, FEXP, FEXP2, FLOG, FSIN, FSQRT, FTANH
C
      CHARACTER LA*1
      CHARACTER IW2*2
      CHARACTER IOPC*2, LALA*2
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LALA / '**' /
C
C     ==================================================================
C
C     START POINT.
C
      ANS = RZERO
      IND = IZERO
      DO 10 I=1,100
        TERM(I) = RZERO
        IOP(I)  = IZERO
        IOPC(I) = LA(45)
  10  CONTINUE
C
C     STEP 1 ...
C        OPERATE ON THE VECTOR IW2(.).
C        IW2(.) SHOULD CONTAIN NO PARENTHESES.
C        IW2(.) SHOULD CONTAIN ONLY--
C             NUMBERS
C             X VALUES
C             PARAMETER VALUES
C             PREVIOUSLY COMPUTED VALUES
C             OPERATIONS (5--+ - * / **)
C             LIBRARY FUNCTIONS.
C        COPY THE NUMBERS, X VALUES, PARAMETER VALUES,
C        AND PREVIOUSLY COMPUTED VALUES OVER TO THE TERM VECTOR.
C        COPY THE OPERATIONS OVER TO THE OPERATIONS VECTOR.
C        ELIMINATE THE LIBRARY FUNCTIONS BY EVALUATING THEM
C        WITH THE NEXT POTENTIAL TERM AND PUTTING
C        THE EVALUATED RESULT INTO THAT NEXT TERM.
C        OUTPUT THE VECTOR TERMS(.) AND THE VECTOR IOP(.)
C        WHICH CONTAIN TERMS AND OPERATIONS RESPECTIVELY.
C
      IF (ITYPE(ISTOP).EQ.ITHRE) GO TO 20
      IF (ITYPE(ISTOP)    .EQ.6) GO TO 20
      GO TO 30
C
  20  CALL ERROR (8)
      IND = IONE
      RETURN
C
C     ..................................................................
C
  30  NOP   = IZERO
      NTERM = IZERO
      I     = ISTART
  40  IDEL  = IONE
      IP1   = I + IONE
      IF (ITYPE(I).EQ.IONE)  GO TO 60
      IF (ITYPE(I).EQ.IZERO) GO TO 50
      IF (ITYPE(I).EQ.27)    GO TO 60
      IF (ITYPE(I).EQ.ITHRE) GO TO 70
      IF (ITYPE(I).GE.6 .AND. ITYPE(I).LT.27 .AND.
     1    ITYPE(IP1).EQ.27) GO TO 100
      IF (ITYPE(I).GE.6 .AND. ITYPE(I).LT.27 .AND.
     1    ITYPE(IP1).EQ.IONE) GO TO 100
      IF (ITYPE(I).GE.6 .AND. ITYPE(I).LT.27 .AND.
     1    ITYPE(IP1).EQ.IZERO) GO TO 100
C
  50  ISUBN = IWW2(I) + ISUB
      NTERM = NTERM + IONE
      TERM(NTERM) = RC(ISUBN)
      W2(I) = RC (ISUBN)
      IOP(NTERM) = 27
      GO TO 280
C
  60  NTERM = NTERM + IONE
      TERM(NTERM) = W2(I)
      IOP(NTERM) = 27
      GO TO 280
C
  70  IF (IW2(I).EQ.LA(40)) GO TO 80
      IF (IW2(I).EQ.LA(39)) GO TO 80
      IF (IW2(I).EQ.LA(41)) GO TO 90
      IF (IW2(I).EQ.LA(37)) GO TO 90
      IF (IW2(I).EQ.LALA)   GO TO 90
      CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
  80  NOP       = NOP + IONE
      IOPC(NOP) = IW2(I)
      IF (NTERM.EQ.IZERO) TERM(1) = RZERO
      IF (NTERM.EQ.IZERO) NTERM   = IONE
      GO TO 280
C
  90  NOP       = NOP + IONE
      IOPC(NOP) = IW2(I)
      IF (NTERM.NE.IZERO) GO TO 280
      CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
 100  IIX  = IWW2(I) - IFIVE
C
       GO TO (110,120,140,130,150,160,180,170,190,200,
     1        220,230,210,240,250,260,140,130,240,250),  IIX
C
 110  NTERM = NTERM + IONE
      TERM(NTERM) = FSQRT (W2(IP1))
      GO TO 270
C
 120  NTERM = NTERM + IONE
      ARG = W2(IP1)
      TERM(NTERM) = FEXP (ARG)
      GO TO 270
C
 130  NTERM = NTERM + IONE
      TERM(NTERM) = FLOG (W2(IP1))
      GO TO 270
C
 140  NTERM = NTERM + IONE
      TERM(NTERM) = ALOG10 (W2(IP1))
      GO TO 270
C
 150  NTERM = NTERM + IONE
      TERM(NTERM) = FSIN (W2(IP1))
      GO TO 270
C
 160  NTERM = NTERM + IONE
      TERM(NTERM) = FCOS (W2(IP1))
      GO TO 270
C
 170  NTERM = NTERM + IONE
      TERM(NTERM) = ATAN (W2(IP1))
      GO TO 270
C
C     PROVISION FOR ATAN2.
C
180   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
 190  NTERM = NTERM + IONE
      TERM(NTERM) = FTANH (W2(IP1))
      GO TO 270
C
 200  NTERM = NTERM + IONE
      TERM(NTERM) = ABS (W2(IP1))
      GO TO 270
C
 210  NTERM = NTERM + IONE
      TERM(NTERM) = AINT (W2(IP1))
      GO TO 270
C
C     PROVISION FOR AMOD.
C
220   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
C     PROVISION FOR SIGN.
C
230   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
C     PROVISION FOR AMAX1.
C
240   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     PROVISION FOR AMIN1
C
250   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
C     PROVISION FOR DIM.
C
260   CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
 270  IOP(NTERM) = 27
      IDEL = ITWO
 280  I = I + IDEL
      IF (I.LE.ISTOP) GO TO 40
C
C     STEP 2 ...
C        CHECK TO SEE THAT THE NUMBER OF TERMS =
C        ONE MORE THAN THE NUMBER OF OPERATIONS.
C        ALSO CHECK TO SEE IF THE SPECIAL CASE
C        EXISTS WHERE THERE IS ONLY 1 TERM--
C        IF SO, SET ANS = TO THIS FIRST TERM AND RETURN.
C
      NOPP1 = NOP + IONE
      IF (NTERM.EQ.NOPP1) GO TO 290
      CALL ERROR (45)
      IND = IONE
      RETURN
C
C     ..................................................................
C
 290  IF (NTERM.GE.ITWO) GO TO 300
      ANS = TERM(1)
      RETURN
C
C     ..................................................................
C
C     STEP 3 ...
C        OPERATE ON THE TERM(.) AND IOP(.) VECTORS.
C        AT THIS POINT WE HAVE ONLY ALTERNATING TERMS AND OPERATIONS
C        WHERE AN OPERATION IS ANY ONE OF THE 5--
C        +   -   *   /   **.
C        EVALUATE AND ELIMINATE ALL **.
C        SQUEEZE THE TERM(.) AND IOP(.) VECTORS UNTIL
C        UNTIL ALL ** ARE GONE.
C
 300  I = IONE
 310  IF (IOPC(I).NE.LALA) GO TO 340
      IP1 = I + IONE
      TERM(I) = FEXP2 (TERM(I) , TERM(IP1))
      NOPM1 = NOP - IONE
      IF (I.GE.NOP) GO TO 330
      DO 320 J=I,NOPM1
        JP1 = J + IONE
        JP2 = J + ITWO
        IOP(J)    = IOP(JP1)
        IOPC(J)   = IOPC(JP1)
        TERM(JP1) = TERM(JP2)
 320  CONTINUE
C
 330  NOP = NOPM1
      GO TO 350
C
 340  I = I + IONE
 350  IF (I.LE.NOP) GO TO 310
      NTERM = NOP + IONE
C
C     STEP 4 ...
C        OPERATE ON THE TERM(.) AND IOP(.) VECTORS.
C        AT THIS POINT WE HAVE ONLY ALTERNATING TERMS AND OPERATIONS
C        WHERE AN OPERATION IS ANY ONE OF THE 4--
C        +   -   *   /   .
C        EVALUATE AND ELIMINATE ALL * AND / IN SEQUENCE.
C        SQUEEZE THE TERM(.) AND IOP(.) VECTORS UNTIL
C        UNTIL ALL * AND / ARE GONE.
C
      I = IONE
 360  IF (IOPC(I).EQ.LA(41)) GO TO 370
      IF (IOPC(I).EQ.LA(37)) GO TO 380
      GO TO 420
C
 370  IP1 = I + IONE
      TERM(I) = TERM(I) * TERM(IP1)
      GO TO 390
C
 380  IP1 = I + IONE
      TERM(I) = FDIV (TERM(I),TERM(IP1),INDD)
 390  NOPM1 = NOP - IONE
      IF (I.GE.NOP) GO TO 410
      DO 400 J=I,NOPM1
        JP1 = J + IONE
        JP2 = J + ITWO
        IOP(J)    = IOP(JP1)
        IOPC(J)   = IOPC(JP1)
        TERM(JP1) = TERM(JP2)
 400  CONTINUE
C
 410  NOP = NOPM1
      GO TO 430
C
 420  I = I + IONE
 430  IF (I.LE.NOP) GO TO 360
      NTERM = NOP + IONE
C
C     STEP 5 ...
C        OPERATE ON THE TERM(.) AND IOP(.) VECTORS.
C        AT THIS POINT WE HAVE ONLY ALTERNATING TERMS AND OPERATIONS
C        WHERE AN OPERATION IS ANY ONE OF THE 2--
C        + OR - .
C        EVALUATE ALL + OR - OPERATIONS IN SEQUENCE.
C        SQUEEZE THE TERM(.) AND IOP(.) VECTORS UNTIL
C        UNTIL ALL + AND - OPERATIONS ARE GONE.
C
      IF (NOP.GE.IONE) GO TO 440
      ANS = TERM(1)
      RETURN
C
C     ..................................................................
C
 440  ANS = TERM(1)
      DO 450 I=1,NOP
        IP1 = I + IONE
        IF (IOPC(I).EQ.LA(40)) ANS = ANS + TERM(IP1)
        IF (IOPC(I).EQ.LA(39)) ANS = ANS - TERM(IP1)
 450  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*EV1PLT
      SUBROUTINE EV1PLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EV1PLT V 7.00  1/16/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES AN EXTREME VALUE TYPE 1
C              PROBABILITY PLOT.
C              THE PROTOTYPE EXTREME VALUE TYPE 1 DISTRIBUTION USED HERE
C              HAS MEAN = EULER'S NUMBER = 0.57721566
C              AND STANDARD DEVIATION = PI/SQRT(6) = 1.28254983.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (EXP(-X)) * (EXP(-(EXP(-X))))
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE EXTREME VALUE TYPE 1 PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE EXTREME VALUE TYPE 1 DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE EXTREME VALUE TYPE 1 PROBABILITY PLOT.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 272-295.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1975.
C                   CURRENT VERSION -  JANUARY, 1992.
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
C     ..................................................................
C
      CHARACTER LHEAD*1
      CHARACTER MT*1, M*1
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
        W(I) = -FLOG (FLOG(FDIV(RONE,W(I),IND)))
  10  CONTINUE
C
      IF (LWIDE.GE.83) WRITE (IPRINT,100) N, (LHEAD(I),I=1,12)
      IF (LWIDE.GE.72 .AND. LWIDE.LT.88) WRITE (IPRINT,110)
     1   (LHEAD(I),I=1,12), N
      IF (LWIDE.LT.71) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12)
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
        K = K + 1
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
 100  FORMAT (15X,13HEXTREME VALUE     ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X,13HEXTREME VALUE     ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X,13HEXTREME VALUE     ,14H PROB PLOT OF ,
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
*EV2PLT
      SUBROUTINE EV2PLT (X,Y,W,N,GAMMA)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EV2PLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A EXTREME VALUE TYPE 2
C              PROBABILITY PLOT
C              (WITH TAIL LENGTH PARAMETER VALUE = GAMMA).
C              THE PROTOTYPE EXTREME VALUE TYPE 2 DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL NON-NEGATIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = GAMMA * (X**(-GAMMA-1)) * EXP(-(X**(-GAMMA))).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE EXTREME VALUE TYPE 2 PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE  EXTREME VALUE TYPE 2 DISTRIBUTION
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
C     OUTPUT--A ONE-PAGE EXTREME VALUE TYPE 2 PROBABILITY PLOT.
C
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C                 --GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 272-295.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--DECEMBER  1972.
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
C                   CURRENT VERSION - NOVEMBER, 1989.
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
      REAL             GAMMA
      REAL             ATEMP(1), YINT(1), YSLOPE(1), V(1)
      REAL             AN, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
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
      IF (GAMMA.GE.RZERO) GO TO 10
      CALL ERROR (38)
      GO TO 110
  10  CALL SORTPP (X,N,Y)
      CALL UNIMED (N,W)
      DO 20 I=1,N
        W(I) = (-FLOG(W(I)))**FDIV(-RONE,GAMMA,IND)
  20  CONTINUE
C
      IF (LWIDE.LT.NCW) GO TO 30
      V(1) = GAMMA
      CALL RFORMT (0,ISIGD,V,A(1),1,20,NW,ND,MT(1),IRF)
      CALL RFORMT (1,ISIGD,A,V(1),0,0,NW,ND,MT(1),IRF)
  30  IF (LWIDE.GE.114) WRITE (IPRINT,120) N, (LHEAD(I),I=1,12),
     1     (MT(J),J=1,NW)
      IF (LWIDE.GE.NCW .AND. LWIDE.LT.114) WRITE (IPRINT,130)
     1       (LHEAD(I),I=1,12), N, (MT(J),J = 1,NW)
      IF (LWIDE.LT.NCW) WRITE (IPRINT,140) (LHEAD(I),I=1,12),
     1      (MT(J),J=1,NW)
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
C
 110  IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 120  FORMAT (15X,13HEXTREME VALUE     ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1,18H WITH PARAMETER = ,13A1)
 130  FORMAT ( 1X,13HEXTREME VALUE     ,13H PR. PLOT OF ,
     1   12A1,4H N =,I5,11H PARAMETER ,13A1)
 140  FORMAT ( 1X,13HEXTREME VALUE,12H PR PLOT OF ,12A1,8H PARAM. ,13A1)
 150  FORMAT (15X,26HPROB. PLOT CORR. COEFF. = ,F6.4,
     1            26H, ESTIMATES * INTERCEPT = ,50A1)
 160  FORMAT ( 1X,16HPLOT COR COEF = ,F6.4,
     1            14H, EST* INT. = ,35A1)
 170  FORMAT (15X,38HPROBABILITY PLOT CORRELATION COEFF. = ,F6.4)
C
C     ==================================================================
C
      END
*EXCHNG
      SUBROUTINE EXCHNG
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXCHNG V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXCHANGE COL (C) WITH (C), COL (C) WITH (C), ETC.
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
      REAL             WORK
C
C     ==================================================================
C
      IF (NARGS.LE.IZERO) GO TO 10
      IF (NARGS.EQ.IDIV(NARGS,ITWO,IND)*ITWO) GO TO 20
  10  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  DO 40 I=1,NARGS,2
        II = I
        CALL ADRESS (II,J)
        IF (J.LT.IZERO) GO TO 50
        CALL ADRESS (II+IONE,K)
        IF (K.LT.IZERO) GO TO 50
        IF (NERROR.NE.IZERO) RETURN
        DO 30 N=1,NRMAX
          JJ = J + N - IONE
          KK = K + N - IONE
          WORK = RC(JJ)
          RC(JJ) = RC(KK)
          RC(KK) = WORK
  30    CONTINUE
  40  CONTINUE
      RETURN
C
C     ..................................................................
C
  50  CALL ERROR (20)
      RETURN
C
C     ==================================================================
C
      END
*EXPCON
      SUBROUTINE EXPCON
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXPCON V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTIONS ...
C
C     L2 = 1,     MVECDIAG
C         MVECDIAG  MATRIX IN R , C  SIZE N , M  PUT DIAGONAL IN C
C         MVECDIAG  MATRIX IN R , C  SIZE N , M  PUT DIAGONAL IN R , C
C
C     L2 = 2,     MVECMAT
C         MVECMAT   MATRIX IN R , C  SIZE N , M PUT ROW BY ROW
C                                               AS A VECTOR IN  C
C         MVECMAT   MATRIX IN R , C  SIZE N , M PUT ROW BY ROW  AS A
C                                               VECTOR  IN  R , C
C
C     L2=3,     MMATVEC
C         MMATVEC   VECTOR C PUT AS ROW X ROW MATRIX IN R , C SIZE N , M
C         MMATVEC   VECTOR R , C PUT AS ROW X ROW MATRIX IN R , C
C                                               SIZE N X M
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
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
      IF (NARGS.EQ.IFIVE .OR. NARGS.EQ.6) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  KRR  = IZERO
      J    = NARGS
      MKKR = 226
      CALL CKIND (J)
      IF (J.EQ.IZERO .AND. L2.LT.ITWO) GO TO 20
      IF (J.EQ.IZERO .AND. L2.EQ.ITWO) GO TO 90
      IF (J.EQ.IZERO .AND. L2.GT.ITWO) GO TO 160
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
C     VEC DIAG ****.
C
  20  IARGS(7) = MIN0(IARGS(3),IARGS(4))
      IARGS(8) = IONE
      IF (NARGS.EQ.6) GO TO 30
      IARGS(6) = IARGS(5)
      IARGS(5) = IONE
  30  IF (IARGS(5)+IARGS(7)-IONE.LE.NROW) GO TO 40
      IARGS(7) = NROW - IARGS(5) + IONE
      KRR      = MKKR
C
C     ERROR 226. COLUMN NOT LONG ENOUGH TO STORE ALL ELEMENTS. ONLY NROW
C        WILL BE STORE.
C
  40  J = ITWO
      CALL MTXCHK (J)
      IF (J.EQ.IZERO) GO TO 50
      IF (J.EQ.IONE) CALL ERROR (3)
      IF (J.GT.IONE) CALL ERROR (17)
  50  IF (NERROR.NE.IZERO) RETURN
      IF (KRR.NE.IZERO .AND. MKKR.EQ.226) CALL ERROR (226)
      IF (KRR.NE.IZERO .AND. MKKR.EQ.227) CALL ERROR (227)
C
C     ..................................................................
C
      GO TO (60,110,180), L2
C
  60  IA = IARGS(1)
      IB = IARGS(7)
      DO 70 I=1,IB
        A(I) = RC(IA)
        IA = IA + NROW + IONE
  70  CONTINUE
C
      IA = IARGS(5)
      DO 80 I=1,IB
        RC(IA) = A(I)
        IA = IA + IONE
  80  CONTINUE
      RETURN
C
C     ..................................................................
C
C     65  VECTORIZE A MATRIX ***.
C
  90  IARGS(7) = IARGS(3) * IARGS(4)
      IARGS(8) = IONE
      IF (NARGS.EQ.6) GO TO 100
      IARGS(6) = IARGS(5)
      IARGS(5) = IONE
 100  MKKR     = 226
      GO TO 30
C
 110  IB = IARGS(7)
      IA = IARGS(1)
      N  = IARGS(3)
      M  = IARGS(4)
      IC = IONE
      DO 130 I=1,N
        IAA = IA
        DO 120 J=1,M
          A(IC) = RC(IAA)
          IF (IC.EQ.IB) GO TO 140
          IC = IC + IONE
          IAA = IAA + NROW
 120    CONTINUE
        IA = IA + IONE
 130  CONTINUE
 140  IA = IARGS(5)
C
      DO 150 I=1,IB
        RC(IA) = A(I)
        IA = IA + IONE
 150  CONTINUE
      RETURN
C
C     ..................................................................
C
C     TAKE A COLUMN AND RESTORE IT TO A MATRIX OR ARRAY.
C
 160  IARGS(8) = IARGS(NARGS)
      IARGS(7) = IARGS(NARGS-1)
      IARGS(6) = IARGS(NARGS-2)
      IARGS(5) = IARGS(NARGS-3)
      IF (NARGS.EQ.6) GO TO 170
      IARGS(2) = IARGS(1)
      IARGS(1) = IONE
 170  IARGS(3) = IARGS(7) * IARGS(8)
      IARGS(4) = IONE
      IF (IARGS(IONE)+IARGS(3)-IONE.LE.NROW) GO TO 40
      IARGS(3) = NROW - IARGS(1) + IONE
      MKKR     = 227
C
C     ERROR 227. NOT ENOUGH ELEMENTS IN COL TO RESTORE MATRIX OR ARRAY.
C        ELEMENTS AVAILABLE WILL BE USED.
C
      GO TO 40
C
 180  IA = IARGS(1)
      IB = IARGS(3)
      DO 190 I=1,IB
        A(I) = RC(IA)
        IA = IA + IONE
 190  CONTINUE
C
      IA = IARGS(5)
      N  = IARGS(7)
      M  = IARGS(8)
      IC = IONE
      DO 210 I=1,N
        IAA = IA
        DO 200 J=1,M
          RC(IAA) = A(IC)
          IF (IC.EQ.IB) RETURN
          IC = IC + IONE
          IAA = IAA + NROW
 200    CONTINUE
        IA = IA + IONE
 210  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*EXPPLT
      SUBROUTINE EXPPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXPPLT V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES AN EXPONENTIAL
C              PROBABILITY PLOT.
C              THE PROTOTYPE EXPONENTIAL DISTRIBUTION USED HEREIN
C              HAS MEAN = 1 AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(-X).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE EXPONENTIAL PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN
C              IS THE EXPONENTIAL DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT--A ONE-PAGE EXPONENTIAL PROBABILITY PLOT.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 207-232.
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
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FDIV, FLOG, FSQRT
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
        W(I) = -FLOG (RONE-W(I))
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
 100  FORMAT (15X,11HEXPONENTIAL       ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X,11HEXPONENTIAL       ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X,11HEXPONENTIAL       ,14H PROB PLOT OF ,
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
*EXTREM
      SUBROUTINE EXTREM
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXTREM V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 4,5 MAX     L2 = 6,7 MIN
C
C     MAX OF (C) TO (C)
C     MAX OF (C) TO (C), CORRESP ENTRY OF (C) TO (C), (C) TO (C), ETC.
C
C     MAX OF (C) TO (C)
C     MAX OF (C) TO (C), CORRESP ENTRY OF (C) TO (C), (C) TO (C), ETC.
C
C     MAX AND MIN ARE ABBREVIATIONS FOR MAXIMUM AND MINIMUM.
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
      IF (NARGS.GT.IZERO .AND. MOD(NARGS,ITWO).EQ.IZERO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
      J = IZERO
      IF (NRMAX.EQ.IONE) GO TO 80
      IF (NRMAX.GT.IONE) GO TO 30
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  30  J = IARGS(1)
      K = J + IONE
      L = K + NRMAX - ITWO
      IF (L2.GT.IFIVE) GO TO 50
C
C     FIND MAXIMUM
C
      DO 40 I=K,L
        IF (RC(J).LT.RC(I)) J = I
  40  CONTINUE
      GO TO 70
C
C     FIND MINIMUM
C
  50  DO 60 I=K,L
        IF (RC(J).GT.RC(I)) J = I
  60  CONTINUE
C
  70  J = J - IARGS(1)
  80  DO 90 I=1,NARGS,2
        K = IARGS(I) + J
        CALL VECTOR (RC(K),IARGS(I+1))
  90  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
