      SUBROUTINE ALLSUB
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ALLSUB V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTION IS OF THE FORM ...
C
C     XXXXXX OF ORDER (N) OF COLUMN (C), PUT IN COLUMN (C)
C
C        XXXXXX MAY BE  (A) NORMLAGUERRE POLYNOMIALS
C                       (B) LAGUERRE POLYNOMIALS
C                       (C) HERMITE POLYNOMIALS
C                       (D) UCHEBYCHEV POLYNOMIALS
C                       (E) LEGENDRE POLYNOMIALS
C                       (F) TCHEBYSHEV POLYNOMIALS
C
C     SEE RECURSIVE FORMULAE FOR THESE POLYNOMIALS IN CODE.
C        EACH OF THE INSTRUCTIONS REQUIRE THREE ARGUMENTS.
C
C               WRITTEN BY -
C                      PHILIP J. WALSH,
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
      REAL             FDIV
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.EQ.ITHRE) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (KIND(1)+KIND(3).EQ.IZERO) GO TO 30
  20  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
C     CHECK THAT X IS WITHIN WORKSHEET, GET ADDRESS OF COLUMN.
C
  30  CALL ADRESS (ITWO,LOCA)
      IF (LOCA.LT.IZERO) GO TO 20
      IARGS(4) = IARGS(1) + IARGS(3) - IONE
      KIND(4)  = IZERO
      CALL ADRESS (IFOUR,LOCB)
      IF (LOCB.LT.IZERO) GO TO 20
      CALL ADRESS (ITHRE,LOCB)
      IF (NRMAX.NE.IZERO) GO TO 40
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IJK = LOCA
      IJ  = LOCB
      DO 90 I=1,NRMAX
        A(1) = RC(IJK)
C
        GO TO (50,50,60,60,70,70), L2
C
  50    RC(IJ) = RONE - A(1)
        GO TO 80
C
  60    RC(IJ) = RTWO * A(1)
        GO TO 80
C
  70    RC(IJ) = A(1)
  80    IJK = IJK + IONE
        IJ  = IJ + IONE
  90  CONTINUE
C
      IF (IARGS(1).EQ.IONE) RETURN
      N = IARGS(1) - IONE
      DO 180 J=1,NRMAX
        IJK  = LOCA + J
        IJ   = LOCB + J
        A(1) = RONE
        A(2) = RC(IJK-1)
        A(3) = RC(IJ-1)
        A(4) = RONE
        A(5) = RTWO
        DO 170 I=1,N
          IARGS(4) = IARGS(3) + I
          CALL ADRESS (IFOUR,LOCC)
C
          GO TO (100,110,120,130,140,150), L2
C
C     L2    = 1      NLSUB          NORMALIZED LAGUERRE POLYNOMIALS
C             RECURSION FORMULA  L(N+1) =(1.+2.*N-X)*L(N)-N**2 *L(N-1)
C             L(0) = 1.
C             L(1) = -X+1.
C             L(2) = X**2 - 4.0*X +2.
C             L(3) =-X**3 + 9.0*X**2-18.0*X+6.
C
C     L(N) = EXP(X) * (DN/DXN(X**N*EXP(-X)))
C
 100      A(4) = I
          A(6) = RONE + RTWO * A(4)
          A(7) = A(4) * A(4)
          A(8) = (A(6)-A(2)) * A(3) - A(7) * A(1)
          GO TO 160
C
C     L2    = 2      LSUB           LAGUERRE POLYNOMIALS
C             RECURSION FORMULA  L(N+1)=(((2.*N+1)-X)*L(N)-N*L(N-1))/
C                                        (N+1)
C             L(0) =  1.
C             L(1) =  -X+1.
C             L(2) =  .5 (X**2 - 4.*X +2)
C             L(3) =  (-X**3 + 9.*X**2 - 18.* X +6.)/6.
C
C     * SEE  ABRAMOWITZ, M. AND STEGUN, I.A.,  HANDBOOK OF MATHEMATICAL
C            FUNCTIONS, NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C            SERIES 55, SUPERINTENDENT OF DOCUMENTS, U.S. GOVERNMENT
C            PRINTING OFFICE, WASHINGTON, D.C. 20402.
C
C     * SEE  HILSENRATH,ZIEGLER,MESSINA,WALSH,HERBOLD,, OMNITAB, NBS
C            HANDBOOK 101 (MARCH 4, 1966) -  FOR FORMULAE USED.
C
 110      A(4) = I
          A(6) = A(4) + RONE
          A(7) = A(4) + A(6)
          A(8) = FDIV ((A(7)-A(2))*A(3)-A(4)*A(1),A(6),IND)
          GO TO 160
C
C     L2    = 3      HSUB           HERMITE POLYNOMIALS
C             RECURSION FORMULA  H(N+1) = 2.0*X*H(N)-2.0*N*H(N-1)
C
C             H(0) = 1.
C             H(1) = RTWO*X
C             H(2) = RFOR*X**2-RTWO
C             H(3) = 8.0*X**3-12.*X
C
 120      A(8) = RTWO * (A(2)*A(3) - A(4)*A(1))
          A(4) = A(4) + RONE
          GO TO 160
C
C     L2    = 4      USUB           CHEBYSHEV POLYNOMIALS
C
C             RECURSION FORMULA  U(N) = 2.0*X*U(N-1)-U(N-2)
C
C             U(0) = 1.
C             U(1) = 2.0*X
C             U(2) = 4.0*X**2-1.0
C             U(3) = 8.0*X**3-RFOR*X
C
 130      A(8) = RTWO * A(2) * A(3) - A(1)
          GO TO 160
C
C     L2    = 5      PSUB           LEGENDRE POLYNOMIALS
C
C             RECUSION FORMULA  P(N+1) =X*P(N)+(N/N+1)*(X*P(N)-P(N-1))
C
C             P(0) = 1.
C             P(1) = X.
C             P(2) = (3./2.)*X**2-(1./2.)
C             P(3) = 2.5*X**3-1.5*X
C
 140      A(6) = FDIV (A(4),A(5),IND)
          A(8) = (RONE+A(6)) * A(2) * A(3) - A(6) * A(1)
          A(4) = A(5)
          A(5) = A(5) + RONE
          GO TO 160
C
C     L2    = 6      TSUB           CHEBYSHEV POLYNOMIALS
C
C             RECURSION FORMULA.
C
C             T(0) = 1.
C             T(1) = X
C             T(2) = 2.*X**2-1.
C             T(3) = RFOR*X**3-RTHRE*X
C
 150      A(8) = RTWO * A(2) * A(3) - A(1)
 160      LJMN = LOCC + J
          RC(LJMN-1) = A(8)
          A(1) = A(3)
          A(3) = A(8)
 170    CONTINUE
 180  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
