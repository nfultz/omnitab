*ABRIDG
      SUBROUTINE ABRIDG       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. ABRIDG V 7.00  5/ 7/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     THE INSTRUCTION ABRIDGE MAY BE USED IN THE FOLLOWING WAYS ...   
C         
C     ABRIDGE  ROW (R) OF COLUMNS (C), (C), ..., (C)        
C     ABRIDGE // ROW (R) OF COLUMNS (C), (C, ... (C)        
C        (USE SPECIFIED FORMAT.)        
C         
C     USE RPRINT UNLESS IOSWT HAS BEEN SET BY FIXED OR FLOATING.      
C     ABRIDGE  WITH FLOATING PT. ARGS USES RPRINT. IOSWT IS NOT RESET.
C         
C                  ORIGINAL VERSION -   
C                   CURRENT VERSION -   APRIL, 1991.
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
C
      CHARACTER MARG*1
      CHARACTER LFMTP*80      
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER LA*1
C
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA NWIDTH / 15 /      
C         
C     ==================================================================        
C         
      IF (NARGS.NE.IZERO) GO TO 20      
  10  CALL ERROR (205)        
      RETURN        
C         
C     ..................................................................        
C         
  20  IF (L2.EQ.IONE .AND. IOSWT.LT.IONE) GO TO 70
      IF (L2.EQ.IONE .AND. IOSWT.EQ.IONE) GO TO 80
      IF (L2.EQ.IONE .AND. IOSWT.GT.IONE) GO TO 100         
      CALL PREPAK (ITWO,L2,L1,LFMT,MARG,LFMTP,IND)     
      IF (IND.NE.IZERO) GO TO 90        
      IP = IONE     
      IF (NARGS.LE.IONE) GO TO 10       
  30  LL = IARGS(1) 
      IARGS(1) = IONE         
      IF (LL.LE.IZERO .OR. LL.GT.NROW) GO TO 10   
      CALL CHKCOL   
      IF (NERROR.NE.IZERO) RETURN       
      DO 40 I=2,NARGS         
        J = IARGS(I) + LL     
        ARGS(I) = RC(J-1)     
  40  CONTINUE      
C         
      IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)       
      GO TO (50,60,110), IP   
  50  WRITE (IPRINT,LFMTP) (ARGS(I),I=2,NARGS)    
      RETURN        
C         
C     ..................................................................        
C         
  60  WRITE (IPRINT,IFMTX) (ARGS(I),I=2,NARGS)    
      RETURN        
C         
C     ..................................................................        
C         
  70  IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)       
      CALL RPRINT   
      RETURN        
C         
C     ..................................................................        
C         
C     USE SPECIFIED FIXED OR FLOATING FORMAT.     
C         
  80  IP = ITWO     
      GO TO 30      
  90  CALL ERROR (222)        
      GO TO 70      
 100  IP = ITHRE    
      GO TO 30      
C         
C     ABRIDGE FOR FIXED 0.    
C         
 110  IA = ITWO     
      IB = NARGS    
      IF (IB-IONE.GT.INUM) IB = IA + INUM - IONE  
 120  DO 130 I=1,120
        MARG(I) = LA(45)      
 130  CONTINUE      
C         
      IBA = IONE    
      DO 140 I=IA,IB
        CALL RFORMT (9,ISIGD,RC,ARGS(I),0,0,NWIDTH,IZERO,MARG(IBA),IRF)         
        IBA = IBA + 15        
 140  CONTINUE      
C         
      IBA = IBA - IONE        
      WRITE (IPRINT,150) (MARG(I),I=1,IBA)        
      IF (IB.GE.NARGS) RETURN 
      IA = IB + IONE
      IB = IB + INUM
      IF (IB.GT.NARGS) IB = NARGS       
      GO TO 120     
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 150  FORMAT (120A1)
C         
C     ==================================================================        
C         
      END   
*ALLSUB
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
*APRINT
      SUBROUTINE APRINT
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. APRINT V 7.00  4/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L1 = 4    APRINT
C     L1 = 7    MPRINT
C
C     MPRINT PRINTS ROW/COL TITLE, APRINT DOES NOT.
C
C     ALL NUMBERS READABLE IF POSSIBLE, OTHERWISE ALL FLOATING.
C
C     THE FORMAT STATEMENTS MAY NEED TO BE MODIFIED.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
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
      DIMENSION IFRV(9), LH(12), LHH(12),N(120), LIFMTX(23)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12)
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
C     ...................................................................
C
      CHARACTER LA*1
      CHARACTER N*1
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1
      CHARACTER LFMTP*80, LIFMTX*1, IFRV*1
      CHARACTER LHH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IFRV(1), IFRV(2), IFRV(3), IFRV(4), IFRV(5), IFRV(6),
     *IFRV(7), IFRV(8), IFRV(9)
     * /'1','X',',','I','5',',','2','X',','/
C
      DATA NWC /     15 /
C
      DATA ICA / 100000 /
C
C     ==================================================================
C
      IF (NARGS.EQ.IFOUR) GO TO 20
  10  CALL ERROR (205)
      RETURN
C
C     ..................................................................
C
  20  J = IONE
      I = IFOUR
      MCOL = INUM
      IF (L1.EQ.7 .AND. IPLACE*INUM+8.GT.LWIDE) MCOL = MCOL - IONE
      CALL CKIND (I)
      IF (I.NE.IZERO) GO TO 10
      KARGI = IARGS(1)
      CALL MTXCHM (J)
      IF (J.NE.IZERO) GO TO 10
      IF (NERROR.NE.IZERO) RETURN
C
C     CHECK TO SEE IF NPAGE=0. IF YES, BEGIN A NEW PAGE.  CALL PAGE(0)
C
      IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)
      IF (ILABEL.EQ.IZERO) GO TO 40
      IL = ((IARGS(2)-IONE) * NROW+KARGI) * ICA + IARGS(3)
      LOC = IARGS(4)
      CALL PREPAK (ITHRE,LOC,IL,LH,LHH,LFMTP,IND)
      IF (IND.NE.IZERO) GO TO 40
      IF (L1.EQ.IFOUR) GO TO 30
      WRITE (IPRINT,420) (LHH(I),I=1,12)
      GO TO 40
  30  WRITE (IPRINT,430) (LHH(I),I=1,12)
C
C     IF L2=1 IOSWT=0 USE READABLE FORMAT.
C
  40  IF (L2.EQ.IONE .AND. IOSWT.EQ.IZERO) GO TO 250
C
C     IF L2=1 IOSWT=1 USE FIXED OR FLOATING.
C
      IF (L2.EQ.IONE .AND. IOSWT.GE.IONE) GO TO 70
C
C     IF (L2.NE.1) USE SPECIFIED FORMAT.
C        FORMAT SHOULD SPECIFY FORMAT FOR ONLY ONE ROW.
C
      CALL PREPAK (ITWO,L2,L1,LFMT,LHH,LFMTP,IND)
      IF (IND.NE.IZERO) GO TO 60
      IA = IARGS(3)
      J1 = IARGS(1)
      J2 = J1 + (IARGS(4)-IONE) * NROW
      DO 50 I=1,IA
        WRITE (IPRINT,LFMTP) (RC(J),J=J1,J2,NROW)
        J1 = J1 + IONE
        J2 = J2 + IONE
  50  CONTINUE
      RETURN
C
C     ..................................................................
C
C     NO FORMAT IS FOUND SO USE READABLE FORMAT.
C
  60  CALL ERROR (222)
      GO TO 250
C
C     FIXED OR FLOATING FORMAT USED.
C
  70  DO 80 I=1,23
        LIFMTX(I) = ' '
  80  CONTINUE
C
      IF (L1.EQ.7) GO TO 100
      DO 90 I=1,12
        LIFMTX(I) = IFMTX(I)
  90  CONTINUE
C
      GO TO 120
 100  LIFMTX(1) = IFMTX(1)
      DO 105 I=1,9
        LIFMTX(I+1) = IFRV(I)
 105  CONTINUE
      DO 110 II=2,12
        LIFMTX(II+9) = IFMTX(II)
 110  CONTINUE
C
 120  KA   = IARGS(1)
      LL   = IARGS(3)
      IBB  = IARGS(4)
      IBBP = MCOL
      I1A  = IARGS(2)
 130  IF (IBB.GT.IBBP) GO TO 140
      IB   = IBB
      IBB  = IZERO
      GO TO 150
 140  IBB  = IBB - IBBP
      IB   = IBBP
 150  KB   = (IB-IONE) * NROW + KA
      KBP  = KB + NROW
      I2A  = I1A + IB - IONE
      MRV  = IONE
      IF (L1.EQ.IFOUR) GO TO 160
      WRITE (IPRINT,360) LA(28), LA(25), LA(33), LA(37), LA(13), LA(25),
     1     LA(22), (JJ,JJ=I1A,I2A)
      MRV  = KARGI
 160  DO 240 M=1,LL
        IF (IOSWT.NE.ITWO) GO TO 200
        DO 170 K=1,120
          N(K) = LA(45)
 170    CONTINUE
C
        IXY = IONE
        DO 180 K=KA,KB,NROW
          CALL RFORMT (9,ISIGD,A,RC(K),0,0,NWC,IZERO,N(IXY),IRF)
          IXY = IXY + 15
 180    CONTINUE
C
        IXY = IXY - IONE
        IF (L1.EQ.IFOUR) GO TO 190
        WRITE (IPRINT,340) MRV, (N(K),K=1,IXY)
        GO TO 210
 190    WRITE (IPRINT,350) (N(K),K=1,IXY)
        GO TO 230
 200    IF (L1.EQ.IFOUR) GO TO 220
        WRITE (IPRINT,LIFMTX) MRV, (RC(K),K=KA,KB,NROW)
 210    MRV = MRV + IONE
        GO TO 230
 220    WRITE (IPRINT,LIFMTX) (RC(K),K=KA,KB,NROW)
 230    KA = KA + IONE
        KB = KB + IONE
 240  CONTINUE
C
      IF (IBB.EQ.IZERO) RETURN
      WRITE (IPRINT,410)
C
C     PRINT NEXT SET OF COLUMNS.
C
      KA = KBP
      I1A = I2A + IONE
      GO TO 130
C
C     2 CALLS TO RFORMT LATER NEED TO BE CHANGED IF NO. OF SD NOT 8.
C
 250  NWMX = IPLACE - ITWO
      NSTART = IARGS(1)
      KSTART = KARGI - IONE
      KR = IARGS(3)
      KC = IARGS(4)
      K1 = IONE
      K2 = NSTART
      DO 270 I=1,KC
        DO 260 J=1,KR
          A(K1) = RC(K2)
          K1 = K1 + IONE
          K2 = K2 + IONE
 260    CONTINUE
        K2 = K2 + NROW - KR
 270  CONTINUE
      KSIZE = KR * KC
      CALL RFORMT (0,ISIGD,A,RC(1),KSIZE,NWMX+1,NWIDTH,NDECS,N(1),IRF)
C
C     MINIMUM OF TWO BLANK SPACES ON LEFT.
C
      NBLANK = NWMX + ITWO - NWIDTH
      I1 = IONE
      I1A = IARGS(2)
      K1 = NSTART - IONE
C
C     LOOP ON BLOCKS.
C
 280  I2 = I1 + MCOL - IONE
      I2 = MIN0(I2,KC)
      I2A = IARGS(2) + I2 - IONE
      K2 = K1 + (I2-I1) * NROW
      K4 = K2
      IF (L1.EQ.IFOUR) GO TO 290
      WRITE (IPRINT,360) LA(28), LA(25), LA(33), LA(37), LA(13), LA(25),
     1      LA(22), (JJ,JJ=I1A,I2A)
C
C     LOOP ON ROWS.
C
 290  DO 330 JJ=1,KR
        K1 = K1 + IONE
        K2 = K2 + IONE
        JJJ = KSTART + JJ
        IF (NWIDTH.LE.NWMX .OR. L1.NE.7) GO TO 300
C
C       WRITE FLOATING IF MPRINT (L1=7) AND NWIDTH GT NWMAX.
C
        WRITE (IPRINT,380) JJJ, (RC(K3),K3=K1,K2,NROW)
        GO TO 330
 300    LL = IONE
        K = K1
C
C       LOOP ON COLUMNS.
C
        DO 310 II=I1,I2
          CALL RFORMT (1,ISIGD,A,RC(K),NBLANK,0,NWIDTH,NDECS,N(LL),IRF)
          K = K + NROW
          LL = LL + NWMX+ITWO
 310    CONTINUE
C
        NL = LL - IONE
        IF (L1.EQ.7) GO TO 320
        WRITE (IPRINT,390) (N(LL),LL=2,NL)
        GO TO 330
 320    WRITE (IPRINT,370) JJJ, (N(LL),LL=2,NL)
 330  CONTINUE
C
      K1 = K4 + NROW
      I1 = I1 + MCOL
      I1A = I1A + MCOL
      IF (I2.GE.KC) RETURN
C
C     LOOP ON BLOCKS.
C        PUT IN BLANK LINE BETWEEN BLOCKS.
C
      WRITE (IPRINT,400)
      GO TO 280
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 340  FORMAT (1X,I5,2X,112A1)
 350  FORMAT (120A1)
 360  FORMAT (1X,7A1,7(6X,I5,4X))
 370  FORMAT (1X,I5,2X,112A1)
 380  FORMAT (1X,I5,2X,1P7E15.6)
 390  FORMAT (1X,119A1)
 400  FORMAT (1H )
 410  FORMAT (1X)
 420  FORMAT (/14H  MATRIX NAME ,12A1)
 430  FORMAT (/14H  ARRAY  NAME ,12A1)
C
C     ==================================================================
C
      END
*ARITH
      SUBROUTINE ARITH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  ARITH V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE ADD, SUB, MULT, DIV AND RAISE INSTRUCTIONS WITH
C        THREE AND FIVE ARGUMENTS.
C
C         L2 = 1,     ADD
C         L2 = 2,     SUBTRACT
C         L2 = 3,     MULTIPLY
C         L2 = 4,     DIVIDE
C         L2 = 5,     RAISE
C         L2 = 6,     ACCURATE DIGITS
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION II(5), KK(5)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      REAL             X, X1, X2, X3, X4
      REAL             FDIV, FEXP2
C
C
C
C     ==================================================================
C
      IF (NARGS.LT.ITHRE .OR. NARGS.GT.IFIVE) CALL ERROR (10)
      IF (NARGS.EQ.IFOUR) CALL ERROR (29)
      IF (KIND(NARGS).NE.IZERO) CALL ERROR (20)
      IF (L2.NE.6) GO TO 10
      IF (NARGS.EQ.ITHRE) GO TO 10
      CALL ERROR (212)
      NARGS = ITHRE
  10  DO 20 J=1,NARGS
        I = J
        KK(I) = IONE
        CALL ADRESS (I,II(I))
        IF (II(I).GE.IZERO) GO TO 20
        KK(I) = IZERO
        II(I) = I
  20  CONTINUE
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      JJ = II(NARGS) + NRMAX - IONE
      IF (NARGS.NE.ITHRE) GO TO 120
      I1 = II(1)
      I2 = II(2)
      I3 = II(3)
      DO 110 I=I3,JJ
        X1 = RC(I1)
        IF (KK(IONE).EQ.IZERO) X1 = ARGS(1)
        X2 = RC(I2)
        IF (KK(ITWO).EQ.IZERO) X2 = ARGS(ITWO)
        GO TO (30,40,50,60,80,90), L2
C
  30    RC(I) = X1 + X2
        GO TO 100
C
  40    RC(I) = X2 - X1
        GO TO 100
C
  50    RC(I) = X1 * X2
        GO TO 100
C
  60    IF (X2.NE.RZERO) GO TO 70
        RC(I) = RZERO
        CALL ERROR (106)
        GO TO 100
C
  70    RC(I) = FDIV (X1,X2,IND)
        GO TO 100
C
  80    RC(I) = FEXP2 (X1,X2)
        GO TO 100
C
  90    CALL ACCDIG (X1,X2,RSD,RC(I),IND)
C
 100    I1 = I1 + KK(1)
        I2 = I2 + KK(2)
 110  CONTINUE
      RETURN
C
C     ..................................................................
C
 120  IF (NARGS.EQ.IFIVE) GO TO 130
      II(5) = II(4)
      KK(5) = KK(4)
 130  I1    = II(1)
      I2    = II(2)
      I3    = II(3)
      I4    = II(4)
      I5    = II(5)
      DO 210 I=I5,JJ
        X1 = RC(I1)
        IF (KK(1).EQ.IZERO) X1 = ARGS(1)
        X2 = RC(I2)
        IF (KK(2).EQ.IZERO) X2 = ARGS(2)
        X3 = RC(I3)
        IF (KK(3).EQ.IZERO) X3 = ARGS(3)
        X4 = RC(I4)
        IF (KK(4).EQ.IZERO) X4 = ARGS(4)
        GO TO (140,150,160,170,190), L2
C
 140    X = X1 + X2
        GO TO 200
C
 150    X = X2 - X1
        GO TO 200
C
 160    X = X1 * X2
        GO TO 200
C
 170    IF (X2.NE.RZERO) GO TO 180
        X = RZERO
        CALL ERROR (106)
        GO TO 200
C
 180    X = FDIV (X1,X2,IND)
        GO TO 200
C
 190    X = FEXP2 (X1,X2)
C
 200    RC(I) = X * X3 + X4
        I1 = I1 + KK(1)
        I2 = I2 + KK(2)
        I3 = I3 + KK(3)
        I4 = I4 + KK(4)
 210  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*ARYVEC
      SUBROUTINE ARYVEC
C
C  *  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ARYVEC V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     MULTIPLY MATRIX BY VECTOR OR VECTOR TRANSPOSE BY MATRIX.
C
C     L2 = 1,     MULTIPLY MATRIX BY VECTOR
C          GENERAL FORM OF INSTRUCTION ...
C              M(AV)  A (,) N,K   VECTOR IN COL I  STORE IN COLUMN J
C              M(AV)  A (,) N,K   VECTOR IN COL I  STORE IN ROW K COL J
C                   N AND K MUST BE SPECIFIED
C
C     L2 = 2,     MULTIPLY VECTOR TRANSPOSE BY MATRIX
C          GENERAL FORM OF INSTRUCTION ...
C              M(V'A) A (,) N,K  VECTOR IN COL I  STORE IN ROW  J
C              M(V'A) A (,) N,K  VECTOR IN COL I  STORE IN ROW K COL J
C                   N AND K MUST BE SPECIFIED
C                   IF ONLY ROW IS GIVEN FOR STORAGE,  COL  1 IS ASSUMED
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
      REAL             FDPCON
C
       INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DX(1)
      DOUBLE PRECISION DSUM
C
C     ==================================================================
C
C     CHECK FOR CORRECT NUMBER OF ARGUMENTS.
C
      IF (NARGS.NE.6 .AND. NARGS.NE.7) CALL ERROR (10)
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) CALL ERROR (3)
C
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE.
C
      IF (L2.EQ.ITWO) GO TO 10
      GO TO 20
 10   IF (NARGS.NE.6) GO TO 20
      IF (IARGS(6).GT.NROW .OR. IARGS(4).GT.NCOL) CALL ERROR (17)
C
C     COMPUTE ADDRESSES OF COLUMNS.
C
  20  IARGS(10) = IARGS(NARGS)
      IARGS(8)  = IONE
      IROWSV    = IONE
      IF (L2.EQ.IONE) GO TO 50
      IF (NARGS.EQ.7) GO TO 30
      J = ITWO
      IROWSV = IARGS(6)
      GO TO 40
  30  IARGS(12) = IARGS(4)
      IARGS(11) = IONE
      IARGS( 9) = IARGS(6)
      J = ITHRE
  40  IARGS(7) = IARGS(3)
      GO TO 70
  50  J = ITHRE
      IARGS(12) = IONE
      IARGS(11) = IARGS(3)
      IARGS( 7) = IARGS(4)
      IF (NARGS.EQ.6) GO TO 60
      IARGS( 9) = IARGS(6)
      GO TO 70
  60  IARGS( 9) = IONE
  70  IARGS( 6) = IARGS(5)
      IARGS( 5) = IONE
      CALL MTXCHK (J)
      IF (J.GT.IONE) GO TO 80
      IF (J.LT.IONE) GO TO 90
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  80  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
C     CHECK FOR PREVIOUS ERRORS.
C
  90  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (L2.EQ.ITWO) GO TO 100
      ICS  = IARGS(9)
      IAP  = IARGS(1)
      IP   = IARGS(3)
      JP   = IARGS(4)
      IAD1 = NROW
      IAD2 = IONE
      IBP  = IARGS(5)
      GO TO 130
C
 100  IBP  = IARGS(1)
      IAP  = IARGS(5)
      IP   = IARGS(4)
      IF (NARGS.EQ.7) GO TO 110
      JP   = IARGS(3)
      ICS  = IROWSV
      GO TO 120
C
 110  JP   = IARGS(3)
      ICS  = IARGS(9)
 120  IAD1 = IONE
      IAD2 = NROW
 130  IC   = IONE
      DO 160 I=1,IP
        IA = IAP
        IB = IBP
        CALL DSUMAL (DX(1),0,DSUM)
        DO 140 J=1,JP
          DX(1) = DBLE (RC(IA)) * DBLE (RC(IB))
          IA = IA + IAD1
          IB = IB + IONE
          CALL DSUMAL (DX(1),-1,DSUM)
 140    CONTINUE
        CALL DSUMAL (DX(1),1,DSUM)
        A(IC) = FDPCON(DSUM)
        IC = IC + IONE
        IF (L2.EQ.ITWO) GO TO 150
        IAP = IAP + IONE
        GO TO 160
 150    IBP = IBP + NROW
 160  CONTINUE
C
C     STORE RESULTS IN WORKSHEET.
C
      IS = IONE
      DO 170 I=1,IP
        RC(ICS) = A(IS)
        IS = IS + IONE
        ICS = ICS + IAD2
 170  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*ATOMIC
      SUBROUTINE ATOMIC
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ATOMIC V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE ATOMIC INSTRUCTION OR MOLWT INSTRUCTION.
C
C     FORMS OF INSTRUCTIONS ARE ...
C
C     ATOMIC MASSES PUT IN COLUMN (C)
C     MOLWT Z=(K) AMOUNT=(K) Z=(K) AMOUNT=(K) ... STORE SUM IN COL (C)
C
C     SEE USER'S MANUAL FOR DETAILS ON STORAGE.
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
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             ATWT(103)
      REAL             WT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     ATOMIC WEIGHTS ADOPTED BY IUPAC IN 1971, IMPLEMENTED JUNE 2,1972.
C
      DATA ATWT(  1), ATWT(  2), ATWT(  3), ATWT(  4), ATWT(  5) /
     1       1.00790,   4.00260,   6.94100,   9.01218,  10.81000 /
      DATA ATWT(  6), ATWT(  7), ATWT(  8), ATWT(  9), ATWT( 10) /
     1      12.01100,  14.00670,  15.99940,  18.99840,  20.17900 /
      DATA ATWT( 11), ATWT( 12), ATWT( 13), ATWT( 14), ATWT( 15) /
     1      22.98977,  24.30500,  26.98154,  28.08600,  30.97376 /
      DATA ATWT( 16), ATWT( 17), ATWT( 18), ATWT( 19), ATWT( 20) /
     1      32.06000,  35.45300,  39.48000,  39.09800,  40.08000 /
      DATA ATWT( 21), ATWT( 22), ATWT( 23), ATWT( 24), ATWT( 25) /
     1      44.95590,  47.90000,  50.94140,  51.99600,  54.93800 /
      DATA ATWT( 26), ATWT( 27), ATWT( 28), ATWT( 29), ATWT( 30) /
     1      55.84700,  58.93320,  58.71000,  63.54600,  65.38000 /
      DATA ATWT( 31), ATWT( 32), ATWT( 33), ATWT( 34), ATWT( 35) /
     1      69.72000,  72.59000,  74.92160,  78.96000,  79.90400 /
      DATA ATWT( 36), ATWT( 37), ATWT( 38), ATWT( 39), ATWT( 40) /
     1      83.80000,  85.46780,  87.62000,  88.90590,  91.22000 /
      DATA ATWT( 41), ATWT( 42), ATWT( 43), ATWT( 44), ATWT( 45) /
     1      92.90640,  95.94000,  98.90620, 101.07000, 102.90550 /
      DATA ATWT( 46), ATWT( 47), ATWT( 48), ATWT( 49), ATWT( 50) /
     1     106.40000, 107.86800, 112.40000, 114.82000, 118.69000 /
      DATA ATWT( 51), ATWT( 52), ATWT( 53), ATWT( 54), ATWT( 55) /
     1     121.75000, 127.60000, 126.90450, 131.30000, 132.90540 /
      DATA ATWT( 56), ATWT( 57), ATWT( 58), ATWT( 59), ATWT( 60) /
     1     137.34000, 138.90550, 140.12000, 140.90770, 144.24000 /
      DATA ATWT( 61), ATWT( 62), ATWT( 63), ATWT( 64), ATWT( 65) /
     1     147.00000, 150.40000, 151.96000, 157.20000, 158.92540 /
      DATA ATWT( 66), ATWT( 67), ATWT( 68), ATWT( 69), ATWT( 70) /
     1     162.50000, 164.93040, 167.26000, 168.93420, 173.04000 /
      DATA ATWT( 71), ATWT( 72), ATWT( 73), ATWT( 74), ATWT( 75) /
     1     174.97000, 178.49000, 180.94790, 183.85000, 186.20000 /
      DATA ATWT( 76), ATWT( 77), ATWT( 78), ATWT( 79), ATWT( 80) /
     1     190.20000, 192.22000, 195.09000, 196.96650, 200.59000 /
      DATA ATWT( 81), ATWT( 82), ATWT( 83), ATWT( 84), ATWT( 85) /
     1     204.37000, 207.20000, 208.98040, 210.00000, 210.00000 /
      DATA ATWT( 86), ATWT( 87), ATWT( 88), ATWT( 89), ATWT( 90) /
     1     222.00000, 223.00000, 226.02540, 227.02000, 232.03810 /
      DATA ATWT( 91), ATWT( 92), ATWT( 93), ATWT( 94), ATWT( 95) /
     1     231.03590, 238.02900, 237.04820, 239.00000, 243.00000 /
      DATA ATWT( 96), ATWT( 97), ATWT( 98), ATWT( 99), ATWT(100) /
     1     247.00000, 247.00000, 249.00000, 254.00000, 253.00000 /
      DATA ATWT(101), ATWT(102), ATWT(103) /
     1     255.00000, 257.00000, 255.00000 /
C
      DATA ICA / 103 /
C
C     ==================================================================
C
      IF (L2.EQ.IFOUR) GO TO 30
C
C     THIS IS ATOMIC.
C
      IF (NARGS.GE.IONE) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.GT.IONE) CALL ERROR (221)
      IF (NARGS.LT.IONE) GO TO 60
C
      CALL ADRESS (IONE,I1)
      IF (I1.LT.IZERO) CALL ERROR (20)
      IF (NROW.LT.ICA) CALL ERROR (226)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      L = ICA
      IF (NROW.LT.ICA) L = NROW
      DO 20 J=1,L
        II1 = I1 + J - IONE
        RC(II1) = ATWT(J)
  20  CONTINUE
      NROLD = NRMAX
      IF (NRMAX.LT.L) NRMAX = L
      IF (NROLD.NE.NRMAX) CALL ERROR (252)
      RETURN
C
C     ..................................................................
C
C     THIS IS MOLWT.
C
  30  I = NARGS
      CALL CKIND (I)
      IF (I.GE.IONE) CALL ERROR (20)
      N = IDIV (NARGS,ITWO,IND)
      IF (NARGS.EQ.ITWO*N) CALL ERROR (10)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      CALL ADRESS (NARGS,I)
      IF (I.LT.IZERO) CALL ERROR (20)
      WT = RZERO
      IF (N.LT.IONE) CALL ERROR (10)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      DO 40 J=2,NARGS,2
        K = IARGS(J-1)
        IF (K.GT.ICA) CALL ERROR (20)
        IF (K.LE.IZERO) CALL ERROR (20)
        WT = WT + ATWT(K) * FLOAT(IARGS(J))
        IF (NERROR.NE.IZERO) RETURN
  40  CONTINUE
C
      DO 50 J=1,NRMAX
        II = I + J - IONE
        RC(II) = WT
  50  CONTINUE
      RETURN
C
C     ..................................................................
C
  60  CALL ERROR (10)
      RETURN
C
C     ==================================================================
C
      END
*BESEL1
      SUBROUTINE BESEL1
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BESEL1 V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE BESSEL FUNCTIONS ...
C
C     L2 =  1,       BJZERO  OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  2,       BJONE   OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  3,       BYZERO  OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  4,       BYONE   OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  5,       BIZERO  OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  6,       BIONE   OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  7,       BKZERO  OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  8,       BKONE   OF COLUMN (C) PUT IN COLUMN (C)
C     L2 =  9,       EXIZERO OF COLUMN (C) PUT IN COLUMN (C)
C     L2 = 10,       EXIONE  OF COLUMN (C) PUT IN COLUMN (C)
C     L2 = 11,       EXKZERO OF COLUMN (C) PUT IN COLUMN (C)
C     L2 = 12,       EXKONE  OF COLUMN (C) PUT IN COLUMN (C)
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
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DXEX, Y, X, XEX
      DOUBLE PRECISION DBEJ, FDDIV, FDEXP
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      XEX = REXP - RTHRE
      DXEX = DXEXP - DFOR
      IF (NARGS.GE.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  N = IZERO
      L = IDIV (L2,ITWO,IND)
      L = ITWO*L
      IF (L.EQ.L2) N = IONE
      IF (NARGS.GT.ITWO) CALL ERROR (10)
      CALL ADRESS (NARGS,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      LT = IONE
      IF (KIND(1).EQ.IONE) GO TO 20
      CALL ADRESS (1,JA)
      IF (JA.LT.IZERO) CALL ERROR (20)
      LT = ITWO
  20  M = IONE
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (L2.GT.ITWO) M = IFIVE
      IF (L2.GT.IFOUR) M = ITHRE
      IF (L2.GT.6) M = 7
      IF (L2.GT.8) M = ITHRE
      IF (L2.GT.ITEN) M = 7
      L = IZERO
      IF (L2.GT.IFOUR) L = IONE
      IF (L2.GT.8) L = ITWO
      IF (LT.EQ.IONE) GO TO 90
      DO 80 I=1,NRMAX
        X = RC(JA)
        JA = JA + IONE
        Y =  DONE
        IF (L.EQ.IZERO) GO TO 40
        IF (L.EQ.ITWO) GO TO 30
        IF (DABS(X).LT.XEX)  GO TO 40
C
C     IF  1X1  IS GREATER THAN XEXP AND LESS THAN DXEXP THE RESULTS
C        WILL BE SCALED BY EXP (X) OR EXP (-X).
C
C     IF  1X1 IS GREATER THAN DXEXP THE SUBROUTINE DBEJ DOES THE SCALING
C        AND A MESSAGE IS PRINTED.
C
C     THIS APPLIES TO THE COMMANDS BIZERO, BIONE, BKZERO, AND BKONE.
C
        CALL ERROR (105)
  30    IF (DABS(X).GT.DXEX)  GO TO 40
        Y = FDEXP(X)
        IF (M.EQ.ITHRE) Y = FDDIV (DONE,Y,IND)
  40    IF (M.EQ.IFIVE .OR. M.EQ.7) GO TO 50
        GO TO 60
 50     IF (X.GT.DZERO) GO TO 60
        RC(J) = RZERO
        CALL ERROR (101)
        GO TO 70
C
C     L2 =  1, M=1, N=0, LT=2, L=0   BJZERO  OF COL (C) PUT IN COL (C)
C     L2 =  2, M=1, N=1, LT=2, L=0   BJONE   OF COL (C) PUT IN COL (C)
C     L2 =  3, M=5, N=0, LT=2, L=0   BYZERO  OF COL (C) PUT IN COL (C)
C     L2 =  4, M=5, N=1, LT=2, L=0   BYONE   OF COL (C) PUT IN COL (C)
C     L2 =  5, M=3, N=0, LT=2, L=1   BIZERO  OF COL (C) PUT IN COL (C)
C     L2 =  6, M=3, N=1, LT=2, L=1   BIONE   OF COL (C) PUT IN COL (C)
C     L2 =  7, M=7, N=0, LT=2, L=1   BKZERO  OF COL (C) PUT IN COL (C)
C     L2 =  8, M=7, N=1, LT=2, L=1   BKONE   OF COL (C) PUT IN COL (C)
C     L2 =  9, M=3, N=0, LT=2, L=2   EXIZERO OF COL (C) PUT IN COL (C)
C     L2 = 10, M=3, N=1, LT=2, L=2   EXIONE  OF COL (C) PUT IN COL (C)
C     L2 = 11, M=7, N=0, LT=2, L=2   EXKZERO OF COL (C) PUT IN COL (C)
C     L2 = 12, M=7, N=1, LT=2, L=2   EXKONE  OF COL (C) PUT IN COL (C)
C
  60    RC(J) = FDPCON (Y*DBEJ(X,N,M))
  70    J = J + IONE
  80  CONTINUE
      RETURN
C
C     ..................................................................
C
  90  X = ARGS(1)
      Y = DONE
      IF (L.EQ.IZERO) GO TO 110
      IF (L.EQ.ITWO) GO TO 100
      IF (DABS(X).LT.XEX)  GO TO 110
C
C     SEE COMMENTS ABOVE ON BOUNDS OF X BEFORE DBEJ IS CALLED.
C
      CALL ERROR (105)
 100  IF (DABS(X).GT.DXEX)  GO TO 110
      Y = FDEXP (X)
      IF (M.EQ.ITHRE) Y = FDDIV (DONE,Y,IND)
C
C     L2 =  1, M=1, N=0, LT=1, L=0   BJZERO  OF (E) PUT IN COL (C)
C     L2 =  2, M=1, N=1  LT=1, L=0   BJONE   OF (E) PUT IN COL (C)
C     L2 =  3, M=5, N=0, LT=1, L=0   BYZERO  OF (E) PUT IN COL (C)
C     L2 =  4, M=5, N=1, LT=1, L=0   BYONE   OF (E) PUT IN COL (C)
C     L2 =  5, M=3, N=0, LT=1, L=1   BIZERO  OF (E) PUT IN COL (C)
C     L2 =  6, M=3, N=1, LT=1, L=1   BIONE   OF (E) PUT IN COL (C)
C     L2 =  6, M=3, N=1, LT=1, L=1   BIONE   OF (E) PUT IN COL (C)
C     L2 =  7, M=7, N=0, LT=1, L=1   BKZERO  OF (E) PUT IN COL (C)
C     L2 =  8, M=7, N=1, LT=1, L=1   BKONE   OF (E) PUT IN COL (C)
C     L2 =  9, M=3, N=0, LT=1, L=2   EXIZERO OF (E) PUT IN COL (C)
C     L2 = 10, M=3, N=1, LT=1, L=2   EXIONE  OF (E) PUT IN COL (C)
C     L2 = 11, M=7, N=0, LT=1, L=2   EXKZERO OF (E) PUT IN COL (C)
C     L2 = 12, M=7, N=1, LT=1, L=2   EXKONE  OF (E) PUT IN COL (C)
C
 110  XX = FDPCON (Y * DBEJ (X,N,M))
      DO 120 I=1,NRMAX
        RC(J) = XX
        J = J + IONE
 120  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*BESEL2
      SUBROUTINE BESEL2
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BESEL2 V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE BESSEL FUNCTIONS ...
C
C     L2 = 13,   KBIZERO OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 14,   KBIONE  OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 15,   KBKZERO OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 16,   KBKONE  OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C
C     L2 = 18,   KEXIONE OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 19,   KEXKZR  OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 20,   KEXKONE OF (E) PUT REAL IN COL (C) IMAGINARY IN COL (C)
C     L2 = 21,   CIZERO  OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 22,   CIONE   OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 23,   CKZERO  OF (E), OHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 24,   CKONE   OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 25,   CEIZERO OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 26,   CEIONE  OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 27,   CEKZERO OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C     L2 = 28,   CEKONE  OF (E), PHI (E) PUT REAL IN COL (C) IMAG IN (C)
C
C               WRITTEN BY -
C                      BRADLEY A. PEAVY,
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
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION E, P, Q, S, T, Y, X, XEX, Z
      DOUBLE PRECISION DPCA
      DOUBLE PRECISION FDCOS, FDDIV, FDEXP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / 0.785398163397D0 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      XEX = REXP - RTHRE
      IF (NARGS.GE.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  N = IZERO
      L = IDIV (L2,ITWO,IND)
      L = ITWO*L
      IF (L.EQ.L2) N = IONE
      JB = IONE
      IF (L2.GT.20) GO TO 110
      IF (NARGS.GT.ITHRE) CALL ERROR (10)
      M = IONE
      IF (L2.GT.14) M = ITWO
      IF (L2.GT.16) M = IONE
      IF (L2.GT.18) M = ITWO
      L = IZERO
      IF (L2.GT.16) L = IONE
      Y =  DPCA
      LV = IZERO
      JX = IZERO
C
  20  CALL ADRESS (NARGS,J2)
      IF (J2.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (NARGS-IONE,J1)
      IF (J1.LT.IZERO) CALL ERROR (20)
      LT = IZERO
      IF (KIND(1).EQ.IONE) GO TO 30
      CALL ADRESS (1,JA)
      IF (JA.LT.IZERO) CALL ERROR (20)
      LT = IONE
C
  30  CONTINUE
      KA = IZERO
      IF (LT+LV.EQ.IZERO) GO TO 100
      IF (LV.EQ.IZERO) GO TO 130
      IF (LT.EQ.IZERO) GO TO 140
  40  IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DO 90 I=1,NRMAX
        IF (KA.EQ.IZERO) X = RC(JA)
        JA = JA + IONE
        E = DONE
        IF (JX.NE.IZERO) Y = RC(JB)
        JB = JB + IONE
C
C     L2 = 15, M=2, N=0, L=0  KBKZERO OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 16, M=2, N=1, L=0  KBKONE  OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 19, M=2, N=0, L=1  KEXKZR  OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 20, M=2, N=1, L=1  KEXKONE OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 23, M=2, N=0, L=0  CKZERO  OF (E) OHI (E) PUT REAL (C) IMAG (C)
C     L2 = 24, M=2, N=1, L=0  CKONE   OF (E) PHI (E) PUT REAL (C) IMAG (C)
C     L2 = 27, M=2, N=0, L=1  CEKZERO OF (E) PHI (E) PUT REAL (C) IMAG (C)
C     L2 = 28, M=2, N=1, L=1  CEKONE  OF (E) PHI (E) PUT REAL (C) IMAG (C)
C
        IF (M.EQ.ITWO) CALL CBEK (X,Y,P,Q,S,T)
C
C     L2 = 13, M=1, N=0, L=0  KBIZERO OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 14, M=1, N=1, L=0  KBIONE  OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 18, M=1, N=1, L=1  KEXIONE OF (E) PUT REAL IN (C) IMAGINARY (C)
C     L2 = 21, M=1, N=0, L=0  CIZERO  OF (E) PHI (E) PUT REAL (C) IMAG (C)
C     L2 = 22, M=1, N=1, L=0  CIONE   OF (E) PHI (E) PUT REAL (C) IMAG (C)
C     L2 = 25, M=1, N=0, L=1  CEIZERO OF (E) PHI (E) PUT REAL (C) IMAG (C)
C     L2 = 26, M=1, N=1, L=1  CEIONE  OF (E) PHI (E) PUT REAL (C) IMAG (C)
C
        IF (M.EQ.IONE) CALL CBEI (X,Y,P,Q,S,T)
        Z = X * FDCOS(Y)
        IF (L.EQ.IONE) GO TO 50
        IF (DABS(Z).LT.XEX)  GO TO 60
        CALL ERROR (105)
  50    E = FDEXP (Z)
        IF (M.EQ.IONE) E = FDDIV (DONE,E,IND)
  60    IF (N.EQ.IZERO) GO TO 70
C
C     STORE INTO WORK SHEET RESULTS OF COMMANDS KBIONE, KBKONE
C     KEXIONE, KEXKONE, CIONE, CEIONE, CEKONE
C
        RC(J1) = E * S
        RC(J2) = E * T
        GO TO 80
C
C     STORE INTO WORK SHEET RESULTS OF COMMANDS KBIZERO, KBKZERO,
C     KEXIZER, KEXKZER, CIZERO, CEIZERO, CEKZERO
C
  70    RC(J1) = E * P
        RC(J2) = E * Q
  80    J1 = J1 + IONE
        J2 = J2 + IONE
  90  CONTINUE
      RETURN
C
C     ..................................................................
C
 100  IF (JX.EQ.IZERO) GO TO 140
      Y = ARGS(2)
      X = ARGS(1)
      KA = IONE
      JX = IZERO
      GO TO 40
C
 110  IF (NARGS.GT.IFOUR) CALL ERROR (10)
      JX = IONE
      LV = IZERO
      IF (KIND(2).EQ.IONE) GO TO 120
      CALL ADRESS (ITWO,JB)
      IF (JB.LT.IZERO) CALL ERROR (20)
      LV = IONE
C
 120  M = IONE
      IF (L2.GT.22) M = ITWO
      IF (L2.GT.24) M = IONE
      IF (L2.GT.26) M = ITWO
      L = IZERO
      IF (L2.GT.24) L = IONE
      GO TO 20
C
 130  IF (JX.EQ.IZERO) GO TO 40
      Y = ARGS(2)
      JX = IZERO
      GO TO 40
C
 140  KA = IONE
      X = ARGS(1)
      GO TO 40
C
C     ==================================================================
C
      END
*BESEL3
      SUBROUTINE BESEL3
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BESEL3 V 7.00  5/16/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE INSTRUCTIONS ...
C
C     L2 = 29,       INTJO
C     L2 = 32,       BESJN
C     L2 = 33,       ZEROS BJONE
C     L2 = 34,       ZEROS BJZERO
C     L2 = 38,       BESIN
C     L2 = 39,       BESKN
C
C               WRITTEN BY -
C                      BRADLEY A. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATOTY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1977.
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION X, Z
      DOUBLE PRECISION DAONE, DARES, DATWO
      DOUBLE PRECISION AA(1000), B(1000), W(102)
      DOUBLE PRECISION BINTJO, DBEJ, FDDIV
C
      EQUIVALENCE (W(1),A(1))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 1000 /
      DATA ICB /    7 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.GE.ITWO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  JA = IONE
      IF (L2.EQ.33 .OR. L2.EQ.34) GO TO 80
C
C     L2 = 29,       INTJO
C     L2 = 34,       ZEROS BJZERO
C     L2 = 38,       BESIN
C
      IF (NARGS.GT.ITWO) CALL ERROR (10)
      CALL ADRESS (NARGS,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      LT = IZERO
      IF (KIND(1).EQ.IONE) GO TO 20
      CALL ADRESS (IONE,JA)
      IF (JA.LT.IZERO) CALL ERROR (20)
      LT = IONE
C
  20  IF (LT.EQ.IONE .AND. L2.GT.29) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IF (LT.EQ.IZERO) X = ARGS(1)
      IF (L2.GT.29) GO TO 40
C
C     L2 = 29                  INTJO  OF (E) PUT IN COL (C)
C
      DO 30 N=1,NRMAX
        IF (LT.EQ.IONE) X = RC(JA)
        JA = JA + IONE
        RC(J) = BINTJO (X,W)
        J = J + IONE
  30  CONTINUE
      RETURN
C
C     ..................................................................
C
  40  Z = X
      K = NRMAX
      IF (K.LE.IHRD) GO TO 60
      K = IHRD
      JA = J + IHRD
      DO 50 I=K,NRMAX
        RC(JA) = RZERO
        JA = JA + IONE
  50  CONTINUE
C
C     L2 = 32                  BESJN X = (K) PUT IN COL (C)
C
  60  IF (L2.EQ.32) CALL BEJN (IZERO,W,Z)
C
C     L2 = 38                  BESIN X = (K) PUT IN COL (C)
C
      IF (L2.EQ.38) CALL BEJN (IONE,W,Z)
      IF (L2.EQ.39) GO TO 120
C
      DO 70 N=1,K
        RC(J) = W(N)
        J = J + IONE
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
C     L2 = 33,       ZEROS BJONE
C     L2 = 34,       ZEROS BJZERO
C
  80  L      = NRMAX
      IF (L.GT.ICA) L = ICA
      IF (NARGS.GT.ITWO) CALL ERROR (10)
      CALL ADRESS (NARGS,J)
      IF (J.LT.IZERO) CALL ERROR (20)
      IF (KIND(1).EQ.IONE) CALL ERROR (20)
      CALL ADRESS (IONE,JA)
      IF (JA.LT.IZERO) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
      IF (L2.EQ.33) GO TO 90
C
C     L2 = 34                  ZEROS BJZERO PUT IN COL (C) AND COL (C)
C
      CALL BEZONE (AA,B,IONE,L)
      GO TO 100
C
C     L2 = 33                  ZEROS BJONE  PUT IN COL (C) AND COL (C)
C
  90  CALL BEZERO (AA,B,IONE,L)
 100   DO 110 N=1,L
        RC(JA) = AA(N)
        RC(J)  = B(N)
        JA     = JA + IONE
        J      = J + IONE
 110  CONTINUE
      RETURN
C
C     ..................................................................
C
 120  IF (X.LT.REXP) GO TO 130
      CALL ERROR (225)
      RETURN
C
C     ..................................................................
C
C     L2 = 39                  BESKN X = (K) PUT IN COL  (C)
C
 130  DAONE = DBEJ (X,IZERO,ICB)
      DATWO = DBEJ (X, IONE,ICB)
      RC(J) = DAONE
      RC(J+1) = DATWO
      J = J + ITWO
      DO 140 I=3,K
        Z = I - ITWO
        DARES = DAONE + FDDIV (DTWO*Z*DATWO,X,IND)
        IF (DARES.GT.DBLE(RPIFY)) GO TO 150
        RC(J) = DARES
        DAONE = DATWO
        DATWO = DARES
        J = J + IONE
 140  CONTINUE
      RETURN
C
C     ..................................................................
C
 150  DO 160 JA = I,K
        RC(J) = RZERO
        J = J + IONE
 160  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*BESTCP
      SUBROUTINE BESTCP
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. BESTCP V 7.00  5/28/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE CP STATISTICS FOR ALL POSSIBLE REGRESSIONS.
C
C     FORM OF INSTRUCTION IS ...
C        BESTCP FOR Y IN (C) WTS (E) VECTORS (K) IN COLS (C),(C),...,(C)
C
C     ARGUMENT STRUCTURE IS THE SAME AS THAT FOR FIT WITHOUT STORAGE.
C
C     CURRENTLY, THE THIRD ARGUMENT IS REDUNDANT BECAUSE THERE IS NO
C        PROVISION FOR STORAGE.  HOWEVER, PROVISION FOR STORAGE MAY BE
C        ADDED LATER.
C
C     NUMBER OF INDEPENDENT VARIABLES, (K), MUST BE GREATER THAN 2 AND
C        LESS THAN 29.
C
C     THIS SUBPROGRAM CALLS A SET OF PROCEDURES GRACIOUSLY SUPPLIED
C        BY G. M. FURNIVAL AND R. W. WILSON, JR.
C
C     REFERENCE -
C        FURNIVAL, G. M. AND WILSON, R. W., JR. (1974).  REGRESSION
C           BY LEAPS AND BOUNDS.  TECHNOMETRICS,16,499-511.
C
C     FURNIVAL AND WILSON LIMIT NUMBER OF INDEPENDENT VARIABLES TO 40,
C        OMNITAB REQUIRES LIMITATION OF 28.
C
C     IF K = 28 AND M = K+1 = 29 ARE CHANGED,
C       CHANGE DIMENSION OF RXY AND SQRTCT AND CHANGE VALUE OF MAXVEC.
C       ALSO MAKE CHANGE IN LINE 41 OF PROCEDURE CRSPRD.
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
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
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
      REAL             RXY(29,29)
C
C     ..................................................................
C
      DOUBLE PRECISION SQRTCT (58)
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
      DATA MAXVEC / 28 /
C
C     ==================================================================
C
C     ERROR CHECKING.
C
      IF (NRMAX.GT.IZERO) GO TO 10
      CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
  10  IF (KIND(3).EQ.IZERO) GO TO 20
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
  20  K = NARGS - ITHRE
      IF (K.GE.ITHRE .AND. K.LE.MAXVEC) GO TO 40
  30  CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  40  NPARAM = IARGS(3)
      IF (K.EQ.NPARAM) GO TO 50
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
C     NVARS = TOTAL NUMBER OF VARIABLES.
C           = ONE DEPENDENT AND NPARAM INDEPENDENT VARIABLES.
C
  50  NVARS  = NPARAM + IONE
      NVARSO = NVARS
C
C     IWT = 0, IF 2ND ARGUMENT IS A CONSTANT
C         = 1, IF 2ND ARGUMENT IS A COLUMN NUMBER.
C
      IWT = IONE - KIND(2)
      IF (IWT.EQ.IONE) GO TO 70
      IF (ARGS(2)) 60,30,80
  60  CALL ERROR (25)
      RETURN
C
C     ..................................................................
C
  70  CALL ADRESS (ITWO,L)
C
C     MOVE IARGS TO LEFT AND CALL CHKCOL.
C
  80  IARGS(NARGS+1) = IARGS(1)
      NARGS = NVARS
      DO 90 I=1,NARGS
        IARGS(I) = IARGS(I+3)
        KIND(I)  = IZERO
  90  CONTINUE
C
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
C     START COMPUTING.
C
      ITYPE = ITHRE
      MBEST = ITEN
      IF (K.EQ.ITHRE) MBEST = 7
      N = NRMAX
      M = IZERO
      IF (IWT.EQ.IZERO) GO TO 130
C
C     (1)   CHECK ON WEIGHTS IF SECOND ARGUMENT IS A COLUMN NUMBER.
C
      M = L
      DO 120 I=1,NRMAX
        IF (RC(M)) 50,100,110
C
C       ZERO WEIGHTS USED.  ADJUST VALUE OF N.  DO NOT MOVE IARGS.
C
 100    N = N - IONE
C
C       CHECK ON NUMBER OF NON-ZERO WEIGHTS BEING USED.
C
        IF (N.GE.NPARAM) GO TO 110
        CALL ERROR (24)
        RETURN
 110    M = M + IONE
 120  CONTINUE
C
C     (2)   MOVE VECTORS TO SCRATCH AREA TO COMPUTE CORRELATION MATRIX.
C
 130  IBSUB = IONE
      DO 170 I=1,NVARS
        IRCSUB = IARGS(I)
        IF (IWT.EQ.IONE) M = L - IONE
        DO 160 J=1,NRMAX
          IF (IWT.EQ.IZERO) GO TO 140
          M = M + IONE
          IF (RC(M).LE.RZERO) GO TO 150
 140      A(IBSUB) = RC(IRCSUB)
          IBSUB    = IBSUB + IONE
 150      IRCSUB   = IRCSUB + IONE
 160    CONTINUE
 170  CONTINUE
C
C     DETERMINE IF FIRST VECTOR IS IDENTICALLY EQUAL TO ONE.
C        INTCPT = 0, IF FIRST X IS NOT IDENTICALLY EQUAL TO ONE AND
C               = 1, IF FIRST X IS     IDENTICALLY EQUAL TO ONE.
C
      INTCPT = IZERO
      ISUBB  = IONE
      DO 180 I=1,N
        IF (A(I).NE.RONE) GO TO 190
 180  CONTINUE
      INTCPT  = IONE
      ISUBB   = IONE + N
      NPARAM  = NPARAM - IONE
      NVARS   = NVARS  - IONE
C
C     (3)   COMPUTE CORRELATION MATRIX.
C
 190  CALL CRSPRD (A(ISUBB),N,NVARS,INTCPT,SQRTCT(1),RXY(1,1))
C
C     (4)   CHANGE IARGS FROM ADRESS BACK TO COLUMN NUMBER.
C
      DO 200 I=1,NVARSO
        IARGS(I) = IDIV (IARGS(I)-IONE,NROW,IND) + IONE
 200  CONTINUE
C
C     PRINT TITLE.
C
      CALL PAGE (IFOUR)
      CALL HEADS (IARGS(NVARSO),IONE,IZERO,IONE)
      IF (INTCPT.EQ.IZERO) WRITE (IPRINT,250) (LHEAD(I),I=1,12), NPARAM,
     1                                         N
      IF (INTCPT.EQ.IONE)  WRITE (IPRINT,260) (LHEAD(I),I=1,12), NPARAM,
     1                                         N
C
      MCOLS = ITWO
      NCOLS = ITWO - MOD (NPARAM,ITWO)
      MA    = LA(44)
      J     = IONE
      L     = IONE + INTCPT
      IEND  = IDIV (NPARAM-IONE,ITWO,IND)
      DO 210 I=1,NPARAM
        KIND(I) = I
 210  CONTINUE
C
      WRITE (IPRINT,290)
      IF (IEND.EQ.IZERO) GO TO 230
      DO 220 I=1,IEND
        CALL HEADS (IARGS(L),MCOLS,IZERO,IONE)
        WRITE (IPRINT,270) KIND(J), (LHEAD(J1),J1=1,12), KIND(J+1),
     1                    (LHEAD(J2),J2=13,24), MA
        J = J + MCOLS
        L = L + MCOLS
 220  CONTINUE
C
 230  MA = LA(45)
      CALL HEADS (IARGS(L),NCOLS,IZERO,IONE)
      IF (NCOLS.EQ.IONE) WRITE (IPRINT,280) KIND(J), (LHEAD(J1),J1=1,12)
      IF (NCOLS.EQ.ITWO) WRITE (IPRINT,270) KIND(J), (LHEAD(J1),J1=1,12)
     1                             , KIND(J+1), (LHEAD(J2),J2=13,24), MA
      WRITE (IPRINT,290)
C
C     (5)   CALL SCREEN.
C
      NDF = N - IONE
      IF (NDF.GT.NPARAM-IONE) GO TO 240
      CALL ERROR (24)
      RETURN
C
C     ..................................................................
C
 240  CALL SCREEN (RXY(1,1),NPARAM,NVARS,NDF,ITYPE,MBEST,INTCPT)
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 250  FORMAT (1H ,20HC(P) STATISTICS FOR ,12A1,24H AS A LINEAR FUNCTION 
     1OF               /2X,13H     1 UP TO ,I2,16H VARIABLES WITH ,
     2     I3,34H MEASUREMENTS WITH NONZERO WEIGHTS)
 260  FORMAT (1H ,20HC(P) STATISTICS FOR ,12A1,39H AS A LINEAR FUNCTION 
     1OF A CONSTANT AND/2X,13HFROM 1 UP TO ,I2,16H VARIABLES WITH ,
     2     I3,34H MEASUREMENTS WITH NONZERO WEIGHTS)
 270  FORMAT (1H ,8HVARIABLE,I3,4H IS ,12A1,1H,,3X,
     1            8HVARIABLE,I3,4H IS ,13A1)
 280  FORMAT (1H ,8HVARIABLE,I3,4H IS ,12A1)
 290  FORMAT (1H )
C
C     ==================================================================
C
      END
*BETRAN
      SUBROUTINE BETRAN (IRAN,KRAN,N,ALPHA,BETA,X,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BETRAN V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
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
      INCLUDE 'WRKSCR.H'
C
      REAL             X(*)
      REAL             ALPHA, BETA
      REAL             Y(1), Z(1)
      REAL             FDIV
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IND = IZERO
      IF (ALPHA.GE.RONE) GO TO 10
        IND = IONE
        RETURN
C
C     ..................................................................
C
  10  IF (BETA.GE.RONE) GO TO 20
        IND = IONE
        RETURN
C
C     ==================================================================
C
C     GENERATE N BETA RANDOM NUMBERS
C     BY USING THE FACT THAT
C     IF X1 IS A GAMMA VARIATE WITH PARAMETER ALPHA
C     AND IF X2 IS A GAMMA VARIATE WITH PARAMETER BETA,
C     THEN THE RATIO X1/(X1+X2) IS A BETA VARIATE
C     WITH PARAMETERS ALPHA AND BETA.
C
  20  DO 30 I=1,N
        CALL GAMRAN (IRAN,KRAN,IONE,ALPHA,Y,IND)
        CALL GAMRAN (IRAN,KRAN,IONE, BETA,Z,IND)
       X(I) = FDIV (Y(1),Y(1)+Z(1),JIND)
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*BINRAN
      SUBROUTINE BINRAN (IRAN,KRAN,N,P,NPAR,X,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. BINRAN V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE N BINOMIAL RANDOM NUMBERS IN X(.)
C
C     BASED ON DATAPAC PROGRAM UNIT BINRAN WRITTEN BY JAMES J. FILLIBEN.
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
      INCLUDE 'WRKSCR.H'
C
      REAL             X(*)
      REAL             P, U
      REAL             UNIRAN
      REAL             PCUT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA PCUT / 0.01 /
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
  20  IF (P.LT.PCUT) GO TO 50
C
C     IF P IS NOT SMALL, GENERATE N BINOMIAL RANDOM NUMBERS
C        USING THE REJECTION METHOD.
C
      DO 40 I=1,N
        ISUM = IZERO
        DO 30 J=1,NPAR
          U  = UNIRAN (IRAN,KRAN,IZERO)
          IF (U.LE.P) ISUM = ISUM + IONE
  30    CONTINUE
        X(I) = ISUM
  40  CONTINUE
      RETURN
C
C     ..................................................................
C
C     IF P IS SMALL, GENERATE N BINOMIAL RANDOM NUMBERS USING
C        THE FACT THAT THE WAITING TIME FOR 1 SUCCESS IN BERNOULLI
C           TRIALS HAS A GEOMETRIC DISTRIBUTION.
C
  50  DO 80 I=1,N
        ISUM = IZERO
        J    = IONE
  60    CALL GEORAN (IRAN,KRAN,1,P,A,IND)
        IG   = A(1) + RHALF
        ISUM = ISUM + IG + IONE
        IF (ISUM.GT.NPAR) GO TO 70
        J    = J + IONE
        GO TO 60
  70    X(I) = J - IONE
  80  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CALCOM
      SUBROUTINE CALCOM (XLIMIT,YLIMIT,LCR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CALCOM V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTIONS ARE ...
C
C        CALCOMP TAPE  L                                  L2 = 16 - 21
C        CALCOMP SPEED   (N)                              L2 = 8
C        CALCOMP SIZE    (N) HEIGHT OF PAPER              L2 = 9
C        CALCOMP SIZE    (K) HEIGHT OF Y-AIXS
C        CALCOMP SIZE    (N) PAPER, (K) Y-AXIS
C        CALCOMP SIZE    (K) Y-AXIS, (K) X-AXIS
C        CALCOMP SIZE    (N), (K) Y-AXIS, (K) X-AXIS
C        CALCOMP PLOT                                     L2 = 10
C     CALCOMP PLOT (N) NO. OF CURVES (E) OPTIONS (C),(C),...,(C) VS (C)
C     CALCOMP PLOT (N), (E), (C) VS (C), (C) VS (C),...,(C) VS (C)
C     CALCOMP PLOT (N), (E) Y LIMITS (K), (K),(C),(C),...,(C) VS (C)
C     CALCOMP PLOT (N),(E),(C),...,(C) VS (C), X-LIMITS (K), (K)
C     CALCOMP PLOT (N), (E), (K),(K), (C),...,(C) VS (C), (K), (K)
C     CALCOMP PLOT (N), (E), (K), (K), (C) VS (C),...,(C) VS (C),(K),(K)
C        CALCOMP SLOW                                     L2 = 11
C        CALCOMP FAST                                     L2 = 12
C        CALCOMP PAPER (N)                                L2 = 13
C        CALCOMP AXIS (K) Y-AXIS, (K) X-AXIS              L2 = 14
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MARCH, 1972.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             XLIMIT(*), YLIMIT(*)
      REAL             CHAR,XL, XR, YB, YT
      REAL             SPCA, SPCB, SPCC, SPCD, SPCE, SPCF, SPCG
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 10000 /
C
      DATA SPCA /     0.5000001 /
      DATA SPCB /     0.00005   /
      DATA SPCC / 10000.0       /
      DATA SPCD /     6.0       /
      DATA SPCE /     9.0       /
      DATA SPCF /    18.0       /
      DATA SPCG /    27.0       /
C
C     ==================================================================
C
      I    = IZERO
      KA   = IONE
      KB   = IONE
      KC   = IONE
      KD   = IONE
      LCR  = IZERO
      YT   = RZERO
      YB   = RZERO
      XL   = RZERO
      XR   = RZERO
      CHAR = RZERO
      L2 = L2 - 7
      IF (L2.LE.7) GO TO 10
C
C     INSTRUCTION IS CALCOM TAPE.
C
      L2 = L2 - 8
      IF (L2.LT.IONE .OR. L2.GT.6) CALL ERROR (28)
      IF (NERROR.NE.IZERO) RETURN
C     NTPE = L2 - IONE + LPTAPE
      RETURN
C
C     ..................................................................
C
  10  GO TO (20,40,170,20,20,100,130), L2
C
C     INSTRUCTION IS EITHER CALCOM SPEED, FAST OR SLOW.
C
  20  IF (NERROR.NE.IZERO) RETURN
      IF (L2.NE.IONE) GO TO 30
      ISPD = IARGS(1)
      IF (NARGS.LT.IONE) ISPD = ITWO
      IF (NARGS.GT.IONE) CALL ERROR (212)
      IF (ISPD.EQ.IZERO .OR. ISPD.EQ.IONE) RETURN
      CALL ERROR (301)
      ISPD = IONE
      RETURN
C
C     ..................................................................
C
C     IF L2 = 4, INSTRUCTION IS CALCOM SLOW.
C     IF L2 = 5, INSTRUCTION IS CALCOM FAST.
C
C 30  IF (L2.EQ.4) ISPD = IZERO
C     IF (L2.EQ.5) ISPD = IONE
  30  CALL ERROR (205)
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS CALCOM SIZE.
C
  40  IF (NARGS.EQ.IZERO) CALL ERROR (10)
      IF (NERROR.NE.IZERO) RETURN
      IF (NARGS.GT.IONE) GO TO 50
C
C      INSTRUCTION IS CALCOMP SIZE WITH ONE ARGUMENT.
C
C     IF KIND(1) = 0, INSTRUCTION IS SAME AS CALCOMP PAPER.
C
      IF (KIND(1).EQ.IZERO) GO TO 100
C
C     INSTRUCTION IS SAME AS CALCOMP AXIS WITH Y-AXIS ONLY DEFINED.
C
      GO TO 150
C
  50  IF (NARGS.GE.ITHRE) GO TO 60
C
C      INSTRUCTION IS CALCOMP SIZE WITH TWO ARGUMENTS.
C
      IF (KIND(1).EQ.IZERO) GO TO 70
C
C     COMMAND IS SAME AS CALCOMP AXIS.
C
      GO TO 140
C
C     INSTRUCTION IS CALCOMP SIZE WITH THREE ARGUMENTS.
C
  60  IF (KIND(3).EQ.IZERO) ARGS(3) = IARGS(3)
      XDH = AINT (ARGS(3) + SPCA)
  70  IF (KIND(2).EQ.IZERO) ARGS(2) = IARGS(2)
      HGT   = AINT (ARGS(2) + SPCA)
      IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)
      NPER = IARGS(1)
      IF (NPER.NE.12 .AND. NPER.NE.30) GO TO 90
      IF (NPER.GE.INT (HGT + RTWO)) RETURN
      IF (NPER.EQ.30) GO TO 80
      XDH = SPCD
      HGT = SPCE
      RETURN
C
C     PAPER SIZE IS 30 INCHES.
C
  80  XDH = SPCG
      HGT = SPCF
      RETURN
C
  90  CALL ERROR (302)
      NPER = 12
      HGT  = SPCE
      XDH  = SPCD
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS CALCOMP PAPER.
C
 100  IF (NERROR.NE.IZERO) RETURN
      IF (NARGS.EQ.IZERO) GO TO 110
      IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)
      NPER = IARGS(1)
      IF (NPER.EQ.12 .OR. NPER.EQ.30) GO TO 120
 110  CALL ERROR (302)
      NPER = 12
 120  XDH  = SPCD
      HGT  = SPCE
      IF (NPER.EQ.12) RETURN
      XDH = SPCF
      HGT = SPCG
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS CALCOMP AXIS.
C
 130  IF (NERROR.NE.IZERO) RETURN
      IF (NARGS.LT.ITWO) GO TO 160
 140  IF (KIND(1).EQ.IZERO) ARGS(1) = IARGS(1)
      IF (KIND(2).EQ.IZERO) ARGS(2) = IARGS(2)
      XDH = AINT (ARGS(2) + SPCA)
 150  HGT = AINT (ARGS(1) + SPCA)
      IF (NPER.GE.INT(HGT + RTWO)) RETURN
C
 160  CALL ERROR (302)
      XDH = SPCD
      HGT = SPCE
      IF (NPER.EQ.12) RETURN
      XDH = SPCF
      HGT = SPCG
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS CALCOM PLOT.
C
 170  NCR    = IZERO
      LST    = IZERO
      IEND   = IZERO
      ISTART = IZERO
      IF (NARGS.GE.IFOUR) GO TO 180
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 180  IF (KIND(1).EQ.IONE) IARGS(1) = ARGS(1)
      NCR = IARGS(1)
      IF (NCR.LE.IZERO) GO TO 510
      ISTART = ITHRE
      IEND = NARGS
      CALL ADRESS (ITWO,JCHAR)
      IARGS(2) = JCHAR
      IF (KIND(3).EQ.IZERO) GO TO 190
      IF (KIND(3)*KIND(4).NE.IONE) GO TO 510
      IF (NERROR.NE.IZERO) RETURN
      LST = LST + IONE
      ISTART = IFIVE
C
C     SET UP Y LIMITS, SINCE THEY ARE DEFINED.
C
      YLIMIT(1) = ARGS(3)
      YLIMIT(2) = ARGS(4)
      KA = IONE
      KB = ITWO
      IF (YLIMIT(1).LT.YLIMIT(2)) GO TO 190
      KB = IONE
      KA = ITWO
 190  IF (KIND(NARGS).EQ.IZERO) GO TO 200
      IF (KIND(NARGS)*KIND(NARGS-1).NE.IONE) GO TO 510
      LST = LST + ITWO
      IEND = IEND - ITWO
C
C     SET UP X LIMITS, SINCE THEY ARE DEFINED.
C
      XLIMIT(1) = ARGS(NARGS-1)
      XLIMIT(2) = ARGS(NARGS)
      KC = IONE
      KD = ITWO
      IF (XLIMIT(1).LT.XLIMIT(2)) GO TO 200
      KC = ITWO
      KD = IONE
 200  IF (IEND.LT.ISTART) GO TO 510
      DO 210 I=ISTART,IEND
        II = I
        CALL ADRESS (II,IARGS(I))
        IF (IARGS(I).LT.IZERO) GO TO 510
 210  CONTINUE
C
      NRG = IEND - ISTART + IONE
      LCR = IZERO
C
C     CHECK TO SEE IF AN X IS SPECIFIED FOR EACH Y.
C
      IF (NCR+IONE.EQ.NRG) LCR = IONE
      IF (ITWO*NCR.EQ.NRG) LCR = ITWO
      IF (LCR.EQ.IZERO) CALL ERROR (10)
      IF (NERROR.NE.IZERO) RETURN
      INE = IEND - IONE
      ISRT = ISTART + IONE
      IF (LCR.EQ.IONE) ISRT = IEND
      LSTA = LST + IONE
      GO TO (220,270,360,420), LSTA
C
C     DETERMINE Y LIMITS AND X LIMITS.
C
 220  K = IARGS(ISTART)
      YT = RC(K)
      YB = RC(K)
      DO 240 I=ISTART,INE,LCR
        K = IARGS(I)
        DO 230 IA=1,NRMAX
          YT = AMAX1(YT,RC(K))
          YB = AMIN1(YB,RC(K))
          K = K + IONE
 230    CONTINUE
 240  CONTINUE
      YLIMIT(1) = YB
      YLIMIT(2) = YT
C
C     DETERMINE X LIMITS.
C
      K = IARGS(ISRT)
      XL = RC(K)
      XR = RC(K)
      DO 260 I=ISRT,IEND,LCR
        K = IARGS(I)
        DO 250 J=1,NRMAX
          XR = AMAX1(XR,RC(K))
          XL = AMIN1(XL,RC(K))
          K = K + IONE
 250    CONTINUE
 260  CONTINUE
      XLIMIT(1) = XL
      XLIMIT(2) = XR
      GO TO 460
C
C     Y LIMITS ARE DEFINED BUT NOT X. DETERMINE X LIMITS.
C      CHECK TO SEE IF ANY PTS. ARE WITHIN LIMITS.
C
 270  KSTART = IONE
      LXR = IZERO
      DO 330 I=ISTART,IEND,LCR
        MA = IARGS(IEND)
        IF (LCR.EQ.ITWO) MA = IARGS(I+1)
        K = IARGS(I)
        DO 320 IA=1,NRMAX
          IF (RC(K).LT.YLIMIT(KA) .OR. RC(K).GT.YLIMIT(KB)) GO TO 310
          GO TO (290,300), KSTART
 290      KSTART = ITWO
          XR = RC(MA)
          XL = RC(MA)
          LXR = IONE
          GO TO 310
 300      XR = AMAX1(XR,RC(MA))
          XL = AMIN1(XL,RC(MA))
 310      MA = MA + IONE
          K = K + IONE
 320    CONTINUE
 330  CONTINUE
C
      IF (LXR.NE.IZERO) GO TO 350
 340  CALL ERROR (238)
      LCR = IZERO
      RETURN
 350  XLIMIT(1) = XL
      XLIMIT(2) = XR
      GO TO 460
C
C     LIMITS ARE DEFINED FOR X,  BUT NOT Y.  DETERMINE Y LIMITS.
C      CHECK TO SEE IF ANY PTS. ARE WITHIN LIMITS.
C
 360  LYT = IZERO
      KSTART = IONE
      DO 410 I=ISTART,IEND,LCR
        MA = IARGS(IEND)
        IF (LCR.EQ.ITWO) MA = IARGS(I+1)
        K = IARGS(I)
        DO 400 IA=1,NRMAX
          IF (RC(MA).LT.XLIMIT(KC) .OR. RC(MA).GT.XLIMIT(KD)) GO TO 390
          GO TO (370,380), KSTART
 370      KSTART = ITWO
          YT = RC(K)
          YB = RC(K)
          LYT = IONE
          GO TO 390
 380      YT = AMAX1(YT,RC(K))
          YB = AMIN1(YB,RC(K))
 390      MA = MA + IONE
          K = K + IONE
 400    CONTINUE
 410  CONTINUE
C
      IF (LYT.EQ.IZERO) GO TO 340
      YLIMIT(1) = YB
      YLIMIT(2) = YT
      GO TO 460
C
C     BOTH LIMITS ARE DEFINED.
C      CHECK TO SEE IF ANY PTS. ARE WITHIN LIMITS.
C
 420  DO 450 I=ISTART,IEND,LCR
        K = IARGS(I)
        MA = IARGS(IEND)
        IF (LCR.EQ.ITWO) MA = IARGS(I+1)
        DO 440 IA=1,NRMAX
          IF (RC(K).LT.YLIMIT(KA) .OR. RC(K).GT.YLIMIT(KB)) GO TO 430
          IF (RC(MA).GE.XLIMIT(KC) .AND. RC(MA).LE.XLIMIT(KD)) GO TO 460
 430      MA = MA + IONE
          K = K + IONE
 440    CONTINUE
 450  CONTINUE
      CALL ERROR (238)
      LCR = IZERO
      RETURN
C
C     CHECK FOR CORRECT PLOTTING SYMBOLS.
C
 460  LJN   = IZERO
      LTALC = IZERO
      NCRA  = NCR
      IF (JCHAR.GT.IZERO) GO TO 470
      NCRA   = IONE
      CHAR   = ARGS(2)
 470  JJCHAR = IABS(JCHAR)
      DO 500 I=1,NCRA
        IF (JCHAR.GT.IZERO) CHAR = RC(JJCHAR)
        JJCHAR = JJCHAR + IONE
        K      = (CHAR+SPCB) * SPCC
        JOIN   = IDIV (K,ICA,IND)
        ISYM   = IDIV (MOD(K,ICA),IHRD,IND)
        NPT    = MOD (K,IHRD)
        IF (JOIN.GE.IONE .AND. JOIN.LE.9) GO TO 480
        LJN = LJN + IONE
 480    IF (ISYM.GE.IZERO .AND. ISYM.LE.85) GO TO 490
        LTALC = LTALC + IONE
 490    IF (NPT.EQ.IZERO) NPT = IONE
 500  CONTINUE
      IF (LJN+LTALC.GT.IZERO) CALL ERROR (303)
      IFG = IFG + IONE
      RETURN
C
C     ..................................................................
C
 510  CALL ERROR (3)
      RETURN
C
C     ==================================================================
C
      END
*CALPLT
      SUBROUTINE CALPLT (XLIMIT,YLIMIT,IX)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CALPLT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALCOMP PLOTTING PROGRAM UNIT.
C
C     NPAPER IS SIZE OF PAPER USED.  12 OR 30 INCH PAPER.
C
C     ISPEED = 0, SLOW
C            = 1, FAST
C
C     NTAPE UNIT FOR OUTPUT OF CALCOMP PLOTTER ROUTINES.
C
C     HGT IS HEIGHT OF Y-AXIS.
C
C     XWDTH IS WIDTH OF X-AXIS.
C
C     IFIG IS GRAPH NUMBER TO BE PRINTED.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  JANUARY, 1972.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NBF(120), ICHAR2(34), ISYMBT(34)
      DIMENSION ISYMB2(15)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
      INCLUDE 'WRKSCR.H'
C
C  MODIFIED JULY 1992 TO USE SELF-CONTAINED CALCOMP COMPATIBLE 
C  LIBRARY.  IF ITEKSW=IZERO, USE STANDARD CALL (WHICH IS LINKED TO
C  EITHER A LOCAL CALCOMP LIBRARY OR A SET OF NULL ROUTINES).  IF
C  ITEKSW=IONE, USE THE SELF-CONTAINED CALCOMP ROUTINES WHICH CURRENTLY
C  SUPPORT TEKTRONIX (TO SCREEN OR FILE), X11 (REQUIRES X11 BEING 
C  AVAILABLE ON YOUR SYSTEM), POSTSCRIPT, HPGL, HPGL FOR THE LASER JET 
C  III, QMS (QUIC PROTOCOL).  OTHERS MAY BE ADDED LATER.
C  FOLLOWING COMMON BLOCK USED BY THE UNDERLYING CALCOMP ROUTINES. 
C
C  IBACK USED TO SET THE X11 BACKGROUND COLOR, AXSIZE AND AYSIZE
C  USED TO SET THEN NUMBER OF INCHES FOR THE PLOT (NOT NECCESSARY TO
C  TRUNCATE BELOW A MAXIMUM VALUE, WILL SCALE OVER-SIZED PLOTS TO
C  FIT THE PARTICULAR DEVICE), IMAXCL SETS MAXIMUM NUMBER OF COLORS
C  FOR HP-GL PLOTTER.  IFORE SETS DEFAULT FOREGROUND COLOR (CAN SET  
C  FOR X11, POSTSCRIPT, AND HP-GL, FOR NOW ONLY DO FOR X11 CASE).
C
      COMMON/VGCALC/IBACK,IFORE,AXSIZE,AYSIZE,IMAXCL
      CHARACTER*80 CTEMP
C
C  CPOST  = NAME OF POSTSCRIPT FILE (DEFAULT: post.dat)
C  CHPGL  = NAME OF HPGL FILE (DEFAULT: hpgl.dat)
C  CHPGL2 = NAME OF HPGL FILE (DEFAULT: hpgl2.dat)
C  CQMS   = NAME OF QMS FILE (DEFAULT: qms.dat)
C  CTEKET = NAME OF TEKTRONIX FILE (DEFAULT: tekt.dat)
C
C  THE DEFAULT NAMES ARE SET IN calcmp.f FILE.  ONLY NEED TO SPECIFY
C  IF WANT TO OVERRIDE THE DEFAULT NAMES.
C
      CHARACTER*80 CPOST,CHPGL,CHPGL2,CQMS,CTEKT
      COMMON/VGNAME/CPOST,CHPGL,CHPGL2,CQMS,CTEKT
      CHARACTER*4 X11FLG
C
C  NAME OF X11 DISPLAY SHOULD BE SET IN IDSPLY (SEND AS ASCII 
C  DECIMAL EQUIVALENTS) IF NOT THE DEFAULT.  THIS ALLOWS IT TO WORK
C  OVER A NETWORK.  NEED TO ADD SOME TYPE OF "X11 DISPLAY" COMMAND.
C
      INTEGER IDSPLY(80)
      COMMON/VGX11/IDSPLY
      COMMON/VGX112/X11FLG
      REAL ARRCAL(2)
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             XLIMIT(*), YLIMIT(*)
      REAL             SYM, X, XSTP, Y, YSTP
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC, SPCD
C
C     ...................................................................
C
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER NBF*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IBUF / 0 /
C
      DATA ICA /   24 /
      DATA ICB / 1000 /
C
      DATA SPCA /      0.000005/
      DATA SPCB /   100.0      /
      DATA SPCC / 10000.0      /
      DATA SPCD /     0.6875   /
      DATA SPCE /100000.0      /
C
      DATA ICHAR2( 1), ICHAR2( 2), ICHAR2( 3), ICHAR2( 4), ICHAR2( 5),
     1     ICHAR2( 6), ICHAR2( 7), ICHAR2( 8), ICHAR2( 9), ICHAR2(10),
     2     ICHAR2(11), ICHAR2(12), ICHAR2(13), ICHAR2(14), ICHAR2(15),
     3     ICHAR2(16), ICHAR2(17), ICHAR2(18), ICHAR2(19), ICHAR2(20),
     4     ICHAR2(21), ICHAR2(22), ICHAR2(23), ICHAR2(24), ICHAR2(25),
     5     ICHAR2(26), ICHAR2(27), ICHAR2(28), ICHAR2(29), ICHAR2(30),
     6     ICHAR2(31), ICHAR2(32), ICHAR2(33), ICHAR2(34)/
     7           118,       120,        86,       114,       193,
     8            59,        77,       120,       121,        64,
     9            65,        63,        87,        57,        53,
     A            52,        73,        74,        75,        58,
     B            54,        56,        66,        15,        72,
     C            69,        61,       170,        71,        78,
     D            55,        62,        11,       111/
C  
C  MODIFIED JULY 1992.  MODIFY TO MORE CLOSELY MATCH VOLKSGRAPHER 
C  SYMBOL TABLE.  WILL TRY TO MATCH WHAT IS IN OMNITAB DOCUMENTATION,
C  BUT THIS NOT ALWAYS POSSIBLE.
CCCCC DATA ISYMBT( 1), ISYMBT( 2), ISYMBT( 3), ISYMBT( 4), ISYMBT( 5),
CCCCC1     ISYMBT( 6), ISYMBT( 7), ISYMBT( 8), ISYMBT( 9), ISYMBT(10),
CCCCC2     ISYMBT(11), ISYMBT(12), ISYMBT(13), ISYMBT(14), ISYMBT(15),
CCCCC3     ISYMBT(16), ISYMBT(17), ISYMBT(18), ISYMBT(19), ISYMBT(20),
CCCCC4     ISYMBT(21), ISYMBT(22), ISYMBT(23), ISYMBT(24), ISYMBT(25),
CCCCC5     ISYMBT(26), ISYMBT(27), ISYMBT(28), ISYMBT(29), ISYMBT(30),
CCCCC6     ISYMBT(31), ISYMBT(32), ISYMBT(33), ISYMBT(34)/
CCCCC7            39,         6,        39,        59,        58,
CCCCC8            44,        39,         6,        39,        49,
CCCCC9            50,        57,         2,        42,        38,
CCCCCA            37,        58,        59,        62,        43,
CCCCCB            39,        41,        53,        51,        61,
CCCCCC            39,        46,        56,        52,        63,
CCCCCD            55,        47,        10,        39/
C  7 = 52-56
C  8 = 57-61
C  9 = 62-66
C  A = 67-71
C  B = 72-76
C  C = 77-81
C  D = 82-85
      DATA ISYMBT( 1), ISYMBT( 2), ISYMBT( 3), ISYMBT( 4), ISYMBT( 5),
     1     ISYMBT( 6), ISYMBT( 7), ISYMBT( 8), ISYMBT( 9), ISYMBT(10),
     2     ISYMBT(11), ISYMBT(12), ISYMBT(13), ISYMBT(14), ISYMBT(15),
     3     ISYMBT(16), ISYMBT(17), ISYMBT(18), ISYMBT(19), ISYMBT(20),
     4     ISYMBT(21), ISYMBT(22), ISYMBT(23), ISYMBT(24), ISYMBT(25),
     5     ISYMBT(26), ISYMBT(27), ISYMBT(28), ISYMBT(29), ISYMBT(30),
     6     ISYMBT(31), ISYMBT(32), ISYMBT(33), ISYMBT(34)/
     7           169,        39,       211,       190,       188,
     8            44,        39,        39,        39,        91,
     9            93,        35,         2,        41,        45,
     A            43,        60,        62,        64,        36,
     B            42,        40,        37,        58,        63,
     C            33,        44,        39,        39,        59,
     D            47,        46,        10,       163/
      DATA ISYMB2( 1), ISYMB2( 2), ISYMB2( 3), ISYMB2( 4), ISYMB2( 5),
     1     ISYMB2( 6), ISYMB2( 7), ISYMB2( 8), ISYMB2( 9), ISYMB2(10),
     2     ISYMB2(11), ISYMB2(12), ISYMB2(13), ISYMB2(14), ISYMB2(15)/
     7             1,         0,         2,         6,         7,
     8             4,         3,         7,        16,        17,
     9             5,        21,         7,        18,        20/
      DATA ARRCAL(1), ARRCAL(2) /0., 0./
      DATA ARRCAL(1), ARRCAL(2) /0., 0./
C
C     ==================================================================
C
C     DEFINE BUFFER AREA AND ZIP MODE IS NOT DONE ALREADY OR OVERLAY
C        HAS OCCURED.
C
C FOLLOWING LINE SCREWS UP THE THE OMNITAB CALCOMP COMPATIBLE ROUTINES.
C THESE NEED TO CALL PLOTS FOR EVERY NEW OCCURENCE.  MOVE THIS CODE.
C
CCCCC IF (IBUF.NE.IZERO) GO TO 20
CCCCC IBUF = IONE
C  
C  JULY 1992.  FOLLOWING 2 LINES MODIFIED.  PLOTS WILL USE A LOCAL
C  CALCOMP LIBRARY (IF AVAILABLE, OTHERWISE, NULL ROUTINE), PLOTST USES
C  OMNITAB CALCOMP COMPATIBLE LIBRARY.
C
CCCCC IF (ITEKSW.EQ.IZERO) CALL PLOTS (A(NRC+1),ICB,NTPE)
CCCCC IF (ITEKSW.EQ.IONE) CALL PLOTST (A(NRC+1),ICB,NTPE)
      IFORE = IONE 
      IF (ITEKSW.EQ.IONE) THEN
        AXSIZE = XDH + 2.0
        AYSIZE = HGT + 1.5
C  FOLLOWING IS TEMPORARY KLUDGE TO SET THE DEVICE UNTIL AN OMNITAB
C  "SET DEVICE" COMMAND CAN BE ADDED.
C 
      CTEMP='post'
      OPEN(UNIT=99,FILE='device.dat',ERR=9019)
      READ(99,'(A80)') CTEMP
 9019 CONTINUE
      IF(CTEMP(1:4).EQ.'tekt' .OR. 
     *   CTEMP(1:4).EQ.'vt10' .OR.
     *   CTEMP(1:4).EQ.'vt20')THEN
        IDEVIC=0
        NJUNK=6
        IF(CTEMP(6:6).EQ.'f')NJUNK=90
      ELSE IF(CTEMP(1:3).EQ.'x11')THEN
        IDEVIC=30
        NJUNK=6
C
C  5,6  - BACKGROUND COLOR (INTEGER BETWEEN 0 AND 64)
C  8,9  - FOREGROUND COLOR (INTEGER BETWEEN 0 AND 64)
C  11-80  - X11 DISPLAY NAME 
C
        IF(CTEMP(5:5).NE.' ') READ(CTEMP(5:6),'(I2)')IBACK
        IF(CTEMP(8:8).NE.' ') READ(CTEMP(8:9),'(I2)')IFORE
        IF(CTEMP(11:11).NE.' ')THEN
          DO9310II=11,80
            IF(CTEMP(II:II).EQ.' ')GOTO9319
            ILAST=II
            IDSPLY(II-10)=ICHAR(CTEMP(II:II))
 9310     CONTINUE
 9319     CONTINUE
          IDSPLY(ILAST+1)=0
        ENDIF
      ELSE IF(CTEMP(1:4).EQ.'post')THEN
        NJUNK=91
        IDEVIC=22
      ELSE IF(CTEMP(1:5).EQ.'hpgl ')THEN
        NJUNK=92
        IDEVIC=20
      ELSE IF(CTEMP(1:5).EQ.'hpgl2')THEN
        NJUNK=93
        IDEVIC=23
      ELSE IF(CTEMP(1:3).EQ.'qms')THEN
        NJUNK=94
        IDEVIC=21
      ELSE
        NJUNK=95
        IDEVIC=22
      ENDIF
      CLOSE(99)
C
        CALL PLOTST (IDEVIC, 0, NJUNK)
C
C  SET FOREGROUND COLOR AND SOLID LINE
        ICNT=0
        CALL DASHST(ARRCAL,ICNT)
        CALL NEWPNT(IFORE)
      ENDIF
C
C  CODE MOVED FROM ABOVE
      IF (IBUF.NE.IZERO) GO TO 20
      IBUF = IONE
      IF (ITEKSW.EQ.IZERO) CALL PLOTS (A(NRC+1),ICB,NTPE)
C
C     SET SPEED NOZIP = SLOW,  GOZIP = FAST.
C
C     IF (ISPD.GT.IZERO) GO TO 10
C     CALL NOZIP
C     GO TO 20
C 10  CALL GOZIP
C
C     ITEKSW IS SET TO -1 ONLY WHEN PROPRIETARY PROCEDURES
C     FOR CALCOMP AND TEKTRONIX ARE MISSING.  A MESSAGE
C     IS PRINTED THAT STATES CALCOMP AND TEKTRONIX COMMANDS
C     ARE NOT EXECUTED. 
C
  20  IF (ITEKSW .EQ. -IONE) THEN
        WRITE (IPRINT,280)
        WRITE (ISCRT,280)
        RETURN
      ENDIF
C
C     SET ORIGIN.
C
      IF (ITEKSW.EQ.IZERO) CALL PLOT (RHALF,SPCD,-ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (RONE+RHALF,SPCD,-ITHRE)
C
C     CALL TICK SUBROUTINE TO SET UP SCALE DRAW AXIS AND TICK MARKS.
C
C  MODIFIED JULY 1992.
CCCCC IF (ITEKSW.EQ.IZERO) CALL NEWPEN (IONE)
CCCCC IF (ITEKSW.NE.IZERO) CALL NEWPNT (IONE)
      IF (ITEKSW.EQ.IZERO) CALL NEWPEN (IFORE)
      IF (ITEKSW.NE.IZERO) CALL NEWPNT (IFORE)
      CALL CALTIK (XLIMIT,YLIMIT,XDH,HGT,A,1,ITLE(1,5),ITLE(1,6),
     1             NOCARD,IFG,NBF(1))
C
C     PLOT EACH CURVE ONE AT A TIME FOR EACH GRAPH.
C
      NCUR = IARGS(1)
      NC   = IZERO
      IBT  = IZERO
      IF (KIND(2).EQ.IZERO) IBT = IARGS(2)
      ISY  = ITHRE
      IF (KIND(3).EQ.IONE) ISY = IFIVE
      ISX  = ISY + IONE
      LSYM = IONE
      IF (IX.EQ.ITWO) GO TO 30
      ISX  = NARGS
      IF (KIND(NARGS).EQ.IONE) ISX = NARGS - ITWO
  30  DO 230 IA=1,NCUR
        IXT  = NRMAX + ITWO
        IXTA = IXT + IONE
        JY   = IARGS(ISY)
        JX   = IARGS(ISX)
        KY   = IONE
        KYP  = ITWO
        IF (YLIMIT(1).LT.YLIMIT(2)) GO TO 40
        KY   = ITWO
        KYP  = IONE
  40    KX   = IONE
        KXP  = ITWO
        IF (XLIMIT(1).LT.XLIMIT(2)) GO TO 50
        KX   = ITWO
        KXP  = IONE
  50    N    = IZERO
        DO 70 I=1,NRMAX
          IF (RC(JY).LT.YLIMIT(KY) .OR. RC(JY).GT.YLIMIT(KYP)) GO TO 60
          IF (RC(JX).LT.XLIMIT(KX) .OR. RC(JX).GT.XLIMIT(KXP)) GO TO 60
          N = N + IONE
          IXT = IXT + IONE
          A(N)   = RC(JY)
          A(IXT) = RC(JX)
  60      JX = JX + IONE
          JY = JY + IONE

  70    CONTINUE
C
C       IF PTS FALL OUT OF X AND Y LIMITS, NC HAS COUNT FOR CURVES/GR.
C
        IF (N.NE.IZERO) GO TO 80
        NC = NC + IONE
        IF (IBT.NE.IZERO) IBT = IBT + IONE
        GO TO 210
  80    SYM = ARGS(2)
        IF (IBT.EQ.IZERO) GO TO 90
        SYM  = RC(IBT)
        IBT  = IBT + IONE
  90    ILN  = INT (SYM+SPCA)
        ISYM = INT ((SYM+SPCA-FLOAT(ILN))*SPCB)
        IPNT = INT ((SYM+SPCA-FLOAT(ILN))*SPCC-FLOAT(ISYM)*SPCB)
        IPEN = INT ((SYM+SPCA)*SPCE)-((ILN*IHRD+ISYM)*IHRD+IPNT)*ITEN
        IF (IPEN.LT.IONE .OR. IPEN.GT.IFOUR) IPEN = IONE
        A(N+1)   = YLIMIT(3)
        A(N+2)   = YLIMIT(4)
        A(IXT+1) = XLIMIT(3)
        A(IXT+2) = XLIMIT(4)
        IF (ILN.GE.IONE .AND. ILN.LE.9) GO TO 100
        ILN = IONE
        LINTYP = IONE
 100    IF (ISYM.LT.IZERO .OR. ISYM.GT.85) ISYM = 85
        LINTYP = IPNT
        IF (LINTYP.LT.IZERO) LINTYP = IZERO
        NPTS = N
        IF (MOD(ILN,2).EQ.IONE) GO TO 110
        LINTYP = IZERO
        GO TO 150
 110    IF (LINTYP.EQ.IZERO) LINTYP = IONE
        IF (ILN.NE.ITHRE) LINTYP = -LINTYP
        IF (ISYM.NE.IZERO) GO TO 120
        ISYM  = LSYM
        LSYM  = LSYM + IONE
 120    INTEQ = ISYM - IONE
C
C     IF ISYM IS LESS THEN 16, INTEQ CONTAINS THE CODE VALUE
C     TO PLOT CENTERED CHARACTERS.
C
C     MODIFIED JULY 1992.  
CCCCCC  IF (ISYM.LE.51) GO TO 150
C
C      SPECIAL CHARACTER IS USED FOR PLOTTING.
C      PICK UP CODE FOR SPECIAL CHARACTER FORM ICHAR
C      AND PUT IN INTEQ.
C
CCCCC   IF (ITEKSW.EQ.IZERO)
CCCCC1  INTEQ = ICHAR(ISYM-51)
CCCCC   IF (ITEKSW.NE.IZERO)
CCCCC1  INTEQ = ISYMBT(ISYM-51)
C      NEW CODE HERE.   `
C        1 - 15   - SPECIAL SYMBOL FOR CALCOMP LIBRARY (THE OMNITAB
C                   CALCOMP COMPATIBLE LIBRARY SUPPORTS SOMEWHAT 
C                   DIFFERENT SET) 
C       16 - 41   - UPPER CASE LETTERS
C       42 - 51   - NUMBERS
C       52 - 85   - MISCELLANEOUS SYMBOLS (MOSTLY NON-ALPHABETIC ASCII
C                   CHARACTERS.
C      THIS WAS ORIGINALLY CODED BEFORE MOST COMPUTERS ACCEPTED THE 
C      ASCII CHARACTER SET.  CONVERT TO SEND THE CORRESPONDING ASCII 
C      NUMBER WHERE APPROPRIATE.
        IF (ITEKSW.EQ.0) THEN
          IF (ISYM.LE.15) THEN 
            GOTO150
          ELSE IF (ISYM.LE.41) THEN
            INTEQ = 64 + ISYM - 15
          ELSE IF (ISYM.LE.51) THEN
            INTEQ = 47 + ISYM - 41
          ELSE
            INTEQ = ICHAR2(ISYM-51)
          END IF
        ELSE
          IF (ISYM.LE.15) THEN 
            INTEQ = ISYMB2(ISYM)
          ELSE IF (ISYM.LE.41) THEN
            INTEQ = 64 + ISYM - 15
          ELSE IF (ISYM.LE.51) THEN
            INTEQ = 47 + ISYM - 41
          ELSE
            INTEQ = ISYMBT(ISYM-51)
          END IF
        END IF 
 150    INC  = IONE
        ISTP = -IONE
        XSTP = FDIV (A(IXTA)-XLIMIT(3),XLIMIT(4),IND)
        YSTP = FDIV (A(1)-YLIMIT(3),YLIMIT(4),IND)
C  MODIFIED JULY 1992.  CALL NEWPNT FOR ITEKSW=1
        IF (ITEKSW.EQ.IZERO) CALL NEWPEN (IPEN)
        IF (ITEKSW.NE.IZERO) CALL NEWPNT (IPEN)
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XSTP,YSTP,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XSTP,YSTP,ITHRE)
C
        GO TO (160,160,160,170,160,180,160,190,160), ILN
C
 160    CALL FLINE (A(IXTA),A(1),NPTS,INC,LINTYP,INTEQ)
        IF (ILN.EQ.7) GO TO 180
        IF (ILN.EQ.9) GO TO 190
        IF (ILN.NE.IFIVE) GO TO 210
        IF (ITEKSW.EQ.IZERO) CALL PLOT  (XSTP,YSTP,ITHRE)
        IF (ITEKSW.NE.IZERO) CALL PLOTT (XSTP,YSTP,ITHRE)
 170    CALL DASHLN (A(IXTA),A(1),IABS(NPTS),INC)
        GO TO 210
C
C  MODIFIED JULY 1992.  DON'T USE PROPRIETARY SMOOTH ROUTINE.  FOR
C  NOW, SIMPLY USE PLOT (I.E., MOVE AND DRAW) WITHOUT SMOOTHING.
C  AT LATER DATE, MAY IMPLEMENT NON-PROPRIETARY VERSION OF SMOOTH
C  ROUTINE.
C
C  THIS IS CURRENLY NOT WORKING CORRECTLY (SCALE FACTORS OFF).  WILL
C  LEAVE FOR NOW, MAY FIX LATER.
C
 180    ISTP = IZERO
 190    X = XSTP
        Y = YSTP
CCCCC   CALL SMOOTH (X,Y,ISTP)
        IF (ITEKSW .EQ. IZERO) THEN
          CALL PLOT(X,Y,ISTP)
        ELSE
          CALL PLOTT(X,Y,ISTP)
        ENDIF
        NMO  = N - IONE
        IXTB = IXTA + IONE
        DO 200 I=2,NMO
          X = FDIV (A(IXTB)-XLIMIT(3),XLIMIT(4),IND)
          Y = FDIV (A(I)-YLIMIT(3),YLIMIT(4),IND)
CCCCCC    CALL SMOOTH (X,Y,-ITWO)
          IF (ITEKSW .EQ. IZERO) THEN
            CALL PLOT(X,Y,ITWO)
          ELSE
            CALL PLOTT(X,Y,ITWO)
          ENDIF
          IXTB = IXTB + IONE
 200    CONTINUE
        X = FDIV (A(IXTB)-XLIMIT(3),XLIMIT(4),IND)
        Y = FDIV (A(N)-YLIMIT(3),YLIMIT(4),IND)
CCCCC   CALL SMOOTH (X,Y,-ICA)
        IF (ITEKSW .EQ. IZERO) THEN
          CALL PLOT(X,Y,IABS(-ICA))
        ELSE
          CALL PLOTT(X,Y,IABS(-ICA))
        ENDIF
 210    IF (IX.EQ.ITWO) GO TO 220
        ISY = ISY + IONE
        GO TO 230
 220    ISY = ISY + ITWO
        ISX = ISX + ITWO
 230  CONTINUE
C
C     MOVE PAPER FIVE INCHES BEYOND GRAPH.
C
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (XDH+RFIVE,RZERO,ITHRE)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (XDH+RFIVE,RZERO,ITHRE)
C
C     CALL PLOT TO INDICATE END OF GRAPH.
C
      IF (ITEKSW.EQ.IZERO) CALL PLOT  (RZERO,RZERO,999)
      IF (ITEKSW.NE.IZERO) CALL PLOTT (RZERO,RZERO,999)
C
C  MODIFIED JULY 1992.  FOLLOWING BLOCK OF CODE NOT NEEDED (PAUSE
C  COMMAND HANDLED FOR SCREEN DEVICES IN THE UNDERLYING GRAPHICS 
C  ROUTINES).
CCCCC IF (ITEKSW.NE.IZERO) THEN
CCCCC   WRITE (IPRINT,260)
CCCCC   READ (NUNIT(1),270,END=240) 
CCCCC   GO TO 250
C
C240    CLOSE (NUNIT(1))
CCCCC   OPEN (NUNIT(1),FILE='sys$INPUT')
CCCCC END IF
 250  RETURN
C
C     ================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 260  FORMAT (4H ...)
 270  FORMAT (1A1)
 280  FORMAT ('    COMMAND NOT AVAILABLE BECAUSE PROPRIETARY PROCEDURES 
     1ARE NOT PROVIDED.')
C
C     ==================================================================
C
      END
*CAUPLT
      SUBROUTINE CAUPLT (X,Y,W,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CAUPLT V 7.00 12/ 5/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE GENERATES A CAUCHY 
C              PROBABILITY PLOT.
C              THE PROTOTYPE CAUCHY DISTRIBUTION USED HEREIN
C              HAS MEDIAN = 0 AND 75% POINT = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/PI) * (1/(1+X*X)).
C              AS USED HEREIN, A PROBABILITY PLOT FOR A DISTRIBUTION
C              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS 
C              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
C              THE CAUCHY PROBABILITY PLOT IS USEFUL IN
C              GRAPHICALLY TESTING THE COMPOSITE (THAT IS,
C              LOCATION AND SCALE PARAMETERS NEED NOT BE SPECIFIED)
C              HYPOTHESIS THAT THE UNDERLYING DISTRIBUTION
C              FROM WHICH THE DATA HAVE BEEN RANDOMLY DRAWN 
C              IS THE CAUCHY DISTRIBUTION.
C              IF THE HYPOTHESIS IS TRUE, THE PROBABILITY PLOT
C              SHOULD BE NEAR-LINEAR.
C              A MEASURE OF SUCH LINEARITY IS GIVEN BY THE
C              CALCULATED PROBABILITY PLOT CORRELATION COEFFICIENT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X. 
C     OUTPUT--A ONE-PAGE CAUCHY PROBABILITY PLOT. 
C     PRINTING--YES.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, UNIMED, PLOT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, SIN, COS.
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
C                 DISTRIBUTIONS--1, 1970, PAGES 154-165.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE  301-975-2845 
C     ORIGINAL VERSION -     JUNE, 1972.
C      CURRENT VERSION - NOVEMBER, 1989.
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
C                    ***   TYPE STATEMENTS   ***
C
      REAL             W(*), X(*), Y(*) 
      REAL             ATEMP(1), YINT(1), YSLOPE(1)
      REAL             AN, ARG, CC, SUM1, SUM2, SUM3, WBAR, YBAR
      REAL             FCOS, FDIV, FSIN, FSQRT
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
        ARG = PI * W(I)
        W(I) = -FDIV (FCOS(ARG),FSIN(ARG),IND)
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
 100  FORMAT (15X, 6HCAUCHY            ,21H PROBABILITY PLOT OF ,
     1   I5,17H MEASUREMENTS IN ,12A1)
 110  FORMAT (15X, 6HCAUCHY            ,21H PROBABILITY PLOT OF ,
     1        12A1,5H, N =,I5)
 120  FORMAT ( 1X,3HN =,I5,6X, 6HCAUCHY            ,14H PROB PLOT OF ,
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
*CBEK
      SUBROUTINE CBEK (R,S,DA,B,C,D)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   CBEK V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE K0(Z) AND K1(Z) FOR COMPLEX ARGUMENT Z=R*E(IS)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /DTCONS/ DALOG2, DEULER
C
      INCLUDE 'WRKSCR.H'
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DALOG2, DEULER
      DOUBLE PRECISION B, C, D, DA, R, S
      DOUBLE PRECISION AA(40), AAB(80), AB(40)
      DOUBLE PRECISION AC, AD, AE, E, F, G, H, P, Q, T, U, V, W, X, Y, Z
      DOUBLE PRECISION FDCOS, FDDIV, FDEXP, FDLOG, FDSIN, FDSQRT
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE
C
      EQUIVALENCE (AAB(1),A(1)), (AA(1),AAB(1)), (AB(1),AAB(41))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA /  3.0D0            /
      DATA DPCB /  8.0D0            /
      DATA DPCC /  0.5D-10          /
      DATA DPCD /  1.25D0           /
      DATA DPCE / -0.785398163397D0 /
C
C     ==================================================================
C
      IF (R.LE.DZERO) GO TO 70
      E = FDCOS (S)
      F = FDSIN (S)
      IF (R.GT.DPCB) GO TO 40
      P = DONE-DTWO*F**2
      Q = DTWO*E*F
      W = P
      Z = Q
      X = FDDIV (R,DTWO,IND)**2
      Y = X
      V = X
      G = E*(DFOR*E**2-DPCA)
      H = F*(DPCA-DFOR*F**2)
      T = FDLOG ( FDDIV(R,DTWO,IND) ) + DEULER
      DA = -T
      B = -S
      C = E*(T-DHALF)-S*F
      U = F*(T-DHALF)+S*E
      AC = DONE
      AD = DTWO
      AA(1) = DONE
      AB(1) = DPCD
      DO 10 N=2,40
        AE = N
        AA(N) = AA(N-1) + FDDIV (DONE,AE,IND)
        AB(N) = AA(N) + FDDIV (DONE,DTWO*(AE+DONE),IND)
  10  CONTINUE
C
      DO 20 N=1,40
        AE = T-AA(N)
        D = P*AE-S*Q
        AE = Q*AE+S*P
        DA = DA - FDDIV (D*X,AC**2,IND)
        B = B - FDDIV (AE*X,AC**2,IND)
        AE = T-AB(N)
        D = G*AE-H*S
        AE = H*AE+G*S
        C = C + FDDIV (D*Y,AC*AD,IND)
        U = U + FDDIV (AE*Y,AC*AD,IND)
        X = FDDIV (X*V,AC**2,IND)
        IF (X.LT.DPCC) GO TO 30
        Y = FDDIV (Y*V,AC*AD,IND)
        AC = AC+DONE
        AD = AD+DONE
        AE = P
        P = AE*W-Q*Z
        Q = Q*W+AE*Z
        AE = G
        G = AE*W-H*Z
        H = H*W+AE*Z
  20  CONTINUE
C
  30  C = FDDIV (E,R,IND) + FDDIV (R*C,DTWO,IND)
      D = -FDDIV (F,R,IND) + FDDIV (R*U,DTWO,IND)
      RETURN
C
C     ..................................................................
C
  40  U = FDEXP(-R*E) * FDSQRT ( FDDIV(DHLFPI,R,IND) )
      V = R*F + FDDIV (S,DTWO,IND)
      Y = U * FDCOS (V)
      Z = U * FDSIN (V)
      W = -DONE
      G = DONE
      H = DPCA
      P = E
      Q = F
      T = DONE
      U = DZERO
      V = DONE
      X = DZERO
      DA = DONE
      B = FDDIV (DONE,DPCB*R,IND)
      C = B
      D = B
      AC = -DONE
      DO 50 N=1,12
        AD = FDDIV (AC*B*G**2,DA,IND)
        AE = FDDIV (AC*C*W*H,DA,IND)
        T = T+AD*P
        U = U-AD*Q
        V = V+AE*P
        X = X-AE*Q
        AD = B
        B = FDDIV (B*D*G**2,DA,IND)
        IF (B.GT.AD) GO TO 60
        IF (B.LT.DPCC) GO TO 60
        C = FDDIV (C*D*W*H,DA,IND)
        W = W+DTWO
        H = H+DTWO
        G = G+DTWO
        DA = DA+DONE
        AC = -DONE*AC
        AD = P
        P = AD*E-Q*F
        Q = Q*E+AD*F
  50  CONTINUE
C
  60  DA = Y*T+U*Z
      B = Y*U-T*Z
      C = Y*V+X*Z
      D = Y*X-V*Z
      RETURN
  70  DA = DZERO
      B = DPCE
      C = DZERO
      D = DZERO
      CALL ERROR (101)
      RETURN
C
C     ==================================================================
C
      END
*CHANGE
      SUBROUTINE CHANGE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CHANGE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CHANGE SIGNS OF COLS (C), (C), (C), ETC.
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
      IF (NARGS.GT.IZERO) GO TO 10
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  DO 30 I=1,NARGS
        K = I
        CALL ADRESS (K,J)
        IF (J.LT.IZERO) CALL ERROR (20)
        IF (NERROR.NE.IZERO) RETURN
        DO 20 N=1,NRMAX
          JJ = J + N - IONE
          RC(JJ) = -RC(JJ)
  20    CONTINUE
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CHSRAN
      SUBROUTINE CHSRAN (IRAN,KRAN,N,NU,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CHSRAN V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     GENERATE N CHISQUARE RANDOM NUMBERS IN X FOR NU DEGREES OF FREEDOM.
C
C        A CHISQUARE VARIATE WITH NU DEGREES OF FREEDOM
C           EQUALS THE SUM OF NU STANDARD NORMAL VARIATES SQUARED.
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
C
      INCLUDE 'WRKSCR.H'
C
      REAL             X(*)
      REAL             TERM(1)
      REAL             SUM
C
C     ==================================================================
C
      DO 20 I=1,N
        CALL NORRAN (IRAN,KRAN,NU,A)
        CALL SUMMAL (TERM,IZERO,SUM)
        DO 10 J=1,NU
          TERM(1) = A(J) ** 2
          CALL SUMMAL (TERM,-IONE,SUM)
  10    CONTINUE
        CALL SUMMAL (TERM,IONE,SUM)
        X(I) = SUM
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CMSEPA
      SUBROUTINE CMSEPA
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CMSEPA V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     L2 = 2,     SEPARATE
C     L2 = 3,     INSERT
C     L2 = 4,     MAXMIN
C
C     SEPARATE FROM COL (C) EVERY (N) ROW START WITH ROW (N) PUT IN (C)
C     INSERT IN (C) FROM (C) AT EVERY (N) ROW START AS (N) PUT IN (C)
C     MAXMIN X IN (C) Y IN (C), PUT XMAX (C) YMAX (C), XMIN (C) YMIN (C)
C
C               WRITTEN BY -
C                      CARLA MESSINA
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                  ORIGINAL VERSION -     JUNE, 1968.
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
      REAL             X1, X2, Y1, Y2
C
C     ==================================================================
C
      J    = NARGS
      ISTP = IONE
C
      GO TO (10,10,30,200), L2
C
  10  NARGS = NARGS + IONE
      J     = NARGS
      DO 20 I=2,NARGS
        IARGS(J) = IARGS(J-1)
        KIND(J)  = KIND(J-1)
        J        = J - IONE
  20  CONTINUE
C
  30  IF (KIND(2).EQ.IZERO) GO TO 40
      ISTP      = ITWO
      KIND(2)   = IZERO
      IARGS(2)  = IARGS(1)
C
  40  CALL CKIND (J)
      IF (J.NE.IZERO) GO TO 400
      IF (NARGS.NE.IFIVE) GO TO 410
      IF (IARGS(1).GT.NROW .OR. IARGS(1).LE.IZERO) GO TO 430
      M = IARGS(3)
      N = IARGS(4)
      DO 60 I=3,4
        IF (IARGS(I).LE.IZERO) GO TO 400
        IF (IARGS(I).LE.NROW)  GO TO 50
        IF (IARGS(I).GT.NROW)  GO TO 440
  50    IARGS(I) = IARGS(1)
  60  CONTINUE
C
      IF (L2.EQ.ITWO .AND. N.GE.NRMAX) GO TO 400
      CALL CHKCOL
      IF (NRMAX.LE.IZERO) GO TO 450
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      DO 70 I=1,NARGS
        IARGS(I) = IARGS(I) - IONE
 70   CONTINUE
C
      L = IARGS(5)
      IF (L2.LE.ITWO) GO TO 80
      IF (L2.GT.ITWO) GO TO 110
C
C     SEPARATE.
C
  80  DO 90 I=1,NRMAX
        J = IARGS(1) + I
        A(I) = RC(J)
  90  CONTINUE
C
      DO 100 K=N,NRMAX,M
        L = L + IONE
        RC(L) = A(K)
 100  CONTINUE
C
      RETURN
C
C     ..................................................................
C
C     INSERT.
C
 110  M  = M - IONE
      IF (M.LE.IZERO) GO TO 400
      N  = N - IONE
      IF (N.LE.IZERO) GO TO 400
      KA = IZERO
      I  = IARGS(1)
      DO 120 K=1,N
        I = I + IONE
        KA = KA + IONE
        A(KA) = RC(I)
 120  CONTINUE
C
      NN = KA + IARGS(1)
      MM = IARGS(2)
      DO 160 K=N,NRMAX,M
        KA = KA + IONE
        IF (ISTP.EQ.IONE) GO TO 130
        A(KA) = ARGS(2)
        GO TO 140
C
 130    MM = MM + IONE
        A(KA) = RC(MM)
 140    DO 150 LL=1,M
          KA = KA + IONE
          NN = NN + IONE
          A(KA) = RC(NN)
 150    CONTINUE
 160  CONTINUE
C
      I = IDIV (NRMAX-N,M,IND) + IONE
      IF (I+NRMAX.GT.NROW) GO TO 170
      NROLD = NRMAX
      NRMAX = NRMAX + I
      CALL ERROR (252)
      GO TO 180
C
 170  NROLD = NRMAX
      NRMAX = NROW
      CALL ERROR (252)
      CALL ERROR (219)
 180  DO 190 K=1,NRMAX
        L = L + IONE
        RC(L) = A(K)
 190  CONTINUE
      RETURN
C
C     ..................................................................
C
 200  IF (NARGS.NE.6) GO TO 420
      CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
      KA  = IZERO
      IUP = -IONE
      IF (NRMAX.LE.IZERO) GO TO 450
      IF (NRMAX.LE.ITWO) GO TO 460
      DO 210 K=1,NRMAX
        I = IARGS(1) + K - IONE
        J = IARGS(2) + K - IONE
        A(K) = RC(I)
        K2 = K + NRMAX
        A(K2) = RC(J)
 210  CONTINUE
      IXMAX = IARGS(3) - IONE
      IYMAX = IARGS(4) - IONE
      IXMIN = IARGS(5) - IONE
      IYMIN = IARGS(6) - IONE
      IF (NRMAX.GE.IFOUR) GO TO 280
      K2 = NRMAX + IONE
      IF (A(K2).EQ.A(K2+1)) GO TO 480
      IF (A(K2).GT.A(K2+1)) GO TO 220
      IF (A(K2+1)-A(K2+2)) 480,480,230
 220  IF (A(K2+1)-A(K2+2)) 240,480,480
 230  IUP = IUP + IONE
 240  IUP = IUP + IONE
      IF (A(1).EQ.A(2)) GO TO 250
      IF (A(1).EQ.A(3)) GO TO 250
      IF (A(2).NE.A(3)) GO TO 260
 250  KA = IONE
      GO TO 460
C
 260  CALL CMPARA (A(1),A(2),A(3),A(K2),A(K2+1),A(K2+2),X1,Y1)
      IF (IUP.GT.IZERO) GO TO 270
      RC(IXMIN+1) = X1
      RC(IYMIN+1) = Y1
      RETURN
C
C     ..................................................................
C
 270  RC(IXMAX+1) = X1
      RC(IYMIN+1) = Y1
      RETURN
C
C     ..................................................................
C
 280  I = NRMAX - ITWO
      DO 390 K=1,I
        IEQUAL = IONE
        K2 = K + NRMAX
        IF (A(K2).GT.A(K2+1)) GO TO 290
        IF (A(K2).EQ.A(K2+1)) GO TO 340
        IF (A(K2+1)-A(K2+2)) 390,390,300
 290    IF (A(K2+1)-A(K2+2)) 310,390,390
 300    IUP = IONE
        GO TO 320
C
 310    IUP = IZERO
 320    IF (A(K).EQ.A(K+1))   GO TO 330
        IF (A(K).EQ.A(K+2))   GO TO 330
        IF (A(K+1).NE.A(K+2)) GO TO 360
 330    KA = KA + IONE
        GO TO 390
C
 340    IF (K.LE.IONE) GO TO 390
        IEQUAL = ITWO
        IF (A(K-1).EQ.A(K))   GO TO 330
        IF (A(K-1).EQ.A(K+1)) GO TO 330
        IF (A(K-1).EQ.A(K+2)) GO TO 330
        IF (A(K2-1).GT.A(K2)) GO TO 350
        IF (A(K2-1).EQ.A(K2)) GO TO 390
        IF (A(K2+1)-A(K2+2)) 390,390,300
 350    IF (A(K2+1)-A(K2+2)) 310,390,390
 360    CALL CMPARA (A(K),A(K+1),A(K+2),A(K2),A(K2+1),A(K2+2),X1,Y1)
        IF (IEQUAL.LT.ITWO) GO TO 370
        CALL CMPARA (A(K-1),A(K),A(K+1),A(K2-1),A(K2),A(K2+1),X2,Y2)
        X1 = RHALF * (X1+X2)
        Y1 = RHALF * (Y1+Y2)
 370    IF (IUP.GT.IZERO) GO TO 380
        IUP = IZERO
        IXMIN = IXMIN + IONE
        IYMIN = IYMIN + IONE
        RC(IXMIN) = X1
        RC(IYMIN) = Y1
        GO TO 390
 380    IXMAX = IXMAX + IONE
        IYMAX = IYMAX + IONE
        RC(IXMAX) = X1
        RC(IYMAX) = Y1
 390  CONTINUE
      GO TO 460
C
C     ..................................................................
C
 400  CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
 410  IF (L2.LE.ITWO) NARGS = NARGS - IONE
 420  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 430  CALL ERROR (11)
      RETURN
C
C     ..................................................................
C
 440  CALL ERROR (16)
      RETURN
C
C     ..................................................................
C
 450  CALL ERROR (9)
      RETURN
C
C     ..................................................................
C
 460  IF (KA.LE.IZERO) GO TO 470
      CALL ERROR (220)
 470  IF (IUP.GE.IZERO) RETURN
 480  CALL ERROR (219)
      RETURN
C
C     ==================================================================
C
      END
*COALES
      SUBROUTINE COALES
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. COALES V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE ACOALESCE AND AAVERAGE INSTRUCTIONS.
C
C        L2 =  9,     ACOALESCE
C        L2 = 10,     AAVERAGE
C
C     FORMS OF INSTRUCTIONS ARE ...
C
C     ACOALESCE ON FIRST COL OF ARRAY (R),(C) SIZE (R)X(C), IN (R),(C)
C     AAVERAGE  ON FIRST COL OF ARRAY (R),(C) SIZE (R),(C), IN (R),(C)
C
C        OR
C
C     ACOALESCE ON (K) IN FIRST COL OF (R),(C) SIZE (R)X(C), IN (R),(C)
C     AAVERAGE  ON (K) IN FIRST COL OF (R),(C) SIZE (R)X(C), IN (R),(C)
C
C               WRITTEN BY -
C                      R. MCCLENON,
C                      NSRDS,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1969.
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
      REAL             DIV, TEMP, Y
      REAL             FDIV
C
C     ==================================================================
C
      Y  = RONE
      L2 = L2 - 8
      IF (NARGS.GT.6) GO TO 10
      IF (NARGS.LT.6) GO TO 200
      KL = IONE
      I = 6
      CALL CKIND (I)
      IF (I.GE.IONE) GO TO 210
      GO TO 30
  10  KL = ITWO
      DO 20 J=2,7
        IF (KIND(J).NE.IZERO) GO TO 210
  20  CONTINUE
C
      IF (NARGS.GE.8) GO TO 200
      IF (KIND(1).LE.IZERO) GO TO 210
      Y = ARGS(1)
  30  KL5 = KL + IFIVE
      DO 40 J=KL,KL5
        IF (IARGS(J).LE.IZERO) GO TO 210
  40  CONTINUE
C
      LROW = IARGS(KL+2)
      LCOL = IARGS(KL+3)
      KROW = IARGS(KL)
      KCOL = IARGS(KL+1)
      IF (KROW+LROW-NROW.GT.IONE) GO TO 220
      IF (KCOL+LCOL-NCOL.GT.IONE) GO TO 220
      MROW = IARGS(KL+4)
      MCOL = IARGS(KL+5)
      IF (MROW+LROW-NROW.GT.IONE) GO TO 220
      IF (MCOL+LCOL-NCOL.GT.IONE) GO TO 220
      IF (NERROR.NE.IZERO) RETURN
      N   = IZERO
      IF (LCOL.LE.IONE) GO TO 260
      IF (KL.LE.IONE) GO TO 90
C
      A(1) = Y
      DO 60 J=1,LROW
        I = KROW + J - IONE
        II  = NROW * (KCOL-IONE) + I
        IF (RC(II).NE.Y) GO TO 60
        N  = N + IONE
        DO 50 JJ=2,LCOL
          NSP = N + NROW * (JJ-ITWO) + IONE
          I = II + (JJ-IONE) * NROW
          A(NSP) = RC(I)
  50    CONTINUE
  60  CONTINUE
C
      IF (N.LE.IZERO) GO TO 240
      M = IONE
      DO 70 J=2,LCOL
        NSP = NROW * (J-ITWO) + ITWO
        CALL SUMMAL (A(NSP),N,TEMP)
        IF (N.EQ.IONE) TEMP = A(NSP)
        A(J) = TEMP
  70  CONTINUE
C
      DIV = RONE
      IF (L2.EQ.ITWO) DIV = N
      DO 80 JJ=2,LCOL
        A(JJ) = FDIV (A(JJ),DIV,IND)
  80  CONTINUE
      GO TO 170
C
  90  MAT = LCOL * LROW
      MAT2 = MAT + LROW
      IF (MAT2-NS.GT.LROW) GO TO 230
      M1 = MAT + IONE
      M2 = MAT + LROW
      DO 100 J=M1,M2
        A(J) = RZERO
 100  CONTINUE
C
      M = IZERO
      DO 160 J=1,LROW
        M1 = MAT + J
        IF (A(M1).NE.RZERO) GO TO 160
        K1 = LCOL * M + IONE
        K2 = LCOL * (M+IONE)
        K3 = K1 + IONE
        NA = MAT2
        N  = IZERO
        L  = NROW * (KCOL-IONE) + J + KROW - IONE
        Y  = RC(L)
        A(K1) = Y
        DO 110 JJ=J,LROW
          II = NROW * (KCOL-IONE) + KROW + JJ - IONE
          IF (RC(II).NE.Y) GO TO 110
          M1 = MAT + JJ
          NA = NA + IONE
          A(M1) = FLOAT(J)
          I  = II + NROW
          A(NA) = RC(I)
          N  = N + IONE
 110    CONTINUE
        IF (N.LE.IZERO) GO TO 160
        CALL SUMMAL (A(MAT2+1),N,TEMP)
        IF (N.EQ.IONE) TEMP = A(MAT2+1)
        A(K3) = TEMP
        IF (LCOL.EQ.ITWO) GO TO 140
        DO 130 JJC=3,LCOL
          NA = MAT2
          DO 120 JJ=J,LROW
            M1 = MAT + JJ
            IF (A(M1).NE.FLOAT(J)) GO TO 120
            I = NROW * (KCOL+JJC-ITWO) + KROW + JJ - IONE
            NA = NA + IONE
            A(NA) = RC(I)
 120      CONTINUE
          K4 = LCOL * M + JJC
          CALL SUMMAL (A(MAT2+1),N,TEMP)
          IF (N.EQ.IONE) TEMP = A(MAT2+1)
          A(K4) = TEMP
 130    CONTINUE
 140    M = M + IONE
        DIV = RONE
        IF (L2.EQ.ITWO) DIV = N
        DO 150 K=K3,K2
          A(K) = FDIV (A(K),DIV,IND)
 150    CONTINUE
 160  CONTINUE
C
 170  DO 190 J=1,M
        DO 180 JJ=1,LCOL
          I = LCOL * (J-IONE) + JJ
          II = NROW * (MCOL-ITWO+JJ) + J + MROW - IONE
          RC(II) = A(I)
 180    CONTINUE
 190  CONTINUE
      RETURN
C
C     ..................................................................
C
 200  CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 210  CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 220  CALL ERROR (17)
      RETURN
C
C     ..................................................................
C
 230  CALL ERROR (23)
      RETURN
C
C     ..................................................................
C
 240  CALL ERROR (203)
      M = IONE
      II = NROW * (MCOL-IONE) + MROW
      RC(II) = ARGS(1)
      DO 250 J=2,LCOL
        II     = II + NROW
        RC(II) = RZERO
 250  CONTINUE
      RETURN
C
C     ..................................................................
C
 260  CALL ERROR (205)
      RETURN
C
C     ==================================================================
C
      END
*COMPIL
      SUBROUTINE COMPIL (IA,NUMCHA,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. COMPIL V 7.00  9/12/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE INTERPRETS AND EVALUATES
C              A FORTRAN MATHEMATICAL FUNCTION EXPRESSION
C              OF THE FORM Y=F(X)
C              AT EACH OF THE N VALUES OF THE VECTOR X.
C              THIS SUBROUTINE IS TYPICALLY
C              ENTERED WITH TWO PASSES--
C              THE FIRST PASS ANALYZES THE STRING
C              AND HAS AS ITS OUTPUT THE HOLLARITH
C              NAMES OF THE VARIOUS PARAMETERS.
C              THESE NAMES ARE OUTPUTTED IN THIS FIRST PASS
C              AS ELEMENTS IN THE VECTOR IPARN.
C              THE SECOND PASS USES INPUT PARAMETER VALUES
C              (INPUTTED IN THE VECTOR PARAM)
C              TO ACTUALLY EVALUATE THE FUNCTION
C              FOR EACH OF THE N VALUES IN THE VECTOR X
C              AND HAS AS ITS OUTPUT THE CORRESPONDING
C              N FUNCTION VALUES (OUTPUTTED IN THE VECTOR Y).
C
C     INPUT  ARGUMENTS -- IA     = THE INTEGER VECTOR WHICH CONTAINS
C                                  THE HOLLARITH CHARACTERS WHICH
C                                  MAKE UP THE LINE OF FORTRAN CODE.
C                                  THIS VECTOR CONTAINS THE STRING
C                                  TO BE OPERATED ON, INTERPRETED,
C                                  AND EVALUATED.
C                      -- NUMCHA = THE INTEGER VALUE WHICH
C                                  DEFINES THE NUMBER OF CHARACTERS IN
C                                  IA.  NUMCHA DEFINES THE LENGTH OF THE
C                                  HOLLARITH STRING TO BE OPERATED ON,
C                                  INTERPRETED, AND EVALUATED.
C                      -- N      = THE INTEGER NUMBER OF VALUES
C                                  IN THE VECTOR X.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER           SUBROUTINES NEEDED--EVAL
C     FORTRAN LIBRARY SUBROUTINES NEEDED--(ALL IN EVAL)
C                                         SQRT
C                                         EXP
C                                         ALOG
C                                         ALOG10
C                                         SIN
C                                         COS
C                                         ATAN
C                                         ATAN2
C                                         TANH
C                                         ABS
C                                         AINT
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     NOTE--THIS SUBROUTINE ALLOWS ONE TO PERFORM
C           INTERACTIVE FUNCTION EVALUATIONS.
C     REFERENCES--NONE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 COMPUTING AND APPLIED MATHEMATICS LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2845
C     ORIGINAL VERSION--NOVEMBER  1976.
C     UPDATED         --JUNE  1977.
C     DATE--NOVEMBER 1, 1976
C
C               ADAPTED TO OMNITAB BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GATITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION-        MAY, 1978.
C                   CURRENT VERSION - SEPTEMBER, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IA(*)
      DIMENSION IB(80)
      DIMENSION IR(80)
      DIMENSION IWORD(18,80)
      DIMENSION ITYPE(80)
      DIMENSION IW2(80)
      DIMENSION ITYPEH(80)
      DIMENSION IW2HOL(80)
      DIMENSION IWW2(80)
      DIMENSION IWW2HL(80)
C
      COMMON /ABCDEF/ LA(74)
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
      REAL             W2(80), W2HOLD(80)
      REAL             Y
C
C     ................................................................
C
      CHARACTER LA*1
      CHARACTER IA*1, IWORD*1, IB*1, IR*1
      CHARACTER IW2*2, IW2HOL*2, LALA*2
C
C     ................................................................
C
      EQUIVALENCE (IWW2HL(1),A(1681)), ( ITYPE(1),A(1761))
      EQUIVALENCE (    W2(1),A(1921))
      EQUIVALENCE (ITYPEH(1),A(2001))
      EQUIVALENCE (W2HOLD(1),A(2161))
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
      DO 20 I=1,80
        DO 10 J=1,18
          IWORD(J,I) = LA(45)
  10    CONTINUE
        IW2(I)    = LA(45)
        W2(I)     = RZERO
        ITYPE(I)  = IZERO
        IW2HOL(I) = LA(45)
        W2HOLD(I) = RZERO
        ITYPEH(I) = IZERO
        IWW2(I)   = IZERO
        IWW2HL(I) = IZERO
  20  CONTINUE
C
C     STEP 1 ...
C        OPERATE ON THE VECTOR IA(.).
C        SQUEEZE OUT ALL BLANKS.
C        OUTPUT THE VECTOR IB(.).
C
      K = IZERO
      DO 30 I=1,NUMCHA
        IF (IA(I).EQ.LA(45)) GO TO 30
        IF (IA(I).EQ.LA(47)) GO TO 35
        K = K + IONE
        IB(K) = IA(I)
  30  CONTINUE
C
  35  NCTOT = K
      IF (NCTOT.GE.ITHRE) GO TO 40
C
C     NUMBER OF CHARACTERS IN EQUATION IS LESS THAN 3.
C
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
C
C     STEP 2 ...
C        OPERATE ON THE VECTOR IB(.).
C        DETERMINE THE NUMBER OF CHARACTERS FOR LEFT-HAND SIDE.
C        OUTPUT THEM INTO THE VECTOR IL(.).
C
  40  DO 50 I=1,NCTOT
        IF (IB(I).EQ.LA(46)) GO TO 60
  50  CONTINUE
      CALL ERROR (43)
      RETURN
C
C     ..................................................................
C
  60  NCL  = I - IONE
      NARG = IONE
      IF (NCL.EQ.IZERO) NARG = IZERO
C
C     STEP 3 ...
C        OPERATE ON THE VECTOR IB(.).
C        DETERMINE THE NUMBER OF CHARACTERS FOR RIGHT-HAND SIDE.
C        OUTPUT THEM INTO THE VECTOR IR(.).
C
      K = IZERO
      NCLP2 = NCL + ITWO
      NCR = NCTOT - NCLP2 + IONE
      DO 80 I=NCLP2,NCTOT
        K = K + IONE
        IR(K) = IB(I)
        DO 75 ILA=49,74
          IF (IR(K) .NE. LA(ILA)) GO TO 75
          IR(K) = LA(ILA-38)
          GO TO 80
  75    CONTINUE    
  80  CONTINUE
C
C     STEP 4--
C        OPERATE ON THE VECTOR IR(.).
C        ANALYZE THE RIGHT-HAND SIDE.
C        DETERMINE THE NUMBER OF DIFFERENT LOGICAL COMPONENTS.
C           1. NUMBER (CONSISTING OF 0,1,2,...,9 OR .)
C           2. X VARIABLE
C           3. OPERATION (+   -   *   /   **)
C           4. PARENTHESES (   (   OR   )    )
C           5. LIBRARY FUNCTION (ALOG   EXP   ETC.)
C           6. PARAMETER (ANYTHING NOT ABOVE)
C        CHECK FOR SYNTAX ERRORS.
C           OUTPUT THE DISTINCT COMPONENTS INTO THE MATRIX IWORD(.,.).
C           OUTPUT THE TYPE COMPONENT INTO ITYPE(.).
C
      NW     = IZERO
      I      = IONE
      ISUBAR = IONE
  90  IPERD  = IZERO
      IP1 = I + IONE
      IP2 = I + ITWO
      IP3 = I + ITHRE
      IP4 = I + IFOUR
      IP5 = I + IFIVE
C
C     CHECK FOR NUMBERS. IPERD=0 IF NUMBER IS A COLUMN (I.E. NO PERIOD).
C        IPERD = 1 IF NUMBER IS A CONSTANT (I.E. HAS A DECIMAL PT.).
C
      DO 100 II=1,10
        IF (IR(I).EQ.LA(II)) GO TO 120
 100  CONTINUE
C
      IF (IR(I).NE.LA(38)) GO TO 110
      IPERD = IONE
      GO TO 120
C
 110  IF (IR(I).EQ.LA(40)) GO TO 160
      IF (IR(I).EQ.LA(39)) GO TO 160
      IF (IR(I).EQ.LA(41)) GO TO 160
      IF (IR(I).EQ.LA(37)) GO TO 160
C
      IF (IR(I).EQ.LA(42)) GO TO 170
      IF (IR(I).EQ.LA(43)) GO TO 180
      NW = NW + IONE
C
C     SQRT.
C
      ITYPE(NW) = 6
      IF (IR(I  ).EQ.LA(29) .AND. IR(IP1).EQ.LA(27) .AND.
     1    IR(IP2).EQ.LA(28) .AND. IR(IP3).EQ.LA(30)) GO TO 200
C
C     EXP.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(15) .AND. IR(IP1).EQ.LA(34) .AND.
     1    IR(IP2).EQ.LA(26)) GO TO 190
C
C     ALOG10.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(22) .AND.
     1    IR(IP2).EQ.LA(25) .AND. IR(IP3).EQ.LA(17) .AND.
     2    IR(IP4).EQ.LA( 2) .AND. IR(IP5).EQ.LA( 1)) GO TO 220
C
C     ALOG.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(22) .AND.
     1    IR(IP2).EQ.LA(25) .AND. IR(IP3).EQ.LA(17)) GO TO 200
C
C     SIN.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(29) .AND. IR(IP1).EQ.LA(19) .AND.
     1    IR(IP2).EQ.LA(24)) GO TO 190
C
C     COS.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(13) .AND. IR(IP1).EQ.LA(25) .AND.
     1    IR(IP2).EQ.LA(29)) GO TO 190
C
C     ATAN2.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(30) .AND.
     1    IR(IP2).EQ.LA(11) .AND. IR(IP3).EQ.LA(24) .AND.
     2    IR(IP4).EQ.LA( 3)) GO TO 210
C
C     ATAN.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(30) .AND.
     1    IR(IP2).EQ.LA(11) .AND. IR(IP3).EQ.LA(24)) GO TO 200
C
C     TANH.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(30) .AND. IR(IP1).EQ.LA(11) .AND.
     1    IR(IP2).EQ.LA(24) .AND. IR(IP3).EQ.LA(18)) GO TO 200
C
C     ABS.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(12) .AND.
     1    IR(IP2).EQ.LA(29)) GO TO 190
C
C     AMOD.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(23) .AND.
     1    IR(IP2).EQ.LA(25) .AND. IR(IP3).EQ.LA(14)) GO TO 200
C
C     SIGN.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(29) .AND. IR(IP1).EQ.LA(19) .AND.
     1    IR(IP2).EQ.LA(17) .AND. IR(IP3).EQ.LA(24)) GO TO 200
C
C     AINT.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(19) .AND.
     1    IR(IP2).EQ.LA(24) .AND. IR(IP3).EQ.LA(30)) GO TO 200
C
C     AMAX1.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(23) .AND.
     1    IR(IP2).EQ.LA(11) .AND. IR(IP3).EQ.LA(34) .AND.
     2    IR(IP4).EQ.LA( 2)) GO TO 210
C
C     AMIN1.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(11) .AND. IR(IP1).EQ.LA(23) .AND.
     1    IR(IP2).EQ.LA(19) .AND. IR(IP3).EQ.LA(24) .AND.
     2    IR(IP4).EQ.LA( 2)) GO TO 210
C
C     DIM.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(14) .AND. IR(IP1).EQ.LA(19) .AND.
     1    IR(IP2).EQ.LA(23)) GO TO 190
C
C     LOGTEN.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(22) .AND. IR(IP1).EQ.LA(25) .AND.
     1    IR(IP2).EQ.LA(17) .AND. IR(IP3).EQ.LA(30) .AND.
     2    IR(IP4).EQ.LA(15) .AND. IR(IP5).EQ.LA(24)) GO TO 220
C
C     LOG.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(22) .AND. IR(IP1).EQ.LA(25) .AND.
     1    IR(IP2).EQ.LA(17)) GO TO 190
C
C     MAX.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(23) .AND. IR(IP1).EQ.LA(11) .AND.
     1    IR(IP2).EQ.LA(34)) GO TO 190
C
C     MIN.
C
      ITYPE(NW) = ITYPE(NW) + IONE
      IF (IR(I  ).EQ.LA(23) .AND. IR(IP1).EQ.LA(19) .AND.
     1    IR(IP2).EQ.LA(34)) GO TO 190
C
      GO TO 230
C
 120  NW = NW + IONE
      ITYPE(NW) = IPERD
      JMIN = I
      J = I
 130  J = J + IONE
      IF (J.GT.NCR) GO TO 150
C
C     CHECK FOR END OF CONSTANT OR COLUMN NUMBER.
C
      DO 140 II=1,10
        IF (IR(J).EQ.LA(II)) GO TO 130
 140  CONTINUE
C
      IF (IR(J).NE.LA(38)) GO TO 150
      IPERD = IONE
      GO TO 130
C
 150  JMAX = J - IONE
C
C     IF COLUMN NUMBER ITYPE(NW) = 0
C     IF CONSTANT ITYPE(NW) = 1
C
      ITYPE(NW) = IPERD
      GO TO 270
C
 160  NW = NW + IONE
      ITYPE(NW) = ITHRE
      JMIN = I
      JMAX = I
      IP1  = I + IONE
      IF (IR(I).EQ.LA(41) .AND. IR(IP1).EQ.LA(41)) JMAX = IP1
      GO TO 270
C
 170  NW = NW + IONE
      ITYPE(NW) = IFOUR
      JMIN = I
      JMAX = I
      GO TO 270
C
 180  NW = NW + IONE
      ITYPE(NW) = IFIVE
      JMIN = I
      JMAX = I
      GO TO 270
C
 190  IF (IR(IP3).NE.LA(42)) GO TO 230
      JMIN = I
      JMAX = I + ITWO
      GO TO 270
C
 200  IF (IR(IP4).NE.LA(42)) GO TO 230
      JMIN = I
      JMAX = I + ITHRE
      GO TO 270
C
 210  IF (IR(IP5).NE.LA(42)) GO TO 230
      JMIN = I
      JMAX = I + IFOUR
      GO TO 270
C
 220  IF (IR(IP5+1).NE.LA(42)) GO TO 230
      JMIN = I
      JMAX = I + IFIVE
      GO TO 270
C
 230  DO 250 IJI=I,NCR
        DO 240 IJX=37,48
          IF (IR(IJI).EQ.LA(IJX)) GO TO 260
 240    CONTINUE
 250  CONTINUE
C
      IJI = NCR + IONE
 260  J = IJI - IONE
      J = J + IONE
      JMIN = I
      JMAX = J - IONE
      ITYPE(NW) = IZERO
 270  K = IZERO
      DO 280 J=JMIN,JMAX
        K = K + IONE
        IWORD(K,NW) = IR(J)
 280  CONTINUE
C
      I = JMAX
      I = I + IONE
      IF (I.LE.NCR) GO TO 90
      IF (NW.EQ.IONE) GO TO 310
      DO 300 I=1,NW
        IP1 = I + IONE
        IF (ITYPE(I).GE.6 .AND. ITYPE(I).LT.26 .AND.
     1      ITYPE(IP1).NE.IFOUR) GO TO 290
        GO TO 300
 290    CALL ERROR (7)
        RETURN
 300  CONTINUE
C
 310  IF (ITYPE(NW).EQ.ITHRE) GO TO 320
      IF (ITYPE(NW).GE.6) GO TO 320
      GO TO 330
C
 320  CALL ERROR (8)
      RETURN
C
C     ..................................................................
C
C     STEP 5 ...
C        OPERATE ON THE MATRIX IWORD(.,.).
C        CONVERT THE NUMBERS TO FLOATING POINT VALUES.
C        SET THE X TO AN DUMMY VALUE OF 0.0 FOR THE TIME BEING.
C        CONVERT THE OPERATIONS INTO A 1-WORD REPRESENTATION.
C        'CONVERT' THE PARENTHESES INTO A 1-WORD REPRESENTATION.
C        CONVERT THE COEFFICIENTS TO COEFFICIENT VALUES.
C        CONVERT THE LIBRARY FUNCTIONS INTO A 1-WORD REPRESENTATION.
C        SAVE THE CONTENTS OF ITYPE, IW2, AND W2 IN
C        ITYPEH, IW2HOL, AND WHOLD FOR LATER USE
C        IN REDEFINING ITYPE, IW2, AND W2 FOR EACH NEW X VALUE.
C        OUTPUT THE VECTORS IW2 AND W2.
C        OUTPUT THE VECTORS IW2HOL, W2HOLD, AND ITYPEH.
C
 330  DO 390 I=1,NW
        IF (ITYPE(I).EQ.IONE) GO TO 340
        IF (ITYPE(I).EQ.ITHRE) GO TO 350
        IF (ITYPE(I).EQ.IFOUR .OR. ITYPE(I).EQ.IFIVE) GO TO 360
        IF (ITYPE(I).EQ.IZERO) GO TO 370
        IF (ITYPE(I).GE.6 .AND. ITYPE(I).LE.26) GO TO 380
        CALL ERROR (3)
        RETURN
 340    ISUBAR = ISUBAR + IONE
        W2(I)  = ABS (ARGS(ISUBAR))
        GO TO 390
C
 350    IW2(I) = IWORD(1,I)
        IF (IWORD(1,I).EQ.LA(41) .AND. IWORD(2,I).EQ.LA(41))
     1      IW2(I) = LALA
        GO TO 390
C
 360    IW2(I) = IWORD(1,I)
        GO TO 390
C
 370    IW2(I) = LA(45)
        ISUBAR = ISUBAR + IONE
        IWW2(I) = IARGS(ISUBAR)
        GO TO 390
C
 380    IWW2(I) = ITYPE(I)
        GO TO 390
C
 390  CONTINUE
      NWHOLD = NW
C
      DO 400 I=1,NW
        ITYPEH(I) = ITYPE(I)
        IW2HOL(I) = IW2(I)
        W2HOLD(I) = W2(I)
        IWW2HL(I) = IWW2(I) 
 400  CONTINUE
C
C     STEP 6 ...
C        OPERATE ON THE W2(.) AND IW2(.) VECTORS.  FIRST MAKE
C        SURE THAT THE NUMBER OF LEFT AND RIGHT PARENTHESES ARE
C        THE SAME (STEP 6 THEN SETS UP A LARGE DO LOOP
C        WHICH GOES THROUGH ALL OF THE VALUES OF THE X VECTOR
C        AND GENERATES CORRESPONDING VALUES OF THE Y VECTOR.)
C        FOR A GIVEN X VALUE, IT EVALUATES THE FUNCTION
C        BY FIRST SEEKING THE INNERMOST PARENTHESES
C        (BY SEARCHING FOR THE FIRST REMAINING RIGHT PARENTHESS).
C        AND THEN EVALUATING ALL SUCH PARENTHETICAL EXPRESSIONS--
C        WORKING FROM THE INNERMOST OUT.
C        AFTER EVALUATING A PARENTHESES PAIR,
C        THE ENTIRE PARENTHESES GROUP (PARENTHESES INCLUDED)
C        IS REPLACED BY THE SCALAR ANSWER.
C        THE IW2, W2, AND ITYPE VECTORS ARE SQUEEZED ACCORDINGLY
C        (IN THE SUBROUTINE EVAL).
C        SINCE THE VECTORS IW2, W2, AND ITYPE ARE ALTERED (SQUEEZED)
C        FOR EACH X VALUE, THEY MUST BE REDEFINED FROM THE SAVED
C        VALUES IN IW22, W22, AND ITYPE2 FOR EACH NEW X VALUE.
C        THE ABOVE SQUEEZING OPERATION IS REPEATED
C        FOR EACH PARENTHESES PAIR UNTIL ALL PARENTHESES
C        ARE GONE AND WE REMAIN ONLY WITH THE FINAL ANSWER.
C        FOR EACH VALUE X(.) OF THE INPUT X VECTOR,
C        OUTPUT THE CORRESPONDING VALUE Y(.) OF
C        THE DESIRED OUTPUT VECTOR.
C        FOR A GIVEN VALUE X(.), THE CORRESPONDING
C        COMPUTED Y(.) WILL BE THE EVALUATED VALUE OF
C        THE RIGHT-HAND SIDE OF THE SPECIFIED EQUATION Y = F(X).
C
      NLP = IZERO
      NRP = IZERO
      DO 410 I=1,NW
        IF (ITYPE(I).EQ.IFOUR) NLP = NLP + IONE
        IF (ITYPE(I).EQ.IFIVE) NRP = NRP + IONE
 410  CONTINUE
      IF (NLP.EQ.NRP) GO TO 420
      CALL ERROR (44)
      RETURN
C
C     ..................................................................
C
 420  DO 430 I = 1,NW
        IF (ITYPE(I).LE.IONE) NARG = NARG + IONE
 430  CONTINUE
C
C     CHECK IF NO. OF ARGUMENTS IN EQUATION CORRESPONDS WITH NARGS.
C
      IF (NARG.EQ.NARGS) GO TO 440
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
 440  DO 550 II=1,N
        NW = NWHOLD
        DO 450 I=1,NW
          ITYPE(I) = ITYPEH(I)
          IW2(I)   = IW2HOL(I)
          W2(I)    = W2HOLD(I)
          IWW2(I)  = IWW2HL(I)
 450    CONTINUE
 460    DO 470 I=1,NW
          IF (ITYPE(I).EQ.IFIVE) GO TO 480
 470    CONTINUE
        ISTOP  = NW + IONE
        ISTART = IZERO
        GO TO 510
 480    ISTOP = I
        IREV  = ISTOP
        DO 490 I=1,ISTOP
          IREV = ISTOP - I + IONE
          IF (ITYPE(IREV).EQ.IFOUR) GO TO 500
 490    CONTINUE
 500    ISTART = IREV
 510    ISTAP1 = ISTART + IONE
        ISTOM1 = ISTOP - IONE
        CALL EVAL (IW2,IWW2,W2,ITYPE,ISTAP1,ISTOM1,Y,II-IONE,IND)
        IF (IND.NE.IZERO) RETURN
        IF (ISTART.LE.IZERO) GO TO 540
        W2(ISTART) = Y
        ITYPE(ISTART) = 27
        IF (NW.EQ.IONE) GO TO 540
        ISTOPP = ISTOP + IONE
        J = ISTART
        IF (ISTOPP.GT.NW) GO TO 530
        DO 520 I=ISTOPP,NW
          J        = J + IONE
          IW2(J)   = IW2(I)
          IWW2(J)  = IWW2(I)
          W2(J)    = W2(I)
          ITYPE(J) = ITYPE(I)
 520    CONTINUE
 530    NW = J
        GO TO 460
C
 540    KSTORE = IARGS(1) + II - IONE
        RC(KSTORE) = Y
 550  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*COMPLX
      SUBROUTINE COMPLX
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. COMPLX V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CADD (E),(E), TO (E),(E) AND PUT IN COLUMNS (C),(C)
C     CSUBTRACT (E),(E), FROM (E),(E) AND PUT IN COLUMNS (C),(C)
C     CMULTIPLY (E),(E) BY (E),(E) AND PUT IN COLUMNS (C),(C)
C     CDIVIDE (E),(E) BY (E),(E) AND PUT IN COLUMNS (C),(C)
C     CRECTANGULAR R IN (E) THETA IN (E), PUT X IN COL (C), Y IN COL (C)
C     CPOLAR OF X IN (E) Y IN (E) PUT R IN COL (C) THETA IN COL (C)
C
C     FIRST ARGUMENT OF EACH PAIR IS REAL, SECOND IS IMAGINARY
C
C     VALUES OF L2 ARE ***
C      1=CADD,    2=CSUB,    3=CMULT,   4=CDIV,    5=CRECTAN  6=CPOLAR
C
C     NARGS = 6 FOR ADD,SUB,MULT,DIV
C     NARGS = 4 FOR CRECTANGULAR AND CPOLAR
C
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1970.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION KK(6)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
      INCLUDE 'WRKSCR.H'
C
      REAL             X1, X2
      REAL             FDPCON
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION D(5)
      DOUBLE PRECISION X, Y
      DOUBLE PRECISION FDCOS, FDDIV, FDSIN, FDSQRT
      DOUBLE PRECISION DATAN2
C
      EQUIVALENCE (I1,IARGS(1)), (I2,IARGS(2)), (I3,IARGS(3))
      EQUIVALENCE (I4,IARGS(4)), (I5,IARGS(5)), (I6,IARGS(6))
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.NE.6 .AND. L2.LT.IFIVE) CALL ERROR (10)
      IF (NARGS.NE.IFOUR .AND. L2.GT.IFOUR) CALL ERROR (10)
      IF (KIND(NARGS).NE.IZERO .AND. KIND(NARGS-1).NE.IZERO)
     1     CALL ERROR (20)
      IF (NRMAX.EQ.IZERO) CALL ERROR (9)
      DO 10 II=1,NARGS
        I = II
        KK(I) = IONE
        CALL ADRESS (I,IARGS(I))
        IF (IARGS(I).GE.IZERO) GO TO 10
        KK(I) = IZERO
        IARGS(I) = I
  10  CONTINUE
C
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      JJ = IARGS(NARGS) + NRMAX - IONE
      IF (L2.GT.IFOUR) GO TO 110
      DO 100 I=I6,JJ
        DO 20 J=1,4
          MM = IARGS(J)
          D(J) = RC(MM)
          IF (KK(J).EQ.IZERO) D(J) = ARGS(J)
  20    CONTINUE
        GO TO (30,40,50,60), L2
C
C       CADD
C
  30    X = D(1) + D(3)
        Y = D(2) + D(4)
        GO TO 90
C
C       CSUBTRACT
C
  40    X = D(3) - D(1)
        Y = D(4) - D(2)
        GO TO 90
C
C       CMULTIPLY
C
  50    X = D(1)*D(3) - D(2)*D(4)
        Y = D(1)*D(4) + D(3)*D(2)
        GO TO 90
C
C       CDIVIDE
C
C       ZERO RETURNED IF DIVISION BY ZERO, DIAGNOSTIC GIVEN. ERROR(104).
C
  60    D(5) = D(3)**2 + D(4)**2
        IF (D(5)-DZERO) 70,70,80
  70    CALL ERROR (106)
        X = DZERO
        Y = DZERO
        GO TO 90
  80    X = FDDIV (D(1)*D(3)+D(2)*D(4),D(5),IND)
        Y = FDDIV (D(3)*D(2)-D(1)*D(4),D(5),IND)
  90    RC(I5) = FDPCON (X)
        RC(I) = FDPCON (Y)
        I1 = I1 + KK(1)
        I2 = I2 + KK(2)
        I3 = I3 + KK(3)
        I4 = I4 + KK(4)
        I5 = I5 + KK(5)
 100  CONTINUE
      RETURN
C
C     CRECTANGULAR AND CPOLAR
C
 110  MM = L2 - IFOUR
      DO 220 I=I4,JJ
        D(1) = RC(I1)
        D(2) = RC(I2)
        IF (KK(1).EQ.IZERO) D(1) = ARGS(1)
        IF (KK(2).EQ.IZERO) D(2) = ARGS(2)
        X1 = FDPCON ( D(1) )
        X2 = FDPCON ( D(2) )
        IF (MM.EQ.ITWO) GO TO 150
C
C       CRECTANGULAR - R,THETA TO X,Y
C
        IF (X1) 140,120,140
 120    X = DZERO
 130    Y = DZERO
        GO TO 210
 140    X = D(1)*FDCOS(D(2))
        Y = D(1)*FDSIN(D(2))
        GO TO 210
C
C       CPOLAR    X,Y TO R,THETA
C
 150    IF (X2) 180,160,180
 160    IF (X1) 170,120,170
C
C       Y=0, X NE 0
C
 170    X = DABS(D(1))
        GO TO 130
 180    IF (X1) 200,190,200
C
C       X=0, Y NE 0
C
 190    X = DABS(D(2))
C
C       IF X=0.0, THEN THETA=HALFPI*SIGN(Y)
C
        Y = SIGN(HALFPI,X2)
        GO TO 210
 200    X = FDSQRT(D(1)**2+D(2)**2)
        Y = DATAN2(D(2),D(1))
 210    RC(I3) = FDPCON(X)
        RC(I) = FDPCON(Y)
        I1 = I1 + KK(1)
        I2 = I2 + KK(2)
        I3 = I3 + KK(3)
 220  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*CONFEL
      SUBROUTINE CONFEL (ISUBHC,KPTS,ISUBX,N,VARNCE,WTS,CWTS,IWTS,SU)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CONFEL V 7.00  5/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTES THE VALUES A, B, C AND F OF THE ELLIPSE
C       A*X**2 + B*X*Y + C*Y**2 + F = 0,
C
C       CALLS ELLIPSE TO COMPUTE THE POINTS ON THE ELLIPSE
C       AND CALLS PLOTCE TO PLOT ELLIPSE.
C
C     INPUT ...
C
C       ISUBHC  STARTING SUBSCRIPT OF COEFFICIENTS IN ARRAY A(.).
C       KPTS    NUMBER OF POINTS COMPUTED FOR THE ELLIPSE.
C       ISUBX   STARTING SUBSCRIPT OF X IN ARRAY A(.).
C       N       NUMBER OF X'S IN VECTOR X.
C       VARNCE  RESIDUAL STANDARD DEVIATION.
C       WTS     A VECTOR OF WEIGHTS IF IWTS IS GREATER THAN 0.
C       CWTS    VALUE FOR WEIGHTS IF IWTS = 0.
C       IWTS    = 0, WEIGHTS ARE CONSTANT AND = CWTS, IF
C               GREATER THAN 0, WEIGHTS ARE A VECTOR.
C       SU      TOTAL NUMBER OF NON-ZERO WEIGHTS.
C
C     STRUCTURE CHART ...
C
C                      ..........
C                      . CONFEL .
C                      ..........
C                          .
C                          .
C           ..............................................
C           .                    .                       .
C           .                    .                       .
C     ..........            ...........             ..........
C     . FNUALF .            . PLOTCE  .             . ELIPSE .
C     ..........            ...........             ..........
C                                .
C                                .
C                         ...............
C                         .             .
C                         .             .
C                    ..........     ..........
C                    . SCALE2 .     .  SORT  .
C                    ..........     ..........
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
C                   CURRENT VERSION -      MAY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             WTS(*)
      REAL             VARNCE, CWTS, SU
      REAL             XX(1)
      REAL             ALPHA, FALPHA, RSQNUM, SUMWTS, SUMWX, SUMXSQ
      REAL             WW, XYCOEF, X2COEF, Y2COEF
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ALPHA / .05 /
C
C     ==================================================================
C
      KTPTS = KPTS * IFOUR
      NSU = SU + RHALF
      SUMWTS = FLOAT(N) * CWTS
      IF (IWTS.NE.IZERO) CALL SUMMAL (WTS,N,SUMWTS)
      IF (N.EQ.IONE) SUMWTS = WTS(1)
      X2COEF = SUMWTS
      CALL SUMMAL (XX,IZERO,SUMWX)
      WW = RZERO
      IF (IWTS.EQ.IZERO) WW = CWTS
      JSUBX = ISUBX
      DO 10 I=1,N
        IF (IWTS.NE.IZERO) WW = WTS(I)
        XX(1) = RC(JSUBX) * WW
        JSUBX = JSUBX + IONE
        CALL SUMMAL (XX,-IONE,SUMWX)
  10  CONTINUE
C
      CALL SUMMAL (XX,IONE,SUMWX)
      XYCOEF = RTWO * SUMWX
      CALL SUMMAL (XX,IZERO,SUMXSQ)
      JSUBX = ISUBX
      DO 20 I=1,N
        IF (IWTS.NE.IZERO) WW = WTS(I)
        XX(1) = WW * RC(JSUBX)**2
        JSUBX = JSUBX + IONE
        CALL SUMMAL (XX,-IONE,SUMXSQ)
  20  CONTINUE
C
      CALL SUMMAL (XX,IONE,SUMXSQ)
      Y2COEF = SUMXSQ
C
C     MOVE COEFFICIENTS TO BEGINNING OF SCRATCH AREA.
C
      A(1) = A(ISUBHC)
      A(2) = A(ISUBHC+1)
C
C     COMPUTE F(ALPHA) FOR (N-2) DEGREES OF FREEDOM.
C
      NSU = SU + RHALF
      CALL FNUALF (NSU-ITWO,ALPHA,FALPHA)
C
C     COMPUTE COORDINATES FOR ELLIPSE.
C
      RSQNUM = RTWO * VARNCE * FALPHA
      JSUBY = ITHRE
      JSUBX = KTPTS + ITHRE
      XCNTER = A(1)
      YCNTER = A(2)
      CALL ELIPSE (X2COEF,XYCOEF,Y2COEF,RSQNUM,XCNTER,YCNTER,KPTS,
     1     A(JSUBX),A(JSUBY))
      CALL PLOTCE (KTPTS)
      RETURN
C
C     ==================================================================
C
      END
*CONPG1
      SUBROUTINE CONPG1 (NR,NC,AGSUMR,AX,SAVCO,ARWSUM,ACLSUM,E,D,FTD)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. CONPG1 V 7.00  4/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT THE FIRST PAGE OF CONTINGENCY TABLE ANALYSIS.
C
C     NUMBER OF ROWS MUST BE LESS THAN 1000.
C
C               WRITTEN BY -
C                      RUTH N. VARNER
C               REVISED BY -
C                      DAVID HOGBEN
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
      DIMENSION LINEPR (120), ISAVCO(20)
      DIMENSION SAVCO(*)
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
C                   ***   TYPE STATEMENTS   ***
C
      REAL             AX(NR,NC), ACLSUM(*), ARWSUM(*)
      REAL             D(NR,NC), E(NR,NC), FTD(NR,NC)
      REAL             AGSUMR
      REAL             TOTMAX, X
      REAL             FLOG10
C
C     ..................................................................
C
      CHARACTER LA*1
      CHARACTER LINEPR*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NX    / 25 /
      DATA NWROW /  7 /
      DATA NLTPR /  5 /
C
C     ==================================================================
C
      CALL PAGE (4)
C
C     DETERMINE PRINTING CONSTANTS.
C
      NLPP = 60
C     IF IN DEMAND MODE SET NLPP EQUAL TO LENGTH.
C
      IF (NCRT.NE.IZERO) NLPP = LENGTH + ITHRE
      TOTMAX = RZERO
      DO 10 J=1,NC
        IF (ACLSUM(J).GT.TOTMAX) TOTMAX = ACLSUM(J)
   10  CONTINUE
      MAXTOT = TOTMAX
C
      NW = FLOG10 ( FLOAT (MAXTOT) )
      NW = NW + IFOUR
C
      NWDIFF = NW
      NDDIFF = IZERO
      DO 20 J=1,NC
        CALL MINNW (D(1,J),NR,ISIGD,NX,LINEPR,IZERO,LW,LD,NNW,NND)
        IF (NNW.LE.NWDIFF) GO TO 20
        NWDIFF = NNW
        NDDIFF = NND
  20  CONTINUE
      ND     = MIN0 (ITWO,NDDIFF)
      NWDIFF = NWDIFF - NDDIFF + ND
      NWINT  = NW - ND - IONE
      IF (NWDIFF.GT.NW) NW = NWDIFF
C
      NB = IFIVE
C
      MW = FLOG10 (AGSUMR)
      MW = MW + IONE
      MW = MAX0 (MW,6)
C
      NFIELD = NB + NW
      MB     = ITHRE
      MFIELD = MB + MW
C
C     DETERMINE NUMBER OF COLUMNS PER PAGE, NCPERP, PRINTED.
C
      NCPERP = NC
C
C     DETERMINE NUMBER OF PAGES TO BE PRINTED, NPAGES.
C
      NPAGES = IONE
C
      LWIDTH = NWROW + NC * NFIELD + MFIELD
      IF (LWIDTH.LE.LWIDE) GO TO 30
      NB = ITHRE
      NFIELD = NB + NW
      LWIDTH = NWROW + NC * NFIELD + MFIELD
      IF (LWIDTH.LE.LWIDE) GO TO 30
      NCPERP = IDIV (LWIDE-NWROW,NFIELD,IND)
      NPAGES = IDIV (NWROW+NCPERP*NFIELD+MFIELD,LWIDE,IND) + IONE
C
C     DETERMINE FIRST COL, JBEG, AND LAST COL PRINTED, JEND, PER PAGE.
C
  30  JBEG = IONE
      JEND = NCPERP
        IF (JEND.GT.NC) JEND = NC
      NCT  = IDIV (LWIDE-45,IFIVE,IND)
      NCT  = MIN0 (NC,NCT)
      IR   = IDIV (LWIDE-20,ITEN,IND)
      NCP  = NC - IR
      LWGO = IONE
      IF (LWIDE.GT.NCW) LWGO = ITWO
      WRITE (IPRINT,320) IARGS(1), IARGS(2)
C
C     OUTER LOOP ON NUMBER OF PAGES.
C
      NLNS = 7
      DO 270 IPAGE=1,NPAGES
        ISAVC1 = SAVCO (1) + .5
        ISAVC2 = SAVCO(NCT) + .5
        WRITE (IPRINT,330) ISAVC1, ISAVC2, LA(38)
        NLNS = 7
        WRITE (IPRINT,340) LA(41), LA(45), (LA(39),I=1,14)
C
C       PRINT COLUMN NUMBERS AT TOP OF TABLE.
C
        K   = ITWO
        LEN = (JEND-JBEG+IONE)*NFIELD + MFIELD
        CALL PUTCH (LA(45),LEN,LINEPR)
        IF (JBEG.GT.NC) GO TO 50
        DO 40 J=JBEG,JEND
          X = SAVCO(J)
          CALL RFORMT (9,ISIGD,A,X,NB,0,NWINT,IZERO,LINEPR(K),IRF)
          K = K + NFIELD
  40    CONTINUE
C
  50  LINEND = K - IONE
      IF (IPAGE.NE.NPAGES) GO TO 60
        K = K + MFIELD - NWROW
        LINEPR(K)   = LA(30)
        LINEPR(K+1) = LA(25)
        LINEPR(K+2) = LA(30)
        LINEPR(K+3) = LA(11)
        LINEPR(K+4) = LA(22)
        LINEPR(K+5) = LA(29)
        LINEND = K + 5
  60    WRITE (IPRINT,350) (LINEPR(K),K=1,LINEND)
C
        WRITE (IPRINT,360)
        NLNS = NLNS + ITHRE
C
C       LOOP ON NUMBER OF ROWS.
C
        DO 220 I=1,NR
C
C         PRINT OBSERVED COUNTS.
C
          LINBEG = IONE
          IF (JBEG.GT.JEND) GO TO 80
          K = IONE
          LEN = (JEND-JBEG+IONE) * NFIELD
          CALL PUTCH (LA(45),LEN,LINEPR)
          DO 70 J=JBEG,JEND
            X = AX(I,J)
            CALL RFORMT (9,ISIGD,A,X,NB,0,NWINT,IZERO,LINEPR(K),IRF)
            K = K + NFIELD
  70      CONTINUE
          LINEND = K - IONE
C
C         PRINT ROW MARGINAL TOTALS.
C
          IF (IPAGE.NE.NPAGES) GO TO 90
          LINBEG = LINEND + IONE
  80      X = ARWSUM(I)
          CALL RFORMT (9,ISIGD,A,X,MB,0,MW,IZERO,LINEPR(LINBEG),IRF)
          LINEND = LINBEG + MFIELD - IONE
  90      WRITE (IPRINT,370) I, (LINEPR(K),K=1,LINEND)
          IF (JBEG.LE.JEND) GO TO 100
            WRITE (IPRINT,380)
            WRITE (IPRINT,380)
            WRITE (IPRINT,380)
            GO TO 140
C
C         PRINT EXPECTED COUNTS.
C
 100      K = IONE
          DO 110 J=JBEG,JEND
            CALL RFORMT (7,ISIGD,A,E(I,J),NB,0,NW,ND,LINEPR(K),IRF)
            K = K + NFIELD
 110      CONTINUE
          LINEND = K - IONE
          WRITE (IPRINT,380) (LINEPR(K),K=1,LINEND)
C
C         PRINT UNDERLINE.
C
          LEN = (JEND-JBEG+IONE) * NFIELD
          CALL PUTCH (LA(45),LEN,LINEPR)
          K = IONE + NB
          DO 120 J=JBEG,JEND
            CALL PUTCH (LA(39),NW,LINEPR(K))
            K = K + NFIELD
 120      CONTINUE
          LINEND = K - IONE - NB
          WRITE (IPRINT,380) (LINEPR(K),K=1,LINEND)
C
C         PRINT DIFFERENCE BETWEEN OBSERVED AND EXPECTED COUNT.
C
          K = IONE
          DO 130 J=JBEG,JEND
            CALL RFORMT (7,ISIGD,A,D(I,J),NB,0,NW,ND,LINEPR(K),IRF)
            K = K + NFIELD
 130      CONTINUE
          LINEND = K - IONE
          WRITE (IPRINT,380) (LINEPR(K),K=1,LINEND)
C
 140      NLNS = NLNS + NLTPR
          IF (I+IONE-NR.EQ.IZERO) GO TO 150
          IF (I+IONE-NR.GT.IZERO) GO TO 160
          IF (NLNS.LE.NLPP)   GO TO 160
          GO TO 170
C
 150      IF ((NLNS+9).LE.NLPP) GO TO 160
          GO TO 170
C
 160      IF ((NLNS+ITHRE).LE.NLPP) GO TO 220
 170      IF (NCRT.NE.IZERO) THEN
            CALL PAGE (IZERO)
            NLNS = IONE
          ELSE
            CALL PAGE (4)
C
C         PRINT TITLE AT TOP OF PAGE.
C
            WRITE (IPRINT,390)
            ISAVC1 = SAVCO (1) + .5
            ISAVC2 = SAVCO(NCT) + .5
            WRITE (IPRINT,330) ISAVC1, ISAVC2, LA(38)
            NLNS = 7
            WRITE (IPRINT,340) LA(41), LA(45), (LA(39),II=1,14)
          ENDIF
          IF (LWGO.EQ.IONE) GO TO 210
C
C         PRINT COLUMN NUMBERS AT TOP OF TABLE.
C
        K = ITWO
        LEN = (JEND-JBEG+IONE)*NFIELD + MFIELD
        CALL PUTCH (LA(45),LEN,LINEPR)
        IF (JBEG.GT.NC) GO TO 190
        DO 180 J=JBEG,JEND
          X = SAVCO(J)
          CALL RFORMT (9,ISIGD,A,X,NB,0,NWINT,IZERO,LINEPR(K),IRF)
          K = K + NFIELD
 180    CONTINUE
C
 190    K      = K + MFIELD - NWROW
        LINEND = K - IONE
        IF (IPAGE.NE.NPAGES) GO TO 200
        LINEPR(K)   = LA(30)
        LINEPR(K+1) = LA(25)
        LINEPR(K+2) = LA(30)
        LINEPR(K+3) = LA(11)
        LINEPR(K+4) = LA(22)
        LINEPR(K+5) = LA(29)
        LINEND = K + 5
 200    WRITE (IPRINT,350) (LINEPR(K),K=1,LINEND)
C
C         PRINT ROW HEADING.
C
          WRITE (IPRINT,360)
C
 210      IF (NCRT.EQ.IZERO) NLNS = NLNS + IFOUR
 220    CONTINUE
C
C       PRINT COLUMN TOTALS AT BOTTOM OF TABLE.
C
        IF (JBEG.GT.JEND) GO TO 240
        K = IONE
        LEN = (JEND-JBEG+IONE)*NFIELD + MFIELD
        CALL PUTCH (LA(45),LEN,LINEPR)
        DO 230 J=JBEG,JEND
          X = ACLSUM(J)
          CALL RFORMT (9,ISIGD,A,X,NB,0,NWINT,IZERO,LINEPR(K),IRF)
          K = K + NFIELD
 230    CONTINUE
        LINEND = K - IONE
        IF (IPAGE.NE.NPAGES) GO TO 250
 240    LINBEG = LINEND + IONE
        IF (JBEG.GT.JEND) LINBEG = IONE
        CALL RFORMT (9,ISIGD,A,AGSUMR,MB,0,MW,IZERO,LINEPR(LINBEG),IRF)
        LINEND = LINBEG + MFIELD - IONE
 250    WRITE (IPRINT,400) (LINEPR(K),K=1,LINEND)
C
        JBEG  = JBEG + NCPERP
        JEND  = JEND + NCPERP
        IF (JEND.GT.NC) JEND = NC
C
        IF (IPAGE.EQ.NPAGES) GO TO 270
        CALL PAGE (4)
        WRITE (IPRINT,390)
 270  CONTINUE
C
C     ..................................................................
C
C     PRINT FREEMAN-TUKEY DEVIATES.
C
      IR  = IDIV (LWIDE-ITEN,6,IND)
      JBEG  = IONE
      JEND  = IR
      JEND  = MIN0 (JEND,NC)
      NCP = NCP - IR
      IF ((NLNS+8+ITWO*NR).GT.NLPP) CALL PAGE (4)
      WRITE (IPRINT,410)
 290  II = IZERO
      DO 295 I=JBEG,JEND
       II = II + IONE
       ISAVCO(II) = SAVCO(I) + .5
 295  CONTINUE
      WRITE (IPRINT,420) (ISAVCO(K),K=1,II)
      WRITE (IPRINT,430)
      DO 300 I=1,NR
        WRITE (IPRINT,440) I, (FTD(I,IA),IA=JBEG,JEND)
 300  CONTINUE
      IF (NCP.LE.IZERO) GO TO 310
      JBEG   = JEND + IONE
      JEND   = JEND + IR
      JEND   = MIN0 (JEND,NC)
      NCP  = NCP - IR
      NLNS = NLNS + 8 + ITWO * NR
      IF ((NLNS+8+ITWO*NR).LE.NLPP) GO TO 290
      WRITE (IPRINT,450)
      GO TO 290
C
 310  WRITE (IPRINT,460)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 320  FORMAT (   /2X,30HCONTINGENCY TABLE ANALYSIS OF ,I3,4H BY ,I3,
     1   7H TABLE.,7X,16H* OBSERVED COUNT)
 330  FORMAT (5X,27HOBSERVED COUNTS IN COLUMNS ,I4,4H TO ,I4,1A1,3X,
     1        24HSHOWING * EXPECTED COUNT)
 340  FORMAT (8X,33HHYPOTHESIS IS P(IJ) = P(I.)P(.J).,15X,16A1/
     1        56X,14H*   DIFFERENCE/)
 350  FORMAT (4X,3HCOL,113A1)
 360  FORMAT (3X,3HROW)
 370  FORMAT (/1X,I4,2X,113A1)
 380  FORMAT (7X,113A1)
 390  FORMAT (2X,38HCONTINGENCY TABLE ANALYSIS (CONTINUED)/)
 400  FORMAT (/1X,6HTOTALS,113A1)
C
C     ..................................................................
C
 410  FORMAT (  //4X,22HFREEMAN-TUKEY DEVIATES/)
 420  FORMAT (6X,3HCOL,2X,15(1X,I4,2X)/)
 430  FORMAT (4X,3HROW)
 440  FORMAT (3X,I4,3X,15(1X,F6.2))
 450  FORMAT (   /2X,34HFREEMAN-TUKEY DEVIATES (CONTINUED)/)
 460  FORMAT (/1X,67HREFERENCE.  FREEMAN, M. F. AND TUKEY, J. W. (1950).
     1 TRANSFORMATIONS/1X,
     2 71HRELATED TO THE ANGULAR AND THE SQUARE ROOT. ANN. MATH. STATIST
     3.,21,607.)
C
C     ==================================================================
C
      END
*CONPG2
      SUBROUTINE CONPG2(NR,NC,IDF,CHISQR,PROB)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. CONPG2 V 7.00  4/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT THE SECOND PAGE OF CONTINGENCY TABLE ANALYSIS.
C
C               WRITTEN BY -
C                      RUTH N. VARNER
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
      DIMENSION IDF(9), IRC(3), IR(2), IC(2)
      DIMENSION CHISQR(*), PROB(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
      INCLUDE 'WRKSCR.H'
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER        LA*1
      CHARACTER*1      IBLK, IC, IR, IRC, J
      CHARACTER*2      ID, IDJ, IJ, IRJ
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IJ, ID, IDJ, J, IRJ / 'IJ', 'I.', '.J', 'J', 'RJ' /
C
      DATA IBLK / ' ' /
C
C     ==================================================================
C
      NRNC = NR * NC
      IF (NRNC.LE.9)  GO TO 20
      IF (NRNC.LE.99) GO TO 10
      I = IDIV (NRNC,IHRD,IND)
      NRNCP  = NRNC - I * IHRD
      IRC(1) = LA(I+1)
      I = IDIV (NRNCP,ITEN,IND)
      NRNCP  = NRNCP - I * ITEN
      IRC(2) = LA(I+1)
      IRC(3) = LA(NRNCP+1)
      GO TO 30
C
  10  I = IDIV (NRNC,ITEN,IND)
      NRNCP  = NRNC - I * ITEN
      IRC(1) = LA(I+1)
      IRC(2) = LA(NRNCP+1)
      IRC(3) = IBLK
      GO TO 30
C
  20  IRC(1) = LA(NRNC+1)
      IRC(2) = IBLK
      IRC(3) = IBLK
  30  IF (NR.LE.9) GO TO 40
      I = IDIV (NR,ITEN,IND)
      IR(1)  = LA(I+1)
      NRP    = NR - I * 10
      IR(2)  = LA(NRP+1)
      GO TO 50
C
  40  IR(1) = LA(NR+1)
      IR(2) = IBLK
  50  IF (NC.LE.9) GO TO 60
      I     = IDIV (NC,ITEN,IND)
      IC(1) = LA(I+1)
      NCP   = NC - I * ITEN
      IC(2) = LA(NCP+1)
      GO TO 70
C
  60  IC(1) = LA(NC+1)
      IC(2) = IBLK
  70  CALL PAGE (IFOUR)
C
      WRITE (IPRINT,80)  (LA(39),I=1,79)
C
      WRITE (IPRINT,90)  IJ, (IRC(I),I=1,3), CHISQR(1), IDF(1), PROB(1),
     1                   (LA(39),I=1,67), ID, (IR(I),I=1,2), CHISQR(2),
     2                   IDF(2), PROB(2), IDJ,
     3                   (IC(I),I=1,2), CHISQR(3), IDF(3), PROB(3),
     4                   IJ, ID, IDJ, CHISQR(4), IDF(4), PROB(4),
     5                   (LA(39),I=1,67), CHISQR(5), IDF(5), PROB(5)
C
C     START NEW PAGE IF IN DEMAND MODE
C
      IF (NCRT.NE.IZERO) CALL PAGE (IFOUR)
      WRITE (IPRINT,100) (LA(39),I=1,67)
C
      WRITE (IPRINT,110) IRJ, (IC(I),I=1,2), CHISQR(6), IDF(6), PROB(6),
     1                   (LA(39),I=1,67), J, (IC(I),I=1,2), CHISQR(7),
     2                   IDF(7), PROB(7), CHISQR(8),
     3                   IDF(8), PROB(8), (LA(39),I=1,67),
     4                   CHISQR(9), IDF(9), PROB(9)
      WRITE (IPRINT,120)
C
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  80  FORMAT (19X,30HANALYSIS OF INFORMATION TABLES//28X,12HINDEPENDENCE
     1/28X,12A1/2X,13HCOMPONENT DUE,9X,22HLIKELIHOOD CHI-SQUARED,12X,
     2 11HCHI-SQUARED/2X,13HTO HYPOTHESIS,11X,17HINFORMATION VALUE,6X,
     3 4HD.F.,5X,11HPROBABILITY/2X,67A1/)
  90  FORMAT (2X,2HP(,A2,6H) = 1/,3A1,13X,F12.3,9X,I4,6X,F10.3/3X,67A1/
     1 2(2X,2HP(,A2,6H) = 1/,2A1,14X,F12.3,9X,I4,6X,F10.3/),2X,2HP(,A2,
     2 6H) = P(,A2,3H)P(,A2,1H),8X,F12.3,9X,I4,6X,F10.3/2X,67A1/3X,
     3 20H(PEARSON CHI-SQUARED,5X,F12.3,9X,I4,6X,F10.3,1H)/)
 100  FORMAT (21X,24HHOMOGENEITY OF R SAMPLES/2X,67A1)
 110  FORMAT (2X,2HP(,A2,6H) = 1/,2A1,1X,11HEACH SAMPLE,2X,F12.3,9X,I4,
     16X,F10.3/2X,67A1/2X,3HP( ,A1,6H) = 1/,2A1,1X,11HALL SAMPLES,2X,
     2 F12.3,9X,I4,6X,F10.3/2X,24HHOMOGENEITY OF R SAMPLES,4X,F10.3,9X,
     3I4,6X,F10.3/2X,67A1/3X,20H(PEARSON CHI-SQUARED,5X,F12.3,9X,I4,6X,
     4 F10.3,1H))
 120  FORMAT (   /2X,13HREFERENCE .../3X,
     1 69HKU, H. H., VARNER, R. N., AND KULLBACK, S. (1971). ON THE ANAL
     2YSIS OF/4X,68HMULTIDIMENSIONAL CONTINGENCY TABLES, J. AMER. STATIS
     3T. ASSOC.,66,55.)
C
C     ==================================================================
C
      END
*CONPG3
      SUBROUTINE CONPG3 (ICCT,IRCT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81. CONPG3 V 7.00  4/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT THE THIRD PAGE OF CONTINGENCY TABLE ANLAYSIS.
C        MEASURES OF ASSOCIATION.
C
C               WRITTEN BY -
C                      RUTH N. VARNER
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
      REAL             RATIO
      REAL             FDIV
C
C     ................................................................
C
      CHARACTER        LA*1
C
C     ==================================================================
C
C     START NEW PAGE IF NCRT IS NOT ZERO.
C
      IF (NCRT.NE.IZERO) THEN
        CALL PAGE(IZERO)
        ELSE
        WRITE (IPRINT,270)
      ENDIF
      WRITE (IPRINT,150) (LA(39),I=1,47)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      WRITE (IPRINT,160) A(15), A(7)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      IF (A(20).EQ.RZERO .OR. A(20).EQ.RONE) GO TO 10
      RATIO = FDIV (A(20),A(17),IND)
      WRITE(IPRINT,170) A(20), A(17), RATIO
      GO TO 20
C
  10  WRITE (IPRINT,170) A(20)
  20  IF (A(19).EQ.RZERO .OR. A(19).EQ.RONE) GO TO 30
      RATIO = FDIV (A(19),A(18),IND)
      WRITE (IPRINT,180) A(19), A(18), RATIO
      GO TO 40
C
  30  WRITE (IPRINT,180) A(19)
  40  IF (A(8).EQ.RZERO .OR. A(8).EQ.RONE) GO TO 50
      RATIO = FDIV (A(8),A(16),IND)
      WRITE (IPRINT,190) A(8), A(16), RATIO
      GO TO 60
C
  50  WRITE (IPRINT,190) A(8)
  60  IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      IF (A(9).EQ.RZERO .OR. A(9).EQ.RONE) GO TO 70
      RATIO = FDIV (A(9),A(10),IND)
      WRITE (IPRINT,200) A(9), A(10), RATIO
      GO TO 80
C
  70  WRITE (IPRINT,200) A(9)
  80  IF (A(11).EQ.RZERO .OR. A(11).EQ.RONE) GO TO 90
      RATIO = FDIV (A(11),A(12),IND)
      WRITE (IPRINT,210) A(11), A(12), RATIO
      GO TO 100
C
  90  WRITE (IPRINT,210) A(11)
 100  IF (A(13).EQ.RZERO .OR. A(13).EQ.RONE) GO TO 110
      RATIO = FDIV (A(13),A(14),IND)
      WRITE (IPRINT,220) A(13), A(14), RATIO
      GO TO 120
C
 110  WRITE (IPRINT,220) A(13)
 120  IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      WRITE (IPRINT,230)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      WRITE (IPRINT,240)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      WRITE (IPRINT,250)
      IF (NCRT.EQ.IZERO) WRITE (IPRINT,270)
      WRITE (IPRINT,260)
      IF (ICCT.EQ.IZERO) GO TO 140
      CALL ERROR (239)
 140  IF (IRCT.EQ.IZERO) RETURN
      CALL ERROR (239)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 150  FORMAT (    49X,10HASYMPTOTIC/
     1             2X,23HMEASURES OF ASSOCIATION,
     2            14X, 5HVALUE,
     3             3X,14HSTANDARD ERROR,
     4             3X, 5HRATIO/
     5             2X,23A1,14X,5A1,3X,14A1,3X,5A1 )
 160  FORMAT (2X,30HBASED ON CHI-SQUARED STATISTIC/8X,12HPHI (CRAMER),
     113X,F12.3/8X,23HCONTINGENCY COEFFICIENT,2X,F12.3 )
 170  FORMAT (2X,31HBASED ON RANK ORDER CORRELATION/
     18X,13HKENDALL'S TAU,12X,3F12.3)
 180  FORMAT (8X,14HSPEARMAN'S RHO,11X,3F12.3)
 190  FORMAT (8X,15HKRUSKAL'S GAMMA,10X,3F12.3)
 200  FORMAT (    2X,34HBASED ON PROBABILITY OF PREDICTION/
     1     8X,8HLAMBDA A,17X,3F12.3)
 210  FORMAT (8X,8HLAMBDA B,17X,3F12.3)
 220  FORMAT (8X,6HLAMBDA,19X,3F12.3)
 230  FORMAT (    2X,65HNOTE ... DISCUSSIONS OF THESE MEASURES AND FORMU
     1LAS FOR COMPUTING/
     2 5X,36HSTANDARD ERRORS MAY BE FOUND IN ... )    
 240  FORMAT (2X,
     1 68HGOODMAN, L. A. AND KRUSKAL, W. H., MEASURES OF ASSOCIATION FOR
     2 CROSS/5X,65HCLASSIFICATIONS. PART I, II, AND III, J. OF AMER. STA
     3TIST. ASSOC./5X,61HVOL. 49,732. (1954). VOL. 54,123. (1959), VOL.
     458,310 (1963).)    
 250  FORMAT (2X,
     1 69HKENDALL, M. AND STUART, A. (1973). THE ADVANCED THEORY OF STAT
     2., V 2.)
 260  FORMAT ( 2X,63HHAYS, W. L. (1973). STATISTICS FOR THE SOCIAL SCIEN
     1CES. 2ND ED.)
 270  FORMAT (1H )
C
C     ==================================================================
C
      END
*CONTB
      SUBROUTINE CONTB
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82.  CONTB V 7.00  8/ 8/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE CONTINGENCY TABLE ANALYSIS INSTRUCTION.
C
C     FORM OF INSTRUCTION IS ...
C        CONTINGENCY FOR (R) BY (C) TABLE BEGINNING IN COLUMN (C)
C
C     DATA TO BE ANALYZED ARE IN CONSECUTIVE COLUMNS STARTING AT (C).
C
C     ALL THE CALCULATIONS ARE DONE IN THIS PROGRAM UNIT.
C
C               WRITTEN BY -
C                      RUTH N. VARNER
C                      STATISTICAL ENGINEERING DIVISION
C               REVISED BY -
C                      SALLY T. PEAVY AND DAVID HOGBEN
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -      MAY, 1978.
C                   CURRENT VERSION -   AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION    IDF(9)
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
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
      REAL             CHISQR(9)
      REAL             PROBAB(9), XS(1)
      REAL             ACCT, ADSTR, AGSUMR, AIP, AIPP, AIQ
      REAL             AIQP, AISUMC, AISUMR, ALAM, ALAMA, ALAMB
      REAL             ALDOT, ALDOT2, ALGC, ALGR, ALSTR, ALSUM, ALSUM2
      REAL             AMAXDM, AMAXMD, ANMCN, ANMCN2, ANMRM, ANMRM2
      REAL             ANNM1, ARCT, ASCANJ, ASRAIN, ASUMC, ASUMIM
      REAL             ASUMMJ, ASUMR, ASUMST, AXMC, AXMR, CONCOF, CSUBJ
      REAL             FIXC, FIXR, FMAXIM, FMAXMJ, GAMKRU, GSUM, P, PCHI
      REAL             PHI, RSUBI,SEKGM, SEKTAU, SELAM, SELAMA, SELAMB
      REAL             SESPRH, SPRHO, SUMST, SUM1, SUM2, SUM3, TAUKEN
      REAL             FDIV, FDPCON, FSQRT
C
C     ..................................................................
C
      DOUBLE PRECISION DXS(3)
      DOUBLE PRECISION DNR, DNC, DGSUM, DCLSUM, DRWSUM
      DOUBLE PRECISION DLNR, DLNC, DLNN, DX, SXLNX, SXID, SXDJ
      DOUBLE PRECISION DISUMR,DISUMC,DSUMN3,DSUMD2,DSPRHO,DGSUM2,DGSUM3
      DOUBLE PRECISION DPT, DPS, DPD, DPSS, DPDD, DPSD, DAMP, DDMP, DAX
      DOUBLE PRECISION DPCA, DPCB
      DOUBLE PRECISION DAIPM, DAIPP, DAIQM, DAIQP, DSCANJ, DSRAIN
      DOUBLE PRECISION DSUMAC, DSUMAM, DSUMAP, DSUMAR
      DOUBLE PRECISION DSUMC, DSUMCC, DSUMCL, DSUMCT
      DOUBLE PRECISION DSUMDM, DSUMDP, DSUMIM, DSUMMJ
      DOUBLE PRECISION DSUMR, DSUMRK, DSUMRR, DSUMRT, DSUMST
      DOUBLE PRECISION FDLOG, FDDIV, FDSQRT
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ..................................................................
C
      EQUIVALENCE (   CONCOF,  A( 7)), (   GAMKRU,  A( 8))
      EQUIVALENCE (    ALAMA,  A( 9)), (   SELAMA,  A(10))
      EQUIVALENCE (    ALAMB,  A(11)), (   SELAMB,  A(12))
      EQUIVALENCE (     ALAM,  A(13)), (    SELAM,  A(14))
      EQUIVALENCE (      PHI,  A(15)), (    SEKGM,  A(16))
      EQUIVALENCE (   SEKTAU,  A(17)), (   SESPRH,  A(18))
      EQUIVALENCE (    SPRHO,  A(19)), (   TAUKEN,  A(20))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / 12.0D0 /
      DATA DPCB / 16.0D0 /
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (NARGS.LT.ITHRE) GO TO 800
C
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS.
C
      J = NARGS
      CALL CKIND (J)
      IF (J.NE.IZERO) GO TO 800
C
      NR   = IARGS(1)
      NC   = IARGS(2)
      NRNC = NR * NC
C
C     CHECK TO SEE IF DIMENSIONS ARE WITHIN BOUNDS.
C
      IF (IARGS(1).GT.NROW .OR. IARGS(2).GT.NCOL) GO TO 800
C
C     CHECK TO SEE IF NUMBER OF ROWS AND COLUMNS IS WITHIN BOUNDS.
C
      IF (IARGS(1).LT.ITWO .OR. IARGS(2).LT.ITWO) GO TO 800
C
C     TOTAL SCRATCH AREA REQUIRED IS NCHECK.  HENCE, NR AND NC
C        MUST BE SUCH THAT NCHECK IS LESS THAN OR EQUAL TO NS.
C
      NCHECK = 8 * NR * NC + IFOUR * NC + ITHRE * NR + 50
      IF (NCHECK.GT.NS) CALL ERROR (23)
C
C     CHECK LWIDE.
C
      IF (IDIV(LWIDE-20,ITEN,IND).LE.IZERO) CALL ERROR (245)
C
C     COMPUTE SUBSCRIPTS FOR ARRAYS EQUIVALENT TO SCRATCH AREA.
C
      ISUBX  = 51
      ISUBAP = ISUBX  + NRNC
      ISUBAM = ISUBAP + NRNC
      ISUBDP = ISUBAM + NRNC
      ISUBDM = ISUBDP + NRNC
      ISUBE  = ISUBDM + NRNC
      ISUBD  = ISUBE  + NRNC
      ISUBTD = ISUBD  + NRNC
      ISUBCO = ISUBTD + NRNC
      ISUBCV = ISUBCO + NC
      ISUBMJ = ISUBCV + NC
      ISUBCL = ISUBMJ + NC
      ISUBRW = ISUBCL + NC
      ISUBRV = ISUBRW + NR
      ISUBIM = ISUBRV + NR
C
C     COMPUTE STARTING ADDRESSES IN WORKSHEET.
C        GENERATE COLUMN NUMBERS.
C
      A(ISUBCO) = IARGS(3)
      JSUBCO = ISUBCO
      DO 20 I=1,NC
        IF (I.EQ.NC) GO TO 10
        A(JSUBCO+1) = A(JSUBCO) + RONE
        IARGS(I+3)       = IARGS(I+2) + IONE
        KIND(I+3)        = IZERO
  10    CALL ADRESS (I+2,IARGS(I+2))
        IF (IARGS(I+2).LE.IZERO) GO TO 800
        JSUBCO = JSUBCO + IONE
  20  CONTINUE
C
C     CHECK NERROR.
C
      IF (NERROR.NE.IZERO) RETURN
C
C     COMPUTE COLUMN SUMS.
C        SAVE INPUT VALUES AS INTEGERS.
C
      DSUMCT = DZERO
      DSUMAC = DZERO
      JSUBCL = ISUBCL
      JSUBX  = ISUBX
      DO 40 L=1,NC
        IADD  = IARGS(L+2)
        DSUMC = DZERO
        DO 30 K=1,NR
          A(JSUBX) = RC(IADD)
          DSUMC     = DSUMC + DBLE (A(JSUBX))
          JSUBX     = JSUBX + IONE
          IADD      = IADD  + IONE
  30    CONTINUE
        DSUMAC         = DSUMAC + DSUMC * (DSUMC - DONE)
        DSUMCT         = DSUMCT + DSUMC
        A(JSUBCL) = FDPCON (DSUMC)
        JSUBCL         = JSUBCL + IONE
  40  CONTINUE
      AISUMC = FDPCON (DSUMAC)
C
C     COMPUTE ROW SUMS.
C
      DSUMRT = DZERO
      DSUMAR = DZERO
      JSUBRW = ISUBRW
      DO 60 L=1,NR
        JSUBX = ISUBX + L - IONE
        DSUMR = DZERO
        DO 50 K=1,NC
          DSUMR = DSUMR + DBLE (A(JSUBX))
          JSUBX = JSUBX + NR
  50    CONTINUE
        DSUMAR         = DSUMAR + DSUMR * (DSUMR - DONE)
        DSUMRT         = DSUMRT + DSUMR
        A(JSUBRW) = DSUMR
        JSUBRW         = JSUBRW + IONE
  60  CONTINUE
      AGSUMR = FDPCON (DSUMRT)
      AISUMR = FDPCON (DSUMAR)
C
C     COMPUTE EXPECTED VALUES, DEVIATIONS AND FREEMAN-TUKEY DEVIATES.
C
      GSUM   = AGSUMR
      IC     = ISUBCL
      JSUBE  = ISUBE
      JSUBD  = ISUBD
      JSUBTD = ISUBTD
      DO 80 L=1,NC
        JSUBRW = ISUBRW
        IADD   = IARGS(L+2)
        DO 70 K=1,NR
          A(JSUBE) = FDIV (A(JSUBRW)*A(IC),GSUM,IND)
          A(JSUBD) = RC(IADD) - A(JSUBE)
          A(JSUBTD) = FSQRT (RC(IADD)) + FSQRT (RC(IADD)+RONE) -
     1               FSQRT (RFOR*A(JSUBE)+RONE)
          IADD   = IADD + IONE
          JSUBE  = JSUBE + IONE
          JSUBD  = JSUBD + IONE
          JSUBTD = JSUBTD + IONE
          JSUBRW = JSUBRW + IONE
  70    CONTINUE
        IC = IC + IONE
  80  CONTINUE
C
C     COMPUTE INDEPENDENCE ANALYSIS INFORMATION TABLES.
C        COMPUTE HOMOGENEITY OF R SAMPLES.
C
      DNR   = NR
      DNC   = NC
      DGSUM = GSUM
      JSUBE = ISUBE
      JSUBD = ISUBD
      CALL SUMMAL (XS,IZERO,PCHI)
      DO 100 J=1,NC
        DO 90 I=1,NR
          XS(1) = FDIV (A(JSUBD)**2,A(JSUBE),IND)
          JSUBE = JSUBE +IONE
          JSUBD = JSUBD +IONE
          CALL SUMMAL (XS,-IONE,PCHI)
  90      CONTINUE
 100  CONTINUE
      CALL SUMMAL (XS,IONE,PCHI)
C
      CALL DSUMAL (DXS,IZERO,SXLNX)
      DO 120 J=1,NC
        IADD = IARGS(J+2)
        DO 110 I=1,NR
          DX = RC(IADD)
          IF (DX.LE.DZERO) DX = DONE
          DXS(1) = DX * FDLOG (DX)
          CALL DSUMAL (DXS,-IONE,SXLNX)
          IADD  = IADD + IONE
 110      CONTINUE
 120  CONTINUE
      CALL DSUMAL (DXS,IONE,SXLNX)
C
      CALL DSUMAL (DXS,IZERO,SXID)
      JSUBRW = ISUBRW
      DO 130 I=1,NR
        DRWSUM = A(JSUBRW)
        JSUBRW = JSUBRW + IONE
        DXS(1) = DRWSUM * FDLOG (DRWSUM)
        CALL DSUMAL (DXS,-IONE,SXID)
 130  CONTINUE
      CALL DSUMAL (DXS,IONE,SXID)
C
      CALL DSUMAL (DXS,IZERO,SXDJ)
      JSUBCL = ISUBCL
      DO 140 J=1,NC
        DCLSUM = A(JSUBCL)
        JSUBCL = JSUBCL + IONE
        DXS(1) = DCLSUM * FDLOG (DCLSUM)
        CALL DSUMAL (DXS,-IONE,SXDJ)
 140  CONTINUE
      CALL DSUMAL (DXS,IONE,SXDJ)
C
      IDF(1) = NR * NC - IONE
      IDF(2) = NR - IONE
      IDF(3) = NC - IONE
      IDF(4) = IDF(2) * IDF(3)
      IDF(5) = IDF(4)
      IDF(6) = NR * IDF(3)
      IDF(7) = IDF(3)
      IDF(8) = IDF(4)
      IDF(9) = IDF(5)
      DLNR = DGSUM * FDLOG (DNR)
      DLNC = DGSUM * FDLOG (DNC)
      DLNN = DGSUM * FDLOG (DGSUM)
      CHISQR(1) = DTWO * (SXLNX+DLNR+DLNC-DLNN)
      CHISQR(2) = DTWO * (SXID+DLNR-DLNN)
      CHISQR(3) = DTWO * (SXDJ+DLNC-DLNN)
      CHISQR(4) = DTWO * (SXLNX-SXID-SXDJ+DLNN)
      CHISQR(5) = PCHI
      CHISQR(6) = DTWO * (SXLNX-SXID+DLNC)
      CHISQR(7) = DTWO * (SXDJ+DLNC-DLNN)
      CHISQR(8) = DTWO * (SXLNX-SXID-SXDJ+DLNN)
      CHISQR(9) = CHISQR(5)
      DO 150 I=1,8
        CALL CTCCDF (CHISQR(I),IDF(I),P,IND)
        PROBAB(I) = RONE - P
 150  CONTINUE
C
      PROBAB(9) = PROBAB(5)
C
C     COMPUTE MEASURES OF ASSOCIATION.
C
C     COMPUTE AGREEMENTS.
C
      JSUBAP = ISUBAP
      LSUBX  = ISUBX
      DAIPP  = DZERO
      DO 200 J=1,NC
        LL = J + IONE
        DO 190 I=1,NR
          DSUMAP = DZERO
          IF (J.EQ.NC .OR. I.EQ.NR) GO TO 180
          KA = I + IONE
          DO 170 L=LL,NC
            JSUBX = ISUBX + (L-IONE) * NR + KA - IONE
            DO 160 K=KA,NR
              DSUMAP = DSUMAP + DBLE (A(JSUBX))
              JSUBX  = JSUBX + IONE
 160        CONTINUE
 170      CONTINUE
 180      A(JSUBAP) = FDPCON (DSUMAP)
          DAIPP       = DAIPP + DSUMAP * DBLE (A(LSUBX))
          JSUBAP      = JSUBAP + IONE
          LSUBX       = LSUBX + IONE
 190    CONTINUE
 200  CONTINUE
      AIPP = FDPCON (DAIPP)
C
      JSUBAM = ISUBAM
      LSUBX  = ISUBX
      DAIPM  = DZERO
      DO 250 J=1,NC
        LL = J - IONE
        DO 240 I=1,NR
          DSUMAM = DZERO
          IF (J.EQ.IONE .OR. I.EQ.IONE) GO TO 230
          KA = I - IONE
          DO 220 L=1,LL
            JSUBX = ISUBX + NR * (L-IONE)
            DO 210 K=1,KA
              DSUMAM = DSUMAM + DBLE (A(JSUBX))
              JSUBX  = JSUBX + IONE
 210        CONTINUE
 220      CONTINUE
 230      A(JSUBAM) = FDPCON (DSUMAM)
          DAIPM       = DAIPM + DSUMAM * DBLE (A(LSUBX))
          JSUBAM      = JSUBAM + IONE
          LSUBX       = LSUBX + IONE
 240    CONTINUE
 250  CONTINUE
C
C     COMPUTE DISAGREEMENTS.
C
      JSUBDP = ISUBDP
      LSUBX  = ISUBX
      DAIQP  = DZERO
      DO 300 J=1,NC
        LL = J - IONE
        DO 290 I=1,NR
          DSUMDP = DZERO
          IF (I.EQ.NR .OR. J.EQ.IONE) GO TO 280
          KA = I+IONE
          DO 270 L=1,LL
            JSUBX = ISUBX + NR * (L-IONE) + KA - IONE
            DO 260 K=KA,NR
              DSUMDP = DSUMDP + DBLE (A(JSUBX))
              JSUBX  = JSUBX + IONE
 260        CONTINUE
 270      CONTINUE
 280      A(JSUBDP) = FDPCON (DSUMDP)
          DAIQP       = DAIQP + DSUMDP * DBLE (A(LSUBX))
          JSUBDP      = JSUBDP + IONE
          LSUBX       = LSUBX + IONE
 290    CONTINUE
 300  CONTINUE
      AIQP = FDPCON (DAIQP)
C
      JSUBDM = ISUBDM
      LSUBX  = ISUBX
      DAIQM  = DZERO
      DO 350 J=1,NC
        LL = J + IONE
        DO 340 I=1,NR
          DSUMDM = DZERO
          IF (I.EQ.IONE .OR. J.EQ.NC) GO TO 330
          KA = I - IONE
          DO 320 L=LL,NC
          JSUBX = ISUBX + (L-IONE) * NR
            DO 310 K=1,KA
              DSUMDM = DSUMDM + DBLE (A(JSUBX))
              JSUBX  = JSUBX + IONE
 310        CONTINUE
 320      CONTINUE
 330      A(JSUBDM) = FDPCON (DSUMDM)
          DAIQM       = DAIQM + DSUMDM * DBLE (A(LSUBX))
          JSUBDM      = JSUBDM + IONE
          LSUBX       = LSUBX + IONE
 340    CONTINUE
 350  CONTINUE
C
C     COMPUTE P AND Q.
C
      AIP = FDPCON (DAIPP + DAIPM)
      AIQ = FDPCON (DAIQP + DAIQM)
C
C     COMPUTE PHI AND CONTINGENCY COEFFICIENT.
C
      PHI    = FDIV (PCHI,GSUM*FLOAT(MIN0(NC-IONE,NR-IONE)),IND)
      PHI    = FSQRT (PHI)
      CONCOF = FSQRT (FDIV(PCHI,GSUM+PCHI,IND))
C
C     COMPUTE KENDALL'S TAU.
C
      ANNM1  = AGSUMR * (AGSUMR-RONE)
      TAUKEN = FSQRT (ANNM1-AISUMR)*FSQRT(ANNM1-AISUMC)
      TAUKEN = FDIV (AIP-AIQ,TAUKEN,IND)
C
C     STANDARD ERROR (KENDALL'S TAU).
C
      SEKTAU = FSQRT (FDIV(RFOR*GSUM+RTEN,9.0*GSUM*(GSUM-RONE),IND) )
C
C     COMPUTE SPEARMAN'S RHO.
C
      CALL DSUMAL (DXS,IZERO,DISUMR)
      JSUBRW = ISUBRW
      DO 400 I=1,NR
        DXS(1) = DBLE (A(JSUBRW))
        DXS(1) = DXS(1) * (DXS(1) ** 2 - DONE)
        CALL DSUMAL (DXS,-IONE,DISUMR)
        JSUBRW = JSUBRW + IONE
 400  CONTINUE
      CALL DSUMAL (DXS,IONE,DISUMR)
      IF (NR.EQ.IONE) DISUMR = DXS(1)
C
      CALL DSUMAL (DXS,IZERO,DISUMC)
      JSUBCL = ISUBCL
      DO 410 J=1,NC
        DXS(1) = DBLE (A(JSUBCL))
        DXS(1) = DXS(1) * (DXS(1) ** 2 - DONE)
        CALL DSUMAL (DXS,-IONE,DISUMC)
        JSUBCL = JSUBCL + IONE
 410  CONTINUE
      CALL DSUMAL (DXS,IONE,DISUMC)
      IF (NC.EQ.IONE) DISUMC = DXS(1)
C
      DSUMN3 = DGSUM * (DGSUM**2-DONE)
      DSUMD2 = FDDIV (DGSUM,DTWO,IND)
      CALL DSUMAL (DXS,IZERO,DSPRHO)
      KSUBRW = ISUBRW
      DO 470 I=1,NR
        JSUBX  = ISUBX + I - IONE
        JSUBCL = ISUBCL
        DO 460 J=1,NC
          DSUMRK = DZERO
          IF (I.EQ.IONE) GO TO 430
          KB = I - IONE
          JSUBRW = ISUBRW
          DO 420 K=1,KB
            DSUMRK = DSUMRK + DBLE (A(JSUBRW))
            JSUBRW = JSUBRW + IONE
 420      CONTINUE
 430      DSUMCL = DZERO
          IF (J.EQ.IONE) GO TO 450
          LB     = J - IONE
          KSUBCL = ISUBCL
          DO 440 L=1,LB
            DSUMCL = DSUMCL + DBLE (A(KSUBCL))
            KSUBCL = KSUBCL + IONE
 440      CONTINUE
 450      DXS(1) = DSUMRK + FDDIV (DBLE(A(KSUBRW)),DTWO,IND)
          DXS(2) = DSUMCL + FDDIV (DBLE(A(JSUBCL)),DTWO,IND)
          DXS(3) = (DXS(1)-DSUMD2) * (DXS(2)-DSUMD2)
          DXS(1) = DBLE(A(JSUBX)) * DXS(3)
          CALL DSUMAL (DXS,-IONE,DSPRHO)
          JSUBX  = JSUBX  + NR
          JSUBCL = JSUBCL + IONE
 460    CONTINUE
        KSUBRW = KSUBRW + IONE
 470  CONTINUE
      CALL DSUMAL (DXS,IONE,DSPRHO)
C
      DSPRHO = DPCA * FDDIV (DSPRHO,FDSQRT(DSUMN3-DISUMR)*
     1          FDSQRT(DSUMN3-DISUMC),IND)
      SPRHO = DSPRHO
C
C     STANDARD ERROR (SPEARMAN'S RHO).
C
      SESPRH = FDSQRT (FDDIV(DONE-DSPRHO*DSPRHO,DGSUM-DTWO,IND))
C
C     COMPUTE KRUSKAL'S GAMMA.
C
      GAMKRU = FDIV (AIP-AIQ,AIP+AIQ,IND)
C
C     STANDARD ERROR (KRUSKAL'S GAMMA).
C
      DGSUM2 = DGSUM * DGSUM
      DGSUM3 = DGSUM2 * DGSUM
      DPS    = FDDIV (DTWO*DBLE(AIPP),DGSUM2,IND)
      DPD    = FDDIV (DTWO*DBLE(AIQP),DGSUM2,IND)
      DPT    = DONE - DPS - DPD
      DPSS   = DZERO
      DPDD   = DZERO
      DPSD   = DZERO
      JSUBX  = ISUBX
      JSUBAM = ISUBAM
      JSUBAP = ISUBAP
      JSUBDM = ISUBDM
      JSUBDP = ISUBDP
      DO 490 J=1,NC
        DO 480 I=1,NR
          DAMP = DBLE (A(JSUBAM)) + DBLE (A(JSUBAP))
          DDMP = DBLE (A(JSUBDM)) + DBLE (A(JSUBDP))
          DAX  = A(JSUBX)
          DPSS = DPSS + DAX * DAMP**2
          DPDD = DPDD + DAX * DDMP**2
          DPSD = DPSD + DAX * DAMP * DDMP
          JSUBX  = JSUBX + IONE
          JSUBAM = JSUBAM + IONE
          JSUBAP = JSUBAP + IONE
          JSUBDM = JSUBDM + IONE
          JSUBDP = JSUBDP + IONE
 480    CONTINUE
 490  CONTINUE
C
      DPSS   = FDDIV (DPSS,DGSUM3,IND)
      DPDD   = FDDIV (DPDD,DGSUM3,IND)
      DPSD   = FDDIV (DPSD,DGSUM3,IND)
      DXS(1) = DPCB * (DPS*DPS*DPDD-DTWO*DPS*DPD*DPSD+DPD*DPD*DPSS)
      DXS(2) = FDDIV (DXS(1),DGSUM*(DONE-DPT)**4,IND)
      SEKGM  = FDSQRT (DXS(2))
C
C     COMPUTE MEASURES OF ASSOCIATION
C             BASED ON PROBABILITY OF PREDICTION.
C                      LAMBDA A, LAMBDA B AND LAMBDA.
C
C     ..................................................................
C
C     AMAXDM = MAXIMUM COLUMN SUM.
C     ICOL   = COLUMN NUMBER WITH MAXIMUM COLUMN SUM (FIRST).
C     ICCT   = 0 ONLY ONE MAXIMUM COLUMN SUM.
C     ICCT   = -1 TWO OR MORE MAXIMUM SUMS ARE EQUAL.
C     AMAXMD = MAXIMUM ROW SUM.
C     IROW   = ROW NUMBER WITH MAXIMUM ROW SUM (FIRST).
C     IRCT   = 0 ONLY ONE MAXIMUM ROW SUM.
C     IRCT   = -1 TWO OR MORE MAXIMUM SUMS ARE EQUAL.
C
C     ..................................................................
C
      AMAXDM = A(ISUBCL)
      ACCT   = RZERO
      ICOL   = IONE
      JSUBCL = ISUBCL + IONE
      DO 520 J=2,NC
        IF (AMAXDM.LT.A(JSUBCL)) GO TO 500
        IF (AMAXDM.GT.A(JSUBCL)) GO TO 510
        ACCT = AMAXDM
        GO TO 510
 500    AMAXDM = A(JSUBCL)
        ICOL   = J
 510    JSUBCL = JSUBCL + IONE
 520  CONTINUE
      ICCT  = IZERO
      IF (ACCT.EQ.AMAXDM) ICCT = -IONE
C
      AMAXMD = A(ISUBRW)
      IROW   = IONE
      JSUBRW = ISUBRW + IONE
      ARCT   = RONE
      DO 550 I=2,NR
        IF (AMAXMD.LT.A(JSUBRW)) GO TO 530
        IF (AMAXMD.GT.A(JSUBRW)) GO TO 540
        ARCT = AMAXMD
        GO TO 540
 530    AMAXMD = A(JSUBRW)
        IROW   = I
 540    JSUBRW = JSUBRW + IONE
 550  CONTINUE
      IRCT  = IZERO
      IF (ARCT.EQ.AMAXMD) IRCT = -IONE
C
C     ..................................................................
C
C     ASUMIM = SUM OF MAXIMUM VALUES IN ROWS.
C     ASUMMJ = SUM OF MAXIMUM VALUES IN COLUMNS.
C
C     ..................................................................
C
      DSUMIM = DZERO
      JSUBIM = ISUBIM
      DO 570 I=1,NR
        JSUBX          = ISUBX + I - IONE
        A(JSUBIM) = A(JSUBX)
        DO 560 J=1,NC
          A(JSUBIM) = AMAX1 (A(JSUBIM),A(JSUBX))
          JSUBX = JSUBX + NR
 560    CONTINUE
        DSUMIM = DSUMIM + DBLE (A(JSUBIM))
        JSUBIM = JSUBIM + IONE
 570  CONTINUE
      ASUMIM = FDPCON (DSUMIM)
C
      DSUMMJ = DZERO
      JSUBMJ = ISUBMJ
      JSUBX  = ISUBX
      DO 590 J=1,NC
        A(JSUBMJ) = A(JSUBX)
        DO 580 I=1,NR
          A(JSUBMJ) = AMAX1 (A(JSUBMJ), A(JSUBX))
          JSUBX  = JSUBX + IONE
 580    CONTINUE
        DSUMMJ = DSUMMJ + DBLE (A(JSUBMJ))
        JSUBMJ = JSUBMJ + IONE
 590  CONTINUE
      ASUMMJ = FDPCON (DSUMMJ)
C
      ALAMA = FDIV (ASUMMJ-AMAXMD,GSUM-AMAXMD,IND)
      ALAMB = FDIV (ASUMIM-AMAXDM,GSUM-AMAXDM,IND)
      ALAM  = FDIV (ASUMIM+ASUMMJ-AMAXDM-AMAXMD,
     1              RTWO*AGSUMR-AMAXDM-AMAXMD,IND)
C
C     COMPUTE STANDARD ERROR OF LAMDA, LAMDA A AND LAMDA B.
C
C     USING COLUMN WITH LARGEST COLUMN TOTAL.
C        SUM ALL VALUES IN THAT COLUMN WHICH
C           ARE LARGEST IN THEIR ROW (ISUMC).
C        REPEAT ABOVE STEPS FOR ROW VALUES (ISUMR).
C
      DSUMCC = DZERO
      JSUBX  = ISUBX + (ICOL-IONE) * NR
      DO 610 I=1,NR
        ALGR   = A(JSUBX)
        KSUBX  = ISUBX + I - IONE
        DO 600 J=1,NC
          ALGR  = AMAX1 (ALGR,A(KSUBX))
          KSUBX = KSUBX + NR
 600    CONTINUE
        IF (ALGR.EQ.A(JSUBX)) DSUMCC = DSUMCC + DBLE (ALGR)
        JSUBX = JSUBX + IONE
 610  CONTINUE
      ASUMC = FDPCON (DSUMCC)
C
      DSUMRR = DZERO
      KSUBX  = ISUBX
      JSUBX  = ISUBX + IROW - IONE
      DO 630 J=1,NC
        ALGC = A(JSUBX)
        DO 620 I=1,NR
          ALGC  = AMAX1 (ALGC,A(KSUBX))
          KSUBX = KSUBX + IONE
 620    CONTINUE
        IF (ALGC.EQ.A(JSUBX)) DSUMRR = DSUMRR + DBLE (ALGC)
        JSUBX = JSUBX + NR
 630  CONTINUE
      ASUMR = FDPCON (DSUMRR)
C
C     FIND MAX VALUE IN COLUMN WITH LARGEST SUM (IXMC).
C     FIND MAX VALUE IN ROW    WITH LARGEST SUM (IXMR).
C
      JSUBX = ISUBX + NR * (ICOL-1)
      AXMC  = A(JSUBX)
      DO 640 I=2,NR
        JSUBX = JSUBX + IONE
        AXMC  = AMAX1 (AXMC,A(JSUBX))
 640  CONTINUE
C
      JSUBX = ISUBX + IROW - IONE
      AXMR  = A(JSUBX)
      DO 650 J=2,NC
        JSUBX = JSUBX + NR
        AXMR = AMAX1 (AXMR,A(JSUBX))
 650  CONTINUE
C
C     SELECT MAXIMUM VALUE IN EACH COLUMN (IXMRV(I)).
C     SELECT MAXIMUM VALUE IN EACH ROW    (IXMCV(J)).
C        SUM WHERE A(I,J) = A(I,M) = A(M,J) (ISUMST).
C
      JSUBRV = ISUBRV
      DO 670 I=1,NR
        JSUBX = ISUBX + I - IONE
        A(JSUBRV) = A(JSUBX)
        DO 660 J=2,NC
          JSUBX = JSUBX + NR
          A(JSUBRV) = AMAX1 (A(JSUBRV),A(JSUBX))
 660    CONTINUE
        JSUBRV = JSUBRV + IONE
 670  CONTINUE
C
      JSUBCV = ISUBCV
      DO 690 J=1,NC
        JSUBX = ISUBX + NR * (J-IONE)
        A(JSUBCV) = A(JSUBX)
        DO 680 I=2,NR
          JSUBX = JSUBX + IONE
          A(JSUBCV) = AMAX1 (A(JSUBCV),A(JSUBX))
 680    CONTINUE
        JSUBCV = JSUBCV + IONE
 690  CONTINUE
C
      DSUMST = DZERO
      JSUBCV = ISUBCV
      JSUBX  = ISUBX
      DO 710 J=1,NC
        JSUBRV = ISUBRV
        DO 700 I=1,NR
        IF (A(JSUBX).EQ.A(JSUBRV) .AND. A(JSUBX).EQ.A(JSUBCV))
     1               DSUMST = DSUMST + DBLE (A(JSUBX))
          JSUBRV = JSUBRV + IONE
          JSUBX  = JSUBX + IONE
 700    CONTINUE
        JSUBCV = JSUBCV + IONE
 710  CONTINUE
      ASUMST = FDPCON (DSUMST)
C
      SUMST  = FDIV (ASUMST,GSUM,IND)
      JSUBX  = ISUBX + NR * (ICOL-IONE) + IROW - IONE
      ADSTR  = A(JSUBX)
      ALDOT  = FDIV (AMAXDM+AMAXMD,GSUM,IND)
      ALSUM  = FDIV (ASUMIM+ASUMMJ,GSUM,IND)
      ALSTR  = FDIV (ASUMC+ASUMR+AXMC+AXMR,GSUM,IND)
      ALDOT2 = RTWO - ALDOT
      ALSUM2 = RTWO - ALSUM
      SELAM  = FSQRT (FDIV(ALDOT2*ALSUM2*(ALDOT+ALSUM+RFOR-RTWO*ALSTR)-
     1     RTWO*ALDOT2*ALDOT2*(RONE-SUMST)-RTWO*ALSUM2*ALSUM2*(RONE-
     2     FDIV(ADSTR,GSUM,IND)),GSUM*(ALDOT2**4),IND))
C
C     COMPUTE SE(LAMBDA A).
C
      CALL SUMMAL (XS,IZERO,SUM1)
      JSUBCL = ISUBCL
      JSUBMJ = ISUBMJ
      JSUBCV = ISUBCV
      JSUBX  = ISUBX + IROW - IONE
      DSCANJ = DZERO
      DO 730 J=1,NC
        CSUBJ  = A(JSUBCL)
        FMAXMJ = A(JSUBMJ)
        XS(1)  = FMAXMJ * (RONE-FDIV(FMAXMJ,CSUBJ,IND))
        CALL SUMMAL (XS,-IONE,SUM1)
        IF (A(JSUBCV).NE.A(JSUBX)) GO TO 720
        DSCANJ = DSCANJ + DBLE (A(JSUBCV))
 720    JSUBCL = JSUBCL + IONE
        JSUBMJ = JSUBMJ + IONE
        JSUBCV = JSUBCV + IONE
        JSUBX  = JSUBX  + NR
 730  CONTINUE
      ASCANJ = FDPCON (DSCANJ)
      CALL SUMMAL (XS,IONE,SUM1)
C
      CALL SUMMAL (XS,IZERO,SUM2)
      JSUBCL = ISUBCL
      JSUBX  = ISUBX + IROW -IONE
      DO 740 J=1,NC
        CSUBJ  = A(JSUBCL)
        FIXR   = A(JSUBX)
        XS(1)  = FIXR * (RONE-FDIV(FIXR,CSUBJ,IND))
        CALL SUMMAL (XS,-IONE,SUM2)
        JSUBCL = JSUBCL + IONE
        JSUBX  = JSUBX  + NR
 740  CONTINUE
      CALL SUMMAL (XS,IONE,SUM2)
C
      CALL SUMMAL (XS,IZERO,SUM3)
      JSUBCL = ISUBCL
      JSUBMJ = ISUBMJ
      JSUBX  = ISUBX + IROW - IONE
      DO 750 J=1,NC
        CSUBJ  = A(JSUBCL)
        FMAXMJ = A(JSUBMJ)
        FIXR   = A(JSUBX)
        JSUBCL = JSUBCL + IONE
        JSUBMJ = JSUBMJ + IONE
        JSUBX  = JSUBX  + NR
        XS(1)  = FDIV (FMAXMJ*FIXR,CSUBJ,IND)
        CALL SUMMAL (XS,-IONE,SUM3)
 750  CONTINUE
      CALL SUMMAL (XS,IONE,SUM3)
C
      ANMRM  = AGSUMR-AMAXMD
      ANMRM2 = (AGSUMR-AMAXMD)**2
      SELAMA = FDIV(ANMRM*ANMRM*SUM1+((GSUM-ASUMMJ)**2)*
     1                SUM2-RTWO*ANMRM*(GSUM-ASUMMJ)*
     2                (ASCANJ-SUM3),ANMRM2,IND)
      SELAMA = AMAX1 (FDIV(SELAMA,ANMRM2,IND),RZERO)
      SELAMA = FSQRT (SELAMA)
C
C     COMPUTE SE(LAMBDA B).
C
      CALL SUMMAL (XS,IZERO,SUM1)
      DSRAIN = DZERO
      JSUBRV = ISUBRV
      JSUBRW = ISUBRW
      JSUBIM = ISUBIM
      JSUBX  = ISUBX + NR * (ICOL-IONE)
      DO 770 I=1,NR
        RSUBI  = A(JSUBRW)
        FMAXIM = A(JSUBIM)
        XS(1)  = FMAXIM * (RONE-FDIV(FMAXIM,RSUBI,IND))
        CALL SUMMAL (XS,-IONE,SUM1)
        IF (A(JSUBRV).NE.A(JSUBX)) GO TO 760
        DSRAIN = DSRAIN + DBLE (A(JSUBRV))
 760    JSUBRV = JSUBRV + IONE
        JSUBRW = JSUBRW + IONE
        JSUBIM = JSUBIM + IONE
        JSUBX  = JSUBX  + IONE
 770  CONTINUE
      ASRAIN = FDPCON (DSRAIN)
      CALL SUMMAL (XS,IONE,SUM1)
C
      JSUBRW = ISUBRW
      JSUBX  = ISUBX + NR * (ICOL-IONE)
      CALL SUMMAL (XS,IZERO,SUM2)
      DO 780 I=1,NR
        RSUBI  = A(JSUBRW)
        FIXC   = A(JSUBX)
        JSUBRW = JSUBRW + IONE
        JSUBX  = JSUBX  + IONE
        XS(1)  = FIXC * (RONE-FDIV(FIXC,RSUBI,IND))
        CALL SUMMAL (XS,-IONE,SUM2)
 780  CONTINUE
      CALL SUMMAL (XS,IONE,SUM2)
C
      JSUBRW = ISUBRW
      JSUBIM = ISUBIM
      JSUBX  = ISUBX + NR * (ICOL-IONE)
      CALL SUMMAL (XS,IZERO,SUM3)
      DO 790 I=1,NR
        RSUBI  = A(JSUBRW)
        FMAXIM = A(JSUBIM)
        FIXC   = A(JSUBX)
        JSUBRW = JSUBRW + IONE
        JSUBIM = JSUBIM + IONE
        JSUBX  = JSUBX  + IONE
        XS(1)  = FDIV (FMAXIM*FIXC,RSUBI,IND)
        CALL SUMMAL (XS,-IONE,SUM3)
 790  CONTINUE
      CALL SUMMAL (XS,IONE,SUM3)
C
      ANMCN  = AGSUMR-AMAXDM
      ANMCN2 = (AGSUMR-AMAXDM)**2
      SELAMB = FDIV(ANMCN*ANMCN*SUM1+((GSUM-ASUMIM)**2)*
     1                SUM2-RTWO*ANMCN*(GSUM-ASUMIM)*
     2                (ASRAIN-SUM3),ANMCN**2,IND)
      SELAMB = AMAX1 (FDIV(SELAMB,ANMCN2,IND),RZERO)
      SELAMB = FSQRT (SELAMB)
C
C     ..................................................................
C
C     PRINT FIRST PAGE OF RESULTS.
C
      CALL CONPG1 (NR,NC,AGSUMR,A(ISUBX),A(ISUBCO),A(ISUBRW),
     1             A(ISUBCL),A(ISUBE),A(ISUBD),A(ISUBTD))
C
C     ..................................................................
C
C     PRINT SECOND PAGE OF RESULTS.
C
      CALL CONPG2 (NR,NC,IDF,CHISQR,PROBAB)
C
C     ..................................................................
C
C     PRINT THIRD PAGE OF RESULTS.
C
      CALL CONPG3 (ICCT,IRCT)
C
      RETURN
C
C     ..................................................................
C
 800  CALL ERROR (205)
      RETURN
C
C     ==================================================================
C
      END
*CORPRT
      SUBROUTINE CORPRT (KEEP,KEEPA,KURT,LOT,LOTTE,NVAR,ERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CORPRT V 7.00  4/30/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     DO PRINT FOR CORRELATION INSTRUCTION.
C
C               WRITTEN BY -
C                      M. STUART SCOTT
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
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
      REAL             ERR(*)
      REAL             F, HL1, HL2, Z
      REAL             FDIV, FLOG, FSQRT, FTANH
      REAL             CONCFA,CONCFB,CRVALA,CRVALB
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA CRVALA /  2.5758293 /
      DATA CRVALB /  1.9599640 /
C
      DATA CONCFA / 99.0       /
      DATA CONCFB / 95.0       /
C
C     ==================================================================
C
C     FIRST PRINTING STAGE (SCC,PCC,SIGNIFICANCE LEVELS,SRCC).
C
      MXLINE = LENGTH
      IF (NCRT.EQ.IZERO ) MXLINE = 60
      IF (KEEP.EQ.IFOUR) GO TO 180
      NLA = IONE
      CALL PAGE (IFOUR)
      WRITE (IPRINT,220) NVAR, NRMAX
      LINE = IFIVE
      CALL MIST (NVAR,A(1),IONE,NLA,IONE,LINE,MXLINE)
      CALL MIST (NVAR,A(KURT+1),IONE,NLA,ITWO,LINE,MXLINE)
      IF (A(LOTTE+1).LE.RZERO) GO TO 10
      CALL MIST (NVAR,A(LOTTE+1),IONE,NLA,ITHRE,LINE,MXLINE)
      F = ABS (AMAX1(ERR(1),ERR(2),ERR(3)))
      NLA = NLA + ITWO
      IF (NRMAX.LE.NVAR) GO TO 10
      CALL MIST (NVAR,A(3*LOTTE+1),IONE,NLA,IFOUR,LINE,MXLINE)
      GO TO 30
  10  IF (NVAR.LE.ITWO) GO TO 30
      IF (LINE + IFIVE.LE.MXLINE) GO TO 20
      CALL PAGE (IFOUR)
      LINE = ITHRE
  20  WRITE (IPRINT,210) NRMAX, NVAR
      LINE = LINE + IFIVE
      IHOLD = IARGS(1)
      IARGS(1) = NVAR
      IF (KEEP.LT.ITHRE) CALL ERROR (240)
      IARGS(1) = IHOLD
  30  CALL MIST (NVAR,A(LOT+1),IONE,NLA,IFIVE,LINE,MXLINE)
      IF (NRMAX.GT.ITHRE) GO TO 50
      IF (LINE + ITWO.LE.MXLINE) GO TO 40
      CALL PAGE (IFOUR)
      LINE = ITHRE
  40  WRITE (IPRINT,230) NRMAX
      LINE = LINE + ITWO
      GO TO 130
  50  Z = FLOAT (NRMAX-ITHRE)
      IND = ITHRE * LOTTE + NRMAX + IONE
      DO 100 J=1,NVAR
        I1 = IARGS(J+1)
        IJ = (J-IONE) * NVAR
        DO 90 I=1,NVAR
          IF (I.NE.J) GO TO 60
          I2 = KURT + IJ + J
          A(I2) = RONE
          GO TO 90
  60      I2 = IARGS(I+1) - IONE
          DO 70 K=1,NRMAX
            K1 = K + I2
            K2 = K + ITHRE * LOTTE
            A(K2) = RC(K1)
  70      CONTINUE
          I2 = KURT + IJ + I
          CALL BJORCK (RC(I1),A(3*LOTTE+1),NRMAX,A(IND),F)
          IF (F.GT.RZERO) GO TO 80
          A(I2) = RONE
          GO TO 90
  80      CALL QFORF (RONE,Z,F,A(I2))
  90    CONTINUE
 100  CONTINUE
      CALL MIST (NVAR,A(KURT+1),IZERO,NLA,6,LINE,MXLINE)
C
C     CONFIDENCE LIMITS FOR SIMPLE CORRELATION COEFFICIENT.
C
      F = FSQRT (FLOAT(NRMAX-ITHRE))
      HL1 = FDIV (CRVALA,F,INB)
      HL2 = FDIV (CRVALB,F,INB)
      A(3*LOTTE) = CONCFA
      A(5*LOTTE) = CONCFB
      NVA = NVAR - IONE
      DO 120 J=1,NVA
        IND = (J-IONE) * NVAR
        K1 = IND + J + KURT
        K2 = K1 + KURT
        A(K1) = CONCFA
        A(K2) = CONCFB
        IJ = J + IONE
        DO 110 I=IJ,NVAR
C
C       INDEX OF SCC.
C
          I1 = IND + I
C
C         INDICES OF UPPER, LOWER SCC CONF. LIMITS (99 PER CENT LEVEL).
C
          K1 = (I-IONE) * NVAR + J + KURT
          K2 = K1 + KURT
          Z = RHALF * FLOG ( FDIV(RONE+A(I1),RONE-A(I1),INB) )
          A(K1) = AMIN1 (FTANH(Z+HL1),RONE)
          A(K2) = AMAX1 (FTANH(Z-HL1),-RONE)
C
C         INDICES OF UPPER, LOWER SCC CONF. LIMITS (95 PER CENT LEVEL).
C
          K1 = I1 + KURT
          K2 = K1 + KURT
          A(K1) = AMIN1 (FTANH(Z+HL2),RONE)
          A(K2) = AMAX1 (FTANH(Z-HL2),-RONE)
 110    CONTINUE
 120  CONTINUE
C
      CALL MIST (NVAR,A(KURT+1),IZERO,NLA,7,LINE,MXLINE)
 130  IF (KEEP.EQ.IONE) RETURN
      GO TO 180
C
C
C     STORE SIMPLE AND PARTIAL CORRELATION COEFFICIENTS IN WORKSHEET.
C
 140  IF (NRMAX.GT.NVAR .OR. NVAR.GT.ITWO) GO TO 150
      NARGS = NARGS - ITHRE
      CALL ERROR (29)
      RETURN
C
C     ..................................................................
C
 150  I1 = MIN0 (NVAR,NCOL-IDIV(IARGS(NVAR+5)-IONE,NROW,INB))
      I2 = MIN0 (NVAR,NROW-(IARGS(NVAR+4)-IARGS(NVAR+5)))
      IF (I1.LT.NVAR .OR. I2.LT.NVAR) CALL ERROR (213)
      DO 170 J=1,I1
        DO 160 I=1,I2
          K1 = (J-IONE) * NVAR + I + LOTTE
          K2 = IARGS(NVAR+4) - IONE + (J-IONE) * NROW + I
          RC(K2) = A(K1)
 160    CONTINUE
 170  CONTINUE
      RETURN
C
C     ..................................................................
C
 180  I1 = MIN0 (NVAR,NCOL-IDIV(IARGS(NVAR+3)-IONE,NROW,INB))
      I2 = MIN0 (NVAR,NROW-(IARGS(NVAR+2)-IARGS(NVAR+3)))
      IF (I1.LT.NVAR .OR. I2.LT.NVAR) CALL ERROR (213)
      DO 200 J=1,I1
        DO 190 I=1,I2
          K1 = (J-IONE) * NVAR + I
          K2 = IARGS(NVAR+2) - IONE + (J-IONE) * NROW + I
          RC(K2) = A(K1)
 190    CONTINUE
 200  CONTINUE
      IF (KEEPA.EQ.ITHRE) GO TO 140
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 210  FORMAT (  //1X,70HTHE PARTIAL CORRELATION COEFFICIENTS (AND SIGNIF
     1ICANCE LEVELS) ARE NOT/5X,61HDEFINED AND ARE NOT PRINTED, BECAUSE
     2THE NO OF MEASUREMENTS (,I4,1H)/5X,41HDOES NOT EXCEED THE NUMBER O
     3F VARIABLES (,I3,2H)./)
 220  FORMAT (  /10X,24HCORRELATION ANALYSIS FOR,I3,15H VARIABLES WITH,
     1     I5,13H OBSERVATIONS)
 230  FORMAT (1H ,10X,59HNONLINEARITY TEST AND APPROXIMATION OF CONFIDEN
     1CE INTERVALS/10X,23HNOT DEFINED FOR NRMAX =,I2)
C
C     ==================================================================
C
      END
*CORREL
      SUBROUTINE CORREL
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CORREL V 7.00  6/20/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE CORRELATION (N), (C), (C), ..., (C)
C
C     COMPUTE AND PRINT ...
C
C     (1)   SIMPLE CORRELATION COEFFICIENTS.
C     (2)   SIGNIFICANCE LEVELS OF SIMPLE CORRELATION COEFFICIENTS.
C     (3)   PARTIAL CORRELATION COEFFICIENTS.
C     (4)   SIGNIFICANCE LEVELS OF PARTIAL CORRELATION COEFFICIENTS.
C     (5)   SPEARMAN RANK CORRELATION COEFFICIENTS.
C     (6)   SIGNIFICANCE LEVELS OF QUADRATIC FIT OVER LINEAR FIT.
C     (7)   CONFIDENCE INTERVALS FOR SIMPLE CORRELATION COEFFICIENTS.
C
C               WRITTEN BY -
C                      M. STUART SCOTT
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                   CURRENT VERSION -  JUNE, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION APS(100)
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
C                         ***   TYPE STATEMENTS   ***
C
      REAL             AVG(100), ERR(4), SD(100), T(100), TEMP(1)
      REAL             CP, D, F, Z
      REAL             FDIV, FSQRT
      REAL             SPCA
C
C     ..................................................................
C
      EQUIVALENCE (SD(1),T(1))
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXVAR / 99 /
C
      DATA SPCA   /  0.4       /
C
C     ==================================================================
C
      K1     = IONE
      K2     = IONE
      ISUBSD = IONE
      ISUBT  = ISUBSD
      L2     = L2 - ITEN
      IF (L2.EQ.IONE .OR. NARGS.NE.IARGS(1)+IONE) GO TO 10
      CALL ERROR (236)
      RETURN
C
C     ..................................................................
C
  10  IF (NARGS.LT.ITHRE) CALL ERROR (10)
      NVAR = IARGS(1)
      IF (NVAR.LT.ITWO .OR. NVAR.GT.MAXVAR) CALL ERROR (3)
      KEEP = IDIV (NARGS-NVAR+IONE,ITWO,IND)
      KEEPA = KEEP
      IF (KEEP.EQ.ITHRE .AND. NRMAX.LE.NVAR .AND. L2.EQ.ITWO)
     1                 CALL ERROR (26)
      IF (KEEP.GE.IONE .AND. KEEP.LE.ITHRE .AND. MOD(NARGS-NVAR,ITWO)
     1     .EQ.IONE) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  20  GO TO (50,40,30), KEEP
  30  K2 = IARGS(NVAR+4)
      IF (K2.LT.IONE .OR. K2.GT.NROW) CALL ERROR (16)
      IARGS(NVAR+4) = IONE
  40  K1 = IARGS(NVAR+2)
      IF (K1.LT.IONE .OR. K1.GT.NROW) CALL ERROR (16)
      IARGS(NVAR+2) = IONE
  50  CALL CHKCOL
      GO TO (80,70,60), KEEP
  60  IARGS(NVAR+4) = IARGS(NVAR+5) + K2 - IONE
  70  IARGS(NVAR+2) = IARGS(NVAR+3) + K1 - IONE
  80  LOTTE = NVAR * NVAR
      KURT = ITWO * LOTTE
C
C     LOT IS SPACE IN ARRAY A RESERVED FOR RANKED DATA.
C
      LOT = MAX0 (NRMAX*(NVAR+IONE),ITHRE*LOTTE+8*NVAR+8,IFOUR*LOTTE)
      IF (NRMAX.LT.ITHRE) NRM = ITHRE
      IF (NRMAX.LT.ITHRE) CALL ERROR (34)
      IF (NRMAX*NVAR.GT.NRC) CALL ERROR (15)
      IF (MAX0(LOT+LOTTE+IHRD,NRMAX*IFOUR+ITHRE*LOTTE).GT.NS)
     1        CALL ERROR (23)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      NVA = NVAR - IONE
      IF (L2.EQ.ITWO) GO TO 150
C
C     RANKS OF OBSERVATIONS.
C
      IND   = NVAR * NRMAX + IONE
      KSUBT = ISUBT
      DO 90 I=1,NVAR
        K1 = (I-IONE) * NRMAX + IONE
        K2 = IARGS(I+1)
        CALL RANKX (NRMAX,RC(K2),A(IND),A(K1),T(KSUBT))
        KSUBT = KSUBT + IONE
  90  CONTINUE
C
C     SPEARMAN RANK CORRELATION COEFFICIENT.
C
      F     = IDIV ((NRMAX-IONE)*NRMAX*(NRMAX+IONE),6,IND)
      I1    = LOT + LOTTE
      A(I1) = RONE
      JSUBT = ISUBT
      DO 140 J=1,NVA
        IND   = (J-IONE) * NVAR + LOT
        I1    = IND + J
        A(I1) = RONE
        IJ    = J + IONE
        KSUBT = JSUBT + IONE
        DO 130 I=IJ,NVAR
          I1 = IND + I
          I2 = (I-IONE) * NVAR + J + LOT
          K1 = RTWO * T(KSUBT) + SPCA
          K2 = RTWO * T(JSUBT) + SPCA
          IF (IFIX(F).GT.K1 .AND. IFIX(F).GT.K2) GO TO 100
          A(I1) = RZERO
          GO TO 120
 100      D = RZERO
          DO 110 K=1,NRMAX
            K1 = (I-IONE) * NRMAX + K
            K2 = (J-IONE) * NRMAX + K
            D = D + (A(K1)-A(K2)) * (A(K1)-A(K2))
 110      CONTINUE
          A(I1) = FDIV (F-D-T(KSUBT)-T(JSUBT),FSQRT(F-RTWO*T(KSUBT))*
     1               FSQRT(F-RTWO*T(JSUBT)),INB)
          IF (ABS(A(I1)).GT.RONE) A(I1) = AINT(A(I1))
 120      A(I2) = A(I1)
          KSUBT = KSUBT + IONE
 130    CONTINUE
        JSUBT = JSUBT + IONE
 140  CONTINUE
C
C     MEANS OF OBSERVATIONS.
C
 150   KSUBVG = IONE
       DO 160 I=1,NVAR
        J = IARGS(I+1)
        CALL SUMMAL (RC(J),NRMAX,AVG(KSUBVG))
        IF (NRMAX.EQ.IONE) AVG(KSUBVG) = RC(J)
        AVG(KSUBVG) = FDIV (AVG(KSUBVG),FLOAT(NRMAX),IND)
        KSUBVG      = KSUBVG + IONE
 160  CONTINUE
C
C     STANDARD DEVIATIONS.
C
      KSUBVG = IONE
      KSUBSD = ISUBSD
      DO 180 I=1,NVAR
        CALL SUMMAL (TEMP,IZERO,SD(KSUBSD))
        K = IARGS(I+1)
        DO 170 J=1,NRMAX
          TEMP(1) = (RC(K)-AVG(KSUBVG)) * (RC(K)-AVG(KSUBVG))
          CALL SUMMAL (TEMP,-IONE,SD(KSUBSD))
          K = K + IONE
 170    CONTINUE
        KSUBVG = KSUBVG + IONE
        CALL SUMMAL (TEMP,IONE,SD(KSUBSD))
        SD(KSUBSD) = FSQRT(SD(KSUBSD))
        KSUBSD     = KSUBSD + IONE
 180  CONTINUE
C
C     SIMPLE CORRELATION COEFFICIENT.
C
      A(LOTTE) = RONE
      Z        = FLOAT (NRMAX-ITWO)
      JSUBVG   = IONE
      JSUBSD   = ISUBSD
      DO 230 J=1,NVA
        IND    = (J-IONE) * NVAR
        I1     = IND + J
        A(I1)  = RONE
        IJ     = J + IONE
        KSUBVG = IONE + J
        KSUBSD = ISUBSD + J
        DO 220 I=IJ,NVAR
          I1 = IND + I
          I2 = (I-IONE) * NVAR + J
          IF (SD(KSUBSD).GT.RZERO .AND. SD(JSUBSD).GT.RZERO) GO TO 190
          A(I1) = RZERO
          GO TO 210
 190      K1 = IARGS(I+1)
          K2 = IARGS(J+1)
          CALL SUMMAL (TEMP,IZERO,CP)
          DO 200 K=1,NRMAX
            TEMP(1) = (RC(K1)-AVG(KSUBVG)) * (RC(K2)-AVG(JSUBVG))
            CALL SUMMAL (TEMP,-IONE,CP)
            K1 = K1 + IONE
            K2 = K2 + IONE
 200      CONTINUE
          CALL SUMMAL (TEMP,IONE,CP)
          A(I1) = FDIV (CP,SD(KSUBSD)*SD(JSUBSD),INB)
          IF (ABS(A(I1)).GT.RONE) A(I1) = SIGN(RONE,A(I1))
 210      A(I2) = A(I1)
          KSUBVG  = KSUBVG + IONE
          KSUBSD  = KSUBSD + IONE
 220    CONTINUE
        JSUBVG    = JSUBVG + IONE
        JSUBSD    = JSUBSD + IONE
 230  CONTINUE
      IF (NVAR.LE.ITWO) GO TO 240
C
C     PARTIAL CORRELATION COEFFICIENT.
C
      IF (NRMAX.GT.NVAR) GO TO 250
C
 240  KEEP = MIN0 (KEEP,ITWO)
      A(LOTTE+1) = RZERO
      IF (L2.EQ.IONE) GO TO 340
      GO TO 400
C
 250  ILT = IONE + KURT
      JLT = ILT + NVAR + LOTTE
      KLT = JLT + NVAR
      MLT = KLT + NVAR
      CALL INVCHK (A(1),NVAR,A(LOTTE+1),A(ILT),NVAR,A(JLT),A(KLT),
     1             A(MLT),SD(ISUBSD),APS(1),1,ERR,IRR)
      IF (IRR.NE.IZERO) CALL ERROR (249)
C
      DO 270 J=1,NVA
        IND = LOTTE + (J-IONE) * NVAR
        K1 = IND + J
        IJ = J + IONE
        DO 260 I=IJ,NVAR
          K2 = LOTTE + (I-IONE) * NVAR + I
          I1 = IND + I
          I2 = LOTTE + (I-IONE) * NVAR + J
          A(I1) = FDIV (-A(I1),FSQRT(A(K1))*FSQRT(A(K2)),INB)
          IF (ABS(A(I1)).GT.RONE) A(I1) = SIGN(RONE,A(I1))
          A(I2) = FDIV (-A(I2),FSQRT(A(K1))*FSQRT(A(K2)),INB)
          IF (ABS(A(I2)).GT.RONE) A(I2) = SIGN(RONE,A(I2))
 260    CONTINUE
 270  CONTINUE
C
      DO 280 I=1,NVAR
        I1 = LOTTE + (I-IONE) * NVAR + I
        A(I1) = RONE
 280  CONTINUE
      IF (L2.EQ.ITWO) GO TO 400
      IF (NRMAX.LE.NVAR) GO TO 340
C
C     SIGNIFICANCE LEVEL OF PARTIAL CORRELATION COEFFICIENT.
C
      Z = FLOAT (NRMAX-NVAR)
      DO 330 J=1,NVAR
        IJ = (J-IONE) * NVAR + LOTTE
        IND = IJ + KURT
        DO 320 I=J,NVAR
          I1 = IND + I
          I2 = (I-IONE) * NVAR + J + ITHRE * LOTTE
          K1 = IJ + I
          IF (A(K1).NE.RZERO) GO TO 290
          A(I1) = RONE
          GO TO 310
 290      IF (ABS(A(K1)).LT.RONE) GO TO 300
          A(I1) = RZERO
          GO TO 310
 300      F = A(K1) * A(K1)
          F = FDIV (Z*F,RONE-F,INB)
          CALL QFORF (RONE,Z,F,A(I1))
 310      A(I2) = A(I1)
 320    CONTINUE
 330  CONTINUE
C
 340  Z = FLOAT (NRMAX-ITWO)
      DO 390 J=1,NVAR
        IJ = (J-IONE) * NVAR
        IND = IJ + KURT
        DO 380 I=J,NVAR
          I1 = IND + I
          I2 = (I-IONE) * NVAR + J + KURT
          K1 = IJ + I
          IF (A(K1).NE.RZERO) GO TO 350
          A(I1) = RONE
          GO TO 370
 350      IF (ABS(A(K1)).LT.RONE) GO TO 360
          A(I1) = RZERO
          GO TO 370
 360      F = A(K1) * A(K1)
          F = FDIV (Z*F,RONE-F,INB)
          CALL QFORF (RONE,Z,F,A(I1))
 370      A(I2) = A(I1)
 380    CONTINUE
 390  CONTINUE
      GO TO 410
C
 400  IF (KEEP.EQ.IONE) RETURN
      KEEP = IFOUR
C
 410  CALL CORPRT (KEEP,KEEPA,KURT,LOT,LOTTE,NVAR,ERR)
      RETURN
C
C     ==================================================================
C
      END
*CVTDEG
      SUBROUTINE CVTDEG
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CVTDEG V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INSTRUCTIONS EXECUTED ARE CTOF AND FTOC.
C
C     THE VALUES OF L2 ARE ...
C        1 - CTOF (CONVERT DEGREES CELSIUS TO FAHRENHEIT)
C        2 - FTOC (CONVERT DEGREES FAHRENHEIT TO CELSIUS)
C
C     FORMS OF INSTRUCTIONS ARE AS FOLLOWS ...
C        CTOF OF (E) PUT IN COLUMN (C)
C        FTOC OF (E) PUT IN COLUMN (C)
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      INCLUDE 'WRKSCR.H'
C
      REAL             X, T
      REAL             FDIV
      REAL             SPCA, SPCB, SPCC
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA /  32.0  /
      DATA SPCB /   1.8  /
      DATA SPCC / 273.15 /
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
  10  CALL ADRESS (ITWO,I2)
      IF (I2.LT.IZERO) CALL ERROR (20)
      CALL ADRESS (IONE,I1)
      IF (NRMAX.LE.IZERO) CALL ERROR (9)
      IF (NERROR.NE.IZERO) RETURN
C
C     ==================================================================
C
      IE = IZERO
C
      IF (L2.EQ.2) GO TO 60
C
C     INSTRUCTION IS CTOF (CONVERT CELSUIS TO FAHREN.)
C
      IF (I1.LT.IZERO) GO TO 40
      DO 30 J=1,NRMAX
        X = RC(I1)
        IF (X.GE.-SPCC) GO TO 20
        X = ABS(X)
        IF (IE.NE.IZERO) GO TO 20
        CALL ERROR (234)
        IE = IONE
  20    RC(I2) = SPCB * X + SPCA
        I1 = I1 + IONE
        I2 = I2 + IONE
  30  CONTINUE
      RETURN
C
C     ..................................................................
C
  40  X = ARGS(1)
      IF (X.GE.-SPCC) GO TO 50
      X = ABS(X)
      CALL ERROR (234)
  50  X = SPCB * X + SPCA
      CALL VECTOR (X,I2)
      RETURN
C
C     ..................................................................
C
C     INSTRUCTION IS FTOC (CONVERT FAHREN. TO CELSUIS)
C
  60  IF (I1.LT.IZERO) GO TO 90
      DO 80 J=1,NRMAX
        T = RC(I1)
        X = FDIV (T - SPCA,SPCB,IND)
        IF (X.GE.-SPCC) GO TO 70
        T = ABS(T)
        X = FDIV (T - SPCA,SPCB,IND)
        IF (IE.NE.IZERO) GO TO 70
        CALL ERROR (234)
        IE = IONE
  70    RC(I2) = X
        I1 = I1 + IONE
        I2 = I2 + IONE
  80  CONTINUE
      RETURN
C
C     ..................................................................
C
  90  T = ARGS(1)
      X = FDIV (T - SPCA,SPCB,IND)
      IF (X.GE.-SPCC) GO TO 100
      T = ABS(T)
      X = FDIV (T - SPCA,SPCB,IND)
      CALL ERROR (234)
 100  CALL VECTOR (X,I2)
      RETURN
C
C     ==================================================================
C
      END
