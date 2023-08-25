*SANDL    
      SUBROUTINE SANDL (XS,SPDPT,ICASE,INND)      
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81.  SANDL V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     PERFORM PRINTING OF STEM LEAF DISPLAYS.     
C         
C     ALGORITHM DEVELOPED BY WESLEY NICHOLSON.    
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - OCTOBER, 1973.        
C                   CURRENT VERSION -   APRIL, 1992.        
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION IDATA(65,2)   
      DIMENSION ISPVX(2), ISTORE(133)   
      DIMENSION ITITLE(12), LLINE(8), ITIT(12)    
      DIMENSION KDEPTH(5), KLEAF(5), KSCRL(6), KSTEM(5)     
C         
      COMMON /ABCDEF/ LA(74)  
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
C         
C     NOTE. 40 = 2*(MXWDTH+1) IN IB(40).  CHANGE, IF MXWDTH IS CHANGED.         
C         
      COMMON /SLCONS/ MXLIN, MXWDTH     
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT      
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ         
      COMMON /SLEAFC/ IJ, IL, INN, INNI, IPERJ, IPRT, IQN, IR, ISWT, IW         
      COMMON /SLEAFD/ IXN, JPERST, JSPA, KC, KDPTH, LMAXA, LRND, MSAL 
      COMMON /SLEAFE/ NDIV, NSAL, NSALST, NSALTP, NSP, NSPP, NSTOP    
      COMMON /SLICHR/ IB(40), IC(6)
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C         
C     IDTHST = 0  DO NOT STORE DEPTH    
C     IDTHST .GT. 0  DIMENSION SIZE OF VECTOR SPDPT         
C     VECTOR SPDPT AND DEPTH STORED     
C         
C     ==================================================================        
C         
C                         ***   TYPE STATEMENTS   ***   
C         
      REAL             SPDPT(*), XS(NZ) 
      REAL             X(1)   
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        LFMTP*80         
      CHARACTER*1      IB, IC
      CHARACTER*1      IDATA, ISTORE, KDEPTH, KLEAF, KSCRL, KSTEM
      CHARACTER        ITITLE*1
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
      DATA KDEPTH(1), KDEPTH(2), KDEPTH(3), KDEPTH(4), KDEPTH(5) /    
     1           'D',       'E',       'P',       'T',       'H' /    
C         
      DATA KLEAF(1), KLEAF(2), KLEAF(3), KLEAF(4), KLEAF(5) /         
     1          'L',      'E',      'A',      'F',      ' ' /         
      DATA KSCRL(1), KSCRL(2), KSCRL(3), KSCRL(4), KSCRL(5), KSCRL(6) /         
     1          'S',      'C',      'R',      'A',      'W',      'L' /         
      DATA KSTEM(1), KSTEM(2), KSTEM(3), KSTEM(4), KSTEM(5) /         
     1          'S',      'T',      'E',      'M',      ' ' /         
C         
C     ==================================================================        
C         
C     INTIALIZE CERTAIN VARIABLES.      
C        DETERMINE WIDTH OF STEM FOR PRINTING     
C           AND CHECK I,J,K AND L.      
C         
C     IF I NE 0 AND ALL DATA HAVE THE SAME SIGN, I IS USED  
C        TO DENOTE THE NUMBER OF DIGITS IN THE STEM WHICH DIFFER IN   
C        THE MINIMUM AND MAXIMUM.       
C         
      INND = IZERO  
      INNI = IZERO  
      INN  = IZERO  
      IF (JZ.LT.IONE .OR. JZ.GE.ISIGD) GO TO 350  
      IF (LZ.LT.IONE .OR. LZ.GE.ISIGD) GO TO 360  
      IF (JZ+LZ.GT.ISIGD) GO TO 330     
      NNMX   = MXWDTH + IONE + JSTRT    
      NMMX   = MXWDTH + ITWO  
      ILF    = ILEAF
      LMAXA  = IZERO
      LBAR   = IZERO
      MW     = NWSL 
      MND    = NDSL 
      IPRT   = IPET 
      IF (IPRT.GE.IONE .AND. ICASE.EQ.IZERO) IPRT = MXLIN   
      IF (IDTHST.GT.IZERO) KDPTH = IONE 
      CALL RFORMT (1,ISIGD,X,XS(1),0,0,MW,MND,IB(1),IRF)    
      CALL RFORMT (1,ISIGD,X,XS(NZ),0,0,MW,MND,IB(NMMX),IRF)
      IF (JZ.EQ.IPER) JZ = JZ + IONE    
      IF (JZ.LT.IPER .AND. IPER.LE.JZ+LZ) LBAR = IONE       
      LRND = JZ + LZ + LBAR   
      KC = NWSL - JSTRT + IONE
      JJSTRT = JSTRT
      NNNMX = NNMX  
      KD = KC       
      MRND = LRND   
      IF (JSTRT.EQ.IONE) GO TO 10       
      JJSTRT = JSTRT - IONE   
      NNNMX = NNMX - IONE     
      MRND = LRND + IONE      
      KD = KC + IONE
  10  CALL RNDATM (IB(JJSTRT),KD,MRND,IND)        
      IF (IND.NE.IZERO) GO TO 320       
      CALL RNDATM (IB(NNNMX),KD,MRND,IND)         
      IF (IND.NE.IZERO) GO TO 320       
      IF (IB(JJSTRT).EQ.LA(45) .AND. IB(NNNMX).EQ.LA(45)) GO TO 20    
      IF (IB(JJSTRT).EQ.LA(1) .AND. IB(NNNMX).EQ.LA(45)) GO TO 20     
      IF (IB(NNNMX).EQ.LA(1) .AND. IB(JJSTRT).EQ.LA(45)) GO TO 20     
      JSTRT = JJSTRT
      NNMX  = NNNMX 
      MRND  = JSTRT + LRND - IONE       
      IF (IB(MRND).EQ.LA(38)) LRND = LRND + IONE  
      KC = KD       
  20  IF (ILEAF.EQ.IZERO) GO TO 60      
      ILEAF  = JZ   
      IPERJ  = IONE 
      JPERST = IZERO
      IF (IB(JJSTRT).EQ.LA(45) .AND. IB(NNNMX).EQ.LA(1)) GO TO 30     
      IF (IN.LE.IZERO .OR. IP+IZ.LE.IZERO) GO TO 30         
      ISB = JSTRT + MXWDTH + IONE       
      IF (IB(JSTRT).NE.LA(38) .OR. IB(ISB).NE.LA(38)) GO TO 60        
      ILEAF = ILEAF - IONE    
      INNI  = IONE  
      GO TO 50      
  30  ISA = JSTRT   
      ISB = JSTRT + MXWDTH + IONE       
      DO 40 IS=1,JZ 
        IF (IB(ISA).NE.IB(ISB)) GO TO 60
        ISA   = ISA + IONE    
        ISB   = ISB + IONE    
        IF (IB(ISA-1).NE.LA(38)) GO TO 40         
        JPERST = IONE         
        ILEAF  = ILEAF - IONE 
        IPERJ  = IZERO        
        GO TO 50    
  40  CONTINUE      
C         
  50  IF (ILEAF.EQ.IZERO) GO TO 340     
  60  CALL CINDEX (IB(JSTRT),ILEAF,JZ,KZ,IL,IND)  
      IF (IND.NE.IZERO) GO TO 380       
      IQMIN = IONE  
      CALL CINDEX (IB(NNMX),ILEAF,JZ,KZ,IU,IND)   
      IF (IND.NE.IZERO) GO TO 380       
      IA  = IABS (IU-IL) + IONE         
      IF (IN.NE.IZERO) GO TO 80         
      IJ  = IZERO   
      IQN = IZERO   
  70  IR  = IA      
      IXN = IL - IONE         
      GO TO 100     
  80  IF (IP.NE.IZERO) GO TO 90         
      IJ  = IN      
      IQN = IA      
      IF (IB(JJSTRT).EQ.LA(45) .AND. IB(NNNMX).EQ.LA(1)) INN = IONE   
      GO TO 70      
  90  IJ  = IN + IDIV (IZ,ITWO,IND)     
      IQN = IL      
      IR  = IL + IU 
      IXN = -IL     
      IF (IB(JJSTRT).EQ.LA(45) .AND. IB(NNNMX).EQ.LA(1)) INN = IONE   
 100  ISWT = IONE   
      IF (ILEAF.EQ.IZERO) GO TO 110     
      IF (KZ.NE.IONE) ISWT = ITWO       
      GO TO 120     
C         
C     MIXED MODE.   
C         
 110  ISWT = ITHRE  
      IF (KZ.EQ.ITHRE) ISWT = IFOUR     
C         
C     ISTEM IS COUNT OF TOTAL NUMBER OF SPACES NEEDED FOR STEM PRINT. 
C         
 120  ISTEM = IZERO 
      GO TO (130,150,130,140), ISWT     
 130  IF (KZ.EQ.IONE) GO TO 160         
      GO TO 370     
 140  IF (KZ.EQ.ITHRE) GO TO 160        
      GO TO 370     
 150  IF (KZ.NE.ITWO .AND. KZ.NE.IFOUR .AND. KZ.NE.ITEN) GO TO 370    
 160  IW = IZERO    
      IF (IN.NE.IZERO) IW = IONE        
      IQMAX = (-IU) + IXN     
      IF (NZ.GT.IJ) IQMAX = (-IQMAX)    
      CALL IXLINE (IQMIN,IQN,IL,IXN,IXMIN)        
      CALL IXLINE (IQMAX,IQN,IL,IXN,IXMAX)        
      IXT = MAX0 (IXMIN,IXMAX)
      MR = IDIV (IXT-ITWO,ITHRE*KZ,IND) 
      IF (MR.LT.IZERO .OR. ILEAF.NE.IZERO) MR = IZERO       
      IV = IONE     
      IF (ILEAF.NE.IZERO) IV = IZERO    
      IUX = IZERO   
      IF (ILEAF.GT.IZERO) IUX = JZ      
      ISTEM = IW + MR + IV + IUX        
      IF (KZ.EQ.IONE .AND. ILEAF.EQ.IZERO) ISTEM = ISTEM + ITWO*IW+IFOUR        
      IF (KZ.EQ.IONE .AND. ILEAF.NE.IZERO) ISTEM = ITWO * ISTEM + IONE
      NSAL = IONE   
      NT = ITEN     
 170  IF (NT.GT.NZ) GO TO 180 
      NT = NT * ITEN
      NSAL = NSAL + IONE      
      GO TO 170     
C         
C     NSAL = MAXIMUM NUMBER OF CHARACTER POSITIONS FOR DEPTH.         
C     NSP = STARTING POSITION FOR STORING STEM IN PRINT LINE.         
C     NSTOP = LAST POSITION OF STEM IN ISTORE.    
C        ISTORE     
C         
 180  NSALST = IONE 
      NSALTP = NSAL 
      IF (NSAL.GT.7) GO TO 190
      NSAL   = IFIVE
      NSALST = IDIV (NSAL-NSALTP,ITWO,IND) + NSALST         
      NSALTP = NSALST + NSALTP - IONE   
 190  NSP    = NSAL + 8       
      IF (ISTEM.LT.IFIVE) NSP = NSP + IFIVE - ISTEM         
      NDIV  = IDIV (NT,ITEN,IND)        
      NSTOP = NSP + ISTEM - IONE        
C         
C     LSTRT = STARTING POSITION OF LEAF.
C         
      LSTRT = NSTOP + 6       
      NSPP  = NSP   
      IF (LSTRT.GE.LUPPER) GO TO 310    
      DO 200 IS=1,133         
        ISTORE(IS) = LA(45)   
 200  CONTINUE      
      IF (IPRT.EQ.IZERO) GO TO 250      
C         
C     NSTOP IS NOT USED IN CALL TO PREPAK.        
C         
      CALL PREPAK (ITHRE,NSTOP,IARGS(1),ITIT,ITITLE,LFMTP,KPT)   
      CALL PAGE (IFOUR)       
      IF (KPT.EQ.IZERO) GO TO 210       
      WRITE (IOUT,410) IARGS(1)         
      GO TO 220     
 210  WRITE (IOUT,420) ITITLE 
 220  WRITE (IOUT,400)        
      KS  = NSAL - IFOUR      
      KSS = NSAL + ITWO       
      KSM = NSTOP - ITHRE     
      KLF = LSTRT   
      DO 230 IS=1,5 
        ISTORE(KS) = KDEPTH(IS)         
        ISTORE(KSS) = KSCRL(IS)         
        ISTORE(KSM) = KSTEM(IS)         
        ISTORE(KLF) = KLEAF(IS)         
        KS  = KS + IONE       
        KSS = KSS + IONE      
        KSM = KSM + IONE      
        KLF = KLF + IONE      
 230  CONTINUE      
      ISTORE(KSS) = KSCRL(6)  
C         
      WRITE (IOUT,390) (ISTORE(IS),IS=1,KLF)      
      DO 240 IS=1,133         
        ISTORE(IS) = LA(45)   
 240  CONTINUE      
      WRITE (IOUT,400)        
 250  ISP = NSP     
C         
C     SETUP PART OF STEM WHICH DOES NOT CHANGE, IF POSSIBLE.
C        PRESET  U(1) ... U(J-I) IF J-I.GT.0 AND I.NE.0.    
C         
      IUSWT = IZERO 
      MSAL  = NSP - IONE + IW 
      IF (ISWT.GE.ITHRE) GO TO 300      
      IUJ   = JZ - ILEAF      
      JSPA  = NSP + ILEAF + IW + IONE   
      IF (ILEAF.EQ.JZ .AND. ISWT.LE.IONE) GO TO 290         
      IF (ILEAF.EQ.JZ .AND. ISWT.GT.IONE) GO TO 300         
      IF (INNI.EQ.IONE .AND. ISWT.LE.IONE) GO TO 290        
      IF (INNI.EQ.IONE .AND. ISWT.GT.IONE) GO TO 300        
      IUSWT = IUJ + IW        
      IUJ   = IUJ + JSTRT - IONE        
      IF (IW.EQ.IZERO) GO TO 260        
      ISTORE(NSP) = LA(39)    
      IW    = IZERO 
      NSP   = NSP + IONE      
      MSAL  = MSAL - IONE     
 260  DO 270 IS=JSTRT,IUJ     
        ISTORE(NSP) = IB(IS)  
        NSP = NSP + IONE      
 270  CONTINUE      
C         
C     NSP LOCATION OF VARIABLE PART OF STEM TO BE STORED.   
C         
      IF (ISWT.EQ.ITWO) GO TO 300       
      JSP = NSP + ILEAF + IONE
      LSP = ISP     
      DO 280 IS=1,IUSWT       
        ISTORE(JSP) = ISTORE(LSP)       
        JSP = JSP + IONE      
        LSP = LSP + IONE      
 280  CONTINUE      
      JSPA = JSP    
 290  JSP  = NSP + ILEAF + IW + INNI    
      ISTORE(JSP) = LA(37)    
 300  CALL SLFPRT (ICASE,IDATA,ISPVX,ISTORE,LINE,LLINE,SPDPT,XS,INND) 
      IF (INND.NE.IZERO) RETURN         
      IF (NCRT.NE.IZERO) CALL PAGE (4)
      CALL SLPTSC (ICASE,IDATA,ILF,IPRT,ISPVX,KC,LINE,LLINE,LMAXA,    
     1                                                LRND,INND)      
      RETURN        
C         
C     ..................................................................        
C         
 310  INND = 8      
      RETURN        
C         
C     ..................................................................        
C         
 320  INND = ITEN   
      RETURN        
C         
C     ..................................................................        
C         
 330  INND = 6      
      RETURN        
C         
C     ..................................................................        
C         
 340  INND = ITWO   
      RETURN        
C         
C     ..................................................................        
C         
 350  INND = IFOUR  
      RETURN        
C         
C     ..................................................................        
C         
 360  INND = IFIVE  
      RETURN        
C         
C     ..................................................................        
C         
 370  INND = 7      
      RETURN        
C         
C     ..................................................................        
C         
 380  INND = 9      
      RETURN        
C         
C     ==================================================================        
C         
C                       ***   FORMAT STATEMENTS   ***       
C         
 390  FORMAT (1X,70A1)        
 400  FORMAT (1H )  
 410  FORMAT (35H  STEM AND LEAF DISPLAY FOR COLUMN ,I3)    
 420  FORMAT (30H  STEM AND LEAF DISPLAY FOR   ,12A1)       
C         
C     ==================================================================        
C         
      END 
*SCALE
      SUBROUTINE SCALE (IS,NC,N,M,IT,LC,NR,W,WC,X,U,Q,SS,B,A,Z,R,SF,IFT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  SCALE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SCALE SCALES THE MATRIX Q IN ORDER TO MITIGATE THE
C        ROUNDING ERROR PROBLEMS WHICH CAN OCCUR IN CONNECTION WITH
C        SOLVING ILL-CONDITIONED SYSTEMS OF EQUATIONS.  THIS IS DONE BY
C        MULTIPLYING EACH COLUMN OF Q BY ITS APPROPRIATE SCALE FACTOR SO
C        THAT THE COLUMNS OF THE SCALED MATRIX ALL HAVE UNIT LENGTH.  IN
C        THE CASE OF POLYNOMIAL TYPE PROBLEMS, THE MEAN OF THE X-VECTOR
C        IS COMPUTED SO THAT IT CAN BE SUBTRACTED FROM EACH ELEMENT OF
C        X WHENEVER POWERS OF X ARE GENERATED (IN SUBROUTINES LSQ AND
C        SLVE).  AFTER A SOLUTION IS OBTAINED FOR A SCALED PROBLEM, THE
C        COEFFICIENTS, RESIDUALS, SQUARED FOURIER COEFFICIENTS AND
C        COVARIANCE MATRIX MUST BE ADJUSTED TO ACCOUNT FOR SCALING.
C
C     REFERENCE --
C        A. BJORCK, COMMENT ON THE ITERATIVE REFINEMENT OF LEAST-SQUARES
C        SOLUTIONS, JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C        VOL. 73 (1978), PP. 161-166.
C
C     INPUT PARAMETERS ...
C
C     IS = 0 MEANS NO SCALING IS TO BE DONE.
C     IS = 1 MEANS SCALING IS TO BE DONE.
C     NC = 1 AND IS = 0 MEANS SET SCALE FACTORS (SF) EQUAL TO 1.
C     NC = 1 AND IS = 1 MEANS COMPUTE VECTOR NORMS, COMPUTE SCALE
C                       FACTORS (SF), AND SCALE MATRIX Q.
C     NC = 2 MEANS COMPUTE MEAN OF X-VECTOR FOR POLYNOMIAL TYPE PROBLEM.
C     NC = 3 MEANS ADJUST SQUARED FOURIER COEFFICIENTS (SS) AND
C            RESIDUALS (Z) FOR SCALING.
C     NC = 4 MEANS ADJUST COEFFICIENTS (B AND A) AND COVARIANCE MATRIX
C            (R) FOR SCALING.
C     N    NUMBER OF OBSERVATIONS.
C     M    NUMBER OF UNKNOWN COEFFICIENTS.
C     IT   PARAMETER WHICH SPECIFIES WHETHER OR NOT A POLYNOMIAL TYPE
C             FIT IS TO BE PERFORMED.
C             IT = 1 INDICATES POLYNOMIAL TYPE.
C             IT = 2 INDICATES NON-POLYNOMIAL TYPE.
C     LC   VECTOR (M BY 1) WHICH GIVES THE LOCATION WITHIN THE ARRAY X
C             OF THE INDEPENDENT VARIABLES TO WHICH THE OBSERVATIONS ARE
C             TO BE FITTED.  SEE SUBROUTINE LSQ FOR FURTHER DETAILS.
C
C     W    VECTOR (N BY 1) OF WEIGHTS.
C     WC   CONSTANT USED FOR WEIGHTS INSTEAD OF VECTOR W(.).
C             IF WC = 0, VECTOR W IS USED FOR WEIGHTS.  OTHERWISE
C                        WEIGHTS EQUAL WC.
C     X    MATRIX (N BY M) OF INDEPENDENT VARIABLES WHICH ARE TO BE
C             FITTED, STORED AS A VECTOR OF LENGTH (N*M BY 1), AS IN
C             SUBROUTINE LSQ.
C
C     INPUT AND OUTPUT PARAMETERS ...
C
C     U    MEAN OF X-VECTOR IN SCALED POLYNOMIAL TYPE PROBLEMS.
C     Q    MATRIX OF SIZE (N BY M+1) STORED AS A VECTOR.
C     SS   VECTOR (M+2 BY 1) OF SQUARED FOURIER COEFFICIENTS.
C     B    VECTOR (M BY 1) OF COEFFICIENTS.
C     A    VECTOR (M BY 1) OF COEFFICIENTS FROM REFIT OF PREDICTED
C             VALUES.
C     Z    VECTOR (N BY 1) OF RESIDUALS.
C     R    COVARIANCE MATRIX (STORED AS A VECTOR OF LENGTH
C             (M+1)*(M+2)/2).
C     SF   VECTOR (M+1 BY 1) OF SCALE FACTORS.  WHEN NC = 4 AND IT = 1,
C             SF IS ALSO USED AS WORK AREA.
C
C     OUTPUT PARAMETER ...
C
C     IFT = 0 MEANS MATRIX Q IS NOT SINGULAR AND EVERYTHING IS FINE.
C     IFT = 1 MEANS MATRIX Q IS SINGULAR AND COMPUTATION CANNOT BE
C             COMPLETED.
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
      DIMENSION LC(*)
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(*), B(*), Q(*), R(*), SF(*), SS(*)
      REAL             W(*), X(*), Z(*)
      REAL             U, WC
      REAL             VNORM2, WW
      REAL             FDIV, FDPCON
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      MP1 = M + IONE
      IFT = IZERO
      GO TO (10,80,100,130), NC
C
  10  IF (IS.EQ.IONE) GO TO 30
C
C     IS = 0.  SET SF(I) = 1.0 FOR I=1,...,M+1.
C
      DO 20 I=1,MP1
        SF(I) = RONE
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
C     IS = 1.  COMPUTE VECTOR NORMS.
C                  COMPUTE SCALE FACTORS (SF).
C                  SCALE MATRIX Q.
C
  30  WW = WC
      DO 70 J=1,MP1
        DSUM = DZERO
        K = (J-IONE) * N + IONE
        DO 40 I=1,N
          IF (WC.LE.RZERO) WW = W(I)
          DSUM = DSUM + DBLE (Q(K)) * DBLE (Q(K)) * DBLE (WW)
          K = K + IONE
  40    CONTINUE
        DSUM   = FDSQRT (DSUM)
        VNORM2 = FDPCON (DSUM)
C
C       VECTOR NORMS COULD BE SAVED HERE, IF DESIRED.
C
        IF (VNORM2.GT.RZERO) GO TO 50
        CALL ERROR (22)
        IFT = IONE
C
C       IFT = 1 INDICATES ERROR RETURN.
C
        RETURN
C
C     ..................................................................
C
  50    SF(J) = FDIV (RONE,VNORM2,IRR)
C
C       SCALE MATRIX Q.
C
        K = (J-IONE) * N + IONE
        DO 60 I=1,N
          Q(K) = Q(K) * SF(J)
          K    = K + IONE
  60    CONTINUE
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COMPUTE MEAN OF X VECTOR (DENOTED BY U) FOR POLYNOMIAL TYPE
C        PROBLEMS.
C
  80  DSUM = DZERO
      NW   = IZERO
      L    = (LC(1) - IONE) * NR
      DO 90 I=1,N
        L    = L + IONE
        IF (WC.LE.RZERO .AND. W(I).EQ.RZERO) GO TO 90
        NW   = NW + IONE
        DSUM = DSUM + DBLE (X(L))
  90  CONTINUE
      U = FDPCON (FDDIV (DSUM,DBLE (FLOAT (NW)),IRR))
      RETURN
C
C     ..................................................................
C
C     ADJUST SQUARED FOURIER COEFFICIENTS (SS) AND RESIDUALS (Z) FOR
C        SCALING.
C
 100   DO 110 J=1,M
        SS(J) = FDIV (SS(J),SF(MP1)*SF(MP1),IRR)
 110  CONTINUE
C
      DO 120 I=1,N
        Z(I) = FDIV (Z(I),SF(MP1),IRR)
 120  CONTINUE
      RETURN
C
C     ..................................................................
C
C     ADJUST COEFFICIENTS (B AND A) AND COVARIANCE MATRIX (R) FOR
C        SCALING.
C
 130  DO 140 J=1,M
        B(J) = FDIV (B(J) * SF(J),SF(MP1),IRR)
        A(J) = FDIV (A(J) * SF(J),SF(MP1),IRR)
 140  CONTINUE
      L = IZERO
      DO 160 I=1,M
        DO 150 J=I,M
          L    = L + IONE
          R(L) = R(L) * SF(I) * SF(J)
 150    CONTINUE
 160  CONTINUE
      IF (IT.EQ.ITWO) RETURN
C
C     ..................................................................
C
C     COMPLETE ADJUSTMENTS OF B, A AND R FOR SCALING IN POLYNOMIAL TYPE
C        PROBLEMS.
C     REFERENCE --
C        G. A. F. SEBER, LINEAR REGRESSION ANALYSIS (1977), THEOREM
C        1.4 AND COROLLARIES, PAGES 10-11.
C
      K = IZERO
      DO 180 I=1,M
        DO 170 J=I,M
          K = K + IONE
          L = (I - IONE) * M + J
          Q(L) = R(K)
          IF (I.EQ.J) GO TO 170
          L = (J - IONE) * M + I
          Q(L) = R(K)
 170    CONTINUE
 180  CONTINUE
      DO 250 I=1,M
        SF(I) = RONE
        IP1   = I + IONE
        IF (IP1.GT.M) GO TO 200
        DO 190 J=IP1,M
          SF(J) = FDPCON (-FDDIV (DBLE(FLOAT(J-1)),DBLE(FLOAT(J-I)),IND)
     1    * DBLE (SF(J-1)) * DBLE (U) )
 190    CONTINUE
 200    DSUM = DZERO
        DO 210 J=I,M
          DSUM = DSUM + DBLE (SF(J)) * DBLE (B(J))
 210    CONTINUE
        B(I) = DSUM
        DSUM = DZERO
        DO 220 J=I,M
          DSUM = DSUM + DBLE (SF(J)) * DBLE (A(J))
 220    CONTINUE
        A(I) = DSUM
        DO 240 J=I,M
          DSUM = DZERO
          DO 230 K=I,M
            L = (K-1)*M + J
            DSUM = DSUM + DBLE (SF(K)) * DBLE (Q(L))
 230      CONTINUE
          L    = (I - IONE) * M + J
          Q(L) = DSUM
 240    CONTINUE
 250  CONTINUE
      DO 300 J=1,M
        SF(J) = RONE
        IP1   = J + IONE
        IF (IP1.GT.M) GO TO 270
        DO 260 I=IP1,M
          SF(I) = FDPCON (-FDDIV (DBLE(FLOAT(I-1)),DBLE(FLOAT(I-J)),IND)
     1    * DBLE (SF(I-1)) * DBLE (U) )
 260    CONTINUE
 270    DO 290 I=1,J
          DSUM = DZERO
          DO 280 K=J,M
            L    = (I - IONE) * M + K
            DSUM = DSUM + DBLE (Q(L)) * DBLE (SF(K))
 280      CONTINUE
          L    = (I - IONE) * M + J
          Q(L) = DSUM
 290    CONTINUE
 300  CONTINUE
      K = IZERO
      DO 320 I=1,M
        DO 310 J=I,M
          K    = K + IONE
          L    = (I - IONE) * M + J
          R(K) = Q(L)
 310    CONTINUE
 320  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SCALE2
      SUBROUTINE SCALE2 (XMIN,XMAX,N,XMINP,XMAXP,DIST,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCALE2 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ANS FORTRAN.
C
C     GIVEN XMIN, XMAX AND N, SCALE2 FINDS A NEW RANGE XMINP AND
C        XMAXP DIVISIBLE INTO EXACTLY N LINEAR INTERVALS OF SIZE
C        DIST, WHERE N IS GREATER THAN 1.
C
C               WRITTEN BY -
C                      C. R. LEWART,
C                      BELL TELPHONE LABORATORIES, INCORPORATED,
C                      HOLMDEL, NJ 07733
C
C               ALGORITHM 463
C                      ALGORITHMS SCALE1, SCALE2, AND SCALE3
C                      FOR DETERMINATION OF SCALES ON COMPUTER
C                      GENERATED PLOTS.  COMMUNICATIONS OF
C                      THE ACM, 16, NO. 10, 639-640.
C
C     IND = FAULT INDICATOR
C         = 0, IF EVERYTHING IS OK.
C         = 1, IF INPUT VALUES ARE IMPROPER.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN SALLY T. PEAVY,
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             DIST, XMAX, XMAXP, XMIN, XMINP
      REAL             VINT(5)
      REAL             A, AL, B, DEL, FM1, FM2, FN
      REAL             FDIV, FLOG10
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA VINT(1), VINT(2), VINT(3), VINT(4), VINT(5) /
     1         1.0,     2.0,     5.0,    10.0,    20.0 /
C
      DATA DEL / 0.00002 /
C
C     ==================================================================
C
C     CHECK WHETHER PROPER INTPUT VALUES WERE SUPPLIED.
C
      IND = IZERO
      IF (XMIN.LT.XMAX .AND. N.GT.IONE) GO TO 10
      IND = IONE
      RETURN
C
C     ..................................................................
C
  10  FN = N
C
C     FIND APPROXIMATE INTERVAL SIZE A.
C
      A = FDIV (XMAX-XMIN,FN,IDFLT)
      AL = FLOG10 (A)
      NAL = AL
      IF (A.LT.RONE) NAL = NAL - IONE
C
C     A IS SCALED INTO VARIABLE NAMED B BETWEEN 1 AND 10.
C
      B = FDIV (A,RTEN**NAL,IDFLT)
C
C     THE CLOSEST PERMISSIBLE VALUE FOR B IS FOUND.
C
      DO 20 I=1,3
        IF (B.LT.(VINT(I)+DEL)) GO TO 30
  20  CONTINUE
      I = IFOUR
C
C     THE INTERVAL SIZE IS COMPUTED.
C
  30  DIST = VINT(I) * RTEN**NAL
      FM1 = FDIV (XMIN,DIST,IDFLT)
      M1 = FM1
      IF (FM1.LT.RZERO) M1 = M1 - IONE
      IF (ABS(FLOAT(M1)+FDIV(RONE,FM1,IDFLT)).LT.DEL) M1 = M1 + IONE
C
C     THE NEW MINIMUM AND MAXIMUM LIMITS ARE FOUND.
C
      XMINP = DIST * FLOAT (M1)
      FM2 = FDIV (XMAX,DIST,IDFLT)
      M2 = FM2 + RONE
      IF (FM2.LT.(-RONE)) M2 = M2 - IONE
      IF (ABS(FM2+RONE-FLOAT(M2)).LT.DEL) M2 = M2 - IONE
      XMAXP = DIST * FLOAT (M2)
C
C     CHECK WHETHER A SECOND PASS IS REQUIRED.
C
      NP = M2 - M1
      IF (NP.LE.N) GO TO 40
      I = I + IONE
      GO TO 30
C
  40  NX = IDIV (N-NP,ITWO,IDFLT)
      XMINP = XMINP - FLOAT (NX) * DIST
      XMAXP = XMINP + FLOAT (N) * DIST
C
C     ADJUST LIMITS TO ACCOUNT FOR ROUND-OFF, IF NECESSARY.
C
      IF (XMINP.GT.XMIN) XMINP = XMIN
      IF (XMAXP.LT.XMAX) XMAXP = XMAX
      RETURN
C
C     ==================================================================
C
      END
*SCALE3
      SUBROUTINE SCALE3 (XMIN,XMAX,N,XMINP,XMAXP,DIST,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCALE3 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ANS FORTRAN.
C
C     GIVEN XMIN, XMAX AND N, SCALE2 FINDS A NEW RANGE XMINP AND
C        XMAXP DIVISIBLE INTO EXACTLY N LINEAR INTERVALS OF SIZE
C        DIST, WHERE N IS GREATER THAN 1.
C
C               WRITTEN BY -
C                      C. R. LEWART,
C                      BELL TELPHONE LABORATORIES, INCORPORATED,
C                      HOLMDEL, NJ 07733
C
C               ALGORITHM 463
C                      ALGORITHMS SCALE1, SCALE2, AND SCALE3
C                      FOR DETERMINATION OF SCALES ON COMPUTER
C                      GENERATED PLOTS.  COMMUNICATIONS OF
C                      THE ACM, 16, NO. 10, 639-640.
C
C     IND = FAULT INDICATOR
C         = 0, IF EVERYTHING IS OK.
C         = 1, IF INPUT VALUES ARE IMPROPER.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN, SALLY T. PEAVY,
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             DIST, XMAX, XMAXP, XMIN, XMINP
      REAL             VINT(5)
      REAL             A, AL, B, DEL, FM1, FM2, FN
      REAL             FDIV, FLOG10
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA VINT(1), VINT(2), VINT(3), VINT(4), VINT(5) /
     1         1.0,     2.0,     5.0,    10.0,    20.0 /
C
      DATA DEL / 0.00002 /
C
C     ==================================================================
C
C     CHECK WHETHER PROPER INTPUT VALUES WERE SUPPLIED.
C
      IND = IZERO
      IF (XMIN.LT.XMAX .AND. N.GT.IONE) GO TO 10
      IND = IONE
      RETURN
C
C     ..................................................................
C
  10  FN = N
C
C     FIND APPROXIMATE INTERVAL SIZE A.
C
      A = FDIV (XMAX-XMIN,FN,IDFLT)
      AL = FLOG10 (A)
      NAL = AL
      IF (A.LT.RONE) NAL = NAL - IONE
C
C     A IS SCALED INTO VARIABLE NAMED B BETWEEN 1 AND 10.
C
      B = FDIV (A,RTEN**NAL,IDFLT)
C
C     THE CLOSEST PERMISSIBLE VALUE FOR B IS FOUND.
C
      DO 20 I=1,3
        IF (B.LT.(VINT(I)+DEL)) GO TO 30
  20  CONTINUE
      I = IFOUR
C
C     THE INTERVAL SIZE IS COMPUTED.
C
  30  DIST = VINT(I) * RTEN**NAL
      FM1 = FDIV (XMIN,DIST,IDFLT)
      M1 = FM1
      IF (FM1.LT.RZERO) M1 = M1 - IONE
      IF (ABS(FLOAT(M1)+FDIV(RONE,FM1,IDFLT)).LT.DEL) M1 = M1 + IONE
C
C     THE NEW MINIMUM AND MAXIMUM LIMITS ARE FOUND.
C
      XMINP = DIST * FLOAT (M1)
      FM2 = FDIV (XMAX,DIST,IDFLT)
      M2 = FM2 + RONE
      IF (FM2.LT.(-RONE)) M2 = M2 - IONE
      IF (ABS(FM2+RONE-FLOAT(M2)).LT.DEL) M2 = M2 - IONE
      XMAXP = DIST * FLOAT (M2)
C
C     CHECK WHETHER A SECOND PASS IS REQUIRED.
C
      NP = M2 - M1
      IF (NP.LE.N) GO TO 40
      I = I + IONE
      GO TO 30
C
  40  NX = IDIV (N-NP,ITWO,IDFLT)
      XMINP = XMINP - FLOAT (NX) * DIST
      XMAXP = XMINP + FLOAT (N) * DIST
C
C     ADJUST LIMITS TO ACCOUNT FOR ROUND-OFF, IF NECESSARY.
C
      IF (XMINP.GT.XMIN) XMINP = XMIN
      IF (XMAXP.LT.XMAX) XMAXP = XMAX
      RETURN
C
C     ==================================================================
C
      END
*SCODE
      SUBROUTINE SCODE (X,Y,Z,M,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  SCODE V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CODES VALUES IN X OF LENGTH M
C       ACCORDING TO DISTINCT VALUES OF Y OF LENGTH N
C       AND STORES THE RESULT IN Z.
C
C     FOR THE SMALLEST VALUE IN X, THE CORRESPONDING VALUE IN Z IS 1.0
C        FOR THE SECOND SMALLEST IN X, THE VALUE IN Z IS 2.0, ETC.
C
C     INPUT  - X, Y, M, N
C     OUTPUT - Z
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
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             X(*), Y(*), Z(*)
C
C     ==================================================================
C
      DO 30 I=1,M
        DO 20 J=1,N
          IF (X(I)-Y(J)) 20,10,20
  10        Z(I) = FLOAT(J)
            GO TO 30
  20    CONTINUE
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SCRAWL
      SUBROUTINE SCRAWL (X,N,ISYMBL,SVALUE,ROWIND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCRAWL V 7.00  2/19/91. **
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
C        IT STORES N, *, H, M, H AND * IN ISYMBL(I) IN A1 FORMAT.
C        IT STORES NUMBER OF MEASUREMENTS, SMALLEST MEASUREMENT,
C           LOWER HINGE, MEDIAN, UPPER HINGE AND LARGEST MEASUREMENT
C           SVALUE (I).
C        IT STORES ROW NUMBERS OF SVALUE(I) IN ROWIND(I), I NE TO 1.
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
      DIMENSION IC(4), ISYMBL(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             ROWIND(*), SVALUE(*), X(*)
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        ISYMBL*1
      CHARACTER        IC*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IC(1), IC(2), IC(3), IC(4) / 'N', '*', 'H', 'M' /
C
C     ==================================================================
C
      SVALUE(1) = FLOAT (N)
      SVALUE(2) = X(1)
      SVALUE(6) = X(N)
      ROWIND(1) = RONE
      ROWIND(5) = SVALUE(1)
C
      DO 10 I=3,5
        K = I - IONE
        NTOP = N + IONE
        IF (I.NE.IFOUR) NTOP = IDIV (NTOP,ITWO,IND) + IONE
        M = IDIV (NTOP,ITWO,IND)
        IF (I.EQ.IFIVE) M = N + IONE - M
        ROWIND(K) = FLOAT (M)
        SVALUE(I) = X(M)
        IF (MOD(NTOP,ITWO).EQ.IZERO) GO TO 10
          IF (I.EQ.IFIVE) M = M - IONE
          ROWIND(K) = ROWIND(K) + RHALF
          SVALUE(I) = FDIV (X(M)+X(M+1),RTWO,IND)
  10  CONTINUE
C
      DO 20 I=1,4
        ISYMBL(I) = IC(I)
  20  CONTINUE
C
      ISYMBL(5) = IC(3)
      ISYMBL(6) = IC(2)
      RETURN
C
C     ==================================================================
C
      END
*SDPRED
       SUBROUTINE SDPRED (N,M,R,Q,SB,SD,SDYHAT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SDPRED V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SDPRED COMPUTES STANDARD DEVIATIONS OF PREDICTED
C        VALUES.
C
C     INPUT PARAMETERS ...
C
C     N      NUMBER OF OBSERVATIONS.
C     M      NUMBER OF UNKNOWN COEFFICIENTS.
C     R      MATRIX WHICH WAS OBTAINED IN THE QR-DECOMPOSITION FROM
C               SUBROUTINE PDECOM, STORED AS A VECTOR OF LENGTH
C               (M+1)*(M+2)/2.
C     Q      MATRIX OF SIZE (N BY M+1) STORED AS A VECTOR.  OBTAINED IN
C               THE QR-DECOMPOSITION FROM SUBROUTINE PDECOM.
C     SD     RESIDUAL STANDARD DEVIATION OF THE LEAST SQUARES FIT.
C
C     OUTPUT PARAMETER ...
C
C     SDYHAT VECTOR (N BY 1) OF STANDARD DEVIATIONS OF PREDICTED VALUES.
C
C     INTERNAL PARAMETER ...
C
C     SB     VECTOR (M+1 BY 1) USED AS WORK AREA WITHIN THIS SUBROUTINE.
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
      REAL             Q(*), R(*), SB(*), SDYHAT(*)
      REAL             SD
      REAL             FDIV, FDPCON, FSQRT
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
      DO 10 J=1,M
        L = IDIV (ITWO*(J-IONE)*(M+IONE)-J*J+ITHRE*J,ITWO,IND)
        SB(J) = FDIV (RONE,FSQRT (R(L)),IND)
  10  CONTINUE
C
      DO 30 I=1,N
        DSUM = DZERO
        DO 20 J=1,M
          L = (J-IONE) * N + I
          DSUM = DSUM + (DBLE (Q(L)) * DBLE (SB(J))) ** 2
  20    CONTINUE
C
        SDYHAT(I) = FDPCON (DSUM)
        IF (SDYHAT(I).LT.RZERO) SDYHAT(I) = RZERO
        SDYHAT(I) = SD * FSQRT (SDYHAT(I))
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SDRND
      SUBROUTINE SDRND (X,N,ISGD,XT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  4/13/82.  SDRND V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
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
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DPCA, DRNDA, DRNDB, Z
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
*SET
      SUBROUTINE SET
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.    SET V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CHECK ARGUMENTS OF SET INSTRUCTION AND SET MODE TO INPUT VALUE = 2
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /REDSET/ IFLAG, ISRFLG, JY, NDROW, NNARG
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
C
C     ==================================================================
C
      ISRFLG = IONE
      IF (NARGS.EQ.IONE .OR. NARGS.EQ.ITWO) GO TO 10
      CALL ERROR (10)
      GO TO 60
C
C     ..................................................................
C
  10  MODE = ITWO
      CALL ADRESS (NARGS,JY)
      IF (JY.GT.IZERO) GO TO 30
      IF (JY.EQ.IZERO) GO TO 60
  20  CALL ERROR (20)
      GO TO 60
C
C     ..................................................................
C
  30  NDROW = JY + NROW - IONE
      IF (NARGS.EQ.IONE) GO TO 50
      IF (KIND(1).NE.IZERO) GO TO 20
      IF (IARGS(1).LE.NROW .AND. IARGS(1).GT.IZERO) GO TO 40
      CALL ERROR (16)
      GO TO 60
C
C     ..................................................................
C
  40  JY = JY + IARGS(1) - IONE
  50  IFLAG = IZERO
      MODE = ITWO
      NROLD = NRMAX
      RETURN
C
C     ..................................................................
C
  60  IFLAG = IONE
      MODE = IONE
      RETURN
C
C     ==================================================================
C
      END
*SICIEI
      SUBROUTINE SICIEI (IC,X,SI,CI,CII,EI,EXNEI,SHI,CHI,CHII,IERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SICIEI V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                             APPENDIX
C
C                       IMPLEMENTING PROGRAM
C     LANGUAGE. AMERICAN NATIONAL STANDARD FORTRAN
C     DEFINITIONS. X, A REAL VARIABLE
C         SI(X) =INTEGRAL(SIN T/T)DT FROM 0 TO X
C         SI(-X)=-SI(X)
C         CI(X) =GAMMA+LN X+INTEGRAL((COS T-1)/T)DT FROM 0 TO X
C         CI(-X)=CI(X)-I PI
C         EI(X) =-P.V.INTEGRAL(EXP(-T)/T)DT FROM -X TO INFINITY
C         EXNEI(X)=EXP(-X)*EI(X)                      (X .GT. 0)
C             INTEGRAL(EXP(-T)/T) DT FROM X TO INFINITY, OFTEN
C             DENOTED BY -EI(-X)=E1(X). (SEE AUTOMATIC COMPUTING
C             METHODS FOR SPECIAL FUNCTIONS, PART II. THE EXPO-
C             NENTIAL INTEGRAL EN(X), J. OF RESEARCH NBS, 78B,
C             OCTOBER-DECEMBER 1974, PP. 199-216.)
C         SHI(X) =INTEGRAL(SINH T/T)DT FROM 0 TO X
C         SHI(-X)=-SHI(X)
C         CHI(X)=GAMMA+LN X+INTEGRAL((COSH T-1)/T)DT FROM 0 TO X
C         CHI(-X)=CHI(X)-I PI
C                     GAMMA(EULER'S CONSTANT)=.5772156649...
C
C       SPECIAL CASES
C         X=0
C           SI(0)=SHI(0)=0
C           CI(0)=EI(0)=EXNEI(0)=CHI(0)=-INFINITY
C                                      =-MAX. MACH. VALUE (DMAXDP)
C         LIMITING VALUES - X APPROACHES INFINITY
C           SI(X)=PI/2
C           CI(X)=0
C           EI(X)=SHI(X)=CHI(X)=INFINITY (DMAXDP)
C           EXNEI(X)=0
C     USAGE. CALL SICIEI (IC,X,SI,CI,CII,EI,EXNEI,SHI,CHI,CHII,
C                                                          IERR)
C
C         FORMAL PARAMETERS
C             IC      INTEGER TYPE                        INPUT
C                         IC  FUNCTIONS TO BE COMPUTED
C                          1    SI,CI
C                          2    EI,EXNEI
C                          3    EI,EXNEI,SHI,CHI
C                          4    SI,CI,EI,EXNEI,SHI,CHI
C             X       REAL OR DOUBLE PRECISION TYPE       INPUT
C             SI=SI(X)             (SAME TYPE AS X)       OUTPUT
C             CI+I CII=CI(X)              ''              OUTPUT
C             EI=EI(X)                    ''              OUTPUT
C             EXNEI=EXP(-X)*EI(X)         ''              OUTPUT
C             SHI=SHI(X)                  ''              OUTPUT
C             CHI+I CHII=CHI(X)           ''              OUTPUT
C             IERR    INTEGER TYPE                        OUTPUT
C                         IERR=0   X .GE. 0, NORMAL RETURN
C                         IERR=1   X .LT. 0, ERROR RETURN IF
C                                               IC=2
C
C     MODIFICATIONS.
C         THE CODE IS SET UP FOR DOUBLE PRECISION COMPUTATION
C         WITH DOUBLE PRECISION TYPE STATEMENTS
C              DOUBLE PRECISION FUNCTION REFERENCES AND,PARTICU-
C         LARLY,FOR THE UNIVAC 1108 WITH (SEE DEFINITIONS BELOW)
C              DMAXDP APPROX. 2**1023,DSNCOS=2**56,NBM=60 AND OTHER
C         CONSTANTS IN DOUBLE PRECISION FORMAT TO 19 SIGNIFICANT
C         FIGURES. ALL ABOVE ITEMS MUST BE CHANGED FOR SINGLE
C         PRECISION COMPUTATIONS WITH DATA ADJUSTMENTS FOR OTHER
C         COMPUTERS.
C       AUXILIARY FUNCTIONS
C           VARIOUS FUNCTIONS ARE AVAILABLE TO GREATER ACCURACY
C           AT INTERMEDIATE POINTS IN THE SUBROUTINE,NAMELY,
C               SI-(PI/2)=IMAG. PART OF THE CONTINUED FRACTION
C               CI(EI AND CHI)-GAMMA-LN X=SUM OF SERIES
C       CAUTION - THE SUBROUTINE CANNOT READILY BE ADAPTED TO
C                 COMPUTE THE FUNCTIONS FOR COMPLEX ARGUMENTS.
C
C     METHOD.   T=ABS(X)
C         POWER SERIES   T .LE. PSLSC(=2) FOR SI,CI
C                        T .LE. AELL(=-LN(TOLER)) FOR EI,SHI,CHI
C             SI=SUMS(SGN(RK)*TM(RK))  IP=-1  RK=1,3,...,RKO
C             CI=SUMC(SGN(RK)*TM(RK))  IP=+1  RK=2,4,...,RKE
C                    +EULER+XLOG
C             SHI=SUMOT(TM(RK))        IP=-1  RK=1,3,...,RKO
C             CHI=SUMET(TM(RK))        IP=+1  RK=2,4,...,RKE
C                    +EULER+XLOG
C             EI=SUMOT+SUMET+EULER+XLOG               (X .GT. 0)
C                   SGN(1)=1
C                   SGN(RK+1)=-SGN(RK)        RK=1,3,...
C                   SGN(RK+1)=+SGN(RK)        RK=2,4,...
C                   TM(RK)=((T**RK)/(1*2...RK))/RK
C                         =PTM(RK)/RK
C                       PTM(1)=T
C                       PTM(RK+1)=PTM(RK)*(T/(RK+1))   RK .GE. 1
C                   IF TM(RK)/SUM .LT. TOLER
C                     RKE=RK WHERE SUM=ABS(SUMC)       IC=1 OR 4
C                                  SUM=SUMET           IC=2 OR 3
C                                              IC=4,X .GT. PSLSC
C                     RKO=RK WHERE SUM=ABS(SUMS)       IC=1 OR 4
C                                  SUM=SUMOT           IC=2 OR 3
C                                              IC=4,X .GT. PSLSC
C             EXNEI= EI/EXP(T/2)/EXP(T/2)
C                  =(EI/EXPHT)/EXPHT
C
C     CONTINUED FRACTION    T .GT. PSLSC
C             -CI+I(SI-PI/2)=E1(IT)
C                           =EXP(-IT)*(1 I/I (1+IT)-
C                                   1**2 I/I (3+IT)-
C                                   2**2 I/I (5+IT)-...)
C                           =EXP(-IT)*II(AM(RM) I/I BM(RM))
C                                                 RM=1,2,...,RMF
C                                AM(1)=1
C                                AM(RM)=-(RM-1)**2     RM .GT. 1
C                                BM(RM)=2*RM-1+IT=BMR+I BMI
C                           =EXP(-IT)*(FM/GM)
C                           =EXP(-IT)*(FMR+I FMI)/(GMR+I GMI)
C                           =EXP(-IT)*F(RM)
C                           =(COST-I SINT)*(FR+I FI)
C             -CI+I(SI-PI/2)=(FR*COST+FI*SINT)+
C                                             I(FI*COST-FR*SINT)
C                             IF RESQ(RM) .LE. TOLSQ(=TOLER**2)
C                                 OR RESQ(RM) .GE. RESQ(RM-1)
C                                   (RESQ .GE. RESQP)
C                             RMF=RM  WHERE
C                                 RESQ=(MOD(1-F(RM-1)/F(RM)))**2
C
C         ASYMPTOTIC EXPANSION    T .GT. AELL
C             EI=(EXNEI*EXPHT)*EXPHT
C             EXNEI=(1+SUME(TM(RK)))/T            RK=1,2,...,RKF
C             SHI=CHI=EI/2
C                 TM(RK)=(1*2...RK)/(T**RK)
C                 TM(0)=1
C                 TM(RK)=(RK/T)*TM(RK-1)               RK .GE. 1
C                   IF TM(RK) .LT. TOLER (CONVERGENCE) RKF=RK OR
C                      TM(RK) .GE. TM(RK-1)(DIVERGENCE) RKF=RK-1
C     RANGE.
C         FOR SI(X),CI(X), ABS(X) .LT. DSNCOS(UPPER LIMIT FOR
C                                               SIN,COS ROUTINE)
C             X=APPROXIMATELY 2**21, NBM=27
C                             2**56, NBM=60
C         FOR EXP(-X)*EI(X), X .LE. DMAXDP
C         FOR EI(X), X .LT. XMAXEI (APPROXIMATELY 92.5,  NBC=8,
C                                                715.6,  NBC=11)
C                     NBC=NUMBER OF BINARY DIGITS IN THE BIASED
C                     CHARACTERISTIC OF A FLOATING POINT NUMBER
C         FOR SHI(X),CHI(X), ABS(X) .LT. XMAXHF
C             X=APPROXIMATELY 93.2,   NBC=8
C                            716.3,   NBC=11
C     ACCURACY. THE MAXIMUM RELATIVE ERROR, EXCEPT FOR REGIONS
C               IN THE IMMEDIATE NEIGHBORHOOD OF ZEROS,ON THE
C               UNIVAC 1108 IS 4.5(-7) FOR SINGLE PRECISION COM-
C               PUTATION AND 7.5(-17) FOR DOUBLE PRECISION COM-
C               PUTATION.
C
C         PRECISION. VARIABLE - BY SETTING THE DESIRED VALUE OF NBM
C                               OR A PREDETERMINED VALUE OF TOLER
C         MAXIMUM     UNIVAC 1108 TIME/SHARING EXECUTIVE SYSTEM
C         TIMING.      NBM=27   NBM=60
C         (SECONDS)     .0093    .070
C         STORAGE. 954 WORDS REQUIRED BY THE UNIVAC 1108 COMPILER
C                            APPROX. 2**(NBM-6) OR 10**(S-2)
C                                       (S=SIGNIFICANT FIGURES)
C                     NBM=ACCURACY DESIRED OR THE
C                         MAXIMUM NUMBER OF BINARY DIGITS IN THE
C                           MANTISSA OF A FLOATING POINT NUMBER
C                     TOLER=UPPER LIMIT FOR RELATIVE ERRORS
C                          =2**(-NBM)=APPROX. 10**(-S)
C     NOTE - ARGUMENT CHECKS PRECEDING FUNCTION REFERENCES
C            NECESSITATE ADDITIONAL MACHINE DEPENDENT STATEMENTS
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1976.
C                   CURRENT VERSION - FEBRUARY, 1990.
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
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DALOG2, DEULER
      DOUBLE PRECISION FDCOS, FDDIV, FDEXP, FDLOG, FDSIN, FDSQRT
      DOUBLE PRECISION X, SI, CI, CII, EI, EXNEI, SHI, CHI, CHII
      DOUBLE PRECISION A(4)
      DOUBLE PRECISION AELL, AM, AMIN, ASUMSC, BMI, BMR, COST
      DOUBLE PRECISION EXPHT, EXPL, FI, FIP, FMI, FMM1I, FMM1R
      DOUBLE PRECISION FMM2I, FMM2R, FMR, FR, FRP, GMI, GMM1I, GMM1R
      DOUBLE PRECISION GMM2I, GMM2R, GMR, PSLL, PSLSC, PTM, RE, RESQ
      DOUBLE PRECISION RESQP, RK, RM, SCC, SFMI, SFMR, SGMI, SGMR, SGN
      DOUBLE PRECISION SINT, SUMC, SUME, SUMEO, SUMET, SUMOT, SUMS
      DOUBLE PRECISION SUMSC, T, TEMP, TEMPA, TEMPB, TM, TMAX, TMM1
      DOUBLE PRECISION TOLER, TOLSQ, TT, TTT, XLOG, XMAXEI, XMAXHF
C
      EQUIVALENCE (FMR,A(1)), (FMI,A(2)), (GMR,A(3)), (GMI,A(4))
C
C     ==================================================================
C
      TOLER = DTWO**(-NBM)
      SI    = DMAXDP
      CI    = DMAXDP
      CII   = DMAXDP
      EI    = DZERO
      EXNEI = DMAXDP
      SHI   = DZERO
      CHI   = DZERO
      CHII  = DMAXDP
      SGN   = DONE
      SUMC  = DZERO
      SUMSC = DZERO
      SUMS  = DZERO
C
C          VALIDITY CHECK ON INPUT PARAMETERS
C               INDICATOR CHECK
C                 SET IND=IC
C                   CHANGE IND=4 IF IC .LT. 1 OR .GT. 4
C
      IND = IC
      IF (IND.LT.IONE .OR. IND.GT.IFOUR) IND = IFOUR
C
C               ARGUMENT CHECK
C                 X .GE. 0    IERR=0
C                 X .LT. 0    IERR=1
C                             (ERROR RETURN IF IC=2)
C
      IERR = IZERO
      T = DABS (X)
      IF (X.EQ.DZERO) GO TO 10
      IF (X.GT.DZERO) GO TO 40
      IF (IND.NE.IONE) IERR = IONE
      IF (IND.NE.ITWO) GO TO 40
      RETURN
C
C          SPECIAL CASES
C               X=0
C
  10  IF (IND.EQ.ITWO) GO TO 20
      IF (IND.LT.ITWO) GO TO 30
      SHI   =  DZERO
      CHI   = -DMAXDP
      CHII  =  DZERO
  20  EI    = -DMAXDP
      EXNEI = -DMAXDP
      IF (IND.NE.IFOUR) RETURN
  30  SI    = DZERO
      CI    = -DMAXDP
      CII   = DZERO
      RETURN
C
C     ..................................................................
C
  40  IF (T.LT.DSNCOS) GO TO 80
C
C               ABS(X) .GE. DSNCOS
C
      IF (IND.EQ.ITWO) GO TO 50
      IF (IND.LT.ITWO) GO TO 70
      SHI   = DMAXDP
      CHI   = DMAXDP
      CHII  = DZERO
      IF (IERR.EQ.IONE) GO TO 60
  50  EI    = DMAXDP
      TT    = FDDIV (DONE,T,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TTT   = FDDIV (DONE+TT,T,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      EXNEI = TTT
  60  IF (IND.NE.IFOUR) GO TO 490
  70  SI    = DHLFPI
      CI    = DZERO
      CII   = DZERO
      GO TO 490
C
C          EVALUATIONS FOR ABS(X)(=T) .GT. 0 AND .LT. DSNCOS
C               ADDITIONAL MACHINE DEPENDENT STATEMENTS
C                    FUNCTION REFERENCES
C                    CONTROL VARIABLES
C
  80  XLOG   = FDLOG (T)
      SINT   = FDSIN (T)
      COST   = FDCOS (T)
      EXPL   = FDLOG (DMAXDP)
      TT     = FDDIV (DONE,EXPL,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      XMAXEI = EXPL + FDLOG (EXPL+FDLOG(EXPL)) - TT
      XMAXHF = XMAXEI + DALOG2
      AELL   = -FDLOG (TOLER)
      AMIN   = FDDIV (DONE,DMAXDP,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      PSLL   = DTWO * FDSQRT (AMIN)
      PSLSC  = DTWO
C
C               EXPONENTIAL FUNCTION DETERMINATION
C
      IF (T.LE.TOLER) GO TO 90
      IF (T.GE.XMAXHF) GO TO 130
      EXPHT = FDEXP (FDDIV (T,DTWO,INND))
      IF (INND.NE.IZERO) CALL ERROR (106)
      GO TO 140
C
  90  EXPHT = DONE
      GO TO 140
C
 130  EXPHT = DMAXDP
C
C               METHOD SELECTION
C
 140  IF (T.LE.PSLSC) GO TO 150
      IF (IND.EQ.IONE) GO TO 310
      IF (IND.EQ.IFOUR) GO TO 310
      IF (T.GT.AELL) GO TO 400
      SUMOT = DZERO
      SUMET = DZERO
      SUMEO = DZERO
      GO TO 160
C
C                    METHOD --- POWER SERIES
C                      SI(X),CI(X),           T .LE. PSLSC
C                      EI(X),SHI(X),CHI(X),   T .LE. AELL
C                         LIMITING VALUES, T NEAR DZERO
C
 150  SUMC  = DZERO
      SUMET = DZERO
      SUMS  = DZERO
      SUMOT = DZERO
      IF (T.LE.PSLL) SUMS  = T
      IF (T.LE.PSLL) SUMOT = T
C
C                         INITIALIZATION FOR SI,CI
C
      SUMSC = DZERO
      SGN   = DONE
C
C                         INITIALIZATION FOR SHI,CHI(AND EI)
C
      SUMEO = DZERO
      IF (T.LE.PSLL) GO TO 270
C
C                              IP -  INDICATOR FOR ODD OR
C                                      EVEN TERMS
C
 160  IP  = -IONE
      RK  = DONE
      PTM = T
C
C                         COMPUTATION OF (T**K)/(1*2...K)/K
C
 170  TM = FDDIV (PTM,RK,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
C
C                         SUMMATION FOR SI(CI)
C
      IF (IND.NE.IONE) GO TO 220
 180  SUMSC = SGN * TM + SUMSC
C
C                         RELATIVE ERROR FOR SI(CI)
C     PARTIAL SUM OF ALTERNATING ODD(EVEN) TERMS MAY EQUAL DZERO
C
      ASUMSC = SUMSC
 190  IF (ASUMSC.GT.DZERO) GO TO 200
      IF (ASUMSC.EQ.DZERO) GO TO 210
      ASUMSC = -ASUMSC
      GO TO 190
C
 200  RE = FDDIV (TM,ASUMSC,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GO TO 230
C
 210  RE = DMAXDP
      GO TO 230
C
C                         SUMMATION FOR SHI(CHI)(AND EI)
C
 220  SUMEO = TM + SUMEO
      IF (IND.EQ.IFOUR) GO TO 180
C
C                         RELATIVE ERROR FOR SHI(CHI)
C
      RE = FDDIV (TM,SUMEO,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
C
C                         SIGN CHANGE AND SELECTION
C                         OF SUMS OF ODD(EVEN) TERMS
C
 230  IF (IP.EQ.IONE) GO TO 240
      SGN   = -SGN
      SUMS  = SUMSC
      SUMSC = SUMC
      SUMOT = SUMEO
      SUMEO = SUMET
      GO TO 250
C
 240  SUMC  = SUMSC
      SUMSC = SUMS
      SUMET = SUMEO
      SUMEO = SUMOT
C
C                         RELATIVE ERROR CHECK
C
 250  IF (RE.LT.TOLER) GO TO 270
C
C                         ADDITIONAL TERMS
C
      RK = RK + DONE
C
C                              UNDERFLOW TEST
C     UNDERFLOWS AFFECTING ACCURACY ARE AVOIDED. ALL OTHER
C     UNDERFLOWS ARE ASSUMED TO BE SET EQUAL TO DZERO
C
      IF (T.GT.PSLSC) GO TO 260
      TT = FDDIV (AMIN*RK*RK,T,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      IF (PTM.LE.TT) GO TO 270
 260  PTM = FDDIV (T,RK,INND) * PTM
      IF (INND.NE.IZERO) CALL ERROR (106)
      IP = -IP
      GO TO 170
C
C                         SI,CI EVALUATION
C
 270  IF (IND.NE.IONE) GO TO 290
 280  SI  = SUMS
      CI  = (SUMC+XLOG) + DEULER
      CII = DZERO
      GO TO 490
C
C                         EI EVALUATION
C
 290  IF (X.LE.DZERO) GO TO 300
      EI = (SUMET+SUMOT+XLOG) + DEULER
      TT = FDDIV (EI,EXPHT,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TTT = FDDIV (TT,EXPHT,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      EXNEI = TTT
      IF (IND.EQ.ITWO) RETURN
C
C                         SHI,CHI EVALUATION
C
 300  SHI  = SUMOT
      CHI  = (DEULER+SUMET) + XLOG
      CHII = DZERO
      IF (IND.NE.IFOUR) GO TO 490
      GO TO 280
C
C                    METHOD --- CONTINUED FRACTION
C                      SI(X),CI(X),       T .GT. PSLSC
C                      -CI(T) + I (SI(T)-HALFPI)=E1(IT)
C                         INITIALIZATION
C
 310  SCC   = FDDIV (DMAXDP,DFOR,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TOLSQ = TOLER * TOLER
      RM    = DONE
      AM    = DONE
      BMR   = DONE
      BMI   = T
      FMM2R = DONE
      FMM2I = DZERO
      GMM2R = DZERO
      GMM2I = DZERO
      FMM1R = DZERO
      FMM1I = DZERO
      GMM1R = DONE
      GMM1I = DZERO
      RESQP = DMAXDP
      FRP   = DZERO
      FIP   = DZERO
C
C                         RECURRENCE RELATION
C                           FM=BM*FMM1 + AM*FMM2
C                           GM=BM*GMM1 + AM*GMM2
C
 320  FMR = BMR * FMM1R - BMI * FMM1I + AM * FMM2R
      FMI = BMI * FMM1R + BMR * FMM1I + AM * FMM2I
      GMR = BMR * GMM1R - BMI * GMM1I + AM * GMM2R
      GMI = BMI * GMM1R + BMR * GMM1I + AM * GMM2I
C
C                         CONVERGENT F=FM/GM
C                           TESTS TO AVOID INCORRECT RESULTS
C                               DUE TO OVERFLOWS(UNDERFLOWS)
C                             FINDING MAXIMUM(=TMAX) OF
C                               ABSOLUTE OF FMR,GMR,FMI,GMI
C                               FOR SCALING PURPOSES
C
      TMAX = DZERO
      I    = IONE
 330  TEMP = A(I)
 340  IF (TEMP.GT.DZERO) GO TO 350
      IF (TEMP.EQ.DZERO) GO TO 360
      TEMP = -TEMP
      GO TO 340
C
 350  IF (TEMP.LE.TMAX) GO TO 360
      TMAX  = TEMP
 360  IF (I.GE.IFOUR) GO TO 370
      I     = I + IONE
      GO TO 330
C
 370  SFMR = FDDIV (FMR,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      SFMI = FDDIV (FMI,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      SGMR = FDDIV (GMR,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      SGMI = FDDIV (GMI,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TEMP = SGMR * SGMR + SGMI * SGMI
      FR   = FDDIV ((SFMR*SGMR+SFMI*SGMI),TEMP,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      FI   = FDDIV ((SFMI*SGMR-SFMR*SGMI),TEMP,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
C
C                         RELATIVE ERROR CHECK
C
      TEMP  = FR * FR + FI * FI
      TEMPA = FDDIV ((FRP*FR+FIP*FI),TEMP,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TEMPB = FDDIV ((FIP*FR-FRP*FI),TEMP,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TEMP  = DONE - TEMPA
      RESQ  = TEMP * TEMP + TEMPB * TEMPB
      IF (RESQ.LE.TOLSQ) GO TO 390
      IF (RESQ.GE.RESQP) GO TO 380
C
C                         ADDITIONAL CONVERGENTS
C
      AM    = - RM * RM
      RM    = RM + DONE
      BMR   = BMR + DTWO
      FMM2R = FMM1R
      FMM2I = FMM1I
      GMM2R = GMM1R
      GMM2I = GMM1I
      FMM1R = FMR
      FMM1I = FMI
      GMM1R = GMR
      GMM1I = GMI
      FRP   = FR
      FIP   = FI
      RESQP = RESQ
C
C                         SCALING
C     SCALING SHOULD NOT BE DELETED AS THE VALUES OF FMR,FMI AND
C     GMR,GMI MAY OVERFLOW FOR SMALL VALUES OF T
C
      TT    = FDDIV (SCC,BMR-AM,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      IF (TMAX.LT.TT) GO TO 320
      FMM2R = FDDIV (FMM2R,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      FMM2I = FDDIV (FMM2I,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GMM2R = FDDIV (GMM2R,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GMM2I = FDDIV (GMM2I,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      FMM1R = FDDIV (FMM1R,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      FMM1I = FDDIV (FMM1I,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GMM1R = FDDIV (GMM1R,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GMM1I = FDDIV (GMM1I,TMAX,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      GO TO 320
C
C                         DIVERGENCE OF RELATIVE ERROR
C                           ACCEPT PRIOR CONVERGENT
C
 380  FR = FRP
      FI = FIP
C
C                         SI,CI EVALUATION
C
 390  SI  = FI * COST - FR * SINT + DHLFPI
      CI  = - (FR*COST + FI*SINT)
      CII = DZERO
C
C                    INDICATOR TO COMPUTE EI,SHI,CHI
C
      IF (IND.EQ.IONE) GO TO 490
      IND = ITHRE
      IF (T.GT.AELL) GO TO 400
      SUMOT = DZERO
      SUMET = DZERO
      SUMEO = DZERO
      GO TO 160
C
C                    METHOD --- ASYMPTOTIC EXPANSION
C                      EI(X),EXNEI(X)         X .GT. AELL
C                      SHI(T)=CHI(T)=EI(T)/2  T .GT. AELL
C                         INITIALIZATION
C
 400  IF (IND.NE.ITWO) GO TO 480
 410  SUME = DZERO
      RK   = DZERO
      TM   = DONE
C
C                         ADDITIONAL TERMS
C
 420  TMM1 = TM
      RK = RK + DONE
      TM = FDDIV (RK,T,INND) * TM
      IF (INND.NE.IZERO) CALL ERROR (106)
C
C                         TOLERANCE CHECK
C
      IF (TM.LT.TOLER) GO TO 440
      IF (TM.GE.TMM1) GO TO 430
      SUME = SUME + TM
      GO TO 420
C
C                         DIVERGENT PATH
C
 430  SUME = SUME - TMM1
C
C                         EXNEI EVALUATION
C
 440  IF (X.LT.DZERO) GO TO 470
      EXNEI = FDDIV (DONE+SUME,T,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
C
C                         EI EVALUATION - X .LT. XMAXEI
C
      IF (T.GE.XMAXEI) GO TO 450
      EI = (EXNEI*EXPHT) * EXPHT
      GO TO 460
C
C                         EI - LIMITING VALUE, X .GE. XMAXEI
C
 450  EI = DMAXDP
C
C                         SHI,CHI EVALUATION - T .LT. XMAXHF
C
 460  IF (IND.EQ.ITWO) RETURN
 470  IF (T.GE.XMAXHF) GO TO 490
      TT   = FDDIV (DONE+SUME,T,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      TTT  = FDDIV(TT,DTWO,INND)
      IF (INND.NE.IZERO) CALL ERROR (106)
      SHI  = (TTT*EXPHT) * EXPHT
      CHI  = SHI
      CHII = DZERO
      GO TO 490
C
C                         SHI,CHI - LIMITING VALUE
C                                              T .GE. XMAXHF
C
 480  IF (T.LT.XMAXHF) GO TO 410
      SHI  = DMAXDP
      CHI  = DMAXDP
      CHII = DZERO
      IF (X.GT.DZERO) GO TO 410
      GO TO 500
C
C          ADJUSTMENTS FOR X .LT. 0
C
 490  IF (X.GT.DZERO) RETURN
 500  IF (IC.EQ.ITHRE) GO TO 510
      SI   = -SI
      CII  = -DPI
      IF (IC.EQ.IONE) RETURN
 510  SHI  = -SHI
      CHII = -DPI
      RETURN
C
C     ==================================================================
C
      END
*SKSYMV
      SUBROUTINE SKSYMV (A,NROW,N,K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SKSYMV V 7.00  4/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     MATRIX A IS TESTED FOR SKEW SYMMETRY.
C
C     INPUT ...
C        A           MATIRX TO BE TESTED.
C        NROW        DIMENSION OF A
C        N           PRESENT SIZE OF MATRIX
C
C     OUTPUT ...
C        K           STATUS
C                       K = 2, NO SYMMETRY
C                       K = 3, EXACT SKEW SYMMETRY
C                       K = 4, RELATIVE (1.E-7) SKEW SYMMETRY
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
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*)
      REAL             T, TOLNCE
      REAL             FDIV
C
C     ==================================================================
C
      TOLNCE = RTEN * RER
      K      = ITHRE
      JJ = IONE
      DO 50 J=1,N
        IF (A(JJ).EQ.RZERO) GO TO 10
        K = ITWO
        RETURN
C
C     ..................................................................
C
  10    I = J + IONE
        IF ( I .GT. N ) GO TO 50
        LJ = JJ + IONE
        JL = JJ + NROW
        DO 40 L=I,N
          IF (A(LJ).NE.RZERO) GO TO 20
          T = ABS(A(JL))
          GO TO 30
  20      T = ABS (RONE+FDIV(A(LJ),A(JL),IND))
  30      IF (T.EQ.RZERO) GO TO 35
          K = IFOUR
          IF (T.LE.TOLNCE) GO TO 35
          K = ITWO
          RETURN
C
C     ..................................................................
C
  35      LJ = LJ + IONE
          JL = JL + NROW
  40    CONTINUE
        JJ = JJ + NROW + IONE
  50  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*SLFPRT
      SUBROUTINE SLFPRT (ICS,IDATA,ISPVX,ISTORE,LN,LLINE,SPDPT,XS,INND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. SLFPRT V 7.00  1/14/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PERFORM PRINTING OF STEM LEAF DISPLAYS.
C
C     ALGORITHM DEVELOPED BY WESLEY NICHOLSON.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - OCTOBER, 1973.
C                   CURRENT VERSION - JANUARY, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IDATA(65,*), IPERNT(2), ISIGN(2), ISPVX(*), ISTORE(*)
      DIMENSION LLINE(*), LSTEM(100)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     NOTE. 40 = 2*(MXWDTH+1) IN IB(40).  CHANGE, IF MXWDTH IS CHANGED.
C
      COMMON /SLCONS/ MXLIN, MXWDTH
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ
      COMMON /SLEAFC/ IJ, IL, INN, INNI, IPERJ, IPRT, IQN, IR, ISWT, IW
      COMMON /SLEAFD/ IXN, JPERST, JSPA, KC, KDPTH, LMAXA, LRND, MSAL
      COMMON /SLEAFE/ NDIV, NSAL, NSALST, NSALTP, NSP, NSPP, NSTOP
      COMMON /SLICHR/ IB(40), IC(6)
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SLRVAR/ RI(5), SV(6)
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     IDTHST = 0  DO NOT STORE DEPTH
C     IDTHST .GT. 0  DIMENSION SIZE OF VECTOR SPDPT
C     VECTOR SPDPT AND DEPTH STORED
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             SPDPT(*), XS(*)
      REAL             X(1)
      REAL             ROUND, SIGN
C
C     .................................................................
C
      CHARACTER        LA*1
      CHARACTER*1      IB, IC
      CHARACTER*1      IDATA, ISTORE
      CHARACTER*1      INTA, IPERNT, ISIGN, LCHAR, LSTEM
C 
C     =================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     LEFT AND RIGHT PARENTHESES ARE USED TO SEPARATE THE STEM FROM THE
C        LEAF.
C
      DATA IPERNT(1), IPERNT(2) / '(', ')' /
C
C     ==================================================================
C
      IY       = IZERO
      IYA      = IZERO
      INTV     = IZERO
      JCOMMA   = IZERO
      MTOM     = IZERO
      MCON     = IZERO
      IAI      = IONE
      JWV      = IONE
      LINE     = LN
      ICASE    = ICS
      INND     = IZERO
      ISPV     = ISPVX(1)
      ISPVA    = ISPVX(2)
      MW       = NWSL
      MND      = NDSL
      JPRNTH   = ITWO
      LMAX     = IZERO
      LMAXA    = IZERO
      LSTRT    = NSTOP + 6
      KDPTH    = IZERO
      IF (IDTHST.GT.IZERO) KDPTH = IONE
      ISIGN(1) = LA(40)
      ISIGN(2) = LA(39)
      JW       = IONE
      ISTPRD   = IZERO
      IF (JZ-ILEAF.GT.IPER .OR. IPER.LE.JZ) ISTPRD = IONE
      ISTORE(NSTOP+3) = IPERNT(1)
      IDPTH  = IZERO
      LDEPTH = IZERO
      LSWT   = IONE
      ISCRL  = ITWO
      NTWO   = IDIV (NZ,ITWO,IND)
      IQP    = IZERO
      LINE   = IONE
      JV     = ITWO - IW
      NSRL   = NSAL + ITHRE
      LEAF   = LZ
      MASTRK = IZERO
      LSTL   = LSTRT
      MCOMMA = IZERO
      IF (LZ.GT.IONE) MCOMMA = IONE
      IF (KZ.EQ.IONE .AND. MCOMMA.EQ.IZERO) MASTRK = IONE
      JLEAF  = JSTRT + JZ - IONE
      MLEAF  = LEAF + MCOMMA + MASTRK
      LASTRK = IZERO
      LINT   = IZERO
      IF (ISWT.GT.ITWO .AND. IW.EQ.IONE) MSAL = MSAL - IONE
      ROUND  = RHALF * RTEN**(-ISIGD-1+IPER)
      ISPVB  = IZERO
C
C     SET UP NUMBER OF LINES TO BE PRINTED PER PAGE.
C
      LCNT = 7
C
C     ISWT IS EITHER 1, 2, 3 OR 4
C        AND CONTROLS THE TYPE OF DISPLAY.
C
C           ISWT = 1, IF I.GT.0 AND K=1
C           ISWT = 2, IF I.GT.0 AND K=2,4 OR 10
C           ISWT = 3, IF I=0    AND K=1
C           ISWT = 4, IF I=0    AND K=3
C
      DO 770 LX=1,NZ
C
C       THE FOLLOWING STATEMENTS DETERMINE THE MAXIMUM NUMBER
C          SIGNIFICANT DIGITS IN DATA.
C
        IF (JLSWT.GT.IZERO) GO TO 70
        CALL RFORMT (1,ISIGD,X,XS(LX)+ROUND,0,0,MW,MND,IB(1),IRF)
        LJSIG = IZERO
        KSTRT = JSTRT
  10    DO 40 LSTP=KSTRT,KC
          IF (IB(LSTP).EQ.LA(45) .AND. LJSIG.LE.IZERO) GO TO 40
          IF (IB(LSTP).EQ.LA(45) .AND. LJSIG.GT.IZERO) GO TO 60
          IF (IB(LSTP).NE.LA(1)) GO TO 30
          IBAP = LSTP
          LJPV = IZERO
          DO 20 LBAP=IBAP,KC
            IF (IB(LBAP).NE.LA(45) .AND. IB(LBAP).NE.LA(1)) GO TO 50
            LJPV = LJPV + IONE
  20      CONTINUE
          GO TO 60
  30      LJSIG = LJSIG + IONE
  40    CONTINUE
C
        GO TO 60
  50    LJSIG = LJSIG + LJPV
        KSTRT = LBAP
        GO TO 10
  60    ISIGNF = MAX0 (ISIGNF,LJSIG)
  70    CALL RFORMT (1,ISIGD,X,XS(LX),0,0,MW,MND,IB(1),IRF)
        CALL RNDATM (IB(JSTRT),KC,LRND,IND)
        IF (IND.NE.IZERO) GO TO 790
        CALL CINDEX (IB(JSTRT),ILEAF,JZ,KZ,INDEX,IND)
        IF (IND.NE.IZERO) GO TO 800
        IQ = IL + IONE - INDEX
        IF (LX.LE.IJ) GO TO 80
        IQ = INDEX - IXN
        IF (INN.NE.IZERO) IQ = INDEX + IXN
  80    IQJ = IQ
        IF (IQ.EQ.LINE .AND. IQP.LE.IZERO) GO TO 90
        IF (IQ.EQ.LINE .AND. IQP.GT.IZERO) GO TO 460
        IF (IQP.NE.IZERO) GO TO 730
        IQJ = LINE
C
C       NEW LINE. SETUP STEM FOR THAT LINE TO PRINT.
C
  90    CALL IXLINE (IQJ,IQN,IL,IXN,IX)
        GO TO (100,100,630,630), ISWT
 100    ISTEM = IDIV (ITWO*(IX-IONE),KZ,IND)
        IY = MOD (ISTEM,ITEN)
        IF (ISWT.EQ.ITWO .AND. IQJ.GT.IQN) GO TO 110
        IF (ISWT.EQ.ITWO .AND. IQJ.LE.IQN) GO TO 120
        IYA = IY + IONE
        IF (IQJ.GT.IQN) GO TO 110
        IT  = IY
        IY  = IYA
        IYA = IT
        GO TO 120
 110    JW = ITWO
 120    LSP = NSP + IW
        IF (ILEAF.EQ.IONE .AND. INNI.NE.IONE) GO TO 160
        IAI = ILEAF
        IF (INNI.EQ.IONE) IAI = ILEAF + IONE
        JDIV = ITEN**(IAI-IONE-ISTPRD)
        ISBN = IZERO
        IF (JPERST.EQ.IONE) JDIV = JDIV * ITEN
        DO 150 IS=2,IAI
          IF (ISTPRD.EQ.IZERO) GO TO 130
          IF (JPERST.EQ.IONE) GO TO 130
          IF (IPER.NE.IS-IPERJ) GO TO 130
          ISTORE(LSP) = LA(38)
          ISBN = IONE
          GO TO 140
 130      INT   = IDIV (ISTEM,JDIV,IND)
          ISTEM = ISTEM - INT * JDIV
          JDIV  = IDIV (JDIV,ITEN,IND)
          INTA  = LA(INT+1)
          IF (INTA.NE.LA(IONE)) ISBN = IONE
          IF (ISTORE(LSP-1).EQ.LA(38)) ISBN = IONE
          IF (ISBN.EQ.IZERO .AND. ISWT.EQ.IZERO) INTA = LA(45)
          ISTORE(LSP) = INTA
 140      LSP = LSP + IONE
 150    CONTINUE
C
 160    ISTORE(LSP) = LA(IY+1)
        IF (IW.EQ.IZERO) GO TO 180
        LSP = NSP
 170    LSP = LSP + IONE
        IF (ISTORE(LSP).EQ.LA(45)) GO TO 170
        JWV = ITHRE - JW
        ISTORE(LSP-1) = ISIGN (JWV)
 180    IF (ISWT.EQ.ITWO) GO TO 210
        JSP = JSPA
        LSP = NSP
        IF (ILEAF.EQ.IONE .AND. INNI.EQ.IZERO) GO TO 200
        IF (INNI.EQ.IONE) JSP = JSP + IONE
        DO 190 IS=JV,IAI
          ISTORE(JSP) = ISTORE(LSP)
          LSP = LSP + IONE
          JSP = JSP + IONE
 190    CONTINUE
C
 200    ISTORE(JSP) = LA(IYA+1)
 210    IQP = IONE
        DO 220 IS=1,LSTRT
          LSTEM(IS) = ISTORE(IS)
 220    CONTINUE
        IF (IQ.EQ.LINE) GO TO 460
C
C       SET UP DEPTH FOR PRINTING.
C
 230    NDPT  = IDPTH
        NNDPT = NDPT
        IF (LINT.NE.IZERO) GO TO 310
        GO TO (240,250), LSWT
 240    IF (IDPTH.LE.NTWO) GO TO 260
        LSWT = ITWO
        IF (LDEPTH.GT.(NZ-NTWO)) GO TO 250
        NNDPT = IZERO
        IF (ISTORE(NSRL-1).NE.LA(45) .OR. ISCRL.NE.IFOUR) GO TO 300
        ISTORE(NSRL) = IC(ISCRL)
        NSRL = NSRL + IONE
        LLINE(ISCRL) = IQJ
        ISCRL = ISCRL + IONE
        GO TO 300
 250    NDPT   = NZ - LDEPTH
 260    JSTORE = IZERO
        NNDPT  = NDPT
        JDIV   = NDIV
        DO 290 IS=NSALST,NSALTP
          INT  = IDIV (NDPT,JDIV,IND)
          NDPT = NDPT - JDIV * INT
          ISTORE(IS) = LA(INT+1)
          IF (JSTORE.NE.IZERO) GO TO 280
          IF (INT.NE.IZERO) GO TO 270
          ISTORE(IS) = LA(45)
          GO TO 280
 270      JSTORE = IONE
 280      JDIV = IDIV (JDIV,ITEN,IND)
 290    CONTINUE
C
 300    IF (LINT.NE.IZERO) GO TO 310
        LDEPTH = IDPTH
        IF (IDTHST.EQ.IZERO) GO TO 310
        KDPTH = KDPTH + IONE
        IF (KDPTH.GT.IDTHST) GO TO 310
        SPDPT(KDPTH) = NNDPT
 310    IF (LINE.GT.IPRT .AND. ICASE.EQ.IZERO) IPRT = IZERO
        IF (IPRT.EQ.IZERO .AND. IPET.NE.IZERO) GO TO 320
        IF (IPRT.EQ.IZERO .AND. IPET.EQ.IZERO) GO TO 410
        WRITE (IOUT,810) (ISTORE(IS),IS=1,LUPPER)
C
C       CHECK TO SEE IF NEW PAGE IS TO BE PRINTED.
C
        LCNT = LCNT + IONE
        IF ( LCNT.GT.LENGTH) THEN
          CALL PAGE (0)
          LCNT = IZERO
        ENDIF
 320    IF (LINE.NE.IONE .OR. ISPVB.NE.IZERO) GO TO 360
        ISPVB = IONE
        ISPVC = ITWO
        ISPV  = IZERO
        DO 330 IS=NSPP,LSTRT
          ISPV = ISPV + IONE
          IDATA(ISPV,1) = LSTEM(IS)
 330    CONTINUE
C
        IDATA(ISPV,1) = ISTORE(LSTRT)
        IJK = LSTRT + IONE
        IF (ISTORE(LSTRT).NE.LA(37) .AND. LEAF.EQ.IONE) GO TO 410
        IF (ISTORE(LSTRT).NE.LA(37)) GO TO 340
        ISPV  = ISPV - IONE
        ISPVC = IONE
        IF (ISTORE(IJK).EQ.LA(37)) IJK = IJK + IONE
 340    DO 350 IS=ISPVC,LEAF
          ISPV = ISPV + IONE
          IDATA(ISPV,1) = ISTORE(IJK)
          IJK = IJK + IONE
 350    CONTINUE
C
        GO TO 410
 360    IF (LX.NE.NZ) GO TO 410
        IJK = LSTRT - IONE
        ISPVA = IZERO
        DO 370 IS=NSPP,IJK
          ISPVA = ISPVA + IONE
          IDATA(ISPVA,2) = LSTEM(IS)
 370    CONTINUE
C
        IJK = LSTL
 380    IF (ISTORE(IJK).NE.LA(45)) GO TO 390
        IJK = IJK - IONE
        GO TO 380
 390    IF (ISTORE(IJK).EQ.LA(44)) IJK = IJK - IONE
        IF (ISTORE(IJK).EQ.LA(37)) IJK = IJK - IONE
        IF (ISTORE(IJK).EQ.LA(37)) IJK = IJK - IONE
        IJK = IJK - LEAF + IONE
        DO 400 IS=1,LEAF
          ISPVA = ISPVA + IONE
          IDATA(ISPVA,2) = ISTORE(IJK)
          IJK = IJK + IONE
 400    CONTINUE
 410    DO 420 IS=LSTRT,LUPPER
          ISTORE(IS) = LA(45)
 420    CONTINUE
C
        NTOP = NSTOP
        IF (LINT.EQ.IONE) GO TO 440
        DO 430 IS=1,NSTOP
          ISTORE(IS) = LSTEM(IS)
 430    CONTINUE
        NTOP = MSAL
 440    DO 450 IS=1,NTOP
          ISTORE(IS) = LA(45)
 450    CONTINUE
C
        NSRL = NSAL + ITHRE
        LSTL = LSTRT
        IF (LINT.EQ.IONE) GO TO 590
        ISTORE(NSTOP+3) = IPERNT(JPRNTH)
        JPRNTH = JPRNTH + IONE
        IF (JPRNTH.GT.ITWO) JPRNTH = IONE
        LASTRK = IZERO
        LMAXA  = MAX0(LMAX,LMAXA)
        LMAX   = IZERO
        IF (IR.EQ.LINE) GO TO 780
        IQP    = IZERO
        LINE   = LINE + IONE
        GO TO 80
C
C       STEM IS SET UP. SET UP SCRAWL.
C          NOW DETERMINE IF LEAF IS TO BE PRINTED.
C
 460    SIGN = RONE
        IF (LSTL+MLEAF.GT.LUPPER) GO TO 580
        IF (IQJ.LE.IQN) SIGN = (-RONE)
        IF (INN.EQ.IONE .AND. IQJ.EQ.IQN) SIGN = RONE
        IF (ISCRL.GT.6) GO TO 480
        IF (SIGN*XS(LX).LT.SV(ISCRL)) GO TO 480
        IF (ISCRL.NE.IFOUR .OR. IQJ.NE.IQN) GO TO 470
        IF (SV(4).EQ.RZERO) GO TO 480
 470    ISTORE(NSRL) = IC(ISCRL)
        NSRL = NSRL + IONE
        LLINE(ISCRL) = IQJ
        ISCRL = ISCRL + IONE
C
C       READY TO SET UP LEAF.
C
 480    GO TO (490,530,560,530), ISWT
 490    IF (LASTRK.EQ.IONE) GO TO 530
        LSP  = NSP + IW + INNI
        JLSP = JZ - ILEAF + JSTRT
        DO 510 IS=1,ILEAF
          IF (LSTEM(LSP).EQ.ISIGN(JWV) .AND. IB(JLSP).EQ.LA(45))
     1           GO TO 500
          IF (LSTEM(LSP).EQ.LA(1) .AND. IB(JLSP).EQ.LA(45)) GO TO 500
          IF (LSTEM(LSP).EQ.LA(45) .AND. IB(JLSP).EQ.LA(1)) GO TO 500
          IF (IB(JLSP).NE.LSTEM(LSP)) GO TO 520
 500      JLSP = JLSP + IONE
          LSP  = LSP + IONE
 510    CONTINUE
        GO TO 530
C
 520    LASTRK = IONE
        LSTL   = LSTL - MCOMMA
        IF (LSTL.LT.LSTRT) LSTL = LSTRT
        ISTORE(LSTL) = LA(37)
        LSTL   = LSTL + IONE
 530    IF (LSTL+MLEAF.GT.LUPPER .AND. LINT.GT.IZERO) GO TO 230
        IF (LSTL+MLEAF.GT.LUPPER .AND. LINT.LE.IZERO) GO TO 580
        IDPTH = IDPTH + IONE
        JLSP  = JLEAF
        DO 550 IS=1,LEAF
 540      JLSP = JLSP + IONE
          IF (IB(JLSP).EQ.LA(38)) GO TO 540
          ISTORE(LSTL) = IB(JLSP)
          IF (ISTORE(LSTL).EQ.LA(45)) ISTORE(LSTL) = LA(1)
          LSTL = LSTL + IONE
 550    CONTINUE
C
        LMAX = LMAX + IONE
        IF (MCOMMA.EQ.IZERO) GO TO 600
        ISTORE(LSTL) = LA(44)
        LSTL = LSTL + IONE
        GO TO 600
 560    IF (INTV.EQ.IZERO) GO TO 530
        LCHAR = IB(JLEAF)
        IF (LCHAR.EQ.LA(38)) LCHAR = IB(JLEAF-1)
        JCOMMA = IONE
        IF (LCHAR.EQ.LA(45)) LCHAR = LA(1)
 570    IF (LASTRK.GE.ITWO) GO TO 530
        NLEAF = INTV + MTOM + MCON * LASTRK
        IF (LCHAR.EQ.LA(NLEAF)) GO TO 530
        LSTL = LSTL - MCOMMA * JCOMMA
        IF (LSTL.LT.LSTRT) LSTL = LSTRT
        JCOMMA = IZERO
        ISTORE(LSTL) = LA(37)
        LSTL = LSTL + IONE
        LASTRK = LASTRK + IONE
        GO TO 570
 580    LINT = IONE
        IF (ISWT.EQ.ITHRE) GO TO 560
        GO TO 230
 590    LINT = IZERO
        ISTORE(NSTOP+3) = LSTEM(NSTOP+3)
        GO TO 530
 600    IF (LX.LT.NZ) GO TO 770
        IF (ISWT.EQ.ITHRE .AND. LASTRK.LT.ITWO .AND. INTV.NE.IZERO)
     1         GO TO 620
        IF (ISWT.NE.IONE) GO TO 230
        IF (LASTRK.NE.IZERO) GO TO 230
        LSTL = LSTL - MCOMMA
 610    ISTORE(LSTL) = LA(37)
        IF (ISWT.EQ.ITHRE .AND. LASTRK.EQ.IZERO) ISTORE(LSTL+1) = LA(37)
        GO TO 230
 620    LSTL = LSTL - MCOMMA * JCOMMA
        GO TO 610
C
C       STEM IS MIXED MODE.
C
 630    INTV = MAX0 (IZERO,((IDIV(ITHRE,KZ,IND))*MOD(IX-ITWO,ITHRE*KZ)
     1             + IONE))
        LR   = IDIV (IX-ITWO,ITHRE*KZ,IND)
        ISP  = NSP
        IF (IW.EQ.IZERO) GO TO 640
        ISTORE(ISP) = LA(39)
        IF (IQJ.GT.IQN) ISTORE(ISP) = LA(40)
        ISP = ISP + IONE
 640    IF (KZ.EQ.IONE .AND. INTV.NE.IZERO) GO TO 700
        ISTORE(ISP) = LA(INTV+1)
        ISP = ISP + IONE
 650    IF (LR.LT.IZERO) LR = IZERO
        LEAF = LZ + LR
        MCOMMA = IZERO
        IF (LEAF.GT.IONE) MCOMMA = IONE
        JLEAF = JSTRT + JZ - LR - IONE
        IF (JZ-LR+IONE.LE.IPER .AND. JZ+LZ.GT.IPER) JLEAF = JLEAF - IONE
        MLEAF = LEAF + MCOMMA + MASTRK
        IF (LR.EQ.IZERO) GO TO 670
        DO 660 IS=1,LR
          ISTORE(ISP) = LA(41)
          ISP = ISP + IONE
 660    CONTINUE
 670    IF (ISP.GT.NSTOP) GO TO 210
        LSP  = ISP - NSP
        ISP  = ISP - IONE
        NNST = NSTOP
        DO 680 IS=1,LSP
          ISTORE(NNST) = ISTORE(ISP)
          NNST = NNST - IONE
          ISP = ISP - IONE
 680    CONTINUE
C
        DO 690 IS=NSP,NNST
          ISTORE(IS) = LA(45)
 690    CONTINUE
C
        GO TO 210
 700    IF (IQ.GT.IQN) GO TO 710
        ISTORE(ISP) = LA(INTV+3)
        ISTORE(ISP+1) = LA(37)
        ISTORE(ISP+2) = LA(39)
        ISTORE(ISP+4) = LA(37)
        ISTORE(ISP+3) = LA(INTV+2)
        ISTORE(ISP+5) = LA(39)
        ISTORE(ISP+6) = LA(INTV+1)
        ISP  = ISP + 7
        MTOM = ITHRE
        MCON = -IONE
        GO TO 650
 710    ISTORE(ISP) = LA(INTV+1)
        ISTORE(ISP+1) = LA(37)
        MTOM = IONE
        MCON = IONE
        IF (IW.EQ.IZERO) GO TO 720
        ISTORE(ISP+2) = LA(40)
        ISTORE(ISP+3) = LA(INTV+2)
        ISTORE(ISP+4) = LA(37)
        ISTORE(ISP+5) = LA(40)
        ISTORE(ISP+6) = LA(INTV+3)
        ISP = ISP + 7
        GO TO 650
 720    ISTORE(ISP+2) = LA(INTV+2)
        ISTORE(ISP+3) = LA(37)
        ISTORE(ISP+4) = LA(INTV+3)
        ISP = ISP + IFIVE
        GO TO 650
 730    IF (LSTL.EQ.LSTRT) GO TO 230
        GO TO (740,230,750,230), ISWT
 740    IF (LASTRK.EQ.IONE) GO TO 230
        LSTL = LSTL - MCOMMA
        ISTORE(LSTL) = LA(37)
        GO TO 230
 750    IF (INTV.EQ.IZERO) GO TO 230
        LTOP = ITWO - LASTRK
        IF (LTOP.LE.IZERO) GO TO 230
        LSTL = LSTL - MCOMMA
        DO 760 IS=1,LTOP
          ISTORE(LSTL) = LA(37)
          LSTL = LSTL + IONE
 760    CONTINUE
        GO TO 230
 770  CONTINUE
 780   IF (IDTHST.GT.IZERO) SPDPT(1) = KDPTH - IONE
      ISPVX(1) = ISPV
      ISPVX(2) = ISPVA
      LN       = LINE
      RETURN
C
C     ..................................................................
C
 790  INND = ITEN
      RETURN
C
C     ..................................................................
C
 800  INND = 9
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 810  FORMAT (1X,133A1)
C
C     ==================================================================
C
      END
*SLOVEN
      SUBROUTINE SLOVEN
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SLOVEN V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY MATTIA HMELJCK.   JUNE 1971.
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
      DATA L(  1) / 1503007094 /
      DATA L(  2) /   77804132 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) /   81315796 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /  906013851 /
      DATA L(  8) /  906014364 /
      DATA L(  9) /  906014067 /
      DATA L( 10) /  906014580 /
      DATA L( 11) /  884414688 /
      DATA L( 12) /  906014796 /
      DATA L( 13) / 1215914396 /
      DATA L( 14) / 1495912081 /
      DATA L( 15) / 1469308991 /
      DATA L( 16) / 1469308991 /
      DATA L( 17) / 1528413391 /
      DATA L( 18) /  105401605 /
      DATA L( 19) / 1503004028 /
      DATA L( 20) / 1502709870 /
      DATA L( 21) / 1502709870 /
      DATA L( 22) /  112706900 /
      DATA L( 23) / 1490514405 /
      DATA L( 24) / 1502714729 /
      DATA L( 25) /  927010206 /
      DATA L( 26) /  927010719 /
      DATA L( 27) /  927010422 /
      DATA L( 28) / 1498914396 /
      DATA L( 29) / 1498914396 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  928910314 /
      DATA L( 32) /  928910422 /
      DATA L( 33) /  128410001 /
      DATA L( 34) /  128701126 /
      DATA L( 35) / 1434203304 /
      DATA L( 36) / 1496702187 /
      DATA L( 37) / 1898409864 /
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
      DATA L( 52) /  846907094 /
      DATA L( 53) /  813208991 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233619539 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) / 1025212165 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  846609870 /
      DATA L( 68) /  259603645 /
      DATA L( 69) / 1215909630 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  844203978 /
      DATA L( 73) /  845000000 /
      DATA L( 74) /  844313851 /
      DATA L( 75) /  844305832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  844400000 /
      DATA L( 78) /  844413851 /
      DATA L( 79) /  844405832 /
      DATA L( 80) / 1439607290 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  846608793 /
      DATA L( 83) /  831713667 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  842814396 /
      DATA L( 86) /  278700000 /
      DATA L( 87) /  296813851 /
      DATA L( 88) / 1024911273 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  334700000 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204049 /
      DATA L( 95) /  306306561 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  306306561 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) / 1902607083 /
      DATA L(103) /  475200000 /
      DATA L(104) /  424009316 /
      DATA L(105) / 1899404033 /
      DATA L(106) /  728503762 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  396100000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /  463700000 /
      DATA L(115) /  464114230 /
      DATA L(116) / 1215507817 /
      DATA L(117) / 1024008797 /
      DATA L(118) / 1430103645 /
      DATA L(119) /  480013566 /
      DATA L(120) /  305406913 /
      DATA L(121) /  486508618 /
      DATA L(122) /  497100000 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204140 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) / 1654314967 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  232710245 /
      DATA L(133) /  233603997 /
      DATA L(134) /  234403736 /
      DATA L(135) /  233604244 /
      DATA L(136) /  233501117 /
      DATA L(137) /  234001443 /
      DATA L(138) / 1617600999 /
      DATA L(139) / 1657908406 /
      DATA L(140) /  233410935 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) / 1100701283 /
      DATA L(145) / 1215912151 /
      DATA L(146) /  727809009 /
      DATA L(147) / 1208311538 /
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
      DATA L(161) / 1401209517 /
      DATA L(162) /  915601053 /
      DATA L(163) /  398405103 /
      DATA L(164) /  398405103 /
      DATA L(165) /  325505103 /
      DATA L(166) /  992707094 /
      DATA L(167) /  402413527 /
      DATA L(168) /  951513851 /
      DATA L(169) /  951514107 /
      DATA L(170) /  951514211 /
      DATA L(171) /  985612081 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) / 1018113384 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  988413169 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982900000 /
      DATA L(183) / 1003504028 /
      DATA L(184) /  992409870 /
      DATA L(185) /  992409870 /
      DATA L(186) /  989414741 /
      DATA L(187) /  990014999 /
      DATA L(188) / 1215510360 /
      DATA L(189) /  980214405 /
      DATA L(190) /  992414729 /
      DATA L(191) /  999301054 /
      DATA L(192) /  988614396 /
      DATA L(193) /  988614396 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003507143 /
      DATA L(196) / 1208210637 /
      DATA L(197) / 1208210637 /
      DATA L(198) / 1007608136 /
      DATA L(199) / 1007608626 /
      DATA L(200) /  986402187 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1035214298 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) /  972000000 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1131816819 /
      DATA L(211) / 1580003178 /
      DATA L(212) /  306310465 /
      DATA L(213) /  306310255 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) /  728503762 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) / 1025107083 /
      DATA L(220) / 1208104637 /
      DATA L(221) / 1216503494 /
      DATA L(222) /  552600000 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1208904026 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) /  728503762 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1063304257 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1899608520 /
      DATA L(236) / 1654315066 /
      DATA L(237) /  419701262 /
      DATA L(238) / 1129107439 /
      DATA L(239) /  395713269 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) /  707706561 /
      DATA L(243) / 1024301791 /
      DATA L(244) /  726504140 /
      DATA L(245) / 1400600000 /
      DATA L(246) /  381614580 /
      DATA L(247) / 1443601018 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410813851 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) /  400801262 /
      DATA L(254) / 1327606561 /
      DATA L(255) / 1427004267 /
      DATA L(256) / 1580003159 /
      DATA L(257) / 1215514163 /
      DATA L(258) /  409208919 /
      DATA L(259) /  844204023 /
      DATA L(260) /  839803403 /
      DATA L(261) /  417801278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1106214725 /
      DATA L(267) / 1106214725 /
      DATA L(268) / 1400514725 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462113851 /
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
     1      11300,   7102,   8438,   2224,  14406,  11664,  11382 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       9532,  10252,   9342,   4797,   9524,  10249,   7083 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1      10252,   9175,   9367,   9367,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1656711468, 1024907083 /
      DATA LD( 3), LD( 4) /  932910477,  161106561 /
      DATA LD( 5), LD( 6) / 1209119102, 1495911926 /
      DATA LD( 7), LD( 8) /  985611926, 1058511926 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) /  161900000,  901014715 /
      DATA LW( 2,1), LW( 2,2) / 1902007264, 1574100000 /
      DATA LW( 3,1), LW( 3,2) / 1063300729, 1440901107 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013, 1217206561 /
      DATA LW(12,1), LW(12,2) /  398107013,  342305346 /
      DATA LW(13,1), LW(13,2) / 1440901107, 1025107083 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1045200000 /
      DATA LW(15,1), LW(15,2) / 1440915908,  402400000 /
      DATA LW(16,1), LW(16,2) / 1654315120, 1443100000 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364, 1296103403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000,  744701291 /
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
      DATA LF( 9), LF(10) / 1433810400, 1419605140 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  951514211, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  396112083 /
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
      DATA LP( 5)         / 1025107083             /
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
     1        'S',    'T',    'O',    'L',    'P',    'E',    'C'/
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
*SLPTSC
      SUBROUTINE SLPTSC (ICAS,ID,ILF,IPRT,IVX,KC,LINE,LNN,LMX,LRND,INND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. SLPTSC V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PRINT THE SCRAWL AND OTHER INFORMATION ON THE BOTTOM
C       OF THE DISPLAY OF STEM AND LEAF.
C
C
C     ALGORITHM DEVELOPED BY WESLEY NICHOLSON.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-751-2845
C                  ORIGINAL VERSION -  OCTOBER, 1973.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION ID(65,*), IVX(*), ISTORE(133)
      DIMENSION LNN(*), MSPV(10)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     NOTE. 40 = 2*(MXWDTH+1) IN IB(40).  CHANGE, IF MXWDTH IS CHANGED.
C
      COMMON /SLCONS/ MXLIN, MXWDTH
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ
      COMMON /SLICHR/ IB(40), IC(6)
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL
      COMMON /SLRVAR/ RI(5), SV(6)
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      REAL             X(1)
      REAL             FDIV
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER*1      IB, IC
      CHARACTER        ID*1
      CHARACTER*1      ISTORE, MSPV
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MSPV( 1), MSPV( 2), MSPV( 3), MSPV( 4), MSPV( 5) /
     1          ' ',      'R',      'E',      'A',      'D' /
      DATA MSPV( 6), MSPV( 7), MSPV( 8), MSPV( 9), MSPV(10) /
     1          'S',      ' ',      'A',      'S',      ' ' /
C
C     ==================================================================
C
      ISPV  = IVX(1)
      ISPVA = IVX(2)
      NMMX  = MXWDTH + ITWO
      MW    = NWSL
      MND   = NDSL
      ILEAV = IZERO
C
C     COMPUTE INFORMATION NEEDED BY RULE.  ALSO, PREPARE TO PRINT
C        SCRAWL AND OTHER INFORMATION.
C
      DO 10 IS=1,133
        ISTORE(IS) = LA(45)
  10  CONTINUE
C
      TEST(1) = FDIV (FLOAT(LNN(5)-LNN(3)+1),FLOAT(LNN(6)),IND)
      TEST(2) = FDIV (FLOAT(LMX),FLOAT(NZ),IND)
      TEST(3) = LNN(6)
      IF (IPET.EQ.IZERO) GO TO 70
      ISCWL = IZERO
      MV    = IZERO
      IBOT  = IONE
      DO 50 IS=2,6
        CALL RFORMT (0,ISIGD,SV(IS),X(1),1,MXWDTH,MSW,MSD,ISTORE(1),IRF)
        IF (IS.EQ.ITWO .AND. MSW+20.GT.LUPPER) GO TO 80
        IF (ISCWL-IBOT+MSW+21.LE.LUPPER) GO TO 40
        IF (MV.GT.IZERO) GO TO 20
        WRITE (IOUT,310) NZ, (ISTORE(ISR),ISR=IBOT,ISCWL)
        MV = IONE
        GO TO 30
  20    WRITE (IOUT,320) (ISTORE(ISR),ISR=IBOT,ISCWL)
  30    IBOT  = ISCWL + IONE
  40    ISCWL = ISCWL + IONE
        ISTORE(ISCWL+1) = IC(IS)
        ISTORE(ISCWL+2) = LA(46)
        ITR = IZERO
        IF (SV(IS).LT.RZERO) ITR = IONE
        CALL RFORMT (1,ISIGD,X,SV(IS),ITR,0,MSW,MSD,ISTORE(ISCWL+3),IRF)
        ISCWL = MSW + ISCWL + ITHRE + ITR
        ISTORE(ISCWL) = LA(44)
        IF (IS.EQ.6) ISTORE(ISCWL) = LA(43)
  50  CONTINUE
C
      ITOP = ISCWL
      IF (MV.GT.IZERO) GO TO 60
      WRITE (IOUT,310) NZ, (ISTORE(IS),IS=1,ITOP)
      GO TO 80
  60  WRITE (IOUT,320) (ISTORE(IS),IS=IBOT,ITOP)
      GO TO 80
  70  IF (IPRT.EQ.IZERO .AND. IPET.NE.IZERO) GO TO 270
      RETURN
C
C     ..................................................................
C
  80  NPER = IPER - JZ
      IF (JSTRT.EQ.IONE) NPER = NPER + IONE
      WRITE (IOUT,300)
      NSTEM = ITEN**(IABS(NPER))
      IF (NPER.LE.IZERO) GO TO 100
      NSTEM = IDIV (NSTEM,ITEN,IND)
      ILEAV = IDIV (NSTEM,ITEN**LZ,IND)
      WRITE (IOUT,330) NSTEM
      IF (ILEAV.EQ.IZERO) GO TO 90
      WRITE (IOUT,340) ILEAV
      GO TO 140
  90  NLZ = LZ - NPER
      IF (NLZ.LE.IONE) GO TO 280
      WRITE (IOUT,390) (LA(1),IS=1,NLZ), LA(2)
      GO TO 140
 100  IS = ITWO
      NRNDA = NMMX + IONE
      IB(1) = LA(38)
      IB(NRNDA-1) = LA(38)
 110  NSTEM = IDIV (NSTEM,ITEN,IND)
      IF (NSTEM.EQ.IONE) GO TO 120
      IB(IS) = LA(1)
      IB(NRNDA) = LA(1)
      IS = IS + IONE
      NRNDA = NRNDA + IONE
      GO TO 110
 120  IB(IS) = LA(2)
      NRND = IS
      IB(NRNDA) = LA(1)
      DO 130 IS=1,LZ
        NRNDA = NRNDA + IONE
        IB(NRNDA) = LA(1)
 130  CONTINUE
C
      IB(NRNDA) = LA(2)
      IF (ILEAF.NE.IZERO) GO TO 290
      WRITE (IOUT,360) (IB(IS),IS=1,NRND)
      WRITE (IOUT,390) (IB(IS),IS=NMMX,NRNDA)
 140  IJK = JZ + LZ + JSTRT - IONE
      IF (IJK.LT.IONE) IJK = IONE
      ISXA = JSTRT + ITWO
      IZB  = IONE
      KE   = KC
      LRNN = LRND
      DO 150 IS=1,133
        ISTORE(IS) = LA(45)
 150  CONTINUE
C
      CALL RFORMT (1,ISIGD,X,SV(2),0,0,MW,MND,ISTORE(4),IRF)
      MARY  = (MW+IFIVE) * IFOUR
      ISCWL = MARY + MW
      CALL RFORMT (1,ISIGD,X,SV(6),0,0,MW,MND,ISTORE(MARY),IRF)
      IF (ISTORE(ISXA+1).NE.LA(38)) GO TO 160
      IZB  = ITWO
      KE   = KE - IONE
      ISXA = ISXA + IONE
      LRNN = LRNN - IONE
 160  DO 240 IS=1,2
        IJKL = IVX(IS)
        CALL RNDATM (ISTORE(ISXA+1),KE,LRNN,IND)
        IF (IND.NE.IZERO) GO TO 260
        ISXB = IZERO
        IF (ILEAV.GT.IONE) GO TO 170
        IF (ILEAV.EQ.IONE) GO TO 190
        IF (IS.EQ.IONE) IJK = IJK + IONE
        GO TO 190
 170    ISXC = ILEAV
 180    ISXC = IDIV (ISXC,ITEN,IND)
        ISXB = ISXB + IONE
        IF (ISXC.GT.IONE) GO TO 180
 190    DO 200 ISX=1,10
          IJKL = IJKL + IONE
          ID(IJKL,IS) = MSPV(ISX)
 200    CONTINUE
        IF (JSTRT.EQ.IONE) ISXA = ISXA + IONE
        IF (IZB.EQ.ITWO) ISXA = ISXA - IONE
        DO 210 ISX=1,IJK
          IJKL = IJKL + IONE
          ID(IJKL,IS) = ISTORE(ISXA)
          ISXA = ISXA + IONE
 210    CONTINUE
        ISXA = ISCWL - NWSL
        IF (IZB.EQ.ITWO) ISXA = JSTRT + ISXA - IONE
        IF (JSTRT.EQ.IONE) ISXA = ISXA - IONE
        IF (ILEAV.LE.IONE .OR. ISXB.EQ.IZERO) GO TO 230
        DO 220 ISX=1,ISXB
          IJKL = IJKL + IONE
          ID(IJKL,IS) = LA(1)
 220    CONTINUE
 230    IVX(IS) = IVX(IS) + IJK + ITEN + ISXB
 240  CONTINUE
      ISPV  = IVX(1)
      ISPVA = IVX(2)
C
      WRITE (IOUT,370) (ID(IS,1),IS=1,ISPV)
      WRITE (IOUT,370) (ID(IS,2),IS=1,ISPVA)
      WRITE (IOUT,300)
      WRITE (IOUT,380) ILF, JZ, KZ, LZ
      WRITE (IOUT,300)
      GO TO 70
C
C     ..................................................................
C
 260  INND = ITEN
      RETURN
C
C     ..................................................................
C
 270  INND = -(LINE-IPET)
      IF (IPET.EQ.IONE .AND. ICAS.EQ.IZERO) INND = INND + MXLIN - IONE
      RETURN
C
C     ..................................................................
C
 280  WRITE (IOUT,350)
      GO TO 140
 290  NSTEM = IONE
      WRITE (IOUT,330) NSTEM
      WRITE (IOUT,390) (IB(IS),IS=NMMX,NRNDA)
      GO TO 140
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 300  FORMAT (1H )
 310  FORMAT (1X/11H SCRAWL (N=,I4,1H,,120A1)
 320  FORMAT (16X,120A1)
 330  FORMAT (22H     EACH STEM UNIT IS,I7)
 340  FORMAT (22H     EACH LEAF UNIT IS,I7)
 350  FORMAT (22H     EACH LEAF UNIT IS,9H       .1)
 360  FORMAT (22H     EACH STEM UNIT IS,1X,10A1)
 370  FORMAT (9X,63A1)
 380  FORMAT (20H FOR THE ABOVE STEM ,28HAND LEAF DISPLAY, THE FOLLOW,23
     1HING VARIABLES WERE USED/3X,6H  I = ,I3,6H, J = ,I3,6H, K = ,I3,
     2     6H, L = ,I3)
 390  FORMAT (22H     EACH LEAF UNIT IS,1X,10A1)
C
C     ==================================================================
C
      END
*SLVE
      SUBROUTINE SLVE (N,M,NR,X,Y,W,WA,IT,LC,E,S,U,Q,D,A,K,B,R,Z,F,G,NI)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SLVE V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SLVE COMPUTES THE SOLUTION (COEFFICIENTS AND RESIDUALS)
C        OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT IS USED TO
C        IMPROVE (IF POSSIBLE) THE ACCURACY OF THE INITIAL SOLUTION.
C
C     SUBROUTINE SLVE IS GENERALLY CALLED TWICE FROM SUBROUTINE LSQ.
C        IN THE FIRST CALL, THE OBSERVATIONS (Y) ARE FITTED.  LET R
C           DENOTE THE RESIDUALS FROM THIS FIT.
C        IN THE SECOND CALL, THE PREDICTED VALUES (Y - R) ARE FITTED.
C           THE COEFFICIENTS OBTAINED FROM THIS FIT WILL BE USED IN
C           ASSESSING THE ACCURACY OF THE COEFFICIENTS FROM THE FIRST FIT.
C
C     INPUT PARAMETERS ...
C
C     N    NUMBER OF OBSERVATIONS.
C     M    NUMBER OF COEFFICIENTS.
C     NR   NUMBER OF MAXIMUM ROWS IN X.
C     X    MATRIX (N BY M) OF INDEPENDENT VARIABLES WHICH ARE TO BE
C             FITTED, STORED AS A VECTOR OF LENGTH (N*M BY 1), AS IN
C             SUBROUTINE LSQ.
C     Y    VECTOR (N BY 1) OF OBSERVATIONS.
C     W    VECTOR (N BY 1) OF WEIGHTS.
C     WA   CONSTANT USED FOR WEIGHTS INSTEAD OF VECTOR W(.).
C             IF WA = 0, VECTOR W IS USED FOR WEIGHTS.
C                        OTHERWISE WEIGHTS EQUAL WA.
C     IT   PARAMETER WHICH SPECIFIES WHETHER OR NOT A POLYNOMIAL TYPE
C             FIT IS TO BE PERFORMED.
C             IT = 1 INDICATES POLYNOMIAL TYPE.
C             IT = 2 INDICATES NON-POLYNOMIAL TYPE.
C                IF SLVE IS USED TO OBTAIN ACCURATE DIGITS, IT = IT + 2.
C
C     LC   VECTOR (M BY 1) WHICH GIVES THE LOCATION WITHIN THE ARRAY X
C             OF THE INDEPENDENT VARIABLES TO WHICH THE OBSERVATIONS ARE
C             TO BE FITTED.  SEE SUBROUTINE LSQ FOR FURTHER DETAILS.
C     E    MACHINE DEPENDENT PARAMETER WHOSE VALUE IS SET IN SUBROUTINE
C             LSQ.  (E IS CALLED ETA IN LSQ.)
C     S    VECTOR (M+1 BY 1) OF SCALE FACTORS.  IF ISCALE = 0 (IN
C             SUBROUTINE LSQ) THE DATA HAVE NOT BEEN SCALED, AND
C             S(I) = 1.0 FOR I=1,...,M+1.  IF ISCALE = 1, THE DATA HAVE
C             BEEN SCALED, AND S(I) EQUALS THE RECIPROCAL OF THE I-TH
C             EUCLIDEAN NORM.
C     U    MEAN OF X-VECTOR IN SCALED POLYNOMIAL TYPE PROBLEMS.  IF
C             DATA HAVE NOT BEEN SCALED, U = 0.
C     Q    MATRIX OF SIZE (N BY M+1) STORED AS A VECTOR.  THE LAST
C             COLUMN OF Q IS USED AS WORK AREA WITHIN THIS SUBROUTINE.
C     A    MATRIX WHICH WAS OBTAINED IN THE QR-DECOMPOSITION FROM
C             SUBROUTINE PDECOM, STORED AS A VECTOR OF LENGTH
C             (M+1)*(M+2)/2.
C     R    VECTOR (N BY 1) OF OLD RESIDUALS FROM FITTING Y.
C
C     INPUT AND OUTPUT PARAMETERS ...
C
C     K    PARAMETER WHICH INDICATES THE FOLLOWING --
C             ON ENTRY, K = 0 MEANS DATA HAVE NOT BEEN SCALED,
C                       K = 1 MEANS DATA HAVE BEEN SCALED.
C             ON EXIT,  K = 0 MEANS THE ITERATIVE PROCEDURE CONVERGED
C                              TO A SOLUTION,
C                       K = 1 MEANS THE ITERATIVE PROCEDURE FAILED TO
C                              CONVERGE TO A SOLUTION.
C
C     OUTPUT PARAMETERS ...
C
C     B    VECTOR (M BY 1) OF COEFFICIENTS.
C     Z    VECTOR (N BY 1) OF RESIDUALS.
C     NI   NUMBER OF ITERATIONS.
C     F(1) AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
C              SOLUTION AND FIRST ITERATION.
C
C     INTERNAL PARAMETERS ...
C
C     D    VECTOR (M+1 BY 1) USED AS WORK AREA.
C     F    VECTOR (N BY 1) USED AS WORK AREA.  F(1) IS ALSO USED FOR
C             OUTPUT (SEE ABOVE).
C     G    VECTOR (M+2 BY 1) USED AS WORK AREA.
C
C *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
C *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
C *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
C *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
C *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
C *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
C *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
C *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
C *                                                                    *
C *      THE SIMPLEST WAY TO OBTAIN THE EFFECT OF OMITTING THE         *
C *      ITERATIVE REFINEMENT (WITHOUT ACTUALLY DOING SO) IS TO CHANGE *
C *      THE ONE STATEMENT WHICH PRESENTLY READS                       *
C *        310  K = IONE                                               *
C *      TO READ                                                       *
C *        310  K = IZERO                                              *
C *                                                                    *
C *      TO ACTUALLY OMIT THE ITERATIVE REFINEMENT THE FOLLOWING       *
C *      APPROACH MAY BE USED.                                         *
C *      1. OMIT USAGE OF E, ETA2, RNB, RNDB1, RNDB2, RNDR1, RNDR2,    *
C *         RNR, AND SPCA FROM SUBROUTINE, REAL, AND DATA STATEMENTS.  *
C *      2. ATTACH LABEL  30  TO THE STATEMENT WHICH PRESENTLY READS   *
C *               DO 50 I=1,KN                                         *
C *      3. INSERT A STATEMENT READING                                 *
C *               GO TO 320                                            *
C *         IMMEDIATELY BEFORE THE STATEMENT WHICH PRESENTLY READS     *
C *          160  DO 210 ISX=1,KM                                      *
C *      4. OMIT THE FOUR BLOCKS OF STATEMENTS WHICH ARE SET OFF IN    *
C *         THE FOLLOWING MANNER --                                    *
C *                                                                    *
C BLOCK I ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C               (STATEMENTS TO BE OMITTED)
C
C BLOCK I (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C *                                                                    *
C *         BLOCK 1 CONTAINS  3 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 2 CONTIANS 10 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 3 CONTAINS 22 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 4 CONTAINS  4 STATEMENTS (EXCLUDING COMMENTS).       *
C *                                                                    *
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
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION LC(*)
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             A(*), B(*), D(*), F(*), G(*), Q(*)
      REAL             R(*), S(*), W(*), X(*), Y(*), Z(*)
      REAL             E, U, WA
      REAL             C, ETA2, DIGITS, DXNORM
      REAL             RNB, RNDB1, RNDB2, RNDR1, RNDR2
      REAL             RNR, WC, WW, XNORM
      REAL             FDIV, FDPCON, FLOG10, FSQRT
      REAL             SPCA
C
      DOUBLE PRECISION DX, DSUM, DY
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 64.0 /
C
C     ==================================================================
C
C     SET ISWAD = 0 IF COEFFICIENTS FOR ACCURATE DIGITS ARE NOT BEING
C                   COMPUTED.
C     SET ISWAD = 1 IF COEFFICIENTS FOR ACCURATE DIGITS ARE BEING
C                   COMPUTED.
C
      ISWAD = IZERO
      IF (IT.GT.ITWO) ISWAD = IONE
      KN = N
      KM = M
      MN = KM * KN
      WC = WA
      ITYP   = IT
      IF (ITYP.GT.ITWO) ITYP = ITYP - ITWO
      MPLUS1 = KM + IONE
      DIGITS = RZERO
C
C BLOCK 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      ITMAX = INT (-FLOG10(E)) - ITWO
      IF (K.EQ.IONE) ITMAX = ITMAX + ITHRE
      ETA2 = E * E
C
C BLOCK 1 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     USE ELEMENTS M*N+1, M*N+2, ..., M*N+N OF ARRAY Q AS WORK AREA.
C
      IF (WC.GT.RZERO) WW = FSQRT(WC)
      DO 10 I=1,KN
        IF (WC.LE.RZERO) WW = FSQRT(W(I))
        IF (ISWAD.EQ.IZERO) F(I) = Y(I) * WW * S(MPLUS1)
        IF (ISWAD.EQ.IONE ) F(I) = (Y(I)-FDIV(R(I),S(MPLUS1),IND)) * WW
     1                            * S(MPLUS1)
        J = MN + I
        Q(J) = RZERO
        Z(I) = RZERO
  10  CONTINUE
C
      DO 20 J=1,KM
        B(J) = RZERO
        G(J) = RZERO
  20  CONTINUE
C
      KI    = IZERO
      RNR   = RZERO
      RNB   = RZERO
      RNDB1 = RZERO
      RNDR1 = RZERO
C
C BLOCK 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      RNDB2 = RZERO
      RNDR2 = RZERO
C
C     BEGIN KI-TH ITERATION STEP.
C
  30  IF (KI.LT.ITWO) GO TO 40
      IF (SPCA*RNDB2.LT.RNDB1 .AND. RNDB2.GT.ETA2*RNB .OR.
     1    SPCA*RNDR2.LT.RNDR1 .AND. RNDR2.GT.ETA2*RNR) GO TO 40
      GO TO 300
C
  40  RNDB1 = RNDB2
      RNDR1 = RNDR2
      RNDB2 = RZERO
      RNDR2 = RZERO
C
C BLOCK 2 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF (KI.EQ.IZERO) GO TO 160
C
C     NEW RESIDUALS.
C
      DO 50 I=1,KN
        IF (WC.LE.RZERO) WW = FSQRT(W(I))
        J = MN + I
        Q(J) = Q(J) + F(I) * WW
        Z(I) = Z(I) + FDIV (F(I),WW,IRR)
  50  CONTINUE
C
      DO 100 ISX=1,KM
        B(ISX) = B(ISX) + G(ISX)
        DSUM = DZERO
        IF (ITYP.EQ.ITWO) GO TO 70
        DO 60 L=1,KN
          JJ = (LC(1)-IONE) * NR + L
          J  = MN + L
          DX = DBLE (Q(J)) * DBLE (S(ISX))
          IF (ISX.GT.IONE) DX = DX * DBLE(X(JJ)-U) ** (ISX-IONE)
          DSUM = DSUM + DX
  60    CONTINUE
        GO TO 90
C
  70    DO 80 L=1,KN
          LL   = (LC(ISX)-IONE) * NR + L
          J    = MN + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (X(LL) * S(ISX))
  80    CONTINUE
C
  90    G(ISX) = -FDPCON (DSUM)
 100  CONTINUE
C
      DO 150 I=1,KN
        DSUM = DBLE ( Z(I) )
        IF (ITYP.EQ.ITWO) GO TO 120
        DSUM = DSUM + DBLE (B(1)) * DBLE (S(1))
        IF (KM.EQ.IONE) GO TO 140
        JJ   = (LC(1)-IONE) * NR + I
        DO 110 L=2,KM
          DSUM = DSUM + DBLE(B(L))*DBLE(X(JJ)-U)**(L-IONE)*DBLE(S(L))
 110    CONTINUE
        GO TO 140
C
 120    DO 130 L=1,KM
          LL   = (LC(L)-IONE) * NR + I
          DSUM = DSUM + DBLE(B(L)) * DBLE(X(LL) * S(L))
 130    CONTINUE
C
 140    DY = DBLE ( Y(I) )
        IF (ISWAD.EQ.IONE) DY = DBLE (Y(I) - FDIV (R(I),S(MPLUS1),IND) )
        DSUM = DSUM - DY * DBLE (S(MPLUS1))
        F(I) = -FDPCON (DSUM)
        IF (WC.LE.RZERO) WW = FSQRT(W(I))
        F(I) = F(I) * WW
        IF (WW.EQ.RZERO) Z(I) = FDPCON (DBLE (Z(I)) - DSUM)
 150  CONTINUE
C
C     END NEW RESIDUALS.
C
 160  DO 210 ISX=1,KM
        LESS1 = ISX - IONE
        DSUM  = - DBLE (G(ISX))
        IF (IONE.GT.LESS1) GO TO 180
        J    = ISX
        DO 170 L=1,LESS1
          DSUM = DSUM + DBLE (D(L)) * DBLE (A(J))
          J = J + MPLUS1 - L
 170    CONTINUE
C
 180    D(ISX) = - FDPCON (DSUM)
        DO 190 L=1,KN
          IF (WC.LE.RZERO) WW = FSQRT (W(L))
          JJ   = (ISX-IONE) * KN + L
          DSUM = DSUM + DBLE (F(L)) * DBLE (Q(JJ)) * DBLE (WW)
 190    CONTINUE
C
        C  = FDPCON (DSUM)
        LD = IDIV (ITWO*(ISX-IONE)*(MPLUS1)-ISX*ISX+ITHRE*ISX,ITWO,IRR)
        C  = FDIV (C,A(LD),IRR)
        G(ISX) = C
        DO 200 I=1,KN
          IF (WC.LE.RZERO) WW = FSQRT (W(I))
          JJ   = (ISX-IONE) * KN + I
          F(I) = F(I) - C * Q(JJ) * WW
 200    CONTINUE
C
 210  CONTINUE
      DO 240 IS=1,KM
        ISX    = MPLUS1 - IS
        IPLUS1 = ISX + IONE
        DSUM   = DBLE (-G(ISX))
        IF (IPLUS1.GT.KM) GO TO 230
        LD     = IDIV (ITWO*(ISX-IONE)*(MPLUS1)-ISX*ISX+3*ISX,ITWO,IRR)
        J      = IZERO
        DO 220 L=IPLUS1,KM
          J    = J + IONE
          LJ   = LD + J
          DSUM = DSUM + DBLE (G(L)) * DBLE (A(LJ))
 220    CONTINUE
 230    G(ISX) = - FDPCON (DSUM)
 240  CONTINUE
C
C BLOCK 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DSUM = RNDB2
      DO 250 ISX=1,KM
        DSUM = DSUM + DBLE (G(ISX) * G(ISX) )
 250  CONTINUE
C
      RNDB2 = FDPCON (DSUM)
      DSUM  = RNDR2
      DO 260 I=1,KN
        DSUM = DSUM + DBLE (F(I) * F(I) )
 260  CONTINUE
C
      RNDR2 = FDPCON (DSUM)
      IF (KI.NE.IZERO) GO TO 270
      RNB = RNDB2
      RNR = RNDR2
C
C     COMPUTE DIGITS = AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN
C                         INITIAL SOLUTION AND FIRST ITERATION.
C
 270  IF (KI.NE.IONE) GO TO 290
      XNORM  = FSQRT (RNB)
      DXNORM = FSQRT (RNDB2)
      IF (XNORM.NE.RZERO) GO TO 280
      DIGITS = - FLOG10 (E)
      GO TO 290
C
 280  DIGITS = - FLOG10 (AMAX1(FDIV(DXNORM,XNORM,IND),E))
C
C     END KI-TH ITERATION STEP.
C
 290  KI = KI + IONE
      IF (KI.GT.ITMAX) GO TO 310
C
C BLOCK 3 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      GO TO 30
C
C BLOCK 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
 300  IF (RNDR2.GT.RFOR*ETA2*RNR .AND. RNDB2.GT.RFOR*ETA2*RNB) GO TO 310
      K = IZERO
      GO TO 320
C
 310  K = IONE
C
C BLOCK 4 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
 320  NI   = KI - IONE
      F(1) = DIGITS
      RETURN
C
C     ==================================================================
C
      END
*SMOOTH
      SUBROUTINE SMOOTH (X,Y,I)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SMOOTH V 7.00  5/18/90. **
C
C     ==================================================================
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      WRITE (IPRINT,10)
10    FORMAT (57H INSTALLATION MUST PROVIDE PROPRIETARY SMOOTH SUBROUTIN
     1E.)
C
      X = X
      Y = Y
      I = I
C
      RETURN
      END
*SNRPPF
      SUBROUTINE SNRPPF (P,PPF,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SNRPPF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN 0 AND STANDARD DEVIATION 1.
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
C     PRINTING--NONE
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, PAGE 933, FORMULA 26.2.23.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY, 1969), PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT, 1970), PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, PAGES 40-111. 
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 TELEPHONE  301-921-2315 
C     ORIGINAL VERSION--JUNE      1972. 
C
C               ADAPTED TO OMNITAB II COMPUTING SYSTEM BY - 
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
      REAL             P, PPF 
      REAL             ADEN, ANUM, C0, C1, C2
      REAL             D1, D2, D3, Q, R, T
      REAL             FDIV, FLOG, FSQRT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA C0 / 2.515517 /
      DATA C1 / 0.802853 /
      DATA C2 / 0.010328 /
      DATA D1 / 1.432788 /
      DATA D2 / 0.189269 /
      DATA D3 / 0.001308 /
C
C     ==================================================================
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IND = IZERO
      IF (P.GT.RZERO .AND. P.LT.RONE) GO TO 10
C
C     INPUT ARGUMENT OUTSIDE ALLOWABLE RANGE
C
      IND = IONE
  10  Q = P
      R = P
C
      IF (Q.EQ.RHALF) PPF = RZERO
      IF (Q.EQ.RHALF) RETURN
      IF (Q.GT.RHALF) R = RONE - R
      T = FSQRT (-RTWO*FLOG(R) )
      ANUM = C0 + C1*T + C2*(T**2)
      ADEN = RONE + D1*T + D2*(T**2) + D3*(T**3)
      PPF = T - FDIV (ANUM,ADEN,IND)
      IF (Q.LT.RHALF) PPF = -PPF
      RETURN
C
C     ==================================================================
C
      END 
*SOLVE
      SUBROUTINE SOLVE (UL,NN,B,X,APS)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  SOLVE V 7.00  5/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE FROM CHAPTER 17 OF G. E. FORSYTHE AND C. B. MOLER'S
C        'COMPUTER SOLUTION OF LINEAR ALGEBRAIC SYSTEMS', PRENTICE-HALL
C        (1967).
C
C     SUBROUTINE SOLVES AX = B USING UL FROM SUBROUTINE 'DECOMP'.
C
C               ADAPTED TO OMNITAB BY - 
C                      ROY H.WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LAOBRATORY,
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
      DIMENSION APS(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW     
C
      REAL             B(*), X(*)
      REAL             UL(*), XX(1)
      REAL             SUM, TSUM
      REAL             FDIV
C
C     ==================================================================
C
      N = NN
      IF (N.GT.IONE) GO TO 10 
      X(1) = FDIV (B(1),UL(1),IND)
      RETURN
C
C     ..................................................................
C
  10  NP1 = N + IONE
C
      IP = APS(1) + 0.05
      X(1) = B(IP)
      DO 30 I=2,N
        IP = APS(I) + 0.05
        IPJ = IP
        IM1 = I - IONE
        CALL SUMMAL (XX,IZERO,SUM)
        DO 20 J=1,IM1
          XX(1) = UL(IPJ) * X(J)
          IPJ = IPJ + NN
          CALL SUMMAL (XX,-IONE,SUM)
  20    CONTINUE
        CALL SUMMAL (XX,IONE,SUM)
        X(I) = B(IP) - SUM
  30  CONTINUE
C
      IPN = APS(N) + 0.05 + (N - IONE) * NN
      X(N) = FDIV (X(N),UL(IPN),IND)
      DO 50 IBACK=2,N
        I = NP1-IBACK
C
C       I GOES (N-1),...,1
C
        IP = APS(I) + 0.05
        IP1 = I + IONE
        CALL SUMMAL (XX,IZERO,SUM)
        DO 40 J=IP1,N
           IPJ = IP + (J - IONE) * NN
           XX(1) = UL(IPJ) * X(J)
          CALL SUMMAL (XX,-IONE,SUM)
  40    CONTINUE
        CALL SUMMAL (XX,IONE,SUM)
        TSUM = SUM
        IPI = IP + (I - IONE) * NN
        X(I) = FDIV ((X(I)-TSUM),UL(IPI),IND)
  50  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*SORT
      SUBROUTINE  SORT (A,B,N,NH)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SORT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SORTS REAL ARRAY A(.) INTO INCREASING ORDER.
C
C     HIERARCHY FROM 1 TO N = JJ - II + 1 IS PUT IN REAL ARRAY B(.).
C
C     IF NH GREATER THAN ZERO, SORT HIERARCHY  FOR EQUAL VALUES IN A(.).
C     IF NH=0, DO NOT SORT HIERARCHY FOR EQUAL VALUES IN A(.).
C
C               ADAPTED TO OMNITAB BY - 
C                      SALLY T. PEAVY,
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
C     ARRAYS IU(K) AND IL(K) PERMIT SORT OF 2**(K+1)-1 NUMBERS.  IF MORE
C        THAN 100,000 NUMBERS ARE TO BE SORTED, K SHOULD BE INCREASED.
C
      DIMENSION IU(16), IL(16)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             A(*), B(*)
      REAL             AB, ABC, S, SS, T, TT
C
      EQUIVALENCE (AB,IAB), (ABC,IABC)
C
C     ==================================================================
C
C     ALGORITHM 347, COMM ACM,12,3,185-187 (1969) BY R. C. SINGLETON. 
C        MODIFIED AND IMPLEMENTED BY DAVID HOGBEN, SEL, NBS.   6/22/72
C     SEE COMMENTS IN COMM.,ACM,VOL 13. P54 (JAN,70) AND 624 (OCT,70).
C      P 54 (JAN, 70) AND 624 (OCT, 70) 
C     USES REAL ARRAY A INSTEAD OF INTEGER ARRAY SPECIFIED BY A347.
C
      II = IONE
      JJ = N
      DO 10 I=1,N
        B(I) = I
  10  CONTINUE
C
      M  = IONE
      I  = II
      J  = JJ
  20  IF (I.GE.J) GO TO 90
  30  K  = I
      IJ = IDIV (J+I,ITWO,IND)
      T     = A(IJ) 
      S     = B(IJ) 
      IF (A(I).LE.T) GO TO 40 
      A(IJ) = A(I)
      B(IJ) = B(I)
      A(I)  = T
      B(I)  = S
      T     = A(IJ) 
      S     = B(IJ) 
  40  L     = J
      IF (A(J).GE.T) GO TO 60 
      A(IJ) = A(J)
      B(IJ) = B(J)
      A(J)  = T
      B(J)  = S
      T     = A(IJ) 
      S     = B(IJ) 
      IF (A(I).LE.T) GO TO 60 
      A(IJ) = A(I)
      B(IJ) = B(I)
      A(I)  = T
      B(I)  = S
      T     = A(IJ) 
      S     = B(IJ) 
      GO TO 60
  50  A(L)  = A(K)
      B(L)  = B(K)
      A(K)  = TT
      B(K)  = SS
  60  L = L - IONE
      IF (A(L).GT.T) GO TO 60 
      TT    = A(L)
      SS    = B(L)
  70  K = K + IONE
      IF (A(K).LT.T) GO TO 70 
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
      M = M + IONE
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
      T = A(I+1)
      S = B(I+1)
      IF (A(I).LE.T) GO TO 110
      K = I
 120  A(K+1) = A(K) 
      B(K+1) = B(K) 
      K = K - IONE
      IF (T.LT.A(K)) GO TO 120
      A(K+1) = T
      B(K+1) = S
      GO TO 110
 130  IF (NH.EQ.IZERO) RETURN 
      II = IZERO
      DO 170 I=2,N
        IF (A(I-1).LT.A(I)) GO TO 140
        AB = A(I-1) 
        ABC = A(I)
        IF (IAB.NE.IABC) GO TO 140
        IF (II.EQ.IZERO) II = I
        IF (I.NE.N) GO TO 170 
        ISTOP = N
        GO TO 150
 140    IF (II.EQ.IZERO) GO TO 170
        ISTOP = I - IONE
 150    INDA = IZERO
        DO 160 IJ=II,ISTOP
          IF (B(IJ-1).LT.B(IJ)) GO TO 160
          T = B(IJ-1)
          B(IJ-1) = B(IJ)
          B(IJ) = T 
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
*SORTLS
      SUBROUTINE SORTLS (A,N,M,B,L,IND) 
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SORTLS V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE PICKS M LARGEST VALUES OF VECTOR 
C       A(.) AND PUTS THEM IN DESCENDING ORDER IN VECTOR B(.).
C
C     INPUT ...
C
C     A(.)   A VECTOR OF INPUT  NUMBERS. M LARGEST VALUES ARE DESTROYED.
C     M      NUMBER OF LARGEST VALUES TO BE FOUND FROM VECTOR A. M.LE.N.
C     N      LENGTH OF VECTOR A.
C
C     OUTPUT ...
C
C     B(.)   A VECTOR CONTAINING FIRST M LARGEST NUMBERS SORTED.
C     L(.)   A VECTOR CONTAININGLOCATION IN A(.) OF B(I).
C     IND    IF M GREATER THEN N, IND = 1.
C            OTHERWISE IND = 0.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION -    MARCH, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(M)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(N), B(M)
      REAL             X
C
C     ==================================================================
C
      IND = IZERO
      IF (M.LE.N) GO TO 10
      IND = IONE
      RETURN
C
C     ..................................................................
C
  10  IF (N.GT.IONE) GO TO 20 
      B(1) = A(1)
      L(1) = IONE
      RETURN
C
C     ..................................................................
C
  20  DO 40 I=1,M
        X = A(1)
        INDEX = IONE
        DO 30 J=2,N 
          IF (X.GE.A(J)) GO TO 30
          X = A(J)
          INDEX = J 
  30    CONTINUE
        B(I) = X
        L(I) = INDEX
        A(INDEX) = RMIFY
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*SORTPP
      SUBROUTINE SORTPP (X,N,Y)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SORTPP V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS ROUTINE SORTS THE ELEMENTS OF THE INPUT VECTOR X AND PUTS THE
C     ELEMENTS INTO THE VECTOR Y.
C     THE INPUT TO THIS ROUTINE IS THE SINGLE PRECISION VECTOR X OF
C     (UNSORTED) OBSERVATIONS, THE INTEGER VALUE N (= SAMPLE SIZE),
C     AND AN EMPTY SINGLE PRECISION VECTOR Y INTO WHICH THE SORTED OBSER
C     WILL BE PLACED.
C     THE OUTPUT FROM THIS ROUTINE IS THE SINGLE PRECISION VECTOR Y INTO
C     THE SORTED OBSERVATIONS HAVE BEEN PLACED.
C     RESTRICTIONS ON THE MAXIMUM ALLOWABLE VALUE OF N--THE DIMENSIONS
C     OF VECTORS IU AND IL (DEFINED AND USED INTERNALLY WITHIN THIS ROUT
C     DETERMINE THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS
C     ROUTINE.  IF IU AND IL EACH HAVE DIMENSION K, THEN N MAY NOT EXCEE
C     2**(K+1) - 1.  FOR THIS ROUTINE AS WRITTEN, THE DIMENSIONS OF IU A
C     HAVE BEEN SET TO 36, THUS THE MAXIMUM ALLOWABLE VALUE OF N IS
C     APPROXIMATELY 137 BILLION.  SINCE THIS EXCEEDS THE MAXIMUM ALLOWAB
C     VALUE FOR AN INTEGER VARIABLE IN MANY COMPUTERS, AND SINCE A SORT
C     BILLION ELEMENTS IS PRESENTLY IMPRACTICAL AND UNLIKELY, THEREFORE
C     TEST FOR WHETHER THE INPUT SAMPLE SIZE N EXCEEDS 137 BILLION HAS B
C     INCORPORATED INTO THIS ROUTINE.  IT IS THUS ASSUMED THAT THERE IS
C     (PRACTICAL) RESTRICTION ON THE MAXIMUM VALUE OF N FOR THIS ROUTINE
C
C     PRINTING--NONE
C     THIS ROUTINE IS SINGLE PRECISION IN INTERNAL OPERATION.
C     SUBROUTINES NEEDED--NONE
C     SORTING METHOD--BINARY SORT
C     REFERENCE--CACM MARCH 1969, PAGE 186 (BINARY SORT ALGORITHM BY RIC
C                C. SINGLETON.
C              --CACM JANUARY 1970, PAGE 54.
C              --CACM OCTOBER 1970, PAGE 624.
C              --JACM JANUARY 1961, PAGE 41.
C
C     THE BINARY SORT ALGORITHM USED HEREIN IS EXTREMELY FAST AS THE
C     FOLLOWING TIME TRIALS (PERFORMED BY SORTING RANDOM NUMBERS)
C     ON THE UNIVAC 1108 EXEC 8 SYSTEM INDICATE.
C     THESE TIME TRIALS WERE CARRIED OUT IN AUGUST, 1974.
C     BY WAY OF COMPARISON, THE TIME TRIAL VALUES FOR THE EASY-TO-PROGRA
C     BUT EXTREMELY INEFFICIENT BUBBLE SORT METHOD HAVE ALSO BEEN
C     INCLUDED@D
C          NUMBER OF RANDOM            BINARY SORT       BUBBLE SORT
C           NUMBERS SORTED
C            N = 10                     .002 SEC          .002 SEC
C            N = 100                    .011 SEC          .045 SEC
C            N = 1000                   .141 SEC         4.332 SEC
C            N = 3000                   .476 SEC        37.683 SEC
C            N = 10000                 1.887 SEC      NOT COMPUTED
C
C     ADAPTION OF R. C. SINGLETON CODE
C        BY JAMES J. FILLIBEN, STATISTICAL ENGINEERING LABORATORY
C           NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C              GAITHERSBURG, MD 20899
C
C               ADAPTED TO OMNITAB 1977 COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - NOVEMBER, 1975.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IU(36), IL(36)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(*), Y(*)
      REAL             AMED, HOLD, TT
C
C     ==================================================================
C
C                         ***   ERROR CHECKING   ***
C
      IF (N.GT.IZERO) GO TO 10
        CALL ERROR ( 9)
        RETURN
C
C     ..................................................................
C
  10  IF (N.GT.IONE) GO TO 20 
        Y(1) = X(1) 
        RETURN
C
C     ==================================================================
C
  20  HOLD = X(1)
      DO 30 I=2,N
        IF (X(I).NE.HOLD) GO TO 50
  30  CONTINUE
C
      DO 40 I=1,N
        Y(I) = X(I) 
  40  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COPY THE VECTOR X INTO THE VECTOR Y
C
  50  DO 60 I=1,N
        Y(I) = X(I) 
  60  CONTINUE
C
C     CHECK TO SEE IF THE INPUT VECTOR IS ALREADY SORTED
C
      NM1 = N - IONE
      DO 70 I = 1,NM1
        IP1 = I + IONE
        IF (Y(I).LE.Y(IP1)) GO TO 70
        GO TO 80
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
  80  M = IONE
      I = IONE
      J = N
  90  IF (I.GE.J) GO TO 160
 100  K = I
      MID = IDIV (I+J,ITWO,IND)
      AMED = Y(MID) 
      IF (Y(I).LE.AMED) GO TO 110
      Y(MID) = Y(I) 
      Y(I) = AMED
      AMED = Y(MID) 
 110  L = J
      IF (Y(J).GE.AMED) GO TO 130
      Y(MID) = Y(J) 
      Y(J) = AMED
      AMED = Y(MID) 
      IF (Y(I).LE.AMED) GO TO 130
      Y(MID) = Y(I) 
      Y(I) = AMED
      AMED = Y(MID) 
      GO TO 130
 120  Y(L) = Y(K)
      Y(K) = TT
 130  L = L - IONE
      IF (Y(L).GT.AMED) GO TO 130
      TT = Y(L)
 140  K = K + IONE
      IF (Y(K).LT.AMED) GO TO 140
      IF (K.LE.L) GO TO 120
      LMI = L - I
      JMK = J - K
      IF (LMI.LE.JMK) GO TO 150
      IL(M) = I
      IU(M) = L
      I = K
      M = M + IONE
      GO TO 170
 150  IL(M) = K
      IU(M) = J
      J = L
      M = M + IONE
      GO TO 170
 160  M = M - IONE
      IF (M.EQ.IZERO) RETURN
      I = IL(M)
      J = IU(M)
 170  JMI = J - I
      IF (JMI.GE.11) GO TO 100
      IF (I.EQ.IONE) GO TO 90 
      I = I - IONE
 180  I = I + IONE
      IF (I.EQ.J) GO TO 160
      AMED = Y(I+1) 
      IF (Y(I).LE.AMED) GO TO 180
      K = I
 190  Y(K+1) = Y(K) 
      K = K - IONE
      IF (AMED.LT.Y(K)) GO TO 190
      Y(K+1) = AMED 
      GO TO 180
C
C     ==================================================================
C
      END 
*SPACE
      SUBROUTINE SPACE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  SPACE V 7.00 12/19/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE SPACE INSTRUCTION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 50 /
C
C     ==================================================================
C
      IF (NARGS.EQ.IONE) GO TO 10
      IF (NARGS.LT.IONE) GO TO 20
      CALL ERROR (10)
      RETURN
C
C     ..................................................................
C
  10  IF (KIND(1).NE.IZERO) IARGS(1) = ARGS(1)
      IF (IARGS(1).EQ.IZERO) RETURN
      IF (IARGS(1).GT.IZERO) GO TO 30
      CALL ERROR (3)
      RETURN
C
C     ..................................................................
C
  20  IARGS(1) = IONE
  30  J = MIN0 (ICA,IARGS(1)) 
      IF (NERROR.NE.IZERO) RETURN
      IF (NPAGE.EQ.IZERO) CALL PAGE (IZERO)
      DO 40 I=1,J
        WRITE (IPRINT,50)
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
  50  FORMAT (1X)
C
C     ==================================================================
C
      END 
*SPANSH
      SUBROUTINE SPANSH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SPANSH V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY JAVIER MORO. APRIL 1975.
C
C     REVISED AND UPDATED BY ING. BERNARDO BULLAIN AND ASSISTANTS
C        CENACO, LA PAZ, BOLIVIA, MAY 1976.
C           AS PART OF AID REGIONAL SEMINAR ON OMNIAB II.
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
      DATA L(  1) /   76006651 /
      DATA L(  2) /  108503160 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) /  429402736 /
      DATA L(  6) /   84208844 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82514040 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514769 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84602445 /
      DATA L( 14) /   84204631 /
      DATA L( 15) /   84616038 /
      DATA L( 16) /   84616285 /
      DATA L( 17) /   79813609 /
      DATA L( 18) /  105401605 /
      DATA L( 19) /  109516191 /
      DATA L( 20) /  110109288 /
      DATA L( 21) /  110109297 /
      DATA L( 22) /  112706900 /
      DATA L( 23) /  117911376 /
      DATA L( 24) /  117614729 /
      DATA L( 25) /  124710206 /
      DATA L( 26) /  124710395 /
      DATA L( 27) /  124710422 /
      DATA L( 28) /  126314409 /
      DATA L( 29) /  126314409 /
      DATA L( 30) /  127010395 /
      DATA L( 31) /  127010402 /
      DATA L( 32) /  127010403 /
      DATA L( 33) / 1181810982 /
      DATA L( 34) /  128701126 /
      DATA L( 35) / 1216509616 /
      DATA L( 36) /   81513527 /
      DATA L( 37) /  401204348 /
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
      DATA L( 52) /  221806651 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614436 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) /  222701702 /
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
      DATA L( 80) /  275910747 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  226913667 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  272114409 /
      DATA L( 86) /  233614613 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  305706944 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1506714176 /
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
      DATA L(102) /  188113149 /
      DATA L(103) /  413700000 /
      DATA L(104) /  424009316 /
      DATA L(105) /  695904134 /
      DATA L(106) / 1325809017 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /   84715842 /
      DATA L(115) /  462700729 /
      DATA L(116) /  470317741 /
      DATA L(117) / 1645514716 /
      DATA L(118) /  471314621 /
      DATA L(119) /  480013566 /
      DATA L(120) / 1170914721 /
      DATA L(121) /  486502759 /
      DATA L(122) /  440913152 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204132 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) / 1208105985 /
      DATA L(129) /  744301232 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) / 1410305103 /
      DATA L(133) /  673003645 /
      DATA L(134) /  673014580 /
      DATA L(135) / 1410704032 /
      DATA L(136) / 1410703895 /
      DATA L(137) / 1410811185 /
      DATA L(138) /  694213270 /
      DATA L(139) /  695804151 /
      DATA L(140) / 1170914720 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696106714 /
      DATA L(145) /  694802431 /
      DATA L(146) /   99108775 /
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
      DATA L(157) / 1208108782 /
      DATA L(158) /  879304637 /
      DATA L(159) / 1208108883 /
      DATA L(160) /  889705651 /
      DATA L(161) /  901014607 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916010253 /
      DATA L(165) /  916003054 /
      DATA L(166) /  950806651 /
      DATA L(167) /  431313154 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806927 /
      DATA L(170) /  952809734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) /  954613609 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406927 /
      DATA L(179) /  973416286 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) / 1007602227 /
      DATA L(183) /  984316191 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) / 1181811301 /
      DATA L(187) /  990014999 /
      DATA L(188) /  990404131 /
      DATA L(189) /  992711376 /
      DATA L(190) /  992414729 /
      DATA L(191) /  992711048 /
      DATA L(192) / 1001114409 /
      DATA L(193) / 1001114409 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) / 1005614839 /
      DATA L(198) /  963615067 /
      DATA L(199) /  982914629 /
      DATA L(200) /  956313527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909924 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078510935 /
      DATA L(209) / 1129514580 /
      DATA L(210) /  110816560 /
      DATA L(211) / 1142504024 /
      DATA L(212) / 1216503349 /
      DATA L(213) / 1443101162 /
      DATA L(214) /  495502620 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1325809017 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) /  559004620 /
      DATA L(220) /  102014298 /
      DATA L(221) / 1216503486 /
      DATA L(222) / 1506713896 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1208904026 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316305527 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1325809017 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327614628 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1321600000 /
      DATA L(235) / 1326111317 /
      DATA L(236) / 1443100900 /
      DATA L(237) / 1389413543 /
      DATA L(238) /  430909171 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) /  204402214 /
      DATA L(243) / 1399803729 /
      DATA L(244) / 1400201216 /
      DATA L(245) /  260411017 /
      DATA L(246) / 1388207876 /
      DATA L(247) /   82513663 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1400000000 /
      DATA L(250) / 1400005103 /
      DATA L(251) / 1400005832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1327615456 /
      DATA L(255) / 1389201342 /
      DATA L(256) /  251214100 /
      DATA L(257) /  417400819 /
      DATA L(258) / 1388815838 /
      DATA L(259) / 1315819056 /
      DATA L(260) /  275503403 /
      DATA L(261) / 1389204178 /
      DATA L(262) /  111214392 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1389316161 /
      DATA L(266) / 1443715067 /
      DATA L(267) / 1443715067 /
      DATA L(268) / 1443100729 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462105103 /
      DATA L(271) / 1462105292 /
      DATA L(272) / 1462105319 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1612703708 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) /  110816161 /
      DATA L(278) / 1539803708 /
      DATA L(279) / 1569602947 /
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
     1        729,  14843,  15648,   4797,   9524,   6928,  13270 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       4026,    788,  15658,  15657,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.) 
C
      DATA LD( 1), LD( 2) / 1326106917,  692813378 /
      DATA LD( 3), LD( 4) / 1181704797,  888800000 /
      DATA LD( 5), LD( 6) /   80104248,   98512159 /
      DATA LD( 7), LD( 8) /  973312159, 1046212159 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014625 /
      DATA LW( 2,1), LW( 2,2) /  260511694,          0 /
      DATA LW( 3,1), LW( 3,2) / 1077816065, 1169806940 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  397812213, 1215909630 /
      DATA LW(12,1), LW(12,2) /  397812213, 1399315691 /
      DATA LW(13,1), LW(13,2) /  559004620, 1326100000 /
      DATA LW(14,1), LW(14,2) / 1440915908,  234010935 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1570200000 /
      DATA LW(16,1), LW(16,2) / 1443100729,  462900729 /
      DATA LW(17,1), LW(17,2) /  234011448,  173104146 /
      DATA LW(18,1), LW(18,2) /  234011448,  174910611 /
      DATA LW(19,1), LW(19,2) /  695903852,  515114373 /
      DATA LW(20,1), LW(20,2) / 1461909153,  624700729 /
      DATA LW(21,1), LW(21,2) / 1439209087,  624700729 /
      DATA LW(22,1), LW(22,2) / 1216501458,  490103758 /
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
     1       9621,    6750,    9513,    9504,    9612,   10620/
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
      DATA LF( 7), LF( 8) /  490103758,  161300729 /
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
      DATA LP( 3), LP( 4) / 1181702336,  105801284 /
      DATA LP( 5)         /  559004620             /
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
     1        'C',    'O',    'L',    'U',    'M',    'N',    'A'/
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
*SRPPT5
      SUBROUTINE SRPPT5 (U,V,PPT05)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SRPPT5 V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT PARAMETERS ARE -
C
C           U = SAMPLE SIZE
C           V = DEGREES OF FREEDOM
C
C     OUPUT PARAMETER IS -
C
C       PPT05 = 5 PERCENT POINT OF STUTENTIZED RANGE DISTRIBUTION WITH
C                  PARAMETERS U AND V.
C
C     USES JOHN MANDEL APPROXIMATION OBTAINED FROM FITING ROWS AND
C        COLUMNS WITH Y = Y0 + A(X-X0)**B.   SUPPLIED 4 /30/70.
C
C     RANGE OF VALIDITY IS -
C                             2 .LE. U .LE. 100
C                             4 .LE. V .LE. 120
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
      REAL             PPT05, U, V
      REAL             C, R, U1, V1 
      REAL             SPC01, SPC02, SPC03, SPC04, SPC05, SPC06, SPC07
      REAL             SPC08, SPC12, SPC13, SPC14
      REAL             SPC15, SPC16, SPC17, SPC18, SPC19
      REAL             SPC23, SPC24, SPC25, SPC26, SPC27
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPC01 / -0.283917    /
      DATA SPC02 /  2.63532     /
      DATA SPC03 /  1.00123     /
      DATA SPC04 / -0.95862     /
      DATA SPC05 / -0.314115    /
      DATA SPC06 /  2.38301     /
      DATA SPC07 /  1.03428     /
      DATA SPC08 / -0.864005    /
      DATA SPC12 /  2.3849867   /
      DATA SPC13 /  2.9051857   /
      DATA SPC14 /  0.57583164  /
      DATA SPC15 / -6.964811e-2  /
      DATA SPC16 /  1.30153     /
      DATA SPC17 /  1.95073     /
      DATA SPC18 /  0.394915    /
      DATA SPC19 / -0.1339783   /
      DATA SPC23 /  6.1507548   /
      DATA SPC24 /  4.441409    /
      DATA SPC25 /  6.7514569   /
      DATA SPC26 /  7.4671282   /
      DATA SPC27 / -0.157537    /
C
C     ==================================================================
C
C     START COMPUTING.
C
       R    = SPC01 + SPC02 * (V-SPC03)**SPC04
      U1    = SPC05 + SPC06 * (V-SPC07)**SPC08
       C    = SPC12 - SPC13 * (U-SPC14)**SPC15
      V1    = SPC16 - SPC17 * (U +SPC18)**SPC19
      PPT05 = SPC23 + SPC24*R + SPC25*C + SPC26*U1*V1 + SPC27
      RETURN
C
C     ==================================================================
C
      END
*SSCRWL
      SUBROUTINE SSCRWL (X,N,ISYMBL,SVALUE,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SSCRWL V 7.00  2/19/91. **
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
C        IT STORES N, *, H, M, H AND * IN ISYMBL(I) IN A1 FORMAT.
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
C                  ORIGINAL VERSION -   AUGUST, 1973.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IC(4), ISYMBL(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL              SVALUE(*), X(*)
      REAL              FDIV
C
C     ..................................................................
C
      CHARACTER*1       IC, ISYMBL
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA IC(1), IC(2), IC(3), IC(4) / 'N', '*', 'H', 'M' /
C
C     ==================================================================
C
      IND = IZERO
      IF (N.GE.IONE) GO TO 10
      IND = IONE
      RETURN
C
C     ..................................................................
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
C
      DO 30 I=1,4
        ISYMBL(I) = IC(I)
  30  CONTINUE
C
      ISYMBL(5) = IC(3)
      ISYMBL(6) = IC(2)
      RETURN
C
C     ==================================================================
C
      END
*STADIF
      SUBROUTINE STADIF (NZW,X,DIF)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STADIF V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE DIFFERENCES BETWEEN SUCCESSIVE X(I).
C
C     INPUT ...
C
C       NZW     NUMBER OF ENTERIES IN VECTOR X(.).
C         X     A VECTOR OF SORTED DATA.
C
C     OUTPUT ...
C
C       DIF     A VECTOR CONTAINING NZW-1 SUCCESSIVE DIFFERENCES.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -            1967.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             X(*), DIF(*)
C
C     ==================================================================
C
C     COMPUTE DIFFERENCES BETWEEN SUCCESSIVE SORTED X(I)
C        AND PUT IN DIF(I).
C
      IXNM1 = NZW - IONE
C
      DO 10 I=1,IXNM1
        DIF(I) = X(I+1) - X(I)
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STAFRQ
      SUBROUTINE STAFRQ (NZW,RANGE,X,FREQ,DELX)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STAFRQ V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE FREQUENCY DISTRIBUTION WITH 10 CLASSES OF EQUAL LENGTH
C        FOR STATISISTICAL ANALYSIS INSTRUCTIONS.
C
C     INPUT ...
C
C         NZW     NUMBER OF ENTERIES IN VECTOR X(.).
C       RANGE     VALUE OF RANGE.
C           X     A VECTOR OF DATA.
C
C     OUTPUT ...
C
C        FREQ     A VECTOR CONTAINING THE 10 FREQUENCIES OF X.
C        DELX     LENGTH OF FREQUENCY DISTRIBUTION
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
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             FREQ(*), X(*)
      REAL             DELX, RANGE
      REAL             XB, XT
      REAL             FDIV
C
C     ==================================================================
C
      DELX = FDIV (RANGE,RTEN,IND)
      XB   = X(1)
      XT   = XB + DELX
C
C     COMPUTE AND STORE FREQUENCY DISTRIBUTION IN FREQ(51),...,FREQ(60).
C
      LSTART = IONE
      DO 30 I=1,10
        IC = IZERO
        DO 10 L=LSTART,NZW
          IF (X(L).GE.XT) GO TO 20
          IC = IC + IONE
  10    CONTINUE
C
        L = NZW
  20    FREQ(I) = FLOAT (IC)
        XT = XT + DELX
        LSTART = L
  30  CONTINUE
C
      DO 40 I=L,NZW
        IF (X(I).GE.XT-DELX) FREQ(10) = FREQ(10) + RONE
  40  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STALSD
      SUBROUTINE STALSD (ND,N,X,IA,ID,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. STALSD V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE FREQUENCY DISTRIBUTION OF LEAST SIGNIFICANT DIGIT OF
C          N MEASUREMENTS IN
C          X VECTOR, USING
C         ND SIGNIFICANT DIGITS, WITH RESULTS STORED IN
C         ID FOR 0, 1, 2, ..., 9
C
C         IA IS USED FOR STORING RESULTS FROM PROGRAM UNIT RFORMT.
C
C        IND = 0, IF RESULTS ARE OBTAINED.
C            = 1, IF RESULTS ARE NOT OBTAINED BECAUSE NUMBERS
C                      VARY SO MUCH THAT WIDTH EXCEEDS MAXDIG.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1978.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IA(*), ID(*)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      REAL             X(*)
      REAL             R(1)
C
C     ..................................................................
C
      CHARACTER        LA*1
      CHARACTER        IA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA MAXDIG / 21 /
C
C     ==================================================================
C
      IND = IZERO
      M   = N
C
C     ROUND TO ONE LESS SIGNIFICANT DIGIT.
C
      NN = ND - IONE
C
C     DETERMINE WIDTH OF FIELD.
C
      CALL RFORMT (0,NN,X,R(1),M,MAXDIG,NWID,NDEC,IA(1),IRF)
      IF (IRF.NE.IZERO) GO TO 90
C
C     DETERMINE POSITION K IN IA(.) OF LEAST SIGNIFICANT DIGIT.
C
      K  = NWID
      KK = NWID
      DO 30 I=1,KK
        DO 10 J=1,M
          CALL RFORMT (1,NN,R,X(J),0,0,NWID,NDEC,IA(1),IRF)
          IF (IA(K).EQ.LA(38)) GO TO 20
          IF (IA(K).EQ.LA(45)) GO TO 10
          IF (IA(K).NE.LA( 1)) GO TO 40
  10    CONTINUE
  20    K = K - IONE
  30  CONTINUE
      GO TO 90
C
C     CLEAR ID(.).
C
  40  DO 50 I=1,10
        ID(I) = IZERO
  50  CONTINUE
C
      DO 80 I=1,M
        CALL RFORMT (1,NN,R,X(I),0,0,NWID,NDEC,IA(1),IRF)
        DO 70 J=1,10
          IF (IA(K).EQ.LA(45)) GO TO 60
          IF (IA(K).NE.LA( J)) GO TO 70
  60      ID(J) = ID(J) + IONE
          GO TO 80
  70    CONTINUE
  80  CONTINUE
      RETURN
C
C     ..................................................................
C
  90  IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*STARNK
      SUBROUTINE STARNK (NZW,HIER,X,RANK)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STARNK V 7.00  4/30/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE RANKS FOR STATISTICAL ANALYSIS INSTRUCTIONS.
C
C     INPUT ...
C
C         NZW       NUMBER OF ENTERIES IN VECTOR X.
C       IHIER       A VECTOR CONTAINING THE HIERARCHY OF VECTOR X.
C           X       A VEXTOR OF DATA.
C
C     OUTPUT ...
C
C        RANK       A VECTOR CONTAING RANKS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -           1967.
C                   CURRENT VERSION -    APRIL, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION HIER(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      REAL             RANK(*), X(*)
      REAL             RNS, RNSS
      REAL             FDIV
C
C     ==================================================================
C
      LLA   = IONE
      IXNM1 = NZW - IONE
      DO 10 I=1,NZW
        K = HIER(I) + 0.5
        RANK(K) = FLOAT(LLA)
        LLA = LLA + IONE
  10  CONTINUE
C
      K    = IZERO
      RNS  = RZERO
      RNSS = RONE
C
C     COMPUTE RANKS AND STORE IN SA(I,1).
C
      LR = IZERO
      DO 60 I=1,IXNM1
        IF (X(I).NE.RZERO .AND. K.EQ.IZERO) GO TO 40
        IF (X(I).NE.RZERO) GO TO 20
        RNS = RNS + RNSS
        K = K + IONE
        GO TO 50
  20    K   = K + IONE
        RNS = RNS + RNSS
        RNS = FDIV (RNS,FLOAT(K),IND)
        DO 30 L=1,K
          LR  = LR + IONE
          LRR = HIER(LR) + 0.5
          RANK(LRR) = RNS
  30    CONTINUE
        LR   = LR - IONE
        RNS  = RZERO
        K    = IZERO
  40    LR   = LR + IONE
  50    RNSS = RNSS + RONE
  60  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STMT
      SUBROUTINE STMT (NSTMT)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   STMT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ASSEMBLE AND CHECK AN INSTRUCTION NUMBER.
C
C        CALLED BY OMNIT.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***

      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 10000 /
C
C     ==================================================================
C
      MISC = ITEN * KARD(KRDPOS)
  10  KRDPOS = KRDPOS + IONE
      K = KARD(KRDPOS)
      IF (K.GE.ITEN) GO TO 30
      MISC = ITEN * (MISC+K)
      IF (MISC.LT.ICA) GO TO 10
C
C     ILLEGAL STATEMENT NUMBER, EXIT.
C
  20  KARG = IONE
      RETURN
C
C     ..................................................................
C
C     NON-NUMERIC FOUND, IS IT A .
C
  30  IF (K.EQ.37) GO TO 50
C
C     IS IT A /
C
  40  IF (K.EQ.36) GO TO 70
C
C     IS IT A SPACE
C
      IF (K.EQ.44) GO TO 60
      GO TO 20
C
C     . FOUND, IT MUST BE FOLLOWED BY ONE AND ONLY ONE NUMERAL.
C
  50  KRDPOS = KRDPOS + IONE
      K = KARD(KRDPOS)
      IF (K.GE.ITEN) GO TO 40
      MISC = MISC + K
  60  KRDPOS = KRDPOS + IONE
      K = KARD(KRDPOS)
      GO TO 40
  70  KRDPOS = KRDPOS + IONE
      K = KARD(KRDPOS)
C
C     / FOUND, IT MUST BE FOLLOWED BY BLANKS AND/OR A LETTER.
C
      IF (K.EQ.44) GO TO 70
      IF (K.GE.36 .OR. K.LT.10) GO TO 20
C
C     LEGAL STATEMENT NUMBER FOUND.
C
      NSTMT = MISC
      KARG = IZERO
      RETURN
C
C     ==================================================================
C
      END
*STORE
      SUBROUTINE STORE (J)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  STORE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C         STORAGE LAYOUT..          STATEMENT NUMBER
C                                   NUMBER OF WORDS IN ENTRY
C                                   NARGS+64*(L1+64*L2)
C     ALL ITEMS ARE STORED IN       (   ENTRY 1    )
C     FLOATING POINT TO ALLOW       (         2    )
C     CONVERSION TO DOUBLE-              .....
C     PRECISION.                    ( LAST WORD    )
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 64 /
C
C     ==================================================================
C
      IF (IOVFL.NE.IZERO) RETURN
      IZE = J + ITWO
      IF (NSTMT.GT.NSTMTH) GO TO 70
C
C         STATEMENT IS AN INSERTION OR A REPLACEMENT
C
      L = LOCATE(NSTMT)
      IF (L.GT.IZERO) GO TO 30
C
      L = -L
      IDIF = IZE
  10  LL = NCOM
C
C         STATEMENT IS AN INSERTION, OPEN GAP.
C
      II = LL + IDIF
      IF (II.GE.LCOM) GO TO 80
      DO 20 I=L,NCOM
        COM(II) = COM(LL)
        II = II - 1
        LL = LL - 1
  20  CONTINUE
      GO TO 50
C
C         STATEMENT IS REPLACEMENT.
C
  30  IDIF = IZE - IFIX (COM(L+1))
      IF (IDIF.EQ.IZERO) GO TO 50
      IF (IDIF.GT.IZERO) GO TO 10
C
C         NEW STATEMENT SMALLER THAN OLD, CLOSE UP GAP.
C
      I = L - IDIF
      II = L
      DO 40 IA=I,NCOM
        COM(II) = COM(IA)
        II = II + IONE
  40  CONTINUE
C
C         INSERT STATEMENT.
C
  50  COM(L) = NSTMT
      COM(L+1) = IZE
      COM(L+2) = NARGS + ICA * (L1+ICA*L2)
      NCOM = NCOM + IDIF
      IF (IZE.EQ.ITHRE) RETURN
      DO 60 I=4,IZE
        COM(L+3) = ARGTAB(I-3)
        L = L + IONE
  60  CONTINUE
      RETURN
C
C     ..................................................................
C
C         PUT STATEMENT ON END.
C
  70  L = NCOM
      IDIF = IZE
      NSTMTX = NSTMTH
      NSTMTH = NSTMT
      IF (NCOM+IDIF.LT.LCOM) GO TO 50
C
C         COM STORAGE OVERFLOW
C
  80  IOVFL = IONE
      CALL ERROR (12)
      RETURN
C
C     ==================================================================
C
      END
*STORMT
      SUBROUTINE STORMT (C,N,NP,K,A)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STORMT V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE STORES MATRIX C(NP,K) FROM SCRATCH AREA A
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             A(*), C(N,*)
C
C     ==================================================================
C
      IS = IONE
      DO 20 J=1,K
        DO 10 I=1,NP
          C(I,J) = A(IS)
          IS = IS + IONE
  10    CONTINUE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*STRLNG
      SUBROUTINE STRLNG (LNGE,MAXCOM,LTOTAL,MM)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STRLNG V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT TO STORE ALL COMMANDS IN SINGLE ARRAY LNGE (.)
C        TO ALLOW SORTING FOR VOCABULARY.
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
      DIMENSION LNGE(MAXCOM,*), MM(*), IRTAPE(2)
C
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD
      COMMON /ARRAYB/ IALPH(6), ICL(10,2), ICP(6), ID(8,2) 
      COMMON /ARRYBC/ ICOLHD(7)
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER ICOLHD*1
      CHARACTER MM*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NMEMLT / 100000 /
C
C     IRTAPE(.) ARE THE VALUES OF I IN IR(I,1) TO PICK UP TAPE AND UNIT.
C
      DATA IRTAPE(1) / 273 /
      DATA IRTAPE(2) / 279 /
C
C     IRCNSR IS THE VALUE OF I IN IR(I,1) TO PICK UP CENSOR.
C
      DATA IRCNSR    /  58 /
C
C     ITAPEQ IS THE NUMBER TO GET Q TO PUT IN TAPE Q.
C
      DATA ITAPEQ    /  17 /
C
C     ICLCMP IS THE VALUES OF NAME(1) AND NAME(2) FOR CALCOMP.
C
      DATA ICLCMP / 222602605 /
C
C     ==================================================================
C
C     LTOT = TOTAL NUMBER OF COMMANDS.
C
      LTOT = IONE
C
C     (1)   ONE-WORD COMMANDS IR(.).
C
      DO 10 I=1,NIR
        IF (IR(I,1).EQ.IZERO) GO TO 10
        LNGE(LTOT,1) = IR(I,1)
        LTOT         = LTOT + IONE
  10  CONTINUE
C
C     (2)   PRINT NOTE EXECUTED BY LOOKUP.
C
      LNGE(LTOT,1) = NMEMLT * NL(13) + NL(14)
      LNGE(LTOT,2) = NMEMLT * NL(7) + NL(8)
      LTOT         = LTOT + IONE
C
C     (3)   COMMANDS EXECUTED BY OMNIT.
C
      DO 70 I=1,7
        K = ITWO * I - IONE
        IF (I.EQ.7) K = K + ITWO
        GO TO (60,60,60,20,30,50,60), I
C
C       NOTE, NOTE1, NOTE2
C
  20    LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
        LTOT         = LTOT + IONE
        LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
        LNGE(LTOT,2) = NMEMLT * IONE
        LTOT         = LTOT + IONE
        LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
        LNGE(LTOT,2) = NMEMLT * ITWO
        LTOT         = LTOT + IONE
        GO TO 70
C
C       TITLE1, TITLE2, TITLE3, TITLE4, TITLEX, TITLEY.
C
  30    DO 40 J=1,6
          LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
          IF (J.LT.IFIVE) LNGE(LTOT,2) = NMEMLT * (J+ITWO)
          IF (J.EQ.IFIVE) LNGE(LTOT,1) = NMEMLT * NL(K) + NL(17)
          IF (J.EQ.6)     LNGE(LTOT,1) = NMEMLT * NL(K) + NL(18)
          LTOT         = LTOT + IONE
  40    CONTINUE
        GO TO 70
C
C       FORMAT 'L'.
C
  50    LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
        LNGE(LTOT,2) = NMEMLT * IALPH(1)
        LTOT         = LTOT + IONE
        GO TO 70
  60    LNGE(LTOT,1) = NMEMLT * NL(K) + NL(K+1)
        LTOT         = LTOT + IONE
  70  CONTINUE
C
C     (4)   ID(.) = RESET, PRINT, PUNCH, READ, ABRIDGE, APRINT, MPRINT.
C
      DO 90 I=1,NID
        DO 80 J=1,2
          LNGE(LTOT,1) = ID(I,1)
          IF (I.EQ.IONE .AND. J.EQ.ITWO) LNGE(LTOT,2) = NMEMLT*NALPH(1)
          IF (I.GT.IONE .AND. J.EQ.ITWO) LNGE(LTOT,2) = NMEMLT*IALPH(1)
          LTOT         = LTOT + IONE
  80    CONTINUE
  90  CONTINUE
C
C     (5)  TWO-WORD COMMANDS IN IRD(.).
C
      DO 100 I=1,NIRD
        IF (IRD(I,1).EQ.IZERO) GO TO 100
        LNGE(LTOT,1) = IRD(I,1)
        LNGE(LTOT,2) = IRD(I,2)
        LTOT         = LTOT + IONE
        IF (I.NE.IFIVE) GO TO 100
C
C       M(X'X) AND M(X'AX) HAVE THE SAME NAME(I).
C
        LNGE(LTOT,1) = IRD(I,1)
        LNGE(LTOT,2) = IRD(I,2)
        LTOT         = LTOT + IONE
C
 100  CONTINUE
C
C     (6)   UNIT AND TAPE OPERATION COMMANDS IN ITP(.).
C
      DO 120 I=1,NITP
        DO 110 J=1,2
          LNGE(LTOT,1) = ITP(I,1)
          K            = IRTAPE(J)
          LNGE(LTOT,2) = IR(K,1) + IONE
          LTOT         = LTOT + IONE
 110    CONTINUE
 120  CONTINUE
C
      DO 140 I=1,3
        DO 130 J=1,2
          LNGE(LTOT,1) = ITP(I,1)
          K            = IRTAPE(J)
          LNGE(LTOT,2) = IR(K,1) + ITAPEQ
          LTOT         = LTOT + IONE
 130    CONTINUE
 140  CONTINUE
C
C     (7)   CENSOR XX IN ICP(.).
C
      DO 150 I=1,6
        LNGE(LTOT,1) = IR(IRCNSR,1)
        LNGE(LTOT,2) = NMEMLT * ICP(I)
        LTOT         = LTOT + IONE
 150  CONTINUE
C
C     (8)   CALCOMP IN ICL(.).
C
      DO 160 I=1,8
        LNGE(LTOT,1) = ICLCMP
        LNGE(LTOT,2) = ICL(I,1)
        LTOT         = LTOT + IONE
  160 CONTINUE
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DO 170 I=1,NDIST
        LNGE(LTOT,1) = IDIST(I)
        LTOT         = LTOT + IONE
 170  CONTINUE
C
C     (10)   PROPERTIES IN IPROP(.).
C
      DO 180 I=1,NPROP
        LNGE(LTOT,1) = IPROP(I)
        LTOT         = LTOT + IONE
 180  CONTINUE
C
C     (11)   TABLE COMMANDS IN ITB(.).
C
      DO 190 I=1,NITB
        LNGE(LTOT,1) = ITB(I)
        LTOT         = LTOT + IONE
 190  CONTINUE
C
C     (12)   LANGUAGES IN IL(.).
C
      DO 200 I=1,NIL
        LNGE(LTOT,1) = IL(I,1)
        LTOT         = LTOT + IONE
 200  CONTINUE
C
C     (13)   COLUMN IN ICOLHD(.)
C
      DO 210 I=1,6
        MM(I+1) = ICOLHD(I)
 210  CONTINUE
      LTOT   = LTOT - IONE
      LTOTAL = LTOT
      RETURN
C
C     ==================================================================
C
      END      
*STRUVE
      SUBROUTINE STRUVE (Z,A,B,C)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. STRUVE V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE STRUVE ZERO AND STRUVE ONE.
C
C               WRITTEN BY -
C                      BRADLEY A. PEAVY
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                  ORIGINAL VERSION -  JANUARY, 1978. 
C                   CURRENT VERSION - FEBRUARY, 1990.
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
      DOUBLE PRECISION C(*)
      DOUBLE PRECISION A, B, P, Q, R, S, X, Z
      DOUBLE PRECISION DBEJ
      DOUBLE PRECISION FDDIV
      DOUBLE PRECISION DPCA, DPCB, DPCC, DPCD, DPCE, DPCF, DPCG, DPCH
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DPCA / 70.0D0           /
      DATA DPCB /  0.78539816339D0 /
      DATA DPCC /  9.0D0           /
      DATA DPCD / 25.0D0           /
      DATA DPCE / 49.0D0           /
      DATA DPCF /  3.0D0           /
      DATA DPCG / 15.0D0           /
      DATA DPCH / 35.0D0           /
C
C     ==================================================================
C
      X = DABS(Z)
      IF (X.GT.DZERO) GO TO 10
      A = DZERO
      B = DZERO
      RETURN
C
C     ..................................................................
C
  10  IF (X.GT.DPCA) GO TO 30
      CALL BEJN (IZERO,C,X)
      P = DZERO
      Q = DZERO
      DO 20 I=1,49
        J = ITWO*I
        K = J + IONE
        R = J - IONE
        S = IFOUR*I**2 - IONE
        P = P + FDDIV (C(J),R,IND)
        Q = Q + FDDIV (C(K),S,IND)
  20  CONTINUE
      A = FDDIV (P,DPCB,IND)
      B = FDDIV (DTWO*Q+DONE-C(1),DHLFPI,IND)
      RETURN
C
C     ..................................................................
C
  30  S = FDDIV (DONE,X**2,IND)
      P = DONE - S*(DONE-DPCC*S*(DONE-DPCD*S*(DONE-DPCE*S)))
      A = DBEJ (X,IZERO,IFIVE) + FDDIV (P,X*DHLFPI,IND)
      Q = DONE + S*(DONE-DPCF*S*(DONE-DPCG*S*(DONE-DPCH*S)))
      B = DBEJ (X,IONE,IFIVE) + FDDIV (Q,DHLFPI,IND)
      RETURN
C
C     ==================================================================
C
      END
*SUMMAL
      SUBROUTINE SUMMAL (X,NN,SUM)
C
C **  NBS OMNITAB 1978 VERSION 6.00 12/30/78. SUMMAL V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROCEDURE IS WRITTEN ESPECIALLY FOR CDC MACHINE.
C     IT REPLACES THE SUMMAL USED BY OMNITAB FOR
C     MACHINES WITH 36 BIT WORD LENGTH AND 8 BITS FOR
C     EXPONENT OF REAL NUMBER.
C
C     NN EQUALS       ZERO, CLEAR AREA TO PREPARE FOR NEW SUM.
C     NN EQUALS        ONE, OBTAIN FINAL SUM.
C     NN GREATER THAN  ONE, CLEAR, DO SUM ON NN TERMS AND GET FINAL SUM.
C     NN LESS THAN    ZERO, CONTINUE SUM FOR NEXT ABS(NN) TERMS,
C                              DO NOT GET FINAL SUM.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1971.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION X(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      REAL             SUM, X
C
      REAL             SPOS, SNEG
C
      DATA RZERO /0.0/
C
C     ==================================================================
C
      IF(NN) 30,10,20
  10  SPOS = RZERO
      SNEG = RZERO
      RETURN
C
C     ..................................................................
C
  20  IF (NN.EQ.IONE) GO TO 50
      SPOS = RZERO
      SNEG = RZERO
C
  30  N = IABS (NN)
      DO 40 I=1,N
        IF (X(I).LT.RZERO) SNEG = SNEG + X(I)
        IF (X(I).GE.RZERO) SPOS = SPOS + X(I)
  40  CONTINUE
C
      IF (NN.LT.IZERO) RETURN
C
  50  SUM = SPOS + SNEG
      RETURN
C
C     ==================================================================
C
      END
*SUNIMD
      SUBROUTINE SUNIMD (N,X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SUNIMD V 7.00  2/22/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT UNIMED.
C
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
C                 PHONE:  301-921-2315
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
*SWEDSH
      SUBROUTINE SWEDSH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SWEDSH V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY HANS ZETTERBURG.   MARCH 1973.
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
      DATA L(  1) / 1461103069 /
      DATA L(  2) / 1493603063 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) / 1215502449 /
      DATA L(  6) / 1509409829 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82514040 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514769 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84104132 /
      DATA L( 14) / 1483715081 /
      DATA L( 15) / 1469716038 /
      DATA L( 16) / 1469716285 /
      DATA L( 17) / 1506703069 /
      DATA L( 18) /  105401605 /
      DATA L( 19) / 1475418785 /
      DATA L( 20) / 1495209288 /
      DATA L( 21) / 1495209297 /
      DATA L( 22) /  112707110 /
      DATA L( 23) / 1472204042 /
      DATA L( 24) / 1473912083 /
      DATA L( 25) /  125110206 /
      DATA L( 26) /  125110395 /
      DATA L( 27) /  125110422 /
      DATA L( 28) / 1511401458 /
      DATA L( 29) / 1511402016 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  127010395 /
      DATA L( 32) /  127010422 /
      DATA L( 33) /  128410080 /
      DATA L( 34) / 1513801126 /
      DATA L( 35) /  961603989 /
      DATA L( 36) / 1497309072 /
      DATA L( 37) / 1439213663 /
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
      DATA L( 52) /  805002916 /
      DATA L( 53) /  813616285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614436 /
      DATA L( 59) /  843711993 /
      DATA L( 60) /  239500000 /
      DATA L( 61) / 1471808168 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  839109297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) / 1326901274 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  844213269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261105103 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261200000 /
      DATA L( 78) /  261205103 /
      DATA L( 79) /  261205832 /
      DATA L( 80) /  112701053 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  846608793 /
      DATA L( 83) /  851008560 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  855302016 /
      DATA L( 86) /  273609078 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  695913531 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1038018785 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  318100000 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  318106674 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390714724 /
      DATA L(101) /  390214722 /
      DATA L(102) / 1315304132 /
      DATA L(103) /  470304955 /
      DATA L(104) /  424009316 /
      DATA L(105) /  215300000 /
      DATA L(106) / 1327304190 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  112706708 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) / 1326413276 /
      DATA L(115) /  305406913 /
      DATA L(116) /  470317741 /
      DATA L(117) / 1575711024 /
      DATA L(118) /  472314621 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486108568 /
      DATA L(121) /  486508618 /
      DATA L(122) /  492309075 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204132 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  609301226 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) / 1129800000 /
      DATA L(133) / 1129813851 /
      DATA L(134) / 1130500000 /
      DATA L(135) / 1129809477 /
      DATA L(136) / 1129900000 /
      DATA L(137) / 1130108748 /
      DATA L(138) / 1439605590 /
      DATA L(139) / 1181900986 /
      DATA L(140) /  597914619 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  710613167 /
      DATA L(146) /  708908901 /
      DATA L(147) /  710613275 /
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
      DATA L(161) / 1216505590 /
      DATA L(162) /  915601053 /
      DATA L(163) /  398405103 /
      DATA L(164) /  398405103 /
      DATA L(165) / 1483809160 /
      DATA L(166) /  950803069 /
      DATA L(167) /  952402404 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806933 /
      DATA L(170) /  952809734 /
      DATA L(171) /  973415081 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) /  996403069 /
      DATA L(176) /  963215003 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  965118785 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) /  989403967 /
      DATA L(187) /  990014985 /
      DATA L(188) /  472315121 /
      DATA L(189) /  961904042 /
      DATA L(190) /  963612083 /
      DATA L(191) / 1000101054 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1001102016 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) / 1005614839 /
      DATA L(198) / 1007608136 /
      DATA L(199) / 1007608371 /
      DATA L(200) /  987009072 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) /  459914406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1062308748 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1608306602 /
      DATA L(211) / 1316305526 /
      DATA L(212) /  306312165 /
      DATA L(213) /  306314431 /
      DATA L(214) /  306304955 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1327304190 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) /  316005590 /
      DATA L(220) / 1208118716 /
      DATA L(221) / 1216503494 /
      DATA L(222) / 1548318785 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  430911318 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316305502 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1327304190 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327614628 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1364800729 /
      DATA L(235) /  134115691 /
      DATA L(236) / 1315314431 /
      DATA L(237) / 1585405252 /
      DATA L(238) /  844214892 /
      DATA L(239) / 1562113613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) /  890300729 /
      DATA L(243) / 1399803962 /
      DATA L(244) / 1415709018 /
      DATA L(245) /  695900918 /
      DATA L(246) / 1580005594 /
      DATA L(247) /  133411441 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410805103 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1566505252 /
      DATA L(254) /  396401278 /
      DATA L(255) / 1590413366 /
      DATA L(256) / 1427414733 /
      DATA L(257) / 1315304861 /
      DATA L(258) / 1575609441 /
      DATA L(259) /  861413547 /
      DATA L(260) /  861403403 /
      DATA L(261) / 1584201278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1548304153 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
      DATA L(268) /  983313149 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462105103 /
      DATA L(272) / 1462105832 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) /  470313717 /
      DATA L(278) / 1585205969 /
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
     1      11300,   7102,  14196,  14607,  14406,  12096,  14739 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1      15067,  13691,  13376,   8800,  11522,  15091,   2484 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       8436,  13691,  13390,  13389,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) / 1315301127, 1509102484 /
      DATA LD( 3), LD( 4) / 1439210720,  296300729 /
      DATA LD( 5), LD( 6) / 1315315868, 1513818317 /
      DATA LD( 7), LD( 8) / 1003518317,  493218317 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) /  694604023, 1216505590 /
      DATA LW( 2,1), LW( 2,2) /  340300000,  679211664 /
      DATA LW( 3,1), LW( 3,2) / 1088100000, 1409800729 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,          0 /
      DATA LW(12,1), LW(12,2) /  111113149,  398107013 /
      DATA LW(13,1), LW(13,2) / 1409803160,          0 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1062308748 /
      DATA LW(15,1), LW(15,2) / 1440915908,  420500000 /
      DATA LW(16,1), LW(16,2) / 1315314431, 1443109630 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364,  861403403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1389210623 /
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
      DATA LF( 9), LF(10) / 1433810400, 1419609929 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  952809734, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  112706708 /
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
     1        'K',    'O',    'L',    'U',    'M',    'N',    ' '/
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
*SYMV
      SUBROUTINE SYMV (A,N,K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SYMV V 7.00  5/23/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT ...
C        A      FIRST ELEMENT OF MATRIX A
C        N      PRESENT SIZE OF A
C
C     OUTPUT ...
C        K   STATUS FOR SYMMETRY
C        K = 0, EXACT SYMMETRY, A(I,J)/A(J,I)) = 1
C        K = 1, SYMMETRY TO A RELATIVE ERROR
C                  ABS(1-A(I,J)/A(J,I)) = OR LESS THAN 1.E-7
C        K = 2, NO SYMMETRY
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
C                   CURRENT VERSION -      MAY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*)
      REAL             ACURCY, DGTOL
C
C     ==================================================================
C
      K     = IZERO
      NN    = N - IONE
      DGTOL = RSD - RTWO + RHALF
      DO 20 I=1,NN
        JBEG = I + IONE
        JX   = (I - IONE) * NROW
        DO 10 J=JBEG,N
         IJ = I + ( J - IONE ) * NROW
         JI = J + JX
          CALL ACCDIG (A(JI),A(IJ),DGTOL,ACURCY,IND)
          IF (ACURCY.GE.RSD) GO TO 10
          K = IONE
          IF (IND.EQ.IZERO) GO TO 10
          K = ITWO
          RETURN
  10    CONTINUE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SYMW
      SUBROUTINE SYMW (A,NROW,N,K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SYMW V 7.00  7/ 7/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT ...
C        A      FIRST ELEMENT OF MATRIX A
C        NROW   NUMBER OF ROWS IN A AS DEFINED IN A DIMENSION STATEMENT
C        N      PRESENT SIZE OF A
C
C     OUTPUT ...
C        K   STATUS FOR SYMMETRY
C        K = 0, EXACT SYMMETRY, A(I,J)/A(J,I)) = 1
C        K = 1, SYMMETRY TO A RELATIVE ERROR
C                  ABS(1-A(I,J)/A(J,I)) = OR LESS THAN 1.E-7
C        K = 2, NO SYMMETRY
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT SYMV.
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
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*)
      REAL             ACURCY, DGTOL
C
C     ==================================================================
C
      K     = IZERO
      NN    = N - IONE
      DGTOL = RSD - RTWO + RHALF
      DO 20 I=1,NN
        JBEG = I + IONE
        JX   = (I + IONE) * NROW
        DO 10 J=JBEG,N
          JI = J + JX
          IJ = I + (J - IONE) * NROW
          CALL ACCDIG (A(JI),A(IJ),DGTOL,ACURCY,IND)
          IF (ACURCY.GE.RSD) GO TO 10
          K = IONE
          IF (IND.EQ.IZERO) GO TO 10
          K = ITWO
          RETURN
  10    CONTINUE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
*SYMX
      SUBROUTINE SYMX (A,NROW,N,K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   SYMX V 7.00  5/21/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     INPUT ...
C        A      FIRST ELEMENT OF MATRIX A
C        NROW   NUMBER OF ROWS IN A AS DEFINED IN A DIMENSION STATEMENT
C        N      PRESENT SIZE OF A
C
C     OUTPUT ...
C        K   STATUS FOR SYMMETRY
C        K = 0, EXACT SYMMETRY, A(I,J)/A(J,I)) = 1
C        K = 1, SYMMETRY TO A RELATIVE ERROR
C                  ABS(1-A(I,J)/A(J,I)) = OR LESS THAN 1.E-7
C        K = 2, NO SYMMETRY
C
C     THIS PROGRAM UNIT IS A COPY OF PROGRAM UNIT SYMV.
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
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(*)
      REAL             ACURCY, DGTOL
C
C     ==================================================================
C
      K     = IZERO
      NN    = N - IONE
      DGTOL = RSD - RTWO + RHALF
      DO 20 I=1,NN
        JBEG = I + IONE
        JX = (I -IONE) * NROW
        DO 10 J=JBEG,N
          JI = JX + J
          IJ = I + (J -IONE) * NROW
          CALL ACCDIG (A(JI),A(IJ),DGTOL,ACURCY,IND)
          IF (ACURCY.GE.RSD) GO TO 10
          K = IONE
          IF (IND.EQ.IZERO) GO TO 10
          K = ITWO
          RETURN
  10    CONTINUE
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
