*NISTOM   
       PROGRAM NISTOM   
C                   THIS IS THE MAIN PROGRAM OF OMNITAB 80  
C         
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. NISTOM V 7.00  4/21/92. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C          ALL OF THE LABELED COMMON IS INCLUDED IN THIS MAIN PROGRAM IN        
C     ORDER TO SIMPLIFY OVERLAYING.  SINCE THE MAIN PROGRAM IS IN THE 
C     SEGMENT WHICH IS RESIDENT IN CORE AT ALL TIMES, ALL OF THE LABELED        
C     COMMON WILL BE FORCED INTO THE MAIN SEGMENT.
C         
C          FOR SOME COMPUTERS (E.G., BURROUGHS 6700) IT WILL BE       
C     NECESSARY TO ADD THE BLOCK DATA PROCEDURES DATA1, DATA2, DATA3, 
C     DATA4, DATA5, DATA6, DATA7 AND DATA8 TO THIS MAIN PROGRAM.      
C         
C               WRITTEN BY -  
C                      SALLY T. PEAVY,  
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD 20899     
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - DECEMBER, 1989.
C                   CURRENT VERSION -    APRIL, 1992.        
C         
C     ==================================================================        
C         
C                    ***   LABELED COMMON STATEMENTS   ***  
C         
C     CHARACTER SET USED BY OMNITAB.    
C         
      COMMON /ABCDEF/ LA(74)  
C         
C     VARIABLES SPECIALLY RELATED TO INSTRUCTION ARGUMENTS. 
C         
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS         
C         
C     ARRAYS USED IN TABLE LOOKUP PROCEDURE TO DETERMINE COMMAND AND  
C        VALUE OF L1 AND L2.  
C         
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD     
      COMMON /ARRAYB/ IALPH(6), ICL(10,2), ICP(6), ID(8,2) 
      COMMON /ARRYBC/ ICOLHD(7)
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)        
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)   
C         
C     VARIABLES USED BY CALCOMP PROGRAM UNITS.    
C         
      COMMON /CCARGS/ HGT, XDH, IFG, ISPD, NPER, NTPE, NCTP, NCNT(2),NRL        
C         
C     COMMON TRANSCENDENTAL CONSTANTS.  
C         
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD     
C         
C     ..................................................................        
C         
C                              DOUBLE PRECISION   
C         
C     FREQUENTLY USED DOUBLE PRECISION CONSTANTS. 
C         
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO         
C         
C     DOUBLE PRECISION MACHINE CONSTANTS.         
C         
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP 
C         
C     DOUBLE PRECISION FUNCTIONS OF PI. 
C         
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP 
C         
      COMMON /DTCONS/ DALOG2, DEULER    
C         
C     ..................................................................        
C         
C     VARIABLES USED BY PROGRAM UNITS WHICH PRINT ERROR MESSAGES.     
C         
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD        
C         
C     VARIABLES USED FOR FILE COMMAND.  
C         
      COMMON /FILE  / IFILE, ISFILE, NUNIT(10)    
C         
C     VARIABLES USED FOR FORMATS.       
C         
      COMMON /FRMATS/ INUM, IOSWT, LFMT(100)      
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
C         
      COMMON /HEADER/ LNCNT, NPAGE
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
C         
C     FREQUENTLY USED INTEGER CONSTANTS.
C         
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO        
C         
C     INTEGER MACHINE CONSTANTS.        
C         
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB   
C         
C     I/O LOGICAL UNITS.      
C         
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT        
      COMMON /IOUNIT/ LPTAPE  
C         
C     FOR MULTILINGUAL CAPABILITY.      
C         
      COMMON /LANGUE/ LANGC, LANGP      
C         
C     ACTUAL LENGTH OF ARRAYS IN BLOCKS ARRAYA, ARRAYB, ARRAYC, ARRAYD.         
C         
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP   
C         
C     VARIABLES NEEDED TO DEFINE THE SIZE OF THE WORKSHEET.
C
      COMMON /NRCOL/ IROW, ICOL
C
C     VARIABLES USED FOR FUNDAMENTAL PHYSICAL CONSTANTS.    
C         
      COMMON /PCONST/ PC(40), JPC, NT(40)         
C         
C     VALUES OF LOGICAL UNITS FOR PERIPHERAL UNITS.         
C         
      COMMON /PERIPH/ LURCD, NBLKPR, NCHPR        
C         
C     VARIABLES NEEDED FOR STORING COLUMN HEADINGS, LABELS AND FORMATS.         
C         
      COMMON /PRHEAD/ IHEAD(6,50), NHEADS         
C         
C     PRINTING CONSTANTS.     
C         
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE         
C         
C     SWITCH NEEDED IN OVERLAY OF XSEG06.         
C         
      COMMON /OVRLAY/ JREPST  
C         
C     FREQUENTLY USED REAL CONSTANTS.   
C         
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
C         
C     VARIABLES USED FOR READING DATA.  
C         
      COMMON /REDSET/ IFLAG, ISRFLG, JY, NDROW, NNARG       
C         
C     VARIABLES USED FOR REPEAT MODE.   
C         
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH         
C         
C     REAL MACHINE CONSTANTS. 
C         
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG         
C         
C     VARIABLES USED IN SCANNING CARD.  
C         
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE         
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
C         
C     LABELED COMMON USED BY STEM-AND-LEAF PROGRAM UNITS.   
C         
      COMMON /SLCONS/ MXLIN, MXWDTH     
      COMMON /SLEAFA/ TEST(3), IDTHST, ILEAF, IPET, ISIGNF, IOUT      
      COMMON /SLEAFB/ JLSWT, JZ, KZ, LUPPER, LZ, NZ         
      COMMON /SLEAFC/ IJ, IM, INN, INNI, IPERJ, IPRT, IQN, IRS, ISWT, IW        
      COMMON /SLEAFD/ IXN, JPERST, JSPA, KC, KDPTH, LMAXA, LRND, MSAL 
      COMMON /SLEAFE/ NDIV, NSAL, NSALST, NSALTP, NSP, NSPP, NSTOP    
      COMMON /SLICHR/ IB(40), IC(6)
      COMMON /SLIVAR/ IN, IP, IPER, IZ, JSTRT, NDSL, NWSL

      COMMON /SLRVAR/ RI(5), SV(6)      
C         
C     VARIABLES USED FOR STORING INSTRUCTIONS IN THE REPEAT MODE.     
C         
      COMMON /STRINS/ IOVFL, IRMV, LCOM, NCOM, NSTMT, NSTMTX
C         
C     VARIABLES USED AS SWITCHES.       
C         
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM       
C         
C     VARIABLES USED BY THE TEKTRONIX COMMANDS.   
C         
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH    
C         
C     VARIABLES USED IN PRINTING.       
C        IPRINT, LENGTH AND LWIDE ARE INITIALLY SET BY XOMNIT,        
C        ISIGD AND NCWDTH ARE SET BY SETUP, AND   
C        LHEAD(.) IS SET BY HEADS.      
C         
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /TPRNTC/ LHEAD(96)
C         
C     MISCELLANEOUS DIMENSIONED VECTORS.
C         
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
      COMMON /VECCHR/ NTPR(120)
C         
      INCLUDE 'WRKSCR.H'
C         
C     ==================================================================        
C         
C                        ***   TYPE STATEMENTS   ***        
C         
C     REAL             ARG, ARG2        
C     REAL             COM(2000)        
C     REAL             ARGTAB(100), VWXYZ(8)      
C     REAL             RSUM(172)        
C     REAL             HGT, XDH         
C     REAL             DEG, E, HALFPI, PI, RAD    
C     REAL             RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG        
C     REAL             RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO,RZERO        
C     REAL             SV(6), RI(5)     
C     REAL             A(13500), RC(12500), ARGS(100)       
C         
C     ..................................................................        
C         
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO        
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DALOG2, DEULER   
C         
C     ..................................................................        
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER LA*1
      CHARACTER ICOLHD*1
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
      CHARACTER NEWCRD*1
      CHARACTER IB*1, IC*1
      CHARACTER NTPR*1
      CHARACTER LHEAD*1
C
C  MODIFIED JULY 1992.  ADD NEW COMMON FOR GRAPHICS.
C
      EXTERNAL XEND
      COMMON/VGCALC/IBACK,IFORE,AXSIZE,AYSIZE,IMAXCL
      CHARACTER*80 CPOST,CHPGL,CHPGL2,CQMS,CTEKT
      COMMON/VGNAME/CPOST,CHPGL,CHPGL2,CQMS,CTEKT
      COMMON/VGFLAG/IPOST,IHPGL,IHPGL2,IQMS,ITEKT,IX11
      INTEGER X11FLG, IDSPLY(80)
      COMMON/VGX11/IDSPLY
      COMMON/VGX112/X11FLG
C         
C     ==================================================================        
C         
C                       ***   CALL MAIN SUBPROGRAM   ***    
C         
      CALL MESSGE
      CALL OMNIT    
C  MODIFIED JULY 1992.  CLOSE X11 IF CALLED.
      IF(IX11.EQ.1 .AND. X11FLG.EQ.'ON') CALL XEND
      STOP
C         
C     ==================================================================        
C         
      END 
