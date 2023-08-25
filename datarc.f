*DATARC   
      BLOCK DATA DATARC
C
C **  NBS OMNITAB 1980 VERSION 7.00  9/20/90. DATARC V 7.00  9/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SET CONSTANTS WHICH MAY HAVE TO BE CHANGED BECAUSE ...
C         SIZE OF WORKSHEET AND SCRATCH AREA IS CHANGED.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                      GAITHERSBURG MD,  20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1990
C                   CURRENT VERSION - SEPTEMBER, 1990.
C
C     ==================================================================
C         
C                   ***     SPECIFICATION STATEMENTS   ***
C
      INCLUDE 'WRKSCR.H' 
      COMMON /NRCOL/ IROW, ICOL
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C           THE FOLLOWING TWO DATA STATEMENTS DEFINE THE SIZE OF THE
C              WORKSHEET AND SCRATCH AREA.
C                 NRC = SIZE OF WORKSHEET   RC(.)
C                 NS  = SIZE OF SCRATCH AREA A(.)
C
C                 NS MUST EQUAL NRC PLUS 1000
C
CCCCC DATA NRC / 12500 /
CCCCC DATA NS  / 13500 /
      DATA NRC / 250000 /
      DATA NS  / 251000 /
C
C     ...................................................................
C
C           THE FOLLOWING TWO DATA STATEMENTS DEFINE THE MAXIMUM NUMBER
C              OF COLUMNS AND ROWS IN THE WORKSHEET.
C           THE PRODUCT OF NROW TIMES NCOL MUST BE LESS THAN OR
C              EQUAL TO NRC.
C
CCCCC DATA ICOL / 62/
CCCCC DATA IROW /201/
      DATA ICOL /100/
      DATA IROW /2500/
C 
C     ==================================================================
C
      END
