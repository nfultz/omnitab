#  Makefile for OMNITAB
#  March 28, 1991
#
#  To create an executable OMNITAB file, execute the command:
#  make omnitab
#
#  This is a makefile for generating the OMNITAB executable
#  file.  It is setup for the Sun, but read the comments to see 
#  if any modifications need to be made.
#  If the object files have not been deleted, then upon executing
#  make omnitab, only the source files that were modified since the    
#  last compilation will be recompiled.
# 
#  On some Sun's a floating point option may be required. If so,
#  set FLOAT_OPTION to appropriate value.  Otherwise, leave as
#  a null string.  The name of the floating point library (SUN_LIB)
#  may be needed to be specified.
#
#FLOAT_OPTION = -f68881
#SUN_LIB =/usr/lib/f68881/libm.il
#
#FLOAT_OPTION =
#SUN_LIB = 
#
#FFLAGS = $(FLOAT_OPTION) -Nx500
#FFLAGS = "-O0 -stackvar"
FFLAGS = 
#MATH_LIB = -lm
LDFLAGS = -L/usr/openwin/lib -L/itl/apps/spro/SunOS-sun4u-5.5.1/SUNWspro/SC4.2/lib -R/usr/openwin/lib -Bstatic -lM77 -lF77 -Bdynamic -lX11 -lc -Bstatic -lsunmath -lm
#
#  The source files are all in the Fortran language.  Therefore, a 
#  Fortran compiler is needed to create the object files.  If the
#  name of your Fortran compiler is other than f77, please enter  
#  the proper name after the = sign in the next command.
#
F77 = f77

OMNITABSYM = symac.o symde.o symfl.o symmr.o syms.o symtz.o
OMNITABWRK = wrkac.o wrkde.o wrkfl.o wrkmr.o wrks.o wrktz.o datarc.o
CALCOMP = calcmp.o agraf_tek.o 
X11OBJ = x11src.o
#
#  If you have X11 installed, activate the following lines.
CC = cc
#CFLAGS = -c -Xc -I/usr/openwin/include  $(FLOAT_OPTION)
set CFLAGS = "-c -O -I/usr/openwin/include"
X11LIB = -lX11
X11SRC = x11src.c
X11COMP = $(CC) $(CFLAGS) 
#
#  Activate the following lines if X11 is not installed on your
#  system.
#  X11LIB =
#  X11SRC = x11src.f
#  X11COMP = $(FF) -c $(FFLAGS) 

#  Omnitab has a TEKTRONIX PLOT and a CALCOMP PLOT command.  The 
#  TEKTRONIX PLOT command will utilitze an OMNITAB provided set of
#  Calcomp compatibility routines.  These routines support Tektronix
#  4014 (either to the screen or to a file), X11 (requires your site
#  have X11 installed, see above note), Postscript, HP-GL, HP-GL 
#  modified to support the LaserJet III,and the QMS (QUIC protocol).
#  These routines are in the files calcmp.f, agraf_tek.f, and x11src.c.
#  The file x11src.f contains dummy routines for those sites that 
#  do not support X11.  In addition, some sites may have a local 
#  device that has its own Calcomp library (many penplotters use 
#  this).  In order to allow for this, the Omnitab provided routines
#  have slightly different names to allow both the Omnitab and the
#  local Calcomp library to be used.  The file caldum.f provides
#  dummy routines for those sites not having a local Calcomp library.
#
#  If you have a local Calcomp library, activate the following lines.
#  CALLIB = -lcalcomp   # Put in the proper name for your system.
#  CALOBJ = 
#
#  Activate the following lines if your site has no local Calcomp 
#  library.
CALLIB =
CALOBJ = caldum.o


omnitab: nistom.f $(OMNITABSYM) $(OMNITABWRK) $(CALCOMP) $(X11OBJ) $(CALOBJ)
	$(F77) -nolib -o omnitab $(FFLAGS) nistom.f $(OMNITABSYM) $(OMNITABWRK) \
	       $(CALCOMP) $(X11OBJ) $(CALOBJ) $(LDFLAGS)

wrkac.o : wrkac.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrkac.f

wrkde.o : wrkde.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrkde.f

wrkfl.o : wrkfl.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrkfl.f

wrkmr.o : wrkmr.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrkmr.f

wrks.o : wrks.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrks.f

wrktz.o : wrktz.f WRKSCR.H
	$(F77) -c $(FFLAGS) wrktz.f

datarc.o : datarc.f WRKSCR.H
	$(F77) -c $(FFLAGS) datarc.f

calcmp.o : calcmp.f 
	$(F77) -c $(FFLAGS) calcmp.f

agraf_tek.o : agraf_tek.f 
	$(F77) -c $(FFLAGS) agraf_tek.f

x11src.o : $(X11SRC)
	$(X11COMP) $(X11SRC)

caldum.o : caldum.f 
	$(F77) -c $(FFLAGS) caldum.f

