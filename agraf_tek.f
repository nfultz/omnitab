c  AGRAF.F  - Plotting service routines for AGRAF		     							
c  This file contains the following FORTRAN functions: 	 	     
c    agraf0   - Sets screen mode to graphics or text.	   
c    agraf4   - Print screen (nonoperable)		
c    agraff   - Plot lines, characters.
c    agrafg   - Set screen dimensions
c    agrafi   - Erase screen
c								     
      subroutine agraf0(ierr,imode)
      integer imode
      character*5 string
      common /gflag/ grafflg

      if(imode.eq.0) then
        string(1:1)= char(13)
        string(2:2)= char(27)
        string(3:3)= char(12)
        string(4:4)= char(29)
        write(*,100)string(1:4)
	grafflg = 1
      else if (imode.eq.1) then
        string(1:1)= char(13)
        string(2:2)= char(27)
        string(3:3)= char(12)
        write(*,100)string(1:3)
      endif	
      return
100   format(1x,A)
      end	

      subroutine agraf4(ier)
      integer ier
   
c    ier = hdcopy()
c    ier = system("/usr/local/qplot")
c    return
      return
      end

      subroutine agraff(iretcd,iptype,n,ix,iy)
      integer iretcd,iptype,n,ix(*),iy(*),clrflg
      integer i, charty, color, bitop, lintyp,bracln,y,j,y1
      character*255 string
      data clrflg /0/
      save clrflg
     
      charty = iptype/256
      color = (iptype - (256*charty))/16
      bitop = (iptype - (256*charty)-(16*color))/8
      lintyp = (iptype - (256*charty)-(16*color) -(8*bitop))
      lintyp = mod(lintyp,6)
      color = mod(color,15)
      if(n .gt. 1 .and. lintyp .gt. 0) then
         string(1:1) = char(29)
         y = 750-iy(1)
	 string(2:2) = char(32+mod(y/32,32))
	 string(3:3) =char(96+mod(y,32))
	 string(4:4) = char(32+mod(ix(1)/32,32))
	 string(5:5) = char(64+mod(ix(1),32))
         j=5
         do 10 i=1,n-1
             y = 750-iy(i+1)
             j = j+1
             if(j.ge.74) then
                string(j:j)=char(31)
                write(*,100)string(1:j)
                string(1:1)=char(29)
                y1=750-iy(i)
	        string(2:2) = char(32+mod(y1/32,32))
	        string(3:3) = char(96+mod(y1,32))
	        string(4:4) = char(32+mod(ix(i)/32,32))
	        string(5:5) = char(64+mod(ix(i),32))
                j=6
             endif
	     string(j:j) = char(32+mod(y/32,32))
             j=j+1
	     string(j:j) =char(96+mod(y,32))
             j=j+1
	     string(j:j) = char(32+mod(ix(i+1)/32,32))
             j=j+1
	     string(j:j) = char(64+mod(ix(i+1),32))
10        continue
          j = j+1
          string(j:j) = char(31)
	  write (*,100) string(1:j)
      else if (charty.eq.191.or.charty.eq.192.or.charty.eq.217
     c               .or.charty.eq.218) then
	 bracln = .02*1000
         if (charty .eq. 191) then
             string(1:1) = char(29)
             y = 750-iy(1)
	     string(2:2) = char(32+mod(y/32,32))
	     string(3:3) =char(96+mod(y,32))
	     string(4:4) = char(32+mod((ix(1)-bracln)/32,32))
	     string(5:5) = char(64+mod((ix(1)-bracln),32))
	     string(6:6) = char(32+mod(y/32,32))
	     string(7:7) =char(96+mod(y,32))
	     string(8:8) = char(32+mod(ix(1)/32,32))
	     string(9:9) = char(64+mod(ix(1),32))
	     string(10:10) = char(32+mod((y-bracln)/32,32))
	     string(11:11) =char(96+mod((y-bracln),32))
	     string(12:12) = char(32+mod(ix(1)/32,32))
	     string(13:13) = char(64+mod(ix(1),32))
             string(14:14) = char(31)
             write(*,100) string(1:14)
         endif
	 if (charty .eq. 192) then
             string(1:1) = char(29)
             y = 750-iy(1)
             string(2:2) = char(32+mod((y+bracln)/32,32))
	     string(3:3) =char(96+mod((y+bracln),32))
	     string(4:4) = char(32+mod(ix(1)/32,32))
	     string(5:5) = char(64+mod(ix(1),32))
	     string(6:6) = char(32+mod(y/32,32))
	     string(7:7) =char(96+mod(y,32))
	     string(8:8) = char(32+mod(ix(1)/32,32))
	     string(9:9) = char(64+mod(ix(1),32))
	     string(10:10) = char(32+mod(y/32,32))
	     string(11:11) =char(96+mod(y,32))
	     string(12:12) = char(32+mod((ix(1)+bracln)/32,32))
	     string(13:13) = char(64+mod((ix(1)+bracln),32))
             string(14:14) = char(31)
             write(*,100) string(1:14)
         endif
	 if (charty .eq. 217) then
             string(1:1) = char(29)
             y = 750-iy(1)
	     string(2:2) = char(32+mod(y/32,32))
	     string(3:3) =char(96+mod(y,32))
	     string(4:4) = char(32+mod((ix(1)-bracln)/32,32))
	     string(5:5) = char(64+mod((ix(1)-bracln),32))
	     string(6:6) = char(32+mod(y/32,32))
	     string(7:7) =char(96+mod(y,32))
	     string(8:8) = char(32+mod(ix(1)/32,32))
	     string(9:9) = char(64+mod(ix(1),32))
	     string(10:10) = char(32+mod((y+bracln)/32,32))
	     string(11:11) =char(96+mod((y+bracln),32))
	     string(12:12) = char(32+mod(ix(1)/32,32))
	     string(13:13) = char(64+mod(ix(1),32))
             string(14:14) = char(31)
             write(*,100) string(1:14)
           endif
       if (charty .eq. 218) then
             string(1:1) = char(29)
             y = 750-iy(1)
	     string(2:2) = char(32+mod((y-bracln)/32,32))
	     string(3:3) =char(96+mod((y-bracln),32))
	     string(4:4) = char(32+mod(ix(1)/32,32))
	     string(5:5) = char(64+mod(ix(1),32))
	     string(6:6) = char(32+mod(y/32,32))
	     string(7:7) =char(96+mod(y,32))
	     string(8:8) = char(32+mod(ix(1)/32,32))
	     string(9:9) = char(64+mod(ix(1),32))
	     string(10:10) = char(32+mod(y/32,32))
	     string(11:11) =char(96+mod(y,32))
	     string(12:12) = char(32+mod((ix(1)+bracln)/32,32))
	     string(13:13) = char(64+mod((ix(1)+bracln),32))
             string(14:14) = char(31)
             write(*,100) string(1:14)
           endif
      else
       if(charty.eq.68.and.iy(1).eq.12.and.ix(1).eq.12) then
          clrflg = 1
       endif
       if(charty.ne.68.and.iy(1).eq.12.and.ix(1).eq.12) then
          clrflg = 0
       endif
       if(iy(1).ne.12) then
          clrflg = 0
       endif
       if(clrflg.eq.0) then
          string(1:1) = char(29)
          y = 750-iy(1)
	  string(2:2) = char(32+mod(y/32,32))
	  string(3:3) =char(96+mod(y,32))
	  string(4:4) = char(32+mod(ix(1)/32,32))
	  string(5:5) = char(64+mod(ix(1),32))
          string(6:6) = char(31)
          string(7:7) = char(charty)
          string(8:8) = char(10)
          write(*,100) string(1:8)
       endif
      endif
      return
100   format(1x,A)
      end

      subroutine agrafg(iretcd,idmode,igcol,igrow,itcol,itrow,
     c  idpage,idmaxp)
      integer iretcd,idmode,igcol,igrow,itcol,itrow,idpage,idmaxp
      common /gflag/ grafflg

      idmode=1-grafflg
      itcol = 80
      itrow = 30
      idpage = 1
      idmaxp = 1
      if (grafflg .eq. 1) then
           igcol=1000 
           igrow=750
      else
           igcol = 80
           igrow = 30
      endif
      return
      end

      subroutine agrafi(iretcd,istore,iwtype,ix1,iy1,iw,ih,array)
      integer iretcd,istore,iwtype,ix1,iy1,iw,ih
      character array(*)

      if(iwtype .eq. 768) then
         continue
      endif
      return
      end
