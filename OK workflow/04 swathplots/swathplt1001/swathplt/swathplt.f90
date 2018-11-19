!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                                                                      %
! Copyright (C) 2003, Statios Software and Services Incorporated.  All %
! rights reserved.                                                     %
!                                                                      %
! This program has been modified from the one distributed in 1996 (see %
! below).  This version is also distributed in the hope that it will   %
! be useful, but WITHOUT ANY WARRANTY. Compiled programs based on this %
! code may be redistributed without restriction; however, this code is %
! for one developer only. Each developer or user of this source code   %
! must purchase a separate copy from Statios.                          %
!                                                                      %
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                                                                      %
! Copyright (C) 1996, The Board of Trustees of the Leland Stanford     %
! Junior University.  All rights reserved.                             %
!                                                                      %
! The programs in GSLIB are distributed in the hope that they will be  %
! useful, but WITHOUT ANY WARRANTY.  No author or distributor accepts  %
! responsibility to anyone for the consequences of using them or for   %
! whether they serve any particular purpose or work at all, unless he  %
! says so in writing.  Everyone is granted permission to copy, modify  %
! and redistribute the programs in GSLIB, but only under the condition %
! that this notice and the above copyright notice remain intact.       %
!                                                                      %
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       program main
!-----------------------------------------------------------------------
!
!                  Swath Plot
!                  **********
!
! INPUT/OUTPUT Parameters:
!
!   datafl           the input data file
!   icx,icy,i3       columns for X, Y, third variable
!   tmin,tmax        trimming limits
!   outfl            the output PostScript file
!   pminx,pmaxx      plotting limits on X variable
!   pminy,pmaxy      plotting limits on Y variable
!   bsize            relative size of bullet
!   title            title
!
!
!
! PROGRAM NOTES:
!
! 1. The program is executed with no command line arguments.  The user
!    will be prompted for the name of a parameter file.  The parameter 
!    file is described in the documentation (see example scatplt.par)
!
! 2. The calculation of the rank correlation coefficient does not
!    handle spikes correctly, nor does it account for declustering
!    weights.
!
!
!
! AUTHOR: Clayton V. Deutsch                             DATE: 1989-1999
! Modified by Jeff Boisver 2008
!-----------------------------------------------------------------------
      use        msflib
      parameter (MV=500, EPSLON=1.0e-21, VERSION=1.000, DEG2RAD=3.141592654/180.0)
      integer    test,nfiles,cnt,xstats,ystats,stats,ilegend,plot_hist,ierr
      character  outfl*512,title*2000,str*512,xlab*24,ylab*24, &
                 str1*50,str2*50,lfmt*8,dash(10)*24
      real       var(MV),xloc(2),yloc(2),redint(24),grnint(24),bluint(24),plotx(500),ploty(500),sposx,sposy
      real       v1min,v1max,v1minh,v1maxh,lqnt,uqnt
      real*8     xxmin,xxmax,yymin,yymax,zzmin,zzmax
      logical    testfl
                

      integer, allocatable      :: ivx(:),ivy(:),ivz(:),ivv(:),nx(:),ny(:),nz(:),gridded(:),bincnt(:)
      character*512,allocatable :: datafl(:),l_text(:)     
      real, allocatable         :: vrv(:), vrp(:), tmin(:), tmax(:), lwid(:),ldash(:),pts(:),color(:),xxx(:),yyy(:),correlation(:),xtmp(:),ytmp(:)
      real, allocatable         :: xmn(:),ymn(:),zmn(:),xsiz(:),ysiz(:),zsiz(:),binval(:),binmid(:),binquant(:,:)
      real*8, allocatable       :: r_coef(:)
     
!
      common /psdata/ lpsout,pscl,pxmin,pxmax,pymin,pymax,xmin, &
                     xmax,ymin,ymax
!
      data lin/1/,lpsout/2/,pscl/0.24/,pxmin/0.0/,pxmax/288.0/ &
          pymin/0.0/,pymax/216.0/,xmin/-10.0/,xmax/83.3/, &
          ymin/-10.0/,ymax/60.0/,hpxmin/-15.0/,hpxmax/95.0/, &
          hpymin/0.0/,hpymax/58.0/
          
       data     redint/1.0000,1.0000,1.0000,0.4980,0.0   ,0.0,   0.0, &
                     1.0000,1.0000,   0.0,0.4980,0.6667,1.0000,0.0, &
                     0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000, &
                     0.7000,0.8000,0.9000/, &
              grnint/0.0,0.4980,1.0000,1.0000,1.0000,1.0000,0.0, &
                     0.0,1.0000,0.0   ,0.0   ,0.3333,0.3333,1.0000, &
                     0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000, &
                     0.7000,0.8000,0.9000/, &
              bluint/0.0   ,0.0   ,0.0   ,0.0   ,0.0   ,1.0000,1.0000, &
                     1.0000,1.0000,0.0   ,1.0000,0.0   ,0.6667,0.4980, &
                     0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000, &
                     0.7000,0.8000,0.9000/
! Dashing
       data dash/'[40 20] 0 setdash       ', &
              '[13 14 13 20] 0 setdash ', &
              '[12 21 4 21] 0 setdash  ', &
              '[10 10] 0 setdash       ', &
              '[20 20] 0 setdash       ', &
              '[30 30] 0 setdash       ', &
              '[40 40] 0 setdash       ', &
              '[ 3  3] 0 setdash       ', &
              '[ 5  5] 0 setdash       ', &
              '[ 8  8] 0 setdash       '/
!
! Note VERSION number:
!
      write(*,9999) VERSION
 9999 format(/' SWATHPLT Version: ',f5.3/)
!
! Get the name of the parameter file - try the default name if no input:
!
      do i=1,512
            str(i:i) = ' '
      end do
      call getarg(1,str)
      if(str(1:1).eq.' ')then
            write(*,*) 'Which parameter file do you want to use? (type "-h" to get help)'
            read (*,'(a)') str
      end if
      
     if(str(1:5).eq.'-h') then
        call help()
        stop    
      end if

      if(str(1:1).eq.' ') str(1:20) = 'swathplt.par         '
      inquire(file=str,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the parameter file does not exist,'
            write(*,*) '        check for the file and try again  '
            write(*,*)
            if(str(1:20).eq.'swathplt.par         ') then
                  write(*,*) '        creating a blank parameter file'
                  call makepar
                  write(*,*)
            end if
            stop
      endif
      open(lin,file=str,status='OLD')
!
! Find Start of Parameters:
!
 1    read(lin,'(a4)',end=97) str(1:4)
      if(str(1:4).ne.'STAR') go to 1
!
! Read Input Parameters:
!
      read(lin,*,err=97) nfiles
      write(*,*) ' number of input files= ',nfiles
      allocate(datafl(nfiles))
      allocate(ivx(nfiles)) ;allocate(ivy(nfiles))  ;allocate(ivz(nfiles)) ;allocate(ivv(nfiles))
      allocate(tmin(nfiles)) ;allocate(tmax(nfiles)) 
      allocate(lwid(nfiles)) ;allocate(ldash(nfiles)) ;allocate(pts(nfiles)) ;allocate(color(nfiles)) ;
      allocate(nx(nfiles),ny(nfiles),nz(nfiles),xmn(nfiles),ymn(nfiles),zmn(nfiles),xsiz(nfiles),ysiz(nfiles),zsiz(nfiles))
      allocate(gridded(nfiles))
      allocate(l_text(nfiles))
      allocate (correlation(nfiles))
      
      
      
    do i=1,nfiles
      read(lin,'(a512)',err=97) datafl(i)
      call chknam(datafl(i),512)
      write(*,*) ' data file = ',datafl(i)(1:40)
      
      read(lin,*,err=97) gridded(i)
      write(*,*) ' gridded? = ',gridded(i)

      if (gridded(i) == 1) then
          read(lin,*,err=97) ivv(i)
          write(*,*) ' column = ',ivv(i)
          read(lin,*,err=97) nx(i),xmn(i),xsiz(i)
          write(*,*) ' x grid = ',nx(i),xmn(i),xsiz(i)
          read(lin,*,err=97) ny(i),ymn(i),ysiz(i)
          write(*,*) ' y grid = ',ny(i),ymn(i),ysiz(i)
          read(lin,*,err=97) nz(i),zmn(i),zsiz(i)
          write(*,*) ' z grid = ',nz(i),zmn(i),zsiz(i)
      else
          read(lin,*,err=97) ivx(i),ivy(i),ivz(i),ivv(i)
          write(*,*) ' columns = ',ivx(i),ivy(i),ivz(i),ivv(i)
      end if
      
      read(lin,*,err=97) tmin(i),tmax(i)
      write(*,*) ' trimming limits = ',tmin(i),tmax(i)
      
      read(lin,*,err=97) lwid(i),ldash(i),pts(i),color(i)
      write(*,*) ' plotting parameters = ', lwid(i),ldash(i),pts(i),color(i)
      
    end do

      read(lin,'(a512)',err=97) outfl
      call chknam(outfl,512)
      write(*,*) ' output file = ',outfl(1:40)
      
      read(lin,*,err=97) azimuth, dip
      write(*,*) ' azimuth / dip = ',azimuth, dip
      azimuth = azimuth * DEG2RAD
      dip     = dip     * DEG2RAD
      
      read(lin,*,err=97) nbin
      write(*,*) ' number of bins = ',nbin
      allocate(binval(nbin),binmid(nbin),bincnt(nbin),binquant(nbin,2))

      read(lin,*,err=97) xscmin,xscmax
      write(*,*) ' X plotting limits = ',xscmin,xscmax

      read(lin,*,err=97) yscmin,yscmax,ilogy
      write(*,*) ' Y plotting limits = ',yscmin,yscmax,ilogy

      read(lin,*,err=97) plot_hist
      write(*,*) ' plot histogram = ',plot_hist

      read(lin,'(a2000)',err=97) title
      call chktitle(title,2000)
      write(*,*) ' title = ',title(1:40)
    
      read(lin,'(a24)',err=97) xlab
      call chktitle(xlab,24)
      write(*,*) ' xlab = ',xlab
      
      read(lin,'(a24)',err=97) ylab
      call chktitle(ylab,24)
      write(*,*) ' ylab = ',ylab
      
      read(lin,*,err=97) ierr,lqnt,uqnt
      write(*,*) ' draw error bars = ',ierr
      if(ierr == 1) write(*,*) ' quantiles = ',lqnt,uqnt
    
 
 !find the legend block
      rewind(lin)
      ilegend=0
      do
          read(lin,'(a)',end=234) str
          if(str(1:29)=='BLOCK FOR USER DEFINED LEGEND') exit
      end do
      
      read(lin,*,err=234) ilegend,sposx,sposy
      if(sposx<-1.5)  sposx=-1.5
      if(sposx>1.5)   sposx=1.5
      if(sposy<-1.5)  sposy=-1.5
      if(sposy>1.5)   sposy=1.5
          
      if(ilegend==1) then

        do i=1,nfiles
          read(lin,'(a512)',err=234) l_text(i)
          call chktitle(l_text(i),512)
        end do
           
      end if
234   close(lin)
      
!check each input file and load the data
!
! Check to make sure the data file exists, then either write an error
! message and stop, or read in as much data as possible:
!
      inquire(file=datafl,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the data file does not exist,'
            write(*,*) '        check for the file and try again  '
            stop
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Read data for the first file:
!
      open(lin,file=datafl,status='OLD')
      read(lin,*,err=99)
      read(lin,*,err=99)     nvari
      do i=1,nvari
            read(lin,*,err=99)
      end do
      
      if(gridded(1) == 1) then
        maxdat = nx(1) * ny(1) * nz(1)
      else
        maxdat = 0
 20     read(lin,*,end=30,err=99)
        maxdat = maxdat + 1
        go to 20
 30     continue
      end if
!       
! Allocate the needed memory.
!
      allocate (xtmp(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
      allocate (ytmp(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!     
      allocate (vrv(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!     
      allocate (vrp(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
           
!
      allocate (yyy(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
      allocate (xxx(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
! Now, read the data into the arrays:
!
      rewind(lin)
      write(*,*); write(*,*) ' Reading first file'
      read(lin,'(a)',err=99) str
      read(lin,*,err=99)     nvari
      do i=1,nvari
            read(lin,'(a24)',err=99) str(1:24)
            if(i.eq.ivx(1) .and. xlab(1:7)=='default') xlab = str(1:24)
            if(i.eq.ivv(1) .and. ylab(1:7)=='default') ylab = str(1:24)
      end do
      nd   = 0
      nt   = 0
      xtwt = 0.0
      v1min = 1.0e21
      v1max =-1.0e21
      v2min = 1.0e21
      v2max =-1.0e21
      xxmax  =-1.0e21
      xxmin  = 1.0e21
      yymax  =-1.0e21
      yymin  = 1.0e21
      zzmax  =-1.0e21
      zzmin  = 1.0e21 
      
      if(gridded(1) == 1) then
!
! Check column number
!      
      if(ivv(1).le.0.or.ivv(1).gt.nvari) then
            write(*,*) ' ERROR: ivr is invalid '
            write(*,*) '        >0 and <number of variables in file'
            stop
      end if      
      
      do k=1,nz(1)
        vrz = (k-1) * zsiz(1) + zmn(1)
      do j=1,ny(1)
        vry = (j-1) * ysiz(1) + ymn(1)
      do i=1,nx(1)
        read(lin,*,end=3,err=99) (var(jj),jj=1,nvari)
!
! Trim this data?
!
        if(var(ivv(1)).lt.tmin(1).or.var(ivv(1)).ge.tmax(1)) cycle
!
! Accept this data:
!
        nd = nd + 1
        vrx = (i-1) * xsiz(1) + xmn(1)
      
        call rotc(vrx,vry,vrz,azimuth,dip,vrp(nd))
        
        write(99,*) vrp(nd)
        
        vrv(nd) = var(ivv(1))
      
        if(vrp(nd).lt.v1min) then
            v1min = vrp(nd)
            xxmin  = min(xxmin,vrx)
            yymin  = min(yymin,vry)
            zzmin  = min(zzmin,vrz)
        end if
        if(vrp(nd).gt.v1max) then
            v1max = vrp(nd)
            xxmax  = max(xxmax,vrx)
            yymax  = max(yymax,vry)
            zzmax  = max(zzmax,vrz)
        end if
        if(vrv(nd).lt.v2min) v2min = vrv(nd)
        if(vrv(nd).gt.v2max) v2max = vrv(nd)
!
! Go back for another data:
!
      end do
      end do
      end do
      
      else
!
! Check column numbers
!      
      if(ivv(1).le.0.or.ivv(1).gt.nvari) then
            write(*,*) ' ERROR: ivr is invalid '
            write(*,*) '        >0 and <number of variables in file'
            stop
      end if      
      
 2    read(lin,*,end=3,err=99) (var(j),j=1,nvari)
!
! Trim this data?
!
      if(var(ivv(1)).lt.tmin(1).or.var(ivv(1)).ge.tmax(1)) then
            nt = nt + 1
            go to 2
      endif
!
! Accept this data:
!
      nd = nd + 1
      vrv(nd) = var(ivv(1))
      if(ivx(1).ge.1) then
            vrx = var(ivx(1))
      else
            vrx = 0.
      endif
      if(ivy(1).ge.1) then
            vry = var(ivy(1))
      else
            vry = 0.
      endif
      if(ivz(1).ge.1) then
            vrz = var(ivz(1))
      else
            vrz = 0.
      endif
      
      call rotc(vrx,vry,vrz,azimuth,dip,vrp(nd))
      
      if(vrp(nd).lt.v1min) then
            v1min = vrp(nd)
            xxmin  = min(xxmin,vrx)
            yymin  = min(yymin,vry)
            zzmin  = min(zzmin,vrz)
            xxmax  = max(xxmax,vrx)
            yymax  = max(yymax,vry)
            zzmax  = max(zzmax,vrz)
      end if
      if(vrp(nd).gt.v1max) then
            v1max = vrp(nd)
            xxmin  = min(xxmin,vrx)
            yymin  = min(yymin,vry)
            zzmin  = min(zzmin,vrz)
            xxmax  = max(xxmax,vrx)
            yymax  = max(yymax,vry)
            zzmax  = max(zzmax,vrz)
      end if
      if(vrv(nd).lt.v2min) v2min = vrv(nd)
      if(vrv(nd).gt.v2max) v2max = vrv(nd)
!
! Go back for another data:
!
      go to 2
      
      end if
      
 3    close(lin)
 
      if(nd.le.1) then
            write(*,*) ' ERROR: too few data ',nd
            stop
      endif
!
! Compute the needed statistics:
!
      vmin  = 1.0e21
      vmax  =-1.0e21
      vrym = 0.0
      do i=1,nd
            vrt2 = vrv(i)
            vmin  = min(vmin,vrt2)
            vmax  = max(vmax,vrt2)
            vrym = vrym + vrt2
      end do
      vrym = vrym / nd
      vryv = 0.0
      do i=1,nd
            vrt2 = vrv(i)
            vryv = vryv + (vrt2-vrym)*(vrt2-vrym)
      end do
!
! Write Some of the Statistics to the screen:
!
      write(*,900) nd,vrym,sqrt(max(vryv,0.0)),ymin,ymax
 900 format(/' There are ',i8,' data with:',/, &
             '   mean value         = ',f12.5,/, &
             '   standard deviation = ',f12.5,/, &
             '   min and max        = ',2f12.5,/) 
!
! Compute average of bins along swath axis
!            
      v1minh = v1min; v1maxh = v1max
      vrp = vrp - v1min 
      v1max = v1max - v1min
      v1min = 0
      binquant = 0
      call binavg(vrp,vrv,maxdat,nd,nbin,v1min,v1max,binmid,binval,bincnt,ierr,lqnt,uqnt,binquant)

!set up output file

!
      if(xscmax.le.xscmin) then
            xscmin = v1min
            xscmax = v1max
      endif
      if(yscmax.le.yscmin) then
            yscmin = vmin
            yscmax = vmax
      endif
!
! Log scaling?
!
      if(ilogy.eq.1) then
            if(yscmin.le.0.0) yscmin = max(v2min,0.00001)
            if(yscmax.le.0.0) yscmax = v2max
            yscmin = real(int(alog10(max(yscmin,EPSLON))-0.9))
            yscmax = real(int(alog10(max(yscmax,EPSLON))+0.9))
      endif
      xrange = hpxmax - hpxmin
      yrange = hpymax - hpymin
!
! Open the output file and add a header:
!
      bsize = 6.0*bsize
      open(lpsout,file=outfl,status='UNKNOWN')
      write(lpsout,998) title(1:20)
 998  format('%!PS-Adobe-3.0                       %    Remove     ', &
         /, '90 234 translate 1.5 1.5 scale       %  these lines  ', &
         /, '                                     % for EPSF file ', &
         /, '%!PS-Adobe-3.0 EPSF-3.0', &
         /, '%%BoundingBox: 0 0 288 216', &
         /, '%%Creator: GSLIB', &
         /, '%%Title:   ',a20, &
         /, '%%CreationDate: ', &
         /, '%%EndComments',/,/,/,'%',/,'%',/,'%',/, &
         /, '/m {moveto} def /l {lineto} def /r {rlineto} def', &
         /, '/s {stroke} def /n {newpath} def /c {closepath} def', &
         /, '/rtext{ dup stringwidth pop -1 div 0 rmoveto show } def', &
         /, '/ctext{ dup stringwidth pop -2 div 0 rmoveto show } def', &
         /, '/ltext{show} def /gr{grestore} def /gs{gsave} def', &
         /, '/tr{translate} def /setc{setrgbcolor} def')
         
      do i=1,nfiles
      if(i<10) then
      write(lpsout,1998) i,pts(i)
 1998  format('/bullet',I1,'{ ',f5.2,' 0 360 arc c fill } def',/,/)
      end if
      
      if(i>=10 .and. i<100) then
      write(lpsout,1997) i,pts(i)
 1997  format('/bullet',I2,'{ ',f5.2,' 0 360 arc c fill } def',/,/)
      end if
      
      if(i>=100) then
      write(lpsout,1996) i,pts(i)
 1996  format('/bullet',I3,'{ ',f5.2,' 0 360 arc c fill } def',/,/)
      end if

      end do
      
      write(lpsout, '(a)') '%72 72 translate'
      write(lpsout, '(a)') '0.240000 0.240000 scale'
!
! Write the title and the labels:
!
      ts   = 7.5
      xloc = hpxmin - 0.15*xrange
      yloc = hpymin + 0.50*yrange
      xloc = hpxmin - 10
      call pstext(xloc,yloc,24,ylab,ts,1,90.0,1)
      xloc = hpxmin + 0.50*xrange
      yloc = hpymin - 0.15*yrange
      call pstext(xloc,yloc,24,xlab,ts,1,0.0,1)
      yloc = hpymax + 0.01*yrange
      call pstext(hpxmin,yloc,2000,title,8.0,3,0.0,0)
!
! Increase the max scaling a little for the labeling of the axes:
!
      xscmax = xscmax + 0.001*(xscmax-xscmin)
      yscmax = yscmax + 0.001*(yscmax-yscmin)
!
! Sort out the scaling for log axis:
!
      xhsmn = xscmin
      xhsmx = xscmax
      if(ilogy.eq.1) then
            yhsmn = 10.0**yscmin
            yhsmx = 10.0**yscmax
      else
            yhsmn = yscmin
            yhsmx = yscmax
      end if
                                    ilog = 0
      if(ilogy.eq.1)                ilog = 2
      
!
! Scale and Draw the scatterplot axes:
!
      call scal(xhsmn,xhsmx,yhsmn,yhsmx,hpxmin,hpxmax,hpymin, &
               hpymax,ilog,0,xxmin,xxmax,yymin,yymax,zzmin,zzmax,azimuth/DEG2RAD,dip/DEG2RAD)

!plot the histograms on the axis
    cnt= 0
    np = 0
    do i=1,nbin
            if(binmid(i).ge.xhsmn.and.binmid(i).lt.xhsmx) then
                  xtmp(i)=binmid(i)
                  ytmp(i)= bincnt(i)
            end if
    end do
    if(plot_hist>0) call draw_histograms(nbin,xscmin,xscmax,yscmin,yscmax,xtmp,ytmp,lpsout,ilogy,plot_hist)
               
               

      write(lpsout,501) 1,lwid(1),redint(color(1)),grnint(color(1)),bluint(color(1))
 501  format('%'/'% Curve #',i3/'%'//''/ &
            ' ',f6.2,' setlinewidth'/ &
            1x,f6.4,1x,f6.4,1x,f6.4,1x,' setrgbcolor')
!
! Locate all the points:
!
      cnt= 0
      np = 0
      do i=1,nbin
            if(binmid(i).ge.xhsmn.and.binmid(i).lt.xhsmx.and. &
              binval(i).ge.yhsmn.and.binval(i).lt.yhsmx) then
                  xval = binmid(i)
                  if(ilogy.eq.1) then
                        yval = alog10(max(binval(i),EPSLON))
                  else
                        yval = binval(i)
                  endif
                  np = np + 1
                  xx = resc(xscmin,xscmax,hpxmin,hpxmax,xval)
                  yy = resc(yscmin,yscmax,hpymin,hpymax,yval)
                  ix = int((resc(xmin,xmax,pxmin,pxmax,xx))/pscl)
                  iy = int((resc(ymin,ymax,pymin,pymax,yy))/pscl)
                  if(pts(1)>0) write(lpsout,101) ix,iy,'1'
 101              format('n ',i5,1x,i5,' bullet',a)
                  cnt=cnt+1
                  xxx(cnt)=real(xx)
                  yyy(cnt)=real(yy)
            end if
      end do
      
      if(lwid(1)>0) call psline(cnt,xxx,yyy,lwid(1)*.24,int(ldash(1)) )
!
! Draw quantile lines:
!
      wid = 5
      if(ierr == 1) then
      do i=1,nbin
            if(binmid(i).ge.xhsmn.and.binmid(i).lt.xhsmx.and. &
              binval(i).ge.yhsmn.and.binval(i).lt.yhsmx) then
                  xval = binmid(i)
                  xtmp(i)=binmid(i)
                  if(ilogy.eq.1) then
                        yval1 = alog10(max(binquant(i,1),EPSLON))
                        yval2 = alog10(max(binquant(i,2),EPSLON))
                  else
                        yval1 = binquant(i,1)
                        yval2 = binquant(i,2)
                  endif
                  np = np + 1
                  xx = resc(xscmin,xscmax,hpxmin,hpxmax,xval)
                  yy1= resc(yscmin,yscmax,hpymin,hpymax,yval1)
                  yy2= resc(yscmin,yscmax,hpymin,hpymax,yval2)
                  ix = int((resc(xmin,xmax,pxmin,pxmax,xx))/pscl)
                  iy1 = int((resc(ymin,ymax,pymin,pymax,yy1))/pscl)
                  iy2 = int((resc(ymin,ymax,pymin,pymax,yy2))/pscl)
                  if(pts(1)>0) write(lpsout,102) ix-5,iy1,ix+5,iy1,ix,iy1,ix,iy2,ix-5,iy2,ix+5,iy2
 102              format('n ',/,i5,1x,i5,' m',/,i5,1x,i5,' l',/,i5,1x,i5,' m',/,i5,1x,i5,' l',/,i5,1x,i5,' m',/,i5,1x,i5,' l',/,'s')
            end if
      end do    
      end if  

!
! Write the Statistics to the Output Device:
!
      write(lpsout,503) 1,2.,redint((10)),grnint((10)),bluint((10))
 503  format('%'/'% Curve #',i3/'%'//' '/ &
            ' ',f6.2,' setlinewidth'/ &
            1x,f6.4,1x,f6.4,1x,f6.4,1x,' setrgbcolor')
            
      x1  = 30+42.0*spos
      x2  = x1+1
      yy  = 55.5
      y1  =  2.5
      y2  =  4.0
      it  =  1
      rt  =  0.0
      ts  =  7.0
            
 504  format(f6.4,1x,f6.4,1x,f6.4,1x,' setrgbcolor')      

!now plot the other data sets
do ii=2,nfiles  

      write(*,*) 'Working on file ',ii
      write(*,*)

      write(lpsout,502) ii,lwid(ii),redint(color(ii)),grnint(color(ii)),bluint(color(ii))
 502  format('%'/'% Curve #',i3/'%'//' '/ &
            ' ',f6.2,' setlinewidth'/ &
            1x,f6.4,1x,f6.4,1x,f6.4,1x,' setrgbcolor')
    
!
! Check to make sure the data file exists, then either write an error
! message and stop, or read in as much data as possible:
!
      inquire(file=datafl(ii),exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the data file does not exist,'
            write(*,*) '        check for the file and try again  '
            write(lpsout,999)
            stop
      endif

!how many data?

! Read data for the files:
!
      open(lin,file=datafl(ii),status='unknown')
      read(lin,*,err=99)
      read(lin,*,err=99)     nvari
      do i=1,nvari
            read(lin,*,err=99)
      end do
      maxdat = 0
 21   read(lin,*,end=31,err=99)
      maxdat = maxdat + 1
      go to 21
 31   continue
!       

!
if(allocated(vrp)) deallocate(vrp)
if(allocated(vrv)) deallocate(vrv)

if(allocated(xxx)) deallocate(xxx); if(allocated(yyy)) deallocate(yyy)

      allocate (yyy(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
      allocate (xxx(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
      allocate (vrp(maxdat), stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                  stop
            end if
!      
      allocate (vrv(maxdat),stat = test)
            if (test.ne.0) then
                  write(*,*) 'Error: Allocation failed due to ', &
                      'insufficient memory!', test
                 stop
           end if
!     
! Now, read the data into the arrays:
!
      rewind(lin)
      read(lin,'(a)',err=99) str
      read(lin,*,err=99)     nvari
      do i=1,nvari
            read(lin,'(a24)',err=99) str(1:24)
      end do
      if(ivv(ii).le.0.or.ivv(ii).gt.nvari) then
            write(*,*) ' ERROR: ivr is invalid '
            write(*,*) '        >0 and <number of variables in file'
            stop
      end if
      nd   = 0
      nt   = 0
      xtwt = 0.0

 if(gridded(ii) == 1) then
      
      do k=1,nz(ii)
        vrz = (k-1) * zsiz(ii) + zmn(ii)
      do j=1,ny(ii)
        vry = (j-1) * ysiz(ii) + ymn(ii)
      do i=1,nx(ii)
        read(lin,*,end=3,err=99) (var(jj),jj=1,nvari)
!
! Trim this data?
!
        if(var(ivv(ii)).lt.tmin(ii).or.var(ivv(ii)).ge.tmax(ii)) cycle
!
! Accept this data:
!
        nd = nd + 1
        vrx = (i-1) * xsiz(ii) + xmn(ii)
      
        call rotc(vrx,vry,vrz,azimuth,dip,vrp(nd))
        
        vrv(nd) = var(ivv(ii))
!
! Go back for another data:
!
      end do
      end do
      end do
      
      else
      
 222  read(lin,*,end=333,err=99) (var(j),j=1,nvari)
!
! Trim this data?
!
      if(var(ivv(ii)).lt.tmin(ii).or.var(ivv(ii)).ge.tmax(ii)) then
            nt = nt + 1
            go to 2
      endif
!
! Accept this data:
!
      nd = nd + 1
      vrv(nd) = var(ivv(ii))
      if(ivx(ii).ge.1) then
            vrx = var(ivx(ii))
      else
            vrx = 0.
      endif
      if(ivy(ii).ge.1) then
            vry = var(ivy(ii))
      else
            vry = 0.
      endif
      if(ivz(ii).ge.1) then
            vrz = var(ivz(ii))
      else
            vrz = 0.
      endif
      
      call rotc(vrx,vry,vrz,azimuth,dip,vrp(nd))
!
! Go back for another data:
!
      go to 222
      
      end if
      
 333  close(lin)

!
! Compute average of bins along swath axis
!      
      vrp = vrp - v1minh 
      call binavg(vrp,vrv,maxdat,nd,nbin,v1min,v1max,binmid,binval,bincnt,ierr,lqnt,uqnt,binquant)
!
! Locate all the points:
!
      cnt=0
      np = 0
      do i=1,nbin
            if(binmid(i).ge.xhsmn.and.binmid(i).lt.xhsmx.and. &
              binval(i).ge.yhsmn.and.binval(i).lt.yhsmx) then
                  xval = binmid(i)
                  if(ilogy.eq.1) then
                        yval = alog10(max(binval(i),EPSLON))
                  else
                        yval = binval(i)
                  endif
                  np = np + 1
                  xx = resc(xscmin,xscmax,hpxmin,hpxmax,xval)
                  yy = resc(yscmin,yscmax,hpymin,hpymax,yval)
                  ix = int((resc(xmin,xmax,pxmin,pxmax,xx))/pscl)
                  iy = int((resc(ymin,ymax,pymin,pymax,yy))/pscl)
                  write(str,'(I5)') ii
                  if(pts(ii)>0) write(lpsout,1011) ix,iy,trim(adjustl(str))
 1011             format('n ',i5,1x,i5,' bullet',a)
                  cnt=cnt+1
                  xxx(cnt)=real(xx)
                  yyy(cnt)=real(yy)
            end if
      end do
!
! Draw quantile lines:
!
      wid = 5
      if(ierr == 1) then
      do i=1,nbin
            if(binmid(i).ge.xhsmn.and.binmid(i).lt.xhsmx.and. &
              binval(i).ge.yhsmn.and.binval(i).lt.yhsmx) then
                  xval = binmid(i)
                  xtmp(np+1)=binmid(i)
                  if(ilogy.eq.1) then
                        yval1 = alog10(max(binquant(i,1),EPSLON))
                        yval2 = alog10(max(binquant(i,2),EPSLON))
                  else
                        yval1 = binquant(i,1)
                        yval2 = binquant(i,2)
                  endif
                  np = np + 1
                  xx = resc(xscmin,xscmax,hpxmin,hpxmax,xval)
                  yy1= resc(yscmin,yscmax,hpymin,hpymax,yval1)
                  yy2= resc(yscmin,yscmax,hpymin,hpymax,yval2)
                  ix = int((resc(xmin,xmax,pxmin,pxmax,xx))/pscl)
                  iy1 = int((resc(ymin,ymax,pymin,pymax,yy1))/pscl)
                  iy2 = int((resc(ymin,ymax,pymin,pymax,yy2))/pscl)
                  if(pts(1)>0) write(lpsout,102) ix-5,iy1,ix+5,iy1,ix,iy1,ix,iy2,ix-5,iy2,ix+5,iy2
            end if
      end do   
      end if   

      if(lwid(ii)>0) call psline(cnt,xxx,yyy,lwid(ii)*.24,ldash(ii))

    yy=ystats
    x2=xstats

 end do !end loop over all data files

!
! Legend
!    
      write(lpsout,1001)
1001  format(' ',/, &
                '/NimbusSanL-Regu  findfont   24 scalefont setfont'/ &
                '0.5 setlinewidth')
      write(lpsout,'(3f6.3,a)') 0.,0.,0.,' setrgbcolor'
      rxs = 550.0 + sposx*450
      rys = 500.0 + sposy*450 
      write(lpsout,1022) rxs-60,rys,rxs+70,rys,azimuth/DEG2RAD
      rys = rys - 30
      write(lpsout,1023) rxs-60,rys,rxs+70,rys,dip/DEG2RAD
      rys = rys - 30
      
      if(ilegend.eq.1) then

            rys = rys - 10
                 
            do i=1,nfiles
                jclr = color(i)
                rirr  = redint(jclr)
                rigg  = grnint(jclr)
                ribb  = bluint(jclr)
                write(lpsout,504) rirr,rigg,ribb

                !draw bullet
                write(str,'(I3)') i
                if(pts(i)>0) write(lpsout,906) real(rxs),real(rys),' bullet',trim(adjustl(str))
                !draw a line
                if(lwid(i)>0) then
                    write(lpsout,'(f6.2,x,a)') real(lwid(i)),'setlinewidth'
                    if(ldash(i).gt.0) write(lpsout,'(a24)') dash(min(10.,ldash(i)))
                    write(lpsout,'(a)') 'n'
                    write(lpsout,'(I8,x,I8,x,a)') int(rxs-60),int(rys),'m'
                    write(lpsout,'(I8,x,I8,x,a)') int(rxs+60),int(rys),'l'
                    write(lpsout,'(a)') 's'
                    if(ldash(i).gt.0) write(lpsout,'(a)') '[] 0 setdash'
                end if
                
                write(lpsout,504) 0,0,0
                write(lpsout,1021) real(rxs+70),real(rys-5),trim(adjustl(l_text(i)))
                rys=rys-30
            end do
      end if
1021 format(2(x,f7.2),' m (',a,') ltext',/,'0 0 translate')
1022 format(2(x,f7.2),' m (Azimuth = ) ltext',2(x,f7.2),' m (',f5.1,') ltext')
1023 format(2(x,f7.2),' m (Dip =     ) ltext',2(x,f7.2),' m (',f5.1,') ltext')
906	 format('n  ',2(x,f7.2),' ',a,a)

!
!
! Add a footer to the Postscript plot file:
!
      write(lpsout,999)
 999  format('%END OF POSTSCRIPT FILE',/,'4.166667 4.166667 scale',/,/, &
            '%%EOF',/,'showpage')
!
! Finished:
!
      close(lpsout)
      write(*,9998) VERSION
 9998 format(/' SWATHPLT Version: ',f5.3, ' Finished'/)
      stop
 97   stop 'Error in parameter file somewhere'
 99   stop 'Error in data file somewhere'
      end
!
!

      integer function labelPrecision(str)
         implicit none
         character(len=*), intent(IN) :: str
         integer :: itrim, i
         itrim=index(str,'.')
         labelPrecision = 0
         do i=itrim+1,len(str)
            if(str(i:i) /= '0') labelPrecision =  &
            max(labelPrecision,i-itrim)
         end do
      end function labelPrecision
      
      
      
      subroutine matrix_mult(X,Y,i,j,l,mmult)
      !sub to preform matrix multiplication
      !x is i by j, y is j by l
      
      integer i,j,k      
      real*8 X(i,j),Y(j,l),mmult(i,l)
      
      mmult=0
      do k=1,i
      do kk=1,l
      
        do kkk=1,j
            mmult(k,kk)=mmult(k,kk)+X(k,kkk)*Y(kkk,kk)
        end do
      end do
      end do

      end subroutine matrix_mult
      
      
 subroutine draw_histograms(nd,xmin,xmax,ymin,ymax,xval,yval,lpsout,ilogy,plot_hist)
 !draw the histograms on the axis if required
      logical    testfl,testdt,testbh,testux,testuy
      integer    lin,lout,ninx(500),niny(500),lpsout,plot_hist,maxicls
      real       xval(nd),yval(nd),pval(nd),bhist(500,500),uhistx(500),uhisty(500),xmin,xmax,ymin,ymax

!
! Draw the "bottom" X axis histogram Plot:
!
      write(*,9996)
 9996 format(/' Writing histograms.....'/)
 
      write(lpsout,1001)
1001  format(' ',/,'/NimbusSanL-Regu  findfont   24 scalefont setfont')
 
! Work out the number of data per class:
      do id=1,nd
            niny(id) = yval(id)
      end do
 
      maxnin = 0.0
      do icls=1,nd
            if(niny(icls).gt.maxnin) then
                maxnin = niny(icls)
                maxicls = icls
            end if
      end do
      
      if(plot_hist==1) then
       
      write(lpsout,111)
 111  format('%',/,'% Bottom Histogram Plot:',/,'%',/,'%',/,'0.0 setlinewidth',/)

      do icls=1,nd
            zlo = -64.0 + real(icls-1)/real(nd)      *1414.0
            zup = -64.0 + real(icls  )/real(nd)      *1414.0
            pup = 128.0 + real(niny(icls))/real(maxnin) * 250.0
            write(lpsout,112) zlo,zlo,pup,zup,pup,zup
      end do
      zlo = -64.0 + real(maxicls-1)/real(nd)      *1414.0
      zup = -64.0 + real(maxicls  )/real(nd)      *1414.0
      pup = 128.0 + real(niny(maxicls))/real(maxnin) * 250.0
      write(lpsout,113) (zlo+zup)/2,pup-30,niny(maxicls)
 112  format('n ',f6.1,' 128 m ',f6.1,1x,f6.1,' l ',f6.1,1x,f6.1,' l ', &
          /,'  ',f6.1,' 128 l c gs 0.90 setgray fill gr s')
 113  format(2f6.1,' m (',i5,') ctext')          
          
      end if

end  subroutine draw_histograms


      subroutine rotc(xorig,yorig,zorig,azimuth,dip,ry)
!-----------------------------------------------------------------------
!
! xorig,yorig,zorig   original coordinates
! azimuth,dip         angles of rotation (radians counter clockwise)
!
! ry                  projected coordinate
!
!
!
!-----------------------------------------------------------------------
      real    xorig,yorig,zorig,azimuth,dip,ry
!
! Get projected coordinate
!

      ry = xorig * sin(azimuth) * cos(dip) &
         + yorig * cos(azimuth) * cos(dip) &
         + zorig * sin(dip)
      

      return
      end subroutine rotc
      
      
      
      subroutine binavg(vrp,vrv,maxdat,nd,nbin,v1min,v1max,binmid,binval,bincnt,ierr,lqnt,uqnt,binquant)
!-----------------------------------------------------------------------
!
!     vrp       -vector with projected coordinate
!     vrv       -vector with grade value
!     maxdat    -size of vrp and vrv vectors
!     nd        -number of data
!     nbin      -number of bins
!     v1min     -minimum projected coordinate
!     v1max     -maximum projected coordinate
!     binmid    -vector with projected coordinate at bin center
!     binval    -vector with average grade in each bin
!     bincnt    -vector with count in each bin
!     binquant  -vectors with 25 and 75 quantiles in each bin
!
!-----------------------------------------------------------------------
    integer         maxdat,nd,nbin,cnt,bincnt(nbin),test,ierr
    real            v1min,v1max,lqnt,uqnt
    real            vrp(maxdat),vrv(maxdat)
    real            binmid(nbin),binval(nbin),binquant(nbin,2)
    
    real,allocatable :: binsort(:)
    
    real            binsize,binmin,binmax,sum
    
!
! Sort by projected coordinate
!    
    call sortem(1,nd,vrp,1,vrv,b,b,b,b,b,b)
    
    allocate(binsort(nd),stat=test)
    if(test /= 0) stop 'ERROR allocating sort array'
!
! Get bin size
!    
    binsize = (v1max-v1min) / nbin
!
! For each bin:
!    
    j = 1
    do i=1,nbin
    !
    ! Get bin min and max coordinates and midpoint
    !
        binmin = (i-1) * binsize + v1min
        binmax =  i    * binsize + v1min
        binmid(i) = (binmin + binmax) * 0.5
    !
    ! Find start of bin
    !       
        do while(vrp(j) < binmin)
            j = j + 1
            if(j > nd) exit
        end do
    !
    ! Get average within bin
    !
        sum = 0
        cnt = 0
        do while(vrp(j) <= binmax)
            sum = sum + vrv(j)
            cnt = cnt + 1
            binsort(cnt) = vrv(j)
            j = j + 1
            if(j > nd) exit
        end do
        binval(i) = sum / cnt
        bincnt(i) = cnt
    !
    ! Get quantiles
    !        
        if(ierr == 1) then
        call sortem(1,cnt,binsort,0,b,b,b,b,b,b,b)
        if(floor(cnt*lqnt/100) > 0) binquant(i,1) = binsort(cnt*lqnt/100)
        if(floor(cnt*uqnt/100) > 0) binquant(i,2) = binsort(cnt*uqnt/100)
        end if
    
    end do
    
    
    return
    end subroutine binavg



subroutine makepar
!----------------------------------------------------------------------
!                       WRITE A PARAMETER FILE                         
!----------------------------------------------------------------------
    integer, parameter :: lun=99
    open(lun,file='swathplt.par',status='UNKNOWN')
    write(lun,10)
10  format(  &
    '                  Parameters for SWATHPLT',/, &
    '                  **********************',/, &
    '',/, &
    'START OF PARAMETERS:',/, &
    '2                       -number of data files to plot',/, &
    'data.out                -main data file (will calculate statistics, default parameters and histogram based on this file)',/, &
    '9   6  0  0             -columns for X, Y, wt, third var.',/, &
    '-1.0e21   1.0e21        -trimming limits',/, &
    '0   0  5  1  2          -line width (0=none), dashing, bullet size (0=none), color',/, &
    'data.out                -main data file (will calculate statistics, default parameters and histogram based on this file)',/, &
    '5   6  0  0             -columns for X, Y, wt, third var.',/, &
    '-1.0e21   1.0e21        -trimming limits',/, &
    '1   0  5  16 -1         -line width (0=none), dashing, bullet size (0=none), color',/, &
    'scatplt.ps              -file for Postscript output',/, &
    '0 -1  0                 -X min and max, (0=arith, 1=log) (max<min for auto scaling based on data file #1)',/, &
    '0 -1  0                 -Y min and max, (0=arith, 1=log) (max<min for auto scaling based on data file #1)',/, &
    '0.0      20             -limits for third variable gray scale',/, &
    '1                       -reference stats to plot(-1=none,0=cor only, 1=minimal,2=full)',/, &
    '1                       -positioning of stats (L to R: -1 to 1)',/, &
    '3                       -plot x/y histograms of the first data file (0=none, 1=xonly, 2=yonly, 3=both) ***WARNING*** POSSIBLE STACK OVERFLOW-FOR LARGE DATA SETS SET TO NONE',/, &
    'Primary vs. Secondary   -title',/, &
    'default                 -X axis label (default=get from data file #1)',/, &
    'default                 -y axis label (default=get from data file #1)',/, &
    '',/, &
    '',/, &
    '******************************************',/, &
    'Can remove lines below to shorten par file',/, &
    '******************************************',/, &
    '',/, &
    'BLOCK FOR USER DEFINED LEGEND:',/, &
    '1 1 -.7                 -plot a legend for these shapes (1=yes),x,y between -1=left 1=right',/, &
    'Data set 1              -label for data set 1',/, &
    'Data set 2              -label for data set 2',/, &
    '',/, &
    '',/, &
    '',/, &
    'Color Codes for the curves:',/, &
    ' 1=red, 2=orange, 3=yellow, 4=light green, 5=green, 6=light blue,',/, &
    ' 7=dark blue, 8=violet, 9=white, 10=black, 11=purple, 12=brown,',/, &
    ' 13=pink, 14=intermediate green, 15=gray 16=gray10, 17=gray20, 18=gray30,',/, &
    ' 19=gray40, 20=gray50, 21=gray60 22=gray70, 23=gray80, 24=gray90',/, &
    '',/, &
    '',/, &
    'Dashing Codes for the curves:',/, &
    ' 1 = [40 20], 2 = [13 14 13 20], 3 = [12 21 4 21], 4 = [10 10],',/, &
    ' 5 = [20 20], 6 = [30 30],  7 = [40 40], 8 = [ 3  3],',/, &
    ' 9 = [ 5  5],  10 = [ 8  8]',/, &
    '',/, &
    '',/, &
    '',/)

    close(lun)
end subroutine makepar


subroutine help
!----------------------------------------------------------------------
!                       WRITE A PARAMETER FILE                         
!----------------------------------------------------------------------
    integer, parameter :: lun=99
    
    write(*,10)
10  format(  &
    '       ',/, &
    '       ',/, &
    'IF YOU GET A STACK OVERFLOW ',/, &
    'REMOVE LINEAR REGRESSION AND HISTOGRAM OPTIONS!',/, &
    '       ',/, &
    '01	                  Parameters for SWATHPLT',/, &
    '02	                  **********************',/, &
    '03	',/, &
    '04	START OF PARAMETERS:',/, &
    '05	2                     	',/, &
    '06	data.out              	',/, &
    '07	9   6  0  0           	',/, &
    '08	-1.0e21   1.0e21      	',/, &
    '09	0   0  5  1  2         	',/, &
    '10	data.out              	',/, &
    '11	5   6   0           	',/, &
    '12	-1.0e21   1.0e21      	',/, &
    '13	1   0  5  16 -1       	',/, &
    '14	scatplt.ps            	',/, &
    '15	0  -1  0               	',/, &
    '16	0  -1  0               	',/, &
    '17	1                     	',/, &
    '18	0.0      20           	',/, &
    '19	1                     	',/, &
    '20	1                     	',/, &
    '21	3                     	',/, &
    '22	Primary vs. Secondary 	',/, &
    '23	default               	',/, &
    '24	default               	',/, &
    '25	',/, &
    '26	BLOCK FOR USER DEFINED LEGEND:',/, &
    '27	1  1   -.7                  	',/, &
    '28	Data set 1               	',/, &
    '29	Data set 2               	',/, &
    '',/, &
    'Line-by-Line Parameter Description',/, &
    'Line 5 - Number of data sets to plot.',/, &
    'Line 7 - The weight is only used for calculating',/, &
    '  statistics.',/, &
    'Line 9 - To plot a line set line width >0.',/, &
    '  To plot bullets set bullet size >0. ',/, &
    'Lines 10-13 - Lines for the second data file.',/, &
    '  Note that in line 11 there is no weight column',/, &
    '  because the statistics are only calculated for',/, &
    '  the first data set.',/, &
    '',/, &
    'TO PLOT MORE THAN TWO DATA FILES REPEAT LINES 10-13',/, &
    'Lines 15-16 - To use automatic scaling set max<min.',/, &
    '  The min and max of the first data set will be used',/, &
    '  to set the axis limits.',/, &
    'Line 19 - Level of detail in the statistical report',/, &
    '  for the first data file:  -1 = no statistics reported ;',/, &
    '  0 = only report the correlation ; 1 = only report',/, &
    '  the number of data and x/y mean values ; 2 = report',/, &
    '  all statistics.',/, &
    'Line 20 - Horizontal location of the statistics.',/, &
    'Line 21 - Option to plot the univariate histograms',/, &
    '  of the first data file on the x or y axis.',/, &
    'Lines 23-24 - If the labels are set to ‘default’',/, &
    '  the labels in the first data file will be used.',/, &
    '',/, &
    'ALL LINES BELOW LINE 24 CAN BE ELIMINATED.',/, &
    'Line 27 - Option to plot a legend for the data sets.',/, &
    '  Can also specify the x location (-1=left , +1=right)',/, &
    '  and the y location (-1=bottom , +1=top).',/, &
    'Lines 28-29 - Labels for the legend.  There should be',/, &
    '  one line per data set plotted (line 5).',/, &
    '',/)

    close(lun)
end subroutine help


