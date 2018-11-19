      program gtcurve_plot
c-----------------------------------------------------------------------
c
c
c
c
c-----------------------------------------------------------------------
      use msflib
      real, parameter    :: VERSION=2.000
      integer, parameter :: MAXLEN=512,MAXCAT=24

      character(len=512) :: outfl,str
      character          :: title*40,dash(10)*24
      logical            :: testfl
      integer            :: test

      real, allocatable      :: linew(:),cutoff(:),tons(:),grade(:),
     +                          var(:),cutoff2(:)
      integer, allocatable   :: lcol(:),ivrc(:),ivrt(:),ivrg(:),idsh(:),
     +                          ibul(:)
      real                   :: redintl(MAXCAT),grnintl(MAXCAT),
     +                          bluintl(MAXCAT),linint
      character, allocatable :: curvefl(:)*512,curvename(:)*40
c
c Common variables for Postscript Plotting:
c
      common /psdata/ lpsout,pscl,pxmin,pxmax,pymin,pymax,xmin,
     +                xmax,ymin,ymax
c
c Hardwire many of the plot parameters:
c
      data lin/1/,lpsout/2/,pscl/0.24/,pxmin/0.0/,pxmax/288.0/
     +     pymin/0.0/,pymax/216.0/,xmin/-10.0/,xmax/83.3/,
     +     ymin/-10.0/,ymax/60.0/,hpxmin/1.0/,hpxmax/59.0/,
     +     hpymin/0.0/,hpymax/58.0/,linc/3/
c
c Hardcoded categorical colors:
c
      data     redintl/1.0000,1.0000,1.0000,0.4980,0.0   ,0.0,   0.0,
     +                 1.0000,1.0000,   0.0,0.4980,0.6667,1.0000,0.0,
     +                 0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000,
     +                 0.7000,0.8000,0.9000/,
     +         grnintl/   0.0,0.4980,1.0000,1.0000,1.0000,1.0000,0.0,
     +                    0.0,1.0000,0.0   ,0.0   ,0.3333,0.3333,1.0000,
     +                 0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000,
     +                 0.7000,0.8000,0.9000/,
     +         bluintl/0.0   ,0.0   ,0.0   ,0.0   ,0.0   ,1.0000,1.0000,
     +                 1.0000,1.0000,0.0   ,1.0000,0.0   ,0.6667,0.4980,
     +                 0.7843,0.1000,0.2000,0.3000,0.4000,0.5000,0.6000,
     +                 0.7000,0.8000,0.9000/
c
c Dashing
c
      data dash/'[40 20] 0 setdash       ',
     +          '[13 14 13 20] 0 setdash ',
     +          '[12 21 4 21] 0 setdash  ',
     +          '[10 10] 0 setdash       ',
     +          '[20 20] 0 setdash       ',
     +          '[30 30] 0 setdash       ',
     +          '[40 40] 0 setdash       ',
     +          '[ 3  3] 0 setdash       ',
     +          '[ 5  5] 0 setdash       ',
     +          '[ 8  8] 0 setdash       '/
c
c Note VERSION number:
c
      write(*,9999) VERSION
 9999 format(/' GTCURVE_PLOT Version: ',f5.3/)
c
c Get the name of the parameter file - try the default name if no input:
c
      do i=1,512
            str(i:i) = ' '
      end do
      call getarg(1,str)
      if(str(1:1).eq.' ')then
            write(*,*) 'Which parameter file do you want to use?'
            read (*,'(a)') str
      end if
      if(str(1:1).eq.' ') str(1:20) = 'gtcurve_plot.par    '
      inquire(file=str,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the parameter file does not exist,'
            write(*,*) '        check for the file and try again  '
            write(*,*)
            if(str(1:20).eq.'gtcurve_plot.par    ') then
                  write(*,*) '        creating a blank parameter file'
                  call makepar
                  write(*,*)
            end if
            stop
      endif
      open(lin,file=str,status='OLD')
c
c Find Start of Parameters:
c
 1    read(lin,'(a4)',end=97) str(1:4)
      if(str(1:4).ne.'STAR') go to 1
c
c Read Input Parameters:
c
      read(lin,*,err=97) ncurve,bulsiz
      write(*,*) ' number of curves, bullet size = ',ncurve,bulsiz
      bulsiz = bulsiz * 8.0

      allocate (curvefl(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating curve file array'
      allocate (curvename(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating line dashing array'
      allocate (ivrc(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating cutoff column array'
      allocate (ivrt(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating tons column array'
      allocate (ivrg(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating grade column array'
      allocate (linew(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating line width array'
      allocate (ibul(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating line bullet array'
      allocate (lcol(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating line color array'
      allocate (idsh(ncurve),stat=test)
      if (test.ne.0) stop ' error allocating line dashing array'
      curvefl(:)=" "
      curvename(:)=" "
      ivrc  = 0
      ivrt  = 0
      ivrg  = 0
      linew = 0.0
      ibul  = 0
      lcol  = 0
      idsh  = 0

      do i=1,ncurve

      read(lin,'(a)',err=97) curvefl(i)
      call chknam(curvefl(i),MAXLEN)
      write(*,*) ' curve file = ',curvefl(i)(1:40)

      read(lin,*,err=97) ivrc(i),ivrt(i),ivrg(i)
      write(*,*) ' column numbers = ',ivrc(i),ivrt(i),ivrg(i)

      read(lin,*,err=97) linew(i), idsh(i), ibul(i) ,lcol(i)
      write(*,*) ' lw,ds,ib,cl = ',linew(i),idsh(i),ibul(i),lcol(i)
      linew(i) = linew(i) * 4.0

      end do

      read(lin,'(a)',err=97) outfl
      call chknam(outfl,MAXLEN)
      write(*,*) ' output file = ',outfl(1:40)

      read(lin,*,err=97) cutmin,cutmax
      write(*,*) ' cutoff limits = ',cutmin,cutmax

      read(lin,*,err=97) tonmin,tonmax
      write(*,*) ' tonnage limits = ',tonmin,tonmax

      read(lin,*,err=97) grdmin,grdmax
      write(*,*) ' grade limits = ',grdmin,grdmax
      tonmax = tonmax*1.001
      grdmax = grdmax*1.001

      read(lin,'(a40)',err=97) title
      call chktitle(title,40)
      write(*,*) ' title = ',title

      read(lin,*,err=97) ilegend,rxs,rys
      write(*,*) ' plot legend, x and y start = ',ilegend,rxs,rys

      if(ilegend.eq.1) then
      do i=1,ncurve
         read(lin,'(a)',err=97) curvename(i)
         call chktitle(curvename(i),40)
         write(*,*) ' curvename  file = ',curvename(i)
      end do
      endif

      close(lin)
c
c Open the output file and add a header:
c
      xrange = hpxmax - hpxmin
      yrange = hpymax - hpymin
      open(lpsout,file=outfl,status='UNKNOWN')
      write(lpsout,998) title(1:20),bulsiz
 998  format('%!PS-Adobe-3.0                       %    Remove     ',
     +    /, '90 234 translate 1.5 1.5 scale       %  these lines  ',
     +    /, '                                     % for EPSF file ',
     +    /, '%!PS-Adobe-3.0 EPSF-3.0',
     +    /, '%%BoundingBox: 0 0 288 216',
     +    /, '%%Creator: GTCURVE_PLOT',
     +    /, '%%Title:   ',a20,
     +    /, '%%CreationDate: ',
     +    /, '%%EndComments',/,/,/,'%',/,'%',/,'%',/,
     +    /, '/m {moveto} def /l {lineto} def /r {rlineto} def',
     +    /, '/s {stroke} def /n {newpath} def /c {closepath} def',
     +    /, '/rtext{ dup stringwidth pop -1 div 0 rmoveto show } def',
     +    /, '/ctext{ dup stringwidth pop -2 div 0 rmoveto show } def',
     +    /, '/ltext{show} def /gr{grestore} def /gs{gsave} def',
     +    /, '/tr{translate} def /setc{setrgbcolor} def',
     +    /, '/bullet{ ',f6.2,' 0 360 arc c fill } def',/,/,
     +    /, '%72 72 translate',/,/,
     +    /, '0.240000 0.240000 scale')
c
c Write the title and the labels:
c
      ts   = 7.5
      xloc = hpxmin - 0.15*xrange
      yloc = hpymin + 0.50*yrange
      str(1:32) = 'Tonnage - Fraction of Total     '
      call pstext(xloc,yloc,32,str,ts,1,90.0,1)
      xloc = hpxmin + 1.20*xrange
      yloc = hpymin + 0.50*yrange
      str(1:20) = 'Grade above cutoff  '
      call pstext(xloc,yloc,20,str,ts,1,90.0,1)
      xloc = hpxmin + 0.50*xrange
      yloc = hpymin - 0.15*yrange
      str(1:20) = 'Cutoff Grade        '
      call pstext(xloc,yloc,12,str,ts,1,0.0,1)
      yloc = hpymax + 0.025*yrange
      call pstext(hpxmin,yloc,40,title,8.0,3,0.0,0)
      xhsmn = cutmin
      xhsmx = cutmax*1.000001
      yhsmn = tonmin
      yhsmx = tonmax
      ilog  = 0
c
c Scale and Draw the scatterplot axes:
c
      call scal(xhsmn,xhsmx,yhsmn,yhsmx,grdmin,grdmax,hpxmin,hpxmax,
     +          hpymin,hpymax,ilog,1)
c
c Write the different curves
c
      do icurve=1,ncurve

      inquire(file=curvefl(icurve),exist=testfl)
      if(.not.testfl) then 
         write(*,*) 'ERROR - the curve file does not exist',
     +               curvefl(icurve)
         stop
      endif
      open(linc,file=curvefl(icurve),status='OLD')
      read(linc,'(a)',err=98) str
      read(linc,*,err=98) nvari
      allocate (var(nvari), stat = test)
      if(test.ne.0) stop 'Error - allocating tons array'

      if(ivrt(icurve).gt.nvari.or.ivrg(icurve).gt.nvari) 
     +       stop ' ivr is greater than the number in curve file!'

      do i=1,nvari
            read(linc,'()',err=98)
      end do
      ncut = 0
 300  read(linc,*,end=310,err=98) (var(j),j=1,nvari)
      ncut = ncut + 1
      go to 300
 310  continue

      allocate (tons(ncut), stat = test)
      if(test.ne.0) stop 'Error - allocating tons array'
      allocate (grade(ncut), stat = test)
      if(test.ne.0) stop 'Error - allocating grade array'
      allocate(cutoff(ncut),stat=test)
      if(test.ne.0) stop 'Error - allocating cutoff array'
      allocate(cutoff2(ncut),stat=test)
      if(test.ne.0) stop 'Error - allocating cutoff array'
      tons   = 0.0
      grade  = 0.0
      cutoff = 0.0
      cutoff2 = 0.0

      rewind(linc)
      do i=1,nvari+2
            read(linc,'()',err=98)
      end do
      its  = 0
      ite  = 0
      igrs = 0
      igre = 0
      do i=1,ncut
         read(linc,*) (var(j),j=1,nvari)
         cutoff(i)  = var(ivrc(icurve))
         cutoff2(i) = var(ivrc(icurve))
         tons(i)    = var(ivrt(icurve))
         grade(i)   = var(ivrg(icurve))
         if(cutoff(i).ge.cutmin.and.cutoff(i).le.cutmax) then
            if(its.eq.0.and.tons(i).le.tonmax)   its=i
            if(ite.eq.0.and.tons(i).lt.tonmin)   ite=i-1
            if(igrs.eq.0.and.grade(i).ge.grdmin) igrs=i
            if(igre.eq.0.and.grade(i).gt.grdmax) igre=i-1
         end if
         if(cutoff(i).gt.cutmax) then
            if(ite.eq.0)  ite=i-1
            if(igre.eq.0) igre=i-1
         endif
      end do
      if(ite.eq.0)  ite=ncut
      if(igre.eq.0) igre=ncut
      if(its.eq.0)  ite=0
      if(igrs.eq.0) igre=0
      close(linc)
c
c Adjust the endpoints
c
      if(its.gt.1) then
         cnew=linint(tons(its),tons(its-1),
     +               cutoff(its),cutoff(its-1),tonmax)
         tnew=linint(cutoff(its-1),cutoff(its),
     +               tons(its-1),tons(its),cutmin)
         if(cnew.ge.cutmin.and.cnew.le.cutmax) then
            cutoff(its-1)=cnew
            tons(its-1)=tonmax
         else
            cutoff(its-1)=cutmin
            tons(its-1)=tnew
         endif
         its=its-1
      end if
      if(ite.gt.0.and.ite.lt.ncut) then
         cnew=linint(tons(ite+1),tons(ite),
     +               cutoff(ite+1),cutoff(ite),tonmin)
         tnew=linint(cutoff(ite),cutoff(ite+1),
     +               tons(ite),tons(ite+1),cutmax)
         if(cnew.ge.cutmin.and.cnew.le.cutmax) then
            cutoff(ite+1)=cnew
            tons(ite+1)=tonmin
         else
            cutoff(ite+1)=cutmax
            tons(ite+1)=tnew
         endif
         ite=ite+1
      end if
      if(igrs.gt.1) then
         cnew=linint(grade(igrs-1),grade(igrs),
     +               cutoff2(igrs-1),cutoff2(igrs),grdmin)
         tnew=linint(cutoff2(igrs-1),cutoff2(igrs),
     +               grade(igrs-1),grade(igrs),cutmin)
         if(cnew.ge.cutmin.and.cnew.le.cutmax) then
            cutoff2(igrs-1)=cnew
            grade(igrs-1)=grdmin
         else
            cutoff2(igrs-1)=cutmin
            grade(igrs-1)=tnew
         endif
         igrs=igrs-1
      end if
      if(igre.gt.0.and.igre.lt.ncut) then
         cnew=linint(grade(igre),grade(igre+1),
     +               cutoff2(igre),cutoff2(igre+1),grdmax)
         tnew=linint(cutoff2(igre),cutoff2(igre+1),
     +               grade(igre),grade(igre+1),cutmax)
         if(cnew.ge.cutmin.and.cnew.le.cutmax) then
            cutoff2(igre+1)=cnew
            grade(igre+1)=grdmax
         else
            cutoff2(igre+1)=cutmax
            grade(igre+1)=tnew
         endif
         igre=igre+1
      end if
c
c Write the curve color properties
c
      write(lpsout,501) icurve,linew(icurve)
 501  format('%',/,'% Curve #',i3,/,'%',/,'gs',/,
     +       ' ',f6.2,' setlinewidth')
      write(lpsout,503) redintl(lcol(icurve)),grnintl(lcol(icurve)),
     +                  bluintl(lcol(icurve))
 503  format(3(x,f6.4),' setrgbcolor')
      if(idsh(icurve).gt.10) idsh(icurve) = 10
      if(idsh(icurve).gt.0)  write(lpsout,'(a24)') dash(idsh(icurve))
c
c Write Tonnage curve:
c
      do icut=its,ite
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,cutoff(icut))
            yy = resc(tonmin,tonmax,hpymin,hpymax,tons(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.its) then
                  write(lpsout,101) xx,yy
 101              format('n ',f6.1,1x,f6.1,' m')
            else
                  write(lpsout,102) xx,yy
 102              format('  ',f6.1,1x,f6.1,' l')
            end if
      end do
      write(lpsout,103)
 103  format('s')
c
c Write Grade curve:
c
      do icut=igrs,igre
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,cutoff2(icut))
            yy = resc(grdmin,grdmax,hpymin,hpymax,grade(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.igrs) then
                  write(lpsout,101) xx,yy
            else
                  write(lpsout,102) xx,yy
            end if
      end do
      write(lpsout,103)
c
c Plot the curve with bullets if asked for
c
      if(ibul(icurve).eq.1) then
      do icut=its,ite
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,cutoff(icut))
            yy = resc(tonmin,tonmax,hpymin,hpymax,tons(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            write(lpsout,104) xx,yy
      end do
      do icut=igrs,igre
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,cutoff2(icut))
            yy = resc(tonmin,tonmax,hpymin,hpymax,grade(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            write(lpsout,104) xx,yy
      end do
 104  format('  ',f6.1,1x,f6.1,' bullet')
      endif

      write(lpsout,*) 'gr'
      deallocate (var,stat=test)
      deallocate (cutoff,stat=test)
      deallocate (cutoff2,stat=test)
      deallocate (tons,stat=test)
      deallocate (grade,stat=test)

      end do
c
c Write the legend
c
      if(ilegend.eq.1) then
      rxs=160+rxs*(500)
      rys=220+rys*(634)

      write(lpsout,1000)
 1000 format('gs',/,
     +       '/NimbusSanL-Regu  findfont   18 scalefont setfont'/
     +       '3.0 setlinewidth')

      str(:)=" "
      do icurve=ncurve,1,-1
      call strlen(curvename(icurve),40,j)
      if(j.eq.0) goto 350

      if(idsh(icurve).gt.0) then
         write(lpsout,1010) redintl(lcol(icurve)),
     +      grnintl(lcol(icurve)),bluintl(lcol(icurve)),
     +      dash(idsh(icurve)),rxs,rys,rxs+30,rys
      else
         write(lpsout,1010) redintl(lcol(icurve)),
     +      grnintl(lcol(icurve)),bluintl(lcol(icurve)),
     +      str(1:24),rxs,rys,rxs+30,rys
      endif
      call strlen(curvename(icurve),40,j)
      write(lpsout,1020) rxs+40,rys-6,curvename(icurve)(1:j)
  350 continue
      enddo

 1010 format('gs ',3(x,f7.2),' setrgbcolor ',a24,
     +       ' n ',2(x,f7.2),' m ',2(x,f7.2),' l s gr')
 1020 format(2(x,f7.2),' m (',a,') ltext',/,'0 -30 translate')

      write(lpsout,1050)
 1050 format('gr')
      endif
c
c Add a footer to the Postscript plot file:
c
      write(lpsout,999)
 999  format('%END OF POSTSCRIPT FILE',/,'4.166667 4.166667 scale',/,/,
     +       '%%EOF',/,'showpage')
c
c Finished:
c
      close(lpsout)
      write(*,9998) VERSION
 9998 format(/' GTCURVE_PLOT Version: ',f5.3, ' Finished'/)
      stop
 97   stop 'Error in parameter file somewhere'
 98   stop 'Error in curve file somewhere'
 99   stop 'Error in data file somewhere'
      end



      subroutine makepar
c-----------------------------------------------------------------------
c
c                      Write a Parameter File
c                      **********************
c
c
c
c-----------------------------------------------------------------------
      lun = 99
      open(lun,file='gtcurve_plot.par',status='UNKNOWN')
      write(lun,10)
  10  format('            Parameters for GTCURVE_PLOT',/,
     +       '            ***************************',/,/,
     +       'START OF PARAMETERS:')

      write(lun,20)
  20  format(' 2  0.5                       ',
     +       '-number of curves, bullet size')
      write(lun,30)
  30  format('gtcurve_1.out                 ',
     +       '-1 file with gtcurve')
      write(lun,40)
  40  format(' 1  2  3                      ',
     +       '-   columns for cutoff, tons, and grade')
      write(lun,50)
  50  format('0.5   0   0   10              ',
     +       '-   line width, dashing, points?, colour')
      write(lun,60)
  60  format('gtcurve_2.out                 ',
     +       '-2 file with gtcurve')
      write(lun,70)
  70  format(' 1  2  3                      ',
     +       '-   columns for cutoff, tons, and grade')
      write(lun,80)
  80  format('0.5   0   0   10              ',
     +       '-   line width, dashing, points?, colour')
      write(lun,90)
  90  format('gtcurve_plot.ps               ',
     +       '-file for postscript output')
      write(lun,100)
 100  format('     0.0    3.0               ',
     +       '-cutoff:      min and max')
      write(lun,110)
 110  format('     0.0    1.0               ',
     +       '-tonnes:      min and max')
      write(lun,120)
 120  format('     0.0    6.0               ',
     +       '-grade:       min and max')
      write(lun,130)
 130  format('Grade - Tonnage Curve         ',
     +       '-title for plot')
      write(lun,140)
 140  format(' 0   0.5   1.0                ',
     +       '-plot legend(0=no, 1=yes), xpos, ypos')
      write(lun,150)
 150  format('GT-Curve 1: Kriging           ',
     +       '-title for curve 1')
      write(lun,160)
 160  format('GT-Curve 2: Simulation        ',
     +       '-title for curve 2')
      write(lun,170)
  170 format(//'Color Codes for the curves:',/,
     +       '      1=red, 2=orange, 3=yellow, 4=light green,',
     +       ' 5=green, 6=light blue,',/,
     +       '      7=dark blue, 8=violet, 9=white,',
     +       ' 10=black, 11=purple, 12=brown,',/,
     +       '      13=pink, 14=intermediate green, 15=gray',
     +       ' 16=gray10, 17=gray20, 18=gray30,',/,
     +       '      19=gray40, 20=gray50, 21=gray60',
     +       ' 22=gray70, 23=gray80, 24=gray90')
      write(lun,180)
  180 format(//'Dashing Codes for the curves:',/,
     +         ' 1 = [40 20], 2 = [13 14 13 20], ',
     +         '3 = [12 21 4 21], 4 = [10 10],'/
     +         ' 5 = [20 20], 6 = [30 30], ',
     +         ' 7 = [40 40], 8 = [ 3  3],'/' 9 = [ 5  5], ',
     +         ' 10 = [ 8  8]')


      close(lun)
      return
      end



      subroutine chknam(str,MAXLEN)
c-----------------------------------------------------------------------
c
c                   Check for a Valid File Name
c                   ***************************
c
c This subroutine takes the character string "str" of length "len" and
c removes all leading blanks and blanks out all characters after the
c first blank found in the string (leading blanks are removed first).
c
c
c
c Author: C.V. Deutsch                                Date: January 1993
c-----------------------------------------------------------------------
      character str(MAXLEN)*1
      len=MAXLEN
c
c Remove leading blanks:
c
      do i=1,len-1
            if(str(i).ne.' ') then
                  if(i.eq.1) go to 1
                  do j=1,len-i+1
                        k = j + i - 1
                        str(j) = str(k)
                  end do
                  do j=len,len-i+2,-1
                        str(j) = ' '
                  end do
                  go to 1
            end if
      end do
 1    continue
c
c Find first blank and blank out the remaining characters:
c
      do i=1,len-1
            if(str(i).eq.' ') then
                  do j=i+1,len
                        str(j) = ' '
                  end do
                  go to 2
            end if
      end do
 2    continue
c
c Return with modified file name:
c
      return
      end



      subroutine chktitle(str,MAXLEN)
c-----------------------------------------------------------------------
c
c                     Check for a Valid Title
c                     ***********************
c
c This subroutine takes the character string "str" of length "len" and
c blanks out all characters after the first back slash
c
c
c
c Author: C.V. Deutsch                                    Date: May 1995
c-----------------------------------------------------------------------
      character str(MAXLEN)*1
      len=MAXLEN
c
c Remove leading blanks:
c
      do i=1,len-1
            if(str(i).ne.' ') then
                  if(i.eq.1) go to 1
                  do j=1,len-i+1
                        k = j + i - 1
                        str(j) = str(k)
                  end do
                  do j=len,len-i+2,-1
                        str(j) = ' '
                  end do
                  go to 1
            end if
      end do
 1    continue
c
c Find first three blanks and blank out remaining characters:
c
      do i=1,len-2
            if(str(i)  .eq.' '.and.
     +         str(i+1).eq.' '.and.
     +         str(i+2).eq.' ') then
                  do j=i+1,len
                        str(j) = ' '
                  end do
                  go to 2
            end if
      end do
 2    continue
c
c Find first back slash and blank out the remaining characters:
c
      do i=1,len-1
            if(str(i).eq.'\\') then
                  do j=i,len
                        str(j) = ' '
                  end do
                  go to 3
            end if
      end do
 3    continue
c
c Return with modified character string:
c
      return
      end



      subroutine psline(np,x,y,lwidt,idsh)
c-----------------------------------------------------------------------
c
c              Write Postscript line commands to a file
c              ****************************************
c
c
c CALLING ARGUMENTS:
c
c  np           the number of points in the x and y array to join
c  x()          array of x values in the range xmin to xmax
c  y()          array of y values in the range ymin to ymax
c  lwidt        the width of the line (1.0 = dark, 0.5 = light)
c  idsh         Dashing Index
c
c NOTES:
c
c  1. The pxmin,pxmax,.. variables are in the standard 1/72 inch 
c     resolution of the postscript page. If a different scale is 
c     going to be used in the printing set pscl to the scale.
c
c  2. If "idsh" is zero then no dashing is perfomed
c
c
c AUTHOR: C. Deutsch                                 DATE: November 1988
c-----------------------------------------------------------------------
      real      x(*),y(*),lwidt,lwold
      character dash(10)*24
c
c Common Block for Postscript Output Unit and Scaling:
c
      common /psdata/ lpsout,pscl,pxmin,pxmax,pymin,pymax,xmin,
     +                xmax,ymin,ymax
      save   lwold
c
c Dash Patterns:
c
      data dash/'[40 20] 0 setdash       ',
     +          '[13 14 13 20] 0 setdash ',
     +          '[12 21 4 21] 0 setdash  ',
     +          '[10 10] 0 setdash       ',
     +          '[20 20] 0 setdash       ',
     +          '[30 30] 0 setdash       ',
     +          '[40 40] 0 setdash       ',
     +          '[50 50] 0 setdash       ',
     +          '[50 50] 0 setdash       ',
     +          '[50 50] 0 setdash       '/
c
c Change the line width if necessary:
c
      if(pscl.lt.0.01) pscl = 1.0
      if(lwidt.ne.lwold) then
            width = lwidt/pscl
            write(lpsout,100) width
 100        format(f6.3,' setlinewidth')
            lwold = lwidt
      endif
c
c Start a new path and loop through the points:
c
      if(idsh.gt.0) write(lpsout,'(a24)') dash(min(10,idsh))
      write(lpsout,101)
 101  format('n')
      do i=1,np
            ix = int(resc(xmin,xmax,pxmin,pxmax,x(i))/pscl)
            iy = int(resc(ymin,ymax,pymin,pymax,y(i))/pscl)
            if(i.eq.1) then
                  write(lpsout,102) ix,iy
 102              format(i5,1x,i5,' m')
            else
                  write(lpsout,103) ix,iy
 103              format(i5,1x,i5,' l')
            endif
      end do      
      write(lpsout,104)
 104  format('s')
      if(idsh.gt.0) write(lpsout,105)
 105  format('[] 0 setdash')
c
c Finished - Return to calling program:
c
      return
      end



      subroutine pstext(xs,ys,lostr,str,tsiz,ifont,rot,iadj)
c-----------------------------------------------------------------------
c
c              Write Postscript Text commands to a file
c              ****************************************
c
c
c CALLING ARGUMENTS:
c
c  xs            starting value of x in the range xmin to xmax
c  ys            starting value of y in the range ymin to ymax
c  lostr      number of characters in str to print
c  str            the character string
c  tsiz            Text size in 1/72 of an inch
c  ifont      Font Number: See font number below
c  rot             Rotation Angle to post the text (default to 0.0)
c  iadj            Adjustment: 0=left adjusted, 1=centre, 2=right
c
c
c AUTHOR: C. Deutsch                                 DATE: November 1988
c-----------------------------------------------------------------------
      character str*80,fnnt(10)*32,line*132,size*4,part1*1,part2*7
c
c Common Block for Postscript Output Unit and Scaling:
c
      common /psdata/ lpsout,pscl,pxmin,pxmax,pymin,pymax,xmin,
     +                xmax,ymin,ymax
      save    fnnt,ifold,tsold,izip
c
c Preset 10 different fonts:
c
      data fnnt/'/NimbusSanL-Regu       findfont ',
     +          '/NimbusSanL-Bold       findfont ',
     +          '/NimbusSanL-BoldItal   findfont ',
     +          '/Times-Roman           findfont ',
     +          '/Times-Bold            findfont ',
     +          '/Times-Italic          findfont ',
     +          '/Times-BoldItalic      findfont ',
     +          '/Courier               findfont ',
     +          '/Courier-Bold          findfont ',
     +          '/Courier-BoldOblique   findfont '/
      data ifold/0/,tsold/0.0/,izip/0/
      part1 = '('
      part2 = ')  text'
c
c Remove leading and trailing blanks:
c
      lost = lostr
      do i=1,lostr
            if(str(1:1).eq.' ') then
                  lost = lost - 1
                  do j=1,lost
                        k = j + 1
                        str(j:j) = str(k:k)
                  end do
            else
                  go to 1
            endif
      end do
 1    k = lost
      do i=1,k
            ix = k - i + 1
            if(str(ix:ix).ne.' ') go to 2
            lost = lost - 1
      end do
 2    if(lost.le.0) return
c
c Create line to set the text size and type:
c
      if(ifont.ne.ifold.or.tsiz.ne.tsold) then
            isiz=int(tsiz/pscl)
            write(size,'(i4)') isiz
            line=fnnt(ifont)//size//' scalefont setfont'      
            write(lpsout,'(a)')line(1:54)
            ifold = ifont
            tsold = tsiz
      endif
c
c Set the correct adjustment:
c
      part2(3:3) = 'l'
      if(iadj.eq.1) part2(3:3) = 'c'
      if(iadj.eq.2) part2(3:3) = 'r'
c
c Write the lines and position to the Postscript file:
c                  
      ix = int((resc(xmin,xmax,pxmin,pxmax,xs))/pscl)
      iy = int((resc(ymin,ymax,pymin,pymax,ys))/pscl)
c
c Rotate if Necessary:
c
      line = part1//str(1:lost)//part2
      if(rot.ne.0.0) then
            irot = int(rot)
            write(lpsout,102)   ix,iy
            write(lpsout,103)   irot 
            write(lpsout,100)   izip,izip
            write(lpsout,'(a)') line(1:lost+8)
            ix   = -1.0 * ix
            iy   = -1.0 * iy
            irot = -1.0 * irot
            write(lpsout,103)   irot 
            write(lpsout,102)   ix,iy
      else
c
c Just write out the text if no rotation:
c
            write(lpsout,100)   ix,iy
            write(lpsout,'(a)') line(1:lost+8)
      endif
 100  format(i5,1x,i5,1x,'m')
 102  format(i5,1x,i5,1x,'translate')
 103  format(i5,1x,'rotate')
c
c Finished - Return to calling program:
c
      return
      end



        subroutine scal(xmin,xmax,ymin,ymax,ymin2,ymax2,xaxmin,xaxmax,
     +                  yaxmin,yaxmax,ilog,i45)
c-----------------------------------------------------------------------
c
c Draws a reasonable graph axes for a PostScript plot.  The appropriate
c labelling and tic mark interval are established.
c
c INPUT VARIABLES:
c       xmin   - the minimum of the x axis (labeled on axis)
c       xmax   - the maximum of the x axis (labeled on axis)
c       ymin   - the minimum of the y axis (labeled on axis)
c       ymax   - the maximum of the y axis (labeled on axis)
c       xaxmin - the minimum of the x axis (on PostScript window)
c       xaxmax - the maximum of the x axis (on PostScript window)
c       yaxmin - the minimum of the y axis (on PostScript window)
c       yaxmax - the maximum of the y axis (on PostScript window)
c       ilog   - scale option: 0 - both cartesian
c                              1 - semi log with x being log scale
c                              2 - semi log with y being log scale
c                              3 - log log with both being log scale
c       i45    - 45 degree line option: 0 - no, 1 - if axes are the same
c              
c
c
c AUTHOR: Clayton Deutsch                      Last Revision: April 1991
c-----------------------------------------------------------------------
      parameter (EPS=0.001)
      real       xloc(5),yloc(5)
      character  label*8,lfmt*8
c
c Common Block for Postscript Output Unit and Scaling:
c
      common /psdata/ lpsout,psscl,pxmin,pxmax,pymin,pymax,wxmin,
     +                wxmax,wymin,wymax
c
c Check to make sure that the scale can be plotted:
c
      if((xmax-xmin).le.0.0001.or.(ymax-ymin).le.0.0001) return
c
c Set up some of the parameters:
c
      tlng =       0.013 * ((xaxmax-xaxmin) + (yaxmax-yaxmin))
      tsht =       0.007 * ((xaxmax-xaxmin) + (yaxmax-yaxmin))
      psz  = 4.0 + 0.060 *  (xaxmax-xaxmin)
      pl1  = 0.6 + 0.005 *  (xaxmax-xaxmin)
      pl2  = 0.3 + 0.003 *  (xaxmax-xaxmin)
c
c Draw the axis:
c
      xloc(1) = xaxmin
      yloc(1) = yaxmax
      xloc(2) = xaxmin
      yloc(2) = yaxmin
      xloc(3) = xaxmax
      yloc(3) = yaxmin
      xloc(4) = xaxmax
      yloc(4) = yaxmax
      xloc(5) = xaxmin
      yloc(5) = yaxmax
      call psline(5,xloc,yloc,pl1,0)
c
c Show a 45 degree line?
c
c     if(i45.eq.1) then
c        if(abs(xmin-ymin).le.0.0001.and.abs(xmax-ymax).le.0.0001) then
c           xloc(1) = xaxmin
c           yloc(1) = yaxmin
c           xloc(2) = xaxmax
c           yloc(2) = yaxmax
c           call psline(2,xloc,yloc,pl1,0)
c        end if
c     end if
c
c CONSTRUCT THE X AXIS:
c
c
c Log scale?
c
      if(ilog.eq.1.or.ilog.eq.3) then
c
c      The start, end, number of log(10) cycles, the tic mark start,
c      and the number of points in defining a tic mark:
c
            tminx   = alog10(xmin)
            tmaxx   = alog10(xmax)
            ncyc    = tmaxx - tminx
            cbas    = xmin/10
            yloc(1) = yaxmin
            num     = 2
c
c      Loop along the axis drawing the tic marks and labels:
c
            do icyc=1,ncyc+1
                  cbas = cbas * 10
                  do i=1,9
                  t1   = alog10(cbas*real(i))
                  xloc(1) = resc(tminx,tmaxx,xaxmin,xaxmax,t1)
                  xloc(2) = xloc(1)
                  if(i.eq.1) then
c
c            First point - long tic mark:
c
                        yloc(2) = yloc(1) - tlng
                        call psline(num,xloc,yloc,pl2,0)
                        yloc(2) = yloc(1) - 2.5*tlng
                        if(abs(t1+9.).le.EPS) label = '1.0e-9  '
                        if(abs(t1+8.).le.EPS) label = '1.0e-8  '
                        if(abs(t1+7.).le.EPS) label = '1.0e-7  '
                        if(abs(t1+6.).le.EPS) label = '1.0e-6  '
                        if(abs(t1+5.).le.EPS) label = '0.00001 '
                        if(abs(t1+4.).le.EPS) label = '0.0001  '
                        if(abs(t1+3.).le.EPS) label = '0.001   '
                        if(abs(t1+2.).le.EPS) label = '0.01    '
                        if(abs(t1+1.).le.EPS) label = '0.1     '
                        if(abs(t1)   .le.EPS) label = '1       '
                        if(abs(t1-1.).le.EPS) label = '10      '
                        if(abs(t1-2.).le.EPS) label = '100     '
                        if(abs(t1-3.).le.EPS) label = '1000    '
                        if(abs(t1-4.).le.EPS) label = '10000   '
                        if(abs(t1-5.).le.EPS) label = '100000  '
                        if(abs(t1-6.).le.EPS) label = '1.0e+6  '
                        if(abs(t1-7.).le.EPS) label = '1.0e+7  '
                        if(abs(t1-8.).le.EPS) label = '1.0e+8  '
                        if(abs(t1-9.).le.EPS) label = '1.0e+9  '
                        call pstext(xloc(1),yloc(2),8,label,
     +                                      psz,1,0.0,1)
                  else
c
c            Not first point - short tic mark:
c
                        if(icyc.le.ncyc) then
                              yloc(2) = yloc(1) - tsht
                              call psline(num,xloc,yloc,pl2,0)
                        endif
                  endif
                    end do
              end do
      else
c
c Arithmetic Scale:
c
            do i=1,20
                  test = (xmax-xmin)/(10.0**(6-i))
                  if(test.gt.0.9) go to 1
            end do
 1          if(test.gt.3.0)                 zval = 1.0
            if(test.le.3.0.and.test.gt.2.0) zval = 0.5
            if(test.le.2.0.and.test.gt.1.2) zval = 0.4
            if(test.le.1.2)                 zval = 0.2
            nval = 5
            if(zval.eq.0.4.or.zval.eq.0.2)  nval = 4
            zval = zval * 10.0**(6-i)
            tval = zval / real(nval)
            if(i.ge.12) lfmt = '(f8.8)'
            if(i.eq.11) lfmt = '(f8.7)'
            if(i.eq.10) lfmt = '(f8.6)'
            if(i.eq.9)  lfmt = '(f8.5)'
            if(i.eq.8)  lfmt = '(f8.4)'
            if(i.eq.7)  lfmt = '(f8.3)'
            if(i.eq.6)  lfmt = '(f8.2)'
            if(i.eq.5)  lfmt = '(f8.1)'
            if(i.le.4)  lfmt = '(f8.0)'
c
c      Loop along the axis drawing the tic marks and labels:
c
            yloc(1) = yaxmin
            pos     = xmin
            num     = 2
            do i=1,100
                  yloc(2) = yaxmin - tlng
                  xloc(1) = resc(xmin,xmax,xaxmin,xaxmax,pos)
                  xloc(2) = resc(xmin,xmax,xaxmin,xaxmax,pos)
                  call psline(num,xloc,yloc,pl2,0)
                  yloc(2) = yloc(1) - 2.5*tlng
                  write(label,lfmt) pos                        
                  call pstext(xloc(1),yloc(2),8,label,psz,1,0.0,1)
                  yloc(2) = yaxmin - tsht
                  do j=1,nval-1
                       pos     = pos + tval
                       if(pos.gt.xmax) go to 2
                       xloc(1) = resc(xmin,xmax,xaxmin,xaxmax,pos)
                       xloc(2) = resc(xmin,xmax,xaxmin,xaxmax,pos)
                       call psline(num,xloc,yloc,pl2,0)
                    end do
                  pos = pos + tval
                  if(pos.gt.xmax) go to 2
            end do
 2          continue
      endif
c
c CONSTRUCT THE Y AXIS:
c
c
c Log scale?
c
      if(ilog.eq.2.or.ilog.eq.3) then
c
c      The start, end, number of log(10) cycles, the tic mark start,
c      and the number of points in defining a tic mark:
c
            tminy   = alog10(ymin)
            tmaxy   = alog10(ymax)
            ncyc    = tmaxy - tminy
            cbas    = ymin/10
            xloc(1) = xaxmin
            num     = 2
c
c      Loop along the axis drawing the tic marks and labels:
c
            do icyc=1,ncyc+1
                  cbas = cbas * 10
                  do i=1,9
                  t1   = alog10(cbas*real(i))
                  yloc(1) = resc(tminy,tmaxy,yaxmin,yaxmax,t1)
                  yloc(2) = yloc(1)
                  if(i.eq.1) then
c
c            First point - long tic mark:
c
                        xloc(2) = xloc(1) - tlng
                        call psline(num,xloc,yloc,pl2,0)
                        xloc(2) = xloc(2) - 0.1*tlng
                        if(abs(t1+9.).le.EPS) label = '1.0e-9  '
                        if(abs(t1+8.).le.EPS) label = '1.0e-8  '
                        if(abs(t1+7.).le.EPS) label = '1.0e-7  '
                        if(abs(t1+6.).le.EPS) label = '1.0e-6  '
                        if(abs(t1+5.).le.EPS) label = '0.00001 '
                        if(abs(t1+4.).le.EPS) label = '0.0001  '
                        if(abs(t1+3.).le.EPS) label = '0.001   '
                        if(abs(t1+2.).le.EPS) label = '0.01    '
                        if(abs(t1+1.).le.EPS) label = '0.1     '
                        if(abs(t1)   .le.EPS) label = '1       '
                        if(abs(t1-1.).le.EPS) label = '10      '
                        if(abs(t1-2.).le.EPS) label = '100     '
                        if(abs(t1-3.).le.EPS) label = '1000    '
                        if(abs(t1-4.).le.EPS) label = '10000   '
                        if(abs(t1-5.).le.EPS) label = '100000  '
                        if(abs(t1-6.).le.EPS) label = '1.0e+6  '
                        if(abs(t1-7.).le.EPS) label = '1.0e+7  '
                        if(abs(t1-8.).le.EPS) label = '1.0e+8  '
                        if(abs(t1-9.).le.EPS) label = '1.0e+9  '
                        call pstext(xloc(2),yloc(2),8,label,
     +                                      psz,1,0.0,2)
                  else
c
c            Not first point - short tic mark:
c
                        if(icyc.le.ncyc) then
                              xloc(2) = xloc(1) - tsht
                              call psline(num,xloc,yloc,pl2,0)
                        endif
                  endif
                  end do
            end do
      else
c
c      Determine a labelling and tic mark increment:
c
            do i=1,20
                  test = (ymax-ymin)/(10.0**(6-i))
                  if(test.gt.0.9) go to 11
            end do
 11         if(test.ge.3.0)                 zval = 1.0
            if(test.le.3.0.and.test.gt.2.0) zval = 0.5
            if(test.le.2.0.and.test.gt.1.2) zval = 0.4
            if(test.le.1.2)                 zval = 0.2
            nval = 5
            if(zval.eq.0.4.or.zval.eq.0.2)  nval = 4
            zval = zval * 10.0**(6-i)
            tval = zval / real(nval)
            if(i.ge.12) lfmt = '(f8.8)'
            if(i.eq.11) lfmt = '(f8.7)'
            if(i.eq.10) lfmt = '(f8.6)'
            if(i.eq.9)  lfmt = '(f8.5)'
            if(i.eq.8)  lfmt = '(f8.4)'
            if(i.eq.7)  lfmt = '(f8.3)'
            if(i.eq.6)  lfmt = '(f8.2)'
            if(i.eq.5)  lfmt = '(f8.1)'
            if(i.le.4)  lfmt = '(f8.0)'
c
c      Loop along the axis drawing the tic marks and labels:
c
            xloc(1) = xaxmin
            pos     = ymin
            num     = 2
            do i=1,100
                  xloc(2) = xaxmin - tlng
                  yloc(1) = resc(ymin,ymax,yaxmin,yaxmax,pos)
                  yloc(2) = resc(ymin,ymax,yaxmin,yaxmax,pos)
                  call psline(num,xloc,yloc,pl2,0)
                  xloc(2) = xloc(2) - 0.2*tlng
                  write(label,lfmt) pos                        
                  call pstext(xloc(2),yloc(2),8,label,psz,1,0.0,2)
                  xloc(2) = xaxmin - tsht
                  do j=1,nval-1
                       pos     = pos + tval
                       if(pos.gt.ymax) go to 12
                       yloc(1) = resc(ymin,ymax,yaxmin,yaxmax,pos)
                       yloc(2) = resc(ymin,ymax,yaxmin,yaxmax,pos)
                       call psline(num,xloc,yloc,pl2,0)
                     end do
                  pos = pos + tval
                  if(pos.gt.ymax) go to 12
            end do
 12         continue
      endif
c
c Second Y axis
c
      do i=1,20
            test = (ymax2-ymin2)/(10.0**(6-i))
            ikeep = i
            if(test.gt.0.9) go to 111
      end do
 111  if(test.ge.3.0)                 zval = 1.0
      if(test.le.3.0.and.test.gt.2.0) zval = 0.5
      if(test.le.2.0.and.test.gt.1.2) zval = 0.4
      if(test.le.1.2)                 zval = 0.2
      nval = 5
      if(zval.eq.0.4.or.zval.eq.0.2)  nval = 4
      zval = zval * 10.0**(6-ikeep)
      tval = zval / real(nval)
      if(i.ge.12) lfmt = '(f8.8)'
      if(i.eq.11) lfmt = '(f8.7)'
      if(i.eq.10) lfmt = '(f8.6)'
      if(i.eq.9)  lfmt = '(f8.5)'
      if(i.eq.8)  lfmt = '(f8.4)'
      if(i.eq.7)  lfmt = '(f8.3)'
      if(i.eq.6)  lfmt = '(f8.2)'
      if(i.eq.5)  lfmt = '(f8.1)'
      if(i.le.4)  lfmt = '(f8.0)'
c
c Loop along the axis drawing the tic marks and labels:
c
      xloc(1) = xaxmax
      pos     = ymin2
      num     = 2
      do i=1,100
            xloc(2) = xaxmax + tlng
            yloc(1) = resc(ymin2,ymax2,yaxmin,yaxmax,pos)
            yloc(2) = resc(ymin2,ymax2,yaxmin,yaxmax,pos)
            call psline(num,xloc,yloc,pl2,0)
            xloc(2) = xloc(2) + 0.2*tlng
            write(label,lfmt) pos                        
            call pstext(xloc(2),yloc(2),8,label,psz,1,0.0,0)
            xloc(2) = xaxmax + tsht
            do j=1,nval-1
                 pos     = pos + tval
                 if(pos.gt.ymax2) go to 112
                 yloc(1) = resc(ymin2,ymax2,yaxmin,yaxmax,pos)
                 yloc(2) = resc(ymin2,ymax2,yaxmin,yaxmax,pos)
                 call psline(num,xloc,yloc,pl2,0)
            end do
            pos = pos + tval
            if(pos.gt.ymax2) go to 112
      end do
 112  continue
c
c Return to calling program:
c
      return
      end



      real function resc(xmin1,xmax1,xmin2,xmax2,x111)
      real*8 rsc
c
c Simple linear rescaling (get a value in coordinate system "2" given
c a value in "1"):
c
      rsc  = dble((xmax2-xmin2)/(xmax1-xmin1))
      resc = xmin2 + real( dble(x111 - xmin1) * rsc )
      return
      end



      real function linint (xlow,xhigh,ylow,yhigh,xval)
c-----------------------------------------------------------------------
c
c Linearly interpolates the value of y between (xlow,ylow) and
c (xhigh,yhigh) for a value of x.
c
c
c
c-----------------------------------------------------------------------
      real, parameter :: EPSLON=1.0e-20
      linint = ylow + (yhigh-ylow)*(xval-xlow) /
     +                 amax1(EPSLON,(xhigh-xlow))
      end function linint



      subroutine strlen(str,MAXLEN,lostr)
c-----------------------------------------------------------------------
c
c      Determine the length of the string minus trailing blanks
c
c
c
c Author: C.V. Deutsch                               Date: July 1992
c Edited: Chad T. Neufeld                            Date: February 2002
c-----------------------------------------------------------------------
      character(MAXLEN) :: str
      lostr = MAXLEN
      do i=1,MAXLEN
            j = MAXLEN - i + 1
            if(str(j:j).ne.' ') exit
            lostr = lostr - 1
      end do
      end subroutine strlen


