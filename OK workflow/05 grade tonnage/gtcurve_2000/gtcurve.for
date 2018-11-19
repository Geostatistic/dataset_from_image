      program gtcruve
c-----------------------------------------------------------------------
c
c
c
c
c-----------------------------------------------------------------------
      use msflib
      real, parameter    :: VERSION=2.000
      integer, parameter :: MAXLEN=512

      character(len=512) :: datafl,outfl,str,outfl2
      character(len=40)  :: title
      real, allocatable  :: vr(:),wt(:),var(:),tons(:),grade(:)
      logical            :: testfl
      integer            :: test
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
     +     hpymin/0.0/,hpymax/58.0/
c
c Note VERSION number:
c
      write(*,9999) VERSION
 9999 format(/' GTCURVE Version: ',f5.3/)
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
      if(str(1:1).eq.' ') str(1:20) = 'gtcurve.par         '
      inquire(file=str,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the parameter file does not exist,'
            write(*,*) '        check for the file and try again  '
            write(*,*)
            if(str(1:20).eq.'gtcurve.par         ') then
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

c      read(lin,*,err=97) ncurve
c      write(*,*) ' number of curves = ',ncurve

      read(lin,'(a)',err=97) datafl
      call chknam(datafl,MAXLEN)
      write(*,*) ' data file = ',datafl(1:40)

      read(lin,*,err=97) ivr,iwt
      write(*,*) ' columns = ',ivr,iwt

      read(lin,*,err=97) tmin,tmax
      write(*,*) ' trimming limits = ',tmin,tmax

      read(lin,*,err=97) gmax
      write(*,*) ' clipping limits = ',gmax

      read(lin,'(a)',err=97) outfl2
      call chknam(outfl2,MAXLEN)
      write(*,*) ' numerical output file = ',outfl2(1:40)

      read(lin,'(a)',err=97) outfl
      call chknam(outfl,MAXLEN)
      write(*,*) ' output file = ',outfl(1:40)

      read(lin,*,err=97) ncut,cutmin,cutmax
      write(*,*) ' cutoff limits = ',ncut,cutmin,cutmax

      read(lin,*,err=97) tonmin,tonmax
      write(*,*) ' tonnage limits = ',tonmin,tonmax

      read(lin,*,err=97) grdmin,grdmax
      write(*,*) ' grade limits = ',grdmin,grdmax
      tonmax = tonmax*1.001
      grdmax = grdmax*1.001

      read(lin,'(a40)',err=97) title
      call chktitle(title,40)
      write(*,*) ' title = ',title

      close(lin)
c
c Check to make sure the data file exists, then either write an error
c message and stop, or read in as much data as possible:
c
      inquire(file=datafl,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the data file does not exist,'
            write(*,*) '        check for the file and try again  '
            stop
      endif
c
c Read data:
c
      open(lin,file=datafl,status='OLD')
      read(lin,*,err=99)
      read(lin,*,err=99) nvari

      allocate(var(nvari),stat=test)
      if(test.ne.0) stop 'Error allocating var array'
      var=0.0

      do i=1,nvari
            read(lin,'()',err=99)
      end do
      nd   = 0
      xtwt = 0.0
 2    read(lin,*,end=3,err=99) (var(j),j=1,nvari)
      if(var(ivr).lt.tmin.or.var(ivr).ge.tmax) go to 2
      if(iwt.ge.1) then
            if(var(iwt).le.1.0e-10) go to 2
      endif
      nd = nd + 1
      go to 2
 3    continue

      allocate(vr(nd),stat=test)
      allocate(wt(nd),stat=test)
      allocate(tons(ncut+1),stat=test)
      allocate(grade(ncut+1),stat=test)
      vr=0.0
      wt=0.0
      tons=0.0
      grade=0.0

      rewind(lin)
      do i=1,nvari+2
            read(lin,'()',err=99)
      end do
      i    = 0
      xtwt = 0.0
 4    read(lin,*,end=5,err=99) (var(j),j=1,nvari)
c
c Trim this data?
c
      if(var(ivr).lt.tmin.or.var(ivr).ge.tmax) go to 4
      if(iwt.ge.1) then
            if(var(iwt).le.1.0e-10) go to 4
      endif
c
c Accept this data:
c
      i = i + 1
      vr(i) = var(ivr)
      if(vr(i).gt.gmax) vr(i) = gmax
      if(iwt.ge.1) then
            wt(i) = var(iwt)
      else
            wt(i) = 1.0
      endif
      xtwt = xtwt + wt(i)
c
c Go back for another data:
c
      go to 4
 5    close(lin)
      if(nd.le.1) then
            write(*,*) ' ERROR: too few data ',nd
            stop
      endif
      write(*,*) ' Found ',nd,' data'
c
c Get grade tonnage curve:
c
c      write(*,*)
      ncut = ncut + 1
      do icut=1,ncut
            tons(icut)  = 0.0
            grade(icut) = 0.0
            cutoff      = cutmin + real(icut-1)/real(ncut-1) *
     +                                 (cutmax-cutmin)
            do i=1,nd
                  if(vr(i).ge.cutoff) then
                        tons(icut)  = tons(icut)  + wt(i)
                        grade(icut) = grade(icut) + vr(i)*wt(i)
                  end if
            end do
            grade(icut) = grade(icut) / max(tons(icut),0.001)
            tons(icut)  = tons(icut)  / max(xtwt,      0.001)
c            write(*,301) cutoff,tons(icut),grade(icut)
c 301        format('Cutoff ',f8.3,' fraction of tonnes ',f6.4,
c     +                            ' grade ',f12.3)
      end do
c      write(*,*)
c
c Open the output file and add a header:
c
      xrange = hpxmax - hpxmin
      yrange = hpymax - hpymin
      open(lpsout,file=outfl,status='UNKNOWN')
      write(lpsout,998) title(1:20)
 998  format('%!PS-Adobe-3.0                       %    Remove     ',
     +    /, '90 234 translate 1.5 1.5 scale       %  these lines  ',
     +    /, '                                     % for EPSF file ',
     +    /, '%!PS-Adobe-3.0 EPSF-3.0',
     +    /, '%%BoundingBox: 0 0 288 216',
     +    /, '%%Creator: GTCURVE',
     +    /, '%%Title:   ',a20,
     +    /, '%%CreationDate: ',
     +    /, '%%EndComments',/,/,/,'%',/,'%',/,'%',/,
     +    /, '/m {moveto} def /l {lineto} def /r {rlineto} def',
     +    /, '/s {stroke} def /n {newpath} def /c {closepath} def',
     +    /, '/rtext{ dup stringwidth pop -1 div 0 rmoveto show } def',
     +    /, '/ctext{ dup stringwidth pop -2 div 0 rmoveto show } def',
     +    /, '/ltext{show} def /gr{grestore} def /gs{gsave} def',
     +    /, '/tr{translate} def /setc{setrgbcolor} def',/,/,
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
      str(1:20) = 'Cutoff Grade'
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
c Write Tonnage curve:
c
      do icut=1,ncut
            xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,xval)
            yy = resc(tonmin,tonmax,hpymin,hpymax,tons(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.1) then
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
      do icut=1,ncut
            xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,xval)
            yy = resc(grdmin,grdmax,hpymin,hpymax,grade(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.1) then
                  write(lpsout,101) xx,yy
            else
                  write(lpsout,102) xx,yy
            end if
      end do
      write(lpsout,103)
c
c Add a footer to the Postscript plot file:
c
      write(lpsout,999)
 999  format('%END OF POSTSCRIPT FILE',/,'4.166667 4.166667 scale',/,/,
     +       '%%EOF',/,'showpage')
c
c Write the numerical output
c
      open(lpsout,file=outfl2,status='REPLACE')
      write(lpsout,1000)
 1000 format('Numerical Output from GTCURVE',
     +    /, '3',
     +    /, 'Cutoff',
     +    /, 'Tons',
     +    /, 'Grade')
      do icut=1,ncut
         xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
         write(lpsout,1010) xval,tons(icut),grade(icut)
      end do
 1010 format(3(x,f14.8))
      close(lpsout)
c
c Deallocate memory
c
      deallocate(var,stat=test)
c
c Finished:
c
      close(lpsout)
      write(*,9998) VERSION
 9998 format(/' GTCURVE Version: ',f5.3, ' Finished'/)
      stop
 97   stop 'Error in parameter file somewhere'
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
      open(lun,file='gtcurve.par',status='UNKNOWN')
      write(lun,10)
  10  format('                 Parameters for GTCURVE',/,
     +       '                 **********************',/,/,
     +       'START OF PARAMETERS:')

      write(lun,20)
  20  format('data.out                      ',
     +       '-file with data')
      write(lun,30)
  30  format('1   3                         ',
     +       '-  columns for grade and weight')
      write(lun,40)
  40  format('-1.0e21   1.0e21              ',
     +       '-  trimming limits')
      write(lun,50)
  50  format(' 15.0                         ',
     +       '-clipping limit (upper limit)')
      write(lun,60)
  60  format('gtcurve.out                   ',
     +       '-file for numerical output')
      write(lun,70)
  70  format('gtcurve.ps                    ',
     +       '-file for postscript output')
      write(lun,80)
  80  format('25   0.0    3.0               ',
     +       '-cutoff: num, min and max')
      write(lun,90)
  90  format('     0.0    1.0               ',
     +       '-tonnes:      min and max')
      write(lun,100)
 100  format('     0.0    6.0               ',
     +       '-grade:       min and max')
      write(lun,110)
 110  format('Grade - Tonnage Curve         ',
     +       '-title for plot')

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



