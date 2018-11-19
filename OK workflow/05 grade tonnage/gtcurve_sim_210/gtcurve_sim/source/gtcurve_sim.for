      program gtcruve_sim
c-----------------------------------------------------------------------
c
c
c
c
c-----------------------------------------------------------------------
      use msflib
      use gslib_binary

      character*5, parameter    :: VERSION='2.1.0'
      integer, parameter :: MAXLEN=512

      character(len=512) :: datafl,outfl,str,outfl2
      character(len=40)  :: title
      real, allocatable  :: vr(:),wt(:),var(:, :), tons(:,:),grade(:,:),
     +                      avgrade(:),avtons(:),p10g(:),p10t(:),
     +                      p90g(:),p90t(:),sort(:)
      real*8, allocatable :: data_(:, :)

      logical            :: testfl
      integer            :: test, ivar(2)

      type(gsb_info) :: gsb
      logical :: usegsb
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
 9999 format(/' GTCURVE_SIM Version: ',a/)
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
      if(str(1:1).eq.' ') str(1:20) = 'gtcurve_sim.par     '
      inquire(file=str,exist=testfl)
      if(.not.testfl) then
            write(*,*) 'ERROR - the parameter file does not exist,'
            write(*,*) '        check for the file and try again  '
            write(*,*)
            if(str(1:20).eq.'gtcurve_sim.par     ') then
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

      read(lin,*,err=97) nsim
      write(*,*) ' number of realizations = ',nsim

      read(lin,*,err=97) nx,ny,nz
      write(*,*) ' number cells in x, y, and z = ',nx,ny,nz
      nxyz=nx*ny*nz

      read(lin,*,err=97) rp10,rp90
      write(*,*) ' lower and upper pvalues = ',rp10,rp90

      read(lin,'(a)',err=97) datafl
      call chknam(datafl,MAXLEN)
      usegsb = checkGSB(datafl)
      if(usegsb)then
        write(*,*) ' GSB-formatted data file = ',datafl(1:40)
      else 
        write(*,*) ' GSLIB-formatted data file = ',datafl(1:40)
      endif


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
      ncut = ncut + 1

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
      if(usegsb)then
          ivar(1) = ivr
          if(iwt > 0)then
              nvari = 2
              ivar(2) = iwt
          else
              nvari = 1
          end if 
          lin = opengsb(gsb, datafl, 'r', nvari)
      else
          open(lin,file=datafl,status='OLD')
          read(lin,*,err=99)
          read(lin,*,err=99) nvari
          do i=1,nvari
                read(lin,'()',err=99)
          end do
      endif
      allocate(data_(nvari, nxyz),stat=test)
      if(test.ne.0) stop 'Error allocating data_ array'
      allocate(var(nxyz, 2),stat=test)
      if(test.ne.0) stop 'Error allocating var array'
      allocate(vr(nxyz),stat=test)
      if(test.ne.0) stop 'Error allocating vr array'
      allocate(wt(nxyz),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(tons(nsim,ncut),stat=test)
      if(test.ne.0) stop 'Error allocating tons array'
      allocate(grade(nsim,ncut),stat=test)
      if(test.ne.0) stop 'Error allocating grade array'
      allocate(avgrade(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(avtons(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(p10g(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(p10t(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(p90g(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(p90t(ncut),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      allocate(sort(nsim),stat=test)
      if(test.ne.0) stop 'Error allocating wt array'
      var      = 0.0
      vr       = 0.0
      wt       = 0.0
      tons     = 0.0
      grade    = 0.0
      avtons   = 0.0
      avgrade  = 0.0
      p10g     = 0.0
      p10t     = 0.0
      p90g     = 0.0
      p90t     = 0.0
      sort     = 0.0

      n = 0
      do isim=1,nsim
         nd   = 0
         xtwt = 0.0
         if(usegsb)then
            test = gsbread(gsb, data_, isim, ivar(1:nvari))
             var(:, 1) = data_(1, :)
             if(iwt > 0) var(:, 2) = data_(2, :)
         else
             read(lin,*,end=5,err=99) data_
             var(:, 1) = data_(ivr, :)
             if(iwt > 0) var(:, 2) = data_(iwt, :)
         endif 
         if(iwt <= 0) var(:, 2) = 1.0      
         do i=1,nxyz
            ! Variable trimming
            if(var(i, 1).lt.tmin.or.var(i, 1).ge.tmax) cycle
            ! Weight trimming
            if(var(i, 2).le.1.0e-10) cycle
            nd = nd + 1
            vr(nd) = var(i, 1)
            if(vr(nd).gt.gmax) vr(nd) = gmax
            wt(nd) = var(i, 2)
            xtwt = xtwt + wt(nd)
         enddo
         if(nd.le.1) then
            write(*,*) ' ERROR: too few data ',nd
            stop
         endif
         write(*,*)' Working on Realization ',isim,' with # data = ',nd
c
c Get grade tonnage curve:
c
      do icut=1,ncut
         tons(isim,icut)  = 0.0
         grade(isim,icut) = 0.0
         cutoff      = cutmin + real(icut-1)/real(ncut-1) *
     +                              (cutmax-cutmin)
         do i=1,nd
            if(vr(i).ge.cutoff) then
               tons(isim,icut)  = tons(isim,icut)  + wt(i)
               grade(isim,icut) = grade(isim,icut) + vr(i)*wt(i)
            end if
         end do
         grade(isim,icut) = grade(isim,icut) /max(tons(isim,icut),0.001)
         tons(isim,icut)  = tons(isim,icut)  /max(xtwt,      0.001)
         avgrade(icut)    = avgrade(icut) + grade(isim,icut)
         avtons(icut)     = avtons(icut)  + tons(isim,icut)
c         write(*,301) cutoff,tons(isim,icut),grade(isim,icut)
c 301     format('Cutoff ',f8.3,' fraction of tonnes ',f6.4,
c     +                         ' grade ',f12.3)
      end do
c
c End loop over the grade tonnage curve calculation
c
      n = n + 1
      end do
   5  close(lin)
      nsim=n
      avgrade = avgrade / nsim
      avtons  = avtons  / nsim
c
c Calculate the upper and lower confidence limits
c
      p10=int(rp10*nsim+0.5)
      p90=int(rp90*nsim+0.5)
      if(p10.eq.0) p10=1
      if(p10.eq.1) p90=nsim
      do icut=1,ncut
         sort=0.0
         sort=grade(1:nsim,icut)
         call sortem(1,nsim,sort,0,b,c,d,e,f,g,h)
         p10g(icut)=sort(p10)
         p90g(icut)=sort(p90)
         sort=0.0
         sort=tons(1:nsim,icut)
         call sortem(1,nsim,sort,0,b,c,d,e,f,g,h)
         p10t(icut)=sort(p10)
         p90t(icut)=sort(p90)
      enddo
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
     +    /, '%%Creator: GTCURVE_SIM',
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
      do isim=1,nsim
      do icut=1,ncut
            xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,xval)
            yy = resc(tonmin,tonmax,hpymin,hpymax,tons(isim,icut))
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
            yy = resc(grdmin,grdmax,hpymin,hpymax,grade(isim,icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.1) then
                  write(lpsout,101) xx,yy
            else
                  write(lpsout,102) xx,yy
            end if
      end do
      write(lpsout,103)
      end do
c
c Write Tonnage curve:
c
      write(lpsout,'(a)') 'gs 1 0 0 setc 3.0 setlinewidth'
      do icut=1,ncut
            xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,xval)
            yy = resc(tonmin,tonmax,hpymin,hpymax,avtons(icut))
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
c Write Grade curve:
c
      do icut=1,ncut
            xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
            xx = resc(cutmin,cutmax,hpxmin,hpxmax,xval)
            yy = resc(grdmin,grdmax,hpymin,hpymax,avgrade(icut))
            xx = (resc(xmin,xmax,pxmin,pxmax,xx))/pscl
            yy = (resc(ymin,ymax,pymin,pymax,yy))/pscl
            if(icut.eq.1) then
                  write(lpsout,101) xx,yy
            else
                  write(lpsout,102) xx,yy
            end if
      end do
      write(lpsout,103)
      write(lpsout,'(a)') 'gr'
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
      write(lpsout,1000) (nsim+3)*2+1
 1000 format('Numerical Output from GTCURVE_SIM',
     +    /,i4,
     +    /, 'Cutoff')
      write(lpsout,1007) int(rp10*100+0.5),int(rp10*100+0.5),
     +                   int(rp90*100+0.5),int(rp90*100+0.5)
 1007 format('Average Tons'/
     +       'Average Grade'/
     +       'P',i2,' Tons'/
     +       'P',i2,' Grade'/
     +       'P',i2,' Tons'/
     +       'P',i2,' Grade')
      do i=1,nsim
         write(lpsout,1005) i,i
      end do
 1005 format('Tons for realization ',i4
     +    /, 'Grade for realization ',i4)
      do icut=1,ncut
         xval = cutmin + real(icut-1)/real(ncut-1)*(cutmax-cutmin)
         write(lpsout,1010) xval,avtons(icut),avgrade(icut),
     +                      p10t(icut),p10g(icut),p90t(icut),p90g(icut),
     +                     (tons(i,icut),grade(i,icut),i=1,nsim)
      end do
 1010 format(<(nsim+3)*2+1>(x,f14.8))
      close(lpsout)
c
c Deallocate memory
c
      deallocate(var,stat=test)
      deallocate(vr,stat=test)
      deallocate(wt,stat=test)
      deallocate(tons,stat=test)
      deallocate(grade,stat=test)
      deallocate(avgrade,stat=test)
      deallocate(avtons,stat=test)
      deallocate(p10g,stat=test)
      deallocate(p10t,stat=test)
      deallocate(p90g,stat=test)
      deallocate(p90t,stat=test)
      deallocate(sort,stat=test)
c
c Finished:
c
      close(lpsout)
      write(*,9998) VERSION
 9998 format(/' GTCURVE Version: ',a, ' Finished'/)
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
      open(lun,file='gtcurve_sim.par',status='UNKNOWN')
      write(lun,10)
  10  format('              Parameters for GTCURVE_SIM',/,
     +       '              **************************',/,/,
     +       'START OF PARAMETERS:')

      write(lun,15)
  15  format(' 1                            ',
     +       '-number of realizations')
      write(lun,17)
  17  format('  10  10   1                  ',
     +       '-number of cells in x, y, and z')
      write(lun,18)
  18  format('  0.10 0.90                   ',
     +       '-lower and upper pvalues')
      write(lun,20)
  20  format('data.out                      ',
     +       '-file with data (GSB detection')
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
  60  format('gtcurve_sim.out               ',
     +       '-file for numerical output')
      write(lun,70)
  70  format('gtcurve_sim.ps                ',
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






      subroutine sortem(ib,ie,a,iperm,b,c,d,e,f,g,h)
c-----------------------------------------------------------------------
c
c                      Quickersort Subroutine
c                      **********************
c
c This is a subroutine for sorting a real array in ascending order. This
c is a Fortran translation of algorithm 271, quickersort, by R.S. Scowen
c in collected algorithms of the ACM.
c
c The method used is that of continually splitting the array into parts
c such that all elements of one part are less than all elements of the
c other, with a third part in the middle consisting of one element.  An
c element with value t is chosen arbitrarily (here we choose the middle
c element). i and j give the lower and upper limits of the segment being
c split.  After the split a value q will have been found such that 
c a(q)=t and a(l)<=t<=a(m) for all i<=l<q<m<=j.  The program then
c performs operations on the two segments (i,q-1) and (q+1,j) as follows
c The smaller segment is split and the position of the larger segment is
c stored in the lt and ut arrays.  If the segment to be split contains
c two or fewer elements, it is sorted and another segment is obtained
c from the lt and ut arrays.  When no more segments remain, the array
c is completely sorted.
c
c
c INPUT PARAMETERS:
c
c   ib,ie        start and end index of the array to be sorteda
c   a            array, a portion of which has to be sorted.
c   iperm        0 no other array is permuted.
c                1 array b is permuted according to array a
c                2 arrays b,c are permuted.
c                3 arrays b,c,d are permuted.
c                4 arrays b,c,d,e are permuted.
c                5 arrays b,c,d,e,f are permuted.
c                6 arrays b,c,d,e,f,g are permuted.
c                7 arrays b,c,d,e,f,g,h are permuted.
c               >7 no other array is permuted.
c
c   b,c,d,e,f,g,h  arrays to be permuted according to array a.
c
c OUTPUT PARAMETERS:
c
c    a      = the array, a portion of which has been sorted.
c
c    b,c,d,e,f,g,h  =arrays permuted according to array a (see iperm)
c
c NO EXTERNAL ROUTINES REQUIRED:
c
c-----------------------------------------------------------------------
      dimension a(*),b(*),c(*),d(*),e(*),f(*),g(*),h(*)
c
c The dimensions for lt and ut have to be at least log (base 2) n
c
      integer   lt(64),ut(64),i,j,k,m,p,q
c
c Initialize:
c
      j     = ie
      m     = 1
      i     = ib
      iring = iperm+1
      if (iperm.gt.7) iring=1
c
c If this segment has more than two elements  we split it
c
 10   if (j-i-1) 100,90,15
c
c p is the position of an arbitrary element in the segment we choose the
c middle element. Under certain circumstances it may be advantageous
c to choose p at random.
c
 15   p    = (j+i)/2
      ta   = a(p)
      a(p) = a(i)
      go to (21,19,18,17,16,161,162,163),iring
 163     th   = h(p)
         h(p) = h(i)
 162     tg   = g(p)
         g(p) = g(i)
 161     tf   = f(p)
         f(p) = f(i)
 16      te   = e(p)
         e(p) = e(i)
 17      td   = d(p)
         d(p) = d(i)
 18      tc   = c(p)
         c(p) = c(i)
 19      tb   = b(p)
         b(p) = b(i)
 21   continue
c
c Start at the beginning of the segment, search for k such that a(k)>t
c
      q = j
      k = i
 20   k = k+1
      if(k.gt.q)     go to 60
      if(a(k).le.ta) go to 20
c
c Such an element has now been found now search for a q such that a(q)<t
c starting at the end of the segment.
c
 30   continue
      if(a(q).lt.ta) go to 40
      q = q-1
      if(q.gt.k)     go to 30
      go to 50
c
c a(q) has now been found. we interchange a(q) and a(k)
c
 40   xa   = a(k)
      a(k) = a(q)
      a(q) = xa
      go to (45,44,43,42,41,411,412,413),iring
 413     xh   = h(k)
         h(k) = h(q)
         h(q) = xh
 412     xg   = g(k)
         g(k) = g(q)
         g(q) = xg
 411     xf   = f(k)
         f(k) = f(q)
         f(q) = xf
 41      xe   = e(k)
         e(k) = e(q)
         e(q) = xe
 42      xd   = d(k)
         d(k) = d(q)
         d(q) = xd
 43      xc   = c(k)
         c(k) = c(q)
         c(q) = xc
 44      xb   = b(k)
         b(k) = b(q)
         b(q) = xb
 45   continue
c
c Update q and search for another pair to interchange:
c
      q = q-1
      go to 20
 50   q = k-1
 60   continue
c
c The upwards search has now met the downwards search:
c
      a(i)=a(q)
      a(q)=ta
      go to (65,64,63,62,61,611,612,613),iring
 613     h(i) = h(q)
         h(q) = th
 612     g(i) = g(q)
         g(q) = tg
 611     f(i) = f(q)
         f(q) = tf
 61      e(i) = e(q)
         e(q) = te
 62      d(i) = d(q)
         d(q) = td
 63      c(i) = c(q)
         c(q) = tc
 64      b(i) = b(q)
         b(q) = tb
 65   continue
c
c The segment is now divided in three parts: (i,q-1),(q),(q+1,j)
c store the position of the largest segment in lt and ut
c
      if (2*q.le.i+j) go to 70
      lt(m) = i
      ut(m) = q-1
      i = q+1
      go to 80
 70   lt(m) = q+1
      ut(m) = j
      j = q-1
c
c Update m and split the new smaller segment
c
 80   m = m+1
      go to 10
c
c We arrive here if the segment has  two elements we test to see if
c the segment is properly ordered if not, we perform an interchange
c
 90   continue
      if (a(i).le.a(j)) go to 100
      xa=a(i)
      a(i)=a(j)
      a(j)=xa
      go to (95,94,93,92,91,911,912,913),iring
 913     xh   = h(i)
         h(i) = h(j)
         h(j) = xh
 912     xg   = g(i)
         g(i) = g(j)
         g(j) = xg
 911     xf   = f(i)
         f(i) = f(j)
         f(j) = xf
   91    xe   = e(i)
         e(i) = e(j)
         e(j) = xe
   92    xd   = d(i)
         d(i) = d(j)
         d(j) = xd
   93    xc   = c(i)
         c(i) = c(j)
         c(j) = xc
   94    xb   = b(i)
         b(i) = b(j)
         b(j) = xb
   95 continue
c
c If lt and ut contain more segments to be sorted repeat process:
c
 100  m = m-1
      if (m.le.0) go to 110
      i = lt(m)
      j = ut(m)
      go to 10
 110  continue
      return
      end



