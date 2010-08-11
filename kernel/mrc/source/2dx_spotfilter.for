      PROGRAM SPOTSE
C
C     Author: Henning.
C
C     (C) 2dx.org
C
C     version of Sept. 7, 2007
C
C
      PARAMETER(IMAXLI=50,IMAXSP=5000)
      integer ISPOTL(-IMAXLI:IMAXLI,-IMAXLI:IMAXLI)
      integer IOXSPT(IMAXSP),IOYSPT(IMAXSP)
      character * 100 CSPOT1,CNAME1,CNAME2,CNAME3,CNAME4
c
      write(*,'('' Input first (good) lattice '')')
      read(*,*,ERR=800) rux,ruy,rvx,rvy
      write(*,'(''read: '',F9.3,'','',F9.3,'','',F9.3,
     1          '','',F9.3)')
     2    rux,ruy,rvx,rvy
C
      write(*,'('' Input second (bad) lattice '')')
      read(*,*,ERR=800) rbux,rbuy,rbvx,rbvy
      write(*,'(''read: '',F9.3,'','',F9.3,'','',F9.3,
     1          '','',F9.3)')
     2    rbux,rbuy,rbvx,rbvy
C
      write(*,'('' Input maximal indices '')')
      read(*,'(I8)',ERR=800) imaxn
      if(imaxn.gt.IMAXLI)then
        write(*,'(''ERROR: maximal '',I4,'' allowed.'')') 
     1    IMAXLI
        goto 900
      endif
      write(*,'(''read: '',I6)') imaxn
C
      write(*,'('' Input minimal distance '')')
      read(*,*,ERR=800) rmindi
      write(*,'(''read: '',F10.3)') rmindi
C
      write(*,
     1'('' Input 0=do nothing,1=do all,2=only GOODSPOT.spt'')')
      read(*,'(I8)',ERR=800) imodsp
      write(*,'(''read: '',I6)') imodsp
C
      write(*,'('' Input name for image spotlist '')')
      read(*,'(A)',ERR=800) CSPOT1
      write(*,'(''read: '',A40)') CSPOT1
C
      write(*,'('' Input name for good spotlist '')')
      read(*,'(A)',ERR=800) CNAME1
      write(*,'(''read: '',A40)') CNAME1
C
      write(*,'('' Input name for bad spotlist '')')
      read(*,'(A)',ERR=800) CNAME2
      write(*,'(''read: '',A40)') CNAME2
C
      write(*,'('' Input name for count file '')')
      read(*,'(A)',ERR=800) CNAME3
      write(*,'(''read: '',A40)') CNAME3
C
      write(*,'('' Input name for corrected spot-file '')')
      read(*,'(A)',ERR=800) CNAME4
      write(*,'(''read: '',A40)') CNAME4
C
      ionspt = 0
      iremsp = 0
      icgood = 0
      icbad = 0
C
      if(imodsp.eq.1)then
C-------read in original spotlist
C
        open(9,FILE=CSPOT1,STATUS='OLD',ERR=810)
C
 100    continue
          ionspt=ionspt + 1
          read(9,*,ERR=150,END=150)
     1         IOXSPT(ionspt),IOYSPT(ionspt)
          if(IOXSPT(ionspt).lt.0)then
            IOXSPT(ionspt)=-IOXSPT(ionspt)
            IOYSPT(ionspt)=-IOYSPT(ionspt)
          endif
          goto 100
 150    continue
        close(9)
      endif
C
      do 240 ix=0,imaxn
        do 230 iy=-imaxn,imaxn
C 
          if(ix.eq.0 .and. iy.le.0)then
            goto 230
          endif
C
          ISPOTL(ix,iy)=1
          icgood = icgood + 1
C
C---------calculate position of this spot
C
          rxgpos = ix*rux + iy*rvx
          rygpos = ix*ruy + iy*rvy
C
          do 220 ibx=-imaxn,imaxn
            do 210 iby=-imaxn,imaxn
C
C-------------calculate position of this spot
C
              rxbpos = ibx*rbux + iby*rbvx
              rybpos = ibx*rbuy + iby*rbvy
C
C-------------calculate distance between spots
C
              rdist = sqrt((rxgpos-rxbpos)**2 +
     1                     (rygpos-rybpos)**2)
C
C                 write(*,'(''rdist ='',F9.3)')rdist 
C                write(*,'(''rxgpos='',F9.3)')rxgpos
C                write(*,'(''rygpos='',F9.3)')rygpos
C                write(*,'(''rxbpos='',F9.3)')rxbpos
C                write(*,'(''rybpos='',F9.3)')rybpos
C                write(*,'(''ix='',I6)')ix
C                write(*,'(''iy='',I6)')iy
C                write(*,'(''ibx='',I6)')ibx
C                write(*,'(''iby='',I6)')iby
              if(rdist.le.rmindi)then
                ISPOTL(ix,iy)=2
                icbad  = icbad  + 1
                icgood = icgood - 1
                goto 225
              endif
C
 210        continue
 220      continue
C
 225      continue
C
 230    continue
 240  continue
C
C-----write out the good spotlist and the bad spotlist
C
      if(imodsp.GE.1)open(10,FILE=CNAME1,STATUS='NEW',ERR=840)
      if(imodsp.EQ.1)open(11,FILE=CNAME2,STATUS='NEW',ERR=840)
C
      do 320 ix=0,imaxn
        do 310 iy=-imaxn,imaxn
C
          if(ISPOTL(ix,iy).eq.1)then
            if(imodsp.GE.1)write(10,'(I6,'','',I6)')ix,iy
          elseif(ISPOTL(ix,iy).eq.2)then
            if(imodsp.eq.1)then
              write(11,'(I6,'','',I6)')ix,iy
              do 305 in=1,ionspt-1
                if(IOXSPT(in).eq.ix .and.
     1             IOYSPT(in).eq.iy ) then
                   IOXSPT(in) = 0
                   IOYSPT(in) = 0
                   iremsp = iremsp + 1
                endif
 305          continue
            endif
          endif
C
 310    continue
 320  continue
C
      if(imodsp.GE.1)close(10)
C
      if(imodsp.eq.1)then
        close(11)
C
        open(12,FILE=CNAME4,STATUS='NEW',ERR=820)
        do 410 in=1,ionspt-1
          if(IOXSPT(in).ne.0 .or.
     1       IOYSPT(in).ne.0 ) then
            write(12,'(I6,'','',I6)')
     1       IOXSPT(in),IOYSPT(in)
          endif
 410    continue
        close(12)
      endif
C
      open(12,FILE=CNAME3,STATUS='NEW',ERR=800)
C
      write(12,'(I8)')icgood
      write(12,'(I8)')icbad
      write(12,'(I8)')ionspt
      irest = ionspt-iremsp
      write(12,'(I8)')irest
C
      close(12)
C
      goto 900
c
 800  continue
        write(*,'('' SPOTSE: ERROR !'')')
        stop
 810  continue
        write(*,'('' SPOTSE: ERROR opening spotlist.'')')
        stop
 820  continue
        write(*,'('' SPOTSE: ERROR opening corr. spot file.'')')
        stop
 840  continue
        write(*,'('' SPOTSE: ERROR opening good and bad spot file.'')')
        stop
 900  continue
        write(*,'('' SPOTSE: normal termination.'')')
C
      STOP
      END
