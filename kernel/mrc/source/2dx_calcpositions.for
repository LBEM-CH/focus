      PROGRAM HCALCPOSITIONS
C
      character * 200 czeile,coutfile,crunfile,cinimage,cname1
c
      read(*,*)ibigsize
      write(*,'('' ibigsize = '',I8)')ibigsize
C
      read(*,*)ismallsize
      write(*,'('' ismallsize = '',I8)')ismallsize
C
      read(*,*)inumx,inumy
      write(*,'('' inumx = '',I8,'' , inumy = '',I8)')inumx,inumy
C
      read(*,*)imode
      write(*,'('' imode = '',I8)')imode
C
      read(*,'(A)')cinimage
      write(*,'('' cinimage = '',A60)')cinimage
C
      read(*,'(A)')coutfile
      write(*,'('' coutfile = '',A60)')coutfile
C
      read(*,'(A)')crunfile
      write(*,'('' crunfile = '',A60)')crunfile
C
      open(11,FILE=coutfile,STATUS='NEW',ERR=900)
      open(12,FILE=crunfile,STATUS='NEW',ERR=900)
C
      write(11,
     1'(''   X   Y    anfX    anfY    endX    endY    cenX    cenY'')')
C
      write(12,'(''#'')')
      write(12,'(''set image = '',A60)')cinimage
      write(12,'(''#'')')
C
      do i = 1,inumx
        do j = 1,inumy
          ianfx = ((i-1)*(ibigsize-ismallsize))/(inumx-1)
          ianfy = ((inumy-j)*(ibigsize-ismallsize))/(inumy-1)
          if(ianfx.lt.1)ianfx=1
          if(ianfy.lt.1)ianfy=1
          if(ianfx.gt.ibigsize-ismallsize)ianfx=ibigsize-ismallsize
          if(ianfy.gt.ibigsize-ismallsize)ianfy=ibigsize-ismallsize
          iendx = ianfx + ismallsize - 1
          iendy = ianfy + ismallsize - 1
          icenx = (iendx + ianfx)/2 + 1
          iceny = (iendy + ianfy)/2 + 1
C
          write(11,'(2I4,6I8)')i,j,ianfx,ianfy,iendx,iendy,icenx,iceny
C
          write(12,'(''#'')')
          write(12,'(''#######################################'',
     1               ''#######################################'')')
          write(12,
     1     '(''${proc_2dx}/lin "Cutting position '',I1,'','',
     .       I1,'' at '',4I7,''"'')')i,j,ianfx,iendx,ianfy,iendy
          write(12,'(''#######################################'',
     1               ''#######################################'')')
          write(12,'(''#'')')
          write(12,'(''\rm -f SCRATCH/TMP.mrc'')')
          write(12,'(''#'')')
          write(12,
     1    '(''\rm -f CUT/${image}_'',I1,''_'',I1,''.mrc'')')i,j
          write(12,'(''#'')')
          write(12,'(''${bin_2dx}/labelh.exe << eot'')')
          write(12,'(''${image}.mrc'')')
          write(12,'(''1'')')
          write(12,'(''SCRATCH/TMP.mrc'')')
          write(12,'(4I6)')ianfx,iendx,ianfy,iendy
          write(12,'(''eot'')')
          write(12,'(''#'')')
          write(cname1,'(''CUT/${image}_'',I1,''_'',I1,''.mrc'')')i,j
          call shorten(cname1,k)
          write(12,'(''\rm -f '',A)')cname1(1:k)
          write(12,'(''#'')')
          if(ismallsize.eq.2048) then
            write(12,'(''${bin_2dx}/labelh.exe << eot'')')
            write(12,'(''SCRATCH/TMP.mrc'')')
            write(12,'(''4'')')
            write(12,'(A)')cname1(1:k)
            write(12,'(''2,2'')')
            write(12,'(''eot'')')
          else
            write(12,'(''\mv -f SCRATCH/TMP.mrc '',A)')cname1(1:k)
          endif
          write(12,'(''#'')')
          if(imode.eq.1 .or. (i.eq.4 .and. j.eq.4))then
            write(12,'(''echo "# IMAGE: '',A,'' <Tile '',I1,'','',I1,
     .            ''>" >> LOGS/${scriptname}.results'')'')')cname1(1:k),i,j
          endif
          write(12,'(''#'')')
        enddo
      enddo
c
      write(12,'(''#'')')
      write(12,'(''\rm -f SCRATCH/TMP.mrc'')')
      write(12,'(''#'')')
      write(12,'(''#######################################'',
     1           ''#######################################'')')
      write(12,
     1 '(''${proc_2dx}/lin "Done."'')')
      write(12,'(''#######################################'',
     1           ''#######################################'')')
      write(12,'(''#'')')
c
      close(11)
      close(12)
      goto 999
C
 900  continue
      stop "ERROR in hcalcpositions."
 999  continue
C
      END
C
c==========================================================
c
      SUBROUTINE shorten(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CTMP2=' '
C
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C

             
