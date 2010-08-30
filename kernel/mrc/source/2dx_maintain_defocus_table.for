      PROGRAM DEFTABL
C
      REAL rfield(7,7)
      character*200 cfile1,cfile2,cline1
C
      read(*,*)cfile1
      read(*,*)cfile2
      read(*,*)ixpos,iypos
      read(*,*)rdef
C
      write(*,'(''Opening for read '',A50)')cfile1
      open(11,FILE=cfile1,STATUS='OLD',ERR=900)
      write(*,'(''Opening for write '',A50)')cfile2
      open(12,FILE=cfile2,STATUS='NEW',ERR=910)
C
      read(11,'(A80)')cline1
      write(12,'(A80)')cline1(1:80)
C
      do i=1,7
        do j=1,7
          rfield(i,j)=0.0
        enddo
      enddo
C
      do j=1,7
        read(11,'(A1)') cline1(1:1)
        read(11,'(7F11.1)',ERR=920,END=800) (rfield(i,j), i=1,7)
      enddo
C
 800  continue
C
      rfield(ixpos,iypos)=rdef
C
      do j=1,7
        write(12,'('' '')') 
        write(12,'(7F11.1)') (rfield(i,j), i=1,7)
      enddo
      write(12,'('' '')') 
C
      goto 9999
C
 900  continue
        write(*,'(''ERROR in 2dx_maintain_defocus_table.for during '',
     .    ''input file open file 1'')')
        stop
C
 910  continue
        write(*,'(''ERROR in 2dx_maintain_defocus_table.for during '',
     .    ''output file open file 2'')')
        stop
C
 920  continue
        write(*,'(''ERROR in 2dx_maintain_defocus_table.for during '',
     .    ''reading from input file 1'')')
        stop
C
 9999 continue
C
      close(11)
      close(12)
C
      STOP
      END

