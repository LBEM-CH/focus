      PROGRAM DATRANS
C
      CHARACTER*200 CLINE1,CLINE2,CLINE3
      CHARACTER*80 CDATA(160)
C
      open(10,FILE='DATAFILE.dat',STATUS='OLD')
      do i=1,160
        READ(10,'(A)')CDATA(i)
      enddo
      close(10)
C
      open(11,FILE='2dx_image.cfg',STATUS='NEW')
C
      write(11,'(''set imagenumber = "'',A10,''"'')')CDATA(11)
      write(11,'(''set imagename = "'',A40,''"'')')CDATA(12)
      write(11,'(''set nonmaskimagename = "'',A40,''"'')')CDATA(12)
      write(11,'(''set imagesidelength = "'',A40,''"'')')CDATA(14)
      write(11,'(''set magnification = "'',A40,''"'')')CDATA(15)
      write(11,'(''set stepdigitizer = "'',A40,''"'')')CDATA(16)
      write(11,'(''set TLTAXIS = "'',A40,''"'')')CDATA(17)
      write(11,'(''set TLTANG = "'',A40,''"'')')CDATA(18)
      write(11,'(''set TLTAXA = "'',A40,''"'')')CDATA(19)
      write(11,'(''set TAXA = "'',A40,''"'')')CDATA(20)
      write(11,'(''set TANGL = "'',A40,''"'')')CDATA(25)
      write(11,'(''set beamtilt = "'',A40,''"'')')CDATA(111)
      write(11,'(''set realcell = "'',A40,''"'')')CDATA(33)
      write(11,'(''set realang = "'',A40,''"'')')CDATA(112)
      write(11,'(''set realcell_local = "'',A40,''"'')')CDATA(33)
      write(11,'(''set realang_local = "'',A40,''"'')')CDATA(112)
      write(11,'(''set lattice = "'',A40,''"'')')CDATA(32)
      write(11,'(''set secondlattice = "'',A40,''"'')')CDATA(35)
      write(11,'(''set defocus = "'',A40,''"'')')CDATA(34)
      write(11,'(''set defocusbackup = "'',A40,''"'')')CDATA(34)
      write(11,'(''set holeb = "'',A40,''"'')')CDATA(41)
      write(11,'(''set maska = "'',A10,''"'')')CDATA(42)
      write(11,'(''set maskb01 = "'',A10,''"'')')CDATA(43)
      write(11,'(''set maskb02 = "'',A10,''"'')')CDATA(44)
      write(11,'(''set maskb03 = "'',A10,''"'')')CDATA(45)
      write(11,'(''set SYN_maska = "'',A10,''"'')')CDATA(68)
      write(11,'(''set SYN_maskb = "'',A10,''"'')')CDATA(69)
      write(11,'(''set boxa1 = "'',A10,''"'')')CDATA(48)
      write(11,'(''set boxb1 = "'',A10,''"'')')CDATA(49)
C      write(11,'(''set boxa2 = "'',A10,''"'')')CDATA()
C      write(11,'(''set boxb2 = "'',A10,''"'')')CDATA()
      write(11,'(''set quadrada = "'',A40,''"'')')CDATA(50)
      write(11,'(''set quadradb = "'',A40,''"'')')CDATA(51)
      write(11,'(''set SYN_quadrada = "'',A40,''"'')')CDATA(70)
      write(11,'(''set SYN_quadradb = "'',A40,''"'')')CDATA(71)
      write(11,'(''set facthresha = "'',A40,''"'')')CDATA(52)
      write(11,'(''set facthreshb = "'',A40,''"'')')CDATA(53)
      write(11,'(''set SYN_Bfact1 = "'',A40,''"'')')CDATA(64)
      write(11,'(''set SYN_Bfact2 = "'',A40,''"'')')CDATA(65)
      write(11,'(''set SYN_facthresha = "'',A40,''"'')')CDATA(72)
      write(11,'(''set quadpreda = "'',A40,''"'')')CDATA(54)
      write(11,'(''set quadpredb = "'',A40,''"'')')CDATA(55)
      write(11,'(''set SYN_quadpreda = "'',A40,''"'')')CDATA(74)
      write(11,'(''set SYN_quadpredb = "'',A40,''"'')')CDATA(75)
      write(11,'(''set RESMAX = "'',A40,''"'')')CDATA(131)
      write(11,'(''set RESMIN = "'',A40,''"'')')CDATA(130)
      write(11,'(''set ALAT = "'',A40,''"'')')CDATA(147)
      write(11,'(''set tempfac = "'',A40,''"'')')CDATA(132)
      write(11,'(''set CS = "'',A3,''"'')')CDATA(160)
      write(11,'(''set KV = "'',A10,''"'')')CDATA(160)(5:15)
      write(11,'(''set radlim = "'',A40,''"'')')CDATA(83)
      write(11,'(''set phaori = "'',A40,''"'')')CDATA(103)
      write(11,'(''set phaoriFouFilter = "'',A40,''"'')')CDATA(103)
C      write(11,'(''set rot90 = "'',A40,''"'')')
C      write(11,'(''set rot180 = "'',A40,''"'')')CDATA()
      if(CDATA(110)(1:1).eq.'0')then
        write(11,'(''set revhk = "n"'')')
      else
        write(11,'(''set revhk = "y"'')')
      endif
C      write(11,'(''set revhnd = "'',A40,''"'')')CDATA()
      write(11,'(''set ctfrev = "n"'')')
C      write(11,'(''set revxsgn = "'',A40,''"'')')CDATA()
C 
      close(11)
      STOP
C
      END

