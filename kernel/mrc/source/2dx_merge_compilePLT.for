       PROGRAM COMPILEPLT
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
C This program will create a cshell script that calls pltiltk.for
C
      character*200 cname1,cname2,cname3,cname4
      character*200 cdir,cprocdir,cbindir,ctmpdir
      character CROT90,CROT180
      character*80 cspcgrp,crealcell,CBMTLT,CPHORI,CIMAGENAME,CTITLE
      character*80 CIMAGENUMBER,CLATTICE
      character*200 CFILE1,cline
      character*1 CNREFOUT,CNSHFTIN
      integer*8 imnum(10000)
C
      write(*,'('':2dx_merge_compilePLT - '',
     .    ''compiling the plotting script'')')
C
      write(*,'(/,''input name of results output file'')')
      read(*,'(A)')CFILE1
C
      write(*,'(/,''input name of directory with procs'')')
      read(*,'(A)')cprocdir
      call shorten(cprocdir,k)
      write(*,'(A)')cprocdir(1:k)
C
      write(*,'(/,''input name of directory with executables'')')
      read(*,'(A)')cbindir
      call shorten(cbindir,k)
      write(*,'(A)')cbindir(1:k)
C
      write(*,'(/,''input name of file with directory info'')')
      read(*,'(A)')cname1
      call shorten(cname1,k)
      write(*,'(A)')cname1(1:k)
C
      write(*,'(/,''input name of script file for output'')')
      read(*,'(A)')cname2
      call shorten(cname2,k)
      write(*,'(A)')cname2(1:k)
C
      write(*,'(/,''input switch for genref (1=y,0=n)'')')
      read(*,*)igenref
      write(*,'(I6)')igenref
C
      write(*,'(/,''input switch for shftin (1=y,0=n)'')')
      read(*,*)ishftin
      write(*,'(I6)')ishftin
C
      write(*,'(/,''input space group'')')
      read(*,'(A)')cspcgrp
      write(*,'(A10)')cspcgrp
C
      write(*,'(/,''input real cell'')')
      read(*,'(A)')crealcell
      write(*,'(A40)')crealcell
C
      write(*,'(/,''input real ang'')')
      read(*,*)realang
      write(*,'(F12.3)')realang
C
      write(*,'(/,''input ALAT'')')
      read(*,*)RALAT
      write(*,'(F12.3)')RALAT
C
      write(*,'(/,''input MergeIQMax'')')
      read(*,*)ImergeIQMAX
      write(*,'(I3)')ImergeIQMAX
C
      write(*,'(/,''input MergeHKMax'')')
      read(*,*)ImergeHKMAX
      write(*,'(I3)')ImergeHKMAX
C
      write(*,'(/,''input IVERBOSE'')')
      read(*,*)IVERBOSE
      write(*,'(I3)')IVERBOSE
C
      write(*,'(/,''input PFACAMP'')')
      read(*,*)RFACAMP
      write(*,'(F12.3)')RFACAMP
C
      write(*,'(/,''input TANGLST'')')
      read(*,*)TANGLST
      write(*,'(F12.3)')TANGLST
C
      write(*,'(/,''input TANGLMAX'')')
      read(*,*)TANGLMAX
      write(*,'(F12.3)')TANGLMAX
C
      write(*,'(/,''input IMQLABEL'')')
      read(*,*)IMQLABEL
      write(*,'(I3)')IMQLABEL
C
      write(*,'(/,''input RMAX'')')
      read(*,*)RMAX
      write(*,'(F12.3)')RMAX
C
      write(*,'(/,''input IQMAX'')')
      read(*,*)IQMAX
      write(*,'(I3)')IQMAX
C
      write(*,'(/,''input RGOOD'')')
      read(*,*)RGOOD
      write(*,'(F12.3)')RGOOD
C
      open(10,FILE=cname1,STATUS='OLD',ERR=900)
C
      open(11,FILE=cname2,STATUS='NEW',ERR=900)
C
      write(11,'(''#!/bin/csh -ef'')')
      write(11,'(''#'')')
C
      if(igenref.eq.1)then
        write(11,'(''echo dummy > APH/REF1.hkl'')')
        write(11,'(''rm -f APH/REF*.hkl'')')
        write(11,'(''#'')')
      endif
C
      if(inshftin.eq.1)then
        write(11,'(''echo dummy > PRJ/HKLAPH1.prj'')')
        write(11,'(''rm -f PRJ/HKLAPH*.prj'')')
        write(11,'(''#'')')
      endif
C
      call shorten(cprocdir,k3)
      write(11,'(''set proc_2dx = "'',A,''"'')')cprocdir(1:k3)
      call shorten(cbindir,k3)
      write(11,'(''set bin_2dx = "'',A,''"'')')cbindir(1:k3)
      write(11,'(''#'')')
      call shorten(cspcgrp,k)
      write(11,'(''set spcgrp = "'',A,''"'')')cspcgrp(1:k)
      call shorten(crealcell,k)
      write(11,'(''set realcell = "'',A,''"'')')crealcell(1:k)
      write(cline,'(F12.3)')RALAT
      call shortshrink(cline,k)
      write(11,'(''set ALAT = "'',A,''"'')')cline(1:k)
      write(cline,'(F12.3)')realang
      call shortshrink(cline,k)
      write(11,'(''set realang = "'',A,''"'')')cline(1:k)
      write(11,'(''set IAQP2 = 0'')')
      write(11,'(''set IVERBOSE = "'',I1,''"'')')IVERBOSE
      write(cline,'(F12.3)')RFACAMP
      call shortshrink(cline,k)
      write(11,'(''set RFACAMP = "'',A,''"'')')cline(1:k)
      write(11,'(''#'')')
      write(cline,'(F12.3)')TANGLST
      call shortshrink(cline,k)
      write(11,'(''set TANGLST = "'',A,''"'')')cline(1:k)
      write(cline,'(F12.3)')TANGLMAX
      call shortshrink(cline,k)
      write(11,'(''set TANGLMAX = "'',A,''"'')')cline(1:k)
      write(cline,'(I3)')IMQLABEL
      call shortshrink(cline,k)
      write(11,'(''set IMQLABEL = "'',A,''"'')')cline(1:k)
      write(cline,'(F12.3)')RMAX
      call shortshrink(cline,k)
      write(11,'(''set RMAX = "'',A,''"'')')cline(1:k)
      write(cline,'(I3)')IQMAX
      call shortshrink(cline,k)
      write(11,'(''set IQMAX = "'',A,''"'')')cline(1:k)
      write(cline,'(F12.3)')RGOOD
      call shortshrink(cline,k)
      write(11,'(''set RGOOD = "'',A,''"'')')cline(1:k)
      write(11,'(''#'')')
      write(11,'(''set LOGOUTPUT = F'')')
      write(11,'(''setenv OMP_NUM_THREADS 4'')')
      write(11,'(''#'')')
C
      write(11,'(''${bin_2dx}/2dx_pltiltk.exe << eot'')')
C
      write(11,'(''Tilt Angle Distribution Plot'')')
      write(11,'(''${TANGLST},${TANGLMAX}'')')
      write(11,'(''${IMQLABEL}'')')
      write(11,'(''${RMAX},${IQMAX}'')')
      write(11,'(''${RGOOD}'')')
C
      write(11,'(''${spcgrp} 0 F F 0 ${realcell} ${ALAT} ${realang} 0 15 '',
     .   ''!ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF'')')
C
      imcount = 0
C
 100  continue
C
        read(10,'(A)',END=200)ctmpdir
        call shorten(ctmpdir,k)
        if(ctmpdir(1:1).eq.'/')then
          write(cdir,'(A)')ctmpdir(1:k)
        else
          write(cdir,'(''../'',A)')ctmpdir(1:k)
        endif
        call shorten(cdir,k)
        write(cname3,'(A,''/2dx_image.cfg'')')cdir(1:k)
        write(*,'(''opening '',A)')cname3
        open(12,FILE=cname3,STATUS='OLD',ERR=910)
C
        call cgetline(CIMAGENAME,"imagename")
        call cgetline(CIMAGENUMBER,"imagenumber")
        imcount=imcount+1
        read(CIMAGENUMBER,*)imnum(imcount)
C       write(*,'(''::imagenumber read = '',I10)')imnum(imcount)
        if(imcount.gt.1)then
          do i=1,imcount-1
            if(imnum(i).eq.imnum(imcount))then
              call shorten(CIMAGENAME,k)
              write(*,'(''WARNING: Imagenumber '',I10,
     .          '' appears twice, here for image '',A)')imnum(i),CIMAGENAME(1:k)
            endif
          enddo
        endif
C
        write(CTITLE(1:40),'('' Merging '')')
        call rgetline(RESMAX,"RESMAX")
        call rgetline(RESMIN,"RESMIN")
C
C        RESMIN=1000.0
C        RESMAX=2.0
C
        call rgetline(RCS,"CS")
        call rgetline(RKV,"KV")
        call cgetline(CBMTLT,"beamtilt")
        read(CBMTLT,*)RTX,RTY
        call rgetline(RTAXA,"TAXA")
        call rgetline(RTANGL,"TANGL")
        call cgetline(CPHORI,"phaori")
        read(CPHORI,*)RPHAORIH,RPHAORIK
        call rgetline(RZWIN,"zstarwin")
        if(imcount.eq.1)then
C---------First film is used as is, without rescaling
          RSCL=1.0
        else
C---------RSCL=0.0 means scaling is automatic for following datasets
          RSCL=0.0
        endif
        call igetline(IROT90,"rot90")
        call igetline(IROT180,"rot180")
        call igetline(IREVHK,"revhk")
        call igetline(ICTFREV,"ctfrev")
        call igetline(IREVHND,"revhnd")
        call igetline(IREVXSGN,"revxsgn")
        close(12)
C
        write(11,'(A10,A40)')CIMAGENUMBER(1:10),CTITLE(1:40)
        call shorten(cdir,k1)
        call shortshrink(CIMAGENAME,k2)
        write(cname4,'(A,''/APH/image_ctfcor_ctf.aph'')')
     .    cdir(1:k1),CIMAGENAME(1:k2)
        call shortshrink(cname4,k1)
        write(11,'(A)')cname4(1:k1)
        write(11,'(''  F'')')
        write(11,'(2F12.3,'' 0'',40X,''! TAXA,TANGL,IORIGT'')')
     .     RTAXA,RTANGL
C
        write(11,'(2F10.3,'' 0.0 '',F10.3,'' 0 '',F9.5,6I2,
     .     ''  ! OH,OK,STEP,WIN,SGNXCH,SCL,R180,RHK,'',
     .      ''CTFREV,ROT90,REVHND,REVSGN'')')
     .     RPHAORIH,RPHAORIK,RZWIN,RSCL,IROT180,IREVHK,ICTFREV,IROT90,
     .    IREVHND,IREVXSGN
C
        write(11,'(4F12.3,''                  ! cs,kv,tx,ty'')')
     .    RCS,RKV,RTX,RTY
        write(11,'(2F12.3,42X,''! resolution limits'')')
     .    RESMIN,RESMAX
C
        goto 100
C
 200  continue
C
      write(11,'(''        -1'')')
      write(11,'(''eot'')')
      write(11,'(''#'')')
C
      close(10)
      close(11)
C
      goto 999
C
 900  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in 2dx_merge_compilePLT'')')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        stop
C
 910  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on directory file open '',
     .     ''in 2dx_merge_compilePLT'')')
        write(*,'('':: Could not open: '',A)')cname3(1:k)
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        stop
C
 999  continue
      stop
      end

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
c==========================================================
c
      SUBROUTINE SHORTSHRINK(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 200 CZEIL2
      CTMP2=' '
C
C-----find the leading spaces and remove them
C
      ilen=len(czeile)
      k=ilen
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)')CZEILE(J:k)
      WRITE(CZEILE(1:k),'(A)')CZEIL2(1:k)
C
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
c==========================================================
c
      SUBROUTINE igetline(ival,cname)
C
      CHARACTER * (*) cname
      character*200 cline
C
      call shorten(cname,k)
C
      rewind(12)
C
 100  continue
C 
        read(12,'(A)',END=900,ERR=900)cline
        if(cline(1:3).ne."set") goto 100
        if(cline(5:4+k).ne.cname(1:k)) goto 100
        if(cline(5+k:5+k).ne." ") goto 100
C
      call shorten(cline,l)
C      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cline(9+k:l-1)
      if(cline(9+k:9+k).eq."n")then
        ival=0
        goto 999
      endif
      if(cline(9+k:9+k).eq."y")then
        ival=1
        goto 999
      endif
      read(cline(9+k:l-1),*,ERR=800)ival
C
      goto 999
C
 800  continue
        write(*,'(''::WARNING: no value for '',A,
     .    '', setting to zero.'')')cname(1:k)
        ival=0
        goto 999
C
 900  continue
        write(*,'(''::ERROR on value read:'',A30)')cname
        stop
C
 999  continue
      RETURN
      END
C
c==========================================================
c
      SUBROUTINE rgetline(rval,cname)
C
      CHARACTER * (*) cname
      character*200 cline
C
      call shorten(cname,k)
C
      rewind(12) 
C
 100  continue
C 
        read(12,'(A)',END=900,ERR=900)cline
        if(cline(1:3).ne."set") goto 100
        if(cline(5:4+k).ne.cname(1:k)) goto 100
        if(cline(5+k:5+k).ne." ") goto 100
C
      call shorten(cline,l)
C      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cline(9+k:l-1)
      read(cline(9+k:l-1),*,ERR=800)rval
C
      goto 999
C
 800  continue
        write(*,'(''::WARNING: no value for '',A,
     .    '', setting to zero.'')')cname(1:k)
        rval=0.0
        goto 999
C
 900  continue
        write(*,'(''::ERROR on value read:'',A30)')cname
        stop
C
 999  continue
      RETURN
      END
C
c==========================================================
c
      SUBROUTINE cgetline(cval,cname)
C
      CHARACTER * (*) cname,cval
      character*200 cline
C
      call shorten(cname,k)
C
      rewind(12)
C
 100  continue
C 
        read(12,'(A)',END=900,ERR=900)cline
        if(cline(1:3).ne."set") goto 100
        if(cline(5:4+k).ne.cname(1:k)) goto 100
        if(cline(5+k:5+k).ne." ") goto 100
C
      call shorten(cline,l)
      ilen=l-k-9
      do i=1,ilen
        cval(i:i) = cline(8+i+k:8+i+k)
      enddo
      n=len(cval)
      write(cval(ilen:n),'(A)')cline(l-1:l-1)
      call shorten(cval,i)
C      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cval(1:i)
C
      goto 999
C
 900  continue
        write(*,'(''::ERROR on value read:'',A30)')cname
        stop
C
 999  continue
      RETURN
      END
C




