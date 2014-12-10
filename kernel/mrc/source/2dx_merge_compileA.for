       PROGRAM COMPILEA
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
C This program will create a cshell script that performs the actual merging, 
C making use of the MRC program 2dx_origtiltk.for
C
      character*200 cname1,cname2,cname3,cname4
      character*200 cdir,cprocdir,cbindir,ctmpdir
      character CROT90,CROT180
      character*80 cspcgrp,crealcell,CBMTLT,CPHORI,CIMAGENAME,CTITLE
      character*80 CIMAGENUMBER,CLATTICE,CMLMERGE
      character*200 CFILE1,cline
      character*1 CNREFOUT,CNSHFTIN
      integer*8 imnum(10000)
C
      write(*,'('':2dx_merge_compileA - '',
     .    ''compiling the merging script'')')
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
      write(*,'(/,''input Switch deciding about which RESMAX to use'')')
      read(*,*)ISRESMAX
      if(ISRESMAX.eq.0)then
        write(*,'(I1,'' = Using global RESMAX'')')ISRESMAX
      else
        write(*,'(I1,'' = Using individual RESMAX values'')')ISRESMAX
      endif
C
      write(*,'(/,''input global RESMIN'')')
      read(*,*)RGRESMIN
      write(*,'(F12.3)')RGRESMIN
C
      write(*,'(/,''input global RESMAX'')')
      read(*,*)RGRESMAX
      write(*,'(F12.3)')RGRESMAX
C
      write(*,'(/,''input merge_ML_data switch'')')
      read(*,*)IMERGEML
      if(IMERGEML.eq.0)then
        write(*,'(I1,'' = Using Fourier filtered results'')')IMERGEML
      else
        write(*,'(I1,'' = Using Maximum Likelihood results '',
     .           ''where allowed'')')IMERGEML
      endif
C
      write(*,'(/,''input ILIST switch'')')
      read(*,*)ILIST
      write(*,'(I3)')ILIST
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
      write(11,'(''set LOGOUTPUT = F'')')
      write(11,'(''set LPROTFOUFIL = F'')')
      write(11,'(''set ILIST = '',I3)')ILIST
      write(11,'(''setenv OMP_NUM_THREADS 4'')')
      write(11,'(''#'')')
      write(11,'(''rm -f 2dx_origtiltk-console.log'')')
      write(11,'(''#'')')
C
      write(11,'(''${bin_2dx}/2dx_origtiltk.exe << eot'')')
C
      call shorten(CFILE1,k)
      write(11,'(A)')CFILE1(1:k)
C
      write(11,'(''${spcgrp} 0 F F ${ILIST} ${realcell} ${ALAT} '',
     .   ''${realang} 0 15 ${IAQP2} ${IVERBOSE} ${LOGOUTPUT} '',
     .   ''!ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE'',
     ,   '',LOGOUTPUT'')')
      write(11,'(''10,0.7,10,0.5'',28X,
     .   ''!itaxastep,rtaxasize,itanglstep,rtanglsize'')')
C
      if(igenref.eq.1)then
        write(CNREFOUT,'(''T'')')
      else
        write(CNREFOUT,'(''F'')')
      endif
C
      if(ishftin.eq.1)then
        write(CNSHFTIN,'(''T'')')
      else
        write(CNSHFTIN,'(''F'')')
      endif
C
      write(11,'(''1001 0 '',2I3,'' 1 '',A1,'' '',A1,
     .   '' ${RFACAMP}          '',
     .   ''!IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP'')')
     .   ImergeHKMax,ImergeIQMax,CNREFOUT,CNSHFTIN
C
      write(11,'(''0000000100 DUMMY'')')
      write(11,'(''${proc_2dx}/dummy.aph'')')
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
C
        call system("pwd")
C
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
              write(*,'(''::'',79(''#''))')
              write(*,'(''::'',79(''#''))')
              write(*,'(''::'',79(''#''))')
              write(*,'(''::ERROR; Imagenumber '',I10,
     .          '' appears twice.'')')imnum(i)
              call shorten(CIMAGENAME,k)
              write(*,'(''::This is for image '',A)')CIMAGENAME(1:k)
              write(*,'(''::'',79(''#''))')
              write(*,'(''::'',79(''#''))')
              write(*,'(''::'',79(''#''))')
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
        call cgetline(CLATTICE,"lattice")
        call rgetline(RTAXA,"TAXA")
        call rgetline(RTANGL,"TANGL")
C
        call dgetline(CMLMERGE,"ML_use_for_merging",iok)
        if(iok.eq.0)then
          write(CMLMERGE(1:1),'(''n'')')
          write(*,'(''::WARNING: ML_use_for_merging not yet defined for this image.'')')
          write(*,'(''::To resolve, open 2dx_image on this image, click on save, and close 2dx_image.'')')
        endif
C
        if(CMLMERGE(1:1).ne."y" .or. IMERGEML.eq.0)then
          call cgetline(CPHORI,"phaori")
          read(CPHORI,*)RPHAORIH,RPHAORIK
          write(*,'(''   using Fourier filtered results, PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
        else
          RPHAORIH=0.0
          RPHAORIK=0.0
          write(*,'(''   using Single Particle  results, PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
        endif
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
        call shorten(CTITLE,k)
        k1=1
        if(k.gt.40)k1=41-k
        write(11,'(A10,A40)')CIMAGENUMBER(1:10),CTITLE(k1:k)
        call shorten(cdir,k1)
        call shortshrink(CIMAGENAME,k2)
C
        if(CMLMERGE(1:1).ne."y" .or. IMERGEML.eq.0)then
          write(cname4,'(A,''/APH/'',A,''_ctf.aph'')')
     .      cdir(1:k1),CIMAGENAME(1:k2)
        else
          write(cname4,'(A,''/APH/ML_result.aph'')')
     .      cdir(1:k1)
        endif
        call shortshrink(cname4,k1)
         write(11,'(A)')cname4(1:k1)
C
        write(11,'(''  F'')')
        write(11,'(2F12.3,'' 0'',40X,''! TAXA,TANGL,IORIGT'')')
     .     RTAXA,RTANGL
C
        call shortshrink(CLATTICE,k)
        write(11,'(A40,26X,''! lattice'')')
     .     CLATTICE(1:k)
C
        write(11,'(2F10.3,'' 0.0 '',F10.3,'' 0 '',F9.5,6I2,
     .     '',${LPROTFOUFIL} '',
     .     '' ! OH,OK,STEP,WIN,SGNXCH,SCL,R180,RHK,'',
     .      ''CTFREV,ROT90,REVHND,REVSGN,LPROTFOUFIL'')')
     .     RPHAORIH,RPHAORIK,RZWIN,RSCL,IROT180,IREVHK,ICTFREV,IROT90,
     .    IREVHND,IREVXSGN
C
        write(11,'(4F12.3,''                  ! cs,kv,tx,ty'')')
     .    RCS,RKV,RTX,RTY
        if(ISRESMAX.eq.0)then
          write(11,'(2F12.3,42X,''! resolution limits'')')
     .      RGRESMIN,RGRESMAX
        else
          write(11,'(2F12.3,42X,''! resolution limits'')')
     .      RESMIN,RESMAX
        endif
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
        write(*,'(''::ERROR on file open in 2dx_merge_compileA'')')
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
     .     ''in 2dx_merge_compileA'')')
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
        write(*,'(''::WARNING: no value vor '',A,
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
        write(*,'(''::WARNING: no value vor '',A,
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
c==========================================================
c
      SUBROUTINE dgetline(cval,cname,iok)
C
C-----Same as cgetline, but no error is generated.
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
      iok=1
C
      goto 999
C
 900  continue
        iok=0
C
 999  continue
      RETURN
      END
C



