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
      character*200 CFILE1,cline,clin2
      character*200 CFILEreflections,CFILEconsole
      character*1 CNREFOUT,CNSHFTIN,CTMP1
      integer*8 imnum(10000)
      logical lexist
      logical LPROTFOUFIL,LUSEML
C
      LPROTFOUFIL = .FALSE.
C
      write(*,'('':2dx_merge_compileA_threaded - '',
     .    ''compiling the merging script'')')
C
      write(*,'(/,''input name of results output file'')')
      read(*,'(A)')CFILE1
      write(*,'(A)')CFILE1
C
C      write(CFILEreflections,'(''2dx_origtiltk-reflections.log'')')
      write(*,'(/,''input name of reflections output file'')')
      read(*,'(A)')CFILEreflections
      write(*,'(A)')CFILEreflections
C
C      write(CFILEconsole,'(''2dx_origtiltk-console.log'')')
      write(*,'(/,''input name of console output file'')')
      read(*,'(A)')CFILEconsole
      write(*,'(A)')CFILEconsole
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
      write(*,'(/,''input ctfrev'')')
      read(*,*)cline
      if(cline(1:1).eq."y")then 
        ICTFREV = 1
      else
        ICTFREV = 0
      endif
      write(*,'(I3)')ICTFREV
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
      write(*,'(/,''input merge_data_type switch'')')
      read(*,*)IMERGEDAT
      write(*,'(I3)')IMERGEDAT
C
      write(*,'(/,''input Thread Number'')')
      read(*,*)ITHRNUM
      if(ITHRNUM.lt.1)ITHRNUM=1
      if(ITHRNUM.gt.24)ITHRNUM=24
      write(*,'(I3)')ITHRNUM
C
      write(*,'(/,''input ILIST switch'')')
      read(*,*)ILIST
      write(*,'(I3)')ILIST
C
      write(*,'(/,''input zstarwin'')')
      read(*,*)RZWIN
      write(*,'(F12.3)')RZWIN
C
      call shorten(cname1,k)
      write(6,'(''Opening file '',A)')cname1(1:k)
      open(10,FILE=cname1,STATUS='OLD',ERR=900)
      imcount = 0
 110  continue
        read(10,'(A)',END=120)cline
        imcount = imcount + 1
      goto 110
 120  continue
      rewind(10)
      imnumber = imcount
      if(imnumber.gt.ITHRNUM)then
        imperthread = imnumber / ITHRNUM 
      else
        imperthread = 1
C-------This is to make sure the last thread takes a bit longer than the others:
        ITHRNUM = imnumber-1
      endif
C
      if(ITHRNUM.eq.0)ITHRNUM = 1
C
      write(*,'('':For '',I6,'' images to merge, creating '',
     .  I3,'' scripts for '',I5,'' images each.'')')
     .  imnumber,ITHRNUM,imperthread
C
      imtotalcount = 0
      imcount = 0
C
      do ithread = 1,ITHRNUM
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''\rm -f '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
        imfrom = imtotalcount + 1
        if(ithread.lt.ITHRNUM)then
          imto = imtotalcount + imperthread
        else
          imto = imnumber 
        endif
        write(*,'(/,'':Creating merging script '',A,
     .       '' for image '',I5,'' to '',I5,'' of '',I5)')
     .    cline(1:k1),imfrom,imto,imnumber
        open(11,FILE=cline,STATUS='NEW',ERR=900)
C
        write(11,'(''#!/bin/csh -ef'')')
        write(11,'(''#'')')
C
C        if(igenref.eq.1)then
C          write(11,'(''echo dummy > APH/REF1.hkl'')')
C          write(11,'(''rm -f APH/REF*.hkl'')')
C          write(11,'(''#'')')
C        endif
C
C        if(inshftin.eq.1)then
C          write(11,'(''echo dummy > PRJ/HKLAPH1.prj'')')
C          write(11,'(''rm -f PRJ/HKLAPH*.prj'')')
C          write(11,'(''#'')')
C        endif
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
        write(11,'(''set ILIST = '',I3)')ILIST
        write(11,'(''setenv OMP_NUM_THREADS 4'')')
        write(11,'(''#'')')
        write(11,'(''rm -f 2dx_origtiltk-console.log'')')
        write(11,'(''#'')')
C
        write(11,'(''${bin_2dx}/2dx_origtiltk.exe << eot'')')
C
        call shorten(CFILE1,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,CFILE1(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k)
        write(11,'(A)')cline(1:k)
C
        call shorten(CFILEreflections,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,CFILEreflections(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k)
        write(11,'(A)')cline(1:k)
C
        call shorten(CFILEconsole,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,CFILEconsole(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k)
        write(11,'(A)')cline(1:k)
C
        write(11,'(''${spcgrp} -1 F F ${ILIST} ${realcell} ${ALAT} '',
     .   ''${realang} 0 15 ${IAQP2} ${IVERBOSE} ${LOGOUTPUT} '',
     .   ''!ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE'',
     .   '',LOGOUTPUT '')')
C
        write(cline,'(''SCRATCH/job_'',I2,''_results.aph'')')ithread
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k)
        write(11,'(A)')cline(1:k)
C
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
 100    continue
C
          if(imcount.ge.imperthread .and. ithread.lt.ITHRNUM)then
C-----------in this case, finish this file and start next one:
            imcount = 0
            goto 200
          endif
C
          imcount=imcount+1
          imtotalcount=imtotalcount+1
C
          read(10,'(A)',END=210)ctmpdir
C
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
          open(12,FILE=cname3,STATUS='OLD',ERR=910)
C
          call cgetline(CIMAGENAME,"imagename")
          call cgetline(CIMAGENUMBER,"imagenumber")

          read(CIMAGENUMBER,*)imnum(imtotalcount)
C         write(*,'(''::imagenumber read = '',I10)')imnum(imtotalcount)
          if(imtotalcount.gt.1)then
            ifound=0
            do i=1,imtotalcount-1
              if(imnum(i).eq.imnum(imtotalcount))then
                if(ifound.eq.0)then
                  call shorten(CIMAGENAME,k)
                  write(*,'(''::WARNING: Imagenumber '',I10,
     .            '' appears twice, here for image '',A)')imnum(i),CIMAGENAME(1:k)
                  write(*,'(''::You should run the Custom Script '',
     .            ''named RENUMBER IMAGENUMBERS to fix this.'')')
                  ifound=1
                endif
              endif
            enddo
          endif
C
          call cgetline(cline,"imagename_original")
          call shorten(cline,k2)
C---------k2 now points to last character in cline. Find last "/":
          do i=k2,1,-1
            READ(cline(i:i),'(A1)')CTMP1
            IF(CTMP1.eq.'/')GOTO 300
          enddo
          i=0
  300     continue
          k1=i+1
C---------Now, k1 point to first character of usable file name
          write(CTITLE,'(A)')cline(k1:k2)
C         
          call rgetline(RESMAX,"RESMAX")
          call rgetline(RESMIN,"RESMIN")
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
            write(CMLMERGE,'(''n'')')
            write(*,'(''::WARNING: ML_use_for_merging not yet '',
     .        ''defined for this image.'')')
            write(*,'(''::To resolve, open 2dx_image on this image,'',
     .        '' click on save, and close 2dx_image.'')')
          endif
C
          if(imcount.eq.1)then
C-----------First film is used as is, without rescaling
            RSCL=1.0
          else
C-----------RSCL=0.0 means scaling is automatic for following datasets
            RSCL=0.0
          endif
          call igetline(IROT90,"rot90")
          call igetline(IROT180,"rot180")
          call igetline(IREVHK,"revhk")
          call igetline(IREVHND,"revhnd")
          call igetline(IREVXSGN,"revxsgn")
          call shorten(CTITLE,k)
C
          if(CMLMERGE(1:1).eq."y" .and. IMERGEDAT.eq.3)then
            call cgetline(CPHORI,"phaori_ML")
            read(CPHORI,*,ERR=901)RPHAORIH,RPHAORIK
            goto 902
 901        continue
            RPHAORIH=0.0
            RPHAORIK=0.0
 902        continue
            write(*,'(''   using Single Particle  results, '',
     .      ''PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
          else
            call cgetline(CPHORI,"phaori")
            read(CPHORI,*,ERR=903)RPHAORIH,RPHAORIK
            goto 904
 903        continue
            RPHAORIH=0.0
            RPHAORIK=0.0
 904        continue
            write(*,'(''   using Fourier filtered results, '',
     .      ''PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
          endif
C
          close(12)
C
C---------Loop over potentially all three unbending forms:
          if (IMERGEDAT.eq.5) then
            ilooend = 3
          else
            ilooend = 1
          endif
          do iloo = 1,ilooend
C
            call shorten(cdir,k1)
            call shortshrink(CIMAGENAME,k2)
C
            if(IMERGEDAT.ne.5)then
              if(CMLMERGE(1:1).eq."y" .and. IMERGEDAT.eq.3)then
                write(cname4,'(A,''/APH/ML_result.aph'')')
     .          cdir(1:k1)
                LUSEML = .TRUE.
              else
                write(cname4,'(A,''/APH/image_ctfcor_ctf.aph'')')
     .          cdir(1:k1)
                LUSEML = .FALSE.
              endif
              call shortshrink(cname4,k4)
              inquire(file=cname4(1:k4),exist=lexist)
            else
              if(iloo.eq.1)then
                write(cname4,'(A,
     .         ''/APH/image_ctfcor_fou_unbent_ctf.aph'')')cdir(1:k1)
              elseif(iloo.eq.2)then
                write(cname4,'(A,
     .          ''/APH/image_ctfcor_movie_fou_ctf.aph'')')cdir(1:k1)
              elseif(iloo.eq.3)then
                write(cname4,'(A,
     .          ''/APH/image_ctfcor_movieB_fou_ctf.aph'')')cdir(1:k1)
              endif
              call shortshrink(cname4,k4)
              inquire(file=cname4(1:k4),exist=lexist)
            endif
            if(.not.lexist)then
              write(6,'('':File not found: '',A)')cname4(1:k4)
            else
              call shorten(CTITLE,k)
              k1=1
              if(k.gt.40)k1=41-k
C
              write(11,'(A10,A40)')CIMAGENUMBER(1:10),CTITLE(k1:k)
              write(11,'(A)')cname4(1:k4)
C
              write(11,'(''  F'')')
              write(11,'(2F12.3,'' 0'',40X,''! TAXA,TANGL,IORIGT'')')
     .         RTAXA,RTANGL
C
              call shortshrink(CLATTICE,k)
              write(11,'(A40,26X,''! lattice'')')CLATTICE(1:k)
C
              write(11,'(2F10.3,'' 0.0 '',F10.3,'' 0 '',F9.5,6I2,2L2,
     .         '' ! OH,OK,STEP,WIN,SGNXCH,SCL,R180,RHK,'',
     .          ''CTFREV,ROT90,REVHND,REVSGN,LPROTFOUFIL,LUSEML'')')
     .         RPHAORIH,RPHAORIK,RZWIN,RSCL,IROT180,IREVHK,ICTFREV,IROT90,
     .         IREVHND,IREVXSGN,
     .         LPROTFOUFIL,LUSEML
C
              write(11,'(4F12.3,''                  ! cs,kv,tx,ty'')')
     .        RCS,RKV,RTX,RTY
              if(ISRESMAX.eq.0)then
                write(11,'(2F12.3,42X,''! resolution limits'')')
     .          RGRESMIN,RGRESMAX
              else
                write(11,'(2F12.3,42X,''! resolution limits'')')
     .          RESMIN,RESMAX
              endif
            endif
C
          enddo
C
        goto 100
C
 200    continue
C
        write(11,'(''        -1'')')
        write(11,'(''eot'')')
        write(11,'(''#'')')
        close(11)
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
      enddo
      goto 220
          
 210  continue
        write(11,'(''        -1'')')
        write(11,'(''eot'')')
        write(11,'(''#'')')
        close(11)
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
 220  continue

      close(10)
C
      goto 999
C
 900  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in '',
     .     ''2dx_merge_compileA_thread'')')
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
C        write(*,'(''::READ:  '',A)')cline(1:5+k)
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
        write(*,'(''::ERROR on value read: '',A)')cname(1:k)
        stop
C
 999  continue
C        write(*,'(''::Found: '',A)')cname(1:k)
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



