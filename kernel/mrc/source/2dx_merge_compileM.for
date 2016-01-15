       PROGRAM COMPILEM
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
      character*200 cname1,cname2,cname3,cname4,cdir,cprocdir,cbindir
      character*200 cccp4dir,cccp4,ctmpdir,cbasedir
      character*80 cspcgrp,crealcell,CBMTLT,CPHORI,CIMAGENAME,CTITLE
      character*80 CIMAGENUMBER,CLATTICE,CMLMERGE,cgrid
      character*200 CFILE1,cline,clin2
      character*1 CNREFOUT,CNSHFTIN
      integer*8 imnum(10000)
      integer stat 
      logical THERE1,THERE2
C
      write(*,'('':2dx_merge_compileM - '',
     .    ''compiling a script to generate the maps'')')
C
      write(*,'(/,''input name of base directory'')')
      read(*,'(A)')cbasedir
      call shorten(cbasedir,k)
      write(*,'(A)')cbasedir(1:k)
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
            write(*,'(/,''input Thread Number'')')
      read(*,*)ITHRNUM
      if(ITHRNUM.lt.1)ITHRNUM=1
      if(ITHRNUM.gt.24)then
        ITHNUMD=24
        write(*,'('':: LIMITING THREADS TO '',I5)')ITHRNUM
      endif
      write(*,'(I3)')ITHRNUM
C
      write(*,'(/,''input merge_data_type switch'')')
      read(*,*)IMERGEML
      if(IMERGEML.eq.0)then
        write(*,'(I1,'' = Using Fourier filtered results with'',
     .    '' later CTF correction '')')IMERGEML
      else if(IMERGEML.eq.1)then
        write(*,'(I1,'' = Using CTF-corrected and then Fourier'',
     .    '' filtered results '')')IMERGEML
      else if(IMERGEML.eq.2)then
        write(*,'(I1,'' = Using CTF-corrected and then Movie-ModeA'',
     .    '' unbent results '')')IMERGEML
      else
        write(*,'(I1,'' = Using Maximum Likelihood results '',
     .           ''where allowed'')')IMERGEML
      endif
C
      read(*,'(A)',iostat=stat)cgrid
      if(stat.ne.0)then
C        write(*,'('': reached EOF '')')
        GOTO 85
      endif
      write(*,'(A40)')cgrid
      write(*,'('':: read grid size: '',A40)')cgrid
      call shorten(cgrid,k)
      write(11,'(''set gridsize = "'',A,''"'')')cgrid(1:k)
C
  85  continue
      open(10,FILE=cname1,STATUS='OLD',ERR=900)
C
      imcount = 0
C
  90  continue
        read(10,'(A)',END=95)cdir
        imcount = imcount + 1
        goto 90
C
  95  continue
C
      itotalnumber = imcount
C
      rewind(10)
C
      itotalnumber = imcount
      if(itotalnumber.gt.ITHRNUM)then
        imperthread = itotalnumber / ITHRNUM 
      else
        imperthread = 1
C-------This is to make sure the last thread takes a bit longer than the others:
        ITHRNUM = itotalnumber-1
      endif
C
      if(ITHRNUM.eq.0)ITHRNUM = 1
C
      write(*,'('':For '',I6,'' images, creating '',
     .  I3,'' scripts for '',I5,'' images each.'')')
     .  itotalnumber,ITHRNUM,imperthread
C
      imtotalcount = 0
      imcount = 0
C
      do ithread = 1,ITHRNUM
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A,''.com'')')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''\rm -f '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
        write(*,'(/,'':Creating script '',A)')cline(1:k1)
        open(11,FILE=cline,STATUS='NEW',ERR=900)
C
        write(11,'(''#!/bin/csh -ef'')')
        write(11,'(''#'')')
        write(11,'(''set proc_2dx = $1'')')
        write(11,'(''set bin_2dx = $2'')')
        write(11,'(''set MergeIQMAX = $3'')')
        write(11,'(''set RFACAMP = $4'')')
        write(11,'(''set RESMIN = $5'')')
        write(11,'(''set RESMAX = $6'')')
        write(11,'(''set CCP4_SYM = $7'')')
        write(11,'(''set zwin = $8'')')
        write(11,'(''set make_reference = $9'')')
        write(11,'(''set create_PS = $10'')')
        write(11,'(''#'')')
        write(11,'(''set mergedir = $PWD'')')
        write(11,'(''#'')')
C
        call shorten(crealcell,k)
        write(11,'(''set realcell = "'',A,''"'')')crealcell(1:k)
        write(cline,'(F12.3)')realang
        call shortshrink(cline,k)
        write(11,'(''set realang = "'',A,''"'')')cline(1:k)
        write(cline,'(F12.3)')RALAT
        call shortshrink(cline,k)
        write(11,'(''set ALAT = "'',A,''"'')')cline(1:k)
        write(11,'(''#'')')
        write(11,'(''source ${proc_2dx}/initialize'')')
        write(11,'(''#'')')
        write(11,'(''#'')')
        write(11,'(''set split = ($realcell:as/,/ /)'')')
        write(11,'(''set cellx = $split[1]'')')
        write(11,'(''set celly = $split[2]'')')

        write(11,'(''#'')')
        write(11,'(''set RESULTSPS = "n"'')')
        write(11,'(''set RESULTSMRC = "n"'')')

        write(11,'(''#'')')
C
        call shorten(cbasedir,kbase)
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

            read(10,'(A)',END=210)ctmpdir
            call shorten(ctmpdir,k)
            if(ctmpdir(1:1).eq.'/')then
              write(cdir,'(A)')ctmpdir(1:k)
            else
              write(cdir,'(A,''/'',A)')cbasedir(1:kbase),ctmpdir(1:k)
            endif
            call shorten(cdir,k)
            write(cname3,'(A,''/2dx_image.cfg'')')cdir(1:k)
            call shortshrink(cname3,k)
            INQUIRE( FILE=cname3, EXIST=THERE1 )     
            if (.not.THERE1)then
              write(6,'(''::WARNING: File '',A,'' does not exist'')')cname3(1:k)
              goto 100
            endif
C
            call shorten(cdir,k)
            write(cname4,'(A,''/APH/image_ctfcor_ctf.aph'')')
     .        cdir(1:k)
            call shortshrink(cname4,k)
            INQUIRE( FILE=cname4, EXIST=THERE2 )     
            if (.not.THERE2)then 
              write(6,'(''::WARNING: File '',A,'' does not exist'')')cname4(1:k)
              goto 100
            endif

C
            write(*,'(''opening '',A)')cname3
            open(12,FILE=cname3,STATUS='OLD',ERR=910)
C
            call cgetline(CIMAGENAME,"imagename")
            call cgetline(CIMAGENUMBER,"imagenumber")
            read(CIMAGENUMBER,*)imnum(imtotalcount)
C           write(*,'(''::imagenumber read = '',I10)')imnum(imcount)
            if(imtotalcount.gt.1)then
              do i=1,imcount-1
                if(imnum(i).eq.imnum(imtotalcount))then
                  call shorten(CIMAGENAME,k)
                  write(*,'(''WARNING: Imagenumber '',I10,
     .              '' appears twice, here for image '',A)')imnum(i),CIMAGENAME(1:k)
                    write(*,'(''::You should run the Custom Script named '',
     .                ''RENUMBER IMAGENUMBERS to fix this.'')')
                endif
              enddo
            endif
C
            call dgetline(CMLMERGE,"ML_use_for_merging",iok)
            if(iok.eq.0)then
              write(CMLMERGE(1:1),'(''n'')')
              write(*,'(''::WARNING: ML_use_for_merging not yet'',
     .          '' defined for this image.'')')
              write(*,'(''::To resolve, open 2dx_image on this image,'',
     .          '' click on save, and close 2dx_image.'')')
            endif
C
            write(11,'(''set ML_use_for_merging = "'',A1,''"'')')CMLMERGE(1:1)
C
            write(11,'(''echo ":: "'')')
            call shorten(cdir,k)
            if(CMLMERGE(1:1).ne."y" .or. IMERGEML.lt.3)then
              write(cline,'(''${proc_2dx}/linhash "'',A,
     .          '' (FOURIER FILTERED RESULT)"'')')
     .          cdir(1:k)
            else
              write(cline,'(''${proc_2dx}/linhash "'',A,
     .          '' (SINGLE PARTICLE RESULT)"'')')
     .          cdir(1:k)
            endif
            call shortshrink(cline,k)
            write(11,'(A)')cline(1:k)
C
            write(11,'(''#'')')
            write(11,'(79(''#''))')
            write(11,'(''cd '',A)')cdir(1:k)
            write(11,'(79(''#''))')
            write(11,'(''#'')')
C
            write(11,'(''set rootdir = $PWD '')')
C
            write(11,'(''#'')')
C
            call cgetline(cline,"imagename")
            call shorten(cline,k)
            write(11,'(''set imagename = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"nonmaskimagename")
            call shorten(cline,k)
            write(11,'(''set nonmaskimagename = "'',A,''"'')')cline(1:k)
C
            call shorten(CIMAGENUMBER,k)
            write(11,'(''set imagenumber =  "'',A,''"'')')CIMAGENUMBER(1:k)
C
            call cgetline(cline,"tempkeep")
            call shorten(cline,k)
            write(11,'(''set tempkeep = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"RESMIN")
            call shorten(cline,k)
            write(11,'(''set RESMIN = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"RESMAX")
            call shorten(cline,k)
            write(11,'(''set RESMAX = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"CS")
            call shorten(cline,k)
            write(11,'(''set CS = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"KV")
            call shorten(cline,k)
            write(11,'(''set KV = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"lattice")
            call shorten(cline,k)
            write(11,'(''set lattice = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"realang")
            call shorten(cline,k)
            write(11,'(''set realang = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"realcell")
            call shorten(cline,k)
            write(11,'(''set realcell = "'',A,''"'')')cline(1:k)
C
            if(CMLMERGE(1:1).ne."y" .or. IMERGEML.lt.3)then
              call cgetline(CPHORI,"phaori")
              read(CPHORI,*)RPHAORIH,RPHAORIK
              write(*,'(''   using Fourier filtered results,'',
     .          '' PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
              call cgetline(cline,"phaori")
              call shorten(cline,k)
              write(11,'(''set phaori = "'',A,''"'')')cline(1:k)
              write(11,'(''set LUSEML = "F"'')')
            else
              call cgetline(CPHORI,"phaori_ML")
              read(CPHORI,*)RPHAORIH,RPHAORIK
              call cgetline(cline,"phaori_ML")
              call shorten(cline,k)
              write(*,'(''   using Single Particle  results,'',
     .          '' PhaseOrigin_ML = '',2F12.3)')RPHAORIH,RPHAORIK
              write(11,'(''set phaori = "'',A,''"'')')cline(1:k)
              write(11,'(''set LUSEML = "T"'')')
            endif
C
            call cgetline(cline,"rot90")
            call shorten(cline,k)
            write(11,'(''set rot90 = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"rot180")
            call shorten(cline,k)
            write(11,'(''set rot180 = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"revhk")
            call shorten(cline,k)
            write(11,'(''set revhk = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"ctfrev")
            call shorten(cline,k)
            write(11,'(''set ctfrev = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"revhnd")
            call shorten(cline,k)
            write(11,'(''set revhnd = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"revxsgn")
            call shorten(cline,k)
            write(11,'(''set revxsgn = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"sgnxch")
            call shorten(cline,k)
            write(11,'(''set sgnxch = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"beamtilt")
            call shorten(cline,k)
            write(11,'(''set beamtilt = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"zstarrange")
            call shorten(cline,k)
            write(11,'(''set zstarrange = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"tempfac")
            call shorten(cline,k)
            write(11,'(''set tempfac = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"TAXA")
            call shorten(cline,k)
            write(11,'(''set TAXA = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"TANGL")
            call shorten(cline,k)
            write(11,'(''set TANGL = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"npo_line1")
            call shorten(cline,k)
            write(11,'(''set npo_line1 = "'',A,''"'')')cline(1:k)
C
            call cgetline(cline,"npo_line2")
            call shorten(cline,k)
            write(11,'(''set npo_line2 = "'',A,''"'')')cline(1:k)
C
            close(12)
C
            write(11,'(''#'')')
C
            write(11,'(''source ${proc_2dx}/2dx_merge_redoMap_sub.com'')')
C
            write(11,'(''#'')')
            iprogress = 50 + (30*2*imcount) / (2 * itotalnumber)
            write(cline,'(I2)')iprogress
            call shorten(cline,k)
            write(11,'(''echo "<<@progress: '',A,''>>"'')')cline(1:k)
            write(11,'(''#'')')
C
            write(11,'(''#'')')
C
            goto 100
C
 200      continue
C
          write(11,'(''#'')')
          write(11,'(79(''#''))')
          write(11,'(''cd ${mergedir}'')')
          write(11,'(79(''#''))')
          write(11,'(''#'')')
C
          write(11,'(''echo "Done."'')')
          write(11,'(''#'')')
C
          close(11)
C
      enddo
      goto 220
      
 210  continue
          write(11,'(''#'')')
          write(11,'(79(''#''))')
          write(11,'(''cd ${mergedir}'')')
          write(11,'(79(''#''))')
          write(11,'(''#'')')
C
          write(11,'(''echo "Done."'')')
          write(11,'(''#'')')
C
          close(11)
C
 220  continue
C
      close(10)
C
      goto 999
C
 900  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in 2dx_merge_compileM'')')
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
     .     ''in 2dx_merge_compileM'')')
        call shorten(cname3,k)
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




