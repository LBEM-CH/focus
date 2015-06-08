       PROGRAM COMPILEB
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
C This program creates a cshell script that performs the refinement
C of the image parameters, making use of the adapted MRC program
C 2dx_origtiltk.for
C
      character*200 cname1,cname2,cname3,cname4,cname5
      character*200 cdir,cprocdir,cbindir,ctmpdir
      character CROT90,CROT180
      character*80 cspcgrp,crealcell,CBMTLT,CPHORI,CIMAGENAME,CTITLE
      character*80 CIMAGENUMBER,CLATTICE,CPHOPROT,CMLMERGE
      character*1 cNBM,cNTL,CNREFOUT
      character*200 CFILE1,cerrmes,cline,clin2
      character*200 CFILEreflections,CFILEconsole
      character*80 CCURRENTREF,CMODUS
      integer*8 imnum(10000)
      logical LPROTFOUFIL,LUSEML
C
      write(*,'('':2dx_merge_compileB - '',
     .    ''compiling the refinement script'')')
C
      write(*,'(/,''input name of results output file'')')
      read(*,'(A)')CFILE1
      call shorten(CFILE1,k)
      write(*,'(A)')CFILE1(1:k)
C
      write(*,'(/,''input name of reflections output file'')')
      read(*,'(A)')CFILEreflections
      call shorten(CFILEreflections,k)
      write(*,'(A)')CFILEreflections(1:k)
C
      write(*,'(/,''input name of console output file'')')
      read(*,'(A)')CFILEconsole
      call shorten(CFILEconsole,k)
      write(*,'(A)')CFILEconsole(1:k)
C
      write(*,'(/,''input name of directory with procs'')')
      read(*,'(A)')cprocdir
      call shorten(cprocdir,k)
      write(*,'(A)')cprocdir(1:k)
C
      write(*,'(/,''input name of directory with bins'')')
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
      write(*,'(/,''input name of postprocessing script '',
     .  ''file for output'')')
      read(*,'(A)')cname5
      call shorten(cname5,k)
      write(*,'(A)')cname5(1:k)
C
      write(*,'(/,''input switch for genref (1=y,0=n)'')')
      read(*,*)igenref
      write(*,'(I6)')igenref
C
      if(igenref.eq.1)then
        write(CNREFOUT,'(''T'')')
      else
        write(CNREFOUT,'(''F'')')
      endif
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
      write(*,'(/,''input zstar window'')')
      read(*,*)rzwin
      write(*,'(F12.3)')rzwin
C
      write(*,'(/,''input ALAT'')')
      read(*,*)RALAT
      write(*,'(F12.3)')RALAT
C
      write(*,'(/,''input IVERBOSE'')')
      read(*,*)IVERBOSE
      write(*,'(I3)')IVERBOSE
C
      write(*,'(/,''input PHSTEP'')')
      read(*,*)RPHSTEP
      write(*,'(F12.3)')RPHSTEP
C
      write(*,'(/,''input PFACAMP'')')
      read(*,*)RFACAMP
      write(*,'(F12.3)')RFACAMP
C
      write(*,'(/,''input IBOXPHS'')')
      read(*,*)IBOXPHS
      write(*,'(I8)')IBOXPHS
C
      write(*,'(/,''input NPRG'')')
      read(*,*)NPRG
      write(*,'(I8)')NPRG
C
      write(*,'(/,''input merge_reference'')')
      read(*,*)IMERGREF
      write(*,'(I8)')IMERGREF
C
      write(*,'(/,''input NBM'')')
      read(*,*)cNBM
      write(*,'(A1)')cNBM
C
      write(*,'(/,''input NTL'')')
      read(*,*)cNTL
      write(*,'(A1)')cNTL
C
      write(*,'(/,''input ITAXASTEP'')')
      read(*,*)ITAXASTEP
      write(*,'(I8)')ITAXASTEP
C
      write(*,'(/,''input RTAXASIZE'')')
      read(*,*)RTAXASIZE
      write(*,'(F12.3)')RTAXASIZE
C
      write(*,'(/,''input ITANGLSTEP'')') 
      read(*,*)ITANGLSTEP
      write(*,'(I8)')ITANGLSTEP
C
      write(*,'(/,''input RTANGLSIZE'')')
      read(*,*)RTANGLSIZE
      write(*,'(F12.3)')RTANGLSIZE
C
      write(*,'(/,''input IQMX'')')
      read(*,*)IQMX
      write(*,'(I8)')IQMX
C
      write(*,'(/,''input HKMX'')')
      read(*,*)IHKMX
      write(*,'(I8)')IHKMX
C
      write(*,'(/,''input Switch deciding about which RESMAX to use'')')
      read(*,*)ISRESMAX
      if(ISRESMAX.eq.0)then
        write(*,'(I1,'' = Using global RESMAX'')')ISRESMAX
      else
        write(*,'(I1,'' = Using individual RESMAX values'')')ISRESMAX
      endif
C
      write(*,'(/,''input RESMIN'')')
      read(*,*)RGRESMIN
      write(*,'(F12.2)')RGRESMIN
C
      write(*,'(/,''input RESMAX'')')
      read(*,*)RGRESMAX
      write(*,'(F12.2)')RGRESMAX
C
      write(*,'(/,''input refine_data_type switch'')')
      read(*,*)IMERGEDAT
      if(IMERGEDAT.eq.0 .or.
     .   IMERGEDAT.eq.2 .or.
     .   IMERGEDAT.eq.4 .or.
     .   IMERGEDAT.eq.8     )then
        write(*,'(I1,'' = Using Fourier filtered results'')')IMERGEDAT
C-------phaoriFouFilter should not be protected:
        LPROTFOUFIL = .FALSE.
        LUSEML = .FALSE.
      elseif(IMERGEDAT.eq.6) then
        write(*,'(I1,'' = Using ML results'')')IMERGEDAT
C-------phaoriFouFilter should be protected:
        LPROTFOUFIL = .TRUE.
        LUSEML = .TRUE.
      else 
        write(*,'(I1,'' = Using SynRef unbent results '')')IMERGEDAT
C-------phaoriFouFilter should be protected:
        LPROTFOUFIL = .TRUE.
        LUSEML = .FALSE.
      endif
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
      write(*,'(/,''input ILIST switch'')')
      read(*,*)ILIST
      write(*,'(I3)')ILIST
C
      write(*,'(/,''Want to refine switches (y=1,0=no)'')')
      read(*,*)irefswitch
      write(*,'(I3)')irefswitch
C
      if(irefswitch.eq.1)then
        write(*,'('' Refining Switches'')')
C
        write(*,'(/,''input refine modus for revhk   '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing revhk     '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefrevhk)
C
        write(*,'(/,''input refine modus for rot90   '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing rot90     '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefrot90)
C
        write(*,'(/,''input refine modus for rot180  '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing rot180    '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefrot180)
C
        write(*,'(/,''input refine modus for sgnxch  '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing sgnxch    '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefsgnxch)
C
        write(*,'(/,''input refine modus for revhnd  '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing revhnd    '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefrevhnd)
C
        write(*,'(/,''input refine modus for revxsgn '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing revxsgn   '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefrevxsgn)
C
        write(*,'(/,''input refine modus for invert_tiltangle '')')
        read(*,*)CMODUS
        call shorten(CMODUS,k)
        write(*,'(''Testing invert_tiltangle '',A)')CMODUS(1:k)
        call norminvert(CMODUS,irefinvtangl)
C
      endif
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
        write(*,'(/,'':Creating refinement script '',A)')cline(1:k1)
        open(11,FILE=cline,STATUS='NEW',ERR=900)
C
        call shorten(cname5,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname5(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''\rm -f '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
        write(*,'('':Creating postprocessing script '',A)')cline(1:k1)
        open(14,FILE=cline,STATUS='NEW',ERR=900)
C
        write(11,'(''#!/bin/csh -ef'')')
        write(11,'(''#'')')
        write(11,'(''rm -f fort.3'')')
        write(11,'(''#'')')
C
        if(igenref.eq.1)then
          write(11,'(''echo dummy > APH/REF1.hkl'')')
          write(11,'(''rm -f APH/REF*.hkl'')')
          write(11,'(''#'')')
        endif
C
        write(14,'(''#!/bin/csh -ef'')')
        write(14,'(''#'')')
        write(14,'(''pwd'')')
        write(14,'(''#'')')
C
        write(14,'(''setenv NCPUS '',I3)')ITHRNUM
        write(14,'(''#'')')
C
        call shorten(cprocdir,k3)
        write(11,'(''set proc_2dx = "'',A,''"'')')cprocdir(1:k3)
        call shorten(cbindir,k)
        write(11,'(''set bin_2dx = "'',A,''"'')')cbindir(1:k)
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
        write(cline,'(F12.3)')rzwin
        call shortshrink(cline,k)
        write(11,'(''set ZSTARWIN = "'',A,''"'')')cline(1:k)
        write(11,'(''set IAQP2 = 0'')')
        write(11,'(''set IVERBOSE = "'',I1,''"'')')IVERBOSE
        write(cline,'(I8)')IBOXPHS
        call shortshrink(cline,k)
        write(11,'(''set IBOXPHS = "'',A,''"'')')cline(1:k)
        write(cline,'(F12.3)')RFACAMP
        call shortshrink(cline,k)
        write(11,'(''set RFACAMP = "'',A,''"'')')cline(1:k)
        write(cline,'(I6)')NPRG
        call shortshrink(cline,k)
        write(11,'(''set NPRG = "'',A,''"'')')cline(1:k)
        write(11,'(''set NTL  = "'',A1,''"'')')cNTL
        write(11,'(''set ITAXASTEP = "'',I8,''"'')')ITAXASTEP
        write(11,'(''set RTAXASIZE = "'',F12.3,''"'')')RTAXASIZE
        write(11,'(''set ITANGLSTEP = "'',I8,''"'')')ITANGLSTEP
        write(11,'(''set RTANGLSIZE = "'',F12.3,''"'')')RTANGLSIZE
        write(11,'(''set NBM  = "'',A1,''"'')')cNBM
        write(11,'(''set ILIST = '',I3)')ILIST
        write(cline,'(I6)')IQMX
        call shortshrink(cline,k)
        write(11,'(''set IQMX = "'',A,''"'')')cline(1:k)
        write(cline,'(I6)')IHKMX
        call shortshrink(cline,k)
        write(11,'(''set HKMX = "'',A,''"'')')cline(1:k)
        write(11,'(''#'')')
        write(11,'(''set LOGOUTPUT = T'')')
        write(11,'(''#'')')
C
        if(NPRG.eq.3)then
          write(11,'(''# 3D reference:'')')
          if ( IMERGREF.eq.1 ) then
            write(11,'(''setenv HKLIN merge3Dref_MRClefthanded.mtz'')')
          elseif ( IMERGREF.eq.2 ) then
            write(11,'(''setenv HKLIN merge3Dref_BackProject_MRClefthanded.mtz'')')
          elseif ( IMERGREF.eq.3 ) then
            write(11,'(''setenv HKLIN merge3Dref_Refined_MRClefthanded.mtz'')')
          endif  
          write(11,'(''setenv OMP_NUM_THREADS '',I4)')ITHRNUM
          write(11,'(''#'')')
        endif
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
        write(11,'(''${spcgrp} ${NPRG} ${NTL} ${NBM} ${ILIST}'',
     .    '' ${realcell} ${ALAT} ${realang} 0 15 ${IAQP2} ${IVERBOSE} '',
     .    ''${LOGOUTPUT} '',
     .    '' ! ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,'',
     .    ''IVERBOSE,LOGOUTPUT'')')
        write(11,'(''${ITAXASTEP} ${RTAXASIZE} ${ITANGLSTEP}'',
     .    '' ${RTANGLSIZE}'',10X,
     .    ''! itaxastep,rtaxasize,itanglstep,rtanglsize'')')
C
        write(11,'(''1001 0 ${HKMX} ${IQMX} ${IBOXPHS} '',A1,
     .    '' F ${RFACAMP}'',22X,
     .    ''! IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP'')')
     .    CNREFOUT
C
        if(NPRG.eq.1)then
          write(11,'(''0000001001 Merge'')')
          write(11,'(''APH/merge.aph'')')
        else if(NPRG.eq.3)then
          write(11,'(''LABIN AMP=F SIG=SIGF PHASE=PHI FOM=FOM'')')
        else
          write(*,'(''::'',79(''#''))')
          write(*,'(''::'',79(''#''))')
          write(*,'(''::ERROR: NPRG of '',I3,'' not supported here'')')
     .      NPRG
          write(*,'(''::'',79(''#''))')
          write(*,'(''::'',79(''#''))')
          stop
        endif
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
          open(12,FILE=cname3,STATUS='OLD',ERR=910)
C
          call cgetline(CIMAGENAME,"imagename")
          call cgetline(CIMAGENUMBER,"imagenumber")
C
          read(CIMAGENUMBER,*)imnum(imtotalcountcount)
C         write(*,'(''::imagenumber read = '',I10)')imnum(imtotalcountcount)
          if(imtotalcountcount.gt.1)then
            do i=1,imtotalcountcount-1
              if(imnum(i).eq.imnum(imtotalcountcount))then
                call shorten(CIMAGENAME,k)
                write(*,'(''WARNING: Imagenumber '',I10,
     .            '' appears twice, here for image '',A)')imnum(i),CIMAGENAME(1:k)
              endif
            enddo
          endif
C
          call shorten(CIMAGENAME,k)
          write(CTITLE(1:80),'('' Imagename = '',A)')CIMAGENAME(1:k)
          call rgetline(RESMAX,"RESMAX")
          if(RESMAX.lt.RGRESMAX)RESMAX=RGRESMAX
          call rgetline(RESMIN,"RESMIN")
          if(RESMIN.gt.RGRESMIN)RESMIN=RGRESMIN
          call rgetline(RCS,"CS")
          call rgetline(RKV,"KV")
          call cgetline(CBMTLT,"beamtilt")
          read(CBMTLT,*)RTX,RTY
          call cgetline(CLATTICE,"lattice")
          call rgetline(RTAXA,"TAXA")
          call rgetline(RTANGL,"TANGL")
          if(irefswitch.eq.1 .and. irefinvtangl.eq.1)then
            RTANGL=-RTANGL
          endif
C
          call dgetline(CMLMERGE,"ML_use_for_merging",iok)
C
          if(CMLMERGE(1:1).eq."y" .and. IMERGEDAT.eq.6)then
            call cgetline(CPHORI,"phaori_ML")
            read(CPHORI,*,ERR=901)RPHAORIH,RPHAORIK
            goto 902
 901        continue
            RPHAORIH=0.0
            RPHAORIK=0.0
 902        continue
            write(*,'(''   using Single Particle  results, '',
     .        ''PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
          else
            call cgetline(CPHORI,"phaori")
            read(CPHORI,*,ERR=903)RPHAORIH,RPHAORIK
            goto 904
 903        continue
            RPHAORIH=0.0
            RPHAORIK=0.0
 904        continue
            write(*,'(''   using Fourier filtered results, '',
     .        ''PhaseOrigin = '',2F12.3)')RPHAORIH,RPHAORIK
          endif
          if(imcount.eq.1)then
C-----------First film is used as is, without rescaling
            RSCL=1.0
          else
C-----------RSCL=0.0 means scaling is automatic for following datasets
            RSCL=0.0
          endif
          call igetline(IROT90,"rot90")
          if(irefswitch.eq.1 .and. irefrot90.eq.1)then
            IROT90=1-IROT90
          endif
          call igetline(IROT180,"rot180")
          if(irefswitch.eq.1 .and. irefrot180.eq.1)then
            IROT180=1-IROT180
          endif
          call igetline(IREVHK,"revhk")
          if(irefswitch.eq.1 .and. irefrevhk.eq.1)then
            IREVHK=1-IREVHK
          endif
          call igetline(ICTFREV,"ctfrev")
          call igetline(IREVHND,"revhnd")
          if(irefswitch.eq.1 .and. irefrevhnd.eq.1)then
            IREVHND=1-IREVHND
          endif
          call igetline(IREVXSGN,"revxsgn")
          if(irefswitch.eq.1 .and. irefrevxsgn.eq.1)then
            IREVXSGN=1-IREVXSGN
          endif
          call igetline(ISGNXCH,"sgnxch")
          if(irefswitch.eq.1 .and. irefxgnxch.eq.1)then
            ISGNXCH=1-ISGNXCH
          endif
C
          close(12)
C
          call shorten(CTITLE,k)
          k1=1
          if(k.gt.40)k1=41-k
          write(11,'(A10,A40)')CIMAGENUMBER(1:10),CTITLE(k1:k)
C
          call shorten(cdir,k)
          call shortshrink(CIMAGENAME,k1)
          if(CMLMERGE(1:1).eq."y" .and. IMERGEDAT.eq.6)then
            write(cname4,'(A,''/APH/ML_result.aph'')')
     .        cdir(1:k)
          else
            write(cname4,'(A,''/APH/image_ctfcor_ctf.aph'')')
     .        cdir(1:k)
          endif
          call shortshrink(cname4,k1)
          write(11,'(A)')cname4(1:k1)
          write(11,'(''  F'',62X,''! NWGT'')')
          write(11,'(2F12.3,'' 1'',39X,''! TAXA,TANGL,IORIGT'')')
     .      RTAXA,RTANGL
C
          call shortshrink(CLATTICE,k)
          write(11,'(A40,25X,''! lattice'')')
     .       CLATTICE(1:k)
C
          write(11,'(4F10.3,I3,F9.5,6I2,L2,L2,
     .       '' ! OH,OK,STEP,WIN,SGNXCH,SCL,ROT,REV,'',
     .       ''CTFREV,ROT90,REVHND,REVSGN,LPROTFOUFIL,LUSEML'')')
     .       RPHAORIH,RPHAORIK,RPHSTEP,RZWIN,ISGNXCH,RSCL,IROT180,IREVHK,ICTFREV,
     .       IROT90,IREVHND,IREVXSGN,
     .       LPROTFOUFIL,LUSEML
C
          write(11,'(4F12.3,''                 ! cs,kv,tx,ty'')')
     .      RCS,RKV,RTX,RTY
          if(ISRESMAX.eq.0)then
            write(11,'(2F12.3,42X,''! resolution limits'')')
     .        RGRESMIN,RGRESMAX
          else
            write(11,'(2F12.3,42X,''! resolution limits'')')
     .        RESMIN,RESMAX
          endif
C
          write(14,'(''#'')')
          write(14,'(''if ( -e APH/REF'',A10,''.hkl ) then'')')
     .      CIMAGENUMBER(1:10)
          call shorten(cdir,k1)
          write(14,'(''  if ( ! -d '',A,''/APH ) then'')')cdir(1:k1)
          write(14,'(''    \mkdir '',A,''/APH'')')cdir(1:k1)
          write(14,'(''  endif'')')
          write(14,'(''  \cp -f APH/REF'',A10,''.hkl '',A,
     .      ''/APH'')')
     .      CIMAGENUMBER(1:10),cdir(1:k1)
C
C          write(14,'(''  echo "# IMAGE: APH/REF'',A10,''.hkl <APH: REF'',A10''.hkl>" >> LOGS/${scriptname}.results'')')
C     .      CIMAGENUMBER(1:10),CIMAGENUMBER(1:10)
C
          write(14,'(''  echo REF'',A10,''.hkl done.'')') 
     .      CIMAGENUMBER(1:10)
          write(14,'(''else'')')
          write(14,'(''  echo "::WARNING: APH/REF'',A10,
     .      ''.hkl not existing."'')')CIMAGENUMBER(1:10)
          write(14,'(''endif'')')
C
          goto 100
C
 200    continue
C
        write(14,'(''wait'')')
        write(11,'(''        -1'')')
        write(11,'(''eot'')')
        write(11,'(''#'')')
        write(11,'(''#'')')
        close(11)
        close(14)
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
        call shorten(cname5,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname5(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
      enddo
      goto 220
C
 210  continue
        write(14,'(''wait'')')
        write(11,'(''        -1'')')
        write(11,'(''eot'')')
        write(11,'(''#'')')
        write(11,'(''#'')')
        close(11)
        close(14)
C
        call shorten(cname2,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname2(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
        call shorten(cname5,k)
        write(cline,'(''SCRATCH/job_'',I2,''_'',A)')ithread,cname5(1:k)
        if(ithread.lt.10)write(cline(13:13),'(''0'')')
        call shorten(cline,k1)
        write(clin2,'(''chmod +x '',A)')cline(1:k1)
        call shorten(clin2,k)
        call system(clin2(1:k))
C
 220  continue
C
      close(10)
C
C
      goto 999
C
 900  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in 2dx_merge_compileB'')')
        call shorten(cerrmes,k)
        write(*,'(''::File '',A,'' not opened.'')')cerrmes(1:k)
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
     .     ''in 2dx_merge_compileB'')')
        call shorten(cname3,k)
        write(*,'(''::Tried to open:'',A)')cname3(1:k)
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
      read(cline(9+k:l-1),*,ERR=800,END=800)ival
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
      read(cline(9+k:l-1),*,ERR=800,END=800)rval
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
      SUBROUTINE norminvert(cval,iref)
C
      CHARACTER * (*) cval
C
      if(cval(1:3).eq."inv")then
        iref=1
      else
        iref=0
      endif
C
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





