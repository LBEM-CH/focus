      PROGRAM HKLSYM4

C       Henning Stahlberg, 16.5.2000
C       updated 10/21/2012

      IMPLICIT NONE
C
      INTEGER MAXSPOT
      PARAMETER (MAXSPOT = 100)
C
C-----NFIELD has to be NFIELD=5*(MAXSPOT*2+1)**3
      INTEGER NFIELD
      PARAMETER (NFIELD = 40603005)
C
C-----MFIELD has to be MFIELD=(MAXSPOT*2+1)**3
      INTEGER MFIELD
      PARAMETER (MFIELD = 8120601)
C
      CHARACTER*200  TITLE 
      CHARACTER*200 cline1,cline2,cline3,cline4
      INTEGER H,K,L,i1sig,icount,ihmax,ikmax,ilmax,ineg,isig
      INTEGER iheader,inum,iwasym,N,ispc
      INTEGER IFILL
      INTEGER IFAIL,JFAIL
      REAL AMP,PHASE,FOM,SIGA,BACK,PI,PX,PY,PHERR
      REAL AMPWGTSUM,PHSWGTSUM,AMPSUM
      REAL SIGMA,WT,R1,R2
      REAL*8 XARG,S18AEF,S18AFF
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,5)
      INTEGER IOUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
C
      COMMON // isig
C
C-----The following table comes from the ALLSPACE program:
C
C-----------ROUTP contains AMP,PHS,BACK,FOM,SIGA
C 
C Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C
C   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C ref in
C  prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
C               K=                     -k +k -k +k                         JK180
C
C  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
C  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
C  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
C 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180
C 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
C 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
C 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
C 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
C 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
C 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
C 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
C 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
C 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
C
C
C-----Fortran counts indices starting with 1 (not with 0 as in C)
C
C-----Fortran matrices are (columns,rows)
C   (1,1)  (1,2)  (1,3)  (1,4)  (1,5)
C   (2,1)  (2,2)  (2,3)  (2,4)  (2,5)
C   (3,1)  (3,2)  (3,3)  (3,4)  (3,5)
C
C-----Fortran stores fields by columns
C in the above example, array element (1,2) will follow element (3,1).
C
C ISYMFIELD(1:15, of spacegroup=p22121) is 4,4,1,0,0,0,0,0,0,0,0,0,0,0,0,
C ISYMFIELD(16, of spacegroup=p22121) is 1, meaning JH180=1
C ISYMFIELD(17, of spacegroup=p22121) is 1, meaning JK180=1
C
C             ISYMFIELD(OPERATION,SPACEGROUP)
C
      INTEGER ISYMFIELD(17,17)
      DATA ISYMFIELD / 
     . 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
     . 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 2,2,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
     . 4,4,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
     . 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     . 0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,
     . 1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
     . 4,4,1,1,4,4,1,0,0,0,0,0,0,0,0,1,1,
     . 0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,
     . 0,0,0,0,0,0,1,0,1,1,0,1,0,0,1,0,0,
     . 0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,0,0,
     . 0,0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,
     . 0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,0,0  /
C
      DATA ROUTP /NFIELD*0.0/
      DATA IOUTP /MFIELD*0/
C
      PI=3.1415926537
C
      WRITE(*,'('': 2dx_hklsym3, to symmetrize an APH file '')')
C
      write(*,'('' Give input file name'')')
      read(*,'(A)')cline1
      call shorten(cline1,k)
      write(*,'('' read: '',A)')cline1(1:k)
C
      write(*,'('' Give output file name'')')
      read(*,'(A)')cline2
      call shorten(cline2,k)
      write(*,'('' read: '',A)')cline2(1:k)
C
      write(cline4,'(''rm -f '',A)')cline2(1:k)
      call system(cline4)
C
      write(*,'('' Give file name for output without header line'')')
      read(*,'(A)')cline3
      call shorten(cline3,k)
      write(*,'('' read: '',A)')cline3(1:k)
C
      write(cline4,'(''rm -f '',A)')cline3(1:k)
      call system(cline4)
C
      write(*,'('' Input spacegroup'')')
      READ (5,'(I6)') ispc
      WRITE (6,'('':Spacegroup = '',I6)') ispc
C
      write(*,'('' Is there a header line (1=y,0=no)'')')
      read(*,*)iheader
      if(iheader.eq.1)then
        write(*,'('' Assuming header line.'')')
      else
        write(*,'('' Assuming no header line.'')')
      endif
C
      write(*,'('' Is there a SIGA column (0=no,1=y,2=Background,'',
     1   ''3=no and setting SIGA to 1.0)'')')
      read(*,*)isig
      i1sig=isig
      if(isig.eq.1)then
        write(*,'('' Assuming siga column.'')')
      elseif(isig.eq.2)then
        write(*,'('' Assuming Background, FOM columns.'')')
      elseif(isig.eq.3)then
        write(*,'('' Creating siga=1 column.'')')
        isig=1
      else
        write(*,'('' Assuming no siga column.'')')
      endif
C
      write(*,'('' Write out only asymmetric unit (1=y,0=n)'')')
      read(*,*)iwasym
      if(iwasym.eq.1)then
        write(*,'('':: Writing only asymmetric unit.'')')
        write(*,'('':: This is not supported right now.'')')
        stop
      else
        write(*,'('' Writing full p1 plane'')')
      endif
C
      write(*,'('' Write out negative L values also (1=y,0=n)'')')
      read(*,*)ineg
      if(ineg.eq.1)then
        write(*,'('' Writing also negative L values.'')')
      else
        write(*,'('' Writing only positive L values.'')')
      endif
C
C-----Open input and output channels:
C
      open(10,FILE=cline1,STATUS='OLD',ERR=940)
      open(11,FILE=cline2,STATUS='NEW',ERR=950)
      open(12,FILE=cline3,STATUS='NEW',ERR=950)
C
C-----Channel 10 is input.
C-----Channel 11 is output with a header line.
C-----Channel 12 is output without the header line.
C
      if(iheader.eq.1)then
        READ (10,'(A200)') TITLE
        call shorten(TITLE,k)
        WRITE (11,'(A)') TITLE(1:k)
      endif
C
C-----Read the entire input file, and copy the read entries
C-----into the ROUTF field, while also occupying the symmetry-related
C-----positions in that field:
C
      icount=0
      ihmax=0
      ikmax=0
      ilmax=0
 1000 continue
        BACK=0.0
        SIGA=0.0
        if(i1sig.eq.1)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM,SIGA
        elseif(i1sig.eq.2)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,BACK,FOM
        else
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM
        endif
C
        if(FOM.gt.100.0)then
          write(*,'('':: ERROR: FOM greater than 100.'')')
          STOP
        endif
        if(FOM.lt.0.0)then
          write(*,'('':: ERROR: FOM less than 0.'')')
          STOP
        endif
        if(abs(H).gt.MAXSPOT)goto 970
        if(abs(K).gt.MAXSPOT)goto 970
        if(abs(L).gt.MAXSPOT)goto 970
C
C-------Treat only right half of Fourier space (H>=0)
C        if(H.lt.0)then
C          H=-H
C          K=-K
C          L=-L
C          PHASE=-PHASE
C        endif
C
C-------Bring the phase value into the range 0...359.9999
C
        call PHACOR(PHASE)
C
        if(ihmax.lt.abs(H))ihmax=abs(H)
        if(ikmax.lt.abs(K))ikmax=abs(K)
        if(ilmax.lt.abs(L))ilmax=abs(L)
C
        icount=icount+1
C
C-------Place this reflection into the ROUTP field:
C
        call ROUTF(ROUTP,IOUTP, H  ,   K, L,AMP,PHASE,BACK,FOM,SIGA,1)
C
C-------And place also the symmetry-related reflections for this one into ROUTF:
C
C-------Below is an implementation of the ALLSPACE table, using:
C               H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C               K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
C               K=                     -k +k -k +k                         JK180

        call ROUTF(ROUTP,IOUTP,-H  ,   K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(1,ispc))
        call ROUTF(ROUTP,IOUTP, H  ,  -K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(2,ispc))
        call ROUTF(ROUTP,IOUTP,-H  ,  -K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(3,ispc))
        call ROUTF(ROUTP,IOUTP,   K, H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(4,ispc))
        call ROUTF(ROUTP,IOUTP,   K,-H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(5,ispc))
        call ROUTF(ROUTP,IOUTP,  -K, H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(6,ispc))
        call ROUTF(ROUTP,IOUTP,  -K,-H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(7,ispc))
        call ROUTF(ROUTP,IOUTP, H  ,-H-K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(8,ispc))
        call ROUTF(ROUTP,IOUTP,-H  , H+K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(9,ispc))
        call ROUTF(ROUTP,IOUTP,   K,-H-K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(10,ispc))
        call ROUTF(ROUTP,IOUTP,  -K, H+K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(11,ispc))
        call ROUTF(ROUTP,IOUTP,-H-K, H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(12,ispc))
        call ROUTF(ROUTP,IOUTP, H+K,-H  , L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(13,ispc))
        call ROUTF(ROUTP,IOUTP,-H-K,   K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(14,ispc))
        call ROUTF(ROUTP,IOUTP, H+K,  -K, L,AMP,PHASE,BACK,FOM,SIGA,ISYMFIELD(15,ispc))
C
      goto 1000
C
 1005 continue
C
      if(icount.eq.0)then
        goto 980
      endif
C
      write(*,'('':Read '',I8,'' reflections.'')')icount
      write(*,'('':H,K,L max = '',3I8)')ihmax,ikmax,ilmax
C
C================================================================================
C================================================================================
C==== Input file is now read in. Now output section.
C================================================================================
C================================================================================
C
C-----Now write out all reflexes in the correctly sorted order
C
      do H=0,ihmax
        do K=-ikmax,ikmax
          do L=-ilmax,ilmax
C
C-----------Test, if this reflection was present
            IFILL=IOUTP(H,K,L)
C
C-----------output only for right half of Fourier plane (and top half of H=0 line):
            if(H.gt.0 .or. K.ge.0 .and. IFILL.gt.0)then
C
C-------------Get the sum of the weights for AMPs
              AMPWGTSUM=ROUTP(H,K,L,4)
C
C-------------Calculate the average AMPlitude
              AMPSUM=ROUTP(H,K,L,1)
C
              if(AMPWGTSUM.gt.0.0000001)then
                AMP = AMPSUM/AMPWGTSUM
              else
                AMP = 0.0
                write(6,'('':WARNING: FOM of zero for '',3I6)')H,K,L
              endif
C
C              write(6,'('':HKL, AMPSUM,PX,PY,AWGT,PWGT'',3I3,5F12.3)')
C     .          H,K,L,ROUTP(H,K,L,1),ROUTP(H,K,L,2),ROUTP(H,K,L,3),ROUTP(H,K,L,4),ROUTP(H,K,L,5)
C
C-------------Calculate the average PHASE
              PX=ROUTP(H,K,L,2)
              PY=ROUTP(H,K,L,3)
C
              PHASE=atan2(PY,PX)*180.0/PI
C
              call PHACOR(PHASE)
C
C-------------Get the sum of the weights for PHASES
              PHSWGTSUM=ROUTP(H,K,L,5)
              if(PHSWGTSUM.lt.0.01)PHSWGTSUM=0.01
C
C-------------Calculate the average FOM
C
C-------------This is taken from AVRGAMPS, and the following explanation 
C-------------from Richard Henderson in an email to Henning Stahlberg on
C-------------Oct. 20, 2012:
C-------------
C-------------  [...] FOMOUT is the most complicated one.  Here we calculate the vector length
C-------------  given by XARG = SQRT(SUMCOS**2 + SUMSIN**2) where SUMCOS and SUMSIN are
C-------------  the same weighted sums as used to calculate COMBPHASE.  The most important
C-------------  aspect of XARG is that it's magnitude is the combined signal-to-noise
C-------------  ratio of the vectorially added structure factors.  In other words, the
C-------------  vector length is XARG and the standard deviation is 1.0.  The way to get
C-------------  the figure of merit, FOM, from this involves integrating around the circle
C-------------  of radius XARG in Argand space with a standard deviation of 1.0.  This is
C-------------  given by the formula S18AFF/S18AEF.  S18AFF and S18AEF are two NAGLIB
C-------------  subroutines that return the modified Bessel functions I1(x) and I0(x)
C-------------  respectively.  I tabulate below the values given by S18AFF and S18AEF, and
C-------------  the corresponding FOM for a range of value of XARG = signal-to-noise
C-------------  ratios.
C-------------  
C-------------  XARG     S18AFF   S18AEF    FOM
C-------------  0        1        0         0
C-------------  0.5      0.257    1.06      0.24
C-------------  1        0.565    1.26      0.45
C-------------  3        3.9      4.9       0.8
C-------------  6        61       67        0.91
C-------------  8        399      427       0.93
C-------------  10       2670     2800      0.95
C-------------  15       328000   339000    0.97
C-------------  20       42400000 43000000  0.99
C-------------  
C-------------  If you don't want to call anyone else's subroutines, you could just create
C-------------  your own look-up table and put in values that you calculate.  We did it
C-------------  this way so we could get a continuously evaluated function.
C-------------  
C-------------  I hope this is helpful.
C-------------  
C-------------  Richard
C-------------
C
              XARG = SQRT(PX**2 + PY**2)
C
              IFAIL=1
              JFAIL=1
C       
              R1=S18AFF(XARG,JFAIL)
              R2=S18AEF(XARG,IFAIL)
              if(abs(R2).gt.0.00000000001)then
                WT=R1 / R2
              else
                IFAIL=1
              endif
C
C-------------IF ABOVE FAILS, GAUSSIAN WILL DO AS PROBABILITY MUST BE VERY SHARP
C
              IF(JFAIL.NE.IFAIL) WRITE(6,299)H,K,L,JFAIL,IFAIL
 299            FORMAT('::S18AFF or S18AEF failed for spot ',3I5,
     .           ', J/I FAIL=',2I6)
              IF(IFAIL.EQ.1.OR.JFAIL.EQ.1)THEN
                SIGMA=SQRT(1.0/XARG)
                FOM=COS(SIGMA) * 100.0
              ELSE
                FOM=WT * 100.0
              END IF
C
              write(6,'(''::XARG,R1,R2,FOM '',4F12.3)')XARG,R1,R2,FOM
C
C-------------QFACTOR is the length of the vector addition of all vector phases, 
C------------------------divided by the sum of all vector phases.
C-------------If the phases all agreed with each other, QFACTOR will be one.
C-------------Otherwise, QFACTOR is smaller than one.
C
C             QFACTOR=XARG/PHSWGTSUM
C             AMP=AMP*QFACTOR         ! takes account of bad phases
C
C-------------Fudge something together for BACK and SIGA:
              BACK=0.0
              SIGA=1.0
C
              if(i1sig.eq.3)SIGA=1.0
C
C-------------Output the values into the output channels
              if(isig.eq.1)then
                write(11,310) H,K,L,AMP,PHASE,FOM,SIGA
                write(12,310) H,K,L,AMP,PHASE,FOM,SIGA
                if(ineg.gt.1)then
                  write(11,310) -H,-K,-L,AMP,-PHASE,FOM,SIGA
                  write(12,310) -H,-K,-L,AMP,-PHASE,FOM,SIGA
                endif
              elseif(isig.eq.2)then
                write(11,310) H,K,L,AMP,PHASE,BACK,FOM
                write(12,310) H,K,L,AMP,PHASE,BACK,FOM
                if(ineg.gt.1)then
                  write(11,310) -H,-K,-L,AMP,-PHASE,BACK,FOM
                  write(12,310) -H,-K,-L,AMP,-PHASE,BACK,FOM
                endif
              else
                write(11,300) H,K,L,AMP,PHASE,FOM
                write(12,300) H,K,L,AMP,PHASE,FOM
                if(ineg.gt.1)then
                  write(11,300) -H,-K,-L,AMP,-PHASE,FOM
                  write(12,300) -H,-K,-L,AMP,-PHASE,FOM
                endif
              endif
              write(*,'('':H,K,L,AMP,PHASE,BACK,SIGA,FOM,FILL='',
     .          3I4,X,5G11.5,I8)') H,K,L,AMP,PHASE,BACK,SIGA,FOM,IFILL
            endif
          enddo
        enddo
      enddo
C
 300  FORMAT (3I6,G16.8,G16.8,G16.8)
 310  FORMAT (3I6,G16.8,G16.8,G16.8,G16.8)
C
      goto 999
C
 940  continue
      write(*,'('':: ERROR while opening input file.'')')
      goto 999
C
 950  continue
      write(*,'('':: ERROR while opening output file.'')')
      goto 999
C
 960  continue
      write(*,'('':: ERROR in input data. No FOM values.'')')
      goto 999
C
 970  continue
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      goto 999
C
 980  continue
      write(*,'('':: ERROR in input data. No data.'')')
      goto 999
C
 999  continue
C
      close(10)
      close(11)
      close(12)
C
C-----Produce a Look-Up-Table for WT(XARG):
C     call system("\rm -f WT-TABLE.txt")
C     open (18,FILE='WT-TABLE.txt',STATUS='NEW')
C     xarg=0.0
C     do H=1,2000
C       xarg=xarg+0.01
C       IFAIL=1
C       JFAIL=1
C       R1=S18AFF(XARG,JFAIL)
C       R2=S18AEF(XARG,IFAIL)
C       WT=R1 / R2
C       write(18,'(2F16.6)')XARG,WT
C     enddo
C     close(18)
C
      STOP

      END     
C
C==========================================================
C
      SUBROUTINE ROUTF(ROUTP,IOUTP,H,K,L,AMP,PHASE,BACK,FOM,SIGA,ISHIFT)
C
C fill in the AMP,PHASE,BACK,FOM,SIGA into ROUTP field
C
C IPHSHIFT is defined as 
C   0 = -  not comparable       
C   1 = 1  directly identical
C   2 = H  differ by 180 * H 
C   3 = K  differ by 180 * K 
C   4 = HK differ by 180 * (H+K)
C
C  Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C
C All reflections are added to a giant matrix REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,8)
C to all possible symmetry-related positions.
C
C By doing so, the ISYMFIELD(column,spacegroupnumber) decides, if a reflection should be added, 
C and if so, if there should be a phase shift of 180 or not.
C
C This all ends up in the table ROUTP, whereby the positions for each H,K,L reflection mean:
C  1 = sum of weighted amplitudes
C  2 = sum of weighted os component of phase
C  3 = sum of weighted in component of phase
C  4 = sum of amp_weights
C  5 = sum of phase_weights
C
C The field IOUTP counts how many entries were added to each HKL spot
C
C                                        
      IMPLICIT NONE
C
      INTEGER MAXSPOT
      PARAMETER (MAXSPOT = 100)
C
      INTEGER H,K,L
      INTEGER ISHIFT,IMAXSPOT
      INTEGER isig
      REAL AMP,PHASE,BACK,FOM,SIGA,PX,PY,RPT,PI
      REAL RAMP,RBACK,RFOM,RSIGA,AMPWGT,PHSWGT
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,5)
      INTEGER IOUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
C
      COMMON // isig
C
      if(ISHIFT.eq.0)then
        RETURN
      endif
C
      PI=3.1415926537
C
      if ((abs(H).gt.MAXSPOT) .or.
     .    (abs(K).gt.MAXSPOT) .or.
     .    (abs(L).gt.MAXSPOT)      ) then
        IMAXSPOT = MAXSPOT
        write(6,'('':: ERROR: Too big index for ROUTP. Increase MAXSPOT'')')
        write(6,'('':: MAXSPOT = '',I8,'', H,K,L = '',3I6)')
     .    IMAXSPOT,H,K,L
        STOP 'too big index for ROUTP. Increase MAXSPOT'
      endif
C      
      if(ISHIFT.eq.2)then
        PHASE=PHASE+H*180.0
      endif
      if(ISHIFT.eq.3)then
        PHASE=PHASE+K*180.0
      endif
      if(ISHIFT.eq.4)then
        PHASE=PHASE+(H+K)*180.0
      endif
      call PHACOR(PHASE)
C
      PHSWGT=FOM/100.0
      RPT=PHASE*PI/180.0
C
      PX=cos(RPT)*PHSWGT
      PY=sin(RPT)*PHSWGT
C
      if(isig.ne.2)then
        AMPWGT = PHSWGT 
      else
        if(BACK.gt.0.0)then
          AMPWGT = 1.0 / (BACK**2)
        else
          write(6,'(''::ERROR: BACK value of zero for HKL='',3I6)')H,K,L
          STOP '::ERROR in 2dx_hklsym4'
        endif
      endif
C
      RAMP=AMP*AMPWGT
C
C-----Store AMP and PHASE
      ROUTP( H, K, L,1) = ROUTP( H, K, L,1) + RAMP
      ROUTP( H, K, L,2) = ROUTP( H, K, L,2) + PX
      ROUTP( H, K, L,3) = ROUTP( H, K, L,3) + PY
      ROUTP( H, K, L,4) = ROUTP( H, K, L,4) + AMPWGT
      ROUTP( H, K, L,5) = ROUTP( H, K, L,5) + PHSWGT
C
      IOUTP( H, K, L) = IOUTP( H, K, L) + 1.0
C
C-----Also fill Friedel symmetric spots:
      ROUTP(-H,-K,-L,1) = ROUTP(-H,-K,-L,1) + RAMP
      ROUTP(-H,-K,-L,2) = ROUTP(-H,-K,-L,2) + PX
      ROUTP(-H,-K,-L,3) = ROUTP(-H,-K,-L,3) - PY
      ROUTP(-H,-K,-L,4) = ROUTP(-H,-K,-L,4) + AMPWGT
      ROUTP(-H,-K,-L,5) = ROUTP(-H,-K,-L,5) + PHSWGT
C
      IOUTP(-H,-K,-L) = IOUTP(-H,-K,-L) + 1.0
C
      RETURN
C
      END
C
C=========================================================
C
      SUBROUTINE PHACOR(PHASE)
C
 100  continue
        if(PHASE.lt.0.0)then
          PHASE=PHASE+360.0
          goto 100
        endif
C
 200  continue
        if(PHASE.gt.360.0)then
          PHASE=PHASE-360.0
          goto 200
        endif
C
      if(PHASE.lt.-179.9999 .and. PHASE.gt.-180.0001)PHASE=180.0
      if(PHASE.gt. 179.9999 .and. PHASE.lt. 180.0001)PHASE=180.0
      if(PHASE.gt. 359.9999 .and. PHASE.lt. 360.0001)PHASE=  0.0
      if(PHASE.gt.  -0.0001 .and. PHASE.lt.   0.0001)PHASE=  0.0
C
      RETURN
C
      END
C
c==========================================================
c
      SUBROUTINE SHORTEN(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2
      CTMP2=' '
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C

