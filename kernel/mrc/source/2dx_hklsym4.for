      PROGRAM HKLSYM4

C       Henning Stahlberg, 16.5.2000
C       updated 10/29/2012
C
C-----I am thankful to Richard Henderson for detailed and very patient
C-----explanations on how to deal with SNR, IQ, FOM, weights, and XARG.
C-----Henning, Oct. 29, 2012. 

      IMPLICIT NONE
C
      INTEGER MAXSPOT
      PARAMETER (MAXSPOT = 100)
C
C-----NFIELD has to be NFIELD=3*(MAXSPOT*2+1)**3
      INTEGER NFIELD
      PARAMETER (NFIELD = 24361803)
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
      REAL WGTSUM,AMPSUM
      REAL SIGMA,WT,R1,R2,SNRX,SNRY,FOM100SNR
      REAL*8 XARG,S18AEF,S18AFF
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,3)
      INTEGER IOUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
C
      COMMON // isig
C
C-----The following table comes from the ALLSPACE program:
C
C Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW  = number to compare + 180 * M
C       HK differ by 180 * (H+K)        where M = H*JH180 + K*JK180
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
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
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
      WRITE(*,'('': 2dx_hklsym4, to symmetrize an APH file '')')
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
            if(H.gt.0.or.K.ge.0)then
                if(IFILL.gt.0)then
C        
C                       1 = sum of amplitudes
C                       2 = sum of XARG-weighted os component of phase
C                       3 = sum of XARG-weighted in component of phase
C        
C        -------------Calculate the average AMPlitude
C        -------------Here, this is the linear average of AMPS, as 
C        -------------Average_AMP=sum(AMP)/N
C        
                      AMPSUM=ROUTP(H,K,L,1)
                      AMP=AMPSUM/IFILL
C        
C        -------------Calculate the average PHASE
C        -------------Here, this is the angle of the added phase vectors.
                      PX=ROUTP(H,K,L,2)
                      PY=ROUTP(H,K,L,3)
                      PHASE=atan2(PY,PX)*180.0/PI
                      call PHACOR(PHASE)
C        
C        -------------Calculate the average FOM
C        -------------Here, this is calculated as
C        -------------Average_FOM=BESSEL_Ratio(sqrt(sum(cos(PHASE)*XARG)**2+sum(sin(PHASE)*XARG)**2))
C        -------------whereby XARG is taken from a look-up table as a function of the FOM values of the reclections.
C        
                      XARG = SQRT(PX**2 + PY**2)
                      if(XARG.gt.49.0)XARG=49.0
C        
                      IFAIL=1
                      JFAIL=1
C               
                      R1=S18AFF(XARG,JFAIL)
                      R2=S18AEF(XARG,IFAIL)
                      if(abs(R2).gt.0.0)then
                        WT=R1 / R2
                      else
                        IFAIL=1
                      endif
C        
C        -------------IF ABOVE FAILS, GAUSSIAN WILL DO AS PROBABILITY MUST BE VERY SHARP
C        
                      IF(JFAIL.NE.IFAIL) WRITE(6,299)H,K,L,JFAIL,IFAIL
299                     FORMAT('::S18AFF or S18AEF failed for spot ',
     .                   3I5,', J/I FAIL=',2I6)
                      IF(IFAIL.EQ.1.OR.JFAIL.EQ.1)THEN
                        SIGMA=SQRT(1.0/XARG)
                        FOM=COS(SIGMA) * 100.0
                      ELSE
                        FOM=WT * 100.0
                      END IF
C        
C                      write(6,'(''::H,K,L,XARG,R1,R2,FOM '',3I5,4G18.6)')H,K,L,XARG,R1,R2,FOM
C        
C        -------------QFACTOR is the length of the vector addition of all vector phases, 
C        ------------------------divided by the sum of all vector phases.
C        -------------If the phases all agreed with each other, QFACTOR will be one.
C        -------------Otherwise, QFACTOR is smaller than one.
C        
C                     QFACTOR=XARG/PHSWGTSUM
C                     AMP=AMP*QFACTOR         ! takes account of bad phases
C        
C        -------------Fudge something together for BACK and SIGA:
                      BACK=0.0
                      SIGA=1.0
C        
                      if(i1sig.eq.3)SIGA=1.0
C        
C        -------------Output the values into the output channels
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
                      write(*,'('':H,K,L,AMP,PHASE,BACK,'',
     .                ''SIGA,FOM,FILL='',
     .                3I4,X,5G11.5,I8)') 
     .                H,K,L,AMP,PHASE,BACK,SIGA,FOM,IFILL
                endif
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
C All reflections are added to a giant matrix REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,3)
C to all possible symmetry-related positions.
C
C By doing so, the ISYMFIELD(column,spacegroupnumber) decides, if a reflection should be added, 
C and if so, if there should be a phase shift of 180 or not.
C
C This all ends up in the table ROUTP, whereby the positions for each H,K,L reflection mean:
C  1 = sum of amplitudes
C  2 = sum of XARG-weighted os component of phase
C  3 = sum of XARG-weighted in component of phase
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
      REAL PAMPX,PAMPY,SNR
      REAL RAMP,RBACK,RFOM,RSIGA,AMPWGT,PHSWGT,XARG
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,3)
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
        write(6,'('':: ERROR: Too big index for ROUTP. '',
     .    ''Increase MAXSPOT'')')
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
      call FOM2XARG(FOM,XARG)
C
      RPT=PHASE*PI/180.0
C
      PX=cos(RPT)*XARG
      PY=sin(RPT)*XARG
C
C-----Store AMP and PHASE
      ROUTP( H, K, L,1) = ROUTP( H, K, L,1) + AMP
      ROUTP( H, K, L,2) = ROUTP( H, K, L,2) + PX
      ROUTP( H, K, L,3) = ROUTP( H, K, L,3) + PY
C
      IOUTP( H, K, L) = IOUTP( H, K, L) + 1
C
C-----Also fill Friedel symmetric spots:
      ROUTP(-H,-K,-L,1) = ROUTP(-H,-K,-L,1) + AMP
      ROUTP(-H,-K,-L,2) = ROUTP(-H,-K,-L,2) + PX
      ROUTP(-H,-K,-L,3) = ROUTP(-H,-K,-L,3) - PY
C
      IOUTP(-H,-K,-L) = IOUTP(-H,-K,-L) + 1
C
      RETURN
C
      END
C
C=========================================================
C
      SUBROUTINE FOM2XARG(FOM,XARG)
C
C-----This routine returns XARG as a function of FOM.
C-----XARG will be used for the Bessel function ratio.
C
C-----Here, FOM should be between 0 and 100.
C
C
C
C-----IQ   S/Nratio   AMP/BACK     FOM    IQ-Weight
C-----------------------------------------------------
C-----1       7          7        0.998     49.00
C-----2      3.5        3.5       0.982     27.56
C-----3      2.3        2.3       0.939      8.51
C-----4      1.7        1.7       0.870      4.17
C-----5      1.4        1.4       0.763      2.48
C-----6      1.15       1.15      0.630      1.65
C-----7      1.0        1.0       0.505      1.17
C-----8      0.5        0.5       0.124      0.25
C-----9       0          0        0.000      0.00
C
C
C-----XARG     S18AFF   S18AEF    FOM
C-----        (Bessel Functions)
C--------------------------------------
C-----20       42400000 43000000  0.9
C-----15       328000   339000    0.97
C-----10       2670     2800      0.95
C-----8        399      427       0.93
C-----6        61       67        0.91
C-----3        3.9      4.9       0.8
C-----1        0.565    1.26      0.45
C-----0.5      0.257    1.06      0.24
C-----0        1        0         0
C
C
      real FOMXARG(202)
      DATA FOMXARG / 
C----------   FOM          XARG
     .      0.000000,    0.000000,
     .      1.049942,    0.021000,
     .      2.152001,    0.043050,
     .      3.308313,    0.066202,
     .      4.521002,    0.090513,
     .      5.792169,    0.116038,
     .      7.123854,    0.142840,
     .      8.518017,    0.170982,
     .      9.976499,    0.200531,
     .     11.500977,    0.231558,
     .     13.092929,    0.264136,
     .     14.753577,    0.298342,
     .     16.483826,    0.334260,
     .     18.284201,    0.371973,
     .     20.154779,    0.411571,
     .     22.095135,    0.453150,
     .     24.104219,    0.496807,
     .     26.180330,    0.542647,
     .     28.320995,    0.590780,
     .     30.522934,    0.641319,
     .     32.781960,    0.694385,
     .     35.092937,    0.750104,
     .     37.449760,    0.808609,
     .     39.845322,    0.870039,
     .     42.271519,    0.934541,
     .     44.719318,    1.002268,
     .     47.178829,    1.073382,
     .     49.639431,    1.148051,
     .     52.089916,    1.226453,
     .     54.518742,    1.308776,
     .     56.914192,    1.395215,
     .     59.264709,    1.485975,
     .     61.559139,    1.581274,
     .     63.787025,    1.681338,
     .     65.938858,    1.786404,
     .     68.006363,    1.896724,
     .     69.982666,    2.012561,
     .     71.862434,    2.134189,
     .     73.641998,    2.261898,
     .     75.319344,    2.395993,
     .     76.894043,    2.536792,
     .     78.367195,    2.684632,
     .     79.741211,    2.839863,
     .     81.019623,    3.002856,
     .     82.206879,    3.173999,
     .     83.308060,    3.353699,
     .     84.328720,    3.542383,
     .     85.274574,    3.740502,
     .     86.151398,    3.948527,
     .     86.964813,    4.166953,
     .     87.720207,    4.396301,
     .     88.422630,    4.637116,
     .     89.076744,    4.889971,
     .     89.686783,    5.155470,
     .     90.256584,    5.434243,
     .     90.789612,    5.726955,
     .     91.288910,    6.034302,
     .     91.757240,    6.357017,
     .     92.197037,    6.695868,
     .     92.610481,    7.051661,
     .     92.999527,    7.425243,
     .     93.365936,    7.817505,
     .     93.711273,    8.229380,
     .     94.037003,    8.661849,
     .     94.344406,    9.115941,
     .     94.634712,    9.592737,
     .     94.908974,   10.093374,
     .     95.168236,   10.619042,
     .     95.413422,   11.170993,
     .     95.645378,   11.750542,
     .     95.864914,   12.359069,
     .     96.072754,   12.998022,
     .     96.269608,   13.668922,
     .     96.456085,   14.373368,
     .     96.632813,   15.113036,
     .     96.800323,   15.889687,
     .     96.959152,   16.705170,
     .     97.109779,   17.561428,
     .     97.252663,   18.460498,
     .     97.388214,   19.404522,
     .     97.516861,   20.395748,
     .     97.638962,   21.436534,
     .     97.754875,   22.529360,
     .     97.864922,   23.676827,
     .     97.969421,   24.881667,
     .     98.068687,   26.146749,
     .     98.162964,   27.475085,
     .     98.252541,   28.869838,
     .     98.337639,   30.334329,
     .     98.418518,   31.872044,
     .     98.495361,   33.486644,
     .     98.568420,   35.181975,
     .     98.637848,   36.962072,
     .     98.703857,   38.831174,
     .     98.766617,   40.793731,
     .     98.826294,   42.854415,
     .     98.883026,   45.018134,
     .     98.936981,   47.290039,
     .     98.988304,   49.675538,
     .     99.037102,   52.180313,
     .     99.083527,   54.810326  /
C
      itest=1
 100  continue
        if(FOM.lt.FOMXARG(itest))then
          XARG=FOMXARG(itest+1)
        else
          itest=itest+2
          if(itest.lt.200)then
            goto 100
          else
            XARG=54.81
          endif
        endif
      return
      end
C
C=================================================================================
C
      SUBROUTINE prepdata
C
      REAL*8 XARG,S18AEF,S18AFF
C
C-----This subroutine is usually not called.
C-----It was called only once, to produce the FOM vs XARG values above.
C
C-----Use the following to produce a Look-Up-Table for FOM(XARG):
C
      call system("\rm -f WT-TABLE.txt")
      open (18,FILE='WT-TABLE.txt',STATUS='NEW')
      rtmp = 0.0
      xarg=rtmp
      istep=100
      xmax=100.0
      do H=0,istep
        IFAIL=1
        JFAIL=1
        R1=S18AFF(XARG,JFAIL)
        R2=S18AEF(XARG,IFAIL)
        if(abs(R2).gt.0.0)then
          WT=R1 / R2
        else
          IFAIL=1
        endif
C
C-------IF ABOVE FAILS, GAUSSIAN WILL DO AS PROBABILITY MUST BE VERY SHARP
C
        IF((JFAIL.NE.IFAIL).or.
     .     (IFAIL.EQ.1.OR.JFAIL.EQ.1))THEN
          WRITE(6,'(''S18AFF or S18AEF failed for XARG= '',F12.3,'',  R1='',F12.3,
     .      '',  R2='',F12.3,'' IFAIL='',I1,'', JFAIL='',I1)')XARG,R1,R2,IFAIL,JFAIL
        endif
        IF(IFAIL.EQ.1.OR.JFAIL.EQ.1)THEN
          if(XARG.eq.0.0)then
            FOM=0.0
          else
            SIGMA=SQRT(1.0/XARG)
            FOM=COS(SIGMA) * 100.0
          endif
        ELSE
          FOM=WT * 100.0
        END IF
C
C       wwrite(6,'(''::H,K,L,XARG,R1,R2,FOM '',3I5,4G18.6)')H,K,L,XARG,R1,R2,FOM
C
        write(18,'(''     .  '',F12.6,'','',F12.6,'','')')FOM,XARG
C
        xarg=(xarg+0.02)*1.05
      enddo
      close(18)
C
      return
      end
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

