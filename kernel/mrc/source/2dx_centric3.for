c	CENTRIC2.FOR - set average phases to 0 or 180


c	Per Bullough 6-4-94
c                    5-5-94   redetermines figure of merit and residual in resolution ranges
C       Henning Stahlberg, follwing advice from Per Bullough (see below)
C                    04-15-07 Adding treatment for other symmetry groups
C                    08-13-11 Adding output of Phase=zero data
c
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C Per Bullough from the University of Sheffield, UK, answered our questions and 
C came to our help with the following (April 2007):
C
C p2: The standard sg number for p2 in CCP4 is 3 but this has the dyad along Y. 
C Instead you should use number 1003 which has the dyad along Z.
C
C p12_a and p12_b both correspond to p2 with the dyad along X and along Y respectively.
C In CCP4 there is no equivalent of p12_a and in this case you will have to reassign axes. 
C p12_b is equivalent to the CCP4 standard setting for P2, space group number 3. 
C The only REAL amplitudes are along k=0 so CENTRIC should only convert phases to 0 or 180 for k=0.
C You will need to check which convention ORIGTILTD uses but I suspect it is with the dyad along Y.
C
C p121_a and p121_b correspond to 2fold screws (21) along X and Y respectively.  
C The standard CCP4 setting has the screw along Y and is space group number 4. 
C There is no CCP4 equivalent to p121_a. I suspect ORIGTILTD uses the standard setting, 
C but you would need to check. Note also that in CCP4 you can explicitly provide symmetry 
C operators if you want to use non-standard settings, but this is messy- it might be 
C neater just to reassign axes.
C For p121_b CENTRIC should only convert to 0 or 180 for k=0. Note that as it stands, 
C CENTRIC currently converts for all (h.k) values, so you would need to write some new code. 
C In the past in cases such as this we have simply converted by hand since there are so few reflections.
C
C c12_a and c12_b are equivalent to C2 in CCP4. 
C The standard setting has dyad along Y giving space group number 5 in CCP4. 
C There is no equivalent to c12_a so again it is best to swap axes. 
C Again CENTRIC should only apply to k=0.
C
C for p222 CCP4 space group number is 16. CENTRIC should be applied to all reflections.
C
C p2221_a and p2221_b have 2fold screws along X and Y respectively and are equivalent 
C to P2221 in CCP4 - space group number 17. The standard setting has the 2fold screw along Z 
C so either you have to reassign the axes or explicitly give the symmetry operators in CCP4. 
C CENTRIC should be applied to all (h,K).
C
C p22121 in MRC has the twofold down Z. 
C In CCP4 it is called P21212 and has number 18. 
C However, there is also a space group number 1018 which has a 1/4, 1/4, 0 origin shift. 
C I'm not sure what's going on in terms of orgin choice here so I think this space group 
C needs careful testing. CENTRIC applies to all (h,k) values.
C
C c222 is equivalent to C222 in CCP4 (number 21). CENTRIC applies to all (H,K). 
C Note the requirement for systematic absences.
C
C p4 is equivalent to P4 (number 75) in CCP4. CENTRIC applies to all (h,k).
C
C p422 is equivalent to P422 (number 89) in CCP4. CENTRIC applies to all (h,k).
C
C p4212 is equivalent to P4212 in CCP (number 90). CENTRIC applies to all (h,k). 
C Note systematic absences for (2n+1,0) and (0,2n+1).
C
C p3 is equivalent to P3 in CCP4 (number 143). CENTRIC does not apply.
C
C p312 is equivalent to P312 in CCP4 (number 149). CENTRIC applies for h=k only
C
C p321 is equivalent to P321 in CCP4 (number 150). CENTRIC only applies to (h,0) and (0,k).
C
C p6 is equivalent to CCP4 P6 (168). CENTRIC applies to all (h,k)
C
C p622 is equivalent to CCP4 P622 (177). CENTRIC applies to all (h,k)
C
C***************************************************************************************
C
C By the way a VERY IMPORTANT POINT is that space groups with 21 screws or c-centering 
C should have systematic absences e.g. amplitude = 0.0 for and (h,k) = (0, 2n+1) 
C for a 21 axis along Y. This is not taken care of in any of the MRC programs and in 
C the past I have always edited them out by hand. If the image is well behaved these 
C reflections should show up with very high IQ values anyway. 
C However if you get a low IQ for what should be an absent reflection it would be worth 
C flagging this up in your program with a warning.
C
C You should be aware that for all of the space groups CENTRIC only 
C applies to projection data i.e. (h,k,0).
C
C***************************************************************************************
C***************************************************************************************
C***************************************************************************************
C The following table is taken from latlinek.for:
C***************************************************************************************
C***************************************************************************************
C***************************************************************************************
C
C                          SPACE GROUP INFO                             *
C                                                                       *
C   Number      Spacegroup   Asymmetric Unit    Real        Imaginary   *
C                                                                       *
C       1       P1              H>= 0                                   *
C                                                                       *
C       2       P21             H,Z>=0          Z=0                     *
C                                                                       *
C       3       P12             H,K>=0          K=0                     *
C                                                                       *
C       4       P121            H,K>=0          K=0                     *
C                                                                       *
C       5       C12             H,K>=0          K=0                     *
C                                                                       *
C       6       P222            H,K,Z>=0        H=0;K=0;Z=0             *
C                                                                       *
C       7       P2221           H,K,Z>=0        (0,2N,Z)     (0,2N+1,Z) *
C                                               (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                                                       *
C       8       P22121          H,K,Z>=0        (H,K,0)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C       9       C222            H,K,Z>=0        (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                                                       *
C      10       P4              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      11       P422            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                               (H,H,Z)                 *
C                                                                       *
C      12       P4212           H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C      13       P3              H,K>=0                                  *
C                                                                       *
C      14       P312            H,K>=0          (H,H,Z)                 *
C                               K>=H                                    *
C                                                                       *
C      15       P321            H,K>=0          (H,0,Z)                 *
C                               K>H             (0,K,Z)                 *
C                                                                       *
C      16       P6              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      17       P622            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                                                       *
C************************************************************************
C***************************************************************************************
C***************************************************************************************
C***************************************************************************************
C
C   Table of phase comparisons to be made
C       -  not comparable
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C       C  CENTRIC needed (phase to 0 or 180 for all spots (h,k))
C       SX CENTRIC special treatment required, applied only along X axis (k=0)
C       SD CENTRIC special treatment required, applied only along diagonal (h=k)
C       SP CENTRIC special treatment required, applied only along Plus (h,0) and (0,k)
C
C   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C In MRC progs                                                               CENTRIC
C ALLSPACE/ORIGTILT                                                                 In CCP4
C numb  # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180       numb symb    comment
C               K=                     -k +k -k +k                         JK180
C
C  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -   -    1   P1
C  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   C    3   P2     but dyad along Y. Use CCP4=1003 or reassign axis.
C  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   SX   3   P2
C  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   -    ------     reassign axis, and use CCP4=3 above.
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180  SX   4  P21
C  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -   -    ------     reassign axis, and use CCP4=4 above.
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   SX   5   C2
C  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   -    ------     reassign axis, and use CCP4=5 above.
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -   C    16  P222 
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -   C    17  P2221  CCP4 P2221 has 2fold screw in Z. Reassign axis.
C 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180  C    ------     reassign axis, and use CCP4=17 above.
C 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180  C    18  P21212 (or 1018 needed?)
C 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -   C    21  C222   Note: Systematic absences
C 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -   C    75  P4
C 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -   C    89  P422   
C 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180  -    90  P4212  Note: Systematic absences for (2n+1,0) and (0,2n+1).
C 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -   -    143 P3
C 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -   SD   149 P312
C 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -   SP   150 P321
C 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -   C    168 P6
C 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -   C    177 P622
C
C***************************************************************************************
C***************************************************************************************
C***************************************************************************************
C
      CHARACTER*50  TITLE,CSYM,CTMP
      CHARACTER*200 CINFILE,COUT1,COUT2,COUT3
      CHARACTER*6   CSTAB(21)
      INTEGER H,K,L
      DIMENSION DELTA(-100:100,-100:100)
      DATA CSTAB /
     .  'p1    ',
     .  'p2    ',
     .  'p12_b ',
     .  'p12_a ',
     .  'p121_b',
     .  'p121_a',
     .  'c12_b ',
     .  'c12_a ',
     .  'p222  ',
     .  'p2221b',
     .  'p2221a',
     .  'p22121',
     .  'c222  ',
     .  'p4    ',
     .  'p422  ',
     .  'p4212 ',
     .  'p3    ',
     .  'p312  ',
     .  'p321  ',
     .  'p6    ',
     .  'p622  '/
C
      PI = 3.14159265437
C
      write(*,'('' '')')
      write(*,'('': '')')
      write(*,'('': 2dx_centric2.exe - to center phases if needed'')')
      write(*,'('': '')')
C    
      WRITE(*,'(/,'' Input Input File Name of APH file'')')
      READ(*,'(A)')CINFILE
      call shorten(CINFILE,k)
      WRITE(*,'('' Read: '',A)')CINFILE(1:k)
C
      WRITE(*,'(/,'' Input First Ouput File Name of APH file'')')
      READ(*,'(A)')COUT1
      call shorten(COUT1,k)
      WRITE(*,'('' Read: '',A)')COUT1(1:k)
C
      WRITE(*,'(/,'' Input Second Ouput File Name of APH file'')')
      READ(*,'(A)')COUT2
      call shorten(COUT2,k)
      WRITE(*,'('' Read: '',A)')COUT2(1:k)
C
      WRITE(*,'(/,'' Input Third Ouput File Name of APH file '',
     1        ''for phase=0 output'')')
      READ(*,'(A)')COUT3
      call shorten(COUT3,k)
      WRITE(*,'('' Read: '',A)')COUT3(1:k)
C
      WRITE(*,'(/,'' Input Lattice Vectors of RealCell A,B,ANGLE'')')
      READ (*,*) A,B,RANG
      WRITE(*,'('' Read: '',3F12.3)')A,B,RANG
C
      if(abs(RANG).lt.0.0001)then
        write(*,'(''::ERROR: Real Cell Angle of '',F12.3,
     .     '' too small.'')')RANG
        stop
      endif
C
      WRITE(*,'(/,'' Input Upper and Lower Resolution Limits in A'')')
      READ (*,*) ARMIN,ARMAX
      WRITE(*,'('' Read: '',2F12.3)')ARMIN,ARMAX
C
      WRITE(*,'(/,'' Input Symmetry Plane Group Name (lowercase)'')')
      READ(*,'(A6)')CSYM(1:6)
      WRITE(*,'('' Read: '',A)')CSYM(1:6)
C
      isym=0
      do i = 1,21
        iok=1
        write(CTMP,('(A)'))CSTAB(i)
        do j = 1,6
          if(CSYM(j:j).ne.CTMP(j:j))iok=0
        enddo
        if(iok.eq.1)isym=i
      enddo
      if(isym.eq.0)then
        write(*,'(''::ERROR: Symmetry '',A6,'' not recognized.'')')
     .    CSYM(1:6)
        goto 999
      else
        write(*,'('' Symmetry '',A6,'' corresponds to internal'',
     .    '' number '',I2)')CSYM(1:6),isym
      endif
C
      if(isym.eq.4 .or. isym.eq.6 .or. isym.eq.8 .or. isym.eq.11) then
C-------p12_a, p121_a, c12_a, p2221a
        write(*,'(''::##############################################'',
     .    ''################################'')')
        write(*,'(''::=============================================='',
     .    ''================================'')')
        write(*,'(''::----------------------------------------------'',
     .    ''--------------------------------'')')
        write(*,'(''::    ERROR: Symmetry '',A6,'' not supported.'')')
     .    CSYM(1:6)
        write(*,'(''::----------------------------------------------'',
     .    ''--------------------------------'')')
        write(*,'(''::   You should reindex 90-deg rotated.'')')
        write(*,'(''::----------------------------------------------'',
     .    ''--------------------------------'')')
        write(*,'(''::=============================================='',
     .    ''================================'')')
        write(*,'(''::##############################################'',
     .    ''################################'')')
        stop 
      endif
C
      if(isym.eq.17) then
C-------p3
        write(*,'('':Symmetry '',A6,'' does not require CENTRIC.'')')
     .    CSYM(1:6)
      endif
C
      if(ARMIN.lt.ARMAX)then
        rtmp=ARMIN
        ARMIN=ARMAX
        ARMAX=rtmp
      endif
C
      write(*,'('' '')')
C
C-----Open input and output files
C
      open(10,FILE=CINFILE,STATUS='OLD',ERR=900)
      open(11,FILE=COUT1,STATUS='NEW',ERR=900)
      open(12,FILE=COUT2,STATUS='NEW',ERR=900)
      open(13,FILE=COUT3,STATUS='NEW',ERR=900)
C
C-----Initiate output files' titles
C
      READ (10,100) TITLE
      WRITE (11,100) TITLE
      WRITE (12,100) TITLE
      WRITE (13,100) TITLE
 100  FORMAT (A50)
C
C
C-----Initialize variables
C
      N = 0
      DELTOT = 0.0
      VARTOT = 0.0
      FOMTOT = 0.0
C
      DO H=-100,100
        DO K=-100,100
          DELTA(H,K) = -9999.0
        ENDDO
      ENDDO
C
C-----Construct the real-space lattice, in Angstroems
C
      ax = A
      ay = 0.0
      bx = B * cos(RANG*PI/180)
      by = B * sin(RANG*PI/180)
C
      write(*,'('' Calculated real space vectors, in Angstroem: '')') 
      write(*,'(4F12.1,/)')ax,ay,bx,by
C
C-----Calculate the reciprocal lattice, in Angstroems, assuming a 1px image side ilength.
C-----ilength does not matter here, it is therefore set to 1.
C
      ilength = 1
      rmodAxB = abs(ax*by-bx*ay)
C
      rax =  by * ilength / rmodAxB
      ray = -bx * ilength / rmodAxB
      rbx = -ay * ilength / rmodAxB
      rby =  ax * ilength / rmodAxB
C
      write(*,'('' Calculated reciprocal vectors for hypothetical '',
     .  ''1px image: '')') 
      write(*,'(4F12.6,/)')rax,ray,rbx,rby
C
      RMIN = ilength / ARMIN
      RMAX = ilength / ARMAX 
C
C-----Read input file
C
1000  continue
        READ (10,*,END=1010) H,K,L,AMP,PHASE,FOM
C         
        if(PHASE.gt. 180.0)PHASE=PHASE-360.0
        if(PHASE.gt. 180.0)PHASE=PHASE-360.0
        if(PHASE.lt.-180.0)PHASE=PHASE+360.0
        if(PHASE.lt.-180.0)PHASE=PHASE+360.0
C
C-------Calculate resolution of this spot
        spotx = H * rax + k * rbx
        spoty = H * ray + k * rby
        RES = sqrt( spotx**2 + spoty**2 )
C
        IF (RES.LT.RMIN.OR.RES.GT.RMAX) GOTO 1000
C         
        N = N + 1
C
C------------------------------------------------------------------
C-------Only change phases and FOM for non-tilted plane:
C------------------------------------------------------------------
C
        if (L.eq.0) then
C
          if(isym.eq.2 .or. isym.eq.9 .or. isym.eq.10 .or. 
     .       isym.eq.12 .or. isym.eq.13 .or. isym.eq.14 .or. 
     .       isym.eq.15 .or. isym.eq.16 .or. isym.eq.20 .or.
     .       isym.eq.21)then
C-----------p2,p222,p2221b,p22121,c222,p4,p422,p4212,p6,p622
C-----------All phases to 0 or 180 (whatever is closer).
            IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
              DELTA(H,K) = ABS(PHASE)
              FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C-------------IF (FOM2.LT.FOM) FOM = FOM2
              FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
              PHASE = 0.0
            ENDIF
            IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
              DELTA(H,K) = 180.0 - ABS(PHASE)
              FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C-------------IF (FOM2.LT.FOM) FOM = FOM2
              FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
              PHASE = 180.0
            ENDIF
C
          else if(isym.eq.3 .or. isym.eq.5 .or. isym.eq.7) then
C-----------p12_b, p121_b, c12_b
C-----------The only REAL amplitudes are along k=0,
C-----------so CENTRIC should only convert phases to 0 or 180 for k=0.
            if(k.eq.0)then
              IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
                DELTA(H,K) = ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 0.0
              ENDIF
              IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
                DELTA(H,K) = 180.0 - ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 180.0
              ENDIF
            endif
C
          else if(isym.eq.18) then
C-----------p312
C-----------CENTRIC applies for h=k only
            if(h.eq.k)then
              IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
                DELTA(H,K) = ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 0.0
              ENDIF
              IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
                DELTA(H,K) = 180.0 - ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 180.0
              ENDIF
            endif
C
          else if(isym.eq.19) then
C-----------p321
C-----------CENTRIC applies only to (h,0) and (0,k)
            if(h.eq.0 .or. k.eq.0)then
              IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
                DELTA(H,K) = ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 0.0
              ENDIF
              IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
                DELTA(H,K) = 180.0 - ABS(PHASE)
                FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
C---------------IF (FOM2.LT.FOM) FOM = FOM2
                FOM = FOM * COS(DELTA(H,K)*3.1415927/180)
                PHASE = 180.0
              ENDIF
            endif
C
          endif
C
C------------------------------------------------------------------
C---------Now take care of systematic absences:
C------------------------------------------------------------------
C
          if(isym.eq.10)then
C-----------p2221b
            if(mod(k,2).eq.1 .and. h.eq.0)then
              AMP=0.0
              FOM=100.0
            endif
C
          else if(isym.eq.3 .or. isym.eq.5)then
C-----------p12_b, p121b
            if(mod(k,2).eq.1 .and. k.eq.0)then
              AMP=0.0
              FOM=100.0
            endif
C
          else if(isym.eq.7)then
C-----------c12b
            if(mod(k,2).eq.1 .and. k.eq.0)then
              AMP=0.0
              FOM=100.0
            endif
            if(mod(h+k,2).eq.1)then
              AMP=0.0
              FOM=100.0
            endif
C
          else if(isym.eq.11)then
C-----------p2212a
            if(mod(h,2).eq.1 .and. k.eq.0)then
              AMP=0.0
              FOM=100.0
            endif
C
          else if(isym.eq.12 .or. isym.eq.16)then
C-----------p22121,p4212
            if((mod(h,2).eq.1 .and. k.eq.0) .or.
     .         (mod(k,2).eq.1 .and. h.eq.0))then
              AMP=0.0
              FOM=100.0
            endif
C
          else if(isym.eq.8 .or. isym.eq.13)then
C-----------c21,c222
            if(mod(h+k,2).eq.1)then
              AMP=0.0
              FOM=100.0
            endif
C
          endif
C
        endif
C
        DELTOT = DELTOT + DELTA(H,K)
        FOMTOT = FOMTOT + FOM
C
        WRITE (11,205) H,K,L,AMP,PHASE,FOM
 205    FORMAT (3I6,2G16.6,G16.6)
        WRITE (12,210) H,K,AMP,PHASE,FOM
 210    FORMAT (2I6,2G16.6,G16.6)
        WRITE (13,215) H,K,L,AMP,FOM
 215    FORMAT (3I6,G16.6,' 0.0 ',G16.6)
C
      GOTO 1000
C
1010  CONTINUE
C
      IF(N.EQ.0) THEN
        WRITE(*,290) N,ARMIN,ARMAX
 290    FORMAT (': ',I5,' REFL. OVER ',F5.1,' - ',
     .    F5.1,' ANG. NO STATISTICS CALCULATED.')
      ELSE
C
        DELAV = DELTOT / N
        FOMAV = FOMTOT / N / 100
C
        DO 1020 H=-100,100
          DO 1020 K=-100,100
            IF (DELTA(H,K).NE.-9999.0) THEN
              VAR = (DELTA(H,K) - DELAV)**2
              VARTOT = VARTOT + VAR
            ENDIF
1020    CONTINUE
C
        VARTOT = SQRT(VARTOT / N)
        RN = N
        STANERR = VARTOT / SQRT(RN)
C
        WRITE (*,300) N,ARMIN,ARMAX,DELAV,STANERR,FOMAV
300     FORMAT (': ',I5,' Refl, Res= ',F5.1,' - ',
     .    F5.1,' A, Resid= ',F7.3,' (45=random), Err= ',F7.3,
     .    ' FOM= ',F7.3)
      ENDIF
C
      goto 999
C
 900  continue
      write(*,'(''::ERROR in 2dx_centric2 on file open'')')
      goto 999
C
 999  continue
C
      close(11)
      close(12)
      close(13)
C
      STOP
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

