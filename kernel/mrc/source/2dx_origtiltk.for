C   ORIGTILTK - THREE-DIMENSIONAL ORIGIN, BEAMTILT AND CRYSTAL TILT REFINEMENT.
C
C         --- remember to change version number in format statement  ---
C         VERSION VX5.02  RH  29.10.01 change FILIN to CHARACTER*80
C         VERSION VX5.01  RH  18.12.00 test to avoid IZLESS.LE.0 out-of-bounds
C         VERSION VX5.00  RH  29.8.00 convert to plot2000 direct postscript plot
C         VERSION VX4.12  RH  24.10.99 debug IF(IH.GE.900) GO TO 210
C         VERSION VX4.11  RH  17.9.99 increase MAXPLT=1500
C         VERSION VX4.10  RH   3.8.99 increase MAXPLT=500, OVERALL format I6
C         VERSION VX4.09  RH  28.5.99 INTEGER*4 JOUT, MAXINDEX=40, MAXRFL=2000
C         VERSION VX4.08  RH  10.2.99 TOTRFL -> 80000
C         VERSION VX4.07  RH  2.1.98 cosmetic correction to printout for ILIST=1
C         VERSION VX4.06  RH  30.5.96 calculate S for mtz input data
C         VERSION VX4.05  RH  4.12.95 cosmetic description of TAXA
C         VERSION VX4.04  RH  17.4.95 option NPROG=3 for MTZ data, ORIGTILTD
C         VERSION VX4.03  RH  7.4.95 removes final fortran STOP to allow csh -e
C         VERSION VX4.02  RH  21.3.95 summary file output with OX,OY,TAXA,TANGL
C     VAX VERSION VX4.01  RH  5.10.93 bug in plotting IREF2 fixed
C     VAX VERSION VX4.00  RH  6.10.92 carries through background and ctf to O/P.
C     VAX VERSION VX3.02  RH  4.3.91 Prints out total no. of refls for sorting.
C     VAX VERSION VX3.01  RH  22.1.90 Minor change to ASYM. 
C     VAX VERSION VX3.00  RH  19.6.89 READS AND WRITES EXTRA FLMWGT COLUMN.
C     VAX VERSION VX2.04  RH  18.6.89 annotated output lines for summary editing
C     VAX VERSION VX2.03  JMB 15.6.89 ALLOWS UP TO 40000 SPOTS
C     VAX VERSION VX2.02  JMB 15.6.89 Read filenames instead of unit numbers
C     VAX VERSION VX2.01  RH  4.4.89 option to write out shifted image data.
C     VAX VERSION VX2.00  RH  3.4.89 option to write out reference amps & phases
C     VAX VERSION VX1.19  RH  30.3.89 limits max tilt refine cycles to 4.
C     VAX VERSION VX1.18  JMB 10.3.89 ALLOWS UP TO 20000 SPOTS
C     VAX VERSION VX1.17  RH  2.3.89 IMPROVED BEAMTILT REFINEMENT, USING VA04A
C     VAX VERSION VX1.16  JMB 24.10.88 ALLOWS UP TO 16000 SPOTS
C     VAX VERSION VX1.15  RH  21.10.88 checks LCF input curves for SIGAMP.
C     VAX VERSION VX1.14  RH  8.8.88 CAN NOW USE ISPG LCFDATA WITH ZSTAR +VE.
C     VAX VERSION VX1.13  RH  30.5.88  weight for IQ=8 made slightly non-zero.
C     VAX VERSION VX1.12  JMB 23.2.88  maxindex =25
C     VAX VERSION VX1.11  JMB 25.8.87 ALLOWS MORE SPOTS PER IMAGE.
C     VAX VERSION VX1.10  RH  1.5.87 ALLOWS POINTS SLIGHTLY OFF ENDS OF CURVES.
C     VAX VERSION VX1.09  RH  20.4.87 ALLOWS ZERO AMPL INPUT, IQ UP TO 9.
C     VAX VERSION VX1.08  RH  24.1.87 NTILT(T/F), LCF ORIGREF W/O XTL TILTREF.
C     VAX VERSION VX1.07  JMB 14.5.86 COMPATIBILITY TO RECENT LCF CHANGES.
C     VAX VERSION VX1.06  RH  7.10.85  GRAPH PLOT TO +/- 0.25 ANG**-1.
C     VAX VERSION VX1.05  RH  1.7.85 NON-LEAST SQ BEAM TILT ALGORITHM.
C     VAX VERSION VX1.04  JMB 27.2.85 STATISTICS VS RESOLUTION.
C     VAX VERSION VX1.03  RH  30.12.84 WITH BEAM TILT REFINEMENT.
C     VAX VERSION VX1.02  RH  26.8.84 WITH CRYSTAL TILT REFINEMENT.
C     VAX VERSION VX1.01  RH
C     VAX VERSION VX1.00  RH
C
C     NOW DOES REFINEMENT OF BEAM AND CRYSTAL TILTANGLES AND TILTAXES.
C                            derived from ORIGMERG  13.4.84, (RH)
C     NOTE: TILT REFINEMENT (CRYSTAL AND BEAM) HAS BEEN TESTED ONLY IN
C                                       P3              30.12.84
C                                       P22121            8.8.88
C                                       P4212             4.4.94
C
C   THREE-DIMENSIONAL IMAGE COMBINING PROGRAM FOR ALL SEVENTEEN
C                   TWO-SIDED PLANE GROUPS
C
C      ORIGINAL PROGRAM WRITTEN BY S.D.FULLER, 10-MAY-1980.
C      MODIFIED L.A.AMOS & R.HENDERSON, JULY 1980 (AT LEAST 30 BUGS REMOVED).
C      MODIFIED 23-7-81 BY DANA LEIFER -- CYCLICAL REFINEMENT OPTION.
C      AMPLITUDE SCALING DEBUG, 1982(RAC)
C      TILTAXIS AND TILTANGLE REFINEMENT 26.8.84.
C      BEAMTILT REFINEMENT 30.12.84
C         LAST MODIFIED 30-DEC-1984 BY R.H.
C      PAY NO ATTENTION TO THE ABOVE HISTORICAL NONSENSE - IT IS ONLY
C      MEANT TO JOG THE MEMORY IN CASE FURTHER FESTERING BUGS EMERGE.
C
C      THIS VERSION --
C                     PRODUCES PLOTTER O/P ON FILE PLOT.PLT(SUBROUTINE_GRAPH)
C                     PRODUCES A MERGED LIST OF H,K,ZSTAR,AMPL,PHASE,FILMNO,
C                        DESCRIPTIVE CODE(+/-IQ) ON UNIT 3.
C                     THE MERGED LIST CAN THEN BE READ IN AND USED FOR
C                        CYCLICAL ORIGIN REFINEMENT.
C                     REFINES CRYSTAL TILTAXIS AN TILTANGLE.
C                     REFINES BEAMTILT (MANIFESTED AS A RESOLUTION-DEPENDENT
C                        SHIFT OF THE PHASE ORIGIN).
C
C###############################################################################
C
C               CARD INPUT ON UNIT 5
C
C   0.1 RESULTS FILE NAME
C
C   1  ISPGRP,NPROG,NTILT,NBEAM,ILIST,ALNG,BLNG,WIDTH,ANG,IPLOT,MINRFL,IAQP2,IVERBOSE (*)
C
C   1.5 itaxastep,rtaxasize,itanglstep,rtanglsize
C
C   2  IRUN,LHMIN,LHMAX,IQMAX,IBOXPHS,NREFOUT,NSHFTIN,RFACAMP                (*)
C
C   3  card 3 input depends on the value of NPROG chosen
C
C  IF  NPROG.EQ.0
C       THE FIRST SET OF REFLECTIONS MUST BE FOR AN UNTILTED
C       IMAGE AND ONLY THE UNIQUE REFLECTIONS SHOULD BE PROVIDED
C       THIS OPTION IS MEANT FOR MERGING RAW DATA -- AS IN ORIGINAL
C       ORIGMERG - This set of data can have zero observations 
C       provided that SCALE = 1.0 (or any non-zero value) on card 9
C  IF  NPROG.EQ.1
C       THE FIRST SET OF DATA READ IN WILL BE FROM O/P OF A PREVIOUS
C       RUN OF ORIGMERG (UNIT 3 O/P). SUBSEQUENT FILMS WILL BE COMPARED
C       ONLY WITH THIS FIRST SET AND NO MERGED OUTPUT WILL BE POSSIBLE.
C       THIS OPTION IS MEANT FOR CYCLICAL ORIGIN REFINEMENT OF AN ALREADY 
C       MERGED SET OF DATA.
C
C                3  IFILM,TITLE                               (I10,10A4)
C
C                3A Filename -- e.g. SS1:[RH15]P4151.APH             (A)
C
C                Then the file contains data of following kind.
C                   ISER                                             (*)
C                   IH,IK,A,P                                        (*)
C                   IH=900 (or EOF) --- THIS ENDS THE SET OF REFLECTIONS
C
C  IF  NPROG.EQ.2 (.LCF) -- (NO INPUT OF ABOVE CARDS 3,4,5, or 6)
C   or NPROG.EQ.3 (.MTZ) data
C       THE REFERENCE DATA IS A THREE-DIMENSIONAL LCF or MTZ DATA FILE.
C       THIS OPTION IS MEANT FOR STRUCTURES THAT HAVE BEEN WELL
C       WORKED OVER ALREADY -- E.G. PURPLE MEMBRANE.
C          Input is on file with name HKLIN for LCF or MTZ file.
C
C  BUT           3  FC=.... SIGFC=.... PHCAL=.... FOM=....   ##(LCF CONTROL)
C   or           3  LABIN AMP=... SIG=... PHASE=... FOM=...  ##(MTZ CONTROL)
C  
C       PROCEEDS ACCORDING TO NPROG.EQ.1, BUT REFINEMENT IS DONE AGAINST
C       AMPLITUDE AND PHASE CURVE-FITTED DATA. THE PHASE ORIGIN IS REFINED
C       AS WELL AS (if NTILT=T) THE TILTANGLE AND TILTAXIS. THIS REFINEMENT
C       OF ORIGIN AND TILT IS CARRIED OUT SEPARATELY UNTIL THERE IS
C       NO FURTHER CHANGE IN THE PARAMETERS.
C
C   4  IFILM,TITLE                                            (I10,10A4)
C
C   5  Filename -- e.g. SS1:[RH15]P4151.APH                          (A)
C
C   6  NWGT                                                          (*)
C
C   7  TAXA,TANGL,IORIGT                                             (*)
C
C   7.5 LATTICE
C
C   8  ORIGH,ORIGK,STEP,WIN,SGNXCH,SCALE,ROT180,REVHK,CTFREV,ROT90,REVHND,REVXSGN (*)
C
C   9  CS,KV,TILTH,TILTK                                             (*)
C
C  10  DRESMAX, DRESMIN (BLANK CARD GIVES 100.0,3.5)                 (*)
C
C     Then the file of data contains data of the following kind
C       ISER                                                         (*)
C       IH,IK,A,P,IQ                                                 (*)
C       IH=900  (or EOF) --- THIS ENDS THE SET OF REFLECTIONS.
C
C            ANY NUMBER OF FURTHER IMAGES SPECIFIED BY THE ABOVE CARDS CAN
C            BE INCLUDED AT THIS STAGE.
C
C  11  IFILM<0  --- THIS ENDS DATA INPUT
C
C  12 TITLE --- TITLE FOR PLOT OUTPUT IF REQUESTED.               (20A4)
C       
C##############################################################################
C
C
C                ISPGRP - NUMBER OF SPACE GROUP AS BELOW
C                NPROG  - DETERMINES TYPE OF RUN
C                         IF =0, NORMAL SEQUENTIAL MERGING (OLD ORIGMERG).
C                         IF =1, READS IN PREVIOUSLY MERGED DATA O/P ON UNIT 3.
C                         IF =2, READS IN REFERENCE DATA FROM LCF FILE.
C                         IF =3, READS IN REFERENCE DATA FROM MTZ FILE.
C                NTILT  -  IF (F) NO CRYSTAL TILTANGLE OR TILTAXIS REFINEMENT.
C                          IF (T) CRYSTAL TILTANGLE AND TILTAXIS ARE REFINED.
C                NBEAM  -  IF (F) NO BEAMTILT REFINEMENT.
C                          IF (T) BEAMTILT IS REFINED -- IN MILLIRADIANS.
C                ILIST  - IF =1 PROGRAM LISTS EACH REFLECTION OTHERWISE
C                           ONLY THE PLOT OF RESIDUALS AND THE FINAL
C                           COMBINED LIST OF REFLECTIONS IS GENERATED
C                ALNG   -  A AXIS IN ANGSTROMS for untilted crystal.
C                BLNG   -  B AXIS IN ANGSTROMS        "        "   .
C                WIDTH  -  THICKNESS OF UNIT CELL IN ANGSTROMS
C                ANG    -  ANGLE BETWEEN A AND B - ONLY FOR P1 OR P2
C                IPLOT  -  IF NOT 0, PLOT FINAL AMPLITUDE & PHASE CURVES
C                            ON CALCOMP PLOTTER
C                MINRFL -  MINIMUM NUMBER OF POINTS REQUIRED FOR A CURVE
C                            TO BE PLOTTED
C                IAQP2  - (1) for AQP2 special symmetry condition (now obsolete)
C                         (0) otherwise
C                IVERBOSE-(0) for no logfile output
C                         (1) for only essential logfile output
C                         (>5) for full logfile output
C                LOGOUTPUT - T for output of Logfile data
C                            F otherwise
C                LPROTFOUFIL - T if the FouFiltered phase origin should be protected
C                              F otherwise
C                LUSEML - T if the output of ML processing should be used
C                         F otherwise
C                IRUN   - RUN NUMBER, USED AS AN IDENTIFIER ON UNIT 3 O/P
C                LHMIN  - MINIMUM H INDEX TO BE PLOTTED
C                LHMAX  - MAXIMUM H INDEX TO BE PLOTTED
C                              (PROGRAM STOPS AFTER LAST PLOT)
C                IQMAX  - REFLECTION NOT USED FOR ORIGIN OR TILTANGLE
C                           REFINEMENT IF IQ>IQMAX
C                IBOXPHS- SIZE OF PHASE ORIGIN SEARCH, (DEFAULT = 121)
C                NREFOUT-  IF (F) NO OUTPUT OF REFERENCE PROJECTION DATA.
C                          IF (T) FILE IS CREATED WITH REFERENCE PROJECTION
C                                 DATA AT SAME ANGLE AS INPUT DATA.
C                NSHFTIN-  IF (F) NO OUTPUT OF ORIGIN SHIFTED INPUT DATA
C                          IF (T) FILE IS CREATED FROM SHIFTED INPUT DATA
C                RFACAMP- Factor for Amplitude weight during merging
C                         RFACAMP=0.0: no weighting, RFACAMP=1.0: full weight
C                IFILM  - INTEGER FILM IDENTIFIER
C                TITLE  - DESCRIPTION OF FILM
C                NIN    - UNIT NUMBER OF INPUT DATA STREAM * NOT USED NOW
C                FILIN  - NAME OF FILE CONTAINING H,K,A,P,IQ DATA
C                NWGT   - IF (T) THEN READ EXTRA FLMWGT DATA FOR EACH REFLECTION
C                         IF (F) THEN NO EXTRA WEIGHT DATA.
C                ISER   - SERIAL NUMBER OF FILM ON UNIT NIN, MUST=IFILM.
C                IH     - H INDEX OF REFLECTION
C                IK     - K INDEX OF REFLECTION
C                P      - PHASE OF REFLECION
C                A      - AMPLITUDE OF REFLECTION
C                IQ     - QUALITY OF REFLECTION
C                TAXA   - ANGLE MEASURED FROM THE TILT AXIS TO THE ASTAR-AXIS,
C                         MEASURED IN DIRECTION OF A TO B BEING POSITIVE.
C                TANGL  - TILT ANGLE IN DEGREES
C                IORIGT - IF IORIGT=1, ORIGIN REFINEMENT IS DONE WITH
C                             WGT = 1.0 FOR EACH NEW SPOT.
C                ORIGH  - INITIAL PHASE SHIFT FOR 1,0 -- IN DEGREES
C                ORIGK  - INITIAL PHASE SHIFT FRO 0,1 --  "    "
C                STEP   - STEP SIZE IN DEGREES FOR ORIGIN REFINEMENT
C                         STEP = 0 ==> NO REFINEMENT
C
C                WIN    - ZSTAR RANGE WITHIN WHICH SPOTS ARE COMPARED
C                         FOR SCALING AND ORIGIN REFINEMENT
C                         A WIN value of 1/(2*ALAT) is useful, 
C                         e.g. 0.005 for ALAT=100
C
C                SCALE  - MULTIPLIED BY AMPLITUDES BEFORE COMBINATION
C                         IF EQUAL TO 0 SCALING IS AUTOMATIC
C                SGNXCH - IF NOT EQUAL TO 0, FLIP AROUND A AXIS, USEFUL IN P121
C                ROT180 - IF NOT=0, ROTATE 180 DEG ABOUT Z-AXIS, USEFUL IN P1,P3
C                ROT90  - IF NOT=0, ROTATE 90  DEG ABOUT Z-AXIS, USEFUL IN P2221 
C                         THIS IS A COSMETIC FEATURE TO FACILITATE INDEXING
C                         DIFFICULT HIGHLY TILTED FILMS. NOTE THAT ALL OTHER
C                         PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                         W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C                REVHK  - IF NOT = 0, H AND K ARE INTERCHANGED ON INPUT.
C                         THIS IS A COSMETIC FEATURE TO FACILITATE INDEXING
C                         DIFFICULT HIGHLY TILTED FILMS. NOTE THAT ALL OTHER
C                         PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                         W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C                REVHND - IF NOT = 0, sign of Z is inverted.
C                         THIS IS A COSMETIC FEATURE TO FACILITATE debugging the 
C                         damned handedness. 
C                         NOTE THAT ALL OTHER
C                         PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                         W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C                REVXSGN - IF NOT = 0, sign of X is inverted.
C                         THIS IS A COSMETIC FEATURE TO FACILITATE debugging the 
C                         damned handedness. 
C                         NOTE THAT ALL OTHER
C                         PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                         W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C                CTFREV - IF NOT = 0, 
C                         REVERSES SIGN OF STRUCTURE FACTOR BY ADDING 180 DEGS
C                         TO THE PHASE. USEFUL FOR LOW DOSE IMAGES WHERE THERE
C                         IS UNCERTAINTY ABOUT WHETHER IMAGE IS OVER-FOCUSSED
C                         OR UNDER-FOCUSSED.
C                CS     - SPHERICAL ABERRATION COEFFICIENT - USED TO GET
C                         BEAMTILT ON RIGHT ABSOLUTE SCALE OF MILIRADIANS.
C                KV     - MICROSCOPE VOLTAGE - USED TO CALCULATE WAVELENGTH.
C                TILTH  - BEAMTILT IN DIRECTION OF ASTAR.
C                TILTK  - BEAMTILT IN DIRECTION OF BSTAR.
C
C###############################################################################
C
C    OUTPUT IS MADE VIA
C         UNIT   6 - LINEPRINTER
C         UNIT   3 - MERGED DATA POINTS(H,K,ZSTAR,AMPL,PHASE,FILMNO,IQ CODE --
C                     CAN BE USED BY ORIGREFN FOR FURTHER CYLES OF REFINEMENT.
C         PLOT.PLT - PLOT FILE FOR PLOT OUTPUT.
C         UNIT   2 - If requested, origin shifted H,K,0,AMP,PHASE of projection.
C         UNIT   4 - If requested, H,K,0,REFAMP,REFPHASE for refdata projection.
C         UNIT   9 - summary file with refined values of tilt,origin & beamtilt.
C
C    INPUT IS MADE VIA
C         UNIT   5 - CONTROL DATA
C         UNIT  10 - NEW IMAGES TO BE MERGED, OR PREVIOUSLY MERGED DATASET
C         HKLIN    - LCF FILE OF AMPLITUDES AND PHASES. (for NPROG.EQ.2)
C         HKLIN    - MTZ FILE OF AMPLITUDES AND PHASES. (for NPROG.EQ.3)
C
C##############################################################################
C  18.8.84 ############  IMPORTANT CHANGE #####################################
C            THE MATRICES IMAT, MAT, IGO HAVE BEEN CHANGED, TOGETHER WITH THE 
C            LREV TEST IN ASYM SO THAT THE CONVENTION IN P4, P3, AND P6 IS
C            FOR THE AXIAL INDICES TO BE H,0 RATHER THAN 0,K.
C            ( THEY ARE THE SAME AS IN EDLCF ).
C
C  PROGRAM MUST NOW BE LINKED USING COMMAND
C   :-   on VAX, PIMLINK ORIGTILT
C                (INCLUDES LCFLIB,IMLIB,MODLIB,PLOT82, (LIBRARIES) ETC)
C   :-   for UNIX fortran -o origtilt.exe origtilt.for ma21.for -lplot82 -lccp4
C
C##############################################################################
C
C
C SPACE GROUP MATRICES --- convention for p3, p4 and p6 is H,0 (not 0,K).
      INTEGER*4 ISPEC(5,17)
C     DATA ISPEC/7*0,1,3*0,1,4*0,1,4*0,1,3*0,3*1,2*0,3*1,0,-1,3*1,0,1,
C    A 3*1,4*0,1,0,0,4*1,0,5*1,8*0,1,0,1,1,5*0,1,4*0,1,1,0/
      DATA ISPEC/
     A  0,0,0,0,0,
     B  0,0,1,0,0,
     C  0,1,0,0,0,
     D  0,1,0,0,0,
     E  0,1,0,0,0,
     F  1,1,1,0,0,
     G  1,1,1,0,-1,
     H  1,1,1,0,1,
     I  1,1,1,0,0,
     J  0,0,1,0,0,
     K  1,1,1,1,0,
     L  1,1,1,1,1,
     M  0,0,0,0,0,
     N  0,0,0,1,0,
     O  1,1,0,0,0,
     P  0,0,1,0,0,
     Q  0,0,1,1,0/
      INTEGER*4 IGO(8,17)
C     DATA IGO/8*5,2*4,2*5,2*4,2*5,
C    A 4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,
C    B 2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,
C    C 2,4,2,5,2,4,2,5,  3,4,3,5,3,4,3,5,  1,2,1,4,1,2,1,5,
C    D 1,2,1,4,1,2,1,5,  4,5,4,5,3,5,3,5,  2,4,2,4,1,5,1,5,
C    E 2,4,2,4,1,5,1,5,  3,4,3,5,1,4,1,5,  2,3,2,4,1,3,1,5/
      DATA IGO/
     A 5,5,5,5,5,5,5,5,
     B 4,4,5,5,4,4,5,5,
     C 4,5,4,5,4,5,4,5,
     D 4,5,4,5,4,5,4,5,
     E 4,5,4,5,4,5,4,5,
     F 2,4,2,5,2,4,2,5,
     G 2,4,2,5,2,4,2,5,
     H 2,4,2,5,2,4,2,5,
     I 2,4,2,5,2,4,2,5,
     J 3,4,3,5,3,4,3,5,
     K 1,2,1,4,1,2,1,5,
     L 1,2,1,4,1,2,1,5,
     M 4,5,4,5,3,5,3,5,
     N 2,4,2,4,1,5,1,5,
     O 2,4,2,4,1,5,1,5,
     P 3,4,3,5,1,4,1,5,
     Q 2,3,2,4,1,3,1,5/
      INTEGER*4 IMAT(5,17)
      DATA IMAT/ 1,1,1,1,1,    1,2,1,1,1,    1,3,1,1,1,
     A           1,4,1,1,1,    1,3,1,1,1,    1,2,1,3,1,
     B           1,2,1,4,1,    1,2,1,6,1,    1,2,1,3,1,
     C           1,2,7,5,1,    1,8,1,2,3,    1,8,1,9,6,
     D           1,10,11,12,1, 1,8,1,10,11,  1,9,1,10,11,
     E           1,2,10,5,11,  1,8,9,10,11/
      INTEGER*4 MAT(8,12)
      DATA MAT/   -1,0,0,-1,-1,0,0,-1,      1,0,0,1,-1,0,0,-1,
     A            1,0,0,-1,1,0,0,-1,        1,0,0,-1,1,0,180,-1,
     B            0,1,-1,0,1,0,0,1,        1,0,0,-1,1,180,180,-1,
     C            0,-1,1,0,1,0,0,1,         0,1,1,0,1,0,0,-1,
     D            0,1,1,0,-1,0,0,1,         0,-1,1,1,-1,0,0,-1,
     E            -1,-1,1,0,1,0,0,1,         1,1,-1,0,-1,0,0,-1/
      LOGICAL LREV(17)
      DATA LREV/9*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,
     1          .FALSE./
      REAL STANG(17)            ! STANDARD SPACE GROUP ANGLES.
      DATA STANG/2*0.0,10*90.0,5*120.0/
C
C        ANGLE BETWEEN A AND B FOR ALL SPACEGROUPS EXCEPT P1, P2 IS FIXED.
C
C        THE ABOVE MATRICES ARE USED BY ASYM TO TRANSFORM ALL REFLECTIONS
C           TO THE STANDARD ASYMMETRIC UNIT AND TO PICK OUT THE SPECIAL
C           REFLECTIONS.
C
C
C     NUMBER   SPACEGROUP    ASYMMETRIC UNIT        REAL   IMAGINARY
C
C          1          P1         H>=0
C
C          2         P21         H,Z>=0              Z=0
C
C          3         P12         H,K>=0              K=0
C
C          4        P121         H,K>=0              K=0
C
C          5         C12         H,K>=0              K=0
C
C          6        P222         H,K,Z>=0            H=0
C                                                    K=0
C                                                    Z=0
C
C          7       P2221         H,K,Z>=0          (0,2N,Z)  (0,2N+1,Z)
C                                                    (H,K,0)
C                                                    (H,0,Z)
C
C          8      P22121         H,K,Z>=0            (H,K,0)
C                                                   (2N,0,Z)  (2N+1,0,Z)
C                                                   (0,2N,Z)  (0,2N+1,Z)
C
C          9        C222         H,K,Z>=0            (H,K,0)
C                                                    (H,0,Z)
C                                                    (0,K,Z)
C
C         10          P4         H,K,Z>=0            (H,K,0)
C
C         11        P422         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,0,Z)
C                                                    (0,K,Z)
C                                                    (H,H,Z)
C
C         12       P4212         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C                                                   (2N,0,Z)   (2N+1,0,Z)
C                                                   (0,2N,Z)   (0,2N+1,Z)
C
C         13          P3         H,K>=0
C
C         14        P312         H,K>=0              (H,H,Z)
C                                K>=H
C
C         15        P321         H,K>=0              (H,0,Z)
C                                 K>H                (0,K,Z)
C
C         16          P6       H,K,Z>=0             (H,K,0)
C
C         17        P622         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C
C******************************************************************************
C  
      PARAMETER (NMAXC=600000)
      INTEGER TOTRFL
      PARAMETER (TOTRFL=30000000)
      PARAMETER (MAXRFL=20000)
      PARAMETER (MAXPLT=1500)
      PARAMETER (MAXINDEX=60)
      PARAMETER (NSLOTS=256)
      PARAMETER (IBXPHSMAX=3601)
C
C  DIMENSION STATEMENTS FOR NPROG.EQ.2 REFINEMENT - i.e. using LCF files.
C      DIMENSION CELL(6)                        ! also used in MTZ section
      INTEGER*4 IDATAIN(40)
C     .         ,LOOKUP(40)                     ! also used in MTZ section
      INTEGER*4 IHC(NMAXC),IKC(NMAXC),ILC(NMAXC),
     . ISC(NMAXC),IFCC(NMAXC),IPHC(NMAXC),IFOM(NMAXC)
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     . IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
CAND (40+40+1)^2=6561 bei MAXINDEX 40
C      DATA IBEGIN/6561*-999/,IFINISH/6561*-999/
CHEN (60+60+1)^2=14641 bei MAXINDEX 60
      DATA IBEGIN/14641*-999/,IFINISH/14641*-999/
C
C----------------------------------------------------------------from here
C  DIMENSION STATEMENTS FOR NPROG.EQ.3 MTZ DATA INPUT
C     .. parameters for mtz aspects
C
      INTEGER*4 N
      INTEGER*8 ISER
      PARAMETER (NLOC=60)
      PARAMETER (MCOLS=300)
      PARAMETER (NPAR=300)
C
      DIMENSION CELL(6),RSYMX(4,4,96)
      LOGICAL EOF
C
C     .. Local Arrays ..
      REAL ADATAIN(MCOLS),ADATAOUT(MCOLS),DUM(2,MCOLS)
      INTEGER JPOINT(NLOC),LOOKUP(NLOC)
      CHARACTER OUTTYP(NLOC)*1,LSPRGI(NLOC)*30,LSPRGO(NLOC)*30,
     +  TITNEW*70,HISNEW(20)*80,CTPRGI(NLOC)*1,DUMMY*10
C
C     .. Scalars for Parser ..
      INTEGER NTOK
      LOGICAL LEND
      CHARACTER KEY*4,LINE*400
C     ..
C     .. Arrays for Parser ..
      REAL FVALUE(NPAR)
      INTEGER IBEG(NPAR),IDEC(NPAR),IEND(NPAR),ITYP(NPAR)
      CHARACTER CVALUE(NPAR)*4
C
C
C---- NLPRGI  =  number of input labels
      DATA NLPRGI,LSPRGI/7,'H','K','L','AMP','SIG','PHASE',
     +     'FOM',53*' '/
C---- .. This code signs which input columns are essential (LOOKUP)
      DATA CTPRGI/'H','H','H','F','Q','P','W',53*' '/
      DATA LOOKUP/-1,-1,-1,-1,-1,-1,-1,53*0/
      DATA JPOINT/60*0/
C----------------------------------------------------------------to here
C
C  DIMENSION STATEMENTS FOR MAIN ORIGMERG PROGRAM.
      INTEGER*8 JREFL,JIN
      CHARACTER*200 FILIN
      CHARACTER*200 CZEIL
      CHARACTER DAT*24
CHEN      INTEGER*2 JH(TOTRFL),JK(TOTRFL),IRP(121,121)
CHEN      INTEGER*2 JH(TOTRFL),JK(TOTRFL),IRP(IBXPHSMAX,IBXPHSMAX)
CHEN      INTEGER*2 JOUT(TOTRFL)
      INTEGER*4 JH(TOTRFL),JK(TOTRFL),IRP(IBXPHSMAX,IBXPHSMAX)
      INTEGER*4 JOUT(TOTRFL)
      INTEGER*4 JSIGN(TOTRFL)
      INTEGER*8 JFILM(TOTRFL),IFILM,IRUN
      REAL PHS(TOTRFL),AMP(TOTRFL),ZSTAR(TOTRFL),FLMWGT(TOTRFL)
      REAL CPHS(TOTRFL),SPHS(TOTRFL),BACK(TOTRFL),CTFS(TOTRFL)
C
C  DIMENSION STATEMENTS FOR INPUT AND ORIGIN/TILTAXIS REFINEMENT.
      DIMENSION RESPOT(MAXRFL)
      INTEGER IHIN(MAXRFL),IKIN(MAXRFL)
C      INTEGER IIH(MAXRFL),IIK(MAXRFL),IQIN(MAXRFL)     ! see common block
      INTEGER IPTEST(MAXRFL)
      INTEGER*4 IP1(MAXRFL),IP2(MAXRFL)
      LOGICAL LSPEC(MAXRFL)     ! TRUE IF PHASE IS RESTRICTED BY SYMMETRY.
C      REAL PHSI(MAXRFL),PHSC(MAXRFL),WGT(MAXRFL)       ! see common block
      REAL PTEMP(MAXRFL)
      REAL PHASIN(MAXRFL),AIN(MAXRFL),WGTIN(MAXRFL)
      REAL BIN(MAXRFL),CTFIN(MAXRFL)
C      REAL BSH(MAXRFL) ! see common block
C
C       DIMENSION STATEMENTS FOR ORIGIN PHASE RESIDUAL HISTOGRAM
        DIMENSION NRESO(NSLOTS),SERRES(NSLOTS),ERRES(NSLOTS)
      DATA IRESTEP/50/
C
C  DIMENSION STATEMENTS FOR BEAMTILT REFINEMENT -- includes COMMON block
C            needed for VA04A minimisation subroutine.
      REAL*4 CS,KV,TILTH,TILTK
      LOGICAL NBEAM,NTILT,NREFOUT,NSHFTIN,NWGT,LOGOUTPUT,LPROTFOUFIL
      LOGICAL USEPYTHON,PYTHONFIRST,LUSEML
      COMMON WORK(28),IN1,IIH(MAXRFL),IIK(MAXRFL),IQIN(MAXRFL),
     . PHSI(MAXRFL),PHSC(MAXRFL),WGT(MAXRFL),SHMIN,SKMIN,TILTH,TILTK,
     . BEAMSHFT(4),BSH(MAXRFL),IQMAX,NCALCFX,
     .   FUNCMIN,RESTOT,NTOT
      EQUIVALENCE (BEAMSHFT(2),ASTAR),(BEAMSHFT(3),BSTAR),
     .   (BEAMSHFT(4),ABANG)
C
C  DIMENSION STATEMENTS FOR NREFOUT=.T. REFERENCE PROJECTION AMP & PHASE OUTPUT.
C                           NSHFTIN=.T. SHIFTED INPUT DATA.
      CHARACTER*40 FNAME
      CHARACTER*1 CTMP
C
      CHARACTER*200 cfile1,cline1,cline2,cline3,cline,CPHAORI
      CHARACTER*200 cfileconsole,cfilereflections
C
C  DIMENSION STATEMENTS FOR OUTPUT SORTING.
      REAL PZ(MAXPLT),PAMP(MAXPLT),PPHS(MAXPLT)
      INTEGER IPSGN(MAXPLT)
C        
      LOGICAL LIST              ! TRUE IF DETAILED PRINTOUT REQUIRED.
      LOGICAL IOK,IOK2
      REAL TITLE(10)
      DATA LIST/.FALSE./        ! IF TRUE DATA IS LISTED AT INPUT AND REFINE.
      DATA JREFL/0/             ! JREFL IS COUNT ON TOTAL NO. OF REFLECTIONS.
      DATA DRAD,RDEG/0.0174532,57.295779/
      DATA IZERO/0/
C
C  DIMENSION STATEMENTS FOR OUTPUT SORTING WITH QUICKSORT
C
        INTEGER M,NSTACK
        PARAMETER (M=7,NSTACK=70)
        INTEGER*4 i,ir,j,jstack,k,l,istack(NSTACK)
        REAL a,temp
                LOGICAL icompare 
C
C
      USEPYTHON = .TRUE.
      PYTHONFIRST = .TRUE.
C
      MAX1=MAXRFL+1
      ZMIN=0.0
      ZMAX=0.0
      AMAX=0.0
      SMIN=40000.
      SMAX=0.
C
      ifirst=0
C
C
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
110   FORMAT(/' THREE-DIMENSIONAL ORIGIN,',
     . ' BEAMTILT AND CRYSTAL TILT',
     . ' REFINEMENT PROGRAM'/' VX6.0 (Oct. 30, 2005)'///)
115   FORMAT('  TWO SIDED PLANE GROUP ',I3,//)
116   FORMAT(' RUN NUMBER USED AS SERIAL NUMBER FOR UNIT 3 O/P =',I5)
117   FORMAT('  CELLAXES: A ',F7.2,'  B ',F7.2,'  THICKNESS ',F7.2,
     1  '  AB ANGLE ',F7.2,' DEGREES'/)
120   FORMAT(I10,10A4)
125   FORMAT('***************************************************',
     .  '*****************************'//)
130   FORMAT(//'    UNTILTED FIRST IMAGE   '///)
132   FORMAT('    THESE REFLECTIONS SHOULD BE BROUGHT TO THE PROPER ORIG
     1IN AND'/'  AVERAGED TO YIELD ONE ASYMMETRIC UNIT   '//)
135   FORMAT(' IMAGE ',I10,5X,10A4)
136   FORMAT(' FOUND ',10A4)
137   FORMAT(4X,'H',4X,'K',6X,'ZSTAR',9X,'PHASE',12X,'AMP         IQ',
     . '    REFPHASE      NREF')
138   FORMAT(10A4)
139   FORMAT(6X,'H',5X,'K',4X,'ZSTAR',7X,'AMP',12X,'PHASE',11X,'JFILM',
     . '  IQ  FLMWGT      BACKGRND        CTF')
145   FORMAT(2I5,G15.5,2G15.5,I6,G15.5,I6)
146   FORMAT(I10)
147   FORMAT(/,':',5X,I5,' REFLECTIONS INPUT ')
148   FORMAT(///'0MORE THAN',I5,' REFLECTIONS FOR THIS IMAGE')
149   FORMAT(///'0TOTAL NUMBER OF REFLECTIONS IS MORE THAN',I10)
151   FORMAT(' REQUIRED FILM IDENTIFIER DOES NOT MATCH FILM SERIAL 
     .     NUMBER 1 AT'/'  HEAD OF DATALIST,  IFILM=',I10,
     .      '    ISER=',I10)
152   FORMAT(' ISPGRP =',I5/'  NPROG =',I5/'  NTILT =   ',L1,
     1      /,'  NBEAM =   ',L1,
     1      /,'  ILIST =',I5/'  IPLOT =',I5/'  MINRFL=',I5,
     2      /,'  LHMIN =',I5/'  LHMAX =',I5/'  IQMAX =',I5,
     3      /,' IBOXPHS=',I5/' NREFOUT=   ',L1,/' NSHFTIN=   ',L1,
     4      /,'  IAQP2 =',I5/'IVERBOSE=',I5,/,'LOGOUTPUT=',L1,
     .      /,' RFACAMP=',F10.6)
155   FORMAT('  A-STAR WAS',F8.3,' DEGS FROM TILTAXIS;  B-STAR WAS',
     1F8.3,' DEGS FROM TILTAXIS '/'  THE TILT ANGLE WAS ',F8.3,
     1' DEGREES ')
156   FORMAT(' TOTAL REFLECTIONS TO BE SORTED =',I7)
162   FORMAT(': INITIAL PHASE SHIFTS TO (1,0) AND (0,1) WERE ',2F10.2,
     1' DEGS',/,' STEP SIZE FOR ORIGIN',
     . ' REFINEMENT, IF ANY, WAS',F10.3,' DEGREES',/,
     2' SCALING AND ORIGIN REFINEMENT',
     3' USED REFLECTIONS CLOSER THAN ',F8.5,' IN ZSTAR'//)
163   FORMAT(' SCALE =',F10.5,/,' SGNXCH=',F10.5,/,' ROT180=',
     1F10.5,/,' REVHK =',F10.5,/,' CTFREV=',F10.5,/,' ROT90 =',F10.5,/,
     2' REVHND =',F10.5,/,' REVXSNG = ',F10.5,
     2      /,' LPROTFOUFIL=',L1,
     3      /,' LUSEML =',L1)
164   FORMAT('  AFTER APPLYING PHASE SHIFT, DATA WILL BE FLIPPED 
     .   ABOUT THE 1HE A AXIS  ')
165   FORMAT(/'  BEAMTILT INPUT PARAMETERS',/,
     . '              CS ==========',F9.3,/,
     . '              KV ==========',F6.0,/,
     . '              TILTH =======',F10.4,/,
     . '              TILTK =======',F10.4,/)
166   FORMAT(/,3X,I10,' REFLECTIONS READ INTO CORE. ')
168   FORMAT(3X,'ORIGIN REFINEMENT DONE BETWEEN ',I5,' OF THE NEW REFLEC
     1TIONS'/'  AND ,',I5,' OF THE REFLECTIONS FROM PREVIOUS FILMS. ',/,
     2 3X,I5,' REFLECTIONS HAVE PHASES CONSTRAINED BY SYMMETRY.')
169   FORMAT(': THIS IS A TOTAL OF ',I5,' COMPARISONS.')
170   FORMAT(/' SCALE FAC ',F10.5,' BETWEEN',I6,' NEW REFLECTIONS',
     . ' AND',I9,' FROM PREVIOUS FILMS')
171   FORMAT(': BEST PHASE RESIDUAL WAS ',F10.3,' DEGREES AT POSITION',
     1 I3,',',I3,' BELOW.')
173   FORMAT(': PHASE SHIFT IS ',F7.2,',',F7.2,' DEGS; NEW ORIGIN IS',
     . F9.2,',',F9.2,' DEGS')
175   FORMAT((1X,361I1))
180   FORMAT(' OUTPUT OF SCALED AND SHIFTED REFLECTIONS. ')
181   FORMAT(' NPROG.NE.0, NO MERGING, TO MERGE RERUN WITH NPROG=0')
182   FORMAT(' SORTING FAILED!!!!!!!!!!!!!!!!!!!! NCHNG= ',I5)
185   FORMAT(' REFLECTION ZSTAR   AMPLITUDE PHASE  FILM',
     1  ' IQ  FLMWGT  BACKGRND  CTF',/,'   IH  IK'/)
186   FORMAT(///,
     .       6X,'H',5X,'K',4X,'ZSTAR',7X,'AMP',12X,'PHASE',11X,'JFILM',
     . '  IQ  FLMWGT      BACKGRND        CTF')
187   FORMAT(///,
     .       6X,'H',5X,'K',4X,'ZSTAR',7X,'AMP',12X,'PHASE',11X,'JFILM',
     . '  IQ  FLMWGT      BACKGRND        CTF,   IQ,',
     . ' if(IQ<5)then again AMP,PHS     Resolution=',F12.3)
188   FORMAT('0TOO FEW POINTS TO BE WORTH PLOTTING FOR N=',I5,' L=',I5)
189   FORMAT(///,
     .       6X,'H',5X,'K',4X,'ZSTAR',7X,'AMP',12X,'PHASE',11X,'JFILM',
     . '  IQ  FLMWGT      BACKGRND        CTF     Resolution=',F12.3,
     . ' A')
CAnd ifilm 10 stellig
CMARC>
C190   FORMAT(1X,2I4,F8.4,F10.1,F7.1,I7,I3,F8.5,F10.1,F7.3)
190   FORMAT(1X,2I6,F9.4,G16.6,G16.6,I12,I3,F10.6,G16.6,F8.3)
191   FORMAT(1X,2I6,F9.4,G16.6, 16X ,I12,I3,F10.6,G16.6,F8.3)
192   FORMAT(1X,2I6,F9.4,G16.6,G16.6,I12,I3,F10.6,G16.6,F8.3,A)
193   FORMAT(' FOR "EQUAL WEIGHT PER INPUT SPOT ON NEW FILM" TYPE',/,
     1'   OF ORIGIN REFINEMENT, TOTAL COMPARISONS =',I5)
195   FORMAT(' IORIGT =',I5)
196   FORMAT(' LATTICE LINE POINTER ARRAY TOO SMALL, MAXINDEX=',I5)
197   FORMAT(' ****  REFERENCE LCF CURVE INPUT BEGINNING  ****')
C
C-----read filenames for result output channels:
C
      write(6,'(''Input name for results file'')')
      read(5,'(A)')cfile1
      write(6,'(A)')cfile1
C
C-----If this name ends with "py", then generate a Python script.
C-----Else generate a results file for 2dx_merge.exe: 
      call shorten(cfile1,k)
      if(cfile1(k:k).eq.'y')then
        write(6,'('':Generating a Python script'')')
        USEPYTHON = .TRUE.
      else
        write(6,'('':Generating a data file for 2dx_merge'')')
        USEPYTHON = .FALSE.
      endif
C
      write(6,'(''Input name for reflections file'')')
      read(5,'(A)')cfilereflections
      write(6,'(A)')cfilereflections
C
      write(6,'(''Input name for console file'')')
      read(5,'(A)')cfileconsole
      write(6,'(A)')cfileconsole
C
C
C        READ SPACE GROUP NUMBER, LIST PARAMETER, UNIT CELL AXES AND IF
C              SPACE GROUP IS P1 OR P2, THE INTER AXIS ANGLE
C
C
      READ(5,*) ISPGRP,NPROG,NTILT,NBEAM,ILIST,ALNG,BLNG,WIDTH,ANG,
     .          IPLOT,MINRFL,IAQP2,IVERBOSE,LOGOUTPUT
      write(*,'('' ISPGRP = '',I10)')ISPGRP
C
C-----Read tilt-axis refinement stepsizes and number or steps:
C
      read(5,*)itaxastep,rtaxasize,itanglstep,rtanglsize
      write(*,'('' read: itaxastep,rtaxasize,itanglstep,rtanglsize = ''
     .   ,I8,F12.3,I8,F12.3)')
     .   itaxastep,rtaxasize,itanglstep,rtanglsize
C
      READ(5,*) IRUN,LHMIN,LHMAX,IQMAX,IBOXPHS,NREFOUT,NSHFTIN,RFACAMP
      write(*,'('' IRUN   = '',I10)')IRUN
C
CHEN      IF(IBOXPHS.EQ.0.OR.IBOXPHS.GT.121) IBOXPHS=121
      IF(IBOXPHS.EQ.0) IBOXPHS=121
      IF(IBOXPHS.GT.IBXPHSMAX) IBOXPHS=IBXPHSMAX
      IF(ILIST.EQ.1) LIST=.TRUE.
C
C     IMAT SHOWS WHICH MATRICES WILL BE USED FROM MAT FOR EACH SPACE GROUP
C       THE FIRST ELEMENT OF EACH IS PASSED TO SET,ASYM FOR LATER USE.
C       THE SAME IS DONE FOR IGO WHICH CONTROLS PROGRAM FLOW IN SET,ASYM
C       AND FOR ISPEC WHICH INDICATES SPECIAL REFLECTIONS.
C
      ABANG=STANG(ISPGRP)
      IF(ISPGRP.LE.2) ABANG=ANG
      WRITE(6,110)              ! ORIGTILT header output, with version number.
      WRITE(6,115)ISPGRP
      IF(ISPGRP.GT.9) BLNG=ALNG
      WRITE(6,116)IRUN
      WRITE(6,117)ALNG,BLNG,WIDTH,ABANG
      WRITE(6,125)
      WRITE(6,152) ISPGRP,NPROG,NTILT,NBEAM,ILIST,IPLOT,MINRFL,
     .    LHMIN,LHMAX,IQMAX,IBOXPHS,NREFOUT,NSHFTIN,IAQP2,IVERBOSE,
     .    LOGOUTPUT,RFACAMP
      ASTAR=1.0/(ALNG*SIN(DRAD*ABANG))
      BSTAR=1.0/(BLNG*SIN(DRAD*ABANG))
      WSTAR=1.0/WIDTH
      ABANG=180.-ABANG  ! NOW ABANG IS RECIPROCAL SPACE ANGLE.
      CALL CCPDPN(9,'SUMMARY','UNKNOWN','F',0,0)
C
C     READ FIRST IMAGE DATA
C      (FOR NPROG.EQ.0)
C     THE FIRST IMAGE SHOULD BE UNTILTED AND ONLY THE ASYMMETRIC UNIT
C     SHOULD BE INPUT WITH ALL REFLECTIONS ON THE PROPER PHASE ORIGIN.
C      (FOR NPROG.EQ.1)
C     THE FIRST DATASET IS A PREVIOUSLY MERGED LIST.
C      (FOR NPROG.EQ.2)
C     THE FIRST DATASET IS A FULLY-FLEDGED LCF FILE.
C      (FOR NPROG.EQ.3)
C     THE FIRST DATASET IS A FULLY-FLEDGED MTZ FILE.
C
CHEN>
      OPEN(UNIT=17,FILE=cfile1,STATUS='UNKNOWN')
      call shorten(cfilereflections,k)
      write(cline,'(''\rm -f '',A)')cfilereflections(1:k)
      call shorten(cline,k)
      call system(cline(1:k))
C      OPEN(UNIT=18,FILE='2dx_origtiltk-reflections.log',STATUS='NEW')
      OPEN(UNIT=18,FILE=cfilereflections,STATUS='NEW')
      call shorten(cfileconsole,k)
      write(cline,'(''\rm -f '',A)')cfileconsole(1:k)
      call shorten(cline,k)
      call system(cline(1:k))
C      OPEN(UNIT=21,FILE='2dx_origtiltk-console.log',STATUS='NEW')
      OPEN(UNIT=21,FILE=cfileconsole,STATUS='NEW')
CHEN<
C
      IF(NPROG.GE.2) GO TO 207
C
      READ(5,120) IFILM,TITLE   ! for NPROG.eq.0 or NPROG.eq.1
      WRITE(6,135)IFILM,TITLE
      READ(5,1005) FILIN
 1005 FORMAT(A)
c      OPEN(UNIT=10,FILE=FILIN,READONLY,STATUS='OLD')
      OPEN(UNIT=10,FILE=FILIN,STATUS='OLD',ERR=9199)
      goto 9200
9199  continue
        call shorten(FILIN,k)
        write(*,'(''::ERROR: The File '',A,'' could not be opened.'')')
     .    FILIN(1:k)
        write(21,'(''::ERROR: The File '',A,'' could not be opened.'')')
     .    FILIN(1:k)
        stop 
9200  continue
      WRITE(6,9201)FILIN
9201  FORMAT(': ',/,': INPUT FILE NAME ',A)
      NIN=10
      READ(NIN,*)ISER
C
C      IF(ISER.NE.IFILM) GO TO 602
C
      IF(ISER.NE.IFILM)THEN
C.......GOTO 602
        WRITE(6,'('': WARNING: REQUIRED FILM IDENTIFIER DOES NOT'')')
        write(6,'('': MATCH FILM SERIAL NUMBER AT HEAD OF'',
     .     '' DATALIST:'')')
        WRITE(6,'('': IFILM='',I10,''  ISER='',I10)')IFILM,ISER
        WRITE(21,'('': WARNING: REQUIRED FILM IDENTIFIER DOES NOT'')')
        write(21,'('': MATCH FILM SERIAL NUMBER AT HEAD OF'',
     .     '' DATALIST:'')')
        WRITE(21,'('': IFILM='',I10,''  ISER='',I10)')IFILM,ISER
      ENDIF
C
      BACKSPACE NIN
      READ(NIN,138)TITLE
      WRITE(6,136)TITLE
C
      IF(NPROG.EQ.0) GO TO 201
      IF(NPROG.EQ.1) GO TO 202
C
201   WRITE(6,130)              ! THIS IF NPROG.EQ.0
      WRITE(6,132)              ! THIS IS ONE ASYMMETRIC UNIT OF REFERENCE DATA.
      IF(LIST) WRITE(6,137)
      DO 200 I=1,MAX1
      READ(NIN,*,END=210)IH,IK,A,P
      IF(IH.GE.900) GO TO 210
      JREFL=JREFL+1
      IF (IH.GE.0) JSIGN(JREFL)=1
      IF (IH.LT.0) JSIGN(JREFL)=-1
      JH(JREFL)=IH
      JK(JREFL)=IK
      ZSTAR(JREFL)=0.0
      FLMWGT(JREFL)=1.0
      JOUT(JREFL)=JREFL
      JFILM(JREFL)=IFILM
      AMP(JREFL)=A
      BACK(JREFL)=0.0
      CTFS(JREFL)=1.0
C      IF(A.LT.0.001) GO TO 215
C
C      ALL PHASES ARE STORED AS VALUES BETWEEN -180.0 AND 180.0 DEGREES
C       THE SIN AND COS OF EACH PHASE IS ALSO STORED FOR USE IN AVERAGES
C
      P=AMOD(P,360.)
      IF(P.LT.-180.0) P=P+360.0
      IF(P.GT.180.0) P=P-360.0
      PHS(JREFL)=P
      P=DRAD*P
      CPHS(JREFL)=COS(P)
      SPHS(JREFL)=SIN(P)
C
CHEN   215   IF(LIST)WRITE(6,145)IH,IK,ZSTAR(JREFL),PHS(JREFL),A,JSIGN(JREFL)
CHEN
215   IF(LIST)WRITE(6,'(''HEN1 '',2I5,F10.4,2F10.1,I6,F10.1,I6)')
     1       IH,IK,ZSTAR(JREFL),PHS(JREFL),A,JSIGN(JREFL)
CHEN
C
200   CONTINUE
      WRITE(6,148) MAXRFL
      STOP
C
202   IF(LIST) WRITE(6,139)
      DO 205 I=1,TOTRFL         ! THIS FOR NPROG.EQ.1
      LREFL=JREFL+1
      READ(NIN,*,END=210)JH(LREFL),JK(LREFL),ZSTAR(LREFL),AMP(LREFL),
     . PHS(LREFL),JFILM(LREFL),JSIGN(LREFL),
     . FLMWGT(LREFL),BACK(LREFL),CTFS(LREFL)
      IF(JH(LREFL).GE.900) GO TO 210
      JREFL=JREFL+1
      P=DRAD*PHS(JREFL)
      CPHS(JREFL)=COS(P)
      SPHS(JREFL)=SIN(P)
      IF(LIST)WRITE(6,190)JH(LREFL),JK(LREFL),ZSTAR(LREFL),AMP(LREFL),
     . PHS(LREFL),JFILM(LREFL),JSIGN(LREFL),
     . FLMWGT(LREFL),BACK(LREFL),CTFS(LREFL)
205   CONTINUE
      WRITE(6,148) TOTRFL
      STOP
C
210   WRITE(6,147)JREFL
      JFIRST=JREFL
C
C   Close first file.
C
        CLOSE (UNIT=NIN)
        GO TO 220
C
207     CONTINUE        ! NPROG.EQ.2 or NPROG.eq.3
      IF(NPROG.EQ.2) THEN
C------------------------------------------------------------------------------------------
C                         REFERENCE CURVE INPUT TO STORAGE. (for LCF file)
C------------------------------------------------------------------------------------------
C       INPUT OF AMPL & PHASES FROM LCF FILE FOR TILTAXIS, TILTANGLE REFINEMENT.
C       USE POINTERS TO INDICATE THE BEGINNING OR END OF EACH LATTICE LINE.
C
C       NOTES:- THE FORMULA FOR CALCULATION OF PHASE AT AN ARBITRARY ZSTAR
C               POSITION DOES NOT TREAT THE SYMMETRY OF SPACE GROUPS WITH
C               LATTICE LINES FOR WHICH ZSTAR IS ONLY POSITIVE PROPERLY.
C
C       IMAT SHOWS WHICH MATRICES WILL BE USED FROM MAT FOR EACH SPACE GROUP
C       THE FIRST ELEMENT OF EACH IS PASSED TO ASYM FOR LATER USE.
C       THE SAME IS DONE FOR IGO WHICH CONTROLS PROGRAM FLOW IN SET,ASYM
C       AND FOR ISPEC WHICH INDICATES SPECIAL REFLECTIONS.
C
C       ALL PHASES ARE STORED AS VALUES BETWEEN -180.0 AND 180.0 DEGREES
C
C       INPUT OF PHASES OF NATIVE DATA 
C          (TITLE WITH THE PHASES IS CARRIED INVISIBLY IN /LCF/ COMMON BLOCK)
C
        CALL SRLCF1(1,'HKLIN',26,'H K L S FC SIGFC PHCAL FOM',LOOKUP,
     .. TRUE.,NCOL,CELL)
        IF(STANG(ISPGRP).EQ.CELL(6))  GO TO 1105
          WRITE(6,1109) STANG(ISPGRP),CELL(6)
1109      FORMAT(' CONFLICT BETWEEN CELL ANGLES FROM SPACE GROUP'/
     .   ' AND LCF INPUT FILE')
          STOP
1105    ABANG = 180.0 - CELL(6)
        WRITE(6,1113)ALNG,CELL(1),BLNG,CELL(2),WIDTH,CELL(3)
1113    FORMAT(' COMPARISON OF CELL DIMENSIONS READ IN WITH CELL',
     . ' DIMENSIONS'/'  FROM LCF FILE OF AMPS AND PHASES'/
     . ' THEY SHOULD BE THE SAME ***********************************'/
     . '            READ IN       LCF FILE'/
     . '  A=',2F15.2/'  B=',2F15.2/'  C=',2F15.2)
C       ASTAR=1.0/(CELL(1)*SIN(DRAD*ABANG))
C       BSTAR=1.0/(CELL(2)*SIN(DRAD*ABANG))
C       CSTAR=1.0/ CELL(3)
C       WSTAR=CSTAR/3.0
        MH=LOOKUP(1)
        MK=LOOKUP(2)
        ML=LOOKUP(3)
        MS=LOOKUP(4)
        MFC=LOOKUP(5)
        MSIGFC=LOOKUP(6)
        MPHCAL=LOOKUP(7)
        MFOM=LOOKUP(8)
        IFOMLIMIT=1     ! corresponds to classical FOM of 0.01
        NLCFOK=0
        NLCFPH=0
        NLCFAM=0
        NREC=0
        WRITE(6,197)
1101      CALL RLCF1(IDATAIN,*1110,*1110)
          NLCFOK=NLCFOK+1
          NREC=NREC+1
          IF(NLCFOK.GT.NMAXC) GO TO 1150
          ITH=IDATAIN(MH)
          ITK=IDATAIN(MK)
          IF(ABS(ITH).GT.MAXINDEX.OR.ABS(ITK).GT.MAXINDEX) THEN
            WRITE(6,196) MAXINDEX
            STOP
          ENDIF
          IHC(NLCFOK)=ITH
          IKC(NLCFOK)=ITK
          ILC(NLCFOK)=IDATAIN(ML)
          ISC(NLCFOK)=IDATAIN(MS)
          IF(ISC(NLCFOK).LT.SMIN) SMIN=ISC(NLCFOK)  ! SMIN,SMAX NOT USED YET.
          IF(ISC(NLCFOK).GT.SMAX) SMAX=ISC(NLCFOK)
C---------HERE TEST FOM TO BE  > IFOMLIMIT  BEFORE ACCEPTING CURVE DATA.
C---------AND TEST FOR PRESENCE OF THE STRUCTURE FACTOR (USING SIGFC)
          IF(IDATAIN(MFOM).GE.IFOMLIMIT) NLCFPH=NLCFPH+1        ! CRITERIA FOM.
          IF(IDATAIN(MSIGFC).GT.0.0)     NLCFAM=NLCFAM+1        ! CRITERIA SIGF.
          IF(IDATAIN(MFOM).GE.IFOMLIMIT.AND.
     .     IDATAIN(MSIGFC).GT.0.0) GO TO 1102
          NLCFOK=NLCFOK-1               ! this was not a valid point after all.
        GO TO 1101
1102    continue
        IFCC(NLCFOK)=IDATAIN(MFC)
        IPHC(NLCFOK)=IDATAIN(MPHCAL)
        IF(NLCFOK.NE.1) GO TO 1103
          IHOLD=IHC(NLCFOK)
          IKOLD=IKC(NLCFOK)
          IBEGIN(IHOLD,IKOLD)=1
1103    IF((IHC(NLCFOK).EQ.IHOLD).AND.(IKC(NLCFOK).EQ.IKOLD)) GO TO 1101
        IFINISH(IHOLD,IKOLD)=NLCFOK-1
        IF(LIST)WRITE(6,*) IHOLD,IKOLD,ILC(IBEGIN(IHOLD,IKOLD)),
     .   ILC(IFINISH(IHOLD,IKOLD)),
     .   IBEGIN(IHOLD,IKOLD),IFINISH(IHOLD,IKOLD)
        IHOLD=IHC(NLCFOK)
        IKOLD=IKC(NLCFOK)
        IBEGIN(IHOLD,IKOLD)=NLCFOK
        GO TO 1101
1110    IFINISH(IHOLD,IKOLD)=NLCFOK
        FOMLIMIT=IFOMLIMIT/100.0
        WRITE(6,1100)NLCFOK,NREC,NLCFAM,NLCFPH,FOMLIMIT
1100    FORMAT(I10,' GOOD S.F.s (i.e.Amp+Phs) read in from LCF FILE'/
     . I10,' TOTAL RECORDS ON THE FILE'/
     . I10,' RECORDS HAD AMPLITUDES WITH NON-ZERO SIGFC'/
     . I10,' RECORDS HAD PHASES WITH FOM >',F5.2)
        CALL CRLCF1
        GO TO 220
      ELSE
C------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------
C-------here only if NPROG.eq.3 -- i.e. for mtz file input
C------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------
        CALL CCPFYP
        CALL MTZINI
        CALL LROPEN(1,'HKLIN',3,IERR)
        CALL LRCELL(1,CELL)
        CALL LRSYMM(1,NSYMX,RSYMX)
        IF(IERR.NE.0) THEN
          WRITE (6,11006)IERR
11006     FORMAT(' ERROR ON INPUT OF MTZ FILE, IERR=',I5)
          STOP
        ENDIF
C-------Find out how many columns and reflections in input file
        CALL LRINFO(1,DUMMY,NCOL,NREF,DUM)
        CALL LKYASN(1,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C
        IF(ISPGRP.GE.3.AND.STANG(ISPGRP).NE.CELL(6)) THEN
          WRITE(6,11008) STANG(ISPGRP),CELL(6)
11008     FORMAT(' Conflict between cell angles from space group',
     $    ' and mtz input file, STANG, CELL=',2F8.3)
          STOP
        ENDIF
C
        IF(ABANG.NE.180.0 - CELL(6)) 
     .   STOP' Conflict between GAMMA and MTZ cell angle'
        N=0
        NAMPS=0
        NPHASES=0
        NREC=0
C
11007   CALL LRREFF(1,RESOL,ADATAIN,EOF)
          IF(EOF) GO TO 11003
          NREC=NREC+1
          IF(ADATAIN(5).EQ.0) GO TO 11007       ! CRITERIA ON SIGF
          N=N+1
          NAMPS=N
          IF(N.GT.NMAXC) GO TO 1150
C
CHEN>
C          ITMP=ADATAIN(4)
C          write(*,'(''### Read '',I9)')ITMP
C          if(ITMP.gt.30000)then
C            write(6,'('':: ERROR: DATA OVERFLOW in origtilt for IFCC. Scale down your data'')')
C            stop
C          endif
CHEN<
          IHC(N)=ADATAIN(1)
          IKC(N)=ADATAIN(2)
          IF(ABS(IHC(N)).GT.MAXINDEX.OR.ABS(IKC(N)).GT.MAXINDEX) THEN
            WRITE(6,196) MAXINDEX
            STOP
          ENDIF
          ILC(N)=ADATAIN(3)
          ISC(N)= 10000.0 * ((IHC(N)*ASTAR)**2 + 
     .     2*IHC(N)*IKC(N)*ASTAR*BSTAR*COS(DRAD*ABANG) + 
     .     (IKC(N)*BSTAR)**2+(ILC(N)*WSTAR)**2)
          IFCC(N)=ADATAIN(4)
          IPHC(N)=ADATAIN(6)
          IFOM(N)=ADATAIN(7)*100.0      ! compatibility LCF vs MTZ
          IF(IFOM(N).GE.IFOMLIMIT) NPHASES=NPHASES+1
C
          IF(N.EQ.1) THEN
            IHOLD=IHC(N)
            IKOLD=IKC(N)
            IBEGIN(IHOLD,IKOLD)=1
          ENDIF
          IF(.NOT.((IHC(N).EQ.IHOLD).AND.(IKC(N).EQ.IKOLD))) THEN
            IFINISH(IHOLD,IKOLD)=N-1
            IHOLD=IHC(N)
            IKOLD=IKC(N)
            IBEGIN(IHOLD,IKOLD)=N
          ENDIF
C
C          if(IVERBOSE.gt.8)then
C            write(*,'('' Reference: H,K,L,F,P,FOM,F = '',6I8,F12.3)')
C     1        IHC(N),IKC(N),ILC(N),IFCC(N),IPHC(N),IFOM(N),ADATAIN(4)
C          endif
C
        GO TO 11007
C
11003   WRITE(6,11004)
11004   FORMAT(' end of mtz input')
        IFINISH(IHOLD,IKOLD)=N
C
        write(6,'(//,'' Lattice line ranges:'',/)')
C
        do I=-10,10
          do J=-10,10
            i1=IBEGIN(I,J)
            i2=IFINISH(I,J)
            if(i1.ne.-999 .and. i2.ne.-999)then
              write(6,'('' Lattice line '',2I4,'' has pointers '',2I6,
     1          '' to limits '',2F12.3)')
     1          I,J,i1,i2,ILC(i1)/CELL(3),ILC(i2)/CELL(3)
            endif
          enddo
        enddo
C
        WRITE(6,11005) NPHASES,NREC
11005   FORMAT(I10,' Phases input on stream 1'/
     $         I10,' total records on stream 1.')
        CALL LRCLOS(1)
      ENDIF      
C
C------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------
C     READ TILTED DATA
C------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------
C
220   WRITE(6,125)
C-----WHEN (NPROG.GE.2), NEW FILMS ARE COMPARED ONLY TO REFERENCE DATASET.
C-----WHEN (NPROG.EQ.1), NEW FILMS ARE COMPARED ONLY TO FIRST DATASET(PREV MERGED)
      IF (NPROG.EQ.1) JREFL=JFIRST
      READ(5,120)IFILM,TITLE
      IF(IFILM.LT.0) GO TO 500
      WRITE(6,135) IFILM,TITLE
      READ(5,1005) FILIN
      OPEN(UNIT=10,FILE=FILIN,STATUS='OLD',ERR=221)
      goto 222
 221  continue
        call shorten(FILIN,k)
        write(*,'(''::ERROR: File '',A,'' could not be opened.'')')
     .    FILIN(1:k)
        write(21,'(''::ERROR: File '',A,'' could not be opened.'')')
     .    FILIN(1:k)
        stop 
 222  continue
      WRITE(6,9201)FILIN
      READ(5,*) NWGT
      write(6,'('' NWGT = '',L1)')NWGT
C
      READ(5,*) TAXA,TANGL,IORIGT
CHEN
      read(5,*)rlattu1,rlattu2,rlattv1,rlattv2
C
      HORITAXA=TAXA
      HORITANGL=TANGL
CHEN
      TAXB=TAXA+ABANG
      WRITE(6,155)TAXA,TAXB,TANGL
      WRITE(6,195)IORIGT
      READ(5,*)ORIGH,ORIGK,STEP,WIN,
     1         SGNXCH,SCALE,ROT180,REVHK,CTFREV,ROT90,REVHND,REVXSGN,
     .          LPROTFOUFIL,LUSEML
CHEN
      HHORIORI=ORIGH
      HKORIORI=ORIGK
CHEN
      READ(5,*)CS,KV,TLTH,TLTK
C
      TILTH=0.0
      TILTK=0.0
CHEN
      HORITLTH=TLTH
      HORITLTK=TLTK
CHEN
      IF(WIN.GT.0.0) WSTAR=WIN
      WRITE(6,162)ORIGH,ORIGK,STEP,WSTAR
      WRITE(6,163)SCALE,SGNXCH,ROT180,REVHK,CTFREV,ROT90,REVHND,
     .          REVXSGN,
     .          LPROTFOUFIL,LUSEML
      WRITE(6,165)CS,KV,TLTH,TLTK
      KV=KV*1000.0
      WL=12.3/SQRT(KV+KV**2/(10.0**6.0))
      WRITE(6,103)WL
      BEAMSHFT(1)=0.360*CS*10.0**7*WL**2
C
C-----READ RESOLUTION LIMITS FOR THIS FILM; DEFAULT 100.0  3.5
C
      READ(5,*)DRESMAX,DRESMIN
      if(DRESMAX.lt.DRESMIN)then
         dtmp=DRESMIN
         DRESMIN=DRESMAX
         DRESMAX=dtmp
      endif
      IF(DRESMAX.EQ.0.0)DRESMAX=100.
      IF(DRESMIN.EQ.0.0)DRESMIN=3.5
      WRITE(6,9165) DRESMAX,DRESMIN
9165  FORMAT(/' CALCULATIONS WILL USE REFLECTIONS IN ',
     . 'RESOLUTION RANGE ',F6.2,' TO ',F6.2,'A'/)
C
      IF(SGNXCH.NE.0.0) WRITE(6,164)
      NIN=10
C     OPEN(UNIT=NIN,READONLY,STATUS='OLD')
      READ(NIN,*)ISER
      IF(ISER.NE.IFILM)THEN
C.......GOTO 602
        WRITE(6,'('' WARNING: REQUIRED FILM IDENTIFIER DOES NOT'')')
        write(6,'('' MATCH FILM SERIAL NUMBER AT HEAD OF DATALIST:'')')
        WRITE(6,'('' IFILM='',I10,''  ISER='',I10)')IFILM,ISER
      ENDIF
      BACKSPACE NIN
      READ(NIN,138)TITLE
      WRITE(6,136)TITLE
C
      DO 250 IN=1,MAX1
9250    IF (NWGT) THEN
           READ(NIN,*,END=260)IH,IK,A,P,IQ,BCK,CTF,W
        ELSE
           READ(NIN,*,END=260)IH,IK,A,P,IQ,BCK,CTF
C          write(*,'(''::Here1: '',2I6,2F10.3,I4,2F10.3)')
C     .       IH,IK,A,P,IQ,BCK,CTF
           W=1.000
        ENDIF
C
        if(A.gt.rmaxamp)rmaxamp=A
C
        IF(IQ.LT.1)IQ=1
        IF(IQ.GT.9)IQ=9         ! DANGEROUS STATEMENT FOR FUTURE -- BEWARE!
        IF(IH.GE.900) GO TO 260
C-------RESOLUTION CHECK  (ON H,K ONLY)
        DSTARSQ=(IH*ASTAR)**2+2*IH*IK*ASTAR*BSTAR*COS(DRAD*ABANG)
     .   +(IK*BSTAR)**2
        DRES=1.0/SQRT(DSTARSQ)
        IF(DRES.LT.DRESMIN.OR.DRES.GT.DRESMAX)GO TO 9250
C
        IF(abs(CTFREV).lt.0.01) GO TO 224
          P=P+180.0                             ! CTFREV HERE - FOR LOW DOSE IMAGES.
224     CONTINUE
C
C-------Store read values in fields AIN,BIN,CTFIN,PHASIN,WGTIN,IHIN,IKIN,IQIN,JIN
C
        AIN(IN)=A                               ! HERE A IS STORED
        BIN(IN)=BCK                     ! HERE background IS STORED
        CTFIN(IN)=CTF                   ! HERE ctf IS STORED
        PHASIN(IN)=P                    ! HERE P IS STORED
        WGTIN(IN)=W                     ! HERE FILM WEIGHT IS STORED
        IHIN(IN)=IH                     ! HERE IH IS STORED AS INPUT
        IKIN(IN)=IK                     ! HERE IK IS STORED AS INPUT
        IQIN(IN)=IQ
        JIN=JREFL+IN
C
        IF (JIN.GT.TOTRFL) GO TO 601
        IF (IH.GE.0) JSIGN(JIN)=IQ      ! HERE IQ IS STORED
        IF (IH.LT.0) JSIGN(JIN)=-1*IQ
C
C-------JSIGN>0: H>=0 FOR THIS REFLECTION IN SPOTDATA (I.E.BEFORE APPLYING SYMMETRY)
C-------JSIGN<0: H<0 FOR THIS REFLECTION IN SPOTDATA
C-------ABSOLUTE VALUE OF JSIGN PROVIDES INFORMATION ON QUALITY
C-------OF PHASE.  PHASE IS PLOTTED WITH THE SYMBOL NOTED IN
C-------IN PARENTHESES.  1=GOOD(*), 2=OKAY(#), 3= UNCERTAINTY IN
C-------CONTRAST TRANSFER FUNCTION(+), 4=SPLIT SPOT BUT PROBABLY
C-------OKAY, OR PEAK OFF LATTICE WITH PHASE PLATEAU ON LATTICE(X),
C-------5=QUESTIONABLE(TRIANGLE), 6=PHASE GRADIENT(SQUARE),
C-------7=AMPLITUDE BELOW NOISE LEVEL(CIRCLE)
C
250   CONTINUE
C
      WRITE(6,148) MAXRFL
      STOP
C
C--------------------------------------------------------------------------------
C-----Tilted real data are read in.
C--------------------------------------------------------------------------------
C
260   CONTINUE
      IN1=IN-1
      WRITE(6,166) IN1
C
C                      REPEAT FROM HERE IF NPROG.GE.2
C
C-----Weighting with amplitudes:
      if(RFACAMP.gt.0.0)then
C-------Determine highest amplitude
        rmaxamp = -99999999.9
        do I=1,IN1
          if(AIN(I).gt.rmaxamp)rmaxamp=AIN(I)
        enddo
C
        if(abs(rmaxamp).lt.0.001)rmaxamp=0.001
        do I=1,IN1
          WGTIN(I)=1.0-RFACAMP+(RFACAMP*AIN(I)/RMAXAMP)
        enddo
      endif
C
      ICALL=0
      IFINSH=0
      IREFOUT =0 
C
270   TAXB = TAXA + ABANG
C
      STAXA=ASTAR*SIN(DRAD*TAXA)
      STAXB=BSTAR*SIN(DRAD*TAXB)
      TTANGL=TAN(TANGL*DRAD)
      ASUM=0.0
      ASUMI=0.0
      NCOMP=0
      NCOMPI=0
      NINEC=0
      SHMIN=0.0
      SKMIN=0.0
      IF(LIST)WRITE(6,137)
C
C--------------------------------------------------------------------------------
C-----Prepare HKL/REF********.hkl file for comparisson or reference image
C--------------------------------------------------------------------------------
C
      IF(NREFOUT .and. IREFOUT.eq.0) THEN
        write(FNAME,30)IFILM
30      FORMAT('APH/REF',I10,'.hkl')
        do i=8,16
          if(FNAME(i:i).eq.' ')FNAME(i:i)='0'
        enddo
C
        call shorten(FNAME,k)
        write(cline1,'(''\rm -f '',A)')FNAME(1:k)
        call system(cline1)
C
        OPEN(UNIT=4,FILE=FNAME,FORM='FORMATTED',STATUS='NEW')
        IREFOUT = IREFOUT + 1
        WRITE(4,31)TITLE
31      FORMAT(10A4,'  Reference amps + phases')
C
        write(*,'('':: Reference file opened for output: '',A)')
     .   FNAME(1:k)
C
      ENDIF
C
C-----Work on the read reflections from fields IHIN,IKIN,...
C
      iminh =  99999
      imink =  99999
      rminz =  99999.0
      imaxh = -99999
      imaxk = -99999
      rmaxz = -99999.0
C
      DO 291 IN=1,IN1
        IH = IHIN(IN)
        IK = IKIN(IN)
        IQ = IQIN(IN)
        A  = AIN(IN)
        P  = PHASIN(IN)
        W  = WGTIN(IN)
        IP1(IN)=1
        IP2(IN)=0
        WGT(IN)=0.0
        PHSC(IN)=0.0
        DPERP=IH*STAXA+IK*STAXB
        Z=DPERP*TTANGL
CHEN>
C        if(abs(IH).lt.3 .and. abs(IK).lt.3) then
C          PTMP=AMOD(P,360.)
C          write(*,'('':: IH='',I6,'', IK='',I6,'' : Read Reflections: A='',F12.3,'', P='',F10.3)')IH,IK,A,PTMP
C        endif
CHEN<
C
        IOK  = .TRUE.
        IOK2 = .TRUE.
C
C-------Apply all kinds of flags to this reflection, like ROT90:
C
C       write(*,'('' Tilted image original: H,K,Z,A,P = '',2I6,3F12.3)')
C     1     IH,IK,Z,A,P
C
        CALL FIDDLE2(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90,REVHND,REVXSGN)
C
C       write(*,'('' Tilted image FIDDLE2 : H,K,Z,A,P = '',2I6,3F12.3)')
C     1     IH,IK,Z,A,P
C
C-------Add PHSHFT:
C-------NOTE: This is done AFTER application of ROT90.
C
        RADSQ=IH**2*ASTAR**2 + IK**2*BSTAR**2 +
     .      2.0*IH*IK*COS(0.0174532*ABANG)*ASTAR*BSTAR+Z**2
        BSH(IN)=BEAMSHFT(1)*RADSQ               ! HERE BSH IS STORED
C
        P=P+PHSHFT(IH,IK,ORIGH,ORIGK,TLTH,TLTK,BEAMSHFT,BSH(IN))
C
        PHSI(IN)=AMOD(P,360.)           ! HERE P IS STORED
        IIH(IN)=IH                      ! HERE IH IS STORED
        IIK(IN)=IK                      ! HERE IK IS STORED
        JIN=JREFL+IN
C
C-------ASYM places reflections into asymetric unit and applies special
C-------selection rules.
C
C-------  SUBROUTINE ASYM(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
C------- 1      A1,A2,A3,A4,A5,IGO,ISPEC,LREV,IAQP2,IVERBOSE)
C
        CALL ASYM(IH,IK,Z,IP1(IN),IP2(IN),LSPEC(IN),IPTEST(IN),
     1    WSTAR,MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     2    MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     3    MAT(1,IMAT(5,ISPGRP)),
     4    IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2,IVERBOSE)
C
C-------for determination of borders of asymetric unit:
        if(IH.lt.iminh)iminh=IH
        if(IH.gt.imaxh)imaxh=IH
        if(IK.lt.imink)imink=IK
        if(IK.gt.imaxk)imaxk=IK
        if(Z .lt.rminz)rminz=Z
        if(Z .gt.rmaxz)rmaxz=Z
C
C       write(*,'('' Tilted image ASYM    : H,K,Z,A,P = '',2I6,3F12.3)')
C     1     IH,IK,Z,A,P
C       write(*,'('' '')')
C
C.........IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C.........IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C.........REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C.........SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C.........WILL BE PERFORMED IN P1.
C.........
C.........LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C.........SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C.........IF IT SHOULD BE IMAGINARY
C
C
        JH(JIN)=IH
        JK(JIN)=IK
        ZSTAR(JIN)=Z
        FLMWGT(JIN)=W
        DSTARSQ=(JH(JIN)*ASTAR)**2+
     .     2*JH(JIN)*JK(JIN)*ASTAR*BSTAR*COS(DRAD*ABANG)+
     .     (JK(JIN)*BSTAR)**2+ZSTAR(JIN)**2
        DRES=1.0/SQRT(DSTARSQ)
        RESPOT(IN)=DRES
        IF(Z.LT.ZMIN) ZMIN=Z
        IF(Z.GT.ZMAX) ZMAX=Z
        IF(SCALE.GT.0.00001 .AND. STEP.LT.0.001) GO TO 290  ! S.F. & ORIGIN INPUT
C
        NSUM=0
C
CHEN>
C-------Check if this reflection is within valid boundaries:
C
        IF(DRES.LT.DRESMIN.OR.DRES.GT.DRESMAX)then
          IOK2 = .FALSE.
        endif
C
        IF(IQ.GT.IQMAX)then
C         write(6,'('' IQ > IQMAX for IQ,IQMAX,H,K,Z,W = '',4I6,2F12.3)')
C     1      IQ,IQMAX,IH,IK,Z,W
          IOK2 = .FALSE.
        endif
C
C-------Skip reflections without significant amplitude ?
C       IF(A.LT.0.001)THEN
C          IOK2 = .FALSE.
C        endif
C
C        if(.not.IOK2) goto 244
        if(.not.IOK2) goto 245
CHEN<
        IF(LSPEC(IN)) NINEC=NINEC+1
C
C========================================
        IF(NPROG.EQ.0.OR.NPROG.EQ.1) THEN
C========================================
C
C---------here for pure merging or refinement against previous merge
C
C         THIS SECTION FOR COMPARISON WITH PREVIOUS FILMS.
          CSUM=0.0
          SSUM=0.0
          DO 240 IREF=1,JREFL
C
C           IF(AMP(IREF).LT. 0.001) GO TO 240
C
            IF(JK(JIN).NE.JK(IREF)) GO TO 240
            IF(JH(JIN).NE.JH(IREF)) GO TO 240
            IF(IFILM.EQ.JFILM(IREF)) GO TO 240
            IF(IABS(JSIGN(IREF)).GT.IQMAX) GO TO 240
            IF(ABS(ZSTAR(JIN)-ZSTAR(IREF)).GT.WSTAR) GO TO 240
C
            NCOMP=NCOMP+1
            NSUM=NSUM+1
            ASUM=ASUM+AMP(IREF)
C
C           WRITE(6,900)IIH(IN),IIK(IN),JH(IREF),JK(IREF),ZSTAR(IREF)
C           WRITE(6,900) NCOMP,NSUM,IN,IREF,NCOMPI
C900        FORMAT(8G12.5)
C
            IF(STEP.EQ.0.0) GO TO 240
C
            IARG=IABS(JSIGN(IREF))
            WEIGHT=1.0/MAX0(2,IARG)
            IF(IARG.EQ.8) WEIGHT = 0.005
            IF(IARG.GT.8) WEIGHT = 0.0
            IF(AMP(IREF).LT.0.001) WEIGHT = 0.0
            CSUM=CSUM+CPHS(IREF)*WEIGHT ! weighted vector average, with
            SSUM=SSUM+SPHS(IREF)*WEIGHT ! an IQ of 1 or 2 getting equal weight.
240       CONTINUE
C
          IF(NSUM.EQ.0) GO TO 290
C
          NCOMPI=NCOMPI+1
          ASUMI=ASUMI+A*NSUM
C
          IF(STEP.EQ.0.0) GO TO 290
C
C---------SPOTS FROM PREVIOUS FILMS ARE COMPARED WITH THE INPUT FILM SPOTS
C---------BY TRANSFORMING THE PREVIOUS FILMS' PHASES WITH IP1 AND IP2
C---------TO CORRESPOND TO REFLECTIONS WITH THE SAME INDICES AS THE
C---------INPUT REFLECTIONS. THE COMPARISON IS THEN DONE IN P1.
C
          PHSC(IN)=RDEG*ATAN2(SSUM,CSUM)*IP1(IN)-IP2(IN)
        ENDIF
C
C----------------------------------------------------------------
C
 244    continue
C
C========================================
        IF(NPROG.GE.2)THEN
C========================================
C
C---------here for LCF file or MTZ file reference:
C
C---------calculate interpolated lattice-line value for this tilted reflection:
C
          CALL GETCRVAL(IN,IHIN,IKIN,IH,IK,Z,ILC,IFCC,IPHC,
     .     IBEGIN,IFINISH,ISPEC(1,ISPGRP),
     .     IOK,CELL(3),FREF,PREF,DPDZCU,IVERBOSE)
C
C---------Is there a valid lattice line data ?:
          IF(.NOT.IOK) GO TO 245
C
          NCOMP=NCOMP+1
          NCOMPI=NCOMPI+1
          ASUM=ASUM + FREF
          ASUMI=ASUMI + A
          NSUM=1
C
          IF(STEP.EQ.0.0) GO TO 290
C
C---------SAME PHASE TRANSFORMATION AS ABOVE.
          PHSC(IN)=PREF*IP1(IN)-IP2(IN)
C
        ENDIF
C
245     WGT(IN)=NSUM
C
        IF(NSUM.EQ.0) GO TO 290
C
          IF(IORIGT.EQ.1) WGT(IN)=1.0
C
290     continue
C
292     IF(NREFOUT .and. IREFOUT.eq.1) THEN
          IF(IOK .and. IOK2)then
CHEN--------IF(IOK)then
C-----------Output of the reference data for checking:
            IF(IIK(IN).GE.0) THEN
              WRITE(4,293) IIH(IN),IIK(IN),IZERO,FREF,PHSC(IN),1.0
            ELSE         ! make K positive in p1, and change phase.
              WRITE(4,293) -IIH(IN),-IIK(IN),IZERO,FREF,-PHSC(IN),1.0
            ENDIF
293         FORMAT(3I5,3G16.6)
CHEN>
C        if(abs(IIH(IN)).lt.3 .and. abs(IIK(IN)).lt.3) then
C          write(*,'('':: IH='',I6,'', IK='',I6,'' : REFERENCE DATA: A='',F12.3,'', P='',F10.3)')
C     .      IIH(IN),IIK(IN),FREF,PHSC(IN)
C        endif
CHEN<
          endif
        ENDIF
C
C       WRITE(6,900)IN,JIN,NCOMP,WGT(IN),NCOMPI,NSUM,IP1(IN),IP2(IN)
C
        IF(LIST)WRITE(6,145)IIH(IN),IIK(IN),ZSTAR(JREFL+IN),
     .     PHSI(IN),AIN(IN),IQ,PHSC(IN),NSUM
C
291   continue
C
      IF(NREFOUT .and. IREFOUT.eq.1) CLOSE(4)
C
C      write(6,'('' '')')
C      write(6,'('' Tilted reflections after ASYM are in :'')')
C      write(6,'('' H: '',I6,'' ... '',I6)')iminh,imaxh
C      write(6,'('' K: '',I6,'' ... '',I6)')imink,imaxk
C      write(6,'('' Z: '',F6.3,'' ... '',F6.3)')rminz,rmaxz
C      write(6,'('' '')')
C
295   IF(STEP.EQ.0.0) GO TO 400      ! NO REFINEMENT OF TILT OR PHASE ORIGIN.
C
C---------------------------------------------------------------------------
C-----Do determination of error for the various shifts
C---------------------------------------------------------------------------
C
      SH  = -0.5*(IBOXPHS+1)*STEP
      SK0 = -0.5*(IBOXPHS+1)*STEP
C
      WRITE(6,168)NCOMPI,NCOMP,NINEC
C
      NCOMPT=NINEC+NCOMP
      NCOMP9=NINEC+NCOMPI
      IF(IORIGT.EQ.1) WRITE(6,193) NCOMP9
      WRITE(6,169) NCOMPT
C
      ERRMIN=999.9
C
C-----Loop over all shift tries in ISH, ISK:
C
      DO 350 ISH=1,IBOXPHS
        SH=STEP+SH
        SK=SK0
        DO 350 ISK=1,IBOXPHS
          SK=STEP+SK
          SERR=0.0
C
C---------Loop over all tilted reflections
C
          DO 340 IREFC=1,IN1
            JIN=IREFC+JREFL
            P=PHSI(IREFC)+PHSHFT(IIH(IREFC),IIK(IREFC),SH,SK,
     .       TILTH,TILTK,BEAMSHFT,BSH(IREFC))
            P=AMOD(P,360.)
            IF(WGT(IREFC).LT.1.0) GO TO 325
              DELTA=P-PHSC(IREFC)
              IF(DELTA.LT.0.0) DELTA=-DELTA
310           IF(DELTA.LE.180.0) GO TO 320
                DELTA=DELTA-360.0
              GO TO 310
320           IF(DELTA.LT.0.0) DELTA=-DELTA
              SERR=WGT(IREFC)*DELTA+SERR
325         CONTINUE
            IF(.NOT.LSPEC(IREFC)) GO TO 340
            IF(IQIN(IREFC).GT.IQMAX)GO TO 340
            IF(RESPOT(IREFC).LT.DRESMIN.OR.RESPOT(IREFC).GT.DRESMAX)
     1        GO TO 340
C
            IF(P.GT.180.0) P=P-360.0
            IF(P.LT.-180.0) P=P+360.0
            IF(P.LT.0.0) P=-P
            DELTA=P-IPTEST(IREFC)
            IF(DELTA.LT.0.0) DELTA=-DELTA
            IF(DELTA.GT.90.0) DELTA=180.0-DELTA
C
C-----------Accumulate the error for this shift
C
            SERR=SERR+DELTA
C
340       CONTINUE
          IF(NCOMP9.NE.0)THEN
            SERR9=SERR/NCOMP9
          ENDIF
          IF(NCOMPT.NE.0)THEN
            SERR=SERR/NCOMPT
          ENDIF
C
C---------Prevent from SERR = 0.0
C
          if(SERR .eq.0.0)SERR =999.0
          if(SERR9.eq.0.0)SERR9=999.0
C
C---------NCOMPT IS NUMBER OF SPECIAL SPOTS + NUMBER OF COMPARISONS WITH
C---------ALL PREVIOUS COMPARABLE SPOTS
C---------NCOMP9 IS NUMBER OF SPECIAL SPOTS + NUMBER OF NEW SPOTS WHICH
C---------HAVE AT LEAST ONE OLD COMPARABLE SPOT
C
          IF(IORIGT.EQ.1) SERR=SERR9
          IF(SERR.GE.ERRMIN) GO TO 345
            SKMIN=SK
            SHMIN=SH
            ISKMIN=ISK
            ISHMIN=ISH
            ERRMIN=SERR
345       CONTINUE
          ISERR=(90.0-SERR)/10
          IF(ISERR.LT.0) ISERR=0
          IRP(ISH,ISK)=ISERR
350   CONTINUE
C
      WRITE(6,171) ERRMIN,ISHMIN,ISKMIN
C---------------------------------------------------------------------------
C---------------------------------------------------------------------------
C---------------------------------------------------------------------------
C
      ORIGHNEW=ORIGH+SHMIN
      ORIGKNEW=ORIGK+SKMIN
C      WRITE(6,173) SHMIN,SKMIN,ORIGHNEW,ORIGKNEW
C
CHEN
C
      TMP1=ORIGHNEW
      if(TMP1.gt. 180.0) TMP1=TMP1-360.0
      if(TMP1.lt.-180.0) TMP1=TMP1+360.0
      TMP2=ORIGKNEW
      if(TMP2.gt. 180.0) TMP2=TMP2-360.0
      if(TMP2.lt.-180.0) TMP2=TMP2+360.0
C
      HSHMIN=SHMIN
      HSKMIN=SKMIN
      HOHRIGSH=ORIGH+SHMIN
      HOKRIGSH=ORIGK+SKMIN
C
      WRITE(6,173) SHMIN,SKMIN,TMP1,TMP2
C
C---------------------------------------------------------------------------
C.....REPEAT CALCULATION FOR COORDS OF BEST RESIDUAL TO GET RESIDUAL
C.....AS FUNCTION OF RESOLUTION WITH ORIGIN IN THIS POSITION
C---------------------------------------------------------------------------
C
C.....CLEAR ARRAYS FOR HISTOGRAM
C
      DO 18010 J=1,NSLOTS
        NRESO(J)=0      ! ZERO HIST0GRAM
        ERRES(J)=0.0
18010 SERRES(J)=0.0
C
      ISH=ISHMIN
      SH=ISH*STEP - 0.5*(IBOXPHS+1)*STEP
      ISK=ISKMIN
      SK=ISK*STEP - 0.5*(IBOXPHS+1)*STEP
      DO 1340 IREFC=1,IN1
        IF(RESPOT(IREFC).LT.DRESMIN.OR.RESPOT(IREFC).GT.DRESMAX) GOTO 1340
        JIN=IREFC+JREFL
C       CALCULATE RESOLUTION OF SPOT 
        if(abs(RESPOT(IREFC)).gt.0.001)then
          DSTARSQ=(1.0/RESPOT(IREFC))**2
        else
          DSTARSQ=(1.0/0.001)**2
        endif
C       DSTARSQ = (IIH(IREFC)*ASTAR)**2 + (IIK(IREFC)*BSTAR)**2 + 
C     . 2.0*IIH(IREFC)*IIK(IREFC)*ASTAR*BSTAR*COS(DRAD*ABANG)
C     .+ZSTAR(JIN)**2
        IRES=DSTARSQ*10000.
C****
        ISLOT= 1 + (IRES-1)/IRESTEP
        IF(ISLOT.LT.1.OR.ISLOT.GE.NSLOTS) THEN
          write(6,'('' RESPOT,IREFC = '',F12.1,I12)')RESPOT(IREFC),IREFC
          write(6,'('' IIH,IIK = '',2I12)')IIH(IREFC),IIK(IREFC)
          write(6,'('' IRES,IRESTEP,IREFC,IN1 = '',4I12)')IRES,IRESTEP,IREFC,
     .       IN1
          WRITE(6,20000)ISLOT
20000     FORMAT(' ERROR, ISLOT=',I10)
          STOP
        END IF
        P=PHSI(IREFC)+PHSHFT(IIH(IREFC),IIK(IREFC),SH,SK,
     .   TILTH,TILTK,BEAMSHFT,BSH(IREFC))
        P=AMOD(P,360.)
        IF(WGT(IREFC).LT.1.0) GO TO 1325
          DELTA=P-PHSC(IREFC)
          IF(DELTA.LT.0.0) DELTA=-DELTA
1310      IF(DELTA.LE.180.0) GO TO 1320
            DELTA=DELTA-360.0
            GO TO 1310
1320      IF(DELTA.LT.0.0) DELTA=-DELTA
          SERR=WGT(IREFC)*DELTA+SERR
C
          SERRES(ISLOT)=SERRES(ISLOT)+WGT(IREFC)*DELTA
          NRESO(ISLOT)=NRESO(ISLOT)+WGT(IREFC)
C
C         WRITE(6,20001)IIH(IREFC),IIK(IREFC),JH(JIN),JK(JIN),
C     .    IRES,IQIN(IREFC),PHSC(IREFC),ZSTAR(JIN)
C20001    FORMAT(5I8,I8,F10.2,F10.5)    
C
1325    CONTINUE
1340  CONTINUE
C
C---------------------------------------------------------------------------
C.....WRITE TABLE OF RESIDUAL AS FUNCTION OF RESOLUTION
C---------------------------------------------------------------------------
C
      WRITE(6,10173)
10173 FORMAT(/,":",5X,'BEST PHASE RESIDUAL IN RESOLUTION RANGES'/
     .   5X,'BETWEEN NEW FILM AND OTHER FILMS (NO TREATMENT OF SPECIAL',
     .   ' REFLECTIONS)',/)
      WRITE(6,10171)
10171 FORMAT(":",5X,' RANGE','     DMIN ','     DMAX ','   RESIDUAL',
     .   '  NUMBER',/)
      NRESALL=0
      SERRESALL=0.0
      DO 10175 I=1,NSLOTS
        IF(NRESO(I).EQ.0)GO TO 10175
        NRESALL=NRESALL+NRESO(I)
        SERRESALL=SERRES(I)+SERRESALL
        ERRES(I)=SERRES(I)/NRESO(I)
        DMIN=SQRT(10000.0/((I-1)*IRESTEP + 1))
        DMAX=SQRT(10000.0/(I*IRESTEP))
        WRITE(6,10172)I,DMIN,DMAX,ERRES(I),NRESO(I)
10175 CONTINUE
10172 FORMAT(":",5X,I6,3F10.3,I7)
      ERRESALL = 0.0
      IF(NRESALL.NE.0)THEN
        ERRESALL=SERRESALL/NRESALL
      ENDIF
      WRITE(6,10174)IFILM,ERRESALL,NRESALL
10174 FORMAT(/,/,":",5X,'OVERALL (',I10,')',10X,F10.3,I7,/,/)
C
C
CHEN>
      write(6,'('' Cross-Correlation Map for this image'',/,
     .          '' ===================================='',/)')
CHEN<
      istep=(IBOXPHS/120)+1
C      if(IVERBOSE.gt.3)then
        write(6,'('' The following map has a reduction factor of '',
     .      I2,/)')
     1    istep
        do J=1,IBOXPHS,istep
          WRITE(6,175) (IRP(I,J),I=1,IBOXPHS,istep)
        enddo
C      endif
C
C---------------------------------------------------------------------------
C  INSERT HERE FOR BEAMTILT REFINEMENT.
C---------------------------------------------------------------------------
C

      IF (NBEAM) THEN
        PRINT *, "::HELLO FROM BEAMTILT IF!"
        CALL BEAMTILTA
        WRITE(6,355)ORIGH+SHMIN,ORIGK+SKMIN,TLTH+TILTH,TLTK+TILTK
355     FORMAT(' AGGREGATE TOTAL OF PARAMETERS --- INPUT PLUS REFINED'/
     . '                ORIGH ...........',F9.2/
     . '                ORIGK ...........',F9.2/
     . '                TILTH ...........',F9.2/
     . '                TILTK ...........',F9.2/)
        WRITE(6,9355)IFILM,ORIGH+SHMIN,ORIGK+SKMIN,TLTH+TILTH,TLTK+TILTK
9355    FORMAT(' LATEST PARAMS',I10,2X,4F9.2)
C
CHEN-  Prepare for output later.
C
        HSHMIN=SHMIN
        HSKMIN=SKMIN
        HOHRIGSH=ORIGH+SHMIN
        HOKRIGSH=ORIGK+SKMIN
        HTHLT=TLTH+TILTH
        HTKLT=TLTK+TILTK
CHEN
C
      ENDIF
      
C
C------------------------------------------------------------------------
C  INSERT HERE FOR TILTANGLE/AXIS REFINEMENT.
C------------------------------------------------------------------------
C
      IF(NPROG.GE.2)THEN
        DO 360 IN=1,IN1
          P=PHSI(IN)+PHSHFT(IIH(IN),IIK(IN),SHMIN,SKMIN,
     .       TILTH,TILTK,BEAMSHFT,BSH(IN))
360     PTEMP(IN) = AMOD(P,360.)
C
        if(NTILT)then
C
          ROTAXA=TAXA
          ROTANGL=TANGL
          RTAXAOPT=TAXA
          RTANGLOPT=TANGL
C
          PHSMIN = 999999.99
C
          do ITAXA=-itaxastep,itaxastep
            do ITANGL=-itanglstep,itanglstep
C
              TAXA=ROTAXA+ITAXA*rtaxasize
              TANGL=ROTANGL+ITANGL*rtanglsize
C
              R1TAXA = TAXA
              R1TANGL = TANGL
              if(IVERBOSE.gt.1)then
                write(*,'(/,''Entering TILTP2 with test values:'')')
                write(*,'('' TAXA = '',F12.3,'' TANGL = '',F12.3)')
     .              TAXA,TANGL
                write(*,'('' ITAXA = '',I5,''   ITANGL = '',I5)')
     .              ITAXA,ITANGL
                write(*,'(/)')
              endif
C
              CALL TILTP2(IN1,IHIN,IKIN,IQIN,AIN,
     .         PTEMP,AMP,JREFL,JH,JK,ZSTAR,
     .         IHC,IKC,ILC,ISC,IFCC,IPHC,IBEGIN,IFINISH,
     .         TAXA,ABANG,TANGL,ASTAR,BSTAR,IFINSH,
     .         MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     .         MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     .         MAT(1,IMAT(5,ISPGRP)),
     .         IGO(1,ISPGRP),ISPEC(1,ISPGRP),CELL(3),
     .         REVHK,SGNXCH,ROT180,IQMAX,LREV(ISPGRP),ROT90,
     .          IAQP2,REVHND,PHRESID,RMSRESID,IVERBOSE)
C
              if(IVERBOSE.gt.1)then
                write(*,'(/,''***************************************'',
     .              ''**************************************'')')
                write(*,'('' This resulted in PHRESID, RMSRESID = '',
     .                  2F15.3)')PHRESID,RMSRESID
               write(*,'('' Test values  : TAXA = '',F12.3,'' TANGL = ''
     .                 ,F12.3)')R1TAXA,R1TANGL
               write(*,'('' Result values: TAXA = '',F12.3,'' TANGL = ''
     .                 ,F12.3,''  Residual = '',F15.3)')
     1            TAXA,TANGL,PHRESID
               write(*,'('' So far best  : TAXA = '',F12.3,'' TANGL = ''
     .                ,F12.3,''  Residual = '',F15.3)')
     1            RTAXAOPT,RTANGLOPT,PHSMIN
                write(*,'(/,''***************************************'',
     .              ''**************************************'')')
                write(*,'(/)')
              endif
C
              if(PHRESID.lt.PHSMIN)then
                PHSMIN=PHRESID
                RTAXAOPT=TAXA
                RTANGLOPT=TANGL
                if(IVERBOSE.gt.1)then
                 write(*,'(/,'' This is now the new record setter.'',/)')
                endif
              endif
C
            enddo
          enddo
C
          TAXA=MOD(RTAXAOPT,360.0)
          if(TAXA.gt.180.0)then
            TAXA=TAXA-360.0
          endif
          TANGL=RTANGLOPT
          write(*,'(/,/,'' Best TAXA and TANGL found with PHSMIN = ''
     .         ,F12.3)')PHSMIN
          write(*,'('' TAXA = '',F12.3,/,'' TANGL = '',F12.3)')
     .         TAXA,TANGL
          if(IVERBOSE.gt.1)then
            write(*,'(/,/,/)')
          endif
C
        endif
C
        ICALL=ICALL+1
C
      ELSE
       IF(NTILT) WRITE(6,401)
401      FORMAT(//'  WARNING !!! - crystal tilt cannot be refined',
     .    ' unless 3D-data with NPROG = 2 or 3 is available'//)
      ENDIF
C
      IF (IFINSH.EQ.1.OR.ICALL.GT.4.OR.NPROG.LT.2.) GO TO 400
      IF (.NOT.NTILT) GO TO 400
C
C------------------------------------------------------------------------
      GO TO 270         ! BACK TO START TO REDO SCALING, ORIGIN AND TILT.
C------------------------------------------------------------------------
C
400   continue
C
      if(TAXA.gt.90.0)then
        TAXA=TAXA-180.0
        TANGL=-TANGL
      endif
      if(TAXA.gt.90.0)then
        TAXA=TAXA-180.0
        TANGL=-TANGL
      endif
      if(TAXA.lt.-90.0)then
        TAXA=TAXA+180.0
        TANGL=-TANGL
      endif
      if(TAXA.lt.-90.0)then
        TAXA=TAXA+180.0
        TANGL=-TANGL
      endif
C
      if(abs(ASUMI).lt.0.001)ASUMI=0.001
      IF(SCALE.LT.0.0001)SCALE=ASUM/ASUMI       ! JUMP HERE IF STEP = 0.
      WRITE(6,170) SCALE,NCOMPI,NCOMP
C
CHEN>
C
C------------------------------------------------------------------------
C     Update RESULTS FILE
C------------------------------------------------------------------------
C
C-----In a FILIN like "../ML-00/ML0012345601/APH/corML0012345601.aph" the string
C-----                "../ML-00/ML0012345601" needs to be found:

      write(CTMP,'(''/'')')
      ihen1=len(FILIN)-1
      ihen2=0
411   continue
        ihen1=ihen1-1
        if((FILIN(ihen1:ihen1).ne.CTMP).and.(ihen1.gt.2)) goto 411
        ihen2=ihen2+1
        if(ihen2.le.1) goto 411
      ihen1=ihen1-1
C
      if(.not.LOGOUTPUT)goto 414
        call system("\rm -f tmp.1")
        write(cline1,'(''cd '',A,''; pwd > tmp.1'')')FILIN(1:ihen1)
        call shorten(cline1,k)
C        write(6,'(''Calling system with '',A)')cline1(1:k)
        call system(cline1(1:k))
        write(cline2,'(A,''/tmp.1'')')FILIN(1:ihen1)
        call shorten(cline2,k2)
        write(cline3,'(''\rm -f '',A,''/tmp.1'')')FILIN(1:ihen1)
        call shorten(cline3,k3)
        open(23,FILE=cline2(1:k2),ERR=413)
        read(23,'(A)')FILIN
        call shorten(FILIN,ihen1)
        close(23)
        call system(cline3(1:k3))
        goto 414
 413    continue
          write(6,'(''::ERROR: in 2dx_origtiltk.exe: '',
     .       ''Could not open tmp.1'')')
 414  continue
C
      if (HOHRIGSH.LT.-180.0) HOHRIGSH=HOHRIGSH+360.0
      if (HOHRIGSH.GT. 180.0) HOHRIGSH=HOHRIGSH-360.0
      if (HOKRIGSH.LT.-180.0) HOKRIGSH=HOKRIGSH+360.0
      if (HOKRIGSH.GT. 180.0) HOKRIGSH=HOKRIGSH-360.0
C
      if (HTHLT.LT.-180.0) HTHLT=HTHLT+360.0
      if (HTHLT.GT. 180.0) HTHLT=HTHLT-360.0
      if (HTKLT.LT.-180.0) HTKLT=HTKLT+360.0
      if (HTKLT.GT. 180.0) HTKLT=HTKLT-360.0
C
      if ( NBEAM ) then
        HSHTHLT=HTHLT-HORITLTH
        HSHTKLT=HTKLT-HORITLTK
      else
        HSHTHLT = 0.0
        HSHTKLT = 0.0
      endif
C
      HSHTAXA=TAXA-HORITAXA
      HSHTANGL=TANGL-HORITANGL
C
      if(LOGOUTPUT)then
        if(USEPYTHON)then
          if(PYTHONFIRST)then
            write(17,'(''# This is a Python script. '')')
            write(17,'(''# This updates the 2dx_image.cfg files'')')
            write(17,'(''#'')')
            write(17,'(''import os'')')
            PYTHONFIRST = .FALSE.
          endif
          write(17,'('' '')')
          write(17,'('' '')')
          write(17,'(''print ":Updating 2dx_image.cfg in '',A,''"'')')FILIN(1:ihen1)
C
          write(cline1,'(2F12.3)')HSHMIN,HSKMIN
          call inkomma(cline1,k1)
          write(cline2,'(F9.3)')HSHTAXA
          call shortshrink(cline2,k2)
          write(cline3,'(F9.3)')HSHTANGL
          call shortshrink(cline3,k3)
          write(17,'(''print ":PhoriChange='',A,'', TAXA-Change='',A,
     .     '', TANGL-Change='',A,
     .     ''"'')')cline1(1:k1),cline2(1:k2),cline3(1:k3)
C
          write(17,415)FILIN(1:ihen1)
 415      FORMAT('f = open("',A,'/2dx_image.cfg",''r'')')
          write(17,'(''lines = []'')')
          write(17,'(''for l in f:'')')
        else
          write(17,'(''<IMAGEDIR="'',A,''">'')')FILIN(1:ihen1)
        endif
      endif
      write(6,'(/,/,/,'' Image Directory is '',A,/)')FILIN(1:ihen1)
C
      write(cline1,'(F15.2)')ERRMIN
      write(cline1,'(F15.2)')ERRESALL
      call shortshrink(cline1,k)
C      write(6,'(''new MergePhaseResidual = "'',A,''"'')')cline1(1:k)
      if(LOGOUTPUT)then
        if(USEPYTHON)then
          write(17,'(T8,''if l.startswith("set MergePhaseResidual "'',
     .      ''):'')')
          write(17,'(T16,''lines.append(''''set MergePhaseResidual = "'',
     .      A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MergePhaseResidual = "'',A,''"'')')cline1(1:k)
        endif
      endif
C
C10171 FORMAT(":",5X,' RANGE','     DMIN ','     DMAX ','   RESIDUAL  NUMBER',/)
C      NRESALL=0
C      SERRESALL=0.0
C      DO 10175 I=1,NSLOTS
C        IF(NRESO(I).EQ.0)GO TO 10175
C        NRESALL=NRESALL+NRESO(I)
C        SERRESALL=SERRES(I)+SERRESALL
C        ERRES(I)=SERRES(I)/NRESO(I)
C        DMIN=SQRT(10000.0/((I-1)*IRESTEP + 1))
C        DMAX=SQRT(10000.0/(I*IRESTEP))
C        WRITE(6,10172)I,DMIN,DMAX,ERRES(I),NRESO(I)
C10175 CONTINUE
C10172 FORMAT(":",5X,I6,3F10.3,I7)
C      ERRESALL = 0.0
C      IF(NRESALL.NE.0)THEN
C        ERRESALL=SERRESALL/NRESALL
C      ENDIF
C      WRITE(6,10174)IFILM,ERRESALL,NRESALL
C10174 FORMAT(/,/,":",5X,'OVERALL (',I10,')',10X,F10.3,I7,/,/)
C
      do I=1,9
        if(NRESO(I).GT.0)then
          write(cline1,'(F15.2)')ERRES(I)
          write(cline2,'(I10)')NRESO(I)
        else
          write(cline1,'(''99.0'')')
          write(cline2,'(''0'')')
        endif
        call shortshrink(cline1,k1)
        call shortshrink(cline2,k2)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set MergePhaseRes_slot'',I1,'' "):'')')I
            write(17,'(T16,''lines.append('',
     .        ''''''set MergePhaseRes_slot'',I1,'' = "'',A,''"\n'''')'')')I,cline1(1:k1)
            write(17,'(T8,''elif l.startswith('',
     .        ''"set MergePhaseRes_numb'',I1,'' "):'')')I
            write(17,'(T16,''lines.append('',
     .        ''''''set MergePhaseRes_numb'',I1,'' = "'',A,''"\n'''')'')')I,cline2(1:k2)
          else
            write(17,'(''set MergePhaseRes_slot'',I1,'' = "'',A,''"'')')I,cline1(1:k1)
            write(17,'(''set MergePhaseRes_numb'',I1,'' = "'',A,''"'')')I,cline2(1:k2)
          endif
        endif
        write(6,'(''new MergePhaseRes_slot'',I1,'' = "'',A,''"'')')I,cline1(1:k1)
        write(6,'(''new MergePhaseRes_numb'',I1,'' = "'',A,''"'')')I,cline2(1:k2)
      enddo
C
      write(cline1,'(2F12.3)')HOHRIGSH,HOKRIGSH
      call inkomma(cline1,k)
      if(LUSEML)then
        write(CPHAORI,'(''phaori_ML'')')
      else
        write(CPHAORI,'(''phaori'')')
      endif
      call shorten(CPHAORI,k2)
      if(ispcgrp.eq.1)then
        if(LOGOUTPUT) then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set '',A,'' "):'')')CPHAORI(1:k2)
            write(17,'(T16,''lines.append('',
     .        ''''''set '',A,'' = "'',A,''"\n'''')'')')CPHAORI(1:k2),cline1(1:k)
          else
            write(17,'(''set '',A,'' = "'',A,''"'')')CPHAORI(1:k2),cline1(1:k)
          endif
        endif
        write(6,'(''new '',A,'' = "'',A,''"'')')CPHAORI(1:k2),cline1(1:k)
      else
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set '',A,'' "):'')')CPHAORI(1:k2)
            write(17,'(T16,''lines.append('',
     .        ''''''set '',A,'' = "'',A,''"\n'''')'')')CPHAORI(1:k2),cline1(1:k)
            if(.not.LPROTFOUFIL)then
              write(17,'(T8,''elif l.startswith('',
     .          ''"set phaoriFouFilter "):'')')
              write(17,'(T16,''lines.append('',
     .          ''''''set phaoriFouFilter = "'',A,''"\n'''')'')')cline1(1:k)
            endif
          else
            write(17,'(''set '',A,'' = "'',A,''"'')')CPHAORI(1:k2),cline1(1:k)
            if(.not.LPROTFOUFIL)then
              write(17,'(''set phaoriFouFilter = "'',A,''"'')')
     .          cline1(1:k)
            endif
          endif
        endif
        write(6,'(''new '',A,'' = "'',A,''"'')')CPHAORI(1:k2),cline1(1:k)
        if(.not.LPROTFOUFIL)then
          write(6,'(''new phaoriFouFilter = "'',A,''"'')')cline1(1:k)
        endif
      endif
C
      if(LOGOUTPUT) then
        write(cline1,'(2F12.3)')HSHMIN,HSKMIN
        call inkomma(cline1,k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set phaori_last_change "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set phaori_last_change = "'',A,''"\n'''')'')')cline1(1:k)
        else
          WRITE(17,'(''set phaori_last_change = "'',A,''"'')')
     .      cline1(1:k)
        endif
        WRITE(6,'(''Last applied change to phaori = "'',A,''"'')')
     .    cline1(1:k)
       endif
C
C      if(ibeamtiltref.eq.1)then
       if ( NBEAM ) then
        write(cline1,'(2F12.3)')HTHLT,HTKLT
        call inkomma(cline1,k)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set beamtilt "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set beamtilt = "'',A,''"\n'''')'')')cline1(1:k)
          else
            write(17,'(''set beamtilt = "'',A,''"'')')cline1(1:k)
          endif
        endif
        write(6,'(''new beamtilt = "'',A,''"'')')cline1(1:k)
C
        RTMP = SQRT(HTHLT**2+HTKLT**2)
        write(cline1,'(F12.3)')RTMP
        call inkomma(cline1,k)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set beamtilt_magnitude "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set beamtilt_magnitude = "'',A,''"\n'''')'')')cline1(1:k)
          else
            write(17,'(''set beamtilt_magnitude = "'',A,''"'')')cline1(1:k)
          endif
        endif
        write(6,'(''new beamtilt_magnitude = "'',A,''"'')')cline1(1:k)
C
        write(cline1,'(2F12.3)')HSHTHLT,HSHTKLT
        call inkomma(cline1,k)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set beamtilt_change "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set beamtilt_change = "'',A,''"\n'''')'')')cline1(1:k)
          else
            write(17,'(''set beamtilt_change = "'',A,''"'')')cline1(1:k)
          endif
        endif
        write(6,'(''new beamtilt_change = "'',A,''"'')')cline1(1:k)
      endif
C
      if(itiltaxref.eq.1)then
        write(cline1,'(F12.3)')TAXA
        call shortshrink(cline1,k)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set TAXA "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set TAXA = "'',A,''"\n'''')'')')cline1(1:k)
          else
            write(17,'(''set TAXA = "'',A,''"'')')cline1(1:k)
          endif
        endif
        write(6,'(''new TAXA = "'',A,''"'')')cline1(1:k)
C
        write(cline1,'(F12.3)')TANGL
        call shortshrink(cline1,k)
        if(LOGOUTPUT)then
          if(USEPYTHON)then
            write(17,'(T8,''elif l.startswith('',
     .        ''"set TAGNL "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set TANGL = "'',A,''"\n'''')'')')cline1(1:k)
            write(17,'(T8,''elif l.startswith('',
     .        ''"set DEFOCUS_ACTIVE "):'')')
            write(17,'(T16,''lines.append('',
     .        ''''''set DEFOCUS_ACTIVE = "'',A,''"\n'''')'')')cline1(1:k)
          else
            write(17,'(''set TANGL = "'',A,''"'')')cline1(1:k)
            write(17,'(''set DEFOCUS_ACTIVE = 4'')')
          endif
        endif
        write(6,'(''new TANGL = "'',A,''"'')')cline1(1:k)
C
C        write(2,'(''mrctestangle 1'')')
      endif
C
      if(LOGOUTPUT)then
        write(cline1,'(F8.3)')SCALE
        call shortshrink(cline1,k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set MergeScaleFactor "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set MergeScaleFactor = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MergeScaleFactor = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(I5)')NCOMPI
        call shortshrink(cline1,k)
        if(USEPYTHON)then
          write(17,'(''#'',T8,''elif l.startswith('',
     .      ''"set MergeNewSpots "):'')')
          write(17,'(''#'',T16,''lines.append('',
     .      ''''''set MergeNewSpots = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''# set MergeNewSpots = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(I5)')NCOMPT
        call shortshrink(cline1,k)
        if(USEPYTHON)then
          write(17,'(''#'',T8,''elif l.startswith('',
     .      ''"set MergeOldSpots "):'')')
          write(17,'(''#'',T16,''lines.append('',
     .      ''''''set MergeOldSpots = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''# set MergeOldSpots = "'',A,''"'')')cline1(1:k)
        endif
      endif
C
      WRITE(6,174)SCALE,NCOMPI,IFILM,NCOMP
174   FORMAT('Scale factor ',F10.5,' between ',I6,' new (',I10
     .    ,') and ',I9,' old spots.')
C
      if ( ifirst .eq. 0 ) then
        write(*,'('':    Number    Scale  Phase Origin        '',
     .    ''Change Phase Ori.     PhaRes'')')
        write(21,'(''    Number    Scale  Phase Origin        '',
     .    ''Change Phase Ori.     PhaRes'')')
        ifirst = 1
      else
        write(*,'('':    Number    Scale  Phase Origin        '',
     .    ''Change Phase Ori.     PhaRes'')')
        write(21,'(''    Number    Scale  Phase Origin        '',
     .    ''Change Phase Ori.     PhaRes'')')
      endif
C
      write(cline1,'(2F9.3)')HOHRIGSH,HOKRIGSH
      call inkomma(cline1,k1)
C
      write(cline2,'(2F9.3)')HSHMIN,HSKMIN
      call inkomma(cline2,k2)
C
      write(*,'('':'',I10,'' '',F8.3,''  '',A20,A20,F7.2)')
     . IFILM,SCALE,cline1,cline2,ERRMIN
      write(21,'(I10,'' '',F8.3,''  '',A20,A20,F7.2)')
     . IFILM,SCALE,cline1,cline2,ERRMIN
C
C------------------------------------------------------------------------
C     Create output for STATUS FILE
C------------------------------------------------------------------------
C
      DRAD = 3.14159265437 / 180.0
C
C-----Calculate TLTAXIS,TLTANGL backwards from refined TAXA,TANGL:
C
C-----TLTANG is the tilt angle measured from the image.
C-----TANGL  is the tilt angle of the crystal. 
C-----These two have the same magnitude, but their sign may be different.
C
C-----TAXA is defined as TILTAX -> A on crystal, but in direction of A->B
C----- "-TAXA" is defined as A -> TILTAX on crystal, but in direction of A->B
C
C-----TLTAXA is defined as TILTAX -> A on image, but in direction of A->B
C-----   This is related to TAXA by cos(TANGL)
C
C-----ANGA is defined as X -> A on image
C-----ANGB is defined as X -> B on image
C
C-----TLTAXIS is defined as X -> TILTAX on image
C
C
C-----Calculate angles of vectors:
C
      ANGA=ATAN2(rlattu2,rlattu1)/DRAD
      ANGB=ATAN2(rlattv2,rlattv1)/DRAD
C
      if(ANGA.gt. 180.0)ANGA=ANGA-360.0
      if(ANGB.gt. 180.0)ANGB=ANGB-360.0
      if(ANGA.lt.-180.0)ANGA=ANGA+360.0
      if(ANGB.lt.-180.0)ANGB=ANGB+360.0
C
      IF(ABS(ANGB-ANGA).GT.180.0)THEN
        ANGB=ANGB-SIGN(360.0,ANGB-ANGA)
      endif
C-----Angle between A and B is now between -180 and +180 degs.
C
C      write(*,'(/,'' ANGA      = '',2F12.3)')ANGA
C      write(*,'('' ANGB      = '',2F12.3)')ANGB
C
C-----TTREFINE calculates TAXA from TLTAXA in the following way:
C      DENOM = SQRT(1.0-(SIN(TLTAXA*DRAD)*SIN(TLTANGL*DRAD))**2)
C      TAXA = RDEG*ACOS((COS(TLTAXA*DRAD))/DENOM)
C      TAXA = SIGN(TAXA,TLTAXA)
C
C-----This is a mathematical solution:
      if(abs(cos(TANGL*DRAD)).gt.0.001)then
        TLTAXA = ATAN(TAN(TAXA*DRAD) / cos(TANGL*DRAD)) / DRAD
        TLTAXA = SIGN(TLTAXA,TAXA)
C        write(*,'(/,''1 TAXA   = '',F15.9)')TAXA
C        write(*,'(''1 TLTAXA = '',F15.9)')TLTAXA
      endif
C
C-----Here, we invert that numerically:
C
C      TLTAXA = CALTLTAXA(TAXA,TANGL)
C      TLTAXA = SIGN(TLTAXA,TAXA)
C
C-----Both should get to the same result:
C      write(*,'(''2 TLTAXA = '',F15.9)')TLTAXA
C
C-----Determine direction of A->B:
C
      if(ANGB-ANGA.gt.0.0)then
        TLTAXIS = ANGA-TLTAXA
      else
        TLTAXIS = ANGA+TLTAXA
      endif
C
      write(*,'(/,'' TLTAXIS = '',F12.3)')TLTAXIS
C
      if(TLTAXIS.le.-90.0)then
        TLTAXIS=TLTAXIS+180.0
      endif
      if(TLTAXIS.le.-90.0)then
        TLTAXIS=TLTAXIS+180.0
      endif
      if(TLTAXIS.ge. 90.0)then
        TLTAXIS=TLTAXIS-180.0
      endif
      if(TLTAXIS.ge. 90.0)then
        TLTAXIS=TLTAXIS-180.0
      endif
C
      write(*,'('' TLTAXIS = '',F12.3)')TLTAXIS
C
      IF(ANGB-ANGA.GT.0.0) THEN           
        HAND =  1.0
      ELSE
        HAND = -1.0
      ENDIF
C
C
C Now get sign of crystallographic tiltangle.
C  by first calculating whether A_IS_ABOVE the original TLTAXIS on the film.
C
C-----TLTAXIS is between -90.0 and 90.0
C-----ANGA is between -180.0 and 180.0
C
      TLTNORM = TLTAXIS + 90.0      ! TLTNORM now GE.0 and LT.180(see above)
      ANGACOMP= ABS(ANGA-TLTNORM)   ! Should be between 0 and 360
      IF((ANGACOMP.GT.90.0).AND.(ANGACOMP.LT.270.0)) THEN
        AISABOVE = -1.0
      ELSE
        AISABOVE =  1.0
      ENDIF
C
      SIGNTLTAXA = SIGN(1.0,TLTAXA)
C
      TLTANG  = (AISABOVE * SIGNTLTAXA * HAND) * TANGL
C
      write(*,'(/'' AISABOVE   = '',F12.3)')AISABOVE
      write(*,'('' SIGNTLTAXA = '',F12.3)')SIGNTLTAXA
      write(*,'('' HAND       = '',F12.3)')HAND
      write(*,'('' TLTAXIS    = '',F12.3)')TLTAXIS
      write(*,'('' TLTNORM    = '',F12.3)')TLTNORM
      write(*,'('' ANGA       = '',F12.3)')ANGA
      write(*,'('' ANGACOMP   = '',F12.3)')ANGACOMP
      write(*,'('' TLTANG     = '',F12.3)')TLTANG
      write(*,'('' TAXA       = '',F12.3)')TAXA
      write(*,'('' TANGL      = '',F12.3)')TANGL
C
C      write(*,'(''Final TLTAXIS,TLTANG = '',2F12.3)')TLTAXIS,TLTANG
C
      if(LOGOUTPUT)then
C
        write(cline1,'(F9.3)')TLTAXIS
        call shortshrink(cline1,k)
        write(6,'(''new MERGE_TLTAXIS = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set MERGE_TLTAXIS "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set MERGE_TLTAXIS = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MERGE_TLTAXIS = "'',A,''"'')')
     .      cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TLTANG
        call shortshrink(cline1,k)
        write(6,'(''new MERGE_TLTANG  = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set MERGE_TLTANG "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set MERGE_TLTANG = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MERGE_TLTANG  = "'',A,''"'')')
     .      cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')HSHTAXA
        call shortshrink(cline1,k)
        write(6,'(''new TAXA_change    = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TAXA_change "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TAXA_change = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TAXA_change    = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')HSHTANGL
        call shortshrink(cline1,k)
        write(6,'(''new TANGL_change    = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TANGL_change "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TANGL_change = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TANGL_change    = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TAXA
        call shortshrink(cline1,k)
        write(6,'(''new MERGE_TAXA    = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set MERGE_TAXA "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set MERGE_TAXA = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MERGE_TAXA    = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TANGL
        call shortshrink(cline1,k)
        write(6,'(''new MERGE_TANGL   = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set MERGE_TANGL "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set MERGE_TANGL = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set MERGE_TANGL   = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TLTAXIS
        call shortshrink(cline1,k)
        write(6,'(''new TLTAXIS = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TLTAXIS "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TLTAXIS = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TLTAXIS = "'',A,''"'')')
     .      cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TLTANG
        call shortshrink(cline1,k)
        write(6,'(''new TLTANG  = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TLTANG "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TLTANG = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TLTANG  = "'',A,''"'')')
     .      cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TAXA
        call shortshrink(cline1,k)
        write(6,'(''new TAXA    = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TAXA "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TAXA = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TAXA    = "'',A,''"'')')cline1(1:k)
        endif
C
        write(cline1,'(F9.3)')TANGL
        call shortshrink(cline1,k)
        write(6,'(''new TANGL   = "'',A,''"'')')cline1(1:k)
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set TANGL "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set TANGL = "'',A,''"\n'''')'')')cline1(1:k)
        else
          write(17,'(''set TANGL   = "'',A,''"'')')cline1(1:k)
        endif
C
        if(USEPYTHON)then
          write(17,'(T8,''elif l.startswith('',
     .      ''"set PHASEORI_done "):'')')
          write(17,'(T16,''lines.append('',
     .      ''''''set PHASEORI_done = "y"\n'''')'')')
        else
          write(17,'(''set PHASEORI_done = "y"'')')
        endif
C
        if(USEPYTHON)then
          write(17,'(T8,''else:'')')
          write(17,'(T16,''lines.append(l)'')')
          write(17,'(''f.close()'')')
          write(17,416)FILIN(1:ihen1)
 416      FORMAT('fout = open("',A,'/2dx_image.cfg",''w'')')
          write(17,'(''fout.writelines(lines)'')')
          write(17,'(''fout.close()'')')
        else
          write(17,'(''</IMAGEDIR>'')')
          write(17,'(''#'')')
C
          write(6,'('' '')')
        endif
      endif
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
CHEN<
C
      IF(LIST) WRITE(6,137)
C
C-----------------------------------------------------------------------
C  Here to create new file if output of origin shifted data is needed.
C-----------------------------------------------------------------------
C
      IF(NSHFTIN) THEN
        write(FNAME,32)IFILM
32      FORMAT('PRJ/HKLAPH',I10,'.PRJ')
        do i=11,19
          if(FNAME(i:i).eq.' ')FNAME(i:i)='0'
        enddo
        OPEN(UNIT=2,FILE=FNAME,FORM='FORMATTED',STATUS='NEW')
        WRITE(2,33)IFILM,TITLE
33      FORMAT(I10,10A4,'  Origin shifted APH data')
      ENDIF
C
C  APPLY SCALE FACTOR AND ORIGIN PHASE SHIFT.
      DO 410  IN=1,IN1
        JREFL=JREFL+1           ! HERE IS WHERE JREFL IS INCREMENTED.
C                       JH,JK,ZSTAR done previously.
        JOUT(JREFL)=JREFL
        PHS(JREFL)=0.0
        AMP(JREFL)=AIN(IN)*SCALE
        BACK(JREFL)=BIN(IN)*SCALE
        CTFS(JREFL)=CTFIN(IN)
        IF(AMP(JREFL).GT.AMAX) AMAX=AMP(JREFL)
        JFILM(JREFL)=IFILM
C       IF(AIN(IN).LT.0.001) GO TO 405
        P=PHSI(IN)+PHSHFT(IIH(IN),IIK(IN),SHMIN,SKMIN,
     .   TILTH,TILTK,BEAMSHFT,BSH(IN))
294     IF(NSHFTIN) THEN                ! output of origin shifted data
          IF(AIN(IN).GT.0.0) THEN
            POUT=AMOD(P,360.)
            IF(POUT.LT.-180.0)POUT=POUT+360.0
            IF(POUT.GT.180.0) POUT=POUT-360.0
            IF(IIK(IN).GE.0) THEN
              WRITE(2,293) IIH(IN),IIK(IN),IZERO,AIN(IN),POUT
            ELSE               ! make K positive in p1, and change phase.
              WRITE(2,293) -IIH(IN),-IIK(IN),IZERO,AIN(IN),-POUT
            ENDIF
          ENDIF
        ENDIF
C
        P=P*IP1(IN)+IP2(IN)
C
C------------------------------------------------------------------------
C       NEW REFLECTIONS ARE TRANSFORMED TO THE UNIQUE ASYMMETRIC UNIT AFTER
C       THE BEST PHASE SHIFT HAS BEEN APPLIED
C------------------------------------------------------------------------
C
        P=AMOD(P,360.)
        IF(P.LT.-180.0)P=P+360.0
        IF(P.GT.180.0) P=P-360.0
        PHS(JREFL)=P
        P=DRAD*P
        CPHS(JREFL)=COS(P)
        SPHS(JREFL)=SIN(P)
405     IF(LIST)WRITE(6,145)JH(JREFL),JK(JREFL),ZSTAR(JREFL),
     .    PHS(JREFL),AMP(JREFL),JSIGN(JREFL)
410   CONTINUE
C
C     Before returning for another read, close file.
C
      CLOSE (UNIT=NIN)
      IF(NSHFTIN) CLOSE(2)      ! close unit 2 for 
C
C------------------------------------------------------------------------
C  Output of summary file for use in updating .inf file, if used.
C------------------------------------------------------------------------
C
      IF(NTILT.OR.NBEAM.OR.STEP.NE.0.0) THEN
        CALL FDATE(DAT)
        WRITE(9,502)IFILM,TITLE,FILIN
C
        IF(NTILT) WRITE(9,503) TAXA,TANGL,DAT(5:24)
C
        IF(NBEAM) THEN
          WRITE(9,505) TLTH+TILTH, TLTK+TILTK, DAT(5:24)
          WRITE(9,504) ORIGH+SHMIN, ORIGK+SKMIN, DAT(5:24)
C
          HOHRIGSH=ORIGH+SHMIN
          HOKRIGSH=ORIGK+SKMIN
C
        ELSE
          IF(STEP.NE.0.0) WRITE(9,504) ORIGHNEW,ORIGKNEW,DAT(5:24)
C
          HOHRIGSH=ORIGH+SHMIN
          HOKRIGSH=ORIGK+SKMIN
C
        ENDIF
        WRITE(9,506)
      ENDIF
502   FORMAT(' ORIGTILT: film ',I10,10A4/'  taken from input file ',A)
503   FORMAT(' TAXATANGL: ',2F8.3,' DATE ',A20)
504   FORMAT(' ORIGHORIGK:',2F9.2,' DATE ',A20)
505   FORMAT(' TILTHTILTK:',2F9.2,' DATE ',A20)
506   FORMAT(/)
C
      GO TO 220         ! BACK TO DO INPUT FOR ANOTHER FILM.
500   WRITE(6,125)
      CLOSE(UNIT=9)
      IF(NPROG.EQ.0) GO TO 501     ! STOP HERE FOR NPROG 1, 2 OR 3.(REFINE ONLY)
      WRITE(6,181)
      GO TO 1107
501   WRITE(6,180)
C
C------------------------------------------------------------------------
C     SORT REFLECTIONS FOR OUTPUT -- BETTER TO USE SHLSRT IF IT GETS SLOW.
C------------------------------------------------------------------------
C
C     REFLECTIONS ARE SORTED INTO ASCENDING ORDER IN H,K AND ZSTAR
C
C      WRITE(6,156) JREFL
C      IF(NPROG.EQ.0) WRITE(3,146)IRUN  ! To ensure disk is there before sorting.
C
C      DO 530 I=1,10
C        NCHNG=0
C        JREFL1=JREFL-1
C        DO 510 IA=1,JREFL1
C          JA=IA+1
C          DO 510 IB=JA,JREFL
C          KA=JOUT(IA)
C          KB=JOUT(IB)
C          IF(KA.LT.1.OR.KA.GT.TOTRFL.OR.KB.LT.1.OR.KB.GT.TOTRFL) THEN
C            WRITE(*,*) 'KA or KB out of bounds',KA,KB
C            STOP
C          ENDIF
C          IF(JH(KA)-JH(KB)) 510,520,525
C525       J1=JOUT(IA)
C          JOUT(IA)=JOUT(IB)
C          JOUT(IB)=J1
C          NCHNG=NCHNG+1
C          GO TO 510
C520       IF(JK(KA)-JK(KB)) 510,515,525
C515       IF(ZSTAR(KA)-ZSTAR(KB)) 510,510,525
C510     CONTINUE
C        IF(NCHNG.EQ.0) GO TO 550
C530   CONTINUE
CC
C      WRITE(6,182) NCHNG
C
C     THIS STATEMENT SHOULD NEVER BE REACHED
C550   WRITE(6,185)
C
C
C------------------------------------------------------------------------
C     Quicksort 
C------------------------------------------------------------------------
C
C
      WRITE(6,156) JREFL
C
CHEN>
      call system("\rm -f SCRATCH/2dx_origtiltk_jrefl.txt")
      open(27,FILE="SCRATCH/2dx_origtiltk_jrefl.txt",STATUS="NEW",ERR=767)
      goto 768
767   continue
        write(6,'(''::ERROR on file open in 2dx_origtiltk.for for'',
     .  '' SCRATCH/2dx_origtiltk_jrefl.txt'')')
        goto 769
768   continue
      write(27,'(I16)')JREFL
      close(27)
769   continue
C
CHEN<
C
      IF(NPROG.EQ.0) WRITE(3,146)IRUN  ! To ensure disk is there before sorting.
CMARC>
C        WRITE(3,186)
CMARC<
        jstack=0
        l=1
        ir=JREFL
C-----------Insertion sort when subarray small enough.---------------------------------
771             if(ir-l.lt.M)then 
                do 7712 j=l+1,ir
                        a=JOUT(j)
                        do 7711 i=j-1,l,-1
                                if(icompare(JH(a),JK(a),ZSTAR(a),JH(JOUT(i)),
     .                             JK(JOUT(i)),ZSTAR(JOUT(i)))) goto 772
                                JOUT(i+1)=JOUT(i)
7711                    enddo 
                        i=l-1
772                     JOUT(i+1)=a
7712            enddo 
                if(jstack.eq.0) goto 7777
C               Pop stack and begin a new round of partitioning.
                ir=istack(jstack) 
                l=istack(jstack-1)
                jstack=jstack-2
C-----------quicksort subarray to big.---------------------------------
        else
                k=(l+ir)/2
C               Choose median of left, center, and right elements as 
C               partitioning
C               element a. Also rearrange so that a(l) <= a(l+1) <= a(ir).
                temp=JOUT(k)
                JOUT(k)=JOUT(l+1)
                JOUT(l+1)=temp
                if(icompare(JH(JOUT(l)),JK(JOUT(l)),ZSTAR(JOUT(l)),JH(JOUT(ir))
     .                    ,JK(JOUT(ir)),ZSTAR(JOUT(ir)))) then
                        temp=JOUT(l)
                        JOUT(l)=JOUT(ir)
                        JOUT(ir)=temp
                endif
                if(icompare(JH(JOUT(l+1)),JK(JOUT(l+1)),ZSTAR(JOUT(l+1))
     .               ,JH(JOUT(ir)),JK(JOUT(ir)),ZSTAR(JOUT(ir)))) then
                        temp=JOUT(l+1)
                        JOUT(l+1)=JOUT(ir)
                        JOUT(ir)=temp
                endif
                if(icompare(JH(JOUT(l)),JK(JOUT(l)),ZSTAR(JOUT(l)),
     .               JH(JOUT(l+1)),JK(JOUT(l+1)),ZSTAR(JOUT(l+1)))) then
                        temp=JOUT(l)
                        JOUT(l)=JOUT(l+1)
                        JOUT(l+1)=temp
                endif
C               Initialize pointers for partitioning.
                i=l+1 
                j=ir
C                       Partitioning element.
                a=JOUT(l+1)
C               Beginning of innermost loop.
773                     continue 
                        i=i+1 
                if(icompare(JH(a),JK(a),ZSTAR(a),JH(JOUT(i)),JK(JOUT(i)),
     .              ZSTAR(JOUT(i)))) goto 773
774                     continue
                        j=j-1
                if(icompare(JH(JOUT(j)),JK(JOUT(j)),ZSTAR(JOUT(j)),JH(a),
     .               JK(a),ZSTAR(a))) goto 774
C               Pointers crossed. Exit with partitioning complete.
                if(j.lt.i)goto 775 
C               Exchange elements.
                temp=JOUT(i) 
                JOUT(i)=JOUT(j)
                JOUT(j)=temp
C                       End of innermost loop.
                goto 773 
C                       Insert partitioning element.
775                     JOUT(l+1)=JOUT(j) 
                JOUT(j)=a
                jstack=jstack+2
C               Push pointers to larger subarray on stack, process smaller subarray =
C               immediately.
                if(jstack.gt.NSTACK)pause 'NSTACK too small in sort'
                if(ir-i+1.ge.j-l)then
                        istack(jstack)=ir
                        istack(jstack-1)=i
                        ir=j-1
                else
                        istack(jstack)=j-1
                        istack(jstack-1)=l
                        l=i
                endif
        endif
        goto 771
7777            continue
C
C
C------------------------------------------------------------------------
C     THE SORTED REFLECTIONS ARE OUTPUT IN ASCENDING ORDER
C------------------------------------------------------------------------
C
C
      LH=-1000
      LK=-1000
      IREFL=1
      DO 600 KK=1,JREFL
        KJ=JOUT(KK)
        IF(IPLOT.EQ.0) GO TO 562
        IF(AMP(KJ).LT.0.001) GO TO 560  ! NO OUTPUT OF REFNS WITH ZERO AMPL.
          PZ(IREFL)=ZSTAR(KJ)
          PAMP(IREFL)=AMP(KJ)
          PPHS(IREFL)=PHS(KJ)
          IPSGN(IREFL)=JSIGN(KJ)
          IREFL=IREFL+1
          IF(IREFL.GT.MAXPLT) THEN
            WRITE(*,*) ' H,K,IREFL,MAXPLT',JH(KJ),JK(KJ),IREFL,MAXPLT
            STOP 'MAXPLT on above lattice line too small'
          ENDIF
560     CONTINUE
C
        IF(KK.EQ.1) GO TO 562
        IF(LH.EQ.JH(KJ).AND.LK.EQ.JK(KJ).AND.KK.NE.JREFL) GO TO 564
          IREF1=IREFL
          IF(KK.NE.JREFL) IREF1=IREFL-1
          IREF2=IREF1-1
          IF(IREF2.GE.MINRFL) GO TO 563
            WRITE(18,188) LH,LK
          GO TO 565
563         IF (LH.LT.LHMIN) GO TO 565
C-----------IF (LH.GT.LHMAX) GO TO 600
            IF (LH.GT.LHMAX) THEN
              WRITE(6,*)' Sorted reflections with H > ',
     .         'LHMAX not printed'
              GOTO 1107
            ENDIF
            CALL GRAPH(ZMIN,ZMAX,AMAX,LH,LK,IREF2,PZ,PAMP,PPHS,IPSGN)
  565     PZ(1)=PZ(IREF1)
          PAMP(1)=PAMP(IREF1)
          PPHS(1)=PPHS(IREF1)
          IPSGN(1)=IPSGN(IREF1)
          IREFL=2
562       IF(LH.NE.JH(KJ).OR.LK.NE.JK(KJ)) then
          DSTARSQ=(JH(KJ)*ASTAR)**2+
     .     2*JH(KJ)*JK(KJ)*ASTAR*BSTAR*COS(DRAD*ABANG)+
     .     (JK(KJ)*BSTAR)**2+ZSTAR(KJ)**2
          RESO=1.0/SQRT(DSTARSQ)
            WRITE(18,187) RESO
          endif
          LH=JH(KJ)
          LK=JK(KJ)
564     IF(AMP(KJ).LT.0.001) GO TO 555
C
CHEN
C
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C=======Here the output file is prepared:
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C
        write(CZEIL,'('' -----------------------'')')
        ILEN = 20 - ABS(JSIGN(KJ) * 2)
        write(CZEIL(ILEN:50),'(I1)') ABS(JSIGN(KJ))
        ILEN = ILEN + 3
        if(ABS(JSIGN(KJ)).LE.5)then
          ILEN = 20
          write(CZEIL(ILEN:50),'(F10.1,F7.1)') AMP(KJ),PHS(KJ)
          ILEN = ILEN + 17
        endif
        WRITE(18,192) LH,LK,ZSTAR(KJ),AMP(KJ),PHS(KJ),
     .     JFILM(KJ),JSIGN(KJ),FLMWGT(KJ),BACK(KJ),CTFS(KJ),
     .     CZEIL(1:ILEN)
C
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
C==========================================================================
CHEN
C
        WRITE(3,190) LH,LK,ZSTAR(KJ),AMP(KJ),PHS(KJ),
     .    JFILM(KJ),JSIGN(KJ),FLMWGT(KJ),BACK(KJ),CTFS(KJ)
        GO TO 600
  555   WRITE(18,191) LH,LK,ZSTAR(KJ),AMP(KJ),
     .     JFILM(KJ),JSIGN(KJ),
     .   FLMWGT(KJ),BACK(KJ),CTFS(KJ)
600   CONTINUE
      GO TO 1107
601   WRITE(6,149) TOTRFL
      CLOSE(UNIT=17)
      CLOSE(UNIT=18)
      CLOSE(UNIT=21)
      STOP
C
602   WRITE(6,151)IFILM,ISER
      CLOSE(UNIT=17)
      STOP
1150  WRITE(6,1106)NMAXC
1106  FORMAT(' REFERENCE PHASE DATA TOO BIG FOR PROGRAM DIMENSION',I6)
1107  CONTINUE
      CLOSE(UNIT=17)
      END
C
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
      FUNCTION icompare(H1,K1,Z1,H2,K2,Z2)
                LOGICAL icompare
                INTEGER*4 H1,K1,H2,K2
                REAL Z1,Z2
        IF(H1-H2) 77510,77520,77525
77520       IF(K1-K2) 77510,77515,77525
77515       IF(Z1-Z2) 77510,77510,77525
77510           icompare=.FALSE.
                goto 77530
77525           icompare=.TRUE.
77530         continue
C               write(6,77777)H1,K1,Z1,H2,K2,Z2,icompare
C77777          format('icompare h1:',I6,' K1:',I6,' Z1:',F6.4,' H2:',I6,' K2:',I6,' Z2:',F6.4,' L:',L1)
                return 
      end
C*******************************************************************************
      SUBROUTINE ASYM(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
     1  A1,A2,A3,A4,A5,IGO,ISPEC,LREV,IAQP2,IVERBOSE)
C
      INTEGER*4 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      LOGICAL SPEC,LREV
C
C Space Group p2221 (7):
C==========================
C          7       P2221         H,K,Z>=0          (0,2N,Z)  (0,2N+1,Z)
C                                                  (H,K,0)
C                                                  (H,0,Z)
C
C Asymetric unit for p2221 is 
C       H,K,Z >= 0
C       if H=0: K>= 0
C
C IGO = 2,4,2,5,2,4,2,5,
C ISPEC = 1,1,1,0,-1,
C IMAT = 1,2,1,4,1
C LREV = .FALSE.
C
C     CALL ASYM(IH,IK,Z,IP1(IN),IP2(IN),LSPEC(IN),IPTEST(IN),
C    1  WSTAR,MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
C    2  MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
C    3  MAT(1,IMAT(5,ISPGRP)),
C    4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2,IVERBOSE)
C
C     CALL ASYM(IH,IK,Z,IP1(IN),IP2(IN),LSPEC(IN),IPTEST(IN),
C    1  WSTAR,MAT(1,1),MAT(1,2),
C    2  MAT(1,1),MAT(1,4),
C    3  MAT(1,1),
C    4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2,IVERBOSE)
C
C       A1 =  -1,0,0,-1,-1,  0,  0,-1
C       A2 =   1,0,0, 1,-1,  0,  0,-1
C       A3 =  -1,0,0,-1,-1,  0,  0,-1
C       A4 =   1,0,0,-1, 1,  0,180,-1
C       A5 =  -1,0,0,-1,-1,  0,  0,-1
C
C.........IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C.........IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C.........REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C.........SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C.........WILL BE PERFORMED IN P1.
C.........
C.........LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C.........SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C.........IF IT SHOULD BE IMAGINARY
C
C
C      WRITE(6,904)A1,A2,A3,A4,A5,IGO,ISPEC,IH,IK,Z,IP1,IP2,SPEC,LREV,IPTEST,WSTAR
C 904  FORMAT(/,'ASYM called with',/,
C     .  ' A1   =',8I5,/,' A2   ='8I5,/,' A3   =',8I5,/,
C     1  ' A4   =',8I5,/,' A5   =',8I5,/,
C     3  ' IGO  =',8I5,/,
C     3  ' ISPEC=',5I5,/,
C     2  50X,'IH,IK,Z      =',2I9,F9.3,/,
C     3  50X,'IP1,IP2      =',2I9,/,
C     3  50X,'SPEC,LREV    =',2L,/,
C     4  50X,'IPTEST,WSTAR =',I9,F9.3)
C
      IF(IH.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
C
C-----MULT does matrix multiplication as:
C     (H' K' Z' AMP' PHS')=(H K Z AMP PHI) <A>
C        <A> HAS FORM     IA(1)  IA(3)     0      0  IA(6)
C                         IA(2)  IA(4)     0      0  IA(7)
C                             0      0 IA(5)      0      0
C                             0      0     0      1      0
C                             0      0     0      0  IA(8)
C
C-----This effects:
C     H=-H
C     K=-K
C     Z=-Z
C     IP1=IP1  
C     IP2=-IP2  
C
      PASS=0    ! second pass (22.1.90) to check all changes made correctly.
50    INDEX=1
      IF(IK.GE.0) INDEX=INDEX+1
      IF(Z.GE.0.0) INDEX=INDEX+2
      IF(IH.LT.IABS(IK)) INDEX=INDEX+4
C
      INDEX=IGO(INDEX)
C
C      WRITE(6,902) INDEX
902   FORMAT (' INDEX = ',I10)
C      WRITE(6,901) IH,IK,Z,IP1,IP2
901   FORMAT(' IH,IK,Z,IP1,IP2 = ',2I5,F10.5,2I5)
      GO TO (100,150,200,250,500), INDEX
C
C     INDEX CLASSIFIES THE REFLECTION BY ITS INDICES
C     IGO INDICATES WHICH MATRIX WILL BRING THE REFLECTION
C        INTO THE UNIQUE ASYMMETRIC UNIT FOR A GIVEN INDEX
C
C    INDEX    K>=0     Z>=0   /K/>=/H/          IGO     GOTO    
C      1       NO       NO      NO              = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      2       YES      NO      NO              = 4     -> 250  => MULT(A2)     H=H,K=K,Z=-Z, IP1=IP1,     IP2=-IP2
C      3       NO       YES     NO              = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      4       YES      YES     NO              = 5     -> 500
C      5       NO       NO      YES             = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      6       YES      NO      YES             = 4     -> 250  => MULT(A2)     H=H,K=K,Z=-Z, IP1=IP1,     IP2=-IP2
C      7       NO       YES     YES             = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      8       YES      YES     YES             = 5     -> 500
C
C      P622 IS THE HIGHEST SYMMETRY AND ITS ASYMMETRIC UNIT IS ONLY
C         INDEX = 8
C
100    CALL MULT(A5,IH,IK,Z,IP1,IP2)
C       WRITE(6,900) A5,IH,IK,Z,IP1,IP2
900    FORMAT(8I5,5X,2I5,F10.5,2I10)
       GO TO 50
150    CALL MULT(A4,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A4,IH,IK,Z,IP1,IP2
       GO TO 50
200    CALL MULT(A3,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A3,IH,IK,Z,IP1,IP2
       GO TO 50
250    CALL MULT(A2,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A2,IH,IK,Z,IP1,IP2
C
C      AFTER REFLECTIONS HAVE BEEN PLACED INTO THE ASYMMETRIC UNIT
C       THEY ARE EXAMINED TO SEE IF THEY ARE SPECIAL REFLECTIONS,
C       ONES WHOSE PHASE MUST BE EITHER REAL (0 OR PI) OR IMAGINARY
C       (PI/2 OR 3*PI/2)
C
500    CONTINUE
C
C------If H=0, then it should be: K>0: Do H=-H, K=-K, Z=-Z, IP1=IP1, IP2=-IP2:
       IF(IH.EQ.0 .AND. IK.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
C
C------LREV=T and H=0 : Do H=H, K=-K, Z=Z, IP1=IP1+180, IP2=-IP2
       IF(LREV .AND. IH.EQ.0) CALL MULT(A4,IH,IK,Z,IP1,IP2)

      PASS=PASS+1                       !
      IF(PASS.EQ.1) GO TO 50            ! Check through again once only.
C                                       ! Done 22.1.90 to fix the -ve zstar
C                                       ! obtained for h=0,k=-ve refls in p2.
C
C------Now, reflections should be in the asymetric unit.
C
C       write(6,'('' In asymmetric unit: '',2I6,F9.3,2I6)')IH,IK,Z,IP1,IP2
C
       SPEC=.FALSE.
       IPTEST=0
C      SPEC WILL BE TRUE IF THE REFLECTION IS SPECIAL .
C      IPTEST WILL BE 0 IF REAL AND 90 IF IMAGINARY
C      ISPEC INDICATES THE CONDITIONS FOR THE REFLECTIONS
C          ISPEC(1)=1  H=0 SPECIAL
C          ISPEC(2)=1  K=0 SPECIAL
C          ISPEC(3)=1  Z=0 SPECIAL
C          ISPEC(4)=1  H=K SPECIAL
C          ISPEC(5)=1  IF FOR H=0 OR K=0 K+H ODD INDICATES AN
C                       IMAGINARY VALUE FOR THE REFLECTION
C                      ALL OTHER SPECIAL REFLECTIONS ARE REAL
C
      IF(ISPEC(1).LT.1) GO TO 510
      IF(IH.EQ.0) GO TO 560
510   CONTINUE
      IF(ISPEC(2).LT.1) GO TO 520
      IF(IK.EQ.0) GO TO 560
520   CONTINUE
      IF(ISPEC(3).LT.1) GO TO 530
C
C-----WSTAR is a parameter (0.5) here and not an epsion ???
      IF(ABS(Z).LT.WSTAR) GO TO 570
530   CONTINUE
      IF(ISPEC(4).LT.1) GO TO 600
C-----IF(IH.EQ.IK) GO TO 570
C-----For A2: P2221 with additional: H=K=even: real, H=K=odd: imaginary
      IF(IH.EQ.IK)then
        if(IAQP2.eq.1)then
          goto 563
        else
          goto 570
        endif
      endif
      GO TO 600
560   CONTINUE
      IF(ISPEC(5).EQ.0) GO TO 570
      IF(ISPEC(5).EQ.-1) GO TO 563
      I=IH+IK
      GO TO 565
563   I=IK
565   I2=2*(I/2)
      IF(I.GT.I2) IPTEST=90
570   SPEC=.TRUE.
C
600   CONTINUE
C
C      WRITE(6,903)IH,IK,Z,IP1,IP2,SPEC
C 903  FORMAT(' Final situation in ASYM: IH,IK,Z,IP1,IP2,SPEC = ',
C     1  2I5,F10.5,2I10,L1)
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE MULT(IA,IH,IK,Z,IP1,IP2)
C
C     DOES MATRIX MULTIPLICATION TO BRING REFLECTIONS INTO THE
C       ASYMMETRIC UNIT.
C
C     (H' K' Z' AMP' PHS')=(H K Z AMP PHI) <A>
C
C
C        <A> HAS FORM     IA(1)  IA(3)     0      0  IA(6)
C                         IA(2)  IA(4)     0      0  IA(7)
C                             0      0 IA(5)      0      0
C                             0      0     0      1      0
C                             0      0     0      0  IA(8)
C           FOR ALL CASES.
C
C
      INTEGER*4 IA(8),IP1,IP2
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
      IH1=IA(1)*IH+IA(2)*IK
      IK=IA(3)*IH+IA(4)*IK
      IH=IH1
      Z=IA(5)*Z
      IP1=IA(8)*IP1
      IP2=IP2+IA(6)*IH+IA(7)*IK
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
C900   FORMAT(' IA,IH,IK,Z,IP1,IP2 ',8G5.1,10X,5G10.5)
      RETURN
      END
C******************************************************************************
      SUBROUTINE GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NOBS,ZSTAR,
     . FOBS,PHIOBS,IPSGN)
        DATA INIT/0/
        DIMENSION ZSTAR(1),FOBS(1),PHIOBS(1),IPSGN(1),TITLE(20)
        CHARACTER*80 LINE
C
C     PLOT AMPLITUDES AND PHASES ALONG EACH (H,K) LINE USING TRILOG.
C
C          NOBS   - NUMBER OF REFLECTIONS TO BE PLOTTED
C          ZSTAR  - ZSTAR VALUES OF DATA
C          FOBS   - AMPLITUDES OF DATA
C          PHIOBS - PHASE OF DATA
C          ZMIN   - MIN ZSTAR OVERALL
C          ZMAX   - MAX ZSTAR OVERALL
C          FMAX   - MAX AMPLITUDE OVERALL
C          IHIN   - H INDEX OF LATTICE LINE
C          IKIN   - K INDEX OF LINE
C          INIT   - 0 TO OPEN PLOT QUEUE, THEN 1
C          IPSGN  - VALUE OF +/- IQ FROM MAIN PROGRAM,
C                   DETERMINES TYPE OF CHARACTER IN PHASE PLOT
C
        ZMAG=1000.
        FMAG=70.
        PMAG=90.                ! NOW PLOT OVER 540 DEGREES
        GAP=8.
        DELZ = .02
        IF(INIT.EQ.1) THEN
                CALL P2K_PAGE
                GO TO 5
        ENDIF
C
C PLOT TITLE
        PLTSIZ=142.5
        FONTSIZE=4.75
        WRITE(6,'(''Input title for PLOT.PLT'')')
        READ(5,1)TITLE
1       FORMAT(20A4)
        WRITE(6,1)TITLE
C
        CALL P2K_OUTFILE('PLOT.PS',10)
5       CALL P2K_HOME
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
        CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.8*PLTSIZ,0.)
        CALL P2K_COLOUR(0)
        CALL P2K_LWIDTH(0.3)
C
        INIT=1
C
C5      ZRANG=ZMAX-ZMIN
        ZRANG=ZMAX-ZMIN
C
6       ZMM=ZRANG*ZMAG
        IF (ZMM .GT. 100.0) GOTO 7
        ZMAG = ZMAG*2.0
        GOTO 6
7       ZERO=-ZMIN*ZMAG
C
C  DRAW AXES FOR AMPLITUDE BOX
C
        IF(NOBS.LE.8) GO TO 100
C       CALL ORIGIN(20.0,30.0,0)
C       CALL LOCCHR(10.0,-15.0,0)
C       CALL FONT(1)
C       CALL STRING(TITLE,40)
C       CALL FONT(0)
C       CALL MOVETO(0.0,0.0)
C       CALL DRAWTO(0.0,FMAG)
C       CALL DRAWTO(ZMM,FMAG)
C       CALL DRAWTO(ZMM,0.0)
C       CALL DRAWTO(0.0,0.0)
C       CALL MOVETO(ZERO,0.0)
C       CALL DRAWTO(ZERO,FMAG)
CC      POSN=ZRANG*ZMAG-22.
C       CALL LOCCHR(POSN,FMAG-10.0,0)
C        WRITE(LINE,'(''('',I2,'','',I2,'')'')')IHIN,IKIN
C       WRITE(LINE,103) IHIN,IKIN
C103    FORMAT('(',I2,',',I2,')')
C       CALL STRING(LINE,20)
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(5.0,30.0,0.)
C       CALL P2K_MOVE(10.,-15.,0.)
        CALL P2K_MOVE(-5.,-12.,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.8)
        CALL P2K_STRING(TITLE,80,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(0.,FMAG,0.)
        CALL P2K_DRAW(ZMM,FMAG,0.)
        CALL P2K_DRAW(ZMM,0.,0.)
        CALL P2K_DRAW(0.,0.,0.)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(ZERO,0.,0.)
C       POSN=ZRANG*ZMAG-22.
        POSN=ZRANG*ZMAG-18.
        CALL P2K_MOVE(POSN,FMAG-10.0,0.)
        WRITE(LINE,103) IHIN,IKIN
103     FORMAT('(',I2,',',I2,')')
        CALL P2K_STRING(LINE,7,0.)
C
        IZ=ZRANG/DELZ
C
        DO 25 J=1,200
          ZPOS=-0.5+J*DELZ
          IF((ZPOS.LT.ZMIN).OR.(ZPOS.GE.ZMAX))GO TO 25
          XPOS=ZERO+ZPOS*ZMAG
          CALL P2K_MOVE(XPOS,0.,0.)
          CALL P2K_DRAW(XPOS,-2.0,0.)
          XPOS=XPOS-7.0
          CALL P2K_MOVE(XPOS,-5.5,0.)
          WRITE(LINE,26) ZPOS
          CALL P2K_STRING(LINE,6,0.)
25      CONTINUE
26      FORMAT(F6.3)
C
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
        POSN=ZRANG*ZMAG-35.
        CALL P2K_MOVE(POSN,FMAG-10.0,0.)
        WRITE(LINE,27)
27      FORMAT('LATTICE LINE')
        CALL P2K_STRING(LINE,12,0.)
        POSN=ZRANG*ZMAG+2.5
        CALL P2K_MOVE(POSN,-4.5,0.)
        CALL P2K_STRING('RECIPROCAL',10,0.)
        CALL P2K_MOVE(POSN,-7.5,0.)
        CALL P2K_STRING('ANGSTROMS',9,0.)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(ZERO,0.0,0.)
C
        SCALE=FMAG/(1.05*FMAX)
        IA=ALOG10(1.05*FMAX)
        B=10.0**IA
        IC=FMAX*1.05/B
        DO 200 J=1,IC
          F=J*B
          YPOS=F*SCALE
          ZA=ZMIN*ZMAG
          ZB=ZMAX*ZMAG
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+2.0
          CALL P2K_STRING(LINE,6,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-2.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
          XPOS=ZB
          CALL P2K_MOVE(XPOS,YPOS,0.)
          WRITE(LINE,201) F
          CALL P2K_STRING(LINE,7,0.)
200     CONTINUE
        CALL P2K_MOVE(XPOS,0.,0.)
        CALL P2K_STRING('    0.0',7,0.)
201     FORMAT(F7.1)
C
C  PLOT OBSERVED AMPLITUDES FIRST
C
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.48)
        DO 50 J=1,NOBS
          IF(FOBS(J).EQ.-999.) GO TO 50
          XP=ZSTAR(J)*ZMAG - 0.05
          YP=FOBS(J)*SCALE - 0.28*FONTSIZE*0.48
          CALL P2K_MOVE(XP,YP,0.)
c         CALL CSTRING(%REF('X'),1)
          CALL P2K_CSTRING('X',1,0.)
50      CONTINUE
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
C
C  
C  DRAW AXES FOR PHASE BOX.
C
        PMAG2 = PMAG/540.               ! NOW PLOT OVER 540 DEGREES
        YPOS=FMAG+GAP+PMAG/3.0          ! NOW PLOT OVER 540 DEGREES
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(0.0,YPOS,0.)
        ZA=ZMIN*ZMAG
        ZB=ZMAX*ZMAG
        YAXIS=180.0*PMAG2
        CALL P2K_MOVE(ZA,-YAXIS,0.)
        CALL P2K_DRAW(ZA,+YAXIS*2.0,0.) ! NOW PLOT OVER 540 DEGREES
        CALL P2K_DRAW(ZB,+YAXIS*2.0,0.) ! NOW PLOT OVER 540 DEGREES
        CALL P2K_DRAW(ZB,-YAXIS,0.)
        CALL P2K_DRAW(ZA,-YAXIS,0.)
        CALL P2K_MOVE(0.0,-YAXIS,0.)
        CALL P2K_DRAW(0.0,+YAXIS*2.0,0.)        ! NOW PLOT OVER 540 DEGREES
C
        DO 620 J=1,11
          YPOS=(PMAG/12)*J -YAXIS       ! NOW PLOT OVER 540 DEGREES
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+2.0
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-2.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
620     CONTINUE
        DO 630 J=1,7                    ! NOW PLOT OVER 540 DEGREES
          IANG=-180+(J-1)*90
          XPOS=ZB+1.0
          YPOS=IANG*PMAG2
          CALL P2K_MOVE(XPOS,YPOS,0.)
          WRITE(LINE,631) IANG
          CALL P2K_STRING(LINE,4,0.)
630     CONTINUE
631     FORMAT(I4)
C
C  PLOT OBS PHASE POINTS
C
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.48)
        DO 500 J=1,NOBS
          IF(PHIOBS(J).EQ.-999.) GO TO 500
          XP=ZSTAR(J)*ZMAG - 0.05
          YP=PHIOBS(J)*PMAG2 - 0.28*FONTSIZE*0.48
          CALL P2K_MOVE(XP,YP,0.)
        IF (IPSGN(J).GT.0)THEN
c         CALL CSTRING(%REF('X'),1)
          CALL P2K_CSTRING('X',1,0.)
        ELSE
c         CALL CSTRING(%REF('O'),1)
          CALL P2K_CSTRING('O',1,0.)
        ENDIF
      IF(PHIOBS(J).LT.0.0) THEN         ! REPEAT PLOT+360 IF PHASE IS .LT.0
                XP=ZSTAR(J)*ZMAG - 0.05
                YP=(PHIOBS(J)+360.0)*PMAG2 - 0.28*FONTSIZE*0.48
                CALL P2K_MOVE(XP,YP,0.)
                IF (IPSGN(J).GT.0)THEN
c                 CALL CSTRING(%REF('X'),1)
                  CALL P2K_CSTRING('X',1,0.)
                ELSE
c                 CALL CSTRING(%REF('O'),1)
                  CALL P2K_CSTRING('O',1,0.)
                ENDIF
      ENDIF
500     CONTINUE
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
C
C  GET READY FOR NEXT PLOTTED LINE.
C
99      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        RETURN
C
100     CALL P2K_MOVE(10.0,20.0,0.)
CTSH    WRITE(LINE,104) IHIN,IKIN
CTSH++
        WRITE(LINE,104) IHIN,IKIN
CTSH--
104     FORMAT('TOO FEW SPOTS ON LINE (',I2,',',I2,')')
        CALL P2K_STRING(LINE,29,0.)
C
        RETURN
        END
C******************************************************************************
      SUBROUTINE TILTP2(IN,IHIN,IKIN,IQIN,AIN,PTEMP,AMP,JREFL,JH,JK,
     1 ZSTAR,JHC,JKC,JLC,JSC,IFCC,IPHC,IBEGIN,IFINISH,TAXA,ABANG,TANGL,
     2 ASTAR,BSTAR,IFINSH,A1,A2,A3,A4,A5,IGO,ISPEC,C,
     3 REVHK,SGNXCH,ROT180,IQMAX,LREV,ROT90,IAQP2,REVHND,PHRESID,
     4 RMSRESID,
     4 IVERBOSE)
C
      PARAMETER (MAXINDEX=60)
      PARAMETER (NSLOTS=256)
      INTEGER IH,IK,IHIN(1),IKIN(1),IQIN(1)
      INTEGER*4 JH(1),JK(1),JHC(1),JKC(1),JLC(1),JSC(1),IFCC(1),IPHC(1)
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
      REAL AIN(1),PTEMP(1),AMP(1),ZSTAR(1)
      REAL*8 A(2,2),B(2),W(20),E
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
      INTEGER*4 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      LOGICAL SPEC,LREV,IOK
      DIMENSION ASIZ(NSLOTS),FREFSIZ(NSLOTS),PHSRES(NSLOTS),
     .  NPHSRES(NSLOTS),AVIQ(NSLOTS)
      DATA ISLOTSIZ/50/
      DATA DRAD,RDEG/0.0174532,57.295779/
      WSTAR=0.01
      IPTEST=0
      TAXB=TAXA+ABANG
      WRITE(6,8051)TAXA,TAXB,ABANG
8051  FORMAT(' ENTERING TILTAXIS REFINEMENT, TAXA,TAXB,ABANG=',3F9.2)
      IF(TANGL.EQ.0.0) TANGL=0.05    ! derivative wrt TAXA infinite if TANGL=0.0
C
      FSHIFT=0.5
      IEND=0
      NCYCL=12
      DO 8300 ICYCL=1,NCYCL
        DO I=1,2
          B(I)=0.
          DO J=1,2
            A(I,J)=0.
          enddo
        enddo
        TAXB=TAXA+ABANG
        if (IVERBOSE.gt.3) then
          WRITE(6,'('' Cycle '',I6,'' tiltaxis refinement: '',3F9.2)')
     1       ICYCL,TAXA,TAXB,ABANG
        endif
C
        ZH=ASTAR*TAN(TANGL*DRAD)*SIN(TAXA*DRAD)
        ZK=BSTAR*TAN(TANGL*DRAD)*SIN(TAXB*DRAD)
        DZTHEH=ASTAR*SIN(TAXA*DRAD)/(COS(TANGL*DRAD))**2
        DZTHEK=BSTAR*SIN(TAXB*DRAD)/(COS(TANGL*DRAD))**2      
        DZPHIH=ASTAR*TAN(TANGL*DRAD)*COS(TAXA*DRAD)
        DZPHIK=BSTAR*TAN(TANGL*DRAD)*COS(TAXB*DRAD)
C
        RMSRESID=0.
        PNUMER=0.
        NP=0
        NOTUSED=0
        NOTNEAR=0
        NOTFOUND=0
        DO J=1,NSLOTS
          NPHSRES(J)=0  ! ZERO HIST0GRAM
          ASIZ(J)=0.0
          AVIQ(J)=0.0
          FREFSIZ(J)=0.0
          PHSRES(J)=0.0
        enddo
        IF(IVERBOSE.gt.5)WRITE(6,1112)
        DO 8100 I=1,IN
C---------FIRST CALCULATE H,K,Z AND DZTHE,DZPHI IN CORRECT ASYMMETRIC UNIT.
          IF(IQIN(I).GT.IQMAX) THEN
            NOTUSED=NOTUSED+1
            GO TO 8100
          ENDIF
          IH=IHIN(I)
          IK=IKIN(I)
          Z=ZH*IH+ZK*IK
          IF(Z.EQ.0.0) Z=0.0000001        ! ALLOWS Z-CHANGE TEST AS FLAG BELOW.
          ZCOMP=Z
          DZTHE=DZTHEH*IH+DZTHEK*IK
          DZPHI=DZPHIH*IH+DZPHIK*IK
C---------NOW REINDEX AND CHECK Z
          CALL FIDDLE2(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90,REVHND,REVXSGN)
          IF(Z.NE.ZCOMP) THEN
            DZTHE=-DZTHE
            DZPHI=-DZPHI
          ENDIF
          ZASYM=Z
          IF(IVERBOSE.gt.5)WRITE(6,8700)IH,IK,Z,IP1,IP2,A1,A2,A3,A4,
     .        A5,IGO,ISPEC
8700      FORMAT(2I5,F10.5,2I5/7(8I5/))
          IP1=1
          IP2=0
C         IF(IVERBOSE.gt.5)WRITE(6,18100)IHIN(I),IKIN(I),IH,IK,ZASYM
          CALL ASYM(IH,IK,ZASYM,IP1,IP2,SPEC,IPTEST,WSTAR,
     1              A1,A2,A3,A4,A5,IGO,ISPEC,LREV,IAQP2,IVERBOSE)
          IF(IVERBOSE.gt.5)WRITE(6,18100)IHIN(I),IKIN(I),IH,IK,ZASYM
C
C---------HERE FOR GETCRVAL SUBROUTINE_CALL !----------------------
          CALL GETCRVAL(I,IHIN,IKIN,IH,IK,ZASYM,
     .       JLC,IFCC,IPHC,IBEGIN,IFINISH,ISPEC,
     .       IOK,C,FREF,PREF,DPDZCU,IVERBOSE)
          IF(.NOT.IOK) THEN
            NOTFOUND=NOTFOUND+1
C           WRITE(6,18100)IHIN(I),IKIN(I),IH,IK,ZASYM
18100       FORMAT(4I5,F10.5)
            GO TO 8100
          ENDIF
          PREF=PREF*IP1-IP2     ! P=P*IP1-IP2 TRANSFORMS FROM CURVE TO INPUT.
          DPDZ = DPDZCU*IP1
          IF(Z.NE.ZASYM) DPDZ=-DPDZ
C---------ABOVE TWO LINES CORRECT THE PHASE GRADIENT IF Z OR PHASE IS CHANGED IN ASYM.
C---------TRANSFORMATION LATER FROM INPUT TO CURVE ASYMMETRIC UNIT IS P=P*IP1+IP2.
          NBEGIN=IBEGIN(IH,IK)
CHEN
          if(NBEGIN.eq.0)NBEGIN=1
CHEN
          IZLESS=INT(ZASYM*C+100)-100
          IZLESS=NBEGIN+(IZLESS-JLC(NBEGIN))
          IF(IZLESS.EQ.0) THEN
            write(*,*)'Warning !!!!! IZLESS=0 for IH,IK',IH,IK
            IZLESS=1
          ENDIF
          IZMORE=IZLESS+1
          ZLESS = JLC(IZLESS)/C
          IF(IVERBOSE.gt.5)WRITE(6,1111)IHIN(I),IKIN(I),Z,AIN(I),
     .      PTEMP(I),IH,IK,ZASYM,
     .      FREF,PREF,IPHC(IZLESS),IPHC(IZMORE),
     .      JHC(IZLESS),JKC(IZLESS),JLC(IZLESS),ZLESS
1111      FORMAT(2I5,F8.4,2F8.1,2I5,F8.4,F8.1,F8.1,5I6,F8.4)
1112      FORMAT(' NKIN NKIN ZSTARIN     AIN   PTEMP   IH',
     . '   IK   ZASYM    AREF    PREF    IPH-  IPH+   IH-   IK-   IL-',
     . '   ZLESS')
C
          WEIGHT=1.0
C---------COULD BE CHANGED TO 1/SIGMA**2 OR AMPLITUDE WEIGHTS LATER.
          JIN=JREFL+I
          AA0=PREF-PTEMP(I)
C
C---------CHECK THAT PHASE IS IN RANGE -180 TO 180 SO THAT MINIMISATION IS USEFUL.
8261      IF(AA0.LT.180.0) GO TO 8260
          AA0=AA0-360.0
          GO TO 8261
8260      IF(AA0.GT.-180.0) GO TO 8262
          AA0=AA0+360.0
          GO TO 8260
8262      CONTINUE
C---------FOR RESOLUTION-DEPENDENT HISTOGRAM OF PHASE RESIDUALS.
          ISLOT=1+(JSC(IZLESS)-1)/ISLOTSIZ
          IF(ISLOT.LT.1.OR.ISLOT.GE.NSLOTS) STOP 'ISLOT problem'
          ASIZ(ISLOT)=ASIZ(ISLOT)+AIN(I)
          AVIQ(ISLOT)=AVIQ(ISLOT)+IQIN(I)
          FREFSIZ(ISLOT)=FREFSIZ(ISLOT)+FREF
          PHSRES(ISLOT)=PHSRES(ISLOT)+ABS(AA0)
          NPHSRES(ISLOT)=NPHSRES(ISLOT)+1
          IF(ABS(AA0).GT.90.0) THEN     ! USE ONLY IF NEAR ENOUGH (90 DEG).
            NOTNEAR=NOTNEAR+1
            GO TO 8100
          ENDIF
C
          AA1=DPDZ*DZTHE
          AA2=DPDZ*DZPHI
          A(1,1)=A(1,1)+WEIGHT*AA1*AA1
          A(1,2)=A(1,2)+WEIGHT*AA1*AA2
          A(2,1)=A(2,1)+WEIGHT*AA2*AA1
          A(2,2)=A(2,2)+WEIGHT*AA2*AA2
          B(1)  =B(1)  -WEIGHT*AA0*AA1
          B(2)  =B(2)  -WEIGHT*AA0*AA2
          IF(AA0.LT.0.0) AA0=-AA0
          PNUMER  = PNUMER  +AA0
          RMSRESID=RMSRESID +AA0**2
          NP = NP+1
          IF(IVERBOSE.gt.5)WRITE(6,8800)IH,IK,Z,PREF,PTEMP(I),AA0,
     .         AA1,AA2,DPDZ
8800      FORMAT(' H,K,Z,PC,PO,A0,A1,A2,DPDZ',
     .            2I5,F8.5,2F8.1,F10.1,2F12.5,F12.1)
8100    CONTINUE
        IF(IEND.EQ.1)RETURN
        IA=2
        N=2
        E=-1.0
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) GO TO 8150
          WRITE(6,8101)E
8101      FORMAT(':: MA21AD FAILED',F10.5)
CHEN>
          WRITE(*,'('':: '')')
          WRITE(*,'('':: MA21AD Failed, but continuing '',
     .     ''nevertheless...'')')
          WRITE(*,'('':: '')')
C---------STOP
          PHRESID = 99999.9
          RETURN
CHEN<
8150    THETA=FSHIFT*B(1)/DRAD
        PHI=FSHIFT*B(2)/DRAD
        if(IVERBOSE.gt.3)then
          IF(ABS(PHI).GT.10.0) WRITE(6,8151)PHI,THETA
          IF(ABS(THETA).GT.10.0) WRITE(6,8151)PHI,THETA
8151      FORMAT(' SHIFTS CALCULATED TO BE TOO LARGE',2F12.2)
        endif
        IF(ABS(THETA).GT.10.0) THETA=SIGN(10.0,THETA)
        IF(ABS(PHI).GT.10.0) PHI=SIGN(10.0,PHI) 
        PHRESID=PNUMER/NP
        RMSRESID=SQRT(RMSRESID/NP)
        TAXA=TAXA+PHI
        TANGL=TANGL+THETA
        IF(IVERBOSE.gt.3)WRITE(6,8152)ICYCL,TAXA,TANGL,PHI,THETA,
     .                NP,NOTFOUND,
     .                NOTUSED,NOTNEAR,PHRESID,RMSRESID
8152    FORMAT(' CYC=',I2,' NEW TAXA,TANGL=',2F8.3,
     1         ' SHFTS=',2F8.3,
     2         ' FOR',I4,'(EXCL',3I4,')SPOTS, RESID=',F7.3,
     3         ' RMSRESID =',F7.3)
        IF(IVERBOSE.gt.3)WRITE(6,18152)NOTFOUND,NOTUSED,NOTNEAR
18152   FORMAT(' SPOTS EXCLUDED FOR; NOTFOUND=',I4,'; NOTUSED(IQ)=',I4/
     .          '  NOTNEAR(>90.0)=',I4/)
        IF((ABS(THETA).LT.0.1).AND.(ABS(PHI).LT.0.1)) IEND=1
        IF((ICYCL.EQ.1).AND.(IEND.EQ.1)) THEN
          IFINSH=1
C---------PRINT OUT RESOLUTION-DEPENDENT RESIDUALS AT END OF REFINEMENT.
          WRITE(6,8292)IQMAX
8292      FORMAT(//2X,10('*'),' PHASE RESID AS FUNCT OF RESOL',
     .   ' ********** FOR ALL SPOTS WITH IQ .LE.'I2/5X,
     .   '  RANGE    DMIN      DMAX   MEANFCURV    MEANA',
     .   '   RESIDUAL  NSPOTS  AVIQ'/)
          NALL=0
          FALL=0.0
          AALL=0.0
          PHSALL=0.
          DO 8290 J=1,NSLOTS
            IF(NPHSRES(J).EQ.0) GO TO 8290
            FALL=FALL+FREFSIZ(J)
            AALL=AALL+ASIZ(J)
            AVIQALL=AVIQALL+AVIQ(J)
            PHSALL=PHSALL+PHSRES(J)
            NALL=NALL+NPHSRES(J)
            PHSRES(J)=PHSRES(J)/NPHSRES(J)
            ASIZ(J)=ASIZ(J)/NPHSRES(J)
            AVIQ(J)=AVIQ(J)/NPHSRES(J)
            FREFSIZ(J)=FREFSIZ(J)/NPHSRES(J)
            DMIN=SQRT(10000.0/((J-1)*ISLOTSIZ + 1))
            DMAX=SQRT(10000.0/(J*ISLOTSIZ))
            WRITE(6,8291)J,DMIN,DMAX,FREFSIZ(J),ASIZ(J),PHSRES(J),
     .                   NPHSRES(J),AVIQ(J)
8291        FORMAT(5X,I6,4F10.3,F10.2,I7,F8.2)
8290      CONTINUE
8293      FORMAT(/4X,'OVERALL',20X,2F10.3,F10.2,I7,F8.2)
          PHSALL=PHSALL/NALL
          FALL=FALL/NALL
          AALL=AALL/NALL
          AVIQALL=AVIQALL/NALL
          WRITE(6,8293)FALL,AALL,PHSALL,NALL,AVIQALL
        ENDIF
8300  CONTINUE
C
      RETURN
C
      END
C
C
C
C******************************************************************************
      SUBROUTINE GETCRVAL(ISPOT,IHIN,IKIN,IH,IK,ZASYM,
     . JLC,IFCC,IPHC,IBEGIN,IFINISH,ISPEC,IOK,C,FREF,PREF,
     .   DPDZCU,IVERBOSE)
        LOGICAL IOK
C
C-----calculates the AMP and PHS value of the interpolated lattice lines 
C-----for the correct height Zstar of the tilted reflection
C
      PARAMETER (MAXINDEX=60)
      INTEGER IH,IK,IHIN(1),IKIN(1)
      INTEGER*4 JLC(1),IFCC(1),IPHC(1)
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     . IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
      REAL*8 A(2,2),B(2),W(20),E
C
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
      INTEGER*4 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      DATA DRAD,RDEG,PI/0.0174532,57.295779,3.14159/
      DATA BTEMP/80.0/
C
      IOK=.TRUE.
C
C-----Get boundaries of concerned lattice line:
C
      NBEGIN=IBEGIN(IH,IK)
      NFINISH=IFINISH(IH,IK)
C
C      WRITE(6,7798)IH,IK,IHIN(ISPOT),IKIN(ISPOT),ZASYM,
C     .JLC(NBEGIN)/C,JLC(NFINISH)/C,NBEGIN,NFINISH
C7798  FORMAT(' GETCRVAL: called for LINE            ',2I5,'   SPOT',2I5,
C     .'        ZSTAR=',F8.4,' RANGE=',2F8.4,2I9)
C
C-----Check, if not existing:
      IF((NBEGIN.NE.-999).AND.(NFINISH.NE.-999)) GO TO 70
        IOK=.FALSE.
        if(IVERBOSE.gt.6)WRITE(6,1107)IH,IK,IHIN(ISPOT),
     .       IKIN(ISPOT),NBEGIN,NFINISH
1107    FORMAT(' GETCRVAL: LATTICE LINE NOT FOUND',2I5,'   SPOT',2I5,
     1    '   NBEGIN',I5,'   NFINISH',I5)
        RETURN
C
 70   continue
C
C      write(6,'(/,'' GETCRVAL called with ISPOT,IHIN,IKIN,IH,IK,ZASYM,'',
C     1     ''JLC,IFCC,IPHC,IBEGIN,IFINISH= '',5I6,F12.3,5I8)')
C     2 ISPOT,IHIN,IKIN,IH,IK,ZASYM,
C     3  JLC,IFCC,IPHC,IBEGIN(IH,IK),IFINISH(IH,IK)
C
      ZBEGIN=JLC(NBEGIN)/C
      ZFINISH=JLC(NFINISH)/C
      ZALLOW = 1.0/(C*2.0)      ! Allow calculation of point from curve provided
C                               ! that point and available curve data are close.
C
C-----Check, if there is sufficient distance to lattice line boundaries:
      IF((ZASYM.LT.ZBEGIN-ZALLOW).OR.(ZASYM.GT.ZFINISH+ZALLOW))THEN
        IOK=.FALSE.
        if(IVERBOSE.gt.5)WRITE(6,1108)IH,IK,IHIN(ISPOT),
     .       IKIN(ISPOT),ZASYM,
     .   JLC(NBEGIN)/C,JLC(NFINISH)/C,NBEGIN,NFINISH
1108    FORMAT(' GETCRVAL: ZSTAR OUTSIDE RANGE ON LINE',2I5,
     .     '   SPOT',2I5,
     .   '        ZSTAR=',F8.4,' RANGE=',2F8.4,2I9)
        RETURN
      endif
C
C.....BELOW IS CALCULATION OF PHASE AT EXACT VALUE OF ZSTAR.
C.....IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C.....DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C.....TO ZERO AT TWO REFLECTIONS AWAY FROM THE POINT BEING
C.....CALCULATED. THUS THE CALCULATION GIVES DOUBLE THE VALUE
C.....OF F WHICH WOULD BE OBTAINED BY SIMPLE INTERPOLATION.
C.....AFTER DIVIDING THE RESULT BY TWO,THE OUTPUT COLUMN 
C.....FREF IS THEREFORE DIRECTLY COMPARABLE WITH THE INPUT
C.....AMPLITUDES.  (see below).
C
      CPART=0.0
      SPART=0.0
      DZ=0.0004 !  SET DZ FOR GRADIENT CALC HERE.
      CPARTDZ=0.0
      SPARTDZ=0.0
C
      DO 85 I=NBEGIN,NFINISH       ! Summation over given lattice line data.
        ZI=JLC(I)/C
        ZDIFF=ZASYM-ZI
        ZDIFFDZ=ZASYM+DZ-ZI
C  Next section is summation over lattice line for all space groups.
        IF(ZDIFF.NE.0) GO TO 81
          SINCDAMP=1.0
        GO TO 82
81        ARGEXP=-0.25*BTEMP*ZDIFF**2
          ARGSINC=0.5*PI*ZDIFF*C
          SINCF=SIN(ARGSINC)/ARGSINC
          SINCDAMP=SINCF*EXP(ARGEXP)
82      CONTINUE
        IF(ZDIFFDZ.NE.0) GO TO 83
          SINCDMPDZ=1.0
        GO TO 84
83        ARGEXP=-0.25*BTEMP*ZDIFFDZ**2
          ARGSINC=0.5*PI*ZDIFFDZ*C
          SINCF=SIN(ARGSINC)/ARGSINC
          SINCDMPDZ=SINCF*EXP(ARGEXP)
84      CONTINUE
        PHS=IPHC(I)*DRAD
CAND
C        WRITE(6,55534)IH,IK,JLC(I),IFCC(I),IPHC(I)
C55534   FORMAT('IH,IK,JLC,IFCC,IPHC:',3I5,2I8)
C
        CPART=CPART+SINCDAMP*COS(PHS)*IFCC(I)
        SPART=SPART+SINCDAMP*SIN(PHS)*IFCC(I)
        CPARTDZ=CPARTDZ+SINCDMPDZ*COS(PHS)*IFCC(I)
        SPARTDZ=SPARTDZ+SINCDMPDZ*SIN(PHS)*IFCC(I)
C
C-------The following section is included for lattice lines in space groups
C-------where only the positive half of the lattice line is stored.
C
        IF(ISPEC(3).EQ.1.AND.JLC(I).NE.0) THEN
          ZDIFFMINUS=ZASYM+ZI
          ZDIFFMINUSDZ=ZASYM+ZI+DZ
          ARGEXP=-0.25*BTEMP*ZDIFFMINUS**2
          ARGSINC=0.5*PI*ZDIFFMINUS*C
          if(ARGSINC.ne.0.0)then
            SINCF=SIN(ARGSINC)/ARGSINC
          else
            SINCF=1.0
          endif
          SINCDAMP=SINCF*EXP(ARGEXP)
          ARGEXP=-0.25*BTEMP*ZDIFFMINUSDZ**2
          ARGSINC=0.5*PI*ZDIFFMINUSDZ*C
          if(ARGSINC.ne.0.0)then
            SINCF=SIN(ARGSINC)/ARGSINC
          else
            SINCF=1.0
          endif
          SINCDMPDZ=SINCF*EXP(ARGEXP)
          PHS= -IPHC(I)*DRAD    ! Phase(hkl) = -Phase(hk-l)
          CPART=CPART+SINCDAMP*COS(PHS)*IFCC(I)
          SPART=SPART+SINCDAMP*SIN(PHS)*IFCC(I)
          CPARTDZ=CPARTDZ+SINCDMPDZ*COS(PHS)*IFCC(I)
          SPARTDZ=SPARTDZ+SINCDMPDZ*SIN(PHS)*IFCC(I)
        ENDIF
85    CONTINUE
C
      FREF=0.5*SQRT(SPART**2+CPART**2)          ! Here for divide by two
C                                               ! referred to above.
      PREF=RDEG*ATAN2(SPART,CPART)
      FREFDZ=0.5*SQRT(SPARTDZ**2+CPARTDZ**2)
      PREFDZ=RDEG*ATAN2(SPARTDZ,CPARTDZ)
      PDIFF=PREFDZ-PREF ! MAX 8 DEG IN 0.0004 DZ == 180 DEG IN 0.01 DZ.
      IF(ABS(PDIFF).GT.180.0) PDIFF=PDIFF-SIGN(360.0,PDIFF)
      IF(ABS(PDIFF).GT.8.0) PDIFF=SIGN(8.0,PDIFF)
      DPDZCU = PDIFF/DZ
      if (IVERBOSE.gt.6) then
         WRITE(6,86)IH,IK,ZASYM,FREF,FREFDZ,PREF,PREFDZ,DPDZCU
86       FORMAT(' H,K,Z,F,F+DZ,P,P+DZ,DPDZ= ',2I5,F8.4,2F10.2,2F10.3,F15.0)
      endif
C
      RETURN
      END
C
C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180)
      IF(REVHK.EQ.0.0) GO TO 225
      I=IH
      IH=IK
      IK=I
      Z=-Z
225   CONTINUE 
      IF(SGNXCH.EQ.0.0) GO TO 230
      IK=-IK
      Z=-Z
  230 IF(ROT180.EQ.0) GO TO 231
      IH=-IH
      IK=-IK
231   CONTINUE
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE2(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90,REVHND,
     1  REVXSGN)
      if(REVHK.GT.0.1)then
        I=IH
        IH=IK
        IK=I
        Z=-Z
      endif
      if(SGNXCH.gt.0.01)then
        IK=-IK
        Z=-Z
      endif
      if(ROT180.gt.0.01)then
        IH=-IH
        IK=-IK
      endif
      if(ROT90.gt.0.01)then
        I=IH
        IH=-IK
        IK=I
      endif
      if(REVHND.gt.0.01)then
        Z=-Z
      endif
      if(REVXSGN.gt.0.01)then
        IH=-IH
      endif
      RETURN
      END
C******************************************************************************
C              TO APPLY ORIGIN AND BEAMTILT PHASE-SHIFT.
      FUNCTION PHSHFT(IH,IK,OX,OY,TX,TY,BEAMSHFT,B)
      DIMENSION BEAMSHFT(4)
      ASTAR=BEAMSHFT(2) ! attempt to not make it p3 specific.
      BSTAR=BEAMSHFT(3) !    "      "
      PHSHFT=IH*OX + IK*OY + B*(IH*TX*ASTAR+IK*TY*BSTAR)
      RETURN
      END
C******************************************************************************
C   PHASE ORIGIN AND BEAM TILT REFINEMENT TOGETHER BY R-FACTOR MINIMISATION.
C   THE RECIPROCAL SPACE DISTANCE ACTUALLY MINIMISED IS
C       L = SUM OF 2 * SIN(ABS(PHASEDIFF/2))
C       New version BEAMTILTA -- 3.3.89 used Harwell subroutine_VA04A by
C                               by M.J.D.Powell to minimise above value of L
C
      SUBROUTINE BEAMTILTA
      PARAMETER  (MAXRFL=20000)
      REAL*4 PARAMS(4),E(4)
      COMMON WORK(28),NREF,IH(MAXRFL),IK(MAXRFL),IQ(MAXRFL),
     . PHSI(MAXRFL),PHSC(MAXRFL),WGT(MAXRFL),OXNEW,OYNEW,TX,TY,
     . BEAMSHFT(4),BSH(MAXRFL),IQMAX,NC,
     .   FUNCMIN,RESTOT,NTOT
      DATA DRAD/0.0174533/
      DATA E/0.02,0.02,0.001,0.001/,ESCALE/200.0/
      DATA IPRINT/0/,ICONV/1/,MAXIT/100/
                PARAMS(1)=OXNEW
                PARAMS(2)=OYNEW
                PARAMS(3)=TX
                PARAMS(4)=TY
                ASTAR=BEAMSHFT(2)
      ITER=0
        WRITE(6,1010)
1010    FORMAT(//' ******* BEAM TILTA REFINEMENT BEGINNING *********'/
     . ' NITER      OX        OY        TX        TY   ',
     . '      FUNCMIN   RESTOT  NTOT ')
        WRITE(6,1011)ITER,(PARAMS(J),J=1,4)
C
      NPARAMS=4
      CALL VA04A(WORK,PARAMS,E,NPARAMS,F,
     . ESCALE,IPRINT,ICONV,MAXIT,ITER)
C
           WRITE(6,1011)ITER,(PARAMS(I),I=1,4),FUNCMIN,RESTOT,NTOT
1011    FORMAT(I6,2F10.2,2F10.3,F12.2,F11.2,I6)
332     BEDGE=225.0*ASTAR**2*BEAMSHFT(1) !  SPECIFIC FOR P3, BUT USED ONLY FOR
C               !  COSMETIC OUTPUT --  I.E.NOT USED IN REAL CALCULATIONS.
        PS1=ABS(PHSHFT(15,0,0.,0.,PARAMS(3),PARAMS(4),BEAMSHFT,BEDGE))
        PS2=ABS(PHSHFT(0,15,0.,0.,PARAMS(3),PARAMS(4),BEAMSHFT,BEDGE))
        PS3=ABS(PHSHFT(-15,15,0.,0.,PARAMS(3),PARAMS(4),
     . BEAMSHFT,BEDGE))
        PMAX15=AMAX1(PS1,PS2,PS3)
        WRITE(6,1012)PMAX15
1012    FORMAT(' THIS AMOUNT OF BEAMTILT CAUSES THE MAX CORRECTION OF',
     . ' PHASE TO A REFLECTION'/
     .   ' AT RADIUS OF (15,0) OF',F10.3,' DEGREES'/
     . ' (THIS CALCULATION IS NOT PRECISE EXCEPT IN SPACE GROUP P3)'//) 
      OXNEW=PARAMS(1)
      OYNEW=PARAMS(2)
      TX=PARAMS(3)
      TY=PARAMS(4)
      RETURN
      END
C******************************************************************************
      SUBROUTINE CALCFX(N,PARAMS,F)
      PARAMETER (MAXRFL=20000)
      REAL*4 E(4),B(4)
      REAL*4 PARAMS(4)
      COMMON WORK(28),NREF,IH(MAXRFL),IK(MAXRFL),IQ(MAXRFL),
     . PHSI(MAXRFL),PHSC(MAXRFL),WGT(MAXRFL),OXNEW,OYNEW,TX,TY,
     . BEAMSHFT(4),BSH(MAXRFL),IQMAX,NC,
     .   FUNCMIN,RESTOT,NTOT
      DATA DRAD,RDEG/0.0174532,57.295779/
        NTOT=0
        RESTOT=0.
C       NCOMP=0
C       NFAR=0
C       RESID=0.0
        FUNCMIN=0.0
        WTOTAL=0.0
        DO 340 M=1,NREF
        IF (WGT(M).EQ.0.0) GO TO 340
        IF (IQ(M).GT.IQMAX) GO TO 340
        PM=PHSI(M)+PHSHFT(IH(M),IK(M),PARAMS(1),PARAMS(2),
     . PARAMS(3),PARAMS(4),BEAMSHFT,BSH(M))
        PN=PHSC(M)
        PDIFF=AMOD((PM-PN),360.0)
        IF(ABS(PDIFF).GT.180.0) PDIFF=PDIFF-SIGN(360.0,PDIFF)
        SINPD2=ABS(PDIFF*DRAD/2.0)
        SINPD2=SIN(SINPD2)
        NTOT=NTOT+1
        RESTOT=RESTOT+ABS(PDIFF)
        WTOTAL=WTOTAL+WGT(M)
        FUNCMIN=FUNCMIN+2.0*SINPD2*WGT(M)
340     CONTINUE
        RESTOT=RESTOT/NTOT
        FUNCMIN=FUNCMIN*57.295779/WTOTAL
        F=FUNCMIN
C          WRITE(6,1011)NC,(PARAMS(I),I=1,4),(B(I),I=1,4),RESID,
C     .                 FUNCMIN,NCOMP,NFAR
1011    FORMAT(I5,2F10.2,2F10.3,F10.2,F7.2,2F7.3,2F10.2,I10,I6)
      RETURN
      END
***************************************************************************
      SUBROUTINE VA04A(W,X,E,N,F,ESCALE,IPRINT,ICON,MAXIT,ITERC)
C  STANDARD FORTRAN 66 (A VERIFIED PFORT SUBROUTINE)
C      COMMON W
      DIMENSION W(1)
      DIMENSION X(1),E(1)
      DDMAG=0.1*ESCALE
      SCER=0.05/ESCALE
      JJ=N*N+N
      JJJ=JJ+N
      K=N+1
      NFCC=1
      IND=1
      INN=1
      DO 1 I=1,N
      DO 2 J=1,N
      W(K)=0.
      IF(I-J)4,3,4
    3 W(K)=ABS(E(I))
      W(I)=ESCALE
    4 K=K+1
    2 CONTINUE
    1 CONTINUE
      ITERC=1
      ISGRAD=2
      CALL CALCFX(N,X,F)
      FKEEP=ABS(F)+ABS(F)
    5 ITONE=1
      FP=F
      SUM=0.
      IXP=JJ
      DO 6 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)
    6 CONTINUE
      IDIRN=N+1
      ILINE=1
    7 DMAX=W(ILINE)
      DACC=DMAX*SCER
      DMAG=AMIN1(DDMAG,0.1*DMAX)
      DMAG=AMAX1(DMAG,20.*DACC)
      DDMAX=10.*DMAG
      GO TO (70,70,71),ITONE
   70 DL=0.
      D=DMAG
      FPREV=F
      IS=5
      FA=F
      DA=DL
    8 DD=D-DL
      DL=D
   58 K=IDIRN
      DO 9 I=1,N
      X(I)=X(I)+DD*W(K)
      K=K+1
    9 CONTINUE
      CALL CALCFX(N,X,F)
C
      NFCC=NFCC+1
      GO TO (10,11,12,13,14,96),IS
   14 IF(F-FA)15,16,24
   16 IF (ABS(D)-DMAX) 17,17,18
   17 D=D+D
      GO TO 8
   18 WRITE(6,19)
   19 FORMAT(5X,44HVA04A MAXIMUM CHANGE DOES NOT ALTER FUNCTION)
      GO TO 20
   15 FB=F
      DB=D
      GO TO 21
   24 FB=FA
      DB=DA
      FA=F
      DA=D
   21 GO TO (83,23),ISGRAD
   23 D=DB+DB-DA
      IS=1
      GO TO 8
   83 D=0.5*(DA+DB-(FA-FB)/(DA-DB))
      IS=4
      IF((DA-D)*(D-DB))25,8,8
   25 IS=1
      IF(ABS(D-DB)-DDMAX)8,8,26
   26 D=DB+SIGN(DDMAX,DB-DA)
      IS=1
      DDMAX=DDMAX+DDMAX
      DDMAG=DDMAG+DDMAG
      IF(DDMAX-DMAX)8,8,27
   27 DDMAX=DMAX
      GO TO 8
   13 IF(F-FA)28,23,23
   28 FC=FB
      DC=DB
   29 FB=F
      DB=D
      GO TO 30
   12 IF(F-FB)28,28,31
   31 FA=F
      DA=D
      GO TO 30
   11 IF(F-FB)32,10,10
   32 FA=FB
      DA=DB
      GO TO 29
   71 DL=1.
      DDMAX=5.
      FA=FP
      DA=-1.
      FB=FHOLD
      DB=0.
      D=1.
   10 FC=F
      DC=D
   30 A=(DB-DC)*(FA-FC)
      B=(DC-DA)*(FB-FC)
      IF((A+B)*(DA-DC))33,33,34
   33 FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 26
   34 D=0.5*(A*(DB+DC)+B*(DA+DC))/(A+B)
      DI=DB
      FI=FB
      IF(FB-FC)44,44,43
   43 DI=DC
      FI=FC
   44 GO TO (86,86,85),ITONE
   85 ITONE=2
      GO TO 45
   86 IF (ABS(D-DI)-DACC) 41,41,93
   93 IF (ABS(D-DI)-0.03*ABS(D)) 41,41,45
   45 IF ((DA-DC)*(DC-D)) 47,46,46
   46 FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 25
   47 IS=2
      IF ((DB-D)*(D-DC)) 48,8,8
   48 IS=3
      GO TO 8
   41 F=FI
      D=DI-DL
      DD=SQRT((DC-DB)*(DC-DA)*(DA-DB)/(A+B))
      DO 49 I=1,N
      X(I)=X(I)+D*W(IDIRN)
      W(IDIRN)=DD*W(IDIRN)
      IDIRN=IDIRN+1
   49 CONTINUE
      W(ILINE)=W(ILINE)/DD
      ILINE=ILINE+1
      IF(IPRINT-1)51,50,51
   50 WRITE(6,52) ITERC,NFCC,F,(X(I),I=1,N)
   52 FORMAT (/1X,9HITERATION,I5,I15,16H FUNCTION VALUES,
     110X,3HF =,E21.14/(5E24.14))
      GO TO(51,53),IPRINT
   51 GO TO (55,38),ITONE
   55 IF (FPREV-F-SUM) 94,95,95
   95 SUM=FPREV-F
      JIL=ILINE
   94 IF (IDIRN-JJ) 7,7,84
   84 GO TO (92,72),IND
   92 FHOLD=F
      IS=6
      IXP=JJ
      DO 59 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)-W(IXP)
   59 CONTINUE
      DD=1.
      GO TO 58
   96 GO TO (112,87),IND
  112 IF (FP-F) 37,37,91
   91 D=2.*(FP+F-2.*FHOLD)/(FP-F)**2
      IF (D*(FP-FHOLD-SUM)**2-SUM) 87,37,37
   87 J=JIL*N+1
      IF (J-JJ) 60,60,61
   60 DO 62 I=J,JJ
      K=I-N
      W(K)=W(I)
   62 CONTINUE
      DO 97 I=JIL,N
      W(I-1)=W(I)
   97 CONTINUE
   61 IDIRN=IDIRN-N
      ITONE=3
      K=IDIRN
      IXP=JJ
      AAA=0.
      DO 65 I=1,N
      IXP=IXP+1
      W(K)=W(IXP)
      IF (AAA-ABS(W(K)/E(I))) 66,67,67
   66 AAA=ABS(W(K)/E(I))
   67 K=K+1
   65 CONTINUE
      DDMAG=1.
      W(N)=ESCALE/AAA
      ILINE=N
      GO TO 7
   37 IXP=JJ
      AAA=0.
      F=FHOLD
      DO 99 I=1,N
      IXP=IXP+1
      X(I)=X(I)-W(IXP)
      IF (AAA*ABS(E(I))-ABS(W(IXP))) 98,99,99
   98 AAA=ABS(W(IXP)/E(I))
   99 CONTINUE
      GO TO 72
   38 AAA=AAA*(1.+DI)
      GO TO (72,106),IND
   72 IF (IPRINT-2) 53,50,50
   53 GO TO (109,88),IND
  109 IF (AAA-0.1) 89,89,76
   89 GO TO (20,116),ICON
  116 IND=2
      GO TO (100,101),INN
  100 INN=2
      K=JJJ
      DO 102 I=1,N
      K=K+1
      W(K)=X(I)
      X(I)=X(I)+10.*E(I)
  102 CONTINUE
      FKEEP=F
      CALL CALCFX (N,X,F)
      NFCC=NFCC+1
      DDMAG=0.
      GO TO 108
   76 IF (F-FP) 35,78,78
   78 WRITE(6,80)
   80 FORMAT (5X,37HVA04A ACCURACY LIMITED BY ERRORS IN F)
      GO TO 20
   88 IND=1
   35 DDMAG=0.4*SQRT(FP-F)
      ISGRAD=1
  108 ITERC=ITERC+1
      IF (ITERC-MAXIT) 5,5,81
   81 WRITE(6,82) MAXIT
   82 FORMAT(I5,30H ITERATIONS COMPLETED BY VA04A)
      IF (F-FKEEP) 20,20,110
  110 F=FKEEP
      DO 111 I=1,N
      JJJ=JJJ+1
      X(I)=W(JJJ)
  111 CONTINUE
      GO TO 20
  101 JIL=1
      FP=FKEEP
      IF (F-FKEEP) 105,78,104
  104 JIL=2
      FP=F
      F=FKEEP
  105 IXP=JJ
      DO 113 I=1,N
      IXP=IXP+1
      K=IXP+N
      GO TO (114,115),JIL
  114 W(IXP)=W(K)
      GO TO 113
  115 W(IXP)=X(I)
      X(I)=W(K)
  113 CONTINUE
      JIL=2
      GO TO 92
  106 IF (AAA-0.1) 20,20,107
20      RETURN
  107 INN=1
      GO TO 35
      END
C
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
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 200 CZEIL2
      CTMP2=' '
      CTMP3=','
C
      ilen=len(czeile)
      DO 70 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 80
  70  CONTINUE
  80  CONTINUE
      IF(k.LT.1)k=1
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)') CZEILE(J:k)
C
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
C    1      k,j,CZEIL2(1:k)
C
C-----Was there a komma recently ?
      KWAR=0
C
      I=1
      L=1
 100  continue
        READ(CZEIL2(I:I),'(A1)')CTMP1
        IF(CTMP1.EQ.CTMP3)THEN
C---------There is a komma.
          IF(KWAR.EQ.0)THEN
C-----------There is a komma, before was no komma. Insert one.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a komma, before was already a komma. Shrink.
            L=L-1
          ENDIF
        ELSEIF(CTMP1.EQ.CTMP2)THEN
C---------There is a space.
          IF(KWAR.EQ.0)THEN
C-----------There is a space, before was no komma. Insert komma.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a space, before was a komma. Shrink.
            L=L-1
          ENDIF
        ELSE
C---------There is no komma, no space. Anulate KWAR, do nothing.
          WRITE(CZEILE(L:L),'(A1)')CZEIL2(I:I)
          KWAR=0
        ENDIF
        I=I+1
        L=L+1
C       WRITE(*,'('' INKOMMA 2b: I='',I4,'' L='',I3,'':'',A)')I,L,CTMP1
C
      IF(I.LE.K)GOTO 100
C
C     WRITE(*,'('' INKOMMA 3: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      IF(L.LE.K)THEN
        WRITE(CZEILE(L:K),'('' '')')
      ENDIF
C
C-----Now check, if the last sign is a komma. If so, remove it.
C
      K=L
      I=K
 200  continue
        READ(CZEILE(I:I),'(A1)')CTMP1
        IF(CTMP1.NE.CTMP2)GOTO 220
        I=I-1
      IF(I.GE.1)GOTO 200
 220  CONTINUE
C
      K=I
C
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C      WRITE(*,'('':: INKOMMA 4: k='',I4,'': "'',A,''"'')')k,CZEILE(1:k)
C
      RETURN
      END
C
c==========================================================

      function CALTLTAXA(TAXA,TANGL)
C
      implicit none
      real CALTLTAXA
      real TAXA,TANGL
      real DRAD,RDEG
      real TLTAXA,TLTANGL,RTAXA
      real RMIN,RNBEST,RBEST,RDIST
      integer i0,i1,ISTEP
C
      real CALTAXA
C
      DRAD = 3.141592654 / 180.0
      RDEG = 180.0 / 3.141592654
C
C
C-----TTREFINE calculates TAXA from TLTAXA in the following way:
C      DENOM = SQRT(1.0-(SIN(TLTAXA*DRAD)*SIN(TLTANGL*DRAD))**2)
C      TAXA = RDEG*ACOS((COS(TLTAXA*DRAD))/DENOM)
C      TAXA = SIGN(TAXA,TLTAXA)
C
C-----Here, we invert that function
C
      RMIN=1e10
      RNBEST=1e10
C
      RDIST = 90.0
      RBEST = 0.0
      ISTEP = 10
      do i0 = 1,8
        do i1 = -ISTEP,ISTEP
          TLTAXA = RBEST + i1 * RDIST / ISTEP
          RTAXA = CALTAXA(TLTAXA,TANGL)
          if(abs(RTAXA-TAXA).lt.RMIN)then
            RMIN=abs(RTAXA-TAXA)
            RNBEST=TLTAXA
          endif
        enddo
        RDIST = RDIST / ISTEP
        RBEST = RNBEST
      enddo
C   
      CALTLTAXA = RBEST
C
      return 
      end
C
C==============================================================
C
      function CALTAXA(TLTAXA,TLTANGL)
C
      implicit none
      real CALTAXA
      real TLTAXA,TLTANGL
      real DENOM,DRAD,RDEG,TAXA
C
      DRAD = 3.141592654 / 180.0
      RDEG = 180.0 / 3.141592654
C
C-----TTREFINE calculates TAXA from TLTAXA in the following way:
C      DENOM = SQRT(1.0-(SIN(TLTAXA*DRAD)*SIN(TLTANGL*DRAD))**2)
C      TAXA = RDEG*ACOS((COS(TLTAXA*DRAD))/DENOM)
C      TAXA = SIGN(TAXA,TLTAXA)
C
      DENOM = SQRT(1.0-(SIN(TLTAXA*DRAD)*SIN(TLTANGL*DRAD))**2)
      TAXA = RDEG*ACOS((COS(TLTAXA*DRAD))/DENOM)
      TAXA = SIGN(TAXA,TLTAXA)
C
      CALTAXA = TAXA
C
      return 
      end
C
