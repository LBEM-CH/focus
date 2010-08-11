c	ODDREMOVE.FOR removes odd in-axis hkl values for p4212 symmetry.

C       Henning Stahlberg, 11.8.98
C       (partly copied from Per Bullough's "centric.f")



      CHARACTER*80  TITLE 
      INTEGER H,K,L

      READ (10,100) TITLE
      WRITE (11,100) TITLE
      WRITE (12,100) TITLE


1000  continue
      
      READ (10,200,END=1010) H,K,L,AMP,PHASE,FOM
      	
      if(h.eq.0)then
        itmp=k/2
        if(itmp*2.ne.k) goto 1000
      endif

      if(k.eq.0)then
        itmp=h/2
        if(itmp*2.ne.h) goto 1000
      endif
      	
      WRITE (11,200) H,K,L,AMP,PHASE,FOM
      WRITE (12,210) H,K,AMP,PHASE,FOM
      GO TO 1000
1010  CONTINUE

      STOP

100   FORMAT (A50)
200   FORMAT (3I4,2F8.1,F8.3)
210   FORMAT (2I4,2F8.1,F8.3)
 
      END     
