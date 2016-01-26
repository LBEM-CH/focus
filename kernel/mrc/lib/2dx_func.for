C***************************************************
C*** set of routines for 2dx
C***************************************************
C
C
C
C**************************************************************
        real function qval(niq,rscamax)
C***************************************************************
C*** function to return the QVal that should be a measure for the 
C*** performance of the image processing.
C
      integer niq(9)
      real rscamax
C
      i = 3
C
      if ( i .eq. 1 ) then
        qval =(  niq(1) * 7 * 2.5
     .         + niq(2) * 6 * 2.0
     .         + niq(3) * 5 * 1.6
     .         + niq(4) * 4 * 1.3
     .         + niq(5) * 3 * 1.1
     .         + niq(6) * 2 * 1 
     .         + niq(7) * 1 * 0 
     .         + niq(8) * 0 
     .         + niq(9) * 0  )
     .         * rscamax / 500.0
      else if ( i.eq.2 ) then
        qval =(  niq(1) * 49.00
     .         + niq(2) * 27.56
     .         + niq(3) *  8.51
     .         + niq(4) *  4.17
     .         + niq(5) *  2.48
     .         + niq(6) *  1.65
     .         + niq(7) *  1.17
     .         + niq(8) *  0.25
     .         + niq(9) *  0.00 )
     .         / 10.0
      else if ( i.eq. 3) then
        qval =(  niq(1) * 7.0
     .         + niq(2) * 3.5
     .         + niq(3) * 2.3
     .         + niq(4) * 1.7
     .         + niq(5) * 1.4
     .         + niq(6) * 1.15
     .         + niq(7) * 1.0
     .         + niq(8) * 0.5
     .         + niq(9) * 0.0 )
     .         / 5.0
      else if ( i.eq. 4 ) then
          qval = rscamax * 10.0
      endif

      return
      end
C
