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
      qval =(  niq(1) * 7 * 2.5
     .       + niq(2) * 6 * 2.0
     .       + niq(3) * 5 * 1.6
     .       + niq(4) * 4 * 1.3
     .       + niq(5) * 3 * 1.1
     .       + niq(6) * 2 * 1 
     .       + niq(7) * 1 * 0 
     .       + niq(8) * 0 
     .       + niq(9) * 0  )
     .       * rscamax / 500.0
        return
        end
