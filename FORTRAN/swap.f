C Fortran demo program
C
C Pass by reference
C 
C Written by mike slattery - sep, 2017

      PROGRAM PARAM
      I = 33
      J = 77
5     PRINT *, I, J
      CALL SWAP(I, J)
	  PRINT *, I, J
      END

	  SUBROUTINE SWAP(K,L)
	  ITEMP = K
	  K = L
	  L = ITEMP
	  END