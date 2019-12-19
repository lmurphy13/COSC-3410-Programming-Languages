C SUBPROGRAMS
C FUNCTION (returns a value)
C SUBROUTINE (no val)
C23456789012345678901234
      PROGRAM MAIN
      PARAMETER (MAXSIZ=99)
      REAL A(MAXSIZ)
100   READ *,K
      IF (K.LE.0 .OR. K.GT.MAXSIZ) STOP
      READ *, (A(I), I=1, K)
      PRINT *, (A(I), I=1, K)
      PRINT *, 'SUM=',VSUM(A,K)
      GOTO 100
      END
C
      FUNCTION VSUM(V,N)
      REAL V(N)
      VSUM=0.0
      DO 200 I=1,N
200   VSUM = VSUM+V(I)
      RETURN
      END
