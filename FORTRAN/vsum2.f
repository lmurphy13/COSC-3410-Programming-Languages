      PROGRAM MAIN
      PARAMETER (MAXSIZ=99)
      REAL A(MAXSIZ)
100   READ *,K
      IF (K.LE.0 .OR. K.GT.MAXSIZ) STOP
      READ 50, (A(I),I=1,K)
50    FORMAT (99F3.1)
      PRINT 51, (A(I),I=1,K)
51    FORMAT (99(F4.1,1X))
      PRINT 52,VSUM(A,K)
52    FORMAT ('Sum=',F6.1)
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
