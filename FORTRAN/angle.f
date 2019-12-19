C Assignment 4
C Written by: Liam Murphy
       PROGRAM MAIN
       PARAMETER (MAXSIZ=99)
       REAL V(MAXSIZ)
       REAL W(MAXSIZ)
       REAL ANGLER
       REAL ANGLED
       ANGLER = 0.0
       ANGLED = 0.0
100    PRINT *, 'ENTER VECTOR LENGTHS:'
       READ *, K
       IF (K .LE. 0 .OR. K .GT. MAXSIZ) STOP
       PRINT *, 'ENTER FIRST VECTOR'
       READ *, (V(I), I=1, K)
C
       PRINT *, 'ENTER SECOND VECTOR'
       READ *, (W(I), I=1, K)
C      
       PRINT *, ACOS( ((DOT(V,W,K)) / (ALEN(V,K) * ALEN(W,K))) )
       ANGLER = ACOS( (DOT(V,W,K)) / (ALEN(V,K) * ALEN(W,K)) )
       ANGLED = ANGLER * (180.0/3.14159265359)
C      
       PRINT *, "VECTOR 1:"
       DO 130 I=1, K
       PRINT *, V(I)
130    CONTINUE
C
       PRINT *, "VECTOR 2:"
       DO 140 I=1, K
       PRINT *, W(I)
140    CONTINUE
C
       PRINT 150, ANGLER
       PRINT 151, ANGLED
150    FORMAT ('ANGLE IN RADIANS: ', F9.3)
151    FORMAT ('ANGLE IN DEGREES: ', F9.3)
       GOTO 100
       END
C
       FUNCTION ALEN(VECT, N)
       REAL VECT(N)
       REAL SUM
       REAL VAL
       VAL = 0.0
       ALEN = 0.0
       SUM = 0.0
       DO 200 I=1, N
       VAL = VECT(I)
       SUM = SUM + (VAL * VAL)
200    CONTINUE
       ALEN = SQRT(SUM)
       RETURN
       END
C
       FUNCTION DOT(VECA, VECB, N)
       REAL VECA(N)
       REAL VECB(N)
       DOT = 0.0
       DO 300 I=1, N
300    DOT = DOT + (VECA(I) * VECB(I))
       RETURN
       END 