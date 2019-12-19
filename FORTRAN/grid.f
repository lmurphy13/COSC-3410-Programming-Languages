      PROGRAM GRID
C Simple program to average values across a grid (e.g. simple
C heat distribution)
C
C Written by mike slattery - may 1996
      INTEGER SIZE
      PARAMETER (SIZE=10)
      REAL G(SIZE,SIZE)
      LOGICAL LOCK(SIZE,SIZE)
C The array G holds the current grid values and LOCK(I,J) is .TRUE.
C if G(I,J) should not be changed in SMOOTH.
C We fix the outer cells at constant zero.
      CALL INIT(G,LOCK,SIZE)
      CALL SETVAL(G,LOCK,SIZE,3,4,12.5)
      CALL SETVAL(G,LOCK,SIZE,6,6,9.2)
      DO 5 I=1,10
      CALL SMOOTH(G,LOCK,SIZE)
      CALL DISP(G,SIZE)
5     CONTINUE
      END
C
      SUBROUTINE INIT(A,L,N)
      REAL A(N,N)
      LOGICAL L(N,N)
      DO 10 I=1,N
      DO 10 J=1,N
      A(I,J)=0.0
      L(I,J)=.FALSE.
10    CONTINUE
      RETURN
      END
C
      SUBROUTINE SETVAL(A,L,N,I,J,V)
      REAL A(N,N)
      LOGICAL L(N,N)
      A(I,J)=V
      L(I,J)=.TRUE.
      RETURN
      END
C
      SUBROUTINE SMOOTH(A,L,N)
      REAL A(N,N)
      LOGICAL L(N,N)
      DO 5 I=2,N-1
      DO 5 J=2,N-1
      IF (L(I,J)) GOTO 5
      S = A(I-1,J)+A(I,J-1)+A(I+1,J)+A(I,J+1)
      A(I,J) = S/4.0
5     CONTINUE
      RETURN
      END
C
      SUBROUTINE DISP(A,N)
      REAL A(N,N)
      DO 7 J=2,N-1
      WRITE (*,30) (A(I,J),I=2,N-1)
30    FORMAT(50F6.2)
7     CONTINUE
      PRINT *,' '
      RETURN
      END
