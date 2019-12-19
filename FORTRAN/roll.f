C Simulate the roll of a die
C Written by mike slattery - sept 1997
C
C23456789012345678901234
      PROGRAM ROLL

      INTEGER TIME, COUNT
C Use the standard function TIME() to seed the
C random number generator
      PRINT *, TIME()
      X = RAND(TIME())
C now roll the die 10 times...
      DO 5 I=1,10

      X = RAND(0)

C convert our random number between 0.0 and 1.0 to
C an integer between 1 and 6
      COUNT = 6*X+1.0
      PRINT *, X
      PRINT *, COUNT
5     CONTINUE
      END
