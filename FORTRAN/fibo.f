C	Fortran demo program
C	Print fibonacci numbers less than 200
C 
C	Written by mike slattery - sept, 2017
C23456789012345678901234

      PROGRAM FIBO
      I = 1
      J = 1
5     PRINT *, I
      K = I + J
      I = J
      J = K
      IF (I .LT. 200) GOTO 5
      END
