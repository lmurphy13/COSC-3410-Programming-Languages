      PROGRAM ODDEVEN
C Determine if a number is odd or even

C Written by mike slattery - oct 2019

100   PRINT *, 'ENTER AN INTEGER:'
      READ *, N
      IF (N .EQ. 9999) GOTO 400
C Next line is integer divide since both K and N are
C implicitly declared to be INTEGER
      K = N/2
      IF (N .EQ. 2*K) GOTO 200
      PRINT *, N, 'IS ODD'
      GOTO 300
200   PRINT *, N, 'IS EVEN'
300   PRINT *, '------------------'
      GOTO 100
400   END
