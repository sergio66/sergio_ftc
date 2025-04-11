C
       PROGRAM REFORM
C
C      Re-format text file data
C
       INTEGER I1, I2, I3, I4, I5, I6, I7, I8, I9, I10
       DOUBLE PRECISION D1, D2, D3, D4, D5, D6, D7, D8, D9, D10
       CHARACTER*256 CLINE
C
C      *****************************************************************
C
 10    READ(5,5010,END=99) CLINE
 5010  FORMAT(A256)
C
C      Ignore any line starting with a "!" or "%")
       IF ( (CLINE(1:1) .EQ. '!') .OR. (CLINE(1:1) .EQ. '%') ) THEN
          GOTO 10
       ENDIF
C
C      ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C      Edit this block as needed
C      ccccccccccccccccccccccccc
C
c       READ (CLINE,*) D1, D2, D3, D4
       READ (CLINE,*) D1, D2
C
       I1=IDINT(D1)
C
c      pnum, Tsurf, Psurf, angle
c       WRITE(6,1234) I1, D2, D3, D4
c 1234  FORMAT(I5, 1X, F8.3, 1X, F8.2, 1X, F8.3)
       WRITE(6,1234) I1, D2
 1234  FORMAT(I5, 1X, F8.2)
C
C      ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       GOTO 10
C
 99    STOP
       END
