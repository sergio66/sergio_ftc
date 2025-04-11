C
       PROGRAM REFORM
C
C      Re-format text file data
C
       INTEGER I1, I2, I3, I4, I5, I6, I7, I8, I9, I10
       DOUBLE PRECISION D1,D2,D3,D4,D5,D6,D7,D8,D9,D10, D11,D12,D13
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
       READ (CLINE,*) D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13
C
       I1=IDINT(D1)
C
       WRITE(6,1234) I1, D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13
 1234  FORMAT(I4, 3(1X,1PE12.5), 1X,0PF8.3, 8(1PE11.4))
C
C      ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       GOTO 10
C
 99    STOP
       END
