C
       PROGRAM NANTEST
C
C      Test for NaN trap
C      Discovered if Absoft compiled with "-O" option, then
C      then NaN trap fails (is invalid data is allowed to pass).
C
       INTEGER I
       DOUBLE PRECISION X, Y, Z, ZMIN, ZMAX
       REAL S
C
       ZMIN=5.0D-7
       ZMAX=1.0D+2
C
       I=1
 10    WRITE(6,1020) I
 1020  FORMAT('------------ test',I3, '---------------------')
       WRITE(6,1030) 'X'
 1030  FORMAT('Enter ',A1)
       READ(5,*) X
       WRITE(6,1030) 'Y'
       READ(5,*) Y
       Z=X/Y
       S=SNGL(Z)
       IF ((Z .GT. ZMIN) .AND. (Z .LT. ZMAX)) THEN
          WRITE(6,1000) I,'passed'
 1000     FORMAT('test',I2,', ',A6)
       ELSE
          WRITE(6,1000) I,'failed'
       ENDIF
       WRITE(6,1010) X,Y,Z,S
 1010  FORMAT('X,Y,Z=X/Y=S(single):',4(1X,1PE12.5))
       IF (X .GT. 0.0) GOTO 10
C
 99    STOP
       END
