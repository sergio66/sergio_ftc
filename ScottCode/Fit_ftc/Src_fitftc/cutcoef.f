C
       PROGRAM CutCoef
C
C      Cut out a portion of fast transmittance data file.
C      Edit the number of channels and coefs to suit the file.
C
       PARAMETER (MAXLEV=100, MAXCOF=29, NCHAN=117)
C
       REAL SFREQ,EFREQ
C
       CHARACTER*1 YNANS
       CHARACTER*79 FNAM
       REAL FREQ, COEF(MAXCOF,MAXLEV)
       INTEGER I, J, IC, IL
C
C***************************100***************************************
C Input the fast trans coefs
C
       WRITE(6,1000)
 1000  FORMAT('Enter start and end freqs of region to pull out')
       READ(5,*) SFREQ,EFREQ
       WRITE(6,1005)
 1005  FORMAT('Enter name of output file to create')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
       OPEN(UNIT=9,FILE=FNAM,FORM='UNFORMATTED',STATUS='NEW')
       WRITE(6,1010)
 1010  FORMAT('Enter the name of the file with the coefs')
       READ(5,9000) FNAM
       OPEN(UNIT=99,FILE=FNAM,FORM='UNFORMATTED',STATUS='OLD')
C
C      Read all the fast trans coefs
       J=0
       DO 100 I=1,NCHAN
C         Read data for this frequency/channel
          READ(99) FREQ,((COEF(IC,IL),IC=1,MAXCOF),IL=1,MAXLEV)
C
          IF ((FREQ .GT. SFREQ) .AND. (FREQ .LT. EFREQ)) THEN
             J=J+1
             WRITE(9) FREQ,((COEF(IC,IL),IC=1,MAXCOF),IL=1,MAXLEV)
          ENDIF
C
 100   CONTINUE
C
       WRITE(6,1020) J, SFREQ, EFREQ
 1020  FORMAT('Output = input cut to ',I4,' points for region ',F12.4,
     + ' to ',F12.4)
C
C      Close the fast trans coefs files
       CLOSE(9)
       CLOSE(99)
C
       STOP
       END
