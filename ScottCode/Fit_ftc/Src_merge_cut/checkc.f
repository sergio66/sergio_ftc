C
       PROGRAM CHECKC
C
       IMPLICIT NONE
C
       INTEGER MAXLAY
       PARAMETER( MAXLAY = 100 )
C
       INTEGER NPRED1
       INTEGER NPRED2
       INTEGER NPRED3
       INTEGER NPRED4
       INTEGER NPRED5
       INTEGER NPRED6
       INTEGER NPRED7
       PARAMETER( NPRED1 = 5 +  8 + 11 +  5 )     ! set1
       PARAMETER( NPRED2 = 5 +  8 + 11 + 10 )     ! set2 ozone
       PARAMETER( NPRED3 = 5 +  8 + 11 +  9 )     ! set3 methane
       PARAMETER( NPRED4 = 5 + 11 + 11 + 13 + 3 ) ! set4 fcow
       PARAMETER( NPRED5 = 5 + 11 +  3 +  1 )     ! set5 bfsw
       PARAMETER( NPRED6 = 5 +  8 +  7 +  1 )     ! set6 mfmw
       PARAMETER( NPRED7 = 5 +  8 + 13 +  1 )     ! set7 mfbw
       INTEGER MAXPRD
       PARAMETER(MAXPRD=NPRED4)
C
       INTEGER I, IC, IL, L, LL, K1, K2, ICHAN
       CHARACTER*1 YNANS
       CHARACTER*79 FNAM
       REAL FREQ, FTCOEF(MAXPRD,MAXLAY)
       LOGICAL VERT
       INTEGER ISET
       INTEGER NPRED
C
C*****************************100***************************************
C
C      Get set number
       WRITE(6,1000)
 1000  FORMAT('Enter coef set number (1 to 7) : ')
       READ(5,*) ISET
       IF (ISET .EQ. 1) NPRED=NPRED1
       IF (ISET .EQ. 2) NPRED=NPRED2
       IF (ISET .EQ. 3) NPRED=NPRED3
       IF (ISET .EQ. 4) NPRED=NPRED4
       IF (ISET .EQ. 5) NPRED=NPRED5
       IF (ISET .EQ. 6) NPRED=NPRED6
       IF (ISET .EQ. 7) NPRED=NPRED7
       IF (ISET .LT. 1 .OR. ISET .GT. 7) THEN
          WRITE(6,1005) ISET
 1005     FORMAT('Error: set =',I7,' is out of range.')
          STOP
       ELSE
          WRITE(6,1007) ISET, NPRED
 1007     FORMAT('Set',I2,' contains ',I2,' coefs')
       ENDIF
C
C      Determine whether to look horizontal or vertical
       WRITE(6,1010)
 1010  FORMAT('! Look vertically or horizontally {v or h}?')
       READ(5,9010) YNANS
 9010  FORMAT(A1)
       VERT=.FALSE.
       IF (YNANS .EQ. 'v' .OR. YNANS .EQ. 'V') VERT=.TRUE.
C
C      Get info about exactly what to look at
       IF (VERT) THEN
          WRITE(6,1020)
 1020     FORMAT('! Enter channel to look at, and 1st and last coef')
       ELSE
          WRITE(6,1030)
 1030     FORMAT('! Enter layer to look at, and 1st and last coef')
       ENDIF
       READ(5,*) L, K1, K2
C
C      Open the file with the fast trans coefs
       WRITE(6,1040)
 1040  FORMAT('! Enter the name of the file with the fast trans coefs')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
       OPEN(UNIT=9,FILE=FNAM,FORM='UNFORMATTED',STATUS='OLD')
C
C      ----------------------
C      Loop over the channels
C      ----------------------
       I=1
C      Read data for this frequency/channel
 10    READ(9,END=99) ICHAN, FREQ,
     $    ((FTCOEF(IC,IL),IC=1,NPRED),IL=1,MAXLAY)
C
       IF (VERT) THEN
C         Display data is current channel is the chosen channel
          IF (I .EQ. L) THEN
             WRITE(6,1050) ICHAN, FREQ
 1050        FORMAT('! channel ', I4, ', center freq=',F10.4)
C
             DO LL=1,MAXLAY
                WRITE(6,1060) LL, (FTCOEF(IC,LL),IC=K1,K2)
 1060           FORMAT(I4,38(' ',1PE11.4))
             ENDDO
          ENDIF
       ELSE
C         Display data for layer L of current channel
          WRITE(6,1070) FREQ, (FTCOEF(IC,L),IC=K1,K2)
 1070     FORMAT(F10.4,30(' ',1PE11.4))
       ENDIF
C
       I=I+1
       GOTO 10
C
C      Close the fast trans coefs files
 99    CLOSE(9)
C      End of loop over channels
C      -------------------------
C
       STOP
       END
