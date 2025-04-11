C
       PROGRAM OPTHEADER
C
C      This program creates a header for the OPTRAN coef files.
C      The header contains info about the OPTRAN layering grid
C      and some average predictor vlues (used to help nomalize
C      the predictors).
C
C      Update: 17 May 2002, Scott Hannon - change to ue include file.
C

C-----------------------------------------------------------------------
C      INCLUDE files
C-----------------------------------------------------------------------
       INCLUDE 'farray_optran.f'


C-----------------------------------------------------------------------
C      Local Variables
C-----------------------------------------------------------------------
C
       CHARACTER*80  FNAME  ! File name
       CHARACTER*255 CLINE  ! Line of characters read from a text file
       INTEGER   IOIN       ! Unit number for input
       INTEGER  IOOUT       ! Unit number for output
       INTEGER   IERR       ! I/O error number
       REAL  WAZOP(MAXLAY)        ! Optran water grid
       REAL WAVGOP(NOWAVG,MAXLAY) ! Optran water average predictors
       INTEGER      I  ! generic looping variable
       INTEGER      J  ! generic looping variable
       INTEGER      L  ! layer looping variable
       INTEGER   NGAS         ! number of gases in optran grid file
       INTEGER  IDGAS(MAXGAS) ! IDs of gases in optran grid file
       INTEGER   IWAT         ! Index of water in IDGAS
       REAL AZGRID(MAXGAS)    ! one grid point per gas in grid file


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      EXECUTABLE CODE begins below
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C      Assign unit numbers for I/O
       IOIN=10
       IOOUT=11
C
C      -------------------------
C      Read the OPTRAN grid file
C      -------------------------
C      Open the file
       WRITE(6,1010)
 1010  FORMAT('Enter name of file containing optran water grid')
       READ(5,5010) FNAME
 5010  FORMAT(A80)
       OPEN(UNIT=IOIN,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAME
 1020     FORMAT('I/O Error number',I6,' openning file:',/,A80)
          STOP
       ENDIF
C
C      Read the header; skip comments
 10    READ(IOIN,5020) CLINE
 5020  FORMAT(A255)
       IF (CLINE(1:1) .EQ. '!' .OR. CLINE(1:1) .EQ. '%') GOTO 10
C
C      Read the gas info
       READ(CLINE,*) NGAS
       READ(CLINE,*) I, (IDGAS(J),J=1,NGAS)
C
C      Find water (gas ID 1)
       IWAT=0
       DO I=1,NGAS
          IF (IDGAS(I) .EQ. 1) IWAT=I
       ENDDO
       IF (IWAT .EQ. 0) THEN
          WRITE(IOERR,1030) FNAME
 1030     FORMAT('Error: did not find water in grid file:',/,A80)
          STOP
       ENDIF
C
C      Read the water grid data
       DO L=1,MAXLAY
          READ(IOIN,*) J, (AZGRID(I),I=1,NOWAVG)
          WAZOP(L)=AZGRID(IWAT)
       ENDDO
C
C      Close file
       CLOSE(IOIN)
C
C      -------------------------------
C      Read the average predictor file
C      -------------------------------
       WRITE(6,2010)
 2010  FORMAT('Enter name of average predictor file')
       READ(5,5010) FNAME
       OPEN(UNIT=IOIN,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNAME
          STOP
       ENDIF
C
C      Read the average predictor data
       DO L=1,MAXLAY
          READ(IOIN,*) (WAVGOP(I,L),I=1,NOWAVG)
       ENDDO
C
C      Close file
       CLOSE(IOIN)
C
C      -----------------------------------
C      Write the binary optran header file
C      -----------------------------------
C      Open the output file
       WRITE(6,3010)
 3010  FORMAT('Enter name of optran header file to create')
       READ(5,5010) FNAME
       OPEN(UNIT=IOOUT,FILE=FNAME,STATUS='NEW',FORM='UNFORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNAME
          STOP
       ENDIF
C
C      Write the header data
       WRITE(IOOUT) (WAZOP(L),L=1,MAXLAY)
       DO I=1,NOWAVG
          WRITE(IOOUT) (WAVGOP(I,L),L=1,MAXLAY)
       ENDDO
C
C      Close file
       CLOSE(IOOUT)
C
       STOP
       END
