C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    DOLIST
C
!F77====================================================================


!SUBROUTINE
C    DOLIST


!ABSTRACT:
C    Read in the AIRS fast transmittance coefficients and write
C    out lists for each coef set.

!CALL PROTOCOL
C    main program

!INPUT PARAMETERS:
C    main program

!OUTPUT PARAMETERS:
C    main program

!INPUT/OUTPUT PARAMETERS:
C    none

!RETURN VALUES:
C    none

!PARENT(S):
C    main program


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    unit IOIN : input file, binary FORTRAN data file. The file is
C       opened, read, and closed. This is done 9 times, once per
C       each of the 7 coef sets, and once each for the variable CO2,
C       OPTRAN water
C    unit IOOUT : output file, ASCII TEXT file. The file is
C       opened, written, and closed. This is done 9 times, once per
C       each of the 7 coef sets, and once each for the variable CO2,
C       OPTRAN water


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Based on rdcoef routine from the AIRS Fast Transmittance Code
C    by L.Strow/S.Hannon.
C
C    The seven data binary files containing the AIRS fast transmittance
C    coefficients are opened and read one channel at a time.  The CO2
C    and OPTRAN coef sets are also read.  As each set is read in, a
C    list file is created specifying chan ID and freq for each chan
C    in the set.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    10 Aug 2000 Scott Hannon   Create from "xrdcoef.f"
C    17 May 2002 Scott Hannon   change to include file


!END====================================================================

C      =================================================================
       PROGRAM DOLIST
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       INCLUDE 'farray_optran.f'
C      Note: always use "farray_optran.f"; need MAXLAY and NOWAVG to
C      read OPTRAN header.


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
C      File I/O
       CHARACTER*80 FILNAM
       INTEGER   IOIN
       INTEGER  IOOUT
       INTEGER   IERR

C      Channel counters
       INTEGER  NCHN1
       INTEGER  NCHN2
       INTEGER  NCHN3
       INTEGER  NCHN4
       INTEGER  NCHN5
       INTEGER  NCHN6
       INTEGER  NCHN7
       INTEGER  NCHNO
       INTEGER  NCHNC

C      Channel freq and ID
       INTEGER IDCHAN
       REAL   FREQ

C      Generic variables
       INTEGER     IC
       INTEGER     IL
       REAL  RJUNK

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      Assign I/O unit number
       IOIN=10
       IOOUT=11
C
C      ----------
C      Read set 1
C      ----------
       WRITE(6,1010) 1
 1010  FORMAT('Enter the name of the fast trans coef data file ',
     $    'for set ',I1,'.')
       READ(5,9000) FILNAM
 9000  FORMAT(A80)
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
 1020     FORMAT('Error ',I5,' openning file:',/,A80)
          STOP
       ENDIF
C
       WRITE(6,1015) 1
 1015  FORMAT('Enter the name of the output list file for set ',I1,'.')
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN1=0
 11    READ(IOIN,END=91) IDCHAN, FREQ
       NCHN1=NCHN1 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
 5010  FORMAT(I5,1X,F9.3)
       GOTO 11
C
 91    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 2
C      ----------
       WRITE(6,1010) 2
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 2
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN2=0
 12    READ(IOIN,END=92) IDCHAN, FREQ
       NCHN2=NCHN2 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 12
C
 92    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 3
C      ----------
       WRITE(6,1010) 3
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 3
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN3=0
 13    READ(IOIN,END=93) IDCHAN, FREQ
       NCHN3=NCHN3 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 13
C
 93    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 4
C      ----------
       WRITE(6,1010) 4
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 4
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN4=0
 14    READ(IOIN,END=94) IDCHAN, FREQ
       NCHN4=NCHN4 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 14
C
 94    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 5
C      ----------
       WRITE(6,1010) 5
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 5
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN5=0
 15    READ(IOIN,END=95) IDCHAN, FREQ
       NCHN5=NCHN5 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 15
C
 95    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 6
C      ----------
       WRITE(6,1010) 6
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 6
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN6=0
 16    READ(IOIN,END=96) IDCHAN, FREQ
       NCHN6=NCHN6 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 16
C
 96    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------
C      Read set 7
C      ----------
       WRITE(6,1010) 7
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1015) 7
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHN7=0
 17    READ(IOIN,END=97) IDCHAN, FREQ
       NCHN7=NCHN7 + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 17
C
 97    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ---------------------------
C      Read CO2 perturbation coefs
C      ---------------------------
       WRITE(6,1030)
 1030  FORMAT('Enter the name of the CO2 perturbation coef file')
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1033)
 1033  FORMAT('Enter the name of the output list file for CO2 pert')
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       NCHNC=0
 18    READ(IOIN,END=98) IDCHAN, FREQ
       NCHNC=NCHNC + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 18
C
 98    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ---------------------
C      Read OPTRAN H2O coefs
C      ---------------------
       WRITE(6,1035)
 1035  FORMAT('Enter the name of the OPTRAN H2O coef file')
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
       WRITE(6,1037)
 1037  FORMAT('Enter the name of the output list file for optran')
       READ(5,9000) FILNAM
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='FORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FILNAM
          STOP
       ENDIF
C
C      Read the header section
       READ(IOIN,END=99) (RJUNK,IL=1,MAXLAY)
       DO IC=1,NOWAVG
          READ(IOIN) (RJUNK,IL=1,MAXLAY)
       ENDDO
C
       NCHNO=0
 19    READ(IOIN,END=99) IDCHAN, FREQ
       NCHNO=NCHNO + 1
       WRITE(IOOUT,5010) IDCHAN, FREQ
       GOTO 19
C
 99    CLOSE(IOIN)
       CLOSE(IOOUT)
C
C
C      ----------------------------
C      Show summary of channel sets
C      ----------------------------
       WRITE(6,1060) 1, NCHN1
 1060  FORMAT('Number of channels for set',I1,' = ',I4)
       WRITE(6,1060) 2, NCHN2
       WRITE(6,1060) 3, NCHN3
       WRITE(6,1060) 4, NCHN4
       WRITE(6,1060) 5, NCHN5
       WRITE(6,1060) 6, NCHN6
       WRITE(6,1060) 7, NCHN7
       WRITE(6,1065) NCHNC, NCHNO
 1065  FORMAT('Number of channels for variable CO2 = ',I4,/,
     $        'Number of channels for optran water = ',I4)
C
       STOP
       END
