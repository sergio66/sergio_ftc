C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             GETGRD
C
!F77====================================================================


!ROUTINE NAME:
C    GETGRD


!ABSTRACT:
C    Read the OPTRAN level-to-space amount grid from a text file


!CALL PROTOCOL:
C    GETGRD(IOIN, IDWANT, AZOP)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IOIN           Text file unit no. none
C    INTEGER    IDWANT         Gas ID no.         none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   AZOP           l-to-s amount grid kmol/cm^2


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITOPTRAN


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    INPUT:
C       IOIN: text file with grid data. File may INCLUDE comment lines
C          (any line starting with a "!").  First non-comment line of
C          file must be:
C             numgas  gasID_1 ...  gasID_numgas
C          The remaining non-comment lines must be
C             lev  azop_1(lev) ...  azop_numgas(lev)
C          where lev goes from 1 to NOPLEV.


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine GETGRD asks for a file name, opens the file, and reads
C    the grid data for the desired gas.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    31 Aug 99 Scott Hannon      Re-written for Aug99 AIRS production
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE GETGRD(IOIN, IDWANT, AZOP)
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       INCLUDE 'farray.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C    none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters
       INTEGER   IOIN
       INTEGER IDWANT
C
C      Output parameters
       REAL  AZOP(NOPLEV)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C      Local variables
       INTEGER      I
       INTEGER   IERR
       INTEGER   IGAS
       INTEGER  IJUNK
       INTEGER   ILAY
       INTEGER      L
       INTEGER   NGAS
       INTEGER IDLIST(MAXBOG)
C
       REAL  RJUNK(MAXBOG)
C
       CHARACTER*79   FNAM
       CHARACTER*120  CLINE


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C    none


C***********************************************************************
C***********************************************************************
C                            EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C
C      ------------------------------
C      Open file containing grid data
C
C      Get name of grid file
       WRITE(6,1010)
 1010  FORMAT('Enter name of file with OPTRAN amount-above grid')
       READ(5,5000) FNAM
 5000  FORMAT(A79)
C
C      Open grid file
       OPEN(UNIT=IOIN,FILE=FNAM,STATUS='OLD',FORM='FORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR,FNAM
 1020     FORMAT('Error ',I5, 'opening file:',/,A79)
          STOP
       ENDIF
C

C      --------------
C      Read grid file
       L=0
C
C      Skip any comment lines
 10    READ(IOIN,5010,END=99) CLINE
 5010  FORMAT(A120)
       IF (CLINE(1:1) .EQ. '!') GOTO 10
C
       IF (L .EQ. 0) THEN
C         Read first non-comment line
          READ(CLINE,*) NGAS
          IF ((NGAS .LT. 1) .OR. (NGAS .GT. MAXBOG)) THEN
             WRITE(IOERR,1030) FNAM, NGAS, MAXBOG, NGAS
 1030        FORMAT('Error in getgrd reading file:',/,A79,
     $       'Number of gases=',I2,' is out of expected range=1 to',I2)
             STOP
          ENDIF
C         Re-read CLINE, this time also read gas IDs
          READ(CLINE,*) IJUNK, (IDLIST(I),I=1,NGAS)
C
C         Find IDWANT in IDLIST
          IGAS=-1
          DO I=1,NGAS
             IF (IDLIST(I) .EQ. IDWANT) IGAS=I
          ENDDO
          IF (IGAS .LT. 1) THEN
             WRITE(IOERR,1040) IDWANT, FNAM
 1040        FORMAT('Error in getgrd, did not find gas ID ',I2,
     $       ' in file:',/,A79)
             STOP
          ENDIF
C
       ELSE
C         Read grid info for current layer
          READ(CLINE,*) ILAY, (RJUNK(I),I=1,NGAS)
          IF ((ILAY .EQ. L) .AND. (L .LE. NOPLEV)) THEN
             AZOP(L)=RJUNK(IGAS)
          ELSE
             WRITE(IOERR,1020) NOPLEV, FNAM
 1050        FORMAT('Error in getgrd, did not find expected OPTRAN'
     $       ' levels 1 thru ',I4,' in file:',/,A79)
             STOP
          ENDIF
       ENDIF
C
       L=L + 1
       GOTO 10
 99    CLOSE(IOIN)
C

       RETURN
       END
