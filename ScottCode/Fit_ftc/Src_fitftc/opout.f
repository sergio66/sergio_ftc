C=======================================================================
C=======================================================================
C
C
C        UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C        AIRS
C
C        OPOUT
C
C
!F77====================================================================


!ROUTINE NAME:
C    OPOUT


!ABSTRACT: 
C    Opens the output file for the fast transmittance coefficients.


!CALL PROTOCOL:
C    OPOUT(IOOUT)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IOOUT          File I/O unit no.  none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    PREP


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    INPUT:
C       I/O Unit 5 (keyboard) : enter filename
C    OUTPUT:
C       I/O Unit IOOUT : fast trans coef output file
C       I/O Unit 6 (screen) : prompt for filename
C       I/O unit IOERR: error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine OPOUT asks for a filename for the fast transmittance
C    coefficient file that is created by the main program. It halts
C    the program if it has trouble openning the file. The file 
C    remains open on exit.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer      Comments
C    --------- --------------- -----------------------------------------
C    12 Dec 96 Scott Hannon    Created
C    21 May 02 Scott Hannon    Error messages now use IOERR (was 6);
C                              add include 'farray.f' (for IOERR)


!END ===================================================================


C      =================================================================
       SUBROUTINE  OPOUT(IOOUT)
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
       INTEGER IOOUT
C
C      Output parameters
C    none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER IERR
       CHARACTER*79 FNAM


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
C      Ask for the name of the output file to create
       WRITE(6,1010)
 1010  FORMAT('Enter name of fast trans coef output file to create')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
C
C      Open the file and make sure it openned OK
       OPEN(UNIT=IOOUT,FILE=FNAM,STATUS='NEW',FORM='UNFORMATTED',
     $ IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR,FNAM
 1020     FORMAT('Error ',I4,' openning file:',/,A79)
          STOP
       ENDIF
C
       RETURN
       END
