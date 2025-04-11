C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              incSUB
C
!F77====================================================================

!ROUTINE NAME: incSUB

!ABSTRACT:
C    Include file for SUBSET program and its routines

!CALL PROTOCOL: none

!INPUT PARAMETERS: none

!OUTPUT PARAMETERS: none

!INPUT/OUTPUT PARAMETERS: none

!RETURN VALUES: none

!PARENT(S):
C    SUBSET

!ROUTINES CALLED: none

!FILES ACCESSED: none

!COMMON BLOCKS: none

!DESCRIPTION:
C    Declare some array dimensions

!ALGORITHM REFERENCES: see DESCRIPTION

!KNOWN BUGS AND LIMITATIONS: none

!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 13 Mar 2001 Scott Hannon      created based on incLAY for klayers


!END====================================================================

C      =================================================================
C      INCLUDE FILE incSUB
C      =================================================================
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
c       IMPLICIT NONE
c some stupid compilers choke on implicit none if it appears in both
c the include and the main code.

C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none (see description in body of code)

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      EXECUTABLE CODE
C-----------------------------------------------------------------------
C      Declarations and parameters only

C      -----------------------------------------------------------------
C      Assign RTP related max sizes
C      -----------------------------------------------------------------
       INTEGER MXCHAN          ! max number of channels
       INTEGER MXPROF          ! max number of profiles
       INTEGER MXGAS           ! max number of gases
       PARAMETER(MXCHAN = 2378)
       PARAMETER(MXPROF = 20000)
       PARAMETER( MXGAS = 44)
C
C      -----------------------------------------------------------------
C      Assign unit numbers for error and info/warning messages
C      -----------------------------------------------------------------
       INTEGER IOERR
       INTEGER IOINFO
       PARAMETER(IOERR = 6)
       PARAMETER(IOINFO = 6)
C
C      End of "incSUB.f" include file
