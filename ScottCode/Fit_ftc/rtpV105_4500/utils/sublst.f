C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    SUBLST
C
!F77====================================================================


!ROUTINE NAME:
C    CHKLST


!ABSTRACT:
C    Subset a list


!CALL PROTOCOL
C    SUBLST ( NMAST, LMAST, NSUB, LSUB, INDSUB )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NMAST   # items on master list      none
C    INT arr   LMAST   master list                 none (ID or index)
C    INTEGER   NSUB    # items on subset list      none
C    INT arr   LSUB    subset list                 none (ID or index)


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INT arr   INDSUB  indices of LSUB in LMAST    none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    SARTA_RTP


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incSUB.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 2001 version of the AIRS RTP KLAYERS+SARTA package.
C    Code by S.Hannon/H.Motteler/L.Strow.
C
C    Compares a subsetting list to a master list, and determines
C    the index in the master list of each item in the subset list.
C    Writes an error message and quits if an item in the subset
C    list is not found in the master list.  Both lists must be
C    sorted in ascending order!
C    


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    12 Mar 2001 Scott Hannon   Created


!END====================================================================

C      =================================================================
       SUBROUTINE SUBLST ( NMAST, LMAST, NSUB, LSUB, INDSUB )
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incSUB.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER  NMAST        ! number of items in master list
       INTEGER  LMAST(*)     ! master list
       INTEGER  NSUB         ! number of items in subset list
       INTEGER  LSUB(*)      ! subset list
C
C      Output
       INTEGER INDSUB(*)     ! indices of subset items in master


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER  ISUB
       INTEGER  IMAST
       INTEGER IPREVM
       INTEGER IPREVS


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
C      -------------------------------
C      Loop over the subset list items
C      -------------------------------
       IMAST=1  ! initialize index in master list
       IPREVM=-65535  ! initialize with some big negative number
       IPREVS=-65535
       DO ISUB=1,NSUB
C
C         Check that subset list is sorted in ascending order
C         and free of repeats
          IF (LSUB(ISUB) .LE. IPREVS) THEN
             WRITE(IOERR,1010) 'subset', ISUB, LSUB(ISUB)
 1010        FORMAT('ERROR! ',A6,' list item #',I6,'=',I6,/,
     $       'Not sorted in ascending order and free of repeats')
             STOP
          ENDIF

C         Find subset item in master list (using a "while" loop)
 10       IF (LSUB(ISUB) .EQ. LMAST(IMAST)) THEN
             INDSUB(ISUB)=IMAST
          ELSE
             IPREVM=LMAST(IMAST)  ! update IPREVM
             IMAST=IMAST + 1  ! increment index in master list
C
C            Check that master list is sorted in ascending order
C            and free of repeats.
             IF (LMAST(IMAST) .LE. IPREVM) THEN
                WRITE(IOERR,1010) 'master', IMAST, LMAST(IMAST)
             ENDIF
C
             GOTO 10
          ENDIF
C
          IPREVS=LSUB(ISUB)  ! update IPREVS
C
       ENDDO
C
       RETURN
       END
