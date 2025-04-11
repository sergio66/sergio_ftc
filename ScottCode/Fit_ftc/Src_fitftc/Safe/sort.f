C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             SORT
C
!F77====================================================================


!ROUTINE NAME:
C    SORT


!ABSTRACT:
C    Sort gas info from IGASi and IBOKi to assign ISORTK and LSTGAS.
C    It then checks that IDGAS includes all gases in LSTGAS and
C    assigns ISORTA.


!CALL PROTOCOL:
C    SORT(IDGAS, LSTGAS, ISORTA, ISORTK)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    IDGAS          data file gas IDs  none (HITRAN mol ID)


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    LSTGAS         desired gas IDs    none
C    INT ARR    ISORTA         gas ind for amount none
C    INT ARR    ISORTK         gas indices for k  none


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
C    OUTPUT:
C        I/O unit IOERR: error message (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine SORT sorts the gas indices according to the order
C    in which the gases are to be broken out.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    15 Jan 97 Scott Hannon      Created
C    26 Aug 99 Scott Hannon      Total re-write
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE SORT(IDGAS, LSTGAS, ISORTA, ISORTK)
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
       INTEGER IDGAS(NUMSET)
C
C      Output parameters
       INTEGER LSTGAS(MAXBOG)
       INTEGER ISORTA(MAXBOG)
       INTEGER ISORTK(MAXBOG)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER     IG
       INTEGER   IGAS


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C    none


C***********************************************************************
C***********************************************************************
C                            EXECUTABLE CODE
C***********************************************************************
C***********************************************************************

C      -----------
C      Check MAXBOG
C      This routine is currently set up for MAXBOG=6
       IF (MAXBOG .NE. 6) THEN
          WRITE(IOERR,1010) MAXBOG
 1010     FORMAT('Error: "sort.f" is currently set up for MAXBOG=6'
     $    ', but "farray.f"',/,'has MAXBOG=',I2)
          STOP
       ENDIF


C      ------------
C      Check NUMSET
       IF (NUMSET .LT. NUMGAS) THEN
          WRITE(IOERR,1020) NUMSET, NUMGAS
 1020     FORMAT('Error, NUMSET=',I2,' is wrong (need NUMSET >=',I2').'
     $    /,'Check the conv trans data to determine the correct size'
     $    ' and then edit',/,'"farray.f" to correct NUMSET.  Also check'
     $    ' that IBOKi variables are OK.')
          STOP
       ENDIF
C

C      ------------
C      Check NUMGAS
       IF (NUMGAS .LT. 1) THEN
          WRITE(IOERR,1030) NUMGAS, MAXBOG
 1030     FORMAT('Error, NUMGAS=',I2,' is wrong.  Must be 1 to',I2,'.'
     $    /,'Consider what fit you are doing (FWO, FMW, FCOW, etc) to'
     $    ' determine the',/,'correct value, and then edit "farray.f"'
     $    ' to correct NUMGAS.  Also check',/,'that the IGASi '
     $    'variables are OK.')
          STOP
       ENDIF
       IF (NUMGAS .GT. MAXBOG) THEN
          WRITE(IOERR,1040) NUMGAS, MAXBOG
 1040     FORMAT('Error, NUMGAS=',I2,' is too big (max size is',I2,
     $    ').',/,'If this value is correct then it is necessary to '
     $    'modify "sort.f" and',/,'"farray.f".')
          STOP
       ENDIF
C

C      -------------
C      Assign ISORTK
       ISORTK(1)=IBOK1
       IF(NUMGAS .GE. 2) ISORTK(2)=IBOK2
       IF(NUMGAS .GE. 3) ISORTK(3)=IBOK3
       IF(NUMGAS .GE. 4) ISORTK(4)=IBOK4
       IF(NUMGAS .GE. 5) ISORTK(5)=IBOK5
       IF(NUMGAS .GE. 6) ISORTK(6)=IBOK6
C

C      -------------
C      Assign LSTGAS
       LSTGAS(1)=IGAS1
       IF(NUMGAS .GE. 2) LSTGAS(2)=IGAS2
       IF(NUMGAS .GE. 3) LSTGAS(3)=IGAS3
       IF(NUMGAS .GE. 4) LSTGAS(4)=IGAS4
       IF(NUMGAS .GE. 5) LSTGAS(5)=IGAS5
       IF(NUMGAS .GE. 6) LSTGAS(6)=IGAS6
C

C      ---------------------------
C      Check IDGAS & assign ISORTA
       DO IGAS=1,NUMGAS
C         Initialize ISORTA with error flag
          ISORTA(IGAS)=-1
C
C         Loop over gases in file
          DO IG=1,NUMSET
             IF (IDGAS(IG) .EQ. LSTGAS(IGAS)) ISORTA(IGAS)=IG
          ENDDO
C
C         Check that the gas was found
          IF (ISORTA(IGAS) .LT. 1) THEN
             WRITE(IOERR,1060) LSTGAS(IGAS), (IDGAS(IG),IG=1,NUMSET) 
 1060        FORMAT('Error: did not find gas ID ',I2,
     $       ' in conv data file.',/,
     $       'Conv trans data files contain gas IDs:',6(X,I3))
             STOP
          ENDIF
       ENDDO
C

       RETURN
       END
