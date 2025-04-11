C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             CHECK1
C
!F77====================================================================


!ROUTINE NAME:
C    CHECK1


!ABSTRACT:
C    Check the header info from a convolved transmittance data file.


!CALL PROTOCOL:
C    CHECK1(IPROF, NCHCHK, IDGCHK, SECCHK, NCHAN, IDGAS, SECANG)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IPROF          Profile number     none
C    INTEGER    NCHCHK         Expected no. chans none
C    INT ARR    IDGCHK         Expected gas IDs   none (HITRAN mol ID)
C    REAL ARR   SECCHK         Expected secants   none
C    INTEGER    NCHAN          Test no. chans     none
C    INT ARR    IDGAS          Test gas IDs       none (HITRAN mol ID)
C    REAL ARR   SECANG         Test secants       none


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
C    OUTPUT:
C        I/O unit IOERR : error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine CHECK1 checks some of the header info from a convolved
C    transmittance data file against the expected values. The routine
C    stops the program if it detects a problem.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    12 Dec 96 Scott Hannon      Created
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE CHECK1(IPROF, NCHCHK, IDGCHK, SECCHK, NCHAN, IDGAS,
     $    SECANG)
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
       INTEGER IPROF, NCHCHK, IDGCHK(NUMSET), NCHAN, IDGAS(NUMSET)
       REAL SECCHK(MAXANG), SECANG(MAXANG)
C
C      Output parameters
C    none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER I
       REAL RTINY


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
C      Assign accuracy requirement for comparing secants
       RTINY=1.0E-5
C
C      Check the number of channels
       IF (NCHAN .NE. NCHCHK) THEN
          WRITE(IOERR,1010) IPROF, NCHAN, NCHCHK
 1010     FORMAT('Error, unexpected number of channels in profile ',
     $       I3,' conv trans data file.',/,'File has ',I3,
     $       ' channels, expecting ',I3,'.')
          STOP
       ENDIF
C
C      Check IDGAS
       DO I=1,NUMSET
          IF (IDGAS(I) .NE. IDGCHK(I)) THEN
             WRITE(IOERR,1030) IPROF, I, IDGAS(I), IDGCHK(I)
 1030        FORMAT('Error, unexpected gas conv data in profile ',
     $          I3,' conv trans data file.',/,'For conv data set ',
     $          I2,' file has gas ',I2,' expecting ',I2,'.')
             STOP
          ENDIF
       ENDDO
C
C      Check the angle secants
       DO I=1,MAXANG
          IF ( (SECANG(I) .LT. SECCHK(I)-RTINY) .OR.
     $    (SECANG(I) .GT. SECCHK(I)+RTINY) ) THEN
             WRITE(IOERR,1050) IPROF, I, SECANG(I), SECCHK(I)
 1050        FORMAT('Error, unexpected angle secant in profile ',
     $          I3,' conv trans data file.',/,'For angle number ',
     $          I2,' file has secant ',F8.5,' expecting ',F8.5,'.')
             STOP
          ENDIF

       ENDDO
C
       RETURN
       END
