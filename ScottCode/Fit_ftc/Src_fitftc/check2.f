C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             CHECK2
C
!F77====================================================================


!ROUTINE NAME:
C    CHECK2


!ABSTRACT:
C    Check SRF info from a convolved transmittance data file


!CALL PROTOCOL:
C    CHECK2(ICHAN, IPROF, IDCCHK, FRECHK, RESCHK, RNFCHK,
C       IDCHAN, FREQ, RES, RNFWHM)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    ICHAN          Channel number     none
C    INTEGER    IPROF          Profile number     none
C    INTEGER    IDCCHK         Expected chan ID   none
C    REAL       FRECHK         Expected chan freq cm^-1
C    REAL       RESCHK         Expected chan res  none (delta_nu/nu)
C    REAL       RNFCHK         Expected no. FWHM  none
C    INTEGER    IDCHAN         Test chan ID       none
C    REAL       FREQ           Test chan freq     cm^-1
C    REAL       RES            Test chan res      none (delta_nu/nu)
C    REAL       RNFWHM         Test no. FWHM      none


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
C    RDCHAN


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    OUTPUT:
C        I/O unit 6 (screen): error message (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine CHECK2 checks some of the channel info from a convolved
C    transmittance data file against the expected values. The routine
C    stops the program if it detects a problem.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    Limitation: The routine does NOT check to make sure the SRF is
C    exactly the same by looking at SRFA, SRFB, SRFC, SRFD, and XNORM.


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    13 Dec 96 Scott Hannon      Created
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6);
C                                added include 'farray.f' (for IOERR)


!END ===================================================================


C      =================================================================
       SUBROUTINE CHECK2(ICHAN, IPROF, IDCCHK, FRECHK, RESCHK, RNFCHK,
     $    IDCHAN, FREQ, RES, RNFWHM)
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
       INTEGER ICHAN, IPROF, IDCCHK, IDCHAN
       REAL FRECHK, RESCHK, RNFCHK, FREQ, RES, RNFWHM
C
C      Output parameters
C    none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
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
C      Assign accuracy requirement for comparing FREQ, RES, RNFWHM
       RTINY=1.0E-3
C
C      Check the channel ID
       IF (IDCHAN .NE. IDCCHK) THEN
          WRITE(IOERR,1010) IPROF, ICHAN, IDCHAN, IDCCHK
 1010     FORMAT('Error, unexpected channel ID for profile ',I3,
     $       ' conv trans data file.',/,'Channel ',I4,
     $       ' has channel ID ',I3,', expecting ',I3,'.')
          STOP
       ENDIF
C
C      Check channel center frequency
       IF ((FREQ .LT. FRECHK-RTINY) .OR. (FREQ .GT. FRECHK+RTINY))
     $ THEN
          WRITE(IOERR,1030) IPROF, ICHAN, FREQ, FRECHK
 1030     FORMAT('Error, unexpected frequency in profile ',I3,
     $       ' conv trans data file.',/,'Channel ',I4,
     $       ' has frequency ',F9.3,' expecting ',F9.3,'.')
          STOP
       ENDIF
C
C      Check channel resolution
       IF ((RES .LT. RESCHK-RTINY) .OR. (RES .GT. RESCHK+RTINY))
     $ THEN
          WRITE(IOERR,1050) IPROF, ICHAN, RES, RESCHK
 1050     FORMAT('Error, unexpected resolution in profile ',I3,
     $       ' conv trans data file.',/,'Channel ',I4,
     $       ' has resolution ',F9.3,' expecting ',F9.3,'.')
          STOP
       ENDIF
C
C      Check channel no. of FWHM
       IF ( (RNFWHM .LT. RNFCHK-RTINY) .OR.
     $ (RNFWHM .GT. RNFCHK+RTINY) ) THEN
          WRITE(IOERR,1070) IPROF, ICHAN, RNFWHM, RNFCHK
 1070     FORMAT('Error, unexpected no. of FWHM in profile ',I3,
     $       ' conv trans data file.',/,'Channel ',I4,
     $       ' has ',F9.3,' FWHMs, expecting ',F9.3,'.')
          STOP
       ENDIF
C
       RETURN
       END
