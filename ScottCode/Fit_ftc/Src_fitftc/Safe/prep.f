C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             PREP
C
!F77====================================================================


!ROUTINE NAME:
C    PREP


!ABSTRACT:
C    Do the housekeeping preparation for the fast transmittance
C    regression program.


!CALL PROTOCOL:
C    PREP(IOIN, IOOUT, NCHAN, IOFFST, PRED, SECANG, LSTGAS, ISORTK)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    IOIN           Conv data unit no. none
C    INTEGER    IOOUT          Output unit no.    none
C    INTEGER    NCHAN          Number of channels none
C    INT ARR    IOFFST         PRED index offsets none
C    REAL ARR   PRED           Predictors         various
C    REAL ARR   SECANG         Angle secant       none
C    INT ARR    LSTGAS         Gas IDs for fits   none
C    INT ARR    ISORTK         Trans data indices none


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITFTC


!ROUTINES CALLED:
C    RDREF: read the reference profile
C    OPCONV: open convolved transmittance data files
C    SORT: sort the gas ID array indices into the order to break out
C    CHECK1: check that conv trans file is as expected
C    CALPRD: calculate the predictors for the regression data
C    OPOUT: open the output file


!FILES ACCESSED:
C    none (except through routines)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine PREP does the preliminary housekeeping routines for the
C    main program. This stuff is done here to keep the main program
C    from getting cluttered. First PREP calls RDREF to read the
C    reference profile (needed to calculate the predictors). Then it
C    calls OPCONV to open all the convolved layer-to-space
C    transmittance files and read the header info. The header
C    contains info about the profiles (needed to calculate the
C    predictors), as well as dimensions for various arrays, etc. After
C    each file (except the first) is openned and the header read,
C    routine CHECK1 is called to check to see that the data file
C    is consistent with array sizes etc it was expecting. After all
C    data files (one per profile) has been openned, the predictors
C    are calculated with a call to CALPRD. Finally, the output file
C    is openned with a call to OPOUT.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    13 Jan 97 Scott Hannon      Created
C    26 Aug 99 Scott Hannon      Change SECANG to output var; uses
C                                   re-written SORT routine; add LSTGAS;
C                                   moved call to RDREF from before to
C                                   after OPCONV and SORT


!END ===================================================================


C      =================================================================
       SUBROUTINE PREP(IOIN, IOOUT, NCHAN, LSTGAS, ISORTK, IOFFST,
     $    PRED, SECANG)
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
C      Input:
C    none
C
C      Output:
       INTEGER   IOIN(MAXPRO)
       INTEGER  IOOUT
       INTEGER  NCHAN
       INTEGER LSTGAS(MAXBOG)
       INTEGER ISORTK(MAXBOG)
       INTEGER IOFFST(NUMGAS)
       REAL   PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY)
       REAL SECANG(MAXANG)



C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER  IOREF
       INTEGER   IPRO
       INTEGER   ILAY
       INTEGER   IGAS
c       INTEGER     IG
       REAL   TPRO(MAXLAY,MAXPRO)
c       REAL   FPRO(MAXLAY,MAXPRO)
       REAL AMTPRO(MAXLAY,MAXPRO,NUMGAS)
C
C      For RDREF
       REAL   PRES(MAXLAY)
       REAL   TREF(MAXLAY)
       REAL AMTREF(MAXLAY,NUMGAS)
C
C      For OPCONV
       INTEGER IDGAS(NUMSET)
       REAL   TEMP(MAXLAY)
       REAL AMOUNT(MAXLAY,NUMSET)
C
C      For first profile OPCONV
       INTEGER NCHCHK
       INTEGER IDGCHK(NUMSET)
       REAL SECCHK(MAXANG)
C
C      For SORT
       INTEGER ISORTA(MAXBOG)


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
C      ---------------------------
C      Assign the I/O unit numbers
C      ---------------------------
       IOREF=10
       IOOUT=10
       IOIN(1)=IOOUT + 1
       DO IPRO=2,MAXPRO
          IOIN(IPRO)=IOIN(IPRO - 1) + 1
       ENDDO
C

C      --------------------------------
C      Open the first profile data file
C      --------------------------------
       IPRO=1
C
       CALL OPCONV(IPRO, IOIN(IPRO), NCHCHK, IDGCHK, SECCHK, TEMP,
     $    AMOUNT)
C
       CALL SORT(IDGCHK, LSTGAS, ISORTA, ISORTK)
C
C      Move the first profile data into arrays for all profiles
       DO ILAY=1,MAXLAY
          TPRO(ILAY,IPRO)=TEMP(ILAY)
       ENDDO
       DO IGAS=1,NUMGAS
          DO ILAY=1,MAXLAY
             AMTPRO(ILAY,IPRO,IGAS)=AMOUNT(ILAY,ISORTA(IGAS))
          ENDDO
       ENDDO
C

C      --------------------------------------
C      Open all the other profiles data files
C      --------------------------------------
       DO IPRO=2,MAXPRO
C
C         Open the conv trans data file for the current profile
          CALL OPCONV(IPRO, IOIN(IPRO), NCHAN, IDGAS, SECANG,
     $       TEMP, AMOUNT)
C
C         Check to see that the current conv data file is as expected
          CALL CHECK1(IPRO, NCHCHK, IDGCHK, SECCHK, NCHAN, IDGAS,
     $       SECANG)
C
C         Move the current profile data into arrays for all profiles
          DO ILAY=1,MAXLAY
             TPRO(ILAY,IPRO)=TEMP(ILAY)
          ENDDO
          DO IGAS=1,NUMGAS
             DO ILAY=1,MAXLAY
                AMTPRO(ILAY,IPRO,IGAS)=AMOUNT(ILAY,ISORTA(IGAS))
             ENDDO
          ENDDO
C
       ENDDO
C      End loop over profiles
C

C      --------------------------
C      Read the reference profile
C      --------------------------
C
       CALL RDREF(IOREF, LSTGAS, PRES, TREF, AMTREF)
C

C      ------------------------
C      Calculate the predictors
C      ------------------------
C
       CALL CALPRD(LSTGAS, SECANG, PRES, TREF, AMTREF, TPRO, AMTPRO,
     $    IOFFST, PRED)
C

C      --------------------
C      Open the output file
C      --------------------
C
       CALL OPOUT(IOOUT)
C

       RETURN
       END
