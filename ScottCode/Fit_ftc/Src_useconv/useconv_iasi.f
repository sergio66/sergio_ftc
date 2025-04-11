C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             USECONV
C
!F77====================================================================


!ROUTINE NAME:
C    USECONV


!ABSTRACT:
C    Calculate radiances using the convolved layer-to-space
C    transmittances and the water continuum fast transmittance
C    coefficients.


!CALL PROTOCOL:
C    none (main program)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


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
C    none


!ROUTINES CALLED:
C    RDREF: read the reference profile
C    URAD: calculate the radiance


!FILES ACCESSED:
C    INPUT:
C        unit 5 (keyboard): enter filename
C        unit IOPREF: reference file (text)
C        unit IODATA: convolved transmittance data file
C        unit IOCON: water con fast trans coefs.
C    OUTPUT:
C        unit IOERR: error messages
C        unit 6 (screen): prompt for filename
C        unit IORAD: output file of radiances 
C        unit IOTAU: output file of surface-to-transmittances


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Calculates radiances for the data in a convolved layer-to-space
C    transmittance file. The water continuum is added on using the
C    water con fast trans coefs. The radiance is computed for each
C    angle of convolved data.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    27 Jun 97 Scott Hannon      Created
C     3 Sep 99 Scott Hannon      Modified to be more generic (don't need
C                                   separate versions for FOW, FMW, etc)
C     3 Aug 2000 Scott Hannon    Re-do I/O unit numbers, and add output
C                                   file for surf-to-space trans
C    11 Aug 2000 Scott Hannon    Change from 4 to 5 term H2O continuum
C    10 May 2002 Scott Hannon    Modified to allow water con coef file
C                                   to contain more channels than
C                                   needed for conv trans.
C    21 May 2002 Scott Hannon    Change errors from unit 6 to IOERR
C    10 Sep 2002 Scott Hannon    Add predictors 6 & 7 to H2O continuum


!END ===================================================================


C      =================================================================
       PROGRAM USECONV
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       INCLUDE 'farray.f'

C      Parameters
       INTEGER MXCHAN  ! max number of channels
       INTEGER NCCOEF  ! number of water con coefs
       PARAMETER (MXCHAN=8461)
       PARAMETER (NCCOEF=7)


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
       REAL BTEMP


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
C      I/O unit numbers
       INTEGER IOCON   ! for input water con coef file
       INTEGER IODATA  ! for input conv trans data file
       INTEGER IOPREF  ! for input ref prof file
       INTEGER IORAD   ! for output rad file
       INTEGER IOTAU   ! for output trans file
C
       INTEGER      I
       INTEGER   IANG
       INTEGER     IC
       INTEGER    ICC
       INTEGER  ICHAN
       INTEGER IDCHAN
       INTEGER  IDGAS(NUMSET)
       INTEGER   IGAS
       INTEGER   ILAY
       INTEGER   ISET
       INTEGER   IWAT
       INTEGER  IWREF
       INTEGER   LBOT
       INTEGER   NANG
       INTEGER NCCHAN ! number of continuum channels
       INTEGER  NCHAN ! number of conv trans channels
       INTEGER   NGAS
       INTEGER   NLAY
C
       REAL AMOUNT(MAXLAY,NUMSET)
       REAL   EBOT
       REAL   FREQ
       REAL    RAD(MAXANG)
       REAL    RES
       REAL RNFWHM
       REAL SECANG(MAXANG)
       REAL   TAUZ(MAXLAY,MAXANG)
       REAL   TBOT
       REAL   TEMP(MAXLAY)
       REAL  TJUNK(MAXANG*MAXLAY)
C
       CHARACTER*40  TITLE
       CHARACTER*79   FNAM
C
C      For the fit H2O continuum
       INTEGER  IDCON(MXCHAN)
       INTEGER  ICOEF
       REAL  CFREQ(MXCHAN)
       REAL CONCOF(NCCOEF,MAXLAY,MXCHAN)
       REAL     TR
       REAL    A_W
       REAL CONPRM(NCCOEF,MAXLAY)
       REAL   KCON
       REAL AKCONZ(MAXANG)
C
C      For RDREF
       INTEGER LSTGAS(MAXBOG)
       REAL   PREF(MAXLAY)
       REAL   TREF(MAXLAY)
       REAL AMTREF(MAXLAY,NUMGAS)


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
C      Assign I/O unit numbers
       IOCON  = 10
       IODATA = 11
       IOPREF = 12
       IORAD  = 13
       IOTAU  = 14

C      ------------------------
C      Read the water con coefs
C      ------------------------
C      Open the file with the con coefs
       WRITE(6,1010)
 1010  FORMAT('Enter the name of the file with the water con coefs')
       READ(5,9000) FNAM
       OPEN(UNIT=IOCON,FILE=FNAM,FORM='UNFORMATTED',STATUS='OLD')
C
C      Read all the con coefs
       ICHAN=0
       IC=1
 10    READ(IOCON,END=19) IDCON(IC), CFREQ(IC),
     $    ((CONCOF(ICOEF,ILAY,IC),ICOEF=1,NCCOEF),ILAY=1,MAXLAY)
C      Make sure con data is sorted in ascending ID order
       IF (IDCON(IC) .LT. ICHAN) THEN
          WRITE(IOERR,1020)
 1020     FORMAT('ERROR, water con data is not sorted in ascending'
     $    ' ID order.')
          STOP
       ENDIF
       ICHAN=IC
       IC=IC + 1
       GOTO 10
 19    NCCHAN=IC - 1
       CLOSE(IOCON)
C

C      -----------------
C      Open output files
C      -----------------
       WRITE(6,1030)
 1030  FORMAT('Enter name of text file for output radiance')
       READ(5,9000) FNAM
       OPEN(UNIT=IORAD,FILE=FNAM,FORM='FORMATTED',STATUS='NEW')
       WRITE(6,1035)
 1035  FORMAT('Enter name of text file for output transmittances')
       READ(5,9000) FNAM
       OPEN(UNIT=IOTAU,FILE=FNAM,FORM='FORMATTED',STATUS='NEW')

C      --------------------------
C      Read the reference profile
C      --------------------------
C      Just want temperature & water amount; ignore other gases
       DO I=1,MAXBOG
          LSTGAS(I)=0
       ENDDO
       IWREF=1
       LSTGAS(IWREF)=1
       CALL RDREF(IOPREF, LSTGAS, PREF, TREF, AMTREF)
C

C      --------------------
C      Open input data file
C      --------------------
       WRITE(6,1050)
 1050  FORMAT('Enter name of file to read')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
       OPEN(UNIT=IODATA,FILE=FNAM,STATUS='OLD',FORM='UNFORMATTED')
C

C      -------------------------
C      Read the data file header
C      -------------------------
       READ(IODATA) NANG, NLAY, NGAS, NCHAN
C
       IF (NLAY .NE. MAXLAY) THEN
          WRITE(IOERR,1100) NLAY, MAXLAY
 1100     FORMAT('ERROR, number of layers',/,
     $    'Data file has ',I4,' layers, expecting ',I4)
          STOP
       ENDIF
C
       IF (NGAS .GT. NUMSET) THEN
          WRITE(IOERR,1110) NGAS, NUMSET
 1110     FORMAT('ERROR, number of gases.',/,
     $    'Data file has ',I4,' gases, expecting max of ',I4)
          STOP
       ENDIF
C
       IF (NANG .GT. MAXANG) THEN
          WRITE(IOERR,1120) NANG, MAXANG
 1120     FORMAT('ERROR, number of angles.',/,
     $    'Data file has ',I4,' angles, expecting max of ',I4)
          STOP
       ENDIF
C
       IF (NCHAN .GT. NCCHAN) THEN
          WRITE(IOERR,1130) NCHAN, NCCHAN
 1130     FORMAT('ERROR, water con file does not span conv trans data',
     $    /,'Trans data file has ',I4,' channels, con file has ',I4)
          STOP
       ENDIF
C
       READ(IODATA) (IDGAS(IGAS),IGAS=1,NGAS)
       READ(IODATA) (SECANG(IANG),IANG=1,NANG)
       READ(IODATA) TITLE
       READ(IODATA) (TEMP(ILAY),ILAY=1,NLAY)
C
C
C      Read the amounts and determine
C      which of the gases is water.
       IWAT=0
       DO IGAS=1,NGAS
          IF (IDGAS(IGAS) .EQ. 1) IWAT=IGAS
          READ(IODATA) (AMOUNT(ILAY,IGAS),ILAY=1,NLAY)
       ENDDO
       IF (IWAT .EQ. 0) THEN
          WRITE(IOERR,1140)
 1140     FORMAT('Error, data file does not include water.')
          STOP
       ENDIF
C

C      ----------------------------
C      Ask for trans/gas set number 
C      ----------------------------
       WRITE(6,1070) NGAS,(IDGAS(I),I=1,NGAS)
 1070  FORMAT('Gas IDs for the sets 1-',I1,' are: ',6(I4))
       WRITE(6,1075)
 1075  FORMAT('Enter set number of trans/gas to use : ')
       READ(5,*) ISET
       IF ((ISET .LT. 1) .OR. (ISET .GT. NGAS)) THEN
          WRITE(IOERR,1080) ISET, NGAS
 1080     FORMAT('Error, ISET=',I3,' is out of expected range 1-',I1)
          STOP
       ENDIF
C

C      ------------------------------
C      Determine bottom layer number,
C      temperature, and emissivity.
C      ------------------------------
       WRITE(6,1150)
 1150  FORMAT('Enter the bottom layer number (1=top)')
       READ(5,*) LBOT
       IF (LBOT .LT. 1) LBOT=1
       IF (LBOT .GT. MAXLAY) LBOT=MAXLAY
       WRITE(6,1170)
 1170  FORMAT('Enter bottom temp and emissivity (-1 for default)')
       READ(5,*) TBOT, EBOT
       IF (TBOT .LT. 0.0) TBOT=TEMP(LBOT)
       IF (EBOT .LT. 0.0) EBOT=0.975
C

C      -----------------------------
C      Calc the water con parameters
C      -----------------------------
       DO ILAY=1,MAXLAY
          TR=TEMP(ILAY)/TREF(ILAY)
          A_W=AMOUNT(ILAY,IWAT)/AMTREF(ILAY,IWREF)
C
          CONPRM(3,ILAY)=A_W/TR
          CONPRM(4,ILAY)=CONPRM(3,ILAY)*A_W
          CONPRM(1,ILAY)=CONPRM(3,ILAY)/TR
          CONPRM(2,ILAY)=CONPRM(1,ILAY)*CONPRM(1,ILAY)
          CONPRM(5,ILAY)=CONPRM(1,ILAY)*A_W
          CONPRM(6,ILAY)=A_W/(TR**4)
          CONPRM(7,ILAY)=A_W
       ENDDO
C

C      ----------------------
C      Loop over the channels
C      ----------------------
       ICC=1
       ICHAN=0
       DO IC=1,NCHAN

C         ----------------------------
C         Read a channel of convolved
C         layer-to-space transmittance.
C         ----------------------------
          READ(IODATA) FREQ, IDCHAN, RES, RNFWHM,
     $       ( (TJUNK(I),I=1,NANG*MAXLAY), IGAS=1,ISET-1),
     $       ( (TAUZ(ILAY,IANG),IANG=1,NANG), ILAY=1,MAXLAY),
     $       ( (TJUNK(I),I=1,NANG*MAXLAY), IGAS=ISET+1,NGAS)

ccc
c       print *, 'freq,idchan=',FREQ,IDCHAN
ccc

C         Make sure conv trans data is sorted in ascending ID order
          IF (IDCHAN .LT. ICHAN) THEN
             WRITE(IOERR,1210)
 1210        FORMAT('ERROR, conv trans data is not sorted in'
     $       ' ascending ID order.')
             STOP
          ENDIF
          ICHAN=IDCHAN

C         --------------------------
C         Add in the water continuum
C         --------------------------
C         Determine index of current channel in IDCON
 20       IF (ICC .GT. NCCHAN) THEN
             WRITE(IOERR,1220) IDCHAN
 1220        FORMAT('ERROR, water con file does not contain data for'
     $       ' channel',I5)
             STOP
          ENDIF
          IF (IDCON(ICC) .NE. IDCHAN) THEN
             ICC=ICC + 1
             GOTO 20
          ENDIF
ccc
c      uncomment to check alignment of trans & con freqs
c      write(6,*) FREQ-CFREQ(ICC)
ccc
C
          DO IANG=1,NANG
             AKCONZ(IANG)=0.0E+0
          ENDDO
C
          DO ILAY=1,MAXLAY
C            Compute the nadir layer water continuum
             KCON=CONCOF(1,ILAY,ICC)*CONPRM(1,ILAY) +
     $            CONCOF(2,ILAY,ICC)*CONPRM(2,ILAY) +
     $            CONCOF(3,ILAY,ICC)*CONPRM(3,ILAY) +
     $            CONCOF(4,ILAY,ICC)*CONPRM(4,ILAY) +
     $            CONCOF(5,ILAY,ICC)*CONPRM(5,ILAY) +
     $            CONCOF(6,ILAY,ICC)*CONPRM(6,ILAY) +
     $            CONCOF(7,ILAY,ICC)*CONPRM(7,ILAY)
             IF (KCON .LT. 0.0) THEN
                WRITE(6,1180) IDCHAN, ILAY
 1180           FORMAT('Warning, Kcon < 0, chan=',I4,', layer=',I3)
                KCON=0.0
             ENDIF

C
ccc
c      uncomment to test with kcon=0
c      kcon=0.0
ccc
C            Calculate the layer-to-space water con for each angle
C            and put it into the total layer-to-space transmittance.
             DO IANG=1,NANG
                AKCONZ(IANG)=AKCONZ(IANG) + KCON*SECANG(IANG)
                TAUZ(ILAY,IANG)=TAUZ(ILAY,IANG)*EXP(-AKCONZ(IANG))
             ENDDO
C
          ENDDO ! End loop over layers for water con

C         -------------------------------
C         Calculate the radiance for each
C         angle for the current channel.
C         -------------------------------
          CALL URAD(FREQ, TAUZ, TEMP, NANG, LBOT, TBOT, EBOT, RAD)

C         ------------------------
C         Write to the output file
C         ------------------------
          WRITE(IORAD,1190) IDCHAN, FREQ, (BTEMP(FREQ, RAD(IANG)),
     $       IANG=1,NANG)
 1190     FORMAT(I5,1X,F9.4,12(1X,F7.3))
          WRITE(IOTAU,1195) IDCHAN, FREQ, (TAUZ(LBOT,IANG),
     $       IANG=1,NANG)
 1195     FORMAT(I5,1X,F9.4,12(1X,F7.5))

       ENDDO ! End loop over channels
C
C      Close the input data and output files
       CLOSE(IODATA)
       CLOSE(IORAD)
       CLOSE(IOTAU)
C
       STOP
       END
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC FUNCTION BTEMP *************************
C
       REAL FUNCTION BTEMP(V, R)
C      (* computes brightness temp from radiance *)
C
       REAL V, R
C
C      Local variables
       REAL V3, C1, C2
C
C      -----------------------------------------------------------------
       C1=1.1911E-8
       C2=1.4387863
C
       V3=V*V*V
C
       IF (R .NE. 0.0) THEN
          BTEMP=C2*V/LOG(1.0+C1*V3/R)
       ELSE
          WRITE(IOERR,*) 'Warning! zero radiance'
       ENDIF
C
       RETURN
       END
