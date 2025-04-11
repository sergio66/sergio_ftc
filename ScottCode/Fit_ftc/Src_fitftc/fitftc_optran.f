C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITFTC_optran
C
!F77====================================================================


!ROUTINE NAME:
C    FITFTC


!ABSTRACT:
C    Do a fit (regression) to determine OPTRAN water fast
C    transmittance coefficients.


!CALL PROTOCOL:
C    Main program.


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
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!PARENT(S):
C    none


!ROUTINES CALLED:
C    GETGRD: reads in the OPTRAN layer boundary grid
C    RDREF: reads the reference profile
C    OPCONV: opens the conv trans data files and reads header info
C    CALPTZ: calcs the predictors on the 100 AIRS pressure layers
C    READACT: reads all conv trans regression data for one channel
C    KEFF: calcs the effective transmittance and abs coef
C    INTERP: interpolates data and predictors to OPTRAN layers
C    FITK: does the regression


!FILES ACCESSED:
C    unit IOUN: used to read text files by getgrd and rdref
C    unit IOAVG: file 'predavg.txt': output text file of pred averages
C    unit IOOUT: file 'fastout': main output file of fast trans coefs
C    units IOIN(MAXPRO): used for reading main conv trans data files


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Program FASTTRAN_optran : computes the fast transmittance coef's
C    for AIRS, including angles, using McMillin's OPTRAN method.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    16 Jan 98 Scott Hannon      Created
C     1 Sep 99 Scott Hannon      Quick partial re-write of program for
C                                   water only; it's still a mess
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       PROGRAM FASTTRAN
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
C      none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C      For GETGRD
       INTEGER   IOUN
       REAL  WAZOP(NOPLEV)
C
C      For RDREF
       INTEGER LSTGAS(MAXBOG)
       REAL   PRES(MAXLAY)
       REAL   TREF(MAXLAY)
       REAL AMTREF(MAXLAY,NUMGAS)
C
C      For OPCONV
       INTEGER  IDGAS(NUMSET)
       INTEGER   IOIN(MAXPRO)
       REAL AMOUNT(MAXLAY,NUMSET)
       REAL SECANG(MAXANG)
       REAL   TEMP(MAXLAY)
C
C      For CALPTZ
       REAL  ANGLE(  MAXM)
       REAL    WAZ(MAXLAY,  MAXM)
       REAL     WP(MAXLAY,  MAXM)
       REAL     WT(MAXLAY,  MAXM)
       REAL    WPZ(MAXLAY,  MAXM)
       REAL    WTZ(MAXLAY,  MAXM)
C
C      For READaCT
       LOGICAL MORDAT
       INTEGER IDCHAN
       REAL   FREQ
       REAL   TAUX(MAXPRO,MAXLAY,MAXANG)
       REAL  TAUXW(MAXPRO,MAXLAY,MAXANG)
C
C      For KEFF
       REAL   WODZ(MAXLAY,  MAXM)
       REAL     WK(MAXLAY,  MAXM)
C
C      For INTERP
       REAL   WPOP(NOPLEV,  MAXM)
       REAL   WTOP(NOPLEV,  MAXM)
       REAL  WPZOP(NOPLEV,  MAXM)
       REAL  WTZOP(NOPLEV,  MAXM)
       REAL WODZOP(NOPLEV,  MAXM)
       REAL   WKOP(NOPLEV,  MAXM)
       REAL  WAVGP(NOPLEV)
       REAL  WAVGT(NOPLEV)
       REAL WAVGPZ(NOPLEV)
       REAL WAVGTZ(NOPLEV)
C
C      For FITK
       INTEGER MNANGW(  MAXN)
       REAL WCOEF(NPREDW,NOPLEV)
       INTEGER IOFFST
C
C      Local variables
       INTEGER  IANG
       INTEGER IDWANT
       INTEGER  IGAS
       INTEGER  ILAY
       INTEGER  IOAVG
       INTEGER  IOOUT
       INTEGER   IPRO
c       INTEGER  IPROF
       INTEGER  IWANT
       INTEGER     IX
       INTEGER     JX
       INTEGER  NCHAN
       integer    lop
       integer  icount
C
       REAL PROANG(MAXPRO,MAXANG)
       REAL   TPRO(MAXPRO,MAXLAY)
       REAL    WAP(MAXPRO,MAXLAY)
C
       CHARACTER*79   FNAM


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
C      Assign the minimum number of angles needed for each predictor
C      The following assumes the predictors are:
C      1) 1
C      2) P
C      3) T
C      4) sqrt(P)
C      5) T^2
C      6) T*P
C      7) a
C      8) Pz
C      9) Tz
C
C      For water and ozone
       MNANGW(1)=0
       MNANGW(2)=0
       MNANGW(3)=0
       MNANGW(4)=1
       MNANGW(5)=1
       MNANGW(6)=1
       MNANGW(7)=3
       MNANGW(8)=1
       MNANGW(9)=1
C
C      --------------------------------
C      Open average predictor text file
       IOAVG=8
       OPEN(UNIT=IOAVG,FILE='predavg.txt',FORM='FORMATTED',
     $    STATUS='NEW')
C

C      ---------------------------------------
C      Get the OPTRAN amount above grid levels
       IOUN=9
       IDWANT=1
       CALL GETGRD(IOUN, IDWANT, WAZOP)
C

C      --------------------
C      Read the ref profile
       LSTGAS(1)=1
       CALL RDREF(IOUN, LSTGAS, PRES, TREF, AMTREF)
C

C      ----------------------------------------
C      Open the first convolved trans data file
       IOOUT=10
       IPRO=1
       IOIN(1)=IOOUT + 1
       CALL OPCONV(IPRO, IOIN(IPRO), NCHAN, IDGAS, SECANG, TEMP,
     $    AMOUNT)
C
C      Find water in IDGAS
       IWANT=-1
       DO IGAS=1,NUMSET
          IF (IDGAS(IGAS) .EQ. 1) IWANT=IGAS
       ENDDO
       IF (IWANT .LT. 1) THEN
          WRITE(IOERR,1010)
 1010     FORMAT('Error, water not found in first conv trans data file')
          STOP
       ENDIF
C
       DO ILAY=1,MAXLAY
          WAP(IPRO,ILAY)=AMOUNT(ILAY,IWANT)
          TPRO(IPRO,ILAY)=TEMP(ILAY)
       ENDDO
       DO IANG=1,MAXANG
          PROANG(IPRO,IANG)=SECANG(IANG)
       ENDDO
C

C      -----------------------------------------------
C      Open the rest of the convolved trans data files
       DO IPRO=2,MAXPRO
          IOIN(IPRO)=IOIN(IPRO - 1) + 1
C
          CALL OPCONV(IPRO, IOIN(IPRO), NCHAN, IDGAS, SECANG, TEMP,
     $       AMOUNT)
C
          DO ILAY=1,MAXLAY
             WAP(IPRO,ILAY)=AMOUNT(ILAY,IWANT)
             TPRO(IPRO,ILAY)=TEMP(ILAY)
          ENDDO
          DO IANG=1,MAXANG
             PROANG(IPRO,IANG)=SECANG(IANG)
          ENDDO
       ENDDO
C

C      -------------------------------------------------------------
C      Compute the ratio (fit/ref) of total H2O and O3 amounts above
       CALL CALPTZ( PROANG, PRES, TPRO, WAP, ANGLE,
     $    WAZ, WP, WT, WPZ, WTZ)

C      --------------------
C      Open the output file
       WRITE(6,1050)
 1050  FORMAT('Enter name of coef output file to create')
       READ(5,5050) FNAM
 5050  FORMAT(A79)
       OPEN(UNIT=IOOUT,FILE=FNAM,FORM='UNFORMATTED')

C      -------------------------
C      Loop over the frequencies
C
       MORDAT=.TRUE.
      icount=0
 20    IF (MORDAT) THEN
C
C         Read all convolved trans data for a single freq
          CALL READaCT(IOIN, TAUX, TAUXW, IDCHAN, FREQ, MORDAT)
C
      icount=icount+1
      write(6,*) 'icount=',icount,', idchan=',idchan,', freq=',freq
c
C         Compute the effective water layer trans
          IF (MORDAT) THEN
C
             CALL KEFF(TAUX, TAUXW, PROANG, WAP, WODZ, WK)
C
      write(6,*) 'done calc eff trans'
C
C            Interpolate the data onto the OPTRAN level grid
             CALL INTERP(WAZ, WAZOP,
     $          WP,   WT,   WPZ,   WTZ,   WODZ,   WK,
     $          WPOP, WTOP, WPZOP, WTZOP, WODZOP, WKOP,
     $          WAVGP, WAVGT, WAVGPZ, WAVGTZ)
      write(6,*) 'done interp water'
C
C            Note: there can be problems with singular matrices
C            for the fixed gases because the layer-to-space fixed
C            gas amount and layer pressures are not profile
C            dependent.  This can cause trouble if all the
C            regression data is for the same angle.
C
             IOFFST=0
             CALL FITK9(WODZOP, WKOP, WPOP, WTOP, WPZOP, WTZOP,
     $          ANGLE, WAZOP, WAVGP, WAVGT, WAVGPZ, WAVGTZ, 2,
     $          MNANGW, WCOEF)
      write(6,*) 'done fitk water'
C
C            Write fitted fast trans coefs to output file
             WRITE(IOOUT) IDCHAN, FREQ,
     $          ( (WCOEF(JX,IX),JX=1,NPREDW),IX=1,NOPLEV )
C
      write(6,*) 'done writing output'
ccccccc
      if (icount .eq. 1) then
       DO lop=1,NOPLEV
          WRITE(IOAVG,1234)
     $       WAVGP(LOP), WAVGT(LOP), WAVGPZ(LOP), WAVGTZ(LOP)
 1234     FORMAT(4(X,1PE13.6))
       ENDDO
       CLOSE(IOAVG)
      endif
ccccccc
             GOTO 20
          ENDIF
       ENDIF
C      End of (while) loop over the channels (frequency)
C
       CLOSE(IOOUT)
C
       STOP
       END
