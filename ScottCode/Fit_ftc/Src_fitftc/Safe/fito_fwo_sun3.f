c This version for FWO 3 term ozone
C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITO_FWO_sun3
C
!F77====================================================================


!ROUTINE NAME:
C    FITO


!ABSTRACT:
C    Solve for the ozone fast transmittance coefs.
C    This version is for FWO.


!CALL PROTOCOL:
C    FITO(ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS, KLAYER,
C       KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    ICHAN          Channel no.        none
C    INT ARR    NDTPTS         No. of data points none
C    INT ARR    NNEG           No. of negative pt none
C    INT ARR    IOFFST         PRED index offsets none
C    REAL ARR   PRED           Predictors         various
C    REAL ARR   KLTOS          Eff l-to-s op dpth none
C    REAL ARR   KLAYER         Eff layer op depth none
C    REAL ARR   KZMIN          Min l-to-s op dpth none
C    REAL ARR   KZMAX          Max l-to-s op dpth none
C    REAL ARR   KMIN           Min layer op depth none
C    REAL ARR   KMAX           Max layer op depth none
C    REAL ARR   SECANG         Angle secants      none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   FTCOEF         Fast trans coefs   various


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITFTC


!ROUTINES CALLED:
C    WGHT2A: weight the regression data
C    FITGAS: Solves for the fast trans coefs


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine FITO solves for the ozone fast transmittance
C    (actually optical depth) coefficients. The routine determines
C    how many predictors to use and does the regression. It then does
C    another regression with one fewer predictors, and compares the
C    fit error with the those from the previous regression. If the fit
C    with the fewer predictors is not significantly worse, then it
C    repeats this process of subtracting a predictor and comparing
C    errors. It ends when the fits get much worse, or it reaches the
C    minimun number of predictors.
C    This versions is for FWO.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    25 Jun 97 Scott Hannon      Created from FOW fito


!END ===================================================================


C      =================================================================
       SUBROUTINE FITO(ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS, KLAYER,
     $    KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)
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
       INTEGER ICHAN, NDTPTS(MAXLAY,NUMGAS), NNEG(MAXLAY,NUMGAS),
     $    IOFFST(NUMGAS)
       REAL PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY),
     $    KLTOS(MAXPRO,MAXANG,MAXLAY,NUMGAS),
     $   KLAYER(MAXPRO,MAXANG,MAXLAY,NUMGAS),
     $    KZMIN(MAXLAY,NUMGAS), KZMAX(MAXLAY,NUMGAS), 
     $     KMIN(MAXLAY,NUMGAS),  KMAX(MAXLAY,NUMGAS), SECANG(MAXANG)
C
C      Output parameters
       REAL FTCOEF(MAXPRD,MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER IGASO, ILAY, MPTS, MXPRED, NPRED, IPRED, MINCOF, IBEST,
     $    NEGPTS, IGASF, IGASW, MXUSE, MINPTS(MAXN), I, J
       REAL ALLCOF(MAXN,MAXN), ALLRMS(MAXN), XKO, XFW, XKZ, RATK,
     $    RATNEG, DIF, SIGDIF
C
C      For WGHT
       INTEGER NGASZ, IGASZ(NUMGAS)
       LOGICAL LUSE(MAXN)
       DOUBLE PRECISION AMAT(MAXM,MAXN), BVEC(MAXM)
C
C      For FITGAS
       INTEGER IBAD
       DOUBLE PRECISION XVEC(MAXN)


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
C      Define the mininum number of points needed to do a regression
C      with a certain number of predictors.
       J=MAXANG + 3
       DO I=1,MAXN
          MINPTS(I)=J*I
          LUSE(I)=.FALSE.
       ENDDO
C
C      For FWO, ozone is the 3rd gas (fixed is 1st and water is 2nd)
       IGASF=1
       IGASW=2
       IGASO=3
C
       MINCOF=1
C
C      List the array positions of the gases which make up
C      which make up the total optical depth.
       NGASZ=3
       IGASZ(1)=1
       IGASZ(2)=2
       IGASZ(3)=3
C
C      Assign the value to use in testing for
C      a significant difference in fit results.
       SIGDIF=1.0E-2
C
C      ------------------------------------------
C      Loop over the layers and do the regression
C      ------------------------------------------
       DO ILAY=1,MAXLAY
C
          MPTS=NDTPTS(ILAY,IGASO)
          NEGPTS=NNEG(ILAY,IGASO)
          XKO=KMAX(ILAY,IGASO)
          XFW=KMIN(ILAY,IGASF) + KMIN(ILAY,IGASW)
          XKZ=KZMIN(ILAY,IGASF) + KZMIN(ILAY,IGASW) + KZMIN(ILAY,IGASO)
          RATK=XKO/XFW
          RATNEG=FLOAT(NEGPTS)/FLOAT(NEGPTS + MPTS)
C
C         ------------------------------------
C         Determine how many predictors to use
C         ------------------------------------
C
          IF ((MPTS .GT. MINPTS(1)) .AND. (RATNEG .LT. 0.40)) THEN
             MXPRED=1
             LUSE(1)=.TRUE.
             MXUSE=1
C
             IF ((MPTS .GT. MINPTS(2)) .AND. (RATNEG .LT. 0.20) .AND.
     $       (XKO .GT. 1.0E-5) .AND. (XKZ .LT. 10.0) .AND.
     $       (RATK .GT. 1.0E-2)) THEN
                MXPRED=2
                LUSE(2)=.TRUE.
                MXUSE=MXUSE + 1
C
                IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $          (RATNEG .LT. 0.10) .AND.
     $          (XKO .GT. 1.0E-4) .AND. (XKZ .LT. 8.0)) THEN
                   MXPRED=3
                   LUSE(3)=.TRUE.
                   MXUSE=MXUSE + 1
C
                ENDIF
             ENDIF
C
          ELSE
             MXPRED=0
             DO IPRED=1,NPREDO
                FTCOEF(IPRED+IOFFST(IGASO),ILAY)=0.0E+0
             ENDDO
          ENDIF 
C
c      write(6,*) 'layer=',ilay,', mpts=',mpts,', mxpred=',mxpred
C         -----------------
C         Do the regression (if necessary)
C         -----------------
          IF (MXPRED .GT. 0) THEN
C
C            -------------------------
C            Scale the regression data
C            and load AMAT and BVEC.
C            -------------------------
C
             CALL WGHT2A(ILAY, IGASO, NGASZ, IGASZ, IGASF, MPTS,
     $          MXPRED, IOFFST, PRED, LUSE, KLTOS, KLAYER, SECANG,
     $          AMAT, BVEC)
C
C            -----------------
C            Do the regression
C            -----------------
C
             NPRED=MXUSE + 1
C
 10          NPRED=NPRED - 1
C
             CALL FITGAS(MPTS, NPRED, AMAT, BVEC, XVEC, ALLRMS(NPRED),
     $          IBAD)
c      write(6,*) 'npred=',npred,', rms=',ALLRMS(NPRED)
C
C            Check the "bad fit" flag
             IF (IBAD .EQ. 0) THEN
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=SNGL( XVEC(IPRED) )
                ENDDO
             ELSE
c      write(6,*) 'npred=',npred,', ibad=',ibad
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=0.0E+0
                ENDDO
             ENDIF
C
C            --------------------------------------
C            Determine whether or not to do another
C            regression with fewer predictors.
C            --------------------------------------
C
             IF (MXUSE .EQ. MINCOF) THEN
C               The last fit is the only fit allowed
                IBEST=MINCOF
             ELSE
                IF (NPRED .EQ. MXUSE) THEN
C                  The last fit was the first fit; do another
                   IBEST=MXUSE
                   GOTO 10
                ELSE
C                  Compare the last fit to the best so far
                   DIF=(ALLRMS(NPRED) - ALLRMS(IBEST))/ALLRMS(IBEST)
                   IF (DIF .LT. SIGDIF) THEN
C
C                     Re-set IBEST if the last fit is better than
C                     previous best (this shouldn't happen, but...)
                      IF (DIF .LT. 0.0E+0) IBEST=NPRED
C
C                     Set IBEST if this the last fit allowed
                      IF (NPRED .EQ. MINCOF) IBEST=MINCOF
C
C                     The last fit was good; do another
                      IF (NPRED .GT. MINCOF) GOTO 10
                   ELSE
C                     The last fit was not good enough
                      IBEST=NPRED + 1
                   ENDIF
                ENDIF
             ENDIF
C
C            Load out the fit results into FTCOEF
             I=0
             DO IPRED=1,NPREDO
                IF ((I .LT. IBEST) .AND. (LUSE(IPRED))) THEN
                   I=I + 1
                   FTCOEF(IPRED+IOFFST(IGASO),ILAY)=ALLCOF(I,IBEST)
                ELSE
                   FTCOEF(IPRED+IOFFST(IGASO),ILAY)=0.0E+0
                ENDIF
             ENDDO
C
c      write(6,*) 'ibest=',ibest
          ENDIF
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
