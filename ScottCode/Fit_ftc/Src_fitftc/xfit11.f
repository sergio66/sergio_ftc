C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             XFIT11
C
!F77====================================================================


!ROUTINE NAME:
C    XFIT11


!ABSTRACT:
C    Solve the equation A*X=B for X, where A=MxN, X=Nx1, and B=Mx1.
C    This version for NSIZE=11.


!CALL PROTOCOL:
C    XFIT11(MI, AI, BI, XO)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    MI             M, no. of rows     none
C    DOUBLE ARR AI             A, MxN matrix      various (predictors)
C    DOUBLE ARR BI             B, Mx1 vector      none (optical depth)


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    DOUBLE ARR XO             X, Nx1 vector      1/various (pred coefs)


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITW


!ROUTINES CALLED:
C    LAPACK (library of Linear Algebgra routines for FORTRAN 77):
C       Three routines are used to solve A * x = b for x:
C       DGEQRF: computes QR factorization of A; A = Q * R  
C       DORMQR: used to compute c = Q^T * b
C       DTRTRS: used to solve R * x = c1 for x. c1 = 1 thru N of c.


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Solves the equation A * x = b for x using LAPACK routines to do
C    the linear algebra. The solution is as discussed in the LAPACK
C    Users' Guide (1992) on pages 21-22.
C    A is the matrix of predictors
C    x is the vector of unknown predictor coefficients
C    b is the vector of optical depths
C    To prevent math/numerical troubles and overwriting the input
C    data, the MI, AI, BI, XO are copied to local work A, B, X.
C    This version for NSIZE=11.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    19 Dec 96 Scott Hannon      Created
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE XFIT11(MI, AI, BI, XO)
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
       INTEGER MI
       DOUBLE PRECISION AI(MAXM,MAXN), BI(MAXM)
C
C      Output parameters
       DOUBLE PRECISION XO(MAXN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
       INTEGER NSIZE
       PARAMETER( NSIZE=11 )
C
       INTEGER I, J
C
C      For LAPACK
       INTEGER M, N, LDA, LWORK, INFO
       DOUBLE PRECISION A(MAXM,NSIZE), B(MAXM), X(NSIZE), TAU(NSIZE),
     $    WORK(NSIZE)


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
C      ------------------------
C      Initialize the variables
C      ------------------------
       M=MI
       N=NSIZE
       LDA=MAXM
       LWORK=NSIZE
C
       DO J=1,M
          B(J)=BI(J)
       ENDDO
       DO I=1,N
          DO J=1,M
             A(J,I)=AI(J,I)
          ENDDO
       ENDDO
C
C      -------------------
C      Calculate A = Q * R
C      -------------------
C
       CALL DGEQRF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
C
       IF (INFO .NE. 0) THEN
          WRITE(IOERR,1010) N, -INFO
 1010     FORMAT('Error in XFIT',I2,/,
     $    'DGEQRF, argument',I4,' had an illegal value')
          STOP
       ENDIF
C
C      -------------------
C      Compute c = Q^T * b
C      -------------------
C
C      Note: on exit "B" is overwritten by c.
C
       CALL DORMQR('L', 'T', M, 1, N, A, LDA, TAU, B, M,
     +    WORK, 1, INFO)
C
       IF (INFO .NE. 0) THEN
          WRITE(IOERR,1030) N, -INFO
 1030     FORMAT('Error in XFIT',I2,/,
     $    'DORMQR, argument',I4,' had an illegal value')
          STOP
       ENDIF
C
C      ----------------------
C      Solve R * x = c1 for x
C      ----------------------
C
C      Note: send c1 (the first N elements of vector c) in x
       DO I=1,N
          X(I)=B(I)
       ENDDO
C
       CALL DTRTRS('U', 'N', 'N', N, 1, A, LDA, X, N, INFO)
C
       IF (INFO .NE. 0) THEN
          WRITE(IOERR,1050) N, -INFO
 1050     FORMAT('Error in XFIT',I2,/,
     $    'DTRTRS, argument',I4,' had an illegal value')
          STOP
       ENDIF
C
C      -------------------------------------
C      Copy the solution to the output array
C      -------------------------------------
C
       DO I=1,N
          XO(I)=X(I)
       ENDDO
C
       RETURN
       END
