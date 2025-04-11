! G10BAF Example Program Text
! Mark 20 Revised. NAG Copyright 2001.
! Mark 20 Revised.  To call thread-safe G05 routines.
!.. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N, NS
      PARAMETER        (N=1000,NS=100)

!.. Local Scalars ..
      real SHI, SLO, WINDOW
      INTEGER          IFAIL, IGEN, NSTEPX, NSTEPY
      LOGICAL          USEFFT

!.. Local Arrays ..
      real FFT(NS), S(NS), SMOOTH(NS), X(N)
      INTEGER          ISEED(4), ISORT(NS)

! .. External Subroutines ..
      EXTERNAL         G01AGF, G05LAF, G10BAF

! .. Executable Statements ..
     WRITE(NOUT,*) 'G10BAF Example Program Results'

! Skip heading in data file
      READ(NIN,*)
      READ(NIN,*) WINDOW
      READ(NIN,*) SLO, SHI

!
!Generate Normal (0,1) Distribution
!
      IGEN = 0
      ISEED(1) = 6698
      ISEED(2) = 7535
      ISEED(3) = 26792
      ISEED(4) = 30140
      IFAIL = 0
      CALL G05LAF(0.0e0,1.0e0,N,X,IGEN,ISEED,IFAIL)

!
!Perform kernel density estimation
!
      USEFFT = .FALSE.
      IFAIL = 0
!
      CALL G10BAF(N,X,WINDOW,SLO,SHI,NS,SMOOTH,S,USEFFT,FFT,IFAIL)
!
!Display smoothed data
      WRITE(NOUT,*)

      NSTEPX = 40
      NSTEPY = 20
      IFAIL = 0
!
      CALL G01AGF(S,SMOOTH,NS,ISORT,NSTEPX,NSTEPY,IFAIL)

      STOP
      END

