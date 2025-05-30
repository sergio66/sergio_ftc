
C      =================================================================
       PROGRAM MERGE
C      =================================================================
C      This program merges con coef data with line coef data.
C      This version for AIRS set1 (8 fixed, 11 water, 5 ozone)
C      and 5 term water con.
C
C      Update: 13 May 2002, Scott Hannon - modified to allow use
C          of a water continuum file containing more channels than
C          strictly required.  Both coef files must be sorted in
C          order of increasing channel ID.
C
C      Update: 17 May 2002, Scott Hannon - change to include file.
C      Update: 11 Aug 2005, S.Hannon - version for MODIS with channel
C         IDs sorted in descending order.

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
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
C      Local variables
       CHARACTER*80 FILNAM
       REAL FRQCHN
       REAL CONCOF(  NCON,MAXLAY)
       REAL   COEF( NCOEF,MAXLAY)
       REAL  RJUNK
       INTEGER IJUNK
       INTEGER IOIN1
       INTEGER IOIN2
       INTEGER IOOUT
       INTEGER ICHAN
       INTEGER I
       INTEGER IC
       INTEGER ICOUNT
       INTEGER IL
       INTEGER IERR
       INTEGER IO
       INTEGER IOC


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
C      Assign I/O unit numbers
       IOIN1=10
       IOIN2=11
       IOOUT=12
C
C      Open the file with con data
       WRITE(6,1010)
 1010  FORMAT('Enter the name of the con fast trans coef data file')
       READ(5,9000) FILNAM
 9000  FORMAT(A80)
C
       OPEN(UNIT=IOIN1,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FILNAM
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
          STOP
       ENDIF
C
C      Open the file with coef data
       WRITE(6,1030)
 1030  FORMAT('Enter the name of the fast trans coef data file')
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOIN2,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FILNAM
          STOP
       ENDIF
C
C      Open the output file to create containing the merged coefs
       WRITE(6,1050)
 1050  FORMAT('Enter the name of the output file to create')
       READ(5,9000) FILNAM
C
       OPEN(UNIT=IOOUT,FILE=FILNAM,FORM='UNFORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FILNAM
          STOP
       ENDIF
C
C      Loop over the channels
       IO=MXCHAN
       IOC=MXCHAN
       ICOUNT=0
       DO I=1,MXCHAN
C
C         Read coef data
          READ(IOIN2,END=99) ICHAN, FRQCHN, ((COEF(IC,IL),IC=1,NCOEF),
     $       IL=1,MAXLAY)
C         Verify coef data is sorted by descending ID
          IF (ICHAN .GT. IO) THEN
             write(6,*) 'ichan,io=',ICHAN,IO
             WRITE(IOERR,1060) 'coef'
 1060        FORMAT('Error: ',A4,' file not sorted by descending ID')
             STOP
          ENDIF
          IO=ICHAN
C
C         Read con data
 10       READ(IOIN1,END=98) IJUNK, RJUNK, ((CONCOF(IC,IL),IC=1,NCON),
     $       IL=1,MAXLAY)
C         Verify wcon data is sorted by descending ID
          IF (IJUNK .GT. IOC) THEN
             write(6,*) 'ijunk,ioc=',IJUNK,IOC
             WRITE(IOERR,1060) 'wcon'
             STOP
          ENDIF
          IOC=IJUNK
C         Check if wcon ID exceeds coef ID
          IF (IJUNK .LT. ICHAN) THEN
 98          WRITE(IOERR,1070)
 1070        FORMAT('Error: wcon ID less than coef ID.  The wcon file'
     $       ' must contain all',/,'channels present in the coef'
     $       'file, and both files',/,'must be sorted by descending'
     $       'ID.')
             STOP
          ENDIF
C         Loop until IDs match (or END=98 error)
          IF (IJUNK .NE. ICHAN) GOTO 10
C
C         Check that FRQCHN=RJUNK
          IF (ABS(FRQCHN-RJUNK) .GT. 1.1) THEN
             WRITE(IOERR,1080) ICHAN, FRQCHN, IJUNK, RJUNK
 1080        FORMAT('Error: chan freqs do not match.',/,
     $       'coef file:',I5,1X,F10.4,' con file:',I5,1X,F10.4)
             STOP
          ENDIF

C         Write the output file
          WRITE(IOOUT) ICHAN, FRQCHN, ((CONCOF(IC,IL),IC=1,NCON),
     $       (COEF(IC,IL),IC=1,NCOEF),IL=1,MAXLAY)
C
          ICOUNT=ICOUNT+1
ccc
c      write(6,*) 'ichan=',ICHAN
ccc
       ENDDO
C
 99    WRITE(6,1090) ICOUNT
 1090  FORMAT('Merged',I5,' channels')
       CLOSE(IOIN2)
       CLOSE(IOIN1)
       CLOSE(IOOUT)
C
       STOP
       END
