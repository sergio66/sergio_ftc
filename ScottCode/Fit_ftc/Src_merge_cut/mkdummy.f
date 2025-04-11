C
       PROGRAM MKDUMMY
C
C      Make a dummy coef file
C      The dummy file contains dummy data for one channel,
C      fake channel MXCHAN+1.
C
C Created: Scott Hannon, 17 May 2002


C-----------------------------------------------------------------------
C      INCLUDE
C-----------------------------------------------------------------------
       INCLUDE 'farray.f'


C-----------------------------------------------------------------------
C      Local Variables
C-----------------------------------------------------------------------
C
       CHARACTER*79   FNAM
C
C      For coef data
       INTEGER IDCHAN(MXCHAN)
       REAL   FREQ(MXCHAN)
       REAL   COEF(NMCOEF,MAXLAY,MXCHAN)
C
       INTEGER     IC
       INTEGER   IERR
       INTEGER     IL
       INTEGER  IOOUT


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below...
C***********************************************************************
C***********************************************************************

C      Assign I/O unit numbers
       IOOUT=9
C

C      -------------------------
C      Assign dummy channel info
C      -------------------------
       DO IL=1,MAXLAY
          DO IC=1,NMCOEF
             COEF(IC,IL,1)=0.0
          ENDDO
       ENDDO
       IDCHAN(1)=MXCHAN + 1
       FREQ(1)=0.0


C      ---------------------
C      Write the output file
C      ---------------------
       WRITE(6,1040)
 1040  FORMAT('Enter name of output file to create')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
       OPEN(UNIT=IOOUT,FILE=FNAM,FORM='UNFORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAM
 1020     FORMAT('Error',I6,' opening file:',/,A79)
          STOP
       ENDIF
C
       WRITE(IOOUT) IDCHAN(1), FREQ(1),
     $     ( (COEF(IC,IL,1),IC=1,NMCOEF ),IL=1,MAXLAY)
C
       CLOSE(IOOUT)
C

       STOP
       END
