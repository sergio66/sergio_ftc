C
       PROGRAM CutListCoef
C
C      Cut out a list selected set of channels from a fast
C      transmittance data file.  The output coef file is sorted
C      in the order specified by the channel list.  The channel
C      list is one or more columns, with the first columns
C      being the channel ID number (only the 1st column is read in).
C
C      Update: Scott Hannon, 17 May 2002 - changed to use include
C      file, add CLOSE(IOOUT), and rename MAXLEV to MAXLAY.


C-----------------------------------------------------------------------
C      INCLUDE
C-----------------------------------------------------------------------
       INCLUDE 'farray.f'


C-----------------------------------------------------------------------
C      Local Variables
C-----------------------------------------------------------------------
C
       CHARACTER*79  CLINE
       CHARACTER*79   FNAM
C
C      For data read from coef file
       INTEGER IDCHAN(MXCHAN)
       REAL   FREQ(MXCHAN)
       REAL   COEF(NMCOEF,MAXLAY,MXCHAN)
C
C      For channel list
       INTEGER ICLIST(MXCHAN)
C
       INTEGER      I
       INTEGER     IC
       INTEGER   IERR
       INTEGER     IL
       INTEGER   IOIN
       INTEGER IOLIST
       INTEGER  IOOUT
       INTEGER      J
       INTEGER NCHANC
       INTEGER NCHANL
       INTEGER NWRITE
       REAL  RJUNK


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below...
C***********************************************************************
C***********************************************************************

C      Assign I/O unit numbers
       IOLIST=9
       IOOUT=9
       IOIN=10
C
C      ------------------
C      Read the list file
C      ------------------
C
C      Open the list file
       WRITE(6,1010)
 1010  FORMAT('Enter name of list file')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
       OPEN(UNIT=IOLIST,FILE=FNAM,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAM
 1020     FORMAT('Error',I6,' opening file:',/,A79)
          STOP
       ENDIF
C
C      Read the list file
       NCHANL=0
 10    READ(IOLIST,9000,END=910) CLINE
       IF (CLINE(1:1) .NE. '!') THEN
          NCHANL=NCHANL + 1
C         Read in list as real and convert to integer
C         Note: this is convenient for lists created with matlab
          READ(CLINE,*) RJUNK
          ICLIST(NCHANL)=NINT(RJUNK)
       ENDIF
       GOTO 10
 910   CLOSE(IOLIST)
C
C
C      ------------------
C      Read the coef file
C      ------------------
       WRITE(6,1030)
 1030  FORMAT('Enter the name of the file with the coefs')
       READ(5,9000) FNAM
       OPEN(UNIT=IOIN,FILE=FNAM,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAM
          STOP
       ENDIF
       I=1
 20    READ(IOIN,END=920) IDCHAN(I), FREQ(I),
     $    ( (COEF(IC,IL,I),IC=1,NMCOEF ),IL=1,MAXLAY)
       I=I + 1
       IF (I .LE. MXCHAN) GOTO 20
 920   CLOSE(IOIN)
       NCHANC=I - 1
C
C
C      ---------------------
C      Write the output file
C      ---------------------
       WRITE(6,1040)
 1040  FORMAT('Enter name of output file to create')
       READ(5,9000) FNAM
       OPEN(UNIT=IOOUT,FILE=FNAM,FORM='UNFORMATTED',STATUS='NEW',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAM
          STOP
       ENDIF
C
       NWRITE=0
       DO I=1,NCHANL
          DO J=1,NCHANC
             IF (IDCHAN(J) .EQ. ICLIST(I)) THEN
                WRITE(IOOUT) IDCHAN(J), FREQ(J),
     $          ( (COEF(IC,IL,J),IC=1,NMCOEF ),IL=1,MAXLAY)
                NWRITE=NWRITE + 1
             ENDIF
          ENDDO
       ENDDO
C
       CLOSE(IOOUT)
C
       IF (NWRITE .LT. NCHANL) THEN
          WRITE(IOERR,1060) NWRITE, NCHANL
 1060     FORMAT('Error, only found',I6,' of',I6,' channels.')
       ELSE
          WRITE(6,1070) NCHANL
 1070     FORMAT('OK, pulled out all',I6,' selected channels.')
       ENDIF
C
       STOP
       END
