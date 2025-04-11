C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              SUBSET
C
!F77====================================================================


!ROUTINE NAME: SUBSET


!ABSTRACT: Subset a RTP file


!CALL PROTOCOL: main program


!INPUT PARAMETERS: none


!OUTPUT PARAMETERS: none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: main program


!PARENT(S): main program


!ROUTINES CALLED:
C    RDINFO : gets info from user about the subset processing
C    OPNRTP : opens input and output RTP files
C    RDRTP  : reads input RTP file
C    WRTRTP : writes output RTP file


!FILES ACCESSED:
C    IOERR : error messages
C    IOINFO: warning/info messages


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    SUBSET program is basically a driver for the routines.
C       RDINFO: processes command line arguments to get info on
C          what & how to subset the input RTP.  Currently set
C          to allow subsetting based on:
C             1) gas list
C             2) channel list
C             3) profile list
C       
C       rtpopen: open/read input RTP
C       SUBLST: check subsetting lists
C       rtpopen: open/write output RTP
C       rtpread: read current profile from input RTP
C    Subset the current profile as needed.
C       rtpwrite: write a subsetted current profile to output RTP.
C    Close files.  The end.
C
C    Note: as currently configured, this SUBSET program does not
C    add any new info to the output RTP file; the output is strictly
C    a subset of the input.
C

!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    None


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 14 Mar 2001 Scott Hannon      Created
C 27 Mar 2001 Scott Hannon      Updated for HEAD.pfields (was obflag)
C 15 Mar 2002 Scott Hannon      Add subsetting for new (rtpV104) PROF
C                                  fields gxover and calflag.


!END====================================================================

C      =================================================================
       PROGRAM SUBSET
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
      include 'incSUB.f'
      include 'rtpdefs.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none (main program)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
C      generic/local
       INTEGER I
       INTEGER IP
       INTEGER IPS
       INTEGER L
       INTEGER NHATT           ! number of header attributes
       CHARACTER*1 MODE        ! RTP file mode (r=read or w=write)
C
C      for LENNB
       INTEGER LENNB           ! function LENNB
C
C      for RDINFO
       CHARACTER*70 FIN        ! input RTP filename
       CHARACTER*70 FOUT       ! output RTP filename
       CHARACTER*70 COMMNT     ! comment string to add to output RTP
       CHARACTER*70 FCLIST     ! channel list filename
       CHARACTER*70 FPLIST     ! profile list filename
       CHARACTER*70 FGLIST     ! gas list filename
C
C      for RDLIST
       INTEGER IOUN            ! generic I/O unit number
       INTEGER NCLIST          ! # items in subset channel list
       INTEGER NPLIST          ! # items in subset profile list
       INTEGER NGLIST          ! # items in subset gas list
       INTEGER CLIST(MXCHAN)   ! subsetting channel list 
       INTEGER PLIST(MXPROF)   ! subsetting profile list 
       INTEGER GLIST(MXGAS )   ! subsetting gas list
C
C      for SUBLST
       INTEGER NCMAST          ! # items on master channel list
c       INTEGER NPMAST          ! # items on master profile list
       INTEGER NGMAST          ! # items on master gas list
       INTEGER CMAST(MXCHAN)   ! master channel list 
c       INTEGER PMAST(MXPROF)   ! master profile list 
       INTEGER GMAST(MXGAS )   ! master gas list
       INTEGER CIND(MXCHAN)    ! channel subset indices on master list 
       INTEGER PIND(MXPROF)    ! profile subset indices on master list 
       INTEGER GIND(MXGAS)     ! gas subset indices on master list 
C
C      for OPNRTP
       INTEGER STATUS          ! status of RTP file open
       INTEGER IOPCI           ! input RTP file I/O channel
       INTEGER IOPCO           ! output RTP file I/O channel
C
C      for N2BITS
       INTEGER*4 NUMBER
       LOGICAL LFLAGS(32)
       LOGICAL LPROF           ! file includes profile data?
       LOGICAL LIRCAL          ! file includes calc'ed IR radiance?
       LOGICAL LIROBS          ! file includes observed IR radiance?
       LOGICAL LMWCAL          ! file includes calc'ed IR radiance?
       LOGICAL LMWOBS          ! file includes observed MW radiance?
C
C      RTP structures (see "rtpdefs.f")
       INTEGER rtpopen                  ! function rtpopen
       INTEGER rtpclose                 ! function rtpclose
       INTEGER rtpread                  ! function rtpread
       INTEGER rtpwrite                 ! function rtpwrite
       RECORD /RTPHEAD/ HEAD            ! header data
       RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
       RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes
       RECORD /RTPPROF/ PROF

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      -----------------------------------
C      Assign generic file I/O unit number 
C      -----------------------------------
C      Note: this is NOT used by the RTP files!
       IOUN=11
C

C      -----------------------------
C      Get info about the subsetting
C      -----------------------------
       CALL RDINFO(FIN, FOUT, COMMNT, FCLIST, FPLIST, FGLIST)
C

C      ----------
C      Read lists (if any)
C      ----------
C      Channel list
       I=ICHAR(FCLIST(1:1))
       IF (I .GT. 0) THEN
          CALL RDLIST(IOUN, FCLIST, MXCHAN, NCLIST, CLIST)
          WRITE(IOINFO,1010) NCLIST, ' chans', FCLIST
 1010     FORMAT('Read',I5,A6,' from list file:',/,A70)
       ELSE
          NCLIST=-1
       ENDIF
C
C      Profile list
       I=ICHAR(FPLIST(1:1))
       IF (I .GT. 0) THEN
          CALL RDLIST(IOUN, FPLIST, MXPROF, NPLIST, PLIST)
          WRITE(IOINFO,1010) NPLIST, ' profs', FPLIST
       ELSE
          NPLIST=-1
       ENDIF
C
C      Gas list
       I=ICHAR(FGLIST(1:1))
       IF (I .GT. 0) THEN
          CALL RDLIST(IOUN, FGLIST, MXGAS, NGLIST, GLIST)
          WRITE(IOINFO,1010) NGLIST, ' gases', FGLIST
       ELSE
          NGLIST=-1
       ENDIF
C

C      -------------------
C      Open RTP input file
C      -------------------
       MODE='r'
       STATUS=rtpopen(FIN, MODE, HEAD, HATT, PATT, IOPCI)
ccc
c       print *, 'read open status = ', STATUS
ccc
C
C      -------------
C      Check pfields
C      -------------
       NUMBER=HEAD.pfields
       CALL N2BITS(NUMBER, LFLAGS)
       LPROF =LFLAGS(1)  ! PROFBIT is bit1
       LIRCAL=LFLAGS(2)  ! IRCALCBIT is bit2
       LIROBS=LFLAGS(3)  ! IROBSVBIT is bit3
       LMWCAL=LFLAGS(4)  ! MWCALCBIT is bit4
       LMWOBS=LFLAGS(5)  ! MWOBSVBIT is bit5
C

C      -----------
C      Check lists
C      -----------
C      Channel list
       NCMAST=HEAD.nchan
       IF (NCLIST .GT. 0) THEN
C         Want a subset of the channels
          DO I=1,NCMAST
             CMAST(I)=HEAD.ichan(I)
          ENDDO
          CALL SUBLST(NCMAST, CMAST, NCLIST, CLIST, CIND)
       ENDIF
C
C      Profile list
C      Note: currently RTP header does not have a var for # profiles
       IF (NPLIST .GT. 0) THEN
C         Want a subset of the profiles
C         Note: PLIST is already indices so don't need to call SUBLST
          DO I=1,NPLIST
             PIND(I)=PLIST(I)
          ENDDO
       ENDIF
C
C      Gas list
       NGMAST=HEAD.ngas
       IF (NGLIST .GT. 0) THEN
C         Want a subset of the gases
          DO I=1,NGMAST
             GMAST(I)=HEAD.glist(I)
          ENDDO
          CALL SUBLST(NGMAST, GMAST, NGLIST, GLIST, GIND)
       ENDIF
C

C      -----------------
C      Subset the header
C      -----------------
C      Channels
       IF (NCLIST .GT. 0) THEN
          HEAD.nchan=NCLIST
          DO I=1,NCLIST
             HEAD.ichan(I)=HEAD.ichan( CIND(I) )
             HEAD.vchan(I)=HEAD.vchan( CIND(I) )
          ENDDO
       ENDIF
C
C      Gases
       IF (NGLIST .GT. 0) THEN
          HEAD.ngas=NGLIST
          DO I=1,NGLIST
             HEAD.glist(I)=HEAD.glist( GIND(I) )
             HEAD.gunit(I)=HEAD.gunit( GIND(I) )
          ENDDO
       ENDIF
C

C      -------------------------------
C      Add COMMNT to header attributes
C      -------------------------------
C      Count the number of header attributes
       I=1
       DO WHILE (ICHAR(HATT(I).fname) .NE. 0 .AND. I .LE. MAXNATTR)
          I=I + 1
       ENDDO
       NHATT=I
       L=LENNB(COMMNT)
       HATT(NHATT).fname='header'  // CHAR(0)
       HATT(NHATT).aname='subset' // CHAR(0)
       HATT(NHATT).atext=COMMNT(1:L) // CHAR(0)
C      Add a char(0) to end of attributes if less than maxnattr
       IF (NHATT .LT. MAXNATTR) THEN
          HATT(NHATT + 1).fname=CHAR(0)
       ENDIF
C

C      --------------------
C      Open output RTP file
C      --------------------
       MODE='c'
       STATUS=rtpopen(FOUT, MODE, HEAD, HATT, PATT, IOPCO)
ccc
c       print *, 'read open status = ', STATUS
ccc
C

C      -----------------------------------------------------------------
C      Start of while loop over profiles
       IP=0  ! initialize input profile loop counter
       IPS=1 ! initialize output profile counter
C      ------------------------
C      Read the current profile
C      ------------------------
 10    STATUS=rtpread(IOPCI, PROF)
C
       IF (STATUS .EQ. -1) GOTO 99  ! reached End Of File
C
       IP=IP + 1  ! increment profile loop counter
C

C      ------------------
C      Skip this profile?
C      ------------------
       IF (NPLIST .GT. 0 .AND. IP .NE. PIND(IPS)) GOTO 10
C

C      --------------
C      Subset profile
C      --------------
C      Channel
       IF (NCLIST .GT. 0) THEN
          IF (LIROBS .AND. LIRCAL) THEN
             DO I=1,NCLIST
                PROF.rcalc(I)=PROF.rcalc( CIND(I) )
                PROF.robs1(I)=PROF.robs1( CIND(I) )
                PROF.calflag(I)=PROF.calflag( CIND(I) )
             ENDDO
          ELSEIF (LIRCAL .AND. .NOT. LIROBS) THEN
             DO I=1,NCLIST
                PROF.rcalc(I)=PROF.rcalc( CIND(I) )
             ENDDO
          ELSEIF (LIROBS .AND. .NOT. LIRCAL) THEN
             DO I=1,NCLIST
                PROF.robs1(I)=PROF.robs1( CIND(I) )
                PROF.calflag(I)=PROF.calflag( CIND(I) )
             ENDDO
          ENDIF
       ENDIF
C
C      Gases
       IF (NGLIST .GT. 0 .AND. LPROF) THEN
          DO I=1,NGLIST
             PROF.gxover(I)=PROF.gxover( GIND(I) )
C            Loop over layers
             DO L=1,PROF.nlevs
                PROF.gamnt(L,I)=PROF.gamnt(L, GIND(I) )
             ENDDO
          ENDDO
       ENDIF

C      --------------------------------
C      Write profile to RTP output file
C      --------------------------------
       STATUS=rtpwrite(IOPCO, PROF)
C
       IF (STATUS .EQ. -1) GOTO 99  ! reached End Of File on output!
C

C      --------------------
C      Loop to next profile
C      --------------------
       IPS=IPS + 1
       IF (NPLIST .GT. 0 .AND. IPS .GT. NPLIST) GOTO 99
       GOTO 10

 99    CONTINUE  ! end of while loop over profiles
C      -----------------------------------------------------------------

C      -----------
C      Close files
C      -----------
C      Close RTP files
       STATUS=rtpclose(IOPCI)
       STATUS=rtpclose(IOPCO)
C
       STOP
       END
