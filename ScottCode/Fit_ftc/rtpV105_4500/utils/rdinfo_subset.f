c rdinfo processes command line arguments
c
c       klayers fin=test.rtp fout=out.rtp
c
c to compile
c   Absoft/Linux: f77 -N109 -o klayers $(SRC) -lU77
c   SGI Irix: no special compiler options are needed
C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              RDINFO_SUBSET
C
!F77====================================================================


!ROUTINE NAME: RDINFO


!ABSTRACT:
C    Get info about the user supplied profile.  Gets info from
C    command line arguments else uses the hardcoded defaults.
C    This version for command-line arguments is a total re-write,
C    and even a few of the passed parameters are different.


!CALL PROTOCOL:
C    RDINFO(FIN, FOUT, COMMNT, FCLIST, FPLIST, FGLIST)


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*70   FIN     input RTP filename          none
C    CHAR*70   FOUT    output RTP filename         none
C    CHAR*70   COMMNT  output RTP header comment   none
C    CHAR*70   FCLIST  channel list filename       none
C    CHAR*70   FPLIST  profile list filename       none
C    CHAR*70   FGLIST  gas list filename           none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): SUBSET


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    IOERR : error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Gets various info about the subsetting to be done on the
C    input RTP to create the outpuyt RTP.
C
C    fin : name of input file; string [required]
C
C    fout : name of output file; string [required]
C
C    comment : new comment to be added to output RTP header; string
C       Note: comment string should be enclosed in quotes
C
C    chanlist : name of channel list textfile; string
C
C    proflist : name of profile list textfile; string
C
C    gaslist : name of gas list textfile; string
C


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 13 Mar 2001 H.Motteler/S.Hannon Created based on RDINFO for klayers
C 27 Mar 2001 Scott Hannon      Changed default list file names from ''
C                               to CHAR(0)
C 19 Nov 2001 Scott Hannon      Add trap for NARG = 0

!END====================================================================


C      =================================================================
       SUBROUTINE RDINFO(FIN, FOUT, COMMNT, FCLIST, FPLIST, FGLIST)
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incSUB.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      From "util.f"
C      function LENNB = length of string excluding trailing blanks
C      function STR2BO = converts true/false string to boolean (LOGICAL)
C      subroutine UPCASE = converts a string to upper case


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input:
C      none
C
C      Output:
       CHARACTER*70 FIN
       CHARACTER*70 FOUT
       CHARACTER*70 COMMNT
       CHARACTER*70 FCLIST
       CHARACTER*70 FPLIST
       CHARACTER*70 FGLIST

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER I
       INTEGER IARGC
       INTEGER J
       INTEGER NARGS

       CHARACTER*80 BUF
       CHARACTER*80 VAL
       CHARACTER*80 VAR


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      ------------
C      Set defaults
C      ------------
       FIN='rtp_in.rtp'             ! input filename
       FOUT='rtp_out.rtp'           ! output filename
       FCLIST=CHAR(0)               ! channel list
       FPLIST=CHAR(0)               ! profile list
       FGLIST=CHAR(0)               ! gas list
C      Comment string
       COMMNT='This RTP file has been processed by SUBSET'

C      -----------------------------------------------------------------
C      Loop on program parameters
C      --------------------------
C      Determine the number of command-line arguments
       NARGS=IARGC()
C
       IF (NARGS .EQ. 0) THEN
          WRITE(IOERR,1010)
 1010     FORMAT('subset_rtp must be run with arguments (see doc)')
          WRITE(IOERR,1011)
 1011     FORMAT('   fin=<filename> rtp input file {manditory}',/,
     $    '   fout=<filename> rtp output file {manditory}',/,
     $    '   comment=<string> {optional}',/,
     $    '   chanlist=<filename> channel list file {optional}',/,
     $    '   proflist=<filename> profile list file {optional}',/,
     $    '   gaslist=<filename> gas list file {optional}')
          WRITE(IOERR,1013)
 1013     FORMAT('{List files must contain a column of ascending',
     $       ' integer values}')
          STOP
       ENDIF
C
C      Loop over the command-line arguments
       DO I = 1, NARGS
C
C         Pull out the ith argument
          CALL GETARG(I, BUF)
C
C         Find the "=" character in the command-line argument string
          J=INDEX(BUF, '=')
C
          IF (J .NE. 0) THEN
C
C            Name of variable
             VAR = BUF(1:J-1)
             CALL UPCASE(VAR)
C
C            Specified value
             VAL = BUF(J+1:LEN(BUF))
C
C            Big "IF" to set parameters
C            ----------------------------
             IF (VAR(1:3) .EQ. 'FIN') THEN
                FIN=VAL

             ELSEIF (VAR(1:4) .EQ. 'FOUT') THEN
                FOUT=VAL

             ELSEIF (VAR(1:7) .EQ. 'COMMENT') THEN
                COMMNT=VAL

             ELSEIF (VAR(1:8) .EQ. 'CHANLIST') THEN
                FCLIST=VAL

             ELSEIF (VAR(1:8) .EQ. 'PROFLIST') THEN
                FPLIST=VAL

             ELSEIF (VAR(1:7) .EQ. 'GASLIST') THEN
                FGLIST=VAL

             ELSE
                WRITE(IOERR,1020) VAR
 1020           FORMAT('Unknown command-line argument: ',A6)
                STOP

             ENDIF

          ENDIF
       ENDDO  ! end of loop over command-line arguments
C
       RETURN
       END
