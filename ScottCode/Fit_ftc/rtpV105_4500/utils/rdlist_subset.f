C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    RDLIST_SUBSET
C
!F77====================================================================


!ROUTINE NAME:
C    RDLIST


!ABSTRACT:
C    Read a text file list of ID numbers/indices.


!CALL PROTOCOL
C    RDLIST ( IOUN, FLIST, MXLIST, NLIST, LIST )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IOUN    I/O unit number             none
C    CHAR*70   FLIST   list filename               none
C    INTEGER   MXLIST  max # items allowed on list none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NLIST   number of items on list     none
C    INT arr   LIST    list of ID/index #          none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    SARTA_RTP


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incSUB.f : include file of parameter statements accessed during
C       compilation only.
C    unit IOUN : text file, channel list


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 2001 version of the AIRS RTP KLAYERS+SARTA package.
C    Code by S.Hannon/H.Motteler/L.Strow.
C
C    Opens the list text file and reads it one line at a time. If
C    it detects a problem it writes an error message and quits.
C    The routine loops over the entries until it reaches the end
C    end of the list file.
C
C    The input list must be sorted in ascending order and have
C    no repeats.
C


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    12 Mar 2001 Scott Hannon   Created based on Aug00 RDLIST for sarta


!END====================================================================

C      =================================================================
       SUBROUTINE RDLIST ( IOUN, FLIST, MXLIST, NLIST, LIST )
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
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER   IOUN        ! I/O unit number for list text file
       CHARACTER*70 FLIST    ! list filename
       INTEGER MXLIST        ! max # of allowed items on list
C
C      Output
       INTEGER  NLIST        ! number of items found on list
       INTEGER   LIST(*)     ! list of items (ID # or index)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IERR
       INTEGER  IJUNK
       INTEGER  IPREV
       REAL  RJUNK
       CHARACTER*80  CLINE


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
C      -----------------------------------
C      Open channel list file
C      -----------------------------------
C      Open the channel list text file
       OPEN(UNIT=IOUN,FILE=FLIST,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1030) IERR, FLIST
 1030     FORMAT('ERROR ',I5,' opening list file:',/,A70)
          STOP
       ENDIF
C

C      ------------------
C      Read the list file
C      ------------------
C      Read a line of text from the list file
       NLIST=0
       IPREV=-65535  ! some large negative integer (eg 2^16 - 1)
 10    READ(IOUN,9000,END=910) CLINE
 9000  FORMAT(A80)
C
       IF (CLINE(1:1) .NE. '!' .AND. CLINE(1:1) .NE. '%') THEN
C         It's data, not a comment
          READ(CLINE,*) RJUNK  ! expect integer but read as real
          IJUNK=NINT(RJUNK)
C
C         Check that new item is larger than previous item
          IF (IJUNK .LE. IPREV) THEN
             WRITE(IOERR,1060) NLIST, IJUNK, FLIST
 1060        FORMAT('ERROR! list item #',I6,'=',I6,', list file:',/,
     $       A70,/,'List file must be sorted in ascending order ',
     $       'with no repeats')
             STOP
          ENDIF
C
C         Update list
          NLIST=NLIST + 1
          LIST(NLIST)=IJUNK
          IPREV=IJUNK
       ENDIF
C
C      Read the next line
       IF (NLIST .LT. MXLIST) GOTO 10
C
 910   CLOSE(IOUN)
C
       RETURN
       END
