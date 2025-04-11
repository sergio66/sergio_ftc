C***********************************************************************
C
C      File farray_useconv.f (included file for useconv)
C
C      Purpose : Assign some array dimensions
C
C***********************************************************************
C
       INTEGER IOERR   ! unit number for error messages
       INTEGER IDFIX   ! HITRAN gas ID number to use for fixed gases
       INTEGER MAXLAY  ! number of layers
       INTEGER MAXANG  ! number of angles
       PARAMETER( IOERR=0 )
       PARAMETER( IDFIX=2 )
       PARAMETER( MAXLAY=100 )
       PARAMETER( MAXANG=12 )
C
       INTEGER MAXBOG  ! max possible number of gases to breakout & fit
       PARAMETER( MAXBOG=6 )
C
       INTEGER NUMGAS  ! always set to one
       PARAMETER( NUMGAS=1 )
C
       INTEGER NUMSET  ! max num of gases/trans in conv trans data files
       PARAMETER( NUMSET=5 )
C
C***********************************************************************
