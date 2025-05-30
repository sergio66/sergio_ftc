C-----------------------------------------------------------------------
C      Include file "farray_set4.f"
C
       INTEGER IOERR    ! unit number for error messages
       INTEGER MXCHAN   ! number of channels
       INTEGER MAXLAY   ! number of layers
       INTEGER NCON     ! number of water continuum coefficients
       INTEGER NCOEF    ! number of non-continuum coefficients
       INTEGER NMCOEF   ! number of merged con + non-con coefficients
C
       PARAMETER( IOERR = 0 )
c       PARAMETER( MXCHAN = 8461 )
       PARAMETER( MXCHAN = 2834 )
       PARAMETER( MAXLAY = 100 )
       PARAMETER( NCON = 7 )
       PARAMETER( NCOEF = 11+11+3+13 )    ! FCOW set4
       PARAMETER( NMCOEF = NCON + NCOEF )
C
C-----------------------------------------------------------------------
