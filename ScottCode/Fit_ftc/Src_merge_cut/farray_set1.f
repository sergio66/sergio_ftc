C-----------------------------------------------------------------------
C      Include file "farray_set1.f"
C
       INTEGER IOERR    ! unit number for error messages
       INTEGER MXCHAN   ! number of channels
       INTEGER MAXLAY   ! number of layers
       INTEGER NCON     ! number of water continuum coefficients
       INTEGER NCOEF    ! number of non-continuum coefficients
       INTEGER NMCOEF   ! number of merged con + non-con coefficients
C
       PARAMETER( IOERR = 0 )
       PARAMETER( MXCHAN = 8461 )
c       PARAMETER( MXCHAN = 2834 )
       PARAMETER( MAXLAY = 100 )
       PARAMETER( NCON = 7 )
       PARAMETER( NCOEF = 8+11+5 )        ! FWO set1
       PARAMETER( NMCOEF = NCON + NCOEF )
C
C-----------------------------------------------------------------------
