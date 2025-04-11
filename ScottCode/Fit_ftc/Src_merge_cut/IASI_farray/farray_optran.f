C-----------------------------------------------------------------------
C      Include file "farray_optran.f"
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
       PARAMETER( MAXLAY = 300 )
       PARAMETER( NCON = 0 )
       PARAMETER( NCOEF = 9 )             ! water OPTRAN
       PARAMETER( NMCOEF = NCON + NCOEF )
C
C
C      Extra parameters used by "optheader.f"
       INTEGER NOWAVG  ! Number of Optran Water predictor Averages
       INTEGER MAXGAS  ! Max number of gases in optran grid file
C
       PARAMETER( NOWAVG = 4 )
       PARAMETER( MAXGAS = 5 )
C
C-----------------------------------------------------------------------
