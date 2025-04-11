
c
c     RTP Fortran API structures and parameters
c
c     The Fortran structures defined here, RTPHEAD, RTPPROF, and
c     RTPATTR, must match the corresponding C structures rtp_head,
c     rtp_prof, and rtpfatt, and the parameters set below must have
c     the same values as the corresponding C #define parameters.
c     
c     See rtp.h and rtpspec.pdf for more information on the fields
c     defined below.
c

c --------------
c RTP parameters
c --------------

        integer BAD         ! value to be used if no data
        integer LEVPRO      ! levels-type profile flag
        integer LAYPRO      ! layers-type profile flag
        integer AIRSLAY     ! AIRS layers-type profile flag

        integer PROFBIT     ! Summed radiance bit flags
        integer IRCALCBIT   ! bit flag for calculated IR radiance
        integer IROBSVBIT   ! bit flag for observed IR radiance
        integer MWCALCBIT   ! bit flag for calculated MW radiance
        integer MWOBSVBIT   ! bit flag for observed MW radiance
        integer PFIELDSMAX  ! max allowed value of PROFBIT

        integer MAXRHO      ! max num of IR reflectivity points
        integer MAXEMIS     ! max num of IR emissivity points
        integer MAXGAS      ! max number of gases
        integer MAXGASID    ! max gas ID value
        integer MAXLEV      ! max number of levels
        integer MAXCHAN     ! max number of IR channels
        integer MWMAXCHAN   ! max number of MW channels
        integer MWMAXEMIS   ! max num of MW emissivity points
        integer MWMAXSTB    ! max num of MW surface Tb's
        integer MAXPNOTE    ! max profile comment string
        integer MAXUDEF     ! max profile or header udef values

        integer MAXOPEN     ! max num of open RTP files
        integer MAXNATTR    ! max number of attributes
        integer MAXVNAME    ! max num of chars in field or vdata name
        integer MAXANAME    ! max num of chars in attribute name
        integer MAXATEXT    ! max num of chars in attribute text

        integer MAXCALF     ! max cal flag bytes, ceil(MAXCHAN/4)*4
        integer MAXPN4      ! true max for pnote, ceil(MAXPNOTE/4)*4

        ! the following parameters must match the values set in rtp.h
        !
        parameter ( BAD = -9999 )
        parameter ( LEVPRO  = 0 )
        parameter ( LAYPRO  = 1 )
        parameter ( AIRSLAY = 2 )

        parameter ( PROFBIT    = 1 )
        parameter ( IRCALCBIT  = 2 )
        parameter ( IROBSVBIT  = 4 )
        parameter ( MWCALCBIT  = 8 )
        parameter ( MWOBSVBIT  = 16 )
        parameter ( PFIELDSMAX = 31 )

        parameter ( MAXRHO    =  100 )
        parameter ( MAXEMIS   =  100 )
        parameter ( MAXGAS    =   50 )
        parameter ( MAXGASID  =  203 )
        parameter ( MAXLEV    =  120 )
        parameter ( MAXCHAN   = 4500 )
        parameter ( MWMAXCHAN =   20 )
        parameter ( MWMAXEMIS =   10 )
        parameter ( MWMAXSTB  =   10 )
        parameter ( MAXPNOTE  =   80 )
        parameter ( MAXUDEF   =   20 )

        parameter ( MAXOPEN   =    8 )
        parameter ( MAXNATTR  =   32 )

        parameter ( MAXCALF   = ((MAXCHAN-1)/4+1)*4 )
        parameter ( MAXPN4    = ((MAXPNOTE-1)/4+1)*4 )

        ! the following parameters must match the values set in pvdefs.h
        ! and also the field sizes in the RTPATTR structure, defined below
        !
        parameter ( MAXVNAME  =   64 )
        parameter ( MAXANAME  =   64 )
        parameter ( MAXATEXT  =  1024 )


c --------------------
c RTP header structure
c --------------------
c
        STRUCTURE /RTPHEAD/

	  ! profile data
          !
          integer*4  ptype		 ! profile type
          integer*4  pfields		 ! profile field set

          real*4     pmin 		 ! min plevs value
          real*4     pmax 		 ! max plevs value
          integer*4  ngas 		 ! number of gases
          integer*4  glist(MAXGAS)	 ! constituent gas list
          integer*4  gunit(MAXGAS)	 ! constituent gas units
                
	  ! radiance data
	  !
          integer*4  nchan		 ! number of channels
          integer*4  ichan(MAXCHAN)	 ! channel numbers
          real*4     vchan(MAXCHAN)	 ! channel center freq.
          real*4     vcmin 		 ! chan set min freq, including wings
          real*4     vcmax 		 ! chan set max freq, including wings

          integer*4  mwnchan		 ! number of MW channels
          real*4     mwfchan(MWMAXCHAN)	 ! MW channel freq's

	  ! maxes for profile fields
          ! these fields are not saved explicitly in the HDF file
	  ! 
          integer*4  mrho		 ! max number of reflectance points
          integer*4  memis		 ! max number of emissivity points
          integer*4  mlevs		 ! max number of pressure level
          integer*4  mwmemis		 ! max number of MW emissivity points
          integer*4  mwmstb		 ! max number of MW surface Tb points

	  ! user defined fields
	  !
          real*4     udef(MAXUDEF)	 ! user-defined array
          real*4     udef1		 ! user-defined scalar
          real*4     udef2		 ! user-defined scalar

        END STRUCTURE


c ---------------------
c RTP profile structure
c ---------------------
c  
        STRUCTURE /RTPPROF/

	  ! surface data
	  !
          real*4     plat                 ! profile latitude
          real*4     plon                 ! profile longitude 
          real*8     ptime                ! profile time

          real*4     stemp                ! surface temperature
          integer*4  nrho                 ! number of refl. pts
          real*4     rho(MAXRHO)          ! surface reflectance
          real*4     rfreq(MAXRHO)        ! reflectance freq's
          integer*4  nemis                ! number of emis. pts
          real*4     emis(MAXEMIS)        ! surface emissivities
          real*4     efreq(MAXEMIS)       ! emissivity freq's

          real*4     salti                ! surface altitude
          real*4     spres                ! surface pressure
          real*4     smoist               ! soil moisture fractio
          real*4     landfrac             ! land fraction
          integer*4  landtype             ! land type code

          ! MW surface data
          ! 
          integer*4  mwnemis              ! number of MW emis pts
          real*4     mwefreq(MWMAXEMIS)   ! MW emissivity freq's
          real*4     mwemis(MWMAXEMIS)    ! MW emissivities
          integer*4  mwnstb               ! number of MW surf pts
          real*4     mwsfreq(MWMAXSTB)    ! MW surface Tb freq's
          real*4     mwstb(MWMAXSTB)      ! MW surface Tbs

          ! atmospheric data
          !
          integer*4  nlevs                ! number of press lev's
          real*4     plevs(MAXLEV)        ! pressure levels
          real*4     plays(MAXLEV)        ! pressure layers
          real*4     palts(MAXLEV)        ! level altitudes
          real*4     ptemp(MAXLEV)        ! temperature profile
          real*4     gamnt(MAXLEV,MAXGAS) ! constituent amounts
          real*4     gxover(MAXGAS)       ! constituent crossover press
          real*4     txover               ! temperature crossover press
          real*4     co2ppm               ! CO2 mixing ratio
                
          real*4     cfrac                ! cloud fraction 
          integer*4  ctype                ! cloud type code
          real*4     cemis                ! cloud top emissivity
          real*4     cprtop               ! cloud top pressure
          real*4     cprbot               ! cloud bottom pressure
          real*4     cngwat               ! cloud non-gas water
          real*4     cpsize               ! cloud particle size

          real*4     wspeed               ! wind speed
          real*4     wsource              ! wind source

	  ! common radiance data
	  !
          real*4     pobs                 ! observation pressure
          real*4     zobs                 ! observation height
          integer*4  upwell               ! radiation direction

          real*4     scanang              ! IR scan angle
          real*4     satzen               ! IR zenith angle
          real*4     satazi               ! sat azimuth angl
          real*4     solzen               ! sun zenith angle
          real*4     solazi               ! sun azimuth angle

          real*4     mwasang              ! AMSU-A scan angle
          real*4     mwaszen              ! AMSU-A zenith angle
          real*4     mwbsang              ! AMSU-B scan angle
          real*4     mwbszen              ! AMSU-B zenith angle

	  ! calculated radiance data
	  !
          real*4     rcalc(MAXCHAN)       ! calculated IR rad.
          real*4     mwcalc(MWMAXCHAN)    ! calculated MW BT

          ! observed radiance data
          ! 
          real*4     rlat                 ! radiance obs lat.
          real*4     rlon                 ! radiance obs lon.
C         integer*4  rfill                ! align rtime on 8 byte bndry
          real*8     rtime                ! radiance obs time
          real*4     robs1(MAXCHAN)       ! observed IR rad.
          character*1 calflag(MAXCALF)    ! IR calibration flags
          integer*4  irinst               ! IR instrument code

          real*4     mwobs(MWMAXCHAN)     ! observed MW BT
          integer*4  mwinst               ! MW instrument code

          integer*4  findex		  ! file (granule) index 
          integer*4  atrack		  ! along-track index
          integer*4  xtrack		  ! cross-track index

	  ! user defined fields
	  !
          character*80  pnote             ! profile annotation, size MAXPN4
          real*4     udef(MAXUDEF)	  ! user-defined array
          real*4     udef1		  ! user-defined scalar
          real*4     udef2		  ! user-defined scalar

        END STRUCTURE


c -----------------------
c RTP attribute structure
c -----------------------
c
c fname is the name of the field the attribute is to be associated
c       with, 'header' for a general header attribute, or 'profiles'
c       for a general profile attribute.  Its size declaration should
c       be the same as the parameter MAXVNAME.
c
c aname is the attribute name, e.g., 'units' for a field attribute, 
c       or 'TITLE', for a general header attribute.  Its size should
c       also be the same as the parameter MAXANAME.
c
c atext is the attribute text, 'e.g., '48 Fitting Profiles' might be
c       the atext of the header 'TITLE' attribute.  Its size should be 
c       the same as the parameter MAXATEXT.
c
        STRUCTURE /RTPATTR/

          character*64 fname	! associated field name
          character*64 aname	! attribute name
          character*1024 atext	! attribute text

        END STRUCTURE

