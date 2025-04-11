Scott Hannon, 16 February 2007

This "4500" version of RTP v1.05 is standard RTP v1.05 but with
the following include file parameters assigned as follows:

MAXGAS
   The max number of gases has been set to 50.  44 is the
   default "all gases" output by the current klayers.  kCARTA uses
   a maximum of 40 gases (including gas IDs 101 and 102 which are
   NOT used by klayers) and ignores unneeded gases.

MAXGASID
   The max gas ID has been set to 203.  The current
   HITRAN 2004 database goes up to ID 39.  Cross-section (XSEC)
   gases are typically assigned IDs starting at 50 and going
   to perhaps the mid-70s.  We are reserving IDs 201 thru 203
   for generic cloud identifiers cloud1 thru cloud3.

MAXCHAN
   The max number of channels has been set to 4500.
   This is close to the maximum possible with the current RTP
   variables and array sizes.

NOTE: the makefile in the src dir copies the two include files
named below to the include directory prior to compilation, so
any changes to the include file should be made in the "src" dir.

Include files
   rtpdefs.f
   rtp.h
