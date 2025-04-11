This is the readme file for dir Src_merge_cut

Last update: 13 May 2002, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The fortran programs in this dir are for merging and cutting the
various coefficient files.  There is a separate "farray*.f" include
file for each coefficient type.

The programs are:

   "merge.f" : merges the water continuum with the non-con coefficients.

   "cutlistcoef.f" : pulls out the selected channels of the merged
      coef file.  The selection is handled by a list file, the
      first colum of which must be an integer channel ID.

   "optheader.f" : creates the header record (containing average
      predictors) for the final optran coef file.  Note the header
      file created by this program must be manually cat'ed with the
      coef file.

   "mkdummy.f" : creates a dummy (merged) coef file.  This dummy file
      might be useful for testing the coefs with SARTA (ie to turn off
      a particular coef type, have SARTA use the dummy coef file).

   "dolist.f" : reads the channel ID and freq portion (only) of all
      the coef files to create channel lists for each coef type.
      Note that number of coefs used by each coef type are hard
      coded into the program...edit as needed if changes are made.

Scott.
