Readme file for Run_so2 and Run_n2o_hno3
Scott Hannon, 07 February 2011
-------------------------------------------------------------------------------
These scripts are somewhat similar to the runtrans and runtherm scripts.
Again, for some instuments the run script will require a command line
argument to tell it which band/version to do.  See the header of the
script for valid arguments.  As with the trans and therm run scripts, 
these also loop over profile numbers 1-48 and run kcarta and matlab
convolution to generate mat files for SO2 or N2O and HNO3 regression
data.  When all 48 profiles are done, you should run the matlab merge
script to gather up the data for regression, and I think it also
includes some profile info.

For IASI there is also a Run_ch4 for shortwave CH4, which is
modelled similar to N2O, HNO3, etc.

After you have run runtrans, runtherm, run_so2, and run_n2o_hno3,
you are ready to start regressions in the Fit_ftc dir.  See the
readme file there for further instructions.

---end of file---
