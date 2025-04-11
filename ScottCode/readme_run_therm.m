Readme file Run_therm
Scott Hannon, 07 February 2011
-------------------------------------------------------------------------------
Doing runtherm is fairly similar to doing runtrans.  For some instruments
the script needs a command line argument to specify the band.  See the header
of the script.  For AIRS the bands are "long" (which includes midwave)
and "short".  The runtrans script loops over the profile numbers
and does a kcarta radiance calc and matlab convolution for the
current profile and saves it as a mat file.  The mat files
contains multiple radiances for with and without reflected thermal
and multiple angles.  When runtherm finishes you will have 48
mat files. The mergetherm.m script is used to gather these up
for use with the reflected therm regression code....I think it
also folds in some profile info.

---end of file---
