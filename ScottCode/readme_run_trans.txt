Readme file for the Run_trans* subdirs
Scott Hannon, 07 February 2011
-------------------------------------------------------------------------------
The Run_trans* subdirs for IASI, CrIS and AIRS all differ from each
other in instrument specific ways, but otherwise are similar.
There is one or more "runtrans*" scripts which runs kcarta and
then matlab to convolve the mono transmittance and save a mat file of
results.  You should not need to edit the runtrans scripts except
for the profile numbers.  See the header section of the script. As
a test I usually run off just the first profile and then check the
output to see if things appear to be working. Then I edit the
profile numbers to do the remaining profiles (ie 2-48).

Most of the runtrans scripts except one or more command line
arguments to tell it which band or bandtype to do.  Look over
the header section of the script to see what are valid
arguments.  For example, "runtrans fow_long" might mean
do the longwave band with breakouts "fow".  In some cases
there are more than one runtrans script.  For example, for
AIRS did 5 separate fast models for "130", "140", "150",
"130x" and "140x", where the number refers to the yoffset
and the "x" refers to the pre-Nov 2003 SRF fringes (the lack
of an x means post-Nov 2003 SRF fringes).  For CrIS, we did fast
models for a few different OPDs.  The standard 0.8/0.4/0.2 cm,
but also 0.8/0.8/0.8 and maybe 0.8/0.4/0.4 cm (the three
numbers being the three bands).

When done, you'll have 48 mat files, but the regression codes
expect fortran binary files. The doall_wrtconvdat.m script is
used to convert the mat to dat.  Besides re-writing the mat
optical depths to fortran, it also includes the profile info.

---end of file---
