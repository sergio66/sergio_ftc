Readme file for top level AIRS_prod08
Scott Hannon, 06 Feb 2011
--------------------------------------------------------------------------------
This is the top level of the fast model production package.  The
"AIRS", "IASI", "CrIS", and "MODIS" subdirs contain Run* code
for the named instrument which should be copied to this level
prior to starting production.  The last fast model created
was CrIS, so the Run*cris dirs are already here.

The nonLTE and solar irradiance is NOT part of this
package. See
   /strowdata2/users/hannon/Solar_data/Chris_Barnet
Convolve the solar irradiance with the SRF.
   /strowdata2/users/hannon/SpinachHome/Fit_deltaR_nonLTE
Sergio creates the nonLTE radiances, the above code
just reformats his data and then fit its.

Run_con is for the water continuum.  I've not re-run
the water con in a number of years.  It contains
both optical depths and fast trans coefficients
for the mtckd(?) 1.0 water con of circa 2000.
For high res instruments such as IASI and AIRS
and even CrIS the water con is approximately a constant
over the spectral width of a channel, so convoluton
with the SRF is not really needed.  The fast model
production assumes this and does NOT add on the
water con when doing "Run_trans".  I forget what
I did for MODIS, but the code is there so it should
easy to figure out.

it is probably not necessary to convolve
the water con optical depth with the instrument function
as the water con is approximate

The Run_rad and Rad_od subdirs are for generic kcarta
radiance and optical depth calcs.  Not really part of
the fast model production package, but useful.

The "kcartaV107" is an updated version of Sergio's 
kcarta code.  It is based on v107 but has been updated
over the years to retain the ability to run with the
updated databases.

The Prof370 and Prof385 dirs are basically the same
contents except for CO2 mixing ratio (370 & 385 ppmv).
The Prof_fcow (fcow = Fixed,CO,Ozone,Water) has fixed mixing
ratios for all gases except CO, O3, and water.  Prof_fmw
has fixed mixing ratios for all gases except Methane and
Water.  Prof_fow has fixed mix ratios for all except Ozone
and Water.  Prof_therm allows all mixing ratios to vary
(I think...you should check that to confirm).
In just a few years a Prof400 should be created.
That is fairly easy, it involves two klayer runs and matlab
to merge subsets of the gases. To get entirely FIXED
mixing ratios you need to use matlab to repeat the layer
amount profile computed for the reference profile for all
the other profiles (fixed amounts need to be truely constant
over all profiles).  You should be able to tell if you are
doing things right if you can re-create Prof370 or Prof385.
If you can do that, then you know how to create another
ProfXXX.

The "Fit_ftc" subdir is where the data produced by the
various Run* is fit to create the various pieces (coef
sets) of a fast model.

---end of file---
