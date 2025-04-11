#
# Makefile for AIRS fast transmittance coefficient regression fit
# programs.  Most of the work is done by make_all which should be
# editted as needed for the options appropriate for your compiler.
#

# ------
# Builds
# ------

default:
	echo 'useage:  make <fowp, fmw, fcow_sun, or fowp_sun>'

iasi:
	rm -f keff.f keff_optran.f fitk9_optran.f
	ln -s keff_iasi.f keff.f
	ln -s keff_optran_iasi.f keff_optran.f
	ln -s fitk9_optran_iasi.f fitk9_optran.f

airs:
	rm -f keff.f keff_optran.f fitk9_optran.f
	ln -s keff_airs.f keff.f
	ln -s keff_optran_airs.f keff_optran.f
	ln -s fitk9_optran_airs.f fitk9_optran.f

######
# fowp: used with set1, set2, co2, and optran
fowp:
	rm -f *.o farray.f
	ln -s farray_set1_fowp.f farray.f
	make -s -f make_all set1
	mv a.out ../Bin/fit_set1_fowp
	rm -f *.o farray.f
	ln -s farray_set2_fowp.f farray.f
	make -s -f make_all set2
	mv a.out ../Bin/fit_set2_fowp
	rm -f *.o farray.f
	ln -s farray_co2_fowp.f farray.f
	make -s -f make_all co2
	mv a.out ../Bin/fit_co2_fowp
	rm -f *.o farray.f
	ln -s farray_optran_fowp.f farray.f
	make -s -f make_all optran
	mv a.out ../Bin/fit_optran_fowp

######
# fmw: used with set3 and optran
fmw:
	rm -f *.o farray.f
	ln -s farray_set3_fmw.f farray.f
	make -s -f make_all set3
	mv a.out ../Bin/fit_set3_fmw
	rm -f *.o farray.f
	ln -s farray_optran_fmw.f farray.f
	make -s -f make_all optran
	mv a.out ../Bin/fit_optran_fmw

######
# fcow_sun: used with set4
fcow_sun:
	rm -f *.o farray.f
	ln -s farray_set4_fcow.f farray.f
	make -s -f make_all set4
	mv a.out ../Bin/fit_set4_fcow_sun

######
# fowp_sun: used with set5, set6, set7, and co2
fowp_sun:
	rm -f *.o farray.f
	ln -s farray_set5_fowp.f farray.f
	make -s -f make_all set5
	mv a.out ../Bin/fit_set5_fowp_sun
	rm -f *.o farray.f
	ln -s farray_set6_fowp.f farray.f
	make -s -f make_all set6
	mv a.out ../Bin/fit_set6_fowp_sun
	rm -f *.o farray.f
	ln -s farray_set7_fowp.f farray.f
	make -s -f make_all set7
	mv a.out ../Bin/fit_set7_fowp_sun
	rm -f *.o farray.f
	ln -s farray_co2_fowp_sun.f farray.f
	make -s -f make_all co2
	mv a.out ../Bin/fit_co2_fowp_sun
	rm -f *.o farray.f
	ln -s farray_co2_5term_fowp_sun.f farray.f
	make -s -f make_all co2_5term
	mv a.out ../Bin/fit_co2_5term_fowp_sun

# ---------------
# File management
# ---------------

clean:
	rm -f keff.f keff_optran.f fitk9_optran.f
	rm -f *.o farray.f
