###############################################################################
# Compiler info: edit as needed
###############
# OPT : compiler options
# F77 : fortran compiler

# -----------------------------------------------------------------------------
# Generic:
#
OPT = -O
F77 = f77

# -----------------------------------------------------------------------------
# Sun:
#
#OPT= -e -fast -w
#F77 = f77

# -----------------------------------------------------------------------------
# Linux absoft:
#    -f    fold variables to lower case
#    -C    array bounds checking
#    -O    moderate optimization
#    -s    static allocation
#    -g    include symbol table info
#    -N26  read big-endian binary files (needed for coefficient files)
#    -N3   use embedded record lengths in binary files
#    -N109 fold variables to upper case
# Note: Absoft requires the U77 library for command line arguments
OPT = -N109 -s -N3 -N26 -C
F77 = /opt/absoft10.0/bin/af77

# -----------------------------------------------------------------------------
# SGI:
#    -O       turn on default optimization (same as -O2)
#    -O0      turn off all optimization
#    -static
#    -check_bounds
#OPT= -static -O0 -check_bounds
#F77 = f77

###############################################################################


all:
	rm -f farray.f
	ln -s farray_set1.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set1 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set1 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set1 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set2.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set2 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set2 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set2 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set3.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set3 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set3 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set3 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set4.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set4 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set4 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set4 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set5.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set5 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set5 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set5 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set6.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set6 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set6 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set6 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_set7.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set7 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set7 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set7 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_optran.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_optran cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_optran mkdummy.f
	#
	rm -f farray.f
	ln -s farray_co2.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_co2 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_co2 mkdummy.f
	#
	rm -f farray.f
	ln -s farray_co2_5term.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_co2_5term cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_co2_5term mkdummy.f
	#
	rm -f farray.f
	$(F77) $(OPT) -o ../Bin/optheader optheader.f
	$(F77) $(OPT) -o ../Bin/dolist dolist.f
	#
	rm -f farray.f
	$(F77) $(OPT) -o ../Bin/checkc checkc.f


set1:
	rm -f farray.f
	ln -s farray_set1.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set1 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set1 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set1 mkdummy.f


set2:
	rm -f farray.f
	ln -s farray_set2.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set2 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set2 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set2 mkdummy.f


set3:
	rm -f farray.f
	ln -s farray_set3.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set3 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set3 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set3 mkdummy.f

set4:
	rm -f farray.f
	ln -s farray_set4.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set4 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set4 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set4 mkdummy.f


set5:
	rm -f farray.f
	ln -s farray_set5.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set5 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set5 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set5 mkdummy.f


set6:
	rm -f farray.f
	ln -s farray_set6.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set6 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set6 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set6 mkdummy.f


set7:
	rm -f farray.f
	ln -s farray_set7.f farray.f
	$(F77) $(OPT) -o ../Bin/merge_set7 merge.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_set7 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_set7 mkdummy.f


optran:
	rm -f farray.f
	ln -s farray_optran.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_optran cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_optran mkdummy.f
	$(F77) $(OPT) -o ../Bin/optheader optheader.f
	$(F77) $(OPT) -o ../Bin/dolist dolist.f


co2:
	rm -f farray.f
	ln -s farray_co2.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_co2 cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_co2 mkdummy.f
	rm -f farray.f
	ln -s farray_co2_5term.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_co2_5term cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_co2_5term mkdummy.f

n2o:
	rm -f farray.f
	ln -s farray_n2o.f farray.f
	$(F77) $(OPT) -o ../Bin/cutlistcoef_n2o cutlistcoef.f
	$(F77) $(OPT) -o ../Bin/mkdummy_n2o mkdummy.f


### end of makefile ###
