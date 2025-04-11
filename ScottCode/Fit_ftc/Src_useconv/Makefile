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
# OPT= -e -fast -w
# F77 = f77

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
#OPT = -N109 -s -N3 -N26 -C -O
OPT = -s -N3 -N26
F77 = /opt/absoft10.0/bin/f77

# -----------------------------------------------------------------------------
# SGI:
#    -O       turn on default optimization (same as -O2)
#    -O0      turn off all optimization
#    -static
#    -check_bounds
#OPT= -static -O0 -check_bounds
#F77 = f77

###############################################################################

SRC= urad.f rdref.f useconv.f

airs:
	rm -f useconv.f
	ln -s useconv_airs.f useconv.f
	$(F77) $(OPT) $(SRC) -o ../Bin/useconv_airs

iasi:
	rm -f useconv.f
	ln -s useconv_iasi.f useconv.f
	$(F77) $(OPT) $(SRC) -o ../Bin/useconv_iasi

cris:
	rm -f useconv.f
	ln -s useconv_cris.f useconv.f
	$(F77) $(OPT) $(SRC) -o ../Bin/useconv_cris

