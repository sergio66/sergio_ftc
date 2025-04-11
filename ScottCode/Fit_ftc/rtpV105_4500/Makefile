#
# RTP top level Makefile
#
# this file sets some common parameters and then calls makefiles
# in the src, test, and utils subdirectories
#
# parameters should normally be set here in preference to setting
# them in the subdirectories.  In most cases, all you need to set
# are HDFHOME, RTPHOME, and your local compiler options
#

# --------------
# HDF parameters
# --------------

# set HDFHOME to point to the local HDF installation
HDFHOME = /usr/local/hdf

# --------------
# RTP parameters
# --------------

# set RTPHOME to point to the local RTP installation
# RTPHOME = /usr/local/rtp
RTPHOME = ..

# ------------------
# C compiler options
# -------------------

# -64 for 64-bit IRIX
# CFLAGS = -g -64
# CFLAGS = -g
CFLAGS = -O
CC = cc

# ------------------------
# Fortran compiler options
# ------------------------

# Absoft Fortran
# --------------
# -N109  fold all names to upper case
# -C     check array bounds
# -O     some optimizations
# -N3    add record info to unformatted files
# -s     static allocation
# FFLAGS = -N109 -C -O -N3 -s
# FFLAGS = -g
FFLAGS = -O
FLIB = -lU77	# Linux Absoft needs -lU77
F77 = /usr/local/absoft/bin/f77

# SunOS options
# -------------
# FFLAGS = -e -fast -w     
# F77 = f77

# SGI options
# -----------
# include -cpp option if not default 
# FFLAGS = -O
# -64 for 64-bit IRIX
# FFLAGS  = -O -64
# F77 = f77

# pass the variables set above to the subdirectory makefiles
#
EXPORTS = HDFHOME="$(HDFHOME)" RTPHOME="$(RTPHOME)" \
	CC="$(CC)" CFLAGS="$(CFLAGS)" CLIB="$(CLIB)" \
	F77="$(F77)" FFLAGS="$(FFLAGS)" FLIB="$(FLIB)"

# -------------
# Make Targets
# -------------

all: SRC TEST UTILS

SRC:
	cd src && make $(EXPORTS)
TEST:
	cd test && make $(EXPORTS)
UTILS:
	cd utils && make $(EXPORTS)

clean:
	cd src && make clean
	cd test && make clean
	cd utils && make clean

# -------------------------
# make an RTP distribution
# -------------------------
#
# "make dist" makes a distribution named by the current working
# directory.  For example, if we are in the subdirectory rtpV102
# "make dist" will clean things up and then create an rtpV102.tar 
# in the parent directory that unpacks to rtpV102/<etc>.
# 
dist: clean
	rm rtp.tar bin/* lib/* include/* 2> /dev/null || true
	rbase=`/bin/pwd`                && \
	    rbase=`basename $${rbase}`  && \
	    cd ..                       && \
	    tar -cf $${rbase}.tar          \
		$${rbase}/bin              \
		$${rbase}/doc              \
		$${rbase}/include          \
		$${rbase}/lib              \
		$${rbase}/Makefile         \
		$${rbase}/README           \
		$${rbase}/src              \
		$${rbase}/test             \
		$${rbase}/utils
	@echo created `/bin/pwd`.tar
	@echo "\"make all\" to rebuild the local distribution"

