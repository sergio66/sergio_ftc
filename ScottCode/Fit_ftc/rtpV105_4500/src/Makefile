#
# RTP library makefile
#
# this makefile builds the library librtp.a for the RTP C and 
# Fortran application interfaces
#
# the site-specific parameters HDFHOME, RTPHOME, and the C and 
# Fortran compiler options can be set in the top level makefile

# --------------
# HDF parameters
# --------------

# set HDFHOME to point to the local HDF installation
HDFHOME = /usr/local/hdf

# HDF definitions, relative to HDFHOME
HDFINC   = -I$(HDFHOME)/include
HDFLIB   = -L$(HDFHOME)/lib -ldf -ljpeg -lz

# --------------
# RTP parameters
# --------------

# set RTPHOME to point to the local RTP installation
# RTPHOME = /usr/local/rtp
RTPHOME = ..

# RTP definitions, relative to RTPHOME
RTPINC  = -I$(RTPHOME)/include		# RTP C and F77 includes
RTPLIB  = -L$(RTPHOME)/lib -lrtp	# RTP C and F77 library

# ------------------
# C compiler options
# -------------------

CFLAGS = -O
CC = cc

# ------------
# make targets
# -------------

all: librtp.a install

# --------------------------
# vdata interface procedures
# --------------------------

PVSRC = pvopen.c pvclose.c \
	pvread1.c pvwrite1.c pvread2.c pvwrite2.c pvread3.c pvwrite3.c 

PVOBS = pvopen.o pvclose.o \
	pvread1.o pvwrite1.o pvread2.o pvwrite2.o pvread3.o pvwrite3.o 

$(PVOBS): $(PVSRC) rtp.h pvdefs.h
	$(CC) $(CFLAGS) -c $(PVSRC) $(HDFINC)

# ----------------
# RTP C procedures
# ----------------

RTPSRC = rtpwrite1.c rtpwrite2.c rtpread1.c rtpread2.c getchan.c \
		rtpclose1.c gsplit.c rtphcheck.c headinit.c profinit.c \
		fmatch.c hsize.c
RTPOBS = rtpwrite1.o rtpwrite2.o rtpread1.o rtpread2.o getchan.o \
		rtpclose1.o gsplit.o rtphcheck.o headinit.o profinit.o \
		fmatch.o hsize.o

$(RTPOBS): $(RTPSRC) rtp.h pvdefs.h
	$(CC) $(CFLAGS) -c $(RTPSRC) $(HDFINC)

# -----------------------
# test support procedures
# -----------------------

TSTSRC = dump_flist.c dump_vbuf.c dump_pstr.c dump_hstr.c \
		dump_attrs.c dump_chan.c

TSTOBS = dump_flist.o dump_vbuf.o dump_pstr.o dump_hstr.o \
		dump_attrs.o dump_chan.o

$(TSTOBS): $(TSTSRC) rtp.h pvdefs.h
	$(CC) $(CFLAGS) -c $(TSTSRC) $(HDFINC)

# -------------
# C/Fortran API
# -------------

FORSRC = rtpopen.c rtpread.c rtpwrite.c rtpclose.c rtpvers.c rtpinit.c
FOROBS = rtpopen.o rtpread.o rtpwrite.o rtpclose.o rtpvers.o rtpinit.o

$(FOROBS): $(FORSRC) rtp.h pvdefs.h
	$(CC) $(CFLAGS) -c $(FORSRC) $(HDFINC)

# -----------
# RTP library
# -----------

librtp.a: $(PVOBS) $(RTPOBS) $(FOROBS) $(TSTOBS)
	ar -rcs librtp.a $(PVOBS) $(RTPOBS) $(FOROBS) $(TSTOBS)

# -------
# install
# -------

install:librtp.a
	cp -a rtp.h pvdefs.h rtpdefs.f $(RTPHOME)/include
	cp -a librtp.a $(RTPHOME)/lib

# -------
# cleanup
# -------

clean:
	rm *.o librtp.a 2> /dev/null || true

