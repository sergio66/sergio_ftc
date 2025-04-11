#
# RTP utilities Makefile
#
# this file builds some sample C and Fortran utilities
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

# ------------------------
# Fortran compiler options
# ------------------------

FFLAGS = -O
FLIB = -lU77
F77 = /usr/local/absoft/bin/f77

# -------------
# Build Targets
# -------------

all: rtpsubset rtpdump install

# ----------------
# rtp dump utility
# ----------------

rtpdump: rtpdump.o
	$(CC) $(CFLAGS) -o rtpdump rtpdump.o $(RTPLIB) $(HDFLIB)
rtpdump.o: rtpdump.c
	$(CC) $(CFLAGS) -c rtpdump.c $(RTPINC) $(HDFINC)

# ------------------
# rtp subset utility
# ------------------

SUBSRC = subset_rtp.f \
	rdinfo_subset.f rdlist_subset.f util_subset.f sublst.f
SUBOBS = subset_rtp.o \
	rdinfo_subset.o rdlist_subset.o util_subset.o sublst.o

rtpsubset: $(SUBOBS)
	$(F77) $(FFLAGS) -o rtpsubset $(SUBOBS) $(RTPLIB) $(HDFLIB) $(FLIB)

$(SUBOBS): $(SUBSRC) incSUB.f
	$(F77) $(FFLAGS) -c $(SUBSRC) $(RTPINC)

# ------------------
# rtp info utility
# ------------------

INFOSRC = info_rtp.f  util_subset.f
INFOOBS = info_rtp.o  util_subset.o

rtpinfo: $(INFOOBS)
	$(F77) $(FFLAGS) -o rtpinfo $(INFOOBS) $(RTPLIB) $(HDFLIB) $(FLIB)

$(INFOOBS): $(INFOSRC)
	$(F77) $(FFLAGS) -c $(INFOSRC) $(RTPINC)

# -------
# install
# -------

install: rtpsubset rtpdump rtpinfo
	cp -a rtpsubset rtpdump rtpinfo $(RTPHOME)/bin

# ---------------
# cleanup
# ---------------

clean:
	rm *.o rtpsubset rtpdump rtpinfo 2> /dev/null || true

