#
# Makefile for SARTA v1.07.  Most of the work is done by make_sarta.
#

# ------
# Builds
# ------

m140_x:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/' incFTC_airs_apr08_x.f > incFTC.f
	make -f xmake_sarta_4500
	mv a.out ../Bin/sarta_m140_x

xx:
	rm -f *.o incFTC.f
	ln -s incFTC_airs_xx.f incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx


iasi_jan08:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_jan08.f incFTC.f
	make -f xmake_sarta_4500
	mv a.out ../Bin/sarta_iasi_jan08

iasi_x:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_x.f incFTC.f
	make -f xmake_sarta_4500
	mv a.out ../Bin/sarta_iasi_x

dec05:
	rm -f *.o incFTC.f
	ln -s incFTC_dec05.f incFTC.f
	make -f make_sarta
	mv a.out ../Bin/sarta_dec05

dec05_4500:
	rm -f *.o incFTC.f
	ln -s incFTC_dec05.f incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_dec05_4500

dec05_wcononly:
	rm -f *.o incFTC.f
	ln -s incFTC_dec05_wcononly.f incFTC.f
	make -f make_sarta
	mv a.out ../Bin/sarta_dec05_wcononly

iasi_sep03_con1:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_sep03_con1.f incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_iasi_sep03_con1

iasi_sep03_con1_x:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_sep03_con1_x.f incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_iasi_sep03_con1_x

iasi_sep03_con1_lte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_sep03_con1.f incFTC.f
	make -f make_sarta_lte_4500
	mv a.out ../Bin/sarta_iasi_sep03_con1_lte

# ---------------
# File management
# ---------------

clean:
	rm -f *.o
