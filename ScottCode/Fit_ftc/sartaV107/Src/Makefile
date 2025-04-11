#
# Makefile for SARTA v1.07.  Most of the work is done by make_sarta.
#

# ------
# Builds
# ------

m130x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130x/" incFTC_airs_template.f > incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_apr08_m130x

m140x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140x/" incFTC_airs_template.f > incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_apr08_m140x

m130:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130/" incFTC_airs_template.f > incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_apr08_m130

m140:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140/" incFTC_airs_template.f > incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_apr08_m140

m150:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m150/" incFTC_airs_template.f > incFTC.f
	make -f make_sarta_4500
	mv a.out ../Bin/sarta_apr08_m150

xx_rdown_m130x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130x/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500_rdown
	mv a.out ../Bin/sarta_xx_rdown_m130x

xx_rdown_m140x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140x/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500_rdown
	mv a.out ../Bin/sarta_xx_rdown_m140x

xx_rdown_m130:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500_rdown
	mv a.out ../Bin/sarta_xx_rdown_m130

xx_rdown_m140:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500_rdown
	mv a.out ../Bin/sarta_xx_rdown_m140

xx_rdown_m150:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m150/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500_rdown
	mv a.out ../Bin/sarta_xx_rdown_m150

xx_m130x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130x/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx_m130x

xx_m140x:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140x/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx_m140x

xx_m130:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m130/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx_m130

xx_m140:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m140/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx_m140

xx_m150:
	rm -f *.o incFTC.f
	sed "s/XCSNAMEX/m150/" incFTC_airs_x_template.f > incFTC.f
	make -f xxmake_sarta_4500
	mv a.out ../Bin/sarta_xx_m150


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
