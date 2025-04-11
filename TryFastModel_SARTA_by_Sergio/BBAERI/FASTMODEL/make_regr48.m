xstartup
fin = '/asl/s1/sergio/pin_feb2002_sea_airsnadir_g80_ip.so2.rtp';
klayers = '/asl/packages/klayers/Bin/klayers_aeri1013';

[h0,ha,p0,pa] = rtpread(fin);

klayerser = ['!' klayers ' fin=' fin ' fout=regr48.op.rtp nwant=-1'];
eval(klayerser);

[h1,ha,p1,pa] = rtpread('regr48.op.rtp');

addpath /home/sergio//MATLABCODE/CONVERT_GAS_UNITS/
[ppmvLAY,ppmvAVG,ppmvMAX] = layers2ppmv(h1,p1,1:length(p1.stemp),2);
plot(ppmvMAX)       %% shows CO2 is 370 ppmv

