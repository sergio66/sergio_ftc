
load yuval_aeridata_20120717_12.mat
lala = abs(t1-160000); it = find(lala == min(lala));

sergio = load('process_1013mb_20120717_12.txt');
plot(sergio(:,1),sergio(:,2),x1,rnc1(:,it),'r')

addpath /home/sergio/MATLABCODE/FCONV
addpath /home/sergio/MATLABCODE/FFTCONV
addpath /home/sergio/KCARTA/MATLAB

[rkc,wkc] = readkcstd('process.dat');
ix = 1:480000;
%[rkc2,wkc2,rad2,wnum2,rint] = sergfconv(rkc, wkc, rdata, fdata,ifp, atype, aparg)
[rkc2,wkc2,rad2,wnum2,rint] = sergfconv_obsNcalc(rkc, wkc, rnc1(:,it), x1, 'aeriB1', 'nb', 6);
[rch, wch] = xfconvkc_serg_iasi(rkc,wkc, 'aeriB1', 'nb', 6);

lala = find(wkc < 1250 | wkc > 1800); 
rkc2 = rkc; rkc2(lala) = nan;
rint = interp1(wch,rch,wnum2);

toffset = 2.4;
h1 = subplot(211); plot(wnum2,rad2bt(wnum2,rint')-toffset,wnum2,rad2bt(wnum2,rad2'),'r',...
                        wkc,rad2bt(wkc,rkc2)-30,'k')
title('b = kc  r = data')
axis([605 1800 200 320]); grid
h2 = subplot(212); plot(wnum2,rad2bt(wnum2,rad2')-(rad2bt(wnum2,rint')-toffset),'r')
axis([605 1800 -10 +10]); grid
addpath /asl/matlab/aslutil/
adjust21(h1,h2,'even')

