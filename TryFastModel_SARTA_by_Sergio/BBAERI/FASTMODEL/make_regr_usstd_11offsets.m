xstartup

fin = 'regr48.op.rtp'; %% made by make_regr48.m
[h,ha,p,pa] = oldrtpread(fin);

addpath /home/sergio//MATLABCODE/CONVERT_GAS_UNITS/
[ppmvLAY,ppmvAVG,ppmvMAX] = layers2ppmv(h,p,1:length(p.stemp),2);
plot(ppmvMAX)       %% shows CO2 is 370 ppmv

[hx,px] = subset_rtp(h,p,[],[],49);
h11 = hx;
p11 = px;
for ii = 1 : 10
  [h11,p11] = cat_rtp(hx,px,h11,p11);
end

for ii = 1 : 11
  p11.ptemp(:,ii) = p11.ptemp(:,ii) + (ii-6)*10;
  p11.stemp(ii)   = p11.stemp(ii) + (ii-6)*10;
end

rtpwrite('regr_usstd_11offsets.op.rtp',h11,ha,p11,pa);