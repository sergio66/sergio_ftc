% Read the various pert RTP files and plot results

addpath /asl/matlab/h4tools

% Read unperturbed
[head, hattr, prof, pattr] = rtpread('reg1013rad_m130.rtp');
id = head.ichan;
f130 = head.vchan;
bt130 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_m140.rtp');
f140 = head.vchan;
bt140 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_m150.rtp');
f150 = head.vchan;
bt150 = mean( radtot(f150, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_m130x.rtp');
bt130x = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_m140x.rtp');
bt140x = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_dec05_wcononly.rtp');
fold = head.vchan;
btold = mean( radtot(f140, prof.rcalc), 2);


% Read CO2
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_m130.rtp');
id = head.ichan;
f130 = head.vchan;
bt130_co2 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_m140.rtp');
f140 = head.vchan;
bt140_co2 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_m150.rtp');
f150 = head.vchan;
bt150_co2 = mean( radtot(f150, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_m130x.rtp');
bt130x_co2 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_m140x.rtp');
bt140x_co2 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_co2_dec05_wcononly.rtp');
btold_co2 = mean( radtot(f140, prof.rcalc), 2);


% Read SO2
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_m130.rtp');
id = head.ichan;
f130 = head.vchan;
bt130_so2 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_m140.rtp');
f140 = head.vchan;
bt140_so2 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_m150.rtp');
f150 = head.vchan;
bt150_so2 = mean( radtot(f150, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_m130x.rtp');
bt130x_so2 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_m140x.rtp');
bt140x_so2 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_so2_dec05_wcononly.rtp');
btold_so2 = mean( radtot(f140, prof.rcalc), 2);



% Read HNO3
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_m130.rtp');
id = head.ichan;
f130 = head.vchan;
bt130_hno3 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_m140.rtp');
f140 = head.vchan;
bt140_hno3 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_m150.rtp');
f150 = head.vchan;
bt150_hno3 = mean( radtot(f150, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_m130x.rtp');
bt130x_hno3 = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_m140x.rtp');
bt140x_hno3 = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_hno3_dec05_wcononly.rtp');
btold_hno3 = mean( radtot(f140, prof.rcalc), 2);


% Read N2O
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_m130.rtp');
id = head.ichan;
f130 = head.vchan;
bt130_n2o = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_m140.rtp');
f140 = head.vchan;
bt140_n2o = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_m150.rtp');
f150 = head.vchan;
bt150_n2o = mean( radtot(f150, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_m130x.rtp');
bt130x_n2o = mean( radtot(f130, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_m140x.rtp');
bt140x_n2o = mean( radtot(f140, prof.rcalc), 2);
[head, hattr, prof, pattr] = rtpread('reg1013rad_n2o_dec05_wcononly.rtp');
btold_n2o = mean( radtot(f140, prof.rcalc), 2);


% Plot CO2 results
clf
disp('k=old, b=130, g=140, r=150, c=130x, m=140x')
subplot(211)
plot(f140,bt140,'g'),grid,title('CO2')
subplot(212)
plot(fold,btold-btold_co2,'k',   f130,bt130-bt130_co2,'b', ...
     f140,bt140-bt140_co2,'g',   f150,bt150-bt150_co2,'r', ...
     f130,bt130x-bt130x_co2,'c', f140,bt140x-bt140x_co2,'m' ),grid
blowup
pause


% Plot SO2 results
clf
disp('k=old, b=130, g=140, r=150, c=130x, m=140x')
subplot(211)
plot(f140,bt140,'g'),grid,title('SO2')
subplot(212)
plot(fold,btold-btold_so2,'k',   f130,bt130-bt130_so2,'b', ...
     f140,bt140-bt140_so2,'g',   f150,bt150-bt150_so2,'r', ...
     f130,bt130x-bt130x_so2,'c', f140,bt140x-bt140x_so2,'m' ),grid
blowup
pause


% Plot HNO3 results
clf
disp('k=old, b=130, g=140, r=150, c=130x, m=140x')
subplot(211)
plot(f140,bt140,'g'),grid,title('HNO3')
subplot(212)
plot(fold,btold-btold_hno3,'k',   f130,bt130-bt130_hno3,'b', ...
     f140,bt140-bt140_hno3,'g',   f150,bt150-bt150_hno3,'r', ...
     f130,bt130x-bt130x_hno3,'c', f140,bt140x-bt140x_hno3,'m' ),grid
blowup
pause


% Plot N2O results
clf
disp('k=old, b=130, g=140, r=150, c=130x, m=140x')
subplot(211)
plot(f140,bt140,'g'),grid,title('N2O')
subplot(212)
plot(fold,btold-btold_n2o,'k',   f130,bt130-bt130_n2o,'b', ...
     f140,bt140-bt140_n2o,'g',   f150,bt150-bt150_n2o,'r', ...
     f130,bt130x-bt130x_n2o,'c', f140,bt140x-bt140x_n2o,'m' ),grid
blowup
pause

%%% end of program %%%
