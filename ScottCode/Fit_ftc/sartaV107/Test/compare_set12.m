% Program compare_set12.m
%
% Read in regular and otpran set1 & set2 data and compare to useconv BT
%

% Created: 02 January 2008, Scott Hannon
% Update: 07 Jan 2007, S.Hannon - minor mod to select more optran
% Update: 16 Apr 2008, S.Hannon - minor changes for AIRS Apr08 production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools

prefix=input('Enter filename prefix (m130x_, m150_, etc) : ','s');


% Read in set1 regular
hfile = [prefix 'set1.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = head.ichan;
nchan = length(id);
f = head.vchan;
bt1 = radtot(f,prof.rcalc);


% Read in set1 optran
hfile = [prefix 'set1o.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
bt1o = radtot(f,prof.rcalc);


% Read in set2
hfile = [prefix 'set2.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
f2 = head.vchan;
if (length(f2) == nchan)
   if (max(abs(f2 - f)/1200) > 0.005*f/1200)
      error('set1 and set2 freqs differ')
   end
   clear f2
else
   error('set1 and set2 nchan differ')
end
bt2 = radtot(f,prof.rcalc);


% Load in useconv data
x=load(['../../Output_useconv/rad_' prefix 'fow_long_3']);
btu = reshape(x(:,3:8),nchan,288);
clear x

% Subset angles to exclude angles over 60 degrees (secant=2)
iok = find(prof.scanang < 2);
bt1=bt1(:,iok);
bt1o=bt1o(:,iok);
bt2=bt2(:,iok);
btu=btu(:,iok);

clear iok

% Difference calculated BT from useconv BT
dbt1 = btu - bt1;
dbt1o = btu - bt1o;
dbt2 = btu - bt2;

% RMS error
rms1  = sqrt( mean(dbt1.^2 ,2) );
rms1o = sqrt( mean(dbt1o.^2,2) );
rms2  = sqrt( mean(dbt2.^2 ,2) );

% Mean BT spectra
btmean = mean(btu,2);


% Find best coefs for each channel
ind_all = 1:nchan;
ind_best1 = ind_all;
rms_best = rms1;
%ind_best1o = find( rms1o+0.02 < rms1);
ind_best1o = find( rms1o+0.008 < rms1 & f > 700);
rms_best(ind_best1o) = rms1o(ind_best1o);
ind_best1 = setdiff(ind_best1, ind_best1o);
ind_best2 = find(rms2+0.01 < rms_best);
rms_best(ind_best2) = rms2(ind_best2); 
ind_best1 = setdiff(ind_best1, ind_best2);
ind_best1o = setdiff(ind_best1o, ind_best2);
%
id_best1 =id(ind_best1);
id_best1o=id(ind_best1o);
id_best2 =id(ind_best2);

clf
subplot(211),plot(f,btmean,'b.-'),grid
subplot(212),plot(f,rms1,'b.-', f,rms1o,'c.-', f,rms2,'r.-', ...
   f(ind_best1),rms1(ind_best1),'bo', f(ind_best1o),rms1o(ind_best1o),'co',...
   f(ind_best2),rms2(ind_best2),'ro'),grid
title('blue=set1, cyan=set1o, red=set2, circle=best') 

%%% end of program %%%
