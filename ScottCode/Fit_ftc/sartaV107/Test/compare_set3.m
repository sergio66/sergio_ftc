% Program compare_set3.m
%
% Read in regular and otpran set3 and compare to useconv BT
%

% Created: 03 January 2008, Scott Hannon
% Update: 08 Jan 2008, S.Hannon - add noe about CH4 near 1700 cm^-1
% Update: 16 Apr 2008, S.Hannon - minor update for Apr08 AIRS production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools

prefix = input('Enter filename prefix (m130x_, m150_, etc) : ','s');


% Read in set3 regular
hfile = [prefix 'set3.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
bt3 = radtot(f,prof.rcalc);
nchan = length(f);

% Read in set3 optran
hfile = [prefix 'set3o.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
bt3o = radtot(f,prof.rcalc);

% Read in set1o
hfile = [prefix 'set1o.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id1o = round( head.ichan ); % exact integer
f1o = head.vchan;
bt1o = radtot(f1o,prof.rcalc);
nchan1o = length(f1o);
[i1,i2,iok] = intersect(id, id1o);
bt1o = bt1o(iok,:);
clear id1o f1o


% Load in useconv data
x=load(['../../Output_useconv/rad_' prefix 'fow_long_3']);
btu1o = reshape(x(:,3:8),nchan1o,288);
btu1o = btu1o(iok,:);
%
x=load(['../../Output_useconv/rad_' prefix 'fmw_3']);
btu = reshape(x(:,3:8),nchan,288);
%
clear x

% Subset angles to exclude angles over 60 degrees (secant=2)
iok = find(prof.scanang < 2);
bt1o = bt1o(:,iok);
bt3  = bt3(:,iok);
bt3o = bt3o(:,iok);
btu1o= btu1o(:,iok);
btu  = btu(:,iok);

clear iok

% Difference calculated BT from useconv BT
dbt1o = btu1o - bt1o;
dbt3  = btu - bt3;
dbt3o = btu - bt3o;
dbt13 = btu - btu1o;

% RMS error
rms1o = sqrt( mean(dbt1o.^2,2) );
rms3  = sqrt( mean(dbt3.^2 ,2) );
rms3o = sqrt( mean(dbt3o.^2,2) );
rms13 = sqrt( mean(dbt13.^2,2) );

% Mean BT spectra
btmean = mean(btu,2);


% Find best coefs for each channel
ind_all = 1:nchan;
rms_best = rms1o;
% Note: There is CH4 in the 1700 cm^-1 region but ozone is stronger and
% dominates the rms13
ind_is3 = find( rms13 > 0.02 & f > 1200.51 & f < 1650); % chan with signif CH4 signal
ind_not3 = setdiff(ind_all,ind_is3);
ind_best3 = ind_is3;
rms_best(ind_best3) = rms3(ind_best3);
%ind_best3o = ind_is3( find(rms3o(ind_is3)+0.02 < rms3(ind_is3)) );
ind_best3o = ind_is3( find(rms3o(ind_is3)+0.008 < rms3(ind_is3)) );
rms_best(ind_best3o) = rms3o(ind_best3o); 
ind_best3 = setdiff(ind_best3, ind_best3o);
%
id_best3 =id(ind_best3);
id_best3o=id(ind_best3o);
id_not3  =id(ind_not3);

% Plot results
clf
subplot(211),plot(f,btmean,'b.-'),grid
subplot(212),plot(f,rms3,'b',f,rms3o,'c',f,rms1o,'r',f,rms13,'k', ...
f(ind_best3),rms3(ind_best3),'bo',f(ind_best3o),rms3o(ind_best3o),'co',...
		  f(ind_not3),rms1o(ind_not3),'ro'),grid
title('blue=set3, cyan=set3o, red=set1o, black = rms(fmw-fow)')


%%% end of program %%%
