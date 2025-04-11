% Program compare_set4.m
%
% Read in set4 and compare to useconv BT
%

% Created: 03 January 2008, Scott Hannon
% Update: 16 Apr 2008, S.Hannon - changes for Apr08 AIRS production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools

prefix = input('Enter filename prefix (m130x_, m150_, etc) : ','s');

% Read in set4
hfile = [prefix 'set4.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
bt4 = radtot(f,prof.rcalc);
nchan = length(f);


% Read in set5
hfile = [prefix 'set5.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id5 = round( head.ichan ); % exact integer
f5 = head.vchan;
bt5 = radtot(f5,prof.rcalc);
id5 = head.ichan;
nchan5 = length(id5);


% Load in useconv data
x=load(['../../Output_useconv/rad_' prefix 'fcow_4']);
btu = reshape(x(:,3:14),nchan,576);
iok = 1:288; % ignore angles 7-12
btu=btu(:,iok);
clear x
%
x=load(['../../Output_useconv/rad_' prefix 'fow_short_3']);
btu5 = reshape(x(:,3:14),nchan5,576);
btu5 = btu5(:,iok);
[junk,i45,i5] = intersect(id,id5);
f5 = f5(i5);
id5 = id5(i5);
btu5 = btu5(i5,:);
bt5 = bt5(i5,:);


% Difference calculated BT from useconv BT
dbt4  = btu - bt4;
dbt5  = btu - bt5;
dbt45 = btu(i45,:) - btu5;

% RMS error
rms4  = sqrt( mean(dbt4.^2  ,2) );
rms5  = sqrt( mean(dbt5.^2  ,2) );
rms45 = sqrt( mean(dbt45.^2 ,2) );

% Mean BT spectra
btmean = mean(btu,2);


ind_co = find(rms45 > 0.015);
id_co = id(ind_co);

% Plot results
clf
subplot(211),plot(id,btmean,'b.-'),grid
subplot(212),plot(id,rms4,'b', id,rms5,'r', id,rms45,'k'),grid
title('blue=set4, red=set5, black=rms(fcow-fow)')

%%% end of program %%%
