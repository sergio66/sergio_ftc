% Program compare_set567.m
%
% Read in set5/6/7 and compare to useconv BT
%

% Created: 03 January 2008, Scott Hannon
% Update: 16 Apr 2008, S.Hannon - minor changes for AIRS Apr08 production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools

prefix = input('Enter filename prefix (m130x_, m150_, etc) : ','s');

% Read in set5
hfile = [prefix 'set5.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
bt5 = radtot(f,prof.rcalc);
nchan = length(f);


% Read in set6
hfile = [prefix 'set6.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
bt6 = radtot(f,prof.rcalc);


% Read in set7
hfile = [prefix 'set7.rtp'];
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
bt7 = radtot(f,prof.rcalc);


% Load in useconv BT
x=load(['../../Output_useconv/rad_' prefix 'fow_short_3']);
btu = reshape(x(:,3:14),nchan,576);
iok = 1:288; % ignore large angles
btu = btu(:,iok);


% Difference calculated BT from useconv BT
dbt5  = btu - bt5;
dbt6  = btu - bt6;
dbt7  = btu - bt7;


% RMS error
rms5  = sqrt( mean(dbt5.^2 ,2) );
rms6  = sqrt( mean(dbt6.^2 ,2) );
rms7  = sqrt( mean(dbt7.^2 ,2) );


% Mean BT spectra
btmean = mean(btu,2);


% Determine best set for each channel
ind_all = 1:nchan;
rms_best = rms6;
ind_best6 = ind_all;
ind_best5 = find( rms5+0.01 < rms_best);
ind_best6 = setdiff(ind_best6, ind_best5);
rms_best(ind_best5) = rms5(ind_best5);
%ind_best7 = find( rms7+0.02 < rms_best);
ind_best7 = find( rms7+0.015 < rms_best);
ind_best5 = setdiff(ind_best5, ind_best7);
ind_best6 = setdiff(ind_best6, ind_best7);
rms_best(ind_best7) = rms(ind_best7);
%
id_best5 = id(ind_best5);
id_best6 = id(ind_best6);
id_best7 = id(ind_best7);


% Plot results
clf
%%% plot by freq
subplot(211),plot(f,btmean,'b.-'),grid
subplot(212),plot(f,rms5,'b', f,rms6,'r', f,rms7,'c', ...
   f(ind_best5),rms5(ind_best5),'bo', f(ind_best6),rms6(ind_best6),'ro', ...
   f(ind_best7),rms7(ind_best7),'co'),grid
%%% plot by channal
%subplot(211),plot(id,btmean,'b.-'),grid
%subplot(212),plot(id,rms5,'b', id,rms6,'r', id,rms7,'c', ...
%   id(ind_best5),rms5(ind_best5),'bo', id(ind_best6),rms6(ind_best6),'ro', ...
%   id(ind_best7),rms7(ind_best7),'co'),grid
%%%
title('blue=set5, red=set6, cyan=set7')

%%% end of program %%%
