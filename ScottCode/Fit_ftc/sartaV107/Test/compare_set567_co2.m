% Program compare_set567_co2.m
%
% Read in final set567 with variable CO2 and compare to fow_short useconv BT
%

% Created: 30 April 2008, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools


% Read in 365.750
hfile = 'rad567_m140_365750.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
id = round( head.ichan ); % exact integer
f = head.vchan;
btcm5 = radtot(f,prof.rcalc);
nchan = length(f);

% Read in 375.387
hfile = 'rad567_m140_375375.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
btcm2h = radtot(f,prof.rcalc);

% Read in 385.000
hfile = 'rad567_m140_385000.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
btc0 = radtot(f,prof.rcalc);

% Read in 394.625
hfile = 'rad567_m140_394625.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
btcp2h = radtot(f,prof.rcalc);

% Read in 404.250
hfile = 'rad567_m140_404250.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
btcp5 = radtot(f,prof.rcalc);

% Read in 413.875
hfile = 'rad567_m140_413875.rtp';
[head,hattr, prof, pattr] = rtpread(hfile);
btcp7h = radtot(f,prof.rcalc);


% Load in useconv BT 365.750
x=load(['../../Output_useconv/rad_co2test_m140_fow_short_1']);
btum5 = reshape(x(:,3:14),nchan,576);
iok = 1:288; % ignore large angles
btum5 = btum5(:,iok);

% Load in useconv BT 375.375
x=load(['../../Output_useconv/rad_co2test_m140_fow_short_2']);
btum2h = reshape(x(:,3:14),nchan,576);
btum2h = btum2h(:,iok);

% Load in useconv BT 385.000
x=load(['../../Output_useconv/rad_m140_fow_short_3']);
btu0 = reshape(x(:,3:14),nchan,576);
btu0 = btu0(:,iok);

% Load in useconv BT 394.625
x=load(['../../Output_useconv/rad_co2test_m140_fow_short_3']);
btup2h = reshape(x(:,3:14),nchan,576);
btup2h = btup2h(:,iok);

% Load in useconv BT 404.250
x=load(['../../Output_useconv/rad_m140_fow_short_4']);
btup5 = reshape(x(:,3:14),nchan,576);
btup5 = btup5(:,iok);

% Load in useconv BT 413.875
x=load(['../../Output_useconv/rad_co2test_m140_fow_short_4']);
btup7h = reshape(x(:,3:14),nchan,576);
btup7h = btup7h(:,iok);
clear iok x


% Calculate RMS difference
dbt  = btum5  - btcm5;   rmsm5 = sqrt( mean(dbt.^2 ,2) );
dbt  = btum2h - btcm2h; rmsm2h = sqrt( mean(dbt.^2 ,2) );
dbt  = btu0   - btc0;     rms0 = sqrt( mean(dbt.^2 ,2) );
dbt  = btup2h - btcp2h; rmsp2h = sqrt( mean(dbt.^2 ,2) );
dbt  = btup5  - btcp5;   rmsp5 = sqrt( mean(dbt.^2 ,2) );
dbt  = btup7h - btcp7h; rmsp7h = sqrt( mean(dbt.^2 ,2) );

% Mean BT spectra
btmean = mean(btu0,2);

% Plot results
clf
%%% plot by freq
subplot(211),plot(f,btmean,'b.-'),grid
subplot(212),plot(f,rmsm5,'b',f,rmsm2h,'g',f,rms0,'r',f,rmsp2h,'c',...
                  f,rmsp5,'m',f,rmsp7h,'y'),grid
title('b=-5%, g=-2.5%, r=0%, c=+2.5%, m=+5%, y=+7.5%')

%%% end of program %%%
