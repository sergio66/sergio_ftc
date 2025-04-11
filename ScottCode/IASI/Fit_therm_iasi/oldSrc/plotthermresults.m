% Plot therm results
%
% Calcs RMS and STD ref therm signal, and also fit RMS error.
%

% Created by Scott Hannon, 5 July 2000
% Last update: 16 April 2002, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
clf

bands={'1', '2', '3'};

rmserror=[];
rmstherm=[];
stdtherm=[];
frequency=[];
btmean=[];

nband=length(bands);
for ib=1:nband
   band=char( bands(ib) );
   disp(['working on band ' band]);

   eval(['load therm_fcoef_' band]);
   frequency=[frequency, freq'];
   rmserror=[rmserror, rmsall'];

   eval(['load ../Data/thermdata_b' band]);

   btt=radtot(freq,1000*radwiththerm);
   btnot=radtot(freq,1000*radnotherm);
   btdif=btt - btnot;

   btmean=[btmean, mean(btt')];
   stdtherm=[stdtherm, std(btdif')];
   rmstherm=[rmstherm, sqrt(mean( btdif'.^2 ))];

end

ind=find(rmserror < 10);

subplot(2,1,1)
plot(frequency,btmean,'b.'),grid
title('mean BT')
subplot(2,1,2)
plot(frequency(ind),rmstherm(ind),'b.',frequency(ind),stdtherm(ind),'c.',...
   frequency(ind),rmserror(ind),'r.'   ),grid
%   frequency(ind),rmserror(ind).*rmstherm(ind),'k.' ),grid
title('blue=RMS therm, cyan=STD therm, red=RMS F-error')
