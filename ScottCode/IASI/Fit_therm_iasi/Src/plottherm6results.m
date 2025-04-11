% Plot therm results
%
% Calcs RMS and STD ref therm signal, and also fit RMS error.
%

% Created by Scott Hannon, 5 July 2000
% Update: 16 April 2002, Scott Hannon
% Update: 25 April 2005, S.Hannon - version for new (Apr05) 6-term fit 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
clf

bands={'long', 'short'};

rmserror=[];
rmstherm=[];
stdtherm=[];
frequency=[];
btmean=[];

nband=length(bands);
for ib=1:nband
   band=char( bands(ib) );
   disp(['working on band ' band]);

   eval(['load therm_fcoef6_' band]);
   frequency=[frequency, freq'];
   rmserror=[rmserror, rmsall'];

   eval(['load ../Data/thermdata_' band]);

   btt=radtot(freq,1000*radwiththerm);
   btnot=radtot(freq,1000*radnotherm);
   btdif=btt - btnot;

   btmean=[btmean, mean(btt')];
   stdtherm=[stdtherm, std(btdif')];
   rmstherm=[rmstherm, sqrt(mean( btdif'.^2 ))];

end

ind=find(rmserror > 0);

subplot(2,1,1)
plot(frequency,btmean,'b.'),grid
title('mean BT')
subplot(2,1,2)
plot(frequency,rmstherm,'b.',frequency,stdtherm,'c.',...
   frequency(ind),0.01*rmstherm(ind).*rmserror(ind),'r.'   ),grid
title('blue=RMS therm, cyan=STD therm, red=RMS F-error')
