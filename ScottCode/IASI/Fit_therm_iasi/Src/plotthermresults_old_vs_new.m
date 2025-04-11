% Plot therm results
%
% Plot RMS error results for old 5 and new 6 term thermal F-factor fits.
%

% Created: 25 April 2005, Scott Hannon - based on plotthermresults.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
clf

bands={'long', 'short'};

rmserror5=[];
rmserror6=[];
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
   rmserror5=[rmserror5, rmsall'];

   eval(['load therm_fcoef6_' band]);
   rmserror6=[rmserror6, rmsall'];

   eval(['load ../Data/thermdata_' band]);

   btt=radtot(freq,1000*radwiththerm);
   btnot=radtot(freq,1000*radnotherm);
   btdif=btt - btnot;

   btmean=[btmean, mean(btt')];
   stdtherm=[stdtherm, std(btdif')];
   rmstherm=[rmstherm, sqrt(mean( btdif'.^2 ))];

end

ind5=find(rmserror5 < 1000);
ind6=find(rmserror6 > 0);

subplot(3,1,1)
plot(frequency,btmean,'b.-'),axis([650 2660 215 280]),grid
title('mean BT spectra (K)')
%
subplot(3,1,2)
plot(frequency,rmstherm,'b.'), axis([650 2660 0 2]),grid
title('RMS refl thermal contribution (K) at e=0.85')
%
subplot(3,1,3)
plot(frequency(ind5),100*rmserror5(ind5),'b.', ...
    frequency(ind6),    rmserror6(ind6),'r.'), axis([650 2660 0 25]),grid
title('Refl thermal RMS fitting error(%): blue=old, red=new')
