% Interpolate the m135 dec05 tuning to the m130/m140/m150 apr08 channel freqs

% Created: 22 April 2008, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load m135 dec05  tuning with corrected M12 freq
junk135=load('dec05deliv');

% Load m130,m140,m150 apr08 "ones" tuning
junk130 = load('ones130');
junk140 = load('ones140');
junk150 = load('ones150');

% Pull out freq info
freq135 = junk135(:,2);
freq130 = junk130(:,2);
freq140 = junk140(:,2);
freq150 = junk150(:,2);

% Long wave
idpt1 = 1:1864;
idpt2 = 1865:2378;
indpt1 = find(freq140 < 2000);
indpt2 = find(freq140 > 2000);

% Find channel past end of M1a
flast = max(freq135);
fpast = flast + 0.33*flast/1200;
indpast = find(freq140 > fpast);

% Interpolate wcon (column 5)
junk130(indpt1,5) = interp1(freq135(idpt1),junk135(idpt1,5), ...
   freq130(indpt1),'linear','extrap');
junk140(indpt1,5) = interp1(freq135(idpt1),junk135(idpt1,5), ...
   freq140(indpt1),'linear','extrap');
junk150(indpt1,5) = interp1(freq135(idpt1),junk135(idpt1,5), ...
   freq150(indpt1),'linear','extrap');
junk130(indpt2,5) = interp1(freq135(idpt2),junk135(idpt2,5), ...
   freq130(indpt2),'linear','extrap');
junk140(indpt2,5) = interp1(freq135(idpt2),junk135(idpt2,5), ...
   freq140(indpt2),'linear','extrap');
junk150(indpt2,5) = interp1(freq135(idpt2),junk135(idpt2,5), ...
   freq150(indpt2),'linear','extrap');
junk130(indpast,5) = 1;
junk140(indpast,5) = 1;
junk150(indpast,5) = 1;
save tunmlt_m130_wcononly junk130 -ascii
save tunmlt_m140_wcononly junk140 -ascii
save tunmlt_m150_wcononly junk150 -ascii

% Fake channels
indfake = 2379:2834;

% Interpolate other columns; skip ozone (col 6)
icol = [3,4,7,8,9];
for ii=1:length(icol)
   ic = icol(ii);
   junk130(:,ic) = interp1(freq135,junk135(:,ic),freq130,'linear','extrap');
   junk140(:,ic) = interp1(freq135,junk135(:,ic),freq140,'linear','extrap');
   junk150(:,ic) = interp1(freq135,junk135(:,ic),freq150,'linear','extrap');
%   junk130(indfake,ic) = 1;
%   junk140(indfake,ic) = 1;
%   junk150(indfake,ic) = 1;
end
save tunmlt_m130 junk130 -ascii
save tunmlt_m140 junk140 -ascii
save tunmlt_m150 junk150 -ascii

%%% end of program %%%
