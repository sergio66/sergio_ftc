% rta_optran.m
%
% Get nets for optran layers
load optran_chan1099_v1_jan23test

load_fcris_hr
chi = 1099;
freq = fcris_hr;

% Put nnet scaling into single variable
for i=1:300
   if netfinal(i).targets_scale
      tscale(i) = netfinal(i).targets_scale;
   else
      tscale(i) = NaN;
   end
end
% Line-by-line secant angles
secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
%---------------------------- Profiles In c ---------------------------
% Profile pressure and temperature in optran space
fn = 'Inputs/profiles_704';
regr49 = false;
[pz,tz,optran_layindex,prof_optran_index,s,nprof,ptemp,temp_ratio,pmean,stemp] = interp_profile_to_optran(fn,secang,regr49);

% !!Be careful about two sets of optran_layindex and prof_optran_index laying around

% Query Nnet to get layer optical depths in OPTRAN space
iend = 300;
for i=1:iend
   inputs = [pz(i:iend,:); tz(i:iend,:)];
   inputs(end+1,:) = s;
   if size(netfinal(i).net) > 0
      a = netfinal(i).net(inputs);
      ctk(i,:) = a*tscale(i);   % Correct?
   else
      ctk(i,:) = nan(1,nprof);
   end
end

% Get truth
regr49 = false;
% This is fwo, and fo (normal use)
if regr49
   load Lay2space/cris_ch1099_fwo_fo_regr49.mat
else
   load Lay2space/cris_ch1099_fwo_fo.mat
end
chi_fwo = 1;
tmin = 2E-6;  % Min l2s allowed
nsecang = length(secang);
koall = optical_depth_fwo_fo(fwo,fo,tmin,nprof,nsecang,chi_fwo);

% What levels will contribute to optical depth?

Mixing optran and profile spaces here!!!  FIX

k1 = ~isnan(net_prof_optran_index);
k2 = ~isnan(koall);
kx = and(k1,k2);

% Convert from OPTRAN levels to 100-layer set
for i=1:nprof
   kcall(:,i) = interp1(net_prof_optran_index(kx(:,i),i),ctk(kx(:,i),i),1:100,'linear');
end

tk = koall;
ctk = kcall;

tk(isnan(tk)) = 0;
ctk(isnan(ctk)) = 0;
ctk(ctk < 0 ) = 0;
% Layer transmittances
tl = exp(-tk);
ctl = exp(-ctk);
tl(101,:) = 1;
ctl(101,:) = 1;
% Compute layer-to-space transmittances
for i=1:101;
   l2s(i,:)  = prod(tl(i:101,:));
   l2sc(i,:) = prod(ctl(i:101,:));
end

% Radiative transfer
for i=1:nprof
   tsurf = stemp(i);
   arad(i) = 0;
   carad(i) = 0;
   sfcemit(i) = bt2rad(freq(chi),tsurf)*l2s(1,i);
   csfcemit(i) = bt2rad(freq(chi),tsurf)*l2sc(1,i);
   for l = 1:100
      arad(i) =  arad(i)  + bt2rad(freq(chi),ptemp(l,i)) .* (l2s(l+1,i) - l2s(l,i));
      carad(i) = carad(i) + bt2rad(freq(chi),ptemp(l,i)) .* (l2sc(l+1,i) - l2sc(l,i));
   end
   rad = sfcemit + arad;
   crad = csfcemit + carad;
end

% Convert to observed BT
btobs = rad2bt(freq(chi),rad);
btcal = rad2bt(freq(chi),crad);

% Get latitudes that go with the 704*6 profile set
load Inputs/lat_4224set
