function [pz, tz, optran_layindex, prof_optran_index, s, nprof, ptemp, temp_ratio, pmean, stemp] = interp_profile_to_optran(fn,secang,regr49);

% Load in various profile quantities for fitting
if regr49
   load(fn,'press','ptemp','az','temp_ratio','stemp');
   ptemp = ptemp(:,1:49);
   az = az(:,1:49);
   temp_ratio = temp_ratio(:,1:49);
else
   load(fn,'press','ptemp','az','temp_ratio','stemp')
end

[~, nprof_orig] = size(ptemp);
nsecang = length(secang);
% Need a secant vector to go with profile vector
s = repmat(secang,nprof_orig,1);
nprof = nprof_orig*nsecang;
s = reshape(s,1,nprof);

% Need secant applied to sum water profile above layer for OPTRAN
az = repmat(az,1,nsecang);
for i=1:100
   az(i,:) = az(i,:).*s;
end
az(101,:) = 1E14;

azop = optran_azop();
% Indices in OPTRAN for each water layer for each profile
optran_layindex = interp1(azop,1:300,az,'linear');
optran_layindex(100:101,:) = 300;

if regr49  % 49 profile set needed more optran levels, leave as is for now
   optran_layindex(1,251) = 1;
end

% Extend pressure and temperature fields to all secants
ptemp = repmat(ptemp,1,nsecang);
temp_ratio = repmat(temp_ratio,1,nsecang);
% Use layer mean pressures
pmean = (press(1:100) + press(2:101))/2;

% OPTRAN transformations
for i=1:nprof
% Indices in 100 layer space for each OPTRAN layer
   prof_optran_index(:,i) = interp1(optran_layindex(1:100,i),1:100,1:300,'linear');
% Profile temperatures interpolated to OPTRAN layers   
% Scaled as shown below
   xx = (temp_ratio(1:100,i) -1)*10;
   tz(:,i) = interp1(1:100,xx,prof_optran_index(:,i),'linear');
end

% Profile pressure interpolated to OPTRAN layers  
pz = interp1(1:100,pmean(1:100),prof_optran_index,'linear');

% Normalize pressures so range not so large (not needed)
% pz_ref = nanmean(pz,2);
% for i=1:nprof
%    pz(:,i) = pz(:,i)./pz_ref;
% end
