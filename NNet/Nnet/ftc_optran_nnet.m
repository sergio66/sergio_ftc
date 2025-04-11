% ftc_optran_nnet.m
%
% One channel test
% This is just fw, used for early testing
% load Lay2space/saf704_f_chan1099

chi = 1;
tmin = 2E-6;  % Min l2s allowed
regr49 = false;

% This is fwo, and fo (normal use)
if regr49
   load Lay2space/cris_ch1099_fwo_fo_regr49.mat
else
   load Lay2space/cris_ch1099_fwo_fo.mat
end
[nprof_orig,~] = size(fwo);

% Line-by-line secant angles
secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
nsecang = length(secang);
nprof = nprof_orig*nsecang;

% Optical depths in optran space
kwel = optical_depth_fwo_fo(fwo,fo,tmin,nprof,nsecang,chi);

% % Need a secant vector to go with profile vector
% s = repmat(secang,nprof_orig,1);
% s = reshape(s,1,nprof);

% Profile pressure and temperature in optran space
fn = 'Inputs/profiles_704';
[pz,tz,optran_layindex,prof_optran_index,s,nprof,~] = interp_profile_to_optran(fn,secang,regr49);

% Convert optical depths on OPTRAN grid
for i=1:nprof
   tk(:,i) = interp1(1:100,kwel(1:100,i),prof_optran_index(:,i),'linear');
end
%--------------------------------------------------------------------
% Start fits
%--------------------------------------------------------------------
% Number of NaN's per OPTRAN level
numbad = sum(isnan(pz),2);
% From TOA, find first level where all scenes are present
iend = find( numbad == 0,1,'last');
% Starting at bottom, find first OPTRAN layer with 150 scenes (no NaN)
istart = find( (nprof - numbad) > 150,1);
% Subsample 1/2 of inputs for training
ig = randsample(1:nprof,round(nprof)/2);
% Loop over optran layers
for i=istart:iend
   i
   % Train on ig subset
   k = logical(zeros(1,nprof));  % Start with none selected
   k(ig) = 1;  % Select ig subset
   k(isnan(tk(i,:))) = 0;  % if data is NaN, ignore as well
% Form inputs from profile, add in secant values
   inputs = [pz(i:iend,:); tz(i:iend,:)];
   inputs(end+1,:) = s;
   inputs = inputs(:,k);  % Subset for training
   targets_raw = tk(i,k);     % Subset for training
   targets_scale = mean(targets_raw);  % Scale targets
   targets = targets_raw./targets_scale;
   if (i > 130 )  % CHANNEL dependent, need algorithm for "130"
      net = fitnet(2);
      net.trainParam.epochs = 50;  % Seems enough, increase when almost done
   else
      net = fitnet(3);
      net.trainParam.epochs = 50;  % Seems enough, increase when almost done
   end
% Train   
   [net,tr]=train(net,inputs,targets);%,[],[],wo(i,ig));
% Test on all profiles
   testinputs = [pz(i:iend,:); tz(i:iend,:)];
% Add in secant as a predictor
   testinputs(end+1,:) = s;
   a = net(testinputs);  % Evaluate nnet
% Rescale
   a = a*targets_scale;
% Diagnostics
   netfitstd(i)  = nanstd(targets_raw-a(k));
   netmean(i)    = nanmean(targets_raw);
   netfitmean(i) = nanmean(targets_raw-a(k));
   netfinal(i).net = net;
   netfinal(i).calc = a;
   netfinal(i).obs = tk(i,:);
   netfinal(i).k = k;
   netfinal(i).targets_scale = targets_scale; 
end
netfit_kselect = ig;
%--------------------------------------------------------------------
% End fits
%--------------------------------------------------------------------

save xoptran_chan1099_v1_regr49_jan23test  netfitstd netmean netfitmean netfit_kselect netfinal net_optran_layindex net_prof_optran_index
%--------------------------------------------------------------------
% End of code
%--------------------------------------------------------------------
