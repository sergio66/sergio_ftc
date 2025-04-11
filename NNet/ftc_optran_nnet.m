% ftc_optran_nnet.m
%
% One channel test
NOT WORKING, integrating new code with old syntax from regression fits

% Load layer-to-space transimttances

% This is just fw, used for early testing
% load Lay2space/saf704_f_chan1099

% This is fwo, and fo (normal use)
load Lay2space/cris_ch1099_fwo_fo.mat

% % Profile inputs from get_profile.m
% load('profiles_704','water_ratio','temp_ratio','ozone_ratio');

% Line-by-line secant angles
secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
nsecang = length(secang);

chi = 1;  % Not to be used in freq(chi)!!!

nprof = 704;

%below works, but need to intergrate after keyboard


% Subset l2s for LW secant angles and channel of interest
   fwo = squeeze(fwo(:,1:nsecang,chi,:));
   fwo(:,:,101) = 1; 
   fo = squeeze(fo(:,1:nsecang,chi,:));
   fo(:,:,101) = 1; 

% Compute layer transmittances 
   wel = squeeze((fwo(:,:,1:100)./fo(:,:,1:100) )./( (fwo(:,:,2:101)./fo(:,:,2:101))));

% Get things in proper order
   wel = reshape(wel,nprof*nsecang,100);

% Do the same with fwo, etc for ease of comparisons later and bad_fwo
   fwo = reshape(fwo,nprof*nsecang,101);

% Find upper envelope of l2s inputs
   [fwo_max,i]=max(fwo',[],2);
% Find index of layer where can set coefficients == 0
   start_layer = find(fwo_max >= 2E-4,1);

% Replace low/high transmittances with a threshold value
   tmin = 2E-6;
   wel(wel < tmin) = tmin;

% tmax = 0.99999;
% fel(fel > 1) = tmax;
% wel(wel > 1) = tmax;
% oel(oel > 1) = tmax;

% Convert to absorption coefficients
   kwel = -log(wel);


% keyboard
% 
% % saf705_f_chan1099 has fw in it for now
% % Do NOT fit top l2s set == 1, really screws things up
% f = squeeze(fw(:,1:nsecang,1,1:100));
% % Want all profiles in a row
% f = reshape(f,704*6,100);
% f = f';
% [~,num_scenes] = size(f);
% 
% Need a secant vector to go with profile vector
s = repmat(secang,704,1);
s = reshape(s,1,704*6);

% Load Scott's OPTRAN layers
load Inputs/olays
% % First way
% azop_extend = 1.2;  % New 704 profiles broke peak azop
% azop = azop_extend*1E3*6.022E23*olays;
% azop = flipud(azop);
% Second way
azop_extend = 1.4;  % New 704 profiles broke peak azop
azop = azop_extend*1E4*6.022E23*olays;
azop = flipud(azop);

% Load in various profile quantities for fitting
load('Inputs/profiles_704','press','ptemp','az','temp_ratio');

% Need secant applied to water profile for OPTRAN
az = repmat(az,1,6);
for i=1:100
   az(i,:) = az(i,:).*s;
end
% Second way
az(101,:) = 1E14;
for i=1:101
   azup(i,:) = sum(az(i:101,:));
end
az = azup;

% Indices in OPTRAN for each water layer for each profile
optran_layindex = interp1(azop,1:300,az,'linear');
optran_layindex(100:101,:) = 300;

% Extend pressure and temperature fields to all secants
ptemp = repmat(ptemp,1,6);
temp_ratio = repmat(temp_ratio,1,6);
% Use layer mean pressures
pmean = (press(1:100) + press(2:101))/2;

% OPTRAN transformations
for i=1:6*704
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
% for i=1:num_scenes
%    pz(:,i) = pz(:,i)./pz_ref;
% end

% Fit optical depths for OPTRAN
% targets = f;
% tmin = 2E-6;
% targets(targets(:) < tmin) = NaN;
% targets(101,:) = 1;
% tl = targets(1:100,:)./targets(2:101,:);
% clear targets
% otk = -log(tl);  % optical depths on pressure grid
% otk(otk<0) = 0;


keyboard
% Convert to optical depths on OPTRAN grid
for i=1:6*704
   tk(:,i) = interp1(1:100,otk(1:100,i),prof_optran_index(:,i),'linear');
%   wo(:,i) = interp1(1:100,w(1:100,i),gg(:,i),'linear');
end
%--------------------------------------------------------------------
% Start fits
%--------------------------------------------------------------------
net = fitnet(1);
net.divideParam.trainRatio = 0.7;
net.divideParam.valRatio = 0.15;
net.divideParam.testRatio = 0.15;

% inputs = [pz; tz];
% Number of NaN's per OPTRAN level
numbad = sum(isnan(pz),2);
% From TOA, find first level where all scenes are present
iend = find( numbad == 0,1,'last');
% Starting at bottom, find first OPTRAN layer with 150 scenes (no NaN)
istart = find( (num_scenes - numbad) > 150,1);

%for i=istart:iend
for i=125:iend
   i
   % Find good profiles for this layer, train on 1/2 of them randomly sampled
   k = ~isnan(pz(i,:));
   ki = find(k == 1);
   ig = randsample(ki,round(length(ki)/2));
   igbad = setdiff(1:length(k),ig);
   kuse = k;
   kuse(igbad) = 0;
   % Fit all data
   kuse = k;
   % Form inputs based on 1/2 of good profiles, train on profile "above"
   inputs = [pz(i:iend,kuse); tz(i:iend,kuse)];
   % Add in secant as a predictor
   inputs(end+1,:) = s(kuse);
   if (i > 130 )
      net = fitnet(2);
      net.trainParam.epochs = 50;
   else
      net = fitnet(3);
   end
   net.trainParam.epochs = 200;   % Limit to 50 iterations

   targets = tk(i,kuse);
   targets_scale = mean(targets);   % Use max(targets?) targets all > 0, so OK? or mean(abs(targets))
   targets = targets./targets_scale;
% Train   
   [net,tr]=train(net,inputs,targets);%,[],[],wo(i,ig));
% Second training sometimes helps, usually pretty fast
   [net,tr]=train(net,inputs,targets);%,[],[],wo(i,ig));
% Test on all good profiles
   testinputs = [pz(i:iend,k); tz(i:iend,k)];
% Add in secant as a predictor
   testinputs(end+1,:) = s(k);
   a = net(testinputs);
% Rescale
   a = a*targets_scale;
   netfitstd(i)  = nanstd(tk(i,k)-a);
   netmean(i)    = nanmean(tk(i,k));
   netfitmean(i) = nanmean(tk(i,k)-a);
   netfinal(i).net = net;
   netfinal(i).calc = a;
   netfinal(i).obs = tk(i,k);
   netfinal(i).k = k;
   netfinal(i).kuse = kuse;
   netfinal(i).targets_scale = targets_scale; 
end
%--------------------------------------------------------------------
% End fits
%--------------------------------------------------------------------

% A few more outputs needed
net_optran_layindex = optran_layindex;
net_prof_optran_index = prof_optran_index;

save fixed_net_optran_chan1099_v1_fitallchans  netfitstd netmean netfitmean netfinal net_optran_layindex net_prof_optran_index

%--------------------------------------------------------------------
% End of code
%--------------------------------------------------------------------














%--------------------------------------------------------------------
%--------------------------------------------------------------------
%--------------------------------------------------------------------
%--------------------------------------------------------------------
% Old code, uses find instead of logical mask for indices
   
%    k = find(~isnan(pz(i,:)) == 1);
%    length(k)
%    igsub = randsample(length(k),round(length(k)/2));
%    ig = k(igsub);
%    ig = k;

%inputs = [pz(i:iend,ig); tz(i:iend,ig)];

% % Weighting always makes things worse
% targets = f;
% targets(101,:) = 1;
% % Weight matrix, derivative of layer-to-space
% w = abs(diff(targets,1));
% w(w<0) = 0;
% % % for all layers at once fit
% for i=1:num_scenes
%    w(:,i) = w(:,i)./max(w(:,i));
% end
% w(99:100,:) = 0;  % Fix this by getting w before cutting off f
% 
% Fits of individual layers
% for i=1:99
%    w(i,:) = w(i,:)./max(w(i,:));
% end
% w(99:100,:) = 0;  % Fix this by getting w before cutting off f

% Normalize this wo
% for i=1:300
%    wo(i,:) = wo(i,:)./max(wo(i,:));
% end

