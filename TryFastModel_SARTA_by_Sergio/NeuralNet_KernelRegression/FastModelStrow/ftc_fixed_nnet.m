% ftc_nnet_water.m
%
% One channel test, fixed only
% Load layer-to-space transimttances
load Lay2space/saf704_f_chan49
% Profile inputs from get_profile.m
load Inputs/profiles_704

secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
nsecang = length(secang);

[~, nprof] = size(ptemp);
nprof = nprof*nsecang;

% This assume f is really fwo, but OK for now
f = squeeze(f(:,1:nsecang,1,1:100));
% Want all profiles in a row
f = reshape(f,nprof,100);

% Need a secant vector to go with profile vector
s = repmat(secang,704,1);
s = reshape(s,1,nprof);

% Clean up l2s
f = f';
% A few negative l2s's sneark in
f(f(:)<= 1E-7) = 1E-7;

% % Weight matrix, derivative of layer-to-space (not used)
% w = abs(diff(f,1));
% w(w<0) = 0;
% for i=1:99
%    w(i,:) = w(i,:)./max(w(i,:));
% end
% w(99:100,:) = 0;

% Compute layer tau (tl) and optical depths (tk)
xf = f;
xf(101,:) = 1;
% From layer trans and layer k
tl = xf(1:100,:)./xf(2:101,:);
tk = -log(tl);
clear xf

%--------------------------------------------------------------------
% Start fits
%--------------------------------------------------------------------

% Fit optical depths
trat = repmat(ptemp,1,6);
trat = (trat-200)/60;  % No need for Tref.  Arb. scaling.
wrat = repmat(water_ratio,1,6);

ig = randsample(1:nprof,round(nprof)/2);
clear ctl
for i=1:100
   i
   net = fitnet(3);
   net.divideParam.trainRatio = 0.7;
   net.divideParam.valRatio = 0.15;
   net.divideParam.testRatio = 0.15;
   net.trainParam.epochs = 50;   % Limit to 50 iterations

   targets = tk(i,:);
   % Select half of input data for training, ignore targets with negative values
   k = logical(zeros(1,nprof));
%   ig = randsample(1:nprof,round(nprof)/2);
   k(ig) = 1;
   k(targets <= 0) = 0;
   n_targets = length(find(k == 1));
   
   if n_targets > 100  % arbitrary cuton
      netexist(i) = true;

      targets_scale = mean(targets(k));   
      targets = targets./targets_scale;

      inputs = [wrat(i:100,k); trat(i:100,k)];
      % Add in secant as a predictor
      inputs(end+1,:) = s(k);

      [net,tr]=train(net,inputs,targets(k));

      % Test on all profiles
      testinputs = [wrat(i:100,:); trat(i:100,:)];
      testinputs(end+1,:) = s;
      % Add in secant as a predictor
      a = net(testinputs);
      % Rescale
      a = a*targets_scale;

      % Fill diagnostics
      netfitstd(i)     = nanstd(tk(i,:)-a);
      netmean(i)       = nanmean(tk(i,:));
      netfitmean(i)    = nanmean(tk(i,:)-a);
      netfinal(i).net  = net;
      netfinal(i).calc = a;
      netfinal(i).obs  = tk(i,:);
      netfinal(i).k    = ig;
      netfinal(i).kuse = k;
      netfinal(i).targets_scale = targets_scale; 
   else
      netexist(i) = false;
   end
end
save fit_k_by_layer_chan49_mod_temp_ratio_newcode_sameig_alllayers netfitstd netmean netfitmean netfinal

% %--------------------------------------------------------------------
% % Fit all l2s at one time
% net = fitnet(7);
% net.divideParam.trainRatio = 0.7;
% net.divideParam.valRatio = 0.15;
% net.divideParam.testRatio = 0.15;
% 
% [net,tr]=train(net,inputs(:,ig),targets(:,ig));
% ctargets = net(inputs);
% % save results

% %--------------------------------------------------------------------
% % Fit individual layer transmittances separately
% net = fitnet(7);
% net.divideParam.trainRatio = 0.7;
% net.divideParam.valRatio = 0.15;
% net.divideParam.testRatio = 0.15;
% 
% clear ctargets
% for i=1:100
%    [net,tr]=train(net,inputs(i:100,ig),tl(i,ig),[],[],w(i,ig));
% % All layers input   [net,tr]=train(net,inputs(:,ig),tl(i,ig),[],[],w(i,ig));
%    ctl(i,:) = net(inputs);
%    i
% end
% % save results

