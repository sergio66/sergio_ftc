% local running to test
% clustcmd -L find_predictors_uplook49_500v2.m 1:201
%
% otherwise when happy
% clustcmd -q short -n 36 find_predictors_uplook49_500v2.m 1:201

disp('hello sergio this is "find_predictors_uplook49_500v2.m" ')
JOB
iChan0 = JOB(1);

xstartup

B = 500;
for ip = 1 : 49
  fin = ['CONV_trans/prof_' num2str(ip) '_band_' num2str(B) '.mat'];
  loader = ['load  ' fin]; eval(loader);

  raaKCARTAconv(ip,:) = rxconv;
  raaFastModel0(ip,:)  = qcx;

  Feff = yqcF;
  Weff = yqcFW./yqcF;
  Oeff = yqcFWO./yqcFW;

  gas_1(ip,:) = pN.gas_1;
  gas_3(ip,:) = pN.gas_3;
  ptemp(ip,:) = pN.ptemp;
  stemp(ip)   = pN.stemp;

  for ix = 100 : -1 : 1
    tixp1 = Feff(:,ix+1) .* Oeff(:,ix+1) .* Weff(:,ix+1);
    tix   = Feff(:,ix-0) .* Oeff(:,ix-0) .* Weff(:,ix-0);
    t     = tixp1./tix;

    kixp1 = Feff(:,ix+1); kix = Feff(:,ix); k = kixp1./kix; k = -log(k); kFeff(ip,ix,:) = k;
    kixp1 = Weff(:,ix+1); kix = Weff(:,ix); k = kixp1./kix; k = -log(k); kWeff(ip,ix,:) = k;
    kixp1 = Oeff(:,ix+1); kix = Oeff(:,ix); k = kixp1./kix; k = -log(k); kOeff(ip,ix,:) = k;
  end
end

plot(fc,squeeze(kWeff(1,:,:))')

[h,ha,p,pa] = rtpread('regr48.op.rtp');
pav = p.plays(1:100,:);
pav = flipud(pav)'/1013;

gas_1 = gas_1(:,1:100);
gas_3 = gas_3(:,1:100);
ptemp = ptemp(:,1:100);

gas_1 = fliplr(gas_1);
gas_3 = fliplr(gas_3);
ptemp = fliplr(ptemp);

plot(fc,squeeze(kWeff(1,:,:))','b',fc,squeeze(kOeff(1,:,:))','g',...
     fc,squeeze(kFeff(1,:,:))','r')
pause(0.1);

iChan = 1;
  water = squeeze(kWeff(:,:,iChan));
figure(1); plot(water(:,1),ptemp(:,1),'.'); title(num2str(iChan));
figure(2); plot(water(:,1),gas_1(:,1)/1e20,'.'); title(num2str(iChan));
figure(3); plot(water(:,1),gas_3(:,1)/1e20,'.'); title(num2str(iChan));

pause(0.1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tref = ptemp(49,:);
g1ref= gas_1(49,:);
g3ref= gas_3(49,:);

dp = pav(:,1:99)-pav(:,2:100); dp(:,100) = zeros(1,49);
pwgt = pav.*dp; pwgt = cumsum(pwgt);

%% now make up La Familia de Predictors
%% Fixed and/or Temp
familyF.name = 'Fixed';
%% first do constant and linear terms
ii=1; vars(ii,:,:) = ones(size(ptemp));           names{ii} = 'constant';
ii=ii+1; vars(ii,:,:) = stemp'*ones(1,100);          names{ii} = 'stemp';
ii=ii+1; vars(ii,:,:) = ptemp;                       names{ii} = 'ptemp';
ii=ii+1; vars(ii,:,:) = gas_1/1e22;                  names{ii} = 'gas_1';
ii=ii+1;  vars(ii,:,:) = gas_3/1e19;                 names{ii} = 'gas_3';

%% then do nonlinear terms
ii=ii+1; vars(ii,:,:) = ptemp ./ (ones(49,1)*tref);    names{ii} = 'T/Tref';
ii=ii+1; vars(ii,:,:) = ptemp -  (ones(49,1)*tref);    names{ii} = 'T-Tref';
ii=ii+1; wgtT = cumsum(pwgt' .* ptemp') ./ cumsum(pwgt');
         vars(ii,:,:) = wgtT'/273;                           names{ii} = 'presswgtTx';
ii=ii+1; vars(ii,:,:) = (ptemp./(ones(49,1)*tref)).^(0.5);  names{ii} = 'sqrt(T/Tref)';
ii=ii+1; vars(ii,:,:) = (ptemp./(ones(49,1)*tref)).^(2.0);  names{ii} = '(T/Tref)^2';

ii=ii+1;  iiW = ii; vars(ii,:,:) = gas_1 ./ (ones(49,1)*g1ref);  names{ii} = 'g1/g1ref';
ii=ii+1; vars(ii,:,:) = sqrt(gas_1 ./ (ones(49,1)*g1ref));       names{ii} = 'sqrt(g1/g1ref)';
ii=ii+1; vars(ii,:,:) = (gas_1 ./ (ones(49,1)*g1ref)).^2;        names{ii} = '(g1/g1ref)^2';
ii=ii+1; wgtW = (pav.*gas_1); vars(ii,:,:) = wgtW/1e21;          names{ii} = 'pressWgtW';
ii=ii+1; wgtW = cumsum(pwgt' .* gas_1') ./ cumsum(pwgt');
         vars(ii,:,:) = wgtW'/1e21;                               names{ii} = 'presswgtWx';
ii=ii+1; vars(ii,:,:) = squeeze(vars(iiW,:,:)) .* ...
                      (ptemp ./ (ones(49,1)*tref));              names{ii} = 'g1/g1ref * T/Tref';

ii=ii+1; iiO = ii; vars(ii,:,:) = gas_3 ./ (ones(49,1)*g3ref);  names{ii} = 'g3/g3ref';
ii=ii+1; vars(ii,:,:) = sqrt(gas_3 ./ (ones(49,1)*g3ref));      names{ii} = 'sqrt(g3/g3ref)';
ii=ii+1; vars(ii,:,:) = (gas_3 ./ (ones(49,1)*g3ref)).^2;       names{ii} = '(g3/g3ref)^2';
ii=ii+1; wgtO = (pav.*gas_3); vars(ii,:,:) = wgtO/1e18;         names{ii} = 'pressWgtO';
ii=ii+1; wgtO = cumsum(pwgt' .* gas_3') ./ cumsum(pwgt');
         vars(ii,:,:) = wgtO'/1e18;                             names{ii} = 'presswgtOx';
ii=ii+1; vars(ii,:,:) = squeeze(vars(iiO,:,:)) .* ...
                      (ptemp ./ (ones(49,1)*tref));             names{ii} = 'g3/g1ref * T/Tref';

familyF.vars  = vars;
familyF.names = names;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  waterOD = max(squeeze(kWeff(:,:,iChan)),0);
  origCW  = waterOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(waterOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(waterOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(waterOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [CW(il,:) coeffW(il,:) listW(il,:)] = ...
      optimize_predictor2(il,waterOD,stemp,ptemp,gas_1,gas_3);
    title('simple'); grid

    figure(2); clf
    [xCW(il,:) xcoeffW(il,:) xlistW(il,:)] = optimize_predictor2x(il,waterOD,familyF);
    title('complex'); grid
    pause(0.1)
  end

end
saver = ['save COEFFS/500/water500_chan' num2str(iChan0) '.mat iChan origCW *CW *coeffW *listW'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  fixedOD = max(squeeze(kFeff(:,:,iChan)),0);
  origCF  = fixedOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(fixedOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(fixedOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(fixedOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [CF(il,:) coeffF(il,:) listF(il,:)] = ...
      optimize_predictor2(il,fixedOD,stemp,ptemp,gas_1,gas_3);
    title('simple'); grid

    figure(2); clf
    [xCF(il,:) xcoeffF(il,:) xlistF(il,:)] = optimize_predictor2x(il,fixedOD,familyF);
    title('complex'); grid
    pause(0.1)

  end

end
saver = ['save COEFFS/500/fixed500_chan' num2str(iChan0) '.mat iChan origCF *CF *coeffF *listF'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  ozoneOD = max(squeeze(kOeff(:,:,iChan)),0);
  origCOz = ozoneOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(ozoneOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(ozoneOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(ozoneOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [COz(il,:) coeffOz(il,:) listOz(il,:)] = ...
      optimize_predictor2(il,ozoneOD,stemp,ptemp,gas_1,gas_3);
    title('simple'); grid

    figure(2); clf
    [xCOz(il,:) xcoeffOz(il,:) xlistOz(il,:)] = optimize_predictor2x(il,ozoneOD,familyF);
    title('complex'); grid
    pause(0.1)

  end

end

saver = ['save COEFFS/500/ozone500_chan' num2str(iChan0) '.mat iChan origCOz *COz *coeffOz *listOz'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now do quick radtrans
%% quickcheck_odpredictors_radtrans500
