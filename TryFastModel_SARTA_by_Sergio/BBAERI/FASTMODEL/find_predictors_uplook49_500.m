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

gas_1 = gas_1(:,1:100);
gas_3 = gas_3(:,1:100);
ptemp = ptemp(:,1:100);

gas_1 = fliplr(gas_1);
gas_3 = fliplr(gas_3);
ptemp = fliplr(ptemp);

plot(fc,squeeze(kWeff(1,:,:))','b',fc,squeeze(kOeff(1,:,:))','g',...
     fc,squeeze(kFeff(1,:,:))','r')
ret

iChan = 1;
  water = squeeze(kWeff(:,:,iChan));
figure(1); plot(water(:,1),ptemp(:,1),'.'); title(num2str(iChan));
figure(2); plot(water(:,1),gas_1(:,1)/1e20,'.'); title(num2str(iChan));
figure(3); plot(water(:,1),gas_3(:,1)/1e20,'.'); title(num2str(iChan));
ret

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = 1 : length(fc)
  waterOD = squeeze(kWeff(:,:,iChan));

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    figure(1); plot(waterOD(:,il),ptemp(:,il),'.'); title(str)
    figure(2); plot(waterOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    figure(3); plot(waterOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    [CW(iChan,il,:) coeffW(iChan,il,:)] = ...
      optimize_predictor1(il,waterOD,stemp,ptemp,gas_1,gas_3);
    save water500.mat CW coeffW
    ret(0.1)
  end

end
save water500.mat CW coeffW

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = 1 : length(fc)
  fixedOD = squeeze(kFeff(:,:,iChan));

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    figure(1); plot(fixedOD(:,il),ptemp(:,il),'.'); title(str)
    figure(2); plot(fixedOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    figure(3); plot(fixedOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    [CF(iChan,il,:) coeffF(iChan,il,:)] = ...
      optimize_predictor1(il,fixedOD,stemp,ptemp,gas_1,gas_3);
    save fixed500.mat CF coeffF
    ret(0.1)
  end

end
save fixed500.mat CF coeffF

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = length(fc) : -1 : 1
  ozoneOD = squeeze(kOeff(:,:,iChan));

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    figure(1); plot(ozoneOD(:,il),ptemp(:,il),'.'); title(str)
    figure(2); plot(ozoneOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    figure(3); plot(ozoneOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    [COz(iChan,il,:) coeffOz(iChan,il,:)] = ...
      optimize_predictor1(il,ozoneOD,stemp,ptemp,gas_1,gas_3);
    save ozone500.mat COz coeffOz
    ret(0.1)
  end

end
save ozone500.mat COz coeffOz

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now do quick radtrans
quickcheck_odpredictors_radtrans500
