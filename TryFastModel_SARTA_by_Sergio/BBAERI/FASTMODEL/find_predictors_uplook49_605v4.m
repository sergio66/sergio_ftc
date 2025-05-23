% local running to test
% clustcmd -L find_predictors_uplook49_605v4.m 1:2490
%
% otherwise when happy
% clustcmd -q short -n 72 find_predictors_uplook49_605v4.m 1:2490

disp('hello sergio this is "find_predictors_uplook49_605v4.m" ')
disp('Scott suggests using Hamming instead of NB ')
disp('Scott suggests using Hamming instead of NB ')
disp('Scott suggests using Hamming instead of NB ')

JOB
iChan0 = JOB(1);

xstartup

B = 605;
for ip = 1 : 49
  fin = ['CONV_trans/aeriSRF_prof_' num2str(ip) '_band_' num2str(B) '.mat'];
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

plot(fc,squeeze(kWeff(1,:,:))','b',fc,squeeze(kOeff(1,:,:))','g',...
     fc,squeeze(kFeff(1,:,:))','r')
pause(0.1);

gas_1 = gas_1(:,1:100);
gas_3 = gas_3(:,1:100);
ptemp = ptemp(:,1:100);

gas_1 = fliplr(gas_1);
gas_3 = fliplr(gas_3);
ptemp = fliplr(ptemp);

NP = length(stemp);   %% number of profiles

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_reference_profiles_uplook

iChan = 1;
  water = squeeze(kWeff(:,:,iChan));
figure(1); plot(water(:,1),ptemp(:,1),'.'); title(num2str(iChan));
figure(2); plot(water(:,1),gas_1(:,1)/1e20,'.'); title(num2str(iChan));
figure(3); plot(water(:,1),gas_3(:,1)/1e20,'.'); title(num2str(iChan));
pause(0.1);

pwgt = pwgt_regr49(49,:)' * ones(1,NP);
pwgt = pwgt';

pav = pav_regr49(49,:)' * ones(1,NP);
pav = pav';

do_make_predictors_v4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  waterOD = real(max(squeeze(kWeff(:,:,iChan)),0));
  origCW  = waterOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(waterOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(waterOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(waterOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [CW(il,:) coeffW(il,:) listW(il,:)] = ...
      optimize_predictor2(il,waterOD,stemp,ptemp,gas_1,gas_3);
    title(['simpleW lay = ' num2str(il)]); grid
    disp(' ')

    figure(2); clf
    [xCW(il,:) xcoeffW(il,:) xlistW(il,:)] = optimize_predictor2x(il,waterOD,familyAll);
    title(['complexW  lay = ' num2str(il)]); grid
    disp(' ')
    pause(0.1)
  end

end
saver = ['save COEFFS/605/aeriSRFv2_water605_chan' num2str(iChan0) '.mat iChan origCW *CW *coeffW *listW'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  fixedOD = real(max(squeeze(kFeff(:,:,iChan)),0));
  origCF  = fixedOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(fixedOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(fixedOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(fixedOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [CF(il,:) coeffF(il,:) listF(il,:)] = ...
      optimize_predictor2(il,fixedOD,stemp,ptemp,gas_1,gas_3);
    title(['simpleF  lay = ' num2str(il)]); grid
    disp(' ')

    figure(2); clf
    [xCF(il,:) xcoeffF(il,:) xlistF(il,:)] = optimize_predictor2x(il,fixedOD,familyAll);
    title(['complexF  lay = ' num2str(il)]); grid
    disp(' ')
    pause(0.1)

  end

end
saver = ['save COEFFS/605/aeriSRFv2_fixed605_chan' num2str(iChan0) '.mat iChan origCF *CF *coeffF *listF'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over predictors to find best set of predictors
for iChan = iChan0
  ozoneOD = real(max(squeeze(kOeff(:,:,iChan)),0));
  origCOz = ozoneOD;

  for il = 1 : 100
    str = ['Chan = ' num2str(iChan) ' freq = ' num2str(fc(iChan)) ' lay = ' num2str(il)];
    %figure(1); plot(ozoneOD(:,il),ptemp(:,il),'.'); title(str)
    %figure(2); plot(ozoneOD(:,il),gas_1(:,il)/1e20,'.'); title(str)
    %figure(3); plot(ozoneOD(:,il),gas_3(:,il)/1e20,'.'); title(str)

    figure(1); clf
    [COz(il,:) coeffOz(il,:) listOz(il,:)] = ...
      optimize_predictor2(il,ozoneOD,stemp,ptemp,gas_1,gas_3);
    disp(' ')
    title(['simpleO  lay = ' num2str(il)]); grid

    figure(2); clf
    [xCOz(il,:) xcoeffOz(il,:) xlistOz(il,:)] = optimize_predictor2x(il,ozoneOD,familyAll);
    title(['complexO  lay = ' num2str(il)]); grid
    disp(' ')
    pause(0.1)

  end

end

saver = ['save COEFFS/605/aeriSRFv2_ozone605_chan' num2str(iChan0) '.mat iChan origCOz *COz *coeffOz *listOz'];
eval(saver)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 totalOD = ozoneOD + waterOD + fixedOD;
CtotalOD = xCOz    + xCW     + xCF;
CtotalOD = CtotalOD';

xstartup
tspace = 2.7;
 origRT = ttorad(fc(iChan)*ones(1,49),tspace);
xorigRT = ttorad(fc(iChan)*ones(1,49),tspace);
for il = 100 : -1 : 1
  thetemp = ptemp(:,il);
  origRT   = origRT .* exp(-totalOD(:,il)) + ttorad(fc(iChan)*ones(1,49),ptemp(:,il)).*(1-exp(-totalOD(:,il)));
  xorigRT  = xorigRT .* exp(-CtotalOD(:,il)) + ttorad(fc(iChan)*ones(1,49),ptemp(:,il)).*(1-exp(-CtotalOD(:,il)));
end

kcartaBT = real(rad2bt(fc(iChan)*ones(1,49),raaKCARTAconv(:,iChan)));
origBT = real(rad2bt(fc(iChan)*ones(1,49),origRT));
xorigBT = real(rad2bt(fc(iChan)*ones(1,49),xorigRT));

figure(1)
plot(1:49,origBT,'o',1:49,xorigBT,'r',1:49,kcartaBT,'k'); 
hl=legend('from FWOeff','from fit','from KC'); set(hl,'fontsize',10); grid

arr = [mean(origBT-xorigBT) std(origBT-xorigBT)];
fprintf(1,'[mean(origBT-xorigBT) std(origBT-xorigBT)] = %8.6f %8.6f \n',arr)

arr = [mean(kcartaBT-xorigBT) std(kcartaBT-xorigBT)];
fprintf(1,'[mean(kcartaBT-xorigBT) std(kcartaBT-xorigBT)] = %8.6f %8.6f \n',arr)

%% now do quick radtrans
%% quickcheck_odpredictors_radtrans605v3

figure(2)
dn = 1:length(names); nO=hist(xlistOz(xlistOz>0),dn);
dn = 1:length(names); nF=hist(xlistF(xlistF>0),dn); 
dn = 1:length(names); nW=hist(xlistW(xlistW>0),dn); 
  plot(dn,nF,dn,nO,dn,nW); hl = legend('fixed','ozone','water');
  set(hl,'fontsize',10); grid
nmax = max(max(max(nF),max(nO)),max(nW));
line([iiW iiW],[0 nmax],'color','k','linewidth',2); 
line([iiO iiO],[0 nmax],'color','k','linewidth',2);

figure(3);
plot(1:100,CF./fixedOD','b',1:100,COz./ozoneOD','g',1:100,CW./waterOD','r')
axis([1 100 -2 3]); grid

figure(4);
plot(1:49,CF'./fixedOD,'bo',1:49,COz'./ozoneOD,'gx',1:49,CW'./waterOD,'rd')
axis([1 50 -2 3]); grid

addpath ~sergio/MATLABCODE/SHOWSTATS/
figure(5)
n = show2dstats_ratio(xCF',fixedOD,-2:0.2:+2,1:49,-1,5);
n = show2dstats_ratio(xCW',waterOD,-2:0.2:+2,1:49,-1,5);

figure(6);
ix = [2 5 7]; plot(1:100,xCW(:,ix),'b',1:100,waterOD(ix,:),'r')

figure(7);
ix = [2 5 7]; plot(1:100,xcoeffW(:,ix))