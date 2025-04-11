iLoad = input('already have ods in memory (+1) , or reload (-1) ??? ');
if iLoad < 0
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
  gas_1 = gas_1(:,1:100);
  gas_3 = gas_3(:,1:100);
  ptemp = ptemp(:,1:100);
  
  gas_1 = fliplr(gas_1);
  gas_3 = fliplr(gas_3);
  ptemp = fliplr(ptemp);
end

iCnt = 0;
for ii = 1 : 2490
  if mod(ii,500) == 0
    fprintf(1,'o')
  elseif mod(ii,100) == 0
    fprintf(1,'.')
  end

  y1str = ['COEFFS/605/aeriSRFv2_water605_chan' num2str(ii) '.mat'];
  y2str = ['COEFFS/605/aeriSRFv2_fixed605_chan' num2str(ii) '.mat'];
  y3str = ['COEFFS/605/aeriSRFv2_ozone605_chan' num2str(ii) '.mat'];

  iaExist(ii) = 0;
  if exist(y1str) > 0 &  exist(y2str) > 0 &  exist(y3str) > 0
    iCnt = iCnt + 1;
    iaExist(ii) = 1;
    ystr = ['COEFFS/605/aeriSRFv2_water605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    CW(ii,:,:) = y.xCW;

    ystr = ['COEFFS/605/aeriSRFv2_fixed605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    CF(ii,:,:) = y.xCF;

    ystr = ['COEFFS/605/aeriSRFv2_ozone605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    COz(ii,:,:) = y.xCOz;
  end
end
fprintf(1,'loaded in  %4i out of 2490 F W O files ...\n',iCnt)

xstartup
iDo = find(iaExist > 0);

emis = 1.0;
space = 2.73;
for ip = 1 : 49
  rtryW   = emis * ttorad(fc(iDo),space);
  rtryF   = emis * ttorad(fc(iDo),space);
  rtryO   = emis * ttorad(fc(iDo),space);
  rtryFW  = emis * ttorad(fc(iDo),space);
  rtryFWO = emis * ttorad(fc(iDo),space);
  temps = ptemp(ip,:);
  odFWO = squeeze(CW(iDo,:,ip)) + squeeze(CF(iDo,:,ip)) + squeeze(COz(iDo,:,ip));
  odFW  = squeeze(CW(iDo,:,ip)) + squeeze(CF(iDo,:,ip));
  odW  = squeeze(CW(iDo,:,ip));
  odF  = squeeze(CF(iDo,:,ip));
  odO  = squeeze(COz(iDo,:,ip));
  for il = 100 : -1 : 1
    layOD = odFWO(:,il);
    rtryFWO = rtryFWO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odFW(:,il);
    rtryFW = rtryFW .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odW(:,il);
    rtryW = rtryW .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odO(:,il);
    rtryO = rtryO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odF(:,il);
    rtryF = rtryF .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

  end
  raaTryO(ip,:)  = rtryO;
  raaTryW(ip,:)  = rtryW;
  raaTryF(ip,:)  = rtryF;
  raaTryFW(ip,:)  = rtryFW;
  raaTryFWO(ip,:) = rtryFWO;
end

plot(fc(iDo),rad2bt(fc(iDo),raaKCARTAconv(:,iDo)'),'b',fc(iDo),rad2bt(fc(iDo),raaTryFWO'),'r')

figure(1); 
plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryW')'),'r',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryW')'),'k')
title('W')

figure(2); 
plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryF')'),'r',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryF')'),'k')
title('F')

figure(3); 
plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryO')'),'r',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryO')'),'k')
title('O')

%figure(4); 
%plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),...
%     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'r',...
%     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'k',...
%     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFW')'),'m');
%hold on
%plot(fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFW')'),'color',[0.5 0.5 0.5])
%hold off
%title('FW(dash) FWO(solid)')

figure(5); 
plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'r',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'k')
title('SuperDuper v2')
axis([605 1800 -3 3]); grid on
axis([605 1800 -3 300]); grid on

figure(6); 
plot(mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'b.',...
     mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'r.')
xlabel('BT'); ylabel('Mean or Std bias')

figure(7);
 dbt = -2:0.05:+2;
 nmean = hist(real(mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')')),dbt);
 nstd  = hist(real(std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')')),dbt);
 plot(dbt,nmean,dbt,nstd,'r')