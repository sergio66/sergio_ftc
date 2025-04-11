iLoad = input('already have ods in memory (+1) , or reload (-1) ??? ');
if iLoad < 0
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
  gas_1 = gas_1(:,1:100);
  gas_3 = gas_3(:,1:100);
  ptemp = ptemp(:,1:100);
  
  gas_1 = fliplr(gas_1);
  gas_3 = fliplr(gas_3);
  ptemp = fliplr(ptemp);
end

for ii = 1 : 201
  ystr = ['COEFFS/500/water500_chan' num2str(ii) '.mat'];
  loader = ['y = load(''' ystr ''');'];
  eval(loader);
  CW(ii,:,:) = y.xCW;

  ystr = ['COEFFS/500/fixed500_chan' num2str(ii) '.mat'];
  loader = ['y = load(''' ystr ''');'];
  eval(loader);
  CF(ii,:,:) = y.xCF;

  ystr = ['COEFFS/500/ozone500_chan' num2str(ii) '.mat'];
  loader = ['y = load(''' ystr ''');'];
  eval(loader);
  COz(ii,:,:) = y.xCOz;
  end

emis = 1.0;
space = 2.73;
for ip = 1 : 49
  rtryW   = emis * ttorad(fc,space);
  rtryF   = emis * ttorad(fc,space);
  rtryO   = emis * ttorad(fc,space);
  rtryFW  = emis * ttorad(fc,space);
  rtryFWO = emis * ttorad(fc,space);
  temps = ptemp(ip,:);
  odFWO = squeeze(CW(:,:,ip)) + squeeze(CF(:,:,ip)) + squeeze(COz(:,:,ip));
  odFW  = squeeze(CW(:,:,ip)) + squeeze(CF(:,:,ip));
  odW  = squeeze(CW(:,:,ip));
  odF  = squeeze(CF(:,:,ip));
  odO  = squeeze(COz(:,:,ip));
  for il = 100 : -1 : 1
    layOD = odFWO(:,il);
    rtryFWO = rtryFWO .* exp(-layOD) + ttorad(fc,temps(il)) .* (1 - exp(-layOD));

    layOD = odFW(:,il);
    rtryFW = rtryFW .* exp(-layOD) + ttorad(fc,temps(il)) .* (1 - exp(-layOD));

    layOD = odW(:,il);
    rtryW = rtryW .* exp(-layOD) + ttorad(fc,temps(il)) .* (1 - exp(-layOD));

    layOD = odO(:,il);
    rtryO = rtryO .* exp(-layOD) + ttorad(fc,temps(il)) .* (1 - exp(-layOD));

    layOD = odF(:,il);
    rtryF = rtryF .* exp(-layOD) + ttorad(fc,temps(il)) .* (1 - exp(-layOD));

  end
  raaTryO(ip,:)  = rtryO;
  raaTryW(ip,:)  = rtryW;
  raaTryF(ip,:)  = rtryF;
  raaTryFW(ip,:)  = rtryFW;
  raaTryFWO(ip,:) = rtryFWO;
end

plot(fc,rad2bt(fc,raaKCARTAconv'),'b',fc,rad2bt(fc,raaTryFWO'),'r')

figure(1); 
plot(fc,mean(rad2bt(fc,raaKCARTAconv')'),...
     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryW')'),'r',...
     fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryW')'),'k')
title('W')

figure(2); 
plot(fc,mean(rad2bt(fc,raaKCARTAconv')'),...
     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryF')'),'r',...
     fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryF')'),'k')
title('F')

figure(3); 
plot(fc,mean(rad2bt(fc,raaKCARTAconv')'),...
     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryO')'),'r',...
     fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryO')'),'k')
title('O')

%figure(4); 
%plot(fc,mean(rad2bt(fc,raaKCARTAconv')'),...
%     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'r',...
%     fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'k',...
%     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFW')'),'m');
%hold on
%plot(fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFW')'),'color',[0.5 0.5 0.5])
%hold off
%title('FW(dash) FWO(solid)')

figure(5); 
plot(fc,mean(rad2bt(fc,raaKCARTAconv')'),...
     fc,mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'r',...
     fc,std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'k')
title('SuperDuper v2')
