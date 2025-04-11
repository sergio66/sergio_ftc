
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

figure(6); 
plot(mean(rad2bt(fc,raaKCARTAconv')'),mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'b.',...
     mean(rad2bt(fc,raaKCARTAconv')'),std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')'),'r.')
xlabel('BT'); ylabel('Mean or Std bias')

figure(7);
 dbt = -10:0.1:10;
 nmean = hist(real(mean(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')')),dbt);
 nstd  = hist(real(std(rad2bt(fc,raaKCARTAconv')' - rad2bt(fc,raaTryFWO')')),dbt);
 plot(dbt,nmean,dbt,nstd,'r')