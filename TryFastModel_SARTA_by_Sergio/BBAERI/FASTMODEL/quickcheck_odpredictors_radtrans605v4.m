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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iHavePredictors = input('do you have the predictors? (-1/+1) : ');
if iHavePredictors > 0
  load v4Predictors_605
else
  load_in_predictors_v4
end

%%%%%%%%%%%%%%%%%%%%%%%%%
NP = length(stemp);

get_reference_profiles_uplook
pwgt = pwgt_regr49(49,:)' * ones(1,NP);
pwgt = pwgt';

pav = pav_regr49(49,:)' * ones(1,NP);
pav = pav';

%%%%%%%%%%%%%%%%%%%%%%%%%
tic
do_make_predictors_v4

reconstruct_odv4

elapse_time = toc;
fprintf(1,'time to reconstruct per profile = %8.6f \n',elapse_time/NP);

xstartup
iDo = find(iaExist > 0);

radtrans_v4;
elapse_time = toc;

fprintf(1,'time to do radtrans etc per profile = %8.6f \n',elapse_time/NP);
%%%%%%%%%%%%%%%%%%%%%%%%%


plot(fc(iDo),rad2bt(fc(iDo),raaKCARTAconv(:,iDo)'),'b',fc(iDo),rad2bt(fc(iDo),raaTryFWO'),'r',...
     fc(iDo),rad2bt(fc(iDo),raaxTryFWO'),'k')

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
plot(fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),'g',...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'r',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'k',...
     fc(iDo),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaxTryFWO')'),'m',...
     fc(iDo),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaxTryFWO')'),'c')
title('SuperDuper v2')
axis([605 1800 -3 3]); grid on
axis([605 1800 -3 300]); grid on

figure(6); 
plot(mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'b.',...
     mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')'),'r.',...
     mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaxTryFWO')'),'cs',...
     mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')'),std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaxTryFWO')'),'ms')
xlabel('BT'); ylabel('Mean (B/C)  or Std (R/M) bias')
title('Dots : fitting   Squares : reconstruction')

figure(7);
 dbt = -2:0.05:+2;
 nmean = hist(real(mean(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')')),dbt);
 nstd  = hist(real(std(rad2bt(fc(iDo),raaKCARTAconv(:,iDo)')' - rad2bt(fc(iDo),raaTryFWO')')),dbt);
 plot(dbt,nmean,dbt,nstd,'r')