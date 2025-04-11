B = 500;
wv = [0.1 0.33 0.5 0.8 1.0 3.3 5.0 8.0 10.0];

for ip = 1 : 11
  for jj = 1 : length(wv)  
    fin = ['CONV_trans/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_band_' num2str(B) '.mat'];
    loader = ['load  ' fin]; eval(loader);

    raaKCARTAconv(ip,jj,:) = rxconv;
    raaFastModel0(ip,jj,:)  = qcx;

    Feff = yqcF;
    Weff = yqcFW./yqcF;
    Oeff = yqcFWO./yqcFW;

    gas_1(ip,jj,:) = pN.gas_1;
    gas_3(ip,jj,:) = pN.gas_3;
    ptemp(ip,jj,:) = pN.ptemp;
    stemp(ip,jj)   = pN.stemp;

    for il = 100 : -1 : 1
      tixp1 = Feff(:,il+1) .* Oeff(:,il+1) .* Weff(:,il+1);
      tix   = Feff(:,il-0) .* Oeff(:,il-0) .* Weff(:,il-0);
      t     = tixp1./tix;

      kixp1 = Feff(:,il+1); kix = Feff(:,il); k = kixp1./kix; k = -log(k); kFeff(ip,jj,il,:) = k;
      kixp1 = Weff(:,il+1); kix = Weff(:,il); k = kixp1./kix; k = -log(k); kWeff(ip,jj,il,:) = k;
      kixp1 = Oeff(:,il+1); kix = Oeff(:,il); k = kixp1./kix; k = -log(k); kOeff(ip,jj,il,:) = k;
    end
  end
end

%plot(fc,squeeze(kWeff(1,:,:,:))')
%plot(fc,squeeze(kWeff(:,1,:,:)))

gas_1o = squeeze(gas_1(6,:,1:100));
gas_3o = squeeze(gas_3(6,:,1:100));
ptempo = squeeze(ptemp(:,:,1:100));

gas_1o = fliplr(gas_1o);
gas_3o = fliplr(gas_3o);
ptempo = flipdim(ptempo,3);

plot(fc,squeeze(kWeff(1,5,:,:))','b',fc,squeeze(kOeff(1,5,:,:))','g',...
     fc,squeeze(kFeff(1,5,:,:))','r')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% now do quick radtrans
ip = input('enter REGR profile? ');
fin = ['CONV_trans/prof_' num2str(ip) '_band_' num2str(B) '.mat'];
loader = ['load  ' fin]; eval(loader);

xFeff = yqcF;
xWeff = yqcFW./yqcF;
xOeff = yqcFWO./yqcFW;

xgas_1 = pN.gas_1;
xgas_3 = pN.gas_3;
xptemp = pN.ptemp;
xstemp = pN.stemp;

xgas_1 = xgas_1(1:100);
xgas_3 = xgas_3(1:100);
xptemp = xptemp(1:100);

xgas_1 = flipud(xgas_1)';
xgas_3 = flipud(xgas_3)';
xptemp = flipud(xptemp)';

figure(1)
plot(xgas_1./gas_1o(5,:),1:100,xgas_3./gas_3o(5,:),1:100,'r'); title('(b) wv (r) o3')

figure(2)
plot(xptemp, 1: 100, squeeze(ptempo(6,5,:)),1:100); title('(b) this prof (r) US Std')

ret

emis = 1;
tspace = 2.73;
rtry  = emis * ttorad(fc,tspace);
xrtry = emis * ttorad(fc,tspace);

for il = 100 : -1 : 1
  tempx  = xptemp(il);

  %% these are the "exact"
  kixp1 = xFeff(:,il+1); kix = xFeff(:,il); k = kixp1./kix; k = -log(k); kxFeff(il,:) = k;
  kixp1 = xWeff(:,il+1); kix = xWeff(:,il); k = kixp1./kix; k = -log(k); kxWeff(il,:) = k;
  kixp1 = xOeff(:,il+1); kix = xOeff(:,il); k = kixp1./kix; k = -log(k); kxOeff(il,:) = k;
  odsum = kxFeff(il,:) + kxWeff(il,:) + kxOeff(il,:);
  xrtry = xrtry .* exp(-odsum') + ttorad(fc,tempx) .* (1-exp(-odsum'));

  %% this is my 'sKarta" with wv
  gas10 = gas_1o(5,il) * wv;;
  B = find(gas10 >= xgas_1(il),1);  B = min(B,10); B = max(B,1);
  A = B - 1;                   
  QA = gas10(A);
  QB = gas10(B);
  saveA(il) = A;

  temp11 = squeeze(ptempo(:,5,il));
  i1 = find(temp11 >= tempx,1);  i1 = min(i1,11); i1 = max(i1,1);
  i0 = i1 - 1;                   
  T1 = temp11(i1);
  T0 = temp11(i0);

  odA = squeeze(kFeff(:,A,il,:))/gas_1o(A,il);
  odB = squeeze(kFeff(:,B,il,:))/gas_1o(A,il);
  odFA = (tempx-T0)/(T1-T0)* (odA(i1,:)-odA(i0,:)) + odA(i0,:);
  odFB = (tempx-T0)/(T1-T0)* (odB(i1,:)-odB(i0,:)) + odB(i0,:);
  odF = ((xgas_1(il)-QA)/(QB-QA)*(odFB-odFA) + odFA)*gas_1(il);
  odF = (log(xgas_1(il)/QA)/log(QB/QA)*(odFB-odFA) + odFA)*gas_1(il);
  oodF(il,:) = odF;

  odA = squeeze(kFeff(:,A,il,:));
  odB = squeeze(kFeff(:,B,il,:));
  odFA = (tempx-T0)/(T1-T0)* (odA(i1,:)-odA(i0,:)) + odA(i0,:);
  odFB = (tempx-T0)/(T1-T0)* (odB(i1,:)-odB(i0,:)) + odB(i0,:);
  odF = (xgas_1(il)-QA)/(QB-QA)*(odFB-odFA) + odFA;
  odF = log(xgas_1(il)/QA)/log(QB/QA)*(odFB-odFA) + odFA;
  oodF(il,:) = odF;
  
  odA = squeeze(kWeff(:,A,il,:))/gas_1o(A,il);
  odB = squeeze(kWeff(:,B,il,:))/gas_1o(B,il);
  odFA = (tempx-T0)/(T1-T0)* (odA(i1,:)-odA(i0,:)) + odA(i0,:);
  odFB = (tempx-T0)/(T1-T0)* (odB(i1,:)-odB(i0,:)) + odB(i0,:);
  odW = ((xgas_1(il)-QA)/(QB-QA)*(odFB-odFA) + odFA)*xgas_1(il);
  odW = (log(xgas_1(il)/QA)/log(QB/QA)*(odFB-odFA) + odFA)*xgas_1(il);
  oodW(il,:) = odW;

  od = squeeze(kOeff(:,5,il,:))/gas_3o(5,il);
  odO = ((tempx-T0)/(T1-T0)* (od(i1,:)-od(i0,:)) + od(i0,:))*xgas_3(il);
  oodO(il,:) = odO;

  odA = squeeze(kOeff(:,A,il,:))/gas_3o(A,il);;
  odB = squeeze(kOeff(:,B,il,:))/gas_3o(B,il);;
  odFA = (tempx-T0)/(T1-T0)* (odA(i1,:)-odA(i0,:)) + odA(i0,:);
  odFB = (tempx-T0)/(T1-T0)* (odB(i1,:)-odB(i0,:)) + odB(i0,:);
  odO = ((xgas_1(il)-QA)/(QB-QA)*(odFB-odFA) + odFA)*xgas_3(il);
  oodO(il,:) = odO;

  odsum = odF + odW + odO;

  rtry = rtry .* exp(-odsum') + ttorad(fc,tempx) .* (1-exp(-odsum'));

end
  
figure(5); il = 1:100; plot(fc,oodF(il,:)./kxFeff(il,:),'r')
figure(5); il = 1:100; il = 1:100; semilogy(fc,oodF(il,:),'bo',fc,kxFeff(il,:),'r')

il = 1;
figure(1); semilogy(fc,oodF(il,:),'bo',fc,kxFeff(il,:),'r'); title('F')
figure(2); semilogy(fc,oodW(il,:),'bo',fc,kxWeff(il,:),'r'); title('W')
figure(3); semilogy(fc,oodO(il,:),'bo',fc,kxOeff(il,:),'r'); title('O')
figure(4); plot(fc,oodF(il,:)./kxFeff(il,:),...
                fc,oodW(il,:)./kxWeff(il,:),...
                fc,oodO(il,:)./kxOeff(il,:))
  title('F W O')

il = 1:100;
figure(1); semilogy(fc,oodF(il,:)./kxFeff(il,:),'r'); title('F')
figure(2); semilogy(fc,oodW(il,:)./kxWeff(il,:),'r'); title('W')
figure(3); semilogy(fc,oodO(il,:)./kxOeff(il,:),'r'); title('O')
figure(1); plot(fc,oodF(il,:)./kxFeff(il,:)); title('F'); axis([min(fc) max(fc) -3 +3])
figure(2); plot(fc,oodW(il,:)./kxWeff(il,:)); title('W'); axis([min(fc) max(fc) -3 +3])
figure(3); plot(fc,oodO(il,:)./kxOeff(il,:)); title('O'); axis([min(fc) max(fc) -3 +3])

figure(5); plot(fc,rad2bt(fc,rtry),fc,rad2bt(fc,xrtry),'k',fc,rad2bt(fc,rxconv),'r')
figure(6); plot(fc,rad2bt(fc,xrtry)-rad2bt(fc,rxconv),'ko',...
                fc,rad2bt(fc,rtry)-rad2bt(fc,rxconv),'r'); 
  title('bias (k) : exact (r) sKarta')
