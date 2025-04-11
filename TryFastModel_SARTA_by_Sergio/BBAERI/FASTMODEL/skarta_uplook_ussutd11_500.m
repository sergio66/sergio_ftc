B = 500;
for ip = 1 : 11
  fin = ['CONV_trans/prof_11us_std_' num2str(ip) '_band_' num2str(B) '.mat'];
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

  for il = 100 : -1 : 1
    tixp1 = Feff(:,il+1) .* Oeff(:,il+1) .* Weff(:,il+1);
    tix   = Feff(:,il-0) .* Oeff(:,il-0) .* Weff(:,il-0);
    t     = tixp1./tix;

    kixp1 = Feff(:,il+1); kix = Feff(:,il); k = kixp1./kix; k = -log(k); kFeff(ip,il,:) = k;
    kixp1 = Weff(:,il+1); kix = Weff(:,il); k = kixp1./kix; k = -log(k); kWeff(ip,il,:) = k;
    kixp1 = Oeff(:,il+1); kix = Oeff(:,il); k = kixp1./kix; k = -log(k); kOeff(ip,il,:) = k;
  end
end

plot(fc,squeeze(kWeff(1,:,:))')
plot(fc,squeeze(kWeff(:,1,:)))

gas_1o = gas_1(6,1:100);
gas_3o = gas_3(6,1:100);
ptempo = ptemp(:,1:100);

gas_1o = fliplr(gas_1o);
gas_3o = fliplr(gas_3o);
ptempo = fliplr(ptempo);

plot(fc,squeeze(kWeff(1,:,:))','b',fc,squeeze(kOeff(1,:,:))','g',...
     fc,squeeze(kFeff(1,:,:))','r')


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
plot(xgas_1./gas_1o,1:100,xgas_3./gas_3o,1:100,'r'); title('(b) wv (r) o3')

figure(2)
plot(xptemp, 1: 100, ptempo(6,:),1:100); title('(b) this prof (r) US Std')

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

  %% this is my 'sKarta"
  temp11 = ptempo(:,il);
  i1 = find(temp11 >= tempx,1);  i1 = min(i1,11); i1 = max(i1,1);
  i0 = i1 - 1;                   
  T1 = temp11(i1);
  T0 = temp11(i0);
  
  od = squeeze(kFeff(:,il,:));
  odF = (tempx-T0)/(T1-T0)* (od(i1,:)-od(i0,:)) + od(i0,:);
  %odF = odF / (gas_1o(il)/xgas_1(il));
  oodF(il,:) = odF;

  od = squeeze(kWeff(:,il,:))/gas_1o(il);
  odW = ((tempx-T0)/(T1-T0)* (od(i1,:)-od(i0,:)) + od(i0,:))*xgas_1(il);
  oodW(il,:) = odW;

  od = squeeze(kOeff(:,il,:))/gas_3o(il);
  odO = ((tempx-T0)/(T1-T0)* (od(i1,:)-od(i0,:)) + od(i0,:))*xgas_3(il);
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

figure(5); plot(fc,rad2bt(fc,rtry),fc,rad2bt(fc,xrtry),'k',fc,rad2bt(fc,rxconv),'r')
figure(6); plot(fc,rad2bt(fc,xrtry)-rad2bt(fc,rxconv),'k',...
                fc,rad2bt(fc,rtry)-rad2bt(fc,rxconv),'r'); 
  title('bias (k) : exact (r) sKarta')
