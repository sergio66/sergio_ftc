%% copied from /strowdata1/shared/sergio/MATLABCODE/BroadBand_CKD_SARTA/

xstartup

rFWHM = 1;   rDelta = 1;
rFWHM = 0.5; rDelta = 0.5;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iChunk = input('Enter 500 or 605 chunk : ');
if iChunk == 500
  B = 500;
else
  B = 605;
end

wv = [0.1 0.33 0.5 0.8 1.0 3.3 5.0 8.0 10.0];

for jj = 1 : length(wv)

  [h,ha,p,pa] = rtpread(['CONV_trans/regr_usstd_11offsets_' num2str(wv(jj)) '.op.rtp']);
  for ip = 1 : 11

    [hN,pN] = subset_rtp(h,p,[],[],ip);

    fnameIN = ['/asl/s1/sergio/BBAERI/WV/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_B_' num2str(B) '.dat'];
    [d1,w] = readkcstd(fnameIN);

    fnameIN = ['/asl/s1/sergio/BBAERI/CO2/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_B_' num2str(B) '.dat'];
    [d2,w] = readkcstd(fnameIN);

    fnameIN = ['/asl/s1/sergio/BBAERI/O3/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_B_' num2str(B) '.dat'];
    [d3,w] = readkcstd(fnameIN);

    fnameIN = ['/asl/s1/sergio/BBAERI/OTHERS/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_B_' num2str(B) '.dat'];
    [d4,w] = readkcstd(fnameIN);

    stemp = p.stemp(ip);
    ptemp = flipud(p.ptemp(1:p.nlevs(ip)-1,ip));
    tspace = 2.73;
    emis = 1.0;
    
    rmono   = emis*ttorad(w,tspace);
    rmono60 = emis*ttorad(w,tspace);
    for ix = length(ptemp) : -1 : 1
      layT = ptemp(ix);
      od = d1(:,ix) + d2(:,ix) + d3(:,ix) + d4(:,ix);
      rmono = rmono.*exp(-od) + ttorad(w,layT).*(1-exp(-od));

      od = od/cos(60*pi/180);
      rmono60 = rmono60.*exp(-od) + ttorad(w,layT).*(1-exp(-od));
    end

    [fc,qcx]   = quickconvolve(w,rmono,rFWHM,rDelta);
    [fc,qcx60] = quickconvolve(w,rmono60,rFWHM,rDelta);

    figure(3)
    plot(w,rmono,fc,qcx,'r',fc,qcx60,'k') %% 

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % just to check I have cumsum and flip correct
    % ds = [12 11 10; 9 8 7; 6 5 4; 3 2 1];    %% 4 chan x 3 layers
    % dF = cumsum(ds,2)                        %% this is now space to layer!
    % whos d ds dF

    clear F FW FWO
    F   = d4 + d2;           % CO2 + others
    FW  = d4 + d2 + d1;      % CO2 + WV + others
    FWO = d4 + d3 + d2 + d1; % CO2 + WV + O3 + others

    F   = [zeros(length(w),1) F];
    FW  = [zeros(length(w),1) FW];
    FWO = [zeros(length(w),1) FWO];

    yF   = cumsum(F,  2);  yF = exp(-yF);
    yFW  = cumsum(FW, 2);  yFW = exp(-yFW);
    yFWO = cumsum(FWO,2);  yFWO = exp(-yFWO);

    [fc,yqcF]   = quickconvolve(w,yF,  rFWHM,rDelta);
    [fc,yqcFW]  = quickconvolve(w,yFW, rFWHM,rDelta);
    [fc,yqcFWO] = quickconvolve(w,yFWO,rFWHM,rDelta);

    [mm,nn] = size(yqcFWO);

    % figure(1)
    % plot(yqcFWO(1,:),1:nn,'bo-',yqcFW(1,:),1:nn,'g+-',yqcF(1,:),1:nn,'rs-'); hold on
    % plot(yqcFWO,1:nn,'bo-',yqcFW,1:nn,'g+-',yqcF,1:nn,'rs-'); hold off
    % %plot(qcFWO(2,:),1:nn,'bo-',qcFW(2,:),1:nn,'g+-',qcF(2,:),1:nn,'rs-'); grid
    %   %% these show qcF has largest transmittance,
    %   %% good as it has "fewest" contributors
    % hl = legend('FWO','FW','F'); title('transmittances of FWO FW F')
    % set(hl,'fontsize',10); grid

    Feff = yqcF;
    Weff = yqcFW./yqcF;
    Oeff = yqcFWO./yqcFW;

    figure(2)
    if B == 605
      i1020 = find(fc >= 1020,1);
      i1420 = find(fc >= 1720,1);
      i0720 = find(fc >= 0720,1);
    else
      i1020 = find(fc >= 531,1);
      i1420 = find(fc >= 536,1);
      i0720 = find(fc >= 600,1);
    end
   ind = [i0720 i1020 i1420];
    plot(Feff(ind,:),1:nn,'bo-',Weff(ind,:),1:nn,'g+-',Oeff(ind,:),1:nn,'rs-');   
    hl = legend('Feff','Weff','O3eff'); 
    title('effective transmittances of F W O3')
    set(hl,'fontsize',10); grid

    rxconv   = emis*ttorad(fc,tspace);
    rxconv60 = emis*ttorad(fc,tspace);

    for ix = length(ptemp) : -1 : 1
      layT = ptemp(ix);

      % this is better, note the offsets!!!
      tixp1 = Feff(:,ix+1) .* Oeff(:,ix+1) .* Weff(:,ix+1);
      tix   = Feff(:,ix-0) .* Oeff(:,ix-0) .* Weff(:,ix-0);

      t     = tixp1./tix;
      t     = -log(t);
      od    = t;
      rxconv = rxconv.*exp(-od) + ttorad(fc,layT).*(1-exp(-od));

      %plot(w,-log(FWO(:,ix)),fc,od); title(num2str(ix));
      if ix == 1
        plot(w,exp(-FWO(:,ix)),fc,exp(-od)); title(num2str(ix));
        ret(0.1)
      end

      od    = t/cos(60*pi/180);
      rxconv60 = rxconv60.*exp(-od) + ttorad(fc,layT).*(1-exp(-od));
    end

    figure(3)
    plot(fc,rad2bt(fc,qcx),'bo-',fc,rad2bt(fc,rxconv),'r',...
         fc,rad2bt(fc,qcx60),'co-',fc,rad2bt(fc,rxconv60),'m')
    hl=legend('convolvedKC','CORRECT');
    set(hl,'fontsize',10); grid

    figure(4)
    plot(fc,rad2bt(fc,qcx)-rad2bt(fc,rxconv),'r',...
         fc,rad2bt(fc,qcx60)-rad2bt(fc,rxconv60),'m')
    title('FastModel-conv(kCARTA)')
    pause(0.1)

    fout = ['CONV_trans/prof_11us_std_' num2str(ip) '_wv_' num2str(wv(jj)) '_band_' num2str(B) '.mat'];
    %saver = ['save ' fout ' F FW FWO ptemp pN'];
    saver = ['save ' fout ' fc yqcF yqcFW yqcFWO pN qcx rxconv qcx60 rxconv60'];
    eval(saver)
  end
end
