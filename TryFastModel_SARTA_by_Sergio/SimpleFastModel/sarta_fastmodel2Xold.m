%xstartup
addpath /home/sergio/KCARTA/MATLAB
addpath /home/sergio/MATLABCODE/
addpath /asl/matlib/aslutil/

rFWHM = 1;   rDelta = 1;
rFWHM = 0.5; rDelta = 0.5;

iNlay = input('enter number ofl ayers to use : ');

iChunk = input('Enter 705 or 955 or 1255 or 1455 chunk : ');
if iChunk == 705
  [d,w] = readkcstd('data705.dat');
elseif iChunk == 955
  [d,w] = readkcstd('data955.dat');
elseif iChunk == 1255
  [d,w] = readkcstd('data1255.dat');
elseif iChunk == 1455
  [d,w] = readkcstd('data1455.dat');
else
  error('chunk??')
end

[fc,qc] = quickconvolve(w,d,rFWHM,rDelta);
[mm,iRad] = size(d);   %%iRad = row with upwelling radiance

figure(1)
plot(w,rad2bt(w,d(:,iRad)),fc,rad2bt(fc,qc(:,iRad)),'r')

profile = load('/home/sergio/SPECTRA/IPFILES/std_co2');
%stemp = profile(4,4)
stemp = 288.249

emis = 0.85;
emis = 1.0;
rmono = emis*ttorad(w,stemp);
rconvOD = emis*ttorad(fc,stemp);

for ix = 4 : iNlay
  layT = profile(ix,4);

  od = d(:,ix) + d(:,ix+100) + d(:,ix+200) + d(:,ix+300) + d(:,ix+400);
  rmono = rmono.*exp(-od') + ttorad(w,layT).*(1-exp(-od'));
  [fc,junk] = quickconvolve(w,rmono,rFWHM,rDelta);
  raaonoconv(ix,:) = junk;
  
  od = qc(:,ix) + qc(:,ix+100) + qc(:,ix+200) + qc(:,ix+300) + qc(:,ix+400);
  rconvOD = rconvOD.*exp(-od') + ttorad(fc,layT).*(1-exp(-od'));
end

figure(2)
plot(w,rad2bt(w,d(:,iRad)),w,rad2bt(w,rmono),'r','linewidth',2)
hl = legend('kcarta mono','matlab mono','location','best');

[fc,qcx] = quickconvolve(w,rmono,rFWHM,rDelta);

figure(3)
plot(fc,rad2bt(fc,qc(:,iRad)),'bo-',fc,rad2bt(fc,qcx),'k',fc,rad2bt(fc,rconvOD),'r','linewidth',2)
hl = legend('convolved kcarta','convolved matlab mono','HORRIBLE convolved OD','location','best');
%% WOW 'r' fails horribly!
%% while 'b'and 'k on top of each other

ret
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% just to check I have cumsum and flip correct
% ds = [12 11 10; 9 8 7; 6 5 4; 3 2 1];    %% 4 chan x 3 layers
% dF = fliplr(cumsum(fliplr(ds),2))
% whos d ds dF

%%>>>>>>>>>>>>>>>>>>>>>>>
%% correct
F   = d(:,(4:iNlay)+100); % CO2
FW  = d(:,(4:iNlay)+100) + d(:,(4:iNlay)+000) + ...
                  + d(:,(4:iNlay)+300) + d(:,(4:iNlay)+400); % CO2 + WV
FWO = d(:,(4:iNlay)+100) + d(:,(4:iNlay)+000)+ d(:,(4:iNlay)+200) + ...
                  + d(:,(4:iNlay)+300) + d(:,(4:iNlay)+400); % CO2 + WV + O3
%%>>>>>>>>>>>>>>>>>>>>>>>
%% wrong
W = FW-F;   %% WV only
O = FWO-FW; %% O  only
%%>>>>>>>>>>>>>>>>>>>>>>>

%%>>>>>>>>>>>>>>>>>>>>>>>
%% correct
F   = [F   zeros(10000,1)];
FW  = [FW  zeros(10000,1)];
FWO = [FWO zeros(10000,1)];
%%>>>>>>>>>>>>>>>>>>>>>>>
%% wrong
W   = [W   zeros(10000,1)];
O   = [O   zeros(10000,1)];
%%>>>>>>>>>>>>>>>>>>>>>>>

%%>>>>>>>>>>>>>>>>>>>>>>>
%correct
F   = fliplr(cumsum(fliplr(F),2));    F = exp(-F);
FW  = fliplr(cumsum(fliplr(FW),2));   FW = exp(-FW);
FWO = fliplr(cumsum(fliplr(FWO),2));  FWO = exp(-FWO);
%%>>>>>>>>>>>>>>>>>>>>>>>
%wrong
W   = fliplr(cumsum(fliplr(W),2));    W = exp(-W);
O   = fliplr(cumsum(fliplr(O),2));    O = exp(-O);
%%>>>>>>>>>>>>>>>>>>>>>>>

%%>>>>>>>>>>>>>>>>>>>>>>>
%correct
[fc,qcF]   = quickconvolve(w,F,  rFWHM,rDelta);
[fc,qcFW]  = quickconvolve(w,FW, rFWHM,rDelta);
[fc,qcFWO] = quickconvolve(w,FWO,rFWHM,rDelta);
%%>>>>>>>>>>>>>>>>>>>>>>>
% wrong
[fc,qcW]   = quickconvolve(w,W,  rFWHM,rDelta);
[fc,qcO]   = quickconvolve(w,O,  rFWHM,rDelta);
%%>>>>>>>>>>>>>>>>>>>>>>>

figure(1)
plot(qcFWO(1,:),1:98,'bo-',qcFW(1,:),1:98,'g+-',qcF(1,:),1:98,'rs-'); hold on
plot(qcFWO,1:98,'bo-',qcFW,1:98,'g+-',qcF,1:98,'rs-'); hold off
%plot(qcFWO(2,:),1:98,'bo-',qcFW(2,:),1:98,'g+-',qcF(2,:),1:98,'rs-'); grid
  %% these show qcF has largest transmittance, 
  %% good as it has "fewest" contributors
hl = legend('FWO','FW','F'); title('transmittances of FWO FW F')
set(hl,'fontsize',10); grid

Feff = qcF;
Weff = qcFW./qcF;
Oeff = qcFWO./qcFW;

FtryIndOD = qcF;
WtryIndOD = qcW;
OtryIndOD = qcO;

figure(2)
plot(Feff(1,:),1:98,'bo-',Weff(1,:),1:98,'g+-',Oeff(1,:),1:98,'rs-'); 
hold on
plot(Feff,1:98,'bo-',Weff,1:98,'g+-',Oeff,1:98,'rs-'); 
hold off
hl = legend('Feff','Weff','O3eff'); 
title('effective transmittances of F W O3')
set(hl,'fontsize',10); grid

figure(3)
plot(FtryIndOD(1,:),1:98,'bo-',WtryIndOD(1,:),1:98,'g+-',OtryIndOD(1,:),1:98,'rs-'); 
hold on
plot(FtryIndOD,1:98,'bo-',WtryIndOD,1:98,'g+-',OtryIndOD,1:98,'rs-'); 
hold off
hl = legend('FtryIndOD','WtryIndOD','O3tryIndOD'); 
title('tryIndOD effective transmittances of F W O3')
set(hl,'fontsize',10); grid

rxconv = emis*ttorad(fc,stemp);
tryIndODrxconv = emis*ttorad(fc,stemp);

for ix = 1 : 97
  layT = profile(ix+3,4);
  
% this is ok
%  tix   = Feff(:,ix-1) .* Oeff(:,ix-1) .* Weff(:,ix-1);
%  tixp1 = Feff(:,ix+0) .* Oeff(:,ix+0) .* Weff(:,ix+0);

% this is better
  tix   = Feff(:,ix+0) .* Oeff(:,ix+0) .* Weff(:,ix+0);
  tixp1 = Feff(:,ix+1) .* Oeff(:,ix+1) .* Weff(:,ix+1);

  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  rxconv = rxconv.*exp(-od') + ttorad(fc,layT).*(1-exp(-od'));
  raarxconv(ix,:) = rxconv;
  
% this is tryIndOD
  tix   = FtryIndOD(:,ix+0) .* OtryIndOD(:,ix+0) .* WtryIndOD(:,ix+0);
  tixp1 = FtryIndOD(:,ix+1) .* OtryIndOD(:,ix+1) .* WtryIndOD(:,ix+1);  
  t     = tix;
  t     = tix./tixp1;  
  t     = -log(t);
  od    = t;
  tryIndODrxconv = tryIndODrxconv.*exp(-od') + ttorad(fc,layT).*(1-exp(-od'));
  raatryIndODrxconv(ix,:) = tryIndODrxconv;
  
end

figure(4)
plot(fc,rad2bt(fc,qc(:,iRad)),fc,rad2bt(fc,rconvOD),'g',...
     fc,rad2bt(fc,rxconv),'r',w,rad2bt(w,d(:,iRad)),'k',fc,rad2bt(fc,tryIndODrxconv),'c')
hl=legend('convolvedKC','HORRIBLE convolvedOD','CORRECT','kc','TRYINDOD','location','best');
set(hl,'fontsize',10); grid

plot(fc,rad2bt(fc,qc(:,iRad)),fc,rad2bt(fc,rconvOD),'g',fc,rad2bt(fc,rxconv),'r')
plot(fc,rad2bt(fc,qcx),'bo-',fc,rad2bt(fc,rconvOD),'g',fc,rad2bt(fc,rxconv),'r',fc,rad2bt(fc,tryIndODrxconv),'c','linewidth',2)
hl=legend('convolvedKC','HORRIBLE convolvedOD','CORRECT','TRYINDOD','location','best');
set(hl,'fontsize',10); grid

figure(5)
qcx = qcx';
plot(fc,rad2bt(fc,qcx)-rad2bt(fc,rconvOD),'g',...
     fc,rad2bt(fc,qcx)-rad2bt(fc,rxconv),'r',fc,rad2bt(fc,qcx)-rad2bt(fc,tryIndODrxconv),'c','linewidth',2)
hl=legend('HORRIBLE convolvedOD','CORRECT','TRYINDOD','location','best');
ylabel('convolvedKC - X')
set(hl,'fontsize',10); grid

for ix = 4 : iNlay
  figure(6)
  plot(fc,rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raarxconv(ix-3,:)),'rx-',...
       fc,rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raatryIndODrxconv(ix-3,:)),'c','linewidth',2);  
  hl=legend('CORRECT','TRYINDOD','location','best');
  ylabel('convolvedKC - X')
  set(hl,'fontsize',10); grid
  title(num2str(ix));

  meanerr_rx(ix) = nanmean(rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raarxconv(ix-3,:)));
  meanerr_tryIndODrx(ix) = nanmean(rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raatryIndODrxconv(ix-3,:)));  
  stderr_rx(ix) = nanstd(rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raarxconv(ix-3,:)));
  stderr_tryIndODrx(ix) = nanstd(rad2bt(fc,raaonoconv(ix,:))-rad2bt(fc,raatryIndODrxconv(ix-3,:)));  

  pause(0.1);
end
ret
plot(meanerr_rx(4:iNlay),4:iNlay,'rx-',meanerr_tryIndODrx(4:iNlay),4:iNlay,'co-',...
     stderr_rx(4:iNlay),4:iNlay,'m--',stderr_tryIndODrx(4:iNlay),4:iNlay,'g--'); grid     
  hl=legend('meanCORRECT','meanTRYINDOD','stdCORRECT','stdTRYINDOD','location','best');
xlabel('mean BT error between kCARTA and eff SARTA')
ylabel('layer (1=gnd,iNlay=TOA')