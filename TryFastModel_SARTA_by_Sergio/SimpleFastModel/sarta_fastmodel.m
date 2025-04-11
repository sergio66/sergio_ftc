xstartup

rFWHM = 1;   rDelta = 1;
rFWHM = 0.5; rDelta = 0.5;

iChunk = input('Enter 705 or 955 chunk : ');
if iChunk == 705
  [d,w] = readkcstd('data705.dat');
elseif iChunk == 955
  [d,w] = readkcstd('data955.dat');
end

[fc,qc] = quickconvolve(w,d,rFWHM,rDelta);

figure(1)
plot(w,rad2bt(w,d(:,301)),fc,rad2bt(fc,qc(:,301)),'r')

profile = load('/home/sergio/SPECTRA/IPFILES/std_co2');
%stemp = profile(4,4)
stemp = 288.249

emis = 0.85;
emis = 1.0;
rmono = emis*ttorad(w,stemp);
rconv = emis*ttorad(fc,stemp);

[mm,iRad] = size(d);   %%iRad = row with upwelling radiance

for ix = 4 : 100
  layT = profile(ix,4);

  od = d(:,ix) + d(:,ix+100) + d(:,ix+200) + d(:,ix+300) + d(:,ix+400);
  rmono = rmono.*exp(-od) + ttorad(w,layT).*(1-exp(-od));

  od = qc(:,ix) + qc(:,ix+100) + qc(:,ix+200) + qc(:,ix+300) + qc(:,ix+400);
  rconv = rconv.*exp(-od) + ttorad(fc,layT).*(1-exp(-od));
end

figure(2)
plot(w,rad2bt(w,d(:,iRad)),w,rad2bt(w,rmono),'r')

figure(3)
plot(fc,rad2bt(fc,qc(:,iRad)),fc,rad2bt(fc,rconv),'r') %% WOW fails badly!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% just to check I have cumsum and flip correct
% ds = [12 11 10; 9 8 7; 6 5 4; 3 2 1];    %% 4 chan x 3 layers
% dF = fliplr(cumsum(fliplr(ds),2))
% whos d ds dF

F   = d(:,(1:100)+100); % CO2
FW  = d(:,(1:100)+100) + d(:,(1:100)+000) + ...
                  + d(:,(1:100)+300) + d(:,(1:100)+400); % CO2 + WV
FWO = d(:,(1:100)+100) + d(:,(1:100)+000)+ d(:,(1:100)+200) + ...
                  + d(:,(1:100)+300) + d(:,(1:100)+400); % CO2 + WV + O3

F   = [F   zeros(10000,1)];
FW  = [FW  zeros(10000,1)];
FWO = [FWO zeros(10000,1)];

F   = fliplr(cumsum(fliplr(F),2));    F = exp(-F);
FW  = fliplr(cumsum(fliplr(FW),2));   FW = exp(-FW);
FWO = fliplr(cumsum(fliplr(FWO),2));  FWO = exp(-FWO);

[fc,qcF]   = quickconvolve(w,F,  rFWHM,rDelta);
[fc,qcFW]  = quickconvolve(w,FW, rFWHM,rDelta);
[fc,qcFWO] = quickconvolve(w,FWO,rFWHM,rDelta);

figure(1)
plot(qcFWO,1:101,'bo-',qcFW,1:101,'g+-',qcF,1:101,'rs-'); grid
%plot(qcFWO(2,:),1:101,'bo-',qcFW(2,:),1:101,'g+-',qcF(2,:),1:101,'rs-'); grid
  %% these show qcF has largest transmittance, 
  %% good as it has "fewest" contributors

Feff = qcF;
Weff = qcFW./qcF;
Oeff = qcFWO./qcFW;

figure(2)
plot(Feff,1:101,'bo-',Weff,1:101,'g+-',Oeff,1:101,'rs-'); grid

rxconv = emis*ttorad(fc,stemp);

for ix = 4 : 100
  layT = profile(ix,4);
% this is better
  tix   = Feff(:,ix+0) .* Oeff(:,ix+0) .* Weff(:,ix+0);
  tixp1 = Feff(:,ix+1) .* Oeff(:,ix+1) .* Weff(:,ix+1);
% this is ok
%  tix   = Feff(:,ix-1) .* Oeff(:,ix-1) .* Weff(:,ix-1);
%  tixp1 = Feff(:,ix+0) .* Oeff(:,ix+0) .* Weff(:,ix+0);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  rxconv = rxconv.*exp(-od) + ttorad(fc,layT).*(1-exp(-od));
end

figure(3)
plot(fc,rad2bt(fc,qc(:,iRad)),fc,rad2bt(fc,rconv),'g',...
     fc,rad2bt(fc,rxconv),'r',w,rad2bt(w,d(:,iRad)),'k')
hl=legend('convolvedKC','WRONG','CORRECT','kc');
set(hl,'fontsize',10); grid

plot(fc,rad2bt(fc,qc(:,iRad)),'bo-',fc,rad2bt(fc,rconv),'g',...
     fc,rad2bt(fc,rxconv),'r')
hl=legend('convolvedKC','WRONG','CORRECT');
set(hl,'fontsize',10); grid

