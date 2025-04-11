function fit = fwxdyo(secants,secang,pStd,hall,pall,iLay,iChan,data,data2);

%% note : this is really for D as it assumes you are sending in data,data2 to do deltaD
%% data  = FWOeff/FWxDyO_0p1 = (F)(W'+0.9D+0.1D)(O)/(F)(W'+0.9D)(O) = 0.1D
%% data2 = FWOeff 

for ii = 1 : 7; figure(ii); colormap jet; clf; end

% input
%   secants = 1x14 = the 14 secants used in the breakout
%   secang = 1 x (numprofx14) = repeating the 14 secants Nprof times
%   pStd   = US std structure
%   hall,pall = structures with the profiles used in making "data"
%   iLay      = which layer are we fitting for
%   iChan     = which channel are we fitting for
%   data      = [(numprofx14) x 101] set of layer2space transmittances

junk = pall.ptemp(iLay,:);
ptemp = junk;
for ii = 2 : length(secants)
  ptemp = [ptemp junk];
end

effOD1 = data(:,2:101) ./ data(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
effOD1 = -log(effOD1);
xyzOD1 = effOD1;
effOD1 = effOD1(:,iLay);

effOD2 = data2(:,2:101) ./ data2(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
effOD2 = -log(effOD2);
xyzOD2 = effOD2;
effOD2 = effOD2(:,iLay);

%xyzOD = xyzOD2 - xyzOD1;
%effOD = effOD2-effOD1;  %% remember data (effOD) is 0.1 HDO while data2 (effOD2) is (F)(W'+0.9D+0.1D)(O) = everything, so this is
%                        %% basically 0.9 HDO and so it is silly to do

xyzOD = xyzOD1;
effOD = effOD1;         %% remember data (effOD) is 0.1 HDO while data2 (effOD2) is (F)(W'+0.9D+0.1D)(O) = eveything

figure(1); plot(effOD(:)); 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% see fastmod.pdf, pg 12

playN = pStd.plevs(1:100) - pStd.plevs(2:101);
playD = log(pStd.plevs(1:100)./pStd.plevs(2:101));
play = playN./playD;

%% these are 100 x 49
Tref  = pStd.ptemp(1:100) * ones(1,length(pall.stemp));
Wref  = pStd.gas_1(1:100) * ones(1,length(pall.stemp));
Oref  = pStd.gas_3(1:100) * ones(1,length(pall.stemp));
Mref  = pStd.gas_6(1:100) * ones(1,length(pall.stemp));
Tprof = pall.ptemp(1:100,:);
Wprof = pall.gas_1(1:100,:);
Oprof = pall.gas_3(1:100,:);
Mprof = pall.gas_6(1:100,:);
P     = play * ones(1,length(pall.stemp))/1013.25;    %% change mb --> atmospheres
Ptoa = (2*P(1)-P(2)) * ones(1,length(pall.stemp));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semilogy(xyzOD1(1,:),P(:,1),'b+-',xyzOD2(1,:),P(:,1),'r+-'); hold on
semilogy(xyzOD(1,:),P(:,1),'ko-','linewidth',2); hold off
set(gca,'ydir','reverse'); xlabel('OD at secang1, prof1'); ylabel('plays'); grid
ax = axis; ax(4) = 1100; axis(ax); title('D comes from FW,FWDx')
hl = legend('depl FW','all FW','D','location','best'); set(hl,'fontsize',10);
%keyboard

pause(0.1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% these are 100 x 49
xTr     = Tprof ./ Tref;
xdeltaT = Tprof - Tref;
xW      = Wprof ./ Wref;
xO      = Oprof ./ Oref;
xM      = Mprof ./Mref;

%% these are 100 x 49
for ii = 1 : 100
  wah = P(2:ii,:) .* (P(2:ii,:) - P(1:ii-1,:)) .* xTr(1:ii-1,:);
  xTz(ii,:) = sum(wah);
  wah1 = P(2:ii,:) .* (P(2:ii,:) - P(1:ii-1,:)) .* Wprof(1:ii-1,:);
  wah2 = P(2:ii,:) .* (P(2:ii,:) - P(1:ii-1,:)) .* Wref(1:ii-1,:);  
  xWz(ii,:) = sum(wah1)./(sum(wah2)+eps);
end  

xTr = xTr(iLay,:);           yTr = xTr;
xdeltaT = xdeltaT(iLay,:);   ydeltaT = xdeltaT;
xW = xW(iLay,:);             yW = xW;
xO = xO(iLay,:);             yO = xO;
xM = xM(iLay,:);             yM = xM;
xTz = xTz(iLay,:);           yTz = xTz;
xWz = xWz(iLay,:);           yWz = xWz;
%xPzop = xPzop(iLay,:);       yPzop = xPzop;
%xTzop = xTzop(iLay,:);       yTzop = xTzop;

for ii = 2 : length(secants)
  yTr = [yTr xTr];
  ydeltaT = [ydeltaT xdeltaT];
  yW = [yW xW];
  yO = [yO xO];
  yM = [yM xM];
  yTz = [yTz xTz];
  yWz = [yWz xWz];
end

addpath /home/sergio/MATLABCODE
%keyboard_nowindow
for ii = 1 : length(pall.stemp)
  indy = 1:length(pall.stemp):length(secants)*length(pall.stemp);
  indy = indy + (ii-1);
  ind  = (1:length(secants)) + (ii-1)*length(secants);
  
  Tr(ind) = yTr(indy);
  deltaT(ind) = ydeltaT(indy);
  W(ind) = yW(indy);
  O(ind) = yO(indy);
  M(ind) = yM(ind);
  Tz(ind) = yTz(indy);
  Wz(ind) = yWz(indy);
  
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OPTRAN
%% see ~/FastModelDevelopment/ScottCode/Fit_ftc/Src_fitftc/calptz_optran.f
%% see ~/FastModelDevelopment/ScottCode/Fit_ftc/Src_fitftc/fitftc_optran.f
%% see ~/FastModelDevelopment/ScottCode/Fit_ftc/Src_fitftc/interp_optran.f
%% look at ../OPTRAN_APPLIED_OPTICS_1_7/optran5_ao1995.pdf

a = secang;
ix = iLay;
xPop = P(ix,:);         yPop = xPop;
xTop = Tprof(ix,:);     yTop = xTop;
for ii = 2 : length(secants)
  yPop = [yPop xPop];
  yTop = [yTop xTop];
end

yPzop = [];
yTzop = [];
for ii = 1 : length(secants)
  wah1 = secants(ii)*P(1:ix,:).*Wprof(1:ix,:);
  wah2 = secants(ii)*Wprof(1:ix,:);
  if ix == 1
    wah = wah1./(wah2 + eps);
  else
    wah = sum(wah1)./(sum(wah2)+eps);
  end
  yPzop = [yPzop wah];

  wah1 = secants(ii)*Tprof(1:ix,:).*Wprof(1:ix,:);
  wah2 = secants(ii)*Wprof(1:ix,:);
  if ix == 1
    wah = wah1./(wah2 + eps);
  else
    wah = sum(wah1)./(sum(wah2)+eps);
  end  
  yTzop = [yTzop wah];
end

Pop = yPop;
Top = yTop;
Pzop = yPzop;
Tzop = yTzop;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a = secang;
pred0 = ones(size(a));
pred1 = a.*W;
pred2 = sqrt(pred1);
pred3 = a.*W.*W./(Wz+eps);
pred4 = a.*W.*deltaT;
pred5 = (a.*W).^2;
pred6 = sqrt(a.*W).*deltaT;
pred7 = (a.*W).^(0.25);
pred8 = sqrt(a.*W).*W ./(Wz+eps);
pred9 = (a.*W).^3;
pred10 = W;
pred11 = (a.*W).*deltaT.*abs(deltaT);

pred12 = a.*a.*W;
pred13 = a.*W.*a.*deltaT;
pred14 = (a.*W).^(7/4);
pred15 = (a.*W.*W);

pred16 = a;
pred17 = a.^2;
pred18 = a.*Tr;
pred19 = a.*Tr.*Tr;
pred20 = Tr;
pred21 = Tr.*Tr;
pred22 = a.*Tz;
pred23 = a.*Tz./Tr;

pred24 = a.*M;
pred25 = sqrt(pred24);
pred26 = pred24.^2;
pred27 = a.*M.*deltaT;

%% OPTRAN
aop = secang;
optran1 = ones(size(aop));
optran2 = Pop;
optran3 = Top;
optran4 = sqrt(Pop);
optran5 = Top.*Top;
optran6 = Pop.*Top;
optran7 = aop;
optran8 = Pzop;
optran9 = Tzop;

figure(1); plot(secang,effOD,'.');      xlabel('secang'); ylabel('breakout effOD');
figure(2); plot(Tr,effOD,'.');          xlabel('Tr'); ylabel('breakout effOD');
figure(3); plot(Tz,effOD,'.');          xlabel('Tz'); ylabel('breakout effOD');

%figure(1); plot(secang);
%figure(2); plot(Tr);                
%figure(3); plot(Tz);
figure(4); plot(effOD,'r');             ylabel('breakout effOD')

matr8  = [pred0; pred1; pred2; pred3; pred4; pred5; pred6; pred7; pred8]';
matr11 = [pred0; pred1; pred2; pred3; pred4; pred5; pred6; pred7; pred8; pred9; pred10; pred11]';
matr15 = [pred0; pred1; pred2; pred3; pred4; pred5; pred6; pred7; pred8; pred9; pred10; pred11; pred12; pred13; pred14; pred15]';
matr23 = [matr15'; pred16; pred17; pred18; pred19; pred20; pred21; pred22; pred23]'; 

matrOPTRAN = [optran1; optran2; optran3; optran4; optran5; optran6; optran7; optran8; optran9]';

matrEureqa1 = [pred0; pred2; pred4; pred11; pred12]';
matrEureqa2 = [pred0; pred1; pred3; pred9; pred21; pred21.^2; pred21.^3]';

matr11_M = [pred0; pred1; pred2; pred3; pred4; pred5; pred6; pred7; pred8; pred9; pred10; pred11; pred24; pred25; pred26; pred27]';

oo1 = find(secang == 3.47); 
boo = find(secang == 1);
whos oo1 boo
predx = pred1; plot(predx,effOD,'.',predx(oo1),effOD(oo1),'ro'); grid
predx = optran9; plot(predx,effOD,'.',predx(oo1),effOD(oo1),'ro',predx(boo),effOD(boo),'gx'); grid
%keyboard

%%%%%%%%%%%%%%%%%%%%%%%%%
matr = matr8;
matr = matr11;
matr = matr15;
matr = matr23;
matr = matrOPTRAN;
matr = matrEureqa2;
matr11 = matr11_M;

%%%%%%%%%%%%%%%%%%%%%%%%%

coef = matr \ effOD;

tryOD = matr * coef;

rearrange0 = 0:14:14*48;
rearrange = [];
for ii = 1 : length(secants)
  rearrange = [rearrange rearrange0+ii];
end

figure(1); plot(secang(rearrange),effOD(rearrange),'b.',secang(rearrange),tryOD(rearrange),'r.');           xlabel('secang');
figure(2); plot(Tr(rearrange),effOD(rearrange),'b.',Tr(rearrange),tryOD(rearrange),'r.');                   xlabel('Tr');
figure(3); plot(Tz(rearrange),effOD(rearrange),'b.',Tz(rearrange),tryOD(rearrange),'r.');                   xlabel('Tz');
figure(4); plot(1:length(effOD),effOD(rearrange),'b',1:length(effOD),tryOD(rearrange),'r'); xlabel('index number')  

therror = effOD-tryOD;
pcerror = (effOD-tryOD)./effOD*100;
figure(5); plot(effOD,therror,'.');  xlabel('effOD');

figure(6); plot(effOD(rearrange),tryOD(rearrange),'bx',effOD(rearrange),effOD(rearrange),'k')
  xlabel('input effOD'); ylabel('reconstructed effOD')
  title(['iLay = ' num2str(iLay)]);

figure(7); plot(exp(-effOD(rearrange)),exp(-tryOD(rearrange)),'bx',exp(-effOD(rearrange)),exp(-effOD(rearrange)),'k')
  xlabel('input eff tau'); ylabel('reconstructed eff tau')
  title(['iLay = ' num2str(iLay)]);


for ii = 1 : 5
  figure(ii); ylabel('effOD'); hl = legend('breakout','predicted','location','best'); set(hl,'fontsize',10); title(['iLay = ' num2str(iLay)]);
end
figure(5); ylabel('effOD-predOD');

%disp('ret to go to wgt'); pause;
pause(0.1);

whos matr data effOD coef tryOD

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iLogOrNorm = -1;
hingeODs = [0.0553 0.190 0.385 0.540 1.000 1.650 2.900 3.490 4.410];
if iLogOrNorm > 1
  hingeODs = log10(hingeODs);
end
weights  = [1      3     5     6     7     6     3     2     1];

wgtOD = interp1(hingeODs,weights,effOD,[],'extrap');
oo = find(effOD < hingeODs(1));    wgtOD(oo) = 1;
oo = find(effOD > hingeODs(end));  wgtOD(oo) = 1;
wgtmatr = diag(wgtOD);

coefW = (wgtmatr * matr) \ (wgtmatr * effOD);

%tryOD = matr * coefW;
%therror = effOD-tryOD;

effODW = wgtmatr * effOD;
tryODW = wgtmatr * matr * coefW;
therror = effODW - tryODW;

rearrange0 = 0:14:14*48;
rearrange = [];
for ii = 1 : length(secants)
  rearrange = [rearrange rearrange0+ii];
end

figure(1); plot(secang(rearrange),effODW(rearrange),'b.',secang(rearrange),tryODW(rearrange),'r.');           xlabel('secang');
figure(2); plot(Tr(rearrange),effODW(rearrange),'b.',Tr(rearrange),tryODW(rearrange),'r.');                   xlabel('Tr');
figure(3); plot(Tz(rearrange),effODW(rearrange),'b.',Tz(rearrange),tryODW(rearrange),'r.');                   xlabel('Tz');
figure(4); plot(1:length(effODW),effODW(rearrange),'b',1:length(effOD),tryODW(rearrange),'r'); xlabel('index number')  

therror = effODW-tryODW;
pcerror = (effODW-tryODW)./effODW*100;
figure(5); plot(effODW,therror,'.');  xlabel('effODW');

figure(6); plot(effODW(rearrange),tryODW(rearrange),'bx',effODW(rearrange),effODW(rearrange),'k')
  xlabel('input effOD'); ylabel('reconstructed effOD')
  %hl = legend('breakout','predicted','location','best'); set(hl,'fontsize',10);

for ii = 1 : 5
  figure(ii); ylabel('effOD');   title(['iLay = ' num2str(iLay)]);
end
figure(5); ylabel('effOD-predOD');

%disp('ret to continue'); pause
pause(0.1);

whos matr data effOD coef tryODW


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
M = [matr effOD];

M = [matr effOD];
M = [matr23 matrOPTRAN effOD];

csvwrite('hdo_csv.txt',M);

run eureqa : "import" data from this directory
odeureka = f(b, c, d, e, f, g, h)
odeureka = 18.99*c + 0.1315*a + -0.3194./a + -2.93*c./b + -23.68;
odeureka = 18.99*M(:,3) + 0.1315*M(:,1) + -0.3194./M(:,1) + -2.93*M(:,3)./M(:,2) + -23.68;
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% see https://www.mathworks.com/help/stats/gaussian-process-regression-models.html
%% and https://www.mathworks.com/help/stats/fitrgp.html
%% and https://www.mathworks.com/matlabcentral/answers/254981-how-to-use-the-gaussian-process-regression-function-in-matlab-2015b

%% somewhat useful https://www.mathworks.com/matlabcentral/answers/251782-calculating-the-matrix-k-at-test-inputs-after-training-a-gaussian-process-with-fitrgp
%% https://www.mathworks.com/matlabcentral/answers/253605-kernel-custom-problem-firgp

%iGKRvers = 2;
%iGKRvers = 3;
iGKRvers = -1;
iGKRvers = 1;

if iGKRvers > 0
  [wahMrows,wahMcolumns] = find(isnan(matr) | isinf(matr));
  wahD = find(isnan(effOD) | isinf(effOD));
  if length(wahMrows) > 0
    wahMrows = unique(wahMrows);
    wahMcolumns = unique(wahMcolumns);
  end
end

if iGKRvers == 1
  gprMdl = fitrgp(matr,effOD);
  %ypred = resubPredict(gprMdl);
elseif iGKRvers == 2
  sigma = 0.5;
  sigma = 0.2;
  gprMdl = fitrgp(matr,effOD,'KernelFunction','squaredexponential','sigma',sigma,'verbose',1);   %% had set sigma as 0.2 originally
elseif iGKRvers == 3
  tbl = [matr effOD];
  tbl.Properties.VariableNames = {'ones','pred1','pred2','pred3','pred4','pred5','pred6','pred7','pred8','effOD'};
  tbl(1:3,1:10)
  whos tbl effOD
  gprMdl = fitrgp(tbl,'effOD','KernelFunction','squaredexponential',...
                  'FitMethod','sr','PredictMethod','fic','Standardize',1)
end

iGKRvers

if iGKRvers > 0
  [ypred,~,yerr_est] = predict(gprMdl,matr);
  [zpred,~,zerr_est] = predict(gprMdl,matr+randn(size(matr)));

  gprMdl

  figure(6);
  plot(effOD,'r.');
  hold on
  plot(ypred,'b.');
  plot(tryOD,'k.')'
  plot(yerr_est(:,1),'c-')
  plot(yerr_est(:,2),'c-')
  plot(zpred,'g-');
  xlabel('x');
  ylabel('y');
  legend({'data','GPR','regular regr'},'Location','Best');
  axis([0 length(effOD) 0 max(effOD)*2]);
  hold off;

  figure(6);
  plot(effOD,ypred,'r.',effOD,tryOD,'k.');
  xlabel('effOD');
  ylabel('predOD');
  legend({'GKR','regular regr'},'Location','Best');
  line([min(effOD) max(effOD)],[min(effOD) max(effOD)],'color',[0.2 0.2 0.2])
  
  figure(7);
  dx = -max(effOD)*2 : 0.01 : +max(effOD)*2;
  dx = linspace(-max(effOD)*2,+max(effOD)*2,100);
  plot(dx,histc(effOD-ypred,dx),'r',dx,histc(effOD-tryOD,dx),'k');
  legend({'GPR','regular regr'},'Location','Best');

  figure(8);
  [Y,I] = sort(effOD);
  plot(1:length(effOD),effOD(I),'r',1:length(effOD),ypred(I),'b',1:length(effOD),tryOD(I),'k',...
       1:length(effOD),yerr_est(I,1),'c',1:length(effOD),yerr_est(I,2),'c','linewidth',2);
  xlabel('x');
  ylabel('y');
  legend({'data','GPR','regular regr'},'Location','Best');
  axis([0 length(effOD) 0 max(effOD)*2]);
  title('Data and fits (cyan = 95pc intervals)')  
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iSergioGKR = -1;
iSergioGKR = +1;
if iSergioGKR > 0
  disp('doing Sergio GKR')
  addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/HDO_AIRS_IASI_CRIS
  gkrpred = loop_do_gkr(matr,effOD,10.^[-4:4]);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fit = [];
fit.coef   = coef;
fit.obsOD  = effOD;
fit.predOD = tryOD;