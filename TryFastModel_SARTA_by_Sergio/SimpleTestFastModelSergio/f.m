function fit = f(secants,secang,pStd,hall,pall,iLay,iChan,data);

%% note : this is for FIXED f

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

effOD = data(:,2:101) ./ data(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
effOD = -log(effOD);
xyzOD = effOD;
effOD = effOD(:,iLay);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% see fastmod.pdf, pg 12

playN = pStd.plevs(1:100) - pStd.plevs(2:101);
playD = log(pStd.plevs(1:100)./pStd.plevs(2:101));
play = playN./playD;

Tref  = pStd.ptemp(1:100) * ones(1,length(pall.stemp));
Wref  = pStd.gas_1(1:100) * ones(1,length(pall.stemp));
Oref  = pStd.gas_3(1:100) * ones(1,length(pall.stemp));
Tprof = pall.ptemp(1:100,:);
Wprof = pall.gas_1(1:100,:);
Oprof = pall.gas_3(1:100,:);
P     = play * ones(1,length(pall.stemp));
Ptoa = 2*P(1)-P(2) * ones(1,length(pall.stemp));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semilogy(xyzOD(1,:),P(:,1),'+-'); set(gca,'ydir','reverse'); xlabel('OD at secang1, prof1'); ylabel('plays'); grid
ax = axis; ax(4) = 1100; axis(ax); title('F')
keyboard
pause(0.1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xTr     = Tprof ./ Tref;
xdeltaT = Tprof - Tref;
xW      = Wprof ./ Wref;
xO      = Oprof ./ Oref;
for ii = 1 : 100
  wah = P(2:ii,:) .* (P(2:ii,:) - P(1:ii-1,:)) .* xTr(1:ii-1,:);
  xTz(ii,:) = sum(wah);
end  

xTr = xTr(iLay,:);           yTr = xTr;
xdeltaT = xdeltaT(iLay,:);   ydeltaT = xdeltaT;
xW = xW(iLay,:);             yW = xW;
xO = xO(iLay,:);             yO = xO;
xTz = xTz(iLay,:);           yTz = xTz;
for ii = 2 : length(secants)
  yTr = [yTr xTr];
  ydeltaT = [ydeltaT xdeltaT];
  yW = [yW xW];
  yO = [yO xO];
  yTz = [yTz xTz];
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
  Tz(ind) = yTz(indy);
end

a = secang;
pred0 = ones(size(a));
pred1 = a;
pred2 = a.^2;
pred3 = a.*Tr;
pred4 = a.*Tr.*Tr;
pred5 = Tr;
pred6 = Tr.*Tr;
pred7 = a.*Tz;
pred8 = a.*Tz./Tr;

figure(1); plot(secang,effOD,'.');      xlabel('secang'); ylabel('breakout effOD');
figure(2); plot(Tr,effOD,'.');          xlabel('Tr'); ylabel('breakout effOD');
figure(3); plot(Tz,effOD,'.');          xlabel('Tz'); ylabel('breakout effOD');

%figure(1); plot(secang);
%figure(2); plot(Tr);                
%figure(3); plot(Tz);
figure(4); plot(effOD,'r');             ylabel('breakout effOD')

matr8 = [pred0; pred1; pred2; pred3; pred4; pred5; pred6; pred7; pred8]'; 

%%%%%%%%%%%%%%%%%%%%%%%%%
matr = matr8;
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

for ii = 1 : 5
  figure(ii); ylabel('effOD'); hl = legend('breakout','predicted','location','best'); set(hl,'fontsize',10); title(['iLay = ' num2str(iLay)]);
end
figure(5); ylabel('effOD-predOD');

pause(0.1);

whos matr data effOD coef tryOD

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% see https://www.mathworks.com/help/stats/gaussian-process-regression-models.html
%% and https://www.mathworks.com/help/stats/fitrgp.html
%% and https://www.mathworks.com/matlabcentral/answers/254981-how-to-use-the-gaussian-process-regression-function-in-matlab-2015b

%% somewhat useful https://www.mathworks.com/matlabcentral/answers/251782-calculating-the-matrix-k-at-test-inputs-after-training-a-gaussian-process-with-fitrgp
%% https://www.mathworks.com/matlabcentral/answers/253605-kernel-custom-problem-firgp

iGKRvers = 1;
iGKRvers = -1;if iGKRvers == 1
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

fit = [];
fit.coef   = coef;
fit.obsOD  = effOD;
fit.predOD = tryOD;
