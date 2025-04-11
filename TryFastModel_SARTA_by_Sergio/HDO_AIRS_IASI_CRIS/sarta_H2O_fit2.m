%xstartup
addpath /home/sergio/KCARTA/MATLAB
addpath /home/sergio/MATLABCODE/
addpath /asl/matlib/aslutil/
addpath /asl/matlib/h4tools
addpath /asl/matlib/rtptools

%% this is BAREBONES, what ya need, dude!!!!!
%% this is BAREBONES, what ya need, dude!!!!!
%% this is BAREBONES, what ya need, dude!!!!!

qcFWO = [];
qcFWxDyO_0p1 = [];

iChan = 445;  %% 791  cm-1
iChan = 1614; %% 1219 cm-1

angles = 1 : 8;
angles = 1 : 14;

thedir = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
for ii = 1 : 49
  fprintf(1,'%2i of 49 \n',ii)
  
  FW        = load([thedir '/FW/convolved_kcarta_FW_'                   num2str(ii) '.mat']);
  F         = load([thedir '/F/convolved_kcarta_F_'                   num2str(ii) '.mat']);  

  if ii == 1
    fc      = FW.fairs;
    secants = FW.secants(angles);
  end

  qcFW(ii,:,:) = squeeze(FW.rairs_all(angles,iChan,:));
  qcF(ii,:,:)  = squeeze(F.rairs_all(angles,iChan,:));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_this_rtp  = 'regr49_1100_400ppm.op.rtp';  %% spres = 1100 mb  %%% >>> use for sea emis rad calcs only
use_this_rtpx = 'regr49_1013_400ppm.op.rtp';  %% spres = 1013 mb  %%% >>> use for  sea emis rad calcs only
[h,ha,p,pa] = rtpread([thedir use_this_rtp]);
mmw = mmwater_rtp(h,p);

x = [1 2 3; 4 5 6]; y = repmat(x,[1 1 3])

gas_1 = flipud(p.gas_1); gas_1 = gas_1(2:101,:); gas_1 = repmat(gas_1,[1 1 8]);
tempr = flipud(p.ptemp); tempr = tempr(2:101,:); tempr = repmat(tempr,[1 1 8]);
secangs = ones(size(gas_1));
for ii = 1 : 8
  secangs(:,:,ii) = secangs(:,:,ii) * secants(ii);
end  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FWeff     = qcFW;
Feff      = qcF;
Weff      = qcFW./qcF;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ix = 1 : 100
  
  tix   = Weff(:,:,ix+0);
  tixp1 = Weff(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  od0   = od;
  raaODW(ix,:,:) = od;

  tix   = Feff(:,:,ix+0);
  tixp1 = Feff(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  od0   = od;
  raaODF(ix,:,:) = od;

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[predsets,pred] = make_sarta_line_predictors(angles);

fitdeltaOD1 = raaODW(:,1:48,:);
do_the_predW2

fitdeltaOD1 = raaODF(:,1:48,:);
do_the_predF2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
M = [booS booT booQ abooT abooQ mmx st boo1 booOD];
csvwrite('hdo_csv.txt',M);

Merror = find(abs(pcerror) > 10); Merror = M(Merror,:);
csvwrite('hdo_csv2.txt',Merror);

run eureqa : load the data in yada
odeureka = f(b, c, d, e, f, g, h)
odeureka = 18.99*c + 0.1315*a + -0.3194./a + -2.93*c./b + -23.68;
odeureka = 18.99*M(:,3) + 0.1315*M(:,1) + -0.3194./M(:,1) + -2.93*M(:,3)./M(:,2) + -23.68;
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(booT,booOD,'.'); set(gca,'ydir','reverse')

% this is "homemeade" code
% tolerance = 5e-2;
% gkrpred = do_gkr(tolerance/10,matr,booOD,predict);

% this is Matlab toolbox
% see /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/GaussianKernelRegression/clouds_predict.m
% do_matlab_gkr

% loop_do_gkr(matr,booOD);   %% loop over tolerance params
