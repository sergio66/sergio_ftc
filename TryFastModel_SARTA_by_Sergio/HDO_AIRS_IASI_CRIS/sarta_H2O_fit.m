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

iChan = 1614; %% 1219 cm-1

thedir = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
for ii = 1 : 49
  fprintf(1,'%2i of 49 \n',ii)
  
  FW        = load([thedir '/FW/convolved_kcarta_FW_'                   num2str(ii) '.mat']);
  F         = load([thedir '/F/convolved_kcarta_F_'                   num2str(ii) '.mat']);  

  if ii == 1
    fc      = FW.fairs;
    secants = FW.secants(1:8);
  end

  qcFW(ii,:,:) = squeeze(FW.rairs_all(1:8,iChan,:));
  qcF(ii,:,:)  = squeeze(F.rairs_all(1:8,iChan,:));
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
Weff      = qcFW./qcF;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ix = 1 : 100
  
  tix   = Weff(:,:,ix+0);
  tixp1 = Weff(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  od0   = od;
  raaOD0(ix,:,:) = od;
end

fitdeltaOD1 = raaOD0(:,1:48,:);

fitT        = tempr(:,1:48,:) - tempr(:,49,:);
fitQ        = gas_1(:,1:48,:)./gas_1(:,49,:);
fitQ        = log10(gas_1(:,1:48,:)) - log10(gas_1(:,49,:));

fitT        = tempr(:,1:48,:);
fitQ        = log10(gas_1(:,1:48,:));
fitQ        = gas_1(:,1:48,:)./gas_1(:,49,:);
fitQ        = log10(gas_1(:,1:48,:)) - log10(gas_1(:,49,:));

fitT        = tempr(:,1:48,:)./tempr(:,49,:);
fitT        = tempr(:,1:48,:)/100;
fitQ        = log10(gas_1(:,1:48,:));
fitS        = secangs(:,1:48,:);

for ii = 1 : 100
  for jj = 1 : 49
    for kk = 1 : 8
     layer_profile_angle(ii,jj,kk) = (jj-1)*100 + ii;
     layer_profile_angleL(ii,jj,kk) = ii;
     layer_profile_angleP(ii,jj,kk) = jj;
     layer_profile_angleA(ii,jj,kk) = kk;
     mmwater_all(ii,jj,kk) = mmw(jj);
     stemp_all(ii,jj,kk)   = p.stemp(jj);     
   end
  end
end  
       
plevs = load('/home/sergio/MATLABCODE/airslevels.dat');
playsN = plevs(1:end-1)-plevs(2:end);
playsD = log(plevs(1:end-1)./plevs(2:end));
plays = playsN./playsD;
plays = [plays; plays(end-1)-2*plays(end)];

abovepred;
%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_the_pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(booT,booOD,'.'); set(gca,'ydir','reverse')

% this is "homemeade" code
% tolerance = 5e-2;
% gkrpred = do_gkr(tolerance/10,matr,booOD,predict);

% this is Matlab toolbox
% see /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/GaussianKernelRegression/clouds_predict.m
% do_matlab_gkr

% loop_do_gkr(matr,booOD);   %% loop over tolerance params
