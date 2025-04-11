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
qcFWxDyO_0p6 = [];
qcFWxDyO_0p3 = [];
qcFWxDyO_0p1 = [];

iChan = 2793; %% from sarta_HDO_test_linear.m     i2722 = find(fc >= 2722,1)
iChan = 1334; %% from sarta_HDO_test_linear.m     i1254 = find(fc >= 1254,1);
iChan = 2637; %% from sarta_HDO_test_linear.m     i281 = find(min(abs(fc-1280.5))

angles = 1 : 8;
angles = 1 : 14;

thedir = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
for ii = 1 : 49
  fprintf(1,'%2i of 49 \n',ii)
  
  FWO        = load([thedir '/FWO/convolved_kcarta_FWO_'                   num2str(ii) '.mat']);
  FWxDyO_0p1 = load([thedir '/FWxDyO_depleted0.1/convolved_kcarta_FWxDyO_' num2str(ii) '.mat']);
  FWxDyO_0p3 = load([thedir '/FWxDyO_depleted0.3/convolved_kcarta_FWxDyO_' num2str(ii) '.mat']);
  FWxDyO_0p6 = load([thedir '/FWxDyO_depleted0.6/convolved_kcarta_FWxDyO_' num2str(ii) '.mat']);

  if ii == 1
    fc      = FWO.fairs;
    secants = FWO.secants(angles);
  end
  
  qcFWO(ii,:,:)        = squeeze(FWO.rairs_all(angles,iChan,:));
  qcFWxDyO_0p6(ii,:,:) = squeeze(FWxDyO_0p6.rairs_all(angles,iChan,:));
  qcFWxDyO_0p3(ii,:,:) = squeeze(FWxDyO_0p3.rairs_all(angles,iChan,:));
  qcFWxDyO_0p1(ii,:,:) = squeeze(FWxDyO_0p1.rairs_all(angles,iChan,:));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_this_rtp  = 'regr49_1100_400ppm.op.rtp';  %% spres = 1100 mb  %%% >>> use for sea emis rad calcs only
use_this_rtpx = 'regr49_1013_400ppm.op.rtp';  %% spres = 1013 mb  %%% >>> use for  sea emis rad calcs only
[h,ha,p,pa] = rtpread([thedir use_this_rtp]);
mmw = mmwater_rtp(h,p);

x = [1 2 3; 4 5 6]; y = repmat(x,[1 1 3])

gas_1 = flipud(p.gas_1); gas_1 = gas_1(2:101,:); gas_1 = repmat(gas_1,[1 1 length(angles)]);
tempr = flipud(p.ptemp); tempr = tempr(2:101,:); tempr = repmat(tempr,[1 1 length(angles)]);
secangs = ones(size(gas_1));
for ii = 1 : length(angles)
  secangs(:,:,ii) = secangs(:,:,ii) * secants(ii);
end  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Feff     = qcFWO;
Deff_0p6 = qcFWO./qcFWxDyO_0p6;
Deff_0p3 = qcFWO./qcFWxDyO_0p3;
Deff_0p1 = qcFWO./qcFWxDyO_0p1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ix = 1 : 100
  
  tix   = Feff(:,:,ix+0);
  tixp1 = Feff(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  od0   = od;
  raaOD0(ix,:,:) = od;

  tix   = Feff(:,:,ix+0)./Deff_0p6(:,:,ix+0);
  tixp1 = Feff(:,:,ix+1)./Deff_0p6(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  raaODdepleted6(ix,:,:) = od;
  raaODdelta6(ix,:,:) = od0-od;  

  tix   = Feff(:,:,ix+0)./Deff_0p3(:,:,ix+0);
  tixp1 = Feff(:,:,ix+1)./Deff_0p3(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  raaODdepleted3(ix,:,:) = od;
  raaODdelta3(ix,:,:) = od0-od;

  tix   = Feff(:,:,ix+0)./Deff_0p1(:,:,ix+0);
  tixp1 = Feff(:,:,ix+1)./Deff_0p1(:,:,ix+1);
  t     = tix./tixp1;
  t     = -log(t);
  od    = t;
  raaODdepleted1(ix,:,:) = od;
  raaODdelta1(ix,:,:) = od0-od;  

end

fitdeltaOD1 = raaODdelta1(:,1:48,:);
fitdeltaOD3 = raaODdelta3(:,1:48,:);
fitdeltaOD6 = raaODdelta6(:,1:48,:);

%fitdeltaOD1 = raaODdepleted1(:,1:48,:);
%fitdeltaOD3 = raaODdepleted3(:,1:48,:);
%fitdeltaOD6 = raaODdepleted6(:,1:48,:);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[predsets,pred] = make_sarta_line_predictors(angles);

do_the_predW2

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

% loop_do_gkr(matr,booOD,-4:1:+4);   %% loop over tolerance params