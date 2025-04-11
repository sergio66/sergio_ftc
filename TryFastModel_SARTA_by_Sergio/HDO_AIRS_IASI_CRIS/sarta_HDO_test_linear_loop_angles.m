%xstartup
addpath /home/sergio/KCARTA/MATLAB
addpath /home/sergio/MATLABCODE/
addpath /asl/matlib/aslutil/
addpath /asl/matlib/h4tools/

[dall,w]  = readkcstd('allHDO.dat');
[dnone,w] = readkcstd('noHDO.dat');
[d0p1,w]  = readkcstd('HDO_deplete0p1.dat');
[fc,qc] = convolve_airs(w,[dnone d0p1 dall],1:2378);
tc = rad2bt(fc,qc);
plot(fc,tc(:,1)-tc(:,3),'b',fc,tc(:,2)-tc(:,3),'r')
title('tropical profile'); ylabel('\delta BT (K)')
hl = legend('No HDO-full HDO','0.9 HDO - full HDO'); set(hl,'fontsize',10)

%% this is BAREBONES, what ya need, dude!!!!!
%% this is BAREBONES, what ya need, dude!!!!!
%% this is BAREBONES, what ya need, dude!!!!!

thedir = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
use_this_rtp  = 'regr49_1100_400ppm.op.rtp';  %% spres = 1100 mb  %%% >>> use for sea emis rad calcs only
use_this_rtpx = 'regr49_1013_400ppm.op.rtp';  %% spres = 1013 mb  %%% >>> use for  sea emis rad calcs only
[h,ha,p,pa] = rtpread([thedir use_this_rtp]);
mmw = mmwater_rtp(h,p);

%{
profile = 49;
FWO    = load('//home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/FWO/convolved_kcarta_FWO_49.mat');
FWxDyO_0p1 = load('//home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/FWxDyO_depleted0.1/convolved_kcarta_FWxDyO_49.mat');
FWxDyO_0p3 = load('//home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/FWxDyO_depleted0.3/convolved_kcarta_FWxDyO_49.mat');
FWxDyO_0p6 = load('//home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/FWxDyO_depleted0.6/convolved_kcarta_FWxDyO_49.mat');
%}

pp = 15;
profile = pp;
  dirin = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
  FWO        = load([dirin 'FWO/convolved_kcarta_FWO_' num2str(pp) '.mat']);
  FWxDyO_0p1 = load([dirin '/FWxDyO_depleted0.1/convolved_kcarta_FWxDyO_' num2str(pp) '.mat']);
  FWxDyO_0p3 = load([dirin '/FWxDyO_depleted0.3/convolved_kcarta_FWxDyO_' num2str(pp) '.mat']);
  FWxDyO_0p6 = load([dirin '/FWxDyO_depleted0.6/convolved_kcarta_FWxDyO_' num2str(pp) '.mat']);

for jj = 1 : 14
  fc           = FWO.fairs;
  qcFWO        = squeeze(FWO.rairs_all(jj,:,:));
  qcFWxDyO_0p6 = squeeze(FWxDyO_0p6.rairs_all(jj,:,:));
  qcFWxDyO_0p3 = squeeze(FWxDyO_0p3.rairs_all(jj,:,:));
  qcFWxDyO_0p1 = squeeze(FWxDyO_0p1.rairs_all(jj,:,:));
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  [mmqcFWO,nnqcFWO] = size(qcFWO);
  figure(1)
  plot(qcFWO(1,:),nnqcFWO,'bo-',qcFWxDyO_0p6(1,:),nnqcFWO,'rs-',qcFWxDyO_0p3(1,:),nnqcFWO,'gd-'); hold on
  %plot(qcFWO,1:nnqcFWO,'bo-',qcFWxDyO_0p6,1:nnqcFWO,'rs-',qcFWxDyO_0p3,1:nnqcFWO,'gd-'); hold off
  hl = legend('FWO','FWxDyO 0.6','FWxDyO 0.3'); title('transmittances of FWO FWxDyO')
  set(hl,'fontsize',10); grid

  Feff     = qcFWO;
  Deff_0p6 = qcFWO./qcFWxDyO_0p6;
  Deff_0p3 = qcFWO./qcFWxDyO_0p3;
  Deff_0p1 = qcFWO./qcFWxDyO_0p1;

  figure(2)
  plot(Feff(1,:),1:nnqcFWO,'bo-',Deff_0p6(1,:),1:nnqcFWO,'rs-',Deff_0p3(1,:),1:nnqcFWO,'gd-');
  hl = legend('Feff','Deff 0p6','Deff 0p3'); 
  title('effective transmittances of F D')
  set(hl,'fontsize',10); grid

  figure(3);
  plot(fc,qcFWO(:,1),'b',fc,qcFWO(:,101),'c',fc,qcFWxDyO_0p6(:,1),'r',fc,qcFWxDyO_0p6(:,101),'m',fc,qcFWxDyO_0p3(:,1),'g',fc,qcFWxDyO_0p3(:,101),'k')

  figure(4);
  %% all HDO should have lower l2s transmittance than depleted HDO so the ratio should be less than 1
  plot(fc,qcFWO(:,1)./qcFWxDyO_0p6(:,1),'r',fc,qcFWO(:,1)./qcFWxDyO_0p3(:,1),'b')

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for ix = 1 : 100
  
    tix   = Feff(:,ix+0); %% Feff = FWO = (F)(W'+1.0D)(O)
    tixp1 = Feff(:,ix+1);
    t     = tix./tixp1;
    t     = -log(t);
    od    = t;
    od0   = od;
    raaOD0(ix,:) = od;

    tix   = Feff(:,ix+0)./Deff_0p6(:,ix+0); %% Feff/Deff = FWO/(FWO/FW''O) = FW''O = (F)(W'+0.4D)(O)
    tixp1 = Feff(:,ix+1)./Deff_0p6(:,ix+1);
    t     = tix./tixp1;
    t     = -log(t);
    od    = t;
    raaODdepleted6(ix,:) = od;
    raaODdelta6(ix,:) = od0-od;  

    tix   = Feff(:,ix+0)./Deff_0p3(:,ix+0); %% Feff/Deff = FWO/(FWO/FW''O) = FW''O = (F)(W'+0.7D)(O)
    tixp1 = Feff(:,ix+1)./Deff_0p3(:,ix+1);
    t     = tix./tixp1;
    t     = -log(t);
    od    = t;
    raaODdepleted3(ix,:) = od;
    raaODdelta3(ix,:) = od0-od;

    tix   = Feff(:,ix+0)./Deff_0p1(:,ix+0); %% Feff/Deff = FWO/(FWO/FW''O) = FW''O = (F)(W'+0.9D)(O)
    tixp1 = Feff(:,ix+1)./Deff_0p1(:,ix+1);
    t     = tix./tixp1;
    t     = -log(t);
    od    = t;
    raaODdepleted1(ix,:) = od;
    raaODdelta1(ix,:) = od0-od;  

  end
  figure(1); clf
  plot(fc,raaODdelta1(1,:),fc,raaODdelta6(1,:)); title('DeltaOD for 0.1 and 0.6 depletion')
  grid

  figure(2)
  imagesc(fc,1:100,raaOD0); colorbar
  ii = 1; plot(fc,raaOD0(1,:),'b',fc,raaODdepleted3(1,:),'g',fc,raaODdepleted6(1,:),'r')
  ii = 1; plot(fc,raaODdelta3(1,:),'b',fc,raaODdelta6(1,:),'r')
  ii = 1; plot(fc,raaODdelta3(1,:)./raaODdelta1(1,:),'b',fc,raaODdelta6(1,:)./raaODdelta1(1,:),'r')
  axis([640 2705 0 10])
  
  ratioDepletedOD = [raaODdelta1./raaODdelta6; raaODdelta3./raaODdelta6];
  plot(0:0.05:1,histc(ratioDepletedOD(:),0:0.05:1))

  figure(4)
  i2722 = find(fc >= 2722,1);
  plot(raaODdelta1(:,i2722),1:100,'b+-',raaODdelta3(:,i2722),1:100,'g+-',raaODdelta6(:,i2722),1:100,'r+-','linewidth',2);grid
  title('Delta OD when HDO depeleted, 2722 cm-1');
  hl = legend('depleted by 0.1','depleted by 0.3','depleted by 0.6'); set(hl,'fontsize',10);

  figure(5)
  i1254 = find(fc >= 1254,1);
  plot(raaODdelta1(:,i1254),1:100,'b+-',raaODdelta3(:,i1254),1:100,'g+-',raaODdelta6(:,i1254),1:100,'r+-','linewidth',2);grid
  title('Delta OD when HDO depeleted, 1254 cm-1');
  hl = legend('depleted by 0.1','depleted by 0.3','depleted by 0.6'); set(hl,'fontsize',10);

  figure(6)
  i1281 = abs(fc - 1280.5); i1281 = find(i1281 == min(i1281));
  plot(raaODdelta1(:,i1281),1:100,'b+-',raaODdelta3(:,i1281),1:100,'g+-',raaODdelta6(:,i1281),1:100,'r+-','linewidth',2);grid
  title('Delta OD when HDO depeleted, 1281 cm-1');
  hl = legend('depleted by 0.1','depleted by 0.3','depleted by 0.6'); set(hl,'fontsize',10);

  for ii = 4:6
    figure(ii); ax = axis; ax(4) = 40; axis(ax);
  end

  figure(7);
  subplot(121);
    semilogy(p.ptemp(:,profile),p.plevs(:,profile)); axis([220 320 300 1100]); grid; set(gca,'ydir','reverse')
  subplot(122);
    loglog(p.gas_1(:,profile),p.plevs(:,profile)); axis([1e16 1e23 300 1100]); grid; set(gca,'ydir','reverse')

  fprintf(1,'angle %2i of 14 \n',jj);
  disp('ret to continue'); pause;
end
