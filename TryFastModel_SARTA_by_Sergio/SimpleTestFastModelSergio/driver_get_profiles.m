addpath /asl/matlib/rtptools
addpath /asl/matlib/aslutil
addpath /asl/matlib/h4tools
addpath /home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA

disp('look at /home/sergio/FastModelDevelopment')
disp('      and  ')
disp('look at /home/sergio/MATLABCODE/BBAERI/find_predictors_uplook49_605v2.m')
disp('look at /home/sergio/MATLABCODE/BBAERI/find_predictors_uplook49_605v4.m')
disp('      and  ')
disp('look at //home/sergio/MATLABCODE/BBAERI/FASTMODEL/*.pdf')

%       12345678901234
%       1 2 3 4 5   = which set of profs  %% 1 is for 49 regr, 2 is for SAF 794, 3 is for SAF 25000, 4 is for ECMWF Marco 83, 5 is for Juying
%       6 7         = breakout (as in set_rtp.m) [01:14][98 99]
%       8 9 10      = layer number (1..101)
%       11          = instr (1 = AIRS, 2 = Cris, 3 = IASI
%       12 13 14 15 = chan number

%       12345678901234
XJOB = '102001401010001';    %% should be two separate sets, "1" and "2"

XJOB = '100000103010449';    %% only one set "1"  FW   792 cm-1
XJOB = '100000103011614';    %% only one set "1"  FW   1419 cm-1

XJOB = '100000203010445';    %% only one set "1"  F             791 cm-1
XJOB = '100003103011614';    %% only one set "1"  W = FW/W,     1420 cm-1

XJOB = '100003303012333';    %% only one set "1"  D = FWOD/FWO  2616 cm-1
XJOB = '100003303012637';    %% only one set "1"  D = FWOD/FWO  1285 cm-1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

theprofiles = XJOB(1:5);
iBreak  = str2num(XJOB(6:7));
iLay    = str2num(XJOB(8:10));
iInstr  = str2num(XJOB(11:11));
if iInstr == 1
  strInstr = 'AIRS';
elseif iInstr == 2
  strInstr = CrisHiRes';
elseif iInstr == 3
  strInstr = 'IASI';
end
iChan   = str2num(XJOB(12:15));

fprintf(1,'iInstr strInstr iChan = %2i %s %5i \n',iInstr,strInstr,iChan)

set_the_individual_iBreak   %% see /home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/set_the_individual_iBreak

outname = ['FITRESULTS/fit_lay_' num2str(iLay,'%03d') '_breakout_' num2str(iBreak) '_' strBreak '_instr_' strInstr '_chan_' num2str(iChan,'%04d')];
fprintf(1,'XJOB = %s ---> outname = %s \n',XJOB,outname);
fprintf(1,' Instr = %s \n',strInstr);
fprintf(1,'   Lay = %3i Chan = %4i \n',iLay,iChan);
fprintf(1,'   iBreak = %3i %s \n',iBreak,strBreak);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

caDir{1} = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Mar2018/';
caDir{1} = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
  caFname{1}.use_this_rtp_1100_seaemis  = 'regr49_1100_400ppm.op.rtp';  %% spres = 1100 mb  %%% >>> USE THIS FOR BREAKOUTS  400pm, sea emis
  caFname{1}.use_this_rtp_1013_seaemis  = 'regr49_1013_400ppm.op.rtp';  %% spres = 1013 mb  %%% >>> use this only for radiance SARTA testing 400 ppm, sea emis
  caFname{1}.use_this_rtp_1100_unitemis = 'regr49_1100_400ppm_unitemiss.op.rtp';  %% spres = 1100 mb  %%% >>> USE THIS FOR BREAKOUTS  400pm, unit emis
  caFname{1}.use_this_rtp_1013_unitemis = 'regr49_1013_400ppm_unitemiss.op.rtp';  %% spres = 1013 mb  %%% >>> use this only for radiance SARTA testing 400 ppm, unit emis

caDir{2} = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/SAF704_400ppm_H2016_Mar2018/';
caDir{2} = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
  caFname{2}.use_this_rtp_1100_seaemis  = 'SAF_704/save_SAF_704_profiles_25-May-2016_xmb_400ppmv_unitemis.op.rtp';       %% 704 profiles from SAF, x mb, unit emis
  caFname{2}.use_this_rtp_1013_seaemis  = 'SAF_704/save_SAF_704_profiles_29-Apr-2016_1100mb_400ppmv_unitemis.op.rtp';    %% 704 profiles from SAF, 1100 mb, unit emis
  caFname{2}.use_this_rtp_1100_unitemis = 'SAF_704/save_SAF_704_profiles_25-May-2016_xmb_400ppmv.op.rtp';       %% 704 profiles from SAF, x mb, sea emis
  caFname{2}.use_this_rtp_1013_unitemis = 'SAF_704/save_SAF_704_profiles_29-Apr-2016_1100mb_400ppmv.op.rtp';    %% 704 profiles from SAF, 1100 mb, sea emis

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now loop over and get
%%   secangs and profiles and breakouts
%% so you are ready for fitting

%% find out how many unique sets there are, and how many profiles in each set
iNumSet = 0;
iaProfSet = [];
for ii = 1 : 5
  xset = str2num(theprofiles(ii:ii));
  if xset >= 1 & xset <= 2
    if iNumSet == 0
      iNumSet = iNumSet + 1;
      iaProfSet(1) = xset;
    elseif iNumSet > 0
      if length(intersect(xset,iaProfSet)) == 0
        iNumSet = iNumSet + 1;
        iaProfSet(iNumSet) = xset;
      end
    end
  end
end
fprintf(1,'from %s found %2i unique profsets \n',theprofiles,iNumSet);
iaProfSet = sort(iaProfSet)

%% load in US Std
ii = 1;  %% this is the 49regProfs
frtp = [caDir{iaProfSet(ii)} '/' caFname{iaProfSet(ii)}.use_this_rtp_1100_seaemis];
  [h,ha,p,pa] = rtpread(frtp);
  [hStd,pStd] = subset_rtp(h,p,[],[],49);
  
%% load in profiles
hall = [];
pall = [];
for ii = 1 : iNumSet
  frtp = [caDir{iaProfSet(ii)} '/' caFname{iaProfSet(ii)}.use_this_rtp_1100_seaemis];
  fprintf(1,'loading %2i %s \n',ii,frtp);
  [h,ha,p,pa] = rtpread(frtp);
  iNumProfsSet(ii) = length(p.stemp);
  if ii == 1
    hall = h;
    pall = p;
  else
    [hall,pall] = cat_rtp(hall,pall,h,p);
  end
end

%% load in breakouts
iCnt = 0;
data = [];
data2 = [];
secang = [];

if iBreak <= 30
  for ii = 1 : iNumSet
    for jj = 1 : iNumProfsSet(ii);
      iCnt = iCnt + 1;
      fprintf(1,'  .... loading data for %4i of %4i \n',iCnt,sum(iNumProfsSet))
      clear junk xdat
      fbreak = [caDir{iaProfSet(ii)} '/' strBreak '/convolved_kcarta_' strBreak '_' num2str(jj) '.mat'];
      loader = ['junk = load(''' fbreak ''');'];
      eval(loader);
      fairs = junk.fairs;
      xdat  = squeeze(junk.rairs_all(:,iChan,:));    
      secants = junk.secants;
      if iCnt == 1
        data = xdat;
        secang = secants;
      else
        data = cat(1,data,xdat);
        secang = [secang secants];
      end  
    end    %% for jj = 1 : iNumProfsSet(ii);
  end      %% for ii = 1 : iNumSet
  data1orig = data;
  
elseif iBreak > 30
  if iBreak == 31
    strBreak = 'FW';
    xstrBreak = 'FW';
    strBreak2 = 'F';
  elseif iBreak == 32
    strBreak = 'FWO';
    xstrBreak = 'FWO';    
    strBreak2 = 'FW';
  elseif iBreak == 33
    strBreak = 'FWxDyO_depleted0.1';
    xstrBreak = 'FWxDyO';
    strBreak2 = 'FWO';
  end
  for ii = 1 : iNumSet
    for jj = 1 : iNumProfsSet(ii);
      iCnt = iCnt + 1;
      fprintf(1,'  .... loading data for %4i of %4i \n',iCnt,sum(iNumProfsSet))

      clear junk xdat
      fbreak = [caDir{iaProfSet(ii)} '/' strBreak '/convolved_kcarta_' xstrBreak '_' num2str(jj) '.mat'];
      loader = ['junk = load(''' fbreak ''');'];
      eval(loader);
      fairs = junk.fairs;
      xdat  = squeeze(junk.rairs_all(:,iChan,:));    
      secants = junk.secants;
      if iCnt == 1
        data = xdat;
        secang = secants;
      else
        data = cat(1,data,xdat);
        secang = [secang secants];
      end

      clear junk xdat
      fbreak = [caDir{iaProfSet(ii)} '/' strBreak2 '/convolved_kcarta_' strBreak2 '_' num2str(jj) '.mat'];
      loader = ['junk = load(''' fbreak ''');'];
      eval(loader);
      fairs = junk.fairs;
      xdat  = squeeze(junk.rairs_all(:,iChan,:));    
      secants = junk.secants;
      if iCnt == 1
        data2 = xdat;
      else
        data2 = cat(1,data2,xdat);
      end  
    end    %% for jj = 1 : iNumProfsSet(ii);
  end      %% for ii = 1 : iNumSet
  data1orig = data;
  data2orig = data2;    
  if iBreak == 31 | iBreak == 32
    data3orig = data1orig./data2orig;
    data = data./data2;
    clear data2
  else
    %% need data = FWxDyO_depleted0.1 and data2 = FWO
    %% note data1orig = FWxDyO_depleted0.1
    %%      data2orig = FWallDO
    %% so OD1 < OD2 and transmission1 > transmission2
    data3orig = data2orig./data1orig;    
    data  = data2./data;  %% FWOeff/FWxDyO_0p1 = (F)(W'+0.9D+0.1D)(O)/(F)(W'+0.9D)(O) = 0.1D
    data2 = data2;        %% FWOeff
  end
end

whos data* xdat

%% just quick check everythig seems ok
wah = ones(1,length(pall.stemp)*length(secants));
whos data secangs wah
plot(secang,data,'.'); xlabel('secang'); ylabel('breakout');

%% this was printed out above, just a reminder ...
fprintf(1,'XJOB = %s ---> outname = %s \n',XJOB,outname);
fprintf(1,' Instr = %s \n',strInstr);
fprintf(1,'   Lay = %3i Chan = %4i \n',iLay,iChan);
fprintf(1,'   iBreak = %3i %s \n',iBreak,strBreak);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure(1); clf
subplot(121);
if exist('data2')
  plot(data(1,:),1:101,'b+-',data2(1,:),1:101,'r.-')
  hl = legend('data1','data2');
else
  plot(data(1,:),1:101,'b+-')
  hl = legend('data1');  
end
title('L2S before fliplr'); grid on
ax = axis; ax(3) = 1; ax(4) = 101; axis(ax);
text(-0.05,000,'GND');
text(-0.05,100,'TOA');

figure(2); clf
subplot(121);
if exist('data2orig') & exist('data3orig')
  %% transmission1 > transmission2 so blue should be slightly larger than red
  plot(data1orig(1,:),1:101,'b+-',data2orig(1,:),1:101,'r.-',data3orig(1,:),1:101,'gx-')
  hl = legend('data1orig','data2orig','data3orig');
elseif exist('data2orig') & ~exist('data3orig')
  plot(data1orig(1,:),1:101,'b+-',data2orig(1,:),1:101,'r.-')
  hl = legend('data1orig','data2orig');
elseif ~exist('data2orig') & ~exist('data3orig')  
  plot(data1orig(1,:),1:101,'b+-')
  hl = legend('data1orig');  
end
title('L2S before fliplr'); grid on
ax = axis; ax(3) = 1; ax(4) = 101; axis(ax);
text(-0.05,000,'GND');
text(-0.05,100,'TOA');

%% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% typically data(:,101) == 1 since it is Top of AIRS atmospehre to space == no od ==> trans = 1
%%           data(:,001) == smaller than 1 since it is layer to space OD
%% while pStd(1) = TOA, pStd(101) = GND

data = fliplr(data);
if iBreak == 33
  data2 = fliplr(data2);
end

data1orig = fliplr(data1orig);
data2orig = fliplr(data2orig);
data3orig = fliplr(data3orig);

%% now data(:,001) == 1 since it is Top of AIRS atmosphere to space == no od ==> trans = 1
%%     data(:,101) == smaller than 1 since it is layer to space OD
%% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

figure(1);
subplot(122)
if exist('data2')
  plot(data(1,:),1:101,'b+-',data2(1,:),1:101,'r.-')
  hl = legend('data1','data2');
else
  plot(data(1,:),1:101,'b+-')
  hl = legend('data1');  
end
title('L2S after fliplr'); grid on
ax = axis; ax(3) = 1; ax(4) = 101; axis(ax);
text(-0.05,100,'GND');
text(-0.05,000,'TOA');
set(gca,'ydir','reverse')

figure(2)
subplot(122)
if exist('data2orig') & exist('data3orig')
  %% transmission1 > transmission2 so blue should be slightly larger than red
  plot(data1orig(1,:),1:101,'b+-',data2orig(1,:),1:101,'r.-',data3orig(1,:),1:101,'gx-')
  hl = legend('data1orig','data2orig','data3orig');
elseif exist('data2orig') & ~exist('data3orig')
  plot(data1orig(1,:),1:101,'b+-',data2orig(1,:),1:101,'r.-')
  hl = legend('data1orig','data2orig');
elseif ~exist('data2orig') & ~exist('data3orig')  
  plot(data1orig(1,:),1:101,'b+-')
  hl = legend('data1orig');  
end
title('L2S after fliplr'); grid on
ax = axis; ax(3) = 1; ax(4) = 101; axis(ax);
text(-0.05,100,'GND');
text(-0.05,000,'TOA');
set(gca,'ydir','reverse')

figure(3); clf
  effOD1 = data(:,2:101) ./ data(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
  effOD1 = -log(effOD1);
mwak = effOD1(:,91); mwak = find(mwak == min(mwak));  
  plot(effOD1,1:100,'b',effOD1(mwak,:),1:100,'r+-'); title('ODeff');
  text(-0.05,100,'GND');
  text(-0.05,000,'TOA');
  set(gca,'ydir','reverse'); grid on

if iBreak == 33
  figure(4); clf
  effODx = data2orig(:,2:101) ./ data2orig(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
  effODx = -log(effODx);
  plot(effODx,1:100,'b',effODx(mwak,:),1:100,'r+-'); title('OD all');
  text(-0.05,100,'GND');
  text(-0.05,000,'TOA');
  set(gca,'ydir','reverse'); grid on
elseif iBreak == 31 | iBreak == 32
  figure(4); clf
  effODx = data1orig(:,2:101) ./ data1orig(:,1:100);  %% keff(i) = -log(taueff(i)/taueff(i-1)) see IEEE 2003 paper
  effODx = -log(effODx);
  plot(effODx,1:100,'b',effODx(mwak,:),1:100,'r+-'); title('OD all');
  text(-0.05,100,'GND');
  text(-0.05,000,'TOA');
  set(gca,'ydir','reverse'); grid on
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_the_fits
plot_the_fits

%{
saver = ['save FITRESULTS/chan_' num2str(iChan,'%04d') '_instr_' strInstr '_break_' num2str(iBreak) '_type_' strBreak '.mat fitall'];
eval(saver)
%}
