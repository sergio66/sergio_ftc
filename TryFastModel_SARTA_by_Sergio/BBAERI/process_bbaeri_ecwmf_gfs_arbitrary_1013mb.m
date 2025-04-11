% same as process_bbaeri_ecwmf_gfs_arbitrary.m except we use 
% /asl/packages/klayers/Bin/klayers_aeri1013

addpath  /asl/matlab/gribtools/
addpath /asl/matlab/aslutil/
addpath /asl/matlab/science/

help rtpadd_ecmwf_data

%% baltimore lat lon
prof.rlat = ones(1,7) * 39.18;
prof.rlon = ones(1,7) * -76.67;

yymmddhh = input('Enter [yy mm dd hh(local time)] : ')

if yymmddhh(4) < 24-5
  %% UTC is in the same day
  yy = yymmddhh(1);
  mm = yymmddhh(2);
  dd = yymmddhh(3);
  hh = yymmddhh(4) + 5;
else
  %% UTC is next day
  yy = yymmddhh(1);
  %% gotta be careful that you aren't in NEXT month!!!
  if mod(yy,4) == 0
    daysInmonth = [31 29 31 30 31 30 31 31 30 31 30 31];
  else
    daysInmonth = [31 28 31 30 31 30 31 31 30 31 30 31];
  end
  mm = yymmddhh(2);
  dd = yymmddhh(3)+1;
  if dd > daysInmonth(mm)
    mm = mm + 1;
    dd = 1;
  end  
  hh = mod((yymmddhh(4) + 5),24);
end

fprintf(1,'you entered localtime/date = %4i %2i %2i %2i \n',yymmddhh);
fprintf(1,'getting ECMWF/GFS data for  %4i %2i %2i %2i \n',yy,mm,dd,hh);

prof.rtime = mattime2tai(datenum(yy,mm,dd,hh,0:10:60,0));
pattr = set_attr([],'rtime','time since 1993','profiles');

[head, hattr, prof, pattr] = rtpadd_ecmwf_data(struct, [], prof, pattr, ...
{'SP','SKT','10U','10V','TCC','CI','T','Q','O3'});

%head.pfields = 0;
%[head, hattr, prof2, pattr] = rtpadd_gfs(head, [], prof, pattr);
prof2 = prof;

plot(prof.ptemp,prof.plevs,'bo',prof2.ptemp,prof2.plevs,'r')
semilogy(prof.ptemp,prof.plevs,'bo',prof2.ptemp,prof2.plevs,'r')
title('b = ECMWF    r= GFS')'

disp('ret to continue'); pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xstartup
klayers = '/asl/packages/klayers/Bin/klayers_airs'; 
klayers = '/asl/packages/klayers/Bin/klayers_aeri1013';
kcarta  = '/home/sergio/KCARTA/BIN/bkcarta.x.116_385ppmv';

head.vcmin = 605; head.vcmax = 2830;
prof2.cfrac(:) = 0;
prof2.upwell   = 2*ones(size(prof2.stemp));
prof2.scanang  = 0*ones(size(prof2.stemp));
prof2.nemis    = 2*ones(size(prof2.stemp));
prof2.efreq    = [head.vcmin-5 head.vcmax+5]' *ones(1,length(prof2.upwell));
prof2.emis     = [1.0          1.0]' *ones(1,length(prof2.upwell));
prof2.remis    = 2*ones(size(prof2.stemp));
prof2.rfreq    = [head.vcmin-5 head.vcmax+5]' *ones(1,length(prof2.upwell));
prof2.rho      = (1-prof2.emis)/pi;

names = fieldnames(prof2); 
for ii = 1 : length(names);
  str = (['prof2.' names{ii} ' = double(prof2.' names{ii} ');']);
  eval(str)
end

disp('running klayers ...')
fip = 'junk.ip.rtp'; fop = 'junk.op.rtp';
head.pfields = 1;
oldrtpwrite(fip,head, hattr, prof2, pattr);
klayerer = ['!' klayers ' fin=' fip ' fout=' fop ' >& ugh ']; eval(klayerer)

[headN,haN,profN,paN] = oldrtpread(fop);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tic
disp('running kcarta ...')
rmer = ['!/bin/rm process.dat']; eval(rmer);
kcartaer = ['!' kcarta ' process.nml process.dat'];
eval(kcartaer)

disp('running FFTconv ...')
[r0,w] = readkcstd('process.dat');

%blah = find(isnan(rc)); rc(blah) = 0.0;

addpath /home/sergio/MATLABCODE/FCONV/
addpath /home/sergio/MATLABCODE/FFTCONV/

[rch, wch] = xfconvkc_serg_iasi([r0]', w, 'aeriB1', 'nb', 6);
elapse_time = toc;
fprintf(1,'time to run kCARTA and convolve = %8.6f secs \n',elapse_time)

figure(1); plot(wch,rad2bt(wch,rch(1,:)'))
  xlabel('Wavenumber cm-1'); ylabel('BT (K)')
  hl = legend('clear'); set(hl,'fontsize',10); title('Using GFS')
figure(2); plot(wch,rch(1,:)')
  xlabel('Wavenumber cm-1'); ylabel('rad mW cm-2/sr-1/cm-1')
  hl = legend('clear'); set(hl,'fontsize',10); title('Using GFS')

str = [num2str(yymmddhh(1),'%04d') num2str(yymmddhh(2),'%02d') ...
       num2str(yymmddhh(3),'%02d') '_' num2str(yymmddhh(4),'%02d')];
fid = fopen(['process_1013mb_' str '.txt'],'w');
data = [wch; rch(1,:); rad2bt(wch,rch(1,:)')'];
data = data(:,6:length(data)-5);
fprintf(fid,'%10.6f %8.4e %8.4e \n',data);
fclose(fid);

%lala = load(['process_' str '.txt']);
%plot(lala(:,1))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% now run my current FASTMODEL
cd FASTMODEL
run_yuval_fastmodel

figure(3); 
  plot(wch,rch(1,:)',fc,raaxTryFWO(1,:),'r')
  xlabel('Wavenumber cm-1'); ylabel('rad mW cm-2/sr-1/cm-1')
  hl = legend('clear KC','clear SARTA'); set(hl,'fontsize',10); title('Using GFS')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error('woof load data???')

if yymmddhh(2) == 03 & yymmddhh(3) == 02  
  yuval = load('AERI_March02.csv');
  figure(3); 
  plot(wch,rch(1,:)',yuval(:,1),yuval(:,3:82),'g',...
                     yuval(:,1),mean(yuval(:,3:82)'),'r')
elseif yymmddhh(2) == 03 & yymmddhh(3) == 30  
  yuval = load('AERI_March30.csv');
  [mma,nna] = size(yuval);
  figure(3); 
  plot(wch,rch(1,:)',yuval(:,2),yuval(:,3:nna),'g',...
                     yuval(:,2),mean(yuval(:,3:nna)'),'r')

  figure(4); 
  blah = real(rad2bt(yuval(:,2),yuval(:,3:nna)));
  plot(yuval(:,2),blah,'g',wch,rad2bt(wch,rch(1,:)'),'b',...
       yuval(:,2),nanmean(blah'),'r')

end

figure(3)
  title(['(b) kCARTA (g) all BBAERI (r) mean BBAERI']);
  xlabel('Wavneumber cm-1'); ylabel('Radiance')
  grid
figure(4)
  title(['(b) kCARTA (g) all BBAERI (r) mean BBAERI']);
  xlabel('Wavneumber cm-1'); ylabel('BT (K)')
  grid
% figname = ['process_rad_' str]; printfig(3,figname,'png')
% figname = ['process_bt_' str];  printfig(4,figname,'png')
