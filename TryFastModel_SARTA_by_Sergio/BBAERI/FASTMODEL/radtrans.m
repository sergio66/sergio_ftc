xstartup

disp('WARNING : NO CONTINUUM!!!')

%% modelled on /home/sergio/MATLABCODE/RADTrans/CLEAR/radtransup25.m

addpath /home/sergio/MATLABCODE/RADTrans/CLEAR/

[h,ha,p,pa] = rtpread('regr48.op.rtp');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ip = 1 : length(p.stemp)
  %% do 500 - 605 cm-1
  B = 500;
  fnameIN = ['/asl/s1/sergio/BBAERI/WV/prof_' num2str(ip) '_B_500.dat'];
  [d1,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/CO2/prof_' num2str(ip) '_B_500.dat'];
  [d2,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/O3/prof_' num2str(ip) '_B_500.dat'];
  [d3,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/OTHERS/prof_' num2str(ip) '_B_500.dat'];
  [d4,w] = readkcstd(fnameIN);
  
  d = d1 + d2 + d3 + d4;

  %rad = radtransupX(raF,raaK,raT,SurfPres,TopPr,rAngle,iS,presslevels)
  raT = p.ptemp(:,ip); raT = flipud(raT);
  raP = p.plevs(:,ip); raP = flipud(raP);
  if raT(1) < 100
    disp('odd ..... raT(1) < 100 K, replace with stemp')
    raT(1) = p.stemp(ip);
  end
  rad = radtransupX(w,d,raT,max(raP),0,0,-1,raP);

  w500 = w;
  rad500(ip,:) = rad;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ip = 1 : length(p.stemp)
  %% do 605 - 1805 cm-1
  B = 605;
  fnameIN = ['/asl/s1/sergio/BBAERI/WV/prof_' num2str(ip) '_B_605.dat'];
  [d1,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/CO2/prof_' num2str(ip) '_B_605.dat'];
  [d2,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/O3/prof_' num2str(ip) '_B_605.dat'];
  [d3,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/OTHERS/prof_' num2str(ip) '_B_605.dat'];
  [d4,w] = readkcstd(fnameIN);
  
  d = d1 + d2 + d3 + d4;

  %rad = radtransupX(raF,raaK,raT,SurfPres,TopPr,rAngle,iS,presslevels)
  raT = p.ptemp(:,ip); raT = flipud(raT); 
  raP = p.plevs(:,ip); raP = flipud(raP);
  if raT(1) < 100
    disp('odd ..... raT(1) < 100 K, replace with stemp')
    raT(1) = p.stemp(ip);
  end
  rad = radtransupX(w,d,raT,max(raP),0,0,-1,raP);

  w605 = w;
  rad605(ip,:) = rad;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ip = 1 : length(p.stemp)
  %% do 1805 - 2830 cm-1
  B = 1805;
  fnameIN = ['/asl/s1/sergio/BBAERI/WV/prof_' num2str(ip) '_B_1805.dat'];
  [d1,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/CO2/prof_' num2str(ip) '_B_1805.dat'];
  [d2,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/O3/prof_' num2str(ip) '_B_1805.dat'];
  [d3,w] = readkcstd(fnameIN);

  fnameIN = ['/asl/s1/sergio/BBAERI/OTHERS/prof_' num2str(ip) '_B_1805.dat'];
  [d4,w] = readkcstd(fnameIN);
  
  d = d1 + d2 + d3 + d4;

  %rad = radtransupX(raF,raaK,raT,SurfPres,TopPr,rAngle,iS,presslevels)
  raT = p.ptemp(:,ip); raT = flipud(raT);
  raP = p.plevs(:,ip); raP = flipud(raP);
  if raT(1) < 100
    disp('odd ..... raT(1) < 100 K, replace with stemp')
    raT(1) = p.stemp(ip);
  end
  rad = radtransupX(w,d,raT,max(raP),0,0,-1,raP);

  w1805 = w;
  rad1805(ip,:) = rad;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saver = ['save /asl/s1/sergio/BBAERI/rad2_kcarta_no_continuum.mat '];
saver = [saver ' w500 rad500 w605 rad605 w1805 rad1805'];
% eval(saver)