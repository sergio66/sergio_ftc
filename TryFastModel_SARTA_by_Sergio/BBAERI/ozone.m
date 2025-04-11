%% computes abs coeffs for a 1cm path length
addpath /home/sergio/SPECTRA

odAERI = [];
wAERI  = [];

iaGases = [1    2   3   4   5   6   7       9   12  22];
mr      = [1000 385 100 100 100 100 0.2*1e6 100 100 0.79*1e6];

iaGases = [3  ];
mr      = [100];

xstartup
[h,ha,p,pa] = rtpread('/asl/s1/sergio/raw_afgl_nadir_seaemis_ip.rtp');
if (nanmean(h.gunit) ~= 10)
  error('oops : hoped this was in ppmv')
end
for ii = 1 : length(iaGases)
  nn = 1 : p.nlevs(1);
  gah = ['junk = p.gas_' num2str(iaGases(ii)) '(nn,1);'];
  eval(gah);
  x = interp1(log10(p.plevs(nn,1)),junk,log10(1013),[],'extrap');
  mr(ii) = x;  
end

T  = input('enter temperature in C : ');
Tc = T;

MGC=8.314674269981136  ;  
% press       = input('Enter total pressure (in atm) : ');
% temperature = input('Enter temperature (in C) : ');
% CellLength      = input('Enter path cell length (in cm) ');
press       = 1013;
temperature = T; 
CellLength      = 1;

addpath /home/sergio/MATLABCODE/CONVERT_GAS_UNITS

saver = ['save /home/sergio/MATLABCODE/BBAERI/yuval_ozone_hatchODs_Tc' num2str(Tc,'%02d') '.mat'];
saver = [saver ' w absgas iaGases mr temperature press CellLength Tc odAERI* wAERI'];
fprintf(1,'%s \n',saver)

cd /home/sergio/SPECTRA

w      = zeros(1,200/25*10000);   %% each 25 cm-1 chunk is 100000 pts, and we are spanning 1500 cm-1
od     = zeros(1,length(w));
absgas = zeros(length(iaGases),length(w));

iPar = +1;   %$% do fast parallel loops
iPar = -1;   %$% do slow single   loops

if iPar > 0
 matlabpool open
 poolSize = matlabpool('size');
 if poolSize == 0
    error('parallel:demo:poolClosed .... This demo needs an open MATLAB pool to run.');
 end
 fprintf('parallel_hatch is running on %d MATLABPOOL workers.\n', matlabpool('size'));
end

deltaF = 100;
indF   = deltaF/25*10000;
freqs = [500 : deltaF : 2000];
freqs = [955 : deltaF : 1155];

units = 3; %% mb

if iPar > 0
  parfor gg = 1 : length(iaGases)
    iGas = iaGases(gg);
    partpress = press * mr(gg)/1e6;
    for ff = 1 : length(freqs)-1
      %% each 25 cm-1 chunk is 10000 pts long, so 200 cm-1 = 80000 pts long
      indx = (1:indF) + (ff-1)*indF;
      iData = +1;
      [wx,odx] = gas_cell_arbitrary(iData,units,press,partpress,temperature,CellLength,iGas,freqs(ff),freqs(ff+1));
      w(indx)  = wx;
      absgas(gg,indx) = odx;
    end         %% for ff
    eval(saver)
  end           %% for gg
elseif iPar < 0
  for gg = 1 : length(iaGases)
    iGas = iaGases(gg);
    partpress = press * mr(gg)/1e6;
    for ff = 1 : length(freqs)-1
      %% each 25 cm-1 chunk is 10000 pts long, so 200 cm-1 = 80000 pts long
      indx = (1:indF) + (ff-1)*indF;
      iData = +1;
      [wx,odx] = gas_cell_arbitrary(iData,units,press,partpress,temperature,CellLength,iGas,freqs(ff),freqs(ff+1));
      w(indx)  = wx;
      od(indx) = odx;
    end   %% for ff
    absgas(gg,:) = od;
  end     %% for gg
  pause(2)
  eval(saver)
end

cd /home/sergio/MATLABCODE/BBAERI/

if iPar > 0
 matlabpool close
end

addpath /home/sergio/MATLABCODE/FCONV/
addpath /home/sergio/MATLABCODE/FFTCONV/
addpath /strowdata1/shared/sergio/MATLABCODE/
[wAERI, odAERI] = quickconvolve(w, absgas, 1,1);

%odall = sum(absgas);
%[odAERIall, wAERI] = quickconvolve(odall, w, 1,1);

eval(saver);

