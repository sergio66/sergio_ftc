https://www.mathworks.com/matlabcentral/answers/312340-how-can-i-choose-the-best-kernel-for-a-gaussian-process-regression-possibly-using-bayesopt-functi

http://keyonvafa.com/gp-tutorial/

https://www.robots.ox.ac.uk/~mebden/reports/GPtutorial.pdf
http://www.mebden.com/index.html

addpath /home/sergio/KCARTA/MATLAB
dir0 = '/asl/ftp/pub/sergio/EMADDY/CLOUD_RCALC/';
thedir = dir([dir0 '/stats*2003.mat']);

rlat = [];
rlon = [];
solzen = [];
satzen = [];
iceOD = [];
icetop = [];
icesze = [];
icefrac = [];
waterOD = [];
watertop = [];
watersze = [];
waterfrac = [];
stemp = [];
landfrac = [];
btcalc = [];

for ii = 1 : length(thedir)
  fprintf(1,'loading  %4i \n',ii) 
  fname = [dir0 '/' thedir(ii).name];
  loader = ['junk = load(''' fname ''');'];
  eval(loader);
  rlat = [rlat junk.data.rlat];
  rlon = [rlon junk.data.rlon];
  solzen = [solzen junk.data.solzen];
  satzen = [satzen junk.data.satzen];  
  landfrac = [landfrac junk.data.landfrac];
  stemp = [stemp junk.data.stemp];
  btcalc = [btcalc rad2bt(junk.fuse,junk.data.rcalc)];

  iceOD = [iceOD junk.data.iceOD];
  icesze = [icesze junk.data.icesze];  
  icetop = [icetop junk.data.icetop];
  icefrac = [icefrac junk.data.icefrac];

  waterOD = [waterOD junk.data.waterOD];
  watersze = [watersze junk.data.watersze];  
  watertop = [watertop junk.data.watertop];
  waterfrac = [waterfrac junk.data.waterfrac];

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error('clouds_predict')
clouds_predict