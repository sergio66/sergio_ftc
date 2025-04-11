function [pred] = make_sarta_preds(angles);

pred.comment1  = 'see ../PDF_Strow_Wehr/fastmod.pdf';
pred.comment2  = 'layer_profile_angleL/P?A = layer(1=GND : 100=TOA) number, profile(1:49) number, angle(1:length(angles)) number';
pred.comment3  = 'a  = secant of path zenith';
pred.comment4  = 'Tr = temperature ratio Tprof/Tref';
pred.comment5  = 'dT = temperature diff  Tprof-Tref';
pred.comment6  = 'W  = water ratio Wprof/Wref';
pred.comment7  = 'O  = ozone ratio Oprof/Oref';
pred.comment8  = 'P  = layer mean pressure; P(101) = -(2P(100)-P(99))';
pred.comment9  = ' ';
pred.comment10 = 'Tz = pressure wgted temperature ratio above';
pred.comment11 = 'Wz = pressure wgted temperature ratio above';
pred.comment12 = 'Oz = pressure wgted temperature ratio above';
pred.comment13 = 'Mz = pressure wgted temperature ratio above';
pred.comment14 = ' ';
pred.comment15 = ' Ox = ozone ratio above';
pred.comment16 = ' To = pressure/ozone weighted temperature difference above';

thedir = '/home/sergio/MATLABCODE/REGR_PROFILES_SARTA/RUN_KCARTA/REGR49_400ppm_H2016_Dec2018_AIRS2834/';
ii = 1;
FWO     = load([thedir '/FWO/convolved_kcarta_FWO_'                   num2str(ii) '.mat']);
fc      = FWO.fairs;
secants = FWO.secants(1:length(angles));

use_this_rtp  = 'regr49_1100_400ppm.op.rtp';  %% spres = 1100 mb  %%% >>> use for sea emis rad calcs only
use_this_rtpx = 'regr49_1013_400ppm.op.rtp';  %% spres = 1013 mb  %%% >>> use for  sea emis rad calcs only
[h,ha,p,pa] = rtpread([thedir use_this_rtp]);
mmw = mmwater_rtp(h,p);

%x = [1 2 3; 4 5 6]; y = repmat(x,[1 1 3])
gas_1 = flipud(p.gas_1); gas_1 = gas_1(2:101,:); pred.gas_1 = repmat(gas_1,[1 1 length(angles)]);
gas_3 = flipud(p.gas_3); gas_3 = gas_3(2:101,:); pred.gas_3 = repmat(gas_3,[1 1 length(angles)]);
gas_6 = flipud(p.gas_6); gas_6 = gas_6(2:101,:); pred.gas_6 = repmat(gas_6,[1 1 length(angles)]);
ptemp = flipud(p.ptemp); ptemp = ptemp(2:101,:); pred.ptemp = repmat(ptemp,[1 1 length(angles)]);

for ii = 1 : 100
  for jj = 1 : 49
    for kk = 1 : length(angles)
     pred.layer_profile_angleL(ii,jj,kk) = ii;
     pred.layer_profile_angleP(ii,jj,kk) = jj;
     pred.layer_profile_angleA(ii,jj,kk) = kk;
     pred.mmwater(ii,jj,kk) = mmw(jj);
     pred.stemp(ii,jj,kk)   = p.stemp(jj);     
   end
  end
end  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% see ../PDF_Strow_Wehr/fastmod.pdf

plevs = load('/home/sergio/MATLABCODE/airslevels.dat')/1013.25;
playsN = plevs(1:end-1)-plevs(2:end);
playsD = log(plevs(1:end-1)./plevs(2:end));
plays  = playsN./playsD;
plays  = [plays; plays(end-1)-2*plays(end)];
pred.P = ones(101,49,length(angles));
for ii = 1 : 101
  pred.P(ii,:,:) = pred.P(ii,:,:) * plays(ii);
end

pred.a     = ones(size(pred.gas_1));
for ii = 1 : length(angles)
  pred.a(:,:,ii) = pred.a(:,:,ii) * secants(ii);
end

%%%%%%%%%%%%%%%%%%%%%%%%%
for ii = 100 : -1 : 1
  numer = pred.ptemp(ii,1:49,:); 
  denom = pred.ptemp(ii,49,:);   denom = repmat(denom,[1 49 1]);
  pred.Tr(ii,:,:) = numer./denom;
  pred.dT(ii,:,:) = numer-denom;

  numer = pred.gas_1(ii,1:49,:); 
  denom = pred.gas_1(ii,49,:);   denom = repmat(denom,[1 49 1]);
  pred.W(ii,:,:) = numer./denom;

  numer = pred.gas_3(ii,1:49,:); 
  denom = pred.gas_3(ii,49,:);   denom = repmat(denom,[1 49 1]);
  pred.O(ii,:,:) = numer./denom;
end

%%%%%%%%%%%%%%%%%%%%%%%%%
pred.Wz = zeros(size(pred.ptemp));;
pred.Oz = zeros(size(pred.ptemp));;
pred.Mz = zeros(size(pred.ptemp));;
pred.Tz = zeros(size(pred.ptemp));;
for ii = 100 : -1 : 1
  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    numer = pred.gas_1(jj,:,:);                                   numer = numer*wah;    
    denom = pred.gas_1(jj,49,:);  denom = repmat(denom,[1 49 1]); denom = denom*wah;
    numerator   = numerator + numer;
    denominator = denominator + denom;
  end
  pah = squeeze(numerator ./ denominator);
  pred.Wz(ii,:,:) = pah;

  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    numer = pred.gas_3(jj,:,:);                                   numer = numer*wah;    
    denom = pred.gas_3(jj,49,:);  denom = repmat(denom,[1 49 1]); denom = denom*wah;
    numerator   = numerator + numer;
    denominator = denominator + denom;
  end
  pah = squeeze(numerator ./ denominator);
  pred.Oz(ii,:,:) = pah;

  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    numer = pred.gas_6(jj,:,:);                                   numer = numer*wah;    
    denom = pred.gas_6(jj,49,:);  denom = repmat(denom,[1 49 1]); denom = denom*wah;
    numerator   = numerator + numer;
    denominator = denominator + denom;
  end
  pah = squeeze(numerator ./ denominator);
  pred.Mz(ii,:,:) = pah;

  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));    
    numer = pred.Tr(jj,:,:);
    numerator = numerator + wah * numer;
  end
  pah = squeeze(numerator);
  pred.Tz(ii,:,:) = pah;

end

%%%%%%%%%%%%%%%%%%%%%%%%%
pred.Ox = zeros(size(pred.ptemp));;
pred.To = zeros(size(pred.ptemp));;
for ii = 100 : -1 : 1

  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));    
    numer = pred.dT(jj,:,:) .* pred.O(jj,:,:);
    numerator = numerator + wah * numer;
  end
  pah = squeeze(numerator);
  pred.To(ii,:,:) = pah;

  numerator = 0;
  denominator = 0;
  for jj = ii : 100
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    numer = pred.gas_3(jj,:,:);                                   
    denom = pred.gas_3(jj,49,:);  denom = repmat(denom,[1 49 1]); 
    numerator   = numerator + numer;
    denominator = denominator + denom;
  end
  pah = squeeze(numerator ./ denominator);
  pred.Ox(ii,:,:) = pah;

end

%%%%%%%%%%%%%%%%%%%%%%%%%