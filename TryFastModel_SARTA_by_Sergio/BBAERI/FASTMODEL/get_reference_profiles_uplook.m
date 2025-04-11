
%% get the REFERENCE stuff for uplook instr
[hRegr49,ha,pRegr49,pa] = rtpread('regr48.op.rtp');
mm = mmwater_rtp(hRegr49,pRegr49);

  gas_1_regr49 = pRegr49.gas_1;
  gas_3_regr49 = pRegr49.gas_3;
  ptemp_regr49 = pRegr49.ptemp;
  stemp_regr49 = pRegr49.stemp;

pav_regr49 = pRegr49.plays(1:100,:);
pav_regr49 = flipud(pav_regr49)'/1013;

gas_1_regr49 = gas_1_regr49(1:100,:)';
gas_3_regr49 = gas_3_regr49(1:100,:)';
ptemp_regr49 = ptemp_regr49(1:100,:)';

gas_1_regr49 = fliplr(gas_1_regr49);
gas_3_regr49 = fliplr(gas_3_regr49);
ptemp_regr49 = fliplr(ptemp_regr49);

tref = ptemp_regr49(49,:);
g1ref= gas_1_regr49(49,:);
g3ref= gas_3_regr49(49,:);

dp = pav_regr49(:,1:99)-pav_regr49(:,2:100); dp(:,100) = zeros(1,49);
pwgt_regr49 = pav_regr49.*dp; pwgt_regr49 = cumsum(pwgt_regr49);           %% this is for downlook eg AIRS
pwgt_regr49 = pav_regr49.*dp; pwgt_regr49 = cumsum(fliplr(pwgt_regr49));   %% this is for uplook   eg BAERI

