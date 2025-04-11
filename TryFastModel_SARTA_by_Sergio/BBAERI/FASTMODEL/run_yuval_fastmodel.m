%%%%%%%%%%%%%%%%%%%%%%%%%
NP = length(profN.stemp);

get_reference_profiles_uplook
pwgt = pwgt_regr49(49,:)' * ones(1,NP);
pwgt = pwgt';

pav = pav_regr49(49,:)' * ones(1,NP);
pav = pav';

%%%%%%%%%%%%%%%%%%%%%%%%%
tic

ptemp = flipud(profN.ptemp)'; ptemp = ptemp(:,1:100);
gas_1 = flipud(profN.gas_1)'; gas_1 = gas_1(:,1:100);
gas_3 = flipud(profN.gas_3)'; gas_3 = gas_3(:,1:100);
mm = mmwater_rtp(headN,profN); mm = mm;

do_make_predictors_v4

load v4Predictors_605
reconstruct_odv4

elapse_time = toc;
fprintf(1,'time to reconstruct per profile = %8.6f \n',elapse_time/NP);

%% now do RT
radtrans_v4_yuval

elapse_time = toc;
fprintf(1,'time to do everything per profle  = %8.6f \n',elapse_time/NP);