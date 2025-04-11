addpath /asl/matlib/h4tools

liuse = 1:100;
secang = [1.00  1.19  1.41  1.68  1.99  2.37];
nsecang = length(secang);

% Get reference profile (#49)
[h,ha,p,pa] = rtpread('/Users/strow/Work/Rta/sarta/test/rtp_drivers/regr_rtp_6angs_49profs_1080mb_unitemis.rtp');

ref_ptemp  = flip(p.ptemp(liuse,49),1);
ref_pwater = flip(p.gas_1(liuse,49),1);
ref_pozone = flip(p.gas_3(liuse,49),1);

% Get fitting profiles
%[h,ha,p,pa]=rtpread('/Users/strow/Work/Rta/sarta/test/rtp_drivers/SAF_6angs_704profs_1080mb_unitemis.rtp');
[h,ha,p,pa]=rtpread('/Users/strow/Data/Work/Rta/ftc_dev/SAF/save_SAF_704_profiles_29-Apr-2016_1100mb.op.rtp');
%[h,ha,p,pa]=rtpread('/Users/strow/Work/Rta/sarta/test/rtp_drivers/SAF_6angs_704profs_1013mb_unitemis.rtp');
[~, nprof] = size(p.rlat);

ptemp =  flip(p.ptemp(liuse,1:nprof),1);
pwater = flip(p.gas_1(liuse,1:nprof),1);
pozone = flip(p.gas_3(liuse,1:nprof),1);
for i=1:nprof
   temp_ratio(:,i)=ptemp(:,i)./ref_ptemp;
   water_ratio(:,i)=pwater(:,i)./ref_pwater(:);
   ozone_ratio(:,i)=pozone(:,i)./ref_pozone(:);
end

[nlevs np]= size(ptemp); 
nlevs=nlevs-1; 

press = flipud(p.plevs(:,1));
stemp = p.stemp;
az = flipud(cumsum(flipud(pwater)));

save profiles_704 press pozone ptemp pwater stemp ref* secang *ratio az
