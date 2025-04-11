% Merge all the ctrans profile data into one big matlab file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edited the following variables as needed

band = input('Enter long or short band :','s');

% outnameprefix: name of output file to create
% Note: fill fame will be <outnameprefix><band>.mat
outnameprefix='crishires_so2_data_';

% rtpname: name of RTP file will profile data
rtpname='../Prof_fow/reg_fow_op.rtp';

% nprof: number of profiles
nprof=48;

% channels and angles to save for each band
% Note: Only save channels with SO2 signal
id_long = [634:721, 722:782, 893:1038]; % with guard channels
id_short= 2068:2204; % with guard channels
iang_long = 1:6;
iang_short=1:12;

% Input data name
prefix=['../SO2_trans_crishires/ctrans_so2_crishires_'];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require any changes

addpath /asl/matlab/h4tools

nang_save = [];
if (strcmp(band,'long'))
   id_save = id_long;
   iang_save = iang_long;
   nang_save = length(iang_save);
end
if (strcmp(band,'short'))
   id_save = id_short;
   iang_save = iang_short;
   nang_save = length(iang_save);
end
if (length(nang_save) == 0)
   error('bad band')
end


% Read profile data
[head, hattr, prof, pattr]=rtpread(rtpname);
if (length(prof.nlevs) ~= nprof)
   error('Unexpected number of profiles in RTP profile data file')
end
clear head hattr pattr rtpname

% Read the first crans file (need to get dimensions)
ip=1;
fname=[prefix int2str(ip)];
disp(['reading ' fname])
eval(['load ' fname]);

% Subset channel data
[junk, ichankeep, ijunk] = intersect(ichan, id_save);
if (length(ichankeep) < length(id_save))
   error('input data lacks some desired channels')
end

ichan = ichan(ichankeep);
fchan = fchan(ichankeep);
ctall = ctall(ichankeep,:);
ctall9 = ctall9(ichankeep,:);
nchan = length(ichan);
clear id_save junk ijunk

% Subset channel data
nang_save = length(iang_save);
nanglay = nang*nlay;
if (nang_save ~= nang)
   secang = secang(iang_save);
   isave = zeros(nang_save,nlay);
   for ii = 1:nang_save
      iang = iang_save(ii);
      isave(iang,:) = iang:nang:nanglay;
   end
   clear ii iang
   nanglay = nang_save*nlay;
   iangkeep =sort( reshape(isave,1,nanglay) );
   clear isave
   ctall = ctall(:,iangkeep);
   ctall9 = ctall9(:,iangkeep);
   nang = nang_save;
end

% Declare output arrays
disp('declaring output arrays')
nprofang = nprof*nang;
%
% profile data
temp =zeros(nlay,nprofang);
h2o  =zeros(nlay,nprofang);
so2  =zeros(nlay,nprofang);
angles = reshape(secang'*ones(1,nprof),1,nprofang); %'
pnum = reshape(ones(nang,1)*(1:48),1,nprofang);
%
% ctrans data
tauz  = zeros(nchan,nlay,nprofang);
tauz9 = zeros(nchan,nlay,nprofang);
%
clear secang


% Assign output profile arrays
for il=1:nlay
   temp(il,:)= reshape(ones(nang,1)*prof.ptemp(il,:),1,nprofang);
   h2o(il,:) = reshape(ones(nang,1)*prof.gas_1(il,:),1,nprofang);
   so2(il,:) = reshape(ones(nang,1)*prof.gas_9(il,:),1,nprofang);
end
clear prof


% Assign data from 1st ctrans file to output arrays
ia=1:nang;
ioffset=0;
iap=ioffset + ia;
for il=1:100
   jj=nanglay - nang*il + ia;
    tauz(:,il,iap) =ctall(:,jj);
   tauz9(:,il,iap)=ctall9(:,jj);
end
ioffset=ioffset + nang;


% Loop over the remain ctrans files
for ip=2:nprof
   fname=[prefix int2str(ip)];
   disp(['reading ' fname])
   eval(['load ' fname]);

   % Subset channel and angle data
   ichan = ichan(ichankeep);
   fchan = fchan(ichankeep);
   ctall = ctall(ichankeep,:);
   ctall9 = ctall9(ichankeep,:);
   nchan = length(ichan);
   if (nang_save ~= nang)
      secang = secang(iang_save);
      ctall  =  ctall(:,iangkeep);
      ctall9 = ctall9(:,iangkeep);
      nang = nang_save;
   end

   iap=ioffset + ia;
   for il=1:100
      jj=nanglay - nang*il + ia;
       tauz(:,il,iap)= ctall(:,jj);
      tauz9(:,il,iap)=ctall9(:,jj);
   end
   ioffset=ioffset + nang;
end
clear ip jj il ia iap ioffset
clear  ctall ctall9 nanglay
clear prefix

clear id_long id_short iang_long iang_short
clear nang_save ichankeep iangkeep iang_save



% Save to output file
fname=[outnameprefix band];
clear outnameprefix
eval(['save ' fname])

disp('done')
