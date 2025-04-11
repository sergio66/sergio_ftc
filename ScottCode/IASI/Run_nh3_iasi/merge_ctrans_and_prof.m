% Merge all the ctrans profile data into one big matlab file
%

% Update: 18 Apr 2008, Scott Hannon - minor changes for AIRS Apr08 production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edited the following variables as needed

% outnameprefix: name of output file to create
% Note: fill fame will be <outnameprefix>.mat
outnameprefix = 'iasi_alldata_nh3';

% rtpname: name of RTP file will profile data
rtpname='../Prof_fow/reg_fow_op.rtp';

% nprof: number of profiles
nprof=48;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require any changes

addpath /asl/matlab/h4tools

% Read profile data
[head, hattr, prof, pattr]=rtpread(rtpname);
if (length(prof.nlevs) ~= nprof)
   error('Unexpected number of profiles in RTP profile data file')
end
clear head hattr pattr rtpname


% Read the first ctrans file (need to get dimensions)
prefix=['../NH3trans/nh3_'];
ip=1;
fname=[prefix int2str(ip)];
disp(['reading ' fname])
eval(['load ' fname]);


% Declare output arrays
nprofang=nprof*nang;
nanglay=nang*nlay;
%
% profile data
temp=zeros(nlay,nprofang);
h2o=zeros(nlay,nprofang);
nh3=zeros(nlay,nprofang);
angles=reshape(secang'*ones(1,nprof),1,nprofang); %'
pnum=reshape(ones(nang,1)*(1:48),1,nprofang);
%
% ctrans data
tauz =zeros(nchan,nlay,nprofang);
tauzx=zeros(nchan,nlay,nprofang);
%
clear secang


% Assign output profile arrays
for il=1:nlay
   temp(il,:)=reshape(ones(nang,1)*prof.ptemp(il,:),1,nprofang);
   h2o(il,:)=reshape(ones(nang,1)*prof.gas_1( il,:),1,nprofang);
   nh3(il,:)=reshape(ones(nang,1)*prof.gas_11(il,:),1,nprofang);
end
clear prof


% Assign data from 1st ctrans file to output arrays
ia=1:nang;
ioffset=0;
iap=ioffset + ia;
for il=1:100
   jj=nanglay - nang*il + ia;
   tauz( :,il,iap)=ctall( :,jj);
   tauzx(:,il,iap)=ctallx(:,jj);
end
ioffset=ioffset + nang;


% Loop over the remain ctrans files
for ip=2:nprof
   fname=[prefix int2str(ip)];
   disp(['reading ' fname])
   eval(['load ' fname]);
   iap=ioffset + ia;
   for il=1:100
      jj=nanglay - nang*il + ia;
      tauz( :,il,iap)=ctall( :,jj);
      tauzx(:,il,iap)=ctallx(:,jj);
   end
   ioffset=ioffset + nang;
end
clear ip jj il ia iap ioffset
clear  ctall ctallx nanglay
clear prefix


% Save to output file
fname=outnameprefix;
clear outnameprefix
eval(['save ' fname])

disp('done')
