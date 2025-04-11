% Merge all the ctrans profile data into one big matlab file
%

% Update: 18 Apr 2008, Scott Hannon - minor changes for AIRS Apr08 production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edited the following variables as needed

csname = input('Enter channel set name (m130x, m150, etc) : ','s');
tefname = input('Enter Tef name (155770 or 156339) : ','s');

% outnameprefix: name of output file to create
% Note: fill fame will be <outnameprefix><band>.mat
outnameprefix = [csname '_alldata_nh3_'];

% rtpname: name of RTP file will profile data
rtpname='../Prof_fow/reg_fow_op.rtp';

% nprof: number of profiles
nprof=48;

% allband: list of supported bands
allband={'long'};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require any changes

addpath /asl/matlab/h4tools

% Ask for band
%band=input('Enter band (long or short): ','s');
band='long';
ii=strmatch(band,allband);
if ( length(ii) > 1 | ii < 1)
   ii
   band
   allband
   error(['invalid band: ' band]);
end
clear allband ii


% Read profile data
[head, hattr, prof, pattr]=rtpread(rtpname);
if (length(prof.nlevs) ~= nprof)
   error('Unexpected number of profiles in RTP profile data file')
end
clear head hattr pattr rtpname


% Read the first crans file (need to get dimensions)
nchar = length(csname);
if (strcmp(csname(nchar),'x'))
%   prefix=['../Data' tefname '/NH3trans_' band '/nh3_' csname tefname ...
%      '_' band '_'];
   prefix=['../NH3trans_' band '/nh3_' csname tefname ...
      '_' band '_'];
else
   prefix=['../NH3trans_' band '/nh3_' csname 'f'...
      '_' band '_'];
%   prefix=['../Data' tefname '/NH3trans_' band '/nh3_' csname 'f'...
%      '_' band '_'];
end
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
   h2o(il,:)=reshape(ones(nang,1)*prof.gas_1(il,:),1,nprofang);
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
fname=[outnameprefix band];
clear outnameprefix
eval(['save ' fname])

disp('done')
