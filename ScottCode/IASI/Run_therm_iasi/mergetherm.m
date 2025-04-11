% This is a matlab program to merge the individual profile reflected
% thermal data files into one matlab output file containing *all* the
% data needed to do the fast model ref therm.
%
% Besides the matlab files with the rad and trans and angle data, we
% also need all the profile temperatures, as well as reference temp
% profile.  This is pulled in from separate a RTP file.

% Extensively revised for the April 2002 production code.
% Update: 12 April 2002, Scott Hannon
% Update: 20 Dec 2007, S.Hannon - minor changes for IASI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%band=input('Enter band : ','s');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modify the variables below as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% comment: string to be included in output file
comment='IASI 2cm Gauss, Dec07';


%%%%%%%
% nprof: Number of profiles
nprof=48;


%%%%%%%
% rtpfile : Name of the matlab file with the reg & ref profiles
% The file must be a "layers" contain the variables:
%    head.ptype = 1 ("layers" profile)
%    prof.nlevs (1 x nprof+1)
%    prof.spres (1 x nprof+1)
%    prof.ptemp (nlay x nprof+1)
% where the reference profile is number nprof+1
rtpfile='../Prof_therm/pin_feb2002_reg_op.rtp';


%%%%%%%
% thermfile: Name prefix of input thermal data files
%thermfile=['../Therm_' band '/m' band '_'];
thermfile=['../Therm_iasi/therm'];


%%%%%%%
% outfile: Name prefix of output matlab file to create
outfile=['iasi_thermdata'];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main executable code begins below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in the first therm data file
iprof=1;
eval(['load ' thermfile int2str(iprof)]);
%
freq=fchan; 
idchan=ichan;
nchan=length(freq);
nang=length(secang);


% Load in the profile temperature data
addpath /asl/matlab/h4tools
[head, hattr, prof, pattr] = rtpread(rtpfile);
%
% Check that the RTP data looks plausible
if (head.ptype ~= 1)
   error('RTP file must contain "layers" type profiles')
end
ii=nprof + 1;
if (length(prof.nlevs) < ii)
   error('RTP file contains too few profiles')
end
if (length(prof.nlevs) > ii)
   disp('WARNING: RTP file contains more profiles than expected')
end
if ( min(prof.nlevs(1:ii))-1 < nlay)
   error('Some RTP profiles have fewer layers than expected')
end
if ( min(prof.spres(1:ii)) < psurf)
   error('Some RTP profiles have lower surface pressure than expected')
end
% Note: prof.spres < psurf is bad, but prof.spres > psurf is OK
%
% Pull out the temperature data (reverse direction so 1=surface)
temp=prof.ptemp(nlay:-1:1,1:nprof);
tref=prof.ptemp(nlay:-1:1,nprof+1);
clear head prof hattr pattr


% Declare output arrays
tprof=zeros(nlay, nang*nprof);
radwiththerm=zeros(nchan, nang*nprof);
radnotherm=zeros(nchan, nang*nprof);
tauz=zeros(nchan, nang*nprof);
angles=zeros(1,nang*nprof);


% Start to fill arrays for iprof=1
ind=1:nang;
radwiththerm(:,ind)=cradtherm;
radnotherm(:,ind)=cradnotherm;
tauz(:,ind)=ctrans;
tprof(:,ind)=temp(:,iprof)*ones(1,nang);
angles(ind)=secang;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the other profiles
for iprof=2:nprof
   ind=ind + nang;
   eval(['load ' thermfile int2str(iprof)]);
   tprof(:,ind)=temp(:,iprof)*ones(1,nang);
   radwiththerm(:,ind)=cradtherm;
   radnotherm(:,ind)=cradnotherm;
   tauz(:,ind)=ctrans;
   angles(ind)=secang;
end


% Clean up unwanted variables
clear cradtherm cradnotherm trans outname secang
secang=angles; clear angles
clear iprof ind temp


% Save output
eval(['save ' outfile      ' tprof secang tauz radwiththerm radnotherm' ...
   ' freq idchan      nang nchan nprof nlay emis tsurf psurf tref comment']);
%eval(['save ' outfile band ' tprof secang tauz radwiththerm radnotherm' ...
%   ' freq idchan band nang nchan nprof nlay emis tsurf psurf tref comment']);
clear

%%% end of program %%%
