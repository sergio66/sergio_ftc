% This is a matlab program to merge the individual profile reflected
% thermal data files into one matlab output file containing *all* the
% data needed to do the fast model ref therm.
%
% Besides the matlab files with the rad and trans and angle data, we
% also need all the profile temperatures, as well as reference temp
% profile.  This is pulled in from separate a RTP file.

% Slightly revised for the September 2003 IASI production code.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iband=1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modify the variables below as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% comment: string to be included in output file
comment='15Sep03 IASI gauss 2cm';


%%%%%%%
% band info
band_fmin =[ 610];
band_fmax =[2825]; 
band_idmin=[   1];
band_idmax=[8861];


%%%%%%%
% surface info
nlay=97;
psurf=1013.9476;
emis=0.850;
tsurf=275.0;


%%%%%%%
% Angles
secang=[1.000  1.190  1.410  1.680  1.990  2.370];


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
thermfile=['rtherm'];


%%%%%%%
% outfile: Name prefix of output matlab file to create
outfile=['thermdata_iasi'];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main executable code begins below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

id=(band_idmin(iband):band_idmax(iband))';


% Read in the first therm data file
iprof=1;
eval(['load ' thermfile int2str(iprof)]);


iok=find(chan_wnums > band_fmin(iband)-0.01 & ...
        chan_wnums < band_fmax(iband)+0.01);
if (length(iok) ~= length(id))
   error('unexpected number of channels')
end
freq=chan_wnums(iok)';
idchan=id;
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
ia1=1:nang;
ia2=(nang+1):(2*nang);
ind=1:nang;
radwiththerm(:,ind)=chan_rad(iok,ia1);
radnotherm(:,ind)=chan_rad(iok,ia2);
tauz(:,ind)=chan_trans(iok,:);
tprof(:,ind)=temp(:,iprof)*ones(1,nang);
angles(ind)=secang;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the other profiles
for iprof=2:nprof
   ind=ind + nang;
   eval(['load ' thermfile int2str(iprof)]);
   tprof(:,ind)=temp(:,iprof)*ones(1,nang);
   radwiththerm(:,ind)=chan_rad(iok,ia1);
   radnotherm(:,ind)=chan_rad(iok,ia2);
   tauz(:,ind)=chan_trans(iok,:);
   angles(ind)=secang;
end


% Clean up unwanted variables
clear chan_rad chan_trans chan_wnums outname secang
secang=angles; clear angles
clear iprof ind temp


% Save output
eval(['save ' outfile ' tprof secang tauz radwiththerm radnotherm' ...
   ' freq idchan iband nang nchan nprof nlay emis tsurf psurf tref comment']);
clear

%%% end of program %%%
