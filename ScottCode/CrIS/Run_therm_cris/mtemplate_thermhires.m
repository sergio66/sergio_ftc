% MATLAB script to convolve ref therm reg data
%
% This version is for the October 2009 CrIS fast model production with four
% guard channels at each end of each band. Note "ichan" is channel index
% but not channel ID. This version for full OPD (hi-res).
%
% Basic outline of this script:
%    Assign secang
%    Assign band info
%    Run readkc3 to read in KCARTA data
%    Convert nadir optical depth to transmittance at secang
%    Convolve all mono data
%    Re-arrange convolved data
%    Save convolved data
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The dummy X strings below should be replaced with the appropriate values
outname='XMOUTNAMEX';
koutname='XKOUTNAMEX';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the variables below as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% secang
% Angle secants (without sun).  These should match the secant of the
% angles in the KCARTA template file.
secang=[1.000  1.190  1.410  1.680  1.990  2.370];


%%%%%%%
% Surface info.  This should match the surface info in the KCARTA
% template file.
emis=0.850;
psurf=1013.9476;
tsurf=275.0;
nlay=97;


%%%%%%%
% allcoffset: index offset into fconvkc output for desired channels
% allcfirst: First channel for each band
% allclast: Last channel for each band
allcoffset  = [       68,         44,         76];
allcfirst   = [        1,        722,       1595];
allclast    = [      721,       1594,       2235];
allffirst   = [ 605.0000,  1180.0000,  2105.0000];
allflast    = [1129.9975,  1779.9975,  2604.9975];
nbands = length(allcoffset);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add path for xfconvkc
addpath /asl/matlab/fftconv


% Read in the kcarta output file
[data, wnums]=readkc3(koutname);
nang=length(secang);
[npts, ndata]=size(data);
% Note: data should contain ndata=1+2*nang items:
%      1  surface-to-space nadir optical depth
%   nang  radiances at secang including refl therm
%   nang  radiances at secang without refl therm
nexpect=2*nang + 1;  % One OD (total) plus two rads per angle
if (ndata ~= nexpect)
   nang
   nexpect
   ndata
   error('therm data file contains unexpected number of items')
end


% Convert nadir optical depth to transmittance at secang
trans=exp(-data(:,1)*secang);
mono=[trans, data(:,2:ndata)];
clear trans data
wnums = round(wnums*400)/400;

% Declare output arrays
nchan = max(allclast); % want output index to match channel ID
ichan = zeros(nchan,1);
fchan = zeros(nchan,1);
ctrans = zeros(nchan,nang);
cradtherm = zeros(nchan,nang);
cradnotherm = zeros(nchan,nang);

% Loop over the bands
for iband=1:nbands

   % Determine requried mono data points
   ib = find(wnums >= allffirst(iband)-0.001 & wnums <= allflast(iband)+0.001);

   % Convolved the mono data
   id = allcfirst(iband):allclast(iband);
   ic = (1:length(id)) + allcoffset(iband);
   paramfile = ['cris12920B' int2str(iband) 'hires'];

   [rout, fout] = xfconvkc(mono(ib,:), paramfile, 'ham');
   rout = rout(ic,:);
   fchan(id) = fout(ic)'; %'
   ichan(id) = id;

   % Re-arrange the data in rout
   ctrans(id,:)=rout(:,1:nang);
   cradtherm(id,:)=rout(:,(nang+1):(2*nang));
   cradnotherm(id,:)=rout(:,(2*nang+1):(3*nang));

end % end of loop over bands

% Save the results
eval(['save ' outname ' ctrans cradtherm cradnotherm fchan ichan secang' ...
   ' emis psurf tsurf nlay nang nchan'])

exit

%%% end of program %%%
