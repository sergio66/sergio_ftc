% MATLAB script to convolve ref therm reg data
%
% This version is for the September 2008 IASI fast model production.
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
cfirst = 1;
clast  = 8461;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Channel numbers
ichan=(cfirst:clast)'; %'
iconv = ichan + 160;
nchan=length(ichan);
clear cfirst clast


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

% Add path for fconvkc
addpath /asl/matlab/fftconv

% Convolve all the data
addpath /asl/matlab/h4tools
addpath /asl/matlab/sconv
[rout, fout] = xfconvkc(mono, 'iasi12992', 'gauss');
clear mono wnums
rout = rout(iconv,:);
fchan = fout(iconv)'; %'
clear iconv fout


% Re-arrange the data in rout
ctrans=rout(:,1:nang);
cradtherm=rout(:,(nang+1):(2*nang));
cradnotherm=rout(:,(2*nang+1):(3*nang));
clear rout


% Save the results
eval(['save ' outname ' ctrans cradtherm cradnotherm fchan ichan secang' ...
   ' emis psurf tsurf nlay nang nchan'])

exit

%%% end of program %%%
