% MATLAB script to convolve monochromatic test radiance data
% This version is for the June 2005 MODIS fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Figure out how many sets of 100-layer optical depths there are
%    Loop over the sets
%       Read in all layer-to-space optical depths for current set
%       Loop over angles
%          Convert nadir optical depth to transmittance for current angle
%          Convolve using sconv2
%       End loop on angles
%    End loop on sets
%    Save convolved trans
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The dummy X strings below should be replaced with the appropriate values
typeband='XTYPEBANDX';
outname='XMOUTNAMEX';
koutname='XKOUTNAMEX';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(' ')
disp('Starting to process data ------------------------------------------')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The variables below should be set by hand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% SRF table
srfname = 'srftable.hdf';


%%%%%%%
% secang: Angle secants (without sun)
secang=[1.000  1.190  1.410  1.680  1.990  2.370];


%%%%%%%
% tempfile: Name of the temporary file to be created by readkc3
tempfile = 'xxx.dat';

%%%%%%%
% alltypeband: Supported typeband
% allcfirst: First channel for each typeband
% allclast: Last channel for each typeband
alltypeband={'fow_long', 'fow_short', 'fmw', 'fcow'};
allcfirst = [    36,         25,       29,     25];
allclast  = [    27,         20,       27,     24];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Add paths for HDF readers and sconv
addpath /asl/matlab/h4tools
addpath /asl/matlab/sconv


ii=strmatch(typeband,alltypeband);
if ( length(ii) > 1 | ii < 1)
   ii
   typeband
   alltypeband
   error(['invalid type_band: ' typeband]);
end


% Channel numbers
cfirst = allcfirst(ii);
clast = allclast(ii);
ichan = (cfirst:-1:clast)';
nchan = length(ichan);
clear ii alltypeband allcfirst allclast


% Read in the kcarta output file and create a temporary output file
disp(' ')
disp(['Unchunking kcarta file ' koutname ' and creating temp file ' tempfile]);
readkc3(koutname,tempfile)
clear koutname

% Open and read in the header of the temporary file
disp(' ')
disp('Starting to processing data from temp file');
fid = fopen(tempfile,'r'); % Note: assumed written in native format
npts = fread(fid,1,'integer*4');
ncol = fread(fid,1,'integer*4');
clear tempfile


% Read freq points
wnums = fread(fid,[npts,1],'real*8');

% Read mono rad data
rad = fread(fid,[npts,ncol],'real*4');
if (length(secang) ~= ncol)
   error('Unexpected number of radiances/angles')
else
   nang = ncol;
end

% Close temporary file
fclose(fid);
clear fid


% Convolve the radiances
[crad, fchan] = sconv2_vd(rad, wnums, ichan, srfname);
clear npts ncol rad wnums


% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ...
   ' crad fchan ichan secang nang nchan typeband'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

%%% end of program %%%
