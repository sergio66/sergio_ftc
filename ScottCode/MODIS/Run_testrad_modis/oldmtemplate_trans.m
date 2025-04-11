% MATLAB script to convolve monochromatic test transmittance data
% This version is for the June 2005 MODIS fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%       Read in all layer-to-space optical depths
%       Loop over angles
%          Convert nadir optical depth to transmittance for current angle
%          Convolve using sconv2
%          Determine planck fmean
%       End loop on angles
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
% Expected number of layers
nlayx = 100;

%%%%%%%
% secang: Angle secant
secang = 1.5;

%%%%%%%
% MATLAB file with temperture data
tfile = '/asl/packages/mypref_2000_07_17.mat'

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


% Load temperature profile  data
% note: this is not necessarily the same profile as used by kcarta to
% generate the layer-to-space optical depths
eval(['load ' tfile])


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
if (ncol ~= nlayx)
   error('Unexpected number of layer optical depths')
end
nlay = ncol;
clear tempfile


% Read freq points
wnums = fread(fid,[npts,1],'real*8');
wnums = round(wnums * 400) / 400;  % round to 0.0025 cm^-1

% Read mono rad data
od = fread(fid,[npts,ncol],'real*4');
% reverse layer order from 1=surf to 1=TAO
indr = ncol:-1:1;
od = od(:,indr);

% Close temporary file
fclose(fid);
clear fid


% Convert od to trans
trans = exp( -secang * od ); 
clear od

% Calculate planck
planck = zeros(npts,ncol);
for ii = 1:nlay
   planck(:,ii) = ttorad( wnums, temp(ii) );
end

pt = planck .* trans;

% Convolve the trans and planck*trans data
[ctop, fchan] = sconv2_vd(pt, wnums, ichan, srfname);
[cbot, fchan] = sconv2_vd(trans, wnums, ichan, srfname);
clear npts ncol rad pt trans

% Find fmean
% note: ctop and cbot can be very small (1E-110) but always(?) non-zero
crad = ctop ./ cbot;
fmean = zeros(nchan, nlay);
for ii = 1:nchan
   for jj = 1:nlay
      % Find index in planck closest to crad
      % Note: for this T range, planck decreases with freq
      ilo = max( find( planck(:,jj) > crad(ii,jj) ) );
      if (length(ilo) == 1)
         ihi = ilo + 1;
         % Linear interpolate to rout
         fmean(ii,jj)=( (wnums(ihi)-wnums(ilo))/(planck(ihi,jj)-...
            planck(ilo,jj)) ) .* (crad(ii,jj) - planck(ilo,jj)) + wnums(ilo);
      end
   end
end

clear planck

% Save the results
disp(' ')
disp(['Saving data to file ' outname]);
eval(['save ' outname ...
   ' crad fchan ichan fmean secang nchan nlay typeband ctop cbot wnums temp'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

%%% end of program %%%
