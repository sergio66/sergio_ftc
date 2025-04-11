% MATLAB script to convolve fast model layer-to-space transmittance data
% This version is for the Dec 2007 IASI 0fast model production.
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
% nlay: Number of layers (per set of mixed paths)
nlay=100;


%%%%%%%
% secang: Angle secants (without sun)
secang=[1.000  1.190  1.410  1.680  1.990  2.370];


%%%%%%%
% secang_sun: Extra angle secants for shortwave
secang_sun=[2.840  3.470  4.300  5.420  6.940  9.020];


%%%%%%%
% tempfile: Name of the temporary file to be created by readkc3
tempfile='xxx.dat';

%%%%%%%
% alltypeband: Supported typeband
% Note: channel numbers are for 1=645 to 8461=2760 cm^-1, while
% the convolution routine returns 610 to 2825 cm^-1. Add 140 to chan ID
% to convert to convolution output index.
% allcfirst: First channel for each typeband
% allclast: Last channel for each typeband
alltypeband={'fow', 'fmow', 'fcow'};
allcfirst = [    1,  2141,   5041];
allclast  = [ 8461,  8461,   6561];
%ffirst   = [  645,  1180,   1905]; 
%flast    = [ 2760,  2760,   2280];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ii=strmatch(typeband,alltypeband);
if ( length(ii) > 1 | ii < 1)
   ii
   typeband
   alltypeband
   error(['invalid type_band: ' typeband]);
end


% Channel numbers
cfirst=allcfirst(ii);
clast=allclast(ii);
ichan=(cfirst:clast)'; %'
iconv=ichan + 160;
nchan=length(ichan);
clear ii alltypeband allcfirst allclast
clear cfirst clast

% Include sun angles
secang=[secang, secang_sun];
nang=length(secang);
clear secang_sun


% Read in the kcarta output file and create a temporary output file
disp(' ')
disp(['Unchunking kcarta file ' koutname ' and creating temp file ' tempfile]);
readkc3(koutname,tempfile)
clear koutname

% Open and read in the header of the temporary file
disp(' ')
disp('Starting to processing data from temp file');
fid=fopen(tempfile,'r'); % Note: assumed written in native format
npts=fread(fid,1,'integer*4'); % number of freq points
ncol=fread(fid,1,'integer*4'); % number of paths
clear tempfile

% Read freq points
wnums=fread(fid,[npts,1],'real*8');


% Determine number of sets
nsets=ncol/nlay;
ijunk=mod(ncol,nlay);
if (ijunk ~= 0)
   ncol, nlay
   error('inappropriate value of ncol or nlay')
end
clear ijunk


% Add path for fftconv tools
addpath /asl/matlab/fftconv


% Convolve the layer-to-space transmittances
ctrans=zeros(nchan,ncol*nang);
indang=0:nang:round(nang*nlay - nang);  % indices for 1st angle of 1st set
% Loop over the sets
for iset=1:nsets
   disp(' ')
   disp(['Working on convolution set ' int2str(iset)]);
   % Read in all mono data for current set
   od=fread(fid,[npts,nlay],'real*4');
   %
   % Loop over angles
   for iang=1:nang
      disp(['doing angle number ' int2str(iang)])
      trans=exp(-od*secang(iang));
      %
      % Convolve
      [rout,fout] = fconvkc(trans, 'iasi', 'gauss');
      ctrans(:,indang+iang)=rout(iconv,:);
   end % loop over angles
   %
   % Increment indang for next set
   indang=indang + nlay*nang;
   %
end % end loop over sets
fchan = fout(iconv);
clear iang iset indang npts ncol od trans rout fout wnums iconv

% Close temporary file
fclose(fid);
clear fid

% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ...
   ' ctrans fchan ichan secang nang nlay nsets nchan typeband'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

