% MATLAB script to convolve fast model layer-to-space transmittance data
% This version is for the April 2002 AIRS fast model production.
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
srfname='srftable.hdf';


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
% allcfirst: First channel for each typeband
% allclast: Last channel for each typeband
alltypeband={'fow_long', 'fow_short', 'fmw', 'fcow'};
allcfirst = [     1,        1865,     1263,   1865];
allclast  = [  1864,        2378,     1654,   2014];


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
ichan=(cfirst:clast)';
nchan=length(ichan);
clear ii alltypeband allcfirst allclast


% Test to see if the sun angles are needed
if (cfirst > 1864)
   secang=[secang, secang_sun];
end
nang=length(secang);
clear secang_sun cfirst clast


% Read in the kcarta output file and create a temporary output file
disp(' ')
disp(['Unchunking kcarta file ' koutname ' and creating temp file ' tempfile]);
readkc3(koutname,tempfile)
clear koutname

% Open and read in the header of the temporary file
disp(' ')
disp('Starting to processing data from temp file');
fid=fopen(tempfile,'r'); % Note: assumed written in native format
npts=fread(fid,1,'integer*4');
ncol=fread(fid,1,'integer*4');
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


% Add paths for HDF readers and sconv
addpath /asl/matlab/h4tools
addpath /asl/matlab/sconv


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
      [rout, fchan] = sconv2(trans, wnums, ichan, srfname);
      ctrans(:,indang+iang)=rout;
   end % loop over angles
   %
   % Increment indang for next set
   indang=indang + nlay*nang;
   %
end % end loop over sets
clear iang iset indang npts ncol od trans rout wnums

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

