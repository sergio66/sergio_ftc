% MATLAB script to convolve CH4 fast model layer-to-space
% transmittance data
% This version is for the December 2007 IASI fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Convert CH4 from layer-to-space od to layer od
%       Loop over angles
%          Convert nadir optical depth to transmittance for current angle
%          Convolve using fconvkc
%       End loop on angles
%    End loop on sets
%    Save convolved trans
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The dummy X strings below should be replaced with the appropriate values
outname='XMOUTNAMEX';
koutname='XKOUTNAMEX';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(' ')
disp('Starting to process data ------------------------------------------')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The variables below should be set by hand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% CH4 multiplier - should match value in kCARTA namelist
ch4_mult = 1.05;


%%%%%%%
% nlay: Number of layers (per set of mixed paths)
nlay = 100;


%%%%%%%
% secang: Angle secants (without sun)
secang = [1.000  1.190  1.410  1.680  1.990  2.370];


%%%%%%%
% secang_sun: Extra angle secants for shortwave
secang_sun = [2.840  3.470  4.300  5.420  6.940  9.020];


%%%%%%%
% tempfile: Name of the temporary file to be created by readkc3
tempfile = 'xxx.dat';

%%%%%%%
cfirst = 1;
clast  = 8461;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Channel numbers
ichan = (cfirst:clast)'; %'
iconv = ichan + 160;
nchan = length(ichan);
clear cfirst clast

% Include sun angles
secang = [secang, secang_sun];
nang = length(secang);
clear secang_sun

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


% Determine number of sets
nsets = ncol/nlay;
ijunk = mod(ncol,nlay);
if (ijunk ~= 0)
   ncol, nlay
   error('inappropriate value of ncol or nlay')
end
clear ijunk
if (nsets ~= 2 )
   error(['Unexpected number of sets; expecting 2, found ' int2str(nsets)])
end


% Add path for fftconv
addpath /asl/matlab/fftconv


% Read mono layer-to-space optical depths
%
% Read in nominal tauz (large array)
% set1=all gases weight=1 (except 1=0 & 3=0)
zodtot = fread(fid,[npts,nlay],'real*4');
%
% Read in tauz with perturbed CH4 (large array)
% set2=all gases weight=1 (except 1=0 & 3=0) & 6=ch4_mult
zodtot6 = fread(fid,[npts,nlay],'real*4');


% Declare convolved trans arrays
ctall  = zeros(nchan,nlay*nang);
ctall6 = zeros(nchan,nlay*nang);


% Loop over angles
indang = 0:nang:round(nang*nlay - nang);  % indices for 1st angle
for iang = 1:nang
   disp(['doing angle number ' int2str(iang)])
   %
   % Do all gases
   trans = exp(-zodtot*secang(iang));  % (large array)
   [rout, fout] = fconvkc(trans, 'iasi', 'gauss');
   ctall(:,indang+iang) = rout(iconv,:);
   %
   % Do all gases with scaled CH4
   trans = exp(-zodtot6*secang(iang));  % (large array)
   [rout, fout] = fconvkc(trans, 'iasi', 'gauss');
   ctall6(:,indang+iang) = rout(iconv,:);
end % loop over angles
%
fchan = fout(iconv)'; %'
clear iang indang npts ncol od trans rout fout wnums iconv


% Close temporary file
fclose(fid);
clear fid


% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ' ctall ctall6 fchan ichan' ...
   ' secang nang nlay nchan ch4_mult'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit
