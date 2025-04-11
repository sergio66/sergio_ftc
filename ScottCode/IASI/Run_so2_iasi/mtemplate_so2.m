% MATLAB script to convolve SO2 fast model layer-to-space transmittance data
% This version is for the September 2008 IASI fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Convert SO2 from layer-to-space od to layer od
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
% SO2 multiplier
so2_mult=1000;


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
if (nsets ~=2 )
   error(['Unexpected number of sets; expecting 2, found ' int2str(nsets)])
end


% Add path for fconvkc reader
addpath /asl/matlab/fftconv


% Read mono layer-to-space optical depths
%
% Read in tauz_all (large array)
odall=fread(fid,[npts,nlay],'real*4');
%
% Read in tauz_so2 (large array)
odso2=fread(fid,[npts,nlay],'real*4');


% Convert odso2 to all gases od with single-layer scaled SO2
% Note: kCARTA layers are 1=surface, 100=top of atmos
for ii=1:(nlay-1)
   odso2(:,ii)=odall(:,ii) + (so2_mult-1)*(odso2(:,ii) - odso2(:,ii+1));
end
odso2(:,nlay)=odall(:,nlay) + (so2_mult-1)*odso2(:,nlay);


% Declare convolved trans arrays
ctall=zeros(nchan,nlay*nang);
ctall9=zeros(nchan,nlay*nang);


% Loop over angles
indang=0:nang:round(nang*nlay - nang);  % indices for 1st angle
for iang=1:nang
   disp(['doing angle number ' int2str(iang)])
   %
   % Do all gases
   trans=exp(-odall*secang(iang));  % (large array)
%   [rout, fout] = fconvkc(trans, 'iasi', 'gauss');
   [rout, fout] = xfconvkc(trans, 'iasi12992', 'gauss');
   ctall(:,indang+iang)=rout(iconv,:);
   %
   % Do all gases with scaled SO2
   trans=exp(-odso2*secang(iang));  % (large array)
%   [rout, fout] = fconvkc(trans, 'iasi', 'gauss');
   [rout, fout] = xfconvkc(trans, 'iasi12992', 'gauss');
   ctall9(:,indang+iang)=rout(iconv,:);
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
eval(['save ' outname ...
   ' ctall ctall9 fchan ichan secang nang nlay nchan so2_mult'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

