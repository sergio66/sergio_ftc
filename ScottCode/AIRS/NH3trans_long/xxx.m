% MATLAB script to convolve NH3 fast model layer-to-space transmittance data
% This version is for the March 2008 AIRS fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Convert NH3 from layer-to-space od to layer od
%       Loop over angles
%          Convert nadir optical depth to transmittance for current angle
%          Convolve using sconv2
%       End loop on angles
%    End loop on sets
%    Save convolved trans
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The dummy X strings below should be replaced with the appropriate values
band='long';
outname='nh3_m150f_long_48';
koutname='kcarta_out.bin';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(' ')
disp('Starting to process data ------------------------------------------')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The variables below should be set by hand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
% NH3 multiplier
nh3_mult=100;


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
% allband: Supported band
% allcfirst: First channel for each band
% allclast: Last channel for each band
allband={'long'};
allichan = { [302:1307, 2403:2622] };


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ii=strmatch(band,allband);
if ( length(ii) > 1 | ii < 1)
   ii
   band
   allband
   error(['invalid band: ' band]);
end


% Channel numbers
ichan = allichan{ii}'; %'
nchan = length(ichan);
clear allband allichan


% Test to see if the sun angles are needed
if (ii > 1)
   secang=[secang, secang_sun];
end
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


% Add paths for HDF readers and sconv
addpath /asl/matlab/h4tools
addpath /asl/matlab/sconv


% Read mono layer-to-space optical depths
%
% Read in tauz_all (large array)
odall=fread(fid,[npts,nlay],'real*4');
%
% Read in tauz_nh3 (large array)
odnh3=fread(fid,[npts,nlay],'real*4');


% Convert odnh3 to all gases od with single-layer scaled NH3
% Note: kCARTA layers are 1=surface, 100=top of atmos
for ii=1:(nlay-1)
   odnh3(:,ii)=odall(:,ii) + (nh3_mult-1)*(odnh3(:,ii) - odnh3(:,ii+1));
end
odnh3(:,nlay)=odall(:,nlay) + (nh3_mult-1)*odnh3(:,nlay);


% Declare convolved trans arrays
ctall =zeros(nchan,nlay*nang);
ctallx=zeros(nchan,nlay*nang);


% Loop over angles
indang=0:nang:round(nang*nlay - nang);  % indices for 1st angle
for iang=1:nang
   disp(['doing angle number ' int2str(iang)])
   %
   % Do all gases
   trans=exp(-odall*secang(iang));  % (large array)
   [rout, fchan] = sconv2(trans, wnums, ichan, srfname);
   ctall(:,indang+iang)=rout;
   %
   % Do all gases with scaled NH3
   trans=exp(-odnh3*secang(iang));  % (large array)
   [rout, fchan] = sconv2(trans, wnums, ichan, srfname);
   ctallx(:,indang+iang)=rout;
end % loop over angles
%
clear iang indang npts ncol od trans rout wnums


% Close temporary file
fclose(fid);
clear fid


% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ...
   ' ctall ctallx fchan ichan secang nang nlay nchan band nh3_mult'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

