% MATLAB script to convolve N2O & HNO3 fast model layer-to-space
% transmittance data
% This version is for the October 2009 CrIS fast model production with four
% guard channels at each end of each band. Note "ichan" is channel index
% but not channel ID.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Convert N2O from layer-to-space od to layer od
%    Convert HNO3 from layer-to-space od to layer od
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
% N2O multiplier - should match value in kCARTA namelist
n2o_mult = 0.75;

%%%%%%%
% HNO3 multiplier - should match value in kCARTA namelist
hno3_mult = 2.0;


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
% allcoffset: index offset into fconvkc output for desired channels
% allcfirst: First channel for each band
% allclast: Last channel for each band
allcoffset  = [       68,         20,         16];
allcfirst   = [        1,        722,       1163];
allclast    = [      721,       1162,       1329];
allffirst   = [ 605.0000,  1180.0000,  2105.0000];
allflast    = [1129.9975,  1779.9975,  2604.9975];
nbands = length(allcoffset);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add path for fftconv
addpath /asl/matlab/fftconv

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
if (nsets ~=3 )
   error(['Unexpected number of sets; expecting 3, found ' int2str(nsets)])
end



% Read mono layer-to-space optical depths
%
% Read in nominal tauz (large array)
% set1=all gases weight=1 (except 1=0 & 3=0)
zodtot = fread(fid,[npts,nlay],'real*4');
%
% Read in tauz with perturbed N2O (large array)
% set2=all gases weight=1 (except 1=0 & 3=0) & 4=n2o_mult
zodtot4 = fread(fid,[npts,nlay],'real*4');
%
% Read in tauz with perturbed HNO3 (large array)
% set3=all gases weight=1 (except 1=0 & 3=0) & 12=hno3_mult
zodtot12 = fread(fid,[npts,nlay],'real*4');


% Declare convolved trans arrays
nchan = max(allclast); % want output index to match channel ID
ichan = zeros(nchan,1);
fchan = zeros(nchan,1);
ctall  = zeros(nchan,nlay*nang);
ctall4 = zeros(nchan,nlay*nang);
ctall12 = zeros(nchan,nlay*nang);

% Loop over the bands
for iband=1:nbands

   % Determine requried mono data points
   ib = find(wnums >= allffirst(iband)-0.001 & wnums <= allflast(iband)+0.001);

   % Convolution info
   id = allcfirst(iband):allclast(iband);
   ic = (1:length(id)) + allcoffset(iband);
   paramfile = ['cris12920B' int2str(iband)];

   % Loop over angles
   indang = 0:nang:round(nang*nlay - nang);  % indices for 1st angle
   for iang = 1:nang
      disp(['doing angle number ' int2str(iang)])
      %
      % Do all gases
      trans = exp(-zodtot(ib,:)*secang(iang));  % (large array)
      [rout, fout] = xfconvkc(trans, paramfile, 'ham');
      ctall(id,indang+iang) = rout(ic,:);
      %
      % Do all gases with scaled N2O
      trans = exp(-zodtot4(ib,:)*secang(iang));  % (large array)
      [rout, fout] = xfconvkc(trans, paramfile, 'ham');
      ctall4(id,indang+iang) = rout(ic,:);
      %
      % Do all gases with scaled HNO3
      trans = exp(-zodtot12(ib,:)*secang(iang));  % (large array)
      [rout, fout] = xfconvkc(trans, paramfile, 'ham');
      ctall12(id,indang+iang) = rout(ic,:);
   end % loop over angles
   %
   fchan(id) = fout(ic)'; %'
   ichan(id) = id;

end % end of loop over bands
clear iang indang npts ncol od trans rout fout wnums iconv


% Close temporary file
fclose(fid);
clear fid


% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ' ctall ctall4 ctall12 fchan ichan' ...
   ' secang nang nlay nchan n2o_mult hno3_mult'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit
