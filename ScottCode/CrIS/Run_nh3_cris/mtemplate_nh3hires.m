% MATLAB script to convolve NH3 fast model layer-to-space transmittance data
% This version is for the October 2009 CrIS fast model production with four
% guard channels at each end of each band. Note "ichan" is channel index
% but not channel ID. This version for full OPD (hi-res).
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Convert NH3 from layer-to-space od to layer od
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
% NH3 multiplier
nh3_mult=100;


%%%%%%%
% nlay: Number of layers (per set of mixed paths)
nlay=100;


%%%%%%%
% secang: Angle secants (without sun)
secang=[1.000  1.190  1.410  1.680  1.990  2.370];
nang=length(secang);


%%%%%%%
% tempfile: Name of the temporary file to be created by readkc3
tempfile='xxx.dat';


%%%%%%%
% allcoffset: index offset into fconvkc output for desired channels
% allcfirst: First channel for each band
% allclast: Last channel for each band
% Note: includes four guard channels at each end of each band ("g4")
allcoffset  = [       68,         44];
allcfirst   = [        1,        722];
allclast    = [      721,       1594];
allffirst   = [ 605.0000,  1180.0000];
allflast    = [1129.9975,  1779.9975];
nbands = length(allcoffset);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add path for fconvkc reader
addpath /asl/matlab/fftconv


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


% Declare output arrays
nchan = max(allclast); % want output index to match channel ID
ichan = zeros(nchan,1);
fchan = zeros(nchan,1);
ctall =zeros(nchan,nlay*nang);
ctallx=zeros(nchan,nlay*nang);

% Loop over the bands
for iband=1:nbands

   % Determine requried mono data points
   ib = find(wnums >= allffirst(iband)-0.001 & wnums <= allflast(iband)+0.001);

   % Convolution info
   id = allcfirst(iband):allclast(iband);
   ic = (1:length(id)) + allcoffset(iband);
   paramfile = ['cris12920B' int2str(iband) 'hires'];

   % Loop over angles
   indang=0:nang:round(nang*nlay - nang);  % indices for 1st angle
   for iang=1:nang
      disp(['doing angle number ' int2str(iang)])
      %
      % Do all gases
      trans=exp(-odall(ib,:)*secang(iang));  % (large array)
      [rout, fout] = xfconvkc(trans, paramfile, 'ham');
      ctall(id,indang+iang)=rout(ic,:);
      %
      % Do all gases with scaled NH3
      trans=exp(-odnh3(ib,:)*secang(iang));  % (large array)
      [rout, fout] = xfconvkc(trans, paramfile, 'ham');
      ctallx(id,indang+iang)=rout(ic,:);
   end % loop over angles
   %
   fchan(id) = fout(ic)'; %'
   ichan(id) = id;

end % end of loop over bands
clear iang indang npts ncol od trans rout fout wnums ic


% Close temporary file
fclose(fid);
clear fid


% Save the results
disp(' ')
disp(['Saving convolved data to file ' outname]);
eval(['save ' outname ...
   ' ctall ctallx fchan ichan secang nang nlay nchan nh3_mult'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit
%%% end of program %%
