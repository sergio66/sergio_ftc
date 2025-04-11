% MATLAB script to convolve monochromatic test transmittance data
% This version is for the June 2005 MODIS fast model production.
%
% Basic outline of this script:
%    Run readkc3 to read in KCARTA data and create temporary output file
%    Read in the header of the temporary file
%    Read in all layer-to-space optical depths
%    Loop over angles
%       Convert nadir optical depth to transmittance for current angle
%       Convolve using sconv2
%       Determine fmean
%    End loop on angles
%    Save fmean and convolved trans & rad
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The dummy X strings below should be replaced with the appropriate values
typeband='XTYPEBANDX';
outname='XMOUTNAMEX';
koutname='XKOUTNAMEX';
pnum=XPROFX;
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
% secangs: Angle secant
secang = [1.000  1.190  1.410  1.680  1.990  2.370];
nang = length(secang);

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
alltype   = {   'fow',      'fow',    'fmw', 'fcow'};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Add paths for HDF readers and sconv
addpath /asl/matlab/h4tools
addpath /asl/matlab/sconv

% Surface emissivity = 0.975;
emis = 0.975;

ii=strmatch(typeband,alltypeband);
if ( length(ii) > 1 | ii < 1)
   ii
   typeband
   alltypeband
   error(['invalid type_band: ' typeband]);
end


% RTP file with profile data
type = char( alltype{ii} );
hfile = ['../Prof_' type '/reg_' type '_op.rtp'];
clear alltype


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


% Read RTP file
[head, hattr, prof, pattr] = rtpread(hfile);
water = prof.gas_1(1:nlayx,pnum)';
ozone = prof.gas_3(1:nlayx,pnum)';
temp  = prof.ptemp(1:nlayx,pnum)';
clear head hattr prof pattr


% Read freq points
wnums = fread(fid,[npts,1],'real*8');
wnums = round(wnums * 400) / 400;  % round to 0.0025 cm^-1

% Read mono layer-to-space optical depth data
od = fread(fid,[npts,ncol],'real*4');
% reverse layer order from 1=surf to 1=TAO
indr = ncol:-1:1;
od = od(:,indr);

% Close temporary file
fclose(fid);
clear fid

% Calculate planck
planck = zeros(npts,ncol);
for ii = 1:nlay
   planck(:,ii) = ttorad( wnums, temp(ii) );
end

% Declare output arrays
nlayp1 = nlay + 1;
nlayp2 = nlay + 2;
crad   = zeros(nchan, nlayp2, nang);
ctrans = zeros(nchan, nlay,   nang);
fmean  = zeros(nchan, nlayp1, nang);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the angles
for ia = 1:nang
   disp(['working on secant angle ' num2str(secang(ia))])

   % Convert od to trans
   trans = exp( -secang(ia) * od );

   % Calculate layer radiance contributions
   rad = zeros(npts,nlayp2);
   % Do the top layer
   rad(:,1) = planck(:,1) .* ( 1 - trans(:,1) );
   % Do all other layers
   ind = 2:nlay;
   indm1 = ind - 1;
   rad(:,ind) = planck(:,ind) .* ( trans(:,indm1) - trans(:,ind) );
   % Do the surface
   rad(:,nlayp1) = emis * planck(:,nlay) .* trans(:,nlay);
   % Calc the total radiance
   rad(:,nlayp2) = sum(rad,2);

   disp('...convolving')
   % Convolve the layer radiance contribution
   [cr, fchan] = sconv2_vd(rad, wnums, ichan, srfname);
   crad(:,:,ia) = cr;

   % Convolve the trans
   [ct, fchan] = sconv2_vd(trans, wnums, ichan, srfname);
   ctrans(:,:,ia) = ct;


   % Calculate effective planck
   ct = [ones(nchan,1), ct, (1 - emis)*ct(:,100)];
   ind = 1:nlayp1;
   indp1 = ind + 1;
   cb = cr(:,ind)./( ct(:,ind) - ct(:,indp1) );
   % note: ct and cr can be very small but always(?) non-zero

   disp('...finding fmean')
   % Find fmean
   for ic = 1:nchan
      for jj = 1:nlayp1
         if (jj > nlay)
            jp = nlay;
         else
            jp = jj;
         end
         % Find index in planck closest to cb
         % Note: for this T range, planck decreases with increasing freq
         ilo = max( find( planck(:,jp) > cb(ic,jj) ) );
         if (length(ilo) == 1)
            ihi = ilo + 1;
            % Linear interpolation in cb
            fmean(ic,jj,ia)=( (wnums(ihi)-wnums(ilo))/(planck(ihi,jp)-...
               planck(ilo,jp)) ) .* (cb(ic,jj) - planck(ilo,jp)) + wnums(ilo);
         end
      end
   end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear od planck trans rad

% Save the results
disp(' ')
disp(['Saving data to file ' outname]);
eval(['save ' outname ...
   ' crad ctrans fchan ichan fmean secang typeband temp water ozone'])

disp(' ')
disp('Finished processing data, quitting matlab')
exit

%%% end of program %%%
