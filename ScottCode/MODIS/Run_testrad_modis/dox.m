% calculcate radiance from comvolved transmittance

% Created: 13 June 2005, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addpath /asl/matlab/h4tools

load polyfit_f_vs_t.mat

% number of profiles
nprof = 48;

% surface emissivity
emis = 0.975;

% surface layer
lsurf = 97;

% Load surface temperature for 48 regression profiles
load reg_all_tsurf.txt


% Get type/band info
%type = input('Enter typeband {fow, fmw, fcow} : ','s');
%if (strcmp(type,'fow'))
%   band = input('Enter fow band {long, short} : ','s');
%   typeband = [type '_' band];
%else
%   band = '';
%   typeband = type;
%end
type='fow';
band='short';
typeband='fow_short'
foffset = input('enter foffset : ')

if (strcmp(type,'fcow'))
   isetrad = 4;
   subsetangles = 6;
else
   isetrad = 3;
end

% load temperature profile
hfile=['../Prof_' type '/reg_' type '_op.rtp'];
[head, hattr, prof, pattr] = rtpread(hfile);

% Read first file to get sizes
ii = 1;
eval(['load ../TransMODIS_' typeband '/ctranscon_' typeband '_' int2str(ii)]);

nangrad = nang;
if (strcmp(type,'fcow') | strcmp(band,'short'))
   nangrad = 6;
end

% Declare 3-D radiance array
radtransall = zeros(nchan, nangrad, nprof);
bttransall = zeros(nchan, nangrad, nprof);
radall = zeros(nchan, nangrad, nprof);
btall = zeros(nchan, nangrad, nprof);

% Declare 3-D profile arrays
temp_all = zeros(nlay, nangrad, nprof);
h2o_all  = zeros(nlay, nangrad, nprof);
o3_all   = zeros(nlay, nangrad, nprof);
co_all   = zeros(nlay, nangrad, nprof);
ch4_all  = zeros(nlay, nangrad, nprof);
ang_all = secang(1:nangrad)' * ones(1, nprof);

% Loop over the files
for ii = 1:nprof;
   disp(['doing profile ' int2str(ii)])

   % Load convolved radiance
   eval(['load ../TestradMODIS_' typeband '/crad_' typeband '_' int2str(ii)]);

   % Load convolved layer-to-space transmittnace
   eval(['load ../TransMODIS_' typeband '/ctranscon_' typeband '_' int2str(ii)]);
   % Note: layers are surface=1 to nang*nlay=TOA
   % (inner loop: nang angles)(mid loop nlay layers)(outer loop nsets)

   %%%
   % Calc the T-dependent mean channel freq
   fmean = zeros(nchan, nlay+1);
   temp = [prof.ptemp(1:nlay,ii); reg_all_tsurf(ii)];
   x = (temp - tmin)/100;
   for jj = 1:nchan
      kk = find( clist == ichan(jj) );
      pcoef = pcoefall(kk,:);
      fmean(jj,:) = fnom(kk) + polyval(pcoef,x);
   end
fmean = fmean + foffset;
   %%%

   ia = 1:nang;

   % Do top layer
   planck = bt2rad( fmean(:,1), prof.ptemp(1,ii) ) * ones(1,nang);
   ia = nang:-1:1;
   ind = nlay*nang*isetrad + 1 - ia;
   rad = planck .* (1 - ctrans(:,ind) );

   % Loop over the remaining layers
   for jj = 2:lsurf
      planck = bt2rad( fmean(:,jj), prof.ptemp(jj,ii) ) * ones(1,nang);
      inda = ind;
      ind = inda - nang;
      rad = rad + planck.*( ctrans(:,inda) - ctrans(:,ind) );
   end

   % Add surface emissision
   planck = bt2rad( fmean(:,nlay+1), reg_all_tsurf(ii) ) * ones(1, nang);
   rad = rad + emis .* planck .* ctrans(:,ind);

   secangrad = secang;
   if (nang > nangrad)
      rad = rad(:,1:nangrad);
      secangrad = secang(1:nangrad);
   end

   temp_all(:,:,ii) = prof.ptemp(1:nlay,ii) * ones(1,nangrad);
   h2o_all(:,:,ii)  = prof.gas_1(1:nlay,ii) * secangrad;
   o3_all(:,:,ii)   = prof.gas_3(1:nlay,ii) * secangrad;
   co_all(:,:,ii)   = prof.gas_5(1:nlay,ii) * secangrad;
   ch4_all(:,:,ii)  = prof.gas_6(1:nlay,ii) * secangrad;

   radtransall(:,:,ii) = rad;
   bttransall(:,:,ii) = radtot(fchan, rad);

   radall(:,:,ii) = crad*1000;
   btall(:,:,ii) = radtot(fchan, crad*1000);

end

for ii=1:nchan
   plot( squeeze( btall(ii,:,:) - bttransall(ii,:,:) )' ),grid
   title(['ii=' int2str(ii) ', ichan=' int2str( ichan(ii) )])
   pause
end

%%% end of program %%%
